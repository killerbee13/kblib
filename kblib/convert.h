/* *****************************************************************************
 * kblib is a general utility library for C++14 and C++17, intended to provide
 * performant high-level abstractions and more expressive ways to do simple
 * things.
 *
 * Copyright (c) 2021 killerbee
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * ****************************************************************************/

/**
 * @file
 * @brief Provides facilities to convert between various kinds of
 * representations.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#include "kblib/stats.h"
#if KBLIB_DEF_MACROS and not defined(pFromStr)
#	define pFromStr(type, val) ::kblib::fromStr<type>((val), #type)
#endif

#ifndef KBLIB_CONVERT_H
#	define KBLIB_CONVERT_H

#	include <algorithm>
#	include <array>
#	include <cassert>
#	include <chrono>
#	include <iomanip>
#	include <sstream>
#	include <stdexcept>
#	include <string>
#	include <typeinfo>

#	include "algorithm.h"
#	include "iterators.h"
#	include "traits.h"

#	if KBLIB_USE_STRING_VIEW

#		include <string_view>

/**
 * @def KBLIB_USE_SPANSTREAM
 * @brief This internal macro is used to determine if kblib can use C++17's
 * std::string_view.
 */
#		if KBLIB_USE_SPANSTREAM

#			include <spanstream>
#		else

#			pragma GCC diagnostic push
#			pragma GCC diagnostic ignored "-W#warnings"
#			include <strstream>
#			pragma GCC diagnostic pop
#		endif

#		include "stringops.h"

#	endif

#	include <iostream>

namespace KBLIB_NS {

template <int base, typename Int>
KBLIB_NODISCARD constexpr auto to_string(Int num) -> std::string {
	static_assert(base <= 62 and base > 0, "Supported bases are 1 thru 62.");
	constexpr auto digits = remove_null_terminator(
	    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
	std::string ret;
	bool neg = false;
	if (num < 0) {
		neg = true;
		num *= -1;
	} else if (num == 0) {
		return "0";
	}
	do {
		ret.push_back(digits[num % base]);
	} while (num /= base);
	if (neg) {
		ret.push_back('-');
	}
	std::reverse(ret.begin(), ret.end());
	return ret;
}

template <typename Int>
KBLIB_NODISCARD constexpr auto to_string(Int num, int base) -> std::string {
	assert(base <= 62 and base > 0);
	constexpr auto digits = remove_null_terminator(
	    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
	std::string ret;
	bool neg = false;
	if (num < 0) {
		neg = true;
		num *= -1;
	} else if (num == 0) {
		return "0";
	}
	do {
		ret.push_back(digits[num % base]);
	} while (num /= base);
	if (neg) {
		ret.push_back('-');
	}
	std::reverse(ret.begin(), ret.end());
	return ret;
}

/**
 * @internal
 */
namespace detail_convert {

	template <typename Result, unsigned variants, std::size_t N>
	KBLIB_NODISCARD constexpr auto read_digits(const char* begin,
	                                           const char* end, unsigned base,
	                                           const char (&digits)[N])
	    -> Result {
		if (begin == end) {
			throw std::invalid_argument("\"\" is not an integer");
		}
		Result result{};
		for (auto c : indirect(begin, end)) {
			if (c != '\'') {
				if (result > static_cast<Result>(max.of<Result>() / base)) {
					throw std::invalid_argument("Integer out of range for type");
				}
				result *= base;
				auto pos = find_in(std::begin(digits),
				                   std::begin(digits) + base * variants, c);
				if (pos != base * variants) {
					result += pos / variants;
				} else {
					throw std::invalid_argument("invalid character in integer");
				}
			}
		}
		return result;
	}

} // namespace detail_convert

/// TODO(killerbee13): implement parsing for signed min (the value with no
/// positive counterpart)
template <typename Result>
KBLIB_NODISCARD constexpr auto parse_integer(const char* begin, const char* end,
                                             int base = 0) -> Result {
	if (begin == end) {
		throw std::invalid_argument("\"\" is not an integer");
	} else if (*begin == '-') {
		if (begin + 1 == end) {
			throw std::invalid_argument("\"-\" is not an integer");
		}
		if (begin[1] == '-' or begin[1] == '+') {
			throw std::invalid_argument("Too many signs in integer");
		}
		return -parse_integer<Result>(begin + 1, end, base);
	}
	if (begin[0] == '+') {
		if (begin + 1 == end) {
			throw std::invalid_argument("\"+\" is not an integer");
		}
		if (begin[1] == '+' or begin[1] == '-') {
			throw std::invalid_argument("Too many signs in integer");
		}
		++begin;
	}
	if (base == 0) {
		if (*begin == '0') {
			if (begin + 1 == end) {
				return 0;
			} else if (begin[1] == '-' or (begin + 2 != end and begin[2] == '-')) {
				throw std::invalid_argument("unexpected - in integer");
			} else if (begin[1] == '+' or (begin + 2 != end and begin[2] == '+')) {
				throw std::invalid_argument("unexpected + in integer");
			} else {
				switch (begin[1]) {
				case 'x':
					return parse_integer<Result>(begin + 2, end, 16);
				case 'b':
					return parse_integer<Result>(begin + 2, end, 2);
				default:
					return parse_integer<Result>(begin + 1, end, 8);
				}
			}
		} else {
			return parse_integer<Result>(begin, end, 10);
		}
	} else {
		if (base < 2 or base > 62) {
			throw std::invalid_argument(
			    "base must be either 0 or a positive number between 2 and 62");
		} else if (base <= 36) {
			return detail_convert::read_digits<Result, 2>(
			    begin, end, to_unsigned(base),
			    "00112233445566778899AaBbCcDdEeFfGgHhIiJjKkLlMmNnOoPpQqRr"
			    "SsTtUuVvWwXxYyZz");
		} else if (base <= 62) {
			return detail_convert::read_digits<Result, 1>(
			    begin, end, to_unsigned(base),
			    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
		}
	}
	// silence warning that control may flow off the end even though all paths
	// return or throw
	return 0;
}

template <typename Result, std::size_t N>
KBLIB_NODISCARD constexpr auto parse_integer(const char (&in)[N], int base = 0)
    -> Result {
	char t = in[N - 1];
	return parse_integer<Result>(std::begin(in), std::end(in) - +(t == '\0'),
	                             base);
}

template <typename Result>
KBLIB_NODISCARD constexpr auto parse_integer(const std::string& in,
                                             int base = 0) -> Result {
	return parse_integer<Result>(to_pointer(begin(in)), to_pointer(end(in)),
	                             base);
}

#	if KBLIB_USE_STRING_VIEW

template <typename Result>
KBLIB_NODISCARD constexpr auto parse_integer(std::string_view in, int base = 0)
    -> Result {
	return parse_integer<Result>(to_pointer(begin(in)), to_pointer(end(in)),
	                             base);
}

#	endif

template <typename T, T V>
struct constant : std::integral_constant<T, V> {
	constexpr auto operator-() -> constant<T, -V> { return {}; }
	constexpr constant() = default;
	constexpr /* implicit */ constant(std::integral_constant<T, V>) noexcept {}
	// reverse conversion handled by slicing
};

inline namespace literals {

	template <char... Cs>
	KBLIB_NODISCARD constexpr auto operator""_c() {
		constexpr char arr[] = {Cs...};
		return constant<std::intmax_t, parse_integer<std::intmax_t>(arr)>{};
	}
	template <char... Cs>
	KBLIB_NODISCARD constexpr auto operator""_cu() {
		constexpr char arr[] = {Cs...};
		return constant<std::uintmax_t, parse_integer<std::uintmax_t>(arr)>{};
	}

} // namespace literals

template <typename E,
          typename = typename std::enable_if<std::is_enum<E>::value>::type>
KBLIB_NODISCARD constexpr auto etoi(E e) -> auto {
	return static_cast<std::underlying_type_t<E>>(e);
}

template <int maxBufLen = 4096, typename clock, typename duration>
KBLIB_NODISCARD auto time_to_str(std::chrono::time_point<clock, duration>& tp,
                                 const std::string& fmt = "%F %T")
    -> std::string {
	std::time_t time = clock::to_time_t(tp);
	std::tm* tmb = std::localtime(&time);
	std::string ret{maxBufLen, '\0'};
	std::strftime(&ret.front(), maxBufLen, fmt.c_str(), tmb);
	return ret;
}

/**
 * @namespace detail_units
 * @internal
 */
namespace detail_units {

	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::nanoseconds) noexcept
	    -> auto {
		return "ns";
	}
	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::microseconds) noexcept
	    -> auto {
		return "us";
	}
	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::milliseconds) noexcept
	    -> auto {
		return "ms";
	}

	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::seconds) noexcept
	    -> auto {
		return "s";
	}
	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::minutes) noexcept
	    -> auto {
		return "min";
	}
	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::hours) noexcept -> auto {
		return "hr";
	}

#	if KBLIB_USE_CXX20

	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::days) noexcept -> auto {
		return "ns";
	}
	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::weeks) noexcept -> auto {
		return "ns";
	}
	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::months) noexcept
	    -> auto {
		return "ns";
	}
	KBLIB_NODISCARD constexpr auto unit_of(std::chrono::years) noexcept -> auto {
		return "ns";
	}

#	endif

	struct KBLIB_NODISCARD prefix {
		char name[16];
		char abbr[4];
	};
// if std::intmax_t can represent the denominator
#	if (-1U >> 63) > (1U << 18)
	constexpr auto name_of(std::yocto) -> prefix { return prefix{"yocto", "y"}; }
#	endif
#	if (-1U >> 63) > (1U << 8)
	constexpr auto name_of(std::zepto) -> prefix { return prefix{"zepto", "z"}; }
#	endif
	constexpr auto name_of(std::atto) -> prefix { return prefix{"atto", "a"}; }
	constexpr auto name_of(std::femto) -> prefix { return prefix{"femto", "f"}; }
	constexpr auto name_of(std::pico) -> prefix { return prefix{"pico", "p"}; }
	constexpr auto name_of(std::nano) -> prefix { return prefix{"nano", "n"}; }
	constexpr auto name_of(std::micro) -> prefix { return prefix{"micro", "u"}; }
	constexpr auto name_of(std::milli) -> prefix { return prefix{"milli", "m"}; }
	constexpr auto name_of(std::centi) -> prefix { return prefix{"centi", "c"}; }
	constexpr auto name_of(std::deci) -> prefix { return prefix{"deci", "d"}; }

	constexpr auto name_of(std::ratio<1, 1>) -> prefix { return prefix{"", ""}; }

	constexpr auto name_of(std::deca) -> prefix { return prefix{"deca", "da"}; }
	constexpr auto name_of(std::hecto) -> prefix { return prefix{"hecto", "h"}; }
	constexpr auto name_of(std::kilo) -> prefix { return prefix{"kilo", "k"}; }
	constexpr auto name_of(std::mega) -> prefix { return prefix{"mega", "M"}; }
	constexpr auto name_of(std::giga) -> prefix { return prefix{"giga", "G"}; }
	constexpr auto name_of(std::tera) -> prefix { return prefix{"tera", "T"}; }
	constexpr auto name_of(std::peta) -> prefix { return prefix{"peta", "P"}; }
	constexpr auto name_of(std::exa) -> prefix { return prefix{"exa", "E"}; }
// if std::intmax_t can represent the numerator
#	if (-1U >> 63) > (1U << 8)
	constexpr auto name_of(std::zetta) -> prefix { return prefix{"zetta", "Z"}; }
#	endif
#	if (-1U >> 63) > (1U << 18)
	constexpr auto name_of(std::yotta) -> prefix { return prefix{"yotta", "Y"}; }
#	endif

	KBLIB_NODISCARD constexpr auto largest_power_1000(std::intmax_t in) -> int {
		if (in % 1000 == 0) {
			return 1 + largest_power_1000(in / 1000);
		} else {
			return 0;
		}
	}

	KBLIB_NODISCARD constexpr auto largest_power_1000_p(double in) -> int {
		if (in / 1000 >= 1) {
			return 1 + largest_power_1000_p(in / 1000.);
		} else {
			return 0;
		}
	}
	KBLIB_NODISCARD constexpr auto largest_power_1000(double in) -> int {
		if (in < 1) {
			return -largest_power_1000_p(1 / in);
		}
		if (in / 1000 >= 1) {
			return 1 + largest_power_1000_p(in / 1000.);
		} else {
			return 0;
		}
	}

	KBLIB_NODISCARD constexpr auto pow1000(int p) -> double {
		auto r = 1.0;
		if (p >= 0) {
			while (p--) {
				r *= 1000.;
			}
		} else {
			while (p++) {
				r /= 1000.;
			}
		}
		return r;
	}

	template <typename R>
	struct is_si_ratio : std::false_type {};
// if std::intmax_t can represent the denominator
#	if (-1U >> 63) > (1U << 18)
	template <>
	struct is_si_ratio<std::yocto> : std::true_type {};
#	endif
#	if (-1U >> 63) > (1U << 8)
	template <>
	struct is_si_ratio<std::zepto> : std::true_type {};
#	endif
	template <>
	struct is_si_ratio<std::atto> : std::true_type {};
	template <>
	struct is_si_ratio<std::femto> : std::true_type {};
	template <>
	struct is_si_ratio<std::pico> : std::true_type {};
	template <>
	struct is_si_ratio<std::nano> : std::true_type {};
	template <>
	struct is_si_ratio<std::micro> : std::true_type {};
	template <>
	struct is_si_ratio<std::milli> : std::true_type {};
	template <>
	struct is_si_ratio<std::centi> : std::true_type {};
	template <>
	struct is_si_ratio<std::deci> : std::true_type {};

	template <>
	struct is_si_ratio<std::ratio<1>> : std::true_type {};

	template <>
	struct is_si_ratio<std::deca> : std::true_type {};
	template <>
	struct is_si_ratio<std::hecto> : std::true_type {};
	template <>
	struct is_si_ratio<std::kilo> : std::true_type {};
	template <>
	struct is_si_ratio<std::mega> : std::true_type {};
	template <>
	struct is_si_ratio<std::giga> : std::true_type {};
	template <>
	struct is_si_ratio<std::tera> : std::true_type {};
	template <>
	struct is_si_ratio<std::peta> : std::true_type {};
	template <>
	struct is_si_ratio<std::exa> : std::true_type {};
// if std::intmax_t can represent the numerator
#	if (-1U >> 63) > (1U << 8)
	template <>
	struct is_si_ratio<std::zetta> : std::true_type {};
#	endif
#	if (-1U >> 63) > (1U << 18)
	template <>
	struct is_si_ratio<std::yotta> : std::true_type {};
#	endif

	template <typename M>
	struct KBLIB_NODISCARD unit_conversion {
		const char* scale_prefix;
		char abbr[6];
		M multiplier;
	};

	template <std::intmax_t Num, std::intmax_t Den>
	constexpr auto ratio_to_SI() noexcept -> unit_conversion<std::intmax_t> {
		return {};
	}

	template <std::intmax_t Num, std::intmax_t Den>
	struct nearest_ratio {};

	template <std::intmax_t Num, std::intmax_t Den>
	using nearest_ratio_t = typename nearest_ratio<Num, Den>::type;

} // namespace detail_units
// TODO: duration_to_str autoscaling
template <typename Rep, typename Ratio,
          enable_if_t<detail_units::is_si_ratio<
              typename Ratio::type>::value>* = nullptr>
KBLIB_NODISCARD constexpr auto duration_to_str(
    std::chrono::duration<Rep, Ratio>& d) -> std::string {
	using ratio = typename Ratio::type;
	auto n = detail_units::name_of(ratio{});
	return concat(d.count() / (static_cast<double>(ratio{}.num) / ratio{}.den),
	              ' ', n.abbr, 's');
}

template <typename Rep, typename Ratio,
          enable_if_t<not detail_units::is_si_ratio<typename Ratio::type>::value
                      and std::is_floating_point<Rep>::value>* = nullptr>
KBLIB_NODISCARD constexpr auto duration_to_str(
    std::chrono::duration<Rep, Ratio>& d) -> std::string {
	using ratio = typename Ratio::type;
	using n_r = detail_units::nearest_ratio_t<ratio::num, ratio::den>;
	auto u = detail_units::name_of(n_r{});

	// require an implicit cast
	std::chrono::duration<Rep, n_r> n_d = d;
	return concat(n_d.count(), ' ', u.abbr, 's');
}

template <typename Rep>
KBLIB_NODISCARD constexpr auto duration_to_str(
    std::chrono::duration<Rep, std::ratio<60>> d) -> std::string {
	return concat(d.count(), " min");
}
template <typename Rep>
KBLIB_NODISCARD constexpr auto duration_to_str(
    std::chrono::duration<Rep, std::ratio<3600>> d) -> std::string {
	return concat(d.count(), " hr");
}

template <typename string>
KBLIB_NODISCARD auto url_encode(const string& value) -> std::string {
	std::ostringstream escaped;
	escaped.fill('0');
	escaped << std::hex;

	for (char c : value) {
		// Keep alphanumeric and other accepted characters intact
		if (std::isalnum(c) or c == '-' or c == '_' or c == '.' or c == '~') {
			escaped << c;
		} else {
			// Any other characters are percent-encoded
			escaped << std::uppercase;
			escaped << '%' << std::setw(2) << int(to_unsigned(c));
			escaped << std::nouppercase;
		}
	}

	return escaped.str();
}

template <typename string>
KBLIB_NODISCARD constexpr auto html_encode(const string& data) -> std::string {
	std::string buffer;
	// Arbitrary estimate for amount of growth caused by the escaping is 12.5%.
	buffer.reserve(data.size() + data.size() / 8);
	for (char c : data) {
		switch (c) {
		case '&':
			buffer.append("&amp;");
			break;
		case '\"':
			buffer.append("&quot;");
			break;
		case '\'':
			buffer.append("&apos;");
			break;
		case '<':
			buffer.append("&lt;");
			break;
		case '>':
			buffer.append("&gt;");
			break;
		default:
			buffer.push_back(c);
			break;
		}
	}
	return buffer;
}

KBLIB_NODISCARD constexpr auto escapify(char c) -> std::string {
	auto value = to_unsigned(c);
	if (value < ' ' or value == '\x7F' or value & to_unsigned('\x80')) {
		constexpr std::array<char, 16> digits{
		    remove_null_terminator("0123456789ABCDEF")};
		std::string rc("\\x  ");
		rc[2] = digits[value >> 4u];
		rc[3] = digits[value & 15u];
		return rc;
	} else {
		return std::string(1, static_cast<char>(value));
	}
}

// Accepts any sequence of char, returns printable string
template <typename string>
KBLIB_NODISCARD auto escapify(const string& value) -> std::string {
	std::ostringstream ret;
	for (char c : value) {
		if (c < ' ' or c >= '\x7F') {
			ret << escapify(c);
		} else {
			ret << c;
		}
	}
	return ret.str();
}

// Given a string and a pointer into it, calculate the effective index of that
// pointer into a string such as created by kblib::escapify(value)
template <typename string>
KBLIB_NODISCARD constexpr auto calculate_translated_index(string&& value,
                                                          const char* p)
    -> std::ptrdiff_t {
	std::ptrdiff_t counter = 0;
	for (auto&& c : value) {
		if (&c == p) {
			return counter;
		}
		counter += (std::isprint(c)) ? 1 : 4;
	}
	return counter;
}

KBLIB_NODISCARD constexpr auto calculate_translated_index(const char* value,
                                                          const char* p)
    -> std::ptrdiff_t {
	if (not value) {
		throw std::invalid_argument(
		    "calculate_translated_index can't take a nullptr");
	}
	std::ptrdiff_t counter = 0;
	while (*value) {
		if (value == p) {
			return counter;
		}
		counter += (std::isprint(*value)) ? 1 : 4;
	}
	return counter;
}

template <typename character, enable_if_t<is_character_v<character>>* = nullptr>
KBLIB_NODISCARD constexpr auto quoted(character c) -> std::string {
	if (c < ' ' or c >= '\x7F') {
		return escapify(c);
	} else if (c == '"') {
		return "\\\"";
	} else if (c == '\\') {
		return "\\\\";
	} else {
		return {1, c};
	}
}

template <typename string, enable_if_t<not is_character_v<string>>* = nullptr>
KBLIB_NODISCARD auto quoted(string&& in) -> std::string {
	std::ostringstream ret;
	ret << '"';
	for (char c : in) {
		if (c < ' ' or c >= '\x7F') {
			ret << escapify(c);
		} else if (c == '"') {
			ret << "\\\"";
		} else if (c == '\\') {
			ret << "\\\\";
		} else {
			ret << c;
		}
	}
	ret << '"';
	return ret.str();
}

// This only uses RTTI because C++ has no other means to get "int" from a
// template parameter.
template <typename T>
KBLIB_NODISCARD auto fromStr(const std::string& val,
                             const char* type = typeid(T).name()) -> T {
	std::stringstream ss(val);
	T ret{};
	if (not (ss >> std::boolalpha >> ret).fail()) {
		return ret;
	} else {
		throw std::runtime_error(kblib::quoted(val) + " is not a " + type);
	}
}
template <>
KBLIB_NODISCARD constexpr auto fromStr(const std::string& val, const char*)
    -> std::string {
	return val;
}
template <>
KBLIB_NODISCARD constexpr auto fromStr(const std::string& val, const char* type)
    -> bool {
	if (val == "1" or val == "true") {
		return true;
	} else if (val == "0" or val == "false") {
		return false;
	} else {
		throw std::runtime_error(kblib::quoted(val) + " is not a " + type);
	}
}

template <typename T>
KBLIB_NODISCARD auto fromStr(std::string&& val,
                             const char* type = typeid(T).name()) -> T {
	std::stringstream ss(val);
	T ret;
	if (not (ss >> std::boolalpha >> ret).fail()) {
		return ret;
	} else {
		throw std::runtime_error(kblib::quoted(val) + " is not a " + type);
	}
}
template <>
KBLIB_NODISCARD constexpr auto fromStr(std::string&& val, const char*)
    -> std::string {
	return std::move(val);
}
template <>
KBLIB_NODISCARD constexpr auto fromStr(std::string&& val, const char* type)
    -> bool {
	if (val == "1" or val == "true") {
		return true;
	} else if (val == "0" or val == "false") {
		return false;
	} else {
		throw std::runtime_error(kblib::quoted(val) + " is not a " + type);
	}
}

#	if KBLIB_USE_STRING_VIEW

template <>
KBLIB_NODISCARD constexpr auto fromStr(const std::string& val, const char*)
    -> std::string_view {
	return val;
}
template <>
inline auto fromStr(std::string&&, const char*) -> std::string_view = delete;

template <typename T>
KBLIB_NODISCARD auto fromStr(std::string_view val,
                             const char* type = typeid(T).name()) -> T {
#		if KBLIB_USE_SPANSTREAM
	std::ispanstream ss(std::span<const char>(val.data(), val.size()));
#		else
	std::istrstream ss(val.data(), kblib::to_signed(val.size()));
#		endif
	T ret;
	if (not (ss >> std::boolalpha >> ret).fail()) {
		return ret;
	} else {
		throw std::runtime_error(kblib::quoted(val) + " is not a " + type);
	}
}
template <>
KBLIB_NODISCARD constexpr auto fromStr(std::string_view val, const char*)
    -> std::string_view {
	return val;
}
template <>
KBLIB_NODISCARD constexpr auto fromStr(std::string_view val, const char*)
    -> std::string {
	return std::string(val);
}
template <>
KBLIB_NODISCARD constexpr auto fromStr(std::string_view val, const char* type)
    -> bool {
	if (val == "1" or val == "true") {
		return true;
	} else if (val == "0" or val == "false") {
		return false;
	} else {
		throw std::runtime_error("\"" + std::string(val) + "\" is not a " + type);
	}
}

template <typename To, std::size_t N>
KBLIB_NODISCARD constexpr auto fromStr(const char (&val)[N],
                                       const char* type = typeid(To).name())
    -> To {
	// N - 1: remove null terminator
	return fromStr<To>(std::string_view(val, N - 1), type);
}

template <typename To, typename _>
KBLIB_NODISCARD constexpr auto fromStr(const char* val,
                                       const char* type = typeid(To).name(),
                                       _ = 0) -> To {
	return fromStr<To>(std::string_view(val), type);
}

#	endif

template <typename T>
KBLIB_NODISCARD auto toStr(T val) -> std::string {
	std::stringstream ss;
	ss << val;
	return ss.str();
}
KBLIB_NODISCARD constexpr auto toStr(std::string val) -> std::string {
	return val;
}

template <typename To, typename From>
struct lexical_caster {
	static auto cast(const From& val, const char* type) -> To {
		std::stringstream ss;
		ss << val;
		To ret;
		if (not (ss >> ret).fail()) {
			return ret;
		} else {
			throw std::runtime_error("Cannot convert \"" + toStr(val) + "\" to "
			                         + type);
		}
	}
};

template <typename Same>
struct lexical_caster<Same, Same> {
	static constexpr auto cast(const Same& val, const char*) -> Same {
		return val;
	}
};

template <>
struct lexical_caster<std::string, std::string> {
	static constexpr auto cast(const std::string& val, const char*)
	    -> std::string {
		return val;
	}
};

template <typename From>
struct lexical_caster<std::string, From> {
	static constexpr auto cast(const From& val, const char*) -> std::string {
		return toStr(val);
	}
};

template <typename To>
struct lexical_caster<To, std::string> {
	static constexpr auto cast(const std::string& val, const char* type) -> To {
		return fromStr<To>(val, type);
	}
};

#	if KBLIB_USE_STRING_VIEW

template <>
struct lexical_caster<std::string_view, std::string_view> {
	static constexpr auto cast(const std::string_view& val, const char*)
	    -> std::string_view {
		return val;
	}
};

template <>
struct lexical_caster<std::string_view, std::string> {
	static constexpr auto cast(const std::string& val, const char*)
	    -> std::string_view {
		return val;
	}
};

template <typename From>
struct lexical_caster<std::string_view, From> {
	static constexpr std::enable_if_t<
	    std::is_convertible_v<From, std::string_view>, std::string_view>
	cast(const From& val, const char*) {
		return From(val);
	}

	// DCL50-CPP-EX2:
	// As stated in the normative text, C-style variadic functions that are
	// declared but never defined are permitted.
	auto cast(...) -> std::string_view = delete;
};

template <typename To>
struct lexical_caster<To, std::string_view> {
	static constexpr auto cast(std::string_view val, const char* type) -> To {
		return fromStr<To>(val, type);
	}
};

#	endif

template <typename To, typename From>
KBLIB_NODISCARD constexpr auto lexical_cast(const From& val,
                                            const char* type
                                            = typeid(To).name()) -> To {
	return lexical_caster<To, From>::cast(val, type);
}

#	if 0
template <typename To, typename From>
KBLIB_NODISCARD constexpr auto lexical_cast(const From& val,
                                  const char* type = typeid(To).name()) -> To {
	using namespace std::literals;
	if constexpr (std::is_same_v<std::decay_t<To>, std::decay_t<From>>) {
		return val;
	} else if constexpr (std::is_same_v<std::decay_t<To>, std::string>) {
		return toStr(val);
	} else if constexpr (std::is_same_v<std::decay_t<From>, std::string>) {
		return fromStr<To>(val, type);
	} else {
		std::stringstream ss;
		ss << val;
		To ret;
		if (not(ss >> ret).fail())
			return ret;
		else
			throw std::runtime_error("Cannot convert \""s + toStr(val) + "\" to " +
			                         type);
	}
}
#	endif

} // namespace KBLIB_NS

#endif // KBLIB_CONVERT_H
