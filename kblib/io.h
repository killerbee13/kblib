#ifndef KBLIB_IO_H
#define KBLIB_IO_H

#include "fakestd.h"
#include "traits.h"

#include <fstream>
#include <functional>
#include <string>

#if KBLIB_USE_CXX17
#include <optional>
#endif

#include <iostream>

namespace kblib {

#if KBLIB_USE_CXX17
/**
 * @brief Read the entire contents of a file into a container, such as
 * std::string or std::vector<char>.
 *
 * @param filename The filename to open.
 * @tparam D A contiguous sequence container, which will be created and filled
 * with the contents of the file to be read.
 * @return std::optional<D> The contents of the file, if reading was successful.
 */
template <typename D = std::string, typename string,
          typename std::enable_if_t<is_contiguous_v<D>, int> = 0>
std::optional<D> get_file_contents(const string& filename) {
	static_assert(std::is_trivially_copyable_v<typename D::value_type>,
	              "D must be a sequence of trivial types");
	static_assert(sizeof(typename D::value_type) == 1,
	              "D must be a sequence of char-sized objects.");
	std::ifstream in(filename, std::ios::in | std::ios::binary);
	if (in) {
		D contents;
		in.seekg(0, std::ios::end);
		auto size = in.tellg();
		contents.resize(size);
		in.seekg(0, std::ios::beg);
		in.read(reinterpret_cast<char*>(contents.data()), size);
		in.close();
		return contents;
	} else {
		return std::nullopt;
	}
}

/**
 * @brief Read the entire contents of a file into a container, such as
 * std::string or std::vector<char>.
 *
 * This overload implements support for non-contiguous containers, such as
 * std::deque<char>. Note that it will be less efficient than for contiguous
 * containers.
 *
 * @param filename The filename to open.
 * @tparam D A non-contiguous sequence container, which will be created and
 * filled with the contents of the file to be read.
 * @return std::optional<D> The contents of the file, if reading was successful.
 */
template <typename D = std::string, typename string,
          typename std::enable_if_t<not is_contiguous_v<D>, int> = 0>
std::optional<D> get_file_contents(const string& filename) {
	static_assert(std::is_trivially_copyable_v<typename D::value_type>,
	              "D must be a sequence of trivial types");
	static_assert(sizeof(typename D::value_type) == 1,
	              "D must be a sequence of char-sized objects.");
	std::ifstream in(filename, std::ios::in | std::ios::binary);
	if (in) {
		D contents;
		in.seekg(0, std::ios::end);
		try_reserve(contents, in.tellg());
		in.seekg(0, std::ios::beg);
		std::copy((std::istreambuf_iterator<char>(in)),
		          std::istreambuf_iterator<char>(), std::back_inserter(contents));
		in.close();
		return contents;
	} else {
		return std::nullopt;
	}
}
#endif

/**
 * @brief By-value std::getline wrapper.
 *
 * @param is The stream to extract from.
 * @return std::string A single line of text from the stream.
 */
inline std::string getline(std::istream& is) {
	std::string ret;
	std::getline(is, ret);
	return ret;
}

/**
 * @brief Consume all non-spaces to first break, then eat that, too.
 *
 * @param is
 * @return std::istream
 */
inline std::istream& eat_word(std::istream& is) {
	do {
		is.get();
	} while (is and not std::isspace(is.peek()));
	return is;
}

/**
 * @brief Eat spaces, don't eat an extra.
 *
 * @deprecated Use std::ws instead.
 *
 * @param is
 * @return std::istream
 */
[[deprecated("Use std::ws instead")]] inline std::istream&
eat_space(std::istream& is) {
	while (is and std::isspace(is.peek())) {
		is.get();
	}
	return is;
}

/**
 * @brief Read in spaces until the end of the line is found.
 *
 * nl may be used to consume whitespace left over after a formatted
 * input operation before doing an unformatted input operation (such as
 * std::getline).
 *
 * @remark Example usage:
 * @code
 * int x{};
 * std::cout << "Enter a number: ";
 * std::cin >> x; // potentially leaves a new line in the stream
 * std::cout << "Got " << x << '\n';
 * std::string str;
 * std::cout << "Enter a line of text: ";
 * std::getline(std::cin >> kblib::nl, str);
 * std::cout << "Got " << std::quoted(str) << '\n';
 * @endcode
 *
 * @param is The stream to read from.
 * @return std::istream& is.
 */
template <typename CharT, typename Traits>
auto nl(std::basic_istream<CharT, Traits>& is)
    -> std::basic_istream<CharT, Traits>& {
	auto n = static_cast<typename Traits::int_type>(is.widen('\n'));
	for (typename Traits::int_type c = is.peek();
	     is and c != kblib::eof<CharT> and
	     std::isspace(static_cast<CharT>(c), is.getloc()) and c != n;
	     c = is.peek()) {
		is.ignore();
	}
	if (is.peek() == n) {
		is.ignore();
	}
	return is;
}

/**
 * @brief A helper class for wrapping stream manipulators.
 */
template <typename F>
struct get_manip {
	F _f;
};

template <typename T, typename U>
struct unicode_widen : std::false_type {};

template <>
struct unicode_widen<char16_t, char32_t> : std::true_type {};

#if KBLIB_USE_CHAR8_T

template <>
struct unicode_widen<char8_t, char16_t> : std::true_type {};

template <>
struct unicode_widen<char8_t, char32_t> : std::true_type {};

#endif

#if KBLIB_CHAR_IS_UTF8

template <>
struct unicode_widen<char, char16_t> : std::true_type {};

template <>
struct unicode_widen<char, char32_t> : std::true_type {};

#endif

template <typename T, typename U>
constexpr static bool unicode_widen_v = unicode_widen<T, U>::value;

/**
 * @brief Read a character from an input stream only if it equals c. Acts as an
 * UnformattedInputOperation, that is, it will not ignore any leading
 * whitespace.
 */
template <typename CharT>
auto unformatted_expect(CharT c) {
	auto _f = [c](auto& istream) -> decltype(istream) {
		using SCharT = typename std::decay<decltype(istream)>::type::char_type;
#if KBLIB_USE_CHAR8_t
		static_assert(std::is_same_v<CharT, char_type> or
		                  (not std::is_same_v<CharT, char8_t> and
		                   not std::is_same_v<char_type, char8_t>),
		              "No support for char8_t conversions.");
#endif
		auto widen_equal = [&](SCharT d) {
			// Feasible:
			// T     -> T      : c == d
			// T     -> char   : istream.widen(c) == d
			// u32   -> u16    : c == d
			// Not currently feasible:
			// SCharT == char  : read multiple chars and convert to CharT?
			// ...             :	convert between different wide char types?

			static_assert(
			    (std::is_same<CharT, SCharT>::value or
			     std::is_same<CharT, char>::value),
			    "Stream character type incompatible with argument type.");
			if constexpr (std::is_same<CharT, SCharT>::value) {
				return c == d;
			} else if (unicode_widen_v<CharT, SCharT>) {
				return c == d;
			} else {
				return istream.widen(c) == d;
			}
		};

		if (widen_equal(istream.peek())) {
			void(istream.get());
		} else {
			istream.setstate(std::ios_base::failbit);
		}
		return istream;
	};
	return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Read a character from an input stream only if it equals c. Acts as a
 * FormattedInputOperation, that is, leading whitespace is ignored.
 */
template <typename CharT>
auto expect(CharT c) {
	auto _f = [c](auto& istream) -> decltype(istream) {
		return istream >> std::ws >> unformatted_expect(c);
	};
	return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Read a whole line into a std::basic_string-like class template.
 *
 * When used like
 *  os >> get_line(str);
 * for a std::ostream& os and std::string str, reads a full line into str
 * instead of just a single word.
 *
 * @param str The string to read a line into.
 */
template <typename CharT, typename... O,
          template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str) {
	auto _f = [&](auto& istream) -> decltype(istream) {
		std::getline(istream, str);
		return istream;
	};
	return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Read a delimited string into a std::basic_string-like class template.
 *
 * When used like
 *  os >> get_line(str, '\n');
 * for a std::ostream& os and std::string str, reads a full line into str
 * instead of just a single word.
 *
 * @param str The string to read into.
 * @param delim The delimiter at which to stop reading text.
 */
template <typename CharT, typename... O,
          template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str, CharT delim) {
	auto _f = [&, delim](auto& istream) -> decltype(istream) {
		std::getline(istream, str, delim);
		return istream;
	};
	return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Actually calls the manipulator.
 */
template <typename F, typename CharT, typename Tr>
std::basic_istream<CharT, Tr>& operator>>(std::basic_istream<CharT, Tr>& is,
                                          get_manip<F> func) {
	return func._f(is);
}

/**
 * @brief Actually calls the manipulator.
 */
template <typename F, typename CharT, typename Tr>
std::basic_ostream<CharT, Tr>& operator<<(std::basic_ostream<CharT, Tr>& is,
                                          get_manip<F> func) {
	return func._f(is);
}

} // namespace kblib

#endif // KBLIB_IO_H
