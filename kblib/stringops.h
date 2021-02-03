#ifndef KBLIB_STRINGOPS_H
#define KBLIB_STRINGOPS_H

#include "algorithm.h"
#include "format.h"
#include "tdecl.h"
#include "traits.h"

#include <algorithm>
#include <initializer_list>
#include <numeric>
#include <string>
#include <type_traits>

#if KBLIB_USE_CXX17
#include <string_view>
#endif

namespace kblib {

#if true or KBLIB_USE_CXX17
/**
 * @brief Determine if the given type, ignoring const or reference qualifiers,
 * is a character type.
 *
 * Standard character types include char, wchar_t, char16_t, char32_t, and,
 * in C++20, char8_t.
 */
template <typename C>
struct is_character
    : detail::contains_type<std::tuple<char, wchar_t, char16_t, char32_t
#ifdef __cpp_char8_t
                                       ,
                                       char8_t
#endif
                                       >,
                            std::decay_t<C>> {
};

/**
 * @brief Equivalent to is_character<C>::value.
 */
template <typename C>
constexpr bool is_character_v = is_character<C>::value;

namespace detail {

	/**
	 * @brief Filter only arithmetic types.
	 *
	 * If T is an arithmetic type, provides the member type = T. Otherwise, type
	 * = void. The primary template is for non-arithmetic types.
	 */
	template <typename T, bool = std::is_arithmetic<T>::value>
	struct arithmetic_type {
		using type = void;
	};
	/**
	 * @brief Filter only arithmetic types.
	 *
	 * Provides the member type = T. This partial specialization is for
	 * arithmetic types.
	 */
	template <typename T>
	struct arithmetic_type<T, true> {
		using type = T;
	};
	/**
	 * @brief Equivalent to typename arithmetic_type<T>::type.
	 */
	template <typename T>
	using arithmetic_type_t = typename arithmetic_type<T>::type;

	/**
	 * @brief Converts arithmetic types to strings, but provides the identity
	 * transformation for all other types.
	 *
	 * This is primarily an implementation detail of concat, provided in the main
	 * namespace because it might be generally useful.
	 */
	template <typename T, typename = arithmetic_type_t<T>>
	struct str_type {
		/**
		 * @brief Arithmetic types can be converted into strings using the
		 * standard library.
		 */
		using type = std::string;
		/**
		 * @brief Forwards to std::to_string.
		 * @param in A numeric value to convert to a string.
		 * @return std::string A string representation of that number.
		 */
		static std::string convert(T in) { return std::to_string(in); }
	};
	/**
	 * @brief Performs a natural conversion to a stringlike type.
	 *
	 * A natural conversion for an arithmetic type is std::to_string. For any
	 * other type, there is no assumed transformation, so they are passed through
	 * unchanged.
	 *
	 * @note This is primarily an implementation detail of concat, provided in
	 * the main namespace because it might be generally useful. This partial
	 * specialization is for non-arithmetic types.
	 */
	template <typename T>
	struct str_type<T, void> {
		/**
		 * @brief Non-arithmetic types are either already stringlike, or have no
		 * natural conversion to std::string.
		 */
		using type = T;
		/**
		 * @brief Returns the argument unchanged.
		 */
		static type convert(T&& in) { return std::forward<T>(in); }
	};
	/**
	 * @brief Override for char to avoid conversion to integer
	 */
	template <>
	struct str_type<char, char> {
		using type = char;
		static char convert(char in) { return in; }
	};
	/**
	 * @brief Override for wchar_t to avoid conversion to integer
	 */
	template <>
	struct str_type<wchar_t, wchar_t> {
		using type = wchar_t;
		static wchar_t convert(wchar_t in) { return in; }
	};
	/**
	 * @brief Override for char16_t to avoid conversion to integer
	 */
	template <>
	struct str_type<char16_t, char16_t> {
		using type = char16_t;
		static char16_t convert(char16_t in) { return in; }
	};
	/**
	 * @brief Override for char32_t to avoid conversion to integer
	 */
	template <>
	struct str_type<char32_t, char32_t> {
		using type = char32_t;
		static char32_t convert(char32_t in) { return in; }
	};
#ifdef __cpp_char8_t
	/**
	 * @brief Override for char8_t to avoid conversion to integer
	 */
	template <>
	struct str_type<char8_t, char8_t> {
		using type = char8_t;
		static char8_t convert(char8_t in) { return in; }
	};
#endif
	/**
	 * @brief Provides the natural stringlike type for representing a T.
	 */
	template <typename T>
	using str_type_t = typename str_type<T>::type;

} // namespace detail

#if KBLIB_USE_CXX17
/**
 * @brief Determines the size in characters of any valid argument to concat or
 * append.
 * @param str A value of any stringlike or arithmetic type to count the
 * characters of.
 * @return std::size_t The number of characters needed to represent str.
 */
template <typename Str>
std::size_t strsize(Str&& str) {
	if constexpr (std::is_array_v<std::remove_reference_t<Str>>) {
		return fakestd::size(str);
	} else if constexpr (std::is_pointer_v<std::decay_t<Str>>) {
		return std::char_traits<std::decay_t<decltype(*str)>>::length(str);
	} else if constexpr (is_character_v<std::decay_t<Str>>) {
		return 1;
	} else if constexpr (std::is_integral_v<std::decay_t<Str>>) {
		return count_digits(str);
	} else {
		return fakestd::size(str);
	}
}

/**
 * @brief Given an object out of resizable stringlike type string, appends all
 * other arguments to it.
 *
 * Stringlike types and characters are simply appended, while arithmetic types
 * are first converted to strings using std::to_string.
 *
 * @param out The string to append to.
 * @param f The first value to append to out.
 * @param tail Any number of subsequent values to append to out.
 */
template <typename string, typename F, typename... S>
void append(string&& out, F&& f, S&&... tail) {
	if constexpr (is_character_v<std::decay_t<F>>) {
		out.append(1, f);
	} else if constexpr (std::is_arithmetic_v<std::decay_t<F>>) {
		out.append(std::to_string(f));
	} else {
		out.append(f);
	}
	if constexpr (sizeof...(S) > 0) {
		append(out, tail...);
	}
	return;
}

namespace detail {

	template <typename string, typename... S, std::size_t... I>
	string concat_impl(std::index_sequence<I...>, S&&... ins) {
		std::tuple<detail::str_type_t<S>...> buf(
		    detail::str_type<S>::convert(std::forward<S>(ins))...);
		string ret;
		std::size_t size = (strsize(std::get<I>(buf)) + ...);
		ret.reserve(size);
		append(ret, std::get<I>(buf)...);
		return ret;
	}

} // namespace detail

/**
 * @brief Returns a string consisting of the concatenation of all arguments.
 *
 * Arithmetic types are first converted by calling std::to_string.
 *
 * @param f The first argument to concatenate.
 * @param ins Any number of arguments to concatenate onto f.
 * @return string A string containing the concatenated values of all the
 * arguments.
 */
template <typename string = std::string, typename F, typename... S>
string concat(F&& f, S&&... ins) {
	return detail::concat_impl<string>(
	    std::make_index_sequence<1 + sizeof...(S)>{}, std::forward<F>(f),
	    std::forward<S>(ins)...);
}

/**
 * @brief Returns a string consisting of the concatenation of all elements of an
 * initializer list.
 * @param ins A series of values to concatenate together.
 * @return string A string containing the concatenated values of all the
 * arguments.
 */
template <typename string = std::string, typename str>
string concat(std::initializer_list<str> ins) {
	string ret;
	ret.reserve(std::accumulate(
	    ins.begin(), ins.end(), std::size_t{0},
	    [](std::size_t z, const str& s) { return z + strsize(s); }));
	for (auto&& s : ins) {
		append(ret, s);
	}
	return ret;
}
#endif

inline bool isspace(char c) { return std::isspace(to_unsigned(c)); }
inline bool isspace(wchar_t c) { return iswspace(to_unsigned(c)); }

struct is_space {
	bool operator()(char c) { return isspace(c); }
	bool operator()(wchar_t c) { return isspace(c); }
};

/**
 * @brief Concatenates all elements of a range together with an optional joiner.
 *
 * range must support iteration and be supported by fakestd::size().
 *
 * @param in A sequence of strings to concatenate.
 * @param joiner A string which will be inserted between every element of in.
 * @return string The joined string.
 */
template <typename range, typename string = std::string>
string join(const range& in, const string& joiner = "") {
	if (in.size() == 0) {
		return {};
	} else if (fakestd::size(in) == 1) {
		return *in.begin();
	} else {
		return std::accumulate(
		    std::next(std::begin(in)), std::end(in), *in.begin(),
		    [&joiner](const string& a, const string& b) -> string {
			    return concat(a, joiner, b);
		    });
	}
}
#endif // KBLIB_USE_CXX17

/**
 * @brief Split a string on all condensed delimiters.
 *
 * @param in The string to split
 * @param spacer A predicate which determines whether a character is a
 * delimiter.
 * @return Container A sequence container of all substrings in the split input.
 */
template <typename Container = std::vector<std::string>, typename Predicate,
          typename String>
return_assert_t<
    is_callable<Predicate, typename Container::value_type::value_type>::value,
    Container>
split_tokens(const String& in, Predicate spacer) {
	Container ret{};
	bool delim_run = true;
	const char* begpos{};
	auto endpos = begpos;
	for (const auto& c : in) {
		if (delim_run) {
			// keep begpos updated as long as in a delimiter run
			begpos = &c;
		}
		if (spacer(c) and not std::exchange(delim_run, true)) {
			// c is first of a run of delimiters
			ret.emplace_back(begpos, &c - begpos);
		} else if (not spacer(c)) {
			// c is not a delimiter
			delim_run = false;
		}
		endpos = &c;
	}
	if (not delim_run and begpos != endpos) {
		ret.emplace_back(&*begpos, endpos - begpos + 1);
	}
	return ret;
}

template <typename Container = std::vector<std::string>, typename String>
Container split_tokens(const String& in) {
	return split_tokens(in, is_space{});
}

/**
 * @brief Split a string on all instances of a delimiter.
 *
 * @param in The string to split
 * @param delim The character to split on. A run of delimiters is condensed.
 * @return Container A sequence container of all substrings in the split input.
 */
template <typename Container = std::vector<std::string>, typename String>
Container split_tokens(const String& in,
                       typename Container::value_type::value_type delim) {
	Container ret{};
	bool delim_run = true;
	using CharT = typename Container::value_type::value_type;
	const CharT* begpos{};
	auto endpos = begpos;
	for (const CharT& c : in) {
		if (delim_run) {
			// keep begpos updated as long as in a delimiter run
			begpos = &c;
		}
		if (c == delim and not std::exchange(delim_run, true)) {
			// c is first of a run of delimiters
			ret.emplace_back(begpos, &c - begpos);
		} else if (c != delim) {
			// c is not a delimiter
			delim_run = false;
		}
		endpos = &c;
	}
	if (not delim_run and begpos != endpos) {
		ret.emplace_back(&*begpos, endpos - begpos + 1);
	}
	return ret;
}

template <typename Container = std::vector<std::string>, typename String>
Container kbsplit2(const String& in, char delim = ' ') {
	Container ret{""};
	bool delim_run = true;
	for (char c : in) {
		if (c == delim and not std::exchange(delim_run, true)) {
			// c is first of a run of delimiters
			ret.emplace_back();
		} else if (c != delim) {
			// c is not a delimiter
			delim_run = false;
			ret.back().push_back(c);
		}
	}
	if (ret.back().empty()) {
		ret.pop_back();
	}
	return ret;
}

/**
 * @brief Split a string on all instances of delim.
 *
 * @param in The string to split
 * @param delim The character to split on.
 * @return Container A sequence container of all substrings in the split input.
 */
template <typename Container = std::vector<std::string>, typename String>
Container split_dsv(const String& str, char delim) {
	Container ret;
	for (std::size_t pos1{}, pos2{str.find(delim)}; pos1 != str.npos;) {
		ret.emplace_back(str, pos1, pos2 - pos1);
		pos1 = std::exchange(pos2, str.find(delim, pos2 + 1));
		if (pos1 != str.npos) {
			++pos1;
		}
	}
	return ret;
}

/**
 * @brief Split a string on all instances of delim.
 *
 * @param in The string to split
 * @param delim A predicate for delimiters.
 * @return Container A sequence container of all substrings in the split input.
 */
template <typename Container = std::vector<std::string>, typename String,
          typename Predicate>
return_assert_t<
    is_callable<Predicate, typename Container::value_type::value_type>::value,
    Container>
split_dsv(const String& str, Predicate delim) {
	Container ret;
	for (std::size_t pos1{}, pos2{str.find(delim)}; pos1 != str.npos;) {
		ret.emplace_back(str, pos1, pos2 - pos1);
		pos1 = std::exchange(
		    pos2, kblib::find_in_if(str.begin() + pos1 + 1, str.end(), delim));
		if (pos1 != str.npos) {
			++pos1;
		}
	}
	return ret;
}

// TODO: figure out if any uses of reverseStr, toLower, toUpper exist in current
// projects

/**
 * @brief Reverses all the elements of its input.
 *
 * @attention This function will not behave correctly with multibyte character
 * encodings.
 *
 * @param val The string to reverse.
 * @return string The reversed range.
 */
template <typename string>
string reverse_str(string val) {
	std::reverse(val.begin(), val.end());
	return val;
}

/**
 * @brief Folds all characters in a string using the default execution character
 * set to lowercase.
 * @param str The string to case-fold.
 * @return string The case-folded string.
 */
template <typename string>
string tolower(string str) {
	std::transform(str.begin(), str.end(), str.begin(),
	               [](auto c) { return std::tolower(c); });
	return str;
}

/**
 * @brief Folds all characters in a string using the default execution character
 * set to uppercase.
 * @param str The string to case-fold.
 * @return string The case-folded string.
 */
template <typename string>
string toupper(string str) {
	std::transform(str.begin(), str.end(), str.begin(),
	               [](auto c) { return std::toupper(c); });
	return str;
}

/**
 * @brief Construct a string consisting of count copies of val concatenated
 * together.
 *
 * This function currently works greedily and will be inefficient for large
 * values of count.
 *
 * @param val
 * @param count
 * @todo Defer constrution of a string with a class.
 */
template <typename string>
string repeat(string val, std::size_t count) {
	string tmp;
	try_reserve(tmp, fakestd::size(val) * count);
	for (std::size_t i = 0; i < count; ++i) {
		tmp += val;
	}
	return tmp;
}
/**
 * @brief Construct a string consisting of count copies of val.
 *
 * This function is a trivial wrapper around a constructor of std::string
 * provided for symmetry with the above overload.
 *
 * @param val The character to be repeated.
 * @param count The number of times to repeat val.
 */
inline std::string repeat(char val, std::size_t count) {
	return std::string(count, val);
}

#if KBLIB_USE_STRING_VIEW

/**
 * @brief Checks if a given string ends with a particular string.
 * @param haystack The string to be checked.
 * @param needle The suffix to check for.
 * @return bool If haystack ends with needle.
 */
inline bool ends_with(std::string_view haystack, std::string_view needle) {
	return haystack.size() >= needle.size() and
	       haystack.compare(haystack.size() - needle.size(),
	                        std::string_view::npos, needle) == 0;
}

/**
 * @brief Checks if a given string ends with a particular string.
 * @param haystack The string to be checked.
 * @param needle The suffix to check for.
 * @return bool If haystack ends with needle.
 */
inline bool ends_with(std::string_view haystack, char needle) {
	return haystack.size() >= 1 and haystack.back() == needle;
}

/**
 * @brief Checks if a given string starts with a particular string.
 * @param haystack The string to be checked.
 * @param needle The prefix to check for.
 * @return bool If haystack starts with needle.
 */
inline bool starts_with(std::string_view haystack, std::string_view needle) {
	return haystack.size() >= needle.size() and
	       haystack.compare(0, needle.size(), needle) == 0;
}

/**
 * @brief Checks if a given string starts with a particular string.
 * @param haystack The string to be checked.
 * @param needle The prefix to check for.
 * @return bool If haystack starts with needle.
 */
inline bool starts_with(std::string_view haystack, char needle) {
	return haystack.size() >= 1 and haystack.front() == needle;
}

#endif

} // namespace kblib

#endif // KBLIB_STRINGOPS_H
