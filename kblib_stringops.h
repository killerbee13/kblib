#ifndef KBLIB_STRINGOPS_H
#define KBLIB_STRINGOPS_H

#include <algorithm>
#include <initializer_list>
#include <numeric>
#include <string>
#include <type_traits>

#if __cplusplus >= 201703L
#include <string_view>
#endif

#include "kblib_format.h"
#include "kblib_traits.h"

namespace kblib {

#if __cplusplus >= 201703L
template <typename C>
struct is_character
    : detail::contains_type<std::tuple<char, wchar_t, char16_t, char32_t>,
                            std::decay_t<C>> {};

template <typename C>
constexpr bool is_character_v = is_character<C>::value;

namespace detail {

template <typename T, bool = std::is_arithmetic_v<T>>
struct arithmetic_type {
using type = void;
};
template <typename T>
struct arithmetic_type<T, true> {
using type = T;
};
template <typename T>
using arithmetic_type_t = typename arithmetic_type<T>::type;

template <typename T, typename = arithmetic_type_t<T>>
struct str_type {
	using type = std::string;
	static std::string convert(T in) {
		return std::to_string(in);
	}
};
template <typename T>
struct str_type<T, void> {
	using type = T;
	static type convert(T&& in) {
		return std::forward<T>(in);
	}
};
template <typename T>
using str_type_t = typename str_type<T>::type;

}

template <typename Str>
std::size_t strsize(Str&& str) {
  if constexpr (std::is_array_v<std::remove_reference_t<Str>>) {
    return std::size(str);
  } else if constexpr (std::is_pointer_v<std::decay_t<Str>>) {
    return std::char_traits<std::decay_t<decltype(*str)>>::length(str);
  } else if constexpr (is_character_v<std::decay_t<Str>>) {
    return 1;
  } else if constexpr (std::is_integral_v<std::decay_t<Str>>) {
    return digitsOf(str);
  } else {
    return std::size(str);
  }
}

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
}

template <typename string, typename... S, std::size_t... I>
string concat_impl(std::index_sequence<I...>, S&&... ins) {
  std::tuple<detail::str_type_t<S>...> buf(detail::str_type<S>::convert(std::forward<S>(ins))...);
  string ret;
  std::size_t size = (strsize(std::get<I>(buf)) + ...);
  ret.reserve(size);
  append(ret, std::get<I>(buf)...);
  return ret;
}

template <typename string = std::string, typename F, typename... S>
string concat(F&& f, S&&... ins) {
  return concat_impl<string>(std::make_index_sequence<1 + sizeof...(S)>{}, f, ins...);
  string ret;
  ret.reserve((strsize(f) + ... + strsize(ins)));
  append(ret, f, ins...);
  return ret;
}

template <typename string = std::string, typename str>
string concat(std::initializer_list<str> ins) {
  string ret;
  ret.reserve(std::accumulate(ins.begin(), ins.end(), std::size_t{0},
                              [](std::size_t z, const str& s) { return z+strsize(s); }));
  for (auto&& s : ins) {
    ret.append(s);
  }
  return ret;
}
#endif

template <typename range, typename string = std::string>
string join(const range& in, const string& joiner = "") {
  if (in.size() == 0) {
    return {};
  } else if (in.size() == 1) {
    return *in.begin();
  } else {
    return std::accumulate(
        std::next(std::begin(in)), std::end(in), *in.begin(),
        [&joiner](const string& a, const string& b) -> string {
          return concat(a, joiner, b);
        });
  }
}

template <typename string>
string reverseStr(string val) {
  std::reverse(val.begin(), val.end());
  return val;
}

template <typename string>
string toLower(string str) {
  std::transform(str.begin(), str.end(), str.begin(), ::tolower);
  return str;
}

template <typename string>
string toUpper(string str) {
  std::transform(str.begin(), str.end(), str.begin(), ::toupper);
  return str;
}

// TODO: defer constrution of a string with a class

template <typename string>
inline auto repeat(string val, int count) {
  string tmp;
  for (int i = 0; i < count; ++i) {
    tmp += val;
  }
  return tmp;
}
inline auto repeat(char val, int count) {
  return std::string(count, val);
}

inline bool ends_with(std::string_view haystack, std::string_view needle) {
  return haystack.size() >= needle.size() &&
         haystack.compare(haystack.size() - needle.size(),
                          std::string_view::npos, needle) == 0;
}

inline bool starts_with(std::string_view haystack, std::string_view needle) {
  return haystack.size() >= needle.size() &&
         haystack.compare(0, needle.size(), needle) == 0;
}

}  // namespace kblib

#endif  // KBLIB_STRINGOPS_H
