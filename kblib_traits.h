#ifndef KBLIB_TRAITS_H_INCLUDED_
#define KBLIB_TRAITS_H_INCLUDED_

#include <array>
#include <cstring>
#include <tuple>
#include <type_traits>

namespace kblib {
namespace detail {

// contains_types adapted from code by Maarten Bamelis,
// https://stackoverflow.com/a/42581257/1924641

template <typename Tuple, typename T>
struct contains_type;

template <typename T, typename U, typename... Ts>
struct contains_type<std::tuple<T, Ts...>, U>
    : contains_type<std::tuple<Ts...>, U> {};

template <typename T, typename... Ts>
struct contains_type<std::tuple<T, Ts...>, T> : std::true_type {};

template <typename T>
struct contains_type<std::tuple<>, T> : std::false_type {};

// -----

template <typename Lhs, typename Rhs>
struct contains_types;

template <typename Tuple, typename T, typename... Ts>
struct contains_types<Tuple, std::tuple<T, Ts...>>
    : std::integral_constant<
          bool, contains_type<Tuple, T>::value &&
                    contains_types<Tuple, std::tuple<Ts...>>::value> {};

template <typename Tuple>
struct contains_types<Tuple, std::tuple<>> : std::true_type {};

template <typename... Ts>
constexpr bool contains_types_v = contains_types<Ts...>::value;

template <typename T, int N, int... I>
constexpr auto trim_array(const T (&arr)[N],
                          std::integer_sequence<int, I...> Is)
    -> std::array<T, Is.size()> {
  return {arr[I]...};
}

}  // namespace detail

template <int trim, typename T, int N,
          typename Indices = std::make_integer_sequence<int, N - trim>>
constexpr std::array<T, N - trim> trim_array(const T (&arr)[N]) {
  return detail::trim_array(arr, Indices{});
}

template <int N, typename Indices = std::make_integer_sequence<int, N - 1>>
constexpr std::array<char, N - 1> remove_null_terminator(const char (&arr)[N]) {
  return detail::trim_array(arr, Indices{});
}

// Will be fully replaceable by std::bit_cast in C++20.
template <typename T, typename F>
T byte_cast(F v) {
  static_assert(
      sizeof(T) == sizeof(F),
      "Source and destination types for byte_cast must be the same size.");
  static_assert(
      std::is_trivially_copyable<T>::value &&
          std::is_trivially_copyable<F>::value,
      "Source and destination types for byte_cast must be trivially copyable.");
  T ret{};
  std::memcpy(&ret, &v, sizeof(T));
  return ret;
}

}  // namespace kblib

#endif  // KBLIB_TRAITS_H_INCLUDED_
