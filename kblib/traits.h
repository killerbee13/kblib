#ifndef KBLIB_TRAITS_H_INCLUDED_
#define KBLIB_TRAITS_H_INCLUDED_

/**
 * @file
 * Contains some type traits not in the standard library that are useful in the
 * implementation of kblib.
 */

#include "tdecl.h"

#include <array>
#include <cstring>
#include <tuple>
#include <type_traits>

namespace kblib {
namespace detail {

// contains_types adapted from code by Maarten Bamelis,
// https://stackoverflow.com/a/42581257/1924641

/**
 * @brief Determines if T is a type in Tuple, which must be a std::tuple.
 */
template <typename Tuple, typename T>
struct contains_type;

template <typename T, typename U, typename... Ts>
struct contains_type<std::tuple<T, Ts...>, U>
    : contains_type<std::tuple<Ts...>, U> {};

template <typename T, typename... Ts>
struct contains_type<std::tuple<T, Ts...>, T> : std::true_type {};

template <typename T>
struct contains_type<std::tuple<>, T> : std::false_type {};

template <typename... Ts>
constexpr bool contains_type_v = contains_type<Ts...>::value;

/**
 * @brief Determines if Lhs contains all of the types in Rhs, where both are
 * std::tuples.
 */
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

/**
 * @brief Truncates an array to its first N elements.
 *
 * @param arr An array of at least N elements to truncate.
 * @param Is An implementation detail.
 * @return std::array<T, Is.size()> An array consisting of the first N elements
 * of arr.
 */
template <typename T, int N, int... I>
constexpr auto trim_array(const T (&arr)[N],
                          std::integer_sequence<int, I...> Is)
    -> std::array<T, Is.size()> {
  return {arr[I]...};
}

}  // namespace detail

template <int trim, typename T, int N,
          typename Indices = std::make_integer_sequence<int, N - trim>>
/**
 * @brief Truncates the last trim elements from an array.
 *
 * @param arr The array to trim.
 * @return std::array<T, N - trim> The trimmed array.
 */
constexpr std::array<T, N - trim> trim_array(const T (&arr)[N]) {
  return detail::trim_array(arr, Indices{});
}

template <int N, typename Indices = std::make_integer_sequence<int, N - 1>>
/**
 * @brief Creates an array of only the meaningful characters in a string
 * literal, and not the null terminator.
 *
 * @param arr A string literal to strip the null terminator from.
 * @return std::array<char, N - 1> A std::array of the meaningful characters of
 * the string literal.
 */
constexpr std::array<char, N - 1> remove_null_terminator(const char (&arr)[N]) {
  return detail::trim_array(arr, Indices{});
}

/**
 * @brief Creates a T with the same object representation as the given F.
 *
 * T and F must be the same size and must both be trivially copyable.
 *
 * @note Will be fully replaceable by std::bit_cast in C++20.
 *
 * @param v A value to reinterpret.
 * @return T The reinterpreted value.
 */
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

namespace detail {

template <typename C, typename = decltype(std::declval<C&>().resize(0))>
constexpr bool calc_resizable() noexcept {
  return true;
}

template <typename C, int = std::tuple_size<C>::value>
constexpr bool calc_resizable() noexcept {
  return false;
}

constexpr bool calc_resizable(...) noexcept { return false; }

// Note that when a type that is not resizable, but also doesn't have a
// constexpr size, is passed, there is a hard error.
template <typename C>
struct is_resizable {
  constexpr static bool value = calc_resizable<C>();
};
/**
 * True if and only if C is a resizable container.
 */
template <typename C>
constexpr bool is_resizable_v = is_resizable<C>::value;

template <typename C, typename = fakestd::void_t<>>
struct has_reserve {
  constexpr static bool value = false;
};

template <typename C>
struct has_reserve<C, fakestd::void_t<decltype(std::declval<C&>.reserve(0))>> {
  constexpr static bool value = true;
};
/**
 * @brief True if and only if C contains an accessible reserve() member.
 */
template <typename C>
constexpr bool has_reserve_v = has_reserve<C>::value;

}  // namespace detail

/**
 * @brief Attempt to reserve capacity in a container. No-op if unsupported.
 *
 * @param c The container to modify.
 * @param s The requested capacity.
 */
template <typename C,
          typename std::enable_if<detail::has_reserve_v<C>, int>::type = 0>
void try_reserve(C& c, std::size_t s) noexcept(noexcept(c.reserve(s))) {
  c.reserve(s);
  return;
}

/**
 * @brief Attempt to reserve capacity in a container. No-op if unsupported.
 *
 * @param c The container to modify.
 * @param s The requested capacity.
 */
template <typename C,
          typename std::enable_if<!detail::has_reserve_v<C>, int>::type = 0>
void try_reserve(C&, std::size_t) noexcept {
  return;
}

}  // namespace kblib

#endif  // KBLIB_TRAITS_H_INCLUDED_
