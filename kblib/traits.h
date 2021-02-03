#ifndef KBLIB_TRAITS_H_INCLUDED_
#define KBLIB_TRAITS_H_INCLUDED_

/**
 * @file traits.h
 * @brief Contains some type traits not in the standard library that are useful
 * in the implementation of kblib.
 */

#include "fakestd.h"
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
	          bool, contains_type<Tuple, T>::value and
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
	 * @return std::array<T, Is.size()> An array consisting of the first N
	 * elements of arr.
	 */
	template <typename T, int N, int... I>
	constexpr auto trim_array(const T (&arr)[N],
	                          std::integer_sequence<int, I...>)
	    -> std::array<T, std::integer_sequence<int, I...>::size()> {
		return {arr[I]...};
	}

} // namespace detail

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

template <typename T>
struct is_unbounded_array : std::false_type {};
template <typename T>
struct is_unbounded_array<T[]> : std::true_type {};

template <typename T>
struct is_bounded_array : std::false_type {};
template <typename T, std::size_t N>
struct is_bounded_array<T[N]> : std::true_type {};

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
	static_assert(std::is_trivially_copyable<T>::value and
	                  std::is_trivially_copyable<F>::value,
	              "Source and destination types for byte_cast must be trivially "
	              "copyable.");
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
	/**
	 * True if and only if C is a resizable container.
	 */
	template <typename C>
	constexpr bool is_resizable_v = calc_resizable<C>();

	template <typename C>
	struct is_resizable {
		constexpr static bool value = is_resizable_v<C>;
	};

	template <typename C, typename = void>
	constexpr bool has_reserve_v = false;

	template <typename C>
	constexpr bool
	    has_reserve_v<C, void_t<decltype(std::declval<C&>().reserve(0))>> = true;
	/**
	 * @brief True if and only if C contains an accessible reserve() member.
	 */
	template <typename C>
	struct has_reserve : bool_constant<has_reserve_v<C>> {};

} // namespace detail

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
          typename std::enable_if<not detail::has_reserve_v<C>, int>::type = 0>
void try_reserve(C&, std::size_t) noexcept {
	return;
}

/**
 * @brief Type trait to determine if a container is contiguous.
 *
 */
template <typename C, typename = void>
constexpr bool is_contiguous_v = false;

template <typename C>
constexpr bool is_contiguous_v<C, void_t<decltype(std::declval<C&>().data())>> =
    true;

template <typename C>
struct is_contiguous : bool_constant<is_contiguous_v<C>> {};

template <typename T>
struct class_of;

template <typename T, typename M>
struct class_of<M T::*> {
	using type = T;
};

template <typename T>
using class_of_t = typename class_of<T>::type;

#if KBLIB_USE_CXX17

/**
 * @brief The type of data member pointed to by M.
 *
 */
/*template <typename T, auto M>
using member_t = std::remove_reference_t<decltype(std::declval<T&>().*M)>;*/

template <auto M>
using class_t = typename class_of<decltype(M)>::type;

#endif

template <typename T>
struct member_of;

template <typename T, typename M>
struct member_of<M T::*> {
	using type = M;
};

template <typename T>
using member_of_t = typename member_of<T>::type;

#if KBLIB_USE_CXX17
template <typename, auto M>
using member_t = member_of_t<decltype(M)>;
#endif

template <typename F>
struct return_type;

template <typename R, typename... Args>
struct return_type<R(Args...)> : meta_type<R> {};

template <typename R, typename... Args>
struct return_type<R(Args...) const> : meta_type<R> {};

template <typename R, typename... Args>
struct return_type<R(Args...) volatile> : meta_type<R> {};

template <typename R, typename... Args>
struct return_type<R(Args...) const volatile> : meta_type<R> {};

template <typename T>
struct exists : std::true_type {};

/**
 * @brief Type trait that determines the iterator type for a range.
 *
 */
template <typename Range>
struct iterator_type_for {
 private:
	static decltype(auto) begin(Range& c) {
		using std::begin;
		return begin(c);
	}

 public:
	using type = decltype(begin(std::declval<Range&>()));
};

template <typename Range>
using iterator_type_for_t = typename iterator_type_for<Range>::type;

template <typename Range, typename = void>
struct is_iterable : std::false_type {};

template <typename Range>
struct is_iterable<
    Range, void_if_t<std::is_base_of<
               std::forward_iterator_tag,
               typename std::iterator_traits<
                   typename Range::iterator>::iterator_category>::value>>
    : std::true_type {};

template <typename T, std::size_t N>
struct is_iterable<T[N], void> : std::true_type {};
template <typename T, std::size_t N>
struct is_iterable<T (&)[N], void> : std::true_type {};

template <typename T, typename = void>
struct is_iterator : std::false_type {};

template <typename T>
struct is_iterator<
    T,
    void_if_t<std::is_base_of<
                  std::input_iterator_tag,
                  typename std::iterator_traits<T>::iterator_category>::value or
              std::is_base_of<
                  std::output_iterator_tag,
                  typename std::iterator_traits<T>::iterator_category>::value>>
    : std::true_type {};

template <typename T>
constexpr bool is_iterator_v = is_iterator<T>::value;

/**
 * @brief Abbreviated name for std::is_reference<T>::value for C++14.
 *
 */
template <typename T>
constexpr bool is_reference_v = std::is_reference<T>::value;

/**
 * @brief Abbreviated name for std::remove_reference<T>::type for C++14.
 *
 */
template <typename T>
using remove_reference_t = typename std::remove_reference<T>::type;

/**
 * @brief Names the EOF value for the given character type in std::char_traits.
 */
template <typename CharT = char>
auto eof = std::char_traits<CharT>::eof();

template <typename T, T V>
struct type_constant {
	operator T() const noexcept { return V; }
};

#if KBLIB_USE_CXX17

template <auto V>
using type_constant_for = type_constant<decltype(V), V>;

#endif

template <typename T>
struct is_aliasing_type : std::false_type {};
template <>
struct is_aliasing_type<char> : std::true_type {};
template <>
struct is_aliasing_type<unsigned char> : std::true_type {};
#if KBLIB_USE_CXX17
template <>
struct is_aliasing_type<std::byte> : std::true_type {};
#endif

template <typename T>
constexpr bool is_aliasing_type_v = is_aliasing_type<T>::value;

} // namespace kblib

#endif // KBLIB_TRAITS_H_INCLUDED_
