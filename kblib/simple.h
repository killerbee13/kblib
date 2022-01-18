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
 * @brief Provides general utilities which do not fit in any more specific
 * header.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_SIMPLE_H
#define KBLIB_SIMPLE_H

#include "iterators.h"

#include <bitset>
#include <climits>
#include <cstdint>
#include <initializer_list>
#include <limits>
#include <numeric>

#define KBLIB_DEBUG_LOG_RANGES 0
#if KBLIB_DEBUG_LOG_RANGES
#	include <iostream>
#endif

namespace kblib {

#if KBLIB_USE_CXX17

template <typename... Ts>
constexpr bool any_void = (std::is_void_v<Ts> or ...);

template <typename F, typename... T>
KBLIB_NODISCARD auto map(F f, T&&... t) noexcept(noexcept(std::tuple{
    kblib::apply(f, std::forward<T>(t))...}))
    -> enable_if_t<
        not any_void<decltype(kblib::apply(f, std::forward<T>(t)))...>,
        decltype(std::tuple{kblib::apply(f, std::forward<T>(t))...})> {
	return std::tuple{kblib::apply(f, std::forward<T>(t))...};
}

template <typename F, typename... T>
auto map(F f, T&&... t) noexcept(
    noexcept((static_cast<void>(kblib::apply(f, std::forward<T>(t))), ...)))
    -> enable_if_t<any_void<decltype(kblib::apply(f, std::forward<T>(t)))...>,
                   void> {
	(static_cast<void>(kblib::apply(f, std::forward<T>(t))), ...);
}
#endif

template <typename T>
struct KBLIB_NODISCARD RAII_wrapper {
	T t;
	~RAII_wrapper() noexcept(noexcept(t())) { t(); }

	RAII_wrapper(T&& t) : t(std::move(t)) {}
	RAII_wrapper(const T& t) : t(t) {}
	RAII_wrapper(const RAII_wrapper&) = delete;
	RAII_wrapper(RAII_wrapper&&) = delete;
	RAII_wrapper& operator=(const RAII_wrapper&) = delete;
	RAII_wrapper& operator=(RAII_wrapper&&) = delete;
};

template <typename F>
auto defer(F f) {
	return RAII_wrapper<F>{std::move(f)};
}

/**
 * @brief Transforms a stateless binary operation into one which takes reversed
 * arguments.
 *
 * For example, kblib::flip<std::minus<>>() returns a function object which
 * subtracts its first argument from its second.
 *
 * @tparam BinaryOperation The operation to reverse.
 */
template <typename BinaryOperation>
KBLIB_NODISCARD constexpr auto flip() -> auto {
	return [](auto&& a, auto&& b) {
		return BinaryOperation{}(static_cast<decltype(b)>(b),
		                         static_cast<decltype(a)>(a));
	};
}

/**
 * @brief Transforms a binary operation into one which takes reversed arguments.
 *
 * For example, kblib::flip(std::minus<>{}) returns a function object which
 * subtracts its first argument from its second.
 *
 * @param op The operation to reverse.
 */
template <typename BinaryOperation>
KBLIB_NODISCARD constexpr auto flip(BinaryOperation op) -> auto {
	return [op = std::move(op)](auto&& a, auto&& b) {
		return op(static_cast<decltype(b)>(b), static_cast<decltype(a)>(a));
	};
}

template <typename T, std::size_t N>
KBLIB_NODISCARD constexpr auto is_consecutive(const T (&array)[N])
    -> enable_if_t<std::is_integral<T>::value, bool> {
	if (N <= 1) {
		return true;
	} else if (N == 2) {
		return (array[1] - array[0]) == 1;
	} else {
		for (std::size_t i = 1; i < N; ++i) {
			if ((array[i] - array[i - 1]) != 1) {
				return false;
			}
		}
		return true;
	}
}

/**
 * @brief Floored integer binary logarithm. Returns floor(lb(val)).
 *
 * Returns the number of significant bits in the given integer.
 */
KBLIB_NODISCARD constexpr auto filg2(
    const std::bitset<std::numeric_limits<std::uintmax_t>::digits> val) noexcept
    -> int {
	for (auto i : range<int>(to_signed(val.size()) - 1, 0, -1)) {
		if (val[to_unsigned(i)])
			return i;
	}
	return 0;
}

template <std::size_t size, typename T, typename... Ts>
struct first_bigger_than
    : std::conditional<sizeof(T) >= size, detail::tag<T>,
                       typename first_bigger_than<size, Ts...>::type> {};

template <std::size_t size, typename T>
struct first_bigger_than<size, T>
    : std::conditional_t<sizeof(T) >= size, detail::tag<T>, void> {};

template <std::uintmax_t I>
using uint_smallest =
    typename first_bigger_than<1 + filg2(I) / CHAR_BIT, unsigned char,
                               unsigned short, unsigned int, unsigned long,
                               unsigned long long, std::uintmax_t>::type;

template <std::uintmax_t I>
using int_smallest =
    typename first_bigger_than<1 + (filg2(I) + 1) / CHAR_BIT, signed char,
                               signed short, signed int, signed long,
                               signed long long, std::uintmax_t>::type;

template <std::uintmax_t I>
using uint_smallest_t = typename uint_smallest<I>::type;

template <std::uintmax_t I>
using int_smallest_t = typename int_smallest<I>::type;

/**
 * @brief The identity function, as a function object.
 */
struct identity {
	template <typename T>
	KBLIB_NODISCARD auto operator()(T&& in) -> T&& {
		return static_cast<T&&>(in);
	}
};

/**
 * @brief Safely propagate an xvalue or lvalue without dangling references
 */
template <typename T>
auto safe_auto(T&& in) -> T {
	return std::forward<T>(in);
}

/**
 * @brief Safely propagate an xvalue or lvalue without dangling references
 */
template <typename T>
auto safe_auto(T& in) -> T& {
	return in;
}

/**
 * @brief Concatenate two dynamic containers together.
 *
 * @param A,B The containers to concatenate together.
 * @return Container A container consisting of the elements of A and B in that
 * order.
 */
template <typename LeftContainer, typename RightContainer>
auto arraycat(LeftContainer A, RightContainer&& B) noexcept(
    noexcept(A.insert(A.end(), B.begin(), B.end()))) -> LeftContainer {
	A.insert(A.end(), B.begin(), B.end());
	return std::move(A);
}

/**
 * @brief Index an array literal without naming its type
 *
 * @attention The return value must not be stored unless the argument is also
 * stored. This is indexable because temporaries live until the end of their
 * full-expression, rather than sub-expression
 *
 * @param a The array literal to index into.
 */
template <typename T>
KBLIB_NODISCARD constexpr auto a(const std::initializer_list<T>& a) -> auto {
	return a.begin();
}
// use like:
// auto v = a({2, 3, 5, 7, 9, 11})[2];

#if KBLIB_USE_CXX20

template <typename T>
concept zero_constructible = requires(T t) {
	{t = 0};
};

struct zero_t {
	consteval zero_t(int i) { i == 0 ? void() : throw 0; }
	template <zero_constructible T>
	constexpr operator T() const noexcept {
		return 0;
	}
	explicit constexpr operator std::nullptr_t() const noexcept {
		return nullptr;
	}
};

#endif

} // namespace kblib

#undef KBLIB_DEBUG_LOG_RANGES

#endif // KBLIB_SIMPLE_H
