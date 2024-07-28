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
 * @brief Provides numerical and mathematical utilities.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_STATS_H
#define KBLIB_STATS_H

#include <array>
#include <cassert>
#include <cinttypes>
#include <cstdlib>
#include <limits>
#include <numeric>

#include "fakestd.h"
#include "logic.h"
#include "tdecl.h"

namespace KBLIB_NS {

template <typename T, typename U>
KBLIB_NODISCARD constexpr auto div(T num, U den) noexcept
    -> decltype(std::div(num, den)) {
	decltype(std::div(num, den)) ret{};
	ret.quot = num / den;
	ret.rem = num % den;
	return ret;
}

/**
 * @brief std::pair isn't constexpr enough, so I'm stuck with this. All I use it
 * for is removing a temporary variable from calc_fib_size().
 */
template <typename T>
struct trivial_pair {
	T first;
	T second;
};

#if 0 and KBLIB_USE_CXX17

/**
 * @brief std::array isn't constexpr enough in C++14, so this is a separate
 * class in that version. In C++17, std::array has all the functionality needed,
 * so this defines an alias instead.
 *
 */
template <typename T, std::size_t N>
using trivial_array = std::array<T, N>;

#else

/**
 * @brief std::array isn't constexpr enough in C++14, so a dedicated array class
 * is needed for constexpr functions.
 */
template <typename T, std::size_t N>
struct trivial_array {
	T arr[N];
	KBLIB_NODISCARD constexpr auto operator[](std::size_t n) -> T& {
		return arr[n];
	}
	KBLIB_NODISCARD constexpr auto operator[](std::size_t n) const -> const T& {
		return arr[n];
	}
	KBLIB_NODISCARD constexpr auto size() const -> std::size_t { return N; }
	KBLIB_NODISCARD constexpr auto begin() & noexcept -> T* { return arr; }
	KBLIB_NODISCARD constexpr auto begin() const& noexcept -> const T* {
		return arr;
	}
	KBLIB_NODISCARD constexpr auto end() & noexcept -> T* { return arr + N; }
	KBLIB_NODISCARD constexpr auto end() const& noexcept -> const T* {
		return arr + N;
	}

	KBLIB_NODISCARD constexpr friend auto operator==(
	    const trivial_array& a, const trivial_array& b) noexcept -> bool {
		return equal(a.begin(), a.end(), b.begin());
	}
	KBLIB_NODISCARD constexpr friend auto operator!=(
	    const trivial_array& a, const trivial_array& b) noexcept -> bool {
		return not (a == b);
	}
};

#endif

/**
 * @brief Calculate the index of the largest fibonacci number that can be
 * represented by a given unsigned integral type.
 *
 * @remark Here is a table of the results for common bit-widths:
 * @verbatim
 bits  N    fibonacci(N)
 --------------------------------------------------
 8     13   144
 16    24   28657
 32    47   1836311903
 64    93   7540113804746346429
 128   186  205697230343233228174223751303346572685
 @endverbatim
 *
 * @return std::size_t
 */
template <typename U>
KBLIB_NODISCARD constexpr auto calc_fib_size() noexcept -> std::size_t {
	static_assert(std::is_unsigned<U>::value, "U must be unsigned");
	std::size_t n{};
	trivial_pair<U> state{0, 1};
	U& a = state.first;
	U& b = state.second;
	while (b >= a) {
		state = {b, static_cast<U>(a + b)};
		++n;
	}
	return n;
}

/**
 * @brief Generates the first N values of the fibonacci sequence.
 *
 * @pre If N > calc_fib_size<U>(), then U must be an unsigned type, and the
 * resulting sequence is modulo 2^bits_of_U.
 * @pre N >= 2
 *
 * @return trivial_array<U, N> An array containing the first N fibonacci
 * numbers.
 */
template <typename U, std::size_t N = calc_fib_size<U>() + 1>
KBLIB_NODISCARD constexpr auto make_fib_arr() noexcept -> trivial_array<U, N> {
	static_assert(
	    implies<(N > calc_fib_size<U>()), std::is_unsigned<U>::value>::value,
	    "signed U with large N would trigger signed overflow");
	// Initialize the first two elements of the array
	trivial_array<U, N> ret{{0, 1}};
	// A loop initializes the rest
	for (std::size_t i = 2; i < N; ++i) {
		ret[i] = ret[i - 1] + ret[i - 2];
	}
	return ret;
}

/**
 * @brief Compile-time table fibonacci function.
 *
 * @pre n <= calc_fib_size<U>()
 *
 * @return The nth fibonacci number.
 */
template <typename U = std::uintmax_t>
KBLIB_NODISCARD constexpr auto fibonacci(int n) noexcept -> U {
	constexpr auto arr = make_fib_arr<U>();
	assert(n >= 0 and static_cast<std::size_t>(n) < arr.size());
	return arr[to_unsigned(n)];
}

inline namespace nums {
	/**
	 * @brief Shorthand for std::numeric_limits::max().
	 *
	 * Implicitly converts to the maximum representable value of any numeric
	 * type. For unsigned destination types, -1 is shorter, but much less clear.
	 * For signed destination types, there is no concise representation for a
	 * generic maximum.
	 *
	 * Also serves as a max(a, b) function object.
	 */
	constexpr struct max_t {
		template <typename T,
		          std::enable_if_t<std::numeric_limits<T>::is_specialized,
		                           void>* = nullptr>
		constexpr /* implicit*/ operator T() const
		    noexcept(noexcept(std::numeric_limits<T>::max())) {
			return std::numeric_limits<T>::max();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr static auto of() noexcept(
		    noexcept(std::numeric_limits<T>::max())) {
			return std::numeric_limits<T>::max();
		}

		/**
		 @brief Return the larger of two values. Returns lhs if equal.
		*/
		template <typename L, typename R>
		KBLIB_NODISCARD constexpr auto operator()(L&& lhs, R&& rhs) const noexcept
		    -> decltype(auto) {
			return std::less<>{}(lhs, rhs) ? std::forward<R>(rhs)
			                               : std::forward<L>(lhs);
		}

		KBLIB_NODISCARD constexpr inline friend auto operator==(max_t, max_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator!=(max_t, max_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<(max_t, max_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>(max_t, max_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<=(max_t, max_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>=(max_t, max_t)
		    -> std::true_type {
			return {};
		}

		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator==(T t, max_t) -> bool {
			return t == of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator==(max_t, T t) -> bool {
			return t == of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator!=(T t, max_t) -> bool {
			return t != of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator!=(max_t, T t) -> bool {
			return t != of<T>();
		}

		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<(T t, max_t) -> bool {
			return t < of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<(max_t, T t) -> bool {
			return of<T>() < t;
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>(T t, max_t) -> bool {
			return t > of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>(max_t, T t) -> bool {
			return of<T>() > t;
		}

		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<=(T t, max_t) -> bool {
			return t <= of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<=(max_t, T t) -> bool {
			return of<T>() <= t;
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>=(T t, max_t) -> bool {
			return t >= of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>=(max_t, T t) -> bool {
			return of<T>() >= t;
		}

	} max; /**< A shorthand for the maximum value of the destination type. Also
	          provides max(a, b). */

	/**
	 * @brief Shorthand for std::numeric_limits::min()
	 *
	 * Implicitly converts to the minimum representable value of any numeric
	 * type. For unsigned destination types, this is always 0. For signed
	 * destination types, it depends on size.
	 *
	 * Also serves as a min(a, b) function object.
	 */
	constexpr struct min_t {
		template <typename T, enable_if_t<std::numeric_limits<T>::is_specialized,
		                                  void>* = nullptr>
		constexpr /* implicit*/ operator T() const
		    noexcept(noexcept(std::numeric_limits<T>::min())) {
			return std::numeric_limits<T>::min();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr static auto of() noexcept(
		    noexcept(std::numeric_limits<T>::min())) {
			return std::numeric_limits<T>::min();
		}

		/**
		 @brief Returns the smaller of two values. Returns rhs if equal.
		*/
		template <typename L, typename R>
		KBLIB_NODISCARD constexpr auto operator()(L&& lhs, R&& rhs) const noexcept
		    -> decltype(auto) {
			return std::less<>{}(lhs, rhs) ? std::forward<R>(lhs)
			                               : std::forward<L>(rhs);
		}

		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator==(T t, min_t) -> bool {
			return t == of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator==(min_t, T t) -> bool {
			return t == of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator!=(T t, min_t) -> bool {
			return t != of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator!=(min_t, T t) -> bool {
			return t != of<T>();
		}

		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<(T t, min_t) -> bool {
			return t < of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<(min_t, T t) -> bool {
			return of<T>() < t;
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>(T t, min_t) -> bool {
			return t > of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>(min_t, T t) -> bool {
			return of<T>() > t;
		}

		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<=(T t, min_t) -> bool {
			return t <= of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator<=(min_t, T t) -> bool {
			return of<T>() <= t;
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>=(T t, min_t) -> bool {
			return t >= of<T>();
		}
		template <typename T>
		KBLIB_NODISCARD constexpr friend auto operator>=(min_t, T t) -> bool {
			return of<T>() >= t;
		}

		KBLIB_NODISCARD constexpr inline friend auto operator==(min_t, min_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator!=(min_t, min_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<(min_t, min_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>(min_t, min_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<=(min_t, min_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>=(min_t, min_t)
		    -> std::true_type {
			return {};
		}

		KBLIB_NODISCARD constexpr inline friend auto operator==(max_t, min_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator==(min_t, max_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator!=(max_t, min_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator!=(min_t, max_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<(max_t, min_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<(min_t, max_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>(max_t, min_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>(min_t, max_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<=(max_t, min_t)
		    -> std::false_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator<=(min_t, max_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>=(max_t, min_t)
		    -> std::true_type {
			return {};
		}
		KBLIB_NODISCARD constexpr inline friend auto operator>=(min_t, max_t)
		    -> std::false_type {
			return {};
		}

	} min; /**< A shorthand for the minimum value of the destination type. Also
	          provides min(a, b). */

} // namespace nums

template <typename T = double>
KBLIB_NODISCARD constexpr auto pi() -> T {
	return 3.1415926535897932384626433832795028841971693993751l;
}
template <typename T = double>
KBLIB_NODISCARD constexpr auto tau() -> T {
	return 2 * pi<T>;
}
template <typename T = double>
KBLIB_NODISCARD constexpr auto e() -> T {
	return 2.7182818284590452353602874713526624977572470937000l;
}
template <typename T = double>
KBLIB_NODISCARD constexpr auto root_2() -> T {
	return 1.4142135623730950488016887242096980785696718753769l;
}
template <typename T = double>
KBLIB_NODISCARD constexpr auto phi() -> T {
	return 1.6180339887498948482045868343656381177203091798058l;
}

// saturating to_unsigned
template <typename A, typename F>
KBLIB_NODISCARD constexpr auto saturating_cast(F x) noexcept
    -> enable_if_t<std::is_integral<A>::value and std::is_integral<F>::value
                       and std::is_unsigned<A>::value,
                   A> {
	if (x < 0) {
		return 0;
		// TODO: verify
	} else if (to_unsigned(x) > A(max)) {
		return max;
	} else {
		return static_cast<A>(x);
	}
}

// saturating to_signed(signed)
template <typename A, typename F>
KBLIB_NODISCARD constexpr auto saturating_cast(F x) noexcept
    -> enable_if_t<std::is_integral<A>::value and std::is_integral<F>::value
                       and std::is_signed<A>::value
                       and std::is_signed<F>::value,
                   A> {
	if (x < A(min)) {
		return min;
	} else if (x > A(max)) {
		return max;
	} else {
		return x;
	}
}
// saturating to_signed(unsigned)
template <typename A, typename F>
KBLIB_NODISCARD constexpr auto saturating_cast(F x) noexcept
    -> enable_if_t<std::is_integral<A>::value and std::is_integral<F>::value
                       and std::is_signed<A>::value
                       and std::is_unsigned<F>::value,
                   A> {
	if (x > to_unsigned(A(max))) {
		return max;
	} else {
		return x;
	}
}

/// TODO(killerbee13): write tests and fix style issues for quantization
/// functions

/**
 * @brief Quantize a real-valued value into a discrete integer.
 *
 * @tparam T An unsigned integral type.
 * @param min The real value corresponding to 0 in the output.
 * @param delta The difference between quantization steps.
 * @param val The input value.
 * @return The quantized value of the input.
 */
template <typename T, typename F>
KBLIB_NODISCARD constexpr auto quantize_step(F low, F delta, F val) noexcept
    -> T {
	static_assert(std::is_unsigned<T>::value, "Destination must be unsigned.");
	return static_cast<T>((val - low) * static_cast<T>(max) * delta);
}

/**
 * @brief Quantize a real-valued value into a discrete integer.
 *
 * @tparam T An unsigned integral type.
 * @param min The real value corresponding to min in the output.
 * @param max The real value corresponding to max in the output.
 * @param val The input value.
 * @return The quantized value of the input.
 */
template <typename T, typename F>
KBLIB_NODISCARD constexpr auto quantize_range(F low, F high, F val) noexcept
    -> T {
	static_assert(std::is_unsigned<T>::value, "Destination must be unsigned.");
	auto delta = (high - low) / static_cast<T>(max);
	return static_cast<T>((val - low) * static_cast<T>(max) * delta);
}

} // namespace KBLIB_NS

#endif // KBLIB_STATS_H
