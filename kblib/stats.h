#ifndef KBLIB_STATS_H
#define KBLIB_STATS_H

#include <array>
#include <cassert>
#include <limits>
#include <numeric>
#include <random>

#include "logic.h"
#include "tdecl.h"

namespace kblib {

/**
 * @brief Given a categorical distribution cats, selects one category
 *
 * @deprecated std::discrete_distribution provides the same functionality, with
 * a worse name. Because it exists, there is no reason to use this function.
 *
 * @param cats A sequence of category weights
 * @param r A <random>-compatible RandomGenerator
 * @todo Refactor to remove the ugly unreachable stuff.
 */
template <typename Array, typename RandomGenerator, typename freqtype = double>
[[deprecated("Use std::discrete_distribution instead")]] auto
chooseCategorical(Array&& cats, RandomGenerator& r) {
	std::uniform_real_distribution<freqtype> uniform(
	    0.0, std::accumulate(cats.begin(), cats.end(), 0.0));
	freqtype choose = uniform(r);
	for (decltype(cats.size()) stop = 0; stop != cats.size(); ++stop) {
		choose -= cats[stop];
		if (choose <= 0) {
			return stop;
		}
	}
#if __has_builtin(__builtin_unreachable)
	__builtin_unreachable();
#else
	return cats.size() - 1;
#endif
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

#if KBLIB_USE_CXX17

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
	constexpr T& operator[](std::size_t n) { return arr[n]; }
	constexpr const T& operator[](std::size_t n) const { return arr[n]; }
	constexpr std::size_t size() const { return N; }
	constexpr T* begin() & noexcept { return arr; }
	constexpr const T* begin() const& noexcept { return arr; }
	constexpr T* end() & noexcept { return arr + N; }
	constexpr const T* end() const& noexcept { return arr + N; }

	constexpr friend bool operator==(const trivial_array& a,
	                                 const trivial_array& b) {
		for (std::size_t idx = 0; idx != N; ++idx) {
			if (a[idx] != b[idx]) {
				return false;
			}
		}
		return true;
	}
	constexpr friend bool operator!=(const trivial_array& a,
	                                 const trivial_array& b) {
		return !(a == b);
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
constexpr std::size_t calc_fib_size() {
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
constexpr trivial_array<U, N> make_fib_arr() {
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
template <typename U = unsigned long long>
constexpr U fibonacci(int n) {
	constexpr auto arr = make_fib_arr<U>();
	assert(n >= 0 && static_cast<std::size_t>(n) < arr.size());
	return arr[n];
}

/**
 * @brief A constexpr version of std::accumulate
 */
template <typename InputIt, typename T>
constexpr auto accumulate(InputIt first, InputIt last, T init)
    -> std::decay_t<decltype(*first)> {
	for (; first != last; ++first) {
		init = std::move(init) + *first;
	}
	return init;
}

/**
 * @brief A constexpr version of std::accumulate
 */
template <class InputIt, class T, class BinaryOperation>
constexpr T accumulate(InputIt first, InputIt last, T init,
                       BinaryOperation op) {
	for (; first != last; ++first) {
		init = op(std::move(init), *first);
	}
	return init;
}

/**
 * @brief Sum a range
 *
 * Convenience wrapper for std::accumulate. For an empty range, returns a
 * value-initialized temporary (usually 0). Deduces the correct type for the
 * initializer, which reduces risk of truncation and incorrect results.
 *
 * @param[in] first Beginning of range
 * @param[in] last End of range
 * @return The sum of the input range.
 */
template <typename InputIt>
constexpr auto sum(InputIt first, InputIt last)
    -> std::decay_t<decltype(*first)> {
	if (first == last) {
		return {};
	}
	auto init = *first++;
	return kblib::accumulate(first, last, std::move(init));
}

/**
 * @brief Fold a range over an operation.
 *
 * Convenience wrapper for std::accumulate. For an empty range, returns a
 * value-initialized temporary (usually 0). Deduces the correct type for the
 * initializer, which reduces risk of truncation and incorrect results.
 *
 * @param[in] first Beginning of range
 * @param[in] last End of range
 * @param[in] op The fold operation
 * @return The sum of the input range.
 */
template <typename InputIt, typename BinaryOperation>
constexpr auto sum(InputIt first, InputIt last, BinaryOperation op)
    -> std::decay_t<decltype(*first)> {
	if (first == last) {
		return {};
	}
	auto init = *first++;
	return kblib::accumulate(first, last, std::move(init), op);
}

/**
 * @brief Sum a range
 *
 * Convenience wrapper for std::accumulate. For an empty range, returns a
 * value-initialized temporary (usually 0). Deduces the correct type for the
 * initializer, which reduces risk of truncation and incorrect results.
 *
 * @param[in] r The range to sum
 * @return The sum of the input range.
 */
template <typename Range>
constexpr auto sum(Range&& r) {
	using std::begin;
	auto first = begin(r);
	auto last = end(r);
	if (first == last) {
		return std::decay_t<decltype(*first)>{};
	}
	auto init = *first++;
	return kblib::accumulate(first, last, std::move(init));
}

inline namespace nums {
	/**
	 * @brief Shorthand for std::numeric_limits::max()
	 *
	 * Implicitly converts to the maximum representable value of any numeric
	 * type. For unsigned destination types, -1 is shorter, but much less clear.
	 * For signed destination types, there is no concise representation for a
	 * generic maximum.
	 */
	KBLIB_UNUSED constexpr struct max_t {
		template <typename T>
		constexpr /* implicit*/ operator T() const
		    noexcept(noexcept(std::numeric_limits<T>::max())) {
			return std::numeric_limits<T>::max();
		}
	} max; /**< A shorthand for the maximum value of the destination type. */

	template <typename T>
	bool operator==(T t, max_t) {
		return t == T(max);
	}
	template <typename T>
	bool operator==(max_t, T t) {
		return t == T(max);
	}
	template <typename T>
	bool operator!=(T t, max_t) {
		return t != T(max);
	}
	template <typename T>
	bool operator!=(max_t, T t) {
		return t != T(max);
	}

	/**
	 * @brief Shorthand for std::numeric_limits::min()
	 *
	 * Implicitly converts to the minimum representable value of any numeric
	 * type. For unsigned destination types, this is always 0. For signed
	 * destination types, it depends on size.
	 */
	KBLIB_UNUSED constexpr struct min_t {
		template <typename T>
		constexpr /* implicit*/ operator T() const
		    noexcept(noexcept(std::numeric_limits<T>::min())) {
			return std::numeric_limits<T>::min();
		}
	} min; /**< A shorthand for the minimum value of the destination type. */

	template <typename T>
	bool operator==(T t, min_t) {
		return t == T(min);
	}
	template <typename T>
	bool operator==(min_t, T t) {
		return t == T(min);
	}
	template <typename T>
	bool operator!=(T t, min_t) {
		return t != T(min);
	}
	template <typename T>
	bool operator!=(min_t, T t) {
		return t != T(min);
	}

} // namespace nums

template <typename T>
constexpr T pi() {
	return 3.1415926535897932384626433832795028841971693993751;
}
template <typename T>
constexpr T tau() {
	return 2 * pi<T>;
}
template <typename T>
constexpr T e() {
	return 2.7182818284590452353602874713526624977572470937000;
}
template <typename T>
constexpr T root_2() {
	return 1.4142135623730950488016887242096980785696718753769;
}
template <typename T>
constexpr T phi() {
	return 1.6180339887498948482045868343656381177203091798058;
}

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
constexpr T quantizeStep(F min, F delta, F val) {
	static_assert(std::is_unsigned<T>::value, "Destination must be unsigned.");
	return static_cast<T>((val - min) * static_cast<T>(nums::max) * delta);
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
constexpr T quantizeRange(F min, F max, F val) {
	static_assert(std::is_unsigned<T>::value, "Destination must be unsigned.");
	auto delta = (max - min) / static_cast<T>(nums::max);
	return static_cast<T>((val - min) * static_cast<T>(nums::max) * delta);
}

} // namespace kblib

#endif // KBLIB_STATS_H
