#ifndef KBLIB_STATS_H
#define KBLIB_STATS_H

#include <array>
#include <limits>
#include <numeric>
#include <random>

#include "tdecl.h"

#if KBLIB_USE_CXX17
#define KBLIB_UNUSED [[maybe_unused]]
#else
#define KBLIB_UNUSED
#endif

namespace kblib {

/**
 * @brief Given a categorical distribution cats, selects one category
 *
 * @param cats A sequence of category weights
 * @param r A <random>-compatible RandomGenerator
 * @todo Refactor to remove the ugly unreachable stuff.
 */
template <typename Array, typename RandomGenerator, typename freqtype = double>
auto chooseCategorical(Array&& cats, RandomGenerator& r) {
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
 * @brief Semi-efficient fibonacci implementation (constant for small n)
 *
 * @param n
 * @return Returns the nth fibonacci number.
 */
inline unsigned fibonacci(unsigned n) {
  constexpr std::array<unsigned, 39> fibs{
      {0,       1,       1,       2,       3,        5,        8,       13,
       21,      34,      55,      89,      144,      233,      377,     610,
       987,     1597,    2584,    4181,    6765,     10946,    17711,   28657,
       46368,   75025,   121393,  196418,  317811,   514229,   832040,  1346269,
       2178309, 3524578, 5702887, 9227465, 14930352, 24157817, 39088169}};
  if (n < fibs.size()) {
    return fibs[n];
    // Iterative fallback
  } else {
    unsigned b1 = fibs[fibs.size() - 2], b2 = fibs[fibs.size() - 1],
             t = b1 + b2;
    for (unsigned i = fibs.size(); i != n; ++i) {
      t = b2 + b1;
      b2 = b1;
      b1 = t;
    }
    return t;
  }
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
 * @brief Sum a numeric range
 *
 * Convenience wrapper for std::accumulate for numeric ranges. For an empty
 * range, returns a value-initialized temporary (usually 0). Deduces the correct
 * type for the initializer, which reduces risk of truncation and incorrect
 * results.
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
  return accumulate(first, last, init);
}

inline namespace nums {
/**
 * @brief Shorthand for std::numeric_limits::max()
 *
 * Implicitly converts to the maximum representable value of any numeric type.
 * For unsigned destination types, -1 is shorter, but much less clear. For
 * signed destination types, there is no concise representation for a generic
 * maximum.
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
 * Implicitly converts to the minimum representable value of any numeric type.
 * For unsigned destination types, this is always 0. For signed destination
 * types, it depends on size.
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

}  // namespace nums

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

}  // namespace kblib

#endif  // KBLIB_STATS_H
