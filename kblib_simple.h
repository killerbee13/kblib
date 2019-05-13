#ifndef KBLIB_SIMPLE_H
#define KBLIB_SIMPLE_H

#include <cstdint>
#include <initializer_list>
#include <string_view>
#include <limits>

#include "kblib_tdecl.h"

namespace kblib {

namespace fnv {

/**
 * @brief The prime to use for the FNVa hash algorithm, as a type trait.
 */
template <typename UInt>
struct fnv_prime {};

template <>
/**
 * @brief
 *
 */
struct fnv_prime<std::uint32_t>
    : std::integral_constant<std::uint32_t, 16777619ul> {};
template <>
/**
 * @brief
 *
 */
struct fnv_prime<std::uint64_t>
    : std::integral_constant<std::uint64_t, 1099511628211ull> {};

/**
 * @brief The starting value for the FNVa hash algorithm, as a type trait.
 */
template <typename UInt>
struct fnv_offset {};

template <>
/**
 * @brief
 *
 */
struct fnv_offset<std::uint32_t>
    : std::integral_constant<std::uint32_t, 2166136261ul> {};
template <>
/**
 * @brief
 *
 */
struct fnv_offset<std::uint64_t>
    : std::integral_constant<std::uint64_t, 14695981039346656037ull> {};

}  // namespace fnv

/**
 * @brief A templatized generic FNVa hash function.
 *
 * @tparam HashInt The unsigned integer type to use as the hash result. Must be either std::uint32_t or std::uint64_t.
 * @param s The data to hash. Any range-for-iterable span of char-like objects.
 * @param hval The initial value for the hash accumulator. Pass in another hash value to create a hash of the concatenation of the two ranges.
 * @return HashInt The FNVa hash of the input range.
 */
template <typename HashInt, typename Span>
constexpr HashInt FNVa(Span&& s,
                       HashInt hval = fnv::fnv_offset<HashInt>::value) {
  static_assert(sizeof(*std::begin(s)) == 1,
                "Can only hash char-like objects.");
  const HashInt prime = fnv::fnv_prime<HashInt>::value;
  for (auto&& c : s) {
    hval ^= static_cast<HashInt>(static_cast<unsigned char>(c));
    hval *= prime;
  }
  return hval;
}

/**
 * @brief A templatized FNVa hash function, for raw character arrays, such as string literals.
 *
 * @tparam HashInt The unsigned integer type to use as the hash result. Must be either std::uint32_t or std::uint64_t.
 * @param s The data to hash. A raw array of char-like objects.
 * @param hval The initial value for the hash accumulator. Pass in another hash value to create a hash of the concatenation of the two ranges.
 * @return HashInt The FNVa hash of the input range.
 */
template <typename HashInt, typename CharT, std::size_t N>
constexpr HashInt FNVa_a(const CharT (&s)[N],
                         HashInt hval = fnv::fnv_offset<HashInt>::value) {
  static_assert(sizeof(s[0]) == 1, "Can only hash char-like objects.");
  const HashInt prime = fnv::fnv_prime<HashInt>::value;
  for (auto&& c : s) {
    hval ^= static_cast<HashInt>(static_cast<unsigned char>(c));
    hval *= prime;
  }
  return hval;
}

/**
 * @brief A standard FNV32a hash function, for string_views.
 *
 * @param s The data to hash.
 * @param hval The initial value for the hash accumulator. Pass in another hash value to create a hash of the concatenation of the two ranges.
 * @return std::uint32_t The FNV32a hash of the input range.
 */
constexpr std::uint32_t FNV32a(std::string_view s, uint32_t hval = 2166136261) {
  const std::uint32_t FNV_32_PRIME = 16777619;
  for (auto&& c : s) {
    hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
    hval *= FNV_32_PRIME;
  }
  return hval;
}

/**
 * @brief A standard FNV32a hash function, for raw character arrays, such as string literals.
 *
 * @param s The data to hash.
 * @param hval The initial value for the hash accumulator. Pass in another hash value to create a hash of the concatenation of the two ranges.
 * @return HashInt The FNV32a hash of the input range.
 */
template <std::size_t N>
constexpr std::uint32_t FNV32a_a(const char (&s)[N],
                                 uint32_t hval = 2166136261) {
  const std::uint32_t FNV_32_PRIME = 16777619;
  for (auto&& c : s) {
    hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
    hval *= FNV_32_PRIME;
  }
  return hval;
}

inline namespace literals {

/**
 * @brief A literal suffix that produces the FNV32a hash of a string literal.
 */
constexpr std::uint32_t operator""_fnv32(const char* str, std::size_t length) {
  return FNV32a({str, length});
}

/**
 * @brief A literal suffix that produces the FNV64a hash of a string literal.
 */
constexpr std::uint64_t operator""_fnv64(const char* str, std::size_t length) {
  return FNVa<std::uint64_t>(std::string_view{str, length});
}

}  // namespace literals

/**
 * @brief A range generator, similar to Python 3's range().
 *
 * Generates a half-open range, [min, max).
 */
template <typename Int>
class range_t {
 public:
  /**
   * @brief 2- and 3-argument constructor. Explicitly specify start, end, and optionally the step amount.
   *
   * @param min_ The first value in the range.
   * @param max_ The end of the range.
   * @param step_ The difference between values in the range.
   */
  range_t(Int min_, Int max_, Int step_ = 1)
      : min(min_), max(max_), step(step_) {normalize();}
  /**
   * @brief 1-argument constructor. Start is implicitly zero and step is implicitly 1.
   *
   * @param max The end of the range.
   */
  range_t(Int max) : range_t(0, max, 0) {}

  /**
   * @brief A helper struct which acts as an iterator for the range elements, as they are generated on the fly.
   *
   */
  struct dummyptr {
    Int val, step{1}; /**< TODO: describe */
    /**
     * @brief Return the "pointed-to" value.
     *
     * @return Int operator
     */
    Int operator*() { return val; }
    /**
     * @brief Prefix increment. Advance to the next value in the range.
     *
     * @return dummyptr& *this.
     */
    dummyptr& operator++() {
      val += step;
      return *this;
    }
    /**
     * @brief Postfix increment. Advance to the next value in the range, but return the current value.
     *
     * @return dummyptr A copy of the pre-incrementing value of *this.
     */
    dummyptr operator++(int) {
      auto ret = *this;
      val += step;
      return ret;
    }
    /**
     * @brief Compare two range iterators for equality.
     *
     * Range iterators compare equal if they point to the same value and have the same step.
     */
    friend bool operator==(dummyptr l, dummyptr r) {
      return l.val == r.val && l.step == r.step;
    }
    /**
     * @brief Compare two range iterators for inequality.
     *
     * Range iterators compare equal if they point to the same value and have the same step.
     */
    friend bool operator!=(dummyptr l, dummyptr r) {
      return l.val != r.val || l.step != r.step;
    }
    /**
     * @brief Compare two range iterators.
     *
     * For range iterators, (A < B) is true when A can be advanced until (*A - *B) changes sign.
     */
    friend bool operator<(dummyptr l, dummyptr r) {
      if (l.step > 0)
        return l.val < r.val;
      else
        return l.val > r.val;
    }
    /**
     * @brief Compare two range iterators.
     *
     * For range iterators, (A < B) is true when A can be advanced until (*A - *B) changes sign.
     */
    friend bool operator<=(dummyptr l, dummyptr r) {return !(r < l);}
    /**
     * @brief Compare two range iterators.
     *
     * For range iterators, (A < B) is true when A can be advanced until (*A - *B) changes sign.
     */
    friend bool operator>(dummyptr l, dummyptr r) {return r < l;}
    /**
     * @brief Compare two range iterators.
     *
     * For range iterators, (A < B) is true when A can be advanced until (*A - *B) changes sign.
     */
    friend bool operator>=(dummyptr l, dummyptr r) {return !(l < r);}
  };

  /**
   * @brief Returns an iterator to the beginning of the range.
   */
  dummyptr begin() const { return {min, step}; }
  /**
   * @brief Return an iterator to the end of the range.
   */
  dummyptr end() const { return {max, step}; }

  /**
   * @brief Returns the distance between start() and stop().
   */
  std::size_t size() const { return (max - min) / step; }

  /**
   * @brief Compare l and r for equality.
   *
   * Ranges are equal when they generate identical ranges.
   */
  friend bool operator==(range_t l, range_t r) {
    return (l.begin() == r.begin()) && (l.end() == r.end());
  }
  /**
   * @brief Compare l and r for inequality.
   *
   * Ranges are equal when they generate identical ranges.
   */
  friend bool operator!=(range_t l, range_t r) {
    return (l.begin() != r.begin()) || (l.end() != r.end());
  }

 private:
  Int min, max, step;

  void normalize() {
    auto difference = max - min;
    auto remainder = difference % step;
    if (remainder != 0) {
      max -= remainder;
    }
    if (min == max) {
      min = 0;
      max = 0;
      step = 1;
    }
    if (step == 0) {
      if (min != std::numeric_limits<Int>::max()) {
        max = min + 1;
      } else {
        max = min - 1;
      }
    }
  }
};

template <typename Int>
/**
 * @brief
 *
 * @param min
 * @param max
 * @param step
 * @return range_t<Int>
 */
range_t<Int> range(Int min, Int max, Int step = 0) {
  if (step == 0) {
    if (min <= max) {
      return {min, max, 1};
    } else {
      return {min, max, -1};
    }
  } else {
    return {min, max, step};
  }
}

template <typename Int>
/**
 * @brief
 *
 * @param max
 * @return range_t<Int>
 */
range_t<Int> range(Int max) {
  return {max};
}

template <typename Container>
/**
 * @brief
 *
 * @param A
 * @param B
 * @return Container
 */
Container arraycat(Container A, Container&& B) {
  A.insert(A.end(), B.begin(), B.end());
  return A;
}

// Index an array literal without naming its type
// Caveat: the return value must not be stored unless the argument is also
// stored. This is indexable because temporaries live until the end of their
// full-expression, rather than sub-expression
template <typename T>
/**
 * @brief
 *
 * @param a
 */
constexpr auto a(const std::initializer_list<T>& a) {
  return a.begin();
}
// use like:
// auto v = a({2, 3, 5, 7, 9, 11})[2];

template <typename T>
/**
 * @brief
 *
 */
using alias = T;

template <typename, typename T>
/**
 * @brief
 *
 */
struct ignore {
  /**
   * @brief
   *
   */
  using type = T;
};
template <typename U, typename T>
/**
 * @brief
 *
 */
using ignore_t = typename ignore<U, T>::type;

#if KBLIB_USE_CXX17
template <bool... args>
constexpr bool conjunction = (args && ...);
#endif

}  // namespace kblib

#endif  // KBLIB_SIMPLE_H
