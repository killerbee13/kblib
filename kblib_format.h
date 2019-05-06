#ifndef KBLIB_FORMAT_H
#define KBLIB_FORMAT_H

#include <algorithm>
#include <cmath>

namespace kblib {

template <typename Number>
constexpr int digitsOf(Number val) {
  if (val > 0) {
    return std::ceil(std::log10(val+1));
  } else if (val < 0) {
    return 1 + std::ceil(std::log10(-val+1));
  } else {
    return 1;
  }
}

template <typename Number>
constexpr int digitsOf(Number val, int base) {
  return std::ceil(std::log(std::abs(val) + 1) / std::log(base)) + std::signbit(val);
}

template <typename ForwardIt>
int digitsList(ForwardIt first, ForwardIt last) {
  return digitsOf(*std::max_element(first, last));
}

template <typename ForwardIt>
int digitsList(ForwardIt first, ForwardIt last, int base) {
  return digitsOf(*std::max_element(first, last), base);
}

}  // namespace kblib

#endif  // KBLIB_FORMAT_H
