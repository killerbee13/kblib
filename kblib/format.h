#ifndef KBLIB_FORMAT_H
#define KBLIB_FORMAT_H

#include "tdecl.h"

#include <algorithm>
#include <cmath>

namespace kblib {

/**
 * @brief Calculates the number of decimal digits needed to represent a number,
 * plus one for negative numbers.
 *
 * @param val The number to be checked.
 * @return int The number of digits needed to represent a number.
 */
template <typename Number>
constexpr auto count_digits(Number val) -> int {
	if (val > 0) {
		return std::ceil(std::log10(val + 1));
	} else if (val < 0) {
		return 1 + std::ceil(std::log10(-val + 1));
	} else {
		return 1;
	}
}

/**
 * @brief Calculates the number of digits needed to represent a number in a
 * given base, plus one for negative numbers.
 *
 * @param val The number to be checked.
 * @param base The base to be used for calculation.
 * @return int The number of digits needed to represent a number.
 */
template <typename Number>
constexpr auto count_digits(Number val, int base) -> int {
	return std::ceil(std::log(std::abs(val) + 1) / std::log(base)) +
	       std::signbit(val);
}

/**
 * @brief Returns the necessary number of digits to represent the largest value
 * in an input range.
 *
 * @param first The beginning of the input range.
 * @param last The end of the input range.
 * @return int The necessary number of digits to represent any value in the
 * input.
 */
template <typename ForwardIt>
auto max_count_digits(ForwardIt first, ForwardIt last) -> int {
	if (first == last) {
		return 0;
	}
	return count_digits(*std::max_element(first, last));
}

/**
 * @brief Returns the necessary number of digits to represent the largest value
 * in an input range.
 *
 * @param first The beginning of the input range.
 * @param last The end of the input range.
 * @param base The base to be used for calculation.
 * @return int The necessary number of digits to represent any value in the
 * input.
 */
template <typename ForwardIt>
auto max_count_digits(ForwardIt first, ForwardIt last, int base) -> int {
	if (first == last) {
		return 0;
	}
	return count_digits(*std::max_element(first, last), base);
}

} // namespace kblib

#endif // KBLIB_FORMAT_H
