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
	if (val == 0) {
		return 1;
	} else if (std::is_floating_point<Number>::value) {
		return std::numeric_limits<Number>::digits10;
	} else {
		return std::ceil(std::log10(val + 1)) + (val < 0);
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
	if (val == 0) {
		return 1;
	} else if (std::is_floating_point<Number>::value) {
		return std::ceil(std::numeric_limits<Number>::digits * std::logb(base));
	} else {
		return std::ceil(std::log(std::abs(val) + 1) / std::log(base)) +
		       (val < 0);
	}
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
