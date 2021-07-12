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
 * @brief Contains some utilities for manipulating and querying string
 * representations.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_FORMAT_H
#define KBLIB_FORMAT_H

#include "fakestd.h"

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
constexpr auto count_digits(Number val)
    -> enable_if_t<std::is_floating_point<Number>::value, int> {
	if (val == 0) {
		return 1;
	} else {
		return std::numeric_limits<Number>::digits10;
	}
}

/**
 * @brief Calculates the number of decimal digits needed to represent a number,
 * plus one for negative numbers.
 *
 * @param val The number to be checked.
 * @return int The number of digits needed to represent a number.
 */
template <typename Number>
constexpr auto count_digits(Number val)
    -> enable_if_t<not std::is_floating_point<Number>::value and
                       std::is_signed<Number>::value,
                   int> {
	if (val == 0 or val == 1) {
		return 1;
	} else {
		return std::ceil(std::nextafter(std::log10(std::fabs(val)), INFINITY)) +
		       (val < 0);
	}
}

/**
 * @brief Calculates the number of decimal digits needed to represent a number.
 *
 * @param val The number to be checked.
 * @return int The number of digits needed to represent a number.
 */
template <typename Number>
constexpr auto count_digits(Number val)
    -> enable_if_t<std::is_unsigned<Number>::value, int> {
	if (val == 0) {
		return 1;
	} else if (val == static_cast<Number>(-1)) {
		return std::ceil(std::log10(val));
	} else {
		return std::ceil(std::log10(val + 1));
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
