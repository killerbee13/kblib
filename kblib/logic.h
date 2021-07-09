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
 * Provides basic compile-time logic operations.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_LOGIC_H
#define KBLIB_LOGIC_H

#include "tdecl.h"

#include <type_traits>

namespace kblib {

/**
 * @brief A metafunction for logical implication. That is, if A, then B. If not
 * A, B is unimportant.
 */
template <bool A, bool B>
struct implies : std::integral_constant<bool, true> {};

/**
 * @brief Logical implication is only not satisfied by true -> false.
 */
template <>
struct implies<true, false> : std::integral_constant<bool, false> {};

#if KBLIB_USE_CXX17
/**
 * Equivalent to implies<A, B>::value.
 */
template <bool A, bool B>
constexpr inline bool implies_v = implies<A, B>::value;

template <bool... args>
constexpr bool conjunction_v = (args and ...);
#endif

} // namespace kblib

#endif // KBLIB_LOGIC_H
