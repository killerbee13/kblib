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
 * @brief Provides macros and basic templates used by the rest of kblib.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_TDECL_H
#define KBLIB_TDECL_H

#include <cstddef>

#if __cplusplus < 201402L
#	error kblib requires C++14 or higher
#endif

// 1MMmmrr
#define KBLIB_VERS 1000203

#define KBLIB_VERS_MAJ 0
#define KBLIB_VERS_MIN 2
#define KBLIB_VERS_REV 3
// MM_mm_rr
#define KBLIB_VERS_S KBLIB_VERS_MAJ##_##KBLIB_VERS_MIN##_##KBLIB_VERS_REV
// api_vMM_mm_rr
#define KBLIB_VERS_NS api_v##KBLIB_VERS_S

/**
 * @def KBLIB_USE_CXX17
 * @brief This internal macro is used to determine if kblib can use C++17
 * features.
 */
#define KBLIB_USE_CXX17 __cplusplus >= 201703L

/**
 * @def KBLIB_USE_CXX20
 * @brief This internal macro is used to determine if kblib can use C++20
 * features.
 */
#define KBLIB_USE_CXX20 __cplusplus >= 202002L
/**
 * @def KBLIB_USE_STRING_VIEW
 * @brief This internal macro is used to determine if kblib can use C++17's
 * std::string_view.
 */
#ifndef KBLIB_USE_STRING_VIEW
#	define KBLIB_USE_STRING_VIEW __cpp_lib_string_view
#endif

// Note that __has_cpp_attribute(nodiscard) does not work with at least certain
// versions of Clang
/**
 * @def KBLIB_NODISCARD
 * @brief This internal macro is used to provide a fallback for [[nodiscard]]
 * in C++14.
 */
#if KBLIB_USE_CXX17
#	define KBLIB_NODISCARD [[nodiscard]]
#else
#	define KBLIB_NODISCARD [[gnu::warn_unused_result]]
#endif

/**
 * @def KBLIB_UNUSED
 * @brief This internal macro is used to provide a fallback for [[maybe_unused]]
 * in C++14.
 */
#if KBLIB_USE_CXX17
#	define KBLIB_UNUSED [[maybe_unused]]
#else
#	define KBLIB_UNUSED [[gnu::unused]]
#endif

#if KBLIB_USE_CXX17
#	define KBLIB_CONSTANT constexpr
#	define KBLIB_CONSTANT_V constexpr bool
#	define KBLIB_CONSTANT_M constexpr inline static
#	define KBLIB_CONSTANT_MV constexpr inline static bool
#else
#	define KBLIB_CONSTANT constexpr static
#	define KBLIB_CONSTANT_V constexpr static bool
#	define KBLIB_CONSTANT_M constexpr static
#	define KBLIB_CONSTANT_MV constexpr static bool
#endif

/**
 * @def KBLIB_CXX20
 * @brief This internal macro is used to selectively use C++20 features.
 */
#if KBLIB_USE_CXX20
#	define KBLIB_CXX20(args) args
#else
#	define KBLIB_CXX20(args)
#endif

#if defined(_DOXYGEN_) and not defined(KBLIB_DEF_MACROS)
/**
 * @def KBLIB_DEF_MACROS
 * @brief If this macro is defined, kblib will define certain macros without the
 * KBLIB_ prefix.
 */
#	define KBLIB_DEF_MACROS
#endif

/**
 * @namespace kblib
 * @brief The main namespace in which all entities from kblib are defined.
 */
namespace kblib {

/**
 * @namespace kblib::detail
 * @brief The namespace used for implementation details within kblib.
 *
 * @internal
 */
namespace detail {

	template <typename Container, bool, typename...>
	struct buildiota_impl;

	template <typename T>
	struct tag {
		using type = T;
	};

	template <typename T>
	struct no_dangle {
		using type = T&;
	};

	template <typename T>
	struct no_dangle<T&&> {
		using type = T;
	};

	template <typename T>
	using no_dangle_t = typename no_dangle<T>::type;

} // namespace detail
enum class endian { unknown, little, big, weird };

#ifdef __BYTE_ORDER__
namespace detail {
	KBLIB_NODISCARD constexpr auto get_system_endian() -> endian {
		if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__) {
			return endian::big;
		} else if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) {
			return endian::little;
		} else {
			return endian::weird;
		}
	}
} // namespace detail

constexpr endian system_endian = detail::get_system_endian();
#else
constexpr endian system_endian = endian::unknown;
#endif

namespace detail {
	KBLIB_NODISCARD constexpr auto get_hash_order() -> endian {
		if (system_endian == endian::little or system_endian == endian::big) {
			return system_endian;
		} else {
			return endian::little;
		}
	}
} // namespace detail

#ifdef KBLIB_CONSISTENT_HASHES
constexpr endian hash_order = little;
#else
constexpr endian hash_order = detail::get_hash_order();
#endif

#if KBLIB_USE_CXX17
using std::byte;
#else
using byte = unsigned char;
#endif

} // namespace kblib

#endif // KBLIB_TDECL_H
