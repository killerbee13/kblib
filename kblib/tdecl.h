#ifndef KBLIB_TDECL_H
#define KBLIB_TDECL_H

#include <iso646.h>

/**
 * @file tdecl.h
 * @brief Contains basic declarations needed by other files.
 */

/**
 * @def KBLIB_USE_CXX17
 * @brief This internal macro is used to determine if kblib can use C++17
 * features.
 */
#define KBLIB_USE_CXX17 __cplusplus >= 201703L
/**
 * @def KBLIB_USE_STRING_VIEW
 * @brief This internal macro is used to determine if kblib can use C++17's
 * std::string_view.
 */
#define KBLIB_USE_STRING_VIEW KBLIB_USE_CXX17

// Note that has_cpp_attribute(nodiscard) does not work with at least certain
// versions of Clang
/**
 * @def KBLIB_NODISCARD
 * @brief This internal macro is used to provide a fallback for [[nodiscard]]
 * in C++14.
 */
#if __cplusplus > 201402L
#define KBLIB_NODISCARD [[nodiscard]]
#else
#define KBLIB_NODISCARD [[gnu::warn_unused_result]]
#endif

/**
 * @def KBLIB_UNUSED
 * @brief This internal macro is used to provide a fallback for [[maybe_unused]]
 * in C++14.
 */
#if KBLIB_USE_CXX17
#define KBLIB_UNUSED [[maybe_unused]]
#else
#define KBLIB_UNUSED [[gnu::unused]]
#endif

#if defined(_DOXYGEN_) && !defined(KBLIB_DEF_MACROS)
/**
 * @def KBLIB_DEF_MACROS
 * @brief If this macro is defined, kblib will define certain macros without the
 * KBLIB_ prefix.
 */
#define KBLIB_DEF_MACROS
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

} // namespace detail

enum class endian { unknown, little, big, weird };

namespace detail {
	constexpr endian get_system_endian() {
		if (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__) {
			return endian::big;
		} else if (__BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__) {
			return endian::little;
		} else {
			return endian::weird;
		}
	}
} // namespace detail

#ifdef __BYTE_ORDER__
constexpr endian system_endian = detail::get_system_endian();
#else
constexpr endian system_endian = endian::unknown;
#endif

namespace detail {
	constexpr endian get_hash_order() {
		if (system_endian == endian::little || system_endian == endian::big) {
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

} // namespace kblib

#endif // KBLIB_TDECL_H
