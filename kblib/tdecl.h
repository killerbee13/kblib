#ifndef KBLIB_TDECL_H
#define KBLIB_TDECL_H

/**
 * @file
 * Contains basic declarations needed by other files.
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

#if defined(_DOXYGEN_) && !defined(KBLIB_DEF_MACROS)
/**
 * @def KBLIB_DEF_MACROS
 * @brief If this macro is defined, kblib will define certain macros without the
 * KBLIB_ prefix.
 */
#define KBLIB_DEF_MACROS
#endif

namespace kblib {
namespace detail {

   template <typename Container, bool, typename...>
   struct buildiota_impl;

	template <typename T>
	struct tag {
		using type = T;
	};

} // namespace detail
} // namespace kblib

#endif // KBLIB_TDECL_H
