#ifndef KBLIB_LOGIC_H
#define KBLIB_LOGIC_H

#include <type_traits>

namespace kblib {

template <bool A, bool B>
/**
 * @brief
 *
 */
struct implies : std::integral_constant<bool, true> {};

template <>
/**
 * @brief
 *
 */
struct implies<true, false> : std::integral_constant<bool, false> {};

#if __cplusplus >= 201703L
template <bool A, bool B>
constexpr inline bool implies_v = implies<A, B>::value; /**< TODO: describe */
#endif

}

#endif // KBLIB_LOGIC_H
