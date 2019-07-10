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

template <>
/**
 * @brief Logical implication is only not satisfied by true -> false.
 */
struct implies<true, false> : std::integral_constant<bool, false> {};

#if KBLIB_USE_CXX17
/**
 * Equivalent to implies<A, B>::value.
 */
template <bool A, bool B>
constexpr inline bool implies_v = implies<A, B>::value;
#endif

} // namespace kblib

#endif // KBLIB_LOGIC_H
