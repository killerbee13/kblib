#ifndef KBLIB_LOGIC_H
#define KBLIB_LOGIC_H

#include <type_traits>

namespace kblib {

template <bool A, bool B>
struct implies : std::bool_constant<true> {};

template <>
struct implies<true, false> : std::bool_constant<false> {};

#if __cplusplus >= 201703L
template <bool A, bool B>
constexpr inline bool implies_v = implies<A, B>::value;
#endif

}

#endif // KBLIB_LOGIC_H
