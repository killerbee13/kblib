#ifndef KBLIB_TDECL_H
#define KBLIB_TDECL_H

/**
 * @file
 * Contains basic declarations needed by other files.
 */

#define KBLIB_USE_CXX17 __cplusplus >= 201703L
#define KBLIB_USE_STRING_VIEW KBLIB_USE_CXX17

namespace kblib {
namespace detail {

template <typename Container, bool, typename...>
struct buildiota_impl;

template <typename T>
struct tag {
  using type = T;
};

}  // namespace detail
}  // namespace kblib

#endif  // KBLIB_TDECL_H
