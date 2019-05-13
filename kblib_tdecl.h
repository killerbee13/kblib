#ifndef KBLIB_TDECL_H
#define KBLIB_TDECL_H

#define KBLIB_USE_CXX17 __cplusplus >= 201703L

namespace kblib {
namespace detail {

template <typename Container, bool, typename...>
struct buildiota_impl;

template <typename T>
struct tag {
  using type = T;
};

}
}

#endif // KBLIB_TDECL_H
