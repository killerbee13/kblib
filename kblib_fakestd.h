#ifndef KBLIB_FAKESTD_H
#define KBLIB_FAKESTD_H

#include <memory>
#include <type_traits>
#include <utility>

// This header provides some features of C++17 <type_traits> for C++14; see
// below.
#ifndef KBLIB_FAKESTD
#define KBLIB_FAKESTD (__cplusplus < 201703L)
#endif

// Note that has_cpp_attribute(nodiscard) does not work with Clang
#if defined(__has_cpp_attribute) && __has_cpp_attribute(nodiscard)
#define NODISCARD [[nodiscard]]
#endif

#if __cplusplus > 201402L
#define KBLIB_NODISCARD [[nodiscard]]
#else
#define KBLIB_NODISCARD
#endif

namespace kblib {

#if KBLIB_FAKESTD
namespace fakestd {  // C++14 implementation of C++17 void_t, invoke_result,
                     // (partially) is_invocable, and is_nothrow_swappable
using std::swap;

namespace detail {
template <class F, class... Args>
decltype(auto) INVOKE(F&& f, Args&&... args) {
  return std::forward<F>(f)(std::forward<Args>(args)...);
}

template <typename AlwaysVoid, typename, typename...>
struct invoke_result {};
template <typename F, typename... Args>
struct invoke_result<decltype(void(detail::INVOKE(std::declval<F>(),
                                                  std::declval<Args>()...))),
                     F, Args...> {
  using type =
      decltype(detail::INVOKE(std::declval<F>(), std::declval<Args>()...));
};
}  // namespace detail
template <class F, class... ArgTypes>
struct invoke_result : detail::invoke_result<void, F, ArgTypes...> {};

template <typename... Ts>
struct make_void {
  typedef void type;
};
template <typename... Ts>
using void_t = typename make_void<Ts...>::type;

namespace detail {
// ALL generic swap overloads MUST already have a declaration available at this
// point.

struct nat {
  nat() = delete;
  nat(const nat&) = delete;
  nat& operator=(const nat&) = delete;
  ~nat() = delete;
};

struct two {
  char lx[2];
};

struct is_referenceable_impl {
  template <class Tp>
  static Tp& test(int);
  template <class Tp>
  static two test(...);
};

template <class Tp>
struct is_referenceable
    : std::integral_constant<
          bool, !std::is_same<decltype(is_referenceable_impl::test<Tp>(0)),
                              two>::value> {};

template <class Tp, class Up = Tp,
          bool NotVoid = !std::is_void<Tp>::value && !std::is_void<Up>::value>
struct swappable_with {
  template <class LHS, class RHS>
  static decltype(swap(std::declval<LHS>(), std::declval<RHS>())) test_swap(
      int);
  template <class, class>
  static nat test_swap(long);

  // Extra parens are needed for the C++03 definition of decltype.
  typedef decltype((test_swap<Tp, Up>(0))) swap1;
  typedef decltype((test_swap<Up, Tp>(0))) swap2;

  static const bool value =
      !std::is_same<swap1, nat>::value && !std::is_same<swap2, nat>::value;
};

template <class Tp, class Up>
struct swappable_with<Tp, Up, false> : std::false_type {};

template <class Tp, class Up = Tp,
          bool Swappable = swappable_with<Tp, Up>::value>
struct nothrow_swappable_with {
  static const bool value =
#ifndef LIBCPP_HAS_NO_NOEXCEPT
      noexcept(swap(std::declval<Tp>(), std::declval<Up>())) &&
      noexcept(swap(std::declval<Up>(), std::declval<Tp>()));
#else
      false;
#endif
};

template <class Tp, class Up>
struct nothrow_swappable_with<Tp, Up, false> : std::false_type {};

}  // namespace detail

template <class Tp>
struct is_swappable
    : public std::integral_constant<bool, detail::swappable_with<Tp&>::value> {
};

template <class Tp>
struct is_nothrow_swappable
    : public std::integral_constant<
          bool, detail::nothrow_swappable_with<Tp&>::value> {};

#if ____cplusplus >= 201703L

template <class Tp, class Up>
struct is_swappable_with
    : public std::integral_constant<bool,
                                    detail::swappable_with<Tp, Up>::value> {};

template <class Tp>
struct is_swappable
    : public std::conditional<
          detail::is_referenceable<Tp>::value,
          is_swappable_with<typename std::add_lvalue_reference<Tp>::type,
                            typename std::add_lvalue_reference<Tp>::type>,
          std::false_type>::type {};

template <class Tp, class Up>
struct is_nothrow_swappable_with
    : public integral_constant<bool,
                               detail::nothrow_swappable_with<Tp, Up>::value> {
};

template <class Tp>
struct is_nothrow_swappable
    : public conditional<
          detail::is_referenceable<Tp>::value,
          is_nothrow_swappable_with<typename add_lvalue_reference<Tp>::type,
                                    typename add_lvalue_reference<Tp>::type>,
          false_type>::type {};

template <class Tp, class Up>
inline constexpr bool is_swappable_with_v = is_swappable_with<Tp, Up>::value;

template <class Tp>
inline constexpr bool is_swappable_v = is_swappable<Tp>::value;

template <class Tp, class Up>
inline constexpr bool is_nothrow_swappable_with_v =
    is_nothrow_swappable_with<Tp, Up>::value;

template <class Tp>
inline constexpr bool is_nothrow_swappable_v = is_nothrow_swappable<Tp>::value;

#endif

}  // namespace fakestd
#else
namespace fakestd = std;
#endif

// metafunction_success:
// SFINAE detector for a ::type member type
template <typename T, typename = void>
struct metafunction_success : std::false_type {};

template <typename T>
struct metafunction_success<T, fakestd::void_t<typename T::type>>
    : std::true_type {};

template <typename... T>
struct is_callable : metafunction_success<fakestd::invoke_result<T...>> {};

template <typename T>
std::unique_ptr<T> to_unique(T* p) {
  return std::unique_ptr<T>(p);
}
template <typename T, typename D>
std::unique_ptr<T, D> to_unique(T* p, D&& d) {
  return std::unique_ptr<T, D>(p, d);
}

template <typename I>
std::make_unsigned_t<I> to_unsigned(I x) {
  return static_cast<std::make_unsigned_t<I>>(x);
}
template <typename I>
std::make_signed_t<I> to_signed(I x) {
  return static_cast<std::make_signed_t<I>>(x);
}

// Cast argument to equivalently-sized type with the same signedness as the
// template parameter
template <typename A, typename F>
KBLIB_NODISCARD
    std::enable_if_t<std::is_integral<A>::value && std::is_integral<F>::value &&
                         std::is_signed<A>::value,
                     std::make_signed_t<F>>
    signed_cast(F x) {
  return to_signed(x);
}

template <typename A, typename F>
KBLIB_NODISCARD
    std::enable_if_t<std::is_integral<A>::value && std::is_integral<F>::value &&
                         std::is_unsigned<A>::value,
                     std::make_unsigned_t<F>>
    signed_cast(F x) {
  return to_unsigned(x);
}

template <typename C, typename T,
          bool = std::is_const<typename std::remove_reference<C>::type>::value>
struct copy_const {
  using type = T;
};

template <typename C, typename T>
struct copy_const<C, T, true> {
  using type = const T;
};

template <typename C, typename V>
using copy_const_t = typename copy_const<C, V>::type;

//
// template <typename A, typename F>
// std::enable_if_t<!(std::is_integral<F>::value && std::is_integral<A>::value),
//                 void>
// signed_cast(F) {
//  static_assert(std::is_integral<F>::value && std::is_integral<A>::value,
//                "signed_cast arguments must be integral.");
//}

template <class InputIt1, class InputIt2>
constexpr bool equal(InputIt1 first1, InputIt1 last1, InputIt2 first2) {
  for (; first1 != last1; ++first1, ++first2) {
    if (!(*first1 == *first2)) {
      return false;
    }
  }
  return true;
}

template <class InputIt1, class InputIt2>
KBLIB_NODISCARD constexpr bool lexicographical_compare(InputIt1 first1,
                                                       InputIt1 last1,
                                                       InputIt2 first2,
                                                       InputIt2 last2) {
  for (; (first1 != last1) && (first2 != last2); ++first1, (void)++first2) {
    if (*first1 < *first2) return true;
    if (*first2 < *first1) return false;
  }
  return (first1 == last1) && (first2 != last2);
}

template <class InputIt, class Size, class OutputIt>
constexpr OutputIt copy_n(InputIt first, Size count, OutputIt result) {
  if (count > 0) {
    *result++ = *first;
    for (Size i = 1; i < count; ++i) {
      *result++ = *++first;
    }
  }
  return result;
}

// cond_ptr: A pointer which can either uniquely own its referent, or which can
// be a non-owning reference. Note that custom deleter support is not present;
// however it will not implicitly strip a deleter from a unique_ptr.

template <typename T>
class cond_ptr {
 public:
  using pointer = T*;
  using element_type = T;
  static_assert(std::is_nothrow_destructible<T>::value,
                "cond_ptr<T> requires that T::~T() not throw exceptions.");

  cond_ptr() noexcept = default;
  cond_ptr(std::nullptr_t) noexcept {}

  explicit cond_ptr(T* p, bool owner = false) noexcept
      : ptr_(p), owns_(owner) {}
  cond_ptr(std::unique_ptr<T>&& p) noexcept : ptr_(p.release()), owns_(true) {}

  // cond_ptr is move-only because it is maybe unique.
  cond_ptr(const cond_ptr&) noexcept = delete;
  cond_ptr(cond_ptr&&) noexcept = default;

  cond_ptr& operator=(const cond_ptr&) noexcept = delete;
  cond_ptr& operator=(cond_ptr&& rhs) noexcept {
    if (owns_) {
      delete ptr_;
    }
    owns_ = rhs.owns();
    ptr_ = rhs.release();
  }

  ~cond_ptr() noexcept {
    if (owns_) {
      delete ptr_;
    }
  }

  bool owns() const noexcept { return owns_; }
  KBLIB_NODISCARD T* release() noexcept {
    owns_ = false;
    return std::exchange(ptr_, nullptr);
  }

  void reset(T* p = nullptr, bool owner = false) noexcept {
    if (owns_) {
      delete ptr_;
    }
    ptr_ = p;
    owns_ = owner;
  }

  void swap(cond_ptr& other) {
    std::swap(ptr_, other.ptr_);
    std::swap(owns_, other.owns_);
  }

  KBLIB_NODISCARD T* get() const noexcept { return ptr_; }

  explicit operator bool() const noexcept { return ptr_; }

  KBLIB_NODISCARD T& operator*() const noexcept { return *ptr_; }

  KBLIB_NODISCARD T* operator->() const noexcept { return ptr_; }

 private:
  T* ptr_ = nullptr;
  bool owns_ = false;
};

}  // namespace kblib

#endif  // KBLIB_FAKESTD_H
