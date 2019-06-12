#ifndef KBLIB_FAKESTD_H
#define KBLIB_FAKESTD_H

#include "tdecl.h"

#include <array>
#include <memory>
#include <type_traits>
#include <utility>

// This header provides some features of C++17 <type_traits> for C++14; see
// below.
#ifndef KBLIB_FAKESTD
#define KBLIB_FAKESTD (__cplusplus < 201703L)
#endif

// Note that has_cpp_attribute(nodiscard) does not work with at least certain
// versions of Clang
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

template <typename Fn, typename... Args,
          std::enable_if_t<std::is_member_pointer<std::decay_t<Fn>>{}, int> = 0>
constexpr decltype(auto) invoke(Fn&& f, Args&&... args) noexcept(
    noexcept(std::mem_fn(f)(std::forward<Args>(args)...))) {
  return std::mem_fn(f)(std::forward<Args>(args)...);
}

template <
    typename Fn, typename... Args,
    std::enable_if_t<!std::is_member_pointer<std::decay_t<Fn>>{}, int> = 0>
constexpr decltype(auto) invoke(Fn&& f, Args&&... args) noexcept(
    noexcept(std::forward<Fn>(f)(std::forward<Args>(args)...))) {
  return std::forward<Fn>(f)(std::forward<Args>(args)...);
}

namespace detail {

template <typename AlwaysVoid, typename, typename...>
struct invoke_result {};
template <typename F, typename... Args>
struct invoke_result<decltype(void(
                         invoke(std::declval<F>(), std::declval<Args>()...))),
                     F, Args...> {
  using type = decltype(invoke(std::declval<F>(), std::declval<Args>()...));
};
}  // namespace detail
template <class F, class... ArgTypes>
struct invoke_result : detail::invoke_result<void, F, ArgTypes...> {};

template <typename F, typename... ArgTypes>
using invoke_result_t = typename invoke_result<F, ArgTypes...>::type;

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

#if KBLIB_USE_CXX17

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

namespace detail {
template <typename F>
struct not_fn_t {
  explicit not_fn_t(F&& f) : fd(std::forward<F>(f)) {}
  not_fn_t(const not_fn_t&) = default;
  not_fn_t(not_fn_t&&) = default;

  template <class... Args>
  auto operator()(Args&&... args) & -> decltype(
      !std::declval<invoke_result_t<std::decay_t<F>&, Args...>>()) {
    return !invoke(fd, std::forward<Args>(args)...);
  }

  template <class... Args>
  auto operator()(Args&&... args) const& -> decltype(
      !std::declval<invoke_result_t<std::decay_t<F> const&, Args...>>()) {
    return !invoke(std::move(fd), std::forward<Args>(args)...);
  }

  std::decay_t<F> fd;
};
}  // namespace detail

template <typename F>
detail::not_fn_t<F> not_fn(F&& f) {
  return detail::not_fn_t<F>(std::forward<F>(f));
}

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

/**
 * @brief Essentially just like std::enable_if, but with a different name that
 * makes it clearer what it does in the context of return type SFINAE.
 */
template <bool V, typename T>
struct return_assert {};

template <typename T>
struct return_assert<true, T> {
  using type = T;
};

template <bool V, typename T>
using return_assert_t = typename return_assert<V, T>::type;

template <typename T>
std::unique_ptr<T> to_unique(T* p) {
  return std::unique_ptr<T>(p);
}
template <typename T, typename D>
std::unique_ptr<T, D> to_unique(T* p, D&& d) {
  return std::unique_ptr<T, D>(p, d);
}

template <typename I>
constexpr std::make_unsigned_t<I> to_unsigned(I x) {
  return static_cast<std::make_unsigned_t<I>>(x);
}
template <typename I>
constexpr std::make_signed_t<I> to_signed(I x) {
  return static_cast<std::make_signed_t<I>>(x);
}

// Cast argument to equivalently-sized type with the same signedness as the
// template parameter
template <typename A, typename F>
KBLIB_NODISCARD
constexpr std::enable_if_t<std::is_integral<A>::value && std::is_integral<F>::value &&
                         std::is_signed<A>::value,
                     std::make_signed_t<F>>
    signed_cast(F x) {
  return to_signed(x);
}

template <typename A, typename F>
KBLIB_NODISCARD
constexpr std::enable_if_t<std::is_integral<A>::value && std::is_integral<F>::value &&
                         std::is_unsigned<A>::value,
                     std::make_unsigned_t<F>>
    signed_cast(F x) {
  return to_unsigned(x);
}

#if KBLIB_USE_CXX17

namespace detail {

template <typename T>
constexpr std::intmax_t max_val = std::numeric_limits<T>::max();

constexpr unsigned long long msb(unsigned long long x) {
  x |= (x >> 1);
  x |= (x >> 2);
  x |= (x >> 4);
  x |= (x >> 8);
  x |= (x >> 16);
  x |= (x >> 32);
  return (x & ~(x >> 1));
}

template <typename Num>
constexpr Num msb_possible() {
  return std::numeric_limits<Num>::max() ^
         (std::numeric_limits<Num>::max() >> 1);
}

template <class... Args>
struct type_list {
  template <std::size_t N>
  using type = typename std::tuple_element<N, std::tuple<Args...>>::type;
};

template <auto K, typename V>
struct type_map_el {
  constexpr static auto key = K;
  using value = V;
};

template <typename Key, typename Comp, typename... Vals>
struct type_map {
  using types = type_list<Vals...>;
  template <std::size_t I>
  using element = typename types::template type<I>;

  template <Key key, std::size_t I = 0>
  constexpr static auto get() {
    static_assert(I < sizeof...(Vals), "key not found");
    if constexpr (Comp{}(key, element<I>::key)) {
      return tag<typename element<I>::value>{};
    } else {
      return get<key, I + 1>();
    }
  }

  template <Key key, typename Default = void, std::size_t I = 0>
  constexpr static auto get_default() {
    if constexpr (I == sizeof...(Vals)) {
      return Default();
    } else if constexpr (Comp{}(key, element<I>::key)) {
      return tag<typename element<I>::value>{};
    } else {
      return get<key, I + 1>();
    }
  }
};

template <typename N>
using make_smap_el =
    type_map_el<static_cast<std::intmax_t>(msb_possible<N>()), N>;

template <typename T>
struct next_larger_signed {
  static_assert(max_val<T> < max_val<std::intmax_t>,
                "Cannot safely promote intmax_t.");
  struct false_compare {
    template <typename U>
    constexpr bool operator()(U, U) {
      return true;
    }
  };

  using ints_map = type_map<
      std::intmax_t, std::less<std::intmax_t>, make_smap_el<std::int_least8_t>,
      make_smap_el<std::int_least16_t>, make_smap_el<std::int_least32_t>,
      make_smap_el<std::int_least64_t>, make_smap_el<std::intmax_t>>;

  using type =
      typename decltype(ints_map::template get_default<max_val<T> + 1>())::type;
};

template <typename N, bool = std::is_signed<N>::value>
struct filter_signed;

template <typename N>
struct filter_signed<N, true> {
  using type = N;
};

template <typename N>
using filter_signed_t = typename filter_signed<N>::type;

template <typename N, bool = std::is_unsigned<N>::value>
struct filter_unsigned;

template <typename N>
struct filter_unsigned<N, true> {
  using type = N;
};

template <typename N>
using filter_unsigned_t = typename filter_unsigned<N>::type;

}  // namespace detail

template <typename N, typename = void>
struct safe_signed;

template <typename N>
struct safe_signed<N, std::enable_if_t<std::is_integral<N>::value, void>> {
  using type = std::conditional_t<
      std::is_signed<N>::value, N,
      typename detail::next_larger_signed<std::make_signed_t<N>>::type>;
};

template <typename N>
using safe_signed_t = typename safe_signed<N>::type;

template <typename N>
KBLIB_NODISCARD safe_signed_t<N> signed_promote(N x) {
  return static_cast<safe_signed_t<N>>(x);
}

#endif

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

template <typename T, typename = void>
struct value_detected : std::false_type {};

template <typename T>
struct value_detected<T, fakestd::void_t<typename T::value_type>>
    : std::true_type {};

template <typename T>
constexpr bool value_detected_v = value_detected<T>::value;

template <typename T, typename = void>
struct key_detected : std::false_type {};

template <typename T>
struct key_detected<T, fakestd::void_t<typename T::key_type>> : std::true_type {
};

template <typename T>
constexpr bool key_detected_v = key_detected<T>::value;

template <typename T, typename = void>
struct mapped_detected : std::false_type {};

template <typename T>
struct mapped_detected<T, fakestd::void_t<typename T::mapped_type>>
    : std::true_type {};

template <typename T>
constexpr bool mapped_detected_v = mapped_detected<T>::value;

template <typename Container, bool = key_detected_v<Container>,
          typename T = typename Container::value_type>
struct value_type_linear {};

template <typename Container>
struct value_type_linear<Container, false, typename Container::value_type> {
  using type = typename Container::value_type;
};

template <typename Container>
using value_type_linear_t = typename value_type_linear<Container>::type;

template <typename Container>
constexpr static bool is_linear_container_v =
    value_detected_v<Container> && !key_detected_v<Container>;

template <typename Container, bool = key_detected_v<Container>,
          bool = mapped_detected_v<Container>>
struct key_type_setlike {};

template <typename Container>
struct key_type_setlike<Container, true, false> {
  using type = typename Container::key_type;
};

template <typename Container>
using key_type_setlike_t = typename key_type_setlike<Container>::type;

template <typename Container>
constexpr static bool is_setlike_v =
    key_detected_v<Container> && !mapped_detected_v<Container>;

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

namespace detail {
template <typename D, typename T, typename = void>
struct pointer {
  using type = T*;
};

template <typename D, typename T>
struct pointer<D, T, fakestd::void_t<typename D::pointer>> {
  using type = typename D::pointer;
};
}

template <typename T>
class heap_value {
public:
  using pointer = T*;
  using element_type = T;
  using reference = T&;

  constexpr heap_value() noexcept : p{nullptr} {}
  constexpr heap_value(std::nullptr_t) noexcept : p{nullptr} {}

  template <typename... Args, typename std::enable_if<std::is_constructible<T, Args...>::value>::type = 0>
  heap_value(std::in_place_t, Args&&... args) : p{new T(args...)} {}

  heap_value(const heap_value& u) : p{(u.p ? (new T(*u.p)) : nullptr)} {}
  heap_value(heap_value&& u) : p{std::exchange(u.p, nullptr)} {}

  heap_value& operator=(const heap_value& u) {
    if (!u) {
      reset();
    } else {
      if (p) {
        *p = *u;
      } else {
        p = new T(*u.p);
      }
    }
    return *this;
  }

  heap_value& operator=(heap_value&& u) {
    reset();
    p = std::exchange(u.p, nullptr);
    return *this;
  }

  void reset() {
    delete p;
    p = nullptr;
    return;
  }

  KBLIB_NODISCARD pointer get() const noexcept {
    return p;
  }

  KBLIB_NODISCARD operator bool() const noexcept {
    return p != nullptr;
  }

  reference operator*() const noexcept {
    return *p;
  }

  ~heap_value() {
    delete p;
  }

private:
  pointer p;
};

}  // namespace kblib

#endif  // KBLIB_FAKESTD_H
