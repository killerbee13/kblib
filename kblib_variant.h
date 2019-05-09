#ifndef KBLIB_VARIANT_H
#define KBLIB_VARIANT_H

#include <cstddef>
#include <new>
#include "kblib_convert.h"
#include "kblib_logic.h"
#include "kblib_simple.h"

namespace kblib {

template <typename To, typename... Ts>
To coerce(const std::variant<Ts...>& v) {
  return std::visit([](const auto& t) { return lexical_cast<To>(t); }, v);
}

namespace detail {

template <typename T>
struct tuple_type {
  using type = std::tuple<>;
};
template <typename... Ts>
struct tuple_type<std::variant<Ts...>> {
  using type = std::tuple<Ts...>;
};

template <typename T>
using tuple_type_t = typename tuple_type<T>::type;

}  // namespace detail

template <typename To, typename From>
To variant_cast(From&& v) {
  static_assert(
      detail::contains_types_v<detail::tuple_type_t<std::decay_t<To>>,
                               detail::tuple_type_t<std::decay_t<From>>>,
      "To must include all types in From");
  return std::visit([](auto&& x) -> To { return std::forward<decltype(x)>(x); },
                    std::forward<From>(v));
}

// Builder class for std::visiting a std::variant
// from eracpp on the C++ Help discord
template <typename... Ts>
struct visitor : Ts... {
  using Ts::operator()...;
};
template <typename... Ts>
visitor(Ts...)->visitor<Ts...>;

template <typename V, typename... Ts>
auto visit(V&& v, Ts&&... ts) {
  return std::visit(visitor{std::forward<Ts>(ts)...}, std::forward<V>(v));
}

namespace detail {
enum class construct_type {
  none = 0,
  copy = 1,
  move = 2,
  both = 3,

};

constexpr construct_type operator|(construct_type l, construct_type r) {
  return static_cast<construct_type>(etoi(l) | etoi(r));
}

constexpr construct_type operator&(construct_type l, construct_type r) {
  return static_cast<construct_type>(etoi(l) & etoi(r));
}

constexpr construct_type operator*(construct_type l, bool r) {
  return r ? l : construct_type::none;
}

template <construct_type traits>
struct construct_conditional;

template <>
struct construct_conditional<construct_type::none> {
  construct_conditional() noexcept = default;
  construct_conditional(const construct_conditional&) = delete;
};

template <>
struct construct_conditional<construct_type::copy> {
  construct_conditional() noexcept = default;
  construct_conditional(const construct_conditional&) noexcept = default;
  construct_conditional(construct_conditional&&) = delete;
};

template <>
struct construct_conditional<construct_type::move> {
  construct_conditional() noexcept = default;
  construct_conditional(construct_conditional&&) noexcept = default;
};

template <>
struct construct_conditional<construct_type::both> {};

template <typename T>
constexpr construct_type construct_traits =
    construct_type::copy* std::is_copy_constructible_v<T> |
    construct_type::move* std::is_move_constructible_v<T>;

template <construct_type traits>
struct assign_conditional;

template <>
struct assign_conditional<construct_type::none> {
  assign_conditional() noexcept = default;
  assign_conditional& operator=(const assign_conditional&) = delete;
};

template <>
struct assign_conditional<construct_type::copy> {
  assign_conditional() noexcept = default;
  assign_conditional& operator=(const assign_conditional&) noexcept = default;
  assign_conditional& operator=(assign_conditional&&) = delete;
};

template <>
struct assign_conditional<construct_type::move> {
  assign_conditional() noexcept = default;
  assign_conditional& operator=(assign_conditional&&) noexcept = default;
};

template <>
struct assign_conditional<construct_type::both> {};

template <typename T>
constexpr construct_type assign_traits =
    construct_type::copy* std::is_copy_assignable_v<T> |
    construct_type::move* std::is_move_assignable_v<T>;

template <typename T>
struct disable_conditional : construct_conditional<construct_traits<T>>,
                             assign_conditional<assign_traits<T>> {};

inline void noop(void*, void*) {}
inline void noop(void*, const void*) {}

template <construct_type traits>
struct erased_construct_helper {};

template <>
struct erased_construct_helper<construct_type::copy> {
  alias<void (*)(void*, const void*)> copy = &noop;
};

template <>
struct erased_construct_helper<construct_type::move> {
  alias<void (*)(void*, void*)> move = &noop;
};

template <>
struct erased_construct_helper<construct_type::both>
    : erased_construct_helper<construct_type::copy>,
      erased_construct_helper<construct_type::move> {};

template <typename T, typename hash = void>
struct erased_hash_t {};

template <typename T>
struct erased_hash_t<T, std::void_t<std::invoke_result_t<std::hash<T>, T>>> {
  static std::size_t default_hash(void* obj) {
    return std::hash<T>()(*reinterpret_cast<const T*>(obj));
  }

  alias<std::size_t (*)(void*)> hash = &default_hash;
};
template <typename T>
void default_copy(void* dest, const void* from) {
  new (dest) T(*reinterpret_cast<const T*>(from));
}
template <typename T>
void default_move(void* dest, void* from) {
  new (dest) T(std::move(*reinterpret_cast<T*>(from)));
}

template <typename T, bool noop = false>
erased_construct_helper<construct_traits<T>> make_ops_t() {
  if constexpr (noop) {
    return {};
  }
  if constexpr (construct_traits<T> == construct_type::none) {
    return {};
  } else if constexpr (construct_traits<T> == construct_type::copy) {
    return {{&default_copy<T>}};
  } else if constexpr (construct_traits<T> == construct_type::move) {
    return {{&default_move<T>}};
  } else {
    return {{&default_copy<T>}, {&default_move<T>}};
  }
}

}  // namespace detail

// Inline polymorphic object. Generally mimics the interface of std::optional
template <typename Obj, std::size_t Capacity = sizeof(Obj)>
class poly_obj
    : detail::disable_conditional<Obj>,
      detail::erased_construct_helper<detail::construct_traits<Obj>> {
  static_assert(Capacity >= sizeof(Obj),
                "Capacity must be large enough for the object type.");
  using disabler = detail::disable_conditional<Obj>;
  using ops_t = detail::erased_construct_helper<detail::construct_traits<Obj>>;

 public:
  constexpr poly_obj() = default;
  constexpr poly_obj(std::nullptr_t) : poly_obj() {}
  constexpr poly_obj(const poly_obj& other) noexcept(
      std::is_nothrow_copy_constructible<Obj>::value)
      : disabler(other), ops_t(other), contains_value(other.contains_value) {
    if (contains_value) {
      this->copy(data, other.get());
    }
  }
  constexpr poly_obj(poly_obj&& other) noexcept(
      std::is_nothrow_move_constructible<Obj>::value)
      : disabler(std::move(other)),
        ops_t(std::move(other)),
        contains_value(other.contains_value) {
    if (contains_value) {
      this->move(data, other.get());
    }
  }

  constexpr poly_obj(const Obj& obj) noexcept(
      std::is_nothrow_copy_constructible<Obj>::value)
      : contains_value(true) {
    new (data) Obj(obj);
  }
  constexpr poly_obj(Obj&& obj) noexcept(
      std::is_nothrow_move_constructible<Obj>::value)
      : contains_value(true) {
    new (data) Obj(std::move(obj));
  }

  template <
      typename... Args,
      typename std::enable_if_t<std::is_constructible_v<Obj, Args...>, int> = 0>
  constexpr explicit poly_obj(std::in_place_t, Args&&... args)
      : ops_t(detail::make_ops_t<Obj>()), contains_value(true) {
    new (data) Obj(std::forward<Args>(args)...);
  }

  /**
   * @brief
   *
   * @param other
   * @return poly_obj &&operator
   */
  poly_obj& operator=(const poly_obj& other) & {
    if (contains_value) {
      get()->~Obj();
    }
    contains_value = other.contains_value;
    static_cast<ops_t&>(*this) = other;
    this->copy(data, reinterpret_cast<const void*>(other.get()));
    return *this;
  }

  poly_obj& operator=(poly_obj&& other) & {
    if (contains_value) {
      get()->~Obj();
    }
    contains_value = other.contains_value;
    static_cast<ops_t&>(*this) = other;
    this->move(data, reinterpret_cast<void*>(other.get()));
    return *this;
  }

  template <typename U, typename... Args>
  static poly_obj make(Args&&... args) {
    static_assert(sizeof(U) <= Capacity,
                  "U must fit inside of the inline capacity.");
    static_assert(std::is_base_of_v<Obj, U> && std::is_convertible_v<U*, Obj*>,
                  "Obj must be an accessible base of Obj.");
    static_assert(std::has_virtual_destructor_v<Obj>,
                  "It must be safe to delete a U through an Obj*.");
    static_assert(implies_v<std::is_copy_constructible_v<Obj>,
                            std::is_copy_constructible_v<U>>,
                  "U must be copy constructible if Obj is.");
    static_assert(implies_v<std::is_move_constructible_v<Obj>,
                            std::is_move_constructible_v<U>>,
                  "U must be move constructible if Obj is.");
    return {tag<U>{}, std::forward<Args>(args)...};
  }

  Obj& operator*() & noexcept {
    return *std::launder(reinterpret_cast<Obj*>(data));
  }
  const Obj& operator*() const& noexcept {
    return *std::launder(reinterpret_cast<const Obj*>(data));
  }
  Obj&& operator*() &&
      noexcept(std::is_nothrow_move_constructible<Obj>::value) {
    return std::move(*std::launder(reinterpret_cast<Obj*>(data)));
  }
  const Obj&& operator*() const&& noexcept(
      std::is_nothrow_move_constructible<Obj>::value) {
    return std::move(*std::launder(reinterpret_cast<const Obj*>(data)));
  }

  Obj* get() & noexcept { return std::launder(reinterpret_cast<Obj*>(data)); }
  const Obj* get() const& noexcept {
    return std::launder(reinterpret_cast<const Obj*>(data));
  }

  Obj* operator->() noexcept { return get(); }
  const Obj* operator->() const noexcept { return get(); }

  bool has_value() const { return contains_value; }

  /**
   * @brief Empties the poly_obj, reverting to a default-constructed state.
   *
   */
  void clear() { *this = {}; }

  ~poly_obj() noexcept {
    if (contains_value) {
      get()->~Obj();
    }
  }

 private:
  alignas(Obj) std::byte data[Capacity];
  bool contains_value = false;
  template <typename U>
  struct tag {};
  template <typename U, typename... Args>
  poly_obj(tag<U>, Args&&... args)
      : ops_t(detail::make_ops_t<U>()), contains_value(true) {
    new (data) U(std::forward<Args>(args)...);
  }
};

}  // namespace kblib

#endif  // KBLIB_VARIANT_H
