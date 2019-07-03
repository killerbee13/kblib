#ifndef KBLIB_ITERATORS_H
#define KBLIB_ITERATORS_H

#include "fakestd.h"

#include <iterator>
#include <cassert>

namespace kblib {

template <typename ptr>
struct to_pointer_impl {
  constexpr auto operator()(ptr&& p) const noexcept {
    return to_pointer_impl<decltype(p.operator->())>{}(p.operator->());
  }
  constexpr auto operator()(const ptr& p) const noexcept {
    return to_pointer_impl<decltype(p.operator->())>{}(p.operator->());
  }
};

template <typename T>
struct to_pointer_impl<T*> {
  constexpr T* operator()(T* p) const noexcept { return p; }
};

/**
 * @brief Gets a raw pointer out of any smart pointer or iterator you might pass
 * in, without dereferencing it or relying on a get() method.
 *
 * @param p A smart pointer to extract from.
 */
template <typename P>
constexpr auto to_pointer(P&& p) noexcept {
  return to_pointer_impl<std::decay_t<P>>{}(p);
}

template <typename Container,
          typename Comp = std::less<value_type_linear_t<Container>>>
value_type_linear_t<Container>* max_element(Container& c, Comp comp) {
  auto it = fakestd::max_element(std::begin(c), std::end(c), comp);
  if (it != std::end(c)) {
    return to_pointer(it);
  } else {
    return nullptr;
  }
}
/**
 * @brief Determine if T is a valid output iterator to which values of type E
 * may be written.
 */
template <typename T, typename E, typename = void>
struct is_output_iterator : std::false_type {};

template <typename T, typename E>
struct is_output_iterator<
    T, E,
    fakestd::void_t<decltype(*std::declval<T&>() = std::declval<const E&>())>>
    : std::true_type {};

template <typename Container>
/**
 * @brief Like a std::back_insert_iterator, but it keeps track of how many
 * insertions it has made, allowing an end iterator to be created.
 *
 * @attention This iterator must be incremented and dereferenced exactly once
 * for each assignment, in order to maintain the accuracy of the counter.
 *
 */
class counting_back_insert_iterator {
 public:
  using value_type = void;
  using difference_type = std::ptrdiff_t;
  using pointer = void;
  using reference = void;
  using iterator_category = std::output_iterator_tag;

  counting_back_insert_iterator() noexcept = default;
  explicit counting_back_insert_iterator(Container& c, std::size_t n = 0)
      : container(std::addressof(c)), count(n) {}
  explicit counting_back_insert_iterator(std::size_t n) : count(n) {}

  struct proxy_iterator {
    using value_type = typename Container::value_type;

    proxy_iterator& operator=(const value_type& value) {
      assert(container);
      // Multiple assignments for a single dereference are not allowed
      assert(*dirty);
      *dirty = false;
      container->push_back(value);
      return *this;
    }

    proxy_iterator& operator=(value_type&& value) {
      assert(container);
      // Multiple assignments for a single dereference are not allowed
      assert(*dirty);
      *dirty = false;
      container->push_back(std::move(value));
      return *this;
    }

    Container* container;
    bool* dirty;
  };

  proxy_iterator operator*() noexcept {
    assert(dirty);
    return {container, &dirty};
  }

  counting_back_insert_iterator& operator++() noexcept {
    assert(!dirty);
    ++count;
    dirty = true;
    return *this;
  }
  counting_back_insert_iterator operator++(int) noexcept = delete;

  friend bool operator==(const counting_back_insert_iterator& a,
                         const counting_back_insert_iterator& b) {
    return a.count == b.count;
  }
  friend bool operator!=(const counting_back_insert_iterator& a,
                         const counting_back_insert_iterator& b) {
    return a.count != b.count;
  }
  friend bool operator<(const counting_back_insert_iterator& a,
                        const counting_back_insert_iterator& b) {
    return a.count < b.count;
  }
  friend bool operator<=(const counting_back_insert_iterator& a,
                         const counting_back_insert_iterator& b) {
    return a.count <= b.count;
  }
  friend bool operator>(const counting_back_insert_iterator& a,
                        const counting_back_insert_iterator& b) {
    return a.count > b.count;
  }
  friend bool operator>=(const counting_back_insert_iterator& a,
                         const counting_back_insert_iterator& b) {
    return a.count >= b.count;
  }
  friend std::ptrdiff_t operator-(const counting_back_insert_iterator& a,
                                  const counting_back_insert_iterator& b) {
    return std::ptrdiff_t(a.count) - ptrdiff_t(b.count);
  }

 protected:
  Container* container = nullptr;
  std::size_t count = 0;
  bool dirty = true;
};

template <typename C>
KBLIB_NODISCARD counting_back_insert_iterator<C> counting_back_inserter(
    C& c, std::size_t count = 0) {
  return counting_back_insert_iterator<C>{c, count};
}

}  // namespace kblib

#endif  // KBLIB_ITERATORS_H
