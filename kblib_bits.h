#ifndef KBLIB_BITS_H
#define KBLIB_BITS_H

#include <array>
#include <bitset>
#include <limits>
#include <memory>
#include <utility>

#include "kblib_fakestd.h"
#include "kblib_tdecl.h"

namespace kblib {

template <typename Int>
constexpr int bits_of = std::numeric_limits<Int>::digits;

namespace detail {

template <typename Key, typename Value>
class trie_node {
  std::unique_ptr<std::array<trie_node, 4>> children;
  unsigned char storage[sizeof(Value)]{};
  bool exists = false;

  template <typename... Args>
  Value& create(Args&&... args) noexcept(
      std::is_nothrow_constructible<Value, Args...>::value) {
    clear();
    // This variable must exist for exception safety. exists should not be set
    // to true if an exception is thrown.
    auto v = *new (storage) Value(args...);
    exists = true;
    return *v;
  }

  void clear() noexcept {
    if (exists) {
      get()->~Value();
    }
    return;
  }

  Value& get() noexcept {
    assert(exists);
    return *reinterpret_cast<Value*>(storage);
  }
  const Value& get() const noexcept {
    assert(exists);
    return *reinterpret_cast<Value*>(storage);
  }
};

}  // namespace detail

template <typename Key, typename Value>
class bit_trie {
 public:
  using value_type = Value;
  using key_type = Key;
  using mapped_type = Value;

  static_assert(std::is_integral<key_type>::value,
                "key_type must be an integral type.");
  static_assert(std::is_nothrow_destructible<mapped_type>::value,
                "mapped_type must be nothrow destructible.");

  void insert(key_type, const mapped_type&);
  void insert(key_type, mapped_type&&);

 private:
  std::array<detail::trie_node, bits_of<key_type>> roots;
};

namespace detail {

/**
 * @brief Floored integer binary logarithm. Returns floor(lb(val)).
 *
 * Returns the number of significant bits in the given integer.
 *
 * @param val
 * @return int
 */
constexpr int filg2(
    std::bitset<std::numeric_limits<std::uintmax_t>::digits> val) {
  for (int i : range(val.size(), 0, -1)) {
    if (val.test(i)) return i;
  }
  return 0;
}

template <std::size_t size, typename T, typename... Ts>
struct first_bigger_than : std::conditional<sizeof(T) >= size, detail::tag_t<T>,
                                            first_bigger_than<size, Ts...>> {};

template <std::size_t size, typename T>
struct first_bigger_than<size, T>
    : std::conditional_t<sizeof(T) >= size, detail::tag_t<T>, void> {};

template <std::uintmax_t I>
using uint_smallest_t =
    typename first_bigger_than<filg2(I) / CHAR_BIT, unsigned char,
                               unsigned short, unsigned int, unsigned long,
                               unsigned long long, std::uintmax_t>::type;

template <std::uintmax_t I>
using int_smallest_t =
    typename first_bigger_than<filg2(I) / CHAR_BIT, signed char, signed short,
                               signed int, signed long, signed long long,
                               std::uintmax_t>::type;

}  // namespace detail

template <typename Key, Key key_range, typename Value>
class compact_bit_trie {
 public:
  using value_type = Value;
  using key_type = Key;
  using mapped_type = Value;
  using size_type = detail::uint_smallest_t<key_range>;
  using difference_type = detail::int_smallest_t<key_range>;
  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;

  using iterator = void;
  using const_iterator = void;
  using reverse_iterator = void;
  using const_reverse_iterator = void;

  static_assert(std::is_integral<key_type>::value,
                "key_type must be an integral type.");
  static_assert(std::is_unsigned<key_type>::value,
                "key_type must be unsigned.");
  static_assert(std::is_nothrow_destructible<mapped_type>::value,
                "mapped_type must be nothrow destructible.");

  value_type at(key_type key) const noexcept(false) {
    if (empty) {throw std::out_of_range("searched in an empty compact_bit_trie");}
    size_type node = 1;
    std::bitset<bits_of<key_type>> search;
    int idx = 0;
    while (node && idx < search.size()) {
      if (auto val = storage[node].val) {
        return values[val];
      } else {
        node = storage[node].children[search.test(idx++)];
      }
    }
    throw std::out_of_range("key not found in compact_bit_trie");
  }

  value_type find_deep(key_type key, size_type depth = -1) const
      noexcept(false) {
    if (empty) {throw std::out_of_range("searched in an empty compact_bit_trie");}
    size_type node = 1;
    size_type found = 0;
    std::bitset<bits_of<key_type>> search;
    int idx = 0;
    while (node && idx < search.size()) {
      if (auto val = storage[node].val) {
        found = val;
        if (depth--) {
          break;
        }
      }
      node = storage[node].children[search.test(idx++)];
    }
    if (found) {
      return values[found];
    } else {
      throw std::out_of_range("key not found in compact_bit_trie");
    }
  }

  void insert(key_type key, int length, const value_type& value);
  void insert(key_type key, int length, value_type&& value);
  void insert_or_assign(key_type key, const value_type& value);
  void insert_or_assign(key_type key, value_type&& value);

  bool erase(key_type key);
  bool prune(key_type prefix);
  void clear();

 private:
  struct inline_node {
    size_type children[2];
    size_type parent;
    size_type val;
  };
  std::vector<inline_node> storage;
  std::vector<Value> values;
};

}  // namespace kblib

#endif  // KBLIB_BITS_H
