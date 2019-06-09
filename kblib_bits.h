#ifndef KBLIB_BITS_H
#define KBLIB_BITS_H

#include <array>
#include <bitset>
#include <limits>
#include <memory>
#include <utility>
#include <vector>

#include "kblib_fakestd.h"
#include "kblib_simple.h"
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
/*
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
};//*/

template <typename Key, Key key_range, typename Value>
class compact_bit_trie {
 public:
  struct key_type {
    Key prefix;
    short bits : detail::filg2(key_range);
  };

  using value_type = Value;
  using mapped_type = Value;
  using size_type = uint_smallest_t<key_range>;
  using difference_type = int_smallest_t<key_range>;
  using reference = value_type&;
  using const_reference = const value_type&;
  using pointer = value_type*;
  using const_pointer = const value_type*;

  using iterator = void;
  using const_iterator = void;
  using reverse_iterator = void;
  using const_reverse_iterator = void;

  static_assert(std::is_integral<Key>::value, "Key must be an integral type.");
  static_assert(std::is_unsigned<Key>::value, "Key must be unsigned.");
  static_assert(std::is_nothrow_destructible<mapped_type>::value,
                "mapped_type must be nothrow destructible.");

  value_type at(key_type key) const noexcept(false) {
    if (empty()) {
      throw std::out_of_range("searched in an empty compact_bit_trie");
    }
    if (key.bits > bits_of<Key>) {
      throw std::invalid_argument("key prefix longer than key length");
    }
    size_type node = 1;
    const std::bitset<bits_of<key_type>> search = key.prefix;
    int idx = 0;
    while (node && idx < key.bits) {
      if (auto val = storage[node].val) {
        return values[val];
      } else {
        node = storage[node].children[search[idx++]];
      }
    }
    throw std::out_of_range("key not found in compact_bit_trie");
  }

  value_type find_deep(key_type key, size_type depth = -1) const
      noexcept(false) {
    if (empty()) {
      throw std::out_of_range("searched in an empty compact_bit_trie");
    }
    if (key.bits > bits_of<Key>) {
      throw std::invalid_argument("key prefix longer than key length");
    }
    size_type node = 1;
    size_type found = 0;
    const std::bitset<bits_of<key_type>> search = key.prefix;
    int idx = 0;
    while (node && idx < key.bits) {
      if (auto val = storage[node].val) {
        found = val;
        if (depth--) {
          break;
        }
      }
      node = storage[node].children[search[idx++]];
    }
    if (found) {
      return values[found];
    } else {
      throw std::out_of_range("key not found in compact_bit_trie");
    }
  }

  bool empty() const noexcept;

  void insert(key_type key, const value_type& value);
  void insert(key_type key, value_type&& value) {
    if (key.bits > bits_of<Key>) {
      throw std::invalid_argument("key prefix longer than key length");
    }
  }

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
