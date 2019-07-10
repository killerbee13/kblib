#ifndef KBLIB_BITS_H
#define KBLIB_BITS_H

#include <array>
#include <bitset>
#include <limits>
#include <memory>
#include <utility>
#include <vector>

#include <iostream>

#include "fakestd.h"
#include "simple.h"
#include "stats.h"
#include "tdecl.h"

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

} // namespace detail
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
		unsigned short bits : detail::filg2(key_range);
	};

	using value_type = Value;
	using mapped_type = Value;
	using size_type = uint_smallest_t<key_range>;
	using difference_type = int_smallest_t<key_range>;
	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;

	template <typename V>
	class iterator_t;

	using iterator = iterator_t<Value>;
	using const_iterator = iterator_t<const Value>;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	using bitset_type = std::bitset<bits_of<Key>>;

	static_assert(std::is_integral<Key>::value, "Key must be an integral type.");
	static_assert(std::is_unsigned<Key>::value, "Key must be unsigned.");
	static_assert(std::is_nothrow_destructible<mapped_type>::value,
	              "mapped_type must be nothrow destructible.");

	const_reference at(key_type key) const noexcept(false) {
		if (empty()) {
			throw std::out_of_range("searched in an empty compact_bit_trie");
		}
		if (key.bits > bits_of<Key>) {
			throw std::invalid_argument("key prefix longer than key length");
		}
		size_type node = 1;
		const bitset_type search = key.prefix;
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

	reference at(key_type key) noexcept(false) {
		if (empty()) {
			throw std::out_of_range("searched in an empty compact_bit_trie");
		}
		if (key.bits > bits_of<Key>) {
			throw std::invalid_argument("key prefix longer than key length");
		}
		size_type node = 1;
		const bitset_type search = key.prefix;
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

	const_reference find_deep(key_type key, size_type depth = -1) const
	    noexcept(false) {
		if (empty()) {
			throw std::out_of_range("searched in an empty compact_bit_trie");
		}
		if (key.bits > bits_of<Key>) {
			throw std::invalid_argument("key prefix longer than key length");
		}
		size_type node = 1;
		size_type found = 0;
		const bitset_type search = key.prefix;
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

	reference find_deep(key_type key, size_type depth = -1) noexcept(false) {
		if (empty()) {
			throw std::out_of_range("searched in an empty compact_bit_trie");
		}
		if (key.bits > bits_of<Key>) {
			throw std::invalid_argument("key prefix longer than key length");
		}
		size_type node = 1;
		size_type found = 0;
		const bitset_type search = key.prefix;
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

	bool empty() const noexcept { return values.empty(); }

	template <typename... Ts>
	bool emplace(key_type key, Ts&&... args) {
		size_type node = get_storage_node_for(key);
		if (auto& v = storage[node].val; v != max) {
			return false;
		} else {
			values.emplace_back(std::forward<Ts>(args)...);
			v = values.size() - 1;
			return true;
		}
	}

	bool insert(key_type key, const value_type& value) {
		return emplace(key, value);
	}
	bool insert(key_type key, value_type&& value) {
		return emplace(key, std::move(value));
	}

	reference insert_or_assign(key_type key, const value_type& value) {
		size_type node = get_storage_node_for(key);
		auto& v = storage[node].val;
		if (v != -1) {
			values.push_back(value);
			v = values.size() - 1;
		} else {
			values[v] = value;
		}
		return values[v];
	}

	reference insert_or_assign(key_type key, value_type&& value) {
		size_type node = get_storage_node_for(key);
		auto& v = storage[node].val;
		if (v != -1) {
			values.push_back(std::move(value));
			v = values.size() - 1;
		} else {
			values[v] = std::move(value);
		}
		return values[v];
	}

	bool erase(key_type key);
	bool prune(key_type prefix);
	void clear() {
		storage.clear();
		values.clear();
	}

	size_type size() const noexcept { return values.size(); }

	std::size_t memory_use() const noexcept {
		return storage.capacity() * sizeof(inline_node) +
		       values.size() * sizeof(Value);
	}

	void shrink_to_fit() {
		storage.shrink_to_fit();
		values.shrink_to_fit();
	}

 private:
	struct inline_node {
		size_type children[2];
		size_type parent;
		size_type val;
	};
	std::vector<inline_node> storage;
	std::vector<Value> values;

	void do_init() {
		if (storage.size() < 2) {
			storage.resize(2);
			storage[0] = {};
			storage[1] = {};
		}
	}

	size_type get_storage_node_for(key_type key) {
		if (key.bits > bits_of<Key>) {
			throw std::invalid_argument("key prefix longer than key length");
		}
		const bitset_type search = key.prefix;
		size_type node = 1;
		do_init();
		for (int i : range(key.bits - 1)) {
			if (auto n = storage[node].children[search[i]]) {
				node = n;
			} else {
				storage.push_back({{0, 0}, node, max});
				auto& n_new = storage[node].children[search[i]];
				n_new = storage.size() - 1;
				node = n_new;
			}
		}
		return node;
	}

 public:
	template <typename V>
	class iterator_t {
	 public:
		using value_type = V;
		using pointer = V*;
		using reference = V&;
		using difference_type = compact_bit_trie::difference_type;
		using iterator_category = std::bidirectional_iterator_tag;

		iterator_t() = default;
		iterator_t(const compact_bit_trie& range)
		    : tree_ptr{&range.storage}, values_ptr{&range.values}, node{0} {}
		iterator_t(const iterator_t&) = default;
		iterator_t& operator=(const iterator_t&) = default;

		reference operator*() const noexcept {
			return (*values_ptr)[(*tree_ptr)[node].val];
		}
		pointer operator->() const noexcept {
			return std::addressof((*values_ptr)[(*tree_ptr)[node].val]);
		}
		iterator_t operator++() {}

	 private:
		iterator_t(const compact_bit_trie& range, size_type n_)
		    : iterator_t(range), node{n_} {}

		const std::vector<inline_node>* tree_ptr{};
		const std::vector<Value>* values_ptr{};
		size_type node{};
	};
};

inline void memswap(void* A, void* B, std::size_t size) {
	auto Ab = static_cast<unsigned char*>(A);
	auto Bb = static_cast<unsigned char*>(B);
	std::swap_ranges(Ab, Ab + size, Bb);
	return;
}

template <int offset, int size, typename Storage>
struct bitfield {
	Storage operator()() const noexcept {
		return (raw >> offset) & ((1 << size) - 1);
	}
	Storage operator()(Storage val) noexcept {
		// Clear the bits for this field
		raw &= ~(((1 << size) - 1) << offset);
		// Set the field
		raw |= (val & ((1 << size) - 1)) << offset;
		return val;
	}
	operator Storage() const noexcept { return (*this)(); }
	Storage operator=(Storage val) noexcept { return (*this)(val); }
	Storage raw;
};

namespace detail {

template <typename Parent, typename Ret, Ret (Parent::*Set)(Ret) noexcept,
          Ret (Parent::*Get)() const noexcept>
struct bitfield_proxy {
	Parent* p;
	constexpr Ret operator=(Ret val) noexcept { return (p->*Set)(val); }
	constexpr operator Ret() const noexcept { return (p->*Get)(); }
};

} // namespace detail

#define KBLIB_INTERNAL_BITFIELD_MACRO(offset, size, name, raw)                 \
 private:                                                                      \
	constexpr decltype(raw) name##_get_impl() const noexcept {                  \
	   return (raw >> offset) & ((1 << size) - 1);                              \
   }                                                                           \
	                                                                            \
 public:                                                                       \
	constexpr decltype(raw) name() const noexcept { return name##_get_impl(); } \
	                                                                            \
 private:                                                                      \
	constexpr decltype(raw) name##_set_impl(decltype(raw) val) noexcept {       \
	   /* Clear the bits for this field */                                      \
	   raw &= ~(((1 << size) - 1) << offset);                                   \
	   /* Set the field */                                                      \
	   raw |= (val & ((1 << size) - 1)) << offset;                              \
	   return val;                                                              \
   }                                                                           \
	                                                                            \
 public:                                                                       \
	constexpr decltype(raw) name(decltype(raw) val) noexcept {                  \
	   return name##_set_impl(val);                                             \
   }                                                                           \
	                                                                            \
	constexpr auto name() noexcept {                                            \
	   using Parent = std::remove_pointer<decltype(this)>::type;                \
	   return kblib::detail::bitfield_proxy<Parent, decltype(raw),              \
	                                        &Parent::name##_set_impl,           \
	                                        &Parent::name##_get_impl>{this};    \
   }

#ifdef KBLIB_DEF_MACROS
#define BITFIELD(offset, size, name, raw) \
	KBLIB_INTERNAL_BITFIELD_MACRO(offset, size, name, raw)
#endif

} // namespace kblib

#endif // KBLIB_BITS_H
