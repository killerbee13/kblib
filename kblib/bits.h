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

#if KBLIB_USE_CXX17

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
			// This variable must exist for exception safety. exists should not be
			// set to true if an exception is thrown.
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
		while (node and idx < key.bits) {
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
		while (node and idx < key.bits) {
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
		while (node and idx < key.bits) {
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
		while (node and idx < key.bits) {
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
		    : iterator_t(range) {
			node = n_;
		}

		const std::vector<inline_node>* tree_ptr{};
		const std::vector<Value>* values_ptr{};
		size_type node{};
	};
};

/**
 * @brief Swaps memory ranges.
 *
 * @pre A and B must not be null.
 * @pre *A and *B must not overlap.
 *
 * @param A A pointer to memory to swap with *B.
 * @param B A pointer to memory to swap with *A.
 * @param size The number of bytes to swap between *A and *B.
 */
inline void memswap(void* A, void* B, std::size_t size) {
	auto Ab = static_cast<unsigned char*>(A);
	auto Bb = static_cast<unsigned char*>(B);
	std::swap_ranges(Ab, Ab + size, Bb);
	return;
}

#endif

/**
 * @brief Implements a bitfield abstraction. May be used in a union with other
 * bitfields.
 *
 * In C++20, [[no_unique_address]] will enable a better implementation which
 * will work in non-union structs, as long as no two bitfields name the same
 * exact bits. ([[no_unique_address]] allows empty objects of different types to
 * be allocated at the same location, but distinct objects of the same type must
 * have distinct addresses, [[no_unique_address]] notwithstanding. This does
 * not apply to unions, though.)
 *
 * @tparam offset The number of bits less significant than the bitfield.
 * @tparam size The number of bits constituting this bitfield.
 * @tparam Storage The underlying type which stores the bits.
 */
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
	// Is this a good idea?
	// void operator&() = delete;
};

namespace detail {

	/**
	 * @brief A proxy reference type for BITFIELD-declared bitfields.
	 *
	 * It may be assigned to, or it may be used as a prvalue of type ReturnT.
	 * Unlike most proxy references, this is actually not that dissimilar to a
	 * language bitfield, which has only those capabilities. Like all other proxy
	 * references, it should not generally be bound to an auto variable.
	 */
	template <typename Parent, typename ReturnT,
	          ReturnT (Parent::*Set)(ReturnT) noexcept,
	          ReturnT (Parent::*Get)() const noexcept>
	struct bitfield_proxy {
		Parent* p;
		constexpr ReturnT operator=(ReturnT val) noexcept {
			return (p->*Set)(val);
		}
		constexpr operator ReturnT() const noexcept { return (p->*Get)(); }
	};

} // namespace detail

/**
 * @def KBLIB_INTERNAL_BITFIELD_MACRO(offset, size, name, raw)
 * @sa See #BITFIELD(offset, size, name, raw) for documentation.
 * @note This macro is defined unconditionally.
 */
#define KBLIB_INTERNAL_BITFIELD_MACRO(offset, size, name, raw)                 \
 private:                                                                      \
	constexpr decltype(raw) name##_get_impl() const noexcept {                  \
		return (raw >> offset) & ((decltype(raw)(1) << size) - 1);               \
	}                                                                           \
                                                                               \
 public:                                                                       \
	constexpr decltype(raw) name() const noexcept { return name##_get_impl(); } \
                                                                               \
 private:                                                                      \
	constexpr decltype(raw) name##_set_impl(decltype(raw) val) noexcept {       \
		/* Clear the bits for this field */                                      \
		raw &= ~(((decltype(raw)(1) << size) - 1) << offset);                    \
		/* Set the field */                                                      \
		raw |= (val & ((decltype(raw)(1) << size) - 1)) << offset;               \
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
	}                                                                           \
                                                                               \
	template <decltype(raw) val, decltype(raw) basis = 0>                       \
	constexpr static decltype(raw) set_##name##_v =                             \
	    (basis & ~(((decltype(raw)(1) << size) - 1) << offset)) |               \
	    (val & ((decltype(raw)(1) << size) - 1)) << offset;                     \
                                                                               \
	template <decltype(raw) basis>                                              \
	constexpr static decltype(raw) get_##name##_v =                             \
	    (basis >> offset) & ((decltype(raw)(1) << size) - 1);

namespace detail {

	template <typename Type, typename Storage>
	struct pun_proxy {
		Storage& base;
		pun_proxy& operator=(Type val) noexcept {
			std::memcpy(&base, &val, sizeof val);
			return *this;
		}
		operator Type() const noexcept {
			Type ret;
			std::memcpy(&ret, &base, sizeof ret);
			return ret;
		}
	};

	template <typename Type, typename Storage>
	struct array_pun_proxy {
		Storage& base;
		Type data;
		bool dirty = false;
		operator Type&() noexcept {
			if (dirty) {
				std::memcpy(&base, &data, sizeof data);
			}
			dirty = true;
			return data;
		}
		operator const Type&() const noexcept {
			if (dirty) {
				std::memcpy(&base, &data, sizeof data);
			}
			return data;
		}
		~array_pun_proxy() {
			if (dirty) {
				std::memcpy(&base, &data, sizeof data);
			}
		}
	};

	template <typename T>
	struct array_filter {
		using type = T;
	};
	template <typename T, std::size_t N>
	struct array_filter<T[N]> {
		using type = std::array<T, N>;
	};
	template <typename T>
	struct array_filter<T[]> {
		using type = std::array<T, 0>;
	};

	template <typename T, std::size_t S>
	struct array_filter2 {
		using type = T;
	};
	template <typename T, std::size_t N, std::size_t S>
	struct array_filter2<T[N], S> {
		using type = std::array<T, N>;
	};
	template <typename T, std::size_t S>
	struct array_filter2<T[], S> {
		using type = std::array<T, S / sizeof(T)>;
	};

	template <typename P, typename Type, std::size_t S, std::size_t,
	          bool aliases =
	              is_aliasing_type<typename std::remove_extent<Type>::type>>
	struct pun_el {

		using type = typename array_filter2<Type, S>::type;

		static_assert(std::is_trivially_copyable<type>::value,
		              "Type must be trivially copyable");

		auto get() {
			return pun_proxy<type, decltype(P::raw)>{static_cast<P*>(this)->raw};
		}
		auto get() const {
			return pun_proxy<const type, const decltype(P::raw)>{
			    static_cast<const P*>(this)->raw};
		}
	};

	template <typename P, typename Type, std::size_t S, std::size_t I>
	struct pun_el<P, Type[S], S, I, true> {
		using type = Type[S];

		decltype(auto) get() {
			return reinterpret_cast<type&>(static_cast<P*>(this)->raw);
		}
		decltype(auto) get() const {
			return reinterpret_cast<const type&>(static_cast<const P*>(this)->raw);
		}
	};

	template <typename P, typename Type, std::size_t S, std::size_t I>
	struct pun_el<P, Type[], S, I, true> {
		using type = Type[S];

		decltype(auto) get() {
			return reinterpret_cast<type&>(static_cast<P*>(this)->raw);
		}
		decltype(auto) get() const {
			return reinterpret_cast<const type&>(static_cast<const P*>(this)->raw);
		}
	};

	template <std::size_t S, typename I_S, typename... Types>
	struct punner_impl;

	template <std::size_t S, std::size_t... Is, typename... Types>
	struct punner_impl<S, std::index_sequence<Is...>, Types...>
	    : pun_el<punner_impl<S, std::index_sequence<Is...>, Types...>, Types, S,
	             Is>... {

		alignas(std::max(
		    {alignof(typename array_filter<Types>::type)...})) std::byte raw[S];
	};

	template <typename... Types>
	constexpr std::size_t
	    max_size = std::max({sizeof(typename array_filter<Types>::type)...});

} // namespace detail

template <typename... Types>
struct punner
    : private detail::punner_impl<detail::max_size<Types...>,
                                  std::index_sequence_for<Types...>, Types...> {
 private:
	constexpr static std::size_t storage_size = detail::max_size<Types...>;
	using impl_t =
	    detail::punner_impl<storage_size, std::index_sequence_for<Types...>,
	                        Types...>;
	using tuple_t = std::tuple<Types...>;
	template <std::size_t I>
	using r_element_t = typename std::tuple_element<I, tuple_t>::type;

	static_assert(std::is_standard_layout<impl_t>::value);

 public:
	template <std::size_t I>
	using base_t = detail::pun_el<impl_t, r_element_t<I>, storage_size, I>;
	template <std::size_t I>
	using element_t = typename base_t<I>::type;

	template <std::size_t I>
	decltype(auto) get() & {
		static_assert(std::is_base_of<base_t<I>, impl_t>::value);
		return static_cast<base_t<I>&>(*this).get();
	}
	template <std::size_t I>
	decltype(auto) get() const& {
		return static_cast<const base_t<I>&>(*this).get();
	}
	template <std::size_t I>
	decltype(auto) get() && {
		return static_cast<base_t<I>&&>(*this).get();
	}
	template <std::size_t I>
	decltype(auto) get() const&& {
		return static_cast<const base_t<I>&&>(*this).get();
	}
};

} // namespace kblib

namespace std {

template <std::size_t I, typename... Types>
struct tuple_element<I, kblib::punner<Types...>> {
	using type = typename kblib::punner<Types...>::template element_t<I>;
};

template <typename... Types>
struct tuple_size<kblib::punner<Types...>>
    : public std::integral_constant<std::size_t, sizeof...(Types)> {};

} // namespace std

namespace kblib {

template <std::size_t I, typename... Types>
decltype(auto) get(punner<Types...>& p) {
	return p.template get<I>();
}
template <std::size_t I, typename... Types>
decltype(auto) get(const punner<Types...>& p) {
	return p.template get<I>();
}
template <std::size_t I, typename... Types>
decltype(auto) get(punner<Types...>&& p) {
	return p.template get<I>();
}
template <std::size_t I, typename... Types>
decltype(auto) get(const punner<Types...>&& p) {
	return p.template get<I>();
}

template <typename Type, auto Storage>
class union_pun {
 private:
	using class_t = kblib::class_t<Storage>;
	using member_t = kblib::member_t<class_t, Storage>;
	using proxy_t = detail::pun_proxy<Type, member_t>;
	using const_proxy_t = detail::pun_proxy<Type, const member_t>;

	static_assert(sizeof(Type) <= sizeof(member_t),
	              "Type will not fit in the provided storage.");
	static_assert(std::is_trivially_copyable_v<Type>,
	              "Type must be trivially copyable.");
	static_assert(
	    std::is_trivially_copyable_v<std::remove_all_extents_t<member_t>>,
	    "Storage type must be trivially copyable.");

	member_t& base() noexcept {
		return reinterpret_cast<class_t*>(this)->*Storage;
	}
	const member_t& base() const noexcept {
		return reinterpret_cast<const class_t*>(this)->*Storage;
	}

 public:
	const_proxy_t operator()() const noexcept { return {base()}; }
	proxy_t operator()(Type val) noexcept {
		std::memcpy(&base(), &val, sizeof val);
		return {base()};
	}
	operator Type() const noexcept { return (*this)(); }
	proxy_t operator=(Type val) noexcept { return (*this)(val); }
};

template <typename Type, std::size_t N, auto Storage>
class union_pun<Type[N], Storage> {
 private:
	using class_t = kblib::class_t<Storage>;
	using member_t = kblib::member_t<class_t, Storage>;
	using type = std::array<Type, N>;
	using proxy_t = detail::pun_proxy<type, member_t>;
	using const_proxy_t = detail::pun_proxy<type, const member_t>;

	static_assert(sizeof(type) <= sizeof(member_t),
	              "Type will not fit in the provided storage.");
	static_assert(std::is_trivially_copyable_v<type>,
	              "Type must be trivially copyable.");
	static_assert(
	    std::is_trivially_copyable_v<std::remove_all_extents_t<member_t>>,
	    "Storage type must be trivially copyable.");

	member_t& base() noexcept {
		return reinterpret_cast<class_t*>(this)->*Storage;
	}
	const member_t& base() const noexcept {
		return reinterpret_cast<const class_t*>(this)->*Storage;
	}

 public:
	const_proxy_t operator()() const noexcept { return {base()}; }
	proxy_t operator()(const Type (&val)[N]) noexcept {
		std::memcpy(&base(), &val, sizeof val);
		return {base()};
	}
	operator type() const noexcept { return (*this)(); }
	proxy_t operator=(const Type (&val)[N]) noexcept { return (*this)(val); }
};

} // namespace kblib

#endif // KBLIB_BITS_H

#if KBLIB_DEF_MACROS and not defined(BITFIELD)
/**
 * @def BITFIELD(offset, size, name, raw)
 * Defines appropriate member functions which operate on a bitfield. The
 * generated functions are constexpr and optimize well.
 *
 * Declare inside a struct to add a simulated bitfield to it. In total, 5 member
 * functions will be defined, three of which are public. (The private two are
 * required only to get around overload resolution problems with the proxy
 * reference type.)
 *
 * One is a const accessor, which returns the value of the field. One is a
 * setter, which takes a new value for the field and assigns it, then returns
 * the new value (after truncating it to the field width). The last function is
 * the non-const accessor, which takes no argument and returns a proxy reference
 * to the bitfield, which may be assigned to or implicitly converted to the
 * value type.
 *
 * @note This macro is only defined if KBLIB_DEF_MACROS is true.
 * @note This macro always declares the member functions publically.
 *
 * @sa See #KBLIB_INTERNAL_BITFIELD_MACRO for definition.
 *
 * @param offset The number of bits less significant than this bitfield.
 * @param size The number of bits in this bitfield.
 * @param name The name of the generated member functions which operate on this
 * bitfield.
 * @param raw The name of the member variable in which the bitfield is stored.
 */
#define BITFIELD(offset, size, name, raw) \
	KBLIB_INTERNAL_BITFIELD_MACRO(offset, size, name, raw)
#endif // KBLIB_DEF_MACROS and not defined(BITFIELD)
