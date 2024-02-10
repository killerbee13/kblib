/* *****************************************************************************
 * kblib is a general utility library for C++14 and C++17, intended to provide
 * performant high-level abstractions and more expressive ways to do simple
 * things.
 *
 * Copyright (c) 2021 killerbee
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * ****************************************************************************/

/**
 * @file
 * @brief Provides direct_map.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef DIRECT_MAP_H
#define DIRECT_MAP_H

#include <kblib/fakestd.h>
#include <kblib/iterators.h>
#include <kblib/tdecl.h>

#include <array>
#include <bitset>
#include <cinttypes>
#include <climits>
#include <limits>
#include <new>
#include <optional>

namespace KBLIB_NS {

/**
 * @namespace detail_direct_map
 * @internal
 */
namespace detail_direct_map {

	template <typename T>
	KBLIB_CONSTANT auto range_of
	    = (std::numeric_limits<T>::digits + std::numeric_limits<T>::is_signed
	       < std::numeric_limits<std::uintmax_t>::digits)
	          ? static_cast<std::uintmax_t>(1)
	                << to_unsigned(std::numeric_limits<T>::digits
	                               + std::numeric_limits<T>::is_signed)
	          : 0;
	static_assert(range_of<unsigned char> == 1u << to_unsigned(CHAR_BIT), "");
	static_assert(range_of<signed char> == 1u << to_unsigned(CHAR_BIT), "");

	template <typename T, bool
	                      = std::is_trivially_default_constructible<T>::value and
	                          std::is_trivially_destructible<T>::value>
	struct alignas(T) storage_for : private std::array<byte, sizeof(T)> {
		template <
		    typename... Args,
		    enable_if_t<std::is_constructible<T, Args&&...>::value, int> = 0>
		constexpr auto construct(Args&&... args) noexcept(
		    std::is_nothrow_constructible<T, Args&&...>::value) -> T& {
			return *new (this->data()) T(std::forward<Args>(args)...);
		}
		storage_for() = default;
		storage_for(const storage_for&) = delete;
		storage_for(storage_for&&) = delete;

		auto operator=(const storage_for&) -> storage_for& = delete;
		auto operator=(storage_for&&) -> storage_for& = delete;

		~storage_for() = default;

		constexpr auto destroy() noexcept -> void { get()->~T(); }

#if __cpp_lib_launder
#	define LAUNDER(x) std::launder(x)
#else
#	define LAUNDER(x) (x)
#endif
		KBLIB_NODISCARD constexpr auto get() & noexcept -> T* {
			return LAUNDER(reinterpret_cast<T*>(this->data()));
		}
		KBLIB_NODISCARD constexpr auto get() const& noexcept -> const T* {
			return LAUNDER(reinterpret_cast<const T*>(this->data()));
		}
#undef LAUNDER
	};
	template <typename T>
	struct alignas(T) storage_for<T, true> {
	 private:
		T t;

	 public:
		template <
		    typename... Args,
		    enable_if_t<std::is_constructible<T, Args&&...>::value, int> = 0>
		constexpr auto construct(Args&&... args) noexcept(
		    std::is_nothrow_constructible<T, Args&&...>::value) -> T& {
			return *new (&t) T(std::forward<Args>(args)...);
		}

		constexpr auto destroy() noexcept -> void {}

		KBLIB_NODISCARD constexpr auto get() & noexcept -> T* { return &t; }
		KBLIB_NODISCARD constexpr auto get() const& noexcept -> const T* {
			return &t;
		}
	};

} // namespace detail_direct_map

template <typename Key, typename T, typename allocator = void>
class direct_map {
	// Allocating direct_map
 public:
	using key_type = Key;
	using mapped_type = T;
	using value_type = std::pair<const Key, T>;
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;

	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;

 private:
	constexpr static std::ptrdiff_t key_range{detail_direct_map::range_of<Key>};
	using storage_type = detail_direct_map::storage_for<value_type>;

	using held_type
	    = std::pair<std::bitset<key_range>, std::array<storage_type, key_range>>;

	template <typename V>
	class iter {
	 public:
		copy_const_t<V, held_type>* storage;
		std::ptrdiff_t pos;

		constexpr iter(decltype(storage) s, std::ptrdiff_t p)
		    : storage(s)
		    , pos(p) {
			if (not storage) {
				pos = max();
			}
		}
		constexpr iter()
		    : iter(nullptr, max()) {}

		using value_type = typename direct_map::value_type;
		using difference_type = typename direct_map::difference_type;
		using reference = copy_const_t<V, value_type>&;
		using pointer = copy_const_t<V, value_type>*;
		using iterator_category = std::bidirectional_iterator_tag;

		KBLIB_NODISCARD constexpr auto operator*() const noexcept -> reference {
			return *storage->second[uindex(pos)].get();
		}
		KBLIB_NODISCARD constexpr auto operator->() const noexcept -> pointer {
			return storage->second[uindex(pos)].get();
		}

		constexpr auto operator++() noexcept -> iter& {
			if (pos == max()) {
				// not required in general, but direct_map::iterator guarantees that
				// ++end() == end() because it simplifies the implementation and is
				// unlikely to be a significant performance impact
				return *this;
			}
			for (auto i : range(++pos, index(max()))) {
				if (storage->first.test(uindex(i))) {
					pos = i;
					return *this;
				}
			}
			pos = index(max());
			return *this;
		}
		constexpr auto operator++(int) noexcept -> iter {
			iter it = *this;
			++*this;
			return it;
		}

		constexpr auto operator--() noexcept -> iter& {
			for (auto i : range(pos - 1, index(min()), -1)) {
				if (storage->first.test(uindex(i))) {
					pos = i;
					return *this;
				}
			}
			pos = index(min());
			return *this;
		}
		constexpr auto operator--(int) noexcept -> iter {
			iter it = *this;
			--*this;
			return it;
		}

		KBLIB_NODISCARD friend constexpr auto operator==(iter l, iter r) noexcept
		    -> bool {
			return l.storage == r.storage and l.pos == r.pos;
		}
		KBLIB_NODISCARD friend constexpr auto operator!=(iter l, iter r) noexcept
		    -> bool {
			return not (l == r);
		}
#define DECL_OP(op)                                                           \
	KBLIB_NODISCARD friend constexpr auto operator op(iter l,                  \
	                                                  iter r) noexcept->bool { \
		assert(l.storage == r.storage);                                         \
		return l.pos op r.pos;                                                  \
	}
		DECL_OP(<)
		DECL_OP(>)
		DECL_OP(>=)
		DECL_OP(<=)
#undef DECL_OP

		constexpr auto swap(iter& other) noexcept -> void {
			kblib::swap(storage, other.storage);
			kblib::swap(pos, other.pos);
		}
	};

 public:
	using iterator = iter<value_type>;
	using const_iterator = iter<const value_type>;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	constexpr direct_map() noexcept = default;

	template <typename InputIt>
	constexpr direct_map(InputIt first, InputIt last)
	    : storage() {
		if (first != last) {
			allocate();
			for (auto v : indirect(first, last)) {
				construct(v.first, v.second);
			}
		}
	}
	// TODO(killerbee13): copy construction for allocating direct_map
	constexpr direct_map(const direct_map& other)
	    : storage()
	    , _size(other._size) {
		if (not other.empty()) {
			allocate();
			storage->first = other.storage->first;
			for (auto k : range(+min(), max() + 1)) {
				if (contains(k)) { // the bitmap is already copied from other
					do_construct(k, other.at(k));
				}
			}
		}
	}

	constexpr direct_map(direct_map&& other) noexcept
	    : storage(std::move(other.storage))
	    , _size(std::exchange(other._size, 0)) {}

	constexpr direct_map(std::initializer_list<value_type> init)
	    : direct_map(init.begin(), init.end()) {}

	/**
	 *
	 */
	KBLIB_CXX20(constexpr) ~direct_map() { clear(); }

	constexpr auto operator=(const direct_map& other) -> direct_map& {
		if (this == &other) {
			return *this;
		}
		clear();
		storage.assign(in_place_agg, other.storage->first);
		_size = other._size;
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, other.at(k));
				bitmap().set(index(k));
			}
		}
		return *this;
	}
	constexpr auto operator=(direct_map&& other) noexcept
	    -> direct_map& = default;
	constexpr auto operator=(std::initializer_list<value_type> init)
	    -> direct_map& {
		clear();
		for (auto it : init) {
			construct(it->first, it->second);
		}
		return *this;
	}

	KBLIB_NODISCARD constexpr auto at(Key key) & -> T& {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr auto at(Key key) && -> T&& {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr auto at(Key key) const& -> const T& {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr auto at(Key key) const&& -> const T&& {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}

	KBLIB_NODISCARD constexpr T& operator[](Key key) noexcept(
	    std::is_nothrow_default_constructible<T>::value) {
		return try_emplace(key).first->second;
	}

	KBLIB_NODISCARD constexpr auto begin() & noexcept -> iterator {
		return {storage.get(), cbegin().pos};
	}
	KBLIB_NODISCARD constexpr auto begin() const& noexcept -> const_iterator {
		return {storage.get(), cbegin().pos};
	}
	KBLIB_NODISCARD constexpr auto cbegin() const& noexcept -> const_iterator {
		if (not empty()) {
			if (contains(to_key(min()))) {
				return {storage.get(), min()};
			} else {
				return ++const_iterator{storage.get(), min()};
			}
		} else {
			return end();
		}
	}

	KBLIB_NODISCARD constexpr auto end() & noexcept -> iterator {
		return {storage.get(), max()};
	}
	KBLIB_NODISCARD constexpr auto end() const& noexcept -> const_iterator {
		return {storage.get(), max()};
	}
	KBLIB_NODISCARD constexpr auto cend() const& noexcept -> const_iterator {
		return {storage.get(), max()};
	}

	KBLIB_NODISCARD constexpr auto rbegin() & noexcept -> auto {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr auto rbegin() const& noexcept -> auto {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr auto crbegin() const& noexcept -> auto {
		return std::make_reverse_iterator(cend());
	}

	KBLIB_NODISCARD constexpr auto rend() & noexcept -> auto {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr auto rend() const& noexcept -> auto {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr auto crend() const& noexcept -> auto {
		return std::make_reverse_iterator(cbegin());
	}

	KBLIB_NODISCARD constexpr auto empty() const& noexcept -> bool {
		return _size == 0;
	}

	KBLIB_NODISCARD constexpr auto size() const& noexcept -> std::size_t {
		return _size;
	}
	KBLIB_NODISCARD constexpr auto ssize() const& noexcept -> std::ptrdiff_t {
		return _size;
	}

	KBLIB_NODISCARD constexpr static auto max_size() noexcept -> std::size_t {
		return key_range;
	}

	constexpr auto clear() noexcept -> void {
		if (_size == 0) {
			return;
		}
		for (auto i : range(+min(), max() + 1)) {
			auto j = static_cast<Key>(i);
			if (contains(j)) {
				unsafe_at(j).destroy();
			}
		}
		storage.reset();
		_size = 0;
	}

	constexpr auto insert(const value_type& value) -> std::pair<iterator, bool> {
		if (not contains(value.first)) {
			construct(value.first, value.second);
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}
	template <typename U>
	constexpr auto insert(U&& value)
	    -> enable_if_t<std::is_constructible<value_type, U&&>::value,
	                   std::pair<iterator, bool>> {
		if (not contains(value.first)) {
			construct(value.first, std::forward<U>(value.second));
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}
	constexpr auto insert(value_type&& value) -> std::pair<iterator, bool> {
		if (not contains(value.first)) {
			construct(value.first, std::move(value.second));
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}

	template <typename U>
	constexpr auto insert_or_assign(Key key, U&& value)
	    -> std::pair<iterator, bool> {
		if (not contains(key)) {
			construct(key, std::forward<U>(value));
			return {{storage.get(), index(key)}, true};
		} else {
			*unsafe_at(key).get() = std::forward<U>(value);
			return {{storage.get(), index(key)}, false};
		}
	}
	template <typename... Args>
	constexpr auto try_emplace(Key key, Args&&... args)
	    -> std::pair<iterator, bool> {
		if (not contains(key)) {
			construct(key, std::forward<Args>(args)...);
			return {{storage.get(), index(key)}, true};
		} else {
			return {{storage.get(), index(key)}, false};
		}
	}

	constexpr auto erase(iterator pos) noexcept -> iterator {
		assert(contains(to_key(pos.pos)));
		bitmap().reset(pos.pos);
		unsafe_at(to_key(pos.pos)).destroy();
		--_size;
		return ++pos;
	}
	constexpr auto erase(const_iterator pos) noexcept -> iterator {
		assert(contains(to_key(pos.pos)));
		bitmap().reset(pos.pos);
		unsafe_at(to_key(pos.pos)).destroy();
		--_size;
		return ++pos;
	}

	constexpr auto erase(iterator first, iterator last) noexcept -> iterator {
		for (auto i : range(first.pos, last.pos)) {
			if (contains(to_key(i))) {
				bitmap().reset(i);
				unsafe_at(to_key(i)).destroy();
				--_size;
			}
		}
		return ++last;
	}

	constexpr auto erase(Key key) noexcept -> std::size_t {
		if (contains(key)) {
			bitmap().reset(index(key));
			unsafe_at(key).destroy();
			--_size;
			return 1;
		} else {
			return 0;
		}
	}

	constexpr auto swap(direct_map& other) noexcept -> void {
		using std::swap;
		swap(storage, other.storage);
		swap(_size, other._size);
	}

	KBLIB_NODISCARD constexpr auto contains(Key key) const noexcept -> bool {
		return storage and bitmap().test(uindex(key));
	}
	KBLIB_NODISCARD constexpr auto count(Key key) const noexcept -> std::size_t {
		return contains(key);
	}

	KBLIB_NODISCARD constexpr auto find(Key key) & noexcept -> iterator {
		return contains(key) ? iterator{storage.get(), index(key)} : end();
	}
	KBLIB_NODISCARD constexpr auto find(Key key) const& noexcept
	    -> const_iterator {
		return contains(key) ? iterator{storage.get(), index(key)} : end();
	}

	KBLIB_NODISCARD constexpr auto equal_range(Key key) & noexcept
	    -> std::pair<iterator, iterator> {
		return {lower_bound(key), upper_bound(key)};
	}
	KBLIB_NODISCARD constexpr auto equal_range(Key key) const& noexcept
	    -> std::pair<const_iterator, const_iterator> {
		return {lower_bound(key), upper_bound(key)};
	}

	KBLIB_NODISCARD constexpr auto lower_bound(Key key) & noexcept -> iterator {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{storage.get(), index(key)};
		}
	}
	KBLIB_NODISCARD constexpr auto lower_bound(Key key) const& noexcept
	    -> const_iterator {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{storage.get(), index(key)};
		}
	}

	KBLIB_NODISCARD constexpr auto upper_bound(Key key) & noexcept -> iterator {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}
	KBLIB_NODISCARD constexpr auto upper_bound(Key key) const& noexcept
	    -> const_iterator {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}

	KBLIB_NODISCARD constexpr static auto min() noexcept -> Key {
		return std::numeric_limits<Key>::min();
	}
	KBLIB_NODISCARD constexpr static auto max() noexcept -> Key {
		return std::numeric_limits<Key>::max();
	}

	KBLIB_NODISCARD friend constexpr auto operator==(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>()
	                                           == std::declval<T&>())) -> bool {
		if (l.size() != r.size()) {
			return false;
		}
		for (auto i : kblib::range(+min(), max() + 1)) {
			if (l.contains(i) != r.contains(i)) {
				return false;
			} else if (l.contains(i)) {
				if (l.at(i) != r.at(i)) {
					return false;
				}
			}
		}
		return true;
	}

	KBLIB_NODISCARD friend constexpr auto operator!=(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>()
	                                           == std::declval<T&>())) -> bool {
		return not (l == r);
	}

	KBLIB_NODISCARD friend constexpr auto operator<(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return kblib::lexicographical_compare(l.begin(), l.end(), r.begin(),
		                                      r.end());
	}
	KBLIB_NODISCARD friend constexpr auto operator>(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return r < l;
	}
	KBLIB_NODISCARD friend constexpr auto operator<=(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return not (r < l);
	}
	KBLIB_NODISCARD friend constexpr auto operator>=(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return not (l < r);
	}

	KBLIB_NODISCARD constexpr static auto index(Key key) noexcept
	    -> std::ptrdiff_t {
		return to_signed(key);
	}
	KBLIB_NODISCARD constexpr static auto uindex(Key key) noexcept
	    -> std::size_t {
		return to_unsigned(key);
	}
	KBLIB_NODISCARD constexpr static auto to_key(std::ptrdiff_t i) noexcept
	    -> Key {
		return Key(i);
	}

 private:
	KBLIB_NODISCARD constexpr auto bitmap() noexcept -> std::bitset<key_range>& {
		return storage->first;
	}
	KBLIB_NODISCARD constexpr const std::bitset<key_range>& bitmap()
	    const noexcept {
		return storage->first;
	}

	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) & -> storage_type& {
		return storage->second[uindex(key)];
	}
	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) && -> storage_type&& {
		return storage->second[uindex(key)];
	}
	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) const& -> const
	    storage_type& {
		return storage->second[uindex(key)];
	}
	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) const&& -> const
	    storage_type&& {
		return storage->second[uindex(key)];
	}

	auto allocate() -> void {
		if (not storage) {
			storage.assign();
		}
	}

	template <typename... Args>
	constexpr void construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		allocate();
		if (not storage->first.test(uindex(key))) {
			do_construct(key, std::forward<Args>(args)...);
			// doing these after construction maintains exception safety.
			storage->first.set(uindex(key));
			++_size;
		}
	}

	template <typename... Args>
	constexpr void do_construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		storage->second[uindex(key)].construct(
		    std::piecewise_construct, std::forward_as_tuple(key),
		    std::forward_as_tuple(std::forward<Args>(args)...));
	}

	// TODO(killerbee13): Implement, test, and document direct_map
	// TODO(killerbee13): allocator support for direct_map
	kblib::heap_value<held_type> storage;
	std::size_t _size{};
};

template <typename Key, typename T>
class direct_map<Key, T, void> {
	// Non-allocating direct_map
 public:
	using key_type = Key;
	using mapped_type = T;
	using value_type = std::pair<const Key, T>;
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;

	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;

 private:
	constexpr static std::ptrdiff_t key_range{detail_direct_map::range_of<Key>};
	using storage_type = detail_direct_map::storage_for<value_type>;

	template <typename V>
	class iter {
	 public:
		copy_const_t<V, direct_map>* map;
		std::ptrdiff_t pos;

		constexpr iter(decltype(map) s, std::ptrdiff_t p)
		    : map(s)
		    , pos(p) {
			if (not map) {
				pos = max();
			}
		}
		constexpr iter()
		    : iter(nullptr, max()) {}

		using value_type = typename direct_map::value_type;
		using difference_type = typename direct_map::difference_type;
		using reference = copy_const_t<V, value_type>&;
		using pointer = copy_const_t<V, value_type>*;
		using iterator_category = std::bidirectional_iterator_tag;

		KBLIB_NODISCARD constexpr auto operator*() const -> reference {
			return *map->elems[uindex(pos)].get();
		}
		KBLIB_NODISCARD constexpr auto operator->() const -> pointer {
			return map->elems[uindex(pos)].get();
		}

		constexpr auto operator++() -> iter& {
			if (pos == max()) {
				// not required in general, but direct_map::iterator guarantees that
				// ++end() == end() because it simplifies the implementation and is
				// unlikely to be a significant performance impact
				return *this;
			}
			for (auto i : range(++pos, index(max()))) {
				if (map->active_elems.test(uindex(i))) {
					pos = i;
					return *this;
				}
			}
			pos = index(max());
			return *this;
		}
		constexpr auto operator++(int) -> iter {
			iter it = *this;
			++*this;
			return it;
		}

		constexpr auto operator--() -> iter& {
			for (auto i : range(pos - 1, index(min()), -1)) {
				if (map->active_elems.test(uindex(i))) {
					pos = i;
					return *this;
				}
			}
			pos = index(min());
			return *this;
		}
		constexpr auto operator--(int) -> iter {
			iter it = *this;
			--*this;
			return it;
		}

		KBLIB_NODISCARD friend constexpr auto operator==(iter l, iter r) noexcept
		    -> bool {
			return l.map == r.map and l.pos == r.pos;
		}
		KBLIB_NODISCARD friend constexpr auto operator!=(iter l, iter r) noexcept
		    -> bool {
			return not (l == r);
		}

#define DECL_OP(op)                                                           \
	KBLIB_NODISCARD friend constexpr auto operator op(iter l,                  \
	                                                  iter r) noexcept->bool { \
		assert(l.map == r.map);                                                 \
		return l.pos op r.pos;                                                  \
	}
		DECL_OP(<)
		DECL_OP(>)
		DECL_OP(>=)
		DECL_OP(<=)
#undef DECL_OP

		constexpr auto swap(iter& other) noexcept -> void {
			kblib::swap(map, other.map);
			kblib::swap(pos, other.pos);
		}
	};

 public:
	using iterator = iter<value_type>;
	using const_iterator = iter<const value_type>;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	constexpr direct_map() noexcept = default;

	template <typename InputIt>
	constexpr direct_map(InputIt first, InputIt last) {
		for (auto&& v : indirect(first, last)) {
			construct(std::forward<decltype(v)>(v).first,
			          std::forward<decltype(v)>(v).second);
		}
	}

	constexpr direct_map(const direct_map& other)
	    : active_elems{other.active_elems}
	    , _size(other._size) {
		for (const key_type k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, other.unsafe_at(k).get()->second);
				bitmap().set(uindex(k));
			}
		}
	}

	constexpr direct_map(direct_map&& other) noexcept(
	    std::is_nothrow_move_constructible<value_type>::value)
	    : active_elems{other.active_elems}
	    , _size(other._size) {
		for (const key_type k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, std::move(other.unsafe_at(k).get()->second));
				bitmap().set(uindex(k));
			}
		}
	}

	constexpr direct_map(std::initializer_list<value_type> init)
	    : direct_map(init.begin(), init.end()) {}

	// TODO(killerbee13): Remove this and allow it to be trivially destructible
	// for trivial Key and T
	KBLIB_CXX20(constexpr) ~direct_map() { clear(); }

	constexpr auto operator=(const direct_map& other) -> direct_map& {
		if (this == &other) {
			return *this;
		}
		clear();
		active_elems = other.active_elems;
		_size = other._size;
		for (const key_type k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, other.unsafe_at(k).get()->second);
				bitmap().set(uindex(k));
			}
		}
		return *this;
	}
	constexpr auto operator=(direct_map&& other) noexcept(
	    std::is_nothrow_move_constructible<T>::value) -> direct_map& {
		if (this == &other) {
			return *this;
		}
		active_elems = other.active_elems;
		_size = other._size;
		for (const key_type k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, std::move(other.unsafe_at(k).get()->second));
				bitmap().set(uindex(k));
			}
		}
		return *this;
	}
	constexpr auto operator=(std::initializer_list<value_type> init)
	    -> direct_map& {
		clear();
		for (auto it : init) {
			construct(it->first, it->second);
		}
		return *this;
	}

	KBLIB_NODISCARD constexpr auto at(Key key) & -> T& {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr auto at(Key key) && -> T&& {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr auto at(Key key) const& -> const T& {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr auto at(Key key) const&& -> const T&& {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}

	KBLIB_NODISCARD constexpr T& operator[](Key key) noexcept(
	    std::is_nothrow_default_constructible<T>::value) {
		return try_emplace(key).first->second;
	}

	KBLIB_NODISCARD constexpr auto begin() & noexcept -> iterator {
		return {this, cbegin().pos};
	}
	KBLIB_NODISCARD constexpr auto begin() const& noexcept -> const_iterator {
		return {this, cbegin().pos};
	}
	KBLIB_NODISCARD constexpr auto cbegin() const& noexcept -> const_iterator {
		if (not empty()) {
			if (contains(to_key(min()))) {
				return {this, min()};
			} else {
				return ++const_iterator{this, min()};
			}
		} else {
			return end();
		}
	}

	KBLIB_NODISCARD constexpr auto end() & noexcept -> iterator {
		return {this, max()};
	}
	KBLIB_NODISCARD constexpr auto end() const& noexcept -> const_iterator {
		return {this, max()};
	}
	KBLIB_NODISCARD constexpr auto cend() const& noexcept -> const_iterator {
		return {this, max()};
	}

	KBLIB_NODISCARD constexpr auto rbegin() & noexcept -> reverse_iterator {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr auto rbegin() const& noexcept
	    -> const_reverse_iterator {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr auto crbegin() const& noexcept
	    -> const_reverse_iterator {
		return std::make_reverse_iterator(crend());
	}

	KBLIB_NODISCARD constexpr auto rend() & noexcept -> reverse_iterator {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr auto rend() const& noexcept
	    -> const_reverse_iterator {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr auto crend() const& noexcept
	    -> const_reverse_iterator {
		return std::make_reverse_iterator(cbegin());
	}

	KBLIB_NODISCARD constexpr auto empty() const& noexcept -> bool {
		return _size == 0;
	}

	KBLIB_NODISCARD constexpr auto size() const& noexcept -> std::size_t {
		return _size;
	}
	KBLIB_NODISCARD constexpr auto ssize() const& noexcept -> std::ptrdiff_t {
		return _size;
	}

	KBLIB_NODISCARD constexpr static auto max_size() noexcept -> std::size_t {
		return key_range;
	}

	constexpr auto clear() noexcept -> void {
		for (auto i : range(+min(), max() + 1)) {
			auto j = static_cast<Key>(i);
			if (contains(j)) {
				unsafe_at(j).destroy();
			}
		}
		_size = 0;
	}

	constexpr auto insert(const value_type& value) -> std::pair<iterator, bool> {
		if (not contains(value.first)) {
			construct(value.first, value.second);
			return {{this, index(value.first)}, true};
		} else {
			return {{this, index(value.first)}, false};
		}
	}
	template <typename U>
	constexpr auto insert(U&& value)
	    -> return_assert_t<std::is_constructible<value_type, U&&>::value,
	                       std::pair<iterator, bool>> {
		if (not contains(value.first)) {
			construct(value.first, std::forward<U>(value.second));
			return {{this, index(value.first)}, true};
		} else {
			return {{this, index(value.first)}, false};
		}
	}
	constexpr auto insert(value_type&& value) -> std::pair<iterator, bool> {
		if (not contains(value.first)) {
			construct(value.first, std::move(value.second));
			return {{this, index(value.first)}, true};
		} else {
			return {{this, index(value.first)}, false};
		}
	}

	template <typename U>
	constexpr auto insert_or_assign(Key key, U&& value)
	    -> std::pair<iterator, bool> {
		if (not contains(key)) {
			construct(key, std::forward<U>(value));
			return {{this, index(key)}, true};
		} else {
			*unsafe_at(key).get() = std::forward<U>(value);
			return {{this, index(key)}, false};
		}
	}
	template <typename... Args>
	constexpr auto try_emplace(Key key, Args&&... args)
	    -> std::pair<iterator, bool> {
		if (not contains(key)) {
			construct(key, std::forward<Args>(args)...);
			return {{this, index(key)}, true};
		} else {
			return {{this, index(key)}, false};
		}
	}

	constexpr auto erase(iterator pos) noexcept -> iterator {
		assert(contains(to_key(pos.pos)));
		destroy(to_key(pos.pos));
		return ++pos;
	}
	constexpr auto erase(const_iterator pos) noexcept -> iterator {
		assert(contains(to_key(pos.pos)));
		destroy(to_key(pos.pos));
		return ++pos;
	}

	constexpr auto erase(iterator first, iterator last) noexcept -> iterator {
		for (auto i : range(first.pos, last.pos)) {
			if (contains(to_key(i))) {
				destroy(to_key(i));
			}
		}
		return ++last;
	}

	constexpr auto erase(Key key) noexcept -> std::size_t {
		if (contains(key)) {
			destroy(key);
			return 1;
		} else {
			return 0;
		}
	}

	constexpr auto swap(direct_map& other) noexcept(
	    std::is_nothrow_move_constructible<value_type>::value and
	        fakestd::is_nothrow_swappable<T>::value) -> void {
		using std::swap;
		for (const key_type k : range(+min(), max() + 1)) {
			if (contains(k)) {
				if (other.contains(k)) {
					kblib::swap(unsafe_at(k).get()->second,
					            other.unsafe_at(k).get()->second);
				} else {
					other.construct(k, std::move(unsafe_at(k).get()->second));
					destroy(k);
				}
			} else if (other.contains(k)) {
				construct(k, std::move(other.unsafe_at(k).get()->second));
				other.destroy(k);
			} else {
				// do nothing
			}
		}

		swap(active_elems, other.active_elems);
		swap(_size, other._size);
	}

	KBLIB_NODISCARD constexpr auto contains(Key key) const noexcept -> bool {
		return bitmap().test(uindex(key));
	}
	KBLIB_NODISCARD constexpr auto count(Key key) const noexcept -> std::size_t {
		return contains(key);
	}

	KBLIB_NODISCARD constexpr auto find(Key key) & noexcept -> iterator {
		return contains(key) ? iterator{this, index(key)} : end();
	}
	KBLIB_NODISCARD constexpr auto find(Key key) const& noexcept
	    -> const_iterator {
		return contains(key) ? iterator{this, index(key)} : end();
	}

	KBLIB_NODISCARD constexpr auto equal_range(Key key) & noexcept
	    -> std::pair<iterator, iterator> {
		return {lower_bound(key), upper_bound(key)};
	}
	KBLIB_NODISCARD constexpr auto equal_range(Key key) const& noexcept
	    -> std::pair<const_iterator, const_iterator> {
		return {lower_bound(key), upper_bound(key)};
	}

	KBLIB_NODISCARD constexpr auto lower_bound(Key key) & noexcept -> iterator {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{this, index(key)};
		}
	}
	KBLIB_NODISCARD constexpr auto lower_bound(Key key) const& noexcept
	    -> const_iterator {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{this, index(key)};
		}
	}

	KBLIB_NODISCARD constexpr auto upper_bound(Key key) & noexcept -> iterator {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}
	KBLIB_NODISCARD constexpr auto upper_bound(Key key) const& noexcept
	    -> const_iterator {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}

	KBLIB_NODISCARD constexpr static auto min() noexcept -> Key {
		return std::numeric_limits<Key>::min();
	}
	KBLIB_NODISCARD constexpr static auto max() noexcept -> Key {
		return std::numeric_limits<Key>::max();
	}

	KBLIB_NODISCARD friend constexpr auto operator==(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>()
	                                           == std::declval<T&>())) -> bool {
		if (l.size() != r.size()) {
			return false;
		}
		for (const key_type i : kblib::range(+min(), max() + 1)) {
			if (l.contains(i) != r.contains(i)) {
				return false;
			} else if (l.contains(i)) {
				if (l.at(i) != r.at(i)) {
					return false;
				}
			}
		}
		return true;
	}

	KBLIB_NODISCARD friend constexpr auto operator!=(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>()
	                                           == std::declval<T&>())) -> bool {
		return not (l == r);
	}

	KBLIB_NODISCARD friend constexpr auto operator<(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return kblib::lexicographical_compare(l.begin(), l.end(), r.begin(),
		                                      r.end());
	}
	KBLIB_NODISCARD friend constexpr auto operator>(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return r < l;
	}
	KBLIB_NODISCARD friend constexpr auto operator<=(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return not (r < l);
	}
	KBLIB_NODISCARD friend constexpr auto operator>=(
	    const direct_map& l,
	    const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                           std::declval<T&>())) -> bool {
		return not (l < r);
	}

	KBLIB_NODISCARD constexpr static auto index(Key key) noexcept
	    -> std::ptrdiff_t {
		return to_signed(key);
	}
	KBLIB_NODISCARD constexpr static auto uindex(Key key) noexcept
	    -> std::size_t {
		return to_unsigned(key);
	}
	KBLIB_NODISCARD constexpr static auto to_key(std::ptrdiff_t i) noexcept
	    -> Key {
		return Key(i);
	}

 private:
	KBLIB_NODISCARD constexpr auto bitmap() noexcept -> std::bitset<key_range>& {
		return active_elems;
	}
	KBLIB_NODISCARD constexpr auto bitmap() const noexcept
	    -> const std::bitset<key_range>& {
		return active_elems;
	}

	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) & -> storage_type& {
		return elems[uindex(key)];
	}
	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) && -> storage_type&& {
		return elems[uindex(key)];
	}
	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) const& -> const
	    storage_type& {
		return elems[uindex(key)];
	}
	KBLIB_NODISCARD constexpr auto unsafe_at(Key key) const&& -> const
	    storage_type&& {
		return elems[uindex(key)];
	}

	template <typename... Args>
	constexpr auto construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) -> void {
		if (not active_elems.test(uindex(key))) {
			do_construct(key, std::forward<Args>(args)...);
			// doing these after construction maintains exception safety.
			active_elems.set(uindex(key));
			++_size;
		}
	}

	template <typename... Args>
	constexpr auto do_construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) -> void {
		elems[uindex(key)].construct(
		    std::piecewise_construct, std::forward_as_tuple(key),
		    std::forward_as_tuple(std::forward<Args>(args)...));
	}

	auto destroy(Key key) -> void {
		assert(contains(key));

		bitmap().reset(uindex(key));
		unsafe_at(key).destroy();
		--_size;
	}

	// TODO(killerbee13): Implement, test, and document direct_map

	std::bitset<key_range> active_elems;
	std::array<storage_type, key_range> elems;

	std::size_t _size{};
};

} // namespace KBLIB_NS

#endif // DIRECT_MAP_H
