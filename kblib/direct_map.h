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

namespace kblib {

namespace detail {

	template <typename T>
	constexpr std::uintmax_t
	    range_of = static_cast<std::uintmax_t>(std::numeric_limits<T>::max()) -
	               std::numeric_limits<T>::min() + 1;
	static_assert(range_of<unsigned char> == 1 << CHAR_BIT, "");

	template <typename T,
	          bool = std::is_trivially_default_constructible<T>::value and
	              std::is_trivially_destructible<T>::value>
	struct alignas(T) storage_for : private std::array<std::byte, sizeof(T)> {
		template <
		    typename... Args,
		    enable_if_t<std::is_constructible<T, Args&&...>::value, int> = 0>
		constexpr T& construct(Args&&... args) noexcept(
		    std::is_nothrow_constructible<T, Args&&...>::value) {
			return *new (this->data()) T(std::forward<Args>(args)...);
		}
		storage_for() = default;
		storage_for(const storage_for&) = delete;
		storage_for(storage_for&&) = delete;

		constexpr void destroy() noexcept { get()->~T(); }

#if KBLIB_USE_CXX17
#define LAUNDER(x) std::launder(x)
#else
#define LAUNDER(x) (x)
#endif
		KBLIB_NODISCARD constexpr T* get() & noexcept {
			return LAUNDER(reinterpret_cast<T*>(this->data()));
		}
		KBLIB_NODISCARD constexpr const T* get() const& noexcept {
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
		constexpr T& construct(Args&&... args) noexcept(
		    std::is_nothrow_constructible<T, Args&&...>::value) {
			return *new (&t) T(std::forward<Args>(args)...);
		}

		constexpr void destroy() noexcept {}

		KBLIB_NODISCARD constexpr T* get() & noexcept { return &t; }
		KBLIB_NODISCARD constexpr const T* get() const& noexcept { return &t; }
	};

} // namespace detail

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
	constexpr static std::ptrdiff_t key_range{detail::range_of<Key>};
	using storage_type = detail::storage_for<value_type>;

	using held_type =
	    std::pair<std::bitset<key_range>, std::array<storage_type, key_range>>;

	template <typename V>
	class iter {
	 public:
		copy_const_t<V, held_type>* storage;
		std::ptrdiff_t pos;

		using value_type = typename direct_map::value_type;
		using difference_type = typename direct_map::difference_type;
		using reference = copy_const_t<V, value_type>&;
		using pointer = copy_const_t<V, value_type>*;
		using iterator_category = std::bidirectional_iterator_tag;

		KBLIB_NODISCARD constexpr reference operator*() const {
			return *storage->second[pos].get();
		}
		KBLIB_NODISCARD constexpr pointer operator->() const {
			return storage->second[pos].get();
		}

		constexpr iter& operator++() {
			if (pos == key_range) {
				// not required in general, but direct_map::iterator guarantees that
				// ++end() == end() because it simplifies the implementation and is
				// unlikely to be a significant performance impact
				return *this;
			}
			for (auto i : range(++pos, key_range)) {
				if (storage->first.test(i)) {
					pos = i;
					return *this;
				}
			}
			pos = key_range;
			return *this;
		}
		constexpr iter operator++(int) {
			iter it = *this;
			++*this;
			return it;
		}

		constexpr iter& operator--() {
			for (auto i : range(pos - 1, std::ptrdiff_t(-1))) {
				if (storage->first.test(i)) {
					pos = i;
					return *this;
				}
			}
			// going past the beginning is UB
			__builtin_unreachable();
		}
		constexpr iter operator--(int) {
			iter it = *this;
			--*this;
			return it;
		}

		KBLIB_NODISCARD friend constexpr bool operator==(iter l, iter r) {
			return l.storage == r.storage and l.pos == r.pos;
		}
		KBLIB_NODISCARD friend constexpr bool operator!=(iter l, iter r) {
			return not(l == r);
		}
#define DECL_OP(op)                                                    \
	KBLIB_NODISCARD friend constexpr bool operator op(iter l, iter r) { \
		assert(l.storage == r.storage);                                  \
		return l.pos op r.pos;                                           \
	}
		DECL_OP(<)
		DECL_OP(>)
		DECL_OP(>=)
		DECL_OP(<=)
#undef DECL_OP

		constexpr void swap(iter& other) noexcept {
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
	constexpr direct_map(InputIt first, InputIt last) : storage(in_place_agg) {
		for (auto v : indirect(first, last)) {
			construct(v.first, v.second);
		}
	}
	// TODO: copy construction for allocating direct_map
	constexpr direct_map(const direct_map& other)
	    : storage(in_place_agg, other.storage->first), _size(other._size) {
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, other.at(k));
				++_size;
			}
		}
	}

	constexpr direct_map(direct_map&& other) noexcept
	    : storage(std::move(other.storage)),
	      _size(std::exchange(other._size, 0)) {}

	constexpr direct_map(std::initializer_list<value_type> init)
	    : direct_map(init.begin(), init.end()) {}

	/**
	 *
	 */
	KBLIB_CXX20(constexpr) ~direct_map() { clear(); }

	constexpr direct_map& operator=(const direct_map& other) {
		if (this == &other) {
			return *this;
		}
		clear();
		storage.assign(in_place_agg, other.storage->first);
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, other.at(k));
				++_size;
				bitmap().set(index(k));
			}
		}
		return *this;
	}
	constexpr direct_map& operator=(direct_map&& other) noexcept = default;
	constexpr direct_map& operator=(std::initializer_list<value_type> init) {
		clear();
		for (auto it : init) {
			construct(it->first, it->second);
		}
		return *this;
	}

	KBLIB_NODISCARD constexpr T& at(Key key) & {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr T&& at(Key key) && {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr const T& at(Key key) const& {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr const T&& at(Key key) const&& {
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

	KBLIB_NODISCARD constexpr iterator begin() & noexcept {
		return {storage.get(), cbegin().pos};
	}
	KBLIB_NODISCARD constexpr const_iterator begin() const& noexcept {
		return {storage.get(), cbegin().pos};
	}
	KBLIB_NODISCARD constexpr const_iterator cbegin() const& noexcept {
		if (not empty()) {
			if (contains(to_key(0))) {
				return {storage.get(), 0};
			} else {
				return ++const_iterator{storage.get(), 0};
			}
		} else {
			return {storage.get(), key_range};
		}
	}

	KBLIB_NODISCARD constexpr iterator end() & noexcept {
		return {storage.get(), key_range};
	}
	KBLIB_NODISCARD constexpr const_iterator end() const& noexcept {
		return {storage.get(), key_range};
	}
	KBLIB_NODISCARD constexpr const_iterator cend() const& noexcept {
		return {storage.get(), key_range};
	}

	KBLIB_NODISCARD constexpr iterator rbegin() & noexcept {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr const_iterator rbegin() const& noexcept {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr const_iterator crbegin() const& noexcept {
		return std::make_reverse_iterator(crend());
	}

	KBLIB_NODISCARD constexpr iterator rend() & noexcept {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr const_iterator rend() const& noexcept {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr const_iterator crend() const& noexcept {
		return std::make_reverse_iterator(cbegin());
	}

	KBLIB_NODISCARD constexpr bool empty() const& noexcept { return _size == 0; }

	KBLIB_NODISCARD constexpr std::size_t size() const& noexcept {
		return _size;
	}
	KBLIB_NODISCARD constexpr std::ptrdiff_t ssize() const& noexcept {
		return _size;
	}

	KBLIB_NODISCARD constexpr static std::size_t max_size() noexcept {
		return key_range;
	}

	constexpr void clear() noexcept {
		for (auto i : range(+min(), max() + 1)) {
			if (contains(i)) {
				unsafe_at(i).destroy();
			}
		}
		storage.reset();
		_size = 0;
	}

	constexpr std::pair<iterator, bool> insert(const value_type& value) {
		if (not contains(value.first)) {
			construct(value.first, value.second);
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}
	template <typename U>
	constexpr return_assert_t<std::is_constructible<value_type, U&&>::value,
	                          std::pair<iterator, bool>>
	insert(U&& value) {
		if (not contains(value.first)) {
			construct(value.first, std::forward<U>(value.second));
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}
	constexpr std::pair<iterator, bool> insert(value_type&& value) {
		if (not contains(value.first)) {
			construct(value.first, std::move(value.second));
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}

	template <typename U>
	constexpr std::pair<iterator, bool> insert_or_assign(Key key, U&& value) {
		if (not contains(key)) {
			construct(key, std::forward<U>(value));
			return {{storage.get(), index(key)}, true};
		} else {
			*unsafe_at(key).get() = std::forward<U>(value);
			return {{storage.get(), index(key)}, false};
		}
	}
	template <typename... Args>
	constexpr std::pair<iterator, bool> try_emplace(Key key, Args&&... args) {
		if (not contains(key)) {
			construct(key, std::forward<Args>(args)...);
			return {{storage.get(), index(key)}, true};
		} else {
			return {{storage.get(), index(key)}, false};
		}
	}

	constexpr iterator erase(iterator pos) noexcept {
		bitmap().reset(pos.pos);
		unsafe_at(to_key(pos.pos)).destroy();
		--_size;
		return ++pos;
	}
	constexpr iterator erase(const_iterator pos) noexcept {
		bitmap().reset(pos.pos);
		unsafe_at(to_key(pos.pos)).destroy();
		--_size;
		return ++pos;
	}

	constexpr iterator erase(iterator first, iterator last) noexcept {
		for (auto i : range(first.pos, last.pos)) {
			if (contains(to_key(i))) {
				bitmap().reset(i);
				unsafe_at(to_key(i)).destroy();
				--_size;
			}
		}
		return ++last;
	}

	constexpr std::size_t erase(Key key) noexcept {
		if (contains(key)) {
			bitmap().reset(index(key));
			unsafe_at(key).destroy();
			--_size;
			return 1;
		} else {
			return 0;
		}
	}

	constexpr void swap(direct_map& other) noexcept {
		using std::swap;
		swap(storage, other.storage);
		swap(_size, other._size);
	}

	KBLIB_NODISCARD constexpr bool contains(Key key) const noexcept {
		return storage and bitmap().test(index(key));
	}
	KBLIB_NODISCARD constexpr std::size_t count(Key key) const noexcept {
		return contains(key);
	}

	KBLIB_NODISCARD constexpr iterator find(Key key) & noexcept {
		return contains(key) ? iterator{storage.get(), index(key)}
		                     : iterator{storage.get(), key_range};
	}
	KBLIB_NODISCARD constexpr const_iterator find(Key key) const& noexcept {
		return contains(key) ? iterator{storage.get(), index(key)}
		                     : iterator{storage.get(), key_range};
	}

	KBLIB_NODISCARD constexpr std::pair<iterator, iterator>
	equal_range(Key key) & noexcept {
		return {lower_bound(key), upper_bound(key)};
	}
	KBLIB_NODISCARD constexpr std::pair<const_iterator, const_iterator>
	equal_range(Key key) const& noexcept {
		return {lower_bound(key), upper_bound(key)};
	}

	KBLIB_NODISCARD constexpr iterator lower_bound(Key key) & noexcept {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{storage.get(), index(key)};
		}
	}
	KBLIB_NODISCARD constexpr const_iterator
	lower_bound(Key key) const& noexcept {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{storage.get(), index(key)};
		}
	}

	KBLIB_NODISCARD constexpr iterator upper_bound(Key key) & noexcept {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}
	KBLIB_NODISCARD constexpr const_iterator
	upper_bound(Key key) const& noexcept {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}

	KBLIB_NODISCARD constexpr static Key min() noexcept {
		return std::numeric_limits<Key>::min();
	}
	KBLIB_NODISCARD constexpr static Key max() noexcept {
		return std::numeric_limits<Key>::max();
	}

	KBLIB_NODISCARD friend constexpr bool
	operator==(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>() ==
	                                                  std::declval<T&>())) {
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

	KBLIB_NODISCARD friend constexpr bool
	operator!=(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>() ==
	                                                  std::declval<T&>())) {
		return not(l == r);
	}

	KBLIB_NODISCARD friend constexpr bool
	operator<(const direct_map& l,
	          const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                 std::declval<T&>())) {
		return kblib::lexicographical_compare(l.begin(), l.end(), r.begin(),
		                                      r.end());
	}
	KBLIB_NODISCARD friend constexpr bool
	operator>(const direct_map& l,
	          const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                 std::declval<T&>())) {
		return r < l;
	}
	KBLIB_NODISCARD friend constexpr bool
	operator<=(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                  std::declval<T&>())) {
		return not(r < l);
	}
	KBLIB_NODISCARD friend constexpr bool
	operator>=(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                  std::declval<T&>())) {
		return not(l < r);
	}

	KBLIB_NODISCARD constexpr static std::ptrdiff_t index(Key key) noexcept {
		return to_unsigned(key);
	}
	KBLIB_NODISCARD constexpr static Key to_key(std::ptrdiff_t i) noexcept {
		return Key(i);
	}

 private:
	KBLIB_NODISCARD constexpr std::bitset<key_range>& bitmap() noexcept {
		return storage->first;
	}
	KBLIB_NODISCARD constexpr const std::bitset<key_range>&
	bitmap() const noexcept {
		return storage->first;
	}

	KBLIB_NODISCARD constexpr storage_type& unsafe_at(Key key) & {
		return storage->second[index(key)];
	}
	KBLIB_NODISCARD constexpr storage_type&& unsafe_at(Key key) && {
		return storage->second[index(key)];
	}
	KBLIB_NODISCARD constexpr const storage_type& unsafe_at(Key key) const& {
		return storage->second[index(key)];
	}
	KBLIB_NODISCARD constexpr const storage_type&& unsafe_at(Key key) const&& {
		return storage->second[index(key)];
	}

	void allocate() {
		if (not storage) {
			storage.assign();
		}
	}

	template <typename... Args>
	constexpr void construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		allocate();
		if (not storage->first.test(index(key))) {
			do_construct(key, std::forward<Args>(args)...);
			// doing these after construction maintains exception safety.
			storage->first.set(index(key));
			++_size;
		}
	}

	template <typename... Args>
	constexpr void do_construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		storage->second[index(key)].construct(
		    std::piecewise_construct, std::forward_as_tuple(key),
		    std::forward_as_tuple(std::forward<Args>(args)...));
	}

	// TODO: Implement, test, and document direct_map
	// TODO: allocator support for direct_map
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
	constexpr static std::ptrdiff_t key_range{detail::range_of<Key>};
	using storage_type = detail::storage_for<value_type>;

	template <typename V>
	class iter {
	 public:
		copy_const_t<V, direct_map>* map;
		std::ptrdiff_t pos;

		using value_type = typename direct_map::value_type;
		using difference_type = typename direct_map::difference_type;
		using reference = copy_const_t<V, value_type>&;
		using pointer = copy_const_t<V, value_type>*;
		using iterator_category = std::bidirectional_iterator_tag;

		KBLIB_NODISCARD constexpr reference operator*() const {
			return *map->elems[pos].get();
		}
		KBLIB_NODISCARD constexpr pointer operator->() const {
			return map->elems[pos].get();
		}

		constexpr iter& operator++() {
			if (pos == key_range) {
				// not required in general, but direct_map::iterator guarantees that
				// ++end() == end() because it simplifies the implementation and is
				// unlikely to be a significant performance impact
				return *this;
			}
			for (auto i : range(++pos, key_range)) {
				if (map->active_elems.test(i)) {
					pos = i;
					return *this;
				}
			}
			pos = key_range;
			return *this;
		}
		constexpr iter operator++(int) {
			iter it = *this;
			++*this;
			return it;
		}

		constexpr iter& operator--() {
			for (auto i : range(pos - 1, std::ptrdiff_t(-1))) {
				if (map->active_elems.test(i)) {
					pos = i;
					return *this;
				}
			}
			// going past the beginning is UB
			__builtin_unreachable();
		}
		constexpr iter operator--(int) {
			iter it = *this;
			--*this;
			return it;
		}

		KBLIB_NODISCARD friend constexpr bool operator==(iter l, iter r) {
			return l.map == r.map and l.pos == r.pos;
		}
		KBLIB_NODISCARD friend constexpr bool operator!=(iter l, iter r) {
			return not(l == r);
		}

#define DECL_OP(op)                                                    \
	KBLIB_NODISCARD friend constexpr bool operator op(iter l, iter r) { \
		assert(l.map == r.map);                                          \
		return l.pos op r.pos;                                           \
	}
		DECL_OP(<)
		DECL_OP(>)
		DECL_OP(>=)
		DECL_OP(<=)
#undef DECL_OP

		constexpr void swap(iter& other) noexcept {
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
	    : active_elems{other.active_elems}, _size(other._size) {
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				construct(k, other.unsafe_at(k).get()->second);
				++_size;
				bitmap().set(index(k));
			}
		}
	}

	constexpr direct_map(direct_map&& other) noexcept(
	    std::is_nothrow_move_constructible<value_type>::value)
	    : active_elems{other.active_elems}, _size(other._size) {
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				construct(k, std::move(other.unsafe_at(k).get()->second));
				++_size;
				bitmap().set(index(k));
			}
		}
	}

	constexpr direct_map(std::initializer_list<value_type> init)
	    : direct_map(init.begin(), init.end()) {}

	// TODO: Remove this and allow it to be trivially destructible for trivial
	// Key and T
	KBLIB_CXX20(constexpr) ~direct_map() { clear(); }

	constexpr direct_map& operator=(const direct_map& other) {
		if (this == &other) {
			return *this;
		}
		clear();
		active_elems = other.active_elems;
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				construct(k, other.unsafe_at(k).get()->second);
				++_size;
				bitmap().set(index(k));
			}
		}
		return *this;
	}
	constexpr direct_map& operator=(direct_map&& other) noexcept(
	    std::is_nothrow_move_constructible<T>::value) {
		if (this == &other) {
			return *this;
		}
		active_elems = other.active_elems;
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				construct(k, std::move(other.unsafe_at(k).get()->second));
				++_size;
				bitmap().set(index(k));
			}
		}
		return *this;
	}
	constexpr direct_map& operator=(std::initializer_list<value_type> init) {
		clear();
		for (auto it : init) {
			construct(it->first, it->second);
		}
		return *this;
	}

	KBLIB_NODISCARD constexpr T& at(Key key) & {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr T&& at(Key key) && {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr const T& at(Key key) const& {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD constexpr const T&& at(Key key) const&& {
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

	KBLIB_NODISCARD constexpr iterator begin() & noexcept {
		return {this, cbegin().pos};
	}
	KBLIB_NODISCARD constexpr const_iterator begin() const& noexcept {
		return {this, cbegin().pos};
	}
	KBLIB_NODISCARD constexpr const_iterator cbegin() const& noexcept {
		if (not empty()) {
			if (contains(to_key(0))) {
				return {this, 0};
			} else {
				return ++const_iterator{this, 0};
			}
		} else {
			return {this, key_range};
		}
	}

	KBLIB_NODISCARD constexpr iterator end() & noexcept {
		return {this, key_range};
	}
	KBLIB_NODISCARD constexpr const_iterator end() const& noexcept {
		return {this, key_range};
	}
	KBLIB_NODISCARD constexpr const_iterator cend() const& noexcept {
		return {this, key_range};
	}

	KBLIB_NODISCARD constexpr iterator rbegin() & noexcept {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr const_iterator rbegin() const& noexcept {
		return std::make_reverse_iterator(end());
	}
	KBLIB_NODISCARD constexpr const_iterator crbegin() const& noexcept {
		return std::make_reverse_iterator(crend());
	}

	KBLIB_NODISCARD constexpr iterator rend() & noexcept {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr const_iterator rend() const& noexcept {
		return std::make_reverse_iterator(begin());
	}
	KBLIB_NODISCARD constexpr const_iterator crend() const& noexcept {
		return std::make_reverse_iterator(cbegin());
	}

	KBLIB_NODISCARD constexpr bool empty() const& noexcept { return _size == 0; }

	KBLIB_NODISCARD constexpr std::size_t size() const& noexcept {
		return _size;
	}
	KBLIB_NODISCARD constexpr std::ptrdiff_t ssize() const& noexcept {
		return _size;
	}

	KBLIB_NODISCARD constexpr static std::size_t max_size() noexcept {
		return key_range;
	}

	constexpr void clear() noexcept {
		for (auto i : range(+min(), max() + 1)) {
			if (contains(i)) {
				unsafe_at(i).destroy();
			}
		}
		_size = 0;
	}

	constexpr std::pair<iterator, bool> insert(const value_type& value) {
		if (not contains(value.first)) {
			construct(value.first, value.second);
			return {{this, index(value.first)}, true};
		} else {
			return {{this, index(value.first)}, false};
		}
	}
	template <typename U>
	constexpr return_assert_t<std::is_constructible<value_type, U&&>::value,
	                          std::pair<iterator, bool>>
	insert(U&& value) {
		if (not contains(value.first)) {
			construct(value.first, std::forward<U>(value.second));
			return {{this, index(value.first)}, true};
		} else {
			return {{this, index(value.first)}, false};
		}
	}
	constexpr std::pair<iterator, bool> insert(value_type&& value) {
		if (not contains(value.first)) {
			construct(value.first, std::move(value.second));
			return {{this, index(value.first)}, true};
		} else {
			return {{this, index(value.first)}, false};
		}
	}

	template <typename U>
	constexpr std::pair<iterator, bool> insert_or_assign(Key key, U&& value) {
		if (not contains(key)) {
			construct(key, std::forward<U>(value));
			return {{this, index(key)}, true};
		} else {
			*unsafe_at(key).get() = std::forward<U>(value);
			return {{this, index(key)}, false};
		}
	}
	template <typename... Args>
	constexpr std::pair<iterator, bool> try_emplace(Key key, Args&&... args) {
		if (not contains(key)) {
			construct(key, std::forward<Args>(args)...);
			return {{this, index(key)}, true};
		} else {
			return {{this, index(key)}, false};
		}
	}

	constexpr iterator erase(iterator pos) noexcept {
		destroy(to_key(pos.pos));
		return ++pos;
	}
	constexpr iterator erase(const_iterator pos) noexcept {
		destroy(to_key(pos.pos));
		return ++pos;
	}

	constexpr iterator erase(iterator first, iterator last) noexcept {
		for (auto i : range(first.pos, last.pos)) {
			if (contains(to_key(i))) {
				destroy(to_key(i));
			}
		}
		return ++last;
	}

	constexpr std::size_t erase(Key key) noexcept {
		if (contains(key)) {
			destroy(key);
			return 1;
		} else {
			return 0;
		}
	}

	constexpr void swap(direct_map& other) noexcept(
	    std::is_nothrow_move_constructible<value_type>::value and
	        fakestd::is_nothrow_swappable<T>::value) {
		using std::swap;
		for (auto k : range(+min(), max() + 1)) {
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

	KBLIB_NODISCARD constexpr bool contains(Key key) const noexcept {
		return bitmap().test(index(key));
	}
	KBLIB_NODISCARD constexpr std::size_t count(Key key) const noexcept {
		return contains(key);
	}

	KBLIB_NODISCARD constexpr iterator find(Key key) & noexcept {
		return contains(key) ? iterator{this, index(key)}
		                     : iterator{this, key_range};
	}
	KBLIB_NODISCARD constexpr const_iterator find(Key key) const& noexcept {
		return contains(key) ? iterator{this, index(key)}
		                     : iterator{this, key_range};
	}

	KBLIB_NODISCARD constexpr std::pair<iterator, iterator>
	equal_range(Key key) & noexcept {
		return {lower_bound(key), upper_bound(key)};
	}
	KBLIB_NODISCARD constexpr std::pair<const_iterator, const_iterator>
	equal_range(Key key) const& noexcept {
		return {lower_bound(key), upper_bound(key)};
	}

	KBLIB_NODISCARD constexpr iterator lower_bound(Key key) & noexcept {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{this, index(key)};
		}
	}
	KBLIB_NODISCARD constexpr const_iterator
	lower_bound(Key key) const& noexcept {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{this, index(key)};
		}
	}

	KBLIB_NODISCARD constexpr iterator upper_bound(Key key) & noexcept {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}
	KBLIB_NODISCARD constexpr const_iterator
	upper_bound(Key key) const& noexcept {
		// if the key exists, upper_bound is the next one
		auto l = lower_bound(key);
		if (l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}

	KBLIB_NODISCARD constexpr static Key min() noexcept {
		return std::numeric_limits<Key>::min();
	}
	KBLIB_NODISCARD constexpr static Key max() noexcept {
		return std::numeric_limits<Key>::max();
	}

	KBLIB_NODISCARD friend constexpr bool
	operator==(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>() ==
	                                                  std::declval<T&>())) {
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

	KBLIB_NODISCARD friend constexpr bool
	operator!=(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>() ==
	                                                  std::declval<T&>())) {
		return not(l == r);
	}

	KBLIB_NODISCARD friend constexpr bool
	operator<(const direct_map& l,
	          const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                 std::declval<T&>())) {
		return kblib::lexicographical_compare(l.begin(), l.end(), r.begin(),
		                                      r.end());
	}
	KBLIB_NODISCARD friend constexpr bool
	operator>(const direct_map& l,
	          const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                 std::declval<T&>())) {
		return r < l;
	}
	KBLIB_NODISCARD friend constexpr bool
	operator<=(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                  std::declval<T&>())) {
		return not(r < l);
	}
	KBLIB_NODISCARD friend constexpr bool
	operator>=(const direct_map& l,
	           const direct_map& r) noexcept(noexcept(std::declval<T&>(),
	                                                  std::declval<T&>())) {
		return not(l < r);
	}

	KBLIB_NODISCARD constexpr static std::ptrdiff_t index(Key key) noexcept {
		return to_unsigned(key);
	}
	KBLIB_NODISCARD constexpr static Key to_key(std::ptrdiff_t i) noexcept {
		return Key(i);
	}

 private:
	KBLIB_NODISCARD constexpr std::bitset<key_range>& bitmap() noexcept {
		return active_elems;
	}
	KBLIB_NODISCARD constexpr const std::bitset<key_range>&
	bitmap() const noexcept {
		return active_elems;
	}

	KBLIB_NODISCARD constexpr storage_type& unsafe_at(Key key) & {
		return elems[index(key)];
	}
	KBLIB_NODISCARD constexpr storage_type&& unsafe_at(Key key) && {
		return elems[index(key)];
	}
	KBLIB_NODISCARD constexpr const storage_type& unsafe_at(Key key) const& {
		return elems[index(key)];
	}
	KBLIB_NODISCARD constexpr const storage_type&& unsafe_at(Key key) const&& {
		return elems[index(key)];
	}

	template <typename... Args>
	constexpr void construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		if (not active_elems.test(index(key))) {
			do_construct(key, std::forward<Args>(args)...);
			// doing these after construction maintains exception safety.
			active_elems.set(index(key));
			++_size;
		}
	}

	template <typename... Args>
	constexpr void do_construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		elems[index(key)].construct(
		    std::piecewise_construct, std::forward_as_tuple(key),
		    std::forward_as_tuple(std::forward<Args>(args)...));
	}

	void destroy(Key key) {
		assert(contains(key));

		bitmap().reset(index(key));
		unsafe_at(key).destroy();
		--_size;
	}

	// TODO: Implement, test, and document direct_map

	std::bitset<key_range> active_elems;
	std::array<storage_type, key_range> elems;

	std::size_t _size{};
};

} // namespace kblib

#endif // DIRECT_MAP_H
