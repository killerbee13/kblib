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
#include <optional>

namespace kblib {

namespace detail {

	template <typename T>
	constexpr std::uintmax_t
	    range_of = static_cast<std::uintmax_t>(std::numeric_limits<T>::max()) -
	               std::numeric_limits<T>::min() + 1;
	static_assert(range_of<unsigned char> == 1 << CHAR_BIT, "");

	template <typename T>
	struct alignas(T) storage_for : private std::array<std::byte, sizeof(T)> {
		template <typename... Args>
		constexpr T& construct(Args&&... args) noexcept(
		    std::is_nothrow_constructible<T, Args&&...>::value) {
			return *new (this->data()) T(std::forward<Args>(args)...);
		}
		template <typename... Args>
		constexpr T& lconstruct(Args&&... args) noexcept(
		    std::is_nothrow_constructible<T, Args&&...>::value) {
			return *new (this->data()) T{std::forward<Args>(args)...};
		}

		constexpr void destroy() noexcept { get()->~T(); }

		constexpr T* get() & noexcept {
			return std::launder(reinterpret_cast<T*>(this->data()));
		}
		constexpr const T* get() const& noexcept {
			return std::launder(reinterpret_cast<T*>(this->data()));
		}
	};

} // namespace detail

template <typename Key, typename T>
class direct_map {
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

	using heap_type =
	    std::pair<std::bitset<key_range>, std::array<storage_type, key_range>>;

	template <typename V>
	class iter {
	 public:
		copy_const_t<V, heap_type>* storage;
		std::ptrdiff_t pos;

		using value_type = typename direct_map::value_type;
		using difference_type = typename direct_map::difference_type;
		using reference = copy_const_t<V, value_type>&;
		using pointer = copy_const_t<V, value_type>*;
		using iterator_category = std::bidirectional_iterator_tag;

		reference operator*() const { return *storage->second[pos].get(); }
		pointer operator->() const { return storage->second[pos].get(); }

		iter& operator++() {
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
		iter operator++(int) {
			iter it = *this;
			++*this;
			return it;
		}

		iter& operator--() {
			for (auto i : range(pos - 1, std::ptrdiff_t(-1))) {
				if (storage->first.test(i)) {
					pos = i;
					return *this;
				}
			}
			// going past the beginning is UB
			__builtin_unreachable();
		}
		iter operator--(int) {
			iter it = *this;
			--*this;
			return it;
		}

		friend bool operator==(iter l, iter r) {
			return l.storage == r.storage && l.pos == r.pos;
		}
		friend bool operator!=(iter l, iter r) { return !(l == r); }

		friend void swap(iter<V>& a, iter<V>& b) {
			using std::swap;
			swap(a.storage, b.storage);
			swap(a.pos, b.pos);
		}
	};

 public:
	using iterator = iter<value_type>;
	using const_iterator = iter<const value_type>;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	constexpr direct_map() noexcept = default;

	template <typename InputIt>
	direct_map(InputIt first, InputIt last) : storage(in_place_agg) {
		for (auto v : indirect(first, last)) {
			construct(v.first, v.second);
		}
	}

	direct_map(const direct_map& other)
	    : storage(in_place_agg, other.storage->first), _size(other._size) {
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, other.at(k));
				++_size;
				bitmap().set(index(k));
			}
		}
	}

	direct_map(direct_map&& other) noexcept
	    : storage(std::move(other.storage)),
	      _size(std::exchange(other._size, 0)) {}

	direct_map(std::initializer_list<value_type> init)
	    : direct_map(init.begin(), init.end()) {}

	/**
	 *
	 */
	~direct_map() { clear(); }

	direct_map& operator=(const direct_map& other) {
		clear();
		storage.assign(in_place_agg, other.storage->first);
		for (auto k : range(+min(), max() + 1)) {
			if (contains(k)) { // the bitmap is already copied from other
				do_construct(k, other.at(k));
				++_size;
				bitmap().set(index(k));
			}
		}
	}
	direct_map& operator=(direct_map&& other) noexcept = default;
	direct_map& operator=(std::initializer_list<value_type> init) {
		clear();
		for (auto it : init) {
			construct(it->first, it->second);
		}
	}

	KBLIB_NODISCARD T& at(Key key) & {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD T&& at(Key key) && {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD const T& at(Key key) const& {
		if (contains(key)) {
			return unsafe_at(key).get()->second;
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}
	KBLIB_NODISCARD const T&& at(Key key) const&& {
		if (contains(key)) {
			return std::move(unsafe_at(key).get()->second);
		} else {
			throw std::out_of_range("direct_map: key out of range");
		}
	}

	T& operator[](Key key) noexcept(
	    std::is_nothrow_default_constructible<T>::value) {
		return try_emplace(key).first->second;
	}

	iterator begin() & noexcept { return {storage.get(), cbegin().pos}; }
	const_iterator begin() const& noexcept {
		return {storage.get(), cbegin().pos};
	}
	const_iterator cbegin() const& noexcept {
		if (!empty()) {
			if (contains(to_key(0))) {
				return {storage.get(), 0};
			} else {
				return ++const_iterator{storage.get(), 0};
			}
		} else {
			return {storage.get(), key_range};
		}
	}

	iterator end() & noexcept { return {storage.get(), key_range}; }
	const_iterator end() const& noexcept { return {storage.get(), key_range}; }
	const_iterator cend() const& noexcept { return {storage.get(), key_range}; }

	iterator rbegin() & noexcept { return std::make_reverse_iterator(end()); }
	const_iterator rbegin() const& noexcept {
		return std::make_reverse_iterator(end());
	}
	const_iterator crbegin() const& noexcept {
		return std::make_reverse_iterator(crend());
	}

	iterator rend() & noexcept { return std::make_reverse_iterator(begin()); }
	const_iterator rend() const& noexcept {
		return std::make_reverse_iterator(begin());
	}
	const_iterator crend() const& noexcept {
		return std::make_reverse_iterator(cbegin());
	}

	bool empty() const& noexcept { return _size == 0; }

	std::size_t size() const& noexcept { return _size; }
	std::ptrdiff_t ssize() const& noexcept { return _size; }

	constexpr static std::size_t max_size() noexcept { return key_range; }

	void clear() noexcept {
		for (auto i : range(+min(), max() + 1)) {
			if (contains(i)) {
				unsafe_at(i).destroy();
			}
		}
		storage.reset();
		_size = 0;
	}

	std::pair<iterator, bool> insert(const value_type& value) {
		if (!contains(value.first)) {
			construct(value.first, value.second);
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}
	template <typename U>
	return_assert_t<std::is_constructible<value_type, U&&>::value,
	                std::pair<iterator, bool>>
	insert(U&& value) {
		if (!contains(value.first)) {
			construct(value.first, std::forward<U>(value.second));
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}
	std::pair<iterator, bool> insert(value_type&& value) {
		if (!contains(value.first)) {
			construct(value.first, std::move(value.second));
			return {{storage.get(), index(value.first)}, true};
		} else {
			return {{storage.get(), index(value.first)}, false};
		}
	}

	template <typename U>
	std::pair<iterator, bool> insert_or_assign(Key key, U&& value) {
		if (!contains(key)) {
			construct(key, std::move(value));
			return {{storage.get(), index(key)}, true};
		} else {
			*unsafe_at(key).get() = std::forward<U>(value);
			return {{storage.get(), index(key)}, false};
		}
	}
	template <typename... Args>
	std::pair<iterator, bool> try_emplace(Key key, Args&&... args) {
		if (!contains(key)) {
			construct(key, std::forward<Args>(args)...);
			return {{storage.get(), index(key)}, true};
		} else {
			return {{storage.get(), index(key)}, false};
		}
	}

	iterator erase(iterator pos) noexcept {
		bitmap().reset(pos.pos);
		unsafe_at(to_key(pos.pos)).destroy();
		--_size;
		return ++pos;
	}
	iterator erase(const_iterator pos) noexcept {
		bitmap().reset(pos.pos);
		unsafe_at(to_key(pos.pos)).destroy();
		--_size;
		return ++pos;
	}

	iterator erase(iterator first, iterator last) noexcept {
		for (auto i : range(first.pos, last.pos)) {
			if (contains(to_key(i))) {
				bitmap().reset(i);
				unsafe_at(to_key(i)).destroy();
				--_size;
			}
		}
		return ++last;
	}

	std::size_t erase(Key key) noexcept {
		if (contains(key)) {
			bitmap().reset(index(key));
			unsafe_at(key).destroy();
			--_size;
			return 1;
		} else {
			return 0;
		}
	}

	void swap(direct_map& other) noexcept {
		using std::swap;
		swap(storage, other.storage);
		swap(_size, other._size);
	}

	bool contains(Key key) const noexcept {
		return storage && bitmap().test(index(key));
	}
	std::size_t count(Key key) const noexcept { return contains(key); }

	iterator find(Key key) & noexcept {
		return contains(key) ? iterator{storage.get(), index(key)}
		                     : iterator{storage.get(), key_range};
	}
	const_iterator find(Key key) const& noexcept {
		return contains(key) ? iterator{storage.get(), index(key)}
		                     : iterator{storage.get(), key_range};
	}

	std::pair<iterator, iterator> equal_range(Key key) & noexcept {
		return {lower_bound(key), upper_bound(key)};
	}
	std::pair<const_iterator, const_iterator>
	equal_range(Key key) const& noexcept {
		return {lower_bound(key), upper_bound(key)};
	}

	iterator lower_bound(Key key) & noexcept {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{storage.get(), index(key)};
		}
	}
	const_iterator lower_bound(Key key) const& noexcept {
		if (contains(key)) {
			return find(key);
		} else {
			return ++iterator{storage.get(), index(key)};
		}
	}

	iterator upper_bound(Key key) & noexcept {
		// if the key exists, upper_bound is the next one
		if (auto l = lower_bound(key); l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}
	const_iterator upper_bound(Key key) const& noexcept {
		// if the key exists, upper_bound is the next one
		if (auto l = lower_bound(key); l.pos == index(key)) {
			return ++l;
			// otherwise upper_bound == lower_bound
		} else {
			return l;
		}
	}

	constexpr static Key min() noexcept {
		return std::numeric_limits<Key>::min();
	}
	constexpr static Key max() noexcept {
		return std::numeric_limits<Key>::max();
	}

	friend bool operator==(
	    const direct_map<Key, T>& l,
	    const direct_map<Key, T>& r) noexcept(noexcept(std::declval<T&>() ==
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

	friend bool operator!=(
	    const direct_map<Key, T>& l,
	    const direct_map<Key, T>& r) noexcept(noexcept(std::declval<T&>() ==
	                                                   std::declval<T&>())) {
		return !(l == r);
	}

	friend bool
	operator<(const direct_map<Key, T>& l, const direct_map<Key, T>& r) noexcept(
	    noexcept(std::declval<T&>(), std::declval<T&>())) {
		return std::lexicographical_compare(l.begin(), l.end(), r.begin(),
		                                    r.end());
	}
	friend bool
	operator>(const direct_map<Key, T>& l, const direct_map<Key, T>& r) noexcept(
	    noexcept(std::declval<T&>(), std::declval<T&>())) {
		return r < l;
	}
	friend bool operator<=(
	    const direct_map<Key, T>& l,
	    const direct_map<Key, T>& r) noexcept(noexcept(std::declval<T&>(),
	                                                   std::declval<T&>())) {
		return !(r < l);
	}
	friend bool operator>=(
	    const direct_map<Key, T>& l,
	    const direct_map<Key, T>& r) noexcept(noexcept(std::declval<T&>(),
	                                                   std::declval<T&>())) {
		return !(l < r);
	}

	constexpr static std::ptrdiff_t index(Key key) noexcept {
		return to_unsigned(key);
	}
	constexpr static Key to_key(std::ptrdiff_t i) noexcept { return Key(i); }

 private:
	std::bitset<key_range>& bitmap() noexcept { return storage->first; }
	const std::bitset<key_range>& bitmap() const noexcept {
		return storage->first;
	}

	storage_type& unsafe_at(Key key) & { return storage->second[index(key)]; }
	storage_type&& unsafe_at(Key key) && { return storage->second[index(key)]; }
	const storage_type& unsafe_at(Key key) const& {
		return storage->second[index(key)];
	}
	const storage_type&& unsafe_at(Key key) const&& {
		return storage->second[index(key)];
	}

	void allocate() {
		if (!storage) {
			storage.assign();
		}
	}

	template <typename... Args>
	void construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		allocate();
		if (!storage->first.test(index(key))) {
			do_construct(key, std::forward<Args>(args)...);
			// doing these after construction maintains exception safety.
			storage->first.set(index(key));
			++_size;
		}
	}

	template <typename... Args>
	void do_construct(Key key, Args&&... args) noexcept(
	    std::is_nothrow_constructible<value_type, Args&&...>::value) {
		storage->second[index(key)].construct(
		    std::piecewise_construct, std::forward_as_tuple(key),
		    std::forward_as_tuple(std::forward<Args>(args)...));
	}

	// TODO: Implement, test, and document direct_map

	kblib::heap_value<heap_type> storage;
	std::size_t _size{};
};

} // namespace kblib

#endif // DIRECT_MAP_H
