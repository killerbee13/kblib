#ifndef KBLIB_CONTAINERS_H
#define KBLIB_CONTAINERS_H

#include "fakestd.h"
#include "iterators.h"
#include "tdecl.h"
#include "traits.h"

#include <cstddef>
#include <deque>
#include <iterator>
#include <memory>
#include <stack>
#include <type_traits>
#include <vector>

namespace kblib {

template <typename C>
typename C::value_type pop(C& s) {
	typename C::value_type ret = std::move(s.top());
	s.pop();
	return ret;
}

template <class C, typename K, typename V>
typename C::mapped_type get_or(const C& m, const K& key, const V& defval) {
	auto it = m.find(key);
	if (it == m.end())
		return defval;
	else
		return it->second;
}

template <typename Map, typename Key>
auto try_get(Map& map, Key&& key)
    -> copy_const_t<Map, typename Map::mapped_type>* {
	auto it = map.find(std::forward<Key>(key));
	if (it == map.end())
		return nullptr;
	else
		return &it->second;
}

template <typename iterator>
struct exists_t {
	iterator it;
	bool found;
	operator bool() const noexcept { return found; }
	auto operator*() const noexcept(noexcept(*it)) -> decltype(*it) {
		return *it;
	}
	iterator operator->() const noexcept { return it; }
	auto* addr() const noexcept { return to_pointer(it); }
};

template <typename M, typename K>
auto get_check(M&& m, const K& key) noexcept(noexcept(m.find(key) != m.end()))
    -> exists_t<decltype(m.find(key))> {
	auto it = m.find(key);
	return {it, it != m.end()};
}

/**
 * @brief std::vector::shrink_to_fit is non-binding, which means that there is
 * no guaranteed way to shrink a vector via its API. This function is a
 * roundabout way of doing that without relying on the sanity of the
 * implementation.
 *
 * This function explicitly constructs a new vector and moves into it, before
 * overwriting the old vector with the new one, meaning that the vector is
 * forced to forget its capacity.
 *
 * This function provides the strong exception guarantee.
 *
 * @param vec The vector to force-shrink.
 */
template <typename V>
void force_shrink_to_fit(V& vec) {
	if (std::is_nothrow_move_constructible<typename V::value_type>::value) {
		V tmp;
		try_reserve(tmp, vec.size());
		std::move(vec.begin(), vec.end(), std::back_inserter(tmp));
		vec = std::move(tmp);
	} else {
		V tmp(vec.begin(), vec.end());
		vec = std::move(tmp);
	}
	return;
}

template <typename C, std::size_t size>
struct construct_with_size {
	constexpr static C make() { return C(size); }
};

template <typename C, std::size_t size>
struct construct_with_capacity {
	constexpr static C make() {
		C c;
		c.reserve(size);
		return c;
	}
};

/**
 * @brief Allows for constructing a container of a specified type from a range
 * object. Copy elision means that this does not result in any extra copies.
 *
 * @tparam Container The container type to construct an object of.
 * @param r The range to construct the returned container from.
 * @return Container Container{begin(r), end(r)};
 */
template <typename Container, typename Range>
Container construct_from_range(Range&& r) {
	using std::begin;
	using std::end;
	return Container{begin(std::forward<Range>(r)), end(std::forward<Range>(r))};
}

template <typename Container,
          bool ArrayLike = !detail::is_resizable_v<Container>>
class KBLIB_NODISCARD build_iterator {
 public:
	using value_type = void;
	using difference_type = void;
	using pointer = void;
	using reference = void;
	using iterator_category = std::output_iterator_tag;

	template <typename... Args>
	build_iterator(Args&&... args)
	    : range(std::make_shared<Container>(std::forward<Args>(args)...)) {}

	Container
	base() noexcept(std::is_nothrow_move_constructible<Container>::value) {
		auto holder = std::move(range);
		return std::move(*holder);
	}

	explicit operator Container() noexcept(
	    std::is_nothrow_move_constructible<Container>::value) {
		auto holder = std::move(range);
		return std::move(*holder);
	}

	/**
	 * @brief Creates a temporary std::back_insert_iterator for the range and
	 * returns it.
	 *
	 * Returning an iterator from operator* might look strange, but
	 * std::back_insert_iterator can be assigned to to insert into the range,
	 * and its operator* returns itself anyhow.
	 */
	decltype(auto) operator*() const
	    noexcept(noexcept(*std::back_inserter(*range))) {
		return std::back_inserter(*range);
	}

	/**
	 * @brief A no-op.
	 */
	build_iterator& operator++() { return *this; }
	/**
	 * @brief A no-op.
	 */
	build_iterator& operator++(int) { return *this; }

 private:
	/**
	 * @brief range A shared_ptr to the managed range.
	 *
	 * It is unfortunate that this has to be a shared_ptr, but that's the only
	 * way to make this class a valid iterator. A move-only build_iterator-alike
	 * could avoid this overhead, and I may write one because several algorithms
	 * don't ever need to copy iterators.
	 */
	std::shared_ptr<Container> range;
};

KBLIB_UNUSED constexpr struct build_end_t {
	template <typename T>
	constexpr operator T() const noexcept(noexcept(T{*this})) {
		return T{this};
	}
} build_end;

template <typename Container>
class KBLIB_NODISCARD build_iterator<Container, true> {
 public:
	using value_type = void;
	using difference_type = void;
	using pointer = void;
	using reference = void;
	using iterator_category = std::output_iterator_tag;

	template <typename... Args>
	build_iterator(Args&&... args)
	    : range(std::make_shared<Container>(std::forward<Args>(args)...)) {}

	build_iterator(const build_end_t&)
	    : range{nullptr}, index(std::tuple_size<Container>::value) {}

	Container
	base() noexcept(std::is_nothrow_move_constructible<Container>::value) {
		auto holder = std::move(range);
		return std::move(*holder);
	}

	explicit operator Container() noexcept(
	    std::is_nothrow_move_constructible<Container>::value) {
		auto holder = std::move(range);
		return std::move(*holder);
	}

	decltype(auto) operator*() const noexcept { return (*range)[index]; }
	auto* operator-> () const noexcept { return &(*range)[index]; }

	/**
	 * @brief Advance to the next element.
	 */
	build_iterator& operator++() {
		++index;
		return *this;
	}
	/**
	 * @brief Advance to the next element.
	 */
	build_iterator& operator++(int) {
		auto tmp = *this;
		++index;
		return tmp;
	}

	constexpr auto size() const noexcept { return kblib::size(*range); }

	friend bool operator==(const build_iterator<Container>& it, build_end_t) {
		return it.index == it.size();
	}
	friend bool operator!=(const build_iterator<Container>& it, build_end_t) {
		return it.index != it.size();
	}

	friend bool operator==(build_end_t, const build_iterator<Container>& it) {
		return it.index == it.size();
	}
	friend bool operator!=(build_end_t, const build_iterator<Container>& it) {
		return it.index != it.size();
	}

	friend bool operator==(const build_iterator<Container>& it1,
	                       const build_iterator<Container>& it2) {
		return it1.index == it2.index;
	}
	friend bool operator!=(const build_iterator<Container>& it1,
	                       const build_iterator<Container>& it2) {
		return it1.index != it2.index;
	}

 private:
	/**
	 * @brief range A shared_ptr to the managed range.
	 *
	 * It is unfortunate that this has to be a shared_ptr, but that's the only
	 * way to make this class a valid iterator. A move-only build_iterator-alike
	 * could avoid this overhead, and I may write one because several algorithms
	 * don't ever need to copy iterators.
	 */
	std::shared_ptr<Container> range;
	std::size_t index{};
};

} // namespace kblib

namespace std {
#if defined(__clang__)
// Because apparently -Wno-mismatched-tags doesn't work
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wmismatched-tags"
#endif

template <typename C, std::size_t size>
struct tuple_size<::kblib::construct_with_size<C, size>>
    : public integral_constant<size_t, size> {};

#if defined(__clang__)
#pragma clang diagnostic pop
#endif
} // namespace std

namespace kblib {
namespace detail {

   template <typename Container, std::size_t N>
   struct buildiota_impl<construct_with_size<Container, N>, false> {
		template <typename T>
		constexpr static Container impl(T value) {
			Container out = construct_with_size<Container, N>::make();
			for (auto& v : out) {
				v = value;
				++value;
			}
			return out;
		}
		template <typename T, typename I>
		constexpr static Container impl(T value, I incr) {
			Container out = construct_with_size<Container, N>::make();
			for (auto& v : out) {
				v = value;
				value += incr;
			}
			return out;
		}
	};

} // namespace detail

template <typename T, typename Container = std::vector<T>>
class stack {
 public:
	// Member types

	using container_type = Container;
	using value_type = typename Container::value_type;
	using size_type = typename Container::size_type;
	using reference = typename Container::reference;
	using const_reference = typename Container::const_reference;

	static_assert(std::is_same<T, value_type>::value,
	              "Container::value_type must be T.");

	// Constructors

	stack() : stack(Container()) {}
	explicit stack(const Container& cont) : backing(cont) {}
	explicit stack(Container&& cont) noexcept(
	    std::is_nothrow_move_constructible<container_type>::value)
	    : backing(std::move(cont)) {}
	stack(const stack& other) : backing(other.backing) {}
	stack(stack&& other) noexcept(
	    std::is_nothrow_move_constructible<container_type>::value)
	    : backing(std::move(other.backing)) {}

	template <
	    typename Alloc,
	    typename std::enable_if<
	        std::uses_allocator<container_type, Alloc>::value, int>::type = 0>
	explicit stack(const Alloc& alloc);
	template <
	    typename Alloc,
	    typename std::enable_if<
	        std::uses_allocator<container_type, Alloc>::value, int>::type = 0>
	stack(const Container& cont, const Alloc& alloc);
	template <
	    typename Alloc,
	    typename std::enable_if<
	        std::uses_allocator<container_type, Alloc>::value, int>::type = 0>
	stack(Container&& cont, const Alloc& alloc);
	template <
	    typename Alloc,
	    typename std::enable_if<
	        std::uses_allocator<container_type, Alloc>::value, int>::type = 0>
	stack(const stack& cont, const Alloc& alloc);
	template <
	    typename Alloc,
	    typename std::enable_if<
	        std::uses_allocator<container_type, Alloc>::value, int>::type = 0>
	stack(stack&& cont, const Alloc& alloc);

	// Element access

	reference top() & noexcept(noexcept(backing.back())) {
		return backing.back();
	}
	const_reference top() const& noexcept(noexcept(backing.back())) {
		return backing.back();
	}

	// Capacity

	KBLIB_NODISCARD bool empty() const noexcept { return backing.empty(); }
	KBLIB_NODISCARD size_type size() const noexcept { return backing.size(); }

	// Modifiers

	void push(const value_type& value) { backing.push_back(value); }
	void push(value_type&& value) { backing.push_back(std::move(value)); }

	template <typename... Args>
	decltype(auto) emplace(Args&&... args) & {
		return backing.emplace_back(std::forward<Args>(args)...);
	}

	void pop() { backing.pop_back(); }
	void clear() { backing.clear(); }

	void swap(stack& other) noexcept(
	    fakestd::is_nothrow_swappable<Container>::value) {
		using std::swap;
		swap(backing, other.backing);
	}

	// Container access

	const container_type& container() const& { return backing; }
	container_type& container() & { return backing; }

	container_type container() && { return std::move(backing); }

 private:
	container_type backing;
};

} // namespace kblib

#endif // KBLIB_CONTAINERS_H
