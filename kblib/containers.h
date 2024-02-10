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
 * @brief Provides generic operations for containers, as well as kblib::stack.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

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

namespace KBLIB_NS {

template <typename C>
KBLIB_NODISCARD constexpr auto pop(C& s) -> typename C::value_type {
	typename C::value_type ret = std::move(s.top());
	s.pop();
	return ret;
}

template <class C, typename K, typename V>
KBLIB_NODISCARD constexpr auto get_or(const C& m, const K& key, const V& defval)
    -> typename C::mapped_type {
	auto it = m.find(key);
	if (it == m.end())
		return defval;
	else
		return it->second;
}

template <typename Map, typename Key>
KBLIB_NODISCARD constexpr auto try_get(Map& map, Key&& key)
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
	KBLIB_NODISCARD constexpr operator bool() const noexcept { return found; }
	KBLIB_NODISCARD constexpr auto operator*() const noexcept(noexcept(*it))
	    -> decltype(*it) {
		return *it;
	}
	KBLIB_NODISCARD constexpr auto operator->() const noexcept -> iterator {
		return it;
	}
	KBLIB_NODISCARD constexpr auto addr() const noexcept -> auto* {
		return to_pointer(it);
	}
};

template <typename M, typename K>
KBLIB_NODISCARD constexpr auto get_check(M&& m, const K& key) noexcept(
    noexcept(m.find(key) != m.end())) -> exists_t<decltype(m.find(key))> {
	auto it = m.find(key);
	return {it, it != m.end()};
}

/**
 * @brief std::vector::shrink_to_fit is non-binding, which means that there is
 * no guaranteed way to shrink a vector via its API. This function is a
 * roundabout way of doing that without relying on the sanity of the
 * implementation (except that it assumes that a vector won't significantly
 * over-allocate on sized construction).
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
auto force_shrink_to_fit(V& vec) -> void {
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
	KBLIB_NODISCARD constexpr static auto make() -> C { return C(size); }
};

template <typename C, std::size_t size>
struct construct_with_capacity {
	KBLIB_NODISCARD constexpr static auto make() -> C {
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
KBLIB_NODISCARD constexpr auto construct_from_range(Range&& r) -> Container {
	using std::begin;
	using std::end;
	return Container{begin(std::forward<Range>(r)), end(std::forward<Range>(r))};
}

template <typename Container, bool ArrayLike = not is_resizable_v<Container>>
class KBLIB_NODISCARD build_iterator {
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

 public:
	using value_type = void;
	using difference_type = void;
	using pointer = void;
	using reference = void;
	using iterator_category = std::output_iterator_tag;

	template <typename... Args>
	build_iterator(Args&&... args)
	    : range(std::make_shared<Container>(std::forward<Args>(args)...)) {}

	KBLIB_NODISCARD constexpr auto base() noexcept(
	    std::is_nothrow_move_constructible<Container>::value) -> Container {
		auto holder = std::move(range);
		return std::move(*holder);
	}

	KBLIB_NODISCARD constexpr explicit operator Container() noexcept(
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
	KBLIB_NODISCARD constexpr auto operator*() const
	    noexcept(noexcept(*std::back_inserter(*range))) -> decltype(auto) {
		return std::back_inserter(*range);
	}

	/**
	 * @brief A no-op.
	 */
	KBLIB_NODISCARD constexpr auto operator++() -> build_iterator& {
		return *this;
	}
	/**
	 * @brief A no-op.
	 */
	KBLIB_NODISCARD constexpr auto operator++(int) -> build_iterator& {
		return *this;
	}
};

KBLIB_UNUSED constexpr struct build_end_t {
	template <typename T>
	KBLIB_NODISCARD constexpr operator T() const
	    noexcept(noexcept(T{std::declval<build_end_t&>()})) {
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
	    : range{nullptr}
	    , index(std::tuple_size<Container>::value) {}

	KBLIB_NODISCARD auto base() noexcept(
	    std::is_nothrow_move_constructible<Container>::value) -> Container {
		auto holder = std::move(range);
		return std::move(*holder);
	}

	KBLIB_NODISCARD explicit operator Container() noexcept(
	    std::is_nothrow_move_constructible<Container>::value) {
		auto holder = std::move(range);
		return std::move(*holder);
	}

	KBLIB_NODISCARD auto operator*() const noexcept -> decltype(auto) {
		return (*range)[index];
	}
	KBLIB_NODISCARD auto operator->() const noexcept -> auto* {
		return &(*range)[index];
	}

	/**
	 * @brief Advance to the next element.
	 */
	auto operator++() -> build_iterator& {
		++index;
		return *this;
	}
	/**
	 * @brief Advance to the next element.
	 */
	auto operator++(int) -> build_iterator& {
		auto tmp = *this;
		++index;
		return tmp;
	}

	KBLIB_NODISCARD constexpr auto size() const noexcept -> std::size_t {
		return kblib::size(*range);
	}

	KBLIB_NODISCARD constexpr friend auto operator==(
	    const build_iterator<Container>& it, build_end_t) noexcept -> bool {
		return it.index == it.size();
	}
	KBLIB_NODISCARD constexpr friend auto operator!=(
	    const build_iterator<Container>& it, build_end_t) noexcept -> bool {
		return it.index != it.size();
	}

	KBLIB_NODISCARD constexpr friend auto operator==(
	    build_end_t, const build_iterator<Container>& it) noexcept -> bool {
		return it.index == it.size();
	}
	KBLIB_NODISCARD constexpr friend auto operator!=(
	    build_end_t, const build_iterator<Container>& it) noexcept -> bool {
		return it.index != it.size();
	}

	KBLIB_NODISCARD constexpr friend auto operator==(
	    const build_iterator<Container>& it1,
	    const build_iterator<Container>& it2) noexcept -> bool {
		return it1.index == it2.index;
	}
	KBLIB_NODISCARD constexpr friend auto operator!=(
	    const build_iterator<Container>& it1,
	    const build_iterator<Container>& it2) noexcept -> bool {
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

} // namespace KBLIB_NS

namespace std {
#if defined(__clang__)
// Because apparently -Wno-mismatched-tags doesn't work
#	pragma clang diagnostic push
#	pragma clang diagnostic ignored "-Wmismatched-tags"
#endif

template <typename C, std::size_t Size>
struct tuple_size<::kblib::construct_with_size<C, Size>>
    : public integral_constant<size_t, Size> {};

#if defined(__clang__)
#	pragma clang diagnostic pop
#endif
} // namespace std

namespace KBLIB_NS {
namespace detail {

	template <typename Container, std::size_t N>
	struct buildiota_impl<construct_with_size<Container, N>, false> {
		template <typename T>
		constexpr static auto impl(T value) -> Container {
			Container out = construct_with_size<Container, N>::make();
			for (auto& v : out) {
				v = value;
				++value;
			}
			return out;
		}
		template <typename T, typename I>
		constexpr static auto impl(T value, I incr) -> Container {
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
class [[deprecated("use a class derived from std::stack instead")]] stack {
 public:
	// Member types

	using container_type = Container;
	using value_type = typename Container::value_type;
	using size_type = typename Container::size_type;
	using reference = typename Container::reference;
	using const_reference = typename Container::const_reference;

	static_assert(std::is_same<T, value_type>::value,
	              "Container::value_type must be T.");

 private:
	container_type backing;

 public:
	// Constructors

	stack()
	    : stack(Container()) {}
	explicit stack(const Container& cont)
	    : backing(cont) {}

	template <typename Alloc,
	          typename std::enable_if<
	              std::uses_allocator<container_type, Alloc>::value, int>::type
	          = 0>
	explicit stack(const Alloc& alloc);
	template <typename Alloc,
	          typename std::enable_if<
	              std::uses_allocator<container_type, Alloc>::value, int>::type
	          = 0>
	stack(const Container& cont, const Alloc& alloc);
	template <typename Alloc,
	          typename std::enable_if<
	              std::uses_allocator<container_type, Alloc>::value, int>::type
	          = 0>
	stack(Container && cont, const Alloc& alloc);
	template <typename Alloc,
	          typename std::enable_if<
	              std::uses_allocator<container_type, Alloc>::value, int>::type
	          = 0>
	stack(const stack& cont, const Alloc& alloc);
	template <typename Alloc,
	          typename std::enable_if<
	              std::uses_allocator<container_type, Alloc>::value, int>::type
	          = 0>
	stack(stack && cont, const Alloc& alloc);

	// Element access

	KBLIB_NODISCARD auto top()& noexcept(noexcept(backing.back()))->reference {
		return backing.back();
	}
	KBLIB_NODISCARD auto top() const& noexcept(noexcept(backing.back()))
	    ->const_reference {
		return backing.back();
	}

	// Capacity

	KBLIB_NODISCARD auto empty() const noexcept->bool { return backing.empty(); }
	KBLIB_NODISCARD auto size() const noexcept->size_type {
		return backing.size();
	}

	// Modifiers

	auto push(const value_type& value)->decltype(auto) {
		return backing.push_back(value);
	}
	auto push(value_type && value)->decltype(auto) {
		return backing.push_back(std::move(value));
	}

	template <typename... Args>
	auto emplace(Args && ... args)&->decltype(auto) {
		return backing.emplace_back(std::forward<Args>(args)...);
	}

	auto pop() noexcept(noexcept(backing.pop_back()))->void {
		backing.pop_back();
		return;
	}
	auto clear() noexcept(noexcept(backing.clear()))->void {
		backing.clear();
		return;
	}

	auto swap(stack
	          & other) noexcept(fakestd::is_nothrow_swappable<Container>::value)
	    ->void {
		using std::swap;
		swap(backing, other.backing);
		return;
	}

	// Container access

	KBLIB_NODISCARD auto container() const&->container_type& { return backing; }
	KBLIB_NODISCARD auto container()&->container_type& { return backing; }

	KBLIB_NODISCARD auto container()&&->container_type {
		return std::move(backing);
	}
};

} // namespace KBLIB_NS

#endif // KBLIB_CONTAINERS_H
