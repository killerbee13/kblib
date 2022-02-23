/*
 * MIT License
 *
 * Copyright (c) 2018 Tobias Widlund
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

#ifndef ENUMERATECONTRIBTW_H
#define ENUMERATECONTRIBTW_H
// Original source available at https://github.com/therocode/enumerate

#include "tdecl.h"

#include <iterator>
#include <utility>

namespace kblib {

/**
 * @brief
 *
 * @author Tobias Widlund, killerbee
 * @date 2018-2020
 * @copyright MIT license.
 */
template <typename It>
struct enumerate_iterator {
	It it;
	std::size_t idx;

	using nested_reference = typename std::iterator_traits<It>::reference;

	using difference_type = std::ptrdiff_t;
	using value_type = std::pair<nested_reference, std::size_t>;
	using pointer = void;
	using reference = value_type;
	using iterator_category = std::input_iterator_tag;

	constexpr auto operator*() -> value_type { return {*it, idx}; }

	constexpr auto operator++() & -> enumerate_iterator& {
		++it;
		++idx;
		return *this;
	}
	constexpr auto operator++(int) -> enumerate_iterator {
		auto tmp = *this;
		++(*this);
		return tmp;
	}

	template <typename OIt>
	constexpr auto operator==(OIt rhs)
	    -> decltype(std::declval<It&>() == std::declval<OIt&>()) {
		return it == rhs;
	}
	template <typename OIt>
	constexpr auto operator!=(OIt rhs)
	    -> decltype(std::declval<It&>() != std::declval<OIt&>()) {
		return it != rhs;
	}

	constexpr friend auto operator==(enumerate_iterator lhs, enumerate_iterator rhs)
	    -> bool {
		return lhs.it == rhs.it;
	}
	constexpr friend auto operator!=(enumerate_iterator lhs, enumerate_iterator rhs)
	    -> bool {
		return lhs.it != rhs.it;
	}
};

template <typename Range, typename = void>
struct enumerate_t;

/**
 * @brief
 *
 * @author Tobias Widlund, killerbee
 * @date 2018-2020
 * @copyright MIT license.
 */
template <typename Range>
struct enumerate_t<Range, void> {
	detail::no_dangle_t<Range> r;

	using range_t = typename std::remove_reference_t<Range>;
	using nested_iterator = decltype(r.begin());
	using nested_end_iterator = decltype(r.end());
	using iterator = enumerate_iterator<nested_iterator>;
	using end_iterator = enumerate_iterator<nested_end_iterator>;

	using nested_const_iterator = typename range_t::const_iterator;
	using const_iterator = enumerate_iterator<nested_const_iterator>;

	constexpr auto begin() const& noexcept(noexcept(r.cbegin())) -> const_iterator {
		return {r.cbegin(), 0};
	}
	constexpr auto begin() & noexcept(noexcept(r.begin())) -> iterator {
		return {r.begin(), 0};
	}

	constexpr auto end() const& noexcept(noexcept(r.cend())) -> const_iterator {
		return {r.cend(), -std::size_t{1}};
	}
	constexpr auto end() & noexcept(noexcept(r.end())) -> end_iterator {
		return {r.end(), -std::size_t{1}};
	}
};

/**
 * @brief
 *
 * @author Tobias Widlund, killerbee
 * @date 2018-2020
 * @copyright MIT license.
 */
template <typename It, typename EndIt>
struct enumerate_t {
	using nested_iterator = It;
	using iterator = enumerate_iterator<nested_iterator>;
	using end_iterator = enumerate_iterator<EndIt>;

	constexpr auto begin() const& noexcept -> iterator { return {r_begin, 0}; }

	constexpr auto end() const& noexcept -> end_iterator {
		return {r_end, -std::size_t{1}};
	}

	It r_begin;
	EndIt r_end;
};

/**
 * @brief Allow access to indexes while using range-based for loops. Safe to use
 * with rvalues.
 *
 * @param r A range to iterate over.
 */
template <typename Range>
constexpr auto enumerate(Range&& r) -> enumerate_t<Range&&> {
	return {std::forward<Range>(r)};
}

/**
 * @brief Allow access to indexes while using range-based for loops.
 *
 * @param begin,end The input range.
 */
template <typename It, typename EIt>
constexpr auto enumerate(It begin, EIt end) -> enumerate_t<It, EIt> {
	return {begin, end};
}

} // namespace kblib

#endif // ENUMERATECONTRIBTW_H
