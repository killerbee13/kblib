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
 * @brief Provides multi_span.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef MULTI_SPAN_H
#define MULTI_SPAN_H

#include "tdecl.h"

#if KBLIB_USE_CXX17

#	include <initializer_list>
#	include <optional>
#	include <ostream>
#	include <type_traits>
#	include <utility>

#	include <boost/iterator.hpp>
#	include <boost/iterator/iterator_facade.hpp>
#	include <gsl/gsl>

namespace KBLIB_NS {

template <typename T>
class multi_span;

namespace multi_impl {

	template <typename T>
	using subspan_t = std::pair<std::ptrdiff_t, gsl::span<T>>;

	template <typename T>
	class mulspan_iterator
	    : public boost::iterator_facade<mulspan_iterator<T>, T,
	                                    std::bidirectional_iterator_tag> {
	 public:
		mulspan_iterator() = default;
		mulspan_iterator(const multi_span<T>& s)
		    : parent(&s) {
			recalculate_cache();
		}
		mulspan_iterator(const multi_span<T>& s, std::ptrdiff_t i)
		    : parent(&s)
		    , index(i) {}

	 private:
		mulspan_iterator(const multi_span<T>& s, std::ptrdiff_t i,
		                 typename std::vector<subspan_t<T>>::const_iterator r,
		                 typename gsl::span<T>::iterator p)
		    : parent(&s)
		    , index(i)
		    , pos_cache(cached_iterator{r, p}) {}

	 public:
		template <typename U,
		          typename
		          = std::enable_if_t<std::is_convertible_v<U (*)[], T (*)[]>>>
		mulspan_iterator(const mulspan_iterator<U>&);
		template <typename U,
		          typename
		          = std::enable_if_t<std::is_convertible_v<U (*)[], T (*)[]>>>
		mulspan_iterator(mulspan_iterator<U>&&);

	 private:
		friend class boost::iterator_core_access;

		auto recalculate_cache() const -> void {
			if (! pos_cache) {
				if (index == 0) {
					pos_cache = {parent->spans.begin(),
					             parent->spans.front().second.begin()};
				} else if (index == parent->size()) {
					pos_cache
					    = {parent->spans.end(), parent->spans.back().second.begin()};
				} else if (auto it = std::prev(parent->spans.end());
				           index >= it->first) {
					pos_cache = {it, it->second.begin() + (index - it->first)};
				} else {
					// get the subspan before the first subspan starting after index
					it = std::prev(std::upper_bound(
					    parent->spans.begin(), parent->spans.end(), index,
					    [](std::ptrdiff_t l, const subspan_t<T>& r) {
						    return l < r.first;
					    }));
					// calculate delta into that subspan
					pos_cache = {it, it->second.begin() + (index - it->first)};
				}
			}
		}
		auto dereference() const noexcept -> T& {
			recalculate_cache();
			assert(index != parent->size());
			return *pos_cache.value().pos;
		}
		template <typename U,
		          typename = decltype(std::declval<T*>() == std::declval<U*>())>
		auto equal(const mulspan_iterator<U>& o) const noexcept -> bool {
			return
			    // if both *this and o have caches,
			    /*(pos_cache and o.pos_cache)
			    //compare them
			    ? pos_cache.value() == o.pos_cache.value()
			    //else, compare parent and index
			    :*/
			    std::tie(parent, index) == std::tie(o.parent, o.index);
		}
		auto increment() noexcept -> void {
			++index;
			if (pos_cache) {
				++pos_cache.value().pos;
				if (pos_cache.value().pos == pos_cache.value().subs->second.end()) {
					++pos_cache.value().subs;
					pos_cache.value().pos = pos_cache.value().subs->second.begin();
				}
			}
		}
		auto decrement() noexcept -> void {
			--index;
			if (pos_cache) {
				if (pos_cache.value().pos
				    == pos_cache.value().subs->second.begin()) {
					--pos_cache.value().subs;
					pos_cache.value().pos = pos_cache.value().subs->second.end();
				} else {
					--pos_cache.value().pos;
				}
			}
		}
		auto advance(std::ptrdiff_t delta) noexcept -> void {
			index += delta;
			pos_cache = std::nullopt;
		}
		// enabled if T* is comparable with U*
		template <typename U,
		          typename = decltype(std::declval<T*>() == std::declval<U*>())>
		auto distance_to(mulspan_iterator<U> o) const noexcept -> std::ptrdiff_t {
			return index - o.index;
		}

		const multi_span<T>* parent{};
		std::ptrdiff_t index{};
		struct cached_iterator {
			typename std::vector<multi_impl::subspan_t<T>>::const_iterator subs;
			typename gsl::span<T>::iterator pos;
			auto operator==(const cached_iterator& o) const noexcept -> bool {
				return std::tie(subs, pos) == std::tie(o.subs, o.pos);
			}
		};
		mutable std::optional<cached_iterator> pos_cache = std::nullopt;

		friend class multi_span<T>;
	};

} // namespace multi_impl

template <typename T>
class multi_span {
 public:
	using element_type = T;
	using value_type = std::remove_cv<T>;
	using index_type = std::ptrdiff_t;
	using difference_type = std::ptrdiff_t;
	using pointer = T*;
	using reference = T&;
	using iterator = multi_impl::mulspan_iterator<T>;
	using const_iterator = multi_impl::mulspan_iterator<const T>;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

	multi_span() noexcept
	    : spans{{0, {}}} {}
	template <typename U,
	          typename
	          = std::enable_if_t<std::is_convertible_v<U (*)[], T (*)[]>>>
	multi_span(gsl::span<U> o) noexcept
	    : spans{{0, o}, {o.size(), {}}} {}
	template <typename U,
	          typename
	          = std::enable_if_t<std::is_convertible_v<U (*)[], T (*)[]>>>
	multi_span(std::initializer_list<gsl::span<U>> i_spans) noexcept {
		index_type c
		    = std::accumulate(i_spans.begin(), i_spans.end(), index_type{0},
		                      [this](int c, gsl::span<U> s) {
			                      spans.push_back({c, s});
			                      return c + s.size();
		                      });
		spans.push_back({c, {}});
	}
	template <typename Iterator,
	          typename = std::enable_if_t<std::is_convertible_v<
	              decltype(*std::declval<Iterator>()), gsl::span<T>>>>
	multi_span(Iterator begin, Iterator end) noexcept {
		index_type c = std::accumulate(begin, end, index_type{0},
		                               [this](int c, gsl::span<T> s) {
			                               spans.push_back({c, s});
			                               return c + s.size();
		                               });
		spans.push_back({c, {}});
	}

	template <typename U,
	          typename
	          = std::enable_if_t<std::is_convertible_v<U (*)[], T (*)[]>>>
	multi_span(const multi_span<U>&);

	multi_span(const multi_span&) = default;
	multi_span(multi_span&&) noexcept = default;

	auto operator=(const multi_span&) -> multi_span& = default;
	auto operator=(multi_span&&) noexcept -> multi_span& = default;

	~multi_span() = default;

	KBLIB_NODISCARD auto begin() noexcept -> iterator { return {*this}; }
	KBLIB_NODISCARD auto begin() const noexcept -> const_iterator {
		return {*this};
	}
	KBLIB_NODISCARD auto cbegin() const noexcept -> const_iterator {
		return {*this};
	}

	KBLIB_NODISCARD auto end() noexcept -> iterator {
		return {*this, size(), spans.end(), spans.back().second.begin()};
	}
	KBLIB_NODISCARD auto end() const noexcept -> const_iterator {
		return {*this, size(), spans.end(), spans.back().second.begin()};
	}
	KBLIB_NODISCARD auto cend() const noexcept -> const_iterator {
		return {*this, size(), spans.end(), spans.back().second.begin()};
	}

	KBLIB_NODISCARD auto rbegin() noexcept -> reverse_iterator {
		return iterator{*this, size(), spans.end(), spans.back().second.begin()};
	}
	KBLIB_NODISCARD auto rbegin() const noexcept -> const_reverse_iterator {
		return iterator{*this, size(), spans.end(), spans.back().second.begin()};
	}
	KBLIB_NODISCARD auto crbegin() const noexcept -> const_reverse_iterator {
		return iterator{*this, size(), spans.end(), spans.back().second.begin()};
	}

	KBLIB_NODISCARD auto rend() noexcept -> reverse_iterator {
		return iterator{*this};
	}
	KBLIB_NODISCARD auto rend() const noexcept -> const_reverse_iterator {
		return iterator{*this};
	}
	KBLIB_NODISCARD auto crend() const noexcept -> const_reverse_iterator {
		return iterator{*this};
	}

	KBLIB_NODISCARD reference operator[](index_type i) const {
		return *iterator{*this, i};
	}

	// see invariant on spans
	KBLIB_NODISCARD auto size() const noexcept -> index_type {
		return spans.back().first;
	}
	KBLIB_NODISCARD auto empty() const noexcept -> bool {
		return spans.back().first == 0;
	}

	auto diag(std::ostream& os) const noexcept -> void {
		os << "Diagnostics: " << spans.size() << '\n';
		for (auto& s : spans) {
			os << &s << '\t' << s.first << ':' << s.second.size() << '\t';
			os << s.second.data() << ',' << s.second.data() + s.second.size()
			   << '\n';
		}
	}

	// multi_span<T> first(index_type count) const
	// {

	// }
	// multi_span<T> last(index_type count) const
	// {

	// }
	// multi_span<T> subspan(index_type offset, index_type count = -1) const
	// {
	// if (count == -1) {
	// count = size() - offset;
	// }

	// }
 private:
	template <typename U>
	friend class multi_impl::mulspan_iterator;
	// invariant: holds one extra value, {size, {}}
	std::vector<multi_impl::subspan_t<T>> spans;
};

} // namespace KBLIB_NS

#endif

#endif // MULTI_SPAN_H
