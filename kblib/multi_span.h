#ifndef MULTI_SPAN_H
#define MULTI_SPAN_H

#if KBLIB_USE_CXX17

#include "tdecl.h"

#include <initializer_list>
#include <optional>
#include <ostream>
#include <type_traits>
#include <utility>

#include <boost/iterator.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <gsl/gsl>

namespace kblib {

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
	mulspan_iterator(const multi_span<T>& s) : parent(&s), index(0) {
		recalculate_cache();
	}
	mulspan_iterator(const multi_span<T>& s, std::ptrdiff_t i)
	    : parent(&s), index(i) {}

 private:
	mulspan_iterator(const multi_span<T>& s, std::ptrdiff_t i,
	                 typename std::vector<subspan_t<T>>::const_iterator r,
	                 typename gsl::span<T>::iterator p)
	    : parent(&s), index(i), pos_cache(cached_iterator{r, p}) {}

 public:
	template <typename U, typename = std::enable_if_t<
	                          std::is_convertible_v<U (*)[], T (*)[]>>>
	mulspan_iterator(const mulspan_iterator<U>&);
	template <typename U, typename = std::enable_if_t<
	                          std::is_convertible_v<U (*)[], T (*)[]>>>
	mulspan_iterator(mulspan_iterator<U>&&);

 private:
	friend class boost::iterator_core_access;

	void recalculate_cache() const {
		if (!pos_cache) {
			if (index == 0) {
				pos_cache = {parent->spans.begin(),
				             parent->spans.front().second.begin()};
			} else if (index == parent->size()) {
				pos_cache = {parent->spans.end(),
				             parent->spans.back().second.begin()};
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
	T& dereference() const noexcept {
		recalculate_cache();
		assert(index != parent->size());
		return *pos_cache.value().pos;
	}
	template <typename U,
	          typename = decltype(std::declval<T*>() == std::declval<U*>())>
	bool equal(const mulspan_iterator<U>& o) const noexcept {
		return
		    // if both *this and o have caches,
		    /*(pos_cache && o.pos_cache)
			 //compare them
			 ? pos_cache.value() == o.pos_cache.value()
			 //else, compare parent and index
			 :*/
		    std::tie(parent, index) == std::tie(o.parent, o.index);
	}
	void increment() noexcept {
		++index;
		if (pos_cache) {
			++pos_cache.value().pos;
			if (pos_cache.value().pos == pos_cache.value().subs->second.end()) {
				++pos_cache.value().subs;
				pos_cache.value().pos = pos_cache.value().subs->second.begin();
			}
		}
	}
	void decrement() noexcept {
		--index;
		if (pos_cache) {
			if (pos_cache.value().pos == pos_cache.value().subs->second.begin()) {
				--pos_cache.value().subs;
				pos_cache.value().pos = pos_cache.value().subs->second.end();
			} else {
				--pos_cache.value().pos;
			}
		}
	}
	void advance(std::ptrdiff_t delta) noexcept {
		index += delta;
		pos_cache = std::nullopt;
	}
	// enabled if T* is comparable with U*
	template <typename U,
	          typename = decltype(std::declval<T*>() == std::declval<U*>())>
	std::ptrdiff_t distance_to(mulspan_iterator<U> o) const noexcept {
		return index - o.index;
	}

	const multi_span<T>* parent;
	std::ptrdiff_t index;
	struct cached_iterator {
		typename std::vector<multi_impl::subspan_t<T>>::const_iterator subs;
		typename gsl::span<T>::iterator pos;
		bool operator==(const cached_iterator& o) {
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

	multi_span() noexcept : spans{{0, {}}} {}
	template <typename U, typename = std::enable_if_t<
	                          std::is_convertible_v<U (*)[], T (*)[]>>>
	multi_span(gsl::span<U> o) noexcept : spans{{0, o}, {o.size(), {}}} {}
	template <typename U, typename = std::enable_if_t<
	                          std::is_convertible_v<U (*)[], T (*)[]>>>
	multi_span(std::initializer_list<gsl::span<U>> i_spans) noexcept {
		index_type c =
		    std::accumulate(i_spans.begin(), i_spans.end(), index_type{0},
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

	template <typename U, typename = std::enable_if_t<
	                          std::is_convertible_v<U (*)[], T (*)[]>>>
	multi_span(const multi_span<U>&);

	multi_span(const multi_span<T>&) = default;
	multi_span(multi_span<T>&&) = default;

	multi_span<T>& operator=(const multi_span<T>&) = default;
	multi_span<T>& operator=(multi_span<T>&&) = default;

	iterator begin() noexcept { return {*this}; }
	const_iterator begin() const noexcept { return {*this}; }
	const_iterator cbegin() const noexcept { return {*this}; }

	iterator end() noexcept {
		return {*this, size(), spans.end(), spans.back().second.begin()};
	}
	const_iterator end() const noexcept {
		return {*this, size(), spans.end(), spans.back().second.begin()};
	}
	const_iterator cend() const noexcept {
		return {*this, size(), spans.end(), spans.back().second.begin()};
	}

	reverse_iterator rbegin() noexcept {
		return iterator{*this, size(), spans.end(), spans.back().second.begin()};
	}
	const_reverse_iterator rbegin() const noexcept {
		return iterator{*this, size(), spans.end(), spans.back().second.begin()};
	}
	const_reverse_iterator crbegin() const noexcept {
		return iterator{*this, size(), spans.end(), spans.back().second.begin()};
	}

	reverse_iterator rend() noexcept { return iterator{*this}; }
	const_reverse_iterator rend() const noexcept { return iterator{*this}; }
	const_reverse_iterator crend() const noexcept { return iterator{*this}; }

	reference operator[](index_type i) const { return *iterator{*this, i}; }

	// see invariant on spans
	index_type size() const noexcept { return spans.back().first; }
	bool empty() const noexcept { return spans.back().first == 0; }

	void diag(std::ostream& os) const noexcept {
		os << "Diagnostics: " << spans.size() << '\n';
		for (auto& s : spans) {
			os << &s << '\t' << s.first << ':' << s.second.size() << '\t';
			os << s.second.data() << ','
			          << s.second.data() + s.second.size() << '\n';
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
	//invariant: holds one extra value, {size, {}}
	std::vector<multi_impl::subspan_t<T>> spans;
};

} // namespace kblib

#endif

#endif // MULTI_SPAN_H
