/* *****************************************************************************
 * Copyright (c) 2020 Krystian Stasiowski
 *
 * This code adapted from code written by Krystian Stasiowski
 * <sdkrystian@gmail.com>
 *
 * No specific license provisions were given, however permission was granted for
 * me to include it in kblib.
 *
 * His code is much faster and cleaner than my magic_enumerate is. HOWEVER, it
 * is fundamentally unable to detect the copying-nonconst-from-const case.
 *
 * My modifications are:
 * - Change from [val, idx] to [idx, val] to match Python's enumerate()
 * - Wrote documentation
 * - Change name from value_and_index() to cry_enumerate()
 * - Silenced Clang warnings about std::tuple_element specializations with
 *   mismatched tags.
 * - Formatting
 *
 * All credit for everything else goes to Krystian.
 *
 * ****************************************************************************/

#ifndef ENUMERATECONTRIB_H
#define ENUMERATECONTRIB_H
#include "tdecl.h"

#include <memory>
#include <type_traits>

#if KBLIB_USE_CXX17

namespace kblib {
/**
 * @internal
 */
namespace detail_cry {
	template <typename>
	struct value_index_pair;

	template <std::size_t N, typename T, std::enable_if_t<N == 0>* = nullptr>
	auto get(T&& t)
	    -> std::conditional_t<std::is_reference_v<T>, const std::size_t&,
	                          const std::size_t> {
		return t.index;
	}

	template <std::size_t N, typename T, std::enable_if_t<N == 1>* = nullptr>
	auto get(T&& t)
	    -> std::conditional_t<std::is_reference_v<T>,
	                          typename std::remove_reference_t<T>::value_type&,
	                          typename std::remove_reference_t<T>::value_type> {
		// static_assert(std::is_reference_v<T>);
		//	static_assert(
		//	    std::is_const_v<std::remove_reference_t<decltype(*t.iter)>>);
		return *t.iter;
	}

	template <typename Iterator>
	struct value_index_pair {
		using value_type
		    = std::remove_reference_t<decltype(*std::declval<Iterator&>())>;

		std::size_t index;
		Iterator iter;
	};

	template <typename Range, typename = void>
	struct value_and_index_base {
	 public:
		using iterator_type = decltype(std::begin(std::declval<Range&>()));

		value_and_index_base(Range& range)
		    : range_begin_(std::begin(range))
		    , range_end_(std::end(range)) {}

		auto range_begin() -> iterator_type { return range_begin_; }

		auto range_end() -> iterator_type { return range_end_; }

		iterator_type range_begin_;
		iterator_type range_end_;
	};

	template <typename Range>
	struct value_and_index_base<
	    Range, std::enable_if_t<not std::is_reference_v<Range>>> {
	 public:
		using iterator_type = decltype(std::begin(std::declval<Range&>()));

		value_and_index_base(Range& range)
		    : range_(std::move(range)) {}

		auto range_begin() -> iterator_type { return std::begin(range_); }

		auto range_end() -> iterator_type { return std::end(range_); }

		Range range_;
	};

	template <typename Range>
	struct value_and_index_impl : value_and_index_base<Range> {
		using iterator_type = typename value_and_index_base<Range>::iterator_type;

		value_and_index_impl(Range& range)
		    : value_and_index_base<Range>(range)
		    , begin_(this->range_begin(), 0)
		    , end_(this->range_end(), 0) {}

		struct iterator {
		 private:
			value_index_pair<iterator_type> pair_;

		 public:
			iterator(iterator_type iter, std::size_t index = 0)
			    : pair_{index, iter} {}

			auto operator*() -> value_index_pair<iterator_type>& { return pair_; }

			auto operator++(int) -> iterator {
				iterator copy(pair_.iter, pair_.index);
				++pair_.iter, ++pair_.index;
				return copy;
			}

			auto operator++() -> iterator& {
				++pair_.iter, ++pair_.index;
				return *this;
			}

			auto operator==(const iterator& other) const -> bool {
				return other.pair_.iter == pair_.iter;
			}

			auto operator!=(const iterator& other) const -> bool {
				return not (other == *this);
			}
		};

		auto begin() -> iterator { return begin_; }

		auto end() -> iterator { return end_; }

	 private:
		iterator begin_;
		iterator end_;
	};
} // namespace detail_cry
} // namespace kblib
namespace std {
#	if defined(__clang__)
// Fix from: https://github.com/nlohmann/json/issues/1401
#		pragma clang diagnostic push
#		pragma clang diagnostic ignored "-Wmismatched-tags"
#	endif
template <typename T>
struct tuple_size<kblib::detail_cry::value_index_pair<T>> {
	static constexpr std::size_t value = 2;
};

template <typename T>
struct tuple_element<0, kblib::detail_cry::value_index_pair<T>> {
	using type = const std::size_t;
};

template <typename T>
struct tuple_element<1, kblib::detail_cry::value_index_pair<T>> {
	using type = std::remove_reference_t<decltype(*std::declval<T&>())>;
};

#	if defined(__clang__)
#		pragma clang diagnostic pop
#	endif
} // namespace std

namespace kblib {

/**
 * @brief
 *
 * @author Krystian Stasiowski, killerbee
 * @date 2020
 */
template <typename Range>
auto cry_enumerate(Range&& range) -> auto {
	return detail_cry::value_and_index_impl<Range>(range);
}

} // namespace kblib

#endif

#endif // ENUMERATECONTRIB_H
