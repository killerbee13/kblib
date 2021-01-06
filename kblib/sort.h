#ifndef SORT_H
#define SORT_H

#include "algorithm.h"
#include "bits.h"
#include "fakestd.h"
#include "simple.h"

#include <bitset>

namespace kblib {

/**
 * @brief In-place insertion sort. As is usual, it is stable.
 * Provides as a guarantee that it will perform no moves on sorted input.
 *
 * @remark Complexity:
 * @remark Average case O(n^2)
 * @remark Best-case Θ(n) (for sorted input)
 * @remark worst-case O(n^2) (for reverse-sorted input)
 *
 * @param begin,end The range to sort
 * @param compare The comparison predicate
 */
template <typename RandomAccessIt, typename Compare = std::less<>>
constexpr void insertion_sort(
    const RandomAccessIt begin, const RandomAccessIt end,
    Compare&& compare =
        {}) noexcept(noexcept(swap(*begin,
                                   *begin)) and noexcept(compare(*begin,
                                                                 *begin))) {
	// Trivial inputs are trivially already sorted.
	if (end - begin <= 1) {
		return;
	}
	for (auto pos = begin + 1; pos != end; ++pos) {
		// This search is linear, not binary, because insertion_sort is meant
		// to never be used for arrays large enough for binary search.
		auto index = kblib::find_if(begin, pos, [&compare, pos](const auto& a) {
			return compare(*pos, a);
		});
		kblib::rotate(index, pos, pos + 1);
	}
	return;
}

/**
 * @brief Out-of-place insertion sort, which does not modify the input.
 * Provides as a guarantee that it will perform no moves on sorted input.
 *
 * @remark Complexity:
 * @remark Average case O(n^2)
 * @remark Best-case Θ(n) (for sorted input)
 * @remark worst-case O(n^2) (for reverse-sorted input)
 *
 * @param begin,end The input range
 * @param d_begin,d_end The output range
 * @param compare The comparison predicate
 */
template <typename RandomAccessIt, typename RandomAccessIt2,
          typename Compare = std::less<>>
constexpr void insertion_sort_copy(
    const RandomAccessIt begin, const RandomAccessIt end,
    const RandomAccessIt2 d_begin, const RandomAccessIt2 d_end,
    Compare&& compare =
        {}) noexcept(noexcept(detail::
                                  shift_backward(
                                      d_begin, d_begin,
                                      d_end)) and noexcept(*d_begin = *begin)) {
	const auto dist = end - begin;
	assert(end - begin == d_end - d_begin);
	if (dist == 0) {
		return;
	} else if (dist == 1) {
		// ranges of 1 are trivial to sort
		*d_begin = *begin;
		return;
	} else if (dist == 2) {
		// Special case distance=2 to perform no swaps, and because the loop
		// for the general case assumes 3 or more elements.
		if (compare(*begin, *(begin + 1))) {
			*d_begin = *begin;
			*(d_begin + 1) = *(begin + 1);
			return;
		} else {
			*d_begin = *(begin + 1);
			*(d_begin + 1) = *begin;
		}
	} else {
		// This loop writes in reverse because shifting backwards can be done
		// in a forward pass, making it simpler than a forward shift, and is
		// sufficient for the algorithm, and reads in reverse to avoid worst-
		// case complexity for sorted inputs, given that it writes in reverse.
		auto read = std::prev(end);
		auto write = std::prev(d_end);
		// push first element to get the loop invariant
		*write = *read;
		do {
			--read, --write;
			// This search is linear, not binary, because insertion_sort_copy
			// is meant to never be used for arrays large enough for binary
			// search.
#if 1
			auto index =
			    kblib::find_if(write + 1, d_end, [&compare, read](const auto& a) {
				    return not compare(a, *read);
			    });
#else
			auto index =
			    kblib::find_if(write + 1, d_end, [&compare, read](const auto& a) {
				    if (stable) {
					    // find first element greater than
					    // *read
					    return compare(*read, a);
				    } else {
					    // find first element greater than
					    // or equal to *read
					    return !compare(a, *read);
				    }
			    });
#endif
			detail::shift_backward(write, write + 1, index);
			*(index - 1) = *read;
		} while (write != d_begin);
		return;
	}
}

/**
 * @brief An adaptive sort which, at the cost of some additional work (time
 * complexity Θ(sqrt(n))), avoids quadratic behavior for reverse-sorted
 * inputs (and, in fact, handles them in optimal time). It's still possible
 * to fool it, but it requires a staggered input, which is a highly unlikely
 * shape for random data.
 *
 * @remark Complexity:
 * @remark Average case O(n^2)
 * @remark Best-case Θ(n + sqrt(n)) (for sorted input)
 * @remark worst-case O(n^2) (for reverse-sorted input)
 *
 * @param begin,end The input range
 * @param d_begin,d_end The output range
 * @param compare The comparison predicate
 */
template <typename RandomAccessIt, typename RandomAccessIt2,
          typename Compare = std::less<>>
constexpr void adaptive_insertion_sort_copy(
    const RandomAccessIt begin, const RandomAccessIt end,
    const RandomAccessIt2 d_begin, const RandomAccessIt2 d_end,
    Compare&& compare =
        {}) noexcept(noexcept(detail::
                                  shift_backward(
                                      d_begin, d_begin,
                                      d_end)) and noexcept(*d_begin = *begin)) {
	const auto dist = end - begin;
	const auto scan_end = begin + static_cast<std::size_t>(std::sqrt(dist));
	// For trivial inputs, don't bother doing anything
	if (dist < 3) {
		insertion_sort_copy(begin, end, d_begin, d_end,
		                    std::forward<Compare>(compare));
	}
	// A very rudimentary way of estimating the sortedness of the input, by
	// counting the relative number of sorted and unsorted adjacent pairs.
	std::make_signed<std::size_t>::type dir{};
	for (auto pos = begin; pos != scan_end - 1; ++pos) {
		if (compare(*pos, *(pos + 1))) {
			++dir;
		} else if (compare(*(pos + 1), *pos)) {
			--dir;
		}
	}
	if (dir >= 0) {
		insertion_sort_copy(begin, end, d_begin, d_end,
		                    std::forward<Compare>(compare));
	} else {
		// If the input is predominantly in reverse order, sort it in reverse
		// IMPORTANT: the reverse iterators go end, begin
		insertion_sort_copy(std::make_reverse_iterator(end),
		                    std::make_reverse_iterator(begin), d_begin, d_end,
		                    std::forward<Compare>(compare));
	}
}

namespace detail {

	template <typename T, typename = void>
	struct is_radix_sortable : std::false_type {};

	template <typename T>
	struct is_radix_sortable<T, void_if_t<std::is_integral<T>::value>>
	    : std::true_type {};

	template <typename T>
	struct is_radix_sortable<T, void_if_t<std::is_enum<T>::value>>
	    : std::true_type {};

	template <std::size_t B>
	struct is_radix_sortable<std::bitset<B>, void> : std::true_type {};

	template <typename T>
	struct is_radix_sortable<
	    T, void_if_t<is_linear_container_v<T> and
	                 std::is_integral<typename T::value_type>::value>>
	    : std::true_type {};

	template <typename T>
	constexpr bool is_radix_sortable_v = is_radix_sortable<T>::value;

	template <typename T>
	constexpr bool is_byte_v =
	    std::is_same<typename std::remove_cv<T>::type, std::byte>::value;

	template <typename T>
	constexpr auto byte_count(T) noexcept
	    -> enable_if_t<std::is_integral<T>::value, std::size_t> {
		auto res =
		    kblib::div(kblib::bits_of<T> + std::is_signed<T>::value, CHAR_BIT);
		return to_unsigned(res.quot + (res.rem != 0));
	}
	template <typename T>
	constexpr auto byte_count(T) noexcept
	    -> enable_if_t<std::is_enum<T>::value, std::size_t> {
		using U = typename std::underlying_type<T>::type;
		auto res =
		    kblib::div(kblib::bits_of<U> + std::is_signed<U>::value, CHAR_BIT);
		return to_unsigned(res.quot + (res.rem != 0));
	}
	template <typename T>
	constexpr std::size_t byte_count(T*) noexcept {
		return byte_count(std::uintptr_t{});
	}
	template <typename T>
	constexpr std::size_t byte_count(const std::unique_ptr<T>&) noexcept {
		return byte_count(std::uintptr_t{});
	}
	template <typename T>
	constexpr auto byte_count(const T& x) noexcept
	    -> enable_if_t<is_linear_container_v<T> and
	                       (std::is_integral<typename T::value_type>::value or
	                        is_byte_v<T>) and
	                       sizeof(typename T::value_type) == 1,
	                   std::size_t> {
		return std::size(x);
	}
	template <typename T>
	constexpr auto byte_count(const T& x) noexcept
	    -> enable_if_t<is_linear_container_v<T> and
	                       std::is_integral<typename T::value_type>::value and
	                       (sizeof(typename T::value_type) > 1),
	                   std::size_t> {
		using value_type = typename T::value_type;
		return std::size(x) * byte_count(value_type{});
	}

	// Can be used to detect unsupported types with overload resolution.
	constexpr void byte_count(...) noexcept { return; }

	template <typename T>
	constexpr auto get_byte_index(T x, std::size_t idx) noexcept
	    -> enable_if_t<std::is_integral<T>::value, unsigned char> {
		return static_cast<unsigned char>(x >> (idx * CHAR_BIT));
	}
	template <typename T>
	constexpr auto get_byte_index(const T& x, std::size_t idx) noexcept
	    -> enable_if_t<std::is_enum<T>::value, unsigned char> {
		return static_cast<unsigned char>(etoi(x) >> (idx * CHAR_BIT));
	}
	template <typename T>
	auto get_byte_index(T* x, std::size_t idx) noexcept -> unsigned char {
		return get_byte_index(reinterpret_cast<std::uintptr_t>(x), idx);
	}
	template <typename T>
	auto get_byte_index(const std::unique_ptr<T>& x, std::size_t idx) noexcept
	    -> unsigned char {
		return get_byte_index(reinterpret_cast<std::uintptr_t>(x.get()), idx);
	}
	template <typename T>
	constexpr auto get_byte_index(const T& x, std::size_t idx) noexcept
	    -> enable_if_t<is_linear_container_v<T> and
	                       (std::is_integral<typename T::value_type>::value or
	                        is_byte_v<T>) and
	                       sizeof(typename T::value_type) == 1,
	                   unsigned char> {
		return static_cast<unsigned char>(x[idx]);
	}
	template <typename T>
	constexpr auto get_byte_index(const T& x, std::size_t idx) noexcept
	    -> enable_if_t<is_linear_container_v<T> and
	                       std::is_integral<typename T::value_type>::value and
	                       (sizeof(typename T::value_type) > 1),
	                   unsigned char> {
		using value_type = typename T::value_type;
		auto bytes_per = byte_count(value_type{});
		return static_cast<unsigned char>(to_unsigned(x[idx / bytes_per]) >>
		                                  (idx % bytes_per * CHAR_BIT));
	}
	template <typename T>
	constexpr auto get_byte_index(const T& x, std::size_t idx) noexcept
	    -> enable_if_t<is_linear_container_v<T> and
	                       std::is_enum<typename T::value_type>::value,
	                   unsigned char> {
		using U = typename std::underlying_type<typename T::U>::type;
		auto bytes_per = byte_count(U{});
		return static_cast<unsigned char>(to_unsigned(etoi(x[idx / bytes_per])) >>
		                                  (idx % bytes_per * CHAR_BIT));
	}

	/**
	 * @brief Sort data after applying an arbitrary transformation to it. The
	 * primary template handles the general case of arbitrary transformation
	 * and arbitrary compare predicate.
	 */
	template <typename RandomAccessIt, typename UnaryOperation,
	          typename BinaryPredicate, typename SortKey,
	          bool = std::is_member_object_pointer<UnaryOperation>::value,
	          bool = std::is_fundamental<SortKey>::value,
	          bool = std::is_integral<SortKey>::value>
	struct sort_transform_impl {
		static constexpr void inplace(RandomAccessIt begin,
		                              const RandomAccessIt end,
		                              UnaryOperation&& transform,
		                              BinaryPredicate&& compare) {
			auto comp = [&compare, &transform](RandomAccessIt a,
			                                   RandomAccessIt b) {
				return kblib::invoke(compare, kblib::invoke(transform, *a),
				                     kblib::invoke(transform, *b));
			};
			if (end - begin < 8) {
				insertion_sort(begin, end, comp);
				return;
			} else {
				/// TODO: write efficient inplace sort_transform
			}
		}

		static constexpr void scratch(RandomAccessIt begin,
		                              const RandomAccessIt end,
		                              UnaryOperation&& transform,
		                              BinaryPredicate&& compare) {
			auto comp = [&compare, &transform](RandomAccessIt a,
			                                   RandomAccessIt b) {
				return kblib::invoke(compare, kblib::invoke(transform, *a),
				                     kblib::invoke(transform, *b));
			};
			if (end - begin < 8) {
				insertion_sort(begin, end, comp);
				return;
			} else {
				/// TODO: write efficient inplace sort_transform
			}
		}

		template <typename RandomAccessIt2>
		static constexpr void copy(RandomAccessIt begin, const RandomAccessIt end,
		                           RandomAccessIt2 d_begin, RandomAccessIt2 d_end,
		                           UnaryOperation&& transform,
		                           BinaryPredicate&& compare) {
			auto comp = [&compare, &transform](RandomAccessIt a,
			                                   RandomAccessIt b) {
				return kblib::invoke(compare, kblib::invoke(transform, *a),
				                     kblib::invoke(transform, *b));
			};
			if (end - begin < 8) {
				insertion_sort_copy(begin, end, d_begin, d_end, comp);
				return;
			} else {
				/// TODO: write efficent sort_transform_copy
			}
		}
	};

#if 0
	/**
	 * @brief Sort implementation for pointer to member object of non-fundamental
	 * type, so sort keys are constant time to extract (this is most similar to
	 * a general sort())
	 */
	template <typename RandomAccessIt, typename UnaryOperation,
	          typename BinaryPredicate, typename SortKey>
	struct sort_transform_impl<RandomAccessIt, UnaryOperation, BinaryPredicate,
	                            SortKey,true, false, false> {
		static constexpr void inplace(RandomAccessIt begin,
		                              const RandomAccessIt end,
		                              UnaryOperation&& transform,
		                              BinaryPredicate&& compare) {
			auto comp = [&compare, &transform](RandomAccessIt a,
			            RandomAccessIt b) {
				return kblib::invoke(compare,(*a).*transform, (*b).*transform);
			};
		}
	};
#endif

	/**
	 * @brief Sort implementation for pointer to member object of fundamental
	 * non-integral type with default sorting, so sort keys are constant time
	 * to extract and compare
	 */
	template <typename RandomAccessIt, typename UnaryOperation, typename LessT,
	          typename SortKey>
	struct sort_transform_impl<RandomAccessIt, UnaryOperation, std::less<LessT>,
	                           SortKey, true, true, false> {
		static constexpr void inplace(KBLIB_UNUSED RandomAccessIt begin,
		                              KBLIB_UNUSED const RandomAccessIt end,
		                              KBLIB_UNUSED UnaryOperation&& transform,
		                              KBLIB_UNUSED std::less<LessT>&& compare) {
			/// TODO: write efficient inplace sort
		}
	};
	/**
	 * @brief Sort implementation for pointer to member object of fundamental
	 * non-integral type with reverse sorting, so sort keys are constant time
	 * to extract and compare
	 */
	template <typename RandomAccessIt, typename UnaryOperation, typename LessT,
	          typename SortKey>
	struct sort_transform_impl<RandomAccessIt, UnaryOperation,
	                           std::greater<LessT>, SortKey, true, true, false> {
		static constexpr void
		inplace(KBLIB_UNUSED RandomAccessIt begin,
		        KBLIB_UNUSED const RandomAccessIt end,
		        KBLIB_UNUSED UnaryOperation&& transform,
		        KBLIB_UNUSED std::greater<LessT>&& compare) {
			/// TODO: write efficient inplace sort_transform
		}
	};

	/**
	 * @brief Sort implementation for pointer to member object of integral
	 * type with default sorting, so we can do radix sort
	 */
	template <typename RandomAccessIt, typename UnaryOperation, typename LessT,
	          typename SortKey>
	struct sort_transform_impl<RandomAccessIt, UnaryOperation, std::less<LessT>,
	                           SortKey, true, true, true> {
		static constexpr void inplace(KBLIB_UNUSED RandomAccessIt begin,
		                              KBLIB_UNUSED const RandomAccessIt end,
		                              KBLIB_UNUSED UnaryOperation&& transform,
		                              KBLIB_UNUSED std::less<LessT>&& compare) {
			/// TODO: write efficient inplace sort_transform
		}
	};
	/**
	 * @brief Sort implementation for pointer to member object of integral
	 * type with reverse sorting, so we can do radix sort
	 */
	template <typename RandomAccessIt, typename UnaryOperation, typename LessT,
	          typename SortKey>
	struct sort_transform_impl<RandomAccessIt, UnaryOperation,
	                           std::greater<LessT>, SortKey, true, true, true> {
		static constexpr void
		inplace(KBLIB_UNUSED RandomAccessIt begin,
		        KBLIB_UNUSED const RandomAccessIt end,
		        KBLIB_UNUSED UnaryOperation&& transform,
		        KBLIB_UNUSED std::greater<LessT>&& compare) {
			/// TODO: write efficient sort_transform
		}
	};

#if 0 // Do string radix sorting
	/**
	 * @brief Sort implementation for pointer to member object of string
	 * type with default sorting, so we can do radix sort
	 */
	template <typename RandomAccessIt, typename UnaryOperation, typename LessT,
	          typename CharT>
	struct sort_transform_impl<RandomAccessIt, UnaryOperation, std::less<LessT>,
	                           std::basic_string<CharT>, true, false, false> {
		static constexpr void inplace(RandomAccessIt begin,
		                              const RandomAccessIt end,
		                              UnaryOperation&& transform,
		           BinaryPredicate&& compare) {
			//TODO
		}
	};
	/**
	 * @brief Sort implementation for pointer to member object of string
	 * type with reverse sorting, so we can do radix sort
	 */
	template <typename RandomAccessIt, typename UnaryOperation, typename LessT,
	          typename CharT>
	struct sort_transform_impl<RandomAccessIt, UnaryOperation,
	                           std::greater<LessT>, std::basic_string<CharT>,
	                           true, false, false> {
		static constexpr void inplace(RandomAccessIt begin,
		                              const RandomAccessIt end,
		                              UnaryOperation&& transform,
		           BinaryPredicate&& compare) {
			//TODO
		}
	};
#endif
} // namespace detail

/**
 * @brief Sorts a range after applying a transformation.
 *
 * Complexity: worst-case O(N log(N)), where N = std::distance(begin, end)
 * comparisons and swaps.
 *
 * @param begin,end The range to sort
 * @param transform A transformer (such as unary function or pointer to
 * member) which will be applied to each object before comparing it. A
 * transformer may not modify the object it is called with.
 *
 * @param compare A comparison predicate which returns true if the first
 * argument shall be ordered before the second. BinaryPredicate must meet the
 * requirements of the Compare named requirement.
 */
template <typename RandomAccessIt, typename UnaryOperation,
          typename BinaryPredicate>
constexpr void sort_transform(RandomAccessIt begin, RandomAccessIt end,
                              UnaryOperation&& transform,
                              BinaryPredicate&& compare) {
	detail::sort_transform_impl<RandomAccessIt, UnaryOperation, BinaryPredicate,
	                            decltype(kblib::invoke(transform, *begin))>::
	    inplace(begin, end, std::forward<UnaryOperation>(transform),
	            std::forward<BinaryPredicate>(compare));
}

/**
 * @brief
 *
 * @param begin,end The range to sort
 * @param transform The transformation to apply
 */
template <typename RandomAccessIt, typename UnaryOperation>
constexpr void sort_transform(RandomAccessIt begin, RandomAccessIt end,
                              UnaryOperation&& transform) {
	detail::sort_transform_impl<RandomAccessIt, UnaryOperation, std::less<>,
	                            decltype(kblib::invoke(transform, *begin))>::
	    inplace(begin, end, std::forward<UnaryOperation>(transform),
	            std::less<>{});
}

} // namespace kblib

#endif // SORT_H