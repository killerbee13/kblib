#ifndef ALGORITHM_H
#define ALGORITHM_H

#include "tdecl.h"

#include "iterators.h"
#include "traits.h"

#include <algorithm>
#include <tuple>

namespace kblib {


/**
 * @brief Abbreviation of the erase-remove idiom as a free function.
 *
 * @param c The container to erase from.
 * @param val The value to remove.
 */
template <typename C, typename T>
void erase(C& c, const T& val) {
	c.erase(std::remove(c.begin(), c.end(), val), c.end());
	return;
}

/**
 * @brief Abbreviation of the erase-remove idiom as a free function.
 *
 * @param c The container to erase from.
 * @param p Erase all elements on which p returns true.
 */
template <typename C, typename UnaryPredicate>
void erase_if(C& c, UnaryPredicate p) {
	c.erase(std::remove_if(c.begin(), c.end(), p), c.end());
	return;
}

/**
 * @brief Searches a range for the last occurence of a match, and returns an
 * iterator to it.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param v The value to find
 * @return It Iterator to the last element equal to v, or end if no such
 * element.
 */
template <typename It, typename T>
KBLIB_NODISCARD It find_last(It begin, It end, const T& v) {
	if (begin == end) {
		return end;
	}
	It result = end;
	while (true) {
		It new_result = std::find(begin, end, v);
		if (new_result == end) {
			break;
		} else {
			result = new_result;
			begin = result;
			++begin;
		}
	}
	return result;
}

/**
 * @brief Searches a range for the last element on which a predicate returns
 * true.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param p The predicate for comparison
 * @return It Iterator to the last element for which p returned true, or end if
 * no such element.
 */
template <typename It, typename Pred>
KBLIB_NODISCARD It find_last_if(It begin, It end, Pred p) {
	if (begin == end) {
		return end;
	}
	It result = end;
	while (true) {
		It new_result = std::find_if(begin, end, p);
		if (new_result == end) {
			break;
		} else {
			result = new_result;
			begin = result;
			++begin;
		}
	}
	return result;
}

/**
 * @brief Searches a range for the last element on which a predicate returns
 * false.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param p The predicate for comparison
 * @return It Iterator to the last element for which p returned false, or end if
 * no such element.
 */
template <typename It, typename Pred>
KBLIB_NODISCARD It find_last_if_not(It begin, It end, Pred p) {
	if (begin == end) {
		return end;
	}
	It result = end;
	while (true) {
		It new_result = std::find_if_not(begin, end, p);
		if (new_result == end) {
			break;
		} else {
			result = new_result;
			begin = result;
			++begin;
		}
	}
	return result;
}

/**
 * @brief Find the offset of the first ocurrence of v in a range from the
 * beginning.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param v The value to search for.
 * @return size_t The offset from begin of the first element equal to v, or
 * distance(begin, end) if not found.
 */
template <typename It, typename T>
KBLIB_NODISCARD size_t find_in(It begin, It end, const T& v) {
	return std::find(begin, end, v) - begin;
}

/**
 * @brief Find the offset of the first element for which p returns true.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param p The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_in_if(It begin, It end, UnaryPredicate p) {
	return std::find_if(begin, end, p) - begin;
}
/**
 * @brief Find the offset of the first element for which p returns false.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param p The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_in_if_not(It begin, It end, UnaryPredicate p) {
	return std::find_if_not(begin, end, p) - begin;
}

// find_last_in:
// 1. Finds last v in range [begin, end) and returns the offset from begin

/**
 * @brief Find the offset of the last ocurrence of v in a range from the
 * beginning.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param v The value to search for.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename It, typename T>
KBLIB_NODISCARD size_t find_last_in(It begin, It end, const T& v) {
	return kblib::find_last(begin, end, v) - begin;
}

/**
 * @brief Find the offset of the last element for which p returns true.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param p The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if(It begin, It end, UnaryPredicate p) {
	return kblib::find_last_if(begin, end, p) - begin;
}
/**
 * @brief Find the offset of the last element for which p returns false.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param p The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if_not(It begin, It end, UnaryPredicate p) {
	return kblib::find_last_if_not(begin, end, p) - begin;
}

/**
 * @brief Find the first element in c equal to v and return the position.
 *
 * Equivalent to find_in(std::begin(c), std::end(c), v)
 *
 * @param c The container to search.
 * @param v The value to search for.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename T>
KBLIB_NODISCARD size_t find_in(const Container& c, const T& v) {
	return std::find(std::begin(c), std::end(c), v) - std::begin(c);
}
#if 0
template<typename ExecutionPolicy, typename Container, typename T>
size_t find_in(ExecutionPolicy&& policy, const Container& c, const T& v) {
  return std::find(policy, std::begin(c), std::end(c), v) - std::begin(c);
}
#endif

template <typename Container, typename UnaryPredicate>
/**
 * @brief Find the first element in c for which p returns true and return the
 * position.
 *
 * Equivalent to find_in_if(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param p The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
KBLIB_NODISCARD size_t find_in_if(const Container& c, UnaryPredicate p) {
	return std::find_if(std::begin(c), std::end(c), p) - std::begin(c);
}
/**
 * @brief Find the first element in c for which p returns false and return the
 * position.
 *
 * Equivalent to find_in_if_not(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param p The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_in_if_not(const Container& c, UnaryPredicate p) {
	return std::find_if_not(std::begin(c), std::end(c), p) - std::begin(c);
}
#if 0
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
size_t find_in_if(ExecutionPolicy&& policy, const Container& c, UnaryPredicate p) {
  return std::find_if(policy, std::begin(c), std::end(c), p) - std::begin(c);
}
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
size_t find_in_if_not(ExecutionPolicy&& policy, const Container& c, UnaryPredicate p) {
  return std::find_if_not(policy, std::begin(c), std::end(c), p) - std::begin(c);
}
#endif

template <typename Container, typename T>
/**
 * @brief Find the last element in c equal to v and return the position.
 *
 * Equivalent to find_last_in(std::begin(c), std::end(c), v)
 *
 * @param c The container to search.
 * @param v The value to search for.
 * @return size_t The position of the element found, or c.size() if not.
 */
KBLIB_NODISCARD size_t find_last_in(const Container& c, const T& v) {
	return kblib::find_last(std::begin(c), std::end(c), v) - std::begin(c);
}

/**
 * @brief Find the last element in c for which p returns true and return the
 * position.
 *
 * Equivalent to find_last_in_if(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param p The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if(const Container& c, UnaryPredicate p) {
	return kblib::find_last_if(std::begin(c), std::end(c), p) - std::begin(c);
}
/**
 * @brief Find the last element in c for which p returns true and return the
 * position.
 *
 * Equivalent to find_last_in_if_not(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param p The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if_not(const Container& c,
                                           UnaryPredicate p) {
	return kblib::find_last_if_not(std::begin(c), std::end(c), p) -
	       std::begin(c);
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [begin, end), in arbitrary order. This overload works for linear
 * containers.
 *
 * This function is included because its performance is sometimes better than
 * the new version, and additionally, it does not rely on
 * default-constructibility for the value type.
 *
 * @attention The returned container will not be sorted, unless it is something
 * like std::multiset which will use the other overload.
 *
 * @param begin The beginning of the range.
 * @param end One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return Container The greatest count elements of the range, in arbitrary
 * order.
 */
template <
    typename Container, typename Comp = std::less<>, typename It,
    typename std::enable_if<is_linear_container_v<Container>, int>::type = 0>
KBLIB_NODISCARD Container get_max_n_old(It begin, It end, std::size_t count,
                                        Comp cmp = {}) {
	assert(begin + count <= end);
	Container c{begin, begin + count};
	std::for_each(begin + count, end, [&](const auto& v) {
		auto& min = *std::min_element(c.begin(), c.end(), cmp);
		if (cmp(min, v)) {
			min = v;
		}
	});
	return c;
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [begin, end). This overload works for set-like types.
 *
 * This function is included because its performance is sometimes better than
 * the new version, and additionally, it does not rely on
 * default-constructibility for the value type.
 *
 * @param begin The beginning of the range.
 * @param end One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return Container The greatest count elements of the range, in arbitrary
 * order.
 */
template <typename Container, typename Comp = std::less<>, typename It,
          typename std::enable_if<is_setlike_v<Container>, int>::type = 0>
KBLIB_NODISCARD Container get_max_n_old(It begin, It end, std::size_t count,
                                        Comp cmp = {}) {
	auto temp = get_max_n_old<std::vector<key_type_setlike_t<Container>>>(
	    begin, end, count, cmp);
	return Container{temp.begin(), temp.end()};
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [begin, end), in descending order. This overload works for linear
 * containers.
 *
 * @param begin The beginning of the range.
 * @param end One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return Container The greatest count elements of the range, in arbitrary
 * order.
 */
template <
    typename Container, typename Comp = std::less<>, typename It,
    typename std::enable_if<is_linear_container_v<Container>, int>::type = 0>
KBLIB_NODISCARD Container get_max_n(It begin, It end, std::size_t count,
                                    Comp cmp = {}) {
	Container c(count);
	std::partial_sort_copy(begin, end, c.begin(), c.end(), fakestd::not_fn(cmp));
	return c;
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [begin, end). This overload works for set-like containers.
 *
 * @param begin The beginning of the range.
 * @param end One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return Container The greatest count elements of the range, in arbitrary
 * order.
 */
template <typename Container, typename Comp = std::less<>, typename It,
          typename std::enable_if<is_setlike_v<Container>, int>::type = 0>
KBLIB_NODISCARD Container get_max_n(It begin, It end, std::size_t count,
                                    Comp cmp = {}) {
	auto temp = get_max_n<std::vector<key_type_setlike_t<Container>>>(
	    begin, end, count, cmp);
	return Container{temp.begin(), temp.end()};
}

/**
 * @brief Copies the count greatest elements according to cmp of the range
 * [begin, end) to the range beginning at d_begin.
 *
 * Note that this function uses O(count) extra memory to store a mutable range
 * for sorting. Directly calling std::partial_sort_copy with a properly sized
 * container will be more efficient than this function because it avoids
 * allocating extra working memory. This function should rather be used for
 * non-random-access output ranges.
 *
 * @param begin The beginning of the range.
 * @param end One past the end of the range.
 * @param d_begin The beginning of the output range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return return_assert_t<is_output_iterator<OIt, ElementT>::value, OIt> An
 * iterator derived by assigning to and advancing d_begin count times.
 */
template <typename Comp = std::less<>, typename IIt, typename OIt,
          typename ElementT = typename std::iterator_traits<IIt>::value_type>
auto get_max_n(IIt begin, IIt end, OIt d_begin, std::size_t count,
               Comp cmp = {})
    -> return_assert_t<is_output_iterator<OIt, ElementT>::value, OIt> {
	auto temp = get_max_n<std::vector<ElementT>>(begin, end, count, cmp);
	return std::move(temp.begin(), temp.end(), d_begin);
}

/**
 * @brief
 *
 * @param first
 * @param last
 * @param second
 * @param f
 * @return BinaryFunction
 */
template <typename It, typename It2, typename BinaryFunction>
KBLIB_NODISCARD constexpr BinaryFunction for_each(It first, It last, It2 second,
                                                  BinaryFunction f) {
	for (; first != last; (void)++first, (void)++second) {
		f(*first, *second);
	}
	return std::move(f);
}

/**
 * @brief
 *
 * @param first
 * @param n
 * @param second
 * @param f
 * @return It
 */
template <typename It, typename It2, typename Size, typename BinaryFunction>
KBLIB_NODISCARD constexpr It for_each_n(It first, Size n, It2 second,
                                        BinaryFunction f) {
	for (Size i = 0; i < n; (void)++first, (void)++second, (void)++i) {
		f(*first, *second);
	}
	return first;
}

/**
 * @brief
 *
 * @param first
 * @param count
 * @param out
 * @param pred
 * @return OutputIt
 */
template <typename InputIt, typename Size, typename OutputIt,
          typename UnaryPredicate>
KBLIB_NODISCARD OutputIt copy_n_if(InputIt first, Size count, OutputIt out,
                                   UnaryPredicate pred) {
	for (Size i = 0; i < count; ++i) {
		if (pred(*first)) {
			*out++ = *first;
		}
		++first;
	}
	return out;
}

/**
 * @brief
 *
 * @param first
 * @param count
 * @param out
 * @param pred
 * @return OutputIt
 */
template <typename InputIt, typename Size, typename OutputIt,
          typename UnaryPredicate>
KBLIB_NODISCARD OutputIt replace_copy_n_if(InputIt first, Size count,
                                           OutputIt out, UnaryPredicate pred) {
	for (Size i = 0; i < count; ++i) {
		if (pred(*first)) {
			*out++ = *first;
		}
		++first;
	}
	return out;
}

template <typename IIt1, typename EIt, typename IIt2>
struct zip_iterator {
	IIt1 pos1{};
	EIt end1{};
	IIt2 pos2{};

	zip_iterator& operator++() {
		++pos1;
		++pos2;
		return *this;
	}
	zip_iterator operator++(int) {
		auto tmp = *this;
		++pos1;
		++pos2;
		return tmp;
	}

	auto operator*() {
		return std::forward_as_tuple(*pos1, *pos2);
	}

	zip_iterator begin() {return *this;}
	zip_iterator<EIt, EIt, IIt2> end() const {return {end1, end1};}

	friend bool operator==(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 == z2.pos1;
	}
	friend bool operator!=(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 != z2.pos1;
	}
	friend bool operator==(const zip_iterator& z1, zip_iterator<EIt, EIt, IIt2> end) {
		return z1.end1 == end.val;
	}
	friend bool operator!=(const zip_iterator& z1, zip_iterator<EIt, EIt, IIt2> end) {
		return z1.end1 != end.val;
	}
};

template <typename It1, typename It2>
struct zip_iterator<It1, It1, It2> {
	It1 pos1{};
	It1 end1{};
	It2 pos2{};

	zip_iterator& operator++() {
		++pos1;
		++pos2;
		return *this;
	}
	zip_iterator operator++(int) {
		auto tmp = *this;
		++pos1;
		++pos2;
		return tmp;
	}

	auto operator*() {
		return std::forward_as_tuple(*pos1, *pos2);
	}

	zip_iterator begin() const {return *this;}
	zip_iterator end() const {return {end1, end1};}

	friend bool operator==(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 == z2.pos1;
	}
	friend bool operator!=(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 != z2.pos1;
	}
};

template <typename IIt1, typename EIt, typename IIt2>
zip_iterator<IIt1, EIt, IIt2> zip(IIt1 begin1, EIt end1, IIt2 begin2) {
	return {begin1, end1, begin2};
}

}

#endif // ALGORITHM_H
