#ifndef ALGORITHM_H
#define ALGORITHM_H

#include "tdecl.h"

#include "iterators.h"
#include "traits.h"

#include <algorithm>
#include <cmath>
#include <tuple>

namespace kblib {

/**
 * @brief Invoke a function N times.
 *
 * @param N The number of times to invoke func.
 * @param func The function to invoke.
 */
template <typename Callable>
constexpr void repeat(std::size_t N, Callable func) noexcept(func()) {
	for (std::size_t I = 0; I != N; ++I) {
		func();
	}
	return;
}

/**
 * @brief Abbreviation of the erase-remove idiom as a free function.
 *
 * @param c The container to erase from.
 * @param val The value to remove.
 */
template <typename Container, typename Elem>
constexpr void erase(Container& c, const Elem& val) noexcept(
    noexcept(c.erase(std::remove(c.begin(), c.end(), val), c.end()))) {
	c.erase(std::remove(c.begin(), c.end(), val), c.end());
	return;
}

/**
 * @brief Abbreviation of the erase-remove idiom as a free function.
 *
 * @param c The container to erase from.
 * @param p Erase all elements on which p returns true.
 */
template <typename Container, typename UnaryPredicate>
constexpr void erase_if(Container& c, UnaryPredicate p) noexcept(noexcept(
    c.erase(std::remove_if(c.begin(), c.end(), std::ref(p)), c.end()))) {
	c.erase(std::remove_if(c.begin(), c.end(), std::ref(p)), c.end());
	return;
}

/**
 * @brief Synthesize an equivalence relation from <.
 *
 * @return bool Whether a is equivalent under < to b.
 */
template <typename Obj>
KBLIB_NODISCARD constexpr bool equals(const Obj& a,
                                      const Obj& b) noexcept(noexcept(a < b)) {
	return !(a < b) && !(b < a);
}

/**
 * @brief Synthesize an equivalence relation from comp.
 *
 * @return bool Whether a is equivalent under comp to b.
 */
template <typename Obj, typename Compare>
KBLIB_NODISCARD constexpr bool
equals(const Obj& a, const Obj& b,
       Compare comp) noexcept(noexcept(comp(a, b))) {
	return !comp(a, b) && !comp(b, a);
}

/**
 * @brief A function object implementing the equivalence relationship over a
 * comparison predicate.
 *
 * @tparam Compare The predicate to compare over. If void, use the < operator.
 * (Equivalent to defaulting to std::less.)
 * @tparam Obj The type of objects to compare. If void, the operator() is a
 * template.
 */
template <typename Compare = void, typename Obj = void>
struct equivalent {
	KBLIB_NODISCARD constexpr bool operator()(const Obj& a, const Obj& b,
	                                          Compare comp) const
	    noexcept(noexcept(equals(a, b, comp))) {
		return equals(a, b, comp);
	}
};

template <typename Obj>
struct equivalent<void, Obj> {
	KBLIB_NODISCARD constexpr bool operator()(const Obj& a, const Obj& b) const
	    noexcept(noexcept(equals(a, b))) {
		return equals(a, b);
	}
};

template <typename Compare>
struct equivalent<Compare, void> {
	template <typename Obj>
	KBLIB_NODISCARD constexpr bool operator()(const Obj& a, const Obj& b,
	                                          Compare comp) const
	    noexcept(noexcept(equals(a, b, comp))) {
		return equals(a, b, comp);
	}
};

template <>
struct equivalent<void, void> {
	template <typename Obj>
	KBLIB_NODISCARD constexpr bool operator()(const Obj& a, const Obj& b) const
	    noexcept(noexcept(equals(a, b))) {
		return equals(a, b);
	}
};

/**
 * @brief Finds a value in range [begin, end). If not found, returns end. It
 * also allows for a sentinel end iterator.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param value The value to search for
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename Elem>
KBLIB_NODISCARD constexpr ForwardIt
find(ForwardIt begin, EndIt end,
     const Elem& value) noexcept(noexcept(*begin == value)) {
	while (begin != end && !(*begin == value)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds a value in range [begin, end). If not found, returns end. It
 * also allows for a sentinel end iterator.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param value The value to search for
 * @param comp The comparison function
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename Elem, typename Comp>
KBLIB_NODISCARD constexpr ForwardIt
find(ForwardIt begin, EndIt end, const Elem& value,
     Comp&& comp) noexcept(noexcept(comp(*begin, value))) {
	while (begin != end && !equals(*begin, value, comp)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds the first value in range [begin, end) for which pred returns
 * true. If not found, returns end. It also allows for a sentinel end iterator.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param pred The predicate to scan with
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt
find_if(ForwardIt begin, EndIt end,
        UnaryPredicate&& pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	while (begin != end && !kblib::invoke(pred, *begin)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds the first value in range [begin, end) for which pred returns
 * false. If not found, returns end. It also allows for a sentinel end iterator.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param pred The predicate to scan with
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt find_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate&& pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	while (begin != end && kblib::invoke(pred, *begin)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Searches a range for the last occurence of a match, and returns an
 * iterator to it. It also allows for a sentinel end iterator.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param value The value to find
 * @return It Iterator to the last element equal to v, or end if no such
 * element.
 */
template <typename ForwardIt, typename EndIt, typename Elem>
KBLIB_NODISCARD constexpr ForwardIt
find_last(ForwardIt begin, EndIt end,
          const Elem& value) noexcept(noexcept(*begin == value)) {
	if (begin == end) {
		return begin;
	}
	auto result = end;
	while (true) {
		auto new_result = kblib::find(begin, end, value);
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
 * true. It also allows for a sentinel end iterator.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param pred The predicate for comparison
 * @return It Iterator to the last element for which p returned true, or end if
 * no such element.
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt find_last_if(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	if (begin == end) {
		return begin;
	}
	auto result = end;
	while (true) {
		auto new_result = kblib::find_if(begin, end, pred);
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
 * false. It also allows for a sentinel end iterator.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param pred The predicate for comparison
 * @return It Iterator to the last element for which p returned false, or end if
 * no such element.
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt find_last_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	if (begin == end) {
		return begin;
	}
	auto result = end;
	while (true) {
		auto new_result = kblib::find_if_not(begin, end, pred);
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
 * beginning. It also allows for a sentinel end iterator.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param value The value to search for.
 * @return size_t The offset from begin of the first element equal to v, or
 * distance(begin, end) if not found.
 */
template <typename ForwardIt, typename EndIt, typename Elem>
KBLIB_NODISCARD constexpr size_t
find_in(ForwardIt begin, EndIt end,
        const Elem& value) noexcept(noexcept(*begin == value)) {
	return kblib::find(begin, end, value) - begin;
}

/**
 * @brief Find the offset of the first element for which p returns true. It also
 * allows for a sentinel end iterator.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param pred The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	return kblib::find_if(begin, end, pred) - begin;
}
/**
 * @brief Find the offset of the first element for which p returns false. It
 * also allows for a sentinel end iterator.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param pred The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	return kblib::find_if_not(begin, end, pred) - begin;
}

// find_last_in:
// 1. Finds last v in range [begin, end) and returns the offset from begin

/**
 * @brief Find the offset of the last ocurrence of v in a range from the
 * beginning. It also allows for a sentinel end iterator.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param value The value to search for.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename ForwardIt, typename EndIt, typename Elem>
KBLIB_NODISCARD constexpr size_t
find_last_in(ForwardIt begin, EndIt end,
             const Elem& value) noexcept(noexcept(*begin == value)) {
	return kblib::find_last(begin, end, value) - begin;
}

/**
 * @brief Find the offset of the last element for which p returns true. It also
 * allows for a sentinel end iterator.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param pred The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_last_in_if(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	return kblib::find_last_if(begin, end, pred) - begin;
}
/**
 * @brief Find the offset of the last element for which p returns false. It also
 * allows for a sentinel end iterator.
 *
 * @param begin The beginning of the range to search.
 * @param end One past the end of the range.
 * @param pred The predicate to check.
 * @return size_t The offset from begin of the element found, or distance(begin,
 * end) if not.
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_last_in_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	return kblib::find_last_if_not(begin, end, pred) - begin;
}

/**
 * @brief Find the first element in c equal to v and return the position.
 *
 * Equivalent to find_in(std::begin(c), std::end(c), v)
 *
 * @param c The container to search.
 * @param value The value to search for.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename T>
KBLIB_NODISCARD constexpr size_t
find_in(const Container& c, const T& value) noexcept(
    noexcept(*std::declval<iterator_type_for_t<const Container>&>() == value)) {
	using std::begin;
	using std::end;
	return kblib::find(begin(c), end(c), value) - begin(c);
}
#if 0
template<typename ExecutionPolicy, typename Container, typename T>
KBLIB_NODISCARD constexpr size_t find_in(ExecutionPolicy&& policy,
                                         const Container& c, const T& v) {
  return std::find(policy, std::begin(c), std::end(c), v) - std::begin(c);
}
#endif

/**
 * @brief Find the first element in c for which p returns true and return the
 * position.
 *
 * Equivalent to find_in_if(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param pred The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t
find_in_if(const Container& c, UnaryPredicate pred) noexcept(noexcept(
    kblib::invoke(pred,
                  *std::declval<iterator_type_for_t<const Container>&>()))) {
	using std::begin;
	using std::end;
	return kblib::find_if(begin(c), end(c), pred) - begin(c);
}
/**
 * @brief Find the first element in c for which p returns false and return the
 * position.
 *
 * Equivalent to find_in_if_not(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param pred The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t
find_in_if_not(const Container& c, UnaryPredicate pred) noexcept(noexcept(
    kblib::invoke(pred,
                  *std::declval<iterator_type_for_t<const Container>&>()))) {
	using std::begin;
	using std::end;
	return kblib::find_if_not(begin(c), end(c), pred) - begin(c);
}
#if 0
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if(ExecutionPolicy&& policy,
                                            const Container& c,
                                            UnaryPredicate p) {
  return std::find_if(policy, std::begin(c), std::end(c), p) - std::begin(c);
}
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if_not(ExecutionPolicy&& policy,
                                                const Container& c,
                                                UnaryPredicate p) {
  return std::find_if_not(policy, std::begin(c), std::end(c), p)
        - std::begin(c);
}
#endif

/**
 * @brief Find the last element in c equal to v and return the position.
 *
 * Equivalent to find_last_in(std::begin(c), std::end(c), v)
 *
 * @param c The container to search.
 * @param value The value to search for.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename T>
KBLIB_NODISCARD constexpr size_t
find_last_in(const Container& c, const T& value) noexcept(
    noexcept(*std::declval<iterator_type_for_t<const Container>&>() == value)) {
	using std::begin;
	using std::end;
	return kblib::find_last(begin(c), end(c), value) - begin(c);
}

/**
 * @brief Find the last element in c for which p returns true and return the
 * position.
 *
 * Equivalent to find_last_in_if(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param pred The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t
find_last_in_if(const Container& c, UnaryPredicate pred) noexcept(noexcept(
    kblib::invoke(pred,
                  *std::declval<iterator_type_for_t<const Container>&>()))) {
	using std::begin;
	using std::end;
	return kblib::find_last_if(begin(c), end(c), pred) - begin(c);
}
/**
 * @brief Find the last element in c for which p returns true and return the
 * position.
 *
 * Equivalent to find_last_in_if_not(std::begin(c), std::end(c), p)
 *
 * @param c The container to search in.
 * @param pred The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t
find_last_in_if_not(const Container& c, UnaryPredicate pred) noexcept(noexcept(
    kblib::invoke(pred,
                  *std::declval<iterator_type_for_t<const Container>&>()))) {
	using std::begin;
	using std::end;
	return kblib::find_last_if_not(begin(c), end(c), pred) - begin(c);
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [first, last), in arbitrary order. This overload works for linear
 * containers.
 *
 * This function is included because its performance is sometimes better than
 * the new version, and additionally, it does not rely on
 * default-constructibility for the value type.
 *
 * @attention The returned container will not be sorted, unless it is something
 * like std::multiset which will use the other overload.
 *
 * @param first The beginning of the range.
 * @param last One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return SequenceContainer The greatest count elements of the range, in
 * arbitrary order.
 */
template <typename SequenceContainer, typename Comp = std::less<>, typename It,
          enable_if_t<is_linear_container_v<SequenceContainer>, int> = 0>
KBLIB_NODISCARD constexpr SequenceContainer
get_max_n_old(It first, It last, std::size_t count, Comp cmp = {}) {
	using std::begin;
	using std::end;
	assert(first + count <= last);
	SequenceContainer c{first, begin + count};
	std::for_each(first + count, last, [&](const auto& v) {
		auto& min = *std::min_element(begin(c), end(c), cmp);
		if (cmp(min, v)) {
			min = v;
		}
	});
	return c;
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [first, last). This overload works for set-like types.
 *
 * This function is included because its performance is sometimes better than
 * the new version, and additionally, it does not rely on
 * default-constructibility for the value type.
 *
 * @param first The beginning of the range.
 * @param first One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return SetlikeContainer The greatest count elements of the range, in
arbitrary
 * order.
 */
template <typename SetlikeContainer, typename Comp = std::less<>, typename It,
          enable_if_t<is_setlike_v<SetlikeContainer>, int> = 0>
KBLIB_NODISCARD constexpr SetlikeContainer
get_max_n_old(It first, It last, std::size_t count, Comp cmp = {}) {
	auto temp = get_max_n_old<std::vector<key_type_setlike_t<SetlikeContainer>>>(
	    first, last, count, cmp);
	return SetlikeContainer{std::make_move_iterator(temp.begin()),
	                        std::make_move_iterator(temp.end())};
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [first, last), in descending order. This overload works for linear
 * containers.
 *
 * @param first The beginning of the range.
 * @param last One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return SequenceContainer The greatest count elements of the range, in
 * arbitrary order.
 */
template <typename SequenceContainer, typename Comp = std::less<>, typename It,
          enable_if_t<is_linear_container_v<SequenceContainer>, int> = 0>
KBLIB_NODISCARD constexpr SequenceContainer
get_max_n(It first, It last, std::size_t count, Comp cmp = {}) {
	using std::begin;
	using std::end;
	SequenceContainer c(count);
	std::partial_sort_copy(first, last, begin(c), end(c),
	                       [&](auto&&... args) -> decltype(auto) {
		                       return !cmp(std::forward<decltype(args)>(args)...);
	                       });
	return c;
}

/**
 * @brief Returns a container of the greatest count elements according to cmp of
 * the range [first, last). This overload works for set-like containers.
 *
 * @param first The beginning of the range.
 * @param last One past the end of the range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return SetlikeContainer The greatest count elements of the range, in
 * arbitrary order.
 */
template <typename SetlikeContainer, typename Comp = std::less<>, typename It,
          enable_if_t<is_setlike_v<SetlikeContainer>, int> = 0>
KBLIB_NODISCARD constexpr SetlikeContainer
get_max_n(It first, It last, std::size_t count, Comp cmp = {}) {
	auto temp = get_max_n<std::vector<key_type_setlike_t<SetlikeContainer>>>(
	    first, last, count, cmp);
	return SetlikeContainer{std::make_move_iterator(temp.begin()),
	                        std::make_move_iterator(temp.end())};
}

/**
 * @brief Copies the count greatest elements according to cmp of the range
 * [first, last) to the range beginning at d_begin.
 *
 * Note that this function uses O(count) extra memory to store a mutable range
 * for sorting. Directly calling std::partial_sort_copy with a properly sized
 * container will be more efficient than this function because it avoids
 * allocating extra working memory. This function should rather be used for
 * non-random-access output ranges.
 *
 * @param first The beginning of the range.
 * @param last One past the end of the range.
 * @param d_begin The beginning of the output range.
 * @param count The number of elements to copy out of the range.
 * @param cmp The comparison function to use.
 * @return return_assert_t<is_output_iterator<OutputIt, ElementT>::value,
 * OutputIt> An iterator to past the last element written.
 */
template <typename Comp = std::less<>, typename InputIt, typename OutputIt,
          typename Elem = typename std::iterator_traits<InputIt>::value_type>
constexpr auto get_max_n(InputIt first, InputIt last, OutputIt d_begin,
                         std::size_t count, Comp cmp = {})
    -> return_assert_t<is_output_iterator<OutputIt, Elem>::value, OutputIt> {
	auto temp = get_max_n<std::vector<Elem>>(first, last, count, cmp);
	return std::move(temp.begin(), temp.end(), d_begin);
}

/**
 * @brief Applies a binary operation to each pair of corresponding elements in
 * two input ranges. It also allows for a sentinel end iterator.
 *
 * In the style of <algorithm> algorithms, the second range is simply assumed to
 * be at least as large as the first.
 *
 * @param first The beginning of the first input range.
 * @param last The end of the first input range.
 * @param second The beginning of the second input range.
 * @param f The operation to apply.
 * @return BinaryFunction f
 */
template <typename ForwardIt, typename EndIt, typename ForwardIt2,
          typename BinaryFunction>
constexpr BinaryFunction for_each(ForwardIt first, EndIt last,
                                  ForwardIt2 second, BinaryFunction f) {
	for (; first != last; (void)++first, (void)++second) {
		f(*first, *second);
	}
	return std::move(f);
}

/**
 * @brief Applies a binary operation to each pair of corresponding elements in
 * two input ranges.
 *
 * @param first The beginning of the first input range.
 * @param n The number of elements to operate on in each input range.
 * @param second The beginning of the second input range.
 * @param f The operation to apply.
 * @return std::pair<ForwardIt, ForwardIt2> Equivalent to `{std::advance(first,
 * n), std::advance(second, n)}`
 */
template <typename ForwardIt, typename ForwardIt2, typename Size,
          typename BinaryFunction>
constexpr std::pair<ForwardIt, ForwardIt2>
for_each_n(ForwardIt first, Size n, ForwardIt2 second, BinaryFunction f) {
	for (Size i = 0; i < n; (void)++first, (void)++second, (void)++i) {
		f(*first, *second);
	}
	return {first, second};
}

/**
 * @brief Copies all elements of [`first`, `last`) to out.  It also allows for a
 * sentinel end iterator.
 *
 * @remark This function is `constexpr` in C++14.
 *
 * @param first The beginning of the input range
 * @param last The end of the input range
 * @param out The beginning of the output range
 * @return OutputIt An iterator to past the last element written
 */
template <typename InputIt, typename EndIt, typename OutputIt>
constexpr OutputIt copy(InputIt first, EndIt last, OutputIt out) {
	while (first != last) {
		*out++ = *first++;
	}
	return out;
}

/**
 * @brief Copies those elements of [`first`, `last`) which satisfy pred to out.
 * It also allows for a sentinel end iterator.
 *
 * @remark This function is `constexpr` in C++14.
 *
 * @param first The beginning of the input range
 * @param last The end of the input range
 * @param out The beginning of the output range
 * @param pred The predicate to apply
 * @return OutputIt An iterator to past the last element written
 */
template <typename InputIt, typename EndIt, typename OutputIt,
          typename UnaryPredicate>
constexpr OutputIt copy_if(InputIt first, EndIt last, OutputIt out,
                           UnaryPredicate pred) {
	while (first != last) {
		if (kblib::invoke(pred, *first)) {
			*out++ = *first;
		}
		first++;
	}
	return out;
}

/**
 * @brief Copies all elements of [`first`, `std::advance(first, n)`) to out.
 *
 * @remark This function is `constexpr` in C++14.
 *
 * @param first The beginning of the input range.
 * @param count The number of elements in the input range.
 * @param out The output range.
 * @return OutputIt OutputIt An iterator to past the last element written
 */
template <typename InputIt, typename Size, typename OutputIt>
constexpr OutputIt copy_n(InputIt first, Size count, OutputIt out) {
	for (Size i = 0; i < count; ++i) {
		*out++ = *first++;
	}
	return out;
}

/**
 * @brief Copies those elements of [`first`, `std::advance(first, n)`) which
 * satisfy pred to out.
 *
 * @remark This function is `constexpr` in C++14.
 *
 * @param first The beginning of the input range.
 * @param count The number of elements in the input range.
 * @param out The output range.
 * @param pred The predicate to apply.
 * @return OutputIt OutputIt An iterator to past the last element written
 */
template <typename InputIt, typename Size, typename OutputIt,
          typename UnaryPredicate>
constexpr OutputIt copy_n_if(InputIt first, Size count, OutputIt out,
                             UnaryPredicate pred) {
	for (Size i = 0; i < count; ++i) {
		if (kblib::invoke(pred, *first)) {
			*out++ = *first;
		}
		++first;
	}
	return out;
}

/**
 * @brief Copies an input range, but every element for which pred is true is
 * replaced by new_value. It also allows for a sentinel end iterator.
 *
 * @remark This function is `constexpr` in C++14.
 *
 * @param first The beginning of the input range.
 * @param last The end of the input range.
 * @param out The beginning of the output range.
 * @param pred The predicate to apply.
 * @param new_value The value to replace those elements for which pred is true
 * with.
 * @return An iterator to past the last element written.
 */
template <typename InputIt, typename EndIt, typename OutputIt,
          typename UnaryPredicate, typename T>
constexpr OutputIt replace_copy_if(InputIt first, EndIt last, OutputIt out,
                                   UnaryPredicate pred, const T& new_value) {
	while (first != last) {
		if (kblib::invoke(pred, *first)) {
			*out = *first;
		} else {
			*out = new_value;
		}
		++first;
		++out;
	}
	return out;
}

/**
 * @brief Copies an input range, but every element for which pred is true is
 * replaced by new_value.
 *
 * @remark This function is `constexpr` in C++14.
 *
 * @param first The beginning of the input range.
 * @param count The number of elements to copy.
 * @param out The beginning of the output range.
 * @param pred The predicate to apply.
 * @param new_value The value to replace those elements for which pred is true
 * with.
 * @return OutputIt An iterator to past the last element written.
 */
template <typename InputIt, typename Size, typename OutputIt,
          typename UnaryPredicate, typename T>
constexpr OutputIt replace_copy_n_if(InputIt first, Size count, OutputIt out,
                                     UnaryPredicate pred, const T& new_value) {
	for (Size i = 0; i < count; ++i) {
		if (kblib::invoke(pred, *first)) {
			*out = *first;
		} else {
			*out = new_value;
		}
		++first;
		++out;
	}
	return out;
}

/**
 * @brief Rotates the input range. This is just a constexpr-in-C++14 version of
 * std::rotate.
 *
 * @param first
 * @param n_first
 * @param last
 * @return ForwardIt
 */
template <class ForwardIt>
constexpr ForwardIt
rotate(ForwardIt first, ForwardIt n_first,
       ForwardIt last) noexcept(noexcept(swap(*first, *first))) {
	if (first == n_first)
		return last;
	if (n_first == last)
		return first;

	ForwardIt read = n_first;
	ForwardIt write = first;
	ForwardIt next_read = first; // read position for when "read" hits "last"

	while (read != last) {
		if (write == next_read)
			next_read = read; // track where "first" went
		swap(*(write++), *(read++));
	}

	// rotate the remaining sequence into place
	kblib::rotate(write, next_read, last);
	return write;
}

/**
 * @brief Like std::generate except that it returns the output iterator at the
 * end. It also allows for a sentinel end iterator.
 *
 * @param first The beginning of the ouput range.
 * @param last The end of the output range.
 * @param g A generator to repeatedly call and assign the return values to the
 * elements of the output range.
 * @return ForwardIt The iterator pointing past the last element written.
 */
template <typename OutputIt, typename EndIt, typename Generator>
constexpr OutputIt generate(OutputIt first, EndIt last, Generator g) noexcept(
    noexcept(first != last) && noexcept(*++first = g())) {
	while (first != last) {
		*first = g();
		++first;
	}
	return first;
}

/**
 * @brief transform applies the given function to a range and stores the result
 * in another range, beginning at d_first. The unary operation unary_op is
 * applied to the range defined by [first1, last1). It also allows for a
 * sentinel end iterator.
 *
 * @remark The expression `*d_first = kblib::invoke(unary_op, *first)` must be
 * valid and must not modify `*first`.
 *
 * @param first The beginning of the input range
 * @param last The end of the input range
 * @param d_first The beginning of the output range
 * @param unary_op The operation to apply
 * @return OutputIt An iterator to past the last element written
 */
template <typename InputIt, typename EndIt, typename OutputIt,
          typename UnaryOperation>
constexpr OutputIt transform(InputIt first, EndIt last, OutputIt d_first,
                             UnaryOperation unary_op) {
	while (first != last) {
		*d_first++ = kblib::invoke(unary_op, *first);
		++first;
	}
	return d_first;
}

/**
 * @brief transform applies the given function to a range and stores the result
 * in another range, beginning at d_first. The unary operation unary_op is
 * applied to the range defined by [first1, last1). It also allows for a
 * sentinel end iterator.
 *
 * @remark The expression `*d_first = kblib::invoke(binary_op, *first, *first2)`
 * must be valid and must not modify `*first` or `*first2`.
 *
 * @param first The beginning of the first input range
 * @param last The end of the first input range
 * @param first2 The beginning of the second input range
 * @param d_first The beginning of the output range
 * @param binary_op The operation to apply
 * @return OutputIt An iterator to past the last element written
 */
template <typename InputIt, typename EndIt, typename InputIt2,
          typename OutputIt, typename BinaryOperation>
constexpr OutputIt transform(InputIt first, EndIt last, InputIt first2,
                             OutputIt d_first, BinaryOperation binary_op) {
	while (first != last) {
		*d_first++ = kblib::invoke(binary_op, *first, *first2);
		++first;
		++first2;
	}
	return d_first;
}

/**
 * @brief transform applies the given function to a range and stores the result
 * in another range, beginning at d_first. The unary operation unary_op is
 * applied to the range defined by [first1, last1). It also allows for a
 * sentinel end iterator.
 *
 * @remark The expression `kblib::invoke(pred, *first)` must be valid and must
 * return a type convertible to `bool`, and must not modify `*first`
 * @remark The expression `*d_first = kblib::invoke(unary_op, *first)` must be
 * valid and must not modify `*first`.
 *
 * @param first The beginning of the input range
 * @param last The end of the input range
 * @param d_first The beginning of the output range
 * @param pred The predicate to apply
 * @param unary_op The operation to apply
 * @return OutputIt An iterator to past the last element written
 */
template <typename InputIt, typename EndIt, typename OutputIt,
          typename UnaryPredicate, typename UnaryOperation>
constexpr OutputIt transform_if(InputIt first, EndIt last, OutputIt d_first,
                                UnaryPredicate pred, UnaryOperation unary_op) {
	while (first != last) {
		if (kblib::invoke(pred, *first)) {
			*d_first++ = kblib::invoke(unary_op, *first);
		}
		++first;
	}
	return d_first;
}

namespace detail {

	/**
	 * @brief Implementation function for insertion_sort_copy. Like
	 * std::move(begin, end, d_begin) but using the interface of rotate and
	 * supporting backward overlapping, but not forward overlapping.
	 *
	 * @param first Start of range to assign to
	 * @param n_first Start of range to read from
	 * @param last End of range to read from
	 */
	template <class ForwardIt>
	constexpr void shift_backward(
	    ForwardIt first, ForwardIt n_first,
	    ForwardIt last) noexcept(noexcept(*first = std::move(*first))) {
		if (first == n_first || n_first == last)
			return;

		ForwardIt read = n_first;
		ForwardIt write = first;

		while (read != last) {
			*(write++) = std::move(*(read++));
		}

		return;
	}

} // namespace detail

/**
 * @brief In-place insertion sort. As is usual, it is stable.
 * Provides as a guarantee that it will perform no moves on sorted input.
 *
 * @remark Complexity:
 * @remark Average case O(n^2)
 * @remark Best-case Θ(n) (for sorted input)
 * @remark worst-case O(n^2) (for reverse-sorted input)
 *
 * @param begin Beginning of range to sort
 * @param end End of range
 * @param compare The comparison predicate
 */
template <typename RandomAccessIt, typename Compare = std::less<>>
constexpr void insertion_sort(
    const RandomAccessIt begin, const RandomAccessIt end,
    Compare&& compare =
        {}) noexcept(noexcept(swap(*begin,
                                   *begin)) && noexcept(compare(*begin,
                                                                *begin))) {
	// Trivial inputs are trivially already sorted.
	if (end - begin <= 1) {
		return;
	}
	for (auto pos = begin + 1; pos != end; ++pos) {
		// This search is linear, not binary, because insertion_sort is meant
		// to never be used for arrays large enough for binary search.
		auto index =
		    kblib::find_last_if(begin, pos, [&compare, pos](const auto& a) {
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
 * @param begin Beginning of input range
 * @param end End of input range
 * @param d_begin Beginning of output range
 * @param d_end End of output range
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
                                      d_end)) && noexcept(*d_begin = *begin)) {
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
				    return !compare(a, *read);
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
 * @param begin Beginning of input range
 * @param end End of input range
 * @param d_begin Beginning of output range
 * @param d_end End of output range
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
                                      d_end)) && noexcept(*d_begin = *begin)) {
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
				/// TODO: write efficient sort_transform
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
				/// TODO: write efficient sort
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
				/// TODO: write efficent sort_copy
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
 * @param begin The beginning of the input range.
 * @param end A past-the-end iterator marking the end of the input range.
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
 * @param begin
 * @param end
 * @param transform
 */
template <typename RandomAccessIt, typename UnaryOperation>
constexpr void sort_transform(RandomAccessIt begin, RandomAccessIt end,
                              UnaryOperation&& transform) {
	detail::sort_transform_impl<RandomAccessIt, UnaryOperation, std::less<>,
	                            decltype(kblib::invoke(transform, *begin))>::
	    inplace(begin, end, std::forward<UnaryOperation>(transform),
	            std::less<>{});
}

template <typename InputIt1, typename EndIt, typename InputIt2>
struct zip_iterator {
	InputIt1 pos1{};
	EndIt end1{};
	InputIt2 pos2{};

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

	auto operator*() { return std::forward_as_tuple(*pos1, *pos2); }

	zip_iterator begin() { return *this; }
	zip_iterator<EndIt, EndIt, InputIt2> end() const { return {end1, end1}; }

	friend bool operator==(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 == z2.pos1;
	}
	friend bool operator!=(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 != z2.pos1;
	}
	friend bool operator==(const zip_iterator& z1,
	                       zip_iterator<EndIt, EndIt, InputIt2> end) {
		return z1.end1 == end.val;
	}
	friend bool operator!=(const zip_iterator& z1,
	                       zip_iterator<EndIt, EndIt, InputIt2> end) {
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

	auto operator*() { return std::forward_as_tuple(*pos1, *pos2); }

	zip_iterator begin() const { return *this; }
	zip_iterator end() const { return {end1, end1}; }

	friend bool operator==(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 == z2.pos1;
	}
	friend bool operator!=(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 != z2.pos1;
	}
};

/**
 * @brief Iterate over two ranges in lockstep, like Python's zip.
 *
 * InputIt1 and EndIt may be different types, however that breaks range-for
 * in C++14.
 *
 * @param begin1 The beginning of the first range to iterate over.
 * @param end1 The end of the first range.
 * @param begin2 The beginning of the second range to iterate over.
 * @return zip_iterator<InputIt1, EndIt, InputIt2> A range (and also an
 * iterator) which represents the two ranges taken in pairs.
 */
template <typename InputIt1, typename EndIt, typename InputIt2>
zip_iterator<InputIt1, EndIt, InputIt2> zip(InputIt1 begin1, EndIt end1,
                                            InputIt2 begin2) {
	return {begin1, end1, begin2};
}

} // namespace kblib

#endif // ALGORITHM_H
