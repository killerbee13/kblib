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
constexpr return_assert_t<is_invocable<Callable>::value, void>
repeat(std::size_t N, Callable func) noexcept(noexcept(func())) {
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
	return not(a < b) and not(b < a);
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
	return not comp(a, b) and not comp(b, a);
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
 * @param begin,end The range to search in
 * @param value The value to search for
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename Elem>
KBLIB_NODISCARD constexpr ForwardIt
find(ForwardIt begin, EndIt end,
     const Elem& value) noexcept(noexcept(*begin == value)) {
	while (begin != end and *begin != value) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds a value in range [begin, end). If not found, returns end. It
 * also allows for a sentinel end iterator.
 *
 * @param begin,end The range to search in
 * @param value The value to search for
 * @param comp The comparison function
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename Elem, typename Comp>
KBLIB_NODISCARD constexpr ForwardIt
find(ForwardIt begin, EndIt end, const Elem& value,
     Comp&& comp) noexcept(noexcept(comp(*begin, value))) {
	while (begin != end and not equals(*begin, value, comp)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds the first value in range [begin, end) for which pred returns
 * true. If not found, returns end. It also allows for a sentinel end iterator.
 *
 * @param begin,end The range to search in
 * @param pred The predicate to scan with
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt
find_if(ForwardIt begin, EndIt end,
        UnaryPredicate&& pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	while (begin != end and not kblib::invoke(pred, *begin)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds the first value in range [begin, end) for which pred returns
 * false. If not found, returns end. It also allows for a sentinel end iterator.
 *
 * @param begin,end The range to search in
 * @param pred The predicate to scan with
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt find_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate&& pred) noexcept(noexcept(kblib::invoke(pred, *begin))) {
	while (begin != end and kblib::invoke(pred, *begin)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Searches a range for the last occurence of a match, and returns an
 * iterator to it. It also allows for a sentinel end iterator.
 *
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
 *
 * @param begin,end The range to search in
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
 * @param begin,end The range to search in
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
 * Equivalent to find_in(begin(c), end(c), v)
 *
 * @param c The container to search.
 * @param value The value to search for.
 * @return size_t The position of the element found, or size(c) if not.
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
 * Equivalent to find_in_if(begin(c), end(c), p)
 *
 * @param c The container to search in.
 * @param pred The predicate to check.
 * @return size_t The position of the element found, or size(c) if not.
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
 * Equivalent to find_in_if_not(begin(c), end(c), p)
 *
 * @param c The container to search in.
 * @param pred The predicate to check.
 * @return size_t The position of the element found, or size(c) if not.
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
 * @brief Determine if a range contains a value.
 *
 * @param set The range to check.
 * @param val The value to search for.
 */
template <typename Set, typename Value>
KBLIB_NODISCARD constexpr bool
contains(const Set& set, const Value& val) noexcept(
    noexcept(*std::declval<iterator_type_for_t<const Set>&>() == val)) {
	using std::begin;
	using std::end;
	return kblib::find(begin(set), end(set), val) != end(set);
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
	SequenceContainer c{first, first + count};
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
    noexcept(first != last) and noexcept(*++first = g())) {
	while (first != last) {
		*first = g();
		++first;
	}
	return first;
}

/**
 * @brief Like std::generate_n except that it is constexpr.
 *
 * @param first The beginning of the ouput range.
 * @param count The number of elements to generate.
 * @param g A generator to repeatedly call and assign the return values to the
 * elements of the output range.
 * @return ForwardIt The iterator pointing past the last element written.
 */
template <typename OutputIt, typename Size, typename Generator>
constexpr OutputIt generate_n(OutputIt first, Size count,
                              Generator g) noexcept(noexcept(*first++ = g())) {
	for (Size i = 0; i < count; i++) {
		*first++ = g();
	}
	return first;
}

template <typename InputIt, typename EndIt, typename... Params>
constexpr InputIt call_each(InputIt first, EndIt last, Params&&... params) {
	while (first != last) {
		(*first)(std::forward<Params>(params)...);
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
		if (first == n_first or n_first == last)
			return;

		ForwardIt read = n_first;
		ForwardIt write = first;

		while (read != last) {
			*(write++) = std::move(*(read++));
		}

		return;
	}

} // namespace detail

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
	const zip_iterator operator++(int) {
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
	const zip_iterator operator++(int) {
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
 * @param begin1,end1 The first range.
 * @param begin2 The beginning of the second range.
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
