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
 * @brief Provides general-purpose algorithms, similar to the <algorithms>
 * header.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

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
constexpr auto repeat(std::size_t N, Callable func) noexcept(noexcept(func()))
    -> return_assert_t<is_invocable<Callable>::value, void> {
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
constexpr auto erase(Container& c, const Elem& val) noexcept(
    noexcept(c.erase(std::remove(c.begin(), c.end(), val), c.end()))) -> void {
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
constexpr auto erase_if(Container& c, UnaryPredicate p) noexcept(
    noexcept(c.erase(std::remove_if(c.begin(), c.end(), std::ref(p)), c.end())))
    -> void {
	c.erase(std::remove_if(c.begin(), c.end(), std::ref(p)), c.end());
	return;
}

/**
 * @brief Synthesize an equivalence relation from <.
 *
 * @return bool Whether a is equivalent under < to b.
 */
template <typename Obj>
KBLIB_NODISCARD constexpr auto equals(const Obj& a,
                                      const Obj& b) noexcept(noexcept(a < b))
    -> bool {
	return not (a < b) and not (b < a);
}

/**
 * @brief Synthesize an equivalence relation from comp.
 *
 * @return bool Whether a is equivalent under comp to b.
 */
template <typename Obj, typename Compare>
KBLIB_NODISCARD constexpr auto equals(const Obj& a, const Obj& b,
                                      Compare comp) noexcept(noexcept(comp(a,
                                                                           b)))
    -> bool {
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
	KBLIB_NODISCARD constexpr auto operator()(const Obj& a, const Obj& b,
	                                          Compare comp) const
	    noexcept(noexcept(equals(a, b, comp))) -> bool {
		return equals(a, b, comp);
	}
};

template <typename Obj>
struct equivalent<void, Obj> {
	KBLIB_NODISCARD constexpr auto operator()(const Obj& a, const Obj& b) const
	    noexcept(noexcept(equals(a, b))) -> bool {
		return equals(a, b);
	}
};

template <typename Compare>
struct equivalent<Compare, void> {
	template <typename Obj>
	KBLIB_NODISCARD constexpr auto operator()(const Obj& a, const Obj& b,
	                                          Compare comp) const
	    noexcept(noexcept(equals(a, b, comp))) -> bool {
		return equals(a, b, comp);
	}
};

template <>
struct equivalent<void, void> {
	template <typename Obj>
	KBLIB_NODISCARD constexpr auto operator()(const Obj& a, const Obj& b) const
	    noexcept(noexcept(equals(a, b))) -> bool {
		return equals(a, b);
	}
};

/**
 * @brief A constexpr version of std::accumulate
 */
template <typename InputIt, typename T>
KBLIB_NODISCARD constexpr auto accumulate(InputIt first, InputIt last, T init)
    -> T {
	for (; first != last; ++first) {
		init = std::move(init) + *first;
	}
	return init;
}

/**
 * @brief A constexpr version of std::accumulate
 */
template <class InputIt, class T, class BinaryOperation>
KBLIB_NODISCARD constexpr auto accumulate(InputIt first, InputIt last, T init,
                                          BinaryOperation op) -> T {
	for (; first != last; ++first) {
		init = op(std::move(init), *first);
	}
	return init;
}

/**
 * @brief Sum a range
 *
 * Convenience wrapper for std::accumulate. For an empty range, returns a
 * value-initialized temporary (usually 0). Deduces the correct type for the
 * initializer, which reduces risk of truncation and incorrect results.
 *
 * @param[in] first Beginning of range
 * @param[in] last End of range
 * @return The sum of the input range.
 */
template <typename InputIt>
KBLIB_NODISCARD constexpr auto sum(InputIt first, InputIt last)
    -> std::decay_t<decltype(*first)> {
	if (first == last) {
		return {};
	}
	auto init = *first++;
	return kblib::accumulate(first, last, std::move(init));
}

/**
 * @brief Fold a range over an operation.
 *
 * Convenience wrapper for std::accumulate. For an empty range, returns a
 * value-initialized temporary (usually 0). Deduces the correct type for the
 * initializer, which reduces risk of truncation and incorrect results.
 *
 * @param[in] first Beginning of range
 * @param[in] last End of range
 * @param[in] op The fold operation
 * @return The sum of the input range.
 */
template <typename InputIt, typename BinaryOperation>
KBLIB_NODISCARD constexpr auto sum(InputIt first, InputIt last,
                                   BinaryOperation op)
    -> std::decay_t<decltype(*first)> {
	if (first == last) {
		return {};
	}
	auto init = *first++;
	return kblib::accumulate(first, last, std::move(init), op);
}

/**
 * @brief Sum a range
 *
 * Convenience wrapper for std::accumulate. For an empty range, returns a
 * value-initialized temporary (usually 0). Deduces the correct type for the
 * initializer, which reduces risk of truncation and incorrect results.
 *
 * @param[in] r The range to sum
 * @return The sum of the input range.
 */
template <typename Range>
KBLIB_NODISCARD constexpr auto sum(Range&& r) -> auto {
	using std::begin;
	using std::end;
	auto first = begin(r);
	auto last = end(r);
	if (first == last) {
		return std::decay_t<decltype(*first)>{};
	}
	auto init = *first++;
	return kblib::accumulate(first, last, std::move(init));
}

template <typename InputIt, typename EndIt, typename OutputIt, typename T,
          typename BinaryAccumulation, typename UnaryTransform>
constexpr auto transform_exclusive_scan(InputIt first, EndIt last,
                                        OutputIt d_first, T init,
                                        BinaryAccumulation accum,
                                        UnaryTransform proj) -> OutputIt {
	while (first != last) {
		*d_first++
		    = kblib::exchange(init, accum(std::move(init), proj(*first++)));
	}
	return d_first;
}

#if 0
template <typename InputIt, typename BinaryAccumulation,
          typename BinaryTransform>
KBLIB_NODISCARD constexpr auto
adjacent_reduce(InputIt begin, InputIt begin1, InputIt end,
                BinaryAccumulation acc, BinaryTransform op) {}

template <typename InputIt, typename BinaryTransform>
KBLIB_NODISCARD constexpr auto adjacent_transform(InputIt begin, InputIt begin1,
                                                  InputIt end,
                                                  BinaryTransform op) {}

template <typename InputIt, typename BinaryAccumulation,
          typename BinaryTransform>
KBLIB_NODISCARD constexpr auto
adjacent_inclusive_scan(InputIt begin, InputIt begin1, InputIt end,
                        BinaryAccumulation acc, BinaryTransform op) {}
#endif

/**
 * @brief Finds a value in range [begin, end). If not found, returns end. It
 * also allows for a sentinel end iterator.
 *
 * @param begin,end The range to search in
 * @param value The value to search for
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename EndIt, typename Elem>
KBLIB_NODISCARD constexpr auto find(
    ForwardIt begin, EndIt end,
    const Elem& value) noexcept(noexcept(*begin == value)) -> ForwardIt {
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
KBLIB_NODISCARD constexpr auto find(
    ForwardIt begin, EndIt end, const Elem& value,
    Comp&& comp) noexcept(noexcept(comp(*begin, value))) -> ForwardIt {
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
KBLIB_NODISCARD constexpr auto find_if(
    ForwardIt begin, EndIt end,
    UnaryPredicate&& pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> ForwardIt {
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
KBLIB_NODISCARD constexpr auto find_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate&& pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> ForwardIt {
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
KBLIB_NODISCARD constexpr auto find_last(
    ForwardIt begin, EndIt end,
    const Elem& value) noexcept(noexcept(*begin == value)) -> ForwardIt {
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
KBLIB_NODISCARD constexpr auto find_last_if(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> ForwardIt {
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
KBLIB_NODISCARD constexpr auto find_last_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> ForwardIt {
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
KBLIB_NODISCARD constexpr auto find_in(
    ForwardIt begin, EndIt end,
    const Elem& value) noexcept(noexcept(*begin == value)) -> size_t {
	return to_unsigned(kblib::find(begin, end, value) - begin);
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
KBLIB_NODISCARD constexpr auto find_in_if(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> size_t {
	return to_unsigned(kblib::find_if(begin, end, pred) - begin);
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
KBLIB_NODISCARD constexpr auto find_in_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> size_t {
	return to_unsigned(kblib::find_if_not(begin, end, pred) - begin);
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
KBLIB_NODISCARD constexpr auto find_last_in(
    ForwardIt begin, EndIt end,
    const Elem& value) noexcept(noexcept(*begin == value)) -> size_t {
	return to_unsigned(kblib::find_last(begin, end, value) - begin);
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
KBLIB_NODISCARD constexpr auto find_last_in_if(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> size_t {
	return to_unsigned(kblib::find_last_if(begin, end, pred) - begin);
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
KBLIB_NODISCARD constexpr auto find_last_in_if_not(
    ForwardIt begin, EndIt end,
    UnaryPredicate pred) noexcept(noexcept(kblib::invoke(pred, *begin)))
    -> size_t {
	return to_unsigned(kblib::find_last_if_not(begin, end, pred) - begin);
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
KBLIB_NODISCARD constexpr auto
find_in(const Container& c, const T& value) noexcept(
    noexcept(*std::declval<iterator_type_for_t<const Container>&>() == value))
    -> size_t {
	using std::begin;
	using std::end;
	return to_unsigned(kblib::find(begin(c), end(c), value) - begin(c));
}
#if 0
template<typename ExecutionPolicy, typename Container, typename T>
KBLIB_NODISCARD constexpr auto find_in(ExecutionPolicy&& policy,
                                         const Container& c, const T& v) -> size_t {
  return to_unsigned(std::find(policy, std::begin(c), std::end(c), v) - std::begin(c));
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
KBLIB_NODISCARD constexpr auto find_in_if(
    const Container& c,
    UnaryPredicate
        pred) noexcept(noexcept(kblib::invoke(pred,
                                              *std::declval<iterator_type_for_t<
                                                  const Container>&>())))
    -> size_t {
	using std::begin;
	using std::end;
	return to_unsigned(kblib::find_if(begin(c), end(c), pred) - begin(c));
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
KBLIB_NODISCARD constexpr auto find_in_if_not(
    const Container& c,
    UnaryPredicate
        pred) noexcept(noexcept(kblib::invoke(pred,
                                              *std::declval<iterator_type_for_t<
                                                  const Container>&>())))
    -> size_t {
	using std::begin;
	using std::end;
	return to_unsigned(kblib::find_if_not(begin(c), end(c), pred) - begin(c));
}
#if 0
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto find_in_if(ExecutionPolicy&& policy,
                                            const Container& c,
                                            UnaryPredicate p) -> size_t {
  return std::find_if(policy, std::begin(c), std::end(c), p) - std::begin(c);
}
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto find_in_if_not(ExecutionPolicy&& policy,
                                                const Container& c,
                                                UnaryPredicate p) -> size_t {
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
KBLIB_NODISCARD constexpr auto
find_last_in(const Container& c, const T& value) noexcept(
    noexcept(*std::declval<iterator_type_for_t<const Container>&>() == value))
    -> size_t {
	using std::begin;
	using std::end;
	return to_unsigned(kblib::find_last(begin(c), end(c), value) - begin(c));
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
KBLIB_NODISCARD constexpr auto find_last_in_if(
    const Container& c,
    UnaryPredicate
        pred) noexcept(noexcept(kblib::invoke(pred,
                                              *std::declval<iterator_type_for_t<
                                                  const Container>&>())))
    -> size_t {
	using std::begin;
	using std::end;
	return to_unsigned(kblib::find_last_if(begin(c), end(c), pred) - begin(c));
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
KBLIB_NODISCARD constexpr auto find_last_in_if_not(
    const Container& c,
    UnaryPredicate
        pred) noexcept(noexcept(kblib::invoke(pred,
                                              *std::declval<iterator_type_for_t<
                                                  const Container>&>())))
    -> size_t {
	using std::begin;
	using std::end;
	return to_unsigned(kblib::find_last_if_not(begin(c), end(c), pred)
	                   - begin(c));
}

template <typename InputIt1, typename EndIt1, typename InputIt2,
          typename BinaryPredicate = std::equal_to<>>
KBLIB_NODISCARD constexpr auto find_match(InputIt1 begin1, EndIt1 end1,
                                          InputIt2 begin2, BinaryPredicate cmp)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value
                       and is_invocable<BinaryPredicate, decltype(*begin1),
                                        decltype(*begin2)>::value,
                   std::pair<InputIt1, InputIt2>> {
	while (begin1 != end1) {
		if (kblib::invoke(cmp, *begin1++, *begin2++)) {
			return std::make_pair(begin1, begin2);
		}
	}
	return std::make_pair(begin1, begin2);
}
template <typename InputIt1, typename EndIt1, typename InputIt2,
          typename EndIt2, typename BinaryPredicate = std::equal_to<>>
KBLIB_NODISCARD constexpr auto find_match(InputIt1 begin1, EndIt1 end1,
                                          InputIt2 begin2, EndIt2 end2,
                                          BinaryPredicate cmp)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value
                       and is_invocable<BinaryPredicate, decltype(*begin1),
                                        decltype(*begin2)>::value,
                   std::pair<InputIt1, InputIt2>> {
	while (begin1 != end1 and begin2 != end2) {
		if (kblib::invoke(cmp, *begin1++, *begin2++)) {
			return std::make_pair(begin1, begin2);
		}
	}
	return std::make_pair(begin1, begin2);
}

/**
 * @brief Checks if a given range starts with a particular subrange.
 */
template <typename InputIt1, typename EndIt1, typename InputIt2,
          typename EndIt2, typename BinaryPred>
KBLIB_NODISCARD constexpr auto starts_with(InputIt1 begin1, EndIt1 end1,
                                           InputIt2 begin2, EndIt2 end2,
                                           BinaryPred pred)
    -> enable_if_t<
        (is_input_iterator_v<InputIt1> and is_input_iterator_v<InputIt2>) //
        and not (is_random_access_iterator_v<
                     InputIt1> and is_random_access_iterator_v<InputIt2>),
        bool> {
	while (begin1 != end1 and begin2 != end2) {
		if (not kblib::invoke(pred, *begin1++, *begin2++)) {
			return false;
		}
	}
	return begin2 == end2;
}

/**
 * @brief Checks if a given range starts with a particular subrange.
 */
template <typename RandomAccessIt1, typename RandomAccessIt2,
          typename BinaryPred = std::equal_to<>>
KBLIB_NODISCARD constexpr auto starts_with(RandomAccessIt1 begin1,
                                           RandomAccessIt1 end1,
                                           RandomAccessIt2 begin2,
                                           RandomAccessIt2 end2,
                                           BinaryPred pred = {})
    -> enable_if_t<
        is_random_access_iterator_v<
            RandomAccessIt1> and is_random_access_iterator_v<RandomAccessIt2>,
        bool> {
	if (end2 - begin2 > end1 - begin1) {
		return false;
	} else {
		auto N = end2 - begin2;
		return kblib::equal(begin1, begin1 + N, begin2, pred);
	}
}

/**
 * @brief Checks if a given range ends with a particular subrange.
 */
template <typename BidirIt1, typename BidirIt2,
          typename BinaryPred = std::equal_to<>>
KBLIB_NODISCARD constexpr auto ends_with(BidirIt1 begin1, BidirIt1 end1,
                                         BidirIt2 begin2, BidirIt2 end2,
                                         BinaryPred pred = {})
    -> enable_if_t<
        (is_bidirectional_iterator_v<BidirIt1>      //
         and is_bidirectional_iterator_v<BidirIt2>) //
        and not (is_random_access_iterator_v<
                     BidirIt1> and is_random_access_iterator_v<BidirIt2>),
        bool> {
	while (begin1 != end1 and begin2 != end2) {
		if (not kblib::invoke(pred, *--end1, *--end2)) {
			return false;
		}
	}
	return begin2 == end2;
}

/**
 * @brief Checks if a given range ends with a particular subrange.
 */
template <typename RandomAccessIt1, typename RandomAccessIt2,
          typename BinaryPred = std::equal_to<>>
KBLIB_NODISCARD constexpr auto ends_with(RandomAccessIt1 begin1,
                                         RandomAccessIt1 end1,
                                         RandomAccessIt2 begin2,
                                         RandomAccessIt2 end2,
                                         BinaryPred pred = {})
    -> enable_if_t<
        is_random_access_iterator_v<
            RandomAccessIt1> and is_random_access_iterator_v<RandomAccessIt2>,
        bool> {
	if (end2 - begin2 > end1 - begin1) {
		return false;
	} else {
		auto N = end2 - begin2;
		return kblib::equal(end1 - N, end1, begin2, pred);
	}
}

template <typename InputIt, typename EndIt, typename T, typename UnaryTransform>
KBLIB_NODISCARD constexpr auto first_result(InputIt begin, EndIt end, T def,
                                            UnaryTransform op)
    -> enable_if_t<is_input_iterator<InputIt>::value,
                   std::decay_t<decltype(op(*begin))>> {
	for (; begin != end; ++begin) {
		auto cur = op(*begin);
		if (cur != def) {
			return cur;
		}
	}
	return def;
}
template <typename InputIt1, typename EndIt1, typename InputIt2, typename T,
          typename BinaryTransform>
KBLIB_NODISCARD constexpr auto first_result(InputIt1 begin1, EndIt1 end1,
                                            InputIt2 begin2, T def,
                                            BinaryTransform op)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value,
                   std::decay_t<decltype(op(*begin1, *begin2))>> {
	for (; begin1 != end1; ++begin1, ++begin2) {
		auto cur = op(*begin1, *begin2);
		if (cur != def) {
			return cur;
		}
	}
	return def;
}
template <typename InputIt1, typename EndIt1, typename InputIt2,
          typename EndIt2, typename T, typename BinaryTransform>
KBLIB_NODISCARD constexpr auto first_result(InputIt1 begin1, EndIt1 end1,
                                            InputIt2 begin2, EndIt2 end2, T def,
                                            BinaryTransform op)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value,
                   std::decay_t<decltype(op(*begin1, *begin2))>> {
	for (; begin1 != end1 and begin2 != end2; ++begin1, ++begin2) {
		auto cur = op(*begin1, *begin2);
		if (cur != def) {
			return cur;
		}
	}
	return def;
}

template <typename InputIt, typename EndIt, typename T, typename UnaryTransform,
          typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto first_result_if(InputIt begin, EndIt end, T def,
                                               UnaryTransform op,
                                               UnaryPredicate ch)
    -> enable_if_t<is_input_iterator<InputIt>::value, decltype(op(*begin))> {
	for (; begin != end; ++begin) {
		if (ch(*begin)) {
			return op(*begin);
		}
	}
	return def;
}
template <typename InputIt1, typename EndIt1, typename InputIt2, typename T,
          typename BinaryTransform, typename BinaryPredicate>
KBLIB_NODISCARD constexpr auto first_result_if(InputIt1 begin1, EndIt1 end1,
                                               InputIt2 begin2, T def,
                                               BinaryTransform op,
                                               BinaryPredicate ch)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value,
                   decltype(op(*begin1, *begin2))> {
	for (; begin1 != end1; ++begin1, ++begin2) {
		if (ch(*begin1, *begin2)) {
			return op(*begin1, *begin2);
		}
	}
	return def;
}
template <typename InputIt1, typename EndIt1, typename InputIt2,
          typename EndIt2, typename T, typename BinaryTransform,
          typename BinaryPredicate>
KBLIB_NODISCARD constexpr auto first_result_if(InputIt1 begin1, EndIt1 end1,
                                               InputIt2 begin2, EndIt2 end2,
                                               T def, BinaryTransform op,
                                               BinaryPredicate ch)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value,
                   decltype(op(*begin1, *begin2))> {
	for (; begin1 != end1 and begin2 != end2; ++begin1, ++begin2) {
		if (ch(*begin1, *begin2)) {
			return op(*begin1, *begin2);
		}
	}
	return def;
}

#if KBLIB_USE_CXX17
template <typename T>
struct is_optional : std::false_type {};
template <typename U>
struct is_optional<std::optional<U>> : std::true_type {};

template <typename InputIt, typename EndIt, typename T, typename UnaryTransform>
KBLIB_NODISCARD constexpr auto first_result_opt(InputIt begin, EndIt end, T def,
                                                UnaryTransform op)
    -> enable_if_t<is_input_iterator<InputIt>::value,
                   std::decay_t<decltype(op(*begin))>> {
	for (; begin != end; ++begin) {
		auto cur = op(*begin);
		if (cur) {
			return cur;
		}
	}
	return def;
}
template <typename InputIt1, typename EndIt1, typename InputIt2, typename T,
          typename BinaryTransform>
KBLIB_NODISCARD constexpr auto first_result_opt(InputIt1 begin1, EndIt1 end1,
                                                InputIt2 begin2, T def,
                                                BinaryTransform op)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value,
                   std::decay_t<decltype(op(*begin1, *begin2))>> {
	for (; begin1 != end1; ++begin1, ++begin2) {
		auto cur = op(*begin1, *begin2);
		if (cur) {
			return cur;
		}
	}
	return def;
}
template <typename InputIt1, typename EndIt1, typename InputIt2,
          typename EndIt2, typename T, typename BinaryTransform>
KBLIB_NODISCARD constexpr auto first_result_opt(InputIt1 begin1, EndIt1 end1,
                                                InputIt2 begin2, EndIt2 end2,
                                                T def, BinaryTransform op)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value,
                   std::decay_t<decltype(op(*begin1, *begin2))>> {
	for (; begin1 != end1 and begin2 != end2; ++begin1, ++begin2) {
		auto cur = op(*begin1, *begin2);
		if (cur) {
			return cur;
		}
	}
	return def;
}
#endif

/**
 * @brief Determine if pred is true for every element of the range.
 */
template <typename InputIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto all_of(InputIt begin, InputIt end,
                                      UnaryPredicate pred)
    -> enable_if_t<is_input_iterator<InputIt>::value, bool> {
	for (; begin != end; ++begin) {
		if (not kblib::invoke(pred, *begin)) {
			return false;
		}
	}
	return true;
}
/**
 * @brief Determine if pred is true for every element of the range.
 */
template <typename Range, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto all_of(Range&& rng, UnaryPredicate pred)
    -> enable_if_t<is_iterable<Range>::value, bool> {
	for (auto&& el : std::forward<Range>(rng)) {
		if (not kblib::invoke(pred, static_cast<decltype(el)>(el))) {
			return false;
		}
	}
	return true;
}
/**
 * @brief Determine if pred is false for every element of the range.
 */
template <typename InputIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto none_of(InputIt begin, InputIt end,
                                       UnaryPredicate pred)
    -> enable_if_t<is_input_iterator<InputIt>::value, bool> {
	for (; begin != end; ++begin) {
		if (kblib::invoke(pred, *begin)) {
			return false;
		}
	}
	return true;
}
/**
 * @brief Determine if pred is true for every element of the range.
 */
template <typename Range, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto none_of(Range&& rng, UnaryPredicate pred)
    -> enable_if_t<is_iterable<Range>::value, bool> {
	for (auto&& el : std::forward<Range>(rng)) {
		if (kblib::invoke(pred, static_cast<decltype(el)>(el))) {
			return false;
		}
	}
	return true;
}
/**
 * @brief Determine if pred is true for at least one element of the range.
 */
template <typename InputIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto any_of(InputIt begin, InputIt end,
                                      UnaryPredicate pred)
    -> enable_if_t<is_input_iterator<InputIt>::value, bool> {
	for (; begin != end; ++begin) {
		if (kblib::invoke(pred, *begin)) {
			return true;
		}
	}
	return false;
}
/**
 * @brief Determine if pred is true for every element of the range.
 */
template <typename Range, typename UnaryPredicate>
KBLIB_NODISCARD constexpr auto any_of(Range&& rng, UnaryPredicate pred)
    -> enable_if_t<is_iterable<Range>::value, bool> {
	for (auto&& el : std::forward<Range>(rng)) {
		if (kblib::invoke(pred, static_cast<decltype(el)>(el))) {
			return true;
		}
	}
	return false;
}

/**
 * @brief Determine if a range contains a value.
 */
template <typename InputIt, typename Value>
KBLIB_NODISCARD constexpr auto contains(
    InputIt begin, InputIt end,
    const Value& val) noexcept(noexcept(*begin == val))
    -> enable_if_t<is_input_iterator<InputIt>::value, bool> {
	return kblib::any_of(begin, end, [&](const auto& e) { return e == val; });
}

/**
 * @brief Determine if a range contains a value.
 */
template <typename Set, typename Value>
KBLIB_NODISCARD constexpr auto contains(
    const Set& set,
    const Value&
        val) noexcept(noexcept(*std::declval<iterator_type_for_t<const Set>&>()
                               == val))
    -> enable_if_t<is_iterable<Set>::value, bool> {
	using std::begin;
	using std::end;
	return kblib::any_of(begin(set), end(set),
	                     [&](const auto& e) { return e == val; });
}

template <typename InputIt1, typename InputIt2>
KBLIB_NODISCARD constexpr auto contains_any(InputIt1 begin, InputIt1 end,
                                            InputIt2 n_begin, InputIt2 n_end)
    -> enable_if_t<is_input_iterator<InputIt1>::value
                       and is_input_iterator<InputIt2>::value,
                   bool> {
	return kblib::any_of(begin, end, [=](const auto& v) {
		return kblib::contains(n_begin, n_end, v);
	});
}

template <typename InputIt, typename Range2>
KBLIB_NODISCARD constexpr auto contains_any(InputIt begin, InputIt end,
                                            Range2&& needle)
    -> enable_if_t<is_input_iterator<InputIt>::value
                       and is_iterable<Range2>::value,
                   bool> {
	return kblib::any_of(begin, end, [&needle](const auto& v) {
		return kblib::contains(needle, v);
	});
}

template <typename Range1, typename Range2>
KBLIB_NODISCARD constexpr auto contains_any(Range1&& haystack, Range2&& needle)
    -> enable_if_t<is_iterable<Range1>::value and is_iterable<Range2>::value,
                   bool> {
	using std::begin;
	using std::end;
	return kblib::any_of(
	    begin(haystack), end(haystack),
	    [&needle](const auto& v) { return kblib::contains(needle, v); });
}

template <typename ForwardIt, typename EndIt, typename Compare = std::less<>>
constexpr auto max_element(ForwardIt first, EndIt last, Compare comp = {})
    -> ForwardIt {
	if (first == last) {
		return first;
	}

	ForwardIt largest = first;
	++first;
	for (; first != last; ++first) {
		if (comp(*largest, *first)) {
			largest = first;
		}
	}
	return largest;
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
KBLIB_NODISCARD constexpr auto get_max_n_old(It first, It last,
                                             std::size_t count, Comp cmp = {})
    -> SequenceContainer {
	using std::begin;
	using std::end;
	assert(first + count <= last);
	SequenceContainer c{first, first + count};
	std::for_each(first + count, last, [&](const auto& v) {
		auto& min_v = *std::min_element(begin(c), end(c), cmp);
		if (kblib::invoke(cmp, min_v, v)) {
			min_v = v;
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
KBLIB_NODISCARD constexpr auto get_max_n_old(It first, It last,
                                             std::size_t count, Comp cmp = {})
    -> SetlikeContainer {
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
KBLIB_NODISCARD constexpr auto get_max_n(It first, It last, std::size_t count,
                                         Comp cmp = {}) -> SequenceContainer {
	using std::begin;
	using std::end;
	SequenceContainer c(count);
	std::partial_sort_copy(first, last, begin(c), end(c),
	                       [&](auto&& a, auto&& b) -> decltype(auto) {
		                       return kblib::invoke(cmp,
		                                            std::forward<decltype(b)>(b),
		                                            std::forward<decltype(a)>(a));
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
KBLIB_NODISCARD constexpr auto get_max_n(It first, It last, std::size_t count,
                                         Comp cmp = {}) -> SetlikeContainer {
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
 * @return OutputIt An iterator to past the last element written.
 */
template <typename Comp = std::less<>, typename InputIt, typename OutputIt,
          typename Elem = typename std::iterator_traits<InputIt>::value_type>
constexpr auto get_max_n(InputIt first, InputIt last, OutputIt d_begin,
                         std::size_t count, Comp cmp = {})
    -> return_assert_t<is_output_iterator_for<OutputIt, Elem>::value,
                       OutputIt> {
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
constexpr auto for_each(ForwardIt first, EndIt last, ForwardIt2 second,
                        BinaryFunction f) -> BinaryFunction {
	for (; first != last; (void)++first, (void)++second) {
		kblib::invoke(f, *first, *second);
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
constexpr auto for_each_n(ForwardIt first, Size n, ForwardIt2 second,
                          BinaryFunction f)
    -> std::pair<ForwardIt, ForwardIt2> {
	for (Size i = 0; i < n; (void)++first, (void)++second, (void)++i) {
		kblib::invoke(f, *first, *second);
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
constexpr auto copy(InputIt first, EndIt last, OutputIt out) -> OutputIt {
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
constexpr auto copy_if(InputIt first, EndIt last, OutputIt out,
                       UnaryPredicate pred) -> OutputIt {
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
constexpr auto copy_n(InputIt first, Size count, OutputIt out) -> OutputIt {
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
constexpr auto copy_n_if(InputIt first, Size count, OutputIt out,
                         UnaryPredicate pred) -> OutputIt {
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
constexpr auto replace_copy_if(InputIt first, EndIt last, OutputIt out,
                               UnaryPredicate pred, const T& new_value)
    -> OutputIt {
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
constexpr auto replace_copy_n_if(InputIt first, Size count, OutputIt out,
                                 UnaryPredicate pred, const T& new_value)
    -> OutputIt {
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

template <typename ForwardIt1, typename ForwardIt2, typename ForwardIt3,
          typename OutputIt, typename BinaryPredicate = std::equal_to<>>
constexpr auto search_replace_copy(ForwardIt1 h_begin, ForwardIt1 h_end,
                                   ForwardIt2 n_begin, ForwardIt2 n_end,
                                   ForwardIt3 r_begin, ForwardIt3 r_end,
                                   OutputIt d_begin,
                                   BinaryPredicate Compare = {}) -> OutputIt {
	if (n_begin == n_end) {
		return copy(h_begin, h_end, d_begin);
	} else {
		const auto needle_length = std::distance(n_begin, n_end);
		while (h_begin != h_end) {
			const auto found
			    = std::search(h_begin, h_end, n_begin, n_end, Compare);
			d_begin = kblib::copy(h_begin, found, d_begin);
			h_begin = found;
			if (h_begin != h_end) {
				d_begin = copy(r_begin, r_end, d_begin);
				std::advance(h_begin, needle_length);
			}
		}
		return d_begin;
	}
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
constexpr auto rotate(ForwardIt first, ForwardIt n_first,
                      ForwardIt last) noexcept(noexcept(swap(*first, *first)))
    -> ForwardIt {
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
		// iter_swap is not constexpr
		// std::iter_swap(write++, read++);
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
constexpr auto generate(OutputIt first, EndIt last,
                        Generator g) noexcept(noexcept(*++first = g()))
    -> OutputIt {
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
constexpr auto generate_n(OutputIt first, Size count,
                          Generator g) noexcept(noexcept(*first++ = g()))
    -> OutputIt {
	for (Size i = 0; i < count; i++) {
		*first++ = g();
	}
	return first;
}

template <typename ForwardIt, typename T>
constexpr auto iota(ForwardIt first, ForwardIt last, T value) noexcept(
    noexcept(*first++ = value) and noexcept(++value)) -> void {
	while (first != last) {
		*first++ = value;
		++value;
	}
}

// For some reason these long noexcept specifications really trip
// up clang-format
// clang-format off
template <typename ForwardIt, typename T, typename UnaryOperation>
constexpr auto iota(
    ForwardIt first, ForwardIt last, T value,
    UnaryOperation unary_op
	) noexcept(noexcept(*first++ = value)
              and noexcept(kblib::invoke(unary_op,
                                         std::move(value))))
    -> void {
	// clang-format on
	while (first != last) {
		*first++ = value;
		value = kblib::invoke(unary_op, std::move(value));
	}
}

// clang-format off
template <typename InputIt, typename EndIt, typename... Params>
constexpr auto call_each(
    InputIt first, EndIt last, Params&&... params
	) noexcept(noexcept(kblib::invoke(*first++,
                                     std::forward<Params>(params)...)))
    -> InputIt {
	// clang-format on
	while (first != last) {
		kblib::invoke(*first++, std::forward<Params>(params)...);
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
constexpr auto transform(InputIt first, EndIt last, OutputIt d_first,
                         UnaryOperation unary_op) -> OutputIt {
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
constexpr auto transform(InputIt first, EndIt last, InputIt first2,
                         OutputIt d_first, BinaryOperation binary_op)
    -> OutputIt {
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
constexpr auto transform_if(InputIt first, EndIt last, OutputIt d_first,
                            UnaryPredicate pred, UnaryOperation unary_op)
    -> OutputIt {
	while (first != last) {
		if (kblib::invoke(pred, *first)) {
			*d_first++ = kblib::invoke(unary_op, *first);
		}
		++first;
	}
	return d_first;
}

namespace detail_algorithm {

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
	constexpr auto shift_backward(
	    ForwardIt first, ForwardIt n_first,
	    ForwardIt last) noexcept(noexcept(*first = std::move(*first))) -> void {
		if (first == n_first or n_first == last)
			return;

		ForwardIt read = n_first;
		ForwardIt write = first;

		while (read != last) {
			*(write++) = std::move(*(read++));
		}

		return;
	}

} // namespace detail_algorithm

} // namespace kblib

#endif // ALGORITHM_H
