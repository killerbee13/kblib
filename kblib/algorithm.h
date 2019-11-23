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
 * @brief Abbreviation of the erase-remove idiom as a free function.
 *
 * @param c The container to erase from.
 * @param val The value to remove.
 */
template <typename Container, typename Elem>
constexpr void erase(Container& c, const Elem& val) {
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
constexpr void erase_if(Container& c, UnaryPredicate p) {
	c.erase(std::remove_if(c.begin(), c.end(), std::ref(p)), c.end());
	return;
}
/**
 * @brief Finds a value in range [begin, end). If not found, returns end.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param value The value to search for
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename Elem, typename Comp>
KBLIB_NODISCARD constexpr ForwardIt find(ForwardIt begin, ForwardIt end,
                                         const Elem& value) {
	auto equal = [](const Elem& a, const Elem& b) {
		return !(a < b) && !(b < a);
	};
	while (begin != end && !equal(*begin, value)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds a value in range [begin, end). If not found, returns end.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param value The value to search for
 * @param comp The comparison function
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename Elem, typename Comp>
KBLIB_NODISCARD constexpr ForwardIt find(ForwardIt begin, ForwardIt end,
                                         const Elem& value, Comp&& comp) {
	auto equal = [&comp](const Elem& a, const Elem& b) {
		return !comp(a, b) && !comp(b, a);
	};
	while (begin != end && !equal(*begin, value)) {
		++begin;
	}
	return begin;
}

/**
 * @brief Finds the first value in range [begin, end) for which pred returns
 * true. If not found, returns end.
 *
 * @param begin Beginning of the range to search
 * @param end One past the end of the range
 * @param pred The predicate to scan with
 * @return It Either the position of the found value, or end if not found
 */
template <typename ForwardIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt find_if(ForwardIt begin, ForwardIt end,
                                            UnaryPredicate&& pred) {
	while (begin != end && !pred(*begin)) {
		++begin;
	}
	return begin;
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
template <typename ForwardIt, typename Elem>
KBLIB_NODISCARD constexpr ForwardIt find_last(ForwardIt begin, ForwardIt end,
                                              const Elem& v) {
	if (begin == end) {
		return end;
	}
	auto result = end;
	while (true) {
		auto new_result = std::find(begin, end, v);
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
template <typename ForwardIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt find_last_if(ForwardIt begin, ForwardIt end,
                                                 UnaryPredicate p) {
	if (begin == end) {
		return end;
	}
	auto result = end;
	while (true) {
		auto new_result = kblib::find_if(begin, end, p);
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
template <typename ForwardIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr ForwardIt
find_last_if_not(ForwardIt begin, ForwardIt end, UnaryPredicate p) {
	if (begin == end) {
		return end;
	}
	auto result = end;
	while (true) {
		auto new_result = std::find_if_not(begin, end, p);
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
template <typename ForwardIt, typename Elem>
KBLIB_NODISCARD constexpr size_t find_in(ForwardIt begin, ForwardIt end,
                                         const Elem& v) {
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
template <typename ForwardIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if(ForwardIt begin, ForwardIt end,
                                            UnaryPredicate p) {
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
template <typename ForwardIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if_not(ForwardIt begin, ForwardIt end,
                                                UnaryPredicate p) {
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
template <typename ForwardIt, typename Elem>
KBLIB_NODISCARD constexpr size_t find_last_in(ForwardIt begin, ForwardIt end,
                                              const Elem& v) {
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
template <typename ForwardIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_last_in_if(ForwardIt begin, ForwardIt end,
                                                 UnaryPredicate p) {
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
template <typename ForwardIt, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t
find_last_in_if_not(ForwardIt begin, ForwardIt end, UnaryPredicate p) {
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
KBLIB_NODISCARD constexpr size_t find_in(const Container& c, const T& v) {
	return std::find(std::begin(c), std::end(c), v) - std::begin(c);
}
#if 0
template<typename ExecutionPolicy, typename Container, typename T>
KBLIB_NODISCARD constexpr size_t find_in(ExecutionPolicy&& policy, const Container& c, const T& v) {
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
 * @param p The predicate to check.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if(const Container& c,
                                            UnaryPredicate p) {
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
KBLIB_NODISCARD constexpr size_t find_in_if_not(const Container& c,
                                                UnaryPredicate p) {
	return std::find_if_not(std::begin(c), std::end(c), p) - std::begin(c);
}
#if 0
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if(ExecutionPolicy&& policy, const Container& c, UnaryPredicate p) {
  return std::find_if(policy, std::begin(c), std::end(c), p) - std::begin(c);
}
template<typename ExecutionPolicy, typename Container, typename UnaryPredicate>
KBLIB_NODISCARD constexpr size_t find_in_if_not(ExecutionPolicy&& policy, const Container& c, UnaryPredicate p) {
  return std::find_if_not(policy, std::begin(c), std::end(c), p) - std::begin(c);
}
#endif

/**
 * @brief Find the last element in c equal to v and return the position.
 *
 * Equivalent to find_last_in(std::begin(c), std::end(c), v)
 *
 * @param c The container to search.
 * @param v The value to search for.
 * @return size_t The position of the element found, or c.size() if not.
 */
template <typename Container, typename T>
KBLIB_NODISCARD constexpr size_t find_last_in(const Container& c, const T& v) {
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
KBLIB_NODISCARD constexpr size_t find_last_in_if(const Container& c,
                                                 UnaryPredicate p) {
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
KBLIB_NODISCARD constexpr size_t find_last_in_if_not(const Container& c,
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
template <typename Container, typename Comp = std::less<>, typename It,
          enable_if_t<is_linear_container_v<Container>, int> = 0>
KBLIB_NODISCARD constexpr Container
get_max_n_old(It begin, It end, std::size_t count, Comp cmp = {}) {
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
          enable_if_t<is_setlike_v<Container>, int> = 0>
KBLIB_NODISCARD constexpr Container
get_max_n_old(It begin, It end, std::size_t count, Comp cmp = {}) {
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
template <typename Container, typename Comp = std::less<>, typename It,
          enable_if_t<is_linear_container_v<Container>, int> = 0>
KBLIB_NODISCARD constexpr Container
get_max_n(It begin, It end, std::size_t count, Comp cmp = {}) {
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
          enable_if_t<is_setlike_v<Container>, int> = 0>
KBLIB_NODISCARD constexpr Container
get_max_n(It begin, It end, std::size_t count, Comp cmp = {}) {
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
 * @return return_assert_t<is_output_iterator<OutputIt, ElementT>::value,
 * OutputIt> An iterator derived by assigning to and advancing d_begin count
 * times.
 */
template <typename Comp = std::less<>, typename InputIt, typename OutputIt,
          typename Elem = typename std::iterator_traits<InputIt>::value_type>
constexpr auto get_max_n(InputIt begin, InputIt end, OutputIt d_begin,
                         std::size_t count, Comp cmp = {})
    -> return_assert_t<is_output_iterator<OutputIt, Elem>::value, OutputIt> {
	auto temp = get_max_n<std::vector<Elem>>(begin, end, count, cmp);
	return std::move(temp.begin(), temp.end(), d_begin);
}

/**
 * @brief Applies a binary operation to each pair of corresponding elements in
 * two input ranges.
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
template <typename ForwardIt, typename ForwardIt2, typename BinaryFunction>
KBLIB_NODISCARD constexpr BinaryFunction
for_each(ForwardIt first, ForwardIt last, ForwardIt2 second, BinaryFunction f) {
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
KBLIB_NODISCARD constexpr std::pair<ForwardIt, ForwardIt2>
for_each_n(ForwardIt first, Size n, ForwardIt2 second, BinaryFunction f) {
	for (Size i = 0; i < n; (void)++first, (void)++second, (void)++i) {
		f(*first, *second);
	}
	return {first, second};
}

/**
 * @brief Copies those elements of [`first`, `std::advance(first, n)`) which
 * satisfy pred to out.
 *
 * @param first The beginning of the input range.
 * @param count The number of elements in the input range.
 * @param out The output range.
 * @param pred The predicate to apply.
 * @return OutputIt `std::advance(first, n)`
 */
template <typename InputIt, typename Size, typename OutputIt,
          typename UnaryPredicate>
KBLIB_NODISCARD constexpr OutputIt
copy_n_if(InputIt first, Size count, OutputIt out, UnaryPredicate pred) {
	for (Size i = 0; i < count; ++i) {
		if (pred(*first)) {
			*out++ = *first;
		}
		++first;
	}
	return out;
}

/**
 * @brief Copies an input range, but every element for which pred is true is
 * replaced by
 *
 * @param first The beginning of the input range.
 * @param count The number of elements to copy.
 * @param out The beginning of the output range.
 * @param pred The predicate to apply.
 * @param new_value The value to replace those elements for which pred is true
 * with.
 * @return OutputIt out after being incremented count times.
 */
template <typename InputIt, typename Size, typename OutputIt,
          typename UnaryPredicate, typename T>
KBLIB_NODISCARD constexpr OutputIt
replace_copy_n_if(InputIt first, Size count, OutputIt out, UnaryPredicate pred,
                  const T& new_value) {
	for (Size i = 0; i < count; ++i) {
		if (pred(*first)) {
			*out++ = *first;
		} else {
			*out++ = new_value;
		}
		++first;
	}
	return out;
}

namespace detail {

   template <typename F, typename... Args,
             enable_if_t<!std::is_member_pointer<fakestd::decay_t<F>>::value,
                         int> = 0>
   constexpr decltype(auto) do_invoke(F&& f, Args&&... args) {
		return std::forward<F>(f)(std::forward<Args>(args)...);
	}
	template <typename F, typename Object, typename... Args,
	          enable_if_t<!std::is_pointer<fakestd::decay_t<Object>>::value &&
	                          std::is_member_function_pointer<F>::value,
	                      int> = 0>
	constexpr decltype(auto) do_invoke(F f, Object&& obj, Args&&... args) {
		return (obj.*f)(std::forward<Args>(args)...);
	}
	template <typename F, typename Pointer, typename... Args,
	          enable_if_t<std::is_pointer<Pointer>::value &&
	                          std::is_member_function_pointer<F>::value,
	                      int> = 0>
	constexpr decltype(auto) do_invoke(F f, Pointer ptr, Args&&... args) {
		return (ptr->*f)(std::forward<Args>(args)...);
	}
	template <typename Member, typename Object,
	          enable_if_t<!std::is_pointer<fakestd::decay_t<Object>>::value &&
	                          std::is_member_object_pointer<Member>::value,
	                      int> = 0>
	constexpr decltype(auto) do_invoke(Member mem, Object&& obj) {
		return std::forward<Object>(obj).*mem;
	}
	template <typename Member, typename Pointer,
	          enable_if_t<std::is_pointer<Pointer>::value &&
	                          std::is_member_object_pointer<Member>::value,
	                      int> = 0>
	constexpr decltype(auto) do_invoke(Member mem, Pointer ptr) {
		return ptr.*mem;
	}
} // namespace detail

template <typename F, typename... Args>
constexpr decltype(auto) invoke(F&& f, Args&&... args) {
#if KBLIB_USE_CXX17
	return std::apply(std::forward<F>(f),
	                  std::forward_as_tuple(std::forward<Args>(args)...));
#else
	return detail::do_invoke(std::forward<F>(f), std::forward<Args>(args)...);
#endif
}

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
 * @brief transform applies the given function to a range and stores the result
 * in another range, beginning at d_first. The unary operation unary_op is
 * applied to the range defined by [first1, last1).
 *
 * @remark The expression `*d_first = std::invoke(unary_op, *first)` must be
 * valid and must not modify `*first`.
 *
 * @param first The beginning of the input range
 * @param last The end of the input range
 * @param d_first The beginning of the output range
 * @param unary_op The operation to apply
 * @return OutputIt Output iterator to the element past the last element
 * transformed
 */
template <typename InputIt, typename OutputIt, typename UnaryOperation>
constexpr OutputIt transform(InputIt first, InputIt last, OutputIt d_first,
                             UnaryOperation unary_op) {
	while (first != last) {
		*d_first++ = fakestd::invoke(unary_op, *first);
		++first;
	}
	return d_first;
}

/**
 * @brief transform applies the given function to a range and stores the result
 * in another range, beginning at d_first. The unary operation unary_op is
 * applied to the range defined by [first1, last1).
 *
 * @remark The expression `*d_first = std::invoke(binary_op, *first, *first2)`
 * must be valid and must not modify `*first` or `*first2`.
 *
 * @param first The beginning of the first input range
 * @param last The end of the first input range
 * @param first2 The beginning of the second input range
 * @param d_first The beginning of the output range
 * @param binary_op The operation to apply
 * @return OutputIt Output iterator to the element past the last element
 * transformed
 */
template <typename InputIt, typename InputIt2, typename OutputIt,
          typename BinaryOperation>
constexpr OutputIt transform(InputIt first, InputIt last, InputIt first2,
                             OutputIt d_first, BinaryOperation binary_op) {
	while (first != last) {
		*d_first++ = fakestd::invoke(binary_op, *first, *first2);
		++first;
		++first2;
	}
	return d_first;
}

/**
 * @brief transform applies the given function to a range and stores the result
 * in another range, beginning at d_first. The unary operation unary_op is
 * applied to the range defined by [first1, last1).
 *
 * @remark The expression `std::invoke(pred, *first)` must be valid and must
 * return a type convertible to `bool`, and must not modify `*first`
 * @remark The expression `*d_first = std::invoke(unary_op, *first)` must be
 * valid and must not modify `*first`.
 *
 * @param first The beginning of the input range
 * @param last The end of the input range
 * @param d_first The beginning of the output range
 * @param pred The predicate to apply
 * @param unary_op The operation to apply
 * @return OutputIt Output iterator to the element past the last element
 * transformed
 */
template <typename InputIt, typename OutputIt, typename UnaryPredicate,
          typename UnaryOperation>
constexpr OutputIt transform_if(InputIt first, InputIt last, OutputIt d_first,
                                UnaryPredicate pred, UnaryOperation unary_op) {
	while (first != last) {
		if (fakestd::invoke(pred, *first)) {
			*d_first++ = fakestd::invoke(unary_op, *first);
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
    Compare&& compare = {}) noexcept(noexcept(swap(*begin, *begin)) &&
                                     noexcept(compare(*begin, *begin))) {
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
    Compare&& compare = {}) noexcept(noexcept(*d_begin = *begin) &&
                                     noexcept(detail::shift_backward(d_begin,
                                                                     d_begin,
                                                                     d_end))) {
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
        {}) noexcept(noexcept(insertion_sort_copy(begin, end, d_begin, d_end,
                                                  std::forward<Compare>(
                                                      compare))) &&
                     noexcept(insertion_sort_copy(
                         std::make_reverse_iterator(begin),
                         std::make_reverse_iterator(end), d_begin, d_end,
                         std::forward<Compare>(compare)))) {
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
				return invoke(compare, invoke(transform, *a),
				              invoke(transform, *b));
			};
			if (end - begin < 8) {
				insertion_sort(begin, end, comp);
				return;
			} else {
				// TODO
			}
		}

		static constexpr void scratch(RandomAccessIt begin,
		                              const RandomAccessIt end,
		                              UnaryOperation&& transform,
		                              BinaryPredicate&& compare) {
			auto comp = [&compare, &transform](RandomAccessIt a,
			                                   RandomAccessIt b) {
				return invoke(compare, invoke(transform, *a),
				              invoke(transform, *b));
			};
			if (end - begin < 8) {
				insertion_sort(begin, end, comp);
				return;
			} else {
				// TODO
			}
		}

		template <typename RandomAccessIt2>
		static constexpr void copy(RandomAccessIt begin, const RandomAccessIt end,
		                           RandomAccessIt2 d_begin, RandomAccessIt2 d_end,
		                           UnaryOperation&& transform,
		                           BinaryPredicate&& compare) {
			auto comp = [&compare, &transform](RandomAccessIt a,
			                                   RandomAccessIt b) {
				return invoke(compare, invoke(transform, *a),
				              invoke(transform, *b));
			};
			if (end - begin < 8) {
				insertion_sort_copy(begin, end, d_begin, d_end, comp);
				return;
			} else {
				// TODO
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
	struct sort_transform_impl<RandomAccessIt, UnaryOperation, BinaryPredicate, SortKey,
	                           true, false, false> {
		static constexpr void inplace(RandomAccessIt begin, const RandomAccessIt
	end, UnaryOperation&& transform, BinaryPredicate&& compare) { auto comp = [&compare,
	&transform](RandomAccessIt a, RandomAccessIt b) { return invoke(compare,
	(*a).*transform, (*b).*transform);
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
			// TODO
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
			// TODO
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
			// TODO
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
			// TODO
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
		static constexpr void inplace(RandomAccessIt begin, const RandomAccessIt end, UnaryOperation&& transform,
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
	struct sort_transform_impl<RandomAccessIt, UnaryOperation, std::greater<LessT>,
	                           std::basic_string<CharT>, true, false, false> {
		static constexpr void inplace(RandomAccessIt begin, const RandomAccessIt end, UnaryOperation&& transform,
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
	                            decltype(invoke(transform, *begin))>::
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
	                            decltype(invoke(transform, *begin))>::
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
