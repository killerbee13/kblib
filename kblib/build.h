#ifndef KBLIB_BUILD_H
#define KBLIB_BUILD_H

#include "tdecl.h"

#include "algorithm.h"
#include "fakestd.h"
#include "iterators.h"
#include "traits.h"

#include <algorithm>
#include <iterator>
#include <numeric>
#include <tuple>

#if __cplusplus >= 201703L
#include <optional>
#endif

namespace kblib {

/**
 * @brief Constructs a container by applying a UnaryFunction to every element of
 *    an input range
 *
 * @param first Beginning of input range.
 * @param last End of input range.
 * @param f A UnaryFunction to apply to each element of the input range.
 * @tparam Container The type of container to return. Must be an AllocatorAware
 *    SequenceContainer.
 * @param allocator The allocator to use for the returned container.
 * @return A Container where each element is a transformed element of the input
 *    range.
 */
template <typename Container, typename InputIt, typename UnaryFunction>
KBLIB_NODISCARD Container build(InputIt first, InputIt last, UnaryFunction f,
                                typename Container::allocator_type allocator =
                                    typename Container::allocator_type{}) {
	Container out(allocator);
	std::transform(first, last, std::back_inserter(out), f);
	return static_cast<void>(out.resize(out.size())), out;
}
/**
 * @brief Constructs a container by applying a BinaryFunction to every pair of
 *    elements in the input ranges
 * @pre The size of the second input range must be equal to or less than the
 *    size of the first.
 *
 * @param first Beginning of input range.
 * @param last End of input range.
 * @param first2 Beginning of second input range
 * @param f A BinaryFunction to apply to the elements of the two input ranges.
 * @tparam Container The type of container to return. Must be an AllocatorAware
 *    SequenceContainer.
 * @param allocator The allocator to use for the returned container.
 * @return A Container where each element is generated from a corresponding pair
 *    of elements in the input ranges.
 */
template <typename Container, typename InputIt, typename InputIt2,
          typename BinaryFunction>
KBLIB_NODISCARD Container build(InputIt first, InputIt last, InputIt2 first2,
                                BinaryFunction f,
                                typename Container::allocator_type allocator =
                                    typename Container::allocator_type{}) {
	Container out(allocator);
	std::transform(first, last, first2, std::back_inserter(out), f);
	return out;
}

/**
 * @brief Constructs an array-like container by applying a UnaryFunction to
 *    every element of an input range
 * @pre The size of the input range must be equal to or less than the size of
 *    Container.
 * @remark Because Array is non-resizable, the entire Array will be
 *    default-constructed and then the elements assigned to, rather than
 *    copy-constructed.
 *
 * @param first Beginning of input range.
 * @param last End of input range.
 * @param f A UnaryFunction to apply to each element of the input range.
 * @tparam Array The type of container to return. Must be a non-resizable
 *    Container similar to std::array.
 * @return An Array where each element is a transformed element of the input
 *    range.
 */
template <typename Array, typename InputIt, typename UnaryFunction,
          enable_if_t<!detail::is_resizable_v<Array>, int> = 0>
KBLIB_NODISCARD Array build(InputIt first, InputIt last, UnaryFunction f) {
	Array out;
	std::transform(first, last, out.begin(), f);
	return out;
}
/**
 * @brief Constructs an array-like container by applying a BinaryFunction to
 * every pair of elements in the input ranges
 * @pre The size of the second input range must be equal to or less than the
 *    size of the first.
 * @remark Because Array is non-resizable, the entire Array will be
 *    default-constructed and then the elements assigned to, rather than
 *    copy-constructed.
 *
 * @param first Beginning of input range.
 * @param last End of input range.
 * @param first2 Beginning of second input range
 * @param f A BinaryFunction to apply to the elements of the two input ranges.
 * @tparam Array The type of container to return. Must be a non-resizable
 *    Container similar to std::array.
 * @return An Array where each element is generated from a corresponding pair
 *    of elements in the input ranges.
 */
template <typename Array, typename InputIt, typename InputIt2,
          typename BinaryFunction,
          enable_if_t<!detail::is_resizable_v<Array>, int> = 0>
KBLIB_NODISCARD Array build(InputIt first, InputIt last, InputIt2 first2,
                            BinaryFunction f) {
	Array out;
	std::transform(first, last, first2, out.begin(), f);
	return out;
}

/**
 * @brief Constructs a container with elements initialized by repeatedly calling
 *    a generating function
 *
 * @tparam Container The type of container to return. Must be an AllocatorAware
 *    SequenceContainer.
 * @param f The functor to repeatedly invoke.
 * @param size The number of times to invoke `f`.
 * @param allocator The allocator to use for the returned container.
 * @return A Container where each element is the result of invoking `f` in
 *    sequence.
 */
template <typename Container, typename Functor>
KBLIB_NODISCARD Container build(Functor f, size_t size,
                                typename Container::allocator_type allocator =
                                    typename Container::allocator_type{}) {
	Container out(allocator);
	try_reserve(out, size);
	std::generate_n(std::back_inserter(out), size, f);
	return out;
}

/**
 * @brief Constructs an array-like container with elements initialized by
 *    repeatedly calling a generating function
 * @remark Because Array is non-resizable, the entire Array will be
 *    default-constructed and then the elements assigned to, rather than
 *    copy-constructed.
 *
 * @tparam Array The type of container to return. Must be a non-resizable
 *    Container similar to std::array.
 * @param f The functor to repeatedly invoke.
 * @param size The number of times to invoke `f`. Defaults to the size of the
 *    Container, which is usually correct.
 * @return An Array where each element is the result of invoking `f` in
 *    sequence.
 */
template <typename Array, typename Functor,
          enable_if_t<!detail::is_resizable_v<Array>, int> = 0>
KBLIB_NODISCARD Array build(Functor f,
                            size_t size = std::tuple_size<Array>::value) {
	Array out;
	std::generate_n(out.begin(), size, f);
	return out;
}

// build_dy: workaround for non-allocator-aware dynamic containers
/**
 * @brief Constructs a container by applying a UnaryFunction to every element of
 *    an input range. Exactly like `build`, but for resizable non-AllocatorAware
 *    Containers (which are hard to detect automatically).
 *
 * @param first Beginning of input range.
 * @param last End of input range.
 * @param f A UnaryFunction to apply to each element of the input range.
 * @tparam Container The type of container to return. Must be a resizable
 *    SequenceContainer.
 * @return A Container where each element is a transformed element of the input
 *    range.
 */
template <typename Container, typename InputIt, typename UnaryFunction>
KBLIB_NODISCARD Container build_dy(InputIt first, InputIt last,
                                   UnaryFunction f) {
	Container out;
	std::transform(first, last, std::back_inserter(out), f);
	return out;
}
/**
 * @brief Constructs a container by applying a BinaryFunction to every pair of
 *    elements in the input ranges. Exactly like `build`, but for resizable
 *    non-AllocatorAware Containers (which are hard to detect automatically).
 * @pre The size of the second input range must be equal to or less than the
 *    size of the first.
 *
 * @param first Beginning of input range.
 * @param last End of input range.
 * @param first2 Beginning of second input range
 * @param f A BinaryFunction to apply to the elements of the two input ranges.
 * @tparam Container The type of container to return. Must be a resizable
 *    SequenceContainer.
 * @return A Container where each element is generated from a corresponding pair
 *    of elements in the input ranges.
 */
template <typename Container, typename InputIt, typename InputIt2,
          typename BinaryFunction>
KBLIB_NODISCARD Container build_dy(InputIt first, InputIt last, InputIt2 first2,
                                   BinaryFunction f) {
	Container out;
	std::transform(first, last, first2, std::back_inserter(out), f);
	return out;
}

/**
 * @brief Constructs a container with elements initialized by repeatedly calling
 *    a generating function. Exactly like `build`, but for resizable
 *    non-AllocatorAware Containers (which are hard to detect automatically).
 *
 * @param f The functor to repeatedly invoke.
 * @param size The number of times to invoke `f`.
 * @return A Container where each element is the result of invoking `f` in
 *    sequence.
 */
template <typename Container, typename Functor>
KBLIB_NODISCARD Container build_dy(Functor f, size_t size) {
	Container out;
	try_reserve(out, size);
	std::generate_n(std::back_inserter(out), size, f);
	return out;
}

#if 0
//I can't overload on both array vs. dynamic container and execution policy
//in any sane way without concepts, so this whole set of functions is cut
// because they're less useful than the array overloads.
template<typename Container, typename ExecutionPolicy,
      typename InputIt, typename UnaryFunction>
Container build(ExecutionPolicy&& policy, InputIt first,
      InputIt last, UnaryFunction f,
      [[gnu::unused]] typename Container::allocator_type = typename Container::allocator_type{}) {
  Container out;
  std::transform(policy, first, last, std::back_inserter(out), f);
  return static_cast<void>(out.resize(out.size())), out;
}
template<typename Container, typename ExecutionPolicy,
      typename InputIt, typename InputIt2, typename BinaryFunction>
Container build(ExecutionPolicy&& policy, InputIt first,
      InputIt last, InputIt2 first2, BinaryFunction f,
      [[gnu::unused]] typename Container::allocator_type = typename Container::allocator_type{}) {
  Container out;
  std::transform(policy, first, last, first2,
     std::back_inserter(out), f);
  return out;
}
template<typename Array, typename ExecutionPolicy,
      typename InputIt, typename UnaryFunction,
      typename std::enable_if<std::is_convertible<typename std::tuple_size<Array>::value_type, size_t>::value, int>::type = 0>
Array build(ExecutionPolicy&& policy, InputIt first,
      InputIt last, UnaryFunction f) {
  Array out;
  std::transform(policy, first, last, out.begin(), f);
  return out;
}
template<typename Array, typename ExecutionPolicy,
      typename InputIt, typename InputIt2, typename BinaryFunction,
      typename std::enable_if<std::is_convertible<typename std::tuple_size<Array>::value_type, size_t>::value, int>::type = 0>
Array build(ExecutionPolicy&& policy, InputIt first,
      InputIt last, InputIt2 first2, BinaryFunction f) {
  Array out;
  std::transform(policy, first, last, first2, out.begin(), f);
  return out;
}
template<typename Container, typename ExecutionPolicy,
      typename Functor>
inline Container build(ExecutionPolicy&& policy, Functor f, size_t size,
      [[gnu::unused]] typename Container::allocator_type = typename Container::allocator_type{}) {
  Container out(size);
  std::generate_n(policy, out.begin(), size, f);
  return out;
}
template<typename Array, typename ExecutionPolicy,
      typename Functor,
      typename std::enable_if<std::is_convertible<typename std::tuple_size<Array>::value_type, size_t>::value, int>::type = 0>
inline Array build(ExecutionPolicy&& policy, Functor f, size_t size = std::tuple_size<Array>::value) {
  Array out;
  std::generate_n(policy, out.begin(), size, f);
  return out;
}
#endif

namespace detail {

   template <typename Container>
   struct buildiota_impl<Container, true> {
		template <typename T>
		constexpr static Container impl(std::size_t count, T value) {
			Container out;
			try_reserve(out, count);
			while (count-- > 0) {
				out.push_back(value);
				++value;
			}
			return out;
		}
		template <typename T, typename I>
		constexpr static Container impl(std::size_t count, T value, I incr) {
			Container out;
			try_reserve(out, count);
			while (count-- > 0) {
				out.push_back(value);
				value += incr;
			}
			return out;
		}
	};

	template <typename Array>
	struct buildiota_impl<Array, false> {
		template <typename T>
		constexpr static Array impl(T value) {
			Array out{};
			for (auto& v : out) {
				v = value;
				++value;
			}
			return out;
		}
		template <typename T, typename I>
		constexpr static Array impl(T value, I incr) {
			Array out{};
			for (auto& v : out) {
				v = value;
				value += incr;
			}
			return out;
		}
	};

} // namespace detail

/**
 * @brief Builds a container of increasing values.
 *
 * @tparam Container Either a resizable (like std::vector) or non-resizable
 * (like std::array) Container.
 * @remark If Container is non-resizable, then elements are first
 * value-initialized and then assigned to, otherwise values are inserted
 * directly.
 * @param args If Container is resizable, the first argument is the size of
 * container to return, otherwise there is no size argument. The next argument
 * is always the starting value. Optionally, an increment may be specified as a
 * final argument.
 */
template <typename Container, typename... Args>
KBLIB_NODISCARD constexpr auto buildiota(Args&&... args) {
	return detail::buildiota_impl<Container, detail::is_resizable_v<Container>>::
	    impl(std::forward<Args>(args)...);
}

/**
 * @brief
 *
 * @param first
 * @param last
 * @param allocator
 * @return Container
 */
template <typename Container, typename InputIt>
KBLIB_NODISCARD Container
build_copy(InputIt first, InputIt last,
           typename Container::allocator_type allocator =
               typename Container::allocator_type{}) {
	Container out(allocator);
	std::copy(first, last, std::back_inserter(out));
	return out;
}

/**
 * @brief
 *
 * @param r
 * @param allocator
 * @return Container
 */
template <typename Container, typename Range>
KBLIB_NODISCARD Container
build_copy(Range&& r, typename Container::allocator_type allocator =
                          typename Container::allocator_type{}) {
	Container out(allocator);
	std::copy(std::begin(r), std::end(r), std::back_inserter(out));
	return out;
}

/**
 * @brief
 *
 * @param first
 * @param last
 * @return Container
 */
template <typename Container, typename InputIt,
          enable_if_t<!detail::is_resizable_v<Container>, int> = 0>
KBLIB_NODISCARD constexpr Container build_copy(InputIt first, InputIt last) {
	Container out{};
	auto pos = std::begin(out);
	auto end = std::end(out);
	for (; first != last && pos != end; ++first, ++pos) {
		*pos = *first;
	}
	return out;
}

/**
 * @brief
 *
 * @param r
 * @return Container
 */
template <typename Container, typename Range,
          enable_if_t<!detail::is_resizable_v<Container>, int> = 0>
KBLIB_NODISCARD constexpr Container build_copy(Range&& r) {
	Container out{};
	auto first = std::begin(r);
	auto last = std::end(r);
	auto pos = std::begin(out);
	auto end = std::end(out);
	for (; first != last && pos != end; ++first, ++pos) {
		*pos = *first;
	}
	return out;
}

/**
 * @brief
 *
 * @param first
 * @param last
 * @param size
 * @return Container
 */
template <typename Container, typename InputIt,
          enable_if_t<!detail::is_resizable_v<Container>, int> = 0>
KBLIB_NODISCARD Container build_copy(InputIt first, InputIt last,
                                     std::size_t size) {
	Container out;
	auto pos = std::begin(out);
	auto end = std::end(out);
	for (std::size_t count = 0; count != size && first != last && pos != end;
	     ++first, ++pos, ++count) {
		*pos = *first;
	}
	return out;
}

/**
 * @brief
 *
 * @param r
 * @param size
 * @return Container
 */
template <typename Container, typename Range,
          enable_if_t<!detail::is_resizable_v<Container>, int> = 0>
KBLIB_NODISCARD Container build_copy(Range&& r, std::size_t size) {
	Container out;
	auto first = std::begin(r);
	auto last = std::end(r);
	auto pos = std::begin(out);
	auto end = std::end(out);
	for (std::size_t count = 0; count != size && first != last && pos != end;
	     ++first, ++pos, ++count) {
		*pos = *first;
	}
	return out;
}

/**
 * @brief
 *
 * @param first
 * @param last
 * @param f
 * @param allocator
 * @return Container
 */
template <typename Container, typename InputIt, typename Predicate>
KBLIB_NODISCARD Container
build_copy_if(InputIt first, InputIt last, Predicate f,
              typename Container::allocator_type allocator =
                  typename Container::allocator_type{}) {
	Container out(allocator);
	kblib::copy_if(first, last, std::back_inserter(out), f);
	return out;
}

/**
 * @brief
 *
 * @param first
 * @param count
 * @param allocator
 * @return Container
 */
template <typename Container, typename InputIt, typename Size>
KBLIB_NODISCARD Container
build_copy_n(InputIt first, Size count,
             typename Container::allocator_type allocator =
                 typename Container::allocator_type{}) {
	Container out(allocator);
	std::copy_n(first, count, std::back_inserter(out));
	return out;
}

/**
 * @brief
 *
 * @param first
 * @param count
 * @param f
 * @param allocator
 * @return Container
 */
template <typename Container, typename InputIt, typename Size,
          typename Predicate>
KBLIB_NODISCARD Container
build_copy_n_if(InputIt first, Size count, Predicate f,
                typename Container::allocator_type allocator =
                    typename Container::allocator_type{}) {
	Container out(allocator);
	kblib::copy_n_if(first, count, std::back_inserter(out), f);
	return out;
}

// transform_accumulate
// transform_partial_sum

} // namespace kblib

#endif // KBLIB_BUILD_H
