#ifndef KBLIB_BUILD_H
#define KBLIB_BUILD_H

#include "kblib_fakestd.h"
#include "kblib_tdecl.h"

#include <algorithm>
#include <iterator>
#include <numeric>
#include <tuple>

#if __cplusplus >= 201703L
#include <optional>
#endif

namespace kblib {

namespace detail {

template <typename C, typename = decltype(std::declval<C&>().resize(0))>
constexpr bool calc_resizable() {
  return true;
}

// Note that when a type that is not resizable, but also doesn't have a
// constexpr size, is passed, there is a hard error.
template <typename C, int = std::tuple_size<C>::value>
constexpr bool calc_resizable() {
  return false;
}

constexpr bool calc_resizable(...) {
  return false;
}

template <typename C>
struct is_resizable {
  constexpr static bool value = calc_resizable<C>();
};
template <typename C>
constexpr bool is_resizable_v = is_resizable<C>::value;

template <typename C, typename = fakestd::void_t<>>
struct has_reserve {
  constexpr static bool value = false;
};

template <typename C>
struct has_reserve<C, fakestd::void_t<decltype(std::declval<C&>.reserve())>> {
  constexpr static bool value = true;
};

template <typename C>
constexpr bool has_reserve_v = has_reserve<C>::value;

}  // namespace detail

template <typename C,
          typename std::enable_if<detail::has_reserve_v<C>, int>::type = 0>
void try_reserve(C& c, std::size_t s) {
  c.reserve(s);
}

template <typename C,
          typename std::enable_if<!detail::has_reserve_v<C>, int>::type = 0>
void try_reserve(C&, std::size_t) {}

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
template <
    typename Array, typename InputIt, typename UnaryFunction,
    typename std::enable_if<!detail::is_resizable_v<Array>, int>::type = 0>
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
template <
    typename Array, typename InputIt, typename InputIt2,
    typename BinaryFunction,
    typename std::enable_if<!detail::is_resizable_v<Array>, int>::type = 0>
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
template <
    typename Array, typename Functor,
    typename std::enable_if<!detail::is_resizable_v<Array>, int>::type = 0>
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
 * @param allocator The allocator to use for the returned container.
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
      [[maybe_unused]] typename Container::allocator_type = typename Container::allocator_type{}) {
  Container out;
  std::transform(policy, first, last, std::back_inserter(out), f);
  return static_cast<void>(out.resize(out.size())), out;
}
template<typename Container, typename ExecutionPolicy,
      typename InputIt, typename InputIt2, typename BinaryFunction>
Container build(ExecutionPolicy&& policy, InputIt first,
      InputIt last, InputIt2 first2, BinaryFunction f,
      [[maybe_unused]] typename Container::allocator_type = typename Container::allocator_type{}) {
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
      [[maybe_unused]] typename Container::allocator_type = typename Container::allocator_type{}) {
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

}  // namespace detail

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
 * @brief Abbreviation of the erase-remove idiom as a free function.
 *
 * @param c The container to erase from.
 * @param val The value to remove.
 */
template <typename C, typename T>
void erase(C& c, const T& val) {
  c.erase(std::remove(c.begin(), c.end(), val), c.end());
}

template <typename C, typename UnaryPredicate>
void erase_if(C& c, UnaryPredicate p) {
  c.erase(std::remove_if(c.begin(), c.end(), p), c.end());
}

// find_last (like for string)
// Searches a range for the last occurence of a match, and returns an iterator
// to it.
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

// find_in:
// 1. Finds v in range [begin, end) and returns the offset from begin

template <typename It, typename T>
KBLIB_NODISCARD size_t find_in(It begin, It end, const T& v) {
  return std::find(begin, end, v) - begin;
}

// find_in_if, find_in_if_not:
// 1. Finds first value in range [begin, end) for which p returns true and
// returns the offset from begin
// 2. Finds first value in range [begin, end) for which p returns false and
// returns the offset from begin

template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_in_if(It begin, It end, UnaryPredicate p) {
  return std::find_if(begin, end, p) - begin;
}
template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_in_if_not(It begin, It end, UnaryPredicate p) {
  return std::find_if_not(begin, end, p) - begin;
}

// find_last_in:
// 1. Finds last v in range [begin, end) and returns the offset from begin

template <typename It, typename T>
KBLIB_NODISCARD size_t find_last_in(It begin, It end, const T& v) {
  return kblib::find_last(begin, end, v) - begin;
}

// find_last_in_if, find_last_in_if_not:
// 1. Finds last value in range [begin, end) for which p returns true and
// returns the offset from begin
// 2. Finds last value in range [begin, end) for which p returns false and
// returns the offset from begin

template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if(It begin, It end, UnaryPredicate p) {
  return kblib::find_last_if(begin, end, p) - begin;
}
template <typename It, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if_not(It begin, It end, UnaryPredicate p) {
  return kblib::find_last_if_not(begin, end, p) - begin;
}

// find_in:
// 1. Finds v in c and returns the offset from the beginning of c

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

// find_in_if, find_in_if_not:
// 1. Finds first value in c for which p returns true and returns the offset
// from the beginning of c
// 2. Finds first value in c for which p returns false and returns the offset
// from the beginning of c

template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_in_if(const Container& c, UnaryPredicate p) {
  return std::find_if(std::begin(c), std::end(c), p) - std::begin(c);
}
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

// find_last_in:
// 1. Finds v in c and returns the offset from the beginning of c

template <typename Container, typename T>
KBLIB_NODISCARD size_t find_last_in(const Container& c, const T& v) {
  return kblib::find_last(std::begin(c), std::end(c), v) - std::begin(c);
}

// find_in_if, find_in_if_not:
// 1. Finds first value in c for which p returns true and returns the offset
// from the beginning of c
// 2. Finds first value in c for which p returns false and returns the offset
// from the beginning of c

template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if(const Container& c, UnaryPredicate p) {
  return kblib::find_last_if(std::begin(c), std::end(c), p) - std::begin(c);
}
template <typename Container, typename UnaryPredicate>
KBLIB_NODISCARD size_t find_last_in_if_not(const Container& c,
                                           UnaryPredicate p) {
  return kblib::find_last_if_not(std::begin(c), std::end(c), p) - std::begin(c);
}

template <typename Container, typename Comp = std::less<>, typename It>
KBLIB_NODISCARD Container get_max_n(It begin, It end, int count,
                                    Comp cmp = {}) {
  assert(begin + count <= end);
  return std::accumulate(begin + count, end, Container{begin, begin + count},
                         [&](Container c, const auto& v) {
                           auto& min =
                               *std::min_element(c.begin(), c.end(), cmp);
                           if (cmp(min, v)) {
                             min = v;
                           }
                           return c;
                         });
}

template <typename It, typename It2, typename BinaryFunction>
KBLIB_NODISCARD constexpr BinaryFunction for_each(It first, It last, It2 second,
                                                  BinaryFunction f) {
  for (; first != last; (void)++first, (void)++second) {
    f(*first, *second);
  }
  return std::move(f);
}

template <typename It, typename It2, typename Size, typename BinaryFunction>
KBLIB_NODISCARD constexpr It for_each_n(It first, Size n, It2 second,
                                        BinaryFunction f) {
  for (Size i = 0; i < n; (void)++first, (void)++second, (void)++i) {
    f(*first, *second);
  }
  return first;
}

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

template <typename Container, typename InputIt>
KBLIB_NODISCARD Container
build_copy(InputIt first, InputIt last,
           typename Container::allocator_type allocator =
               typename Container::allocator_type{}) {
  Container out(allocator);
  std::copy(first, last, std::back_inserter(out));
  return out;
}

template <typename Container, typename Range>
KBLIB_NODISCARD Container
build_copy(Range&& r, typename Container::allocator_type allocator =
                          typename Container::allocator_type{}) {
  Container out(allocator);
  std::copy(std::begin(r), std::end(r), std::back_inserter(out));
  return out;
}

template <
    typename Container, typename InputIt,
    typename std::enable_if<!detail::is_resizable_v<Container>, int>::type = 0>
NODISCARD constexpr Container build_copy(InputIt first, InputIt last) {
  Container out{};
  auto pos = std::begin(out);
  auto end = std::end(out);
  for (; first != last && pos != end; ++first, ++pos) {
    *pos = *first;
  }
  return out;
}

template <
    typename Container, typename Range,
    typename std::enable_if<!detail::is_resizable_v<Container>, int>::type = 0>
NODISCARD constexpr Container build_copy(Range&& r) {
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

template <
    typename Container, typename InputIt,
    typename std::enable_if<!detail::is_resizable_v<Container>, int>::type = 0>
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

template <
    typename Container, typename Range,
    typename std::enable_if<!detail::is_resizable_v<Container>, int>::type = 0>
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

template <typename Container, typename InputIt, typename Predicate>
KBLIB_NODISCARD Container
build_copy_if(InputIt first, InputIt last, Predicate f,
              typename Container::allocator_type allocator =
                  typename Container::allocator_type{}) {
  Container out(allocator);
  std::copy_if(first, last, std::back_inserter(out), f);
  return out;
}

template <typename Container, typename InputIt, typename Size>
KBLIB_NODISCARD Container
build_copy_n(InputIt first, Size count,
             typename Container::allocator_type allocator =
                 typename Container::allocator_type{}) {
  Container out(allocator);
  std::copy_n(first, count, std::back_inserter(out));
  return out;
}

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

template <typename T, std::size_t N>
struct vec : std::array<T, N> {
//  template <typename Vec,
//            typename std::enable_if<detail::is_resizable_v<Vec>, int>::type = 0>
//  operator Vec() const {
//    return {this->begin(), this->end()};
//  }

  template <typename U>
  operator std::vector<U>() const {
    return {this->begin(), this->end()};
  }

  template <typename U>
  operator std::array<U, N>() const {
    return build_copy<std::array<U, N>>(*this);
  }
};

#if __cplusplus >= 201703L
template <typename... Ts>
vec(Ts...) -> vec<std::common_type_t<Ts...>, sizeof...(Ts)>;
#endif

// transform_accumulate
// transform_partial_sum

template <typename T>
struct containing_ptr {
  constexpr T& operator*() noexcept { return val; }
  constexpr const T& operator*() const noexcept { return val; }

  constexpr T* operator->() noexcept { return &val; }
  constexpr const T* operator->() const noexcept { return &val; }

  constexpr T* get() noexcept { return &val; }
  constexpr const T* get() const noexcept { return &val; }

  T val;
};

#if __cplusplus >= 201703L

template <typename base_iterator, typename operation>
class transform_iterator {
 private:
  base_iterator it;
  std::optional<operation> op;

 public:
  using difference_type = std::ptrdiff_t;
  using result_type = decltype(std::invoke(*op, *it));
  using const_result_type =
      decltype(std::invoke(const_cast<const operation&>(*op),
                           const_cast<const base_iterator&>(*it)));
  using value_type = result_type;
  using pointer = void;
  using reference = value_type;
  using iterator_category = std::input_iterator_tag;

  transform_iterator(base_iterator _it, operation _op) : it(_it), op(_op) {}

  // constructs a non-dereferenceable sentinel iterator
  transform_iterator(base_iterator end_it) : it(end_it), op(std::nullopt) {}

  decltype(auto) operator*() { return std::invoke(*op, *it); }
  decltype(auto) operator*() const { return std::invoke(*op, *it); }

  auto operator-> () {
    return containing_ptr<result_type>{{std::invoke(*op, *it)}};
  }
  auto operator-> () const {
    return containing_ptr<const_result_type>{{std::invoke(*op, *it)}};
  }

  decltype(auto) operator++() { return ++it; }

  auto operator++(int) -> decltype(it++) { return it++; }

  friend bool operator==(
      const transform_iterator<base_iterator, operation>& lhs,
      const transform_iterator<base_iterator, operation>& rhs) {
    return lhs.it == rhs.it;
  }

  friend bool operator!=(
      const transform_iterator<base_iterator, operation>& lhs,
      const transform_iterator<base_iterator, operation>& rhs) {
    return lhs.it != rhs.it;
  }
};

template <typename base_iterator, typename operation>
transform_iterator<base_iterator, operation> make_transform_iterator(
    base_iterator it, operation op) {
  return {it, op};
}
#endif

// from marttyfication on the C++ Help discord
template <typename Container, typename F>
class back_insert_iterator_F {
 public:
  explicit back_insert_iterator_F(Container& c, F f)
      : container(c), fun(std::move(f)) {}

  using value_type = void;
  using difference_type = void;
  using pointer = void;
  using reference = void;
  using iterator_category = std::output_iterator_tag;

  template <typename V>
  auto& operator=(V&& value) {
    container.push_back(std::invoke(fun, std::forward<V>(value)));
    return *this;
  }

  auto& operator*() { return *this; }
  auto& operator++() { return *this; }

 private:
  Container& container;
  F fun;
};

template <typename F>
class consume_iterator {
 public:
  using value_type = void;
  using difference_type = void;
  using pointer = void;
  using reference = void;
  using iterator_category = std::output_iterator_tag;

  explicit consume_iterator(F f) : fun(std::move(f)) {}

  template <typename V>
  auto& operator=(V&& value) {
    (void)std::invoke(fun, std::forward<V>(value));
    return *this;
  }

  auto& operator*() { return *this; }
  auto& operator++() { return *this; }

 private:
  F fun;
};

template <typename F>
consume_iterator<F> consumer(F f) {
  return consume_iterator<F>{std::move(f)};
}

}  // namespace kblib

#endif  // KBLIB_BUILD_H
