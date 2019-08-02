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
template <
    typename Container, typename InputIt,
    typename std::enable_if<!detail::is_resizable_v<Container>, int>::type = 0>
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
template <
    typename Container, typename Range,
    typename std::enable_if<!detail::is_resizable_v<Container>, int>::type = 0>
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

/**
 * @brief
 *
 * @param r
 * @param size
 * @return Container
 */
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
	std::copy_if(first, last, std::back_inserter(out), f);
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

/**
 * @brief A smart pointer to an object contained inside the smart pointer
 * object.
 *
 */
template <typename T>
struct containing_ptr {
	/**
	 * @brief Returns the contained object.
	 */
	constexpr T& operator*() noexcept { return val; }
	/**
	 * @brief Returns the contained object.
	 */
	constexpr const T& operator*() const noexcept { return val; }

	/**
	 * @brief Return the address of the contained object.
	 */
	constexpr T* operator->() noexcept { return &val; }
	/**
	 * @brief Return the address of the contained object.
	 */
	constexpr const T* operator->() const noexcept { return &val; }

	/**
	 * @brief Returns the address of the contained object.
	 */
	constexpr T* get() noexcept { return &val; }
	/**
	 * @brief Returns the address of the contained object.
	 */
	constexpr const T* get() const noexcept { return &val; }

	T val;
};

#if KBLIB_USE_CXX17

/**
 * @brief An InputIterator that applies a transformation to the elements of the
 * range.
 *
 * @attention This class template depends on features introduced in C++17.
 */
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

	/**
	 * @brief Constructs a transform_iterator which applies _op to the values
	 * obtained from *_it.
	 *
	 * @param _it An InputIterator to a range to be transformed.
	 * @param _op The operation to apply to each element.
	 */
	transform_iterator(base_iterator _it, operation _op) : it(_it), op(_op) {}

	/**
	 * @brief constructs a non-dereferenceable sentinel iterator
	 *
	 * @param end_it An iterator that marks the end of the input range.
	 */
	transform_iterator(base_iterator end_it) : it(end_it), op(std::nullopt) {}

	/**
	 * @brief Transforms the value obtained by dereferencing it.
	 *
	 * @return decltype(auto) The result of invoking op on *it.
	 */
	decltype(auto) operator*() { return std::invoke(*op, *it); }
	/**
	 * @brief Transforms the value obtained by dereferencing it.
	 *
	 * @return decltype(auto) The result of invoking op on *it.
	 */
	decltype(auto) operator*() const { return std::invoke(*op, *it); }

	/**
	 * @brief Returns a containing_ptr with the transformed value, because
	 * operator-> expects a pointer-like return type.
	 */
	auto operator-> () {
		return containing_ptr<result_type>{{std::invoke(*op, *it)}};
	}
	/**
	 * @brief Returns a containing_ptr with the transformed value, because
	 * operator-> expects a pointer-like return type.
	 */
	auto operator-> () const {
		return containing_ptr<const_result_type>{{std::invoke(*op, *it)}};
	}

	/**
	 * @brief Increments the underlying iterator and returns *this.
	 */
	transform_iterator& operator++() {
		++it;
		return *this;
	}

	/**
	 * @brief Increments the underlying iterator and returns a copy of the
	 * current value.
	 */
	[[deprecated(
	    "Needlessly copies op. Use preincrement instead.")]] transform_iterator
	operator++(int) {
		return {it++, op};
	}

	/**
	 * @brief Compares the base iterators of lhs and rhs.
	 */
	friend bool
	operator==(const transform_iterator<base_iterator, operation>& lhs,
	           const transform_iterator<base_iterator, operation>& rhs) {
		return lhs.it == rhs.it;
	}

	/**
	 * @brief Compares the base iterators of lhs and rhs.
	 */
	friend bool
	operator!=(const transform_iterator<base_iterator, operation>& lhs,
	           const transform_iterator<base_iterator, operation>& rhs) {
		return lhs.it != rhs.it;
	}
};

/**
 * @brief Factory function to make transform_iterators.
 *
 * @param it An InputIterator to a range to transform.
 * @param op The transformation to apply.
 * @return transform_iterator<base_iterator, operation>
 */
template <typename base_iterator, typename operation>
transform_iterator<base_iterator, operation>
make_transform_iterator(base_iterator it, operation op) {
	return {it, op};
}
#endif

/**
 * @brief An OutputIterator that transforms the values assigned to it before
 * inserting them into the back of a container.
 *
 * @author From marttyfication#4235 on the C++ Help discord.
 */
template <typename Container, typename F>
class back_insert_iterator_F {
 public:
	/**
	 * @brief
	 *
	 * @param c The container to be inserted into.
	 * @param f The tranformation to apply to each argument.
	 */
	explicit back_insert_iterator_F(Container& c, F f)
	    : container(c), fun(std::move(f)) {}

	using value_type = void;
	using difference_type = void;
	using pointer = void;
	using reference = void;
	using iterator_category = std::output_iterator_tag;

	template <typename V>
	/**
	 * @brief Calls container.push_back(std::invoke(fun,
	 * std::forward<V>(value)));
	 *
	 * @param value The value to transform and insert.
	 * @return back_insert_iterator& *this.
	 */
	back_insert_iterator_F& operator=(V&& value) {
		container.push_back(fakestd::invoke(fun, std::forward<V>(value)));
		return *this;
	}

	/**
	 * @brief A no-op.
	 */
	back_insert_iterator_F& operator*() { return *this; }
	/**
	 * @brief A no-op.
	 */
	back_insert_iterator_F& operator++() { return *this; }

 private:
	Container& container;
	F fun;
};

/**
 * @brief An OutputIterator that simply calls a provided functor for each value
 * assigned to it.
 */
template <typename F>
class consume_iterator {
 public:
	using value_type = void;
	using difference_type = void;
	using pointer = void;
	using reference = void;
	using iterator_category = std::output_iterator_tag;

	/**
	 * @brief Constructs a consume_iterator with the given function object.
	 *
	 * @param f The functor to pass values to.
	 */
	explicit consume_iterator(F f) : fun(std::move(f)) {}

	/**
	 * @brief Pass value to F.
	 *
	 * @param value The argument for the functor.
	 * @return consume_iterator& *this.
	 */
	template <typename V>
	consume_iterator& operator=(V&& value) noexcept(
	    noexcept(fakestd::invoke(fun, std::forward<V>(value)))) {
		fakestd::invoke(fun, std::forward<V>(value));
		return *this;
	}

	/**
	 * @brief A no-op.
	 */
	consume_iterator& operator*() { return *this; }
	/**
	 * @brief A no-op.
	 */
	consume_iterator& operator++() { return *this; }

 private:
	F fun;
};

/**
 * @brief Creates a consume_iterator of deduced type F.
 *
 * This could be a deduction guide, if kblib didn't also support C++14. Thus,
 * the old style is used for compatibility.
 *
 * @param f A functor to call on assignment.
 * @return consume_iterator<F>
 */
template <typename F>
consume_iterator<F> consumer(F f) {
	return consume_iterator<F>{std::move(f)};
}

} // namespace kblib

#endif // KBLIB_BUILD_H
