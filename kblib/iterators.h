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
 * @brief This file provides some iterators, ranges, iterator/range adapters,
 * and operations that can be performed on iterators or smart pointers.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_ITERATORS_H
#define KBLIB_ITERATORS_H

#include "enumerate-contrib-cry.h"
#include "enumerate-contrib-tw.h"
#include "fakestd.h"

#include <cassert>
#include <iterator>
#include <vector>

#if KBLIB_USE_CXX17
#	include <optional>
#endif

namespace kblib {

template <typename ptr>
struct to_pointer_impl {
	constexpr auto operator()(ptr&& p) const noexcept -> auto {
		return to_pointer_impl<decltype(p.operator->())>{}(p.operator->());
	}
	constexpr auto operator()(const ptr& p) const noexcept -> auto {
		return to_pointer_impl<decltype(p.operator->())>{}(p.operator->());
	}
};

template <typename T>
struct to_pointer_impl<T*> {
	constexpr auto operator()(T* p) const noexcept -> T* { return p; }
};

/**
 * @brief Gets a raw pointer out of any smart pointer or iterator you might pass
 * in, without dereferencing it or relying on a get() method.
 *
 * @param p A smart pointer to extract from.
 */
template <typename P>
constexpr auto to_pointer(P&& p) noexcept -> auto* {
	return to_pointer_impl<std::decay_t<P>>{}(p);
}

template <typename Container,
          typename Comp = std::less<value_type_linear_t<Container>>>
auto max_element(Container& c, Comp comp) -> value_type_linear_t<Container>* {
	auto it = max_element(std::begin(c), std::end(c), comp);
	if (it != std::end(c)) {
		return to_pointer(it);
	} else {
		return nullptr;
	}
}
/**
 * @brief Determine if T is a valid output iterator to which values of type E
 * may be written.
 */
template <typename T, typename E, typename = void>
struct is_output_iterator_for : std::false_type {};

template <typename T, typename E>
struct is_output_iterator_for<
    T, E, void_t<decltype(*std::declval<T&>()++ = std::declval<const E&>())>>
    : std::true_type {};

template <typename Container>
/**
 * @brief Like a std::back_insert_iterator, but it keeps track of how many
 * insertions it has made, allowing an end iterator to be created.
 *
 * @attention This iterator must be incremented and dereferenced exactly once
 * for each assignment, in order to maintain the accuracy of the counter.
 *
 */
class counting_back_insert_iterator {
 public:
	using value_type = void;
	using difference_type = std::ptrdiff_t;
	using pointer = void;
	using reference = void;
	using iterator_category = std::output_iterator_tag;

	counting_back_insert_iterator() noexcept = default;
	explicit counting_back_insert_iterator(Container& c, std::size_t n = 0)
	    : container(std::addressof(c))
	    , count(n) {}
	explicit counting_back_insert_iterator(std::size_t n)
	    : count(n) {}

	struct proxy_iterator {
		using value_type = typename Container::value_type;

		auto operator=(const value_type& value) & -> proxy_iterator& {
			assert(container);
			// Multiple assignments for a single dereference are not allowed
			assert(*dirty);
			*dirty = false;
			container->push_back(value);
			return *this;
		}

		auto operator=(value_type&& value) & -> proxy_iterator& {
			assert(container);
			// Multiple assignments for a single dereference are not allowed
			assert(*dirty);
			*dirty = false;
			container->push_back(std::move(value));
			return *this;
		}

		Container* container;
		bool* dirty;
	};

	auto operator*() noexcept -> proxy_iterator {
		assert(dirty);
		return {container, &dirty};
	}

	auto operator++() & noexcept -> counting_back_insert_iterator& {
		assert(not dirty);
		++count;
		dirty = true;
		return *this;
	}
	auto operator++(int) noexcept -> counting_back_insert_iterator = delete;

	friend auto operator==(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) noexcept
	    -> bool {
		return a.count == b.count;
	}
	friend auto operator!=(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) noexcept
	    -> bool {
		return a.count != b.count;
	}
	friend auto operator<(const counting_back_insert_iterator& a,
	                      const counting_back_insert_iterator& b) noexcept
	    -> bool {
		return a.count < b.count;
	}
	friend auto operator<=(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) noexcept
	    -> bool {
		return a.count <= b.count;
	}
	friend auto operator>(const counting_back_insert_iterator& a,
	                      const counting_back_insert_iterator& b) noexcept
	    -> bool {
		return a.count > b.count;
	}
	friend auto operator>=(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) noexcept
	    -> bool {
		return a.count >= b.count;
	}
	friend auto operator-(const counting_back_insert_iterator& a,
	                      const counting_back_insert_iterator& b) noexcept
	    -> std::ptrdiff_t {
		return std::ptrdiff_t(a.count) - ptrdiff_t(b.count);
	}

 protected:
	Container* container = nullptr;
	std::size_t count = 0;
	bool dirty = true;
};

template <typename C>
KBLIB_NODISCARD counting_back_insert_iterator<C> counting_back_inserter(
    C& c, std::size_t count = 0) {
	return counting_back_insert_iterator<C>{c, count};
}

/**
 * @brief A range generator, similar to Python 3's range().
 *
 * Generates a half-open range, [min, max).
 * @tparam Value The type of elements in the range. Must support comparison
 * (equality and relational) and subtraction, but does not strictly need to be
 * numeric. Must be copyable. Must be value-initializable. Notably,
 * RandomAccessIterators are valid Value types.
 * @tparam Delta A type used to mutate Value values.
 * std::declval<Value>() + std::declval<Delta>() must be a valid expression
 * and must return type Value. Delta must be implicitly constructible and
 * assignable from, as well as comparable to, int.
 */
template <typename Value, typename Delta>
class range_t {
 private:
	Value min, max;
	Delta step;

	constexpr static bool nothrow_copyable
	    = std::is_nothrow_copy_constructible<Value>::value;
	constexpr static bool nothrow_steppable = noexcept(min + step);

 public:
	/**
	 * @brief 2- and 3-argument constructor. Explicitly specify start, end, and
	 * optionally the step amount.
	 *
	 * @param min_ The first value in the range.
	 * @param max_ The end of the range.
	 * @param step_ The difference between values in the range.
	 */
	constexpr range_t(Value min_, Value max_, Delta step_ = 1)
	    : min(min_)
	    , max(max_)
	    , step(step_) {
		normalize();
	}
	/**
	 * @brief 1-argument constructor. Start is implicitly zero and step is 1 or
	 * -1, depending on the sign of max.
	 *
	 * @param max The end of the range.
	 */
	constexpr range_t(Value max_)
	    : range_t(Value{}, max_, (max_ >= Value{}) ? 1 : -1) {}

	/**
	 * @brief A helper struct which acts as an iterator for the range elements,
	 * as they are generated on the fly.
	 */
	struct iterator {
		Value val;
		Delta step;

		using difference_type = std::ptrdiff_t;
		using value_type = Value;
		using pointer = const Value*;
		using reference = Value;
		using iterator_category = std::input_iterator_tag;

		/**
		 * @brief Return the "pointed-to" value.
		 *
		 * @return Value The value in the range this iterator corresponds to.
		 */
		constexpr auto operator*() const noexcept(nothrow_copyable) -> Value {
			return val;
		}
		/**
		 * @brief Return a pointer to the value.
		 *
		 * @return pointer A pointer to a value equivalent to *(*this). Valid
		 * until the iterator is modified in any way or destroyed.
		 */
		constexpr auto operator->() const noexcept -> pointer { return &val; }
		/**
		 * @brief Prefix increment. Advance to the next value in the range.
		 *
		 * @return iterator& *this.
		 */
		constexpr auto operator++() & noexcept(nothrow_steppable) -> iterator& {
			val = static_cast<Value>(val + step);
			return *this;
		}
		/**
		 * @brief Postfix increment. Advance to the next value in the range, but
		 * return the current value.
		 *
		 * @return iterator A copy of the pre-incrementing value of *this.
		 */
		constexpr auto operator++(int) noexcept(nothrow_steppable) -> iterator {
			auto ret = *this;
			val = val + step;
			return ret;
		}
		/**
		 * @brief Compare two range iterators for equality.
		 *
		 * Range iterators compare equal if they point to the same value and have
		 * the same step.
		 */
		constexpr friend auto operator==(iterator l, iterator r) noexcept
		    -> bool {
			return l.val == r.val and l.step == r.step;
		}
		/**
		 * @brief Compare two range iterators for inequality.
		 *
		 * Range iterators compare equal if they point to the same value and have
		 * the same step.
		 */
		constexpr friend auto operator!=(iterator l, iterator r) noexcept
		    -> bool {
			return l.val != r.val or l.step != r.step;
		}
		/**
		 * @brief Compare two range iterators.
		 *
		 * For range iterators, (A < B) is true when A can be advanced until (*A -
		 * *B) changes sign.
		 */
		constexpr friend auto operator<(iterator l, iterator r) noexcept -> bool {
			if (l.step > 0)
				return l.val < r.val;
			else
				return l.val > r.val;
		}
		/**
		 * @brief Compare two range iterators.
		 *
		 * For range iterators, (A < B) is true when A can be advanced until (*A -
		 * *B) changes sign.
		 */
		constexpr friend auto operator<=(iterator l, iterator r) noexcept
		    -> bool {
			return not (r < l);
		}
		/**
		 * @brief Compare two range iterators.
		 *
		 * For range iterators, (A < B) is true when A can be advanced until (*A -
		 * *B) changes sign.
		 */
		constexpr friend auto operator>(iterator l, iterator r) noexcept -> bool {
			return r < l;
		}
		/**
		 * @brief Compare two range iterators.
		 *
		 * For range iterators, (A < B) is true when A can be advanced until (*A -
		 * *B) changes sign.
		 */
		constexpr friend auto operator>=(iterator l, iterator r) noexcept
		    -> bool {
			return not (l < r);
		}
		constexpr auto operator[](std::ptrdiff_t x) const noexcept -> Value {
			return static_cast<Value>(val + x * step);
		}
		template <typename Integral>
		constexpr auto operator[](Integral x) const noexcept -> Value {
			return static_cast<Value>(val + std::ptrdiff_t(x) * step);
		}
	};

	/**
	 * @brief Returns an iterator to the beginning of the range.
	 */
	constexpr auto begin() const noexcept -> iterator { return {min, step}; }
	/**
	 * @brief Return an iterator to the end of the range.
	 */
	constexpr auto end() const noexcept -> iterator { return {max, step}; }

	/**
	 * @brief Returns the distance between start() and stop().
	 */
	constexpr auto size() const noexcept -> std::size_t {
		return static_cast<std::size_t>(std::abs(max - min) / step);
	}

	/**
	 * @brief Returns an iterator to the beginning of the range.
	 */
	friend constexpr auto begin(const range_t& r) noexcept -> iterator {
		return {r.min, r.step};
	}
	/**
	 * @brief Return an iterator to the end of the range.
	 */
	friend constexpr auto end(const range_t& r) noexcept -> iterator {
		return {r.max, r.step};
	}

	/**
	 * @brief Returns the distance between start() and stop().
	 */
	friend constexpr auto size(const range_t& r) noexcept -> std::size_t {
		return (r.max - r.min) / r.step;
	}

	/**
	 * @brief Query whether the range will generate any elements.
	 */
	constexpr auto empty() const noexcept -> bool { return size() == 0; }

	template <typename Integral>
	constexpr auto operator[](Integral x) const noexcept
	    -> decltype(begin()[x]) {
		return begin()[x];
	}

	constexpr auto lesser() const noexcept(nothrow_copyable) -> Value {
		return (step > 0) ? max : min;
	}

	constexpr auto greater() const noexcept(nothrow_copyable) -> Value {
		return (step > 0) ? min : max;
	}

	/**
	 \brief Returns a linear container whose elements are this range
	*/
	template <
	    typename Container,
	    enable_if_t<
	        is_linear_container_v<
	            Container> and std::is_constructible<Container, iterator, iterator>::value>* = nullptr>
	explicit operator Container() const
	    noexcept(noexcept(Container(std::declval<iterator>(),
	                                std::declval<iterator>()))) {
		return Container(begin(), end());
	}

	/**
	 \brief Returns a setlike container whose elements are this range
	*/
	template <
	    typename Container,
	    enable_if_t<
	        is_setlike_v<
	            Container> and std::is_constructible<Container, iterator, iterator>::value>* = nullptr>
	explicit operator Container() const
	    noexcept(noexcept(Container(std::declval<iterator>(),
	                                std::declval<iterator>()))) {
		return Container(begin(), end());
	}

	/**
	 * @brief Compare l and r for equality.
	 *
	 * Ranges are equal when they generate identical ranges.
	 */
	constexpr friend auto operator==(range_t l, range_t r) noexcept -> bool {
		return (l.empty() and r.empty())
		       or ((l.begin() == r.begin()) and (l.end() == r.end())
		           and (l.step == r.step));
	}
	/**
	 * @brief Compare l and r for inequality.
	 *
	 * Ranges are equal when they generate identical ranges.
	 */
	constexpr friend auto operator!=(range_t l, range_t r) noexcept -> bool {
		return not (l == r);
	}

 private:
	template <typename T>
	static constexpr enable_if_t<not std::is_signed<T>::value, std::true_type>
	positive(const T&) {
		return {};
	}
	template <typename T>
	static constexpr enable_if_t<std::is_signed<T>::value, bool> positive(T v) {
		return v >= 0;
	}
	template <typename R, typename T,
	          enable_if_t<std::is_integral<R>::value
	                      and std::is_integral<T>::value>* = nullptr>
	static constexpr auto signed_cast(T v) {
		return kblib::signed_cast<R>(v);
	}
	template <typename R, typename T,
	          enable_if_t<not (std::is_integral<R>::value
	                           and std::is_integral<T>::value)>* = nullptr>
	static constexpr auto signed_cast(T v) {
		return v;
	}

	constexpr auto normalize() noexcept(nothrow_steppable) -> void {
		if (min == max) {
		} else if (step == 0) {
			if (min != std::numeric_limits<Value>::max()) {
				max = min + 1;
			} else {
				max = min - 1;
			}
		} else {
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wsign-conversion"
#pragma GCC diagnostic ignored "-Wconversion"
#pragma GCC diagnostic ignored "-Wsign-compare"
#pragma GCC diagnostic ignored "-Wimplicit-int-conversion"
#pragma GCC diagnostic ignored "-Wshorten-64-to-32"
			auto difference = max - min;
			std::ptrdiff_t sign = (step > 0) ? 1 : -1;
			if ((sign * to_signed(difference)) <= (sign * step)) {
				step = sign;
				max = min + step;
			} else {
				auto remainder = difference % step;
				if (remainder != 0) {
					max = max - remainder;
					assert(not (positive(max)
					            and (signed_cast<Delta>(
					                     std::numeric_limits<Value>::max() - max)
					                 < step)));
					max = max + step;
				}
			}
#pragma GCC diagnostic pop
		}
	}
};

namespace detail_iterators {
	template <typename T, typename U, typename = void>
	struct is_addable : std::false_type {};

	template <typename T, typename U>
	struct is_addable<T, U,
	                  void_t<decltype(std::declval<T&>() + std::declval<U&>())>>
	    : std::true_type {};
} // namespace detail_iterators

struct adjuster {
	std::ptrdiff_t adj;
	constexpr adjuster(std::ptrdiff_t adj_) noexcept
	    : adj(adj_) {}
	constexpr operator std::ptrdiff_t() const noexcept { return adj; }
};

template <typename T>
constexpr auto operator+(T val, adjuster a) noexcept
    -> enable_if_t<not detail_iterators::is_addable<T, std::ptrdiff_t>::value,
                   decltype(std::advance(val, a.adj))> {
	return std::advance(val, a.adj);
}

/**
 * @brief A struct which increments anything it is added to. Suitable for use as
 * a Delta type for range_t.
 */
struct incrementer {
	constexpr incrementer() noexcept = default;
	constexpr incrementer(int) noexcept {}
	constexpr operator int() const noexcept { return 1; }
	friend constexpr auto operator*(std::ptrdiff_t x, incrementer) {
		return adjuster{x};
	}

	template <typename T>
	constexpr auto operator()(T& t) -> T& {
		return ++t;
	}
};

/**
 * @brief Increments val.
 */
template <typename T>
constexpr auto operator+(T val, incrementer) -> T {
	return ++val;
}

/**
 * @brief A struct which decrements anything it is added to. Suitable for use as
 * a Delta type for range_t.
 */
struct decrementer {
	constexpr decrementer() noexcept = default;
	constexpr decrementer(int) noexcept {}
	constexpr operator int() const noexcept { return -1; }
	friend constexpr auto operator*(std::ptrdiff_t x, decrementer) {
		return adjuster{-x};
	}
	template <typename T>
	constexpr auto operator()(T& t) -> T& {
		return --t;
	}
};

/**
 * @brief Decrements val.
 */
template <typename T>
constexpr auto operator+(T val, decrementer) -> T {
	return --val;
}

/**
 * @brief Constructs a range from beginning, end, and step amount. The range is
 * half-open, that is min is in the range but max is not. If unspecified, the
 * step is automatically either 1 or -1, depending on whether max > min.
 *
 * @param min The first value in the produced range.
 * @param max The first value not in the produced range.
 * @param step The difference between values in the produced range.
 * @return range_t<Value, Delta> An iterable range [min, max).
 */
template <typename Value, typename Delta = int>
constexpr auto range(Value min, Value max, Delta step = 0)
    -> range_t<Value, Delta> {
	if (step == 0) {
		if (min <= max) {
			return {min, max, 1};
		} else {
			return {min, max, -1};
		}
	} else {
		return {min, max, step};
	}
}

/**
 * @brief Constructs a half-open range [0, max). The step is automatically
 * determined based on the sign of max.
 *
 * @param max The first value not in the produced range.
 * @return range_t<Value, int> An iterable range [0, max).
 */
template <typename Value>
constexpr auto range(Value max) -> range_t<Value, incrementer> {
	return {max};
}

#if KBLIB_USE_CXX17

template <typename Value, typename Delta>
class irange_t {
 public:
	Value min, max;
	Delta step;

	constexpr static bool nothrow_copyable
	    = std::is_nothrow_copy_constructible<Value>::value;
	constexpr static bool nothrow_steppable = noexcept(min + step);
};

template <typename Value, typename Delta = int>
constexpr auto irange(Value, Value, Delta = 0) {}

template <typename T>
class enumerator_iterator;

/**
 * @internal
 */
namespace detail_enumerate {

	template <typename T1, typename T2>
	auto get_or(T1&& t1, T2&& t2) -> decltype(auto) {
		return t1 ? *t1 : *t2;
	}

	struct force_copy_tag {};
	// Get a pointer which is guaranteed to be invalid to use (but not UB to
	// merely store)
	template <typename T>
	auto get_magic_ptr() -> T* {
		static const char enumeration_magic_pointer = '\0';
		return reinterpret_cast<T*>(
		    const_cast<char*>(&enumeration_magic_pointer));
	}

} // namespace detail_enumerate

template <typename T>
class enumeration {
 public:
	enumeration() = default;

	enumeration(const enumeration& other)
	    : idx(other.idx)
	    , local([&] {
		    assert(other.source or other.local);
		    assert(other.source != detail_enumerate::get_magic_ptr<T>());
		    return other.copied();
	    }())
	    , source(nullptr) {}
	enumeration(volatile enumeration& other)
	    : enumeration(const_cast<const enumeration&>(other)) {}

	enumeration(enumeration&&) = delete;
	auto operator=(const enumeration&) = delete;
	auto operator=(enumeration&&) = delete;

	~enumeration() = default;

 private:
	enumeration(detail_enumerate::force_copy_tag, std::size_t i)
	    : idx(i) {}

 public:
	auto index() const noexcept -> std::size_t { return idx; }

	auto copied() & noexcept -> std::remove_const_t<T>& {
		assert(source != detail_enumerate::get_magic_ptr<T>());
		assert(local);
		return *local;
	}
	auto copied() const& noexcept -> const T& {
		assert(source != detail_enumerate::get_magic_ptr<T>());
		return detail_enumerate::get_or(local, source);
	}

	auto reffed() & noexcept -> T& {
		assert(source != detail_enumerate::get_magic_ptr<T>());
		return detail_enumerate::get_or(local, source);
	}
	auto reffed() const& noexcept -> const T& {
		assert(source != detail_enumerate::get_magic_ptr<T>());
		return detail_enumerate::get_or(local, source);
	}

 private:
	auto set(T* t) & -> void { source = t; }

	auto advance() & noexcept -> void {
		++idx;
		source = detail_enumerate::get_magic_ptr<T>();
		local = std::nullopt;
	}

	std::size_t idx = 0;
	mutable std::optional<std::remove_const_t<T>> local = std::nullopt;
	T* source = detail_enumerate::get_magic_ptr<T>();

	template <typename>
	friend class enumerator_iterator;
};
} // namespace kblib

namespace std {
#	if defined(__clang__)
// Fix from: https://github.com/nlohmann/json/issues/1401
#		pragma clang diagnostic push
#		pragma clang diagnostic ignored "-Wmismatched-tags"
#	endif

template <typename T>
class tuple_size<::kblib::enumeration<T>>
    : public std::integral_constant<std::size_t, 2> {};

template <typename T>
class tuple_element<0, ::kblib::enumeration<T>> {
 public:
	using type = std::size_t;
};
template <typename T>
class tuple_element<0, volatile ::kblib::enumeration<T>> {
 public:
	using type = std::size_t;
};
template <typename T>
class tuple_element<0, const volatile ::kblib::enumeration<T>> {
 public:
	using type = const std::size_t;
};

/**
 * @brief references keep the same qualifications as the original
 */
template <typename T>
class tuple_element<1, volatile ::kblib::enumeration<T>> {
 public:
	using type = T;
};
/**
 * @brief but you can also add const to a reference
 */
template <typename T>
class tuple_element<1, const volatile ::kblib::enumeration<T>> {
 public:
	using type = const T;
};
/**
 * @brief copies decay away the const
 */
template <typename T>
class tuple_element<1, ::kblib::enumeration<T>> {
 public:
	using type = std::remove_const_t<T>;
};
/**
 * @brief unless const is explicitly added
 */
template <typename T>
class tuple_element<1, const ::kblib::enumeration<T>> {
 public:
	using type = const T;
};

#	if defined(__clang__)
#		pragma clang diagnostic pop
#	endif

} // namespace std

namespace kblib {

// When a structured binding is created by value, this function is called as if
// by get<i>(std::move(e)), so it does not call the lvalue reference function.
// I have no idea why it's implicitly moved, but it works for my purposes.
template <std::size_t I, typename T>
auto get(enumeration<T>&& e) -> decltype(auto) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return e.index();
	} else {
		return e.copied();
	}
}
template <std::size_t I, typename T>
auto get(const enumeration<T>&& e) -> decltype(auto) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return e.index();
	} else {
		return e.copied();
	}
}
// When captured by reference, the volatile qualifier is added, which allows
// std::tuple_element to detect the copying-nonconst-from-const case.
template <std::size_t I, typename T>
auto get(volatile enumeration<T>& e) -> decltype(auto) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return const_cast<enumeration<T>&>(e).index();
	} else {
		return const_cast<enumeration<T>&>(e).reffed();
	}
}

template <std::size_t I, typename T>
auto get(const volatile enumeration<T>& e) -> decltype(auto) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return const_cast<const enumeration<T>&>(e).index();
	} else {
		return const_cast<const enumeration<T>&>(e).reffed();
	}
}

template <typename It>
class enumerator_iterator {
 public:
	using nested_value
	    = copy_const_t<decltype(*std::declval<It&>()),
	                   typename std::iterator_traits<It>::value_type>;

	using value_type = enumeration<nested_value>;
	using difference_type = std::ptrdiff_t;
	using pointer = const value_type*;
	using reference = const value_type&;
	using iterator_category = std::input_iterator_tag;

	enumerator_iterator() = default;
	enumerator_iterator(const enumerator_iterator& other)
	    : enumerator_iterator(detail_enumerate::force_copy_tag{},
	                          other.curr_.idx, other.it_) {}
	enumerator_iterator(It it)
	    : it_(it) {}

	enumerator_iterator(enumerator_iterator&&) = default;
	auto operator=(const enumerator_iterator&) -> enumerator_iterator& = default;
	auto operator=(enumerator_iterator&&) -> enumerator_iterator& = default;

	~enumerator_iterator() = default;

	auto operator*() & -> volatile value_type& {
		if (not captured) {
			curr_.set(to_pointer(it_));
			captured = true;
		}
		return curr_;
	}
	auto operator++() & -> enumerator_iterator& {
		curr_.advance();
		captured = false;
		++it_;
		return *this;
	}

	friend auto operator==(const enumerator_iterator& lhs,
	                       const enumerator_iterator& rhs) noexcept -> bool {
		return lhs.it_ == rhs.it_;
	}
	friend auto operator!=(const enumerator_iterator& lhs,
	                       const enumerator_iterator& rhs) noexcept -> bool {
		return lhs.it_ != rhs.it_;
	}

 private:
	enumerator_iterator(detail_enumerate::force_copy_tag t, std::size_t idx,
	                    It it)
	    : it_(it)
	    , curr_(t, idx) {}

	It it_;
	bool captured = false;
	value_type curr_;
};

template <typename Range, typename = void>
class enumerator_t;

template <typename Range>
class enumerator_t<Range, void> {
 public:
	detail::no_dangle_t<Range> r;

	using range_t = typename std::remove_reference_t<Range>;
	using nested_iterator = decltype(r.begin());
	using nested_end_iterator = decltype(r.end());
	using iterator = enumerator_iterator<nested_iterator>;
	using end_iterator = enumerator_iterator<nested_end_iterator>;

	using nested_const_iterator = typename range_t::const_iterator;
	using const_iterator = enumerator_iterator<nested_const_iterator>;

	auto begin() const& noexcept(noexcept(r.cbegin())) -> const_iterator {
		return r.cbegin();
	}
	auto begin() & noexcept(noexcept(r.begin())) -> iterator {
		return r.begin();
	}

	auto end() const& noexcept(noexcept(r.cend())) -> const_iterator {
		return r.cend();
	}
	auto end() & noexcept(noexcept(r.end())) -> end_iterator { return r.end(); }
};

template <typename It, typename EndIt>
class enumerator_t {
 public:
	using nested_iterator = It;
	using iterator = enumerator_iterator<nested_iterator>;
	using end_iterator = enumerator_iterator<EndIt>;

	auto begin() const& noexcept -> iterator { return {r_begin}; }

	auto end() const& noexcept -> end_iterator { return {r_end}; }

	It r_begin;
	EndIt r_end;
};

/**
 * @brief Allow access to indexes while using range-based for loops.
 *
 * The "magic" part is that
 * \code
 * for (auto&& [idx, val] : kblib::magic_enumerate(range)) {
 * \endcode
 * captures 'val' by reference, while
 * \code
 * for (auto [idx, val] : kblib::magic_enumerate(range)) {
 * \endcode
 * captures 'val' by value, so that the effect is similar to if you had written
 * \code
 * for (auto val : range) {
 * \endcode
 * and kept track of the index manually.
 *
 * @param begin,end The input range.
 */
template <typename It, typename EIt>
auto magic_enumerate(It begin, EIt end) -> enumerator_t<It, EIt> {
	return {begin, end};
}

/**
 * @brief Allow access to indexes while using range-based for loops. Safe to use
 * with rvalues.
 *
 * The "magic" part is that
 * \code
 * for (auto&& [idx, val] : kblib::magic_enumerate(range)) {
 * \endcode
 * captures 'val' by reference, while
 * \code
 * for (auto [idx, val] : kblib::magic_enumerate(range)) {
 * \endcode
 * captures 'val' by value, so that the effect is similar to if you had written
 * \code
 * for (auto val : range) {
 * \endcode
 * and kept track of the index manually.
 *
 * @param r A range to iterate over.
 */
template <typename Range>
auto magic_enumerate(Range&& r) -> auto {
	if constexpr (std::is_lvalue_reference_v<Range&&>) {
		using std::begin;
		using std::end;
		return magic_enumerate(begin(r), end(r));
	} else {
		return enumerator_t<Range&&>{std::forward<Range>(r)};
	}
}

#endif

/**
 * @brief Allow range-for iteration of an iterator pair.
 */
template <typename Iter1, typename Iter2>
struct indirect_range {
	Iter1 begin_;
	Iter2 end_;

	using value_type = decltype(*begin_);

	constexpr auto begin() const noexcept -> Iter1 { return begin_; }
	constexpr auto end() const noexcept -> Iter2 { return end_; }
	constexpr auto rbegin() const noexcept -> auto {
		return std::make_reverse_iterator(begin_);
	}
	constexpr auto rend() const noexcept -> auto {
		return std::make_reverse_iterator(end_);
	}

	constexpr friend auto begin(const indirect_range& r) noexcept -> Iter1 {
		return r.begin_;
	}
	constexpr friend auto end(const indirect_range& r) noexcept -> Iter2 {
		return r.end_;
	}
};

/**
 * @brief Create a range from an iterator pair. Primarily useful for range-for
 * loops.
 *
 * @param begin,end The range to wrap.
 */
template <typename Iter1, typename Iter2>
constexpr auto indirect(Iter1 begin, Iter2 end) noexcept(noexcept(
    indirect_range<Iter1, Iter2>{begin, end})) -> indirect_range<Iter1, Iter2> {
	return {begin, end};
}

#if KBLIB_USE_CXX17

template <typename Iter1, typename Iter2>
indirect_range(Iter1, Iter2) -> indirect_range<Iter1, Iter2>;

template <typename Iter1, typename Iter2>
auto cry_enumerate(Iter1 begin, Iter2 end) -> auto {
	return cry_enumerate(indirect_range{begin, end});
}

#endif

// Fixed number of ranges
template <typename Iter1, typename EndIter = Iter1, std::size_t count = 0>
class multi_range {
 public:
 private:
	struct range {
		Iter1 begin;
		EndIter end;
	};

	std::array<range, count> ranges;
};

// Dynamic number of ranges
template <typename Iter1, typename EndIter>
class multi_range<Iter1, EndIter, 0> {
 public:
 private:
	struct range {
		Iter1 begin;
		EndIter end;
	};

	std::vector<range> ranges;
};

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
	constexpr auto operator*() noexcept -> T& { return val; }
	/**
	 * @brief Returns the contained object.
	 */
	constexpr auto operator*() const noexcept -> const T& { return val; }

	/**
	 * @brief Return the address of the contained object.
	 */
	constexpr auto operator->() noexcept -> T* { return &val; }
	/**
	 * @brief Return the address of the contained object.
	 */
	constexpr auto operator->() const noexcept -> const T* { return &val; }

	/**
	 * @brief Returns the address of the contained object.
	 */
	constexpr auto get() noexcept -> T* { return &val; }
	/**
	 * @brief Returns the address of the contained object.
	 */
	constexpr auto get() const noexcept -> const T* { return &val; }

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
	operation op;

 public:
	using difference_type = std::ptrdiff_t;
	using result_type = decltype(kblib::invoke(op, *it));
	using const_result_type
	    = decltype(kblib::invoke(std::as_const(op), *std::as_const(it)));
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
	transform_iterator(base_iterator _it,
	                   operation _op) noexcept(noexcept(base_iterator{
	    _it}) and noexcept(std::is_nothrow_move_constructible<operation>::value))
	    : it(_it)
	    , op(std::move(_op)) {}

	/**
	 * @brief constructs a non-dereferenceable sentinel iterator
	 *
	 * @param end_it An iterator that marks the end of the input range.
	 */
	transform_iterator(base_iterator end_it) noexcept(noexcept(base_iterator{
	    end_it}))
	    : it(end_it)
	    , op() {}

	/**
	 * @brief Transforms the value obtained by dereferencing it.
	 *
	 * @return decltype(auto) The result of invoking op on *it.
	 */
	auto operator*() noexcept(noexcept(kblib::invoke(op, *it)))
	    -> decltype(auto) {
		return kblib::invoke(op, *it);
	}
	/**
	 * @brief Transforms the value obtained by dereferencing it.
	 *
	 * @return decltype(auto) The result of invoking op on *it.
	 */
	decltype(auto) operator*() const noexcept(noexcept(kblib::invoke(op, *it))) {
		return kblib::invoke(op, *it);
	}

	/**
	 * @brief Returns a containing_ptr with the transformed value, because
	 * operator-> expects a pointer-like return type.
	 */
	auto operator->() noexcept(noexcept(kblib::invoke(op, *it))) -> auto {
		return containing_ptr<result_type>{{kblib::invoke(op, *it)}};
	}
	/**
	 * @brief Returns a containing_ptr with the transformed value, because
	 * operator-> expects a pointer-like return type.
	 */
	auto operator->() const noexcept(noexcept(kblib::invoke(op, *it))) -> auto {
		return containing_ptr<const_result_type>{{kblib::invoke(op, *it)}};
	}

	/**
	 * @brief Increments the underlying iterator and returns *this.
	 */
	auto operator++() noexcept(noexcept(++it)) -> transform_iterator& {
		++it;
		return *this;
	}

	/**
	 * @brief Increments the underlying iterator and returns a copy of the
	 * current value.
	 */
	[[deprecated("Needlessly copies op. Use preincrement instead.")]] auto
	operator++(int) noexcept(noexcept(transform_iterator{it++, op}))
	    -> transform_iterator {
		return {it++, op};
	}

	auto base() const noexcept -> base_iterator { return it; }

	/**
	 * @brief Compares the base iterators of lhs and rhs.
	 */
	friend auto operator==(const transform_iterator& lhs,
	                       const transform_iterator& rhs) noexcept -> bool {
		return lhs.it == rhs.it;
	}

	/**
	 * @brief Compares the base iterators of lhs and rhs.
	 */
	friend auto operator!=(const transform_iterator& lhs,
	                       const transform_iterator& rhs) noexcept -> bool {
		return lhs.it != rhs.it;
	}

	template <typename OIt>
	friend auto operator==(const transform_iterator& lhs,
	                       const OIt& rhs) noexcept -> bool {
		return lhs.base() == rhs;
	}
	template <typename OIt>
	friend auto operator==(const OIt& lhs,
	                       const transform_iterator& rhs) noexcept -> bool {
		return lhs == rhs.base();
	}

	template <typename OIt>
	friend auto operator!=(const transform_iterator& lhs,
	                       const OIt& rhs) noexcept -> bool {
		return lhs.base() != rhs;
	}
	template <typename OIt>
	friend auto operator!=(const OIt& lhs,
	                       const transform_iterator& rhs) noexcept -> bool {
		return lhs != rhs.base();
	}
};

template <typename It, typename operation>
transform_iterator(It, operation) -> transform_iterator<It, operation>;

/**
 * @brief Factory function to make transform_iterators.
 *
 * @param it An InputIterator to a range to transform.
 * @param op The transformation to apply.
 * @return transform_iterator<base_iterator, operation>
 *
 * @deprecated Use transformer instead
 */
template <typename base_iterator, typename operation>
[[deprecated("use transformer instead")]] auto make_transform_iterator(
    base_iterator it,
    operation
        op) noexcept(noexcept(transform_iterator<base_iterator, operation>{
    it, std::move(op)})) -> transform_iterator<base_iterator, operation> {
	return {it, std::move(op)};
}

/**
 * @brief Factory function to make transform_iterators.
 *
 * @param it An InputIterator to a range to transform.
 * @param op The transformation to apply.
 * @return transform_iterator<base_iterator, operation>
 */
template <typename base_iterator, typename operation>
auto transformer(base_iterator it, operation op) noexcept(
    noexcept(transform_iterator<base_iterator, operation>{it, std::move(op)}))
    -> transform_iterator<base_iterator, operation> {
	return {it, std::move(op)};
}

template <typename It, typename EndIt, typename operation>
auto transform_range(It begin, EndIt end, operation op) noexcept(
    noexcept(indirect(transform_iterator{begin, op}, end))) -> auto {
	return indirect(transform_iterator{begin, op}, end);
}
#endif

template <typename InputIt1, typename EndIt, typename InputIt2>
struct zip_iterator {
	InputIt1 pos1{};
	EndIt end1{};
	InputIt2 pos2{};

	constexpr static bool is_nothrow_copyable
	    = std::is_nothrow_copy_constructible<InputIt1>::value
	      and std::is_nothrow_copy_constructible<EndIt>::value
	      and std::is_nothrow_copy_constructible<InputIt2>::value;

	auto operator++() noexcept(noexcept(++pos1) and noexcept(++pos2))
	    -> zip_iterator& {
		++pos1;
		++pos2;
		return *this;
	}
	auto operator++(int) noexcept(is_nothrow_copyable and noexcept(
	    ++pos1) and noexcept(++pos2)) -> const zip_iterator {
		auto tmp = *this;
		++pos1;
		++pos2;
		return tmp;
	}

	KBLIB_NODISCARD auto operator*() const noexcept -> auto {
		return std::forward_as_tuple(*pos1, *pos2);
	}

	KBLIB_NODISCARD auto begin() const noexcept(is_nothrow_copyable)
	    -> zip_iterator {
		return *this;
	}
	KBLIB_NODISCARD auto end() const
	    noexcept(std::is_nothrow_copy_constructible<EndIt>::value and
	                 std::is_nothrow_copy_constructible<InputIt2>::value)
	        -> zip_iterator<EndIt, EndIt, InputIt2> {
		return {end1, end1};
	}

	KBLIB_NODISCARD friend auto operator==(
	    const zip_iterator& z1,
	    const zip_iterator& z2) noexcept(noexcept(z1.pos1 == z2.pos1)) -> bool {
		return z1.pos1 == z2.pos1;
	}
	KBLIB_NODISCARD friend auto operator!=(
	    const zip_iterator& z1,
	    const zip_iterator& z2) noexcept(noexcept(z1.pos1 != z2.pos1)) -> bool {
		return z1.pos1 != z2.pos1;
	}
	KBLIB_NODISCARD friend auto operator==(
	    const zip_iterator& z1,
	    zip_iterator<EndIt, EndIt, InputIt2> end) noexcept(noexcept(z1.pos1
	                                                                == end.val))
	    -> bool {
		return z1.end1 == end.val;
	}
	KBLIB_NODISCARD friend auto operator!=(
	    const zip_iterator& z1,
	    zip_iterator<EndIt, EndIt, InputIt2> end) noexcept(noexcept(z1.pos1
	                                                                == end.val))
	    -> bool {
		return z1.end1 != end.val;
	}
};

template <typename It1, typename It2>
struct zip_iterator<It1, It1, It2> {
	It1 pos1{};
	It1 end1{};
	It2 pos2{};

	constexpr static bool is_nothrow_copyable
	    = std::is_nothrow_copy_constructible<It1>::value
	      and std::is_nothrow_copy_constructible<It2>::value;

	auto operator++() noexcept(noexcept(++pos1)) -> zip_iterator& {
		++pos1;
		++pos2;
		return *this;
	}
	auto operator++(int) noexcept(is_nothrow_copyable and noexcept(++pos1))
	    -> const zip_iterator {
		auto tmp = *this;
		++pos1;
		++pos2;
		return tmp;
	}

	KBLIB_NODISCARD auto operator*() -> auto {
		return std::forward_as_tuple(*pos1, *pos2);
	}

	KBLIB_NODISCARD auto begin() const noexcept(is_nothrow_copyable)
	    -> zip_iterator {
		return *this;
	}
	KBLIB_NODISCARD auto end() const
	    noexcept(std::is_nothrow_copy_constructible<It1>::value)
	        -> zip_iterator {
		return {end1, end1, pos2};
	}

	KBLIB_NODISCARD friend auto operator==(
	    const zip_iterator& z1,
	    const zip_iterator& z2) noexcept(noexcept(z1.pos1 == z2.pos1)) -> bool {
		return z1.pos1 == z2.pos1;
	}
	KBLIB_NODISCARD friend auto operator!=(
	    const zip_iterator& z1,
	    const zip_iterator& z2) noexcept(noexcept(z1.pos1 == z2.pos1)) -> bool {
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
KBLIB_NODISCARD auto zip(InputIt1 begin1, EndIt end1, InputIt2 begin2) noexcept(
    zip_iterator<InputIt1, EndIt, InputIt2>::is_nothrow_copyable)
    -> zip_iterator<InputIt1, EndIt, InputIt2> {
	return {begin1, end1, begin2};
}

/**
 * @brief Iterate over two ranges in lockstep, like Python's zip.
 *
 * @param r1 The first range.
 * @param r2 The second range.
 * @return zip_iterator<decltype(begin(r1)), decltype(end(r1)),
 * decltype(begin(r2))> A range (and also an iterator) which represents the two
 * ranges taken in pairs.
 */
template <typename Range1, typename Range2>
KBLIB_NODISCARD auto zip(Range1&& r1, Range2&& r2) noexcept(
    zip_iterator<decltype(begin(r1)), decltype(end(r1)),
                 decltype(begin(r2))>::is_nothrow_copyable)
    -> zip_iterator<decltype(begin(r1)), decltype(end(r1)),
                    decltype(begin(r2))> {
	return {begin(r1), end(r1), begin(r2)};
}

/// TODO(killerbee): Implement adjacent_iterator
template <typename ForwardIt, typename EndIt>
class adjacent_iterator {
 public:
	using difference_type = std::ptrdiff_t;
	using base_reference = typename std::iterator_traits<ForwardIt>::reference;
	using value_type = std::pair<base_reference, base_reference>;
	using pointer = void;
	using reference = value_type;
	using iterator_category = std::input_iterator_tag;

 private:
	ForwardIt it;
};

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
	    : container(c)
	    , fun(std::move(f)) {}

	using value_type = void;
	using difference_type = void;
	using pointer = void;
	using reference = void;
	using iterator_category = std::output_iterator_tag;

	template <typename V>
	/**
	 * @brief Calls container.push_back(kblib::invoke(fun,
	 * std::forward<V>(value)));
	 *
	 * @param value The value to transform and insert.
	 * @return back_insert_iterator& *this.
	 */
	auto operator=(V&& value) -> back_insert_iterator_F& {
		container.push_back(invoke(fun, std::forward<V>(value)));
		return *this;
	}

	/**
	 * @brief A no-op.
	 */
	auto operator*() -> back_insert_iterator_F& { return *this; }
	/**
	 * @brief A no-op.
	 */
	auto operator++() -> back_insert_iterator_F& { return *this; }

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
 private:
	F fun;

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
	explicit consume_iterator(F f)
	    : fun(std::move(f)) {}

	/**
	 * @brief Pass value to F.
	 *
	 * @param value The argument for the functor.
	 * @return consume_iterator& *this.
	 */
	template <typename V>
	auto operator=(V&& value) noexcept(noexcept(
	    kblib::invoke(fun, std::forward<V>(value)))) -> consume_iterator& {
		kblib::invoke(fun, std::forward<V>(value));
		return *this;
	}

	/**
	 * @brief A no-op.
	 */
	auto operator*() -> consume_iterator& { return *this; }
	/**
	 * @brief A no-op.
	 */
	auto operator++() -> consume_iterator& { return *this; }
	/**
	 * @brief A no-op.
	 */
	auto operator++(int) -> consume_iterator& { return *this; }
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
auto consumer(F f) -> consume_iterator<F> {
	return consume_iterator<F>{std::move(f)};
}

} // namespace kblib

#endif // KBLIB_ITERATORS_H
