#ifndef KBLIB_ITERATORS_H
#define KBLIB_ITERATORS_H

#include "fakestd.h"

#include <cassert>
#include <iterator>

#if KBLIB_USE_CXX17
#include <optional>
#endif

namespace kblib {

template <typename ptr>
struct to_pointer_impl {
	constexpr auto operator()(ptr&& p) const noexcept {
		return to_pointer_impl<decltype(p.operator->())>{}(p.operator->());
	}
	constexpr auto operator()(const ptr& p) const noexcept {
		return to_pointer_impl<decltype(p.operator->())>{}(p.operator->());
	}
};

template <typename T>
struct to_pointer_impl<T*> {
	constexpr T* operator()(T* p) const noexcept { return p; }
};

/**
 * @brief Gets a raw pointer out of any smart pointer or iterator you might pass
 * in, without dereferencing it or relying on a get() method.
 *
 * @param p A smart pointer to extract from.
 */
template <typename P>
constexpr auto to_pointer(P&& p) noexcept {
	return to_pointer_impl<std::decay_t<P>>{}(p);
}

template <typename Container,
          typename Comp = std::less<value_type_linear_t<Container>>>
value_type_linear_t<Container>* max_element(Container& c, Comp comp) {
	auto it = fakestd::max_element(std::begin(c), std::end(c), comp);
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
struct is_output_iterator : std::false_type {};

template <typename T, typename E>
struct is_output_iterator<
    T, E,
    void_t<decltype(*std::declval<T&>() = std::declval<const E&>())>>
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
	    : container(std::addressof(c)), count(n) {}
	explicit counting_back_insert_iterator(std::size_t n) : count(n) {}

	struct proxy_iterator {
		using value_type = typename Container::value_type;

		proxy_iterator& operator=(const value_type& value) & {
			assert(container);
			// Multiple assignments for a single dereference are not allowed
			assert(*dirty);
			*dirty = false;
			container->push_back(value);
			return *this;
		}

		proxy_iterator& operator=(value_type&& value) & {
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

	proxy_iterator operator*() noexcept {
		assert(dirty);
		return {container, &dirty};
	}

	counting_back_insert_iterator& operator++() & noexcept {
		assert(!dirty);
		++count;
		dirty = true;
		return *this;
	}
	counting_back_insert_iterator operator++(int) noexcept = delete;

	friend bool operator==(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) {
		return a.count == b.count;
	}
	friend bool operator!=(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) {
		return a.count != b.count;
	}
	friend bool operator<(const counting_back_insert_iterator& a,
	                      const counting_back_insert_iterator& b) {
		return a.count < b.count;
	}
	friend bool operator<=(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) {
		return a.count <= b.count;
	}
	friend bool operator>(const counting_back_insert_iterator& a,
	                      const counting_back_insert_iterator& b) {
		return a.count > b.count;
	}
	friend bool operator>=(const counting_back_insert_iterator& a,
	                       const counting_back_insert_iterator& b) {
		return a.count >= b.count;
	}
	friend std::ptrdiff_t operator-(const counting_back_insert_iterator& a,
	                                const counting_back_insert_iterator& b) {
		return std::ptrdiff_t(a.count) - ptrdiff_t(b.count);
	}

 protected:
	Container* container = nullptr;
	std::size_t count = 0;
	bool dirty = true;
};

template <typename C>
KBLIB_NODISCARD counting_back_insert_iterator<C>
counting_back_inserter(C& c, std::size_t count = 0) {
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
	    : min(min_), max(max_), step(step_) {
		normalize();
	}
	/**
	 * @brief 1-argument constructor. Start is implicitly zero and step is 1 or
	 * -1, depending on the sign of max.
	 *
	 * @param max The end of the range.
	 */
	constexpr range_t(Value max)
	    : range_t(Value{}, max, (max >= Value{}) ? 1 : -1) {}

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
		constexpr Value operator*() { return val; }
		/**
		 * @brief Return a pointer to the value.
		 *
		 * @return pointer A pointer to a value equivalent to *(*this). Valid
		 * until the iterator is modified in any way or destroyed.
		 */
		constexpr pointer operator->() { return &val; }
		/**
		 * @brief Prefix increment. Advance to the next value in the range.
		 *
		 * @return iterator& *this.
		 */
		constexpr iterator& operator++() & {
			val = val + step;
			return *this;
		}
		/**
		 * @brief Postfix increment. Advance to the next value in the range, but
		 * return the current value.
		 *
		 * @return iterator A copy of the pre-incrementing value of *this.
		 */
		constexpr iterator operator++(int) {
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
		constexpr friend bool operator==(iterator l, iterator r) {
			return l.val == r.val && l.step == r.step;
		}
		/**
		 * @brief Compare two range iterators for inequality.
		 *
		 * Range iterators compare equal if they point to the same value and have
		 * the same step.
		 */
		constexpr friend bool operator!=(iterator l, iterator r) {
			return l.val != r.val || l.step != r.step;
		}
		/**
		 * @brief Compare two range iterators.
		 *
		 * For range iterators, (A < B) is true when A can be advanced until (*A -
		 * *B) changes sign.
		 */
		constexpr friend bool operator<(iterator l, iterator r) {
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
		constexpr friend bool operator<=(iterator l, iterator r) {
			return !(r < l);
		}
		/**
		 * @brief Compare two range iterators.
		 *
		 * For range iterators, (A < B) is true when A can be advanced until (*A -
		 * *B) changes sign.
		 */
		constexpr friend bool operator>(iterator l, iterator r) { return r < l; }
		/**
		 * @brief Compare two range iterators.
		 *
		 * For range iterators, (A < B) is true when A can be advanced until (*A -
		 * *B) changes sign.
		 */
		constexpr friend bool operator>=(iterator l, iterator r) {
			return !(l < r);
		}
	};

	/**
	 * @brief Returns an iterator to the beginning of the range.
	 */
	constexpr iterator begin() const { return {min, step}; }
	/**
	 * @brief Return an iterator to the end of the range.
	 */
	constexpr iterator end() const { return {max, step}; }

	/**
	 * @brief Returns the distance between start() and stop().
	 */
	constexpr std::size_t size() const { return (max - min) / step; }

	/**
	 * @brief Compare l and r for equality.
	 *
	 * Ranges are equal when they generate identical ranges.
	 */
	constexpr friend bool operator==(range_t l, range_t r) {
		return (l.begin() == r.begin()) && (l.end() == r.end()) &&
		       (l.step == r.step);
	}
	/**
	 * @brief Compare l and r for inequality.
	 *
	 * Ranges are equal when they generate identical ranges.
	 */
	constexpr friend bool operator!=(range_t l, range_t r) { return !(l == r); }

 private:
	Value min, max;
	Delta step;

	constexpr void normalize() {
#if KBLIB_DEBUG_LOG_RANGES
		std::clog << "(" << min << ", " << max << ", " << step << ") -> ";
#endif
		if (min == max) {
			min = Value{};
			max = Value{};
			step = 1;
		} else if (step == 0) {
			if (min != std::numeric_limits<Value>::max()) {
				max = min + 1;
			} else {
				max = min - 1;
			}
		} else {
			auto difference = max - min;
			int sign = (step > 0) ? 1 : -1;
			if ((sign * difference) <= (sign * step)) {
				step = sign;
				max = min + step;
			} else {
				auto remainder = difference % step;
				if (remainder != 0) {
					max = max + step;
					max = max - remainder;
				}
			}
		}
#if KBLIB_DEBUG_LOG_RANGES
		std::clog << "(" << min << ", " << max << ", " << step << ")\n";
#endif
	}
};

/**
 * @brief A struct which increments anything it is added to. Suitable for use as
 * a Delta type for range_t.
 */
struct incrementer {
	constexpr incrementer() = default;
	constexpr incrementer(int) {}
	constexpr operator int() const noexcept { return 1; }
};

/**
 * @brief Increments val.
 */
template <typename T>
constexpr T operator+(T val, incrementer) {
	return ++val;
}

/**
 * @brief A struct which decrements anything it is added to. Suitable for use as
 * a Delta type for range_t.
 */
struct decrementer {
	constexpr decrementer() = default;
	constexpr decrementer(int) {}
	constexpr operator int() const noexcept { return -1; }
};

/**
 * @brief Decrements val.
 */
template <typename T>
constexpr T operator+(T val, decrementer) {
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
constexpr range_t<Value, Delta> range(Value min, Value max, Delta step = 0) {
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
 * @brief Constructs a half-open range [from 0 to max. [0, max). The step is
 * automatically determined based on the sign of max.
 *
 * @param max The first value not in the produced range.
 * @return range_t<Value, int> An iterable range [0, max).
 */
template <typename Value>
constexpr range_t<Value, int> range(Value max) {
	return {max};
}

// enumerate partially based on code by Tobias Widlund. License reproduced
// below. Original source available at https://github.com/therocode/enumerate

/*
 * MIT License
 *
 * Copyright (c) 2018 Tobias Widlund
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

namespace detail {

   template <typename T>
   struct no_dangle {
		using type = T&;
	};

	template <typename T>
	struct no_dangle<T&&> {
		using type = T;
	};

	template <typename T>
	using no_dangle_t = typename no_dangle<T>::type;
} // namespace detail

template <typename It>
struct enumerate_iterator {
	It it;
	std::size_t idx;

	using nested_reference = typename std::iterator_traits<It>::reference;

	using difference_type = std::ptrdiff_t;
	using value_type = std::pair<nested_reference, std::size_t>;
	using pointer = void;
	using reference = value_type;
	using iterator_category = std::input_iterator_tag;

	value_type operator*() { return {*it, idx}; }

	enumerate_iterator& operator++() & {
		++it;
		++idx;
		return *this;
	}
	enumerate_iterator operator++(int) {
		auto tmp = *this;
		++(*this);
		return tmp;
	}

	template <typename OIt>
	auto operator==(OIt rhs)
	    -> decltype(std::declval<It&>() == std::declval<OIt&>()) {
		return it == rhs;
	}
	template <typename OIt>
	auto operator!=(OIt rhs)
	    -> decltype(std::declval<It&>() != std::declval<OIt&>()) {
		return it != rhs;
	}

	friend bool operator==(enumerate_iterator lhs, enumerate_iterator rhs) {
		return lhs.it == rhs.it;
	}
	friend bool operator!=(enumerate_iterator lhs, enumerate_iterator rhs) {
		return lhs.it != rhs.it;
	}
};

template <typename Range, typename = void>
struct enumerate_t;

template <typename Range>
struct enumerate_t<Range, void> {
	detail::no_dangle_t<Range> r;

	using range_t = typename std::remove_reference_t<Range>;
	using nested_iterator = decltype(r.begin());
	using nested_end_iterator = decltype(r.end());
	using iterator = enumerate_iterator<nested_iterator>;
	using end_iterator = enumerate_iterator<nested_end_iterator>;

	using nested_const_iterator = typename range_t::const_iterator;
	using const_iterator = enumerate_iterator<nested_const_iterator>;

	const_iterator begin() const& noexcept(noexcept(r.cbegin())) {
		return {r.cbegin(), 0};
	}
	iterator begin() & noexcept(noexcept(r.begin())) { return {r.begin(), 0}; }

	const_iterator end() const& noexcept(noexcept(r.cend())) {
		return {r.cend(), -std::size_t{1}};
	}
	end_iterator end() & noexcept(noexcept(r.end())) {
		return {r.end(), -std::size_t{1}};
	}
};

template <typename It, typename EndIt>
struct enumerate_t {
	using nested_iterator = It;
	using iterator = enumerate_iterator<nested_iterator>;
	using end_iterator = enumerate_iterator<EndIt>;

	iterator begin() const& noexcept { return {r_begin, 0}; }

	end_iterator end() const& noexcept { return {r_end, -std::size_t{1}}; }

	It r_begin;
	EndIt r_end;
};

/**
 * @brief Allow access to indexes while using range-based for loops. Safe to use
 * with rvalues.
 *
 * @param r A range to iterate over.
 */
template <typename Range>
enumerate_t<Range&&> enumerate(Range&& r) {
	return {std::forward<Range>(r)};
}

/**
 * @brief Allow access to indexes while using range-based for loops.
 *
 * @param begin The beginning of the input range.
 * @param end The end of the input range.
 */
template <typename It, typename EIt>
enumerate_t<It, EIt> enumerate(It begin, EIt end) {
	return {begin, end};
}

#if KBLIB_USE_CXX17

template <typename T>
class enumerator_iterator;

namespace detail {

   template <typename T1, typename T2>
   decltype(auto) get_or(T1&& t1, T2&& t2) {
		return t1 ? *t1 : *t2;
	}

	struct force_copy_tag {};
	// Get a pointer which is guaranteed to be invalid to use
	template <typename T>
	T* get_magic_ptr() {
		static const char enumeration_magic_pointer = '\0';
		return reinterpret_cast<T*>(
		    const_cast<char*>(&enumeration_magic_pointer));
	}

} // namespace detail

template <typename T>
class enumeration {
 public:
	enumeration() = default;

	enumeration(const enumeration& other)
	    : idx(other.idx), local([&] {
		      assert(other.source || other.local);
				assert(other.source != detail::get_magic_ptr<T>());
				return *other;
	      }()),
	      source(nullptr) {}
	enumeration(volatile enumeration& other)
	    : enumeration(const_cast<const enumeration&>(other)) {}

 private:
	enumeration(detail::force_copy_tag, std::size_t i) : idx(i) {}

 public:
	std::size_t index() const noexcept { return idx; }

	std::remove_const_t<T>& operator*() & noexcept {
		assert(source != detail::get_magic_ptr<T>());
		assert(local);
		return *local;
	}
	const T& operator*() const& noexcept {
		assert(source != detail::get_magic_ptr<T>());
		return detail::get_or(local, source);
	}

	T& operator*() volatile& noexcept {
		auto& self = *const_cast<enumeration*>(this);
		assert(self.source != detail::get_magic_ptr<T>());
		return detail::get_or(self.local, self.source);
	}
	const T& operator*() const volatile& noexcept {
		auto& self = *const_cast<const enumeration*>(this);
		assert(self.source != detail::get_magic_ptr<T>());
		return detail::get_or(self.local, self.source);
	}

 private:
	void set(T* t) & { source = t; }

	void advance() & noexcept {
		++idx;
		source = detail::get_magic_ptr<T>();
		local = std::nullopt;
	}

	std::size_t idx = 0;
	mutable std::optional<std::remove_const_t<T>> local = std::nullopt;
	T* source = detail::get_magic_ptr<T>();

	template <typename>
	friend class enumerator_iterator;
};
} // namespace kblib

namespace std {
#if defined(__clang__)
    // Fix from: https://github.com/nlohmann/json/issues/1401
    #pragma clang diagnostic push
    #pragma clang diagnostic ignored "-Wmismatched-tags"
#endif

template <typename T>
class tuple_size<::kblib::enumeration<T>>
    : public std::integral_constant<std::size_t, 2> {};

template <typename T>
class tuple_element<0, ::kblib::enumeration<T>> {
 public:
	using type = std::size_t;
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

#if defined(__clang__)
    #pragma clang diagnostic pop
#endif

} // namespace std

namespace kblib {

template <std::size_t I, typename T>
decltype(auto) get(const enumeration<T>& e) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return e.index();
	} else {
		return *e;
	}
}

template <std::size_t I, typename T>
decltype(auto) get(enumeration<T>& e) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return e.index();
	} else {
		return *e;
	}
}

template <std::size_t I, typename T>
decltype(auto) get(volatile enumeration<T>& e) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return const_cast<enumeration<T>&>(e).index();
	} else {
		return *e;
	}
}

template <std::size_t I, typename T>
decltype(auto) get(const volatile enumeration<T>& e) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return const_cast<enumeration<T>&>(e).index();
	} else {
		return *e;
	}
}

template <std::size_t I, typename T>
decltype(auto) get(enumeration<T>&& e) {
	static_assert(I <= 1, "enumeration only has two elements");
	if constexpr (I == 0) {
		return e.index();
	} else {
		return *e;
	}
}

template <typename It>
class enumerator_iterator {
 public:
	using nested_value =
	    copy_const_t<decltype(*std::declval<It&>()), typename It::value_type>;

	using value_type = enumeration<nested_value>;
	using difference_type = std::ptrdiff_t;
	using pointer = const value_type*;
	using reference = const value_type&;
	using iterator_category = std::input_iterator_tag;

	enumerator_iterator() = default;
	// enumerator_iterator(const enumerator_iterator& other) = delete;
	enumerator_iterator(const enumerator_iterator& other)
	    : enumerator_iterator(detail::force_copy_tag{}, other.curr_.idx,
	                          other.it_) {}
	enumerator_iterator(It it) : it_(it) {}

	volatile value_type& operator*() & {
		if (!captured) {
			curr_.set(to_pointer(it_));
			captured = true;
		}
		return curr_;
	}
	enumerator_iterator& operator++() & {
		curr_.advance();
		captured = false;
		++it_;
		return *this;
	}
	enumerator_iterator operator++(int) & {
		curr_.advance();
		captured = false;
		return {detail::force_copy_tag{}, curr_.idx, it_++};
	}

	friend bool operator==(const enumerator_iterator& lhs,
	                       const enumerator_iterator& rhs) {
		return lhs.it_ == rhs.it_;
	}
	friend bool operator!=(const enumerator_iterator& lhs,
	                       const enumerator_iterator& rhs) {
		return lhs.it_ != rhs.it_;
	}

 private:
	enumerator_iterator(detail::force_copy_tag t, std::size_t idx, It it)
	    : it_(it), curr_(t, idx) {}

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

	const_iterator begin() const& noexcept(noexcept(r.cbegin())) {
		return r.cbegin();
	}
	iterator begin() & noexcept(noexcept(r.begin())) { return r.begin(); }

	const_iterator end() const & noexcept(noexcept(r.cend())) { return r.cend(); }
	end_iterator end() & noexcept(noexcept(r.end())) { return r.end(); }
};

template <typename It, typename EndIt>
class enumerator_t {
 public:
	using nested_iterator = It;
	using iterator = enumerator_iterator<nested_iterator>;
	using end_iterator = enumerator_iterator<EndIt>;

	iterator begin() const& noexcept { return {r_begin}; }

	end_iterator end() const& noexcept { return {r_end}; }

	It r_begin;
	EndIt r_end;
};

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
enumerator_t<Range&&> magic_enumerate(Range&& r) {
	return {std::forward<Range>(r)};
}

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
 * @param begin The beginning of the input range.
 * @param end The end of the input range.
 */
template <typename It, typename EIt>
enumerator_t<It, EIt> magic_enumerate(It begin, EIt end) {
	return {begin, end};
}

#endif

/**
 * @brief Allow range-for iteration of an iterator pair.
 */
template <typename Iter1, typename Iter2>
struct indirect_range {
	Iter1 begin_;
	Iter2 end_;

	Iter1 begin() const noexcept { return begin_; }
	Iter2 end() const noexcept { return end_; }
	auto rbegin() const noexcept { return std::make_reverse_iterator(begin_); }
	auto rend() const noexcept { return std::make_reverse_iterator(end_); }
};

} // namespace kblib

#endif // KBLIB_ITERATORS_H
