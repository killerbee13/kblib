#ifndef KBLIB_SIMPLE_H
#define KBLIB_SIMPLE_H

#include "tdecl.h"
#include "traits.h"

#include <bitset>
#include <climits>
#include <cstdint>
#include <initializer_list>
#include <limits>

#define KBLIB_DEBUG_LOG_RANGES 0
#if KBLIB_DEBUG_LOG_RANGES
#include <iostream>
#endif

#if KBLIB_USE_CXX17
#include <string_view>
#endif

namespace kblib {

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
		constexpr iterator& operator++() {
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
		return (l.begin() == r.begin()) && (l.end() == r.end()) && (l.step == r.step);
	}
	/**
	 * @brief Compare l and r for inequality.
	 *
	 * Ranges are equal when they generate identical ranges.
	 */
	constexpr friend bool operator!=(range_t l, range_t r) {
		return !(l == r);
	}

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
	constexpr operator int() { return 1; }
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
	constexpr operator int() { return -1; }
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

	enumerate_iterator& operator++() {
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

	const_iterator begin() const noexcept(noexcept(r.cbegin())) {
		return {r.cbegin(), 0};
	}
	iterator begin() noexcept(noexcept(r.begin())) { return {r.begin(), 0}; }

	const_iterator end() const noexcept(noexcept(r.cend())) { return {r.cend(), -std::size_t{1}}; }
	end_iterator end() noexcept(noexcept(r.end())) { return {r.end(), -std::size_t{1}}; }
};

template <typename It, typename EndIt>
struct enumerate_t {
	using nested_iterator = It;
	using iterator = enumerate_iterator<nested_iterator>;
	using end_iterator = enumerate_iterator<EndIt>;

	iterator begin() const noexcept { return {r_begin, 0}; }

	end_iterator end() const noexcept { return {r_end, -std::size_t{1}}; }

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

/**
 * @todo Write indirect_range
 */
template <typename Iter1, typename Iter2>
struct indirect_range {
	Iter1 begin_;
	Iter2 end_;

	Iter1 begin() const noexcept { return begin_; }
	Iter2 end() const noexcept { return end_; }
};

namespace fnv {

   /**
	 * @brief The prime to use for the FNVa hash algorithm, as a type trait.
	 */
   template <typename UInt>
   struct fnv_prime {};

	template <>
	struct fnv_prime<std::uint32_t>
	    : std::integral_constant<std::uint32_t, 16777619ul> {};
	template <>
	struct fnv_prime<std::uint64_t>
	    : std::integral_constant<std::uint64_t, 1099511628211ull> {};

	/**
	 * @brief The starting value for the FNVa hash algorithm, as a type trait.
	 */
	template <typename UInt>
	struct fnv_offset {};

	template <>
	struct fnv_offset<std::uint32_t>
	    : std::integral_constant<std::uint32_t, 2166136261ul> {};
	template <>
	struct fnv_offset<std::uint64_t>
	    : std::integral_constant<std::uint64_t, 14695981039346656037ull> {};

} // namespace fnv

/**
 * @brief A templatized generic FNVa hash function.
 *
 * @tparam HashInt The unsigned integer type to use as the hash result. Must be
 * either std::uint32_t or std::uint64_t.
 * @param s The data to hash. Any range-for-iterable span of char-like objects.
 * @param hval The initial value for the hash accumulator. Pass in another hash
 * value to create a hash of the concatenation of the two ranges.
 * @return HashInt The FNVa hash of the input range.
 */
template <typename HashInt, typename Span>
constexpr HashInt FNVa(Span&& s,
                       HashInt hval = fnv::fnv_offset<HashInt>::value) {
	static_assert(sizeof(*std::begin(s)) == 1,
	              "Can only hash char-like objects.");
	const HashInt prime = fnv::fnv_prime<HashInt>::value;
	for (auto&& c : s) {
		hval ^= static_cast<HashInt>(static_cast<unsigned char>(c));
		hval *= prime;
	}
	return hval;
}

/**
 * @brief A templatized FNVa hash function, for raw character arrays, such as
 * string literals.
 *
 * @tparam HashInt The unsigned integer type to use as the hash result. Must be
 * either std::uint32_t or std::uint64_t.
 * @param s The data to hash. A raw array of char-like objects.
 * @param hval The initial value for the hash accumulator. Pass in another hash
 * value to create a hash of the concatenation of the two ranges.
 * @return HashInt The FNVa hash of the input range.
 */
template <typename HashInt, typename CharT, std::size_t N>
constexpr HashInt FNVa_a(const CharT (&s)[N],
                         HashInt hval = fnv::fnv_offset<HashInt>::value) {
	static_assert(sizeof(s[0]) == 1, "Can only hash char-like objects.");
	const HashInt prime = fnv::fnv_prime<HashInt>::value;
	for (auto&& c : s) {
		hval ^= static_cast<HashInt>(static_cast<unsigned char>(c));
		hval *= prime;
	}
	return hval;
}

#if KBLIB_USE_CXX17
/**
 * @brief A standard FNV32a hash function, for string_views.
 *
 * @param s The data to hash.
 * @param hval The initial value for the hash accumulator. Pass in another hash
 * value to create a hash of the concatenation of the two ranges.
 * @return std::uint32_t The FNV32a hash of the input range.
 */
constexpr inline std::uint32_t FNV32a(std::string_view s,
                                      uint32_t hval = 2166136261) {
	const std::uint32_t FNV_32_PRIME = 16777619;
	for (auto&& c : s) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
		hval *= FNV_32_PRIME;
	}
	return hval;
}
#endif

template <typename HashInt>
constexpr HashInt FNVa_s(const char* begin, std::size_t length,
                         HashInt hval = fnv::fnv_offset<HashInt>::value) {
	const HashInt prime = fnv::fnv_prime<HashInt>::value;
	//  for (const char* pos = begin; pos != begin + length; ++pos) {
	//    hval ^= static_cast<HashInt>(static_cast<unsigned char>(*pos));
	//    hval *= prime;
	//  }
	for (const char* pos : range(begin, begin + length)) {
		hval ^= static_cast<HashInt>(static_cast<unsigned char>(*pos));
		hval *= prime;
	}
	return hval;
}

/**
 * @brief A standard FNV32a hash function, for raw character arrays, such as
 * string literals.
 *
 * @param s The data to hash.
 * @param hval The initial value for the hash accumulator. Pass in another hash
 * value to create a hash of the concatenation of the two ranges.
 * @return HashInt The FNV32a hash of the input range.
 */
template <std::size_t N>
constexpr std::uint32_t FNV32a_a(const char (&s)[N],
                                 uint32_t hval = 2166136261) {
	const std::uint32_t FNV_32_PRIME = 16777619;
	for (auto&& c : s) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
		hval *= FNV_32_PRIME;
	}
	return hval;
}

constexpr inline std::uint32_t FNV32a_s(const char* begin, std::size_t length,
                                        uint32_t hval = 2166136261) {
	const std::uint32_t FNV_32_PRIME = 16777619;
	for (const char* pos = begin; pos != begin + length; ++pos) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(*pos));
		hval *= FNV_32_PRIME;
	}
	return hval;
}

inline namespace literals {
	/**
	 * @brief A literal suffix that produces the FNV32a hash of a string literal.
	 */
	constexpr std::uint32_t operator""_fnv32(const char* str,
	                                         std::size_t length) {
		return FNV32a_s(str, length);
	}

	/**
	 * @brief A literal suffix that produces the FNV64a hash of a string literal.
	 */
	constexpr std::uint64_t operator""_fnv64(const char* str,
	                                         std::size_t length) {
		return FNVa_s<std::uint64_t>(str, length);
	}

} // namespace literals

template <typename T>
struct padding_bits
    : std::integral_constant<int, CHAR_BIT * sizeof(T) -
                                      std::numeric_limits<T>::digits -
                                      std::numeric_limits<T>::is_signed> {};

template <typename T>
constexpr int padding_bits_v = padding_bits<T>::value;

template <typename Key, typename = void>
struct FNV_hash {
	FNV_hash() = delete;
	FNV_hash(FNV_hash&&) = delete;
	FNV_hash& operator=(FNV_hash&&) = delete;
};

template <>
struct FNV_hash<bool, void> {
	std::size_t operator()(bool key) {
		char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp);
	}
};

template <>
struct FNV_hash<char, void> {
	std::size_t operator()(char key) {
		char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp);
	}
};

template <>
struct FNV_hash<signed char, void> {
	std::size_t operator()(signed char key) {
		signed char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp);
	}
};

template <>
struct FNV_hash<unsigned char, void> {
	std::size_t operator()(unsigned char key) {
		unsigned char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp);
	}
};

template <typename T>
struct FNV_hash<T, fakestd::void_t<typename std::enable_if<
                       std::is_integral<T>::value, T>::type>> {
	std::size_t operator()(T key) {
		char tmp[sizeof(T)];
		std::memcpy(tmp, &key, sizeof(T));
		return FNVa_a<std::size_t>(tmp);
	}
};

#if KBLIB_USE_CXX17
template <typename CharT>
struct FNV_hash<std::basic_string_view<CharT>, void> {
	std::size_t operator()(std::basic_string_view<CharT> key) {
		return FNVa(key);
	}
};
#endif

namespace detail {

   /**
	 * @brief Floored integer binary logarithm. Returns floor(lb(val)).
	 *
	 * Returns the number of significant bits in the given integer.
	 *
	 * @param val
	 * @return int
	 */
   constexpr int
	filg2(const std::bitset<std::numeric_limits<std::uintmax_t>::digits> val) {
		for (int i : range(to_signed(val.size() - 1), std::ptrdiff_t{0}, -1)) {
			if (val[i])
				return i;
		}
		return 0;
	}

} // namespace detail

template <std::size_t size, typename T, typename... Ts>
struct first_bigger_than
    : std::conditional<sizeof(T) >= size, detail::tag<T>,
                       typename first_bigger_than<size, Ts...>::type> {};

template <std::size_t size, typename T>
struct first_bigger_than<size, T>
    : std::conditional_t<sizeof(T) >= size, detail::tag<T>, void> {};

template <std::uintmax_t I>
using uint_smallest =
    typename first_bigger_than<1 + detail::filg2(I) / CHAR_BIT, unsigned char,
                               unsigned short, unsigned int, unsigned long,
                               unsigned long long, std::uintmax_t>::type;

template <std::uintmax_t I>
using int_smallest = typename first_bigger_than<
    1 + (detail::filg2(I) + 1) / CHAR_BIT, signed char, signed short,
    signed int, signed long, signed long long, std::uintmax_t>::type;

template <std::uintmax_t I>
using uint_smallest_t = typename uint_smallest<I>::type;

template <std::uintmax_t I>
using int_smallest_t = typename int_smallest<I>::type;

template <typename Container>
/**
 * @brief
 *
 * @param A
 * @param B
 * @return Container
 */
Container arraycat(Container A, Container&& B) {
	A.insert(A.end(), B.begin(), B.end());
	return A;
}

// Index an array literal without naming its type
// Caveat: the return value must not be stored unless the argument is also
// stored. This is indexable because temporaries live until the end of their
// full-expression, rather than sub-expression
template <typename T>
/**
 * @brief
 *
 * @param a
 */
constexpr auto a(const std::initializer_list<T>& a) {
	return a.begin();
}
// use like:
// auto v = a({2, 3, 5, 7, 9, 11})[2];

template <typename T>
/**
 * @brief
 *
 */
using alias = T;

template <typename, typename T>
/**
 * @brief
 *
 */
struct ignore {
	/**
	 * @brief
	 *
	 */
	using type = T;
};
template <typename U, typename T>
/**
 * @brief
 *
 */
using ignore_t = typename ignore<U, T>::type;

#if KBLIB_USE_CXX17
template <bool... args>
constexpr bool conjunction = (args && ...);
#endif

} // namespace kblib

#undef KBLIB_DEBUG_LOG_RANGES

#endif // KBLIB_SIMPLE_H
