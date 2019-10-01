#ifndef KBLIB_SIMPLE_H
#define KBLIB_SIMPLE_H

#include "tdecl.h"
#include "traits.h"
#include "iterators.h"

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
