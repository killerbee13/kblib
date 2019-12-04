#ifndef KBLIB_SIMPLE_H
#define KBLIB_SIMPLE_H

#include "iterators.h"
#include "tdecl.h"
#include "traits.h"

#include <bitset>
#include <climits>
#include <cstdint>
#include <initializer_list>
#include <limits>
#include <numeric>

#define KBLIB_DEBUG_LOG_RANGES 0
#if KBLIB_DEBUG_LOG_RANGES
#include <iostream>
#endif

#if KBLIB_USE_CXX17
#include <string_view>
#endif



namespace kblib {

/**
 * @brief Transforms a stateless binary operation into one which takes reversed
 * arguments.
 *
 * For example, kblib::flip<std::minus<>>() returns a function object which
 * subtracts its first argument from its second.
 *
 * @tparam BinaryOperation The operation to reverse.
 */
template <typename BinaryOperation>
constexpr auto flip() {
	return [](auto&& a, auto&& b) {
		return BinaryOperation{}(static_cast<decltype(b)>(b),
		                         static_cast<decltype(a)>(a));
	};
}

/**
 * @brief Transforms a binary operation into one which takes reversed arguments.
 *
 * For example, kblib::flip(std::minus<>{}) returns a function object which
 * subtracts its first argument from its second.
 *
 * @param op The operation to reverse.
 */
template <typename BinaryOperation>
constexpr auto flip(BinaryOperation op) {
	return [op = std::move(op)](auto&& a, auto&& b) {
		return op(static_cast<decltype(b)>(b), static_cast<decltype(a)>(a));
	};
}

template <typename Integral, typename CharT>
constexpr void to_bytes_le(Integral ival,
                           CharT (&dest)[sizeof(Integral)]) noexcept {
	static_assert(std::is_integral<CharT>::value && sizeof(CharT) == 1 &&
	                  !std::is_same<CharT, bool>::value,
	              "CharT must be a char-like type.");
	typename std::make_signed<Integral>::type val = ival;
	for (int byte = 0; byte != sizeof(Integral); ++byte) {
		dest[byte] = (CharT)(val >> (CHAR_BIT * byte));
	}
}

template <typename Integral, typename CharT>
constexpr void to_bytes_be(Integral ival,
                           CharT (&dest)[sizeof(Integral)]) noexcept {
	static_assert(std::is_integral<CharT>::value && sizeof(CharT) == 1 &&
	                  !std::is_same<CharT, bool>::value,
	              "CharT must be a char-like type.");
	typename std::make_signed<Integral>::type val = ival;
	for (int byte = 0; byte != sizeof(Integral); ++byte) {
		dest[(sizeof(Integral) - 1) - byte] = (CharT)(val >> (CHAR_BIT * byte));
	}
}

template <typename Integral, typename CharT>
constexpr void to_bytes(Integral val,
                        CharT (&dest)[sizeof(Integral)]) noexcept {
	static_assert(std::is_integral<CharT>::value && sizeof(CharT) == 1 &&
	                  !std::is_same<CharT, bool>::value,
	              "CharT must be a char-like type.");
	if (hash_order == endian::little) {
		to_bytes_le(val, dest);
	} else {
		to_bytes_be(val, dest);
	}
}

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
constexpr HashInt
FNVa(Span&& s, HashInt hval = fnv::fnv_offset<HashInt>::value) noexcept {
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
constexpr HashInt
FNVa_a(const CharT (&s)[N],
       HashInt hval = fnv::fnv_offset<HashInt>::value) noexcept {
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
constexpr inline std::uint32_t
FNV32a(std::string_view s,
       uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept {
	const std::uint32_t prime = fnv::fnv_prime<std::uint32_t>::value;
	for (auto&& c : s) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
		hval *= prime;
	}
	return hval;
}
#endif

template <typename HashInt>
constexpr HashInt
FNVa_s(const char* begin, std::size_t length,
       HashInt hval = fnv::fnv_offset<HashInt>::value) noexcept {
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
constexpr std::uint32_t
FNV32a_a(const char (&s)[N],
         uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept {
	const std::uint32_t prime = fnv::fnv_prime<std::uint32_t>::value;
	for (auto&& c : s) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
		hval *= prime;
	}
	return hval;
}

constexpr inline std::uint32_t
FNV32a_s(const char* begin, std::size_t length,
         uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept {
	const std::uint32_t prime = fnv::fnv_prime<std::uint32_t>::value;
	for (const char* pos = begin; pos != begin + length; ++pos) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(*pos));
		hval *= prime;
	}
	return hval;
}

inline namespace literals {
	/**
	 * @brief A literal suffix that produces the FNV32a hash of a string literal.
	 */
	constexpr std::uint32_t operator""_fnv32(const char* str,
	                                         std::size_t length) noexcept {
		return FNVa_s<std::uint32_t>(str, length);
	}

	/**
	 * @brief A literal suffix that produces the FNV64a hash of a string literal.
	 */
	constexpr std::uint64_t operator""_fnv64(const char* str,
	                                         std::size_t length) noexcept {
		return FNVa_s<std::uint64_t>(str, length);
	}

	constexpr std::uint32_t operator""_fnv32(unsigned long long val) {
		unsigned char bytes[sizeof(unsigned long long)]{};
		to_bytes(val, bytes);
		return FNVa_a<std::uint32_t>(bytes);
	}

	constexpr std::uint64_t operator""_fnv64(unsigned long long val) {
		unsigned char bytes[sizeof(unsigned long long)]{};
		to_bytes(val, bytes);
		return FNVa_a<std::uint64_t>(bytes);
	}

} // namespace literals

/**
 * @brief Get the number of padding bits in an integral type.
 *
 */
template <typename T>
struct padding_bits
    : std::integral_constant<int, CHAR_BIT * sizeof(T) -
                                      std::numeric_limits<T>::digits -
                                      std::numeric_limits<T>::is_signed> {};

template <typename T>
constexpr int padding_bits_v = padding_bits<T>::value;

/**
 * @brief The primary template has to exist, but not be constructible, in order
 * to meet the requirements of Hash.
 *
 */
template <typename Key, typename = void>
struct FNV_hash {
	FNV_hash() = delete;
	FNV_hash(FNV_hash&&) = delete;
	FNV_hash& operator=(FNV_hash&&) = delete;
};

/**
 * @brief Hasher for bool. Note that because bool has only two states, this
 * specialization does not actually use FNV to hash the bool, and it uses a more
 * trivial algorithm instead.
 *
 */
template <>
struct FNV_hash<bool, void> {
	constexpr std::size_t
	operator()(bool key,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		return key ? offset : ~offset;
	}
};

/**
 * @brief Hasher for char
 *
 */
template <>
struct FNV_hash<char, void> {
	constexpr std::size_t
	operator()(char key,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief Hasher for signed char
 *
 */
template <>
struct FNV_hash<signed char, void> {
	constexpr std::size_t
	operator()(signed char key,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		signed char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief Hasher for unsigned char
 *
 */
template <>
struct FNV_hash<unsigned char, void> {
	constexpr std::size_t
	operator()(unsigned char key,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		unsigned char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief Hasher for any integral type not explicitly mentioned above.
 *
 */
template <typename T>
struct FNV_hash<T, void_if_t<std::is_integral<T>::value>> {
	constexpr std::size_t
	operator()(T key,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		unsigned char tmp[sizeof(T)]{};
		to_bytes(key, tmp);
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief Hasher for any non-integral trivial type.
 *
 * @note Unfortunately, this specialization cannot be constexpr until C++20
 * brings std::bit_cast.
 *
 */
template <typename T>
struct FNV_hash<
    T, void_if_t<std::is_trivial<T>::value && !std::is_integral<T>::value>> {
	constexpr std::size_t
	operator()(T key,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		char tmp[sizeof(T)];
		std::memcpy(tmp, &key, sizeof(T));
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief Container hasher
 *
 */
template <typename Container>
struct FNV_hash<Container, void_t<typename Container::value_type>> {
	constexpr std::size_t
	operator()(const Container& key,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		using Elem = typename Container::value_type;
		return std::accumulate(key.cbegin(), key.cend(), offset,
		                       [](std::size_t offset, const Elem& elem) {
			                       return FNV_hash<Elem>{}(elem, offset);
		                       });
	}
};

namespace detail {

   /**
	 * @brief Hash each element of a tuple. This overload is for tuples of a
	 * single type, or as the base case for the other overload.
	 *
	 * @param tuple
	 * @param offset
	 * @return std::size_t
	 */
   template <typename Tuple, std::size_t I>
   constexpr std::size_t hash_tuple_impl(const Tuple& tuple, std::size_t offset,
	                                      std::index_sequence<I>) noexcept {
		return FNV_hash<typename std::tuple_element<I, Tuple>::type>{}(
		    std::get<I>(tuple), offset);
	}

	/**
	 * @brief Hash each element of a tuple. This overload is for tuples of at
	 * least 2 elements.
	 *
	 * @param tuple
	 * @param offset
	 * @return std::size_t
	 */
	template <typename Tuple, std::size_t I, std::size_t I2, std::size_t... Is>
	constexpr std::size_t
	hash_tuple_impl(const Tuple& tuple, std::size_t offset,
	                std::index_sequence<I, I2, Is...>) noexcept {
		std::size_t first_hash =
		    FNV_hash<typename std::tuple_element<I, Tuple>::type>{}(
		        std::get<I>(tuple), offset);
		return hash_tuple_impl(tuple, first_hash,
		                       std::index_sequence<I2, Is...>{});
	}

} // namespace detail

/**
 * @brief Tuple-like (but not array-like) type hasher
 *
 */
template <typename Tuple>
struct FNV_hash<Tuple, void_if_t<(std::tuple_size<Tuple>::value > 0u) &&
                                 !is_linear_container_v<Tuple>>> {
   constexpr std::size_t
   operator()(const Tuple& key,
              std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
       noexcept {
      return detail::hash_tuple_impl(
          key, offset,
          std::make_index_sequence<std::tuple_size<Tuple>::value>{});
   }
};

/**
 * @brief The hash of an empty tuple is trivial.
 */
template <typename Tuple>
struct FNV_hash<Tuple, void_if_t<std::tuple_size<Tuple>::value == 0u &&
                                 !is_linear_container_v<Tuple>>> {
	constexpr std::size_t
	operator()(const Tuple&,
	           std::size_t offset = fnv::fnv_offset<std::size_t>::value) const
	    noexcept {
		return offset;
	}
};

template <typename T, std::size_t N>
constexpr return_assert_t<std::is_integral<T>::value, bool> is_consecutive(const T (&array)[N]) {
	if constexpr (N <= 1) {
		return true;
	} else if constexpr (N == 2) {
		return (array[1] - array[0]) == 1;
	} else {
		for (std::size_t i = 1; i < N; ++i) {
			if ((array[i] - array[i - 1]) != 1) {
				return false;
			}
		}
		return true;
	}
}

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
	filg2(const std::bitset<std::numeric_limits<std::uintmax_t>::digits>
	          val) noexcept {
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

/**
 * @brief Concatenate two dynamic containers together.
 *
 * @param A,B The containers to concatenate together.
 * @return Container A container consisting of the elements of A and B in that
 * order.
 */
template <typename Container>
Container arraycat(Container A,
                   Container&& B) noexcept(noexcept(A.insert(A.end(), B.begin(),
                                                             B.end()))) {
	A.insert(A.end(), B.begin(), B.end());
	return A;
}

/**
 * @brief Index an array literal without naming its type
 *
 * @attention The return value must not be stored unless the argument is also
 * stored. This is indexable because temporaries live until the end of their
 * full-expression, rather than sub-expression
 *
 * @param a The array literal to index into.
 */
template <typename T>
constexpr auto a(const std::initializer_list<T>& a) {
	return a.begin();
}
// use like:
// auto v = a({2, 3, 5, 7, 9, 11})[2];

template <typename T>
/**
 * @brief A simple identity alias for simplifying some compound type
 * declarations, such as function pointers.
 */
using alias = T;

/**
 * @brief Ignores its first template argument entirely, and returns its second
 */
template <typename, typename T>
struct ignore {
	using type = T;
};
/**
 * @brief An alias for ignore<U, T>::type
 *
 */
template <typename U, typename T>
using ignore_t = typename ignore<U, T>::type;

#if KBLIB_USE_CXX17

template <bool... args>
constexpr bool conjunction = (args && ...);
#endif

#if KBLIB_USE_CXX20

template <typename T>
concept zero_constructible = requires(T t) {
  {t = 0};
};

struct zero_t {
  consteval zero_t(int i) { i == 0 ? void() : throw 0; }
  template <zero_constructible T>
  constexpr operator T() const noexcept {
	 return 0;
  }
  explicit constexpr operator std::nullptr_t() const noexcept {
	 return nullptr;
  }
};

#endif

} // namespace kblib

#undef KBLIB_DEBUG_LOG_RANGES

#endif // KBLIB_SIMPLE_H
