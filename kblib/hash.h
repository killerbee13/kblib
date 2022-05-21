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
 * @brief Provides generic facilities for hashing data, and aliases for standard
 * unordered containers using the provided hash objects.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef HASH_H
#define HASH_H

#include "iterators.h"
#include "tdecl.h"
#include "traits.h"
#include "variant.h"

#include <climits>
#include <cstdint>
#include <numeric>
#include <string>
#include <unordered_map>
#include <unordered_set>

#if KBLIB_USE_CXX17
#	include <optional>
#	include <string_view>
#	include <variant>
#endif

namespace kblib {

template <typename Integral, typename CharT>
constexpr auto to_bytes_le(Integral ival,
                           CharT (&dest)[sizeof(Integral)]) noexcept -> void {
	static_assert(std::is_integral<CharT>::value and sizeof(CharT) == 1
	                  and not std::is_same<CharT, bool>::value,
	              "CharT must be a char-like type.");
	for (int byte = 0; byte != sizeof(Integral); ++byte) {
		dest[byte] = static_cast<CharT>(to_unsigned(ival)
		                                >> to_unsigned(CHAR_BIT * byte));
	}
}

template <typename Integral, typename CharT>
constexpr auto to_bytes_be(Integral ival,
                           CharT (&dest)[sizeof(Integral)]) noexcept -> void {
	static_assert(std::is_integral<CharT>::value and sizeof(CharT) == 1
	                  and not std::is_same<CharT, bool>::value,
	              "CharT must be a char-like type.");
	for (auto byte : range(sizeof(Integral))) {
		dest[(sizeof(Integral) - 1) - byte]
		    = static_cast<CharT>(ival >> (CHAR_BIT * byte));
	}
}

template <typename Integral, typename CharT>
constexpr auto to_bytes(Integral val, CharT (&dest)[sizeof(Integral)]) noexcept
    -> void {
	static_assert(std::is_integral<CharT>::value and sizeof(CharT) == 1
	                  and not std::is_same<CharT, bool>::value,
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
	struct fnv_prime;

	template <>
	struct fnv_prime<std::uint32_t>
	    : std::integral_constant<std::uint32_t, 16777619ul> {};
	template <>
	struct fnv_prime<std::uint64_t>
	    : std::integral_constant<std::uint64_t, 1099511628211ull> {};

	template <typename UInt>
	struct fnv_prime {
		KBLIB_CONSTANT_M UInt value = (sizeof(UInt) == sizeof(std::uint64_t)
		                                   ? fnv_prime<std::uint64_t>::value
		                                   : fnv_prime<std::uint32_t>::value);
	};

	/**
	 * @brief The starting value for the FNVa hash algorithm, as a type trait.
	 */
	template <typename UInt>
	struct fnv_offset;

	template <>
	struct fnv_offset<std::uint32_t>
	    : std::integral_constant<std::uint32_t, 2166136261ul> {};
	template <>
	struct fnv_offset<std::uint64_t>
	    : std::integral_constant<std::uint64_t, 14695981039346656037ull> {};

	template <typename UInt>
	struct fnv_offset {
		KBLIB_CONSTANT_M UInt value = (sizeof(UInt) == sizeof(std::uint64_t)
		                                   ? fnv_offset<std::uint64_t>::value
		                                   : fnv_offset<std::uint32_t>::value);
	};

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
KBLIB_NODISCARD constexpr auto FNVa(Span&& s,
                                    HashInt hval
                                    = fnv::fnv_offset<HashInt>::value) noexcept
    -> HashInt {
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
KBLIB_NODISCARD constexpr auto FNVa_a(
    const CharT (&s)[N],
    HashInt hval = fnv::fnv_offset<HashInt>::value) noexcept -> HashInt {
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
KBLIB_NODISCARD constexpr auto FNV32a(
    std::string_view s,
    std::uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept
    -> std::uint32_t {
	const std::uint32_t prime = fnv::fnv_prime<std::uint32_t>::value;
	for (auto&& c : s) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
		hval *= prime;
	}
	return hval;
}
#endif

template <typename HashInt>
KBLIB_NODISCARD constexpr auto FNVa_s(
    const char* begin, std::size_t length,
    HashInt hval = fnv::fnv_offset<HashInt>::value) noexcept -> HashInt {
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
KBLIB_NODISCARD constexpr auto FNV32a_a(
    const char (&s)[N],
    std::uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept
    -> std::uint32_t {
	const std::uint32_t prime = fnv::fnv_prime<std::uint32_t>::value;
	for (auto&& c : s) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
		hval *= prime;
	}
	return hval;
}

KBLIB_NODISCARD constexpr auto FNV32a_s(
    const char* begin, std::size_t length,
    uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept
    -> std::uint32_t {
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
	KBLIB_NODISCARD constexpr auto operator""_fnv32(const char* str,
	                                                std::size_t length) noexcept
	    -> std::uint32_t {
		return FNVa_s<std::uint32_t>(str, length);
	}

	/**
	 * @brief A literal suffix that produces the FNV64a hash of a string literal.
	 */
	KBLIB_NODISCARD constexpr auto operator""_fnv64(const char* str,
	                                                std::size_t length) noexcept
	    -> std::uint64_t {
		return FNVa_s<std::uint64_t>(str, length);
	}

	// google-runtime-int not relevant here due to standard requirements
	KBLIB_NODISCARD constexpr auto operator""_fnv32(unsigned long long val)
	    -> std::uint32_t {
		unsigned char bytes[sizeof(unsigned long long)]{};
		to_bytes(val, bytes);
		return FNVa_a<std::uint32_t>(bytes);
	}

	KBLIB_NODISCARD constexpr auto operator""_fnv64(unsigned long long val)
	    -> std::uint64_t {
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
    : std::integral_constant<int, CHAR_BIT * sizeof(T)
                                      - std::numeric_limits<T>::digits
                                      - std::numeric_limits<T>::is_signed> {};

template <typename T>
constexpr int padding_bits_v = padding_bits<T>::value;

/**
 * @brief The primary template has to exist, but not be constructible, in order
 * to meet the requirements of Hash.
 *
 */
template <typename Key, typename HashInt = std::size_t, typename = void>
struct FNV_hash {
	FNV_hash() = delete;
	FNV_hash(const FNV_hash&) = delete;
	FNV_hash(FNV_hash&&) = delete;
	FNV_hash& operator=(const FNV_hash&) = delete;
	FNV_hash& operator=(FNV_hash&&) = delete;
	~FNV_hash() = delete;
};

template <typename Key, typename = void>
struct is_hashable : std::false_type {};

template <typename Key>
struct is_hashable<Key, void_if_t<std::is_constructible<FNV_hash<Key>>::value>>
    : std::true_type {};

template <typename Key>
KBLIB_CONSTANT_V is_hashable_v = is_hashable<Key>::value;

/**
 * @brief Hasher for bool.
 *
 */
template <typename HashInt>
struct FNV_hash<bool, HashInt, void> {
	KBLIB_NODISCARD constexpr auto operator()(
	    bool key, HashInt offset
	              = fnv::fnv_offset<HashInt>::value) const noexcept -> HashInt {
		char tmp[1] = {key};
		return FNVa_a(tmp, offset);
	}
};

/**
 * @brief Hasher for char
 *
 */
template <typename HashInt>
struct FNV_hash<char, HashInt, void> {
	KBLIB_NODISCARD constexpr auto operator()(
	    char key, HashInt offset
	              = fnv::fnv_offset<HashInt>::value) const noexcept -> HashInt {
		char tmp[1] = {key};
		return FNVa_a(tmp, offset);
	}
};

/**
 * @brief Hasher for signed char
 *
 */
template <typename HashInt>
struct FNV_hash<signed char, HashInt, void> {
	KBLIB_NODISCARD constexpr auto operator()(
	    signed char key,
	    HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		signed char tmp[1] = {key};
		return FNVa_a(tmp, offset);
	}
};

/**
 * @brief Hasher for unsigned char
 *
 */
template <typename HashInt>
struct FNV_hash<unsigned char, HashInt, void> {
	KBLIB_NODISCARD constexpr auto operator()(
	    unsigned char key,
	    HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		unsigned char tmp[1] = {key};
		return FNVa_a(tmp, offset);
	}
};

/**
 * @brief An empty type is treated as if it were a single null byte.
 */
template <typename T, typename HashInt>
struct FNV_hash<T, HashInt, void_if_t<std::is_empty<T>::value>> {
	KBLIB_NODISCARD constexpr auto operator()(
	    const T&, HashInt offset
	              = fnv::fnv_offset<HashInt>::value) const noexcept -> HashInt {
		return FNVa_a("", offset); // hashes the null terminator
	}
};

#if KBLIB_USE_CXX17
template <typename T>
KBLIB_CONSTANT_V is_trivially_hashable_v
    = (std::is_trivially_copyable_v<
           T> and std::has_unique_object_representations_v<T> and not std::is_empty<T>::value);
#else
template <typename T>
KBLIB_CONSTANT_V is_trivially_hashable_v
    = (std::is_integral<T>::value and padding_bits<T>::value == 0)
      or std::is_pointer<T>::value or std::is_member_object_pointer<T>::value
      or std::is_member_function_pointer<T>::value;
#endif
template <typename T>
struct is_trivially_hashable : bool_constant<is_trivially_hashable_v<T>> {};

/**
 * @brief Hasher for any trivially hashable type not explicitly mentioned above.
 *
 */
template <typename T, typename HashInt>
struct FNV_hash<
    T, HashInt,
    void_if_t<std::is_integral<T>::value and is_trivially_hashable_v<T>>> {
	KBLIB_NODISCARD constexpr auto operator()(
	    T key, HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		unsigned char tmp[sizeof(T)]{};
		to_bytes(key, tmp);
		return FNVa_a(tmp, offset);
	}
};

/**
 * @brief Hasher for any pointer type.
 *
 * @note Unfortunately, this specialization cannot be constexpr
 *
 */
// uses std::is_pointer instead of just specializing on T* for cv pointers
template <typename T, typename HashInt>
struct FNV_hash<T, HashInt, void_if_t<std::is_pointer<T>::value>> {
	KBLIB_NODISCARD auto operator()(
	    T key_in, HashInt offset
	              = fnv::fnv_offset<HashInt>::value) const noexcept -> HashInt {
		return FNV_hash<std::uintptr_t, HashInt>{}(
		    reinterpret_cast<std::uintptr_t>(key_in), offset);
	}
};

/**
 * @brief Hasher for any forward iterator type.
 *
 * @note Unfortunately, this specialization cannot be constexpr.
 *
 */
template <typename T, typename HashInt>
struct FNV_hash<T, HashInt,
                void_if_t<(std::is_base_of<std::forward_iterator_tag,
                                           typename std::iterator_traits<
                                               T>::iterator_category>::value and
                           not std::is_pointer<T>::value and
                           not is_trivially_hashable_v<T> and
                           std::is_pointer<typename fakestd::invoke_result<
                               decltype(&T::operator->), T>::type>::value)>> {
	KBLIB_NODISCARD auto operator()(
	    T key_in, HashInt offset
	              = fnv::fnv_offset<HashInt>::value) const noexcept -> HashInt {
		if (key_in
		    == T{}) { // avoid calling to_pointer on a value-initialized iterator
			return FNV_hash<std::uintptr_t, HashInt>{}(0, offset);
		} else {
			return FNV_hash<std::uintptr_t, HashInt>{}(
			    reinterpret_cast<std::uintptr_t>(to_pointer(key_in)), offset);
		}
	}
};

/**
 * @internal
 */
namespace asserts {

	template <typename Container>
	KBLIB_CONSTANT_V is_trivial_container
	    = (is_contiguous<Container>::value
	       and is_trivially_hashable_v<typename Container::value_type>);
	static_assert(is_trivial_container<std::string>,
	              "kblib bug: std::string should be trivially hashable");

} // namespace asserts

/**
 * @brief Container hasher, for contiguously-stored trivial elements
 *
 */
template <typename Container, typename HashInt>
struct FNV_hash<
    Container, HashInt,
    void_if_t<(
        is_contiguous_v<
            Container> and is_trivially_hashable_v<typename Container::value_type>)>> {

	KBLIB_NODISCARD auto hash_fast(const Container& key,
	                               HashInt offset) const noexcept -> HashInt {
		return FNVa_s<HashInt>(reinterpret_cast<const char*>(key.data()),
		                       key.size() * sizeof(*key.begin()), offset);
	}

	KBLIB_NODISCARD auto operator()(
	    const Container& key,
	    HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
#if KBLIB_USE_CXX20
		if (std::is_constant_evaluated()) {
			using T = typename Container::value_type;
			for (const auto& e : key) {
				offset = FNV_hash<T>{}(e, offset);
			}
		} else {
			return hash_fast(key, offset);
		}
#else
		return hash_fast(key, offset);
#endif
	}
};

/**
 * @brief Hasher for any non-integral trivially copyable type that has no
 * padding.
 *
 * @note Unfortunately, this specialization cannot be constexpr until C++20
 * brings std::bit_cast.
 *
 */
template <typename T, typename HashInt>
struct FNV_hash<
    T, HashInt,
    void_if_t<not is_contiguous<T>::value and not std::is_integral<T>::value
              and not std::is_pointer<T>::value
              and is_trivially_hashable_v<T>>> {
	KBLIB_NODISCARD KBLIB_CXX20(constexpr) auto operator()(
	    T key, HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		char tmp[sizeof(T)];
		std::memcpy(tmp, &key, sizeof(T));
		return FNVa_a(tmp, offset);
	}
};

/**
 * @brief Container hasher, for non-trivial elements (or non-contiguous storage)
 *
 */
template <typename Container, typename HashInt>
struct FNV_hash<
    Container, HashInt,
    void_if_t<
        value_detected<Container>::value
        and is_hashable_v<
            value_detected_t<Container>> and not hash_detected<Container>::value
        and is_iterable<Container>::value
        and not (is_contiguous<Container>::value
                 and is_trivially_hashable_v<typename Container::value_type>)
        and not is_iterator_v<Container>>> {
	KBLIB_NODISCARD constexpr auto operator()(
	    const Container& key,
	    HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		using Elem = typename Container::value_type;
		return std::accumulate(cbegin(key), cend(key), offset,
		                       [](HashInt offset, const Elem& elem) {
			                       return FNV_hash<Elem, HashInt>{}(elem, offset);
		                       });
	}
};

/**
 * @namespace detail_hash
 * @internal
 */
namespace detail_hash {

	/**
	 * @brief Hash each element of a tuple. This overload is for tuples of a
	 * single type, or as the base case for the other overload.
	 *
	 * @param tuple
	 * @param offset
	 * @return HashInt
	 */
	template <typename Tuple, typename HashInt, std::size_t I>
	constexpr auto hash_tuple_impl(const Tuple& tuple, HashInt offset,
	                               std::index_sequence<I>) noexcept -> HashInt {
		return FNV_hash<typename std::tuple_element<I, Tuple>::type, HashInt>{}(
		    std::get<I>(tuple), offset);
	}

	/**
	 * @brief Hash each element of a tuple. This overload is for tuples of at
	 * least 2 elements.
	 *
	 * @param tuple
	 * @param offset
	 * @return HashInt
	 */
	template <typename Tuple, typename HashInt, std::size_t I, std::size_t I2,
	          std::size_t... Is>
	constexpr auto hash_tuple_impl(const Tuple& tuple, HashInt offset,
	                               std::index_sequence<I, I2, Is...>) noexcept
	    -> HashInt {
		HashInt first_hash
		    = FNV_hash<typename std::tuple_element<I, Tuple>::type, HashInt>{}(
		        std::get<I>(tuple), offset);
		return hash_tuple_impl(tuple, first_hash,
		                       std::index_sequence<I2, Is...>{});
	}

#if KBLIB_USE_CXX17
	template <typename Tuple, std::size_t... Is>
	constexpr auto all_hashable_impl(std::index_sequence<Is...>) -> bool {
		return (...
		        and is_hashable_v<typename std::tuple_element<Is, Tuple>::type>);
	}
#else

	template <typename Tuple, typename IS>
	struct all_hashable_impl_t;

	template <typename Tuple, std::size_t I, std::size_t... Is>
	struct all_hashable_impl_t<Tuple, std::index_sequence<I, Is...>>
	    : bool_constant<(
	          is_hashable_v<
	              typename std::tuple_element<I, Tuple>::
	                  type> and all_hashable_impl_t<Tuple, std::index_sequence<Is...>>::value)> {
	};

	template <typename Tuple, std::size_t I>
	struct all_hashable_impl_t<Tuple, std::index_sequence<I>>
	    : bool_constant<
	          is_hashable_v<typename std::tuple_element<I, Tuple>::type>> {};

	template <typename Tuple, std::size_t... Is>
	constexpr auto all_hashable_impl(std::index_sequence<Is...>) -> bool {
		return all_hashable_impl_t<Tuple, std::index_sequence<Is...>>::value;
	}
#endif

	template <
	    typename Tuple,
	    typename std::enable_if<(std::tuple_size<Tuple>::value > 0u), int>::type
	    = 0>
	constexpr auto all_hashable() -> bool {
		return all_hashable_impl<Tuple>(
		    std::make_index_sequence<std::tuple_size<Tuple>::value>{});
	}

} // namespace detail_hash

/**
 * @brief Tuple-like (but not array-like) type hasher
 *
 */
template <typename Tuple, typename HashInt>
struct FNV_hash<Tuple, HashInt,
                void_if_t<detail_hash::all_hashable<Tuple>()
                          and not is_trivially_hashable_v<
                              Tuple> and (std::tuple_size<Tuple>::value > 0u)
                          and not is_linear_container_v<Tuple>>> {
	KBLIB_NODISCARD constexpr auto operator()(
	    const Tuple& key,
	    HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		return detail_hash::hash_tuple_impl(
		    key, offset,
		    std::make_index_sequence<std::tuple_size<Tuple>::value>{});
	}
};

#if KBLIB_USE_CXX17

template <typename T, typename HashInt>
struct FNV_hash<std::optional<T>, HashInt, void> {
	KBLIB_NODISCARD constexpr auto operator()(
	    const std::optional<T>& key,
	    HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		if (key) {
			return FNV_hash<T, HashInt>{}(key.value(), offset);
		} else {
			return FNV_hash<std::nullopt_t, HashInt>{}(std::nullopt, offset);
		}
	}
};

template <typename... Ts, typename HashInt>
struct FNV_hash<std::variant<Ts...>, HashInt,
                void_if_t<detail_hash::all_hashable<std::tuple<Ts...>>()>> {
	KBLIB_NODISCARD constexpr auto operator()(
	    const std::variant<Ts...>& key,
	    HashInt offset = fnv::fnv_offset<HashInt>::value) const noexcept
	    -> HashInt {
		// Variant index is hashed alongside the value
		offset = FNV_hash<HashInt>{}(key.index(), offset);
		// visit2_nop does nothing when the variant is valueless_by_exception
		kblib::visit2_nop(key, [&](auto& V) {
			offset = FNV_hash<typename std::remove_reference<decltype(V)>::type,
			                  HashInt>{}(V, offset);
		});
		return offset;
	}
};

#endif

template <typename Key, typename Value>
using hash_map = std::unordered_map<Key, Value, FNV_hash<Key>>;
template <typename Key, typename Value>
using hash_multimap = std::unordered_multimap<Key, Value, FNV_hash<Key>>;
template <typename T, typename HashInt>
using hash_set = std::unordered_set<T, FNV_hash<T>>;
template <typename T, typename HashInt>
using hash_multiset = std::unordered_set<T, FNV_hash<T>>;

} // namespace kblib

#endif // HASH_H
