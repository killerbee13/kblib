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
#include <optional>
#include <string_view>
#include <variant>
#endif

namespace kblib {

template <typename Integral, typename CharT>
constexpr auto to_bytes_le(Integral ival,
                           CharT (&dest)[sizeof(Integral)]) noexcept -> void {
	static_assert(std::is_integral<CharT>::value and sizeof(CharT) == 1 and
	                  not std::is_same<CharT, bool>::value,
	              "CharT must be a char-like type.");
	for (int byte = 0; byte != sizeof(Integral); ++byte) {
		dest[byte] =
		    static_cast<CharT>(to_unsigned(ival) >> to_unsigned(CHAR_BIT * byte));
	}
}

template <typename Integral, typename CharT>
constexpr auto to_bytes_be(Integral ival,
                           CharT (&dest)[sizeof(Integral)]) noexcept -> void {
	static_assert(std::is_integral<CharT>::value and sizeof(CharT) == 1 and
	                  not std::is_same<CharT, bool>::value,
	              "CharT must be a char-like type.");
	for (auto byte : range(sizeof(Integral))) {
		dest[(sizeof(Integral) - 1) - byte] =
		    static_cast<CharT>(ival >> (CHAR_BIT * byte));
	}
}

template <typename Integral, typename CharT>
constexpr auto to_bytes(Integral val, CharT (&dest)[sizeof(Integral)]) noexcept
    -> void {
	static_assert(std::is_integral<CharT>::value and sizeof(CharT) == 1 and
	                  not std::is_same<CharT, bool>::value,
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
		constexpr static UInt value = (sizeof(UInt) == sizeof(std::uint64_t)
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
		constexpr static UInt value = (sizeof(UInt) == sizeof(std::uint64_t)
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
KBLIB_NODISCARD constexpr auto
FNVa(Span&& s, HashInt hval = fnv::fnv_offset<HashInt>::value) noexcept
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
KBLIB_NODISCARD constexpr auto
FNVa_a(const CharT (&s)[N],
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
KBLIB_NODISCARD constexpr inline auto
FNV32a(std::string_view s,
       uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept
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
KBLIB_NODISCARD constexpr auto
FNVa_s(const char* begin, std::size_t length,
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
KBLIB_NODISCARD constexpr auto
FNV32a_a(const char (&s)[N],
         uint32_t hval = fnv::fnv_offset<std::uint32_t>::value) noexcept
    -> std::uint32_t {
	const std::uint32_t prime = fnv::fnv_prime<std::uint32_t>::value;
	for (auto&& c : s) {
		hval ^= static_cast<std::uint32_t>(static_cast<unsigned char>(c));
		hval *= prime;
	}
	return hval;
}

KBLIB_NODISCARD constexpr inline auto
FNV32a_s(const char* begin, std::size_t length,
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
	FNV_hash(const FNV_hash&) = delete;
	FNV_hash(FNV_hash&&) = delete;
	FNV_hash& operator=(const FNV_hash&) = delete;
	FNV_hash& operator=(FNV_hash&&) = delete;
	~FNV_hash() = delete;
};

template <typename Key, typename = void>
struct is_hashable {
	static constexpr bool value = false;
};

template <typename Key>
struct is_hashable<Key,
                   void_if_t<std::is_constructible<FNV_hash<Key>>::value>> {
	static constexpr bool value = true;
};

/**
 * @brief Hasher for bool.
 *
 */
template <>
struct FNV_hash<bool, void> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(bool key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief Hasher for char
 *
 */
template <>
struct FNV_hash<char, void> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(char key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
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
	KBLIB_NODISCARD constexpr std::size_t
	operator()(signed char key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
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
	KBLIB_NODISCARD constexpr std::size_t
	operator()(unsigned char key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		unsigned char tmp[1] = {key};
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief An empty type is treated as if it were a single null byte.
 */
template <typename T>
struct FNV_hash<T, void_if_t<std::is_empty<T>::value>> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const T&,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		return FNVa_a<std::size_t>("", offset); // hashes the null terminator
	}
};

#if KBLIB_USE_CXX17

template <typename T>
struct is_trivially_hashable {
	constexpr static bool value =
	    (std::is_trivially_copyable_v<T> and
	     std::has_unique_object_representations_v<T> and
	     not std::is_empty<T>::value);
};

template <typename T>
constexpr bool is_trivially_hashable_v = is_trivially_hashable<T>::value;

#else

template <typename T>
struct is_trivially_hashable
    : std::integral_constant<bool, (std::is_integral<T>::value and
                                    padding_bits<T>::value == 0) or
                                       std::is_pointer<T>::value> {};

template <typename T>
constexpr bool is_trivially_hashable_v = is_trivially_hashable<T>::value;
#endif

/**
 * @brief Hasher for any trivially hashable type not explicitly mentioned above.
 *
 */
template <typename T>
struct FNV_hash<T, void_if_t<std::is_integral<T>::value and
                             is_trivially_hashable<T>::value>> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(T key, std::size_t offset =
	                      fnv::fnv_offset<std::size_t>::value) const noexcept {
		unsigned char tmp[sizeof(T)]{};
		to_bytes(key, tmp);
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

#if 1
/**
 * @brief Hasher for any pointer type.
 *
 * @note Unfortunately, this specialization cannot be constexpr
 *
 */
template <typename T>
struct FNV_hash<T, void_if_t<std::is_pointer<T>::value>> {
	KBLIB_NODISCARD std::size_t
	operator()(T key_in,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		return FNV_hash<std::uintptr_t>{}(byte_cast<std::uintptr_t>(key_in),
		                                  offset);
	}
};
#endif

/**
 * @brief Hasher for any forward iterator type.
 *
 * @note Unfortunately, this specialization cannot be constexpr.
 *
 */
template <typename T>
struct FNV_hash<T, void_if_t<std::is_base_of<std::forward_iterator_tag,
                                             typename std::iterator_traits<T>::
                                                 iterator_category>::value and
                             not std::is_pointer<T>::value and
                             not is_trivially_hashable_v<T> and
                             std::is_pointer<typename fakestd::invoke_result<
                                 decltype(&T::operator->), T>::type>::value>> {
	KBLIB_NODISCARD std::size_t
	operator()(T key_in,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		if (key_in ==
		    T{}) { // avoid calling to_pointer on a value-initialized iterator
			return FNV_hash<std::uintptr_t>{}(0, offset);
		} else {
			return FNV_hash<std::uintptr_t>{}(
			    byte_cast<std::uintptr_t>(to_pointer(key_in)), offset);
		}
	}
};

namespace asserts {

	template <typename Container>
	constexpr bool is_trivial_container =
	    (is_contiguous<Container>::value and
	     is_trivially_hashable<typename Container::value_type>::value);
	static_assert(is_trivial_container<std::string>,
	              "kblib bug: std::string should be trivially hashable");

} // namespace asserts

/**
 * @brief Container hasher, for contiguously-stored trivial elements
 *
 */
template <typename Container>
struct FNV_hash<
    Container,
    void_if_t<(is_contiguous<Container>::value and
               is_trivially_hashable<typename Container::value_type>::value)>> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const Container& key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		return FNVa_s(byte_cast<const char*>(key.data()),
		              key.size() * sizeof(*key.begin()), offset);
	}
};

#if 1

/**
 * @brief Hasher for any non-integral trivially copyable type that has no
 * padding.
 *
 * @note Unfortunately, this specialization cannot be constexpr until C++20
 * brings std::bit_cast.
 *
 */
template <typename T>
struct FNV_hash<T, void_if_t<not std::is_integral<T>::value and
                             not std::is_pointer<T>::value and
                             is_trivially_hashable<T>::value>> {
	KBLIB_NODISCARD KBLIB_CXX20(constexpr) std::size_t
	operator()(T key, std::size_t offset =
	                      fnv::fnv_offset<std::size_t>::value) const noexcept {
		char tmp[sizeof(T)];
		std::memcpy(tmp, &key, sizeof(T));
		return FNVa_a<std::size_t>(tmp, offset);
	}
};

/**
 * @brief Container hasher, for non-trivial elements (or non-contiguous storage)
 *
 */
template <typename Container>
struct FNV_hash<Container,
                void_if_t<value_detected<Container>::value and
                          is_hashable<value_detected_t<Container>>::value and
                          not hash_detected<Container>::value and
                          is_iterable<Container>::value and
                          not(is_contiguous<Container>::value and
                              is_trivially_hashable<
                                  typename Container::value_type>::value) and
                          not is_iterator_v<Container>>> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const Container& key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		using Elem = typename Container::value_type;
		return std::accumulate(key.cbegin(), key.cend(), offset,
		                       [](std::size_t offset, const Elem& elem) {
			                       return FNV_hash<Elem>{}(elem, offset);
		                       });
	}
};

/**
 * @brief Container hasher, for contiguously-stored trivial elements
 */
template <typename Container>
struct FNV_hash<Container,
                void_if_t<is_linear_container_v<Container> and
                          is_contiguous_v<Container> and
                          is_trivially_hashable_v<Container::value_type>>> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const Container& key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		return FNVa_s(byte_cast<const char*>(key.data()),
		              key.size() * sizeof(*key.begin()), offset);
	}
};

#endif

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

#if KBLIB_USE_CXX17
	template <typename Tuple, std::size_t... Is>
	constexpr auto all_hashable_impl(std::index_sequence<Is...>) -> bool {
		return (... and
		        is_hashable<typename std::tuple_element<Is, Tuple>::type>::value);
	}
#else

	template <typename Tuple, typename IS>
	struct all_hashable_impl_t;

	template <typename Tuple, std::size_t I, std::size_t... Is>
	struct all_hashable_impl_t<Tuple, std::index_sequence<I, Is...>> {
		constexpr static bool value =
		    (is_hashable<typename std::tuple_element<I, Tuple>::type>::value and
		     all_hashable_impl_t<Tuple, std::index_sequence<Is...>>::value);
	};

	template <typename Tuple, std::size_t I>
	struct all_hashable_impl_t<Tuple, std::index_sequence<I>> {
		constexpr static bool value =
		    is_hashable<typename std::tuple_element<I, Tuple>::type>::value;
	};

	template <typename Tuple, std::size_t... Is>
	constexpr auto all_hashable_impl(std::index_sequence<Is...>) -> bool {
		return all_hashable_impl_t<Tuple, std::index_sequence<Is...>>::value;
	}
#endif

	template <typename Tuple,
	          typename std::enable_if<(std::tuple_size<Tuple>::value > 0u),
	                                  int>::type = 0>
	constexpr auto all_hashable() -> bool {
		return all_hashable_impl<Tuple>(
		    std::make_index_sequence<std::tuple_size<Tuple>::value>{});
	}

} // namespace detail

/**
 * @brief Tuple-like (but not array-like) type hasher
 *
 */
template <typename Tuple>
struct FNV_hash<Tuple, void_if_t<detail::all_hashable<Tuple>() and
                                 not is_trivially_hashable<Tuple>::value and
                                 (std::tuple_size<Tuple>::value > 0u) and
                                 not is_linear_container_v<Tuple>>> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const Tuple& key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		return detail::hash_tuple_impl(
		    key, offset,
		    std::make_index_sequence<std::tuple_size<Tuple>::value>{});
	}
};

#if KBLIB_USE_CXX17

namespace detail {}

template <typename T>
struct FNV_hash<std::optional<T>, void> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const std::optional<T>& key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		if (key) {
			return FNV_hash<T>{}(key.value(), offset);
		} else {
			return FNV_hash<std::nullopt_t>{}(std::nullopt, offset);
		}
	}
};

template <typename... Ts>
struct FNV_hash<std::variant<Ts...>,
                void_if_t<detail::all_hashable<std::tuple<Ts...>>()>> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const std::variant<Ts...>& key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		offset = FNV_hash<std::size_t>{}(key.index(), offset);
		kblib::visit2_nop(key, [&](auto& V) {
			offset = FNV_hash<typename std::remove_reference<decltype(V)>::type>{}(
			    V, offset);
		});
		return offset;
	}
};

#endif

template <typename Key, typename Value>
using hash_map = std::unordered_map<Key, Value, FNV_hash<Key>>;
template <typename Key, typename Value>
using hash_multimap = std::unordered_multimap<Key, Value, FNV_hash<Key>>;
template <typename T>
using hash_set = std::unordered_set<T, FNV_hash<T>>;
template <typename T>
using hash_multiset = std::unordered_set<T, FNV_hash<T>>;

} // namespace kblib

#endif // HASH_H
