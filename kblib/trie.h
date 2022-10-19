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
 * @brief Provides the trie data structure.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef TRIE_H
#define TRIE_H

#include "sort.h"
#include "tdecl.h"
#include "traits.h"

#include <array>
#include <bitset>
#include <vector>

namespace kblib {

enum class extractor_policy {
	forward_iteration,
	random_access,
};

template <typename Container>
struct iterator_extractor {
	using value_type =
	    typename std::remove_cv<typename std::remove_reference<decltype(
	        *begin(std::declval<Container&>()))>::type>::type;
};

template <typename Container>
struct indexer_extractor : iterator_extractor<Container> {
	using value_type =
	    typename std::remove_cv<typename std::remove_reference<decltype(
	        std::declval<Container&>()[0])>::type>::type;

	template <typename index_type>
	KBLIB_NODISCARD constexpr static auto subscript(
	    Container&& c, index_type index) noexcept(noexcept(c[index]))
	    -> decltype(auto) {
		return c[index];
	}
};

template <typename Container, typename = void>
struct extractor_policy_for {
	constexpr static extractor_policy value
	    = extractor_policy::forward_iteration;
};

template <typename Container>
struct extractor_policy_for<Container,
                            void_t<decltype(std::declval<Container>()[0])>> {
	constexpr static extractor_policy value = extractor_policy::random_access;
};

template <typename Container>
using default_extractor_t = typename std::conditional<
    extractor_policy_for<Container>::value == extractor_policy::random_access,
    indexer_extractor<Container>, iterator_extractor<Container>>::type;

namespace detail {

	template <typename Elem, typename Value>
	struct node {};

} // namespace detail

template <typename Key, typename T,
          typename Extractor = default_extractor_t<Key>,
          bool = kblib::is_linear_container<Key>::value>
class trie {
 public:
	using key_type = Key;
	using mapped_type = T;
	using value_type = std::pair<key_type, mapped_type>;
	using extractor = Extractor;
	using node_type = detail::node<typename extractor::value_type, value_type>;

	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using reference = T&;
	using const_reference = const T&;
	using pointer = T*;
	using const_pointer = const T*;

	using iterator = void;
	using const_iterator = void;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

 private:
	std::unique_ptr<node_type> root;

 public:
};

template <typename KeyElem, typename T, typename Extractor>
class trie<KeyElem, T, Extractor, false> {};

template <typename KeyElem, typename T, typename Extractor>
class trie<KeyElem[], T, Extractor, false> {};

// Array-mapped tries are better for denser sets
// Tree-based tries are better for sparser sets

// trie_map and trie_set are mostly API compatible with std::map, std::set
// trie_qmap and trie_qset use proxy iterators to avoid having to store Keys
// (saves memory, less compatible with stdlib)

template <typename Key, typename = void>
struct default_extract;

template <typename Key>
struct default_extract<Key, void_if_t<is_linear_container_v<Key>>> {
	using value_type = typename Key::value_type;
	static_assert(static_cast<value_type>(max) < static_cast<std::size_t>(max),
	              "Key too large to index array");
	// won't overflow because of above assertion
	KBLIB_CONSTANT_M std::size_t key_cardinality
	    = static_cast<value_type>(max) + std::size_t{1};
	static_assert(std::is_integral<value_type>::value,
	              "Key elements must be integral");

	KBLIB_NODISCARD static constexpr auto begin(Key& key) noexcept(
	    noexcept(key.begin())) -> decltype(auto) {
		return key.begin();
	}
	KBLIB_NODISCARD static constexpr auto end(Key& key) noexcept(
	    noexcept(key.end())) -> decltype(auto) {
		return key.end();
	}
	KBLIB_NODISCARD static constexpr auto index(
	    Key& key, std::size_t idx) noexcept(noexcept(key[idx]))
	    -> decltype(auto) {
		return key[idx];
	}
};

template <typename KeyElem>
struct default_extract<KeyElem[], void_if_t<std::is_integral_v<KeyElem>>> {
	using value_type = KeyElem;
	static_assert(static_cast<value_type>(max) < static_cast<std::size_t>(max),
	              "Key too large to index array");
	// won't overflow because of above assertion
	KBLIB_CONSTANT_M std::size_t key_cardinality
	    = static_cast<value_type>(max) + std::size_t{1};
	static_assert(std::is_integral<value_type>::value,
	              "Key elements must be integral");

	template <std::size_t Size>
	KBLIB_NODISCARD static constexpr auto begin(KeyElem (&key)[Size]) noexcept(
	    noexcept(std::begin(key))) -> decltype(auto) {
		return std::begin(key);
	}
	template <std::size_t Size>
	KBLIB_NODISCARD static constexpr auto end(KeyElem (&key)[Size]) noexcept(
	    noexcept(std::end(key))) -> decltype(auto) {
		return std::end(key);
	}
	template <std::size_t Size>
	KBLIB_NODISCARD static constexpr auto index(
	    KeyElem (&key)[Size], std::size_t idx) noexcept(noexcept(key[idx]))
	    -> decltype(auto) {
		return key[idx];
	}
};

template <typename Key>
struct default_extract<Key, void_if_t<is_radix_sortable_v<Key>>>;

// qset: does not store Keys (generates them on-demand)
// Key: the type used for lookup
// Extractor: controls how lookup is performed from the key
// offset_type: used to represent jumps in the internal array. Must be a signed
// integral type
template <typename Key, typename Extractor = default_extract<Key>,
          typename offset_type = std::ptrdiff_t>
class trie_qset {
 public:
	using key_type = Key;
	using value_type = Key;
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;

	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;

	using extractor = Extractor;
	using key_elem = typename extractor::value_type;
	KBLIB_CONSTANT_M std::size_t key_elem_cardinality
	    = extractor::key_cardinality;

 private:
	// Bitset stores leaf status, array holds jumps, 0 represents no jump (leaf
	// or no entry)
	using row_type = std::pair<std::bitset<key_elem_cardinality>,
	                           std::array<offset_type, key_elem_cardinality>>;
	std::vector<row_type> data_;
};

template <typename Key, typename Extractor = default_extract<Key>,
          typename offset_type = std::ptrdiff_t>
class trie_set {
 public:
	using key_type = Key;
	using value_type = Key;
	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;

	using reference = value_type&;
	using const_reference = const value_type&;
	using pointer = value_type*;
	using const_pointer = const value_type*;

	using extractor = Extractor;
	using key_elem = typename extractor::value_type;
	KBLIB_CONSTANT_M std::size_t key_elem_cardinality
	    = extractor::key_cardinality;

 private:
	// offset 0 represents no jump (leaf or no entry)
	using row_type = std::array<std::pair<std::optional<Key>, offset_type>,
	                            key_elem_cardinality>;
	std::vector<row_type> data_;
};

template <typename Key, typename Value,
          typename Extractor = default_extract<Key>,
          typename offset_type = std::ptrdiff_t,
          template <typename> typename SequenceContainer = std::vector>
class trie_map {};
template <typename Key, typename Value,
          typename Extractor = default_extract<Key>,
          typename offset_type = std::ptrdiff_t,
          template <typename> typename SequenceContainer = std::vector>
class trie_qmap {};

template <typename Key, typename Extractor = default_extract<Key>>
class sparse_trie_set {};

template <typename Key, typename Value,
          typename Extractor = default_extract<Key>>
class sparse_trie_map {};

} // namespace kblib

#endif // TRIE_H
