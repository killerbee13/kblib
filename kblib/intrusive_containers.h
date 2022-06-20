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
 * @brief Provides map types which directly use the members of the value type to
 * create the map structure.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef INTRUSIVE_CONTAINERS_H
#define INTRUSIVE_CONTAINERS_H

#include "hash.h"
#include "traits.h"
#include <deque>
#include <unordered_map>

namespace kblib {

#if KBLIB_USE_CXX17

template <typename Value, auto KeyExtract, typename Hash = kblib::FNV_hash<>,
          typename KeyEqual = std::equal_to<>>
class intrusive_hash_map {
 public:
	using value_type = Value;
	using key_type
	    = remove_cvref_t<std::invoke_result_t<decltype(KeyExtract), Value&>>;
	using mapped_type = Value;

	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using hasher = Hash;
	using key_equal = KeyEqual;
	using allocator_type = void;

	using reference = value_type&;
	using const_reference = const value_type&;

	using pointer = value_type*;
	using const_pointer = const value_type*;

	using iterator = void;
	using const_iterator = void;
	using local_iterator = void;
	using const_local_iterator = void;

	// etc

	template <int>
	auto get() -> auto;

 private:
	std::deque<Value> storage;
	double load_factor;
};

template <
    typename Value, auto KeyExtract1, auto KeyExtract2,
    typename Hash1 = kblib::FNV_hash<>, typename Hash2 = kblib::FNV_hash<>,
    typename KeyEqual1 = std::equal_to<>, typename KeyEqual2 = std::equal_to<>>
class intrusive_dual_map {
 public:
	using value_type = Value;
	using key_type_a
	    = remove_cvref_t<std::invoke_result_t<decltype(KeyExtract1), Value&>>;
	using key_type_b
	    = remove_cvref_t<std::invoke_result_t<decltype(KeyExtract2), Value&>>;
	using mapped_type = Value;
	// etc

	template <int>
	auto get() -> auto;

 private:
	std::deque<Value> storage;
	std::unordered_map<key_type_a, Value*, Hash1, KeyEqual1> map_a;
	std::unordered_map<key_type_b, Value*, Hash2, KeyEqual2> map_b;
};

#endif

} // namespace kblib

#endif // INTRUSIVE_CONTAINERS_H
