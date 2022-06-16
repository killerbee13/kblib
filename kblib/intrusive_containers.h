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

template <typename Value, auto Key,
          typename Hash = kblib::FNV_hash<member_t<Value, Key>>>
class intrusive_map {
 public:
	using value_type = Value;
	using key_type = member_t<Value, Key>;
	using mapped_type = Value;
	// etc

	template <int>
	auto get() -> auto;

 private:
	std::deque<Value> storage;
	double load_factor;
};

template <typename Value, auto Key1, auto Key2,
          typename Hash1 = kblib::FNV_hash<member_t<Value, Key1>>,
          typename Hash2 = kblib::FNV_hash<member_t<Value, Key2>>>
class intrusive_dual_map {
 public:
	using value_type = Value;
	using key_type_a = member_t<Value, Key1>;
	using key_type_b = member_t<Value, Key2>;
	using mapped_type = Value;
	// etc

	template <int>
	auto get() -> auto;

 private:
	std::deque<Value> storage;
	std::unordered_map<key_type_a, Value*, Hash1> map_a;
	std::unordered_map<key_type_b, Value*, Hash2> map_b;
};

#endif

} // namespace kblib

#endif // INTRUSIVE_CONTAINERS_H
