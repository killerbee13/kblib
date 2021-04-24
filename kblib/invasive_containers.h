#ifndef INVASIVE_CONTAINERS_H
#define INVASIVE_CONTAINERS_H

#include "hash.h"
#include "traits.h"
#include <deque>
#include <unordered_map>

namespace kblib {

#if KBLIB_USE_CXX17

template <typename Value, auto Key,
          typename Hash = kblib::FNV_hash<member_t<Value, Key>>>
class invasive_map {
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
class invasive_dual_map {
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

#endif // INVASIVE_CONTAINERS_H
