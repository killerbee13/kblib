#ifndef INVASIVE_CONTAINERS_H
#define INVASIVE_CONTAINERS_H

#include "traits.h"
#include <deque>
#include <unordered_map>

namespace kblib {

#if KBLIB_USE_CXX17

template <typename Value, auto Key1, auto Key2, typename Hash1 = std::hash<member_t<Value, Key1>>, typename Hash2 = std::hash<member_t<Value, Key2>>>
class invasive_map {
 public:
	using value_type = Value;
	using key_type_a = member_t<Value, Key1>;
	using key_type_b = member_t<Value, Key2>;
	using mapped_type = Value;
	// etc

	template<int> auto get();

private:
	std::deque<Value> storage;
	std::unordered_map<key_type_a, Value*, Hash1> map_a;
	std::unordered_map<key_type_b, Value*, Hash2> map_b;
};

#endif

} // namespace kblib

#endif // INVASIVE_CONTAINERS_H
