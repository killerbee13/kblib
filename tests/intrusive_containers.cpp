#include "kblib/intrusive_containers.h"

#include <string>

#include "catch2/catch.hpp"

#if KBLIB_USE_CXX17

struct example {
	int first;
	int second;
	std::string payload;
};

TEST_CASE("intrusive_map") {
	using X = kblib::intrusive_hash_map<example, &example::first>;
	X a;
	const X b{};
	static_assert(std::is_same_v<X::key_type, int>);
	static_assert(std::is_same_v<X::mapped_type, example>);
	static_assert(std::is_same_v<X::value_type, example>);
	static_assert(std::is_same_v<X::hasher, kblib::FNV_hash<>>);
	static_assert(std::is_same_v<X::key_equal, std::equal_to<>>);
}

TEST_CASE("intrusive_dual_map") {
	kblib::intrusive_dual_map<example, &example::first, &example::second> map;
}

#endif
