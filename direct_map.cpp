#include "catch.hpp"

#include <kblib/direct_map.h>

TEST_CASE("direct_map") {
	kblib::direct_map<char, int> map;
	REQUIRE(map.begin() == map.end());
	REQUIRE(map.size() == 0);
	REQUIRE(map.empty());
	REQUIRE_FALSE(map.contains('a'));
	map['a'] = 42;
	REQUIRE(map.contains('a'));
	REQUIRE_FALSE(map.contains('b'));
	REQUIRE(map.find('a') == map.begin());
	REQUIRE_FALSE(map.begin() == map.end());
	REQUIRE(map.at('a') == 42);
	REQUIRE(map.size() == 1);
	{
		auto b = map.begin();
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE_FALSE(b == n);
		REQUIRE(b->second == 42);
		REQUIRE(n == e);
	}
	REQUIRE(map.begin() == --map.end());
	REQUIRE(map.lower_bound('a') == map.find('a'));
	REQUIRE(map.upper_bound('a') == map.end());
	for (auto [i, v] : map) {
		REQUIRE(i == 'a');
		REQUIRE(v == 42);
	}

	const auto ZERO_V = 4567854;
	map['\0'] = ZERO_V;
	REQUIRE(map.contains('\0'));
	REQUIRE(map.at('a') == 42);
	REQUIRE(map.at('\0') == ZERO_V);
	REQUIRE(map.size() == 2);
	{
		auto b = map.begin();
		auto f = map.find('\0');
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE(b == f);
		REQUIRE_FALSE(b == n);
		REQUIRE(b->second == ZERO_V);
		REQUIRE_FALSE(n == e);
	}
	REQUIRE_FALSE(++map.begin() == map.end());
	REQUIRE(std::next(map.begin(), 2) == map.end());
	REQUIRE(std::prev(map.end(), 2) == map.begin());
	REQUIRE(std::distance(map.begin(), map.end()) == 2);
	REQUIRE(map.upper_bound('\t') == map.find('a'));

	map[map.min()] = 2;
	REQUIRE(map.at('a') == 42);
	REQUIRE(map.at(map.min()) == 2);
	{
		auto b = map.begin();
		auto f = map.find(map.min());
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE(f->second == 2);
		REQUIRE_FALSE(b == n);
		REQUIRE_FALSE(b == e);
		REQUIRE_FALSE(n == e);
	}
	REQUIRE_FALSE(++map.begin() == map.end());
	REQUIRE(std::next(map.begin(), 3) == map.end());
	REQUIRE(std::prev(map.end(), 3) == map.begin());
	REQUIRE(std::distance(map.begin(), map.end()) == 3);
}
