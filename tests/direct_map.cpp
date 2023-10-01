#include "catch.hpp"

#include <kblib/direct_map.h>

// these tests assume char min != 0
static_assert(std::is_signed_v<char>);

TEST_CASE("direct_map") {
	kblib::direct_map<char, std::string> map;
	REQUIRE(map.begin() == map.end());
	REQUIRE((map.size()) == 0); // extra parens to silence a warning
	REQUIRE(map.empty());
	REQUIRE_FALSE(map.contains('a'));
	map['a'] = "42";
	REQUIRE(map.contains('a'));
	REQUIRE_FALSE(map.contains('b'));
	REQUIRE(map.find('a') == map.begin());
	REQUIRE_FALSE(map.begin() == map.end());
	REQUIRE(map.at('a') == "42");
	REQUIRE(map.size() == 1);
	{
		auto b = map.begin();
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE_FALSE(b == n);
		REQUIRE(n == e);
		CHECK(b->first == 'a');
		CHECK(b->second == "42");
	}
	REQUIRE(++map.begin() == map.end());
	REQUIRE(map.begin() == --map.end());
	REQUIRE(map.lower_bound('a') == map.find('a'));
	REQUIRE(map.upper_bound('a') == map.end());
	for (const auto& el : map) {
		REQUIRE(el.first == 'a');
		REQUIRE(el.second == "42");
	}

	const auto ZERO_V = "4567854";
	map['\0'] = ZERO_V;
	REQUIRE(map.contains('\0'));
	REQUIRE(map.at('a') == "42");
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

	map[decltype(map)::min()] = "2";
	REQUIRE(map.contains('a'));
	REQUIRE(map.at('a') == "42");
	REQUIRE(map.contains(map.min()));
	REQUIRE(map.at(map.min()) == "2");
	{
		auto b = map.begin();
		auto f = map.find(decltype(map)::min());
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE(b == f);
		REQUIRE(f->second == "2");
		REQUIRE_FALSE(b == n);
		REQUIRE_FALSE(b == e);
		REQUIRE_FALSE(n == e);
	}
	REQUIRE(map.size() == 3);
	REQUIRE_FALSE(++map.begin() == map.end());
	{
		auto b = map.begin();
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == decltype(map)::min());
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 0);
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 'a');
		++b;
		REQUIRE(b == map.end());
	}
	REQUIRE(std::next(map.begin(), 3) == map.end());
	REQUIRE(std::prev(map.end(), 3) == map.begin());
	REQUIRE(std::distance(map.begin(), map.end()) == 3);
	{
		auto map2 = map;
		REQUIRE(map == map2);
		auto map3 = std::move(map);
		REQUIRE(map2 == map3);
		map2.swap(map);
		REQUIRE(map == map3);
	}
}

KBLIB_UNUSED static constexpr auto l(kblib::direct_map<char, int>& map,
                                     kblib::direct_map<char, int>& map2,
                                     kblib::direct_map<char, int>& map3)
    -> bool {
	using map_t = kblib::direct_map<char, int>;
	map['a'] = 42;
	static_cast<void>(map.contains('a'));
	static_cast<void>(not map.contains('b'));
	static_cast<void>(map.find('a') == map.begin());
	static_cast<void>(map.begin() != map.end());
	static_cast<void>(map.at('a') == 42);
	static_cast<void>(map.size() == 1);
	{
		auto b = map.begin();
		auto e = map.end();
		auto n = b;
		++n;
		static_cast<void>(b != n);
		static_cast<void>(b->second == 42);
		static_cast<void>(n == e);
	}
	static_cast<void>(map.begin() == --map.end());
	static_cast<void>(map.lower_bound('a') == map.find('a'));
	static_cast<void>(map.upper_bound('a') == map.end());
	for (auto el : map) {
		static_cast<void>(el.first == 'a');
		static_cast<void>(el.second == 42);
	}

	const auto ZERO_V = 4567854;
	map['\0'] = ZERO_V;
	static_cast<void>(map.contains('\0'));
	static_cast<void>(map.at('a') == 42);
	static_cast<void>(map.at('\0') == ZERO_V);
	static_cast<void>(map.size() == 2);
	{
		auto b = map.begin();
		auto f = map.find('\0');
		auto e = map.end();
		auto n = std::next(map.begin());
		static_cast<void>(b == f);
		static_cast<void>(b != n);
		static_cast<void>(b->second == ZERO_V);
		static_cast<void>(n != e);
	}
	static_cast<void>(++map.begin() != map.end());
	static_cast<void>(std::next(map.begin(), 2) == map.end());
	static_cast<void>(std::prev(map.end(), 2) == map.begin());
	static_cast<void>(std::distance(map.begin(), map.end()) == 2);
	static_cast<void>(map.upper_bound('\t') == map.find('a'));

	map[map_t::min()] = 2;
	static_cast<void>(map.at('a') == 42);
	static_cast<void>(map.at(map_t::min()) == 2);
	{
		auto b = map.begin();
		auto f = map.find(map_t::min());
		auto e = map.end();
		auto n = std::next(map.begin());
		static_cast<void>(f->second == 2);
		static_cast<void>(b != n);
		static_cast<void>(b != e);
		static_cast<void>(n != e);
	}
	static_cast<void>(++map.begin() != map.end());
	static_cast<void>(std::next(map.begin(), 3) == map.end());
	static_cast<void>(std::prev(map.end(), 3) == map.begin());
	static_cast<void>(std::distance(map.begin(), map.end()) == 3);
	{
		map2 = map;
		static_cast<void>(map == map2);
		map3 = std::move(map);
		static_cast<void>(map2 == map3);
		map2.swap(map);
		static_cast<void>(map == map3);
	}
	return true;
}

TEST_CASE("direct_map<trivial>") {
	kblib::direct_map<char, int> map;
	REQUIRE(map.begin() == map.end());
	REQUIRE((map.size()) == 0);
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
		REQUIRE(n == e);
		CHECK(b->first == 'a');
		CHECK(b->second == 42);
	}
	REQUIRE(++map.begin() == map.end());
	REQUIRE(map.begin() == --map.end());
	REQUIRE(map.lower_bound('a') == map.find('a'));
	REQUIRE(map.upper_bound('a') == map.end());
	for (auto el : map) {
		REQUIRE(el.first == 'a');
		REQUIRE(el.second == 42);
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

	map[decltype(map)::min()] = 2;
	REQUIRE(map.contains('a'));
	REQUIRE(map.at('a') == 42);
	REQUIRE(map.contains(map.min()));
	REQUIRE(map.at(map.min()) == 2);
	{
		auto b = map.begin();
		auto f = map.find(decltype(map)::min());
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE(b == f);
		REQUIRE(f->second == 2);
		REQUIRE_FALSE(b == n);
		REQUIRE_FALSE(b == e);
		REQUIRE_FALSE(n == e);
	}
	REQUIRE(map.size() == 3);
	REQUIRE_FALSE(++map.begin() == map.end());
	{
		auto b = map.begin();
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == decltype(map)::min());
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 0);
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 'a');
		++b;
		REQUIRE(b == map.end());
	}
	REQUIRE(std::next(map.begin(), 3) == map.end());
	REQUIRE(std::prev(map.end(), 3) == map.begin());
	REQUIRE(std::distance(map.begin(), map.end()) == 3);
	{
		auto map2 = map;
		REQUIRE(map == map2);
		auto map3 = std::move(map);
		REQUIRE(map2 == map3);
		map2.swap(map);
		REQUIRE(map == map3);
	}
}

TEST_CASE("direct_map (heap)") {
	kblib::direct_map<char, std::string,
	                  std::allocator<std::pair<const char, std::string>>>
	    map;
	REQUIRE(map.begin() == map.end());
	REQUIRE((map.size()) == 0);
	REQUIRE(map.empty());
	REQUIRE_FALSE(map.contains('a'));
	map['a'] = "42";
	REQUIRE(map.contains('a'));
	REQUIRE_FALSE(map.contains('b'));
	REQUIRE(map.find('a') == map.begin());
	REQUIRE_FALSE(map.begin() == map.end());
	REQUIRE(map.at('a') == "42");
	REQUIRE(map.size() == 1);
	{
		auto b = map.begin();
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE_FALSE(b == n);
		REQUIRE(n == e);
		CHECK(b->first == 'a');
		CHECK(b->second == "42");
	}
	REQUIRE(++map.begin() == map.end());
	REQUIRE(map.begin() == --map.end());
	REQUIRE(map.lower_bound('a') == map.find('a'));
	REQUIRE(map.upper_bound('a') == map.end());
	for (const auto& el : map) {
		REQUIRE(el.first == 'a');
		REQUIRE(el.second == "42");
	}

	const auto ZERO_V = "4567854";
	map['\0'] = ZERO_V;
	REQUIRE(map.contains('\0'));
	REQUIRE(map.at('a') == "42");
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

	map[decltype(map)::min()] = "2";
	REQUIRE(map.contains('a'));
	REQUIRE(map.at('a') == "42");
	REQUIRE(map.contains(map.min()));
	REQUIRE(map.at(map.min()) == "2");
	{
		auto b = map.begin();
		auto f = map.find(decltype(map)::min());
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE(b == f);
		REQUIRE(f->second == "2");
		REQUIRE_FALSE(b == n);
		REQUIRE_FALSE(b == e);
		REQUIRE_FALSE(n == e);
	}
	REQUIRE(map.size() == 3);
	REQUIRE_FALSE(++map.begin() == map.end());
	{
		auto b = map.begin();
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == decltype(map)::min());
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 0);
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 'a');
		++b;
		REQUIRE(b == map.end());
	}
	REQUIRE(std::next(map.begin(), 3) == map.end());
	REQUIRE(std::prev(map.end(), 3) == map.begin());
	REQUIRE(std::distance(map.begin(), map.end()) == 3);
	{
		auto map2 = map;
		REQUIRE(map == map2);
		auto map3 = std::move(map);
		REQUIRE(map2 == map3);
		map2.swap(map);
		REQUIRE(map == map3);
	}
}

TEST_CASE("direct_map<trivial> (heap)") {
	kblib::direct_map<char, int,
	                  std::allocator<std::pair<const char, std::string>>>
	    map;
	REQUIRE(map.begin() == map.end());
	REQUIRE((map.size()) == 0);
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
		REQUIRE(n == e);
		CHECK(b->first == 'a');
		CHECK(b->second == 42);
	}
	REQUIRE(++map.begin() == map.end());
	REQUIRE(map.begin() == --map.end());
	REQUIRE(map.lower_bound('a') == map.find('a'));
	REQUIRE(map.upper_bound('a') == map.end());
	for (auto el : map) {
		REQUIRE(el.first == 'a');
		REQUIRE(el.second == 42);
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

	map[decltype(map)::min()] = 2;
	REQUIRE(map.contains('a'));
	REQUIRE(map.at('a') == 42);
	REQUIRE(map.contains(map.min()));
	REQUIRE(map.at(map.min()) == 2);
	{
		auto b = map.begin();
		auto f = map.find(decltype(map)::min());
		auto e = map.end();
		auto n = std::next(map.begin());
		REQUIRE(b == f);
		REQUIRE(f->second == 2);
		REQUIRE_FALSE(b == n);
		REQUIRE_FALSE(b == e);
		REQUIRE_FALSE(n == e);
	}
	REQUIRE(map.size() == 3);
	REQUIRE_FALSE(++map.begin() == map.end());
	{
		auto b = map.begin();
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == decltype(map)::min());
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 0);
		++b;
		REQUIRE_FALSE(b == map.end());
		CHECK(b->first == 'a');
		++b;
		REQUIRE(b == map.end());
	}
	REQUIRE(std::next(map.begin(), 3) == map.end());
	REQUIRE(std::prev(map.end(), 3) == map.begin());
	REQUIRE(std::distance(map.begin(), map.end()) == 3);
	{
		auto map2 = map;
		REQUIRE(map == map2);
		auto map3 = std::move(map);
		REQUIRE(map2 == map3);
		map2.swap(map);
		REQUIRE(map == map3);
	}
}
