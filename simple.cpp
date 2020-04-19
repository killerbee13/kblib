#include "kblib/simple.h"
#include "catch.hpp"
#include "kblib/build.h"
#include "kblib/stats.h"

#include <array>
#include <deque>
#include <iostream>
#include <iterator>
#include <list>
#include <map>
#include <sstream>
#include <unordered_map>

struct has_padding {
	char c;
	int i;
};

struct empty_t {};

static_assert(kblib::is_linear_container_v<std::string> &&
              kblib::is_contiguous_v<std::string> &&
              kblib::is_trivially_hashable_v<std::string::value_type>);
static_assert(kblib::asserts::is_trivial_container<std::vector<char>>);

TEST_CASE("FNV_hash") {
	kblib::FNV_hash<int*>{}({});
	kblib::FNV_hash<std::string>{}({});
	kblib::FNV_hash<std::vector<char>>{}({});
	kblib::FNV_hash<std::vector<int>>{}({});
	kblib::FNV_hash<std::deque<char>>{}({});
	kblib::FNV_hash<std::tuple<>>{}({});
	kblib::FNV_hash<std::tuple<int*>>{}({});
	kblib::FNV_hash<std::tuple<std::wstring, int*>>{}({});

	std::unordered_map<std::tuple<std::wstring, int*>,
	                   std::vector<std::basic_string<bool>>,
	                   kblib::FNV_hash<std::tuple<std::wstring, int*>>>
	    test_map;
	kblib::FNV_hash<std::map<std::tuple<std::wstring, int*>,
	                         std::vector<std::basic_string<bool>>,
	                         kblib::FNV_hash<std::tuple<std::wstring, int*>>>>
	    test_hash1;
	KBLIB_UNUSED kblib::FNV_hash<std::array<std::basic_string<bool>, 4>>
	    test_hash2;
	KBLIB_UNUSED auto call = &decltype(test_hash1)::operator();
	//	std::hash<
	//	    std::unordered_map<std::wstring, std::vector<std::basic_string<bool>>,
	//	                       std::hash<std::wstring>>>
	//	    std_hash;
	using namespace kblib::literals;
	kblib::FNV_hash<unsigned long long> h_i;
	static_assert(3452452_fnv64 == h_i(3452452ull),
	              "hash literal and FNH_hash don't agree");

	REQUIRE(kblib::FNV_hash<int>{}(1000) == 3434534542295815964);
	kblib::FNV_hash<std::vector<int>>{}({});
	kblib::FNV_hash<std::vector<empty_t>>{}({});
	// fails because of padding:
	// kblib::FNV_hash<std::vector<has_padding>>{}({});
	// fails because of unorderedness:
	// kblib::FNV_hash<std::unordered_map<int, int>>{}({});
}

TEST_CASE("range comparison") {
	auto equal = [](auto r1, auto r2) {
		auto r1b = r1.begin();
		auto r1e = r1.end();
		auto r2b = r2.begin();
		auto r2e = r2.end();
		return (std::distance(r1b, r1e) == std::distance(r2b, r2e)) &&
		       kblib::equal(r1b, r1e, r2b);
	};

	SECTION("equivalency") {

		auto target10 = std::vector<int>{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
		auto target10m = std::vector<int>{0, -1, -2, -3, -4, -5, -6, -7, -8, -9};
		auto target10r = std::vector<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1};

		REQUIRE(equal(kblib::range(0, 10), target10));
		REQUIRE(equal(kblib::range(10), target10));
		REQUIRE(equal(kblib::range(0, 10, kblib::incrementer{}), target10));

		REQUIRE(equal(kblib::range(0, 10, 2), std::vector<int>{0, 2, 4, 6, 8}));

		REQUIRE(equal(kblib::range(10, 0, -1), target10r));
		REQUIRE(equal(kblib::range(10, 0, kblib::decrementer{}), target10r));

		REQUIRE(equal(kblib::range(0, -10, -1), target10m));
		REQUIRE(equal(kblib::range(0, -10), target10m));

		REQUIRE(equal(kblib::range(0, 11, 2), kblib::range(0, 12, 2)));
	}

	SECTION("comparisons") {
		REQUIRE(kblib::range(0, 0, 1) == kblib::range(1, 1, 2));
		REQUIRE(kblib::range(100, 100, 1) == kblib::range(0, 0, 2));
		REQUIRE(kblib::range(0, 10) != kblib::range(10, 0));
		REQUIRE(kblib::range(0, 11, 2) != kblib::range(0, 10, 2));
		REQUIRE(kblib::range(0, 0, 1) != kblib::range(0, 1, 2));
	}

	SECTION("buildiota equivalency") {
		auto l = kblib::buildiota<std::list<int>>(10, 10, -1);
		REQUIRE(equal(kblib::range(10, 0, -1), l));
	}
}

TEST_CASE("range from iterators") {
	std::string str = "abcdefghijklmnopqrstuvwxyz";
	// Just asserting that this loop compiles.
	for ([[gnu::unused]] std::string::iterator c :
	     kblib::range(str.begin(), str.end())) {
	}
}

TEST_CASE("ranges overflow") {
	unsigned char c{};
	for (auto i : kblib::range(static_cast<unsigned char>(255))) {
		c = i;
	}
	REQUIRE(c == 254);
}

template <typename>
struct print;

TEST_CASE("enumerate") {
	std::vector<unsigned long long> persistent{0, 1, 1, 2, 3, 5, 8};
	SECTION("lvalue") {
		for (auto t : kblib::enumerate(persistent)) {
			[[gnu::unused]] auto& v = std::get<0>(t);
			[[gnu::unused]] auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
		}
	}
	SECTION("const lvalue") {
		const auto& cp = persistent;
		for (auto t : kblib::enumerate(cp)) {
			[[gnu::unused]] auto& v = std::get<0>(t);
			[[gnu::unused]] auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const when the range is const");
		}
	}

	SECTION("iterators") {
		for (auto t : kblib::enumerate(persistent.cbegin(), persistent.cend())) {
			[[gnu::unused]] auto& v = std::get<0>(t);
			[[gnu::unused]] auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
		}
	}
	SECTION("mutable iterators") {
		std::vector<int> range{0, 1, 2, 3, 4, 5, 6, 7};
		for (auto t : kblib::enumerate(range.begin(), range.end())) {
			[[gnu::unused]] auto& v = std::get<0>(t);
			[[gnu::unused]] auto& i = std::get<1>(t);
			v = 0;
		}
		REQUIRE(std::all_of(range.begin(), range.end(),
		                    [](int v) { return v == 0; }));
	}
	SECTION("reverse iterators") {
		std::vector<std::size_t> reversed{7, 6, 5, 4, 3, 2, 1, 0};
		for (auto t : kblib::enumerate(reversed.rbegin(), reversed.rend())) {
			[[gnu::unused]] auto& v = std::get<0>(t);
			[[gnu::unused]] auto& i = std::get<1>(t);
			REQUIRE(v == i);
		}
	}

	SECTION("temporary") {
		for (auto t : kblib::enumerate(
		         std::vector<unsigned long long>{0, 1, 1, 2, 3, 5, 8})) {
			[[gnu::unused]] auto& v = std::get<0>(t);
			[[gnu::unused]] auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
		}
	}
	SECTION("vector<bool>") {
		for (auto t : kblib::enumerate(
		         std::vector<bool>{false, true, false, true, false})) {
			[[gnu::unused]] auto& v = std::get<0>(t);
			[[gnu::unused]] auto& i = std::get<1>(t);
			REQUIRE(v == i % 2);
			// print<decltype(v)>{};
		}
	}
}
