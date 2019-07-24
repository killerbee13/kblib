#include "catch.hpp"
#include "kblib/simple.h"
#include "kblib/stats.h"
#include <iostream>

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
	std::vector<int> persistent{0, 1, 1, 2, 3, 5, 8};
	SECTION("lvalue") {
		for (auto t : kblib::enumerate(persistent)) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
		}
	}
	SECTION("const lvalue") {
		const auto& cp = persistent;
		for (auto t : kblib::enumerate(cp)) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
			static_assert(std::is_const<typename std::remove_reference<decltype(v)>::type>::value, "v must refer to const when the range is const");
		}
	}

	SECTION("iterators") {
		for (auto t : kblib::enumerate(persistent.cbegin(), persistent.cend())) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
		}
	}
	SECTION("mutable iterators") {
		std::vector<int> range{0, 1, 2, 3, 4, 5, 6, 7};
		for (auto t : kblib::enumerate(range.begin(), range.end())) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			v = 0;
		}
		REQUIRE(std::all_of(range.begin(), range.end(), [](int v) {return v == 0;}));
	}
	SECTION("reverse iterators") {
		std::vector<int> reversed{7, 6, 5, 4, 3, 2, 1, 0};
		for (auto t : kblib::enumerate(reversed.rbegin(), reversed.rend())) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			REQUIRE(v == i);
		}
	}

	SECTION("temporary") {
		for (auto t : kblib::enumerate(std::vector<int>{0, 1, 1, 2, 3, 5, 8})) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			REQUIRE(v == kblib::fibonacci(i));
		}
	}
	SECTION("vector<bool>") {
		for (auto t : kblib::enumerate(std::vector<bool>{false, true, false, true, false})) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			REQUIRE(v == i % 2);
			//print<decltype(v)>{};
		}
	}
}
