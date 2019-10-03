#include "kblib/simple.h"
#include "catch.hpp"
#include "kblib/build.h"
#include "kblib/stats.h"
#include <iostream>
#include <list>
#include <iterator>
#include <sstream>

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

		auto target10  = std::vector<int>{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
		auto target10m = std::vector<int>{0, -1, -2, -3, -4, -5, -6, -7, -8, -9};
		auto target10r = std::vector<int>{10, 9, 8, 7, 6, 5, 4, 3, 2, 1};

		REQUIRE(equal(kblib::range(0, 10), target10));
		REQUIRE(equal(kblib::range(10), target10));
		REQUIRE(equal(kblib::range(0, 10, kblib::incrementer{}), target10));

		REQUIRE(equal(kblib::range(0, 10, 2), std::vector<int>{0, 2, 4, 6, 8}));

		REQUIRE(equal(kblib::range(10, 0, -1),
		              target10r));
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
		auto range = kblib::range(0, 9);

		auto l = kblib::buildiota<std::list<int>>(10, 10, -1);
		auto li = kblib::buildiota<std::vector<std::list<int>::iterator>>(l.size(),
		                                                                  l.begin());
		{
			decltype(li) its;
			const auto end = l.end();
			for (auto it = l.begin(); it != end; ++it) {
				its.push_back(it);
			}
			REQUIRE(its == li);
		}
		REQUIRE(equal(kblib::range(10, 0, -1), l));
	}
}

TEST_CASE("range from iterators") {
	std::string str = "abcdefghijklmnopqrstuvwxyz";
	for (std::string::iterator c : kblib::range(str.begin(), str.end())) {

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
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const when the range is const");
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
		REQUIRE(std::all_of(range.begin(), range.end(),
		                    [](int v) { return v == 0; }));
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
		for (auto t : kblib::enumerate(
		         std::vector<bool>{false, true, false, true, false})) {
			auto& v = std::get<0>(t);
			auto& i = std::get<1>(t);
			REQUIRE(v == i % 2);
			// print<decltype(v)>{};
		}
	}
}
