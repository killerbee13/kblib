#include "kblib/iterators.h"
#include "catch.hpp"
#include <iterator>
#include <sstream>

#if KBLIB_USE_CXX17

TEST_CASE("magic_enumerate") {
	std::vector<int> persistent{0, 1, 1, 2, 3, 5, 8};
	SECTION("lvalue") {
		for (auto&& [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("non-forwarding lvalue") {
		for (auto& [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("copy") {
		for (auto [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
		}
	}

	SECTION("const lvalue") {
		const auto& cp = persistent;
		for (auto&& [i, v] : kblib::magic_enumerate(cp)) {
			REQUIRE(&v == &cp[i]);
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const when the range is const");
		}
	}
	SECTION("const reference") {
		for (const auto& [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(&v == &persistent[i]);
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const when the bound reference is const");
		}
	}
	SECTION("copy from const") {
		const auto& cp = persistent;
		for (auto [i, v] : kblib::magic_enumerate(cp)) {
			static_assert(
			    !std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must not be const when copied from a const range");
		}
	}
	SECTION("const copy from non-const") {
		for (const auto [i, v] : kblib::magic_enumerate(persistent)) {
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const in a const copy");
		}
	}

	SECTION("iterators") {
		for (auto&& [i, v] :
		     kblib::magic_enumerate(persistent.cbegin(), persistent.cend())) {
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("mutable iterators") {
		std::vector<int> range{0, 1, 2, 3, 4, 5, 6, 7};
		for (auto&& [i, v] : kblib::magic_enumerate(range.begin(), range.end())) {
			v = 0;
		}
		REQUIRE(std::all_of(range.begin(), range.end(),
		                    [](int v) { return v == 0; }));
	}
	SECTION("reverse iterators") {
		std::vector<int> reversed{7, 6, 5, 4, 3, 2, 1, 0};
		for (auto&& [i, v] :
		     kblib::magic_enumerate(reversed.rbegin(), reversed.rend())) {
			REQUIRE(v == i);
		}
	}

	SECTION("temporary") {
		for (auto&& [i, v] :
		     kblib::magic_enumerate(std::vector<int>(persistent))) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
		}
	}
	SECTION("input iterators") {
		std::istringstream is{"0 0 1 1 2 2 3 3 4 4 5 5 6 6"};
		for (auto&& [i, v] : kblib::magic_enumerate(
		         std::istream_iterator<int>(is), std::istream_iterator<int>())) {
			REQUIRE(v == i / 2);
		}
	}
	SECTION("copied input iterators") {
		std::istringstream is{"0 0 1 1 2 2 3 3 4 4 5 5 6 6"};
		for (auto [i, v] : kblib::magic_enumerate(std::istream_iterator<int>(is),
		                                          std::istream_iterator<int>())) {
			REQUIRE(v == i / 2);
		}
	}

	SECTION("move-only") {
		std::vector<std::unique_ptr<int>> ptr_vec(10);
		for (auto&& [i, ptr] : kblib::magic_enumerate(ptr_vec)) {
			(void)i;
		}
	}
}

#endif
