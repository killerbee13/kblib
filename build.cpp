#include "kblib/build.h"
#include "catch.hpp"
#include <array>
#include <iostream>

TEST_CASE("build family", "[build]") {
	using arr = std::array<int, 8>;
	const arr input = {2, 3, 5, 7, 11, 13, 17, 19};
	const arr input2 = {1, 1, 1, 1, 1, 1, 1, 1};
	const auto unary_f = [](int x) { return x * x; };
	const auto binary_f = [](int a, int b) { return a - b; };
	const arr squared = {4, 9, 25, 49, 121, 169, 289, 361};
	const arr pminusone = {1, 2, 4, 6, 10, 12, 16, 18};

	const auto equal = [](auto a, auto b) {
		return std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b));
	};
	[[maybe_unused]] auto print_arr = [&](auto c) {
		for (const auto& v : c) {
			std::cout << v << ", ";
		}
		std::cout << '\n';
	};

	SECTION("unary dynamic build") {
		auto built =
		    kblib::build<std::vector<int>>(input.begin(), input.end(), unary_f);
		REQUIRE(equal(squared, built));
	}
	SECTION("binary dynamic build") {
		auto built = kblib::build<std::vector<int>>(input.begin(), input.end(),
		                                            input2.begin(), binary_f);
		REQUIRE(equal(pminusone, built));
	}
	SECTION("unary array build") {
		auto built = kblib::build<arr>(input.begin(), input.end(), unary_f);
		REQUIRE(equal(squared, built));
	}
	SECTION("binary array build") {
		auto built = kblib::build<arr>(input.begin(), input.end(), input2.begin(),
		                               binary_f);
		REQUIRE(equal(pminusone, built));
	}
	const arr iota = {0, 1, 2, 3, 4, 5, 6, 7};
	SECTION("dynamic generator build") {
		auto built =
		    kblib::build<std::vector<int>>([x = 0]() mutable { return x++; }, 8);
		REQUIRE(equal(iota, built));
	}
	SECTION("array generator build") {
		auto built = kblib::build<arr>([x = 0]() mutable { return x++; });
		REQUIRE(equal(iota, built));
	}
	SECTION("dynamic buildiota") {
		auto built = kblib::buildiota<std::vector<int>>(8, 0);
		REQUIRE(equal(iota, built));
	}
	SECTION("array buildiota") {
		auto built = kblib::buildiota<arr>(0);
		REQUIRE(equal(iota, built));
	}
}

TEST_CASE("build_copy family") {
	// TODO
}

TEST_CASE("build.h iterators") {
	// TODO
}
