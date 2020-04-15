#include "kblib/build.h"
#include "catch.hpp"
#include <array>
#include <iostream>

#include "kblib/containers.h"

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
	[[gnu::unused]] auto print_arr = [&](auto c) {
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

TEST_CASE("buildiota") {
	auto equal = [](auto r1, auto r2) {
		auto r1b = r1.begin();
		auto r1e = r1.end();
		auto r2b = r2.begin();
		auto r2e = r2.end();
		return (std::distance(r1b, r1e) == std::distance(r2b, r2e)) &&
		       kblib::equal(r1b, r1e, r2b);
	};

	constexpr auto target = std::array<int, 10>{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}};
	auto i1 = kblib::buildiota<std::vector<int>>(10, 0);
	auto i2 = kblib::buildiota<std::vector<int>>(10, 0, 1);
	auto i3 = kblib::buildiota<std::array<int, 10>>(0);
	auto i4 = kblib::buildiota<std::array<int, 10>>(0, 1);
	auto i5 =
	    kblib::buildiota<kblib::construct_with_size<std::vector<int>, 10>>(0);

	REQUIRE((i1.size() == target.size() && equal(i1, target)));
	REQUIRE((i2.size() == target.size() && equal(i2, target)));
	REQUIRE((i3.size() == target.size() && equal(i3, target)));
	REQUIRE((i4.size() == target.size() && equal(i4, target)));
	REQUIRE((i5.size() == target.size() && equal(i5, target)));
}

TEST_CASE("build_copy family") {
	// TODO
}

TEST_CASE("build.h iterators") {
	// TODO
}
