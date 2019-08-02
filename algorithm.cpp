#include "catch.hpp"
#include "kblib/algorithm.h"

#include <vector>
#include <string>

TEST_CASE("erase") {
	const auto equal = [](auto a, auto b) {
		return std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b));
	};
	SECTION("erase and erase_if") {
		const std::vector<int> erase_test{2, 2, 3, 4, 5, 7, 8, 11};
		const std::vector<int> no_2s = {3, 4, 5, 7, 8, 11};
		auto erase_copy = erase_test;
		kblib::erase(erase_copy, 2);
		REQUIRE(equal(no_2s, erase_copy));
		erase_copy = erase_test;
		const std::vector<int> no_evens = {3, 5, 7, 11};
		kblib::erase_if(erase_copy, [](int x) { return (~x) & 1; });
		REQUIRE(equal(no_evens, erase_copy));
	}
}

TEST_CASE("find family") {
	// TODO
}

TEST_CASE("get_max family") {
	// TODO
}

TEST_CASE("general algorithms") {
	// TODO
}

TEST_CASE("zip") {
	SECTION("non-overlapping") {
		std::vector<int> input1 {1, 2, 3, 4, 5, 6};
		std::vector<short> input2 {2, 3, 4, 5, 6, 7};

		for (auto t : kblib::zip(input1.begin(), input1.end(), input2.begin())) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			REQUIRE(a + 1 == b);
		}
	}
	SECTION("identical") {
		std::vector<int> input {1, 2, 3, 4, 5, 6, 7, 8};
		for (auto t : kblib::zip(input.begin(), input.end(), input.begin())) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			REQUIRE(&a == &b);
		}
	}
	SECTION("overlapping") {
		std::vector<int> input {1, 2, 3, 4, 5, 6, 7, 8};
		for (auto t : kblib::zip(input.begin(), input.end() - 1, input.begin() + 1)) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			REQUIRE(a + 1 == b);
		}
	}
}
