#include "catch.hpp"
#include "kblib/algorithm.h"

#include <vector>
#include <string>

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
