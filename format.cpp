#include "kblib/format.h"
#include "catch.hpp"

TEST_CASE("count_digits") {
	REQUIRE(kblib::count_digits(0) == 1);
	REQUIRE(kblib::count_digits(1) == 1);
	REQUIRE(kblib::count_digits(10) == 2);
	REQUIRE(kblib::count_digits(99) == 2);
	REQUIRE(kblib::count_digits(-1) == 2);

	REQUIRE(kblib::count_digits(1.0) == 1);
	REQUIRE(kblib::count_digits(10.0) == 2);
	REQUIRE(kblib::count_digits(10.1) == 2);
}
