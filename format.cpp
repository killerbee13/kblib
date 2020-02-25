#include "kblib/format.h"
#include "catch.hpp"

TEST_CASE("digitsOf") {
	REQUIRE(kblib::digitsOf(0) == 1);
	REQUIRE(kblib::digitsOf(1) == 1);
	REQUIRE(kblib::digitsOf(10) == 2);
	REQUIRE(kblib::digitsOf(99) == 2);
	REQUIRE(kblib::digitsOf(-1) == 2);

	REQUIRE(kblib::digitsOf(1.0) == 1);
	REQUIRE(kblib::digitsOf(10.0) == 2);
	REQUIRE(kblib::digitsOf(10.1) == 2);
}
