#include "catch.hpp"
#include "kblib/convert.h"

TEST_CASE("bases") {
	REQUIRE(kblib::to_string<62>(0) == "0");
	REQUIRE(kblib::to_string<62>(1) == "1");
	REQUIRE(kblib::to_string<62>(-1) == "-1");
	REQUIRE(kblib::to_string<62>(10) == "A");
	REQUIRE(kblib::to_string<62>(26) == "Q");
	REQUIRE(kblib::to_string<62>(35) == "Z");
	REQUIRE(kblib::to_string<62>(36) == "a");
	REQUIRE(kblib::to_string<62>(61) == "z");
	REQUIRE(kblib::to_string<62>(62) == "10");
	REQUIRE(kblib::to_string<62>(63) == "11");
	REQUIRE(kblib::to_string<62>(630) == "AA");

	REQUIRE(kblib::to_string<2>(65536) == "1""00000000""00000000");
}
