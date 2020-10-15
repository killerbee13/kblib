#include "kblib/convert.h"
#include "catch.hpp"

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

	REQUIRE(kblib::to_string<2>(65536) == "1"
	                                      "00000000"
	                                      "00000000");
}

TEST_CASE("fromStr") {
	using namespace std::literals;
	const std::string str = "100";
	REQUIRE(kblib::fromStr<std::string>(str) == str);
	REQUIRE(kblib::fromStr<int>(str) == 100);
	REQUIRE(kblib::fromStr<bool>("1") == true);
	REQUIRE(kblib::fromStr<bool>("true") == true);
	REQUIRE(kblib::fromStr<bool>("0") == false);
	REQUIRE(kblib::fromStr<bool>("false") == false);

	const std::string_view strv = str;
	REQUIRE(kblib::fromStr<std::string>(strv) == strv);
	REQUIRE(kblib::fromStr<int>(strv) == 100);
	REQUIRE(kblib::fromStr<bool>("1"sv) == true);
	REQUIRE(kblib::fromStr<bool>("true"sv) == true);
	REQUIRE(kblib::fromStr<bool>("0"sv) == false);
	REQUIRE(kblib::fromStr<bool>("false"sv) == false);
}
