#include "kblib/convert.h"
#include "catch.hpp"

using namespace std::literals;

TEST_CASE("bases") {
	CHECK(kblib::to_string<62>(0) == "0");
	CHECK(kblib::to_string<62>(1) == "1");
	CHECK(kblib::to_string<62>(-1) == "-1");
	CHECK(kblib::to_string<62>(10) == "A");
	CHECK(kblib::to_string<62>(26) == "Q");
	CHECK(kblib::to_string<62>(35) == "Z");
	CHECK(kblib::to_string<62>(36) == "a");
	CHECK(kblib::to_string<62>(61) == "z");
	CHECK(kblib::to_string<62>(62) == "10");
	CHECK(kblib::to_string<62>(63) == "11");
	CHECK(kblib::to_string<62>(630) == "AA");

	CHECK(kblib::to_string<2>(65536) == "10000000000000000");

	using namespace kblib::literals;
	auto one = 1_c;
	static_assert(decltype(one)::value == 1, "");
	static_assert(one == 1, "");
}

TEST_CASE("parse_integer") {
	using namespace Catch::Matchers;

	CHECK_THROWS_MATCHES(kblib::parse_integer<long>(""), std::invalid_argument,
	                     Message("\"\" is not an integer"));
	CHECK_THROWS_MATCHES(kblib::parse_integer<long>(""s), std::invalid_argument,
	                     Message("\"\" is not an integer"));
	CHECK(kblib::parse_integer<long>("0") == 0);
	CHECK(kblib::parse_integer<long>("1") == 1);
	CHECK(kblib::parse_integer<long>("+1") == 1);
	CHECK(kblib::parse_integer<long>("-1") == -1);
	CHECK(kblib::parse_integer<long>("10") == 10);
	CHECK(kblib::parse_integer<long>("010") == 010);
	CHECK(kblib::parse_integer<long>("0x10") == 0x10);
	CHECK(kblib::parse_integer<long>("0b10") == 0b10);
	CHECK(kblib::parse_integer<long>("0b1'0000'0000'0000'0000")
	      == 0b1'0000'0000'0000'0000);
	CHECK(kblib::parse_integer<long>("-10") == -10);
	CHECK(kblib::parse_integer<long>("+10") == 10);
	// octal literals
	CHECK(kblib::parse_integer<long>("-010") == -010);
	CHECK(kblib::parse_integer<long>("+010") == 010);
	CHECK(kblib::parse_integer<long>("-0x10") == -0x10);
	CHECK(kblib::parse_integer<long>("+0x10") == 0x10);
	CHECK(kblib::parse_integer<long>("-0b10") == -0b10);
	CHECK(kblib::parse_integer<long>("+0b10") == 0b10);
	CHECK(kblib::parse_integer<long>("-0b1'0000'0000'0000'0000")
	      == -0b1'0000'0000'0000'0000);
	CHECK(kblib::parse_integer<long>("+0b1'0000'0000'0000'0000")
	      == 0b1'0000'0000'0000'0000);

	CHECK_THROWS_AS(kblib::parse_integer<long>("", 1), std::invalid_argument);
	CHECK_THROWS_MATCHES(
	    kblib::parse_integer<long>("1", 1), std::invalid_argument,
	    Message("base must be either 0 or a positive number between 2 and 62"));
	CHECK_THROWS_AS(kblib::parse_integer<long>("1", -1), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("1", 63), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("1", 100), std::invalid_argument);
	CHECK(kblib::parse_integer<long>("0", 10) == 0);
	CHECK(kblib::parse_integer<long>("1'0000'0000'0000'0000", 2) == 65536);

	CHECK_THROWS_MATCHES(kblib::parse_integer<long>("2135agfd"),
	                     std::invalid_argument,
	                     Message("invalid character in integer"));
	CHECK_THROWS_MATCHES(kblib::parse_integer<long>("0-1"),
	                     std::invalid_argument,
	                     Message("unexpected - in integer"));
	CHECK_THROWS_MATCHES(kblib::parse_integer<long>("0-"), std::invalid_argument,
	                     Message("unexpected - in integer"));
	CHECK_THROWS_MATCHES(kblib::parse_integer<long>("0x-1"),
	                     std::invalid_argument,
	                     Message("unexpected - in integer"));
	CHECK_THROWS_MATCHES(kblib::parse_integer<long>("0b-1"),
	                     std::invalid_argument,
	                     Message("unexpected - in integer"));
	CHECK_THROWS_AS(kblib::parse_integer<long>("0+"), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("0+1"), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("0x+1"), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("0b+1"), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("--1"), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("-+1"), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("+-1"), std::invalid_argument);
	CHECK_THROWS_AS(kblib::parse_integer<long>("++1"), std::invalid_argument);

	// signed min: (NYI)
	CHECK(kblib::parse_integer<std::int16_t>("-32768") == -32768);
	CHECK(kblib::parse_integer<std::int32_t>("-0x80'00'00'00")
	      == static_cast<std::int32_t>(-0x80'00'00'00));
}

TEST_CASE("fromStr") {
	using namespace std::literals;
	const std::string str = "100";
	CHECK(kblib::fromStr<std::string>(str) == str);
	CHECK(kblib::fromStr<int>(str) == 100);
	CHECK(kblib::fromStr<bool>("1") == true);
	CHECK(kblib::fromStr<bool>("true") == true);
	CHECK(kblib::fromStr<bool>("0") == false);
	CHECK(kblib::fromStr<bool>("false") == false);

#if KBLIB_USE_STRING_VIEW
	const std::string_view strv = str;
	CHECK(kblib::fromStr<std::string>(strv) == strv);
	CHECK(kblib::fromStr<int>(strv) == 100);
	CHECK(kblib::fromStr<bool>("1"sv) == true);
	CHECK(kblib::fromStr<bool>("true"sv) == true);
	CHECK(kblib::fromStr<bool>("0"sv) == false);
	CHECK(kblib::fromStr<bool>("false"sv) == false);
#endif
}
