#include "kblib/stringops.h"
#include "catch.hpp"

using namespace std::literals;

#if KBLIB_USE_CXX17
TEST_CASE("concat") {
	REQUIRE(kblib::concat("a") == "a");
	REQUIRE(kblib::concat(1) == "1");
	REQUIRE(kblib::concat(1, "a") == "1a");
	REQUIRE(kblib::concat("a"sv) == "a");
	REQUIRE(kblib::concat(1, "a"sv) == "1a");
	REQUIRE(kblib::concat(1, 2, 3, 4) == "1234");
	REQUIRE(kblib::concat('a', 'b', 'c') == "abc");
	REQUIRE(kblib::concat("a", 'b', 'c') == "abc");
	REQUIRE(kblib::concat('a', "b", 'c') == "abc");
	REQUIRE(kblib::concat("a", "b", "c") == "abc");
	REQUIRE(kblib::concat("a"sv, 'b', 'c') == "abc");
	REQUIRE(kblib::concat("a", "b"sv, 'c') == "abc");
	REQUIRE(kblib::concat("a"sv, "b", 'c') == "abc");
	REQUIRE(kblib::concat("a", "b"sv, "c") == "abc");
	REQUIRE(kblib::concat("a"sv, "b"sv, 'c') == "abc");
	REQUIRE(kblib::concat("a"sv, "b"sv, "c") == "abc");
	REQUIRE(kblib::concat("ab", "cd", 12, 3, 4) == "abcd1234");
	static_assert(std::is_same_v<std::string, kblib::detail::str_type_t<double>>,
	              "arithmetic types are converted to std::string");
	REQUIRE(kblib::concat(1.0) == "1.000000");
	REQUIRE(kblib::concat(1.5) == "1.500000");
	REQUIRE(kblib::concat(0.0) == "0.000000");
	{
		std::string val = "1";
		const char* type = "bool";
		REQUIRE(kblib::concat("\"", val, "\" is not a ", type) ==
		        "\"1\" is not a bool");
	}
	{
		std::string_view val = "1";
		const char* type = "bool";
		REQUIRE(kblib::concat("\"", val, "\" is not a ", type) ==
		        "\"1\" is not a bool");
	}
}

TEST_CASE("join and split") {
	std::vector<std::string> vec{"Hello", "world!", "How", "do", "you", "do?"};
	auto joined = kblib::join(vec, " "s);
	REQUIRE(joined == "Hello world! How do you do?");
	auto split = kblib::split(joined);
	REQUIRE(split == vec);
}
#endif
