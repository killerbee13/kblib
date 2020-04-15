#include "kblib/stringops.h"
#include "catch.hpp"

using namespace std::string_literals;

#if KBLIB_USE_CXX17
TEST_CASE("concat") {
	REQUIRE(kblib::concat(1, 2, 3, 4) == "1234");
	REQUIRE(kblib::concat('a', 'b', 'c') == "abc");
	REQUIRE(kblib::concat("ab", "cd", 12, 3, 4) == "abcd1234");
	static_assert(std::is_same_v<std::string, kblib::detail::str_type_t<double>>,
	              "arithmetic types are converted to std::string");
	REQUIRE(kblib::concat(1.0) == "1.000000");
	REQUIRE(kblib::concat(1.5) == "1.500000");
	REQUIRE(kblib::concat(0.0) == "0.000000");
}

TEST_CASE("join and split") {
	std::vector<std::string> vec{"Hello", "world!", "How", "do", "you", "do?"};
	auto joined = kblib::join(vec, " "s);
	REQUIRE(joined == "Hello world! How do you do?");
	auto split = kblib::split(joined);
	REQUIRE(split == vec);
}
#endif
