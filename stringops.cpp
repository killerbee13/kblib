#include "kblib/stringops.h"
#include "catch.hpp"

using namespace std::string_literals;

#if KBLIB_USE_CXX17
TEST_CASE("join and split") {
	std::vector<std::string> vec{"Hello", "world!", "How", "do", "you", "do?"};
	auto joined = kblib::join(vec, " "s);
	REQUIRE(joined == "Hello world! How do you do?");
	auto split = kblib::split(joined);
	REQUIRE(split == vec);
}
#endif
