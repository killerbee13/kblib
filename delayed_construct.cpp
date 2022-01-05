#include "kblib/delayed_construct.h"
#include "catch.hpp"

#if KBLIB_USE_CXX17

TEST_CASE("delayed_construct") {
	kblib::delayed_construct<int> a(0), b(0);
	REQUIRE(a == b);
	REQUIRE(not (a != b));
	REQUIRE(not (a < b));
	REQUIRE(a <= b);
	REQUIRE(not (a > b));
	REQUIRE(a >= b);
}

class no_hash {};

static_assert(
    std::is_default_constructible_v<std::hash<kblib::delayed_construct<int>>>,
    "hashing a delayed_construct<T> is possible if hashing T is possible");
static_assert(not std::is_default_constructible_v<
                  std::hash<kblib::delayed_construct<no_hash>>>,
              "hashing a delayed_construct<T> is not possible if hashing T is "
              "not possible");

#endif
