#include "kblib/random.h"
#include "catch.hpp"

template <std::size_t>
struct print;

TEST_CASE("seed mt19937") {
	auto gen = kblib::seed_with<std::mt19937>(std::random_device{});
	static_assert(kblib::state_size_v<std::mt19937> == 624);
	static_assert(kblib::state_size_v<std::mt19937_64> == 624);
	static_assert(kblib::state_size_v<std::ranlux24> == 1);
}
