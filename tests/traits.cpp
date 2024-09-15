#include "kblib/traits.h"
#include "catch2/catch.hpp"

struct k {
	char c[10];
};

static_assert(not std::is_reference<kblib::member_of_t<decltype(&k::c)>>::value,
              "");
static_assert(kblib::is_iterable<int[10]>::value, "");

static void f(int i, std::string s) {}

TEST_CASE("type_constant") {
	auto fw = kblib::type_constant<decltype(f)*, f>{};
	int i{};
	std::string s;
	f(i, s);
	fw(i, s);
}
