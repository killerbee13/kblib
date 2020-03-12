#include "kblib/memory.h"
#include "catch.hpp"

TEST_CASE("live_ptr") {
	auto obj = kblib::to_unique(new kblib::live_wrapper<int>{0});
	auto ptr = obj->ref();
	REQUIRE(ptr);
	REQUIRE(ptr == &obj->data);

	auto ptr2 = ptr;
	REQUIRE(ptr == ptr2);

	obj = nullptr;
	REQUIRE(!ptr);
	REQUIRE(ptr == ptr2);
}
