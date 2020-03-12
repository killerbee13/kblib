#include "kblib/memory.h"
#include "catch.hpp"

TEST_CASE("live_ptr") {
	auto obj = kblib::to_unique(new kblib::live_wrapper<int>{0});
	kblib::live_ptr<int> dptr{};
	REQUIRE(!dptr);
	auto ptr = obj->ref();
	REQUIRE(ptr);
	REQUIRE(ptr != dptr);
	REQUIRE(ptr == &obj->data);

	auto ptr2 = ptr;
	REQUIRE(ptr == ptr2);

	kblib::live_ptr<const int> cptr = ptr;
	REQUIRE(cptr == ptr);

	obj = nullptr;
	REQUIRE(!ptr);
	REQUIRE(ptr == ptr2);
	REQUIRE(ptr == dptr);
}
