#include "kblib/memory.h"

#include "catch.hpp"
#include "kblib/fakestd.h"

TEST_CASE("live_ptr<int>") {
	// default-initialized live_ptrs are null
	kblib::live_ptr<int> dptr;
	REQUIRE(!dptr);
	// can be compared to nullptr
	REQUIRE(dptr == nullptr);

	// obtain a live_ptr to an object
	auto obj = kblib::to_unique(new kblib::live_wrapper<int>{0});
	auto ptr = obj->ref();
	REQUIRE(ptr);
	REQUIRE(ptr != dptr);

	// access through the pointer
	REQUIRE(obj->data == *ptr);
	++*ptr;
	REQUIRE(obj->data == *ptr);

	// can be compared with a raw pointer
	REQUIRE(ptr == &obj->data);

	// copy the pointer
	auto ptr2 = ptr;
	REQUIRE(ptr == ptr2);

	// move the pointer
	auto ptr3 = std::move(ptr2);
	REQUIRE(ptr3 == ptr);
	REQUIRE(!ptr2);

	// reassign a pointer
	ptr2 = ptr3;

	// implicitly add const
	kblib::live_ptr<const int> cptr = ptr;
	REQUIRE(cptr == ptr);

	auto cptr2 = cptr;
	REQUIRE(cptr2 == ptr);
	auto cptr3 = std::move(cptr2);
	REQUIRE(!cptr2);

	// destroy the referenced object
	obj.reset();

	// all pointers to it are now null
	REQUIRE(!ptr);
	REQUIRE(ptr == ptr3);
	REQUIRE(ptr == dptr);
	REQUIRE(!cptr);
}

TEST_CASE("live_ptr<string>") {
	// default-initialized live_ptrs are null
	kblib::live_ptr<std::string> dptr;
	REQUIRE(!dptr);
	// can be compared to nullptr
	REQUIRE(dptr == nullptr);

	// obtain a live_ptr to an object
	auto obj = kblib::to_unique(new kblib::live_wrapper<std::string>{});
	auto ptr = obj->ref();
	REQUIRE(ptr);
	REQUIRE(ptr != dptr);

	// access through the pointer
	REQUIRE(obj->data == *ptr);
	REQUIRE(ptr->length() == 0);
	*ptr = "test";
	REQUIRE(obj->data == *ptr);
	REQUIRE(ptr->length() == 4);

	// can be compared with a raw pointer
	REQUIRE(ptr == &obj->data);

	// copy the pointer
	auto ptr2 = ptr;
	REQUIRE(ptr == ptr2);

	// move the pointer
	auto ptr3 = std::move(ptr2);
	REQUIRE(ptr3 == ptr);
	REQUIRE(!ptr2);

	// reassign a pointer
	ptr2 = ptr3;

	// implicitly add const
	kblib::live_ptr<const std::string> cptr = ptr;
	REQUIRE(cptr == ptr);

	auto cptr2 = cptr;
	REQUIRE(cptr2 == ptr);
	auto cptr3 = std::move(cptr2);
	REQUIRE(!cptr2);

	// destroy the referenced object
	obj.reset();

	// all pointers to it are now null
	REQUIRE(!ptr);
	REQUIRE(ptr == ptr3);
	REQUIRE(ptr == dptr);
	REQUIRE(!cptr);
}
