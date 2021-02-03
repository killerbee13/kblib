#include "kblib/memory.h"

#include "catch.hpp"
#include "kblib/fakestd.h"

TEST_CASE("live_ptr<int>") {
	// default-initialized live_ptrs are null
	kblib::live_ptr<int> dptr;
	REQUIRE(not dptr);
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
	REQUIRE(not ptr2);

	// reassign a pointer
	ptr2 = ptr3;

	// implicitly add const
	kblib::live_ptr<const int> cptr = ptr;
	REQUIRE(cptr == ptr);

	auto cptr2 = cptr;
	REQUIRE(cptr2 == ptr);
	auto cptr3 = std::move(cptr2);
	REQUIRE(not cptr2);

	// destroy the referenced object
	obj.reset();

	// all pointers to it are now null
	REQUIRE(not ptr);
	REQUIRE(ptr == ptr3);
	REQUIRE(ptr == dptr);
	REQUIRE(not cptr);
}

TEST_CASE("live_ptr<string>") {
	// default-initialized live_ptrs are null
	kblib::live_ptr<std::string> dptr;
	REQUIRE(not dptr);
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
	REQUIRE(not ptr2);

	// reassign a pointer
	ptr2 = ptr3;

	// implicitly add const
	kblib::live_ptr<const std::string> cptr = ptr;
	REQUIRE(cptr == ptr);

	auto cptr2 = cptr;
	REQUIRE(cptr2 == ptr);
	auto cptr3 = std::move(cptr2);
	REQUIRE(not cptr2);

	// destroy the referenced object
	obj.reset();

	// all pointers to it are now null
	REQUIRE(not ptr);
	REQUIRE(ptr == ptr3);
	REQUIRE(ptr == dptr);
	REQUIRE(not cptr);
}

TEST_CASE("cond_ptr") {
	int a{42};
	auto op = kblib::make_cond_ptr(std::make_unique<int>(42));
	auto rp = kblib::make_cond_ptr(&a);
	REQUIRE(op);
	REQUIRE(rp);
	REQUIRE(*op == *rp);
	REQUIRE(op.owns());
	REQUIRE_FALSE(rp.owns());
	auto cp = op.weak();
	REQUIRE(cp);
	REQUIRE(cp == op);
	REQUIRE(op.owns());
	REQUIRE_FALSE(cp.owns());
	auto up = std::unique_ptr<int>(std::move(rp));
	REQUIRE_FALSE(up);
	up = std::move(op).to_unique();
	REQUIRE(up == cp);
	REQUIRE(up);
	op.reset(up.release(), true);
	REQUIRE(cp == op);
	rp = std::move(up);
	REQUIRE_FALSE(rp);
	REQUIRE_FALSE(rp.owns());
}

TEST_CASE("cond_ptr fptr") {
	auto del = [](int* p) noexcept { delete p; };
	int a{42};
	auto op = kblib::cond_ptr<int, decltype(+del)>(
	    std::unique_ptr<int, decltype(+del)>(new int{42}, del));
	auto rp = kblib::cond_ptr<int, decltype(+del)>(&a, del);
	REQUIRE(op);
	REQUIRE(rp);
	REQUIRE(*op == *rp);
	REQUIRE(op.owns());
	REQUIRE_FALSE(rp.owns());
	auto cp = op.weak();
	REQUIRE(cp);
	REQUIRE(cp == op);
	REQUIRE(op.owns());
	REQUIRE_FALSE(cp.owns());
	auto up = std::unique_ptr<int, decltype(+del)>(std::move(rp));
	REQUIRE_FALSE(up);
	up = std::move(op).to_unique();
	REQUIRE(up == cp);
	REQUIRE(up);
	op.reset(up.release(), true, up.get_deleter());
	REQUIRE(cp == op);
	rp = std::move(up);
	REQUIRE_FALSE(rp);
	REQUIRE_FALSE(rp.owns());
}

TEST_CASE("cond_ptr rfptr") {
	auto del = [](int* p) noexcept { delete p; };
	int a{42};
	auto op = kblib::cond_ptr<int, decltype(*+del)>(
	    std::unique_ptr<int, decltype(*+del)>(new int{42}, *del));
	auto rp = kblib::cond_ptr<int, decltype(*+del)>(&a, *del);
	REQUIRE(op);
	REQUIRE(rp);
	REQUIRE(*op == *rp);
	REQUIRE(op.owns());
	REQUIRE_FALSE(rp.owns());
	auto cp = op.weak();
	REQUIRE(cp);
	REQUIRE(cp == op);
	REQUIRE(op.owns());
	REQUIRE_FALSE(cp.owns());
	auto up = std::unique_ptr<int, decltype(*+del)>(std::move(rp));
	REQUIRE_FALSE(up);
}

TEST_CASE("cond_ptr array") {
	int a[10]{42};
	auto op = kblib::make_cond_ptr(std::make_unique<int[]>(42));
	auto rp = kblib::cond_ptr<int[]>(a);
	REQUIRE(op);
	REQUIRE(rp);
	REQUIRE_FALSE(op == rp);
	REQUIRE(op.owns());
	REQUIRE_FALSE(rp.owns());
	auto cp = op.weak();
	REQUIRE(cp);
	REQUIRE(cp == op);
	REQUIRE(op.owns());
	REQUIRE_FALSE(cp.owns());
	auto up = std::unique_ptr<int[]>(std::move(rp));
	REQUIRE_FALSE(up);
	up = std::move(op).to_unique();
	REQUIRE(up == cp);
	REQUIRE(up);
	op.reset(up.release(), true);
	REQUIRE(cp == op);
	rp = std::move(up);
	REQUIRE_FALSE(rp);
	REQUIRE_FALSE(rp.owns());
}
