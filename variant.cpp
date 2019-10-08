#include "catch.hpp"
#include <iostream>

#include "kblib/variant.h"

#if KBLIB_USE_CXX17

TEST_CASE("visit") {
	std::variant<std::monostate, int, std::string> var = 10;

	// standard syntax (with visitor helper):
	std::visit(kblib::visitor{[](std::monostate) { REQUIRE(false); },
	                          [](int) { REQUIRE(true); },
	                          [](const std::string&) { REQUIRE(false); }},
	           var);

	// Basic single-variant syntax:
	kblib::visit(var, [](std::monostate) { REQUIRE(false); },
	             [](int) { REQUIRE(true); },
	             [](const std::string&) { REQUIRE(false); });

	// Pattern-matching-like syntax:
	kblib::visit(var)
	      ([](std::monostate) { REQUIRE(false); },
	       [](int) { REQUIRE(true); },
	       [](const std::string&) { REQUIRE(false); });
}

struct good_base {
	good_base() = default;
	virtual ~good_base() noexcept { std::cout << "~good_base\n"; }

	virtual void bark() const { std::cout << "good_base\n"; }
};
struct good_derived : good_base {
	virtual ~good_derived() noexcept { std::cout << "~good_derived\n"; }

	void bark() const override { std::cout << "good_derived\n"; }
};
struct unrelated {};
struct bad_nocopy : good_base {
	bad_nocopy() = default;
	bad_nocopy(const bad_nocopy&) = delete;
};

struct bad_base1 {
	virtual void bark() const { std::cout << "bad_base1\n"; }
	~bad_base1() noexcept { std::cout << "~bad_base1\n"; }
};
struct bad_derived1 : bad_base1 {
	virtual void bark() const { std::cout << "bad_derived1\n"; }
	~bad_derived1() noexcept { std::cout << "~bad_derived1\n"; }
};

struct bad_base2 {
	bad_base2() = default;
	virtual ~bad_base2() noexcept { std::cout << "~bad_base2\n"; }
	virtual void bark() const { std::cout << "bad_base2\n"; }
};
struct bad_derived2 : protected bad_base2 {
	virtual void bark() const { std::cout << "bad_derived2\n"; }
	virtual ~bad_derived2() noexcept { std::cout << "~bad_derived2\n"; }
};
struct small_base {
	small_base() = default;
	virtual ~small_base() noexcept = default;
	virtual void bark() const { std::cout << "small_base\n"; }
	virtual int id() const { return 0; }
};
struct big_derived : small_base {
	std::size_t x =
	    (x = 1,
	     kblib::FNVa_a<std::size_t>(
	         reinterpret_cast<const char (&)[sizeof(big_derived)]>(*this)));
	void bark() const override {
		std::cout << "big_derived " << std::hex << x << "\n";
	}
	virtual ~big_derived() noexcept = default;
	int id() const override { return 1; }
};

struct not_copyable {
	not_copyable() = default;
	not_copyable(const not_copyable&) = delete;
	not_copyable(not_copyable&&) = default;
	virtual ~not_copyable() = default;
};

struct copyable_derived : not_copyable {
	copyable_derived() = default;
	copyable_derived(const copyable_derived&) : not_copyable() {}
};

struct copyable_base {
	virtual ~copyable_base() = default;
};

struct noncopyable_derived : copyable_base {
	noncopyable_derived() = default;
	noncopyable_derived(const noncopyable_derived&) = delete;
	noncopyable_derived(noncopyable_derived&&) = default;
};

TEST_CASE("poly_obj") {
	SECTION("basic") {
		kblib::poly_obj<good_base> o1, o2{std::in_place};
		REQUIRE(!o1.has_value());
		REQUIRE(o2.has_value());
		o2->bark(); // good_base
		o1 =
		    kblib::poly_obj<good_base>::make<good_derived>(); // temporary deleted
		REQUIRE(o1.has_value());
		o1->bark(); // good_derived
		o2 = o1;    // o2 ~good_base
		o2->bark(); // good_derived
		o1.clear(); //~good_base
		REQUIRE(!o1.has_value());
		// kblib::poly_obj<good_base> o3 =
		// kblib::poly_obj<good_base>::make<bad_nocopy>();
	}

	SECTION("non-virtual destructor") {
		// Warning issued because of non-virtual destructor call
		kblib::poly_obj<bad_base1> o3, o4{std::in_place};
		// unsafe - no virtual destructor
		// o3 = kblib::poly_obj<bad_base1>::make<bad_derived1>();
	}

	SECTION("private base") {
		kblib::poly_obj<bad_base2> o5, o6{std::in_place};
		// illegal - non-public base
		// o5 = kblib::poly_obj<bad_base2>::make<bad_derived2>();
	}

	SECTION("capacity") {
		// illegal - unrelated types
		// kblib::poly_obj<good_base>::make<unrelated>();
		// illegal - derived too big
		// kblib::poly_obj<bad_base3>::make<bad_derived3>();
		// fine
		kblib::poly_obj<small_base, sizeof(big_derived)> o7, o8{std::in_place};
		o8->bark();
		o7 =
		    kblib::poly_obj<small_base, sizeof(big_derived)>::make<big_derived>();
		REQUIRE(o7.has_value());
		o7->bark();
	}

	SECTION("non-copyable type") {
		// A poly_obj of non-copyable type is allowed and non-copyable
		kblib::poly_obj<not_copyable> o1{std::in_place}, o2;
		// illegal
		// o2 = o1;
		// legal
		o2 = std::move(o1);
		// legal
		auto o3 = kblib::poly_obj<not_copyable>::make<copyable_derived>();
		// illegal (can't copy even if derived is copyable)
		// o1 = o3;
	}

	SECTION("non-copyable derived") {
		kblib::poly_obj<copyable_base, sizeof(copyable_base), kblib::move_only_t>
		    o1{std::in_place};
		// Valid because the template parameters explicitly disable copying, so
		// derived classes don't need to be copyable.
		auto o2 = decltype(o1)::make<noncopyable_derived>();
	}

	SECTION("mixed vector") {
		auto r = [n = static_cast<unsigned short>(16127)]() mutable {
			n ^= 19937;
			n *= 4889;
			return kblib::FNV32a({reinterpret_cast<char*>(&n), sizeof(n)});
		};

		using obj = kblib::poly_obj<small_base, sizeof(big_derived)>;

		std::vector<obj> polyvec;
		int g[2] = {};
		std::generate_n(std::back_inserter(polyvec), 128, [&] {
			if ((r() % 2)) {
				++g[1];
				return obj::make<big_derived>();
			} else {
				++g[0];
				return obj(std::in_place);
			}
		});
		std::cout << std::dec << "\ng: " << g[0] << ' ' << g[1] << '\n';

		int c[2] = {};
		char ab[] = "ab";
		for (const auto& o : polyvec) {
			++c[o->id()];
			std::cout << ab[o->id()];
		}
		std::cout << "\nc: " << c[0] << ' ' << c[1] << '\n';
	}

	SECTION("free make function") {
		// Automatically takes the larger capacity value, but returns a base
		// pointer
		auto o = kblib::make_poly_obj<small_base, big_derived>();
	}
}

#endif
