#include "catch.hpp"
#include <chrono>
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
	kblib::visit(var)([](std::monostate) { REQUIRE(false); },
	                  [](int) { REQUIRE(true); },
	                  [](const std::string&) { REQUIRE(false); });
}

TEST_CASE("visit_indexed") {
	std::variant<std::monostate, int, std::string> var(std::in_place_type<int>,
	                                                   10);
	kblib::visit_indexed(var, [](auto constant, auto val) {
		REQUIRE(constant == 1);
		static_assert(
		    std::is_same_v<decltype(val),
		                   std::variant_alternative_t<constant, decltype(var)>>);
	});
}

TEST_CASE("variant_cast") {
	std::variant<int, std::string> from(std::in_place_type<int>, 10);
	auto to =
	    kblib::variant_cast<std::variant<std::monostate, int, std::string>>(
	        from);
	REQUIRE(to.index() == 1);
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
		// static_assertion fails because of non-virtual destructor
		// kblib::poly_obj<bad_base1> o3, o4{std::in_place};
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

struct Base {
	virtual unsigned operator()() const noexcept = 0;
	Base() = default;
	Base(const Base&) {}
	virtual ~Base() = default;
};

struct Derived1 final : Base {
	unsigned operator()() const noexcept override { return member; }
	unsigned member;
	Derived1(unsigned i) : member(i) {}
};

struct Derived2 final : Base {
	unsigned operator()() const noexcept override { return member * 2; }
	unsigned member;
	Derived2(unsigned i) : member(i) {}
};

struct Derived3 final : Base {
	unsigned operator()() const noexcept override { return member / 2; }
	unsigned member;
	Derived3(unsigned i) : member(i) {}
};

struct Derived4 final : Base {
	unsigned operator()() const noexcept override { return ~member; }
	unsigned member;
	Derived4(unsigned i) : member(i) {}
};

TEST_CASE("poly_obj performance") {
	const unsigned count = 1000000;

	/*
	 * Averages of 5 runes (Release / Debug):
	 *
	 * baseline (non-virtual):	  4 /  35 ns
	 * std::unique_ptr:       	 12 /  77 ns
	 * std::function:         	 12 / 102 ns
	 * std::variant (visit):  	 32 / 246 ns
	 * std::variant (get_if): 	  9 / 169 ns
	 * std::variant (switch): 	  9 / 158 ns
	 * kblib::poly_obj:       	  8 /  91 ns
	 *
	 * Conclusions:
	 *
	 * std::visit is extremely slow for some reason. std::variant is slow in
	 * general in debug. std::unique_ptr is faster in debug than kblib::poly_obj,
	 * but kblib::poly_obj optimizes better than it, most likely due to the
	 * data locality.
	 *
	 */

	SECTION("baseline") {
		std::vector<Derived1> d1;
		std::vector<Derived2> d2;
		std::vector<Derived3> d3;
		std::vector<Derived4> d4;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d1.push_back(v);
				break;
			case 1:
				d2.push_back(v);
				break;
			case 2:
				d3.push_back(v);
				break;
			case 3:
				d4.push_back(v);
			}
		}

		const auto start = std::chrono::steady_clock::now();
		unsigned accum{};
		for (const auto& x : d1) {
			accum += x();
		}
		for (const auto& x : d2) {
			accum += x();
		}
		for (const auto& x : d3) {
			accum += x();
		}
		for (const auto& x : d4) {
			accum += x();
		}
		const auto end = std::chrono::steady_clock::now();
		auto time = (end - start) / count;
		std::cout << "baseline (non-virtual):\t" << time.count() << " ns\n";
	}
	SECTION("unique pointer") {
		std::vector<std::unique_ptr<Base>> d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d.push_back(std::make_unique<Derived1>(v));
				break;
			case 1:
				d.push_back(std::make_unique<Derived2>(v));
				break;
			case 2:
				d.push_back(std::make_unique<Derived3>(v));
				break;
			case 3:
				d.push_back(std::make_unique<Derived4>(v));
			}
		}

		const auto start = std::chrono::steady_clock::now();
		unsigned accum{};
		for (const auto& x : d) {
			accum += (*x)();
		}
		const auto end = std::chrono::steady_clock::now();
		auto time = (end - start) / count;
		std::cout << "unique pointer:        \t" << time.count() << " ns\n";
	}
	SECTION("std::function") {
		std::vector<std::function<unsigned()>> d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d.push_back(Derived1(v));
				break;
			case 1:
				d.push_back(Derived2(v));
				break;
			case 2:
				d.push_back(Derived3(v));
				break;
			case 3:
				d.push_back(Derived4(v));
			}
		}

		const auto start = std::chrono::steady_clock::now();
		unsigned accum{};
		for (const auto& x : d) {
			accum += x();
		}
		const auto end = std::chrono::steady_clock::now();
		auto time = (end - start) / count;
		std::cout << "std::function:         \t" << time.count() << " ns\n";
	}
	SECTION("std::variant") {
		std::vector<std::variant<Derived1, Derived2, Derived3, Derived4>> d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d.push_back(Derived1(v));
				break;
			case 1:
				d.push_back(Derived2(v));
				break;
			case 2:
				d.push_back(Derived3(v));
				break;
			case 3:
				d.push_back(Derived4(v));
			}
		}
		{
			const auto start = std::chrono::steady_clock::now();
			unsigned accum{};
			for (const auto& x : d) {
				accum += std::visit([](const auto& v) { return v(); }, x);
			}
			const auto end = std::chrono::steady_clock::now();
			auto time = (end - start) / count;
			std::cout << "std::variant (visit):  \t" << time.count() << " ns\n";
		}
		{
			const auto start = std::chrono::steady_clock::now();
			unsigned accum{};
			for (const auto& x : d) {
				if (auto* p = std::get_if<0>(&x)) {
					accum += (*p)();
				} else if (auto* p = std::get_if<1>(&x)) {
					accum += (*p)();
				} else if (auto* p = std::get_if<2>(&x)) {
					accum += (*p)();
				} else if (auto* p = std::get_if<3>(&x)) {
					accum += (*p)();
				}
			}
			const auto end = std::chrono::steady_clock::now();
			auto time = (end - start) / count;
			std::cout << "std::variant (get_if): \t" << time.count() << " ns\n";
		}
		{
			const auto start = std::chrono::steady_clock::now();
			unsigned accum{};
			for (const auto& x : d) {
				switch (x.index()) {
				case 0:
					accum += std::get<0>(x)();
					break;
				case 1:
					accum += std::get<1>(x)();
					break;
				case 2:
					accum += std::get<2>(x)();
					break;
				case 3:
					accum += std::get<3>(x)();
				}
			}
			const auto end = std::chrono::steady_clock::now();
			auto time = (end - start) / count;
			std::cout << "std::variant (switch): \t" << time.count() << " ns\n";
		}
	}
	SECTION("poly_obj") {
		using poly_t = kblib::poly_obj<Base, sizeof(Derived1), int>;
		std::vector<poly_t> d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d.push_back(poly_t::make<Derived1>(v));
				break;
			case 1:
				d.push_back(poly_t::make<Derived2>(v));
				break;
			case 2:
				d.push_back(poly_t::make<Derived3>(v));
				break;
			case 3:
				d.push_back(poly_t::make<Derived4>(v));
			}
		}

		const auto start = std::chrono::steady_clock::now();
		unsigned accum{};
		for (const auto& x : d) {
			accum += x();
		}
		const auto end = std::chrono::steady_clock::now();
		auto time = (end - start) / count;
		std::cout << "kblib::poly_obj:       \t" << time.count() << " ns\n";
	}
	// std::cout<<denom_of<std::chrono::steady_clock::period><<'\n';
}

#endif
