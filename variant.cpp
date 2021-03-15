#define CATCH_CONFIG_ENABLE_BENCHMARKING
#include "catch.hpp"
#include <chrono>
#include <iostream>
#include <map>
#include <string_view>

#include "kblib/variant.h"

#include "kblib/hash.h"

//#define FAST_TEST

#if KBLIB_USE_CXX17

TEST_CASE("visit") {
	std::variant<std::monostate, int, std::string> var = 10;

	// standard syntax (with visitor helper):
	std::visit(kblib::visitor{[](std::monostate) { REQUIRE(false); },
	                          [](int) { REQUIRE(true); },
	                          [](const std::string&) { REQUIRE(false); }},
	           var);

	// Basic single-variant syntax:
	kblib::visit(
	    var, [](std::monostate) { REQUIRE(false); }, [](int) { REQUIRE(true); },
	    [](const std::string&) { REQUIRE(false); });

	// Pattern-matching-like syntax:
	kblib::visit(var)([](std::monostate) { REQUIRE(false); },
	                  [](int) { REQUIRE(true); },
	                  [](const std::string&) { REQUIRE(false); });

	//	kblib::visit2(var, [](int) {});
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

namespace {

enum barks {
	vgood_base,
	dgood_base,

	vgood_derived,
	dgood_derived,

	vbad_base1,
	dbad_base1,

	vbad_derived1,
	dbad_derived1,

	vbad_base2,
	dbad_base2,

	vbad_derived2,
	dbad_derived2,

	vsmall_base,
	dsmall_base,

	vbig_derived,
	dbig_derived,
};

std::vector<barks> bark_log;

struct good_base {
	good_base() = default;
	virtual ~good_base() noexcept { bark_log.push_back(dgood_base); }

	virtual void bark() const { bark_log.push_back(vgood_base); }
};
struct good_derived : good_base {
	~good_derived() noexcept override { bark_log.push_back(dgood_derived); }

	void bark() const override { bark_log.push_back(vgood_derived); }
};
struct unrelated {};
struct bad_nocopy : good_base {
	bad_nocopy(const bad_nocopy&) = delete;
};

struct bad_base1 {
	virtual void bark() const { bark_log.push_back(vbad_base1); }
	// non-virtual destructor
	~bad_base1() noexcept { bark_log.push_back(dbad_base1); }
};
struct bad_derived1 : bad_base1 {
	void bark() const override { bark_log.push_back(vbad_derived1); }
	KBLIB_UNUSED ~bad_derived1() noexcept { bark_log.push_back(dbad_derived1); }
};

struct bad_base2 {
	bad_base2() = default;
	virtual ~bad_base2() noexcept { bark_log.push_back(dbad_base2); }
	virtual void bark() const { bark_log.push_back(vbad_base2); }
};
struct bad_derived2 : protected bad_base2 {
	void bark() const override { bark_log.push_back(vbad_derived2); }
	~bad_derived2() noexcept override { bark_log.push_back(dbad_derived2); }
};
struct small_base {
	small_base() = default;
	virtual ~small_base() noexcept = default;
	virtual void bark() const { bark_log.push_back(vsmall_base); }
	virtual int id() const { return 0; }
};
struct big_derived : small_base {
	std::size_t x =
	    (static_cast<void>(x = 1),
	     kblib::FNVa_a<std::size_t>(
	         reinterpret_cast<const char (&)[sizeof(big_derived)]>(*this)));
	void bark() const override { bark_log.push_back(vbig_derived); }
	~big_derived() noexcept override = default;
	int id() const override { return 1; }
};

struct not_copyable {
	not_copyable() = default;
	not_copyable(const not_copyable&) = delete;
	KBLIB_UNUSED not_copyable(not_copyable&&) noexcept = default;
	virtual ~not_copyable() = default;
};

struct copyable_derived : not_copyable {
	KBLIB_UNUSED copyable_derived() = default;
	copyable_derived(const copyable_derived&) {}
	copyable_derived(copyable_derived&&) noexcept = default;
};

struct copyable_base {
	virtual ~copyable_base() = default;
};

struct noncopyable_derived : copyable_base {
	KBLIB_UNUSED noncopyable_derived() = default;
	noncopyable_derived(const noncopyable_derived&) = delete;
	KBLIB_UNUSED noncopyable_derived(noncopyable_derived&&) = default;
};

struct hinted_base {
	constexpr static std::size_t max_derived_size = sizeof(big_derived);
	virtual ~hinted_base() = default;
};

struct hinted_derived : hinted_base {
	int member;
};

template <typename T>
struct print;

} // namespace

TEST_CASE("poly_obj") {
	SECTION("basic") {
		std::vector<barks> expected;
		bark_log.clear();
		{
			kblib::poly_obj<good_base> o1, o2{std::in_place};
			REQUIRE(not o1.has_value());
			REQUIRE(o2.has_value());
			o2->bark(); // good_base
			expected.push_back(vgood_base);
			REQUIRE(bark_log == expected);

			o1 = kblib::poly_obj<good_base>::make<good_derived>(); // temporary
			                                                       // destroyed
			expected.push_back(dgood_derived);
			expected.push_back(dgood_base);
			REQUIRE(bark_log == expected);
			REQUIRE(o1.has_value());

			o1->bark(); // good_derived
			expected.push_back(vgood_derived);
			REQUIRE(bark_log == expected);

			o2 = o1; // o2 ~good_base
			expected.push_back(dgood_base);
			REQUIRE(bark_log == expected);

			o2->bark(); // good_derived
			expected.push_back(vgood_derived);
			REQUIRE(bark_log == expected);

			o1.clear(); //~good_derived, ~good_base
			expected.push_back(dgood_derived);
			expected.push_back(dgood_base);
			REQUIRE(bark_log == expected);
			REQUIRE(not o1.has_value());
			REQUIRE(o2.has_value());
			// kblib::poly_obj<good_base> o3 =
			// kblib::poly_obj<good_base>::make<bad_nocopy>();
		} // o2: ~good_derived, ~good_base

		expected.push_back(dgood_derived);
		expected.push_back(dgood_base);
		REQUIRE(bark_log == expected);
	}

	SECTION("non-virtual destructor") {
		// static_assertion fails because of non-virtual destructor
		// kblib::poly_obj<bad_base1> o3, o4{std::in_place};
		// o3 = kblib::poly_obj<bad_base1>::make<bad_derived1>();
	}

	SECTION("private base") {
		std::vector<barks> expected;
		bark_log.clear();
		{
			kblib::poly_obj<bad_base2> o5, o6{std::in_place};
			// illegal - non-public base
			// o5 = kblib::poly_obj<bad_base2>::make<bad_derived2>();
		}
		expected.push_back(dbad_base2);
		REQUIRE(bark_log == expected);
	}

	SECTION("capacity") {
		std::vector<barks> expected;
		bark_log.clear();

		// illegal - unrelated types
		// kblib::poly_obj<good_base>::make<unrelated>();
		// illegal - derived too big
		// kblib::poly_obj<small_base>::make<big_derived>();
		// fine
		kblib::poly_obj<small_base, sizeof(big_derived)> o7, o8{std::in_place};
		o8->bark();
		expected.push_back(vsmall_base);
		REQUIRE(bark_log == expected);
		o7 =
		    kblib::poly_obj<small_base, sizeof(big_derived)>::make<big_derived>();
		REQUIRE(o7.has_value());
		o7->bark();
		expected.push_back(vbig_derived);
		REQUIRE(bark_log == expected);
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
		auto r = [n = std::uint16_t{16127}]() mutable {
			n ^= 19937u;
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
		REQUIRE(g[0] == 64);
		REQUIRE(g[1] == 64);

		int c[2] = {};
		// char ab[] = "ab";
		for (const auto& o : polyvec) {
			++c[o->id()];
			// std::cout << ab[o->id()];
		}
		REQUIRE(c[0] == 64);
		REQUIRE(c[1] == 64);
	}

	SECTION("free make function") {
		// Automatically takes the larger capacity value, but returns a base
		// pointer
		auto o = kblib::make_poly_obj<small_base, big_derived>();
		static_assert(
		    std::is_same_v<decltype(o),
		                   kblib::poly_obj<small_base, sizeof(big_derived)>>,
		    "make_poly_obj must return the correct type.");
	}
	SECTION("hinted") {
		static_assert(kblib::detail::extract_derived_size<hinted_base>::value ==
		              hinted_base::max_derived_size);
		kblib::poly_obj<hinted_base> o;
		static_assert(decltype(o)::capacity ==
		              decltype(o)::base_type::max_derived_size);
		o = decltype(o)::make<hinted_derived>();
	}
}

namespace {
struct mem_ptr_test {
	int member{42};
	int get_member() & noexcept { return member; }
	int cget_member() const& noexcept { return member; }
	int rget_member() && noexcept { return member; }
	int crget_member() const&& noexcept { return member; }

	int get_member2() noexcept { return member; }
	int cget_member2() const noexcept { return member; }

	virtual ~mem_ptr_test() = default;
};
} // namespace

TEST_CASE("poly_obj->*") {
	kblib::poly_obj<mem_ptr_test> obj{std::in_place};
	SECTION("member data") {
		constexpr auto data_member = &mem_ptr_test::member;
		REQUIRE(obj->*data_member == 42);
		static_assert(std::is_same_v<decltype(obj->*data_member), int&>);
		static_assert(std::is_same_v<decltype(std::as_const(obj)->*data_member),
		                             const int&>);
		static_assert(
		    std::is_same_v<decltype(std::move(obj)->*data_member), int&&>);
		static_assert(
		    std::is_same_v<decltype(std::move(std::as_const(obj))->*data_member),
		                   const int&&>);
	}
	SECTION("member function") {
		constexpr auto func_member = &mem_ptr_test::get_member;
		constexpr auto cfunc_member = &mem_ptr_test::cget_member;
		constexpr auto rfunc_member = &mem_ptr_test::rget_member;
		constexpr auto crfunc_member = &mem_ptr_test::crget_member;
		REQUIRE((obj->*func_member)() == 42);
		static_assert(std::is_same_v<decltype((obj->*func_member)()), int>);
		REQUIRE((obj->*cfunc_member)() == 42);
		static_assert(std::is_same_v<decltype((obj->*cfunc_member)()), int>);
		REQUIRE((std::as_const(obj)->*cfunc_member)() == 42);
		static_assert(
		    std::is_same_v<decltype((std::as_const(obj)->*cfunc_member)()), int>);
		REQUIRE((std::move(obj)->*rfunc_member)() == 42);
		static_assert(
		    std::is_same_v<decltype((std::move(obj)->*rfunc_member)()), int>);
		REQUIRE((std::move(obj)->*crfunc_member)() == 42);
		static_assert(
		    std::is_same_v<decltype((std::move(obj)->*crfunc_member)()), int>);
		REQUIRE((std::move(std::as_const(obj))->*crfunc_member)() == 42);
		static_assert(std::is_same_v<
		              decltype((std::move(std::as_const(obj))->*crfunc_member)()),
		              int>);

		constexpr auto func_member2 = &mem_ptr_test::get_member2;
		constexpr auto cfunc_member2 = &mem_ptr_test::cget_member2;

		REQUIRE((obj->*func_member2)() == 42);
		static_assert(std::is_same_v<decltype((obj->*func_member2)()), int>);
		REQUIRE((obj->*cfunc_member2)() == 42);
		static_assert(std::is_same_v<decltype((obj->*cfunc_member2)()), int>);
		REQUIRE((std::as_const(obj)->*cfunc_member2)() == 42);
		static_assert(
		    std::is_same_v<decltype((std::as_const(obj)->*cfunc_member2)()),
		                   int>);
	}
}

namespace {
struct Base {
	virtual unsigned operator()() const noexcept = 0;
	Base() = default;
	Base(const Base&) = default;
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

struct thrower final : Base {
	unsigned operator()() const noexcept override { return member; }

	unsigned member;

	thrower(unsigned i = 0) : member(i) {}
	thrower(const thrower&) = default;

	thrower& operator=(const thrower& other) & {
		check_throw(other.member);
		member = other.member;
		return *this;
	}

 private:
	static void check_throw(unsigned v) {
		if ((v & 7u) == 0) {
			throw 0;
		}
	}
};
} // namespace

#ifndef FAST_TEST
TEST_CASE("poly_obj performance") {
	const auto start = std::chrono::steady_clock::now();
#ifdef NDEBUG
	constexpr unsigned count = 1000;
#else
	constexpr unsigned count = 100;
#endif

	std::vector<std::pair<unsigned, std::string_view>> reproducibility_test;

	auto push_checksum = [&](unsigned s, std::string_view name) {
		auto begin = reproducibility_test.begin();
		auto end = reproducibility_test.end();
		// Take only the first result for each run type
		if (std::find_if(begin, end, [&](const auto& p) {
			    return p.second == name;
		    }) == end) {
			reproducibility_test.emplace_back(s, name);
		}
	};

	/*
	 * Release / Debug, all times in microseconds:
	 *
	 *   Four separate contiguous arrays
	 * baseline:                    5.377 /  37.549
	 *   Polymorphism, always valid
	 * raw pointer:                10.470 /  51.205
	 * unique_ptr:                 11.826 /  70.346
	 * poly_obj:                   14.644 /  79.075
	 *   Type erasure, always valid
	 * std::function:              15.247 / 106.562
	 * std::visit(v, f):           27.375 / 238.055
	 * kblib::visit(v, f...):      37.308 / 258.965
	 * kblib::visit(v)(f...):      37.253 / 288.355
	 * visit_indexed:              30.203 / 208.894
	 * kblib::visit2:               8.407 / 211.649
	 * kblib::visit2_nop:           8.650 / 222.502
	 * std::get_if:                 8.580 / 158.164
	 * switch (v.index):            8.984 / 170.739
	 *   Polymorphism, checking for invalid
	 * raw pointer, ch:            15.859 /  56.917
	 * unique_ptr, ch:             18.344 / 108.046
	 * poly_obj, ch:               18.290 /  96.168
	 *   Type erasure, checking for invalid
	 * std::function, ch:          25.200 / 122.038
	 * kblib::visit2_nop, ch:      12.327 / 322.764
	 * std::get_if, ch:            12.386 / 232.006
	 * switch(v.index()), ch:      16.817 / 203.752
	 *   Type erasure, exceptions for invalid
	 * std::function, ex:          23.758 / 126.152
	 * std::visit(v, f), ex:       40.646 / 310.900
	 * kblib::visit(v, f...), ex:  44.016 / 328.484
	 * kblib::visit(v)(f...), ex:  46.345 / 377.681
	 * visit_indexed, ex:          40.066 / 261.261
	 * kblib::visit2, ex:          12.353 / 315.509
	 *
	 * Overall test runtime:  80s / 286s
	 *
	 * Conclusions:
	 *
	 * In release builds, visit_indexed, std::function, and std::visit are very
	 * slow, contiguous access is very fast, everything else is comparable to
	 * each other, except that exceptions are, of course, very slow.
	 *
	 * In debug builds, type erasure seems to be extremely slow, enough even to
	 * mostly drown out the exceptions, but the fastest is std::function by far.
	 * visit2 is a bit faster in release builds, but poly_obj isn't much slower
	 * and is much much faster than it in debug. In every instance, poly_obj
	 * compares favorably to unique_ptr, which is good, because it is meant to
	 * replace it in some uses.
	 *
	 * std::get_if and switch are predictably the fastest way of accessing a
	 * variant in debug, but in release are tied with visit2. I do not think
	 * that the messy and verbose code is justified by the debug performance
	 * gain compared to the other options.
	 *
	 * If you want to use exceptions for invalid objects, kblib::visit2 is the
	 * fastest way, followed by std::function. The others are equally twice as
	 * slow.
	 *
	 * I cannot tell why visit_indexed is so much slower than visit2 in this
	 * test. Their code is very similar. It must be the additional parameter
	 * causing delays in setting up the call.
	 *
	 */

	BENCHMARK_ADVANCED("baseline")(Catch::Benchmark::Chronometer meter) {
		std::vector<Derived1> d1;
		std::vector<Derived2> d2;
		std::vector<Derived3> d3;
		std::vector<Derived4> d4;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d1.emplace_back(v);
				break;
			case 1:
				d2.emplace_back(v);
				break;
			case 2:
				d3.emplace_back(v);
				break;
			case 3:
				d4.emplace_back(v);
			}
		}

		unsigned accum{};
		meter.measure([&] {
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
			return accum;
		});
		// push_checksum(accum, "baseline");
	};
	BENCHMARK_ADVANCED("raw pointer")(Catch::Benchmark::Chronometer meter) {
		std::vector<Base*> d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.push_back(new Derived1(v));
				break;
			case 1:
				d.push_back(new Derived2(v));
				break;
			case 2:
				d.push_back(new Derived3(v));
				break;
			case 3:
				d.push_back(new Derived4(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (auto x : d) {
				accum += (*x)();
			}
			return accum;
		});
		push_checksum(accum, "raw pointer");
		for (auto x : d) {
			delete x;
		}
	};
	BENCHMARK_ADVANCED("unique_ptr")
	(Catch::Benchmark::Chronometer meter) {
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

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += (*x)();
			}
			return accum;
		});
		push_checksum(accum, "unique pointer");
	};
	BENCHMARK_ADVANCED("poly_obj")(Catch::Benchmark::Chronometer meter) {
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

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += x();
			}
			return accum;
		});
		push_checksum(accum, "poly_obj");
	};
	BENCHMARK_ADVANCED("std::function")(Catch::Benchmark::Chronometer meter) {
		std::vector<std::function<unsigned()>> d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.emplace_back(Derived1(v));
				break;
			case 1:
				d.emplace_back(Derived2(v));
				break;
			case 2:
				d.emplace_back(Derived3(v));
				break;
			case 3:
				d.emplace_back(Derived4(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += x();
			}
			return accum;
		});
		push_checksum(accum, "std::function");
	};
	{
		std::vector<std::variant<Derived1, Derived2, Derived3, Derived4>> d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.emplace_back(Derived1(v));
				break;
			case 1:
				d.emplace_back(Derived2(v));
				break;
			case 2:
				d.emplace_back(Derived3(v));
				break;
			case 3:
				d.emplace_back(Derived4(v));
				break;
			}
		}
		BENCHMARK_ADVANCED("std::visit(v, f)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += std::visit([](const auto& v) { return v(); }, x);
				}
				return accum;
			});
			push_checksum(accum, "std::visit(v, f)");
		};
		BENCHMARK_ADVANCED("kblib::visit(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit(x, [](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit(v)(f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit(x)([](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit(v)(f...)");
		};
		BENCHMARK_ADVANCED("visit_indexed(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit_indexed(
					    x, [](auto, const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "visit_indexed(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit2(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit2(x, [](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit2(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit2_nop(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					kblib::visit2_nop(x, [&](const auto& v) { accum += v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit2_nop(v, f...)");
		};
		BENCHMARK_ADVANCED("std::get_if")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
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
				return accum;
			});
			push_checksum(accum, "std::get_if");
		};
		BENCHMARK_ADVANCED("switch (v.index())")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
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
						break;
					default:;
					}
				}
				return accum;
			});
			push_checksum(accum, "switch (v.index())");
		};
	}

	// Test speed when some objects are invalid
	{
		BENCHMARK_ADVANCED("raw pointer, ch")
		(Catch::Benchmark::Chronometer meter) {
			std::vector<Base*> d;
			kblib::FNV_hash<unsigned> h;
			for (auto i : kblib::range(count)) {
				auto v = static_cast<unsigned>(h(i));
				switch (v % 5) {
				case 0:
					d.push_back(new Derived1(v));
					break;
				case 1:
					d.push_back(new Derived2(v));
					break;
				case 2:
					d.push_back(new Derived3(v));
					break;
				case 3:
					d.push_back(new Derived4(v));
					break;
				case 4:
					try {
						d.push_back(new thrower(v));
					} catch (int) {
						d.push_back(nullptr);
					}
				}
			}

			unsigned accum{};
			meter.measure([&] {
				for (auto x : d) {
					if (x) {
						accum += (*x)();
					}
				}
				return accum;
			});
			for (auto x : d) {
				delete x;
			}
		};
		BENCHMARK_ADVANCED("unique_ptr, ch")
		(Catch::Benchmark::Chronometer meter) {
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
					break;
				case 4:
					try {
						d.push_back(std::make_unique<thrower>(v));
					} catch (int) {
						d.push_back(nullptr);
					}
				}
			}

			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (x) {
						accum += (*x)();
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("poly_obj, ch")(Catch::Benchmark::Chronometer meter) {
			using poly_t = kblib::poly_obj<Base, sizeof(Derived1), int>;
			std::vector<poly_t> d;
			kblib::FNV_hash<unsigned> h;
			for (auto i : kblib::range(count)) {
				auto v = h(i);
				switch (v % 5) {
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
					break;
				case 4:
					try {
						d.push_back(poly_t::make<thrower>(v));
					} catch (int) {
						d.emplace_back(nullptr);
					}
				}
			}

			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (x) {
						accum += x();
					}
				}
				return accum;
			});
		};
		std::vector<std::function<unsigned()>> df;
		std::vector<std::variant<Derived1, Derived2, Derived3, Derived4, thrower>>
		    d;
		kblib::FNV_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 5) {
			case 0:
				df.emplace_back(Derived1(v));
				d.emplace_back(Derived1(v));
				break;
			case 1:
				df.emplace_back(Derived2(v));
				d.emplace_back(Derived2(v));
				break;
			case 2:
				df.emplace_back(Derived3(v));
				d.emplace_back(Derived3(v));
				break;
			case 3:
				df.emplace_back(Derived4(v));
				d.emplace_back(Derived4(v));
				break;
				// Test speed of exception throwing and catching
			case 4:
				try {
					df.emplace_back(thrower(v));
				} catch (int) {
					d.emplace_back(thrower());
				}
				// These have to be done in separate try blocks because otherwise
				// df would throw and d wouldn't get pushed
				try {
					auto& b = d.emplace_back(std::in_place_type_t<thrower>{}, 0);
					b = thrower(v);
				} catch (int) {
				}
			}
		}
		BENCHMARK_ADVANCED("std::function, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : df) {
					if (x) {
						accum += x();
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit2_nop(v, f...), ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					kblib::visit2_nop(x, [&](const auto& v) { accum += v(); });
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("std::get_if, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (auto* p = std::get_if<0>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<1>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<2>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<3>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<4>(&x)) {
						accum += (*p)();
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("switch (v.index()), ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
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
						break;
					case 4:
						accum += std::get<4>(x)();
						break;
					default:;
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("std::function, ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : df) {
					try {
						accum += x();
					} catch (const std::bad_function_call&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("std::visit(v, f), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += std::visit([](const auto& v) { return v(); }, x);
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit(v)(f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit(x)([](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("visit_indexed(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit_indexed(
						    x, [](auto, const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit2(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit2(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
	}
	auto end = std::chrono::steady_clock::now();
	std::chrono::duration<float, std::ratio<1, 1>> time = end - start;
	std::cout << "\n\nProfiling took " << time.count() << " seconds\n";

	// All runs should produce identical results
	unsigned expected_value = reproducibility_test[0].first;
	for (auto i : kblib::range(std::size_t(1), reproducibility_test.size())) {
		const auto& run = reproducibility_test[i];
		INFO(i << ": " << run.second << ": " << run.first
		       << " != " << expected_value);
		REQUIRE(run.first == expected_value);
	}
}
#endif

#endif
