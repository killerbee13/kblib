#include "kblib/poly_obj.h"

#include "catch2/catch.hpp"

#if KBLIB_USE_CXX17

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

	virtual auto bark() const -> void { bark_log.push_back(vgood_base); }
};
struct good_derived : good_base {
	~good_derived() noexcept override { bark_log.push_back(dgood_derived); }

	auto bark() const -> void override { bark_log.push_back(vgood_derived); }
};
struct unrelated {};
struct bad_nocopy : good_base {
	bad_nocopy(const bad_nocopy&) = delete;
};

struct bad_base1 {
	virtual auto bark() const -> void { bark_log.push_back(vbad_base1); }
	// non-virtual destructor
	~bad_base1() noexcept { bark_log.push_back(dbad_base1); }
};
struct bad_derived1 : bad_base1 {
	auto bark() const -> void override { bark_log.push_back(vbad_derived1); }
	KBLIB_UNUSED ~bad_derived1() noexcept { bark_log.push_back(dbad_derived1); }
};

struct bad_base2 {
	bad_base2() = default;
	virtual ~bad_base2() noexcept { bark_log.push_back(dbad_base2); }
	virtual auto bark() const -> void { bark_log.push_back(vbad_base2); }
};
struct bad_derived2 : protected bad_base2 {
	auto bark() const -> void override { bark_log.push_back(vbad_derived2); }
	~bad_derived2() noexcept override { bark_log.push_back(dbad_derived2); }
};
struct small_base {
	small_base() = default;
	virtual ~small_base() noexcept = default;
	virtual auto bark() const -> void { bark_log.push_back(vsmall_base); }
	virtual auto id() const -> int { return 0; }
};
struct big_derived : small_base {
	std::size_t x
	    = (static_cast<void>(x = 1),
	       kblib::FNVa_a<std::size_t>(
	           reinterpret_cast<const char (&)[sizeof(big_derived)]>(*this)));
	auto bark() const -> void override { bark_log.push_back(vbig_derived); }
	~big_derived() noexcept override = default;
	auto id() const -> int override { return 1; }
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
		o7 = kblib::poly_obj<small_base,
		                     sizeof(big_derived)>::make<big_derived>();
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
		kblib::poly_obj<copyable_base, sizeof(copyable_base),
		                kblib::move_only_traits<copyable_base>>
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
		static_assert(kblib::poly_obj_traits<hinted_base>::default_capacity
		              == hinted_base::max_derived_size);
		kblib::poly_obj<hinted_base> o;
		static_assert(decltype(o)::capacity
		              == decltype(o)::base_type::max_derived_size);
		o = decltype(o)::make<hinted_derived>();
	}
}

namespace {
struct mem_ptr_test {
	int member{42};
	auto get_member() & noexcept -> int { return member; }
	auto cget_member() const& noexcept -> int { return member; }
	auto rget_member() && noexcept -> int { return member; }
	auto crget_member() const&& noexcept -> int { return member; }

	auto get_member2() noexcept -> int { return member; }
	auto cget_member2() const noexcept -> int { return member; }

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

#endif // KBLIB_USE_CXX17
