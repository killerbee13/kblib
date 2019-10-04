#include "kblib/containers.h"
#include "kblib/icu.h"
#include "kblib/kblib.h"

#include <array>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string_view>

#include "catch.hpp"

template <class T>
constexpr std::string_view type_name_f() {
	using namespace std;
#ifdef __clang__
	string_view p = __PRETTY_FUNCTION__;
	return string_view(p.data() + 36, p.size() - 36 - 1);
#elif defined(__GNUC__)
	string_view p = __PRETTY_FUNCTION__;
#if __cplusplus < 201402
	return string_view(p.data() + 36, p.size() - 36 - 1);
#else
	return string_view(p.data() + 49, p.find(';', 49) - 49);
#endif
#elif defined(_MSC_VER)
	string_view p = __FUNCSIG__;
	return string_view(p.data() + 84, p.size() - 84 - 7);
#endif
}

template <typename C>
constexpr const char type_name[] = "unknown";

template <>
KBLIB_UNUSED constexpr const char type_name<char>[] = "char";
template <>
KBLIB_UNUSED constexpr const char type_name<unsigned char>[] = "unsigned char";
template <>
KBLIB_UNUSED constexpr const char type_name<signed char>[] = "signed char";

template <int depth>
struct bad_iterator {
	int* p;
	constexpr bad_iterator<depth - 1> operator->() const noexcept { return {p}; }
};

template <>
struct bad_iterator<0> {
	int* p;
	constexpr int* operator->() const noexcept { return p; }
};

constexpr bool test() noexcept {
	int i{};
	return &i == kblib::to_pointer(&i) &&
	       &i == kblib::to_pointer(bad_iterator<0>{&i}) &&
	       &i == kblib::to_pointer(bad_iterator<1>{&i}) &&
	       &i == kblib::to_pointer(bad_iterator<100>{&i});
}

static_assert(test(), "");

void test(std::streamsize s) { std::cout << s << '\n'; }
void test_trie();

#if KBLIB_USE_CXX17
void poly_test();
#endif

TEST_CASE("main") {
	std::cout << kblib::signed_cast<unsigned>(-1ll) << '\n'
	          << kblib::signed_cast<signed>(static_cast<unsigned short>(78))
	          << '\n';
	static_assert(
	    std::is_same<signed short, decltype(kblib::signed_cast<signed>(
	                                   std::declval<unsigned short>()))>::value,
	    "");
	static_assert(std::is_same<unsigned short,
	                           decltype(kblib::signed_cast<unsigned>(
	                               std::declval<unsigned short>()))>::value,
	              "");
	static_assert(std::is_same<signed short, decltype(kblib::signed_cast<signed>(
	                                             std::declval<short>()))>::value,
	              "");
	static_assert(std::is_same<unsigned short,
	                           decltype(kblib::signed_cast<unsigned>(
	                               std::declval<unsigned short>()))>::value,
	              "");
	static_assert(
	    std::is_same<signed int, decltype(kblib::signed_cast<signed>(
	                                 std::declval<unsigned int>()))>::value,
	    "");
	std::cout << type_name<std::make_signed_t<char>> << '\n';
	std::cout << type_name<std::make_signed_t<unsigned char>> << '\n';
	std::cout << type_name<std::make_unsigned_t<char>> << '\n';
	std::cout << type_name<std::make_unsigned_t<signed char>> << '\n';
	std::cout << "char is "
	          << (std::is_signed<char>::value ? "signed" : "not signed") << '\n';
	//  auto _1 = kblib::signed_cast<signed>(0.0);
	//  auto _2 = kblib::signed_cast<double>(0);
#if KBLIB_USE_CXX17
	std::cout
	    << "Next_larger: \n"
	    << "signed char: "
	    << type_name_f<kblib::detail::next_larger_signed<signed char>::type>()
	    << '\n'
	    << "short:       "
	    << type_name_f<kblib::detail::next_larger_signed<short>::type>() << '\n'
	    << "int:         "
	    << type_name_f<kblib::detail::next_larger_signed<int>::type>() << '\n';
	// long long can't be promoted
	// std::cout
	//     << "long long:   "
	//     << type_name_f<kblib::detail::next_larger_signed<long long>::type>()
	//     <<
	//     '\n';

	auto filestr = kblib::get_file_contents("main.cpp");
	if (filestr) {
		std::cout << "FNV32a(main.cpp): " << kblib::FNV32a(*filestr) << '\n';
	} else {
		std::cout << "failed to open main.cpp\n";
	}
	auto filevec = kblib::get_file_contents<std::vector<uint8_t>>("main.cpp");
	if (filevec) {
		std::cout << "FNV32a(main.cpp): " << kblib::FNVa<std::uint32_t>(*filevec)
		          << '\n';
	} else {
		std::cout << "failed to open main.cpp\n";
	}
	// auto filewstr = kblib::get_file_contents<std::u32string>("main.cpp");
	// deque is not a contiguous container, and does not provide a .data() member
	// so get_file_contents doesn't work.
	// auto fileerror = kblib::get_file_contents<std::deque<char>>("main.cpp");
#endif

	for (int b = 0; b < 62; ++b) {
		long long i = 1ll << b;
		std::pair<int, int> lengths[] = {
		    {std::to_string(i).length(), kblib::digitsOf(i)},
		    {std::to_string(-i).length(), kblib::digitsOf(-i)},
		};
		for (auto test : lengths) {
			if (test.first != test.second) {
				std::cout << "digitsOf failure at: " << i << ".\nExpected "
				          << test.first << ", got " << test.second << ".\n";
				break;
			}
		}
	}
	{
		// to_unique can make unique_ptrs to anonymous types
		auto p = kblib::to_unique(new auto([] {}), [](auto* p) {
			delete p;
		});
	}

	{
		auto equal = [](auto r1, auto r2) {
			auto r1b = r1.begin();
			auto r1e = r1.end();
			auto r2b = r2.begin();
			auto r2e = r2.end();
			return (std::distance(r1b, r1e) == std::distance(r2b, r2e)) &&
			       kblib::equal(r1b, r1e, r2b);
		};

		constexpr auto target =
		    std::array<int, 10>{{0, 1, 2, 3, 4, 5, 6, 7, 8, 9}};
		auto i1 = kblib::buildiota<std::vector<int>>(10, 0);
		auto i2 = kblib::buildiota<std::vector<int>>(10, 0, 1);
		auto i3 = kblib::buildiota<std::array<int, 10>>(0);
		auto i4 = kblib::buildiota<std::array<int, 10>>(0, 1);
		auto i5 =
		    kblib::buildiota<kblib::construct_with_size<std::vector<int>, 10>>(0);

		REQUIRE((i1.size() == target.size() && equal(i1, target)));
		REQUIRE((i2.size() == target.size() && equal(i2, target)));
		REQUIRE((i3.size() == target.size() && equal(i3, target)));
		REQUIRE((i4.size() == target.size() && equal(i4, target)));
		REQUIRE((i5.size() == target.size() && equal(i5, target)));

		{
			std::array<int, 10> haystack{1, 1, 2, 5, 6, 2, 4, 7, 0, 6};
			const int N = 5;
			std::array<int, N> maxN_target{7, 6, 6, 5, 4};

			REQUIRE(equal(maxN_target, kblib::get_max_n<std::vector<int>>(
			                               haystack.begin(), haystack.end(), N)));
			std::vector<int> maxN_copy;
			kblib::get_max_n(haystack.begin(), haystack.end(),
			                 std::back_inserter(maxN_copy), 5);
			REQUIRE(equal(maxN_target, maxN_copy));

			REQUIRE(equal(maxN_target,
			              kblib::get_max_n<std::multiset<int, std::greater<>>>(
			                  haystack.begin(), haystack.end(), N)));

			std::vector<int> maxN_psc(N);
			std::partial_sort_copy(haystack.begin(), haystack.end(),
			                       maxN_psc.begin(), maxN_psc.end(),
			                       std::greater<>{});
			REQUIRE(equal(maxN_target, maxN_psc));

			// SFINAE prevents get_max_n from being called with invalid arguments,
			// even though count has moved from third to fourth on this overload.
			// It will also fail when the output range is not assignable.

			// kblib::get_max_n(haystack.begin(), haystack.end(), maxN.cbegin(),
			// 1); kblib::get_max_n(haystack.begin(), haystack.end(), 1,
			// maxN.begin());

			auto range = kblib::range(0, 50, 3);
			REQUIRE(
			    equal(range, std::vector<int>{0, 3, 6, 9, 12, 15, 18, 21, 24, 27,
			                                  30, 33, 36, 39, 42, 45, 48}));
			auto maxN_range =
			    kblib::get_max_n<std::vector<int>>(range.begin(), range.end(), 5);
			REQUIRE(maxN_range == std::vector<int>{48, 45, 42, 39, 36});
		}
	}
#if KBLIB_USE_CXX17
	poly_test();
#endif
	{
		std::istringstream is{"line1\nline2\nline3\n"};
		std::string line;
		while (is >> kblib::get_line(line)) {
			std::cout << std::quoted(line) << '\n';
		}
	}
	{
		std::wistringstream is{L"line1\nline2\nline3\n"};
		std::wstring line;
		while (is >> kblib::get_line(line)) {
			std::wcout << std::quoted(line) << '\n';
		}
	}
	{
		std::map<int, int> m;
		const auto& cm = m;

		// m[0] doesn't exist
		REQUIRE(kblib::try_get(m, 0) == nullptr);
		// Works for const maps
		REQUIRE(kblib::try_get(cm, 0) == nullptr);
		// Make m[0]
		m[0] = 0;

		// Basic correctness testing
		REQUIRE(kblib::try_get(m, 0) == &m[0]);
		*kblib::try_get(m, 0) = 1;
		REQUIRE(m[0] == 1);

		// Returns pointer to const when given const map
		//*kblib::try_get(cm, 0) = 1;

		// Can't call for temporaries because it would return a dangling pointer
		// kblib::try_get(std::map<int, int>{{0, 1}}, 0);
	}
	{
		auto smart_ptr = std::make_unique<int>(0);
		[[maybe_unused]] auto* raw_pointer = kblib::to_pointer(smart_ptr);
		const auto& smart_ptr_const_ref = smart_ptr;
		[[maybe_unused]] auto* raw_pointer2 = kblib::to_pointer(smart_ptr_const_ref);
		std::cout << "FNV_hash(1000): " << kblib::FNV_hash<int>{}(1000) << '\n';

		// range supports iterators and other similar types.
		std::vector<int> v(100);
		REQUIRE(v.begin() == *kblib::range(v.begin(), v.end()).begin());
		REQUIRE(v.end() == *kblib::range(v.begin(), v.end()).end());
	}
	// test_trie();
}

#if KBLIB_USE_CXX17
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

void poly_test() {
	{
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

	{
		// Warning issued because of non-virtual destructor call
		kblib::poly_obj<bad_base1> o3, o4{std::in_place};
		// unsafe - no virtual destructor
		// o3 = kblib::poly_obj<bad_base1>::make<bad_derived1>();
	}

	{
		kblib::poly_obj<bad_base2> o5, o6{std::in_place};
		// illegal - non-public base
		// o5 = kblib::poly_obj<bad_base2>::make<bad_derived2>();
	}

	{
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

	{
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

	{
		kblib::poly_obj<copyable_base, sizeof(copyable_base), kblib::move_only_t>
		    o1{std::in_place};
		// Valid because the template parameters explicitly disable copying, so
		// derived classes don't need to be copyable.
		auto o2 = decltype(o1)::make<noncopyable_derived>();
	}

	{
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
}
#endif
