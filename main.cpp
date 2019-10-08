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
	SECTION("to_unique(lambda)") {
		// to_unique can make unique_ptrs to anonymous types
		auto p = kblib::to_unique(new auto([] {}), [](auto* p) {
			delete p;
		});
	}

	SECTION("assorted algorithms") {
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

	SECTION("get_line") {
		std::istringstream is{"line1\nline2\nline3\n"};
		std::string line;
		while (is >> kblib::get_line(line)) {
			std::cout << std::quoted(line) << '\n';
		}
	}
	SECTION("wide get_line") {
		std::wistringstream is{L"line1\nline2\nline3\n"};
		std::wstring line;
		while (is >> kblib::get_line(line)) {
			std::wcout << std::quoted(line) << '\n';
		}
	}
	SECTION("try_get") {
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
	SECTION("to_pointer") {
		auto smart_ptr = std::make_unique<int>(0);
		[[maybe_unused]] auto* raw_pointer = kblib::to_pointer(smart_ptr);
		const auto& smart_ptr_const_ref = smart_ptr;
		[[maybe_unused]] auto* raw_pointer2 = kblib::to_pointer(smart_ptr_const_ref);
		std::cout << "FNV_hash(1000): " << kblib::FNV_hash<int>{}(1000) << '\n';
	}
	SECTION("range") {
		// range supports iterators and other similar types.
		std::vector<int> v(100);
		REQUIRE(v.begin() == *kblib::range(v.begin(), v.end()).begin());
		REQUIRE(v.end() == *kblib::range(v.begin(), v.end()).end());
	}
	// test_trie();
}
