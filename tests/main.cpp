#include "kblib/containers.h"
#include "kblib/icu.h"
#include "kblib/kblib.h"

#include <array>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string_view>

#include "catch/catch.hpp"

#if KBLIB_USE_CXX17
template <class T>
constexpr auto type_name_f() -> std::string_view {
	using namespace std;
	auto sz = sizeof("type_name_f") - 1;
#	ifdef __clang__
	string_view p = __PRETTY_FUNCTION__;
	auto begin = 25 + sz;
	return string_view(p.data() + begin, p.size() - begin - 1);
#	elif defined(__GNUC__)
	string_view p = __PRETTY_FUNCTION__;
#		ifdef __INTEL_COMPILER
	auto begin = 76 + sz;
	return string_view(p.data() + begin, p.size() - begin - 1);
#		elif __cplusplus < 201402
	auto begin = 17 + sz;
	return string_view(p.data() + begin, p.size() - begin - 1);
#		else
	auto begin = 40 + sz;
	return string_view(p.data() + begin, p.find(';', begin) - begin);
#		endif
#	elif defined(_MSC_VER)
	string_view p = __FUNCSIG__;
	auto begin = 75 + sz;
	return string_view(p.data() + begin, p.size() - begin - 7);
#	endif
}
static_assert(type_name_f<char>() == "char");
#endif

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
	constexpr auto operator->() const noexcept -> bad_iterator<depth - 1> {
		return {p};
	}
};

template <>
struct bad_iterator<0> {
	int* p;
	constexpr auto operator->() const noexcept -> int* { return p; }
};

constexpr auto test() noexcept -> bool {
	int i{};
	return &i == kblib::to_pointer(&i)
	       and &i == kblib::to_pointer(bad_iterator<0>{&i})
	       and &i == kblib::to_pointer(bad_iterator<1>{&i})
	       and &i == kblib::to_pointer(bad_iterator<100>{&i});
}

static_assert(test(), "");

auto test(std::streamsize s) -> void { std::cout << s << '\n'; }
void test_trie();

TEST_CASE("main") {

	for (unsigned b = 0; b < 62; ++b) {
		std::uint64_t i = 1ull << b;
		std::pair<int, int> lengths[] = {
		    {std::to_string(i).length(), kblib::count_digits(i)},
		    {std::to_string(-i).length(), kblib::count_digits(-i)},
		};
		for (auto test : lengths) {
			if (test.first != test.second) {
				std::cout << "count_digits failure at: " << i << ".\nExpected "
				          << test.first << ", got " << test.second << ".\n";
				break;
			}
		}
	}

	// test_trie();
}
