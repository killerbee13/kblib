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

	// test_trie();
}
