#include "kblib/containers.h"
#include "kblib/icu.h"
#include "kblib/kblib.h"

#include <array>
#include <iostream>
#include <list>
#include <map>
#include <set>
#include <string_view>

#include "catch2/catch.hpp"

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
	return &i == kblib::to_pointer(&i) and
	       & i == kblib::to_pointer(bad_iterator<0>{&i}) and
	       & i == kblib::to_pointer(bad_iterator<1>{&i}) and
	       & i == kblib::to_pointer(bad_iterator<100>{&i});
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
