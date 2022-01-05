#include "kblib/algorithm.h"
#include "catch.hpp"

#include <iostream>
#include <set>
#include <string>
#include <vector>

TEST_CASE("erase") {
	const auto equal = [](auto a, auto b) {
		return std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b));
	};
	SECTION("erase and erase_if") {
		const std::vector<unsigned> erase_test{2, 2, 3, 4, 5, 7, 8, 11};
		const std::vector<unsigned> no_2s = {3, 4, 5, 7, 8, 11};
		auto erase_copy = erase_test;
		kblib::erase(erase_copy, 2);
		CHECK(equal(no_2s, erase_copy));
		erase_copy = erase_test;
		const std::vector<unsigned> no_evens = {3, 5, 7, 11};
		kblib::erase_if(erase_copy, [](unsigned x) { return (~x) & 1u; });
		CHECK(equal(no_evens, erase_copy));
	}
}

TEST_CASE("find family") {
	//           0  1  2  3  4  5  6  7  8  9  e
	int vec[] = {0, 3, 4, 5, 6, 1, 2, 4, 4, 5};
	auto begin = std::begin(vec);
	auto end = std::end(vec);
	CHECK(kblib::find(begin, end, 4) == begin + 2);
	CHECK(kblib::find(begin, end, 10) == end);
	CHECK(kblib::find(begin, end, 0) == begin);
	CHECK(kblib::find(begin, end, 10) == end);

	CHECK(kblib::find_last(begin, end, 10) == end);
	CHECK(kblib::find_last(begin, end, 0) == begin);
	CHECK(kblib::find_last(begin, end, 4) == begin + 8);

	CHECK(kblib::find_if(begin, end, [](int i) { return i == 2; }) == begin + 6);
	CHECK(kblib::find_if_not(begin, end, [](int i) { return i == 2; }) == begin);
	CHECK(kblib::find_if(begin, end, [](int) { return false; }) == end);
	CHECK(kblib::find_if_not(begin, end, [](int) { return true; }) == end);

	CHECK(kblib::find_last_if(begin, end, [](int i) { return i == 2; })
	      == begin + 6);
	CHECK(kblib::find_last_if_not(begin, end, [](int i) { return i == 2; })
	      == begin + 9);
	CHECK(kblib::find_last_if(begin, end, [](int) { return false; }) == end);
	CHECK(kblib::find_last_if_not(begin, end, [](int) { return true; }) == end);

	CHECK(kblib::contains(vec, 0));
	CHECK(kblib::contains(vec, 1));
	CHECK(kblib::contains(vec, 2));
	CHECK(kblib::contains(vec, 3));
	CHECK(kblib::contains(vec, 4));
	CHECK(kblib::contains(vec, 5));
	CHECK(kblib::contains(vec, 6));
	CHECK(not kblib::contains(vec, 7));
	CHECK(not kblib::contains(vec, 8));
	CHECK(not kblib::contains(vec, -1));

	CHECK(kblib::contains_any(vec, vec));
	int none[] = {7, 8, -1};
	CHECK(not kblib::contains_any(vec, none));
}
TEST_CASE("find_in") {
	//           0  1  2  3  4  5  6  7  8  9  e
	int vec[] = {0, 3, 4, 5, 6, 1, 2, 4, 4, 5};
	auto begin = std::begin(vec);
	auto end = std::end(vec);
	auto size = kblib::fakestd::size(vec);
	CHECK(kblib::find_in(begin, end, 4) == 2);
	CHECK(kblib::find_in(begin, end, 10) == size);
	CHECK(kblib::find_in(begin, end, 0) == 0);
	CHECK(kblib::find_in(begin, end, 10) == size);

	CHECK(kblib::find_last_in(begin, end, 10) == size);
	CHECK(kblib::find_last_in(begin, end, 0) == 0);
	CHECK(kblib::find_last_in(begin, end, 4) == 8);

	CHECK(kblib::find_in_if(begin, end, [](int i) { return i == 2; }) == 6);
	CHECK(kblib::find_in_if_not(begin, end, [](int i) { return i == 2; }) == 0);
	CHECK(kblib::find_in_if(begin, end, [](int) { return false; }) == size);
	CHECK(kblib::find_in_if_not(begin, end, [](int) { return true; }) == size);

	CHECK(kblib::find_last_in_if(begin, end, [](int i) { return i == 2; }) == 6);
	CHECK(kblib::find_last_in_if_not(begin, end, [](int i) { return i == 2; })
	      == 9);
	CHECK(kblib::find_last_in_if(begin, end, [](int) { return false; }) == size);
	CHECK(kblib::find_last_in_if_not(begin, end, [](int) { return true; })
	      == size);
}

TEST_CASE("search_replace") {
	using namespace std::literals;
	using std::begin;
	using std::end;
	SECTION("search_replace_copy_1") {
		const auto haystack = "a string with words"s;
		const auto needle = "with"s;
		const auto replace = "comprising"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == "a string comprising words");
	}
	SECTION("search_replace_copy_2(no match)") {
		const auto haystack = "a string with words"s;
		const auto needle = "without"s;
		const auto replace = "comprising"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == haystack);
	}
	SECTION("search_replace_copy_3(multiple matches)") {
		const auto haystack = "a string with multiple 'with's"s;
		const auto needle = "with"s;
		const auto replace = "containing"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == "a string containing multiple 'containing's");
	}
	SECTION("search_replace_copy_4(empty match)") {
		const auto haystack = "a string with words"s;
		const auto needle = ""s;
		const auto replace = "comprising"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == haystack);
	}
	SECTION("search_replace_copy_5(match at beginning)") {
		const auto haystack = "a string with words"s;
		const auto needle = "a"s;
		const auto replace = "another"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == "another string with words");
	}
	SECTION("search_replace_copy_6(match at end)") {
		const auto haystack = "a string with words"s;
		const auto needle = "words"s;
		const auto replace = "letters"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == "a string with letters");
	}
	SECTION("search_replace_copy_7(only matches)") {
		const auto haystack = "abababababab"s;
		const auto needle = "ab"s;
		const auto replace = "ba"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == "babababababa");
	}
	SECTION("search_replace_copy_8(match is whole string)") {
		const auto haystack = "string"s;
		const auto needle = haystack;
		const auto replace = "replace"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == replace);
	}
	SECTION("search_replace_copy_9(overlap)") {
		const auto haystack = "abcabcab"s;
		const auto needle = "abcab"s;
		const auto replace = "a"s;
		std::string result;
		kblib::search_replace_copy(begin(haystack), end(haystack), begin(needle),
		                           end(needle), begin(replace), end(replace),
		                           std::back_inserter(result));
		CHECK(result == "acab");
	}
}

TEST_CASE("get_max family") {
	/// TODO(killerbee13): tests for get_max_*
	std::array<int, 11> arr{0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
	{
		auto max_two
		    = kblib::get_max_n_old<std::vector<int>>(arr.begin(), arr.end(), 2);
		CHECK(max_two.size() == 2);
		CHECK(std::max(max_two[0], max_two[1]) == 10);
		CHECK(std::min(max_two[0], max_two[1]) == 9);
	}
	{
		auto max_two
		    = kblib::get_max_n_old<std::set<int>>(arr.begin(), arr.end(), 2);
		CHECK(max_two.size() == 2);
		CHECK(max_two.find(10) != max_two.end());
		CHECK(max_two.find(9) != max_two.end());
	}
	{
		auto max_two
		    = kblib::get_max_n<std::vector<int>>(arr.begin(), arr.end(), 2);
		CHECK(max_two.size() == 2);
		CHECK(std::max(max_two[0], max_two[1]) == 10);
		CHECK(std::min(max_two[0], max_two[1]) == 9);
	}
	{
		auto max_two = kblib::get_max_n<std::set<int>>(arr.begin(), arr.end(), 2);
		CHECK(max_two.size() == 2);
		CHECK(max_two.find(10) != max_two.end());
		CHECK(max_two.find(9) != max_two.end());
	}
}

TEST_CASE("general algorithms") {
	/// TODO(killerbee13): tests for other algorithms
}

TEST_CASE("assorted algorithms") {
	auto equal = [](auto r1, auto r2) {
		auto r1b = r1.begin();
		auto r1e = r1.end();
		auto r2b = r2.begin();
		auto r2e = r2.end();
		return (std::distance(r1b, r1e) == std::distance(r2b, r2e))
		       and kblib::equal(r1b, r1e, r2b);
	};
	std::array<int, 10> haystack{1, 1, 2, 5, 6, 2, 4, 7, 0, 6};
	const int N = 5;
	std::array<int, N> maxN_target{7, 6, 6, 5, 4};

	CHECK(equal(maxN_target, kblib::get_max_n<std::vector<int>>(
	                             haystack.begin(), haystack.end(), N)));
	std::vector<int> maxN_copy;
	kblib::get_max_n(haystack.begin(), haystack.end(),
	                 std::back_inserter(maxN_copy), 5);
	CHECK(equal(maxN_target, maxN_copy));

	CHECK(
	    equal(maxN_target, kblib::get_max_n<std::multiset<int, std::greater<>>>(
	                           haystack.begin(), haystack.end(), N)));

	std::vector<int> maxN_psc(N);
	std::partial_sort_copy(haystack.begin(), haystack.end(), maxN_psc.begin(),
	                       maxN_psc.end(), std::greater<>{});
	CHECK(equal(maxN_target, maxN_psc));

	// SFINAE prevents get_max_n from being called with invalid arguments,
	// even though count has moved from third to fourth on this overload.
	// It will also fail when the output range is not assignable.

	// kblib::get_max_n(haystack.begin(), haystack.end(), maxN.cbegin(),
	// 1); kblib::get_max_n(haystack.begin(), haystack.end(), 1,
	// maxN.begin());

	auto range = kblib::range(0, 50, 3);
	CHECK(equal(range, std::vector<int>{0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30,
	                                    33, 36, 39, 42, 45, 48}));
	auto maxN_range
	    = kblib::get_max_n<std::vector<int>>(range.begin(), range.end(), 5);
	CHECK(maxN_range == std::vector<int>{48, 45, 42, 39, 36});
}

TEST_CASE("zip") {
	SECTION("non-overlapping") {
		std::vector<int> input1{1, 2, 3, 4, 5, 6};
		std::vector<short> input2{2, 3, 4, 5, 6, 7};

		for (auto t : kblib::zip(input1.begin(), input1.end(), input2.begin())) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			CHECK(a + 1 == b);
		}
	}
	SECTION("identical") {
		std::vector<int> input{1, 2, 3, 4, 5, 6, 7, 8};
		for (auto t : kblib::zip(input.begin(), input.end(), input.begin())) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			CHECK(&a == &b);
		}
	}
	SECTION("overlapping") {
		std::vector<int> input{1, 2, 3, 4, 5, 6, 7, 8};
		for (auto t :
		     kblib::zip(input.begin(), input.end() - 1, input.begin() + 1)) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			CHECK(a + 1 == b);
		}
	}
}
