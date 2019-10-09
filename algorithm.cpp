#include "kblib/algorithm.h"
#include "catch.hpp"

#include <iostream>
#include <string>
#include <vector>

TEST_CASE("erase") {
	const auto equal = [](auto a, auto b) {
		return std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b));
	};
	SECTION("erase and erase_if") {
		const std::vector<int> erase_test{2, 2, 3, 4, 5, 7, 8, 11};
		const std::vector<int> no_2s = {3, 4, 5, 7, 8, 11};
		auto erase_copy = erase_test;
		kblib::erase(erase_copy, 2);
		REQUIRE(equal(no_2s, erase_copy));
		erase_copy = erase_test;
		const std::vector<int> no_evens = {3, 5, 7, 11};
		kblib::erase_if(erase_copy, [](int x) { return (~x) & 1; });
		REQUIRE(equal(no_evens, erase_copy));
	}
}

TEST_CASE("find family") {
	// TODO
}

TEST_CASE("get_max family") {
	// TODO
}

TEST_CASE("general algorithms") {
	// TODO
}

template <typename T, std::size_t N>
constexpr bool sort_test(std::array<T, N> val) noexcept {
	std::array<T, N> out{};
	kblib::insertion_sort_copy(val.begin(), val.end(), out.begin(), out.end());
	kblib::insertion_sort(val.begin(), val.end());
	return true;
}

TEST_CASE("sort") {
	constexpr std::array<int, 7> input{{3, 7, 4, 3, 1, 9, 5}};
	const auto goal = [&] {
		auto copy = input;
		std::sort(copy.begin(), copy.end());
		return copy;
	}();

	[[maybe_unused]] auto print_arr = [&](auto c) {
		for (const auto& v : c) {
			std::cout << v << ", ";
		}
		std::cout << '\n';
	};

	static_assert(sort_test(input), "insertion_sort should be constexpr");

	SECTION("insertion_sort") {
		auto input_copy = input;
		kblib::insertion_sort(input_copy.begin(), input_copy.end());
		REQUIRE(input_copy == goal);
		static_assert(noexcept(kblib::insertion_sort(
		                  input_copy.begin(), input_copy.end())),
		              "insertion_sort for array<int> should be noexcept");
	}
	SECTION("insertion_sort_copy") {
		std::remove_const<decltype(input)>::type output;
		kblib::insertion_sort_copy(input.begin(), input.end(), output.begin(),
		                           output.end());
		REQUIRE(output == goal);
		static_assert(noexcept(kblib::insertion_sort_copy(
		                  input.begin(), input.end(), output.begin(),
		                  output.end())),
		              "insertion_sort_copy for array<int> should be noexcept");
	}
	SECTION("adaptive_insertion_sort_copy") {
		std::remove_const<decltype(input)>::type output;
		kblib::adaptive_insertion_sort_copy(input.begin(), input.end(),
		                                    output.begin(), output.end());
		REQUIRE(output == goal);
	}
}

TEST_CASE("zip") {
	SECTION("non-overlapping") {
		std::vector<int> input1{1, 2, 3, 4, 5, 6};
		std::vector<short> input2{2, 3, 4, 5, 6, 7};

		for (auto t : kblib::zip(input1.begin(), input1.end(), input2.begin())) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			REQUIRE(a + 1 == b);
		}
	}
	SECTION("identical") {
		std::vector<int> input{1, 2, 3, 4, 5, 6, 7, 8};
		for (auto t : kblib::zip(input.begin(), input.end(), input.begin())) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			REQUIRE(&a == &b);
		}
	}
	SECTION("overlapping") {
		std::vector<int> input{1, 2, 3, 4, 5, 6, 7, 8};
		for (auto t :
		     kblib::zip(input.begin(), input.end() - 1, input.begin() + 1)) {
			auto& a = std::get<0>(t);
			auto& b = std::get<1>(t);
			REQUIRE(a + 1 == b);
		}
	}
}
