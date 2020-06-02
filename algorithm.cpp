#include "kblib/algorithm.h"
#include "catch.hpp"

#include "kblib/stats.h"

#include <iostream>
#include <map>
#include <set>
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
	//           0  1  2  3  4  5  6  7  8  9  e
	int vec[] = {0, 3, 4, 5, 6, 1, 2, 4, 4, 5};
	auto begin = std::begin(vec);
	auto end = std::end(vec);
	REQUIRE(kblib::find(begin, end, 4) == begin + 2);
	REQUIRE(kblib::find(begin, end, 10) == end);
	REQUIRE(kblib::find(begin, end, 0) == begin);
	REQUIRE(kblib::find(begin, end, 10) == end);

	REQUIRE(kblib::find_last(begin, end, 10) == end);
	REQUIRE(kblib::find_last(begin, end, 0) == begin);
	REQUIRE(kblib::find_last(begin, end, 4) == begin + 8);

	REQUIRE(kblib::find_if(begin, end, [](int i) { return i == 2; }) ==
	        begin + 6);
	REQUIRE(kblib::find_if_not(begin, end, [](int i) { return i == 2; }) ==
	        begin);
	REQUIRE(kblib::find_if(begin, end, [](int) { return false; }) == end);
	REQUIRE(kblib::find_if_not(begin, end, [](int) { return true; }) == end);

	REQUIRE(kblib::find_last_if(begin, end, [](int i) { return i == 2; }) ==
	        begin + 6);
	REQUIRE(kblib::find_last_if_not(begin, end, [](int i) { return i == 2; }) ==
	        begin + 9);
	REQUIRE(kblib::find_last_if(begin, end, [](int) { return false; }) == end);
	REQUIRE(kblib::find_last_if_not(begin, end, [](int) { return true; }) ==
	        end);
}
TEST_CASE("find_in") {
	//           0  1  2  3  4  5  6  7  8  9  e
	int vec[] = {0, 3, 4, 5, 6, 1, 2, 4, 4, 5};
	auto begin = std::begin(vec);
	auto end = std::end(vec);
	auto size = kblib::fakestd::size(vec);
	REQUIRE(kblib::find_in(begin, end, 4) == 2);
	REQUIRE(kblib::find_in(begin, end, 10) == size);
	REQUIRE(kblib::find_in(begin, end, 0) == 0);
	REQUIRE(kblib::find_in(begin, end, 10) == size);

	REQUIRE(kblib::find_last_in(begin, end, 10) == size);
	REQUIRE(kblib::find_last_in(begin, end, 0) == 0);
	REQUIRE(kblib::find_last_in(begin, end, 4) == 8);

	REQUIRE(kblib::find_in_if(begin, end, [](int i) { return i == 2; }) == 6);
	REQUIRE(kblib::find_in_if_not(begin, end, [](int i) { return i == 2; }) ==
	        0);
	REQUIRE(kblib::find_in_if(begin, end, [](int) { return false; }) == size);
	REQUIRE(kblib::find_in_if_not(begin, end, [](int) { return true; }) == size);

	REQUIRE(kblib::find_last_in_if(begin, end, [](int i) { return i == 2; }) ==
	        6);
	REQUIRE(kblib::find_last_in_if_not(begin, end,
	                                   [](int i) { return i == 2; }) == 9);
	REQUIRE(kblib::find_last_in_if(begin, end, [](int) { return false; }) ==
	        size);
	REQUIRE(kblib::find_last_in_if_not(begin, end, [](int) { return true; }) ==
	        size);
}

TEST_CASE("get_max family") {
	/// TODO: tests for get_max_*
}

TEST_CASE("general algorithms") {
	/// TODO: tests for other algorithms
}

TEST_CASE("assorted algorithms") {
	auto equal = [](auto r1, auto r2) {
		auto r1b = r1.begin();
		auto r1e = r1.end();
		auto r2b = r2.begin();
		auto r2e = r2.end();
		return (std::distance(r1b, r1e) == std::distance(r2b, r2e)) &&
		       kblib::equal(r1b, r1e, r2b);
	};
	std::array<int, 10> haystack{1, 1, 2, 5, 6, 2, 4, 7, 0, 6};
	const int N = 5;
	std::array<int, N> maxN_target{7, 6, 6, 5, 4};

	REQUIRE(equal(maxN_target, kblib::get_max_n<std::vector<int>>(
	                               haystack.begin(), haystack.end(), N)));
	std::vector<int> maxN_copy;
	kblib::get_max_n(haystack.begin(), haystack.end(),
	                 std::back_inserter(maxN_copy), 5);
	REQUIRE(equal(maxN_target, maxN_copy));

	REQUIRE(
	    equal(maxN_target, kblib::get_max_n<std::multiset<int, std::greater<>>>(
	                           haystack.begin(), haystack.end(), N)));

	std::vector<int> maxN_psc(N);
	std::partial_sort_copy(haystack.begin(), haystack.end(), maxN_psc.begin(),
	                       maxN_psc.end(), std::greater<>{});
	REQUIRE(equal(maxN_target, maxN_psc));

	// SFINAE prevents get_max_n from being called with invalid arguments,
	// even though count has moved from third to fourth on this overload.
	// It will also fail when the output range is not assignable.

	// kblib::get_max_n(haystack.begin(), haystack.end(), maxN.cbegin(),
	// 1); kblib::get_max_n(haystack.begin(), haystack.end(), 1,
	// maxN.begin());

	auto range = kblib::range(0, 50, 3);
	REQUIRE(equal(range, std::vector<int>{0, 3, 6, 9, 12, 15, 18, 21, 24, 27, 30,
	                                      33, 36, 39, 42, 45, 48}));
	auto maxN_range =
	    kblib::get_max_n<std::vector<int>>(range.begin(), range.end(), 5);
	REQUIRE(maxN_range == std::vector<int>{48, 45, 42, 39, 36});
}

template <typename T, std::size_t N>
constexpr bool sort_test(kblib::trivial_array<T, N> val) noexcept {
	kblib::trivial_array<T, N> out{};
	kblib::insertion_sort_copy(val.begin(), val.end(), out.begin(), out.end());
	kblib::insertion_sort(val.begin(), val.end());
	return true;
}

TEST_CASE("sort") {
	constexpr kblib::trivial_array<int, 7> input{{3, 7, 4, 3, 1, 9, 5}};
	const auto goal = [&] {
		auto copy = input;
		std::sort(copy.begin(), copy.end());
		return copy;
	}();

	[[gnu::unused]] auto print_arr = [&](auto c) {
		for (const auto& v : c) {
			std::cout << v << ", ";
		}
		std::cout << '\n';
	};

#if KBLIB_USE_CXX17
	static_assert(sort_test(input), "insertion_sort should be constexpr");
#endif

	SECTION("insertion_sort") {
		auto input_copy = input;
		kblib::insertion_sort(input_copy.begin(), input_copy.end());
		REQUIRE(input_copy == goal);
		static_assert(
		    noexcept(kblib::insertion_sort(input_copy.begin(), input_copy.end())),
		    "insertion_sort for array<int> should be noexcept");
	}
	SECTION("insertion_sort_copy") {
		std::remove_const<decltype(input)>::type output;
		kblib::insertion_sort_copy(input.begin(), input.end(), output.begin(),
		                           output.end());
		REQUIRE(output == goal);
		static_assert(
		    noexcept(kblib::insertion_sort_copy(input.begin(), input.end(),
		                                        output.begin(), output.end())),
		    "insertion_sort_copy for array<int> should be noexcept");
	}
	SECTION("adaptive_insertion_sort_copy") {
		std::remove_const<decltype(input)>::type output;
		kblib::adaptive_insertion_sort_copy(input.begin(), input.end(),
		                                    output.begin(), output.end());
		REQUIRE(output == goal);
	}
	SECTION("insertion_sort is stable") {
		std::minstd_rand rng;
		std::uniform_int_distribution<int> dist(0, 8);
		for ([[gnu::unused]] auto _i : kblib::range(100)) {
			// sort based on first key, second is used to distinguish between equal
			// elements
			std::vector<std::pair<int, int>> inputs;
			auto pcomp = [](auto a, auto b) { return a.first < b.first; };

			{
				std::map<int, int> counts;
				for ([[gnu::unused]] auto _j : kblib::range(100)) {
					auto r = dist(rng);
					inputs.push_back({r, counts[r]++});
				}
			}

			kblib::insertion_sort(inputs.begin(), inputs.end(), pcomp);

			{
				std::map<int, int> counts;
				for (auto p : inputs) {
					REQUIRE(p.second == counts[p.first]++);
				}
			}
		}
	}
	SECTION("insertion_sort on random data") {
		std::minstd_rand rng;
		std::uniform_int_distribution<int> dist(0, 65535);
		for ([[gnu::unused]] auto _i : kblib::range(100)) {
			std::vector<int> input;
			std::generate_n(std::back_inserter(input), 100,
			                [&] { return dist(rng); });

			auto output_std = input;
			std::sort(output_std.begin(), output_std.end());
			decltype(input) output_kblib(input.size());
			kblib::insertion_sort_copy(input.cbegin(), input.cend(),
			                           output_kblib.begin(), output_kblib.end());
			REQUIRE(output_std == output_kblib);
		}
	}
}

TEST_CASE("insertion sort performance") {
	SECTION("insertion_sort_copy on sorted data is fast") {
		auto time_per = [](std::size_t size) {
			std::vector<int> input(size);
			decltype(input) output(size);
			std::iota(input.begin(), input.end(), 0);

			auto start = std::chrono::high_resolution_clock::now();
			kblib::insertion_sort_copy(input.cbegin(), input.cend(),
			                           output.begin(), output.end());
			auto end = std::chrono::high_resolution_clock::now();
			auto duration = end - start;
			return static_cast<double>(duration.count());
		};

		auto time_fast = time_per(30) / 30;
		double time_slow = time_per(10000) / 10000;
		double error = time_slow / time_fast;
		std::cout << __LINE__ << ": " << time_fast << '\t' << time_slow << '\t'
		          << error << '\t' << time_slow * 10000 << '\t' << time_fast * 30
		          << "\n";

		// Can't overshoot the bound by more than 5%:
		REQUIRE(error < 1.05);
	}
	SECTION("insertion_sort_copy on reverse sorted data is slow") {
		auto time_per = [](std::size_t size) {
			std::vector<int> input(size);
			decltype(input) output(size);
			std::iota(input.rbegin(), input.rend(), 0);

			auto start = std::chrono::high_resolution_clock::now();
			kblib::insertion_sort_copy(input.cbegin(), input.cend(),
			                           output.begin(), output.end());
			auto end = std::chrono::high_resolution_clock::now();
			auto duration = end - start;
			return static_cast<double>(duration.count());
		};

		auto time_fast = time_per(30) / (30 * 30);
		auto time_slow = time_per(1000) / (1000 * 1000);
		auto error = time_slow / time_fast;
		std::cout << __LINE__ << ": " << time_fast << '\t' << time_slow << '\t'
		          << error << '\t' << time_slow * 1000 * 1000 << '\t'
		          << time_fast * 30 * 30 << "\n";

		// Can't overshoot the bound by more than 5%:
		REQUIRE(error < 1.05);
	}
	SECTION("adaptive_insertion_sort_copy on sorted data is fast") {
		auto time_per = [](std::size_t size) {
			std::vector<int> input(size);
			decltype(input) output(size);
			std::iota(input.begin(), input.end(), 0);

			auto start = std::chrono::high_resolution_clock::now();
			kblib::adaptive_insertion_sort_copy(input.cbegin(), input.cend(),
			                                    output.begin(), output.end());
			auto end = std::chrono::high_resolution_clock::now();
			auto duration = end - start;
			return static_cast<double>(duration.count());
		};

		auto time_fast = time_per(30) / (30);
		auto time_slow = time_per(10'000) / (10'000);
		double error = time_slow / time_fast;
		std::cout << __LINE__ << ": " << time_fast << '\t' << time_slow << '\t'
		          << error << '\t' << time_slow * 10'000 << '\t' << time_fast * 30
		          << "\n";

		// Can't overshoot the bound by more than 5%:
		REQUIRE(error < 1.05);
	}
	SECTION("adaptive_insertion_sort_copy on reverse sorted data is fast") {
		auto time_per = [](std::size_t size) {
			std::vector<int> input(size);
			decltype(input) output(size);
			std::iota(input.rbegin(), input.rend(), 0);

			auto start = std::chrono::high_resolution_clock::now();
			kblib::adaptive_insertion_sort_copy(input.cbegin(), input.cend(),
			                                    output.begin(), output.end());
			auto end = std::chrono::high_resolution_clock::now();
			auto duration = end - start;
			return static_cast<double>(duration.count());
		};

		auto time_fast = time_per(30) / (30);
		auto time_slow = time_per(10'000) / (10'000);
		double error = time_slow / time_fast;
		std::cout << __LINE__ << ": " << time_fast << '\t' << time_slow << '\t'
		          << error << '\t' << time_slow * 10'000 << '\t' << time_fast * 30
		          << "\n";

		// Can't overshoot the bound by more than 5%:
		REQUIRE(error < 1.05);
	}
	SECTION("insertion_sort_copy on mostly sorted data is fast") {
		std::minstd_rand rng;
		auto time_per = [&](std::size_t size, int noise) {
			std::uniform_int_distribution<int> dist(-noise, noise);
			std::vector<int> input(size);
			decltype(input) output(size);
			for (auto i : kblib::range(size)) {
				input[i] = i + dist(rng);
			}

			auto start = std::chrono::high_resolution_clock::now();
			kblib::insertion_sort_copy(input.cbegin(), input.cend(),
			                           output.begin(), output.end());
			auto end = std::chrono::high_resolution_clock::now();
			auto duration = end - start;
			return static_cast<double>(duration.count());
		};

		auto n = 10000;
		auto v = 250;
		auto time_fast = time_per(n, 0) / (n);
		auto time_slow = time_per(n, v) / (n);
		auto ratio = time_slow / time_fast;
		auto error = ratio / (n / v);
		std::cout << __LINE__ << ": " << time_fast << '\t' << time_slow << '\t'
		          << error << '\t' << time_slow * n << '\t' << time_fast * n
		          << "\n";

		// Can't overshoot the bound by more than 5%:
		REQUIRE(error < 1.05);

		// This test causes spurious failures in Debug builds:
		// error = 1.19 in Debug build
		// error = 0.76 in Release build
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
