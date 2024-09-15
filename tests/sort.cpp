#include "catch2/catch.hpp"

#include "kblib/sort.h"

#include "kblib/stats.h"
#include "kblib/stringops.h"
#include <iomanip>
#include <iostream>
#include <map>

template <typename T, std::size_t N>
constexpr auto sort_test(kblib::trivial_array<T, N> val) noexcept -> bool {
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

	KBLIB_UNUSED auto print_arr = [&](auto c) {
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
		CHECK(input_copy == goal);
		static_assert(
		    noexcept(kblib::insertion_sort(input_copy.begin(), input_copy.end())),
		    "insertion_sort for array<int> should be noexcept");
	}
	SECTION("insertion_sort_copy") {
		std::remove_const<decltype(input)>::type output;
		kblib::insertion_sort_copy(input.begin(), input.end(), output.begin(),
		                           output.end());
		CHECK(output == goal);
		static_assert(
		    noexcept(kblib::insertion_sort_copy(input.begin(), input.end(),
		                                        output.begin(), output.end())),
		    "insertion_sort_copy for array<int> should be noexcept");
	}
	SECTION("adaptive_insertion_sort_copy") {
		std::remove_const<decltype(input)>::type output;
		kblib::adaptive_insertion_sort_copy(input.begin(), input.end(),
		                                    output.begin(), output.end());
		CHECK(output == goal);
	}
	SECTION("insertion_sort is stable") {
		std::minstd_rand rng{std::random_device{}()};
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
					inputs.emplace_back(r, counts[r]++);
				}
			}

			kblib::insertion_sort(inputs.begin(), inputs.end(), pcomp);

			{
				std::map<int, int> counts;
				for (auto p : inputs) {
					CHECK(p.second == counts[p.first]++);
				}
			}
		}
	}
	SECTION("insertion_sort on random data") {
		std::minstd_rand rng{std::random_device{}()};
		std::uniform_int_distribution<int> dist(0, 65535);
		for ([[gnu::unused]] auto _i : kblib::range(100)) {
			std::vector<int> input2(100);
			std::generate(begin(input2), end(input2), [&] { return dist(rng); });

			auto output_std = input2;
			std::sort(output_std.begin(), output_std.end());
			decltype(input2) output_kblib(input2.size());
			kblib::insertion_sort_copy(cbegin(input2), cend(input2),
			                           output_kblib.begin(), output_kblib.end());
			CHECK(output_std == output_kblib);
		}
	}
}

static std::ostream& log_location = std::cout;

auto linear(std::size_t i) { return static_cast<double>(i); }

TEST_CASE("insertion sort performance") {
	auto time_and_log
	    = [&](auto line, auto&& f, std::size_t quick = 30,
	          std::size_t slow = 10000, double (*O)(std::size_t) = linear) {
		      auto time_fast = f(quick) / O(quick);
		      auto time_slow = f(slow) / O(slow);
		      auto error = time_slow / time_fast;
		      log_location << __FILE__ ":" << std::left << line << ": \t"
		                   << time_fast << "\t " << std::setw(12) << time_slow
		                   << '\t' << std::setw(14) << error << '\t'
		                   << time_slow * 10000 << '\t' << time_fast * 30 << "\n";

		      // Can't overshoot the bound by more than 5%:
		      CHECK(error < 1.05);
	      };
#define TIME(...) time_and_log(__LINE__, __VA_ARGS__)

	SECTION("labels") {
		using namespace std::literals;
		log_location << __FILE__ ":" << std::left << __LINE__
		             << ": \t/el (30)\t /el (10000)"
		                "\tsuperlinearity\ttotal (10000)\t"
		                "total (30)\n"
		             << kblib::repeat(" -"s, 60) << '\n';
	}

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

		TIME(time_per);
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

		TIME(
		    time_per, 30, 1000, +[](std::size_t i) {
			    return static_cast<double>(i) * static_cast<double>(i);
		    });
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

		TIME(time_per);
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

		TIME(time_per);
	}
	SECTION("insertion_sort_copy on mostly sorted data is fast") {
		std::minstd_rand rng{std::random_device{}()};
		auto time_per = [&](int ssize, int noise) {
			auto size = static_cast<std::size_t>(ssize);
			std::uniform_int_distribution<int> dist(-noise, noise);
			std::vector<int> input(size);
			decltype(input) output(size);
			for (auto i : kblib::range(ssize)) {
				input[kblib::to_unsigned(i)] = i + dist(rng);
			}

			auto start = std::chrono::high_resolution_clock::now();
			kblib::insertion_sort_copy(input.cbegin(), input.cend(),
			                           output.begin(), output.end());
			auto end = std::chrono::high_resolution_clock::now();
			auto duration = end - start;
			return static_cast<double>(duration.count());
		};

		auto n = 10000;
		// deviation by +/- sqrt(n)
		auto v = 100;

		using namespace std::literals;
		log_location << kblib::repeat(" -"s, 60) << '\n'
		             << __FILE__ ":" << std::left << __LINE__
		             << ": \t/el (s) \t /el (v)    "
		                "\tsuperlinearity\ttotal (v)    \t"
		                "total (s)\n"
		             << kblib::repeat(" -"s, 60) << '\n';

		auto time_fast = time_per(n, 0) / n;
		auto time_slow = time_per(n, v) / n;
		auto ratio = time_slow / time_fast;
		auto error = ratio / (n / v);

		log_location << __FILE__ ":" << std::left << __LINE__ << ": \t"
		             << time_fast << "\t " << std::setw(12) << time_slow << '\t'
		             << std::setw(14) << error << '\t' << time_slow * 10000
		             << '\t' << time_fast * 30 << "\n";

		// Can't overshoot the bound by more than 5%:
		CHECK(error < 1.05);
	}
	log_location << std::flush;
#undef TIME
}

TEST_CASE("byte extraction") {
	constexpr std::uint32_t x{0xAB23CD67};

	static_assert(std::numeric_limits<std::uint32_t>::digits
	                  == sizeof(uint32_t) * CHAR_BIT,
	              "these checks assume uint32_t does not have padding.");
	static_assert(CHAR_BIT == 8, "these checks assume 8-bit bytes");

	static_assert(kblib::byte_count(x) == sizeof(x), "");
	static_assert(+kblib::get_byte_index(x, 0) == 0x67, "");
	static_assert(+kblib::get_byte_index(x, 1) == 0xCD, "");
	static_assert(+kblib::get_byte_index(x, 2) == 0x23, "");
	static_assert(+kblib::get_byte_index(x, 3) == 0xAB, "");

	std::string str{"0123456789"};
	CHECK(kblib::byte_count(str) == str.length());
	for (auto i : kblib::range(str.length())) {
		CHECK((kblib::get_byte_index(str, i)) == str[i]);
	}

	constexpr std::array<std::int32_t, 2> arr{0x10325476,
	                                          std::int32_t(0x98BADCFE)};
	static_assert(kblib::byte_count(arr) == 8, "");
	static_assert(+kblib::get_byte_index(arr, 0) == 0x76, "");
	static_assert(+kblib::get_byte_index(arr, 1) == 0x54, "");
	static_assert(+kblib::get_byte_index(arr, 2) == 0x32, "");
	static_assert(+kblib::get_byte_index(arr, 3) == 0x10, "");
	static_assert(+kblib::get_byte_index(arr, 4) == 0xFE, "");
	static_assert(+kblib::get_byte_index(arr, 5) == 0xDC, "");
	static_assert(+kblib::get_byte_index(arr, 6) == 0xBA, "");
	static_assert(+kblib::get_byte_index(arr, 7) == 0x98, "");
}
