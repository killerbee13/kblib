#include "catch.hpp"

#include "kblib/format.h"
#include "kblib/iterators.h"
#include "kblib/stringops.h"

#include <iostream>
#include <string_view>

#if KBLIB_USE_CXX17
template <typename T>
auto pred_count_digits(T v) {
	return Catch::Predicate<T>(
	    [](T v_) -> bool {
		    return kblib::count_digits(v_)
		           == kblib::to_signed(std::to_string(v_).length());
	    },
	    kblib::concat(v, ": digits=", std::to_string(v).length(),
	                  "; calc=", kblib::count_digits(v)));
}
#	define REQUIRE_DIGITS(v) REQUIRE_THAT((v), pred_count_digits((v)))

template <typename T, typename U>
auto test_count_digits_range(kblib::range_t<T, U> r) {
	//	std::clog << kblib::concat("[", *r.begin(), ", ", *r.end(), ") / ",
	//	                           r.begin().step, "\n");
	for (auto v : r) {
		// std::clog << v << ' ';
		REQUIRE_DIGITS(v);
		for (auto d : kblib::range(1, 100)) {
			REQUIRE_DIGITS(v + v / d);
		}
	}
	// std::clog << '\n';
}

TEST_CASE("count_digits") {
	using namespace std::literals;
	SECTION("signed") {
		CHECK(kblib::count_digits(0) == 1);
		CHECK(kblib::count_digits(1) == 1);
		CHECK(kblib::count_digits(3) == 1);
		CHECK(kblib::count_digits(10) == 2);
		CHECK(kblib::count_digits(42) == 2);
		CHECK(kblib::count_digits(99) == 2);
		CHECK(kblib::count_digits(-1) == 2);
		CHECK(kblib::count_digits(std::numeric_limits<std::int32_t>::max())
		      == "2147483647"sv.length());
		CHECK(kblib::count_digits(std::numeric_limits<std::int32_t>::min())
		      == "-2147483648"sv.length());

		for (auto i : kblib::range(std::numeric_limits<int>::digits - 1)) {
			auto v = 1 << i;
			// std::clog << v << "(1<<" << i << ") ";
			REQUIRE_DIGITS(v);
			for (auto d : kblib::range(1, 100)) {
				REQUIRE_DIGITS(v + v / d);
				REQUIRE(kblib::count_digits(v + v / d)
				        == kblib::to_signed(std::to_string(v + v / d).length()));
			}
		}
		// std::clog << '\n';

		{
			std::int32_t tens = 10;
			for (auto prime : {7, 71, 701, 7001, 70'001, 700'001, 7'000'003}) {
				test_count_digits_range(kblib::range(tens, tens * 100, prime));
				tens *= 10;
			}
		}
		for (auto v : kblib::range(std::numeric_limits<int>::max() - 1000000,
		                           std::numeric_limits<int>::max() - 541, 541)) {
			// std::clog << v << ' ';
			REQUIRE_DIGITS(v);
			REQUIRE(kblib::count_digits(v)
			        == kblib::to_signed(std::to_string(v).length()));
		}
		// std::clog << '\n';
	}

	SECTION("unsigned") {
		CHECK(kblib::count_digits(0u) == 1);
		CHECK(kblib::count_digits(1u) == 1);
		CHECK(kblib::count_digits(3u) == 1);
		CHECK(kblib::count_digits(10u) == 2);
		CHECK(kblib::count_digits(42u) == 2);
		CHECK(kblib::count_digits(99u) == 2);
		CHECK(kblib::count_digits(std::uint16_t(-1)) == "65535"sv.length());
		CHECK(kblib::count_digits(std::uint32_t(-1)) == "4294967295"sv.length());
		CHECK(kblib::count_digits(std::uint64_t(-1))
		      == "18446744073709551615"sv.length());
	}

	SECTION("floating-point") {
		CHECK(kblib::count_digits(1.0) == 1);
		CHECK(kblib::count_digits(10.0) == 2);
		CHECK(kblib::count_digits(10.1) == 4);
	}
}
#endif
