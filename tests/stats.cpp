#include "kblib/stats.h"

#include "catch2/catch.hpp"

#include <array>
#include <cmath>
#include <iostream>

[[gnu::unused]] static long double phi = 1.6180339887498948482L;
// not using sqrtl because of https://gcc.gnu.org/bugzilla/show_bug.cgi?id=79700
[[gnu::unused]] static long double r5 = std::sqrt(5);

TEST_CASE("fibonacci") {
	std::array<std::uint_least64_t, 41> fibs{
	    0,       1,        1,        2,        3,        5,        8,
	    13,      21,       34,       55,       89,       144,      233,
	    377,     610,      987,      1597,     2584,     4181,     6765,
	    10946,   17711,    28657,    46368,    75025,    121393,   196418,
	    317811,  514229,   832040,   1346269,  2178309,  3524578,  5702887,
	    9227465, 14930352, 24157817, 39088169, 63245986, 102334155};
	for (std::size_t i{}; i != fibs.size(); ++i) {
		REQUIRE(fibs[i] == kblib::fibonacci(static_cast<int>(i)));
	}
	/*for (int i{fibs.size()}; i < 93; ++i) {
	   std::cout<<(kblib::fibonacci(i) - std::llroundl(std::powl(phi,
	i)/r5))<<'\n';
	}//*/
}
