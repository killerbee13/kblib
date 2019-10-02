#include "kblib/stats.h"
#include "catch.hpp"
#include <array>
#include <iostream>
#include <cmath>

static long double phi = 1.6180339887498948482L;
static long double r5 = std::sqrtl(5);

TEST_CASE("fibonacci") {
	std::array<long long, 41> fibs{
		 0,       1,        1,        2,        3,        5,        8,
		 13,      21,       34,       55,       89,       144,      233,
		 377,     610,      987,      1597,     2584,     4181,     6765,
		 10946,   17711,    28657,    46368,    75025,    121393,   196418,
		 317811,  514229,   832040,   1346269,  2178309,  3524578,  5702887,
		 9227465, 14930352, 24157817, 39088169, 63245986, 102334155};
	for (int i{}; i != fibs.size(); ++i) {
		REQUIRE(fibs[i] == kblib::fibonacci(i));
	}
	for (int i{fibs.size()}; i < 93; ++i) {
		std::cout<<(kblib::fibonacci(i) - std::llroundl(std::powl(phi, i)/r5))<<'\n';
	}
}
