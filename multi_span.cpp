#include "kblib/multi_span.h"
#include "catch.hpp"

#include <array>
#include <sstream>

TEST_CASE("multi_span") {
	std::array<std::array<int, 10>, 10> arrs{};
	kblib::multi_span<int> span{arrs.begin(), arrs.end()};
	REQUIRE(std::distance(span.begin(), span.end()) == 10 * 10);
}

#if 0
TEST_CASE("input iterators") {
	std::istringstream i1("0 1 2 3 4 5 6 7 8 9");
	std::istringstream i2("9 8 7 6 5 4 3 2 1 0");
	kblib::multi_span<int> span{
		 {std::istream_iterator<int>(i1), std::istream_iterator<int>()},
		 {std::istream_iterator<int>(i2), std::istream_iterator<int>()}};
}
#endif
