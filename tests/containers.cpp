#include "kblib/containers.h"
#include "catch2/catch.hpp"
#include "kblib/algorithm.h"

#include <iostream>
#include <map>

TEST_CASE("try_get") {
	std::map<int, int> m;
	const auto& cm = m;

	// m[0] doesn't exist
	REQUIRE(kblib::try_get(m, 0) == nullptr);
	// Works for const maps
	REQUIRE(kblib::try_get(cm, 0) == nullptr);
	// Make m[0]
	m[0] = 0;

	// Basic correctness testing
	REQUIRE(kblib::try_get(m, 0) == &m[0]);
	*kblib::try_get(m, 0) = 1;
	REQUIRE(m[0] == 1);

	// Returns pointer to const when given const map
	//*kblib::try_get(cm, 0) = 1;

	// Can't call for temporaries because it would return a dangling pointer
	// kblib::try_get(std::map<int, int>{{0, 1}}, 0);
}

TEST_CASE("build_iterator", "[build]") {
	using arr = std::array<int, 8>;
	const arr input = {2, 3, 5, 7, 11, 13, 17, 19};
	const arr input2 = {1, 1, 1, 1, 1, 1, 1, 1};
	const auto unary_f = [](int x) { return x * x; };
	const auto binary_f = [](int a, int b) { return a - b; };
	const arr squared = {4, 9, 25, 49, 121, 169, 289, 361};
	const arr pminusone = {1, 2, 4, 6, 10, 12, 16, 18};

	const auto equal = [](auto a, auto b) {
		return std::equal(std::begin(a), std::end(a), std::begin(b), std::end(b));
	};
	[[gnu::unused]] auto print_arr = [&](auto c) {
		for (const auto& v : c) {
			std::cout << v << ", ";
		}
		std::cout << '\n';
	};

	SECTION("unary dynamic build") {
		// auto built = kblib::build<std::vector<int>>(input.begin(), input.end(),
		// unary_f);
		auto built
		    = std::transform(input.begin(), input.end(),
		                     kblib::build_iterator<std::vector<int>>{}, unary_f)
		          .base();
		REQUIRE(equal(squared, built));
	}
	SECTION("binary dynamic build") {
		// auto built = kblib::build<std::vector<int>>(input.begin(), input.end(),
		// input2.begin(), binary_f);
		auto built
		    = std::transform(input.begin(), input.end(), input2.begin(),
		                     kblib::build_iterator<std::vector<int>>{}, binary_f)
		          .base();
		REQUIRE(equal(pminusone, built));
	}
	SECTION("unary array build") {
		// auto built = kblib::build<arr>(input.begin(), input.end(), unary_f);
		auto built = std::transform(input.begin(), input.end(),
		                            kblib::build_iterator<arr>{}, unary_f)
		                 .base();
		REQUIRE(equal(squared, built));
	}
	SECTION("binary array build") {
		// auto built = kblib::build<arr>(input.begin(), input.end(),
		// input2.begin(), binary_f);
		auto built = std::transform(input.begin(), input.end(), input2.begin(),
		                            kblib::build_iterator<arr>{}, binary_f)
		                 .base();
		REQUIRE(equal(pminusone, built));
	}
	const arr iota = {0, 1, 2, 3, 4, 5, 6, 7};
	SECTION("dynamic generator build") {
		// auto built = kblib::build<std::vector<int>>([x = 0]() mutable { return
		// x++; }, 8);
		auto built = std::generate_n(kblib::build_iterator<std::vector<int>>{}, 8,
		                             [x = 0]() mutable { return x++; })
		                 .base();
		REQUIRE(equal(iota, built));
	}
	SECTION("array generator build") {
		// auto built = kblib::build<arr>([x = 0]() mutable { return x++; });
		auto built
		    = kblib::generate(kblib::build_iterator<arr>{}, kblib::build_end,
		                      [x = 0]() mutable { return x++; })
		          .base();
		// Using only the standard library, it would have to be this in order to
		// avoid a magic number for the size.
		//		auto built = std::generate_n(kblib::build_iterator<arr>{},
		//		                             std::tuple_size<arr>::value,
		//		                             [x = 0]() mutable { return x++; })
		//		                 .base();
		REQUIRE(equal(iota, built));
	}
	// std::iota returns void, not the iterator
	SECTION("dynamic buildiota") {
		// auto built = kblib::buildiota<std::vector<int>>(8, 0);
		// auto built = ...
		auto built
		    = kblib::construct_from_range<std::vector<int>>(kblib::range(0, 8));
		REQUIRE(equal(iota, built));
	}
	SECTION("array buildiota") {
		// auto built = kblib::buildiota<arr>(0);
		// auto built = ...
		// REQUIRE(equal(iota, built));
	}
}

TEST_CASE("build_iterator is nodiscard") {
	using arr = std::array<int, 8>;
	KBLIB_UNUSED const arr input = {2, 3, 5, 7, 11, 13, 17, 19};

	// This line correctly generates a warning that the return value is ignored.
	// std::copy(input.begin(), input.end(), kblib::build_iterator<arr>{});
}
