#define KBLIB_DEBUG_SEED_SEQ 1

#include "kblib/random.h"
#include "catch.hpp"

#include <iostream>
#include <sstream>

template <std::size_t>
struct print;

TEST_CASE("Random engine seeding") {
	SECTION("Mersenne Twister Engine") {
		std::clog << "MT: ";
		KBLIB_UNUSED auto gen = kblib::seeded<std::mt19937>();
		std::clog << '\n';
		static_assert(kblib::state_size_v<std::mt19937> == 624, ".");
		static_assert(kblib::state_size_v<std::mt19937_64> == 624, ".");
	}
	SECTION("Linear Congruential Engine") {
		std::clog << "LCG: ";
		KBLIB_UNUSED auto gen = kblib::seeded<std::minstd_rand>();
		std::clog << '\n';
		static_assert(kblib::state_size_v<std::minstd_rand> == 1, ".");
		static_assert(kblib::state_size_v<std::minstd_rand0> == 1, ".");
		std::clog << "LCG(64): ";
		KBLIB_UNUSED auto gen2 = kblib::seeded<kblib::lcgs::java_rand>();
		std::clog << '\n';
		static_assert(kblib::state_size_v<kblib::lcgs::java_rand> == 2, ".");
	}
	SECTION("Subtract with Carry Engine") {
		std::clog << "SwCE 24: ";
		KBLIB_UNUSED auto gen = kblib::seeded<std::ranlux24_base>();
		std::clog << "SwCE 48: ";
		KBLIB_UNUSED auto gen2 = kblib::seeded<std::ranlux24_base>();
		std::clog << '\n';
		static_assert(kblib::state_size_v<std::ranlux24_base> == 24, ".");
		static_assert(kblib::state_size_v<std::ranlux48_base> == 24, ".");
	}
	SECTION("Discard Block Engine") {
		std::clog << "DBE: ";
		KBLIB_UNUSED auto gen
		    = kblib::seeded<std::discard_block_engine<std::minstd_rand, 16, 8>>();
		std::clog << '\n';
		std::clog << "RANLUX 24: ";
		KBLIB_UNUSED auto gen2 = kblib::seeded<std::ranlux24>();
		std::clog << '\n';
		static_assert(
		    kblib::state_size_v<
		        std::discard_block_engine<std::minstd_rand, 16, 8>> == 1,
		    ".");
		static_assert(kblib::state_size_v<
		                  std::discard_block_engine<std::mt19937, 16, 8>> == 624,
		              ".");
		static_assert(kblib::state_size_v<std::ranlux24> == 24, ".");
	}
	SECTION("Independent Bits Engine") {
		std::clog << "IBE: ";
		KBLIB_UNUSED auto gen
		    = kblib::seeded<std::independent_bits_engine<std::ranlux24, 32,
		                                                 std::uint_fast32_t>>();
		std::clog << '\n';
		static_assert(kblib::state_size_v<std::independent_bits_engine<
		                      std::ranlux24, 32, std::uint_fast32_t>> == 24,
		              ".");
		static_assert(kblib::state_size_v<std::independent_bits_engine<
		                      std::mt19937, 32, std::uint_fast32_t>> == 624,
		              ".");
		static_assert(kblib::state_size_v<std::independent_bits_engine<
		                      std::minstd_rand, 32, std::uint_fast32_t>> == 1,
		              ".");
	}
	SECTION("Shuffle Order Engine") {
		std::clog << "SOE: ";
		KBLIB_UNUSED auto gen
		    = kblib::seeded<std::shuffle_order_engine<std::ranlux24, 32>>();
		std::clog << '\n';
		static_assert(kblib::state_size_v<
		                  std::shuffle_order_engine<std::ranlux24, 32>> == 24,
		              ".");
		static_assert(kblib::state_size_v<
		                  std::shuffle_order_engine<std::mt19937, 32>> == 624,
		              ".");
		static_assert(kblib::state_size_v<
		                  std::shuffle_order_engine<std::minstd_rand, 32>> == 1,
		              ".");
	}
}

TEST_CASE("transform_engine") {
	using E
	    = kblib::transform_engine<std::ranlux24,
	                              kblib::shift_mask<std::uint_fast32_t, 16u>>;
	auto gen = kblib::seeded<E>();
	static_assert(E::min() == 0, ".");
	static_assert(E::max() == ((std::ranlux24::max() >> 16u)), ".");
	SECTION("copying") {
		auto gen1 = gen;
		REQUIRE(gen == gen1);
	}
	SECTION("i/o") {
		decltype(gen) gen1, gen2;
		REQUIRE(gen1 == gen2);
		REQUIRE(gen != gen1);
		REQUIRE(gen != gen2);
		std::stringstream ss;
		ss << gen;
		ss >> gen1;
		REQUIRE(gen == gen1);
		REQUIRE(gen != gen2);
		REQUIRE(gen1 != gen2);
	}
	std::uniform_int_distribution<int> dist;
	(void)dist(gen);
}
