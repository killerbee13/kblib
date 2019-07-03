#include "kblib/bits.h"
#include "catch.hpp"

template <typename T>
struct print;

//These tests are POSIX-based, so spell out that requirement.
static_assert(CHAR_BIT == 8, "8-bit bytes");
static_assert(sizeof(short) == 2, "16-bit shorts");
static_assert(sizeof(int) == 4, "32-bit ints");
static_assert(sizeof(long) == 8, "64-bit longs");


static_assert(std::is_same<kblib::uint_smallest_t<0>, unsigned char>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1>, unsigned char>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<255>, unsigned char>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<256>, unsigned short>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<32768>, unsigned short>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<65535>, unsigned short>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<65536>, unsigned int>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1u<<31>, unsigned int>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<UINT_MAX>, unsigned int>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1ul<<32>, unsigned long>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1ul<<63>, unsigned long>::value,
              "uint_smallest_t");

//print<kblib::int_smallest_t<1>> t{};
static_assert(std::is_same<kblib::int_smallest_t<1>, signed char>::value,
              "int_smallest_t");
static_assert(std::is_same<kblib::int_smallest_t<127>, signed char>::value,
              "int_smallest_t");
static_assert(std::is_same<kblib::int_smallest_t<256>, signed short>::value,
              "int_smallest_t");
static_assert(std::is_same<kblib::int_smallest_t<32767>, signed short>::value,
              "int_smallest_t");
static_assert(std::is_same<kblib::int_smallest_t<32768>, signed int>::value,
              "int_smallest_t");
static_assert(std::is_same<kblib::int_smallest_t<65536>, signed int>::value,
              "int_smallest_t");
static_assert(std::is_same<kblib::int_smallest_t<INT_MAX>, signed int>::value,
              "int_smallest_t");
static_assert(std::is_same<kblib::int_smallest_t<long(INT_MAX) + 1>, signed long>::value,
              "int_smallest_t");

TEST_CASE("test_trie") {
  kblib::compact_bit_trie<unsigned short, 1024, int> test;
  REQUIRE(test.insert({0b1000100010000000, 10}, 1));
  //has UB, so I hardcoded a test failure as a reminder
  //std::cout<<test.at({0b1000100010000000, 10})<<'\n';
  REQUIRE(!"test.at({0b1000100010000000, 10}) has UB");
}
