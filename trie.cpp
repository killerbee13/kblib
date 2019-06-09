#include "kblib_bits.h"

template <typename T>
struct print;

static_assert(std::is_same<kblib::uint_smallest_t<0>, unsigned char>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1>, unsigned char>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<255>, unsigned char>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<256>, unsigned short>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<65535>, unsigned short>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<65536>, unsigned int>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1u<<31>, unsigned int>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1ul<<32>, unsigned long>::value,
              "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<1ul<<63>, unsigned long>::value,
              "uint_smallest_t");

void test_trie() {
  kblib::compact_bit_trie<unsigned short, 1024, int> test;
  test.insert({0b1000100010001000, 16}, 1);
}
