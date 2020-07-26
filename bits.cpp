#define KBLIB_DEF_MACROS 1
#include "kblib/bits.h"
#include "catch.hpp"

template <typename T>
struct print;

// These tests are POSIX-based, so spell out that requirement.
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
static_assert(
    std::is_same<kblib::uint_smallest_t<32768>, unsigned short>::value,
    "uint_smallest_t");
static_assert(
    std::is_same<kblib::uint_smallest_t<65535>, unsigned short>::value,
    "uint_smallest_t");
static_assert(std::is_same<kblib::uint_smallest_t<65536>, unsigned int>::value,
              "uint_smallest_t");
static_assert(
    std::is_same<kblib::uint_smallest_t<1u << 31>, unsigned int>::value,
    "uint_smallest_t");
static_assert(
    std::is_same<kblib::uint_smallest_t<UINT_MAX>, unsigned int>::value,
    "uint_smallest_t");
static_assert(
    std::is_same<kblib::uint_smallest_t<1ul << 32>, unsigned long>::value,
    "uint_smallest_t");
static_assert(
    std::is_same<kblib::uint_smallest_t<1ul << 63>, unsigned long>::value,
    "uint_smallest_t");

// print<kblib::int_smallest_t<1>> t{};
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
static_assert(
    std::is_same<kblib::int_smallest_t<long(INT_MAX) + 1>, signed long>::value,
    "int_smallest_t");

#if KBLIB_USE_CXX17
TEST_CASE("test_trie") {
	kblib::compact_bit_trie<unsigned short, 1024, int> test;
	REQUIRE(test.insert({0b1000100010000000, 10}, 1));
	// has UB, so I hardcoded a test failure as a reminder
	// std::cout<<test.at({0b1000100010000000, 10})<<'\n';
	REQUIRE(!"test.at({0b1000100010000000, 10}) has UB");
}
#endif

struct buffer {
	struct ret_proxy {
		const char* buf;
		template <typename T,
		          typename std::enable_if<std::is_trivially_copyable<T>::value,
		                                  int>::type = 0>
		operator T() const {
			T t;
			std::memcpy(&t, buf, sizeof(T));
			return t;
		}
	};
	std::vector<char> buf;
	ret_proxy operator[](std::size_t p) { return {&buf[p]}; }
};

using kblib::bitfield;

union Addr1 {
	bitfield<0, 16, uint16_t> raw{};

	bitfield<0, 5, uint16_t> cX;
	bitfield<5, 5, uint16_t> cY;
	bitfield<10, 2, uint16_t> nt;
	bitfield<12, 3, uint16_t> fY;

	bitfield<0, 8, uint16_t> l;
	bitfield<8, 7, uint16_t> h;

	bitfield<0, 14, uint16_t> addr;
};

TEST_CASE("bitfields1") {
	static_assert(sizeof(Addr1) == 2, "");
	Addr1 a;
	a.l = 0b1100'1100;
	a.h(0b001'1001);
	REQUIRE(a.raw == 0b0001'1001'1100'1100);
	REQUIRE(a.cX() == 0b0'1100);
	REQUIRE(a.cY == 0b0'1110);
	REQUIRE(a.nt == 0b10);
	REQUIRE(a.fY == 0b001);
}

// Loopy's VRAM address
struct Addr {
	std::uint16_t raw{};

	BITFIELD(0, 5, cX, raw)
	BITFIELD(5, 5, cY, raw)
	BITFIELD(10, 2, nt, raw)
	BITFIELD(12, 3, fY, raw)

	BITFIELD(0, 8, l, raw)
	BITFIELD(8, 7, h, raw)

	BITFIELD(0, 14, addr, raw)
};

constexpr Addr test_bitfield() {
	Addr a;
	a.l() = 0b1100'1100;
	a.h(0b001'1001);
	return a;
}

TEST_CASE("bitfields") {
	static_assert(sizeof(Addr) == 2, "");
	constexpr Addr a = test_bitfield();
	static_assert(a.raw == 0b0001'1001'1100'1100, "");
	static_assert(a.cX() == 0b0'1100, "");
	static_assert(a.cY() == 0b0'1110, "");
	static_assert(a.nt() == 0b10, "");
	static_assert(a.fY() == 0b001, "");

	static_assert(Addr::get_cX_v<a.raw> == 0b0'1100, "");
	static_assert(Addr::get_cY_v<a.raw> == 0b0'1110, "");
	static_assert(Addr::get_nt_v<a.raw> == 0b10, "");
	static_assert(Addr::get_fY_v<a.raw> == 0b001, "");

	constexpr Addr b = {Addr::set_h_v<0b001'1001, Addr::set_l_v<0b1100'1100>>};

	static_assert(Addr::get_cX_v<b.raw> == 0b0'1100, "");
	static_assert(Addr::get_cY_v<b.raw> == 0b0'1110, "");
	static_assert(Addr::get_nt_v<b.raw> == 0b10, "");
	static_assert(Addr::get_fY_v<b.raw> == 0b001, "");
}
