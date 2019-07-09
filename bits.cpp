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

TEST_CASE("test_trie") {
  kblib::compact_bit_trie<unsigned short, 1024, int> test;
  REQUIRE(test.insert({0b1000100010000000, 10}, 1));
  // has UB, so I hardcoded a test failure as a reminder
  // std::cout<<test.at({0b1000100010000000, 10})<<'\n';
  REQUIRE(!"test.at({0b1000100010000000, 10}) has UB");
}

struct buffer {
  struct ret_proxy {
    const char* buf;
    template <typename T, typename std::enable_if_t<
                              std::is_trivially_copyable_v<T>, int> = 0>
    operator T() const {
      T t;
      std::memcpy(&t, buf, sizeof(T));
      return t;
    }
  };
  std::vector<char> buf;
  ret_proxy operator[](std::size_t p) { return {&buf[p]}; }
};

template <typename Parent, typename Ret, Ret (Parent::*Set)(Ret) noexcept,
          Ret (Parent::*Get)() const noexcept>
struct bitfield_proxy {
  Parent* p;
  Ret operator=(Ret val) noexcept { return (p->*Set)(val); }
  operator Ret() const noexcept { return (p->*Get)(); }
};

#define BITFIELD(offset, size, name, raw)                                  \
 private:                                                                  \
  decltype(raw) name##_get_impl() const noexcept {                         \
    return (raw >> offset) & ((1 << size) - 1);                            \
  }                                                                        \
                                                                           \
 public:                                                                   \
  decltype(raw) name() const noexcept { return name##_get_impl(); }        \
                                                                           \
 private:                                                                  \
  decltype(raw) name##_set_impl(decltype(raw) val) noexcept {              \
    auto v = raw;                                                          \
    /* Clear the bits for this field */                                    \
    v &= ~(((1 << size) - 1) << offset);                                   \
    /* Set the field */                                                    \
    v |= (val & ((1 << size) - 1)) << offset;                              \
    raw = v;                                                               \
    return val;                                                            \
  }                                                                        \
                                                                           \
 public:                                                                   \
  decltype(raw) name(decltype(raw) val) noexcept {                         \
    return name##_set_impl(val);                                           \
  }                                                                        \
                                                                           \
  auto name() noexcept {                                                   \
    using Parent = std::remove_pointer<decltype(this)>::type;              \
    return bitfield_proxy<Parent, decltype(raw), &Parent::name##_set_impl, \
                          &Parent::name##_get_impl>{this};                 \
  }

// Loopy's VRAM address
struct Addr {
  std::uint16_t raw = 0;

  BITFIELD(0, 5, cX, raw)
  BITFIELD(5, 5, cY, raw)
  BITFIELD(10, 2, nt, raw)
  BITFIELD(12, 3, fY, raw)

  BITFIELD(0, 8, l, raw)
  BITFIELD(8, 7, h, raw)

  BITFIELD(0, 14, addr, raw)

  // bitfield<0, 5, uint16_t, Addr> cX;
  // bitfield<5, 5, uint16_t, Addr> cY;
  // bitfield<10, 2, uint16_t, Addr> nt;
  // bitfield<12, 3, uint16_t, Addr> fY;

  // bitfield<0, 8, uint16_t, Addr> l;
  // bitfield<8, 7, uint16_t, Addr> h;

  // bitfield<0, 14, uint16_t, Addr> addr;
};

TEST_CASE("bitfields") {
  REQUIRE(sizeof(Addr) == 2);
  Addr a;
  a.l() = 0b11001100;
  a.h(0b0011001);
  REQUIRE(a.raw == 0b0001100111001100);
  REQUIRE(a.cX() == 0b01100);
  REQUIRE(a.cY() == 0b01110);
  REQUIRE(a.nt() == 0b10);
  REQUIRE(a.fY() == 0b001);
}
