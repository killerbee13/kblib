#include "kblib/hash.h"

#include "catch2/catch.hpp"

#include <deque>
#include <map>
#include <set>
#include <unordered_map>

struct has_padding {
	char c;
	int i;
};

struct no_padding {
	int i, j;
};

struct empty_t {};

static_assert(kblib::is_linear_container_v<std::string>
                  and kblib::is_contiguous_v<std::string>
                  and kblib::is_trivially_hashable_v<std::string::value_type>
                  and kblib::is_trivially_hashable_v<int*>,
              "");
static_assert(kblib::asserts::is_trivial_container<std::vector<char>>, "");

#if KBLIB_USE_CXX17
static_assert(kblib::is_trivially_hashable_v<no_padding>);
static_assert(std::is_same_v<decltype(kblib::FNV_hash<no_padding>{}(
                                 std::declval<no_padding&>())),
                             std::size_t>);
static_assert(std::is_default_constructible_v<kblib::FNV_hash<no_padding>>
              and std::is_invocable_r_v<
                  std::size_t, kblib::FNV_hash<no_padding>, const no_padding&>);
#endif

template <typename T>
static constexpr bool is_disabled_hash
    = not (std::is_nothrow_default_constructible_v<kblib::FNV_hash<T>>
           and std::is_invocable_v<kblib::FNV_hash<T>, const T&>);
template <typename T>
static constexpr bool is_enabled_hash
    = std::is_nothrow_default_constructible_v<kblib::FNV_hash<T>>
      and std::is_invocable_r_v<std::size_t, kblib::FNV_hash<T>, const T&>;

TEST_CASE("FNV_hash") {
	(void)kblib::FNV_hash<int*>{}({});
	(void)kblib::FNV_hash<std::string>{}({});
	(void)kblib::FNV_hash<std::vector<char>>{}({});
	(void)kblib::FNV_hash<std::vector<int>>{}({});
	(void)kblib::FNV_hash<std::deque<char>>{}({});
	(void)kblib::FNV_hash<std::tuple<>>{}({});
	(void)kblib::FNV_hash<std::tuple<int*>>{}({});
	(void)kblib::FNV_hash<std::tuple<std::wstring, int*>>{}({});
	(void)kblib::FNV_hash<std::string::iterator>{}({});
	(void)kblib::FNV_hash<std::vector<int>::iterator>{}({});
	(void)kblib::FNV_hash<std::deque<char>::iterator>{}({});
	(void)kblib::FNV_hash<std::set<int>::iterator>{}({});

	std::unordered_map<std::tuple<std::wstring, int*>,
	                   std::vector<std::array<bool, 16>>, kblib::FNV_hash<>,
	                   std::equal_to<>>
	    test_map;
	using map_t = kblib::hash_map<std::tuple<std::wstring, int*>,
	                              std::vector<std::array<bool, 16>>>;
	static_assert(std::is_same<decltype(test_map), map_t>::value, "");
	KBLIB_UNUSED kblib::FNV_hash<std::map<int, int>> test_hash1;
	KBLIB_UNUSED kblib::FNV_hash<std::tuple<int, int*>> test_hash1a;
	KBLIB_UNUSED kblib::FNV_hash<std::pair<int* const, int>> test_hash1b;
	KBLIB_UNUSED kblib::FNV_hash<std::pair<const std::tuple<int, int*>, int>>
	    test_hash1c;
	KBLIB_UNUSED kblib::FNV_hash<std::map<std::tuple<std::wstring, int*>,
	                                      std::vector<std::array<bool, 16>>>>
	    test_hash2;
	KBLIB_UNUSED kblib::FNV_hash<std::array<std::array<bool, 16>, 4>> test_hash3;
	KBLIB_UNUSED auto call = &decltype(test_hash1)::operator();
	//	std::hash<
	//	    std::unordered_map<std::wstring,
	// std::vector<std::array<bool, 16>>, std::hash<std::wstring>>> 	    std_hash;
	using namespace kblib::literals;
	kblib::FNV_hash<std::uint64_t> h_i;
	static_assert(3452452_fnv64 == h_i(3452452ull),
	              "hash literal and FNH_hash don't agree");

	CHECK(kblib::FNV_hash<int>{}(1000) == 0x2fa9eaf82259d71cu);
	(void)kblib::FNV_hash<std::vector<int>>{}({});
	(void)kblib::FNV_hash<std::vector<empty_t>>{}({});
	// fails because of padding:
	static_assert(is_disabled_hash<std::vector<has_padding>>);
	// fails because of unorderedness:
	static_assert(is_disabled_hash<std::unordered_map<int, int>>);

	kblib::FNV_hash<int has_padding::*> test_hash4;
	CHECK(test_hash4(nullptr) != test_hash4(&has_padding::i));
#if KBLIB_USE_CXX17
	kblib::FNV_hash<std::optional<int>> test_opt_hash;
	(void)test_opt_hash(std::nullopt);
	(void)test_opt_hash(42);
	CHECK(test_opt_hash(42) == kblib::FNV_hash<int>{}(42));

	std::variant<int, int, std::string> var(std::in_place_index<1>, 42);
	kblib::FNV_hash<decltype(var)> test_var_hash;
	CHECK_FALSE(test_var_hash(var) == kblib::FNV_hash<int>{}(42));
	CHECK_FALSE(test_var_hash(var)
	            == test_var_hash(decltype(var){std::in_place_index<0>, 42}));
#endif
}
TEST_CASE("FNV_hash (32 bit)") {
	(void)kblib::FNV_hash<int*, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::string, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::vector<char>, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::vector<int>, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::deque<char>, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::tuple<>, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::tuple<int*>, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::tuple<std::wstring, int*>, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::string::iterator, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::vector<int>::iterator, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::deque<char>::iterator, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::set<int>::iterator, std::uint32_t>{}({});

	KBLIB_UNUSED kblib::FNV_hash<std::map<int, int>, std::uint32_t> test_hash1;
	KBLIB_UNUSED kblib::FNV_hash<std::tuple<int, int*>, std::uint32_t>
	    test_hash1a;
	KBLIB_UNUSED kblib::FNV_hash<std::pair<int* const, int>, std::uint32_t>
	    test_hash1b;
	KBLIB_UNUSED kblib::FNV_hash<std::pair<const std::tuple<int, int*>, int>,
	                             std::uint32_t>
	    test_hash1c;
	KBLIB_UNUSED
	kblib::FNV_hash<std::map<std::tuple<std::wstring, int*>,
	                         std::vector<std::array<bool, 16>>>,
	                std::uint32_t>
	    test_hash2;
	KBLIB_UNUSED
	kblib::FNV_hash<std::array<std::array<bool, 16>, 4>, std::uint32_t>
	    test_hash3;
	KBLIB_UNUSED auto call = &decltype(test_hash1)::operator();
	//	std::hash<
	//	    std::unordered_map<std::wstring,
	// std::vector<std::array<bool, 16>>, std::hash<std::wstring>>> 	    std_hash;
	using namespace kblib::literals;
	kblib::FNV_hash<std::uint64_t, std::uint32_t> h_i;
	static_assert(3452452_fnv32 == h_i(3452452u),
	              "hash literal and FNH_hash don't agree");

	CHECK(kblib::FNV_hash<int, std::uint32_t>{}(1000) == 0xf49c691cu);
	(void)kblib::FNV_hash<std::vector<int>, std::uint32_t>{}({});
	(void)kblib::FNV_hash<std::vector<empty_t>, std::uint32_t>{}({});
	// fails because of padding:
	// kblib::FNV_hash<std::vector<has_padding>>{}({});
	// fails because of unorderedness:
	// kblib::FNV_hash<std::unordered_map<int, int>>{}({});

	kblib::FNV_hash<int has_padding::*, std::uint32_t> test_hash4;
	CHECK(test_hash4(nullptr) != test_hash4(&has_padding::i));
#if KBLIB_USE_CXX17
	kblib::FNV_hash<std::optional<int>, std::uint32_t> test_opt_hash;
	(void)test_opt_hash(std::nullopt);
	(void)test_opt_hash(42);
	CHECK(test_opt_hash(42) == kblib::FNV_hash<int, std::uint32_t>{}(42));

	std::variant<int, int, std::string> var(std::in_place_index<1>, 42);
	kblib::FNV_hash<decltype(var)> test_var_hash;
	CHECK_FALSE(test_var_hash(var) == kblib::FNV_hash<int, std::uint32_t>{}(42));
	CHECK_FALSE(test_var_hash(var)
	            == test_var_hash(decltype(var){std::in_place_index<0>, 42}));
#endif
}
