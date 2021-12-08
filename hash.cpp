#include "kblib/hash.h"

#include "catch.hpp"

#include <deque>
#include <map>
#include <set>
#include <unordered_map>

struct has_padding {
	char c;
	int i;
};

struct empty_t {};

static_assert(kblib::is_linear_container_v<std::string> and
                  kblib::is_contiguous_v<std::string> and
                  kblib::is_trivially_hashable_v<std::string::value_type> and
                  kblib::is_trivially_hashable_v<int*>,
              "");
static_assert(kblib::asserts::is_trivial_container<std::vector<char>>, "");

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
	                   std::vector<std::basic_string<bool>>,
	                   kblib::FNV_hash<std::tuple<std::wstring, int*>>>
	    test_map;
	using map_t = kblib::hash_map<std::tuple<std::wstring, int*>,
	                              std::vector<std::basic_string<bool>>>;
	static_assert(std::is_same<decltype(test_map), map_t>::value, "");
	KBLIB_UNUSED kblib::FNV_hash<std::map<int, int>> test_hash1;
	KBLIB_UNUSED kblib::FNV_hash<std::tuple<int, int*>> test_hash1a;
	KBLIB_UNUSED kblib::FNV_hash<std::pair<int* const, int>> test_hash1b;
	KBLIB_UNUSED kblib::FNV_hash<std::pair<const std::tuple<int, int*>, int>>
	    test_hash1c;
	KBLIB_UNUSED kblib::FNV_hash<std::map<std::tuple<std::wstring, int*>,
	                                      std::vector<std::basic_string<bool>>>>
	    test_hash2;
	KBLIB_UNUSED kblib::FNV_hash<std::array<std::basic_string<bool>, 4>>
	    test_hash3;
	KBLIB_UNUSED auto call = &decltype(test_hash1)::operator();
	//	std::hash<
	//	    std::unordered_map<std::wstring, std::vector<std::basic_string<bool>>,
	//	                       std::hash<std::wstring>>>
	//	    std_hash;
	using namespace kblib::literals;
	kblib::FNV_hash<std::uint64_t> h_i;
	static_assert(3452452_fnv64 == h_i(3452452ull),
	              "hash literal and FNH_hash don't agree");

	CHECK(kblib::FNV_hash<int>{}(1000) == 0x2fa9eaf82259d71cu);
	(void)kblib::FNV_hash<std::vector<int>>{}({});
	(void)kblib::FNV_hash<std::vector<empty_t>>{}({});
	// fails because of padding:
	// kblib::FNV_hash<std::vector<has_padding>>{}({});
	// fails because of unorderedness:
	// kblib::FNV_hash<std::unordered_map<int, int>>{}({});

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
	CHECK_FALSE(test_var_hash(var) ==
	            test_var_hash(decltype(var){std::in_place_index<0>, 42}));
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
	KBLIB_UNUSED kblib::FNV_hash<std::map<std::tuple<std::wstring, int*>,
	                                      std::vector<std::basic_string<bool>>>,
	                             std::uint32_t>
	    test_hash2;
	KBLIB_UNUSED
	kblib::FNV_hash<std::array<std::basic_string<bool>, 4>, std::uint32_t>
	    test_hash3;
	KBLIB_UNUSED auto call = &decltype(test_hash1)::operator();
	//	std::hash<
	//	    std::unordered_map<std::wstring, std::vector<std::basic_string<bool>>,
	//	                       std::hash<std::wstring>>>
	//	    std_hash;
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
	CHECK_FALSE(test_var_hash(var) ==
	            test_var_hash(decltype(var){std::in_place_index<0>, 42}));
#endif
}
