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
	kblib::FNV_hash<int*>{}({});
	kblib::FNV_hash<std::string>{}({});
	kblib::FNV_hash<std::vector<char>>{}({});
	kblib::FNV_hash<std::vector<int>>{}({});
	kblib::FNV_hash<std::deque<char>>{}({});
	kblib::FNV_hash<std::tuple<>>{}({});
	kblib::FNV_hash<std::tuple<int*>>{}({});
	kblib::FNV_hash<std::tuple<std::wstring, int*>>{}({});
	kblib::FNV_hash<std::string::iterator>{}({});
	kblib::FNV_hash<std::vector<int>::iterator>{}({});
	kblib::FNV_hash<std::deque<char>::iterator>{}({});
	kblib::FNV_hash<std::set<int>::iterator>{}({});

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
	kblib::FNV_hash<unsigned long long> h_i;
	static_assert(3452452_fnv64 == h_i(3452452ull),
	              "hash literal and FNH_hash don't agree");

	REQUIRE(kblib::FNV_hash<int>{}(1000) == 3434534542295815964);
	kblib::FNV_hash<std::vector<int>>{}({});
	kblib::FNV_hash<std::vector<empty_t>>{}({});
	// fails because of padding:
	// kblib::FNV_hash<std::vector<has_padding>>{}({});
	// fails because of unorderedness:
	// kblib::FNV_hash<std::unordered_map<int, int>>{}({});
#if KBLIB_USE_CXX17
	kblib::FNV_hash<std::optional<int>> test_opt_hash;
	test_opt_hash(std::nullopt);
	test_opt_hash(42);
	REQUIRE(test_opt_hash(42) == kblib::FNV_hash<int>{}(42));

	std::variant<int, int, std::string> var(std::in_place_index<1>, 42);
	kblib::FNV_hash<decltype(var)> test_var_hash;
	REQUIRE_FALSE(test_var_hash(var) == kblib::FNV_hash<int>{}(42));
	REQUIRE_FALSE(test_var_hash(var) ==
	              test_var_hash(decltype(var){std::in_place_index<0>, 42}));
#endif
}
