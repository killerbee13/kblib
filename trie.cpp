#include "kblib/trie.h"
#include "catch.hpp"

#include <string>

TEST_CASE("trie") {
	auto test =
	    kblib::trie<std::string, int, kblib::indexer_extractor<std::string>>{};
	auto test2 = kblib::trie<char, int, void>{};
	auto test3 = kblib::trie<char[], int, void>{};
	static_assert(kblib::extractor_policy_for<std::string>::value ==
	                  kblib::extractor_policy::random_access,
	              "std::string is a random access container");
	static_assert(
	    std::is_same<typename kblib::indexer_extractor<std::string>::value_type,
	                 char>::value,
	    "value_type of std::string is char");
}
