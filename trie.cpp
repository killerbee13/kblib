#include "kblib/trie.h"
#include "catch.hpp"

#include <string>

TEST_CASE("trie") {
	KBLIB_UNUSED auto test = kblib::trie<std::string, int>{};
	static_assert(std::is_same<kblib::indexer_extractor<std::string>,
	                           kblib::default_extractor_t<std::string>>::value,
	              "default_extractor<std::string> is indexer");
	static_assert(kblib::extractor_policy_for<std::string>::value
	                  == kblib::extractor_policy::random_access,
	              "std::string is a random access container");
	static_assert(
	    std::is_same<typename kblib::indexer_extractor<std::string>::value_type,
	                 char>::value,
	    "value_type of std::string is char");
	KBLIB_UNUSED auto test2 = kblib::trie<char, int>{};
	KBLIB_UNUSED auto test3 = kblib::trie<char[], int>{};
}
