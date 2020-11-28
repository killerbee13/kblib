#include "kblib/invasive_containers.h"

#include <string>

#include "catch.hpp"

#if KBLIB_USE_CXX17

struct example {
	int first;
	int second;
	std::string payload;
};

TEST_CASE("invasive_map") { kblib::invasive_map<example, &example::first> map; }

TEST_CASE("invasive_dual_map") {
	kblib::invasive_dual_map<example, &example::first, &example::second> map;
}

#endif
