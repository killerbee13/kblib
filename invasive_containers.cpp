#include "kblib/invasive_containers.h"

#include <string>

#include "catch.hpp"

#if KBLIB_USE_CXX17

struct example {
	int first;
	int second;
	std::string payload;
};

TEST_CASE("invasive") {
	kblib::invasive_dual_map<example, &example::first, &example::second> map;
}

#endif
