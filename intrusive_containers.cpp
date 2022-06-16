#include "kblib/intrusive_containers.h"

#include <string>

#include "catch.hpp"

#if KBLIB_USE_CXX17

struct example {
	int first;
	int second;
	std::string payload;
};

TEST_CASE("intrusive_map") {
	kblib::intrusive_map<example, &example::first> map;
}

TEST_CASE("intrusive_dual_map") {
	kblib::intrusive_dual_map<example, &example::first, &example::second> map;
}

#endif
