#include "catch/catch.hpp"
#include <chrono>
#include <iostream>
#include <map>
#include <string_view>

#include "kblib/variant.h"

#include "kblib/hash.h"

#if KBLIB_USE_CXX17

TEST_CASE("visit") {
	std::variant<std::monostate, int, std::string> var = 10;

	// standard syntax (with visitor helper):
	std::visit(kblib::visitor{[](std::monostate) { REQUIRE(false); },
	                          [](int) { REQUIRE(true); },
	                          [](const std::string&) { REQUIRE(false); }},
	           var);

	// Basic single-variant syntax:
	kblib::visit(
	    var, [](std::monostate) { REQUIRE(false); }, [](int) { REQUIRE(true); },
	    [](const std::string&) { REQUIRE(false); });

	// Pattern-matching-like syntax:
	kblib::visit(var)([](std::monostate) { REQUIRE(false); },
	                  [](int) { REQUIRE(true); },
	                  [](const std::string&) { REQUIRE(false); });

	//	kblib::visit2(var, [](int) {});
}

TEST_CASE("visit2") {
	std::variant<std::monostate, int, std::string> var = 10;

	// standard syntax (with visitor helper):
	std::visit(kblib::visitor{[](std::monostate) { REQUIRE(false); },
	                          [](int) { REQUIRE(true); },
	                          [](const std::string&) { REQUIRE(false); }},
	           var);

	// Basic single-variant syntax:
	kblib::visit2(
	    var, [](std::monostate) { REQUIRE(false); }, [](int) { REQUIRE(true); },
	    [](const std::string&) { REQUIRE(false); });
}

TEST_CASE("visit_indexed") {
	std::variant<std::monostate, int, std::string> var(std::in_place_type<int>,
	                                                   10);
	kblib::visit_indexed(var, [](auto constant, auto val) {
		REQUIRE(constant == 1);
		static_assert(
		    std::is_same_v<decltype(val),
		                   std::variant_alternative_t<constant, decltype(var)>>);
	});
}

TEST_CASE("variant_cast") {
	std::variant<int, std::string> from(std::in_place_type<int>, 10);
	auto to
	    = kblib::variant_cast<std::variant<std::monostate, int, std::string>>(
	        from);
	REQUIRE(to.index() == 1);
}

#endif
