#include <iostream>

#include "kblib/variant.h"

#define KBLIB_VARIANT_WORKING 0

#if KBLIB_VARIANT_WORKING
void exemplar(std::variant<std::monostate, int, std::string> var) {
	std::visit(kblib::visitor{[](std::monostate) { std::cout << "empty\n"; },
	                          [](int i) { std::cout << "int: " << i << '\n'; },
	                          [](const std::string& s) {
	                             std::cout << "string: " << s << '\n';
	                          }},
	           var);

	kblib::visit(var)(
	    [](std::monostate) { std::cout << "empty\n"; },
	    [](int i) { std::cout << "int: " << i << '\n'; },
	    [](const std::string& s) { std::cout << "string: " << s << '\n'; });
}

#endif
