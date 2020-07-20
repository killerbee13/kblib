#include "kblib/io.h"
#include "catch.hpp"

#include "kblib/simple.h"

#include <iostream>
#include <sstream>
#include <vector>

TEST_CASE("get_line") {
	std::istringstream is{"line1\nline2\nline3\n"};
	std::string line;
	std::vector<std::string> v;
	while (is >> kblib::get_line(line)) {
		v.push_back(line);
	}
	REQUIRE(v == std::vector<std::string>{"line1", "line2", "line3"});
}
TEST_CASE("wide get_line") {
	std::wistringstream is{L"line1\nline2\nline3\n"};
	std::wstring line;
	std::vector<std::wstring> v;
	while (is >> kblib::get_line(line)) {
		v.push_back(line);
	}
	REQUIRE(v == std::vector<std::wstring>{L"line1", L"line2", L"line3"});
}

TEST_CASE("nl") {
	std::istringstream is("abc  \t \n"
	                      " xyz  \n"
	                      "shd sgdf ");
	std::string v;
	is >> v;
	REQUIRE(v == "abc");
	is >> kblib::nl;
	REQUIRE(is.peek() == ' ');
	std::getline(is, v);
	REQUIRE(v == " xyz  ");
	is >> v;
	REQUIRE(v == "shd");
	is >> kblib::nl;
	is >> v;
	REQUIRE(v == "sgdf");
	is >> kblib::nl;
	REQUIRE(is.eof());
}

TEST_CASE("nl(wchar_t)") {
	std::wistringstream is(L"abc  \t \n"
	                       " xyz  \n"
	                       "shd sgdf ");
	std::wstring v;
	is >> v;
	REQUIRE(v == L"abc");
	is >> kblib::nl;
	REQUIRE(is.peek() == ' ');
	std::getline(is, v);
	REQUIRE(v == L" xyz  ");
	is >> v;
	REQUIRE(v == L"shd");
	is >> kblib::nl;
	is >> v;
	REQUIRE(v == L"sgdf");
	is >> kblib::nl;
	REQUIRE(is.eof());
}

#if KBLIB_USE_CXX17
TEST_CASE("get_file_contents") {
	auto filename = "kblib";
	auto filestr = kblib::get_file_contents(filename);
	if (filestr) {
		std::cout << "FNV32a(" << filename << "): " << kblib::FNV32a(*filestr)
		          << '\n';
	} else {
		std::cout << "failed to open " << filename << "\n";
	}
	auto filevec = kblib::get_file_contents<std::vector<uint8_t>>(filename);
	if (filevec) {
		std::cout << "FNV32a(" << filename
		          << "): " << kblib::FNVa<std::uint32_t>(*filevec) << '\n';
	} else {
		std::cout << "failed to open " << filename << "\n";
	}
	// D must be a sequence of char-sized objects, char32_t doesn't work.
	// auto filewstr = kblib::get_file_contents<std::u32string>(filename);

	// deque is not a contiguous container, and does not provide a .data() member
	// so the second overload of get_file_contents will be used.
	auto fileerror = kblib::get_file_contents<std::deque<char>>(filename);
}
#endif
