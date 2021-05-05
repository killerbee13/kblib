#include "kblib/io.h"
#include "catch.hpp"

#include "kblib/hash.h"

#include <deque>
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

TEST_CASE("expect(good)") {
	std::istringstream is("11/17/2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is);
	is >> day >> kblib::expect('/') >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(fail)") {
	std::istringstream is("11-17-2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> day >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(wchar_t, good)") {
	std::wistringstream is(L"11/17/2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect(L'/');
	REQUIRE(is);
	is >> day >> kblib::expect(L'/') >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(wchar_t, fail)") {
	std::wistringstream is(L"11-17-2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect(L'/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect(L'-');
	REQUIRE(is);
	is >> day >> kblib::expect(L'/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect(L'-');
	REQUIRE(is);
	is >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(widening, good)") {
	std::wistringstream is(L"11/17/2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is);
	is >> day >> kblib::expect('/') >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(widening, fail)") {
	std::wistringstream is(L"11-17-2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> day >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

// No locale support for unicode
#if 0
TEST_CASE("expect(widening, UFT-16, good)") {
	std::basic_istringstream<char16_t> is(u"11/17/2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is);
	is >> day >> kblib::expect('/') >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(widening, UTF-16, fail)") {
	std::basic_istringstream<char16_t> is(u"11-17-2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> day >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(widening, UFT-32, good)") {
	std::basic_istringstream<char32_t> is(U"11/17/2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is);
	is >> day >> kblib::expect('/') >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(widening, UTF-32, fail)") {
	std::basic_istringstream<char32_t> is(U"11-17-2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> day >> kblib::expect('/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect('-');
	REQUIRE(is);
	is >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(widening, UTF-16 to UFT-32, good)") {
	std::basic_istringstream<char32_t> is(U"11/17/2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect(u'/');
	REQUIRE(is);
	is >> day >> kblib::expect(u'/') >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(widening, UTF-16 to UTF-32, fail)") {
	std::basic_istringstream<char32_t> is(U"11-17-2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect(u'/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect(u'-');
	REQUIRE(is);
	is >> day >> kblib::expect(u'/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect(u'-');
	REQUIRE(is);
	is >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}
#endif

#if 0 // Compile error
TEST_CASE("expect(narrowing, good)") {
	std::istringstream is("11/17/2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect(L'/');
	REQUIRE(is);
	is >> day >> kblib::expect(L'/') >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}

TEST_CASE("expect(narrowing, fail)") {
	std::istringstream is("11-17-2020");
	int day{}, month{}, year{};
	is >> month >> kblib::expect(L'/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect(L'-');
	REQUIRE(is);
	is >> day >> kblib::expect(L'/');
	REQUIRE(is.fail());
	is.clear();
	is >> kblib::expect(L'-');
	REQUIRE(is);
	is >> year;
	REQUIRE(is);
	REQUIRE(month == 11);
	REQUIRE(day == 17);
	REQUIRE(year == 2020);
}
#endif

#if KBLIB_USE_CXX17
TEST_CASE("get_file_contents") {
	auto filename = "medfile";
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

TEST_CASE("tee_stream") {
	std::ostringstream a, b;
#if KBLIB_USE_CXX17
	auto os = kblib::tee(a, b);
#else
	// kblib::basic_teestream<std::ostringstream, std::ostringstream> os(a, b);
	auto&& os = kblib::tee(a, b);
#endif

	os << "test" << std::flush;
	REQUIRE(os);
	REQUIRE(a.str() == "test");
	REQUIRE(a.str() == b.str());

	os << 's' << std::flush;
	REQUIRE(os);
	REQUIRE(a.str() == "tests");
	REQUIRE(a.str() == b.str());

	os << std::setw(8) << 42 << std::flush;
	REQUIRE(os);
	REQUIRE(a.str() == "tests      42");
	REQUIRE(a.str() == b.str());

	{
		auto len = a.str().length();
		for (KBLIB_UNUSED auto _ : kblib::range(2048)) {
			os << '.' << std::flush;
			REQUIRE(a.str() == b.str());
			REQUIRE(a.str().length() == ++len);
			REQUIRE(a.str().back() == '.');
		}
		a.str("");
		b.str("");
	}

	{
		auto big_str = std::string(2048, '?');
		os << big_str << std::flush;
		REQUIRE(os);
		REQUIRE(a.str() == big_str);
		REQUIRE(a.str() == b.str());
	}
}
