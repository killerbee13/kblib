#include "kblib/iterators.h"
#include "catch.hpp"
#include <iterator>
#include <set>
#include <sstream>

TEST_CASE("to_pointer") {
	auto smart_ptr = std::make_unique<int>(0);
	auto* raw_pointer = kblib::to_pointer(smart_ptr);
	REQUIRE(raw_pointer == smart_ptr.get());
	const auto& smart_ptr_const_ref = smart_ptr;
	auto* raw_pointer2 = kblib::to_pointer(smart_ptr_const_ref);
	REQUIRE(raw_pointer2 == smart_ptr_const_ref.get());
}

TEST_CASE("range") {
	// range supports iterators and other similar types.
	std::vector<int> v(100);
	REQUIRE(v.begin() == *kblib::range(v.begin(), v.end()).begin());
	REQUIRE(v.end() == *kblib::range(v.begin(), v.end()).end());

	std::vector<int> r{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
	auto r2 = kblib::range(10);
	REQUIRE(r.size() == r2.size());
	REQUIRE(std::equal(r.begin(), r.end(), r2.begin()));
	REQUIRE(r2[0] == 0);
	REQUIRE(r2[9] == 9);
	REQUIRE(r2[-1] == -1);
}

TEST_CASE("range conversion") {
	SECTION("vector") {
		auto r = kblib::range(2, 5);
		auto v = std::vector<int>(r);
		REQUIRE(v.size() == r.size());
		REQUIRE(std::equal(v.begin(), v.end(), r.begin()));
	}
	SECTION("string") {
		auto r = kblib::range(+'a', 'z' + 1);
		auto v = std::string(r);
		REQUIRE(v == "abcdefghijklmnopqrstuvwxyz");
		REQUIRE(v.size() == r.size());
		REQUIRE(std::equal(v.begin(), v.end(), r.begin()));
	}
	auto x = std::set<int>(kblib::range(0, 10));
}

#if KBLIB_USE_CXX17

TEST_CASE("magic_enumerate") {
	std::vector<int> persistent{0, 1, 1, 2, 3, 5, 8};
	SECTION("lvalue") {
		for (auto&& [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("non-forwarding lvalue") {
		for (auto& [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("copy") {
		for (auto [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
		}
	}

	SECTION("const lvalue") {
		const auto& cp = persistent;
		for (auto&& [i, v] : kblib::magic_enumerate(cp)) {
			REQUIRE(&v == &cp[i]);
			static_assert(std::is_const_v<std::remove_reference_t<decltype(v)>>,
			              "v must refer to const when the range is const");
		}
	}
	SECTION("const reference") {
		for (const auto& [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(&v == &persistent[i]);
			static_assert(
			    std::is_const_v<std::remove_reference_t<decltype(v)>>,
			    "v must refer to const when the bound reference is const");
		}
	}
	SECTION("copy from const") {
		const auto& cp = persistent;
		for (auto [i, v] : kblib::magic_enumerate(cp)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
			static_assert(
			    not std::is_const_v<std::remove_reference_t<decltype(v)>>,
			    "v must not be const when copied from a const range");
		}
	}
	SECTION("const copy from non-const") {
#if defined(__clang__)
		// Suppress warning for intentional behavior
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wrange-loop-analysis"
#endif
		for (const auto [i, v] : kblib::magic_enumerate(persistent)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
			static_assert(std::is_const_v<std::remove_reference_t<decltype(v)>>,
			              "v must refer to const in a const copy");
		}
#if defined(__clang__)
#pragma clang diagnostic pop
#endif
	}

	SECTION("iterators") {
		for (auto&& [i, v] :
		     kblib::magic_enumerate(persistent.cbegin(), persistent.cend())) {
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("mutable iterators") {
		std::vector<int> range{0, 1, 2, 3, 4, 5, 6, 7};
		for (auto&& [i, v] : kblib::magic_enumerate(range.begin(), range.end())) {
			v = 0;
		}
		REQUIRE(std::all_of(range.begin(), range.end(),
		                    [](int v) { return v == 0; }));
	}
	SECTION("reverse iterators") {
		std::vector<int> reversed{7, 6, 5, 4, 3, 2, 1, 0};
		int last = -1;
		for (auto&& [i, v] :
		     kblib::magic_enumerate(reversed.rbegin(), reversed.rend())) {
			REQUIRE(v == i);
			++v;
			last = v;
		}
		REQUIRE(last == reversed.front());
	}

	SECTION("temporary") {
		for (auto&& [i, v] :
		     kblib::magic_enumerate(std::vector<int>(persistent))) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
		}
	}
	SECTION("input iterators") {
		std::istringstream is{"0 0 1 1 2 2 3 3 4 4 5 5 6 6"};
		for (auto&& [i, v] : kblib::magic_enumerate(
		         std::istream_iterator<int>(is), std::istream_iterator<int>())) {
			REQUIRE(v == i / 2);
		}
	}
	SECTION("copied input iterators") {
		std::istringstream is{"0 0 1 1 2 2 3 3 4 4 5 5 6 6"};
		for (auto [i, v] : kblib::magic_enumerate(std::istream_iterator<int>(is),
		                                          std::istream_iterator<int>())) {
			REQUIRE(v == i / 2);
		}
	}

	SECTION("move-only") {
		std::vector<std::unique_ptr<int>> ptr_vec(10);
		for ([[maybe_unused]] auto&& [i, ptr] : kblib::magic_enumerate(ptr_vec)) {
			REQUIRE(not ptr);
		}
	}

	SECTION("array") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (auto [i, v] : kblib::magic_enumerate(arr)) {
			REQUIRE(&arr[i] != &v);
			REQUIRE(i == v);
			REQUIRE(arr[i] == v);
		}
	}

	SECTION("array by ref") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (auto& [i, v] : kblib::magic_enumerate(arr)) {
			REQUIRE(&arr[i] == &v);
		}
	}

	SECTION("array by const ref") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (const auto& [i, v] : kblib::magic_enumerate(arr)) {
			REQUIRE(&arr[i] == &v);
		}
	}

	SECTION("array by forwarding ref") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (auto&& [i, v] : kblib::magic_enumerate(arr)) {
			REQUIRE(&arr[i] == &v);
		}
	}
}

TEST_CASE("cry_enumerate") {
	std::vector<int> persistent{0, 1, 1, 2, 3, 5, 8};
	SECTION("lvalue") {
		for (auto&& [i, v] : kblib::cry_enumerate(persistent)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("non-forwarding lvalue") {
		for (auto& [i, v] : kblib::cry_enumerate(persistent)) {
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("copy") {
		for (auto [i, v] : kblib::cry_enumerate(persistent)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
		}
	}

	SECTION("const lvalue") {
		const auto& cp = persistent;
		for (auto&& [i, v] : kblib::cry_enumerate(cp)) {
			REQUIRE(&v == &cp[i]);
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const when the range is const");
		}
	}
	SECTION("const reference") {
		for (const auto& [i, v] : kblib::cry_enumerate(persistent)) {
			REQUIRE(&v == &persistent[i]);
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const when the bound reference is const");
		}
	}
	SECTION("copy from const") {
		const auto& cp = persistent;
		for (auto [i, v] : kblib::cry_enumerate(cp)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
			// This approach can't do this
			/*static_assert(
			    not std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must not be const when copied from a const range");*/
		}
	}
	SECTION("const copy from non-const") {
#if defined(__clang__)
		// Suppress warning for intentional behavior
#pragma clang diagnostic push
#pragma clang diagnostic ignored "-Wrange-loop-analysis"
#endif
		for (const auto [i, v] : kblib::cry_enumerate(persistent)) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
			static_assert(
			    std::is_const<
			        typename std::remove_reference<decltype(v)>::type>::value,
			    "v must refer to const in a const copy");
		}
#if defined(__clang__)
#pragma clang diagnostic pop
#endif
	}

	SECTION("iterators") {
		for (auto&& [i, v] :
		     kblib::cry_enumerate(persistent.cbegin(), persistent.cend())) {
			REQUIRE(&v == &persistent[i]);
		}
	}
	SECTION("mutable iterators") {
		std::vector<int> range{0, 1, 2, 3, 4, 5, 6, 7};
		for (auto&& [i, v] : kblib::cry_enumerate(range.begin(), range.end())) {
			v = 0;
		}
		REQUIRE(std::all_of(range.begin(), range.end(),
		                    [](int v) { return v == 0; }));
	}
	SECTION("reverse iterators") {
		std::vector<int> reversed{7, 6, 5, 4, 3, 2, 1, 0};
		for (auto&& [i, v] :
		     kblib::cry_enumerate(reversed.rbegin(), reversed.rend())) {
			REQUIRE(v == i);
		}
	}

	SECTION("temporary") {
		for (auto&& [i, v] : kblib::cry_enumerate(std::vector<int>(persistent))) {
			REQUIRE(v == persistent[i]);
			REQUIRE(&v != &persistent[i]);
		}
	}
	SECTION("input iterators") {
		std::istringstream is{"0 0 1 1 2 2 3 3 4 4 5 5 6 6"};
		for (auto&& [i, v] : kblib::cry_enumerate(std::istream_iterator<int>(is),
		                                          std::istream_iterator<int>())) {
			REQUIRE(v == i / 2);
		}
	}
	SECTION("copied input iterators") {
		std::istringstream is{"0 0 1 1 2 2 3 3 4 4 5 5 6 6"};
		for (auto [i, v] : kblib::cry_enumerate(std::istream_iterator<int>(is),
		                                        std::istream_iterator<int>())) {
			REQUIRE(v == i / 2);
		}
	}

	SECTION("move-only") {
		std::vector<std::unique_ptr<int>> ptr_vec(10);
		for (auto&& [i, ptr] : kblib::cry_enumerate(ptr_vec)) {
			(void)i;
		}
	}

	SECTION("array") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (auto [i, v] : kblib::cry_enumerate(arr)) {
			REQUIRE(i == v);
		}
	}

	SECTION("array by ref") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (auto& [i, v] : kblib::cry_enumerate(arr)) {
			REQUIRE(i == v);
		}
	}

	SECTION("array by const ref") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (const auto& [i, v] : kblib::cry_enumerate(arr)) {
			REQUIRE(i == v);
		}
	}

	SECTION("array by forwarding ref") {
		int arr[5] = {0, 1, 2, 3, 4};
		for (auto&& [i, v] : kblib::cry_enumerate(arr)) {
			REQUIRE(i == v);
		}
	}
}

#endif
