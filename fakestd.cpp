#include "kblib/fakestd.h"
#include "catch.hpp"

template <class... Args>
struct Ref {

	auto operator->() {
		return kblib::meta_type_t<Args...>{};
	} // present only if sizeof...(Args) == 1
};

// static decltype(&Ref<int>::operator->) f;
// static decltype(&Ref<int, int>::operator->) f2;

static Ref<int> r;
KBLIB_UNUSED static Ref<int, int> r2;

KBLIB_UNUSED static void blah() {
	r.operator->();
	// r2.operator->();
	std::tuple<int, int> a{0, 0}, b{0, 1};
	kblib::swap(a, b);
}

TEST_CASE("to_unique(lambda)") {
	// to_unique can make unique_ptrs to anonymous types
	auto p = kblib::to_unique(new auto([] {}), [](auto* p) { delete p; });
}

TEST_CASE("signed_cast") {
	REQUIRE(kblib::signed_cast<unsigned>(-1ll) == -1ull);
	REQUIRE(kblib::signed_cast<signed>(static_cast<unsigned short>(78)) == 78);
	static_assert(
	    std::is_same<signed short, decltype(kblib::signed_cast<signed>(
	                                   std::declval<unsigned short>()))>::value,
	    "");
	static_assert(std::is_same<unsigned short,
	                           decltype(kblib::signed_cast<unsigned>(
	                               std::declval<unsigned short>()))>::value,
	              "");
	static_assert(std::is_same<signed short, decltype(kblib::signed_cast<signed>(
	                                             std::declval<short>()))>::value,
	              "");
	static_assert(std::is_same<unsigned short,
	                           decltype(kblib::signed_cast<unsigned>(
	                               std::declval<unsigned short>()))>::value,
	              "");
	static_assert(
	    std::is_same<signed int, decltype(kblib::signed_cast<signed>(
	                                 std::declval<unsigned int>()))>::value,
	    "");
	//  auto _1 = kblib::signed_cast<signed>(0.0);
	//  auto _2 = kblib::signed_cast<double>(0);
}

#if KBLIB_USE_CXX17
template <class T>
constexpr std::string_view type_name_f() {
	using namespace std;
#ifdef __clang__
	string_view p = __PRETTY_FUNCTION__;
	return string_view(p.data() + 36, p.size() - 36 - 1);
#elif defined(__GNUC__)
	string_view p = __PRETTY_FUNCTION__;
#if __cplusplus < 201402
	return string_view(p.data() + 36, p.size() - 36 - 1);
#else
	return string_view(p.data() + 49, p.find(';', 49) - 49);
#endif
#elif defined(_MSC_VER)
	string_view p = __FUNCSIG__;
	return string_view(p.data() + 84, p.size() - 84 - 7);
#endif
}
#endif

#if KBLIB_USE_CXX17
TEST_CASE("next_larger") {
	static_assert(
	    std::is_same<kblib::detail::next_larger_signed<signed char>::type,
	                 short>::value,
	    "signed char < short");
	static_assert(
	    std::is_same<kblib::detail::next_larger_signed<short>::type, int>::value,
	    "short < int");
	static_assert(
	    std::is_same<kblib::detail::next_larger_signed<int>::type, long>::value,
	    "int < long");
#if 0
	std::cout
	    << "Next_larger: \n"
	    << "signed char: "
	    << type_name_f<kblib::detail::next_larger_signed<signed char>::type>()
	    << '\n'
	    << "short:       "
	    << type_name_f<kblib::detail::next_larger_signed<short>::type>() << '\n'
	    << "int:         "
	    << type_name_f<kblib::detail::next_larger_signed<int>::type>() << '\n';
	// long long can't be promoted
	//	std::cout
	//	    << "long long:   "
	//	    << type_name_f<kblib::detail::next_larger_signed<long long>::type>()
	//	    << '\n';
#endif
}
#endif
