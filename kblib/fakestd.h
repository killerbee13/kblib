/* *****************************************************************************
 * kblib is a general utility library for C++14 and C++17, intended to provide
 * performant high-level abstractions and more expressive ways to do simple
 * things.
 *
 * Copyright (c) 2021 killerbee
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * ****************************************************************************/

/**
 * @file
 * @brief This header provides some features of C++17 <type_traits> and other
 * headers for C++14, as well as some other traits.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_FAKESTD_H
#define KBLIB_FAKESTD_H

#include "tdecl.h"

#include <array>
#include <functional>
#include <limits>
#include <memory>
#include <type_traits>
#include <utility>

#if __has_include("gsl/pointers")
#	include "gsl/pointers"
#endif

#ifndef KBLIB_FAKESTD
#	define KBLIB_FAKESTD (__cplusplus < 201703L)
#endif

namespace KBLIB_NS {

#if __has_include("gsl/pointers")
using gsl::owner;
#else
template <class T, class = std::enable_if_t<std::is_pointer<T>::value>>
using owner = T;
#endif

template <bool B, typename T = void>
using enable_if_t = typename std::enable_if<B, T>::type;

template <typename T>
using decay_t = typename std::decay<T>::type;

template <typename T>
using remove_cvref_t =
    typename std::remove_reference<typename std::remove_cv<T>::type>::type;

template <bool v>
using bool_constant = std::integral_constant<bool, v>;

#if __cpp_lib_constexpr_functional
using std::invoke;
#else

#	if __cpp_lib_apply

template <typename F, typename... Args>
constexpr auto invoke(F&& f, Args&&... args) noexcept(noexcept(std::apply(
    std::forward<F>(f), std::forward_as_tuple(std::forward<Args>(args)...))))
    -> decltype(auto) {
	return std::apply(std::forward<F>(f),
	                  std::forward_as_tuple(std::forward<Args>(args)...));
}

#	else

namespace detail {

	template <typename F, typename... Args,
	          enable_if_t<not std::is_member_pointer<remove_cvref_t<F>>::value,
	                      int> = 0>
	constexpr auto do_invoke(F&& f, Args&&... args) noexcept(noexcept(
	    std::forward<F>(f)(std::forward<Args>(args)...))) -> decltype(auto) {
		return std::forward<F>(f)(std::forward<Args>(args)...);
	}

	template <typename F, typename Object, typename... Args,
	          enable_if_t<not std::is_pointer<remove_cvref_t<Object>>::value
	                          and std::is_member_function_pointer<F>::value,
	                      int> = 0>
	constexpr auto do_invoke(F f, Object&& obj, Args&&... args) noexcept(
	    noexcept((std::forward<Object>(obj).*f)(std::forward<Args>(args)...)))
	    -> decltype(auto) {
		return (obj.*f)(std::forward<Args>(args)...);
	}

	template <typename F, typename Pointer, typename... Args,
	          enable_if_t<std::is_pointer<Pointer>::value
	                          and std::is_member_function_pointer<F>::value,
	                      int> = 0>
	constexpr auto do_invoke(F f, Pointer ptr, Args&&... args) noexcept(
	    noexcept((ptr->*f)(std::forward<Args>(args)...))) -> decltype(auto) {
		return (ptr->*f)(std::forward<Args>(args)...);
	}

	template <typename Member, typename Object,
	          enable_if_t<not std::is_pointer<remove_cvref_t<Object>>::value
	                          and std::is_member_object_pointer<Member>::value,
	                      int> = 0>
	constexpr auto do_invoke(Member mem, Object&& obj) noexcept
	    -> decltype(auto) {
		return std::forward<Object>(obj).*mem;
	}

	template <typename Member, typename Pointer,
	          enable_if_t<std::is_pointer<Pointer>::value
	                          and std::is_member_object_pointer<Member>::value,
	                      int> = 0>
	constexpr auto do_invoke(Member mem, Pointer ptr) noexcept
	    -> decltype(auto) {
		return ptr.*mem;
	}
} // namespace detail

template <typename F, typename... Args>
constexpr auto invoke(F&& f, Args&&... args) noexcept(noexcept(
    detail::do_invoke(std::forward<F>(f), std::forward<Args>(args)...)))
    -> decltype(auto) {
	return detail::do_invoke(std::forward<F>(f), std::forward<Args>(args)...);
}

#	endif // __cpp_lib_apply
#endif    // __cpp_lib_constexpr_functional

/**
 * @namespace kblib::fakestd
 * @brief A namespace which holds all the C++14 implementations of C++17
 * standard library features. In C++17, it is simply defined as an alias to std.
 */
#if KBLIB_FAKESTD
namespace fakestd { // C++14 implementation of C++17 void_t, invoke_result,
	                 // (partially) is_invocable, and is_nothrow_swappable
	using std::swap;

	/**
	 * @namespace kblib::fakestd::detail
	 * @brief Implementation details for kblib::fakestd features
	 *
	 * @internal
	 */
	namespace detail {

		template <typename AlwaysVoid, typename, typename...>
		struct invoke_result {};
		template <typename F, typename... Args>
		struct invoke_result<decltype(void(invoke(std::declval<F>(),
		                                          std::declval<Args>()...))),
		                     F, Args...> {
			using type
			    = decltype(invoke(std::declval<F>(), std::declval<Args>()...));
		};
	} // namespace detail
	template <class F, class... ArgTypes>
	struct invoke_result : detail::invoke_result<void, F, ArgTypes...> {};

	template <typename F, typename... ArgTypes>
	using invoke_result_t = typename invoke_result<F, ArgTypes...>::type;

	template <typename... Ts>
	struct make_void {
		typedef void type;
	};
	template <typename... Ts>
	using void_t = typename make_void<Ts...>::type;

	namespace detail {
		// ALL generic swap overloads MUST already have a declaration available at
		// this point.

		struct nat {
			nat() = delete;
			nat(const nat&) = delete;
			nat& operator=(const nat&) = delete;
			~nat() = delete;
		};

		struct two {
			char lx[2];
		};

		struct is_referenceable_impl {
			template <class Tp>
			static Tp& test(int);
			template <class Tp>
			static two test(...);
		};

		template <class Tp>
		struct is_referenceable
		    : std::integral_constant<
		          bool,
		          not std::is_same<decltype(is_referenceable_impl::test<Tp>(0)),
		                           two>::value> {};

		template <class Tp, class Up = Tp,
		          bool NotVoid
		          = not std::is_void<Tp>::value and not std::is_void<Up>::value>
		struct swappable_with {
			template <class LHS, class RHS>
			static decltype(swap(std::declval<LHS>(), std::declval<RHS>()))
			test_swap(int);
			template <class, class>
			static nat test_swap(long);

			// Extra parens are needed for the C++03 definition of decltype.
			using swap1 = decltype((test_swap<Tp, Up>(0)));
			using swap2 = decltype((test_swap<Up, Tp>(0)));

			static const bool value = not std::is_same<swap1, nat>::value
			                          and not std::is_same<swap2, nat>::value;
		};

		template <class Tp, class Up>
		struct swappable_with<Tp, Up, false> : std::false_type {};

		template <class Tp, class Up = Tp,
		          bool Swappable = swappable_with<Tp, Up>::value>
		struct nothrow_swappable_with {
			static const bool value = noexcept(
			    swap(std::declval<Tp>(),
			         std::declval<Up>()))and noexcept(swap(std::declval<Up>(),
			                                               std::declval<Tp>()));
		};

		template <class Tp, class Up>
		struct nothrow_swappable_with<Tp, Up, false> : std::false_type {};

	} // namespace detail

	template <class Tp>
	struct is_swappable
	    : public std::integral_constant<bool,
	                                    detail::swappable_with<Tp&>::value> {};

	template <class Tp>
	struct is_nothrow_swappable
	    : public std::integral_constant<
	          bool, detail::nothrow_swappable_with<Tp&>::value> {};

#	if KBLIB_USE_CXX17

	template <class Tp, class Up>
	struct is_swappable_with
	    : public std::integral_constant<bool,
	                                    detail::swappable_with<Tp, Up>::value> {
	};

	template <class Tp>
	struct is_swappable
	    : public std::conditional<
	          detail::is_referenceable<Tp>::value,
	          is_swappable_with<typename std::add_lvalue_reference<Tp>::type,
	                            typename std::add_lvalue_reference<Tp>::type>,
	          std::false_type>::type {};

	template <class Tp, class Up>
	struct is_nothrow_swappable_with
	    : public integral_constant<
	          bool, detail::nothrow_swappable_with<Tp, Up>::value> {};

	template <class Tp>
	struct is_nothrow_swappable
	    : public conditional<
	          detail::is_referenceable<Tp>::value,
	          is_nothrow_swappable_with<typename add_lvalue_reference<Tp>::type,
	                                    typename add_lvalue_reference<Tp>::type>,
	          false_type>::type {};

	template <class Tp, class Up>
	constexpr bool is_swappable_with_v = is_swappable_with<Tp, Up>::value;

	template <class Tp>
	constexpr bool is_swappable_v = is_swappable<Tp>::value;

	template <class Tp, class Up>
	constexpr bool is_nothrow_swappable_with_v
	    = is_nothrow_swappable_with<Tp, Up>::value;

	template <class Tp>
	constexpr bool is_nothrow_swappable_v = is_nothrow_swappable<Tp>::value;

#	endif

	namespace detail {
		template <typename F>
		struct not_fn_t {
			constexpr explicit not_fn_t(F&& f)
			    : fd(std::forward<F>(f)) {}
			constexpr not_fn_t(const not_fn_t&) = default;
			constexpr not_fn_t(not_fn_t&&) = default;

			template <class... Args>
			constexpr auto operator()(Args&&... args) & -> decltype(
			    not std::declval<invoke_result_t<std::decay_t<F>&, Args...>>()) {
				return not invoke(fd, std::forward<Args>(args)...);
			}

			template <class... Args>
			constexpr auto operator()(Args&&... args) const& -> decltype(
			    not std::declval<
			        invoke_result_t<std::decay_t<F> const&, Args...>>()) {
				return not invoke(std::move(fd), std::forward<Args>(args)...);
			}

			std::decay_t<F> fd;
		};
	} // namespace detail

	template <typename F>
	auto not_fn(F&& f) -> detail::not_fn_t<F> {
		return detail::not_fn_t<F>(std::forward<F>(f));
	}

	struct in_place_t {
		explicit in_place_t() = default;
	};
	static constexpr in_place_t in_place{};

	template <class ForwardIt>
	constexpr auto max_element(ForwardIt first, ForwardIt last) -> ForwardIt {
		if (first == last)
			return last;

		ForwardIt largest = first;
		++first;
		for (; first != last; ++first) {
			if (*largest < *first) {
				largest = first;
			}
		}
		return largest;
	}

	template <class ForwardIt, class Compare>
	constexpr auto max_element(ForwardIt first, ForwardIt last, Compare comp)
	    -> ForwardIt {
		if (first == last)
			return last;

		ForwardIt largest = first;
		++first;
		for (; first != last; ++first) {
			if (comp(*largest, *first)) {
				largest = first;
			}
		}
		return largest;
	}

	template <class C>
	constexpr auto size(const C& c) -> decltype(c.size()) {
		return c.size();
	}

	template <class T, std::size_t N>
	constexpr auto size(const T (&)[N]) noexcept -> std::size_t {
		return N;
	}

	// Adapted from libstdc++ code, licensed under GPL

	namespace detail {
		// invokable
		template <class Ret, class Fp, class... Args>
		struct invokable_r {
			template <class XFp, class... XArgs>
			static auto try_call(int)
			    -> decltype(kblib::invoke(std::declval<XFp>(),
			                              std::declval<XArgs>()...));
			template <class XFp, class... XArgs>
			static auto try_call(...) -> detail::nat;

			using Result = decltype(try_call<Fp, Args...>(0));

			using type = typename std::conditional<
			    not std::is_same<Result, detail::nat>::value,
			    typename std::conditional<std::is_void<Ret>::value, std::true_type,
			                              std::is_convertible<Result, Ret>>::type,
			    std::false_type>::type;
			static const bool value = type::value;
		};
		template <class Fp, class... Args>
		using invokable = invokable_r<void, Fp, Args...>;

		template <bool IsInvokable, bool IsCVVoid, class Ret, class Fp,
		          class... Args>
		struct nothrow_invokable_r_imp {
			static const bool value = false;
		};

		template <class Ret, class Fp, class... Args>
		struct nothrow_invokable_r_imp<true, false, Ret, Fp, Args...> {
			using ThisT = nothrow_invokable_r_imp;

			template <class Tp>
			static void test_noexcept(Tp) noexcept;

			static const bool value = noexcept(ThisT::test_noexcept<Ret>(
			    kblib::invoke(std::declval<Fp>(), std::declval<Args>()...)));
		};

		template <class Ret, class Fp, class... Args>
		struct nothrow_invokable_r_imp<true, true, Ret, Fp, Args...> {
			static const bool value = noexcept(
			    kblib::invoke(std::declval<Fp>(), std::declval<Args>()...));
		};

		template <class Ret, class Fp, class... Args>
		using nothrow_invokable_r
		    = nothrow_invokable_r_imp<invokable_r<Ret, Fp, Args...>::value,
		                              std::is_void<Ret>::value, Ret, Fp, Args...>;

		template <class Fp, class... Args>
		using nothrow_invokable
		    = nothrow_invokable_r_imp<invokable<Fp, Args...>::value, true, void,
		                              Fp, Args...>;

		template <class Fp, class... Args>
		struct invoke_of
		    : public std::enable_if<
		          invokable<Fp, Args...>::value,
		          typename invokable_r<void, Fp, Args...>::Result> {};
	} // namespace detail

	// is_invocable

	template <class Fn, class... Args>
	struct is_invocable
	    : std::integral_constant<bool, detail::invokable<Fn, Args...>::value> {};

	template <class Ret, class Fn, class... Args>
	struct is_invocable_r
	    : std::integral_constant<bool,
	                             detail::invokable_r<Ret, Fn, Args...>::value> {
	};

	template <class Fn, class... Args>
	constexpr bool is_invocable_v = is_invocable<Fn, Args...>::value;

	template <class Ret, class Fn, class... Args>
	constexpr bool is_invocable_r_v = is_invocable_r<Ret, Fn, Args...>::value;

	// is_nothrow_invocable

	template <class Fn, class... Args>
	struct is_nothrow_invocable
	    : std::integral_constant<bool,
	                             detail::nothrow_invokable<Fn, Args...>::value> {
	};

	template <class Ret, class Fn, class... Args>
	struct is_nothrow_invocable_r
	    : std::integral_constant<
	          bool, detail::nothrow_invokable_r<Ret, Fn, Args...>::value> {};

	template <class Fn, class... Args>
	constexpr bool is_nothrow_invocable_v
	    = is_nothrow_invocable<Fn, Args...>::value;

	template <class Ret, class Fn, class... Args>
	constexpr bool is_nothrow_invocable_r_v
	    = is_nothrow_invocable_r<Ret, Fn, Args...>::value;

} // namespace fakestd
#else
namespace fakestd = std;
#endif

using fakestd::is_invocable;
using fakestd::is_invocable_v;

using fakestd::is_invocable_r;
using fakestd::is_invocable_r_v;

using fakestd::is_nothrow_invocable;
using fakestd::is_nothrow_invocable_v;

using fakestd::is_nothrow_invocable_r;
using fakestd::is_nothrow_invocable_r_v;

template <typename... Ts>
struct meta_type {};

template <typename T>
struct meta_type<T> {
	using type = T;
};

template <typename... Ts>
using meta_type_t = typename meta_type<Ts...>::type;

template <bool>
struct void_if {};

template <>
struct void_if<true> : meta_type<void> {};
template <bool b>
using void_if_t = typename void_if<b>::type;

using fakestd::void_t;

// metafunction_success:
// SFINAE detector for a ::type member type
template <typename T, typename = void>
struct metafunction_success : std::false_type {};

template <typename T>
struct metafunction_success<T, void_t<typename T::type>> : std::true_type {};

template <typename... T>
struct is_callable : metafunction_success<fakestd::invoke_result<T...>> {};

template <typename T>
using metafunction_value_t
    = std::integral_constant<decltype(T::value), T::value>;

/**
 * @brief Essentially just like std::enable_if, but with a different name that
 * makes it clearer what it does in the context of return type SFINAE.
 */
template <bool V, typename T>
struct return_assert {};

template <typename T>
struct return_assert<true, T> : meta_type<T> {};

template <bool V, typename T>
using return_assert_t = typename return_assert<V, T>::type;

namespace detail {

	template <typename F, typename Arg, typename = void>
	struct apply_impl {
		template <std::size_t... Is>
		constexpr static auto do_apply(F&& f, Arg&& arg) noexcept(
		    noexcept(kblib::invoke(std::forward<F>(f),
		                           std::get<Is>(std::forward<Arg>(arg))...)))
		    -> decltype(auto) {
			return kblib::invoke(std::forward<F>(f),
			                     std::get<Is>(std::forward<Arg>(arg))...);
		}
	};

} // namespace detail

template <typename F, typename Arg>
constexpr auto apply(F&& f, Arg&& arg) noexcept(
    noexcept(detail::apply_impl<F, Arg>::do_apply(
        std::forward<F>(f), std::forward<Arg>(arg),
        std::index_sequence<std::tuple_size<Arg>::value>{})))
    -> decltype(auto) {
	return detail::apply_impl<F, Arg>::do_apply(
	    std::forward<F>(f), std::forward<Arg>(arg),
	    std::index_sequence<std::tuple_size<Arg>::value>{});
}

template <typename T>
KBLIB_NODISCARD auto to_unique(owner<T*> p) -> std::unique_ptr<T> {
	return std::unique_ptr<T>(p);
}
template <typename T, typename D>
KBLIB_NODISCARD auto to_unique(owner<T*> p, D&& d) -> std::unique_ptr<T, D> {
	return std::unique_ptr<T, D>(p, d);
}

/**
 * @brief Cast integral argument to corresponding unsigned type
 */
template <typename I>
KBLIB_NODISCARD constexpr auto to_unsigned(I x) -> std::make_unsigned_t<I> {
	return static_cast<std::make_unsigned_t<I>>(x);
}
/**
 * @brief Cast integral argument to corresponding signed type
 */
template <typename I>
KBLIB_NODISCARD constexpr auto to_signed(I x) -> std::make_signed_t<I> {
	return static_cast<std::make_signed_t<I>>(x);
}

/**
 * @brief Cast argument to equivalently-sized type with the same signednessas
 * the template parameter
 */
template <typename A, typename F>
KBLIB_NODISCARD constexpr auto signed_cast(F x)
    -> enable_if_t<std::is_integral<A>::value and std::is_integral<F>::value
                       and std::is_signed<A>::value,
                   std::make_signed_t<F>> {
	return to_signed(x);
}

/**
 * @brief Cast argument to equivalently-sized type with the same signednessas
 * the template parameter
 */
template <typename A, typename F>
KBLIB_NODISCARD constexpr auto signed_cast(F x)
    -> enable_if_t<std::is_integral<A>::value and std::is_integral<F>::value
                       and std::is_unsigned<A>::value,
                   std::make_unsigned_t<F>> {
	return to_unsigned(x);
}

template <typename T>
struct has_member_swap {
 private:
	using yes = char (&)[1];
	using no = char (&)[2];

	template <typename C>
	static auto check(decltype(&C::swap)) -> yes;
	template <typename>
	static auto check(...) -> no;

 public:
	constexpr static bool value = sizeof(check<T>(nullptr)) == sizeof(yes);
};

template <typename T, typename = void>
struct is_tuple_like : std::false_type {};

template <typename T>
struct is_tuple_like<T, void_t<typename std::tuple_element<0, T>::type>>
    : std::true_type {};

namespace detail {

	template <typename... Ts>
	constexpr auto ignore(Ts&&... /*unused*/) noexcept -> void {}

	template <typename T, std::size_t... Is>
	constexpr auto
	swap_tuple_impl(T& a, T& b, std::index_sequence<Is...> /*unused*/) noexcept(
	    noexcept(ignore(((void)swap(std::get<Is>(a), std::get<Is>(b)), 0)...)))
	    -> void {
		ignore(((void)swap(std::get<Is>(a), std::get<Is>(b)), 0)...);
	}

} // namespace detail

KBLIB_UNUSED struct {
	/**
	 * @brief Swaps two objects, using move operations.
	 *
	 * @param a,b The objects that will be swapped.
	 */
	template <typename T, enable_if_t<not has_member_swap<T>::value
	                                      and not is_tuple_like<T>::value,
	                                  int> = 0>
	KBLIB_UNUSED constexpr auto operator()(T& a, T& b) const
	    noexcept(std::is_nothrow_move_constructible<T>::value and
	                 std::is_nothrow_move_assignable<T>::value) -> void {
		auto tmp = std::move(a);
		a = std::move(b);
		b = std::move(tmp);
		return;
	}

	/**
	 * @brief Swaps two objects, using a member swap function, if detected.
	 *
	 * @param a,b The objects that will be swapped.
	 */
	template <typename T, enable_if_t<has_member_swap<T>::value, int> = 0>
	KBLIB_UNUSED constexpr auto operator()(T& a, T& b) const
	    noexcept(noexcept(a.swap(b))) -> void {
		a.swap(b);
		return;
	}

	/**
	 * @brief Swaps two arrays elementwise.
	 *
	 * @param a,b The arrays that will be swapped.
	 */
	template <typename T, std::size_t N>
	KBLIB_UNUSED constexpr auto operator()(T (&a)[N], T (&b)[N]) const
	    noexcept(std::is_nothrow_move_constructible<T>::value and
	                 std::is_nothrow_move_assignable<T>::value) -> void {
		for (std::size_t i = 0; i < N; ++i) {
			swap(a[i], b[i]);
		}
	}

	/**
	 * @brief Swaps two tuples elementwise.
	 *
	 * @param a,b The tuples that will be swapped.
	 */
	template <typename T, enable_if_t<is_tuple_like<T>::value
	                                      and not has_member_swap<T>::value,
	                                  std::size_t>
	                          N
	                      = std::tuple_size<T>::value>
	KBLIB_UNUSED constexpr auto operator()(T& a, T& b) const noexcept(noexcept(
	    detail::swap_tuple_impl(a, b, std::make_index_sequence<N>{}))) -> void {
		detail::swap_tuple_impl(a, b, std::make_index_sequence<N>{});
	}
} KBLIB_CONSTANT swap;

template <typename T, typename U = T>
KBLIB_NODISCARD constexpr auto exchange(T& obj, U&& new_value) -> T {
	T old_value = std::move(obj);
	obj = std::forward<U>(new_value);
	return old_value;
}

#if KBLIB_USE_CXX17

namespace detail {

	template <typename T>
	constexpr std::intmax_t max_val = std::numeric_limits<T>::max();

	KBLIB_NODISCARD constexpr auto msb(std::uintmax_t x) -> std::uintmax_t {
		x |= (x >> 1u);
		x |= (x >> 2u);
		x |= (x >> 4u);
		x |= (x >> 8u);
		x |= (x >> 16u);
		x |= (x >> 32u);
		return (x & ~(x >> 1u));
	}

	template <typename Num>
	KBLIB_NODISCARD constexpr auto msb_possible() -> Num {
		return static_cast<Num>(typename std::make_unsigned<Num>::type{1}
		                        << (std::numeric_limits<Num>::digits - 1u));
	}

	template <class... Args>
	struct type_list {
		template <std::size_t N>
		using type = typename std::tuple_element<N, std::tuple<Args...>>::type;
	};

	template <auto K, typename V>
	struct type_map_el {
		constexpr static auto key = K;
		using value = V;
	};

	template <typename Key, typename Comp, typename... Vals>
	struct type_map {
		using types = type_list<Vals...>;
		template <std::size_t I>
		using element = typename types::template type<I>;

		template <Key key, std::size_t I = 0>
		KBLIB_NODISCARD constexpr static auto get() noexcept -> auto {
			static_assert(I < sizeof...(Vals), "key not found");
			if constexpr (Comp{}(key, element<I>::key)) {
				return tag<typename element<I>::value>{};
			} else {
				return get<key, I + 1>();
			}
		}

		template <Key key, typename Default = void, std::size_t I = 0>
		KBLIB_NODISCARD constexpr static auto get_default() noexcept -> auto {
			if constexpr (I == sizeof...(Vals)) {
				return Default();
			} else if constexpr (Comp{}(key, element<I>::key)) {
				return tag<typename element<I>::value>{};
			} else {
				return get<key, I + 1>();
			}
		}
	};

	template <typename N>
	using make_smap_el
	    = type_map_el<static_cast<std::intmax_t>(msb_possible<N>()), N>;

	template <typename T>
	struct next_larger_signed {
		static_assert(max_val<T> < max_val<std::intmax_t>,
		              "Cannot safely promote intmax_t.");
		struct false_compare {
			template <typename U>
			constexpr auto operator()(U, U) noexcept -> bool {
				return true;
			}
		};

		using ints_map = type_map<
		    std::intmax_t, std::less<>, make_smap_el<std::int_least8_t>,
		    make_smap_el<std::int_least16_t>, make_smap_el<std::int_least32_t>,
		    make_smap_el<std::int_least64_t>, make_smap_el<std::intmax_t>>;

		using type = typename decltype(
		    ints_map::template get_default<max_val<T> + 1>())::type;
	};

	template <typename N, bool = std::is_signed<N>::value>
	struct filter_signed;

	template <typename N>
	struct filter_signed<N, true> {
		using type = N;
	};

	template <typename N>
	using filter_signed_t = typename filter_signed<N>::type;

	template <typename N, bool = std::is_unsigned<N>::value>
	struct filter_unsigned;

	template <typename N>
	struct filter_unsigned<N, true> {
		using type = N;
	};

	template <typename N>
	using filter_unsigned_t = typename filter_unsigned<N>::type;

} // namespace detail

template <typename N, typename = void>
struct safe_signed;

template <typename N>
struct safe_signed<N, std::enable_if_t<std::is_integral<N>::value, void>> {
	using type = std::conditional_t<
	    std::is_signed<N>::value, N,
	    typename detail::next_larger_signed<std::make_signed_t<N>>::type>;
};

template <typename N>
using safe_signed_t = typename safe_signed<N>::type;

template <typename N>
KBLIB_NODISCARD constexpr auto signed_promote(N x) noexcept
    -> safe_signed_t<N> {
	return static_cast<safe_signed_t<N>>(x);
}

#endif

template <typename C, typename T,
          bool = std::is_const<typename std::remove_reference<C>::type>::value>
struct copy_const : meta_type<T> {};

template <typename C, typename T>
struct copy_const<C, T, true> : meta_type<const T> {};

template <typename C, typename T>
struct copy_const<C, T&, true> : meta_type<const T&> {};

template <typename C, typename T>
struct copy_const<C, T&&, true> : meta_type<const T&&> {};

template <typename C, typename V>
using copy_const_t = typename copy_const<C, V>::type;

template <typename T, typename = void>
struct value_detected : std::false_type {};

template <typename T>
struct value_detected<T, void_t<typename T::value_type>> : std::true_type {
	using type = typename T::value_type;
};

template <typename T>
constexpr bool value_detected_v = value_detected<T>::value;
template <typename T>
using value_detected_t = typename value_detected<T>::type;

template <typename T, typename = void>
struct key_detected : std::false_type {};

template <typename T>
struct key_detected<T, void_t<typename T::key_type>> : std::true_type {
	using type = typename T::key_type;
};

template <typename T>
constexpr bool key_detected_v = key_detected<T>::value;
template <typename T>
using key_detected_t = typename key_detected<T>::type;

template <typename T, typename = void>
struct mapped_detected : std::false_type {};

template <typename T>
struct mapped_detected<T, void_t<typename T::mapped_type>> : std::true_type {
	using type = typename T::mapped_type;
};

template <typename T>
constexpr bool mapped_detected_v = mapped_detected<T>::value;
template <typename T>
using mapped_detected_t = typename mapped_detected<T>::type;

template <typename T, typename = void>
struct hash_detected : std::false_type {};

template <typename T>
struct hash_detected<T, void_t<typename T::hasher>> : std::true_type {
	using type = typename T::hasher;
};

template <typename T>
constexpr bool hash_detected_v = hash_detected<T>::value;
template <typename T>
using hash_detected_t = typename hash_detected<T>::type;

template <typename Container, bool = key_detected_v<Container>,
          typename T = typename Container::value_type>
struct value_type_linear {};

template <typename Container>
struct value_type_linear<Container, false, typename Container::value_type>
    : meta_type<typename Container::value_type> {};

template <typename Container>
using value_type_linear_t = typename value_type_linear<Container>::type;

template <typename Container>
constexpr static bool is_linear_container_v
    = value_detected_v<Container> and not key_detected_v<Container>;

template <typename Container>
struct is_linear_container : bool_constant<is_linear_container_v<Container>> {};

template <typename Container, bool = key_detected_v<Container>,
          bool = mapped_detected_v<Container>>
struct key_type_setlike {};

template <typename Container>
struct key_type_setlike<Container, true, false>
    : meta_type<typename Container::key_type> {};

template <typename Container>
using key_type_setlike_t = typename key_type_setlike<Container>::type;

template <typename Container>
constexpr static bool is_setlike_v
    = (key_detected_v<
           Container> and value_detected_v<Container> and not mapped_detected_v<Container> and std::is_same<key_detected_t<Container>, value_detected_t<Container>>::value);

template <class InputIt1, class InputIt2>
KBLIB_NODISCARD constexpr auto equal(InputIt1 first1, InputIt1 last1,
                                     InputIt2 first2) -> bool {
	for (; first1 != last1; ++first1, ++first2) {
		if (not (*first1 == *first2)) {
			return false;
		}
	}
	return true;
}

template <typename InputIt1, typename InputIt2, typename BinaryPredicate,
          typename kblib::enable_if_t<
              not std::is_same<InputIt2, BinaryPredicate>::value, int> = 0>
KBLIB_NODISCARD constexpr auto equal(InputIt1 first1, InputIt1 last1,
                                     InputIt2 first2, BinaryPredicate p)
    -> bool {
	for (; first1 != last1; ++first1, ++first2) {
		if (not p(*first1, *first2)) {
			return false;
		}
	}
	return true;
}
template <class RandomIt1, class RandomIt2,
          typename kblib::enable_if_t<
              std::is_base_of<std::random_access_iterator_tag,
                              typename std::iterator_traits<
                                  RandomIt1>::iterator_category>::value
                  and std::is_base_of<std::random_access_iterator_tag,
                                      typename std::iterator_traits<
                                          RandomIt2>::iterator_category>::value,
              int> = 0>
KBLIB_NODISCARD constexpr auto equal(RandomIt1 first1, RandomIt1 last1,
                                     RandomIt2 first2, RandomIt2 last2)
    -> bool {
	if (std::distance(first1, last1) == std::distance(first2, last2)) {
		return false;
	}
	for (; first1 != last1; ++first1, ++first2) {
		if (not (*first1 == *first2)) {
			return false;
		}
	}
	return true;
}

template <class RandomIt1, class RandomIt2, typename BinaryPredicate,
          typename kblib::enable_if_t<
              std::is_base_of<std::random_access_iterator_tag,
                              typename std::iterator_traits<
                                  RandomIt1>::iterator_category>::value
                  and std::is_base_of<std::random_access_iterator_tag,
                                      typename std::iterator_traits<
                                          RandomIt2>::iterator_category>::value,
              int> = 0>
KBLIB_NODISCARD constexpr auto equal(RandomIt1 first1, RandomIt1 last1,
                                     RandomIt2 first2, RandomIt2 last2,
                                     BinaryPredicate p) -> bool {
	if (std::distance(first1, last1) == std::distance(first2, last2)) {
		return false;
	}
	for (; first1 != last1; ++first1, ++first2) {
		if (not p(*first1, *first2)) {
			return false;
		}
	}
	return true;
}
template <
    class InputIt1, class InputIt2,
    typename kblib::enable_if_t<
        not std::is_base_of<
            std::random_access_iterator_tag,
            typename std::iterator_traits<InputIt1>::iterator_category>::value
            or not std::is_base_of<std::random_access_iterator_tag,
                                   typename std::iterator_traits<
                                       InputIt2>::iterator_category>::value,
        int> = 0>
KBLIB_NODISCARD constexpr auto equal(InputIt1 first1, InputIt1 last1,
                                     InputIt2 first2, InputIt2 last2) -> bool {
	for (; first1 != last1 and first2 != last2; ++first1, ++first2) {
		if (not (*first1 == *first2)) {
			return false;
		}
	}
	return (first1 == last1 and first2 == last2);
}

template <
    typename InputIt1, typename InputIt2, typename BinaryPredicate,
    typename kblib::enable_if_t<
        not std::is_base_of<
            std::random_access_iterator_tag,
            typename std::iterator_traits<InputIt1>::iterator_category>::value
            or not std::is_base_of<std::random_access_iterator_tag,
                                   typename std::iterator_traits<
                                       InputIt2>::iterator_category>::value,
        int> = 0>
KBLIB_NODISCARD constexpr auto equal(InputIt1 first1, InputIt1 last1,
                                     InputIt2 first2, InputIt2 last2,
                                     BinaryPredicate p) -> bool {
	for (; first1 != last1 and first2 != last2; ++first1, ++first2) {
		if (not p(*first1, *first2)) {
			return false;
		}
	}
	return (first1 == last1 and first2 == last2);
}

template <typename C>
KBLIB_NODISCARD constexpr auto size(const C& c) -> decltype(c.size()) {
	return c.size();
}

template <typename T, std::size_t N>
KBLIB_NODISCARD constexpr auto size(const T (&)[N]) noexcept -> std::size_t {
	return N;
}

template <class InputIt1, class InputIt2>
KBLIB_NODISCARD constexpr auto lexicographical_compare(InputIt1 first1,
                                                       InputIt1 last1,
                                                       InputIt2 first2,
                                                       InputIt2 last2) -> bool {
	for (; (first1 != last1) and (first2 != last2); ++first1, (void)++first2) {
		if (*first1 < *first2)
			return true;
		if (*first2 < *first1)
			return false;
	}
	return (first1 == last1) and (first2 != last2);
}

namespace detail {
	template <typename D, typename T, typename = void>
	struct pointer {
		using type = T*;
	};

	template <typename D, typename T>
	struct pointer<D, T, void_t<typename D::pointer>> {
		using type = typename D::pointer;
	};

} // namespace detail

constexpr struct in_place_agg_t {
} in_place_agg;

template <typename T>
class heap_value {
 public:
	using element_type = T;
	using pointer = T*;
	using const_pointer = const T*;
	using reference = T&;
	using const_reference = const T&;

	constexpr heap_value() noexcept
	    : p{nullptr} {}
	constexpr heap_value(std::nullptr_t) noexcept
	    : p{nullptr} {}

	template <typename... Args,
	          enable_if_t<std::is_constructible<T, Args...>::value> = 0>
	heap_value(fakestd::in_place_t, Args&&... args)
	    : p{new T(args...)} {}
	template <typename... Args>
	heap_value(in_place_agg_t, Args&&... args)
	    : p{new T{args...}} {}

	heap_value(const heap_value& u)
	    : p{(u.p ? (new T(*u.p)) : nullptr)} {}
	heap_value(heap_value&& u) noexcept
	    : p{std::exchange(u.p, nullptr)} {}

	auto operator=(const heap_value& u) & -> heap_value& {
		if (this == &u) {
			return *this;
		} else if (not u) {
			p.reset();
		} else if (p) {
			*p = *u;
		} else {
			p.reset(new T(*u.p));
		}
		return *this;
	}

	auto operator=(heap_value&& u) & noexcept -> heap_value& {
		if (this == &u) {
			return *this;
		}
		p = std::exchange(u.p, nullptr);
		return *this;
	}

	auto operator=(const T& val) & -> heap_value& {
		if (this == &val) {
			return *this;
		}
		p.reset(new T(val));
	}
	auto operator=(T&& val) & -> heap_value& {
		if (this == &val) {
			return *this;
		}
		p.reset(new T(std::move(val)));
	}

	auto assign() & -> void { p.reset(new T()); }
	auto assign(const T& val) & -> void { p.reset(new T(val)); }
	auto assign(T&& val) & -> void { p.reset(new T(std::move(val))); }
	template <typename... Args,
	          enable_if_t<std::is_constructible<T, Args...>::value> = 0>
	auto assign(fakestd::in_place_t, Args&&... args) -> void {
		p.reset(new T(std::forward<Args>(args)...));
	}
	template <typename... Args>
	auto assign(in_place_agg_t, Args&&... args) -> void {
		p.reset(new T{std::forward<Args>(args)...});
	}

	auto reset() noexcept -> void {
		p.reset();
		return;
	}

	KBLIB_NODISCARD explicit operator bool() const& noexcept {
		return p != nullptr;
	}

	constexpr auto swap(heap_value& other) noexcept -> void {
		kblib::swap(p, other.p);
	}

	KBLIB_NODISCARD auto get() & noexcept -> pointer { return p.get(); }
	KBLIB_NODISCARD auto get() const& noexcept -> const_pointer {
		return p.get();
	}

	KBLIB_NODISCARD auto value() & noexcept -> reference { return *p; }
	KBLIB_NODISCARD auto value() const& noexcept -> const_reference {
		return *p;
	}
	KBLIB_NODISCARD auto value() && noexcept -> T&& { return *p; }
	KBLIB_NODISCARD auto value() const&& noexcept -> const T&& { return *p; }

	KBLIB_NODISCARD auto operator*() & noexcept -> reference { return *p; }
	KBLIB_NODISCARD auto operator*() const& noexcept -> const_reference {
		return *p;
	}
	KBLIB_NODISCARD auto operator*() && noexcept -> T&& { return *p; }
	KBLIB_NODISCARD auto operator*() const&& noexcept -> const T&& { return *p; }

	KBLIB_NODISCARD auto operator->() & noexcept -> pointer { return p.get(); }
	KBLIB_NODISCARD auto operator->() const& noexcept -> const_pointer {
		return p.get();
	}

	~heap_value() = default;

 private:
	std::unique_ptr<element_type> p;
};

template <typename T, typename D>
class heap_value2 : private std::unique_ptr<T, D> {
	using Base = std::unique_ptr<T, D>;

 public:
	using typename Base::deleter_type;
	using typename Base::element_type;
	using typename Base::pointer;
	using reference = decltype(*std::declval<pointer>());
	using const_reference = const element_type&;

	using Base::Base;
	using Base::operator=;
	heap_value2(const heap_value2& other);
	heap_value2& operator=(const heap_value2& other);

	using Base::release;
	using Base::reset;
	using Base::swap;

	using Base::get;
	using Base::get_deleter;
	using Base::operator bool;

	using Base::operator*;
	using Base::operator->;

	KBLIB_NODISCARD auto value() & noexcept -> reference { return **this; }
	KBLIB_NODISCARD auto value() const& noexcept -> const_reference {
		return **this;
	}
	KBLIB_NODISCARD auto value() && noexcept -> element_type&& { return **this; }
	KBLIB_NODISCARD auto value() const&& noexcept -> const element_type&& {
		return **this;
	}

	~heap_value2() = default;
};

template <typename T, typename D>
class heap_value2<T[], D> : private std::unique_ptr<T[], D> {
	using Base = std::unique_ptr<T[], D>;

 public:
	using typename Base::deleter_type;
	using typename Base::element_type;
	using typename Base::pointer;
	using reference = element_type&;
	using const_reference = const element_type&;

	using Base::Base;
	using Base::operator=;
	heap_value2(const heap_value2& other);
	heap_value2& operator=(const heap_value2& other);

	using Base::release;
	using Base::reset;
	using Base::swap;

	using Base::get;
	using Base::get_deleter;
	using Base::operator bool;

	using Base::operator[];

	KBLIB_NODISCARD auto value() & noexcept -> reference { return **this; }
	KBLIB_NODISCARD auto value() const& noexcept -> const_reference {
		return **this;
	}
	KBLIB_NODISCARD auto value() && noexcept -> element_type&& { return **this; }
	KBLIB_NODISCARD auto value() const&& noexcept -> const element_type&& {
		return **this;
	}

	~heap_value2() = default;
};

} // namespace KBLIB_NS

#endif // KBLIB_FAKESTD_H
