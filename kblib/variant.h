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
 * Provides utilities for working with std::variant more expressively and more
 * efficiently.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_VARIANT_H
#define KBLIB_VARIANT_H

#include "convert.h"
#include "logic.h"
#include "tdecl.h"

#include <cstddef>
#include <new>

#if KBLIB_USE_CXX17
#include <variant>
#endif

namespace kblib {

#if KBLIB_USE_CXX17

template <typename T, typename = void>
constexpr inline bool is_variant_like_v = false;
template <typename T>
constexpr inline bool is_variant_like_v<T, void_t<std::variant_size<T>>> = true;

template <typename T>
struct is_variant_like : std::bool_constant<is_variant_like_v<T>> {};

/**
 * @brief Lexically converts the value of v (no matter its type) to type To.
 *
 * @deprecated Use lexical_coerce instead, as it more clearly expresses intent.
 *
 * @param v A variant to coerce.
 * @return To The type to coerce to.
 */
template <typename To, typename... Ts>
KBLIB_NODISCARD [[deprecated("Use new lexical_coerce instead.")]] auto
coerce(const std::variant<Ts...>& v) -> To {
	return std::visit([](const auto& t) -> To { return lexical_cast<To>(t); },
	                  v);
}

/**
 * @brief static_casts the value of v (no matter its type) to type To.
 *
 * @param v A variant to coerce.
 * @return To The type to coerce to.
 */
template <typename To, typename... Ts>
KBLIB_NODISCARD auto static_coerce(const std::variant<Ts...>& v) -> To {
	return std::visit([](const auto& t) -> To { return static_cast<To>(t); }, v);
}

/**
 * @brief Lexically converts the value of v (no matter its type) to type To.
 *
 * @param v A variant to coerce.
 * @return To The type to coerce to.
 */
template <typename To, typename... Ts>
KBLIB_NODISCARD auto lexical_coerce(const std::variant<Ts...>& v) -> To {
	return std::visit([](const auto& t) { return lexical_cast<To>(t); }, v);
}

/**
 * @brief Helper class for std::visiting a std::variant.
 *
 * When constructed from a set of lambdas or functors, corrals all of them into
 * a single overload set, suitable for std::visit.
 */
template <typename... Ts>
struct visitor : Ts... {
	using Ts::operator()...;
};
template <typename... Ts>
visitor(Ts...) -> visitor<Ts...>;

namespace detail {

	/**
	 * @brief Given a std::variant T, provides the member type which is a tuple
	 * of the same types.
	 */
	template <typename T>
	struct tuple_type {
		/**
		 * @brief Non-variant inputs produce the empty tuple.
		 */
		using type = std::tuple<>;
	};
	/**
	 * @brief Given a std::variant T, provides the member type which is a tuple
	 * of the same types.
	 */
	template <typename... Ts>
	struct tuple_type<std::variant<Ts...>> {
		/**
		 * @brief A tuple of the same types as T is a variant of.
		 */
		using type = std::tuple<Ts...>;
	};
	/**
	 * Equivalent to typename tuple_type<T>::type
	 */
	template <typename T>
	using tuple_type_t = typename tuple_type<T>::type;

	/**
	 * @brief Generates an array of function pointers which will unwrap the
	 * variant and pass the index to the function.
	 */
	template <typename Variant, typename F, std::size_t... Is>
	constexpr auto indexed_visitor_impl(std::index_sequence<Is...>) -> auto {
		return std::array{+[](Variant&& variant, F&& f) {
			return std::forward<F>(f)(
			    std::integral_constant<std::size_t, Is>{},
			    std::get<Is>(std::forward<Variant>(variant)));
		}...};
	}

} // namespace detail

/**
 * @brief Visit a variant, but pass the index (as an integral_constant) to the
 * visitor. This allows for a visitor of a variant with duplicated types to
 * maintain index information.
 *
 * @param variant The variant to visit.
 * @param fs Any number of functors, which taken together as an overload set can
 * be unambiguously called with (I, A),
 * for I = std::integral_constant<std::size_t, variant.index()>
 * and A = std::get<variant.index()>(variant).
 */
template <typename Variant, typename... Fs>
constexpr auto visit_indexed(Variant&& variant, Fs&&... fs) -> decltype(auto) {
	if (variant.valueless_by_exception()) {
		throw std::bad_variant_access();
	}
	using visitor_t = decltype(visitor{std::forward<Fs>(fs)...});
	return detail::indexed_visitor_impl<Variant, visitor_t>(
	    std::make_index_sequence<
	        std::variant_size_v<std::decay_t<Variant>>>())[variant.index()](
	    std::forward<Variant>(variant), visitor{std::forward<Fs>(fs)...});
}

/**
 * @brief Promotes an input variant to a super-variant. That is, one which
 * provides at least the same set of types.
 * @param v The variant to promote.
 * @return To A super-variant with the same value as v.
 */
template <typename To, typename From>
KBLIB_NODISCARD constexpr auto variant_cast(From&& v) -> To {
	static_assert(contains_types_v<detail::tuple_type_t<std::decay_t<To>>,
	                               detail::tuple_type_t<std::decay_t<From>>>,
	              "To must include all types in From");

	return visit_indexed(std::forward<From>(v), [](auto constant, auto&& x) {
		return To(std::in_place_type<
		              std::variant_alternative_t<constant, std::decay_t<From>>>,
		          std::forward<decltype(x)>(x));
	});

	//	return std::visit([](auto&& x) { return std::forward<decltype(x)>(x); },
	//	                  std::forward<From>(v));
}

/**
 * @brief Wraps std::visit to provide an interface taking one variant and any
 * number of functors providing an overload set.
 *
 * Also moves the variant to the left side of the operation, improving
 * readability.
 *
 * @param v The variant to visit over.
 * @param fs Any number of functors, which taken together as an overload set can
 * be unambiguously called with any type in V.
 */
template <typename V, typename F, typename... Fs>
KBLIB_NODISCARD constexpr auto visit(V&& v, F&& f, Fs&&... fs)
    -> decltype(auto) {
	return std::visit(visitor{std::forward<F>(f), std::forward<Fs>(fs)...},
	                  std::forward<V>(v));
}

namespace detail {

	template <typename F, typename... Ts>
	constexpr bool invocable_with_all_v =
	    (ignore_t<std::invoke_result_t<F, Ts>, std::true_type>::value and ...);

	template <typename Callable, typename Variant>
	constexpr bool v_invocable_with_all_v = false;

	template <typename F, typename... Ts>
	constexpr bool v_invocable_with_all_v<F, std::variant<Ts...>> =
	    invocable_with_all_v<F, Ts...>;

	template <typename F, typename... Ts>
	constexpr bool v_invocable_with_all_v<F, const std::variant<Ts...>> =
	    invocable_with_all_v<F, const Ts...>;

	template <typename F, typename... Ts>
	constexpr bool v_invocable_with_all_v<F, std::variant<Ts...>&> =
	    invocable_with_all_v<F, Ts&...>;

	template <typename F, typename... Ts>
	constexpr bool v_invocable_with_all_v<F, const std::variant<Ts...>&> =
	    invocable_with_all_v<F, const Ts&...>;

	template <typename F, typename... Ts>
	constexpr bool v_invocable_with_all_v<F, std::variant<Ts...>&&> =
	    invocable_with_all_v<F, Ts&&...>;

	template <typename F, typename... Ts>
	constexpr bool v_invocable_with_all_v<F, const std::variant<Ts...>&&> =
	    invocable_with_all_v<F, const Ts&&...>;

	template <typename V, typename F, std::size_t I, std::size_t... Is>
	[[gnu::always_inline]] constexpr decltype(auto)
	visit_impl(V&& v, F&& f, std::index_sequence<I, Is...>) {
		static_assert(I < std::variant_size_v<std::decay_t<V>>);
		if (auto* p = std::get_if<I>(&v)) {
			return std::forward<F>(f)(
			    static_cast<decltype(std::get<I>(std::forward<V>(v)))>(*p));
		} else if constexpr (sizeof...(Is) > 0) {
			return visit_impl(std::forward<V>(v), std::forward<F>(f),
			                  std::index_sequence<Is...>{});
		} else {
			throw std::bad_variant_access();
		}
	}

	template <typename V, typename F, std::size_t I, std::size_t... Is>
	[[gnu::always_inline]] constexpr void
	visit_nop_impl(V&& v, F&& f, std::index_sequence<I, Is...>) noexcept {
		static_assert(I < std::variant_size_v<std::decay_t<V>>);
		if (auto* p = std::get_if<I>(&v)) {
			std::forward<F>(f)(
			    static_cast<decltype(std::get<I>(std::forward<V>(v)))>(*p));
			return;
		} else if constexpr (sizeof...(Is) > 0) {
			return visit_nop_impl(std::forward<V>(v), std::forward<F>(f),
			                      std::index_sequence<Is...>{});
		} else {
			// valueless_by_exception case
			return;
		}
	}

} // namespace detail

template <typename V, typename F, typename... Fs>
[[gnu::always_inline]] constexpr auto visit2(V&& v, F&& f, Fs&&... fs)
    -> decltype(auto) {
	auto visitor_obj = visitor{std::forward<F>(f), std::forward<Fs>(fs)...};
	static_assert(detail::v_invocable_with_all_v<decltype(visitor_obj), V&&>,
	              "Some variant types not accepted by any visitors.");
	return detail::visit_impl(
	    std::forward<V>(v), std::move(visitor_obj),
	    std::make_index_sequence<std::variant_size_v<std::decay_t<V>>>{});
}

template <typename V, typename F, typename... Fs>
[[gnu::always_inline]] constexpr auto visit2_nop(V&& v, F&& f, Fs&&... fs)
    -> void {
	auto visitor_obj = visitor{std::forward<F>(f), std::forward<Fs>(fs)...};
	static_assert(detail::v_invocable_with_all_v<decltype(visitor_obj), V&&>,
	              "Some variant types not accepted by any visitors.");
	return detail::visit_nop_impl(
	    std::forward<V>(v), visitor_obj,
	    std::make_index_sequence<std::variant_size_v<std::decay_t<V>>>{});
}

/**
 * @brief Two-step visiting interface. Takes a variant, and returns an object
 * which can be called with any number of callable arguments, builds an overload
 * set from them, and visits the variant.
 *
 *
 *
 * @note The returned callable object contains a reference to v, so care must be
 * taken to avoid dangling. However, if v is long-lived, the returned object
 * may be stored and used to visit the same variant multiple times.
 *
 * @param v A variant to visit.
 * @return auto A callable object which takes callable arguments and visits the
 * visitor.
 */
template <typename V>
KBLIB_NODISCARD constexpr auto visit(V& v) -> auto {
	return [&v](auto... fs) -> decltype(auto) {
		return kblib::visit(v, std::forward<decltype(fs)>(fs)...);
	};
}

#endif // KBLIB_USE_CXX17

} // namespace kblib

#endif // KBLIB_VARIANT_H
