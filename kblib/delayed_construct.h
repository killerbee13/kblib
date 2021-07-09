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
 * @brief Provides delayed_construct, an optional-like type that cannot be
 * cleared.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef DELAYED_CONSTRUCT_H
#define DELAYED_CONSTRUCT_H

#include "hash.h"
#include "tdecl.h"

#if KBLIB_USE_CXX17

#include <optional>

namespace kblib {

template <typename T>
class delayed_construct : protected std::optional<T> {
 private:
	using Base = std::optional<T>;

 public:
	using Base::Base;

	template <typename U,
	          std::enable_if_t<std::is_assignable_v<T&, U&&>, int> = 0>
	auto operator=(U&& t) -> delayed_construct& {
		Base::operator=(std::forward<U>(t));
		return *this;
	}

	using Base::operator->;
	using Base::operator*;

	using Base::operator bool;
	KBLIB_NODISCARD constexpr auto is_constructed() const noexcept -> bool {
		return Base::has_value();
	}

	using Base::value;

	using Base::emplace;

	// TODO(killerbee): add C++20 operator<=> support to delayed_construct

#if 0 && KBLIB_USE_CXX20

	KBLIB_NODISCARD auto operator<=>(const delayed_construct& lhs,
	                 const delayed_construct& rhs) = default;
	KBLIB_NODISCARD auto operator<=>(const delayed_construct& lhs, std::nullopt_t rhs);
	KBLIB_NODISCARD auto operator<=>(const delayed_construct& lhs, const U& rhs);

#else

#define OVERLOAD_DEFER_OP(op)                                                \
	KBLIB_NODISCARD friend constexpr auto operator op(                        \
	    const delayed_construct& lhs,                                         \
	    const delayed_construct& rhs) noexcept->bool {                        \
		return static_cast<const Base&>(lhs) op static_cast<const Base&>(rhs); \
	}                                                                         \
	template <typename U>                                                     \
	KBLIB_NODISCARD friend constexpr auto operator op(                        \
	    const delayed_construct& lhs,                                         \
	    const delayed_construct<U>& rhs) noexcept->bool {                     \
		return static_cast<const Base&>(lhs)                                   \
		    op static_cast<const std::optional<U>&>(rhs);                      \
	}                                                                         \
	KBLIB_NODISCARD friend constexpr auto operator op(                        \
	    const delayed_construct& lhs, std::nullopt_t rhs) noexcept->bool {    \
		return static_cast<const Base&>(lhs) op rhs;                           \
	}                                                                         \
	KBLIB_NODISCARD friend constexpr auto operator op(                        \
	    std::nullopt_t lhs, const delayed_construct& rhs) noexcept->bool {    \
		return lhs op static_cast<const Base&>(rhs);                           \
	}                                                                         \
	template <typename U>                                                     \
	KBLIB_NODISCARD friend constexpr auto operator op(                        \
	    const delayed_construct& opt, const U& value) noexcept->bool {        \
		return static_cast<const Base&>(opt) op value;                         \
	}                                                                         \
	template <typename U>                                                     \
	KBLIB_NODISCARD friend constexpr auto operator op(                        \
	    const U& value, const delayed_construct& opt) noexcept->bool {        \
		return value op static_cast<const Base&>(opt);                         \
	}

	/**
	 * @defgroup Equality operators.
	 *
	 * @brief Two delayed_construct<T> objects are equal if either neither
	 * contains a value, or if both contain the same value. std::nullopt_t is
	 * equivalent to a non-constructed object, and a value is equivalent to a
	 * constructed one.
	 */
	///@{
	OVERLOAD_DEFER_OP(==)
	OVERLOAD_DEFER_OP(!=)
	///@}

	/**
	 * @defgroup Comparison operators.
	 *
	 * @brief A non-constructed delayed_construct<T> object is less than any
	 * constructed one. std::nullopt_t is equivalent to a non-constructed object,
	 * and a value is equivalent to a constructed one.
	 */
	///@{
	OVERLOAD_DEFER_OP(<)
	OVERLOAD_DEFER_OP(<=)
	OVERLOAD_DEFER_OP(>)
	OVERLOAD_DEFER_OP(>=)
	///@}

#undef OVERLOAD_DEFER_OP
#endif

	friend struct std::hash<delayed_construct<T>>;
	friend struct FNV_hash<delayed_construct<T>>;
};

template <typename T>
struct FNV_hash<delayed_construct<T>, void> {
	KBLIB_NODISCARD constexpr std::size_t
	operator()(const delayed_construct<T>& key,
	           std::size_t offset =
	               fnv::fnv_offset<std::size_t>::value) const noexcept {
		if (key) {
			return FNV_hash<T>{}(key.value(), offset);
		} else {
			return FNV_hash<std::nullopt_t>{}(std::nullopt, offset);
		}
	}
};

} // namespace kblib

namespace std {

// Inheriting from another hash specialization means that this hash is (mostly)
// disabled when std::hash<T> is (if I read the requirements correctly,
// operator() should not be declared in a disabled std::hash, but I basically
// can't do that in any sensible way)
template <typename T>
struct hash<kblib::delayed_construct<T>> : hash<T> {
	using argument_type = kblib::delayed_construct<T>;
	auto operator()(const argument_type& value) const noexcept -> std::size_t {
		return hash<optional<T>>{}(static_cast<const optional<T>&>(value));
	}
};

} // namespace std

#endif // KBLIB_USE_CXX17

#endif // DELAYED_CONSTRUCT_H
