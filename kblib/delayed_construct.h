#ifndef DELAYED_CONSTRUCT_H
#define DELAYED_CONSTRUCT_H

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
	/**
	 * @brief delayed_construct<T> is comparable. A non-constructed object is
	 * less than any constructed one. std::nullopt_t is equivalent to a non-
	 * constructed object, and a value is equivalent to a constructed one.
	 */
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

	OVERLOAD_DEFER_OP(==)
	OVERLOAD_DEFER_OP(!=)
	OVERLOAD_DEFER_OP(<)
	OVERLOAD_DEFER_OP(<=)
	OVERLOAD_DEFER_OP(>)
	OVERLOAD_DEFER_OP(>=)

#undef OVERLOAD_DEFER_OP
#endif

	friend struct std::hash<T>;
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
