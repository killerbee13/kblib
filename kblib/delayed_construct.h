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
	delayed_construct& operator=(U&& t) {
		Base::operator=(std::forward<U>(t));
		return *this;
	}

	using Base::operator->;
	using Base::operator*;

	using Base::operator bool;
	constexpr bool is_constructed() const noexcept { return Base::has_value(); }

	using Base::value;

	using Base::emplace;

	/**
	 * @brief delayed_construct<T> is comparable. A non-constructed object is
	 * less than any constructed one. std::nullopt_t is equivalent to a non-
	 * constructed object, and a value is equivalent to a constructed one.
	 */
#define OVERLOAD_DEFER_OP(op)                                                \
	friend constexpr bool operator op(const delayed_construct& lhs,           \
	                                  const delayed_construct& rhs) {         \
		return static_cast<const Base&>(lhs) op static_cast<const Base&>(rhs); \
	}                                                                         \
	template <typename U>                                                     \
	friend constexpr bool operator op(const delayed_construct& lhs,           \
	                                  const delayed_construct<U>& rhs) {      \
		return static_cast<const Base&>(lhs)                                   \
		    op static_cast<const std::optional<U>&>(rhs);                      \
	}                                                                         \
	friend constexpr bool operator op(const delayed_construct& lhs,           \
	                                  std::nullopt_t rhs) {                   \
		return static_cast<const Base&>(lhs) op rhs;                           \
	}                                                                         \
	friend constexpr bool operator op(std::nullopt_t lhs,                     \
	                                  const delayed_construct& rhs) {         \
		return lhs op static_cast<const Base&>(rhs);                           \
	}                                                                         \
	template <typename U>                                                     \
	friend constexpr bool operator op(const delayed_construct& opt,           \
	                                  const U& value) {                       \
		return static_cast<const Base&>(opt) op value;                         \
	}                                                                         \
	template <typename U>                                                     \
	friend constexpr bool operator op(const U& value,                         \
	                                  const delayed_construct& opt) {         \
		return value op static_cast<const Base&>(opt);                         \
	}

	OVERLOAD_DEFER_OP(==)
	OVERLOAD_DEFER_OP(!=)
	OVERLOAD_DEFER_OP(<)
	OVERLOAD_DEFER_OP(<=)
	OVERLOAD_DEFER_OP(>)
	OVERLOAD_DEFER_OP(>=)

#undef OVERLOAD_DEFER_OP

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
	size_t operator()(const argument_type& value) const noexcept {
		return hash<optional<T>>{}(static_cast<const optional<T>&>(value));
	}
};

} // namespace std

#endif // KBLIB_USE_CXX17

#endif // DELAYED_CONSTRUCT_H
