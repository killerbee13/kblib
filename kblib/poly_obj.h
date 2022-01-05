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
 * @brief Provides poly_obj, which enables polymorphism to be used without
 * unnecessary per-object dynamic allocations.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef POLY_OBJ_H
#define POLY_OBJ_H

#include "hash.h"
#include "variant.h"

namespace kblib {

enum class construct_type : unsigned {
	none = 0,
	copy_only = 1,
	move = 2,
	both = 3,
	throw_move = 4,
	both_throw = 5,
};
KBLIB_NODISCARD constexpr auto operator|(construct_type l,
                                         construct_type r) noexcept
    -> construct_type {
	return static_cast<construct_type>(etoi(l) | etoi(r));
}

KBLIB_NODISCARD constexpr auto operator&(construct_type l,
                                         construct_type r) noexcept
    -> construct_type {
	return static_cast<construct_type>(etoi(l) & etoi(r));
}

KBLIB_NODISCARD constexpr auto operator*(construct_type l, bool r) noexcept
    -> construct_type {
	return r ? l : construct_type::none;
}

/**
 * @namespace kblib::detail_poly
 * @brief Implementation details for poly_obj.
 * @internal
 */
namespace detail_poly {

	KBLIB_NODISCARD constexpr auto copyable(construct_type type) noexcept
	    -> bool {
		return (type & construct_type::copy_only) != construct_type::none;
	}
	KBLIB_NODISCARD constexpr auto movable(construct_type type) noexcept
	    -> bool {
		return (type & construct_type::move) != construct_type::none;
	}
	KBLIB_NODISCARD constexpr auto nothrow_movable(construct_type type) noexcept
	    -> bool {
		return (type & construct_type::move) != construct_type::none
		       and (type & construct_type::throw_move) == construct_type::none;
	}

	KBLIB_NODISCARD constexpr auto make_ctype(bool copyable, bool movable,
	                                          bool nothrow_movable)
	    -> construct_type {
		if (copyable) {
			if (not movable) {
				return construct_type::copy_only;
			} else if (nothrow_movable) {
				return construct_type::both;
			} else {
				return construct_type::both_throw;
			}
		} else if (movable) {
			return construct_type::move;
		} else {
			return construct_type::none;
		}
	}

	template <construct_type traits>
	struct construct_conditional;

	template <>
	struct construct_conditional<construct_type::none> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) = delete;
		construct_conditional(construct_conditional&&) = delete;

		auto operator=(const construct_conditional&)
		    -> construct_conditional& = default;
		auto operator=(construct_conditional &&)
		    -> construct_conditional& = default;
		~construct_conditional() = default;
	};

	template <>
	struct construct_conditional<construct_type::copy_only> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) noexcept = default;
		construct_conditional(construct_conditional&&) = delete;

		auto operator=(const construct_conditional&)
		    -> construct_conditional& = default;
		auto operator=(construct_conditional &&)
		    -> construct_conditional& = default;
		~construct_conditional() = default;
	};

	template <>
	struct construct_conditional<construct_type::move> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) noexcept = delete;
		construct_conditional(construct_conditional&&) noexcept = default;

		auto operator=(const construct_conditional&)
		    -> construct_conditional& = default;
		auto operator=(construct_conditional &&)
		    -> construct_conditional& = default;
		~construct_conditional() = default;
	};

	template <>
	struct construct_conditional<construct_type::both> {};

	template <>
	struct construct_conditional<construct_type::throw_move> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) noexcept = delete;
		construct_conditional(construct_conditional&&) noexcept(false) {}

		auto operator=(const construct_conditional&)
		    -> construct_conditional& = default;
		auto operator=(construct_conditional &&)
		    -> construct_conditional& = default;
		~construct_conditional() = default;
	};

	template <>
	struct construct_conditional<construct_type::both_throw> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) = default;
		construct_conditional(construct_conditional&&) noexcept(false) {}

		auto operator=(const construct_conditional&)
		    -> construct_conditional& = default;
		auto operator=(construct_conditional &&)
		    -> construct_conditional& = default;
		~construct_conditional() = default;
	};

	template <typename T>
	constexpr construct_type construct_traits
	    = construct_type::copy_only* std::is_copy_constructible<T>::value
	      | construct_type::move* std::is_move_constructible<T>::value
	      | construct_type::throw_move*(
	          std::is_move_constructible<T>::value
	          & not std::is_nothrow_move_constructible<T>::value);

	template <typename T, typename hash = void>
	struct erased_hash_t {};

	template <typename T>
	struct erased_hash_t<T, void_t<fakestd::invoke_result_t<std::hash<T>, T>>> {
		static auto default_hash(void* obj) -> std::size_t {
			return std::hash<T>()(*static_cast<const T*>(obj));
		}

		alias<std::size_t (*)(void*)> hash = &default_hash;
	};

	template <typename T, typename hash = void>
	struct kblib_erased_hash_t {};

	template <typename T>
	struct kblib_erased_hash_t<
	    T, void_t<fakestd::invoke_result_t<std::hash<T>, T>>> {
		static auto default_hash(void* obj) -> std::size_t {
			return FNV_hash<T>{}(*static_cast<const T*>(obj));
		}

		alias<std::size_t (*)(void*)> hash = &default_hash;
	};

	template <typename Traits>
	struct erased_construct
	    : Traits::copy_t
	    , Traits::move_t
	    , Traits::destroy_t {
		using Traits::copy_t::copy;
		using Traits::move_t::move;

		using Traits::destroy_t::destroy;
	};

	template <typename T, typename Traits>
	KBLIB_NODISCARD auto make_ops_t() noexcept -> erased_construct<Traits> {
		static_assert(implies<Traits::copyable,
		                      std::is_copy_constructible<T>::value>::value,
		              "T must be copy constructible if Traits is.");
		static_assert(
		    implies<Traits::nothrow_movable,
		            std::is_nothrow_move_constructible<T>::value>::value,
		    "T must be nothrow move constructible if Traits is.");
		static_assert(
		    implies<Traits::movable, std::is_move_constructible<T>::value>::value,
		    "T must be move constructible if Traits is.");
		return {{typename Traits::copy_t{static_cast<T*>(nullptr)}},
		        {typename Traits::move_t{static_cast<T*>(nullptr)}},
		        {typename Traits::destroy_t{static_cast<T*>(nullptr)}}};
	}

	template <typename T, typename = void>
	struct extract_derived_size
	    : std::integral_constant<std::size_t, sizeof(T)> {};
	template <typename T>
	struct extract_derived_size<T, void_if_t<(T::max_derived_size > sizeof(T))>>
	    : std::integral_constant<std::size_t, T::max_derived_size> {};

} // namespace detail_poly

/// Does nothing; matches the copy construction signature.
template <typename T>
inline auto noop(void*, const T*) -> T* {
	return nullptr;
}
/// Does nothing; matches the move construction signature.
template <typename T, bool nothrow>
inline auto noop(void*, T*) noexcept(nothrow) -> T* {
	return nullptr;
}
/// Implements type erasure for copy construction.
template <typename Obj, bool copyable>
struct default_copy {
 private:
	template <typename T>
	static auto do_copy(void* dest, const Obj* from) -> Obj* {
		return static_cast<Obj*>(new (dest) T(*static_cast<const T*>(from)));
	}

	auto (*p_copy)(void* dest, const Obj* from) -> Obj* = &noop<Obj>;

 public:
	/// Constructs an object which does nothing.
	default_copy() noexcept = default;
	/// Constructs an object which can copy a T.
	template <typename T>
	explicit default_copy(T*) noexcept
	    : p_copy(&do_copy<T>) {}

	/// Copies an object of previously-established type.
	KBLIB_NODISCARD auto copy(void* dest, const Obj* from) -> Obj* {
		return p_copy(dest, from);
	}
};
template <typename Obj>
struct default_copy<Obj, false> {
	default_copy() noexcept = default;
	template <typename T>
	explicit default_copy(T*) noexcept {}
	auto copy(void* dest, const Obj* from) -> Obj* = delete;
};
/**
 * @brief Implements copy construction using a virtual clone method.
 * This type is provided mostly as an example.
 */
template <typename Obj, auto (Obj::*clone)(void*) const->Obj*>
struct clone_copy {
	/// Does nothing
	clone_copy() = default;
	/// Does nothing
	template <typename T>
	clone_copy(T*) {}

	/// Invokes the clone method to copy the object
	KBLIB_NODISCARD auto copy(void* dest, const Obj* from) -> Obj* {
		return from->*clone(dest);
	}
};
/// Implements type erasure for move construction.
template <typename Obj, bool movable, bool nothrow, bool copyable>
struct default_move {
 private:
	template <typename T>
	static auto do_move(void* dest, Obj* from) noexcept(nothrow) -> Obj* {
		return static_cast<Obj*>(new (dest) T(std::move(*static_cast<T*>(from))));
	}
	// noexcept isn't working here on clang
	auto (*p_move)(void* dest, Obj* from) noexcept(false)
	    -> Obj* = &noop<Obj, nothrow>;

 public:
	/// Constructs an object which does nothing.
	default_move() noexcept = default;
	/// Constructs an object which can move a T.
	template <typename T>
	explicit default_move(T*) noexcept
	    : p_move(&do_move<T>) {}

	/// Moves an object of previously-established type.
	KBLIB_NODISCARD auto move(void* dest, Obj* from) noexcept(nothrow) -> Obj* {
		return p_move(dest, from);
	}
};
template <typename Obj, bool nothrow>
struct default_move<Obj, false, nothrow, false> {
	default_move() noexcept = default;
	template <typename T>
	explicit default_move(T*) noexcept {}
	auto move(void* dest, Obj* from) noexcept(nothrow) -> Obj* = delete;
};
// fallback on copy
template <typename Obj, bool nothrow>
struct default_move<Obj, false, nothrow, true>
    : private default_copy<Obj, true> {
	using default_copy<Obj, true>::default_copy;

	KBLIB_NODISCARD auto move(void* dest, const Obj* from) noexcept(nothrow)
	    -> Obj* {
		return this->copy(dest, from);
	}
};
/**
 * @brief Uses the class's virtual destructor.
 */
template <typename Obj>
struct default_destroy {
	default_destroy() noexcept = default;
	template <typename T>
	explicit default_destroy(T*) noexcept {}

	auto destroy(Obj* obj) -> void { obj->~Obj(); }
	static_assert(std::has_virtual_destructor<Obj>::value,
	              "Obj must have a virtual destructor");
};

/**
 * @brief poly_obj_traits is a traits class template which abstracts the allowed
 * operations on a polymorphic type hierarchy. Any operation allowed by the
 * traits must be usable for the entire hierarchy, not just the base class.
 *
 * Users of poly_obj may provide explicit specializations of this type, as long
 * as they satisfy all the requirements listed here. Alternatively, users may
 * write their own traits types, subject to the same restrictions.
 */
template <typename Obj,
          construct_type CType = detail_poly::construct_traits<Obj>>
struct poly_obj_traits {

	/**
	 * @brief The default capacity to use if not overridden.
	 *
	 * The default implementation uses Obj::max_derived_size if it exists, or
	 * sizeof(Obj) otherwise.
	 */
	constexpr static std::size_t default_capacity
	    = detail_poly::extract_derived_size<Obj>::value;

	/**
	 * @brief How much to align the storage by.
	 */
	constexpr static std::size_t alignment
	    = std::max(alignof(Obj), alignof(std::max_align_t));

	/**
	 * @brief If the object is copy constructible.
	 */
	constexpr static bool copyable = detail_poly::copyable(CType);

	/**
	 * @brief If the object is move constructible
	 */
	constexpr static bool movable = detail_poly::movable(CType);
	/**
	 * @brief If the object is nothrow move constructible
	 */
	constexpr static bool nothrow_movable = detail_poly::nothrow_movable(CType);

	/**
	 * @brief Implements type erasure for copy construction.
	 *
	 * Must be assignable.
	 * If copying is enabled, copy_t must have:
	 * - a nothrow default constructor.
	 * - a nothrow constructor template taking a single parameter of type T*
	 *   (always nullptr) that produces a copy constructor for type T.
	 * - a member function 'copy' which performs the actual copying.
	 *
	 * @see kblib::default_copy
	 */
	using copy_t = default_copy<Obj, copyable>;

	/**
	 * @brief Implements type erasure for move construction.
	 *
	 * Must be assignable.
	 * If moving is enabled, move_t must have:
	 * - a nothrow default constructor.
	 * - a nothrow constructor template taking a single parameter of type T*
	 *   (always nullptr) that produces a move constructor for type T.
	 * - a member function 'move' which performs the actual moving.
	 *
	 * @note Move may fall back on copy.
	 * @see kblib::default_move
	 */
	using move_t = default_move<Obj, movable, nothrow_movable, copyable>;

	/**
	 * @brief Implements type erasure for destruction. The default implementation
	 * requires and always uses virtual dispatch for destruction.
	 *
	 * Must be assignable.
	 * Must have:
	 * - a nothrow default constructor.
	 * - a nothrow constructor template taking a single parameter of type T*
	 *   (always nullptr) that produces a destroyer for type T.
	 * - a member function 'destroy' which performs the actual destruction.
	 *
	 * @see kblib::default_destroy
	 */
	using destroy_t = default_destroy<Obj>;
	static_assert(std::is_empty<destroy_t>::value, "");
};

template <typename Obj>
using move_only_traits = poly_obj_traits<Obj, construct_type::move>;
template <typename Obj>
using no_move_traits = poly_obj_traits<Obj, construct_type::none>;

/**
 * @brief Inline polymorphic object. Generally mimics the interfaces of
 * std::optional and std::variant.
 *
 * Provides dynamic polymorphism without dynamic allocation. By default, it is
 * copy and move constructible if and only if Obj is. The storage capacity can
 * be overloaded in case derived objects are larger than the base (this is
 * expected to be commonplace).
 *
 * @tparam Obj The base class type which the poly_obj will store. Must have a
 * virtual destructor unless a custom traits class is provided.
 * @tparam Capacity The inline capacity allocated for the contained object. May
 * be set larger than sizeof(Obj) to account for larger derived classes. A value
 * of 0 indicates that the traits type should be used to obtain the capacity.
 * @tparam Traits A type providing member types and constants defining the
 * allowed operations on the object type.
 *
 * @see kblib::poly_obj_traits
 * @nosubgrouping
 */
template <typename Obj, std::size_t Capacity = 0,
          typename Traits = poly_obj_traits<Obj>>
class poly_obj
    : private detail_poly::construct_conditional<detail_poly::make_ctype(
          Traits::copyable, Traits::movable, Traits::nothrow_movable)>
    , private detail_poly::erased_construct<Traits> {
 private:
	using disabler = detail_poly::construct_conditional<detail_poly::make_ctype(
	    Traits::copyable, Traits::movable, Traits::nothrow_movable)>;
	using ops_t = detail_poly::erased_construct<Traits>;

 public:
	/**
	 * @brief Equal to Capacity if specified, else Traits::default_capacity.
	 */
	static constexpr std::size_t capacity
	    = Capacity > 0 ? Capacity : Traits::default_capacity;

	static_assert(capacity >= sizeof(Obj),
	              "Capacity must be large enough for the object type.");
	static_assert(not std::is_array<Obj>::value,
	              "poly_obj of array type is disallowed.");

	using base_type = Obj;
	using traits_type = Traits;

	/**
	 * @name Construction
	 */
	///@{
	/**
	 * @brief The default constructor does not construct any contained object.
	 */
	constexpr poly_obj() = default;
	/**
	 * @brief Explicitly do not construct an object.
	 */
	constexpr poly_obj(std::nullptr_t) noexcept
	    : poly_obj() {}
	/**
	 * @brief Copy-constructs the contained object from obj.
	 *
	 * This function can only be called if Obj is copy-constructible.
	 *
	 * @param obj The object to copy.
	 */
	constexpr poly_obj(const Obj& obj)
	    : ptr(new (data) Obj(obj)) {}
	/**
	 * @brief Move-constructs the contained object from obj.
	 *
	 * This function can only be called if Obj is move-constructible.
	 *
	 * @param obj The object to move from.
	 */
	constexpr poly_obj(Obj&& obj) noexcept(Traits::nothrow_movable)
	    : ptr(new (data) Obj(std::move(obj))) {}

	/**
	 * @brief Constructs the contained object in-place without copying or moving.
	 *
	 * @param args Arguments to be passed to the constructor of Obj.
	 * @see kblib::fakestd::in_place
	 */
	template <typename... Args,
	          typename std::enable_if_t<
	              std::is_constructible<Obj, Args...>::value, int> = 0>
	constexpr explicit poly_obj(fakestd::in_place_t, Args&&... args) noexcept(
	    std::is_nothrow_constructible<Obj, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<Obj, Traits>())
	    , ptr(new (data) Obj(std::forward<Args>(args)...)) {}

	/**
	 * @brief Constructs the contained object in-place without copying or moving.
	 *
	 * @param args Arguments to be passed to the constructor of Obj.
	 * @see kblib::in_place_agg
	 */
	template <typename... Args,
	          typename std::enable_if_t<
	              std::is_constructible<Obj, Args...>::value, int> = 0>
	constexpr explicit poly_obj(kblib::in_place_agg_t, Args&&... args) noexcept(
	    std::is_nothrow_constructible<Obj, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<Obj, Traits>())
	    , ptr(new (data) Obj{std::forward<Args>(args)...}) {}

	/**
	 * @brief Constructs a poly_obj containing an object of derived type.
	 *
	 * This function provides the polymorphism of poly_obj.
	 *
	 * sizeof(U) must be less than or equal to capacity, and must be
	 * copy-constructible and move-constructible if Traits is.
	 *
	 * @tparam U A type publically derived from Obj.
	 * @param args Arguments to pass to the constructor of U.
	 * @return poly_obj A poly_obj<Obj> containing an object of type U.
	 */
	template <typename U, typename... Args>
	KBLIB_NODISCARD static auto make(Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value) -> poly_obj {
		static_assert(sizeof(U) <= capacity,
		              "U must fit inside of the inline capacity.");
		static_assert(alignof(U) <= Traits::alignment,
		              "U must be no more aligned than Traits::alignment");
		static_assert(std::is_base_of<Obj, U>::value
		                  and std::is_convertible<U*, Obj*>::value,
		              "Obj must be an accessible base of Obj.");
		static_assert(
		    implies<Traits::copyable,
		            std::is_copy_constructible<U>::value>::value,
		    "U must be copy constructible if Traits::copyable is true.");
		static_assert(
		    implies<Traits::movable, std::is_move_constructible<U>::value>::value,
		    "U must be move constructible if Traits::movable is true.");
		return {tag<U>{}, std::forward<Args>(args)...};
	}

	/**
	 * @brief Constructs a poly_obj containing an object of derived type.
	 *
	 * This function provides the polymorphism of poly_obj.
	 *
	 * sizeof(U) must be less than or equal to capacity, and must be
	 * copy-constructible and move-constructible if Traits is.
	 *
	 * @tparam U A type publically derived from Obj.
	 * @param args Arguments to pass to the constructor of U.
	 * @return poly_obj A poly_obj<Obj> containing an object of type U.
	 */
	template <typename U, typename... Args>
	KBLIB_NODISCARD static auto make_aggregate(Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value) -> poly_obj {
		static_assert(sizeof(U) <= capacity,
		              "U must fit inside of the inline capacity.");
		static_assert(alignof(U) <= Traits::alignment,
		              "U must be no more aligned than Traits::alignment");
		static_assert(std::is_base_of<Obj, U>::value
		                  and std::is_convertible<U*, Obj*>::value,
		              "Obj must be an accessible base of Obj.");
		static_assert(
		    implies<Traits::copyable,
		            std::is_copy_constructible<U>::value>::value,
		    "U must be copy constructible if Traits::copyable is true.");
		static_assert(
		    implies<Traits::movable, std::is_move_constructible<U>::value>::value,
		    "U must be move constructible if Traits::movable is true.");
		return {tag<U, true>{}, std::forward<Args>(args)...};
	}
	///@}

	/**
	 * @name Copy/move operators
	 */
	///@{
	/**
	 * @brief Constructs a copy of other.
	 *
	 * This function can only be called if Traits::copyable is true.
	 *
	 * @param other A poly_obj to copy from.
	 */
	constexpr poly_obj(const poly_obj& other)
	    : disabler(other)
	    , ops_t(other) {
		if (other.ptr) {
			ptr = this->copy(data, other.ptr);
		}
	}
	/**
	 * @brief Moves the contained object of other into this. Note that the moved-
	 * from poly_obj is not cleared; instead, its contained value is moved from.
	 *
	 * This function can only be called if Traits::movable is true.
	 *
	 * @param other A poly_obj to move from.
	 */
	constexpr poly_obj(poly_obj&& other) noexcept(Traits::nothrow_movable)
	    : disabler(std::move(other))
	    , ops_t(std::move(other)) {
		if (other.ptr) {
			ptr = this->move(data, other.ptr);
		}
	}

	/**
	 * @brief Destroys the contained object, if any, and then copies other as in
	 * the copy constructor.
	 *
	 * @param other A poly_obj to copy from.
	 * @return poly_obj& *this.
	 *
	 * @exceptions In the event that the constructor of Obj throws, the poly_obj
	 * is cleared and the exception rethrown.
	 */
	auto operator=(const poly_obj& other) & -> poly_obj& {
		if (this == &other) {
			return *this;
		}
		clear();
		static_cast<ops_t&>(*this) = other;
		if (other.ptr) {
			ptr = this->copy(data, other.ptr);
		}
		return *this;
	}

	/**
	 * @brief Destroys the contained object, if any, and then moves from other as
	 * in the move constructor.
	 *
	 * @param other A poly_obj to move from.
	 * @return poly_obj& *this.
	 *
	 * @exceptions In the event that the constructor of Obj throws, the poly_obj
	 * is cleared and the exception rethrown.
	 */
	auto operator=(poly_obj&& other) & noexcept(Traits::nothrow_movable)
	    -> poly_obj& {
		if (this == &other) {
			return *this;
		}
		clear();
		static_cast<ops_t&>(*this) = other;
		if (other.ptr) {
			ptr = this->move(data, other.ptr);
		}
		return *this;
	}
	///@}

	/**
	 * @name Validity
	 * @brief Check if the poly_obj contains a value.
	 */
	///@{
	KBLIB_NODISCARD auto has_value() const& noexcept -> bool { return ptr; }

	explicit operator bool() const& noexcept { return has_value(); }
	///@}

	/**
	 * @name Destruction
	 */
	///@{
	/**
	 * @brief Empties the poly_obj, reverting to a default-constructed state.
	 */
	auto clear() noexcept -> void {
		if (ptr) {
			this->destroy(ptr);
			ptr = nullptr;
		}
		static_cast<ops_t&>(*this) = {};
	}

	~poly_obj() noexcept {
		if (ptr) {
			this->destroy(ptr);
		}
	}
	///@}

	/**
	 * @name Object Access
	 * @brief These functions allow access to the contained value.
	 */
	///@{
	/**
	 * @brief Returns a reference to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness and reference
	 * qualification of *this carries over to the contained object, because it is
	 * contained inside of *this.
	 *
	 * @return Obj& A reference to the contained object.
	 */
	KBLIB_NODISCARD auto operator*() & noexcept -> Obj& { return *get(); }
	/**
	 * @brief Returns a reference to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness and reference
	 * qualification of *this carries over to the contained object, because it is
	 * contained inside of *this.
	 *
	 * @return const Obj& A reference to the contained object.
	 */
	KBLIB_NODISCARD auto operator*() const& noexcept -> const Obj& {
		return *get();
	}
	/**
	 * @brief Returns a reference to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness and reference
	 * qualification of *this carries over to the contained object, because it is
	 * contained inside of *this.
	 *
	 * @return Obj&& An rvalue reference to the contained object.
	 */
	KBLIB_NODISCARD auto operator*() && noexcept(Traits::nothrow_movable)
	    -> Obj&& {
		return std::move(*get());
	}
	/**
	 * @brief Returns a reference to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness and reference
	 * qualification of *this carries over to the contained object, because it is
	 * contained inside of *this.
	 *
	 * This particular overload is not expected to be very useful, but it is
	 * provided for completeness.
	 *
	 * @return const Obj&& A const rvalue reference to the contained object.
	 */
	KBLIB_NODISCARD auto operator*() const&& noexcept(Traits::nothrow_movable)
	    -> const Obj&& {
		return std::move(*get());
	}

	/**
	 * @brief Returns a pointer to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness of *this carries
	 * over to the contained object, because it is contained inside of *this.
	 *
	 * @return Obj* A pointer to the contained object.
	 */
	KBLIB_NODISCARD auto get() & noexcept -> Obj* { return ptr; }
	/**
	 * @brief Returns a pointer to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness of *this carries
	 * over to the contained object, because it is contained inside of *this.
	 *
	 * @return const Obj* A pointer to the contained object.
	 */
	KBLIB_NODISCARD auto get() const& noexcept -> const Obj* { return ptr; }

	/**
	 * @brief Returns a pointer to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness of *this carries
	 * over to the contained object, because it is contained inside of *this.
	 *
	 * @return Obj* A pointer to the contained object.
	 */
	KBLIB_NODISCARD auto operator->() & noexcept -> Obj* { return ptr; }
	/**
	 * @brief Returns a pointer to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness of *this carries
	 * over to the contained object, because it is contained inside of *this.
	 *
	 * @return const Obj* A pointer to the contained object.
	 */
	KBLIB_NODISCARD auto operator->() const& noexcept -> const Obj* {
		return ptr;
	}

	/**
	 * @brief Invokes the contained function object, if Obj is a callable type.
	 *
	 * Invokes the contained object, if it exists. If it does not exist, the
	 * behavior is undefined.
	 *
	 * @param args The arguments to forward to the function.
	 * @return invoke_result_t<Obj&, Args&&...> The return value of
	 * the function.
	 */
	template <typename... Args>
	auto operator()(Args&&... args) noexcept(
	    is_nothrow_invocable<Obj&, Args&&...>::value)
	    -> fakestd::invoke_result_t<Obj&, Args&&...> {
		return kblib::invoke(*get(), std::forward<Args>(args)...);
	}
	/**
	 * @brief Invokes the contained function object, if Obj is a callable type.
	 *
	 * Invokes the contained object, if it exists. If it does not exist, the
	 * behavior is undefined.
	 *
	 * @param args The arguments to forward to the function.
	 * @return invoke_result_t<const Obj&, Args&&...> The return value of
	 * the function.
	 */
	template <typename... Args>
	auto operator()(Args&&... args) const
	    noexcept(is_nothrow_invocable<const Obj&, Args&&...>::value)
	        -> fakestd::invoke_result_t<const Obj&, Args&&...> {
		return kblib::invoke(*get(), std::forward<Args>(args)...);
	}

	/**
	 * @brief Access a member variable using a pointer to member
	 */
	template <typename member_type>
	enable_if_t<not std::is_member_function_pointer<member_type Obj::*>::value,
	            member_type>&
	operator->*(member_type Obj::*member) & noexcept {
		return get()->*member;
	}

	/**
	 * @brief Access a member variable using a pointer to member
	 */
	template <typename member_type>
	const enable_if_t<
	    not std::is_member_function_pointer<member_type Obj::*>::value,
	    member_type>&
	operator->*(member_type Obj::*member) const& noexcept {
		return get()->*member;
	}

	/**
	 * @brief Access a member variable using a pointer to member
	 */
	template <typename member_type>
	enable_if_t<not std::is_member_function_pointer<member_type Obj::*>::value,
	            member_type>&&
	operator->*(member_type Obj::*member) && noexcept {
		return std::move(get()->*member);
	}

	/**
	 * @brief Access a member variable using a pointer to member
	 */
	template <typename member_type>
	const enable_if_t<
	    not std::is_member_function_pointer<member_type Obj::*>::value,
	    member_type>&&
	operator->*(member_type Obj::*member) const&& noexcept {
		return std::move(get()->*member);
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark unqualified on &
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(
	    member_type (Obj::*member)(Args...)) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const on &
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const on const&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const) const& noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const on &&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const) && noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark & on &
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(
	    member_type (Obj::*member)(Args...) &) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const& on &
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const& on const&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&) const& noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark && on &&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                 &&) && noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (std::move(*value).*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const& on &&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&) && noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark Unqualified on &&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(
	    member_type (Obj::*member)(Args...)) && noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const&& on &&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&&) && noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (std::move(*value).*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const&& on const&&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&&) const&& noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (std::move(*value).*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Call a member function using a pointer to member
	 * @remark const& on const&&
	 */
	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&) const&& noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}
	///@}

 private:
	alignas(Traits::alignment) byte data[capacity]{};
	Obj* ptr{};

	template <typename U, bool = false>
	struct tag {};

	template <typename U, typename... Args>
	poly_obj(tag<U>, Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<U, Traits>())
	    , ptr(new (data) U(std::forward<Args>(args)...)) {}

	template <typename U, typename... Args>
	poly_obj(tag<U, true>, Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<U, Traits>())
	    , ptr(new (data) U{std::forward<Args>(args)...}) {}
};

template <bool warn>
constexpr auto poly_warn_if() -> std::nullptr_t {
	return {};
}
template <>
[[deprecated(
    "make_poly_obj<T, D, Capacity>(): Specifying capacity with this "
    "API is error-prone, consider "
    "using a type alias instead to avoid repetition.")]] constexpr inline auto
poly_warn_if<true>() -> std::nullptr_t {
	return {};
}

/**
 * @brief A convenience factory for making poly_objs.
 * @relates poly_obj
 */
template <typename T, typename D = T, std::size_t Capacity = sizeof(D),
          typename Traits = poly_obj_traits<T>, typename... Args>
KBLIB_NODISCARD auto make_poly_obj(
    Args&&... args,
    std::nullptr_t = poly_warn_if<(Capacity != sizeof(D)
                                   and Capacity > Traits::default_capacity)>())
    -> poly_obj<T, std::max(Capacity, Traits::default_capacity), Traits> {
	return poly_obj<T, std::max(Capacity, Traits::default_capacity),
	                Traits>::template make<D>(std::forward<Args>(args)...);
}

} // namespace kblib

#endif // POLY_OBJ_H
