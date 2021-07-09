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
		return (type & construct_type::move) != construct_type::none and
		       (type & construct_type::throw_move) == construct_type::none;
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
	constexpr construct_type construct_traits =
	    construct_type::copy_only* std::is_copy_constructible_v<T> |
	    construct_type::move* std::is_move_constructible_v<T> |
	    construct_type::throw_move*(std::is_move_constructible_v<T> &
	                                not std::is_nothrow_move_constructible_v<T>);

	template <construct_type traits>
	struct erased_construct_helper {};

	inline auto noop(void*, void*) noexcept -> void* { return nullptr; }
	inline auto noop(void*, const void*) -> void* { return nullptr; }
	inline auto throw_noop(void*, void*) noexcept(false) -> void* {
		return nullptr;
	}

	template <>
	struct erased_construct_helper<construct_type::copy_only> {
		alias<void* (*)(void*, const void*)> copy = &noop;
	};

	template <>
	struct erased_construct_helper<construct_type::move> {
		alias<void* (*)(void*, void*) noexcept> move = &noop;
	};

	template <>
	struct erased_construct_helper<construct_type::throw_move> {
		alias<void* (*)(void*, void*) noexcept(false)> move = &throw_noop;
	};

	template <>
	struct erased_construct_helper<construct_type::both>
	    : erased_construct_helper<construct_type::copy_only>,
	      erased_construct_helper<construct_type::move> {};

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

	template <typename T, typename Traits>
	KBLIB_NODISCARD auto make_ops_t1() noexcept
	    -> erased_construct_helper<Traits::ctype> {
		static_assert(implies<copyable(Traits::ctype),
		                      std::is_copy_constructible<T>::value>::value,
		              "T must be copy constructible if Traits is.");
		static_assert(
		    implies<nothrow_movable(Traits::ctype),
		            std::is_nothrow_move_constructible<T>::value>::value,
		    "T must be nothrow move constructible if Traits is.");
		static_assert(implies<movable(Traits::ctype),
		                      std::is_move_constructible<T>::value>::value,
		              "T must be move constructible if Traits is.");
		if constexpr (Traits::ctype == construct_type::none) {
			return {};
		} else if constexpr (Traits::ctype == construct_type::copy_only) {
			return {{&Traits::template copy<T>}};
		} else if constexpr (Traits::ctype == construct_type::move or
		                     Traits::ctype == construct_type::throw_move) {
			return {{&Traits::template move<T>}};
		} else {
			return {{&Traits::template copy<T>}, {&Traits::template move<T>}};
		}
	}

	template <typename Traits>
	struct erased_construct : Traits::copy_t, Traits::move_t, Traits::destroy_t {
		using Traits::copy_t::copy;
		using Traits::move_t::move;

		using Traits::destroy_t::destroy;
	};

	template <typename T, typename Traits>
	KBLIB_NODISCARD auto make_ops_t() noexcept -> erased_construct<Traits> {
		static_assert(implies<copyable(Traits::ctype),
		                      std::is_copy_constructible<T>::value>::value,
		              "T must be copy constructible if Traits is.");
		static_assert(
		    implies<nothrow_movable(Traits::ctype),
		            std::is_nothrow_move_constructible<T>::value>::value,
		    "T must be nothrow move constructible if Traits is.");
		static_assert(implies<movable(Traits::ctype),
		                      std::is_move_constructible<T>::value>::value,
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

#if KBLIB_USE_CXX17
	using std::launder;
#else
	template <typename T>
	auto launder(T* x) -> T* {
		return x;
	}
#endif

} // namespace detail_poly

/**
 *
 */
using copy_fn = auto (*)(void*, const void*) -> void*;
/**
 *
 */
template <bool nothrow>
using move_fn = auto (*)(void*, void*) noexcept(nothrow) -> void*;
/**
 * @brief noop
 */
inline auto noop(void*, const void*) -> void* { return nullptr; }
template <bool nothrow>
/**
 * @brief noop
 */
inline auto noop(void*, void*) noexcept(nothrow) -> void* {
	return nullptr;
}
/**
 * @brief The default_copy struct
 */
template <bool copyable>
struct default_copy {
 private:
	template <typename T>
	static auto do_copy(void* dest, const void* from) -> void* {
		return static_cast<void*>(new (dest) T(*static_cast<const T*>(from)));
	}

	auto (*p_copy)(void* dest, const void* from) -> void* = &noop;

 public:
	default_copy() noexcept = default;
	template <typename T>
	explicit default_copy(T*) noexcept : p_copy(&do_copy<T>) {}

	auto copy(void* dest, const void* from) -> void* {
		return p_copy(dest, from);
	}
};
template <>
struct default_copy<false> {
	default_copy() noexcept = default;
	template <typename T>
	explicit default_copy(T*) noexcept {}
	auto copy(void* dest, const void* from) -> void* = delete;
};
/**
 *
 */
template <typename Obj, void* (Obj::*clone)(void*) const>
struct clone_copy {
	clone_copy() = default;
	template <typename T>
	clone_copy(T*) {}

	auto copy(void* dest, const void* from) -> void* {
		return static_cast<const Obj*>(from)->*clone(dest);
	}
};
/**
 *
 */
template <bool movable, bool nothrow, bool copyable>
struct default_move {
	// private:
	template <typename T>
	static auto do_move(void* dest, void* from) noexcept(nothrow) -> void* {
		return static_cast<void*>(new (dest)
		                              T(std::move(*static_cast<T*>(from))));
	}
	// noexcept isn't working here on clang
	auto (*p_move)(void* dest, void* from) noexcept(false)
	    -> void* = &noop<nothrow>;

 public:
	default_move() noexcept = default;
	template <typename T>
	explicit default_move(T*) noexcept : p_move(&do_move<T>) {}

	auto move(void* dest, void* from) noexcept(nothrow) -> void* {
		return p_move(dest, from);
	}
};
template <bool nothrow>
struct default_move<false, nothrow, false> {
	default_move() noexcept = default;
	template <typename T>
	explicit default_move(T*) noexcept {}
	auto move(void* dest, void* from) noexcept(nothrow) -> void* = delete;
};
// fallback on copy
template <bool nothrow>
struct default_move<false, nothrow, true> : private default_copy<true> {
	using default_copy::default_copy;

	auto move(void* dest, const void* from) noexcept(nothrow) -> void* {
		return copy(dest, from);
	}
};
/**
 *
 */
template <typename Obj>
struct default_destroy {
	default_destroy() noexcept = default;
	template <typename T>
	explicit default_destroy(T*) noexcept {}

	auto destroy(void* obj) -> void { static_cast<Obj*>(obj)->~Obj(); }
	static_assert(std::has_virtual_destructor_v<Obj>,
	              "Obj must have a virtual destructor");
};

/**
 * @brief poly_obj_traits is a traits class template which abstracts the allowed
 * operations on a polymorphic type hierarchy. Any operation allowed by the
 * traits must be usable for the entire hierarchy, not just the base class.
 *
 * Users of poly_obj may provide explicit instantiations of this type, as long
 * as they satisfy all the requirements listed here. Alternatively, users may
 * write their own traits types.
 */
template <typename Obj,
          construct_type CType = detail_poly::construct_traits<Obj>>
struct poly_obj_traits {

	constexpr static construct_type ctype = CType;

	/**
	 * @brief The default capacity to use if not overridden.
	 *
	 * The default implementation uses Obj::max_derived_size if it exists, or
	 * sizeof(Obj) otherwise.
	 */
	constexpr static std::size_t default_capacity =
	    detail_poly::extract_derived_size<Obj>::value;

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
	 * If copying is enabled, copy_t must have:
	 * - a nothrow default constructor.
	 * - a nothrow constructor template taking a single parameter of type T*
	 *   (always nullptr) that produces a copy constructor for type T.
	 * - a member function 'copy' which performs the actual copying.
	 */
	using copy_t = default_copy<copyable>;

	/**
	 * @brief Implements type erasure for move construction.
	 *
	 * If moving is enabled, move_t must have:
	 * - a nothrow default constructor.
	 * - a nothrow constructor template taking a single parameter of type T*
	 *   (always nullptr) that produces a move constructor for type T.
	 * - a member function 'move' which performs the actual moving.
	 *
	 * @note Move may fall back on copy.
	 */
	using move_t = default_move<movable, nothrow_movable, copyable>;

	/**
	 * @brief Implements type erasure for destruction. The default implementation
	 * requires and always uses virtual dispatch for destruction.
	 *
	 * Must have:
	 * - a nothrow default constructor.
	 * - a nothrow constructor template taking a single parameter of type T*
	 *   (always nullptr) that produces a destroyer for type T.
	 * - a member function 'destroy' which performs the actual destruction.
	 */
	using destroy_t = default_destroy<Obj>;
};

template <typename Obj>
using move_only_traits = poly_obj_traits<Obj, construct_type::move>;
template <typename Obj>
using no_move_traits = poly_obj_traits<Obj, construct_type::none>;

/**
 * @brief Inline polymorphic object. Generally mimics the interfaces of
 * std::optional and std::variant.
 *
 * Provides dynamic polymorphism without dynamic allocation. It is copy and move
 * constructible if and only if Obj is. The storage capacity can be overloaded
 * in case derived objects are larger than the base (this is expected to be
 * commonplace).
 *
 * @tparam Obj The base class type which the poly_obj will store. Must have a
 * virtual destructor unless a custom traits class is provided.
 * @tparam Capacity The inline capacity allocated for the contained object. May
 * be set larger than sizeof(Obj) to account for larger derived classes.
 * @tparam Traits A type providing member types and constants defining the
 * allowed operations on the object type.
 */
template <typename Obj, std::size_t Capacity = 0,
          typename Traits = poly_obj_traits<Obj>>
class poly_obj
    : private detail_poly::construct_conditional<detail_poly::make_ctype(
          Traits::copyable, Traits::movable, Traits::nothrow_movable)>,
      private detail_poly::erased_construct<Traits> {
 private:
	using disabler = detail_poly::construct_conditional<detail_poly::make_ctype(
	    Traits::copyable, Traits::movable, Traits::nothrow_movable)>;
	using ops_t = detail_poly::erased_construct<Traits>;

 public:
	static constexpr std::size_t capacity =
	    Capacity > 0 ? Capacity : Traits::default_capacity;

	static_assert(capacity >= sizeof(Obj),
	              "Capacity must be large enough for the object type.");
	static_assert(not std::is_array<Obj>::value,
	              "poly_obj of array type is disallowed.");

	using base_type = Obj;

	/**
	 * @brief The default constructor does not construct any contained object.
	 */
	constexpr poly_obj() = default;
	/**
	 * @brief Explicitly do not construct an object.
	 */
	constexpr poly_obj(std::nullptr_t) noexcept : poly_obj() {}
	/**
	 * @brief Constructs a copy of other.
	 *
	 * This function can only be called if Traits is copy-constructible.
	 *
	 * @param other A poly_obj to copy from.
	 */
	constexpr poly_obj(const poly_obj& other)
	    : disabler(other), ops_t(other), valid(other.valid) {
		if (valid) {
			this->copy(data, other.get());
		}
	}
	/**
	 * @brief Moves the contained object of other into this.
	 *
	 * This function can only be called if Traits is move-constructible.
	 *
	 * @param other A poly_obj to move from.
	 */
	constexpr poly_obj(poly_obj&& other) noexcept(Traits::nothrow_movable)
	    : disabler(std::move(other)), ops_t(std::move(other)),
	      valid(other.valid) {
		if (valid) {
			this->move(data, other.get());
		}
	}

	/**
	 * @brief Copy-constructs the contained object from obj.
	 *
	 * This function can only be called is Traits is copy-constructible.
	 *
	 * @param obj The object to copy.
	 */
	constexpr poly_obj(const Obj& obj) : valid(true) { new (data) Obj(obj); }
	/**
	 * @brief Move-constructs the contained object from obj.
	 *
	 * This function can only be called if Traits is move-constructible.
	 *
	 * @param obj The object to move from.
	 */
	constexpr poly_obj(Obj&& obj) noexcept(Traits::nothrow_movable)
	    : valid(true) {
		new (data) Obj(std::move(obj));
	}

	/**
	 * @brief Constructs the contained object in-place without copying or moving.
	 *
	 * @param args Arguments to be passed to the constructor of Obj.
	 *
	 * @exceptions In the event that the constructor of Obj throws, the poly_obj
	 * is cleared and the exception rethrown.
	 */
	template <typename... Args,
	          typename std::enable_if_t<
	              std::is_constructible<Obj, Args...>::value, int> = 0>
	constexpr explicit poly_obj(fakestd::in_place_t, Args&&... args) noexcept(
	    std::is_nothrow_constructible<Obj, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<Obj, Traits>()), valid(true) {
		new (data) Obj(std::forward<Args>(args)...);
	}

	/**
	 * @brief Constructs the contained object in-place without copying or moving.
	 *
	 * @param args Arguments to be passed to the constructor of Obj.
	 *
	 * @exceptions In the event that the constructor of Obj throws, the poly_obj
	 * is cleared and the exception rethrown.
	 */
	template <typename... Args,
	          typename std::enable_if_t<
	              std::is_constructible<Obj, Args...>::value, int> = 0>
	constexpr explicit poly_obj(kblib::in_place_agg_t, Args&&... args) noexcept(
	    std::is_nothrow_constructible<Obj, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<Obj, Traits>()), valid(true) {
		new (data) Obj{std::forward<Args>(args)...};
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
		clear();
		static_cast<ops_t&>(*this) = other;
		if (other.valid) {
			this->copy(data, static_cast<const void*>(other.data));
			valid = true;
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
		clear();
		static_cast<ops_t&>(*this) = other;
		if (other.valid) {
			this->move(data, static_cast<void*>(other.data));
			valid = true;
		}
		return *this;
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
	KBLIB_NODISCARD static auto make(Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value) -> poly_obj {
		static_assert(sizeof(U) <= capacity,
		              "U must fit inside of the inline capacity.");
		static_assert(std::is_base_of<Obj, U>::value and
		                  std::is_convertible<U*, Obj*>::value,
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
		static_assert(std::is_base_of<Obj, U>::value and
		                  std::is_convertible<U*, Obj*>::value,
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

	/*template <typename U = Obj, typename... Args>
	constexpr auto emplace(Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value) -> Obj& {
	   clear();
	   static_cast<ops_t&>(*this) = detail_poly::make_ops_t<U,
	Traits>(); value = new (data) U(std::forward<Args>(args)...);
	}*/

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
	KBLIB_NODISCARD auto get() & noexcept -> Obj* {
		return detail_poly::launder(reinterpret_cast<Obj*>(data));
	}
	/**
	 * @brief Returns a pointer to the contained object.
	 *
	 * Returns a reference to the contained object, if it exists. If it does not
	 * exist, the behavior is undefined. Notably, the constness of *this carries
	 * over to the contained object, because it is contained inside of *this.
	 *
	 * @return const Obj* A pointer to the contained object.
	 */
	KBLIB_NODISCARD auto get() const& noexcept -> const Obj* {
		return detail_poly::launder(reinterpret_cast<const Obj*>(data));
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
	KBLIB_NODISCARD auto operator->() & noexcept -> Obj* {
		return detail_poly::launder(reinterpret_cast<Obj*>(data));
	}
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
		return detail_poly::launder(reinterpret_cast<const Obj*>(data));
	}

	template <typename member_type>
	return_assert_t<
	    not std::is_member_function_pointer<member_type Obj::*>::value,
	    member_type>&
	operator->*(member_type Obj::*member) & noexcept {
		return get()->*member;
	}

	template <typename member_type>
	const return_assert_t<
	    not std::is_member_function_pointer<member_type Obj::*>::value,
	    member_type>&
	operator->*(member_type Obj::*member) const& noexcept {
		return get()->*member;
	}

	template <typename member_type>
	return_assert_t<
	    not std::is_member_function_pointer<member_type Obj::*>::value,
	    member_type>&&
	operator->*(member_type Obj::*member) && noexcept {
		return std::move(get()->*member);
	}

	template <typename member_type>
	const return_assert_t<
	    not std::is_member_function_pointer<member_type Obj::*>::value,
	    member_type>&&
	operator->*(member_type Obj::*member) const&& noexcept {
		return std::move(get()->*member);
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto
	operator->*(member_type (Obj::*member)(Args...)) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const) const& noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto
	operator->*(member_type (Obj::*member)(Args...) &) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&) & noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&) const& noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (value->*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto
	operator->*(member_type (Obj::*member)(Args...) &&) && noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (std::move(*value).*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&&) && noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (std::move(*value).*member)(std::forward<Args>(args)...);
		};
	}

	template <typename member_type, typename... Args>
	KBLIB_NODISCARD auto operator->*(member_type (Obj::*member)(Args...)
	                                     const&&) const&& noexcept {
		return [member, value = get()](Args... args) -> decltype(auto) {
			return (std::move(*value).*member)(std::forward<Args>(args)...);
		};
	}

	/**
	 * @brief Invokes the container function object, if Obj is a callable type.
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
	 * @brief Invokes the container function object, if Obj is a callable type.
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
	 * @brief Check if the poly_obj contains a value.
	 */
	KBLIB_NODISCARD auto has_value() const& noexcept -> bool { return valid; }

	explicit operator bool() const& noexcept { return has_value(); }

	/**
	 * @brief Empties the poly_obj, reverting to a default-constructed state.
	 */
	auto clear() noexcept -> void {
		if (valid) {
			// get()->~Obj();
			this->destroy(data);
			valid = false;
		}
		static_cast<ops_t&>(*this) = {};
	}

	~poly_obj() noexcept {
		if (valid) {
			// get()->~Obj();
			this->destroy(data);
		}
	}

 private:
	alignas(Obj) byte data[capacity]{};
	bool valid{};

	template <typename U, bool = false>
	struct tag {};

	template <typename U, typename... Args>
	poly_obj(tag<U>, Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<U, Traits>()), valid(true) {
		new (data) U(std::forward<Args>(args)...);
	}

	template <typename U, typename... Args>
	poly_obj(tag<U, true>, Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value)
	    : ops_t(detail_poly::make_ops_t<U, Traits>()), valid(true) {
		new (data) U{std::forward<Args>(args)...};
	}
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

template <typename T, typename D = T, std::size_t Capacity = sizeof(D),
          typename Traits = poly_obj_traits<T>, typename... Args>
KBLIB_NODISCARD auto make_poly_obj(
    Args&&... args,
    std::nullptr_t = poly_warn_if<(Capacity != sizeof(D) and
                                   Capacity > Traits::default_capacity)>())
    -> poly_obj<T, std::max(Capacity, Traits::default_capacity), Traits> {
	return poly_obj<T, std::max(Capacity, Traits::default_capacity),
	                Traits>::template make<D>(std::forward<Args>(args)...);
}

} // namespace kblib

#endif // POLY_OBJ_H
