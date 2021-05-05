#ifndef POLY_OBJ_H
#define POLY_OBJ_H

#include "hash.h"
#include "variant.h"

namespace kblib {

namespace detail {

	enum class construct_type : unsigned {
		none = 0,
		copy = 1,
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

	template <construct_type traits>
	struct construct_conditional;

	template <>
	struct construct_conditional<construct_type::none> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) = delete;
	};

	template <>
	struct construct_conditional<construct_type::copy> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) noexcept = default;
		construct_conditional(construct_conditional&&) = delete;
	};

	template <>
	struct construct_conditional<construct_type::move> {
		construct_conditional() noexcept = default;
		construct_conditional(construct_conditional&&) noexcept = default;
	};

	template <>
	struct construct_conditional<construct_type::both> {};

	template <>
	struct construct_conditional<construct_type::throw_move> {
		construct_conditional() noexcept = default;
		construct_conditional(construct_conditional&&) noexcept(false) {}
	};

	template <>
	struct construct_conditional<construct_type::both_throw> {
		construct_conditional() noexcept = default;
		construct_conditional(const construct_conditional&) = default;
		construct_conditional(construct_conditional&&) noexcept(false) {}
	};

	template <typename T>
	constexpr construct_type construct_traits =
	    construct_type::copy* std::is_copy_constructible<T>::value |
	    construct_type::move* std::is_move_constructible<T>::value;

	template <construct_type traits>
	struct assign_conditional;

	template <>
	struct assign_conditional<construct_type::none> {
		assign_conditional() noexcept = default;
		auto operator=(const assign_conditional&) -> assign_conditional& = delete;
	};

	template <>
	struct assign_conditional<construct_type::copy> {
		assign_conditional() noexcept = default;
		assign_conditional&
		operator=(const assign_conditional&) noexcept = default;
		auto operator=(assign_conditional &&) -> assign_conditional& = delete;
	};

	template <>
	struct assign_conditional<construct_type::move> {
		assign_conditional() noexcept = default;
		auto operator=(assign_conditional&&) noexcept
		    -> assign_conditional& = default;
	};

	template <>
	struct assign_conditional<construct_type::both> {};

	// clang-format off

	// It gets confused and thinks these are pointers for some reason and misaligns
	// the asterisks.

	template <typename T>
	constexpr construct_type assign_traits =
	    construct_type::copy * std::is_copy_assignable<T>::value |
	    construct_type::move * std::is_move_assignable<T>::value;

	// clang-format on

	template <typename T>
	struct disable_conditional : construct_conditional<construct_traits<T>>,
	                             assign_conditional<assign_traits<T>> {};

	inline auto noop(void*, void*) noexcept -> void* { return nullptr; }
	inline auto noop(void*, const void*) -> void* { return nullptr; }
	inline auto throw_noop(void*, void*) noexcept(false) -> void* {
		return nullptr;
	}

	template <construct_type traits>
	struct erased_construct_helper {};

	template <>
	struct erased_construct_helper<construct_type::copy> {
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
	    : erased_construct_helper<construct_type::copy>,
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
	template <typename T>
	auto default_copy(void* dest, const void* from) -> void* {
		return static_cast<void*>(new (dest) T(*static_cast<const T*>(from)));
	}
	template <typename T>
	auto default_move(void* dest, void* from) noexcept(
	    std::is_nothrow_move_constructible<T>::value) -> void* {
		return static_cast<void*>(new (dest)
		                              T(std::move(*static_cast<T*>(from))));
	}

	template <typename T, typename Traits, bool noop = false>
	KBLIB_NODISCARD auto make_ops_t() noexcept
	    -> erased_construct_helper<construct_traits<Traits>> {
		if constexpr (noop) {
			return {};
		}
		static_assert(implies<std::is_copy_constructible<Traits>::value,
		                      std::is_copy_constructible<T>::value>::value,
		              "T must be copy constructible if Traits is.");
		static_assert(
		    implies<std::is_nothrow_move_constructible<Traits>::value,
		            std::is_nothrow_move_constructible<T>::value>::value,
		    "T must be nothrow move constructible if Traits is.");
		static_assert(implies<std::is_move_constructible<Traits>::value,
		                      std::is_move_constructible<T>::value>::value,
		              "T must be move constructible if Traits is.");
		if constexpr (construct_traits<Traits> == construct_type::none) {
			return {};
		} else if constexpr (construct_traits<Traits> == construct_type::copy) {
			return {{&default_copy<T>}};
		} else if constexpr (construct_traits<Traits> == construct_type::move or
		                     construct_traits<Traits> ==
		                         construct_type::throw_move) {
			return {{&default_move<T>}};
		} else {
			return {{&default_copy<T>}, {&default_move<T>}};
		}
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

} // namespace detail

/**
 * @brief A tag type for poly_obj, usable as a Traits type, which disables
 * copying.
 */
struct move_only_t {
	move_only_t() noexcept = default;

	move_only_t(move_only_t&&) noexcept = default;
	move_only_t(const move_only_t&) = delete;

	auto operator=(move_only_t&&) noexcept -> move_only_t& = default;
	auto operator=(const move_only_t&) -> move_only_t& = delete;

	~move_only_t() = default;
};

/**
 * @brief A tag type for poly_obj, usable as a Traits type, which disables
 * copying and moving.
 */
struct no_move_t {
	no_move_t() noexcept = default;

	no_move_t(const no_move_t&) = delete;

	auto operator=(const no_move_t&) -> no_move_t& = delete;

	~no_move_t() = default;
};

/**
 * @brief Inline polymorphic object. Generally mimics the interfaces of
 * std::optional and std::variant.
 *
 * Provides dynamic polymorphism without dynamic allocation. It is copy and move
 * constructible if and only if Obj is. The storage capacity can be overloaded
 * in case derived objects are larger than the base (this is expected to be
 * commonplace).
 *
 * @tparam Obj The base class type which the poly_obj will store.
 * @tparam Capacity The inline capacity allocated for the contained object. May
 * be set larger than sizeof(Obj) to account for larger derived classes.
 * @tparam Traits The type to base copy/move constructibility and assignability
 * of the poly_obj on. poly_obj will not create objects of type Traits, this
 * template parameter is only used in type traits.
 */
template <typename Obj,
          std::size_t Capacity = detail::extract_derived_size<Obj>::value,
          typename Traits = Obj>
class poly_obj
    : detail::disable_conditional<Traits>,
      detail::erased_construct_helper<detail::construct_traits<Traits>> {
 private:
	using disabler = detail::disable_conditional<Traits>;
	using ops_t =
	    detail::erased_construct_helper<detail::construct_traits<Traits>>;

 public:
	static_assert(Capacity >= sizeof(Obj),
	              "Capacity must be large enough for the object type.");
	static_assert(
	    std::has_virtual_destructor<Obj>::value,
	    "Obj must have a virtual destructor to be used as a base class object.");
	static_assert(not std::is_array<Obj>::value,
	              "poly_obj of array type is disallowed.");

	static constexpr std::size_t capacity = Capacity;
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
	constexpr poly_obj(poly_obj&& other) noexcept(
	    std::is_nothrow_move_constructible<Traits>::value)
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
	constexpr poly_obj(Obj&& obj) noexcept(
	    std::is_nothrow_move_constructible<Traits>::value)
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
	    : ops_t(detail::make_ops_t<Obj, Traits>()), valid(true) {
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
	    : ops_t(detail::make_ops_t<Obj, Traits>()), valid(true) {
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
	auto operator=(poly_obj&& other) & noexcept(
	    std::is_nothrow_move_assignable<Traits>::value) -> poly_obj& {
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
	 * sizeof(U) must be less than or equal to Capacity, and must be
	 * copy-constructible and move-constructible if Traits is.
	 *
	 * @tparam U A type publically derived from Obj.
	 * @param args Arguments to pass to the constructor of U.
	 * @return poly_obj A poly_obj<Obj> containing an object of type U.
	 */
	template <typename U, typename... Args>
	KBLIB_NODISCARD static auto make(Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value) -> poly_obj {
		static_assert(sizeof(U) <= Capacity,
		              "U must fit inside of the inline capacity.");
		static_assert(std::is_base_of<Obj, U>::value and
		                  std::is_convertible<U*, Obj*>::value,
		              "Obj must be an accessible base of Obj.");
		static_assert(std::has_virtual_destructor<Obj>::value,
		              "It must be safe to delete a U through an Obj*.");
		static_assert(implies<std::is_copy_constructible<Traits>::value,
		                      std::is_copy_constructible<U>::value>::value,
		              "U must be copy constructible if Traits is.");
		static_assert(implies<std::is_move_constructible<Traits>::value,
		                      std::is_move_constructible<U>::value>::value,
		              "U must be move constructible if Traits is.");
		return {tag<U>{}, std::forward<Args>(args)...};
	}

	/**
	 * @brief Constructs a poly_obj containing an object of derived type.
	 *
	 * This function provides the polymorphism of poly_obj.
	 *
	 * sizeof(U) must be less than or equal to Capacity, and must be
	 * copy-constructible and move-constructible if Traits is.
	 *
	 * @tparam U A type publically derived from Obj.
	 * @param args Arguments to pass to the constructor of U.
	 * @return poly_obj A poly_obj<Obj> containing an object of type U.
	 */
	template <typename U, typename... Args>
	KBLIB_NODISCARD static auto make_aggregate(Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value) -> poly_obj {
		static_assert(sizeof(U) <= Capacity,
		              "U must fit inside of the inline capacity.");
		static_assert(std::is_base_of<Obj, U>::value and
		                  std::is_convertible<U*, Obj*>::value,
		              "Obj must be an accessible base of Obj.");
		static_assert(std::has_virtual_destructor<Obj>::value,
		              "It must be safe to delete a U through an Obj*.");
		static_assert(implies<std::is_copy_constructible<Traits>::value,
		                      std::is_copy_constructible<U>::value>::value,
		              "U must be copy constructible if Traits is.");
		static_assert(implies<std::is_move_constructible<Traits>::value,
		                      std::is_move_constructible<U>::value>::value,
		              "U must be move constructible if Traits is.");
		return {tag<U, true>{}, std::forward<Args>(args)...};
	}

	/*template <typename U = Obj, typename... Args>
	constexpr auto emplace(Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value) -> Obj& {
	   clear();
	   static_cast<ops_t&>(*this) = detail::make_ops_t<U, Traits>();
	   value = new (data) U(std::forward<Args>(args)...);
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
	KBLIB_NODISCARD auto
	operator*() && noexcept(std::is_nothrow_move_constructible<Obj>::value)
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
	KBLIB_NODISCARD auto
	operator*() const&& noexcept(std::is_nothrow_move_constructible<Obj>::value)
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
		return detail::launder(reinterpret_cast<Obj*>(data));
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
		return detail::launder(reinterpret_cast<const Obj*>(data));
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
		return detail::launder(reinterpret_cast<Obj*>(data));
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
		return detail::launder(reinterpret_cast<const Obj*>(data));
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
			get()->~Obj();
			valid = false;
		}
		static_cast<ops_t&>(*this) = {};
	}

	~poly_obj() noexcept {
		if (valid) {
			get()->~Obj();
		}
	}

 private:
	alignas(Obj) byte data[Capacity]{};
	bool valid{};

	template <typename U, bool = false>
	struct tag {};

	template <typename U, typename... Args>
	poly_obj(tag<U>, Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value)
	    : ops_t(detail::make_ops_t<U, Traits>()), valid(true) {
		new (data) U(std::forward<Args>(args)...);
	}

	template <typename U, typename... Args>
	poly_obj(tag<U, true>, Args&&... args) noexcept(
	    std::is_nothrow_constructible<U, Args&&...>::value)
	    : ops_t(detail::make_ops_t<U, Traits>()), valid(true) {
		new (data) U{std::forward<Args>(args)...};
	}
};

template <typename T, typename D = T, std::size_t Capacity = sizeof(D),
          typename Traits = T, typename... Args>
KBLIB_NODISCARD auto make_poly_obj(Args&&... args)
    -> poly_obj<T, Capacity, Traits> {
	return poly_obj<T, Capacity, Traits>::template make<D>(
	    std::forward<Args>(args)...);
}

} // namespace kblib

#endif // POLY_OBJ_H
