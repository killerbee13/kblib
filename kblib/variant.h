#ifndef KBLIB_VARIANT_H
#define KBLIB_VARIANT_H

#include "convert.h"
#include "logic.h"
#include "simple.h"
#include "tdecl.h"
#include <cstddef>
#include <new>

#if KBLIB_USE_CXX17
#include <variant>
#endif

namespace kblib {

#if KBLIB_USE_CXX17

/**
 * @brief Lexically converts the value of v (no matter its type) to type To.
 *
 * @deprecated Use lexical_coerce instead, as it more clearly expresses intent.
 *
 * @param v A variant to coerce.
 * @return To The type to coerce to.
 */
template <typename To, typename... Ts>
[[deprecated("Use new lexical_coerce instead.")]] To
coerce(const std::variant<Ts...>& v) {
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
To static_coerce(const std::variant<Ts...>& v) {
	return std::visit([](const auto& t) -> To { return static_cast<To>(t); }, v);
}

/**
 * @brief Lexically converts the value of v (no matter its type) to type To.
 *
 * @param v A variant to coerce.
 * @return To The type to coerce to.
 */
template <typename To, typename... Ts>
To lexical_coerce(const std::variant<Ts...>& v) {
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
visitor(Ts...)->visitor<Ts...>;

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
	auto indexed_visitor_impl(std::index_sequence<Is...>) {
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
decltype(auto) visit_indexed(Variant&& variant, Fs&&... fs) {
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
To variant_cast(From&& v) {
	static_assert(
	    detail::contains_types_v<detail::tuple_type_t<std::decay_t<To>>,
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
auto visit(V&& v, F&& f, Fs&&... fs) {
	return std::visit(visitor{std::forward<F>(f), std::forward<Fs>(fs)...},
	                  std::forward<V>(v));
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
KBLIB_NODISCARD auto visit(V&& v) {
	return [v](auto... fs) -> decltype(auto) {
		return std::visit(visitor{fs...}, v);
	};
}

namespace detail {
   enum class construct_type {
		none = 0,
		copy = 1,
		move = 2,
		both = 3,

	};
	constexpr construct_type operator|(construct_type l, construct_type r) {
		return static_cast<construct_type>(etoi(l) | etoi(r));
	}

	constexpr construct_type operator&(construct_type l, construct_type r) {
		return static_cast<construct_type>(etoi(l) & etoi(r));
	}

	constexpr construct_type operator*(construct_type l, bool r) {
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

	template <typename T>
	constexpr construct_type construct_traits =
	    construct_type::copy* std::is_copy_constructible_v<T> |
	    construct_type::move* std::is_move_constructible_v<T>;

	template <construct_type traits>
	struct assign_conditional;

	template <>
	struct assign_conditional<construct_type::none> {
		assign_conditional() noexcept = default;
		assign_conditional& operator=(const assign_conditional&) = delete;
	};

	template <>
	struct assign_conditional<construct_type::copy> {
		assign_conditional() noexcept = default;
		assign_conditional&
		operator=(const assign_conditional&) noexcept = default;
		assign_conditional& operator=(assign_conditional&&) = delete;
	};

	template <>
	struct assign_conditional<construct_type::move> {
		assign_conditional() noexcept = default;
		assign_conditional& operator=(assign_conditional&&) noexcept = default;
	};

	template <>
	struct assign_conditional<construct_type::both> {};

	// clang-format off

// It gets confused and thinks these are pointers for some reason and misaligns
// the asterisks.

template <typename T>
constexpr construct_type assign_traits =
    construct_type::copy * std::is_copy_assignable_v<T> |
    construct_type::move * std::is_move_assignable_v<T>;

   // clang-format on

   template <typename T>
   struct disable_conditional : construct_conditional<construct_traits<T>>,
	                             assign_conditional<assign_traits<T>> {};

	inline void* noop(void*, void*) { return nullptr; }
	inline void* noop(void*, const void*) { return nullptr; }

	template <construct_type traits>
	struct erased_construct_helper {};

	template <>
	struct erased_construct_helper<construct_type::copy> {
		alias<void* (*)(void*, const void*)> copy = &noop;
	};

	template <>
	struct erased_construct_helper<construct_type::move> {
		alias<void* (*)(void*, void*)> move = &noop;
	};

	template <>
	struct erased_construct_helper<construct_type::both>
	    : erased_construct_helper<construct_type::copy>,
	      erased_construct_helper<construct_type::move> {};

	template <typename T, typename hash = void>
	struct erased_hash_t {};

	template <typename T>
	struct erased_hash_t<T, std::void_t<std::invoke_result_t<std::hash<T>, T>>> {
		static std::size_t default_hash(void* obj) {
			return std::hash<T>()(*static_cast<const T*>(obj));
		}

		alias<std::size_t (*)(void*)> hash = &default_hash;
	};
	template <typename T>
	void* default_copy(void* dest, const void* from) {
		return static_cast<void*>(new (dest) T(*static_cast<const T*>(from)));
	}
	template <typename T>
	void* default_move(void* dest, void* from) {
		return static_cast<void*>(new (dest)
		                              T(std::move(*static_cast<T*>(from))));
	}

	template <typename T, typename Traits, bool noop = false>
	erased_construct_helper<construct_traits<Traits>> make_ops_t() {
		if constexpr (noop) {
			return {};
		}
		static_assert(implies_v<std::is_copy_constructible_v<Traits>,
		                        std::is_copy_constructible_v<T>>,
		              "T must be copy constructible if Traits is.");
		static_assert(implies_v<std::is_move_constructible_v<Traits>,
		                        std::is_move_constructible_v<T>>,
		              "T must be move constructible if Traits is.");
		if constexpr (construct_traits<Traits> == construct_type::none) {
			return {};
		} else if constexpr (construct_traits<Traits> == construct_type::copy) {
			return {{&default_copy<T>}};
		} else if constexpr (construct_traits<Traits> == construct_type::move) {
			return {{&default_move<T>}};
		} else {
			return {{&default_copy<T>}, {&default_move<T>}};
		}
	}

} // namespace detail

/**
 * @brief A tag type for poly_obj, usable as a Traits type, which disables
 * copying.
 */
struct move_only_t {
	move_only_t() = default;

	move_only_t(move_only_t&&) = default;
	move_only_t(const move_only_t&) = delete;

	move_only_t& operator=(move_only_t&&) = default;
	move_only_t& operator=(const move_only_t&) = delete;
};

/**
 * @brief A tag type for poly_obj, usable as a Traits type, which disables
 * copying and moving.
 */
struct no_move_t {
	no_move_t() = default;

	no_move_t(const no_move_t&) = delete;

	no_move_t& operator=(const no_move_t&) = delete;
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
template <typename Obj, std::size_t Capacity = sizeof(Obj),
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
	    std::has_virtual_destructor_v<Obj>,
	    "Obj must have a virtual destructor to be used as a base class object.");
	static_assert(!std::is_array_v<Obj>,
	              "poly_obj of array type is disallowed.");
	/**
	 * @brief The default constructor does not construct any contained object.
	 */
	constexpr poly_obj() = default;
	/**
	 * @brief Explicitly do not construct an object.
	 */
	constexpr poly_obj(std::nullptr_t) : poly_obj() {}
	/**
	 * @brief Constructs a copy of other.
	 *
	 * This function can only be called if Traits is copy-constructible.
	 *
	 * @param other A poly_obj to copy from.
	 */
	constexpr poly_obj(const poly_obj& other) noexcept(
	    std::is_nothrow_copy_constructible<Obj>::value)
	    : disabler(other), ops_t(other) {
		if (other.value) {
			value = static_cast<Obj*>(this->copy(data, other.get()));
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
	    std::is_nothrow_move_constructible<Obj>::value)
	    : disabler(std::move(other)), ops_t(std::move(other)) {
		if (other.value) {
			value = static_cast<Obj*>(this->move(data, other.get()));
		}
	}

	/**
	 * @brief Copy-constructs the contained object from obj.
	 *
	 * This function can only be called is Traits is copy-constructible.
	 *
	 * @param obj The object to copy.
	 */
	constexpr poly_obj(const Obj& obj) noexcept(
	    std::is_nothrow_copy_constructible<Obj>::value)
	    : value(new (data) Obj(obj)) {}
	/**
	 * @brief Move-constructs the contained object from obj.
	 *
	 * This function can only be called if Traits is move-constructible.
	 *
	 * @param obj The object to move from.
	 */
	constexpr poly_obj(Obj&& obj) noexcept(
	    std::is_nothrow_move_constructible<Obj>::value)
	    : value(new (data) Obj(std::move(obj))) {}

	/**
	 * @brief Constructs the contained object in-place without copying or moving.
	 *
	 * @param args Arguments to be passed to the constructor of Obj.
	 */
	template <typename... Args,
	          typename std::enable_if_t<std::is_constructible_v<Obj, Args...>,
	                                    int> = 0>
	constexpr explicit poly_obj(std::in_place_t, Args&&... args)
	    : ops_t(detail::make_ops_t<Obj, Traits>()),
	      value(new (data) Obj(std::forward<Args>(args)...)) {}

	/**
	 * @brief Destroys the contained object, if any, and then copies other as in
	 * the copy constructor.
	 *
	 * @param other A poly_obj to copy from.
	 * @return poly_obj& *this.
	 */
	poly_obj& operator=(const poly_obj& other) & {
		clear();
		static_cast<ops_t&>(*this) = other;
		value = static_cast<Obj*>(
		    this->copy(data, static_cast<const void*>(other.value)));
		return *this;
	}

	/**
	 * @brief Destroys the contained object, if any, and then moves from other as
	 * in the move constructor.
	 *
	 * @param other A poly_obj to move from.
	 * @return poly_obj& *this.
	 */
	poly_obj& operator=(poly_obj&& other) & {
		clear();
		static_cast<ops_t&>(*this) = other;
		value =
		    static_cast<Obj*>(this->move(data, static_cast<void*>(other.value)));
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
	static poly_obj make(Args&&... args) {
		static_assert(sizeof(U) <= Capacity,
		              "U must fit inside of the inline capacity.");
		static_assert(std::is_base_of_v<Obj, U> &&
		                  std::is_convertible_v<U*, Obj*>,
		              "Obj must be an accessible base of Obj.");
		static_assert(std::has_virtual_destructor_v<Obj>,
		              "It must be safe to delete a U through an Obj*.");
		static_assert(implies_v<std::is_copy_constructible_v<Traits>,
		                        std::is_copy_constructible_v<U>>,
		              "U must be copy constructible if Traits is.");
		static_assert(implies_v<std::is_move_constructible_v<Traits>,
		                        std::is_move_constructible_v<U>>,
		              "U must be move constructible if Traits is.");
		return {tag<U>{}, std::forward<Args>(args)...};
	}

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
	Obj& operator*() & noexcept {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return *value;
	}
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
	const Obj& operator*() const& noexcept {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return *value;
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
	Obj&& operator*() &&
	    noexcept(std::is_nothrow_move_constructible<Obj>::value) {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return std::move(*value);
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
	const Obj&& operator*() const&& noexcept(
	    std::is_nothrow_move_constructible<Obj>::value) {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return std::move(*value);
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
	Obj* get() & noexcept {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return value;
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
	const Obj* get() const& noexcept {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return value;
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
	Obj* operator->() & noexcept {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return value;
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
	const Obj* operator->() const& noexcept {
#if __has_builtin(__builtin_assume)
		__builtin_assume(value == reinterpret_cast<const Obj*>(data));
#endif
		return value;
	}

	/**
	 * @brief Invokes the container function object, if Obj is a callable type.
	 *
	 * Invokes the contained object, if it exists. If it does not exist, the
	 * behavior is undefined.
	 *
	 * @param args The arguments to forward to the function.
	 * @return std::invoke_result_t<const Obj&, Args&&...> The return value of
	 * the function.
	 */
	template <typename... Args>
	auto operator()(Args&&... args) noexcept(
	    std::is_nothrow_invocable_v<Obj&, Args&&...>)
	    -> std::invoke_result_t<Obj&, Args&&...> {
		return std::invoke(**this, std::forward<Args>(args)...);
	}
	/**
	 * @brief Invokes the container function object, if Obj is a callable type.
	 *
	 * Invokes the contained object, if it exists. If it does not exist, the
	 * behavior is undefined.
	 *
	 * @param args The arguments to forward to the function.
	 * @return std::invoke_result_t<const Obj&, Args&&...> The return value of
	 * the function.
	 */
	template <typename... Args>
	auto operator()(Args&&... args) const
	    noexcept(std::is_nothrow_invocable_v<const Obj&, Args&&...>)
	        -> std::invoke_result_t<const Obj&, Args&&...> {
		return std::invoke(**this, std::forward<Args>(args)...);
	}

	/**
	 * @brief Check if the poly_obj contains a value.
	 */
	bool has_value() const { return value; }

	/**
	 * @brief Empties the poly_obj, reverting to a default-constructed state.
	 */
	void clear() {
		if (value) {
			value->~Obj();
			value = nullptr;
		}
		static_cast<ops_t&>(*this) = {};
	}

	~poly_obj() noexcept {
		if (value) {
			value->~Obj();
		}
	}

 private:
	alignas(Obj) std::byte data[Capacity];
	Obj* value = nullptr;

	template <typename U>
	struct tag {};

	template <typename U, typename... Args>
	poly_obj(tag<U>, Args&&... args)
	    : ops_t(detail::make_ops_t<U, Traits>()),
	      value(new (data) U(std::forward<Args>(args)...)) {}
};

template <typename T, typename D = T, std::size_t Capacity = sizeof(D),
          typename Traits = T, typename... Args>
poly_obj<T, Capacity, Traits> make_poly_obj(Args&&... args) {
	return poly_obj<T, Capacity, Traits>::template make<D>(
	    std::forward<Args>(args)...);
}

#endif // KBLIB_USE_CXX17

} // namespace kblib

#endif // KBLIB_VARIANT_H
