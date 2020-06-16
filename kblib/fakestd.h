#ifndef KBLIB_FAKESTD_H
#define KBLIB_FAKESTD_H

#include "tdecl.h"

#include <array>
#include <memory>
#include <type_traits>
#include <utility>

// This header provides some features of C++17 <type_traits> for C++14; see
// below.
#ifndef KBLIB_FAKESTD
#define KBLIB_FAKESTD (__cplusplus < 201703L)
#endif

namespace kblib {

template <bool B, typename T = void>
using enable_if_t = typename std::enable_if<B, T>::type;

template <typename T>
using decay_t = typename std::decay<T>::type;

namespace detail {

   template <typename F, typename... Args,
             enable_if_t<!std::is_member_pointer<decay_t<F>>::value, int> = 0>
   constexpr decltype(auto) do_invoke(F&& f, Args&&... args) noexcept(
	    noexcept(std::forward<F>(f)(std::forward<Args>(args)...))) {
		return std::forward<F>(f)(std::forward<Args>(args)...);
	}

	template <typename F, typename Object, typename... Args,
	          enable_if_t<!std::is_pointer<decay_t<Object>>::value &&
	                          std::is_member_function_pointer<F>::value,
	                      int> = 0>
	constexpr decltype(auto)
	do_invoke(F f, Object&& obj, Args&&... args) noexcept(
	    noexcept((obj.*f)(std::forward<Args>(args)...))) {
		return (obj.*f)(std::forward<Args>(args)...);
	}

	template <typename F, typename Pointer, typename... Args,
	          enable_if_t<std::is_pointer<Pointer>::value &&
	                          std::is_member_function_pointer<F>::value,
	                      int> = 0>
	constexpr decltype(auto)
	do_invoke(F f, Pointer ptr, Args&&... args) noexcept(
	    noexcept((ptr->*f)(std::forward<Args>(args)...))) {
		return (ptr->*f)(std::forward<Args>(args)...);
	}

	template <typename Member, typename Object,
	          enable_if_t<!std::is_pointer<decay_t<Object>>::value &&
	                          std::is_member_object_pointer<Member>::value,
	                      int> = 0>
	constexpr decltype(auto) do_invoke(Member mem, Object&& obj) noexcept {
		return std::forward<Object>(obj).*mem;
	}

	template <typename Member, typename Pointer,
	          enable_if_t<std::is_pointer<Pointer>::value &&
	                          std::is_member_object_pointer<Member>::value,
	                      int> = 0>
	constexpr decltype(auto) do_invoke(Member mem, Pointer ptr) noexcept {
		return ptr.*mem;
	}
} // namespace detail

template <typename F, typename... Args>
constexpr decltype(auto) invoke(F&& f, Args&&... args) noexcept(noexcept(
    detail::do_invoke(std::forward<F>(f), std::forward<Args>(args)...))) {
#if KBLIB_USE_CXX17
	return std::apply(std::forward<F>(f),
	                  std::forward_as_tuple(std::forward<Args>(args)...));
#else
	return detail::do_invoke(std::forward<F>(f), std::forward<Args>(args)...);
#endif
}

#if KBLIB_FAKESTD
namespace fakestd { // C++14 implementation of C++17 void_t, invoke_result,
                    // (partially) is_invocable, and is_nothrow_swappable
   using std::swap;

	namespace detail {

	   template <typename AlwaysVoid, typename, typename...>
	   struct invoke_result {};
		template <typename F, typename... Args>
		struct invoke_result<decltype(void(invoke(std::declval<F>(),
		                                          std::declval<Args>()...))),
		                     F, Args...> {
			using type =
			    decltype(invoke(std::declval<F>(), std::declval<Args>()...));
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
		          !std::is_same<decltype(is_referenceable_impl::test<Tp>(0)),
		                        two>::value> {};

		template <class Tp, class Up = Tp,
		          bool NotVoid =
		              !std::is_void<Tp>::value && !std::is_void<Up>::value>
		struct swappable_with {
			template <class LHS, class RHS>
			static decltype(swap(std::declval<LHS>(), std::declval<RHS>()))
			test_swap(int);
			template <class, class>
			static nat test_swap(long);

			// Extra parens are needed for the C++03 definition of decltype.
			typedef decltype((test_swap<Tp, Up>(0))) swap1;
			typedef decltype((test_swap<Up, Tp>(0))) swap2;

			static const bool value = !std::is_same<swap1, nat>::value &&
			                          !std::is_same<swap2, nat>::value;
		};

		template <class Tp, class Up>
		struct swappable_with<Tp, Up, false> : std::false_type {};

		template <class Tp, class Up = Tp,
		          bool Swappable = swappable_with<Tp, Up>::value>
		struct nothrow_swappable_with {
			static const bool value =
			    noexcept(swap(std::declval<Tp>(), std::declval<Up>())) &&
			    noexcept(swap(std::declval<Up>(), std::declval<Tp>()));
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

#if KBLIB_USE_CXX17

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
	inline constexpr bool is_swappable_with_v = is_swappable_with<Tp, Up>::value;

	template <class Tp>
	inline constexpr bool is_swappable_v = is_swappable<Tp>::value;

	template <class Tp, class Up>
	inline constexpr bool is_nothrow_swappable_with_v =
	    is_nothrow_swappable_with<Tp, Up>::value;

	template <class Tp>
	inline constexpr bool is_nothrow_swappable_v =
	    is_nothrow_swappable<Tp>::value;

#endif

	namespace detail {
	   template <typename F>
	   struct not_fn_t {
			explicit not_fn_t(F&& f) : fd(std::forward<F>(f)) {}
			not_fn_t(const not_fn_t&) = default;
			not_fn_t(not_fn_t&&) = default;

			template <class... Args>
			auto operator()(Args&&... args) & -> decltype(
			    !std::declval<invoke_result_t<std::decay_t<F>&, Args...>>()) {
				return !invoke(fd, std::forward<Args>(args)...);
			}

			template <class... Args>
			auto operator()(Args&&... args) const& -> decltype(
			    !std::declval<
			        invoke_result_t<std::decay_t<F> const&, Args...>>()) {
				return !invoke(std::move(fd), std::forward<Args>(args)...);
			}

			std::decay_t<F> fd;
		};
	} // namespace detail

	template <typename F>
	detail::not_fn_t<F> not_fn(F&& f) {
		return detail::not_fn_t<F>(std::forward<F>(f));
	}

	struct in_place_t {
		explicit in_place_t() = default;
	};
	static constexpr in_place_t in_place{};

	template <class ForwardIt>
	constexpr ForwardIt max_element(ForwardIt first, ForwardIt last) {
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
	constexpr ForwardIt max_element(ForwardIt first, ForwardIt last,
	                                Compare comp) {
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
	constexpr std::size_t size(const T (&)[N]) noexcept {
		return N;
	}

} // namespace fakestd
#else
namespace fakestd = std;
#endif

template <bool>
struct void_if {};

template <>
struct void_if<true> {
	using type = void;
};
template <bool b>
using void_if_t = typename void_if<b>::type;

using fakestd::void_t;

template <typename... Ts>
struct unary_identity {};

template <typename T>
struct unary_identity<T> {
	using type = T;
};

template <typename... Ts>
using unary_identity_t = typename unary_identity<Ts...>::type;

// metafunction_success:
// SFINAE detector for a ::type member type
template <typename T, typename = void>
struct metafunction_success : std::false_type {};

template <typename T>
struct metafunction_success<T, void_t<typename T::type>> : std::true_type {};

template <typename... T>
struct is_callable : metafunction_success<fakestd::invoke_result<T...>> {};

/**
 * @brief Essentially just like std::enable_if, but with a different name that
 * makes it clearer what it does in the context of return type SFINAE.
 */
template <bool V, typename T>
struct return_assert {};

template <typename T>
struct return_assert<true, T> {
	using type = T;
};

template <bool V, typename T>
using return_assert_t = typename return_assert<V, T>::type;

template <typename T>
std::unique_ptr<T> to_unique(T* p) {
	return std::unique_ptr<T>(p);
}
template <typename T, typename D>
std::unique_ptr<T, D> to_unique(T* p, D&& d) {
	return std::unique_ptr<T, D>(p, d);
}

template <typename I>
constexpr std::make_unsigned_t<I> to_unsigned(I x) {
	return static_cast<std::make_unsigned_t<I>>(x);
}
template <typename I>
constexpr std::make_signed_t<I> to_signed(I x) {
	return static_cast<std::make_signed_t<I>>(x);
}

// Cast argument to equivalently-sized type with the same signedness as the
// template parameter
template <typename A, typename F>
KBLIB_NODISCARD constexpr enable_if_t<std::is_integral<A>::value &&
                                          std::is_integral<F>::value &&
                                          std::is_signed<A>::value,
                                      std::make_signed_t<F>>
signed_cast(F x) {
	return to_signed(x);
}

template <typename A, typename F>
KBLIB_NODISCARD constexpr enable_if_t<std::is_integral<A>::value &&
                                          std::is_integral<F>::value &&
                                          std::is_unsigned<A>::value,
                                      std::make_unsigned_t<F>>
signed_cast(F x) {
	return to_unsigned(x);
}

template <typename T>
struct has_member_swap {
	using yes = char (&)[1];
	using no = char (&)[2];

	template <typename C>
	static yes check(decltype(&C::swap));
	template <typename>
	static no check(...);

	constexpr static bool value = sizeof(check<T>(0)) == sizeof(yes);
};

template <typename T, typename = void>
struct is_tuple_like {
	constexpr static bool value = false;
};

template <typename T>
struct is_tuple_like<T, void_t<typename std::tuple_element<0, T>::type>> {
	constexpr static bool value = true;
};

template <typename T,
          enable_if_t<!has_member_swap<T>::value && !is_tuple_like<T>::value,
                      int> = 0>
constexpr void
swap(T& a, T& b) noexcept(std::is_nothrow_move_constructible<T>::value&&
                              std::is_nothrow_move_assignable<T>::value) {
	auto tmp = std::move(a);
	a = std::move(b);
	b = std::move(tmp);
	return;
}

template <
    typename T,
    enable_if_t<has_member_swap<T>::value && !is_tuple_like<T>::value, int> = 0>
constexpr void swap(T& a, T& b) noexcept(noexcept(a.swap(b))) {
	a.swap(b);
	return;
}

template <typename T, std::size_t N>
constexpr void
swap(T (&a)[N],
     T (&b)[N]) noexcept(std::is_nothrow_move_constructible<T>::value&&
                             std::is_nothrow_move_assignable<T>::value) {
	for (std::size_t i = 0; i < N; ++i) {
		swap(a[i], b[i]);
	}
}
namespace detail {

   template <typename... Ts>
   constexpr void ignore(Ts&&... /*unused*/) noexcept {}

	template <typename T, std::size_t... Is>
	constexpr void
	swap_tuple_impl(T& a, T& b, std::index_sequence<Is...> /*unused*/) noexcept(
	    noexcept(ignore(((void)swap(std::get<Is>(a), std::get<Is>(b)), 0)...))) {
		ignore(((void)swap(std::get<Is>(a), std::get<Is>(b)), 0)...);
	}

} // namespace detail

template <typename T, std::size_t N = std::tuple_size<T>::value>
constexpr void swap(T& a, T& b) noexcept(
    noexcept(detail::swap_tuple_impl(a, b, std::make_index_sequence<N>{}))) {
	detail::swap_tuple_impl(a, b, std::make_index_sequence<N>{});
}

#if KBLIB_USE_CXX17

namespace detail {

   template <typename T>
   constexpr std::intmax_t max_val = std::numeric_limits<T>::max();

	constexpr unsigned long long msb(unsigned long long x) {
		x |= (x >> 1u);
		x |= (x >> 2u);
		x |= (x >> 4u);
		x |= (x >> 8u);
		x |= (x >> 16u);
		x |= (x >> 32u);
		return (x & ~(x >> 1u));
	}

	template <typename Num>
	constexpr Num msb_possible() {
		return std::numeric_limits<Num>::max() ^
		       (std::numeric_limits<Num>::max() >> 1u);
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
		constexpr static auto get() {
			static_assert(I < sizeof...(Vals), "key not found");
			if constexpr (Comp{}(key, element<I>::key)) {
				return tag<typename element<I>::value>{};
			} else {
				return get<key, I + 1>();
			}
		}

		template <Key key, typename Default = void, std::size_t I = 0>
		constexpr static auto get_default() {
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
	using make_smap_el =
	    type_map_el<static_cast<std::intmax_t>(msb_possible<N>()), N>;

	template <typename T>
	struct next_larger_signed {
		static_assert(max_val<T> < max_val<std::intmax_t>,
		              "Cannot safely promote intmax_t.");
		struct false_compare {
			template <typename U>
			constexpr bool operator()(U, U) {
				return true;
			}
		};

		using ints_map = type_map<
		    std::intmax_t, std::less<std::intmax_t>,
		    make_smap_el<std::int_least8_t>, make_smap_el<std::int_least16_t>,
		    make_smap_el<std::int_least32_t>, make_smap_el<std::int_least64_t>,
		    make_smap_el<std::intmax_t>>;

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
KBLIB_NODISCARD constexpr safe_signed_t<N> signed_promote(N x) noexcept {
	return static_cast<safe_signed_t<N>>(x);
}

#endif

template <typename C, typename T,
          bool = std::is_const<typename std::remove_reference<C>::type>::value>
struct copy_const {
	using type = T;
};

template <typename C, typename T>
struct copy_const<C, T, true> {
	using type = const T;
};

template <typename C, typename T>
struct copy_const<C, T&, true> {
	using type = const T&;
};

template <typename C, typename T>
struct copy_const<C, T&&, true> {
	using type = const T&&;
};

template <typename C, typename V>
using copy_const_t = typename copy_const<C, V>::type;

template <typename T, typename = void>
struct value_detected : std::false_type {};

template <typename T>
struct value_detected<T, void_t<typename T::value_type>> : std::true_type {};

template <typename T>
constexpr bool value_detected_v = value_detected<T>::value;

template <typename T, typename = void>
struct key_detected : std::false_type {};

template <typename T>
struct key_detected<T, void_t<typename T::key_type>> : std::true_type {};

template <typename T>
constexpr bool key_detected_v = key_detected<T>::value;

template <typename T, typename = void>
struct mapped_detected : std::false_type {};

template <typename T>
struct mapped_detected<T, void_t<typename T::mapped_type>> : std::true_type {};

template <typename T>
constexpr bool mapped_detected_v = mapped_detected<T>::value;

template <typename T, typename = void>
struct hash_detected : std::false_type {};

template <typename T>
struct hash_detected<T, void_t<typename T::hasher>> : std::true_type {};

template <typename T>
constexpr bool hash_detected_v = hash_detected<T>::value;

template <typename Container, bool = key_detected_v<Container>,
          typename T = typename Container::value_type>
struct value_type_linear {};

template <typename Container>
struct value_type_linear<Container, false, typename Container::value_type> {
	using type = typename Container::value_type;
};

template <typename Container>
using value_type_linear_t = typename value_type_linear<Container>::type;

template <typename Container>
struct is_linear_container {
	constexpr static bool value =
	    value_detected_v<Container> && !key_detected_v<Container>;
};

template <typename Container>
constexpr static bool is_linear_container_v =
    value_detected_v<Container> && !key_detected_v<Container>;

template <typename Container, bool = key_detected_v<Container>,
          bool = mapped_detected_v<Container>>
struct key_type_setlike {};

template <typename Container>
struct key_type_setlike<Container, true, false> {
	using type = typename Container::key_type;
};

template <typename Container>
using key_type_setlike_t = typename key_type_setlike<Container>::type;

template <typename Container>
constexpr static bool is_setlike_v =
    key_detected_v<Container> && !mapped_detected_v<Container>;

template <class InputIt1, class InputIt2>
constexpr bool equal(InputIt1 first1, InputIt1 last1, InputIt2 first2) {
	for (; first1 != last1; ++first1, ++first2) {
		if (!(*first1 == *first2)) {
			return false;
		}
	}
	return true;
}

template <typename C>
constexpr auto size(const C& c) -> decltype(c.size()) {
	return c.size();
}

template <typename T, std::size_t N>
constexpr std::size_t size(const T (&)[N]) noexcept {
	return N;
}

template <class InputIt1, class InputIt2>
KBLIB_NODISCARD constexpr bool
lexicographical_compare(InputIt1 first1, InputIt1 last1, InputIt2 first2,
                        InputIt2 last2) {
	for (; (first1 != last1) && (first2 != last2); ++first1, (void)++first2) {
		if (*first1 < *first2)
			return true;
		if (*first2 < *first1)
			return false;
	}
	return (first1 == last1) && (first2 != last2);
}

namespace detail {
   template <typename D, typename T, typename V = void>
   struct filter_deleter_pointer {
		using type = T*;
	};

	template <typename D, typename T>
	struct filter_deleter_pointer<D, T, void_t<typename D::pointer>> {
		using type = typename std::remove_reference<D>::type::pointer;
	};

	template <typename D, typename T>
	using filter_deleter_pointer_t = typename filter_deleter_pointer<D, T>::type;

	template <typename T,
	          bool = std::is_class<T>::value&& std::is_empty<T>::value &&
	                 !std::is_final<T>::value,
	          bool =
	              std::is_object<typename std::remove_reference<T>::type>::value>
	struct as_base_class;

	template <typename T>
	struct as_base_class<T, false, true> {
		T base_;
		T& base() noexcept { return base_; }
		const T& base() const noexcept { return base_; }
	};

	template <typename T>
	struct as_base_class<T, true, true> : T {
		T& base() noexcept { return *this; }
		const T& base() const noexcept { return *this; }
	};

	template <typename R, typename A, bool E>
	struct as_base_class<R (&)(A) noexcept(E), false, false> {
		using type = R(A) noexcept(E);
		type* base_;
		type& base() const noexcept { return *base_; }
	};

	template <typename T, bool B>
	struct as_base_class<T&, B, true> {
		std::reference_wrapper<T> base_;
		T& base() noexcept { return base_; }
		const T& base() const noexcept { return base_; }
	};
} // namespace detail

// cond_ptr: A pointer which can either uniquely own its referent, or which can
// be a non-owning reference. Note that custom deleter support is not present;
// however it will not implicitly strip a deleter from a unique_ptr.

template <typename T, typename Deleter = std::default_delete<T>>
class cond_ptr : private detail::as_base_class<Deleter> {
	using d_base = detail::as_base_class<Deleter>;

 public:
	using pointer = detail::filter_deleter_pointer_t<Deleter, T>;
	using element_type = T;
	using deleter_type = Deleter;

	static_assert(std::is_nothrow_invocable<Deleter&, pointer>::value,
	              "cond_ptr<T> requires that get_deleter not throw exceptions.");

	using unique = std::unique_ptr<T, Deleter>;

	cond_ptr() noexcept = default;
	cond_ptr(std::nullptr_t) noexcept {}

	explicit cond_ptr(T* p, bool owner = false,
	                  std::decay_t<Deleter> del = {}) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(owner) {}
	explicit cond_ptr(T* p, std::decay_t<Deleter> del) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(false) {}
	cond_ptr(unique&& p) noexcept
	    : d_base{p.get_deleter()}, ptr_(p.release()), owns_(ptr_) {}

	cond_ptr(const cond_ptr& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_), owns_(false) {}
	cond_ptr(cond_ptr&& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_),
	      owns_(std::exchange(other.owns_, false)) {}

	cond_ptr& operator=(const cond_ptr& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = false;
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(cond_ptr&& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = rhs.owns();
		static_cast<d_base&>(*this) = {std::move(rhs.get_deleter())};
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(unique&& rhs) {
		static_cast<d_base&>(*this) = {std::move(rhs.get_deleter())};
		ptr_ = rhs.release();
		owns_ = bool(ptr_);
		return *this;
	}

	/**
	 * @brief Transfers ownership to a unique_ptr if possible. If *this is not
	 * owning, returns nullptr.
	 *
	 * @post If ownership was transferred, *this is null. Otherwise, does
	 * nothing.
	 *
	 * @return std::unique_ptr<T, Deleter> Either a pointer which owns what *this
	 * owned, or a null pointer.
	 */
	unique to_unique() && noexcept {
		if (owns_) {
			return {release(), std::move(get_deleter())};
		} else {
			return {nullptr, get_deleter()};
		}
	}

	explicit operator unique() && noexcept {
		return std::move(*this).to_unique();
	}

	~cond_ptr() noexcept {
		if (owns_ && ptr_) {
			get_deleter()(ptr_);
		}
	}

	KBLIB_NODISCARD cond_ptr weak() const& noexcept {
		return cond_ptr{ptr_, false};
	}

	bool owns() const noexcept { return owns_; }
	KBLIB_NODISCARD T* release() & noexcept {
		owns_ = false;
		return std::exchange(ptr_, nullptr);
	}

	Deleter& get_deleter() noexcept { return this->d_base::base(); }

	const Deleter& get_deleter() const noexcept { return this->d_base::base(); }

	void reset(T* p = nullptr, bool owner = false,
	           std::decay_t<Deleter> del = {}) &
	    noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = owner;
		get_deleter() = std::move(del);
	}
	void reset(T* p, std::decay_t<Deleter> del = {}) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = false;
		get_deleter() = std::move(del);
	}

	void swap(cond_ptr& other) {
		std::swap(ptr_, other.ptr_);
		std::swap(owns_, other.owns_);
		std::swap(get_deleter(), other.get_deleter());
	}

	KBLIB_NODISCARD T* get() & noexcept { return ptr_; }

	KBLIB_NODISCARD const T* get() const& noexcept { return ptr_; }

	explicit operator bool() const noexcept { return ptr_; }

	KBLIB_NODISCARD T& operator*() & noexcept { return *ptr_; }

	KBLIB_NODISCARD const T& operator*() const& noexcept { return *ptr_; }

	KBLIB_NODISCARD T* operator->() & noexcept { return ptr_; }

	KBLIB_NODISCARD const T* operator->() const& noexcept { return ptr_; }

	friend constexpr bool operator==(const cond_ptr& lhs, const cond_ptr& rhs) {
		return lhs.ptr_ == rhs.ptr_;
	}

	friend constexpr bool operator==(const unique& lhs, const cond_ptr& rhs) {
		return lhs.get() == rhs.ptr_;
	}

	friend constexpr bool operator==(const cond_ptr& lhs, const unique& rhs) {
		return lhs.ptr_ == rhs.get();
	}

 private:
	T* ptr_ = nullptr;
	bool owns_ = false;
};

template <typename T, typename Deleter>
class cond_ptr<T[], Deleter> : private detail::as_base_class<Deleter> {
	using d_base = detail::as_base_class<Deleter>;

 public:
	using pointer = detail::filter_deleter_pointer_t<Deleter, T>;
	using element_type = T;
	using deleter_type = Deleter;

	static_assert(std::is_nothrow_invocable<Deleter&, pointer>::value,
	              "cond_ptr<T> requires that get_deleter not throw exceptions.");

	using unique = std::unique_ptr<T[], Deleter>;

	cond_ptr() noexcept = default;
	cond_ptr(std::nullptr_t) noexcept {}

	explicit cond_ptr(T* p, bool owner = false,
	                  std::decay_t<Deleter> del = {}) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(owner) {}
	explicit cond_ptr(T* p, std::decay_t<Deleter> del) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(false) {}
	cond_ptr(unique&& p) noexcept
	    : d_base{p.get_deleter()}, ptr_(p.release()), owns_(ptr_) {}

	cond_ptr(const cond_ptr& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_), owns_(false) {}
	cond_ptr(cond_ptr&& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_),
	      owns_(std::exchange(other.owns_, false)) {}

	cond_ptr& operator=(const cond_ptr& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = false;
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(cond_ptr&& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = rhs.owns();
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(unique&& rhs) {
		ptr_ = rhs.release();
		owns_ = bool(ptr_);
		get_deleter() = std::move(rhs.get_deleter());
		return *this;
	}

	/**
	 * @brief Transfers ownership to a unique_ptr if possible. If *this is not
	 * owning, returns nullptr.
	 *
	 * @post If ownership was transferred, *this is null. Otherwise, does
	 * nothing.
	 *
	 * @return std::unique_ptr<T, Deleter> Either a pointer which owns what *this
	 * owned, or a null pointer.
	 */
	unique to_unique() && noexcept {
		if (owns_) {
			return {release(), get_deleter()};
		} else {
			return nullptr;
		}
	}

	explicit operator unique() && noexcept {
		return std::move(*this).to_unique();
	}

	~cond_ptr() noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
	}

	KBLIB_NODISCARD cond_ptr weak() const& noexcept {
		return cond_ptr{ptr_, false};
	}

	bool owns() const noexcept { return owns_; }
	KBLIB_NODISCARD T* release() & noexcept {
		owns_ = false;
		return std::exchange(ptr_, nullptr);
	}

	Deleter& get_deleter() noexcept { return *this; }

	const Deleter& get_deleter() const noexcept { return *this; }

	void reset(T* p = nullptr, bool owner = false,
	           std::decay_t<Deleter> del = {}) &
	    noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = owner;
		get_deleter() = std::move(del);
	}
	void reset(T* p, std::decay_t<Deleter> del = {}) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = false;
		get_deleter() = std::move(del);
	}

	void swap(cond_ptr& other) {
		std::swap(ptr_, other.ptr_);
		std::swap(owns_, other.owns_);
		std::swap(get_deleter(), other.get_deleter());
	}

	KBLIB_NODISCARD T* get() & noexcept { return ptr_; }

	KBLIB_NODISCARD const T* get() const& noexcept { return ptr_; }

	explicit operator bool() const noexcept { return ptr_; }

	KBLIB_NODISCARD T& operator[](std::size_t index) & noexcept {
		return ptr_[index];
	}

	KBLIB_NODISCARD const T& operator[](std::size_t index) const& noexcept {
		return ptr_[index];
	}

	friend constexpr bool operator==(const cond_ptr& lhs, const cond_ptr& rhs) {
		return lhs.ptr_ == rhs.ptr_;
	}

	friend constexpr bool operator==(const unique& lhs, const cond_ptr& rhs) {
		return lhs.get() == rhs.ptr_;
	}

	friend constexpr bool operator==(const cond_ptr& lhs, const unique& rhs) {
		return lhs.ptr_ == rhs.get();
	}

 private:
	T* ptr_ = nullptr;
	bool owns_ = false;
};

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

template <typename T>
class heap_value {
 public:
	using pointer = T*;
	using element_type = T;
	using reference = T&;

	constexpr heap_value() noexcept : p{nullptr} {}
	constexpr heap_value(std::nullptr_t) noexcept : p{nullptr} {}

	template <typename... Args,
	          enable_if_t<std::is_constructible<T, Args...>::value> = 0>
	heap_value(fakestd::in_place_t, Args&&... args) : p{new T(args...)} {}

	heap_value(const heap_value& u) : p{(u.p ? (new T(*u.p)) : nullptr)} {}
	heap_value(heap_value&& u) : p{std::exchange(u.p, nullptr)} {}

	heap_value& operator=(const heap_value& u) & {
		if (!u) {
			reset();
		} else {
			if (p) {
				*p = *u;
			} else {
				p = new T(*u.p);
			}
		}
		return *this;
	}

	heap_value& operator=(heap_value&& u) & {
		reset();
		p = std::exchange(u.p, nullptr);
		return *this;
	}

	void reset() & {
		delete p;
		p = nullptr;
		return;
	}

	KBLIB_NODISCARD pointer get() const& noexcept { return p; }

	KBLIB_NODISCARD explicit operator bool() const& noexcept {
		return p != nullptr;
	}

	reference operator*() const& noexcept { return *p; }

	~heap_value() { delete p; }

 private:
	pointer p;
};

} // namespace kblib

#endif // KBLIB_FAKESTD_H
