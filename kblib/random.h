#ifndef RANDOM_H
#define RANDOM_H

#include "algorithm.h"
#include "iterators.h"
#include "memory.h"
#include "simple.h"
#include "stats.h"
#include "tdecl.h"

#include <limits>
#include <random>
#include <vector>

#include <iostream>

namespace kblib {

class KBLIB_NODISCARD trivial_seed_seq {
 public:
	using result_type = std::uint32_t;

	trivial_seed_seq() = default;
	template <typename InputIt>
	trivial_seed_seq(InputIt begin, InputIt end) : data(begin, end) {}
	template <typename T>
	trivial_seed_seq(std::initializer_list<T> il)
	    : trivial_seed_seq(il.begin(), il.end()) {}
	template <typename Generator>
	trivial_seed_seq(Generator gen, std::size_t count) : data(count) {
		kblib::generate_n(data.begin(), count, std::ref(gen));
	}

	template <typename RandomAccessIt>
	auto generate(RandomAccessIt begin, RandomAccessIt end) const -> void {
		auto o_size = end - begin;
		auto d_size = to_signed(data.size());
		if (data.empty()) {
			std::fill(begin, end, 0x8b8b8b8bu); // copied from std::seed_seq
		} else {
			if (o_size > d_size) {
				std::clog << "trivial_seed_seq: unexpectedly wrapping, output size "
				          << o_size << " greater than data size " << d_size << '\n';
			}
			auto dpos = begin;
			do {
				auto blk_size =
				    saturating_cast<std::size_t>(std::min(d_size, o_size));
				dpos = std::copy_n(data.begin(), blk_size, dpos);
			} while ((o_size -= d_size) > 0);
		}
		return;
	}

	KBLIB_NODISCARD auto size() const noexcept -> std::size_t {
		return data.size();
	}

	template <typename OutputIt>
	auto param(OutputIt dest) const -> void {
		std::copy(data.begin(), data.end(), dest);
		return;
	}

 private:
	std::vector<std::uint32_t> data;
};

template <typename T>
struct state_size;

template <typename UIntType, size_t w, size_t n, size_t m, size_t r, UIntType a,
          size_t u, UIntType d, size_t s, UIntType b, size_t t, UIntType c,
          size_t l, UIntType f>
struct state_size<std::mersenne_twister_engine<UIntType, w, n, m, r, a, u, d, s,
                                               b, t, c, l, f>>
    : std::integral_constant<std::size_t, n*((w + 31) / 32)> {};

template <typename UIntType, UIntType a, UIntType c, UIntType m>
struct state_size<std::linear_congruential_engine<UIntType, a, c, m>>
    : std::integral_constant<std::size_t, (detail::filg2(m) + 31) / 32> {};

template <typename UIntType, std::size_t w, std::size_t s, std::size_t r>
struct state_size<std::subtract_with_carry_engine<UIntType, w, s, r>>
    : std::integral_constant<std::size_t, r*((w + 31) / 32)> {};

template <typename Engine, std::size_t P, std::size_t R>
struct state_size<std::discard_block_engine<Engine, P, R>>
    : state_size<Engine> {};

template <typename Engine, std::size_t W, typename UIntType>
struct state_size<std::independent_bits_engine<Engine, W, UIntType>>
    : state_size<Engine> {};

template <typename Engine, std::size_t K>
struct state_size<std::shuffle_order_engine<Engine, K>> : state_size<Engine> {};

template <typename T>
constexpr std::size_t state_size_v = state_size<T>::value;

template <typename Gen, typename Source>
KBLIB_NODISCARD auto seeded(Source&& s) -> Gen {
	auto seed = trivial_seed_seq(std::ref(s), state_size_v<Gen>);
	return Gen{seed};
}

template <typename Gen>
KBLIB_NODISCARD auto seeded() -> Gen {
	auto seed = trivial_seed_seq(std::random_device{}, state_size_v<Gen>);
	return Gen{seed};
}

template <typename URBG, typename Transform>
class KBLIB_NODISCARD transform_engine : URBG {
 private:
	using E = URBG;
	static_assert(std::is_default_constructible<Transform>::value, "");

	KBLIB_NODISCARD auto engine() -> E& { return static_cast<E&>(*this); }
	KBLIB_NODISCARD auto engine() const -> const E& {
		return static_cast<const E&>(*this);
	}

 public:
	using result_type = typename Transform::result_type;

	transform_engine() = default;
	transform_engine(const transform_engine&) noexcept(
	    std::is_nothrow_copy_constructible<URBG>::value) = default;
	transform_engine(transform_engine&&) noexcept(
	    std::is_nothrow_move_constructible<URBG>::value) = default;
	transform_engine(result_type s) : E(s) {}
	template <typename SSeq, typename = enable_if_t<
	                             !std::is_same<SSeq, transform_engine>::value>>
	transform_engine(SSeq& s) : E(s) {}

	KBLIB_NODISCARD auto operator=(const transform_engine&)
	    -> transform_engine& = delete;
	KBLIB_NODISCARD auto operator=(transform_engine &&)
	    -> transform_engine& = delete;

	~transform_engine() = default;

	KBLIB_NODISCARD constexpr auto operator()() noexcept -> result_type {
		return Transform{}(engine()());
	}

	using E::seed;

	using E::discard;

	KBLIB_NODISCARD static constexpr auto min() noexcept -> result_type {
		return Transform::min(URBG::min(), URBG::max());
	}
	KBLIB_NODISCARD static constexpr auto max() noexcept -> result_type {
		return Transform::max(URBG::min(), URBG::max());
	}

	KBLIB_NODISCARD friend auto operator==(const transform_engine& lhs,
	                                       const transform_engine& rhs) noexcept
	    -> bool {
		return lhs.engine() == rhs.engine();
	}
	KBLIB_NODISCARD friend auto operator!=(const transform_engine& lhs,
	                                       const transform_engine& rhs) noexcept
	    -> bool {
		return !(lhs == rhs);
	}

	friend auto operator<<(std::ostream& os, const transform_engine& e)
	    -> std::ostream& {
		return os << e.engine();
	}
	friend auto operator>>(std::istream& is, transform_engine& e)
	    -> std::istream& {
		return is >> e.engine();
	}
};

template <typename Engine, typename Transform>
struct state_size<transform_engine<Engine, Transform>> : state_size<Engine> {};

template <typename UIntType, UIntType shift, UIntType mask = max>
struct shift_mask {
	using result_type = UIntType;

	template <typename UIntInput>
	KBLIB_NODISCARD static constexpr auto g(UIntInput in) noexcept -> UIntType {
		return static_cast<UIntType>(in >> shift) & mask;
	}
	KBLIB_NODISCARD constexpr auto operator()(UIntType in) const noexcept
	    -> UIntType {
		return g(in);
	}
	KBLIB_NODISCARD static constexpr auto
	min(UIntType min, [[maybe_unused]] UIntType max) noexcept -> UIntType {
		return g(min);
	}
	KBLIB_NODISCARD static constexpr auto max([[maybe_unused]] UIntType min,
	                                          UIntType max) noexcept
	    -> UIntType {
		return g(max);
	}
};

template <typename UIntType>
KBLIB_NODISCARD constexpr auto ipow2(UIntType b) noexcept -> UIntType {
	if (b == std::numeric_limits<UIntType>::digits) {
		return 0u;
	} else {
		return UIntType{1} << b;
	}
}

inline namespace lcgs {
	template <typename UIntType, UIntType a, UIntType c, UIntType b>
	using lcg_p2 = std::linear_congruential_engine<UIntType, a, c, ipow2(b)>;

	inline namespace common_lcgs {
		using rand48 =
		    transform_engine<lcg_p2<std::uint_fast64_t, 25214903917u, 11u, 48u>,
		                     shift_mask<std::uint_fast32_t, 16u>>;

	} // namespace common_lcgs

	inline namespace best_lcgs {

		using lcg32 = lcg_p2<std::uint_fast32_t, 0xa13fc965u, 1u, 32u>;
		using mcg32 = lcg_p2<std::uint_fast32_t, 0x93d765ddu, 0u, 32u>;

		using lcg48 = lcg_p2<std::uint_fast64_t, 0xb67a49a5466du, 1u, 48u>;
		using mcg48 = lcg_p2<std::uint_fast64_t, 0xbdcdbb079f8du, 0u, 48u>;

		using lcg64 = lcg_p2<std::uint_fast64_t, 0xaf251af3b0f025b5u, 1u, 64u>;
		using mcg64 = lcg_p2<std::uint_fast64_t, 0xf1357aea2e62a9c5u, 0u, 64u>;

	} // namespace best_lcgs

} // namespace lcgs

} // namespace kblib

#endif // RANDOM_H
