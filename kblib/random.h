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

class trivial_seed_seq {
 public:
	using result_type = std::uint32_t;

	trivial_seed_seq() = default;
	template <typename InputIt>
	trivial_seed_seq(InputIt begin, InputIt end) : data(begin, end) {
		assert(data.size() * sizeof(std::uint32_t) <=
		       static_cast<std::make_signed<std::size_t>::type>(max));
	}
	template <typename T>
	trivial_seed_seq(std::initializer_list<T> il)
	    : trivial_seed_seq(il.begin(), il.end()) {}
	template <typename Generator>
	trivial_seed_seq(Generator gen, std::size_t count) : data(count) {
		kblib::generate_n(data.begin(), count, std::ref(gen));
	}

	template <typename RandomAccessIt>
	void generate(RandomAccessIt begin, RandomAccessIt end) const {
		if (end - begin > kblib::to_signed(data.size())) {
			std::clog << "trivial_seed_seq: unexpectedly wrapping, output size "
			          << end - begin << " greater than data size " << data.size()
			          << '\n';
		}
		if (data.empty()) {
			for (auto& i : indirect(begin, end)) {
				i = 0x8b8b8b8b; // copied from std::seed_seq
			}
		} else {
			std::size_t index{};
			bool wrapped = false;
			for (auto& i : indirect(begin, end)) {
				if (index == data.size()) {
					wrapped = true;
					index = 0;
				}
				i = data[index++];
			}
			if (wrapped) {
				std::clog << "final index: " << index << '\n';
			}
		}
		return;
	}

	template <typename T>
	enable_if_t<std::is_integral<T>::value, void> generate(T* begin,
	                                                       T* end) const {
		auto r_begin = reinterpret_cast<char*>(begin);
		auto r_end = reinterpret_cast<char*>(end);
		auto r_size = r_end - r_begin;
		auto d_size = kblib::to_signed(data.size() * sizeof(std::uint32_t));
		if (data.empty()) {
			std::memset(r_begin, 0x8b, saturating_cast<std::size_t>(r_size));
		} else {
			auto dr_begin = reinterpret_cast<const char*>(data.data());
			if (r_size > d_size) {
				std::clog << "trivial_seed_seq: unexpectedly wrapping, output size "
				          << r_size << " greater than data size " << d_size << '\n';
			}
			auto dpos = r_begin;
			do {
				auto blk_size =
				    saturating_cast<std::size_t>(std::min(d_size, r_size));
				dpos = std::copy_n(dr_begin, blk_size, dpos);
			} while ((r_size -= d_size) > 0);
			if (r_size != 0) {
				std::clog << "trivial_seed_seq: odd output size, last block size = "
				          << r_size + d_size << '\n';
			}
		}
		return;
	}

	std::size_t size() const noexcept { return data.size(); }

	template <typename OutputIt>
	void param(OutputIt dest) const {
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
Gen seeded(Source&& s) {
	auto seed = trivial_seed_seq(std::ref(s), state_size_v<Gen>);
	return Gen{seed};
}

template <typename Gen>
Gen seeded() {
	auto seed = trivial_seed_seq(std::random_device{}, state_size_v<Gen>);
	return Gen{seed};
}

template <typename URBG, typename Transform>
class transform_engine : URBG {
 private:
	using E = URBG;
	static_assert(std::is_default_constructible<Transform>::value, "");

	E& engine() { return static_cast<E&>(*this); }
	const E& engine() const { return static_cast<const E&>(*this); }

 public:
	using result_type = typename E::result_type;

	transform_engine() = default;
	transform_engine(const transform_engine&) = default;
	transform_engine(result_type s) : E(s) {}
	template <typename SSeq, typename = enable_if_t<
	                             !std::is_same<SSeq, transform_engine>::value>>
	transform_engine(SSeq& s) : E(s) {}

	transform_engine& operator=(const transform_engine&) = delete;

	~transform_engine() = default;

	constexpr result_type operator()() noexcept {
		return Transform{}(engine()());
	}

	using E::seed;

	using E::discard;

	static constexpr result_type min() noexcept {
		return Transform::min(URBG::min());
	}
	static constexpr result_type max() noexcept {
		return Transform::max(URBG::max());
	}

	friend bool operator==(const transform_engine& lhs,
	                       const transform_engine& rhs) noexcept {
		return lhs.engine() == rhs.engine();
	}
	friend bool operator!=(const transform_engine& lhs,
	                       const transform_engine& rhs) noexcept {
		return !(lhs == rhs);
	}

	friend std::ostream& operator<<(std::ostream& os,
	                                const transform_engine& e) {
		return os << e.engine();
	}
	friend std::istream& operator>>(std::istream& is, transform_engine& e) {
		return is >> e.engine();
	}
};
template <typename Engine, typename Transform>
struct state_size<transform_engine<Engine, Transform>> : state_size<Engine> {};

template <typename UIntType, UIntType shift, UIntType mask = max>
struct shift_mask {
	static constexpr auto g(UIntType in) noexcept -> UIntType {
		return (in >> shift) & mask;
	}
	KBLIB_NODISCARD constexpr auto operator()(UIntType in) const noexcept
	    -> UIntType {
		return g(in);
	}
	static constexpr auto min(UIntType in) noexcept -> UIntType { return g(in); }
	static constexpr auto max(UIntType in) noexcept -> UIntType { return g(in); }
};

} // namespace kblib

#endif // RANDOM_H
