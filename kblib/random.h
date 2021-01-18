#ifndef RANDOM_H
#define RANDOM_H

#include "iterators.h"
#include "simple.h"
#include "tdecl.h"

#include <limits>
#include <random>
#include <vector>

namespace kblib {

class trivial_seed_seq {
 public:
	trivial_seed_seq() = default;
	template <typename InputIt>
	trivial_seed_seq(InputIt begin, InputIt end) : data(begin, end) {}
	template <typename T>
	trivial_seed_seq(std::initializer_list<T> il)
	    : trivial_seed_seq(il.begin(), il.end()) {}
	template <typename Generator>
	trivial_seed_seq(Generator gen, std::size_t count) : data(count) {
		std::generate_n(data.begin(), count, gen);
	}

	template <typename RandomAccessIt>
	void generate(RandomAccessIt begin, RandomAccessIt end) const {
		if (data.empty()) {
			for (auto& i : indirect(begin, end)) {
				i = 0x8b8b8b8b; // copied from std::seed_seq
			}
		} else {
			std::size_t index{};
			for (auto& i : indirect(begin, end)) {
				i = data[index++ % data.size()];
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
    : std::integral_constant<std::size_t, (w + 31) / 32> {};

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
Gen seed_with(Source&& s) {
	auto seed = trivial_seed_seq(std::ref(s), state_size_v<Gen>);
	return Gen{seed};
}

} // namespace kblib

#endif // RANDOM_H
