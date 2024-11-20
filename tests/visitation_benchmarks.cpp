/* *****************************************************************************
 * %{QMAKE_PROJECT_NAME}
 * Copyright (c) %YEAR% killerbee
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
#include "kblib/poly_obj.h"

#define CATCH_CONFIG_ENABLE_BENCHMARKING
#include "catch/catch.hpp"

//#define FAST_TEST

#if KBLIB_USE_CXX17

#	ifndef FAST_TEST

namespace {
struct Base {
	virtual auto operator()() const noexcept -> unsigned = 0;
	Base() = default;
	Base(const Base&) = default;
	Base(Base&&) noexcept = default;
	virtual ~Base() = default;

	// this is used, but the compiler doesn't think so because it's in a .cpp
	// file
	[[maybe_unused]] constexpr static inline std::size_t max_derived_size
	    = sizeof(unsigned);
};

struct thrower final : Base {
	auto operator()() const noexcept -> unsigned override { return member; }

	unsigned member;

	thrower(unsigned i = 0)
	    : member(i) {}
	thrower(const thrower&) = default;

	auto operator=(const thrower& other) & -> thrower& {
		check_throw(other.member);
		member = other.member;
		return *this;
	}

 private:
	static auto check_throw(unsigned v) -> void {
		if ((v & 7u) == 0) {
			throw 0;
		}
	}
};

struct fptr {
	unsigned member;
	auto (*p)(const fptr*) noexcept -> unsigned;
	auto operator()() const noexcept -> unsigned { return p(this); }
};

#		define MAKE_DERIVED2(Number, Expr)                           \
			auto fptr_d##Number(const fptr* p) noexcept->unsigned {    \
				const auto x = p->member;                               \
				return Expr;                                            \
			}                                                          \
			struct Derived##Number final : Base {                      \
				auto operator()() const noexcept -> unsigned override { \
					return Expr;                                         \
				}                                                       \
				unsigned x;                                             \
				Derived##Number(unsigned i)                             \
				    : x(i) {}                                           \
			}
#		define MAKE_DERIVED(Number, Expr) MAKE_DERIVED2(Number, Expr)

constexpr auto big_number = kblib::fnv::fnv_prime<unsigned>::value;

MAKE_DERIVED(__COUNTER__, x);
MAKE_DERIVED(__COUNTER__, x * 2);
MAKE_DERIVED(__COUNTER__, x / 2);
MAKE_DERIVED(__COUNTER__, ~x);
MAKE_DERIVED(__COUNTER__, x + 42);
MAKE_DERIVED(__COUNTER__, x * 2 + 42);
MAKE_DERIVED(__COUNTER__, -x);
MAKE_DERIVED(__COUNTER__, x - 42);
MAKE_DERIVED(__COUNTER__, 42 - x);
MAKE_DERIVED(__COUNTER__, (x * x));
MAKE_DERIVED(__COUNTER__, (x * x * 2));
MAKE_DERIVED(__COUNTER__, -x - 42);
MAKE_DERIVED(__COUNTER__, x & 42);
MAKE_DERIVED(__COUNTER__, x | 42);
MAKE_DERIVED(__COUNTER__, x << 2);
MAKE_DERIVED(__COUNTER__, x >> 2);
MAKE_DERIVED(__COUNTER__, ~x << 2);
MAKE_DERIVED(__COUNTER__, ~x >> 2);
MAKE_DERIVED(__COUNTER__, (x >> 8) | (x << 24));
MAKE_DERIVED(__COUNTER__, (x >> 16) | (x << 16));
MAKE_DERIVED(__COUNTER__, (x >> 24) | (x << 8));
MAKE_DERIVED(__COUNTER__, (~x >> 8) | (~x << 24));
MAKE_DERIVED(__COUNTER__, (~x >> 16) | (~x << 16));
MAKE_DERIVED(__COUNTER__, (~x >> 24) | (~x << 8));
MAKE_DERIVED(__COUNTER__, (static_cast<void>(x), 42));
MAKE_DERIVED(__COUNTER__, (x >> 16) | (~x << 16));
MAKE_DERIVED(__COUNTER__, (~x >> 16) | (x << 16));
MAKE_DERIVED(__COUNTER__, x ^ 42);
MAKE_DERIVED(__COUNTER__, x ^ ((x >> 16) | (x << 16)));
MAKE_DERIVED(__COUNTER__, -~x);
MAKE_DERIVED(__COUNTER__, ~-x);
MAKE_DERIVED(__COUNTER__, x * 42);
MAKE_DERIVED(__COUNTER__, -x * 42);
MAKE_DERIVED(__COUNTER__, -x / 42);
MAKE_DERIVED(__COUNTER__, x % 42);
MAKE_DERIVED(__COUNTER__, -(x % 42));
MAKE_DERIVED(__COUNTER__, (x * big_number));
MAKE_DERIVED(__COUNTER__, x ^ big_number);
MAKE_DERIVED(__COUNTER__, x + big_number);
MAKE_DERIVED(__COUNTER__, x - big_number);
MAKE_DERIVED(__COUNTER__, big_number - x);
MAKE_DERIVED(__COUNTER__, x / big_number);
MAKE_DERIVED(__COUNTER__, x % big_number);
MAKE_DERIVED(__COUNTER__, big_number / x);
MAKE_DERIVED(__COUNTER__, big_number % x);
MAKE_DERIVED(__COUNTER__, (void(x), big_number));
MAKE_DERIVED(__COUNTER__, x + (big_number >> 16));
MAKE_DERIVED(__COUNTER__, x + (big_number >> 24));
MAKE_DERIVED(__COUNTER__, (x << 16) + big_number);
MAKE_DERIVED(__COUNTER__, (x << 16) - big_number);
MAKE_DERIVED(__COUNTER__, big_number - (x << 16));
MAKE_DERIVED(__COUNTER__, (x << 16) ^ big_number);
MAKE_DERIVED(__COUNTER__, (x << 16) / big_number);
MAKE_DERIVED(__COUNTER__, big_number / (x << 16));
MAKE_DERIVED(__COUNTER__, (x << 16) & big_number);
MAKE_DERIVED(__COUNTER__, (x << 16) + (big_number & 0x0000FFFF));
MAKE_DERIVED(__COUNTER__, (x << 16) % big_number);
MAKE_DERIVED(__COUNTER__, big_number % (x << 16));
MAKE_DERIVED(__COUNTER__, x * 42 + big_number);
MAKE_DERIVED(__COUNTER__, (x * (big_number % 42)));
MAKE_DERIVED(__COUNTER__, (x * (-big_number % 42)));
MAKE_DERIVED(__COUNTER__, (x * 14 + (big_number >> 16)));
MAKE_DERIVED(__COUNTER__, (x * 14 - (big_number >> 16)));
MAKE_DERIVED(__COUNTER__, (x * 14 ^ (big_number >> 16)));

using variant_type = std::variant<
    Derived0, Derived1, Derived2, Derived3, Derived4, Derived5, Derived6,
    Derived7, Derived8, Derived9, Derived10, Derived11, Derived12, Derived13,
    Derived14, Derived15, Derived16, Derived17, Derived18, Derived19, Derived20,
    Derived21, Derived22, Derived23, Derived24, Derived25, Derived26, Derived27,
    Derived28, Derived29, Derived30, Derived31, Derived32, Derived33, Derived34,
    Derived35, Derived36, Derived37, Derived38, Derived39, Derived40, Derived41,
    Derived42, Derived43, Derived44, Derived45, Derived46, Derived47, Derived48,
    Derived49, Derived50, Derived51, Derived52, Derived53, Derived54, Derived55,
    Derived56, Derived57, Derived58, Derived59, Derived60, Derived61, Derived62,
    Derived63>;
using variant_type_throws = std::variant<
    Derived0, Derived1, Derived2, Derived3, Derived4, Derived5, Derived6,
    Derived7, Derived8, Derived9, Derived10, Derived11, Derived12, Derived13,
    Derived14, Derived15, Derived16, Derived17, Derived18, Derived19, Derived20,
    Derived21, Derived22, Derived23, Derived24, Derived25, Derived26, Derived27,
    Derived28, Derived29, Derived30, Derived31, Derived32, Derived33, Derived34,
    Derived35, Derived36, Derived37, Derived38, Derived39, Derived40, Derived41,
    Derived42, Derived43, Derived44, Derived45, Derived46, Derived47, Derived48,
    Derived49, Derived50, Derived51, Derived52, Derived53, Derived54, Derived55,
    Derived56, Derived57, Derived58, Derived59, Derived60, Derived61, Derived62,
    Derived63, thrower>;
constexpr std::array<decltype(&fptr_d0), 64> fptrs{
    fptr_d0,  fptr_d1,  fptr_d2,  fptr_d3,  fptr_d4,  fptr_d5,  fptr_d6,
    fptr_d7,  fptr_d8,  fptr_d9,  fptr_d10, fptr_d11, fptr_d12, fptr_d13,
    fptr_d14, fptr_d15, fptr_d16, fptr_d17, fptr_d18, fptr_d19, fptr_d20,
    fptr_d21, fptr_d22, fptr_d23, fptr_d24, fptr_d25, fptr_d26, fptr_d27,
    fptr_d28, fptr_d29, fptr_d30, fptr_d31, fptr_d32, fptr_d33, fptr_d34,
    fptr_d35, fptr_d36, fptr_d37, fptr_d38, fptr_d39, fptr_d40, fptr_d41,
    fptr_d42, fptr_d43, fptr_d44, fptr_d45, fptr_d46, fptr_d47, fptr_d48,
    fptr_d49, fptr_d50, fptr_d51, fptr_d52, fptr_d53, fptr_d54, fptr_d55,
    fptr_d56, fptr_d57, fptr_d58, fptr_d59, fptr_d60, fptr_d61, fptr_d62,
    fptr_d63,
};

template <unsigned N = 4u>
auto make_fptr(unsigned v) noexcept -> fptr {
	switch (v % N) {
	case 0:
		return {v, &fptr_d1};
	case 1:
		return {v, &fptr_d2};
	case 2:
		return {v, &fptr_d3};
	case 3:
		return {v, &fptr_d4};
	default:
		__builtin_unreachable();
	}
	return {v, fptrs[v % N]};
}

template <std::size_t Num, std::size_t... Is, typename T, typename F>
auto do_push_elem(std::index_sequence<Is...>, T& d, unsigned v, F f) {
	const auto i = v % Num;
	((i == Is
	  and (d.emplace_back(f(kblib::constant<std::size_t, Is>{}, v)), true))
	 or ...);
	return;
}

template <std::size_t Num, typename T, typename F>
auto push_elem(T& d, unsigned v, F f) {
	return do_push_elem<Num>(std::make_index_sequence<Num>{}, d, v, f);
};

template <typename... Ts>
auto baseline_generic(std::vector<Ts>... args) {
	return (
	    std::accumulate(args.begin(), args.end(), 0u,
	                    [](unsigned accum, const Ts& x) { return accum + x(); })
	    + ... + 0);
}

} // namespace

TEST_CASE("poly_obj performance(4_old)") {
	const auto start = std::chrono::steady_clock::now();
#		ifdef NDEBUG
	constexpr unsigned count = 1000;
#		else
	constexpr unsigned count = 100;
#		endif

#		ifdef SANITIZERS
#			define STR(s) #         s
	std::cout << "Sanitizers active\n";
#		endif

	std::vector<std::pair<unsigned, std::string_view>> reproducibility_test;
	using poly_t = kblib::poly_obj<
	    Base, sizeof(Derived1),
	    kblib::poly_obj_traits<Base, kblib::construct_type::both>>;

	auto push_checksum = [&](unsigned s, std::string_view name) {
		auto begin = reproducibility_test.begin();
		auto end = reproducibility_test.end();
		// Take only the first result for each run type
		if (std::find_if(begin, end,
		                 [&](const auto& p) { return p.second == name; })
		    == end) {
			reproducibility_test.emplace_back(s, name);
		}
	};

	/*
	 * Release / Debug, all times in microseconds:
	 *
	 *   Four separate contiguous arrays
	 * baseline:                    5.377 /  37.549
	 *   Polymorphism, always valid
	 * raw pointer:                10.470 /  51.205
	 * unique_ptr:                 11.826 /  70.346
	 * poly_obj:                   14.644 /  79.075
	 *   Type erasure, always valid
	 * std::function:              15.247 / 106.562
	 * std::visit(v, f):           27.375 / 238.055
	 * kblib::visit(v, f...):      37.308 / 258.965
	 * kblib::visit(v)(f...):      37.253 / 288.355
	 * visit_indexed:              30.203 / 208.894
	 * kblib::visit2:               8.407 / 211.649
	 * kblib::visit2_nop:           8.650 / 222.502
	 * std::get_if:                 8.580 / 158.164
	 * switch (v.index):            8.984 / 170.739
	 *   Polymorphism, checking for invalid
	 * raw pointer, ch:            15.859 /  56.917
	 * unique_ptr, ch:             18.344 / 108.046
	 * poly_obj, ch:               18.290 /  96.168
	 *   Type erasure, checking for invalid
	 * std::function, ch:          25.200 / 122.038
	 * kblib::visit2_nop, ch:      12.327 / 322.764
	 * std::get_if, ch:            12.386 / 232.006
	 * switch(v.index()), ch:      16.817 / 203.752
	 *   Type erasure, exceptions for invalid
	 * std::function, ex:          23.758 / 126.152
	 * std::visit(v, f), ex:       40.646 / 310.900
	 * kblib::visit(v, f...), ex:  44.016 / 328.484
	 * kblib::visit(v)(f...), ex:  46.345 / 377.681
	 * visit_indexed, ex:          40.066 / 261.261
	 * kblib::visit2, ex:          12.353 / 315.509
	 *
	 * Overall test runtime:  80s / 286s
	 *
	 * Conclusions:
	 *
	 * In release builds, visit_indexed, std::function, and std::visit are very
	 * slow, contiguous access is very fast, everything else is comparable to
	 * each other, except that exceptions are, of course, very slow.
	 *
	 * In debug builds, type erasure seems to be extremely slow, enough even to
	 * mostly drown out the exceptions, but the fastest is std::function by far.
	 * visit2 is a bit faster in release builds, but poly_obj isn't much slower
	 * and is much much faster than it in debug. In every instance, poly_obj
	 * compares favorably to unique_ptr, which is good, because it is meant to
	 * replace it in some uses.
	 *
	 * std::get_if and switch are predictably the fastest way of accessing a
	 * variant in debug, but in release are tied with visit2. I do not think
	 * that the messy and verbose code is justified by the debug performance
	 * gain compared to the other options.
	 *
	 * If you want to use exceptions for invalid objects, kblib::visit2 is the
	 * fastest way, followed by std::function. The others are equally twice as
	 * slow.
	 *
	 * I cannot tell why visit_indexed is so much slower than visit2 in this
	 * test. Their code is very similar. It must be the additional parameter
	 * causing delays in setting up the call.
	 *
	 */

	{
		std::vector<Derived0> d1;
		std::vector<Derived1> d2;
		std::vector<Derived2> d3;
		std::vector<Derived3> d4;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d1.emplace_back(v);
				break;
			case 1:
				d2.emplace_back(v);
				break;
			case 2:
				d3.emplace_back(v);
				break;
			case 3:
				d4.emplace_back(v);
			}
		}

		BENCHMARK_ADVANCED("baseline")(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d1) {
					accum += x();
				}
				for (const auto& x : d2) {
					accum += x();
				}
				for (const auto& x : d3) {
					accum += x();
				}
				for (const auto& x : d4) {
					accum += x();
				}
				return accum;
			});
			push_checksum(accum, "baseline");
		};
		BENCHMARK_ADVANCED("baseline_generic")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] { accum = baseline_generic(d1, d2, d3, d4); });
			push_checksum(accum, "baseline_generic");
		};
	}
	BENCHMARK_ADVANCED("raw pointer")(Catch::Benchmark::Chronometer meter) {
		std::vector<Base*> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.push_back(new Derived0(v));
				break;
			case 1:
				d.push_back(new Derived1(v));
				break;
			case 2:
				d.push_back(new Derived2(v));
				break;
			case 3:
				d.push_back(new Derived3(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (auto x : d) {
				accum += (*x)();
			}
			return accum;
		});
		push_checksum(accum, "raw pointer");
		for (auto x : d) {
			delete x;
		}
	};
	BENCHMARK_ADVANCED("unique_ptr")
	(Catch::Benchmark::Chronometer meter) {
		std::vector<std::unique_ptr<Base>> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d.push_back(std::make_unique<Derived0>(v));
				break;
			case 1:
				d.push_back(std::make_unique<Derived1>(v));
				break;
			case 2:
				d.push_back(std::make_unique<Derived2>(v));
				break;
			case 3:
				d.push_back(std::make_unique<Derived3>(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += (*x)();
			}
			return accum;
		});
		push_checksum(accum, "unique pointer");
	};
	BENCHMARK_ADVANCED("poly_obj")(Catch::Benchmark::Chronometer meter) {
		std::vector<poly_t> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d.push_back(poly_t::make<Derived0>(v));
				break;
			case 1:
				d.push_back(poly_t::make<Derived1>(v));
				break;
			case 2:
				d.push_back(poly_t::make<Derived2>(v));
				break;
			case 3:
				d.push_back(poly_t::make<Derived3>(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += x();
			}
			return accum;
		});
		push_checksum(accum, "poly_obj");
	};
	BENCHMARK_ADVANCED("function pointer")(Catch::Benchmark::Chronometer meter) {
		std::vector<fptr> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			d.push_back(make_fptr(v));
		}

		unsigned accum{};
		meter.measure([&] {
			for (auto x : d) {
				accum += x.p(&x);
			}
			return accum;
		});
		push_checksum(accum, "raw pointer");
	};
	BENCHMARK_ADVANCED("function pointer (wrapped)")
	(Catch::Benchmark::Chronometer meter) {
		std::vector<fptr> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			d.push_back(make_fptr(v));
		}

		unsigned accum{};
		meter.measure([&] {
			for (auto x : d) {
				accum += x();
			}
			return accum;
		});
		push_checksum(accum, "raw pointer");
	};
	BENCHMARK_ADVANCED("std::function")(Catch::Benchmark::Chronometer meter) {
		std::vector<std::function<unsigned()>> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.emplace_back(Derived0(v));
				break;
			case 1:
				d.emplace_back(Derived1(v));
				break;
			case 2:
				d.emplace_back(Derived2(v));
				break;
			case 3:
				d.emplace_back(Derived3(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += x();
			}
			return accum;
		});
		push_checksum(accum, "std::function");
	};
	{
		std::vector<std::variant<Derived0, Derived1, Derived2, Derived3>> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.emplace_back(Derived0(v));
				break;
			case 1:
				d.emplace_back(Derived1(v));
				break;
			case 2:
				d.emplace_back(Derived2(v));
				break;
			case 3:
				d.emplace_back(Derived3(v));
				break;
			}
		}
		BENCHMARK_ADVANCED("std::visit(v, f)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += std::visit([](const auto& v) { return v(); }, x);
				}
				return accum;
			});
			push_checksum(accum, "std::visit(v, f)");
		};
		BENCHMARK_ADVANCED("kblib::visit(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit(x, [](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit(v)(f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit(x)([](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit(v)(f...)");
		};
		BENCHMARK_ADVANCED("visit_indexed(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit_indexed(
					    x, [](auto, const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "visit_indexed(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit2(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit2(x, [](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit2(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit2_nop(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					kblib::visit2_nop(x, [&](const auto& v) { accum += v(); });
				}
				return accum;
			});
			push_checksum(accum, "kblib::visit2_nop(v, f...)");
		};
		BENCHMARK_ADVANCED("std::get_if")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (auto* const p = std::get_if<0>(&x)) {
						accum += (*p)();
					} else if (auto* const p = std::get_if<1>(&x)) {
						accum += (*p)();
					} else if (auto* const p = std::get_if<2>(&x)) {
						accum += (*p)();
					} else if (auto* const p = std::get_if<3>(&x)) {
						accum += (*p)();
					}
				}
				return accum;
			});
			push_checksum(accum, "std::get_if");
		};
		BENCHMARK_ADVANCED("switch (v.index())")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					switch (x.index()) {
					case 0:
						accum += std::get<0>(x)();
						break;
					case 1:
						accum += std::get<1>(x)();
						break;
					case 2:
						accum += std::get<2>(x)();
						break;
					case 3:
						accum += std::get<3>(x)();
						break;
					default:;
					}
				}
				return accum;
			});
			push_checksum(accum, "switch (v.index())");
		};
	}

	// Test speed when some objects are invalid
	{
		BENCHMARK_ADVANCED("raw pointer, ch")
		(Catch::Benchmark::Chronometer meter) {
			std::vector<Base*> d;
			kblib::FNV32_hash<unsigned> h;
			for (auto i : kblib::range(count)) {
				auto v = static_cast<unsigned>(h(i));
				switch (v % 5) {
				case 0:
					d.emplace_back(new Derived0(v));
					break;
				case 1:
					d.emplace_back(new Derived1(v));
					break;
				case 2:
					d.emplace_back(new Derived2(v));
					break;
				case 3:
					d.emplace_back(new Derived3(v));
					break;
				case 4:
					try {
						d.push_back(new thrower(v));
					} catch (int) {
						d.push_back(nullptr);
					}
				}
			}

			unsigned accum{};
			meter.measure([&] {
				for (auto x : d) {
					if (x) {
						accum += (*x)();
					}
				}
				return accum;
			});
			for (auto x : d) {
				delete x;
			}
		};
		BENCHMARK_ADVANCED("unique_ptr, ch")
		(Catch::Benchmark::Chronometer meter) {
			std::vector<std::unique_ptr<Base>> d;
			kblib::FNV32_hash<unsigned> h;
			for (auto i : kblib::range(count)) {
				auto v = h(i);
				switch (v % 4) {
				case 0:
					d.push_back(std::make_unique<Derived0>(v));
					break;
				case 1:
					d.push_back(std::make_unique<Derived1>(v));
					break;
				case 2:
					d.push_back(std::make_unique<Derived2>(v));
					break;
				case 3:
					d.push_back(std::make_unique<Derived3>(v));
					break;
				case 4:
					try {
						d.push_back(std::make_unique<thrower>(v));
					} catch (int) {
						d.push_back(nullptr);
					}
				}
			}

			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (x) {
						accum += (*x)();
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("poly_obj, ch")(Catch::Benchmark::Chronometer meter) {
			std::vector<poly_t> d;
			kblib::FNV32_hash<unsigned> h;
			for (auto i : kblib::range(count)) {
				auto v = h(i);
				switch (v % 5) {
				case 0:
					d.push_back(poly_t::make<Derived0>(v));
					break;
				case 1:
					d.push_back(poly_t::make<Derived1>(v));
					break;
				case 2:
					d.push_back(poly_t::make<Derived2>(v));
					break;
				case 3:
					d.push_back(poly_t::make<Derived3>(v));
					break;
				case 4:
					try {
						d.push_back(poly_t::make<thrower>(v));
					} catch (int) {
						d.emplace_back(nullptr);
					}
				}
			}

			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (x) {
						accum += x();
					}
				}
				return accum;
			});
		};
		std::vector<std::function<unsigned()>> df;
		std::vector<std::variant<Derived0, Derived1, Derived2, Derived3, thrower>>
		    d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 5) {
			case 0:
				df.emplace_back(Derived0(v));
				d.emplace_back(Derived0(v));
				break;
			case 1:
				df.emplace_back(Derived1(v));
				d.emplace_back(Derived1(v));
				break;
			case 2:
				df.emplace_back(Derived2(v));
				d.emplace_back(Derived2(v));
				break;
			case 3:
				df.emplace_back(Derived3(v));
				d.emplace_back(Derived3(v));
				break;
				// Test speed of exception throwing and catching
			case 4:
				try {
					auto& b = df.emplace_back(thrower());
					b = thrower(v);
				} catch (int) {
				}
				// These have to be done in separate try blocks because otherwise
				// df would throw and d wouldn't get pushed
				try {
					auto& b = d.emplace_back(std::in_place_type_t<thrower>{}, 0);
					b = thrower(v);
				} catch (int) {
				}
			}
		}
		CHECK(d.size() == df.size());
		CHECK(df.size() == count);
		CHECK(d.size() == count);
		BENCHMARK_ADVANCED("std::function, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : df) {
					if (x) {
						accum += x();
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit2_nop(v, f...), ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					kblib::visit2_nop(x, [&](const auto& v) { accum += v(); });
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("std::get_if, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (auto* const p = std::get_if<0>(&x)) {
						accum += (*p)();
					} else if (auto* const p = std::get_if<1>(&x)) {
						accum += (*p)();
					} else if (auto* const p = std::get_if<2>(&x)) {
						accum += (*p)();
					} else if (auto* const p = std::get_if<3>(&x)) {
						accum += (*p)();
					} else if (auto* const p = std::get_if<4>(&x)) {
						accum += (*p)();
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("switch (v.index()), ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					switch (x.index()) {
					case 0:
						accum += std::get<0>(x)();
						break;
					case 1:
						accum += std::get<1>(x)();
						break;
					case 2:
						accum += std::get<2>(x)();
						break;
					case 3:
						accum += std::get<3>(x)();
						break;
					case 4:
						accum += std::get<4>(x)();
						break;
					default:;
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("std::function, ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : df) {
					try {
						accum += x();
					} catch (const std::bad_function_call&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("std::visit(v, f), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += std::visit([](const auto& v) { return v(); }, x);
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit(v)(f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit(x)([](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("visit_indexed(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit_indexed(
						    x, [](auto, const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
		BENCHMARK_ADVANCED("kblib::visit2(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit2(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
					}
				}
				return accum;
			});
		};
	}
	auto end = std::chrono::steady_clock::now();
	std::chrono::duration<float, std::ratio<1, 1>> time = end - start;
	std::cout << "\n\nProfiling took " << time.count() << " seconds\n";

	// All runs should produce identical results
	unsigned expected_value = reproducibility_test[0].first;

	std::cout << "Checksum: " << expected_value;

	for (auto i : kblib::range(std::size_t(1), reproducibility_test.size())) {
		const auto& run = reproducibility_test[i];
		if (run.first != expected_value) {
			WARN(i << ": " << run.second << ": " << run.first
			       << " != " << expected_value);
		}
		REQUIRE(run.first == expected_value);
	}
}

#	endif // not defined(FAST_TEST)

#endif // KBLIB_USE_CXX17
