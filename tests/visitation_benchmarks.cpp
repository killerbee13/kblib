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
#include "catch2/catch.hpp"

// #define FAST_TEST

#if KBLIB_USE_CXX17

namespace {
struct Base {
	virtual auto operator()() const noexcept -> unsigned = 0;
	Base() = default;
	Base(const Base&) noexcept {}
	Base(Base&&) noexcept {}
	Base& operator=(const Base&) noexcept { return *this; }
	Base& operator=(Base&&) noexcept { return *this; };
	virtual ~Base() = default;

	// this is used, but the compiler doesn't think so because it's in a .cpp
	// file
	[[maybe_unused]] constexpr static inline std::size_t max_derived_size
	    = sizeof(unsigned);
};

struct Derived1 final : Base {
	auto operator()() const noexcept -> unsigned override { return member; }
	unsigned member;
	Derived1(unsigned i)
	    : member(i) {}
};

struct Derived2 final : Base {
	auto operator()() const noexcept -> unsigned override { return member * 2; }
	unsigned member;
	Derived2(unsigned i)
	    : member(i) {}
};

struct Derived3 final : Base {
	auto operator()() const noexcept -> unsigned override { return member / 2; }
	unsigned member;
	Derived3(unsigned i)
	    : member(i) {}
};

struct Derived4 final : Base {
	auto operator()() const noexcept -> unsigned override { return ~member; }
	unsigned member;
	Derived4(unsigned i)
	    : member(i) {}
};

struct thrower final : Base {
	auto operator()() const noexcept -> unsigned override { return member; }

	unsigned member;

	thrower(unsigned i = 0)
	    : member(i) {
		check_throw(i);
	}
	// delay throwing until move
	thrower(unsigned i, std::nothrow_t)
	    : member(i) {}
	thrower(const thrower&) = default;
	thrower(thrower&& other)
	    : member(other.member) {
		check_throw(other.member);
	}
	auto operator=(const thrower& other) -> thrower& {
		check_throw(other.member);
		member = other.member;
		return *this;
	}
	auto operator=(thrower&& other) -> thrower& {
		check_throw(other.member);
		member = other.member;
		return *this;
	}

 private:
	static auto check_throw(unsigned v) -> void {
		if ((v & 3u) == 0) {
			throw 0;
		}
	}
};

static_assert(std::is_move_constructible_v<Derived1>);
static_assert(std::is_move_constructible_v<Derived2>);
static_assert(std::is_move_constructible_v<Derived3>);
static_assert(std::is_move_constructible_v<Derived4>);
static_assert(std::is_move_constructible_v<thrower>);

struct fptr {
	unsigned member;
	auto (*p)(const fptr*) noexcept -> unsigned;
	auto operator()() const noexcept -> unsigned { return p(this); }
};

auto fptr_d1(const fptr* p) noexcept -> unsigned { return p->member; }
auto fptr_d2(const fptr* p) noexcept -> unsigned { return p->member * 2; }
auto fptr_d3(const fptr* p) noexcept -> unsigned { return p->member / 2; }
auto fptr_d4(const fptr* p) noexcept -> unsigned { return ~p->member; }

auto make_fptr(unsigned v) noexcept -> fptr {
	switch (v % 4u) {
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
}

} // namespace

#	ifndef FAST_TEST
TEST_CASE("poly_obj performance") {
	const auto start = std::chrono::steady_clock::now();
#		ifdef NDEBUG
	constexpr unsigned count = 1000;
#		else
	constexpr unsigned count = 500;
#		endif

#		ifdef SANITIZERS
#			define STR(s) #s
	std::cout << "Sanitizers active\n";
#		endif

	struct repro_entry {
		unsigned accum;
		std::string_view name;
		unsigned line;
	};

	std::vector<repro_entry> repro_test;
	std::vector<repro_entry> repro_test_ch;
	std::vector<repro_entry> repro_test_sk;
	using poly_t = kblib::poly_obj<
	    Base, sizeof(Derived1),
	    kblib::poly_obj_traits<Base,
	                           kblib::detail_poly::construct_traits<thrower>>>;

	auto push_checksum
	    = [&](auto& vec, unsigned s, std::string_view name, unsigned line) {
		      auto begin = vec.begin();
		      auto end = vec.end();
		      // Take only the first result for each run type
		      if (std::find_if(begin, end,
		                       [&](const auto& p) { return p.line == line; })
		          == end) {
			      vec.push_back({s, name, line});
		      }
	      };
#		define push_checksum(vec, s, name) push_checksum(vec, s, name, __LINE__)

	/*
	 * 2024-02-24:
	 * Release / Debug, all times in nanoseconds:
	 *
	 *   Four separate contiguous arrays
	 * baseline:                   37.6897 / 3977.27
	 *   Polymorphism, always valid
	 * raw pointer:                279.219 / 5350.22
	 * unique_ptr:                 220.259 / 6114.19
	 * poly_obj:                   219.71  / 7592.35
	 * function pointer            227.07  / 4866.43
	 * function pointer (wrapped)  220.759 / 5473.22
	 *   Type erasure, always valid
	 * std::function:              219.991 / 8519.65
	 * std::visit(v, f):           220.499 / 23276.3
	 * kblib::visit(v, f...):      220.814 / 25983.3
	 * kblib::visit(v)(f...):      248.823 / 26336.9
	 * visit_indexed:              220.866 / 19938.9
	 * kblib::visit2:              132.788 / 15481.0
	 * visit2_nop:          204.583 / 16972.3
	 * std::get_if:                198.42  / 13370.1
	 * switch (v.index):           202.93  / 12705.9
	 *   Polymorphism, checking for invalid
	 * raw pointer, ch:            693.886 / 5174.58
	 * unique_ptr, ch:             (incorrect test)
	 * poly_obj, ch:               694.705 / 7683.41
	 *   Type erasure, checking for invalid
	 * std::function, ch:          632.045 / 9070.61
	 * visit2_nop, ch:      547.768 / 19497.8
	 * std::get_if, ch:            539.255 / 15197.5
	 * switch(v.index()), ch:      560.278 / 13331.7
	 *   Type erasure, exceptions for invalid
	 * std::function, ex:          756.938 / 8813.42
	 * std::visit(v, f), ex:       769.363 / 23981.1
	 * kblib::visit(v, f...), ex:  774.633 / 27869.6
	 * kblib::visit(v)(f...), ex:  769.774 / 28470.2
	 * visit_indexed, ex:          642.899 / 20355.8
	 * kblib::visit2, ex:          447.362 / 17819.4
	 *
	 * Overall test runtime:  33.3043s / 324.342s
	 *
	 * Conclusions:
	 *
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
	 * visit2_nop:           8.650 / 222.502
	 * std::get_if:                 8.580 / 158.164
	 * switch (v.index):            8.984 / 170.739
	 *   Polymorphism, checking for invalid
	 * raw pointer, ch:            15.859 /  56.917
	 * unique_ptr, ch:             18.344 / 108.046
	 * poly_obj, ch:               18.290 /  96.168
	 *   Type erasure, checking for invalid
	 * std::function, ch:          25.200 / 122.038
	 * visit2_nop, ch:      12.327 / 322.764
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

	BENCHMARK_ADVANCED("baseline")(Catch::Benchmark::Chronometer meter) {
		std::vector<Derived1> d1;
		std::vector<Derived2> d2;
		std::vector<Derived3> d3;
		std::vector<Derived4> d4;
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
		push_checksum(repro_test, accum, "baseline");
	};
	BENCHMARK_ADVANCED("raw pointer")(Catch::Benchmark::Chronometer meter) {
		std::vector<Base*> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.push_back(new Derived1(v));
				break;
			case 1:
				d.push_back(new Derived2(v));
				break;
			case 2:
				d.push_back(new Derived3(v));
				break;
			case 3:
				d.push_back(new Derived4(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (auto x : d) {
				accum += (*x)();
			}
			return accum;
		});
		push_checksum(repro_test, accum, "raw pointer");
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
				d.push_back(std::make_unique<Derived1>(v));
				break;
			case 1:
				d.push_back(std::make_unique<Derived2>(v));
				break;
			case 2:
				d.push_back(std::make_unique<Derived3>(v));
				break;
			case 3:
				d.push_back(std::make_unique<Derived4>(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += (*x)();
			}
			return accum;
		});
		push_checksum(repro_test, accum, "unique pointer");
	};
	BENCHMARK_ADVANCED("poly_obj")(Catch::Benchmark::Chronometer meter) {
		std::vector<poly_t> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
			case 0:
				d.emplace_back(poly_t::tag<Derived1>{}, v);
				break;
			case 1:
				d.emplace_back(poly_t::tag<Derived2>{}, v);
				break;
			case 2:
				d.emplace_back(poly_t::tag<Derived3>{}, v);
				break;
			case 3:
				d.emplace_back(poly_t::tag<Derived4>{}, v);
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += x();
			}
			return accum;
		});
		push_checksum(repro_test, accum, "poly_obj");
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
		push_checksum(repro_test, accum, "function pointer");
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
		push_checksum(repro_test, accum, "function pointer (wrapped)");
	};
	BENCHMARK_ADVANCED("std::function")(Catch::Benchmark::Chronometer meter) {
		std::vector<std::function<unsigned()>> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.emplace_back(Derived1(v));
				break;
			case 1:
				d.emplace_back(Derived2(v));
				break;
			case 2:
				d.emplace_back(Derived3(v));
				break;
			case 3:
				d.emplace_back(Derived4(v));
			}
		}

		unsigned accum{};
		meter.measure([&] {
			for (const auto& x : d) {
				accum += x();
			}
			return accum;
		});
		push_checksum(repro_test, accum, "std::function");
	};
	{
		std::vector<std::variant<Derived1, Derived2, Derived3, Derived4>> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 4) {
			case 0:
				d.emplace_back(Derived1(v));
				break;
			case 1:
				d.emplace_back(Derived2(v));
				break;
			case 2:
				d.emplace_back(Derived3(v));
				break;
			case 3:
				d.emplace_back(Derived4(v));
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
			push_checksum(repro_test, accum, "std::visit(v, f)");
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
			push_checksum(repro_test, accum, "kblib::visit(v, f...)");
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
			push_checksum(repro_test, accum, "kblib::visit(v)(f...)");
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
			push_checksum(repro_test, accum, "visit_indexed(v, f...)");
		};
		BENCHMARK_ADVANCED("visit_indexed_old(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit_indexed_old(
					    x, [](auto, const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(repro_test, accum, "visit_indexed_old(v, f...)");
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
			push_checksum(repro_test, accum, "kblib::visit2(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit2b(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit2b(x, [](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(repro_test, accum, "kblib::visit2b(v, f...)");
		};
		BENCHMARK_ADVANCED("kblib::visit2s(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					accum += kblib::visit2s(x, [](const auto& v) { return v(); });
				}
				return accum;
			});
			push_checksum(repro_test, accum, "kblib::visit2s(v, f...)");
		};
		BENCHMARK_ADVANCED("visit2_nop(v, f...)")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					kblib::visit2_nop(x, [&](const auto& v) { accum += v(); });
				}
				return accum;
			});
			push_checksum(repro_test, accum, "visit2_nop(v, f...)");
		};
		BENCHMARK_ADVANCED("std::get_if")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (auto* p = std::get_if<0>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<1>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<2>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<3>(&x)) {
						accum += (*p)();
					}
				}
				return accum;
			});
			push_checksum(repro_test, accum, "std::get_if");
		};
		BENCHMARK_ADVANCED("switch")
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
			push_checksum(repro_test, accum, "switch");
		};
		BENCHMARK_ADVANCED("unsafe_get switch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			meter.measure([&] {
				for (const auto& x : d) {
					switch (x.index()) {
					case 0:
						accum += (*std::get_if<0>(&x))();
						break;
					case 1:
						accum += (*std::get_if<1>(&x))();
						break;
					case 2:
						accum += (*std::get_if<2>(&x))();
						break;
					case 3:
						accum += (*std::get_if<3>(&x))();
						break;
					default:;
					}
				}
				return accum;
			});
			push_checksum(repro_test, accum, "unsafe_get switch");
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
					d.push_back(new Derived1(v));
					break;
				case 1:
					d.push_back(new Derived2(v));
					break;
				case 2:
					d.push_back(new Derived3(v));
					break;
				case 3:
					d.push_back(new Derived4(v));
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
			unsigned sk{};
			meter.measure([&] {
				for (auto x : d) {
					if (x) {
						accum += (*x)();
					} else {
						++sk;
					}
				}
				return accum;
			});
			for (auto x : d) {
				delete x;
			}
			push_checksum(repro_test_ch, accum, "raw pointer, ch");
			push_checksum(repro_test_sk, sk, "raw pointer, ch");
		};
		BENCHMARK_ADVANCED("unique_ptr, ch")
		(Catch::Benchmark::Chronometer meter) {
			std::vector<std::unique_ptr<Base>> d;
			kblib::FNV32_hash<unsigned> h;
			for (auto i : kblib::range(count)) {
				auto v = h(i);
				switch (v % 5) {
				case 0:
					d.push_back(std::make_unique<Derived1>(v));
					break;
				case 1:
					d.push_back(std::make_unique<Derived2>(v));
					break;
				case 2:
					d.push_back(std::make_unique<Derived3>(v));
					break;
				case 3:
					d.push_back(std::make_unique<Derived4>(v));
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
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (x) {
						accum += (*x)();
					} else {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "unique_ptr, ch");
			push_checksum(repro_test_sk, sk, "unique_ptr, ch");
		};
		BENCHMARK_ADVANCED("poly_obj, ch")(Catch::Benchmark::Chronometer meter) {
			std::vector<poly_t> d;
			kblib::FNV32_hash<unsigned> h;
			for (auto i : kblib::range(count)) {
				auto v = h(i);
				switch (v % 5) {
				case 0:
					d.push_back(poly_t::make<Derived1>(v));
					break;
				case 1:
					d.push_back(poly_t::make<Derived2>(v));
					break;
				case 2:
					d.push_back(poly_t::make<Derived3>(v));
					break;
				case 3:
					d.push_back(poly_t::make<Derived4>(v));
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
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (x) {
						accum += x();
					} else {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "poly_obj, ch");
			push_checksum(repro_test_sk, sk, "poly_obj, ch");
		};
		std::vector<std::function<unsigned()>> df;
		std::vector<std::variant<Derived1, Derived2, Derived3, Derived4, thrower>>
		    d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = static_cast<unsigned>(h(i));
			switch (v % 5) {
			case 0:
				df.emplace_back(Derived1(v));
				d.emplace_back(Derived1(v));
				break;
			case 1:
				df.emplace_back(Derived2(v));
				d.emplace_back(Derived2(v));
				break;
			case 2:
				df.emplace_back(Derived3(v));
				d.emplace_back(Derived3(v));
				break;
			case 3:
				df.emplace_back(Derived4(v));
				d.emplace_back(Derived4(v));
				break;
				// Test speed of exception throwing and catching
			case 4:
				try {
					df.emplace_back(thrower(v));
				} catch (int) {
					df.emplace_back();
				}
				// These have to be done in separate try blocks because otherwise
				// df would throw and d wouldn't get pushed. this uses assignment
				// instead of construction because that's the only way to make a
				// variant valueless_by_exception
				try {
					auto& b = d.emplace_back(std::in_place_type_t<Derived1>{}, 0);
					decltype(d)::value_type tmp{std::in_place_type_t<thrower>{}, v,
					                            std::nothrow};
					b = std::move(tmp);
				} catch (int) {
					REQUIRE(d.back().valueless_by_exception());
				}
			}
		}
		BENCHMARK_ADVANCED("std::function, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : df) {
					if (x) {
						accum += x();
					} else {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "std::function, ch");
			push_checksum(repro_test_sk, sk, "std::function, ch");
		};
		BENCHMARK_ADVANCED("visit2_nop(v, f...), ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					kblib::visit2_nop(x, [&](const auto& v) { accum += v(); });
					if (x.valueless_by_exception()) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "visit2_nop(v, f...), ch");
			push_checksum(repro_test_sk, sk, "visit2_nop(v, f...), ch");
		};
		BENCHMARK_ADVANCED("std::get_if, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					if (auto* p = std::get_if<0>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<1>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<2>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<3>(&x)) {
						accum += (*p)();
					} else if (auto* p = std::get_if<4>(&x)) {
						accum += (*p)();
					} else {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "std::get_if, ch");
			push_checksum(repro_test_sk, sk, "std::get_if, ch");
		};
		BENCHMARK_ADVANCED("switch, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
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
					default:
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "switch, ch");
			push_checksum(repro_test_sk, sk, "switch, ch");
		};
		BENCHMARK_ADVANCED("unsafe_get switch, ch")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					switch (x.index()) {
					case 0:
						accum += (*std::get_if<0>(&x))();
						break;
					case 1:
						accum += (*std::get_if<1>(&x))();
						break;
					case 2:
						accum += (*std::get_if<2>(&x))();
						break;
					case 3:
						accum += (*std::get_if<3>(&x))();
						break;
					case 4:
						accum += (*std::get_if<4>(&x))();
						break;
					default:
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "switch, ch");
			push_checksum(repro_test_sk, sk, "switch, ch");
		};
		BENCHMARK_ADVANCED("std::function, ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : df) {
					try {
						accum += x();
					} catch (const std::bad_function_call&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "std::function, ex");
			push_checksum(repro_test_sk, sk, "std::function, ex");
		};
		BENCHMARK_ADVANCED("std::visit(v, f), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += std::visit([](const auto& v) { return v(); }, x);
					} catch (const std::bad_variant_access&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "std::visit(v, f), ex");
			push_checksum(repro_test_sk, sk, "std::visit(v, f), ex");
		};
		BENCHMARK_ADVANCED("kblib::visit(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "kblib::visit(v, f...), ex");
			push_checksum(repro_test_sk, sk, "kblib::visit(v, f...), ex");
		};
		BENCHMARK_ADVANCED("kblib::visit(v)(f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit(x)([](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "kblib::visit(v)(f...), ex");
			push_checksum(repro_test_sk, sk, "kblib::visit(v)(f...), ex");
		};
		BENCHMARK_ADVANCED("visit_indexed(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit_indexed(
						    x, [](auto, const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "visit_indexed(v, f...), ex");
			push_checksum(repro_test_sk, sk, "visit_indexed(v, f...), ex");
		};
		BENCHMARK_ADVANCED("kblib::visit2(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit2(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "kblib::visit2(v, f...), ex");
			push_checksum(repro_test_sk, sk, "kblib::visit2(v, f...), ex");
		};
		BENCHMARK_ADVANCED("kblib::visit2b(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit2b(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "kblib::visit2b(v, f...), ex");
			push_checksum(repro_test_sk, sk, "kblib::visit2b(v, f...), ex");
		};
		BENCHMARK_ADVANCED("kblib::visit2s(v, f...), ex")
		(Catch::Benchmark::Chronometer meter) {
			unsigned accum{};
			unsigned sk{};
			meter.measure([&] {
				for (const auto& x : d) {
					try {
						accum += kblib::visit2s(x, [](const auto& v) { return v(); });
					} catch (const std::bad_variant_access&) {
						++sk;
					}
				}
				return accum;
			});
			push_checksum(repro_test_ch, accum, "kblib::visit2s(v, f...), ex");
			push_checksum(repro_test_sk, sk, "kblib::visit2s(v, f...), ex");
		};
	}
	auto end = std::chrono::steady_clock::now();
	std::chrono::duration<float, std::ratio<1, 1>> time = end - start;
	std::cout << "\n\nProfiling took " << time.count() << " seconds\n";

	// All runs should produce identical results
	auto check_repro = [&](auto& v, std::string_view name) {
		unsigned expected_value = v[0].accum;
		for (auto i : kblib::range(std::size_t(1), v.size())) {
			const auto& run = v[i];
			INFO(name << '[' << i << "]: " << run.name << '[' << run.line
			          << "]: " << run.accum << " != " << expected_value);
			CHECK(run.accum == expected_value);
		}
		// std::clog << name << ": expected " << expected_value << '\n';
	};
#		define check_repro(v) check_repro(v, #v)
	check_repro(repro_test);
	check_repro(repro_test_ch);
	check_repro(repro_test_sk);
}

#	endif // not defined(FAST_TEST)

#endif // KBLIB_USE_CXX17
