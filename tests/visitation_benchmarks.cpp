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

//#define FAST_TEST

#if KBLIB_USE_CXX17

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
		// push_checksum(accum, "baseline");
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
		push_checksum(accum, "unique pointer");
	};
	BENCHMARK_ADVANCED("poly_obj")(Catch::Benchmark::Chronometer meter) {
		std::vector<poly_t> d;
		kblib::FNV32_hash<unsigned> h;
		for (auto i : kblib::range(count)) {
			auto v = h(i);
			switch (v % 4) {
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
		push_checksum(accum, "std::function");
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
					d.emplace_back(thrower());
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
	for (auto i : kblib::range(std::size_t(1), reproducibility_test.size())) {
		const auto& run = reproducibility_test[i];
		INFO(i << ": " << run.second << ": " << run.first
		       << " != " << expected_value);
		REQUIRE(run.first == expected_value);
	}
}

#	endif // not defined(FAST_TEST)

#endif // KBLIB_USE_CXX17
