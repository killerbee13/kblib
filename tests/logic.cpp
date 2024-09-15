#include "catch/catch.hpp"

#include "kblib/logic.h"

#include <array>
#include <iostream>
#include <string>

template <int I1, int I2>
auto multi_template() -> int {
	return I1 * I2;
}

template <int... Is>
struct tag {};

template <std::size_t COL, typename T, std::size_t rows, std::size_t cols,
          std::size_t... row>
constexpr auto get_col(const std::array<std::array<T, cols>, rows>& in,
                       std::index_sequence<row...>) -> std::array<T, rows> {
	static_assert(rows == sizeof...(row), "");
	return {std::get<COL>(std::get<row>(in))...};
}

template <typename T, std::size_t rows, std::size_t cols, std::size_t... row,
          std::size_t... col>
constexpr auto transpose_arr_impl(
    const std::array<std::array<T, cols>, rows>& in,
    std::index_sequence<col...>) -> std::array<std::array<T, rows>, cols> {
	return {get_col<col>(in, std::make_index_sequence<rows>{})...};
}

template <typename T, std::size_t rows, std::size_t cols>
constexpr auto transpose_arr(std::array<std::array<T, cols>, rows> in)
    -> std::array<std::array<T, rows>, cols> {
	return transpose_arr_impl(in, std::make_index_sequence<cols>{});
	// C++17 version
	/*
	std::array<std::array<T, cols>, rows> ret{};
	for (std::size_t C = 0; C != cols; ++C) {
	   for (std::size_t R = 0; R != rows; ++R) {
	      ret[R][C] = in[C][R];
	   }
	}
	return ret;*/
}

TEST_CASE("transpose") {
	std::array<std::array<int, 4>, 3> input{
	    {{0, 1, 2, 3}, {4, 5, 6, 7}, {8, 9, 10, 11}}};
	std::array<std::array<int, 3>, 4> transposed{
	    {{0, 4, 8}, {1, 5, 9}, {2, 6, 10}, {3, 7, 11}}};

	REQUIRE(transpose_arr(input) == transposed);
}

template <int I2, int... Is1>
constexpr auto gen_row(tag<Is1...>) -> std::array<int (*)(), 8> {
	return {&multi_template<Is1, I2>...};
}

template <typename Tag1, int... Is2>
constexpr auto gen_dispatch_table() -> std::array<std::array<int (*)(), 8>, 8> {
	std::array<std::array<int (*)(), 8>, 8> tmp = {gen_row<Is2>(Tag1{})...};
	return transpose_arr(tmp);
}

constexpr std::array<std::array<int (*)(), 8>, 8> dispatch_table
    = gen_dispatch_table<tag<6, 13, 14, 19, 22, 22, 24, 27>, 1, 3, 5, 6, 18, 21,
                         25, 31>();

auto multi_dispatcher(int i1, int i2) -> int {
	constexpr std::array<int, 8> i1_lookup = {6, 13, 14, 19, 22, 22, 24, 27};
	constexpr std::array<int, 8> i2_lookup = {1, 3, 5, 6, 18, 21, 25, 31};
	auto find = [](int v, const std::array<int, 8>& arr) {
		for (std::size_t i = 0; i != 8; ++i) {
			if (arr[i] == v) {
				return i;
			}
		}
		throw std::out_of_range(std::to_string(v));
	};

	return dispatch_table[find(i1, i1_lookup)][find(i2, i2_lookup)]();
}

TEST_CASE("run-time template dispatching") {
	for (auto x : {6, 13, 14, 19, 22, 22, 24, 27}) {
		for (auto y : {1, 3, 5, 6, 18, 21, 25, 31}) {
			// std::cout<<x<<" * "<<y<<" = "<<multi_dispatcher(x, y)<<'\n';
			REQUIRE(multi_dispatcher(x, y) == x * y);
		}
	}
}
