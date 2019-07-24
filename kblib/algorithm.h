#ifndef ALGORITHM_H
#define ALGORITHM_H

#include "tdecl.h"
#include <tuple>

namespace kblib {

template <typename IIt1, typename EIt, typename IIt2>
struct zip_iterator {
	IIt1 pos1{};
	EIt end1{};
	IIt2 pos2{};

	zip_iterator& operator++() {
		++pos1;
		++pos2;
		return *this;
	}
	zip_iterator operator++(int) {
		auto tmp = *this;
		++pos1;
		++pos2;
		return tmp;
	}

	auto operator*() {
		return std::forward_as_tuple(*pos1, *pos2);
	}

	zip_iterator begin() {return *this;}
	zip_iterator<EIt, EIt, IIt2> end() const {return {end1, end1};}

	friend bool operator==(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 == z2.pos1;
	}
	friend bool operator!=(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 != z2.pos1;
	}
	friend bool operator==(const zip_iterator& z1, zip_iterator<EIt, EIt, IIt2> end) {
		return z1.end1 == end.val;
	}
	friend bool operator!=(const zip_iterator& z1, zip_iterator<EIt, EIt, IIt2> end) {
		return z1.end1 != end.val;
	}
};

template <typename It1, typename It2>
struct zip_iterator<It1, It1, It2> {
	It1 pos1{};
	It1 end1{};
	It2 pos2{};

	zip_iterator& operator++() {
		++pos1;
		++pos2;
		return *this;
	}
	zip_iterator operator++(int) {
		auto tmp = *this;
		++pos1;
		++pos2;
		return tmp;
	}

	auto operator*() {
		return std::forward_as_tuple(*pos1, *pos2);
	}

	zip_iterator begin() const {return *this;}
	zip_iterator end() const {return {end1, end1};}

	friend bool operator==(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 == z2.pos1;
	}
	friend bool operator!=(const zip_iterator& z1, const zip_iterator& z2) {
		return z1.pos1 != z2.pos1;
	}
};

template <typename IIt1, typename EIt, typename IIt2>
zip_iterator<IIt1, EIt, IIt2> zip(IIt1 begin1, EIt end1, IIt2 begin2) {
	return {begin1, end1, begin2};
}

}

#endif // ALGORITHM_H
