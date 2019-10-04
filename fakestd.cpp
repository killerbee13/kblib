#include "kblib/fakestd.h"

template <class... Args>
struct Ref {

	auto operator-> () {
		return kblib::unary_identity_t<Args...>{};
	} // present only if sizeof...(Args) == 1
};

// static decltype(&Ref<int>::operator->) f;
// static decltype(&Ref<int, int>::operator->) f2;

static Ref<int> r;
[[maybe_unused]] static Ref<int, int> r2;

void blah() {
	r.operator->();
	// r2.operator->();
}
