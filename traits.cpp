#include "kblib/traits.h"

struct k {
	char c[10];
};

static_assert(not std::is_reference<kblib::member_of_t<decltype(&k::c)>>::value,
              "");
