#ifndef DIRECT_MAP_H
#define DIRECT_MAP_H

#include <kblib/fakestd.h>
#include <kblib/tdecl.h>

#include <array>
#include <cinttypes>
#include <limits>
#include <optional>

namespace detail {

template <typename T>
constexpr std::uintmax_t
    range_of = static_cast<std::uintmax_t>(std::numeric_limits<T>::max()) -
               std::numeric_limits<T>::min();

}

template <typename Key, typename T>
class direct_map {
 public:
	using key_type = Key;
	using mapped_type = T;
	using value_type = std::pair<const Key, T>;

 private:
	// TODO: Implement, test, and document direct_map
	kblib::heap_value<
	    std::array<std::optional<value_type>, detail::range_of<Key>>>
	    storage;
};

#endif // DIRECT_MAP_H
