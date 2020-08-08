#ifndef TRIE_H
#define TRIE_H

#include "tdecl.h"
#include "traits.h"

namespace kblib {

enum class extractor_policy {
	forward_iteration,
	random_access,
};

template <typename Container>
struct iterator_extractor {
	using value_type =
	    typename std::remove_cv<typename std::remove_reference<decltype(
	        *begin(std::declval<Container>()))>::type>::type;
};

template <typename Container>
struct indexer_extractor : iterator_extractor<Container> {

	template <typename index_type>
	constexpr static decltype(auto)
	subscript(Container&& c, index_type index) noexcept(noexcept(c[index])) {
		return c[index];
	}
};

template <typename Container, typename = void>
struct extractor_policy_for {
	constexpr static extractor_policy value =
	    extractor_policy::forward_iteration;
};

template <typename Container>
struct extractor_policy_for<Container,
                            void_t<decltype(std::declval<Container>()[0])>> {
	constexpr static extractor_policy value = extractor_policy::random_access;
};

namespace detail {

	template <typename Elem, typename Value>
	struct node {};

} // namespace detail

template <typename Key, typename T, typename Extractor,
          bool = kblib::is_linear_container<Key>::value>
class trie {
 public:
	using key_type = Key;
	using mapped_type = T;
	using value_type = std::pair<key_type, mapped_type>;
	using extractor = Extractor;
	using node_type = detail::node<typename extractor::value_type, value_type>;

	using size_type = std::size_t;
	using difference_type = std::ptrdiff_t;
	using reference = T&;
	using const_reference = const T&;
	using pointer = T*;
	using const_pointer = const T*;

	using iterator = void;
	using const_iterator = void;
	using reverse_iterator = std::reverse_iterator<iterator>;
	using const_reverse_iterator = std::reverse_iterator<const_iterator>;

 private:
	std::unique_ptr<node_type> root;

 public:
};

template <typename KeyElem, typename T, typename Extractor>
class trie<KeyElem, T, Extractor, false> {};

template <typename KeyElem, typename T, typename Extractor>
class trie<KeyElem[], T, Extractor, false> {};

} // namespace kblib

#endif // TRIE_H
