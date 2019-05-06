#ifndef KBLIB_IO_H
#define KBLIB_IO_H

#include "kblib_fakestd.h"

#include <fstream>
#include <functional>
#include <optional>
#include <string>

namespace kblib {

#if __cplusplus >= 201703L
template <typename D = std::string, typename string>
std::optional<D> get_file_contents(const string& filename) {
  static_assert(std::is_trivially_copyable_v<typename D::value_type>,
                "D must be a sequence of trivial types");
  std::ifstream in(filename, std::ios::in | std::ios::binary);
  if (in) {
    D contents;
    in.seekg(0, std::ios::end);
    contents.resize(in.tellg());
    in.seekg(0, std::ios::beg);
    in.read(reinterpret_cast<char*>(&contents[0]),
            contents.size() * sizeof(typename D::value_type));
    in.close();
    return contents;
  }
  return std::nullopt;
}
#endif

inline std::string getline(std::istream& is) {
  std::string ret;
  std::getline(is, ret);
  return ret;
}

// Consume all non-spaces to first break, then eat that, too
inline std::istream& eatWord(std::istream& is) {
  do {
    is.get();
  } while (is && !isspace(is.peek()));
  return is;
}

// Eat spaces, don't eat an extra
inline std::istream& eatSpace(std::istream& is) {
  while (is && isspace(is.peek())) {
    is.get();
  }
  return is;
}

template <typename F>
struct get_manip {
  F _f;
};

template <typename CharT, typename... O, template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str) {
  auto _f = [&](auto& istream) -> decltype(istream) {
    std::getline(istream, str);
    return istream;
  };
  return get_manip<decltype(_f)>{_f};
}

template <typename CharT, typename... O, template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str, CharT delim) {
  auto _f = [&](auto& istream) -> decltype(istream) {
    std::getline(istream, str, delim);
    return istream;
  };
  return get_manip<decltype(_f)>{_f};
}

template <typename F, typename CharT, typename Tr>
std::basic_istream<CharT, Tr>& operator>>(std::basic_istream<CharT, Tr>& is, get_manip<F> func) {
  return func._f(is);
}

template <typename F, typename CharT, typename Tr>
std::basic_ostream<CharT, Tr>& operator<<(std::basic_ostream<CharT, Tr>& is, get_manip<F> func) {
  return func._f(is);
}

}  // namespace kblib

#endif  // KBLIB_IO_H
