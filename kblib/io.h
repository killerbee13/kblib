#ifndef KBLIB_IO_H
#define KBLIB_IO_H

#include "fakestd.h"

#include <fstream>
#include <functional>
#include <optional>
#include <string>

namespace kblib {

#if KBLIB_USE_CXX17
/**
 * @brief Read the entire contents of a file into a container, such as
 * std::string or std::vector<char>.
 *
 * @param filename The filename to open.
 * @return std::optional<D> The contents of the file, if reading was successful.
 */
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
  } else {
    return std::nullopt;
  }
}
#endif

/**
 * @brief By-value std::getline wrapper.
 *
 * @param is The stream to extract from.
 * @return std::string A single line of text from the stream.
 */
inline std::string getline(std::istream& is) {
  std::string ret;
  std::getline(is, ret);
  return ret;
}

/**
 * @brief Consume all non-spaces to first break, then eat that, too.
 *
 * @param is
 * @return std::istream
 */
inline std::istream& eatWord(std::istream& is) {
  do {
    is.get();
  } while (is && !isspace(is.peek()));
  return is;
}

/**
 * @brief Eat spaces, don't eat an extra.
 *
 * @param is
 * @return std::istream
 */
inline std::istream& eatSpace(std::istream& is) {
  while (is && isspace(is.peek())) {
    is.get();
  }
  return is;
}

/**
 * @brief A helper class for wrapping stream manipulators.
 */
template <typename F>
struct get_manip {
  F _f;
};

/**
 * @brief Read a whole line into a std::basic_string-like class template.
 *
 * When used like
 *  os >> get_line(str);
 * for a std::istream& os and std::string str, reads a full line into str
 * instead of just a single word.
 *
 * @param str The string to read a line into.
 */
template <typename CharT, typename... O,
          template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str) {
  auto _f = [&](auto& istream) -> decltype(istream) {
    std::getline(istream, str);
    return istream;
  };
  return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Read a delimited string into a std::basic_string-like class template.
 *
 * When used like
 *  os >> get_line(str, '\n');
 * for a std::istream& os and std::string str, reads a full line into str
 * instead of just a single word.
 *
 * @param str The string to read into.
 * @param delim The delimiter at which to stop reading text.
 */
template <typename CharT, typename... O,
          template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str, CharT delim) {
  auto _f = [&](auto& istream) -> decltype(istream) {
    std::getline(istream, str, delim);
    return istream;
  };
  return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Actually calls the manipulator.
 */
template <typename F, typename CharT, typename Tr>
std::basic_istream<CharT, Tr>& operator>>(std::basic_istream<CharT, Tr>& is,
                                          get_manip<F> func) {
  return func._f(is);
}

/**
 * @brief Actually calls the manipulator.
 */
template <typename F, typename CharT, typename Tr>
std::basic_ostream<CharT, Tr>& operator<<(std::basic_ostream<CharT, Tr>& is,
                                          get_manip<F> func) {
  return func._f(is);
}

}  // namespace kblib

#endif  // KBLIB_IO_H
