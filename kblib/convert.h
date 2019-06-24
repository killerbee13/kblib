#ifndef KBLIB_CONVERT_H
#define KBLIB_CONVERT_H

#include <array>
#include <chrono>
#include <exception>
#include <iomanip>
#include <sstream>
#include <stdexcept>
#include <string>
#include <typeinfo>

#include "traits.h"

#if KBLIB_DEF_MACROS
#define pFromStr(type, val) ::kblib::fromStr<type>((val), #type)
#endif

namespace kblib {

// This only uses RTTI because C++ has no other means to get "int" from a
// template parameter.
template <typename T>
T fromStr(const std::string& val, const char* type = typeid(T).name()) {
  std::stringstream ss(val);
  T ret;
  if (!(ss >> ret).fail())
    return ret;
  else
    throw std::runtime_error("\"" + val + "\" is not a " + type);
}
inline std::string fromStr(const std::string& val, const char*) { return val; }

template <typename T>
std::string toStr(T val) {
  std::stringstream ss;
  ss << val;
  return ss.str();
}
inline std::string toStr(std::string val) { return val; }

template <typename To, typename From>
struct lexical_caster {
  static To cast(const From& val, const char* type) {
    std::stringstream ss;
    ss << val;
    To ret;
    if (!(ss >> ret).fail())
      return ret;
    else
      throw std::runtime_error("Cannot convert \"" + toStr(val) + "\" to " +
                               type);
  }
};

template <typename Same>
struct lexical_caster<Same, Same> {
  static Same cast(const Same& val, const char*) { return val; }
};

template <>
struct lexical_caster<std::string, std::string> {
  static std::string cast(const std::string& val, const char*) { return val; }
};

template <typename From>
struct lexical_caster<std::string, From> {
  static std::string cast(const From& val, const char*) { return toStr(val); }
};

template <typename To>
struct lexical_caster<To, std::string> {
  static To cast(const std::string& val, const char* type) {
    return fromStr<To>(val, type);
  }
};

template <typename To, typename From>
To lexical_cast(const From& val, const char* type = typeid(To).name()) {
  return lexical_caster<To, From>::cast(val, type);
}

#if 0
template <typename To, typename From>
To lexical_cast(const From& val, const char* type = typeid(To).name()) {
  using namespace std::literals;
  if constexpr (std::is_same_v<std::decay_t<To>, std::decay_t<From>>) {
    return val;
  } else if constexpr (std::is_same_v<std::decay_t<To>, std::string>) {
    return toStr(val);
  } else if constexpr (std::is_same_v<std::decay_t<From>, std::string>) {
    return fromStr<To>(val, type);
  } else {
    std::stringstream ss;
    ss << val;
    To ret;
    if (!(ss >> ret).fail())
      return ret;
    else
      throw std::runtime_error("Cannot convert \""s + toStr(val) + "\" to " +
                               type);
  }
}
#endif

template <int base, typename Int>
inline std::string to_string(Int num) {
  static_assert(base <= 62 && base > 0, "Supported bases are 1 thru 62.");
  constexpr auto digits = remove_null_terminator(
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
  std::string ret;
  bool neg = false;
  if (num < 0) {
    neg = true;
    num *= -1;
  } else if (num == 0) {
    return "0";
  }
  do {
    ret.push_back(digits[num % base]);
  } while (num /= base);
  if (neg) {
    ret.push_back('-');
  }
  std::reverse(ret.begin(), ret.end());
  return ret;
}

template <typename Int>
inline std::string to_string(Int num, int base) {
  assert(base <= 62 && base > 0);
  constexpr auto digits = remove_null_terminator(
      "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz");
  std::string ret;
  bool neg = false;
  if (num < 0) {
    neg = true;
    num *= -1;
  } else if (num == 0) {
    return "0";
  }
  do {
    ret.push_back(digits[num % base]);
  } while (num /= base);
  if (neg) {
    ret.push_back('-');
  }
  std::reverse(ret.begin(), ret.end());
  return ret;
}

template <typename E,
          typename = typename std::enable_if<std::is_enum<E>::value>::type>
constexpr auto etoi(E e) {
  return static_cast<std::underlying_type_t<E>>(e);
}

template <int maxBufLen = 4096, typename clock, typename duration>
std::string time_to_str(std::chrono::time_point<clock, duration>& tp,
                        const std::string& fmt = "%F %T") {
  std::time_t time = clock::to_time_t(tp);
  std::tm* tmb = std::localtime(&time);
  std::string ret{maxBufLen, '\0'};
  std::strftime(&ret.front(), maxBufLen, fmt.c_str(), tmb);
  return ret;
}

inline std::string url_encode(const std::string& value) {
  std::ostringstream escaped;
  escaped.fill('0');
  escaped << std::hex;

  for (auto i = value.begin(), n = value.end(); i != n; ++i) {
    char c = (*i);

    // Keep alphanumeric and other accepted characters intact
    if (std::isalnum(c) || c == '-' || c == '_' || c == '.' || c == '~') {
      escaped << c;
    } else {
      // Any other characters are percent-encoded
      escaped << std::uppercase;
      escaped << '%' << std::setw(2) << int((unsigned char)c);
      escaped << std::nouppercase;
    }
  }

  return escaped.str();
}

inline std::string html_encode(const std::string data) {
  std::string buffer;
  buffer.reserve(data.size() + data.size() / 8);
  for (size_t pos = 0; pos != data.size(); ++pos) {
    switch (data[pos]) {
      case '&':
        buffer.append("&amp;");
        break;
      case '\"':
        buffer.append("&quot;");
        break;
      case '\'':
        buffer.append("&apos;");
        break;
      case '<':
        buffer.append("&lt;");
        break;
      case '>':
        buffer.append("&gt;");
        break;
      default:
        buffer.append(&data[pos], 1);
        break;
    }
  }
  return buffer;
}

inline std::string escapify(char c) {
  auto value = (unsigned char)c;
  if (value < ' ' || value == '\x7F' || value & '\x80') {
    constexpr std::array<char, 16> digits{
        remove_null_terminator("0123456789ABCDEF")};
    std::string rc("\\x  ");
    rc[2] = digits[value >> 4];
    rc[3] = digits[value & 15];
    return rc;
  } else {
    return std::string(1, value);
  }
}

// Accepts any sequence of char, returns printable string
template <typename string>
std::string escapify(string value) {
  std::ostringstream ret;
  for (char c : value) {
    if (c < ' ' || c >= '\x7F') {
      ret << escapify(c);
    } else {
      ret << c;
    }
  }
  return ret.str();
}

// Given a string and a pointer into it, calculate the effective index of that
// pointer into a string such as created by kblib::escapify(value)
template <typename string>
int calculate_translated_index(string&& value, const char* p) {
  int counter = 0;
  for (auto&& c : value) {
    if (&c == p) {
      return counter;
    }
    counter += (std::isprint(c)) ? 1 : 4;
  }
  return counter;
}

// template <>
inline int calculate_translated_index(const char* value, const char* p) {
  if (!value) {
    throw std::invalid_argument(
        "calculate_translated_index can't take a nullptr");
  }
  int counter = 0;
  while (*value) {
    if (value == p) {
      return counter;
    }
    counter += (std::isprint(*value)) ? 1 : 4;
  }
  return counter;
}

template <typename string>
std::string quoted(string&& in) {
  std::ostringstream ret;
  ret << '"';
  for (char c : in) {
    if (c < ' ' || c >= '\x7F') {
      ret << escapify(c);
    } else if (c == '"') {
      ret << "\\\"";
    } else if (c == '\\') {
      ret << "\\\\";
    } else {
      ret << c;
    }
  }
  ret << '"';
  return ret.str();
}

}  // namespace kblib

#endif  // KBLIB_CONVERT_H
