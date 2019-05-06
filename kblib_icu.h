#ifndef KBLIB_ICU_H
#define KBLIB_ICU_H

#include <unicode/unistr.h>
#include <ostream>
#include <string>
#include <typeinfo>

namespace kblib {

// For elegance
template <typename string = std::string>
string toUTF8(const icu::UnicodeString& s) {
  string res;
  return s.toUTF8String(res);
}
// For consistency
template <typename string>
icu::UnicodeString fromUTF8(string s) {
  return icu::UnicodeString::fromUTF8(s);
}
// For elegance
template <typename string = std::u32string>
string toUTF32(const icu::UnicodeString& s) {
  string res(s.countChar32(), '\0');
  UErrorCode ec{U_ZERO_ERROR};
  s.toUTF32(reinterpret_cast<UChar32*>(&res[0]), res.size(), ec);
  if (U_FAILURE(ec)) {
    throw ec;
  }
  return res;
}
// For consistency
template <typename string>
icu::UnicodeString fromUTF32(string s) {
  return icu::UnicodeString::fromUTF32(s.data(), s.length());
}

inline std::ostream& operator<<(std::ostream& os,
                                const icu::UnicodeString& str) {
  return os << toUTF8(str);
}

namespace icu_str_ops {
// ICU basically provides a + so I might as well translate it
inline std::string operator+(std::string lhs, const icu::UnicodeString& str) {
  return str.toUTF8String(lhs);
}

inline icu::UnicodeString operator+(icu::UnicodeString lhs,
                                    const std::string& rhs) {
  return lhs + fromUTF8(rhs);
}
}  // namespace icu_str_ops

template <typename T>
T fromStr(const icu::UnicodeString& val, const char* type = typeid(T).name()) {
  return fromStr<T>(toUTF8(val), type);
}

}  // namespace kblib

#endif  // KBLIB_ICU_H
