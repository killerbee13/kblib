/* *****************************************************************************
 * kblib is a general utility library for C++14 and C++17, intended to provide
 * performant high-level abstractions and more expressive ways to do simple
 * things.
 *
 * Copyright (c) 2021 killerbee
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 * ****************************************************************************/

/**
 * @file
 * Provides some basic interfaces to make using ICU smoother.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef KBLIB_ICU_H
#define KBLIB_ICU_H

#include "tdecl.h"

#include <ostream>
#include <string>
#include <typeinfo>
#include <unicode/unistr.h>

namespace kblib {

/**
 * @brief Convert a UnicodeString to a UTF-8 string.
 *
 * This functionality is present in ICU, however the interface is inelegant.
 *
 * @param s The string to convert.
 * @return string The re-encoded result.
 */
template <typename string = std::string>
auto toUTF8(const icu::UnicodeString& s) -> string {
	string res;
	return s.toUTF8String(res);
}

/**
 * @brief Convert a UTF-8 string into a UnicodeString.
 *
 * This functionality is present in ICU, and is only provided here for
 * consistency with the above.
 *
 * @param s The string to convert.
 * @return icu::UnicodeString The re-encoded result.
 */
template <typename string>
auto fromUTF8(string s) -> icu::UnicodeString {
	return icu::UnicodeString::fromUTF8(s);
}

/**
 * @brief Converts a UnicodeString to UTF-32.
 *
 * @param s The string to convert.
 * @return string The re-encoded result.
 */
template <typename string = std::u32string>
auto toUTF32(const icu::UnicodeString& s) -> string {
	string res(s.countChar32(), '\0');
	UErrorCode ec{U_ZERO_ERROR};
	s.toUTF32(&res[0], res.size(), ec);
	if (U_FAILURE(ec)) {
		// silence warnings about ec not being a temporary
		throw UErrorCode{ec};
	}
	return res;
}

/**
 * @brief Converts a UTF-32 string into a UnicodeString.
 *
 * This functionality is present in ICU, and is only provided here for
 * consistency with the above.
 *
 * @param s The string to convert.
 * @return icu::UnicodeString The re-encoded result.
 */
template <typename string>
auto fromUTF32(string s) -> icu::UnicodeString {
	return icu::UnicodeString::fromUTF32(s.data(), s.length());
}

/**
 * @warning Defining operators for external types is error-prone and may
 * unexpectedly break in the future.
 */
namespace icu_str_ops {

	/**
	 * @brief Provides a transcoding stream insertion operator for
	 * UnicodeStrings.
	 *
	 * @param os The stream to insert to.
	 * @param str The string to output.
	 * @return std::ostream& A reference to os.
	 */
	inline auto operator<<(std::ostream& os, const icu::UnicodeString& str)
	    -> std::ostream& {
		return os << toUTF8(str);
	}

	/**
	 * @brief Give the strange ICU interface for concatenating UTF-8 and
	 * UnicodeStrings a more idiomatic name in the form of operator+.
	 */
	inline auto operator+(std::string lhs, const icu::UnicodeString& str)
	    -> std::string {
		return str.toUTF8String(lhs);
	}

	/**
	 * @brief
	 */
	inline auto operator+(icu::UnicodeString lhs, const std::string& rhs)
	    -> icu::UnicodeString {
		return lhs += fromUTF8(rhs);
	}
} // namespace icu_str_ops

/**
 * @brief Reencodes val to UTF-8 and then converts it to T using the primary
 * overload.
 *
 * @param val A string holding data to convert.
 * @param type A type name to be used in error messages.
 * @return T The converted value.
 */
template <typename T>
auto fromStr(const icu::UnicodeString& val, const char* type = typeid(T).name())
    -> T {
	return fromStr<T>(toUTF8(val), type);
}

} // namespace kblib

#endif // KBLIB_ICU_H
