#ifndef KBLIB_IO_H
#define KBLIB_IO_H

#include "fakestd.h"
#include "traits.h"

#include <fstream>
#include <functional>
#include <string>
#include <vector>

#if KBLIB_USE_CXX17
#include <cstdio>
#include <filesystem>
#include <optional>

#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || \
                         (defined(__APPLE__) && defined(__MACH__)))
/* UNIX-style OS. ------------------------------------------- */
#include <unistd.h>
#if defined(_POSIX_VERSION)
#define KBLIB_POSIX_TMPFILE
#endif
#endif
#endif

#include <iostream>

namespace kblib {

template <typename D = std::string,
          typename std::enable_if_t<is_contiguous_v<D>, int> = 0>
auto get_contents(std::istream& in, D& out) -> auto {
	in.seekg(0, std::ios::end);
	auto size = in.tellg();
	out.resize(size);
	in.seekg(0, std::ios::beg);
	in.read(reinterpret_cast<char*>(out.data()), size);
	return size;
}

template <typename D = std::string,
          typename std::enable_if_t<not is_contiguous_v<D>, int> = 0>
auto get_contents(std::istream& in, D& out) -> auto {
	in.seekg(0, std::ios::end);
	auto size = in.tellg();
	out.resize(size);
	in.seekg(0, std::ios::beg);
	std::copy((std::istreambuf_iterator<char>(in)),
	          std::istreambuf_iterator<char>(), out.begin());
	return size;
}

#if KBLIB_USE_CXX17
/**
 * @brief Read the entire contents of a file into a container, such as
 * std::string or std::vector<char>. Note that it will be most efficient to read
 * into contiguous containers, as opposed to non-contiguous containers.
 *
 * @param filename The filename to open.
 * @tparam D A contiguous sequence container, which will be created and filled
 * with the contents of the file to be read.
 * @return std::optional<D> The contents of the file, if reading was successful.
 */
template <typename D = std::string, typename string>
auto get_file_contents(const string& filename) -> std::optional<D> {
	static_assert(std::is_trivially_copyable_v<typename D::value_type>,
	              "D must be a sequence of trivial types");
	static_assert(sizeof(typename D::value_type) == 1,
	              "D must be a sequence of char-sized objects.");
	std::optional<D> out;
	if (std::ifstream in(filename, std::ios::in | std::ios::binary); in) {
		const auto fsize = get_contents(in, out.emplace());
		if (fsize != to_signed(out->size())) {
		}
	}
	return out;
}
#endif

/**
 * @brief Read the entire contents of a file into a container, such as
 * std::string or std::vector<char>. Note that it will be most efficient to read
 * into contiguous containers, as opposed to non-contiguous containers.
 *
 * @param filename The filename to open.
 * @tparam D A contiguous sequence container, which will be created and filled
 * with the contents of the file to be read.
 * @return std::optional<D> The contents of the file, if reading was successful.
 */
template <typename D = std::string, typename string>
auto try_get_file_contents(const string& filename) -> D {
	static_assert(std::is_trivially_copyable<typename D::value_type>::value,
	              "D must be a sequence of trivial types");
	static_assert(sizeof(typename D::value_type) == 1,
	              "D must be a sequence of char-sized objects.");
	D out;
	std::ifstream in(filename, std::ios::in | std::ios::binary);
	if (in) {
		in.exceptions(std::ios_base::failbit | std::ios_base::badbit);
		get_contents(in, out);
	} else {
		throw std::system_error(std::make_error_code(std::errc::io_error),
		                        "could not open file " + std::string(filename));
	}
	return out;
}

/**
 * @brief By-value std::getline wrapper.
 *
 * @param is The stream to extract from.
 * @return std::string A single line of text from the stream.
 */
inline auto getline(std::istream& is) -> std::string {
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
inline auto eat_word(std::istream& is) -> std::istream& {
	do {
		is.get();
	} while (is and not std::isspace(is.peek()));
	return is;
}

/**
 * @brief Eat spaces, don't eat an extra.
 *
 * @deprecated Use std::ws instead.
 *
 * @param is
 * @return std::istream
 */
[[deprecated("Use std::ws instead")]] inline std::istream&
eat_space(std::istream& is) {
	while (is and std::isspace(is.peek())) {
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
 * @brief Read in spaces until the end of the line is found.
 *
 * nl may be used to consume whitespace left over after a formatted
 * input operation before doing an unformatted input operation (such as
 * std::getline).
 *
 * @remark Example usage:
 * @code
 * int x{};
 * std::cout << "Enter a number: ";
 * std::cin >> x; // potentially leaves a new line in the stream
 * std::cout << "Got " << x << '\n';
 * std::string str;
 * std::cout << "Enter a line of text: ";
 * std::getline(std::cin >> kblib::nl, str);
 * std::cout << "Got " << std::quoted(str) << '\n';
 * @endcode
 *
 * @param is The stream to read from.
 * @return std::istream& is.
 */
template <typename CharT, typename Traits>
auto nl(std::basic_istream<CharT, Traits>& is)
    -> std::basic_istream<CharT, Traits>& {
	auto n = static_cast<typename Traits::int_type>(is.widen('\n'));
	for (typename Traits::int_type c = is.peek();
	     is and c != kblib::eof<CharT> and
	     std::isspace(static_cast<CharT>(c), is.getloc()) and c != n;
	     c = is.peek()) {
		is.ignore();
	}
	if (is.peek() == n) {
		is.ignore();
	}
	return is;
}

template <typename T, typename U>
struct unicode_widen : std::false_type {};

template <>
struct unicode_widen<char16_t, char32_t> : std::true_type {};

#if KBLIB_USE_CHAR8_T

template <>
struct unicode_widen<char8_t, char16_t> : std::true_type {};

template <>
struct unicode_widen<char8_t, char32_t> : std::true_type {};

#endif

#if KBLIB_CHAR_IS_UTF8

template <>
struct unicode_widen<char, char16_t> : std::true_type {};

template <>
struct unicode_widen<char, char32_t> : std::true_type {};

#endif

template <typename T, typename U>
constexpr static bool unicode_widen_v = unicode_widen<T, U>::value;

/**
 * @brief Read a character from an input stream only if it equals c. Acts as an
 * UnformattedInputOperation, that is, it will not ignore any leading
 * whitespace.
 */
template <typename CharT>
auto unformatted_expect(CharT c) -> auto {
	auto _f = [c](auto& istream) -> decltype(istream) {
		using SCharT = typename std::decay<decltype(istream)>::type::char_type;
#if KBLIB_USE_CHAR8_t
		static_assert(std::is_same_v<CharT, char_type> or
		                  (not std::is_same_v<CharT, char8_t> and
		                   not std::is_same_v<char_type, char8_t>),
		              "No support for char8_t conversions.");
#endif
		auto widen_equal = [&](SCharT d) {
			// Feasible:
			// T     -> T      : c == d
			// T     -> char   : istream.widen(c) == d
			// u32   -> u16    : c == d
			// Not currently feasible:
			// SCharT == char  : read multiple chars and convert to CharT?
			// ...             :	convert between different wide char types?

			static_assert(
			    (std::is_same<CharT, SCharT>::value or
			     std::is_same<CharT, char>::value),
			    "Stream character type incompatible with argument type.");
#if KBLIB_USE_CXX17
			if constexpr (std::is_same<CharT, SCharT>::value) {
#else
			if (std::is_same<CharT, SCharT>::value) {
#endif
				return c == d;
			} else if (unicode_widen_v<CharT, SCharT>) {
				return c == d;
			} else {
				return istream.widen(c) == d;
			}
		};

		if (widen_equal(istream.peek())) {
			void(istream.get());
		} else {
			istream.setstate(std::ios_base::failbit);
		}
		return istream;
	};
	return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Read a character from an input stream only if it equals c. Acts as a
 * FormattedInputOperation, that is, leading whitespace is ignored.
 */
template <typename CharT>
auto expect(CharT c) -> auto {
	auto _f = [c](auto& istream) -> decltype(istream) {
		return istream >> std::ws >> unformatted_expect(c);
	};
	return get_manip<decltype(_f)>{_f};
}

/**
 * @brief Read a whole line into a std::basic_string-like class template.
 *
 * When used like
 *  os >> get_line(str);
 * for a std::ostream& os and std::string str, reads a full line into str
 * instead of just a single word.
 *
 * @param str The string to read a line into.
 */
template <typename CharT, typename... O,
          template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str) -> auto {
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
 * for a std::ostream& os and std::string str, reads a full line into str
 * instead of just a single word.
 *
 * @param str The string to read into.
 * @param delim The delimiter at which to stop reading text.
 */
template <typename CharT, typename... O,
          template <typename, typename...> class string>
inline auto get_line(string<CharT, O...>& str, CharT delim) -> auto {
	auto _f = [&, delim](auto& istream) -> decltype(istream) {
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

namespace detail {

	/*
	class steambuf_template : public std::streambuf {
	protected:
	   auto imbue(const std::locale& loc) -> void override;

	   auto setbuf(char_type* s, std::streamsize n) -> std::streambuf* override;
	   auto seekoff(off_type off, std::ios_base::seekdir dir,
	std::ios_base::openmode which) -> pos_type override; auto seekpos(pos_type
	pos, std::ios_base::openmode which) -> pos_type override; auto sync() -> int
	override;

	   auto showmanyc() -> std::streamsize override;
	   auto underflow() -> int_type override;
	   auto uflow() -> int_type override;
	   auto xsgetn(char_type* s, std::streamsize count) -> std::streamsize
	override;

	   auto xsputn(const char_type* s, std::streamsize count) -> std::streamsize
	override; auto overflow(int_type ch) -> int_type override;

	   auto pbackfail(int_type c) -> int_type override;
	};
	 */

	template <typename SB1_t, typename SB2_t>
	class basic_teestreambuf
	    : public std::basic_streambuf<typename SB1_t::char_type,
	                                  typename SB1_t::traits_type> {
	 public:
		using base_type = std::basic_streambuf<typename SB1_t::char_type,
		                                       typename SB1_t::traits_type>;
		static_assert(std::is_same<typename SB1_t::char_type,
		                           typename SB2_t::char_type>::value,
		              "Backing streams must be compatible.");
		static_assert(std::is_same<typename SB1_t::traits_type,
		                           typename SB2_t::traits_type>::value,
		              "Backing streams must be compatible.");

		using typename base_type::char_type;
		using typename base_type::traits_type;

		using typename base_type::int_type;
		using typename base_type::off_type;
		using typename base_type::pos_type;

		basic_teestreambuf() = delete;
		basic_teestreambuf(SB1_t* a, SB2_t* b) : buffer(1024), a(a), b(b) {
			this->setp(buffer.data(), buffer.data() + buffer.size() - 1);
		}

	 private:
		auto flush() -> bool {
			std::streamsize count = this->pptr() - this->pbase();
			auto a_ct = a->sputn(this->pbase(), count);
			auto b_ct = b->sputn(this->pbase(), count);

			std::streamsize successful = std::min(a_ct, b_ct);

			if (successful == count) {
				this->pbump(-count);
				return true;
			} else {
				fail();
				return false;
			}
		}

		auto bool_to_failure(bool B) const noexcept -> int_type {
			return B ? traits_type::to_int_type(char_type{}) : traits_type::eof();
		}

		auto fail() noexcept -> void {
			this->setp(buffer.data(), buffer.data() + buffer.size() - 1);
			this->pbump(buffer.size() - 1);
			return;
		}

	 protected:
		auto imbue(const std::locale& loc) -> void override {
			a->pubimbue(loc);
			b->pubimbue(loc);
			return;
		}

		auto sync() -> int override { return a->pubsync() | b->pubsync(); }

		auto uflow() -> int_type override { return traits_type::eof(); }

		auto xsgetn(char_type*, std::streamsize) -> std::streamsize override {
			return 0;
		}

		std::streamsize xsputn(const char_type* s,
		                       std::streamsize count) override {
			bool success = flush();
			auto a_ct = a->sputn(s, count);
			auto b_ct = b->sputn(s, count);

			std::streamsize successful = success ? std::min(a_ct, b_ct) : 0;

			if (successful == count) {
				return count;
			} else {
				fail();
				return 0;
			}
		}

		auto overflow(int_type ch) -> int_type override {
			if (not traits_type::eq_int_type(ch, traits_type::eof())) {
				traits_type::assign(*this->pptr(), traits_type::to_char_type(ch));
				this->pbump(1);
			}
			return bool_to_failure(flush());
		}

	 private:
		std::vector<char_type> buffer;
		SB1_t* a;
		SB2_t* b;
	};

	template <typename Stream>
	using buf_for =
	    std::remove_pointer_t<decltype(std::declval<Stream&>().rdbuf())>;

} // namespace detail

template <typename StreamA, typename StreamB>
class basic_teestream
    : public std::basic_ostream<typename StreamA::char_type,
                                typename StreamA::traits_type> {
 private:
	using buf_type = detail::basic_teestreambuf<detail::buf_for<StreamA>,
	                                            detail::buf_for<StreamB>>;
	buf_type buf;
	using ostream_type = std::basic_ostream<typename StreamA::char_type,
	                                        typename StreamA::traits_type>;

 public:
	using typename ostream_type::char_type;
	using typename ostream_type::traits_type;

	using typename ostream_type::int_type;
	using typename ostream_type::off_type;
	using typename ostream_type::pos_type;

	basic_teestream(StreamA& a, StreamB& b)
	    : ostream_type(&buf), buf(a.rdbuf(), b.rdbuf()) {}

	auto rdbuf() const -> buf_type* { return &buf; }
};

#if 1 || KBLIB_USE_CXX17
template <typename StreamA, typename StreamB>
auto tee(StreamA& a, StreamB& b) -> basic_teestream<StreamA, StreamB> {
	return {a, b};
}
#endif

#if KBLIB_USE_CXX17

template <typename F, typename D = std::default_delete<F>,
          typename P = typename D::pointer>
struct file_deleter {
	std::filesystem::path path;
	using pointer = P;
	void operator()(P fs) {
		static_cast<D&>(this)(fs);
		std::filesystem::remove(path);
	}
};

#ifdef KBLIB_POSIX_TMPFILE
namespace detail {
	struct fd_closer {
		void operator()(int fd) const noexcept { close(fd); }
		using pointer = int;
	};
} // namespace detail

using fd_deleter = file_deleter<int, detail::fd_closer>;
#endif

template <typename File = std::fstream>
[[nodiscard]] auto
scoped_file(const std::filesystem::path& path,
            std::ios_base::openmode mode = std::ios_base::in |
                                           std::ios_base::out) {
	return std::unique_ptr<File, file_deleter<File>>{
	    new std::fstream{path, mode}, {path}};
}

template <typename File = std::fstream>
[[nodiscard]] auto tmpfile(const std::filesystem::path& path,
                           std::ios_base::openmode mode = std::ios_base::in |
                                                          std::ios_base::out) {
#ifdef KBLIB_POSIX_TMPFILE
	auto p = std::make_unique<File>(path, mode);
	std::filesystem::remove(path);
	return p;
#else
	return scoped_file<File>(path, mode);
#endif
}

#endif

} // namespace kblib

#endif // KBLIB_IO_H
