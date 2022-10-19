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
 * @brief Provides utilities to enable safe and expressive memory management and
 * low-level memory manipulation.
 *
 * @author killerbee
 * @date 2019-2021
 * @copyright GNU General Public Licence v3.0
 */

#ifndef MEMORY_H
#define MEMORY_H

#include "algorithm.h"
#include "tdecl.h"

#include <utility>
#include <vector>

namespace kblib {

template <typename T, bool = std::is_class<T>::value>
struct null_construct {
	null_construct() noexcept(std::is_nothrow_default_constructible<T>::value)
	    : t{} {}

	T t;

	operator T&() noexcept { return t; }
	operator const T&() const noexcept { return t; }
};

template <typename T>
struct null_construct<T, true> : public T {
	null_construct() noexcept(std::is_nothrow_default_constructible<T>::value)
	    : T{} {}
};

#if __cpp_nontype_template_parameter_auto
template <auto FunPtr>
struct fun_ptr_deleter;

template <typename Arg, void (*FunPtr)(Arg)>
struct fun_ptr_deleter<FunPtr> {
	auto operator()(Arg arg) const -> void { return FunPtr(arg); }
};
#endif

namespace detail_memory {

	template <typename D, typename T, typename V = void>
	struct filter_deleter_pointer {
		using type = T*;
	};

	template <typename D, typename T>
	struct filter_deleter_pointer<D, T, void_t<typename D::pointer>> {
		using type = typename std::remove_reference<D>::type::pointer;
	};

	template <typename D, typename T>
	using filter_deleter_pointer_t = typename filter_deleter_pointer<D, T>::type;

	template <typename T,
	          bool = std::is_class<T>::value and not std::is_final<T>::value,
	          bool
	          = std::is_object<typename std::remove_reference<T>::type>::value>
	struct as_base_class;

	template <typename T>
	struct as_base_class<T, false, true> {
		T base_;
		auto base() noexcept -> T& { return base_; }
		auto base() const noexcept -> const T& { return base_; }
		explicit operator T&() noexcept { return base(); }
		explicit operator const T&() const noexcept { return base(); }
	};

	template <typename T>
	struct as_base_class<T, true, true> : T {
		auto base() noexcept -> T& { return *this; }
		auto base() const noexcept -> const T& { return *this; }
	};

#if KBLIB_USE_CXX17
	template <typename R, typename A, bool E>
	struct as_base_class<R (&)(A) noexcept(E), false, false> {
		using type = R(A) noexcept(E);
		type* base_;
		auto base() const noexcept -> type& { return *base_; }
		explicit operator type&() const noexcept { return base(); }
	};
#else
	template <typename R, typename A>
	struct as_base_class<R (&)(A), false, false> {
		using type = R(A);
		type* base_;
		auto base() const noexcept -> type& { return *base_; }
		explicit operator type&() const noexcept { return base(); }
	};
#endif

	template <typename T>
	struct as_base_class<T&, false, true> {
		std::reference_wrapper<T> base_;

		auto base() noexcept -> T& { return base_; }
		auto base() const noexcept -> const T& { return base_; }

		explicit operator T&() noexcept { return base(); }
		explicit operator const T&() const noexcept { return base(); }
	};

	struct noop_t {
		auto operator()() const noexcept -> void {}
	};
	struct value_init {};

	template <typename T, typename Construct = noop_t, typename Destroy = noop_t>
	struct rule_zero : as_base_class<T> {
		template <typename... Args>
		rule_zero(Args&&... args)
		    : as_base_class<T>(std::forward<Args>(args)...) {}
	};

	template <typename T, typename D>
	struct on_destroy
	    : as_base_class<T>
	    , as_base_class<D> {
		on_destroy() noexcept = default;
		on_destroy(const on_destroy&) noexcept(
		    std::is_nothrow_copy_constructible<T>::value and
		        std::is_nothrow_copy_constructible<D>::value)
		    = default;
		on_destroy(on_destroy&&) noexcept(
		    std::is_nothrow_move_constructible<T>::value and
		        std::is_nothrow_move_constructible<D>::value)
		    = default;
		on_destroy& operator=(const on_destroy&) noexcept(
		    std::is_nothrow_copy_assignable<T>::value and
		        std::is_nothrow_copy_assignable<D>::value)
		    = default;
		on_destroy& operator=(on_destroy&&) noexcept(
		    std::is_nothrow_move_assignable<T>::value and
		        std::is_nothrow_move_assignable<D>::value)
		    = default;

		using as_base_class<T>::base;
		operator T&() noexcept { return base(); }
		operator const T&() const noexcept { return base(); }

		~on_destroy() {
			(invoke)(static_cast<D&&>(*this), static_cast<T&&>(*this));
		}
	};

} // namespace detail_memory

template <typename T>
class live_ptr;

template <typename T>
class live_wrapper {
 public:
	live_ptr<T> ref();
	live_ptr<const T> ref() const;
	live_ptr<const T> cref() const;

	T data;

	struct _destroy {
		auto operator()(std::vector<live_wrapper**>&& self) const noexcept
		    -> void {
			for (auto p : self) {
				if (p) {
					*p = nullptr;
				}
			}
		}
	};

	null_construct<
	    detail_memory::on_destroy<std::vector<live_wrapper**>, _destroy>>
	    _observers{};
};

template <typename T>
class live_wrapper<const T> : public live_wrapper<T> {};

namespace detail_memory {

	template <typename T>
	struct template_param;
	template <template <typename T> class C, typename T>
	struct template_param<C<T>> {
		using type = T;
	};

	template <typename D>
	struct live_ptr_base {
	 private:
		using T = typename template_param<D>::type;
		using mT = typename std::remove_const<T>::type;

	 public:
		auto operator*() noexcept -> T& { return obj->data; }
		auto operator*() const noexcept -> const T& { return obj->data; }

		auto operator->() noexcept -> T* { return &obj->data; }
		auto operator->() const noexcept -> const T* { return &obj->data; }

		operator bool() const noexcept { return obj; }

		friend auto operator==(const D& lhs, std::nullptr_t) -> bool {
			return not lhs;
		}
		friend auto operator==(std::nullptr_t, const D& rhs) -> bool {
			return not rhs;
		}

		friend auto operator==(const D& lhs, const D& rhs) -> bool {
			return lhs.obj == rhs.obj;
		}

		friend auto operator==(const D& lhs, const T* rhs) -> bool {
			if (not lhs and not rhs) {
				return true;
			} else {
				return lhs and &lhs.obj->data == rhs;
			}
		}
		friend auto operator==(const T* lhs, const D& rhs) -> bool {
			if (not rhs and not lhs) {
				return true;
			} else {
				return rhs and &rhs.obj->data == lhs;
			}
		}

		live_ptr_base() noexcept = default;
		live_ptr_base(live_wrapper<mT>* p)
		    : obj{p} {
			add();
		}
		auto operator=(const live_ptr_base& o) noexcept -> live_ptr_base& {
			rem();
			obj = o.obj;
			add();
			return *this;
		}
		auto operator=(live_ptr_base&& o) noexcept -> live_ptr_base& {
			rem();
			move(o);
			return *this;
		}

		live_ptr_base(const live_ptr_base& o)
		    : obj{o.obj} {
			add();
		}
		live_ptr_base(live_ptr_base&& o) noexcept { move(o.as_D()); }
		~live_ptr_base() { rem(); }

		auto operator=(const D& o) -> D& {
			rem();
			obj = o.obj;
			add();
			return as_D();
		}
		auto operator=(D&& o) noexcept -> D& {
			this->rem();
			this->move(o);
			return as_D();
		}

	 protected:
		auto add() -> void { obj->_observers.base().push_back(&obj); }
		auto rem() -> void {
			if (obj) {
				erase(obj->_observers.base(), &obj);
			}
		}
		auto move(D& o) -> void {
			if ((obj = std::exchange(o.obj, nullptr))) {
				std::replace(obj->_observers.base().begin(),
				             obj->_observers.base().end(), &o.obj, &obj);
			}
		}
		mutable live_wrapper<mT>* obj = nullptr;

	 private:
		auto as_D() noexcept -> D& { return static_cast<D&>(*this); }
		auto as_D() const noexcept -> const D& {
			return static_cast<const D&>(*this);
		}
	};
} // namespace detail_memory

template <typename T>
class live_ptr : public detail_memory::live_ptr_base<live_ptr<T>> {
	using base = detail_memory::live_ptr_base<live_ptr<T>>;

 public:
	using value_type = T;

	live_ptr() = default;
	live_ptr(const live_ptr& o) = default;
	live_ptr(live_ptr&& o) noexcept = default;
	auto operator=(const live_ptr& o) -> live_ptr& = default;
	auto operator=(live_ptr&& o) noexcept -> live_ptr& = default;

	explicit live_ptr(live_wrapper<T>& o)
	    : base{&o} {
		this->add();
	}
	auto operator=(live_wrapper<T>& o) -> live_ptr& {
		this->rem();
		this->obj = &o;
		this->add();
		return *this;
	}

	~live_ptr() = default;

 private:
	friend class live_ptr<const T>;
};

template <typename mT>
class live_ptr<const mT>
    : public detail_memory::live_ptr_base<live_ptr<const mT>> {
	using base = detail_memory::live_ptr_base<live_ptr<const mT>>;

 public:
	using T = const mT;
	using value_type = T;
	live_ptr() = default;
	live_ptr(const live_ptr<T>& o) = default;
	live_ptr(const live_ptr<mT>& o)
	    : base{o.obj} {}
	live_ptr(live_ptr<T>&& o) noexcept = default;
	live_ptr(live_ptr<mT>&& o) noexcept { this->move(o); }
	auto operator=(const live_ptr<T>& o) -> live_ptr& = default;
	auto operator=(live_ptr<T>&& o) noexcept -> live_ptr& = default;

	auto operator=(const live_ptr<mT>& o) -> live_ptr& {
		this->obj = o.obj;
		this->add();
		return *this;
	}
	auto operator=(live_ptr<mT>&& o) noexcept -> live_ptr& {
		this->obj = o.obj;
		this->add();
		return *this;
	}

	explicit live_ptr(const live_wrapper<mT>& o)
	    : base{&o} {
		this->add();
	}
	auto operator=(const live_wrapper<mT>& o) -> live_ptr& {
		this->rem();
		this->obj = &o;
		this->add();
		return *this;
	}

	~live_ptr() = default;
};

template <typename T>
live_ptr<T> live_wrapper<T>::ref() {
	return live_ptr<T>{*this};
}

template <typename T>
live_ptr<const T> live_wrapper<T>::ref() const {
	return live_ptr<const T>{*this};
}

template <typename T>
live_ptr<const T> live_wrapper<T>::cref() const {
	return live_ptr<const T>{*this};
}

// cond_ptr: A pointer which can either uniquely own its referent, or which can
// be a non-owning reference. Note that custom deleter support is not present;
// however it will not implicitly strip a deleter from a unique_ptr.

template <typename T, typename Deleter = std::default_delete<T>>
class cond_ptr : private detail_memory::as_base_class<Deleter> {
	using d_base = detail_memory::as_base_class<Deleter>;

 public:
	using pointer = detail_memory::filter_deleter_pointer_t<Deleter, T>;
	using element_type = T;
	using deleter_type = Deleter;

#if 0
	static_assert(std::is_nothrow_invocable<Deleter&, pointer>::value,
	              "cond_ptr<T> requires that get_deleter not throw exceptions.");
#endif

	using unique = std::unique_ptr<T, Deleter>;

	cond_ptr() noexcept = default;
	cond_ptr(std::nullptr_t) noexcept {}

	explicit cond_ptr(T* p, bool owner = false,
	                  std::decay_t<Deleter> del = {}) noexcept
	    : d_base{std::move(del)}
	    , ptr_(p)
	    , owns_(owner) {}
	explicit cond_ptr(T* p, std::decay_t<Deleter> del) noexcept
	    : d_base{std::move(del)}
	    , ptr_(p) {}

	cond_ptr(unique&& p) noexcept
	    : d_base{p.get_deleter()}
	    , ptr_(p.release())
	    , owns_(ptr_) {}

	cond_ptr(const cond_ptr& other) = delete;
	//	cond_ptr(const cond_ptr& other) noexcept
	//	    : d_base{other.get_deleter()}, ptr_(other.ptr_) {}
	cond_ptr(cond_ptr&& other) noexcept
	    : d_base{other.get_deleter()}
	    , ptr_(other.ptr_)
	    , owns_(std::exchange(other.owns_, false)) {}

	KBLIB_NODISCARD static auto adopt(T* p) noexcept -> cond_ptr {
		return {p, true};
	}
	KBLIB_NODISCARD static auto adopt(T* p, deleter_type del) noexcept
	    -> cond_ptr {
		return {p, true, del};
	}

	auto operator=(const cond_ptr& rhs) & -> cond_ptr& = delete;
	//	 auto operator=(const cond_ptr& rhs) & noexcept -> cond_ptr& {
	//		if (owns_) {
	//			get_deleter()(ptr_);
	//		}
	//		owns_ = false;
	//		ptr_ = rhs.release();
	//		return *this;
	//	}
	auto operator=(cond_ptr&& rhs) & noexcept -> cond_ptr& {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = rhs.owns();
		static_cast<d_base&>(*this) = {std::move(rhs.get_deleter())};
		ptr_ = rhs.release();
		return *this;
	}
	auto operator=(unique&& rhs) -> cond_ptr& {
		static_cast<d_base&>(*this) = {std::move(rhs.get_deleter())};
		ptr_ = rhs.release();
		owns_ = bool(ptr_);
		return *this;
	}

	/**
	 * @brief Transfers ownership to a unique_ptr if possible. If *this is not
	 * owning, returns nullptr.
	 *
	 * @post If ownership was transferred, *this is null. Otherwise, does
	 * nothing.
	 *
	 * @return std::unique_ptr<T, Deleter> Either a pointer which owns what *this
	 * owned, or a null pointer.
	 */
	KBLIB_NODISCARD auto to_unique() && noexcept -> unique {
		if (owns_) {
			return {release(), std::move(get_deleter())};
		} else {
			return {nullptr, get_deleter()};
		}
	}

	explicit operator unique() && noexcept {
		return std::move(*this).to_unique();
	}

	~cond_ptr() noexcept {
		if (owns_ and ptr_) {
			get_deleter()(ptr_);
		}
	}

	KBLIB_NODISCARD auto weak() const& noexcept -> cond_ptr {
		return cond_ptr{ptr_, false};
	}

	KBLIB_NODISCARD auto owns() const noexcept -> bool { return owns_; }
	KBLIB_NODISCARD auto release() & noexcept -> T* {
		owns_ = false;
		return std::exchange(ptr_, nullptr);
	}

	KBLIB_NODISCARD auto get_deleter() noexcept -> Deleter& {
		return this->d_base::base();
	}

	KBLIB_NODISCARD auto get_deleter() const noexcept -> const Deleter& {
		return this->d_base::base();
	}

	auto reset(T* p = nullptr, bool owner = false,
	           std::decay_t<Deleter> del = {}) & noexcept -> void {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = owner;
		get_deleter() = std::move(del);
	}
	auto reset(T* p, std::decay_t<Deleter> del = {}) & noexcept -> void {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = false;
		get_deleter() = std::move(del);
	}

	auto swap(cond_ptr& other) noexcept(
	    fakestd::is_nothrow_swappable<Deleter>::value) -> void {
		using std::swap;
		swap(ptr_, other.ptr_);
		swap(owns_, other.owns_);
		swap(get_deleter(), other.get_deleter());
	}

	KBLIB_NODISCARD auto get() & noexcept -> T* { return ptr_; }

	KBLIB_NODISCARD auto get() const& noexcept -> const T* { return ptr_; }

	KBLIB_NODISCARD explicit operator bool() const noexcept { return ptr_; }

	KBLIB_NODISCARD auto operator*() & noexcept -> T& { return *ptr_; }

	KBLIB_NODISCARD auto operator*() const& noexcept -> const T& {
		return *ptr_;
	}

	KBLIB_NODISCARD auto operator->() & noexcept -> T* { return ptr_; }

	KBLIB_NODISCARD auto operator->() const& noexcept -> const T* {
		return ptr_;
	}

	KBLIB_NODISCARD friend constexpr auto operator==(
	    const cond_ptr& lhs, const cond_ptr& rhs) noexcept -> bool {
		return lhs.ptr_ == rhs.ptr_;
	}

	KBLIB_NODISCARD friend constexpr auto operator==(
	    const unique& lhs, const cond_ptr& rhs) noexcept -> bool {
		return lhs.get() == rhs.ptr_;
	}

	KBLIB_NODISCARD friend constexpr auto operator==(const cond_ptr& lhs,
	                                                 const unique& rhs) noexcept
	    -> bool {
		return lhs.ptr_ == rhs.get();
	}

 private:
	T* ptr_ = nullptr;
	bool owns_ = false;
};

template <typename T, typename Deleter>
class cond_ptr<T[], Deleter> : private detail_memory::as_base_class<Deleter> {
	using d_base = detail_memory::as_base_class<Deleter>;

 public:
	using pointer = detail_memory::filter_deleter_pointer_t<Deleter, T>;
	using element_type = T;
	using deleter_type = Deleter;

#if 0
	static_assert(std::is_nothrow_invocable<Deleter&, pointer>::value,
	              "cond_ptr<T[]> requires that deleter not throw exceptions.");
#endif

	using unique = std::unique_ptr<T[], Deleter>;

	cond_ptr() noexcept = default;
	cond_ptr(std::nullptr_t) noexcept {}

	explicit cond_ptr(T* p, bool owner = false,
	                  std::decay_t<Deleter> del = {}) noexcept
	    : d_base{std::move(del)}
	    , ptr_(p)
	    , owns_(owner) {}
	explicit cond_ptr(T* p, std::decay_t<Deleter> del) noexcept
	    : d_base{std::move(del)}
	    , ptr_(p) {}

	cond_ptr(unique&& p) noexcept
	    : d_base{p.get_deleter()}
	    , ptr_(p.release())
	    , owns_(ptr_) {}

	cond_ptr(const cond_ptr& other) = delete;
	//	cond_ptr(const cond_ptr& other) noexcept
	//	    : d_base{other.get_deleter()}, ptr_(other.ptr_) {}
	cond_ptr(cond_ptr&& other) noexcept
	    : d_base{other.get_deleter()}
	    , ptr_(other.ptr_)
	    , owns_(std::exchange(other.owns_, false)) {}

	KBLIB_NODISCARD static auto adopt(T* p) noexcept -> cond_ptr {
		return {p, true};
	}
	KBLIB_NODISCARD static auto adopt(T* p, deleter_type del) noexcept
	    -> cond_ptr {
		return {p, true, del};
	}

	auto operator=(const cond_ptr& rhs) & -> cond_ptr& = delete;
	//	 auto operator=(const cond_ptr& rhs) & noexcept -> cond_ptr& {
	//		if (owns_) {
	//			get_deleter()(ptr_);
	//		}
	//		owns_ = false;
	//		ptr_ = rhs.release();
	//		return *this;
	//	}
	auto operator=(cond_ptr&& rhs) & noexcept -> cond_ptr& {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = rhs.owns();
		ptr_ = rhs.release();
		return *this;
	}
	auto operator=(unique&& rhs) -> cond_ptr& {
		ptr_ = rhs.release();
		owns_ = bool(ptr_);
		get_deleter() = std::move(rhs.get_deleter());
		return *this;
	}

	/**
	 * @brief Transfers ownership to a unique_ptr if possible. If *this is not
	 * owning, returns nullptr.
	 *
	 * @post If ownership was transferred, *this is null. Otherwise, does
	 * nothing.
	 *
	 * @return std::unique_ptr<T, Deleter> Either a pointer which owns what *this
	 * owned, or a null pointer.
	 */
	KBLIB_NODISCARD auto to_unique() && noexcept -> unique {
		if (owns_) {
			return {release(), get_deleter()};
		} else {
			return nullptr;
		}
	}

	explicit operator unique() && noexcept {
		return std::move(*this).to_unique();
	}

	~cond_ptr() noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
	}

	KBLIB_NODISCARD auto weak() const& noexcept -> cond_ptr {
		return cond_ptr{ptr_, false};
	}

	KBLIB_NODISCARD auto owns() const noexcept -> bool { return owns_; }
	KBLIB_NODISCARD auto release() & noexcept -> T* {
		owns_ = false;
		return std::exchange(ptr_, nullptr);
	}

	KBLIB_NODISCARD auto get_deleter() noexcept -> Deleter& { return *this; }

	KBLIB_NODISCARD auto get_deleter() const noexcept -> const Deleter& {
		return *this;
	}

	auto reset(T* p = nullptr, bool owner = false,
	           std::decay_t<Deleter> del = {}) & noexcept -> void {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = owner;
		get_deleter() = std::move(del);
	}
	auto reset(T* p, std::decay_t<Deleter> del = {}) & noexcept -> void {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = false;
		get_deleter() = std::move(del);
	}

	auto swap(cond_ptr& other) -> void {
		std::swap(ptr_, other.ptr_);
		std::swap(owns_, other.owns_);
		std::swap(get_deleter(), other.get_deleter());
	}

	KBLIB_NODISCARD auto get() & noexcept -> T* { return ptr_; }

	KBLIB_NODISCARD auto get() const& noexcept -> const T* { return ptr_; }

	explicit operator bool() const noexcept { return ptr_; }

	KBLIB_NODISCARD T& operator[](std::size_t index) & noexcept {
		return ptr_[index];
	}

	KBLIB_NODISCARD const T& operator[](std::size_t index) const& noexcept {
		return ptr_[index];
	}

	KBLIB_NODISCARD friend constexpr auto operator==(
	    const cond_ptr& lhs, const cond_ptr& rhs) noexcept -> bool {
		return lhs.ptr_ == rhs.ptr_;
	}

	KBLIB_NODISCARD friend constexpr auto operator==(
	    const unique& lhs, const cond_ptr& rhs) noexcept -> bool {
		return lhs.get() == rhs.ptr_;
	}

	KBLIB_NODISCARD friend constexpr auto operator==(const cond_ptr& lhs,
	                                                 const unique& rhs) noexcept
	    -> bool {
		return lhs.ptr_ == rhs.get();
	}

 private:
	T* ptr_ = nullptr;
	bool owns_ = false;
};

template <typename T, typename Deleter>
KBLIB_NODISCARD auto make_cond_ptr(std::unique_ptr<T, Deleter>&& arg) noexcept
    -> cond_ptr<T, Deleter> {
	return cond_ptr<T>(std::move(arg));
}

template <typename T>
KBLIB_NODISCARD auto make_cond_ptr(T* arg, bool owner = false) noexcept
    -> cond_ptr<T> {
	return cond_ptr<T>(arg, owner);
}

template <typename T, typename Deleter>
KBLIB_NODISCARD auto make_cond_ptr(T* arg, Deleter del) noexcept
    -> cond_ptr<T, Deleter> {
	return cond_ptr<T, Deleter>(arg, del);
}

template <typename T, typename Deleter>
KBLIB_NODISCARD auto make_cond_ptr(T* arg, bool owner, Deleter del) noexcept
    -> cond_ptr<T, Deleter> {
	return cond_ptr<T, Deleter>(arg, owner, del);
}

} // namespace kblib

#endif // MEMORY_H
