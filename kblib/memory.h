#ifndef MEMORY_H
#define MEMORY_H

#include "algorithm.h"
#include "tdecl.h"

#include <utility>
#include <vector>

namespace kblib {

template <typename T, bool = std::is_class<T>::value>
struct null_construct {
	null_construct() : t{} {}

	T t;

	operator T&() { return t; }
	operator const T&() const { return t; }
};

template <typename T>
struct null_construct<T, true> : public T {
	null_construct() : T{} {}
};

template <auto T>
struct type_constant {
	operator decltype(T)() const noexcept { return T; }
};

template <auto FunPtr>
struct fun_ptr_deleter;

template <typename Arg, void (*FunPtr)(Arg)>
struct fun_ptr_deleter<FunPtr> {
	void operator()(Arg arg) const { return FunPtr(arg); }
};

template <typename T>
class live_ptr;

template <typename T>
class live_wrapper {
 public:
	// no constructors to make it an aggregate
	~live_wrapper() {
		for (auto p : _observers) {
			if (p) {
				*p = nullptr;
			}
		}
	}

	live_ptr<T> ref();
	live_ptr<const T> ref() const;
	live_ptr<const T> cref() const;

	T data;

	kblib::null_construct<std::vector<live_wrapper**>> _observers{};
};

template <typename T>
class live_wrapper<const T> : public live_wrapper<T> {};

template <typename T>
class live_ptr {
 public:
	live_ptr() = default;
	live_ptr(const live_ptr& o) : obj(o.obj) { add(); }
	live_ptr(live_ptr&& o) noexcept : obj(std::exchange(o.obj, nullptr)) {
		if (obj) {
			std::replace(obj->_observers.begin(), obj->_observers.end(), &o.obj,
			             &obj);
		}
	}
	explicit live_ptr(live_wrapper<T>& o) : obj(&o) { add(); }

	live_ptr& operator=(const live_ptr& o) {
		rem();
		obj = o.obj;
		add();
		return *this;
	}
	live_ptr& operator=(live_ptr&& o) noexcept {
		rem();
		if ((obj = std::exchange(o.obj, nullptr))) {
			std::replace(obj->_observers.begin(), obj->_observers.end(), &o.obj,
			             &obj);
		}
		return *this;
	}
	live_ptr& operator=(live_wrapper<T>& o) {
		rem();
		obj = &o;
		add();
		return *this;
	}

	T& operator*() noexcept { return obj->data; }
	const T& operator*() const noexcept { return obj->data; }

	T* operator->() noexcept { return &obj->data; }
	const T* operator->() const noexcept { return &obj->data; }

	operator bool() const noexcept { return obj; }

	friend bool operator==(const live_ptr& lhs, std::nullptr_t) { return !lhs; }
	friend bool operator==(std::nullptr_t, const live_ptr& rhs) { return !rhs; }

	friend bool operator==(const live_ptr& lhs, const live_ptr& rhs) {
		return lhs.obj == rhs.obj;
	}

	friend bool operator==(const live_ptr& lhs, const T* rhs) {
		if (!lhs && !rhs) {
			return true;
		} else {
			return lhs && &lhs.obj->data == rhs;
		}
	}
	friend bool operator==(const T* lhs, const live_ptr& rhs) {
		if (!rhs && !lhs) {
			return true;
		} else {
			return rhs && &rhs.obj->data == lhs;
		}
	}

 private:
	void add() { obj->_observers.push_back(&obj); }
	void rem() {
		if (obj) {
			erase(obj->_observers, &obj);
		}
	}

	mutable live_wrapper<T>* obj = nullptr;

	friend class live_ptr<const T>;
};

template <typename mT>
class live_ptr<const mT> {
 public:
	using T = const mT;
	live_ptr() = default;
	live_ptr(const live_ptr<T>& o) : obj(o.obj) { add(); }
	live_ptr(const live_ptr<mT>& o) : obj(o.obj) { add(); }
	live_ptr(live_ptr<T>&& o) : obj(o.pop()) { add(); }
	live_ptr(live_ptr<mT>&& o) : obj(o.pop()) { add(); }
	explicit live_ptr(live_wrapper<T>& o) : obj(&o) { add(); }
	explicit live_ptr(live_wrapper<mT>& o) : obj(&o) { add(); }

	live_ptr& operator=(const live_ptr& o) {
		rem();
		obj = o.obj;
		add();
		return *this;
	}
	live_ptr& operator=(live_ptr&& o) noexcept {
		rem();
		if ((obj = std::exchange(o.obj, nullptr))) {
			std::replace(obj->_observers.begin(), obj->_observers.end(), &o.obj,
			             &obj);
		}
		return *this;
	}
	live_ptr& operator=(live_wrapper<T>& o) {
		rem();
		obj = &o;
		add();
		return *this;
	}

	T& operator*() noexcept { return *obj->data; }
	const T& operator*() const noexcept { return *obj->data; }

	T* operator->() noexcept { return obj->data; }
	const T* operator->() const noexcept { return obj->data; }

	operator bool() const noexcept { return obj; }

	friend bool operator==(const live_ptr& lhs, std::nullptr_t) { return !lhs; }
	friend bool operator==(std::nullptr_t, const live_ptr& rhs) { return !rhs; }

	friend bool operator==(const live_ptr& lhs, const live_ptr& rhs) {
		return lhs.obj == rhs.obj;
	}

	friend bool operator==(const live_ptr& lhs, const T* rhs) {
		if (!lhs && !rhs) {
			return true;
		} else {
			return lhs && &lhs.obj->data == rhs;
		}
	}
	friend bool operator==(const T* lhs, const live_ptr& rhs) {
		if (!rhs && !lhs) {
			return true;
		} else {
			return rhs && &rhs.obj->data == lhs;
		}
	}

 private:
	void add() { obj->_observers.push_back(&obj); }
	void rem() {
		if (obj) {
			erase(obj->_observers, &obj);
		}
	}
	live_wrapper<mT>* pop() {
		rem();
		return std::exchange(obj, nullptr);
	}

	mutable live_wrapper<mT>* obj = nullptr;
};

template <typename T>
live_ptr<T> live_wrapper<T>::ref() {
	return live_ptr<T>{*this};
}

template <typename T>
live_ptr<const T> live_wrapper<T>::ref() const {
	return live_ptr<T>{*this};
}

template <typename T>
live_ptr<const T> live_wrapper<T>::cref() const {
	return live_ptr<T>{*this};
}

namespace detail {
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
	          bool = std::is_class<T>::value&& std::is_empty<T>::value &&
	                 !std::is_final<T>::value,
	          bool =
	              std::is_object<typename std::remove_reference<T>::type>::value>
	struct as_base_class;

	template <typename T>
	struct as_base_class<T, false, true> {
		T base_;
		T& base() noexcept { return base_; }
		const T& base() const noexcept { return base_; }
	};

	template <typename T>
	struct as_base_class<T, true, true> : T {
		T& base() noexcept { return *this; }
		const T& base() const noexcept { return *this; }
	};

	template <typename R, typename A, bool E>
	struct as_base_class<R (&)(A) noexcept(E), false, false> {
		using type = R(A) noexcept(E);
		type* base_;
		type& base() const noexcept { return *base_; }
	};

	template <typename T, bool B>
	struct as_base_class<T&, B, true> {
		std::reference_wrapper<T> base_;
		T& base() noexcept { return base_; }
		const T& base() const noexcept { return base_; }
	};
} // namespace detail

// cond_ptr: A pointer which can either uniquely own its referent, or which can
// be a non-owning reference. Note that custom deleter support is not present;
// however it will not implicitly strip a deleter from a unique_ptr.

template <typename T, typename Deleter = std::default_delete<T>>
class cond_ptr : private detail::as_base_class<Deleter> {
	using d_base = detail::as_base_class<Deleter>;

 public:
	using pointer = detail::filter_deleter_pointer_t<Deleter, T>;
	using element_type = T;
	using deleter_type = Deleter;

	static_assert(std::is_nothrow_invocable<Deleter&, pointer>::value,
	              "cond_ptr<T> requires that get_deleter not throw exceptions.");

	using unique = std::unique_ptr<T, Deleter>;

	cond_ptr() noexcept = default;
	cond_ptr(std::nullptr_t) noexcept {}

	explicit cond_ptr(T* p, bool owner = false,
	                  std::decay_t<Deleter> del = {}) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(owner) {}
	explicit cond_ptr(T* p, std::decay_t<Deleter> del) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(false) {}
	cond_ptr(unique&& p) noexcept
	    : d_base{p.get_deleter()}, ptr_(p.release()), owns_(ptr_) {}

	cond_ptr(const cond_ptr& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_), owns_(false) {}
	cond_ptr(cond_ptr&& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_),
	      owns_(std::exchange(other.owns_, false)) {}

	cond_ptr& operator=(const cond_ptr& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = false;
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(cond_ptr&& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = rhs.owns();
		static_cast<d_base&>(*this) = {std::move(rhs.get_deleter())};
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(unique&& rhs) {
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
	unique to_unique() && noexcept {
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
		if (owns_ && ptr_) {
			get_deleter()(ptr_);
		}
	}

	KBLIB_NODISCARD cond_ptr weak() const& noexcept {
		return cond_ptr{ptr_, false};
	}

	bool owns() const noexcept { return owns_; }
	KBLIB_NODISCARD T* release() & noexcept {
		owns_ = false;
		return std::exchange(ptr_, nullptr);
	}

	Deleter& get_deleter() noexcept { return this->d_base::base(); }

	const Deleter& get_deleter() const noexcept { return this->d_base::base(); }

	void reset(T* p = nullptr, bool owner = false,
	           std::decay_t<Deleter> del = {}) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = owner;
		get_deleter() = std::move(del);
	}
	void reset(T* p, std::decay_t<Deleter> del = {}) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = false;
		get_deleter() = std::move(del);
	}

	void swap(cond_ptr& other) {
		std::swap(ptr_, other.ptr_);
		std::swap(owns_, other.owns_);
		std::swap(get_deleter(), other.get_deleter());
	}

	KBLIB_NODISCARD T* get() & noexcept { return ptr_; }

	KBLIB_NODISCARD const T* get() const& noexcept { return ptr_; }

	explicit operator bool() const noexcept { return ptr_; }

	KBLIB_NODISCARD T& operator*() & noexcept { return *ptr_; }

	KBLIB_NODISCARD const T& operator*() const& noexcept { return *ptr_; }

	KBLIB_NODISCARD T* operator->() & noexcept { return ptr_; }

	KBLIB_NODISCARD const T* operator->() const& noexcept { return ptr_; }

	friend constexpr bool operator==(const cond_ptr& lhs, const cond_ptr& rhs) {
		return lhs.ptr_ == rhs.ptr_;
	}

	friend constexpr bool operator==(const unique& lhs, const cond_ptr& rhs) {
		return lhs.get() == rhs.ptr_;
	}

	friend constexpr bool operator==(const cond_ptr& lhs, const unique& rhs) {
		return lhs.ptr_ == rhs.get();
	}

 private:
	T* ptr_ = nullptr;
	bool owns_ = false;
};

template <typename T, typename Deleter>
class cond_ptr<T[], Deleter> : private detail::as_base_class<Deleter> {
	using d_base = detail::as_base_class<Deleter>;

 public:
	using pointer = detail::filter_deleter_pointer_t<Deleter, T>;
	using element_type = T;
	using deleter_type = Deleter;

	static_assert(std::is_nothrow_invocable<Deleter&, pointer>::value,
	              "cond_ptr<T> requires that get_deleter not throw exceptions.");

	using unique = std::unique_ptr<T[], Deleter>;

	cond_ptr() noexcept = default;
	cond_ptr(std::nullptr_t) noexcept {}

	explicit cond_ptr(T* p, bool owner = false,
	                  std::decay_t<Deleter> del = {}) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(owner) {}
	explicit cond_ptr(T* p, std::decay_t<Deleter> del) noexcept
	    : d_base{std::move(del)}, ptr_(p), owns_(false) {}
	cond_ptr(unique&& p) noexcept
	    : d_base{p.get_deleter()}, ptr_(p.release()), owns_(ptr_) {}

	cond_ptr(const cond_ptr& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_), owns_(false) {}
	cond_ptr(cond_ptr&& other) noexcept
	    : d_base{other.get_deleter()}, ptr_(other.ptr_),
	      owns_(std::exchange(other.owns_, false)) {}

	cond_ptr& operator=(const cond_ptr& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = false;
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(cond_ptr&& rhs) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		owns_ = rhs.owns();
		ptr_ = rhs.release();
		return *this;
	}
	cond_ptr& operator=(unique&& rhs) {
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
	unique to_unique() && noexcept {
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

	KBLIB_NODISCARD cond_ptr weak() const& noexcept {
		return cond_ptr{ptr_, false};
	}

	bool owns() const noexcept { return owns_; }
	KBLIB_NODISCARD T* release() & noexcept {
		owns_ = false;
		return std::exchange(ptr_, nullptr);
	}

	Deleter& get_deleter() noexcept { return *this; }

	const Deleter& get_deleter() const noexcept { return *this; }

	void reset(T* p = nullptr, bool owner = false,
	           std::decay_t<Deleter> del = {}) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = owner;
		get_deleter() = std::move(del);
	}
	void reset(T* p, std::decay_t<Deleter> del = {}) & noexcept {
		if (owns_) {
			get_deleter()(ptr_);
		}
		ptr_ = p;
		owns_ = false;
		get_deleter() = std::move(del);
	}

	void swap(cond_ptr& other) {
		std::swap(ptr_, other.ptr_);
		std::swap(owns_, other.owns_);
		std::swap(get_deleter(), other.get_deleter());
	}

	KBLIB_NODISCARD T* get() & noexcept { return ptr_; }

	KBLIB_NODISCARD const T* get() const& noexcept { return ptr_; }

	explicit operator bool() const noexcept { return ptr_; }

	KBLIB_NODISCARD T& operator[](std::size_t index) & noexcept {
		return ptr_[index];
	}

	KBLIB_NODISCARD const T& operator[](std::size_t index) const& noexcept {
		return ptr_[index];
	}

	friend constexpr bool operator==(const cond_ptr& lhs, const cond_ptr& rhs) {
		return lhs.ptr_ == rhs.ptr_;
	}

	friend constexpr bool operator==(const unique& lhs, const cond_ptr& rhs) {
		return lhs.get() == rhs.ptr_;
	}

	friend constexpr bool operator==(const cond_ptr& lhs, const unique& rhs) {
		return lhs.ptr_ == rhs.get();
	}

 private:
	T* ptr_ = nullptr;
	bool owns_ = false;
};

} // namespace kblib

#endif // MEMORY_H
