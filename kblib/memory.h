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

} // namespace kblib

#endif // MEMORY_H
