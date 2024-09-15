TEMPLATE = app
CONFIG += console testcase
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    tests/intrusive_containers.cpp \
    tests/main.cpp \
    tests/build.cpp \
    tests/containers.cpp \
    tests/convert.cpp \
    tests/fakestd.cpp \
    tests/format.cpp \
    tests/icu.cpp \
    tests/iterators.cpp \
    tests/logic.cpp \
    tests/simple.cpp \
    tests/stats.cpp \
    tests/stringops.cpp \
    tests/tdecl.cpp \
    tests/traits.cpp \
    tests/variant.cpp \
    tests/bits.cpp \
    tests/catch_main.cpp \
    tests/algorithm.cpp \
    tests/multi_span.cpp \
    tests/delayed_construct.cpp \
    tests/memory.cpp \
    tests/io.cpp \
    tests/trie.cpp \
    tests/direct_map.cpp \
    tests/hash.cpp \
    tests/sort.cpp \
    tests/random.cpp \
    tests/poly_obj.cpp \
    tests/visitation_benchmarks.cpp

HEADERS += \
    kblib/bits.h \
    kblib/build.h \
    kblib/containers.h \
    kblib/convert.h \
    kblib/fakestd.h \
    kblib/format.h \
    kblib/icu.h \
    kblib/intrusive_containers.h \
    kblib/io.h \
    kblib/iterators.h \
    kblib/logic.h \
    kblib/simple.h \
    kblib/stats.h \
    kblib/stringops.h \
    kblib/tdecl.h \
    kblib/traits.h \
    kblib/variant.h \
    kblib/kblib.h \
    kblib/algorithm.h \
    kblib/multi_span.h \
    kblib/delayed_construct.h \
    kblib/memory.h \
    kblib/trie.h \
    kblib/direct_map.h \
    kblib/hash.h \
    kblib/sort.h \
    kblib/random.h \
    kblib/poly_obj.h \
    kblib/enumerate-contrib-cry.h \
    kblib/enumerate-contrib-tw.h

DISTFILES += \
    .clang-format \
    LICENSE.txt \
    README.md \
    doc/table_ana.yml \
    doc/table_cata.yml \
    var_timings.log \
    Doxyfile \
    doc/algorithm_intuition_ana.html \
    doc/algorithm_intuition_cata.html \
    doc/algorithm_intuition.html \
    doc/Doxyfile \
    doc/algorithm_intuition_named_ops.html

VERSION = 0.4.02

QMAKE_CXXFLAGS += -std=c++23 -g
QMAKE_CXXFLAGS += -march=native

QMAKE_LFLAGS += -v

#QMAKE_CXXFLAGS += -stdlib=libc++ -fstandalone-debug
QMAKE_CXXFLAGS += -Wall -Wextra -Wpedantic
QMAKE_CXXFLAGS += -Wshadow # clang
#QMAKE_CXXFLAGS += -Wshadow=compatible-local # gcc
QMAKE_CXXFLAGS += -Wno-missing-braces
QMAKE_CXXFLAGS += -Wconversion -Wno-deprecated-declarations \
 -Wold-style-cast -Wzero-as-null-pointer-constant \
 -Wno-mismatched-tags -Wimplicit-fallthrough -fmax-errors=500
QMAKE_CXXFLAGS += -Wno-pragmas
#QMAKE_CXXFLAGS += -Wduplicated-cond -Wlogical-op -Wreturn-std-move -Wpointer-to-int-cast
#QMAKE_LFLAGS += -stdlib=libc++ -lc++ -lc++abi -fuse-ld=lld -L/usr/local/lib

CONFIG(debug, debug|release)|CONFIG(sanitize) {
	QMAKE_CXXFLAGS += -fsanitize=address,undefined
	QMAKE_LFLAGS += -fsanitize=address,undefined
	DEFINES += "SANITIZERS=\\\"address,undefined\\\""
}

CONFIG(build_trace) {
	QMAKE_CXXFLAGS += -ftime-trace
}

#QMAKE_CXXFLAGS_RELEASE += -flto
#QMAKE_LFLAGS_RELEASE += -flto
