TEMPLATE = app
CONFIG += console c++17 testcase
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    main.cpp \
    build.cpp \
    containers.cpp \
    convert.cpp \
    fakestd.cpp \
    format.cpp \
    icu.cpp \
    iterators.cpp \
    logic.cpp \
    simple.cpp \
    stats.cpp \
    stringops.cpp \
    tdecl.cpp \
    traits.cpp \
    variant.cpp \
    bits.cpp \
    catch_main.cpp \
    algorithm.cpp \
    multi_span.cpp \
    invasive_containers.cpp \
    delayed_construct.cpp \
    memory.cpp \
    io.cpp \
    trie.cpp \
    direct_map.cpp \
    hash.cpp \
    sort.cpp \
    random.cpp \
    poly_obj.cpp

HEADERS += \
    kblib/bits.h \
    kblib/build.h \
    kblib/containers.h \
    kblib/convert.h \
    kblib/fakestd.h \
    kblib/format.h \
    kblib/icu.h \
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
    kblib/invasive_containers.h \
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
    LICENSE.txt \
    doc/table_ana.yml \
    doc/table_cata.yml \
    var_timings.log \
    medfile \
    Doxyfile \
    doc/algorithm_intuition_ana.html \
    doc/algorithm_intuition_cata.html \
    doc/algorithm_intuition.html \
    doc/Doxyfile \
    doc/algorithm_intuition_named_ops.html

VERSION = 0.2.1

QMAKE_CXXFLAGS += -std=c++17
QMAKE_CXXFLAGS += -march=native
QMAKE_CXXFLAGS += -Wall -Wextra -Wpointer-to-int-cast -Wpedantic -Wshadow
QMAKE_CXXFLAGS += -Wno-missing-braces
QMAKE_CXXFLAGS += -I/mnt/Vers1/include

#QMAKE_CXXFLAGS += -stdlib=libc++ -fstandalone-debug
QMAKE_CXXFLAGS += -Wconversion -Wno-deprecated-declarations \
 -Wold-style-cast -Wshadow -Wzero-as-null-pointer-constant \
 -Wreturn-std-move -Wno-mismatched-tags
#QMAKE_CXXFLAGS += -Wduplicated-cond -Wlogical-op
#QMAKE_LFLAGS += -stdlib=libc++ -lc++ -lc++abi -fuse-ld=lld -L/usr/local/lib

CONFIG(debug, debug|release)|CONFIG(sanitize) {
	QMAKE_CXXFLAGS += -fsanitize=address,undefined
	QMAKE_LFLAGS += -fsanitize=address,undefined
}

CONFIG(build_trace) {
	QMAKE_CXXFLAGS += -ftime-trace
}

#QMAKE_CXXFLAGS_RELEASE += -flto
#QMAKE_LFLAGS_RELEASE += -flto

medfile.target = medfile
medfile.commands = rm $$medfile.target; \
    ln -s /mnt/Vers1/assets/medfile $$medfile.target

QMAKE_EXTRA_TARGETS += medfile

PRE_TARGETDEPS += medfile
