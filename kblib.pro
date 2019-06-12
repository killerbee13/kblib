TEMPLATE = app
CONFIG += console c++17
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    main.cpp \
    trie.cpp

HEADERS += \
    kblib.h \
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
    kblib/variant.h

QMAKE_CXXFLAGS += -stdlib=libc++ -std=c++17
QMAKE_CXXFLAGS += -Wall -Wextra -pedantic-errors -Wno-missing-braces -Wno-mismatched-tags
QMAKE_LFLAGS += -stdlib=libc++ -lc++ -lc++abi

QMAKE_CXXFLAGS += -fsanitize=address,undefined
QMAKE_LFLAGS += -fsanitize=address,undefined
