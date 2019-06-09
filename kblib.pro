TEMPLATE = app
CONFIG += console c++17
CONFIG -= app_bundle
CONFIG -= qt

SOURCES += \
    main.cpp

HEADERS += \
    kblib.h \
    kblib_convert.h \
    kblib_build.h \
    kblib_stats.h \
    kblib_simple.h \
    kblib_fakestd.h \
    kblib_stringops.h \
    kblib_io.h \
    kblib_containers.h \
    kblib_format.h \
    kblib_icu.h \
    kblib_variant.h \
    kblib_traits.h \
    kblib_tdecl.h \
    kblib_logic.h \
    kblib_iterators.h \
    kblib_bits.h

QMAKE_CXXFLAGS += -stdlib=libc++ -std=c++17
QMAKE_CXXFLAGS += -Wall -Wextra -pedantic-errors -Wno-missing-braces -Wno-mismatched-tags
QMAKE_LFLAGS += -stdlib=libc++ -lc++ -lc++abi

QMAKE_CXXFLAGS += -fsanitize=address,undefined
QMAKE_LFLAGS += -fsanitize=address,undefined
