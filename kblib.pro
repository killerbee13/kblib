TEMPLATE = app
CONFIG += console c++17
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
    io.cpp

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
    kblib/memory.h

QMAKE_CXXFLAGS += -stdlib=libc++ -std=c++17
QMAKE_CXXFLAGS += -Wall -Wextra -pedantic-errors -Wno-missing-braces -Wno-mismatched-tags -Wreturn-std-move
QMAKE_CXXFLAGS += -I/mnt/Vers1/include
QMAKE_CXXFLAGS += -glldb
QMAKE_LFLAGS += -stdlib=libc++ -lc++ -lc++abi -fuse-ld=lld -L/usr/local/lib

QMAKE_CXXFLAGS += -fsanitize=address,undefined -fstandalone-debug
QMAKE_LFLAGS += -fsanitize=address,undefined

QMAKE_CXXFLAGS_RELEASE += -flto
QMAKE_LFLAGS_RELEASE += -flto
