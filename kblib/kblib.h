#ifndef KBLIB_H
#define KBLIB_H

/**
 * @file kblib.h
 * @brief Includes most other headers in kblib.
 *
 * @note Provided as a simple upgrade path for the original single-header
 * version of the library, whose functions are split between the below files.
 */

/**
 * @mainpage
 * kblib is a general utility library for C++14 and C++17, intended to provide
 * performant high-level abstractions and more expressive ways to do simple
 * things.
 */

#include "kblib/bits.h"
#include "kblib/build.h"
#include "kblib/containers.h"
#include "kblib/convert.h"
#include "kblib/fakestd.h"
#include "kblib/format.h"
#include "kblib/io.h"
#include "kblib/logic.h"
#include "kblib/simple.h"
#include "kblib/stats.h"
#include "kblib/stringops.h"
#include "kblib/variant.h"

#endif // KBLIB_H
