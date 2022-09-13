#ifndef COMMONS_HPP
#define COMMONS_HPP
#include <cassert>
#include <memory>
#include <type_traits>
#endif

#ifdef TSL_DEBUG
#define tsl_sh_assert(expr) assert(expr)
#else
#define tsl_sh_assert(expr) (static_cast<void>(0))
#endif