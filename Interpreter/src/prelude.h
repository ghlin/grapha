#pragma once

#include <cstddef>           // size_t
#include <cstdint>           // [u]int_*_t
#include <cstdio>
#include <cstring>           // c style str*

#include <functional>
#include <utility>           // forward / move

#include <algorithm>
#include <iterator>

#include <string>
#include <tuple>

// smart pointers
#include <memory>

// nullable
#include <optional>

// tagged union
#include <variant>

// exceptions
#include <exception>
#include <stdexcept>

// containers
#include <array>
#include <list>
#include <vector>
#include <map>
#include <set>

namespace gi {

using i8  = std::int8_t;
using i16 = std::int16_t;
using i32 = std::int32_t;
using i64 = std::int64_t;

using u8  = std::uint8_t;
using u16 = std::uint16_t;
using u32 = std::uint32_t;
using u64 = std::uint64_t;

using buffer_ptr_t = u8 const*;

using std::size_t;

using namespace std::literals;

} // namespace gi

