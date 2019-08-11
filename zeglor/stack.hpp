#pragma once

#include <boost/range/iterator_range.hpp>

#include <cstdint>

namespace zeglor
{
    // Get all words that are on the call stack of the calling thread. These are
    // the words between the bottom of the stack as reported by pthread, and the
    // top of the stack as reported by RSP.
    using stack_words_range = boost::iterator_range<std::intptr_t*>;
    stack_words_range stack_words() noexcept;
}
