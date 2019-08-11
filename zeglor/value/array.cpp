#include "array.hpp"

#include "../heap.hpp"

#include <algorithm>

static_assert(sizeof(zeglor::array_value) % sizeof(zeglor::value*) == 0);

zeglor::array_value::array_value(value* const* begin, value* const* end)
    noexcept
    : length(end - begin)
{
    std::copy(begin, end, reinterpret_cast<value**>(this + 1));
}

std::size_t zeglor::array_value::size(value* const* begin, value* const* end)
    noexcept
{
    return sizeof(array_value) + sizeof(value*) * (end - begin);
}

zeglor::array_value::elements_range zeglor::array_value::elements()
    const noexcept
{
    auto begin = reinterpret_cast<value* const*>(this + 1);
    auto end   = begin + length;
    return {begin, end};
}

zeglor::value::children_range zeglor::array_value::children()
    const noexcept
{
    return elements();
}

zeglor::array_value*
zeglor::make_array_value(heap& heap, value* const* begin, value* const* end)
    noexcept
{
    return heap.make<array_value>(begin, end);
}
