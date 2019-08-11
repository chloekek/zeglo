#pragma once

#include "../value.hpp"

#include <boost/range/iterator_range.hpp>

#include <cstddef>

namespace zeglor
{
    class heap;
}

namespace zeglor
{
    class array_value
        : public value
    {
    public:
        // Construct the array with the elements given by the iterator pair.
        array_value(value* const*, value* const*) noexcept;
        static std::size_t size(value* const*, value* const*) noexcept;

        // Return the elements of the array.
        using elements_range = boost::iterator_range<value* const*>;
        elements_range elements() const noexcept;

        children_range children() const noexcept override;

    private:
        std::size_t length;
    };

    array_value* make_array_value(heap&, value* const*, value* const*) noexcept;
}
