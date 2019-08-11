#pragma once

#include <boost/range/iterator_range.hpp>

namespace zeglor
{
    // Every value is an instance of this abstract class. The compiler generates
    // more subclasses of this class, so you should not blindly cause the layout
    // (including the vtable layout) of the base class to be changed.
    class value
    {
    public:
        value();
        value(value const&) = delete;
        value& operator=(value const&) = delete;
        virtual ~value();

        // Find the values reachable through one indirection from this value.
        // The garbage collector will use this information to find reachable
        // values.
        //
        // The order of the values in the returned range is insignificant and
        // duplicates are not problematic.
        using children_range = boost::iterator_range<value* const*>;
        virtual children_range children() const noexcept = 0;
    };
}
