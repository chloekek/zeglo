#include "heap.hpp"
#include "stack.hpp"
#include "value.hpp"

#include <boost/range/adaptor/transformed.hpp>

#include <algorithm>
#include <utility>

using boost::adaptors::transformed;

zeglor::heap::heap() = default;

zeglor::heap::~heap()
{
    for (auto value : values)
    {
        value->~value();
        deallocate(value);
    }
}

zeglor::value* zeglor::heap::allocate(std::size_t size)
    noexcept
{
    auto void_pointer = operator new(size);
    auto pointer = static_cast<value*>(void_pointer);
    values.push_back(pointer);
    return pointer;
}

void zeglor::heap::deallocate(value* value)
    noexcept
{
    operator delete(value);
}

void zeglor::heap::collect_garbage()
    noexcept
{
    // Find the roots. Roots are values pointed to from the stack. Because we
    // have not yet implemented the generation or use of stack maps, we just
    // assume every word on the stack is a pointer to a value.
    auto roots
        = stack_words()
        | transformed([] (std::intptr_t word) {
              return reinterpret_cast<value*>(word);
          });

    // All values that are marked.
    std::unordered_set<value*> markeds;
    auto mark      = [&] (auto value) { markeds.insert(value); };
    auto is_marked = [&] (auto value) { return !!markeds.count(value); };

    // All values that survive, in reverse order of creation time.
    std::vector<value*> survivors;

    // First, mark all roots.
    for (auto value : roots)
        mark(value);

    // Then, loop through the values in reverse order of creation, and mark and
    // sweep them.
    for (auto it = values.rbegin(); it != values.rend(); ++it) {
        auto value = *it;

        if (is_marked(value)) {
            // Marked objects survive.
            survivors.push_back(value);

            // Mark all immediate children. Because we loop backwards, and
            // values only point to older values, we will eventually reach these
            // children too, so no recursion is necessary.
            for (auto child : value->children())
                mark(child);
        } else {
            // Not marked, so sweep.
            value->~value();
            deallocate(value);
        }
    }

    // Ensure the survivors are in order of creation time and reassign the
    // current heap value array.
    std::reverse(survivors.begin(), survivors.end());
    values = std::move(survivors);
}
