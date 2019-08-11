#pragma once

#include <cstddef>
#include <vector>

#include <unordered_set>

namespace zeglor
{
    class value;
}

namespace zeglor
{
    // A heap is an object for allocating and deallocating memory for values.
    // Heaps keep track of all values created through them and provide garbage
    // collection.
    class heap
    {
    public:
        heap();
        heap(heap const&) = delete;
        heap& operator=(heap const&) = delete;
        ~heap();

        // Allocate memory for a value and initialize the value. The arguments
        // will be forwarded to the constructor of T. T must be a subclass of
        // the value class.
        //
        // T::size(Args...) must return the amount of memory to allocate. This
        // may be larger than sizeof(T) for dynamically sized values such as
        // arrays. It must be at least sizeof(T).
        template<typename T, typename... Args>
        T* make(Args&&...) noexcept;

        // Find values that are not reachable and free the memory they occupy.
        // Must be called on the mutator thread.
        void collect_garbage() noexcept;

    private:
        // Allocate memory for a value. The memory is not initialized.
        value* allocate(std::size_t) noexcept;

        // Free memory for a value, but do not touch the values array.
        static void deallocate(value*) noexcept;

        // The values created on this heap, in order of creation time. Values
        // later in the vector may only point to (as in value::children())
        // values earlier in the vector.
        std::vector<value*> values;
    };
}

template<typename T, typename... Args>
T* zeglor::heap::make(Args&&... args)
    noexcept
{
    auto pointer = allocate(T::size(std::forward<Args>(args)...));
    new (pointer) T(std::forward<Args>(args)...);
    return static_cast<T*>(pointer);
}
