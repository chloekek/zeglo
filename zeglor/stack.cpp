#include "stack.hpp"

#include <cassert>
#include <pthread.h>

namespace
{
    // Check whether a pointer is aligned to the size of a word.
    bool word_aligned(void* ptr)
        noexcept
    {
        return reinterpret_cast<std::intptr_t>(ptr)
                   % sizeof(std::intptr_t) == 0;
    }

    // Find the low address of the stack, i.e. the value of RSP.
    __attribute__((always_inline))
    void* stack_low()
        noexcept
    {
        return __builtin_frame_address(0);
    }

    // Find the high address of the stack, i.e. the bottom of the stack, as
    // reported by pthread.
    void* stack_high()
        noexcept
    {
        void* stack_address;
        std::size_t stack_size;
        pthread_attr_t attr;
        pthread_getattr_np(pthread_self(), &attr);
        pthread_attr_getstack(&attr, &stack_address, &stack_size);
        return static_cast<char*>(stack_address) + stack_size;
    }
}

__attribute__((noinline))
zeglor::stack_words_range zeglor::stack_words()
    noexcept
{
    void* low  = stack_low();
    void* high = stack_high();

    // Ensure that we create a valid range. If the pointers are misaligned then
    // incrementing low will never result in a value equal to high.
    assert(low <= high);
    assert(word_aligned(low));
    assert(word_aligned(high));

    return {
        static_cast<std::intptr_t*>(low),
        static_cast<std::intptr_t*>(high),
    };
}
