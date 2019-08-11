#include "heap.hpp"
#include "value/array.hpp"
#include "value/pod.hpp"

int main()
{
    zeglor::heap heap;

    auto a = zeglor::make_i32_value(heap, 42);
    auto b = zeglor::make_f32_value(heap, 42);
    std::vector<zeglor::value*> x{a, b};
    auto c = zeglor::make_array_value(heap, x.data(), x.data() + x.size());

    heap.collect_garbage();
    heap.collect_garbage();

    (void)a;
    (void)b;
    (void)c;

    return 0;
}
