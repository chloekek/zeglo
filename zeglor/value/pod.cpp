#include "pod.hpp"

#include "../heap.hpp"

#define ZEGLOR_POD_MAKE_DEFN(r, data, name_T) \
    zeglor::BOOST_PP_TUPLE_ELEM(2, 0, name_T)* \
    BOOST_PP_CAT(zeglor::make_, BOOST_PP_TUPLE_ELEM(2, 0, name_T)) \
    (heap& heap, BOOST_PP_TUPLE_ELEM(2, 1, name_T) payload) noexcept \
    { \
        return heap.make<zeglor::BOOST_PP_TUPLE_ELEM(2, 0, name_T)>(payload); \
    }

#define ZEGLOR_POD_READ_DEFN(r, data, name_T) \
    zeglor::BOOST_PP_TUPLE_ELEM(2, 1, name_T) \
    BOOST_PP_CAT(zeglor::read_, BOOST_PP_TUPLE_ELEM(2, 0, name_T)) \
    (BOOST_PP_TUPLE_ELEM(2, 0, name_T)* value) noexcept \
    { \
        return value->payload; \
    }

BOOST_PP_SEQ_FOR_EACH(ZEGLOR_POD_MAKE_DEFN, _, ZEGLOR_POD_TYPES)
