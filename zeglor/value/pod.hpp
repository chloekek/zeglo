#pragma once

#include "../value.hpp"

#include <boost/preprocessor.hpp>

#include <cstddef>
#include <cstdint>

namespace zeglor
{
    class heap;
}

////////////////////////////////////////////////////////////////////////////////

namespace zeglor
{
    // Subclass of value for housing PODs. This is useful for simple types such
    // as integer types and floating-point number types.
    //
    // PODs inside pod_value values should not point to other values, as
    // pod_value<T>::children() returns the empty range.
    template<typename T>
    class pod_value
        : public value
    {
    public:
        explicit pod_value(T) noexcept;
        static std::size_t size(T) noexcept;

        T payload;

        children_range children() const noexcept override;
    };
}

template<typename T>
zeglor::pod_value<T>::pod_value(T payload)
    noexcept
    : payload(payload)
{
}

template<typename T>
std::size_t zeglor::pod_value<T>::size(T)
    noexcept
{
    return sizeof(zeglor::pod_value<T>);
}

template<typename T>
zeglor::value::children_range zeglor::pod_value<T>::children()
    const noexcept
{
    return children_range();
}

////////////////////////////////////////////////////////////////////////////////

// Below we declare functions for making and reading POD values. We use
// Boost.Preprocessor to generate this boilerplate for many POD types. These
// functions are defined in pod.cpp using a similar mechanism.

#define ZEGLOR_POD_TYPES \
    (( i8_value  , std::int8_t  )) \
    (( i16_value , std::int16_t )) \
    (( i32_value , std::int32_t )) \
    (( i64_value , std::int64_t )) \
    (( f32_value , float        )) \
    (( f64_value , double       ))

#define ZEGLOR_POD_ALIAS(r, data, name_T) \
    using BOOST_PP_TUPLE_ELEM(2, 0, name_T) = \
        pod_value<BOOST_PP_TUPLE_ELEM(2, 1, name_T)>;

#define ZEGLOR_POD_MAKE_DECL(r, data, name_T) \
    BOOST_PP_TUPLE_ELEM(2, 0, name_T)* \
    BOOST_PP_CAT(make_, BOOST_PP_TUPLE_ELEM(2, 0, name_T)) \
    (heap&, BOOST_PP_TUPLE_ELEM(2, 1, name_T)) noexcept;

#define ZEGLOR_POD_READ_DECL(r, data, name_T) \
    BOOST_PP_TUPLE_ELEM(2, 1, name_T) \
    BOOST_PP_CAT(read_, BOOST_PP_TUPLE_ELEM(2, 0, name_T)) \
    (BOOST_PP_TUPLE_ELEM(2, 0, name_T)*) noexcept;

namespace zeglor
{
    BOOST_PP_SEQ_FOR_EACH(ZEGLOR_POD_ALIAS     , _ , ZEGLOR_POD_TYPES)
    BOOST_PP_SEQ_FOR_EACH(ZEGLOR_POD_MAKE_DECL , _ , ZEGLOR_POD_TYPES)
    BOOST_PP_SEQ_FOR_EACH(ZEGLOR_POD_READ_DECL , _ , ZEGLOR_POD_TYPES)
}

#undef ZEGLOR_POD_ALIAS
#undef ZEGLOR_POD_MAKE_DECL
#undef ZEGLOR_POD_READ_DECL
