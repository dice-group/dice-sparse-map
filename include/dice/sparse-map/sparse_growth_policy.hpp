/**
 * MIT License
 *
 * Copyright (c) 2017 Thibaut Goetghebuer-Planchon <tessil@gmx.com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */
#ifndef DICE_SPARSE_MAP_SPARSE_GROWTH_POLICY_HPP
#define DICE_SPARSE_MAP_SPARSE_GROWTH_POLICY_HPP

#include <cstddef>

namespace dice::sparse_map::sh {

    /**
     * The growth policy of `sparse_map` and `sparse_set`. The class has no members, it only names the policy. The
     * containers accept only `power_of_two_growth_policy<2>`, their default. The number of buckets is 0 or a power of
     * two and doubles when the table grows. A hash picks its bucket with a mask, `hash & (bucket_count() - 1)`.
     *
     * @tparam GrowthFactor the factor by which the number of buckets grows. Only 2 is accepted.
     */
    template<std::size_t GrowthFactor>
    struct power_of_two_growth_policy {};

}  // namespace dice::sparse_map::sh

#endif
