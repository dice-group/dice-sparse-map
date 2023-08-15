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
#define DOCTEST_CONFIG_IMPLEMENT_WITH_MAIN
#include <doctest/doctest.h>

#include <dice/sparse_map/sparse_growth_policy.hpp>

#include <limits>
#include <ratio>
#include <stdexcept>

#define TEST_POLICIES dice::sparse_map::power_of_two_growth_policy<2>, \
					  dice::sparse_map::power_of_two_growth_policy<4>, \
					  dice::sparse_map::prime_growth_policy, \
					  dice::sparse_map::mod_growth_policy<>, \
					  dice::sparse_map::mod_growth_policy<std::ratio<7, 2>>

TEST_SUITE("policies") {
	TEST_CASE_TEMPLATE("test policy", Policy, TEST_POLICIES) {

		// Call next_bucket_count() on the policy until we reach its
		// max_bucket_count()
		bool exception_thrown = false;

		std::size_t bucket_count = 0;
		Policy policy(bucket_count);

		CHECK_EQ(policy.bucket_for_hash(0), 0);
		CHECK_EQ(bucket_count, 0);

		try {
			while (true) {
				const std::size_t previous_bucket_count = bucket_count;

				bucket_count = policy.next_bucket_count();
				policy = Policy(bucket_count);

				CHECK_EQ(policy.bucket_for_hash(0), 0);
				CHECK(bucket_count > previous_bucket_count);
			}
		} catch (const std::length_error&) {
			exception_thrown = true;
		}

		CHECK(exception_thrown);
	}

	TEST_CASE_TEMPLATE("min bucket count", Policy, TEST_POLICIES) {
		// Check policy when a bucket_count of 0 is asked.
		std::size_t bucket_count = 0;
		Policy policy(bucket_count);

		CHECK_EQ(policy.bucket_for_hash(0), 0);
	}

	TEST_CASE_TEMPLATE("max bucket count", Policy, TEST_POLICIES) {
		// Test a bucket_count equals to the max_bucket_count limit and above
		std::size_t bucket_count = 0;
		Policy policy(bucket_count);

		bucket_count = policy.max_bucket_count();
		Policy policy2(bucket_count);

		bucket_count = std::numeric_limits<std::size_t>::max();
		CHECK_THROWS_AS((Policy(bucket_count)), std::length_error);

		bucket_count = policy.max_bucket_count() + 1;
		CHECK_THROWS_AS((Policy(bucket_count)), std::length_error);
	}
}
