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

#include <algorithm>
#include <array>
#include <cassert>
#include <concepts>
#include <cmath>
#include <cstddef>
#include <limits>
#include <ratio>
#include <stdexcept>

namespace dice::sparse_map {

	template<typename G>
	concept growth_policy = requires (G const cgpol, G gpol, std::size_t &min_bucket_count_in_out, std::size_t hash) {
		G{min_bucket_count_in_out};
		{ cgpol.bucket_for_hash(hash) } -> std::convertible_to<std::size_t>;
		{ cgpol.next_bucket_count() } -> std::convertible_to<std::size_t>;
		{ G::max_bucket_count() } -> std::convertible_to<std::size_t>;
		gpol.clear();

		noexcept(cgpol.bucket_for_hash(hash));
		noexcept(G::max_bucket_count());
		noexcept(gpol.clear());
	};

	/**
	 * Grow the hash table by a factor of GrowthFactor keeping the bucket count to a
	 * power of two. It allows the table to use a mask operation instead of a modulo
	 * operation to map a hash to a bucket.
	 *
	 * GrowthFactor must be a power of two >= 2.
	 */
	template<std::size_t GrowthFactor> requires (std::has_single_bit(GrowthFactor) && GrowthFactor >= 2)
	struct power_of_two_growth_policy {
	protected:
		std::size_t m_mask;

	public:
		/**
         * Called on the hash table creation and on rehash. The number of buckets for
         * the table is passed in parameter. This number is a minimum, the policy may
         * update this value with a higher value if needed (but not lower).
         *
         * If 0 is given, min_bucket_count_in_out must still be 0 after the policy
         * creation and bucket_for_hash must always return 0 in this case.
         */
		explicit constexpr power_of_two_growth_policy(std::size_t &min_bucket_count_in_out) {
			if (min_bucket_count_in_out > max_bucket_count()) [[unlikely]] {
				throw std::length_error{"The hash table exceeds its maximum size."};
			}

			if (min_bucket_count_in_out > 0) {
				min_bucket_count_in_out = std::bit_ceil(min_bucket_count_in_out);
				m_mask = min_bucket_count_in_out - 1;
			} else {
				m_mask = 0;
			}
		}

		/**
         * Return the bucket [0, bucket_count()) to which the hash belongs.
         * If bucket_count() is 0, it must always return 0.
         */
		[[nodiscard]] constexpr std::size_t bucket_for_hash(std::size_t hash) const noexcept {
			return hash & m_mask;
		}

		/**
         * Return the number of buckets that should be used on next growth.
         */
		[[nodiscard]] constexpr std::size_t next_bucket_count() const {
			if ((m_mask + 1) > max_bucket_count() / GrowthFactor) [[unlikely]] {
				throw std::length_error("The hash table exceeds its maximum size.");
			}

			return (m_mask + 1) * GrowthFactor;
		}

		/**
         * Return the maximum number of buckets supported by the policy.
         */
		[[nodiscard]] static constexpr std::size_t max_bucket_count() noexcept {
			// Largest power of two.
			return (std::numeric_limits<std::size_t>::max() / 2) + 1;
		}

		/**
         * Reset the growth policy as if it was created with a bucket count of 0.
         * After a clear, the policy must always return 0 when bucket_for_hash is
         * called.
         */
		constexpr void clear() noexcept {
			m_mask = 0;
		}

		[[nodiscard]] constexpr std::size_t mask() const noexcept {
			return m_mask;
		}
	};

	/**
	 * Grow the hash table by GrowthFactor::num / GrowthFactor::den and use a modulo
	 * to map a hash to a bucket. Slower but it can be useful if you want a slower
	 * growth.
	 */
	template<typename GrowthFactor = std::ratio<3, 2>>
	struct mod_growth_policy {
	protected:
		static constexpr double REHASH_SIZE_MULTIPLICATION_FACTOR = 1.0 * GrowthFactor::num / GrowthFactor::den;
		static_assert(REHASH_SIZE_MULTIPLICATION_FACTOR >= 1.1, "Growth factor should be >= 1.1.");

		static constexpr std::size_t MAX_BUCKET_COUNT = static_cast<std::size_t>(static_cast<double>(std::numeric_limits<std::size_t>::max()) / REHASH_SIZE_MULTIPLICATION_FACTOR);

		std::size_t m_mod;

	public:
		explicit constexpr mod_growth_policy(std::size_t &min_bucket_count_in_out) {
			if (min_bucket_count_in_out > max_bucket_count()) [[unlikely]] {
				throw std::length_error{"The hash table exceeds its maximum size."};
			}

			if (min_bucket_count_in_out > 0) {
				m_mod = min_bucket_count_in_out;
			} else {
				m_mod = 1;
			}
		}

		[[nodiscard]] constexpr std::size_t bucket_for_hash(std::size_t hash) const noexcept {
			return hash % m_mod;
		}

		[[nodiscard]] constexpr std::size_t next_bucket_count() const {
			if (m_mod == max_bucket_count()) [[unlikely]] {
				throw std::length_error{"The hash table exceeds its maximum size."};
			}

			auto const next_bucket_count = std::ceil(static_cast<double>(m_mod) * REHASH_SIZE_MULTIPLICATION_FACTOR);
			if (!std::isnormal(next_bucket_count)) [[unlikely]] {
				throw std::length_error{"The hash table exceeds its maximum size."};
			}

			if (next_bucket_count > static_cast<double>(max_bucket_count())) {
				return max_bucket_count();
			}

			return static_cast<std::size_t>(next_bucket_count);
		}

		[[nodiscard]] static constexpr std::size_t max_bucket_count() noexcept {
			return MAX_BUCKET_COUNT;
		}

		void clear() noexcept {
			m_mod = 1;
		}
	};

	/**
	 * Grow the hash table by using prime numbers as bucket count. Slower than
	 * dice::sh::power_of_two_growth_policy in general but will probably distribute
	 * the values around better in the buckets with a poor hash function.
	 *
	 * To allow the compiler to optimize the modulo operation, a lookup table is
	 * used with constant primes numbers.
	 *
	 * With a switch the code would look like:
	 * \code
	 * switch(iprime) { // iprime is the current prime of the hash table
	 *     case 0: hash % 5ul;
	 *             break;
	 *     case 1: hash % 17ul;
	 *             break;
	 *     case 2: hash % 29ul;
	 *             break;
	 *     ...
	 * }
	 * \endcode
	 *
	 * Due to the constant variable in the modulo the compiler is able to optimize
	 * the operation by a series of multiplications, substractions and shifts.
	 *
	 * The 'hash % 5' could become something like 'hash - (hash * 0xCCCCCCCD) >> 34)
	 * * 5' in a 64 bits environment.
	 */
	struct prime_growth_policy {
	protected:
		static constexpr std::array<std::size_t, 40> PRIMES{1ul, 5ul, 17ul, 29ul, 37ul,
															53ul, 67ul, 79ul, 97ul, 131ul,
															193ul, 257ul, 389ul, 521ul, 769ul,
															1031ul, 1543ul, 2053ul, 3079ul, 6151ul,
															12289ul, 24593ul, 49157ul, 98317ul, 196613ul,
															393241ul, 786433ul, 1572869ul, 3145739ul, 6291469ul,
															12582917ul, 25165843ul, 50331653ul, 100663319ul, 201326611ul,
															402653189ul, 805306457ul, 1610612741ul, 3221225473ul, 4294967291ul};

		std::uint8_t m_iprime;

	public:
		explicit constexpr prime_growth_policy(std::size_t &min_bucket_count_in_out) {
			auto it_prime = std::lower_bound(PRIMES.begin(), PRIMES.end(), min_bucket_count_in_out);
			if (it_prime == PRIMES.end()) [[unlikely]] {
				throw std::length_error{"The hash table exceeds its maximum size."};
			}

			m_iprime = static_cast<unsigned int>(std::distance(PRIMES.begin(), it_prime));

			if (min_bucket_count_in_out > 0) {
				min_bucket_count_in_out = *it_prime;
			} else {
				min_bucket_count_in_out = 0;
			}
		}

		[[nodiscard]] constexpr std::size_t bucket_for_hash(std::size_t hash) const noexcept {
			return hash % PRIMES[m_iprime];
		}

		[[nodiscard]] constexpr std::size_t next_bucket_count() const {
			if (static_cast<size_t>(m_iprime) + 1 >= PRIMES.size()) {
				throw std::length_error("The hash table exceeds its maximum size.");
			}

			return PRIMES[m_iprime + 1];
		}

		[[nodiscard]] static constexpr std::size_t max_bucket_count() noexcept {
			return PRIMES.back();
		}

		constexpr void clear() noexcept {
			m_iprime = 0;
		}
	};

}// namespace dice::sparse_map

#endif
