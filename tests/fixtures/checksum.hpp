#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_CHECKSUM_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_CHECKSUM_HPP

#include "fixtures/counter.hpp"

#include <cstdint>
#include <string_view>

/**
 * Order independent checksums of maps, to compare the contents of two maps.
 *
 * Ported from the test fixtures of ankerl::unordered_dense (MIT license).
 */
namespace dice::sparse_map::tests::checksum {

    /// final step of MurmurHash3
    [[nodiscard]] inline std::uint64_t mix(std::uint64_t k) noexcept {
        k ^= k >> 33U;
        k *= 0xff51afd7ed558ccdULL;
        k ^= k >> 33U;
        k *= 0xc4ceb9fe1a85ec53ULL;
        k ^= k >> 33U;
        return k;
    }

    /// FNV-1a
    [[nodiscard]] inline std::uint64_t mix(std::string_view data) noexcept {
        std::uint64_t val = 14695981039346656037ULL;
        for (auto c : data) {
            val ^= static_cast<std::uint64_t>(c);
            val *= 1099511628211ULL;
        }
        return val;
    }

    [[nodiscard]] inline std::uint64_t mix(counter::obj const &cdv) {
        return mix(cdv.get());
    }

    /// `boost::hash_combine`
    [[nodiscard]] inline std::uint64_t combine(std::uint64_t seed, std::uint64_t value) noexcept {
        return seed ^ (value + 0x9e3779b9 + (seed << 6U) + (seed >> 2U));
    }

    /**
     * Checksum of a map. The order of the elements does not change the result.
     */
    template<typename Map>
    [[nodiscard]] std::uint64_t map(Map const &map) {
        std::uint64_t combined_hash = 1;
        std::uint64_t num_elements = 0;
        for (auto const &entry : map) {
            combined_hash ^= combine(mix(entry.first), mix(entry.second));
            ++num_elements;
        }
        return combine(combined_hash, num_elements);
    }

    /**
     * Checksum of a map whose mapped values are maps.
     */
    template<typename MapMap>
    [[nodiscard]] std::uint64_t mapmap(MapMap const &mapmap) {
        std::uint64_t combined_hash = 1;
        std::uint64_t num_elements = 0;
        for (auto const &entry : mapmap) {
            combined_hash ^= combine(mix(entry.first), map(entry.second));
            ++num_elements;
        }
        return combine(combined_hash, num_elements);
    }

} // namespace dice::sparse_map::tests::checksum

#endif // DICE_SPARSE_MAP_TESTS_FIXTURES_CHECKSUM_HPP
