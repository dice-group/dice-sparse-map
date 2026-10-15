#include <dice/sparse-map/sparse_set.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <cstdint>

/**
 * After `reserve(n)`, the container holds `n` elements without a rehash, and after `rehash(n)` it has at least
 * `size() / max_load_factor()` buckets. The tests use a size above 2^24, where a `float` cannot hold every
 * integer any more.
 */
namespace {
    /// the smallest size that a `float` cannot hold exactly
    constexpr std::size_t nb_elements = (std::size_t{1} << 24) + 1;

    /// `bucket_count() * max_load_factor()`, the number of elements that `set` holds without a rehash
    template<typename Set>
    std::size_t room(Set const &set) {
        return static_cast<std::size_t>(static_cast<double>(set.bucket_count()) * static_cast<double>(set.max_load_factor()));
    }
}  // namespace

TEST_CASE("reserve makes room for more than 2^24 elements") {
    auto set = dice::sparse_map::sparse_set<std::uint64_t>{};
    set.reserve(nb_elements);
    CHECK(room(set) >= nb_elements);

    auto const bucket_count = set.bucket_count();
    for (std::uint64_t key = 0; key < nb_elements; ++key) {
        set.insert(key);
    }
    CHECK(set.size() == nb_elements);
    // no rehash
    CHECK(set.bucket_count() == bucket_count);

    set.rehash(0);
    CHECK(room(set) >= set.size());
}
