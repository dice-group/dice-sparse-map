#include "fixtures/counter.hpp"
#include "fixtures/inline_containers.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <cstddef>
#include <string>
#include <utility>

using namespace dice::unordered_sparse::tests;

/*
 * `emplace` and `emplace_hint` with the element as the arguments: one `value_type`, or one `key_type` and one
 * `mapped_type`, by value or by reference. They look up the key first. On a key that is in the container they copy,
 * move and destroy nothing, and they leave rvalue arguments as they are. A new element is constructed once, in its
 * slot. Other arguments, for example a `std::pair<Key const, T>`, construct the element first.
 */

TEST_CASE_MAP("emplace of a key and a mapped value constructs nothing on a present key", counter::obj, counter::obj) {
    auto counts = counter();
    INFO(counts);
    auto map = map_t();
    auto key = counter::obj(1, counts);
    auto mapped = counter::obj(2, counts);
    REQUIRE(map.emplace(key, mapped).second);
    auto const before = counts.data();
    REQUIRE_FALSE(map.emplace(key, mapped).second);
    REQUIRE_FALSE(map.emplace(std::as_const(key), std::as_const(mapped)).second);
    REQUIRE_FALSE(map.emplace(std::move(key), std::move(mapped)).second);  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
    CHECK(counts.copy_ctor() == before.copy_ctor);
    CHECK(counts.move_ctor() == before.move_ctor);
    CHECK(counts.dtor() == before.dtor);
    CHECK(map.size() == 1);
}

TEST_CASE_MAP("emplace of a value_type constructs nothing on a present key", counter::obj, counter::obj) {
    auto counts = counter();
    INFO(counts);
    auto map = map_t();
    auto value = typename map_t::value_type(counter::obj(1, counts), counter::obj(2, counts));
    REQUIRE(map.emplace(value).second);
    auto const before = counts.data();
    REQUIRE_FALSE(map.emplace(value).second);
    REQUIRE_FALSE(map.emplace(std::as_const(value)).second);
    REQUIRE_FALSE(map.emplace(std::move(value)).second);
    // `insert(P &&)` with a `value_type &` calls `emplace`
    REQUIRE_FALSE(map.insert(value).second);  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
    CHECK(counts.copy_ctor() == before.copy_ctor);
    CHECK(counts.move_ctor() == before.move_ctor);
    CHECK(counts.dtor() == before.dtor);
    CHECK(map.size() == 1);
}

TEST_CASE_MAP("emplace of a pair with a const key constructs the element on a present key", counter::obj, counter::obj) {
    auto counts = counter();
    INFO(counts);
    auto map = map_t();
    auto const pair = std::pair<counter::obj const, counter::obj>(counter::obj(1, counts), counter::obj(2, counts));
    REQUIRE(map.emplace(pair).second);
    auto const before = counts.data();
    REQUIRE_FALSE(map.emplace(pair).second);
    // the element is constructed from the pair (two copies) and destroyed again
    CHECK(counts.copy_ctor() == before.copy_ctor + 2);
    CHECK(counts.dtor() == before.dtor + 2);
    CHECK(map.size() == 1);
}

TEST_CASE_MAP("emplace of a new key and a mapped value constructs the element once, in its slot", counter::obj, counter::obj) {
    // with the inline capacity 4 the element is in the map object, `reserve(4)` keeps the map inline
    constexpr std::size_t capacity = inline_capacity_of_v<map_t>;
    auto counts = counter();
    INFO(counts);
    auto map = map_t();
    map.reserve(capacity > 0 ? capacity : 10);
    auto const key = counter::obj(1, counts);
    auto const mapped = counter::obj(2, counts);
    auto const before = counts.data();
    REQUIRE(map.emplace(key, mapped).second);
    CHECK(elements_in_object(map) == (capacity > 0));
    CHECK(counts.copy_ctor() == before.copy_ctor + 2);
    CHECK(counts.move_ctor() == before.move_ctor);
    CHECK(counts.dtor() == before.dtor);
    CHECK(map.at(key).get() == 2);
}

TEST_CASE_MAP("emplace_hint of a key and a mapped value constructs nothing on a present key", counter::obj, counter::obj) {
    auto counts = counter();
    INFO(counts);
    auto map = map_t();
    auto const key = counter::obj(1, counts);
    auto const mapped = counter::obj(2, counts);
    map.emplace(key, mapped);
    auto const before = counts.data();
    CHECK(map.emplace_hint(map.cend(), key, mapped) == map.find(key));
    CHECK(map.emplace_hint(map.find(key), key, mapped) == map.find(key));
    CHECK(counts.copy_ctor() == before.copy_ctor);
    CHECK(counts.move_ctor() == before.move_ctor);
    CHECK(counts.dtor() == before.dtor);
}

TEST_CASE_MAP("emplace of a key and a mapped value leaves rvalue arguments on a present key", std::string, std::string) {
    auto map = map_t();
    map.try_emplace(std::string(100, 'k'), "first");
    auto key = std::string(100, 'k');
    auto mapped = std::string(100, 'x');
    REQUIRE_FALSE(map.emplace(std::move(key), std::move(mapped)).second);
    CHECK(key == std::string(100, 'k'));     // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
    CHECK(mapped == std::string(100, 'x'));  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
    CHECK(map.at(std::string(100, 'k')) == "first");

    auto new_key = std::string(100, 'n');
    REQUIRE(map.emplace(std::move(new_key), std::move(mapped)).second);
    CHECK(map.at(std::string(100, 'n')) == std::string(100, 'x'));
    CHECK(map.size() == 2);
}

TEST_CASE_SET("emplace of a key constructs nothing on a present key", counter::obj) {
    auto counts = counter();
    INFO(counts);
    auto set = set_t();
    auto key = counter::obj(1, counts);
    REQUIRE(set.emplace(key).second);
    auto const before = counts.data();
    REQUIRE_FALSE(set.emplace(key).second);
    REQUIRE_FALSE(set.emplace(std::move(key)).second);
    CHECK(set.emplace_hint(set.cend(), key) == set.find(key));  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
    CHECK(counts.copy_ctor() == before.copy_ctor);
    CHECK(counts.move_ctor() == before.move_ctor);
    CHECK(counts.dtor() == before.dtor);
    CHECK(set.size() == 1);
}

/*
 * A container with an inline capacity keeps up to that many elements in the container object, in the inline group.
 * There `emplace` looks the key up first too. `counter_identity_hash` puts the key `k` into bucket `k % 64` of the
 * inline group and of a table of 64 buckets: `65` and `129` have the home bucket 1 and are in the buckets 2 and 4, so
 * the erasure of `1` leaves a deleted bucket that their lookups pass.
 */
namespace {

    struct counter_identity_hash {
        using is_avalanching = void;

        [[nodiscard]] std::size_t operator()(counter::obj const &key) const {
            return key.get();
        }
    };

}  // namespace

TEST_CASE_MAP("emplace looks the key up first also in the inline group", counter::obj, counter::obj, counter_identity_hash) {
    // with the inline capacity 4 the elements are in the map object
    constexpr bool inline_map = inline_capacity_of_v<map_t> > 0;
    auto counts = counter();
    INFO(counts);
    auto map = map_t();
    for (std::size_t const k : {5, 1, 65, 129}) {
        REQUIRE(map.try_emplace(counter::obj(k, counts), counter::obj(k + 1000, counts)).second);
    }
    REQUIRE(map.erase(counter::obj(1, counts)) == 1);
    CHECK(elements_in_object(map) == inline_map);
    for (std::size_t const k : {5, 65, 129}) {
        CAPTURE(k);
        auto key = counter::obj(k, counts);
        auto mapped = counter::obj(k + 2000, counts);
        auto value = typename map_t::value_type(counter::obj(k, counts), counter::obj(k + 3000, counts));
        auto const before = counts.data();
        REQUIRE_FALSE(map.emplace(key, mapped).second);
        REQUIRE_FALSE(map.emplace(std::move(key), std::move(mapped)).second);  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
        REQUIRE_FALSE(map.emplace(value).second);
        CHECK(map.emplace_hint(map.cend(), value) == map.find(value.first));
        CHECK(counts.copy_ctor() == before.copy_ctor);
        CHECK(counts.move_ctor() == before.move_ctor);
        CHECK(counts.dtor() == before.dtor);
        CHECK(map.at(value.first).get() == k + 1000);
    }
    CHECK(map.size() == 3);
    CHECK(elements_in_object(map) == inline_map);
    // a new key with the home bucket 1 takes the deleted bucket
    REQUIRE(map.emplace(counter::obj(193, counts), counter::obj(1193, counts)).second);
    CHECK(map.begin()->first.get() == 193);
    CHECK(map.size() == 4);
    CHECK(elements_in_object(map) == inline_map);
    CHECK(map.at(counter::obj(193, counts)).get() == 1193);
}

TEST_CASE_SET("emplace of a key looks it up first also in the inline group", counter::obj, counter_identity_hash) {
    // with the inline capacity 4 the elements are in the set object
    constexpr bool inline_set = inline_capacity_of_v<set_t> > 0;
    auto counts = counter();
    INFO(counts);
    auto set = set_t();
    for (std::size_t const k : {5, 1, 65, 129}) {
        REQUIRE(set.emplace(counter::obj(k, counts)).second);
    }
    REQUIRE(set.erase(counter::obj(1, counts)) == 1);
    CHECK(elements_in_object(set) == inline_set);
    for (std::size_t const k : {5, 65, 129}) {
        CAPTURE(k);
        auto key = counter::obj(k, counts);
        auto const before = counts.data();
        REQUIRE_FALSE(set.emplace(key).second);
        REQUIRE_FALSE(set.emplace(std::move(key)).second);
        CHECK(set.emplace_hint(set.cend(), key) == set.find(key));  // NOLINT(bugprone-use-after-move,hicpp-invalid-access-moved)
        CHECK(counts.copy_ctor() == before.copy_ctor);
        CHECK(counts.move_ctor() == before.move_ctor);
        CHECK(counts.dtor() == before.dtor);
    }
    CHECK(set.size() == 3);
    CHECK(elements_in_object(set) == inline_set);
}
