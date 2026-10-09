#include "fixtures/allocators.hpp"
#include "fixtures/counter.hpp"

#include <dice/sparse-map/sparse_map.hpp>

#include <doctest/doctest.h>

#include <cstddef>
#include <functional>
#include <type_traits>
#include <utility>

/**
 * How a group moves its values on an insertion and on an erase in the middle of the group.
 *
 * A value type that is nothrow move constructible and nothrow move assignable moves by move assignment, as in
 * `std::vector`: an insertion constructs one value behind the last one through the allocator (besides the new value
 * in its holder), and an erase destroys the last one. Any other nothrow move constructible value type moves by
 * construction and destruction through the allocator. `construct_counting_allocator` counts the calls of `construct`
 * and `destroy`, `counter` counts the moves and the destructions of the values.
 */
namespace {
    using namespace dice::sparse_map;
    using namespace dice::sparse_map::tests;

    /**
     * `counter::obj` whose move assignment can throw. Its move constructor cannot.
     */
    struct throwing_assign_obj : counter::obj {
        using counter::obj::obj;

        throwing_assign_obj(throwing_assign_obj const &other) = default;
        throwing_assign_obj(throwing_assign_obj &&other) noexcept = default;
        throwing_assign_obj &operator=(throwing_assign_obj const &other) = default;

        throwing_assign_obj &operator=(throwing_assign_obj &&other) noexcept(
            false
        ) { // NOLINT(performance-noexcept-move-constructor)
            counter::obj::operator=(std::move(other));
            return *this;
        }

        ~throwing_assign_obj() = default;
    };

    static_assert(
        std::is_nothrow_move_constructible_v<counter::obj> && std::is_nothrow_move_assignable_v<counter::obj>
    );
    static_assert(std::is_nothrow_move_constructible_v<throwing_assign_obj>);
    static_assert(!std::is_nothrow_move_assignable_v<throwing_assign_obj>);

    template<typename T>
    using group_t = detail_sparse_hash::sparse_array<
        T,
        construct_counting_allocator<T>,
        sh::sparsity::medium,
        sh::allocation_failure::terminating
    >;

    using map_t = sparse_map<
        counter::obj,
        counter::obj,
        bucket_hash,
        std::equal_to<counter::obj>,
        construct_counting_allocator<std::pair<counter::obj, counter::obj>>
    >;

    /**
     * What an operation did: the calls of `construct` and `destroy` of the allocator, and the move constructions,
     * the move assignments and the destructions of `counter::obj`.
     */
    struct shift_counts {
        std::size_t constructs = 0;
        std::size_t destroys = 0;
        std::size_t move_ctor = 0;
        std::size_t move_assign = 0;
        std::size_t dtor = 0;
    };

    /// runs `operation` and returns what it did
    template<typename F>
    shift_counts counted(counter const &counts, F &&operation) {
        construct_counts const calls = construct_calls;
        counter::data_t const data = counts.data();
        std::forward<F>(operation)();
        return shift_counts{
            .constructs = construct_calls.constructs - calls.constructs,
            .destroys = construct_calls.destroys - calls.destroys,
            .move_ctor = counts.move_ctor() - data.move_ctor,
            .move_assign = counts.move_assign() - data.move_assign,
            .dtor = counts.dtor() - data.dtor,
        };
    }

    /**
     * Fills a group with the values 10, 20 and 30 at the indices 10, 20 and 30 (capacity 4), inserts the value 5 at
     * index 5, in front of all of them, and erases it again. Returns what the insertion and the erase did.
     */
    template<typename T>
    std::pair<shift_counts, shift_counts> insert_and_erase_in_front(counter &counts) {
        construct_counting_allocator<T> alloc;
        group_t<T> group;
        for (std::size_t const n : {10, 20, 30}) {
            group.set(alloc, static_cast<typename group_t<T>::size_type>(n), n, counts);
        }
        REQUIRE(group.size() == 3);

        auto const insertion = counted(counts, [&] { group.set(alloc, 5, std::size_t{5}, counts); });
        REQUIRE(group.size() == 4);
        CHECK(group.value(5)->get() == 5);
        CHECK(group.value(10)->get() == 10);
        CHECK(group.value(20)->get() == 20);
        CHECK(group.value(30)->get() == 30);

        auto const erase = counted(counts, [&] { group.erase(alloc, group.value(5), 5); });
        REQUIRE(group.size() == 3);
        CHECK(group.value(10)->get() == 10);
        CHECK(group.value(20)->get() == 20);
        CHECK(group.value(30)->get() == 30);

        group.clear(alloc);
        return {insertion, erase};
    }

} // namespace

TEST_CASE("a nothrow move assignable value moves by assignment in its group") {
    auto counts = counter{};
    auto const [insertion, erase] = insert_and_erase_in_front<counter::obj>(counts);

    // the new value in its holder, and the last value moved into the slot behind it
    CHECK(insertion.constructs == 2);
    CHECK(insertion.move_ctor == 1);
    // 20 and 10 one place to the back, and the new value into its place
    CHECK(insertion.move_assign == 3);
    // the holder
    CHECK(insertion.destroys == 1);
    CHECK(insertion.dtor == 1);

    // 10, 20 and 30 one place to the front, then the last slot, which is moved from
    CHECK(erase.constructs == 0);
    CHECK(erase.move_ctor == 0);
    CHECK(erase.move_assign == 3);
    CHECK(erase.destroys == 1);
    CHECK(erase.dtor == 1);
}

TEST_CASE("a value whose move assignment can throw moves by construction and destruction in its group") {
    auto counts = counter{};
    auto const [insertion, erase] = insert_and_erase_in_front<throwing_assign_obj>(counts);

    // the holder, 30, 20 and 10 one place to the back, and the new value at its place
    CHECK(insertion.constructs == 5);
    CHECK(insertion.move_ctor == 4);
    CHECK(insertion.move_assign == 0);
    // the old places of 30, 20 and 10, and the holder
    CHECK(insertion.destroys == 4);
    CHECK(insertion.dtor == 4);

    // the erased value, and 10, 20 and 30 one place to the front
    CHECK(erase.constructs == 3);
    CHECK(erase.move_ctor == 3);
    CHECK(erase.move_assign == 0);
    CHECK(erase.destroys == 4);
    CHECK(erase.dtor == 4);
}

TEST_CASE("an insertion right before the last value and an erase of the last value move no other value") {
    auto counts = counter{};
    construct_counting_allocator<counter::obj> alloc;
    group_t<counter::obj> group;
    for (std::size_t const n : {10, 20, 30}) {
        group.set(alloc, static_cast<group_t<counter::obj>::size_type>(n), n, counts);
    }

    // 25 goes between 20 and 30, at the offset of the last value
    auto const insertion = counted(counts, [&] { group.set(alloc, 25, std::size_t{25}, counts); });
    REQUIRE(group.size() == 4);
    CHECK(group.value(20)->get() == 20);
    CHECK(group.value(25)->get() == 25);
    CHECK(group.value(30)->get() == 30);
    // the new value in its holder, and 30 moved into the slot behind it
    CHECK(insertion.constructs == 2);
    CHECK(insertion.move_ctor == 1);
    // only the new value into its place
    CHECK(insertion.move_assign == 1);
    // the holder
    CHECK(insertion.destroys == 1);
    CHECK(insertion.dtor == 1);

    // 30 is the last value, so nothing moves
    auto const erase = counted(counts, [&] { group.erase(alloc, group.value(30), 30); });
    REQUIRE(group.size() == 3);
    CHECK(group.value(10)->get() == 10);
    CHECK(group.value(20)->get() == 20);
    CHECK(group.value(25)->get() == 25);
    CHECK(erase.constructs == 0);
    CHECK(erase.move_ctor == 0);
    CHECK(erase.move_assign == 0);
    CHECK(erase.destroys == 1);
    CHECK(erase.dtor == 1);

    group.clear(alloc);
}

TEST_CASE("a group of one value: an insertion in front of it and an erase of each value") {
    auto counts = counter{};
    construct_counting_allocator<counter::obj> alloc;
    group_t<counter::obj> group;
    group.set(alloc, 10, std::size_t{10}, counts);

    auto const insertion = counted(counts, [&] { group.set(alloc, 5, std::size_t{5}, counts); });
    REQUIRE(group.size() == 2);
    CHECK(group.value(5)->get() == 5);
    CHECK(group.value(10)->get() == 10);
    // the new value in its holder, and 10 moved into the slot behind it, then the new value into its place
    CHECK(insertion.constructs == 2);
    CHECK(insertion.move_ctor == 1);
    CHECK(insertion.move_assign == 1);
    CHECK(insertion.destroys == 1);
    CHECK(insertion.dtor == 1);

    // 10 is the last value, then 5 is the only one: nothing moves
    for (std::size_t const n : {10, 5}) {
        auto const erase = counted(counts, [&] {
            group.erase(
                alloc,
                group.value(static_cast<group_t<counter::obj>::size_type>(n)),
                static_cast<group_t<counter::obj>::size_type>(n)
            );
        });
        CHECK(erase.constructs == 0);
        CHECK(erase.move_ctor == 0);
        CHECK(erase.move_assign == 0);
        CHECK(erase.destroys == 1);
        CHECK(erase.dtor == 1);
    }
    REQUIRE(group.size() == 0);

    group.clear(alloc);
}

TEST_CASE("an insertion and an erase in front of the elements of a group of a map move the elements by assignment") {
    auto counts = counter{};
    auto map = map_t{};
    map.reserve(32);
    REQUIRE(map.bucket_count() == 64);
    for (std::size_t const n : {10, 20, 30}) {
        map.try_emplace(counter::obj{n, counts}, n, counts);
    }

    // key 5 goes in front of 10, 20 and 30, into a group with room for one more element
    counter::obj key{5, counts};
    auto const insertion = counted(counts, [&] { map.try_emplace(std::move(key), std::size_t{5}, counts); });
    REQUIRE(map.size() == 4);

    // the new element in its holder (its key is moved in), and the last element moved into the slot behind it
    CHECK(insertion.constructs == 2);
    CHECK(insertion.move_ctor == 3);
    // key and value of 20 and 10 one place to the back, and of the new element into its place
    CHECK(insertion.move_assign == 6);
    // key and value of the holder
    CHECK(insertion.destroys == 1);
    CHECK(insertion.dtor == 2);

    auto const it = map.find(counter::obj{5, counts});
    REQUIRE(it != map.end());
    auto const erase = counted(counts, [&] { map.erase(it); });
    REQUIRE(map.size() == 3);

    // key and value of 10, 20 and 30 one place to the front, then the last slot, which is moved from
    CHECK(erase.constructs == 0);
    CHECK(erase.move_ctor == 0);
    CHECK(erase.move_assign == 6);
    CHECK(erase.destroys == 1);
    CHECK(erase.dtor == 2);

    for (std::size_t const n : {10, 20, 30}) {
        auto const found = map.find(counter::obj{n, counts});
        REQUIRE(found != map.end());
        CHECK(found->second.get() == n);
    }
}
