#include "fixtures/counter.hpp"
#include "fixtures/test_types.hpp"

#include <doctest/doctest.h>

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <functional>
#include <iterator>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

/**
 * Randomized differential test. A random sequence of operations runs on a container and on
 * `std::unordered_map` or `std::unordered_set`, and the two must agree after every step. The
 * sequences come from fixed seeds, so every run does the same. Keys and mapped values are
 * `counter::obj`, so a container that loses, duplicates or leaks an element is noticed. The
 * configurations with `copied_obj`, whose move constructor can throw, have groups with holes. The
 * reference holds plain numbers.
 *
 * Partly ported from the API fuzz test of ankerl::unordered_dense (MIT license).
 */
namespace {
    using namespace dice::sparse_map::tests;

    /// splitmix64, a small random generator that gives the same numbers on every platform
    struct random_source {
        explicit random_source(std::uint64_t seed) noexcept
            : state_(seed) {
        }

        std::uint64_t next() noexcept {
            state_ += 0x9e3779b97f4a7c15ULL;
            std::uint64_t z = state_;
            z = (z ^ (z >> 30U)) * 0xbf58476d1ce4e5b9ULL;
            z = (z ^ (z >> 27U)) * 0x94d049bb133111ebULL;
            return z ^ (z >> 31U);
        }

        /// a number in [0, `bound`)
        std::size_t below(std::size_t bound) noexcept {
            return static_cast<std::size_t>(next() % bound);
        }

    private:
        std::uint64_t state_;
    };

    /**
     * hash that maps every key to one of 7 values, so that many keys share one probe sequence. It is not avalanching,
     * but it is marked as avalanching, so that the containers accept it. The keys collide on purpose.
     */
    struct colliding_hash {
        using is_avalanching = void;

        std::size_t operator()(counter::obj const &key) const {
            return key.get_for_hash() % 7;
        }
    };

    /// an operation of the test and how often it is picked, relative to the other operations
    struct weighted_op {
        std::string_view name;
        std::size_t weight;
    };

    constexpr std::array map_ops{
        weighted_op{"insert", 100},
        weighted_op{"emplace", 70},
        weighted_op{"try_emplace", 70},
        weighted_op{"insert_or_assign", 70},
        weighted_op{"subscript", 70},
        weighted_op{"erase_key", 140},
        weighted_op{"erase_iterator", 40},
        weighted_op{"erase_range", 8},
        weighted_op{"find", 100},
        weighted_op{"count", 40},
        weighted_op{"contains", 40},
        weighted_op{"at", 40},
        weighted_op{"rehash", 6},
        weighted_op{"reserve", 6},
        weighted_op{"clear", 2},
        weighted_op{"copy_construct", 4},
        weighted_op{"copy_assign", 4},
        weighted_op{"move_assign", 4},
        weighted_op{"swap", 4},
        weighted_op{"merge", 4},
        weighted_op{"erase_if", 2},
        weighted_op{"fill_side", 30},
    };

    constexpr std::array set_ops{
        weighted_op{"insert", 170},
        weighted_op{"emplace", 170},
        weighted_op{"erase_key", 140},
        weighted_op{"erase_iterator", 40},
        weighted_op{"erase_range", 8},
        weighted_op{"find", 100},
        weighted_op{"count", 40},
        weighted_op{"contains", 40},
        weighted_op{"rehash", 6},
        weighted_op{"reserve", 6},
        weighted_op{"clear", 2},
        weighted_op{"copy_construct", 4},
        weighted_op{"copy_assign", 4},
        weighted_op{"move_assign", 4},
        weighted_op{"swap", 4},
        weighted_op{"merge", 4},
        weighted_op{"erase_if", 2},
        weighted_op{"fill_side", 30},
    };

    template<std::size_t num_ops>
    std::string_view pick(random_source &random, std::array<weighted_op, num_ops> const &ops) {
        std::size_t total = 0;
        for (auto const &op : ops) {
            total += op.weight;
        }
        auto roll = random.below(total);
        for (auto const &op : ops) {
            if (roll < op.weight) {
                return op.name;
            }
            roll -= op.weight;
        }
        return ops.back().name;
    }

    constexpr std::array<std::uint64_t, 8> seeds{1, 2, 3, 4, 5, 6, 7, 0x5eed};
    constexpr std::size_t steps_per_seed = 50000;
    /// every this many steps, the whole contents are compared
    constexpr std::size_t full_check_interval = 256;
    /// every this many steps, the test picks a new range for the keys
    constexpr std::size_t phase_length = 1000;

    /// key ranges for a good hash: from 16 keys, where keys repeat often, to 2^20 keys, where most inserts add a new
    /// key. The containers hold up to a few hundred elements.
    constexpr std::array<std::size_t, 4> spread_key_ranges{16, 256, 4096, std::size_t{1} << 20U};
    /// key ranges for `colliding_hash`: smaller, because each lookup walks a long probe sequence
    constexpr std::array<std::size_t, 3> colliding_key_ranges{16, 128, 1024};

    using reference_map = std::unordered_map<std::size_t, std::size_t>;
    using reference_set = std::unordered_set<std::size_t>;

    /// true if `map` holds exactly the entries of `reference`, by iteration and by lookup
    template<typename Map>
    bool same_contents(Map const &map, reference_map const &reference, counter &counts) {
        if (map.size() != reference.size()) {
            return false;
        }
        std::unordered_set<std::size_t> seen;
        for (auto const &entry : map) {
            auto const it = reference.find(entry.first.get());
            if (it == reference.end() || it->second != entry.second.get() || !seen.insert(entry.first.get()).second) {
                return false;
            }
        }
        if (seen.size() != reference.size()) {
            return false;
        }
        for (auto const &[key, value] : reference) {
            auto const it = map.find(typename Map::key_type{key, counts});
            if (it == map.end() || it->second.get() != value) {
                return false;
            }
        }
        return true;
    }

    /// true if `set` holds exactly the keys of `reference`, by iteration and by lookup
    template<typename Set>
    bool same_contents(Set const &set, reference_set const &reference, counter &counts) {
        if (set.size() != reference.size()) {
            return false;
        }
        std::unordered_set<std::size_t> seen;
        for (auto const &key : set) {
            if (!reference.contains(key.get()) || !seen.insert(key.get()).second) {
                return false;
            }
        }
        if (seen.size() != reference.size()) {
            return false;
        }
        for (auto const key : reference) {
            if (set.find(typename Set::key_type{key, counts}) == set.end()) {
                return false;
            }
        }
        return true;
    }

    /// key of the element `it` points to
    template<typename Iterator>
    std::size_t key_at(Iterator const &it) {
        if constexpr (requires { it->first; }) {
            return it->first.get();
        } else {
            return it->get();
        }
    }

    /// key of an element of a container under test, or of a reference container
    template<typename Element>
    std::size_t key_of(Element const &element) {
        if constexpr (requires { element.first.get(); }) {
            return element.first.get();
        } else if constexpr (requires { element.first; }) {
            return element.first;
        } else if constexpr (requires { element.get(); }) {
            return element.get();
        } else {
            return element;
        }
    }

    /**
     * The operations that maps and sets share. Returns false if `op` is not one of them.
     * `container` and `reference` are the pair under test, `side` and `side_reference` a second pair
     * for the operations that need two containers.
     */
    template<typename Container, typename Reference, typename Insert>
    bool run_common_op(std::string_view op,
                       random_source &random,
                       std::size_t key,
                       std::size_t key_range,
                       counter &counts,
                       Container &container,
                       Reference &reference,
                       Container &side,
                       Reference &side_reference,
                       Insert const &insert_into) {
        if (op == "erase_key") {
            REQUIRE(container.erase(typename Container::key_type{key, counts}) == reference.erase(key));
        } else if (op == "erase_iterator") {
            if (!container.empty()) {
                auto const index = static_cast<std::ptrdiff_t>(random.below(container.size()));
                auto const it = std::next(container.begin(), index);
                auto const erased_key = key_at(it);
                auto const next = std::next(it);
                bool const next_is_end = next == container.end();
                std::size_t const next_key = next_is_end ? std::size_t{0} : key_at(next);

                auto const returned = container.erase(it);

                REQUIRE(reference.erase(erased_key) == 1);
                REQUIRE((returned == container.end()) == next_is_end);
                if (!next_is_end) {
                    REQUIRE(key_at(returned) == next_key);
                }
            }
        } else if (op == "erase_range") {
            auto const first_index = random.below(container.size() + 1);
            auto const length = std::min(random.below(16), container.size() - first_index);
            auto const first = std::next(container.cbegin(), static_cast<std::ptrdiff_t>(first_index));
            auto const last = std::next(first, static_cast<std::ptrdiff_t>(length));
            std::vector<std::size_t> erased_keys;
            for (auto it = first; it != last; ++it) {
                erased_keys.push_back(key_at(it));
            }
            bool const last_is_end = last == container.cend();
            std::size_t const last_key = last_is_end ? std::size_t{0} : key_at(last);

            auto const returned = container.erase(first, last);

            for (auto const erased_key : erased_keys) {
                REQUIRE(reference.erase(erased_key) == 1);
            }
            REQUIRE((returned == container.end()) == last_is_end);
            if (!last_is_end) {
                REQUIRE(key_at(returned) == last_key);
            }
        } else if (op == "count") {
            REQUIRE(container.count(typename Container::key_type{key, counts}) == reference.count(key));
        } else if (op == "contains") {
            REQUIRE(container.contains(typename Container::key_type{key, counts}) == reference.contains(key));
        } else if (op == "rehash") {
            container.rehash(random.below(2048));
        } else if (op == "reserve") {
            container.reserve(random.below(2048));
        } else if (op == "clear") {
            container.clear();
            reference.clear();
        } else if (op == "copy_construct") {
            auto copy = Container{container};
            REQUIRE(copy.size() == container.size());
            REQUIRE(same_contents(copy, reference, counts));
            REQUIRE(copy == container);
            // go on with the copy, to see that it works as well as the original
            container.swap(copy);
        } else if (op == "copy_assign") {
            auto const direction = random.below(3);
            if (direction == 0) {
                side = container;
                side_reference = reference;
                REQUIRE(side == container);
            } else if (direction == 1) {
                container = side;
                reference = side_reference;
                REQUIRE(container == side);
            } else {
                auto const &alias = container;
                container = alias;
            }
        } else if (op == "move_assign") {
            if (random.below(2) == 0) {
                side = std::move(container);
                side_reference = std::move(reference);
                container.clear();  // NOLINT(bugprone-use-after-move)
                reference.clear();  // NOLINT(bugprone-use-after-move)
            } else {
                container = std::move(side);
                reference = std::move(side_reference);
                side.clear();            // NOLINT(bugprone-use-after-move)
                side_reference.clear();  // NOLINT(bugprone-use-after-move)
            }
        } else if (op == "swap") {
            if (random.below(2) == 0) {
                container.swap(side);
            } else {
                swap(container, side);
            }
            reference.swap(side_reference);
        } else if (op == "merge") {
            // the elements of `side` whose keys are not in `container` move over, the others stay in `side`
            container.merge(side);
            reference.merge(side_reference);
            REQUIRE(side.size() == side_reference.size());
        } else if (op == "erase_if") {
            auto const divisor = random.below(4) + 2;
            auto const pred = [divisor](auto const &element) {
                return key_of(element) % divisor == 0;
            };
            REQUIRE(erase_if(container, pred) == std::erase_if(reference, pred));
        } else if (op == "fill_side") {
            auto const num = random.below(8) + 1;
            for (std::size_t i = 0; i < num; ++i) {
                auto const side_key = random.below(key_range);
                auto const side_value = random.below(1000000);
                insert_into(side, side_reference, side_key, side_value);
            }
            REQUIRE(side.size() == side_reference.size());
        } else {
            return false;
        }
        return true;
    }

    template<typename Map, std::size_t num_ranges>
    void run_map_test(std::uint64_t seed, std::array<std::size_t, num_ranges> const &key_ranges) {
        auto counts = counter{};
        {
            auto map = Map{};
            auto side = Map{};
            reference_map reference;
            reference_map side_reference;
            auto random = random_source{seed};
            std::size_t key_range = key_ranges.front();

            auto const obj = [&counts](std::size_t data) {
                return typename Map::key_type{data, counts};
            };
            auto const insert_into = [&obj](Map &target, reference_map &target_reference, std::size_t key, std::size_t value) {
                target.insert_or_assign(obj(key), obj(value));
                target_reference.insert_or_assign(key, value);
            };

            for (std::size_t step = 0; step < steps_per_seed; ++step) {
                if (step % phase_length == 0) {
                    key_range = key_ranges[random.below(num_ranges)];
                }
                auto const key = random.below(key_range);
                auto const value = random.below(1000000);
                auto const op = pick(random, map_ops);
                INFO("seed ", seed, ", step ", step, ", operation ", std::string{op}, ", key ", key);

                if (op == "insert") {
                    auto const [it, inserted] = map.insert(typename Map::value_type{obj(key), obj(value)});
                    auto const [reference_it, reference_inserted] = reference.insert({key, value});
                    REQUIRE(inserted == reference_inserted);
                    REQUIRE(it->first.get() == key);
                    REQUIRE(it->second.get() == reference_it->second);
                } else if (op == "emplace") {
                    auto const [it, inserted] = map.emplace(obj(key), obj(value));
                    auto const [reference_it, reference_inserted] = reference.emplace(key, value);
                    REQUIRE(inserted == reference_inserted);
                    REQUIRE(it->first.get() == key);
                    REQUIRE(it->second.get() == reference_it->second);
                } else if (op == "try_emplace") {
                    auto const [it, inserted] = map.try_emplace(obj(key), value, counts);
                    auto const [reference_it, reference_inserted] = reference.try_emplace(key, value);
                    REQUIRE(inserted == reference_inserted);
                    REQUIRE(it->first.get() == key);
                    REQUIRE(it->second.get() == reference_it->second);
                } else if (op == "insert_or_assign") {
                    auto const [it, inserted] = map.insert_or_assign(obj(key), obj(value));
                    auto const [reference_it, reference_inserted] = reference.insert_or_assign(key, value);
                    REQUIRE(inserted == reference_inserted);
                    REQUIRE(it->first.get() == key);
                    REQUIRE(it->second.get() == value);
                } else if (op == "subscript") {
                    map[obj(key)] = obj(value);
                    reference[key] = value;
                    REQUIRE(map.at(obj(key)).get() == value);
                } else if (op == "find") {
                    auto const reference_it = reference.find(key);
                    if (random.below(2) == 0) {
                        auto const it = map.find(obj(key));
                        REQUIRE((it != map.end()) == (reference_it != reference.end()));
                        if (it != map.end()) {
                            REQUIRE(it->second.get() == reference_it->second);
                        }
                    } else {
                        auto const &const_map = map;
                        auto const it = const_map.find(obj(key));
                        REQUIRE((it != const_map.end()) == (reference_it != reference.end()));
                        if (it != const_map.end()) {
                            REQUIRE(it->second.get() == reference_it->second);
                        }
                    }
                } else if (op == "at") {
                    auto const reference_it = reference.find(key);
                    auto const &const_map = map;
                    if (reference_it != reference.end()) {
                        REQUIRE(map.at(obj(key)).get() == reference_it->second);
                        REQUIRE(const_map.at(obj(key)).get() == reference_it->second);
                    } else {
                        REQUIRE_THROWS_AS(static_cast<void>(map.at(obj(key))), std::out_of_range);
                        REQUIRE_THROWS_AS(static_cast<void>(const_map.at(obj(key))), std::out_of_range);
                    }
                } else {
                    REQUIRE(run_common_op(op, random, key, key_range, counts, map, reference, side, side_reference, insert_into));
                }

                REQUIRE(map.size() == reference.size());
                REQUIRE(map.empty() == reference.empty());
                if (step % full_check_interval == 0) {
                    REQUIRE(same_contents(map, reference, counts));
                    REQUIRE(same_contents(side, side_reference, counts));
                }
            }
            REQUIRE(same_contents(map, reference, counts));
            REQUIRE(same_contents(side, side_reference, counts));
        }
        CHECK(counts.dtor() + counter::static_dtor == counts.ctor() + counter::static_ctor + counts.copy_ctor() + counts.move_ctor());
    }

    template<typename Set, std::size_t num_ranges>
    void run_set_test(std::uint64_t seed, std::array<std::size_t, num_ranges> const &key_ranges) {
        auto counts = counter{};
        {
            auto set = Set{};
            auto side = Set{};
            reference_set reference;
            reference_set side_reference;
            auto random = random_source{seed};
            std::size_t key_range = key_ranges.front();

            auto const insert_into = [&counts](Set &target, reference_set &target_reference, std::size_t key, std::size_t /*value*/) {
                target.insert(typename Set::key_type{key, counts});
                target_reference.insert(key);
            };

            for (std::size_t step = 0; step < steps_per_seed; ++step) {
                if (step % phase_length == 0) {
                    key_range = key_ranges[random.below(num_ranges)];
                }
                auto const key = random.below(key_range);
                auto const op = pick(random, set_ops);
                INFO("seed ", seed, ", step ", step, ", operation ", std::string{op}, ", key ", key);

                if (op == "insert") {
                    auto const [it, inserted] = set.insert(typename Set::key_type{key, counts});
                    REQUIRE(inserted == reference.insert(key).second);
                    REQUIRE(it->get() == key);
                } else if (op == "emplace") {
                    auto const [it, inserted] = set.emplace(key, counts);
                    REQUIRE(inserted == reference.emplace(key).second);
                    REQUIRE(it->get() == key);
                } else if (op == "find") {
                    bool const in_reference = reference.contains(key);
                    if (random.below(2) == 0) {
                        auto const it = set.find(typename Set::key_type{key, counts});
                        REQUIRE((it != set.end()) == in_reference);
                        if (it != set.end()) {
                            REQUIRE(it->get() == key);
                        }
                    } else {
                        auto const &const_set = set;
                        auto const it = const_set.find(typename Set::key_type{key, counts});
                        REQUIRE((it != const_set.end()) == in_reference);
                        if (it != const_set.end()) {
                            REQUIRE(it->get() == key);
                        }
                    }
                } else {
                    REQUIRE(run_common_op(op, random, key, key_range, counts, set, reference, side, side_reference, insert_into));
                }

                REQUIRE(set.size() == reference.size());
                REQUIRE(set.empty() == reference.empty());
                if (step % full_check_interval == 0) {
                    REQUIRE(same_contents(set, reference, counts));
                    REQUIRE(same_contents(side, side_reference, counts));
                }
            }
            REQUIRE(same_contents(set, reference, counts));
            REQUIRE(same_contents(side, side_reference, counts));
        }
        CHECK(counts.dtor() + counter::static_dtor == counts.ctor() + counter::static_ctor + counts.copy_ctor() + counts.move_ctor());
    }

}  // namespace

TEST_CASE_MAP("a map agrees with std::unordered_map, with std::hash", counter::obj, counter::obj) {
    for (auto const seed : seeds) {
        run_map_test<map_t>(seed, spread_key_ranges);
    }
}

TEST_CASE_SET("a set agrees with std::unordered_set, with std::hash", counter::obj) {
    for (auto const seed : seeds) {
        run_set_test<set_t>(seed, spread_key_ranges);
    }
}

namespace {
    /// a map whose elements have a move constructor that can throw, so that its groups have holes
    template<dice::sparse_map::sh::sparsity Sparsity, typename Hash = test_hash<counter::obj>>
    using copied_map = dice::sparse_map::sparse_map<copied_obj,
                                                    copied_obj,
                                                    Hash,
                                                    std::equal_to<counter::obj>,
                                                    std::allocator<std::pair<copied_obj, copied_obj>>,
                                                    Sparsity>;

    /// a set whose elements have a move constructor that can throw, so that its groups have holes
    template<dice::sparse_map::sh::sparsity Sparsity, typename Hash = test_hash<counter::obj>>
    using copied_set = dice::sparse_map::sparse_set<copied_obj,
                                                    Hash,
                                                    std::equal_to<counter::obj>,
                                                    std::allocator<copied_obj>,
                                                    Sparsity>;

    namespace sh = dice::sparse_map::sh;
}  // namespace

TEST_CASE_TEMPLATE("a map of elements whose move constructor can throw agrees with std::unordered_map, with test_hash",
                   map_t,
                   copied_map<sh::sparsity::high>,
                   copied_map<sh::sparsity::medium>,
                   copied_map<sh::sparsity::low>) {
    for (auto const seed : seeds) {
        run_map_test<map_t>(seed, spread_key_ranges);
    }
}

TEST_CASE_TEMPLATE("a map of elements whose move constructor can throw agrees with std::unordered_map, with a colliding hash",
                   map_t,
                   copied_map<sh::sparsity::medium, colliding_hash>) {
    for (auto const seed : seeds) {
        run_map_test<map_t>(seed, colliding_key_ranges);
    }
}

TEST_CASE_TEMPLATE("a set of elements whose move constructor can throw agrees with std::unordered_set",
                   set_t,
                   copied_set<sh::sparsity::medium>,
                   copied_set<sh::sparsity::medium, colliding_hash>) {
    for (auto const seed : seeds) {
        if constexpr (std::is_same_v<typename set_t::hasher, colliding_hash>) {
            run_set_test<set_t>(seed, colliding_key_ranges);
        } else {
            run_set_test<set_t>(seed, spread_key_ranges);
        }
    }
}
