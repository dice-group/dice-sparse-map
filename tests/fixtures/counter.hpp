#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_COUNTER_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_COUNTER_HPP

#include <cstddef>
#include <cstring>
#include <functional>
#include <iosfwd>
#include <string>
#include <string_view>
#include <type_traits>

namespace dice::sparse_map::tests {

    /**
     * Counts what a container does with its elements: constructions, copies, moves, destructions,
     * comparisons and hash calls. Each `counter::obj` reports to the `counter` it was created with.
     * The destructor of `counter` aborts if an object is still alive or if the number of
     * destructions does not match the number of constructions.
     *
     * Ported from the test fixtures of ankerl::unordered_dense (MIT license).
     */
    struct counter {
        struct data_t {
            std::size_t ctor{};
            std::size_t default_ctor{};
            std::size_t copy_ctor{};
            std::size_t dtor{};
            std::size_t assign{};
            std::size_t swaps{};
            std::size_t get{};
            std::size_t const_get{};
            std::size_t hash{};
            std::size_t equals{};
            std::size_t less{};
            std::size_t move_ctor{};
            std::size_t move_assign{};

            friend bool operator==(data_t const &a, data_t const &b) noexcept {
                static_assert(std::has_unique_object_representations_v<data_t>);
                return 0 == std::memcmp(&a, &b, sizeof(data_t));
            }
        };

        counter(counter const &) = delete;
        counter(counter &&) = delete;
        counter &operator=(counter const &) = delete;
        counter &operator=(counter &&) = delete;

        /**
         * An element type that reports every operation to a `counter`.
         * A default constructed `obj` has no counter, its construction and destruction go to
         * `counter::static_ctor` and `counter::static_dtor`.
         */
        struct obj {
            obj();
            obj(std::size_t const &data, counter &counts);
            obj(obj const &o);
            obj(obj &&o) noexcept;
            ~obj();

            bool operator==(obj const &o) const;
            bool operator<(obj const &o) const;
            obj &operator=(obj const &o);
            obj &operator=(obj &&o) noexcept;

            [[nodiscard]] std::size_t const &get() const;
            std::size_t &get();

            [[nodiscard]] counter &counts();

            void swap(obj &other);
            [[nodiscard]] std::size_t get_for_hash() const;

        private:
            /**
             * True between the constructor and the destructor of this object. Every operation checks it,
             * so using a destroyed object, or destroying one twice, aborts.
             */
            [[nodiscard]] bool is_alive() const;

            std::size_t data_;
            counter *counts_;
            obj const *alive_;
        };

        counter();
        ~counter();

        void check_all_done() const;

        [[nodiscard]] std::size_t ctor() const noexcept {
            return data_.ctor;
        }
        [[nodiscard]] std::size_t default_ctor() const noexcept {
            return data_.default_ctor;
        }
        [[nodiscard]] std::size_t copy_ctor() const noexcept {
            return data_.copy_ctor;
        }
        [[nodiscard]] std::size_t dtor() const noexcept {
            return data_.dtor;
        }
        [[nodiscard]] std::size_t equals() const noexcept {
            return data_.equals;
        }
        [[nodiscard]] std::size_t less() const noexcept {
            return data_.less;
        }
        [[nodiscard]] std::size_t assign() const noexcept {
            return data_.assign;
        }
        [[nodiscard]] std::size_t swaps() const noexcept {
            return data_.swaps;
        }
        [[nodiscard]] std::size_t get() const noexcept {
            return data_.get;
        }
        [[nodiscard]] std::size_t const_get() const noexcept {
            return data_.const_get;
        }
        [[nodiscard]] std::size_t hash() const noexcept {
            return data_.hash;
        }
        [[nodiscard]] std::size_t move_ctor() const noexcept {
            return data_.move_ctor;
        }
        [[nodiscard]] std::size_t move_assign() const noexcept {
            return data_.move_assign;
        }
        [[nodiscard]] data_t const &data() const noexcept {
            return data_;
        }

        /**
         * Appends a line with all current counts and `title` to the record that `operator<<` prints.
         */
        void operator()(std::string_view title);

        [[nodiscard]] std::size_t total() const;

        friend std::ostream &operator<<(std::ostream &os, counter const &c);

        /// constructions of objects without a counter: default constructed, or copied or moved from such an object
        static std::size_t static_ctor;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
        static std::size_t static_dtor;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    private:
        data_t data_{};
        std::string records_ = "\n     ctor  defctor  cpyctor     dtor   assign    swaps      get  cnstget     hash   equals     less   ctormv assignmv|   total |\n";
    };

    /**
     * The hash of a `counter::obj` is its number, so key `k` lands in bucket `k` of a table with more than `k`
     * buckets. It is not avalanching, but it is marked as avalanching, so that the containers accept it.
     */
    struct bucket_hash {
        using is_avalanching = void;

        std::size_t operator()(counter::obj const &key) const {
            return key.get_for_hash();
        }
    };

    /**
     * `counter::obj` whose move constructor can throw, so that a container copies it where it would move a
     * `counter::obj`. Its move operations copy. Hash it like a `counter::obj`, for example with
     * `test_hash<counter::obj>`, and compare it with `std::equal_to<counter::obj>`.
     */
    struct copied_obj : counter::obj {
        using counter::obj::obj;

        copied_obj(copied_obj const &other) = default;

        copied_obj(copied_obj &&other) noexcept(false)  // NOLINT(performance-noexcept-move-constructor)
            : counter::obj(static_cast<counter::obj const &>(other)) {
        }

        copied_obj &operator=(copied_obj const &other) = default;

        copied_obj &operator=(copied_obj &&other) noexcept(false) {  // NOLINT(performance-noexcept-move-constructor)
            counter::obj::operator=(static_cast<counter::obj const &>(other));
            return *this;
        }

        ~copied_obj() = default;
    };

}  // namespace dice::sparse_map::tests

template<>
struct std::hash<dice::sparse_map::tests::counter::obj> {
    [[nodiscard]] std::size_t operator()(dice::sparse_map::tests::counter::obj const &c) const noexcept {
        return std::hash<std::size_t>{}(c.get_for_hash());
    }
};

#endif  // DICE_SPARSE_MAP_TESTS_FIXTURES_COUNTER_HPP
