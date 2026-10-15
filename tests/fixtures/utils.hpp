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
#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_UTILS_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_UTILS_HPP

#include <boost/numeric/conversion/cast.hpp>

#include <cstddef>
#include <cstdint>
#include <functional>
#include <memory>
#include <ostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <utility>
#include <vector>

/**
 * Key and value types, hash functions and helpers for the tests that come from tsl::sparse_map.
 */
namespace dice::sparse_map::tests {

    /**
     * Hash that returns the value itself.
     */
    template<typename T>
    struct identity_hash {
        std::size_t operator()(T const &value) const {
            return static_cast<std::size_t>(value);
        }
    };

    /**
     * `std::hash` modulo `mod`. A small `mod` gives many collisions.
     */
    template<unsigned int mod>
    struct mod_hash {
        template<typename T>
        std::size_t operator()(T const &value) const {
            return std::hash<T>()(value) % mod;
        }
    };

    /**
     * Value with a pointer to its own member. Copies and moves point it to the member of the new object.
     * The move constructor copies and is not `noexcept`.
     */
    struct self_reference_member_test {
        self_reference_member_test()
            : value_(std::to_string(-1)),
              value_ptr_(&value_) {
        }

        explicit self_reference_member_test(std::int64_t value)
            : value_(std::to_string(value)),
              value_ptr_(&value_) {
        }

        self_reference_member_test(self_reference_member_test const &other)
            : value_(*other.value_ptr_),
              value_ptr_(&value_) {
        }

        self_reference_member_test(self_reference_member_test &&other)
            : value_(*other.value_ptr_),
              value_ptr_(&value_) {
        }

        self_reference_member_test &operator=(self_reference_member_test const &other) {
            value_ = *other.value_ptr_;
            value_ptr_ = &value_;

            return *this;
        }

        self_reference_member_test &operator=(self_reference_member_test &&other) {
            value_ = *other.value_ptr_;
            value_ptr_ = &value_;

            return *this;
        }

        std::string value() const {
            return *value_ptr_;
        }

        friend std::ostream &operator<<(std::ostream &stream, self_reference_member_test const &value) {
            stream << *value.value_ptr_;

            return stream;
        }

        friend bool operator==(self_reference_member_test const &lhs, self_reference_member_test const &rhs) {
            return *lhs.value_ptr_ == *rhs.value_ptr_;
        }

        friend bool operator!=(self_reference_member_test const &lhs, self_reference_member_test const &rhs) {
            return !(lhs == rhs);
        }

        friend bool operator<(self_reference_member_test const &lhs, self_reference_member_test const &rhs) {
            return *lhs.value_ptr_ < *rhs.value_ptr_;
        }

    private:
        std::string value_;
        std::string *value_ptr_;
    };

    /**
     * Value that can be moved but not copied. A moved from object holds no value.
     */
    struct move_only_test {
        explicit move_only_test(std::int64_t value)
            : value_(new std::string(std::to_string(value))) {
        }

        explicit move_only_test(std::string value)
            : value_(new std::string(std::move(value))) {
        }

        move_only_test(move_only_test const &) = delete;
        move_only_test(move_only_test &&) = default;
        move_only_test &operator=(move_only_test const &) = delete;
        move_only_test &operator=(move_only_test &&) = default;

        friend std::ostream &operator<<(std::ostream &stream, move_only_test const &value) {
            if (value.value_ == nullptr) {
                stream << "null";
            } else {
                stream << *value.value_;
            }

            return stream;
        }

        friend bool operator==(move_only_test const &lhs, move_only_test const &rhs) {
            if (lhs.value_ == nullptr || rhs.value_ == nullptr) {
                return lhs.value_ == nullptr && rhs.value_ == nullptr;
            } else {
                return *lhs.value_ == *rhs.value_;
            }
        }

        friend bool operator!=(move_only_test const &lhs, move_only_test const &rhs) {
            return !(lhs == rhs);
        }

        friend bool operator<(move_only_test const &lhs, move_only_test const &rhs) {
            if (lhs.value_ == nullptr && rhs.value_ == nullptr) {
                return false;
            } else if (lhs.value_ == nullptr) {
                return true;
            } else if (rhs.value_ == nullptr) {
                return false;
            } else {
                return *lhs.value_ < *rhs.value_;
            }
        }

        std::string const &value() const {
            return *value_;
        }

    private:
        std::unique_ptr<std::string> value_;
    };

    /**
     * Value that can be copied but has no move constructor, so a move copies.
     */
    struct copy_only_test {
        explicit copy_only_test(std::int64_t value)
            : value_(std::to_string(value)) {
        }

        copy_only_test(copy_only_test const &other)
            : value_(other.value_) {
        }

        copy_only_test &operator=(copy_only_test const &other) {
            value_ = other.value_;

            return *this;
        }

        ~copy_only_test() {
        }

        friend std::ostream &operator<<(std::ostream &stream, copy_only_test const &value) {
            stream << value.value_;

            return stream;
        }

        friend bool operator==(copy_only_test const &lhs, copy_only_test const &rhs) {
            return lhs.value_ == rhs.value_;
        }

        friend bool operator!=(copy_only_test const &lhs, copy_only_test const &rhs) {
            return !(lhs == rhs);
        }

        friend bool operator<(copy_only_test const &lhs, copy_only_test const &rhs) {
            return lhs.value_ < rhs.value_;
        }

        std::string value() const {
            return value_;
        }

    private:
        std::string value_;
    };

}  // namespace dice::sparse_map::tests

namespace std {
    template<>
    struct hash<dice::sparse_map::tests::self_reference_member_test> {
        std::size_t operator()(dice::sparse_map::tests::self_reference_member_test const &val) const {
            return std::hash<std::string>()(val.value());
        }
    };

    template<>
    struct hash<dice::sparse_map::tests::move_only_test> {
        std::size_t operator()(dice::sparse_map::tests::move_only_test const &val) const {
            return std::hash<std::string>()(val.value());
        }
    };

    template<>
    struct hash<dice::sparse_map::tests::copy_only_test> {
        std::size_t operator()(dice::sparse_map::tests::copy_only_test const &val) const {
            return std::hash<std::string>()(val.value());
        }
    };
}  // namespace std

namespace dice::sparse_map::tests {

    /**
     * Keys and values for the tests. `get_key<T>(i)` and `get_value<T>(i)` return the i-th key and value of type `T`.
     * `get_filled_hash_map<HMap>(n)` returns a map with the first `n` keys and values.
     */
    struct utils {
        template<typename T>
        static T get_key(std::size_t counter);

        template<typename T>
        static T get_value(std::size_t counter);

        template<typename HMap>
        static HMap get_filled_hash_map(std::size_t nb_elements);
    };

    template<>
    inline std::int64_t utils::get_key<std::int64_t>(std::size_t counter) {
        return boost::numeric_cast<std::int64_t>(counter);
    }

    template<>
    inline self_reference_member_test utils::get_key<self_reference_member_test>(std::size_t counter) {
        return self_reference_member_test(boost::numeric_cast<std::int64_t>(counter));
    }

    template<>
    inline std::string utils::get_key<std::string>(std::size_t counter) {
        return "Key " + std::to_string(counter);
    }

    template<>
    inline move_only_test utils::get_key<move_only_test>(std::size_t counter) {
        return move_only_test(boost::numeric_cast<std::int64_t>(counter));
    }

    template<>
    inline copy_only_test utils::get_key<copy_only_test>(std::size_t counter) {
        return copy_only_test(boost::numeric_cast<std::int64_t>(counter));
    }

    template<>
    inline std::int64_t utils::get_value<std::int64_t>(std::size_t counter) {
        return boost::numeric_cast<std::int64_t>(counter * 2);
    }

    template<>
    inline self_reference_member_test utils::get_value<self_reference_member_test>(std::size_t counter) {
        return self_reference_member_test(boost::numeric_cast<std::int64_t>(counter * 2));
    }

    template<>
    inline std::string utils::get_value<std::string>(std::size_t counter) {
        return "Value " + std::to_string(counter);
    }

    template<>
    inline move_only_test utils::get_value<move_only_test>(std::size_t counter) {
        return move_only_test(boost::numeric_cast<std::int64_t>(counter * 2));
    }

    template<>
    inline copy_only_test utils::get_value<copy_only_test>(std::size_t counter) {
        return copy_only_test(boost::numeric_cast<std::int64_t>(counter * 2));
    }

    template<typename HMap>
    inline HMap utils::get_filled_hash_map(std::size_t nb_elements) {
        using key_t = typename HMap::key_type;
        using value_t = typename HMap::mapped_type;

        HMap map;
        map.reserve(nb_elements);

        for (std::size_t i = 0; i < nb_elements; i++) {
            map.insert({utils::get_key<key_t>(i), utils::get_value<value_t>(i)});
        }

        return map;
    }

    template<typename T>
    struct is_pair : std::false_type {};

    template<typename T1, typename T2>
    struct is_pair<std::pair<T1, T2>> : std::true_type {};

    /**
     * Serializer for the `serialize` functions of the containers. It writes into a string.
     */
    struct serializer {
        serializer() {
            ostream_.exceptions(ostream_.badbit | ostream_.failbit);
        }

        template<typename T>
        void operator()(T const &val) {
            serialize_impl(val);
        }

        std::string str() const {
            return ostream_.str();
        }

    private:
        template<typename T, typename U>
        void serialize_impl(std::pair<T, U> const &val) {
            serialize_impl(val.first);
            serialize_impl(val.second);
        }

        void serialize_impl(std::string const &val) {
            serialize_impl(boost::numeric_cast<std::uint64_t>(val.size()));
            ostream_.write(val.data(), val.size());
        }

        void serialize_impl(move_only_test const &val) {
            serialize_impl(val.value());
        }

        template<typename T, typename std::enable_if<std::is_arithmetic<T>::value>::type * = nullptr>
        void serialize_impl(T const &val) {
            ostream_.write(reinterpret_cast<char const *>(&val), sizeof(val));
        }

        std::stringstream ostream_;
    };

    /**
     * Deserializer for the `deserialize` functions of the containers. It reads what `serializer` wrote.
     */
    struct deserializer {
        deserializer(std::string const &init_str = "")
            : istream_(init_str) {
            istream_.exceptions(istream_.badbit | istream_.failbit | istream_.eofbit);
        }

        template<typename T>
        T operator()() {
            return deserialize_impl<T>();
        }

    private:
        template<typename T, typename std::enable_if<is_pair<T>::value>::type * = nullptr>
        T deserialize_impl() {
            auto first = deserialize_impl<typename T::first_type>();
            return std::make_pair(std::move(first), deserialize_impl<typename T::second_type>());
        }

        template<typename T, typename std::enable_if<std::is_same<std::string, T>::value>::type * = nullptr>
        T deserialize_impl() {
            std::size_t const str_size = boost::numeric_cast<std::size_t>(deserialize_impl<std::uint64_t>());

            // TODO std::string::data() return a const pointer pre-C++17. Avoid the
            // inefficient double allocation.
            std::vector<char> chars(str_size);
            istream_.read(chars.data(), str_size);

            return std::string(chars.data(), chars.size());
        }

        template<typename T, typename std::enable_if<std::is_same<move_only_test, T>::value>::type * = nullptr>
        move_only_test deserialize_impl() {
            return move_only_test(deserialize_impl<std::string>());
        }

        template<typename T, typename std::enable_if<std::is_arithmetic<T>::value>::type * = nullptr>
        T deserialize_impl() {
            T val;
            istream_.read(reinterpret_cast<char *>(&val), sizeof(val));

            return val;
        }

        std::stringstream istream_;
    };

}  // namespace dice::sparse_map::tests

#endif  // DICE_SPARSE_MAP_TESTS_FIXTURES_UTILS_HPP
