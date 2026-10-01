#include "fixtures/counter.hpp"

#include <cstdio>
#include <cstdlib>
#include <format>
#include <ostream>
#include <stdexcept>
#include <utility>

namespace dice::sparse_map::tests {

    namespace {
        /**
         * Number of `counter::obj` that are alive. Each object carries its own liveness flag, and only this number
         * is global, so the checks do not depend on where objects are placed in memory.
         */
        std::size_t &num_alive() {
            static std::size_t value{};
            return value;
        }

        [[noreturn]] void fail(char const *where) {
            std::fprintf(stderr, "ERROR in counter::obj: %s\n", where);
            std::fflush(stderr);
            std::abort();
        }
    }  // namespace

    std::size_t counter::static_default_ctor = 0;  // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)
    std::size_t counter::static_dtor = 0;          // NOLINT(cppcoreguidelines-avoid-non-const-global-variables)

    bool counter::obj::is_alive() const {
        return this == alive_;
    }

    counter::obj::obj()
        : data_(0),
          counts_(nullptr),
          alive_(this) {
        ++num_alive();
        ++static_default_ctor;
    }

    counter::obj::obj(std::size_t const &data, counter &counts)
        : data_(data),
          counts_(&counts),
          alive_(this) {
        ++num_alive();
        ++counts_->data_.ctor;
    }

    counter::obj::obj(obj const &o)
        : data_(o.data_),
          counts_(o.counts_),
          alive_(this) {
        if (!o.is_alive()) {
            fail("copy constructor from a dead object");
        }
        ++num_alive();
        if (counts_ != nullptr) {
            ++counts_->data_.copy_ctor;
        }
    }

    counter::obj::obj(obj &&o) noexcept
        : data_(o.data_),
          counts_(o.counts_),
          alive_(this) {
        if (!o.is_alive()) {
            fail("move constructor from a dead object");
        }
        ++num_alive();
        if (counts_ != nullptr) {
            ++counts_->data_.move_ctor;
        }
    }

    counter::obj::~obj() {
        if (!is_alive()) {
            fail("destructor of a dead object");
        }
        alive_ = nullptr;
        --num_alive();
        if (counts_ != nullptr) {
            ++counts_->data_.dtor;
        } else {
            ++static_dtor;
        }
    }

    bool counter::obj::operator==(obj const &o) const {
        if (!is_alive() || !o.is_alive()) {
            fail("operator== on a dead object");
        }
        if (counts_ != nullptr) {
            ++counts_->data_.equals;
        }
        return data_ == o.data_;
    }

    bool counter::obj::operator<(obj const &o) const {
        if (!is_alive() || !o.is_alive()) {
            fail("operator< on a dead object");
        }
        if (counts_ != nullptr) {
            ++counts_->data_.less;
        }
        return data_ < o.data_;
    }

    // NOLINTNEXTLINE(bugprone-unhandled-self-assignment,cert-oop54-cpp)
    counter::obj &counter::obj::operator=(obj const &o) {
        if (!is_alive() || !o.is_alive()) {
            fail("copy assignment with a dead object");
        }
        counts_ = o.counts_;
        if (counts_ != nullptr) {
            ++counts_->data_.assign;
        }
        data_ = o.data_;
        return *this;
    }

    counter::obj &counter::obj::operator=(obj &&o) noexcept {
        if (!is_alive() || !o.is_alive()) {
            fail("move assignment with a dead object");
        }
        if (o.counts_ != nullptr) {
            counts_ = o.counts_;
        }
        data_ = o.data_;
        if (counts_ != nullptr) {
            ++counts_->data_.move_assign;
        }
        return *this;
    }

    std::size_t const &counter::obj::get() const {
        if (counts_ != nullptr) {
            ++counts_->data_.const_get;
        }
        return data_;
    }

    std::size_t &counter::obj::get() {
        if (counts_ != nullptr) {
            ++counts_->data_.get;
        }
        return data_;
    }

    counter &counter::obj::counts() {
        return *counts_;
    }

    void counter::obj::swap(obj &other) {
        if (!is_alive() || !other.is_alive()) {
            fail("swap with a dead object");
        }
        using std::swap;
        swap(data_, other.data_);
        swap(counts_, other.counts_);
        if (counts_ != nullptr) {
            ++counts_->data_.swaps;
        }
    }

    std::size_t counter::obj::get_for_hash() const {
        if (counts_ != nullptr) {
            ++counts_->data_.hash;
        }
        return data_;
    }

    counter::counter() {
        static_default_ctor = 0;
        static_dtor = 0;
    }

    void counter::check_all_done() const {
        if (num_alive() != 0) {
            std::fprintf(stderr, "ERROR at ~counter(): %zu objects still alive\n", num_alive());
            std::abort();
        }
        if (data_.dtor + static_dtor != data_.ctor + static_default_ctor + data_.copy_ctor + data_.default_ctor + data_.move_ctor) {
            std::fprintf(stderr,
                         "ERROR at ~counter(): %zu dtor + %zu static dtor != %zu ctor + %zu static default ctor + %zu copy ctor + %zu default ctor + %zu move ctor\n",
                         data_.dtor, static_dtor, data_.ctor, static_default_ctor, data_.copy_ctor, data_.default_ctor, data_.move_ctor);
            std::abort();
        }
    }

    counter::~counter() {
        check_all_done();
    }

    std::size_t counter::total() const {
        return data_.ctor + static_default_ctor + data_.copy_ctor + (data_.dtor + static_dtor) + data_.equals
               + data_.less + data_.assign + data_.swaps + data_.get + data_.const_get + data_.hash
               + data_.move_ctor + data_.move_assign;
    }

    void counter::operator()(std::string_view title) {
        records_ += std::format("{:9}{:9}{:9}{:9}{:9}{:9}{:9}{:9}{:9}{:9}{:9}{:9}{:9}|{:9}| {}\n",
                                data_.ctor,
                                static_default_ctor,
                                data_.copy_ctor,
                                data_.dtor + static_dtor,
                                data_.assign,
                                data_.swaps,
                                data_.get,
                                data_.const_get,
                                data_.hash,
                                data_.equals,
                                data_.less,
                                data_.move_ctor,
                                data_.move_assign,
                                total(),
                                title);
    }

    std::ostream &operator<<(std::ostream &os, counter const &c) {
        return os << c.records_;
    }

}  // namespace dice::sparse_map::tests
