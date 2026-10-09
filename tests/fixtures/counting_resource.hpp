#ifndef DICE_SPARSE_MAP_TESTS_FIXTURES_COUNTING_RESOURCE_HPP
#define DICE_SPARSE_MAP_TESTS_FIXTURES_COUNTING_RESOURCE_HPP

#include <cstddef>
#include <memory_resource>

namespace dice::sparse_map::tests {

    /**
     * Memory resource that counts the requests it gets and passes them on to
     * `std::pmr::new_delete_resource()`. It also counts requests that a container should never make:
     * allocations of 0 bytes, and deallocations of a null pointer or of 0 bytes.
     * Two instances compare equal only if they are the same object.
     */
    struct counting_resource : std::pmr::memory_resource {
        [[nodiscard]] std::size_t allocations() const noexcept {
            return allocations_;
        }

        [[nodiscard]] std::size_t deallocations() const noexcept {
            return deallocations_;
        }

        /// bytes that were allocated and not deallocated yet
        [[nodiscard]] std::size_t bytes_in_use() const noexcept {
            return bytes_in_use_;
        }

        /// allocations of 0 bytes, and deallocations of a null pointer or of 0 bytes
        [[nodiscard]] std::size_t empty_requests() const noexcept {
            return empty_requests_;
        }

    private:
        void *do_allocate(std::size_t bytes, std::size_t alignment) override {
            if (bytes == 0) {
                ++empty_requests_;
            }
            void *p = std::pmr::new_delete_resource()->allocate(bytes, alignment);
            ++allocations_;
            bytes_in_use_ += bytes;
            return p;
        }

        void do_deallocate(void *p, std::size_t bytes, std::size_t alignment) override {
            if (p == nullptr || bytes == 0) {
                ++empty_requests_;
            }
            ++deallocations_;
            bytes_in_use_ -= bytes;
            std::pmr::new_delete_resource()->deallocate(p, bytes, alignment);
        }

        [[nodiscard]] bool do_is_equal(std::pmr::memory_resource const &other) const noexcept override {
            return this == &other;
        }

        std::size_t allocations_ = 0;
        std::size_t deallocations_ = 0;
        std::size_t bytes_in_use_ = 0;
        std::size_t empty_requests_ = 0;
    };

} // namespace dice::sparse_map::tests

#endif // DICE_SPARSE_MAP_TESTS_FIXTURES_COUNTING_RESOURCE_HPP
