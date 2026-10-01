#ifndef DICE_SPARSE_MAP_OPTIONAL_REF_HPP
#define DICE_SPARSE_MAP_OPTIONAL_REF_HPP

#include <concepts>
#include <functional>
#include <memory>
#include <optional>
#include <type_traits>
#include <utility>

namespace dice::sparse_map {

#if defined(__cpp_lib_optional) && __cpp_lib_optional >= 202506L

    /**
     * The result type of `sparse_map::lookup`: `std::optional<T &>`, which the standard library provides.
     */
    template<typename T>
    using optional_ref = std::optional<T &>;

#else

    /**
     * The result type of `sparse_map::lookup`. A reference to a `T` or nothing.
     *
     * It has the interface of `std::optional<T &>` (C++26), which this standard library does not provide yet.
     * With a standard library that provides it, `optional_ref<T>` is `std::optional<T &>`.
     */
    template<typename T>
    struct optional_ref {
        using value_type = T;
        using iterator = T *;

        constexpr optional_ref() noexcept = default;

        constexpr optional_ref(std::nullopt_t /*nullopt*/) noexcept {  // NOLINT(google-explicit-constructor)
        }

        constexpr optional_ref(T &ref) noexcept  // NOLINT(google-explicit-constructor)
            : ptr_(std::addressof(ref)) {
        }

        /**
         * Does not bind a temporary, like `std::optional<T &>`.
         */
        optional_ref(std::remove_cv_t<T> &&) = delete;
        optional_ref(std::remove_cv_t<T> const &&) = delete;

        /**
         * Converts e.g. `optional_ref<T>` into `optional_ref<T const>`.
         */
        template<typename U>
        requires (!std::is_same_v<U, T> && std::is_convertible_v<U *, T *>)
        constexpr optional_ref(optional_ref<U> const &other) noexcept  // NOLINT(google-explicit-constructor)
            : ptr_(other.has_value() ? std::addressof(*other) : nullptr) {
        }

        [[nodiscard]] constexpr bool has_value() const noexcept {
            return ptr_ != nullptr;
        }

        constexpr explicit operator bool() const noexcept {
            return has_value();
        }

        [[nodiscard]] constexpr T &operator*() const noexcept {
            return *ptr_;
        }

        [[nodiscard]] constexpr T *operator->() const noexcept {
            return ptr_;
        }

        /**
         * @throws std::bad_optional_access if there is no value
         */
        [[nodiscard]] constexpr T &value() const {
            if (!has_value()) {
                throw std::bad_optional_access{};
            }
            return *ptr_;
        }

        /**
         * @return a copy of the referenced value, or `default_value` converted to `std::remove_cv_t<T>`
         */
        template<typename U = std::remove_cv_t<T>>
        [[nodiscard]] constexpr std::remove_cv_t<T> value_or(U &&default_value) const {
            if (has_value()) {
                return *ptr_;
            }
            return static_cast<std::remove_cv_t<T>>(std::forward<U>(default_value));
        }

        /**
         * @return `f(value())` if there is a value, otherwise an empty value of the result type of `f`
         */
        template<typename F>
        constexpr auto and_then(F &&f) const {
            using result_type = std::remove_cvref_t<std::invoke_result_t<F, T &>>;
            if (has_value()) {
                return std::invoke(std::forward<F>(f), *ptr_);
            }
            return result_type{};
        }

        /**
         * @return `f(value())` wrapped in an optional if there is a value, otherwise an empty optional.
         * If `f` returns an lvalue reference, the result is an `optional_ref`, otherwise a `std::optional`.
         */
        template<typename F>
        constexpr auto transform(F &&f) const {
            using result_type = std::invoke_result_t<F, T &>;
            if constexpr (std::is_lvalue_reference_v<result_type>) {
                using optional_type = optional_ref<std::remove_reference_t<result_type>>;
                if (has_value()) {
                    return optional_type{std::invoke(std::forward<F>(f), *ptr_)};
                }
                return optional_type{};
            } else {
                using optional_type = std::optional<std::remove_cv_t<result_type>>;
                if (has_value()) {
                    return optional_type{std::invoke(std::forward<F>(f), *ptr_)};
                }
                return optional_type{};
            }
        }

        /**
         * @return `*this` if there is a value, otherwise `f()`
         */
        template<typename F>
        requires std::same_as<std::remove_cvref_t<std::invoke_result_t<F>>, optional_ref>
        constexpr optional_ref or_else(F &&f) const {
            if (has_value()) {
                return *this;
            }
            return std::invoke(std::forward<F>(f));
        }

        constexpr T &emplace(T &ref) noexcept {
            ptr_ = std::addressof(ref);
            return ref;
        }

        /**
         * Does not bind a temporary, like `std::optional<T &>`.
         */
        T &emplace(std::remove_cv_t<T> &&) = delete;
        T &emplace(std::remove_cv_t<T> const &&) = delete;

        constexpr void reset() noexcept {
            ptr_ = nullptr;
        }

        constexpr void swap(optional_ref &other) noexcept {
            std::swap(ptr_, other.ptr_);
        }

        /**
         * Iterates over the value, if there is one.
         */
        [[nodiscard]] constexpr iterator begin() const noexcept {
            return ptr_;
        }

        [[nodiscard]] constexpr iterator end() const noexcept {
            return ptr_ == nullptr ? nullptr : ptr_ + 1;
        }

        friend constexpr bool operator==(optional_ref const &lhs, std::nullopt_t /*nullopt*/) noexcept {
            return !lhs.has_value();
        }

        /**
         * Compares the referenced values, like `std::optional` does. Two empty values are equal.
         */
        template<typename U>
        friend constexpr bool operator==(optional_ref const &lhs, optional_ref<U> const &rhs) {
            if (lhs.has_value() != rhs.has_value()) {
                return false;
            }
            return !lhs.has_value() || *lhs == *rhs;
        }

    private:
        T *ptr_ = nullptr;
    };

#endif

}  // namespace dice::sparse_map

#endif  // DICE_SPARSE_MAP_OPTIONAL_REF_HPP
