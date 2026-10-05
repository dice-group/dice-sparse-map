// A key type with its own dice_hash_overload that does not declare is_avalanching does not compile with the default
// hash function.
#include <dice/sparse-map/sparse_map.hpp>

#include <cstddef>
#include <cstdint>

struct node_id {
    std::uint64_t value;

    bool operator==(node_id const &other) const = default;
};

namespace dice::hash {
    template<typename Policy>
    struct dice_hash_overload<Policy, node_id> {
        static std::size_t dice_hash(node_id const &id) noexcept {
            return dice_hash_templates<Policy>::dice_hash(id.value);
        }
    };
}  // namespace dice::hash

int main() {
    dice::sparse_map::sparse_map<node_id, int> map;
    return static_cast<int>(map.size());
}
