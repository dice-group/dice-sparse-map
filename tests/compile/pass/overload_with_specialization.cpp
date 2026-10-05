// A key type with its own dice_hash_overload that does not declare is_avalanching compiles with the default hash
// function if the user specializes dice::sparse_map::sh::hash_is_avalanching for it.
#include <dice/sparse-map/sparse_map.hpp>

#include <cstddef>
#include <cstdint>
#include <type_traits>

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

template<>
struct dice::sparse_map::sh::hash_is_avalanching<dice::hash::DiceHash<node_id, dice::hash::Policies::wyhash>>
    : std::true_type {};

int main() {
    dice::sparse_map::sparse_map<node_id, int> map;
    map[node_id{1}] = 1;
    return map.size() == 1 ? 0 : 1;
}
