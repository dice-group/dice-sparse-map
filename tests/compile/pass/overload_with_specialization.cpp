// DiceHash with the policy Martinus declares no is_avalanching. A map with it compiles if the user specializes
// dice::sparse_map::sh::hash_is_avalanching for it.
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
} // namespace dice::hash

template<>
struct dice::sparse_map::sh::hash_is_avalanching<dice::hash::DiceHash<node_id, dice::hash::Policies::Martinus>> :
    std::true_type {};

int main() {
    dice::sparse_map::sparse_map<node_id, int, dice::hash::DiceHash<node_id, dice::hash::Policies::Martinus>> map;
    map[node_id{1}] = 1;
    return map.size() == 1 ? 0 : 1;
}
