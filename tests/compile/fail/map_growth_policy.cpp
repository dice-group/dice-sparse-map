// The map accepts only power_of_two_growth_policy<2> as GrowthPolicy.
#include <dice/sparse-map/sparse_map.hpp>

#include <functional>
#include <memory>
#include <utility>

int main() {
    dice::sparse_map::sparse_map<int,
                                 int,
                                 dice::hash::DiceHash<int, dice::hash::Policies::wyhash>,
                                 std::equal_to<int>,
                                 std::allocator<std::pair<int, int>>,
                                 dice::sparse_map::sh::power_of_two_growth_policy<4>>
        map;
    return static_cast<int>(map.size());
}
