// The set accepts only power_of_two_growth_policy<2> as GrowthPolicy.
#include <dice/sparse-map/sparse_set.hpp>

#include <functional>
#include <memory>

int main() {
    dice::sparse_map::sparse_set<int,
                                 dice::hash::DiceHash<int, dice::hash::Policies::wyhash>,
                                 std::equal_to<int>,
                                 std::allocator<int>,
                                 dice::sparse_map::sh::power_of_two_growth_policy<4>>
        set;
    return static_cast<int>(set.size());
}
