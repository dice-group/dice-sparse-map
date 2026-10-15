// DiceHash with the policy Martinus declares no is_avalanching, so the map does not compile with it.
#include <dice/sparse-map/sparse_map.hpp>

int main() {
    dice::sparse_map::sparse_map<int, int, dice::hash::DiceHash<int, dice::hash::Policies::Martinus>> map;
    return static_cast<int>(map.size());
}
