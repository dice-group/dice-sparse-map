#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

int main() {
    dice::sparse_map::sparse_map<int, int> map;
    map[1] = 1;
    map.find(1)->second = 2;

    dice::sparse_map::sparse_set<int> set{1, 2, 3};

    auto const it = map.find(1);
    return (it != map.end() && it->second == 2 && set.contains(3)) ? 0 : 1;
}
