#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

int main() {
    dice::sparse_map::sparse_map<int, int> map;
    map[1] = 1;
    map.find(1)->second = 2;

    dice::sparse_map::sparse_set<int> set{1, 2, 3};

    return (map.lookup(1).value_or(0) == 2 && set.contains(3)) ? 0 : 1;
}
