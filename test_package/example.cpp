#include <dice/sparse-map/sparse_map.hpp>

int main() {
    dice::sparse_map::sparse_map<int, int> x;
    x[1] = 1;
}