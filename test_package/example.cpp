#include <Dice/sparse-map/sparse_map.hpp>

#include <iostream>

int main() {
    Dice::sparse_map::sparse_map<int, int> x;
    x[1] = 1;
}