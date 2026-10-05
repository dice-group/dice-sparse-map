// std::hash is not marked as avalanching, so the map does not compile with it.
#include <dice/sparse-map/sparse_map.hpp>

#include <functional>

int main() {
    dice::sparse_map::sparse_map<int, int, std::hash<int>> map;
    return static_cast<int>(map.size());
}
