// std::hash is not marked as avalanching, so the set does not compile with it.
#include <dice/sparse-map/sparse_set.hpp>

#include <functional>

int main() {
    dice::sparse_map::sparse_set<int, std::hash<int>> set;
    return static_cast<int>(set.size());
}
