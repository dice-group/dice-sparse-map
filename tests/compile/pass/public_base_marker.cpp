// A hash function that inherits is_avalanching from a public base is marked, so the map compiles with it.
#include <dice/sparse-map/sparse_map.hpp>

#include <cstddef>

struct marker {
    using is_avalanching = void;
};

struct public_base_marker : marker {
    std::size_t operator()(int key) const noexcept {
        return static_cast<std::size_t>(key);
    }
};

int main() {
    dice::sparse_map::sparse_map<int, int, public_base_marker> map;
    map[1] = 1;
    return map.size() == 1 ? 0 : 1;
}
