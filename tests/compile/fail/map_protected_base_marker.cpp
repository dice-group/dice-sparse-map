// A hash function that inherits is_avalanching from a protected base is not marked, so the map does not compile with
// it.
#include <dice/sparse-map/sparse_map.hpp>

#include <cstddef>

struct marker {
    using is_avalanching = void;
};

struct protected_base_marker : protected marker {
    std::size_t operator()(int key) const noexcept {
        return static_cast<std::size_t>(key);
    }
};

int main() {
    dice::sparse_map::sparse_map<int, int, protected_base_marker> map;
    return static_cast<int>(map.size());
}
