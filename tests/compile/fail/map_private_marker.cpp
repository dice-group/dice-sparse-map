// A hash function whose member type is_avalanching is private is not marked, so the map does not compile with it.
#include <dice/sparse-map/sparse_map.hpp>

#include <cstddef>

struct private_marker {
    std::size_t operator()(int key) const noexcept {
        return static_cast<std::size_t>(key);
    }

private:
    using is_avalanching = void;
};

int main() {
    dice::sparse_map::sparse_map<int, int, private_marker> map;
    return static_cast<int>(map.size());
}
