// Maps and sets of integers and strings compile with the default hash function.
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

#include <string>

int main() {
    dice::sparse_map::sparse_map<int, int> int_map;
    int_map[1] = 1;
    dice::sparse_map::sparse_set<int> int_set;
    int_set.insert(1);
    dice::sparse_map::sparse_map<std::string, int> string_map;
    string_map["one"] = 1;
    dice::sparse_map::sparse_set<std::string> string_set;
    string_set.insert("one");
    return int_map.size() + int_set.size() + string_map.size() + string_set.size() == 4 ? 0 : 1;
}
