
## A C++ implementation of a memory efficient hash map and hash set

The sparse-map library is a C++ implementation of a memory efficient hash map and hash set based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). We added support for fancy pointers. It uses open-addressing with sparse quadratic probing. The goal of the library is to be the most memory efficient possible, even at low load factor, while keeping reasonable performances. You can find an [article](https://smerity.com/articles/2015/google_sparsehash.html) of Stephen Merity which explains the idea behind `google::sparse_hash_map` and this project.

Two classes are provided: `dice::sparse_map` and `dice::sparse_set`. The number of buckets is a power of two. The hash of a hash function that is not marked as avalanching is mixed with one multiplication before it picks a bucket, see `dice::unordered_sparse::hash_is_avalanching`.

A **benchmark** of `dice::sparse_map` against other hash maps may be found [here](https://tessil.github.io/2016/08/29/benchmark-hopscotch-map.html). The benchmark, in its additional tests page, notably includes `google::sparse_hash_map` and `spp::sparse_hash_map` to which `dice::sparse_map` is an alternative. This page also gives some advices on which hash table structure you should try for your use case (useful if you are a bit lost with the multiple hash tables implementations in the `tsl` namespace).

### Key features

- Header-only library, just add the [include](include/) directory to your include path and you are ready to go. If you use CMake, you can also use the `dice-sparse-map::dice-sparse-map` target from the [CMakeLists.txt](CMakeLists.txt).
- Memory efficient while keeping good lookup speed, see the [benchmark](https://tessil.github.io/2016/08/29/benchmark-hopscotch-map.html) for some numbers.
- Support for heterogeneous lookups allowing the usage of `find` with a type different than `Key` (e.g. if you have a map that uses `std::unique_ptr<foo>` as key, you can use a `foo*` or a `std::uintptr_t` as key parameter to `find` without constructing a `std::unique_ptr<foo>`, see [example](#heterogeneous-lookups)).
- No need to reserve any sentinel value from the keys.
- If the hash is known before a lookup, it is possible to pass it as parameter to speed-up the lookup (see `precalculated_hash` parameter in [API](https://tessil.github.io/sparse-map/classtsl_1_1sparse__map.html)).
- Possibility to control the balance between insertion speed and memory usage with the `Sparsity` template parameter. A high sparsity means less memory but longer insertion times, and vice-versa for low sparsity. The default medium sparsity offers a good compromise (see [API](https://tessil.github.io/sparse-map/classtsl_1_1sparse__map.html#details) for details). For reference, with simple 64 bits integers as keys and values, a low sparsity offers ~15% faster insertions times but uses ~12% more memory. Nothing change regarding lookup speed.
- API closely similar to `std::unordered_map` and `std::unordered_set`.

### Differences compared to `std::unordered_map`

`dice::sparse_map` tries to have an interface similar to `std::unordered_map`, but some differences exist.

- **By default only the basic exception safety is guaranteed** which mean that, in case of exception, all resources used by the hash map will be freed (no memory leaks) but the hash map may end-up in an undefined state (undefined here means that some elements may be missing). This can ONLY happen on rehash (either on insert or if `rehash` is called explicitly) and will occur if the `Allocator` can't allocate memory (`std::bad_alloc`) or if the copy constructor (when a nothrow move constructor is not available) throws and exception. This can be avoided by calling `reserve` beforehand. It is the same guarantee that the one provided by `google::sparse_hash_map` and `spp::sparse_hash_map` which don't provide the strong exception guarantee. For more information and if you need the strong exception guarantee, check the `ExceptionSafety` template parameter (see [API](https://tessil.github.io/sparse-map/classtsl_1_1sparse__map.html#details) for details).
- Iterator invalidation doesn't behave in the same way, any operation modifying the hash table invalidate them (see [API](https://tessil.github.io/sparse-map/classtsl_1_1sparse__map.html#details) for details).
- References and pointers to keys or values in the map are invalidated in the same way as iterators to these keys-values.
- For iterators, `operator*()` and `operator->()` return a reference and a pointer to `const std::pair<Key, T>` instead of `std::pair<const Key, T>` making the value `T` not modifiable. To modify the value you have to call the `value()` method of the iterator to get a mutable reference. Example:
```c++
dice::sparse_map<int, int> map = {{1, 1}, {2, 1}, {3, 1}};
for(auto it = map.begin(); it != map.end(); ++it) {
    //it->second = 2; // Illegal
    it.value() = 2; // Ok
}
```
- Move-only types must have a nothrow move constructor.
- No support for some buckets related methods (like `bucket_size`, `bucket`, ...).

These differences also apply between `std::unordered_set` and `dice::sparse_set`.

Thread-safety guarantees are the same as `std::unordered_map/set` (i.e. possible to have multiple readers with no writer).

### Optimization

#### Popcount
The library relies heavily on the [popcount](https://en.wikipedia.org/wiki/Hamming_weight) operation. 

With Clang and GCC, the library uses the `__builtin_popcount` function which will use the fast CPU instruction `POPCNT` when the library is compiled with `-mpopcnt`. Using the `POPCNT` instruction offers an improvement of ~15% to ~30% on lookups. So if you are compiling your code for a specific architecture that support the operation, don't forget the `-mpopcnt` (or `-march=native`) flag of your compiler.

On Windows with MSVC, the detection is done at runtime.

#### Move constructor
Make sure that your key `Key` and potential value `T` have a `noexcept` move constructor. The library will work without it but insertions will be much slower if the copy constructor is expensive (the structure often needs to move some values around on insertion).

### Installation

To use sparse-map, just add the [include](include/) directory to your include path. It is a **header-only** library.

If you use CMake, you can also use the `dice-sparse-map::dice-sparse-map` target from the [CMakeLists.txt](CMakeLists.txt) with `target_link_libraries`. 
```cmake
# Example where the sparse-map project is stored in a third-party directory
add_subdirectory(third-party/sparse-map)
target_link_libraries(your_target PRIVATE dice-sparse-map::dice-sparse-map)  
```

If the project has been installed through `make install`, you can also use `find_package(dice-sparse-map REQUIRED)` instead of `add_subdirectory`.

The code should work with any C++17 standard-compliant compiler.

To run the tests you will need the Boost Test library and CMake.

```bash
git clone https://github.com/Tessil/sparse-map.git
cd sparse-map/tests
mkdir build
cd build
cmake ..
cmake --build .
./tsl_sparse_map_tests 
```

### Usage

The API can be found [here](https://tessil.github.io/sparse-map/). 

All methods are not documented yet, but they replicate the behaviour of the ones in `std::unordered_map` and `std::unordered_set`, except if specified otherwise.

### Example

```c++
#include <cstdint>
#include <iostream>
#include <string>
#include <dice/unordered_sparse.hpp>

int main() {
    dice::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map["d"] = 4;
    
    map.insert({"e", 5});
    map.erase("b");
    
    for(auto it = map.begin(); it != map.end(); ++it) {
        //it->second += 2; // Not valid.
        it.value() += 2;
    }
    
    // {d, 6} {a, 3} {e, 7} {c, 5}
    for(const auto& key_value : map) {
        std::cout << "{" << key_value.first << ", " << key_value.second << "}" << std::endl;
    }
    
    
    if(map.find("a") != map.end()) {
        std::cout << "Found \"a\"." << std::endl;
    }
    
    const std::size_t precalculated_hash = std::hash<std::string>()("a");
    // If we already know the hash beforehand, we can pass it as argument to speed-up the lookup.
    if(map.find("a", precalculated_hash) != map.end()) {
        std::cout << "Found \"a\" with hash " << precalculated_hash << "." << std::endl;
    }
    
    
    
    
    dice::sparse_set<int> set;
    set.insert({1, 9, 0});
    set.insert({2, -1, 9});
    
    // {0} {1} {2} {9} {-1}
    for(const auto& key : set) {
        std::cout << "{" << key << "}" << std::endl;
    }
}
```

#### Heterogeneous lookups

Heterogeneous overloads allow the usage of other types than `Key` for lookup and erase operations as long as the used types are hashable and comparable to `Key`.

To activate the heterogeneous overloads in `dice::sparse_map/set`, the qualified-id `KeyEqual::is_transparent` must be valid. It works the same way as for [`std::map::find`](http://en.cppreference.com/w/cpp/container/map/find). You can either use [`std::equal_to<>`](http://en.cppreference.com/w/cpp/utility/functional/equal_to_void) or define your own function object.

Both `KeyEqual` and `Hash` will need to be able to deal with the different types.

```c++
#include <functional>
#include <iostream>
#include <string>
#include <dice/unordered_sparse.hpp>


struct employee {
    employee(int id, std::string name) : m_id(id), m_name(std::move(name)) {
    }
    
    // Either we include the comparators in the class and we use `std::equal_to<>`...
    friend bool operator==(const employee& empl, int empl_id) {
        return empl.m_id == empl_id;
    }
    
    friend bool operator==(int empl_id, const employee& empl) {
        return empl_id == empl.m_id;
    }
    
    friend bool operator==(const employee& empl1, const employee& empl2) {
        return empl1.m_id == empl2.m_id;
    }
    
    
    int m_id;
    std::string m_name;
};

// ... or we implement a separate class to compare employees.
struct equal_employee {
    using is_transparent = void;
    
    bool operator()(const employee& empl, int empl_id) const {
        return empl.m_id == empl_id;
    }
    
    bool operator()(int empl_id, const employee& empl) const {
        return empl_id == empl.m_id;
    }
    
    bool operator()(const employee& empl1, const employee& empl2) const {
        return empl1.m_id == empl2.m_id;
    }
};

struct hash_employee {
    std::size_t operator()(const employee& empl) const {
        return std::hash<int>()(empl.m_id);
    }
    
    std::size_t operator()(int id) const {
        return std::hash<int>()(id);
    }
};


int main() {
    // Use std::equal_to<> which will automatically deduce and forward the parameters
    dice::sparse_map<employee, int, hash_employee, std::equal_to<>> map; 
    map.insert({employee(1, "John Doe"), 2001});
    map.insert({employee(2, "Jane Doe"), 2002});
    map.insert({employee(3, "John Smith"), 2003});

    // John Smith 2003
    auto it = map.find(3);
    if(it != map.end()) {
        std::cout << it->first.m_name << " " << it->second << std::endl;
    }

    map.erase(1);



    // Use a custom KeyEqual which has an is_transparent member type
    dice::sparse_map<employee, int, hash_employee, equal_employee> map2;
    map2.insert({employee(4, "Johnny Doe"), 2004});

    // 2004
    std::cout << map2.at(4) << std::endl;
}
```

### License

The code is licensed under the MIT license, see the LICENSE files ([1](LICENSE-tsl-sparse-map), [2](LICENSE-dice-sparse-map)) for details.
