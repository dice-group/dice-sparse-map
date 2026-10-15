
## A C++ implementation of a memory efficient hash map and hash set

The sparse-map library is a C++ implementation of a memory efficient hash map and hash set based on [tsl::sparse_map](https://github.com/Tessil/sparse-map). We added support for fancy pointers. It uses open-addressing with sparse quadratic probing. The goal of the library is to be the most memory efficient possible, even at low load factor, while keeping reasonable performances. You can find an [article](https://smerity.com/articles/2015/google_sparsehash.html) of Stephen Merity which explains the idea behind `google::sparse_hash_map` and this project.

Two classes are provided: `dice::sparse_map::sparse_map` and `dice::sparse_map::sparse_set`. The number of buckets is 0 or a power of two, see [Bucket count](#bucket-count). The hash function must be avalanching, see [Hash function](#hash-function).

A **benchmark** of `dice::sparse_map::sparse_map` against other hash maps may be found [here](https://tessil.github.io/2016/08/29/benchmark-hopscotch-map.html). The benchmark, in its additional tests page, notably includes `google::sparse_hash_map` and `spp::sparse_hash_map` to which `dice::sparse_map::sparse_map` is an alternative. This page also gives some advices on which hash table structure you should try for your use case (useful if you are a bit lost with the multiple hash tables implementations in the `tsl` namespace).

### Key features

- Header-only library, just add the [include](include/) directory to your include path and you are ready to go. If you use CMake, you can also use the `dice::sparse_map::sparse_map` exported target from the [CMakeLists.txt](CMakeLists.txt).
- Memory efficient while keeping good lookup speed, see the [benchmark](https://tessil.github.io/2016/08/29/benchmark-hopscotch-map.html) for some numbers.
- Support for heterogeneous lookups allowing the usage of `find` with a type different than `Key` (e.g. if you have a map that uses `std::unique_ptr<foo>` as key, you can use a `foo*` or a `std::uintptr_t` as key parameter to `find` without constructing a `std::unique_ptr<foo>`, see [example](#heterogeneous-lookups)).
- No need to reserve any sentinel value from the keys.
- If the hash is known before a lookup, it is possible to pass it as parameter to speed-up the lookup (see `precalculated_hash` parameter in [API](https://tessil.github.io/sparse-map/classtsl_1_1sparse__map.html)).
- Possibility to control the balance between insertion speed and memory usage with the `Sparsity` template parameter. A high sparsity means less memory but longer insertion times, and vice-versa for low sparsity. The default medium sparsity offers a good compromise (see [API](https://tessil.github.io/sparse-map/classtsl_1_1sparse__map.html#details) for details). For reference, with simple 64 bits integers as keys and values, a low sparsity offers ~15% faster insertions times but uses ~12% more memory. Nothing change regarding lookup speed.
- API closely similar to `std::unordered_map` and `std::unordered_set`.
- All member functions are `constexpr`. A map or a set works in a constant expression if its hash function, key equality, allocator and elements do. The default hash function `dice::hash::DiceHash` is not `constexpr`, so a map or a set in a constant expression needs another hash function.

### Differences compared to `std::unordered_map`

`dice::sparse_map::sparse_map` tries to have an interface similar to `std::unordered_map`, but some differences exist.

- **Allocation failure.** By default the process ends with `std::abort()` when an allocation of the map fails, that is when the `allocate` of the allocator throws. The last template parameter `AllocationFailure` chooses this: `dice::sparse_map::sh::allocation_failure::terminating` (the default) aborts, `dice::sparse_map::sh::allocation_failure::throwing` lets the exception of the allocator propagate (`std::bad_alloc` for `std::allocator`). In both modes a size limit of the map throws `std::length_error`, and an allocation that an element makes itself (like the copy of a `std::string`) is an exception of the element.
- **Exception safety.** The guarantee follows from the type of the elements. If the insertion of one element throws, the map holds the same elements as before, except in one case: a rehash can leave the map empty. An insertion, `merge`, `rehash` or `reserve` can rehash. Calling `reserve` beforehand avoids rehashes. How a rehash transfers the elements depends on their type:
  - Elements whose move constructor cannot throw are moved in the order of their buckets. The memory of the old buckets is freed while they are moved. If the hash function throws while the elements are moved, or the allocator with `allocation_failure::throwing`, the map is empty. Any other exception leaves the map unchanged.
  - Elements whose move constructor can throw are copied. The old buckets are freed at the end, so the old and the new buckets are in memory at the same time. An exception leaves the map unchanged.
- Iterator invalidation doesn't behave in the same way, any operation modifying the hash table invalidate them (see [API](https://tessil.github.io/sparse-map/classtsl_1_1sparse__map.html#details) for details).
- References and pointers to keys or values in the map are invalidated in the same way as iterators to these keys-values.
- For iterators of a map, `*it` is a proxy `std::pair<const Key &, T &>` (`std::pair<const Key &, const T &>` for `const_iterator`), not `std::pair<const Key, T> &`. `it->second` is a mutable reference to the value. `*it` is not an lvalue: bind it with `auto &&` or `const auto &`, not with `auto &`. Example:
```c++
dice::sparse_map::sparse_map<int, int> map = {{1, 1}, {2, 1}, {3, 1}};
for(auto it = map.begin(); it != map.end(); ++it) {
    it->second = 2;
}
for(auto &&[key, value] : map) {
    value = 3;
}
```
- Move-only types must have a nothrow move constructor.
- No support for some buckets related methods (like `bucket_size`, `bucket`, ...).

These differences also apply between `std::unordered_set` and `dice::sparse_map::sparse_set`.

Thread-safety guarantees are the same as `std::unordered_map/set` (i.e. possible to have multiple readers with no writer).

### Optimization

#### Popcount
The library relies heavily on the [popcount](https://en.wikipedia.org/wiki/Hamming_weight) operation. 

With Clang and GCC, the library uses the `__builtin_popcount` function which will use the fast CPU instruction `POPCNT` when the library is compiled with `-mpopcnt`. Using the `POPCNT` instruction offers an improvement of ~15% to ~30% on lookups. So if you are compiling your code for a specific architecture that support the operation, don't forget the `-mpopcnt` (or `-march=native`) flag of your compiler.

On Windows with MSVC, the detection is done at runtime.

#### Move constructor
Make sure that your key `Key` and potential value `T` have a `noexcept` move constructor. The library will work without it but insertions will be much slower if the copy constructor is expensive (the structure often needs to move some values around on insertion).

### Bucket count

The number of buckets is 0 or a power of two and doubles when the table grows. A hash picks its bucket with a mask, <code>hash & (2<sup>n</sup> - 1)</code>, not with a modulo.

### Hash function

A hash picks its bucket with its low bits as it is, without mixing. So the hash function must be avalanching: each bit of the key changes each bit of the hash with a probability of about one half. A `static_assert` checks `dice::sparse_map::sh::hash_is_avalanching<Hash>`. The trait is true if `Hash` has the public member type `is_avalanching`, its own or of a public base: `using is_avalanching = void;` as in `ankerl::unordered_dense`, or `using is_avalanching = std::true_type;` as in `boost::unordered`. A member type with a `value` that is false, like `std::false_type`, does not count. For a hash function that you cannot change, specialize `dice::sparse_map::sh::hash_is_avalanching`. `std::hash` is not avalanching: for integers, libstdc++ and libc++ return the value itself.

The default hash function is `dice::hash::DiceHash<Key, dice::hash::Policies::wyhash>` of [dice-hash](https://github.com/dice-group/dice-hash). `DiceHash` declares `is_avalanching` if its policy does, for every key type: `wyhash`, `xxh3` and `rapidhash` do, `Martinus` does not. A `dice::hash::dice_hash_overload` for your own key type must keep the avalanche of the policy, for example by returning `dice_hash_templates<Policy>::dice_hash` of its members (see the README of dice-hash). The default hash function does not guard against `0.0` and `-0.0`: they compare equal but have different hashes, so they can be two keys. A hash picks the bucket, so the hash values of `DiceHash` are part of the persisted layout of a map: a dice-hash version that changes them needs a new `pobr_version`. `tests_default_hash` checks some of them.

### Installation

To use sparse-map, just add the [include](include/) directory to your include path. It is a **header-only** library. It needs the headers of [dice-hash](https://github.com/dice-group/dice-hash) and its dependencies.

If you use CMake, you can also use the `dice::sparse_map::sparse_map` exported target from the [CMakeLists.txt](CMakeLists.txt) with `target_link_libraries`. 
```cmake
# Example where the sparse-map project is stored in a third-party directory
add_subdirectory(third-party/sparse-map)
target_link_libraries(your_target PRIVATE dice::sparse_map)  
```

If the project has been installed through `make install`, you can also use `find_package(tsl-sparse-map REQUIRED)` instead of `add_subdirectory`.

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
#include <dice/sparse-map/sparse_map.hpp>
#include <dice/sparse-map/sparse_set.hpp>

int main() {
    dice::sparse_map::sparse_map<std::string, int> map = {{"a", 1}, {"b", 2}};
    map["c"] = 3;
    map["d"] = 4;
    
    map.insert({"e", 5});
    map.erase("b");
    
    for(auto it = map.begin(); it != map.end(); ++it) {
        it->second += 2;
    }
    
    // The order depends on the hash function.
    for(const auto& key_value : map) {
        std::cout << "{" << key_value.first << ", " << key_value.second << "}" << std::endl;
    }
    
    
    if(map.find("a") != map.end()) {
        std::cout << "Found \"a\"." << std::endl;
    }
    
    const std::size_t precalculated_hash = map.hash_function()("a");
    // If we already know the hash beforehand, we can pass it as argument to speed-up the lookup.
    if(map.find("a", precalculated_hash) != map.end()) {
        std::cout << "Found \"a\" with hash " << precalculated_hash << "." << std::endl;
    }
    
    
    
    
    dice::sparse_map::sparse_set<int> set;
    set.insert({1, 9, 0});
    set.insert({2, -1, 9});
    
    // The order depends on the hash function.
    for(const auto& key : set) {
        std::cout << "{" << key << "}" << std::endl;
    }
}
```

#### Heterogeneous lookups

Heterogeneous overloads allow the usage of other types than `Key` for lookup and erase operations as long as the used types are hashable and comparable to `Key`.

To activate the heterogeneous overloads in `dice::sparse_map::sparse_map/set`, the qualified-id `KeyEqual::is_transparent` must be valid. It works the same way as for [`std::map::find`](http://en.cppreference.com/w/cpp/container/map/find). You can either use [`std::equal_to<>`](http://en.cppreference.com/w/cpp/utility/functional/equal_to_void) or define your own function object.

Both `KeyEqual` and `Hash` will need to be able to deal with the different types.

```c++
#include <functional>
#include <iostream>
#include <string>
#include <dice/sparse-map/sparse_map.hpp>


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

// The hash must be avalanching, see "Hash function" above.
struct hash_employee {
    using is_avalanching = void;
    
    std::size_t operator()(const employee& empl) const {
        return dice::hash::DiceHash<int, dice::hash::Policies::wyhash>()(empl.m_id);
    }
    
    std::size_t operator()(int id) const {
        return dice::hash::DiceHash<int, dice::hash::Policies::wyhash>()(id);
    }
};


int main() {
    // Use std::equal_to<> which will automatically deduce and forward the parameters
    dice::sparse_map::sparse_map<employee, int, hash_employee, std::equal_to<>> map; 
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
    dice::sparse_map::sparse_map<employee, int, hash_employee, equal_employee> map2;
    map2.insert({employee(4, "Johnny Doe"), 2004});

    // 2004
    std::cout << map2.at(4) << std::endl;
}
```

### License

The code is licensed under the MIT license, see the LICENSE files ([1](LICENSE-tsl-sparse-map), [2](LICENSE-dice-sparse-map)) for details.
