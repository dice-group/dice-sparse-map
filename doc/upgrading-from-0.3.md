# Upgrading from 0.3

[README](../README.md) · [Usage](usage.md) · [Design](design.md) · [Benchmarks](benchmarks.md) · **Upgrading from 0.3**

What changes for code that uses dice-sparse-map 0.3.

- The containers are `dice::sparse_map` and `dice::sparse_set` from `<dice/unordered_sparse.hpp>`, the only header to include. The other names, for example `sparsity` and `hash_is_avalanching`, are in `dice::unordered_sparse`. The namespace `dice::sparse_map::sh` and the headers in `dice/sparse-map/` are gone.
- `iterator::value()` and `iterator::key()` are gone. Use `it->second` and `it->first`.
- `*it` of a map iterator is a `std::pair` of references, not an lvalue. `auto &x = *it` and `for (auto &x : map)` do not compile. Use `auto &&` or `auto const &`.
- The `GrowthPolicy` template parameter, `prime_growth_policy`, `mod_growth_policy`, `power_of_two_growth_policy`, `sparse_pg_map` and `sparse_pg_set` are gone. The bucket count is always a power of two. The template parameters are `sparse_map<Key, T, Hash, KeyEqual, Allocator, sparsity, inline_capacity>` and `sparse_set<Key, Hash, KeyEqual, Allocator, sparsity, inline_capacity>`. The inline capacity is new, its default 0 keeps every element in allocated groups, see [small maps](usage.md#small-maps).
- A hash function that is not marked as avalanching is mixed. The elements are placed differently, so containers that were persisted with 0.3 cannot be opened (`pobr_version` is 3). See [the persisted format](design.md#the-persisted-format).
- The `exception_safety` template parameter is gone. See [exception safety](usage.md#exception-safety).
- `serialize` and `deserialize` are gone.
- Heterogeneous overloads need `is_transparent` in the hash function as well as in the key equality, as for `std::unordered_map`. 0.3 needed it only in the key equality. Without it, the argument is converted to `Key`, and an argument that does not convert implicitly (for example `std::string_view` for a `std::string` key) does not compile. Add `using is_transparent = void;` to the hash function.
- The CMake package, its target and the conan package are named `dice-unordered-sparse`: `find_package(dice-unordered-sparse)` and `dice-unordered-sparse::dice-unordered-sparse` instead of `dice-sparse-map`.
