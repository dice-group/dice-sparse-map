/*
 * Uses the common map interface once with the allocator of maps.hpp, for one map and one key type
 * (`DSM_README_MAP`, `DSM_README_STRING_KEYS`). With `DSM_README_METALL`, the map lives in a fresh
 * metall datastore in `DSM_README_METALL_DIR` (default `/tmp/dsm-readme-metall`).
 *
 * The benchmark itself needs only `try_emplace`, `count`, `erase` and iteration. This program shows
 * whether the rest of the interface compiles too, for example `operator[]` and `operator->` of the
 * iterator. It prints "ok" or exits with code 1.
 */

#include "maps.hpp"

#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <filesystem>
#include <string>
#include <system_error>
#include <unistd.h>
#include <utility>

#ifndef DSM_README_MAP
#error "DSM_README_MAP must name a map struct of maps.hpp"
#endif

namespace {

    using namespace dice::sparse_map::bench::readme;

#if defined(DSM_README_STRING_KEYS)
    using key_type = std::string;

    key_type key_of(int i) {
        return "a key that does not fit into the string itself, number " + std::to_string(i);
    }
#else
    using key_type = std::uint64_t;

    key_type key_of(int i) {
        return static_cast<key_type>(i) * 0x9E3779B97F4A7C15ULL;
    }
#endif

    using map_t = DSM_README_MAP::type<key_type>;

    template<typename Make>
    bool check(Make &&make) {
        auto map = make();
        map[key_of(0)] = 1;
        map[key_of(1)] += 2;
        map.at(key_of(1)) += 1;
        map.insert({key_of(2), 3});
        map.emplace(key_of(3), 4);
        map.try_emplace(key_of(4), 5);
        map.insert_or_assign(key_of(4), 6);
        auto it = map.find(key_of(0));
        if (it == map.end()) {
            return false;
        }
        it->second += 10;
        std::size_t sum = 0;
        for (auto const &element : map) {
            sum += element.second;
        }
        auto copy = map;
        copy.erase(copy.find(key_of(2)));
        map.erase(key_of(3));
        map.reserve(1000);
        map.rehash(2000);
        auto moved = std::move(copy);
        bool const right = sum == 11 + 3 + 3 + 4 + 6
            && map.size() == 4
            && moved.size() == 4
            && map.count(key_of(2)) == 1
            && moved.count(key_of(2)) == 0;
        map.clear();
        return right && map.empty();
    }

} // namespace

int main() {
#if defined(DSM_README_METALL)
    char const *dir = std::getenv("DSM_README_METALL_DIR");
    std::filesystem::path const
        path = std::filesystem::path{dir != nullptr && *dir != '\0' ? dir : "/tmp/dsm-readme-metall"}
        / ("api-check-" + std::to_string(::getpid()));
    std::filesystem::create_directories(path.parent_path());
    bool ok = false;
    {
        metall::manager manager{metall::create_only, path.c_str()};
        ok = check([&] { return map_t(typename map_t::allocator_type(manager.get_allocator())); });
    }
    static_cast<void>(metall::manager::remove(path.c_str()));
    std::error_code error;
    std::filesystem::remove_all(path, error);
#else
    bool const ok = check([] { return map_t(); });
#endif
    std::printf("%s\n", ok ? "ok" : "wrong results");
    return ok ? 0 : 1;
}
