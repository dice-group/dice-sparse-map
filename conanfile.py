import os
import re

from conan import ConanFile
from conan.tools.cmake import CMake
from conan.tools.files import rmdir, copy, load


class Recipe(ConanFile):
    url = "https://github.com/dice-group/dice-sparse-map"
    topics = "c++20", "hash-map", "data-structures", "header-only", "hash-table"
    settings = "os", "compiler", "build_type", "arch"
    exports_sources = "include/*", "CMakeLists.txt", "cmake/*", "LICENSE*"
    package_type = "header-library"
    generators = "CMakeDeps", "CMakeToolchain"
    no_copy_source = True
    options = {"with_test_deps": [True, False]}
    default_options = {"with_test_deps": False}

    def requirements(self):
        if self.options.with_test_deps:
            self.requires("boost/1.83.0")

    def set_name(self):
        if not hasattr(self, 'name') or self.version is None:
            cmake_file = load(self, os.path.join(self.recipe_folder, "CMakeLists.txt"))
            self.name = re.search(r"project\(\s*([a-z\-]+)\s+VERSION", cmake_file).group(1)

    def set_version(self):
        if not hasattr(self, 'version') or self.version is None:
            cmake_file = load(self, os.path.join(self.recipe_folder, "CMakeLists.txt"))
            self.version = re.search(r"project\([^)]*VERSION\s+(\d+\.\d+.\d+)[^)]*\)", cmake_file).group(1)
        if not hasattr(self, 'description') or self.description is None:
            cmake_file = load(self, os.path.join(self.recipe_folder, "CMakeLists.txt"))
            self.description = re.search(r"project\([^)]*DESCRIPTION\s+\"([^\"]+)\"[^)]*\)", cmake_file).group(1)

    def package(self):
        cmake = CMake(self)
        cmake.configure(variables={"USE_CONAN": False})
        cmake.install()

        for dir in ("lib", "res", "share"):
            rmdir(self, os.path.join(self.package_folder, dir))

        copy(self, "LICENSE*", src=self.source_folder, dst=os.path.join(self.package_folder, "licenses"))

    def package_info(self):
        self.cpp_info.bindirs = []
        self.cpp_info.libdirs = []

        self.cpp_info.set_property("cmake_find_mode", "both")
        self.cpp_info.set_property("cmake_target_name", "dice-sparse-map::dice-sparse-map")
        self.cpp_info.set_property("cmake_file_name", "dice-sparse-map")
