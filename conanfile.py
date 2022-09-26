import os
import re

from conans import CMake, ConanFile, load
from conans.util.files import rmdir


class Recipe(ConanFile):
    url = "https://github.com/dice-group/dice-sparse-map"
    topics = ("c++20", "hash-map", "data-structures", "header-only", "hash-table")

    settings = "os", "compiler", "build_type", "arch"
    exports_sources = "include/*", "CMakeLists.txt", "cmake/*", "LICENSE*"
    requires = "boost/1.79.0"
    generators = "cmake_find_package"

    def set_name(self):
        if not hasattr(self, 'name') or self.version is None:
            cmake_file = load(os.path.join(self.recipe_folder, "CMakeLists.txt"))
            self.name = re.search(r"project\(\s*([a-z\-]+)\s+VERSION", cmake_file).group(1)

    def set_version(self):
        if not hasattr(self, 'version') or self.version is None:
            cmake_file = load(os.path.join(self.recipe_folder, "CMakeLists.txt"))
            self.version = re.search(r"project\([^)]*VERSION\s+(\d+\.\d+.\d+)[^)]*\)", cmake_file).group(1)
        if not hasattr(self, 'description') or self.description is None:
            cmake_file = load(os.path.join(self.recipe_folder, "CMakeLists.txt"))
            self.description = re.search(r"project\([^)]*DESCRIPTION\s+\"([^\"]+)\"[^)]*\)", cmake_file).group(1)

    def package(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.install()
        for file in os.listdir(self.folders.package_folder):
            if file != "include":
                rmdir(os.path.join(self.package_folder, file))
        self.copy(pattern="LICENSE*", dst="licenses", src=self.folders.source_folder)

    def package_id(self):
        self.info.header_only()
