import os
import re

from conan import ConanFile
from conan.tools.cmake import CMake, cmake_layout, CMakeDeps, CMakeToolchain
from conans import tools
from conans.util.files import rmdir, load


class ProjectConanFile(ConanFile):
    # name, version and description are set below
    author = "https://dice-research.org"
    url = "https://github.com/dice-group/dice-sparse-map"
    topics = ("C++20", "hash-map", "data-structures", "header-only", "hash-table")

    # Binary configuration
    settings = "os", "compiler", "build_type", "arch"
    options = {"shared": [True, False], "fPIC": [True, False]} # todo: remove when the boost dependency is not needed anymore
    default_options = {"shared": False, "fPIC": True, "boost:header_only": True} # todo: remove when the boost dependency is not needed anymore
    exports_sources = "include/*", "CMakeLists.txt", "cmake/*", "LICENSE*"
    requires = "boost/1.69.0"

    generators = ("CMakeDeps", "CMakeToolchain")

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


    def config_options(self):  # todo: remove when the boost dependency is not needed anymore
        if self.settings.os == "Windows":
            del self.options.fPIC

    _cmake = None

    def _configure_cmake(self):
        if self._cmake:
            return self._cmake
        self._cmake = CMake(self)
        self._cmake.configure()

        return self._cmake

    def layout(self):
        cmake_layout(self)

    def build(self):
        self._configure_cmake().build()

    def package(self):
        self._configure_cmake().install()
        for file in os.listdir(self.folders.package_folder):
            if file != "include":
                rmdir(os.path.join(self.package_folder, file))
        self.copy(pattern="LICENSE*", dst="licenses", src=self.folders.source_folder)

    def package_id(self):
        self.info.header_only()

