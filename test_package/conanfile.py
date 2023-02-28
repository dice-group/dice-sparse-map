import os
from conan.tools.cmake import CMake, CMakeToolchain
from conan.tools.layout import cmake_layout
from conan import ConanFile

required_conan_version = ">=1.59"


class TestPackageConan(ConanFile):
    settings = "os", "compiler", "build_type", "arch"
    generators = "CMakeDeps", "CMakeToolchain"

    def generate(self):
        tc = CMakeToolchain(self)
        tc.generate()

    def layout(self):
        cmake_layout(self)

    def build(self):
        cmake = CMake(self)
        cmake.configure()
        cmake.build()

    def test(self):
        self.run(os.path.join(self.cpp.build.bindirs[0], "example"), run_environment=True)
