#include "RVInterpeter.hpp"
#include <cstdint>
#include <fstream>
#include <iostream>
#include <stdexcept>
#include <vector>

std::vector<uint8_t> load_file(const std::string &filename) {
  std::ifstream file(filename, std::ios::binary | std::ios::ate);
  if (!file.is_open()) {
    throw std::runtime_error("Could not open file: " + filename);
  }
  std::streamsize size = file.tellg();
  file.seekg(0, std::ios::beg);
  std::vector<uint8_t> buffer(size);
  if (!file.read(reinterpret_cast<char *>(buffer.data()), size)) {
    throw std::runtime_error("Could not read file: " + filename);
  }
  return buffer;
}

int main(int argc, char *argv[]) {

  if (argc < 2) {
    std::cerr << "Usage: " << argv[0]
              << " <riscv_binary_file> [load_address_hex]" << std::endl;
    return 1;
  }

  std::string filename = argv[1];
  uint32_t load_addr =
      0x1000; // Default load address, common for simple bare-metal

  if (argc > 2) {
    try {
      load_addr = std::stoul(argv[2], nullptr, 16); // Read address as hex
    } catch (const std::exception &e) {
      std::cerr << "Invalid load address format: " << argv[2]
                << ". Use hexadecimal (e.g., 80000000)." << std::endl;
      return 1;
    }
  }

  try {
    RVInterpreter interpreter; // Use default memory size
    std::vector<uint8_t> program = load_file(filename);

    interpreter.load_program(program, load_addr);

    // interpreter.dump_state(); // Optional: Show state before running

    interpreter.run();

    interpreter.dump_state(); // Show final state

  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << std::endl;
    return 1;
  }

  return 0;
}
