/// @file includes/RVInterpreter.hpp
/// @copyright Copyright 2025 Lohith Madalli, Shlok Powar, Siddharth Mohanty. All rights reserved.
/// @license This project is released under the MIT License.



#include <array>
#include <cstdint> // For uint32_t, int32_t, etc.
#include <iomanip> // For std::hex, std::setw, std::setfill
#include <iostream>
#include <stdexcept> // For exceptions
#include <vector>

/**
 * @file includes/RVInterpeter.hpp
 * @brief RISC-V interpreter class declaration.
 */
const uint32_t MEMORY_SIZE = 1024 * 1024;

/**
 * @class RVInterpreter
 * @brief A simple RISC-V RV32I interpreter.
 */
class RVInterpreter {
public:
  /**
   * @brief Construct a new interpreter.
   * @param mem_size Size of memory in bytes (default 1 MiB).
   */
  RVInterpreter(uint32_t mem_size = MEMORY_SIZE);

  /**
   * @brief Load a program into the interpreter’s memory.
   * @param program_data Byte vector containing the program.
   * @param load_address Address in memory where the program is placed.
   */
  void load_program(const std::vector<uint8_t> &program_data,
                    uint32_t load_address);

  /**
   * @brief Execute instructions in a fetch–decode–execute loop until halted
   *        or cycle limit is reached.
   */
  void run();

  /**
   * @brief Print the current registers and program counter to stdout.
   */
  void dump_state() const;

private:
  std::vector<uint8_t> memory;
  std::array<uint32_t, 32> regs;

  uint32_t pc; // Program Counter
  bool running = false;
  uint64_t cycle_count = 0;

  uint64_t max_cycles = 1000000; // Limit execution cycles

  /** @name Memory Access
   *  @brief Read and write little-endian values from/to the interpreter memory.
   */
  ///@{
  /**
   * @brief Read a value of type T from memory.
   * @tparam T The type to read (e.g., uint32_t for words).
   * @param address Memory address to read from.
   * @return The value read, or halts on invalid access.
   */
  template <typename T> T read_memory(uint32_t address);
  /**
   * @brief Write a value of type T to memory.
   * @tparam T The type to write (e.g., uint32_t for words).
   * @param address Memory address to write to.
   * @param value The value to write.
   */
  template <typename T> void write_memory(uint32_t address, T value);
  ///@}

  /** @name Fetch
   *  @brief Retrieve the next instruction word from memory.
   */
  ///@{
  /**
   * @brief Fetch a 32-bit instruction and advance the PC.
   * @return The raw instruction word.
   */
  uint32_t fetch() {
    uint32_t instruction = read_memory<uint32_t>(pc);
    if (!running)
      return 0; // Check if read_memory caused a halt
    // std::cout << "Fetched instruction 0x" << std::hex << std::setw(8) <<
    // std::setfill('0') << instruction
    //           << " from PC 0x" << std::setw(8) << pc << std::dec <<
    //           std::endl; // Debug fetch
    pc += 4; // Increment PC for the next instruction (usually)

    return instruction;
  }

  /** @name Core Execution
   *  @brief Decode and execute instructions, and handle system calls.
   */
  ///@{
  /**
   * @brief Decode and execute a single instruction.
   * @param instruction Raw 32-bit instruction word.
   */
  void decode_and_execute(uint32_t instruction);
  /**
   * @brief Handle an ECALL from the guest program.
   */
  void handle_ecall();
  ///@}
};
