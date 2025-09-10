#include "RVInterpeter.hpp"
#include <cstdint>

RVInterpreter::RVInterpreter(uint32_t mem_size) : memory(mem_size, 0), pc(0) {
  // Initialize registers to 0. x0 is hardwired zero, but we enforce it in
  // operations.
  regs.fill(0);
  // Conventionally, stack pointer (x2) starts at the end of memory
  regs[2] = mem_size;
}

void RVInterpreter::load_program(const std::vector<uint8_t> &program_data,
                                 uint32_t load_address) {
  if (load_address + program_data.size() > memory.size()) {
    throw std::runtime_error(
        "Program too large for memory or invalid load address");
  }

  std::copy(program_data.begin(), program_data.end(),
            memory.begin() + load_address);
  pc = load_address; // Set PC to the start of the program
  std::cout << "Loaded program (" << program_data.size()
            << " bytes) at address 0x" << std::hex << load_address << std::dec
            << std::endl;
}

void RVInterpreter::run() {
  running = true;
  while (running) {

    if (pc >= memory.size() || pc % 4 != 0) { // Check alignment and bounds
      std::cerr << "Error: PC out of bounds or misaligned: 0x" << std::hex << pc
                << std::dec << std::endl;
      dump_state();
      running = false;
      break;
    }

    uint32_t instruction = fetch();
    decode_and_execute(instruction);

    // Ensure x0 is always zero
    regs[0] = 0;

    // Basic cycle counter (optional)
    cycle_count++;
    if (cycle_count > max_cycles) { // Prevent infinite loops
      std::cerr << "Error: Exceeded maximum cycle count (" << max_cycles << ")"
                << std::endl;
      dump_state();
      running = false;
      break;
    }
  }
  std::cout << "Execution halted after " << cycle_count << " cycles."
            << std::endl;
  std::cout << "Final PC: 0x" << std::hex << pc << std::dec << std::endl;
}

void RVInterpreter::dump_state() const {
  std::cout << "--- CPU State ---" << std::endl;
  std::cout << "PC: 0x" << std::hex << std::setw(8) << std::setfill('0') << pc
            << std::dec << std::endl;
  for (int i = 0; i < 32; ++i) {
    std::cout << "x" << std::setw(2) << std::setfill(' ') << i << ": 0x"
              << std::hex << std::setw(8) << std::setfill('0') << regs[i]
              << std::dec;
    if ((i + 1) % 4 == 0) {
      std::cout << std::endl;
    } else {
      std::cout << "  ";
    }
  }
  std::cout << "-----------------" << std::endl;
}

template <typename T> T RVInterpreter::read_memory(uint32_t address) {
  if (address + sizeof(T) > memory.size() || address % alignof(T) != 0) {
    std::cerr << "Error: Memory read violation at 0x" << std::hex << address
              << std::dec << std::endl;
    dump_state();
    running = false; // Halt on error
    return 0;
  }
  // Assuming little-endian host matching RISC-V standard
  return *reinterpret_cast<T *>(&memory[address]);
}

template <typename T>
void RVInterpreter::write_memory(uint32_t address, T value) {
  if (address + sizeof(T) > memory.size() || address % alignof(T) != 0) {
    std::cerr << "Error: Memory write violation at 0x" << std::hex << address
              << std::dec << std::endl;
    dump_state();
    running = false; // Halt on error
    return;
  }
  // Assuming little-endian host
  *reinterpret_cast<T *>(&memory[address]) = value;
}

struct instruction_fmt {
  uint32_t opcode;
  uint32_t rd;
  uint32_t funct3;
  uint32_t rs1;
  uint32_t rs2;
  uint32_t funct7;
};

instruction_fmt format_instruction(uint32_t instruction) {
  return {
      instruction & 0x7F, // Lowest 7 bits
      (instruction >> 7) & 0x1F,  (instruction >> 12) & 0x7,
      (instruction >> 15) & 0x1F, (instruction >> 20) & 0x1F,
      (instruction >> 25) & 0x7F,
  };
}

void RVInterpreter::decode_and_execute(uint32_t instruction) {
  auto [opcode, rd, funct3, rs1, rs2, funct7] = format_instruction(instruction);

  // Immediate value decoding (depends on instruction type)
  int32_t imm_i =
      static_cast<int32_t>(instruction) >> 20; // Sign-extend I-type immediate

  int32_t imm_s = ((static_cast<int32_t>(instruction) >> 20) & 0xFFFFFFE0) |
                  ((instruction >> 7) & 0x1F); // S-type
  int32_t imm_b =
      ((static_cast<int32_t>(instruction) >> 19) & 0xFFFFF800) // imm[12|10:5]
      | ((instruction >> 7) & 0x1F) << 1 // imm[4:1|11] -- Reconstruct B-type
      | ((instruction >> 20) & 0xFE0);

  uint32_t imm_u = instruction & 0xFFFFF000; // U-type (upper 20 bits)

  int32_t imm_j =
      (static_cast<int32_t>(instruction & 0x80000000) >> 11) // imm[20]
      | (instruction & 0xFF000)                              // imm[19:12]
      | ((instruction >> 9) & 0x800)                         // imm[11]
      | ((instruction >> 20) & 0x7FE);                       // imm[10:1]
  // Note: J immediates are also multiplied by 2

  // std::cout << "  Opcode: 0x" << std::hex << opcode << ", rd: " << std::dec
  // << rd << ", rs1: " << rs1 << ", rs2: " << rs2
  //           << ", funct3: 0x" << std::hex << funct3 << ", funct7: 0x" <<
  //           funct7 << std::dec << std::endl; // Debug decode

  [[maybe_unused]]
  bool pc_changed = false;

  switch (opcode) {
  case 0x13: // OP-IMM instructions (ADDI, SLTI, SLTIU, XORI, ORI, ANDI, SLLI,
             // SRLI, SRAI)
    switch (funct3) {
    case 0x0: // ADDI
      // std::cout << "  Executing ADDI x" << rd << ", x" << rs1 << ", " <<
      // imm_i << std::endl;
      if (rd != 0)
        regs[rd] = regs[rs1] + imm_i;
      break;

      // --- TODO: Add other OP-IMM instructions (SLTI, SLTIU, XORI, ORI,
      // ANDI...) ---

    case 0x1: // SLLI (Shift Left Logical Immediate) - funct7 must be 0
      if (funct7 == 0x00 && rd != 0) {
        // Lower 5 bits of imm_i specify shift amount (shamt) for RV32I
        uint32_t shamt = imm_i & 0x1F;
        regs[rd] = regs[rs1] << shamt;
      } else {
        std::cerr << "Warning: Unimplemented/Invalid SLLI variant (funct7=0x"
                  << std::hex << funct7 << ")" << std::endl;
      }
      break;
    case 0x5: // SRLI / SRAI (Shift Right Logical/Arithmetic Immediate)
      if (rd != 0) {
        uint32_t shamt = imm_i & 0x1F;
        if (funct7 == 0x00) { // SRLI
          regs[rd] = regs[rs1] >> shamt;
        } else if (funct7 == 0x20) { // SRAI
          // C++ >> on signed types is arithmetic shift
          regs[rd] = static_cast<int32_t>(regs[rs1]) >> shamt;
        } else {
          std::cerr
              << "Warning: Unimplemented/Invalid SRLI/SRAI variant (funct7=0x"
              << std::hex << funct7 << ")" << std::endl;
        }
      }
      break;
    case 0x4: // XORI
      if (rd != 0)
        regs[rd] = regs[rs1] ^ imm_i;
      break;
    case 0x6: // ORI
      if (rd != 0)
        regs[rd] = regs[rs1] | imm_i;
      break;
    case 0x7: // ANDI
      if (rd != 0)
        regs[rd] = regs[rs1] & imm_i;

      break;
    default:
      std::cerr << "Warning: Unimplemented OP-IMM funct3: 0x" << std::hex
                << funct3 << std::dec << std::endl;
      break;
    }
    break;

  case 0x33: // OP instructions (ADD, SUB, SLL, SLT, SLTU, XOR, SRL, SRA, OR,
             // AND)
    switch (funct3) {
    case 0x0: // ADD / SUB

      if (funct7 == 0x00) { // ADD
        // std::cout << "  Executing ADD x" << rd << ", x" << rs1 << ", x" <<
        // rs2 << std::endl;
        if (rd != 0)
          regs[rd] = regs[rs1] + regs[rs2];

      } else if (funct7 == 0x20) { // SUB
        // std::cout << "  Executing SUB x" << rd << ", x" << rs1 << ", x" <<
        // rs2 << std::endl;
        if (rd != 0)
          regs[rd] = regs[rs1] - regs[rs2];
      } else {
        std::cerr << "Warning: Unimplemented OP funct7 for funct3=0: 0x"
                  << std::hex << funct7 << std::dec << std::endl;
      }
      break;
      // --- TODO: Add other OP instructions (SLL, SLT, SLTU, XOR, SRL, SRA,
      // OR, AND...) ---
    default:
      std::cerr << "Warning: Unimplemented OP funct3: 0x" << std::hex << funct3
                << std::dec << std::endl;
      break;
    }
    break;

  case 0x03: // LOAD instructions (LB, LH, LW, LBU, LHU)
  {
    uint32_t address = regs[rs1] + imm_i;
    switch (funct3) {

    case 0x2: // LW (Load Word)
              // std::cout << "  Executing LW x" << rd << ", " << imm_i <<
              // "(x" << rs1 << ") -> address 0x" << std::hex << address <<
              // std::dec << std::endl;
      if (rd != 0)
        regs[rd] = read_memory<int32_t>(
            address); // Load word (sign-extended by C++ type)
      break;
      // --- TODO: Add LB, LH, LBU, LHU ---
    case 0x0: // LB (Load Byte)

      if (rd != 0)
        regs[rd] = static_cast<int32_t>(
            read_memory<int8_t>(address)); // Sign-extend byte
      break;
    case 0x1: // LH (Load Halfword)
      if (rd != 0)
        regs[rd] = static_cast<int32_t>(
            read_memory<int16_t>(address)); // Sign-extend halfword

      break;
    case 0x4: // LBU (Load Byte Unsigned)
      if (rd != 0)
        regs[rd] = read_memory<uint8_t>(address); // Zero-extend byte
      break;
    case 0x5: // LHU (Load Halfword Unsigned)
      if (rd != 0)
        regs[rd] = read_memory<uint16_t>(address); // Zero-extend halfword
      break;
    default:
      std::cerr << "Warning: Unimplemented LOAD funct3: 0x" << std::hex
                << funct3 << std::dec << std::endl;
      break;
    }
    if (!running)
      return; // Check if memory read failed
  } break;

  case 0x23: // STORE instructions (SB, SH, SW)

  {
    uint32_t address = regs[rs1] + imm_s; // Use S-type immediate
    switch (funct3) {
    case 0x2: // SW (Store Word)
      // std::cout << "  Executing SW x" << rs2 << ", " << imm_s << "(x" <<
      // rs1 << ") -> address 0x" << std::hex << address << std::dec <<
      // std::endl;
      write_memory<uint32_t>(address, regs[rs2]);
      break;
      // --- TODO: Add SB, SH ---
    case 0x0: // SB (Store Byte)
      write_memory<uint8_t>(address, static_cast<uint8_t>(regs[rs2] & 0xFF));
      break;
    case 0x1: // SH (Store Halfword)
      write_memory<uint16_t>(address,
                             static_cast<uint16_t>(regs[rs2] & 0xFFFF));

      break;
    default:
      std::cerr << "Warning: Unimplemented STORE funct3: 0x" << std::hex
                << funct3 << std::dec << std::endl;
      break;
    }
    if (!running)
      return; // Check if memory write failed
  } break;

  case 0x6F: // JAL (Jump and Link) - UJ-Type
             // std::cout << "  Executing JAL x" << rd << ", 0x" << std::hex
             // << (pc - 4 + imm_j) << std::dec << std::endl;
    if (rd != 0) {
      regs[rd] = pc; // Store return address (PC of instruction *after* JAL)
    }
    pc = (pc - 4) + imm_j; // Jump to target address (PC relative)
    // Ensure PC is aligned (although J/B immediates guarantee this if tools
    // are correct)
    if (pc % 2 != 0) {

      std::cerr << "Error: JAL target PC misaligned: 0x" << std::hex << pc
                << std::dec << std::endl;
      running = false;
    }
    pc_changed = true;
    break;

  case 0x67: // JALR (Jump and Link Register) - I-Type
    switch (funct3) {

    case 0x0: {
      uint32_t target_address =
          (regs[rs1] + imm_i) & ~1U; // Calculate target, ensure LSB is 0
      // std::cout << "  Executing JALR x" << rd << ", x" << rs1 << ", " <<
      // imm_i << " -> target 0x" << std::hex << target_address << std::dec <<
      // std::endl;

      if (rd != 0) {
        regs[rd] = pc; // Store return address (PC of instruction *after* JALR)
      }
      pc = target_address;
      pc_changed = true;
    } break;
    default:
      std::cerr << "Warning: Unimplemented JALR funct3: 0x" << std::hex
                << funct3 << std::dec << std::endl;
      break;
    }
    break;

  case 0x63: // BRANCH instructions (BEQ, BNE, BLT, BGE, BLTU, BGEU) - SB-Type
  {
    bool should_branch = false;
    // Note: Comparison uses signed values for BLT/BGE, unsigned for BLTU/BGEU
    switch (funct3) {
    case 0x0: // BEQ (Branch if Equal)
              // std::cout << "  Executing BEQ x" << rs1 << ", x" << rs2 << ",
              // target_offset " << imm_b << std::endl;
      if (regs[rs1] == regs[rs2])
        should_branch = true;
      break;
    case 0x1: // BNE (Branch if Not Equal)

      // std::cout << "  Executing BNE x" << rs1 << ", x" << rs2 << ",
      // target_offset " << imm_b << std::endl;
      if (regs[rs1] != regs[rs2])
        should_branch = true;
      break;
    case 0x4: // BLT (Branch if Less Than - Signed)
      if (static_cast<int32_t>(regs[rs1]) < static_cast<int32_t>(regs[rs2]))
        should_branch = true;
      break;
    case 0x5: // BGE (Branch if Greater Than or Equal - Signed)
      if (static_cast<int32_t>(regs[rs1]) >= static_cast<int32_t>(regs[rs2]))
        should_branch = true;
      break;
    case 0x6: // BLTU (Branch if Less Than - Unsigned)
      if (regs[rs1] < regs[rs2])
        should_branch = true;
      break;
    case 0x7: // BGEU (Branch if Greater Than or Equal - Unsigned)

      if (regs[rs1] >= regs[rs2])
        should_branch = true;
      break;
    default:
      std::cerr << "Warning: Unimplemented BRANCH funct3: 0x" << std::hex
                << funct3 << std::dec << std::endl;
      break;
    }

    if (should_branch) {
      pc = (pc - 4) + imm_b; // Branch is PC-relative
      // Ensure PC is aligned (B immediates guarantee this if tools are
      // correct)
      if (pc % 2 != 0) {
        std::cerr << "Error: Branch target PC misaligned: 0x" << std::hex << pc
                  << std::dec << std::endl;
        running = false;
      }

      pc_changed = true;
      // std::cout << "    Branch taken. New PC: 0x" << std::hex << pc <<
      // std::dec << std::endl;
    }
  } break;

  case 0x37: // LUI (Load Upper Immediate) - U-Type
             // std::cout << "  Executing LUI x" << rd << ", 0x" << std::hex
             // << (imm_u >> 12) << std::dec << std::endl;
    if (rd != 0)
      regs[rd] = imm_u;

    break;

