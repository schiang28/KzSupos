import sys
import re

SHOW_TRACE = True

if len(sys.argv) != 2:
    print("Usage: ./rm.py <file>")
    sys.exit(1)

with open(sys.argv[1], "r") as f:
    # read the list of instructions
    machine = [
        line
        for line in f.read().lower().replace(" ", "").splitlines()
        if line and line[0] != "#"
    ]

# read the configuration (l, r0, r1, ...)
config_contents = re.search(r"(\d+(,\d+)*)", machine[0])
if config_contents is None:
    print(
        "Invalid format for initial configuration. Expected: (first "
        "instruction, ...initial register values)"
    )
    sys.exit(2)
config = [int(r) for r in config_contents.group().split(",")]
i, registers = config[0], config[1:]

instructions = {}
for line in machine[1:]:
    label_body = line.split(":")
    if len(label_body) != 2:
        print(
            f"Invalid format for instruction {line}. Expected format: "
            f"index: operation, ...arguments"
        )
        sys.exit(3)
    index, instruction_string = int(label_body[0]), label_body[1]
    if index in instructions:
        print(f"Multiple instructions have index {index}")
        sys.exit(4)
    instruction = {"operation": None, "register": None, "arguments": None}
    arguments = instruction_string.split(",")
    if len(arguments) == 1:
        if arguments[0] == "halt":
            instruction["operation"] = "halt"
    else:
        op, args = arguments[0], arguments[1:]
        op_match = re.search(r"\w+\(r\d+\)", op)
        if op_match is None:
            print(f"Invalid instruction: {line}")
            sys.exit(5)
        operation, register_string = op[:-1].split("(")

        if operation == "inc":
            if len(args) != 1:
                print(f"Operation `inc` should take one argument: {line}")
        elif operation == "dec":
            if len(args) != 2:
                print(f"Operation `dec` should take two arguments: {line}")
        else:
            print(f"Invalid instruction (invalid operation): {line}")
            sys.exit(6)

        register = int(register_string[1:])
        instruction["operation"] = operation
        instruction["register"] = register
        instruction["arguments"] = [int(arg) for arg in args]
    if instruction["operation"] is None:
        print(f"Invalid instruction: {line}")
        sys.exit(6)
    instructions[index] = instruction

pc = 0
if SHOW_TRACE:
    print("pc, current index, register values")
while True:
    if SHOW_TRACE:
        print(f"{pc}, {i}, {tuple(registers)}")
    pc += 1

    if i not in instructions.keys():
        print("Erroneous HALT")
        break

    inst = instructions[i]
    op, reg, args = inst["operation"], inst["register"], inst["arguments"]

    if reg is not None and reg >= len(registers):
        for _ in range(reg - len(registers) + 1):
            registers.append(0)

    if op == "halt":
        print("Proper HALT")
        break
    if op == "inc":
        registers[reg] += 1
        i = args[0]
        continue
    if op == "dec":
        if registers[reg] > 0:
            registers[reg] -= 1
            i = args[0]
        else:
            i = args[1]
        continue

print()
print(f"Final register state: {tuple(registers)}")
print(f"Number of operations: {pc}")
print(f"Last instruction index: {i}")
