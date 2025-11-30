import sys


def read_all() -> str:
    return sys.stdin.read()

def grow_zeroed(tape: list[int], new_len: int) -> list[int]:
    old_len = len(tape)
    if new_len > old_len:
        # allocate new, copy old, zero the rest
        return tape + [0] * (new_len - old_len)
    return tape

def match_forward(prog: str, pos: int) -> int:
    n = len(prog)
    depth = 1
    i = pos + 1
    while i < n:
        ch = prog[i]
        if ch == '[':
            # is_command '['
            depth += 1
        elif ch == ']':
            # is_command ']'
            depth -= 1
            if depth == 0:
                return i
        i += 1
    return -1

def match_backward(prog: str, pos: int) -> int:
    depth = 1
    i = pos - 1
    while i >= 0:
        ch = prog[i]
        if ch == ']':
            # is_command ']' while scanning backward
            depth += 1
        elif ch == '[':
            # is_command '[' while scanning backward
            depth -= 1
            if depth == 0:
                return i
        i -= 1
    return -1

def ensure_tape_capacity(tape: list[int], needed_index: int) -> list[int]:
    needed_len = needed_index + 1
    if len(tape) < needed_len:
        # grow to needed_len
        return grow_zeroed(tape, needed_len)
    return tape

def cell_inc(v: int) -> int:
    nv = v + 1
    if nv > 255:
        return 0
    return nv

def cell_dec(v: int) -> int:
    nv = v - 1
    if nv < 0:
        return 255
    return nv

def run_bf(program: str):
    pc = 0
    plen = len(program)
    dp = 0

    # start with 1 zeroed cell
    tape = [0]

    while pc < plen:
        op = program[pc]
        is_command = False

        if op == '>':
            # '>'
            is_command = True
            dp += 1
            tape = ensure_tape_capacity(tape, dp)
            pc += 1

        elif op == '<':
            # '<'
            is_command = True
            dp -= 1
            if dp < 0:
                dp = 0
            pc += 1

        elif op == '+':
            # '+'
            is_command = True
            tape[dp] = cell_inc(tape[dp])
            pc += 1

        elif op == '-':
            # '-'
            is_command = True
            tape[dp] = cell_dec(tape[dp])
            pc += 1

        elif op == '.':
            # '.'
            is_command = True
            sys.stdout.write(chr(tape[dp]))
            sys.stdout.flush()
            pc += 1

        elif op == '[':
            # '['
            is_command = True
            if tape[dp] == 0:
                j = match_forward(program, pc)
                if j < 0:
                    # unmatched '['
                    pc += 1
                else:
                    pc = j + 1
            else:
                pc += 1

        elif op == ']':
            # ']'
            is_command = True
            if tape[dp] == 0:
                pc += 1
            else:
                j2 = match_backward(program, pc)
                if j2 < 0:
                    # unmatched ']'
                    pc += 1
                else:
                    pc = j2 + 1

        if not is_command:
            # ignore non-BF chars
            pc += 1

def main() -> int:
    program = read_all()
    run_bf(program)
    return 0

if __name__ == "__main__":
    sys.exit(main())
