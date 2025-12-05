#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// Read the entire stdin into a dynamically growing buffer
static char *read_all(void) {
  size_t size = 0;
  char *result = malloc(1);
  if (!result)
    exit(1);

  int x;
  while ((x = getc(stdin)) != EOF) {
    char c = (char)x;
    char *new_result = malloc(size + 2);
    if (!new_result)
      exit(1);
    memcpy(new_result, result, size);
    new_result[size] = c;
    size++;
    new_result[size] = '\0';
    free(result);
    result = new_result;
  }
  return result;
}

// Allocate a new buffer, copy the old one, and zero the rest
static char *grow_zeroed(char *s, size_t old_len, size_t new_len) {
  if (new_len <= old_len)
    return s;
  // allocate new, copy old, zero the rest
  char *t = malloc(new_len);
  if (!t)
    exit(1);
  memcpy(t, s, old_len);
  memset(t + old_len, 0, new_len - old_len);
  free(s);
  return t;
}

// Scan forward from '[' to its matching ']'
static int match_forward(const char *prog, int pos) {
  int depth = 1;
  for (int i = pos + 1; prog[i] != 0; i++) {
    int ch = prog[i];
    if (ch == '[') { // is_command '['
      depth++;
    }
    if (ch == ']') { // is_command ']'
      depth--;
      if (depth == 0)
        return i;
    }
  }
  return -1;
}

// Scan backward from ']' to its matching '['
static int match_backward(const char *prog, int pos) {
  int depth = 1;
  for (int i = pos - 1; i >= 0; i--) {
    int ch = prog[i];
    if (ch == ']') { // is_command ']' while scanning backward
      depth++;
    }
    if (ch == '[') { // is_command '[' while scanning backward
      depth--;
      if (depth == 0)
        return i;
    }
  }
  return -1;
}

// Ensure the tape is large enough for the given index
static char *ensure_tape_capacity(char *tape, size_t *len,
                                  size_t needed_index) {
  size_t needed_len = needed_index + 1;
  if (*len < needed_len) {
    tape = grow_zeroed(tape, *len, needed_len * 2);
    *len = needed_len * 2;
  }
  return tape;
}

// Increment cell, wrapping from 255 -> 0
static int cell_inc(int v) {
  int nv = v + 1;
  if (nv > 255)
    return 0;
  return nv;
}

// Decrement cell, wrapping from 0 -> 255
static int cell_dec(int v) {
  int nv = v - 1;
  if (nv < 0)
    return 255;
  return nv;
}

// Run a Brainfuck program
static void run_bf(const char *program) {
  int pc = 0;
  int plen = (int)strlen(program);
  int dp = 0;

  // start with 1 zeroed cell
  size_t tape_len = 1;
  char *tape = calloc(1, tape_len);
  if (!tape)
    exit(1);

  while (pc < plen) {
    int op = program[pc];
    int is_command = 0;

    if (op == '>') {
      // '>'
      is_command = 1;
      dp++;
      tape = ensure_tape_capacity(tape, &tape_len, dp);
      pc++;
    } else if (op == '<') {
      // '<'
      is_command = 1;
      dp--;
      if (dp < 0)
        dp = 0;
      pc++;
    } else if (op == '+') {
      // '+'
      is_command = 1;
      int cur = (unsigned char)tape[dp];
      cur = cell_inc(cur);
      tape[dp] = (char)cur;
      pc++;
    } else if (op == '-') {
      // '-'
      is_command = 1;
      int cur = (unsigned char)tape[dp];
      cur = cell_dec(cur);
      tape[dp] = (char)cur;
      pc++;
    } else if (op == '.') {
      // '.'
      is_command = 1;
      putchar((unsigned char)tape[dp]);
      fflush(stdout);
      pc++;
    } else if (op == '[') {
      // '['
      is_command = 1;
      int cur = (unsigned char)tape[dp];
      if (cur == 0) {
        int j = match_forward(program, pc);
        if (j < 0) {
          // unmatched '['
          pc++;
        } else {
          pc = j + 1;
        }
      } else {
        pc++;
      }
    } else if (op == ']') {
      // ']'
      is_command = 1;
      int cur = (unsigned char)tape[dp];
      if (cur == 0) {
        pc++;
      } else {
        int j = match_backward(program, pc);
        if (j < 0) {
          // unmatched ']'
          pc++;
        } else {
          pc = j + 1;
        }
      }
    }

    if (!is_command) {
      // ignore non-BF chars
      pc++;
    }
  }

  free(tape);
}

int main(void) {
  char *program = read_all();
  run_bf(program);
  free(program);
  return 0;
}
