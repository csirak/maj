# Maj: A BASED Language Inspired by Adam Majmudar's GPU Project

## Overview

Maj is a high-performance, statically typed programming language. Inspired by [Adam Majmudar's GPU project](https://github.com/adam-maj/tiny-gpu) and built using Scala.

## Features

- **Static Typing**: Catch errors early with a strong, statically typed system that ensures type safety without
  sacrificing flexibility.
- **Performance Optimization**: Automatic performance optimizations using `MajIR`, reducing the need for manual
  optimization.
- **Inline Assembly**: Write low-level code directly in the language using inline assembly, enabling fine-grained
  control.
- **Composite Types**: Define complex data structures with ease using composite types, including structs, and more.
- **Concise Syntax**: A syntax that is expressive yet concise, minimizing boilerplate while maximizing readability and
  productivity.

## Installation

### Prerequisites

- Java Development Kit (JDK) 8 or above
- Scala 2.13.x
- An Adam Majmudar GPU

### Steps

1. **Clone the repository:**
   ```bash
   git clone https://github.com/csirak1528/maj.git
   cd maj
   ```

2. **Run the project:**
   ```bash
   sbt run
   ```

## Features

### Variables

There are 2 kinds of variables in Maj. Constants and Variables. Constants are declared using the `const` keyword and
variables are declared using the `var` keyword. Constants are immutable and cannot be changed once assigned. Variables
can be reassigned.

```
func main() {
    const x = 10;
    var y = true;
    var z = 'f';
    
    z = 't';
    y = false;
}

```

### Control Flow

Maj supports the following control flow statements:

- `if` statements
- `else if` statements
- `while` loops

```
func main() {
    var x = 0;
    var y = 10;
    
    if (x < y) {
        x = x + 1;
    } else {
        x = x - 1;
    }
    
    while (x < y) {
        x = x + 1;
    }
}
```

### Functions

Functions are declared using the `func` keyword. Functions can have parameters and return values. Functions can also be
recursive. If there is no return type annotation the compiler will assume return type of `void`.

```
func add(x: int, y: int): int {
    return x + y;
}
```

### Types

Maj supports the following types:

- `int`: 32-bit signed integer
- `char`: 8-bit character
- `bool`: boolean
- `void`: no return value or null
- `struct`: user-defined data structure (COMING SOON)

These types can be composed using `&` and `|` operators to create composite types.

```
type IntOrChar = int | char;

type Point = struct {
    x: int,
    y: int
};

type Height = struct {
    z: int
};

type Point3D = Point & Height;
```

### Inline Assembly

Maj supports inline assembly for low-level operations. Inline assembly is enclosed in a block and can be used to write
low-level code directly in the language.

Here we use the RISC-V UART interface to output chars to the screen in ASCII

```
func putchar(letter: char): void {
    asm {
        li       t0, UART_BASE             # load UART base address

        .Lputchar_loop:
                lw       t1, UART_REG_TXFIFO(t0)   # read UART TX FIFO status
                li       t2, 0x80000000
                and      t1, t1, t2
                bnez     t1, .Lputchar_loop        # if TX FIFO is full, wait
                sw       a0, UART_REG_TXFIFO(t0)   # write character to TX FIFO
    }
}
```
