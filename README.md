# Parser Project

## Overview
This is a compiler frontend project that includes a scanner (lexical analyzer) and parser for a simple programming language. The parser generates an Abstract Syntax Tree (AST) based on the grammar defined in LL1.ebnf.

## Project Structure
- **Scanner**: Performs lexical analysis, converting source code into tokens
- **Parser**: Performs syntax analysis according to the grammar rules
- **Symbol Table**: Manages symbol information
- **Abstract Syntax Tree**: Represents the parsed program structure

## Files
- `ast.cpp/h`: Abstract Syntax Tree implementation
- `FileDescriptor.cpp/h`: File handling functionality
- `main.cpp`: Program entry point
- `Parser.cpp/h`: Parser implementation
- `Scanner.cpp/h`: Lexical analyzer implementation
- `stable.cpp/h`: Symbol table implementation
- `stentry.h`, `stlist.cpp/h`: Symbol table entry and list handling
- `LL1.ebnf`: Grammar definition in Extended Backus-Naur Form

## Example Files
- `Input.txt`: Sample input code
- `ex1.txt`, `ex2.txt`, `ex3.txt`, `ex4.txt`: Additional test examples
- `output.txt`: Generated output

## Language Features
The parser supports a language with the following features:
- Variable declarations with different types (integer, float, boolean, string)
- Constants
- Functions and procedures
- Control statements (if-then-else, while loops, for loops)
- Read and write operations
- Expressions with operators

## How to Build and Run

### Building the Parser
```
g++ *.cpp -o parser
```

### Running the Parser
```
.\parser
```

The parser reads from `Input.txt` by default and processes the code according to the defined grammar.

## Grammar
The language follows an LL(1) grammar as defined in `LL1.ebnf`. Main components include:
- Programs consist of declaration lists
- Declarations can be variables, constants, functions, or procedures
- Expressions support arithmetic and logical operations
- Control structures include if-then-else statements and loops

## Notes
- Comments in the source code begin with `##`
- The parser validates both syntax and semantics of the input program
- The generated AST can be used for further compilation stages
