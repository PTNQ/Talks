# WebAssembly Presentation

---

## What is WebAssembly

- WebAssembly (abbreviated Wasm) is

- a binary instruction format for a stack-based virtual machine.

- Wasm is designed as a portable compilation target for programming languages,

- enabling deployment on the web for client and server applications.

Definition from webassembly.org

---

## What is WebAssembly

- WebAssembly: it's neither web, nor assembly.

---

## Binary Instruction Format

- Compact to be used on the web

- Close to machine instructions while remaining portable

- WAT: WebAssembly Text Format

- Look! S-expressions, like in Lisp!

---

## Stack-Based Virtual Machine

- Not a register machine unlike most computers

- Only numerical data type

- Secure sandbox

- Linear memory

---

## What about Java

- Java Applet :-/

- No DOM integration

- Not sandboxed enough

- JVM is (almost) limited to Java

---

## Compilation Target for Programming Languages

- Many languages can be compiled to wasm

- Most popular: C/C++ and Rust

- In Rust

`cargo build --target wasm32-unknown-unknown`

---

## Deployment on the Web

- Supported by 4 major browsers.
- Fourth language to run natively in browser with: HTML, CSS, and JavaScript
- It runs on the client-side, on the browser.
- But it relies on JavaScript.

---

## Beyond the Web

- WebAssembly System Interface (WASI)

- WebAssembly in the cloud

---

