
# Lambda Calculus Interpreter

An interactive lambda calculus interpreter written in Haskell. This project was created as part of a university course on advanced functional programming and demonstrates core techniques like monads, functors, and monadic parsing.

## 📚 Features

- Parse and evaluate untyped lambda calculus expressions
- Supports full evaluation to normal form via β-reduction
- Step-by-step reduction with trace output
- Pretty-printed output with Unicode support (e.g., `λx.x`)
- REPL with helpful commands and sample expressions

## 🧠 Functional Programming Techniques Used

- **Custom AST** using Haskell's algebraic data types and a `Variable a = Bound | Free a` system
- **Monads and Functors** for structure traversal and substitution
- **Monadic parsing** using [`Megaparsec`](https://hackage.haskell.org/package/megaparsec)
- **Pretty printing** using [`prettyprinter`](https://hackage.haskell.org/package/prettyprinter)
- **Traversable and Applicative** type class instances for structure mapping

## 🧪 Sample Expressions

```text
λx.x                     -- Identity function
(λx.x) y                 -- Apply identity to y
(λx.λy.x)                -- Constant function
((λx.λy.x) a b)          -- Apply constant to a and b
(λf.(λx.(f (f x))))      -- Church numeral 2
```

## 🚀 Usage

### Prerequisites

You need [GHC](https://www.haskell.org/ghc/) and [Stack](https://docs.haskellstack.org/en/stable/README/) or Cabal installed.

### Build and Run

```bash
stack build
stack exec lambda-calculus
```

### REPL Commands

| Command              | Description                                     |
|----------------------|-------------------------------------------------|
| `:q`                 | Quit the interpreter                            |
| `:h`                 | Show help and available commands                |
| `:eval <expr>`       | Evaluate the expression to normal form          |
| `:step <expr>`       | Perform step-by-step evaluation with prompts    |
| `:trace <expr>`      | Print all evaluation steps in order             |
| `:sam`               | Show sample lambda calculus expressions         |

### Expression Syntax

- Lambda abstraction: `λx.x` or `\x.x`
- Application: `(λx.x) y`
- Parentheses control evaluation order

### Example

```text
λ> :eval (λx.x) y
y

λ> :step ((λx.λy.x) a b)
Step 1: ((λx.λy.x) a b)
⏎ to continue, :q to quit> 
Step 2: (λy.a b)
⏎ to continue, :q to quit> 
Step 3: a
(Expression is in normal form)
```

## 📁 Project Structure

| File           | Purpose                                      |
|----------------|----------------------------------------------|
| `LambdaAST.hs` | Abstract syntax tree and pretty-printing     |
| `Evaluator.hs` | Evaluation logic with β-reduction            |
| `Parser.hs`    | Parser for lambda calculus expressions       |
| `Main.hs`      | REPL interface and command dispatch          |




