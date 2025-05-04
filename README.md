## Lambda Calculus Interpreter 

I was introduced to this concept in one of the first lectures in this course, and I found it fascinating that a calculus with only three syntactic elements was powerful enough to express every computation that a computer will ever be able to do. I was therefore curious to become more comfortable with this notation, and to learn more about how it influences the design of programming languages, and its influence on the compiler. I felt that this was an important and valuable concept to understand more deeply, and I was curious to also become more comfortable with fundamental concepts in programming language design, to understand and use parser tools, interpretation and compilation more clearly. 

Since much of this project emphasizes my own desire to learn these things, I think it will prove useful to make this interpreter to be an educational tool. The focus of the project is first and foremost to create a functioning lambda calculus interpreter, and secondly, to make it available to a user as a read-evaluate-print-loop interface on the web. It would also be interesting to make it an option to compile a lambda expression to SKI combinators, to see the connection between the two clearly as well. 

## How did it go?

All in all, I am pretty satisfied with how it turned out. I was probably a little too bad at Haskell when I started this course, and I quickly fell off the wagon as I had overlapping lectures in both courses. It has been very interesting to get a clearer view of what the possibilities are in Haskell, and what you can achieve with it if you get proficient with it. I hope the project is decent enough. 

## How to use my project

The program takes your input, written in standard lambda notation:

\x.x x 

which then outputs the unicode version

λx_1.x x

and then if you hit enter, you will see
"Performed substituttion"

And then you'll get the unicode version of that, and that loop will go on until the expression is in normal form. 

## Functional Programming Techniques
A description of the functional programming techniques applied in the project. This includes:

    Describing some of the relevant theory behind the technique. 
    Demonstrating your understanding of the techniques.
    Explaining how the technique was applied in the project.
    An evaluation of how well suited the technique was for the purpose.


## LambdaAST.hs

In defining our Abstract Syntax Tree (AST), we employ a number of core functional programming concepts and type classes that allow us to write elegant and compositional code.
Algebraic Data Types (ADTs)

We begin by defining the lambda calculus syntax using Haskell's algebraic data types:

```haskell
data Variable a = Bound | Free a

data Lambda a = Var (Variable a)
              | App (Lambda a) (Lambda a)
              | LamAbs String (Lambda (Variable a))
```

This separates bound variables from free ones, which is essential for correctly implementing alpha conversion - renaming variables to avoid variable capture - and beta reduction (substitution). The Lambda type is also parametrically polymorphic, allowing us to define a lambda term over any kind of variable representation — strings, integers, or even more structured forms.


To make our Variable and Lambda types composable, we define instances of several key type classes:

### 1. Functor

* Allows us to apply a function to a value “inside” a structure (e.g., mapping over all free variables in a lambda term).

### 2. Applicative
* Extends functors with the ability to apply functions embedded in context to arguments in context — we don’t use it extensively in this file, but define it for completeness and uniformity.

### 3. Monad 
* The most powerful of the three, allowing us to define >>= (bind), which in this case provides the foundation for implementing substitution. In lambda calculus, substitution is replacing a variable with an expression. Since lambda expressions can be nested arbitrarily deep, monads offer a natural recursive pattern for traversing and modifying the structure.

```haskell
instance Monad Lambda where
  (Var a)       >>= f = f a
  (App t u)     >>= f = App (t >>= f) (u >>= f)
  (LamAbs i t)  >>= f = LamAbs i (t >>= mapM f)
```
Here, we see how the mapM function is used in LamAbs to traverse the body, applying a monadic transformation to each free variable, while leaving bound variables untouched — just as the theory demands.

### Traversable and Foldable

We also derive Traversable and Foldable for both Variable and Lambda, which allow generic operations like collecting all free variables (foldMap) or applying an effectful function to all variables (traverse). These capabilities become useful in the evaluator and for implementing alpha conversion.

### Pretty-printing with Context Awareness

We also define a prettyExpr function using the prettyprinter library, which formats lambda terms into human-readable form. This is enriched by awareness of variable context (free vs. bound), and supports Unicode lambda symbols (λ), enabling us to produce output close to textbook notation.

### Summary

By designing our AST this way, we embed the structural semantics of lambda calculus into Haskell’s type system. The use of monads, functors, and polymorphism gives us a declarative and modular way to manipulate expressions. These abstractions not only simplify the implementation of key algorithms, but also provide a foundation that aligns closely with both the theoretical and practical demands of lambda calculus.
2. Parser.hs

Our parser for lambda calculus expressions is built using the Megaparsec library — a powerful and composable parser combinator framework.
Parser Combinators and Monadic Parsing

Parser combinators are a functional pattern where we build complex parsers from small, reusable components. Each parser is a first-class value that consumes part of an input string and produces either a result or an error. Since parsers are monads, we can sequence them using do notation.

Here’s a simplified example:

abstractionParser :: Parser (Lambda String)
abstractionParser = do
  _ <- lexeme (char '\\' <|> char 'λ')
  var <- identifier
  _ <- symbol "."
  body <- expressionParser
  return (lam var body)

This parser consumes a lambda abstraction of the form λx. body. The function lam is responsible for converting the parsed variable into a Bound form in the body — this is crucial for distinguishing between bound and free variables, and is part of our hygienic handling of variable scope.
Megaparsec Integration

We chose Megaparsec because:

    It has great error messages and debugging support.

    It provides built-in support for whitespace and symbol parsing.

    It integrates cleanly with Haskell’s monadic and applicative syntax.

Key parser combinators used include:

    <|> for alternatives (e.g., variable vs. abstraction)

    between and parens for grouping

    many and some for repetition

    lexeme and symbol to handle spacing consistently

Parsing Unicode and Free vs. Bound Variables

We also support parsing λ (Unicode) and ASCII \ as interchangeable lambda abstractions. By lifting parsed strings into our Lambda String representation and converting them with lam, we ensure all variables are correctly annotated with Bound or Free.
Summary

Using Megaparsec allowed us to build a clear, modular, and robust parser using purely functional techniques. The compositional style of parser combinators fits naturally with the declarative nature of lambda calculus, and integrating it with our well-typed AST ensures correctness at every step.

Examples:

We are aiming to parse an expression such as λx.x, the identity function.
The user provides an input, "λx.x", which is then treated by the lam function:

```
lam :: String -> Lambda String -> Lambda String
lam x t = LamAbs x (fmap s t)
  where 
    s x' = if x' == x then Bound else Free x' 
```

Which gives us 
```
LamAbs "x" (Var Bound)
```


## Self evaluation 

The most rewarding part of this project was to learn more about parsing. 

## 
