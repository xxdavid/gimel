![Logo](assets/logo.svg)

Gimel is a toy functional language that compiles to machine code using LLVM.

```haskell
data List = Cons Int List | Nil

map f l = case l do
    Nil -> Nil
    Cons x xs -> Cons (f x) (map f xs)
end
```

## Features
### Functions
Functions are the basic blocks of every functional program. There are no mutable variables, only constants that are in essence (nullary) functions as well. Their definition and usage (application) are very terse, see for example a function that computes the volume of a cylinder:

```elixir
pi = 3 # roughly
area r = pi * r * r
volume r h = h * area r
```

### Static typing
Gimel employs strong static typing. Every program is type-checked before it's compiled, if it doesn't pass, no compilation is gonna happen. This ensures your program won't have type errors during runtime.

There are three categories of types in Gimel: integers (`Int`), function types (e.g. `Int -> Bool`) and *algebraic data types* (ADTs for short).
The last ones are user-defined data types, each of them can have multiple forms (differentiated by so-called *constructors*) and they can wrap multiple values of other types. This is best illustrated with an example.

```haskell
data List = Nil | Cons Int List

length l = case l do
    Nil -> 0
    Cons x xs -> length xs + 1
end

three = length (Cons 42 (Cons 123 (Cons 1984 Nil)))
```

An instance of the declared data type `List` is always either a `Nil` (an empty list) or a `Cons` with an associated integer (element of the list) and the rest of the list. The expression `(Cons 42 (Cons 123 (Cons 1984 Nil)))` thus represents a list of 42, 123, and 1984.

The `case` expression is the only way how to unpack an ADT. You list all the constructors for the given type and what the value of the whole expression should be in each case. If you leave out a constructor, you'll get a type error.

You can use ADTs for lists, trees, optional values and so on. There is one predefined algebraic data type, and that is `Bool` with constructors `True` and `False`. All comparison functions return this type and you can match it like any other ADT (with `case`).

Writing down all the types can be tedious so the good thing is that everything is *inferred automatically* and there are no type annotations.
So far so good, there is one big thing missing though — *polymorphism*. Types in Gimel cannot be polymorphic which means you cannot define
a general `map` or `id` function but you have to define a separate one for each type you want to use it with.

### Lazy evaluation
Expressions in Gimel evaluate only when needed. This gives you the ability to, for instance, work with infinite data structures. (And makes your programs far from performant on the other hand.)

```haskell
natsFrom n = Cons n (natsFrom (n + 1))
nats = natsFrom 1

take n l = case n == 0 do
    True -> Nil
    False -> case l do
        Nil -> Nil
        Cons x xs -> Cons x (take (n - 1) xs)
    end
end

firstTenNats = take 10 nats
```

### Compilation to machine code
Programs in Gimel are first compiled to instructions for an abstract [G-Machine](https://link.springer.com/chapter/10.1007/3-540-15975-4_50) that knows how to evaluate expressions lazily in an efficient way (not really but more than the naïve lazy evaluation) using [graph reduction](https://en.wikipedia.org/wiki/Graph_reduction). It's similar to what one compilation phase of GHC compiles Haskell to (it also uses G-Machine but a more advanced version). The G-Machine instructions are then compiled to LLVM IR instructions and these are then handed to the LLVM compiler that compiles it down to optimized machine code tailored for your architecture.

As we've seen, each program is a set of function and ADT definitions but that doesn't form a complete program, one thing is missing for the compiler to know what to do. It is the entry point, the root expression that should be evaluated. For this purpose, each program has to include a nullary function called `main`. This function is always evaluated when the compiled program is run and the evaluated expression is then printed on the standard output.

## Using the compiler

### Building
The Gimel compiler is written in Haskell and uses [Stack](https://docs.haskellstack.org/en/stable/) as a build tool. In an ideal world, everything needed to build the compiler should be the magical

```bash
stack build
```

After that (actually, it does not matter when) you can run `stack install` and the compiler binary will be copied to some folder on your computer.

### Running
Use the `gimel` binary to compile Gimel source files. You can pass `-o` to specify the output file.

```bash
$ cat > fact_list.gm << EOF
fact n = case n == 0 do
    True -> 1
    False -> n * fact (n - 1)
end

data List = Nil | Cons Int List

map fn list = case list do
    Nil -> Nil
    Cons x xs -> Cons (fn x) (map fn xs)
end

main = map fact (Cons 5 (Cons 10 Nil))
EOF

$ gimel fact_list.gm
$ ./fact_list
(Cons 120 (Cons 3628800 Nil))
```

## Why the name?
Gimel is a Hebrew letter (ג) that resembles lambda and so it looked like a perfect name for a functional language to me.

## Acknowledgement
I was inspired by the beatiful series [Compiling a Functional Language Using C++](https://danilafe.com/blog/00_compiler_intro/). It helped me to understand the G-Machine and I looked into it from time to time when I got lost with the implementation.

I used `llvm-hs-pure` for generating the LLVM IR and its documentation is very sparse (as for almost every Haskell library) so [this awesome talk](https://www.youtube.com/watch?v=_Qb0mL72l2o) and [this excellent blog post](https://blog.josephmorag.com/posts/mcc3/) were a great help to me.