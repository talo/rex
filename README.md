# 🦖

Rex (short for Rush Expressions) is a strongly-typed domain-specific functional
programming language for defining complex workflows in the Rush platform.

It comes with a set of built-in functions (like `map`, `fold`, `zip` etc) but
all other functions are defined and implemented by the host VM. These host
functions are typically high-performance computing modules that are intended to
be dispatched to supercomputers.

Right now, all of that is managed by a proprietary VM implementation (known as
`tengu`), but we will slowly be moving more and more of that logic into Rex
itself (mostly so that local dev is easier).

## Example

```rex
-- Double everything in a list
map (λx → 2 * x) [1, 2, 3, 4]

-- Or, if you prefer currying
map ((*) 2) [1, 2, 3, 4]
```

## Design

1. `rex-lexer` turns a string into tokens.
2. `rex-parser` makes sure the tokens are syntactically valid and transforms
   them into an AST of expressions.
3. `rex-type-system` uses a Hindley-Milner type system to run type inference.
4. `rex-engine` evalutes the type inferred AST and implements the built-in
   functions.
5. `rex` is the command-line tool that puts it all together.

Some limitations:

* We do not allow overloaded functions that have a different number of
  parameters. This is because our syntax makes it impossible to differentiate
  between calls to a function with fewer parameters, and calls to a function
  with more parameters that are meant to result in currying.
* We do not allow polymorphic functions to be overloaded. This is because
  eventually we want to move away from overloading and instead move towards type
  classes (this is the "right" way to support overloading).

## Contribute

Made with ♡ by QDX
