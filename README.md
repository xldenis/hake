# hake

This is **not** meant to be a full build system like `rake` / `make` for that use `shake` instead. `hake` is meant to provide an easy interface for automated tasks, similar to how `rake` is used to script most cli tasks in ruby.

Yes, I am aware that there is a name collision on Hackage already :P

I'll need to find a better name for this package (or leverage the _actual_ Hake package).

Currently, the package provides an executable named `hake` that serves as a haskell preprocessor.

To use Hake, create an executable target that contains all your task files. Make sure the `Main` file of your target looks like

```haskell
{-# OPTIONS_GHC -F -pgmF hake #-}
```

This will produce an executable capable of running your hake tasks.

### Hake Syntax

A hake task consists of four things: a name, an optional description, a list of dependencies and an action.

There is a convenient set of combinators provided to help build your tasks up

_NOTE TO SELF:_ maybe provide some sort of writer monad combinators to easily build up a list of tasks

```haskell
description "this task does some very important things " $
  task "name" [] $ do
    putStrLn "SRS BSNS"

```

#### Dependencies

A task can provide a list of dependencies that will be run before the task. These should be tasks themselves and will be evaluated in left-to-right order.

#### Namespaces

To help organize tasks into logical groups, hake provides a `namespace` combinator. Namespaces prefix the contained tasks with the supplied argument.

```haskell
namespace "something" $
  [ task "name" [] $ pure ()
  , task "other" [] $ pure ()
  ]
```

Produces two tasks called `something:name` and `something:other` respectively.


