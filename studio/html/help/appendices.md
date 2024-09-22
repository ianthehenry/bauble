# Language {#language}

Bauble is implemented in the [Janet](https://janet-lang.org/) programming language, and the programs you write in Bauble are just Janet programs that get evaluated with a particular environment already in scope.

If you'd like to learn more about Janet, I wrote a free book called [*Janet for Mortals*](https://janet.guide/) that gives an introduction to the language.

# Notation {#notation}

Before Bauble executes your script, it performs three purely syntactic transformations on it. You can think of these like macros that are implicitly wrapped around every top-level expression, because that's exactly what they are.

## Dot notation {#dot-notation}

Dot notation gives you an easy way to access fields of vectors and structs. Symbols with dots inside them, like `p.x`, will be rewritten to `(. p x)`, where `.` is an ordinary Janet macro defined in the Bauble standard environment.

Dot notation only applies to Janet *symbols*, so if you want to access a field of an expression, you have to invoke it using traditional prefix notation:

```
# this doesn't work!
(camera/perpsective [10 1 0] [0 0 0]).direction

# you have to do this instead:
(. (camera/perpsective [10 1 0] [0 0 0]) direction)
```

Just like in GLSL, dot notation supports vector swizzling:

```example
(def radius [30 0])
(rect 100 :r radius.xyyx)
```

## Pipe notation {#pipe-notation}

Pipe notation is a way to write function applications in a postfix order, sort of like method-chaining in other languages.

```example
# these two lines are the same
(move (circle 50) [100 0])
(circle 50 | move [100 0])
```

This is purely syntactic, like the `->` or `->>` macros in the Janet standard library.

By default the argument on the left of the pipe is inserted just after the first argument. But you can specify a different location for it using `_`:

```example
# these two lines are the same
(move (circle 50) [100 0])
([100 0] | move (circle 50) _)
```

You can insert the left-hand side at any position you want:

```example
# these two lines are the same
(move (circle 50) [100 0])
(move | _ (circle 50) [100 0])
```

I don't know why you'd want to do that, but it's nice to know you have the option.

Notice that if there are multiple expressions to the left of the pipe, as in `circle 50 | move ...`, they are implicitly wrapped in parentheses. But if there is only a single expression, it is *not* wrapped. So if you have a function that takes no arguments, you need to explicitly call it to use it with pipe notation: `((get-my-cool-shape) | move [50 0])`.

Janet *normally* uses the pipe character as a way to create single-expression anonymous functions:

```
# this won't work in bauble
(map |(* 2 $) [1 2 3]) # [2 4 6]
```

But Bauble co-opts the character for postfix application, and doesn't have anything to replace it with. So you just can't. You have to make a normal `fn`:

```
(map (fn [$] (* 2 $)) [1 2 3]) # [2 4 6]
```

It's not too bad. It's a good tradeoff.

## Infix notation {#infix-notation}

In addition to `|`, four other symbols are treated specially: `+`, `-`, `*`, and `/`. These are interpreted as infix symbols, and they get rewritten like so:

```example
# these two lines are the same
(circle (+ 10 20))
(circle (10 + 20))
```

There is no order of operations or precedence in Bauble's infix notation. Operations always happen from left to right:

```example
# these two lines are the same
(circle (* (+ 5 5) 10))
(circle (5 + 5 * 10))
```

You can still use prefix notation in Bauble; `(+ 10 20)` will not be rewritten to anything. `+` `-` `*` `/` are only special when they appear in the middle of a form like that. (Sometimes it's nice to use the variadic prefix forms.)

If there are multiple forms to the left of an infix operator, they will be implicitly wrapped in parentheses, just like pipe notation:

```example
# these two lines are the same
(circle (+ (* (sin+ t) 50) 50))
(circle (sin+ t * 50 + 50))
```

As an annoying corollary to this, there's no way to pass any of these functions around as regular first-class functions. For example, this code would work in regular Janet:

```
(reduce + 0 [1 2 3])
```

But in Bauble, that gets rewritten to:

```
(+ reduce (0 [1 2 3]))
```

And you'll get an inscrutable error. To work around this, Bauble defines the symbols `@+`, `@-`, `@*`, and `@/` as aliases for the operators. These symbols have the same value, but the infix notation won't treat them specially, so you can safely write this:

```
(reduce @+ 0 [1 2 3])
```

Although Bauble's infix notation has no concept of precedence, the infix notation pass runs *after* the pipe notation pass. This means that you can use `_` to move an expression "over" an infix operator. In practice this is useful to say e.g. `(cos t | 1 - _)`.

If you're ever confused by what Bauble's notation is doing, you can see what your code expands to like this:

```example
(defmacro show-syntax [x] (pp x) x)

(show-syntax (p.x | 1 + 2 * _))
```

# Bauble CLI {#cli}

Okay so there is, technically, a command-line version of Bauble. You can run it locally using native OpenGL instead of WebGL and . And it has exciting features like exporing an OBJ file, and rendering non-square images.

But it's like pre-pre-alpha quality, and the only way to get it is to build it from source, and I'm sorry about that. There are instructions [in the GitHub repo](https://github.com/ianthehenry/bauble#cli).
