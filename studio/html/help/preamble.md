# Help

underconstruction.gif

- [The language](#language)
- [Bauble notation](#notation)
    - [Dot notation](#dot-notation)
    - [Pipe notation](#pipe-notation)
    - [Infix notation](#infix-notation)
- [API Reference](#reference)

First things first: there's a [Discord where you can ask questions](https://discord.gg/NzR375gJH6), or you're welcome [to use GitHub discussions](https://github.com/ianthehenry/bauble/discussions). Or <a href="mailto:ianthehenry@gmail.com?subject=Let's talk about Bauble">to shoot me an email</a>. I love talkin' Bauble. Not that... not that help forums or human interaction are an excuse for bad documentation. That's definitely not why I'm bringing this up first. why would you even assume

Second things second: Bauble's autocomplete is Not Half Bad, and all the docs you see here are accessible within the Bauble composer. It should just pop up automatically as you type stuff, but you can press `ctrl-space` to trigger it manually.

When you first visited Bauble, you were greeted by an interactive tutorial. You can restore it by deleting your script (just clear out the text field) and then refreshing the page. Make sure to save a backup of your script if you've done something interesting!

One day this page might have links to video tutorials. But... there are no video tutorials yet. So.

All of the examples on this page are interactive. To prevent annoying scrolljacking, you have to click on a canvas to focus it before you can zoom.

# The language {#language}

Bauble is implemented in the [Janet](https://janet-lang.org/) programming language, and the programs you write in Bauble are just Janet programs that get evaluated with a particular environment already in scope.

If you'd like to learn more about Janet, I wrote a free book called [*Janet for Mortals*](https://janet.guide/) that gives an introduction to the language.

# Notation {#notation}

Before Bauble executes your script, it performs three purely syntactic transformations on it. You can think of these like macros that are implicitly wrapped around every top-level expression, because that's exactly what they are.

## Dot notation {#dot-notation}

Dot notation gives you an easy way to access fields of vectors and structs. Symbols with dots inside them, like `p.x`, will be rewritten to `(. p x)`, where `.` is an ordinary Janet macro defined in the Bauble standard environment.

Dot notation only applies to Janet *symbols*, so if you want to access a field of an expression, you have to invoke it using traditional prefix notation:

```
# this doesn't work!
(camera/perpsective [10 1 0] [0 0 0]).dir

# you have to do this instead:
(. (camera/perpsective [10 1 0] [0 0 0]) dir)
```

Just like in GLSL, dot notation supports vector swizzling:

```example
(def radius [30 0])
(rect 100 :r radius.xyyx)
```

## Pipe notation {#pipe-notation}

Pipe notation is a way to write function applications in a postfix order, sort of like method-chaining in other languages.

```example
(circle 50 | move [100 0])

# exactly the same as:
# (move (circle 50) [100 0])
```

This is purely syntactic, like the `->` or `->>` macros in the Janet standard library.

By default the argument on the left of the pipe is inserted just after the first argument. But you can specify a different location for it using `_`:

```example
([50 0] | move (circle 50) _)
```

You can insert the left-hand side at any position you want:

```example
(move | _ (circle 50) [50 0])
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
(circle (10 + 20))
# is the same as:
# (circle (+ 10 20))
```

There is no order of operations or precedence in Bauble's infix notation. Operations always happen from left to right:

```example
(circle (5 + 5 * 10))
# is the same as:
# (circle (* (+ 5 5) 10))
```

You can still use prefix notation in Bauble; `(+ 10 20)` will not be rewritten to anything. `+` `-` `*` `/` are only special when they appear in the middle of a form like that.

If there are multiple forms to the left of an infix operator, they will be implicitly wrapped in parentheses, just like pipe notation:

```example
(circle (sin+ t * 50 + 50))

# is the same as:
# (circle (+ (* (sin+ t) 50) 50))
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

# API Reference {#reference}
