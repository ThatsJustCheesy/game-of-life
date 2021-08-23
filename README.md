# (Yet Another) [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life)

## Written in Fennel for the web

An interactive web-based implementation of [Conway's Game of Life](https://en.wikipedia.org/wiki/Conway's_Game_of_Life), available [here](https://igregory.ca/game-of-life/).

It's written in [Fennel](https://fennel-lang.org/), a Lisp that compiles to Lua code, which is then run in a browser by [Fengari](https://fengari.io/), a Lua interpreter written in JavaScript.

### Why?

- Excuse to learn a Lisp
  - Functional languages such as Lisp derivatives model these sorts of simulations nicely
- Part of it is groundwork for something else I want to build
  - The actual project is a massive undertaking
  - I can get something else that looks neat in the meantime with minimal extra work

### Thoughts on Fennel

Fennel itself is a very nice Lisp veneer over top of Lua. It seamlessly integrates with existing Lua code, including well-established libraries. My main gripes with it come directly from Lua's semantics:

- Having to explicitly use iterators (`pairs`/`ipairs`) to iterate over things can be annoying
- The ability to invoke undefined behaviour by poking at a list wrong is frustrating
- Minimal built-in higher-order functions

The latter point could be seen as a feature, as it allows you to choose your own preferred higher-order function library. There _are_ built-in `map` and `reduce`/`fold` variants, but they require you use the long-winded iterator (`pairs`/`ipairs`) pattern.

I haven't explored the macro system at all yet.
