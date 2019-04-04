## What is this?

r5js is an interpreter for the [Scheme](https://en.wikipedia.org/wiki/Scheme_(programming_language))
programming language that runs in the browser. It aims to support all of the
[R5RS](https://www.schemers.org/Documents/Standards/R5RS/HTML/) specification, including advanced
features (hygienic macros, first-class continuations, proper tail recursion) that tend to be
overlooked in hobbyist implementations.

I wrote r5js in 2011–2012. I give it a fresh coat of paint occasionally, adopting a new technology
or improving its software engineering process. As time goes on, the technical challenges of building
an interpreter are less interesting to me than the practicalities of evolving a codebase over many
years. Over its lifetime, the project has been implemented in three different languages (plain
JavaScript, Closure-style JavaScript, now TypeScript) and used as many build systems (Make, Maven,
now Bazel).

## What this is not

It is rare for technical projects to draw attention to their flaws. Being able to read between
the lines to understand the risks of using technology, when no one is willing to come out and tell
you, is an important skill. I wish this weren't the case; documentation should be expository
writing, not marketing.

In this spirit, r5js has many significant problems.

- It has no practical use. Scheme is fundamentally no more powerful than other general-purpose
  programming languages. Choosing to program in Scheme does not magically make the work of software
  engineering easier. It is likely to make it harder; the language and its ecosystem lack many
  of the affordances of modern software development.
- It has no community. It's just me. The project will only evolve in directions that I want it to.
