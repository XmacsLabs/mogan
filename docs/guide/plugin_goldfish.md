# Goldfish Scheme
[Goldfish Scheme](https://github.com/LiiiLabs/goldfish) is the community edition of Liii Scheme, and Mogan STEM Suite is the community edition of Liii STEM Suite.

Goldfish Scheme is built-in as a plugin in Mogan STEM Suite, and it does not provide REPL impl by its own, instead, Mogan STEM Suite provides the structured REPL for Goldfish Scheme.

Beginners of the Scheme language can learn the Scheme language using Goldfish Scheme (R7RS) in Mogan.

## Structured REPL
The significant difference between a structured REPL and a common REPL is that both the input and the output can be structured. For a programming language's REPL, generally, the input is code in plain text format; in Racket, it is supported to include images as part of the code. In the Guix Scheme session, the input is still plain text Scheme code, while the output is structured documentation. This document demonstrates the structured output effect of the Goldfish Scheme session with the following example:

### Special Rules for Rendering
For Scheme snippets starting with the markup `document`、`math`、`equation*`、`align`、`with`、`graphics`:

![](../images/goldfish_rendering.png)

### Side Effect and Eval Result
Eval result is rendered in green background. Side effect is in normal white background.

![](../images/goldfish_side_effect.png)

## Installation
No installation is required, as both Goldfish Scheme and the Goldfish Scheme plugin are built-in within Mogan.

## Built-in Documentation See `Help -> Plugins -> Goldfish Scheme`
