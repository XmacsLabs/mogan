# Liii STEM Keyboard Shortcuts

Welcome to the keyboard shortcuts guide for Mogan. Here we list the available shortcut combinations to help you use the software more efficiently.

The documentation comes from the [Liii STEM official website](https://liiistem.com/docs/guide-keyboard-shortcuts.html). The shortcut list on the official website is continuously updated, and Mogan usually synchronizes the content in a timely manner, but there may be some delay. Please refer to the official documentation for the most up-to-date information. You can also get the latest PDF version of the shortcut manual on the official website.

## Environment Shortcuts

::: tip Hint
These shortcuts are used to control the overall environment and formatting settings of the document. This includes basic typesetting functions like spaces, indentation, alignment, chapter titles, lists, etc. Using these shortcuts correctly can help you edit documents more efficiently.
:::

| Windows                                           | GNU/Linux                                         | Mac                                               | $\LaTeX$ Equivalent                                      | Description                             |
| ------------------------------------------------- | ------------------------------------------------- | ------------------------------------------------- | -------------------------------------------------------- | --------------------------------------- |
| <kbd>space</kbd> + <kbd>tab</kbd>                 | <kbd>space</kbd> + <kbd>tab</kbd>                 | <kbd>space</kbd> + <kbd>tab</kbd>                 | Non-breaking space ($\backslash\texttt{nbsp}$ or $\sim$) | Insert non-breaking space               |
| <kbd>ctrl</kbd> + <kbd>t</kbd>                    | /                                                 | /                                                 | $\backslash\texttt{indent}$                              | Indent first line of paragraph          |
| <kbd>ctrl</kbd> + <kbd>l</kbd>                    | /                                                 | /                                                 | $\backslash\texttt{raggedleft}$                          | Right-align text                        |
| <kbd>ctrl</kbd> + <kbd>e</kbd>                    | /                                                 | /                                                 | $\backslash\texttt{centering}$                           | Center-align text                       |
| <kbd>ctrl</kbd> + <kbd>r</kbd>                    | /                                                 | /                                                 | $\backslash\texttt{raggedright}$                         | Left-align text                         |
| <kbd>alt</kbd> + <kbd>1</kbd>                     | <kbd>alt</kbd> + <kbd>1</kbd>                     | <kbd>option</kbd> + <kbd>1</kbd>                  | $\backslash\texttt{section}$                             | Create level 1 heading                  |
| <kbd>alt</kbd> + <kbd>2</kbd>                     | <kbd>alt</kbd> + <kbd>2</kbd>                     | <kbd>option</kbd> + <kbd>2</kbd>                  | $\backslash\texttt{subsection}$                          | Create level 2 heading                  |
| <kbd>alt</kbd> + <kbd>3</kbd>                     | <kbd>alt</kbd> + <kbd>3</kbd>                     | <kbd>option</kbd> + <kbd>3</kbd>                  | $\backslash\texttt{subsubsection}$                       | Create level 3 heading                  |
| <kbd>alt</kbd> + <kbd>4</kbd>                     | <kbd>alt</kbd> + <kbd>4</kbd>                     | <kbd>option</kbd> + <kbd>4</kbd>                  | $\backslash\texttt{paragraph}$                           | Create paragraph heading                |
| <kbd>+</kbd> + <kbd>tab</kbd>                     | <kbd>+</kbd> + <kbd>tab</kbd>                     | <kbd>+</kbd> + <kbd>tab</kbd>                     | $\backslash\texttt{itemize}$                             | Create unordered list item              |
| <kbd>1</kbd> + <kbd>.</kbd> + <kbd>tab</kbd>      | <kbd>1</kbd> + <kbd>.</kbd> + <kbd>tab</kbd>      | <kbd>1</kbd> + <kbd>.</kbd> + <kbd>tab</kbd>      | $\backslash\texttt{enumerate}$                           | Create ordered list item                |
| <kbd>$</kbd>                                      | <kbd>$</kbd>                                      | <kbd>$</kbd>                                      | inline math mode                                         | Insert inline math formula              |
| <kbd>alt</kbd> + <kbd>$</kbd>                     | <kbd>alt</kbd> + <kbd>$</kbd>                     | <kbd>option</kbd> + <kbd>$</kbd>                  | single-line math mode                                    | Insert single-line display math formula |
| <kbd>alt</kbd> + <kbd>7</kbd>                     | <kbd>alt</kbd> + <kbd>7</kbd>                     | <kbd>option</kbd> + <kbd>7</kbd>                  | multi-line math mode                                     | Insert multi-line math environment      |
| <kbd>ctrl</kbd> + <kbd>#</kbd>                    | <kbd>ctrl</kbd> + <kbd>#</kbd>                    | <kbd>ctrl</kbd> + <kbd>#</kbd>                    | add equation number                                      | Add equation number to math formula     |
| <kbd>alt</kbd> + <kbd>arrow</kbd>                 | <kbd>alt</kbd> + <kbd>arrow</kbd>                 | <kbd>option</kbd> + <kbd>arrow</kbd>              | add new row/column in matrix/table                       | Add new row or column in matrix/table   |
| <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>f</kbd> | <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>f</kbd> | <kbd>ctrl</kbd> + <kbd>shift</kbd> + <kbd>f</kbd> | add footnote                                             | Add footnote                            |
| <kbd>ctrl</kbd> + <kbd>n</kbd>                    | <kbd>ctrl</kbd> + <kbd>n</kbd>                    | <kbd>ctrl</kbd> + <kbd>n</kbd>                    | add new script                                           | Create new document                     |
| <kbd>ctrl</kbd> + <kbd>p</kbd>                    | <kbd>ctrl</kbd> + <kbd>p</kbd>                    | <kbd>ctrl</kbd> + <kbd>p</kbd>                    | export to PDF                                            | Export document to PDF format           |

::: warning Notes
1. On Mac, use the Option key instead of the Alt key used on Windows/Linux.
2. Some shortcuts may conflict with system shortcuts and need to be changed in system settings.
3. Math formula related shortcuts need to be used within the corresponding math environment.
4. Table and matrix operation shortcuts only work in the corresponding environments.
5. It is recommended to memorize frequently used shortcuts to improve editing efficiency.
6. Shortcuts for paragraph first line indent, right align, center align, left align currently only support Windows.
:::

## Common Constructs

:::tip Hint
This document lists common shortcuts for entering mathematical formulas in the editor. Press all keys sequentially. Before using these shortcuts, press <kbd>$</kbd> to enter math input mode.
:::

| Windows/GNU/Linux                                                                                          | Mac                                                                                                           | $\LaTeX$ Equivalent          | Description                    |
| ---------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------------------- | ---------------------------- | ------------------------------ |
| <kbd>x</kbd> + <kbd>^</kbd> + <kbd>2</kbd>                                                                 | <kbd>x</kbd> + <kbd>^</kbd> + <kbd>2</kbd>                                                                    | $x^2$ (x^2)                  | Superscript                    |
| <kbd>x</kbd> + <kbd>_</kbd> + <kbd>{i,j}</kbd>                                                             | <kbd>x</kbd> + <kbd>_</kbd> + <kbd>{i,j}</kbd>                                                                | $x_{i,j}$ (x_{i.j})          | Subscript                      |
| <kbd>alt</kbd> + <kbd>s</kbd> + <kbd>2</kbd>                                                               | <kbd>option</kbd> + <kbd>s</kbd> + <kbd>2</kbd>                                                               | $\sqrt{2}$ (\sqrt{2})        | Square root                    |
| <kbd>alt</kbd> + <kbd>s</kbd> + <kbd>Tab</kbd> + <kbd>3</kbd> + <kbd>←</kbd> + <kbd>←</kbd> + <kbd>n</kbd> | <kbd>option</kbd> + <kbd>s</kbd> + <kbd>Tab</kbd> + <kbd>3</kbd> + <kbd>←</kbd> + <kbd>←</kbd> + <kbd>n</kbd> | $\sqrt[n]{3}$ (\sqrt[n]{3})  | nth root                       |
| <kbd>alt</kbd> + <kbd>f</kbd>                                                                              | <kbd>option</kbd> + <kbd>f</kbd>                                                                              | $\frac{2}{3}$ (\frac{2}{3})  | Fraction                       |
| <kbd>alt</kbd> + <kbd>t</kbd>                                                                              | /                                                                                                             | $\backslash\texttt{tabular}$ | Centered table without borders |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd>                                                             | /                                                                                                             | $\backslash\texttt{matrix}$  | Matrix                         |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                            | /                                                                                                             |                              | Determinant                    |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                           | /                                                                                                             |                              | Cases                          |
| <kbd>alt</kbd> + <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>          | /                                                                                                             |                              | Stack                          |

:::warning Notes
1. When using these shortcuts, ensure the editor is in math input mode.
2. If the formula is not displayed correctly, check if you have correctly entered math input mode.
3. Shortcuts for constructs like tables and matrices are too complex on Mac and are therefore not listed.
:::

## Font Styles

:::tip Hint
When entering mathematical expressions, you can use these shortcuts to quickly change the font style.
:::

| Windows/GNU/Linux                             | Mac                                           | $\LaTeX$ Equivalent              | Description     |
| --------------------------------------------- | --------------------------------------------- | -------------------------------- | --------------- |
| <kbd>ctrl</kbd> + <kbd>u</kbd> + <kbd>A</kbd> | <kbd>ctrl</kbd> + <kbd>u</kbd> + <kbd>A</kbd> | $\underline{A}$  (\underline{A}) | Underlined      |
| <kbd>ctrl</kbd> + <kbd>i</kbd> + <kbd>A</kbd> | <kbd>ctrl</kbd> + <kbd>i</kbd> + <kbd>A</kbd> | $\mathit{A}$ (\mathit{A})        | Italic          |
| <kbd>ctrl</kbd> + <kbd>b</kbd> + <kbd>A</kbd> | <kbd>ctrl</kbd> + <kbd>b</kbd> + <kbd>A</kbd> | $\mathbf{A}$ (\mathbf{A})        | Bold            |
| <kbd>F7</kbd> + <kbd>A</kbd>                  | <kbd>F7</kbd> + <kbd>A</kbd>                  | $\mathcal{A}$ (\mathcal{A})      | Calligraphic    |
| <kbd>F8</kbd> + <kbd>A</kbd>                  | <kbd>F8</kbd> + <kbd>A</kbd>                  | $\mathfrak{A}$ (\mathfrak{A})    | Fraktur         |
| <kbd>A</kbd> + <kbd>A</kbd>                   | <kbd>A</kbd> + <kbd>A</kbd>                   | $\mathbb{A}$ (\mathbb{A})        | Blackboard bold |

:::warning Notes
Some font styles (like calligraphic, fraktur, blackboard bold, etc.) only apply to Latin letters.
:::

## Greek Letters

:::tip Hint
- All Greek letter shortcuts are letter + Tab combination.
- Use uppercase letter + Tab for uppercase Greek letters.
- Press Tab after typing the letter in the math editor.
:::

| Windows/GNU/Linux/Mac                                                                          | $\LaTeX$ Equivalent                          | Name    |
| ---------------------------------------------------------------------------------------------- | -------------------------------------------- | ------- |
| <kbd>a</kbd> + <kbd>tab</kbd>                                                                  | $\alpha$ (\alpha)                            | Alpha   |
| <kbd>b</kbd> + <kbd>tab</kbd>                                                                  | $\beta$ (\beta)                              | Beta    |
| <kbd>g</kbd> + <kbd>tab</kbd>, <kbd>G</kbd> + <kbd>tab</kbd>                                   | $\gamma$ (\gamma), $\Gamma$ (\Gamma)         | Gamma   |
| <kbd>d</kbd> + <kbd>tab</kbd>, <kbd>D</kbd> + <kbd>tab</kbd>                                   | $\delta$ (\delta), $\Delta$ (\Delta)         | Delta   |
| <kbd>e</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                | $\epsilon$ (\epsilon)                        | Epsilon |
| <kbd>e</kbd> + <kbd>tab</kbd>                                                                  | $\varepsilon$ (\varepsilon)                  | Epsilon |
| <kbd>z</kbd> + <kbd>tab</kbd>                                                                  | $\zeta$ (\zeta)                              | Zeta    |
| <kbd>h</kbd> + <kbd>tab</kbd>                                                                  | $\eta$ (\eta)                                | Eta     |
| <kbd>j</kbd> + <kbd>tab</kbd>, <kbd>J</kbd> + <kbd>tab</kbd>                                   | $\theta$ (\theta)                            | Theta   |
| <kbd>j</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                | $\vartheta$ (\vartheta)                      | Theta   |
| <kbd>i</kbd> + <kbd>tab</kbd>                                                                  | $\iota$ (\iota)                              | Iota    |
| <kbd>k</kbd> + <kbd>tab</kbd>, <kbd>K</kbd> + <kbd>tab</kbd>                                   | $\kappa$ (\kappa)                            | Kappa   |
| <kbd>l</kbd> + <kbd>tab</kbd>, <kbd>L</kbd> + <kbd>tab</kbd>                                   | $\lambda$ (\lambda), $\Lambda$ (\Lambda)     | Lambda  |
| <kbd>m</kbd> + <kbd>tab</kbd>                                                                  | $\mu$ (\mu)                                  | Mu      |
| <kbd>n</kbd> + <kbd>tab</kbd>                                                                  | $\nu$ (\nu)                                  | Nu      |
| <kbd>x</kbd> + <kbd>tab</kbd>, <kbd>X</kbd> + <kbd>tab</kbd>                                   | $\xi$ (\xi), $\Xi$ (\Xi)                     | Xi      |
| <kbd>p</kbd> + <kbd>tab</kbd>, <kbd>P</kbd> + <kbd>tab</kbd>                                   | $\pi$ (\pi), $\Pi$ (\Pi)                     | Pi      |
| <kbd>p</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                | $\varpi$ (\varpi)                            | Pi      |
| <kbd>o</kbd> + <kbd>tab</kbd>, <kbd>O</kbd> + <kbd>tab</kbd>                                   | $\omicron$ (\omicron)                        | Omicron |
| <kbd>r</kbd> + <kbd>tab</kbd>                                                                  | $\rho$ (\rho)                                | Rho     |
| <kbd>r</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                 | $\varrho$ (\varrho)                          | Rho     |
| <kbd>s</kbd> + <kbd>tab</kbd>, <kbd>S</kbd> + <kbd>tab</kbd> / <kbd>C</kbd> + <kbd>tab</kbd>   | $\sigma$ (\sigma), $\Sigma$ (\Sigma)         | Sigma   |
| <kbd>s</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> / <kbd>c</kbd> + <kbd>tab</kbd>                 | $\varsigma$ (\varsigma)                      | Sigma   |
| <kbd>t</kbd> + <kbd>tab</kbd>                                                                  | $\tau$ (\tau)                                | Tau     |
| <kbd>u</kbd> + <kbd>tab</kbd>, <kbd>U</kbd> + <kbd>tab</kbd>                                   | $\upsilon$ (\upsilon), $\Upsilon$ (\Upsilon) | Upsilon |
| <kbd>f</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>, <kbd>F</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\phi$ (\phi), $\Phi$ (\Phi)                 | Phi     |
| <kbd>f</kbd> + <kbd>tab</kbd>                                                                  | $\varphi$ (\varphi)                          | Phi     |
| <kbd>q</kbd> + <kbd>tab</kbd>                                                                  | $\chi$ (\chi)                                | Chi     |
| <kbd>y</kbd> + <kbd>tab</kbd>, <kbd>Y</kbd> + <kbd>tab</kbd>                                   | $\psi$ (\psi), $\Psi$ (\Psi)                 | Psi     |
| <kbd>w</kbd> + <kbd>tab</kbd>, <kbd>W</kbd> + <kbd>tab</kbd>                                   | $\omega$ (\omega), $\Omega$ (\Omega)         | Omega   |

:::warning Notes
- Some Greek letters have multiple variant forms (e.g., ε and ϵ), please distinguish them.
- Case sensitive, please ensure you enter the correct case.
:::

## Set Theory and Logic

:::tip Hint
These shortcuts can help you quickly input mathematical symbols, improving efficiency when writing mathematical formulas.
:::

| Windows/GNU/Linux                                               | Mac                                                             | $\LaTeX$ Equivalent           | Description               |
| --------------------------------------------------------------- | --------------------------------------------------------------- | ----------------------------- | ------------------------- |
| <kbd>%</kbd> + <kbd>tab</kbd>                                   | <kbd>%</kbd> + <kbd>tab</kbd>                                   | $\cup$ (\cup)                 | Union symbol              |
| <kbd>&</kbd> + <kbd>tab</kbd>                                   | <kbd>&</kbd> + <kbd>tab</kbd>                                   | $\cap$ (\cap)                 | Intersection symbol       |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | $\subset$ (\subset)           | Proper subset             |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd>   | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd>   | $\subseteq$ (\subseteq)       | Subset                    |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | $\supset$ (\supset)           | Proper superset           |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd>   | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd>   | $\supseteq$ (\supseteq)       | Superset                  |
| <kbd><</kbd> + <kbd>tab</kbd>                                   | <kbd><</kbd> + <kbd>tab</kbd>                                   | $\in$ (\in)                   | Element of                |
| <kbd>></kbd> + <kbd>tab</kbd>                                   | <kbd>></kbd> + <kbd>tab</kbd>                                   | $\ni$ (\ni)                   | Contains as element       |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>/</kbd>                    | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>/</kbd>                    | $\notin$ (\notin)             | Not element of            |
| <kbd>R</kbd> + <kbd>R</kbd>                                     | <kbd>R</kbd> + <kbd>R</kbd>                                     | $\mathbb{R}$ (\mathbb{R})     | Set of real numbers       |
| <kbd>Z</kbd> + <kbd>Z</kbd>                                     | <kbd>Z</kbd> + <kbd>Z</kbd>                                     | $\mathbb{Z}$ (\mathbb{Z})     | Set of integers           |
| <kbd>Q</kbd> + <kbd>Q</kbd>                                     | <kbd>Q</kbd> + <kbd>Q</kbd>                                     | $\mathbb{Q}$ (\mathbb{Q})     | Set of rational numbers   |
| <kbd>N</kbd> + <kbd>N</kbd>                                     | <kbd>N</kbd> + <kbd>N</kbd>                                     | $\mathbb{N}$ (\mathbb{N})     | Set of natural numbers    |
| <kbd>C</kbd> + <kbd>C</kbd>                                     | <kbd>C</kbd> + <kbd>C</kbd>                                     | $\mathbb{C}$ (\mathbb{C})     | Set of complex numbers    |
| <kbd>@</kbd> + <kbd>/</kbd>                                     | <kbd>@</kbd> + <kbd>/</kbd>                                     | $\varnothing$ (\varnothing)   | Empty set                 |
| <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\aleph$ (\aleph)             | Aleph number              |
| <kbd>=</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | <kbd>=</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | $\equiv$ (\equiv)             | Defined as / Identical to |
| <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | <kbd>A</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | $\forall$ (\forall)           | Universal quantifier      |
| <kbd>E</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | <kbd>E</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | $\exists$ (\exists)           | Existential quantifier    |
| <kbd>!</kbd> + <kbd>tab</kbd>                                   | <kbd>!</kbd> + <kbd>tab</kbd>                                   | $\neg$ (\neg)                 | Logical NOT               |
| <kbd>%</kbd>                                                    | <kbd>%</kbd>                                                    | $\vee$ (\vee)                 | Disjunction (OR)          |
| <kbd>&</kbd>                                                    | <kbd>&</kbd>                                                    | $\wedge$ (\wedge)             | Conjunction (AND)         |
| <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>-</kbd>                   | <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>-</kbd>                   | $\vdash$ (\vdash)             | Turnstile                 |
| <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>=</kbd>                   | <kbd>\|</kbd> + <kbd>tab</kbd> + <kbd>=</kbd>                   | $\models$ (\models)           | Models symbol             |
| <kbd>=</kbd> + <kbd>></kbd>                                     | <kbd>=</kbd> + <kbd>></kbd>                                     | $\Rightarrow$ (\Rightarrow)   | Implies symbol            |
| <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd>                      | <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd>                      | $\nRightarrow$ (\nRightarrow) | Does not imply symbol     |

:::warning Notes
- Ensure no other characters interfere when inputting shortcuts.
- The Tab key needs to be fully pressed before proceeding to the next step.
:::

## Accents and Diacritics

:::tip Hint
In Liii STEM, you can use keyboard shortcuts to quickly input common mathematical accents and diacritics. These symbols are often used to denote derivatives, vectors, etc.
:::

| Windows/GNU/Linux                            | Mac                                             | $\LaTeX$ Equivalent   | Description                |
| -------------------------------------------- | ----------------------------------------------- | --------------------- | -------------------------- |
| <kbd>alt</kbd> + <kbd>˙</kbd>                | <kbd>option</kbd> + <kbd>˙</kbd>                | $\dot{}$ (\dot{})     | First derivative           |
| <kbd>alt</kbd> + <kbd>˙</kbd> + <kbd>˙</kbd> | <kbd>option</kbd> + <kbd>˙</kbd> + <kbd>˙</kbd> | $\ddot{}$ (\ddot{})   | Second derivative          |
| <kbd>alt</kbd> + <kbd>ˆ</kbd>                | <kbd>option</kbd> + <kbd>ˆ</kbd>                | $\hat{}$ (\hat{})     | Hat accent (unit vector)   |
| <kbd>alt</kbd> + <kbd>˜</kbd>                | <kbd>option</kbd> + <kbd>˜</kbd>                | $\tilde{}$ (\tilde{}) | Tilde accent (equivalence) |
| <kbd>alt</kbd> + <kbd>-</kbd>                | <kbd>option</kbd> + <kbd>-</kbd>                | $\bar{}$ (\bar{})     | Bar accent (mean value)    |

:::warning Notes
1. Accents and diacritics need to be added after the character is entered.
2. Some accents may conflict with system shortcuts; ensure your system settings allow these shortcuts.
3. Mac users, ensure the special character input function for the Option key is enabled.
:::

## Dots

:::tip Hint
After inputting dots, you can press the <kbd>Tab</kbd> key to switch between different types of dots.
:::

| Windows/GNU/Linux                                                                               | Mac                                                                                             | $\LaTeX$ Equivalent | Description              |
| ----------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------- | ------------------- | ------------------------ |
| <kbd>.</kbd> + <kbd>.</kbd>                                                                     | <kbd>.</kbd> + <kbd>.</kbd>                                                                     | $\ldots$ (\ldots)   | Baseline ellipsis (text) |
| <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd>                                                    | <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd>                                                    | $\cdots$ (\cdots)   | Centered ellipsis (math) |
| <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | $\vdots$ (\vdots)   | Vertical ellipsis        |
| <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>.</kbd> + <kbd>.</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\ddots$ (\ddots)   | Diagonal ellipsis        |

:::warning Notes
Please distinguish the usage scenarios of centered dots ($\cdots$) and baseline dots ($\ldots$).
:::

## Variable Size Operators

::: tip Hint
These shortcuts can quickly input common mathematical operation symbols.
:::

| Windows/GNU/Linux                                          | Mac                                                        | $\LaTeX$ Equivalent | Description         |
| ---------------------------------------------------------- | ---------------------------------------------------------- | ------------------- | ------------------- |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd>                | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd>                | $\int$ (\int)       | Integral sign       |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd> + <kbd>I</kbd> | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>I</kbd> + <kbd>I</kbd> | $\iint$ (\iint)     | Double integral     |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>O</kbd>                | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>O</kbd>                | $\oint$ (\oint)     | Contour integral    |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>U</kbd>                | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>U</kbd>                | $\bigcup$ \bigcup   | Union symbol        |
| <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>N</kbd>                | <kbd>⇧</kbd> + <kbd>F5</kbd> + <kbd>N</kbd>                | $\bigcap$ (\bigcap) | Intersection symbol |

::: warning Notes
Please ensure the function key (F5) on your keyboard works correctly. Some laptops may require pressing the Fn key additionally.
:::

## Arrows

::: tip Hint
In Liii STEM, arrow symbols can be quickly entered using simple keyboard combinations. Remembering these shortcuts can improve the efficiency of entering mathematical formulas.
:::

| Windows/GNU/Linux                                             | Mac                                                           | $\LaTeX$ Equivalent                 | Description                |
| ------------------------------------------------------------- | ------------------------------------------------------------- | ----------------------------------- | -------------------------- |
| <kbd>-</kbd> + <kbd>></kbd>                                   | <kbd>-</kbd> + <kbd>></kbd>                                   | $\rightarrow$ (\rightarrow)         | Right arrow                |
| <kbd>-</kbd> + <kbd>></kbd> + <kbd>/</kbd>                    | <kbd>-</kbd> + <kbd>></kbd> + <kbd>/</kbd>                    | $\nrightarrow$ (\nrightarrow)       | Negated right arrow        |
| <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd>                    | <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd>                    | $\longrightarrow$ (\longrightarrow) | Long right arrow           |
| <kbd>=</kbd> + <kbd>></kbd>                                   | <kbd>=</kbd> + <kbd>></kbd>                                   | $\Rightarrow$ (\Rightarrow)         | Double right arrow         |
| <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd>                    | <kbd>=</kbd> + <kbd>></kbd> + <kbd>/</kbd>                    | $\nRightarrow$ (\nRightarrow)       | Negated double right arrow |
| <kbd>=</kbd> + <kbd>=</kbd> + <kbd>></kbd>                    | <kbd>=</kbd> + <kbd>=</kbd> + <kbd>></kbd>                    | $\Longrightarrow$ (\Longrightarrow) | Long double right arrow    |
| <kbd>~</kbd> + <kbd>></kbd>                                   | <kbd>~</kbd> + <kbd>></kbd>                                   | $\leadsto$ (\leadsto)               | Squiggly arrow             |
| <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>></kbd>                   | <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>></kbd>                   | $\mapsto$ (\mapsto)                 | Maps to arrow              |
| <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd>    | <kbd>\|</kbd> + <kbd>-</kbd> + <kbd>-</kbd> + <kbd>></kbd>    | $\longmapsto$ (\longmapsto)         | Long maps to arrow         |
| <kbd><</kbd> + <kbd>-</kbd>                                   | <kbd><</kbd> + <kbd>-</kbd>                                   | $\leftarrow$ (\leftarrow)           | Left arrow                 |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd>                    | <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd>                    | $\leftrightarrow$ (\leftrightarrow) | Left-right arrow           |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd>                  | <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd>                  | $\uparrow$ (\uparrow)               | Up arrow                   |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\downarrow$ (\downarrow)           | Down arrow                 |
| <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd> + <kbd>tab</kbd>   | <kbd><</kbd> + <kbd>-</kbd> + <kbd>></kbd> + <kbd>tab</kbd>   | $\updownarrow$ (\updownarrow)       | Up-down arrow              |

::: warning Notes
When inputting arrows, the combination keys need to be pressed sequentially.
:::

## Brackets and Delimiters

::: tip Hint
Liii STEM provides shortcut input methods for various brackets and delimiters. Common mathematical symbols can be quickly entered using key combinations, improving formula editing efficiency.
:::

| Windows/GNU/Linux                                                                                                  | Mac                                                                                                                | $\LaTeX$ Equivalent                         | Description              |
| ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------ | ------------------------------------------- | ------------------------ |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\langle \rangle$ (\langle \rangle)         | Angle brackets (vectors) |
| <kbd>\|</kbd> + <kbd>.</kbd>                                                                                       | <kbd>\|</kbd> + <kbd>.</kbd>                                                                                       | $\lfloor \rfloor$ (\lfloor \rfloor)         | Floor brackets           |
| <kbd>\|</kbd> + <kbd>'</kbd>                                                                                       | <kbd>\|</kbd> + <kbd>'</kbd>                                                                                       | $\lceil \rceil$ (\lceil \rceil)             | Ceiling brackets         |
| <kbd>\|</kbd> + <kbd>\|</kbd>                                                                                      | <kbd>\|</kbd> + <kbd>\|</kbd>                                                                                      | $\parallel \parallel$ (\parallel \parallel) | Norm symbol              |

::: warning Notes
1. Ensure brackets are paired correctly when inputting, with equal numbers of left and right brackets.
2. Some delimiters may have different meanings in different contexts.
3. It is recommended to first familiarize yourself with common shortcut combinations and gradually master more combinations.
:::

## Other Symbols

::: tip Hint
- All shortcuts need to be used within the math environment.
- Typing the shortcut immediately triggers symbol input.
- The number of Tab key presses affects the final symbol output.
:::

| Windows/GNU/Linux                                                                                 | Mac                                                                                               | $\LaTeX$ Equivalent                 | Description              |
| ------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------------- | ----------------------------------- | ------------------------ |
| <kbd><</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>                                                      | <kbd><</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>                                                      | $\leq$ (\leq)                       | Less than or equal to    |
| <kbd>></kbd> + <kbd>=</kbd> + <kbd>tab</kbd>                                                      | <kbd>></kbd> + <kbd>=</kbd> + <kbd>tab</kbd>                                                      | $\geq$ (\geq)                       | Greater than or equal to |
| <kbd>=</kbd> + <kbd>\\</kbd>                                                                      | <kbd>=</kbd> + <kbd>\\</kbd>                                                                      | $\neq$ (\neq)                       | Not equal to             |
| <kbd><</kbd> + <kbd><</kbd>                                                                       | <kbd><</kbd> + <kbd><</kbd>                                                                       | $\ll$ (\ll)                         | Much less than           |
| <kbd>></kbd> + <kbd>></kbd>                                                                       | <kbd>></kbd> + <kbd>></kbd>                                                                       | $\gg$ (\gg)                         | Much greater than        |
| <kbd>˜</kbd> + <kbd>˜</kbd>                                                                       | <kbd>˜</kbd> + <kbd>˜</kbd>                                                                       | $\approx$ (\approx)                 | Approximately equal to   |
| <kbd>=</kbd> + <kbd>tab</kbd>                                                                     | <kbd>=</kbd> + <kbd>tab</kbd>                                                                     | $\asymp$ (\asymp)                   | Asymptotically equal to  |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\prec$ (\prec)                     | Precedes                 |
| <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>   | <kbd><</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>   | $\preceq$ (\preceq)                 | Precedes or equal to     |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\succ$ (\succ)                     | Succeeds                 |
| <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>   | <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>   | $\succeq$ (\succeq)                 | Succeeds or equal to     |
| <kbd>@</kbd> + <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                     | <kbd>@</kbd> + <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                     | $\propto$ (\propto)                 | Proportional to          |
| <kbd>.</kbd> + <kbd>=</kbd>                                                                       | <kbd>.</kbd> + <kbd>=</kbd>                                                                       | $\doteq$ (\doteq)                   | Dot equals               |
| <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                  | <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>@</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                    | $\angle$ (\angle)                   | Angle                    |
| <kbd>l</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>l</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\ell$ (\ell)                       | Script lowercase L       |
| <kbd>⇧</kbd> + <kbd>f5</kbd> + <kbd>b</kbd>                                                       | <kbd>⇧</kbd> + <kbd>f5</kbd> + <kbd>b</kbd>                                                       | $\parallel$ (\parallel)             | Parallel to              |
| <kbd>~</kbd> + <kbd>=</kbd>                                                                       | <kbd>~</kbd> + <kbd>=</kbd>                                                                       | $\cong$ (\cong)                     | Congruent to             |
| <kbd>~</kbd> + <kbd>=</kbd> + <kbd>\/</kbd>                                                       | <kbd>~</kbd> + <kbd>=</kbd> + <kbd>\/</kbd>                                                       | $\ncong$ (\ncong)                   | Not congruent to         |
| <kbd>~</kbd>                                                                                      | <kbd>~</kbd>                                                                                      | $\sim$ (\sim)                       | Similar to               |
| <kbd>~</kbd> + <kbd>-</kbd>                                                                       | <kbd>~</kbd> + <kbd>-</kbd>                                                                       | $\simeq$ (\simeq)                   | Similar and equal to     |
| <kbd>~</kbd> + <kbd>\/</kbd>                                                                      | <kbd>~</kbd> + <kbd>\/</kbd>                                                                      | $\nsim$ (\nsim)                     | Not similar              |
| <kbd>@</kbd> + <kbd>+</kbd>                                                                       | <kbd>@</kbd> + <kbd>+</kbd>                                                                       | $\oplus$ (\oplus)                   | Circled plus             |
| <kbd>@</kbd> + <kbd>-</kbd>                                                                       | <kbd>@</kbd> + <kbd>-</kbd>                                                                       | $\ominus$ (\ominus)                 | Circled minus            |
| <kbd>@</kbd> + <kbd>.</kbd>                                                                       | <kbd>@</kbd> + <kbd>.</kbd>                                                                       | $\odot$ (\odot)                     | Circled dot              |
| <kbd>@</kbd> + <kbd>*</kbd>                                                                       | <kbd>@</kbd> + <kbd>*</kbd>                                                                       | $\otimes$ (\otimes)                 | Circled times            |
| <kbd>@</kbd> + <kbd>\/</kbd>                                                                      | <kbd>@</kbd> + <kbd>\/</kbd>                                                                      | $\oslash$ (\oslash)                 | Circled slash            |
| <kbd>\/</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                   | <kbd>\/</kbd> + <kbd>-</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                   | $\upharpoonright$ (\upharpoonright) | Restriction              |
| <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\cdot$ (\cdot)                     | Centered dot             |
| <kbd>+</kbd> + <kbd>-</kbd>                                                                       | <kbd>+</kbd> + <kbd>-</kbd>                                                                       | $\pm$ (\pm)                         | Plus-minus sign          |
| <kbd>-</kbd> + <kbd>+</kbd>                                                                       | <kbd>-</kbd> + <kbd>+</kbd>                                                                       | $\mp$ (\mp)                         | Minus-plus sign          |
| <kbd>*</kbd> + <kbd>tab</kbd>                                                                     | <kbd>*</kbd> + <kbd>tab</kbd>                                                                     | $\times$ (\times)                   | Times sign               |
| <kbd>\/</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                   | <kbd>\/</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                   | $\div$ (\div)                       | Division sign            |
| <kbd>*</kbd> + <kbd>tab</kbd>                                                                     | <kbd>*</kbd> + <kbd>tab</kbd>                                                                     | $\ast$ (\ast)                       | Asterisk operator        |
| <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\partial$ (\partial)               | Partial derivative       |
| <kbd>v</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>v</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\nabla$ (\nabla)                   | Nabla (gradient)         |
| <kbd>@</kbd>                                                                                      | <kbd>@</kbd>                                                                                      | $\circ$ (\circ)                     | Circle operator          |
| <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | <kbd>*</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> | $\star$ (\star)                     | Star operator            |
| <kbd>i</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>i</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\imath$ (\imath)                   | Dotless lowercase i      |
| <kbd>j</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>j</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\jmath$ (\jmath)                   | Dotless lowercase j      |
| <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\beth$ (\beth)                     | Hebrew letter beth       |
| <kbd>g</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>g</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\gimel$ (\gimel)                   | Hebrew letter gimel      |
| <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>d</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\daleth$ (\daleth)                 | Hebrew letter daleth     |
| <kbd>r</kbd> + <kbd>e</kbd>                                                                       | <kbd>r</kbd> + <kbd>e</kbd>                                                                       | $\Re$ (\Re)                         | Real part                |
| <kbd>w</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>w</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\mho$ (\mho)                       | Mho (inverted Ohm)       |
| <kbd>p</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>p</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\wp$ (\wp)                         | Weierstrass p            |
| <kbd>@</kbd> + <kbd>@</kbd>                                                                       | <kbd>@</kbd> + <kbd>@</kbd>                                                                       | $\infty$ (\infty)                   | Infinity                 |
| <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\top$ (\top)                       | Top symbol               |
| <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | <kbd>t</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                   | $\bot$ (\bot)                       | Bottom symbol            |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>   | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>   | $\clubsuit$ (\clubsuit)             | Club suit                |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd>                                                      | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd>                                                      | $\diamondsuit$ (\diamondsuit)       | Diamond suit             |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                     | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                     | $\heartsuit$ (\heartsuit)           | Heart suit               |
| <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                    | <kbd><</kbd> + <kbd>></kbd> + <kbd>tab</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                    | $\spadesuit$ (\spadesuit)           | Spade suit               |
| <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>b</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\flat$ (\flat)                     | Flat sign                |
| <kbd>#</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>#</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\natural$ (\natural)               | Natural sign             |
| <kbd>#</kbd> + <kbd>tab</kbd>                                                                     | <kbd>#</kbd> + <kbd>tab</kbd>                                                                     | $\sharp$ (\sharp)                   | Sharp sign               |
| <kbd>@</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>                                                      | <kbd>@</kbd> + <kbd>=</kbd> + <kbd>tab</kbd>                                                      | $\triangleq$ (\triangleq)           | Triangle equals          |
| <kbd>+</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | <kbd>+</kbd> + <kbd>tab</kbd> + <kbd>tab</kbd>                                                    | $\dagger$ (\dagger)                 | Dagger                   |

::: warning Note
Under different input methods, you need to switch to English input mode.
:::