<!--toc:start-->
- [Plaintext Text Objects](#plaintext-text-objects)
 - [Words](#words)
 - [Sentences](#sentences)
 - [Paragraphs](#paragraphs)
- [Programming Language Text Objects](#programming-language-text-objects)
 - [Strings](#strings)
 - [Parentheses](#parentheses)
 - [Brackets](#brackets)
 - [Markup Language Tags](#markup-language-tags)
- [Vim Scripts Providing Additional Text Objects](#vim-scripts-providing-additional-text-objects)
 - [CamelCaseMotion](#camelcasemotion)
 - [VimTextObj](#vimtextobj)
 - [Indent Object](#indent-object)
- [Targets.vim](#targetsvim)
 - [Pair Text Objects](#pair-text-objects)
  - [In Pair](#in-pair)
  - [A Pair](#a-pair)
  - [Inside Pair](#inside-pair)
  - [Around Pair](#around-pair)
  - [Next and Last Pair](#next-and-last-pair)
  - [Pair Seek](#pair-seek)
 - [Quote Text Objects](#quote-text-objects)
  - [In Quote](#in-quote)
  - [A Quote](#a-quote)
  - [Inside Quote](#inside-quote)
  - [Around Quote](#around-quote)
  - [Next and Last Quote](#next-and-last-quote)
  - [Quote Seek](#quote-seek)
 - [Separator Text Objects](#separator-text-objects)
  - [In Separator](#in-separator)
  - [A Separator](#a-separator)
  - [Inside Separator](#inside-separator)
  - [Around Separator](#around-separator)
  - [Next and Last Separator](#next-and-last-separator)
  - [Separator Seek](#separator-seek)
 - [Argument Text Objects](#argument-text-objects)
  - [In Argument](#in-argument)
  - [An Argument](#an-argument)
  - [Inside Argument](#inside-argument)
  - [Around Argument](#around-argument)
  - [Next and Last Argument](#next-and-last-argument)
  - [Argument Seek](#argument-seek)
 - [Multi Text Objects](#multi-text-objects)
  - [Any Block](#any-block)
  - [Any Quote](#any-quote)
- [Treesitter](#treesitter)
<!--toc:end-->

# Plaintext Text Objects
## Words
- `` aw `` - a word (includes surrounding white space)
- `` iw `` - inner word (does not include surrounding white space)

```
Lorem ipsum dolor sit amet...
```

`daw`

Lorem dolor sit amet...


## Sentences
- `` as `` - a sentence
- `` is `` - inner sentence

```
Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt
ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.
```

`cis`

```
 Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris
nisi ut aliquip ex ea commodo consequat.
```

## Paragraphs

- `` ap `` - a paragraph
- `` ip `` - inner paragraph

```
Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do
eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis 
nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat.

Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt
mollit anim id est laborum.
```

`dap`

```
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla
pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt
mollit anim id est laborum.
```

# Programming Language Text Objects
## Strings


- `` a" `` - a double quoted string
- `` i" `` - inner double quoted string
- `` a' `` - a single quoted string
- `` i' `` - inner single quoted string
- `` a\` `` - a back quoted string
- `` i\` `` - inner back quoted string

```ruby
puts 'Hello "world"'
```

`` ci" ``

```ruby
puts 'Hello ""'
```

Notice that the cursor was not even within the double-quoted phrase (“world”); the command defaulted to changing the first double-quoted phrase in the line.

```ruby
puts 'Hello "world"'
```

`` ci' ``

```ruby
puts ''
```

## Parentheses
- `` a) `` - a parenthesized block
- `` i) `` - inner parenthesized block

```
Project.all(:conditions => { :published => true })
```

`` da) ``

```
Project.all
```

## Brackets

- `` a] `` - a bracketed block
- `` i] `` - inner bracketed block

```lisp
(defn sum [x y]
  (+ x y))
```

`` di] ``

```lisp
(defn sum []
  (+ x y))
```

## Markup Language Tags
- `` at `` - a tag block
- `` it `` - inner tag block

```html
<h2>Sample Title</h2>
```

`cit`

```html
<h2></h2>
```

Notice that the cursor was not even within the \<h2\>. This is a very efficient way to quickly replace tag content.

- `` a> `` - a single tag
- `` i> `` - inner single tag

```html
<div id="content"></div>
```

`` di> ``

```html
<></div>
```

# Vim Scripts Providing Additional Text Objects
## CamelCaseMotion
- `` i,w `` - inner camel or snake-cased word

```
BeanFactoryTransactionAttributeSourceAdvisor
```

`` ci,w ``

```
FactoryTransactionAttributeSourceAdvisor
```

## VimTextObj

- `` aa `` - an argument
- `` ia `` - inner argument

```python
foo(42, bar(5), 'hello');
```

`cia`

```python
foo(42, , 'hello');
```


## Indent Object
- `` ai `` - the current indentation level and the line above
- `` ii `` - the current indentation level excluding the line above

```python
def foo():
  if 3 > 5:
    return True
  return "foo"
```

`dai`

```python
def foo():
  return "foo"
```

# Targets.vim

## Pair Text Objects

These text objects are similar to the built in text objects such as `i)`.
Supported trigger characters:

- `(` `)` (work on parentheses)
- `{` `}` `B` (work on curly braces)
- `[` `]` (work on square brackets)
- `<` `>` (work on angle brackets)
- `` t `` (work on tags)

Pair text objects work over multiple lines and support seeking. See below for
details about seeking.

The following examples will use parentheses, but they all work for each listed
trigger character accordingly.

### In Pair

`i( i) i{ i} iB i[ i] i< i> it`

- Select inside of pair characters.
- This overrides Vim's default text object to allow seeking for the next pair
  in the current line to the right or left when the cursor is not inside a
  pair. This behavior is similar to Vim's seeking behavior of `di'` when not
  inside of quotes, but it works both ways.
- Accepts a count to select multiple blocks.

```
      ............
a ( b ( cccccccc ) d ) e
   │   └── i) ──┘   │
   └───── 2i) ──────┘
```

### A Pair

`a( a) a{ a} aB a[ a] a< a> at`

- Select a pair including pair characters.
- Overrides Vim's default text object to allow seeking.
- Accepts a count.

```
      ............
a ( b ( cccccccc ) d ) e
  │   └─── a) ───┘   │
  └────── 2a) ───────┘
```

### Inside Pair

`I( I) I{ I} IB I[ I] I< I> It`

- Select contents of pair characters.
- Like inside of parentheses, but exclude whitespace at both ends. Useful for
  changing contents while preserving spacing.
- Accepts a count.

```
      ............
a ( b ( cccccccc ) d ) e
    │   └─ I) ─┘   │
    └──── 2I) ─────┘
```

### Around Pair

`A( A) A{ A} AB A[ A] A< A> At`

- Select around pair characters.
- Like a pair, but include whitespace at one side of the pair. Prefers to
  select trailing whitespace, falls back to select leading whitespace.
- Accepts a count.

```
      ............
a ( b ( cccccccc ) d ) e
  │   └─── A) ────┘   │
  └────── 2A) ────────┘
```

### Next and Last Pair

`in( an( In( An( il( al( Il( Al( ...`

Work directly on distant pairs without moving there separately.

All the above pair text objects can be shifted to the next pair by
including the letter `n`. The command `in)` selects inside of the next
pair. Use the letter `l` instead to work on the previous (last) pair. Uses
a count to skip multiple pairs. Skipping works over multiple lines.

See our Cheat Sheet for two charts summarizing all pair mappings.

### Pair Seek

If any of the normal pair commands (not containing `n` or `l`) is executed when
the cursor is not positioned inside a pair, it seeks for pairs before or after
the cursor by searching for the appropriate delimiter on the current line. This
is similar to using the explicit version containing `n` or `l`, but in only
seeks on the current line.

## Quote Text Objects

These text objects are similar to the built in text objects such as `i'`.
Supported trigger characters:

- `'`     (work on single quotes)
- `"`     (work on double quotes)
- `` ` `` (work on back ticks)

These quote text objects try to be smarter than the default ones. They count
the quotation marks from the beginning of the line to decide which of these are
the beginning of a quote and which ones are the end.

If you type `ci"` on the `,` in the example below, it will automatically skip
and change `world` instead of changing `,` between `hello` and `world`.

```
buffer │ join("hello", "world")
proper │      └─────┘  └─────┘
false  │            └──┘
```

Quote text objects work over multiple lines and support seeking. See below for
details about seeking.

The following examples will use single quotes, but they all work for each
mentioned separator character accordingly.

### In Quote

`` i' i" i` ``

- Select inside quote.
- This overrides Vim's default text object to allow seeking in both directions.

```
  ............
a ' bbbbbbbb ' c ' d ' e
   └── i' ──┘
```

### A Quote

``a' a" a` ``

- Select a quote.
- This overrides Vim's default text object to support seeking.
- Unlike Vim's quote text objects, this incudes no surrounding whitespace.

```
  ............
a ' bbbbbbbb ' c ' d ' e
  └─── a' ───┘
```

### Inside Quote

``I' I" I` ``

- Select contents of a quote.
- Like inside quote, but exclude whitespace at both ends. Useful for changing
  contents while preserving spacing.

```
  ............
a ' bbbbbbbb ' c ' d ' e
    └─ I' ─┘
```

### Around Quote

``A' A" A` ``

- Select around a quote.
- Like a quote, but include whitespace in one direction. Prefers to select
  trailing whitespace, falls back to select leading whitespace.

```
  ............
a ' bbbbbbbb ' c ' d ' e
  └─── A' ────┘
```

### Next and Last Quote

`in' In' An' il' Il' Al' ...`

Work directly on distant quotes without moving there separately.

All the above pair text objects can be shifted to the next quote by
including the letter `n`. The command `in'` selects inside of the next
single quotes. Use the letter `l` instead to work on the previous (last)
quote. Uses a count to skip multiple quotation characters.

See our [Cheat Sheet][cheatsheet] for a chart summarizing all quote mappings.

### Quote Seek

If any of the normal quote commands (not containing `n` or `l`) is executed
when the cursor is not positioned inside a quote, it seeks for quotes before or
after the cursor by searching for the appropriate delimiter on the current
line. This is similar to using the explicit version containing `n` or `l`.

## Separator Text Objects

These text objects are based on single separator characters like the comma in
one of our examples above. The text between two instances of the separator
character can be operated on with these targets.

Supported separators:

```
, . ; : + - = ~ _ * # / | \ & $
```

Separator text objects work over multiple lines and support seeking.

The following examples will use commas, but they all work for each listed
separator character accordingly.

### In Separator

`i, i. i; i: i+ i- i= i~ i_ i* i# i/ i| i\ i& i$`

- Select inside separators. Similar to in quote.

```
      ...........
a , b , cccccccc , d , e
       └── i, ──┘
```

### A Separator

`a, a. a; a: a+ a- a= a~ a_ a* a# a/ a| a\ a& a$`

- Select an item in a list separated by the separator character.
- Includes the leading separator, but excludes the trailing one. This leaves
  a proper list separated by the separator character after deletion. See the
  examples above.

```
      ...........
a , b , cccccccc , d , e
      └─── a, ──┘
```

### Inside Separator

`I, I. I; I: I+ I- I= I~ I_ I* I# I/ I| I\ I& I$`

- Select contents between separators.
- Like inside separators, but exclude whitespace at both ends. Useful for
  changing contents while preserving spacing.

```
      ...........
a , b , cccccccc , d , e
        └─ I, ─┘
```

### Around Separator

`A, A. A; A: A+ A- A= A~ A_ A* A# A/ A| A\ A& A$`

- Select around a pair of separators.
- Includes both separators and a surrounding whitespace, similar to `a'` and
  `A(`.

```
      ...........
a , b , cccccccc , d , e
      └─── A, ────┘
```

### Next and Last Separator

`in, an, In, An, il, al, Il, Al, ...`

Work directly on distant separators without moving there separately.

All the above separator text objects can be shifted to the next separator by
including the letter `n`. The command `in,` selects inside of the next commas.
Use the letter `l` instead to work on the previous (last) separators. Uses the
count to skip multiple separator characters.

See our [Cheat Sheet][cheatsheet] for a chart summarizing all separator mappings.

### Separator Seek

Like quote seeking. If any of the normal separator commands (not
containing `n` or `l`) is executed when the cursor is not positioned inside a
pair of separators, it seeks for the separator before or after the cursor.
This is similar to using the explicit version containing `n` or `l`.

## Argument Text Objects

These text objects are similar to separator text objects, but are specialized
for arguments surrounded by braces and commas. They also take matching braces
into account to capture only valid arguments.

Argument text objects work over multiple lines and support seeking.

### In Argument

`ia`

- Select inside arguments. Similar to in quote.
- Accepts a count.

```
      ...........
a , b ( cccccccc , d ) e
       └── ia ──┘
```

### An Argument

`aa`

- Select an argument in a list of arguments.
- Includes a separator if preset, but excludes surrounding braces. This leaves
  a proper argument list after deletion.
- Accepts a count.

```
      ...........
a , b ( cccccccc , d ) e
        └─── aa ──┘
```

### Inside Argument

`Ia`

- Select content of an argument.
- Like inside separators, but exclude whitespace at both ends. Useful for
  changing contents while preserving spacing.
- Accepts a count.

```
      ...........
a , b ( cccccccc , d ) e
        └─ Ia ─┘
```

### Around Argument

`Aa`

- Select around an argument.
- Includes both delimiters and a surrounding whitespace, similar to `a'` and
  `A(`.
- Accepts a count.

```
      ...........
a , b ( cccccccc , d ) e
      └─── Aa ────┘
```

### Next and Last Argument

`ina ana Ina Ana ila ala Ila Ala`

Work directly on distant arguments without moving there separately.

All the above argument text objects can be shifted to the next argument by
including the letter `n`. The command `ina` selects inside of the next
argument. Use the letter `l` instead to work on the previous (last) argument.
Uses a [count] to skip multiple argument characters. The order is determined by
the nearest surrounding argument delimiter.

See our [Cheat Sheet][cheatsheet] for a chart summarizing all argument mappings.

### Argument Seek

Like separator seeking. If any of the normal argument commands (not containing
`n` or `l`) is executed when the cursor is not positioned inside an argument,
it seeks for the argument before or after the cursor. This is similar to using
the explicit version containing `n` or `l`.

## Multi Text Objects

Two multi text objects are included in default settings. See the section on
settings below to see how to set up other similar multi text objects or
customize the built in ones.

### Any Block

`inb anb Inb Anb ilb alb Ilb Alb`

Similar to pair text objects, if you type `dib` within `()` it will delete in
these. If you do the same within `{}` it will delete in those. If you type
`d2inb` it will skip one next pair (any kind) and delete in the one after (any
kind). If you're within `()` nested in `{}`, type `d2ib` to delete in `{}`. All
of the usual seeking, growing and skipping works.

### Any Quote

`inq anq Inq Anq ilq alq Ilq Alq`

Similar to quote text objects, if you type `diq` within `""` it will delete in
these. If you do the same within `''` it will delete in those. If you type
`d2inq` it will skip one next quote text object (any kind) and delete in the
one after (any kind). If you're within `""` nested in `''`, type `d2iq` to
delete in `''`. All of the usual seeking, growing and skipping works.

# Treesitter

- @attribute.inner
- @attribute.outer
- @block.inner
- @block.outer
- @call.inner
- @call.outer
- `ic` - @class.inner
- `ac` - @class.outer
- @comment.outer
- @conditional.inner
- @conditional.outer
- @frame.inner
- @frame.outer
- `if` - @function.inner
- `af` - @function.outer
- @loop.inner
- @loop.outer
- @parameter.inner
- @parameter.outer
- @scopename.inner
- @statement.outer
