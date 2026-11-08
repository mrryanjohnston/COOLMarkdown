# COOLMarkdown API

This document gives the facts and the instances of the codec. A program
matches them with a pattern, or finds them with `find-instance` or
`find-fact`.

Each instance has an `id` slot. The `id` is the id of the stream that the
document came from. The `MD-DOCUMENT` instance is the last instance that the
decoder makes for a stream. A rule that matches it sees a complete document.

Each class has a `pprint` message-handler. It writes the instance to a
logical name as Markdown.

## Facts

### `md-parse`

The start fact. Assert it one time for each stream. The codec retracts it.

| Slot | Value |
|---|---|
| `stream` | The logical name of an open file or socket. |
| `id` | The id of the results from this stream. |

### `md-ready`

The codec asserts it when it starts to read. It retracts it when it makes
the `MD-DOCUMENT`.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |

### `md-stream`

The codec asserts it when it starts to read. It retracts it when it makes
the `MD-DOCUMENT`. Match it to get the logical name of the stream for the
id.

| Slot | Value |
|---|---|
| `id` | The id of the stream. |
| `stream` | The logical name of the stream. |

The codec reads to the end of the stream. It does not close the stream. A
last line with no newline is a line.

The other facts of the codec are the parse state. They have the prefix
`md-`. The codec retracts them before it makes the `MD-DOCUMENT`.

## Instances

Each class is a subclass of `MD-MARKDOWN`. Each instance has this slot:

| Slot | Value |
|---|---|
| `id` | The id of the stream. |

A text slot holds the text as it is in the stream, with the backslash
escapes. The encoder writes a backslash before a character that can open a
construct.

### Blocks

The classes that follow occur in a `blocks` slot. The encoder writes a
blank line between two blocks. It does not write a blank line between two
`MD-LINK-DEFINITION` instances.

#### `MD-DOCUMENT`

One document.

| Slot | Value |
|---|---|
| `blocks` | The block instances, in sequence. |

`(send ?d pprint ?t)` writes the document to the logical name `?t`, with a
newline at the end.

#### `MD-BLOCK-QUOTE`

One block quote.

| Slot | Value |
|---|---|
| `blocks` | The block instances in the quote, in sequence. |

The encoder writes `> ` before each line.

#### `MD-LIST`

One list.

| Slot | Value |
|---|---|
| `ordered` | `TRUE` for a list with numbers. If not, `FALSE`. |
| `start` | The number of the first item of an ordered list. The default is `1`. |
| `loose` | `TRUE` when a blank line was between the items in the stream. If not, `FALSE`. |
| `items` | The `MD-LIST-ITEM` instances, in sequence. |

The encoder writes `- ` before each item of a list that is not ordered.
It writes a number, a `.` and a space before each item of an ordered
list. The numbers start at `start`. It writes a blank line between the
items when `loose` is `TRUE`. It also writes one when an item has a block
after its first block that is not an `MD-LIST`.

#### `MD-LIST-ITEM`

One item of a list.

| Slot | Value |
|---|---|
| `blocks` | The block instances in the item, in sequence. |

#### `MD-HEADING`

One heading.

| Slot | Value |
|---|---|
| `level` | `1` to `6`. |
| `setext` | `TRUE` for a heading with a line of `=` or `-` below it. `FALSE` for a heading with `#` before it. |
| `inlines` | The inline instances, in sequence. |

A setext heading has a `level` of `1` or `2`. The encoder writes `===`
below a level 1 setext heading. It writes `---` below a level 2 setext
heading.

#### `MD-PARAGRAPH`

One paragraph.

| Slot | Value |
|---|---|
| `inlines` | The inline instances, in sequence. |

#### `MD-CODE-BLOCK`

One code block.

| Slot | Value |
|---|---|
| `info` | The info string after the first fence, or `""`. |
| `text` | The lines of code, with a newline between each line. |
| `fenced` | `TRUE` for a block between fences. `FALSE` for a block with an indent of four spaces. |

The encoder writes a fence of backticks before and after a fenced block.
The fence is longer than the longest run of backticks in `text`. It has
three backticks or more. The encoder writes four spaces before each line
of a block that is not fenced. A block that is not fenced has `""` in
`info`.

#### `MD-THEMATIC-BREAK`

One thematic break. It has no slots of its own. The encoder writes `---`.

#### `MD-HTML-BLOCK`

One block of HTML.

| Slot | Value |
|---|---|
| `text` | The lines of HTML as they are in the stream, with a newline between each line. |

#### `MD-LINK-DEFINITION`

One link reference definition.

| Slot | Value |
|---|---|
| `label` | The label between the brackets, as text. |
| `destination` | The destination as text. |
| `title` | The title as text, or `nil`. |

The encoder writes the label between brackets, a colon, a space, the
destination and the title. It writes the title between double quotes. It
writes the destination between `<` and `>` when the destination is `""`
or has a space.

### Inlines

The classes that follow occur in an `inlines` slot.

#### `MD-TEXT`

A run of text.

| Slot | Value |
|---|---|
| `value` | The text. |

The encoder writes a backslash before these characters:

- a `\`, a backtick, a `*` or a `[`
- a `_` next to a character that is not a letter or a digit
- a `<` before a letter or a digit
- a `]` in a link
- a character that can open a block, at the start of a line.

#### `MD-EMPHASIS`

A run of emphasis.

| Slot | Value |
|---|---|
| `inlines` | The inline instances, in sequence. |

The encoder writes a `*` before and after.

#### `MD-STRONG`

A run of strong emphasis.

| Slot | Value |
|---|---|
| `inlines` | The inline instances, in sequence. |

The encoder writes `**` before and after.

#### `MD-CODE-SPAN`

One code span.

| Slot | Value |
|---|---|
| `value` | The code, without the space that a span can have before and after it. |
| `fence` | The number of backticks before and after the code in the stream. The default is `1`. |

The encoder writes `fence` backticks before and after the code. It writes
more when `value` has a run of backticks that is as long. It writes a
space between the backticks and a `value` that starts or ends with a
backtick.

#### `MD-LINK`

One link.

| Slot | Value |
|---|---|
| `inlines` | The inline instances of the link text, in sequence. |
| `destination` | The destination as text. `""` when the link has none. |
| `title` | The title as text, or `nil`. |
| `label` | `nil` for an inline link. `""` for a reference link that has its text as its label. If not, the label as text. |

A reference link gets `destination` and `title` from the first
`MD-LINK-DEFINITION` of the document with the same label. Case and
repeated spaces do not make a label different. A reference that no
definition matches keeps its label. It has `""` in `destination`.

The encoder writes the text between brackets. When `label` is not `nil`,
it then writes the label between brackets. If not, it writes the
destination and the title between parentheses, in the form of a
definition.

#### `MD-IMAGE`

One image.

| Slot | Value |
|---|---|
| `description` | The text between the brackets. |
| `destination` | The destination as text. `""` when the image has none. |
| `title` | The title as text, or `nil`. |
| `label` | `nil` for an inline image. `""` for a reference image that has its description as its label. If not, the label as text. |

The `description` is the text of the `MD-TEXT` instances between the
brackets. The other inlines are not in it. The encoder writes `!` and then
the same as for an `MD-LINK`.

#### `MD-AUTOLINK`

One autolink.

| Slot | Value |
|---|---|
| `destination` | The URL or the email address, without the `<` and the `>`. |

#### `MD-ENTITY`

One entity reference.

| Slot | Value |
|---|---|
| `value` | The reference as it is in the stream, with the `&` and the `;`. For example `&amp;`. |

#### `MD-HTML-INLINE`

One HTML tag.

| Slot | Value |
|---|---|
| `value` | The tag as it is in the stream, with the `<` and the `>`. |

#### `MD-SOFT-BREAK`

A line end in a paragraph. It has no slots of its own. The encoder writes
a newline.

#### `MD-HARD-BREAK`

A line end with two spaces or more before it. It has no slots of its own.
The encoder writes two spaces and a newline.

## Examples

Each heading in each parsed file:

```
CLIPS> (do-for-all-instances ((?h MD-HEADING)) TRUE
  (println ?h:id " " ?h:level " " (send (nth$ 1 ?h:inlines) get-value)))
test29.md 1 A Setext H1
test29.md 2 A Setext H2
test24.md 1 Mixed document
test24.md 2 A section
test21.md 2 A heading with a closing sequence
test.md 1 Heading one
test.md 2 Heading two
test.md 3 Heading three
test.md 4 Heading four
test.md 5 Heading five
test.md 6 Heading six
```

A `defrule` that finds each link in a list item:

```
CLIPS> (defrule links-in-lists
?link <- (object (is-a MD-LINK))
?p <- (object (is-a MD-PARAGRAPH) (inlines $? =(instance-name ?link) $?))
(object (is-a MD-LIST-ITEM) (blocks $? =(instance-name ?p) $?))
=>
(println (send ?link get-destination)))
CLIPS> (run)
https://clipsrules.net
https://spec.commonmark.org
```
