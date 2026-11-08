# CLIPSMarkdown

A codex for Markdown written in
[CLIPS](https://clipsrules.net/)
and CLIPS's Object Oriented Language, COOL!
This repo provides a way to turn Markdown into CLIPS objects
as well as print out Markdown from CLIPS objects.

[API.md](API.md) gives the instances and the facts that a program can match.

## Usage

You must have CLIPS installed and available on your system. Check out the
[official sourceforge page](https://sourceforge.net/projects/clipsrules/)
for download links or use
[CLIPSenv](https://github.com/mrryanjohnston/CLIPSenv)
to install. The file `program.bat` parses the `test*.md` files in
`tests/cases`. Running it looks like this:

```
$ clips -f2 program.bat
:::::::::::##################::!!!!!!!!!!!!!%%%%%%%%%%%%%%%%!!%%%%%***********~~~~~~~
8922 rules fired        Run time is 0.0718710422515869 seconds.
124139.009543903 rules per second.
94 mean number of facts (131 maximum).
391 mean number of instances (791 maximum).
34 mean number of activations (61 maximum).
         CLIPS (6.4.2 1/14/25)
CLIPS>
```

Every parsed file leaves behind one `MD-DOCUMENT` instance whose `id` slot
holds the id the file was parsed under:

```
CLIPS> (do-for-instance ((?d MD-DOCUMENT)) (eq ?d:id test13.md) (ppinstance ?d))
[gen650] of MD-DOCUMENT
   (id test13.md)
   (blocks [gen612] [gen649])
```

`send` the `pprint` message to any instance to write it back out as Markdown:

```
CLIPS> (send [gen650] pprint t)
> A quote holding a list:
>
> - quoted one
> - quoted two
>
> And a closing paragraph.
```

Parse your own Markdown by opening it and asserting the start fact:

```
CLIPS> (open notes.md notes)
TRUE
CLIPS> (assert (md-parse (stream notes) (id notes)))
<Fact-52>
CLIPS> (run)
CLIPS> (do-for-instance ((?d MD-DOCUMENT)) (eq ?d:id notes) (send ?d pprint t))
```

The document is a graph of instances, so `do-for-all-instances` and
`defrule` can query it. [API.md](API.md) gives each class and its slots,
with examples.

## Developer notes

A few "rules of thumb" I've determined from making CLIPS codecs:

1. Aim for 1 `defrule` activation on average always. The more activations you have,
   the slower things go.
2. Avoid `if` statements in the RHS of rules. Just create a new rule to handle
   the conditional.
3. Check one character at a time with `(get-char)`. Checking each character
   individually might be tedious, but it allows us the highest fidelity
   and means that we never need to "backtrack" and consider characters
   that were read in the past.

## Supported syntax

Everything John Gruber's
[syntax description](https://daringfireball.net/projects/markdown/syntax.text)
specifies, plus fenced code blocks from
[CommonMark](https://spec.commonmark.org).

## Tests

Run the suite with:

```
make test
```

There are two directories: `cases` and `expected`.
`cases` are read into CLIPS, and a successful test is when
the `MD-DOCUMENT` parsed from a file in `cases` prints back out
into its same named file in `expected`.

## Linting

Run `make lint` to check that the rules follow the practices of a COOL codec.
The lint script, `lint/lint.bat`, is written in CLIPS. It parses every
file in `tests/cases` one rule at a time and checks:

1. only one rule is on the agenda at a time while one case is parsed
2. all the cases parse together in one run, each to the file named the same
   in `tests/expected`, firing as many rules as they fired alone put together
3. no rule has an `(if` in its right hand side
4. no deffunction called from a rule's right hand side, directly or through
   another deffunction, has an `(if`

It prints one line per violation, a summary, and exits with status 1 when
it found a violation.

## License

[MIT](LICENSE)
