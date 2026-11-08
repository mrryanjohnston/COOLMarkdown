; A Markdown codex: rules read one character at a time and grow a tree of
; COOL instances; `pprint` message handlers turn that tree back into Markdown.
; Grammar follows CommonMark (https://spec.commonmark.org) for the subset
; listed in README.md.

(defglobal ?*md-digit* = (create$ 48 49 50 51 52 53 54 55 56 57))
(defglobal ?*md-upalpha* = (create$ 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90))
(defglobal ?*md-lowalpha* = (create$ 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122))
(defglobal ?*md-alpha* = (create$ ?*md-upalpha* ?*md-lowalpha*))
(defglobal ?*md-alphanum* = (create$ ?*md-alpha* ?*md-digit*))
; ASCII punctuation, the characters a backslash may escape
(defglobal ?*md-punctuation* = (create$ 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 58 59 60 61 62 63 64 91 92 93 94 95 96 123 124 125 126))
; characters that may begin a block when they open a line
(defglobal ?*md-block-marker* = (create$ 35 61 60 62 45 42 43 95 96 126 32 9 10 13 -1 48 49 50 51 52 53 54 55 56 57))
; characters that end a run of literal inline text
(defglobal ?*md-inline-marker* = (create$ 42 95 96 91 93 33 38 60 92 10 13 -1))

;;; the prefix `pprint` re-emits after every newline, so that the blocks
;;; nested inside a quote or a list item stay inside it.
(defglobal ?*md-prefix* = "")
;;; TRUE while the inline being printed opens a line, where a character that
;;; opens a block has to be escaped; see md-print-inlines
(defglobal ?*md-line-start* = FALSE)
;;; TRUE while the inlines of a link are printed, where a `]` would close it
(defglobal ?*md-in-link* = FALSE)

;;; ------------------------------------------------------------------
;;; the document tree
;;; ------------------------------------------------------------------
(defclass MD-MARKDOWN (is-a USER) (slot id))

(defclass MD-DOCUMENT (is-a MD-MARKDOWN) (multislot blocks))
(defclass MD-BLOCK-QUOTE (is-a MD-MARKDOWN) (multislot blocks))
(defclass MD-LIST (is-a MD-MARKDOWN)
	(slot ordered (default FALSE))
	(slot start (default 1))
	;; TRUE when blank lines separated the items as they were written
	(slot loose (default FALSE))
	(multislot items))
(defclass MD-LIST-ITEM (is-a MD-MARKDOWN) (multislot blocks))
(defclass MD-PARAGRAPH (is-a MD-MARKDOWN) (multislot inlines))
(defclass MD-HEADING (is-a MD-MARKDOWN) (slot level) (slot setext (default FALSE)) (multislot inlines))
(defclass MD-CODE-BLOCK (is-a MD-MARKDOWN) (slot info (default "")) (slot text (default ""))
	(slot fenced (default TRUE)))
(defclass MD-THEMATIC-BREAK (is-a MD-MARKDOWN))
(defclass MD-LINK-DEFINITION (is-a MD-MARKDOWN) (slot label) (slot destination) (slot title (default nil)))

(defclass MD-TEXT (is-a MD-MARKDOWN) (slot value))
(defclass MD-EMPHASIS (is-a MD-MARKDOWN) (multislot inlines))
(defclass MD-STRONG (is-a MD-MARKDOWN) (multislot inlines))
(defclass MD-CODE-SPAN (is-a MD-MARKDOWN) (slot value) (slot fence (default 1)))
(defclass MD-LINK (is-a MD-MARKDOWN) (multislot inlines) (slot destination) (slot title (default nil))
	;; nil for an inline link, "" for an implicit reference, else the label
	(slot label (default nil)))
(defclass MD-IMAGE (is-a MD-MARKDOWN) (slot description) (slot destination) (slot title (default nil))
	(slot label (default nil)))
(defclass MD-AUTOLINK (is-a MD-MARKDOWN) (slot destination))
(defclass MD-ENTITY (is-a MD-MARKDOWN) (slot value))
(defclass MD-HTML-INLINE (is-a MD-MARKDOWN) (slot value))
(defclass MD-HTML-BLOCK (is-a MD-MARKDOWN) (slot text))
(defclass MD-SOFT-BREAK (is-a MD-MARKDOWN))
(defclass MD-HARD-BREAK (is-a MD-MARKDOWN))

;;; the plain text an inline lends to an image description or a reference
;;; label: only text has any
(defmessage-handler MD-MARKDOWN describe primary () "")
(defmessage-handler MD-TEXT describe primary () ?self:value)

;;; TRUE for a run of text that gathered nothing, which a container drops
;;; as it closes; see md-without-empty-text
(defmessage-handler MD-MARKDOWN blank primary () FALSE)
(defmessage-handler MD-TEXT blank primary () (eq ?self:value ""))

; a tag opens with a name, a `!` declaration or comment, or a `?` instruction
(defglobal ?*md-tag-opener* = (create$ ?*md-alpha* 33 63))
(defglobal ?*md-word-chars* = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz")

;;; `str-index` answers FALSE rather than 0 when it finds nothing, and an empty
;;; needle is never a match.
(deffunction md-contains (?needle ?haystack)
	(and (neq ?needle "") (neq FALSE (str-index ?needle ?haystack))))

(deffunction md-word-char (?character)
	(md-contains ?character ?*md-word-chars*))

;;; A list is loose when an item carries a block past its first that is not a
;;; nested list; loose items are separated by a blank line, tight ones are not.
(deffunction md-loose-item (?item)
	(bind ?loose FALSE)
	(bind ?index 0)
	(foreach ?block (send ?item get-blocks)
		(bind ?index (+ ?index 1))
		(if (and (> ?index 1) (neq MD-LIST (class ?block)))
		    then (bind ?loose TRUE)))
	?loose)

(deffunction md-loose-list (?items)
	(bind ?loose FALSE)
	(foreach ?item ?items
		(if (md-loose-item ?item) then (bind ?loose TRUE)))
	?loose)

;;; A code span's fence has to outlast the longest run of backticks it holds,
;;; and content that starts or ends with a backtick needs a space to stand
;;; clear of the fence.
(deffunction md-code-span-fence (?value)
	(bind ?longest 0)
	(bind ?run 0)
	(bind ?i 1)
	(while (<= ?i (str-length ?value))
		(if (eq "`" (sub-string ?i ?i ?value))
		    then (bind ?run (+ ?run 1))
			 (if (> ?run ?longest) then (bind ?longest ?run))
		    else (bind ?run 0))
		(bind ?i (+ ?i 1)))
	(+ ?longest 1))

(deffunction md-code-span-pad (?value)
	(if (or (eq "`" (sub-string 1 1 ?value))
		(eq "`" (sub-string (str-length ?value) (str-length ?value) ?value)))
	    then " "
	    else ""))

;;; A fence has to outlast the longest run of backticks in the body it wraps.
(deffunction md-fence-length (?text)
	(bind ?longest 0)
	(bind ?run 0)
	(bind ?i 1)
	(while (<= ?i (str-length ?text))
		(if (eq "`" (sub-string ?i ?i ?text))
		    then (bind ?run (+ ?run 1))
			 (if (> ?run ?longest) then (bind ?longest ?run))
		    else (bind ?run 0))
		(bind ?i (+ ?i 1)))
	(if (>= ?longest 3) then (+ ?longest 1) else 3))

(deffunction md-print-destination (?t ?destination)
	(if (or (eq ?destination "") (md-contains " " ?destination))
	    then (printout ?t "<" ?destination ">")
	    else (printout ?t ?destination)))

(deffunction md-repeat (?string ?times)
	(bind ?result "")
	(loop-for-count ?times (bind ?result (str-cat ?result ?string)))
	?result)

;;; A quote contributes "> " to the prefix, but a blank line inside a quote is
;;; written as a bare ">", so trailing blanks come off before it is emitted.
(deffunction md-trim-right (?string)
	(bind ?end (str-length ?string))
	(while (and (> ?end 0) (eq " " (sub-string ?end ?end ?string)))
		(bind ?end (- ?end 1)))
	(sub-string 1 ?end ?string))

(deffunction md-trim (?string)
	(bind ?start 1)
	(bind ?end (str-length ?string))
	(while (and (<= ?start ?end) (eq " " (sub-string ?start ?start ?string)))
		(bind ?start (+ ?start 1)))
	(while (and (>= ?end ?start) (eq " " (sub-string ?end ?end ?string)))
		(bind ?end (- ?end 1)))
	(sub-string ?start ?end ?string))

;;; Reference labels match without regard to case or to how their internal
;;; whitespace is written: tabs count as spaces, a run of them as one, and
;;; those at either end not at all.
(deffunction md-normalize-label (?label)
	(bind ?result (lowcase ?label))
	(bind ?tab (str-index (format nil "%c" 9) ?result))
	(while (neq FALSE ?tab) do
		(bind ?result (str-cat (sub-string 1 (- ?tab 1) ?result) " "
		                       (sub-string (+ ?tab 1) (str-length ?result) ?result)))
		(bind ?tab (str-index (format nil "%c" 9) ?result)))
	(bind ?run (str-index "  " ?result))
	(while (neq FALSE ?run) do
		(bind ?result (str-cat (sub-string 1 ?run ?result)
		                       (sub-string (+ ?run 2) (str-length ?result) ?result)))
		(bind ?run (str-index "  " ?result)))
	(md-trim ?result))

;;; TRUE when ?line is a run of at least ?length copies of ?marker and nothing
;;; else, which is how a fenced code block is closed.
(deffunction md-closing-fence (?line ?marker ?length)
	(bind ?trimmed (md-trim ?line))
	(bind ?size (str-length ?trimmed))
	(bind ?fence (str-cat (md-repeat (format nil "%c" ?marker) ?size)))
	(and (>= ?length 3) (>= ?size ?length) (eq ?trimmed ?fence)))

;;; A run of text ends as an MD-TEXT among its container's inlines, even a
;;; run that gathered nothing; the container drops those as it closes.
(deffunction md-append-text (?name ?items ?value)
	(create$ ?items (make-instance of MD-TEXT (id ?name) (value ?value))))

;;; TRUE when a container has gathered nothing but blank runs of text
(deffunction md-all-blank (?inlines)
	(bind ?blank TRUE)
	(foreach ?inline ?inlines (bind ?blank (and ?blank (send ?inline blank))))
	?blank)

(deffunction md-without-empty-text (?inlines)
	(foreach ?empty (find-all-instances ((?t MD-TEXT))
			(and (eq ?t:value "") (member$ (instance-name ?t) ?inlines)))
		(bind ?inlines (delete-member$ ?inlines (instance-name ?empty)))
		(unmake-instance ?empty))
	?inlines)

;;; A run needs content beside it to open or close: a blank, a line ending, the
;;; end of the file, or the start of the line before the run is not content.
(deffunction md-content (?char)
	(not (member$ ?char (create$ 0 32 9 10 13 -1))))

;;; `_` only flanks a word, so that snake_case is never emphasis; `*` may open
;;; or close anywhere.
;;;
;;; A run also has to be left-flanking to open: content on its right, and, when
;;; that content is punctuation, a blank or punctuation on its left.  That is
;;; what keeps the `***` of `***a*****.` from opening against the full stop
;;; after it, leaving it to close the runs it is inside instead.
(deffunction md-can-open (?marker ?before ?after)
	(and (md-content ?after)
	     (or (not (member$ ?after ?*md-punctuation*))
		 (not (md-content ?before))
		 (member$ ?before ?*md-punctuation*))
	     (or (= ?marker 42) (not (member$ ?before ?*md-alphanum*)))))

(deffunction md-can-close (?marker ?after)
	(or (= ?marker 42) (not (member$ ?after ?*md-alphanum*))))

(deffunction md-is-kind (?fact ?kind ?marker)
	(and (eq (fact-slot-value ?fact kind) ?kind)
	     (= (fact-slot-value ?fact marker) ?marker)))

;;; TRUE when the run closes the run it sits inside.  Each closing rule takes
;;; the markers its own kind is written with and leaves the rest of the run to
;;; close what is around it, so `*****a*****` closes an emphasis and then two
;;; strongs.  A run that has opened something is inside what it opened, and
;;; what is left of it nests rather than closes.
(deffunction md-delimiter-closes (?parent ?marker ?count ?char ?opened)
	(and (not ?opened)
	     (md-can-close ?marker ?char)
	     (or (md-is-kind ?parent emphasis ?marker)          ; delimiter-closes-emphasis
		 (and (>= ?count 2)
		      (md-is-kind ?parent strong ?marker)))))  ; delimiter-closes-strong

;;; TRUE when the run opens emphasis.  A run that has opened nothing yet stands
;;; aside for the closing rules when it sits directly inside a run of its own
;;; kind, but what is left of a run that has already opened one nests, which is
;;; how `****strong****` becomes strong inside strong.
(deffunction md-delimiter-opens (?parent ?marker ?count ?char ?before ?opened)
	(and (md-can-open ?marker ?before ?char)
	     (or ?opened
		 (and (>= ?count 2)
		      (not (md-is-kind ?parent strong ?marker)))     ; delimiter-opens-strong
		 (and (= ?count 1)
		      (not (md-is-kind ?parent emphasis ?marker)))))) ; delimiter-opens-emphasis

;;; TRUE when nothing more specific than `delimiter-literal` can claim this run,
;;; so its markers become text: it has stopped growing, it closes nothing and it
;;; opens nothing.
(deffunction md-delimiter-literal (?parent ?marker ?count ?char ?before ?opened)
	(and (neq ?char ?marker)                                   ; delimiter-continue
	     (not (md-delimiter-closes ?parent ?marker ?count ?char ?opened))
	     (not (md-delimiter-opens ?parent ?marker ?count ?char ?before ?opened))))

;;; The alt text of an image is plain text, so only the text of its inlines
;;; survives.
(deffunction md-describe (?inlines)
	(bind ?result "")
	(foreach ?inline ?inlines
		(bind ?result (str-cat ?result (send ?inline describe))))
	?result)

;;; Splits the body of a code block back into the lines it was read as.
(deffunction md-lines (?text)
	(bind ?lines (create$))
	(bind ?rest ?text)
	(bind ?break (str-index (format nil "%c" 10) ?rest))
	(while (neq FALSE ?break)
		(bind ?lines (create$ ?lines (sub-string 1 (- ?break 1) ?rest)))
		(bind ?rest (sub-string (+ ?break 1) (str-length ?rest) ?rest))
		(bind ?break (str-index (format nil "%c" 10) ?rest)))
	(if (neq ?rest "") then (bind ?lines (create$ ?lines ?rest)))
	?lines)

(deffunction md-newline (?t)
	(printout ?t crlf ?*md-prefix*))

(deffunction md-blank-line (?t)
	(printout ?t crlf (md-trim-right ?*md-prefix*) crlf ?*md-prefix*))

;;; A blank line inside a code block holds no code, so the prefix that would
;;; otherwise pad it out to the content column comes off, the same way a blank
;;; line between blocks is written as a bare `>`.
(deffunction md-code-newline (?t ?line)
	(if (eq ?line "")
	    then (printout ?t crlf (md-trim-right ?*md-prefix*))
	    else (md-newline ?t)))

;;; Blocks are parted by a blank line, except that adjacent link definitions
;;; belong together.
(deffunction md-separate-blocks (?t ?previous ?block)
	(if (and (eq MD-LINK-DEFINITION (class ?previous))
		 (eq MD-LINK-DEFINITION (class ?block)))
	    then (md-newline ?t)
	    else (md-blank-line ?t)))

;;; the index just past a run of ?character that opens ?value
(deffunction md-run-end (?value ?character)
	(bind ?i 1)
	(while (eq ?character (sub-string ?i ?i ?value))
		(bind ?i (+ ?i 1)))
	?i)

;;; TRUE when ?value ends before index ?i or holds a space or tab there
(deffunction md-space-or-end (?value ?i)
	(or (> ?i (str-length ?value))
	    (md-contains (sub-string ?i ?i ?value) " 	")))

;;; The index of the character whose escape stops ?value from opening a block
;;; when it opens a line, or 0 when it opens none: `#` of a heading, `>` of
;;; a quote, `-` `+` of a bullet, `-` `=` of an underline or a thematic break,
;;; `~` of a fence, and the `.` or `)` after the ordinal of a list item.
(deffunction md-block-opener (?value)
	(if (eq ?value "") then (return 0))
	(bind ?first (sub-string 1 1 ?value))
	(bind ?end (md-run-end ?value ?first))
	(if (eq ?first ">") then (return 1))
	(if (and (eq ?first "#") (<= ?end 7) (md-space-or-end ?value ?end)) then (return 1))
	(if (and (eq ?first "-") (md-space-or-end ?value ?end)) then (return 1))
	(if (and (eq ?first "=") (> ?end (str-length ?value))) then (return 1))
	(if (and (eq ?first "+") (md-space-or-end ?value 2) (< 1 (str-length ?value))) then (return 1))
	(if (and (eq ?first "~") (>= ?end 4)) then (return 1))
	(if (md-contains ?first "0123456789")
	    then (bind ?i 1)
		 (while (md-contains (sub-string ?i ?i ?value) "0123456789")
			(bind ?i (+ ?i 1)))
		 (if (and (md-contains (sub-string ?i ?i ?value) ".)")
			  (<= ?i (str-length ?value))
			  (md-space-or-end ?value (+ ?i 1))
			  (< (+ ?i 1) (str-length ?value)))
		     then (return ?i)))
	0)

;;; Text is stored unescaped, so the characters that would otherwise reopen an
;;; inline construct are escaped on the way out, as is a character that would
;;; open a block where the text opens a line, and a `]` that would close the
;;; link the text sits in.  Escaping is idempotent, which keeps parse/print
;;; round trips stable.  `_` is only escaped where it would actually flank a
;;; word, so that snake_case survives untouched.
(deffunction md-escaped (?value ?line-start ?in-link)
	(bind ?result "")
	(bind ?length (str-length ?value))
	(bind ?opener (if ?line-start then (md-block-opener ?value) else 0))
	(bind ?i 1)
	(while (<= ?i ?length)
		(bind ?character (sub-string ?i ?i ?value))
		(if (or (md-contains ?character "\\`*[")
			(= ?i ?opener)
			(and ?in-link (eq ?character "]"))
			(and (eq ?character "<")
			     (md-word-char (sub-string (+ ?i 1) (+ ?i 1) ?value)))
			(and (eq ?character "_")
			     (or (not (md-word-char (sub-string (- ?i 1) (- ?i 1) ?value)))
				 (not (md-word-char (sub-string (+ ?i 1) (+ ?i 1) ?value))))))
		    then (bind ?result (str-cat ?result "\\")))
		(bind ?result (str-cat ?result ?character))
		(bind ?i (+ ?i 1)))
	?result)

;;; A title is printed between double quotes, so a quote or a backslash in it
;;; is escaped.
(deffunction md-escaped-title (?title)
	(bind ?result "")
	(bind ?length (str-length ?title))
	(bind ?i 1)
	(while (<= ?i ?length)
		(bind ?character (sub-string ?i ?i ?title))
		(if (md-contains ?character "\\\"") then (bind ?result (str-cat ?result "\\")))
		(bind ?result (str-cat ?result ?character))
		(bind ?i (+ ?i 1)))
	?result)

(deffunction md-print-title (?t ?title)
	(if (neq ?title nil) then (printout ?t " \"" (md-escaped-title ?title) "\"")))

;;; Prints a run of inlines, telling each whether it opens a line: the first
;;; does when its container does, and any that follows a break does.
(deffunction md-print-inlines (?t ?inlines ?line-start)
	(foreach ?inline ?inlines
		(bind ?*md-line-start* ?line-start)
		(send ?inline pprint ?t)
		(bind ?line-start (or (eq MD-SOFT-BREAK (class ?inline))
				      (eq MD-HARD-BREAK (class ?inline))))))

;;; ------------------------------------------------------------------
;;; parse state
;;;
;;; Every open construct points at its parent, and a finished child is appended
;;; to its parent's multislot as an instance-name.  At any moment exactly one
;;; path from the document down to the innermost open construct exists, so
;;; `depth` alone says whether a fact sits inside a given container.
;;; ------------------------------------------------------------------
(deftemplate md-blocks
	(slot name)
	(slot parent (default nil))
	(slot kind (default document)) ; document | quote | item
	(slot depth (default 0))
	(multislot items))

(deftemplate md-list
	(slot name)
	(slot parent)
	(slot depth)
	(slot ordered (default FALSE))
	(slot start (default 1))
	(slot blank (default FALSE)) ; a blank line has closed the item now open
	(slot loose (default FALSE))
	(slot marker)  ; the bullet, or the delimiter that follows an ordinal
	(slot indent)  ; column the item content starts at
	(multislot items))

(deftemplate md-inlines
	(slot name)
	(slot parent)
	(slot depth)
	(slot kind) ; paragraph | heading | emphasis | strong | link
	(slot level (default 0))
	(slot marker (default 0)) ; the delimiter that will close an emphasis
	(slot destination (default ""))
	(slot title (default nil))
	(slot prefix (default "")) ; the `[` or `![` a link or image opened with
	(multislot items))

(deftemplate md-text
	(slot name)
	(slot parent)
	(slot value (default ""))
	;; the code of the character last appended, which is what decides whether a
	;; `_` here would flank a word
	(slot last (default 0)))

(deftemplate md-code-block
	(slot name)
	(slot parent)
	(slot depth)
	(slot marker)
	(slot length)
	(slot info (default ""))
	(slot text (default ""))
	(slot fenced (default TRUE))
	(slot blanks (default 0)) ; blank lines held back, in case the block goes on
	(slot phase (default info))) ; info | body

;;; line level bookkeeping
(deftemplate md-line-start
	(slot name)
	(slot container)
	(slot indent (default 0))
	;; TRUE for the single space a quote marker is allowed to swallow
	(slot fresh (default FALSE)))
(deftemplate md-resume (slot name))
(deftemplate md-closing (slot name) (slot target) (slot depth))
(deftemplate md-atx (slot name) (slot container) (slot level))
(deftemplate md-code-line (slot name) (slot block) (slot value (default "")))
(deftemplate md-run
	(slot name)
	(slot container)
	(slot marker)
	(slot count)
	(slot spaces (default 0)) ; a thematic break may space its markers apart
	(slot spaced (default FALSE)) ; TRUE once a marker has followed a space
	(slot prefix (default "")) ; the characters consumed, verbatim
	(slot indent (default 0)))
(deftemplate md-ordinal (slot name) (slot container) (slot digits) (slot delim (default 0)) (slot indent (default 0)) (slot last (default 0)))
(deftemplate md-indent (slot name) (slot container) (slot count))

;;; inline level bookkeeping
(deftemplate md-delimiter
	(slot name) (slot marker) (slot count) (slot before (default 0))
	;; TRUE once this run has opened a run of its own, so that what is left of
	;; it may nest inside what it just opened
	(slot opened (default FALSE)))
(deftemplate md-pending-delimiter (slot name) (slot marker) (slot count))
(deftemplate md-pending-ticks (slot name) (slot count))
;;; a run of `#` inside a heading, and the blanks after it
(deftemplate md-hashes (slot name) (slot hashes) (slot blanks (default "")))
(deftemplate md-bang (slot name) (slot char))
(deftemplate md-ticks (slot name) (slot count))
(deftemplate md-code-span (slot name) (slot parent) (slot length) (slot value (default "")) (slot pending (default 0)))
(deftemplate md-angle (slot name) (slot parent) (slot value (default "")) (slot last (default 60)) (multislot codes))
(deftemplate md-entity (slot name) (slot value (default "")) (slot last (default 38)))
(deftemplate md-setext-heading (slot name) (slot heading))
(deftemplate md-html-block (slot name) (slot parent) (slot depth) (slot text (default "")))
(deftemplate md-html-line (slot name) (slot block) (slot value (default "")))
(deftemplate md-block-angle
	(slot name) (slot container)
	(slot value (default "")) ; the characters read after the `<`, verbatim
	(slot tag (default ""))   ; the name among them, lowercased
	(slot last (default 60))  ; the code of the character read last
	(multislot codes))
(deftemplate md-pending-angle (slot name) (slot value) (slot last) (multislot codes))
(deftemplate md-escape (slot name) (slot char))
(deftemplate md-destination
	(slot name) (slot link)
	(slot value (default "")) (slot title (default nil))
	(slot label (default nil))
	(slot phase (default destination)))
(deftemplate md-definition
	(slot name) (slot container) (slot label)
	(slot destination (default "")) (slot title (default nil))
	(slot closer (default 34))
	(slot phase (default pre-destination)))
(deftemplate md-link-definition (slot name) (slot label) (slot destination) (slot title) (slot sequence))
(deftemplate md-unresolved (slot name) (slot link) (slot label) (slot sequence))
;;; numbers the definitions and references of a document in the order read
(deftemplate md-sequence (slot name) (slot next (default 1)))

(deffunction md-trailing-spaces (?value)
	(bind ?count 0)
	(bind ?i (str-length ?value))
	(while (and (> ?i 0) (eq " " (sub-string ?i ?i ?value)))
		(bind ?count (+ ?count 1))
		(bind ?i (- ?i 1)))
	?count)

;;; TRUE when a rule more specific than `run-literal` claims this combination.
;;; Kept beside the run-* rules below; every clause names the rule it guards.
(deffunction md-run-resolves (?marker ?count ?spaces ?char)
	(or
		(= ?marker ?char)                                  ; run-continue
		(and (member$ ?marker (create$ 45 61 42 43 95))
		     (member$ ?char (create$ 32 9)))               ; run-absorb-space
		(and (member$ ?marker (create$ 45 61))
		     (member$ ?char (create$ 10 -1)))              ; run-setext-heading, run-literal-underline, run-literal-spaced-underline
		(and (member$ ?marker (create$ 45 42 95))
		     (>= ?count 3)
		     (member$ ?char (create$ 10 -1)))              ; run-thematic-break, run-thematic-break-closes-paragraph
		(and (member$ ?marker (create$ 45 42 43))
		     (= ?count 1) (> ?spaces 0)
		     (not (member$ ?char (create$ 32 9 10 13 -1)))) ; run-open-list, run-next-list-item
		(and (member$ ?marker (create$ 42 95))
		     (= ?spaces 0)
		     (not (member$ ?char (create$ 32 9 10 13 -1)))) ; run-emphasis
		(and (member$ ?marker (create$ 96 126)) (>= ?count 3)) ; run-code-fence
		(and (= ?marker 96) (<= ?count 2))))               ; run-code-span

(deftemplate md-literal (slot name) (slot container) (slot value) (slot last (default 0)))
(deftemplate md-pending-break (slot name) (slot parent) (slot hard (default FALSE)))
(deftemplate md-pending-block
	(slot name)
	(slot container)
	(slot kind) ; paragraph | heading | thematic-break | code | quote | item
	(slot level (default 0))
	(slot marker (default 0))
	(slot length (default 0))
	(slot ordered (default FALSE))
	(slot start (default 1))
	(slot indent (default 0)))
(deftemplate md-at-eof (slot name))

;;; ------------------------------------------------------------------
;;; closing --- retire everything open strictly inside (closing target).
;;;
;;; Each rule waits until the subtree beneath the construct it closes is
;;; already finished, so the cascade unwinds innermost first without any
;;; salience.  The depth test keeps the cascade from reaching past the target
;;; into its ancestors: at any moment exactly one path is open, so the facts
;;; shallower than the target are precisely that target's ancestors.
;;; ------------------------------------------------------------------
(defrule md-close-text-run
	(md-closing (name ?name) (depth ?depth))
	?p <- (md-inlines (name ?name) (depth ?d&:(> ?d ?depth)) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	(test (neq "" (md-trim-right ?value)))
	=>
	(retract ?t)
	(modify ?p (items ?items (make-instance of MD-TEXT (id ?name) (value (md-trim-right ?value))))))

(defrule md-close-blank-text-run
	(md-closing (name ?name) (depth ?depth))
	?p <- (md-inlines (name ?name) (depth ?d&:(> ?d ?depth)))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	(test (eq "" (md-trim-right ?value)))
	=>
	(retract ?t))

(defrule md-discard-pending-break
	(md-closing (name ?name))
	(not (md-text (name ?name)))
	?b <- (md-pending-break (name ?name))
	=>
	(retract ?b))

;;; An emphasis or strong run whose closing delimiter never arrived was never
;;; emphasis at all, so its delimiter goes back to being literal text.
(defrule md-close-unfinished-emphasis
	(md-closing (name ?name) (depth ?depth))
	?p <- (md-inlines (name ?name) (items $?outer))
	?e <- (md-inlines (name ?name) (parent ?p) (depth ?d&:(> ?d ?depth))
		(kind ?kind&emphasis|strong) (marker ?marker) (items $?inner))
	(md-delimiter-width ?kind ?width)
	(not (md-text (name ?name)))
	(not (md-pending-break (name ?name)))
	(not (md-inlines (name ?name) (parent ?e)))
	=>
	(retract ?e)
	(modify ?p (items ?outer
		(make-instance of MD-TEXT (id ?name)
			(value (md-repeat (format nil "%c" ?marker) ?width)))
		?inner)))

;;; A link or an image whose `](target)` never arrived keeps its opening
;;; bracket and the inlines it had already collected.
(defrule md-close-unfinished-link
	(md-closing (name ?name) (depth ?depth))
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (depth ?d&:(> ?d ?depth))
		(kind link|image) (prefix ?prefix) (items $?inner))
	(not (md-text (name ?name)))
	(not (md-pending-break (name ?name)))
	(not (md-destination (name ?name)))
	(not (md-inlines (name ?name) (parent ?l)))
	=>
	(retract ?l)
	(modify ?p (items ?outer
		(make-instance of MD-TEXT (id ?name) (value ?prefix))
		?inner)))

(defrule md-discard-unfinished-destination
	(md-closing (name ?name))
	?d <- (md-destination (name ?name))
	=>
	(retract ?d))

(defrule md-close-paragraph
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name) (items $?items))
	?p <- (md-inlines (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth)) (kind paragraph) (items $?inner&:(> (length$ ?inner) 0)))
	(not (md-text (name ?name)))
	(not (md-pending-break (name ?name)))
	(not (md-inlines (name ?name) (parent ?p)))
	=>
	(retract ?p)
	(modify ?c (items ?items (make-instance of MD-PARAGRAPH (id ?name)
		(inlines (md-without-empty-text ?inner))))))

;;; A paragraph that never collected an inline (a line of only whitespace, say)
;;; leaves nothing behind.
(defrule md-close-empty-paragraph
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name))
	?p <- (md-inlines (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth)) (kind paragraph) (items))
	(not (md-text (name ?name)))
	(not (md-pending-break (name ?name)))
	(not (md-inlines (name ?name) (parent ?p)))
	=>
	(retract ?p))

(defrule md-close-heading
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name) (items $?items))
	?h <- (md-inlines (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth)) (kind heading) (level ?level) (items $?inner))
	(not (md-text (name ?name)))
	(not (md-setext-heading (name ?name) (heading ?h)))
	(not (md-inlines (name ?name) (parent ?h)))
	=>
	(retract ?h)
	(modify ?c (items ?items (make-instance of MD-HEADING (id ?name) (level ?level)
		(inlines (md-without-empty-text ?inner))))))

(defrule md-close-setext-heading
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name) (items $?items))
	?h <- (md-inlines (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth)) (kind heading) (level ?level) (items $?inner))
	?marker <- (md-setext-heading (name ?name) (heading ?h))
	(not (md-text (name ?name)))
	(not (md-pending-break (name ?name)))
	(not (md-inlines (name ?name) (parent ?h)))
	=>
	(retract ?h ?marker)
	(modify ?c (items ?items (make-instance of MD-HEADING (id ?name) (level ?level)
		(setext TRUE) (inlines (md-without-empty-text ?inner))))))

(defrule md-close-code-block
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name) (items $?items))
	?b <- (md-code-block (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth))
		(info ?info) (text ?text) (fenced ?fenced))
	(not (md-code-line (name ?name) (block ?b)))
	=>
	(retract ?b)
	(modify ?c (items ?items (make-instance of MD-CODE-BLOCK (id ?name)
		(info ?info) (text ?text) (fenced ?fenced)))))

(defrule md-close-html-block
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name) (items $?items))
	?b <- (md-html-block (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth)) (text ?text))
	(not (md-html-line (name ?name) (block ?b)))
	=>
	(retract ?b)
	(modify ?c (items ?items (make-instance of MD-HTML-BLOCK (id ?name) (text ?text)))))

(defrule md-close-list-item
	(md-closing (name ?name) (depth ?depth))
	?l <- (md-list (name ?name) (items $?items))
	?i <- (md-blocks (name ?name) (parent ?l) (depth ?d&:(> ?d ?depth)) (kind item) (items $?blocks))
	(not (md-text (name ?name)))
	(not (md-inlines (name ?name) (parent ?i)))
	(not (md-blocks (name ?name) (parent ?i)))
	(not (md-list (name ?name) (parent ?i)))
	(not (md-code-block (name ?name) (parent ?i)))
	=>
	(retract ?i)
	(modify ?l (items ?items (make-instance of MD-LIST-ITEM (id ?name) (blocks ?blocks)))))

(defrule md-close-list
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name) (items $?items))
	?l <- (md-list (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth))
		(ordered ?ordered) (start ?start) (loose ?loose) (items $?entries))
	(not (md-blocks (name ?name) (parent ?l)))
	=>
	(retract ?l)
	(modify ?c (items ?items
		(make-instance of MD-LIST (id ?name) (ordered ?ordered) (start ?start)
			(loose ?loose) (items ?entries)))))

(defrule md-close-block-quote
	(md-closing (name ?name) (depth ?depth))
	?c <- (md-blocks (name ?name) (items $?items))
	?q <- (md-blocks (name ?name) (parent ?c) (depth ?d&:(> ?d ?depth)) (kind quote) (items $?blocks))
	(not (md-text (name ?name)))
	(not (md-inlines (name ?name) (parent ?q)))
	(not (md-blocks (name ?name) (parent ?q)))
	(not (md-list (name ?name) (parent ?q)))
	(not (md-code-block (name ?name) (parent ?q)))
	=>
	(retract ?q)
	(modify ?c (items ?items (make-instance of MD-BLOCK-QUOTE (id ?name) (blocks ?blocks)))))

(defrule md-end-closing
	?closing <- (md-closing (name ?name) (target ?target))
	(not (md-text (name ?name)))
	(not (md-inlines (name ?name) (parent ?target)))
	(not (md-blocks (name ?name) (parent ?target)))
	(not (md-list (name ?name) (parent ?target)))
	(not (md-code-block (name ?name) (parent ?target)))
	(not (md-html-block (name ?name) (parent ?target)))
	=>
	(retract ?closing))

;;; ------------------------------------------------------------------
;;; lines
;;; ------------------------------------------------------------------
;;; The block-level elements Gruber names.  Facts rather than a function, so
;;; that the rules below match against them instead of calling out to a scan.
(deffacts md-html-block-tags
	(md-html-block-tag "div")
	(md-html-block-tag "table")
	(md-html-block-tag "pre")
	(md-html-block-tag "p")
	(md-html-block-tag "blockquote")
	(md-html-block-tag "ul")
	(md-html-block-tag "ol")
	(md-html-block-tag "li")
	(md-html-block-tag "dl")
	(md-html-block-tag "dt")
	(md-html-block-tag "dd")
	(md-html-block-tag "h1")
	(md-html-block-tag "h2")
	(md-html-block-tag "h3")
	(md-html-block-tag "h4")
	(md-html-block-tag "h5")
	(md-html-block-tag "h6")
	(md-html-block-tag "hr")
	(md-html-block-tag "form")
	(md-html-block-tag "fieldset")
	(md-html-block-tag "iframe")
	(md-html-block-tag "math")
	(md-html-block-tag "ins")
	(md-html-block-tag "del")
	(md-html-block-tag "script")
	(md-html-block-tag "noscript")
	(md-html-block-tag "section")
	(md-html-block-tag "article")
	(md-html-block-tag "header")
	(md-html-block-tag "footer")
	(md-html-block-tag "nav")
	(md-html-block-tag "aside")
	(md-html-block-tag "figure")
	(md-html-block-tag "main")
)

;;; the underlines a setext heading may have, each with the level it gives
(deffacts md-setext-underlines
	(md-setext-underline 61 1) ; =
	(md-setext-underline 45 2) ; -
)

;;; the markers an emphasis or strong run is written with
(deffacts md-delimiter-widths
	(md-delimiter-width emphasis 1)
	(md-delimiter-width strong 2)
)

;;; the quotes a definition title may be written in, each with its closer
(deffacts md-title-quotes
	(md-title-quote 34 34) ; "
	(md-title-quote 39 39) ; '
	(md-title-quote 40 41) ; ( )
)

;;; the start fact: a program asserts it with the
;;; logical name of an open file or socket and an id for the result, then
;;; runs.  A document codec reads to the end of the stream.
(deftemplate md-parse
	(slot stream)
	(slot id))

;;; one stream that the codec reads: the id of its facts and its logical name
;;; the ready fact has the shape of the protocol codecs; a
;;; document codec asserts it at the start and retracts it with the document
(deftemplate md-ready
	(slot id))

(deftemplate md-stream
	(slot id)
	(slot stream))

(deffunction md-read (?stream)
	"the next character of the stream, past any carriage return; a rule that reads matches the md-stream fact for the stream's logical name"
	(bind ?char (get-char ?stream))
	(while (= ?char 13) do (bind ?char (get-char ?stream)))
	?char)

(defrule md-start
	?p <- (md-parse (stream ?stream) (id ?name))
	=>
	(retract ?p)
	(assert (md-stream (id ?name) (stream ?stream)) (md-ready (id ?name))
	        (md-sequence (name ?name))
	        (md-line-start (name ?name) (container (assert (md-blocks (name ?name))))))
	(assert (md-char ?name (md-read ?stream))))

;;; The line-start facts walk down the containers that are already open, one
;;; marker at a time, and stop at the container the line's content belongs to.
;;; Every rule below is blocked while a fenced code block owns that container,
;;; so code content is never mistaken for a marker.

(defrule md-line-start-continue-quote
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	?q <- (md-blocks (name ?name) (parent ?c) (kind quote))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-line-start (name ?name) (container ?q) (fresh TRUE)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-open-quote
	?c <- (md-blocks (name ?name) (depth ?depth))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	(not (md-blocks (name ?name) (parent ?c) (kind quote)))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind quote)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-quote-marker-space
	?ls <- (md-line-start (name ?name) (fresh TRUE))
	?ch <- (md-char ?name 32)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?ls (fresh FALSE))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-quote-marker-no-space
	?ls <- (md-line-start (name ?name) (fresh TRUE))
	(md-char ?name ?ch&~32)
	=>
	(modify ?ls (fresh FALSE)))

;;; Indenting as far as the content column of the open list item steps into it,
;;; which is what lets a list item hold more than one block.
(defrule md-line-start-indent-into-item
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (indent ?i) (fresh FALSE))
	?l <- (md-list (name ?name) (parent ?c) (indent ?column))
	?item <- (md-blocks (name ?name) (parent ?l) (kind item))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 32)
	(test (= (+ ?i 1) ?column))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-line-start (name ?name) (container ?item)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-indent
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (indent ?i&:(< ?i 3)) (fresh FALSE))
	(not (md-list (name ?name) (parent ?c) (indent ?column&:(= (+ ?i 1) ?column))))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 32)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?ls (indent (+ ?i 1)))
	(assert (md-char ?name (md-read ?stream))))

;;; Four spaces of indentation open a code block, unless they are continuing a
;;; paragraph, in which case they are only indentation.
(defrule md-line-start-indent-under-paragraph
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (indent 3) (fresh FALSE))
	(md-inlines (name ?name) (parent ?c) (kind paragraph))
	(not (md-list (name ?name) (parent ?c) (indent 4)))
	?ch <- (md-char ?name 32)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?ls (indent 4))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-open-indented-code
	?c <- (md-blocks (name ?name) (depth ?depth))
	?ls <- (md-line-start (name ?name) (container ?c) (indent 3) (fresh FALSE))
	(not (md-inlines (name ?name) (parent ?c) (kind paragraph)))
	(not (md-code-block (name ?name)))
	(not (md-list (name ?name) (parent ?c) (indent 4)))
	?ch <- (md-char ?name 32)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-code-line (name ?name) (block
		(assert (md-code-block (name ?name) (parent ?c) (depth (+ ?depth 1))
			(marker 0) (length 0) (fenced FALSE) (phase body))))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-continue-indented-code
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (indent 3) (fresh FALSE))
	?b <- (md-code-block (name ?name) (fenced FALSE))
	(not (md-list (name ?name) (parent ?c) (indent 4)))
	?ch <- (md-char ?name 32)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-code-line (name ?name) (block ?b)))
	(assert (md-char ?name (md-read ?stream))))

;;; A blank line does not end an indented block; it is held back until a
;;; further indented line proves the block went on.
(defrule md-indented-code-blank-line
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	?b <- (md-code-block (name ?name) (fenced FALSE) (blanks ?blanks))
	?ch <- (md-char ?name 10)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(modify ?b (blanks (+ ?blanks 1)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-indented-code-block-at-end-of-file
	?c <- (md-blocks (name ?name) (depth ?depth))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	(md-code-block (name ?name) (fenced FALSE))
	(md-char ?name -1)
	=>
	(retract ?ls)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-resume (name ?name))))

	;;; A tab at the start of a line stands for four columns of indentation,
;;; which is enough on its own to open a code block.
(defrule md-line-start-tab-into-item
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (indent ?i) (fresh FALSE))
	?l <- (md-list (name ?name) (parent ?c) (indent ?column&:(<= ?column (+ ?i 4))))
	?item <- (md-blocks (name ?name) (parent ?l) (kind item))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-line-start (name ?name) (container ?item) (indent (- (+ ?i 4) ?column))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-tab-continues-indented-code
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (indent ?i) (fresh FALSE))
	?b <- (md-code-block (name ?name) (fenced FALSE))
	(not (md-list (name ?name) (parent ?c) (indent ?column&:(<= ?column (+ ?i 4)))))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-code-line (name ?name) (block ?b)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-line-start-tab-opens-indented-code
	?c <- (md-blocks (name ?name) (depth ?depth))
	?ls <- (md-line-start (name ?name) (container ?c) (indent ?i) (fresh FALSE))
	(not (md-inlines (name ?name) (parent ?c) (kind paragraph)))
	(not (md-code-block (name ?name)))
	(not (md-list (name ?name) (parent ?c) (indent ?column&:(<= ?column (+ ?i 4)))))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-code-line (name ?name) (block
		(assert (md-code-block (name ?name) (parent ?c) (depth (+ ?depth 1))
			(marker 0) (length 0) (fenced FALSE) (phase body))))))
	(assert (md-char ?name (md-read ?stream))))

;;; A tab continuing a paragraph is only indentation.
(defrule md-line-start-tab
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (indent ?i) (fresh FALSE))
	(md-inlines (name ?name) (parent ?c) (kind paragraph))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-list (name ?name) (parent ?c) (indent ?column&:(<= ?column (+ ?i 4)))))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?ls (indent (+ ?i 4)))
	(assert (md-char ?name (md-read ?stream))))

;;; A blank line closes everything open inside the container the line-start
;;; walked down to, so an unmarked line ends a quote while a `>` line reaches
;;; the quote and ends only the blocks inside it.  A list is the exception: it
;;; survives a blank line, which is what makes its items loose.
(defrule md-blank-line-inside-list-item
	?c <- (md-blocks (name ?name) (depth ?column))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	?l <- (md-list (name ?name) (depth ?list-depth))
	;; the line stopped at the item or above it; a line that walked deeper is a
	;; blank line inside whatever it walked into
	?b <- (md-blocks (name ?name) (parent ?l) (kind item) (depth ?depth&:(>= ?depth ?column)))
	;; a quote between the container and the list never got its marker on this
	;; line, so the blank line ends the quote and takes the list with it
	(not (md-blocks (name ?name) (kind quote)
		(depth ?d&:(> ?d ?column)&:(< ?d ?list-depth))))
	(not (md-list (name ?name) (parent ?b)))
	(not (md-code-block (name ?name)))
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(modify ?l (blank TRUE))
	(assert (md-closing (name ?name) (target ?b) (depth ?depth)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-blank-line
	?b <- (md-blocks (name ?name) (depth ?depth) (kind ?kind&~item))
	?ls <- (md-line-start (name ?name) (container ?b) (fresh FALSE))
	;; a list inside the container survives, which is what makes its items loose
	(not (md-list (name ?name) (parent ?b)))
	(not (md-code-block (name ?name)))
	(not (md-html-block (name ?name) (parent ?b)))
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-closing (name ?name) (target ?b) (depth ?depth)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-resume-line
	?r <- (md-resume (name ?name))
	(not (md-closing (name ?name)))
	(not (md-pending-block (name ?name)))
	(not (md-literal (name ?name)))
	?b <- (md-blocks (name ?name) (kind document))
	(md-char ?name ?ch&~-1)
	=>
	(retract ?r)
	(assert (md-line-start (name ?name) (container ?b))))

(defrule md-resume-at-end-of-file
	?r <- (md-resume (name ?name))
	(not (md-closing (name ?name)))
	(not (md-pending-block (name ?name)))
	(not (md-literal (name ?name)))
	?b <- (md-blocks (name ?name) (kind document) (depth ?depth))
	?ch <- (md-char ?name -1)
	=>
	(retract ?r ?ch)
	(assert (md-closing (name ?name) (target ?b) (depth ?depth)))
	(assert (md-at-eof (name ?name))))

(defrule md-finish-document
	?e <- (md-at-eof (name ?name))
	(not (md-closing (name ?name)))
	(not (md-definition (name ?name)))
	(not (md-unresolved (name ?name)))
	?b <- (md-blocks (name ?name) (kind document) (items $?items))
	?q <- (md-sequence (name ?name))
	?r <- (md-ready (id ?name))
	?s <- (md-stream (id ?name))
	=>
	(retract ?e ?b ?q ?r ?s)
	(do-for-all-facts ((?d md-link-definition)) (eq ?d:name ?name) (retract ?d))
	(make-instance of MD-DOCUMENT (id ?name) (blocks ?items)))

;;; ------------------------------------------------------------------
;;; opening a block, once the container it belongs to has been emptied
;;; ------------------------------------------------------------------
(defrule md-open-pending-quote
	?c <- (md-blocks (name ?name) (depth ?depth))
	?p <- (md-pending-block (name ?name) (container ?c) (kind quote))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-line-start (name ?name) (fresh TRUE)
		(container (assert (md-blocks (name ?name) (parent ?c) (kind quote) (depth (+ ?depth 1))))))))

(defrule md-open-pending-paragraph
	?c <- (md-blocks (name ?name) (depth ?depth))
	?p <- (md-pending-block (name ?name) (container ?c) (kind paragraph))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-text (name ?name)
		(parent (assert (md-inlines (name ?name) (parent ?c) (depth (+ ?depth 1)) (kind paragraph)))))))

(defrule md-open-pending-heading
	?c <- (md-blocks (name ?name) (depth ?depth))
	?p <- (md-pending-block (name ?name) (container ?c) (kind heading) (level ?level))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-text (name ?name)
		(parent (assert (md-inlines (name ?name) (parent ?c) (depth (+ ?depth 1))
			(kind heading) (level ?level)))))))

(defrule md-open-pending-thematic-break
	?c <- (md-blocks (name ?name) (items $?items))
	?p <- (md-pending-block (name ?name) (container ?c) (kind thematic-break))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(modify ?c (items ?items (make-instance of MD-THEMATIC-BREAK (id ?name)))))

(defrule md-open-pending-code-block
	?c <- (md-blocks (name ?name) (depth ?depth))
	?p <- (md-pending-block (name ?name) (container ?c) (kind code) (marker ?marker) (length ?length))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-code-block (name ?name) (parent ?c) (depth (+ ?depth 1))
		(marker ?marker) (length ?length))))

(defrule md-open-pending-list
	?c <- (md-blocks (name ?name) (depth ?depth))
	?p <- (md-pending-block (name ?name) (container ?c) (kind list)
		(marker ?marker) (ordered ?ordered) (start ?start) (indent ?indent))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-line-start (name ?name) (container
		(assert (md-blocks (name ?name) (kind item) (depth (+ ?depth 2)) (parent
			(assert (md-list (name ?name) (parent ?c) (depth (+ ?depth 1)) (marker ?marker)
				(ordered ?ordered) (start ?start) (indent ?indent))))))))))

(defrule md-open-pending-list-item
	?l <- (md-list (name ?name) (parent ?c) (depth ?depth) (marker ?marker))
	?p <- (md-pending-block (name ?name) (container ?c) (kind item) (marker ?marker) (indent ?indent))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(modify ?l (indent ?indent))
	(assert (md-line-start (name ?name) (container
		(assert (md-blocks (name ?name) (parent ?l) (kind item) (depth (+ ?depth 1))))))))

;;; ------------------------------------------------------------------
;;; `#` --- an ATX heading, or literal text when no space follows
;;; ------------------------------------------------------------------
(defrule md-line-start-hash
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 35)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-atx (name ?name) (container ?c) (level 1)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-atx-continue
	?a <- (md-atx (name ?name) (level ?level&:(< ?level 7)))
	?ch <- (md-char ?name 35)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?a (level (+ ?level 1)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-atx-open-heading
	?c <- (md-blocks (name ?name) (depth ?depth))
	?a <- (md-atx (name ?name) (container ?c) (level ?level&:(< ?level 7)))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?a ?ch)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind heading) (level ?level)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-atx-empty-heading
	?c <- (md-blocks (name ?name) (depth ?depth))
	?a <- (md-atx (name ?name) (container ?c) (level ?level&:(< ?level 7)))
	(md-char ?name 10|-1)
	=>
	(retract ?a)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind heading) (level ?level))))

(defrule md-atx-literal
	?c <- (md-blocks (name ?name))
	?a <- (md-atx (name ?name) (container ?c) (level ?level))
	(md-char ?name ?ch&~35&~32&~9&~10&~-1)
	=>
	(retract ?a)
	(assert (md-literal (name ?name) (container ?c) (value (md-repeat "#" ?level)) (last 35))))

(defrule md-atx-literal-too-deep
	?c <- (md-blocks (name ?name))
	?a <- (md-atx (name ?name) (container ?c) (level 7))
	(md-char ?name ?ch&~35)
	=>
	(retract ?a)
	(assert (md-literal (name ?name) (container ?c) (value "#######") (last 35))))

;;; ------------------------------------------------------------------
;;; runs of `- * + _ ` ~` --- a thematic break, a bullet, a code fence,
;;; an emphasis delimiter, a code span, or literal text
;;; ------------------------------------------------------------------
(defrule md-line-start-run
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE) (indent ?i))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name ?marker&45|61|42|43|95|96|126)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-run (name ?name) (container ?c) (marker ?marker) (count 1) (indent ?i)
		(prefix (format nil "%c" ?marker))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-run-continue
	?r <- (md-run (name ?name) (marker ?marker) (count ?count) (spaces ?spaces) (prefix ?prefix))
	?ch <- (md-char ?name ?marker)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?r (count (+ ?count 1)) (spaced (> ?spaces 0))
		(prefix (format nil "%s%c" ?prefix ?marker)))
	(assert (md-char ?name (md-read ?stream))))

;;; A thematic break is allowed to space its markers apart, so a space does not
;;; settle the line on its own; the character after it does.
(defrule md-run-absorb-space
	?r <- (md-run (name ?name) (marker 45|61|42|43|95) (spaces ?spaces) (prefix ?prefix))
	?ch <- (md-char ?name ?char&32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?r (spaces (+ ?spaces 1)) (prefix (format nil "%s%c" ?prefix ?char)))
	(assert (md-char ?name (md-read ?stream))))

;;; An underline directly beneath a paragraph makes that paragraph a heading,
;;; which is why a thematic break of `-` may not claim the line first.  An
;;; underline has no spaces between its markers, so `- - -` is never one.
(defrule md-run-setext-heading
	?c <- (md-blocks (name ?name) (depth ?depth))
	?p <- (md-inlines (name ?name) (parent ?c) (kind paragraph))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&61|45) (spaced FALSE))
	(md-setext-underline ?marker ?level)
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?r ?ch)
	(modify ?p (kind heading) (level ?level))
	(assert (md-setext-heading (name ?name) (heading ?p)))
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; An underline with no paragraph above it is just text.
(defrule md-run-literal-underline
	?c <- (md-blocks (name ?name))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&61|45) (count ?count) (prefix ?prefix))
	(not (md-inlines (name ?name) (parent ?c) (kind paragraph)))
	(md-char ?name 10|-1)
	(test (or (= ?marker 61) (< ?count 3)))
	=>
	(retract ?r)
	(assert (md-literal (name ?name) (container ?c) (value ?prefix) (last ?marker))))

;;; A spaced underline beneath a paragraph is text, since an underline has no
;;; spaces between its markers; when it is not a thematic break it continues
;;; the paragraph.
(defrule md-run-literal-spaced-underline
	?c <- (md-blocks (name ?name))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&61|45) (count ?count) (spaced TRUE) (prefix ?prefix))
	(md-inlines (name ?name) (parent ?c) (kind paragraph))
	(md-char ?name 10|-1)
	(test (or (= ?marker 61) (< ?count 3)))
	=>
	(retract ?r)
	(assert (md-literal (name ?name) (container ?c) (value ?prefix) (last ?marker))))

(defrule md-run-thematic-break
	?c <- (md-blocks (name ?name) (depth ?depth))
	(not (md-inlines (name ?name) (parent ?c) (kind paragraph)))
	?r <- (md-run (name ?name) (container ?c) (marker 45|42|95) (count ?count&:(>= ?count 3)))
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?r ?ch)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind thematic-break)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; A thematic break directly beneath a paragraph closes it, unless the line
;;; is an unspaced run of `-`, which is the paragraph's underline instead.
(defrule md-run-thematic-break-closes-paragraph
	?c <- (md-blocks (name ?name) (depth ?depth))
	(md-inlines (name ?name) (parent ?c) (kind paragraph))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&45|42|95) (count ?count&:(>= ?count 3)) (spaced ?spaced))
	?ch <- (md-char ?name 10|-1)
	(test (or (neq ?marker 45) ?spaced))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?r ?ch)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind thematic-break)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-run-next-list-item
	?c <- (md-blocks (name ?name))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&45|42|43) (count 1)
		(spaces ?spaces&:(> ?spaces 0)) (prefix ?prefix) (indent ?i))
	?l <- (md-list (name ?name) (parent ?c) (depth ?depth) (blank ?blank) (loose ?loose) (marker ?marker) (ordered FALSE))
	(md-char ?name ?ch&~32&~9&~10&~13&~-1)
	;; a run that is still growing belongs to run-continue
	(test (neq ?ch ?marker))
	=>
	(retract ?r)
	(modify ?l (blank FALSE) (loose (or ?loose ?blank)))
	(assert (md-closing (name ?name) (target ?l) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind item) (marker ?marker)
		(indent (+ ?i (str-length ?prefix))))))

(defrule md-run-open-list
	?c <- (md-blocks (name ?name) (depth ?depth))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&45|42|43) (count 1)
		(spaces ?spaces&:(> ?spaces 0)) (prefix ?prefix) (indent ?i))
	(not (md-list (name ?name) (parent ?c) (marker ?marker) (ordered FALSE)))
	(md-char ?name ?ch&~32&~9&~10&~13&~-1)
	;; a run that is still growing belongs to run-continue
	(test (neq ?ch ?marker))
	=>
	(retract ?r)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind list) (marker ?marker)
		(ordered FALSE) (indent (+ ?i (str-length ?prefix))))))

(defrule md-run-code-fence
	?c <- (md-blocks (name ?name) (depth ?depth))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&96|126) (count ?count&:(>= ?count 3)))
	(md-char ?name ?ch)
	(test (neq ?ch ?marker))
	=>
	(retract ?r)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind code) (marker ?marker) (length ?count))))

;;; A delimiter run that opens the line still belongs to a paragraph, so the
;;; paragraph is requested first and the delimiter is replayed into it.
(defrule md-run-emphasis
	?c <- (md-blocks (name ?name))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker&42|95) (spaces 0)
		(count ?count))
	(md-char ?name ?ch&~32&~9&~10&~13&~-1)
	(test (neq ?ch ?marker))
	=>
	(retract ?r)
	(assert (md-literal (name ?name) (container ?c) (value "")))
	(assert (md-pending-delimiter (name ?name) (marker ?marker) (count ?count))))

(defrule md-run-code-span
	?c <- (md-blocks (name ?name))
	?r <- (md-run (name ?name) (container ?c) (marker 96) (count ?count&:(<= ?count 2)))
	(md-char ?name ?ch&~96)
	=>
	(retract ?r)
	(assert (md-literal (name ?name) (container ?c) (value "")))
	(assert (md-pending-ticks (name ?name) (count ?count))))

(defrule md-run-literal
	?c <- (md-blocks (name ?name))
	?r <- (md-run (name ?name) (container ?c) (marker ?marker) (count ?count)
		(spaces ?spaces) (prefix ?prefix))
	(md-char ?name ?ch)
	(test (not (md-run-resolves ?marker ?count ?spaces ?ch)))
	=>
	(retract ?r)
	(assert (md-literal (name ?name) (container ?c) (value ?prefix) (last ?marker))))

;;; ------------------------------------------------------------------
;;; `<` --- an HTML block when it names a block-level element, and an
;;; ordinary inline otherwise
;;; ------------------------------------------------------------------
(defrule md-line-start-angle
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	(not (md-inlines (name ?name) (parent ?c) (kind paragraph)))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name 60) ; <
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-block-angle (name ?name) (container ?c)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-block-angle
	?a <- (md-block-angle (name ?name) (value ?value) (tag ?tag) (codes $?codes))
	?ch <- (md-char ?name ?char&:(member$ ?char ?*md-alphanum*))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?a (value (format nil "%s%c" ?value ?char))
		(tag (str-cat ?tag (lowcase (format nil "%c" ?char))))
		(last ?char)
		(codes ?codes ?char))
	(assert (md-char ?name (md-read ?stream))))

;;; A closing tag's slash comes before the name and is not part of it.
(defrule md-build-block-angle-slash
	?a <- (md-block-angle (name ?name) (value ?value) (tag ""))
	?ch <- (md-char ?name 47) ; /
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?a (value (format nil "%s%c" ?value 47)) (last 47))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-open-html-block
	?c <- (md-blocks (name ?name) (depth ?depth))
	?a <- (md-block-angle (name ?name) (container ?c) (value ?value) (tag ?tag))
	(md-char ?name ?ch)
	(md-html-block-tag ?tag)
	(test (not (or (member$ ?ch ?*md-alphanum*)
		       (and (= ?ch 47) (eq ?tag "")))))
	=>
	(retract ?a)
	(assert (md-html-line (name ?name) (value (format nil "%c%s" 60 ?value)) (block
		(assert (md-html-block (name ?name) (parent ?c) (depth (+ ?depth 1))))))))

;;; Not a block-level element, so the line is a paragraph after all and the
;;; characters already taken are handed to the inline machinery.
(defrule md-block-angle-is-inline
	?c <- (md-blocks (name ?name))
	?a <- (md-block-angle (name ?name) (container ?c) (value ?value) (tag ?tag) (last ?last) (codes $?codes))
	(md-char ?name ?ch)
	(not (md-html-block-tag ?tag))
	(test (not (or (member$ ?ch ?*md-alphanum*)
		       (and (= ?ch 47) (eq ?tag "")))))
	=>
	(retract ?a)
	(assert (md-literal (name ?name) (container ?c) (value "")))
	(assert (md-pending-angle (name ?name) (value ?value) (last ?last) (codes ?codes))))

(defrule md-open-pending-angle
	(md-text (name ?name))
	?p <- (md-pending-angle (name ?name) (value ?value) (last ?last) (codes $?codes))
	(not (md-literal (name ?name)))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-angle (name ?name) (value ?value) (last ?last) (codes ?codes))))

(defrule md-html-block-line-start
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	?b <- (md-html-block (name ?name) (parent ?c))
	(md-char ?name ?ch&~10&~-1)
	=>
	(retract ?ls)
	(assert (md-html-line (name ?name) (block ?b))))

(defrule md-build-html-line
	?b <- (md-html-block (name ?name))
	?l <- (md-html-line (name ?name) (block ?b) (value ?value))
	?ch <- (md-char ?name ?char&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?l (value (format nil "%s%c" ?value ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-html-line
	?b <- (md-html-block (name ?name) (text ?text))
	?l <- (md-html-line (name ?name) (block ?b) (value ?value))
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?l ?ch)
	(modify ?b (text (format nil "%s%s%c" ?text ?value 10)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; A blank line ends an HTML block, which is how Markdown gets back in.
(defrule md-end-html-block
	?c <- (md-blocks (name ?name) (depth ?depth))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	(md-html-block (name ?name) (parent ?c))
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; ------------------------------------------------------------------
;;; digits --- an ordered list marker, or literal text
;;; ------------------------------------------------------------------
(defrule md-line-start-digit
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE) (indent ?i))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	?ch <- (md-char ?name ?digit&:(member$ ?digit ?*md-digit*))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ls ?ch)
	(assert (md-ordinal (name ?name) (container ?c) (digits (format nil "%c" ?digit)) (indent ?i) (last ?digit)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-ordinal-continue
	?o <- (md-ordinal (name ?name) (digits ?digits) (delim 0))
	?ch <- (md-char ?name ?digit&:(member$ ?digit ?*md-digit*))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?o (digits (format nil "%s%c" ?digits ?digit)) (last ?digit))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-ordinal-delimiter
	?o <- (md-ordinal (name ?name) (delim 0))
	?ch <- (md-char ?name ?delim&46|41) ; . )
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?o (delim ?delim))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-ordinal-next-list-item
	?c <- (md-blocks (name ?name))
	?o <- (md-ordinal (name ?name) (container ?c) (digits ?digits) (delim ?delim&~0) (indent ?i))
	?l <- (md-list (name ?name) (parent ?c) (depth ?depth) (blank ?blank) (loose ?loose) (marker ?delim) (ordered TRUE))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?o ?ch)
	(modify ?l (blank FALSE) (loose (or ?loose ?blank)))
	(assert (md-closing (name ?name) (target ?l) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind item) (marker ?delim)
		(indent (+ ?i (+ 2 (str-length ?digits))))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-ordinal-open-list
	?c <- (md-blocks (name ?name) (depth ?depth))
	?o <- (md-ordinal (name ?name) (container ?c) (digits ?digits) (delim ?delim&~0) (indent ?i))
	(not (md-list (name ?name) (parent ?c) (marker ?delim) (ordered TRUE)))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?o ?ch)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind list) (marker ?delim) (ordered TRUE)
		(start (string-to-field ?digits)) (indent (+ ?i (+ 2 (str-length ?digits))))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-ordinal-literal
	?c <- (md-blocks (name ?name))
	?o <- (md-ordinal (name ?name) (container ?c) (digits ?digits) (delim 0) (last ?last))
	(md-char ?name ?ch&~46&~41)
	(test (not (member$ ?ch ?*md-digit*)))
	=>
	(retract ?o)
	(assert (md-literal (name ?name) (container ?c) (value ?digits) (last ?last))))

(defrule md-ordinal-delimiter-literal
	?c <- (md-blocks (name ?name))
	?o <- (md-ordinal (name ?name) (container ?c) (digits ?digits) (delim ?delim&~0))
	(md-char ?name ?ch&~32&~9)
	=>
	(retract ?o)
	(assert (md-literal (name ?name) (container ?c)
		(value (format nil "%s%c" ?digits ?delim)) (last ?delim))))

;;; ------------------------------------------------------------------
;;; anything else opens or continues a paragraph
;;; ------------------------------------------------------------------
(defrule md-line-start-text
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	(not (md-code-block (name ?name) (parent ?c) (fenced TRUE)))
	(not (md-html-block (name ?name) (parent ?c)))
	(md-char ?name ?ch)
	(test (not (member$ ?ch ?*md-block-marker*)))
	=>
	(retract ?ls)
	(assert (md-literal (name ?name) (container ?c) (value ""))))

(defrule md-literal-into-text
	?l <- (md-literal (name ?name) (value ?value&~"") (last ?last))
	?t <- (md-text (name ?name) (value ?existing))
	(not (md-closing (name ?name)))
	=>
	(retract ?l)
	(modify ?t (value (str-cat ?existing ?value)) (last ?last)))

;;; An empty literal only asks for a paragraph; it adds nothing to the text and
;;; so leaves the preceding character alone.
(defrule md-discard-empty-literal
	?l <- (md-literal (name ?name) (value ""))
	(md-text (name ?name))
	(not (md-closing (name ?name)))
	=>
	(retract ?l))

(defrule md-literal-continues-paragraph
	(md-literal (name ?name))
	?p <- (md-inlines (name ?name) (items $?items))
	?b <- (md-pending-break (name ?name) (parent ?p) (hard FALSE))
	(not (md-text (name ?name)))
	(not (md-closing (name ?name)))
	=>
	(retract ?b)
	(modify ?p (items ?items (make-instance of MD-SOFT-BREAK (id ?name))))
	(assert (md-text (name ?name) (parent ?p))))

(defrule md-literal-continues-paragraph-after-hard-break
	(md-literal (name ?name))
	?p <- (md-inlines (name ?name) (items $?items))
	?b <- (md-pending-break (name ?name) (parent ?p) (hard TRUE))
	(not (md-text (name ?name)))
	(not (md-closing (name ?name)))
	=>
	(retract ?b)
	(modify ?p (items ?items (make-instance of MD-HARD-BREAK (id ?name))))
	(assert (md-text (name ?name) (parent ?p))))

(defrule md-literal-opens-paragraph
	?c <- (md-blocks (name ?name) (depth ?depth))
	(md-literal (name ?name) (container ?c))
	(not (md-inlines (name ?name) (kind paragraph)))
	(not (md-text (name ?name)))
	(not (md-closing (name ?name)))
	(not (md-pending-block (name ?name)))
	=>
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-pending-block (name ?name) (container ?c) (kind paragraph))))

;;; ------------------------------------------------------------------
;;; ending a line
;;; ------------------------------------------------------------------
(defrule md-end-line-in-heading
	?b <- (md-blocks (name ?name) (depth ?depth))
	(md-inlines (name ?name) (parent ?b) (kind heading))
	(md-text (name ?name))
	;; end of file answers -1 forever, so wait for the close this asks for
	(not (md-closing (name ?name)))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-closing (name ?name) (target ?b) (depth ?depth)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; Two or more trailing spaces make the break a hard one; otherwise the break
;;; is soft, and either way it only becomes an inline if the paragraph is
;;; picked up again by the next line.
(defrule md-end-line-hard-break
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	(not (md-inlines (name ?name) (kind heading)))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 10|-1)
	(test (>= (md-trailing-spaces ?value) 2))
	(test (neq "" (md-trim-right ?value)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?t ?ch)
	(modify ?p (items ?items (make-instance of MD-TEXT (id ?name) (value (md-trim-right ?value)))))
	(assert (md-pending-break (name ?name) (parent ?p) (hard TRUE)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-line-soft-break
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	(not (md-inlines (name ?name) (kind heading)))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 10|-1)
	(test (< (md-trailing-spaces ?value) 2))
	(test (neq "" (md-trim-right ?value)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?t ?ch)
	(modify ?p (items ?items (make-instance of MD-TEXT (id ?name) (value (md-trim-right ?value)))))
	(assert (md-pending-break (name ?name) (parent ?p) (hard FALSE)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-line-without-text
	?p <- (md-inlines (name ?name))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	(not (md-inlines (name ?name) (kind heading)))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 10|-1)
	(test (eq "" (md-trim-right ?value)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?t ?ch)
	(assert (md-pending-break (name ?name) (parent ?p) (hard FALSE)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; ------------------------------------------------------------------
;;; fenced code blocks
;;; ------------------------------------------------------------------
(defrule md-build-code-info
	?b <- (md-code-block (name ?name) (phase info) (info ?info))
	?ch <- (md-char ?name ?char&~10&~-1&~13)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?b (info (format nil "%s%c" ?info ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-code-info
	?b <- (md-code-block (name ?name) (phase info) (info ?info))
	?ch <- (md-char ?name 10|-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?b (phase body) (info (md-trim ?info)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-code-block-line-start
	?c <- (md-blocks (name ?name))
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	?b <- (md-code-block (name ?name) (parent ?c) (fenced TRUE) (phase body))
	=>
	(retract ?ls)
	(assert (md-code-line (name ?name) (block ?b))))

;;; A blank line carries no marker to walk down, so the line-start never
;;; reaches the container the fence sits in; the line is code all the same.
(defrule md-code-block-blank-line
	?ls <- (md-line-start (name ?name) (container ?c) (fresh FALSE))
	?b <- (md-code-block (name ?name) (parent ~?c) (fenced TRUE) (phase body))
	(md-char ?name 10|-1)
	=>
	(retract ?ls)
	(assert (md-code-line (name ?name) (block ?b))))

(defrule md-build-code-line
	?b <- (md-code-block (name ?name))
	?l <- (md-code-line (name ?name) (block ?b) (value ?value))
	?ch <- (md-char ?name ?char&~10&~-1&~13)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?l (value (format nil "%s%c" ?value ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-code-line
	?b <- (md-code-block (name ?name) (marker ?marker) (length ?length) (text ?text) (blanks ?blanks))
	?l <- (md-code-line (name ?name) (block ?b) (value ?value))
	?ch <- (md-char ?name 10)
	(test (not (md-closing-fence ?value ?marker ?length)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?l ?ch)
	(modify ?b (text (format nil "%s%s%s%c" ?text (md-repeat (format nil "%c" 10) ?blanks) ?value 10))
		(blanks 0))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-code-line-at-end-of-file
	?b <- (md-code-block (name ?name) (marker ?marker) (length ?length) (text ?text) (blanks ?blanks))
	?l <- (md-code-line (name ?name) (block ?b) (value ?value&~""))
	?ch <- (md-char ?name -1)
	(test (not (md-closing-fence ?value ?marker ?length)))
	=>
	(retract ?l)
	(modify ?b (text (format nil "%s%s%s%c" ?text (md-repeat (format nil "%c" 10) ?blanks) ?value 10))
		(blanks 0))
	(assert (md-resume (name ?name))))

(defrule md-discard-empty-code-line-at-end-of-file
	?b <- (md-code-block (name ?name))
	?l <- (md-code-line (name ?name) (block ?b) (value ""))
	(md-char ?name -1)
	=>
	(retract ?l)
	(assert (md-resume (name ?name))))

(defrule md-end-code-block
	?c <- (md-blocks (name ?name) (depth ?depth))
	?b <- (md-code-block (name ?name) (parent ?c) (fenced TRUE) (marker ?marker) (length ?length))
	?l <- (md-code-line (name ?name) (block ?b) (value ?value))
	?ch <- (md-char ?name 10|-1)
	(test (md-closing-fence ?value ?marker ?length))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?l ?ch)
	(assert (md-closing (name ?name) (target ?c) (depth ?depth)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; ------------------------------------------------------------------
;;; inline text
;;;
;;; A `text` fact is open for as long as a leaf block is, and its parent is
;;; always the innermost open inline container.  The delimiter, backtick and
;;; angle bracket runs below pause it rather than end it, which is why the
;;; rules that grow the run of literal text stand aside while one is pending.
;;; ------------------------------------------------------------------
(defrule md-build-text
	?t <- (md-text (name ?name) (value ?value))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	(not (md-closing (name ?name)))
	;; a `#` inside a heading belongs to text-hashes
	(not (and (md-inlines (name ?name) (kind heading)) (md-char ?name 35)))
	?ch <- (md-char ?name ?char)
	(test (not (member$ ?char ?*md-inline-marker*)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?t (value (format nil "%s%c" ?value ?char)) (last ?char))
	(assert (md-char ?name (md-read ?stream))))

;;; ------------------------------------------------------------------
;;; `#` inside a heading --- the optional closing sequence when nothing but
;;; blanks follow it and a blank, or the start of the heading, comes before
;;; it; otherwise text
;;; ------------------------------------------------------------------
(defrule md-text-hashes
	(md-text (name ?name))
	(md-inlines (name ?name) (kind heading))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	(not (md-closing (name ?name)))
	?ch <- (md-char ?name 35)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-hashes (name ?name) (hashes "#")))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-hashes-continue
	?h <- (md-hashes (name ?name) (hashes ?hashes) (blanks ""))
	?ch <- (md-char ?name 35)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?h (hashes (str-cat ?hashes "#")))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-hashes-blank
	?h <- (md-hashes (name ?name) (blanks ?blanks))
	?ch <- (md-char ?name ?char&32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?h (blanks (format nil "%s%c" ?blanks ?char)))
	(assert (md-char ?name (md-read ?stream))))

;;; A `#` after the blanks starts a run of its own; the one before was text.
(defrule md-hashes-again
	?t <- (md-text (name ?name) (value ?value))
	?h <- (md-hashes (name ?name) (hashes ?hashes) (blanks ?blanks&~""))
	?ch <- (md-char ?name 35)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?t (value (str-cat ?value ?hashes ?blanks)) (last 32))
	(modify ?h (hashes "#") (blanks ""))
	(assert (md-char ?name (md-read ?stream))))

;;; The run ends the line with a blank, or nothing at all, before it: it is
;;; the closing sequence and goes.  The blank it followed is trimmed with
;;; the text.
(defrule md-hashes-close-heading
	(md-text (name ?name) (last 32|9|0))
	?h <- (md-hashes (name ?name))
	(md-char ?name 10|-1)
	=>
	(retract ?h))

(defrule md-hashes-are-text-at-line-end
	?t <- (md-text (name ?name) (value ?value) (last ~32&~9&~0))
	?h <- (md-hashes (name ?name) (hashes ?hashes))
	(md-char ?name 10|-1)
	=>
	(retract ?h)
	(modify ?t (value (str-cat ?value ?hashes)) (last 35)))

(defrule md-hashes-are-text
	?t <- (md-text (name ?name) (value ?value))
	?h <- (md-hashes (name ?name) (hashes ?hashes) (blanks ?blanks))
	(md-char ?name ~35&~32&~9&~10&~-1)
	=>
	(retract ?h)
	(modify ?t (value (str-cat ?value ?hashes ?blanks)) (last 35)))

;;; A backslash takes the next character with it, so no other rule ever sees it.
(defrule md-begin-escape
	(md-text (name ?name))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 92)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-escape (name ?name) (char (md-read ?stream)))))

(defrule md-escaped-punctuation
	?t <- (md-text (name ?name) (value ?value))
	?e <- (md-escape (name ?name) (char ?char&:(member$ ?char ?*md-punctuation*)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?e)
	(modify ?t (value (format nil "%s%c" ?value ?char)) (last ?char))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-escaped-non-punctuation
	?t <- (md-text (name ?name) (value ?value))
	?e <- (md-escape (name ?name) (char ?char))
	(test (not (member$ ?char ?*md-punctuation*)))
	=>
	(retract ?e)
	(modify ?t (value (format nil "%s%c" ?value 92)) (last 92))
	(assert (md-char ?name ?char)))

(defrule md-unmatched-close-bracket
	?p <- (md-inlines (name ?name) (kind ?kind&~link&~image))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 93) ; ]
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?t (value (format nil "%s%c" ?value 93)) (last 93))
	(assert (md-char ?name (md-read ?stream))))

;;; ------------------------------------------------------------------
;;; emphasis and strong emphasis
;;; ------------------------------------------------------------------
(defrule md-text-delimiter
	?t <- (md-text (name ?name) (last ?last))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name ?marker&42|95)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-delimiter (name ?name) (marker ?marker) (count 1) (before ?last)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-open-pending-delimiter
	?t <- (md-text (name ?name))
	?p <- (md-pending-delimiter (name ?name) (marker ?marker) (count ?count))
	(not (md-literal (name ?name)))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-delimiter (name ?name) (marker ?marker) (count ?count))))

(defrule md-delimiter-continue
	?d <- (md-delimiter (name ?name) (marker ?marker) (count ?count))
	?ch <- (md-char ?name ?marker)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (count (+ ?count 1)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-delimiter-consumed
	?d <- (md-delimiter (name ?name) (count 0))
	=>
	(retract ?d))

(defrule md-delimiter-closes-strong
	?g <- (md-inlines (name ?name) (items $?outer))
	?p <- (md-inlines (name ?name) (parent ?g) (kind strong) (marker ?marker) (items $?inner))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?d <- (md-delimiter (name ?name) (marker ?marker) (count ?count&:(>= ?count 2)) (opened FALSE))
	(md-char ?name ?ch)
	(test (md-can-close ?marker ?ch))
	;; a run that is still growing belongs to delimiter-continue
	(test (neq ?ch ?marker))
	=>
	(retract ?t ?p)
	(modify ?d (count (- ?count 2)))
	(modify ?g (items ?outer (make-instance of MD-STRONG (id ?name)
		(inlines (md-without-empty-text (md-append-text ?name ?inner ?value))))))
	(assert (md-text (name ?name) (parent ?g))))

(defrule md-delimiter-closes-emphasis
	?g <- (md-inlines (name ?name) (items $?outer))
	?p <- (md-inlines (name ?name) (parent ?g) (kind emphasis) (marker ?marker) (items $?inner))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?d <- (md-delimiter (name ?name) (marker ?marker) (count ?count&:(>= ?count 1))
		(before ?before) (opened FALSE))
	(md-char ?name ?ch)
	(test (md-can-close ?marker ?ch))
	;; a long run that could open a strong here does that instead, so that the
	;; `**` of `*a ***b*** c*` opens rather than ends the emphasis
	(test (not (md-delimiter-opens ?p ?marker ?count ?ch ?before FALSE)))
	;; a run that is still growing belongs to delimiter-continue
	(test (neq ?ch ?marker))
	=>
	(retract ?t ?p)
	(modify ?d (count (- ?count 1)))
	(modify ?g (items ?outer (make-instance of MD-EMPHASIS (id ?name)
		(inlines (md-without-empty-text (md-append-text ?name ?inner ?value))))))
	(assert (md-text (name ?name) (parent ?g))))

(defrule md-delimiter-opens-strong
	?p <- (md-inlines (name ?name) (depth ?depth) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?d <- (md-delimiter (name ?name) (marker ?marker) (count ?count&:(>= ?count 2))
		(before ?before) (opened ?opened))
	(md-char ?name ?ch&~32&~9&~10&~13&~-1)
	;; a fresh run inside a strong of this marker closes it instead
	(test (or ?opened (not (md-is-kind ?p strong ?marker))))
	(test (md-can-open ?marker ?before ?ch))
	;; a run that is still growing belongs to delimiter-continue
	(test (neq ?ch ?marker))
	=>
	(retract ?t)
	(modify ?d (count (- ?count 2)) (opened TRUE))
	(modify ?p (items (md-append-text ?name ?items ?value)))
	(assert (md-text (name ?name) (parent
		(assert (md-inlines (name ?name) (parent ?p) (depth (+ ?depth 1))
			(kind strong) (marker ?marker)))))))

(defrule md-delimiter-opens-emphasis
	?p <- (md-inlines (name ?name) (depth ?depth) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?d <- (md-delimiter (name ?name) (marker ?marker) (count 1) (before ?before)
		(opened ?opened))
	(md-char ?name ?ch&~32&~9&~10&~13&~-1)
	;; a fresh run inside an emphasis of this marker closes it instead
	(test (or ?opened (not (md-is-kind ?p emphasis ?marker))))
	(test (md-can-open ?marker ?before ?ch))
	;; a run that is still growing belongs to delimiter-continue
	(test (neq ?ch ?marker))
	=>
	(retract ?t)
	(modify ?d (count 0) (opened TRUE))
	(modify ?p (items (md-append-text ?name ?items ?value)))
	(assert (md-text (name ?name) (parent
		(assert (md-inlines (name ?name) (parent ?p) (depth (+ ?depth 1))
			(kind emphasis) (marker ?marker)))))))

(defrule md-delimiter-literal
	?p <- (md-inlines (name ?name))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?d <- (md-delimiter (name ?name) (marker ?marker) (count ?count&~0) (before ?before)
		(opened ?opened))
	(md-char ?name ?ch)
	(test (md-delimiter-literal ?p ?marker ?count ?ch ?before ?opened))
	=>
	(retract ?d)
	(modify ?t (value (str-cat ?value (md-repeat (format nil "%c" ?marker) ?count))) (last ?marker)))

;;; ------------------------------------------------------------------
;;; code spans
;;; ------------------------------------------------------------------
(defrule md-text-ticks
	?t <- (md-text (name ?name))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 96)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-ticks (name ?name) (count 1)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-open-pending-ticks
	?t <- (md-text (name ?name))
	?p <- (md-pending-ticks (name ?name) (count ?count))
	(not (md-literal (name ?name)))
	(not (md-closing (name ?name)))
	=>
	(retract ?p)
	(assert (md-ticks (name ?name) (count ?count))))

(defrule md-ticks-continue
	?tk <- (md-ticks (name ?name) (count ?count))
	?ch <- (md-char ?name 96)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?tk (count (+ ?count 1)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-ticks-open-code-span
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?tk <- (md-ticks (name ?name) (count ?count))
	(md-char ?name ?ch&~96)
	=>
	(retract ?tk ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)))
	(assert (md-code-span (name ?name) (parent ?p) (length ?count))))

(defrule md-build-code-span
	?s <- (md-code-span (name ?name) (value ?value) (pending 0))
	?ch <- (md-char ?name ?char&~96&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?s (value (format nil "%s%c" ?value ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-code-span-ticks
	?s <- (md-code-span (name ?name) (pending ?pending))
	?ch <- (md-char ?name 96)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?s (pending (+ ?pending 1)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-close-code-span
	?p <- (md-inlines (name ?name) (items $?items))
	?s <- (md-code-span (name ?name) (parent ?p) (length ?count) (value ?value) (pending ?count))
	(md-char ?name ?ch&~96)
	=>
	(retract ?s)
	(modify ?p (items ?items
		(make-instance of MD-CODE-SPAN (id ?name) (value (md-trim ?value)) (fence ?count))))
	(assert (md-text (name ?name) (parent ?p))))

;;; A run of the wrong length is content, not the end of the span.
(defrule md-code-span-ticks-are-content
	?s <- (md-code-span (name ?name) (length ?count) (value ?value) (pending ?pending&~0))
	(md-char ?name ?ch&~96)
	(test (neq ?pending ?count))
	=>
	(modify ?s (value (str-cat ?value (md-repeat "`" ?pending))) (pending 0)))

;;; A code span never spans lines; an unterminated one falls back to text.
(defrule md-code-span-unterminated
	?p <- (md-inlines (name ?name))
	?s <- (md-code-span (name ?name) (parent ?p) (length ?count) (value ?value) (pending ?pending))
	(md-char ?name 10|-1)
	;; a run that closes the span belongs to close-code-span
	(test (neq ?pending ?count))
	=>
	(retract ?s)
	(assert (md-text (name ?name) (parent ?p)
		(value (str-cat (md-repeat "`" ?count) ?value (md-repeat "`" ?pending))) (last 96))))

;;; ------------------------------------------------------------------
;;; autolinks
;;; ------------------------------------------------------------------
(defrule md-text-open-angle
	?t <- (md-text (name ?name))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 60)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-angle (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-angle
	?a <- (md-angle (name ?name) (value ?value) (codes $?codes))
	?ch <- (md-char ?name ?char&~62&~60&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?a (value (format nil "%s%c" ?value ?char)) (last ?char) (codes ?codes ?char))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-autolink
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?a <- (md-angle (name ?name) (value ?link) (codes $? 58 47 47 $?)) ; ://
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?a ?ch ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)
		(make-instance of MD-AUTOLINK (id ?name) (destination ?link))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

;;; An address is a name, an `@`, and a host with a dot in it.
(defrule md-end-mail-autolink
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?a <- (md-angle (name ?name) (value ?link) (codes $? 64 $? 46 $?)) ; @ then .
	(not (md-angle (name ?name) (codes $? 58 47 47 $?)))
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?a ?ch ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)
		(make-instance of MD-AUTOLINK (id ?name) (destination ?link))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

;;; A tag is a name, a declaration or an instruction directly after the bracket.
(defrule md-end-inline-html
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?a <- (md-angle (name ?name) (value ?tag)
		(codes ?first&:(member$ ?first ?*md-tag-opener*) $?))
	(not (md-angle (name ?name) (codes $? 58 47 47 $?)))
	(not (md-angle (name ?name) (codes $? 64 $? 46 $?)))
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?a ?ch ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)
		(make-instance of MD-HTML-INLINE (id ?name) (value (format nil "%c%s%c" 60 ?tag 62)))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

;;; A closing tag puts its slash before the name.
(defrule md-end-inline-closing-html
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?a <- (md-angle (name ?name) (value ?tag)
		(codes 47 ?second&:(member$ ?second ?*md-alpha*) $?))
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?a ?ch ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)
		(make-instance of MD-HTML-INLINE (id ?name) (value (format nil "%c%s%c" 60 ?tag 62)))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

;;; A `<`, or the end of the line, closes nothing at all.
(defrule md-angle-is-text
	?t <- (md-text (name ?name) (value ?value))
	?a <- (md-angle (name ?name) (value ?link) (last ?last))
	(md-char ?name 60|10|-1)
	=>
	(retract ?a)
	(modify ?t (value (format nil "%s%c%s" ?value 60 ?link)) (last ?last)))

;;; A `>` closes it, but what it held was none of the shapes above.
(defrule md-angle-is-text-at-bracket
	?t <- (md-text (name ?name) (value ?value))
	?a <- (md-angle (name ?name) (value ?link) (last ?last))
	(not (md-angle (name ?name) (codes ?first&:(member$ ?first ?*md-tag-opener*) $?)))
	(not (md-angle (name ?name) (codes 47 ?second&:(member$ ?second ?*md-alpha*) $?)))
	(not (md-angle (name ?name) (codes $? 58 47 47 $?)))
	(not (md-angle (name ?name) (codes $? 64 $? 46 $?)))
	(md-char ?name 62)
	=>
	(retract ?a)
	(modify ?t (value (format nil "%s%c%s" ?value 60 ?link)) (last ?last)))

	;;; ------------------------------------------------------------------
;;; link reference definitions
;;;
;;; `[label]: target "title"` opening a block is a definition rather than a
;;; link, which is settled by the `:` arriving while the link is still the only
;;; thing its paragraph holds.
;;; ------------------------------------------------------------------
(defrule md-open-link-definition
	?c <- (md-blocks (name ?name))
	?p <- (md-inlines (name ?name) (parent ?c) (kind paragraph) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind link) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase awaiting))
	?ch <- (md-char ?name 58) ; :
	(test (md-all-blank ?outer))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?l ?p ?ch)
	(foreach ?blank ?outer (unmake-instance ?blank))
	(assert (md-definition (name ?name) (container ?c) (label (md-describe ?inner))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-definition-skip-space
	(md-definition (name ?name) (phase pre-destination|pre-title))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-char ?name (md-read ?stream))))

(defrule md-open-definition-angle-destination
	?d <- (md-definition (name ?name) (phase pre-destination))
	?ch <- (md-char ?name 60) ; <
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase angle-destination))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-definition-angle-destination
	?d <- (md-definition (name ?name) (phase angle-destination) (destination ?destination))
	?ch <- (md-char ?name ?char&~62&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (destination (format nil "%s%c" ?destination ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-close-definition-angle-destination
	?d <- (md-definition (name ?name) (phase angle-destination))
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase pre-title))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-definition-destination
	?d <- (md-definition (name ?name) (phase ?phase&pre-destination|destination) (destination ?destination))
	?ch <- (md-char ?name ?char&~32&~9&~10&~13&~-1)
	;; a `<` opening the destination belongs to open-definition-angle-destination
	(test (or (neq ?char 60) (eq ?phase destination)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase destination) (destination (format nil "%s%c" ?destination ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-definition-destination
	?d <- (md-definition (name ?name) (phase destination))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase pre-title))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-open-definition-title
	?d <- (md-definition (name ?name) (phase pre-title|title-next-line))
	?ch <- (md-char ?name ?quote&34|39|40)
	(md-title-quote ?quote ?closer)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase title) (title "") (closer ?closer))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-definition-title
	?d <- (md-definition (name ?name) (phase title) (title ?title) (closer ?closer))
	?ch <- (md-char ?name ?char&~92&~10&~13&~-1)
	(test (neq ?char ?closer))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (title (format nil "%s%c" ?title ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-close-definition-title
	?d <- (md-definition (name ?name) (phase title) (closer ?closer))
	?ch <- (md-char ?name ?closer)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase done))
	(assert (md-char ?name (md-read ?stream))))

;;; A backslash in a title escapes the punctuation after it, its own closing
;;; quote included, and is literal before anything else.
(defrule md-escape-in-definition-title
	?d <- (md-definition (name ?name) (phase title))
	?ch <- (md-char ?name 92)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase title-escape))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-escaped-punctuation-in-definition-title
	?d <- (md-definition (name ?name) (phase title-escape) (title ?title))
	?ch <- (md-char ?name ?char&:(member$ ?char ?*md-punctuation*))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase title) (title (format nil "%s%c" ?title ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-literal-backslash-in-definition-title
	?d <- (md-definition (name ?name) (phase title-escape) (title ?title))
	(md-char ?name ?char&~10&~13&~-1)
	(test (not (member$ ?char ?*md-punctuation*)))
	=>
	(modify ?d (phase title) (title (format nil "%s%c" ?title 92))))

;;; Anything after the target that is not a title, or after the title, is
;;; dropped, so a line that is nearly a definition still ends.
(defrule md-definition-junk-after-destination
	?d <- (md-definition (name ?name) (phase pre-title))
	(md-char ?name ?ch&~32&~9&~10&~-1&~34&~39&~40)
	=>
	(modify ?d (phase done)))

(defrule md-definition-skip-junk
	(md-definition (name ?name) (phase done))
	?ch <- (md-char ?name ?char&~10&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-char ?name (md-read ?stream))))

;;; The title may sit on the line below the target, so the end of the first
;;; line does not finish the definition on its own.
(defrule md-definition-line-ends-before-title
	?d <- (md-definition (name ?name) (phase pre-title|destination))
	?ch <- (md-char ?name 10)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase title-next-line))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-definition-next-line-skip-space
	(md-definition (name ?name) (phase title-next-line))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-definition-at-line-end
	?q <- (md-sequence (name ?name) (next ?n))
	?c <- (md-blocks (name ?name) (items $?items))
	?d <- (md-definition (name ?name) (container ?c)
		(phase pre-destination|angle-destination|title|title-escape|done)
		(label ?label) (destination ?destination) (title ?title))
	?ch <- (md-char ?name 10)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?ch)
	(modify ?c (items ?items (make-instance of MD-LINK-DEFINITION (id ?name)
		(label ?label) (destination ?destination) (title ?title))))
	(modify ?q (next (+ ?n 1)))
	(assert (md-link-definition (name ?name) (label (md-normalize-label ?label))
		(destination ?destination) (title ?title) (sequence ?n)))
	(assert (md-resume (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

;;; Nothing on the following line opened a title, so that line is left for the
;;; ordinary rules to read.
(defrule md-end-definition-without-next-line-title
	?q <- (md-sequence (name ?name) (next ?n))
	?c <- (md-blocks (name ?name) (items $?items))
	?d <- (md-definition (name ?name) (container ?c) (phase title-next-line)
		(label ?label) (destination ?destination) (title ?title))
	(md-char ?name ?ch&~32&~9&~34&~39&~40&~-1)
	=>
	(retract ?d)
	(modify ?c (items ?items (make-instance of MD-LINK-DEFINITION (id ?name)
		(label ?label) (destination ?destination) (title ?title))))
	(modify ?q (next (+ ?n 1)))
	(assert (md-link-definition (name ?name) (label (md-normalize-label ?label))
		(destination ?destination) (title ?title) (sequence ?n)))
	(assert (md-resume (name ?name))))

(defrule md-end-definition-at-end-of-file
	?q <- (md-sequence (name ?name) (next ?n))
	?c <- (md-blocks (name ?name) (items $?items))
	?d <- (md-definition (name ?name) (container ?c)
		(label ?label) (destination ?destination) (title ?title))
	(md-char ?name -1)
	=>
	(retract ?d)
	(modify ?c (items ?items (make-instance of MD-LINK-DEFINITION (id ?name)
		(label ?label) (destination ?destination) (title ?title))))
	(modify ?q (next (+ ?n 1)))
	(assert (md-link-definition (name ?name) (label (md-normalize-label ?label))
		(destination ?destination) (title ?title) (sequence ?n)))
	(assert (md-resume (name ?name))))

;;; A definition may come after the references to it, so references are
;;; resolved once the whole document has been read, oldest first, and the
;;; first definition of a label is the one that counts.
(defrule md-resolve-reference
	(md-at-eof (name ?name))
	(not (md-closing (name ?name)))
	?u <- (md-unresolved (name ?name) (link ?link) (label ?label) (sequence ?s))
	(not (md-unresolved (name ?name) (sequence ?earlier&:(< ?earlier ?s))))
	(md-link-definition (name ?name) (label ?label) (destination ?destination) (title ?title) (sequence ?d))
	(not (md-link-definition (name ?name) (label ?label) (sequence ?first&:(< ?first ?d))))
	=>
	(retract ?u)
	(send ?link put-destination ?destination)
	(send ?link put-title ?title))

;;; A reference nothing defines keeps its label and an empty target.
(defrule md-discard-unresolved-reference
	(md-at-eof (name ?name))
	(not (md-closing (name ?name)))
	?u <- (md-unresolved (name ?name) (label ?label) (sequence ?s))
	(not (md-unresolved (name ?name) (sequence ?earlier&:(< ?earlier ?s))))
	(not (md-link-definition (name ?name) (label ?label)))
	=>
	(retract ?u))

;;; ------------------------------------------------------------------
;;; entity references --- kept verbatim, since the output is Markdown
;;; ------------------------------------------------------------------
(defrule md-text-ampersand
	(md-text (name ?name))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 38) ; &
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-entity (name ?name)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-entity
	?e <- (md-entity (name ?name) (value ?value))
	?ch <- (md-char ?name ?char&:(or (member$ ?char ?*md-alphanum*) (= ?char 35)))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?e (value (format nil "%s%c" ?value ?char)) (last ?char))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-entity
	?p <- (md-inlines (name ?name) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?e <- (md-entity (name ?name) (value ?entity&~""))
	?ch <- (md-char ?name 59) ; ;
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?e ?ch ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)
		(make-instance of MD-ENTITY (id ?name) (value (format nil "&%s;" ?entity)))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-entity-is-text
	?t <- (md-text (name ?name) (value ?value))
	?e <- (md-entity (name ?name) (value ?entity) (last ?last))
	(md-char ?name ?ch)
	(test (not (or (member$ ?ch ?*md-alphanum*) (= ?ch 35)
		       (and (= ?ch 59) (neq ?entity "")))))
	=>
	(retract ?e)
	(modify ?t (value (format nil "%s%c%s" ?value 38 ?entity)) (last ?last)))

;;; ------------------------------------------------------------------
;;; links and images
;;; ------------------------------------------------------------------
(defrule md-text-open-bracket
	?p <- (md-inlines (name ?name) (depth ?depth) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 91) ; [
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)))
	(assert (md-text (name ?name) (parent
		(assert (md-inlines (name ?name) (parent ?p) (depth (+ ?depth 1)) (kind link) (prefix "["))))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-text-bang
	(md-text (name ?name))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 33) ; !
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-bang (name ?name) (char (md-read ?stream)))))

(defrule md-bang-opens-image
	?p <- (md-inlines (name ?name) (depth ?depth) (items $?items))
	?t <- (md-text (name ?name) (parent ?p) (value ?value))
	?b <- (md-bang (name ?name) (char 91)) ; [
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?b ?t)
	(modify ?p (items (md-append-text ?name ?items ?value)))
	(assert (md-text (name ?name) (parent
		(assert (md-inlines (name ?name) (parent ?p) (depth (+ ?depth 1)) (kind image) (prefix "!["))))))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-bang-is-text
	?t <- (md-text (name ?name) (value ?value))
	?b <- (md-bang (name ?name) (char ?char))
	(test (neq ?char 91))
	=>
	(retract ?b)
	(modify ?t (value (format nil "%s%c" ?value 33)) (last 33))
	(assert (md-char ?name ?char)))

(defrule md-end-link-text
	?l <- (md-inlines (name ?name) (kind link|image) (items $?items))
	?t <- (md-text (name ?name) (parent ?l) (value ?value))
	;; an inline run still owed to this line has to land first
	(not (md-literal (name ?name)))
	(not (md-hashes (name ?name)))
	(not (md-delimiter (name ?name)))
	(not (md-ticks (name ?name)))
	(not (md-angle (name ?name)))
	(not (md-entity (name ?name)))
	;; an inline run queued by the line-start rules opens before text resumes
	(not (md-pending-delimiter (name ?name)))
	(not (md-pending-ticks (name ?name)))
	(not (md-pending-angle (name ?name)))
	?ch <- (md-char ?name 93) ; ]
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch ?t)
	(modify ?l (items (md-append-text ?name ?items ?value)))
	(assert (md-destination (name ?name) (link ?l) (phase awaiting)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-open-link-destination
	?d <- (md-destination (name ?name) (phase awaiting))
	?ch <- (md-char ?name 40) ; (
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase destination))
	(assert (md-char ?name (md-read ?stream))))

;;; Only the inline `[text](target)` form is a link; anything else keeps the
;;; brackets and the content it had collected.
(defrule md-open-link-label
	?d <- (md-destination (name ?name) (phase awaiting))
	?ch <- (md-char ?name 91) ; [
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase label) (label ""))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-link-label
	?d <- (md-destination (name ?name) (phase label) (label ?label))
	?ch <- (md-char ?name ?char&~93&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (label (format nil "%s%c" ?label ?char)))
	(assert (md-char ?name (md-read ?stream))))

;;; Appends a finished reference to its container and leaves its label for
;;; resolve-reference once the document has been read.
(deffunction md-end-reference (?name ?stream ?p ?outer ?q ?n ?reference ?label)
	(modify ?p (items ?outer ?reference))
	(modify ?q (next (+ ?n 1)))
	(assert (md-unresolved (name ?name) (link ?reference) (sequence ?n)
		(label (md-normalize-label ?label))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-reference-link
	?q <- (md-sequence (name ?name) (next ?n))
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind link) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase label) (label ?label&~""))
	?ch <- (md-char ?name 93) ; ]
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?l ?ch)
	(md-end-reference ?name ?stream ?p ?outer ?q ?n
		(make-instance of MD-LINK (id ?name) (inlines (md-without-empty-text ?inner))
			(destination "") (label ?label))
		?label))

;;; An empty label means the link text is the label.
(defrule md-end-collapsed-reference-link
	?q <- (md-sequence (name ?name) (next ?n))
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind link) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase label) (label ""))
	?ch <- (md-char ?name 93) ; ]
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?l ?ch)
	(md-end-reference ?name ?stream ?p ?outer ?q ?n
		(make-instance of MD-LINK (id ?name) (inlines (md-without-empty-text ?inner))
			(destination "") (label ""))
		(md-describe ?inner)))

(defrule md-end-reference-image
	?q <- (md-sequence (name ?name) (next ?n))
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind image) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase label) (label ?label&~""))
	?ch <- (md-char ?name 93) ; ]
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?l ?ch)
	(md-end-reference ?name ?stream ?p ?outer ?q ?n
		(make-instance of MD-IMAGE (id ?name) (description (md-describe ?inner))
			(destination "") (label ?label))
		?label))

(defrule md-end-collapsed-reference-image
	?q <- (md-sequence (name ?name) (next ?n))
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind image) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase label) (label ""))
	?ch <- (md-char ?name 93) ; ]
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?l ?ch)
	(md-end-reference ?name ?stream ?p ?outer ?q ?n
		(make-instance of MD-IMAGE (id ?name) (description (md-describe ?inner))
			(destination "") (label ""))
		(md-describe ?inner)))

(defrule md-end-line-in-link-label
	?l <- (md-inlines (name ?name) (kind link|image))
	?d <- (md-destination (name ?name) (link ?l) (phase label) (label ?label))
	(md-char ?name 10|-1)
	=>
	(retract ?d)
	(assert (md-text (name ?name) (parent ?l) (value (str-cat "][" ?label)))))

(defrule md-link-without-destination
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind link|image) (prefix ?prefix) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase awaiting))
	(md-char ?name ?ch&~40&~91)
	;; `[label]:` opening a block is a definition, not a link
	(test (not (and (= ?ch 58) (md-all-blank ?outer)
			(eq paragraph (fact-slot-value ?p kind)))))
	=>
	(retract ?d ?l)
	(modify ?p (items ?outer
		(make-instance of MD-TEXT (id ?name) (value ?prefix))
		?inner
		(make-instance of MD-TEXT (id ?name) (value "]"))))
	(assert (md-text (name ?name) (parent ?p))))

;;; A destination that runs off the end of the line was never a link.
(defrule md-end-line-in-link-destination
	?l <- (md-inlines (name ?name) (kind link|image))
	?d <- (md-destination (name ?name) (link ?l) (phase ?phase&~awaiting&~label) (value ?value))
	(md-char ?name 10|-1)
	=>
	(retract ?d)
	(assert (md-text (name ?name) (parent ?l) (value (str-cat "](" ?value)))))

(defrule md-open-angle-link-destination
	?d <- (md-destination (name ?name) (phase destination) (value ""))
	?ch <- (md-char ?name 60) ; <
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase angle-destination))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-angle-link-destination
	?d <- (md-destination (name ?name) (phase angle-destination) (value ?value))
	?ch <- (md-char ?name ?char&~62&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (value (format nil "%s%c" ?value ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-close-angle-link-destination
	?d <- (md-destination (name ?name) (phase angle-destination))
	?ch <- (md-char ?name 62) ; >
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase pre-title))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-link-destination
	?d <- (md-destination (name ?name) (phase destination) (value ?value))
	?ch <- (md-char ?name ?char&~41&~32&~9&~10&~13&~-1)
	;; a `<` opening the destination belongs to open-angle-link-destination
	(test (or (neq ?char 60) (neq ?value "")))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (value (format nil "%s%c" ?value ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-start-link-title
	?d <- (md-destination (name ?name) (phase destination))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase pre-title))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-skip-link-title-space
	?d <- (md-destination (name ?name) (phase pre-title|closed))
	?ch <- (md-char ?name 32|9)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-char ?name (md-read ?stream))))

;;; The space did not introduce a title after all, so it belongs to the
;;; destination and collecting it resumes.
(defrule md-link-destination-continues-after-space
	?d <- (md-destination (name ?name) (phase pre-title) (value ?value))
	?ch <- (md-char ?name ?char&~34&~32&~9&~41&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase destination) (value (format nil "%s %c" ?value ?char)))
	(assert (md-char ?name (md-read ?stream))))

;;; Whatever sits between a closed title and the closing paren is dropped.
(defrule md-skip-after-link-title
	(md-destination (name ?name) (phase closed))
	?ch <- (md-char ?name ?char&~41&~32&~9&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(assert (md-char ?name (md-read ?stream))))

(defrule md-open-link-title
	?d <- (md-destination (name ?name) (phase pre-title))
	?ch <- (md-char ?name 34) ; "
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase title) (title ""))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-build-link-title
	?d <- (md-destination (name ?name) (phase title) (title ?title))
	?ch <- (md-char ?name ?char&~34&~92&~10&~13&~-1)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (title (format nil "%s%c" ?title ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-close-link-title
	?d <- (md-destination (name ?name) (phase title))
	?ch <- (md-char ?name 34)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase closed))
	(assert (md-char ?name (md-read ?stream))))

;;; A backslash in a title escapes the punctuation after it, its own closing
;;; quote included, and is literal before anything else.
(defrule md-escape-in-link-title
	?d <- (md-destination (name ?name) (phase title))
	?ch <- (md-char ?name 92)
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase title-escape))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-escaped-punctuation-in-link-title
	?d <- (md-destination (name ?name) (phase title-escape) (title ?title))
	?ch <- (md-char ?name ?char&:(member$ ?char ?*md-punctuation*))
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?ch)
	(modify ?d (phase title) (title (format nil "%s%c" ?title ?char)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-literal-backslash-in-link-title
	?d <- (md-destination (name ?name) (phase title-escape) (title ?title))
	(md-char ?name ?char&~10&~13&~-1)
	(test (not (member$ ?char ?*md-punctuation*)))
	=>
	(modify ?d (phase title) (title (format nil "%s%c" ?title 92))))

(defrule md-end-link
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind link) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase destination|pre-title|closed)
		(value ?value) (title ?title))
	?ch <- (md-char ?name 41) ; )
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?l ?ch)
	(modify ?p (items ?outer (make-instance of MD-LINK (id ?name)
		(inlines (md-without-empty-text ?inner)) (destination ?value) (title ?title))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

(defrule md-end-image
	?p <- (md-inlines (name ?name) (items $?outer))
	?l <- (md-inlines (name ?name) (parent ?p) (kind image) (items $?inner))
	?d <- (md-destination (name ?name) (link ?l) (phase destination|pre-title|closed)
		(value ?value) (title ?title))
	?ch <- (md-char ?name 41) ; )
	(md-stream (id ?name) (stream ?stream))
	=>
	(retract ?d ?l ?ch)
	(modify ?p (items ?outer (make-instance of MD-IMAGE (id ?name)
		(description (md-describe ?inner)) (destination ?value) (title ?title))))
	(assert (md-text (name ?name) (parent ?p)))
	(assert (md-char ?name (md-read ?stream))))

;;; ------------------------------------------------------------------
;;; printing --- turns the tree back into Markdown.
;;;
;;; A handler prints its own first line assuming the caller has already laid
;;; down the container prefix, and never ends with a newline; the container
;;; handlers put the separators between their children.  `?*md-prefix*` carries
;;; the `> ` and the list item indent that every later line has to repeat.
;;; ------------------------------------------------------------------
(defmessage-handler MD-TEXT pprint primary (?t)
	(printout ?t (md-escaped ?self:value ?*md-line-start* ?*md-in-link*)))

(defmessage-handler MD-SOFT-BREAK pprint primary (?t)
	(md-newline ?t))

(defmessage-handler MD-HARD-BREAK pprint primary (?t)
	(printout ?t "  ")
	(md-newline ?t))

(defmessage-handler MD-CODE-SPAN pprint primary (?t)
	(bind ?width (md-code-span-fence ?self:value))
	(if (> ?self:fence ?width) then (bind ?width ?self:fence))
	(bind ?fence (md-repeat "`" ?width))
	(bind ?pad (md-code-span-pad ?self:value))
	(printout ?t ?fence ?pad ?self:value ?pad ?fence))

(defmessage-handler MD-EMPHASIS pprint primary (?t)
	(printout ?t "*")
	(md-print-inlines ?t ?self:inlines FALSE)
	(printout ?t "*"))

(defmessage-handler MD-STRONG pprint primary (?t)
	(printout ?t "**")
	(md-print-inlines ?t ?self:inlines FALSE)
	(printout ?t "**"))

(defmessage-handler MD-AUTOLINK pprint primary (?t)
	(printout ?t "<" ?self:destination ">"))

(defmessage-handler MD-LINK pprint primary (?t)
	(printout ?t "[")
	(bind ?*md-in-link* TRUE)
	(md-print-inlines ?t ?self:inlines FALSE)
	(bind ?*md-in-link* FALSE)
	(printout ?t "]")
	(if (neq ?self:label nil)
	    then (printout ?t "[" ?self:label "]")
	    else (printout ?t "(")
		 (md-print-destination ?t ?self:destination)
		 (md-print-title ?t ?self:title)
		 (printout ?t ")")))

(defmessage-handler MD-IMAGE pprint primary (?t)
	(printout ?t "![" (md-escaped ?self:description FALSE TRUE) "]")
	(if (neq ?self:label nil)
	    then (printout ?t "[" ?self:label "]")
	    else (printout ?t "(")
		 (md-print-destination ?t ?self:destination)
		 (md-print-title ?t ?self:title)
		 (printout ?t ")")))

(defmessage-handler MD-LINK-DEFINITION pprint primary (?t)
	(printout ?t "[" ?self:label "]: ")
	(md-print-destination ?t ?self:destination)
	(md-print-title ?t ?self:title))

(defmessage-handler MD-ENTITY pprint primary (?t)
	(printout ?t ?self:value))

(defmessage-handler MD-HTML-INLINE pprint primary (?t)
	(printout ?t ?self:value))

(defmessage-handler MD-HTML-BLOCK pprint primary (?t)
	(bind ?index 0)
	(foreach ?line (md-lines ?self:text)
		(bind ?index (+ ?index 1))
		(if (> ?index 1) then (md-newline ?t))
		(printout ?t ?line)))

(defmessage-handler MD-PARAGRAPH pprint primary (?t)
	(md-print-inlines ?t ?self:inlines TRUE))

(defmessage-handler MD-HEADING pprint primary (?t)
	(if ?self:setext
	    then (md-print-inlines ?t ?self:inlines TRUE)
		 (md-newline ?t)
		 (printout ?t (md-repeat (if (= ?self:level 1) then "=" else "-") 3))
	    else (printout ?t (md-repeat "#" ?self:level))
		 (if (> (length$ ?self:inlines) 0) then (printout ?t " "))
		 (md-print-inlines ?t ?self:inlines FALSE)))

(defmessage-handler MD-THEMATIC-BREAK pprint primary (?t)
	(printout ?t "---"))

(defmessage-handler MD-CODE-BLOCK pprint primary (?t)
	(if ?self:fenced
	    then (bind ?fence (md-repeat "`" (md-fence-length ?self:text)))
		 (printout ?t ?fence ?self:info)
		 (foreach ?line (md-lines ?self:text)
			(md-code-newline ?t ?line)
			(printout ?t ?line))
		 (md-newline ?t)
		 (printout ?t ?fence)
	    else (bind ?index 0)
		 (foreach ?line (md-lines ?self:text)
			(bind ?index (+ ?index 1))
			(if (> ?index 1) then (md-code-newline ?t ?line))
			(if (neq ?line "") then (printout ?t "    " ?line)))))

(defmessage-handler MD-BLOCK-QUOTE pprint primary (?t)
	(bind ?previous ?*md-prefix*)
	(printout ?t "> ")
	(bind ?*md-prefix* (str-cat ?previous "> "))
	(bind ?index 0)
	(bind ?previous-block nil)
	(foreach ?block ?self:blocks
		(bind ?index (+ ?index 1))
		(if (> ?index 1) then (md-separate-blocks ?t ?previous-block ?block))
		(send ?block pprint ?t)
		(bind ?previous-block ?block))
	(bind ?*md-prefix* ?previous))

(defmessage-handler MD-LIST-ITEM pprint primary (?t)
	(bind ?index 0)
	(bind ?previous-block nil)
	(foreach ?block ?self:blocks
		(bind ?index (+ ?index 1))
		(if (> ?index 1)
		    then (if (eq MD-LIST (class ?block))
			     then (md-newline ?t)
			     else (md-separate-blocks ?t ?previous-block ?block)))
		(send ?block pprint ?t)
		(bind ?previous-block ?block)))

(defmessage-handler MD-LIST pprint primary (?t)
	(bind ?previous ?*md-prefix*)
	(bind ?number ?self:start)
	(bind ?loose (or ?self:loose (md-loose-list ?self:items)))
	(bind ?index 0)
	(foreach ?item ?self:items
		(bind ?index (+ ?index 1))
		(if (> ?index 1)
		    then (if ?loose then (md-blank-line ?t) else (md-newline ?t)))
		(if ?self:ordered
		    then (bind ?marker (str-cat ?number ". "))
		    else (bind ?marker "- "))
		(printout ?t ?marker)
		(bind ?*md-prefix* (str-cat ?previous (md-repeat " " (str-length ?marker))))
		(send ?item pprint ?t)
		(bind ?*md-prefix* ?previous)
		(bind ?number (+ ?number 1))))

(defmessage-handler MD-DOCUMENT pprint primary (?t)
	(bind ?*md-prefix* "")
	(bind ?index 0)
	(bind ?previous-block nil)
	(foreach ?block ?self:blocks
		(bind ?index (+ ?index 1))
		(if (> ?index 1) then (md-separate-blocks ?t ?previous-block ?block))
		(send ?block pprint ?t)
		(bind ?previous-block ?block))
	(printout ?t crlf))
