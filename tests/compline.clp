;;; Compares two files line by line and returns a multifield of report lines
;;; describing the differences.  An empty multifield means the files match.
(deffunction compare-files (?base-file ?test-file)
   (open ?base-file base)
   (open ?test-file test)
   (bind ?i 0)
   (bind ?differences (create$))
   (bind ?base-line (readline base))
   (bind ?test-line (readline test))
   (while (and (neq ?base-line EOF) (neq ?test-line EOF)) do
      (bind ?i (+ ?i 1))
      (if (neq ?base-line ?test-line) then
          (bind ?differences (create$ ?differences
             (format nil "   %3d expected: %s" ?i ?base-line)
             (format nil "   %3d actual:   %s" ?i ?test-line))))
      (bind ?base-line (readline base))
      (bind ?test-line (readline test)))
   (if (or (neq ?base-line EOF) (neq ?test-line EOF)) then
      (bind ?differences (create$ ?differences
         "   files do not have the same # of lines:"
         (format nil "   expected last line %s" ?base-line)
         (format nil "   actual last line %s" ?test-line))))
   (close base)
   (close test)
   ?differences)
