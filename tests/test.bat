;;; Runs every case under every conflict resolution strategy CLIPS offers.  A
;;; COOL codec keeps only one rule on the agenda at a time, so it decodes the
;;; same way under each strategy; a case that fails under one strategy and not
;;; another points at a rule that relies on firing order.  All the cases
;;; decode in one run, so a strategy like breadth advances every document a
;;; character at a time in lockstep: a rule whose patterns are not all tied to
;;; one document then matches facts from two documents at once, which decoding
;;; one case at a time cannot show.
;(watch rules)
;(watch facts)
;(watch activations)
;(watch statistics)
(load* compline.clp)
(load* ../program.clp)

(defglobal ?*strategies* = (create$ depth breadth lex mea complexity simplicity random))

;;; the strategy under test, named in each failure.  It survives each reset.
(set-reset-globals FALSE)
(defglobal ?*strategy* = depth)

;;; the MD-DOCUMENT instance the codec made for a case file, or nil
(deffunction case-document (?file)
	(bind ?document nil)
	(do-for-instance ((?d MD-DOCUMENT)) (eq ?d:id ?file)
		(bind ?document ?d))
	?document)

;;; Compares what the codec parsed from one case with the file of the same
;;; name in expected.  Prints the failure, if any, and returns TRUE when the
;;; case passed.
(deffunction check-case (?file)
	(bind ?document (case-document ?file))
	(if (eq ?document nil) then
		(println "FAIL " ?file " (" ?*strategy* "): did not find an MD-DOCUMENT instance")
		(return FALSE))
	(open actual actual "w")
	(send ?document pprint actual)
	(close actual)
	(bind ?differences (compare-files (sym-cat expected/ ?file) actual))
	(if (> (length$ ?differences) 0) then
		(println "FAIL " ?file " (" ?*strategy* ")")
		(foreach ?line ?differences (println ?line))
		(return FALSE))
	TRUE)

;;; Parses every case at once under one strategy, then checks each case.
;;; Returns the number of cases that failed.
(deffunction run-strategy (?strategy ?files)
	(bind ?*strategy* ?strategy)
	(set-strategy ?strategy)
	(reset)
	(foreach ?file ?files
		(open (sym-cat cases/ ?file) ?file)
		(assert (md-parse (stream ?file) (id ?file))))
	(run)
	(bind ?failed 0)
	(foreach ?file ?files
		(if (not (check-case ?file)) then
			(bind ?failed (+ ?failed 1))))
	(foreach ?file ?files (close ?file))
	?failed)

;;; Runs every strategy, reporting only the failures plus a one line summary.
;;; Exits with 1 on a failure, so make test and CI fail.
(deffunction run-tests (?files)
	(bind ?failed 0)
	(foreach ?strategy ?*strategies*
		(bind ?failed (+ ?failed (run-strategy ?strategy ?files))))
	(bind ?strategies (length$ ?*strategies*))
	(bind ?total (* (length$ ?files) ?strategies))
	(println (length$ ?files) " cases x " ?strategies " strategies = "
	         ?total " tests, " (- ?total ?failed) " passed, " ?failed " failed")
	(if (> ?failed 0) then (exit 1) else (exit 0)))

(run-tests (create$
test.md
test2.md
test3.md
test4.md
test5.md
test6.md
test7.md
test8.md
test9.md
test10.md
test11.md
test12.md
test13.md
test14.md
test15.md
test16.md
test17.md
test18.md
test19.md
test20.md
test21.md
test22.md
test23.md
test24.md
test25.md
test26.md
test27.md
test28.md
test29.md
test30.md
test31.md
test32.md
test33.md
test34.md
test35.md
test36.md
test37.md
test38.md
test39.md
test40.md
test41.md
test42.md
test43.md
test44.md
test45.md
test46.md
test47.md
test48.md
test49.md
test50.md
test51.md
test52.md
test53.md
test54.md
test55.md
test56.md
test57.md
test58.md
test59.md
test60.md
test61.md
test62.md
test63.md
test64.md
test65.md
test66.md
test67.md
test68.md
test69.md
test70.md
test71.md
test72.md
test73.md
test74.md
test75.md
test76.md
test77.md
test78.md
test79.md
test80.md
test81.md
test82.md
test83.md
test84.md
test85.md
test86.md
test87.md
test88.md
test89.md
))
