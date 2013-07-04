cl-sort - a collection of sorts in common lisp
----------------------------------------------

This is a collection of different sorting implementations.  The sorts
included are:

* bubble-sort
* selection-sort
* insertion-sort
* shell-sort
* merge-sort
* quicksort
* double-pivot quicksort

Each sorting routine is intended to be a drop-in replacement for the
CL:SORT routine, though obviously the performance characteristics will
vary greatly for each.

* bubble-sort seq predicate &key key
