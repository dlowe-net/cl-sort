(defpackage #:cl-sort.tests
  (:use #:cl #:cl-sort)
  (:export #:run-tests))

(in-package #:cl-sort.tests)

(defun test-sort (func n pred key)
  (let* ((sample (make-array n :initial-contents (loop
                                                    for i from 0 upto (1- n)
                                                    collect (random n))))
         (sorted-sample (sort (copy-seq sample) pred :key key)))
    (let ((test-sample (funcall func (copy-seq sample) pred :key key)))
      (cond
        ((equalp test-sample sorted-sample)
         t)
        (t
         (format t "ERR: ~a ~a ~a ~a didn't sort correctly!~%" func n pred key)
         (format t "~22a: ~s~%~22a: ~s~%~22a: ~s~%" "ORIGINAL" sample func test-sample "SORTED" sorted-sample)
         nil)))))

(defun run-tests ()
  (let ((result t))
    (dolist (func '(bubble-sort
                    insertion-sort
                    selection-sort
                    shell-sort
                    merge-sort
                    quicksort
                    dual-pivot-quicksort))
      (dolist (n '(5 30 100 1000))
        (dolist (pred '(< >))
          (dolist (key-func '(identity -))
            (unless (test-sort func n pred key-func)
              (setf result nil))))))
    result))
