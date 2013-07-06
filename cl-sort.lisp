(in-package #:cl-sort)

(defun bubble-sort (seq pred &key key)
  "Performs a bubble sort on SEQ using PRED.  The function KEY is
called on the elements of SEQ if passed.

Bubble sort is one of the slowest sorts and almost never used in
practice. An optimized version is included here for completeness."
  (unless key
    (setf key #'identity))
  (flet ((less-than-p (a b)
           (funcall pred
                    (funcall key (elt seq a))
                    (funcall key (elt seq b)))))
    (loop
       with last-idx = (length seq)
       with new-last-idx = 0
       while (plusp last-idx)
       do
         (setf new-last-idx 0)
         (loop
            for i from 1 upto (1- last-idx)
            when (less-than-p i (1- i))
            do
              (setf new-last-idx i)
              (rotatef (elt seq i) (elt seq (1- i))))
         (setf last-idx new-last-idx)))
  seq)

(defun selection-sort (seq pred &key key)
  "Performs a selection sort on SEQ using PRED.  The function KEY is
called on the elements of SEQ if passed."
  (unless key
    (setf key #'identity))
  (flet ((less-than-p (a b)
           (funcall pred
                    (funcall key (elt seq a))
                    (funcall key (elt seq b)))))
  (loop
     for i from 0 upto (1- (length seq))
     as selection = (loop
                       with min-idx = i
                       for j from (1+ i) upto (1- (length seq))
                       when (less-than-p j min-idx)
                       do (setf min-idx j)
                       finally (return min-idx))
     do
       (rotatef (elt seq i) (elt seq selection))))
  seq)

(defun %insertion-sort (seq pred key left right)
  (flet ((less-than-p (a b)
           (funcall pred
                    (funcall key (elt seq a))
                    (funcall key (elt seq b)))))

    (loop for i from (1+ left) upto right do
         (loop
            for j from i downto (1+ left)
            while (less-than-p j (1- j))
            do
              (rotatef (elt seq j)
                       (elt seq (1- j)))))
    seq))


(defun insertion-sort (seq pred &key key)
  "Performs an insertion sort on SEQ using PRED.  The function KEY is
called on the elements of SEQ if passed."
  (%insertion-sort seq pred (or key #'identity) 0 (1- (length seq))))


(defun shell-sort (seq pred &key key)
  "Performs a shell sort on SEQ using PRED.  The function KEY is
called on the elements of SEQ if passed.  This particular version of
shell sort dynamically selects the gap used, then does some ham-handed
hacking to ensure that the sort does a final pass with a gap of 1,
then terminates.."
  (unless key
    (setf key #'identity))
  (loop
     for h = (loop for ph = 1 then (1+ (* 3 ph))
                while (<= ph (floor (length seq) 9))
                finally (return ph))
     then (cond
            ((= h 1)
             0)
            ((< h 3)
             1)
            (t
             (floor h 3)))
     while (plusp h)
     do
       (loop for i from h upto (1- (length seq)) do
            (let ((v (elt seq i)))
              (loop
                 for j from i downto h by h
                 until (funcall pred (funcall key (elt seq (- j h))) (funcall key v))
                 do
                   (setf (elt seq j) (elt seq (- j h)))
                 finally
                   (setf (elt seq j) v)))))
  seq)

(defun %quick-sort (seq pred key left right)
  (when (< left right)
    (let* ((pivot (elt seq left))
           (pivot-val (funcall key pivot))
           (store-idx left))
      (rotatef (elt seq left)
               (elt seq right))
      (loop
         for i from left to (1- right)
         unless (funcall pred
                         pivot-val
                         (funcall key (elt seq i)))
         do
           (rotatef (elt seq i)
                    (elt seq store-idx))
           (incf store-idx))
      (rotatef (elt seq right)
               (elt seq store-idx))

      (%quick-sort seq pred key left (1- store-idx))
      (%quick-sort seq pred key (1+ store-idx) right)))
  seq)

(defun quicksort (seq pred &key key)
  "Performs an in-place quick-sort on SEQ using PRED.  The function KEY is
called on the elements of SEQ if passed."
  (%quick-sort seq pred (or key #'identity) 0 (1- (length seq))))



(defun %dual-pivot-quicksort (seq pred key left right &aux (+dist-size+ 13) (+tiny-size+ 17))
  (flet ((key-elt (idx)
           (funcall key (elt seq idx))))
    (let ((len (- right left)))
      (when (< len +tiny-size+)           ; insertion sort on tiny array
        (%insertion-sort seq pred key left right)
        (return-from %dual-pivot-quicksort seq))

      ;; median indexes
      (let* ((sixth (floor len 6))
             (m1 (+ left sixth))
             (m2 (+ m1 sixth))
             (m3 (+ m2 sixth))
             (m4 (+ m3 sixth))
             (m5 (+ m4 sixth)))
        ;; 5-element sorting network
        (macrolet ((conditional-swap (a b)
                     `(progn
                        (when (funcall pred
                                       (key-elt ,b)
                                       (key-elt ,a))
                          (rotatef (elt seq ,a)
                                   (elt seq ,b))))))
          (conditional-swap m1 m2)
          (conditional-swap m4 m5)
          (conditional-swap m1 m3)
          (conditional-swap m2 m3)
          (conditional-swap m1 m4)
          (conditional-swap m3 m4)
          (conditional-swap m2 m5)
          (conditional-swap m2 m3)
          (conditional-swap m4 m5))

        ;; pivots: [ < pivot1 | pivot1 <= && <= pivot2 | > pivot2 ]
        (let* ((pivot1 (elt seq m2))
               (pivot1-keyval (key-elt m2))
               (pivot2 (elt seq m4))
               (pivot2-keyval (key-elt m4))
               (diff-pivots (/= pivot1-keyval pivot2-keyval)))

          ;; these values are stored in pivot1 and pivot2
          (setf (elt seq m2) (elt seq left)
                (elt seq m4) (elt seq right))

          ;; center part pointers
          (let ((less (1+ left))
                (great (1- right)))
            ;; sorting
            (cond
              (diff-pivots
               (loop for k = less then (1+ k)
                  while (<= k great)
                  do
                    (let ((x (elt seq k))
                          (x-keyval (key-elt k)))
                      (cond
                        ((funcall pred x-keyval pivot1-keyval)
                         (setf (elt seq k) (elt seq less))
                         (setf (elt seq less) x)
                         (incf less))
                        ((funcall pred pivot2-keyval x-keyval)
                         (loop while (and (funcall pred pivot2-keyval (key-elt great))
                                          (< k great))
                            do (decf great))
                         (setf (elt seq k) (elt seq great))
                         (setf (elt seq great) x)
                         (decf great)
                         (setf x (elt seq k))
                         (setf x-keyval (key-elt k))
                         (when (funcall pred x-keyval pivot1-keyval)
                           (setf (elt seq k) (elt seq less)
                                 (elt seq less) x)
                           (incf less)))))))
              (t
               (loop for k = less then (1+ k)
                  while (<= k great)
                  do
                    (let ((x (elt seq k))
                          (x-keyval (key-elt k)))
                      (cond
                        ((funcall pred x-keyval pivot1-keyval)
                         (setf (elt seq k) (elt seq less))
                         (setf (elt seq less) x)
                         (incf less))
                        ((funcall pred pivot1-keyval x-keyval)
                         (loop while (and (funcall pred pivot2-keyval (key-elt great))
                                          (< k great))
                            do (decf great))
                         (setf (elt seq k) (elt seq great))
                         (setf (elt seq great) x)
                         (decf great)
                         (setf x (elt seq k))
                         (setf x-keyval (key-elt k))
                         (when (funcall pred x-keyval pivot1-keyval)
                           (setf (elt seq k) (elt seq less)
                                 (elt seq less) x)
                           (incf less))))))))
            ;; swap
            (setf (elt seq left) (elt seq (1- less))
                  (elt seq (1- less)) pivot1
                  (elt seq right) (elt seq (1+ great))
                  (elt seq (1+ great)) pivot2)
            ;; left and right parts
            (%dual-pivot-quicksort seq pred key left (- less 2))
            (%dual-pivot-quicksort seq pred key (+ great 2) right)
            ;; equal elements
            (when (and (> (- great less)
                          (- len +dist-size+))
                       diff-pivots)
              (loop for k = less then (1+ k)
                 while (<= k great) do
                   (let ((x (elt seq k))
                         (x-keyval (key-elt k)))
                     (cond
                       ((and (not (funcall pred x-keyval pivot1-keyval))
                             (not (funcall pred pivot1-keyval x-keyval)))
                        (setf (elt seq k)
                              (elt seq less)
                              (elt seq less)
                              x)
                        (incf less))
                       ((and (not (funcall pred x-keyval pivot2-keyval))
                             (not (funcall pred pivot2-keyval x-keyval)))
                        (setf (elt seq k)
                              (elt seq great)
                              (elt seq great)
                              x)
                        (decf great)
                        (setf x (elt seq k))
                        (setf x-keyval (key-elt k))
                        (when (= x-keyval pivot1-keyval)
                          (setf (elt seq k)
                                (elt seq less)
                                (elt seq less)
                                x)
                          (incf less)))))))
            ;; center part
            (when diff-pivots
              (%dual-pivot-quicksort seq pred key less great)))))))
  seq)

(defun dual-pivot-quicksort (seq pred &key key)
  "Performs an in-place dual-pivot quick-sort on SEQ using PRED.  The
function KEY is called on the elements of SEQ if passed."
  (%dual-pivot-quicksort seq pred (or key #'identity) 0 (1- (length seq))))

(defun %bottom-up-merge (seq pred key left right end dest)
  (let ((i0 left)
        (i1 right))
    (loop
       for j from left upto (1- end)
       if (and (< i0 right)
               (or (>= i1 end)
                   (funcall pred (funcall key (elt seq i0)) (funcall key (elt seq i1)))))
       do
         (setf (elt dest j) (elt seq i0))
         (incf i0)
       else do
         (setf (elt dest j) (elt seq i1))
         (incf i1))))

(defun merge-sort (seq pred &key key)
  "Performs an in-place merge sort on SEQ using PRED.  The
function KEY is called on the elements of SEQ if passed."
  (unless key
    (setf key #'identity))
  (let* ((len (length seq))
         (tmp (make-array (list len))))
    (loop
       for width = 1 then (* 2 width)
       while (< width len)
       do
         (loop
            for i = 0 then (+ i (* 2 width))
            while (< i len)
            do
              (%bottom-up-merge seq
                                pred
                                key
                                i 
                                (min (+ i width) len)
                                (min (+ i (* 2 width)) len)
                                tmp))
         (map-into seq #'identity tmp)))
  seq)

