;; https://adventofcode.com/2022/day/4
;; https://adventofcode.com/2022/day/4/input
(progn
  (defun read-file-content (file-name)
    (with-temp-buffer
      (insert-file-contents file-name)
      (buffer-substring-no-properties (point-min) (point-max))))

  (defun get-range-pairs (file-content)
    (remove ""(split-string file-content "\n")))

  (defun get-sections (list)
    (mapcar #'get-sections-helper list))

  (defun get-sections-helper (file-content)
    (split-string file-content ","))

  (defun split-pair (range-string-pair)
    (mapcar #'string-to-number (split-string range-string-pair "-")))

  (defun get-range (range-string-pair)
    (mapcar #'split-pair range-string-pair))

  (defun treat-ranges (list-of-range-string-pairs)
    (mapcar #'get-range list-of-range-string-pairs))

  (defun overlap-range (range-1 range-2)
    (let
        (
         (a (car range-1))
         (b (car (last range-1)))
         (c (car range-2))
         (d (car (last range-2)))
         )
      (and (<= a d) (>= b c))
      )
    )

  (defun overlap-range-helper (list)
    (if (overlap-range (car list) (car (last list))) 1 0))

  (defun get-overlap-bool (list)
    (mapcar #'overlap-range-helper list))

  (defun sum-positives (list)
    (apply #'+ list))

  (sum-positives
   (get-overlap-bool
    (treat-ranges
     (get-sections
      (get-range-pairs (read-file-content "input.txt"))))))
  )
