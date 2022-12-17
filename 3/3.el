;; https://adventofcode.com/2022/day/3
;; https://adventofcode.com/2022/day/3/input
(progn
  (defun read-from-file (file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max))))

  (defun get-list-of-strings (input)
    (remove "" (split-string (read-from-file input) "\n")))

  (defun split-list-evenly (list)
    (seq-partition list (/ (length list-item) 2)))

  (defun get-common-chars (list-item)
    (seq-uniq (apply #'seq-intersection
                     (split-list-evenly (remove "" (split-string list-item ""))))))

  (defun get-priority-char-list (list-of-strings)
    (apply #'append (mapcar #'get-common-chars list-of-strings)))

  (defun get-char-priority (char)
    (let ((uppercase-char-number-threshold 90)
          (lowercase-char-number-diff (- ?a 1)))
      (if (> (string-to-char char) uppercase-char-number-threshold)
          (- (string-to-char char) lowercase-char-number-diff)
        (+ (- ?z lowercase-char-number-diff) (- (string-to-char char) (- ?A 1))))))

  (defun get-priority-number-list (list-of-chars)
    (mapcar #'get-char-priority list-of-chars))

  (defun get-sum-of-priorities (priorities-list)
    (apply '+ priorities-list))

  (get-sum-of-priorities
   (get-priority-number-list
    (get-priority-char-list
     (get-list-of-strings "input.txt")))))
