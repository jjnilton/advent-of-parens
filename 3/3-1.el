;; https://adventofcode.com/2022/day/3
;; https://adventofcode.com/2022/day/3/input
(progn
  (defun read-from-file (file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max))))


  (defun split-groups-from-file (input-file)
    (seq-partition (split-string (read-from-file input-file)) 3))


  (defun split-and-trim (list)
    (remove "" (split-string list "")))

  (defun find-common-element (group)
    (let (
          (first-item
           (split-and-trim (car group)))
          (second-item
           (split-and-trim (car (cdr group))))
          (third-item
           (split-and-trim (car (last group))))
          )
      (seq-uniq
       (seq-intersection
        third-item (seq-intersection first-item second-item)))))

  (defun get-common-elements (list-of-groups)
    (flatten-list (mapcar #'find-common-element list-of-groups)))

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
    (get-common-elements (split-groups-from-file "input.txt" ))))
  )
