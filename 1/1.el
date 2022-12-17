;; https://adventofcode.com/2022/day/1
;; https://adventofcode.com/2022/day/1/input

;; read file as string
(progn
  (defun read-from-file (file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max))))

  ;; split by \n\n e then by \n
  (setq-local input-string-list
              (split-string (read-from-file "input.txt") "\n\n" t))

  ;; split lists again
  (setq-local input-string-list-2 (mapcar 'split-string input-string-list))

  ;; convert to lists of numbers
  (defun strings-to-numbers (l)
    (mapcar 'string-to-number l))

  (setq-local nested-numbers-list (mapcar #'strings-to-numbers input-string-list-2))

  (defun sum-list-items (list) (apply '+ list))

  (apply #'max (mapcar 'sum-list-items nested-numbers-list)))
