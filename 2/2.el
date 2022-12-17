;; https://adventofcode.com/2022/day/2
;; https://adventofcode.com/2022/day/2/input
(progn
  (defun read-from-file (file)
    (with-temp-buffer
      (insert-file-contents file)
      (buffer-substring-no-properties (point-min) (point-max))))

  (defun letters-to-numbers (str)
    (setq-local str (replace-regexp-in-string "A\\|X" "1" str))
    (setq-local str (replace-regexp-in-string "B\\|Y" "2" str))
    (setq-local str (replace-regexp-in-string "C\\|Z" "3" str)))

  (setq-local letters (read-from-file "input.txt"))

  (setq-local number-string-list (letters-to-numbers letters))

  (setq-local number-string-list-splitted (split-string number-string-list "\n" t))

  (defun split-number-pair-string (number-pair-string)
    (split-string number-pair-string))

  (setq-local number-pair-string-list (mapcar #'split-number-pair-string number-string-list-splitted))

  (defun string-list-to-number-list (list-of-strings)
    (mapcar 'string-to-number list-of-strings))

  (setq-local list-of-lists-of-numbers (mapcar #'string-list-to-number-list number-pair-string-list))

  (defun calculate-score-helper (pair)
    (let (
          (a (car pair))
          (b (car (cdr pair))))
      (calculate-score (list a b))
      )
    )

  ;; a = opponent
  ;; b = me
  (defun calculate-score (pair)
    (cond
     ((= b a) (+ 3 b))
     ((or
       (and (= 3 a) (= 1 b))
       (and (= 1 a) (= 2 b))
       (and (= 2 a) (= 3 b))) (+ 6 b))
     (t b)))

  (defun get-total-score (list)
    (mapcar #'calculate-score-helper list))

  (get-total-score list-of-lists-of-numbers)

  (defun find-loser (opponent-number)
    (cond
     ((= 1 opponent-number) 3)
     ((= 2 opponent-number) 1)
     ((= 3 opponent-number) 2)
     )
    )

  (defun find-winner (opponent-number)
    (cond
     ((= 1 opponent-number) 2)
     ((= 2 opponent-number) 3)
     ((= 3 opponent-number) 1)
     )
    )

  (defun find-the-right-move (pair)
    (let (
          (a (car pair))
          (b (car (cdr pair))))
      (cond
       ((= 2 b) (+ 3 a))
       ((= 1 b) (+ 0 (find-loser a)))
       ((= 3 b) (+ 6 (find-winner a)))
       )
      )
    )

  (apply '+ (get-total-score list-of-lists-of-numbers))

  (setq-local temp-list '((1 2) (2 1) (3 3)))

  (apply '+ (mapcar #'find-the-right-move list-of-lists-of-numbers)))
