(progn
  (defun read-file-content (file-name)
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring-no-properties (point-min) (point-max))))

  (defun get-steps (file-content-string)
    (split-string (car (last (split-string file-content-string "\n\n"))) "\n"))

  (defun get-stacks (file-content-string)
    (butlast(split-string (car (split-string file-content-string "\n\n")) "\n")))

  (defun snoc (list element-to-add)
    (append list (list element-to-add)))

  (defun keep-every-fourth-char (string)
    (let ((result "")
          (i 1))
      (while (< i (length string))
        (setq result (concat result (substring string i (1+ i))))
        (setq i (+ i 4)))
      result))

  (defun clean-stack-lists (stack-list)
    (mapcar #'keep-every-fourth-char stack-list))

  (defun split-cleaned-stack-lists (cleaned-stack-list)
    (butlast (cdr (split-string cleaned-stack-list ""))))

  (defun transpose-lists-rows (lists)
    (let ((result '()))
      (while (and lists (car lists))
        (let ((row (nreverse (mapcar #'car lists))))
          (push row result)
          (setq lists (mapcar #'cdr lists))))
      (nreverse result)))

  (defun move-list-item (stacks amount from to)
    (let (
          (stacks-copy (copy-sequence stacks))
          (from-list (remove " "(nth (- from 1) stacks)))
          (to-list (nth (- to 1) stacks))
          )
      (let (
            (to-be-moved (reverse (seq-subseq from-list
                                              (if (> amount (length from-list))
                                                  (* (length from-list) -1)
                                                (* amount -1)
                                                  )
                                              (length from-list))))
            )
        (setf (nth (- to 1) stacks-copy) (remove " " (append to-list to-be-moved)))
        (setf (nth (- from 1) stacks-copy) (seq-subseq from-list 0 (if (> amount (length from-list)) 0 (- (length from-list) amount))))
        stacks-copy
        )
      )
    )

  (setq-local stacks (transpose-lists-rows (mapcar #'split-cleaned-stack-lists
                                                   (clean-stack-lists (get-stacks (read-file-content "input.txt"))))))

  (defun get-last-item-of-each-sublist (stacks)
    (string-join (mapcar (lambda (e) (car (reverse e))) stacks)))

  (dolist (element (mapcar (lambda (e) (mapcar 'string-to-number e)) (mapcar (lambda (e) (remove "" (split-string e "[^[:digit:]]+"))) (remove "" (get-steps (read-file-content "input.txt"))))))
    (setq-local stacks (move-list-item stacks (nth 0 element) (nth 1 element) (nth 2 element)))
    )
  stacks
  (get-last-item-of-each-sublist stacks)
  )
