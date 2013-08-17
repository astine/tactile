(setq lexical-binding t)

(defmacro if-let (binding &rest body)
  `(let ((,(first binding) ,(second binding)))
     (if ,(first binding)
	 ,@body)))

(defface tactile-top-level-form-face 
  '((((class color) (background dark)) (:background "#073642" :underline nil :bold nil))
    (((class color) (background light)) (:background "#93a1a1" :underline nil :bold nil))
    (t (:underline t)))
  ""
  :group 'eshell-prompt)

(defface tactile-form-at-point-face
  '((((class color) (background dark)) (:background "#586e75" :underline nil :bold nil))
    (((class color) (background light)) (:background "#fdf6e3" :underline nil :bold nil))
    (t (:underline t)))
  ""
  :group 'eshell-prompt)

(defface tactile-atom-face
  '((((class color) (background dark)) (:background "#586e75" :underline nil :bold nil))
    (((class color) (background light)) (:background "#fdf6e3" :underline nil :bold nil))
    (t (:underline t)))
  ""
  :group 'eshell-prompt)

(defface tactile-atom-at-point-face
  '((((class color) (background dark)) (:background "#002b36" :underline t :bold nil))
    (((class color) (background light)) (:background "#fdf6e3" :underline nil :bold nil))
    (t (:underline t)))
  ""
  :group 'eshell-prompt)

(defvar-local tactile-top-level-forms nil)
(defvar-local tactile-quote-markers nil)
(defvar-local no-parse-forms nil 
  "Boolean to block the parsing of the top level forms when we know they are imbalanced")

(defvar-local tactile-current-form-depth 0)

(defvar tactile-kill-ring nil)
(defvar tactile-kill-ring-size 10)
(defvar tactile-kill-ring-last-yank nil)

(defvar-local top-level-overlay nil)
(defvar-local at-point-overlay nil)
(defvar-local atom-at-point-overlay nil)

;;; Form reading and manipulation

(defun member-start (member)
  (first member))

(defun member-end (member)
  (second member))

(defun member-text (member)
  (third member))

(defun member-type (member)
  (fourth member))

(defun quote-escaped-p (&optional point)
  (let ((point (or point (point))))
    (save-excursion
      (goto-char point)
      (when (and (char-after) (= (char-after) 34))
	(let ((slash-count 0))
	  (while (and (char-before) (= (char-before) 92))
	    (incf slash-count)
	    (backward-char))
	  (oddp slash-count))))))

(defun at-unescaped-quote-p ()
  (and (char-after) (= (char-after) ?\")
       (not (quote-escaped-p))))

(defun get-best-quote-marker (&optional quote-markers)
  (let ((quote-markers (or quote-markers tactile-quote-markers '((0 nil)))))
    (while (<= (point) (caar quote-markers))
      (setq quote-markers (rest quote-markers)))
    (first quote-markers)))

(defun in-quotes-p (&optional point origin)
  (let ((point (or point (point)))
	(origin (or origin (get-best-quote-marker))))
    (save-excursion
      (goto-char (first origin))
      (let ((quote-count 0))
	(while (< (point) point)
	  (cond ((at-unescaped-quote-p)
		 (incf quote-count)
		 (forward-char))
		((and (char-after) (= (char-after) 59) 
		      (if (second origin)
			  (oddp quote-count)
			(evenp quote-count))) ;on comment
		 (forward-line))
		(t
		 (forward-char))))
	(if (second origin)
	    (evenp quote-count)
	  (oddp quote-count))))))

(defun find-closing-quote ()
  (forward-char)
  (while (not (at-unescaped-quote-p))
    (forward-char)))

(defun find-opening-quote ()
  (backward-char)
  (while (not (at-unescaped-quote-p))
    (backward-char)))

(defun tactile-get-quote-markers ()
  (save-excursion
    (let ((quote-markers (list '(0 nil))))
      (goto-char 0)
      (while (> (count-lines (point) (point-max)) 10)
	(forward-line 10)
	(push (list (point)
		    (in-quotes-p (point) (first quote-markers)))
	      quote-markers))
      quote-markers)))

(defun in-comment-p (&optional point)
  (let ((point (or point (point))))
    (save-excursion
      (beginning-of-line) 
      (let ((in-comment-p nil))
	(while (and (< (point) (line-end-position))
		    (not in-comment-p))
	  (when (and (char-after)
		     (= (char-after) 59)
		     (not (in-quotes-p)))
	    (setq in-comment-p (point)))
	  (forward-char))
	in-comment-p))))

(defun find-closing-paren (&optional max)
  (save-excursion
    (let ((depth 0)
	  (location nil))
      (while (and (< (point) (or max (point-max))) (null location))
	(forward-char)
					;40 is open paren, 41 is close paren
	(cond ((at-unescaped-quote-p) ;on quotation marks
	       (find-closing-quote))
	      ((and (char-after) (= (char-after) 59)) ;on comment
	       (forward-line))
	      ((and (zerop depth) (char-after) (= (char-after) 41))
	       (setq location (point)))
	      ((and (char-after) (= (char-after) 40))
	       (incf depth 1))
	      ((and (char-after) (= (char-after) 41))
	       (decf depth 1))))
      location)))
	
(defun tactile-find-top-level-forms ()
  (save-excursion
    (let ((forms nil))
      (goto-char (point-min))
      (while (< (point) (point-max))
					;40 is open paren, 41 is close paren
	(when (and (char-after) (= (char-after) 40)
		    (not (in-quotes-p)) (not (in-comment-p)))
	  (let ((form nil))
	    (push (point) form)
	    (let ((end (find-closing-paren)))
	      (if end ;If this form does not close, don't add it and finish parsing
		  (progn
		    (push end form)
		    (goto-char (1+ (first form)))
		    (push (buffer-substring-no-properties (second form) (1+ (first form))) form)
		    (push :form form)
		    (push (nreverse form) forms))
		(goto-char (1- (point-max)))))))
	(when (< (point) (point-max))
	  (forward-char)))
      (nreverse forms))))

(defun tactile-get-top-level-form ()
  (let ((forms tactile-top-level-forms))
    (while (and forms (< (member-end (first forms)) (point)))
      (setq forms (rest forms)))
    (when (and forms (< (member-start (first forms)) (point)))
      (first forms))))

(defun tactile-get-form-at-point (&optional layers)
  (let* ((outer-form (tactile-get-top-level-form))
	 (search-area-start (if outer-form (member-start outer-form) (point-min)))
	 (search-area-end (if outer-form (member-end outer-form) (point-max))))
    (save-excursion
      (let ((begin nil)
	    (end nil)
	    (form-depth (or layers 0)))
	(when (in-quotes-p) 
	  (find-opening-quote)
	  (backward-char))
	(when (in-comment-p)
	  (goto-char (in-comment-p)))
	(unless (or (and (char-after) (= (char-after) 41)) (= (point) (point-max)))
	  (forward-char))
	(while (and (> (point) search-area-start) (not begin))
	  (backward-char)
	  (cond ((at-unescaped-quote-p)
		 (find-opening-quote))
		((and (char-after) (= (char-after) 10) (in-comment-p))
		 (goto-char (in-comment-p)))
		((and (zerop form-depth) (char-after) (= (char-after) 40))
		 (setq begin (point)))
		((and (char-after) (= (char-after) 41))
		 (incf form-depth 1))
		((and (char-after) (= (char-after) 40))
		 (decf form-depth 1))))
	(when begin
	  (setq end (find-closing-paren search-area-end))
	  (when end
	    (list begin end (buffer-substring-no-properties begin (1+ end)) :form)))))))

;;; Atom reading and manipulation

(defun read-atom ()
  (let ((start (point))
	(end (1- (re-search-forward "[\n\s\t()]"))))
    (goto-char end)
    (list start end (buffer-substring-no-properties start end) :atom)))

(defun read-str ()
  (let ((start (point)))
    (find-closing-quote)
    (forward-char)
    (list start (point)  (buffer-substring-no-properties start (point)) :string)))

(defun read-form ()
  (let ((start (point))
	(end (find-closing-paren)))
    (goto-char (1+ end))
    (list start end (buffer-substring-no-properties start (1+ end)) :form)))

(defun read-form-members (form)
  (save-excursion
    (when form
      (goto-char (1+ (member-start form)))
      (let ((members nil))
	(while (< (point) (member-end form))
	  (cond ((= (char-after) 34)
		 (push (read-str) members))
		((= (char-after) 40)
		 (push (read-form) members))
		(t
		 (push (read-atom) members)))
	  (re-search-forward "[^\n\s\t]")
	  (backward-char))
	(nreverse members)))))

(defun top-level-forms-as-atoms ()
  (mapcar (lambda (form)
	    (list (member-start form)
		  (member-end form)
		  nil
		  :form))
       tactile-top-level-forms))
 
(defun in-which-member (members point &optional include-nearest-p)
  (cl-labels ((find-current-atom (atoms)
				 (when atoms
				   (if (<= (member-start (first atoms)) point)
				       (when (or include-nearest-p
						 (<= point (member-end (first atoms))))
					 (first atoms))
				     (find-current-atom (rest atoms))))))
    (find-current-atom (reverse (sort members (lambda (a b)
						(< (member-start a) (member-start b))))))))

(defun member-at-point (&optional jump)
  (let ((form (tactile-get-form-at-point (or jump 0))))
    (if jump
	form
      (when form
	(in-which-member (read-form-members form) (point))))))

(defun surrounding-three-members (&optional jump)
  (let* ((form (tactile-get-form-at-point (or jump 0)))
	 (members (if form (read-form-members form) (top-level-forms-as-atoms)))
	 (prev nil)
	 (result nil))
    (while (null result)
      (cond ((null members)
	     (setq result (list prev nil nil)))
	    ((and (<= (member-start (first members)) (point))
		  (<= (point) (member-end (first members))))
	     (setq result (list prev (first members) (second members))))
	    ((and (> (member-start (first members)) (point))
		  (or (not prev) (> (point) (member-end prev))))
	     (setq result (list prev nil (first members)))))
      (setq prev (first members))
      (setq members (rest members)))
    result))

(defun tactile-highlight-atom-at-point ()
  (let ((member (member-at-point)))
    (when atom-at-point-overlay
      (delete-overlay atom-at-point-overlay))
    (when member
      (move-overlay atom-at-point-overlay (member-start member) (member-end member)))))

(defun trim-unecessary-form-spaces (form)
  (let ((real-point (point)))
    (save-excursion
      ;(goto-char (member-start form))
      ;(while (<= (point) (member-end form))
	;(cond ((and (char-before) (= (char-before) 40)
		    ;(char-after) (or (= (char-before) 32) (= (char-before) 9))
		    ;(not (= (point) real-point)))
	       ;(delete-char 1))
	      ;((and (char-before) (or (= (char-before) 32) (= (char-before) 9))
		  ;(char-after) (= (char-after) 41)
		  ;(not (= (point) real-point)))
	       ;(delete-char -1))
	      ;(t
	       ;(forward-char))))
      (goto-char (member-start (tactile-get-top-level-form)))
      (indent-pp-sexp))))

(defun tactile-on-change (x y z)
  (setq tactile-quote-markers (tactile-get-quote-markers))
  (setq tactile-top-level-forms (tactile-find-top-level-forms))
  (when (tactile-get-top-level-form)
    (trim-unecessary-form-spaces (tactile-get-top-level-form))))

(defmacro tactile-one-change (&rest body)
  `(progn
     (if no-parse-forms
	 (incf no-parse-forms)
       (setq no-parse-forms 0))
     ,@body
     (if (zerop no-parse-forms)
	 (setq no-parse-forms nil)
       (decf no-parse-forms))
     (tactile-on-change nil nil nil)))

(defun tactile-highlight-top-level-form ()
  (let ((form (tactile-get-top-level-form)))
    (when top-level-overlay
      (delete-overlay top-level-overlay))
    (when form
      (move-overlay top-level-overlay (member-start form) (1+ (member-end form))))))

(defun tactile-highlight-selected-form ()
  (let ((form (tactile-get-form-at-point tactile-current-form-depth)))
    (when at-point-overlay
      (delete-overlay at-point-overlay))
    (when form
      (move-overlay at-point-overlay (member-start form) (1+ (member-end form))))))

(defun highlight-forms ()
  (tactile-highlight-top-level-form)
  (tactile-highlight-selected-form)
  (tactile-highlight-atom-at-point))

(defun tactile-on-move ()
  (when (not (equal (member-text (first tactile-kill-ring-last-yank))
		    (member-text (member-at-point))))
    (setq tactile-kill-ring-last-yank nil))
  (highlight-forms))

(defun goto-member (member &optional reversep)
  (case (member-type member)
    (:atom (goto-char (member-end member)))
    (:string (goto-char (1- (member-end member))))
    (:form (enter-member member reversep))))

(defun enter-member (member reversep)
  (let* ((members (read-form-members member))
	 (inner-member (if reversep (first (last members)) (first members))))
    (if inner-member
	(goto-member inner-member reversep)
      (goto-char (member-end member)))))

(defun navigate-atoms (reversep &optional jump)
  (destructuring-bind (prev member next)
      (surrounding-three-members jump)
    (let ((next-member (if reversep prev next)))
      (if next-member 
	  (goto-member next-member reversep)
	(navigate-atoms reversep (1+ (or jump 0)))))))

(defun move-foreward () (interactive) (navigate-atoms nil))
(defun move-backward () (interactive) (navigate-atoms 't))

(defun tactile-start-new-member ()
  (interactive)
  (destructuring-bind (prev member next)
      (surrounding-three-members)
    (when (or prev member next)
      (goto-char (or (member-end (or member prev)) (member-start next)))
      (when (equal (member-type (or member prev)) :form)
	(forward-char))
      (insert-char 32)
      (unless (or prev member)
	(backward-char)))))

(defun tactile-start-new-member-reverse ()
  (interactive)
  (destructuring-bind (prev member next)
      (surrounding-three-members)
    (when (or prev member next)
      (goto-char (or (member-start (or member next)) (member-end prev)))
      (insert-char 32)
      (when (or member next)
	(backward-char)))))

(defun insert-parentheses ()
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (insert-char 40))
	  (:atom (tactile-start-new-member)
		 (insert-parentheses)))
      (insert-char 40)
      (insert-char 41)
      (backward-char))))

(defun handle-close-parentheses ()
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (insert-char 41))
	  (:atom (goto-char (member-end (tactile-get-form-at-point)))
		 (forward-char)
		 (tactile-start-new-member)))
    (goto-char (member-end (tactile-get-form-at-point)))
    (forward-char)
    (tactile-start-new-member))))

(defun atom-to-string ()
  (let ((member (member-at-point)))
    (when (and member (equal (member-type member) :atom))
      (save-excursion
	(combine-after-change-calls
	 (goto-char (member-end member))
	 (insert-char 34)
	 (goto-char (member-start member))
	 (insert-char 34))))))

(defun string-to-atom ()
  (let ((member (member-at-point)))
    (when (and member (equal (member-type member) :string))
      (save-excursion
	(combine-after-change-calls
	 (goto-char (member-end member))
	 (delete-char -1)
	 (goto-char (member-start member))
	 (delete-char 1))))))

(defun handle-quote ()
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:atom (if (= (point) (member-end member))
		     (progn
		       (tactile-start-new-member)
		       (insert-string "\"\"")
		       (backward-char))
		   (atom-to-string)))
	  (:string (cond ((> (1+ (point)) (member-end member))
			  (tactile-start-new-member))
			 ((= (point) (member-start member))
			  (forward-char))
			 (t
			  (insert-string "\\\"")))))
      (insert-string "\"\"")
      (backward-char))))

(defun delete-member (member)
  (combine-after-change-calls
    (goto-char (member-start member))
    (delete-char (- (member-end member) (member-start member)))
    (while (and (char-after) (= (char-after) 32))
      (delete-char 1))
    (while (and (char-before) (= (char-before) 32))
      (delete-char -1))
    (when (and (char-before) (not (= (char-before) 40))
	       (char-after) (not (= (char-after) 41)))
      (insert-char 32))))

(defun delete-current-form (&optional force jump)
  (let ((form (tactile-get-form-at-point (or jump tactile-current-form-depth))))
    (when (or force
	      (zerop (length (read-form-members form)))
	      (y-or-n-p "Really delete form?"))
      (delete-member form))))

(defun delete-current-member ()
  (let ((member (member-at-point)))
    (when member
      (delete-member member))))

(defun tactile-add-to-kill-ring (member)
  (when (>= (length tactile-kill-ring) tactile-kill-ring-size)
    (setq tactile-kill-ring (butlast tactile-kill-ring)))
  (push member tactile-kill-ring))

(defun add-current-form-to-kill-ring (&optional jump)
  (interactive)
  (tactile-add-to-kill-ring 
   (tactile-get-form-at-point 
    (or jump tactile-current-form-depth))))

(defun add-current-member-to-kill-ring ()
  (interactive)
  (let ((member (member-at-point)))
    (when member
      (tactile-add-to-kill-ring member))))

(defun kill-current-form (&optional jump)
  (interactive)
  (add-current-form-to-kill-ring jump)
  (delete-current-form t jump))

(defun kill-current-member ()
  (interactive)
  (add-current-member-to-kill-ring)
  (delete-current-member))

(defun tactile-insert-member (member &optional reversep)
  (destructuring-bind (prev memb next)
      (surrounding-three-members)
    (combine-after-change-calls
      (if reversep
	  (tactile-start-new-member-reverse)
	(tactile-start-new-member))
      (insert-string (member-text member)))) )

(defun tactile-replace-current-member (new-member &optional jump)
  (combine-after-change-calls
   (if jump
       (delete-current-form t jump)
     (delete-current-member))
   (tactile-insert-member new-member)))

(defun tactile-yank (&optional reversep)
  (interactive)
  (tactile-insert-member (first tactile-kill-ring) reversep)
  (setq tactile-kill-ring-last-yank (list (first tactile-kill-ring) 0)))

(defun tactile-yank-again ()
  (interactive)
  (cond ((not tactile-kill-ring-last-yank)
	 (tactile-yank))
	((< (1+ (second tactile-kill-ring-last-yank)) (length tactile-kill-ring))	
	 (let ((new-index (1+ (second tactile-kill-ring-last-yank))))
	   (tactile-replace-current-member (nth new-index tactile-kill-ring))
	   (setq tactile-kill-ring-last-yank (list (nth new-index tactile-kill-ring) new-index))))
	(t
	 (tactile-replace-current-member (first tactile-kill-ring))
	 (setq tactile-kill-ring-last-yank (list (first tactile-kill-ring) 0)))))

(defun handle-backspace ()
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (cond ((or (= (member-start member) (point))
			      (= (+ (member-start member) 2) (member-end member)))
			  (delete-current-member))
			 ((or (= (member-start member) (1- (point)))
			      (= (member-end member) (point)))
			  (string-to-atom))
			 ((and (char-before) (= (char-before) 34))
			  (delete-char -2))
			 (t
			  (delete-char -1))))
	  (:atom (cond ((and (= (member-start member) (point))
			     (and (char-before) (= (char-before) 40)))
			(delete-current-form))
		       ((= (member-start member) (point))
			(delete-current-member))
		       (t
			(delete-char -1)))))
      (if (and (char-before) (= (char-before) 40))
	  (delete-current-form)
	(delete-char -1)))))

(defun handle-space ()
  (interactive)
  (let ((member (member-at-point)))
    (when member
      (cond ((not (or (= (member-start member) (point))
			   (= (member-end member) (point))))
	     (if (equal (member-type member) :string)
		 (combine-after-change-calls
		  (insert-char 32))
	       ;(combine-after-change-calls
		;(insert-char 92)
		;(insert-char 32))
	       ))
	    ((= (member-end member) (point))
	     (tactile-start-new-member))
	    ((= (member-start member) (point))
	     nil)))))
  
(defun switch-back () (interactive) (remove-overlays) (emacs-lisp-mode))

(define-derived-mode tactile-mode emacs-lisp-mode "Tactile"
  "Major mode extending emacs lisp mode for structural editing of lisp
   code"
  (setq top-level-overlay (make-overlay 0 0))
  (setq at-point-overlay (make-overlay 0 0))
  (setq atom-at-point-overlay (make-overlay 0 0))

  (overlay-put top-level-overlay 'face 'tactile-top-level-form-face)
  (overlay-put at-point-overlay 'face 'tactile-form-at-point-face)
  (overlay-put atom-at-point-overlay 'face 'tactile-atom-at-point-face)

  (add-hook 'after-change-functions 'tactile-on-change nil t)
  (add-hook 'post-command-hook 'tactile-on-move nil t)

  (setq tactile-quote-markers (tactile-get-quote-markers))
  (setq tactile-top-level-forms (tactile-find-top-level-forms))

  (define-key (current-local-map) (kbd "M-n") 'move-foreward)
  (define-key (current-local-map) (kbd "M-p") 'move-backward)
  (define-key (current-local-map) (kbd "TAB") 'tactile-start-new-member)
  (define-key (current-local-map) (kbd "<backtab>") 'tactile-start-new-member-reverse)
  (define-key (current-local-map) (kbd "(") 'insert-parentheses)
  (define-key (current-local-map) (kbd ")") 'handle-close-parentheses)
  (define-key (current-local-map) (kbd "\"") 'handle-quote)
  (define-key (current-local-map) (kbd "<backspace>") 'handle-backspace)
  (define-key (current-local-map) (kbd "SPC") 'handle-space)
  (define-key (current-local-map) (kbd "C-c C-k") 'kill-current-member)
  (define-key (current-local-map) (kbd "C-c C-y") 'tactile-yank)

  (define-key (current-local-map) (kbd "C-q") 'switch-back)
  (viper-change-state-to-emacs))
