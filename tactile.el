(setq lexical-binding t)

(defmacro if-let (binding &rest body)
  "Binds the result of an if conditional statement so that it can be used in the 
  body of the if statement."
  (declare (indent defun))
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

(defvar-local tactile-current-form-depth nil)

(defvar tactile-kill-ring nil)
(defvar tactile-kill-ring-size 10)
(defvar tactile-kill-ring-last-yank nil)

(defvar-local top-level-overlay nil)
(defvar-local active-member-overlay nil)

(defvar-local tactile-last-point 0)

;;; Form reading and manipulation

(defun member-start (member)
  "Returns the buffer location of member's start"
  (first member))

(defun member-end (member)
  "Returns the buffer location of member's end"
  (second member))

(defun member-text (member)
  "Returns the text of the member"
  (third member))

(defun member-type (member)
  "Returns the type of the member"
  (fourth member))

(defun quote-escaped-p (&optional point)
  "Returns true if the character under the point is a quote, and that quote
  is escaped by an odd number of backslashes."
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
  "Returns true if the character under the point is a double quote and
  that quote is not escaped by an odd number of backslashes."
  (and (char-after) (= (char-after) ?\")
       (not (quote-escaped-p))))

(defun get-best-quote-marker (&optional quote-markers)
  "Returns the quote marker which is furthest in the file, while still being
  before the point."
  (let ((quote-markers (or quote-markers tactile-quote-markers '((0 nil)))))
    (while (<= (point) (caar quote-markers))
      (setq quote-markers (rest quote-markers)))
    (first quote-markers)))

(defun in-quotes-p (&optional point origin)
  "Returns true if the point is within a pair of quotes."
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
  "Find the next unescaped quote in the buffer."
  (forward-char)
  (while (not (at-unescaped-quote-p))
    (forward-char)))

(defun find-opening-quote ()
  "Find the last unescaped quote in the buffer."
  (backward-char)
  (while (not (at-unescaped-quote-p))
    (backward-char)))

(defun tactile-get-quote-markers ()
  "Go through the entire buffer and mark whether the beginning of every tenth line
  is in quotes. This is to make testing whether the point is in a pair of quotes 
  faster."
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
  "Return true if the point is within a comment (after a semicolon before the end of a line.)"
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
  "Finds the next closing paren at the same paren depth as the point."
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
  "Traverses the entire buffer looking for top level forms and returns a list of
  them in the order they were found."
  (save-excursion
    (let ((forms nil))
      (goto-char (point-min))
      (while (< (point) (point-max))
					;40 is open paren, 41 is close paren
	(when (and (char-after) (= (char-after) 40)
		    (not (in-quotes-p)) (not (in-comment-p)))
	  (let ((form nil))
	    (push (point) form)
	    (let ((end (1+ (find-closing-paren))))
	      (if end ;If this form does not close, don't add it and finish parsing
		  (progn
		    (push end form)
		    (goto-char (1+ (first form)))
		    (push (buffer-substring-no-properties (second form) (first form)) form)
		    (push :form form)
		    (push (nreverse form) forms))
		(goto-char (1- (point-max)))))))
	(when (< (point) (point-max))
	  (forward-char)))
      (nreverse forms))))

(defun tactile-get-top-level-form ()
  "Returns the top level form at point. (This function will fail is tactile-top-level-forms
  is blank.)"
  (let ((forms tactile-top-level-forms))
    (while (and forms (< (member-end (first forms)) (point)))
      (setq forms (rest forms)))
    (when (and forms (< (member-start (first forms)) (point)))
      (first forms))))

(defun tactile-get-form-at-point (&optional jump)
  "Finds and returns the form surrounding the point. Jump can be used to find the form surrounding
  that one or the one surrounding that, etc."
  (let* ((outer-form (tactile-get-top-level-form))
	 (search-area-start (if outer-form (member-start outer-form) (point-min)))
	 (search-area-end (if outer-form (member-end outer-form) (point-max))))
    (save-excursion
      (let ((begin nil)
	    (end nil)
	    (form-depth (or jump 0)))
	(when (at-unescaped-quote-p)
	  (backward-char))
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
	  (setq end (1+ (find-closing-paren search-area-end)))
	  (when end
	    (list begin end (buffer-substring-no-properties begin end) :form)))))))

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
	(end (1+ (find-closing-paren))))
    (goto-char end)
    (list start end (buffer-substring-no-properties start end) :form)))

(defun read-form-members (form)
  (save-excursion
    (when form
      (goto-char (1+ (member-start form)))
      (let ((members nil))
	(while (< (point) (1- (member-end form)))
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

(defun active-member ()
  (member-at-point tactile-current-form-depth))

(defmacro tactile-with-active-member (member-name &rest body)
  (declare (indent defun))
  `(let ((,(first member-name) (active-member)))
     (when ,(first member-name)
       ,@body)))

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

(defun trim-unecessary-form-spaces (form)
  (let ((real-point (point)))
    (save-excursion
      (combine-after-change-calls
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
	(indent-pp-sexp)))))

(defun tactile-on-change (x y z)
  (when (tactile-get-top-level-form)
    (trim-unecessary-form-spaces (tactile-get-top-level-form)))
  (setq tactile-quote-markers (tactile-get-quote-markers))
  (setq tactile-top-level-forms (tactile-find-top-level-forms)))

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

(defun tactile-highlight-atom-at-point ()
  (let ((member (member-at-point)))
    (when atom-at-point-overlay
      (delete-overlay atom-at-point-overlay))
    (when member
      (move-overlay atom-at-point-overlay (member-start member) (member-end member)))))

(defun tactile-highlight-active-member ()
  (let ((member (active-member)))
    (when active-member-overlay
      (delete-overlay active-member-overlay))
    (when member
      (move-overlay active-member-overlay (member-start member) (member-end member)))))

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
  (tactile-highlight-active-member))

(defun tactile-on-move ()
  (unless (= (point) tactile-last-point)
    (unless (or (equal last-command 'tactile-yank)
		(equal this-command 'tactile-yank))
      (setq tactile-kill-ring-last-yank nil))
    (reset-active-member)
    (setq tactile-last-point (point)))
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
      (insert 32)
      (unless (or prev member)
	(backward-char)))))

(defun tactile-start-new-member-reverse ()
  (interactive)
  (destructuring-bind (prev member next)
      (surrounding-three-members)
    (when (or prev member next)
      (goto-char (or (member-start (or member next)) (member-end prev)))
      (insert 32)
      (when (or member next)
	(backward-char)))))

(defun insert-parentheses ()
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (insert 40))
	  (:atom (combine-after-change-calls
		   (tactile-start-new-member)
		   (insert-parentheses))))
      (combine-after-change-calls
	(insert "()")
	(backward-char)))))

(defun handle-close-parentheses ()
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (insert 41))
	  (:atom (combine-after-change-calls
		   (goto-char (member-end (tactile-get-form-at-point)))
		   (tactile-start-new-member))))
      (combine-after-change-calls
	(goto-char (member-end (tactile-get-form-at-point)))
	(tactile-start-new-member)))))

(defun atom-to-string ()
  (let ((member (member-at-point)))
    (when (and member (equal (member-type member) :atom))
      (save-excursion
	(combine-after-change-calls
	 (goto-char (member-end member))
	 (insert 34)
	 (goto-char (member-start member))
	 (insert 34))))))

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
		       (insert "\"\"")
		       (backward-char))
		   (atom-to-string)))
	  (:string (cond ((> (1+ (point)) (member-end member))
			  (tactile-start-new-member))
			 ((= (point) (member-start member))
			  (forward-char))
			 (t
			  (insert "\\\"")))))
      (insert "\"\"")
      (backward-char))))

(defun tactile-delete-member (member)
  (combine-after-change-calls
    (goto-char (member-start member))
    (delete-char (- (member-end member) (member-start member)))
    (while (and (char-after) (= (char-after) 32))
      (delete-char 1))
    (while (and (char-before) (= (char-before) 32))
      (delete-char -1))
    (when (and (char-before) (not (= (char-before) 40))
	       (char-after) (not (= (char-after) 41)))
      (insert 32))))

(defun tactile-delete-active-member (&optional force)
  (interactive)
  (tactile-with-active-member (member)
    (when (or (not (equal (member-type member) :form))
	      force
	      (zerop (length (read-form-members form)))
	      (y-or-n-p "Really delete form?"))
      (tactile-delete-member member))))

(defun tactile-add-to-kill-ring (member)
  (when (>= (length tactile-kill-ring) tactile-kill-ring-size)
    (setq tactile-kill-ring (butlast tactile-kill-ring)))
  (push member tactile-kill-ring))

(defun tactile-add-active-member-to-kill-ring ()
  (interactive)
  (tactile-with-active-member (member)
    (tactile-add-to-kill-ring member)))

(defun tactile-kill-active-member ()
  (interactive)
  (tactile-with-active-member (member)
    (tactile-add-to-kill-ring member)
    (tactile-delete-member member)))

(defun tactile-insert-member (member &optional reversep)
  (destructuring-bind (prev memb next)
      (surrounding-three-members)
    (combine-after-change-calls
      (if reversep
	  (tactile-start-new-member-reverse)
	(tactile-start-new-member))
      (insert (member-text member)))))

(defun tactile-replace-active-member (new-member)
  (combine-after-change-calls
    (tactile-delete-active-member)
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
	   (tactile-replace-active-member (nth new-index tactile-kill-ring))
	   (setq tactile-kill-ring-last-yank (list (nth new-index tactile-kill-ring) new-index))))
	(t
	 (tactile-replace-active-member (first tactile-kill-ring))
	 (setq tactile-kill-ring-last-yank (list (first tactile-kill-ring) 0)))))

(defun reset-active-member ()
  (interactive)
  (setq tactile-current-form-depth nil))

(defun expand-active-member ()
  (interactive)
  (unless (equal (active-member) (tactile-get-top-level-form))
    (if tactile-current-form-depth
	(incf tactile-current-form-depth)
      (setq tactile-current-form-depth 0))))

(defun shrink-active-member ()
  (interactive)
  (when tactile-current-form-depth
    (if (zerop tactile-current-form-depth)
	(setq tactile-current-form-depth nil)
      (decf tactile-current-form-depth))))

(defun handle-backspace ()
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (cond ((or (= (member-start member) (point))
			      (= (+ (member-start member) 2) (member-end member)))
			  (tactile-delete-active-member))
			 ((or (= (member-start member) (1- (point)))
			      (= (member-end member) (point)))
			  (string-to-atom))
			 ((and (char-before) (= (char-before) 34))
			  (delete-char -2))
			 (t
			  (delete-char -1))))
	  (:atom (cond ((and (= (member-start member) (point))
			     (and (char-before) (= (char-before) 40)))
			(tactile-delete-active-member))
		       ((= (member-start member) (point))
			(tactile-delete-active-member))
		       (t
			(delete-char -1)))))
      (if (and (char-before) (= (char-before) 40))
	  (tactile-delete-active-member)
	(delete-char -1)))))

(defun handle-space ()
  (interactive)
  (let ((member (member-at-point)))
    (when member
      (cond ((not (or (= (member-start member) (point))
			   (= (member-end member) (point))))
	     (if (equal (member-type member) :string)
		 (combine-after-change-calls
		  (insert 32))
	       ;(combine-after-change-calls
		;(insert 92)
		;(insert 32))
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
  (setq active-member-overlay (make-overlay 0 0))

  (overlay-put top-level-overlay 'face 'highlight)
  (overlay-put active-member-overlay 'face 'region)

  (add-hook 'after-change-functions 'tactile-on-change nil t)
  (add-hook 'post-command-hook 'tactile-on-move nil t)

  (setq tactile-quote-markers (tactile-get-quote-markers))
  (setq tactile-top-level-forms (tactile-find-top-level-forms))
  (setq tactile-last-point (point))

  (define-key (current-local-map) (kbd "M-n") 'move-foreward)
  (define-key (current-local-map) (kbd "M-p") 'move-backward)
  (define-key (current-local-map) (kbd "M-N") 'shrink-active-member)
  (define-key (current-local-map) (kbd "M-P") 'expand-active-member)
  (define-key (current-local-map) (kbd "TAB") 'tactile-start-new-member)
  (define-key (current-local-map) (kbd "<backtab>") 'tactile-start-new-member-reverse)
  (define-key (current-local-map) (kbd "(") 'insert-parentheses)
  (define-key (current-local-map) (kbd ")") 'handle-close-parentheses)
  (define-key (current-local-map) (kbd "\"") 'handle-quote)
  (define-key (current-local-map) (kbd "<backspace>") 'handle-backspace)
  (define-key (current-local-map) (kbd "SPC") 'handle-space)
  (define-key (current-local-map) (kbd "C-c C-k") 'tactile-kill-active-member)
  (define-key (current-local-map) (kbd "C-c C-y") 'tactile-yank)

  (define-key (current-local-map) (kbd "C-q") 'switch-back)
  (viper-change-state-to-emacs))
