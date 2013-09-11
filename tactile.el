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
(defvar tactile-kill-ring-pointer nil)

(defvar-local top-level-overlay nil)
(defvar-local active-member-overlay nil)

(defvar-local tactile-last-point 0)

;;; Member manipulation and definition functions

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

(defun member-quoted (member)
  "Returns whether the member is quoted: \"'`\""
  (fifth member))

(defun member-start-unquoted (member)
  "Returns the buffer location of member's start, after any lisp quotes"
  (case (member-quoted member)
    (:unquote-list (+ 2 (member-start member)))
    ((:quote :backquote :unquote) (1+ (member-start member)))
    (t (member-start member))))

;;; Tactile Kill Ring management

(defun tactile-rotate-kill-ring ()
  "Cycles the kill ring pointer through the tactile kill ring. If the pointer is
  unset, set it to the first item in the ring."
  (if (and tactile-kill-ring-pointer
	   (< (1+ tactile-kill-ring-pointer) (length tactile-kill-ring)))
      (incf tactile-kill-ring-pointer)
    (setq tactile-kill-ring-pointer 0)))

(defun tactile-reset-kill-ring ()
  "Unset the kill ring pointer."
  (setq tactile-kill-ring-pointer nil))

(defun tactile-next-kill-ring ()
  "Returns the next item in the tactile kill ring, incrementing the pointer in the
  process."
  (tactile-rotate-kill-ring)
  (nth tactile-kill-ring-pointer tactile-kill-ring))

(defun tactile-peek-kill-ring ()
  "Return the next item from the tactile kill ring."
  (if (and tactile-kill-ring-pointer
	   (< (1+ tactile-kill-ring-pointer) (length tactile-kill-ring)))
      (nth (1+ tactile-kill-ring-pointer) tactile-kill-ring)
    (first tactile-kill-ring)))

(defun tactile-add-to-kill-ring (member)
  "Add member to the tactile kill ring."
  (when (>= (length tactile-kill-ring) tactile-kill-ring-size)
    (setq tactile-kill-ring (butlast tactile-kill-ring)))
  (funcall interprogram-cut-function (member-text member))
  (push member tactile-kill-ring))

;;; Form reading and manipulation

(defun point-equals (char &optional point)
  "Returns true if the char at point is equal to char"
  (and (char-after (or point (point)))
       (= (char-after (or point (point))) char)))

(defun quote-escaped-p (&optional point)
  "Returns true if the character under the point is a quote, and that quote
  is escaped by an odd number of backslashes."
  (let ((point (or point (point))))
    (save-excursion
      (goto-char point)
      (when (point-equals 34)
	(let ((slash-count 0))
	  (while (point-equals 92 (1- (point)))
	    (incf slash-count)
	    (backward-char))
	  (oddp slash-count))))))

(defun at-unescaped-quote-p ()
  "Returns true if the character under the point is a double quote and
  that quote is not escaped by an odd number of backslashes."
  (and (point-equals ?\")
       (not (quote-escaped-p))))

(defun get-best-quote-marker (&optional quote-markers)
  "Returns the quote marker which is furthest in the file, while still being
  before the point."
  (let ((quote-markers (or quote-markers tactile-quote-markers `((,(point-min-marker) nil)))))
    (while (< (point) (caar quote-markers))
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
		((and (point-equals 59)
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
    (let ((quote-markers (list `(,(point-min-marker) nil))))
      (goto-char 0)
      (while (> (count-lines (point) (point-max)) 10)
	(forward-line 10)
	(push (list (point-marker)
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
	  (when (and (point-equals 59)
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
	      ((point-equals 59) ;on comment
	       (forward-line))
	      ((and (zerop depth) (point-equals 41))
	       (setq location (point)))
	      ((point-equals 40)
	       (incf depth 1))
	      ((point-equals 41)
	       (decf depth 1))))
      location)))

(defun lisp-quoted (&optional point)
  "Returns whether the member beginning at point is lisp quoted, lisp unquoted,
   or otherwise."
  (cond ((point-equals 39 (1- (or point (point))))
	 :quote)
	((point-equals 96 (1- (or point (point))))
	 :backquote)
	((point-equals 44 (1- (or point (point))))
	 :unquote)
	((and (point-equals 64 (1- (or point (point))))
	      (point-equals 44 (- (or point (point)) 2)))
	 :unquote-list)))
	
(defun tactile-find-top-level-forms ()
  "Traverses the entire buffer looking for top level forms and returns a list of
  them in the order they were found."
  (save-excursion
    (let ((forms nil))
      (goto-char (point-min))
      (while (< (point) (point-max))
					;40 is open paren, 41 is close paren
	(when (and (point-equals 40)
		   (not (in-quotes-p)) (not (in-comment-p)))
	  (let ((form nil))
	    (case (lisp-quoted)
	      ((:quote :backquote :unquote) (push (copy-marker (1- (point))) form))
	      (:unquote-list (push (copy-marker (- (point) 2)) form))
	      (t (push (point-marker) form)))
	    (let ((end (copy-marker (1+ (find-closing-paren)))))
	      (if end ;If this form does not close, don't add it and finish parsing
		  (progn
		    (push end form)
		    (goto-char (1+ (first form)))
		    (push (buffer-substring-no-properties (second form) (first form)) form)
		    (push :form form)
		    (push (lisp-quoted (car (last form))) form)
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
	    (quoted nil)
	    (form-depth (or jump 0)))
	(when (at-unescaped-quote-p)
	  (backward-char))
	(when (in-quotes-p) 
	  (find-opening-quote)
	  (backward-char))
	(when (in-comment-p)
	  (goto-char (in-comment-p)))
	(when (and (or (point-equals 96) (point-equals 39) (point-equals 44)
		       (and (point-equals 64) (point-equals 44 (1- (point)))))
		   (point-equals 40 (1+ (point))))
	  (forward-char))
	(when (and (point-equals 44) (point-equals 64 (1+ (point))))
	  (forward-char 2))
	(unless (or (point-equals 41) (point-equals 34) (= (point) (point-max)))
	  (forward-char))
	(while (and (> (point) search-area-start) (not begin))
	  (backward-char)
	  (cond ((at-unescaped-quote-p)
		 (find-opening-quote))
		((and (point-equals 10) (in-comment-p))
		 (goto-char (in-comment-p)))
		((and (zerop form-depth) (point-equals 40))
		 (case (lisp-quoted)
		   ((:quote :backquote :unquote) (setq begin (copy-marker (1- (point)))))
		   (:unquote-list (setq begin (copy-marker (- (point) 2))))
		   (t (setq begin (point-marker))))
		 (setq quoted (lisp-quoted)))
		((point-equals 41)
		 (incf form-depth 1))
		((point-equals 40)
		 (decf form-depth 1))))
	(when begin
	  (setq end (copy-marker (1+ (find-closing-paren search-area-end))))
	  (when end
	    (list begin end (buffer-substring-no-properties begin end) :form quoted)))))))

;;; Atom reading and manipulation

(defun read-atom (&optional quote)
  "Reads in either a symbol or a number which starts at the point."
  (let ((start (- (point) (or (case quote ((:quote :unquote :backquote) 1) (:unquote-list 2)) 0)))
	(end (1- (re-search-forward "[\n\s\t()]"))))
    (goto-char end)
    (list (copy-marker start) (copy-marker end) 
	  (buffer-substring-no-properties start end) :atom quote)))

(defun read-str (&optional quote)
  "Reads in a string starting at the point."
  (let ((start (- (point) (or (case quote ((:quote :unquote :backquote) 1) (:unquote-list 2)) 0))))
    (find-closing-quote)
    (forward-char)
    (list (copy-marker start) (point-marker)
	  (buffer-substring-no-properties start (point)) :string quote)))

(defun read-form (&optional quote)
  "Reads in a form starting at the point."
  (let ((start (- (point) (or (case quote ((:quote :unquote :backquote) 1) (:unquote-list 2)) 0)))
	(end (1+ (find-closing-paren))))
    (goto-char end)
    (list (copy-marker start) (copy-marker end) 
	  (buffer-substring-no-properties start end) :form quote)))

(defun read-member (&optional quote)
  "With the point at the beginning of a member, either an atom or a form, read in the member
  and return a member object describing it. Detects whether the member is quoted and delegates
  the rest to one of the read-x functions."
  (when (char-after)
    (case (char-after)
      (34 (read-str quote))
      (40 (read-form quote))
      (96 (forward-char)
	  (read-member :backquote))
      (39 (forward-char)
	  (read-member :quote))
      (44 (forward-char)
	  (read-member :unquote))
      (64 (if (equal quote :unquote)
	      (progn
		(forward-char)
		(read-member :unquote-list))
	    (read-atom quote)))
      (t (read-atom quote)))))

(defun read-form-members (form)
  "Returns a list of all members of *form*."
  (unless undo-in-progress
    (save-excursion
      (when form
	(goto-char (1+ (member-start form)))
	(when (member-quoted form)
	  (forward-char))
	(let ((members nil))
	  (while (< (point) (1- (member-end form)))
	    (push (read-member) members)
	    (re-search-forward "[^\n\s\t]")
	    (backward-char))
	  (nreverse members))))))

(defun in-which-member (members point &optional include-nearest-p)
  "Returns the member of *members* in which *point* is located."
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
  "Returns the member in which the point is located. If *jump* is non-nil, this function
  'goes-up' *jump* levels and returns a surrounding form."
  (let ((form (tactile-get-form-at-point (or jump 0))))
    (if jump
	form
      (when form
	(in-which-member (read-form-members form) (point))))))

(defun active-member ()
  "Returns the active member, which is the member at point zoomed out to the
  current selection depth."
  (member-at-point tactile-current-form-depth))

(defmacro tactile-with-active-member (member-name &rest body)
  "Evaluate *body* with *member-name* bound to the current active member."
  (declare (indent defun))
  `(let ((,(first member-name) (active-member)))
     (when ,(first member-name)
       ,@body)))

(defun surrounding-three-members (&optional jump)
  "Returns the three nearest members surrounding the point. If jump is non-nil,
  returns surrounding forms instead of atoms."
  (let* ((form (tactile-get-form-at-point (or jump 0)))
	 (members (if form (read-form-members form) tactile-top-level-forms))
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

(defun trim-member-whitespace (member &optional recursive real-point)
  "Removes unnecessary trailing white space from *member*. If *recursive*,
  and *member* is a form, also trims form members."
  (goto-char (member-end member))
  (while (and (char-after) (or (= (char-after) 32) (= (char-after) 9))
	      (not (= (point) (or real-point -1)))
	      (not (= (1+ (point)) (or real-point -1))))
    (delete-char 1))
  (unless (and (char-after) (or (= (char-after) 32) (= (char-after) 41) (= (char-after) 9)))
    (insert-before-markers 32))
  (when (and recursive (equal (member-type member) :form))
    (tactile-balance-form-whitespace member recursive real-point)))

(defun tactile-balance-form-whitespace (form &optional recursive real-point)
  "Removes unnecessary white space from the interior of *form*."
  (let ((real-point (or real-point (point-marker))))
    (save-excursion
      (combine-after-change-calls
	(goto-char (1+ (member-start form)))
	(while (and (char-after) (or (= (char-after) 32) (= (char-after) 9)))
	  (delete-char 1))
	(mapcar (lambda (member) (trim-member-whitespace member recursive real-point))
		(nreverse (read-form-members form)))))))

(defun tactile-pretty-print-form (form)
  "Rebalances the white space and indentation in *form*."
  (let ((real-point (point)))
    (save-excursion
      (combine-after-change-calls
	(tactile-balance-form-whitespace form t)
	(goto-char (member-start (tactile-get-top-level-form)))
	(indent-pp-sexp)))))

(defun tactile-on-change (x y z)
  "This function is run every time a change is made to the text. It pretty prints the
  current top-level form, resets the quote markers, and resets the top level forms list."
  (unless undo-in-progress
    (if-let (top-level-form (tactile-get-top-level-form))
      (tactile-pretty-print-form top-level-form))
    (setq tactile-quote-markers (tactile-get-quote-markers))
    (setq tactile-top-level-forms (tactile-find-top-level-forms))))

(defun tactile-highlight-atom-at-point ()
  "Highlights the atom at point."
  (let ((member (member-at-point)))
    (when atom-at-point-overlay
      (delete-overlay atom-at-point-overlay))
    (when member
      (move-overlay atom-at-point-overlay (member-start member) (member-end member)))))

(defun tactile-highlight-active-member ()
  "Highlights the active member."
  (let ((member (active-member)))
    (when active-member-overlay
      (delete-overlay active-member-overlay))
    (when member
      (move-overlay active-member-overlay (member-start member) (member-end member)))))

(defun tactile-highlight-top-level-form ()
  "Highlights the current top level form."
  (let ((form (tactile-get-top-level-form)))
    (when top-level-overlay
      (delete-overlay top-level-overlay))
    (when form
      (move-overlay top-level-overlay (member-start form) (1+ (member-end form))))))

(defun tactile-highlight-selected-form ()
  "Highlights the form at point."
  (let ((form (tactile-get-form-at-point tactile-current-form-depth)))
    (when at-point-overlay
      (delete-overlay at-point-overlay))
    (when form
      (move-overlay at-point-overlay (member-start form) (1+ (member-end form))))))

(defun highlight-forms ()
  "Highlight all the forms and members that need highlighting."
  (tactile-highlight-top-level-form)
  (tactile-highlight-active-member))

(defun tactile-on-move ()
  "Run every time a command happens. Handles much of tactile's inner plumbing,
  like keeping the kill ring up to date, clearing the active member, and resetting
  the highlighting."
  (unless undo-in-progress
    (unless (= (point) tactile-last-point)
      (unless (or (equal last-command 'tactile-yank)
		  (equal this-command 'tactile-yank)
		  (equal last-command 'tactile-yank-again)
		  (equal this-command 'tactile-yank-again))
	(tactile-reset-kill-ring))
      (reset-active-member)
      (setq tactile-last-point (point-marker)))
    (highlight-forms)))

(defun goto-member (member &optional reversep)
  "Moves the point to the beginning of member."
  (case (member-type member)
    (:atom (goto-char (member-end member)))
    (:string (goto-char (1- (member-end member))))
    (:form (enter-member member reversep))))

(defun enter-member (member reversep)
  "Assuming member is a form, moves the point to the first (or last)
  member of the form."
  (let* ((members (read-form-members member))
	 (inner-member (if reversep (first (last members)) (first members))))
    (if inner-member
	(goto-member inner-member reversep)
      (goto-char (member-end member)))))

(defun navigate-atoms (reversep &optional jump)
  "Moves point either to the next or the previous atomic member, jumping in or out
  of forms as necessary."
  (destructuring-bind (prev member next)
      (surrounding-three-members jump)
    (let ((next-member (if reversep prev next)))
      (if next-member 
	  (goto-member next-member reversep)
	(navigate-atoms reversep (1+ (or jump 0)))))))

(defun move-foreward () "Navigate to next member." (interactive) (navigate-atoms nil))
(defun move-backward () "Navigate to previous member." (interactive) (navigate-atoms 't))

(defun tactile-start-new-member ()
  "Add extra space between the current member and the next one, and
  moves the point between the two so a new member can be typed."
  (interactive)
  (destructuring-bind (prev member next)
      (surrounding-three-members)
    (when (or prev member next)
      (goto-char (or (member-end (or member prev)) (member-start next)))
      (if (equal (or member prev) (tactile-get-top-level-form))
	  (insert 10 10)
	(insert 32))
      (unless (or prev member)
	(backward-char)))))

(defun tactile-start-new-member-reverse ()
  "Add extra space between the current member and the previous one, and
  moves the point between the two so a new member can be typed."
  (interactive)
  (destructuring-bind (prev member next)
      (surrounding-three-members)
    (when (or prev member next)
      (goto-char (or (member-start (or member next)) (member-end prev)))
      (insert 32)
      (when (or member next)
	(backward-char)))))

(defun insert-parentheses ()
  "Starts a new member and inserts a new pair of parentheses, unless point is in a string,
  in which case, a literal parenthesis is inserted."
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (insert 40))
	  (:atom (combine-after-change-calls
		   (if (lisp-quoted)
		       (progn
			 (insert "()")
			 (backward-char))
		     (tactile-start-new-member)
		     (insert-parentheses)))))
      (combine-after-change-calls
	(insert "()")
	(backward-char)))))

(defun handle-close-parentheses ()
  "Moves the point to the end and outside of the current form and starts a new member,
  unless point is in a string, in which case a literal parenthesis is inserted."
  (interactive)
  (let ((member (member-at-point)))
    (if member
	(case (member-type member)
	  (:string (insert 41))
	  (:atom (combine-after-change-calls
		   (let ((form (tactile-get-form-at-point)))
		     (goto-char (member-end form))
		     (tactile-start-new-member)
		     (tactile-balance-form-whitespace form))))
	  (:form (combine-after-change-calls
		   (goto-char (1+ (member-end member)))
		   (tactile-start-new-member))))
      (combine-after-change-calls
	(let ((form (tactile-get-form-at-point)))
	  (goto-char (member-end form))
	  (tactile-start-new-member)
	  (tactile-balance-form-whitespace form))))))

(defun atom-to-string ()
  "Wraps member at point with quotes, turning it into a string."
  (let ((member (member-at-point)))
    (when (and member (equal (member-type member) :atom))
      (save-excursion
	(combine-after-change-calls
	 (goto-char (member-end member))
	 (insert 34)
	 (goto-char (member-start member))
	 (insert 34))))))

(defun string-to-atom ()
  "Deletes the quotes at the beginning and end of the string at point
  turning it either into a symbol or a number."
  (let ((member (member-at-point)))
    (when (and member (equal (member-type member) :string))
      (save-excursion
	(combine-after-change-calls
	 (goto-char (member-end member))
	 (delete-char -1)
	 (goto-char (member-start member))
	 (delete-char 1))))))

(defun handle-quote ()
  "When a quote is pressed, either turn the current member into a string, or
  insert an escaped quote if currently in a string"
  (interactive)
  (combine-after-change-calls
    (let ((member (member-at-point)))
      (if member
	  (case (member-type member)
	    (:atom (if (= (point) (member-end member))
		       (progn
			 (tactile-start-new-member)
			 (insert "\"\"")
			 (backward-char))
		     (atom-to-string)))
	    (:string (cond ((and (point-equals 92 (1- (point)))
				 (point-equals 92 (- (point) 2)))
			    (delete-char -2)
			    (insert "\\\""))
			   ((>= (1+ (point)) (member-end member))
			    (tactile-start-new-member))
			   ((= (point) (member-start member))
			    (forward-char))
			   (t
			    (insert "\\\"")))))
	(insert "\"\"")
	(backward-char)))))

(defun handle-backslash ()
  "When a backslash is press it is inserted, unless it is in a string, in which
  case a double backslash is inserted."
  (interactive)
  (combine-after-change-calls
    (let ((member (member-at-point)))
      (if member
	  (case (member-type member)
	    (:string (unless (or (>= (point) (member-end member))
				 (<= (point) (member-start member)))
		       (insert "\\\\")))
	    (t (insert 92)))
	(insert 92)))))

(defun tactile-delete-member (member)
  "Deletes *member* from the file."
  (combine-after-change-calls
    (goto-char (member-start member))
    (delete-char (- (member-end member) (member-start member)))
    (while (point-equals 32)
      (delete-char 1))
    (while (point-equals 32 (1- (point)))
      (delete-char -1))
    (when (and (char-before) (not (= (char-before) 40))
	       (char-after) (not (= (char-after) 41)))
      (insert 32)
      (backward-char))))

(defun tactile-delete-active-member (&optional force)
  "Deletes the active member. If active member is a form with one or more
  members and force is nil, confirm with the user first."
  (interactive)
  (tactile-with-active-member (member)
    (when (or (not (equal (member-type member) :form))
	      force
	      (zerop (length (read-form-members form)))
	      (y-or-n-p "Really delete form?"))
      (tactile-delete-member member))))

(defun tactile-save-active-member ()
  "Push the active member to the tactile kill ring."
  (interactive)
  (tactile-with-active-member (member)
    (tactile-add-to-kill-ring member)))

(defun tactile-kill-active-member ()
  "Delete the active member and add it to the tactile kill ring."
  (interactive)
  (tactile-with-active-member (member)
    (tactile-add-to-kill-ring member)
    (tactile-delete-member member)))

(defun tactile-insert-member (member &optional reversep)
  "Inserts *member* just after the member at the point."
  (destructuring-bind (prev memb next)
      (surrounding-three-members)
    (combine-after-change-calls
      (if reversep
	  (tactile-start-new-member-reverse)
	(tactile-start-new-member))
      (insert (member-text member)))))

(defun tactile-replace-active-member (new-member)
  "Replace the active member inline with *new-member*."
  (combine-after-change-calls
    (tactile-delete-active-member)
    (tactile-insert-member new-member)))

(defun tactile-cycle-kill-ring ()
  "Cycles through the members of the kill ring for future yanks."
  (interactive)
  (tactile-rotate-kill-ring)
  (message (member-text (tactile-peek-kill-ring))))

(defun tactile-yank (&optional reversep)
  "Inserts the first member in the tactile kill ring after the member at point."
  (interactive)
  (when (equal last-command 'tactile-yank)
    (tactile-reset-kill-ring))
  (tactile-insert-member (tactile-next-kill-ring) reversep))

(defun tactile-yank-again ()
  "Replace the active member with the next member in the tactile kill ring."
  (interactive)
  (tactile-replace-active-member (tactile-next-kill-ring)))

(defun reset-active-member ()
  "Reverts the active member to the member at point."
  (interactive)
  (setq tactile-current-form-depth nil))

(defun expand-active-member ()
  "Expands the active member to the form surrounding the current active member."
  (interactive)
  (unless (equal (active-member) (tactile-get-top-level-form))
    (if tactile-current-form-depth
	(incf tactile-current-form-depth)
      (setq tactile-current-form-depth 0))))

(defun shrink-active-member ()
  "Shrinks the active member to the member of the current active member at the point."
  (interactive)
  (when tactile-current-form-depth
    (if (zerop tactile-current-form-depth)
	(setq tactile-current-form-depth nil)
      (decf tactile-current-form-depth))))

(defun handle-backspace ()
  "Delete a char if in a member, but delete the whole member if at the beginning of the member."
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
			 ((and (or (point-equals 92 (- (point) 1))
				   (point-equals 34 (- (point) 1)))
			       (point-equals 92 (- (point) 2)))
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
      (if (point-equals 40 (1- (point)))
	  (if (point-equals 41)
	      (tactile-delete-member (tactile-get-form-at-point))
	    (tactile-delete-active-member))
	(delete-char -1)))))

(defun handle-space ()
  "Start a new member if at end of member, else if in string, enter space."
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

(defun quote-active-member (quote-type)
  "Cycles through the lisp quoting of the active member. Will either convert to a new *quote-type*
  or will remove the lisp quoting altogether."
  (combine-after-change-calls
    (cl-flet ((toggle-quotes ()
			     (goto-char (member-start member))
			     (case (member-quoted member)
			       ((:quote :backquote :unquote) (delete-char 1))
			       (:unquote-list (delete-char 2)))
			     (case quote-type
			       (:quote (unless (equal (member-quoted member) :quote)
					 (insert 39)))
			       (:backquote (unless (equal (member-quoted member) :backquote)
					     (insert 96)))
			       (:unquote (case (member-quoted member)
					   (:unquote-list)
					   (:unquote (insert 44 64))
					   (t (insert 44)))))))
      (let ((member (active-member)))
	(if (or (not member)
		(equal (member-type member) :string))
	    (unless (and member
			 (or (<= (point) (member-start member))
			     (>= (point) (member-end member))))
	      (case quote-type
		(:quote (insert 39))
		(:backquote (insert 96))
		(:unquote (insert 44))))
	  ; Emacs has a problem whereby if the character which the point starts
	  ; at during a save-excursion is deleted, save-excursion fails to restore
	  ; the character to that point, so we need this awkward construct:
	  (if (<= (point) (member-start-unquoted member))
	      (toggle-quotes)
	    (save-excursion (toggle-quotes))))))))

(defun tactile-quote ()
  "Toggle active member quoting."
  (interactive)
  (quote-active-member :quote))

(defun tactile-backquote ()
  "Toggle active member backquoting."
  (interactive)
  (quote-active-member :backquote))

(defun tactile-unquote ()
  "Cycle active member unquoting."
  (interactive)
  (quote-active-member :unquote))

(defun toggle-tactile () 
  "Toggles between tactile and regular emacs lisp mode."
  (interactive) 
  (remove-overlays) 
  (emacs-lisp-mode)
  (define-key (current-local-map) (kbd "C-c C-q") 'tactile-mode))

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
  (define-key (current-local-map) (kbd "\\") 'handle-backslash)
  (define-key (current-local-map) (kbd "<backspace>") 'handle-backspace)
  (define-key (current-local-map) (kbd "SPC") 'handle-space)

  (define-key (current-local-map) (kbd "C-c C-k") 'tactile-kill-active-member)
  (define-key (current-local-map) (kbd "C-c C-w") 'tactile-save-active-member)
  (define-key (current-local-map) (kbd "C-c C-y") 'tactile-yank)
  (define-key (current-local-map) (kbd "C-c M-y") 'tactile-yank-again)
  (define-key (current-local-map) (kbd "C-c C-M-y") 'tactile-cycle-kill-ring)

  (define-key (current-local-map) (kbd "'") 'tactile-quote)
  (define-key (current-local-map) (kbd "`") 'tactile-backquote)
  (define-key (current-local-map) (kbd ",") 'tactile-unquote)

  (define-key (current-local-map) (kbd "C-c C-q") 'switch-back)
  (viper-change-state-to-emacs))
