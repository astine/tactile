Tactile: A True Structural Editor for Emacs Lisp
================================================

Lisp, unlike most programming languages, explicitly exposes the structure of it programs in its syntax. This makes lisp code very easy to parse, read, and operate on programmatically. Yet, there are very few editors/coding environments for lisp which actually make extensive use of this feature. _Tactile_ is an attempt to make an editor for Emacs lisp which is truly structural and enables edits against the structured lisp code rather than the text that makes it up.

Features
--------

_Tactile_ is still very early in its development, but already it:

 - Balances parentheses, quotes, and keeps your from breaking the structure of your code
 - Partially auto pretty prints code non-disruptively as you type it
 - Allows you to quickly move through your program by atom or s-expression
 - Allows you to quickly select, copy, cut, and paste atoms and s-expressions
 
Future features should include:
 
 - Mouse interaction
 - Full pretty printing
 - Intelligent auto completion 
 - *Many* refactoring facilities.

Install
-------

Copy tactile.el to your `.emacs.d` directory and copy this to your `.emacs`:

    (autoload 'tactile-mode "tactile" "Load tactile emacs lisp mode" t)
    
Then use `M-x tactile-mode` to load an elisp file into tactile-mode.

Usage
-----

_Tactile_ is different from most editing modes in that it attempts to prevent you from treating your code simply as text. Emacs lisp code is a list structure. As you type in code, _Tactile_ will auto format and auto balance your code, keeping it valid and preserving the list structure. You can navigate that structure by pressing `M-n` and `M-p` to move by the lisp object. 

By default, the lisp atom at the point is selected, but you can select the surrounding form by pressing `M-P`. You can repeat that call to select the next surrounding form and so forth until you reach the top level form. `M-N` will reverse `M-P`. This selection is called the active member. You can cut the kill the active member with `C-c C-k` and then yank it with `C-c C-y`. Pressing `'`, `` ` ``, or `,` will cycle through various states of lisp quoting and unquoting.

If you find that _Tactile_ gets in your way at any point, you can press `C-c C-q` to switch to ordinary Emacs lisp mode. `C-c C-q` will get you back to tactile mode again when you want it.

Here is the entire current keymap for reference:

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

