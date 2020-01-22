;;; This file is used to make the index file for the OBVIUS
;;; documentation.  It reads as input the file "main.idx", produced by
;;; the \makeindex command in LaTeX.  It creates an index file called
;;; "index.tex" that can be included in the final document.

;; UNFINISHED: use the sort-fields, sort-regexp-fields, sort-lines commands
(let* ((doc-dir "/v/obvius-2.1/doc/") infile outfile names page)
  (setq outfile (find-file-noselect (concat doc-dir "index.tex")))
  (set-buffer outfile)
  (erase-buffer)			
  (setq infile (find-file-noselect (concat doc-dir "main.idx")))
  (set-buffer infile)
  (beginning-of-buffer)
  (while (re-search-forward "indexentry" nil 'move)
    (setq names
	  (buffer-substring (point) (progn (forward-sexp 1) (point))))
    (setq page
	  (buffer-substring (point) (progn (forward-sexp 1) (point))))
    (save-excursion
      
      (set-buffer outfile)
      (insert names)
      (insert "\n")		;make sure there's a trailing newline
      (insert page))
    ))
  
