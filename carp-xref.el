;;; carp-xref.el --- An xref backend for Carp  -*- lexical-binding: t; -*-
;;; Commentary: Xref backend using carp's REPL
;;; Code:

(require 'xref)
(require 'inf-carp-mode)

(defun carp-xref--query-symbol (process identifier)
  "Query the Repl PROCESS for informantion on a given IDENTIFIER."
  (let* ((comint-filt (process-filter process))
	 (kept        ""))
    (unwind-protect
	(progn (set-process-filter process
				   (lambda (_proc string)
				     (setq kept (concat kept string))))
	       (process-send-string process (concat
					     ":i "
					     identifier
					     "\n"))
	       (accept-process-output process 2)
	       kept)
      (set-process-filter process comint-filt))))

(defun carp-xref--find-symbol (identifier)
  "Look up a IDENTIFIER using carps Repl and register the location if possible with Xref."
  (interactive
   (list (thing-at-point 'symbol)))
  (list
   (let* ((carp-output (carp-xref--query-symbol
                        (inf-carp-proc) identifier)))
     (with-temp-buffer
       (insert carp-output)
       (if (search-backward-regexp
	    "Defined at line \\([0-9]+\\), column \\([0-9]+\\) in '\\(.*.carp\\)'"
	    nil t)
	   (let ((desc (match-string 0))
                 (line (match-string 1))
	         (column (match-string 2))
	         (file (match-string 3)))
             (xref-make desc
                        (xref-make-file-location
                         file
                         (string-to-number line)
                         (string-to-number column))))
         (error "Failed to find definition for %s" identifier))))))
;;;###autoload
(defun carp-xref-backend ()
  "Xref backend using Carp's REPL"
  'carp-xref)

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql carp-xref)))
  "Method used by xref to identify the current symbol at point."
  (let ((current-symbol (symbol-at-point)))
    (when current-symbol
      (symbol-name current-symbol))))

(cl-defmethod xref-backend-definitions ((_backend (eql carp-xref)) symbol)
  "Method used by xref to look up a SYMBOL."
  (carp-xref--find-symbol symbol))

(provide 'carp-xref)
;;; carp-xref.el ends here
