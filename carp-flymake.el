;;; carp-flymake.el --- A carp Flymake backend  -*- lexical-binding: t; -*-
;;; Commentary: Flymake backend using carp's --check command.
;;; Code:
(require 'flymake)

(defvar-local carp--flymake-proc nil
  "Variable storing the currently active flymake process running the carp checker.
This is used to determine if the current diagnostics are out of date.")

;;;###autoload
(defun carp-flymake (report-fn &rest _args)
  "Flymake backend for Carp syntax check.
REPORT-FN is Flymake’s callback function."
  ;; If the carp compiler is not installed signal an error
  (unless (executable-find "carp")
    (error "Cannot find a suitable version of the carp compiler"))
  ;; If there is already a check runnning kill it
  (when (process-live-p carp--flymake-proc)
    (kill-process carp--flymake-proc))

  ;; Save the current buffer, the narrowing restriction, remove any
  ;; narrowing restriction.
  ;;
  (let ((source (current-buffer)))
    (save-restriction
      (widen)
      ;; Reset the `carp--flymake-proc' process to a new process
      ;; calling the carp tool.
      ;;
      (setq carp--flymake-proc
	    (make-process
	     :name "carp-flymake" :noquery t :connection-type 'pipe
	     ;; Make output go to a temporary buffer.
	     :buffer (generate-new-buffer " *carp-flymake*")
	     :command (list "carp" "--check" (buffer-file-name source))
	     :sentinel
	     (lambda (proc _event)
	       ;; Check that the process has indeed exited, as it might
	       ;; be simply suspended.
	       (when (memq (process-status proc) '(exit signal))
		 (unwind-protect
		     ;; Only proceed if `proc' is the same as
		     ;; `carp--flymake-proc', which indicates that
		     ;; `proc' is not an obsolete process.
		     ;;
		     (if (with-current-buffer source (eq proc carp--flymake-proc))
			 (with-current-buffer (process-buffer proc)
			   (goto-char (point-min))
			   ;; Parse the output buffer for diagnostic's
			   ;; messages and locations, collect them in a list
			   ;; of objects, and call `report-fn'.

			   (cl-loop
			    while (search-forward-regexp
				   "^\\(?:.*.carp\\|-\\):\\([0-9]+\\):\\(.*\\)$"
				   nil t)
			    for msg = (match-string 2)
			    for (beg . end) = (flymake-diag-region
					       source
					       (string-to-number (match-string 1)))
			    for type = :error
			    collect (flymake-make-diagnostic source
							     beg
							     end
							     type
							     msg)
			    into diags
			    finally (funcall report-fn diags)))
		       (flymake-log :warning "Canceling obsolete check %s"
				    proc))
		   ;; Cleanup the temporary buffer used to hold the
		   ;; check's output.
		   (kill-buffer (process-buffer proc))))))))))

(defun carp-setup-flymake-backend ()
  "Add the flymake-backend to the `flymake-diagnostic-functions'."
  (add-hook 'flymake-diagnostic-functions 'carp-flymake nil t))

(provide 'carp-flymake)
;;; carp-flymake.el ends here
