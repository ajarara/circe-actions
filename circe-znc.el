;;; -*- lexical-binding: t -*-

;;; circe-znc.el --- A convenient interface to *controlpanel

;; Author: alphor
;; Version: 0.0.1
;; Keywords: circe znc

;; This set of functions requires you to have the controlpanel module enabled.
;; The neat thing about the controlpanel module is that it allows you to message
;; commands that modify configuration that would require you to use the web front
;; end. This is a little tedious. But the syntax for controlpanel is also kind of
;; tedious.
;; These functions make it very easy to do what you want.

(require 'circe-actions)
(require 'subr-x)

(defvar circe-znc-status-table
  (let ((hash-table (make-hash-table :test #'equal)))
    (puthash "help" (lambda () (message "Test passed!")) hash-table))
  "")


(defvar circe-znc-collect-timeout 10
  "Number of seconds to wait before deactivating listener for some ZNC output.
Raise this if output is cut off (ie missing help messages)")

(defvar circe-znc-controlpanel-table
  (let ((hash-table (make-hash-table :test #'equal)))
    (puthash "help" (lambda () (message "Test failed!")) hash-table)
    hash-table)
  "")


;; ;; it's kind of silly to make this a hash table if I query for the list everytime.
;; ;; the only time I would keep it a hash table is if the results were themselves
;; ;; interactive functions.
(defvar circe-znc-modules-table
  (let ((hash-table (make-hash-table :test #'equal)))
    (puthash "*controlpanel" circe-znc-controlpanel-table hash-table)
    (puthash "*status" circe-znc-status-table hash-table)
    hash-table)
  "A top level hash table linking modules to their options defined in the last version of ZNC (1.6.3).")

(defun circe-znc-module-help ()
  "Prompt for a module, call the help function of that modules table."
  (interactive)
  (let* ((module (completing-read "Module\: "
                                  (hash-table-keys circe-znc-modules-table)
                                  nil
                                  t))
         (module-table (gethash module circe-znc-modules-table)))
    (funcall (gethash "help" module-table))))


(defun circe-znc--collect-response-in-buf (bufname string)
  "if no buffer, create it and display it.

   Insert string at end of buffer."
  (let ((buffer (circe-znc--get-buffer-create bufname)))
    (with-current-buffer buffer
      (let ((buffer-read-only nil))
        (insert string)))
    ;; pop-to-buffer instead?
    (display-buffer buffer)))

 (defun circe-znc--get-buffer-create (bufname)
   "If circe-znc--output-stale is set, clear the buffer.
 This means that the associated handler has been deactivated!
 (An alternate implementation might be to associate the symbol as a
 buffer-local variable and simply check if the assoc handler is
 deactivated.)
 Otherwise simply return the buffer."
   (if (get-buffer bufname) ; returns nil if it doesn't exist or killed
       (with-current-buffer bufname
         (if (funcall circe-znc--is-live-p) ; is buffer's handler still alive?
             bufname 
           (kill-buffer bufname)
           (circe-znc--get-buffer-create bufname)))
     (let ((newbuf (generate-new-buffer bufname)))
       (with-current-buffer newbuf
         (circe-znc-output-mode)
         (setq-local circe-znc--is-live-p (lambda () nil)) ; initialize live-p
         newbuf))))
       
(define-derived-mode circe-znc-output-mode
  special-mode
  "*ZNC Command Output*"
  "a (very) thin wrapper around special-mode")

(provide 'circe-znc)
;;; circe-znc.el ends here
