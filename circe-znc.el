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

;; not yet needed
;; (require 'subr-x)



(defvar circe-znc-status-table
  (let ((hash-table (make-hash-table :test #'equal)))
    (puthash "help" (lambda () (message "Test passed!")) hash-table))
  "")



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

;; (defun circe-znc-module-help ()
;;   "Prompt for a module, call the help function of that modules table."
;;   (interactive)
;;   (let* ((module (completing-read "Module\: "
;;                                   (hash-table-keys circe-znc-modules-table)
;;                                   nil
;;                                   t))
;;          (module-table (gethash module circe-znc-modules-table)))
;;     (funcall (gethash "help" module-table))))

(defvar circe-znc-collect-timeout 10
  "Number of seconds to wait before deactivating listener for some ZNC output.
Raise this if output is cut off, lower this ")
;; (defun circe-znc--collect-response-in-buf ()
  
         
(provide 'circe-znc)
;;; circe-znc.el ends here
