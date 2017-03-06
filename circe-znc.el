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
    (puthash "help" (lambda () (message "Test passed!")) hash-table)
    hash-table)
  "")


(defvar circe-znc-collect-timeout 10
  "Number of seconds to wait before deactivating listener for some
  extended ZNC output (AKA to a buffer).  Raise this if output is cut
  off (ie missing help messages). Lower this if subsequent in")

(defvar circe-znc-controlpanel-table
  (let ((hash-table (make-hash-table :test #'equal)))
    (puthash "help" (lambda () (message "Test failed!")) hash-table)
    hash-table)
  "Do NOT access this directly. Instead, use `circe-znc-get-command'")

(defvar circe-znc-modules-table
  (let ((hash-table (make-hash-table :test #'equal)))
    (puthash "*controlpanel" circe-znc-controlpanel-table hash-table)
    (puthash "*status" circe-znc-status-table hash-table)
    hash-table)
  "A top level hash table linking modules to their options defined in the last version of ZNC (1.6.3). Do not access this directly! Instead use `circe-znc-get-command-table'. This is to allow for modification of this table down the line.")

(defun circe-znc-get-command-table (module-name)
  "Get the hash table of a specific ZNC module, given its name."
  
  (gethash module-name circe-znc-modules-table))

(defun circe-znc-get-command (module-name command)
  "Get appropriate command from module, given its name. Usage:
  (circe-znc-get-command \"*status\" \"broadcast\")
  => (lambda (&optional broadcast-string) ...)"
  
  (let ((module-commands (gethash module-name circe-znc-modules-table)))
    (gethash command module-commands)))
     
(defun circe-znc-module-help (&optional module)
  "Prompt for a module, call the help function of that modules table."
  (interactive)
  ;; should I wrap this and the next let-expr in a let*?
  ;; it's for sure uglier looking
  (when (not module)
    (setq module (completing-read "Module\: "
                                  (hash-table-keys circe-znc-modules-table)
                                  nil
                                  t)))
  (funcall (gethash "help"
                    (circe-znc-get-command-table module))))


(defun circe-znc--collect-response-in-buf (bufname string)
  "if no buffer, create it and display it.

   Insert string at end of buffer."
  (let ((buffer (circe-znc--get-buffer-create bufname)))
    (with-current-buffer buffer
      ;; is this the right way to output to a read only buffer?
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
       
(defvar circe-znc--help-sentinels
  (let ((table (make-hash-table :test #'equal)))
    ;; thank computer jesus for re-builder
    (puthash "*controlpanel" '(:re "^+=\+\\+=\+\\+$" :times 3) table)
    table)
  "set of conditions that determine if help output has ceased. As of now only supports :re and :times as conditions, eventual support for custom timeouts.")

(defun circe-znc--deactivation-p-gen (module-name)
  "given a module-name, look up the sentinel in
  `circe-znc--help-sentinels' and generate a closure for it, returning
  true when the sentinel's :re and :times is satisfied.

Planned: generate get time of closure generation. If time differs by circe-znc-collect-timeout, return true."
  (let* ((sentinel (gethash module-name circe-znc--help-sentinels))
         (re (plist-get sentinel :re))
         (times (plist-get sentinel :times))
         (matches 0))
    (lambda (string)
      (if (string-match-p re string)
          (if (<= times matches)
              t
            (setq matches (1+ matches)))))))

(define-derived-mode circe-znc-output-mode
  special-mode
  "*ZNC Command Output*"
  "a (very) thin wrapper around special-mode")

(provide 'circe-znc)
;;; circe-znc.el ends here
