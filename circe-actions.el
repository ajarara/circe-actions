;;; -*- lexical-binding: t -*-

;; Utility functions for interfacing with circe-irc-handler-table
(require 'irc)
(require 'circe)


(defgroup circe-actions nil
  "Convenient interface to circe events"
  :group 'convenience)

(defcustom circe-actions-maximum-handlers 3
  "Do not allow more than this many active handlers. This number is
   compared against circe-actions-handlers-alist's length. Once it is
   greater than or equal to the length of the alist, ignore any
   requests to add more to the queue, instead alert the user with a
   message."
  :group 'circe-actions
  :type 'integer)

(defvar circe-actions-handlers-alist '()
  "Store all the symbols of the generated event handlers here. The
  symbols assigned to any circe-action should be uninterned so that
  they do not pollute the function namespace (as an arbitrary number
  are generated)

  The values corresponding to the symbols are the handlers they are on.")

;; circe-actions-generate-handler-function returns functions that are
;; primarily designed to deal with irc.message-like events (irc.ctcp
;; is also included). Later on it may be necessary to change this.

(defconst circe-actions-version "0.0.13")
  
;; should be set to nil and populated on circe-actions-enable?
(defvar circe-actions-event-plists
  (let ((hash-table (make-hash-table))
	(default-event-signature (list :server-proc :event :fq-username :target :contents)))
    (puthash "irc.message" default-event-signature hash-table)
    ))


(defun circe-actions-generate-handler-function
    (condition-p-function action-function symbol event &optional persist)
  "Produce (and return) a procedure aliased to a SYMBOL that executes
  ACTION-FUNCTION when CONDITION-P-FUNCTION

SYMBOL should be uninterned, but doesn't have to be. This is not the
same symbol(s) passed to circe-actions-register.
EVENT is a string key, like irc.message obtained from circe-irc-handler-table

if PERSIST is non-nil, do not remove the symbol from the handler
obtained from circe-actions-handlers-alist, do not remove the handler
from (circe-irc-handler-table), do not pass go.

PERSIST is a dangerous flag to set. If you pass in an expensive
condition or action, paired with a high occurence rate event, your
emacs system will slow to a crawl, and the only way to deactivate it
is through an interactive circe-actions-deactivate-function call, or
by calling circe-actions-panic, which deactivates all handlers
indiscriminately.

CONDITION-P-FUNCTION and ACTION-FUNCTION must be procedures that have the same event signature as the event it handles, as described in circe-actions-event-plists. In the case of \"irc.message\", it should either take in a list of arguments (to be processed by `circe-actions-plistify')

server-proc - usually the result of (circe-server-process)
event - the event, ie irc.message or a ctcp ping
fq-username - the username followed by cloak in whois format
channel - channel or, if channel is your nick, a query
contents - text payload of the event"
  (defalias symbol
    (lambda (server-proc event &rest rest-args)
      (let ((args (cons server-proc (cons event rest-args))))
	(when (apply condition-p-function args)
	  (unless persist
	    (circe-actions-deactivate-function symbol event))
	  (apply action-function args))))))

(defun circe-actions-deactivate-function (handler-function event)
  "Remove HANDLER-FUNCTION from EVENT bucket in circe-irc-handler-table, and remove it from the alist, in that order."
  (irc-handler-remove (circe-irc-handler-table)
		      event
		      handler-function)
  (setq circe-actions-handlers-alist
	(delete (assoc handler-function circe-actions-handlers-alist)
		circe-actions-handlers-alist)))
	
(defun circe-actions-activate-function (handler-function event)
  "Given a HANDLER-FUNCTION created by
  circe-actions-generate-handler-function, get the symbol associated
  with it. If the length of circe-actions-handlers-alist exceeds
  circe-actions-maximum-handlers, message the user the length of the
  list and the symbol of the handler-function that was attempted to be
  activated.

Otherwise, add the HANDLER-FUNCTION to the
circe-actions-handlers-alist (with a key of symbol and HANDLER), then
place it at event in the hash-table obtained from circe's irc handler table."
  (let ((alist-len (length circe-actions-handlers-alist)))
    (if (>= alist-len circe-actions-maximum-handlers)
	(warn "circe-actions: Handler tolerance of %s exceeded, nothing added to %s! Change " alist-len event)
      (progn
	;; add the handler-function to the list
	(setq circe-actions-handlers-alist
	      (cons (list handler-function event) circe-actions-handlers-alist))
	;; add the handler-function to the event table. The function
	;; is now called everytime event occurs.
	(irc-handler-add (circe-irc-handler-table)
			 event
			 handler-function)))))

(defun circe-actions--gensym ()
  (gensym "circe-actions-gensym-"))

(defun circe-actions-register (condition-p-function action-function event &optional persist)
  "Given a CONDITION-P-FUNCTION and ACTION-FUNCTION that takes args
  consistent with the EVENT passed (as shown in the README.)

1) generate a procedure that executes ACTION-FUNCTION when CONDITION-P-FUNCTION
2) place it and its associated event on circe-actions-handlers-alist
3) place it on the bucket corresponding to event in (circe-irc-handler-table)

If persist is set, the procedure does not remove itself after being called once. This is potentially very dangerous if your condition function is computationally expensive (or, y'know, monetarily expensive). Be careful!"
  (let* ((arg-list (append (list condition-p-function
				 action-function
				 (circe-actions--gensym)
				 event)
			   persist)) ; if unset, persist is nil, the empty list
	 (handler-function (apply 'circe-actions-generate-handler-function
				  arg-list)))
    ;; to gain introspection, pass in condition function, activate function.
    ;; then this allows us to add in the prin1-to-string forms to allow printing
    ;; of the expressions so that we can inspect them while they are active.
    ;; we can use this to deactivate them by symbol quickly
    ;; possibly with completing-read, and a default value of our gensym.
    ;; this complicates the activate-function interface, if implemented
    ;; could  be implemented as an optional two arguments.
    (circe-actions-activate-function handler-function event)))

(defun circe-actions-is-active-p (handler-function event)
  (and (circe-actions-handler-is-on-handler-table-p handler-function event)
       (circe-actions-handler-is-on-alist-p handler-function event)))

(defun circe-actions-handler-is-on-alist-p (handler-function event)
  (member (list handler-function event)
	  circe-actions-handlers-alist))

(defun circe-actions-handler-is-on-handler-table-p (handler-function event)
  (member handler-function
	(gethash event (circe-irc-handler-table))))

(defun circe-actions-panic ()
  "Iterate through circe-actions-handlers-alist, deactivating all the
functions stored in the alist. This is the function you want to run if
something is causing errors constantly"
  (interactive)
  (mapcar (lambda (handler-list)
	    (let ((handler (car handler-list))
		  (event (cadr handler-list)))
	      (circe-actions-deactivate-function handler event)))
	  circe-actions-handlers-alist)
  (message "All handlers cleared!"))

;; -------------------- generalized plistify function --------------------

(defun circe-actions-plistify (arglist &optional event)
  "Given an event, obtain the event signature list from
  `circe-actions-event-plists', interleave the arglist with whatever
  was obtained, and return it. The result is a plist. If no event
  given, attempt to get the event from the arglist. Example:

  ;; calling
  (circe-actions-plistify '((circe-server-process)
			     \"irc.message\"
			     \"alphor!@~...\"
			     \"#freenode\"
			     \"Meow!\")
                             \"irc.message\")

  ;; yields this
  '(:server-proc (circe-server-process)
    :event \"irc.message\"
    :fq-username \"alphor!@~...\"
    :channel \"#freenode\"
    :contents \"Meow!\"))

"
  (unless event
    (setq event (nth 1 arglist))) ; if event is not set, obtain it from the arglist.
  ;; circe-actions--who-needs-dash is -interleave from dash.el
  (circe-actions--who-needs-dash (gethash event circe-actions-event-plists)
				 arglist))

(defun circe-actions--who-needs-dash (list-1 list-2)
  "-interleave from dash.el does exactly this, but expanding the
  dependency graph just for this one use is a cost I'm not willing to
  pay. Error message reflects usage in circe-actions-plistify."
  (let ((xor-func (lambda (bool-1 bool-2)
		    (or (and bool-1 (not bool-2))
			(and (not bool-1) bool-2))))
	(list-1-null (null list-1))
	(list-2-null (null list-2)))
    (cond ((funcall xor-func list-1-null list-2-null)
	   (error "circe-actions-plistify didn't plistify this event correctly! plist-keys: %s \n arglist: %s" list-1 list-2))
	  ((null list-1) nil)
	  (t
	    (cons (car list-1)
		  (cons (car list-2)
			(circe-actions--who-needs-dash (cdr list-1)
						       (cdr list-2))))))))

    

;; -------------------- utility functions? Sure! --------------------

(defun circe-actions-irc-message-contents (server-proc event fq-username channel contents)
  (message "%s" contents))

(defalias 'circe-actions-ctcp-message-payload
  'circe-actions-irc-message-contents)

(defun circe-actions-t (&rest IGNORE)
  "Use this as a condition if you want the action to always occur on event"
  t)

;; -------------------- predicate functions --------------------

;; all of the below functions need lexical binding enabled.
(defun circe-actions-is-from-p (sender)
  "Return a condition-func appropriate for circe-actions-register that
strictly compares the username behind the event with SENDER. 

For a condition-func that uses string-prefix-p, use
circe-actions-hippie-is-from-p"
  (lambda (server-proc event fq-username &rest IGNORE)
    (string-equal sender fq-username)))

(defun circe-actions-hippie-is-from-p (sender)
  "Return a condition-func appropriate for circe-actions-register that checks if the username behind the event starts with SENDER"
  (lambda (server-proc event fq-username &rest IGNORE)
    (string-prefix-p sender fq-username)))
  
(defun circe-actions-sent-to-p (channel-or-user)
  "Return a condition-func appropriate for circe-actions-register that
strictly compares if the target of an event is CHANNEL-OR-USER. 

For a condition-func that uses string-prefix-p, use
circe-actions-hippie-is-from-p"
  (lambda (server-proc event fq-username channel &rest IGNORE)
    (string-equal channel-or-user channel)))

(defun circe-actions-hippie-sent-to-p (channel-or-user)
  "Return a condition-func appropriate for circe-actions-register that checks if the target of an event starts with CHANNEL-OR-USER"
  (lambda (server-proc event fq-username channel &rest IGNORE)
    (string-prefix-p channel-or-user channel)))

;;;###autoload
(defun enable-circe-actions ()
  "load in circe-actions.el. do nothing else."
  (interactive)
  nil)

;;;###autoload
(defun disable-circe-actions ()
  "remove all active handlers, persistent or otherwise. Essentially a defalias to circe-actions-panic with a worse docstring."
  (interactive)
  ;; there really isn't anything else to do besides killing the handlers
  ;; unload functions from function namespace?
  (circe-actions-panic))

;; unimplemented parts of the package
;; (defvar circe-actions-inspect-arg-list '()
;;   "A list of variables that were passed to circe-actions-inspect-args.")
;; (defun circe-actions-inspect-args (&rest args)
;;   "A utility function designed to show you what is passed to an
;;   arbitrary handler. Was very useful when inspecting, so I thought
;;   I'd leave it in here. Be warned with 30+ channels
;;   circe-actions-inspect-arg-list grows mighty fast, if you're crazy
;;   like me and use circe-actions-t as a condition-function-p"
;;   (setq circe-actions-inspect-arg-list (cons args circe-actions-inspect-arg-list))
;;   (message
;;    (with-temp-buffer
;;      (cl-prettyprint args)
;;      (buffer-string)
;;      )))

(provide 'circe-actions)
;;; circe-actions.el ends here
