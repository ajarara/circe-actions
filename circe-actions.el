;;; -*- lexical-binding: t -*-

;; Utility functions for interfacing with circe-irc-handler-table
(require 'irc)
(require 'circe)


(defgroup circe-actions nil
  "Convenient interface to circe events"
  :group 'convenience)

(defcustom circe-actions-maximum-handlers 1
  "Do not allow this many active handlers. This number is compared against
   circe-actions-handlers-alist's length. Once it is greater than or equal to
   the length of the alist, ignore any requests to add more to the
   queue, instead alert the user with a message."
  :group 'circe-actions
  :type 'integer)

(defvar circe-actions-handlers-alist
  '()
  "Store all the symbols of the generated event handlers here. The symbols assigned to any circe-action should be uninterned so that they do not pollute the function namespace (as an arbitrary number are generated)

 The values corresponding to the symbols are the handlers they are on.")

  

(defun circe-actions-generate-handler-function
    (condition-p-function action-function symbol event &optional persist)
  "Produce a procedure aliased to a SYMBOL that executes
  ACTION-FUNCTION when CONDITION-P-FUNCTION

SYMBOL should be uninterned, but doesn't have to be.
EVENT is a string key, like irc.message obtained from circe-irc-handler-table

if PERSIST is non-nil, do not remove the symbol from the handler
obtained from circe-actions-handlers-alist, do not remove the handler
from (circe-irc-handler-table), do not pass go.

PERSIST is a dangerous flag to set. If you pass in an expensive condition or action, paired with a high occurence rate event, your emacs system will slow to a crawl, and the only way to deactivate it is through an interactive circe-actions-deactivate-function call.

CONDITION-P-FUNCTION and ACTION-FUNCTION must be procedures with this
argument signature:

server-proc - usually the result of (circe-server-process)
event - the event, ie irc.message or a ctcp ping
fq-username - the username followed by cloak in whois format
channel - channel or, if channel is your nick, a query
contents - text payload of the event"
  (defalias symbol
    (lambda (server-proc event fq-username channel contents)
      (let ((args (list server-proc event fq-username channel contents)))
	(when (apply condition-p-function args)
	  (apply action-function args)
	  (unless persist
	    (circe-actions-deactivate-function symbol event)))))))

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
	(message "circe-actions: Handler tolerance of %s exceeded, nothing added to %s! Run M-x circe-actions-inspect (unimplemented)" alist-len event)
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
  "Given a CONDITION-P-FUNCTION and ACTION-FUNCTION that takes args consistent with the EVENT passed (as shown in the README, or consulting `circe-actions-handler-arguments'):

1) generate a procedure that executes ACTION-FUNCTION when CONDITION-P-FUNCTION
2) place it and its associated event on circe-actions-handlers-alist
3) place it on the bucket corresponding to event in (circe-irc-handler-table)

If persist is set, the procedure does not remove itself after being called once. This is potentially very dangerous if your condition function is computationally expensive (or, y'know, monetarily expensive). Be careful!
"
  (let* ((arg-list (append (list condition-p-function
				 action-function
				 (circe-actions--gensym)
				 event)
			   persist)) ; if unset, persist is nil, the empty list
	 (handler-function (apply 'circe-actions-generate-handler-function
				  arg-list)))
    (circe-actions-activate-function handler-function event)))


(defun circe-actions-panic ()
  "Iterate through circe-actions-handlers-alist, deactivating all the functions stored in the alist."
  (interactive)
  (mapcar (lambda (handler-list)
	    (let ((handler (car handler-list))
		  (event (cadr handler-list)))
	      (circe-actions-deactivate-function handler event)))
	  circe-actions-handlers-alist)
  (message "All handlers cleared!"))

;; -------------------- utility functions? Sure! --------------------

(defun circe-actions-message-contents (server-proc event fq-username channel contents)
  (message contents))

(defun circe-actions-t (&rest IGNORE)
  t)

;; almost all of the below functions need lexical binding enabled.
(defun circe-actions-wait-for (username)
  "Return a proc that strictly compares the passed username. Use hippy-wait-for to get a function that uses an in-house version of starts-with"
  (lambda (server-proc event fq-username &rest IGNORE)
    (equal username fq-username)))

(defun circe-actions-hippy-wait-for (username)
  "Return a proc that tests if fq-username starts with username"
  (let ((usr-str-len (length username)))
    (lambda (server-proc event fq-username &rest IGNORE)
      (string-equal (substring fq-username 0 usr-str-len) username))))
  
;; (defvar circe-actions-inspect-arg-list '()
;;   "A list of variables passed to circe-actions-inspect-args.")
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
