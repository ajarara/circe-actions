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

  

(defun circe-actions-generate-handler-function (condition-p-function action-function symbol event &optional persist)
  "Produce a procedure aliased to a SYMBOL that executes
  ACTION-FUNCTION when CONDITION-P-FUNCTION

SYMBOL should be uninterned, but doesn't have to be.
EVENT is a string key, like irc.message obtained from circe-irc-handler-table


if PERSIST is non-nil, do not remove the symbol from the handler
obtained from circe-actions-handlers-alist, do not remove the handler
from (circe-irc-handler-table), do not pass go.

CONDITION-P-FUNCTION and ACTION-FUNCTION must be procedures with this
argument signature:

server-proc - usually the result of (circe-server-process)
event - the event, ie irc.message or a ctcp ping
fq-username - the username followed by cloak in whois format
channel - channel or, if channel is your nick, a query
contents - text payload of the event"
  (defalias symbol
    (lambda (server-proc event fq-username channel contents)
      (let ((args '(server-proc event fq-username channel contents)))
	(when (apply condition-p-function args)
	  (apply action-function args)
	  (unless persist
	    ;; remove itself from the handler list first.
	    (irc-handler-remove (circe-irc-handler-table)
				handler
				symbol)
	    ;; huh? no destructive op to remove elements from a list?
	    (setq circe-actions-handlers-alist
		  (delete (assoc symbol circe-handlers-alist)
			  circe-actions-handlers-alist)))
	  )))))

(defun circe-actions-activate-function (handler-function event)
  "Given a HANDLER-FUNCTION created by
  circe-actions-generate-handler-function, get the symbol associated
  with it. If the length of circe-actions-handlers-alist exceeds
  circe-actions-maximum-handlers, message the user the length of the
  list and the symbol of the handler-function that was attempted to be
  activated.

Otherwise, add the HANDLER-FUNCTION to the
circe-actions-handlers-alist (with a key of symbol and HANDLER), then
place it at event in the hash-table obtained from
(circe-irc-handler-table).

TODO: symbol-value doesn't work in lexical-binding mode. -_-
Fix it."
  (let ((symbol (symbol-value 'handler-function))
	(alist-len (length circe-actions-handlers-alist)))
    (if (>= alist-len circe-actions-maximum-handlers)
	(message "circe-actions: Handler tolerance of %s exceeded, nothing added to %s! Run M-x circe-actions-inspect" alist-len event)
      (progn
	;; add the handler-function to the list
	(setq circe-actions-handlers-alist
	      (cons (list symbol handler-function) circe-actions-handlers-alist))
	;; add the handler-function to the event table. The function is now live.
	(irc-handler-add (circe-irc-handler-table)
			 event
			 'handler-function)))))
			 
(defun circe-actions-gensym ()
  (gensym "circe-actions-gensym-"))

;; example usage? Sure!
(defun circe-actions-message-contents (server-proc event fq-username channel contents)
  (message contents))

(defun circe-actions-lower-standards (server-proc event &rest IGNORE)
  "Please respond."
  (equal event "irc.message"))

(circe-actions-activate-function
 (circe-actions-generate-handler-function 'circe-actions-lower-standards
					 'circe-actions-message-contents
					 (circe-actions-gensym)
					 "irc.message")
 "irc.message"
 )
