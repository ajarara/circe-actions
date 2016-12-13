;; This set of functions requires you to have the controlpanel module enabled.
;; The neat thing about the controlpanel module is that it allows you to message
;; commands that modify configuration that would require you to use the web front
;; end. This is a little tedious. But the syntax for controlpanel is also kind of
;; tedious.
;; These functions make it very easy to do what you want.

;; Interesting idea: znc does config very strangely, by focusing on
;; the web and irc interface to change values. Unfortunately, that
;; means that version control is a little difficult. What would be
;; useful is the ability to trigger a version control update from znc
;; on save from *status. This is undoubtedly difficult, even with the
;; shell access ZNC gives admins.

;; not implemented
;; (defcustom zncirce-enable-command-mutex t
;;    "Wait for responses before allowing the user to send another command.
;;     If a new command is attempted while waiting for a reply from the 
;;     previous one, an error results, and nothing is attempted."
;;     :type 'boolean
;;     :group 'zncirce)


;; Hmm. Unfortunately it is difficult to figure out how to dynamically
;; guess what server you're on. IDK how to do it. For now I will wrap
;; this in a lambda and just return freenode.
(defvar zncirce-server-name
    (lambda ()
      "freenode"))
  
  
(defvar zncirce-command-mutex nil
  "When set, any commands that add a function to the irc.message
handler error out. This is unset after the function removes
itself, or when you call zncirce-remove-control-reply (which
removes the function for you")

;; this no longer works
(defun zncirce-remove-control-reply ()
  " This no longer works!! "
  (interactive)
  (irc-handler-remove (circe-irc-handler-table)
		      "irc.message"
		      #'zncirce-control-reply)
  (setq zncirce-command-mutex nil)
  (message "Unset circe-znc-mutex"))

(defun zncirce-control-reply (server-proc event fq-username channel contents)
  "this is put on circe's irc-handler-table, reads the next event from
,*controlpanel, displays it in the minibuffer, and then pops itself
off. Can easily be replaced with a function that does something else,
(ie by using a macro) if I could figure out how to refer to the
generated function."
  (when (equal fq-username "*controlpanel!znc@znc.in")
    (message contents)
    (irc-handler-remove (circe-irc-handler-table)
                        "irc.message"
                        #'zncirce-control-reply)
    (setq zncirce-command-mutex nil)))

(defmacro zncirce-control-reply-gen (condition action symbol)
  "Given a condition and action, both lambda expressions that take five arguments:
- server-proc
- event
- fq-username
- channel
- contents
return a function that is designed to be placed on circe's irc-message event.

This generated function has the following responsibilities:
When called, pass the arguments to the condition. When the condition returns true, then pass the arguments to the action.

Then remove itself off circe-irc-handler-table's 'irc.message' bucket, and unset the mutex. (in the future, remove itself off the list of active handlers)

The symbol should be uninterned, but doesn't have to be."
  `(defun ,symbol (server-proc event fq-username channel contents)
     (when (condition
		    server-proc event fq-username channel contents)
       (action
		server-proc event fq-username channel contents)
       (irc-handler-remove (circe-irc-handler-table)
			   "irc.message"
			   ,symbol)
       (setq zncirce-command-mutex nil))))

;; (defun zncirce-control-reply-gen (condition action symbol)
;;   (defun ,symbol (server-proc event fq-username channel contents)
;;     (when (funcall condition
;; 		   server-proc event fq-username channel contents)
;;       (funcall action
;; 	       server-proc event fq-username channel contents)
;;       (irc-handler-remove (circe-irc
      
   
(defun zncirce-controlpanel-wrap (func-to-wrap condition action)
  "given a function, add the zncirce-control-reply handler to circe's
irc.message event, and execute the function. zncirce-control-reply waits for a
message (in this case, anything that we get back from *controlpanel),
removes itself, and the function "
  (if zncirce-command-mutex
      (message "Currently waiting for a message from *controlpanel. Try again later or remove the circe handler with zncirce-remove-control-reply")
    (let ((symbol (gensym "zncirce-sym")))
      (setq zncirce-command-mutex t)
      (irc-handler-add (circe-irc-handler-table)
		       "irc.message"
		       (zncirce-control-reply-gen 'condition
						  'action
						  symbol))
      (funcall func-to-wrap))))

(defun zncirce-get-buffer-for-chan (buf &optional arg)
  "Query *controlpanel for the buffer variable for a specific channel
  (how many lines of chat to playback upon reconnection to ZNC) for a
  specific chat buffer.  Universal argument instead sets the buffer
  variable.

   Display the result in the minibuffer, using a fancy irc-handler. Only one of these can be active at a time.

   Improvements I'd like to make:
take out the querying *controlpanel functionality in this code, move it to it's own function. (which would mean a general call to that function would prompt for the variable to set/get)"
  
  (interactive "b\np")
  (if (= arg 4)
      ;; arg is set, set variable for the channel instead.
      (let ((buffervar (read-number "Number of messages to buffer: ")))
	(zncirce-controlpanel-wrap 
	 (lambda ()
	   (circe-command-MSG "*controlpanel"
			      (concat "setchan buffer $me "
				      (funcall zncirce-server-name)
				      " "
				      buf
				      " "
				      (number-to-string buffervar))))
	 'zncirce-from-controlpanelp
	 'zncirce-message-contents
	 ))
    (zncirce-controlpanel-wrap
     (lambda ()
       (circe-command-MSG "*controlpanel"
			  (concat "getchan buffer $me "
				  (funcall zncirce-server-name)
				  " "
				  buf)))
     'zncirce-from-controlpanelp
     'zncirce-message-contents)))

;; examples of conditions
(defun zncirce-from-controlpanelp (server-proc event fq-username channel contents)
  (equal fq-username "*controlpanel!znc@znc.in"))

;; examples of actions
(defun zncirce-message-contents (server-proc event fq-username channel contents)
  (message contents))

