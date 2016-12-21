;; This set of functions requires you to have the controlpanel module enabled.
;; The neat thing about the controlpanel module is that it allows you to message
;; commands that modify configuration that would require you to use the web front
;; end. This is a little tedious. But the syntax for controlpanel is also kind of
;; tedious.
;; These functions make it very easy to do what you want.

;; (require 'circe-actions)

(defgroup zncirce nil
  "Set of commands to inspect and modify ZNC parameters on the fly"
  :group 'convenience)

;; There isn't a module to get the server you are querying on. Unfortunate. IDK how to parameterize it.
(defvar zncirce-server-name
  (lambda ()
    "freenode")
  "TODO: get current ZNC host")

(defun zncirce-from-controlpanelp (server-proc event fq-username channel contents)
  (equal fq-username "*controlpanel!znc@znc.in"))

(defun zncirce-message-contents (server-proc event fq-username channel contents)
  (message contents))

(defun zncirce-get-buffer-for-chan (buf &optional arg)
    "Query *controlpanel for the buffer variable for a specific channel
  (how many lines of chat to playback upon reconnection to ZNC) for a
  specific chat buffer.  Universal argument instead sets the buffer
  variable.

   Display the result in the minibuffer, using a fancy
   irc-handler. The number of these that can be active is exactly the
   number that can be active in the circe-actions-maximum-handlers
   defcustom."
  (interactive "b\np")
  (let ((circe-callback-func
	 (lambda ()
	   (circe-actions-register 'zncirce-from-controlpanelp
				'zncirce-message-contents
				"irc.message"))))
    (if (= arg 4)
	;; arg is set, set variable for the channel.
	(let ((buffervar (read-number "Number of messages to buffer: ")))
	 ;; register the callback
	  (funcall circe-callback-func)

	  ;; execute our query
	  (circe-command-MSG "*controlpanel"
			     (concat "setchan buffer $me "
				     (funcall zncirce-server-name)
				     " "
				     buf
				     " "
				     (number-to-string buffervar))))
      ;; arg wasn't set, just query for it instead.
      (progn
	;; register the callback
	(funcall circe-callback-func)

	;; execute our query
	(circe-command-MSG "*controlpanel"
			   (concat "getchan buffer $me "
				   (funcall zncirce-server-name)
				   " "
				   buf))))))

(defun zncirce-save-config ()
  (interactive)
  (circe-actions-register (lambda (server-proc event fq-username channel contents)
			    (equal fq-username "*status!znc@znc.in"))
			  (lambda (server-proc event fq-username channel contents)
			    (message contents))
			  "irc.message")
  (circe-command-MSG "*status"
		     "SaveConfig"))
			  
