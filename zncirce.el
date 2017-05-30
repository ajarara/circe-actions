;;; -*- lexical-binding: t -*-

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

;; There isn't a module to get the network you are querying
;; on. Unfortunate. IDK how to parameterize it.  znc seems to have
;; added a $network parameter to allow you to do this, but it's
;; a relatively new addition. In any case, this will work if you use
;; the user field to instruct ZNC to connect to a specific
;; network. The wiki explains this in the FAQ. (linked in docstring)
(defvar zncirce-server-name-func
  (lambda ()
    (let ((nick-network (irc-connection-get (circe-server-process) :user)))
      (cadr (split-string nick-network "/"))))
	  
  "Assuming :user in circe-network-options follows the conventions
  laid forth in the ZNC wiki homepage: http://wiki.znc.in/ZNC This
  will obtain your :user string, and extract the relevant network
  name. If you only use ZNC to connect to one network, and so have no
  need for the suffix network string, simply replace this with a
  procedure like so: (lambda () \"freenode\")

Finally, if you are on a very new version of ZNC (as of 12/24/16), you
can also replace this sexp with: (lambda () \"$network\")

")


(defalias 'zncirce-from-controlpanel-p
  (circe-actions-is-from-p "*controlpanel!znc@znc.in")
  "Return t if the event passed is sent from the controlpanel module")

(defalias 'zncirce-from-status-p
  (circe-actions-is-from-p "*status!znc@znc.in")
  "Return t if the event passed is sent from the controlpanel module")

(defun zncirce-get-buffer-for-chan (buf &optional arg)
    "Query *controlpanel for the buffer variable for a specific channel
  (how many lines of chat to playback upon reconnection to ZNC) for a
  specific chat buffer.  Universal argument instead sets the buffer
  variable.

   Display the result in the minibuffer, using a not-so-fancy
   irc-handler. The number of these that can be active is exactly the
   number that can be active in the circe-actions-maximum-handlers
   defcustom."
  (interactive "b\np")
  (let ((server-name (funcall zncirce-server-name-func))
        (circe-callback-func
	 (lambda ()
	   (circe-actions-register "irc.message"
                                   'zncirce-from-controlpanel-p
                                   'circe-actions-irc-message-contents
                                   ))))
    ;; why not call the callback function now if we're calling it
    ;; independently of arg? because setting it prompts for a
    ;; variable, which is interactive and thus could be exited out of,
    ;; but the callback would already be registered. Kind of harmless,
    ;; if it actually occurred, but so is doing it this way.
    (if (= arg 4)
	;; arg is set, set variable for the channel.
	(let ((buffervar (read-number "Number of messages to buffer: ")))
	 ;; register the callback
	  (funcall circe-callback-func)

	  ;; execute our query
	  (circe-command-MSG "*controlpanel"
			     (concat "setchan buffer $me "
				     server-name
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
				   server-name
				   " "
				   buf))))))

(defun zncirce-save-config ()
  (interactive)
  (circe-actions-register "irc.message"
                          'zncirce-from-status-p
			  'circe-actions-irc-message-contents)
  (circe-command-MSG "*status"
		     "SaveConfig"))
			  

(provide 'zncirce)
;;; zncirce.el ends here
