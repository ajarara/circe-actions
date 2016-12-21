
(require 'ert)

(require 'irc)
(require 'circe)


(defun circe-mock-table ()
  "There has got to be a better way to do this..."
  (let ((table (irc-handler-table)))
      (irc-handler-add table "irc.registered" #'circe--irc-conn-registered)
      (irc-handler-add table "conn.disconnected" #'circe--irc-conn-disconnected)
      (irc-handler-add table nil #'circe--irc-display-event)
      (irc-handle-registration table)
      (irc-handle-ping-pong table)
      (irc-handle-isupport table)
      (irc-handle-initial-nick-acquisition table)
      (irc-handle-ctcp table)
      (irc-handle-state-tracking table)
      (irc-handle-nickserv table)
      (irc-handle-auto-join table)
      table))


(ert-deftest generate-test ()
  (let (ignore-func (circe-actions-generate
