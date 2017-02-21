
(require 'circe-actions)
(require 'subr-x)

(describe "circe-actions"
  (it "is being tested against the right version."
    (expect circe-actions-version :to-equal "0.0.14")))

(describe "circe-actions-t"
  (it "should return true independent of the arguments"
    (expect (circe-actions-t "whatever" 'is "put" "here" nil 1)
            :to-be t)
    (expect (circe-actions-t)
            :to-be t)))

(describe "circe-actions-activate-function"
  :var (first-symbol second-symbol third-symbol)
  (before-each
    (spy-on 'warn)
    (spy-on 'irc-handler-add :and-call-through)
    (spy-on 'circe-irc-handler-table :and-call-through)
    
    (setq circe-actions-handlers-alist '())
    (setq circe-actions-maximum-handlers 2)

    ;; since we're always going to activate at least one function,
    ;; might as well do it here.
    (circe-actions-activate-function 'first-symbol "irc.message")
    )
  (after-each
    (setq circe-actions-handlers-alist '()))
    
    
  (it "should add symbols and their events to circe-actions-handlers-alist"
    (expect (length circe-actions-handlers-alist) :to-be 1)
    (let ((first-entry (car circe-actions-handlers-alist)))
      (expect (car first-entry) :to-be 'first-symbol)
      (expect (cadr first-entry) :to-equal "irc.message")))

  (it "should add symbols and their events to the handler table"
    (expect 'irc-handler-add :to-have-been-called-with
            (circe-irc-handler-table)
            "irc.message"
            'first-symbol))

  (it "should continue to append symbols and events to circe-actions-handlers-alist"
    (circe-actions-activate-function 'second-symbol "whatever")
    (expect (length circe-actions-handlers-alist) :to-be 2)
    (let ((second-entry (car circe-actions-handlers-alist)))
      (expect (car second-entry) :to-be 'second-symbol)
      (expect (cadr second-entry) :to-equal "whatever")))

  (it "should continue to append symbols and their events to the handler table bucket"
    (circe-actions-activate-function 'second-symbol "whatever")
    (expect 'irc-handler-add :to-have-been-called-with
            (circe-irc-handler-table)
            "whatever"
            'second-symbol))

  (it "should whine to the user when circe-actions-maximum-handlers is exceeded"
    (circe-actions-activate-function 'second-symbol "PRIVMSG")
    (expect 'warn :not :to-have-been-called) ; we're still good.
    (circe-actions-activate-function 'third-symbol "301") ; RUH OH

    (expect (length circe-actions-handlers-alist) :to-be 2)
    
    (expect 'irc-handler-add :not :to-have-been-called-with
            (circe-irc-handler-table)
            "301"
            'third-symbol)
    
    (expect 'warn :to-have-been-called-with
            "circe-actions: Handler tolerance of %s exceeded, nothing added to %s! Clear active handlers or increase circe-actions-maximum-handlers to continue."
            2
            "301")))

            
