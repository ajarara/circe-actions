;;; -*- lexical-binding: t -*-

(require 'circe-actions)
(require 'subr-x)

(describe "circe-actions-t"
  (it "should return true independent of the arguments"
    (expect (circe-actions-t "whatever" 'is "put" "here" nil 1)
            :to-be t)
    (expect (circe-actions-t)
            :to-be t)))

(describe "circe-actions-activate-function"
  :var (first-symbol second-symbol third-symbol)
  (before-all
    (setq circe-actions-maximum-handlers 2)) ; for brevity
  (after-all
    (setq circe-actions-maximum-handlers 3)) ; the default
  
  (before-each
    (spy-on 'warn)
    (spy-on 'irc-handler-add :and-call-through)
    
    (setq circe-actions-handlers-alist '())

    ;; since we're always going to activate at least one function,
    ;; might as well do it here.
    ;; manipulating the handler table and then nuking it at the end of every
    ;; test seems to me a bit much. Possibly rewrite this test shadowing
    ;; circe--irc-handler-table in a let expression, which would mean I
    ;; put the below call in every test and remove all this setup.
    (circe-actions-activate-function 'first-symbol "irc.message")
    )
  (after-each
    ;; force regeneration. alternatively empty out the buckets
    ;; selectively, but this requires a lot of coordination between
    ;; the tests and the setup if I want to test for arbitrary events.
    (setq circe--irc-handler-table nil) 
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

(describe "circe-actions-deactivate-function"
  (spy-on 'irc-handler-remove :and-call-through)
  (it "should remove an activated symbol from the handler table and alist."
    (let ((event "irc.message"))
      ;; asserting a clean slate, no handlers for the event being tested.
      ;; possibly a decent idea is to test for adding every event, but
      ;; there is little reason to do this.
      (expect circe-actions-handlers-alist :to-be nil)
      (expect (gethash event (circe-irc-handler-table)) :to-be nil)

      (circe-actions-activate-function 'some-sym event)

      (expect (length circe-actions-handlers-alist) :to-be 1)
      (expect (length (gethash event (circe-irc-handler-table))) :to-be 1)
      
      (circe-actions-deactivate-function 'some-sym event)

      (expect 'irc-handler-remove :to-have-been-called-with
              (circe-irc-handler-table)
              event
              'some-sym)
      (expect (length circe-actions-handlers-alist) :to-be 0)
      (expect (length (gethash event (circe-irc-handler-table))) :to-be 0))))
      
      
(describe "circe-actions-generate-symbol"
  (it "should test that internal symbol is preserved"
    (let* ((some-sym (gensym "circe-actions-sym-"))
           (handler-func
            (circe-actions-generate-handler-function 'circe-actions-t
                                                     (lambda (&rest args)
                                                       nil)
                                                     some-sym
                                                     "irc.message")))
      (expect (symbol-name some-sym) :to-equal (symbol-name handler-func)))))


(describe "circe-actions-generate-handler-function"
  (it "should return a function"
    (expect (functionp
             (circe-actions-generate-handler-function (lambda nil nil)
                                                      (lambda nil nil)
                                                      'some-symbol
                                                      "some.event"))))
  (it "should execute an action when the condition is satisfied"
    (let* ((lever)
           (handler-func
            (circe-actions-generate-handler-function (lambda (&rest args)
                                                       (car args))
                                                     (lambda (&rest args)
                                                       (setq lever (not lever)))
                                                     'some-symbol
                                                     "some.event")))
      ;; assert lever hasn't been set
      (expect lever :to-be nil)

      ;; Pull the lever, Kronk!
      (funcall handler-func nil nil 6 'whatever "boop")
      (expect lever :to-be nil)
      
      (funcall handler-func 5 "") ;; again, this time satisfying the condition
      (expect lever))))

(describe "circe-actions-is-active-p"
  (it "should assert false until a function has been activated"
    (let ((circe-actions-handlers-alist)
           ;; set it, assign it, shadow it.
          (circe--irc-handler-table (circe-irc-handler-table))
          (handler-func
           (circe-actions-generate-handler-function 'cond
                                                    'action
                                                    'some-symbol
                                                    "irc.message")))
      (expect :not (circe-actions-is-active-p handler-func "irc.message"))
      (circe-actions-activate-function handler-func "irc.message")
      (expect (circe-actions-is-active-p handler-func "irc.message"))))
  (it "should error when someone's been messing with the handler table"
    (let ((circe-actions-handlers-alist)
          (circe--irc-handler-table (circe-irc-handler-table))
          (event "irc.message"))
      
      (circe-actions-activate-function 'some-func event)

      ;; someone's being NAUGHTY
      (irc-handler-remove (circe-irc-handler-table)
                          event
                          'some-func)
      
      (expect (lambda () (circe-actions-is-active-p 'some-func event)) :to-throw 'error))))


(describe "circe-actions-handler-is-on-alist-p"
  (it "correctly checks if a symbol and its assoc'd handler is on the alist"
    (let ((circe-actions-handlers-alist)
          (circe--irc-handler-table (circe-irc-handler-table))
          (symbol 'circe-actions-test-whatever)
          (event "301"))
      (expect :not (circe-actions-handler-is-on-alist-p symbol event))
      
      (circe-actions-activate-function symbol event)

      (expect (circe-actions-handler-is-on-alist-p symbol event))
      
      ;; this is kind of ridiculous, but why not.
      (expect :not (circe-actions-handler-is-on-alist-p symbol "some-other-event"))
      (expect :not (circe-actions-handler-is-on-alist-p 'different-symbol event))
      (expect :not (circe-actions-handler-is-on-alist-p 'different-symbol "some-other-event"))

      (circe-actions-deactivate-function symbol event)

      (expect :not (circe-actions-handler-is-on-alist-p symbol event)))))
      
                         

(describe "circe-actions-handler-is-on-handler-table-p"
  (it "has an absurdly long name"
    (expect t))
  (it "correctly checks if a symbol is in the bucket accessed by event"
    (let ((circe-actions-handlers-alist)
          (circe--irc-handler-table (circe-irc-handler-table))
          (symbol 'circe-actions-test-whatever)
          (event "QUIT"))
      (expect :not (circe-actions-handler-is-on-handler-table-p symbol event))
      
      (circe-actions-activate-function symbol event)

      (expect (circe-actions-handler-is-on-handler-table-p symbol event))

      ;; ridiculousness! there likely is a better way to do this.
      (expect :not (circe-actions-handler-is-on-handler-table-p symbol "some-other-event"))
      (expect :not (circe-actions-handler-is-on-handler-table-p 'different-symbol event))
      (expect :not (circe-actions-handler-is-on-handler-table-p 'different-symbol "some-other-event"))

      (circe-actions-deactivate-function symbol event)

      (expect :not (circe-actions-handler-is-on-handler-table-p symbol event)))))


(describe "circe-actions-generate persistence-test"
  (it "once generated and activated, they do not deactivate themselves."
    (let* ((circe-actions-handlers-alist)
          (circe--irc-handler-table (circe-irc-handler-table))
          (symbol 'ca-arbitrary)
          (persist-symbol 'ca-other-arbitrary)
          (event "001")
          ;; for a control, we generate a callback style function with
          ;; very similar arguments
          (callback-arg-list `(circe-actions-t ignore ,symbol ,event))
          (callback-func
           (apply 'circe-actions-generate-handler-function callback-arg-list))
          
          ;; all we do is append the persist bit for persistence
          (persistent-arg-list `(circe-actions-t ignore ,persist-symbol ,event t))
          (persistent-func
           (apply 'circe-actions-generate-handler-function persistent-arg-list)))
      (expect :not (circe-actions-is-active-p callback-func event))
      (expect :not (circe-actions-is-active-p persistent-func event))
      
      (circe-actions-activate-function callback-func event)
      (circe-actions-activate-function persistent-func event)

      (expect (circe-actions-is-active-p callback-func event))
      (expect (circe-actions-is-active-p persistent-func event))

      (funcall callback-func nil nil) ; should deactivate itself
      (funcall persistent-func nil nil) ; should stay put

      ;; (expect :not (circe-actions-is-active-p callback-func event))
      (expect (circe-actions-is-active-p persistent-func event)))))
      

;; -------------------- internal, unrelated functions      --------------------
(describe "circe-actions--interleave"
  (it "should interleave two lists"
    (let* ((list-1 (list :yes :no :other))
           (list-2 (list "yes" "no" "other"))
           (list-interleaved (circe-actions--interleave list-1 list-2)))
      (expect (plist-member list-interleaved :other)) ; no plistp? c'mon emacs
      (expect (plist-get list-interleaved :yes) :to-equal "yes")
      (expect (plist-get list-interleaved :other) :to-equal "other"))))

(describe "circe-actions--replace-prefixed-string"
  (it "should be identity on strings like :this"
    (expect (circe-actions--replace-prefixed-string ":this" ":") :to-equal ":this")
    (expect (circe-actions--replace-prefixed-string ":test" ":") :to-equal ":test"))
  (it "should transform strings with :@strange prefixes given prefix"
    (expect (circe-actions--replace-prefixed-string
             ":@lemony" ":@") :to-equal ":lemony")
    (expect (circe-actions--replace-prefixed-string
             ":!snicket" ":!") :to-equal ":snicket")))

(describe "circe-actions--deep-map"
  (let ((sym-to-string (lambda (x) (symbol-name x))))
  (it "should behave like mapcar on flat lists of symbols"
    (let ((ex-list `(this is a bunch of symbols)))
      (expect (circe-actions--deep-map sym-to-string ex-list) :to-equal
              (mapcar sym-to-string ex-list))))
  (it "shouldn't touch anything that isn't a symbol"
      (expect (circe-actions--deep-map sym-to-string
                                       `(this 5 wont be stringified))
              :to-equal
              `("this" 5 "wont" "be" "stringified")))
  (it "should recurse into nested lists with ease"
    (expect (circe-actions--deep-map sym-to-string
                                     `(hmm how (would I put in apostrophes) in ((symbol)) names))
            :to-equal
            `("hmm" "how" ("would" "I" "put" "in" "apostrophes") "in" (("symbol")) "names")))
  ))

(describe "with-circe-actions-closure"
  (it "should have a stubbed test"
    (expect t :to-be t)))

(describe "circe-actions--transform-sym"
  (it "should have a stubbed test"
    (expect t :to-be t)))

    
(describe "circe-actions--xor"
  (it "should return false on two truthy values"
    (expect (circe-actions--xor t t) :to-be nil)
    (expect (circe-actions--xor 5 10) :to-be nil))
  (it "should return false on two false values"
    (expect (circe-actions--xor nil nil) :to-be nil))
  (it "should return true on one truthy, one not, independent of order"
    (expect (circe-actions--xor t nil) :to-be t)
    (expect (circe-actions--xor nil t) :to-be t)

    (expect (circe-actions--xor 5 nil) :not :to-be nil)
    (expect (circe-actions--xor nil 10) :not :to-be nil)))

























