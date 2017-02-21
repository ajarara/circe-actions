
;; (require 'ert)
;; (require 'circe-actions)
;; (require 'subr-x) ; necessary for hash-table-keys, it's built in.

;; ;; all tests are of the form (should EXPECTED ACTUAL)

;; (defvar circe-actions--mock-table nil
;;   "As in the real scenario, do not access this variable directly unless you're cleaning up after a test. Use circe-actions-mock-table")

;; (defun circe-actions-mock-table ()
;;   "There has got to be a better way to do this...
;;    In tests, make sure to set circe-actions--mock-table nil afterwards!"
;;   (if circe-actions--mock-table
;;       circe-actions--mock-table
;;     (let ((table (irc-handler-table)))
;;       (irc-handler-add table "irc.registered" #'circe--irc-conn-registered)
;;       (irc-handler-add table "conn.disconnected" #'circe--irc-conn-disconnected)
;;       (irc-handler-add table nil #'circe--irc-display-event)
;;       (irc-handle-registration table)
;;       (irc-handle-ping-pong table)
;;       (irc-handle-isupport table)
;;       (irc-handle-initial-nick-acquisition table)
;;       (irc-handle-ctcp table)
;;       (irc-handle-state-tracking table)
;;       (irc-handle-nickserv table)
;;       (irc-handle-auto-join table)
;;       (setq circe-actions--mock-table table)
;;       table)))


;; (ert-deftest circe-actions-version-test ()
;;   "Check that we're testing the correct version"
;;   (should (equal "0.0.14" circe-actions-version)))

;; (ert-deftest circe-actions-t-test ()
;;   "Test circe-actions-t always returns t independent of the arguments"
;;   (should (circe-actions-t "whatever" 'is "put" "here" 1 nil))
;;   (should (circe-actions-t)))

;; (ert-deftest circe-actions-deactivate-function-test ()
;;   (let ((circe-actions-handlers-alist '())
;; 	(some-sym (gensym "circe-actions-sym-")))
;;     (flet ((circe-irc-handler-table () (circe-actions-mock-table)))

;;       ;; do what circe-actions-activate-function would have done
;;       (setq circe-actions-handlers-alist
;; 	    (cons (list some-sym "irc.message") circe-actions-handlers-alist))
;;       (irc-handler-add (circe-irc-handler-table)
;; 		       "irc.message"
;; 		       some-sym)

;;       (circe-actions-deactivate-function some-sym "irc.message")
;;       (should (equal 0 (length circe-actions-handlers-alist)))
;;       (should (equal 0 (length (gethash "irc.message" (circe-irc-handler-table))))))))
      
;; (ert-deftest circe-actions-activate-function-test ()
;;   "Given an arbitrary thing and associated event (must be a key of circe-handler-table as generated in circe-actions-mock-table), add it to a mock list, unless it exceeds circe-actions-maximum-handlers (which is also mocked here)"
;;   (let ((circe-actions-handlers-alist '())
;; 	(circe-actions-maximum-handlers 2))
;;     ;; changing to cl-flet breaks the test
;;     ;; for clear reasons: http://stackoverflow.com/a/18895704
;;     ;; we are intentionally shadowing circe-irc-handler-table here.
;;     ;; if there's an equivalent construct that shuts the evaluator up,
;;     ;; lemme know.
;;     (flet ((circe-irc-handler-table () (circe-actions-mock-table)))
;;       (unwind-protect
;; 	  (progn
;; 	    (circe-actions-activate-function 'arb-sym1 "irc.message")
;; 	    (should (equal 1 (length circe-actions-handlers-alist)))

;; 	    ;; symbol comparison intended, there aren't any objects behind these
;; 	    (should (equal 'arb-sym1 (car (nth 0 circe-actions-handlers-alist))))

;; 	    (circe-actions-activate-function 'arb-sym2 "irc.message")
;; 	    (should (equal 2 (length circe-actions-handlers-alist)))
	    
;; 	    ;; no errors should occur (it would be kind of obnoxious)
;; 	    ;; instead a message is shown to the user.
;; 	    (circe-actions-activate-function 'arb-sym3 "irc.message")
;; 	    (should (equal 2 (length circe-actions-handlers-alist)))

;; 	    ;; check that sym1 and sym2 are still in the list
;; 	    (should (member '(arb-sym1 "irc.message") circe-actions-handlers-alist))
;; 	    (should (member '(arb-sym2 "irc.message") circe-actions-handlers-alist))

;; 	    ;; check arb-sym3 is not in the list.
;; 	    (should (null (member '(arb-sym3 "irc.message")
;; 				  circe-actions-handlers-alist))))
;; 	(progn
;; 	  (circe-actions-deactivate-function 'arb-sym1 "irc.message")
;; 	  (circe-actions-deactivate-function 'arb-sym2 "irc.message"))))))



;; generate-test should test circe-actions-generate-handler-function
;; can be improved by splitting it and testing each factor of g-h-f

;; (ert-deftest circe-actions-generate-symbol-test ()
;;   "Test that internal symbol is preserved"
;;   (let* ((some-sym (gensym "circe-actions-sym-"))
;; 	 (handler-func
;; 	   (circe-actions-generate-handler-function 'circe-actions-t
;; 						    ;; return message
;; 						    (lambda (&rest args)
;; 						      (nth 4 args))

;; 						    some-sym
;; 						    "irc.message")))
;;     (should (string-equal (symbol-name some-sym) (symbol-name handler-func)))))

;; (ert-deftest circe-actions-generate-condition-test ()
;;   (let* ((some-sym (gensym "circe-actions-sym-"))
;; 	 (handler-func
;; 	 (circe-actions-generate-handler-function 'circe-actions-t
;; 						  ;; return message
;; 						  (lambda (&rest args)
;; 						    (nth 4 args))
;; 						  some-sym
;; 						  "irc.message")))
;;     (should (equal "to-be-returned"
;; 	     (funcall handler-func
;; 		      nil
;; 		      nil
;; 		      6
;; 		      'arb-sym
;; 		      "to-be-returned")))))


;; (ert-deftest circe-actions-generate-joint-cond-action-pass-test ()
;;   (let* ((cond-func
;; 	  (lambda (server-proc event &rest args)
;; 	    "Return true if args is a non-empty list"
;; 	    (if args
;; 		t
;; 	      nil)))
;; 	 (act-func
;; 	  (lambda (server-proc event &rest args)
;; 	    "Return the event passed"
;; 	    event))
;; 	 (some-sym (gensym "circe-actions-sym-"))
;; 	 (handler-func
;; 	  (circe-actions-generate-handler-function cond-func
;; 						   act-func
;; 						   some-sym
;; 						   "irc.ctcp.ping")))
;; 	 ;; if condition passes, we get the results of our action function
;; 	 ;; (in this case, we just get returned the event it was called with)
;; 	 (should (equal "irc.ctcp.ping"
;; 			(funcall handler-func
;; 				 'arbitrary-process
;; 				 "irc.ctcp.ping"
;; 				 'non-empty-arg)))))

;; (ert-deftest circe-actions-generate-joint-cond-action-fail-test ()
;;   "generate a function with constantly failing condition and call it immediately, making sure action does not occur"
;;   (let* ((cond-func
;; 	  (lambda (server-proc event &rest args)
;; 	    "Return true if args is a non-empty list"
;; 	    (if args
;; 		t
;; 	      nil)))
;; 	 (act-func
;; 	  (lambda (server-proc event &rest args)
;; 	    "Return the event passed"
;; 	    event))
;; 	 (some-sym (gensym "circe-actions-sym-"))
;; 	 (handler-func
;; 	  (circe-actions-generate-handler-function cond-func
;; 						   act-func
;; 						   some-sym
;; 						   "irc.message")))
;;     (should (null (funcall handler-func
;; 			   'arb
;; 			   "irc.ctcp.ping")))))

;; (ert-deftest circe-actions-is-active-p-test ()
;;   "unimplemented"
;;   (flet ((circe-irc-handler-table () (circe-actions-mock-table)))
;;     (let* ((circe-actions-handlers-alist)
;; 	   (event "irc.ctcp.ping")
;; 	   (some-func (circe-actions-generate-handler-function
;; 		      'circe-actions-t
;; 		      (lambda (&rest args)
;; 		        nil)
;; 		      (gensym "circe-actions-sym-")
;; 		      event)))
;;       (should (null (circe-actions-is-active-p some-func event)))
;;       (circe-actions-activate-function some-func event)
;;       (should (circe-actions-is-active-p some-func event)))))

;; should something like this be separated into multiple tests?
;; (ert-deftest circe-actions-handler-is-on-alist-p-test ()
;;   "Initially, check that circe-...-alist-p is false.
;; Then, add the generated symbol and its assoc handler to the alist,
;; Then, check circe-...-alist-p is true.
;; Then, remove the generated symbol and its assoc handler to the alist.
;; Then, check circe-...alist-p is false."
;;   (flet ((circe-irc-handler-table () (circe-actions-mock-table)))
;;     (let (circe-actions-handlers-alist
;; 	  (some-sym (gensym "circe-actions-sym-"))
;; 	  (event "irc.message"))
;;       (should (null (circe-actions-handler-is-on-alist-p some-sym event)))
;;       (setq circe-actions-handlers-alist
;; 	    (cons (list some-sym event) circe-actions-handlers-alist))
;;       (should (circe-actions-handler-is-on-alist-p some-sym event))
;;       (setq circe-actions-handlers-alist
;; 	    (cdr circe-actions-handlers-alist)))))
	

;; (ert-deftest circe-actions-handler-is-on-handler-table-p-test ()
;;   "Initially, check that there is nothing on the given event bucket.
;; Activate the function, and check that the generated symbol and its assoc handler is in the event bucket.
;; Then, remove the generated symbol and its assoc handler to the alist
;; Then, check it's not on the bucket anymore."
;;   (flet ((circe-irc-handler-table () (circe-actions-mock-table)))
;;     (let ((circe-actions-handlers-alist)
;; 	  (circe-actions--mock-table) 
;; 	  (some-sym (gensym "circe-actions-sym-"))
;; 	  (event "QUIT"))
;;       (defalias some-sym
;; 	(lambda (&rest args)
;; 	  t))
;;       (should
;;        (null (circe-actions-handler-is-on-handler-table-p some-sym
;;       							  event)))
;;       ;; Huh... weird, irc.message is not on the mock handler table.
;;       ;; (message (with-temp-buffer
;;       ;; 		 (print "Event as given: ")
;;       ;; 		 (cl-prettyprint event)
		 
;;       ;; 		 (let ((keys (hash-table-keys (circe-irc-handler-table))))
;;       ;; 		   (print "Event in handler list?")
;;       ;; 		   (cl-prettyprint (member event keys))
;;       ;; 		   (print "Handler list")
;;       ;; 		   (cl-prettyprint keys))
;;       ;; 		 (buffer-string)))
	       
;;       (circe-actions-activate-function some-sym event)
;;       (should
;;        (circe-actions-handler-is-on-handler-table-p some-sym
;;       						    event))
;;       (circe-actions-deactivate-function some-sym event)
;;       (setq circe-actions--mock-table nil))))


;; (ert-deftest circe-actions-generate-persistence-test ()
;;   (let* ((circe-actions-handlers-alist)
;; 	 (cond-func
;; 	  (lambda (server-proc event &rest args)
;; 	    "Return true always"
;; 	    t))
;; 	 (act-func
;; 	  (lambda (server-proc event &rest args)
;; 	    "Return non-nil"
;; 	    1))
;; 	 (some-sym (gensym "circe-actions-sym-"))
;; 	 (handler-func
;; 	  (circe-actions-generate-handler-function
;; 	   cond-func
;; 	   act-func
;; 	   some-sym
;; 	   "PRIVMSG"
;; 	   t))
;; 	 ;; we need to generate a non-terminating process to satisfy irc-handler-run
;; 	 (proc (start-process "fake" nil "cat"))
;; 	 (event "PRIVMSG")) ; arbitrary, but necessary.
;;     (flet ((circe-irc-handler-table () (circe-actions-mock-table)))
;;       (progn
;; 	 (circe-actions-activate-function handler-func event)
	 
;; 	 ;; (irc-handler-run (circe-irc-handler-table) event
;; 	 ;; 		  proc
;; 	 ;; 		  'arg2
;; 	 ;; 		  event
;; 	 ;; 		  "target"
;; 	 ;; 		  "contents")
	 
;; 	 ;; in case the test fails, we don't want to have tons of
;; 	 ;; waiting cats.  They'll get hungry!
;; 	 (kill-process proc)
	 
;; 	 ;; check the function is still active
;; 	 ;; notice we don't quote handler-func here, as handler-func
;; 	 ;; points to a symbol which points to a function.
;; 	 (should (circe-actions-is-active-p handler-func event)) 
;; 	 (should (circe-actions-is-active-p some-sym event))

;; 	 ;; while we're here check deactivating the function works
;; 	 ;; regardless of persistence.
       
;; 	 (circe-actions-deactivate-function some-sym event)

;; 	 ;; ;; check it's no longer active
;; 	 (should (null (circe-actions-is-active-p some-sym event)))
;;       ))))

;; (ert-deftest circe-actions--interleave ()
;;   (let* ((list-1 (list :yes :no :other))
;; 	 (list-2 (list "yes" "no" "other"))
;; 	 (list-interleaved (circe-actions--interleave list-1 list-2)))
;;     (should (plist-member list-interleaved :yes))
;;     (should (equal (plist-get list-interleaved :yes) "yes"))
;;     (should (equal (plist-get list-interleaved :other) "other"))
;;     (should-error (circe-actions--interleave '(1 2) '(1)))
;;     ))
