(require 'subr-x)
(require 'circe-znc)

(describe "circe-znc-modules-table"
  (it "has a hash table for _every_ key"
    (let ((keys (hash-table-keys circe-znc-modules-table)))
      (expect (member nil (mapc 'hash-table-p keys)) :to-be nil))))

(describe "circe-znc-collect-timeout"
  (it "shouldn't be yuge"
    (expect (> 1024 circe-znc-collect-timeout))))

(describe "circe-znc-module-help"
  :var (circe-znc-modules-table)
  (before-each
    (let ((fake-module-name "*fake")
          (module-commands (make-hash-table :test #'equal))
          (circe-znc-modules-table circe-znc-modules-table))
      (puthash "help" 'ignore module-commands)
      
      (puthash "*fake" module-commands circe-znc-modules-table)
      circe-znc-modules-table))

  (it "calls completing-read to obtain a value when needed"
    (spy-on 'completing-read :and-return-value "*fake")
    (circe-znc-module-help)
    (expect 'completing-read :to-have-been-called))
  
  (it "immediately calls the relevant help when given an argument"
    (spy-on 'funcall)
    (circe-znc-module-help "*fake")
    (expect 'funcall :to-have-been-called-with 'ignore)))

(describe "circe-znc--deactivation-p-gen"
  (it "has a stubbed test"
    (expect t :to-be t)))

(describe "circe-znc--get-buffer-create"
  (it "has a stubbed test"
    (expect t :to-be t)))

(describe "circe-znnc--collect-response-in-buf"
  (it "has a stubbed test"
    (expect t :to-be t)))

   
