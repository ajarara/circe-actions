
(require 'subr-x)
(require 'circe-znc)


(describe "circe-znc-modules-table"
  (it "should have a hash table for _every_ key"
    (let ((keys (hash-table-keys circe-znc-modules-table)))
      (expect (member nil (mapc 'hash-table-p keys)) :to-be nil))))

(describe "circe-znc-collect-timeout"
  (it "shouldn't be yuge"
    (expect (> 1024 circe-znc-collect-timeout))))

(describe "circe-znc-module-help"
  (spy-on 'completing-read)
  (spy-on 'hash-table-keys)
  :var (circe-znc-modules-table)
  (it "should call completing-read"
    (circe-znc-module-help)
    (expect 'completing-read :to-have-been-called))
  (it "should get the keys of the modules-table"
    (circe-znc-module-help)
    (expect 'hash-table-keys :to-have-been-called-with circe-znc-modules-table))
  )
    
