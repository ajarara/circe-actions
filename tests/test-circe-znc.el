
(require 'subr-x)
(require 'circe-znc)


(describe "circe-znc-modules-table"
  (it "should have a hash table for _every_ key"
    (let ((keys (hash-table-keys circe-znc-modules-table)))
      (expect (and (apply 'and (mapc 'hash-table-p keys))) :not :to-be nil))))
