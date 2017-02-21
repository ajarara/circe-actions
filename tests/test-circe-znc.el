
(require 'subr-x)
(require 'circe-znc)

(describe "circe-znc-modules-table"
  (it "should have a hash table for _every_ key"
    (let ((keys (hash-table-keys circe-znc-modules-table)))
      (expect (and (mapcar 'hash-table-p keys))))))
