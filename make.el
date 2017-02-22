(package-initialize)

(let ((circe-actions-path "~/proj/circe-znc/circe-actions.el")
      (circe-znc-path "~/proj/circe-znc/circe-znc.el"))
  (quelpa `(circe-actions :fetcher file :path ,circe-actions-path) :upgrade t)
  (quelpa `(circe-znc :fetcher file :path ,circe-znc-path) :upgrade t))
