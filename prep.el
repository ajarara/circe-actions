;; file to prep project state for testing
(package-initialize)

(let ((project-path default-directory))
  (add-to-list 'load-path project-path))
