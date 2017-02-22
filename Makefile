build:
	emacs -batch -l make.el
test:
	emacs -batch -f package-initialize -f buttercup-run-discover
PHONY: test
