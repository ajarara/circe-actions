test:
	emacs -batch -f package-initialize -f buttercup-run-discover
casktest:
	cask exec buttercup -L
PHONY: test casktest
