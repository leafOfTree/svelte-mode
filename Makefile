EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch -L . -l test/svelte-mode-test.el -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch -L . -f batch-byte-compile svelte-mode.el

clean:
	rm -f *.elc test/*.elc
