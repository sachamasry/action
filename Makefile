all: prepare-staging-area compile
	@echo "===> System built"

compile:
	@echo "---> Starting to compile"
	cd /tmp/act && \
	CC=gcc sbcl --load bundle.lisp --script action-clon.lisp

install:
	@echo "===> Installing binary"
	doas cp /tmp/act/act /usr/local/bin 

uninstall:
	@echo "===> Uninstalling binary"
	doas rm /usr/local/bin/act

prepare-staging-area: clean bundle copy-system
	@echo "===> Prepared build staging area"

bundle:
	@echo "---> Bundling system dependencies"
	sbcl --load "config/bundle-action.lisp" 

copy-system:
	@echo "---> Copying system to build"
	cp -r ../action /tmp/act/local-projects/ && \
	cp action-clon.lisp /tmp/act/ 

clean:
	@echo "---> Cleaning target"
	rm -rf /tmp/act 
