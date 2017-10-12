TMP_DIR="tmp"
BUILD_DIR="action"
INSTALL_DIR="/usr/local/bin"

ifeq ($(OS),Windows_NT)
BIN_DIR="bin/Windows_NT/"
else
BIN_DIR="bin/"
endif

BIN_NAME="act"

all: prepare-staging-area compile
	@echo "===> System built"

compile:
	@echo "---> Starting to compile"
ifeq ($(OS),Windows_NT)
	cd $(TMP_DIR)/$(BUILD_DIR)/ && \
	sbcl --script action-clon.lisp && \
	cd ../../
else
	cd $(TMP_DIR)/$(BUILD_DIR)/ && \
	CC=gcc sbcl --script action-clon.lisp && \
	cd ../../
endif
	mkdir -p $(BIN_DIR) && cp $(TMP_DIR)/$(BUILD_DIR)/act $(BIN_DIR)/$(BIN_NAME)

build: compile

install:
	@echo "===> Installing binary"
ifeq ($(OS),Windows_NT)
	cp $(BIN_DIR)/$(BIN_NAME) $(INSTALL_DIR)/
else
	doas cp $(TMP_DIR)/$(BUILD_DIR)/act $(INSTALL_DIR)/$(BIN_NAME)
endif

uninstall:
	@echo "===> Uninstalling binary"
ifeq ($(OS),Windows_NT)
	rm $(INSTALL_DIR)/$(BIN_NAME)
else
	doas rm $(INSTALL_DIR)/$(BIN_NAME)
endif

prepare-staging-area: clean bundle copy-system
	@echo "===> Prepared build staging area"

bundle:
	@echo "---> Bundling system dependencies"
ifeq ($(OS),Windows_NT)
	sbcl --load "../../setup.lisp" --load "config/bundle-action.lisp" 
else
	sbcl --load "config/bundle-action.lisp" 
endif

copy-system:
	@echo "---> Copying system to build"
	cp -rf action.asd src/ $(TMP_DIR)/$(BUILD_DIR)/local-projects/ && \
	cp action-clon.lisp $(TMP_DIR)/$(BUILD_DIR)

clean:
	@echo "---> Cleaning target"
	rm -rf $(TMP_DIR)
