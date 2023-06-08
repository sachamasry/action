# Detect the operating system, putting the result in the detected_OS variable
# Windows always sets the OS variable to Windows_NT, while all other UNIX-based
# systems will respond to uname -s
ifeq ($(OS),Windows_NT)
	detected_OS := Windows
else
	detected_OS := $(shell sh -c 'uname -s 2>/dev/null || echo not')
endif

# Any variables which need to be customised by platform can be set in the 
# following code block testing all expected operating systems
ifeq ($(detected_OS),Windows)
	DO_AS_SU=
	BIN_DIR="bin/Windows/$(PROCESSOR_ARCHITECTURE)/"
	BUNDLE_CMD=--load "../../setup.lisp" --load "config/bundle-action.lisp"
	COMPILE_CMD=--script action-clon.lisp
	INSTALL_CMD=cp $(BIN_DIR)/$(BIN_NAME) $(INSTALL_DIR)/
	UNINSTALL_CMD=rm $(INSTALL_DIR)/$(BIN_NAME)
endif
ifeq ($(detected_OS),Darwin)	# Mac OS X/macOS
	BIN_DIR="bin/macOS/$(shell sh -c 'uname -p')/$(shell sh -c 'uname -r')"
	BUNDLE_CMD=--load config/bundle-action.lisp
	COMPILE_CMD=--script action-clon.lisp
	COMPILE_ENV=CC=gcc
endif
ifeq ($(detected_OS),Linux)
	BIN_DIR="bin/Linux/"
endif
ifeq ($(detected_OS),GNU)
endif
ifeq ($(detected_OS),GNU/kFreeBSD)
endif
ifeq ($(detected_OS),OpenBSD)
	DO_AS_SU=doas
	BIN_DIR="bin/OpenBSD/$(shell sh -c 'uname -p')/$(shell sh -c 'uname -r')"
	BUNDLE_CMD=--load config/bundle-action.lisp
	COMPILE_CMD=--script action-clon.lisp
	COMPILE_ENV=CC=gcc
	INSTALL_CMD=$(DO_AS_SU) cp $(TMP_DIR)/$(BUILD_DIR)/act $(INSTALL_DIR)/$(BIN_NAME)
	UNINSTALL_CMD=$(DO_AS_SU) rm $(INSTALL_DIR)/$(BIN_NAME)
endif
ifeq ($(detected_OS),FreeBSD)
	BIN_DIR="bin/FreeBSD/"
endif
ifeq ($(detected_OS),NetBSD)
	BIN_DIR="bin/NetBSD/"
endif
ifeq ($(detected_OS),DragonFly)
	BIN_DIR="bin/DragonFly/"
endif
ifeq ($(detected_OS),Haiku)
	BIN_DIR="bin/Haiku/"
endif

# Directories
TMP_DIR="tmp"
BUILD_DIR="action"
INSTALL_DIR="/usr/local/bin"

BIN_NAME="act"

# Binaries
SBCL=sbcl

all: prepare-staging-area compile
	@echo "===> System built"

compile:
	@echo "---> Starting to compile"
	cd $(TMP_DIR)/$(BUILD_DIR)/ && \
	$(COMPILE_ENV) $(SBCL) $(COMPILE_CMD)
	cd ../../
	mkdir -p $(BIN_DIR) && cp $(TMP_DIR)/$(BUILD_DIR)/act $(BIN_DIR)/$(BIN_NAME)

build: compile

install:
	@echo "===> Installing binary"
	$(INSTALL_CMD)

uninstall:
	@echo "===> Uninstalling binary"
	$(UNINSTALL_CMD)

prepare-staging-area: clean bundle copy-system
	@echo "===> Prepared build staging area"

bundle:
	@echo "---> Bundling system dependencies"
	$(SBCL) $(BUNDLE_CMD)

copy-system:
	@echo "---> Copying system to build"
	cp -rf action.asd *.lisp $(TMP_DIR)/$(BUILD_DIR)/local-projects/ && \
	cp action-clon.lisp $(TMP_DIR)/$(BUILD_DIR)

clean:
	@echo "---> Cleaning target"
	rm -rf $(TMP_DIR)
