# Makefile for Syllabus Management System

# Compiler
COBC = cobc
# Compiler flags
COBFLAGS = -x -I src/copybooks

# Source directories
SRC_DIR = src
BIN_DIR = bin
COPYBOOK_DIR = src/copybooks

# Ensure bin directory exists
$(shell mkdir -p $(BIN_DIR))

# Source files
SOURCES = $(SRC_DIR)/SYLABUS.cbl \
          $(SRC_DIR)/SYLREG.cbl \
          $(SRC_DIR)/SYLUPD.cbl \
          $(SRC_DIR)/SYLDEL.cbl \
          $(SRC_DIR)/SYLQRY.cbl \
          $(SRC_DIR)/SYLLST.cbl \
          $(SRC_DIR)/SYLRPT.cbl \
          $(SRC_DIR)/SYLCOM.cbl \
          $(SRC_DIR)/hello.cob

# Object files
OBJECTS = $(patsubst $(SRC_DIR)/%.cbl,$(BIN_DIR)/%,$(filter %.cbl,$(SOURCES))) \
          $(patsubst $(SRC_DIR)/%.cob,$(BIN_DIR)/%,$(filter %.cob,$(SOURCES)))

# Default target
all: $(OBJECTS)

# Special rule for SYLREG program
$(BIN_DIR)/SYLREG: $(SRC_DIR)/SYLREG.cbl src/copybooks/SYLFILE.cpy
	$(COBC) $(COBFLAGS) -o $@ $<

# Special rule for hello program
$(BIN_DIR)/hello: $(SRC_DIR)/hello.cob
	$(COBC) $(COBFLAGS) -o $@ $<

# General rule for other programs
$(BIN_DIR)/%: $(SRC_DIR)/%.cbl
	$(COBC) $(COBFLAGS) -o $@ $<

# Run the SYLLABUS main program
run: all
	cd $(BIN_DIR) && ./SYLABUS

# Run the hello program
run-hello: $(BIN_DIR)/hello
	cd $(BIN_DIR) && ./hello

# Clean build artifacts
clean:
	rm -rf $(BIN_DIR)/*

.PHONY: all run run-hello clean
