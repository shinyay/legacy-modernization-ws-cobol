SRCDIR = src
BINDIR = bin
PROG = hello

.PHONY: all clean run

all: $(BINDIR)/$(PROG)

$(BINDIR)/$(PROG): $(SRCDIR)/$(PROG).cob | $(BINDIR)
	cobc -x -o $@ $<

$(BINDIR):
	mkdir -p $(BINDIR)

run: $(BINDIR)/$(PROG)
	./$(BINDIR)/$(PROG)

clean:
	rm -rf $(BINDIR)
