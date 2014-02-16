PREFIX := /usr/local
LIBDIR := lib
BINDIR := bin
INSTALL = install
INSTALL_DIR = $(INSTALL) -m755 -d
INSTALL_PROGRAM = $(INSTALL) -m755

compile:
	./rebar compile

clean:
	-rm ./ebin/*.beam
	-rm $(PREFIX)/$(BINDIR)/eos
	-rm -rf $(PREFIX)/$(LIBDIR)/erleos/

install:
	-rm $(PREFIX)/$(BINDIR)/eos
	-mkdir $(PREFIX)/$(LIBDIR)/erleos/
	$(INSTALL_DIR) $(PREFIX)/$(LIBDIR)/erleos/ebin
	$(INSTALL_PROGRAM) ./ebin/* $(PREFIX)/$(LIBDIR)/erleos/ebin/
	$(INSTALL_PROGRAM) ./eos.sh $(PREFIX)/$(LIBDIR)/erleos/eos.sh
	ln -s $(PREFIX)/$(LIBDIR)/erleos/eos.sh $(PREFIX)/$(BINDIR)/eos

install_cygpath:
	-rm $(PREFIX)/$(BINDIR)/eos
	-mkdir $(PREFIX)/$(LIBDIR)/erleos/
	$(INSTALL_DIR) $(PREFIX)/$(LIBDIR)/erleos/ebin
	$(INSTALL_PROGRAM) ./ebin/* $(PREFIX)/$(LIBDIR)/erleos/ebin/
	$(INSTALL_PROGRAM) ./eos.sh $(PREFIX)/$(LIBDIR)/erleos/eos.sh
	ln -s $(PREFIX)/$(LIBDIR)/erleos/eos_cygpath.sh $(PREFIX)/$(BINDIR)/eos
