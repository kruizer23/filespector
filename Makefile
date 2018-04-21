#
# Targets
#

#
# The First target is the one build when there is nothing on make command line
#
all:
	./make.sh

clean:
	./clean.sh

install:
	./install.sh DESTDIR=$(DESTDIR)

uninstall:
	./uninstall.sh DESTDIR=$(DESTDIR)
	