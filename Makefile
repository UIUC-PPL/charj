CRUN = scala-2.10
CFLAGS = -deprecation -feature -Xprint-types
CC = scalac-2.10

SRC=$(wildcard *.scala)
OUT=$(SRC:.scala=.stamp)

#%.stamp: %.scala

build: $(SRC)
	$(CC) $(CFLAGS) $^ -d classes

test:
	cd classes && $(CRUN) CharjParser.Parse

clean:
	-rm -rf classes/*
