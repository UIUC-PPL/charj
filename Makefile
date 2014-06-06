CRUN = scala-2.10
CFLAGS = -deprecation -feature -Xprint-types
CC = scalac-2.10

SRC=$(wildcard *.scala)
OUT=$(SRC:.scala=.stamp)

#%.stamp: %.scala

build: $(SRC)
	$(CC) $(CFLAGS) $^ -d classes

test:
	$(CRUN) -cp classes CharjParser.Parse

clean:
	-rm -rf classes/*
