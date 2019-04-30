CRUN = scala-2.10
CFLAGS = -deprecation -feature -Xprint-types
CC = scalac-2.10

SRC=$(wildcard *.scala)
OUT=$(SRC:.scala=.stamp)

all: bin/charj.jar

bin/charj.jar: $(SRC)
	sbt assembly
	mv target/scala-*/charj-assembly-*.jar bin/charj.jar

test:
	./compile.sh system.cp

clean:
	-rm -rf target/* bin/charj.jar
