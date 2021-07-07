Charj is a language for productive parallel programming, based on the [Charm++ runtime system](https://github.com/UIUC-PPL/charm). While its compiler is experimental in nature, it can be used to build and run simple programs.

To begin using Charj, you will need a (recent) version of Charm++ present at `CHARM_HOME`, a JDK installation, and a SBT installation. Then, run the `./build.sh` script from this repository, and refer to the instructions it generates throughout its execution.

Note, Charj's syntax has undergone numerous changes throughout the years. Where it was, historically, Java-based, it is now Scala-based. We occasionally use the term (Scala-)Charj to refer to the current iteration.

The most comprehensive documentation for Charj is [Aaron Becker's PhD thesis](http://charm.cs.illinois.edu/newPapers/12-44/paper.pdf). While its examples are based on the Java-based iteration of Charj, its ideas translate to the modern version of Charj found here.
