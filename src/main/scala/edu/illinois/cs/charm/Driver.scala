package edu.illinois.cs.charm

import Parse.verbose
import java.nio.file.{Paths, Files}
import scala.util.Properties
import sys.process._

object Driver extends App {
  val version = getClass.getPackage.getImplementationVersion
  val parse = new Parse()
  // get the options from the command-line args
  val (options, files) = args.partition(x => x startsWith "-")
  verbose = options contains "--verbose"
  val seq = options contains "--sequential"
  val msa = options contains "--msa"
  val outdir = Paths.get((options.find(_ startsWith "-o=") getOrElse "-o=output").drop(3))
  // handle the version and help command line options
  if (options contains "--version") {
    println(s"charj $version")
    sys.exit(0)
  } else if ((options contains "--help") || files.isEmpty) {
    println("usage is: charj OPTIONS? INPUT_FILES\n")
    println("--help\t\tdisplays this help message")
    println("--version\tdisplays the version number")
    println("--msa\t\tcompiles with MSA module and passes")
    println("--sequential\tgenerates code that does not target Charm++")
    println("--verbose\tdisplays all intermediate compilation information")
    println("-o=<outdir>\tspecifies output directory (default: output)")
    sys.exit(0)
  }
  // get the environment variables
  val charj = Properties.envOrNone("CHARJ_HOME").map(Paths.get(_))
  val cxx = Properties.envOrElse("CXX", "c++")
  val cflags = Properties.envOrElse("CFLAGS", "") + "-w"
  val charm = Properties.envOrNone("CHARM_HOME").map(Paths.get(_))
  val xlat = charm.map(_.resolve("src/xlat-i"))
  val charmc = charm.map(_.resolve("bin/charmc"))
  val ready = charj.isDefined && (charmc.isDefined || seq)
  if (!ready) println("WARNING : CHARJ_HOME or CHARM_HOME is undefined, may be unable to generate binaries.")
  // if the outdir doesn't exist, create it
  if (!Files.exists(outdir)) Files.createDirectory(outdir)
  // add the first argument as an include
  parse.lstIncludes += files.head
  // parse the input files
  val tree = parse.parseRecur(files.head, files.tail)  
  if (verbose) println("--- successfully parsed AST ---")
  if (verbose) println(tree)
  // start the stages of the compilation process
  if (verbose) println("--- start manipulate ast (pass 1) ---")
  (new ModifyASTInitial(tree, verbose)).start()

  if (verbose) println("--- start enclose pass ---")
  (new DetermineEnclose(tree, verbose)).start()

  Pass.run(classOf[DefineWhenEntries], tree)

  if (verbose) println("--- start manipulate ast (pass 2) ---")
  (new ModifyASTPostEnclose(tree, verbose)).start()

  if (verbose) println("--- start scope builder pass ---")
  (new ScopeBuilder(tree, verbose)).start()

  if (verbose) println("--- start scope expand ---")
  (new ScopeExpand(tree, verbose)).start()

  if (verbose) println("--- start import symbol ---")
  (new ScopeImport(tree, verbose)).start()

  if (verbose) println("--- start scope uniqueness test ---")
  (new ScopeUniqueness(tree, verbose)).start()

  if (verbose) println("--- start resolve all symbols ---")
  (new ResolveSymbol(tree, verbose)).start()

  if (verbose) println("--- start CFA analysis ---")
  (new ControlFlow.Analysis(tree, verbose)).start()

  if (verbose) println("--- start sync analysis ---")
  (new SyncAnalysis(tree, verbose)).start()

  if (msa) {
    if (verbose) println("--- start MSA analysis ---")
    (new PhaseAnalysis(tree, verbose)).start()
    (new HandleConversion(tree, verbose)).start()
  }

  if (verbose) println("--- start future code generation ---")
  (new FutureConversion(tree, verbose)).start()

  if (verbose) println("--- start code generation ---")
  (new CodeGeneration(tree, verbose)).start()

  if (ready) {
    val result : Int = if (seq) {
      Process(s"""$cxx -O3 -std=c++11 -I${charj.get.resolve("include")} generate.cc -o generate""", outdir.toFile) !
    } else {
      val objFiles = ((List("xi-main.o", "CaseList.o", "Forall.o", "SdagEntry.o", "xi-AstNode.o", "xi-Member.o", "xi-symbol.o", "Case.o", "For.o", "sdag-globals.o", "xi-BlockConstruct.o", "xi-Message.o", "xi-Template.o", "CEntry.o", "If.o", "Serial.o", "xi-Chare.o", "xi-Module.o", "xi-Type.o", "CParsedFile.o", "SList.o", "xi-Construct.o", "xi-Parameter.o", "xi-util.o", "CSdagConstruct.o", "IntExpr.o", "Template.o", "xi-Entry.o", "xi-scan.o", "xi-Value.o", "CStateVar.o", "OList.o", "When.o", "xi-grammar.tab.o", "xi-SdagCollection.o", "Else.o", "Overlap.o", "While.o", "xi-SdagConstruct.o").map(obj => charj.get.resolve(s"obj/$obj"))) mkString " ")

      Process(s"""$cxx -g -c -o generate_ci.o generate_ci.cc ${cflags} ${if (msa) "-D__MSA__" else ""} -I${charm.get.resolve("include")} -I${xlat.get} -I${xlat.get}/sdag -I${xlat.get}/sdag/constructs""", outdir.toFile) #&&
      Process(s"$cxx -g -o generate_ci generate_ci.o $objFiles ${cflags}", outdir.toFile) #&&
      Process(s"./generate_ci", outdir.toFile) #&&
      Process(s"""${charmc.get} ${if (msa) "-module msa -D__MSA__" else ""} -std=c++11 -I${charj.get.resolve("include")} -D__PARALLEL__ -O3 generate.cc -o generate_charm""", outdir.toFile) !
    }

    if (result != 0) sys.exit(result)
    else println(s"""DONE : binary generated as $outdir/generate${if (seq) "" else "_charm"}.""")
  }
}
