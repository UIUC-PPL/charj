include "basics.cp"

type Phase { Read, Write, Accumulate, Unknown }

// dummy class to keep this compiling
template class _MSA_[T] {
  system def ENROLL(n : int)   : unit { "$me.enroll($n)"; }
  system def GETINITIALWRITE() : unit { "$me.getInitialWrite()"; }
  system def GETINITIALACCUM() : unit { "$me.getInitialAccum()"; }
}

template class MSA1[T] {
  val size  : int;
  val arr   : _MSA_[T];
  var phase : Phase = Phase::Unknown;

  def this(=size : int, numWorkers : int, maxBytes : int) { MKMSA1D(size, numWorkers, maxBytes); }

  def MKMSA1D(size : int, numWorkers : int, maxBytes : int) : unit { }

  // These will never get called directly
  system def [] (i : int) : T { ""; }
  system def syncToRead () : unit { ""; }
  system def syncToWrite() : unit { ""; }
  system def syncToAccum() : unit { ""; }

  def enroll(n : int) : unit { arr.ENROLL(n); }
  def getInitialWrite() : unit { return arr.GETINITIALWRITE(); }
  def getInitialAccum() : unit { return arr.GETINITIALACCUM(); }
}

template class MSA2[T] {
  val size1 : int;
  val size2 : int;
  val arr   : _MSA_[T];

  def this(=size1 : int, =size2 : int, numWorkers : int, maxBytes : int) { MKMSA2D(size1, size2, numWorkers, maxBytes); }

  def MKMSA2D(size1 : int, size2 : int, numWorkers : int, maxBytes : int) : unit { }

  // These will never get called directly
  system def [] (i : int, j : int) : T { ""; }
  system def syncToRead () : unit { ""; }
  system def syncToWrite() : unit { ""; }
  system def syncToAccum() : unit { ""; }

  def enroll(n : int) : unit { arr.ENROLL(n); }
  def getInitialWrite() : unit { return arr.GETINITIALWRITE(); }
  def getInitialAccum() : unit { return arr.GETINITIALACCUM(); }
}

template class MSA3[T] {
  val size1 : int;
  val size2 : int;
  val size3 : int;
  val arr   : _MSA_[T];

  def this(=size1 : int, =size2 : int, =size3 : int, numWorkers : int, maxBytes : int) { MKMSA3D(size1, size2, size3, numWorkers, maxBytes); }

  def MKMSA3D(size1 : int, size2 : int, size3 : int, numWorkers : int, maxBytes : int) : unit { }

  // These will never get called directly
  system def [] (i : int, j : int, k : int) : T { ""; }
  system def syncToRead () : unit { ""; }
  system def syncToWrite() : unit { ""; }
  system def syncToAccum() : unit { ""; }

  def enroll(n : int) : unit { arr.ENROLL(n); }
  def getInitialWrite() : unit { return arr.GETINITIALWRITE(); }
  def getInitialAccum() : unit { return arr.GETINITIALACCUM(); }
}
