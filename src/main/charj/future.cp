include "basics.cp"
include "option.cp"

import Option::_

// Internally replaced with CkFuture
// Should not be directly instantiated
template class _FUTURE_[T] {
  system def PROBE()    : boolean { "!CkProbeFuture($me)"; }
  system def GET()      : T       { "((ValueMsg*)CkWaitFuture($me))->value"; }
  system def RELEASE()  : unit    { "CkReleaseFuture($me)"; }
  system def SET(t : T) : unit    { ""; }
}

// Generated instead of a return in async functions
// Should not be directly called
system def sendToFuture[M](m : M) : Future[M] { "$m"; }

class Future[T] {
  val f : _FUTURE_[T];

  def probe() : Maybe[T] {
    if (f.PROBE()) {
      return $(f.GET());
    } else {
      return new None[T]();
    }
  }

  def release()  : unit  { f.RELEASE(); }
  def set(t : T) : unit  { f.SET(t); }
  sync def get() : T     { return f.GET(); }
  sync def #()   : T     { return get(); }
}
