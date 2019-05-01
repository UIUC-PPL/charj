#if !defined(__MAIN_CHARJ_HEADER__)
#define __MAIN_CHARJ_HEADER__

#if defined(__PARALLEL__)
  #include "charm++.h"
#endif

#include <cctype>

union GenericType {
   int32_t i32;
   int64_t i64;
   bool b;
   void* v;
   float f;
   double d;
   char* c;
   int16_t i16;

   GenericType(int32_t i32_)   : i32(i32_) {}
   GenericType(int64_t i64_)   : i64(i64_) {}
   GenericType(bool b_)        : b(b_)     {}
   GenericType(void* v_)       : v(v_)     {}
   GenericType(float f_)       : f(f_)     {}
   GenericType(double d_)      : d(d_)     {}
   GenericType(char* c_)       : c(c_) {}
   GenericType(std::string c_) : c(const_cast<char*>(c_.c_str())) {}
   GenericType()               : v(NULL)   {}
   GenericType(int16_t i16_)   : i16(i16_) {}
};
#if defined(__PARALLEL__)
  PUPbytes(GenericType);
#endif

struct Generic {
  int w;
  GenericType t;

  Generic(int32_t i32_)   : w(0), t(GenericType(i32_)) {}
  Generic(int64_t i64_)   : w(1), t(GenericType(i64_)) {}
  Generic(bool b_)        : w(2), t(GenericType(b_))     {}
  Generic(char* c_)       : w(6), t(GenericType(c_))     {}
  Generic(void* v_)       : w(3), t(GenericType(v_))     {}
  Generic(float f_)       : w(4), t(GenericType(f_))     {}
  Generic(double d_)      : w(5), t(GenericType(d_))     {}
  Generic(std::string c_) : w(6), t(GenericType(const_cast<char*>(c_.c_str())))     {}
  Generic()               : w(7), t(GenericType()) {}
  Generic(int16_t i16_)   : w(8), t(GenericType(i16_)) {}

  #if defined(__PARALLEL__)
  void pup(PUP::er &p) {
    p | w;

    if (w != 3 && w != 6) {
      p | t;
    } else if (w == 3) {
      bool ptrWasNull = (t.v == 0);
      p | ptrWasNull;
      if (!ptrWasNull) {
        PUP::able *ptr_able = (PUP::able*)t.v;
        p | ptr_able;
        t.v = ptr_able;
      } else {
        t.v = 0;
      }
    }
  }
  #endif
};

std::ostream& operator<<(std::ostream& stream, Generic const& self) {
  switch (self.w) {
  case 0: return stream << self.t.i32;
  case 1: return stream << self.t.i64;
  case 2: return stream << self.t.b;
  case 4: return stream << self.t.f;
  case 5: return stream << self.t.d;
  case 6: return stream << self.t.c;
  case 8: return stream << self.t.i16;
  default: return stream << "ERROR";
  }
}

namespace std {
  template <typename T>
  std::string to_string_base(const T& n) {
    std::ostringstream stm;
    stm << n ;
    return stm.str();
  }

  const char* to_char_arr(Generic const& self) {
    switch (self.w) {
    case 0: return const_cast<char*>(std::to_string_base(self.t.i32).c_str());
    case 1: return const_cast<char*>(std::to_string_base(self.t.i64).c_str());
    case 2: return const_cast<char*>(std::to_string_base(self.t.b).c_str());
    case 4: return const_cast<char*>(std::to_string_base(self.t.f).c_str());
    case 5: return const_cast<char*>(std::to_string_base(self.t.d).c_str());
    case 6: return self.t.c;
    case 8: return const_cast<char*>(std::to_string_base(self.t.i16).c_str());
    default: return "ERROR";
    }
  }

  std::string to_string(Generic const& self) {
    switch (self.w) {
    case 0: return std::to_string_base(self.t.i32);
    case 1: return std::to_string_base(self.t.i64);
    case 2: return std::to_string_base(self.t.b);
    case 4: return std::to_string_base(self.t.f);
    case 5: return std::to_string_base(self.t.d);
    case 6: return std::string(self.t.c);
    case 8: return std::to_string_base(self.t.i16);
    default: return "ERROR";
    }
  }
}

#endif /*__MAIN_CHARJ_HEADER__*/
