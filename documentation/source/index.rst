.. Charj documentation master file, created by
   sphinx-quickstart on Wed Apr  3 14:14:45 2019.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.

Welcome to Charj's documentation!
=================================

Contents:

.. toctree::
   :maxdepth: 2

Introduction
============
Charj is a language built on top of the Charm++ runtime libraries; it was designed to offer high-performance, convenient and familiar syntax, and safety. This manual is intended for users that are already familiar with Charm++ that are interested in Charj as an alternative to the traditional C++ and Charmxi-based approach. While Charj can be used to develop serial applications that are independent of Charm++, these concepts will not be explicitly discussed here.

Getting Started
------------------
The Charj compiler is a source-to-source compiler that compiles Charj code to C++ code. It is built in Scala using the `scala-parser-combinators library <https://github.com/scala/scala-parser-combinators>`_, and requires SBT. TODO.

Charj Concepts
==============

Classes and Traits
------------------
TODO

Charm++ Concepts
================

Chares and Chare Arrays
-----------------------
Per the standard Charm++ programming model, Charj programs consist of collections of objects that interact via asynchronous method invocation; these objects are called chares. Chares can stand alone as singletons, or be collected into chare *arrays* and *groups*. Charj's syntax for their creation will be familiar to existing users of Charm+::

  chare class Singleton {
    // ...
  }

  chare array1d class Array1d {
    // ...
  }

  chare group class Group {
    // ...
  }

It is worth noting that we must specify whether or not the chare is based on a class or a trait, since abstract chare-traits cannot be instantiated on their own (but can contain entry methods to be mixed-in with other chares). While most modifiers must be specified after the ``chare`` keyword (e.g. ``array[1d|2d|3d]``, and ``[node|]group``), ``mainchare`` is used without the ``chare`` keyword instead::

  mainchare class Main {
    // ...
  }

Entry Methods
-------------
Chares may contain remotely invocable methods, known as *entry methods*; these methods can only be called via the chare's proxy and cannot return a value. Entry methods take arguments like normal functions, but the arguments are serialized by the sender, sent to the receiver in a message, and deserialized before use by the receiver; this process is called *parameter marshalling*. Entry methods are defined like normal Charj functions, but must be within a chare and use the ``entry`` keyword::

  mainchare class Main {
    entry def this(args : Array[string]) {
      // an entry method invocation using the chare's proxy
      thisProxy.foo(/* ... */);
    }

    entry def foo(bar : int) {
      // ...
    }
  }

Shown above, unlike a normal class, a chare's constructor must be declared as an entry method.

Chare Instantiation and Proxies
-------------------------------
TODO

Chare-Specific Fields
---------------------
TODO

Reductions
----------
Charj provides reductions as a building block for parallel applications and, unlike in Charm++, all entry methods may be used as a reduction target in Charj. Reductions usually collect a value from every chare in a collection and "reduce" them at a single entry-point, but may not collect values at all. The following built-in reduction operations are provided in Charj:

* Logical_AND
* Logical_OR

An example of a reduction with arguments and one without arguments to an entry method ``bar`` of ``fooProxy`` is shown below::

    contribute(fooProxy::bar);
    // OR
    contribute(true, Reduction::Logical_AND, fooProxy::bar);

Custom reduction operations are defined by applying the ``reducer`` keyword to a function and parameterizing it with the type to be reduced::

    reducer[foo] def reduce_foo(accum : foo, x : foo) : foo {
      // ...
    }

And are used as follows::

    val x : foo = /* ... */;
    contribute(x, reduce_foo, barProxy::receive_foo);
    // ...
    entry def receive_foo(someFoo : foo) : unit {
      // ...
    }

* :ref:`genindex`
* :ref:`modindex`
* :ref:`search`
