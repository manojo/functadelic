functadelic
===========

[![Build Status](https://travis-ci.org/manojo/functadelic.svg?branch=master)](https://travis-ci.org/manojo/functadelic)

This repo is a hub for projects related to multi-staged programming in Scala. It
uses [Lightweight Modular Staging](http://scala-lms.github.io). The main project
is on staged parser combinators. Please find documentation for this project
[here](https://manojo.github.io/2015/09/02/staged-parser-combinators/).

To run the code, please follow these steps:

  1. Clone this here repo in a separate folder: `git clone git@github.com:manojo/functadelic.git`.
  2. Profit:
  ```
    $ cd functadelic
    $ sbt
    > test
  ```

Hope you have fun!


LMS Coding conventions
======================

Here are some basic LMS conventions I use when writing LMS code:

  * Each new `Ops` gets its own file
  * An `Exp` trait only mixes other Exp traits that are not `Opt` trait
  * `Opt` traits are mixed in at a later stage. If you are often going to
    use one, create an `Opt` trait for your `Ops` which mixes the relevant
    `Opt` traits. `Fat` optimizations should not be mixed in with the classical
    `Opt` traits, but used independently.
  * If I use a certain feature (ex Option) in my current trait, I mix it in
    explicitly, even if some other trait I'm using has it already. Serves for
    documentation purposes. Of course, for really trivial stuff it's not necessary
    to do so.
