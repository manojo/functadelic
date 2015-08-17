functadelic
===========

This repo contains code that I write/experiment with. The code is mostly
tied to blogposts written [here](http://manojo.github.io).

Some of this code uses [LMS](http://scala-lms.github.io), or Lightweight
Modular Staging. If you want to check out the code, and have it run,
please follow these steps:

  1. Clone the lms repo: `git clone git@github.com:TiarkRompf/virtualization-lms-core.git lms`.
  2. Make sure you are on the `develop` branch: `cd lms; git checkout develop`.
  3. Publish locally: `sbt "publish-local"`.
  4. Clone this here repo in a separate folder: `git clone git@github.com:manojo/functadelic.git`.
  5. Profit:
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
