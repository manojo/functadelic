experiments
===========

This repo contains code that I write/experiment with. The code is mostly
tied to blogposts written [here](http://manojo.github.io).

Some of this code uses [LMS](http://scala-lms.github.io), or Lightweight
Modular Staging. If you want to check out the code, and have it run,
please do the following steps:

  1. Clone the lms repo: `git clone git@github.com:TiarkRompf/virtualization-lms-core.git lms`.
  2. Make sure you are on the `develop` branch: `cd lms; git checkout develop`.
  3. Publish locally: `sbt "publish-local"`.
  4. Clone this here repo in a separate folder: `git clone git@github.com:manojo/functadelic.git`.
  5. Profit:
      $ cd functadelic
      $ sbt
      > test

Hope you have fun!
