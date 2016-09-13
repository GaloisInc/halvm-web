# halvm-web

This repository contains a simple (for now) web server for the
[HaLVM](http://halvm.org). By compiling this web server and combining it with a
tar file containing your website, you can serve simple static websites from your
own [Xen server](https://www.xenproject.org/) or [Amazon's
EC2](https://aws.amazon.com/ec2/). Eventually, we hope this web server expands
to support dynamically-generated functions and other cool features. See the
issue list for more information and interesting things to work on.

# Quick Start

The easiest way to build `halvm-web` is to use the [HaLVM Docker
tools](https://github.com/GaloisInc/HaLVM/wiki/Using-Docker-and-the-HaLVM). In
this case, perform the following steps:

  1. `git clone https://github.com/GaloisInc/halvm-web`
  1. `docker run -v ${PWD}:/halvm halvm/extended-gmp halvm-cabal sandbox init`
  1. `docker run -v ${PWD}:/halvm halvm/extended-gmp halvm-cabal install --package-db=/usr/lib64/HaLVM-2.1.1/package.conf.d`

[sorry for the weird argument there. We're working on it.]

These three steps will create a `.cabal-sandbox/bin/halvm-web` binary on your
local disk, which contains the `halvm-web` unikernel. Now its up to you to build
your site! So go do that. Perhaps this is the time for you to try
[Hakyll](https://jaspervdj.be/hakyll/), to double down on the Haskell nerdiness.

When you're done, create a new directory called `site`, and copy all your files
into that directory. So, for example, your mainline index file should be
`site/index.html`.

Now ask yourself: Where do I want to run this?

# Option #1: I want to run this right here!

If you happen to be running on a host running Xen, you should be able to start
up your web server using the handy `run.sh` provided with this repository. It'll
prep your tar file for you, kill off any old web servers running, and start
yours. You'll see it boot and find an IP address, which you can then play around
with as you see fit.

# Option #2: I want to run this IN THE CLOUD!

In this case, we have two steps to follow. The first is simple, the second very
long. Also, I'm going to assume that you have the appropriate AWS credentials
stored in `AWS_ACCESS_KEY` and `AWS_SECRET_KEY`, as per normal.

  1. `tar cvf site.tar site/`
  1. `docker run -v ${PWD}/halvm halvm/extended ec2-unikernel -o ${AWS_ACCESS_KEY} -w ${AWS_SECRET_KEY} .cabal-sandbox/bin/halvm-web site.tar`

This will print a lot of status updates, but at the end of this process, it will
give you an AMI reference. Go to your AWS console, and you can launch it as
normal; just make sure to give it a network card, and make sure that the
security group you assign it allows for access to port 80.
