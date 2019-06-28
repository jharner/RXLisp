# RXLisp
An XLisp-Stat package for R

This is based on original work done by Duncan Temple Lang

This is an approach to creating an embedded version of XLisp-Stat
which can be dynamically loaded into other applications.
Specifically, we create an interface between R and XLisp
which allows XLisp to be loaded into R as a regular package.

Currently, this works on Unix. Work will be needed to configure the
changes to XLisp for other platforms.  Similarly, event loop
integration will be needed for the different platforms. Fortunately, I
am currently investigating a general framework for this for R.

--- Added by Jun Tan

A version of xlisp with added option to be built as a shared library is
included here under xlisp folder.

To build libxlisp.so:
1. change directory to xlisp
2. run command ./configure
3. run command make libxlisp.so

Once libxlisp.so is built, copy it to appropriate directory and/or set
environment variable LD_LIBRARY_PATH so that it can be found by dynamic
loader.

Then install RXLisp by running command: R CMD INSTALL RXLisp
(This step is not verified yet as there were compilation errors

--- End

To install this software, you will need to apply the necessary changes to
XLisp-Stat and build that as a shared library. The new files are located
in the xlisp/ directory. There is a script in the xlisp/ directory
that does this for you. It can be invoked simply as

  setenv XLISP_SRC_DIR  <directory containing XLisp source>
  cd xlisp/
  ./install

This script can be automatically invoked via the configuration of the
R package using
 
  setenv XLISP_SRC_DIR  <directory containing XLisp source>
  R CMD INSTALL --configure-args='--with-build-xlisp-dll' RXLisp

What the script does is simply copy the modified versions of the XLisp
files to the top-level XLisp source directory and then run the
commands

  cd <xlisp source directory>
  configure
  make libxlisp.so

To do this using the install script in the xlisp/ directory, you must
specify the location of the XLisp-Stat source by setting the
environment variable XLISP_SRC_DIR.

Having built the library, the R package can be built.

To run R and load this package, you will need to make libxlisp.so
available to the system's dynamic loader.  You can do this by
installing libxlisp.so in a directory the system searches and
re-running /sbin/ldconfig to rebuild the cache of available shared
libraries. Alternatively, one can include the directory containing the
shared library in the LD_LIBRARY_PATH environment variable.


Steps for building RXLisp
1. Build shared library for xlisp
   a. I have copied xlisp source code and the update into xlisp directory.
   b. Change directory to xlisp: cd xlisp
   c. Run configuration script: ./configure
   d. Build xlisp shared library: make libxlisp.so
   e. Copy the libxlisp.so file to appropriate location. (I haven't figure out
   this yet. I tried /usr/local/lib. It seems not working although ldconfig
   it show up in ldconfig cache.)
2. Install RXLisp library
   a. Go back to project base
   b. Install RXLisp: R CMD INSTALL RXLisp <-l path-to-local-r-library-folder>
   The -l part is optional. Use that when you want to install the package in
   a location different to the standard one.


------ Options for communication between R processes
1. Share information through file system.
2. For unix, pipe/message queue/shared memory.
3. Network connection. Processes don't have to be on the same machine.