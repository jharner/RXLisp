# RXLisp
An XLisp-Stat package for R

This is an approach to creating an embedded version of XLisp-Stat
which can be dynamically loaded into other applications.
Specifically, we create an interface between R and XLisp
which allows XLisp to be loaded into R as a regular package.

Currently, this works on Unix. Work will be needed to configure the
changes to XLisp for other platforms.  Similarly, event loop
integration will be needed for the different platforms. Fortunately, I
am currently investigating a general framework for this for R.

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

