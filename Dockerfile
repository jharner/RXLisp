FROM ubuntu:18.04
MAINTAINER Jim Harner ejharner@gmail.com

# update apt
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y r-base r-base-dev gcc make libc6-dev libx11-dev

COPY . /RXLisp/

WORKDIR /RXLisp/xlisp

# build and install xlsip
RUN ./configure
RUN make
RUN make install
RUN ldconfig /usr/local/lib/xlispstat

# build and install RXLisp package
WORKDIR /RXLisp

ENV XLISPLIB=/usr/local/xlisp/lib/xlispstat
ENV XLISP_SRC_DIR=/RXLisp/xlisp

RUN R CMD build RXLisp
RUN R CMD INSTALL RXLisp_0.3.tar.gz

CMD R
