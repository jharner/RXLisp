FROM ubuntu:18.04
MAINTAINER Jim Harner ejharner@gmail.com

# update apt
ENV DEBIAN_FRONTEND noninteractive

RUN apt-get update && apt-get install -y r-base r-base-dev gcc make libc6-dev libx11-dev

