#
# Each instruction in this file generates a new layer that gets pushed to your local image cache
#
#
# Lines preceeded by # are regarded as comments and ignored
#
#
# The line below states we will base our new image on the Latest Official Ubuntu 
#FROM ubuntu:latest

FROM ubuntu:20.04

# ...needed because of ubuntu 20.04 and zlib1g-dev:
ENV TZ=Europe/Berlin
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

LABEL org.opencontainers.image.source="https://github.com/hopr-framework/hopr" 


# Update the image and install packages needed (option "--no-install-recommends" could reduce image size in apt-get install, but generates problems)
RUN apt-get update && \
    apt-get clean && \
    apt-get install -y vim git cmake gcc g++ gfortran liblapack3 liblapack-dev zlib1g-dev  && \
    rm -rf /var/lib/apt/lists/* 

WORKDIR /home

#compile HOPR and install into home/hopr/bin/hopr , then clean-up
RUN git clone https://github.com/hopr-framework/hopr.git && \
    cd hopr && mkdir build && \
    cd build && cmake ../ && make && make install && \
    cd .. && rm -rf build share/GNU
