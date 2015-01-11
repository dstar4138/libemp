MAINTAINER Alexander Dean <dstar@csh.rit.edu>

# We rely on at least R17+ for EMP due to a reliance on the map type.
ENV OTP_VERSION 17.4

# Pulling from a basic linux 
FROM debian:latest

# Pull Erlang/EMP/Basic-Plugin Dependencies.
RUN \
    apt-get update &&  \
    apt-get install -y \
        build-essential \
        git \
        libncurses5-dev \
        libssl-dev \
        openssl

# Download and install Erlang package
ADD http://erlang.org/download/otp_src_${OTP_VERSION}.tar.gz /usr/src/
RUN cd /usr/src \
    && tar xf otp_src_${OTP_VERSION}.tar.gz \
    && cd otp_src_${OTP_VERSION} \
    && ./configure \
    && make \
    && make install \
    && cd / && rm -rf /usr/src/otp_src_${OTP_VERSION}

# TODO: Clean up and build release instead.
RUN make libemp && ./startemp.sh

