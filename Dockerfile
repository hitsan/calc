FROM ubuntu:22.04

RUN apt-get -y update \
 && apt-get -y --no-install-recommends install \
    curl \
    gnupg \
    openjdk-11-jdk \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" >> /etc/apt/sources.list.d/sbt.list \
 && echo "deb https://repo.scala-sbt.org/scalasbt/debian /" >> /etc/apt/sources.list.d/sbt_old.list \
 && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add
RUN apt-get -y update \
 && apt-get -y --no-install-recommends install \ 
    sbt \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*

# Pre-download sbt dependencies
# WORKDIR /app/calcrator
# RUN mkdir project && echo "sbt.version=1.9.3" > project/build.properties && sbt update