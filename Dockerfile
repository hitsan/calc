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

# Download sbt dependance
RUN cd calcrator && sbt update

# Download metals
RUN curl -Lo /usr/local/bin/coursier https://git.io/coursier-cli && chmod +x /usr/local/bin/coursier
RUN coursier bootstrap org.scalameta:metals_2.12:latest.stable -o /usr/local/bin/metals