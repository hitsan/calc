FROM ubuntu:22.04

RUN apt-get -y update \
 && apt-get -y --no-install-recommends install \
    curl \
    gnupg \
    gosu \
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

# COPY endpoint.sh /usr/bin/endpoint.sh
# RUN chmod +x /usr/bin/endpoint.sh
# ENTRYPOINT [ "/usr/bin/endpoint.sh" ]
# CMD ["/bin/bash"]