FROM ubuntu:16.04
MAINTAINER dangtv <dangtv18@gmail.com>

ENV OS_LOCALE="en_US.UTF-8" \
    LANG=${OS_LOCALE} \
    LANGUAGE=${OS_LOCALE} \
    LC_ALL=${OS_LOCALE} \
    PG_VERSION=9.6 \
    OCAML_VERSION=4.07.0 \
    PG_USER=postgres \
    PG_HOME=/var/lib/postgresql \
    PG_RUN_DIR=/run/postgresql \
    PG_LOG_DIR=/var/log/postgresql

ENV PG_CONF_DIR="/etc/postgresql/${PG_VERSION}/main" \
    PG_BIN_DIR="/usr/lib/postgresql/${PG_VERSION}/bin" \
    PG_DATA_DIR="${PG_HOME}/${PG_VERSION}/main"

RUN apt-get update && apt-get install -y locales && locale-gen ${OS_LOCALE} \
 && dpkg-reconfigure locales && rm -rf /var/lib/apt/lists/*

# installing postgresql
RUN apt-get update && apt-get install -y wget \
 && wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add - \
 && echo 'deb http://apt.postgresql.org/pub/repos/apt/ xenial-pgdg main' > /etc/apt/sources.list.d/pgdg.list \
 && apt-get update && apt-get install -y postgresql-${PG_VERSION} postgresql-client-${PG_VERSION} \
 postgresql-contrib-${PG_VERSION} postgresql-server-dev-${PG_VERSION} postgresql-common \
 && locale-gen ${OS_LOCALE} \
 && systemctl enable postgresql \
 && update-rc.d postgresql enable \
 && apt-get purge -y --auto-remove wget \
#  && rm /etc/apt/sources.list.d/pgdg.list \
 && rm -rf /var/lib/apt/lists/*

# configuring postgresql
COPY docker/ubuntu/config/postgresql/pg_hba.conf ${PG_CONF_DIR}
RUN echo "listen_addresses='*'" >> ${PG_CONF_DIR}/postgresql.conf

# installing plsh
WORKDIR /root/
RUN BUILD_PKGS="git build-essential make" \
 && RUNTIME_PKGS="" \
 && apt-get update && apt-get install -y ${BUILD_PKGS} ${RUNTIME_PKGS} \
 && git clone https://github.com/petere/plsh \
 && cd plsh && git checkout 9b108516a005a0592d9c9e145189825f92f820e1 \
 && make && make install \
 && rm -rf /root/plsh \
 && apt-get purge -y --auto-remove ${BUILD_PKGS} \
 && rm -rf /var/lib/apt/lists/*

# installing Lean and Z3
WORKDIR /root/
RUN BUILD_PKGS="wget unzip" \
 && RUNTIME_PKGS="libgomp1" \
 && apt-get update && apt-get install -y ${BUILD_PKGS} ${RUNTIME_PKGS} \
 && wget https://github.com/leanprover/lean/releases/download/v3.4.2/lean-3.4.2-linux.tar.gz \
 && tar -xzvf lean-3.4.2-linux.tar.gz \ 
 && mv lean-3.4.2-linux /usr/lib/lean \ 
 && ln -s /usr/lib/lean/bin/lean /usr/bin/lean \
 && ln -s /usr/lib/lean/bin/leanpkg /usr/bin/leanpkg \
 && ln -s /usr/lib/lean/bin/leanchecker /usr/bin/leanchecker \
 && wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.4/z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04.zip \
 && unzip z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04.zip \
 && mv z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04 /usr/lib/z3 \
 && ln -s /usr/lib/z3/bin/z3 /usr/bin/z3 \
 && rm lean-3.4.2-linux.tar.gz \
 && rm z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04.zip \
 && apt-get purge -y --auto-remove ${BUILD_PKGS} \
 && rm -rf /var/lib/apt/lists/*

# install npm
WORKDIR /root/
RUN BUILD_PKGS="wget" \
 && RUNTIME_PKGS="" \
 && apt-get update && apt-get install -y ${BUILD_PKGS} ${RUNTIME_PKGS} \
 && wget https://nodejs.org/dist/v8.16.0/node-v8.16.0-linux-x64.tar.gz \
 && tar xvfz node-v8.16.0-linux-x64.tar.gz \
 && mkdir -p /usr/local/nodejs \
 && mv node-v8.16.0-linux-x64/* /usr/local/nodejs \
 && ln -s /usr/local/nodejs/bin/node /usr/local/bin/node \
 && ln -s /usr/local/nodejs/bin/npm /usr/local/bin/npm \
 && ln -s /usr/local/nodejs/bin/npx /usr/local/bin/npx \
 && npm install npm -g \
 && rm -rf node-v8.16.0-linux-x64 \
 && rm node-v8.16.0-linux-x64.tar.gz \
 && apt-get purge -y --auto-remove ${BUILD_PKGS} \
 && rm -rf /var/lib/apt/lists/*

RUN  mkdir -p /root/.lean && mkdir -p /root/birds && mkdir -p /usr/lib/birds

# installing lean libs for birds
COPY verification /usr/lib/birds/verification
COPY docker/ubuntu/config/lean/leanpkg.path /root/.lean/
WORKDIR /usr/lib/birds/verification/
RUN BUILD_PKGS="git" \
 && RUNTIME_PKGS="" \
 && apt-get update && apt-get install -y ${BUILD_PKGS} ${RUNTIME_PKGS} \
 && leanpkg configure && cd /usr/lib/birds/verification/_target/deps/mathlib/ && leanpkg configure && leanpkg build -- --threads=1 \
 && cd /usr/lib/birds/verification/_target/deps/super/ && leanpkg configure && leanpkg build \
 && cd /usr/lib/birds/verification/ && leanpkg build \
 && apt-get purge -y --auto-remove ${BUILD_PKGS} \
 && rm -rf /var/lib/apt/lists/*

# installing BIRDS web-based editor
COPY webui /usr/lib/birds/webui
WORKDIR /usr/lib/birds/webui/
RUN BUILD_PKGS="python build-essential make" \
 && RUNTIME_PKGS="" \
 && apt-get update && apt-get install -y ${BUILD_PKGS} ${RUNTIME_PKGS} \
 && scripts/build.sh && rm -rf client && rm -rf scripts && rm -rf node_modules \
 && rm package.json package-lock.json .travis.yml \
 && apt-get purge -y --auto-remove ${BUILD_PKGS} \
 && rm -rf /var/lib/apt/lists/*

# installing BIRDS
COPY src /root/birds/src
COPY Makefile /root/birds/
WORKDIR /root/
RUN BUILD_PKGS="wget build-essential git make opam m4" \
 && RUNTIME_PKGS="" \
 && apt-get update && apt-get install -y ${BUILD_PKGS} ${RUNTIME_PKGS} \
 && wget https://github.com/ocaml/ocaml/archive/4.07.0.tar.gz \
 && tar -xzvf 4.07.0.tar.gz && cd ocaml-4.07.0 && ./configure && make world.opt && umask 022 && make install \ 
 && rm -rf /root/ocaml-4.07.0 \
 && rm /root/4.07.0.tar.gz \
 && echo "y" | opam init && eval `opam config env` \
 && opam install -y num postgresql \
 && cd /root/birds && make depend && make release \
 && mv release/birds /usr/bin/ \
 && make clean \
 # clean
 && apt-get purge -y --auto-remove ${BUILD_PKGS} \
 && rm -rf /var/lib/apt/lists/* \
 && rm -rf /root/.opam \
 && rm -rf /usr/local/lib/ocaml \
 && rm -rf /usr/local/bin/ocaml* \
 && rm -rf /root/birds

WORKDIR /
COPY dockerentrypoint.sh /sbin/dockerentrypoint.sh
RUN chmod 755 /sbin/dockerentrypoint.sh

EXPOSE 5050 3010 3000 5432/tcp
VOLUME ["${PG_HOME}", "${PG_RUN_DIR}"]
# ENTRYPOINT ["dockerentrypoint.sh"]

CMD ["/sbin/dockerentrypoint.sh"]
# CMD ["/bin/bash"]
