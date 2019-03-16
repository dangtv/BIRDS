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
COPY config/postgresql/pg_hba.conf ${PG_CONF_DIR}
RUN echo "listen_addresses='*'" >> ${PG_CONF_DIR}/postgresql.conf

# installing pgAdmin4
RUN apt-get update && apt-get install -y python python-pkg-resources python-pip \
#  && pip install --no-cache-dir sphinx flask_security flask_paranoid python-dateutil flask_sqlalchemy flask_gravatar flask_migrate simplejson \
 && pip --no-cache-dir install https://ftp.postgresql.org/pub/pgadmin/pgadmin4/v4.3/pip/pgadmin4-4.3-py2.py3-none-any.whl \
 && apt-get purge -y --auto-remove python-pip \
 && rm -rf /var/lib/apt/lists/*

# configuring pgAdmin4
COPY config/pgadmin4/config.py /usr/local/lib/python2.7/dist-packages/pgadmin4/

RUN export PGADMIN_SETUP_EMAIL=admin@admin.org && export PGADMIN_SETUP_PASSWORD=admin \
 && python /usr/local/lib/python2.7/dist-packages/pgadmin4/setup.py

COPY config/pgadmin4/pgAdmin4.py /usr/local/lib/python2.7/dist-packages/pgadmin4/

RUN chmod +x /usr/local/lib/python2.7/dist-packages/pgadmin4/pgAdmin4.py

COPY config/pgadmin4/pgadmin4.service /lib/systemd/system/

# installing plsh
WORKDIR /root/
RUN apt-get update && apt-get install -y git build-essential make \
 && git clone https://github.com/petere/plsh \
 && cd plsh && git checkout 9b108516a005a0592d9c9e145189825f92f820e1 \
 && make && make install \
 && rm -rf /root/plsh \
 && apt-get purge -y --auto-remove git build-essential make \
 && rm -rf /var/lib/apt/lists/*

# installing Lean and Z3
WORKDIR /root/
 RUN apt-get update && apt-get install -y wget unzip libgomp1\
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
 && apt-get purge -y --auto-remove wget unzip \
 && rm -rf /var/lib/apt/lists/*

RUN  mkdir -p /root/.lean

COPY config/lean/leanpkg.path /root/.lean/

# installing ocaml-4.07.0 and BIRDS
WORKDIR /root/
RUN apt-get update && apt-get install -y wget build-essential git make opam m4\
 && wget http://caml.inria.fr/pub/distrib/ocaml-4.07/ocaml-4.07.0.tar.gz \
 && tar -xzvf ocaml-4.07.0.tar.gz && cd ocaml-4.07.0 && ./configure && make world.opt && umask 022 && make install \ 
 && rm -rf /root/ocaml-4.07.0 \
 && rm /root/ocaml-4.07.0.tar.gz \
 && cd /usr/lib/ \
 && echo "y" | opam init && eval `opam config env` \
 && opam install -y num postgresql \
 && git clone https://github.com/dangtv/birds.git \
 && cd birds && make depend && make release \
 && mv release/birds /usr/bin/ \
 && make clean \
 && cd verification/ && leanpkg configure && leanpkg build \
 # clean
 && apt-get purge -y --auto-remove wget build-essential git make opam m4 \
 && rm -rf /root/.opam \
 && rm -rf /usr/local/lib/ocaml \
 && rm -rf /usr/local/bin/ocaml* \
 && rm -rf /var/lib/apt/lists/* 

WORKDIR /
COPY dockerentrypoint.sh /sbin/dockerentrypoint.sh
RUN chmod 755 /sbin/dockerentrypoint.sh

EXPOSE 5050 5432/tcp
VOLUME ["${PG_HOME}", "${PG_RUN_DIR}"]
# ENTRYPOINT ["dockerentrypoint.sh"]

CMD ["/sbin/dockerentrypoint.sh"]
# CMD ["/bin/bash"]
