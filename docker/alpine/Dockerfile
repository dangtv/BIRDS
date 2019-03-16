FROM postgres:9.6-alpine
FROM python:3.7-alpine3.9
MAINTAINER dangtv <dangtv18@gmail.com>

ENV LANG=${OS_LOCALE} \
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

RUN apk add --no-cache python python-dev py-pip build-base wget git unzip opam m4 \
 && echo "host all all 0.0.0.0/0 trust" >> ${PG_CONF_DIR}/pg_hba.conf \
 && echo "local all all md5" >> ${PG_CONF_DIR}/pg_hba.conf \
 && echo "listen_addresses='*'" >> ${PG_CONF_DIR}/postgresql.conf

WORKDIR /root/
RUN wget http://caml.inria.fr/pub/distrib/ocaml-4.07/ocaml-${OCAML_VERSION}.tar.gz \
 && tar -xzvf ocaml-4.07.0.tar.gz && cd ocaml-4.07.0 && ./configure && make world.opt && umask 022 && make install \
 && rm -rf /root/ocaml-4.07.0

RUN pip --no-cache-dir install https://ftp.postgresql.org/pub/pgadmin/pgadmin4/v4.3/pip/pgadmin4-4.3-py2.py3-none-any.whl \
 && export PGADMIN_SETUP_EMAIL=admin@admin.org && export PGADMIN_SETUP_PASSWORD=admin \
 && python /usr/local/lib/python2.7/dist-packages/pgadmin4/setup.py

COPY config/pgadmin4/config.py /usr/local/lib/python2.7/dist-packages/pgadmin4/

RUN python /usr/local/lib/python2.7/dist-packages/pgadmin4/setup.py

COPY config/pgadmin4/pgAdmin4.py /usr/local/lib/python2.7/dist-packages/pgadmin4/

RUN chmod +x /usr/local/lib/python2.7/dist-packages/pgadmin4/pgAdmin4.py

COPY config/pgadmin4/pgadmin4.service /lib/systemd/system/

WORKDIR /root/
RUN git clone https://github.com/petere/plsh \
 && cd plsh && git checkout 9b108516a005a0592d9c9e145189825f92f820e1 \
 && make && make install \
 && wget https://github.com/leanprover/lean/releases/download/v3.4.2/lean-3.4.2-linux.tar.gz \
 && tar -xzvf lean-3.4.2-linux.tar.gz \ 
 && mv lean-3.4.2-linux /usr/lib/lean \ 
 && ln -s /usr/lib/lean/bin/lean /usr/bin/lean \
 && ln -s /usr/lib/lean/bin/leanpkg /usr/bin/leanpkg \
 && ln -s /usr/lib/lean/bin/leanchecker /usr/bin/leanchecker \
 && wget https://github.com/Z3Prover/z3/releases/download/z3-4.8.4/z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04.zip \
 && unzip z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04.zip \
 && mv z3-4.8.4.d6df51951f4c-x64-ubuntu-16.04 /usr/lib/z3 \
 && ln -s /usr/lib/z3/bin/z3 /usr/bin/z3

RUN mkdir -p /usr/lib/birds \
 && mkdir -p /usr/lib/birds/src \
 && mkdir -p /usr/lib/birds/verification \
 && mkdir -p /root/.lean

COPY config/lean/leanpkg.path /root/.lean/
COPY src /usr/lib/birds/src/
COPY verification /usr/lib/birds/verification/
COPY Makefile /usr/lib/birds

WORKDIR /usr/lib/birds
RUN opam install -y num postgresql \
 && make depend && make release \
 && cp release/birds /usr/bin/

WORKDIR /usr/lib/birds/verification
RUN leanpkg configure && leanpkg build 

WORKDIR /
COPY dockerentrypoint.sh /sbin/dockerentrypoint.sh
RUN chmod 755 /sbin/dockerentrypoint.sh

EXPOSE 5050 5432/tcp
VOLUME ["${PG_HOME}", "${PG_RUN_DIR}"]
# ENTRYPOINT ["dockerentrypoint.sh"]

CMD ["/sbin/dockerentrypoint.sh"]
# CMD ["/bin/bash"]
