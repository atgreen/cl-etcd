FROM quay.io/containerlisp/lisp-10-ubi8

RUN curl -L https://github.com/etcd-io/etcd/releases/download/v3.5.0/etcd-v3.5.0-linux-amd64.tar.gz | tar xvfz - && mkdir /opt/app-root/src/bin && mv etcd-v3.5.0-linux-amd64/* /opt/app-root/src/bin
COPY test /tmp/src
COPY . /opt/app-root/quicklisp/local-projects/cl-etcd
RUN ls -l /opt/app-root/quicklisp/local-projects/cl-etcd
USER 0
RUN chown -R 1001.1001 /opt/app-root
USER 1001
ARG APP_VERSION=APP_VERSION
ENV APP_VERSION=${APP_VERSION}
RUN APP_SYSTEM_NAME=etcd-test /usr/libexec/s2i/assemble
CMD APP_SYSTEM_NAME=etcd-test APP_EVAL="\"(etcd-test:start)\"" /usr/libexec/s2i/run
