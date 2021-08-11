FROM quay.io/moxielogic/cl-etcd-test-base

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
