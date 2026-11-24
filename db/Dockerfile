# Imagen de la BD = la misma timescaledb que corria en produccion (fijada por digest)
# + PostGIS compilado contra su PostgreSQL. Mismo Alpine, mismo PG 15.18, mismo
# timescaledb 2.25.2 y mismo PGDATA: el volumen ./pgdata se monta tal cual.
# Receta basada en postgis/docker-postgis (variante alpine).
FROM timescale/timescaledb@sha256:b7aba9c1fcff0bf219b8aefdb4e8bfc1a673c37de761425d3fbf08d24860a854

ENV POSTGIS_VERSION=3.6.1

RUN set -eux \
    && apk add --no-cache --virtual .fetch-deps ca-certificates openssl tar \
    && wget -O /tmp/postgis.tar.gz "https://download.osgeo.org/postgis/source/postgis-${POSTGIS_VERSION}.tar.gz" \
    && mkdir -p /usr/src/postgis \
    && tar --extract --file /tmp/postgis.tar.gz --directory /usr/src/postgis --strip-components 1 \
    && rm /tmp/postgis.tar.gz \
    && apk add --no-cache --virtual .build-deps \
        $DOCKER_PG_LLVM_DEPS \
        gdal-dev geos-dev proj-dev proj-util \
        file g++ gcc gettext-dev json-c-dev libtool libxml2-dev make pcre2-dev perl protobuf-c-dev \
    && cd /usr/src/postgis \
    && ./configure --without-sfcgal \
    && make -j"$(nproc)" \
    && make install \
    && apk add --no-cache --virtual .postgis-rundeps \
        gdal geos proj json-c libstdc++ pcre2 protobuf-c ca-certificates \
    && cd / \
    && rm -rf /usr/src/postgis \
    && apk del .fetch-deps .build-deps \
    && ls /usr/local/share/postgresql/extension/postgis.control
