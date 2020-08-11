FROM postgres:latest
ENV POSTGRES_USER snouser
ENV POSTGRES_PASSWORD sno001
ENV POSTGRES_DB sno

# ...
COPY snotest.dump snotest.dump
#COPY docker-entrypoint-initdb.d/*.sh /docker-entrypoint-initdb.d/
# ...

# Run restore
#RUN ["chmod", "+x", "docker-entrypoint-initdb.d/restore-db.sh"]
