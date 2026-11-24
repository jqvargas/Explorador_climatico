FROM nginx:alpine
RUN apk add --no-cache gettext
COPY html /usr/share/nginx/html
COPY nginx.conf.template /etc/nginx/nginx.conf.template
COPY snippets /etc/nginx/snippets
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh
EXPOSE 80
ENTRYPOINT ["/docker-entrypoint.sh"]
