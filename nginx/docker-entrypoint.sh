#!/bin/sh
set -e

export API_KEY="${API_KEY:-}"
export CLERK_PUBLISHABLE_KEY="${CLERK_PUBLISHABLE_KEY:-}"
export CLERK_ACCOUNT_URL="${CLERK_ACCOUNT_URL:-https://accounts.rodaja.cl}"
export STRIPE_PUBLISHABLE_KEY="${STRIPE_PUBLISHABLE_KEY:-}"
export GTM_CONTAINER_ID="${GTM_CONTAINER_ID:-}"
export GA4_MEASUREMENT_ID="${GA4_MEASUREMENT_ID:-}"
export ADSENSE_PUBLISHER_ID="${ADSENSE_PUBLISHER_ID:-}"
export SITE_CANONICAL_BASE="${SITE_CANONICAL_BASE:-https://app.rodaja.cl}"
export SHINY_AT_ROOT="${SHINY_AT_ROOT:-0}"

CONF="/etc/nginx/conf.d/default.conf"
SNIP="/etc/nginx/snippets"
HTML="/usr/share/nginx/html"
PARTIALS="$HTML/_partials"

if [ "$SHINY_AT_ROOT" = "1" ]; then
  EXPLORADOR_SNIP="$SNIP/explorador-proxy.conf"
  ROOT_SNIP="$SNIP/root-shiny.conf"
  echo "[nginx] SHINY_AT_ROOT=1 (portada en /; Explorador en /explorador/; catch-all Shiny en /)"
else
  EXPLORADOR_SNIP="$SNIP/explorador-proxy.conf"
  ROOT_SNIP="$SNIP/root-static.conf"
  echo "[nginx] SHINY_AT_ROOT=0 (landing en /, Explorador en /explorador/)"
fi

envsubst '${API_KEY}' < /etc/nginx/nginx.conf.template > "$CONF"

awk -v efile="$EXPLORADOR_SNIP" -v rfile="$ROOT_SNIP" '
  $0 == "__EXPLORADOR_BLOCK__" {
    while ((getline line < efile) > 0) print line
    close(efile)
    next
  }
  $0 == "__ROOT_BLOCK__" {
    while ((getline line < rfile) > 0) print line
    close(rfile)
    next
  }
  { print }
' "$CONF" > "${CONF}.tmp" && mv "${CONF}.tmp" "$CONF"

if [ -f "$HTML/rodaja-config.js.template" ]; then
  envsubst '${CLERK_PUBLISHABLE_KEY} ${CLERK_ACCOUNT_URL} ${STRIPE_PUBLISHABLE_KEY} ${GTM_CONTAINER_ID} ${GA4_MEASUREMENT_ID} ${ADSENSE_PUBLISHER_ID} ${SITE_CANONICAL_BASE} ${CARTO_BASEMAP_KEY}' \
    < "$HTML/rodaja-config.js.template" \
    > "$HTML/rodaja-config.js"
fi

# GTM partials (vacios si no hay container ID)
mkdir -p "$PARTIALS" 2>/dev/null || true
if [ -n "$GTM_CONTAINER_ID" ] && [ -f "$PARTIALS/gtm-head.html.template" ]; then
  envsubst '${GTM_CONTAINER_ID}' < "$PARTIALS/gtm-head.html.template" > "$PARTIALS/gtm-head.html"
  envsubst '${GTM_CONTAINER_ID}' < "$PARTIALS/gtm-body.html.template" > "$PARTIALS/gtm-body.html"
  echo "[nginx] GTM partials generados ($GTM_CONTAINER_ID)"
else
  printf '<!-- GTM disabled -->\n' > "$PARTIALS/gtm-head.html" 2>/dev/null || true
  printf '<!-- GTM disabled -->\n' > "$PARTIALS/gtm-body.html" 2>/dev/null || true
fi

# ads.txt (publisher ID sin prefijo ca-)
if [ -n "$ADSENSE_PUBLISHER_ID" ] && [ -f "$HTML/ads.txt.template" ]; then
  PUB="$ADSENSE_PUBLISHER_ID"
  PUB="${PUB#ca-}"
  export ADSENSE_PUBLISHER_ID="$PUB"
  envsubst '${ADSENSE_PUBLISHER_ID}' < "$HTML/ads.txt.template" > "$HTML/ads.txt"
  echo "[nginx] ads.txt generado"
else
  printf '# ads.txt — configure ADSENSE_PUBLISHER_ID en .env\n' > "$HTML/ads.txt" 2>/dev/null || true
fi

if [ -n "$CLERK_PUBLISHABLE_KEY" ]; then
  mkdir -p "$HTML/descargar" 2>/dev/null || true
  GTM_JS=""
  if [ -n "$GTM_CONTAINER_ID" ]; then
    GTM_JS=", gtmId: \"$GTM_CONTAINER_ID\""
  fi
  if ! printf 'window.RODAJA_CONFIG = { clerkPublishableKey: "%s", accountUrl: "%s", apiBase: "/api"%s };\n' \
    "$CLERK_PUBLISHABLE_KEY" "$CLERK_ACCOUNT_URL" "$GTM_JS" \
    > "$HTML/descargar/config.js" 2>/dev/null; then
    echo "[nginx] warn: no se pudo escribir descargar/config.js (directorio de solo lectura?)"
  fi
  mkdir -p "$HTML/alertas" 2>/dev/null || true
  if ! printf 'window.RODAJA_CONFIG = { clerkPublishableKey: "%s", accountUrl: "%s", apiBase: "/api"%s };\n' \
    "$CLERK_PUBLISHABLE_KEY" "$CLERK_ACCOUNT_URL" "$GTM_JS" \
    > "$HTML/alertas/config.js" 2>/dev/null; then
    echo "[nginx] warn: no se pudo escribir alertas/config.js (directorio de solo lectura?)"
  fi
fi

echo "[nginx] Config generada (API_KEY $([ -n "$API_KEY" ] && echo set || echo unset), Clerk $([ -n "$CLERK_PUBLISHABLE_KEY" ] && echo set || echo unset), GTM $([ -n "$GTM_CONTAINER_ID" ] && echo set || echo unset))"

# Access log persistente: carpeta montada desde ./logs/nginx; retencion 90 dias.
mkdir -p /var/log/nginx/rodaja 2>/dev/null || true
chown nginx:nginx /var/log/nginx/rodaja 2>/dev/null || true
( while true; do find /var/log/nginx/rodaja -name 'access-*.log' -mtime +90 -delete 2>/dev/null; sleep 86400; done ) &

exec nginx -g "daemon off;"
