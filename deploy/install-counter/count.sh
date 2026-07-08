#!/bin/sh
# Rolling 30-day active-installs counter.
#
# Once a day: ask Tempo for the distinct values of the span.installation.id
# attribute over the trailing 30 days, count them, and publish the count to
# Pushgateway as cellar_active_installs_30d. Prometheus scrapes it from there
# and retains it long-term. Only the aggregate count is stored — no
# installation.id ever reaches Prometheus, preserving the "no pseudonymous
# identifier in the metrics store" invariant.
#
# The count is approximate by design: it is a rolling MAU-style figure bounded
# by trace retention (30d), sampling, and Tempo's max_bytes_per_tag_values_query
# response cap. When the response nears that cap the count would silently
# under-report, so we warn rather than publish a wrong number quietly.
set -eu

TEMPO_URL=http://tempo:3200
PUSHGATEWAY_URL=http://pushgateway:9091
WINDOW_SECONDS=2592000   # 30 days
INTERVAL_SECONDS=86400   # once a day, after a successful push
RETRY_SECONDS=60         # after a failure (e.g. Tempo not yet ready)
# ~90% of Tempo's default 5 MB max_bytes_per_tag_values_query. Past this the
# tag-values response may be truncated and the count under-reports.
CAP_WARN_BYTES=4718592

apk add --no-cache curl jq >/dev/null

while true; do
  end="$(date +%s)"
  start="$((end - WINDOW_SECONDS))"
  sleep_for="$RETRY_SECONDS"

  if body="$(curl -sf "${TEMPO_URL}/api/v2/search/tag/span.installation.id/values?start=${start}&end=${end}")"; then
    count="$(printf '%s' "$body" | jq '.tagValues | length')"
    bytes=${#body}

    if [ "$bytes" -ge "$CAP_WARN_BYTES" ]; then
      echo "WARN: tag-values response is ${bytes} bytes, near max_bytes_per_tag_values_query cap; count ${count} may be truncated" >&2
    fi

    if printf 'cellar_active_installs_30d %s\n' "$count" \
        | curl -sf --data-binary @- "${PUSHGATEWAY_URL}/metrics/job/install_counter"; then
      echo "pushed cellar_active_installs_30d=${count}"
      sleep_for="$INTERVAL_SECONDS"
    else
      echo "ERROR: push to Pushgateway failed" >&2
    fi
  else
    echo "ERROR: Tempo tag-values query failed" >&2
  fi

  sleep "$sleep_for"
done
