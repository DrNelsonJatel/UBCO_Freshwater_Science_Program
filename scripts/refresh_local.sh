#!/bin/bash
# scripts/refresh_local.sh
#
# Local daily refresh of the UBCO Freshwater Science course data.
#
# WHY LOCAL: the UBC Okanagan calendar server resets connections from
# GitHub's cloud runners (datacenter-IP WAF block), so the scrape cannot
# run in GitHub Actions. It runs here, from a network UBCO allows, then
# pushes the refreshed data. The push triggers the deploy-site workflow,
# which republishes the Quarto site in the cloud. The planner is
# redeployed from here only when course CONTENT actually changes.
#
# Scheduled by launchd: scripts/launchd/ca.limnology.ubco-fw-refresh.plist
# (installed to ~/Library/LaunchAgents/). Also runnable by hand.

export PATH="/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin"

REPO="$(cd "$(dirname "$0")/.." && pwd)"
cd "$REPO" || { echo "cannot cd to repo: $REPO"; exit 1; }

stamp() { date "+%Y-%m-%d %H:%M:%S %Z"; }
echo "===== refresh_local.sh start $(stamp) ====="

# 1. Scrape -> data/courses.parquet + data/scrape_meta.json.
if ! Rscript data-raw/01_scrape_ubco_calendar.R; then
  echo "[$(stamp)] SCRAPE FAILED; data left untouched."
  exit 1
fi

# 2. Did course CONTENT change, or only the scrape_meta timestamp?
CONTENT_CHANGED=false
git diff --quiet -- data/courses.parquet || CONTENT_CHANGED=true

# 3. Commit + push the refreshed data. scrape_meta changes on every run,
#    so a healthy job lands a daily commit; deploy-site then republishes
#    the site (which makes the "last refreshed" stamp current).
if git diff --quiet -- data/; then
  echo "[$(stamp)] no data changes (unexpected: scrape_meta should change)."
else
  if $CONTENT_CHANGED; then
    MSG="data: local UBCO calendar refresh, content updated ($(date -u +%Y-%m-%d))"
  else
    MSG="data: local UBCO calendar refresh, no content change ($(date -u +%Y-%m-%d))"
  fi
  git add data/
  git commit -m "$MSG"
  if git push; then
    echo "[$(stamp)] pushed: $MSG"
  else
    echo "[$(stamp)] GIT PUSH FAILED."
    exit 1
  fi
fi

# 4. Redeploy the planner only when course content changed (its bundled
#    data + freshness badge then update). Between content changes the app
#    shows the live "source last checked" date by fetching the published
#    scrape_meta.json, so it does not need a redeploy every day.
if $CONTENT_CHANGED; then
  echo "[$(stamp)] course content changed -> redeploying planner..."
  if Rscript scripts/deploy.R; then
    echo "[$(stamp)] planner redeployed."
  else
    echo "[$(stamp)] PLANNER DEPLOY FAILED (data already pushed; rerun scripts/deploy.R)."
  fi
else
  echo "[$(stamp)] no content change; planner redeploy skipped."
fi

echo "===== refresh_local.sh done $(stamp) ====="
