#!/usr/bin/env sh
cd hsdata/
git fetch
LOCAL=$(git rev-parse @)
REMOTE=$(git rev-parse @{u})
if [ $LOCAL = $REMOTE ]; then
    git pull
    cd ../
    rm cards.db
    python3 -m hy tools/database.hy
fi
