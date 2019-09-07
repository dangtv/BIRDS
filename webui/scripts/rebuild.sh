#!/bin/bash

# Get directory script is in
SCRIPTS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
cd $SCRIPTS_DIR/..
SQLPAD_DIR=`pwd`

# Build front-end
npm run build --prefix "$SQLPAD_DIR/client"

# Copy front-end build to server directory
rm -rf server/public
mkdir server/public
cp -r ./client/build/* ./server/public
