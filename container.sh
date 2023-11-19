#!/bin/bash

export CUR_DIR=$(pwd)
export USER_ID=$(id -u)
export GROUP_ID=$(id -g)
export USER=$(whoami)

if [ "$1" = "up" ]; then
    docker compose up -d
elif [ "$1" = "down" ]; then
    docker compose down
else
    echo "Invalid argument. Please specify 'up' or 'down'."
fi