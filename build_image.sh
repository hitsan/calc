#!/bin/bash

export CUR_DIR=$(pwd)
export USER_ID=$(id -u)
export GROUP_ID=$(id -g)
export USER=$(whoami)

eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_rsa

docker compose build --no-cache