#!/bin/bash

eval "$(ssh-agent -s)"
export CUR_DIR=$(pwd)
export USER_ID=$(id -u)
export GROUP_ID=$(id -g)
export USER=$(whoami)
export SSH_AUTH_SOCK=$SSH_AUTH_SOCK
ssh-add ~/.ssh/id_rsa

docker compose up -d