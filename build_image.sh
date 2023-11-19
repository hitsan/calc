#!/bin/bash

export CUR_DIR=$(pwd)
export USER_ID=$(id -u)
export GROUP_ID=$(id -g)
export USER=$(whoami)

docker-compose build