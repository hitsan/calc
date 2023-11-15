#!/bin/bash
USER_ID=$(id -u)
GROUP_ID=$(id -g)
USER_NAME=hitsan
groupadd -g $GROUP_ID $USER_NAME
useradd -m -s /bin/bash -u $USER_ID -g $GROUP_ID $USER_NAME