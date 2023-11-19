#!/bin/bash
USER_ID=$(id -u)
GROUP_ID=$(id -g)
USER_NAME=$(id -un)
groupadd -g $GROUP_ID $USER_NAME
useradd -m -s /bin/bash -u $USER_ID -g $GROUP_ID $USER_NAME
exec "$@"