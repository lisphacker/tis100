#!/bin/bash

./build.sh
stack run tissim -- examples/segment00150/segment00150.asm -c examples/segment00150/segment00150.cfg 2>&1 | tee test.log
# stack run tissim -- examples/segment20176/segment20176.asm -c examples/segment20176/segment20176.cfg 2>&1 | tee test.log
