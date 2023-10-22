#!/bin/bash

stack build --pedantic 2>&1 | tee build.log
