#!/bin/bash
find . -name '.stack-work' -exec sudo rm -rf {} \;
stack install --local-bin-path bin --allow-different-user && upx bin/*