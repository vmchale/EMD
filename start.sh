#!/bin/bash
export HASKPATH=$(pwd)/
(echo 'export HASKPATH='$(pwd)/) >> ~/.bashrc
(nproc --all) > haskemd/haskemd/processors
sudo apt-get install -y libdevil-dev
sudo apt-get install -y llvm
sudo apt-get install -y imagemagick
