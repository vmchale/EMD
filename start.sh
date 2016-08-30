#!/bin/bash
export HASKPATH=$(pwd)/
(echo 'export HASKPATH='$(pwd)/) >> ~/.bashrc
(nproc --all) > haskemd/haskemd/processors
PATH=$PATH:~/.local/bin
(echo 'PATH=$PATH:~/.local/bin') >> ~/.bashrc
sudo apt-get install -y libdevil-dev
sudo apt-get install -y llvm
sudo apt-get install -y imagemagick
#brew install devil
#brew install llvm
#brew install imagemagick
wget -qO- https://get.haskellstack.org/ | sh
stack setup && stack build && stack install
sudo python3 haskemd/setup.py install
mkdir data/
#echo 'binaries built, remove libraries? (y/n) (1.8G)'
#sudo rm -r ~/.stack/
#sudo rm -r .stack-work/
#echo 'test program now? (y/n)'
