#!/bin/sh
brew install p7zip
pip3 install runipy
pip3 install pandas
pip3 install pyunpack
pip3 install beautifulsoup4
pip3 install requests
pip3 install patool

runipy json_converter.ipynb
Rscript clean.r 



