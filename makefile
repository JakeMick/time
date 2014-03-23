help:
	@echo "-------------------------------------------------------------"
	@echo "Makefile for Forecasting Webserver "
	@echo " "
	@echo "Usage: "
	@echo " make host deps + init + build "
	@echo " make init Deletes output dir "
	@echo " make build Stages assets, compiles site, runs site "
	@echo " make deps Copies dependencies "
	@echo "-------------------------------------------------------------"


host: deps init build run

uglify:
	uglifyjs Static/js/script.js -o Static/js/script.min.js -m -c

assets: uglify css

init:

build:

test:

css:
	compass compile seeEssEss

run:
	cabal run

deps:
	@echo "------------------------------------"
	@echo "Copying dependencies to src/ folders"
	@echo "------------------------------------"

scss:
