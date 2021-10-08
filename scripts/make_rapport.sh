#!/bin/bash
latexmk -output-directory=../mkrapport ../rapport.tex
mv ../mkrapport/rapport.pdf ../
