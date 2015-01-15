@echo off

rem This script requires that you first download and install Pandoc.
rem See http://johnmacfarlane.net/pandoc/ for more information.

if not exist .\html\*.* mkdir html

cd .\FigaroGuide
pandoc -r latex -w html -s -o ..\html\FigaroGuide.html FigaroGuide.tex

cd ..\Tutorial
pandoc -r latex -w html -s -o ..\html\FigaroTutorial.html FigaroTutorial.tex
