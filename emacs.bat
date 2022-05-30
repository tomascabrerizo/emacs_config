@echo off

rem start /d".\emacs-28.1\bin\" runemacs.exe %*
.\emacs-28.1\bin\runemacs.exe -l ".\init.el" %*
