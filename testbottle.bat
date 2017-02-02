@echo off
ccsc -C18 _temp\Bottles__CS(w).txt -o tmp.hi
fc tmp.hi _temp\Bottles__BF_c.txt
del tmp.hi