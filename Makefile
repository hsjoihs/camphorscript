foo : ccsc ccsrc

ccsc : 
	ghc --make ccsc -o ccsc && chmod 755 ccsc

ccsrc : 
	ghc --make ccsrc -o ccsrc && chmod 755 ccsrc
