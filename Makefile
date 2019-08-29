foo : 
	stack build :ccsc --copy-bins --local-bin-path . && chmod 755 ccsc
	stack build :ccsrc --copy-bins --local-bin-path . && chmod 755 ccsrc
