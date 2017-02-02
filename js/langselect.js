function init()
{
	document.c.from.selectedIndex=1;setTo();
	document.c.to.selectedIndex=6;
}

function setTo()
{
	var j = document.c.from.selectedIndex;
	for (var i=1; i<7; i++){
		document.c.to.options[i].text=j?toReal[langTable[j-1][i-1]]:""
	}
	document.c.to.selectedIndex=0;
}
var langTable = [
	["PCS","HCCS","CCS","ND","BF","BF_c"], // CS
	["HCCS","CCS","ND","BF","BF_c"], // PCS
	["CCS","ND","BF","BF_c"], // HCCS
	["ND","BF","BF_c"], // CCS
	["CCS_rev","BF","BF_c"], // ND
	["CCS_rev","ND_rev","BF_c"], // BF
	["CCS_rev","ND_rev","BF_i"], // BF_c
]
var toReal = {
	CS     :"CamphorScript",
	PCS    :"PreprocessedCamphorScript",
	HCCS   :"HalfCompiledCamphorScript",
	CCS    :"CompiledCamphorScript",
	CCS_rev:"CompiledCamphorScript(reverse compiled)",
	ND     :"NouDarake",
	ND_rev :"NouDarake(reversed compiled)",
	BF     :"Brainf*ck(with spaces)",
	BF_c   :"Brainf*ck(without spaces)",
	BF_i   :"Indented Brainf*ck(reversed compiled)",
	""     :"",
	undefined:""
	}

var toNum = {
	CS     :1,
	PCS    :2,
	HCCS   :3,
	CCS    :4,
	CCS_rev:4,
	ND     :7,
	ND_rev :7,
	BF     :8,
	BF_c   :9,
	BF_i   :8,
	""     :0,
	undefined:0
	}