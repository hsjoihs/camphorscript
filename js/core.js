

function openFile(path){
	var fso = new ActiveXObject("Scripting.FileSystemObject");
	if(fso.FileExists(path)){
		var stream = fso.OpenTextFile(path);
		var result = stream.ReadAll();
		stream.Close();
		document.c.inp.value=result;
	}else{
	document.c.err.value="ファイルがありません";
	}
}

function runCmd(cmd)
{	
	var ws = new ActiveXObject("WScript.Shell");
	var ws_exec = ws.Exec(cmd);
	var res_exec = ws_exec.StdOut.ReadAll();
	var res_err = ws_exec.StdErr.ReadAll();
	if(res_err){document.c.err.value=res_err}
	document.c.out.value = res_exec;
}

function run()
{
	document.c.err.value = "";
	var from = document.c.from.selectedIndex;
	var i = document.c.to.selectedIndex;
	if(!langTable[from-1]){alert("変換元言語が選択されていません");return;}
	var to = toNum[langTable[from-1][i-1]]
	if(!to){alert("変換先言語が選択されていません");return;}
	var opt="-C";
	var compiler = "ccsc";
	if(from<to) //ccsc
	{
		opt += ""+from+(to-1) //植木算
		
	}
	else //ccsrc
	{
		opt += ""+(from+1)+to //植木算
		compiler="ccsrc"
	}
	runCmd(compiler+" "+opt+" \""+document.c.path.value+"\" -o con ");	
}