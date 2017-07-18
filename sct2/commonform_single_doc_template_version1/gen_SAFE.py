import re, os
# import typing

print("See ./README.md if you get an error.")

OUTROOT = "./generated/"

next_str_re = re.compile("((?:\*|b|d|c|m){1,4})\s+\"\"\"(.+?)\"\"\"", re.DOTALL)

with open("SAFE-combined.txt", 'r') as commonfile:
	common_contents = commonfile.read()
    
outnames = {
	'b' : "SAFE-cap-discount.gen",
	'c' : "SAFE-cap.gen",
	'd' : "SAFE-discount.gen",
	'm' : "SAFE-MFN.gen"
}
commonform_outpaths = {k:OUTROOT + outnames[k]+".commonform" for k in outnames}
docx_outpaths = {k:OUTROOT + outnames[k]+".docx" for k in outnames}

next_pos = 0
outstrings = {k:"" for k in outnames}
while True:
	match = next_str_re.search(common_contents,next_pos)	
	if not match:
		break	
	pattern = match.group(1)	
	thestr = match.group(2)
	next_pos = match.end() 
	
	for k in outnames.keys():
		if '*' in pattern or k in pattern:
			outstrings[k] += thestr

for k,path in commonform_outpaths.items():	
	with open(path,'w') as outfile:
		outfile.write(outstrings[k])

for k,outpath in docx_outpaths.items():
	inpath = commonform_outpaths[k]
	os.system(r"commonform render -f docx --mark-filled {} > {}".format(inpath,outpath))

print("\nOriginal CommonForm files are in ../3rdparties/commonform/commonform-samples")
print("\nGenerated files are in " + OUTROOT)