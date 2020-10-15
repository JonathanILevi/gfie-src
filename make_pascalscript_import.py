#!/usr/bin/env python
import os,sys,re,math,time,random

units = []
mainInclude = "PSImports"+os.sep+"uPSI_AllMySources.pas"
include_graph = {}
for fn in os.listdir('.'):
	fnlower = fn.lower()
	if fnlower.endswith(".pas") and not fnlower.startswith("upsi_"):
		# is the unit Windows-specific?
		with open(fn) as f:
			s = f.read()
		if s.lower().find("unit is specific to windows") >= 0 and sys.platform.find("linux") >= 0:
			print(fn+" is Windows-specific, skipping")
			continue
		
		unit = fn.replace(".pas", "")
		mt = os.path.getmtime(fn)
		try:
			mtInclude = os.path.getmtime("PSImports"+os.sep+"uPSI_"+fn)
		except:
			mtInclude = -1
		unit_ok = True
		if mt >= mtInclude:
			os.system("pascalscript"+os.sep+"unit-importing"+os.sep+"CMDimp "+fn)
			if not os.path.exists("uPSI_"+fn) and (mtInclude < 0):
				print(fn+" could not be converted")
				unit_ok = False
		else:
			print(fn+" has not changed")
		if unit_ok:
			units.append(unit)
			include_graph[unit] = {}
os.system("mv uPSI_*.pas PSImports")

for unit in units:
	with open(unit + ".pas") as fu:
		s = fu.read()
	s = s[s.lower().find("uses"):]
	s = s[:s.find(";")+1]
	for unit2 in units:
		if unit2 != unit:
			if s.lower().find(unit2.lower()+",") >= 0 or s.lower().find(unit2.lower()+";") >= 0:
				#print(unit+" includes "+unit2)
				include_graph[unit][unit2] = True

units_sorted = []
while len(include_graph) > 0:
	found = False
	for u in units:
		if (u in include_graph) and len(include_graph[u]) == 0:
			units_sorted.append(u)
			#print(u)
			for u2 in units:
				if (u2 in include_graph) and (u in include_graph[u2]):
					del include_graph[u2][u]
			del include_graph[u]
			found = True
	if not found:
		for u in units:
			if (u in include_graph):
				print(u+ " ! " + str(len(include_graph[u])))
		break

units = units_sorted

with open(mainInclude, "w") as f:
	f.write("""unit uPSI_AllMySources;

// This unit has been automatically generated.
interface

uses
  SysUtils, Classes, uPSComponent, uPSRuntime, uPSCompiler;

procedure PSImportAllMySources(PSScript1: TPSScript);

implementation

uses
  """);

	first = True
	for unit in units:
		if not first:
			f.write(", ")
		f.write("uPSI_"+unit)
		first = False;

	f.write(""";

procedure PSImportAllMySources(PSScript1: TPSScript);
begin
""");
	for unit in units:
		f.write("  TPSPluginItem(PSScript1.Plugins.Add).Plugin := TPSImport_"+unit+".Create(PSScript1);\n")
	f.write("""end;

end.
""")
