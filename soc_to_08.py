#writing will be done to the mainfile
import csv
from statistics import mean

#mainfile = open("corrtab88-08.txt", 'r')

riskmap = open("frey_osborne_risk_map.txt", 'r')

#make dictionary for isco-08 and SOC 2010
#String[] bar = foo.split("\t")

#isco_soc_crosswalk =  open("isco_soc_crosswalk.txt", 'r')
#isco_soc_crosswalk = isco_soc_crosswalk.read()

key08 = []
key08title = []
valsoc = []

with open("isco_soc_crosswalk.txt", 'r') as crosswalktvs:
	crosswalk = csv.DictReader(crosswalktvs, delimiter = "\t")
	for line in crosswalk:
		key08.append(line["ISCO-08_Code"].strip())
		key08title.append(line["ISCO-08_Title"].strip())
		valsoc.append(line["2010 _SOC_Code"].strip())


#08 to SOC
dict08soc = {} #dict(zip(key08, valsoc))
#dict08title = dict(zip(key08, key08title))

for k, v in zip(key08, valsoc):
	if k in dict08soc:
		dict08soc[k].append(v)
	else:
		dict08soc[k] = [v]
	

#Probabilities to SOC
prob = []
soc = []

with open("frey_osborne_risk_map.txt", "r") as riskmaptvs:
	riskmap = csv.DictReader(riskmaptvs, delimiter="\t")
	for line in riskmap:
		soc.append(line["SOC code"].strip())
		prob.append(line["Probability"].strip())


dictprob = dict(zip(soc,prob))

#print(dictprob)

#take average probability
probs08 = {}

for isco08, socs in dict08soc.items():
	probs = []
	for code in socs:
		if code in dictprob:
			probs.append(float(dictprob[code]))
	if len(probs) != 0:
		probs08[isco08] = round(mean(probs),3)

 
#write probabilities to file with 08/88
with open('corrtab88-08.txt', 'r') as corrinput:
	with open('corrtab88-08 - Copy.txt', 'w') as corroutput:
		writer = csv.writer(corroutput, lineterminator='\n', delimiter='\t')
		reader = csv.reader(corrinput, delimiter = "\t")

		all = []
		row = next(reader) 
		row.append('probability')
		all.append(row)

		for row in reader:
			if row[4] in probs08:
				row.append(probs08[row[4]])
				all.append(row)	

		writer.writerows(all)

