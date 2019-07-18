import unidecode
import codecs

fw = open("shark_attacks_cln.csv", "w")

with codecs.open("shark_attacks.csv", "rb", encoding='utf-8', errors='ignore') as f:
#with codecs.open("shark_attacks.csv", "rb", encoding='utf-8') as f:
	for line in f:
		fw.write("{}\n".format(unidecode.unidecode(line.rstrip("\n"))))
fw.close()
f.close()
