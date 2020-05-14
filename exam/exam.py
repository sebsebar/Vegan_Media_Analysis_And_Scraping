# -*- coding: utf-8 -*-
#!/usr/bin/python

#import modules
import io, unicodedata
from afinn import Afinn
import os
import re

#add all text files from folder to a list of allFiles
allFiles = []
for root, dirs, files in os.walk('/Users/NinaDyrberg/Desktop/exam/'):
    for file in files:
        if file.startswith('article'):
            allFiles.append(file)

år = "2016-2017"
avis = "jyllandsposten"
filename = "jpv2016.csv"
header = "a_score,score,avis,år,file\n"
with open(filename, "w") as f:
    f.write(header)
# apply Afinn da 32
afinn = Afinn(language='da')
# length normalized score
def normScore(text):
    score=afinn.score(text)/len(text.split())
    return score

#cleaner for text
def clean(txt):
    txt = re.sub("[:\.\?()\&%,><@+\/\\\*]", "", txt)
    txt = re.sub("\[.*[\],\S*]", "", txt)
    txt = re.sub("-", "", txt)
    txt = re.sub("'", "", txt)
    txt=txt.lower()
    return txt


for file in allFiles:
    fileDirectory = '/Users/NinaDyrberg/Desktop/exam/'+file
    f = io.open(fileDirectory,'r',encoding='utf8')
    text = f.read()
    text=clean(text) 
    meanscore = normScore(text)
    ascore = afinn.score(text)
    summary = "{},{},{},{},{}\n".format(meanscore, ascore, avis, år, file)
    with open (filename, "a") as f:
        f.write(summary)
#    for word in text.split():
#        print afinn.score(word), word

#    text = unicodedata.normalize('NFKD', file) # normalize and ascii encode
#    score=normScore(text)

