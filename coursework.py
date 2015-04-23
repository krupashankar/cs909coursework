# Preprocessing of the data - This program extracts feature words from the text in the articles and
# places it in another column in a new file called pre_processed.tsv


import nltk
from nltk import word_tokenize
from nltk.corpus import stopwords
from nltk.stem.wordnet import WordNetLemmatizer 

lemmatizer1 = WordNetLemmatizer()

with open ("reutersCSV.csv", "r") as myfile:
 documents=myfile.readlines()

fout = open('pre_processed_new.csv', 'a')


#word_corpus = []
loop=0
for doc in documents[0:]:
	contents = doc.split(",",139)
	#print("Contents of the document are :**************************************")
	raw = contents[-1]
	#print(raw)
	tokens = word_tokenize(raw)
	#print("Contents split into tokens are = ",len(tokens),"************************************")
	#print(tokens)

	pos_tags = nltk.pos_tag(tokens)
	#print("Pos Tagging = ********************************************************")
	#print(pos_tags)

	wordnet_tags = {'NN':'n','JJ':'a','VB':'v','RB':'r'}
	lem_tags = ['NN','JJ','VB','RB']
	
	tokens_lem = []

	for (word,tag) in pos_tags:
		if tag[0:2] in lem_tags:
			tag = wordnet_tags[tag[0:2]]
			tokens_lem.append(unicode(lemmatizer1.lemmatize(word,tag)))
		else:
			tokens_lem.append(unicode(lemmatizer1.lemmatize(word,'n')))

	#print("Lemmatized tokens =",len(tokens_lem),"***********************************************")
	#print(tokens_lem)

	tokens2 = [token for token in tokens_lem if token.isalpha()]
	#print("Tokens after removing numerals and symbols = ",len(tokens2),"***************************")
	#print(tokens2)


	tokens3 = [token.lower() for token in tokens2]
	#tokens3 = set(tokens3)
	#print("Tokens after removing duplicates = ",len(tokens3),"***************************")

	tokens4 = [token for token in tokens3 if token not in stopwords.words("english")]
	#print("Tokens after removing stopwords = ",len(tokens4),"***************************")
	#print(tokens3)

	#print(tokens4)
	features = "("
	for i in tokens4:
		features = features + i +";"
	features = features[:-1] + ")"
	
	doc2 = "\n"+features+","+doc[:-1]
	#print(doc2)
	fout.write(doc2)
	#word_corpus = word_corpus + tokens4
	if(loop%100==0):print(loop)
	loop=loop+1
	

