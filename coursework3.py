# This code picks each document and checks the topics it has been assigned to. 
# if the document has been assigned to more than one topic, then it creates a duplicate rows for
# each topic it is assigned to. 

import nltk
from nltk import word_tokenize
from nltk.corpus import stopwords
from nltk.stem.wordnet import WordNetLemmatizer 

lemmatizer1 = WordNetLemmatizer()

with open ("pre_processed_new3.csv", "r") as myfile:
 documents=myfile.readlines()

fout = open('pre_processed_new6.csv', 'a')

topics_list = ["topic.acq","topic.corn","topic.crude","topic.earn","topic.grain","topic.interest","topic.money.fx","topic.ship","topic.trade","topic.wheat"]

#word_corpus = []
loop=0
for doc in documents[1:]:
	contents = doc.split(",",15)

	features = contents[-1]
	title= contents[-2]
	pid = contents[0]
	filename = contents[1]
	purpose = contents[2]

	topics = contents[3:13]

	#print("Contents of the document are :**************************************")
	#print(raw)

	#print("topics = "+ str(topics))
	topics_num = [int(value) for value in topics]

	topics_sum = sum(topics_num)
	#print("Topic sum = "+ str(topics_sum))
	if(topics_sum == 0): continue
	
	if(topics_sum>0):
		for i in range(0,10):
			if(topics_num[i]==1):	
				# topics2 = ""
				# for j in  range(0,10):
				# 	if(j==i): 
				# 		topics2=topics2+"1"+","
				# 	else: 
				# 		topics2 = topics2+"0"+","
				# topics2 = topics2[0:-1]
				doc2 = pid+","+filename+","+purpose+","+topics_list[i]+","+title+","+features
				#print(doc2)
				fout.write(doc2)

	#else:
	# 	doc2 = "\n"+doc[:-1]
	# 	#print(doc2)
	# 	fout.write(doc2)
		
	if(loop%100==0):print(loop)
	loop=loop+1
	

