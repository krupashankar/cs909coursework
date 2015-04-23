# Rerunning the classifier by training on the 'train' records and testing on the 'test' records
# The feature set used is taken from the terms obtained for LDA topic modelling done on the entire data set.

train_set<- data1[which(data1$purpose=="train"),]
test_set<- data1[which(data1$purpose=="test"),]

corpus_train<- Corpus(VectorSource(train_set$features))
corpus_test<- Corpus(VectorSource(test_set$features))

dtm_train<- DocumentTermMatrix(corpus_train,control=list(wordLengths=c(3,Inf),stopwords = TRUE,dictionary=topic_model_word_list))
dtm_test<- DocumentTermMatrix(corpus_test,control=list(wordLengths=c(3,Inf),stopwords = TRUE,dictionary=topic_model_word_list))

training <- as.data.frame(as.matrix(dtm_train))
test <- as.data.frame(as.matrix(dtm_test))

training <- cbind(training,train_set$topic)
test <- cbind(test,test_set$topic)

names(training)[names(training)=="train_set$topic"]="topic"
names(test)[names(test)=="test_set$topic"]="topic"



#SVM 
SVM_model<- svm(training$topic~.,data=training)
SVM_predictions<- predict(SVM_model,test)
result_SVM_final<- table(observed= test[,"topic"],predicted=SVM_predictions)
print("Confusion matrix from SVM is:")
print(result_SVM_final)
table3_final<- measures(result_SVM_final)
print("Measurements for SVM are:")
print(table3_final)

print("Average Measures of the testing are")
avg_measure_final <- average_measures(result_SVM_final)
avg_measure_final<- rbind(avg_measure_final,c(0,0,0,0,0))
colnames(avg_measure_final)<-c("Avg. Micro Recall","Avg. Micro Precision","Avg. Macro Recall","Avg. Macro Precision","Accuracy")
print(avg_measure_final)
