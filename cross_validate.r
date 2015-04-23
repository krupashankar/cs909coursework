data1<- read.csv("C:/Python27/krupa/pre_processed_new6.csv",stringsAsFactors=FALSE)
dim(data1)

corpus_full<- Corpus(VectorSource(data1$features))
dtm_full<- DocumentTermMatrix(corpus_full,control=list(wordLengths=c(3,Inf),stopwords = TRUE))
dim(dtm_full)
#word_matrix = as.data.frame(as.matrix(dtm_full))


# Features from Topic modelling
row_sum<- row_sums(dtm_full)
row_sum[which(row_sum<1)]

dtm_full2<- dtm_full[-c(5821,5822),]
LDA1<-LDA(dtm_full2,10, method = "Gibbs")
# Get the list of 50 significant words for each topic
topic_model_word_list <- as.vector(terms(LDA1, k=50))
dtm_topic_model <- DocumentTermMatrix(corpus_full,control=list(wordLengths=c(3,Inf),stopwords = TRUE,dictionary=topic_model_word_list))


# Features from TF-IDF scores
term_tfidf <-tapply(dtm_full$v/row_sums(dtm_full)[dtm_full$i], dtm_full$j, mean) * log2(nDocs(dtm_full)/col_sums(dtm_full > 0))
dtm_tfidf <- dtm_full[, term_tfidf >= 0.5]

dim(dtm_topic_model)
dim(dtm_tfidf)

dtm_tfidf <- dtm_full[, term_tfidf >= 0.9]
dim(dtm_tfidf)

word_matrix_tm = as.data.frame(as.matrix(dtm_topic_model))
word_matrix_tfidf = as.data.frame(as.matrix(dtm_tfidf))

word_matrix_tm<- cbind(word_matrix_tm,data1$topic)
word_matrix_tfidf<- cbind(word_matrix_tfidf,data1$topic)

names(word_matrix_tm)[names(word_matrix_tm)=="data1$topic"]="topic"
names(word_matrix_tfidf)[names(word_matrix_tfidf)=="data1$topic"]="topic"

# K FOLD CROSS VALIDATION
set.seed(111)
selected_rows<- nrow(word_matrix_tfidf)- (nrow(word_matrix_tfidf)%%10)
ai2<- word_matrix_tfidf[1:selected_rows,]


# split the data into 10 folds
# creates a column of indices for splitting the training data
k_sample <- split(sample(nrow(ai2)), rep(1:10))
accuracy<- NULL
rf_total<- NULL
nb_total<- NULL
svm_total<- NULL
measures_table<- NULL

  
  # ****************  K-FOLD TRAINING ************************************************
  for(i in 1:10)
  {
    # parse through each column and consider the column as the test dataset
    test<- NULL
    test <- ai2[k_sample[[i]],]
    
    # rest of the columns are the training rows 
    training<- NULL
    for(j in 1:10)
    {
      if (j!=i) {training<- rbind(training,ai2[k_sample[[j]],])}
    }
    
    cat("\nFold number = ")
    cat(i)
    cat("\n")
    
    # Random forest
    RF_model<- randomForest(training$topic~.,data=training)
    RF_predictions<- predict(RF_model,test)
    test_vector<- k_sample[[i]]
    train_vector<- NULL
    for(j in 1:10)
    {
      if (j!=i) {train_vector<- rbind(train_vector,k_sample[[j]])}
    }
    train_vector<- as.vector(train_vector)
      
    result_RF<- table(observed = test[,"topic"],predicted = RF_predictions)
    if(i==1) { confusion_table_sum_RF <- result_RF} else { confusion_table_sum_RF <- confusion_table_sum_RF + result_RF }
    
    print("Confusion matrix from Random Forest is:")
    print(result_RF)
    
    # Class-wise measures
    table1<- measures(result_RF)
    print("Measurements for Random Forest are:")
    print(table1)
    
    
    # NaiveBayes
    
    NB_model<- naiveBayes(training$topic~.,data=training)
    NB_predictions<- predict(NB_model,test)
    result_NB<- table(observed = test[,"topic"],predicted = NB_predictions)
    if(i==1) { confusion_table_sum_NB <- result_NB} else {confusion_table_sum_NB <- confusion_table_sum_NB + result_NB}
    print("Confusion matrix from Naive Bayes is:")
    print(result_NB)
    
    # Class-wise measures
    table2<- measures(result_NB)
    
    
    print("Measurements for Naive Bayes are:")
    print(table2)
    
    
    #SVM 
    SVM_model<- svm(training$topic~.,data=training)
    SVM_predictions<- predict(SVM_model,test)
    result_SVM<- table(observed= test[,"topic"],predicted=SVM_predictions)
    if(i==1) { confusion_table_sum_SVM <- result_SVM } else { confusion_table_sum_SVM <- confusion_table_sum_SVM + result_SVM } 
    print("Confusion matrix from SVM is:")
    print(result_SVM)
    table3<- measures(result_SVM)
    print("Measurements for SVM are:")
    print(table3)
    
    
    # Save class-wise measurement tables for later use
    rf_total[[i]] <- table1
    nb_total[[i]] <- table2
    svm_total[[i]]<- table3
    
    # For average measures for the current fold
    # Calculates the precision and recall(micro and macro) and the accuracy 
    measures_table<- rbind(measures_table,c(average_measures(result_RF),average_measures(result_NB),average_measures(result_SVM)))
    
  }

colnames(measures_table)<- c("RF_uR","RF_uP","RF_MR","RF_MP","RF_Acc","NB_uR","NB_uP","NB_MR","NB_MP","NB_Acc","SVM_uR","SVM_uP","SVM_MR","SVM_MP","SVM_Acc")
rownames(measures_table)<- c(rep(paste("Fold_",1:10,sep="")))



avg_measure_comb_cf_matrix<- c(0,0,0,0,0)
avg_measure_comb_cf_matrix<- rbind(avg_measure_comb_cf_matrix,c(0,0,0,0,0))
colnames(avg_measure_comb_cf_matrix)<-c("Avg. Micro Recall","Avg. Micro Precision","Avg. Macro Recall","Avg. Macro Precision","Accuracy")

print("*****************   PAPER STYLE   **********************************")

# Printing sum of confusion matrices
print("Random Forest summed confusion matrix")
print(confusion_table_sum_RF)
print(measures(confusion_table_sum_RF))
avg_measure_comb_cf_matrix <- average_measures(confusion_table_sum_RF)

print("Naive Bayes summed confusion matrix")
print(confusion_table_sum_NB)
print(measures(confusion_table_sum_NB))
avg_measure_comb_cf_matrix <- rbind(avg_measure_comb_cf_matrix,average_measures(confusion_table_sum_NB))

print("SVM summed confusion matrix")
print(confusion_table_sum_SVM)
print(measures(confusion_table_sum_SVM))
avg_measure_comb_cf_matrix<- rbind(avg_measure_comb_cf_matrix,average_measures(confusion_table_sum_SVM))


colnames(avg_measure_comb_cf_matrix)<-c("Avg. Micro Recall","Avg. Micro Precision","Avg. Macro Recall","Avg. Macro Precision","Accuracy")
rownames(avg_measure_comb_cf_matrix)<- c("RF","NB","SVM")
print("Average Measures for summed confusion matrix")
print(avg_measure_comb_cf_matrix)







print("***********************   SLIDES STYLE - Class wise  **********************************")
# Sum all the tables for adding the class wise measures across the 10 folds.  Then average it.
# ***********************************************************************************************
for(i in 1:10)
{
  
  if(i==1)
  {
    rf_total2<- rf_total[[i]]
    nb_total2<- nb_total[[i]]
    svm_total2<-svm_total[[i]]
  }
  else{
    rf_total2<- rf_total2 +rf_total[[i]]
    nb_total2<- nb_total2 +nb_total[[i]]
    svm_total2<- svm_total2 +svm_total[[i]]
  }
}

rf_total2<- rf_total2/10
nb_total2<- nb_total2/10
svm_total2<- svm_total2/10

print("Average measures calculated for each fold")
print("Overall Class-Wise Measures")
print("Random Forest:")
print(rf_total2)
cat("\n")
cat("\n")
print("Naive Bayes:")
print(nb_total2)
cat("\n")
cat("\n")
print("Support Vector Machine:")
print(svm_total2)
cat("\n")
cat("\n")



print("*****************   SLIDES STYLE - Overall  **********************************")
print("Average measures calculated for each fold")
print("Overall Average Measures")
print(measures_table)

