#clustering

# The following section is common to perform the clustering for 3 algorithms. 
# This part needs to be executed on clean environment before any run of the 3 clustering.  

#------------------- COMMON PART START ---------------------------------------------------
data1<- read.csv("C:/Python27/krupa/pre_processed_new6.csv",stringsAsFactors=FALSE)
dim(data1)
corpus_full<- Corpus(VectorSource(data1$features))
dtm_full<- DocumentTermMatrix(corpus_full,control=list(wordLengths=c(3,Inf),stopwords = TRUE))

row_sum<- row_sums(dtm_full)
row_sum[which(row_sum<1)]

dtm_full2<- dtm_full[-which(row_sum<1),]
dim(dtm_full2)

LDA1<-LDA(dtm_full2,10, method = "Gibbs")
# Get the list of 50 significant words for each topic
topic_model_word_list <- as.vector(terms(LDA1, k=50))
dtm_topic_model <- DocumentTermMatrix(corpus_full,control=list(wordLengths=c(3,Inf),stopwords = TRUE,dictionary=topic_model_word_list))
dim(dtm_topic_model)


row_sum2<- row_sums(dtm_topic_model)
row_sum2[which(row_sum2<1)]
dtm_topic_model2<- dtm_topic_model[-which(row_sum2<1),]
dim(dtm_topic_model2)
topic_tags<- data1$topic
topic_tags<- topic_tags[-which(row_sum2<1)]


word_matrix_tm <- as.matrix(dtm_topic_model2)
rownames(word_matrix_tm)<- 1:nrow(word_matrix_tm)


#******************************************************************
# Additional Methods for validation of clusters. 
#******************************************************************
# To be executed before running any code.

# For KMeans
# Gets the representation of k clusters using result object of clustering and the oringinal dtm 
getClusterRep<- function(dtm_obj,cluster_result,total_k)
{
  cluster_set<- NULL
  cluster_topic_list<- NULL
  for(k in 1:total_k)
  {
    # get indices of documents which represent the cluster k
    cluster_set[[k]]<- as.vector(which(cluster_result$cluster==k))
    
    cluster_rep<- NULL
    # using the indices and the dtm object get the most frequent words in those documents
    cluster_rep[[k]] <- getClusterWords(dtm_obj,cluster_set[[k]])
    most_common_tag <- names(which.max(table(topic_tags[cluster_set[[k]]])))
    cat("\n Cluster k = ",k," Size = ", cluster_result$size[k]," common tag ",most_common_tag," \n")
    cluster_topic_list[[k]]<- most_common_tag
    cat(cluster_rep[[k]][1:10])
  }
  cluster_topic_list<- as.vector(cluster_topic_list)
  cat("\n",cluster_topic_list,";",length(table(cluster_topic_list)))
  
}

# For DBSCAN
getClusterRep_dbscan<- function(dtm_obj,cluster_result,total_k,topic_tags)
{
  cluster_set<- NULL
  cluster_topic_list<- NULL
  for(k in 1:total_k)
  {
    # get indices of documents which represent the cluster k
    cluster_set[[k]]<- as.vector(which(cluster_result$cluster==k))
    size <- length(which(cluster_result$cluster==k))
    cluster_rep<- NULL
    # using the indices and the dtm object get the most frequent words in those documents
    cluster_rep[[k]] <- getClusterWords(dtm_obj,cluster_set[[k]])
    most_common_tag <- names(which.max(table(topic_tags[cluster_set[[k]]])))
    
    #cat("\nTopic Tags in this cluster = ")
    #cat(names(table(topic_tags[cluster_set[[k]]])))
    #cat(table(topic_tags[cluster_set[[k]]]))
    
    cat("\n Cluster k = ;",k,";Size = ;", size,";common tag;",most_common_tag," \n")
    cluster_topic_list[[k]]<- most_common_tag
    cat(cluster_rep[[k]][1:10])
  }
  cluster_topic_list<- as.vector(cluster_topic_list)
  cat("\n",cluster_topic_list,";",length(table(cluster_topic_list)))
  
}


# For HAC 
getClusterRep_HAC<- function(dtm_obj,cluster_result,total_k,topic_tags)
{
  cluster_set<- NULL
  cluster_topic_list<- NULL
  for(k in 1:total_k)
  {
    # get indices of documents which represent the cluster k
    cluster_set[[k]]<- as.vector(cluster_result[[k]])
    size <- length(cluster_result[[k]])
    cluster_rep<- NULL
    # using the indices and the dtm object get the most frequent words in those documents
    cluster_rep[[k]] <- getClusterWords(dtm_obj,cluster_set[[k]])
    most_common_tag <- names(which.max(table(topic_tags[cluster_set[[k]]])))
    
    #cat("\nTopic Tags in this cluster = ")
    #cat(names(table(topic_tags[cluster_set[[k]]])))
    #cat(table(topic_tags[cluster_set[[k]]]))
    
    cat("\n Cluster k = ;",k,";Size = ;", size,";common tag;",most_common_tag," \n")
    cluster_topic_list[[k]]<- most_common_tag
    cat("Words: ")
    cat(cluster_rep[[k]][1:10])
  }
  cluster_topic_list<- as.vector(cluster_topic_list)
  cat("\n",cluster_topic_list,";",length(table(cluster_topic_list)))
  
}



# Common for KMeans, DBSCAN and HAC
# Finding what a cluster represents based on the documents in the dtm.
# Returns a set of frequent words from the given dtm and the given vector of document indices in the dtm.
getClusterWords<- function(dtm_obj,vector_docs)
{
  # Get the frequency of the term occuring maximum times in the dtm
  freq = max(dtm_obj$v)
  count1 <- length(findFreqTerms(dtm_obj[vector_docs,],freq))
  while(count1<=10)
  {
    freq = freq -1;
    count1 <- length(findFreqTerms(dtm_obj[vector_docs,],freq))
  }
  
  return(findFreqTerms(dtm_obj[vector_docs,],freq))
}


#--------------------------------- COMMON PART END ---------------------------------------------------



#********************************************************************
# K MEANS   
#********************************************************************
norm_eucl<- function(m) m/apply(m,1,function(x) sum(x^2)^0.5)
m_norm<- norm_eucl(word_matrix_tm) 

results<-NULL
for (k in 5:15)
{ 
  cat("\nPerforming clustering with K= ",k)
  result<- kmeans(m_norm,k,30)
  getClusterRep(dtm_topic_model2,result,k)
  cat("\nCluster Sizes at k = ",k,"\n")
  cat(result$size)
  cat("\nIn between-ness = ",result$betweenss)
  cat("\nTotal SS  = ",result$totss)
  
}

#********************************************************************
# DBSCAN
#********************************************************************
# Reduce the dimensions
term_tfidf_fordbscan <-tapply(dtm_topic_model2$v/row_sums(dtm_topic_model2)[dtm_topic_model2$i], dtm_topic_model2$j, mean) * log2(nDocs(dtm_topic_model2)/col_sums(dtm_topic_model2 > 0))
dim(dtm_topic_model2)
dtm_topic_model3 <- dtm_topic_model2[, term_tfidf_fordbscan >= 0.2]
dim(dtm_topic_model3)

#> dim(dtm_topic_model2)
#[1] 9167  378
#> dtm_topic_model3 <- dtm_topic_model2[, term_tfidf_fordbscan >= 0.2]
#> dim(dtm_topic_model3)
#[1] 9167   69

# To normalize, there should be no row with zero sums. Otherwise it results in NAs which are not accepted in the clustering functions
row_sum3<- row_sums(dtm_topic_model3)
row_sum3[which(row_sum3<1)]
dtm_topic_model4<- dtm_topic_model3[-which(row_sum3<1),]
#The topic tags need to be kept updated because many of the rows have been removed.
#To keep the document to topic aligned the topic tags vector is updated.
topic_tags2<- topic_tags[-which(row_sum3<1)]
length(topic_tags2)

word_matrix_tm_dbscan <- as.matrix(dtm_topic_model4)
dim(word_matrix_tm_dbscan)
rownames(word_matrix_tm_dbscan)<- 1:nrow(word_matrix_tm_dbscan)
norm_eucl<- function(m) m/apply(m,1,function(x) sum(x^2)^0.5)
m_norm2<- norm_eucl(word_matrix_tm_dbscan)


eps_list<- c(0.5,0.4,.6,.9,2,5,10)
MinPts_list <- c(25,5,10,20,30)
results2<-NULL
for(minPts in MinPts_list)
{
for (eps in eps_list)
{ 
  cat("\nPerforming DBSCAN clustering with EPS = ",eps, " and MinPts = ", minPts)
  result2<- dbscan(m_norm2, eps,MinPts=minPts)
  k = max(result2$cluster)
  getClusterRep_dbscan(dtm_topic_model3,result2,k,topic_tags2)
    
}
}


#********************************************************************
#HAC clustering
#********************************************************************
# For the heirarchical clustering we use the same reduced set of the 
# of the term document matrix with 9K documents and 69 features

term_tfidf_forHAC <-tapply(dtm_topic_model2$v/row_sums(dtm_topic_model2)[dtm_topic_model2$i], dtm_topic_model2$j, mean) * log2(nDocs(dtm_topic_model2)/col_sums(dtm_topic_model2 > 0))
dim(dtm_topic_model2)
dtm_topic_model3 <- dtm_topic_model2[, term_tfidf_forHAC >= 0.2]
dim(dtm_topic_model3)

#> dim(dtm_topic_model2)
#[1] 9167  378
#> dtm_topic_model3 <- dtm_topic_model2[, term_tfidf_fordbscan >= 0.2]
#> dim(dtm_topic_model3)
#[1] 9167   69


row_sum3<- row_sums(dtm_topic_model3)
row_sum3[which(row_sum3<1)]
dtm_topic_model4<- dtm_topic_model3[-which(row_sum3<1),]
topic_tags2<- topic_tags[-which(row_sum3<1)]
length(topic_tags2)
#length(topic_tags2)
#[1] 7659

word_matrix_tm_HAC <- as.matrix(dtm_topic_model4)
dim(word_matrix_tm_HAC)
#> dim(word_matrix_tm_HAC)
#[1] 7659   69

rownames(word_matrix_tm_HAC)<- 1:nrow(word_matrix_tm_HAC)
norm_eucl<- function(m) m/apply(m,1,function(x) sum(x^2)^0.5)
m_norm2<- norm_eucl(word_matrix_tm_HAC)


# We compute a distance matrix where the distance is calculated bet
distance_matrix <- dist(m_norm2,method="euclidean")
str(distance_matrix)
hclust1<- hclust(distance_matrix,method="single")
hclust2<- hclust(distance_matrix,method="complete")
hclust3<- hclust(distance_matrix,method="average")

plot(hclust1,label=FALSE,main="Single")

rect.hclust(hclust1,k=10,border="red")



for (k in 5:15)
  { 
    cat("\nPerforming HAC clustering for k = ",k)
    cat("\n************* SINGLE*************************")
    plot(hclust1,label=FALSE,main="Single")
    result1<- rect.hclust(hclust1,k=k,border="red")
    getClusterRep_HAC(dtm_topic_model4,result1,k,topic_tags2)
    
    cat("\n************* COMPLETE*************************")
    plot(hclust2,label=FALSE,main="complete")
    result1<- rect.hclust(hclust2,k=k,border="red")
    getClusterRep_HAC(dtm_topic_model4,result1,k,topic_tags2)
    
    cat("\n************* AVERAGE*************************")
    plot(hclust3,label=FALSE,main="AVERAGE")
    result1<- rect.hclust(hclust3,k=k,border="red")
    getClusterRep_HAC(dtm_topic_model4,result1,k,topic_tags2)
    
  }




#***********************************************************************************************





#***************************************************************************
# The following is a test to see if Normalization provides improvement to clustering

result<- kmeans(word_matrix_tm,10,30)
cat("\nSize = ", result$size)
cat("\nIn between-ness = ",result$betweenss)
cat("\nTotal SS  = ",result$totss)

Size =  656 2 263 219 693 2457 815 422 3710 128
> cat("\nIn between-ness = ",result$betweenss)

In between-ness =  424591.7
> cat("\nTotal SS  = ",result$totss)

Total SS  =  1314030
#-------------------------------------------------------------------------
# Normalization
norm_eucl<- function(m) m/apply(m,1,function(x) sum(x^2)^0.5)
m_norm<- norm_eucl(word_matrix_tm) 


result2<- kmeans(m_norm,10,30)
cat("\nSize = ", result2$size)
cat("\nIn between-ness = ",result2$betweenss)
cat("\nTotal SS  = ",result2$totss)
Size =  913 947 1901 656 532 973 261 1253 655 1274
> cat("\nIn between-ness = ",result2$betweenss)

In between-ness =  2127.029
> cat("\nTotal SS  = ",result2$totss)

Total SS  =  7698.95

#**************************************************************************
