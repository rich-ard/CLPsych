# In the following line, only the rows in 'DTMasDF' which represent
# transcriptions of interviews with depressed or non-depressed parties would
# be identified (or any other characteristic to be evaluated). In this case,
# presume that texts reflected in DTMasDF rows no. 1, 2, 3, 4 and 5 are those
# you'd like to evaluate:
DTMasDFsums<-colSums(DTMasDF[c(1,2,3,4,5),])

T_NRC_Matrix<-as.data.frame(t(NRCMatrix))

WordCountBySentiment <- T_NRC_Matrix
for (i in row.names(T_NRC_Matrix)){
  WordCountBySentiment[i,] <- T_NRC_Matrix[i,]*DTMasDFsums
}

# Here, any NRC structure can be evaluated by replacing 'anger' with the sentiment or valence.
top10words <- WordCountBySentiment['anger', head(order(WordCountBySentiment['anger',], decreasing=T), 10)]
