## ----setup, include=F, echo=F, message= F, warning= F--------------------------------------------------------------------------------------------------------------------
######## DATA PREPARATION #########

# Load Packages
library(readxl)
library(magrittr)
library(tm)

# set seed
set.seed(1234)

# Load Data
data <- read.csv("c-span-inagural-addresses.csv", stringsAsFactors = F)

# Create Corpus
pres_corpus <- Corpus(VectorSource(data$speach.content))

# Clean Corpus
pres_clean <- pres_corpus %>%
  tm_map(tolower) %>% 
  tm_map(removeNumbers) %>%
  tm_map(removeWords, c("will","united","states","must","can","the","and","however", "shall","upon","may","let", "one", "new","now", "every","government", "people","great", stopwords("english"))) %>% 
  tm_map(removePunctuation) %>% 
  tm_map(stripWhitespace)

# Create Document-Term-Matrix
pres_dtm <- DocumentTermMatrix(pres_clean)

# Inspection of Document-Term-Matrix
inspect(pres_dtm)

# Remove Very Rare Terms
pres_dtm <- removeSparseTerms(pres_dtm, 0.90)
inspect(pres_dtm)

# Convert Corpus into Matrix
pres_mat <- as.matrix(pres_dtm)

#### Term distribution in Data ####

# Load Packages
library(ggplot2)
library(dplyr)
library(reshape2)

# Plot total term distribution in data
pres_mat %>% 
  melt() %>% 
  group_by(Terms) %>% 
  tally(value) %>% 
  top_n(20, n) %>%
  ggplot(aes(x = reorder(Terms,n), y = n, fill = factor(n))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Total Frequency", title = "Figure 1: Total Term Distribution") +
  coord_flip() +
  theme(text=element_text(size=12,  family="serif"))

#### Determining The Amount Of Topics To Extract ####

# Load Packages
library(knitr)
library(kableExtra)

# To extract 75 percent of the total variance, we should retain 9 topics
out <- svd(pres_mat)
varcont <- out$d^2/(sum(out$d^2))
singular <- round(100*varcont,1)
singular_df <- as.data.frame(singular[1:9])

# Create Vector with Topic names
topics <- paste0("Topic ",1:9)

# Rename Rows and Columns
rownames(singular_df) <- topics
colnames(singular_df) <- "Explained Variance"
singular_df$CumSum <- cumsum(singular_df)

# Output table
kable(singular_df, caption = "Explained Variance of Topics", booktabs = T) %>%
kable_styling(latex_options = "striped")

#### Topic-Term Distribution ####

# Topic-Term Matrix: Create the matrix V^t
library(svs)
nnf.fit <- fast_nmf(pres_mat, 9, type = "KL", tol = 1e-16)
term_topics <- round(nnf.fit$pos2,0)
rownames(term_topics) <- topics

# Rearrange Data into long format
top <- melt(term_topics)

# Create plot data for all topics
p1 <- top %>%
  filter(top$Var1 == "Topic 1") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 1") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p2 <- top %>%
  filter(top$Var1 == "Topic 2") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 2") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p3 <- top %>%
  filter(top$Var1 == "Topic 3") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 3") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p4 <- top %>%
  filter(top$Var1 == "Topic 4") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 4") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p5 <- top %>%
  filter(top$Var1 == "Topic 5") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 5") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p6 <- top %>%
  filter(top$Var1 == "Topic 6") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 6") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p7 <- top %>%
  filter(top$Var1 == "Topic 7") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 7") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p8 <- top %>%
  filter(top$Var1 == "Topic 8") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 8") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p9 <- top %>%
  filter(top$Var1 == "Topic 9") %>% 
  top_n(10, value) %>%
  ggplot(aes(x = reorder(Var2,value), y = value, fill = factor(Var2))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Term", y = "Frequency", title = "Topic 9") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

# Arrange Plots
library(gridExtra)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, top ="Figure 2: Topic-Term Distribution By Topic")


## ---- echo=F, message= F, warning= F-------------------------------------------------------------------------------------------------------------------------------------
themes <- c("America As A Nation","Foreign Policy","Civil Conflict","Constitution & the Union","World Peace","Seperation of Powers","Law","Nation Building","Economy")
newtable <- cbind(topics,themes)
kable(newtable, caption = "Topics And Their Themes", booktab = T,) %>%
kable_styling(latex_options = "striped")

#### Document-Topic Distribution #####

# Create Vector with Document names
d <- data$president
y <- data$year
document <- paste0(d,y)

# Document-Topics Matrix: Show the matrix U
doc_topics <- nnf.fit$pos1

# Rename rows and columns
colnames(doc_topics) <- topics
rownames(doc_topics) <- document
docs <- melt(doc_topics)

# Convert Matrix into Dataset
doc_topics <- as.data.frame(doc_topics)

# Plot Topic Distribution
p10 <- docs %>%
  filter(docs$Var2 == "Topic 1") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 1") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p11 <- docs %>%
  filter(docs$Var2 == "Topic 2") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 2") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p12 <- docs %>%
  filter(docs$Var2 == "Topic 3") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 3") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p13 <- docs %>%
  filter(docs$Var2 == "Topic 4") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 4") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p14 <- docs %>%
  filter(docs$Var2 == "Topic 5") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 5") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p15 <- docs %>%
  filter(docs$Var2 == "Topic 6") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 6") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p16 <- docs %>%
  filter(docs$Var2 == "Topic 7") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 7") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p17 <- docs %>%
  filter(docs$Var2 == "Topic 8") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 8") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))

p18 <- docs %>%
  filter(docs$Var2 == "Topic 9") %>% 
  top_n(10, value) %>%
  arrange(-value) %>% 
  ggplot(aes(x = reorder(Var1,value), y = value, fill = factor(Var1))) +
  geom_col(show.legend = FALSE) +
  labs(x = "Document", y = "Association", title = "Topic 9") +
  coord_flip()+
  theme(text=element_text(size=12,  family="serif"))
# Arrange Plots
grid.arrange(p10,p11,p12,p13,p14,p15,p16,p17,p18, top = "Figure 3: Topic-Document Distribution by Topic")
