# install.packages("devtools")
if(!require(BaySREn)){
  devtools::install_github("bakaburg1/BaySREn")
  library(BaySREn) # Load the framework
}
60 # amount of RAM available


library(dplyr)
library(googlesheets4)
library(rJava)
library(data.table)
library(revtools)
library(stringr)
library(ggplot2)

# Set save directory
save_dir<-"BaySREn/results"
if(!dir.exists(save_dir)){
  dir.create(save_dir,recursive = T)
}


# Read in query
gs4_auth()
1
sheet_url <- "https://docs.google.com/spreadsheets/d/1VUkcWho2ggz5LrQXGIWMXtcItbd-aayPm9Kr-1spRPs/edit#gid=211922423"
search_terms <- data.table(read_sheet(sheet_url,sheet=6))
query<-search_terms[NBS=="Nutrient Management",7]

year_filter <- "1950-2023"

# Read in colandr bibliography ####
to_screen<-fread("Colandr - 17.05.2023 1348.csv")
to_screen[citation_screening_status =="excluded",result:="n"
            ][citation_screening_status =="included",result:="y"
              ][citation_screening_status =="not_screened",result:=NA]

setnames(to_screen,c("study_id","citation_title","citation_authors","citation_abstract","citation_keywords"),c("ID","Title","Authors","Abstract","Keywords"))

# Remove entries with no abstract
to_screen<-to_screen[!is.na(Abstract)][Abstract!="NA"]

# Remove entries with duplicate abstracts
to_screen[,N:=.N,by=Abstract]
dups<-to_screen[N>1,unique(Abstract)]

for(i in 1:length(dups)){
  rev_manual<-to_screen[Abstract==dups[i],Rev_manual]

  if(sum(is.na(rev_manual))==1){
    to_screen<-to_screen[!(Abstract==dups[i] & is.na(Rev_manual))]
  }else{
    dID<-to_screen[Abstract==dups[i],ID]
    dID<-dID[2:length(N)]
    to_screen<-to_screen[!ID %in% dID]
  }
}

to_screen[,N:=.N,by=Abstract]
dups<-to_screen[N>1,unique(Abstract)]

# Check for duplicate titles
to_screen[,N:=.N,by=Title]
dups<-to_screen[N>1,unique(Title)]

for(i in 1:length(dups)){
  rev_manual<-to_screen[Title==dups[i],Rev_manual]
  
  if(sum(is.na(rev_manual))==1){
    to_screen<-to_screen[!(Title==dups[i] & is.na(Rev_manual))]
  }else{
    dID<-to_screen[Title==dups[i],ID]
    dID<-dID[2:length(N)]
    to_screen<-to_screen[!ID %in% dID]
  }
}

to_screen[,N:=.N,by=Title]
dups<-to_screen[N>1,unique(Title)]

to_screen[Title %in% dups]

# Load original search data ####
search_results<- "data/ACDC Search results.csv"  
# Read in WoS bib files
if(F){ # Set to TRUE if data are a series of bibtex files that need to be imported and wrangled into a table
search_results_bib<-do.call("rbind",lapply(list.dirs(wos_not_hi)[-1],FUN=function(x){
  NBS<-unlist(tstrsplit(x,"/",keep=stringr::str_count(x,"/")+1))
  nbs_files<-list.files(x,".bib",full.names = T)
  data<-lapply(1:length(nbs_files),FUN=function(i){
    print(paste0(NBS,"-",i,"/",length(nbs_files)))
    bib_data<-read_bibliography(nbs_files[i],return_df = TRUE)
    bib_data[,c("editor","booktitle","series","wii","note")]<-NULL
    if(!"organization" %in% colnames(bib_data)){
      bib_data$organization<-NA
    }
    
    if(!"isbn" %in% colnames(bib_data)){
      bib_data$isbn<-NA
    }
    
    bib_data
  })
  
  data<-do.call("rbind",data)
  
  data$NBS<-NBS
  data
}))
search_results<-data.table(search_results)
}else{
  search_results<-fread(wos_not_hi)
}
search_results$source<-"WoS"


# Index in keywords from data downloaded from WoS ####
to_screen[is.na(Keywords),.N]

# Match on Abstract
to_screen[,Keywords:=search_results[match(to_screen$Abstract,search_results$abstract),"keywords"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),Abstract],search_results[,abstract]),"keywords_plus"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),tolower(str_replace_all(Abstract, "[[:punct:]]", ""))],
                                                         search_results[,tolower(str_replace_all(abstract, "[[:punct:]]", ""))]),"keywords"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),tolower(str_replace_all(Abstract, "[[:punct:]]", ""))],
                                                         search_results[,tolower(str_replace_all(abstract, "[[:punct:]]", ""))]),"keywords_plus"]]
to_screen[is.na(Keywords),.N]

to_screen[,DOI:=search_results[match(to_screen$Abstract,search_results$abstract),"doi"]]
to_screen[is.na(DOI),DOI:=search_results[match(to_screen[is.na(DOI),tolower(str_replace_all(Abstract, "[[:punct:]]", ""))],
                                                         search_results[,tolower(str_replace_all(abstract, "[[:punct:]]", ""))]),"doi"]]

# Match on Title
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),Title],search_results$title),"keywords"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),Title],search_results$title),"keywords_plus"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),tolower(str_replace_all(Title, "[[:punct:]]", ""))],
                                                         search_results[,tolower(str_replace_all(title, "[[:punct:]]", ""))]),"keywords"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),tolower(str_replace_all(Title, "[[:punct:]]", ""))],
                                                         search_results[,tolower(str_replace_all(title, "[[:punct:]]", ""))]),"keywords_plus"]]
to_screen[is.na(Keywords),.N]

to_screen[is.na(DOI),DOI:=search_results[match(to_screen[is.na(DOI),Title],search_results$title),"doi"]]
to_screen[is.na(DOI),DOI:=search_results[match(to_screen[is.na(DOI),tolower(str_replace_all(Title, "[[:punct:]]", ""))],
                                                         search_results[,tolower(str_replace_all(title, "[[:punct:]]", ""))]),"doi"]]

# Match on Author
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),Authors],search_results$author),"keywords"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),Authors],search_results$author),"keywords_plus"]]
to_screen[is.na(Keywords),.N]
to_screen[is.na(Keywords),Keywords:=search_results[match(to_screen[is.na(Keywords),tolower(str_replace_all(Authors, "[[:punct:]]", ""))],
                                                         search_results[,tolower(str_replace_all(author, "[[:punct:]]", ""))]),"keywords"]]
to_screen[is.na(Keywords),.N]

to_screen[is.na(DOI),DOI:=search_results[match(to_screen[is.na(DOI),Authors],search_results$author),"doi"]]
to_screen[is.na(DOI),DOI:=search_results[match(to_screen[is.na(DOI),tolower(str_replace_all(Authors, "[[:punct:]]", ""))],
                                               search_results[,tolower(str_replace_all(author, "[[:punct:]]", ""))]),"doi"]]

to_screen[is.na(DOI),.N]
to_screen[is.na(DOI) & is.na(Keywords),.N]


# Prepare data for use with BayRESn functions ####
to_screen<-to_screen %>% 
  mutate(
  Rev_manual=result,
  Rev_prediction = NA, # Column for the evaluation of the predicted classes
  Rev_prediction_new = NA,
  Mesh="none",
  .before = DOI
)

to_screen<-to_screen[sample(1:nrow(to_screen),nrow(to_screen),replace=F)]

Annotation_data<-create_annotation_file(records=data.frame(to_screen),reorder_query = query)
Annotation_data$Rev_manual<-Annotation_data$result
Annotation_data$Rev_prediction_new<-NA
Annotation_data$Rev_prediction<-NA

plot_classification_trend(Annotation_data)

create_session(Records=Annotation_data,session_name = "Session1")

New_annotations <- enrich_annotation_file("Session1")

# Assess performance ####
New_annotations<-data.table(New_annotations)


# https://data.library.virginia.edu/roc-curves-and-auc-for-models-used-for-binary-classification/
sim_roc <- pROC::roc(response = New_annotations$Rev_manual,
               predictor = New_annotations$Pred_Med,
               levels = c('n', 'y'))

thresholds<-seq(0.5,0.95,0.05)

# Confusion matrix
threshold_results<-lapply(1:length(thresholds),FUN=function(i){
  threshold<-thresholds[i]

  New_annotations[,Manual:=Rev_manual][is.na(Manual),Manual:="*"][,Pred:="*"][Pred_Med<threshold,Pred:="n"][Pred_Med>=threshold,Pred:="y"]
  cm<-New_annotations[,table(Manual,Pred)]
  n<-rowSums(cm)
  cm<-round(100*cm/n,1)
  cm<-cbind(n,cm)
  colnames(cm)[2:ncol(cm)]<-paste0("p_",colnames(cm)[2:ncol(cm)])
  rownames(cm)<-paste0("o_",rownames(cm))
  cm
  })

names(threshold_results)<-thresholds

t_summary<-rbindlist(lapply(threshold_results,FUN=function(x){
  data.frame(false_rejection=x[3,2],
             false_inclusion=x[2,3],
             false_rejection_n=x[3,2]*x[3,1]/100,
             false_inclusion_n=x[2,3]*x[2,1]/100)
}))
t_summary$threshold<-names(threshold_results)

data<-melt(t_summary,id.vars = "threshold")
data$type<-"percent"
data[grepl("_n",variable),type:="n"][,variable:=gsub("_n","",variable)]
data[,threshold:=as.numeric(threshold)]

ggplot(data[type=="percent"], aes(x=threshold, y=value,group=variable,col=variable)) +
  geom_line()+
  labs(y="% error")+
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))

ggplot(data[type=="n"], aes(x=threshold, y=value,group=variable,col=variable)) +
  geom_line()+
  labs(y="num papers")+
  theme_bw()+
  scale_y_continuous(expand=c(0,0))+
  scale_x_continuous(expand=c(0,0))

data<-dcast(data,threshold+variable~type)

ggplot(data)  + 
  geom_bar(aes(x=threshold, y=n,fill=variable),stat="identity")+
  geom_line(aes(x=threshold, y=10*percent,lty=variable),col="black")+
  labs(title= "Threshold error rate",
       x="Threshold",y="no papers wrong")+
  scale_y_continuous(sec.axis=sec_axis(~.*0.1,name="Percent"),expand = c(0,0))+
  scale_x_continuous(expand=c(0,0))


