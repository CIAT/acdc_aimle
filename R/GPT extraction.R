require(chatgpt)
require(data.table)

#require(gptchatteR)
#chatter.auth("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")
#chatter.create(model = "text-davinci-003",
#               temperature = 0.1,
#               max_tokens = 4000)


# Consider moving to openai pacakge openai::create_completion
# See https://github.com/isinaltinkaya/gptchatteR/blob/main/R/main.R for how this is used

#Set API key for ChatGPT
#Replace "sk-XXXXXXXXXXXXXXXXX" with the API key you received
Sys.setenv(OPENAI_API_KEY = "sk-XXXXXXXXXXXXXXXXX")

# Set gpt save directory
save_dir<-"analysis"
if(!dir.exists(save_dir)){
  dir.create(save_dir,recursive = T)
}

# Set results save directory
results_dir<-"results"
if(!dir.exists(results_dir)){
  dir.create(results_dir,recursive = T)
}

# Read in biblography
bib<-fread("data/ACDC Search results.csv")
bib[,code:=1:.N]

data<-bib[citation_screening_status =="included",unique(citation_abstract)]
N<-bib[match(data,citation_abstract),study_id]

# Set questions ####
questions<-c("Extract any mentions of a field or on-farm experiment? Extract as written. Answer as a comma separated list. If none, state None.",
             "In what countries was this research conducted? Answer as a comma separated list of country names, do not use a ; to separate country names. If none, state None.",
             "Extract any mentions of interviews, surveys or focus groups.  Extract as written. Answer as a comma separated list. If none, state None.",
             "Extract any mentions of laboratories, greenhouse or pots.  Extract as written. Answer as a comma separated list. If none, state None.",
             "Extract any mentions of an experimental control or check. Extract as written. Answer as a comma separated list. If none, state None.",
             "Extract any names and descriptions of  experimental conditions, comparisons or treatments. Exclude the control. Report as they are written . Answer as a comma separated list, do not use a semi-colon. If none, state None.",
             "Extract any experimental methods mentioned. Report as they are written in the text. Answer as a comma separated list. If none, state None.",
             "Extract the names of any crops, commodities or animals mentioned as they are written in the text. Answer as a comma separated list. If none, state None.",
             "Extract of any economic variables mentioned. Report as they are written in the text. Answer as a comma separated list. If none, state None.",
             "Extract mentions of crop or animal yield variables. Report as they are written in the text. Answer as a comma separated list. If none, state None.",
             "List the names of any farming or livestock management practices this experiment tested. Answer as a comma separated list. If none, state None.",
             "Extract the change(s) in management or behaviour that this experiment tests. Do not include the outcome being measured in the answer. Report as written in the text.Answer as a comma separated list. If none, state None.")
questions_num<-paste(paste0(1:length(questions),") ",questions),collapse = ";")

questions_short<-c("Field Exp","Country","Survey","Lab","Control","Treatments","Methods","Products","Econ_Out","Crop_Out","Practices","Change")
length(questions)==length(questions_short)

# Split questions in case of too many tokens
questions1<-questions[1:5]
questions_num1<-paste(paste0(1:length(questions1),") ",questions1),collapse = ";")
questions_short1<-questions_short[1:6]

questions2<-questions[6]
questions_num2<-paste(paste0(1:length(questions2),") ",questions2),collapse = ";")
questions_short2<-questions_short[6]

questions3<-questions[7:length(questions)]
questions_num3<-paste(paste0(1:length(questions3),") ",questions3),collapse = ";")
questions_short3<-questions_short[7:length(questions)]


col_names<-c("abstract",questions_short)

prompt_0<-"
  We are agricultural scientists extracting information from peer-reveiewed artciles for a meta-analysis on the impacts of agricultural management practices.
  For the scientific abstract provided above, using a temperature of 0.1, answer the following questions. 
  Do not expand on allowed answers. 
  Separate answers with a semi-colon and change any semi-colons within answers to commas."
prompt_n<-"Abstract = "


prompt_tidy<-function(prompt){
  results<-capture.output(ask_chatgpt(prompt))
  results<-results[length(results)]
  results<-gsub("[.]","",results)
  results<-gsub("\\\"","",results)
  results<-gsub("\\\\n","",results)
  results<-unlist(strsplit(results[length(results)],";"))
  if(grepl("1)",results[1])){
    results<-unlist(tstrsplit(results,") ",keep=2))
  }
  return(results)
}

prompt_fun<-function(prompts,len,dat,col_names){

  results<-prompt_tidy(prompts[[1]])

  if(length(results)!=len){
    results1<-prompt_tidy(prompt=prompts[[2]])
    results2<-prompt_tidy(prompts[[3]])
    results3<-prompt_tidy(prompts[[4]])
    
    results<-c(results1,results2,results3)
  }
  
  if(length(results)!=len){
    errorCondition("Error: results are incorrect length") 
  }
  
  results<-c(dat,results)
  names(results)<-col_names
  results<-data.table(data.frame(as.list(results)))

  return(results)
}

tryCatchRepeat <- function(prompts, n, len, dat, col_names) {
  success <- FALSE  # Flag to track successful result
  result <- NULL  # Variable to store the result
  
  for (i in 1:n) {
    tryCatch(
      {
        result <- prompt_fun(prompts, len, dat, col_names)
        success <- TRUE  # Set flag to indicate success
        break  # Exit the loop if successful result obtained
      },
      error = function(e) {
        message(paste("Error retrying...", i, "/", n, sep = ""))
      }
    )
  }
  
  if (success) {
    return(result)  # Return the successful result
  } else {
    stop("Error: All attempts failed.")  # or any other desired error message
  }
}


attempts<-6

results<-lapply(1:length(data),FUN=function(i){
  # Display progress
  cat('\r                                                                                                                                          ')
  cat('\r',paste0(i,"/",length(data),"-",N[i]))
  flush.console()
  
  save_file<-paste0(save_dir,"/",N[i],".csv")
  if(!file.exists(save_file)){
    
    dat<-data[i]
    
    prompt<-paste(prompt_n,dat,prompt_0,questions_num)
    prompt1<-paste(prompt_n,dat,prompt_0,questions_num1)
    prompt2<-paste(prompt_n,dat,prompt_0,questions_num2)
    prompt3<-paste(prompt_n,dat,prompt_0,questions_num3)
    
    
    prompts<-list(prompt,prompt1,prompt2,prompt3)

    results<-tryCatchRepeat(prompts=prompts,n=attempts,len=length(questions_short),dat=dat,col_names=col_names)

    fwrite(results,save_file)
    results
    
  }else{
    fread(save_file)
  }
  })

# Detect any tables with too many columns, remove and then rerun the above
(ncol_issue<-which(unlist(lapply(results,ncol))!=13))
save_file<-paste0(save_dir,"/",N[ncol_issue],".csv")
unlink(save_file)

# join results ####
results_joined<-data.table(do.call("rbind",results))

txt_clean<-function(data){
  data<-tolower(data)
  data<-trimws(data)
  data<-gsub(" ,|, ",",",data)
  data<-gsub("\\\"","",data)
  return(data)
}

Cols<-colnames(results_joined)[-1]

results_joined<-data.frame(results_joined)

for(COL in Cols){
  data<-txt_clean(unlist(results_joined[,COL]))
  results_joined[,COL]<-data
}

results_joined<-data.table(results_joined)

fwrite(results_joined,paste0(results_dir,"/Colandr - 17.05.2023 1348.csv"))

# Econ values ####
results_joined[,Econ_Out:=gsub("  "," ",gsub("-"," ",Econ_Out))
               ][,Econ_Out:=gsub("costs","cost",Econ_Out)
               ][,Econ_Out:=gsub("benefits","benefit",Econ_Out)
               ][,Econ_Out:=gsub("returns","return",Econ_Out)
               ][,Econ_Out:=gsub("rates","rate",Econ_Out)
               ][,Econ_Out:=gsub("gains","gain",Econ_Out)
               ][,Econ_Out:=gsub("margins","margin",Econ_Out)
               ][,Econ_Out:=gsub("ratios","ratio",Econ_Out)
               ][,Econ_Out:=gsub("efficiencies","efficiency",Econ_Out)
               ][,Econ_Out:=gsub("savings","saving",Econ_Out)
               ][,Econ_Out:=gsub("cost to benefit|cost/benefit|cost:benefit|cost: benefit|c:b|c/b","cost benefit",Econ_Out)
               ][,Econ_Out:=gsub("benefit to cost|benefit: cost|benefit/cost|benefit:cost|b:c|b/c","benefit cost",Econ_Out)
               ][,Econ_Out:=gsub("rates","rate",Econ_Out)
               ][,Econ_Out:=gsub(",and|, and",",",Econ_Out)
               ][,Econ_Out:=strsplit(Econ_Out,",")]
   
X<-lapply(results_joined$Econ_Out,FUN=function(data){
  trimws(unlist(unique(tstrsplit(data,"[(]",keep=1))))
})

results_joined[,Econ_Out:=X]

# Practice values ####
results_joined[,Practices:=gsub("  "," ",gsub("-"," ",Practices))
               ][,Practices:=gsub(",and|, and",",",Practices)
                 ][,Practices:=strsplit(Practices,",")]


X<-lapply(results_joined$Practices,FUN=function(data){
  trimws(unlist(unique(tstrsplit(data,"[(]",keep=1))))
})

results_joined[,Practices:=X]

# Crop outcome values ####
results_joined[,Crop_Out:=gsub("  "," ",gsub("-"," ",Crop_Out))
               ][,Crop_Out:=gsub(",and|, and",",",Crop_Out)
               ][,Crop_Out:=gsub("yields","yield",Crop_Out)
               ][,Crop_Out:=gsub("efficiencies","efficiency",Crop_Out)
               ][,Crop_Out:=gsub("levels","level",Crop_Out)
               ][,Crop_Out:=gsub("stocks","stock",Crop_Out)
                 ][,Crop_Out:=strsplit(Crop_Out,",")]


X<-lapply(results_joined$Crop_Out,FUN=function(data){
  trimws(unlist(unique(tstrsplit(data,"[(]",keep=1))))
})

results_joined[,Crop_Out:=X]

# Country values ####
results_joined[,Country:=gsub("  "," ",gsub("-"," ",Country))
               ][,Country:=gsub(",and | and ",",",Country)
               ][,Country:=gsub("northern |southern |eastern |western |central |south western |south east |south eastern |north east |
                                north west |northeast ","",Country)
               ][,Country:=gsub("african","africa",Country)
               ][,Country:=strsplit(Country,",")]


X<-lapply(results_joined$Country,FUN=function(data){
  trimws(unlist(unique(tstrsplit(data,"[(]",keep=1))))
})

results_joined[,Country:=X]

# Crop values  ####
results_joined[,Products:=gsub("  "," ",gsub("-"," ",Products))
][,Products:=gsub(",and | and ",",",Products)
][,Products:=gsub(" l)",")",Products)
][,Products:=gsub("crops","crop",Products)
][,Products:=gsub(" system| systems| farming systems","",Products)
][,Products:=strsplit(Products,",")]


save(results_joined,file=paste0(results_dir,"/Colandr - 17.05.2023 1348 - lists.RData"))

# Join unique values to make harmonization table ####
Country<-sort(unique(unlist(results_joined$Country)))
Products<-sort(unique(unlist(results_joined$Products)))
Practices<-sort(unique(unlist(results_joined$Practices)))
Econ_Out<-sort(unique(unlist(results_joined$Econ_Out)))
Crop_Out<-sort(unique(unlist(results_joined$Crop_Out)))

n <- max(length(Country), length(Products), length(Practices), length(Econ_Out), length(Crop_Out))


harmonization_table<-data.table(Country=Country[1:n],
                          Country_harmonized=NA,
                          Products=Products[1:n],
                          Products_harmonized=NA,
                          Practices=Practices[1:n],
                          Practices_harmonized=NA,
                          Econ_Out=Econ_Out[1:n],
                          Econ_Out_harmonized=NA,
                          Crop_Out=Crop_Out[1:n],
                          Crop_Out_harmonized=NA)

fwrite(harmonization_table,paste0(results_dir,"/harmonization_table.csv"))

