encrypt <- function(msg, key, padding_sign = '`'){
  
  # msg: character string. It's the plain text to encrypt
  # key: character string, but only numbers inside, like "1234" instead of 1234 or "abc3". It's the "password". It should be of given length. Here we would like to have its length as 16. And ONLY numbers can be used here
  # padding_sign: the single-length character used for padding. padding_sign should be something which will NOT appear in your plain text for sure.
  
  #check if the necessary package is installed ------------------
  if('digest' %in% installed.packages()[,1]==FALSE){
    cat("please install the package 'digest'\n\n")
    confirm <- readline("Are you going to install 'digest' package now?(y/n)\n")
    if(confirm=='y'){
      install.packages('digest')
      cat('\n\n')
    }else{
      break
    }
  }
  
  library(digest)
  
  
  
  
  
  options(warn=-1) # trun off the warning
  
  
  # to check if the length of the key meet our requirement
  if(nchar(key) != 16){
    cat('the key must be 16-digit')
    break
  }
  
  
  # prepare the plain text for encryption
  # The text length must be a multiple of 16 bytes
  msg <- paste(msg, paste(rep(padding_sign, 16-(nchar(msg) %% 16)), collapse=""), sep="")
  
  
  # prepare the key
  key <- strsplit(key,split='')[[1]]
  key <- as.integer(key)  
  key <- as.raw(key)
  
  
  # prepare the AES cipher object
  aes <- AES(key, mode="ECB")
  
  
  cipher_text <- aes$encrypt(msg)
  
  temp <- as.character(as.numeric(cipher_text))
  
  value_to_return <- temp[1]
  for(i in 2:length(temp)){
    value_to_return <- paste(value_to_return,temp[i],sep=':')
  }
  
  options(warn=0) # get the warning option back
  return(value_to_return)
}