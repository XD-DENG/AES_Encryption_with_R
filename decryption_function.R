decrypt <- function(cipher_text, key, padding_sign = '`'){
  
  # cipher_text: character string. It's the result from the encryption.
  # key: character string, but only numbers inside, like "1234" instead of 1234 or "abc3". It's the "password". It should be of given length. Here we would like to have its length as 16. And ONLY numbers can be used here
  # padding_sign: the single-length character used for padding. Must be consistent with what was used during encryption. padding_sign should be something which will NOT appear in your plain text for sure.
  
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
  if(nchar(key)!=16){
    cat('the key must be 16-digit')
    break
  }
  
  
  # pre-process the cipher text for decryption
  cipher_text <- as.raw(as.numeric(strsplit(cipher_text,split=":")[[1]]))

  # pre-process the key for decryption  
  key <- strsplit(key,split='')[[1]]
  key <- as.integer(key)  
  key <- as.raw(key)
  
  aes <- AES(key, mode="ECB")
  plain_text <- aes$decrypt(cipher_text)
  
  
  # remove the padding
  plain_text <- gsub(padding_sign, "", plain_text)
  
  options(warn=0) # get the warning option back
  return(plain_text)
}