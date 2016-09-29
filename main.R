rm(list=ls())

source('encryption_function.R')

source('decryption_function.R')




# Usage -------------------------------------------------------------------

true_key <- "4321474846127593"

cipher <- encrypt("This is a test. This is a test for AES encryption of strings in R.", key = true_key)

decrypt(cipher_text = cipher, key = "4321474846127592")
decrypt(cipher_text = cipher, key = true_key, padding_sign = "`")




# Time Consuming test -----------------------------------------------------

library(microbenchmark)

# time consuming for encryption
microbenchmark(encrypt("This is a test.", key = true_key))

# time consuming for decryption
microbenchmark(decrypt(cipher_text = cipher, key = true_key))


# Time consuming for encryption for 1e4 records
microbenchmark(sapply(rep("S1234567", 1e3), 
                       function(x){
                         encrypt(x, key = true_key)
                       }),
               times=5)
  
  