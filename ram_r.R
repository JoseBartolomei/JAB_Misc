# Function to calculate the number of GB that R needs
# to store a data.frame

# ((number of columns * number of rows * 8 bytes/numeric) / 2^20) /2^10

df.gb <- function(data.frame){
((ncol(data.frame) * nrow(data.frame) * 8) / 2^20) / 2^10 
}
