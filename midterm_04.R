# 1.將課堂中的自訂排序函數加入 decreasing = 的參數（預設為 FALSE）讓使用者可以指定遞增或遞減排序
exchange.sort.asc <- function(input_vector,decreasing=FALSE) {
  if(decreasing==TRUE){
    for (i in 1:(length(input_vector) - 1)) {
      for (j in (i + 1):length(input_vector)) {
        if (input_vector[i] > input_vector[j]) {
          temp <- input_vector[i]
          input_vector[i] <- input_vector[j]
          input_vector[j] <- temp
        }
      }
    }
    return(input_vector)
  }else{for (i in 1:(length(input_vector) - 1)) {
    for (j in (i + 1):length(input_vector)) {
      if (input_vector[i] < input_vector[j]) {
        temp <- input_vector[i]
        input_vector[i] <- input_vector[j]
        input_vector[j] <- temp
      }
    }
  }
    return(input_vector)
  }
}



# 產出隨機向量
unsorted_vector <- round(runif(10) * 100)
# 排序
exchange.sort.asc(unsorted_vector,decreasing= T)
exchange.sort.asc(unsorted_vector,decreasing= F)






#2.自訂計算樣本標準差的函數

# Section 2: self-defined functions
my.sd <- function(input_vector) {
  sum <- 0
  count <- 0
  for (i in input_vector) {
    sum<- sum + (i - mean(input_vector))^2
    count <- count + 1
  }
  return(sqrt(sum/(count-1)))
}

# Section 3: inputs and parameters
my_vector <- 1:30


# Section 4: function calls
my.sd(my_vector)

sd(my_vector) #check with sd()