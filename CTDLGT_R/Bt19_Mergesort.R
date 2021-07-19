merge <- function(a, b) {
  # Tạo mảng tạm thời
  temp <- numeric(length(a) + length(b))
  
  # lấy hai biến ban đầu trỏ đến
  # bắt đầu của các mảng con đã sắp xếp
  # và j trỏ đến bắt đầu bắt đầu
  # của mảng tạm thời
  astart <- 1
  bstart <- 1
  j <- 1
  for(j in 1 : length(temp)) {
    # nếu a[astart] < b[bstart]
    if((astart <= length(a) &&
        a[astart] < b[bstart]) ||
       bstart > length(b)) {
      # chèn a[start] tạm thời 
      # và tăng tới tiếp theo
      temp[j] <- a[astart]
      astart <- astart + 1
    }
    else {
      temp[j] <- b[bstart]
      bstart <- bstart + 1
    }
  }
  temp
}

# Hàm để sắp xếp
mergeSort <- function(arr) {
  
  # nếu độ dài của mảng lớn hơn 1 thì thực hiện sắp xếp
  if(length(arr) > 1) {
    
    # tìm điểm giữa mà mảng cần được chia
    mid <- ceiling(length(arr)/2)
    
    # phần đầu tiên của mảng sẽ từ 1 đến giữa
    a <- mergeSort(arr[1:mid])
    
    # phần thứ hai của mảng sẽ từ (mid + 1) đến chiều dài (arr)
    b <- mergeSort(arr[(mid+1):length(arr)])
    
    # hợp nhất các mảng đã sắp xếp ở trên
    merge(a, b)
  }
  # khác chỉ trả về arr với một phần tử duy nhất
  else {
    arr
  }
}

# lấy danh sách ban đầu đầu vào
arr <- sample(1:100, 10)
# Gọi hàm mergeSort
result <- mergeSort(arr)
# In ra kết quả
result
