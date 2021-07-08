install.packages("purrr")
install.packages("dplyr")
install.packages("tidyverse")
library(purrr)
library(dplyr)
library(tidyverse)

# THUẬT TOÁN CỦA BÀI TOÁN

# Tính toán đệ quy các bước di chuyển cho một quân mã để hoàn thành chuyến tham quan
# Đã đề xuất động thái tiếp theo. 2 vectơ số phần tử của (hàng, cột) vị trí để đặt quân mã tiếp theo
# Di chuyển danh sách các vectơ. Mỗi vectơ có độ dài = 2 và cho biết vị trí hàng / cột trong chuyến tham quan của quân mã cho đến nay
# Truy cập ma trận lôgic 8x8 cho biết quân mã đã đến thăm một hình vuông hay chưa. Khi được người dùng gọi, ma trận này chỉ được chứa FALSE 


# Quân mã bù trừ nghĩa là các chuyển động có thể có của một quân mã từ vị trí hiện tại
knight_offsets <- matrix(c(
  1,    2,
  2,    1,
  -2,    1,
  -1,    2,
  2,   -1,
  1,   -2,
  -1,   -2,
  -2,   -1
), ncol = 2, byrow = TRUE)

move_knight <- function(this_move, moves, visited) {
  
  # Đánh dấu việc quân mã đã đi qua
  moves <- append(moves, list(this_move))
  visited[this_move[1] + (this_move[2] - 1)*8] <- TRUE
  
  # Kết thúc nếu tất cả ô quân mã đã đi qua
  if (all(visited)) {
    return(moves)
  }
  
  # Tìm tất cả các chuyển động có thể từ vị trí hiện tại
  next_move <- cbind(knight_offsets[,1] + this_move[1], knight_offsets[,2] + this_move[2])
  
  
  # Chỉ giữ các nước đi còn lại trên bàn cờ
  on_board  <- next_move[,1] %in% 1:8 & next_move[,2] %in% 1:8
  next_move <- next_move[on_board,,drop=FALSE]
  
  # Chỉ giữ các bước di chuyển nhắm các ô trên bàn cờ mà quân mã chưa đi qua
  not_yet_visited <- !visited[next_move]
  next_move <- next_move[not_yet_visited,, drop = FALSE]
  
  # Lặp lại mọi động thái tiếp theo có thể xảy ra
  for (i in seq_len(nrow(next_move))) {
    res <- move_knight(next_move[i,, drop = FALSE], moves, visited)
    if (!is.null(res)) {
      return(res)
    }
  }
  
  NULL
}

system.time({
  moves <- move_knight(c(4, 8), moves = list(), visited = matrix(FALSE, 8, 8))
})

# Chuyển đổi kết quả thành data.frame cho ggplot
moves_df <- as.data.frame(do.call(rbind, moves))
moves_df <- set_names(moves_df, c('x', 'y'))
moves_df$idx <- 1:nrow(moves_df)


# Lập kế hoạch cho các bước di chuyển của quân mã
ggplot(moves_df, aes(x, y)) + 
  geom_tile(aes(fill=as.logical((x+y)%%2)), colour = 'black') +
  geom_path(alpha = 0.7, linetype = 1, size = 0.25) +
  geom_text(aes(label = idx)) + 
  scale_fill_manual(values = c('grey70', 'white')) + 
  theme_void() + 
  theme(legend.position = 'none') + 
  coord_equal() + 
  labs(
    title = "A knight's tour in #RStats"
  )