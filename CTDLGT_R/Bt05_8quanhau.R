library(tidyverse)
library(ggplot2)

#-----------------------------------------------------------------------------
#' Đặt một quân hậu cho trước một vectơ vị trí của các quân hậu hiện tại
#'
#' Hàm này tự gọi đệ quy cho mọi vị trí hợp lệ của quân hậu tiếp theo
#'
#' Một vectơ số nguyên đại diện cho vị trí cột của các quân hậu cho đến nay.
#' Chỉ mục trong danh sách này là hàng và giá trị là cột.
#' Để tạo tất cả các giải pháp, hãy chuyển vào một vectơ trống (mặc định)
#'
#'  Ví dụ quân hậu = c(1, 4, 7) tương ứng với các quân hậu được đặt tại c(1, 1), c(2, 4) và c(3, 7)
#'                  
#'
#'    (Cột)   
#'      8  |   |   |   |   |   |   |   |   |
#'      | ---------------------------------
#'      7  |   |   |   |   |   |   |   |   |
#'      | ---------------------------------
#'      6  |   |   |   |   |   |   |   |   |
#'      | ---------------------------------
#'      5  |   |   |   |   |   |   |   |   |
#'      | ---------------------------------
#'      4  |   |   |   |   |   |   |   |   |
#'      | ---------------------------------
#'      3  |   |   |   |   |   |   | Q |   |    Hàng thứ 3, cột thứ 7
#'      | --------------------------------- 
#'      2  |   |   |   | Q |   |   |   |   |    Hàng thứ 2, cột thứ 4
#'      | ---------------------------------
#'      1  | Q |   |   |   |   |   |   |   |    Hàng thứ 1, cột thứ 1
#'      | 
#'      0____1___2___3___4___5___6___7___8 (Hàng)
#'
#' Kết quả trả về danh sách trong đó mỗi phần tử là một vectơ gồm 8 số nguyên chính là một giải pháp cho bài toán 8 quân hậu
#' 
#-----------------------------------------------------------------------------

place_queen <- function(queens=c()) { 
  
  # Nếu có 8 quân hậu được đặt, thì đây phải là một giải pháp.
  if (length(queens) == 8) {
    return(list(queens))
  }
  
  # Tìm ra nơi quân hậu có thể được đặt ở hàng tiếp theo.
  # Bỏ tất cả các cột đã được lấy
  # vì không thể đặt một quân hậu bên dưới một quân hậu hiện có
  possible_placements <- setdiff(1:8, queens)
  
  # Đối với mỗi quân hậu đã có trên bàn cờ
  # Hãy tìm các vị trí đường chéo mà nó có thể nhìn thấy trong hàng này.
  diag_offsets <- seq.int(length(queens), 1)
  diags <- c(queens + diag_offsets, queens - diag_offsets)
  diags <- diags[diags > 0 & diags < 9]
  
  # Thả các cột đường chéo này khỏi các vị trí có thể có
  possible_placements <- setdiff(possible_placements, diags)
  
  # Đối với mỗi vị trí có thể, hãy thử và đặt một quân hậu
  possible_placements %>% 
    map(~place_queen(c(queens, .x))) %>%
    keep(~length(.x) > 0) %>%
    flatten()
}


# Lập một giải pháp duy nhất
# Một vectơ gồm 8 số nguyên cho vị trí cột của 8 hoàng hậu
plot_single_8queens <- function(queens, title = NULL) {
  queens_df <- tibble(cols = queens, rows=1:8)
  board_df <- expand.grid(cols = 1:8, rows = 1:8) %>% 
    mutate(check = (cols + rows) %%2 == 1)
  
  p <- ggplot(queens_df, aes(rows, cols)) + 
    geom_tile(data=board_df, aes(fill=check), colour='black') +
    geom_text(label='\u2655', family="Arial Unicode MS", size = 8) + 
    theme_void() + 
    coord_equal() + 
    scale_fill_manual(values = c('TRUE'='white', 'FALSE'='grey70')) + 
    theme(
      legend.position = 'none'
    ) 
  
  if (is.null(title)) {
    p <- p + labs(title = paste("Queens:", deparse(as.numeric(queens)))) 
  } else {
    p <- p + labs(title = title)
  }
}



# Bắt đầu với không có quân hậu nào được đặt và tạo ra tất cả các giải pháp.
solutions <- place_queen()
v=1:8
f=function(q){L=length(q)
if(L==8){q}else{flatten(map(setdiff(v,c(q,q+L:1,q-L:1)),~f(c(q,.))))}}
s=data.frame(c=unlist(f(c())),r=v,x=rep(1:92,e=8),z=3)
b=mutate(crossing(c=v,r=v),z=(c+r)%%2)
g=geom_tile
ggplot(s,aes(r,c,fill=z))+g(d=b)+g()+facet_wrap(~x)
