install.packages('dequer')

library(dequer)

# STACK
s <- stack()
for (i in 1:3) push(s, i)
# Thêm lần lượt phần tử 1, 2, 3
str(s)
# Hiện tại 1-2-3
pop(s)
# Lấy phần tử cuối cùng trong danh sách là 3
str(s)
# Hiện tại có 1-2
pop(s)
# Lấy phần tử cuối cùng trong danh sách là 2
str(s)
#  Hiện tại có 1
pop(s)
# Lấy phần tử cuối cùng trong danh sách là 1
str(s)
# Hiện tại không có phần tử 
