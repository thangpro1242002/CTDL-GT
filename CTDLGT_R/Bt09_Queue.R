install.packages('dequer')

library(dequer)

# QUEUE
q <- queue()
for (i in 1:3) pushback(q, i)
# Thêm lần lượt phần tử 1, 2, 3
str(q)
# Hiện tại 1-2-3
pop(q)
# Lấy phần tử đầu tiên trong danh sách là 1
str(q)
# Hiện tại có 2-3
pop(q)
# Lấy phần tử đầu tiên trong danh sách là 2
str(q)
# Hiện tại có 3
pop(q)
# Lấy phần tử đầu tiên trong danh sách là 3
str(q)
# Hiện tại không có phần tử
