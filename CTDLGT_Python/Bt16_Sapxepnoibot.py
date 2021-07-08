#Sắp xếp theo thứ tự giảm dần
x = int(input('Bạn muốn nhập bao nhiêu số: '))
A = list()
for i in range(0,x):
    A.append(int(input('Nhập số thứ %d: ' % (i+1))))
for i in range(x-1):
     for j in range(i+1, x):
         if A[i] > A[j]:
             tam = A[j]
             A[j] = A[i]
             A[i] = tam

# Cách khác:
# A.sort()

print("====> Kết quả:", A)