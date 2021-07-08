def sort(array):

    less = []
    equal = []
    greater = []

    if len(array) > 1:
        pivot = array[0]
        for x in array:
            if x < pivot:
                less.append(x)
            elif x == pivot:
                equal.append(x)
            elif x > pivot:
                greater.append(x)
        return sort(less)+equal+sort(greater)
    else:  # xử lý phần ở cuối đệ quy - khi chỉ có một phần tử trong mảng của mình, chỉ cần trả về mảng.
        return array

print(sort([12,4,5,6,7,3,1,15]))