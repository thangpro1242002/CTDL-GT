def selection_sort(nums):
    # Giá trị này của i tương ứng với bao nhiêu giá trị đã được sắp xếp
    for i in range(len(nums)):
        # Giả định rằng mục đầu tiên của phân đoạn chưa được sắp xếp là mục nhỏ nhất
        lowest_value_index = i
        # Vòng lặp này lặp lại các mục chưa được sắp xếp
        for j in range(i + 1, len(nums)):
            if nums[j] < nums[lowest_value_index]:
                lowest_value_index = j
        # Hoán đổi các giá trị của phần tử chưa được sắp xếp thấp nhất với phần tử chưa được sắp xếp đầu tiên
        # Phần tử
        nums[i], nums[lowest_value_index] = nums[lowest_value_index], nums[i]

random_list_of_nums = [12, 8, 3, 20, 11]
selection_sort(random_list_of_nums)
print(random_list_of_nums)