def insertion_sort(nums):
    # Bắt đầu trên phần tử thứ hai vì chúng tôi giả sử phần tử đầu tiên được sắp xếp
    for i in range(1, len(nums)):
        item_to_insert = nums[i]
        # Giữ một tham chiếu về chỉ mục của phần tử trước đó
        j = i - 1
        # Di chuyển tất cả các phần tử của danh sách đã sắp xếp
        # về phía trước nếu chúng lớn hơn phần tử để chèn
        while j >= 0 and nums[j] > item_to_insert:
            nums[j + 1] = nums[j]
            j -= 1
        # Chèn phần tử
        nums[j + 1] = item_to_insert


random_list_of_nums = [9, 1, 15, 28, 6]
insertion_sort(random_list_of_nums)
print(random_list_of_nums)