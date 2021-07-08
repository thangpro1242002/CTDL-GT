def merge(left_list, right_list):
    sorted_list = []
    left_list_index = right_list_index = 0

    # Lấy độ dài danh sách ban đầu cho danh sách sắp xếp
    left_list_length, right_list_length = len(left_list), len(right_list)

    for _ in range(left_list_length + right_list_length):
        if left_list_index < left_list_length and right_list_index < right_list_length:
            # Kiểm tra giá trị nào từ đầu danh sách nhỏ hơn
            # Nếu phần tử ở đầu danh sách bên trái nhỏ hơn, hãy thêm nó vào
            # vào danh sách đã sắp xếp
            if left_list[left_list_index] <= right_list[right_list_index]:
                sorted_list.append(left_list[left_list_index])
                left_list_index += 1
            # Nếu phần tử ở đầu danh sách bên phải nhỏ hơn
            # Thêm nó vào vào danh sách đã sắp xếp
            else:
                sorted_list.append(right_list[right_list_index])
                right_list_index += 1

        # Nếu đã đến cuối danh sách bên trái
        # thì thêm các phần tử từ danh sách bên phải
        elif left_list_index == left_list_length:
            sorted_list.append(right_list[right_list_index])
            right_list_index += 1
        # Nếu đã đến cuối danh sách bên phải
        # thìthêm các phần tử từ danh sách bên trái
        elif right_list_index == right_list_length:
            sorted_list.append(left_list[left_list_index])
            left_list_index += 1
    # Trả về danh sách đã sắp xếp
    return sorted_list


def merge_sort(nums):
    # Nếu danh sách là một phần tử duy nhất, hãy trả về chúng nó
    if len(nums) <= 1:
        return nums

    # Chia 2 để lấy phần tử ở chính giữa, số lượng phần tử phải là số nguyên
    mid = len(nums) // 2

    # Sắp xếp từ đầu đến phần tử ở giữa
    left_list = merge_sort(nums[:mid])
    right_list = merge_sort(nums[mid:])

    # Hợp nhất các danh sách đã sắp xếp thành một danh sách mới
    return merge(left_list, right_list)


# Code bắt đầu chạy
random_list_of_nums = [120, 45, 68, 250, 176]
random_list_of_nums = merge_sort(random_list_of_nums)
print(random_list_of_nums)  