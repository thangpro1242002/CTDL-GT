class Node:

    # Hàm khởi tạo dối tượng  node
    def __init__(self, data):
        self.data = data  # Gán dữ liệu
        self.next = None  # Khởi tạo tiếp theo là null


# Lớp Danh sách được liên kết chứa một đối tượng Node
class LinkedList:

    # Hàm khởi tạo head
    def __init__(self):
        self.head = None

    # Hàm này in nội dung của danh sách liên kết
    # Bắt đầu từ head
    def printList(self):
        temp = self.head
        while (temp):
            print(temp.data)
            temp = temp.next


# CQuá trình thực thi mã bắt đầu ở đây
if __name__ == '__main__':
    # Bắt đầu với ds rỗng
    llist = LinkedList()

    llist.head = Node(4)
    second = Node(2)
    third = Node(5)

    llist.head.next = second;  # Liên kết node thứ nhất vs node thứ 2
    second.next = third;  # Liên kết node thứ 2 với thứ 3

    llist.printList()