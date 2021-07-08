class Node:
    def __init__(self, next=None, prev=None, data=None):
        self.next = next  # tham chiếu đến node tiếp theo trong DLL
        self.prev = prev  # tham chiếu đến node trước đó trong DLL
        self.data = data

        # Thêm một node ở đầu danh sách

    def push(self, new_data):

        # 1 & 2: Phân bổ node và đưa vào dữ liệu
        new_node = Node(data=new_data)

        # 3. Thực hiện tiếp theo của node mới làm đầu và trước đó là NULL
        new_node.next = self.head
        new_node.prev = None

        # 4. thay đổi trước của node đầu thành node mới
        if self.head is not None:
            self.head.prev = new_node

            # 5. di chuyển đầu để trỏ đến node mới
        self.head = new_node

        # Đã cho một node là prev_node

    # hãy chèn một node mới vào sau node đã cho

    def insertAfter(self, prev_node, new_data):

        # 1. kiểm tra xem giá trị trước đó có phải là NULL không
        if prev_node is None:
            print("This node doesn't exist in DLL")
            return

        # 2. phân bổ node & 3. đưa vào dữ liệu
        new_node = Node(data=new_data)

        # 4. Thực hiện tiếp theo của node mới như tiếp theo của prev_node
        new_node.next = prev_node.next

        # 5. Đặt phần tiếp theo của prev_node làm new_node
        prev_node.next = new_node

        # 6. Đặt prev_node làm previous của new_node
        new_node.prev = prev_node

        # 7. thay đổi previous của new_node's next node */
        if new_node.next is not None:
            new_node.next.prev = new_node