class Node():

    def __init__(self, val, cdr=None):
        self.val = val
        self.cdr = cdr


class Lili():

    def __init__(self):
        self.head = None

    def __str__(self):
        acc = self.head.val.__str__()
        node = self.head.cdr
        while node:
            acc += " -> "
            acc += node.val.__str__()
            node = node.cdr
        return acc

    def append(self, val):
        self.head = Node(val, self.head)

    def car(self):
        self.head.val

    def cdr(self):
        self.head.cdr

    def is_loop(self):
        slow = self.head
        fast = self.head.cdr
        while (slow and fast and fast.cdr):
            slow = slow.cdr
            fast = fast.cdr.cdr
            if fast == slow:
                return True
        return False


l = Lili()
l.append(1)
l.append(2)
l.append(3)
print(l.is_loop())

l.head.cdr.cdr.cdr = l.head
print(l.is_loop())
