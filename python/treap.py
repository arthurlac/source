import sys
from random import randint


def gen_pri():
    return randint(0, sys.maxsize)


class Node(object):

    def __init__(self, parent, key, data):
        # Node data
        self.key = key
        self.data = data
        self.pri = gen_pri()
        self.parent = parent
        # Children
        self.left = None
        self.right = None

    def left_rotate(node):
        a = node
        b = a.right
        a.right = b.left
        b.left = a
        a = b
        b = a.left

    def right_rotate(node):
        a = node
        b = a.left
        a.left = b.right
        b.right = a
        a = b
        b = a.right


class Treap(object):

    def __init__(self, key, data):
        self.root = Node(None, key, data)

    def find(self, key):
        node = self.root
        while node is not None:
            if node.key == key:
                return node.data
            elif node.key > key:
                node = node.left
            else:
                node = node.right
        return None

    # Make self method
    def heapify(treap, node):
        if node.parent is None:
            # We have root, make sure it's treap root
            treap.root = node
        elif node.pri > node.parent.pri:
            if node.parent.left is node:
                print("Is left ch")
                Node.right_rotate(node)
                Treap.heapify(treap, node)
            else:
                print("Is right ch")
                Node.left_rotate(node)
                Treap.heapify(treap, node)

    def insert(treap, key, data, replace=False):
        node = treap.root
        Treap._ins(treap, node, key, data, replace)

    # Python NOT TAIL RECURSIVE :////
    def _ins(treap, node, key, data, replace):
        if node.key == key:
            if replace:
                node.data = data
        elif node.key > key:
            if node.left is None:
                node.left = Node(node, key, data)
                Treap.heapify(node.left)
            else:
                Treap._ins(treap, node.left, key, data, replace)
        else:
            if node.right is None:
                node.right = Node(node, key, data)
                Treap.heapify(node.right)
            else:
                Treap._ins(treap, node.right, key, data, replace)


t = Treap(1, "abc")
print(t.find(1))
Treap.insert(t, 9, "zyx")
print(Treap.find(t, 9))
Treap.insert(t, 5, "ddd")
print(Treap.find(t, 5))
