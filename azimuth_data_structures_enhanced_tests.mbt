// Enhanced Data Structures Test Suite for Azimuth
// 增强数据结构测试套件 - 测试各种数据结构的高级操作

// Test 1: 高级数组操作
test "enhanced array operations" {
  let arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // 数组分块
  let chunks = arr.chunk(3)
  assert_eq(chunks, [[1, 2, 3], [4, 5, 6], [7, 8, 9], [10]])
  
  // 数组展平
  let nested = [[1, 2], [3, 4], [5, 6]]
  let flattened = nested.flat()
  assert_eq(flattened, [1, 2, 3, 4, 5, 6])
  
  // 数组去重
  let with_duplicates = [1, 2, 2, 3, 4, 4, 5]
  let unique = with_duplicates.unique()
  assert_eq(unique, [1, 2, 3, 4, 5])
  
  // 数组交集
  let arr1 = [1, 2, 3, 4, 5]
  let arr2 = [4, 5, 6, 7, 8]
  let intersection = arr1.intersection(arr2)
  assert_eq(intersection, [4, 5])
  
  // 数组差集
  let difference = arr1.difference(arr2)
  assert_eq(difference, [1, 2, 3])
}

// Test 2: 哈希表操作
test "hash map operations" {
  let map = {
    "apple": 5,
    "banana": 3,
    "orange": 7,
    "grape": 2
  }
  
  // 获取值
  assert_eq(map.get("apple"), Some(5))
  assert_eq(map.get("pear"), None)
  
  // 添加键值对
  let map2 = map.set("pear", 4)
  assert_eq(map2.get("pear"), Some(4))
  
  // 删除键值对
  let map3 = map2.delete("banana")
  assert_eq(map3.get("banana"), None)
  
  // 检查键存在
  assert_true(map3.contains("apple"))
  assert_false(map3.contains("banana"))
  
  // 获取所有键
  let keys = map3.keys()
  assert_eq(keys.length(), 4) // apple, orange, grape, pear
  
  // 获取所有值
  let values = map3.values()
  assert_eq(values.sum(), 5 + 7 + 2 + 4) // 18
  
  // 合并哈希表
  let map4 = {"kiwi": 6, "mango": 8}
  let merged = map3.merge(map4)
  assert_eq(merged.get("kiwi"), Some(6))
  assert_eq(merged.get("mango"), Some(8))
}

// Test 3: 集合操作
test "set operations" {
  let set1 = {1, 2, 3, 4, 5}
  let set2 = {4, 5, 6, 7, 8}
  
  // 检查元素存在
  assert_true(set1.contains(3))
  assert_false(set1.contains(6))
  
  // 添加元素
  let set3 = set1.add(6)
  assert_true(set3.contains(6))
  assert_eq(set3.size(), 6)
  
  // 删除元素
  let set4 = set3.remove(1)
  assert_false(set4.contains(1))
  assert_eq(set4.size(), 5)
  
  // 集合并集
  let union = set1.union(set2)
  assert_eq(union.size(), 8)
  assert_true(union.contains(1))
  assert_true(union.contains(8))
  
  // 集合交集
  let intersection = set1.intersection(set2)
  assert_eq(intersection.size(), 2)
  assert_true(intersection.contains(4))
  assert_true(intersection.contains(5))
  
  // 集合差集
  let difference = set1.difference(set2)
  assert_eq(difference.size(), 3)
  assert_true(difference.contains(1))
  assert_false(difference.contains(4))
}

// Test 4: 链表操作
test "linked list operations" {
  // 创建链表节点
  let node3 = {value: 3, next: None}
  let node2 = {value: 2, next: Some(node3)}
  let node1 = {value: 1, next: Some(node2)}
  
  // 转换为数组
  let list_to_array = fn(node) {
    let mut result = []
    let mut current = Some(node)
    
    while let Some(n) = current {
      result.push(n.value)
      current = n.next
    }
    
    result
  }
  
  let array = list_to_array(node1)
  assert_eq(array, [1, 2, 3])
  
  // 反转链表
  let reverse_list = fn(node) {
    let mut prev = None
    let mut current = Some(node)
    
    while let Some(n) = current {
      let next = n.next
      current = Some({value: n.value, next: prev})
      prev = current
      current = next
    }
    
    prev
  }
  
  let reversed_node = reverse_list(node1)
  let reversed_array = list_to_array(reversed_node.unwrap())
  assert_eq(reversed_array, [3, 2, 1])
}

// Test 5: 栈操作
test "stack operations" {
  let mut stack = []
  
  // 检查空栈
  assert_true(stack.is_empty())
  assert_eq(stack.size(), 0)
  
  // 压入元素
  stack.push(1)
  stack.push(2)
  stack.push(3)
  
  assert_false(stack.is_empty())
  assert_eq(stack.size(), 3)
  
  // 查看栈顶元素
  assert_eq(stack.peek(), Some(3))
  
  // 弹出元素
  let popped = stack.pop()
  assert_eq(popped, Some(3))
  assert_eq(stack.size(), 2)
  
  // 继续弹出所有元素
  assert_eq(stack.pop(), Some(2))
  assert_eq(stack.pop(), Some(1))
  assert_eq(stack.pop(), None)
  
  assert_true(stack.is_empty())
}

// Test 6: 队列操作
test "queue operations" {
  let mut queue = []
  
  // 检查空队列
  assert_true(queue.is_empty())
  assert_eq(queue.size(), 0)
  
  // 入队
  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)
  
  assert_false(queue.is_empty())
  assert_eq(queue.size(), 3)
  
  // 查看队首元素
  assert_eq(queue.front(), Some(1))
  
  // 出队
  let dequeued = queue.dequeue()
  assert_eq(dequeued, Some(1))
  assert_eq(queue.size(), 2)
  
  // 继续出队所有元素
  assert_eq(queue.dequeue(), Some(2))
  assert_eq(queue.dequeue(), Some(3))
  assert_eq(queue.dequeue(), None)
  
  assert_true(queue.is_empty())
}

// Test 7: 二叉树操作
test "binary tree operations" {
  // 创建二叉树节点
  let create_node = fn(value, left, right) {
    {value, left, right}
  }
  
  // 创建示例树
  //      5
  //     / \
  //    3   7
  //   / \   \
  //  2   4   8
  let tree = create_node(
    5,
    Some(create_node(
      3,
      Some(create_node(2, None, None)),
      Some(create_node(4, None, None))
    )),
    Some(create_node(
      7,
      None,
      Some(create_node(8, None, None))
    ))
  )
  
  // 前序遍历
  let preorder = fn(node) {
    match node {
      Some(n) => {
        let mut result = [n.value]
        result = result + preorder(n.left)
        result = result + preorder(n.right)
        result
      }
      None => []
    }
  }
  
  assert_eq(preorder(tree), [5, 3, 2, 4, 7, 8])
  
  // 中序遍历
  let inorder = fn(node) {
    match node {
      Some(n) => {
        let mut result = inorder(n.left)
        result.push(n.value)
        result = result + inorder(n.right)
        result
      }
      None => []
    }
  }
  
  assert_eq(inorder(tree), [2, 3, 4, 5, 7, 8])
  
  // 后序遍历
  let postorder = fn(node) {
    match node {
      Some(n) => {
        let mut result = postorder(n.left)
        result = result + postorder(n.right)
        result.push(n.value)
        result
      }
      None => []
    }
  }
  
  assert_eq(postorder(tree), [2, 4, 3, 8, 7, 5])
  
  // 查找值
  let contains = fn(node, value) {
    match node {
      Some(n) => {
        if n.value == value {
          true
        } else if value < n.value {
          contains(n.left, value)
        } else {
          contains(n.right, value)
        }
      }
      None => false
    }
  }
  
  assert_true(contains(tree, 4))
  assert_false(contains(tree, 6))
}

// Test 8: 堆操作
test "heap operations" {
  let mut heap = []
  
  // 检查空堆
  assert_true(heap.is_empty())
  
  // 插入元素
  heap.push(5)
  heap.push(3)
  heap.push(7)
  heap.push(2)
  heap.push(8)
  heap.push(4)
  
  // 查看堆顶元素（最小值）
  assert_eq(heap.peek(), Some(2))
  
  // 弹出最小值
  assert_eq(heap.pop(), Some(2))
  assert_eq(heap.pop(), Some(3))
  assert_eq(heap.pop(), Some(4))
  assert_eq(heap.pop(), Some(5))
  assert_eq(heap.pop(), Some(7))
  assert_eq(heap.pop(), Some(8))
  assert_eq(heap.pop(), None)
  
  assert_true(heap.is_empty())
}

// Test 9: 图操作
test "graph operations" {
  // 创建图
  let graph = {
    nodes: ["A", "B", "C", "D", "E"],
    edges: [
      {from: "A", to: "B", weight: 5},
      {from: "A", to: "C", weight: 3},
      {from: "B", to: "D", weight: 2},
      {from: "C", to: "D", weight: 1},
      {from: "C", to: "E", weight: 4},
      {from: "D", to: "E", weight: 6}
    ]
  }
  
  // 获取节点的邻居
  let get_neighbors = fn(graph, node) {
    graph.edges
      .filter(fn(edge) { edge.from == node })
      .map(fn(edge) { edge.to })
  }
  
  let neighbors_a = get_neighbors(graph, "A")
  assert_eq(neighbors_a, ["B", "C"])
  
  let neighbors_c = get_neighbors(graph, "C")
  assert_eq(neighbors_c, ["D", "E"])
  
  // 计算节点的出度
  let out_degree = fn(graph, node) {
    get_neighbors(graph, node).length()
  }
  
  assert_eq(out_degree(graph, "A"), 2)
  assert_eq(out_degree(graph, "D"), 1)
  
  // 检查边是否存在
  let has_edge = fn(graph, from, to) {
    graph.edges.any(fn(edge) { edge.from == from && edge.to == to })
  }
  
  assert_true(has_edge(graph, "A", "B"))
  assert_false(has_edge(graph, "B", "A"))
}

// Test 10: 高级数据结构转换
test "advanced data structure conversions" {
  // 数组转哈希表
  let pairs = [("apple", 5), ("banana", 3), ("orange", 7)]
  let map = pairs.to_map()
  assert_eq(map.get("apple"), Some(5))
  assert_eq(map.get("banana"), Some(3))
  
  // 哈希表转数组
  let map_entries = map.entries()
  assert_eq(map_entries.length(), 3)
  assert_true(map_entries.contains(("apple", 5)))
  
  // 数组转集合
  let numbers = [1, 2, 2, 3, 4, 4, 5]
  let set = numbers.to_set()
  assert_eq(set.size(), 5)
  assert_true(set.contains(1))
  assert_true(set.contains(5))
  
  // 集合转数组
  let set_array = set.to_array()
  assert_eq(set_array.length(), 5)
  
  // 链表转数组
  let create_linked_list = fn(arr) {
    let mut result = None
    for i in arr.reverse() {
      result = Some({value: i, next: result})
    }
    result
  }
  
  let linked_list = create_linked_list([1, 2, 3])
  let linked_list_to_array = fn(node) {
    let mut result = []
    let mut current = node
    while let Some(n) = current {
      result.push(n.value)
      current = n.next
    }
    result
  }
  
  let converted_array = linked_list_to_array(linked_list)
  assert_eq(converted_array, [1, 2, 3])
}