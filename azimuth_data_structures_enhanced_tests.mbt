// Azimuth 基础数据结构测试用例
// 专注于核心数据结构和集合操作

// 测试1: 链表数据结构操作
test "链表数据结构基本操作" {
  // 定义链表节点类型
  type ListNode[T] = {
    value: T,
    next: Option[ListNode[T]]
  }
  
  // 创建链表辅助函数
  let create_node = fn(value: T) {
    { value, next: None }
  }
  
  let append_node = fn(head: ListNode[T], value: T) {
    let new_node = create_node(value)
    let mut current = head
    while true {
      match current.next {
        None => {
          current.next = Some(new_node)
          break
        }
        Some(node) => current = node
      }
    }
    head
  }
  
  let list_to_array = fn(head: ListNode[T]) {
    let mut result = []
    let mut current = Some(head)
    while true {
      match current {
        Some(node) => {
          result = result.push(node.value)
          current = node.next
        }
        None => break
      }
    }
    result
  }
  
  // 测试链表创建和操作
  let head = create_node(1)
  let list = append_node(append_node(append_node(head, 2), 3), 4)
  let array_result = list_to_array(list)
  
  assert_eq(array_result, [1, 2, 3, 4])
  assert_eq(array_result.length(), 4)
}

// 测试2: 二叉树数据结构操作
test "二叉树数据结构基本操作" {
  // 定义二叉树节点类型
  type TreeNode[T] = {
    value: T,
    left: Option[TreeNode[T]],
    right: Option[TreeNode[T]]
  }
  
  // 创建树节点辅助函数
  let create_tree_node = fn(value: T) {
    { value, left: None, right: None }
  }
  
  let insert_tree_node = fn(root: TreeNode[T], value: T) where T: Order {
    if value < root.value {
      match root.left {
        None => root.left = Some(create_tree_node(value))
        Some(node) => insert_tree_node(node, value)
      }
    } else {
      match root.right {
        None => root.right = Some(create_tree_node(value))
        Some(node) => insert_tree_node(node, value)
      }
    }
  }
  
  let inorder_traversal = fn(root: Option[TreeNode[T]]) where T: Order {
    let mut result = []
    let mut stack = []
    let mut current = root
    
    while stack.length() > 0 or current != None {
      match current {
        Some(node) => {
          stack = stack.push(node)
          current = node.left
        }
        None => {
          match stack.pop() {
            Some(node) => {
              result = result.push(node.value)
              current = node.right
            }
            None => current = None
          }
        }
      }
    }
    result
  }
  
  // 测试二叉树创建和遍历
  let root = create_tree_node(5)
  insert_tree_node(root, 3)
  insert_tree_node(root, 7)
  insert_tree_node(root, 2)
  insert_tree_node(root, 4)
  insert_tree_node(root, 6)
  insert_tree_node(root, 8)
  
  let traversal_result = inorder_traversal(Some(root))
  assert_eq(traversal_result, [2, 3, 4, 5, 6, 7, 8])
}

// 测试3: 哈希表操作模拟
test "哈希表操作模拟" {
  // 定义哈希表条目类型
  type HashEntry[K, V] = (K, V)
  
  // 简单哈希函数
  let simple_hash = fn(key: String, table_size: Int) {
    let mut hash = 0
    for i in 0..key.length() {
      hash = (hash + key.char_code(i)) % table_size
    }
    hash
  }
  
  // 创建哈希表
  let create_hash_table = fn(size: Int) {
    let mut table = []
    for i in 0..size {
      table = table.push([])
    }
    table
  }
  
  // 哈希表插入
  let hash_insert = fn(table: Array[Array[HashEntry[String, Int]]], key: String, value: Int) {
    let index = simple_hash(key, table.length())
    let bucket = table[index]
    let mut updated_bucket = []
    let mut found = false
    
    for (k, v) in bucket {
      if k == key {
        updated_bucket = updated_bucket.push((k, value))
        found = true
      } else {
        updated_bucket = updated_bucket.push((k, v))
      }
    }
    
    if not(found) {
      updated_bucket = updated_bucket.push((key, value))
    }
    
    table[index] = updated_bucket
  }
  
  // 哈希表查找
  let hash_get = fn(table: Array[Array[HashEntry[String, Int]]], key: String) {
    let index = simple_hash(key, table.length())
    let bucket = table[index]
    
    for (k, v) in bucket {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // 测试哈希表操作
  let hash_table = create_hash_table(10)
  
  hash_insert(hash_table, "apple", 5)
  hash_insert(hash_table, "banana", 3)
  hash_insert(hash_table, "orange", 7)
  hash_insert(hash_table, "grape", 2)
  
  assert_eq(hash_get(hash_table, "apple"), Some(5))
  assert_eq(hash_get(hash_table, "banana"), Some(3))
  assert_eq(hash_get(hash_table, "orange"), Some(7))
  assert_eq(hash_get(hash_table, "grape"), Some(2))
  assert_eq(hash_get(hash_table, "melon"), None)
  
  // 测试更新操作
  hash_insert(hash_table, "apple", 10)
  assert_eq(hash_get(hash_table, "apple"), Some(10))
}

// 测试4: 栈数据结构操作
test "栈数据结构操作" {
  // 定义栈类型
  type Stack[T] = {
    items: Array[T],
    top: Int
  }
  
  // 创建栈
  let create_stack = fn() {
    { items: [], top: -1 }
  }
  
  // 入栈操作
  let stack_push = fn(stack: Stack[T], item: T) {
    stack.items = stack.items.push(item)
    stack.top = stack.top + 1
  }
  
  // 出栈操作
  let stack_pop = fn(stack: Stack[T]) {
    if stack.top >= 0 {
      let item = stack.items[stack.top]
      stack.items = stack.items.slice(0, stack.top)
      stack.top = stack.top - 1
      Some(item)
    } else {
      None
    }
  }
  
  // 查看栈顶元素
  let stack_peek = fn(stack: Stack[T]) {
    if stack.top >= 0 {
      Some(stack.items[stack.top])
    } else {
      None
    }
  }
  
  // 检查栈是否为空
  let stack_is_empty = fn(stack: Stack[T]) {
    stack.top < 0
  }
  
  // 测试栈操作
  let stack = create_stack()
  
  assert_true(stack_is_empty(stack))
  
  stack_push(stack, 10)
  stack_push(stack, 20)
  stack_push(stack, 30)
  
  assert_false(stack_is_empty(stack))
  assert_eq(stack_peek(stack), Some(30))
  
  let popped = stack_pop(stack)
  assert_eq(popped, Some(30))
  assert_eq(stack_peek(stack), Some(20))
  
  let popped2 = stack_pop(stack)
  assert_eq(popped2, Some(20))
  
  let popped3 = stack_pop(stack)
  assert_eq(popped3, Some(10))
  
  let popped4 = stack_pop(stack)
  assert_eq(popped4, None)
  assert_true(stack_is_empty(stack))
}

// 测试5: 队列数据结构操作
test "队列数据结构操作" {
  // 定义队列类型
  type Queue[T] = {
    items: Array[T],
    front: Int,
    rear: Int
  }
  
  // 创建队列
  let create_queue = fn() {
    { items: [], front: 0, rear: 0 }
  }
  
  // 入队操作
  let queue_enqueue = fn(queue: Queue[T], item: T) {
    queue.items = queue.items.push(item)
    queue.rear = queue.rear + 1
  }
  
  // 出队操作
  let queue_dequeue = fn(queue: Queue[T]) {
    if queue.front < queue.rear {
      let item = queue.items[queue.front]
      queue.front = queue.front + 1
      Some(item)
    } else {
      None
    }
  }
  
  // 查看队首元素
  let queue_front = fn(queue: Queue[T]) {
    if queue.front < queue.rear {
      Some(queue.items[queue.front])
    } else {
      None
    }
  }
  
  // 检查队列是否为空
  let queue_is_empty = fn(queue: Queue[T]) {
    queue.front >= queue.rear
  }
  
  // 测试队列操作
  let queue = create_queue()
  
  assert_true(queue_is_empty(queue))
  
  queue_enqueue(queue, "first")
  queue_enqueue(queue, "second")
  queue_enqueue(queue, "third")
  
  assert_false(queue_is_empty(queue))
  assert_eq(queue_front(queue), Some("first"))
  
  let dequeued = queue_dequeue(queue)
  assert_eq(dequeued, Some("first"))
  assert_eq(queue_front(queue), Some("second"))
  
  let dequeued2 = queue_dequeue(queue)
  assert_eq(dequeued2, Some("second"))
  
  let dequeued3 = queue_dequeue(queue)
  assert_eq(dequeued3, Some("third"))
  
  let dequeued4 = queue_dequeue(queue)
  assert_eq(dequeued4, None)
  assert_true(queue_is_empty(queue))
}

// 测试6: 堆数据结构操作
test "堆数据结构操作" {
  // 定义堆类型
  type Heap[T] = {
    items: Array[T],
    size: Int,
    compare: (T, T) -> Bool
  }
  
  // 创建最大堆
  let create_max_heap = fn() {
    { 
      items: [], 
      size: 0,
      compare: fn(a: T, b: T) { a > b }
    }
  }
  
  // 创建最小堆
  let create_min_heap = fn() {
    { 
      items: [], 
      size: 0,
      compare: fn(a: T, b: T) { a < b }
    }
  }
  
  // 堆的上浮操作
  let heapify_up = fn(heap: Heap[T]) {
    let mut index = heap.size - 1
    while index > 0 {
      let parent_index = (index - 1) / 2
      if heap.compare(heap.items[index], heap.items[parent_index]) {
        let temp = heap.items[index]
        heap.items[index] = heap.items[parent_index]
        heap.items[parent_index] = temp
        index = parent_index
      } else {
        break
      }
    }
  }
  
  // 堆的下沉操作
  let heapify_down = fn(heap: Heap[T]) {
    let mut index = 0
    while true {
      let left_child = 2 * index + 1
      let right_child = 2 * index + 2
      let mut largest = index
      
      if left_child < heap.size and heap.compare(heap.items[left_child], heap.items[largest]) {
        largest = left_child
      }
      
      if right_child < heap.size and heap.compare(heap.items[right_child], heap.items[largest]) {
        largest = right_child
      }
      
      if largest != index {
        let temp = heap.items[index]
        heap.items[index] = heap.items[largest]
        heap.items[largest] = temp
        index = largest
      } else {
        break
      }
    }
  }
  
  // 堆插入操作
  let heap_insert = fn(heap: Heap[T], item: T) {
    heap.items = heap.items.push(item)
    heap.size = heap.size + 1
    heapify_up(heap)
  }
  
  // 堆删除根节点操作
  let heap_extract = fn(heap: Heap[T]) {
    if heap.size == 0 {
      None
    } else {
      let root = heap.items[0]
      heap.items[0] = heap.items[heap.size - 1]
      heap.items = heap.items.slice(0, heap.size - 1)
      heap.size = heap.size - 1
      
      if heap.size > 0 {
        heapify_down(heap)
      }
      
      Some(root)
    }
  }
  
  // 测试最大堆
  let max_heap = create_max_heap()
  
  heap_insert(max_heap, 5)
  heap_insert(max_heap, 10)
  heap_insert(max_heap, 3)
  heap_insert(max_heap, 8)
  heap_insert(max_heap, 1)
  
  let extracted1 = heap_extract(max_heap)
  assert_eq(extracted1, Some(10))
  
  let extracted2 = heap_extract(max_heap)
  assert_eq(extracted2, Some(8))
  
  let extracted3 = heap_extract(max_heap)
  assert_eq(extracted3, Some(5))
  
  let extracted4 = heap_extract(max_heap)
  assert_eq(extracted4, Some(3))
  
  let extracted5 = heap_extract(max_heap)
  assert_eq(extracted5, Some(1))
  
  let extracted6 = heap_extract(max_heap)
  assert_eq(extracted6, None)
  
  // 测试最小堆
  let min_heap = create_min_heap()
  
  heap_insert(min_heap, 5)
  heap_insert(min_heap, 10)
  heap_insert(min_heap, 3)
  heap_insert(min_heap, 8)
  heap_insert(min_heap, 1)
  
  let min_extracted1 = heap_extract(min_heap)
  assert_eq(min_extracted1, Some(1))
  
  let min_extracted2 = heap_extract(min_heap)
  assert_eq(min_extracted2, Some(3))
  
  let min_extracted3 = heap_extract(min_heap)
  assert_eq(min_extracted3, Some(5))
  
  let min_extracted4 = heap_extract(min_heap)
  assert_eq(min_extracted4, Some(8))
  
  let min_extracted5 = heap_extract(min_heap)
  assert_eq(min_extracted5, Some(10))
  
  let min_extracted6 = heap_extract(min_heap)
  assert_eq(min_extracted6, None)
}

// 测试7: 图数据结构基本操作
test "图数据结构基本操作" {
  // 定义图边类型
  type Edge = {
    from: Int,
    to: Int,
    weight: Float
  }
  
  // 定义图类型
  type Graph = {
    vertices: Array[Int],
    edges: Array[Edge],
    adjacency_list: Array[(Int, Array[Int])]
  }
  
  // 创建图
  let create_graph = fn() {
    { 
      vertices: [], 
      edges: [],
      adjacency_list: []
    }
  }
  
  // 添加顶点
  let add_vertex = fn(graph: Graph, vertex: Int) {
    if not(graph.vertices.contains(vertex)) {
      graph.vertices = graph.vertices.push(vertex)
      graph.adjacency_list = graph.adjacency_list.push((vertex, []))
    }
  }
  
  // 添加边
  let add_edge = fn(graph: Graph, from: Int, to: Int, weight: Float) {
    add_vertex(graph, from)
    add_vertex(graph, to)
    
    graph.edges = graph.edges.push({ from, to, weight })
    
    // 更新邻接表
    for i in 0..graph.adjacency_list.length() {
      let (vertex, neighbors) = graph.adjacency_list[i]
      if vertex == from {
        let updated_neighbors = neighbors.push(to)
        graph.adjacency_list[i] = (vertex, updated_neighbors)
      }
    }
  }
  
  // 获取顶点的邻居
  let get_neighbors = fn(graph: Graph, vertex: Int) {
    for (v, neighbors) in graph.adjacency_list {
      if v == vertex {
        return Some(neighbors)
      }
    }
    None
  }
  
  // 深度优先搜索
  let dfs = fn(graph: Graph, start: Int, target: Int) {
    let mut visited = []
    let mut stack = [start]
    let mut found = false
    
    while stack.length() > 0 and not(found) {
      let current = stack.pop()
      if not(visited.contains(current)) {
        visited = visited.push(current)
        
        if current == target {
          found = true
          break
        }
        
        match get_neighbors(graph, current) {
          Some(neighbors) => {
            for neighbor in neighbors {
              if not(visited.contains(neighbor)) {
                stack = stack.push(neighbor)
              }
            }
          }
          None => {}
        }
      }
    }
    
    found
  }
  
  // 广度优先搜索
  let bfs = fn(graph: Graph, start: Int, target: Int) {
    let mut visited = []
    let mut queue = [start]
    let mut found = false
    
    while queue.length() > 0 and not(found) {
      let current = queue[0]
      queue = queue.slice(1)
      
      if not(visited.contains(current)) {
        visited = visited.push(current)
        
        if current == target {
          found = true
          break
        }
        
        match get_neighbors(graph, current) {
          Some(neighbors) => {
            for neighbor in neighbors {
              if not(visited.contains(neighbor)) {
                queue = queue.push(neighbor)
              }
            }
          }
          None => {}
        }
      }
    }
    
    found
  }
  
  // 测试图操作
  let graph = create_graph()
  
  add_edge(graph, 1, 2, 1.0)
  add_edge(graph, 1, 3, 1.5)
  add_edge(graph, 2, 4, 2.0)
  add_edge(graph, 3, 4, 1.2)
  add_edge(graph, 4, 5, 0.8)
  
  // 验证顶点和边
  assert_eq(graph.vertices.length(), 5)
  assert_eq(graph.edges.length(), 5)
  
  // 验证邻接表
  let neighbors_1 = get_neighbors(graph, 1)
  assert_eq(neighbors_1, Some([2, 3]))
  
  let neighbors_4 = get_neighbors(graph, 4)
  assert_eq(neighbors_4, Some([5]))
  
  // 测试DFS
  assert_true(dfs(graph, 1, 5))
  assert_true(dfs(graph, 2, 3))
  assert_false(dfs(graph, 5, 1))
  
  // 测试BFS
  assert_true(bfs(graph, 1, 5))
  assert_true(bfs(graph, 2, 3))
  assert_false(bfs(graph, 5, 1))
}

// 测试8: 集合操作模拟
test "集合操作模拟" {
  // 定义集合类型
  type Set[T] = {
    items: Array[T]
  }
  
  // 创建集合
  let create_set = fn() {
    { items: [] }
  }
  
  // 添加元素到集合
  let set_add = fn(set: Set[T], item: T) {
    if not(set.items.contains(item)) {
      set.items = set.items.push(item)
    }
  }
  
  // 集合并集
  let set_union = fn(set_a: Set[T], set_b: Set[T]) {
    let result = create_set()
    for item in set_a.items {
      set_add(result, item)
    }
    for item in set_b.items {
      set_add(result, item)
    }
    result
  }
  
  // 集合交集
  let set_intersection = fn(set_a: Set[T], set_b: Set[T]) {
    let result = create_set()
    for item in set_a.items {
      if set_b.items.contains(item) {
        set_add(result, item)
      }
    }
    result
  }
  
  // 集合差集
  let set_difference = fn(set_a: Set[T], set_b: Set[T]) {
    let result = create_set()
    for item in set_a.items {
      if not(set_b.items.contains(item)) {
        set_add(result, item)
      }
    }
    result
  }
  
  // 检查子集
  let is_subset = fn(set_a: Set[T], set_b: Set[T]) {
    for item in set_a.items {
      if not(set_b.items.contains(item)) {
        return false
      }
    }
    true
  }
  
  // 测试集合操作
  let set_a = create_set()
  let set_b = create_set()
  
  set_add(set_a, 1)
  set_add(set_a, 2)
  set_add(set_a, 3)
  set_add(set_a, 4)
  
  set_add(set_b, 3)
  set_add(set_b, 4)
  set_add(set_b, 5)
  set_add(set_b, 6)
  
  // 测试并集
  let union_set = set_union(set_a, set_b)
  assert_eq(union_set.items.length(), 6)
  assert_true(union_set.items.contains(1))
  assert_true(union_set.items.contains(2))
  assert_true(union_set.items.contains(3))
  assert_true(union_set.items.contains(4))
  assert_true(union_set.items.contains(5))
  assert_true(union_set.items.contains(6))
  
  // 测试交集
  let intersection_set = set_intersection(set_a, set_b)
  assert_eq(intersection_set.items.length(), 2)
  assert_true(intersection_set.items.contains(3))
  assert_true(intersection_set.items.contains(4))
  
  // 测试差集
  let difference_set = set_difference(set_a, set_b)
  assert_eq(difference_set.items.length(), 2)
  assert_true(difference_set.items.contains(1))
  assert_true(difference_set.items.contains(2))
  
  // 测试子集
  let subset_a = create_set()
  set_add(subset_a, 1)
  set_add(subset_a, 3)
  
  assert_true(is_subset(subset_a, set_a))
  assert_false(is_subset(subset_a, set_b))
}