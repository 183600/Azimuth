// Azimuth 高级数据结构测试用例
// 专注于高级数据结构操作和算法测试

// 测试1: 高级队列操作
test "高级队列操作测试" {
  // 创建优先级队列
  type PriorityQueue = {
    elements: Array[(Int, String)], // (priority, value)
    size: Int
  }
  
  let create_priority_queue = fn() {
    { elements: [], size: 0 }
  }
  
  let enqueue = fn(queue: PriorityQueue, priority: Int, value: String) {
    let new_element = (priority, value)
    let mut inserted = false
    let mut new_elements = []
    
    for elem in queue.elements {
      if not(inserted) and elem.0 > priority {
        new_elements = new_elements.push(new_element)
        inserted = true
      }
      new_elements = new_elements.push(elem)
    }
    
    if not(inserted) {
      new_elements = new_elements.push(new_element)
    }
    
    { elements: new_elements, size: queue.size + 1 }
  }
  
  let dequeue = fn(queue: PriorityQueue) {
    if queue.size == 0 {
      (None, queue)
    } else {
      let first = queue.elements[0]
      let remaining = queue.elements.slice(1, queue.size - 1)
      (Some(first.1), { elements: remaining, size: queue.size - 1 })
    }
  }
  
  // 测试优先级队列
  let mut pq = create_priority_queue()
  pq = enqueue(pq, 3, "low priority")
  pq = enqueue(pq, 1, "high priority")
  pq = enqueue(pq, 2, "medium priority")
  
  assert_eq(pq.size, 3)
  
  let (value1, pq1) = dequeue(pq)
  assert_eq(value1, Some("high priority"))
  assert_eq(pq1.size, 2)
  
  let (value2, pq2) = dequeue(pq1)
  assert_eq(value2, Some("medium priority"))
  assert_eq(pq2.size, 1)
  
  let (value3, pq3) = dequeue(pq2)
  assert_eq(value3, Some("low priority"))
  assert_eq(pq3.size, 0)
}

// 测试2: 高级树结构操作
test "高级树结构操作测试" {
  // 定义二叉搜索树
  type TreeNode = {
    value: Int,
    left: Option[TreeNode],
    right: Option[TreeNode]
  }
  
  let create_node = fn(value: Int) {
    { value, left: None, right: None }
  }
  
  let insert = fn(node: Option[TreeNode], value: Int) {
    match node {
      None => Some(create_node(value))
      Some(n) => {
        if value < n.value {
          Some({ n | left: insert(n.left, value) })
        } else if value > n.value {
          Some({ n | right: insert(n.right, value) })
        } else {
          Some(n) // 已存在，不插入
        }
      }
    }
  }
  
  let inorder_traversal = fn(node: Option[TreeNode]) {
    let mut result = []
    
    let traverse = fn(n: Option[TreeNode]) {
      match n {
        None => ()
        Some(tree_node) => {
          traverse(tree_node.left)
          result = result.push(tree_node.value)
          traverse(tree_node.right)
        }
      }
    }
    
    traverse(node)
    result
  }
  
  let search = fn(node: Option[TreeNode], value: Int) {
    match node {
      None => false
      Some(n) => {
        if value == n.value {
          true
        } else if value < n.value {
          search(n.left, value)
        } else {
          search(n.right, value)
        }
      }
    }
  }
  
  // 测试二叉搜索树
  let mut tree = None
  let values = [50, 30, 70, 20, 40, 60, 80]
  
  for v in values {
    tree = insert(tree, v)
  }
  
  let inorder = inorder_traversal(tree)
  assert_eq(inorder, [20, 30, 40, 50, 60, 70, 80])
  
  assert_true(search(tree, 50))
  assert_true(search(tree, 20))
  assert_true(search(tree, 80))
  assert_false(search(tree, 25))
  assert_false(search(tree, 90))
}

// 测试3: 高级图结构操作
test "高级图结构操作测试" {
  // 定义图结构
  type Graph = {
    vertices: Array[String],
    edges: Array[(String, String)]
  }
  
  let create_graph = fn() {
    { vertices: [], edges: [] }
  }
  
  let add_vertex = fn(graph: Graph, vertex: String) {
    if graph.vertices.contains(vertex) {
      graph
    } else {
      { graph | vertices: graph.vertices.push(vertex) }
    }
  }
  
  let add_edge = fn(graph: Graph, from: String, to: String) {
    let updated_graph = add_vertex(add_vertex(graph, from), to)
    let new_edge = (from, to)
    { updated_graph | edges: updated_graph.edges.push(new_edge) }
  }
  
  let get_neighbors = fn(graph: Graph, vertex: String) {
    let mut neighbors = []
    for (from, to) in graph.edges {
      if from == vertex {
        neighbors = neighbors.push(to)
      }
    }
    neighbors
  }
  
  let depth_first_search = fn(graph: Graph, start: String) {
    let mut visited = []
    let mut stack = [start]
    let mut visited_set = []
    
    while stack.length() > 0 {
      let current = stack[stack.length() - 1]
      stack = stack.slice(0, stack.length() - 1)
      
      if not(visited_set.contains(current)) {
        visited = visited.push(current)
        visited_set = visited_set.push(current)
        
        let neighbors = get_neighbors(graph, current)
        for neighbor in neighbors {
          if not(visited_set.contains(neighbor)) {
            stack = stack.push(neighbor)
          }
        }
      }
    }
    
    visited
  }
  
  // 测试图操作
  let mut graph = create_graph()
  graph = add_edge(graph, "A", "B")
  graph = add_edge(graph, "A", "C")
  graph = add_edge(graph, "B", "D")
  graph = add_edge(graph, "C", "E")
  graph = add_edge(graph, "D", "F")
  graph = add_edge(graph, "E", "F")
  
  assert_eq(graph.vertices.length(), 6)
  assert_true(graph.vertices.contains("A"))
  assert_true(graph.vertices.contains("F"))
  
  let neighbors_a = get_neighbors(graph, "A")
  assert_eq(neighbors_a.length(), 2)
  assert_true(neighbors_a.contains("B"))
  assert_true(neighbors_a.contains("C"))
  
  let dfs_result = depth_first_search(graph, "A")
  assert_eq(dfs_result.length(), 6)
  assert_true(dfs_result.contains("A"))
  assert_true(dfs_result.contains("F"))
}

// 测试4: 高级哈希表操作
test "高级哈希表操作测试" {
  // 定义哈希表结构
  type HashTable = {
    buckets: Array[Array[(String, String)]],
    size: Int,
    capacity: Int
  }
  
  let hash_function = fn(key: String, capacity: Int) {
    let mut hash = 0
    for i in 0..key.length() {
      hash = (hash + key.to_char_array()[i].to_int()) % capacity
    }
    hash
  }
  
  let create_hash_table = fn(capacity: Int) {
    let mut buckets = []
    for i in 0..capacity {
      buckets = buckets.push([])
    }
    { buckets, size: 0, capacity }
  }
  
  let put = fn(table: HashTable, key: String, value: String) {
    let index = hash_function(key, table.capacity)
    let bucket = table.buckets[index]
    
    // 检查键是否已存在
    let mut found = false
    let mut updated_bucket = []
    
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
    
    let mut new_buckets = table.buckets
    new_buckets[index] = updated_bucket
    
    let new_size = if found { table.size } else { table.size + 1 }
    
    { buckets: new_buckets, size: new_size, capacity: table.capacity }
  }
  
  let get = fn(table: HashTable, key: String) {
    let index = hash_function(key, table.capacity)
    let bucket = table.buckets[index]
    
    let mut found = None
    for (k, v) in bucket {
      if k == key {
        found = Some(v)
      }
    }
    found
  }
  
  let remove = fn(table: HashTable, key: String) {
    let index = hash_function(key, table.capacity)
    let bucket = table.buckets[index]
    
    let mut found = false
    let mut updated_bucket = []
    
    for (k, v) in bucket {
      if k == key {
        found = true
      } else {
        updated_bucket = updated_bucket.push((k, v))
      }
    }
    
    if found {
      let mut new_buckets = table.buckets
      new_buckets[index] = updated_bucket
      
      ({ buckets: new_buckets, size: table.size - 1, capacity: table.capacity }, true)
    } else {
      (table, false)
    }
  }
  
  // 测试哈希表
  let mut table = create_hash_table(10)
  table = put(table, "name", "Alice")
  table = put(table, "age", "30")
  table = put(table, "city", "New York")
  
  assert_eq(table.size, 3)
  assert_eq(get(table, "name"), Some("Alice"))
  assert_eq(get(table, "age"), Some("30"))
  assert_eq(get(table, "city"), Some("New York"))
  assert_eq(get(table, "country"), None)
  
  let (table2, removed) = remove(table, "age")
  assert_true(removed)
  assert_eq(table2.size, 2)
  assert_eq(get(table2, "age"), None)
  
  let (table3, not_removed) = remove(table2, "nonexistent")
  assert_false(not_removed)
  assert_eq(table3.size, 2)
}

// 测试5: 高级堆操作
test "高级堆操作测试" {
  // 定义最大堆结构
  type MaxHeap = {
    data: Array[Int],
    size: Int
  }
  
  let create_max_heap = fn() {
    { data: [], size: 0 }
  }
  
  let parent_index = fn(i: Int) { (i - 1) / 2 }
  let left_child = fn(i: Int) { 2 * i + 1 }
  let right_child = fn(i: Int) { 2 * i + 2 }
  
  let swap = fn(data: Array[Int], i: Int, j: Int) {
    let temp = data[i]
    let mut new_data = data
    new_data[i] = data[j]
    new_data[j] = temp
    new_data
  }
  
  let heapify_up = fn(heap: MaxHeap, index: Int) {
    if index == 0 {
      heap
    } else {
      let parent = parent_index(index)
      if heap.data[index] > heap.data[parent] {
        let new_data = swap(heap.data, index, parent)
        heapify_up({ heap | data: new_data }, parent)
      } else {
        heap
      }
    }
  }
  
  let heapify_down = fn(heap: MaxHeap, index: Int) {
    let left = left_child(index)
    let right = right_child(index)
    let mut largest = index
    
    if left < heap.size and heap.data[left] > heap.data[largest] {
      largest = left
    }
    
    if right < heap.size and heap.data[right] > heap.data[largest] {
      largest = right
    }
    
    if largest != index {
      let new_data = swap(heap.data, index, largest)
      heapify_down({ heap | data: new_data }, largest)
    } else {
      heap
    }
  }
  
  let insert = fn(heap: MaxHeap, value: Int) {
    let new_data = heap.data.push(value)
    let new_heap = { heap | data: new_data, size: heap.size + 1 }
    heapify_up(new_heap, heap.size)
  }
  
  let extract_max = fn(heap: MaxHeap) {
    if heap.size == 0 {
      (None, heap)
    } else {
      let max_value = heap.data[0]
      let last = heap.data[heap.size - 1]
      let mut new_data = heap.data
      new_data[0] = last
      new_data = new_data.slice(0, heap.size - 1)
      
      let new_heap = { heap | data: new_data, size: heap.size - 1 }
      let heapified = heapify_down(new_heap, 0)
      
      (Some(max_value), heapified)
    }
  }
  
  // 测试最大堆
  let mut heap = create_max_heap()
  heap = insert(heap, 10)
  heap = insert(heap, 20)
  heap = insert(heap, 15)
  heap = insert(heap, 30)
  heap = insert(heap, 25)
  
  assert_eq(heap.size, 5)
  
  let (max1, heap1) = extract_max(heap)
  assert_eq(max1, Some(30))
  assert_eq(heap1.size, 4)
  
  let (max2, heap2) = extract_max(heap1)
  assert_eq(max2, Some(25))
  assert_eq(heap2.size, 3)
  
  let (max3, heap3) = extract_max(heap2)
  assert_eq(max3, Some(20))
  assert_eq(heap3.size, 2)
  
  let (max4, heap4) = extract_max(heap3)
  assert_eq(max4, Some(15))
  assert_eq(heap4.size, 1)
  
  let (max5, heap5) = extract_max(heap4)
  assert_eq(max5, Some(10))
  assert_eq(heap5.size, 0)
}

// 测试6: 高级链表操作
test "高级链表操作测试" {
  // 定义双向链表节点
  type DoublyListNode = {
    value: String,
    prev: Option[DoublyListNode],
    next: Option[DoublyListNode]
  }
  
  type DoublyLinkedList = {
    head: Option[DoublyListNode],
    tail: Option[DoublyListNode],
    size: Int
  }
  
  let create_node = fn(value: String) {
    { value, prev: None, next: None }
  }
  
  let create_list = fn() {
    { head: None, tail: None, size: 0 }
  }
  
  let push_front = fn(list: DoublyLinkedList, value: String) {
    let new_node = create_node(value)
    
    match list.head {
      None => {
        {
          head: Some(new_node),
          tail: Some(new_node),
          size: 1
        }
      }
      Some(current_head) => {
        let updated_head = { current_head | prev: Some(new_node) }
        let new_head = { new_node | next: Some(updated_head) }
        
        {
          head: Some(new_head),
          tail: list.tail,
          size: list.size + 1
        }
      }
    }
  }
  
  let push_back = fn(list: DoublyLinkedList, value: String) {
    let new_node = create_node(value)
    
    match list.tail {
      None => {
        {
          head: Some(new_node),
          tail: Some(new_node),
          size: 1
        }
      }
      Some(current_tail) => {
        let updated_tail = { current_tail | next: Some(new_node) }
        let new_tail = { new_node | prev: Some(updated_tail) }
        
        {
          head: list.head,
          tail: Some(new_tail),
          size: list.size + 1
        }
      }
    }
  }
  
  let to_array = fn(list: DoublyLinkedList) {
    let mut result = []
    let mut current = list.head
    
    while current.is_some() {
      match current {
        Some(node) => {
          result = result.push(node.value)
          current = node.next
        }
        None => ()
      }
    }
    
    result
  }
  
  let to_array_reverse = fn(list: DoublyLinkedList) {
    let mut result = []
    let mut current = list.tail
    
    while current.is_some() {
      match current {
        Some(node) => {
          result = result.push(node.value)
          current = node.prev
        }
        None => ()
      }
    }
    
    result
  }
  
  // 测试双向链表
  let mut list = create_list()
  list = push_front(list, "first")
  list = push_back(list, "second")
  list = push_front(list, "zero")
  list = push_back(list, "third")
  
  assert_eq(list.size, 4)
  
  let forward = to_array(list)
  assert_eq(forward, ["zero", "first", "second", "third"])
  
  let backward = to_array_reverse(list)
  assert_eq(backward, ["third", "second", "first", "zero"])
}

// 测试7: 高级栈操作
test "高级栈操作测试" {
  // 定义栈结构
  type Stack[T] = {
    elements: Array[T],
    top: Int
  }
  
  let create_stack = fn() {
    { elements: [], top: -1 }
  }
  
  let push = fn(stack: Stack[T], element: T) {
    { 
      elements: stack.elements.push(element), 
      top: stack.top + 1 
    }
  }
  
  let pop = fn(stack: Stack[T]) {
    if stack.top < 0 {
      (None, stack)
    } else {
      let top_element = stack.elements[stack.top]
      let new_elements = stack.elements.slice(0, stack.top)
      (Some(top_element), { elements: new_elements, top: stack.top - 1 })
    }
  }
  
  let peek = fn(stack: Stack[T]) {
    if stack.top < 0 {
      None
    } else {
      Some(stack.elements[stack.top])
    }
  }
  
  let is_empty = fn(stack: Stack[T]) {
    stack.top < 0
  }
  
  // 测试栈操作
  let mut stack = create_stack()
  assert_true(is_empty(stack))
  
  stack = push(stack, "first")
  stack = push(stack, "second")
  stack = push(stack, "third")
  
  assert_false(is_empty(stack))
  assert_eq(peek(stack), Some("third"))
  
  let (element1, stack1) = pop(stack)
  assert_eq(element1, Some("third"))
  assert_eq(peek(stack1), Some("second"))
  
  let (element2, stack2) = pop(stack1)
  assert_eq(element2, Some("second"))
  assert_eq(peek(stack2), Some("first"))
  
  let (element3, stack3) = pop(stack2)
  assert_eq(element3, Some("first"))
  assert_true(is_empty(stack3))
  
  let (element4, stack4) = pop(stack3)
  assert_eq(element4, None)
}

// 测试8: 高级集合操作
test "高级集合操作测试" {
  // 定义集合结构
  type Set = {
    elements: Array[String],
    size: Int
  }
  
  let create_set = fn() {
    { elements: [], size: 0 }
  }
  
  let add = fn(set: Set, element: String) {
    if set.elements.contains(element) {
      set
    } else {
      { elements: set.elements.push(element), size: set.size + 1 }
    }
  }
  
  let remove = fn(set: Set, element: String) {
    if not(set.elements.contains(element)) {
      set
    } else {
      let new_elements = set.elements.filter(fn(x) { x != element })
      { elements: new_elements, size: set.size - 1 }
    }
  }
  
  let contains = fn(set: Set, element: String) {
    set.elements.contains(element)
  }
  
  let union = fn(set1: Set, set2: Set) {
    let mut result = set1
    for element in set2.elements {
      result = add(result, element)
    }
    result
  }
  
  let intersection = fn(set1: Set, set2: Set) {
    let mut result = create_set()
    for element in set1.elements {
      if contains(set2, element) {
        result = add(result, element)
      }
    }
    result
  }
  
  let difference = fn(set1: Set, set2: Set) {
    let mut result = create_set()
    for element in set1.elements {
      if not(contains(set2, element)) {
        result = add(result, element)
      }
    }
    result
  }
  
  // 测试集合操作
  let mut set1 = create_set()
  set1 = add(set1, "apple")
  set1 = add(set1, "banana")
  set1 = add(set1, "cherry")
  
  let mut set2 = create_set()
  set2 = add(set2, "banana")
  set2 = add(set2, "cherry")
  set2 = add(set2, "date")
  
  assert_eq(set1.size, 3)
  assert_true(contains(set1, "apple"))
  assert_false(contains(set1, "date"))
  
  let union_set = union(set1, set2)
  assert_eq(union_set.size, 4)
  assert_true(contains(union_set, "apple"))
  assert_true(contains(union_set, "banana"))
  assert_true(contains(union_set, "cherry"))
  assert_true(contains(union_set, "date"))
  
  let intersection_set = intersection(set1, set2)
  assert_eq(intersection_set.size, 2)
  assert_true(contains(intersection_set, "banana"))
  assert_true(contains(intersection_set, "cherry"))
  assert_false(contains(intersection_set, "apple"))
  assert_false(contains(intersection_set, "date"))
  
  let difference_set = difference(set1, set2)
  assert_eq(difference_set.size, 1)
  assert_true(contains(difference_set, "apple"))
  assert_false(contains(difference_set, "banana"))
}