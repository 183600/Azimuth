// Azimuth Fundamental Data Structures Comprehensive Test Suite
// This file contains comprehensive test cases for fundamental data structures used in telemetry

// Test 1: Stack Operations
test "stack operations with telemetry data" {
  // Define stack type
  type Stack[T] = {
    items: Array[T],
    size: Int
  }
  
  // Stack operations
  let create_stack = fn() {
    { items: [], size: 0 }
  }
  
  let push = fn(stack: Stack[T], item: T) {
    { stack | items: stack.items.push(item), size: stack.size + 1 }
  }
  
  let pop = fn(stack: Stack[T]) {
    if stack.size > 0 {
      let top = stack.items[stack.size - 1]
      let remaining = stack.items.slice(0, stack.size - 1)
      (Some(top), { stack | items: remaining, size: stack.size - 1 })
    } else {
      (None, stack)
    }
  }
  
  let peek = fn(stack: Stack[T]) {
    if stack.size > 0 {
      Some(stack.items[stack.size - 1])
    } else {
      None
    }
  }
  
  // Test stack operations
  let mut stack = create_stack()
  assert_eq(stack.size, 0)
  assert_eq(stack.items.length(), 0)
  
  // Push operations
  stack = push(stack, "span-1")
  stack = push(stack, "span-2")
  stack = push(stack, "span-3")
  
  assert_eq(stack.size, 3)
  assert_eq(peek(stack), Some("span-3"))
  
  // Pop operations
  let (top, new_stack) = pop(stack)
  assert_eq(top, Some("span-3"))
  assert_eq(new_stack.size, 2)
  
  let (next_top, final_stack) = pop(new_stack)
  assert_eq(next_top, Some("span-2"))
  assert_eq(final_stack.size, 1)
  
  // Test empty stack
  let mut empty_stack = create_stack()
  let (empty_result, _) = pop(empty_stack)
  assert_eq(empty_result, None)
  assert_eq(peek(empty_stack), None)
}

// Test 2: Queue Operations
test "queue operations with telemetry events" {
  // Define queue type
  type Queue[T] = {
    items: Array[T],
    front: Int,
    rear: Int,
    size: Int,
    capacity: Int
  }
  
  // Queue operations
  let create_queue = fn(capacity: Int) {
    { items: Array::with_capacity(capacity), front: 0, rear: 0, size: 0, capacity }
  }
  
  let enqueue = fn(queue: Queue[T], item: T) {
    if queue.size < queue.capacity {
      let new_items = queue.items.set(queue.rear % queue.capacity, item)
      { queue | items: new_items, rear: queue.rear + 1, size: queue.size + 1 }
    } else {
      queue // Queue is full
    }
  }
  
  let dequeue = fn(queue: Queue[T]) {
    if queue.size > 0 {
      let item = queue.items[queue.front % queue.capacity]
      (Some(item), { queue | front: queue.front + 1, size: queue.size - 1 })
    } else {
      (None, queue)
    }
  }
  
  let is_empty = fn(queue: Queue[T]) {
    queue.size == 0
  }
  
  let is_full = fn(queue: Queue[T]) {
    queue.size == queue.capacity
  }
  
  // Test queue operations
  let mut queue = create_queue(5)
  assert_true(is_empty(queue))
  assert_false(is_full(queue))
  
  // Enqueue operations
  queue = enqueue(queue, "event-1")
  queue = enqueue(queue, "event-2")
  queue = enqueue(queue, "event-3")
  
  assert_false(is_empty(queue))
  assert_eq(queue.size, 3)
  
  // Dequeue operations
  let (first, queue1) = dequeue(queue)
  assert_eq(first, Some("event-1"))
  assert_eq(queue1.size, 2)
  
  let (second, queue2) = dequeue(queue1)
  assert_eq(second, Some("event-2"))
  assert_eq(queue2.size, 1)
  
  let (third, queue3) = dequeue(queue2)
  assert_eq(third, Some("event-3"))
  assert_true(is_empty(queue3))
  
  // Test empty queue dequeue
  let (empty_result, _) = dequeue(queue3)
  assert_eq(empty_result, None)
}

// Test 3: Binary Tree Operations
test "binary tree operations for telemetry hierarchy" {
  // Define binary tree node type
  type TreeNode[T] = {
    value: T,
    left: Option[TreeNode[T]],
    right: Option[TreeNode[T]]
  }
  
  // Tree operations
  let create_node = fn(value: T) {
    {
      value,
      left: None,
      right: None
    }
  }
  
  let insert = fn(node: Option[TreeNode[T]], value: T, compare: (T, T) -> Bool) {
    match node {
      None => Some(create_node(value))
      Some(current) => {
        if compare(value, current.value) {
          Some({ current | left: insert(current.left, value, compare) })
        } else {
          Some({ current | right: insert(current.right, value, compare) })
        }
      }
    }
  }
  
  let search = fn(node: Option[TreeNode[T]], value: T, compare: (T, T) -> Bool) {
    match node {
      None => false
      Some(current) => {
        if value == current.value {
          true
        } else if compare(value, current.value) {
          search(current.left, value, compare)
        } else {
          search(current.right, value, compare)
        }
      }
    }
  }
  
  let inorder_traversal = fn(node: Option[TreeNode[T]]) {
    let mut result = []
    let traverse = fn(n: Option[TreeNode[T]]) {
      match n {
        None => ()
        Some(current) => {
          traverse(current.left)
          result = result.push(current.value)
          traverse(current.right)
        }
      }
    }
    traverse(node)
    result
  }
  
  // Test tree operations
  let mut tree = None
  let compare_ints = fn(a: Int, b: Int) { a < b }
  
  // Insert values
  tree = insert(tree, 50, compare_ints)
  tree = insert(tree, 30, compare_ints)
  tree = insert(tree, 70, compare_ints)
  tree = insert(tree, 20, compare_ints)
  tree = insert(tree, 40, compare_ints)
  tree = insert(tree, 60, compare_ints)
  tree = insert(tree, 80, compare_ints)
  
  // Test search
  assert_true(search(tree, 50, compare_ints))
  assert_true(search(tree, 20, compare_ints))
  assert_true(search(tree, 80, compare_ints))
  assert_false(search(tree, 25, compare_ints))
  assert_false(search(tree, 90, compare_ints))
  
  // Test traversal
  let inorder = inorder_traversal(tree)
  assert_eq(inorder, [20, 30, 40, 50, 60, 70, 80])
}

// Test 4: Hash Map Operations
test "hash map operations for telemetry attributes" {
  // Define hash map entry type
  type HashMapEntry[K, V] = (K, V)
  
  // Hash map operations
  let hash_string = fn(key: String) {
    let mut hash = 0
    for i in 0..key.length() {
      hash = hash + key[i].to_int() * 31
    }
    hash
  }
  
  let create_hash_map = fn(size: Int) {
    Array::with_capacity(size).map(fn(_) { [] })
  }
  
  let put = fn(map: Array[Array[HashMapEntry[String, String]]], key: String, value: String) {
    let index = hash_string(key) % map.length()
    let bucket = map[index]
    
    // Check if key already exists
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
    
    map.set(index, updated_bucket)
  }
  
  let get = fn(map: Array[Array[HashMapEntry[String, String]]], key: String) {
    let index = hash_string(key) % map.length()
    let bucket = map[index]
    
    let mut result = None
    for (k, v) in bucket {
      if k == key {
        result = Some(v)
      }
    }
    result
  }
  
  let remove = fn(map: Array[Array[HashMapEntry[String, String]]], key: String) {
    let index = hash_string(key) % map.length()
    let bucket = map[index]
    
    let mut filtered_bucket = []
    for (k, v) in bucket {
      if k != key {
        filtered_bucket = filtered_bucket.push((k, v))
      }
    }
    
    map.set(index, filtered_bucket)
  }
  
  // Test hash map operations
  let mut map = create_hash_map(10)
  
  // Put operations
  map = put(map, "service.name", "payment-service")
  map = put(map, "service.version", "1.2.3")
  map = put(map, "environment", "production")
  map = put(map, "trace.id", "trace-12345")
  
  // Get operations
  assert_eq(get(map, "service.name"), Some("payment-service"))
  assert_eq(get(map, "service.version"), Some("1.2.3"))
  assert_eq(get(map, "environment"), Some("production"))
  assert_eq(get(map, "trace.id"), Some("trace-12345"))
  assert_eq(get(map, "nonexistent.key"), None)
  
  // Update existing key
  map = put(map, "environment", "staging")
  assert_eq(get(map, "environment"), Some("staging"))
  
  // Remove operations
  map = remove(map, "trace.id")
  assert_eq(get(map, "trace.id"), None)
  assert_eq(get(map, "service.name"), Some("payment-service")) // Other keys remain
}

// Test 5: Linked List Operations
test "linked list operations for telemetry chain" {
  // Define linked list node type
  type ListNode[T] = {
    value: T,
    next: Option[ListNode[T]]
  }
  
  // Linked list operations
  let create_node = fn(value: T) {
    { value, next: None }
  }
  
  let prepend = fn(list: Option[ListNode[T]], value: T) {
    Some({ value, next: list })
  }
  
  let append = fn(list: Option[ListNode[T]], value: T) {
    match list {
      None => Some(create_node(value))
      Some(head) => {
        let mut current = head
        let mut new_list = Some(head)
        
        // Find the last node
        while true {
          match current.next {
            None => {
              // Append new node
              new_list = Some({ head | next: Some(create_node(value)) })
              break
            }
            Some(next_node) => {
              current = next_node
            }
          }
        }
        
        new_list
      }
    }
  }
  
  let to_array = fn(list: Option[ListNode[T]]) {
    let mut result = []
    let mut current = list
    
    while true {
      match current {
        None => break
        Some(node) => {
          result = result.push(node.value)
          current = node.next
        }
      }
    }
    
    result
  }
  
  let find = fn(list: Option[ListNode[T]], value: T) {
    let mut current = list
    let mut found = false
    
    while true {
      match current {
        None => break
        Some(node) => {
          if node.value == value {
            found = true
            break
          }
          current = node.next
        }
      }
    }
    
    found
  }
  
  // Test linked list operations
  let mut list = None
  
  // Prepend operations
  list = prepend(list, "span-3")
  list = prepend(list, "span-2")
  list = prepend(list, "span-1")
  
  let array_result = to_array(list)
  assert_eq(array_result, ["span-1", "span-2", "span-3"])
  
  // Append operations
  list = append(list, "span-4")
  list = append(list, "span-5")
  
  let updated_array = to_array(list)
  assert_eq(updated_array, ["span-1", "span-2", "span-3", "span-4", "span-5"])
  
  // Find operations
  assert_true(find(list, "span-1"))
  assert_true(find(list, "span-5"))
  assert_false(find(list, "span-10"))
  
  // Test empty list
  let empty_list: Option[ListNode[String]] = None
  assert_eq(to_array(empty_list), [])
  assert_false(find(empty_list, "anything"))
}

// Test 6: Priority Queue Operations
test "priority queue operations for telemetry metrics" {
  // Define priority queue item type
  type PriorityItem[T] = {
    value: T,
    priority: Int
  }
  
  // Priority queue operations
  let create_priority_item = fn(value: T, priority: Int) {
    { value, priority }
  }
  
  let enqueue_priority = fn(queue: Array[PriorityItem[T]], item: PriorityItem[T]) {
    let mut result = queue
    let mut inserted = false
    
    // Find the correct position based on priority (higher priority first)
    for i in 0..queue.length() {
      if item.priority > queue[i].priority {
        let before = queue.slice(0, i)
        let after = queue.slice(i, queue.length())
        result = before + [item] + after
        inserted = true
        break
      }
    }
    
    if not(inserted) {
      result = queue + [item]
    }
    
    result
  }
  
  let dequeue_priority = fn(queue: Array[PriorityItem[T]]) {
    if queue.length() > 0 {
      (Some(queue[0]), queue.slice(1, queue.length()))
    } else {
      (None, queue)
    }
  }
  
  let peek_priority = fn(queue: Array[PriorityItem[T]]) {
    if queue.length() > 0 {
      Some(queue[0])
    } else {
      None
    }
  }
  
  // Test priority queue operations
  let mut queue = []
  
  // Enqueue operations with different priorities
  queue = enqueue_priority(queue, create_priority_item("metric-low", 1))
  queue = enqueue_priority(queue, create_priority_item("metric-high", 10))
  queue = enqueue_priority(queue, create_priority_item("metric-medium", 5))
  queue = enqueue_priority(queue, create_priority_item("metric-critical", 15))
  
  // Peek should return highest priority item
  assert_eq(peek_priority(queue), Some(create_priority_item("metric-critical", 15)))
  
  // Dequeue operations should return items in priority order
  let (first, queue1) = dequeue_priority(queue)
  assert_eq(first, Some(create_priority_item("metric-critical", 15)))
  
  let (second, queue2) = dequeue_priority(queue1)
  assert_eq(second, Some(create_priority_item("metric-high", 10)))
  
  let (third, queue3) = dequeue_priority(queue2)
  assert_eq(third, Some(create_priority_item("metric-medium", 5)))
  
  let (fourth, queue4) = dequeue_priority(queue3)
  assert_eq(fourth, Some(create_priority_item("metric-low", 1)))
  
  // Queue should be empty now
  let (empty_result, final_queue) = dequeue_priority(queue4)
  assert_eq(empty_result, None)
  assert_eq(final_queue.length(), 0)
}

// Test 7: Graph Operations
test "graph operations for telemetry service dependencies" {
  // Define graph edge type
  type Edge = {
    from: String,
    to: String,
    weight: Int
  }
  
  // Define graph type
  type Graph = {
    nodes: Array[String],
    edges: Array[Edge]
  }
  
  // Graph operations
  let create_graph = fn() {
    { nodes: [], edges: [] }
  }
  
  let add_node = fn(graph: Graph, node: String) {
    if not(graph.nodes.contains(node)) {
      { graph | nodes: graph.nodes.push(node) }
    } else {
      graph
    }
  }
  
  let add_edge = fn(graph: Graph, from: String, to: String, weight: Int) {
    let edge = { from, to, weight }
    { graph | edges: graph.edges.push(edge) }
  }
  
  let get_neighbors = fn(graph: Graph, node: String) {
    let mut neighbors = []
    for edge in graph.edges {
      if edge.from == node {
        neighbors = neighbors.push((edge.to, edge.weight))
      }
    }
    neighbors
  }
  
  let has_path = fn(graph: Graph, from: String, to: String) {
    let mut visited = []
    let mut queue = [from]
    let mut found = false
    
    while queue.length() > 0 and not(found) {
      let current = queue[0]
      queue = queue.slice(1, queue.length())
      
      if not(visited.contains(current)) {
        visited = visited.push(current)
        
        if current == to {
          found = true
        } else {
          let neighbors = get_neighbors(graph, current)
          for (neighbor, _) in neighbors {
            if not(visited.contains(neighbor)) {
              queue = queue.push(neighbor)
            }
          }
        }
      }
    }
    
    found
  }
  
  // Test graph operations
  let mut graph = create_graph()
  
  // Add nodes
  graph = add_node(graph, "api-gateway")
  graph = add_node(graph, "auth-service")
  graph = add_node(graph, "user-service")
  graph = add_node(graph, "payment-service")
  graph = add_node(graph, "notification-service")
  
  assert_eq(graph.nodes.length(), 5)
  assert_true(graph.nodes.contains("api-gateway"))
  assert_true(graph.nodes.contains("payment-service"))
  
  // Add edges (dependencies)
  graph = add_edge(graph, "api-gateway", "auth-service", 1)
  graph = add_edge(graph, "api-gateway", "user-service", 1)
  graph = add_edge(graph, "user-service", "payment-service", 2)
  graph = add_edge(graph, "payment-service", "notification-service", 3)
  
  assert_eq(graph.edges.length(), 4)
  
  // Test neighbors
  let api_gateway_neighbors = get_neighbors(graph, "api-gateway")
  assert_eq(api_gateway_neighbors.length(), 2)
  assert_true(api_gateway_neighbors.contains(("auth-service", 1)))
  assert_true(api_gateway_neighbors.contains(("user-service", 1)))
  
  let payment_neighbors = get_neighbors(graph, "payment-service")
  assert_eq(payment_neighbors.length(), 1)
  assert_true(payment_neighbors.contains(("notification-service", 3)))
  
  // Test path finding
  assert_true(has_path(graph, "api-gateway", "auth-service"))
  assert_true(has_path(graph, "api-gateway", "payment-service"))
  assert_true(has_path(graph, "user-service", "notification-service"))
  assert_false(has_path(graph, "notification-service", "api-gateway"))
  assert_false(has_path(graph, "nonexistent", "auth-service"))
}

// Test 8: Circular Buffer Operations
test "circular buffer operations for telemetry streaming" {
  // Define circular buffer type
  type CircularBuffer[T] = {
    buffer: Array[Option[T]],
    head: Int,
    tail: Int,
    size: Int,
    capacity: Int
  }
  
  // Circular buffer operations
  let create_circular_buffer = fn(capacity: Int) {
    {
      buffer: Array::with_capacity(capacity).map(fn(_) { None }),
      head: 0,
      tail: 0,
      size: 0,
      capacity
    }
  }
  
  let is_empty = fn(buffer: CircularBuffer[T]) {
    buffer.size == 0
  }
  
  let is_full = fn(buffer: CircularBuffer[T]) {
    buffer.size == buffer.capacity
  }
  
  let enqueue = fn(cb: CircularBuffer[T], item: T) {
    if not(is_full(cb)) {
      let new_buffer = cb.buffer.set(cb.tail, Some(item))
      {
        buffer: new_buffer,
        head: cb.head,
        tail: (cb.tail + 1) % cb.capacity,
        size: cb.size + 1,
        capacity: cb.capacity
      }
    } else {
      cb // Buffer is full
    }
  }
  
  let dequeue = fn(cb: CircularBuffer[T]) {
    if not(is_empty(cb)) {
      let item = cb.buffer[cb.head]
      let new_buffer = cb.buffer.set(cb.head, None)
      (item, {
        buffer: new_buffer,
        head: (cb.head + 1) % cb.capacity,
        tail: cb.tail,
        size: cb.size - 1,
        capacity: cb.capacity
      })
    } else {
      (None, cb)
    }
  }
  
  let to_array = fn(cb: CircularBuffer[T]) {
    let mut result = []
    let mut index = cb.head
    
    for i in 0..cb.size {
      match cb.buffer[index] {
        Some(item) => result = result.push(item)
        None => ()
      }
      index = (index + 1) % cb.capacity
    }
    
    result
  }
  
  // Test circular buffer operations
  let mut cb = create_circular_buffer(5)
  
  // Initial state
  assert_true(is_empty(cb))
  assert_false(is_full(cb))
  assert_eq(cb.size, 0)
  
  // Enqueue operations
  cb = enqueue(cb, "event-1")
  cb = enqueue(cb, "event-2")
  cb = enqueue(cb, "event-3")
  
  assert_false(is_empty(cb))
  assert_eq(cb.size, 3)
  
  let array_result = to_array(cb)
  assert_eq(array_result, ["event-1", "event-2", "event-3"])
  
  // Dequeue operations
  let (first, cb1) = dequeue(cb)
  assert_eq(first, Some("event-1"))
  assert_eq(cb1.size, 2)
  
  let (second, cb2) = dequeue(cb1)
  assert_eq(second, Some("event-2"))
  assert_eq(cb2.size, 1)
  
  // Enqueue more items
  let cb3 = enqueue(cb2, "event-4")
  let cb4 = enqueue(cb3, "event-5")
  
  assert_eq(cb4.size, 3)
  
  let final_array = to_array(cb4)
  assert_eq(final_array, ["event-3", "event-4", "event-5"])
  
  // Test empty buffer dequeue
  let empty_cb = create_circular_buffer(3)
  let (empty_result, _) = dequeue(empty_cb)
  assert_eq(empty_result, None)
}