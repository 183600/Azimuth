// Advanced Data Structures Test Cases for Azimuth Telemetry System
// This file contains test cases for advanced data structures used in telemetry

// Test 1: Tree Structure Operations
test "tree structure operations" {
  // Create a simple tree node structure
  type TreeNode {
    value : String
    children : Array[TreeNode]
  }
  
  let root = TreeNode {
    value: "root",
    children: [
      TreeNode {
        value: "child1",
        children: [
          TreeNode { value: "grandchild1", children: [] },
          TreeNode { value: "grandchild2", children: [] }
        ]
      },
      TreeNode {
        value: "child2",
        children: [
          TreeNode { value: "grandchild3", children: [] }
        ]
      }
    ]
  }
  
  // Test tree traversal
  let mut count = 0
  let mut stack = [root]
  
  while stack.length() > 0 {
    let node = stack.pop()
    count = count + 1
    for child in node.children {
      stack.push(child)
    }
  }
  
  assert_eq(count, 6) // root + 2 children + 3 grandchildren
}

// Test 2: Graph Structure Operations
test "graph structure operations" {
  // Create a simple graph using adjacency list
  type GraphNode {
    id : Int
    value : String
    edges : Array[Int] // Store indices of connected nodes
  }
  
  let nodes = [
    GraphNode { id: 0, value: "A", edges: [1, 2] },
    GraphNode { id: 1, value: "B", edges: [0, 3] },
    GraphNode { id: 2, value: "C", edges: [0, 3] },
    GraphNode { id: 3, value: "D", edges: [1, 2] }
  ]
  
  // Test graph traversal (BFS)
  let mut visited = []
  let mut queue = [0] // Start with node index 0
  let mut traversal_order = []
  
  while queue.length() > 0 {
    let current_index = queue.shift()
    
    // Skip if already visited
    let mut already_visited = false
    for v in visited {
      if v == current_index {
        already_visited = true
        break
      }
    }
    
    if already_visited {
      continue
    }
    
    visited.push(current_index)
    traversal_order.push(nodes[current_index].value)
    
    // Add neighbors to queue
    for neighbor_index in nodes[current_index].edges {
      queue.push(neighbor_index)
    }
  }
  
  assert_eq(traversal_order.length(), 4)
  assert_eq(traversal_order[0], "A")
}

// Test 3: Priority Queue Operations
test "priority queue operations" {
  // Simple priority queue implementation using array
  type PriorityQueue {
    elements : Array[(Int, String)] // (priority, value)
  }
  
  let enqueue = fn(queue: PriorityQueue, priority: Int, value: String) -> PriorityQueue {
    let mut new_elements = queue.elements
    new_elements.push((priority, value))
    
    // Sort by priority (ascending)
    let mut sorted = true
    for i in 0..<(new_elements.length() - 1) {
      if new_elements[i].0 > new_elements[i + 1].0 {
        sorted = false
        break
      }
    }
    
    if !sorted {
      // Simple bubble sort
      let mut n = new_elements.length()
      while n > 1 {
        for i in 0..<(n - 1) {
          if new_elements[i].0 > new_elements[i + 1].0 {
            let temp = new_elements[i]
            new_elements[i] = new_elements[i + 1]
            new_elements[i + 1] = temp
          }
        }
        n = n - 1
      }
    }
    
    PriorityQueue { elements: new_elements }
  }
  
  let dequeue = fn(queue: PriorityQueue) -> (String, PriorityQueue) {
    if queue.elements.length() == 0 {
      ("", PriorityQueue { elements: [] })
    } else {
      let value = queue.elements[0].1
      let mut new_elements = []
      for i in 1..queue.elements.length() {
        new_elements.push(queue.elements[i])
      }
      (value, PriorityQueue { elements: new_elements })
    }
  }
  
  let mut pq = PriorityQueue { elements: [] }
  pq = enqueue(pq, 3, "low priority")
  pq = enqueue(pq, 1, "high priority")
  pq = enqueue(pq, 2, "medium priority")
  
  let (first, pq1) = dequeue(pq)
  assert_eq(first, "high priority")
  
  let (second, pq2) = dequeue(pq1)
  assert_eq(second, "medium priority")
  
  let (third, _) = dequeue(pq2)
  assert_eq(third, "low priority")
}

// Test 4: Hash Map Operations
test "hash map operations" {
  // Simple hash map using array of key-value pairs
  type HashMap {
    entries : Array[(String, String)]
  }
  
  let put = fn(map: HashMap, key: String, value: String) -> HashMap {
    let mut new_entries = []
    let mut key_found = false
    
    // Update existing key
    for entry in map.entries {
      if entry.0 == key {
        new_entries.push((key, value))
        key_found = true
      } else {
        new_entries.push(entry)
      }
    }
    
    // Add new key if not found
    if !key_found {
      new_entries.push((key, value))
    }
    
    HashMap { entries: new_entries }
  }
  
  let get = fn(map: HashMap, key: String) -> Option[String] {
    for entry in map.entries {
      if entry.0 == key {
        return Some(entry.1)
      }
    }
    None
  }
  
  let mut map = HashMap { entries: [] }
  map = put(map, "key1", "value1")
  map = put(map, "key2", "value2")
  map = put(map, "key1", "updated_value1") // Update existing key
  
  match get(map, "key1") {
    Some(value) => assert_eq(value, "updated_value1")
    None => assert_true(false)
  }
  
  match get(map, "key2") {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match get(map, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Set Operations
test "set operations" {
  // Simple set implementation using array
  type Set {
    elements : Array[String]
  }
  
  let add = fn(set: Set, element: String) -> Set {
    // Check if element already exists
    let mut exists = false
    for e in set.elements {
      if e == element {
        exists = true
        break
      }
    }
    
    if !exists {
      let mut new_elements = set.elements
      new_elements.push(element)
      Set { elements: new_elements }
    } else {
      set
    }
  }
  
  let contains = fn(set: Set, element: String) -> Bool {
    for e in set.elements {
      if e == element {
        return true
      }
    }
    false
  }
  
  let union = fn(set1: Set, set2: Set) -> Set {
    let mut result = set1
    for element in set2.elements {
      result = add(result, element)
    }
    result
  }
  
  let intersection = fn(set1: Set, set2: Set) -> Set {
    let mut result = Set { elements: [] }
    for element in set1.elements {
      if contains(set2, element) {
        result = add(result, element)
      }
    }
    result
  }
  
  let set1 = Set { elements: ["a", "b", "c"] }
  let set2 = Set { elements: ["c", "d", "e"] }
  
  let union_set = union(set1, set2)
  assert_eq(union_set.elements.length(), 5)
  assert_true(contains(union_set, "a"))
  assert_true(contains(union_set, "b"))
  assert_true(contains(union_set, "c"))
  assert_true(contains(union_set, "d"))
  assert_true(contains(union_set, "e"))
  
  let intersection_set = intersection(set1, set2)
  assert_eq(intersection_set.elements.length(), 1)
  assert_true(contains(intersection_set, "c"))
}

// Test 6: Stack Operations
test "stack operations" {
  type Stack {
    elements : Array[String]
  }
  
  let push = fn(stack: Stack, element: String) -> Stack {
    let mut new_elements = stack.elements
    new_elements.push(element)
    Stack { elements: new_elements }
  }
  
  let pop = fn(stack: Stack) -> (Option[String], Stack) {
    if stack.elements.length() == 0 {
      (None, stack)
    } else {
      let element = stack.elements[stack.elements.length() - 1]
      let mut new_elements = []
      for i in 0..<(stack.elements.length() - 1) {
        new_elements.push(stack.elements[i])
      }
      (Some(element), Stack { elements: new_elements })
    }
  }
  
  let peek = fn(stack: Stack) -> Option[String] {
    if stack.elements.length() == 0 {
      None
    } else {
      Some(stack.elements[stack.elements.length() - 1])
    }
  }
  
  let mut stack = Stack { elements: [] }
  stack = push(stack, "first")
  stack = push(stack, "second")
  stack = push(stack, "third")
  
  match peek(stack) {
    Some(element) => assert_eq(element, "third")
    None => assert_true(false)
  }
  
  let (element, stack1) = pop(stack)
  match element {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  let (element2, stack2) = pop(stack1)
  match element2 {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  let (element3, _) = pop(stack2)
  match element3 {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
}

// Test 7: Queue Operations
test "queue operations" {
  type Queue {
    elements : Array[String]
  }
  
  let enqueue = fn(queue: Queue, element: String) -> Queue {
    let mut new_elements = queue.elements
    new_elements.push(element)
    Queue { elements: new_elements }
  }
  
  let dequeue = fn(queue: Queue) -> (Option[String], Queue) {
    if queue.elements.length() == 0 {
      (None, queue)
    } else {
      let element = queue.elements[0]
      let mut new_elements = []
      for i in 1..queue.elements.length() {
        new_elements.push(queue.elements[i])
      }
      (Some(element), Queue { elements: new_elements })
    }
  }
  
  let front = fn(queue: Queue) -> Option[String] {
    if queue.elements.length() == 0 {
      None
    } else {
      Some(queue.elements[0])
    }
  }
  
  let mut queue = Queue { elements: [] }
  queue = enqueue(queue, "first")
  queue = enqueue(queue, "second")
  queue = enqueue(queue, "third")
  
  match front(queue) {
    Some(element) => assert_eq(element, "first")
    None => assert_true(false)
  }
  
  let (element, queue1) = dequeue(queue)
  match element {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  let (element2, queue2) = dequeue(queue1)
  match element2 {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  let (element3, _) = dequeue(queue2)
  match element3 {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
}

// Test 8: Linked List Operations
test "linked list operations" {
  type ListNode {
    value : String
    next : Option[ListNode]
  }
  
  type LinkedList {
    head : Option[ListNode]
    size : Int
  }
  
  let create_list = fn() -> LinkedList {
    LinkedList { head: None, size: 0 }
  }
  
  let add_front = fn(list: LinkedList, value: String) -> LinkedList {
    let new_node = ListNode {
      value: value,
      next: list.head
    }
    LinkedList { head: Some(new_node), size: list.size + 1 }
  }
  
  let get_at = fn(list: LinkedList, index: Int) -> Option[String] {
    if index < 0 || index >= list.size {
      None
    } else {
      let mut current = list.head
      let mut current_index = 0
      
      while current_index < index {
        match current {
          Some(node) => current = node.next
          None => return None
        }
        current_index = current_index + 1
      }
      
      match current {
        Some(node) => Some(node.value)
        None => None
      }
    }
  }
  
  let mut list = create_list()
  list = add_front(list, "first")
  list = add_front(list, "second")
  list = add_front(list, "third")
  
  assert_eq(list.size, 3)
  
  match get_at(list, 0) {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  match get_at(list, 1) {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  match get_at(list, 2) {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  match get_at(list, 3) {
    Some(_) => assert_true(false) // Out of bounds
    None => assert_true(true)
  }
}