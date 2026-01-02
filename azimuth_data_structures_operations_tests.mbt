// Azimuth Data Structures Operations Test Suite
// This file contains comprehensive test cases for data structure operations

// Test 1: Advanced Array Operations
test "advanced array operations" {
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  // Test array slicing
  let slice = numbers[2:6]  // Should contain [3, 4, 5, 6]
  assert_eq(slice.length(), 4)
  assert_eq(slice[0], 3)
  assert_eq(slice[3], 6)
  
  // Test array concatenation
  let first_half = numbers[0:5]
  let second_half = numbers[5:10]
  let concatenated = first_half @ second_half
  assert_eq(concatenated.length(), 10)
  assert_eq(concatenated, numbers)
  
  // Test array reversal
  let mut reversed = []
  for i in 0..numbers.length() {
    reversed = reversed @ [numbers[numbers.length() - 1 - i]]
  }
  assert_eq(reversed[0], 10)
  assert_eq(reversed[9], 1)
  
  // Test array filtering with complex condition
  let even_numbers = numbers.filter(fn(x) { x % 2 == 0 })
  let odd_numbers = numbers.filter(fn(x) { x % 2 != 0 })
  assert_eq(even_numbers.length(), 5)
  assert_eq(odd_numbers.length(), 5)
  
  // Test array transformation
  let squared = numbers.map(fn(x) { x * x })
  assert_eq(squared[0], 1)
  assert_eq(squared[3], 16)
  assert_eq(squared[9], 100)
}

// Test 2: Stack Operations
test "stack operations implementation" {
  // Implement stack using array
  let mut stack = []
  
  // Test push operations
  stack = stack.push(10)
  stack = stack.push(20)
  stack = stack.push(30)
  
  assert_eq(stack.length(), 3)
  
  // Test pop operations
  let (popped_value, new_stack) = (stack[stack.length() - 1], stack[0:stack.length() - 1])
  stack = new_stack
  assert_eq(popped_value, 30)
  assert_eq(stack.length(), 2)
  
  // Test peek operation
  let top_value = stack[stack.length() - 1]
  assert_eq(top_value, 20)
  
  // Test empty stack
  stack = stack[0:stack.length() - 1]  // Pop 20
  stack = stack[0:stack.length() - 1]  // Pop 10
  assert_eq(stack.length(), 0)
}

// Test 3: Queue Operations
test "queue operations implementation" {
  // Implement queue using array
  let mut queue = []
  
  // Test enqueue operations
  queue = queue @ [10]
  queue = queue @ [20]
  queue = queue @ [30]
  
  assert_eq(queue.length(), 3)
  
  // Test dequeue operations
  let (dequeued_value, new_queue) = (queue[0], queue[1:queue.length()])
  queue = new_queue
  assert_eq(dequeued_value, 10)
  assert_eq(queue.length(), 2)
  
  // Test front operation
  let front_value = queue[0]
  assert_eq(front_value, 20)
  
  // Test empty queue
  queue = queue[1:queue.length()]  // Dequeue 20
  queue = queue[1:queue.length()]  // Dequeue 30
  assert_eq(queue.length(), 0)
}

// Test 4: Dictionary/Map Operations
test "dictionary map operations" {
  // Implement map using array of key-value pairs
  let mut map = [
    ("apple", 5),
    ("banana", 3),
    ("cherry", 8)
  ]
  
  // Test lookup
  let find_value = fn(key, map_data) {
    let mut found = None
    for (k, v) in map_data {
      if k == key {
        found = Some(v)
        break
      }
    }
    found
  }
  
  match find_value("apple", map) {
    Some(v) => assert_eq(v, 5)
    None => assert_true(false)
  }
  
  match find_value("orange", map) {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test insert/update
  let update_map = fn(key, value, map_data) {
    let mut updated = false
    let mut result = []
    
    for (k, v) in map_data {
      if k == key {
        result = result @ [(k, value)]
        updated = true
      } else {
        result = result @ [(k, v)]
      }
    }
    
    if not updated {
      result = result @ [(key, value)]
    }
    
    result
  }
  
  map = update_map("banana", 4, map)
  match find_value("banana", map) {
    Some(v) => assert_eq(v, 4)
    None => assert_true(false)
  }
  
  map = update_map("orange", 6, map)
  match find_value("orange", map) {
    Some(v) => assert_eq(v, 6)
    None => assert_true(false)
  }
  
  assert_eq(map.length(), 4)
}

// Test 5: Set Operations
test "set operations implementation" {
  // Implement set using array
  let set1 = [1, 2, 3, 4, 5]
  let set2 = [4, 5, 6, 7, 8]
  
  // Test union
  let union = fn(s1, s2) {
    let mut result = s1
    for elem in s2 {
      let mut found = false
      for e in s1 {
        if e == elem {
          found = true
          break
        }
      }
      if not found {
        result = result @ [elem]
      }
    }
    result
  }
  
  let union_set = union(set1, set2)
  assert_eq(union_set.length(), 8)
  
  // Test intersection
  let intersection = fn(s1, s2) {
    let mut result = []
    for elem in s1 {
      for e in s2 {
        if e == elem {
          result = result @ [elem]
          break
        }
      }
    }
    result
  }
  
  let intersection_set = intersection(set1, set2)
  assert_eq(intersection_set.length(), 2)
  
  // Test difference
  let difference = fn(s1, s2) {
    let mut result = []
    for elem in s1 {
      let mut found = false
      for e in s2 {
        if e == elem {
          found = true
          break
        }
      }
      if not found {
        result = result @ [elem]
      }
    }
    result
  }
  
  let diff_set = difference(set1, set2)
  assert_eq(diff_set.length(), 3)
  
  // Test subset
  let is_subset = fn(s1, s2) {
    for elem in s1 {
      let mut found = false
      for e in s2 {
        if e == elem {
          found = true
          break
        }
      }
      if not found {
        return false
      }
    }
    true
  }
  
  assert_true(is_subset([1, 2], set1))
  assert_false(is_subset([1, 6], set1))
}

// Test 6: Tree Operations
test "tree operations implementation" {
  // Implement binary tree node
  type TreeNode {
    value : Int
    left : Option[TreeNode]
    right : Option[TreeNode]
  }
  
  // Create a simple tree
  let leaf1 = { value: 1, left: None, right: None }
  let leaf2 = { value: 3, left: None, right: None }
  let leaf3 = { value: 5, left: None, right: None }
  let leaf4 = { value: 7, left: None, right: None }
  
  let node2 = { value: 2, left: Some(leaf1), right: Some(leaf2) }
  let node3 = { value: 6, left: Some(leaf3), right: Some(leaf4) }
  let root = { value: 4, left: Some(node2), right: Some(node3) }
  
  // Test tree traversal (in-order)
  let inorder_traversal = fn(node, acc) {
    match node {
      Some(n) => {
        let left_result = inorder_traversal(n.left, acc)
        let with_current = left_result @ [n.value]
        let right_result = inorder_traversal(n.right, with_current)
        right_result
      }
      None => acc
    }
  }
  
  let result = inorder_traversal(Some(root), [])
  assert_eq(result, [1, 2, 3, 4, 5, 6, 7])
  
  // Test tree search
  let tree_search = fn(node, target) {
    match node {
      Some(n) => {
        if n.value == target {
          true
        } else if target < n.value {
          tree_search(n.left, target)
        } else {
          tree_search(n.right, target)
        }
      }
      None => false
    }
  }
  
  assert_true(tree_search(Some(root), 5))
  assert_false(tree_search(Some(root), 8))
}

// Test 7: Linked List Operations
test "linked list operations implementation" {
  // Implement linked list node
  type ListNode {
    value : Int
    next : Option[ListNode]
  }
  
  // Create a linked list: 1 -> 2 -> 3 -> 4 -> 5
  let node5 = { value: 5, next: None }
  let node4 = { value: 4, next: Some(node5) }
  let node3 = { value: 3, next: Some(node4) }
  let node2 = { value: 2, next: Some(node3) }
  let node1 = { value: 1, next: Some(node2) }
  
  // Test linked list traversal
  let list_to_array = fn(node) {
    let mut result = []
    let mut current = node
    while true {
      match current {
        Some(n) => {
          result = result @ [n.value]
          current = n.next
        }
        None => break
      }
    }
    result
  }
  
  let array_result = list_to_array(Some(node1))
  assert_eq(array_result, [1, 2, 3, 4, 5])
  
  // Test linked list search
  let list_search = fn(node, target) {
    let mut current = node
    let mut found = false
    while true {
      match current {
        Some(n) => {
          if n.value == target {
            found = true
            break
          }
          current = n.next
        }
        None => break
      }
    }
    found
  }
  
  assert_true(list_search(Some(node1), 3))
  assert_false(list_search(Some(node1), 6))
  
  // Test linked list length
  let list_length = fn(node) {
    let mut count = 0
    let mut current = node
    while true {
      match current {
        Some(_) => {
          count = count + 1
          match current {
            Some(n) => current = n.next
            None => break
          }
        }
        None => break
      }
    }
    count
  }
  
  assert_eq(list_length(Some(node1)), 5)
  assert_eq(list_length(None), 0)
}

// Test 8: Graph Operations
test "graph operations implementation" {
  // Implement graph using adjacency list
  type Graph {
    vertices : Array[String]
    edges : Array[(String, String)]
  }
  
  let graph = {
    vertices: ["A", "B", "C", "D", "E"],
    edges: [
      ("A", "B"), ("A", "C"),
      ("B", "D"), ("C", "D"),
      ("D", "E")
    ]
  }
  
  // Test get neighbors
  let get_neighbors = fn(vertex, g) {
    let mut neighbors = []
    for (from, to) in g.edges {
      if from == vertex {
        neighbors = neighbors @ [to]
      }
    }
    neighbors
  }
  
  let a_neighbors = get_neighbors("A", graph)
  assert_eq(a_neighbors.length(), 2)
  assert_true(a_neighbors.contains("B"))
  assert_true(a_neighbors.contains("C"))
  
  let d_neighbors = get_neighbors("D", graph)
  assert_eq(d_neighbors.length(), 1)
  assert_eq(d_neighbors[0], "E")
  
  // Test has edge
  let has_edge = fn(from, to, g) {
    let mut found = false
    for (f, t) in g.edges {
      if f == from and t == to {
        found = true
        break
      }
    }
    found
  }
  
  assert_true(has_edge("A", "B", graph))
  assert_true(has_edge("D", "E", graph))
  assert_false(has_edge("B", "A", graph))
  assert_false(has_edge("E", "A", graph))
  
  // Test vertex degree
  let vertex_degree = fn(vertex, g) {
    let mut degree = 0
    for (from, _) in g.edges {
      if from == vertex {
        degree = degree + 1
      }
    }
    degree
  }
  
  assert_eq(vertex_degree("A", graph), 2)
  assert_eq(vertex_degree("B", graph), 1)
  assert_eq(vertex_degree("E", graph), 0)
}