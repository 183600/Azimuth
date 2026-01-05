// Azimuth Premium Data Structure Tests
// This file contains high-quality test cases for advanced data structures

// Test 1: Tree Structure Operations
test "tree structure operations" {
  // Test binary tree creation and traversal
  type TreeNode {
    value : Int
    left : Option[TreeNode]
    right : Option[TreeNode]
  }
  
  let leaf1 = TreeNode { value: 1, left: None, right: None }
  let leaf2 = TreeNode { value: 3, left: None, right: None }
  let leaf3 = TreeNode { value: 5, left: None, right: None }
  let leaf4 = TreeNode { value: 7, left: None, right: None }
  
  let node1 = TreeNode { value: 2, left: Some(leaf1), right: Some(leaf2) }
  let node2 = TreeNode { value: 6, left: Some(leaf3), right: Some(leaf4) }
  let root = TreeNode { value: 4, left: Some(node1), right: Some(node2) }
  
  // Test tree traversal (in-order)
  let mut inorder_result = []
  let mut stack = [Some(root)]
  
  while stack.length() > 0 {
    let current = stack.pop()
    match current {
      Some(node) => {
        // Visit right subtree first (stack LIFO)
        match node.right {
          Some(right) => stack.push(Some(right))
          None => ()
        }
        stack.push(None) // Marker for processing the node
        match node.left {
          Some(left) => stack.push(Some(left))
          None => ()
        }
      }
      None => {
        if stack.length() > 0 {
          let process_node = stack.pop()
          match process_node {
            Some(node) => inorder_result.push(node.value)
            None => ()
          }
        }
      }
    }
  }
  
  assert_eq(inorder_result, [1, 2, 3, 4, 5, 6, 7])
  
  // Test tree height calculation
  let tree_height = fn(node : Option[TreeNode]) -> Int {
    match node {
      Some(n) => {
        let left_height = tree_height(n.left)
        let right_height = tree_height(n.right)
        1 + (if left_height > right_height { left_height } else { right_height })
      }
      None => 0
    }
  }
  
  assert_eq(tree_height(Some(root)), 3)
  assert_eq(tree_height(Some(leaf1)), 1)
  assert_eq(tree_height(None), 0)
}

// Test 2: Graph Structure Operations
test "graph structure operations" {
  // Test adjacency list representation
  type Graph {
    vertices : Array[String]
    edges : Array[(Int, Int)] // Indices into vertices array
  }
  
  let vertices = ["A", "B", "C", "D", "E"]
  let edges = [(0, 1), (0, 2), (1, 3), (2, 3), (3, 4)]
  let graph = Graph { vertices: vertices, edges: edges }
  
  // Test degree calculation
  let calculate_degree = fn(g : Graph, vertex_index : Int) -> Int {
    let mut degree = 0
    for (from, to) in g.edges {
      if from == vertex_index { degree = degree + 1 }
      if to == vertex_index { degree = degree + 1 }
    }
    degree
  }
  
  assert_eq(calculate_degree(graph, 0), 2) // Vertex A
  assert_eq(calculate_degree(graph, 1), 2) // Vertex B
  assert_eq(calculate_degree(graph, 2), 2) // Vertex C
  assert_eq(calculate_degree(graph, 3), 3) // Vertex D
  assert_eq(calculate_degree(graph, 4), 1) // Vertex E
  
  // Test breadth-first search
  let bfs = fn(g : Graph, start_index : Int) -> Array[Int] {
    let mut visited = []
    let mut queue = [start_index]
    let mut discovered = [false; g.vertices.length()]
    discovered[start_index] = true
    
    while queue.length() > 0 {
      let current = queue.shift()
      visited.push(current)
      
      // Find all neighbors
      for (from, to) in g.edges {
        let neighbor = if from == current { to } else if to == current { from } else { -1 }
        if neighbor >= 0 && !discovered[neighbor] {
          discovered[neighbor] = true
          queue.push(neighbor)
        }
      }
    }
    
    visited
  }
  
  let bfs_result = bfs(graph, 0)
  assert_eq(bfs_result.length(), 5) // All vertices should be reachable
  assert_eq(bfs_result[0], 0) // Start with vertex A
}

// Test 3: Priority Queue Operations
test "priority queue operations" {
  // Test min-heap implementation
  type PriorityQueue {
    heap : Array[(Int, String)] // (priority, value)
  }
  
  let create_priority_queue = fn() -> PriorityQueue {
    PriorityQueue { heap: [] }
  }
  
  let heapify_up = fn(pq : PriorityQueue, index : Int) -> PriorityQueue {
    if index <= 0 { return pq }
    
    let parent_index = (index - 1) / 2
    if pq.heap[index].0 < pq.heap[parent_index].0 {
      let mut new_heap = pq.heap
      let temp = new_heap[index]
      new_heap[index] = new_heap[parent_index]
      new_heap[parent_index] = temp
      
      let new_pq = PriorityQueue { heap: new_heap }
      return heapify_up(new_pq, parent_index)
    }
    
    pq
  }
  
  let enqueue = fn(pq : PriorityQueue, priority : Int, value : String) -> PriorityQueue {
    let mut new_heap = pq.heap
    new_heap.push((priority, value))
    let new_pq = PriorityQueue { heap: new_heap }
    heapify_up(new_pq, new_heap.length() - 1)
  }
  
  let heapify_down = fn(pq : PriorityQueue, index : Int) -> PriorityQueue {
    let left_child = 2 * index + 1
    let right_child = 2 * index + 2
    let mut smallest = index
    
    if left_child < pq.heap.length() && pq.heap[left_child].0 < pq.heap[smallest].0 {
      smallest = left_child
    }
    
    if right_child < pq.heap.length() && pq.heap[right_child].0 < pq.heap[smallest].0 {
      smallest = right_child
    }
    
    if smallest != index {
      let mut new_heap = pq.heap
      let temp = new_heap[index]
      new_heap[index] = new_heap[smallest]
      new_heap[smallest] = temp
      
      let new_pq = PriorityQueue { heap: new_heap }
      return heapify_down(new_pq, smallest)
    }
    
    pq
  }
  
  let dequeue = fn(pq : PriorityQueue) -> (Option[(Int, String)], PriorityQueue) {
    if pq.heap.length() == 0 {
      return (None, pq)
    }
    
    if pq.heap.length() == 1 {
      return (Some(pq.heap[0]), PriorityQueue { heap: [] })
    }
    
    let mut new_heap = pq.heap
    let min_value = new_heap[0]
    new_heap[0] = new_heap.pop()
    let new_pq = PriorityQueue { heap: new_heap }
    let final_pq = heapify_down(new_pq, 0)
    
    (Some(min_value), final_pq)
  }
  
  // Test priority queue operations
  let mut pq = create_priority_queue()
  
  // Enqueue elements with different priorities
  pq = enqueue(pq, 5, "low priority")
  pq = enqueue(pq, 1, "high priority")
  pq = enqueue(pq, 3, "medium priority")
  pq = enqueue(pq, 2, "high-medium priority")
  pq = enqueue(pq, 4, "medium-low priority")
  
  // Dequeue elements (should come out in priority order)
  let (result1, pq1) = dequeue(pq)
  match result1 {
    Some((priority, value)) => {
      assert_eq(priority, 1)
      assert_eq(value, "high priority")
    }
    None => assert_true(false)
  }
  
  let (result2, pq2) = dequeue(pq1)
  match result2 {
    Some((priority, value)) => {
      assert_eq(priority, 2)
      assert_eq(value, "high-medium priority")
    }
    None => assert_true(false)
  }
  
  let (result3, pq3) = dequeue(pq2)
  match result3 {
    Some((priority, value)) => {
      assert_eq(priority, 3)
      assert_eq(value, "medium priority")
    }
    None => assert_true(false)
  }
  
  let (result4, pq4) = dequeue(pq3)
  match result4 {
    Some((priority, value)) => {
      assert_eq(priority, 4)
      assert_eq(value, "medium-low priority")
    }
    None => assert_true(false)
  }
  
  let (result5, pq5) = dequeue(pq4)
  match result5 {
    Some((priority, value)) => {
      assert_eq(priority, 5)
      assert_eq(value, "low priority")
    }
    None => assert_true(false)
  }
  
  // Queue should be empty now
  let (result6, _) = dequeue(pq5)
  assert_eq(result6, None)
}

// Test 4: Advanced Hash Map Operations
test "advanced hash map operations" {
  // Test hash map with collision handling
  type HashMapEntry {
    key : String
    value : Int
    next : Option[HashMapEntry]
  }
  
  type HashMap {
    buckets : Array[Option[HashMapEntry]]
    size : Int
  }
  
  let hash_function = fn(key : String, capacity : Int) -> Int {
    let mut hash = 0
    for char in key.to_char_array() {
      hash = (hash * 31 + char.to_int()) % capacity
    }
    if hash < 0 { hash = hash + capacity }
    hash
  }
  
  let create_hash_map = fn(capacity : Int) -> HashMap {
    HashMap { 
      buckets: [None; capacity], 
      size: 0 
    }
  }
  
  let put = fn(map : HashMap, key : String, value : Int) -> HashMap {
    let index = hash_function(key, map.buckets.length())
    let mut new_buckets = map.buckets
    
    // Check if key already exists
    let mut current = new_buckets[index]
    let mut found = false
    
    match current {
      Some(entry) => {
        if entry.key == key {
          // Update existing entry
          new_buckets[index] = Some(HashMapEntry { 
            key: key, 
            value: value, 
            next: entry.next 
          })
          found = true
        } else {
          // Traverse linked list
          let mut prev = Some(entry)
          let mut curr = entry.next
          
          while curr.is_some() {
            match curr {
              Some(curr_entry) => {
                if curr_entry.key == key {
                  // Update existing entry
                  match prev {
                    Some(prev_entry) => {
                      new_buckets[index] = Some(HashMapEntry {
                        key: prev_entry.key,
                        value: prev_entry.value,
                        next: Some(HashMapEntry {
                          key: key,
                          value: value,
                          next: curr_entry.next
                        })
                      })
                    }
                    None => ()
                  }
                  found = true
                  break
                }
                prev = curr
                curr = curr_entry.next
              }
              None => break
            }
          }
        }
      }
      None => ()
    }
    
    if !found {
      // Add new entry
      let new_entry = HashMapEntry { 
        key: key, 
        value: value, 
        next: new_buckets[index] 
      }
      new_buckets[index] = Some(new_entry)
    }
    
    HashMap { 
      buckets: new_buckets, 
      size: if found { map.size } else { map.size + 1 } 
    }
  }
  
  let get = fn(map : HashMap, key : String) -> Option[Int] {
    let index = hash_function(key, map.buckets.length())
    let mut current = map.buckets[index]
    
    while current.is_some() {
      match current {
        Some(entry) => {
          if entry.key == key {
            return Some(entry.value)
          }
          current = entry.next
        }
        None => break
      }
    }
    
    None
  }
  
  // Test hash map operations
  let mut map = create_hash_map(10)
  
  // Add entries
  map = put(map, "apple", 5)
  map = put(map, "banana", 3)
  map = put(map, "cherry", 7)
  map = put(map, "date", 2)
  map = put(map, "elderberry", 9)
  
  // Test retrieval
  assert_eq(get(map, "apple"), Some(5))
  assert_eq(get(map, "banana"), Some(3))
  assert_eq(get(map, "cherry"), Some(7))
  assert_eq(get(map, "date"), Some(2))
  assert_eq(get(map, "elderberry"), Some(9))
  
  // Test non-existent key
  assert_eq(get(map, "fig"), None)
  
  // Test update existing key
  map = put(map, "apple", 10)
  assert_eq(get(map, "apple"), Some(10))
  
  // Test collision handling (keys that hash to same bucket)
  map = put(map, "kelpp", 15) // Should collide with "apple"
  assert_eq(get(map, "kelpp"), Some(15))
  assert_eq(get(map, "apple"), Some(10)) // Original should still be accessible
}

// Test 5: Balanced Tree Operations
test "balanced tree operations" {
  // Test AVL tree rotation operations
  type AVLNode {
    value : Int
    left : Option[AVLNode]
    right : Option[AVLNode]
    height : Int
  }
  
  let height = fn(node : Option[AVLNode]) -> Int {
    match node {
      Some(n) => n.height
      None => 0
    }
  }
  
  let update_height = fn(node : AVLNode) -> AVLNode {
    let left_h = height(node.left)
    let right_h = height(node.right)
    AVLNode {
      value: node.value,
      left: node.left,
      right: node.right,
      height: 1 + (if left_h > right_h { left_h } else { right_h })
    }
  }
  
  let balance_factor = fn(node : AVLNode) -> Int {
    height(node.left) - height(node.right)
  }
  
  let rotate_right = fn(y : AVLNode) -> AVLNode {
    match y.left {
      Some(x) => {
        let T2 = x.right
        let new_x = AVLNode {
          value: x.value,
          left: x.left,
          right: Some(y),
          height: 0
        }
        let new_y = AVLNode {
          value: y.value,
          left: T2,
          right: y.right,
          height: 0
        }
        update_height(new_y)
        update_height(new_x)
      }
      None => y
    }
  }
  
  let rotate_left = fn(x : AVLNode) -> AVLNode {
    match x.right {
      Some(y) => {
        let T2 = y.left
        let new_y = AVLNode {
          value: y.value,
          left: Some(x),
          right: y.right,
          height: 0
        }
        let new_x = AVLNode {
          value: x.value,
          left: x.left,
          right: T2,
          height: 0
        }
        update_height(new_x)
        update_height(new_y)
      }
      None => x
    }
  }
  
  // Test basic tree creation
  let leaf1 = AVLNode { value: 10, left: None, right: None, height: 1 }
  let leaf2 = AVLNode { value: 30, left: None, right: None, height: 1 }
  let leaf3 = AVLNode { value: 40, left: None, right: None, height: 1 }
  
  let node1 = AVLNode { value: 20, left: Some(leaf1), right: Some(leaf2), height: 0 }
  let node1 = update_height(node1)
  
  let root = AVLNode { value: 30, left: Some(node1), right: Some(leaf3), height: 0 }
  let root = update_height(root)
  
  // Test height calculations
  assert_eq(height(Some(leaf1)), 1)
  assert_eq(height(Some(node1)), 2)
  assert_eq(height(Some(root)), 3)
  
  // Test balance factors
  assert_eq(balance_factor(leaf1), 0)
  assert_eq(balance_factor(node1), 0)
  assert_eq(balance_factor(root), 1)
  
  // Test rotations (simplified test)
  let left_heavy = AVLNode { 
    value: 30, 
    left: Some(AVLNode { value: 20, left: Some(leaf1), right: None, height: 0 }), 
    right: None, 
    height: 0 
  }
  
  let rotated = rotate_right(left_heavy)
  assert_eq(rotated.value, 20)
}