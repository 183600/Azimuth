// Azimuth Data Structure Tests
// This file contains comprehensive tests for various data structure operations

// Test 1: Stack Operations
test "stack operations with push/pop" {
  let mut stack = []
  
  // Test push operations
  stack = stack.push(10)
  stack = stack.push(20)
  stack = stack.push(30)
  
  assert_eq(stack.length(), 3)
  
  // Test pop operations
  let (popped_value, new_stack) = stack.pop()
  match popped_value {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  assert_eq(new_stack.length(), 2)
  
  // Test empty stack pop
  let empty_stack = []
  let (empty_popped, _) = empty_stack.pop()
  match empty_popped {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Queue Operations
test "queue operations with enqueue/dequeue" {
  let queue = [10, 20, 30]
  
  // Test dequeue (remove from front)
  if queue.length() > 0 {
    let front = queue[0]
    assert_eq(front, 10)
    
    let remainder = queue.slice(1, queue.length())
    assert_eq(remainder, [20, 30])
  } else {
    assert_true(false)
  }
  
  // Test enqueue (add to back)
  let new_queue = queue.push(40)
  assert_eq(new_queue, [10, 20, 30, 40])
  assert_eq(new_queue.length(), 4)
}

// Test 3: Binary Tree Operations
test "binary tree structure and traversal" {
  // Define binary tree structure
  type TreeNode[T] {
    Node(T, TreeNode[T], TreeNode[T])
    Leaf
  }
  
  // Create a simple tree
  let tree = TreeNode::Node(
    10,
    TreeNode::Node(5, TreeNode::Leaf, TreeNode::Leaf),
    TreeNode::Node(15, TreeNode::Leaf, TreeNode::Leaf)
  )
  
  // Test tree traversal
  let inorder_values = []
  let traverse_inorder = fn(node : TreeNode[Int]) {
    match node {
      TreeNode::Node(value, left, right) => {
        traverse_inorder(left)
        inorder_values = inorder_values.push(value)
        traverse_inorder(right)
      }
      TreeNode::Leaf => ()
    }
  }
  
  traverse_inorder(tree)
  assert_eq(inorder_values, [5, 10, 15])
}

// Test 4: Hash Map Operations
test "hash map operations and lookups" {
  // Simulate hash map using tuples
  let map = [("key1", "value1"), ("key2", "value2"), ("key3", "value3")]
  
  // Test lookup
  let find_value = fn(key : String, pairs : [(String, String)]) {
    let mut result = None
    for pair in pairs {
      if pair.0 == key {
        result = Some(pair.1)
        break
      }
    }
    result
  }
  
  let found = find_value("key2", map)
  match found {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  let not_found = find_value("key4", map)
  match not_found {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Linked List Operations
test "linked list operations" {
  // Define linked list structure
  type List[T] {
    Cons(T, List[T])
    Nil
  }
  
  // Create list
  let list = List::Cons(1, List::Cons(2, List::Cons(3, List::Nil)))
  
  // Test list length
  let list_length = fn(lst : List[Int]) {
    let mut count = 0
    let mut current = lst
    while true {
      match current {
        List::Cons(_, tail) => {
          count = count + 1
          current = tail
        }
        List::Nil => break
      }
    }
    count
  }
  
  assert_eq(list_length(list), 3)
  
  // Test list to array conversion
  let to_array = fn(lst : List[Int]) {
    let mut result = []
    let mut current = lst
    while true {
      match current {
        List::Cons(head, tail) => {
          result = result.push(head)
          current = tail
        }
        List::Nil => break
      }
    }
    result.reverse()
  }
  
  assert_eq(to_array(list), [1, 2, 3])
}

// Test 6: Set Operations
test "set operations and membership" {
  // Simulate set using array
  let set1 = [1, 2, 3, 4, 5]
  let set2 = [4, 5, 6, 7, 8]
  
  // Test union
  let union = fn(arr1 : [Int], arr2 : [Int]) {
    let mut result = arr1
    for item in arr2 {
      if !result.contains(item) {
        result = result.push(item)
      }
    }
    result
  }
  
  let union_result = union(set1, set2)
  assert_eq(union_result.length(), 8)
  assert_true(union_result.contains(1))
  assert_true(union_result.contains(8))
  
  // Test intersection
  let intersection = fn(arr1 : [Int], arr2 : [Int]) {
    let mut result = []
    for item in arr1 {
      if arr2.contains(item) && !result.contains(item) {
        result = result.push(item)
      }
    }
    result
  }
  
  let intersection_result = intersection(set1, set2)
  assert_eq(intersection_result, [4, 5])
}

// Test 7: Priority Queue Operations
test "priority queue with custom ordering" {
  // Define priority queue item
  type PriorityItem {
    PriorityItem(Int, String) // (priority, value)
  }
  
  let items = [
    PriorityItem::PriorityItem(3, "low"),
    PriorityItem::PriorityItem(1, "high"),
    PriorityItem::PriorityItem(2, "medium")
  ]
  
  // Sort by priority
  let sort_by_priority = fn(items : [PriorityItem]) {
    let mut result = items
    // Simple bubble sort for demonstration
    let n = result.length()
    for i in 0..<(n-1) {
      for j in 0..<(n-i-1) {
        match (result[j], result[j+1]) {
          (PriorityItem::PriorityItem(p1, _), PriorityItem::PriorityItem(p2, _)) => {
            if p1 > p2 {
              let temp = result[j]
              result[j] = result[j+1]
              result[j+1] = temp
            }
          }
        }
      }
    }
    result
  }
  
  let sorted = sort_by_priority(items)
  match sorted[0] {
    PriorityItem::PriorityItem(priority, value) => {
      assert_eq(priority, 1)
      assert_eq(value, "high")
    }
  }
}

// Test 8: Graph Operations
test "graph traversal and connectivity" {
  // Define graph as adjacency list
  let graph = [
    (0, [1, 2]),
    (1, [0, 3]),
    (2, [0, 3]),
    (3, [1, 2, 4]),
    (4, [3])
  ]
  
  // Test BFS traversal
  let bfs = fn(start : Int, graph : [(Int, [Int])]) {
    let mut visited = []
    let mut queue = [start]
    let mut result = []
    
    while queue.length() > 0 {
      let current = queue[0]
      queue = queue.slice(1, queue.length())
      
      if !visited.contains(current) {
        visited = visited.push(current)
        result = result.push(current)
        
        // Find neighbors
        let find_neighbors = fn(node : Int, g : [(Int, [Int])]) {
          let mut neighbors = []
          for pair in g {
            if pair.0 == node {
              neighbors = pair.1
              break
            }
          }
          neighbors
        }
        
        let neighbors = find_neighbors(current, graph)
        for neighbor in neighbors {
          if !visited.contains(neighbor) {
            queue = queue.push(neighbor)
          }
        }
      }
    }
    result
  }
  
  let traversal_result = bfs(0, graph)
  assert_eq(traversal_result, [0, 1, 2, 3, 4])
}

// Test 9: Heap Operations
test "min-heap operations" {
  // Simple heap implementation using array
  let mut heap = []
  
  // Insert elements
  let insert = fn(heap : [Int], value : Int) {
    let mut new_heap = heap.push(value)
    let mut index = new_heap.length() - 1
    
    // Bubble up
    while index > 0 {
      let parent = (index - 1) / 2
      if new_heap[parent] > new_heap[index] {
        let temp = new_heap[parent]
        new_heap[parent] = new_heap[index]
        new_heap[index] = temp
        index = parent
      } else {
        break
      }
    }
    new_heap
  }
  
  heap = insert(heap, 5)
  heap = insert(heap, 3)
  heap = insert(heap, 8)
  heap = insert(heap, 1)
  heap = insert(heap, 7)
  
  // Test heap property
  assert_eq(heap[0], 1) // Min element at root
  
  // Test heapify property for all nodes
  let is_valid_heap = fn(arr : [Int]) {
    let n = arr.length()
    for i in 0..<n {
      let left = 2 * i + 1
      let right = 2 * i + 2
      
      if left < n && arr[i] > arr[left] {
        false
      } else if right < n && arr[i] > arr[right] {
        false
      } else {
        true
      }
    }
  }
  
  assert_true(is_valid_heap(heap))
}

// Test 10: Tree Balancing
test "balanced tree verification" {
  // Define binary tree structure
  type BalancedTreeNode[T] {
    BNode(T, BalancedTreeNode[T], BalancedTreeNode[T])
    BLeaf
  }
  
  // Create balanced tree
  let balanced_tree = BalancedTreeNode::BNode(
    10,
    BalancedTreeNode::BNode(5, BalancedTreeNode::BLeaf, BalancedTreeNode::BLeaf),
    BalancedTreeNode::BNode(15, BalancedTreeNode::BLeaf, BalancedTreeNode::BLeaf)
  )
  
  // Create unbalanced tree
  let unbalanced_tree = BalancedTreeNode::BNode(
    10,
    BalancedTreeNode::BNode(
      5,
      BalancedTreeNode::BNode(2, BalancedTreeNode::BLeaf, BalancedTreeNode::BLeaf),
      BalancedTreeNode::BLeaf
    ),
    BalancedTreeNode::BLeaf
  )
  
  // Check if tree is balanced
  let is_balanced = fn(node : BalancedTreeNode[Int]) {
    let height_diff = fn(n : BalancedTreeNode[Int]) {
      let max_depth = fn(n : BalancedTreeNode[Int]) {
        match n {
          BalancedTreeNode::BNode(_, left, right) => {
            let left_depth = max_depth(left)
            let right_depth = max_depth(right)
            1 + (if left_depth > right_depth { left_depth } else { right_depth })
          }
          BalancedTreeNode::BLeaf => 0
        }
      }
      
      let min_depth = fn(n : BalancedTreeNode[Int]) {
        match n {
          BalancedTreeNode::BNode(_, left, right) => {
            let left_depth = min_depth(left)
            let right_depth = min_depth(right)
            1 + (if left_depth < right_depth { left_depth } else { right_depth })
          }
          BalancedTreeNode::BLeaf => 0
        }
      }
      
      (max_depth(node) - min_depth(node)) <= 1
    }
    
    height_diff(node)
  }
  
  assert_true(is_balanced(balanced_tree))
  assert_true(is_balanced(unbalanced_tree))
}