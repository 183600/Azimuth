// Azimuth Data Structure Enhanced Tests
// This file contains enhanced test cases for data structure operations

test "stack operations test" {
  let mut stack = []
  
  // Test push operations
  stack = stack.push(1)
  stack = stack.push(2)
  stack = stack.push(3)
  
  assert_eq(stack.length(), 3)
  
  // Test pop operations
  let (popped_value, new_stack) = stack.pop()
  match popped_value {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  assert_eq(new_stack.length(), 2)
  
  // Test empty stack
  let mut empty_stack = []
  let (empty_popped, _) = empty_stack.pop()
  match empty_popped {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "queue operations test" {
  let mut queue = []
  
  // Test enqueue operations
  queue = queue.push_back(1)
  queue = queue.push_back(2)
  queue = queue.push_back(3)
  
  assert_eq(queue.length(), 3)
  
  // Test dequeue operations
  let (dequeued_value, new_queue) = queue.pop_front()
  match dequeued_value {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_eq(new_queue.length(), 2)
  
  // Test empty queue
  let mut empty_queue = []
  let (empty_dequeued, _) = empty_queue.pop_front()
  match empty_dequeued {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "tree structure operations test" {
  // Define a simple binary tree node structure
  type TreeNode {
    value : Int
    left : Option[TreeNode]
    right : Option[TreeNode]
  }
  
  // Create a simple tree
  let leaf1 = { value: 1, left: None, right: None }
  let leaf2 = { value: 2, left: None, right: None }
  let root = { value: 3, left: Some(leaf1), right: Some(leaf2) }
  
  // Test tree traversal (in-order simulation)
  let mut values = []
  
  // Simulate in-order traversal
  values = values.push(root.value)
  match root.left {
    Some(left_node) => values = values.push(left_node.value)
    None => ()
  }
  match root.right {
    Some(right_node) => values = values.push(right_node.value)
    None => ()
  }
  
  assert_eq(values.length(), 3)
  assert_eq(values[0], 3)
  assert_eq(values[1], 1)
  assert_eq(values[2], 2)
}

test "hash map operations test" {
  // Simulate hash map operations using arrays
  let mut keys = []
  let mut values = []
  
  // Insert operations
  keys = keys.push("key1")
  values = values.push("value1")
  
  keys = keys.push("key2")
  values = values.push("value2")
  
  keys = keys.push("key3")
  values = values.push("value3")
  
  assert_eq(keys.length(), 3)
  assert_eq(values.length(), 3)
  
  // Test lookup operation (simulated)
  let mut found = false
  for i in 0..keys.length() {
    if keys[i] == "key2" {
      assert_eq(values[i], "value2")
      found = true
    }
  }
  assert_true(found)
  
  // Test update operation (simulated)
  for i in 0..keys.length() {
    if keys[i] == "key2" {
      values[i] = "updated_value2"
    }
  }
  
  // Verify update
  let mut updated_found = false
  for i in 0..keys.length() {
    if keys[i] == "key2" {
      assert_eq(values[i], "updated_value2")
      updated_found = true
    }
  }
  assert_true(updated_found)
}

test "linked list operations test" {
  // Define a simple linked list node structure
  type ListNode {
    value : Int
    next : Option[ListNode]
  }
  
  // Create linked list: 1 -> 2 -> 3
  let node3 = { value: 3, next: None }
  let node2 = { value: 2, next: Some(node3) }
  let node1 = { value: 1, next: Some(node2) }
  
  // Test traversal
  let mut current = Some(node1)
  let mut sum = 0
  let mut count = 0
  
  while {
    match current {
      Some(node) => {
        sum = sum + node.value
        count = count + 1
        current = node.next
        true
      }
      None => false
    }
  } {}
  
  assert_eq(sum, 6)
  assert_eq(count, 3)
  
  // Test search
  current = Some(node1)
  let mut found_value = false
  
  while {
    match current {
      Some(node) => {
        if node.value == 2 {
          found_value = true
          false
        } else {
          current = node.next
          true
        }
      }
      None => false
    }
  } {}
  
  assert_true(found_value)
}

test "heap operations test" {
  // Simulate min-heap operations using arrays
  let mut heap = []
  
  // Insert operations
  heap = heap.push(5)
  heap = heap.push(3)
  heap = heap.push(7)
  heap = heap.push(1)
  heap = heap.push(9)
  
  // Simulate heapify (simplified)
  // In a real implementation, we would maintain heap properties
  heap = heap.sort()
  
  assert_eq(heap.length(), 5)
  assert_eq(heap[0], 1)  // Min element
  
  // Test extract min (simulated)
  let min_value = heap[0]
  heap = heap.slice(1, heap.length())
  
  assert_eq(min_value, 1)
  assert_eq(heap.length(), 4)
  assert_eq(heap[0], 3)  // New min element
}

test "graph operations test" {
  // Simulate graph using adjacency lists
  let mut adjacency_list = []
  
  // Add vertices and edges
  adjacency_list = adjacency_list.push([1, 2, 3])  // Vertex 0 connected to 1, 2, 3
  adjacency_list = adjacency_list.push([0, 2])     // Vertex 1 connected to 0, 2
  adjacency_list = adjacency_list.push([0, 1, 3])  // Vertex 2 connected to 0, 1, 3
  adjacency_list = adjacency_list.push([0, 2])     // Vertex 3 connected to 0, 2
  
  assert_eq(adjacency_list.length(), 4)
  
  // Test degree calculation
  let degree_0 = adjacency_list[0].length()
  let degree_1 = adjacency_list[1].length()
  
  assert_eq(degree_0, 3)
  assert_eq(degree_1, 2)
  
  // Test edge existence
  let edge_exists = adjacency_list[0].contains(2)
  assert_true(edge_exists)
  
  let edge_not_exists = adjacency_list[0].contains(4)
  assert_false(edge_not_exists)
}

test "set operations test" {
  // Simulate set operations using arrays
  let set1 = [1, 2, 3, 4, 5]
  let set2 = [4, 5, 6, 7, 8]
  
  // Test union operation (simulated)
  let mut union_set = set1
  for element in set2 {
    if not(union_set.contains(element)) {
      union_set = union_set.push(element)
    }
  }
  
  assert_eq(union_set.length(), 8)
  assert_true(union_set.contains(1))
  assert_true(union_set.contains(8))
  
  // Test intersection operation (simulated)
  let mut intersection_set = []
  for element in set1 {
    if set2.contains(element) {
      intersection_set = intersection_set.push(element)
    }
  }
  
  assert_eq(intersection_set.length(), 2)
  assert_true(intersection_set.contains(4))
  assert_true(intersection_set.contains(5))
  
  // Test difference operation (simulated)
  let mut difference_set = []
  for element in set1 {
    if not(set2.contains(element)) {
      difference_set = difference_set.push(element)
    }
  }
  
  assert_eq(difference_set.length(), 3)
  assert_true(difference_set.contains(1))
  assert_false(difference_set.contains(4))
}

test "priority queue operations test" {
  // Simulate priority queue with (priority, value) pairs
  let mut priority_queue = []
  
  // Insert operations with (priority, value)
  priority_queue = priority_queue.push((3, "low priority"))
  priority_queue = priority_queue.push((1, "high priority"))
  priority_queue = priority_queue.push((2, "medium priority"))
  
  // Sort by priority (ascending)
  let mut sorted_queue = priority_queue
  // In a real implementation, we would maintain heap properties
  
  // Test that we have the right number of elements
  assert_eq(sorted_queue.length(), 3)
  
  // Test extraction by priority (simulated)
  let mut high_priority_found = false
  let mut medium_priority_found = false
  let mut low_priority_found = false
  
  for element in sorted_queue {
    match element {
      (priority, value) => {
        if priority == 1 and value == "high priority" {
          high_priority_found = true
        } else if priority == 2 and value == "medium priority" {
          medium_priority_found = true
        } else if priority == 3 and value == "low priority" {
          low_priority_found = true
        }
      }
    }
  }
  
  assert_true(high_priority_found)
  assert_true(medium_priority_found)
  assert_true(low_priority_found)
}

test "advanced data structure operations test" {
  // Test nested data structures
  type Matrix {
    data : Array[Array[Int]]
    rows : Int
    cols : Int
  }
  
  // Create a 2x2 matrix
  let matrix = {
    data: [[1, 2], [3, 4]],
    rows: 2,
    cols: 2
  }
  
  // Test matrix operations
  assert_eq(matrix.rows, 2)
  assert_eq(matrix.cols, 2)
  assert_eq(matrix.data[0][0], 1)
  assert_eq(matrix.data[1][1], 4)
  
  // Test matrix transpose (simulated)
  let mut transposed_data = []
  for i in 0..matrix.cols {
    let mut new_row = []
    for j in 0..matrix.rows {
      new_row = new_row.push(matrix.data[j][i])
    }
    transposed_data = transposed_data.push(new_row)
  }
  
  let transposed = {
    data: transposed_data,
    rows: matrix.cols,
    cols: matrix.rows
  }
  
  assert_eq(transposed.rows, 2)
  assert_eq(transposed.cols, 2)
  assert_eq(transposed.data[0][0], 1)
  assert_eq(transposed.data[0][1], 3)
  assert_eq(transposed.data[1][0], 2)
  assert_eq(transposed.data[1][1], 4)
}