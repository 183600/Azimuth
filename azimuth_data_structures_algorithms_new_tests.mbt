// Azimuth Telemetry System - Data Structures and Algorithms Tests
// This file contains test cases for data structures and algorithms operations

// Test 1: Stack Operations
test "stack operations" {
  let mut stack = []
  
  // Test empty stack
  assert_eq(stack.length(), 0)
  
  // Test push operations
  stack.push(1)
  stack.push(2)
  stack.push(3)
  assert_eq(stack.length(), 3)
  
  // Test pop operations
  let top = stack.pop()
  match top {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  assert_eq(stack.length(), 2)
  
  // Test peek operation
  let peek_value = stack[stack.length() - 1]
  assert_eq(peek_value, 2)
  
  // Test empty stack after pops
  stack.pop()
  stack.pop()
  assert_eq(stack.length(), 0)
  let empty_pop = stack.pop()
  match empty_pop {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Queue Operations
test "queue operations" {
  let mut queue = []
  
  // Test empty queue
  assert_eq(queue.length(), 0)
  
  // Test enqueue operations
  queue.push(1)
  queue.push(2)
  queue.push(3)
  assert_eq(queue.length(), 3)
  
  // Test dequeue operations (FIFO)
  let front = queue.shift()
  match front {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  assert_eq(queue.length(), 2)
  
  // Test peek front operation
  let front_value = queue[0]
  assert_eq(front_value, 2)
  
  // Test empty queue after dequeues
  queue.shift()
  queue.shift()
  assert_eq(queue.length(), 0)
  let empty_dequeue = queue.shift()
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: Binary Search Algorithm
test "binary search algorithm" {
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  // Test binary search for existing elements
  assert_eq(binary_search(sorted_array, 7), 3)
  assert_eq(binary_search(sorted_array, 1), 0)
  assert_eq(binary_search(sorted_array, 19), 9)
  assert_eq(binary_search(sorted_array, 13), 6)
  
  // Test binary search for non-existing elements
  assert_eq(binary_search(sorted_array, 0), -1)
  assert_eq(binary_search(sorted_array, 20), -1)
  assert_eq(binary_search(sorted_array, 8), -1)
  
  // Test binary search on empty array
  let empty_array = []
  assert_eq(binary_search(empty_array, 5), -1)
  
  // Test binary search on single element array
  let single_array = [5]
  assert_eq(binary_search(single_array, 5), 0)
  assert_eq(binary_search(single_array, 3), -1)
}

// Binary search helper function
fn binary_search(arr: Array[Int], target: Int) -> Int {
  let mut left = 0
  let mut right = arr.length() - 1
  
  while left <= right {
    let mid = left + (right - left) / 2
    let mid_value = arr[mid]
    
    if mid_value == target {
      return mid
    } else if mid_value < target {
      left = mid + 1
    } else {
      right = mid - 1
    }
  }
  
  return -1
}

// Test 4: Quick Sort Algorithm
test "quick sort algorithm" {
  let unsorted_array = [5, 2, 9, 1, 5, 6]
  let sorted_array = quick_sort(unsorted_array)
  let expected = [1, 2, 5, 5, 6, 9]
  
  assert_eq(sorted_array.length(), expected.length())
  for i in 0..sorted_array.length() {
    assert_eq(sorted_array[i], expected[i])
  }
  
  // Test with already sorted array
  let already_sorted = [1, 2, 3, 4, 5]
  let result = quick_sort(already_sorted)
  for i in 0..result.length() {
    assert_eq(result[i], already_sorted[i])
  }
  
  // Test with reverse sorted array
  let reverse_sorted = [5, 4, 3, 2, 1]
  let reverse_result = quick_sort(reverse_sorted)
  let reverse_expected = [1, 2, 3, 4, 5]
  for i in 0..reverse_result.length() {
    assert_eq(reverse_result[i], reverse_expected[i])
  }
  
  // Test with empty array
  let empty_array = []
  let empty_result = quick_sort(empty_array)
  assert_eq(empty_result.length(), 0)
  
  // Test with single element array
  let single_array = [42]
  let single_result = quick_sort(single_array)
  assert_eq(single_result.length(), 1)
  assert_eq(single_result[0], 42)
}

// Quick sort helper function
fn quick_sort(arr: Array[Int]) -> Array[Int] {
  if arr.length() <= 1 {
    return arr
  }
  
  let pivot = arr[0]
  let mut left = []
  let mut right = []
  
  for i in 1..arr.length() {
    if arr[i] <= pivot {
      left.push(arr[i])
    } else {
      right.push(arr[i])
    }
  }
  
  let sorted_left = quick_sort(left)
  let sorted_right = quick_sort(right)
  
  let result = []
  for i in 0..sorted_left.length() {
    result.push(sorted_left[i])
  }
  result.push(pivot)
  for i in 0..sorted_right.length() {
    result.push(sorted_right[i])
  }
  
  return result
}

// Test 5: Hash Map Operations
test "hash map operations" {
  let mut map = Map::new()
  
  // Test empty map
  assert_eq(map.size(), 0)
  
  // Test insert operations
  map.insert("key1", "value1")
  map.insert("key2", "value2")
  map.insert("key3", "value3")
  assert_eq(map.size(), 3)
  
  // Test get operations
  let value1 = map.get("key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let value2 = map.get("key2")
  match value2 {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  // Test get non-existent key
  let non_existent = map.get("non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test update operation
  map.insert("key1", "new_value1")
  let updated_value = map.get("key1")
  match updated_value {
    Some(v) => assert_eq(v, "new_value1")
    None => assert_true(false)
  }
  
  // Test remove operation
  map.remove("key2")
  assert_eq(map.size(), 2)
  let removed_value = map.get("key2")
  match removed_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test contains operation
  assert_true(map.contains_key("key1"))
  assert_true(map.contains_key("key3"))
  assert_false(map.contains_key("key2"))
  assert_false(map.contains_key("non_existent"))
}

// Test 6: Linked List Operations
test "linked list operations" {
  let mut list = LinkedList::new()
  
  // Test empty list
  assert_eq(list.length(), 0)
  assert_true(list.is_empty())
  
  // Test add to front
  list.add_front(1)
  list.add_front(2)
  list.add_front(3)
  assert_eq(list.length(), 3)
  
  // Test add to back
  list.add_back(4)
  list.add_back(5)
  assert_eq(list.length(), 5)
  
  // Test remove from front
  let front_value = list.remove_front()
  match front_value {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  assert_eq(list.length(), 4)
  
  // Test remove from back
  let back_value = list.remove_back()
  match back_value {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  assert_eq(list.length(), 3)
  
  // Test get operations
  let first_value = list.get(0)
  match first_value {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  let last_value = list.get(list.length() - 1)
  match last_value {
    Some(value) => assert_eq(value, 4)
    None => assert_true(false)
  }
  
  // Test empty list after removals
  list.remove_front()
  list.remove_front()
  list.remove_front()
  assert_eq(list.length(), 0)
  assert_true(list.is_empty())
  
  let empty_front = list.remove_front()
  match empty_front {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let empty_back = list.remove_back()
  match empty_back {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Tree Operations
test "binary tree operations" {
  let mut tree = BinaryTree::new()
  
  // Test empty tree
  assert_true(tree.is_empty())
  assert_eq(tree.size(), 0)
  
  // Test insert operations
  tree.insert(5)
  tree.insert(3)
  tree.insert(7)
  tree.insert(2)
  tree.insert(4)
  tree.insert(6)
  tree.insert(8)
  
  assert_false(tree.is_empty())
  assert_eq(tree.size(), 7)
  
  // Test search operations
  assert_true(tree.search(5))
  assert_true(tree.search(3))
  assert_true(tree.search(7))
  assert_true(tree.search(2))
  assert_true(tree.search(4))
  assert_true(tree.search(6))
  assert_true(tree.search(8))
  
  assert_false(tree.search(1))
  assert_false(tree.search(9))
  assert_false(tree.search(10))
  
  // Test in-order traversal
  let inorder = tree.inorder_traversal()
  let expected = [2, 3, 4, 5, 6, 7, 8]
  assert_eq(inorder.length(), expected.length())
  for i in 0..inorder.length() {
    assert_eq(inorder[i], expected[i])
  }
  
  // Test pre-order traversal
  let preorder = tree.preorder_traversal()
  let preorder_expected = [5, 3, 2, 4, 7, 6, 8]
  assert_eq(preorder.length(), preorder_expected.length())
  for i in 0..preorder.length() {
    assert_eq(preorder[i], preorder_expected[i])
  }
  
  // Test post-order traversal
  let postorder = tree.postorder_traversal()
  let postorder_expected = [2, 4, 3, 6, 8, 7, 5]
  assert_eq(postorder.length(), postorder_expected.length())
  for i in 0..postorder.length() {
    assert_eq(postorder[i], postorder_expected[i])
  }
  
  // Test remove operations
  tree.remove(3)  // Node with two children
  assert_eq(tree.size(), 6)
  assert_false(tree.search(3))
  
  tree.remove(2)  // Leaf node
  assert_eq(tree.size(), 5)
  assert_false(tree.search(2))
  
  tree.remove(5)  // Root node with one child
  assert_eq(tree.size(), 4)
  assert_false(tree.search(5))
}

// Test 8: Graph Operations
test "graph operations" {
  let mut graph = Graph::new()
  
  // Test empty graph
  assert_eq(graph.vertex_count(), 0)
  assert_eq(graph.edge_count(), 0)
  
  // Test add vertex operations
  graph.add_vertex("A")
  graph.add_vertex("B")
  graph.add_vertex("C")
  graph.add_vertex("D")
  assert_eq(graph.vertex_count(), 4)
  
  // Test add edge operations
  graph.add_edge("A", "B")
  graph.add_edge("A", "C")
  graph.add_edge("B", "D")
  graph.add_edge("C", "D")
  assert_eq(graph.edge_count(), 4)
  
  // Test has vertex operations
  assert_true(graph.has_vertex("A"))
  assert_true(graph.has_vertex("B"))
  assert_true(graph.has_vertex("C"))
  assert_true(graph.has_vertex("D"))
  assert_false(graph.has_vertex("E"))
  
  // Test has edge operations
  assert_true(graph.has_edge("A", "B"))
  assert_true(graph.has_edge("A", "C"))
  assert_true(graph.has_edge("B", "D"))
  assert_true(graph.has_edge("C", "D"))
  assert_false(graph.has_edge("B", "C"))
  assert_false(graph.has_edge("D", "A"))
  
  // Test get neighbors
  let neighbors_a = graph.get_neighbors("A")
  assert_eq(neighbors_a.length(), 2)
  assert_true(contains(neighbors_a, "B"))
  assert_true(contains(neighbors_a, "C"))
  
  let neighbors_b = graph.get_neighbors("B")
  assert_eq(neighbors_b.length(), 1)
  assert_true(contains(neighbors_b, "D"))
  
  // Test remove edge operations
  graph.remove_edge("A", "C")
  assert_eq(graph.edge_count(), 3)
  assert_false(graph.has_edge("A", "C"))
  
  // Test remove vertex operations
  graph.remove_vertex("D")
  assert_eq(graph.vertex_count(), 3)
  assert_eq(graph.edge_count(), 1)
  assert_false(graph.has_vertex("D"))
  assert_false(graph.has_edge("B", "D"))
  assert_false(graph.has_edge("C", "D"))
}

// Helper function to check if array contains a value
fn contains(arr: Array[String], value: String) -> Bool {
  for item in arr {
    if item == value {
      return true
    }
  }
  return false
}

// Test 9: Heap Operations
test "heap operations" {
  let mut heap = MaxHeap::new()
  
  // Test empty heap
  assert_true(heap.is_empty())
  assert_eq(heap.size(), 0)
  
  // Test insert operations
  heap.insert(5)
  heap.insert(3)
  heap.insert(7)
  heap.insert(2)
  heap.insert(4)
  heap.insert(6)
  heap.insert(8)
  
  assert_false(heap.is_empty())
  assert_eq(heap.size(), 7)
  
  // Test peek operations
  let max_value = heap.peek()
  match max_value {
    Some(value) => assert_eq(value, 8)
    None => assert_true(false)
  }
  
  // Test extract max operations
  let max1 = heap.extract_max()
  match max1 {
    Some(value) => assert_eq(value, 8)
    None => assert_true(false)
  }
  assert_eq(heap.size(), 6)
  
  let max2 = heap.extract_max()
  match max2 {
    Some(value) => assert_eq(value, 7)
    None => assert_true(false)
  }
  assert_eq(heap.size(), 5)
  
  let max3 = heap.extract_max()
  match max3 {
    Some(value) => assert_eq(value, 6)
    None => assert_true(false)
  }
  assert_eq(heap.size(), 4)
  
  // Test heap property after extractions
  let current_max = heap.peek()
  match current_max {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  
  // Test empty heap after all extractions
  heap.extract_max()
  heap.extract_max()
  heap.extract_max()
  heap.extract_max()
  assert_eq(heap.size(), 0)
  assert_true(heap.is_empty())
  
  let empty_extract = heap.extract_max()
  match empty_extract {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let empty_peek = heap.peek()
  match empty_peek {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 10: Trie Operations
test "trie operations" {
  let mut trie = Trie::new()
  
  // Test empty trie
  assert_true(trie.is_empty())
  assert_eq(trie.size(), 0)
  
  // Test insert operations
  trie.insert("apple")
  trie.insert("app")
  trie.insert("application")
  trie.insert("banana")
  trie.insert("band")
  trie.insert("bandana")
  
  assert_false(trie.is_empty())
  assert_eq(trie.size(), 6)
  
  // Test search operations
  assert_true(trie.search("apple"))
  assert_true(trie.search("app"))
  assert_true(trie.search("application"))
  assert_true(trie.search("banana"))
  assert_true(trie.search("band"))
  assert_true(trie.search("bandana"))
  
  assert_false(trie.search("appl"))
  assert_false(trie.search("applications"))
  assert_false(trie.search("ban"))
  assert_false(trie.search("bandage"))
  
  // Test starts with operations
  assert_true(trie.starts_with("app"))
  assert_true(trie.starts_with("appl"))
  assert_true(trie.starts_with("ban"))
  assert_true(trie.starts_with("band"))
  
  assert_false(trie.starts_with("cat"))
  assert_false(trie.starts_with("dog"))
  
  // Test remove operations
  trie.remove("app")  // Remove a word that is a prefix of another
  assert_eq(trie.size(), 5)
  assert_false(trie.search("app"))
  assert_true(trie.search("apple"))
  assert_true(trie.search("application"))
  
  trie.remove("bandana")  // Remove a word with no children
  assert_eq(trie.size(), 4)
  assert_false(trie.search("bandana"))
  assert_true(trie.search("band"))
  
  trie.remove("application")  // Remove a word with children
  assert_eq(trie.size(), 3)
  assert_false(trie.search("application"))
  assert_true(trie.search("apple"))
}