// Azimuth Telemetry System - Advanced Data Structures and Algorithms Tests
// This file contains comprehensive test cases for advanced data structures and algorithms

// Test 1: Tree Structure Operations
test "tree structure operations" {
  // Test binary tree node creation and operations
  let leaf1 = TreeNode::new(1)
  let leaf2 = TreeNode::new(2)
  let leaf3 = TreeNode::new(3)
  let leaf4 = TreeNode::new(4)
  
  // Build tree structure
  let internal1 = TreeNode::with_children(5, [leaf1, leaf2])
  let internal2 = TreeNode::with_children(6, [leaf3, leaf4])
  let root = TreeNode::with_children(7, [internal1, internal2])
  
  // Test tree traversal
  let pre_order_result = TreeNode::pre_order_traversal(root)
  assert_eq(pre_order_result.length(), 7)
  assert_eq(pre_order_result[0], 7) // Root
  assert_eq(pre_order_result[1], 5) // First internal
  assert_eq(pre_order_result[2], 1) // First leaf
  
  let in_order_result = TreeNode::in_order_traversal(root)
  assert_eq(in_order_result.length(), 7)
  
  let post_order_result = TreeNode::post_order_traversal(root)
  assert_eq(post_order_result.length(), 7)
  assert_eq(post_order_result[6], 7) // Root last in post-order
  
  // Test tree properties
  assert_eq(TreeNode::depth(root), 0)
  assert_eq(TreeNode::depth(internal1), 1)
  assert_eq(TreeNode::depth(leaf1), 2)
  
  assert_eq(TreeNode::height(leaf1), 0)
  assert_eq(TreeNode::height(internal1), 1)
  assert_eq(TreeNode::height(root), 2)
  
  // Test tree search
  let found_node = TreeNode::search(root, 3)
  match found_node {
    Some(node) => assert_eq(TreeNode::value(node), 3)
    None => assert_true(false)
  }
  
  let not_found = TreeNode::search(root, 99)
  match not_found {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Graph Structure Operations
test "graph structure operations" {
  // Test graph creation and operations
  let graph = Graph::new()
  
  // Add vertices
  let v1 = Graph::add_vertex(graph, "A")
  let v2 = Graph::add_vertex(graph, "B")
  let v3 = Graph::add_vertex(graph, "C")
  let v4 = Graph::add_vertex(graph, "D")
  
  // Add edges
  Graph::add_edge(graph, v1, v2, 5)
  Graph::add_edge(graph, v1, v3, 3)
  Graph::add_edge(graph, v2, v4, 2)
  Graph::add_edge(graph, v3, v4, 1)
  
  // Test vertex count
  assert_eq(Graph::vertex_count(graph), 4)
  
  // Test edge count
  assert_eq(Graph::edge_count(graph), 4)
  
  // Test adjacency
  let v1_neighbors = Graph::neighbors(graph, v1)
  assert_eq(v1_neighbors.length(), 2)
  
  // Test graph traversal - BFS
  let bfs_result = Graph::bfs(graph, v1)
  assert_eq(bfs_result.length(), 4)
  assert_eq(bfs_result[0], "A") // Start vertex
  
  // Test graph traversal - DFS
  let dfs_result = Graph::dfs(graph, v1)
  assert_eq(dfs_result.length(), 4)
  
  // Test shortest path
  let shortest_path = Graph::shortest_path(graph, v1, v4)
  match shortest_path {
    Some(path) => {
      assert_eq(path.length(), 3) // A -> C -> D
      assert_eq(path[0], "A")
      assert_eq(path[1], "C")
      assert_eq(path[2], "D")
    }
    None => assert_true(false)
  }
  
  // Test cycle detection
  let has_cycle = Graph::has_cycle(graph)
  assert_false(has_cycle) // This is a DAG
  
  // Add a cycle
  Graph::add_edge(graph, v4, v1, 4)
  let has_cycle_after = Graph::has_cycle(graph)
  assert_true(has_cycle_after)
}

// Test 3: Hash Table Operations
test "hash table operations" {
  // Test hash table creation and operations
  let hash_table = HashTable::new()
  
  // Test insertion
  HashTable::insert(hash_table, "key1", "value1")
  HashTable::insert(hash_table, "key2", "value2")
  HashTable::insert(hash_table, "key3", "value3")
  
  // Test retrieval
  let value1 = HashTable::get(hash_table, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  let non_existent = HashTable::get(hash_table, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test update
  HashTable::insert(hash_table, "key1", "updated_value1")
  let updated_value = HashTable::get(hash_table, "key1")
  match updated_value {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  // Test deletion
  let deleted = HashTable::remove(hash_table, "key2")
  assert_true(deleted)
  
  let value_after_deletion = HashTable::get(hash_table, "key2")
  match value_after_deletion {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test size
  assert_eq(HashTable::size(hash_table), 2)
  
  // Test keys and values
  let keys = HashTable::keys(hash_table)
  assert_eq(keys.length(), 2)
  
  let values = HashTable::values(hash_table)
  assert_eq(values.length(), 2)
  
  // Test collision handling by inserting many items
  for i in 0..=100 {
    HashTable::insert(hash_table, "collision_key_" + i.to_string(), "value_" + i.to_string())
  }
  
  // Verify all items are accessible
  for i in 0..=100 {
    let value = HashTable::get(hash_table, "collision_key_" + i.to_string())
    match value {
      Some(v) => assert_eq(v, "value_" + i.to_string())
      None => assert_true(false)
    }
  }
}

// Test 4: Priority Queue Operations
test "priority queue operations" {
  // Test priority queue creation and operations
  let priority_queue = PriorityQueue::new()
  
  // Test insertion with different priorities
  PriorityQueue::enqueue(priority_queue, "low priority", 1)
  PriorityQueue::enqueue(priority_queue, "high priority", 10)
  PriorityQueue::enqueue(priority_queue, "medium priority", 5)
  PriorityQueue::enqueue(priority_queue, "highest priority", 15)
  
  // Test size
  assert_eq(PriorityQueue::size(priority_queue), 4)
  
  // Test peek (highest priority element)
  let peeked = PriorityQueue::peek(priority_queue)
  match peeked {
    Some((value, priority)) => {
      assert_eq(value, "highest priority")
      assert_eq(priority, 15)
    }
    None => assert_true(false)
  }
  
  // Test dequeue (should return elements in priority order)
  let dequeued1 = PriorityQueue::dequeue(priority_queue)
  match dequeued1 {
    Some((value, priority)) => {
      assert_eq(value, "highest priority")
      assert_eq(priority, 15)
    }
    None => assert_true(false)
  }
  
  let dequeued2 = PriorityQueue::dequeue(priority_queue)
  match dequeued2 {
    Some((value, priority)) => {
      assert_eq(value, "high priority")
      assert_eq(priority, 10)
    }
    None => assert_true(false)
  }
  
  let dequeued3 = PriorityQueue::dequeue(priority_queue)
  match dequeued3 {
    Some((value, priority)) => {
      assert_eq(value, "medium priority")
      assert_eq(priority, 5)
    }
    None => assert_true(false)
  }
  
  let dequeued4 = PriorityQueue::dequeue(priority_queue)
  match dequeued4 {
    Some((value, priority)) => {
      assert_eq(value, "low priority")
      assert_eq(priority, 1)
    }
    None => assert_true(false)
  }
  
  // Test empty queue
  assert_eq(PriorityQueue::size(priority_queue), 0)
  
  let empty_peek = PriorityQueue::peek(priority_queue)
  match empty_peek {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let empty_dequeue = PriorityQueue::dequeue(priority_queue)
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Stack and Queue Operations
test "stack and queue operations" {
  // Test stack operations
  let stack = Stack::new()
  
  // Test push
  Stack::push(stack, "first")
  Stack::push(stack, "second")
  Stack::push(stack, "third")
  
  // Test size
  assert_eq(Stack::size(stack), 3)
  
  // Test peek
  let stack_peek = Stack::peek(stack)
  match stack_peek {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  // Test pop (LIFO)
  let popped1 = Stack::pop(stack)
  match popped1 {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  let popped2 = Stack::pop(stack)
  match popped2 {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  let popped3 = Stack::pop(stack)
  match popped3 {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  // Test empty stack
  assert_eq(Stack::size(stack), 0)
  
  let empty_pop = Stack::pop(stack)
  match empty_pop {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test queue operations
  let queue = Queue::new()
  
  // Test enqueue
  Queue::enqueue(queue, "first")
  Queue::enqueue(queue, "second")
  Queue::enqueue(queue, "third")
  
  // Test size
  assert_eq(Queue::size(queue), 3)
  
  // Test peek
  let queue_peek = Queue::peek(queue)
  match queue_peek {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  // Test dequeue (FIFO)
  let dequeued1 = Queue::dequeue(queue)
  match dequeued1 {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  let dequeued2 = Queue::dequeue(queue)
  match dequeued2 {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  let dequeued3 = Queue::dequeue(queue)
  match dequeued3 {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  // Test empty queue
  assert_eq(Queue::size(queue), 0)
  
  let empty_dequeue = Queue::dequeue(queue)
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Sorting Algorithms
test "sorting algorithms" {
  // Test bubble sort
  let unsorted_array = [5, 2, 8, 1, 9, 3]
  let bubble_sorted = SortingAlgorithms::bubble_sort(unsorted_array)
  assert_eq(bubble_sorted, [1, 2, 3, 5, 8, 9])
  
  // Test quick sort
  let quick_sorted = SortingAlgorithms::quick_sort(unsorted_array)
  assert_eq(quick_sorted, [1, 2, 3, 5, 8, 9])
  
  // Test merge sort
  let merge_sorted = SortingAlgorithms::merge_sort(unsorted_array)
  assert_eq(merge_sorted, [1, 2, 3, 5, 8, 9])
  
  // Test with already sorted array
  let sorted_array = [1, 2, 3, 4, 5]
  let already_sorted = SortingAlgorithms::quick_sort(sorted_array)
  assert_eq(already_sorted, [1, 2, 3, 4, 5])
  
  // Test with reverse sorted array
  let reverse_sorted = [5, 4, 3, 2, 1]
  let reverse_fixed = SortingAlgorithms::quick_sort(reverse_sorted)
  assert_eq(reverse_fixed, [1, 2, 3, 4, 5])
  
  // Test with duplicate elements
  let with_duplicates = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  let duplicates_sorted = SortingAlgorithms::quick_sort(with_duplicates)
  assert_eq(duplicates_sorted, [1, 1, 2, 3, 4, 5, 5, 6, 9])
  
  // Test with single element
  let single_element = [42]
  let single_sorted = SortingAlgorithms::quick_sort(single_element)
  assert_eq(single_sorted, [42])
  
  // Test with empty array
  let empty_array = []
  let empty_sorted = SortingAlgorithms::quick_sort(empty_array)
  assert_eq(empty_sorted, [])
}

// Test 7: Search Algorithms
test "search algorithms" {
  // Test binary search on sorted array
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  // Test finding existing elements
  let found1 = SearchAlgorithms::binary_search(sorted_array, 7)
  assert_eq(found1, Some(3)) // Index 3
  
  let found2 = SearchAlgorithms::binary_search(sorted_array, 1)
  assert_eq(found2, Some(0)) // Index 0
  
  let found3 = SearchAlgorithms::binary_search(sorted_array, 19)
  assert_eq(found3, Some(9)) // Index 9
  
  // Test finding non-existing elements
  let not_found1 = SearchAlgorithms::binary_search(sorted_array, 8)
  assert_eq(not_found1, None)
  
  let not_found2 = SearchAlgorithms::binary_search(sorted_array, 0)
  assert_eq(not_found2, None)
  
  let not_found3 = SearchAlgorithms::binary_search(sorted_array, 20)
  assert_eq(not_found3, None)
  
  // Test linear search on unsorted array
  let unsorted_array = [5, 2, 8, 1, 9, 3]
  
  let linear_found1 = SearchAlgorithms::linear_search(unsorted_array, 8)
  assert_eq(linear_found1, Some(2)) // Index 2
  
  let linear_found2 = SearchAlgorithms::linear_search(unsorted_array, 5)
  assert_eq(linear_found2, Some(0)) // Index 0
  
  let linear_found3 = SearchAlgorithms::linear_search(unsorted_array, 3)
  assert_eq(linear_found3, Some(5)) // Index 5
  
  let linear_not_found = SearchAlgorithms::linear_search(unsorted_array, 7)
  assert_eq(linear_not_found, None)
  
  // Test with empty array
  let empty_array = []
  let empty_binary = SearchAlgorithms::binary_search(empty_array, 5)
  assert_eq(empty_binary, None)
  
  let empty_linear = SearchAlgorithms::linear_search(empty_array, 5)
  assert_eq(empty_linear, None)
  
  // Test with single element
  let single_array = [42]
  let single_binary_found = SearchAlgorithms::binary_search(single_array, 42)
  assert_eq(single_binary_found, Some(0))
  
  let single_binary_not_found = SearchAlgorithms::binary_search(single_array, 7)
  assert_eq(single_binary_not_found, None)
}

// Test 8: Set Operations
test "set operations" {
  // Test set creation and operations
  let set1 = Set::new()
  let set2 = Set::new()
  
  // Add elements to sets
  Set::add(set1, "a")
  Set::add(set1, "b")
  Set::add(set1, "c")
  
  Set::add(set2, "c")
  Set::add(set2, "d")
  Set::add(set2, "e")
  
  // Test membership
  assert_true(Set::contains(set1, "a"))
  assert_true(Set::contains(set1, "b"))
  assert_true(Set::contains(set1, "c"))
  assert_false(Set::contains(set1, "d"))
  
  // Test set size
  assert_eq(Set::size(set1), 3)
  assert_eq(Set::size(set2), 3)
  
  // Test union
  let union_set = Set::union(set1, set2)
  assert_eq(Set::size(union_set), 5)
  assert_true(Set::contains(union_set, "a"))
  assert_true(Set::contains(union_set, "b"))
  assert_true(Set::contains(union_set, "c"))
  assert_true(Set::contains(union_set, "d"))
  assert_true(Set::contains(union_set, "e"))
  
  // Test intersection
  let intersection_set = Set::intersection(set1, set2)
  assert_eq(Set::size(intersection_set), 1)
  assert_true(Set::contains(intersection_set, "c"))
  assert_false(Set::contains(intersection_set, "a"))
  assert_false(Set::contains(intersection_set, "d"))
  
  // Test difference
  let difference_set = Set::difference(set1, set2)
  assert_eq(Set::size(difference_set), 2)
  assert_true(Set::contains(difference_set, "a"))
  assert_true(Set::contains(difference_set, "b"))
  assert_false(Set::contains(difference_set, "c"))
  
  // Test subset
  let subset_set = Set::new()
  Set::add(subset_set, "a")
  Set::add(subset_set, "b")
  
  assert_true(Set::is_subset(subset_set, set1))
  assert_false(Set::is_subset(set1, subset_set))
  assert_true(Set::is_subset(set1, set1)) // Set is subset of itself
  
  // Test removal
  Set::remove(set1, "b")
  assert_eq(Set::size(set1), 2)
  assert_false(Set::contains(set1, "b"))
  
  // Test clearing
  Set::clear(set2)
  assert_eq(Set::size(set2), 0)
  assert_false(Set::contains(set2, "c"))
}