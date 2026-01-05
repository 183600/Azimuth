// Data Structures Test Cases for Azimuth Telemetry System
// This file contains comprehensive tests for various data structures

test "stack operations" {
  let mut stack = []
  
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
  
  // Test empty stack
  while let Some(_) = stack.pop() {}
  let empty_pop = stack.pop()
  match empty_pop {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "queue operations" {
  let mut queue = []
  
  // Test enqueue operations
  queue.push_back(1)
  queue.push_back(2)
  queue.push_back(3)
  assert_eq(queue.length(), 3)
  
  // Test dequeue operations
  let front = queue.pop_front()
  match front {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  assert_eq(queue.length(), 2)
  
  // Test empty queue
  while let Some(_) = queue.pop_front() {}
  let empty_dequeue = queue.pop_front()
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "hash map operations" {
  let mut map = Map::new()
  
  // Test insert operations
  map.insert("key1", "value1")
  map.insert("key2", "value2")
  map.insert("key3", "value3")
  
  // Test get operations
  match map.get("key1") {
    Some(value) => assert_eq(value, "value1")
    None => assert_true(false)
  }
  
  match map.get("nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test remove operations
  let removed = map.remove("key2")
  match removed {
    Some(value) => assert_eq(value, "value2")
    None => assert_true(false)
  }
  
  match map.get("key2") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

test "binary tree operations" {
  // Simple binary tree node structure test
  let leaf1 = TreeNode { value: 1, left: None, right: None }
  let leaf2 = TreeNode { value: 2, left: None, right: None }
  let root = TreeNode { value: 3, left: Some(leaf1), right: Some(leaf2) }
  
  // Test tree traversal
  let mut values = []
  
  // In-order traversal
  fn inorder_traverse(node : TreeNode, values : Array[Int]) {
    match node.left {
      Some(left_node) => inorder_traverse(left_node, values)
      None => ()
    }
    values.push(node.value)
    match node.right {
      Some(right_node) => inorder_traverse(right_node, values)
      None => ()
    }
  }
  
  inorder_traverse(root, values)
  assert_eq(values.length(), 3)
  assert_eq(values[0], 1)
  assert_eq(values[1], 3)
  assert_eq(values[2], 2)
}

test "linked list operations" {
  let mut list = LinkedList::new()
  
  // Test insert at beginning
  list.push_front(1)
  list.push_front(2)
  list.push_front(3)
  
  // Test insert at end
  list.push_back(4)
  list.push_back(5)
  
  // Test length
  assert_eq(list.length(), 5)
  
  // Test get operations
  assert_eq(list.get(0), Some(3))
  assert_eq(list.get(2), Some(1))
  assert_eq(list.get(4), Some(5))
  assert_eq(list.get(5), None)
  
  // Test remove operations
  let removed_front = list.pop_front()
  match removed_front {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  let removed_back = list.pop_back()
  match removed_back {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  
  assert_eq(list.length(), 3)
}

test "heap operations" {
  let mut heap = MinHeap::new()
  
  // Test insert operations
  heap.insert(5)
  heap.insert(2)
  heap.insert(8)
  heap.insert(1)
  heap.insert(7)
  
  // Test extract min operations
  let min1 = heap.extract_min()
  match min1 {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  let min2 = heap.extract_min()
  match min2 {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  let min3 = heap.extract_min()
  match min3 {
    Some(value) => assert_eq(value, 5)
    None => assert_true(false)
  }
  
  assert_eq(heap.length(), 2)
}

test "set operations" {
  let mut set = Set::new()
  
  // Test insert operations
  set.insert(1)
  set.insert(2)
  set.insert(3)
  set.insert(2) // Duplicate
  
  // Test contains operations
  assert_true(set.contains(1))
  assert_true(set.contains(2))
  assert_true(set.contains(3))
  assert_false(set.contains(4))
  
  // Test length
  assert_eq(set.length(), 3) // Duplicates are not counted
  
  // Test remove operations
  set.remove(2)
  assert_false(set.contains(2))
  assert_eq(set.length(), 2)
}

test "graph operations" {
  let mut graph = Graph::new()
  
  // Test add vertices
  let v1 = graph.add_vertex("A")
  let v2 = graph.add_vertex("B")
  let v3 = graph.add_vertex("C")
  
  // Test add edges
  graph.add_edge(v1, v2, 5)
  graph.add_edge(v2, v3, 3)
  graph.add_edge(v1, v3, 10)
  
  // Test get neighbors
  let neighbors_v1 = graph.get_neighbors(v1)
  assert_eq(neighbors_v1.length(), 2)
  
  // Test get edge weight
  let weight_v1_v2 = graph.get_edge_weight(v1, v2)
  match weight_v1_v2 {
    Some(w) => assert_eq(w, 5)
    None => assert_true(false)
  }
  
  let weight_v2_v1 = graph.get_edge_weight(v2, v1)
  match weight_v2_v1 {
    Some(_) => assert_true(false) // Directed graph
    None => assert_true(true)
  }
}

test "trie operations" {
  let mut trie = Trie::new()
  
  // Test insert operations
  trie.insert("hello")
  trie.insert("world")
  trie.insert("help")
  trie.insert("helicopter")
  
  // Test search operations
  assert_true(trie.search("hello"))
  assert_true(trie.search("world"))
  assert_true(trie.search("help"))
  assert_true(trie.search("helicopter"))
  assert_false(trie.search("hell"))
  assert_false(trie.search("helping"))
  
  // Test prefix operations
  assert_true(trie.starts_with("hel"))
  assert_true(trie.starts_with("wor"))
  assert_false(trie.starts_with("xyz"))
  
  // Test count operations
  assert_eq(trie.count_words_with_prefix("hel"), 3)
  assert_eq(trie.count_words_with_prefix("wor"), 1)
  assert_eq(trie.count_words_with_prefix("xyz"), 0)
}