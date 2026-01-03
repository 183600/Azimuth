// Enhanced Data Structures Tests for Azimuth Telemetry System
// This file contains comprehensive test cases for data structure operations

// Test 1: Stack Operations
test "stack operations" {
  let mut stack = []
  
  // Test push operations
  stack.push(1)
  stack.push(2)
  stack.push(3)
  assert_eq(stack.length(), 3)
  
  // Test pop operations
  let popped = stack.pop()
  match popped {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  assert_eq(stack.length(), 2)
  
  // Test peek operations
  let peeked = stack.peek()
  match peeked {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  assert_eq(stack.length(), 2) // Peek should not remove element
  
  // Test empty stack
  let mut empty_stack = []
  let empty_pop = empty_stack.pop()
  match empty_pop {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let empty_peek = empty_stack.peek()
  match empty_peek {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 2: Queue Operations
test "queue operations" {
  let mut queue = []
  
  // Test enqueue operations
  queue.enqueue(1)
  queue.enqueue(2)
  queue.enqueue(3)
  assert_eq(queue.length(), 3)
  
  // Test dequeue operations
  let dequeued = queue.dequeue()
  match dequeued {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  assert_eq(queue.length(), 2)
  
  // Test front operations
  let front = queue.front()
  match front {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  assert_eq(queue.length(), 2) // Front should not remove element
  
  // Test empty queue
  let mut empty_queue = []
  let empty_dequeue = empty_queue.dequeue()
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: HashMap Operations
test "hashmap operations" {
  let mut map = Map::new()
  
  // Test insert operations
  map.insert("key1", "value1")
  map.insert("key2", "value2")
  map.insert("key3", "value3")
  
  // Test get operations
  let value1 = map.get("key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test non-existent key
  let non_existent = map.get("non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test update operations
  map.insert("key1", "updated_value1")
  let updated_value = map.get("key1")
  match updated_value {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  // Test remove operations
  let removed_value = map.remove("key2")
  match removed_value {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  let after_removal = map.get("key2")
  match after_removal {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test contains operations
  assert_true(map.contains_key("key1"))
  assert_false(map.contains_key("key2"))
  
  // Test size operations
  assert_eq(map.size(), 2)
  
  // Test clear operations
  map.clear()
  assert_eq(map.size(), 0)
  assert_false(map.contains_key("key1"))
}

// Test 4: HashSet Operations
test "hashset operations" {
  let mut set = Set::new()
  
  // Test add operations
  set.add(1)
  set.add(2)
  set.add(3)
  set.add(1) // Duplicate should not increase size
  
  // Test contains operations
  assert_true(set.contains(1))
  assert_true(set.contains(2))
  assert_true(set.contains(3))
  assert_false(set.contains(4))
  
  // Test size operations
  assert_eq(set.size(), 3)
  
  // Test remove operations
  assert_true(set.remove(2))
  assert_false(set.contains(2))
  assert_eq(set.size(), 2)
  
  // Test removing non-existent element
  assert_false(set.remove(5))
  assert_eq(set.size(), 2)
  
  // Test clear operations
  set.clear()
  assert_eq(set.size(), 0)
  assert_false(set.contains(1))
}

// Test 5: LinkedList Operations
test "linkedlist operations" {
  let mut list = LinkedList::new()
  
  // Test add operations
  list.add_first(1)
  list.add_last(2)
  list.add_first(3)
  list.add_last(4)
  
  // Test size operations
  assert_eq(list.size(), 4)
  
  // Test get first and last
  let first = list.get_first()
  match first {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  let last = list.get_last()
  match last {
    Some(value) => assert_eq(value, 4)
    None => assert_true(false)
  }
  
  // Test remove operations
  let removed_first = list.remove_first()
  match removed_first {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  assert_eq(list.size(), 3)
  
  let removed_last = list.remove_last()
  match removed_last {
    Some(value) => assert_eq(value, 4)
    None => assert_true(false)
  }
  assert_eq(list.size(), 2)
  
  // Test empty list
  let mut empty_list = LinkedList::new()
  let empty_first = empty_list.get_first()
  match empty_first {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let empty_last = empty_list.get_last()
  match empty_last {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 6: Tree Operations
test "tree operations" {
  let mut tree = Tree::new(1)
  
  // Test insert operations
  tree.insert(2)
  tree.insert(3)
  tree.insert(0)
  tree.insert(4)
  
  // Test search operations
  assert_true(tree.search(1))
  assert_true(tree.search(2))
  assert_true(tree.search(3))
  assert_true(tree.search(0))
  assert_true(tree.search(4))
  assert_false(tree.search(5))
  
  // Test size operations
  assert_eq(tree.size(), 5)
  
  // Test min and max operations
  let min = tree.min()
  match min {
    Some(value) => assert_eq(value, 0)
    None => assert_true(false)
  }
  
  let max = tree.max()
  match max {
    Some(value) => assert_eq(value, 4)
    None => assert_true(false)
  }
  
  // Test remove operations
  assert_true(tree.remove(2))
  assert_false(tree.search(2))
  assert_eq(tree.size(), 4)
  
  // Test removing non-existent element
  assert_false(tree.remove(10))
  assert_eq(tree.size(), 4)
}

// Test 7: Priority Queue Operations
test "priority queue operations" {
  let mut pq = PriorityQueue::new()
  
  // Test enqueue operations (higher priority first)
  pq.enqueue("high", 10)
  pq.enqueue("medium", 5)
  pq.enqueue("low", 1)
  pq.enqueue("urgent", 15)
  
  // Test size operations
  assert_eq(pq.size(), 4)
  
  // Test dequeue operations (should return highest priority first)
  let urgent = pq.dequeue()
  match urgent {
    Some((value, priority)) => {
      assert_eq(value, "urgent")
      assert_eq(priority, 15)
    }
    None => assert_true(false)
  }
  
  let high = pq.dequeue()
  match high {
    Some((value, priority)) => {
      assert_eq(value, "high")
      assert_eq(priority, 10)
    }
    None => assert_true(false)
  }
  
  // Test peek operations
  let medium = pq.peek()
  match medium {
    Some((value, priority)) => {
      assert_eq(value, "medium")
      assert_eq(priority, 5)
    }
    None => assert_true(false)
  }
  
  // Test empty priority queue
  let mut empty_pq = PriorityQueue::new()
  let empty_dequeue = empty_pq.dequeue()
  match empty_dequeue {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 8: Vector Operations
test "vector operations" {
  let mut vec = Vector::new()
  
  // Test push operations
  vec.push(1)
  vec.push(2)
  vec.push(3)
  assert_eq(vec.length(), 3)
  
  // Test access operations
  assert_eq(vec[0], 1)
  assert_eq(vec[1], 2)
  assert_eq(vec[2], 3)
  
  // Test insert operations
  vec.insert(1, 10)
  assert_eq(vec.length(), 4)
  assert_eq(vec[0], 1)
  assert_eq(vec[1], 10)
  assert_eq(vec[2], 2)
  assert_eq(vec[3], 3)
  
  // Test remove operations
  let removed = vec.remove(1)
  assert_eq(removed, 10)
  assert_eq(vec.length(), 3)
  assert_eq(vec[0], 1)
  assert_eq(vec[1], 2)
  assert_eq(vec[2], 3)
  
  // Test slice operations
  let slice = vec.slice(1, 3)
  assert_eq(slice.length(), 2)
  assert_eq(slice[0], 2)
  assert_eq(slice[1], 3)
  
  // Test clear operations
  vec.clear()
  assert_eq(vec.length(), 0)
}

// Test 9: Tuple Operations
test "tuple operations" {
  // Test tuple creation and access
  let tuple1 = (1, "hello", true)
  assert_eq(tuple1.0, 1)
  assert_eq(tuple1.1, "hello")
  assert_true(tuple1.2)
  
  // Test nested tuples
  let nested_tuple = ((1, 2), (3, 4))
  assert_eq(nested_tuple.0.0, 1)
  assert_eq(nested_tuple.0.1, 2)
  assert_eq(nested_tuple.1.0, 3)
  assert_eq(nested_tuple.1.1, 4)
  
  // Test tuple comparison
  let tuple2 = (1, "hello", true)
  let tuple3 = (1, "world", true)
  
  assert_eq(tuple1.0, tuple2.0)
  assert_eq(tuple1.1, tuple2.1)
  assert_eq(tuple1.2, tuple2.2)
  
  assert_eq(tuple1.0, tuple3.0)
  assert_ne(tuple1.1, tuple3.1)
  assert_eq(tuple1.2, tuple3.2)
}

// Test 10: Enum Operations
test "enum operations" {
  // Test enum creation and matching
  let option1 = Some(42)
  let option2 = None
  
  match option1 {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  match option2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test nested enum matching
  let nested_option = Some(Some(42))
  match nested_option {
    Some(Some(inner_value)) => assert_eq(inner_value, 42)
    Some(None) => assert_true(false)
    None => assert_true(false)
  }
  
  // Test enum with different types
  let result1 = Ok(42)
  let result2 = Err("error message")
  
  match result1 {
    Ok(value) => assert_eq(value, 42)
    Err(_) => assert_true(false)
  }
  
  match result2 {
    Ok(_) => assert_true(false)
    Err(message) => assert_eq(message, "error message")
  }
}