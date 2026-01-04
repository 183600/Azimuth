// Azimuth Telemetry System - Fundamental Data Structures Tests
// This file contains test cases for fundamental data structures used in the telemetry system

// Test 1: Stack Operations
test "stack operations" {
  let mut stack = Stack::new()
  
  // Test empty stack
  assert_true(Stack::is_empty(stack))
  assert_eq(Stack::size(stack), 0)
  
  // Test push operations
  Stack::push(stack, 1)
  Stack::push(stack, 2)
  Stack::push(stack, 3)
  
  assert_false(Stack::is_empty(stack))
  assert_eq(Stack::size(stack), 3)
  
  // Test pop operations
  let popped_value = Stack::pop(stack)
  match popped_value {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  assert_eq(Stack::size(stack), 2)
  
  // Test peek operation
  let peeked_value = Stack::peek(stack)
  match peeked_value {
    Some(value) => assert_eq(value, 2)
    None => assert_true(false)
  }
  
  // Test pop remaining elements
  assert_eq(Stack::pop(stack), Some(2))
  assert_eq(Stack::pop(stack), Some(1))
  assert_eq(Stack::pop(stack), None) // Stack should be empty now
  
  assert_true(Stack::is_empty(stack))
}

// Test 2: Queue Operations
test "queue operations" {
  let mut queue = Queue::new()
  
  // Test empty queue
  assert_true(Queue::is_empty(queue))
  assert_eq(Queue::size(queue), 0)
  
  // Test enqueue operations
  Queue::enqueue(queue, "first")
  Queue::enqueue(queue, "second")
  Queue::enqueue(queue, "third")
  
  assert_false(Queue::is_empty(queue))
  assert_eq(Queue::size(queue), 3)
  
  // Test dequeue operations
  let dequeued_value = Queue::dequeue(queue)
  match dequeued_value {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  assert_eq(Queue::size(queue), 2)
  
  // Test front operation
  let front_value = Queue::front(queue)
  match front_value {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  // Test dequeue remaining elements
  assert_eq(Queue::dequeue(queue), Some("second"))
  assert_eq(Queue::dequeue(queue), Some("third"))
  assert_eq(Queue::dequeue(queue), None) // Queue should be empty now
  
  assert_true(Queue::is_empty(queue))
}

// Test 3: HashMap Operations
test "hashmap operations" {
  let mut map = HashMap::new()
  
  // Test empty map
  assert_true(HashMap::is_empty(map))
  assert_eq(HashMap::size(map), 0)
  
  // Test insert operations
  HashMap::insert(map, "key1", 100)
  HashMap::insert(map, "key2", 200)
  HashMap::insert(map, "key3", 300)
  
  assert_false(HashMap::is_empty(map))
  assert_eq(HashMap::size(map), 3)
  
  // Test get operations
  let value1 = HashMap::get(map, "key1")
  match value1 {
    Some(v) => assert_eq(v, 100)
    None => assert_true(false)
  }
  
  let value2 = HashMap::get(map, "key2")
  match value2 {
    Some(v) => assert_eq(v, 200)
    None => assert_true(false)
  }
  
  // Test get non-existent key
  let non_existent = HashMap::get(map, "non_existent")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test contains key
  assert_true(HashMap::contains_key(map, "key1"))
  assert_false(HashMap::contains_key(map, "non_existent"))
  
  // Test update operation
  HashMap::insert(map, "key1", 150)
  let updated_value = HashMap::get(map, "key1")
  match updated_value {
    Some(v) => assert_eq(v, 150)
    None => assert_true(false)
  }
  
  // Test remove operation
  let removed_value = HashMap::remove(map, "key2")
  match removed_value {
    Some(v) => assert_eq(v, 200)
    None => assert_true(false)
  }
  
  assert_eq(HashMap::size(map), 2)
  assert_false(HashMap::contains_key(map, "key2"))
}

// Test 4: LinkedList Operations
test "linkedlist operations" {
  let mut list = LinkedList::new()
  
  // Test empty list
  assert_true(LinkedList::is_empty(list))
  assert_eq(LinkedList::size(list), 0)
  
  // Test add to front
  LinkedList::add_front(list, 10)
  LinkedList::add_front(list, 20)
  LinkedList::add_front(list, 30)
  
  assert_eq(LinkedList::size(list), 3)
  
  // Test add to back
  LinkedList::add_back(list, 40)
  LinkedList::add_back(list, 50)
  
  assert_eq(LinkedList::size(list), 5)
  
  // Test get operations
  let first = LinkedList::get(list, 0)
  match first {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  let last = LinkedList::get(list, 4)
  match last {
    Some(value) => assert_eq(value, 50)
    None => assert_true(false)
  }
  
  // Test remove from front
  let removed_front = LinkedList::remove_front(list)
  match removed_front {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  assert_eq(LinkedList::size(list), 4)
  assert_eq(LinkedList::get(list, 0), Some(20))
  
  // Test remove from back
  let removed_back = LinkedList::remove_back(list)
  match removed_back {
    Some(value) => assert_eq(value, 50)
    None => assert_true(false)
  }
  
  assert_eq(LinkedList::size(list), 3)
  assert_eq(LinkedList::get(list, 2), Some(40))
}

// Test 5: Binary Tree Operations
test "binary tree operations" {
  let mut tree = BinaryTree::new()
  
  // Test empty tree
  assert_true(BinaryTree::is_empty(tree))
  assert_eq(BinaryTree::size(tree), 0)
  
  // Test insert operations
  BinaryTree::insert(tree, 50)
  BinaryTree::insert(tree, 30)
  BinaryTree::insert(tree, 70)
  BinaryTree::insert(tree, 20)
  BinaryTree::insert(tree, 40)
  BinaryTree::insert(tree, 60)
  BinaryTree::insert(tree, 80)
  
  assert_false(BinaryTree::is_empty(tree))
  assert_eq(BinaryTree::size(tree), 7)
  
  // Test search operations
  assert_true(BinaryTree::contains(tree, 50))
  assert_true(BinaryTree::contains(tree, 20))
  assert_true(BinaryTree::contains(tree, 80))
  assert_false(BinaryTree::contains(tree, 25))
  
  // Test find min and max
  let min_value = BinaryTree::find_min(tree)
  match min_value {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  let max_value = BinaryTree::find_max(tree)
  match max_value {
    Some(value) => assert_eq(value, 80)
    None => assert_true(false)
  }
  
  // Test in-order traversal
  let inorder_result = BinaryTree::inorder_traversal(tree)
  assert_eq(inorder_result, [20, 30, 40, 50, 60, 70, 80])
  
  // Test remove operations
  BinaryTree::remove(tree, 20) // Leaf node
  BinaryTree::remove(tree, 30) // Node with one child
  BinaryTree::remove(tree, 50) // Node with two children
  
  assert_eq(BinaryTree::size(tree), 4)
  assert_false(BinaryTree::contains(tree, 20))
  assert_false(BinaryTree::contains(tree, 30))
  assert_false(BinaryTree::contains(tree, 50))
}