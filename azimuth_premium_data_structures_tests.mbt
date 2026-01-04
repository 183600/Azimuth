// Azimuth Premium Data Structures Tests
// This file contains high-quality test cases for advanced data structure operations

// Test 1: Advanced Stack Operations with Error Handling
test "advanced stack operations with error handling" {
  let mut stack = Stack::new()
  
  // Test empty stack behavior
  assert_eq(Stack::size(stack), 0)
  assert_true(Stack::is_empty(stack))
  
  // Test push operations
  Stack::push(stack, 10)
  Stack::push(stack, 20)
  Stack::push(stack, 30)
  assert_eq(Stack::size(stack), 3)
  assert_false(Stack::is_empty(stack))
  
  // Test peek operations
  match Stack::peek(stack) {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  // Test pop operations
  match Stack::pop(stack) {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  match Stack::peek(stack) {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  // Test pop all elements
  assert_eq(Stack::pop(stack), Some(20))
  assert_eq(Stack::pop(stack), Some(10))
  assert_eq(Stack::pop(stack), None)  // Stack is now empty
  
  // Test empty stack again
  assert_eq(Stack::size(stack), 0)
  assert_true(Stack::is_empty(stack))
  assert_eq(Stack::peek(stack), None)
}

// Test 2: Queue Operations with Capacity Management
test "queue operations with capacity management" {
  let mut queue = Queue::with_capacity(5)
  
  // Test empty queue
  assert_eq(Queue::size(queue), 0)
  assert_true(Queue::is_empty(queue))
  
  // Test enqueue operations
  assert_true(Queue::enqueue(queue, "first"))
  assert_true(Queue::enqueue(queue, "second"))
  assert_true(Queue::enqueue(queue, "third"))
  assert_eq(Queue::size(queue), 3)
  
  // Test dequeue operations (FIFO)
  match Queue::dequeue(queue) {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  match Queue::dequeue(queue) {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  // Test front element
  match Queue::front(queue) {
    Some(value) => assert_eq(value, "third")
    None => assert_true(false)
  }
  
  // Test capacity limits
  assert_true(Queue::enqueue(queue, "fourth"))
  assert_true(Queue::enqueue(queue, "fifth"))
  
  // This should fail due to capacity limit
  assert_false(Queue::enqueue(queue, "sixth"))
  
  // Test full queue operations
  assert_eq(Queue::size(queue), 4)
  assert_false(Queue::is_empty(queue))
  
  // Clear queue
  Queue::clear(queue)
  assert_eq(Queue::size(queue), 0)
  assert_true(Queue::is_empty(queue))
}

// Test 3: HashMap Operations with Complex Keys
test "hashmap operations with complex keys" {
  let mut map = HashMap::new()
  
  // Test basic operations
  HashMap::insert(map, "key1", 100)
  HashMap::insert(map, "key2", 200)
  HashMap::insert(map, "key3", 300)
  
  assert_eq(HashMap::size(map), 3)
  assert_true(HashMap::contains_key(map, "key1"))
  assert_false(HashMap::contains_key(map, "nonexistent"))
  
  // Test get operations
  match HashMap::get(map, "key1") {
    Some(value) => assert_eq(value, 100)
    None => assert_true(false)
  }
  
  // Test update operations
  HashMap::insert(map, "key1", 150)  // Update existing key
  match HashMap::get(map, "key1") {
    Some(value) => assert_eq(value, 150)
    None => assert_true(false)
  }
  
  // Test remove operations
  match HashMap::remove(map, "key2") {
    Some(value) => assert_eq(value, 200)
    None => assert_true(false)
  }
  
  assert_false(HashMap::contains_key(map, "key2"))
  assert_eq(HashMap::size(map), 2)
  
  // Test iteration
  let mut sum = 0
  let mut count = 0
  for (key, value) in HashMap::iter(map) {
    sum = sum + value
    count = count + 1
  }
  assert_eq(sum, 450)  // 150 + 300
  assert_eq(count, 2)
}

// Test 4: Binary Search Tree Operations
test "binary search tree operations" {
  let mut bst = BinarySearchTree::new()
  
  // Test empty tree
  assert_true(BinarySearchTree::is_empty(bst))
  assert_eq(BinarySearchTree::size(bst), 0)
  
  // Test insert operations
  BinarySearchTree::insert(bst, 50)
  BinarySearchTree::insert(bst, 30)
  BinarySearchTree::insert(bst, 70)
  BinarySearchTree::insert(bst, 20)
  BinarySearchTree::insert(bst, 40)
  BinarySearchTree::insert(bst, 60)
  BinarySearchTree::insert(bst, 80)
  
  assert_false(BinarySearchTree::is_empty(bst))
  assert_eq(BinarySearchTree::size(bst), 7)
  
  // Test search operations
  assert_true(BinarySearchTree::contains(bst, 50))
  assert_true(BinarySearchTree::contains(bst, 20))
  assert_true(BinarySearchTree::contains(bst, 80))
  assert_false(BinarySearchTree::contains(bst, 25))
  
  // Test min/max operations
  match BinarySearchTree::min(bst) {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  match BinarySearchTree::max(bst) {
    Some(value) => assert_eq(value, 80)
    None => assert_true(false)
  }
  
  // Test remove operations
  BinarySearchTree::remove(bst, 20)  // Remove leaf node
  assert_false(BinarySearchTree::contains(bst, 20))
  assert_eq(BinarySearchTree::size(bst), 6)
  
  BinarySearchTree::remove(bst, 30)  // Remove node with one child
  assert_false(BinarySearchTree::contains(bst, 30))
  assert_eq(BinarySearchTree::size(bst), 5)
  
  BinarySearchTree::remove(bst, 50)  // Remove node with two children
  assert_false(BinarySearchTree::contains(bst, 50))
  assert_eq(BinarySearchTree::size(bst), 4)
}

// Test 5: Priority Queue Operations
test "priority queue operations" {
  let mut pq = PriorityQueue::new()
  
  // Test empty priority queue
  assert_true(PriorityQueue::is_empty(pq))
  assert_eq(PriorityQueue::size(pq), 0)
  
  // Test enqueue operations (with priority)
  PriorityQueue::enqueue(pq, ("task1", 3))
  PriorityQueue::enqueue(pq, ("task2", 1))
  PriorityQueue::enqueue(pq, ("task3", 5))
  PriorityQueue::enqueue(pq, ("task4", 2))
  
  assert_false(PriorityQueue::is_empty(pq))
  assert_eq(PriorityQueue::size(pq), 4)
  
  // Test dequeue operations (highest priority first)
  match PriorityQueue::dequeue(pq) {
    Some((task, priority)) => {
      assert_eq(task, "task3")
      assert_eq(priority, 5)
    }
    None => assert_true(false)
  }
  
  match PriorityQueue::dequeue(pq) {
    Some((task, priority)) => {
      assert_eq(task, "task1")
      assert_eq(priority, 3)
    }
    None => assert_true(false)
  }
  
  match PriorityQueue::dequeue(pq) {
    Some((task, priority)) => {
      assert_eq(task, "task4")
      assert_eq(priority, 2)
    }
    None => assert_true(false)
  }
  
  match PriorityQueue::dequeue(pq) {
    Some((task, priority)) => {
      assert_eq(task, "task2")
      assert_eq(priority, 1)
    }
    None => assert_true(false)
  }
  
  // Test empty queue again
  assert_true(PriorityQueue::is_empty(pq))
  assert_eq(PriorityQueue::size(pq), 0)
  assert_eq(PriorityQueue::dequeue(pq), None)
}

// Test 6: Linked List Operations with Edge Cases
test "linked list operations with edge cases" {
  let mut list = LinkedList::new()
  
  // Test empty list
  assert_true(LinkedList::is_empty(list))
  assert_eq(LinkedList::size(list), 0)
  
  // Test add operations
  LinkedList::add_first(list, "first")
  LinkedList::add_last(list, "last")
  LinkedList::add_first(list, "new_first")
  
  assert_eq(LinkedList::size(list), 3)
  
  // Test get operations
  match LinkedList::get_first(list) {
    Some(value) => assert_eq(value, "new_first")
    None => assert_true(false)
  }
  
  match LinkedList::get_last(list) {
    Some(value) => assert_eq(value, "last")
    None => assert_true(false)
  }
  
  // Test remove operations
  match LinkedList::remove_first(list) {
    Some(value) => assert_eq(value, "new_first")
    None => assert_true(false)
  }
  
  match LinkedList::remove_last(list) {
    Some(value) => assert_eq(value, "last")
    None => assert_true(false)
  }
  
  assert_eq(LinkedList::size(list), 1)
  
  // Test remove the last element
  match LinkedList::remove_first(list) {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  // Test empty list again
  assert_true(LinkedList::is_empty(list))
  assert_eq(LinkedList::size(list), 0)
  assert_eq(LinkedList::get_first(list), None)
  assert_eq(LinkedList::get_last(list), None)
}

// Test 7: Graph Operations with Traversal
test "graph operations with traversal" {
  let mut graph = Graph::new()
  
  // Test empty graph
  assert_eq(Graph::vertex_count(graph), 0)
  assert_eq(Graph::edge_count(graph), 0)
  
  // Test add vertices
  let v1 = Graph::add_vertex(graph, "A")
  let v2 = Graph::add_vertex(graph, "B")
  let v3 = Graph::add_vertex(graph, "C")
  let v4 = Graph::add_vertex(graph, "D")
  
  assert_eq(Graph::vertex_count(graph), 4)
  
  // Test add edges
  Graph::add_edge(graph, v1, v2, 5)
  Graph::add_edge(graph, v1, v3, 3)
  Graph::add_edge(graph, v2, v4, 2)
  Graph::add_edge(graph, v3, v4, 1)
  
  assert_eq(Graph::edge_count(graph), 4)
  
  // Test adjacency
  let v1_neighbors = Graph::neighbors(graph, v1)
  assert_eq(v1_neighbors.length(), 2)
  
  // Test edge weight
  match Graph::edge_weight(graph, v1, v2) {
    Some(weight) => assert_eq(weight, 5)
    None => assert_true(false)
  }
  
  // Test non-existent edge
  assert_eq(Graph::edge_weight(graph, v2, v3), None)
  
  // Test traversal (BFS)
  let bfs_result = Graph::bfs(graph, v1)
  assert_eq(bfs_result.length(), 4)  // Should visit all vertices
  
  // Test traversal (DFS)
  let dfs_result = Graph::dfs(graph, v1)
  assert_eq(dfs_result.length(), 4)  // Should visit all vertices
}

// Test 8: Advanced Set Operations
test "advanced set operations" {
  let mut set1 = Set::new()
  let mut set2 = Set::new()
  
  // Test empty sets
  assert_true(Set::is_empty(set1))
  assert_true(Set::is_empty(set2))
  
  // Test add operations
  Set::add(set1, "apple")
  Set::add(set1, "banana")
  Set::add(set1, "cherry")
  
  Set::add(set2, "banana")
  Set::add(set2, "cherry")
  Set::add(set2, "date")
  
  assert_eq(Set::size(set1), 3)
  assert_eq(Set::size(set2), 3)
  
  // Test contains operations
  assert_true(Set::contains(set1, "apple"))
  assert_true(Set::contains(set1, "banana"))
  assert_false(Set::contains(set1, "date"))
  
  // Test set operations
  let union = Set::union(set1, set2)
  assert_eq(Set::size(union), 4)  // apple, banana, cherry, date
  
  let intersection = Set::intersection(set1, set2)
  assert_eq(Set::size(intersection), 2)  // banana, cherry
  
  let difference = Set::difference(set1, set2)
  assert_eq(Set::size(difference), 1)  // apple only
  
  // Test remove operations
  Set::remove(set1, "banana")
  assert_false(Set::contains(set1, "banana"))
  assert_eq(Set::size(set1), 2)
  
  // Test clear operations
  Set::clear(set1)
  assert_true(Set::is_empty(set1))
  assert_eq(Set::size(set1), 0)
}