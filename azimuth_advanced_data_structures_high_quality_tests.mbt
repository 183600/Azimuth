// Azimuth Telemetry System - Advanced Data Structures Tests
// This file contains comprehensive test cases for advanced data structures

// Test 1: Tree Data Structure Operations
test "tree data structure operations" {
  // Test binary tree creation and traversal
  let tree = BinaryTree::new()
  BinaryTree::insert(tree, 5)
  BinaryTree::insert(tree, 3)
  BinaryTree::insert(tree, 7)
  BinaryTree::insert(tree, 2)
  BinaryTree::insert(tree, 4)
  BinaryTree::insert(tree, 6)
  BinaryTree::insert(tree, 8)
  
  // Test in-order traversal
  let in_order_result = BinaryTree::in_order_traversal(tree)
  assert_eq(in_order_result, [2, 3, 4, 5, 6, 7, 8])
  
  // Test pre-order traversal
  let pre_order_result = BinaryTree::pre_order_traversal(tree)
  assert_eq(pre_order_result, [5, 3, 2, 4, 7, 6, 8])
  
  // Test post-order traversal
  let post_order_result = BinaryTree::post_order_traversal(tree)
  assert_eq(post_order_result, [2, 4, 3, 6, 8, 7, 5])
  
  // Test search functionality
  assert_true(BinaryTree::search(tree, 5))
  assert_true(BinaryTree::search(tree, 2))
  assert_false(BinaryTree::search(tree, 10))
  
  // Test tree properties
  assert_eq(BinaryTree::size(tree), 7)
  assert_eq(BinaryTree::height(tree), 3)
  assert_true(BinaryTree::is_balanced(tree))
}

// Test 2: Graph Data Structure Operations
test "graph data structure operations" {
  // Test directed graph creation and operations
  let graph = DirectedGraph::new()
  
  // Add vertices
  let v1 = DirectedGraph::add_vertex(graph, "A")
  let v2 = DirectedGraph::add_vertex(graph, "B")
  let v3 = DirectedGraph::add_vertex(graph, "C")
  let v4 = DirectedGraph::add_vertex(graph, "D")
  
  // Add edges
  DirectedGraph::add_edge(graph, v1, v2, 5)
  DirectedGraph::add_edge(graph, v1, v3, 3)
  DirectedGraph::add_edge(graph, v2, v4, 2)
  DirectedGraph::add_edge(graph, v3, v4, 4)
  
  // Test vertex count
  assert_eq(DirectedGraph::vertex_count(graph), 4)
  
  // Test edge count
  assert_eq(DirectedGraph::edge_count(graph), 4)
  
  // Test adjacency
  assert_true(DirectedGraph::has_edge(graph, v1, v2))
  assert_true(DirectedGraph::has_edge(graph, v1, v3))
  assert_false(DirectedGraph::has_edge(graph, v4, v1))
  
  // Test path finding
  let path = DirectedGraph::find_path(graph, v1, v4)
  assert_eq(path.length(), 3)  // A -> B -> D or A -> C -> D
  
  // Test shortest path
  let shortest_path = DirectedGraph::shortest_path(graph, v1, v4)
  assert_eq(shortest_path.length(), 3)  // A -> B -> D (weight 5+2=7 is shorter than A -> C -> D weight 3+4=7)
}

// Test 3: Priority Queue Operations
test "priority queue operations" {
  // Test max priority queue
  let max_pq = MaxPriorityQueue::new()
  MaxPriorityQueue::push(max_pq, 5, "high")
  MaxPriorityQueue::push(max_pq, 3, "medium")
  MaxPriorityQueue::push(max_pq, 7, "highest")
  MaxPriorityQueue::push(max_pq, 1, "low")
  
  // Test size
  assert_eq(MaxPriorityQueue::size(max_pq), 4)
  
  // Test peek (should be highest priority)
  assert_eq(MaxPriorityQueue::peek(max_pq), Some("highest"))
  
  // Test pop (should return in priority order)
  assert_eq(MaxPriorityQueue::pop(max_pq), Some("highest"))
  assert_eq(MaxPriorityQueue::pop(max_pq), Some("high"))
  assert_eq(MaxPriorityQueue::pop(max_pq), Some("medium"))
  assert_eq(MaxPriorityQueue::pop(max_pq), Some("low"))
  assert_eq(MaxPriorityQueue::pop(max_pq), None)
  
  // Test min priority queue
  let min_pq = MinPriorityQueue::new()
  MinPriorityQueue::push(min_pq, 5, "high")
  MinPriorityQueue::push(min_pq, 3, "medium")
  MinPriorityQueue::push(min_pq, 7, "highest")
  MinPriorityQueue::push(min_pq, 1, "low")
  
  // Test peek (should be lowest priority)
  assert_eq(MinPriorityQueue::peek(min_pq), Some("low"))
  
  // Test pop (should return in reverse priority order)
  assert_eq(MinPriorityQueue::pop(min_pq), Some("low"))
  assert_eq(MinPriorityQueue::pop(min_pq), Some("medium"))
  assert_eq(MinPriorityQueue::pop(min_pq), Some("high"))
  assert_eq(MinPriorityQueue::pop(min_pq), Some("highest"))
  assert_eq(MinPriorityQueue::pop(min_pq), None)
}

// Test 4: Hash Table Operations
test "hash table operations" {
  // Test hash table creation and operations
  let hash_table = HashTable::new()
  
  // Test insertion
  HashTable::insert(hash_table, "key1", "value1")
  HashTable::insert(hash_table, "key2", "value2")
  HashTable::insert(hash_table, "key3", "value3")
  
  // Test size
  assert_eq(HashTable::size(hash_table), 3)
  
  // Test lookup
  assert_eq(HashTable::get(hash_table, "key1"), Some("value1"))
  assert_eq(HashTable::get(hash_table, "key2"), Some("value2"))
  assert_eq(HashTable::get(hash_table, "key3"), Some("value3"))
  assert_eq(HashTable::get(hash_table, "nonexistent"), None)
  
  // Test update
  HashTable::insert(hash_table, "key1", "new_value1")
  assert_eq(HashTable::get(hash_table, "key1"), Some("new_value1"))
  
  // Test removal
  HashTable::remove(hash_table, "key2")
  assert_eq(HashTable::get(hash_table, "key2"), None)
  assert_eq(HashTable::size(hash_table), 2)
  
  // Test contains
  assert_true(HashTable::contains(hash_table, "key1"))
  assert_true(HashTable::contains(hash_table, "key3"))
  assert_false(HashTable::contains(hash_table, "key2"))
  
  // Test keys and values
  let keys = HashTable::keys(hash_table)
  let values = HashTable::values(hash_table)
  assert_eq(keys.length(), 2)
  assert_eq(values.length(), 2)
}

// Test 5: Stack and Queue Operations
test "stack and queue operations" {
  // Test stack operations
  let stack = Stack::new()
  
  // Test push
  Stack::push(stack, 1)
  Stack::push(stack, 2)
  Stack::push(stack, 3)
  
  // Test size
  assert_eq(Stack::size(stack), 3)
  
  // Test peek
  assert_eq(Stack::peek(stack), Some(3))
  
  // Test pop (LIFO)
  assert_eq(Stack::pop(stack), Some(3))
  assert_eq(Stack::pop(stack), Some(2))
  assert_eq(Stack::pop(stack), Some(1))
  assert_eq(Stack::pop(stack), None)
  
  // Test queue operations
  let queue = Queue::new()
  
  // Test enqueue
  Queue::enqueue(queue, "first")
  Queue::enqueue(queue, "second")
  Queue::enqueue(queue, "third")
  
  // Test size
  assert_eq(Queue::size(queue), 3)
  
  // Test peek
  assert_eq(Queue::peek(queue), Some("first"))
  
  // Test dequeue (FIFO)
  assert_eq(Queue::dequeue(queue), Some("first"))
  assert_eq(Queue::dequeue(queue), Some("second"))
  assert_eq(Queue::dequeue(queue), Some("third"))
  assert_eq(Queue::dequeue(queue), None)
}

// Test 6: Set Operations
test "set operations" {
  // Test set creation and operations
  let set1 = Set::new()
  let set2 = Set::new()
  
  // Test insertion
  Set::insert(set1, 1)
  Set::insert(set1, 2)
  Set::insert(set1, 3)
  
  Set::insert(set2, 3)
  Set::insert(set2, 4)
  Set::insert(set2, 5)
  
  // Test size
  assert_eq(Set::size(set1), 3)
  assert_eq(Set::size(set2), 3)
  
  // Test contains
  assert_true(Set::contains(set1, 1))
  assert_true(Set::contains(set1, 2))
  assert_true(Set::contains(set1, 3))
  assert_false(Set::contains(set1, 4))
  
  // Test union
  let union_set = Set::union(set1, set2)
  assert_eq(Set::size(union_set), 5)
  assert_true(Set::contains(union_set, 1))
  assert_true(Set::contains(union_set, 5))
  
  // Test intersection
  let intersection_set = Set::intersection(set1, set2)
  assert_eq(Set::size(intersection_set), 1)
  assert_true(Set::contains(intersection_set, 3))
  
  // Test difference
  let difference_set = Set::difference(set1, set2)
  assert_eq(Set::size(difference_set), 2)
  assert_true(Set::contains(difference_set, 1))
  assert_true(Set::contains(difference_set, 2))
  assert_false(Set::contains(difference_set, 3))
  
  // Test subset
  let subset = Set::new()
  Set::insert(subset, 1)
  Set::insert(subset, 2)
  assert_true(Set::is_subset(subset, set1))
  assert_false(Set::is_subset(set1, subset))
}

// Test 7: Linked List Operations
test "linked list operations" {
  // Test singly linked list
  let list = LinkedList::new()
  
  // Test insertion at end
  LinkedList::append(list, 1)
  LinkedList::append(list, 2)
  LinkedList::append(list, 3)
  
  // Test size
  assert_eq(LinkedList::size(list), 3)
  
  // Test get by index
  assert_eq(LinkedList::get(list, 0), Some(1))
  assert_eq(LinkedList::get(list, 1), Some(2))
  assert_eq(LinkedList::get(list, 2), Some(3))
  assert_eq(LinkedList::get(list, 3), None)
  
  // Test insertion at beginning
  LinkedList::prepend(list, 0)
  assert_eq(LinkedList::get(list, 0), Some(0))
  assert_eq(LinkedList::get(list, 1), Some(1))
  assert_eq(LinkedList::size(list), 4)
  
  // Test insertion at specific position
  LinkedList::insert_at(list, 2, 5)
  assert_eq(LinkedList::get(list, 2), Some(5))
  assert_eq(LinkedList::get(list, 3), Some(2))
  assert_eq(LinkedList::size(list), 5)
  
  // Test removal
  LinkedList::remove_at(list, 2)
  assert_eq(LinkedList::get(list, 2), Some(2))
  assert_eq(LinkedList::size(list), 4)
  
  // Test conversion to array
  let array = LinkedList::to_array(list)
  assert_eq(array, [0, 1, 2, 3])
}

// Test 8: Advanced Tree Operations
test "advanced tree operations" {
  // Test AVL tree (self-balancing binary search tree)
  let avl_tree = AVLTree::new()
  
  // Insert elements that would cause imbalance in regular BST
  AVLTree::insert(avl_tree, 10)
  AVLTree::insert(avl_tree, 20)
  AVLTree::insert(avl_tree, 30)
  AVLTree::insert(avl_tree, 40)
  AVLTree::insert(avl_tree, 50)
  
  // Test that tree remains balanced
  assert_true(AVLTree::is_balanced(avl_tree))
  
  // Test search
  assert_true(AVLTree::search(avl_tree, 30))
  assert_false(AVLTree::search(avl_tree, 25))
  
  // Test removal
  AVLTree::remove(avl_tree, 20)
  assert_false(AVLTree::search(avl_tree, 20))
  assert_true(AVLTree::is_balanced(avl_tree))
  
  // Test red-black tree
  let rb_tree = RedBlackTree::new()
  
  // Insert elements
  RedBlackTree::insert(rb_tree, 10)
  RedBlackTree::insert(rb_tree, 20)
  RedBlackTree::insert(rb_tree, 30)
  RedBlackTree::insert(rb_tree, 15)
  RedBlackTree::insert(rb_tree, 25)
  
  // Test that tree remains balanced
  assert_true(RedBlackTree::is_balanced(rb_tree))
  
  // Test search
  assert_true(RedBlackTree::search(rb_tree, 15))
  assert_false(RedBlackTree::search(rb_tree, 35))
  
  // Test in-order traversal
  let in_order = RedBlackTree::in_order_traversal(rb_tree)
  assert_eq(in_order, [10, 15, 20, 25, 30])
}

// Test 9: Advanced Graph Algorithms
test "advanced graph algorithms" {
  // Test weighted undirected graph
  let graph = WeightedGraph::new()
  
  // Add vertices
  let v1 = WeightedGraph::add_vertex(graph, "A")
  let v2 = WeightedGraph::add_vertex(graph, "B")
  let v3 = WeightedGraph::add_vertex(graph, "C")
  let v4 = WeightedGraph::add_vertex(graph, "D")
  let v5 = WeightedGraph::add_vertex(graph, "E")
  
  // Add edges with weights
  WeightedGraph::add_edge(graph, v1, v2, 2)
  WeightedGraph::add_edge(graph, v1, v3, 4)
  WeightedGraph::add_edge(graph, v2, v3, 1)
  WeightedGraph::add_edge(graph, v2, v4, 7)
  WeightedGraph::add_edge(graph, v3, v4, 3)
  WeightedGraph::add_edge(graph, v3, v5, 5)
  WeightedGraph::add_edge(graph, v4, v5, 1)
  
  // Test Dijkstra's algorithm
  let distances = WeightedGraph::dijkstra(graph, v1)
  assert_eq(distances[v1], 0)  // Distance to itself
  assert_eq(distances[v2], 2)  // A -> B
  assert_eq(distances[v3], 3)  // A -> B -> C
  assert_eq(distances[v4], 6)  // A -> B -> C -> D or A -> B -> D
  assert_eq(distances[v5], 7)  // A -> B -> C -> D -> E or A -> B -> C -> E
  
  // Test minimum spanning tree (Prim's algorithm)
  let mst = WeightedGraph::prim_mst(graph)
  assert_eq(mst.length(), 4)  // 5 vertices, 4 edges in MST
  
  // Test minimum spanning tree (Kruskal's algorithm)
  let mst_kruskal = WeightedGraph::kruskal_mst(graph)
  assert_eq(mst_kruskal.length(), 4)  // 5 vertices, 4 edges in MST
  
  // Test connected components
  let components = WeightedGraph::connected_components(graph)
  assert_eq(components.length(), 1)  // All vertices are connected
  
  // Test topological sort
  let dag = DirectedAcyclicGraph::new()
  let a = DirectedAcyclicGraph::add_vertex(dag, "A")
  let b = DirectedAcyclicGraph::add_vertex(dag, "B")
  let c = DirectedAcyclicGraph::add_vertex(dag, "C")
  let d = DirectedAcyclicGraph::add_vertex(dag, "D")
  
  DirectedAcyclicGraph::add_edge(dag, a, b)
  DirectedAcyclicGraph::add_edge(dag, a, c)
  DirectedAcyclicGraph::add_edge(dag, b, d)
  DirectedAcyclicGraph::add_edge(dag, c, d)
  
  let topological_order = DirectedAcyclicGraph::topological_sort(dag)
  // A should come before B and C, and B and C should come before D
  let a_index = topological_order.index_of(a)
  let b_index = topological_order.index_of(b)
  let c_index = topological_order.index_of(c)
  let d_index = topological_order.index_of(d)
  
  assert_true(a_index < b_index)
  assert_true(a_index < c_index)
  assert_true(b_index < d_index)
  assert_true(c_index < d_index)
}

// Test 10: Advanced Data Structure Performance
test "advanced data structure performance" {
  // Test performance with large datasets
  let large_array = Array::new(10000)
  for i in 0..10000 {
    large_array[i] = i
  }
  
  // Test binary search on sorted array
  let index = binary_search(large_array, 5000)
  assert_eq(index, Some(5000))
  
  let not_found = binary_search(large_array, 15000)
  assert_eq(not_found, None)
  
  // Test heap operations with large dataset
  let heap = MaxHeap::new()
  for i in 0..10000 {
    MaxHeap::push(heap, i)
  }
  
  // Test that heap maintains max property
  assert_eq(MaxHeap::peek(heap), Some(9999))
  
  // Test extracting all elements
  let mut last_value = 10000
  while MaxHeap::size(heap) > 0 {
    let value = MaxHeap::pop(heap)
    match value {
      Some(v) => {
        assert_true(v <= last_value)
        last_value = v
      }
      None => assert_true(false)
    }
  }
  
  // Test trie (prefix tree) operations
  let trie = Trie::new()
  let words = ["apple", "app", "application", "banana", "band", "bandana"]
  
  for word in words {
    Trie::insert(trie, word)
  }
  
  // Test search
  assert_true(Trie::search(trie, "apple"))
  assert_true(Trie::search(trie, "app"))
  assert_false(Trie::search(trie, "appl"))
  
  // Test prefix search
  let app_words = Trie::prefix_search(trie, "app")
  assert_eq(app_words.length(), 3)  // app, apple, application
  
  let ban_words = Trie::prefix_search(trie, "ban")
  assert_eq(ban_words.length(), 3)  // banana, band, bandana
  
  // Test deletion
  Trie::remove(trie, "app")
  assert_false(Trie::search(trie, "app"))
  assert_true(Trie::search(trie, "apple"))
  assert_true(Trie::search(trie, "application"))
}