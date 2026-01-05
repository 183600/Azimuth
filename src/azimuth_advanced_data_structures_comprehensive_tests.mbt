// Azimuth Advanced Data Structure Tests
// This file contains comprehensive test cases for advanced data structures used in the telemetry system

// Test 1: Circular Buffer Implementation
test "circular buffer operations" {
  let buffer_size = 5
  let buffer = CircularBuffer::new(buffer_size)
  
  // Test empty buffer
  assert_true(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 0)
  
  // Test adding elements
  CircularBuffer::push(buffer, 1)
  CircularBuffer::push(buffer, 2)
  CircularBuffer::push(buffer, 3)
  
  assert_false(CircularBuffer::is_empty(buffer))
  assert_false(CircularBuffer::is_full(buffer))
  assert_eq(CircularBuffer::size(buffer), 3)
  
  // Test removing elements
  let elem1 = CircularBuffer::pop(buffer)
  match elem1 {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_eq(CircularBuffer::size(buffer), 2)
  
  // Fill buffer to capacity
  CircularBuffer::push(buffer, 4)
  CircularBuffer::push(buffer, 5)
  assert_true(CircularBuffer::is_full(buffer))
  
  // Test overwriting behavior
  CircularBuffer::push(buffer, 6)
  let elem2 = CircularBuffer::pop(buffer)
  match elem2 {
    Some(value) => assert_eq(value, 3) // First element should be overwritten
    None => assert_true(false)
  }
}

// Test 2: Priority Queue Implementation
test "priority queue operations" {
  let priority_queue = PriorityQueue::new()
  
  // Test empty queue
  assert_true(PriorityQueue::is_empty(priority_queue))
  assert_eq(PriorityQueue::size(priority_queue), 0)
  
  // Test adding elements with different priorities
  PriorityQueue::enqueue(priority_queue, "low_priority_task", 1)
  PriorityQueue::enqueue(priority_queue, "high_priority_task", 10)
  PriorityQueue::enqueue(priority_queue, "medium_priority_task", 5)
  
  assert_false(PriorityQueue::is_empty(priority_queue))
  assert_eq(PriorityQueue::size(priority_queue), 3)
  
  // Test dequeue order (highest priority first)
  let task1 = PriorityQueue::dequeue(priority_queue)
  match task1 {
    Some((task, priority)) => {
      assert_eq(task, "high_priority_task")
      assert_eq(priority, 10)
    }
    None => assert_true(false)
  }
  
  let task2 = PriorityQueue::dequeue(priority_queue)
  match task2 {
    Some((task, priority)) => {
      assert_eq(task, "medium_priority_task")
      assert_eq(priority, 5)
    }
    None => assert_true(false)
  }
  
  let task3 = PriorityQueue::dequeue(priority_queue)
  match task3 {
    Some((task, priority)) => {
      assert_eq(task, "low_priority_task")
      assert_eq(priority, 1)
    }
    None => assert_true(false)
  }
  
  // Test empty queue after dequeue
  assert_true(PriorityQueue::is_empty(priority_queue))
  
  // Test dequeue from empty queue
  let empty_result = PriorityQueue::dequeue(priority_queue)
  match empty_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 3: LRU Cache Implementation
test "lru cache operations" {
  let cache_capacity = 3
  let cache = LRUCache::new(cache_capacity)
  
  // Test empty cache
  assert_eq(LRUCache::size(cache), 0)
  
  // Test adding elements
  LRUCache::put(cache, "key1", "value1")
  LRUCache::put(cache, "key2", "value2")
  LRUCache::put(cache, "key3", "value3")
  
  assert_eq(LRUCache::size(cache), 3)
  
  // Test getting elements
  let value1 = LRUCache::get(cache, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test updating existing key
  LRUCache::put(cache, "key1", "updated_value1")
  let updated_value = LRUCache::get(cache, "key1")
  match updated_value {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  // Test LRU eviction (key2 should be evicted as it's least recently used)
  LRUCache::put(cache, "key4", "value4")
  assert_eq(LRUCache::size(cache), 3)
  
  let evicted_value = LRUCache::get(cache, "key2")
  match evicted_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test non-existent key
  let non_existent = LRUCache::get(cache, "non_existent_key")
  match non_existent {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Trie (Prefix Tree) Implementation
test "trie prefix tree operations" {
  let trie = Trie::new()
  
  // Test empty trie
  assert_false(Trie::contains(trie, "test"))
  assert_eq(Trie::count_prefixes(trie, "test"), 0)
  
  // Test inserting words
  Trie::insert(trie, "test")
  Trie::insert(trie, "testing")
  Trie::insert(trie, "tester")
  Trie::insert(trie, "team")
  
  // Test word existence
  assert_true(Trie::contains(trie, "test"))
  assert_true(Trie::contains(trie, "testing"))
  assert_true(Trie::contains(trie, "tester"))
  assert_true(Trie::contains(trie, "team"))
  assert_false(Trie::contains(trie, "tent"))
  
  // Test prefix counting
  assert_eq(Trie::count_prefixes(trie, "test"), 3) // test, testing, tester
  assert_eq(Trie::count_prefixes(trie, "te"), 4) // test, testing, tester, team
  assert_eq(Trie::count_prefixes(trie, "xyz"), 0)
  
  // Test word removal
  Trie::remove(trie, "testing")
  assert_false(Trie::contains(trie, "testing"))
  assert_true(Trie::contains(trie, "test"))
  assert_true(Trie::contains(trie, "tester"))
  
  // Test prefix counting after removal
  assert_eq(Trie::count_prefixes(trie, "test"), 2) // test, tester
}

// Test 5: Bloom Filter Implementation
test "bloom filter operations" {
  let expected_size = 1000
  let false_positive_rate = 0.01
  let bloom_filter = BloomFilter::new(expected_size, false_positive_rate)
  
  // Test empty filter
  assert_false(BloomFilter::might_contain(bloom_filter, "test_item"))
  
  // Test adding items
  BloomFilter::add(bloom_filter, "item1")
  BloomFilter::add(bloom_filter, "item2")
  BloomFilter::add(bloom_filter, "item3")
  
  // Test item existence (no false negatives)
  assert_true(BloomFilter::might_contain(bloom_filter, "item1"))
  assert_true(BloomFilter::might_contain(bloom_filter, "item2"))
  assert_true(BloomFilter::might_contain(bloom_filter, "item3"))
  
  // Test non-existent items (might have false positives)
  let might_contain = BloomFilter::might_contain(bloom_filter, "non_existent_item")
  // This can be true or false due to bloom filter characteristics
  
  // Test union operation
  let another_filter = BloomFilter::new(expected_size, false_positive_rate)
  BloomFilter::add(another_filter, "item4")
  BloomFilter::add(another_filter, "item5")
  
  let union_filter = BloomFilter::union(bloom_filter, another_filter)
  assert_true(BloomFilter::might_contain(union_filter, "item1"))
  assert_true(BloomFilter::might_contain(union_filter, "item4"))
}

// Test 6: Skip List Implementation
test "skip list operations" {
  let skip_list = SkipList::new()
  
  // Test empty skip list
  assert_true(SkipList::is_empty(skip_list))
  assert_eq(SkipList::size(skip_list), 0)
  
  // Test inserting elements
  SkipList::insert(skip_list, 10, "value10")
  SkipList::insert(skip_list, 5, "value5")
  SkipList::insert(skip_list, 15, "value15")
  SkipList::insert(skip_list, 7, "value7")
  
  assert_false(SkipList::is_empty(skip_list))
  assert_eq(SkipList::size(skip_list), 4)
  
  // Test finding elements
  let found_value = SkipList::find(skip_list, 7)
  match found_value {
    Some(v) => assert_eq(v, "value7")
    None => assert_true(false)
  }
  
  // Test non-existent element
  let not_found = SkipList::find(skip_list, 99)
  match not_found {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test removing elements
  let removed_value = SkipList::remove(skip_list, 5)
  match removed_value {
    Some(v) => assert_eq(v, "value5")
    None => assert_true(false)
  }
  
  assert_eq(SkipList::size(skip_list), 3)
  
  // Test finding removed element
  let removed_not_found = SkipList::find(skip_list, 5)
  match removed_not_found {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test range queries
  let range_values = SkipList::range_query(skip_list, 7, 15)
  assert_eq(range_values.length(), 2) // Should contain values for keys 7 and 15
}

// Test 7: Union-Find (Disjoint Set) Implementation
test "union find operations" {
  let union_find = UnionFind::new(10)
  
  // Test initial state (each element is in its own set)
  for i in 0..10 {
    assert_eq(UnionFind::find(union_find, i), i)
  }
  
  // Test union operations
  UnionFind::union(union_find, 0, 1)
  UnionFind::union(union_find, 2, 3)
  UnionFind::union(union_find, 0, 2) // This should connect all four elements
  
  // Test find operations after union
  assert_eq(UnionFind::find(union_find, 0), UnionFind::find(union_find, 1))
  assert_eq(UnionFind::find(union_find, 2), UnionFind::find(union_find, 3))
  assert_eq(UnionFind::find(union_find, 0), UnionFind::find(union_find, 2))
  
  // Test connected components
  assert_true(UnionFind::connected(union_find, 0, 3))
  assert_true(UnionFind::connected(union_find, 1, 2))
  assert_false(UnionFind::connected(union_find, 0, 4))
  
  // Test count of disjoint sets
  assert_eq(UnionFind::count_sets(union_find), 7) // 4 elements connected, 6 separate elements
  
  // Test more union operations
  UnionFind::union(union_find, 4, 5)
  UnionFind::union(union_find, 5, 6)
  UnionFind::union(union_find, 6, 7)
  
  assert_eq(UnionFind::count_sets(union_find), 4) // 4 elements + 4 elements + 2 separate elements
}

// Test 8: Segment Tree Implementation
test "segment tree operations" {
  let data = [1, 3, 5, 7, 9, 11]
  let segment_tree = SegmentTree::new(data)
  
  // Test range sum queries
  assert_eq(SegmentTree::range_query(segment_tree, 0, 2), 9)  // 1 + 3 + 5
  assert_eq(SegmentTree::range_query(segment_tree, 1, 4), 24) // 3 + 5 + 7 + 9
  assert_eq(SegmentTree::range_query(segment_tree, 0, 5), 36) // Sum of all elements
  
  // Test point updates
  SegmentTree::point_update(segment_tree, 2, 10) // Update index 2 from 5 to 10
  
  // Test queries after update
  assert_eq(SegmentTree::range_query(segment_tree, 0, 2), 14) // 1 + 3 + 10
  assert_eq(SegmentTree::range_query(segment_tree, 0, 5), 41) // Total sum after update
  
  // Test range minimum queries (if supported)
  let min_segment_tree = SegmentTree::new_min(data)
  assert_eq(SegmentTree::range_query(min_segment_tree, 0, 2), 1)  // min(1, 3, 5)
  assert_eq(SegmentTree::range_query(min_segment_tree, 1, 4), 3)  // min(3, 5, 7, 9)
  
  // Test range maximum queries (if supported)
  let max_segment_tree = SegmentTree::new_max(data)
  assert_eq(SegmentTree::range_query(max_segment_tree, 0, 2), 5)  // max(1, 3, 5)
  assert_eq(SegmentTree::range_query(max_segment_tree, 1, 4), 9)  // max(3, 5, 7, 9)
}

// Test 9: Hash Map with Custom Hash Function
test "custom hash map operations" {
  let custom_hash_map = CustomHashMap::new()
  
  // Test empty map
  assert_true(CustomHashMap::is_empty(custom_hash_map))
  assert_eq(CustomHashMap::size(custom_hash_map), 0)
  
  // Test inserting key-value pairs
  CustomHashMap::insert(custom_hash_map, "key1", "value1")
  CustomHashMap::insert(custom_hash_map, "key2", "value2")
  CustomHashMap::insert(custom_hash_map, "key3", "value3")
  
  assert_false(CustomHashMap::is_empty(custom_hash_map))
  assert_eq(CustomHashMap::size(custom_hash_map), 3)
  
  // Test getting values
  let value1 = CustomHashMap::get(custom_hash_map, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Test updating existing key
  CustomHashMap::insert(custom_hash_map, "key1", "updated_value1")
  let updated_value = CustomHashMap::get(custom_hash_map, "key1")
  match updated_value {
    Some(v) => assert_eq(v, "updated_value1")
    None => assert_true(false)
  }
  
  // Test removing keys
  let removed_value = CustomHashMap::remove(custom_hash_map, "key2")
  match removed_value {
    Some(v) => assert_eq(v, "value2")
    None => assert_true(false)
  }
  
  assert_eq(CustomHashMap::size(custom_hash_map), 2)
  
  // Test getting removed key
  let removed_key_value = CustomHashMap::get(custom_hash_map, "key2")
  match removed_key_value {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test iteration over map
  let keys = CustomHashMap::keys(custom_hash_map)
  assert_eq(keys.length(), 2)
  assert_true(keys.contains("key1"))
  assert_true(keys.contains("key3"))
  
  let values = CustomHashMap::values(custom_hash_map)
  assert_eq(values.length(), 2)
  assert_true(values.contains("updated_value1"))
  assert_true(values.contains("value3"))
}

// Test 10: Graph Data Structure Operations
test "graph data structure operations" {
  let graph = Graph::new()
  
  // Test empty graph
  assert_eq(Graph::vertex_count(graph), 0)
  assert_eq(Graph::edge_count(graph), 0)
  
  // Test adding vertices
  Graph::add_vertex(graph, "A")
  Graph::add_vertex(graph, "B")
  Graph::add_vertex(graph, "C")
  Graph::add_vertex(graph, "D")
  
  assert_eq(Graph::vertex_count(graph), 4)
  
  // Test adding edges
  Graph::add_edge(graph, "A", "B", 5)
  Graph::add_edge(graph, "B", "C", 3)
  Graph::add_edge(graph, "C", "D", 2)
  Graph::add_edge(graph, "A", "D", 10)
  
  assert_eq(Graph::edge_count(graph), 4)
  
  // Test edge existence
  assert_true(Graph::has_edge(graph, "A", "B"))
  assert_true(Graph::has_edge(graph, "C", "D"))
  assert_false(Graph::has_edge(graph, "B", "A")) // Assuming directed graph
  assert_false(Graph::has_edge(graph, "A", "E")) // Non-existent vertex
  
  // Test getting edge weight
  let weight_ab = Graph::edge_weight(graph, "A", "B")
  match weight_ab {
    Some(w) => assert_eq(w, 5)
    None => assert_true(false)
  }
  
  // Test getting neighbors
  let neighbors_a = Graph::neighbors(graph, "A")
  assert_eq(neighbors_a.length(), 2) // B and D
  assert_true(neighbors_a.contains("B"))
  assert_true(neighbors_a.contains("D"))
  
  // Test removing edges
  Graph::remove_edge(graph, "A", "D")
  assert_eq(Graph::edge_count(graph), 3)
  assert_false(Graph::has_edge(graph, "A", "D"))
  
  // Test removing vertices
  Graph::remove_vertex(graph, "C")
  assert_eq(Graph::vertex_count(graph), 3)
  assert_eq(Graph::edge_count(graph), 1) // Only A->B should remain
  
  // Test shortest path (if implemented)
  let path = Graph::shortest_path(graph, "A", "D")
  match path {
    Some(p) => {
      // Path should be A -> B (and then to other vertices if connected)
      assert_eq(p.length(), 2)
      assert_eq(p[0], "A")
      assert_eq(p[1], "B")
    }
    None => assert_true(true) // No path from A to D after removing C
  }
}