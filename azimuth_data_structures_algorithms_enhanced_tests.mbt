// Azimuth Telemetry System - Enhanced Data Structures and Algorithms Tests
// This file contains comprehensive test cases for advanced data structures and algorithms

// Test 1: Advanced Tree Structures
test "advanced tree structures for telemetry data" {
  // AVL Tree implementation
  type AVLNode[T] = {
    value: T,
    left: Option[AVLNode[T>>,
    right: Option[AVLNode[T>>,
    height: Int
  }
  
  type AVLTree[T] = {
    root: Option[AVLNode[T>>
    compare: (T, T) -> Int
  }
  
  let avl_tree_new = fn[T](compare: (T, T) -> Int) -> AVLTree[T] {
    { root: None, compare }
  }
  
  let avl_node_height = fn[T](node: Option[AVLNode[T>>) -> Int {
    match node {
      Some(n) => n.height
      None => 0
    }
  }
  
  let avl_node_balance = fn[T](node: Option[AVLNode[T>>) -> Int {
    match node {
      Some(n) => avl_node_height(n.left) - avl_node_height(n.right)
      None => 0
    }
  }
  
  let avl_node_new = fn[T](value: T, left: Option[AVLNode[T>>, right: Option[AVLNode[T>>) -> AVLNode[T] {
    {
      value,
      left,
      right,
      height: 1 + max(avl_node_height(left), avl_node_height(right))
    }
  }
  
  let avl_tree_rotate_right = fn[T](y: AVLNode[T]) -> AVLNode[T] {
    match y.left {
      Some(x) => {
        let T2 = x.right
        
        let new_x = { value: x.value, left: x.left, right: Some(y), height: x.height }
        let new_y = { value: y.value, left: T2, right: y.right, height: y.height }
        
        avl_node_new(new_x.value, new_x.left, Some(new_y))
      }
      None => y
    }
  }
  
  let avl_tree_rotate_left = fn[T](x: AVLNode[T]) -> AVLNode[T] {
    match x.right {
      Some(y) => {
        let T2 = y.left
        
        let new_y = { value: y.value, left: Some(x), right: y.right, height: y.height }
        let new_x = { value: x.value, left: x.left, right: T2, height: x.height }
        
        avl_node_new(new_y.value, Some(new_x), new_y.right)
      }
      None => x
    }
  }
  
  let avl_tree_insert_node = fn[T](node: Option[AVLNode[T>>, value: T, compare: (T, T) -> Int) -> AVLNode[T] {
    match node {
      None => avl_node_new(value, None, None)
      Some(n) => {
        let cmp = compare(value, n.value)
        
        if cmp < 0 {
          let new_left = avl_tree_insert_node(n.left, value, compare)
          let new_node = { value: n.value, left: Some(new_left), right: n.right, height: n.height }
          
          let balance = avl_node_balance(Some(new_node))
          
          if balance > 1 && compare(value, new_left.value) < 0 {
            avl_tree_rotate_right(new_node)
          } else if balance < -1 && compare(value, new_node.right.unwrap().value) > 0 {
            avl_tree_rotate_left(new_node)
          } else {
            new_node
          }
        } else if cmp > 0 {
          let new_right = avl_tree_insert_node(n.right, value, compare)
          let new_node = { value: n.value, left: n.left, right: Some(new_right), height: n.height }
          
          let balance = avl_node_balance(Some(new_node))
          
          if balance > 1 && compare(value, n.left.unwrap().value) > 0 {
            let rotated_left = avl_tree_rotate_left(n.left.unwrap())
            let new_left = { value: rotated_left.value, left: rotated_left.left, right: Some(n), height: rotated_left.height }
            avl_tree_rotate_right(new_node)
          } else if balance < -1 && compare(value, new_right.unwrap().value) < 0 {
            let rotated_right = avl_tree_rotate_right(new_node.right.unwrap())
            let new_right = { value: n.value, left: Some(rotated_right), right: new_right.right, height: new_right.height }
            avl_tree_rotate_left(new_node)
          } else {
            new_node
          }
        } else {
          n  // Duplicate value
        }
      }
    }
  }
  
  let avl_tree_insert = fn[T](tree: AVLTree[T], value: T) -> AVLTree[T] {
    let new_root = avl_tree_insert_node(tree.root, value, tree.compare)
    { root: Some(new_root), compare: tree.compare }
  }
  
  let avl_tree_contains = fn[T](tree: AVLTree[T], value: T) -> Bool {
    let mut current = tree.root
    
    while true {
      match current {
        Some(node) => {
          let cmp = tree.compare(value, node.value)
          if cmp == 0 {
            return true
          } else if cmp < 0 {
            current = node.left
          } else {
            current = node.right
          }
        }
        None => return false
      }
    }
  }
  
  // Test AVL tree
  let int_compare = fn(a: Int, b: Int) -> Int {
    if a < b { -1 } else if a > b { 1 } else { 0 }
  }
  
  let tree1 = avl_tree_new(int_compare)
  let tree2 = avl_tree_insert(tree1, 10)
  let tree3 = avl_tree_insert(tree2, 20)
  let tree4 = avl_tree_insert(tree3, 30)
  let tree5 = avl_tree_insert(tree4, 40)
  let tree6 = avl_tree_insert(tree5, 50)
  let tree7 = avl_tree_insert(tree6, 25)
  
  assert_true(avl_tree_contains(tree7, 10))
  assert_true(avl_tree_contains(tree7, 20))
  assert_true(avl_tree_contains(tree7, 25))
  assert_true(avl_tree_contains(tree7, 30))
  assert_true(avl_tree_contains(tree7, 40))
  assert_true(avl_tree_contains(tree7, 50))
  assert_false(avl_tree_contains(tree7, 15))
}

// Test 2: Advanced Graph Algorithms
test "advanced graph algorithms for telemetry topology" {
  type GraphNode = {
    id: String,
    value: String
  }
  
  type GraphEdge = {
    from: String,
    to: String,
    weight: Float
  }
  
  type Graph = {
    nodes: Array[GraphNode>,
    edges: Array[GraphEdge>
  }
  
  let graph_new = fn() -> Graph {
    { nodes: [], edges: [] }
  }
  
  let graph_add_node = fn(graph: Graph, id: String, value: String) -> Graph {
    { nodes: graph.nodes + [{ id, value }], edges: graph.edges }
  }
  
  let graph_add_edge = fn(graph: Graph, from: String, to: String, weight: Float) -> Graph {
    { nodes: graph.nodes, edges: graph.edges + [{ from, to, weight }] }
  }
  
  let graph_get_neighbors = fn(graph: Graph, node_id: String) -> Array[String] {
    let mut neighbors = []
    
    for edge in graph.edges {
      if edge.from == node_id {
        neighbors = neighbors + [edge.to]
      }
    }
    
    neighbors
  }
  
  // Dijkstra's algorithm
  let dijkstra = fn(graph: Graph, start: String, end: String) -> (Float, Array[String]) {
    let mut distances = {}
    let mut previous = {}
    let mut unvisited = []
    
    // Initialize distances
    for node in graph.nodes {
      distances = distances.with(node.id, Float::infinity())
      previous = previous.with(node.id, "")
      unvisited = unvisited + [node.id]
    }
    
    distances = distances.with(start, 0.0)
    
    while unvisited.length() > 0 {
      // Find unvisited node with minimum distance
      let mut min_distance = Float::infinity()
      let mut current = ""
      
      for node_id in unvisited {
        if distances[node_id] < min_distance {
          min_distance = distances[node_id]
          current = node_id
        }
      }
      
      if current == "" || current == end {
        break
      }
      
      // Remove current from unvisited
      let mut new_unvisited = []
      for node_id in unvisited {
        if node_id != current {
          new_unvisited = new_unvisited + [node_id]
        }
      }
      unvisited = new_unvisited
      
      // Update distances to neighbors
      let neighbors = graph_get_neighbors(graph, current)
      
      for neighbor in neighbors {
        let mut edge_weight = Float::infinity()
        
        for edge in graph.edges {
          if edge.from == current && edge.to == neighbor {
            edge_weight = edge.weight
            break
          }
        }
        
        let alt = distances[current] + edge_weight
        
        if alt < distances[neighbor] {
          distances = distances.with(neighbor, alt)
          previous = previous.with(neighbor, current)
        }
      }
    }
    
    // Reconstruct path
    let mut path = []
    let mut current = end
    
    while current != "" {
      path = [current] + path
      current = previous[current]
    }
    
    (distances[end], path)
  }
  
  // Test graph algorithms
  let graph1 = graph_new()
  let graph2 = graph_add_node(graph1, "A", "Service A")
  let graph3 = graph_add_node(graph2, "B", "Service B")
  let graph4 = graph_add_node(graph3, "C", "Service C")
  let graph5 = graph_add_node(graph4, "D", "Service D")
  let graph6 = graph_add_edge(graph5, "A", "B", 10.0)
  let graph7 = graph_add_edge(graph6, "A", "C", 5.0)
  let graph8 = graph_add_edge(graph7, "B", "D", 1.0)
  let graph9 = graph_add_edge(graph8, "C", "D", 20.0)
  let graph10 = graph_add_edge(graph9, "B", "C", 2.0)
  
  // Test neighbors
  let neighbors_a = graph_get_neighbors(graph10, "A")
  assert_eq(neighbors_a.length(), 2)
  assert_true(neighbors_a.contains("B"))
  assert_true(neighbors_a.contains("C"))
  
  // Test Dijkstra's algorithm
  let (distance_a_d, path_a_d) = dijkstra(graph10, "A", "D")
  assert_eq(distance_a_d, 13.0)  // A -> B (10) + B -> D (1) + B -> C (2) is not shorter than A -> C (5) + C -> D (20)
  assert_eq(path_a_d, ["A", "B", "D"])
  
  let (distance_a_c, path_a_c) = dijkstra(graph10, "A", "C")
  assert_eq(distance_a_c, 7.0)  // A -> B (10) + B -> C (2) is shorter than A -> C (5)
  assert_eq(path_a_c, ["A", "B", "C"])
}

// Test 3: Advanced Hash Tables
test "advanced hash tables for telemetry metrics" {
  type HashEntry[K, V] = {
    key: K,
    value: V,
    next: Option[HashEntry[K, V>>
  }
  
  type HashTable[K, V] = {
    buckets: Array[Option[HashEntry[K, V>>>,
    size: Int,
    hash_fn: (K) -> Int
  }
  
  let hash_table_new = fn[K, V](size: Int, hash_fn: (K) -> Int) -> HashTable[K, V] {
    let mut buckets = []
    for i in 0..=size-1 {
      buckets = buckets + [None]
    }
    
    { buckets, size, hash_fn }
  }
  
  let hash_table_put = fn[K: Eq, V](table: HashTable[K, V], key: K, value: V) -> HashTable[K, V] {
    let index = table.hash_fn(key) % table.size
    
    // Check if key already exists
    let mut current = table.buckets[index]
    let mut found = false
    
    match current {
      Some(entry) => {
        if entry.key == key {
          // Update existing entry
          let new_entry = { key, value, next: entry.next }
          let new_buckets = table.buckets.with(index, Some(new_entry))
          return { buckets: new_buckets, size: table.size, hash_fn: table.hash_fn }
        } else {
          // Search in chain
          let mut new_entry = entry
          let mut prev = None
          
          while true {
            if new_entry.key == key {
              // Update existing entry
              let updated = { key, value, next: new_entry.next }
              
              match prev {
                Some(p) => {
                  let new_prev = { key: p.key, value: p.value, next: Some(updated) }
                  // This is simplified - in a real implementation, we'd need to update the chain properly
                  return table
                }
                None => {
                  let new_buckets = table.buckets.with(index, Some(updated))
                  return { buckets: new_buckets, size: table.size, hash_fn: table.hash_fn }
                }
              }
            }
            
            match new_entry.next {
              Some(next) => {
                prev = Some(new_entry)
                new_entry = next
              }
              None => break
            }
          }
          
          // Add to end of chain
          let new_entry = { key, value, next: None }
          let updated = { key: entry.key, value: entry.value, next: Some(new_entry) }
          let new_buckets = table.buckets.with(index, Some(updated))
          return { buckets: new_buckets, size: table.size, hash_fn: table.hash_fn }
        }
      }
      None => ()
    }
    
    // Add new entry
    let new_entry = { key, value, next: None }
    let new_buckets = table.buckets.with(index, Some(new_entry))
    { buckets: new_buckets, size: table.size, hash_fn: table.hash_fn }
  }
  
  let hash_table_get = fn[K: Eq, V](table: HashTable[K, V], key: K) -> Option[V] {
    let index = table.hash_fn(key) % table.size
    
    match table.buckets[index] {
      Some(entry) => {
        let mut current = entry
        
        while true {
          if current.key == key {
            return Some(current.value)
          }
          
          match current.next {
            Some(next) => current = next
            None => return None
          }
        }
      }
      None => None
    }
  }
  
  // Test hash table
  let string_hash = fn(s: String) -> Int {
    let mut hash = 0
    for char in s.to_char_array() {
      hash = hash * 31 + char.to_int()
    }
    if hash < 0 { -hash } else { hash }
  }
  
  let table1 = hash_table_new(10, string_hash)
  let table2 = hash_table_put(table1, "counter1", 42)
  let table3 = hash_table_put(table2, "counter2", 100)
  let table4 = hash_table_put(table3, "gauge1", 3.14)
  
  // Test get
  match hash_table_get(table4, "counter1") {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  match hash_table_get(table4, "counter2") {
    Some(value) => assert_eq(value, 100)
    None => assert_true(false)
  }
  
  match hash_table_get(table4, "gauge1") {
    Some(value) => assert_eq(value, 3.14)
    None => assert_true(false)
  }
  
  match hash_table_get(table4, "nonexistent") {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 4: Advanced Priority Queue
test "advanced priority queue for telemetry events" {
  type PriorityQueueItem[T] = {
    value: T,
    priority: Int
  }
  
  type PriorityQueue[T] = {
    heap: Array[PriorityQueueItem[T>>
  }
  
  let priority_queue_new = fn[T]() -> PriorityQueue[T] {
    { heap: [] }
  }
  
  let priority_queue_parent = fn(index: Int) -> Int {
    (index - 1) / 2
  }
  
  let priority_queue_left_child = fn(index: Int) -> Int {
    2 * index + 1
  }
  
  let priority_queue_right_child = fn(index: Int) -> Int {
    2 * index + 2
  }
  
  let priority_queue_swap = fn[T](heap: Array[PriorityQueueItem[T>>, i: Int, j: Int) -> Array[PriorityQueueItem[T>> {
    let temp = heap[i]
    heap.with(i, heap[j]).with(j, temp)
  }
  
  let priority_queue_heapify_up = fn[T](heap: Array[PriorityQueueItem[T>>, index: Int) -> Array[PriorityQueueItem[T>> {
    if index == 0 {
      return heap
    }
    
    let parent_index = priority_queue_parent(index)
    
    if heap[parent_index].priority > heap[index].priority {
      let new_heap = priority_queue_swap(heap, parent_index, index)
      priority_queue_heapify_up(new_heap, parent_index)
    } else {
      heap
    }
  }
  
  let priority_queue_heapify_down = fn[T](heap: Array[PriorityQueueItem[T>>, index: Int) -> Array[PriorityQueueItem[T>> {
    let left_index = priority_queue_left_child(index)
    let right_index = priority_queue_right_child(index)
    let mut smallest = index
    
    if left_index < heap.length() && heap[left_index].priority < heap[smallest].priority {
      smallest = left_index
    }
    
    if right_index < heap.length() && heap[right_index].priority < heap[smallest].priority {
      smallest = right_index
    }
    
    if smallest != index {
      let new_heap = priority_queue_swap(heap, index, smallest)
      priority_queue_heapify_down(new_heap, smallest)
    } else {
      heap
    }
  }
  
  let priority_queue_enqueue = fn[T](queue: PriorityQueue[T], value: T, priority: Int) -> PriorityQueue[T] {
    let item = { value, priority }
    let new_heap = queue.heap + [item]
    let heapified = priority_queue_heapify_up(new_heap, new_heap.length() - 1)
    { heap: heapified }
  }
  
  let priority_queue_dequeue = fn[T](queue: PriorityQueue[T]) -> (PriorityQueue[T], Option[PriorityQueueItem[T>>) {
    if queue.heap.length() == 0 {
      return (queue, None)
    }
    
    if queue.heap.length() == 1 {
      return ({ heap: [] }, Some(queue.heap[0]))
    }
    
    let root = queue.heap[0]
    let last = queue.heap[queue.heap.length() - 1]
    
    let new_heap = queue.heap.with(0, last).slice(0, queue.heap.length() - 1)
    let heapified = priority_queue_heapify_down(new_heap, 0)
    
    ({ heap: heapified }, Some(root))
  }
  
  let priority_queue_peek = fn[T](queue: PriorityQueue[T]) -> Option[PriorityQueueItem[T>> {
    if queue.heap.length() == 0 {
      None
    } else {
      Some(queue.heap[0])
    }
  }
  
  // Test priority queue
  let queue1 = priority_queue_new()
  let queue2 = priority_queue_enqueue(queue1, "low", 3)
  let queue3 = priority_queue_enqueue(queue2, "high", 1)
  let queue4 = priority_queue_enqueue(queue3, "medium", 2)
  let queue5 = priority_queue_enqueue(queue4, "highest", 0)
  
  // Test peek
  match priority_queue_peek(queue5) {
    Some(item) => {
      assert_eq(item.value, "highest")
      assert_eq(item.priority, 0)
    }
    None => assert_true(false)
  }
  
  // Test dequeue
  let (queue6, item1) = priority_queue_dequeue(queue5)
  match item1 {
    Some(item) => {
      assert_eq(item.value, "highest")
      assert_eq(item.priority, 0)
    }
    None => assert_true(false)
  }
  
  let (queue7, item2) = priority_queue_dequeue(queue6)
  match item2 {
    Some(item) => {
      assert_eq(item.value, "high")
      assert_eq(item.priority, 1)
    }
    None => assert_true(false)
  }
  
  let (queue8, item3) = priority_queue_dequeue(queue7)
  match item3 {
    Some(item) => {
      assert_eq(item.value, "medium")
      assert_eq(item.priority, 2)
    }
    None => assert_true(false)
  }
  
  let (queue9, item4) = priority_queue_dequeue(queue8)
  match item4 {
    Some(item) => {
      assert_eq(item.value, "low")
      assert_eq(item.priority, 3)
    }
    None => assert_true(false)
  }
  
  // Queue should be empty now
  let (queue10, item5) = priority_queue_dequeue(queue9)
  match item5 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 5: Advanced Bloom Filter
test "advanced bloom filter for telemetry deduplication" {
  type BloomFilter = {
    bitset: Array[Bool],
    hash_functions: Array<(String) -> Int>
  }
  
  let bloom_filter_new = fn(size: Int, hash_functions: Array[(String) -> Int>) -> BloomFilter {
    let mut bitset = []
    for i in 0..=size-1 {
      bitset = bitset + [false]
    }
    
    { bitset, hash_functions }
  }
  
  let bloom_filter_add = fn(filter: BloomFilter, item: String) -> BloomFilter {
    let mut new_bitset = filter.bitset
    
    for hash_fn in filter.hash_functions {
      let hash = hash_fn(item) % filter.bitset.length()
      new_bitset = new_bitset.with(hash, true)
    }
    
    { bitset: new_bitset, hash_functions: filter.hash_functions }
  }
  
  let bloom_filter_contains = fn(filter: BloomFilter, item: String) -> Bool {
    for hash_fn in filter.hash_functions {
      let hash = hash_fn(item) % filter.bitset.length()
      if !filter.bitset[hash] {
        return false
      }
    }
    true
  }
  
  // Simple hash functions
  let hash1 = fn(s: String) -> Int {
    let mut hash = 0
    for char in s.to_char_array() {
      hash = hash * 31 + char.to_int()
    }
    if hash < 0 { -hash } else { hash }
  }
  
  let hash2 = fn(s: String) -> Int {
    let mut hash = 0
    for char in s.to_char_array() {
      hash = hash * 17 + char.to_int()
    }
    if hash < 0 { -hash } else { hash }
  }
  
  let hash3 = fn(s: String) -> Int {
    let mut hash = 0
    for char in s.to_char_array() {
      hash = hash * 13 + char.to_int()
    }
    if hash < 0 { -hash } else { hash }
  }
  
  // Test bloom filter
  let filter1 = bloom_filter_new(100, [hash1, hash2, hash3])
  let filter2 = bloom_filter_add(filter1, "trace1")
  let filter3 = bloom_filter_add(filter2, "trace2")
  let filter4 = bloom_filter_add(filter3, "trace3")
  
  // Test contains
  assert_true(bloom_filter_contains(filter4, "trace1"))
  assert_true(bloom_filter_contains(filter4, "trace2"))
  assert_true(bloom_filter_contains(filter4, "trace3"))
  
  // Test non-existent item
  assert_false(bloom_filter_contains(filter4, "trace4"))
  
  // Test false positive possibility
  let filter5 = bloom_filter_add(filter4, "trace4")
  assert_true(bloom_filter_contains(filter5, "trace4"))
  
  // May have false positives for non-existent items
  let result = bloom_filter_contains(filter5, "trace5")
  // We can't assert a specific value here since it depends on hash collisions
}

// Test 6: Advanced Trie
test "advanced trie for telemetry string matching" {
  type TrieNode = {
    children: Array[(Char, TrieNode)>,
    is_end: Bool,
    value: Option[String>
  }
  
  let trie_node_new = fn() -> TrieNode {
    { children: [], is_end: false, value: None }
  }
  
  let trie_insert = fn(node: TrieNode, word: String, value: String) -> TrieNode {
    let mut current = node
    
    for char in word.to_char_array() {
      let mut found = false
      
      for i in 0..=current.children.length()-1 {
        if current.children[i].0 == char {
          current = current.children[i].1
          found = true
          break
        }
      }
      
      if !found {
        let new_node = trie_node_new()
        current = { children: current.children + [(char, new_node)], is_end: current.is_end, value: current.value }
      }
    }
    
    { children: current.children, is_end: true, value: Some(value) }
  }
  
  let trie_search = fn(node: TrieNode, word: String) -> Option[String> {
    let mut current = node
    
    for char in word.to_char_array() {
      let mut found = false
      
      for i in 0..=current.children.length()-1 {
        if current.children[i].0 == char {
          current = current.children[i].1
          found = true
          break
        }
      }
      
      if !found {
        return None
      }
    }
    
    if current.is_end {
      current.value
    } else {
      None
    }
  }
  
  let trie_starts_with = fn(node: TrieNode, prefix: String) -> Bool {
    let mut current = node
    
    for char in prefix.to_char_array() {
      let mut found = false
      
      for i in 0..=current.children.length()-1 {
        if current.children[i].0 == char {
          current = current.children[i].1
          found = true
          break
        }
      }
      
      if !found {
        return false
      }
    }
    
    true
  }
  
  // Test trie
  let trie1 = trie_node_new()
  let trie2 = trie_insert(trie1, "trace", "Trace data")
  let trie3 = trie_insert(trie2, "track", "Track data")
  let trie4 = trie_insert(trie3, "tracing", "Tracing data")
  let trie5 = trie_insert(trie4, "metric", "Metric data")
  
  // Test search
  match trie_search(trie5, "trace") {
    Some(value) => assert_eq(value, "Trace data")
    None => assert_true(false)
  }
  
  match trie_search(trie5, "track") {
    Some(value) => assert_eq(value, "Track data")
    None => assert_true(false)
  }
  
  match trie_search(trie5, "tracing") {
    Some(value) => assert_eq(value, "Tracing data")
    None => assert_true(false)
  }
  
  match trie_search(trie5, "metric") {
    Some(value) => assert_eq(value, "Metric data")
    None => assert_true(false)
  }
  
  match trie_search(trie5, "tra") {
    Some(_) => assert_true(false)  // Not a complete word
    None => assert_true(true)
  }
  
  // Test starts with
  assert_true(trie_starts_with(trie5, "tr"))
  assert_true(trie_starts_with(trie5, "tra"))
  assert_true(trie_starts_with(trie5, "trac"))
  assert_false(trie_starts_with(trie5, "tre"))
  assert_true(trie_starts_with(trie5, "met"))
  assert_false(trie_starts_with(trie5, "metr"))
}

// Test 7: Advanced Skip List
test "advanced skip list for telemetry data indexing" {
  type SkipListNode = {
    value: String,
    next: Array[Option[SkipListNode>>
  }
  
  type SkipList = {
    head: SkipListNode,
    max_level: Int,
    probability: Float
  }
  
  let skip_list_new = fn(max_level: Int, probability: Float) -> SkipList {
    let head = {
      value: "",
      next: []
    }
    
    let mut new_head = head
    for i in 0..=max_level-1 {
      new_head = { value: "", next: new_head.next + [None] }
    }
    
    { head: new_head, max_level, probability }
  }
  
  let random_level = fn(max_level: Int, probability: Float) -> Int {
    let mut level = 0
    let mut random = 0.5  // Simplified random value
    
    while level < max_level - 1 && random < probability {
      level = level + 1
      random = 0.5  // Simplified random value
    }
    
    level
  }
  
  let skip_list_insert = fn(list: SkipList, value: String) -> SkipList {
    let level = random_level(list.max_level, list.probability)
    let mut new_node = { value, next: [] }
    
    for i in 0..=level {
      new_node = { value, next: new_node.next + [None] }
    }
    
    // Find insertion points
    let mut current = list.head
    let mut update = []
    
    for i in 0..=list.max_level-1 {
      update = update + [None]
    }
    
    for i in list.max_level-1 ..=0 {
      while true {
        match current.next[i] {
          Some(node) => {
            if node.value < value {
              current = node
            } else {
              break
            }
          }
          None => break
        }
      }
      update = update.with(i, Some(current))
    }
    
    // Insert node
    for i in 0..=level {
      match update[i] {
        Some(node) => {
          new_node = { value, next: new_node.next.with(i, node.next[i]) }
          let updated_next = node.next.with(i, Some(new_node))
          current = { value: current.value, next: updated_next }
        }
        None => ()
      }
    }
    
    list
  }
  
  let skip_list_contains = fn(list: SkipList, value: String) -> Bool {
    let mut current = list.head
    
    for i in list.max_level-1 ..=0 {
      while true {
        match current.next[i] {
          Some(node) => {
            if node.value < value {
              current = node
            } else {
              break
            }
          }
          None => break
        }
      }
    }
    
    match current.next[0] {
      Some(node) => node.value == value
      None => false
    }
  }
  
  // Test skip list
  let list1 = skip_list_new(4, 0.5)
  let list2 = skip_list_insert(list1, "trace1")
  let list3 = skip_list_insert(list2, "trace2")
  let list4 = skip_list_insert(list3, "trace3")
  
  // Test contains
  assert_true(skip_list_contains(list4, "trace1"))
  assert_true(skip_list_contains(list4, "trace2"))
  assert_true(skip_list_contains(list4, "trace3"))
  assert_false(skip_list_contains(list4, "trace4"))
}

// Test 8: Advanced LRU Cache
test "advanced lru cache for telemetry data" {
  type LRUNode[K, V] = {
    key: K,
    value: V,
    prev: Option[LRUNode[K, V>>,
    next: Option[LRUNode[K, V>>
  }
  
  type LRUCache[K, V] = {
    capacity: Int,
    cache: Array[(K, LRUNode[K, V>)>,
    head: Option[LRUNode[K, V>>,
    tail: Option[LRUNode[K, V>>
  }
  
  let lru_cache_new = fn[K, V](capacity: Int) -> LRUCache[K, V] {
    { capacity, cache: [], head: None, tail: None }
  }
  
  let lru_cache_remove_node = fn[K, V](cache: LRUCache[K, V], node: LRUNode[K, V]) -> LRUCache[K, V] {
    // Update prev and next pointers
    match node.prev {
      Some(prev) => {
        let updated_prev = { key: prev.key, value: prev.value, prev: prev.prev, next: node.next }
        match node.next {
          Some(next) => {
            let updated_next = { key: next.key, value: next.value, prev: Some(updated_prev), next: next.next }
            {
              capacity: cache.capacity,
              cache: cache.cache,
              head: cache.head,
              tail: cache.tail
            }
          }
          None => {
            {
              capacity: cache.capacity,
              cache: cache.cache,
              head: cache.head,
              tail: Some(updated_prev)
            }
          }
        }
      }
      None => {
        match node.next {
          Some(next) => {
            let updated_next = { key: next.key, value: next.value, prev: None, next: next.next }
            {
              capacity: cache.capacity,
              cache: cache.cache,
              head: Some(updated_next),
              tail: cache.tail
            }
          }
          None => {
            {
              capacity: cache.capacity,
              cache: cache.cache,
              head: None,
              tail: None
            }
          }
        }
      }
    }
  }
  
  let lru_cache_add_to_head = fn[K, V](cache: LRUCache[K, V], node: LRUNode[K, V]) -> LRUCache[K, V] {
    let new_node = { key: node.key, value: node.value, prev: None, next: cache.head }
    
    match cache.head {
      Some(head) => {
        let updated_head = { key: head.key, value: head.value, prev: Some(new_node), next: head.next }
        {
          capacity: cache.capacity,
          cache: cache.cache,
          head: Some(new_node),
          tail: cache.tail
        }
      }
      None => {
        {
          capacity: cache.capacity,
          cache: cache.cache,
          head: Some(new_node),
          tail: Some(new_node)
        }
      }
    }
  }
  
  let lru_cache_move_to_head = fn[K, V](cache: LRUCache[K, V], node: LRUNode[K, V]) -> LRUCache[K, V] {
    let cache_without_node = lru_cache_remove_node(cache, node)
    lru_cache_add_to_head(cache_without_node, node)
  }
  
  let lru_cache_get = fn[K: Eq, V](cache: LRUCache[K, V], key: K) -> (LRUCache[K, V], Option[V]) {
    // Find node in cache
    for i in 0..=cache.cache.length()-1 {
      if cache.cache[i].0 == key {
        let node = cache.cache[i].1
        let new_cache = lru_cache_move_to_head(cache, node)
        return (new_cache, Some(node.value))
      }
    }
    
    (cache, None)
  }
  
  let lru_cache_put = fn[K: Eq, V](cache: LRUCache[K, V], key: K, value: V) -> LRUCache[K, V] {
    // Check if key already exists
    for i in 0..=cache.cache.length()-1 {
      if cache.cache[i].0 == key {
        let node = cache.cache[i].1
        let updated_node = { key, value, prev: node.prev, next: node.next }
        let new_cache = lru_cache_move_to_head(cache, updated_node)
        
        // Update cache entry
        let mut new_cache_entries = new_cache.cache
        new_cache_entries = new_cache_entries.with(i, (key, updated_node))
        
        return {
          capacity: new_cache.capacity,
          cache: new_cache_entries,
          head: new_cache.head,
          tail: new_cache.tail
        }
      }
    }
    
    // Add new node
    let new_node = { key, value, prev: None, next: None }
    
    if cache.cache.length() >= cache.capacity {
      // Remove tail
      match cache.tail {
        Some(tail) => {
          let cache_without_tail = lru_cache_remove_node(cache, tail)
          
          // Remove from cache entries
          let mut new_cache_entries = []
          for i in 0..=cache_without_tail.cache.length()-1 {
            if cache_without_tail.cache[i].0 != tail.key {
              new_cache_entries = new_cache_entries + [cache_without_tail.cache[i]]
            }
          }
          
          let cache_with_new = lru_cache_add_to_head(cache_without_tail, new_node)
          
          {
            capacity: cache_with_new.capacity,
            cache: new_cache_entries + [(key, new_node)],
            head: cache_with_new.head,
            tail: cache_with_new.tail
          }
        }
        None => {
          let cache_with_new = lru_cache_add_to_head(cache, new_node)
          {
            capacity: cache_with_new.capacity,
            cache: cache_with_new.cache + [(key, new_node)],
            head: cache_with_new.head,
            tail: cache_with_new.tail
          }
        }
      }
    } else {
      let cache_with_new = lru_cache_add_to_head(cache, new_node)
      {
        capacity: cache_with_new.capacity,
        cache: cache_with_new.cache + [(key, new_node)],
        head: cache_with_new.head,
        tail: cache_with_new.tail
      }
    }
  }
  
  // Test LRU cache
  let cache1 = lru_cache_new(2)
  let cache2 = lru_cache_put(cache1, "key1", "value1")
  let cache3 = lru_cache_put(cache2, "key2", "value2")
  
  // Test get
  let (cache4, value1) = lru_cache_get(cache3, "key1")
  match value1 {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // Add new item, should evict key2
  let cache5 = lru_cache_put(cache4, "key3", "value3")
  
  // key1 should still be in cache (recently accessed)
  let (cache6, value1_again) = lru_cache_get(cache5, "key1")
  match value1_again {
    Some(v) => assert_eq(v, "value1")
    None => assert_true(false)
  }
  
  // key2 should be evicted
  let (cache7, value2) = lru_cache_get(cache6, "key2")
  match value2 {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 9: Advanced Search Algorithms
test "advanced search algorithms for telemetry data" {
  // Binary search
  let binary_search = fn(arr: Array[Int], target: Int) -> Int {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = left + (right - left) / 2
      
      if arr[mid] == target {
        return mid
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    -1  // Not found
  }
  
  // Interpolation search
  let interpolation_search = fn(arr: Array[Int], target: Int) -> Int {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right && target >= arr[left] && target <= arr[right] {
      if left == right {
        if arr[left] == target {
          return left
        } else {
          break
        }
      }
      
      // Estimate position
      let pos = left + ((target - arr[left]) * (right - left)) / (arr[right] - arr[left])
      
      if arr[pos] == target {
        return pos
      } else if arr[pos] < target {
        left = pos + 1
      } else {
        right = pos - 1
      }
    }
    
    -1  // Not found
  }
  
  // Exponential search
  let exponential_search = fn(arr: Array[Int], target: Int) -> Int {
    if arr.length() == 0 {
      return -1
    }
    
    if arr[0] == target {
      return 0
    }
    
    let mut i = 1
    while i < arr.length() && arr[i] <= target {
      i = i * 2
    }
    
    // Binary search in the found range
    let left = i / 2
    let right = if i < arr.length() { i } else { arr.length() - 1 }
    
    let mut left_search = left
    let mut right_search = right
    
    while left_search <= right_search {
      let mid = left_search + (right_search - left_search) / 2
      
      if arr[mid] == target {
        return mid
      } else if arr[mid] < target {
        left_search = mid + 1
      } else {
        right_search = mid - 1
      }
    }
    
    -1  // Not found
  }
  
  // Test search algorithms
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  // Test binary search
  assert_eq(binary_search(sorted_array, 7), 3)
  assert_eq(binary_search(sorted_array, 1), 0)
  assert_eq(binary_search(sorted_array, 19), 9)
  assert_eq(binary_search(sorted_array, 8), -1)  // Not found
  
  // Test interpolation search
  assert_eq(interpolation_search(sorted_array, 7), 3)
  assert_eq(interpolation_search(sorted_array, 1), 0)
  assert_eq(interpolation_search(sorted_array, 19), 9)
  assert_eq(interpolation_search(sorted_array, 8), -1)  // Not found
  
  // Test exponential search
  assert_eq(exponential_search(sorted_array, 7), 3)
  assert_eq(exponential_search(sorted_array, 1), 0)
  assert_eq(exponential_search(sorted_array, 19), 9)
  assert_eq(exponential_search(sorted_array, 8), -1)  // Not found
}

// Test 10: Advanced Sorting Algorithms
test "advanced sorting algorithms for telemetry data" {
  // Merge sort
  let merge_sort = fn(arr: Array[Int]) -> Array[Int] {
    if arr.length() <= 1 {
      return arr
    }
    
    let mid = arr.length() / 2
    let left = merge_sort(arr.slice(0, mid))
    let right = merge_sort(arr.slice(mid, arr.length()))
    
    let mut result = []
    let mut i = 0
    let mut j = 0
    
    while i < left.length() && j < right.length() {
      if left[i] <= right[j] {
        result = result + [left[i]]
        i = i + 1
      } else {
        result = result + [right[j]]
        j = j + 1
      }
    }
    
    while i < left.length() {
      result = result + [left[i]]
      i = i + 1
    }
    
    while j < right.length() {
      result = result + [right[j]]
      j = j + 1
    }
    
    result
  }
  
  // Quick sort
  let quick_sort = fn(arr: Array[Int]) -> Array[Int] {
    if arr.length() <= 1 {
      return arr
    }
    
    let pivot = arr[arr.length() / 2]
    let mut left = []
    let mut middle = []
    let mut right = []
    
    for item in arr {
      if item < pivot {
        left = left + [item]
      } else if item == pivot {
        middle = middle + [item]
      } else {
        right = right + [item]
      }
    }
    
    quick_sort(left) + middle + quick_sort(right)
  }
  
  // Heap sort
  let heap_sort = fn(arr: Array[Int]) -> Array[Int] {
    let mut heap = arr
    let n = heap.length()
    
    // Build max heap
    for i in n/2-1 ..=0 {
      let mut current = i
      
      while true {
        let left = 2 * current + 1
        let right = 2 * current + 2
        let mut largest = current
        
        if left < n && heap[left] > heap[largest] {
          largest = left
        }
        
        if right < n && heap[right] > heap[largest] {
          largest = right
        }
        
        if largest != current {
          let temp = heap[current]
          heap = heap.with(current, heap[largest]).with(largest, temp)
          current = largest
        } else {
          break
        }
      }
    }
    
    // Extract elements from heap
    let mut result = []
    
    for i in n-1 ..=0 {
      let temp = heap[0]
      heap = heap.with(0, heap[i]).with(i, temp)
      
      result = [temp] + result
      
      // Heapify root element
      let heap_size = i
      let mut current = 0
      
      while true {
        let left = 2 * current + 1
        let right = 2 * current + 2
        let mut largest = current
        
        if left < heap_size && heap[left] > heap[largest] {
          largest = left
        }
        
        if right < heap_size && heap[right] > heap[largest] {
          largest = right
        }
        
        if largest != current {
          let temp = heap[current]
          heap = heap.with(current, heap[largest]).with(largest, temp)
          current = largest
        } else {
          break
        }
      }
    }
    
    result
  }
  
  // Test sorting algorithms
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6]
  let expected = [1, 2, 3, 4, 5, 6, 7, 8, 9]
  
  // Test merge sort
  let merge_sorted = merge_sort(unsorted)
  assert_eq(merge_sorted, expected)
  
  // Test quick sort
  let quick_sorted = quick_sort(unsorted)
  assert_eq(quick_sorted, expected)
  
  // Test heap sort
  let heap_sorted = heap_sort(unsorted)
  assert_eq(heap_sorted, expected)
}