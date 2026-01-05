// Azimuth Premium Advanced Data Structures Test Suite
// This file contains advanced data structure test cases for telemetry systems

// Test 1: Tree Structure Operations
test "tree structure operations for telemetry hierarchy" {
  // Define a tree node for telemetry span hierarchy
  type TreeNode = {
    id: String,
    data: String,
    children: Array[TreeNode]
  }
  
  // Create a tree constructor
  let create_node = fn(id: String, data: String) {
    {
      id,
      data,
      children: []
    }
  }
  
  // Create a tree add child function
  let add_child = fn(parent: TreeNode, child: TreeNode) {
    {
      parent |
      children: parent.children.push(child)
    }
  }
  
  // Create a tree traversal function
  let traverse_tree = fn(node: TreeNode, accumulator: Array[String]) {
    let mut result = accumulator.push(node.id)
    for child in node.children {
      result = traverse_tree(child, result)
    }
    result
  }
  
  // Build a telemetry span tree
  let root = create_node("root", "main_operation")
  let child1 = create_node("child1", "database_query")
  let child2 = create_node("child2", "cache_lookup")
  let grandchild1 = create_node("grandchild1", "sql_select")
  let grandchild2 = create_node("grandchild2", "index_scan")
  
  // Assemble the tree
  let with_child1 = add_child(root, child1)
  let with_child2 = add_child(with_child1, child2)
  let with_grandchild1 = add_child(add_child(with_child2.children[0], grandchild1), grandchild2)
  
  // Test tree traversal
  let all_nodes = traverse_tree(root, [])
  assert_eq(all_nodes.length(), 5)
  assert_true(all_nodes.contains("root"))
  assert_true(all_nodes.contains("child1"))
  assert_true(all_nodes.contains("child2"))
  assert_true(all_nodes.contains("grandchild1"))
  assert_true(all_nodes.contains("grandchild2"))
  
  // Test tree depth calculation
  let calculate_depth = fn(node: TreeNode) {
    if node.children.length() == 0 {
      1
    } else {
      let mut max_child_depth = 0
      for child in node.children {
        let child_depth = calculate_depth(child)
        if child_depth > max_child_depth {
          max_child_depth = child_depth
        }
      }
      max_child_depth + 1
    }
  }
  
  let tree_depth = calculate_depth(root)
  assert_eq(tree_depth, 3)
  
  // Test tree search
  let find_node = fn(node: TreeNode, target_id: String) {
    if node.id == target_id {
      Some(node)
    } else {
      let mut found = None
      for child in node.children {
        match find_node(child, target_id) {
          Some(n) => {
            found = Some(n)
            break
          }
          None => ()
        }
      }
      found
    }
  }
  
  let found_node = find_node(root, "grandchild1")
  assert_true(found_node.is_some())
  match found_node {
    Some(n) => {
      assert_eq(n.id, "grandchild1")
      assert_eq(n.data, "sql_select")
    }
    None => assert_true(false)
  }
  
  let not_found = find_node(root, "nonexistent")
  assert_true(not_found.is_none())
}

// Test 2: Graph Structure Operations
test "graph structure operations for service dependencies" {
  // Define a graph edge
  type Edge = {
    from: String,
    to: String,
    weight: Float
  }
  
  // Define a graph
  type Graph = {
    nodes: Array[String],
    edges: Array[Edge]
  }
  
  // Create graph constructor
  let create_graph = fn() {
    {
      nodes: [],
      edges: []
    }
  }
  
  // Add node to graph
  let add_node = fn(graph: Graph, node: String) {
    if not(graph.nodes.contains(node)) {
      { graph | nodes: graph.nodes.push(node) }
    } else {
      graph
    }
  }
  
  // Add edge to graph
  let add_edge = fn(graph: Graph, from: String, to: String, weight: Float) {
    let edge = { from, to, weight }
    { graph | edges: graph.edges.push(edge) }
  }
  
  // Build a service dependency graph
  let initial_graph = create_graph()
  
  let with_nodes = initial_graph
    |> add_node("api-gateway")
    |> add_node("user-service")
    |> add_node("payment-service")
    |> add_node("notification-service")
    |> add_node("database")
  
  let complete_graph = with_nodes
    |> add_edge("api-gateway", "user-service", 1.0)
    |> add_edge("api-gateway", "payment-service", 1.0)
    |> add_edge("user-service", "database", 2.0)
    |> add_edge("payment-service", "database", 3.0)
    |> add_edge("payment-service", "notification-service", 1.5)
    |> add_edge("user-service", "notification-service", 0.5)
  
  // Test graph properties
  assert_eq(complete_graph.nodes.length(), 5)
  assert_eq(complete_graph.edges.length(), 6)
  
  // Test node adjacency
  let get_neighbors = fn(graph: Graph, node: String) {
    let mut neighbors = []
    for edge in graph.edges {
      if edge.from == node {
        neighbors = neighbors.push(edge.to)
      }
    }
    neighbors
  }
  
  let api_neighbors = get_neighbors(complete_graph, "api-gateway")
  assert_eq(api_neighbors.length(), 2)
  assert_true(api_neighbors.contains("user-service"))
  assert_true(api_neighbors.contains("payment-service"))
  
  // Test path finding (simplified BFS)
  let find_path = fn(graph: Graph, start: String, end: String) {
    let mut queue = [(start, [start])]
    let mut visited = [start]
    let mut found_path = None
    
    while queue.length() > 0 and found_path.is_none() {
      let (current, path) = queue[0]
      queue = queue.slice(1, queue.length())
      
      let neighbors = get_neighbors(graph, current)
      for neighbor in neighbors {
        if neighbor == end {
          found_path = Some(path.push(neighbor))
          break
        } else if not(visited.contains(neighbor)) {
          visited = visited.push(neighbor)
          queue = queue.push((neighbor, path.push(neighbor)))
        }
      }
    }
    
    found_path
  }
  
  let path1 = find_path(complete_graph, "api-gateway", "database")
  assert_true(path1.is_some())
  match path1 {
    Some(p) => {
      assert_eq(p[0], "api-gateway")
      assert_eq(p[p.length() - 1], "database")
      assert_true(p.length() >= 3)
    }
    None => assert_true(false)
  }
  
  let path2 = find_path(complete_graph, "user-service", "notification-service")
  assert_true(path2.is_some())
  match path2 {
    Some(p) => {
      assert_eq(p[0], "user-service")
      assert_eq(p[p.length() - 1], "notification-service")
    }
    None => assert_true(false)
  }
  
  // Test cycle detection
  let has_cycle = fn(graph: Graph) {
    let mut visited = []
    let mut recursion_stack = []
    
    let check_cycle = fn(node: String) {
      if recursion_stack.contains(node) {
        true
      } else if visited.contains(node) {
        false
      } else {
        visited = visited.push(node)
        recursion_stack = recursion_stack.push(node)
        
        let mut cycle_found = false
        let neighbors = get_neighbors(graph, node)
        for neighbor in neighbors {
          if check_cycle(neighbor) {
            cycle_found = true
            break
          }
        }
        
        recursion_stack = recursion_stack.slice(0, recursion_stack.length() - 1)
        cycle_found
      }
    }
    
    let mut result = false
    for node in graph.nodes {
      if not(visited.contains(node)) and check_cycle(node) {
        result = true
        break
      }
    }
    
    result
  }
  
  // Our current graph should not have cycles
  assert_false(has_cycle(complete_graph))
  
  // Add a cycle and test again
  let cyclic_graph = complete_graph
    |> add_edge("database", "api-gateway", 4.0)
  
  assert_true(has_cycle(cyclic_graph))
}

// Test 3: Priority Queue Operations
test "priority queue operations for telemetry processing" {
  // Define a priority queue item
  type PriorityItem = {
    priority: Int,
    data: String,
    timestamp: Int
  }
  
  // Create priority queue constructor
  let create_priority_queue = fn() {
    []
  }
  
  // Insert item with priority (lower number = higher priority)
  let enqueue = fn(queue: Array[PriorityItem], item: PriorityItem) {
    let mut result = []
    let mut inserted = false
    
    for existing in queue {
      if not(inserted) and item.priority < existing.priority {
        result = result.push(item)
        inserted = true
      }
      result = result.push(existing)
    }
    
    if not(inserted) {
      result = result.push(item)
    }
    
    result
  }
  
  // Remove highest priority item
  let dequeue = fn(queue: Array[PriorityItem]) {
    if queue.length() > 0 {
      (Some(queue[0]), queue.slice(1, queue.length()))
    } else {
      (None, queue)
    }
  }
  
  // Test priority queue operations
  let initial_queue = create_priority_queue()
  
  let queue1 = initial_queue
    |> enqueue({ priority: 3, data: "low_priority_metric", timestamp: 1000 })
    |> enqueue({ priority: 1, data: "high_priority_alert", timestamp: 1001 })
    |> enqueue({ priority: 2, data: "medium_priority_log", timestamp: 1002 })
    |> enqueue({ priority: 5, data: "lowest_priority_span", timestamp: 1003 })
    |> enqueue({ priority: 1, data: "another_high_priority", timestamp: 1004 })
  
  // Test queue ordering
  assert_eq(queue1.length(), 5)
  assert_eq(queue1[0].priority, 1)
  assert_eq(queue1[0].data, "high_priority_alert")
  assert_eq(queue1[1].priority, 1)
  assert_eq(queue1[1].data, "another_high_priority")
  assert_eq(queue1[2].priority, 2)
  assert_eq(queue1[2].data, "medium_priority_log")
  
  // Test dequeue operations
  let (item1, queue2) = dequeue(queue1)
  assert_true(item1.is_some())
  match item1 {
    Some(i) => {
      assert_eq(i.priority, 1)
      assert_eq(i.data, "high_priority_alert")
    }
    None => assert_true(false)
  }
  
  let (item2, queue3) = dequeue(queue2)
  assert_true(item2.is_some())
  match item2 {
    Some(i) => {
      assert_eq(i.priority, 1)
      assert_eq(i.data, "another_high_priority")
    }
    None => assert_true(false)
  }
  
  let (item3, queue4) = dequeue(queue3)
  assert_true(item3.is_some())
  match item3 {
    Some(i) => {
      assert_eq(i.priority, 2)
      assert_eq(i.data, "medium_priority_log")
    }
    None => assert_true(false)
  }
  
  // Test empty queue
  let mut empty_queue = create_priority_queue()
  let (empty_item, _) = dequeue(empty_queue)
  assert_true(empty_item.is_none())
  
  // Test priority queue with same priority items (FIFO order)
  let same_priority_queue = create_priority_queue()
    |> enqueue({ priority: 1, data: "first", timestamp: 1000 })
    |> enqueue({ priority: 1, data: "second", timestamp: 1001 })
    |> enqueue({ priority: 1, data: "third", timestamp: 1002 })
  
  let (first_item, _) = dequeue(same_priority_queue)
  match first_item {
    Some(i) => assert_eq(i.data, "first")
    None => assert_true(false)
  }
}

// Test 4: Trie Structure for String Operations
test "trie structure for telemetry string operations" {
  // Define a trie node
  type TrieNode = {
    children: Map[Char, TrieNode],
    is_end: Bool,
    value: Option[String]
  }
  
  // Create trie node constructor
  let create_trie_node = fn() {
    {
      children: {},
      is_end: false,
      value: None
    }
  }
  
  // Insert string into trie
  let insert = fn(node: TrieNode, key: String, value: String) {
    let chars = key.to_char_array()
    let mut current = node
    
    for i in 0..chars.length() {
      let c = chars[i]
      if not(current.children.contains(c)) {
        current.children = current.children.set(c, create_trie_node())
      }
      current = current.children[c]
    }
    
    current.is_end = true
    current.value = Some(value)
    node
  }
  
  // Search for string in trie
  let search = fn(node: TrieNode, key: String) {
    let chars = key.to_char_array()
    let mut current = node
    
    for i in 0..chars.length() {
      let c = chars[i]
      if not(current.children.contains(c)) {
        return None
      }
      current = current.children[c]
    }
    
    if current.is_end {
      current.value
    } else {
      None
    }
  }
  
  // Test trie operations
  let root = create_trie_node()
  
  // Insert telemetry service names
  let trie_with_services = root
    |> insert("user-service", "User Management Service")
    |> insert("payment-service", "Payment Processing Service")
    |> insert("notification-service", "Notification Service")
    |> insert("auth-service", "Authentication Service")
    |> insert("api-gateway", "API Gateway Service")
  
  // Test search operations
  let user_service = search(trie_with_services, "user-service")
  assert_true(user_service.is_some())
  match user_service {
    Some(v) => assert_eq(v, "User Management Service")
    None => assert_true(false)
  }
  
  let payment_service = search(trie_with_services, "payment-service")
  assert_true(payment_service.is_some())
  match payment_service {
    Some(v) => assert_eq(v, "Payment Processing Service")
    None => assert_true(false)
  }
  
  // Test prefix search
  let starts_with = fn(node: TrieNode, prefix: String) {
    let chars = prefix.to_char_array()
    let mut current = node
    
    for i in 0..chars.length() {
      let c = chars[i]
      if not(current.children.contains(c)) {
        return false
      }
      current = current.children[c]
    }
    
    true
  }
  
  assert_true(starts_with(trie_with_services, "user"))
  assert_true(starts_with(trie_with_services, "payment"))
  assert_false(starts_with(trie_with_services, "order"))
  
  // Test non-existent search
  let not_found = search(trie_with_services, "order-service")
  assert_true(not_found.is_none())
  
  // Test partial search (prefix exists but not complete word)
  let partial = search(trie_with_services, "user")
  assert_true(partial.is_none())
}

// Test 5: Bloom Filter for Telemetry Deduplication
test "bloom filter for telemetry deduplication" {
  // Simplified bloom filter implementation
  type BloomFilter = {
    size: Int,
    bit_array: Array[Bool],
    hash_count: Int
  }
  
  // Create bloom filter constructor
  let create_bloom_filter = fn(size: Int, hash_count: Int) {
    {
      size,
      bit_array: [false; size],
      hash_count
    }
  }
  
  // Simple hash functions for demonstration
  let hash1 = fn(key: String, size: Int) {
    let mut hash = 0
    let chars = key.to_char_array()
    for i in 0..chars.length() {
      hash = (hash * 31 + chars[i].to_int()) % size
    }
    if hash < 0 { hash + size } else { hash }
  }
  
  let hash2 = fn(key: String, size: Int) {
    let mut hash = 5381
    let chars = key.to_char_array()
    for i in 0..chars.length() {
      hash = ((hash << 5) + hash + chars[i].to_int()) % size
    }
    if hash < 0 { hash + size } else { hash }
  }
  
  // Add item to bloom filter
  let add = fn(filter: BloomFilter, key: String) {
    let mut updated_filter = filter
    let h1 = hash1(key, filter.size)
    let h2 = hash2(key, filter.size)
    
    for i in 0..filter.hash_count {
      let hash = (h1 + i * h2) % filter.size
      updated_filter.bit_array[hash] = true
    }
    
    updated_filter
  }
  
  // Check if item might exist in bloom filter
  let might_contain = fn(filter: BloomFilter, key: String) {
    let h1 = hash1(key, filter.size)
    let h2 = hash2(key, filter.size)
    
    for i in 0..filter.hash_count {
      let hash = (h1 + i * h2) % filter.size
      if not(filter.bit_array[hash]) {
        return false
      }
    }
    
    true
  }
  
  // Test bloom filter operations
  let initial_filter = create_bloom_filter(100, 3)
  
  // Add telemetry trace IDs
  let filter_with_traces = initial_filter
    |> add("trace-12345")
    |> add("trace-67890")
    |> add("trace-11111")
    |> add("trace-22222")
  
  // Test positive cases (should return true)
  assert_true(might_contain(filter_with_traces, "trace-12345"))
  assert_true(might_contain(filter_with_traces, "trace-67890"))
  assert_true(might_contain(filter_with_traces, "trace-11111"))
  assert_true(might_contain(filter_with_traces, "trace-22222"))
  
  // Test negative cases (should return false)
  assert_false(might_contain(filter_with_traces, "trace-33333"))
  assert_false(might_contain(filter_with_traces, "trace-44444"))
  assert_false(might_contain(filter_with_traces, "trace-55555"))
  
  // Test false positives (possible with bloom filter)
  let filter_with_more = filter_with_traces
    |> add("span-12345")
    |> add("span-67890")
    |> add("span-11111")
    |> add("span-22222")
    |> add("span-33333")
    |> add("span-44444")
    |> add("span-55555")
    |> add("span-66666")
    |> add("span-77777")
    |> add("span-88888")
  
  // These might return true due to hash collisions
  let maybe_positive1 = might_contain(filter_with_more, "unknown-12345")
  let maybe_positive2 = might_contain(filter_with_more, "unknown-67890")
  
  // We can't predict false positives, but we can verify the filter works correctly
  assert_true(might_contain(filter_with_more, "span-12345"))
  assert_true(might_contain(filter_with_more, "span-88888"))
}

// Test 6: LRU Cache for Telemetry Data
test "lru cache for telemetry data" {
  // Define LRU cache entry
  type CacheEntry = {
    key: String,
    value: String,
    access_time: Int
  }
  
  // Define LRU cache
  type LRUCache = {
    capacity: Int,
    entries: Array[CacheEntry],
    current_time: Int
  }
  
  // Create LRU cache constructor
  let create_lru_cache = fn(capacity: Int) {
    {
      capacity,
      entries: [],
      current_time: 0
    }
  }
  
  // Get value from cache
  let get = fn(cache: LRUCache, key: String) {
    let mut found_index = -1
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_index = i
        break
      }
    }
    
    if found_index >= 0 {
      let entry = cache.entries[found_index]
      let updated_entry = { entry | access_time: cache.current_time }
      let updated_entries = cache.entries.slice(0, found_index) + 
                           [updated_entry] + 
                           cache.entries.slice(found_index + 1, cache.entries.length())
      (Some(entry.value), { cache | entries: updated_entries, current_time: cache.current_time + 1 })
    } else {
      (None, { cache | current_time: cache.current_time + 1 })
    }
  }
  
  // Put value into cache
  let put = fn(cache: LRUCache, key: String, value: String) {
    let new_entry = { key, value, access_time: cache.current_time }
    
    // Check if key already exists
    let mut found_index = -1
    for i in 0..cache.entries.length() {
      if cache.entries[i].key == key {
        found_index = i
        break
      }
    }
    
    if found_index >= 0 {
      // Update existing entry
      let updated_entries = cache.entries.slice(0, found_index) + 
                           [new_entry] + 
                           cache.entries.slice(found_index + 1, cache.entries.length())
      { cache | entries: updated_entries, current_time: cache.current_time + 1 }
    } else if cache.entries.length() < cache.capacity {
      // Add new entry if capacity not reached
      { cache | entries: cache.entries.push(new_entry), current_time: cache.current_time + 1 }
    } else {
      // Remove least recently used entry and add new one
      let mut oldest_index = 0
      let mut oldest_time = cache.entries[0].access_time
      
      for i in 1..cache.entries.length() {
        if cache.entries[i].access_time < oldest_time {
          oldest_time = cache.entries[i].access_time
          oldest_index = i
        }
      }
      
      let updated_entries = cache.entries.slice(0, oldest_index) + 
                           cache.entries.slice(oldest_index + 1, cache.entries.length())
      { cache | entries: updated_entries.push(new_entry), current_time: cache.current_time + 1 }
    }
  }
  
  // Test LRU cache operations
  let initial_cache = create_lru_cache(3)
  
  // Add entries
  let cache1 = initial_cache
    |> put("trace-1", "trace-data-1")
    |> put("trace-2", "trace-data-2")
    |> put("trace-3", "trace-data-3")
  
  // Test retrieval
  let (value1, cache2) = get(cache1, "trace-1")
  assert_true(value1.is_some())
  match value1 {
    Some(v) => assert_eq(v, "trace-data-1")
    None => assert_true(false)
  }
  
  let (value2, cache3) = get(cache2, "trace-2")
  assert_true(value2.is_some())
  match value2 {
    Some(v) => assert_eq(v, "trace-data-2")
    None => assert_true(false)
  }
  
  // Test non-existent key
  let (value3, _) = get(cache3, "trace-999")
  assert_true(value3.is_none())
  
  // Add entry beyond capacity (should evict least recently used)
  let cache4 = put(cache3, "trace-4", "trace-data-4")
  
  // trace-3 should be evicted (least recently used)
  let (evicted_value, _) = get(cache4, "trace-3")
  assert_true(evicted_value.is_none())
  
  // trace-1 and trace-2 should still exist
  let (existing_value1, _) = get(cache4, "trace-1")
  let (existing_value2, _) = get(cache4, "trace-2")
  let (existing_value4, _) = get(cache4, "trace-4")
  
  assert_true(existing_value1.is_some())
  assert_true(existing_value2.is_some())
  assert_true(existing_value4.is_some())
  
  // Test cache update
  let cache5 = put(cache4, "trace-1", "updated-trace-data-1")
  let (updated_value, _) = get(cache5, "trace-1")
  assert_true(updated_value.is_some())
  match updated_value {
    Some(v) => assert_eq(v, "updated-trace-data-1")
    None => assert_true(false)
  }
}

// Test 7: Circular Buffer for Telemetry Streaming
test "circular buffer for telemetry streaming" {
  // Define circular buffer
  type CircularBuffer = {
    capacity: Int,
    buffer: Array[String],
    head: Int,
    tail: Int,
    size: Int
  }
  
  // Create circular buffer constructor
  let create_circular_buffer = fn(capacity: Int) {
    {
      capacity,
      buffer: [""; capacity],
      head: 0,
      tail: 0,
      size: 0
    }
  }
  
  // Check if buffer is empty
  let is_empty = fn(buffer: CircularBuffer) {
    buffer.size == 0
  }
  
  // Check if buffer is full
  let is_full = fn(buffer: CircularBuffer) {
    buffer.size == buffer.capacity
  }
  
  // Add item to buffer
  let enqueue = fn(buffer: CircularBuffer, item: String) {
    if is_full(buffer) {
      // Overwrite oldest data
      let updated_buffer = buffer.buffer.with(buffer.tail, item)
      let new_tail = (buffer.tail + 1) % buffer.capacity
      { buffer | buffer: updated_buffer, tail: new_tail, head: new_tail }
    } else {
      // Add new data
      let updated_buffer = buffer.buffer.with(buffer.head, item)
      let new_head = (buffer.head + 1) % buffer.capacity
      { buffer | buffer: updated_buffer, head: new_head, size: buffer.size + 1 }
    }
  }
  
  // Remove item from buffer
  let dequeue = fn(buffer: CircularBuffer) {
    if is_empty(buffer) {
      (None, buffer)
    } else {
      let item = buffer.buffer[buffer.tail]
      let new_tail = (buffer.tail + 1) % buffer.capacity
      (Some(item), { buffer | tail: new_tail, size: buffer.size - 1 })
    }
  }
  
  // Get all items in buffer (without removing)
  let peek_all = fn(buffer: CircularBuffer) {
    let mut result = []
    let mut current = buffer.tail
    
    for i in 0..buffer.size {
      result = result.push(buffer.buffer[current])
      current = (current + 1) % buffer.capacity
    }
    
    result
  }
  
  // Test circular buffer operations
  let initial_buffer = create_circular_buffer(3)
  
  // Test initial state
  assert_true(is_empty(initial_buffer))
  assert_false(is_full(initial_buffer))
  
  // Add items
  let buffer1 = initial_buffer
    |> enqueue("metric-1")
    |> enqueue("metric-2")
    |> enqueue("metric-3")
  
  // Test full state
  assert_false(is_empty(buffer1))
  assert_true(is_full(buffer1))
  
  // Test peek all
  let all_items = peek_all(buffer1)
  assert_eq(all_items.length(), 3)
  assert_true(all_items.contains("metric-1"))
  assert_true(all_items.contains("metric-2"))
  assert_true(all_items.contains("metric-3"))
  
  // Test dequeue
  let (item1, buffer2) = dequeue(buffer1)
  assert_true(item1.is_some())
  match item1 {
    Some(i) => assert_eq(i, "metric-1")
    None => assert_true(false)
  }
  
  // Test state after dequeue
  assert_false(is_empty(buffer2))
  assert_false(is_full(buffer2))
  
  // Add item to full buffer (should overwrite)
  let buffer3 = buffer2 |> enqueue("metric-4")
  let all_items2 = peek_all(buffer3)
  assert_eq(all_items2.length(), 3)
  assert_true(all_items2.contains("metric-2"))
  assert_true(all_items2.contains("metric-3"))
  assert_true(all_items2.contains("metric-4"))
  assert_false(all_items2.contains("metric-1"))  // Should be overwritten
  
  // Test emptying buffer
  let (item2, buffer4) = dequeue(buffer3)
  let (item3, buffer5) = dequeue(buffer4)
  let (item4, buffer6) = dequeue(buffer5)
  
  assert_true(item2.is_some())
  assert_true(item3.is_some())
  assert_true(item4.is_some())
  
  match item2 { Some(i) => assert_eq(i, "metric-2") None => assert_true(false) }
  match item3 { Some(i) => assert_eq(i, "metric-3") None => assert_true(false) }
  match item4 { Some(i) => assert_eq(i, "metric-4") None => assert_true(false) }
  
  // Test empty state
  assert_true(is_empty(buffer6))
  assert_false(is_full(buffer6))
  
  // Test dequeue from empty buffer
  let (empty_item, _) = dequeue(buffer6)
  assert_true(empty_item.is_none())
}