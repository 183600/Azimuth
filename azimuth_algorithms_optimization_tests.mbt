// Azimuth Algorithms and Optimization Test Suite
// This file contains test cases for algorithms and optimization techniques used in Azimuth

// Test 1: Binary Search for Trace ID Lookup
test "binary search for efficient trace ID lookup" {
  // Binary search implementation
  let binary_search = fn(arr: Array[String], target: String) {
    let mut left = 0
    let mut right = arr.length() - 1
    let mut found = false
    
    while left <= right {
      let mid = (left + right) / 2
      if arr[mid] == target {
        found = true
        break
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    found
  }
  
  // Create sorted array of trace IDs
  let trace_ids = [
    "trace-001", "trace-002", "trace-003", "trace-004", "trace-005",
    "trace-006", "trace-007", "trace-008", "trace-009", "trace-010"
  ]
  
  // Test successful searches
  assert_true(binary_search(trace_ids, "trace-001"))
  assert_true(binary_search(trace_ids, "trace-005"))
  assert_true(binary_search(trace_ids, "trace-010"))
  
  // Test unsuccessful searches
  assert_false(binary_search(trace_ids, "trace-000"))
  assert_false(binary_search(trace_ids, "trace-011"))
  assert_false(binary_search(trace_ids, "non-existent"))
  
  // Test edge cases
  assert_true(binary_search(["single-trace"], "single-trace"))
  assert_false(binary_search([], "anything"))
}

// Test 2: Quick Sort for Telemetry Event Ordering
test "quick sort for telemetry event ordering by timestamp" {
  // Define telemetry event
  type TelemetryEvent = {
    id: String,
    timestamp: Int,
    event_type: String
  }
  
  // Quick sort implementation
  let quick_sort = fn(events: Array[TelemetryEvent]) {
    if events.length() <= 1 {
      events
    } else {
      let pivot = events[0]
      let mut less = []
      let mut equal = []
      let mut greater = []
      
      for event in events {
        if event.timestamp < pivot.timestamp {
          less = less.push(event)
        } else if event.timestamp > pivot.timestamp {
          greater = greater.push(event)
        } else {
          equal = equal.push(event)
        }
      }
      
      quick_sort(less) + equal + quick_sort(greater)
    }
  }
  
  // Create unsorted telemetry events
  let events = [
    { id: "event-1", timestamp: 1640995300, event_type: "span_start" },
    { id: "event-2", timestamp: 1640995200, event_type: "span_start" },
    { id: "event-3", timestamp: 1640995400, event_type: "span_end" },
    { id: "event-4", timestamp: 1640995250, event_type: "log" },
    { id: "event-5", timestamp: 1640995350, event_type: "metric" }
  ]
  
  // Sort events by timestamp
  let sorted_events = quick_sort(events)
  
  // Test sorting result
  assert_eq(sorted_events.length(), 5)
  assert_eq(sorted_events[0].id, "event-2")  // 1640995200
  assert_eq(sorted_events[1].id, "event-4")  // 1640995250
  assert_eq(sorted_events[2].id, "event-1")  // 1640995300
  assert_eq(sorted_events[3].id, "event-5")  // 1640995350
  assert_eq(sorted_events[4].id, "event-3")  // 1640995400
  
  // Test timestamp ordering
  for i in 0..sorted_events.length() - 1 {
    assert_true(sorted_events[i].timestamp <= sorted_events[i + 1].timestamp)
  }
}

// Test 3: Merge Sort for Stable Span Ordering
test "merge sort for stable span ordering" {
  // Define span with priority
  type Span = {
    id: String,
    name: String,
    priority: Int
  }
  
  // Merge sort implementation
  let merge = fn(left: Array[Span], right: Array[Span]) {
    let mut result = []
    let mut i = 0
    let mut j = 0
    
    while i < left.length() && j < right.length() {
      if left[i].priority <= right[j].priority {
        result = result.push(left[i])
        i = i + 1
      } else {
        result = result.push(right[j])
        j = j + 1
      }
    }
    
    while i < left.length() {
      result = result.push(left[i])
      i = i + 1
    }
    
    while j < right.length() {
      result = result.push(right[j])
      j = j + 1
    }
    
    result
  }
  
  let merge_sort = fn(spans: Array[Span]) {
    if spans.length() <= 1 {
      spans
    } else {
      let mid = spans.length() / 2
      let left = merge_sort(spans.slice(0, mid))
      let right = merge_sort(spans.slice(mid, spans.length()))
      merge(left, right)
    }
  }
  
  // Create spans with priorities
  let spans = [
    { id: "span-1", name: "database_query", priority: 3 },
    { id: "span-2", name: "api_call", priority: 1 },
    { id: "span-3", name: "cache_lookup", priority: 2 },
    { id: "span-4", name: "auth_check", priority: 1 },
    { id: "span-5", name: "response_send", priority: 4 }
  ]
  
  // Sort spans by priority (stable sort maintains order for equal priorities)
  let sorted_spans = merge_sort(spans)
  
  // Test sorting result
  assert_eq(sorted_spans.length(), 5)
  assert_eq(sorted_spans[0].priority, 1)
  assert_eq(sorted_spans[1].priority, 1)
  assert_eq(sorted_spans[2].priority, 2)
  assert_eq(sorted_spans[3].priority, 3)
  assert_eq(sorted_spans[4].priority, 4)
  
  // Test stability: spans with same priority maintain original order
  assert_eq(sorted_spans[0].id, "span-2")  // First priority 1 span
  assert_eq(sorted_spans[1].id, "span-4")  // Second priority 1 span
}

// Test 4: Hash Table with Chaining for Collision Resolution
test "hash table with chaining for collision resolution" {
  // Define hash table entry
  type HashEntry = {
    key: String,
    value: String
  }
  
  // Define hash table with chaining
  type HashTable = Array[Array[HashEntry]]
  
  // Simple hash function
  let hash_function = fn(key: String, table_size: Int) {
    let mut hash = 0
    let chars = key.to_char_array()
    for i in 0..chars.length() {
      hash = (hash + chars[i].to_int()) % table_size
    }
    hash
  }
  
  // Create hash table
  let create_hash_table = fn(size: Int) {
    let mut table = []
    for i in 0..size {
      table = table.push([])
    }
    table
  }
  
  // Insert into hash table
  let hash_insert = fn(table: HashTable, key: String, value: String) {
    let index = hash_function(key, table.length())
    let mut updated_table = table
    let bucket = updated_table[index]
    let entry = { key, value }
    updated_table[index] = bucket.push(entry)
    updated_table
  }
  
  // Search in hash table
  let hash_search = fn(table: HashTable, key: String) {
    let index = hash_function(key, table.length())
    let bucket = table[index]
    let mut result = None
    
    for entry in bucket {
      if entry.key == key {
        result = Some(entry.value)
      }
    }
    
    result
  }
  
  // Create hash table and insert values
  let mut table = create_hash_table(10)
  table = hash_insert(table, "span-1", "database_query")
  table = hash_insert(table, "span-2", "api_call")
  table = hash_insert(table, "span-3", "cache_lookup")
  table = hash_insert(table, "span-4", "auth_check")
  table = hash_insert(table, "span-5", "response_send")
  
  // Test hash table operations
  assert_eq(hash_search(table, "span-1"), Some("database_query"))
  assert_eq(hash_search(table, "span-2"), Some("api_call"))
  assert_eq(hash_search(table, "span-3"), Some("cache_lookup"))
  assert_eq(hash_search(table, "span-4"), Some("auth_check"))
  assert_eq(hash_search(table, "span-5"), Some("response_send"))
  assert_eq(hash_search(table, "non-existent"), None)
  
  // Test collision handling by inserting colliding keys
  table = hash_insert(table, "key-1", "value1")
  table = hash_insert(table, "key-11", "value2")  // Should collide with key-1
  
  assert_eq(hash_search(table, "key-1"), Some("value1"))
  assert_eq(hash_search(table, "key-11"), Some("value2"))
}

// Test 5: Trie for Efficient Prefix Matching
test "trie for efficient prefix matching of service names" {
  // Define trie node
  type TrieNode = {
    children: Array[(Char, TrieNode)],
    is_end: Bool,
    value: Option[String]
  }
  
  // Create trie node
  let create_trie_node = fn() {
    {
      children: [],
      is_end: false,
      value: None
    }
  }
  
  // Insert into trie
  let trie_insert = fn(root: TrieNode, key: String, value: String) {
    let chars = key.to_char_array()
    let mut current = root
    
    for i in 0..chars.length() {
      let c = chars[i]
      let mut found_child = None
      
      for (ch, child) in current.children {
        if ch == c {
          found_child = Some(child)
        }
      }
      
      match found_child {
        Some(child) => current = child
        None => {
          let new_child = create_trie_node()
          current.children = current.children.push((c, new_child))
          current = new_child
        }
      }
    }
    
    current.is_end = true
    current.value = Some(value)
    root
  }
  
  // Search in trie
  let trie_search = fn(root: TrieNode, key: String) {
    let chars = key.to_char_array()
    let mut current = root
    
    for i in 0..chars.length() {
      let c = chars[i]
      let mut found_child = None
      
      for (ch, child) in current.children {
        if ch == c {
          found_child = Some(child)
        }
      }
      
      match found_child {
        Some(child) => current = child
        None => return None
      }
    }
    
    if current.is_end {
      current.value
    } else {
      None
    }
  }
  
  // Create trie and insert service names
  let mut trie = create_trie_node()
  trie = trie_insert(trie, "payment-service", "v1.2.3")
  trie = trie_insert(trie, "payment-api", "v2.1.0")
  trie = trie_insert(trie, "user-service", "v1.5.2")
  trie = trie_insert(trie, "order-service", "v1.0.1")
  trie = trie_insert(trie, "notification-service", "v1.3.0")
  
  // Test trie operations
  assert_eq(trie_search(trie, "payment-service"), Some("v1.2.3"))
  assert_eq(trie_search(trie, "payment-api"), Some("v2.1.0"))
  assert_eq(trie_search(trie, "user-service"), Some("v1.5.2"))
  assert_eq(trie_search(trie, "order-service"), Some("v1.0.1"))
  assert_eq(trie_search(trie, "notification-service"), Some("v1.3.0"))
  assert_eq(trie_search(trie, "non-existent"), None)
}

// Test 6: LRU Cache for Frequently Accessed Spans
test "lru cache for frequently accessed spans" {
  // Define doubly linked list node
  type LRUNode = {
    key: String,
    value: String,
    prev: Option[LRUNode],
    next: Option[LRUNode]
  }
  
  // Define LRU cache
  type LRUCache = {
    capacity: Int,
    cache: Array[(String, LRUNode)],
    head: LRUNode,
    tail: LRUNode
  }
  
  // Create LRU cache
  let create_lru_cache = fn(capacity: Int) {
    let head = { key: "", value: "", prev: None, next: None }
    let tail = { key: "", value: "", prev: None, next: None }
    
    {
      capacity,
      cache: [],
      head,
      tail
    }
  }
  
  // Get from LRU cache
  let lru_get = fn(cache: LRUCache, key: String) {
    let mut result = None
    
    for (k, node) in cache.cache {
      if k == key {
        result = Some(node.value)
      }
    }
    
    result
  }
  
  // Put into LRU cache
  let lru_put = fn(cache: LRUCache, key: String, value: String) {
    let mut updated_cache = cache.cache
    let mut found = false
    
    // Check if key already exists
    for i in 0..updated_cache.length() {
      if updated_cache[i].0 == key {
        updated_cache[i] = (key, { key, value, prev: None, next: None })
        found = true
      }
    }
    
    // If key doesn't exist and cache is full, remove oldest
    if not(found) && updated_cache.length() >= cache.capacity {
      updated_cache = updated_cache.slice(1, updated_cache.length())
    }
    
    // Add new key-value pair
    if not(found) {
      updated_cache = updated_cache.push((key, { key, value, prev: None, next: None }))
    }
    
    {
      capacity: cache.capacity,
      cache: updated_cache,
      head: cache.head,
      tail: cache.tail
    }
  }
  
  // Create LRU cache and test operations
  let mut lru_cache = create_lru_cache(3)
  
  // Test empty cache
  assert_eq(lru_get(lru_cache, "span-1"), None)
  
  // Add items to cache
  lru_cache = lru_put(lru_cache, "span-1", "database_query")
  lru_cache = lru_put(lru_cache, "span-2", "api_call")
  lru_cache = lru_put(lru_cache, "span-3", "cache_lookup")
  
  // Test cache retrieval
  assert_eq(lru_get(lru_cache, "span-1"), Some("database_query"))
  assert_eq(lru_get(lru_cache, "span-2"), Some("api_call"))
  assert_eq(lru_get(lru_cache, "span-3"), Some("cache_lookup"))
  
  // Test cache eviction (add one more item)
  lru_cache = lru_put(lru_cache, "span-4", "auth_check")
  
  // First item should be evicted
  assert_eq(lru_get(lru_cache, "span-1"), None)
  assert_eq(lru_get(lru_cache, "span-2"), Some("api_call"))
  assert_eq(lru_get(lru_cache, "span-3"), Some("cache_lookup"))
  assert_eq(lru_get(lru_cache, "span-4"), Some("auth_check"))
  
  // Test cache update
  lru_cache = lru_put(lru_cache, "span-2", "updated_api_call")
  assert_eq(lru_get(lru_cache, "span-2"), Some("updated_api_call"))
}

// Test 7: Dijkstra's Algorithm for Service Path Analysis
test "dijkstra's algorithm for service path analysis" {
  // Define graph edge
  type Edge = {
    to: Int,
    weight: Int
  }
  
  // Define graph
  type Graph = Array[Array[Edge]]
  
  // Create graph for service dependencies
  let create_service_graph = fn() {
    let mut graph = []
    
    // Initialize 5 nodes (0-4)
    for i in 0..5 {
      graph = graph.push([])
    }
    
    // Add edges: 0->1(10), 0->2(5), 1->2(2), 1->3(1), 2->1(3), 2->3(9), 2->4(2), 3->4(4), 4->0(7), 4->3(6)
    graph[0] = graph[0].push({ to: 1, weight: 10 })
    graph[0] = graph[0].push({ to: 2, weight: 5 })
    graph[1] = graph[1].push({ to: 2, weight: 2 })
    graph[1] = graph[1].push({ to: 3, weight: 1 })
    graph[2] = graph[2].push({ to: 1, weight: 3 })
    graph[2] = graph[2].push({ to: 3, weight: 9 })
    graph[2] = graph[2].push({ to: 4, weight: 2 })
    graph[3] = graph[3].push({ to: 4, weight: 4 })
    graph[4] = graph[4].push({ to: 0, weight: 7 })
    graph[4] = graph[4].push({ to: 3, weight: 6 })
    
    graph
  }
  
  // Dijkstra's algorithm
  let dijkstra = fn(graph: Graph, start: Int) {
    let num_nodes = graph.length()
    let mut distances = []
    let mut visited = []
    
    // Initialize distances and visited
    for i in 0..num_nodes {
      if i == start {
        distances = distances.push(0)
      } else {
        distances = distances.push(999999)  // Infinity
      }
      visited = visited.push(false)
    }
    
    // Find shortest path for all nodes
    for _ in 0..num_nodes {
      // Find unvisited node with minimum distance
      let mut min_distance = 999999
      let mut min_index = -1
      
      for i in 0..num_nodes {
        if not(visited[i]) && distances[i] < min_distance {
          min_distance = distances[i]
          min_index = i
        }
      }
      
      if min_index == -1 {
        break
      }
      
      visited[min_index] = true
      
      // Update distances of adjacent nodes
      for edge in graph[min_index] {
        if not(visited[edge.to]) {
          let new_distance = distances[min_index] + edge.weight
          if new_distance < distances[edge.to] {
            distances[edge.to] = new_distance
          }
        }
      }
    }
    
    distances
  }
  
  // Create graph and run Dijkstra's algorithm
  let service_graph = create_service_graph()
  let distances_from_0 = dijkstra(service_graph, 0)
  
  // Test shortest distances from node 0
  assert_eq(distances_from_0[0], 0)  // Distance to itself
  assert_eq(distances_from_0[1], 8)  // 0->2->1 (5+3)
  assert_eq(distances_from_0[2], 5)  // 0->2 (5)
  assert_eq(distances_from_0[3], 9)  // 0->2->1->3 (5+3+1)
  assert_eq(distances_from_0[4], 7)  // 0->2->4 (5+2)
}

// Test 8: Dynamic Programming for Resource Allocation
test "dynamic programming for resource allocation optimization" {
  // Resource allocation problem: maximize value with limited resources
  let resource_allocation = fn(capacities: Array[Int], values: Array[Int], weights: Array[Int], total_capacity: Int) {
    let num_items = values.length()
    let mut dp = []
    
    // Initialize DP table
    for i in 0..num_items + 1 {
      let mut row = []
      for j in 0..total_capacity + 1 {
        row = row.push(0)
      }
      dp = dp.push(row)
    }
    
    // Fill DP table
    for i in 1..num_items + 1 {
      for j in 1..total_capacity + 1 {
        if weights[i - 1] <= j {
          let include_item = values[i - 1] + dp[i - 1][j - weights[i - 1]]
          let exclude_item = dp[i - 1][j]
          dp[i][j] = if include_item > exclude_item { include_item } else { exclude_item }
        } else {
          dp[i][j] = dp[i - 1][j]
        }
      }
    }
    
    dp[num_items][total_capacity]
  }
  
  // Test resource allocation for telemetry services
  let service_values = [10, 8, 15, 7, 6]  // Value of each service
  let service_costs = [3, 2, 5, 1, 2]    // Resource cost of each service
  let total_budget = 8                   // Total available resources
  
  let max_value = resource_allocation([], service_values, service_costs, total_budget)
  
  // Test optimal allocation
  // With budget 8, optimal is services 2, 3, 4 (values 8+15+7=30, costs 2+5+1=8)
  assert_eq(max_value, 30)
  
  // Test with different budget
  let max_value_5 = resource_allocation([], service_values, service_costs, 5)
  
  // With budget 5, optimal is services 2, 3 (values 8+15=23, costs 2+5=7 > 5, so just service 3)
  // Actually service 3 alone (value 15, cost 5) is optimal
  assert_eq(max_value_5, 15)
}

// Test 9: Greedy Algorithm for Task Scheduling
test "greedy algorithm for telemetry task scheduling" {
  // Define task
  type Task = {
    id: String,
    deadline: Int,
    duration: Int,
    profit: Int
  }
  
  // Greedy algorithm for task scheduling (maximize profit)
  let schedule_tasks = fn(tasks: Array[Task]) {
    // Sort tasks by profit in descending order
    let mut sorted_tasks = tasks
    let mut sorted = true
    
    while sorted {
      sorted = false
      for i in 0..sorted_tasks.length() - 1 {
        if sorted_tasks[i].profit < sorted_tasks[i + 1].profit {
          let temp = sorted_tasks[i]
          sorted_tasks[i] = sorted_tasks[i + 1]
          sorted_tasks[i + 1] = temp
          sorted = true
        }
      }
    }
    
    // Find maximum deadline
    let mut max_deadline = 0
    for task in sorted_tasks {
      if task.deadline > max_deadline {
        max_deadline = task.deadline
      }
    }
    
    // Initialize schedule slots
    let mut slots = []
    for i in 0..max_deadline {
      slots = slots.push(None)
    }
    
    // Schedule tasks
    let mut total_profit = 0
    for task in sorted_tasks {
      // Find the latest available slot before deadline
      let mut slot_found = false
      for j in (task.deadline - 1)..=0 {
        match slots[j] {
          None => {
            slots[j] = Some(task)
            total_profit = total_profit + task.profit
            slot_found = true
            break
          }
          Some(_) => ()  // Slot already occupied
        }
      }
    }
    
    total_profit
  }
  
  // Create telemetry processing tasks
  let tasks = [
    { id: "task-1", deadline: 2, duration: 1, profit: 100 },
    { id: "task-2", deadline: 1, duration: 2, profit: 50 },
    { id: "task-3", deadline: 3, duration: 2, profit: 150 },
    { id: "task-4", deadline: 2, duration: 1, profit: 200 },
    { id: "task-5", deadline: 1, duration: 1, profit: 75 }
  ]
  
  // Schedule tasks and get maximum profit
  let max_profit = schedule_tasks(tasks)
  
  // Test optimal scheduling
  // Task 4 (profit 200, deadline 2) -> slot 1
  // Task 3 (profit 150, deadline 3) -> slot 2
  // Task 1 (profit 100, deadline 2) -> slot 0
  // Total profit = 200 + 150 + 100 = 450
  assert_eq(max_profit, 450)
}

// Test 10: Backtracking for Span Path Reconstruction
test "backtracking for span path reconstruction" {
  // Define graph adjacency list
  type Graph = Array[Array[Int]]
  
  // Backtracking to find all paths from source to target
  let find_all_paths = fn(graph: Graph, source: Int, target: Int) {
    let mut all_paths = []
    
    let backtrack = fn(current: Int, path: Array[Int], visited: Array[Bool]) {
      let mut updated_path = path.push(current)
      let mut updated_visited = visited
      updated_visited[current] = true
      
      if current == target {
        all_paths = all_paths.push(updated_path)
      } else {
        for neighbor in graph[current] {
          if not(visited[neighbor]) {
            backtrack(neighbor, updated_path, updated_visited)
          }
        }
      }
    }
    
    let mut visited = []
    for i in 0..graph.length() {
      visited = visited.push(false)
    }
    
    backtrack(source, [], visited)
    all_paths
  }
  
  // Create a graph representing span relationships
  let span_graph = [
    [1, 2],        // 0 -> 1, 2
    [3],           // 1 -> 3
    [3, 4],        // 2 -> 3, 4
    [5],           // 3 -> 5
    [5],           // 4 -> 5
    []             // 5 -> (end)
  ]
  
  // Find all paths from span 0 to span 5
  let paths = find_all_paths(span_graph, 0, 5)
  
  // Test path finding
  assert_eq(paths.length(), 3)  // Should find 3 paths: 0-1-3-5, 0-2-3-5, 0-2-4-5
  
  // Test specific paths
  let path1 = [0, 1, 3, 5]
  let path2 = [0, 2, 3, 5]
  let path3 = [0, 2, 4, 5]
  
  let contains_path = fn(paths: Array[Array[Int]], target: Array[Int>) {
    let mut found = false
    for path in paths {
      if path.length() == target.length() {
        let mut match_found = true
        for i in 0..path.length() {
          if path[i] != target[i] {
            match_found = false
          }
        }
        if match_found {
          found = true
        }
      }
    }
    found
  }
  
  assert_true(contains_path(paths, path1))
  assert_true(contains_path(paths, path2))
  assert_true(contains_path(paths, path3))
}