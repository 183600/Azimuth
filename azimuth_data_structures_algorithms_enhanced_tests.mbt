// Azimuth Data Structures and Algorithms Enhanced Test Suite
// This file contains comprehensive tests for data validation and conversion functionality

// Test 1: Telemetry Data Type Conversion
test "telemetry data type conversion" {
  // Test string to numeric conversions
  let string_to_int = fn(s: String) {
    let mut result = 0
    let mut is_negative = false
    let mut index = 0
    
    if s.length() > 0 and s[0] == '-' {
      is_negative = true
      index = 1
    }
    
    while index < s.length() {
      let char = s[index]
      if char >= '0' and char <= '9' {
        result = result * 10 + (char.to_int() - '0'.to_int())
      } else {
        return None  // Invalid character
      }
      index = index + 1
    }
    
    if is_negative {
      result = -result
    }
    
    Some(result)
  }
  
  let string_to_float = fn(s: String) {
    let mut integer_part = 0
    let mut fractional_part = 0
    let mut fractional_divisor = 1.0
    let mut is_negative = false
    let mut index = 0
    let mut in_fractional = false
    
    if s.length() > 0 and s[0] == '-' {
      is_negative = true
      index = 1
    }
    
    while index < s.length() {
      let char = s[index]
      if char >= '0' and char <= '9' {
        if in_fractional {
          fractional_part = fractional_part * 10 + (char.to_int() - '0'.to_int())
          fractional_divisor = fractional_divisor * 10.0
        } else {
          integer_part = integer_part * 10 + (char.to_int() - '0'.to_int())
        }
      } else if char == '.' {
        in_fractional = true
      } else {
        return None  // Invalid character
      }
      index = index + 1
    }
    
    let result = integer_part.to_float() + (fractional_part.to_float() / fractional_divisor)
    if is_negative {
      result = -result
    }
    
    Some(result)
  }
  
  // Test integer conversions
  assert_eq(string_to_int("42"), Some(42))
  assert_eq(string_to_int("-17"), Some(-17))
  assert_eq(string_to_int("0"), Some(0))
  assert_eq(string_to_int("123456"), Some(123456))
  assert_eq(string_to_int("invalid"), None)
  assert_eq(string_to_int("12a34"), None)
  
  // Test float conversions
  assert_eq(string_to_float("3.14"), Some(3.14))
  assert_eq(string_to_float("-2.5"), Some(-2.5))
  assert_eq(string_to_float("0.0"), Some(0.0))
  assert_eq(string_to_float("123.456"), Some(123.456))
  assert_eq(string_to_float("invalid"), None)
  assert_eq(string_to_float("12.34.56"), None)
  
  // Test boolean conversion
  let string_to_bool = fn(s: String) {
    match s.to_lowercase() {
      "true" => Some(true)
      "false" => Some(false)
      "1" => Some(true)
      "0" => Some(false)
      "yes" => Some(true)
      "no" => Some(false)
      _ => None
    }
  }
  
  assert_eq(string_to_bool("true"), Some(true))
  assert_eq(string_to_bool("false"), Some(false))
  assert_eq(string_to_bool("TRUE"), Some(true))
  assert_eq(string_to_bool("1"), Some(true))
  assert_eq(string_to_bool("0"), Some(false))
  assert_eq(string_to_bool("yes"), Some(true))
  assert_eq(string_to_bool("no"), Some(false))
  assert_eq(string_to_bool("maybe"), None)
}

// Test 2: Data Validation Framework
test "data validation framework" {
  // Define validation types
  type ValidationError = {
    field: String,
    message: String,
    code: String
  }
  
  type ValidationResult = {
    is_valid: Bool,
    errors: Array[ValidationError]
  }
  
  // Validation functions
  let validate_required = fn(value: String, field_name: String) {
    if value.length() == 0 {
      [{
        field: field_name,
        message: field_name + " is required",
        code: "REQUIRED"
      }]
    } else {
      []
    }
  }
  
  let validate_length = fn(value: String, min: Int, max: Int, field_name: String) {
    let mut errors = []
    if value.length() < min {
      errors = errors.push({
        field: field_name,
        message: field_name + " must be at least " + min.to_string() + " characters",
        code: "MIN_LENGTH"
      })
    }
    if value.length() > max {
      errors = errors.push({
        field: field_name,
        message: field_name + " must be at most " + max.to_string() + " characters",
        code: "MAX_LENGTH"
      })
    }
    errors
  }
  
  let validate_pattern = fn(value: String, pattern: String, field_name: String) {
    // Simplified pattern matching for trace IDs (alphanumeric with hyphens)
    let is_valid = fn(c: Char) {
      (c >= 'a' and c <= 'z') or
      (c >= 'A' and c <= 'Z') or
      (c >= '0' and c <= '9') or
      c == '-'
    }
    
    let mut valid = true
    for i in 0..value.length() {
      if not(is_valid(value[i])) {
        valid = false
        break
      }
    }
    
    if valid {
      []
    } else {
      [{
        field: field_name,
        message: field_name + " contains invalid characters",
        code: "INVALID_PATTERN"
      }]
    }
  }
  
  let validate_range = fn(value: Int, min: Int, max: Int, field_name: String) {
    if value < min or value > max {
      [{
        field: field_name,
        message: field_name + " must be between " + min.to_string() + " and " + max.to_string(),
        code: "OUT_OF_RANGE"
      }]
    } else {
      []
    }
  }
  
  // Test validation functions
  assert_eq(validate_required("", "name"), [{
    field: "name",
    message: "name is required",
    code: "REQUIRED"
  }])
  
  assert_eq(validate_required("valid", "name"), [])
  
  assert_eq(validate_length("ab", 3, 10, "name"), [{
    field: "name",
    message: "name must be at least 3 characters",
    code: "MIN_LENGTH"
  }])
  
  assert_eq(validate_length("very long name", 3, 10, "name"), [{
    field: "name",
    message: "name must be at most 10 characters",
    code: "MAX_LENGTH"
  }])
  
  assert_eq(validate_length("valid", 3, 10, "name"), [])
  
  assert_eq(validate_pattern("trace-123", "trace_id", "trace_id"), [])
  assert_eq(validate_pattern("trace@123", "trace_id", "trace_id"), [{
    field: "trace_id",
    message: "trace_id contains invalid characters",
    code: "INVALID_PATTERN"
  }])
  
  assert_eq(validate_range(5, 1, 10, "value"), [])
  assert_eq(validate_range(0, 1, 10, "value"), [{
    field: "value",
    message: "value must be between 1 and 10",
    code: "OUT_OF_RANGE"
  }])
  assert_eq(validate_range(15, 1, 10, "value"), [{
    field: "value",
    message: "value must be between 1 and 10",
    code: "OUT_OF_RANGE"
  }])
  
  // Composite validation
  let validate_span = fn(name: String, trace_id: String, duration: Int) {
    let name_errors = validate_required(name, "name") + validate_length(name, 1, 50, "name")
    let trace_errors = validate_required(trace_id, "trace_id") + validate_pattern(trace_id, "trace_id", "trace_id")
    let duration_errors = validate_range(duration, 0, 3600000, "duration")
    
    let all_errors = name_errors + trace_errors + duration_errors
    
    {
      is_valid: all_errors.length() == 0,
      errors: all_errors
    }
  }
  
  // Test composite validation
  let valid_span = validate_span("database_query", "trace-123", 250)
  assert_true(valid_span.is_valid)
  assert_eq(valid_span.errors.length(), 0)
  
  let invalid_span = validate_span("", "trace@123", -10)
  assert_false(invalid_span.is_valid)
  assert_true(invalid_span.errors.length() >= 3)
  
  // Check specific errors
  let has_name_error = invalid_span.errors.any(fn(e) { e.field == "name" })
  let has_trace_error = invalid_span.errors.any(fn(e) { e.field == "trace_id" })
  let has_duration_error = invalid_span.errors.any(fn(e) { e.field == "duration" })
  
  assert_true(has_name_error)
  assert_true(has_trace_error)
  assert_true(has_duration_error)
}

// Test 3: Advanced Data Structure Operations
test "advanced data structure operations" {
  // Binary tree implementation
  type TreeNode = {
    value: Int,
    left: Option[TreeNode],
    right: Option[TreeNode]
  }
  
  let create_tree_node = fn(value: Int) {
    {
      value,
      left: None,
      right: None
    }
  }
  
  let tree_insert = fn(node: TreeNode, value: Int) {
    if value < node.value {
      match node.left {
        Some(left_node) => {
          node.left = Some(tree_insert(left_node, value))
        }
        None => {
          node.left = Some(create_tree_node(value))
        }
      }
    } else if value > node.value {
      match node.right {
        Some(right_node) => {
          node.right = Some(tree_insert(right_node, value))
        }
        None => {
          node.right = Some(create_tree_node(value))
        }
      }
    }
    // If equal, do nothing (no duplicates)
    node
  }
  
  let tree_contains = fn(node: TreeNode, value: Int) {
    if value == node.value {
      true
    } else if value < node.value {
      match node.left {
        Some(left_node) => tree_contains(left_node, value)
        None => false
      }
    } else {
      match node.right {
        Some(right_node) => tree_contains(right_node, value)
        None => false
      }
    }
  }
  
  let tree_inorder = fn(node: TreeNode) {
    let mut result = []
    
    match node.left {
      Some(left_node) => {
        result = result + tree_inorder(left_node)
      }
      None => {}
    }
    
    result = result.push(node.value)
    
    match node.right {
      Some(right_node) => {
        result = result + tree_inorder(right_node)
      }
      None => {}
    }
    
    result
  }
  
  // Test binary tree operations
  let root = create_tree_node(50)
  let tree_with_values = tree_insert(tree_insert(tree_insert(root, 30), 70), 20)
  let final_tree = tree_insert(tree_insert(tree_with_values, 40), 60)
  
  assert_true(tree_contains(final_tree, 50))
  assert_true(tree_contains(final_tree, 30))
  assert_true(tree_contains(final_tree, 70))
  assert_true(tree_contains(final_tree, 20))
  assert_true(tree_contains(final_tree, 40))
  assert_true(tree_contains(final_tree, 60))
  assert_false(tree_contains(final_tree, 10))
  assert_false(tree_contains(final_tree, 80))
  
  let inorder_result = tree_inorder(final_tree)
  assert_eq(inorder_result, [20, 30, 40, 50, 60, 70])
  
  // Hash table simulation
  type HashEntry = {
    key: String,
    value: String,
    next: Option[HashEntry]
  }
  
  let hash_function = fn(key: String, table_size: Int) {
    let mut hash = 0
    for i in 0..key.length() {
      hash = (hash * 31 + key[i].to_int()) % table_size
    }
    if hash < 0 {
      hash = hash + table_size
    }
    hash
  }
  
  let hash_table_create = fn(size: Int) {
    let mut table = []
    for i in 0..size {
      table = table.push(None)
    }
    table
  }
  
  let hash_table_insert = fn(table: Array[Option[HashEntry]], key: String, value: String) {
    let index = hash_function(key, table.length())
    let new_entry = {
      key,
      value,
      next: table[index]
    }
    table[index] = Some(new_entry)
    table
  }
  
  let hash_table_get = fn(table: Array[Option[HashEntry]], key: String) {
    let index = hash_function(key, table.length())
    
    match table[index] {
      Some(entry) => {
        let mut current = Some(entry)
        while true {
          match current {
            Some(e) => {
              if e.key == key {
                return Some(e.value)
              }
              current = e.next
            }
            None => {
              return None
            }
          }
        }
        None  // Should never reach here
      }
      None => None
    }
  }
  
  // Test hash table operations
  let hash_table = hash_table_create(10)
  let table_with_data = hash_table_insert(
    hash_table_insert(
      hash_table_insert(hash_table, "service", "payment"),
      "trace_id", "trace-12345"
    ),
    "span_id", "span-67890"
  )
  
  assert_eq(hash_table_get(table_with_data, "service"), Some("payment"))
  assert_eq(hash_table_get(table_with_data, "trace_id"), Some("trace-12345"))
  assert_eq(hash_table_get(table_with_data, "span_id"), Some("span-67890"))
  assert_eq(hash_table_get(table_with_data, "nonexistent"), None)
  
  // Test hash function distribution
  let keys = ["key1", "key2", "key3", "key4", "key5", "key6", "key7", "key8", "key9", "key10"]
  let mut hash_counts = []
  for i in 0..10 {
    hash_counts = hash_counts.push(0)
  }
  
  for key in keys {
    let hash = hash_function(key, 10)
    hash_counts[hash] = hash_counts[hash] + 1
  }
  
  // Verify distribution (not too concentrated)
  let max_count = hash_counts.reduce(fn(acc, count) { if count > acc { count } else { acc }, 0)
  assert_true(max_count <= 3)  // No bucket should have more than 3 keys
}

// Test 4: Algorithm Optimization
test "algorithm optimization" {
  // Linear search vs binary search comparison
  let linear_search = fn(arr: Array[Int], target: Int) {
    let mut index = 0
    let mut found = false
    
    while index < arr.length() {
      if arr[index] == target {
        found = true
        break
      }
      index = index + 1
    }
    
    if found {
      Some(index)
    } else {
      None
    }
  }
  
  let binary_search = fn(arr: Array[Int], target: Int) {
    let mut left = 0
    let mut right = arr.length() - 1
    let mut found = false
    let mut result_index = -1
    
    while left <= right {
      let mid = left + (right - left) / 2
      
      if arr[mid] == target {
        found = true
        result_index = mid
        break
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    if found {
      Some(result_index)
    } else {
      None
    }
  }
  
  // Create sorted array
  let mut sorted_array = []
  for i in 0..100 {
    sorted_array = sorted_array.push(i * 2)
  }
  
  // Test searches
  assert_eq(linear_search(sorted_array, 50), Some(25))
  assert_eq(binary_search(sorted_array, 50), Some(25))
  
  assert_eq(linear_search(sorted_array, 99), None)
  assert_eq(binary_search(sorted_array, 99), None)
  
  assert_eq(linear_search(sorted_array, 198), Some(99))
  assert_eq(binary_search(sorted_array, 198), Some(99))
  
  // String searching algorithms
  let naive_search = fn(text: String, pattern: String) {
    let text_len = text.length()
    let pattern_len = pattern.length()
    
    if pattern_len == 0 {
      return Some(0)
    }
    
    if pattern_len > text_len {
      return None
    }
    
    for i in 0..=(text_len - pattern_len) {
      let mut match_found = true
      
      for j in 0..pattern_len {
        if text[i + j] != pattern[j] {
          match_found = false
          break
        }
      }
      
      if match_found {
        return Some(i)
      }
    }
    
    None
  }
  
  // Test string searching
  let text = "azimuth telemetry system for distributed tracing"
  assert_eq(naive_search(text, "telemetry"), Some(8))
  assert_eq(naive_search(text, "tracing"), Some(42))
  assert_eq(naive_search(text, "nonexistent"), None)
  assert_eq(naive_search(text, ""), Some(0))
  assert_eq(naive_search("", "pattern"), None)
  
  // Sorting algorithms
  let bubble_sort = fn(arr: Array[Int]) {
    let mut result = arr.clone()
    let n = result.length()
    
    for i in 0..n {
      for j in 0..(n - i - 1) {
        if result[j] > result[j + 1] {
          let temp = result[j]
          result[j] = result[j + 1]
          result[j + 1] = temp
        }
      }
    }
    
    result
  }
  
  let quick_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[0]
      let mut less = []
      let mut equal = []
      let mut greater = []
      
      for item in arr {
        if item < pivot {
          less = less.push(item)
        } else if item > pivot {
          greater = greater.push(item)
        } else {
          equal = equal.push(item)
        }
      }
      
      quick_sort(less) + equal + quick_sort(greater)
    }
  }
  
  // Test sorting algorithms
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6]
  let bubble_sorted = bubble_sort(unsorted)
  let quick_sorted = quick_sort(unsorted)
  
  assert_eq(bubble_sorted, [1, 2, 3, 4, 5, 6, 7, 8, 9])
  assert_eq(quick_sorted, [1, 2, 3, 4, 5, 6, 7, 8, 9])
  
  // Test with duplicates
  let with_duplicates = [3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5]
  let bubble_sorted_dup = bubble_sort(with_duplicates)
  let quick_sorted_dup = quick_sort(with_duplicates)
  
  assert_eq(bubble_sorted_dup, [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9])
  assert_eq(quick_sorted_dup, [1, 1, 2, 3, 3, 4, 5, 5, 5, 6, 9])
  
  // Test with empty and single-element arrays
  assert_eq(bubble_sort([]), [])
  assert_eq(quick_sort([]), [])
  
  assert_eq(bubble_sort([42]), [42])
  assert_eq(quick_sort([42]), [42])
}

// Test 5: Graph Algorithms
test "graph algorithms" {
  // Graph representation
  type GraphEdge = {
    to: Int,
    weight: Float
  }
  
  type GraphNode = {
    id: Int,
    edges: Array[GraphEdge]
  }
  
  let create_graph_node = fn(id: Int) {
    {
      id,
      edges: []
    }
  }
  
  let add_edge = fn(from: GraphNode, to: Int, weight: Float) {
    let new_edge = { to, weight }
    { from | edges: from.edges.push(new_edge) }
  }
  
  // Build a sample graph
  let node1 = create_graph_node(1)
  let node2 = create_graph_node(2)
  let node3 = create_graph_node(3)
  let node4 = create_graph_node(4)
  let node5 = create_graph_node(5)
  
  let graph = [
    add_edge(add_edge(node1, 2, 1.5), 3, 2.0),
    add_edge(add_edge(node2, 3, 1.0), 4, 3.2),
    add_edge(add_edge(add_edge(node3, 1, 2.5), 4, 1.8), 5, 2.7),
    add_edge(node4, 5, 2.3),
    create_graph_node(5)  // No outgoing edges
  ]
  
  // Depth-First Search (DFS)
  let dfs = fn(graph: Array[GraphNode], start_id: Int, target_id: Int) {
    let mut visited = []
    let mut stack = [start_id]
    let mut found = false
    
    while stack.length() > 0 and not(found) {
      let current = stack[stack.length() - 1]
      stack = stack.slice(0, stack.length() - 1)
      
      if not(visited.contains(current)) {
        visited = visited.push(current)
        
        if current == target_id {
          found = true
          break
        }
        
        // Find the node with current_id
        let mut current_node = None
        for node in graph {
          if node.id == current {
            current_node = Some(node)
            break
          }
        }
        
        match current_node {
          Some(node) => {
            // Add neighbors to stack (in reverse order for correct DFS order)
            for i in (node.edges.length() - 1)..=0 {
              stack = stack.push(node.edges[i].to)
            }
          }
          None => {}
        }
      }
    }
    
    found
  }
  
  // Breadth-First Search (BFS)
  let bfs = fn(graph: Array[GraphNode], start_id: Int, target_id: Int) {
    let mut visited = []
    let mut queue = [start_id]
    let mut found = false
    
    while queue.length() > 0 and not(found) {
      let current = queue[0]
      queue = queue.slice(1, queue.length())
      
      if not(visited.contains(current)) {
        visited = visited.push(current)
        
        if current == target_id {
          found = true
          break
        }
        
        // Find the node with current_id
        let mut current_node = None
        for node in graph {
          if node.id == current {
            current_node = Some(node)
            break
          }
        }
        
        match current_node {
          Some(node) => {
            // Add neighbors to queue
            for edge in node.edges {
              queue = queue.push(edge.to)
            }
          }
          None => {}
        }
      }
    }
    
    found
  }
  
  // Test graph traversal
  assert_true(dfs(graph, 1, 5))
  assert_true(dfs(graph, 1, 4))
  assert_true(dfs(graph, 2, 5))
  assert_false(dfs(graph, 5, 1))
  assert_true(dfs(graph, 3, 4))
  
  assert_true(bfs(graph, 1, 5))
  assert_true(bfs(graph, 1, 4))
  assert_true(bfs(graph, 2, 5))
  assert_false(bfs(graph, 5, 1))
  assert_true(bfs(graph, 3, 4))
  
  // Shortest path (Dijkstra's algorithm simplified)
  let shortest_path = fn(graph: Array[GraphNode], start_id: Int, end_id: Int) {
    let mut distances = {}
    let mut previous = {}
    let mut unvisited = []
    
    // Initialize
    for node in graph {
      distances = distances.insert(node.id, if node.id == start_id { 0.0 } else { Float::infinity })
      previous = previous.insert(node.id, None)
      unvisited = unvisited.push(node.id)
    }
    
    while unvisited.length() > 0 {
      // Find unvisited node with minimum distance
      let mut min_node = -1
      let mut min_distance = Float::infinity()
      
      for node_id in unvisited {
        let dist = distances[node_id]
        if dist < min_distance {
          min_distance = dist
          min_node = node_id
        }
      }
      
      if min_node == -1 or min_node == end_id {
        break
      }
      
      // Remove min_node from unvisited
      let mut new_unvisited = []
      for node_id in unvisited {
        if node_id != min_node {
          new_unvisited = new_unvisited.push(node_id)
        }
      }
      unvisited = new_unvisited
      
      // Update distances to neighbors
      let mut current_node = None
      for node in graph {
        if node.id == min_node {
          current_node = Some(node)
          break
        }
      }
      
      match current_node {
        Some(node) => {
          for edge in node.edges {
            let alt = distances[min_node] + edge.weight
            if alt < distances[edge.to] {
              distances = distances.insert(edge.to, alt)
              previous = previous.insert(edge.to, Some(min_node))
            }
          }
        }
        None => {}
      }
    }
    
    // Reconstruct path
    let mut path = []
    let mut current = end_id
    
    while true {
      match previous[current] {
        Some(prev) => {
          path = [prev] + path
          if prev == start_id {
            path = path.push(end_id)
            return Some(path)
          }
          current = prev
        }
        None => {
          if current == start_id {
            return Some([start_id])
          } else {
            return None
          }
        }
      }
    }
  }
  
  // Test shortest path
  let path_1_to_5 = shortest_path(graph, 1, 5)
  assert_eq(path_1_to_5, Some([1, 3, 5]))
  
  let path_2_to_5 = shortest_path(graph, 2, 5)
  assert_eq(path_2_to_5, Some([2, 3, 5]))
  
  let path_1_to_4 = shortest_path(graph, 1, 4)
  assert_eq(path_1_to_4, Some([1, 3, 4]))
  
  let path_5_to_1 = shortest_path(graph, 5, 1)
  assert_eq(path_5_to_1, None)
}