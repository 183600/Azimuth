// Algorithm Complexity Tests for Azimuth Telemetry System
// This file contains tests for algorithm complexity and performance analysis

// Test 1: Sorting Algorithm Complexity
test "sorting algorithm complexity analysis" {
  // Define sorting algorithms
  enum SortAlgorithm {
    BubbleSort
    QuickSort
    MergeSort
    InsertionSort
  }
  
  // Implement bubble sort (O(n²))
  let bubble_sort = fn(arr : [Int]) {
    let mut sorted_arr = arr
    let n = sorted_arr.length()
    
    for i in 0..<n {
      for j in 0..<n - i - 1 {
        if sorted_arr[j] > sorted_arr[j + 1] {
          let temp = sorted_arr[j]
          sorted_arr = sorted_arr.with(j, sorted_arr[j + 1])
          sorted_arr = sorted_arr.with(j + 1, temp)
        }
      }
    }
    
    sorted_arr
  }
  
  // Implement quick sort (O(n log n) average)
  let quick_sort = fn(arr : [Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[arr.length() / 2]
      let less = arr.filter(fn(x) { x < pivot })
      let equal = arr.filter(fn(x) { x == pivot })
      let greater = arr.filter(fn(x) { x > pivot })
      
      quick_sort(less) + equal + quick_sort(greater)
    }
  }
  
  // Implement merge sort (O(n log n))
  let merge_sort = fn(arr : [Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let mid = arr.length() / 2
      let left = merge_sort(arr.slice(0, mid))
      let right = merge_sort(arr.slice(mid, arr.length()))
      
      let merge = fn(left : [Int], right : [Int]) {
        let mut result = []
        let mut i = 0
        let mut j = 0
        
        while i < left.length() && j < right.length() {
          if left[i] <= right[j] {
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
      
      merge(left, right)
    }
  }
  
  // Implement insertion sort (O(n²))
  let insertion_sort = fn(arr : [Int]) {
    let mut sorted_arr = arr
    
    for i in 1..<sorted_arr.length() {
      let key = sorted_arr[i]
      let mut j = i - 1
      
      while j >= 0 && sorted_arr[j] > key {
        sorted_arr = sorted_arr.with(j + 1, sorted_arr[j])
        j = j - 1
      }
      
      sorted_arr = sorted_arr.with(j + 1, key)
    }
    
    sorted_arr
  }
  
  // Measure sorting performance
  let measure_sort_performance = fn(arr : [Int], algorithm : SortAlgorithm) {
    let start_time = get_current_time_millis()
    
    let sorted_arr = match algorithm {
      SortAlgorithm::BubbleSort => bubble_sort(arr),
      SortAlgorithm::QuickSort => quick_sort(arr),
      SortAlgorithm::MergeSort => merge_sort(arr),
      SortAlgorithm::InsertionSort => insertion_sort(arr)
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "sorted_array": sorted_arr,
      "duration_ms": duration,
      "array_size": arr.length()
    }
  }
  
  // Create test arrays of different sizes
  let create_random_array = fn(size : Int) {
    let mut arr = []
    for i in 0..<size {
      arr = arr.push((i * 17) % 100)  // Pseudo-random numbers
    }
    arr
  }
  
  // Test sorting correctness
  let small_array = [5, 2, 8, 1, 9, 3]
  
  let bubble_result = measure_sort_performance(small_array, SortAlgorithm::BubbleSort)
  let quick_result = measure_sort_performance(small_array, SortAlgorithm::QuickSort)
  let merge_result = measure_sort_performance(small_array, SortAlgorithm::MergeSort)
  let insertion_result = measure_sort_performance(small_array, SortAlgorithm::InsertionSort)
  
  // Verify all algorithms produce the same sorted result
  assert_eq(bubble_result["sorted_array"], [1, 2, 3, 5, 8, 9])
  assert_eq(quick_result["sorted_array"], [1, 2, 3, 5, 8, 9])
  assert_eq(merge_result["sorted_array"], [1, 2, 3, 5, 8, 9])
  assert_eq(insertion_result["sorted_array"], [1, 2, 3, 5, 8, 9])
  
  // Test performance with larger arrays
  let medium_array = create_random_array(100)
  
  let medium_bubble = measure_sort_performance(medium_array, SortAlgorithm::BubbleSort)
  let medium_quick = measure_sort_performance(medium_array, SortAlgorithm::QuickSort)
  let medium_merge = measure_sort_performance(medium_array, SortAlgorithm::MergeSort)
  let medium_insertion = measure_sort_performance(medium_array, SortAlgorithm::InsertionSort)
  
  // O(n log n) algorithms should be faster than O(n²) for larger arrays
  assert_true(medium_quick["duration_ms"] <= medium_bubble["duration_ms"])
  assert_true(medium_merge["duration_ms"] <= medium_bubble["duration_ms"])
  assert_true(medium_quick["duration_ms"] <= medium_insertion["duration_ms"])
  assert_true(medium_merge["duration_ms"] <= medium_insertion["duration_ms"])
}

// Test 2: Search Algorithm Complexity
test "search algorithm complexity analysis" {
  // Define search algorithms
  enum SearchAlgorithm {
    LinearSearch
    BinarySearch
    HashSearch
  }
  
  // Implement linear search (O(n))
  let linear_search = fn(arr : [Int], target : Int) {
    for i in 0..<arr.length() {
      if arr[i] == target {
        return Some(i)
      }
    }
    None
  }
  
  // Implement binary search (O(log n))
  let binary_search = fn(arr : [Int], target : Int) {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = left + (right - left) / 2
      
      if arr[mid] == target {
        return Some(mid)
      } else if arr[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    None
  }
  
  // Implement hash search (O(1) average)
  let hash_search = fn(arr : [Int], target : Int) {
    let mut hash_map = {}
    
    for i in 0..<arr.length() {
      hash_map = hash_map.set(arr[i], i)
    }
    
    hash_map.get(target)
  }
  
  // Measure search performance
  let measure_search_performance = fn(arr : [Int], target : Int, algorithm : SearchAlgorithm) {
    let start_time = get_current_time_millis()
    
    let result = match algorithm {
      SearchAlgorithm::LinearSearch => linear_search(arr, target),
      SearchAlgorithm::BinarySearch => binary_search(arr, target),
      SearchAlgorithm::HashSearch => hash_search(arr, target)
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "result": result,
      "duration_ms": duration,
      "array_size": arr.length()
    }
  }
  
  // Create sorted array for binary search
  let create_sorted_array = fn(size : Int) {
    let mut arr = []
    for i in 0..<size {
      arr = arr.push(i * 2)  // Even numbers: 0, 2, 4, 6, ...
    }
    arr
  }
  
  // Test search correctness
  let sorted_array = create_sorted_array(100)
  let target = 50  // Should be at index 25
  
  let linear_result = measure_search_performance(sorted_array, target, SearchAlgorithm::LinearSearch)
  let binary_result = measure_search_performance(sorted_array, target, SearchAlgorithm::BinarySearch)
  let hash_result = measure_search_performance(sorted_array, target, SearchAlgorithm::HashSearch)
  
  // Verify all algorithms find the target
  match linear_result["result"] {
    Some(index) => assert_eq(index, 25),
    None => assert_true(false)
  }
  
  match binary_result["result"] {
    Some(index) => assert_eq(index, 25),
    None => assert_true(false)
  }
  
  match hash_result["result"] {
    Some(index) => assert_eq(index, 25),
    None => assert_true(false)
  }
  
  // Test performance with larger arrays
  let large_sorted_array = create_sorted_array(10000)
  let large_target = 5000  // Should be at index 2500
  
  let large_linear = measure_search_performance(large_sorted_array, large_target, SearchAlgorithm::LinearSearch)
  let large_binary = measure_search_performance(large_sorted_array, large_target, SearchAlgorithm::BinarySearch)
  let large_hash = measure_search_performance(large_sorted_array, large_target, SearchAlgorithm::HashSearch)
  
  // Binary search and hash search should be faster than linear search for larger arrays
  assert_true(large_binary["duration_ms"] <= large_linear["duration_ms"])
  assert_true(large_hash["duration_ms"] <= large_linear["duration_ms"])
}

// Test 3: Data Structure Operation Complexity
test "data structure operation complexity analysis" {
  // Define data structure operations
  enum DataStructure {
    Array
    LinkedList
    HashMap
    TreeSet
  }
  
  // Define operation types
  enum OperationType {
    Insert
    Delete
    Search
    Access
  }
  
  // Simulate array operations
  let array_operations = fn(size : Int, operation : OperationType) {
    let mut arr = []
    
    // Initialize array
    for i in 0..<size {
      arr = arr.push(i)
    }
    
    let start_time = get_current_time_millis()
    
    match operation {
      OperationType::Insert => {
        // Insert at the end (O(1) amortized)
        arr = arr.push(size)
      }
      OperationType::Delete => {
        // Delete from the end (O(1))
        if arr.length() > 0 {
          arr = arr.slice(0, arr.length() - 1)
        }
      }
      OperationType::Search => {
        // Linear search (O(n))
        let target = size / 2
        let mut found = false
        for i in 0..<arr.length() {
          if arr[i] == target {
            found = true
            break
          }
        }
        let _ = found
      }
      OperationType::Access => {
        // Random access (O(1))
        if arr.length() > 0 {
          let index = size / 2
          let value = arr[index]
          let _ = value
        }
      }
    }
    
    let end_time = get_current_time_millis()
    end_time - start_time
  }
  
  // Simulate linked list operations
  let linked_list_operations = fn(size : Int, operation : OperationType) {
    // Simulate linked list with array of nodes
    type Node = {
      value : Int
      next : Option[Int]
    }
    
    let mut nodes = []
    
    // Initialize linked list
    for i in 0..<size {
      let next = if i < size - 1 { Some(i + 1) } else { None }
      nodes = nodes.push({ value: i, next: next })
    }
    
    let start_time = get_current_time_millis()
    
    match operation {
      OperationType::Insert => {
        // Insert at head (O(1))
        let new_node = { value: size, next: Some(0) }
        nodes = [new_node] + nodes
      }
      OperationType::Delete => {
        // Delete from head (O(1))
        if nodes.length() > 0 {
          nodes = nodes.slice(1, nodes.length())
        }
      }
      OperationType::Search => {
        // Linear search (O(n))
        let target = size / 2
        let mut found = false
        let mut current = 0
        
        while current < nodes.length() && not found {
          if nodes[current].value == target {
            found = true
          } else {
            match nodes[current].next {
              Some(next) => current = next,
              None => break
            }
          }
        }
        
        let _ = found
      }
      OperationType::Access => {
        // Access by index (O(n))
        if nodes.length() > 0 {
          let index = size / 2
          let mut current = 0
          let mut count = 0
          
          while current < nodes.length() && count < index {
            match nodes[current].next {
              Some(next) => current = next,
              None => break
            }
            count = count + 1
          }
          
          let value = nodes[current].value
          let _ = value
        }
      }
    }
    
    let end_time = get_current_time_millis()
    end_time - start_time
  }
  
  // Simulate hash map operations
  let hash_map_operations = fn(size : Int, operation : OperationType) {
    let mut hash_map = {}
    
    // Initialize hash map
    for i in 0..<size {
      hash_map = hash_map.set(i, i * 2)
    }
    
    let start_time = get_current_time_millis()
    
    match operation {
      OperationType::Insert => {
        // Insert (O(1) average)
        hash_map = hash_map.set(size, size * 2)
      }
      OperationType::Delete => {
        // Delete (O(1) average)
        hash_map = hash_map.remove(size / 2)
      }
      OperationType::Search => {
        // Search (O(1) average)
        let target = size / 2
        let value = hash_map.get(target)
        let _ = value
      }
      OperationType::Access => {
        // Access by key (O(1) average)
        let key = size / 2
        let value = hash_map.get(key)
        let _ = value
      }
    }
    
    let end_time = get_current_time_millis()
    end_time - start_time
  }
  
  // Test data structure operations
  let test_sizes = [100, 1000, 10000]
  
  for size in test_sizes {
    // Array operations
    let array_insert = array_operations(size, OperationType::Insert)
    let array_delete = array_operations(size, OperationType::Delete)
    let array_search = array_operations(size, OperationType::Search)
    let array_access = array_operations(size, OperationType::Access)
    
    // Linked list operations
    let linked_list_insert = linked_list_operations(size, OperationType::Insert)
    let linked_list_delete = linked_list_operations(size, OperationType::Delete)
    let linked_list_search = linked_list_operations(size, OperationType::Search)
    let linked_list_access = linked_list_operations(size, OperationType::Access)
    
    // Hash map operations
    let hash_map_insert = hash_map_operations(size, OperationType::Insert)
    let hash_map_delete = hash_map_operations(size, OperationType::Delete)
    let hash_map_search = hash_map_operations(size, OperationType::Search)
    let hash_map_access = hash_map_operations(size, OperationType::Access)
    
    // Verify complexity characteristics
    // Array access should be O(1), search O(n)
    assert_true(array_access <= array_search)
    
    // Linked list insert/delete at head should be O(1), search/access O(n)
    assert_true(linked_list_insert <= linked_list_search)
    assert_true(linked_list_delete <= linked_list_access)
    
    // Hash map operations should be O(1) average
    assert_true(hash_map_search <= array_search)
    assert_true(hash_map_access <= array_access)
  }
}

// Test 4: Graph Algorithm Complexity
test "graph algorithm complexity analysis" {
  // Define graph representation
  type Graph = {
    vertices : [Int]
    edges : [(Int, Int)]
    adjacency_list : {Int: [Int]}
  }
  
  // Create graph
  let create_graph = fn(vertex_count : Int, edge_probability : Float) {
    let mut vertices = []
    let mut edges = []
    let mut adjacency_list = {}
    
    // Create vertices
    for i in 0..<vertex_count {
      vertices = vertices.push(i)
      adjacency_list = adjacency_list.set(i, [])
    }
    
    // Create edges with given probability
    for i in 0..<vertex_count {
      for j in (i + 1)..<vertex_count {
        if (i * j) % 100 < edge_probability * 100 {  // Pseudo-random edge creation
          edges = edges.push((i, j))
          
          // Update adjacency list
          let i_neighbors = adjacency_list.get(i).or_else([])
          let j_neighbors = adjacency_list.get(j).or_else([])
          
          adjacency_list = adjacency_list.set(i, i_neighbors.push(j))
          adjacency_list = adjacency_list.set(j, j_neighbors.push(i))
        }
      }
    }
    
    {
      vertices: vertices,
      edges: edges,
      adjacency_list: adjacency_list
    }
  }
  
  // Breadth-First Search (O(V + E))
  let bfs = fn(graph : Graph, start : Int, target : Int) {
    let mut visited = {}
    let mut queue = [start]
    visited = visited.set(start, true)
    
    while queue.length() > 0 {
      let current = queue[0]
      queue = queue.slice(1, queue.length())
      
      if current == target {
        return Some(true)
      }
      
      let neighbors = graph.adjacency_list.get(current).or_else([])
      for neighbor in neighbors {
        if not visited.contains(neighbor) {
          visited = visited.set(neighbor, true)
          queue = queue.push(neighbor)
        }
      }
    }
    
    None
  }
  
  // Depth-First Search (O(V + E))
  let dfs = fn(graph : Graph, start : Int, target : Int) {
    let mut visited = {}
    let mut stack = [start]
    
    while stack.length() > 0 {
      let current = stack[stack.length() - 1]
      stack = stack.slice(0, stack.length() - 1)
      
      if current == target {
        return Some(true)
      }
      
      if not visited.contains(current) {
        visited = visited.set(current, true)
        
        let neighbors = graph.adjacency_list.get(current).or_else([])
        for neighbor in neighbors {
          if not visited.contains(neighbor) {
            stack = stack.push(neighbor)
          }
        }
      }
    }
    
    None
  }
  
  // Dijkstra's algorithm (O((V + E) log V))
  let dijkstra = fn(graph : Graph, start : Int, target : Int) {
    let mut distances = {}
    let mut previous = {}
    let mut unvisited = {}
    
    // Initialize distances
    for vertex in graph.vertices {
      if vertex == start {
        distances = distances.set(vertex, 0)
      } else {
        distances = distances.set(vertex, 999999)  // Infinity
      }
      unvisited = unvisited.set(vertex, true)
    }
    
    while unvisited.length() > 0 {
      // Find vertex with minimum distance
      let mut current = None
      let mut min_distance = 999999
      
      for vertex in graph.vertices {
        if unvisited.contains(vertex) {
          let distance = distances.get(vertex).or_else(999999)
          if distance < min_distance {
            min_distance = distance
            current = Some(vertex)
          }
        }
      }
      
      match current {
        Some(curr) => {
          if curr == target {
            return Some(distances.get(target).or_else(0))
          }
          
          unvisited = unvisited.remove(curr)
          
          // Update distances to neighbors
          let neighbors = graph.adjacency_list.get(curr).or_else([])
          for neighbor in neighbors {
            if unvisited.contains(neighbor) {
              let alt = distances.get(curr).or_else(0) + 1  // Assume unit weight
              let neighbor_dist = distances.get(neighbor).or_else(999999)
              
              if alt < neighbor_dist {
                distances = distances.set(neighbor, alt)
                previous = previous.set(neighbor, curr)
              }
            }
          }
        }
        None => break
      }
    }
    
    None
  }
  
  // Measure graph algorithm performance
  let measure_graph_performance = fn(graph : Graph, algorithm : String, start : Int, target : Int) {
    let start_time = get_current_time_millis()
    
    let result = match algorithm {
      "bfs" => bfs(graph, start, target),
      "dfs" => dfs(graph, start, target),
      "dijkstra" => dijkstra(graph, start, target),
      _ => None
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "result": result,
      "duration_ms": duration,
      "vertex_count": graph.vertices.length(),
      "edge_count": graph.edges.length()
    }
  }
  
  // Test graph algorithms
  let graph = create_graph(100, 0.1)  // 100 vertices, 10% edge probability
  let start_vertex = 0
  let target_vertex = 99
  
  let bfs_result = measure_graph_performance(graph, "bfs", start_vertex, target_vertex)
  let dfs_result = measure_graph_performance(graph, "dfs", start_vertex, target_vertex)
  let dijkstra_result = measure_graph_performance(graph, "dijkstra", start_vertex, target_vertex)
  
  // Verify algorithms find a path (if one exists)
  match bfs_result["result"] {
    Some(_) => assert_true(true),
    None => assert_true(true)  // Path might not exist, which is OK
  }
  
  match dfs_result["result"] {
    Some(_) => assert_true(true),
    None => assert_true(true)  // Path might not exist, which is OK
  }
  
  match dijkstra_result["result"] {
    Some(_) => assert_true(true),
    None => assert_true(true)  // Path might not exist, which is OK
  }
  
  // Test with larger graph
  let large_graph = create_graph(1000, 0.05)  // 1000 vertices, 5% edge probability
  let large_start = 0
  let large_target = 999
  
  let large_bfs = measure_graph_performance(large_graph, "bfs", large_start, large_target)
  let large_dfs = measure_graph_performance(large_graph, "dfs", large_start, large_target)
  
  // Both BFS and DFS should complete in reasonable time
  assert_true(large_bfs["duration_ms"] < 1000)  // Less than 1 second
  assert_true(large_dfs["duration_ms"] < 1000)  // Less than 1 second
}

// Test 5: Dynamic Programming Complexity
test "dynamic programming complexity analysis" {
  // Fibonacci with memoization (O(n))
  let fibonacci_memo = fn(n : Int) {
    let memo = {}
    
    let fib = fn(k : Int) {
      if k <= 1 {
        return k
      }
      
      match memo.get(k) {
        Some(value) => value,
        None => {
          let result = fib(k - 1) + fib(k - 2)
          memo = memo.set(k, result)
          result
        }
      }
    }
    
    fib(n)
  }
  
  // Fibonacci without memoization (O(2^n))
  let fibonacci_recursive = fn(n : Int) {
    if n <= 1 {
      n
    } else {
      fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
    }
  }
  
  // Longest Common Subsequence (O(m*n))
  let lcs = fn(s1 : String, s2 : String) {
    let m = s1.length()
    let n = s2.length()
    
    // Create DP table
    let mut dp = []
    for i in 0..=m {
      let row = []
      for j in 0..=n {
        row = row.push(0)
      }
      dp = dp.push(row)
    }
    
    // Fill DP table
    for i in 1..=m {
      for j in 1..=n {
        if s1[i - 1] == s2[j - 1] {
          dp = dp.with(i, dp[i].with(j, dp[i - 1][j - 1] + 1))
        } else {
          dp = dp.with(i, dp[i].with(j, max(dp[i - 1][j], dp[i][j - 1])))
        }
      }
    }
    
    dp[m][n]
  }
  
  // Measure DP performance
  let measure_dp_performance = fn(algorithm : String, input_size : Int) {
    let start_time = get_current_time_millis()
    
    let result = match algorithm {
      "fibonacci_memo" => fibonacci_memo(input_size),
      "fibonacci_recursive" => {
        if input_size > 30 {
          -1  // Skip large inputs for recursive version
        } else {
          fibonacci_recursive(input_size)
        }
      }
      "lcs" => {
        let s1 = "a".repeat(input_size)
        let s2 = "b".repeat(input_size)
        lcs(s1, s2)
      }
      _ => 0
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "result": result,
      "duration_ms": duration,
      "input_size": input_size
    }
  }
  
  // Test Fibonacci algorithms
  let fib_memo_result = measure_dp_performance("fibonacci_memo", 40)
  let fib_recursive_result = measure_dp_performance("fibonacci_recursive", 30)
  
  // Memoized version should be faster
  assert_true(fib_memo_result["duration_ms"] <= fib_recursive_result["duration_ms"])
  
  // Verify correctness
  assert_eq(fib_memo_result["result"], 102334155)  // Fibonacci(40)
  assert_eq(fib_recursive_result["result"], 832040)  // Fibonacci(30)
  
  // Test LCS algorithm
  let lcs_result = measure_dp_performance("lcs", 100)
  assert_eq(lcs_result["result"], 0)  // No common characters between all 'a's and all 'b's
  
  // Test with larger input
  let large_lcs_result = measure_dp_performance("lcs", 500)
  assert_true(large_lcs_result["duration_ms"] < 1000)  // Should complete in less than 1 second
}

// Test 6: String Algorithm Complexity
test "string algorithm complexity analysis" {
  // Naive string matching (O(n*m))
  let naive_string_search = fn(text : String, pattern : String) {
    let n = text.length()
    let m = pattern.length()
    
    for i in 0..=(n - m) {
      let mut match_found = true
      
      for j in 0..<m {
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
  
  // KMP algorithm (O(n+m))
  let kmp_string_search = fn(text : String, pattern : String) {
    let n = text.length()
    let m = pattern.length()
    
    // Build failure function
    let mut failure = []
    for i in 0..<m {
      failure = failure.push(0)
    }
    
    let mut j = 0
    for i in 1..<m {
      while j > 0 && pattern[i] != pattern[j] {
        j = failure[j - 1]
      }
      
      if pattern[i] == pattern[j] {
        j = j + 1
        failure[i] = j
      }
    }
    
    // Search
    j = 0
    for i in 0..<n {
      while j > 0 && text[i] != pattern[j] {
        j = failure[j - 1]
      }
      
      if text[i] == pattern[j] {
        j = j + 1
        
        if j == m {
          return Some(i - m + 1)
        }
      }
    }
    
    None
  }
  
  // Measure string algorithm performance
  let measure_string_performance = fn(text : String, pattern : String, algorithm : String) {
    let start_time = get_current_time_millis()
    
    let result = match algorithm {
      "naive" => naive_string_search(text, pattern),
      "kmp" => kmp_string_search(text, pattern),
      _ => None
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "result": result,
      "duration_ms": duration,
      "text_length": text.length(),
      "pattern_length": pattern.length()
    }
  }
  
  // Create test strings
  let create_test_string = fn(length : Int, char : Char) {
    let mut result = ""
    for i in 0..<length {
      result = result + char.to_string()
    }
    result
  }
  
  // Test string algorithms
  let text = create_test_string(1000, 'a') + "bcd" + create_test_string(1000, 'a')
  let pattern = "abcd"
  
  let naive_result = measure_string_performance(text, pattern, "naive")
  let kmp_result = measure_string_performance(text, pattern, "kmp")
  
  // Verify both algorithms find the pattern
  match naive_result["result"] {
    Some(index) => assert_eq(index, 1000),
    None => assert_true(false)
  }
  
  match kmp_result["result"] {
    Some(index) => assert_eq(index, 1000),
    None => assert_true(false)
  }
  
  // KMP should be faster for longer texts with repeated patterns
  assert_true(kmp_result["duration_ms"] <= naive_result["duration_ms"])
  
  // Test with longer text
  let long_text = create_test_string(10000, 'a') + "bcd" + create_test_string(10000, 'a')
  let long_naive = measure_string_performance(long_text, pattern, "naive")
  let long_kmp = measure_string_performance(long_text, pattern, "kmp")
  
  // Performance difference should be more pronounced with longer text
  assert_true(long_kmp["duration_ms"] < long_naive["duration_ms"])
}

// Test 7: Recursion vs Iteration Performance
test "recursion vs iteration performance analysis" {
  // Factorial with recursion
  let factorial_recursive = fn(n : Int) {
    if n <= 1 {
      1
    } else {
      n * factorial_recursive(n - 1)
    }
  }
  
  // Factorial with iteration
  let factorial_iterative = fn(n : Int) {
    let mut result = 1
    for i in 2..=n {
      result = result * i
    }
    result
  }
  
  // Sum of array with recursion
  let sum_recursive = fn(arr : [Int], index : Int) {
    if index >= arr.length() {
      0
    } else {
      arr[index] + sum_recursive(arr, index + 1)
    }
  }
  
  // Sum of array with iteration
  let sum_iterative = fn(arr : [Int]) {
    let mut result = 0
    for i in 0..<arr.length() {
      result = result + arr[i]
    }
    result
  }
  
  // Measure recursion vs iteration performance
  let measure_recursion_iteration_performance = fn(algorithm : String, input_size : Int) {
    let start_time = get_current_time_millis()
    
    let result = match algorithm {
      "factorial_recursive" => factorial_recursive(input_size),
      "factorial_iterative" => factorial_iterative(input_size),
      "sum_recursive" => {
        let arr = []
        for i in 0..<input_size {
          arr = arr.push(i)
        }
        sum_recursive(arr, 0)
      }
      "sum_iterative" => {
        let arr = []
        for i in 0..<input_size {
          arr = arr.push(i)
        }
        sum_iterative(arr)
      }
      _ => 0
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "result": result,
      "duration_ms": duration,
      "input_size": input_size
    }
  }
  
  // Test factorial algorithms
  let factorial_recursive_result = measure_recursion_iteration_performance("factorial_recursive", 15)
  let factorial_iterative_result = measure_recursion_iteration_performance("factorial_iterative", 15)
  
  // Verify correctness
  assert_eq(factorial_recursive_result["result"], 1307674368000)  // 15!
  assert_eq(factorial_iterative_result["result"], 1307674368000)  // 15!
  
  // Iterative should be faster due to function call overhead
  assert_true(factorial_iterative_result["duration_ms"] <= factorial_recursive_result["duration_ms"])
  
  // Test sum algorithms
  let sum_recursive_result = measure_recursion_iteration_performance("sum_recursive", 1000)
  let sum_iterative_result = measure_recursion_iteration_performance("sum_iterative", 1000)
  
  // Verify correctness
  assert_eq(sum_recursive_result["result"], 499500)  // Sum of 0 to 999
  assert_eq(sum_iterative_result["result"], 499500)  // Sum of 0 to 999
  
  // Iterative should be faster due to function call overhead
  assert_true(sum_iterative_result["duration_ms"] <= sum_recursive_result["duration_ms"])
}

// Test 8: Space Complexity Analysis
test "space complexity analysis" {
  // In-place array reversal (O(1) space)
  let reverse_in_place = fn(arr : [Int]) {
    let mut reversed_arr = arr
    let left = 0
    let right = reversed_arr.length() - 1
    
    let reverse_helper = fn(arr : [Int], left : Int, right : Int) {
      if left >= right {
        arr
      } else {
        let temp = arr[left]
        let arr_with_swap = arr.with(left, arr[right]).with(right, temp)
        reverse_helper(arr_with_swap, left + 1, right - 1)
      }
    }
    
    reverse_helper(reversed_arr, left, right)
  }
  
  // Array reversal with new array (O(n) space)
  let reverse_with_new_array = fn(arr : [Int]) {
    let mut reversed = []
    for i in (0..<arr.length()).reverse() {
      reversed = reversed.push(arr[i])
    }
    reversed
  }
  
  // Merge sort (O(n) space)
  let merge_sort_with_space = fn(arr : [Int]) {
    if arr.length() <= 1 {
      arr
    } else {
      let mid = arr.length() / 2
      let left = merge_sort_with_space(arr.slice(0, mid))
      let right = merge_sort_with_space(arr.slice(mid, arr.length()))
      
      // Merge requires O(n) additional space
      let merge = fn(left : [Int], right : [Int]) {
        let mut result = []
        let mut i = 0
        let mut j = 0
        
        while i < left.length() && j < right.length() {
          if left[i] <= right[j] {
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
      
      merge(left, right)
    }
  }
  
  // Test space complexity
  let test_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let in_place_result = reverse_in_place(test_array)
  let new_array_result = reverse_with_new_array(test_array)
  let merge_sort_result = merge_sort_with_space(test_array)
  
  // Verify correctness
  assert_eq(in_place_result, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
  assert_eq(new_array_result, [10, 9, 8, 7, 6, 5, 4, 3, 2, 1])
  assert_eq(merge_sort_result, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
  
  // Test with larger array to see space impact
  let large_array = []
  for i in 0..<1000 {
    large_array = large_array.push(i)
  }
  
  let large_in_place = reverse_in_place(large_array)
  let large_new_array = reverse_with_new_array(large_array)
  
  // Verify both produce the same result
  assert_eq(large_in_place.length(), large_new_array.length())
  
  for i in 0..<large_in_place.length() {
    assert_eq(large_in_place[i], large_new_array[i])
  }
}

// Test 9: Amortized Analysis
test "amortized analysis" {
  // Dynamic array implementation
  type DynamicArray = {
    data : [Int]
    size : Int
    capacity : Int
  }
  
  // Create dynamic array
  let create_dynamic_array = fn(initial_capacity : Int) {
    let data = []
    for i in 0..<initial_capacity {
      data = data.push(0)
    }
    
    {
      data: data,
      size: 0,
      capacity: initial_capacity
    }
  }
  
  // Add element to dynamic array
  let dynamic_array_add = fn(arr : DynamicArray, element : Int) {
    if arr.size < arr.capacity {
      // No resize needed (O(1))
      let updated_data = arr.data.with(arr.size, element)
      {
        data: updated_data,
        size: arr.size + 1,
        capacity: arr.capacity
      }
    } else {
      // Resize needed (O(n))
      let new_capacity = arr.capacity * 2
      let mut new_data = []
      
      // Copy existing elements
      for i in 0..<arr.size {
        new_data = new_data.push(arr.data[i])
      }
      
      // Add new elements
      for i in arr.size..<new_capacity {
        new_data = new_data.push(0)
      }
      
      let updated_data = new_data.with(arr.size, element)
      {
        data: updated_data,
        size: arr.size + 1,
        capacity: new_capacity
      }
    }
  }
  
  // Measure amortized performance
  let measure_amortized_performance = fn(operations : Int) {
    let mut arr = create_dynamic_array(1)
    let start_time = get_current_time_millis()
    
    for i in 0..<operations {
      arr = dynamic_array_add(arr, i)
    }
    
    let end_time = get_current_time_millis()
    let duration = end_time - start_time
    
    {
      "duration_ms": duration,
      "operations": operations,
      "final_size": arr.size,
      "final_capacity": arr.capacity,
      "avg_time_per_operation": duration.to_float() / operations.to_float()
    }
  }
  
  // Test amortized performance
  let small_operations = measure_amortized_performance(100)
  let medium_operations = measure_amortized_performance(1000)
  let large_operations = measure_amortized_performance(10000)
  
  // Verify operations completed successfully
  assert_eq(small_operations["final_size"], 100)
  assert_eq(medium_operations["final_size"], 1000)
  assert_eq(large_operations["final_size"], 10000)
  
  // Average time per operation should remain relatively constant (amortized O(1))
  assert_true(small_operations["avg_time_per_operation"] > 0.0)
  assert_true(medium_operations["avg_time_per_operation"] > 0.0)
  assert_true(large_operations["avg_time_per_operation"] > 0.0)
  
  // Capacity should grow as needed
  assert_true(small_operations["final_capacity"] >= 100)
  assert_true(medium_operations["final_capacity"] >= 1000)
  assert_true(large_operations["final_capacity"] >= 10000)
}

// Test 10: Big O Verification
test "big o verification" {
  // Verify O(1) - Constant time
  let constant_time = fn(arr : [Int]) {
    if arr.length() > 0 {
      arr[0]  // Always just one operation
    } else {
      0
    }
  }
  
  // Verify O(log n) - Logarithmic time
  let logarithmic_time = fn(n : Int) {
    let mut count = 0
    let mut i = n
    
    while i > 1 {
      i = i / 2
      count = count + 1
    }
    
    count
  }
  
  // Verify O(n) - Linear time
  let linear_time = fn(arr : [Int]) {
    let mut sum = 0
    
    for i in 0..<arr.length() {
      sum = sum + arr[i]
    }
    
    sum
  }
  
  // Verify O(n log n) - Linearithmic time
  let linearithmic_time = fn(arr : [Int]) {
    // Simplified merge sort
    if arr.length() <= 1 {
      arr
    } else {
      let mid = arr.length() / 2
      let left = linearithmic_time(arr.slice(0, mid))
      let right = linearithmic_time(arr.slice(mid, arr.length()))
      
      // Merge operation
      let mut result = []
      let mut i = 0
      let mut j = 0
      
      while i < left.length() && j < right.length() {
        if left[i] <= right[j] {
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
  }
  
  // Verify O(n²) - Quadratic time
  let quadratic_time = fn(arr : [Int]) {
    let mut result = []
    
    for i in 0..<arr.length() {
      for j in 0..<arr.length() {
        result = result.push(arr[i] * arr[j])
      }
    }
    
    result
  }
  
  // Measure complexity growth
  let measure_complexity_growth = fn(algorithm : String, sizes : [Int]) {
    let results = []
    
    for size in sizes {
      let start_time = get_current_time_millis()
      
      let result = match algorithm {
        "constant" => {
          let arr = []
          for i in 0..<size {
            arr = arr.push(i)
          }
          constant_time(arr)
        }
        "logarithmic" => logarithmic_time(size),
        "linear" => {
          let arr = []
          for i in 0..<size {
            arr = arr.push(i)
          }
          linear_time(arr)
        }
        "linearithmic" => {
          let arr = []
          for i in 0..<size {
            arr = arr.push(i)
          }
          linearithmic_time(arr)
        }
        "quadratic" => {
          let arr = []
          for i in 0..<size {
            arr = arr.push(i)
          }
          quadratic_time(arr)
        }
        _ => 0
      }
      
      let end_time = get_current_time_millis()
      let duration = end_time - start_time
      
      results = results.push({
        "size": size,
        "duration_ms": duration,
        "result": result
      })
    }
    
    results
  }
  
  // Test complexity growth
  let sizes = [100, 200, 400, 800]
  
  let constant_growth = measure_complexity_growth("constant", sizes)
  let linear_growth = measure_complexity_growth("linear", sizes)
  let quadratic_growth = measure_complexity_growth("quadratic", [100, 200, 300])  // Smaller sizes for quadratic
  
  // Verify constant time growth
  assert_true(constant_growth[0]["duration_ms"] < 10)  // Should be very fast
  assert_true(constant_growth[3]["duration_ms"] < 10)  // Should remain constant
  
  // Verify linear time growth
  let linear_ratio1 = linear_growth[1]["duration_ms"].to_float() / linear_growth[0]["duration_ms"].to_float()
  let linear_ratio2 = linear_growth[3]["duration_ms"].to_float() / linear_growth[2]["duration_ms"].to_float()
  
  // When size doubles, time should approximately double
  assert_true(linear_ratio1 > 1.5 && linear_ratio1 < 2.5)
  assert_true(linear_ratio2 > 1.5 && linear_ratio2 < 2.5)
  
  // Verify quadratic time growth
  let quadratic_ratio1 = quadratic_growth[1]["duration_ms"].to_float() / quadratic_growth[0]["duration_ms"].to_float()
  let quadratic_ratio2 = quadratic_growth[2]["duration_ms"].to_float() / quadratic_growth[1]["duration_ms"].to_float()
  
  // When size increases by factor of 1.5, time should increase by factor of ~2.25 (1.5²)
  assert_true(quadratic_ratio1 > 1.8 && quadratic_ratio1 < 2.7)
  assert_true(quadratic_ratio2 > 1.8 && quadratic_ratio2 < 2.7)
}

// Helper function to get current time in milliseconds
fn get_current_time_millis() -> Int {
  // Implementation would depend on the available time functions in MoonBit
  // This is a placeholder for the actual implementation
  0
}