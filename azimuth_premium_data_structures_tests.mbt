// Azimuth Premium Test Suite - Advanced Data Structures and Algorithms
// This file contains comprehensive test cases for advanced data structures and algorithmic operations

// Test 1: Tree Data Structures and Traversal Algorithms
test "tree data structures and traversal algorithms" {
  // Define binary tree node structure
  let create_node = |value, left, right| {
    { value: value, left: left, right: right }
  }
  
  // Build a sample binary tree
  //       5
  //      / \
  //     3   8
  //    / \   \
  //   1   4   9
  let tree = create_node(
    5,
    create_node(
      3,
      create_node(1, None, None),
      create_node(4, None, None)
    ),
    create_node(
      8,
      None,
      create_node(9, None, None)
    )
  )
  
  // In-order traversal (left, root, right)
  let inorder_traversal = |node| {
    match node {
      None => []
      Some(n) => inorder_traversal(n.left) + [n.value] + inorder_traversal(n.right)
    }
  }
  
  // Pre-order traversal (root, left, right)
  let preorder_traversal = |node| {
    match node {
      None => []
      Some(n) => [n.value] + preorder_traversal(n.left) + preorder_traversal(n.right)
    }
  }
  
  // Post-order traversal (left, right, root)
  let postorder_traversal = |node| {
    match node {
      None => []
      Some(n) => postorder_traversal(n.left) + postorder_traversal(n.right) + [n.value]
    }
  }
  
  // Test traversals
  let inorder_result = inorder_traversal(Some(tree))
  let preorder_result = preorder_traversal(Some(tree))
  let postorder_result = postorder_traversal(Some(tree))
  
  assert_eq(inorder_result, [1, 3, 4, 5, 8, 9])
  assert_eq(preorder_result, [5, 3, 1, 4, 8, 9])
  assert_eq(postorder_result, [1, 4, 3, 9, 8, 5])
  
  // Test binary search tree validation
  let is_bst = |node, min_val, max_val| {
    match node {
      None => true
      Some(n) => {
        let left_valid = is_bst(n.left, min_val, n.value)
        let right_valid = is_bst(n.right, n.value, max_val)
        let current_valid = n.value > min_val && n.value < max_val
        left_valid && current_valid && right_valid
      }
    }
  }
  
  assert_true(is_bst(Some(tree), -999999, 999999))
  
  // Test tree height calculation
  let tree_height = |node| {
    match node {
      None => 0
      Some(n) => {
        let left_height = tree_height(n.left)
        let right_height = tree_height(n.right)
        1 + if left_height > right_height { left_height } else { right_height }
      }
    }
  }
  
  assert_eq(tree_height(Some(tree)), 3)
  
  // Test tree search
  let tree_search = |node, target| {
    match node {
      None => false
      Some(n) => {
        if n.value == target {
          true
        } else if target < n.value {
          tree_search(n.left, target)
        } else {
          tree_search(n.right, target)
        }
      }
    }
  }
  
  assert_true(tree_search(Some(tree), 4))
  assert_true(tree_search(Some(tree), 9))
  assert_false(tree_search(Some(tree), 7))
}

// Test 2: Graph Data Structures and Algorithms
test "graph data structures and algorithms" {
  // Define graph structure using adjacency list
  let create_graph = |vertices, edges| {
    let mut adjacency_list = {}
    
    // Initialize empty adjacency lists for all vertices
    for vertex in vertices {
      adjacency_list = adjacency_list + { vertex: [] }
    }
    
    // Add edges to adjacency lists
    for (from, to) in edges {
      let current_neighbors = adjacency_list[from]
      adjacency_list = adjacency_list + { from: current_neighbors + [to] }
    }
    
    adjacency_list
  }
  
  // Create a sample directed graph
  let vertices = [1, 2, 3, 4, 5]
  let edges = [(1, 2), (1, 3), (2, 4), (3, 4), (4, 5)]
  let graph = create_graph(vertices, edges)
  
  // Test graph structure
  assert_eq(graph[1], [2, 3])
  assert_eq(graph[2], [4])
  assert_eq(graph[3], [4])
  assert_eq(graph[4], [5])
  assert_eq(graph[5], [])
  
  // Depth-First Search (DFS)
  let dfs = |graph, start, visited| {
    let mut stack = [start]
    let mut visited_set = visited
    let mut result = []
    
    while stack.length() > 0 {
      let current = stack.pop()
      
      if !visited_set.contains(current) {
        visited_set = visited_set + [current]
        result = result + [current]
        
        // Add neighbors to stack (in reverse order for correct traversal)
        let neighbors = graph[current]
        for i = neighbors.length() - 1; i >= 0; i = i - 1 {
          if !visited_set.contains(neighbors[i]) {
            stack.push(neighbors[i])
          }
        }
      }
    }
    
    (result, visited_set)
  }
  
  let (dfs_result, _) = dfs(graph, 1, [])
  assert_eq(dfs_result, [1, 2, 4, 5, 3])  // DFS traversal order
  
  // Breadth-First Search (BFS)
  let bfs = |graph, start| {
    let mut queue = [start]
    let mut visited = [start]
    let mut result = []
    
    while queue.length() > 0 {
      let current = queue.shift()
      result = result + [current]
      
      for neighbor in graph[current] {
        if !visited.contains(neighbor) {
          visited = visited + [neighbor]
          queue.push(neighbor)
        }
      }
    }
    
    result
  }
  
  let bfs_result = bfs(graph, 1)
  assert_eq(bfs_result, [1, 2, 3, 4, 5])  // BFS traversal order
  
  // Test path finding
  let find_path = |graph, start, end| {
    let mut queue = [(start, [start])]
    let mut visited = [start]
    
    while queue.length() > 0 {
      let (current, path) = queue.shift()
      
      if current == end {
        return Some(path)
      }
      
      for neighbor in graph[current] {
        if !visited.contains(neighbor) {
          visited = visited + [neighbor]
          queue.push((neighbor, path + [neighbor]))
        }
      }
    }
    
    None
  }
  
  let path_1_to_5 = find_path(graph, 1, 5)
  match path_1_to_5 {
    Some(p) => assert_eq(p, [1, 2, 4, 5])
    None => assert_true(false)
  }
  
  let path_3_to_5 = find_path(graph, 3, 5)
  match path_3_to_5 {
    Some(p) => assert_eq(p, [3, 4, 5])
    None => assert_true(false)
  }
  
  let no_path = find_path(graph, 5, 1)
  assert_eq(no_path, None)  // No path from 5 to 1 in directed graph
  
  // Test cycle detection
  let has_cycle = |graph, start, visited, rec_stack| {
    if rec_stack.contains(start) {
      return true
    }
    
    if visited.contains(start) {
      return false
    }
    
    let mut new_visited = visited + [start]
    let mut new_rec_stack = rec_stack + [start]
    
    for neighbor in graph[start] {
      if has_cycle(graph, neighbor, new_visited, new_rec_stack) {
        return true
      }
    }
    
    false
  }
  
  // Create a graph with a cycle
  let cyclic_edges = [(1, 2), (2, 3), (3, 4), (4, 2)]  // Cycle: 2 -> 3 -> 4 -> 2
  let cyclic_graph = create_graph(vertices, cyclic_edges)
  
  assert_false(has_cycle(graph, 1, [], []))  // Original graph has no cycles
  assert_true(has_cycle(cyclic_graph, 1, [], []))  // Cyclic graph has a cycle
}

// Test 3: Hash Table and Dictionary Operations
test "hash table and dictionary operations" {
  // Create a simple hash table implementation using arrays
  let create_hash_table = |size| {
    let buckets = []
    for i = 0; i < size; i = i + 1 {
      buckets.push([])
    }
    buckets
  }
  
  let hash_function = |key, table_size| {
    // Simple hash function for strings
    let mut hash = 0
    for i = 0; i < key.length(); i = i + 1 {
      hash = (hash + key.char_code_at(i)) % table_size
    }
    hash
  }
  
  let hash_table_insert = |table, key, value| {
    let table_size = table.length()
    let index = hash_function(key, table_size)
    let bucket = table[index]
    
    // Check if key already exists in bucket
    let mut updated_bucket = []
    let mut key_found = false
    
    for (k, v) in bucket {
      if k == key {
        updated_bucket.push((key, value))  // Update existing key
        key_found = true
      } else {
        updated_bucket.push((k, v))  // Keep existing pair
      }
    }
    
    if !key_found {
      updated_bucket.push((key, value))  // Add new key-value pair
    }
    
    // Update the bucket in the table
    let mut updated_table = table
    updated_table[index] = updated_bucket
    updated_table
  }
  
  let hash_table_get = |table, key| {
    let table_size = table.length()
    let index = hash_function(key, table_size)
    let bucket = table[index]
    
    for (k, v) in bucket {
      if k == key {
        return Some(v)
      }
    }
    
    None
  }
  
  // Test hash table operations
  let hash_table = create_hash_table(10)
  let table1 = hash_table_insert(hash_table, "apple", 5)
  let table2 = hash_table_insert(table1, "banana", 3)
  let table3 = hash_table_insert(table2, "orange", 7)
  let table4 = hash_table_insert(table3, "grape", 2)
  
  // Test retrieval
  assert_eq(hash_table_get(table4, "apple"), Some(5))
  assert_eq(hash_table_get(table4, "banana"), Some(3))
  assert_eq(hash_table_get(table4, "orange"), Some(7))
  assert_eq(hash_table_get(table4, "grape"), Some(2))
  assert_eq(hash_table_get(table4, "pear"), None)
  
  // Test update operation
  let table5 = hash_table_insert(table4, "apple", 10)  // Update existing key
  assert_eq(hash_table_get(table5, "apple"), Some(10))
  
  // Test collision handling
  let table6 = hash_table_insert(table5, "bpple", 15)  // Should collide with "apple"
  assert_eq(hash_table_get(table6, "apple"), Some(10))
  assert_eq(hash_table_get(table6, "bpple"), Some(15))
  
  // Test dictionary-style operations
  let dict_merge = |dict1, dict2| {
    let mut merged = dict1
    for (key, value) in dict2 {
      merged = hash_table_insert(merged, key, value)
    }
    merged
  }
  
  let dict1 = hash_table_insert(create_hash_table(5), "x", 1)
  let dict1 = hash_table_insert(dict1, "y", 2)
  let dict2 = hash_table_insert(create_hash_table(5), "y", 3)  // Override
  let dict2 = hash_table_insert(dict2, "z", 4)
  
  let merged_dict = dict_merge(dict1, dict2)
  assert_eq(hash_table_get(merged_dict, "x"), Some(1))
  assert_eq(hash_table_get(merged_dict, "y"), Some(3))  // Overridden
  assert_eq(hash_table_get(merged_dict, "z"), Some(4))
}

// Test 4: Advanced Sorting and Searching Algorithms
test "advanced sorting and searching algorithms" {
  // Test quicksort algorithm
  let quicksort = |arr| {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[0]
      let less = arr.slice(1, arr.length()).filter(|x| x <= pivot)
      let greater = arr.slice(1, arr.length()).filter(|x| x > pivot)
      quicksort(less) + [pivot] + quicksort(greater)
    }
  }
  
  // Test merge sort algorithm
  let merge = |left, right| {
    let mut result = []
    let mut i = 0
    let mut j = 0
    
    while i < left.length() && j < right.length() {
      if left[i] <= right[j] {
        result.push(left[i])
        i = i + 1
      } else {
        result.push(right[j])
        j = j + 1
      }
    }
    
    // Add remaining elements
    while i < left.length() {
      result.push(left[i])
      i = i + 1
    }
    
    while j < right.length() {
      result.push(right[j])
      j = j + 1
    }
    
    result
  }
  
  let mergesort = |arr| {
    if arr.length() <= 1 {
      arr
    } else {
      let mid = arr.length() / 2
      let left = mergesort(arr.slice(0, mid))
      let right = mergesort(arr.slice(mid, arr.length()))
      merge(left, right)
    }
  }
  
  // Test heap sort algorithm
  let heapify = |arr, n, i| {
    let mut largest = i
    let left = 2 * i + 1
    let right = 2 * i + 2
    
    if left < n && arr[left] > arr[largest] {
      largest = left
    }
    
    if right < n && arr[right] > arr[largest] {
      largest = right
    }
    
    if largest != i {
      let temp = arr[i]
      arr[i] = arr[largest]
      arr[largest] = temp
      heapify(arr, n, largest)
    }
  }
  
  let heapsort = |arr| {
    let n = arr.length()
    let mut sorted_arr = arr.copy()
    
    // Build max heap
    for i = n / 2 - 1; i >= 0; i = i - 1 {
      heapify(sorted_arr, n, i)
    }
    
    // Extract elements from heap
    for i = n - 1; i > 0; i = i - 1 {
      let temp = sorted_arr[0]
      sorted_arr[0] = sorted_arr[i]
      sorted_arr[i] = temp
      heapify(sorted_arr, i, 0)
    }
    
    sorted_arr
  }
  
  // Test sorting algorithms
  let unsorted_array = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
  
  let quicksorted = quicksort(unsorted_array)
  let mergesorted = mergesort(unsorted_array)
  let heapsorted = heapsort(unsorted_array)
  
  // Verify all sorting algorithms produce the same result
  assert_eq(quicksorted, [11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90])
  assert_eq(mergesorted, quicksorted)
  assert_eq(heapsorted, quicksorted)
  
  // Test binary search algorithm
  let binary_search = |arr, target| {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right {
      let mid = (left + right) / 2
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
  
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25]
  
  assert_eq(binary_search(sorted_array, 7), Some(3))
  assert_eq(binary_search(sorted_array, 1), Some(0))
  assert_eq(binary_search(sorted_array, 25), Some(12))
  assert_eq(binary_search(sorted_array, 8), None)
  assert_eq(binary_search(sorted_array, 0), None)
  assert_eq(binary_search(sorted_array, 26), None)
  
  // Test interpolation search (for uniformly distributed data)
  let interpolation_search = |arr, target| {
    let mut left = 0
    let mut right = arr.length() - 1
    
    while left <= right && target >= arr[left] && target <= arr[right] {
      if left == right {
        if arr[left] == target {
          return Some(left)
        } else {
          return None
        }
      }
      
      // Estimate position
      let pos = left + ((target - arr[left]) * (right - left)) / (arr[right] - arr[left])
      
      if arr[pos] == target {
        return Some(pos)
      } else if arr[pos] < target {
        left = pos + 1
      } else {
        right = pos - 1
      }
    }
    
    None
  }
  
  let uniform_array = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  
  assert_eq(interpolation_search(uniform_array, 30), Some(2))
  assert_eq(interpolation_search(uniform_array, 10), Some(0))
  assert_eq(interpolation_search(uniform_array, 100), Some(9))
  assert_eq(interpolation_search(uniform_array, 35), None)
}

// Test 5: Dynamic Programming and Memoization
test "dynamic programming and memoization" {
  // Fibonacci with memoization
  let fibonacci_memo = |n| {
    let memo = {}
    
    let fib = |k| {
      if memo.contains(k) {
        return memo[k]
      }
      
      let result = if k <= 1 {
        k
      } else {
        fib(k - 1) + fib(k - 2)
      }
      
      memo[k] = result
      result
    }
    
    fib(n)
  }
  
  // Test memoized Fibonacci
  assert_eq(fibonacci_memo(0), 0)
  assert_eq(fibonacci_memo(1), 1)
  assert_eq(fibonacci_memo(5), 5)  // 0, 1, 1, 2, 3, 5
  assert_eq(fibonacci_memo(10), 55)
  assert_eq(fibonacci_memo(15), 610)
  
  // Longest Common Subsequence (LCS)
  let lcs = |s1, s2| {
    let m = s1.length()
    let n = s2.length()
    
    // Create DP table
    let mut dp = []
    for i = 0; i <= m; i = i + 1 {
      let row = []
      for j = 0; j <= n; j = j + 1 {
        row.push(0)
      }
      dp.push(row)
    }
    
    // Fill DP table
    for i = 1; i <= m; i = i + 1 {
      for j = 1; j <= n; j = j + 1 {
        if s1[i - 1] == s2[j - 1] {
          dp[i][j] = dp[i - 1][j - 1] + 1
        } else {
          dp[i][j] = if dp[i - 1][j] > dp[i][j - 1] { dp[i - 1][j] } else { dp[i][j - 1] }
        }
      }
    }
    
    // Reconstruct LCS
    let mut i = m
    let mut j = n
    let mut lcs_string = ""
    
    while i > 0 && j > 0 {
      if s1[i - 1] == s2[j - 1] {
        lcs_string = s1[i - 1] + lcs_string
        i = i - 1
        j = j - 1
      } else if dp[i - 1][j] > dp[i][j - 1] {
        i = i - 1
      } else {
        j = j - 1
      }
    }
    
    lcs_string
  }
  
  // Test LCS
  assert_eq(lcs("ABCBDAB", "BDCABA"), "BCBA")
  assert_eq(lcs("AGGTAB", "GXTXAYB"), "GTAB")
  assert_eq(lcs("ABCDE", "ACE"), "ACE")
  assert_eq(lcs("XMJYAUZ", "MZJAWXU"), "MJAU")
  
  // Coin Change Problem
  let coin_change = |coins, amount| {
    let n = coins.length()
    
    // Create DP table
    let mut dp = []
    for i = 0; i <= n; i = i + 1 {
      let row = []
      for j = 0; j <= amount; j = j + 1 {
        if j == 0 {
          row.push(1)  // One way to make amount 0 (no coins)
        } else {
          row.push(0)
        }
      }
      dp.push(row)
    }
    
    // Fill DP table
    for i = 1; i <= n; i = i + 1 {
      for j = 1; j <= amount; j = j + 1 {
        // Solutions without current coin
        dp[i][j] = dp[i - 1][j]
        
        // Solutions with current coin
        if j >= coins[i - 1] {
          dp[i][j] = dp[i][j] + dp[i][j - coins[i - 1]]
        }
      }
    }
    
    dp[n][amount]
  }
  
  // Test Coin Change
  assert_eq(coin_change([1, 2, 5], 5), 4)  // [5], [2,2,1], [2,1,1,1], [1,1,1,1,1]
  assert_eq(coin_change([2, 3, 7], 12), 4)  // [2,2,2,2,2,2], [2,2,2,3,3], [3,3,3,3], [2,2,3,5]
  assert_eq(coin_change([1, 3, 4, 5], 7), 6)
  
  // 0/1 Knapsack Problem
  let knapsack = |weights, values, capacity| {
    let n = weights.length()
    
    // Create DP table
    let mut dp = []
    for i = 0; i <= n; i = i + 1 {
      let row = []
      for j = 0; j <= capacity; j = j + 1 {
        row.push(0)
      }
      dp.push(row)
    }
    
    // Fill DP table
    for i = 1; i <= n; i = i + 1 {
      for j = 1; j <= capacity; j = j + 1 {
        if weights[i - 1] <= j {
          dp[i][j] = if dp[i - 1][j] > dp[i - 1][j - weights[i - 1]] + values[i - 1] {
            dp[i - 1][j]
          } else {
            dp[i - 1][j - weights[i - 1]] + values[i - 1]
          }
        } else {
          dp[i][j] = dp[i - 1][j]
        }
      }
    }
    
    dp[n][capacity]
  }
  
  // Test Knapsack
  assert_eq(knapsack([1, 3, 4, 5], [1, 4, 5, 7], 7), 9)  // Items 2 and 4 (3+4 weight, 4+7 value)
  assert_eq(knapsack([2, 3, 4, 5], [3, 4, 5, 6], 5), 7)   // Items 1 and 2 (2+3 weight, 3+4 value)
  assert_eq(knapsack([1, 2, 3], [10, 15, 40], 5), 55)     // Items 2 and 3 (2+3 weight, 15+40 value)
}

// Test 6: Advanced Linked List Operations
test "advanced linked list operations" {
  // Define linked list node structure
  let create_node = |value, next| {
    { value: value, next: next }
  }
  
  // Create a sample linked list: 1 -> 2 -> 3 -> 4 -> 5
  let list = create_node(1, create_node(2, create_node(3, create_node(4, create_node(5, None)))))
  
  // Convert linked list to array for testing
  let linked_list_to_array = |node| {
    let mut result = []
    let mut current = Some(node)
    
    while current.is_some() {
      match current {
        Some(n) => {
          result.push(n.value)
          current = n.next
        }
        None => break
      }
    }
    
    result
  }
  
  // Test linked list to array conversion
  assert_eq(linked_list_to_array(list), [1, 2, 3, 4, 5])
  
  // Reverse linked list
  let reverse_linked_list = |node| {
    let mut prev = None
    let mut current = Some(node)
    
    while current.is_some() {
      match current {
        Some(n) => {
          let next = n.next
          let reversed_node = create_node(n.value, prev)
          prev = Some(reversed_node)
          current = next
        }
        None => break
      }
    }
    
    prev
  }
  
  let reversed_list = reverse_linked_list(list)
  assert_eq(linked_list_to_array(reversed_list.unwrap()), [5, 4, 3, 2, 1])
  
  // Find middle node (using slow and fast pointers)
  let find_middle = |node| {
    let mut slow = Some(node)
    let mut fast = Some(node)
    
    while fast.is_some() {
      match fast {
        Some(f) => {
          fast = f.next
          if fast.is_some() {
            match fast {
              Some(f2) => {
                fast = f2.next
                match slow {
                  Some(s) => slow = s.next
                  None => break
                }
              }
              None => break
            }
          }
        }
        None => break
      }
    }
    
    slow
  }
  
  let middle_node = find_middle(list)
  match middle_node {
    Some(n) => assert_eq(n.value, 3)
    None => assert_true(false)
  }
  
  // Detect cycle in linked list
  let create_cyclic_list = || {
    let node1 = create_node(1, None)
    let node2 = create_node(2, Some(node1))
    let node3 = create_node(3, Some(node2))
    let node4 = create_node(4, Some(node3))
    let node5 = create_node(5, Some(node4))
    
    // Create cycle: node5 -> node3 -> node4 -> node5 ...
    node1.next = Some(node3)
    
    Some(node5)
  }
  
  let has_cycle = |node| {
    let mut slow = node
    let mut fast = node
    
    while fast.is_some() {
      match fast {
        Some(f) => {
          fast = f.next
          if fast.is_some() {
            match fast {
              Some(f2) => {
                fast = f2.next
                match slow {
                  Some(s) => slow = s.next
                  None => break
                }
              }
              None => break
            }
          }
        }
        None => break
      }
      
      if slow == fast {
        return true
      }
    }
    
    false
  }
  
  assert_false(has_cycle(Some(list)))  // Regular list has no cycle
  assert_true(has_cycle(create_cyclic_list()))  // Cyclic list has cycle
  
  // Merge two sorted linked lists
  let merge_sorted_lists = |list1, list2| {
    let mut dummy = create_node(0, None)
    let mut tail = Some(dummy)
    let mut l1 = list1
    let mut l2 = list2
    
    while l1.is_some() && l2.is_some() {
      match (l1, l2) {
        (Some(node1), Some(node2)) => {
          if node1.value <= node2.value {
            match tail {
              Some(t) => {
                t.next = Some(node1)
                tail = node1.next
              }
              None => break
            }
            l1 = node1.next
          } else {
            match tail {
              Some(t) => {
                t.next = Some(node2)
                tail = node2.next
              }
              None => break
            }
            l2 = node2.next
          }
        }
        _ => break
      }
    }
    
    // Attach remaining elements
    if l1.is_some() {
      match tail {
        Some(t) => t.next = l1
        None => {}
      }
    } else if l2.is_some() {
      match tail {
        Some(t) => t.next = l2
        None => {}
      }
    }
    
    match dummy.next {
      Some(n) => n.next
      None => None
    }
  }
  
  // Create two sorted lists
  let list1 = create_node(1, create_node(3, create_node(5, None)))
  let list2 = create_node(2, create_node(4, create_node(6, None)))
  
  let merged_list = merge_sorted_lists(Some(list1), Some(list2))
  assert_eq(linked_list_to_array(merged_list.unwrap()), [1, 2, 3, 4, 5, 6])
}

// Test 7: Stack and Queue Operations
test "stack and queue operations" {
  // Stack implementation using array
  let create_stack = || {
    { elements: [], top: -1 }
  }
  
  let stack_push = |stack, element| {
    { elements: stack.elements + [element], top: stack.top + 1 }
  }
  
  let stack_pop = |stack| {
    if stack.top >= 0 {
      let element = stack.elements[stack.top]
      let new_elements = stack.elements.slice(0, stack.top)
      (Some(element), { elements: new_elements, top: stack.top - 1 })
    } else {
      (None, stack)
    }
  }
  
  let stack_peek = |stack| {
    if stack.top >= 0 {
      Some(stack.elements[stack.top])
    } else {
      None
    }
  }
  
  let stack_is_empty = |stack| {
    stack.top < 0
  }
  
  // Test stack operations
  let mut stack = create_stack()
  assert_true(stack_is_empty(stack))
  
  stack = stack_push(stack, 10)
  stack = stack_push(stack, 20)
  stack = stack_push(stack, 30)
  
  assert_false(stack_is_empty(stack))
  assert_eq(stack_peek(stack), Some(30))
  
  let (popped, new_stack) = stack_pop(stack)
  match popped {
    Some(value) => assert_eq(value, 30)
    None => assert_true(false)
  }
  
  assert_eq(stack_peek(new_stack), Some(20))
  
  let (popped2, new_stack2) = stack_pop(new_stack)
  match popped2 {
    Some(value) => assert_eq(value, 20)
    None => assert_true(false)
  }
  
  assert_eq(stack_peek(new_stack2), Some(10))
  
  // Queue implementation using array
  let create_queue = || {
    { elements: [], front: 0, rear: -1, size: 0 }
  }
  
  let queue_enqueue = |queue, element| {
    let new_elements = queue.elements + [element]
    { elements: new_elements, front: queue.front, rear: queue.rear + 1, size: queue.size + 1 }
  }
  
  let queue_dequeue = |queue| {
    if queue.size > 0 {
      let element = queue.elements[queue.front]
      let new_elements = queue.elements.slice(queue.front + 1, queue.elements.length())
      (Some(element), { elements: new_elements, front: 0, rear: queue.rear - 1, size: queue.size - 1 })
    } else {
      (None, queue)
    }
  }
  
  let queue_front = |queue| {
    if queue.size > 0 {
      Some(queue.elements[queue.front])
    } else {
      None
    }
  }
  
  let queue_is_empty = |queue| {
    queue.size == 0
  }
  
  // Test queue operations
  let mut queue = create_queue()
  assert_true(queue_is_empty(queue))
  
  queue = queue_enqueue(queue, "first")
  queue = queue_enqueue(queue, "second")
  queue = queue_enqueue(queue, "third")
  
  assert_false(queue_is_empty(queue))
  assert_eq(queue_front(queue), Some("first"))
  
  let (dequeued, new_queue) = queue_dequeue(queue)
  match dequeued {
    Some(value) => assert_eq(value, "first")
    None => assert_true(false)
  }
  
  assert_eq(queue_front(new_queue), Some("second"))
  
  let (dequeued2, new_queue2) = queue_dequeue(new_queue)
  match dequeued2 {
    Some(value) => assert_eq(value, "second")
    None => assert_true(false)
  }
  
  assert_eq(queue_front(new_queue2), Some("third"))
  
  // Test stack for palindrome checking
  let is_palindrome = |str| {
    let mut stack = create_stack()
    
    // Push all characters onto stack
    for i = 0; i < str.length(); i = i + 1 {
      stack = stack_push(stack, str[i])
    }
    
    // Pop and compare
    for i = 0; i < str.length(); i = i + 1 {
      let (popped, new_stack) = stack_pop(stack)
      match popped {
        Some(char) => {
          if char != str[i] {
            return false
          }
          stack = new_stack
        }
        None => return false
      }
    }
    
    true
  }
  
  assert_true(is_palindrome("racecar"))
  assert_true(is_palindrome("madam"))
  assert_false(is_palindrome("hello"))
  assert_true(is_palindrome(""))
  assert_true(is_palindrome("a"))
  
  // Test queue for task scheduling
  let process_tasks = |tasks| {
    let mut queue = create_queue()
    
    // Enqueue all tasks
    for task in tasks {
      queue = queue_enqueue(queue, task)
    }
    
    let mut processed = []
    
    // Process all tasks
    while !queue_is_empty(queue) {
      let (task, new_queue) = queue_dequeue(queue)
      match task {
        Some(t) => {
          processed = processed + [t + "_processed"]
          queue = new_queue
        }
        None => break
      }
    }
    
    processed
  }
  
  let tasks = ["task1", "task2", "task3"]
  let processed_tasks = process_tasks(tasks)
  
  assert_eq(processed_tasks, ["task1_processed", "task2_processed", "task3_processed"])
}

// Test 8: Priority Queue and Heap Operations
test "priority queue and heap operations" {
  // Min-heap implementation
  let create_min_heap = || {
    { elements: [] }
  }
  
  let heap_parent = |i| (i - 1) / 2
  let heap_left_child = |i| 2 * i + 1
  let heap_right_child = |i| 2 * i + 2
  
  let heap_swap = |heap, i, j| {
    let temp = heap.elements[i]
    heap.elements[i] = heap.elements[j]
    heap.elements[j] = temp
  }
  
  let heapify_up = |heap, index| {
    if index == 0 {
      return heap
    }
    
    let parent = heap_parent(index)
    if heap.elements[index] < heap.elements[parent] {
      heap_swap(heap, index, parent)
      heapify_up(heap, parent)
    } else {
      heap
    }
  }
  
  let heapify_down = |heap, index, size| {
    let left = heap_left_child(index)
    let right = heap_right_child(index)
    let mut smallest = index
    
    if left < size && heap.elements[left] < heap.elements[smallest] {
      smallest = left
    }
    
    if right < size && heap.elements[right] < heap.elements[smallest] {
      smallest = right
    }
    
    if smallest != index {
      heap_swap(heap, index, smallest)
      heapify_down(heap, smallest, size)
    } else {
      heap
    }
  }
  
  let heap_insert = |heap, value| {
    let new_heap = { elements: heap.elements + [value] }
    heapify_up(new_heap, new_heap.elements.length() - 1)
  }
  
  let heap_extract_min = |heap| {
    if heap.elements.length() == 0 {
      (None, heap)
    } else {
      let min = heap.elements[0]
      let last = heap.elements[heap.elements.length() - 1]
      heap.elements[0] = last
      let new_elements = heap.elements.slice(0, heap.elements.length() - 1)
      let new_heap = { elements: new_elements }
      heapify_down(new_heap, 0, new_heap.elements.length())
      (Some(min), new_heap)
    }
  }
  
  let heap_peek = |heap| {
    if heap.elements.length() > 0 {
      Some(heap.elements[0])
    } else {
      None
    }
  }
  
  // Test min-heap operations
  let mut heap = create_min_heap()
  
  heap = heap_insert(heap, 5)
  heap = heap_insert(heap, 3)
  heap = heap_insert(heap, 8)
  heap = heap_insert(heap, 1)
  heap = heap_insert(heap, 10)
  
  assert_eq(heap_peek(heap), Some(1))
  
  let (min1, heap1) = heap_extract_min(heap)
  match min1 {
    Some(value) => assert_eq(value, 1)
    None => assert_true(false)
  }
  
  assert_eq(heap_peek(heap1), Some(3))
  
  let (min2, heap2) = heap_extract_min(heap1)
  match min2 {
    Some(value) => assert_eq(value, 3)
    None => assert_true(false)
  }
  
  assert_eq(heap_peek(heap2), Some(5))
  
  // Test heap sort using min-heap
  let heap_sort = |arr| {
    let mut heap = create_min_heap()
    
    // Build heap from array
    for value in arr {
      heap = heap_insert(heap, value)
    }
    
    // Extract elements to get sorted array
    let mut sorted = []
    while heap.elements.length() > 0 {
      let (min, new_heap) = heap_extract_min(heap)
      match min {
        Some(value) => sorted.push(value)
        None => break
      }
      heap = new_heap
    }
    
    sorted
  }
  
  let unsorted = [64, 34, 25, 12, 22, 11, 90, 88, 76, 50, 42]
  let sorted = heap_sort(unsorted)
  
  assert_eq(sorted, [11, 12, 22, 25, 34, 42, 50, 64, 76, 88, 90])
  
  // Test priority queue for task scheduling
  let schedule_tasks = |tasks| {
    let mut heap = create_min_heap()
    
    // Insert tasks with priority (lower number = higher priority)
    for (task, priority) in tasks {
      heap = heap_insert(heap, { task: task, priority: priority })
    }
    
    // Process tasks in priority order
    let mut processed = []
    while heap.elements.length() > 0 {
      let (min_task, new_heap) = heap_extract_min(heap)
      match min_task {
        Some(t) => {
          processed = processed + [t.task]
          heap = new_heap
        }
        None => break
      }
    }
    
    processed
  }
  
  let tasks_with_priority = [
    ("low_priority_task", 5),
    ("high_priority_task", 1),
    ("medium_priority_task", 3),
    ("urgent_task", 0),
    ("normal_task", 2)
  ]
  
  let scheduled_tasks = schedule_tasks(tasks_with_priority)
  assert_eq(scheduled_tasks, ["urgent_task", "high_priority_task", "normal_task", "medium_priority_task", "low_priority_task"])
  
  // Test heap for finding kth smallest element
  let kth_smallest = |arr, k| {
    let mut heap = create_min_heap()
    
    // Build heap from array
    for value in arr {
      heap = heap_insert(heap, value)
    }
    
    // Extract k elements
    let mut result = None
    for i = 0; i < k; i = i + 1 {
      let (min, new_heap) = heap_extract_min(heap)
      match min {
        Some(value) => {
          result = Some(value)
          heap = new_heap
        }
        None => break
      }
    }
    
    result
  }
  
  let numbers = [7, 10, 4, 3, 20, 15]
  assert_eq(kth_smallest(numbers, 3), Some(7))  // 3rd smallest is 7 (3, 4, 7, 10, 15, 20)
  assert_eq(kth_smallest(numbers, 1), Some(3))  // 1st smallest is 3
  assert_eq(kth_smallest(numbers, 6), Some(20)) // 6th smallest is 20
}

// Test 9: Trie (Prefix Tree) Operations
test "trie prefix tree operations" {
  // Define trie node structure
  let create_trie_node = |is_end_of_word| {
    { children: {}, is_end_of_word: is_end_of_word }
  }
  
  // Create empty trie
  let create_trie = || {
    create_trie_node(false)
  }
  
  // Insert word into trie
  let trie_insert = |trie, word| {
    let mut current = trie
    
    for i = 0; i < word.length(); i = i + 1 {
      let char = word[i]
      
      if !current.children.contains(char) {
        current.children[char] = create_trie_node(false)
      }
      
      current = current.children[char]
    }
    
    current.is_end_of_word = true
    trie
  }
  
  // Search for word in trie
  let trie_search = |trie, word| {
    let mut current = trie
    
    for i = 0; i < word.length(); i = i + 1 {
      let char = word[i]
      
      if !current.children.contains(char) {
        return false
      }
      
      current = current.children[char]
    }
    
    current.is_end_of_word
  }
  
  // Check if any word starts with prefix
  let trie_starts_with = |trie, prefix| {
    let mut current = trie
    
    for i = 0; i < prefix.length(); i = i + 1 {
      let char = prefix[i]
      
      if !current.children.contains(char) {
        return false
      }
      
      current = current.children[char]
    }
    
    true
  }
  
  // Get all words with given prefix
  let trie_get_words_with_prefix = |trie, prefix| {
    let mut current = trie
    
    // Navigate to prefix node
    for i = 0; i < prefix.length(); i = i + 1 {
      let char = prefix[i]
      
      if !current.children.contains(char) {
        return []
      }
      
      current = current.children[char]
    }
    
    // Collect all words from this node
    let mut words = []
    let mut stack = [(current, prefix)]
    
    while stack.length() > 0 {
      let (node, word) = stack.pop()
      
      if node.is_end_of_word {
        words.push(word)
      }
      
      for (char, child_node) in node.children {
        stack.push((child_node, word + char))
      }
    }
    
    words
  }
  
  // Test trie operations
  let mut trie = create_trie()
  
  // Insert words
  trie = trie_insert(trie, "apple")
  trie = trie_insert(trie, "app")
  trie = trie_insert(trie, "application")
  trie = trie_insert(trie, "banana")
  trie = trie_insert(trie, "band")
  trie = trie_insert(trie, "bandana")
  
  // Test search
  assert_true(trie_search(trie, "apple"))
  assert_true(trie_search(trie, "app"))
  assert_true(trie_search(trie, "application"))
  assert_true(trie_search(trie, "banana"))
  assert_true(trie_search(trie, "band"))
  assert_true(trie_search(trie, "bandana"))
  
  assert_false(trie_search(trie, "appl"))  // Not a complete word
  assert_false(trie_search(trie, "apples"))  // Not inserted
  assert_false(trie_search(trie, "orange"))  // Not inserted
  
  // Test starts with
  assert_true(trie_starts_with(trie, "app"))
  assert_true(trie_starts_with(trie, "appl"))
  assert_true(trie_starts_with(trie, "ban"))
  assert_true(trie_starts_with(trie, "band"))
  
  assert_false(trie_starts_with(trie, "ora"))
  assert_false(trie_starts_with(trie, "xyz"))
  
  // Test get words with prefix
  let app_words = trie_get_words_with_prefix(trie, "app")
  assert_eq(app_words.sort(), ["app", "apple", "application"])
  
  let ban_words = trie_get_words_with_prefix(trie, "ban")
  assert_eq(ban_words.sort(), ["banana", "band", "bandana"])
  
  let band_words = trie_get_words_with_prefix(trie, "band")
  assert_eq(band_words.sort(), ["band", "bandana"])
  
  // Test trie for autocomplete functionality
  let autocomplete = |trie, prefix, limit| {
    let words = trie_get_words_with_prefix(trie, prefix)
    if words.length() <= limit {
      words
    } else {
      words.slice(0, limit)
    }
  }
  
  let app_suggestions = autocomplete(trie, "app", 2)
  assert_eq(app_suggestions.length(), 2)
  assert_true(app_suggestions.contains("app"))
  assert_true(app_suggestions.contains("apple") || app_suggestions.contains("application"))
  
  // Test trie for spell checking
  let spell_check = |trie, word| {
    if trie_search(trie, word) {
      "correct"
    } else if trie_starts_with(trie, word) {
      "prefix"
    } else {
      "incorrect"
    }
  }
  
  assert_eq(spell_check(trie, "apple"), "correct")
  assert_eq(spell_check(trie, "app"), "correct")
  assert_eq(spell_check(trie, "appl"), "prefix")
  assert_eq(spell_check(trie, "orange"), "incorrect")
}

// Test 10: Advanced Set Operations and Algorithms
test "advanced set operations and algorithms" {
  // Set implementation using hash table
  let create_set = || {
    { elements: {} }
  }
  
  let set_add = |set, element| {
    { elements: set.elements + { element: true } }
  }
  
  let set_remove = |set, element| {
    let mut new_elements = set.elements
    if new_elements.contains(element) {
      new_elements = new_elements.remove(element)
    }
    { elements: new_elements }
  }
  
  let set_contains = |set, element| {
    set.elements.contains(element)
  }
  
  let set_to_array = |set| {
    let mut result = []
    for element in set.elements {
      result.push(element)
    }
    result
  }
  
  // Set operations
  let set_union = |set1, set2| {
    let mut result = set1
    for element in set2.elements {
      if !set_contains(result, element) {
        result = set_add(result, element)
      }
    }
    result
  }
  
  let set_intersection = |set1, set2| {
    let mut result = create_set()
    for element in set1.elements {
      if set_contains(set2, element) {
        result = set_add(result, element)
      }
    }
    result
  }
  
  let set_difference = |set1, set2| {
    let mut result = create_set()
    for element in set1.elements {
      if !set_contains(set2, element) {
        result = set_add(result, element)
      }
    }
    result
  }
  
  let set_is_subset = |set1, set2| {
    for element in set1.elements {
      if !set_contains(set2, element) {
        return false
      }
    }
    true
  }
  
  // Test set operations
  let set1 = create_set()
  let set1 = set_add(set1, 1)
  let set1 = set_add(set1, 2)
  let set1 = set_add(set1, 3)
  let set1 = set_add(set1, 4)
  
  let set2 = create_set()
  let set2 = set_add(set2, 3)
  let set2 = set_add(set2, 4)
  let set2 = set_add(set2, 5)
  let set2 = set_add(set2, 6)
  
  assert_eq(set_to_array(set1).sort(), [1, 2, 3, 4])
  assert_eq(set_to_array(set2).sort(), [3, 4, 5, 6])
  
  // Test union
  let union_set = set_union(set1, set2)
  assert_eq(set_to_array(union_set).sort(), [1, 2, 3, 4, 5, 6])
  
  // Test intersection
  let intersection_set = set_intersection(set1, set2)
  assert_eq(set_to_array(intersection_set).sort(), [3, 4])
  
  // Test difference
  let difference_set = set_difference(set1, set2)
  assert_eq(set_to_array(difference_set).sort(), [1, 2])
  
  // Test subset
  assert_true(set_is_subset(intersection_set, set1))
  assert_true(set_is_subset(intersection_set, set2))
  assert_false(set_is_subset(set1, set2))
  assert_false(set_is_subset(set2, set1))
  
  // Test set for finding unique elements
  let find_unique = |arr| {
    let mut unique_set = create_set()
    let mut duplicate_set = create_set()
    
    for element in arr {
      if set_contains(unique_set, element) {
        duplicate_set = set_add(duplicate_set, element)
      } else {
        unique_set = set_add(unique_set, element)
      }
    }
    
    // Remove duplicates from unique set
    let result = set_difference(unique_set, duplicate_set)
    set_to_array(result).sort()
  }
  
  let array_with_duplicates = [1, 2, 3, 2, 4, 5, 1, 6, 3, 7]
  let unique_elements = find_unique(array_with_duplicates)
  assert_eq(unique_elements, [4, 5, 6, 7])
  
  // Test set for power set generation
  let power_set = |arr| {
    let n = arr.length()
    let power_set_size = 1 << n  // 2^n
    let mut result = []
    
    for i = 0; i < power_set_size; i = i + 1 {
      let mut subset = []
      for j = 0; j < n; j = j + 1 {
        if (i & (1 << j)) != 0 {
          subset.push(arr[j])
        }
      }
      result.push(subset)
    }
    
    result
  }
  
  let input_set = [1, 2, 3]
  let generated_power_set = power_set(input_set)
  
  assert_eq(generated_power_set.length(), 8)  // 2^3 = 8 subsets
  assert_true(generated_power_set.contains([]))
  assert_true(generated_power_set.contains([1]))
  assert_true(generated_power_set.contains([2]))
  assert_true(generated_power_set.contains([3]))
  assert_true(generated_power_set.contains([1, 2]))
  assert_true(generated_power_set.contains([1, 3]))
  assert_true(generated_power_set.contains([2, 3]))
  assert_true(generated_power_set.contains([1, 2, 3]))
  
  // Test set for finding all permutations
  let permutations = |arr| {
    let n = arr.length()
    if n <= 1 {
      [arr]
    } else {
      let mut result = []
      
      for i = 0; i < n; i = i + 1 {
        let element = arr[i]
        let mut remaining = []
        for j = 0; j < n; j = j + 1 {
          if j != i {
            remaining.push(arr[j])
          }
        }
        
        let perms = permutations(remaining)
        for perm in perms {
          result.push([element] + perm)
        }
      }
      
      result
    }
  }
  
  let input_array = [1, 2, 3]
  let generated_permutations = permutations(input_array)
  
  assert_eq(generated_permutations.length(), 6)  // 3! = 6 permutations
  assert_true(generated_permutations.contains([1, 2, 3]))
  assert_true(generated_permutations.contains([1, 3, 2]))
  assert_true(generated_permutations.contains([2, 1, 3]))
  assert_true(generated_permutations.contains([2, 3, 1]))
  assert_true(generated_permutations.contains([3, 1, 2]))
  assert_true(generated_permutations.contains([3, 2, 1]))
}