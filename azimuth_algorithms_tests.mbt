// Algorithms Test Cases for Azimuth Telemetry System
// This file contains comprehensive tests for various algorithms

test "sorting algorithms - bubble sort" {
  fn bubble_sort(arr : Array[Int]) -> Array[Int] {
    let mut result = arr.copy()
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
  
  let unsorted = [5, 2, 8, 1, 9, 3]
  let sorted = bubble_sort(unsorted)
  let expected = [1, 2, 3, 5, 8, 9]
  
  assert_eq(sorted, expected)
  
  // Test with already sorted array
  let already_sorted = [1, 2, 3, 4, 5]
  let still_sorted = bubble_sort(already_sorted)
  assert_eq(still_sorted, already_sorted)
  
  // Test with empty array
  let empty = []
  let still_empty = bubble_sort(empty)
  assert_eq(still_empty, empty)
}

test "sorting algorithms - quick sort" {
  fn quick_sort(arr : Array[Int]) -> Array[Int] {
    if arr.length() <= 1 {
      return arr
    }
    
    let pivot = arr[0]
    let mut less = []
    let mut equal = []
    let mut greater = []
    
    for i in 0..arr.length() {
      if arr[i] < pivot {
        less.push(arr[i])
      } else if arr[i] > pivot {
        greater.push(arr[i])
      } else {
        equal.push(arr[i])
      }
    }
    
    let sorted_less = quick_sort(less)
    let sorted_greater = quick_sort(greater)
    
    // Concatenate arrays
    let mut result = sorted_less
    for i in 0..equal.length() {
      result.push(equal[i])
    }
    for i in 0..sorted_greater.length() {
      result.push(sorted_greater[i])
    }
    
    result
  }
  
  let unsorted = [10, 7, 8, 9, 1, 5]
  let sorted = quick_sort(unsorted)
  let expected = [1, 5, 7, 8, 9, 10]
  
  assert_eq(sorted, expected)
  
  // Test with duplicate values
  let with_duplicates = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  let sorted_duplicates = quick_sort(with_duplicates)
  let expected_duplicates = [1, 1, 2, 3, 4, 5, 5, 6, 9]
  
  assert_eq(sorted_duplicates, expected_duplicates)
}

test "search algorithms - binary search" {
  fn binary_search(arr : Array[Int], target : Int) -> Int {
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
    
    return -1 // Not found
  }
  
  let sorted = [1, 3, 5, 7, 9, 11, 13, 15]
  
  // Test existing elements
  assert_eq(binary_search(sorted, 1), 0)
  assert_eq(binary_search(sorted, 7), 3)
  assert_eq(binary_search(sorted, 15), 7)
  
  // Test non-existing elements
  assert_eq(binary_search(sorted, 0), -1)
  assert_eq(binary_search(sorted, 8), -1)
  assert_eq(binary_search(sorted, 16), -1)
}

test "search algorithms - linear search" {
  fn linear_search(arr : Array[String], target : String) -> Int {
    for i in 0..arr.length() {
      if arr[i] == target {
        return i
      }
    }
    return -1 // Not found
  }
  
  let strings = ["apple", "banana", "cherry", "date", "elderberry"]
  
  // Test existing elements
  assert_eq(linear_search(strings, "apple"), 0)
  assert_eq(linear_search(strings, "cherry"), 2)
  assert_eq(linear_search(strings, "elderberry"), 4)
  
  // Test non-existing elements
  assert_eq(linear_search(strings, "fig"), -1)
  assert_eq(linear_search(strings, "grape"), -1)
}

test "graph algorithms - depth-first search" {
  // Simple graph representation using adjacency list
  let graph = [
    [1, 2],     // Node 0 connects to nodes 1 and 2
    [0, 3, 4],  // Node 1 connects to nodes 0, 3, and 4
    [0, 5],     // Node 2 connects to nodes 0 and 5
    [1],        // Node 3 connects to node 1
    [1],        // Node 4 connects to node 1
    [2]         // Node 5 connects to node 2
  ]
  
  fn dfs(graph : Array[Array[Int]], node : Int, visited : Array[Bool]) -> Array[Int] {
    let mut result = []
    let mut visited_copy = visited.copy()
    visited_copy[node] = true
    result.push(node)
    
    for i in 0..graph[node].length() {
      let neighbor = graph[node][i]
      if not visited_copy[neighbor] {
        let dfs_result = dfs(graph, neighbor, visited_copy)
        for j in 0..dfs_result.length() {
          result.push(dfs_result[j])
        }
      }
    }
    
    result
  }
  
  let visited = [false, false, false, false, false, false]
  let traversal = dfs(graph, 0, visited)
  
  // Check that all nodes were visited (order may vary)
  assert_eq(traversal.length(), 6)
  
  // Check that each node appears exactly once
  let mut visited_check = [false, false, false, false, false, false]
  for i in 0..traversal.length() {
    visited_check[traversal[i]] = true
  }
  
  for i in 0..6 {
    assert_true(visited_check[i])
  }
}

test "graph algorithms - breadth-first search" {
  // Simple graph representation using adjacency list
  let graph = [
    [1, 2],     // Node 0 connects to nodes 1 and 2
    [0, 3, 4],  // Node 1 connects to nodes 0, 3, and 4
    [0, 5],     // Node 2 connects to nodes 0 and 5
    [1],        // Node 3 connects to node 1
    [1],        // Node 4 connects to node 1
    [2]         // Node 5 connects to node 2
  ]
  
  fn bfs(graph : Array[Array[Int]], start : Int) -> Array[Int] {
    let mut queue = []
    let mut visited = [false, false, false, false, false, false]
    let mut result = []
    
    queue.push_back(start)
    visited[start] = true
    
    while queue.length() > 0 {
      let node = queue.pop_front().unwrap()
      result.push(node)
      
      for i in 0..graph[node].length() {
        let neighbor = graph[node][i]
        if not visited[neighbor] {
          visited[neighbor] = true
          queue.push_back(neighbor)
        }
      }
    }
    
    result
  }
  
  let traversal = bfs(graph, 0)
  
  // BFS traversal from node 0 should visit nodes in order: 0, 1, 2, 3, 4, 5
  assert_eq(traversal, [0, 1, 2, 3, 4, 5])
}

test "dynamic programming - fibonacci" {
  fn fibonacci(n : Int) -> Int {
    if n <= 1 {
      return n
    }
    
    let mut dp = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    dp[0] = 0
    dp[1] = 1
    
    for i in 2..=n {
      dp[i] = dp[i - 1] + dp[i - 2]
    }
    
    dp[n]
  }
  
  // Test fibonacci sequence
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(2), 1)
  assert_eq(fibonacci(3), 2)
  assert_eq(fibonacci(4), 3)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(6), 8)
  assert_eq(fibonacci(7), 13)
  assert_eq(fibonacci(8), 21)
  assert_eq(fibonacci(10), 55)
}

test "dynamic programming - longest common subsequence" {
  fn lcs(s1 : String, s2 : String) -> Int {
    let m = s1.length()
    let n = s2.length()
    
    // Create DP table
    let mut dp = []
    for i in 0..(m + 1) {
      let mut row = []
      for j in 0..(n + 1) {
        row.push(0)
      }
      dp.push(row)
    }
    
    // Fill DP table
    for i in 1..=m {
      for j in 1..=n {
        if s1[i - 1] == s2[j - 1] {
          dp[i][j] = dp[i - 1][j - 1] + 1
        } else {
          dp[i][j] = max(dp[i - 1][j], dp[i][j - 1])
        }
      }
    }
    
    dp[m][n]
  }
  
  // Test LCS
  assert_eq(lcs("AGGTAB", "GXTXAYB"), 4) // "GTAB"
  assert_eq(lcs("ABCBDAB", "BDCABA"), 4) // "BCBA" or "BDAB"
  assert_eq(lcs("", "ABC"), 0)
  assert_eq(lcs("ABC", ""), 0)
  assert_eq(lcs("ABC", "ABC"), 3)
}

test "string algorithms - palindrome check" {
  fn is_palindrome(s : String) -> Bool {
    let n = s.length()
    for i in 0..(n / 2) {
      if s[i] != s[n - i - 1] {
        return false
      }
    }
    return true
  }
  
  // Test palindrome check
  assert_true(is_palindrome("racecar"))
  assert_true(is_palindrome("madam"))
  assert_true(is_palindrome("level"))
  assert_true(is_palindrome("a"))
  assert_true(is_palindrome(""))
  
  assert_false(is_palindrome("hello"))
  assert_false(is_palindrome("world"))
  assert_false(is_palindrome("azimuth"))
}

test "string algorithms - anagram check" {
  fn is_anagram(s1 : String, s2 : String) -> Bool {
    if s1.length() != s2.length() {
      return false
    }
    
    let mut count = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    
    // Count characters in s1
    for i in 0..s1.length() {
      let char_code = s1[i].to_int() - 'a'.to_int()
      if 0 <= char_code && char_code < 26 {
        count[char_code] = count[char_code] + 1
      }
    }
    
    // Subtract characters in s2
    for i in 0..s2.length() {
      let char_code = s2[i].to_int() - 'a'.to_int()
      if 0 <= char_code && char_code < 26 {
        count[char_code] = count[char_code] - 1
      }
    }
    
    // Check if all counts are zero
    for i in 0..26 {
      if count[i] != 0 {
        return false
      }
    }
    
    return true
  }
  
  // Test anagram check
  assert_true(is_anagram("listen", "silent"))
  assert_true(is_anagram("evil", "vile"))
  assert_true(is_anagram("dusty", "study"))
  
  assert_false(is_anagram("hello", "world"))
  assert_false(is_anagram("test", "tess"))
  assert_false(is_anagram("abc", "ab"))
}