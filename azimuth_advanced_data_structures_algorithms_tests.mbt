// Azimuth Advanced Data Structures and Algorithms Test Suite
// This file contains tests for advanced data structures and algorithms

// Test 1: Priority Queue Implementation
test "priority queue implementation" {
  // Priority queue element
  type PQElement = {
    value: String,
    priority: Int
  }
  
  // Priority queue
  type PriorityQueue = {
    elements: Array[PQElement]
  }
  
  // Create empty priority queue
  let create_pq = fn() {
    { elements: [] }
  }
  
  // Check if priority queue is empty
  let is_empty = fn(pq: PriorityQueue) {
    pq.elements.length() == 0
  }
  
  // Get size of priority queue
  let size = fn(pq: PriorityQueue) {
    pq.elements.length()
  }
  
  // Insert element into priority queue (min-heap)
  let insert = fn(pq: PriorityQueue, value: String, priority: Int) {
    let new_element = { value, priority }
    let mut new_elements = pq.elements.push(new_element)
    
    // Bubble up to maintain heap property
    let mut index = new_elements.length() - 1
    while index > 0 {
      let parent_index = (index - 1) / 2
      if new_elements[index].priority < new_elements[parent_index].priority {
        let temp = new_elements[index]
        new_elements[index] = new_elements[parent_index]
        new_elements[parent_index] = temp
        index = parent_index
      } else {
        break
      }
    }
    
    { elements: new_elements }
  }
  
  // Extract minimum element
  let extract_min = fn(pq: PriorityQueue) {
    if is_empty(pq) {
      return (pq, None)
    }
    
    let min_element = pq.elements[0]
    let last_element = pq.elements[pq.elements.length() - 1]
    
    // Move last element to root
    let mut new_elements = pq.elements.slice(0, pq.elements.length() - 1)
    
    if new_elements.length() > 0 {
      new_elements[0] = last_element
      
      // Bubble down to maintain heap property
      let mut index = 0
      let heap_size = new_elements.length()
      
      while true {
        let left_child = 2 * index + 1
        let right_child = 2 * index + 2
        let mut smallest = index
        
        if left_child < heap_size and new_elements[left_child].priority < new_elements[smallest].priority {
          smallest = left_child
        }
        
        if right_child < heap_size and new_elements[right_child].priority < new_elements[smallest].priority {
          smallest = right_child
        }
        
        if smallest != index {
          let temp = new_elements[index]
          new_elements[index] = new_elements[smallest]
          new_elements[smallest] = temp
          index = smallest
        } else {
          break
        }
      }
    }
    
    ({ elements: new_elements }, Some(min_element))
  }
  
  // Peek at minimum element
  let peek = fn(pq: PriorityQueue) {
    if is_empty(pq) {
      None
    } else {
      Some(pq.elements[0])
    }
  }
  
  // Test priority queue operations
  let pq = create_pq()
  assert_true(is_empty(pq))
  assert_eq(size(pq), 0)
  assert_eq(peek(pq), None)
  
  // Insert elements
  let pq1 = insert(pq, "task1", 5)
  assert_false(is_empty(pq1))
  assert_eq(size(pq1), 1)
  assert_eq(peek(pq1).unwrap().value, "task1")
  
  let pq2 = insert(pq1, "task2", 3)
  assert_eq(size(pq2), 2)
  assert_eq(peek(pq2).unwrap().value, "task2")  // Higher priority (lower number)
  
  let pq3 = insert(pq2, "task3", 7)
  let pq4 = insert(pq3, "task4", 1)
  let pq5 = insert(pq4, "task5", 4)
  
  assert_eq(size(pq5), 5)
  assert_eq(peek(pq5).unwrap().value, "task4")  // Highest priority
  
  // Extract elements in priority order
  let (pq6, min1) = extract_min(pq5)
  assert_eq(min1.unwrap().value, "task4")
  assert_eq(min1.unwrap().priority, 1)
  assert_eq(size(pq6), 4)
  
  let (pq7, min2) = extract_min(pq6)
  assert_eq(min2.unwrap().value, "task2")
  assert_eq(min2.unwrap().priority, 3)
  
  let (pq8, min3) = extract_min(pq7)
  assert_eq(min3.unwrap().value, "task5")
  assert_eq(min3.unwrap().priority, 4)
  
  let (pq9, min4) = extract_min(pq8)
  assert_eq(min4.unwrap().value, "task1")
  assert_eq(min4.unwrap().priority, 5)
  
  let (pq10, min5) = extract_min(pq9)
  assert_eq(min5.unwrap().value, "task3")
  assert_eq(min5.unwrap().priority, 7)
  
  assert_true(is_empty(pq10))
  
  // Test extraction from empty queue
  let (pq11, empty_min) = extract_min(pq10)
  assert_true(is_empty(pq11))
  assert_eq(empty_min, None)
}

// Test 2: Trie (Prefix Tree) Implementation
test "trie prefix tree implementation" {
  // Trie node
  type TrieNode = {
    children: Array[(Char, TrieNode)],
    is_end_of_word: Bool,
    value: Option[String>
  }
  
  // Create new trie node
  let create_trie_node = fn() {
    {
      children: [],
      is_end_of_word: false,
      value: None
    }
  }
  
  // Insert word into trie
  let insert = fn(node: TrieNode, word: String, value: String) {
    let mut current_node = node
    let chars = word.to_char_array()
    
    for i in 0..chars.length() {
      let char = chars[i]
      let mut child_found = false
      
      // Check if character already exists in children
      let mut updated_children = []
      let mut child_index = -1
      
      for i in 0..current_node.children.length() {
        let (c, child) = current_node.children[i]
        updated_children = updated_children.push((c, child))
        
        if c == char {
          child_found = true
          child_index = i
        }
      }
      
      if child_found {
        // Navigate to existing child
        current_node = updated_children[child_index].1
      } else {
        // Create new child
        let new_child = create_trie_node()
        updated_children = updated_children.push((char, new_child))
        current_node = new_child
      }
      
      // Update current node with new children
      current_node = { current_node | children: updated_children }
    }
    
    // Mark as end of word and set value
    { current_node | is_end_of_word: true, value: Some(value) }
  }
  
  // Search for word in trie
  let search = fn(node: TrieNode, word: String) {
    let mut current_node = node
    let chars = word.to_char_array()
    
    for i in 0..chars.length() {
      let char = chars[i]
      let mut child_found = false
      
      for (c, child) in current_node.children {
        if c == char {
          current_node = child
          child_found = true
          break
        }
      }
      
      if not(child_found) {
        return None
      }
    }
    
    if current_node.is_end_of_word {
      current_node.value
    } else {
      None
    }
  }
  
  // Check if any word starts with prefix
  let starts_with = fn(node: TrieNode, prefix: String) {
    let mut current_node = node
    let chars = prefix.to_char_array()
    
    for i in 0..chars.length() {
      let char = chars[i]
      let mut child_found = false
      
      for (c, child) in current_node.children {
        if c == char {
          current_node = child
          child_found = true
          break
        }
      }
      
      if not(child_found) {
        return false
      }
    }
    
    true
  }
  
  // Create trie
  let trie = create_trie_node()
  
  // Insert words
  let trie1 = insert(trie, "cat", "feline animal")
  let trie2 = insert(trie1, "car", "vehicle")
  let trie3 = insert(trie2, "dog", "canine animal")
  let trie4 = insert(trie3, "cart", "wheeled vehicle")
  
  // Test search
  assert_eq(search(trie4, "cat"), Some("feline animal"))
  assert_eq(search(trie4, "car"), Some("vehicle"))
  assert_eq(search(trie4, "dog"), Some("canine animal"))
  assert_eq(search(trie4, "cart"), Some("wheeled vehicle"))
  
  // Test non-existing words
  assert_eq(search(trie4, "bat"), None)
  assert_eq(search(trie4, "care"), None)
  
  // Test prefix search
  assert_true(starts_with(trie4, "ca"))
  assert_true(starts_with(trie4, "car"))
  assert_true(starts_with(trie4, "cat"))
  assert_false(starts_with(trie4, "ba"))
  assert_false(starts_with(trie4, "do"))
  
  // Test empty trie
  let empty_trie = create_trie_node()
  assert_eq(search(empty_trie, "word"), None)
  assert_false(starts_with(empty_trie, "prefix"))
  
  // Test single character words
  let trie5 = insert(trie4, "a", "first letter")
  assert_eq(search(trie5, "a"), Some("first letter"))
  assert_true(starts_with(trie5, "a"))
}

// Test 3: Graph Algorithms - Shortest Path
test "graph shortest path algorithms" {
  // Graph edge
  type GraphEdge = {
    to: Int,
    weight: Int
  }
  
  // Graph node
  type GraphNode = {
    id: Int,
    edges: Array[GraphEdge>
  }
  
  // Path result
  type PathResult = {
    distance: Int,
    path: Array[Int]
  }
  
  // Create graph node
  let create_node = fn(id: Int) {
    {
      id,
      edges: []
    }
  }
  
  // Add edge to node
  let add_edge = fn(node: GraphNode, to: Int, weight: Int) {
    {
      node |
      edges: node.edges.push({ to, weight })
    }
  }
  
  // Dijkstra's algorithm for shortest path
  let dijkstra = fn(graph: Array[GraphNode], start: Int, end: Int) {
    let mut distances = {}
    let mut previous = {}
    let mut unvisited = []
    
    // Initialize distances
    for node in graph {
      let distance = if node.id == start { 0 } else { 1000000 }  // Large number as infinity
      distances = distances.insert(node.id, distance)
      previous = previous.insert(node.id, None)
      unvisited = unvisited.push(node.id)
    }
    
    while unvisited.length() > 0 {
      // Find unvisited node with minimum distance
      let mut min_distance = 1000000
      let mut current = -1
      let mut current_index = -1
      
      for i in 0..unvisited.length() {
        let node_id = unvisited[i]
        if distances[node_id] < min_distance {
          min_distance = distances[node_id]
          current = node_id
          current_index = i
        }
      }
      
      if current == -1 or current == end {
        break
      }
      
      // Remove current from unvisited
      let mut new_unvisited = []
      for i in 0..unvisited.length() {
        if i != current_index {
          new_unvisited = new_unvisited.push(unvisited[i])
        }
      }
      unvisited = new_unvisited
      
      // Update distances to neighbors
      let mut current_node = None
      for node in graph {
        if node.id == current {
          current_node = Some(node)
          break
        }
      }
      
      match current_node {
        Some(node) => {
          for edge in node.edges {
            let alt = distances[current] + edge.weight
            if alt < distances[edge.to] {
              distances = distances.insert(edge.to, alt)
              previous = previous.insert(edge.to, Some(current))
            }
          }
        }
        None => {}
      }
    }
    
    // Reconstruct path
    let mut path = []
    let mut current = end
    
    while true {
      match previous[current] {
        Some(prev) => {
          path = [prev] + path
          if prev == start {
            path = path.push(end)
            return {
              distance: distances[end],
              path
            }
          }
          current = prev
        }
        None => {
          if current == start {
            return {
              distance: 0,
              path: [start]
            }
          } else {
            return {
              distance: -1,  // No path
              path: []
            }
          }
        }
      }
    }
  }
  
  // Create test graph
  let node1 = create_node(1)
  let node2 = create_node(2)
  let node3 = create_node(3)
  let node4 = create_node(4)
  let node5 = create_node(5)
  let node6 = create_node(6)
  
  // Add edges
  let graph = [
    add_edge(add_edge(node1, 2, 7), 3, 9),
    add_edge(add_edge(add_edge(node2, 1, 7), 3, 10), 4, 15),
    add_edge(add_edge(add_edge(node3, 1, 9), 2, 10), 4, 11),
    add_edge(add_edge(add_edge(node4, 2, 15), 3, 11), 5, 6),
    add_edge(add_edge(node5, 4, 6), 6, 9),
    create_node(6)  // No outgoing edges
  ]
  
  // Test shortest paths
  let path_1_to_5 = dijkstra(graph, 1, 5)
  assert_eq(path_1_to_5.distance, 26)  // 1->3->4->5
  assert_eq(path_1_to_5.path, [1, 3, 4, 5])
  
  let path_1_to_4 = dijkstra(graph, 1, 4)
  assert_eq(path_1_to_4.distance, 20)  // 1->3->4
  assert_eq(path_1_to_4.path, [1, 3, 4])
  
  let path_2_to_5 = dijkstra(graph, 2, 5)
  assert_eq(path_2_to_5.distance, 21)  // 2->4->5
  assert_eq(path_2_to_5.path, [2, 4, 5])
  
  let path_5_to_6 = dijkstra(graph, 5, 6)
  assert_eq(path_5_to_6.distance, 9)
  assert_eq(path_5_to_6.path, [5, 6])
  
  // Test path to self
  let path_3_to_3 = dijkstra(graph, 3, 3)
  assert_eq(path_3_to_3.distance, 0)
  assert_eq(path_3_to_3.path, [3])
  
  // Test no path (disconnected node)
  let path_6_to_1 = dijkstra(graph, 6, 1)
  assert_eq(path_6_to_1.distance, -1)
  assert_eq(path_6_to_1.path, [])
}

// Test 4: Dynamic Programming - Fibonacci Sequence
test "dynamic programming fibonacci sequence" {
  // Calculate Fibonacci using memoization
  let fibonacci_memo = fn(n: Int, memo: Array[Option[Int]]) {
    if n <= 1 {
      return (n, memo)
    }
    
    match memo[n] {
      Some(value) => (value, memo),
      None => {
        let (fib1, memo1) = fibonacci_memo(n - 1, memo)
        let (fib2, memo2) = fibonacci_memo(n - 2, memo1)
        let result = fib1 + fib2
        
        // Update memo
        let mut updated_memo = memo2
        updated_memo[n] = Some(result)
        
        (result, updated_memo)
      }
    }
  }
  
  // Calculate Fibonacci using tabulation (bottom-up)
  let fibonacci_tabular = fn(n: Int) {
    if n <= 1 {
      return n
    }
    
    let mut fib = []
    for i in 0..(n + 1) {
      fib = fib.push(0)
    }
    
    fib[0] = 0
    fib[1] = 1
    
    for i in 2..(n + 1) {
      fib[i] = fib[i - 1] + fib[i - 2]
    }
    
    fib[n]
  }
  
  // Initialize memoization table
  let init_memo = fn(size: Int) {
    let mut memo = []
    for i in 0..size {
      memo = memo.push(None)
    }
    memo
  }
  
  // Test Fibonacci sequences
  assert_eq(fibonacci_tabular(0), 0)
  assert_eq(fibonacci_tabular(1), 1)
  assert_eq(fibonacci_tabular(2), 1)
  assert_eq(fibonacci_tabular(3), 2)
  assert_eq(fibonacci_tabular(4), 3)
  assert_eq(fibonacci_tabular(5), 5)
  assert_eq(fibonacci_tabular(6), 8)
  assert_eq(fibonacci_tabular(7), 13)
  assert_eq(fibonacci_tabular(8), 21)
  assert_eq(fibonacci_tabular(9), 34)
  assert_eq(fibonacci_tabular(10), 55)
  
  // Test memoization
  let memo = init_memo(20)
  let (fib10, _) = fibonacci_memo(10, memo)
  assert_eq(fib10, 55)
  
  let (fib15, _) = fibonacci_memo(15, init_memo(20))
  assert_eq(fib15, 610)
  
  // Compare results
  for i in 0..15 {
    let tab_result = fibonacci_tabular(i)
    let (memo_result, _) = fibonacci_memo(i, init_memo(20))
    assert_eq(tab_result, memo_result)
  }
  
  // Test edge cases
  assert_eq(fibonacci_tabular(0), 0)
  assert_eq(fibonacci_tabular(1), 1)
  
  // Test larger values
  assert_eq(fibonacci_tabular(20), 6765)
  assert_eq(fibonacci_tabular(25), 75025)
}

// Test 5: String Searching Algorithms
test "string searching algorithms" {
  // Naive string search
  let naive_search = fn(text: String, pattern: String) {
    let text_len = text.length()
    let pattern_len = pattern.length()
    
    if pattern_len == 0 {
      return Some(0)
    }
    
    if pattern_len > text_len {
      return None
    }
    
    for i in 0..(text_len - pattern_len + 1) {
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
  
  // Knuth-Morris-Pratt (KMP) algorithm
  let kmp_compute_lps = fn(pattern: String) {
    let pattern_len = pattern.length()
    let mut lps = []
    
    for i in 0..pattern_len {
      lps = lps.push(0)
    }
    
    let mut len = 0  // Length of previous longest prefix suffix
    let mut i = 1
    
    while i < pattern_len {
      if pattern[i] == pattern[len] {
        len = len + 1
        lps[i] = len
        i = i + 1
      } else {
        if len != 0 {
          len = lps[len - 1]
        } else {
          lps[i] = 0
          i = i + 1
        }
      }
    }
    
    lps
  }
  
  let kmp_search = fn(text: String, pattern: String) {
    let text_len = text.length()
    let pattern_len = pattern.length()
    
    if pattern_len == 0 {
      return Some(0)
    }
    
    if pattern_len > text_len {
      return None
    }
    
    let lps = kmp_compute_lps(pattern)
    let mut i = 0  // Index for text
    let mut j = 0  // Index for pattern
    
    while i < text_len {
      if text[i] == pattern[j] {
        i = i + 1
        j = j + 1
        
        if j == pattern_len {
          return Some(i - j)
        }
      } else {
        if j != 0 {
          j = lps[j - 1]
        } else {
          i = i + 1
        }
      }
    }
    
    None
  }
  
  // Rabin-Karp algorithm
  let rabin_karp_search = fn(text: String, pattern: String) {
    let text_len = text.length()
    let pattern_len = pattern.length()
    let prime = 101  // A prime number for modulus
    
    if pattern_len == 0 {
      return Some(0)
    }
    
    if pattern_len > text_len {
      return None
    }
    
    let mut pattern_hash = 0
    let mut text_hash = 0
    let mut h = 1
    
    // Calculate h = pow(d, pattern_len-1) % prime
    for i in 0..(pattern_len - 1) {
      h = (h * 256) % prime
    }
    
    // Calculate initial hash values
    for i in 0..pattern_len {
      pattern_hash = (256 * pattern_hash + pattern[i].to_int()) % prime
      text_hash = (256 * text_hash + text[i].to_int()) % prime
    }
    
    // Slide the pattern over text
    for i in 0..(text_len - pattern_len + 1) {
      // Check hash values
      if pattern_hash == text_hash {
        // Check actual characters
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
      
      // Calculate hash for next window
      if i < (text_len - pattern_len) {
        text_hash = (256 * (text_hash - text[i].to_int() * h) + text[i + pattern_len].to_int()) % prime
        
        if text_hash < 0 {
          text_hash = text_hash + prime
        }
      }
    }
    
    None
  }
  
  // Test string searching algorithms
  let text = "ABABDABACDABABCABAB"
  let pattern = "ABABCABAB"
  
  // Test naive search
  let naive_result = naive_search(text, pattern)
  assert_eq(naive_result, Some(10))
  
  // Test KMP search
  let kmp_result = kmp_search(text, pattern)
  assert_eq(kmp_result, Some(10))
  
  // Test Rabin-Karp search
  let rk_result = rabin_karp_search(text, pattern)
  assert_eq(rk_result, Some(10))
  
  // Test with pattern at beginning
  let pattern_begin = "ABABD"
  assert_eq(naive_search(text, pattern_begin), Some(0))
  assert_eq(kmp_search(text, pattern_begin), Some(0))
  assert_eq(rabin_karp_search(text, pattern_begin), Some(0))
  
  // Test with pattern at end
  let pattern_end = "ABAB"
  assert_eq(naive_search(text, pattern_end), Some(15))
  assert_eq(kmp_search(text, pattern_end), Some(15))
  assert_eq(rabin_karp_search(text, pattern_end), Some(15))
  
  // Test with non-existing pattern
  let pattern_nonexistent = "XYZ"
  assert_eq(naive_search(text, pattern_nonexistent), None)
  assert_eq(kmp_search(text, pattern_nonexistent), None)
  assert_eq(rabin_karp_search(text, pattern_nonexistent), None)
  
  // Test with empty pattern
  assert_eq(naive_search(text, ""), Some(0))
  assert_eq(kmp_search(text, ""), Some(0))
  assert_eq(rabin_karp_search(text, ""), Some(0))
  
  // Test with pattern longer than text
  assert_eq(naive_search("short", "longerpattern"), None)
  assert_eq(kmp_search("short", "longerpattern"), None)
  assert_eq(rabin_karp_search("short", "longerpattern"), None)
  
  // Test with single character
  assert_eq(naive_search("hello", "h"), Some(0))
  assert_eq(kmp_search("hello", "h"), Some(0))
  assert_eq(rabin_karp_search("hello", "h"), Some(0))
  
  // Test with repeated characters
  let repeated_text = "AAAAAA"
  let repeated_pattern = "AAA"
  assert_eq(naive_search(repeated_text, repeated_pattern), Some(0))
  assert_eq(kmp_search(repeated_text, repeated_pattern), Some(0))
  assert_eq(rabin_karp_search(repeated_text, repeated_pattern), Some(0))
}