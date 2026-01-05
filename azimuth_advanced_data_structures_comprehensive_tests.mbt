// Azimuth Telemetry System - Advanced Data Structures Comprehensive Test Suite
// This file contains comprehensive test cases for advanced data structures and algorithms

// Test 1: Tree Data Structure Operations
test "tree data structure operations" {
  // Test binary tree creation and traversal
  type TreeNode {
    value : Int
    left : Option[TreeNode]
    right : Option[TreeNode]
  }
  
  // Create a binary tree
  let tree = TreeNode {
    value: 10,
    left: Some(TreeNode {
      value: 5,
      left: Some(TreeNode { value: 3, left: None, right: None }),
      right: Some(TreeNode { value: 7, left: None, right: None })
    }),
    right: Some(TreeNode {
      value: 15,
      left: Some(TreeNode { value: 12, left: None, right: None }),
      right: Some(TreeNode { value: 20, left: None, right: None })
    })
  }
  
  // In-order traversal should yield sorted values
  let mut inorder_result = []
  let mut stack = []
  let mut current = Some(tree)
  
  while current.is_some() || stack.length() > 0 {
    match current {
      Some(node) => {
        stack.push(node)
        current = node.left
      }
      None => {
        let node = stack.pop()
        inorder_result.push(node.value)
        current = node.right
      }
    }
  }
  
  assert_eq(inorder_result, [3, 5, 7, 10, 12, 15, 20])
  
  // Test tree search
  let tree_search = |node: Option[TreeNode], target: Int| -> Bool {
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
  
  assert_true(tree_search(Some(tree), 7))
  assert_false(tree_search(Some(tree), 8))
}

// Test 2: Graph Data Structure Operations
test "graph data structure operations" {
  // Simple graph representation using adjacency list
  type Graph {
    vertices: Array[String]
    edges: Array[(String, String)]
  }
  
  // Create a graph
  let graph = Graph {
    vertices: ["A", "B", "C", "D", "E"],
    edges: [
      ("A", "B"), ("A", "C"),
      ("B", "D"), ("C", "D"),
      ("D", "E"), ("C", "E")
    ]
  }
  
  // Test adjacency list creation
  let create_adjacency_list = |g: Graph| -> Array[(String, Array[String])] {
    let mut adjacency_list = []
    
    for vertex in g.vertices {
      let mut neighbors = []
      for (from, to) in g.edges {
        if from == vertex {
          neighbors.push(to)
        }
      }
      adjacency_list.push((vertex, neighbors))
    }
    
    adjacency_list
  }
  
  let adjacency_list = create_adjacency_list(graph)
  
  // Verify adjacency list
  let find_neighbors = |vertex: String| -> Array[String] {
    for (v, neighbors) in adjacency_list {
      if v == vertex {
        return neighbors
      }
    }
    []
  }
  
  assert_eq(find_neighbors("A"), ["B", "C"])
  assert_eq(find_neighbors("B"), ["D"])
  assert_eq(find_neighbors("C"), ["D", "E"])
  assert_eq(find_neighbors("D"), ["E"])
  assert_eq(find_neighbors("E"), [])
  
  // Test breadth-first search
  let bfs = |g: Graph, start: String, target: String| -> Bool {
    if !g.vertices.contains(start) || !g.vertices.contains(target) {
      return false
    }
    
    let mut queue = [start]
    let mut visited = []
    visited.push(start)
    
    while queue.length() > 0 {
      let current = queue.shift()
      
      if current == target {
        return true
      }
      
      let neighbors = find_neighbors(current)
      for neighbor in neighbors {
        if !visited.contains(neighbor) {
          visited.push(neighbor)
          queue.push(neighbor)
        }
      }
    }
    
    false
  }
  
  assert_true(bfs(graph, "A", "E"))
  assert_false(bfs(graph, "E", "A"))
}

// Test 3: Hash Table Operations
test "hash table operations" {
  // Simple hash table implementation using separate chaining
  type HashTable {
    buckets: Array[Array[(String, String)]]
    size: Int
  }
  
  let hash_function = |key: String, size: Int| -> Int {
    let mut hash = 0
    for char in key.to_char_array() {
      hash = (hash * 31 + char.to_int()) % size
    }
    hash
  }
  
  let create_hash_table = |size: Int| -> HashTable {
    let mut buckets = []
    for i = 0; i < size; i = i + 1 {
      buckets.push([])
    }
    HashTable { buckets, size }
  }
  
  let hash_table_insert = |table: HashTable, key: String, value: String| -> HashTable {
    let index = hash_function(key, table.size)
    let mut new_buckets = table.buckets
    
    let mut bucket = new_buckets[index]
    let mut found = false
    
    // Check if key already exists
    for i = 0; i < bucket.length(); i = i + 1 {
      if bucket[i].0 == key {
        bucket[i] = (key, value)
        found = true
        break
      }
    }
    
    // If key doesn't exist, add it
    if !found {
      bucket.push((key, value))
    }
    
    new_buckets[index] = bucket
    HashTable { buckets: new_buckets, size: table.size }
  }
  
  let hash_table_get = |table: HashTable, key: String| -> Option[String] {
    let index = hash_function(key, table.size)
    let bucket = table.buckets[index]
    
    for (k, v) in bucket {
      if k == key {
        return Some(v)
      }
    }
    
    None
  }
  
  // Test hash table operations
  let table = create_hash_table(10)
  let table1 = hash_table_insert(table, "name", "Azimuth")
  let table2 = hash_table_insert(table1, "version", "1.0.0")
  let table3 = hash_table_insert(table2, "type", "telemetry")
  
  assert_eq(hash_table_get(table3, "name"), Some("Azimuth"))
  assert_eq(hash_table_get(table3, "version"), Some("1.0.0"))
  assert_eq(hash_table_get(table3, "type"), Some("telemetry"))
  assert_eq(hash_table_get(table3, "nonexistent"), None)
  
  // Test updating existing key
  let table4 = hash_table_insert(table3, "version", "2.0.0")
  assert_eq(hash_table_get(table4, "version"), Some("2.0.0"))
}

// Test 4: Stack and Queue Operations
test "stack and queue operations" {
  // Test stack operations
  type Stack {
    items: Array[String]
  }
  
  let stack_push = |stack: Stack, item: String| -> Stack {
    let mut new_items = stack.items
    new_items.push(item)
    Stack { items: new_items }
  }
  
  let stack_pop = |stack: Stack| -> (Stack, Option[String]) {
    if stack.items.length() == 0 {
      return (stack, None)
    }
    
    let mut new_items = stack.items
    let item = new_items.pop()
    (Stack { items: new_items }, Some(item))
  }
  
  let stack_peek = |stack: Stack| -> Option[String] {
    if stack.items.length() == 0 {
      return None
    }
    Some(stack.items[stack.items.length() - 1])
  }
  
  let empty_stack = Stack { items: [] }
  let stack1 = stack_push(empty_stack, "first")
  let stack2 = stack_push(stack1, "second")
  let stack3 = stack_push(stack2, "third")
  
  assert_eq(stack_peek(stack3), Some("third"))
  
  let (stack4, item1) = stack_pop(stack3)
  assert_eq(item1, Some("third"))
  assert_eq(stack_peek(stack4), Some("second"))
  
  let (stack5, item2) = stack_pop(stack4)
  assert_eq(item2, Some("second"))
  assert_eq(stack_peek(stack5), Some("first"))
  
  let (stack6, item3) = stack_pop(stack5)
  assert_eq(item3, Some("first"))
  assert_eq(stack_peek(stack6), None)
  
  let (_, item4) = stack_pop(stack6)
  assert_eq(item4, None)
  
  // Test queue operations
  type Queue {
    items: Array[String]
  }
  
  let queue_enqueue = |queue: Queue, item: String| -> Queue {
    let mut new_items = queue.items
    new_items.push(item)
    Queue { items: new_items }
  }
  
  let queue_dequeue = |queue: Queue| -> (Queue, Option[String]) {
    if queue.items.length() == 0 {
      return (queue, None)
    }
    
    let mut new_items = queue.items
    let item = new_items.shift()
    (Queue { items: new_items }, Some(item))
  }
  
  let queue_front = |queue: Queue| -> Option[String] {
    if queue.items.length() == 0 {
      return None
    }
    Some(queue.items[0])
  }
  
  let empty_queue = Queue { items: [] }
  let queue1 = queue_enqueue(empty_queue, "first")
  let queue2 = queue_enqueue(queue1, "second")
  let queue3 = queue_enqueue(queue2, "third")
  
  assert_eq(queue_front(queue3), Some("first"))
  
  let (queue4, item1) = queue_dequeue(queue3)
  assert_eq(item1, Some("first"))
  assert_eq(queue_front(queue4), Some("second"))
  
  let (queue5, item2) = queue_dequeue(queue4)
  assert_eq(item2, Some("second"))
  assert_eq(queue_front(queue5), Some("third"))
  
  let (queue6, item3) = queue_dequeue(queue5)
  assert_eq(item3, Some("third"))
  assert_eq(queue_front(queue6), None)
  
  let (_, item4) = queue_dequeue(queue6)
  assert_eq(item4, None)
}

// Test 5: Priority Queue Operations
test "priority queue operations" {
  type PriorityQueue {
    items: Array[(Int, String)]  // (priority, value)
  }
  
  let priority_queue_enqueue = |queue: PriorityQueue, priority: Int, value: String| -> PriorityQueue {
    let mut new_items = queue.items
    new_items.push((priority, value))
    
    // Sort by priority (ascending)
    new_items.sort_by(|(p1, _), (p2, _)| p1 - p2)
    
    PriorityQueue { items: new_items }
  }
  
  let priority_queue_dequeue = |queue: PriorityQueue| -> (PriorityQueue, Option[(Int, String)]) {
    if queue.items.length() == 0 {
      return (queue, None)
    }
    
    let mut new_items = queue.items
    let item = new_items.shift()
    (PriorityQueue { items: new_items }, Some(item))
  }
  
  let empty_pq = PriorityQueue { items: [] }
  let pq1 = priority_queue_enqueue(empty_pq, 3, "low priority")
  let pq2 = priority_queue_enqueue(pq1, 1, "high priority")
  let pq3 = priority_queue_enqueue(pq2, 2, "medium priority")
  
  // Dequeue should return items in priority order
  let (pq4, item1) = priority_queue_dequeue(pq3)
  assert_eq(item1, Some((1, "high priority")))
  
  let (pq5, item2) = priority_queue_dequeue(pq4)
  assert_eq(item2, Some((2, "medium priority")))
  
  let (pq6, item3) = priority_queue_dequeue(pq5)
  assert_eq(item3, Some((3, "low priority")))
  
  let (_, item4) = priority_queue_dequeue(pq6)
  assert_eq(item4, None)
}

// Test 6: Advanced Sorting Algorithms
test "advanced sorting algorithms" {
  // Test merge sort
  let merge_sort = |arr: Array[Int]| -> Array[Int] {
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
        result.push(left[i])
        i = i + 1
      } else {
        result.push(right[j])
        j = j + 1
      }
    }
    
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
  
  // Test quick sort
  let quick_sort = |arr: Array[Int]| -> Array[Int] {
    if arr.length() <= 1 {
      return arr
    }
    
    let pivot = arr[0]
    let mut less = []
    let mut equal = []
    let mut greater = []
    
    for item in arr {
      if item < pivot {
        less.push(item)
      } else if item > pivot {
        greater.push(item)
      } else {
        equal.push(item)
      }
    }
    
    let mut result = quick_sort(less)
    for e in equal {
      result.push(e)
    }
    for g in quick_sort(greater) {
      result.push(g)
    }
    
    result
  }
  
  // Test sorting algorithms
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6]
  
  let merge_sorted = merge_sort(unsorted)
  assert_eq(merge_sorted, [1, 2, 3, 4, 5, 6, 7, 8, 9])
  
  let quick_sorted = quick_sort(unsorted)
  assert_eq(quick_sorted, [1, 2, 3, 4, 5, 6, 7, 8, 9])
  
  // Test with edge cases
  let empty = []
  let single = [42]
  let already_sorted = [1, 2, 3, 4, 5]
  let reverse_sorted = [5, 4, 3, 2, 1]
  let with_duplicates = [3, 1, 4, 1, 5, 9, 2, 6, 5]
  
  assert_eq(merge_sort(empty), [])
  assert_eq(merge_sort(single), [42])
  assert_eq(merge_sort(already_sorted), [1, 2, 3, 4, 5])
  assert_eq(merge_sort(reverse_sorted), [1, 2, 3, 4, 5])
  assert_eq(merge_sort(with_duplicates), [1, 1, 2, 3, 4, 5, 5, 6, 9])
  
  assert_eq(quick_sort(empty), [])
  assert_eq(quick_sort(single), [42])
  assert_eq(quick_sort(already_sorted), [1, 2, 3, 4, 5])
  assert_eq(quick_sort(reverse_sorted), [1, 2, 3, 4, 5])
  assert_eq(quick_sort(with_duplicates), [1, 1, 2, 3, 4, 5, 5, 6, 9])
}

// Test 7: Advanced Search Algorithms
test "advanced search algorithms" {
  // Test binary search
  let binary_search = |arr: Array[Int], target: Int| -> Int {
    let mut low = 0
    let mut high = arr.length() - 1
    
    while low <= high {
      let mid = (low + high) / 2
      if arr[mid] == target {
        return mid
      } else if arr[mid] < target {
        low = mid + 1
      } else {
        high = mid - 1
      }
    }
    
    -1  // Not found
  }
  
  // Test interpolation search
  let interpolation_search = |arr: Array[Int], target: Int| -> Int {
    let mut low = 0
    let mut high = arr.length() - 1
    
    while low <= high && target >= arr[low] && target <= arr[high] {
      if low == high {
        if arr[low] == target {
          return low
        }
        return -1
      }
      
      // Estimate position
      let pos = low + ((target - arr[low]) * (high - low)) / (arr[high] - arr[low])
      
      if arr[pos] == target {
        return pos
      } else if arr[pos] < target {
        low = pos + 1
      } else {
        high = pos - 1
      }
    }
    
    -1  // Not found
  }
  
  // Test search algorithms
  let sorted = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19]
  
  assert_eq(binary_search(sorted, 7), 3)
  assert_eq(binary_search(sorted, 1), 0)
  assert_eq(binary_search(sorted, 19), 9)
  assert_eq(binary_search(sorted, 8), -1)  // Not found
  
  assert_eq(interpolation_search(sorted, 7), 3)
  assert_eq(interpolation_search(sorted, 1), 0)
  assert_eq(interpolation_search(sorted, 19), 9)
  assert_eq(interpolation_search(sorted, 8), -1)  // Not found
  
  // Test with uniformly distributed data (better for interpolation search)
  let uniform = [10, 20, 30, 40, 50, 60, 70, 80, 90, 100]
  
  assert_eq(binary_search(uniform, 50), 4)
  assert_eq(interpolation_search(uniform, 50), 4)
  assert_eq(binary_search(uniform, 25), -1)
  assert_eq(interpolation_search(uniform, 25), -1)
}

// Test 8: Dynamic Programming - Fibonacci
test "dynamic programming - fibonacci" {
  // Recursive Fibonacci (inefficient)
  let fib_recursive = |n: Int| -> Int {
    if n <= 1 {
      return n
    }
    fib_recursive(n - 1) + fib_recursive(n - 2)
  }
  
  // Memoized Fibonacci (efficient)
  let fib_memoized = |n: Int| -> Int {
    let mut memo = []
    for i = 0; i <= n; i = i + 1 {
      memo.push(-1)
    }
    
    let fib_helper = |k: Int| -> Int {
      if memo[k] != -1 {
        return memo[k]
      }
      
      if k <= 1 {
        memo[k] = k
        return k
      }
      
      memo[k] = fib_helper(k - 1) + fib_helper(k - 2)
      memo[k]
    }
    
    fib_helper(n)
  }
  
  // Iterative Fibonacci (most efficient)
  let fib_iterative = |n: Int| -> Int {
    if n <= 1 {
      return n
    }
    
    let mut a = 0
    let mut b = 1
    
    for i = 2; i <= n; i = i + 1 {
      let temp = a + b
      a = b
      b = temp
    }
    
    b
  }
  
  // Test Fibonacci implementations
  assert_eq(fib_recursive(0), 0)
  assert_eq(fib_recursive(1), 1)
  assert_eq(fib_recursive(5), 5)
  assert_eq(fib_recursive(7), 13)
  
  assert_eq(fib_memoized(0), 0)
  assert_eq(fib_memoized(1), 1)
  assert_eq(fib_memoized(5), 5)
  assert_eq(fib_memoized(7), 13)
  assert_eq(fib_memoized(10), 55)
  assert_eq(fib_memoized(20), 6765)
  
  assert_eq(fib_iterative(0), 0)
  assert_eq(fib_iterative(1), 1)
  assert_eq(fib_iterative(5), 5)
  assert_eq(fib_iterative(7), 13)
  assert_eq(fib_iterative(10), 55)
  assert_eq(fib_iterative(20), 6765)
}

// Test 9: Linked List Operations
test "linked list operations" {
  type LinkedList {
    value: String
    next: Option[LinkedList]
  }
  
  let linked_list_create = |values: Array[String]| -> Option[LinkedList] {
    if values.length() == 0 {
      return None
    }
    
    let mut head = None
    for i = values.length() - 1; i >= 0; i = i - 1 {
      head = Some(LinkedList {
        value: values[i],
        next: head
      })
    }
    
    head
  }
  
  let linked_list_to_array = |list: Option[LinkedList]| -> Array[String] {
    let mut result = []
    let mut current = list
    
    while current.is_some() {
      match current {
        Some(node) => {
          result.push(node.value)
          current = node.next
        }
        None => break
      }
    }
    
    result
  }
  
  let linked_list_length = |list: Option[LinkedList]| -> Int {
    let mut length = 0
    let mut current = list
    
    while current.is_some() {
      match current {
        Some(_) => {
          length = length + 1
          current = match current { Some(n) => n.next, None => None }
        }
        None => break
      }
    }
    
    length
  }
  
  let linked_list_reverse = |list: Option[LinkedList]| -> Option[LinkedList] {
    let mut prev = None
    let mut current = list
    
    while current.is_some() {
      match current {
        Some(node) => {
          let next = node.next
          let reversed_node = LinkedList {
            value: node.value,
            next: prev
          }
          prev = Some(reversed_node)
          current = next
        }
        None => break
      }
    }
    
    prev
  }
  
  // Test linked list operations
  let values = ["first", "second", "third", "fourth"]
  let list = linked_list_create(values)
  
  assert_eq(linked_list_length(list), 4)
  assert_eq(linked_list_to_array(list), ["first", "second", "third", "fourth"])
  
  let reversed_list = linked_list_reverse(list)
  assert_eq(linked_list_to_array(reversed_list), ["fourth", "third", "second", "first"])
  
  // Test empty list
  let empty_list = linked_list_create([])
  assert_eq(linked_list_length(empty_list), 0)
  assert_eq(linked_list_to_array(empty_list), [])
  
  let reversed_empty = linked_list_reverse(empty_list)
  assert_eq(linked_list_to_array(reversed_empty), [])
}

// Test 10: Trie (Prefix Tree) Operations
test "trie prefix tree operations" {
  type TrieNode {
    children: Array[(Char, TrieNode)]
    is_end_of_word: Bool
  }
  
  let trie_create = |words: Array[String]| -> TrieNode {
    let root = TrieNode { children: [], is_end_of_word: false }
    
    for word in words {
      let mut current = root
      
      for char in word.to_char_array() {
        let mut found_child = None
        
        for (c, child) in current.children {
          if c == char {
            found_child = Some(child)
            break
          }
        }
        
        match found_child {
          Some(child) => {
            current = child
          }
          None => {
            let new_child = TrieNode { children: [], is_end_of_word: false }
            current.children.push((char, new_child))
            // Get the newly added child
            for (c, child) in current.children {
              if c == char {
                current = child
                break
              }
            }
          }
        }
      }
      
      current.is_end_of_word = true
    }
    
    root
  }
  
  let trie_search = |root: TrieNode, word: String| -> Bool {
    let mut current = root
    
    for char in word.to_char_array() {
      let mut found_child = None
      
      for (c, child) in current.children {
        if c == char {
          found_child = Some(child)
          break
        }
      }
      
      match found_child {
        Some(child) => {
          current = child
        }
        None => {
          return false
        }
      }
    }
    
    current.is_end_of_word
  }
  
  let trie_starts_with = |root: TrieNode, prefix: String| -> Bool {
    let mut current = root
    
    for char in prefix.to_char_array() {
      let mut found_child = None
      
      for (c, child) in current.children {
        if c == char {
          found_child = Some(child)
          break
        }
      }
      
      match found_child {
        Some(child) => {
          current = child
        }
        None => {
          return false
        }
      }
    }
    
    true
  }
  
  // Test trie operations
  let words = ["apple", "app", "application", "apply", "banana", "band", "bandana"]
  let trie = trie_create(words)
  
  // Test exact word search
  assert_true(trie_search(trie, "apple"))
  assert_true(trie_search(trie, "app"))
  assert_true(trie_search(trie, "application"))
  assert_true(trie_search(trie, "apply"))
  assert_true(trie_search(trie, "banana"))
  assert_true(trie_search(trie, "band"))
  assert_true(trie_search(trie, "bandana"))
  
  // Test non-existent words
  assert_false(trie_search(trie, "appl"))
  assert_false(trie_search(trie, "applications"))
  assert_false(trie_search(trie, "ban"))
  assert_false(trie_search(trie, "bandage"))
  
  // Test prefix search
  assert_true(trie_starts_with(trie, "app"))
  assert_true(trie_starts_with(trie, "appl"))
  assert_true(trie_starts_with(trie, "ban"))
  assert_true(trie_starts_with(trie, "band"))
  
  // Test non-existent prefixes
  assert_false(trie_starts_with(trie, "cat"))
  assert_false(trie_starts_with(trie, "dog"))
  assert_false(trie_starts_with(trie, "z"))
}