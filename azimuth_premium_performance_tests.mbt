// Azimuth Premium Performance Tests
// This file contains high-quality test cases for performance optimization

// Test 1: Algorithm Complexity Optimization
test "algorithm complexity optimization" {
  // Test O(n²) vs O(n log n) sorting algorithms
  
  // Inefficient bubble sort (O(n²))
  let bubble_sort = fn(arr : Array[Int]) -> Array[Int] {
    let mut result = arr.clone()
    let n = result.length()
    
    for i = 0; i < n; i = i + 1 {
      for j = 0; j < n - i - 1; j = j + 1 {
        if result[j] > result[j + 1] {
          let temp = result[j]
          result[j] = result[j + 1]
          result[j + 1] = temp
        }
      }
    }
    
    result
  }
  
  // Efficient merge sort (O(n log n))
  let merge_sort = fn(arr : Array[Int]) -> Array[Int] {
    if arr.length() <= 1 {
      return arr
    }
    
    let mid = arr.length() / 2
    let left = merge_sort(arr.slice(0, mid))
    let right = merge_sort(arr.slice(mid, arr.length()))
    
    let merge = fn(left : Array[Int], right : Array[Int]) -> Array[Int] {
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
    
    merge(left, right)
  }
  
  // Test with small array (performance difference minimal)
  let small_array = [5, 2, 8, 1, 9, 3]
  let bubble_small = bubble_sort(small_array)
  let merge_small = merge_sort(small_array)
  
  assert_eq(bubble_small, [1, 2, 3, 5, 8, 9])
  assert_eq(merge_small, [1, 2, 3, 5, 8, 9])
  
  // Test with larger array (performance difference significant)
  let large_array = [42, 17, 23, 8, 16, 15, 4, 42, 23, 15]
  let bubble_large = bubble_sort(large_array)
  let merge_large = merge_sort(large_array)
  
  assert_eq(bubble_large, [4, 8, 15, 15, 16, 17, 23, 23, 42, 42])
  assert_eq(merge_large, [4, 8, 15, 15, 16, 17, 23, 23, 42, 42])
  
  // Test search algorithms
  let linear_search = fn(arr : Array[Int], target : Int) -> Option[Int] {
    for i = 0; i < arr.length(); i = i + 1 {
      if arr[i] == target {
        return Some(i)
      }
    }
    None
  }
  
  let binary_search = fn(arr : Array[Int], target : Int) -> Option[Int] {
    let mut low = 0
    let mut high = arr.length() - 1
    
    while low <= high {
      let mid = (low + high) / 2
      if arr[mid] == target {
        return Some(mid)
      } else if arr[mid] < target {
        low = mid + 1
      } else {
        high = mid - 1
      }
    }
    
    None
  }
  
  let sorted_array = [1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25]
  
  // Test linear search
  let linear_result = linear_search(sorted_array, 15)
  assert_eq(linear_result, Some(7))
  
  // Test binary search
  let binary_result = binary_search(sorted_array, 15)
  assert_eq(binary_result, Some(7))
  
  // Test not found cases
  assert_eq(linear_search(sorted_array, 8), None)
  assert_eq(binary_search(sorted_array, 8), None)
}

// Test 2: Memory Access Patterns Optimization
test "memory access patterns optimization" {
  // Test cache-friendly vs cache-unfriendly operations
  
  // Cache-unfriendly: Column-major access in row-major array
  let column_major_sum = fn(matrix : Array[Array[Int]]) -> Int {
    let mut sum = 0
    let cols = matrix[0].length()
    let rows = matrix.length()
    
    for col = 0; col < cols; col = col + 1 {
      for row = 0; row < rows; row = row + 1 {
        sum = sum + matrix[row][col]
      }
    }
    
    sum
  }
  
  // Cache-friendly: Row-major access in row-major array
  let row_major_sum = fn(matrix : Array[Array[Int]]) -> Int {
    let mut sum = 0
    
    for row = 0; row < matrix.length(); row = row + 1 {
      for col = 0; col < matrix[row].length(); col = col + 1 {
        sum = sum + matrix[row][col]
      }
    }
    
    sum
  }
  
  let test_matrix = [
    [1, 2, 3, 4],
    [5, 6, 7, 8],
    [9, 10, 11, 12],
    [13, 14, 15, 16],
    [17, 18, 19, 20]
  ]
  
  let column_sum = column_major_sum(test_matrix)
  let row_sum = row_major_sum(test_matrix)
  
  // Both should give the same result
  assert_eq(column_sum, row_sum)
  assert_eq(column_sum, 210) // Sum of 1 to 20
  
  // Test memory locality with array operations
  let sequential_access = fn(arr : Array[Int]) -> Int {
    let mut sum = 0
    for i = 0; i < arr.length(); i = i + 1 {
      sum = sum + arr[i]
    }
    sum
  }
  
  let random_access = fn(arr : Array[Int], indices : Array[Int]) -> Int {
    let mut sum = 0
    for index in indices {
      if index >= 0 && index < arr.length() {
        sum = sum + arr[index]
      }
    }
    sum
  }
  
  let test_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  let random_indices = [15, 2, 8, 11, 3, 14, 6, 9, 1, 12]
  
  let sequential_result = sequential_access(test_array)
  let random_result = random_access(test_array, random_indices)
  
  assert_eq(sequential_result, 136) // Sum of 1 to 16
  assert_eq(random_result, 81) // Sum of elements at random indices
}

// Test 3: Lazy Evaluation and Memoization
test "lazy evaluation and memoization" {
  // Test memoization for expensive calculations
  type MemoCache {
    cache : Array[(Int, Int)]
    max_size : Int
  }
  
  let create_cache = fn(max_size : Int) -> MemoCache {
    MemoCache { 
      cache: [], 
      max_size: max_size 
    }
  }
  
  let get_from_cache = fn(cache : MemoCache, key : Int) -> Option[Int] {
    for (k, v) in cache.cache {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  let add_to_cache = fn(cache : MemoCache, key : Int, value : Int) -> MemoCache {
    let mut new_cache = cache.cache
    
    // Remove oldest entry if cache is full
    if new_cache.length() >= cache.max_size {
      new_cache.shift()
    }
    
    new_cache.push((key, value))
    MemoCache { 
      cache: new_cache, 
      max_size: cache.max_size 
    }
  }
  
  // Expensive function to memoize
  let expensive_fibonacci = fn(n : Int, cache : MemoCache) -> (Int, MemoCache) {
    let cached = get_from_cache(cache, n)
    match cached {
      Some(value) => return (value, cache)
      None => ()
    }
    
    let result = if n <= 1 {
      n
    } else {
      let (fib1, cache1) = expensive_fibonacci(n - 1, cache)
      let (fib2, cache2) = expensive_fibonacci(n - 2, cache1)
      fib1 + fib2
    }
    
    let new_cache = add_to_cache(cache, n, result)
    (result, new_cache)
  }
  
  // Test memoization
  let cache = create_cache(100)
  let (fib10, _) = expensive_fibonacci(10, cache)
  assert_eq(fib10, 55)
  
  // Test lazy evaluation with thunks
  type LazyValue {
    computed : Bool
    value : Int
    computation : () -> Int
  }
  
  let create_lazy = fn(computation : () -> Int) -> LazyValue {
    LazyValue { 
      computed: false, 
      value: 0, 
      computation: computation 
    }
  }
  
  let force = fn(lazy : LazyValue) -> (Int, LazyValue) {
    if lazy.computed {
      (lazy.value, lazy)
    } else {
      let computed_value = lazy.computation()
      let forced = LazyValue {
        computed: true,
        value: computed_value,
        computation: lazy.computation
      }
      (computed_value, forced)
    }
  }
  
  // Test lazy evaluation
  let expensive_computation = fn() -> Int {
    // Simulate expensive computation
    let mut sum = 0
    for i = 0; i < 1000; i = i + 1 {
      sum = sum + i
    }
    sum
  }
  
  let lazy_value = create_lazy(expensive_computation)
  assert_false(lazy_value.computed)
  
  let (value1, forced1) = force(lazy_value)
  assert_eq(value1, 499500) // Sum of 0 to 999
  assert_true(forced1.computed)
  
  // Second access should use cached value
  let (value2, forced2) = force(forced1)
  assert_eq(value2, 499500)
  assert_true(forced2.computed)
}

// Test 4: String Operations Optimization
test "string operations optimization" {
  // Test string concatenation methods
  let inefficient_concat = fn(strings : Array[String]) -> String {
    let mut result = ""
    for s in strings {
      result = result + s
    }
    result
  }
  
  let efficient_concat = fn(strings : Array[String]) -> String {
    let mut result_parts = []
    let mut total_length = 0
    
    for s in strings {
      result_parts.push(s)
      total_length = total_length + s.length()
    }
    
    // Pre-allocate and join
    let mut result = ""
    for part in result_parts {
      result = result + part
    }
    result
  }
  
  let test_strings = ["Hello", " ", "World", "!", " ", "This", " ", "is", " ", "a", " ", "test"]
  
  let inefficient_result = inefficient_concat(test_strings)
  let efficient_result = efficient_concat(test_strings)
  
  assert_eq(inefficient_result, "Hello World! This is a test")
  assert_eq(efficient_result, "Hello World! This is a test")
  
  // Test string searching algorithms
  let naive_search = fn(text : String, pattern : String) -> Option[Int] {
    let text_chars = text.to_char_array()
    let pattern_chars = pattern.to_char_array()
    
    for i = 0; i <= text_chars.length() - pattern_chars.length(); i = i + 1 {
      let mut found = true
      
      for j = 0; j < pattern_chars.length(); j = j + 1 {
        if text_chars[i + j] != pattern_chars[j] {
          found = false
          break
        }
      }
      
      if found {
        return Some(i)
      }
    }
    
    None
  }
  
  let kmp_search = fn(text : String, pattern : String) -> Option[Int] {
    // Simplified KMP implementation
    let text_chars = text.to_char_array()
    let pattern_chars = pattern.to_char_array()
    
    if pattern_chars.length() == 0 {
      return Some(0)
    }
    
    // Build failure function
    let mut failure = [0; pattern_chars.length()]
    let mut j = 0
    
    for i = 1; i < pattern_chars.length(); i = i + 1 {
      while j > 0 && pattern_chars[j] != pattern_chars[i] {
        j = failure[j - 1]
      }
      
      if pattern_chars[j] == pattern_chars[i] {
        j = j + 1
        failure[i] = j
      }
    }
    
    // Search
    j = 0
    for i = 0; i < text_chars.length(); i = i + 1 {
      while j > 0 && pattern_chars[j] != text_chars[i] {
        j = failure[j - 1]
      }
      
      if pattern_chars[j] == text_chars[i] {
        j = j + 1
        
        if j == pattern_chars.length() {
          return Some(i - j + 1)
        }
      }
    }
    
    None
  }
  
  let text = "abracadabra"
  let pattern = "cad"
  
  let naive_result = naive_search(text, pattern)
  let kmp_result = kmp_search(text, pattern)
  
  assert_eq(naive_result, Some(4))
  assert_eq(kmp_result, Some(4))
  
  // Test not found
  assert_eq(naive_search(text, "xyz"), None)
  assert_eq(kmp_search(text, "xyz"), None)
}

// Test 5: Data Structure Selection Optimization
test "data structure selection optimization" {
  // Test array vs linked list for different operations
  type ListNode {
    value : Int
    next : Option[ListNode]
  }
  
  let create_linked_list = fn(values : Array[Int]) -> Option[ListNode] {
    if values.length() == 0 {
      return None
    }
    
    let mut head = None
    for i = values.length() - 1; i >= 0; i = i - 1 {
      head = Some(ListNode { 
        value: values[i], 
        next: head 
      })
    }
    
    head
  }
  
  let linked_list_to_array = fn(head : Option[ListNode]) -> Array[Int] {
    let mut result = []
    let mut current = head
    
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
  
  // Test insertion at beginning (O(1) for linked list, O(n) for array)
  let linked_list_insert = fn(head : Option[ListNode], value : Int) -> Option[ListNode] {
    Some(ListNode { 
      value: value, 
      next: head 
    })
  }
  
  let array_insert_beginning = fn(arr : Array[Int], value : Int) -> Array[Int] {
    let mut result = [value]
    for v in arr {
      result.push(v)
    }
    result
  }
  
  let test_values = [1, 2, 3, 4, 5]
  let linked_head = create_linked_list(test_values)
  let array_values = test_values
  
  // Insert at beginning
  let new_linked_head = linked_list_insert(linked_head, 0)
  let new_array = array_insert_beginning(array_values, 0)
  
  let linked_result = linked_list_to_array(new_linked_head)
  
  assert_eq(linked_result, [0, 1, 2, 3, 4, 5])
  assert_eq(new_array, [0, 1, 2, 3, 4, 5])
  
  // Test random access (O(n) for linked list, O(1) for array)
  let array_get = fn(arr : Array[Int], index : Int) -> Option[Int] {
    if index >= 0 && index < arr.length() {
      Some(arr[index])
    } else {
      None
    }
  }
  
  let linked_list_get = fn(head : Option[ListNode], index : Int) -> Option[Int] {
    let mut current = head
    let mut i = 0
    
    while current.is_some() && i < index {
      match current {
        Some(node) => {
          current = node.next
          i = i + 1
        }
        None => break
      }
    }
    
    match current {
      Some(node) => Some(node.value)
      None => None
    }
  }
  
  assert_eq(array_get(new_array, 3), Some(3))
  assert_eq(linked_list_get(new_linked_head, 3), Some(3))
  
  // Test hash map vs array for lookups
  type SimpleMap {
    keys : Array[String]
    values : Array[Int]
  }
  
  let create_map = fn() -> SimpleMap {
    SimpleMap { keys: [], values: [] }
  }
  
  let map_set = fn(map : SimpleMap, key : String, value : Int) -> SimpleMap {
    let mut new_keys = map.keys
    let mut new_values = map.values
    
    new_keys.push(key)
    new_values.push(value)
    
    SimpleMap { 
      keys: new_keys, 
      values: new_values 
    }
  }
  
  let map_get = fn(map : SimpleMap, key : String) -> Option[Int] {
    for i = 0; i < map.keys.length(); i = i + 1 {
      if map.keys[i] == key {
        return Some(map.values[i])
      }
    }
    None
  }
  
  let array_lookup = fn(arr : Array[(String, Int)], key : String) -> Option[Int] {
    for (k, v) in arr {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  // Test map operations
  let mut map = create_map()
  map = map_set(map, "apple", 5)
  map = map_set(map, "banana", 3)
  map = map_set(map, "cherry", 7)
  
  let array_data = [("apple", 5), ("banana", 3), ("cherry", 7)]
  
  assert_eq(map_get(map, "banana"), Some(3))
  assert_eq(array_lookup(array_data, "banana"), Some(3))
  
  assert_eq(map_get(map, "orange"), None)
  assert_eq(array_lookup(array_data, "orange"), None)
}

// Test 6: Parallel Processing Optimization
test "parallel processing optimization" {
  // Test divide and conquer vs sequential processing
  let sequential_sum = fn(arr : Array[Int]) -> Int {
    let mut sum = 0
    for v in arr {
      sum = sum + v
    }
    sum
  }
  
  let parallel_sum = fn(arr : Array[Int]) -> Int {
    if arr.length() <= 2 {
      return sequential_sum(arr)
    }
    
    let mid = arr.length() / 2
    let left_sum = parallel_sum(arr.slice(0, mid))
    let right_sum = parallel_sum(arr.slice(mid, arr.length()))
    
    left_sum + right_sum
  }
  
  let test_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  
  let seq_result = sequential_sum(test_array)
  let par_result = parallel_sum(test_array)
  
  assert_eq(seq_result, 136) // Sum of 1 to 16
  assert_eq(par_result, 136)
  
  // Test map-reduce pattern
  let map_reduce = fn(arr : Array[Int], map_fn : Int -> Int, reduce_fn : (Int, Int) -> Int, initial : Int) -> Int {
    let mut result = initial
    
    for v in arr {
      let mapped = map_fn(v)
      result = reduce_fn(result, mapped)
    }
    
    result
  }
  
  // Calculate sum of squares
  let sum_of_squares = map_reduce(
    test_array,
    |x| x * x,
    |acc, val| acc + val,
    0
  )
  
  // Verify with sequential approach
  let mut sequential_sq_sum = 0
  for v in test_array {
    sequential_sq_sum = sequential_sq_sum + v * v
  }
  
  assert_eq(sum_of_squares, sequential_sq_sum)
  assert_eq(sum_of_squares, 1496) // Sum of squares of 1 to 16
  
  // Test batch processing
  let process_batch = fn(batch : Array[Int]) -> Array[Int] {
    batch.map(|x| x * 2)
  }
  
  let batch_process = fn(arr : Array[Int], batch_size : Int) -> Array[Int] {
    let mut result = []
    
    for i = 0; i < arr.length(); i = i + batch_size {
      let end = if i + batch_size > arr.length() { arr.length() } else { i + batch_size }
      let batch = arr.slice(i, end)
      let processed = process_batch(batch)
      
      for v in processed {
        result.push(v)
      }
    }
    
    result
  }
  
  let batched_result = batch_process(test_array, 4)
  let expected = [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32]
  
  assert_eq(batched_result, expected)
}