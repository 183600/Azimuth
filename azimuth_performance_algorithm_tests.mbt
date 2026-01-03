// Azimuth Performance and Algorithm Complexity Tests
// This file contains comprehensive test cases for performance evaluation and algorithm complexity

test "sorting algorithms performance comparison" {
  // Test data generation
  let generate_test_data = fn(size : Int) -> Array[Int] {
    let mut data = []
    for i in 0..size {
      data = data.push((size - i))  // Reverse sorted data (worst case for some algorithms)
    }
    data
  }
  
  // Bubble sort implementation (O(n²))
  let bubble_sort = fn(arr : Array[Int]) -> Array[Int] {
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
  
  // Selection sort implementation (O(n²))
  let selection_sort = fn(arr : Array[Int]) -> Array[Int] {
    let mut result = arr.copy()
    let n = result.length()
    
    for i in 0..n {
      let mut min_idx = i
      for j in (i + 1)..n {
        if result[j] < result[min_idx] {
          min_idx = j
        }
      }
      
      let temp = result[i]
      result[i] = result[min_idx]
      result[min_idx] = temp
    }
    
    result
  }
  
  // Test with small dataset
  let small_data = generate_test_data(10)
  let bubble_result = bubble_sort(small_data)
  let selection_result = selection_sort(small_data)
  
  // Verify both algorithms produce correct results
  assert_eq(bubble_result.length(), 10)
  assert_eq(selection_result.length(), 10)
  
  // Check if sorted
  let mut is_sorted = true
  for i in 1..bubble_result.length() {
    if bubble_result[i - 1] > bubble_result[i] {
      is_sorted = false
      break
    }
  }
  assert_true(is_sorted)
  
  // Performance comparison (conceptual)
  let large_data = generate_test_data(100)
  
  // In a real scenario, we would measure actual time
  // For this test, we just verify the algorithms work with larger datasets
  let large_bubble_result = bubble_sort(large_data)
  let large_selection_result = selection_sort(large_data)
  
  assert_eq(large_bubble_result.length(), 100)
  assert_eq(large_selection_result.length(), 100)
}

test "search algorithms complexity analysis" {
  // Test data generation
  let generate_sorted_data = fn(size : Int) -> Array[Int] {
    let mut data = []
    for i in 0..size {
      data = data.push(i * 2)  // Even numbers: 0, 2, 4, 6, ...
    }
    data
  }
  
  // Linear search implementation (O(n))
  let linear_search = fn(arr : Array[Int], target : Int) -> Option[Int] {
    for i in 0..arr.length() {
      if arr[i] == target {
        return Some(i)
      }
    }
    None
  }
  
  // Binary search implementation (O(log n))
  let binary_search = fn(arr : Array[Int], target : Int) -> Option[Int] {
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
  
  let test_data = generate_sorted_data(1000)
  
  // Test search for existing elements
  let existing_target = 500  // Should be at index 250
  let linear_result = linear_search(test_data, existing_target)
  let binary_result = binary_search(test_data, existing_target)
  
  match linear_result {
    Some(index) => assert_eq(index, 250)
    None => assert_true(false)
  }
  
  match binary_result {
    Some(index) => assert_eq(index, 250)
    None => assert_true(false)
  }
  
  // Test search for non-existing elements
  let non_existing_target = 501  // Odd number, not in our even array
  let linear_non_result = linear_search(test_data, non_existing_target)
  let binary_non_result = binary_search(test_data, non_existing_target)
  
  match linear_non_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  match binary_non_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Performance comparison (conceptual)
  // Binary search should be more efficient for large datasets
  let large_data = generate_sorted_data(10000)
  let large_target = 8000
  
  // Both should find the element, but binary search should be faster
  let large_linear_result = linear_search(large_data, large_target)
  let large_binary_result = binary_search(large_data, large_target)
  
  match large_linear_result {
    Some(index) => assert_eq(index, 4000)
    None => assert_true(false)
  }
  
  match large_binary_result {
    Some(index) => assert_eq(index, 4000)
    None => assert_true(false)
  }
}

test "data structure operation complexity analysis" {
  // Array operations (O(1) for access, O(n) for insertion/deletion)
  let array_access_test = fn() {
    let large_array = []
    for i in 0..10000 {
      large_array = large_array.push(i)
    }
    
    // Random access should be O(1)
    let access_start = 5000
    let value = large_array[access_start]
    assert_eq(value, 5000)
    
    // Insertion at the end should be O(1) amortized
    let mut extended_array = large_array
    extended_array = extended_array.push(10000)
    assert_eq(extended_array.length(), 10001)
    assert_eq(extended_array[10000], 10000)
  }
  
  array_access_test()
  
  // Linked list operations (O(n) for access, O(1) for insertion/deletion at head)
  type ListNode {
    value : Int
    next : Option[ListNode]
  }
  
  let create_linked_list = fn(size : Int) -> Option[ListNode] {
    let mut head = None
    for i in (0..size).rev() {
      head = Some({ value: i, next: head })
    }
    head
  }
  
  let linked_list = create_linked_list(1000)
  
  // Access to head should be O(1)
  match linked_list {
    Some(node) => assert_eq(node.value, 0)
    None => assert_true(false)
  }
  
  // Search for a value should be O(n)
  let search_linked_list = fn(head : Option[ListNode], target : Int) -> Bool {
    let mut current = head
    let mut found = false
    
    while {
      match current {
        Some(node) => {
          if node.value == target {
            found = true
            false
          } else {
            current = node.next
            true
          }
        }
        None => false
      }
    } {}
    
    found
  }
  
  assert_true(search_linked_list(linked_list, 999))
  assert_false(search_linked_list(linked_list, 1001))
}

test "recursive vs iterative performance comparison" {
  // Fibonacci calculation - recursive (exponential) vs iterative (linear)
  let fibonacci_recursive = fn(n : Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci_recursive(n - 1) + fibonacci_recursive(n - 2)
    }
  }
  
  let fibonacci_iterative = fn(n : Int) -> Int {
    if n <= 1 {
      n
    } else {
      let mut a = 0
      let mut b = 1
      let mut i = 2
      
      while i <= n {
        let temp = a + b
        a = b
        b = temp
        i = i + 1
      }
      
      b
    }
  }
  
  // Test with small values (recursive is feasible)
  assert_eq(fibonacci_recursive(5), 5)
  assert_eq(fibonacci_iterative(5), 5)
  
  assert_eq(fibonacci_recursive(10), 55)
  assert_eq(fibonacci_iterative(10), 55)
  
  // Test with larger values (only iterative is feasible)
  assert_eq(fibonacci_iterative(20), 6765)
  assert_eq(fibonacci_iterative(30), 832040)
  
  // Factorial calculation - recursive vs iterative
  let factorial_recursive = fn(n : Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial_recursive(n - 1)
    }
  }
  
  let factorial_iterative = fn(n : Int) -> Int {
    let mut result = 1
    for i in 2..=n {
      result = result * i
    }
    result
  }
  
  // Test factorial calculations
  assert_eq(factorial_recursive(5), 120)
  assert_eq(factorial_iterative(5), 120)
  
  assert_eq(factorial_recursive(10), 3628800)
  assert_eq(factorial_iterative(10), 3628800)
  
  // Test with larger values
  assert_eq(factorial_iterative(12), 479001600)
}

test "memory usage optimization tests" {
  // Test memory-efficient algorithms
  let reverse_array_in_place = fn(arr : Array[Int]) -> Array[Int] {
    let mut result = arr.copy()
    let mut left = 0
    let mut right = result.length() - 1
    
    while left < right {
      let temp = result[left]
      result[left] = result[right]
      result[right] = temp
      left = left + 1
      right = right - 1
    }
    
    result
  }
  
  let reverse_array_with_new_array = fn(arr : Array[Int]) -> Array[Int] {
    let mut result = []
    for i in (0..arr.length()).rev() {
      result = result.push(arr[i])
    }
    result
  }
  
  let test_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let in_place_result = reverse_array_in_place(test_array)
  let new_array_result = reverse_array_with_new_array(test_array)
  
  // Both should produce the same result
  assert_eq(in_place_result.length(), 10)
  assert_eq(new_array_result.length(), 10)
  
  // Check if reversed correctly
  assert_eq(in_place_result[0], 10)
  assert_eq(in_place_result[9], 1)
  assert_eq(new_array_result[0], 10)
  assert_eq(new_array_result[9], 1)
  
  // Test with larger array
  let large_array = []
  for i in 0..1000 {
    large_array = large_array.push(i)
  }
  
  let large_in_place_result = reverse_array_in_place(large_array)
  assert_eq(large_in_place_result[0], 999)
  assert_eq(large_in_place_result[999], 0)
}

test "caching and memoization performance tests" {
  // Fibonacci with memoization
  let fibonacci_memoized = fn() -> (Int -> Int) {
    let mut cache = []
    
    // Initialize cache with -1 values
    for i in 0..50 {
      cache = cache.push(-1)
    }
    
    let fib = fn(n : Int) -> Int {
      if cache[n] != -1 {
        return cache[n]
      }
      
      let result = if n <= 1 {
        n
      } else {
        fib(n - 1) + fib(n - 2)
      }
      
      cache[n] = result
      result
    }
    
    fib
  }
  
  let memoized_fib = fibonacci_memoized()
  
  // Test memoized version
  assert_eq(memoized_fib(10), 55)
  assert_eq(memoized_fib(20), 6765)
  assert_eq(memoized_fib(30), 832040)
  
  // Test cache effectiveness
  // Second call should be faster (conceptual test)
  assert_eq(memoized_fib(30), 832040)
  
  // Simple caching for expensive calculations
  let expensive_calculation = fn(x : Int) -> Int {
    // Simulate expensive calculation
    let mut result = 1
    for i in 1..=x {
      result = result * i
    }
    result
  }
  
  let cached_calculation = fn() -> (Int -> Int) {
    let mut cache = []
    
    let calc = fn(x : Int) -> Int {
      // Check if result is cached
      let mut cached = false
      let mut result = 0
      
      for i in 0..cache.length() {
        match cache[i] {
          (key, value) => {
            if key == x {
              result = value
              cached = true
              break
            }
          }
        }
      }
      
      if not(cached) {
        result = expensive_calculation(x)
        cache = cache.push((x, result))
      }
      
      result
    }
    
    calc
  }
  
  let cached_calc = cached_calculation()
  
  // First calculation
  assert_eq(cached_calc(5), 120)
  // Second calculation (should use cache)
  assert_eq(cached_calc(5), 120)
}

test "algorithm optimization techniques" {
  // Test string searching optimization
  let naive_string_search = fn(text : String, pattern : String) -> Option[Int] {
    let text_chars = text.to_char_array()
    let pattern_chars = pattern.to_char_array()
    
    for i in 0..(text_chars.length() - pattern_chars.length() + 1) {
      let mut match_found = true
      
      for j in 0..pattern_chars.length() {
        if text_chars[i + j] != pattern_chars[j] {
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
  let text = "This is a test string for searching patterns"
  let pattern = "search"
  
  let naive_result = naive_string_search(text, pattern)
  match naive_result {
    Some(index) => assert_eq(index, 25)
    None => assert_true(false)
  }
  
  // Test with non-existing pattern
  let non_existing_pattern = "notfound"
  let non_existing_result = naive_string_search(text, non_existing_pattern)
  match non_existing_result {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Test array merging optimization
  let merge_sorted_arrays = fn(arr1 : Array[Int], arr2 : Array[Int]) -> Array[Int] {
    let mut result = []
    let mut i = 0
    let mut j = 0
    
    while i < arr1.length() and j < arr2.length() {
      if arr1[i] <= arr2[j] {
        result = result.push(arr1[i])
        i = i + 1
      } else {
        result = result.push(arr2[j])
        j = j + 1
      }
    }
    
    // Add remaining elements
    while i < arr1.length() {
      result = result.push(arr1[i])
      i = i + 1
    }
    
    while j < arr2.length() {
      result = result.push(arr2[j])
      j = j + 1
    }
    
    result
  }
  
  let sorted_array1 = [1, 3, 5, 7, 9]
  let sorted_array2 = [2, 4, 6, 8, 10]
  
  let merged_result = merge_sorted_arrays(sorted_array1, sorted_array2)
  assert_eq(merged_result.length(), 10)
  
  // Verify merged array is sorted
  let mut is_sorted = true
  for i in 1..merged_result.length() {
    if merged_result[i - 1] > merged_result[i] {
      is_sorted = false
      break
    }
  }
  assert_true(is_sorted)
  
  // Verify all elements are present
  for i in 1..=10 {
    assert_eq(merged_result[i - 1], i)
  }
}