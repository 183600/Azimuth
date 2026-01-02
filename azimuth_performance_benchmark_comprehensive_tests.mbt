// Azimuth Performance Benchmark Comprehensive Test Suite
// This file contains performance benchmark tests for various operations

// Test 1: Array Operations Performance
test "array operations performance" {
  let create_large_array = fn(size: Int) -> Array<Int> {
    let mut arr = []
    for i in 0..<size {
      arr = arr.push(i)
    }
    arr
  }
  
  let array_sum = fn(arr: Array<Int>) -> Int {
    let mut sum = 0
    for i in arr {
      sum = sum + i
    }
    sum
  }
  
  let array_filter = fn(arr: Array<Int>) -> Array<Int> {
    arr.filter(fn(x) { x % 2 == 0 })
  }
  
  let array_map = fn(arr: Array<Int>) -> Array<Int> {
    arr.map(fn(x) { x * 2 })
  }
  
  // Small array performance
  let small_arr = create_large_array(100)
  assert_eq(small_arr.length(), 100)
  assert_eq(array_sum(small_arr), 4950)  // Sum of 0..99
  
  let small_filtered = array_filter(small_arr)
  assert_eq(small_filtered.length(), 50)
  
  let small_mapped = array_map(small_arr)
  assert_eq(small_mapped.length(), 100)
  assert_eq(small_mapped[0], 0)
  assert_eq(small_mapped[99], 198)
  
  // Medium array performance
  let medium_arr = create_large_array(1000)
  assert_eq(medium_arr.length(), 1000)
  assert_eq(array_sum(medium_arr), 499500)  // Sum of 0..999
  
  let medium_filtered = array_filter(medium_arr)
  assert_eq(medium_filtered.length(), 500)
  
  let medium_mapped = array_map(medium_arr)
  assert_eq(medium_mapped.length(), 1000)
  assert_eq(medium_mapped[0], 0)
  assert_eq(medium_mapped[999], 1998)
}

// Test 2: String Operations Performance
test "string operations performance" {
  let create_large_string = fn(size: Int) -> String {
    let mut str = ""
    for i in 0..<size {
      str = str + "a"
    }
    str
  }
  
  let string_reverse = fn(s: String) -> String {
    let mut reversed = ""
    for i in 0..<s.length() {
      reversed = reversed + s.substring(s.length() - 1 - i, 1)
    }
    reversed
  }
  
  let string_count_char = fn(s: String, char: String) -> Int {
    let mut count = 0
    for i in 0..<s.length() {
      if s.substring(i, 1) == char {
        count = count + 1
      }
    }
    count
  }
  
  // Small string performance
  let small_str = create_large_string(100)
  assert_eq(small_str.length(), 100)
  
  let small_reversed = string_reverse(small_str)
  assert_eq(small_reversed.length(), 100)
  assert_eq(small_reversed[0], "a")
  
  let small_count = string_count_char(small_str, "a")
  assert_eq(small_count, 100)
  
  // Medium string performance
  let medium_str = create_large_string(1000)
  assert_eq(medium_str.length(), 1000)
  
  let medium_reversed = string_reverse(medium_str)
  assert_eq(medium_reversed.length(), 1000)
  assert_eq(medium_reversed[0], "a")
  
  let medium_count = string_count_char(medium_str, "a")
  assert_eq(medium_count, 1000)
}

// Test 3: Hash Map Operations Performance
test "hash map operations performance" {
  type HashMap = Array<(String, Int)>
  
  let create_hash_map = fn(size: Int) -> HashMap {
    let mut map = []
    for i in 0..<size {
      let key = "key" + i.to_string()
      map = map.push((key, i))
    }
    map
  }
  
  let hash_map_get = fn(map: HashMap, key: String) -> Option<Int> {
    for (k, v) in map {
      if k == key {
        return Some(v)
      }
    }
    None
  }
  
  let hash_map_contains = fn(map: HashMap, key: String) -> Bool {
    for (k, _) in map {
      if k == key {
        return true
      }
    }
    false
  }
  
  let hash_map_keys = fn(map: HashMap) -> Array<String> {
    let mut keys = []
    for (k, _) in map {
      keys = keys.push(k)
    }
    keys
  }
  
  let hash_map_values = fn(map: HashMap) -> Array<Int> {
    let mut values = []
    for (_, v) in map {
      values = values.push(v)
    }
    values
  }
  
  // Small hash map performance
  let small_map = create_hash_map(100)
  assert_eq(small_map.length(), 100)
  
  let small_get = hash_map_get(small_map, "key42")
  match small_get {
    Some(value) => assert_eq(value, 42)
    None => assert_true(false)
  }
  
  assert_true(hash_map_contains(small_map, "key99"))
  assert_false(hash_map_contains(small_map, "key100"))
  
  let small_keys = hash_map_keys(small_map)
  assert_eq(small_keys.length(), 100)
  assert_true(small_keys.contains("key0"))
  assert_true(small_keys.contains("key99"))
  
  let small_values = hash_map_values(small_map)
  assert_eq(small_values.length(), 100)
  assert_true(small_values.contains(0))
  assert_true(small_values.contains(99))
  
  // Medium hash map performance
  let medium_map = create_hash_map(1000)
  assert_eq(medium_map.length(), 1000)
  
  let medium_get = hash_map_get(medium_map, "key500")
  match medium_get {
    Some(value) => assert_eq(value, 500)
    None => assert_true(false)
  }
  
  assert_true(hash_map_contains(medium_map, "key999"))
  assert_false(hash_map_contains(medium_map, "key1000"))
  
  let medium_keys = hash_map_keys(medium_map)
  assert_eq(medium_keys.length(), 1000)
  assert_true(medium_keys.contains("key0"))
  assert_true(medium_keys.contains("key999"))
  
  let medium_values = hash_map_values(medium_map)
  assert_eq(medium_values.length(), 1000)
  assert_true(medium_values.contains(0))
  assert_true(medium_values.contains(999))
}

// Test 4: Recursive Function Performance
test "recursive function performance" {
  let fibonacci = fn(n: Int) -> Int {
    if n <= 1 {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }
  
  let factorial = fn(n: Int) -> Int {
    if n <= 1 {
      1
    } else {
      n * factorial(n - 1)
    }
  }
  
  let tree_depth = fn(depth: Int) -> Int {
    if depth <= 0 {
      1
    } else {
      1 + tree_depth(depth - 1) + tree_depth(depth - 1)
    }
  }
  
  // Small recursive operations
  assert_eq(fibonacci(0), 0)
  assert_eq(fibonacci(1), 1)
  assert_eq(fibonacci(2), 1)
  assert_eq(fibonacci(3), 2)
  assert_eq(fibonacci(5), 5)
  assert_eq(fibonacci(10), 55)
  
  assert_eq(factorial(0), 1)
  assert_eq(factorial(1), 1)
  assert_eq(factorial(2), 2)
  assert_eq(factorial(3), 6)
  assert_eq(factorial(4), 24)
  assert_eq(factorial(5), 120)
  
  assert_eq(tree_depth(0), 1)
  assert_eq(tree_depth(1), 3)
  assert_eq(tree_depth(2), 7)
  assert_eq(tree_depth(3), 15)
  
  // Medium recursive operations (limited to avoid stack overflow)
  assert_eq(fibonacci(15), 610)
  assert_eq(factorial(7), 5040)
  assert_eq(tree_depth(4), 31)
}

// Test 5: Sorting Algorithm Performance
test "sorting algorithm performance" {
  let bubble_sort = fn(arr: Array<Int>) -> Array<Int> {
    let mut sorted = arr
    let n = sorted.length()
    
    for i in 0..<n {
      for j in 0..<(n - i - 1) {
        if sorted[j] > sorted[j + 1] {
          let temp = sorted[j]
          sorted = sorted.with(j, sorted[j + 1])
          sorted = sorted.with(j + 1, temp)
        }
      }
    }
    
    sorted
  }
  
  let quick_sort_helper = fn(arr: Array<Int>, low: Int, high: Int) -> Array<Int> {
    if low < high {
      let partition = fn(a: Array<Int>, l: Int, h: Int) -> (Array<Int>, Int) {
        let pivot = a[h]
        let mut i = l - 1
        let mut result = a
        
        for j in l..<(h) {
          if result[j] <= pivot {
            i = i + 1
            let temp = result[i]
            result = result.with(i, result[j])
            result = result.with(j, temp)
          }
        }
        
        let temp = result[i + 1]
        result = result.with(i + 1, result[h])
        result = result.with(h, temp)
        
        (result, i + 1)
      }
      
      let (new_arr, pi) = partition(sorted, low, high)
      sorted = new_arr
      
      sorted = quick_sort_helper(sorted, low, pi - 1)
      sorted = quick_sort_helper(sorted, pi + 1, high)
    }
    
    sorted
  }
  
  let quick_sort = fn(arr: Array<Int>) -> Array<Int> {
    if arr.length() <= 1 {
      arr
    } else {
      quick_sort_helper(arr, 0, arr.length() - 1)
    }
  }
  
  let create_random_array = fn(size: Int) -> Array<Int> {
    let mut arr = []
    let mut seed = 42
    
    for i in 0..<size {
      seed = (seed * 1103515245 + 12345) % 2147483647
      let value = seed % 1000
      arr = arr.push(value)
    }
    
    arr
  }
  
  let is_sorted = fn(arr: Array<Int>) -> Bool {
    for i in 0..<(arr.length() - 1) {
      if arr[i] > arr[i + 1] {
        return false
      }
    }
    true
  }
  
  // Small array sorting
  let small_arr = create_random_array(10)
  let small_bubble = bubble_sort(small_arr)
  assert_true(is_sorted(small_bubble))
  
  let small_quick = quick_sort(small_arr)
  assert_true(is_sorted(small_quick))
  
  // Medium array sorting
  let medium_arr = create_random_array(50)
  let medium_bubble = bubble_sort(medium_arr)
  assert_true(is_sorted(medium_bubble))
  
  let medium_quick = quick_sort(medium_arr)
  assert_true(is_sorted(medium_quick))
}

// Test 6: Search Algorithm Performance
test "search algorithm performance" {
  let linear_search = fn(arr: Array<Int>, target: Int) -> Option<Int> {
    for i in 0..<arr.length() {
      if arr[i] == target {
        return Some(i)
      }
    }
    None
  }
  
  let binary_search = fn(arr: Array<Int>, target: Int) -> Option<Int> {
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
  
  let create_sorted_array = fn(size: Int) -> Array<Int> {
    let mut arr = []
    for i in 0..<size {
      arr = arr.push(i)
    }
    arr
  }
  
  // Small array search
  let small_arr = create_sorted_array(100)
  
  let small_linear = linear_search(small_arr, 42)
  match small_linear {
    Some(index) => assert_eq(index, 42)
    None => assert_true(false)
  }
  
  let small_binary = binary_search(small_arr, 42)
  match small_binary {
    Some(index) => assert_eq(index, 42)
    None => assert_true(false)
  }
  
  let small_linear_missing = linear_search(small_arr, 150)
  match small_linear_missing {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let small_binary_missing = binary_search(small_arr, 150)
  match small_binary_missing {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // Medium array search
  let medium_arr = create_sorted_array(1000)
  
  let medium_linear = linear_search(medium_arr, 500)
  match medium_linear {
    Some(index) => assert_eq(index, 500)
    None => assert_true(false)
  }
  
  let medium_binary = binary_search(medium_arr, 500)
  match medium_binary {
    Some(index) => assert_eq(index, 500)
    None => assert_true(false)
  }
  
  let medium_linear_missing = linear_search(medium_arr, 1500)
  match medium_linear_missing {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  let medium_binary_missing = binary_search(medium_arr, 1500)
  match medium_binary_missing {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
}

// Test 7: Memory Allocation Performance
test "memory allocation performance" {
  let create_nested_structures = fn(depth: Int, breadth: Int) -> Array<Array<Int>> {
    let mut result = []
    
    for i in 0..<breadth {
      let mut inner = []
      for j in 0..<depth {
        inner = inner.push(i * depth + j)
      }
      result = result.push(inner)
    }
    
    result
  }
  
  let sum_nested_structures = fn(nested: Array<Array<Int>>) -> Int {
    let mut sum = 0
    for inner in nested {
      for value in inner {
        sum = sum + value
      }
    }
    sum
  }
  
  let flatten_nested = fn(nested: Array<Array<Int>>) -> Array<Int> {
    let mut result = []
    for inner in nested {
      for value in inner {
        result = result.push(value)
      }
    }
    result
  }
  
  // Small nested structures
  let small_nested = create_nested_structures(10, 10)
  assert_eq(small_nested.length(), 10)
  assert_eq(small_nested[0].length(), 10)
  
  let small_sum = sum_nested_structures(small_nested)
  assert_eq(small_sum, 4950)  // Sum of 0..99
  
  let small_flattened = flatten_nested(small_nested)
  assert_eq(small_flattened.length(), 100)
  assert_eq(small_flattened[0], 0)
  assert_eq(small_flattened[99], 99)
  
  // Medium nested structures
  let medium_nested = create_nested_structures(20, 20)
  assert_eq(medium_nested.length(), 20)
  assert_eq(medium_nested[0].length(), 20)
  
  let medium_sum = sum_nested_structures(medium_nested)
  assert_eq(medium_sum, 39900)  // Sum of 0..399
  
  let medium_flattened = flatten_nested(medium_nested)
  assert_eq(medium_flattened.length(), 400)
  assert_eq(medium_flattened[0], 0)
  assert_eq(medium_flattened[399], 399)
}

// Test 8: Function Call Overhead Performance
test "function call overhead performance" {
  let simple_function = fn(x: Int) -> Int {
    x + 1
  }
  
  let complex_function = fn(x: Int) -> Int {
    let mut result = x
    for i in 0..<10 {
      result = result + i
    }
    result
  }
  
  let higher_order_function = fn(f: (Int) -> Int, x: Int) -> Int {
    f(f(x))
  }
  
  let recursive_function = fn(x: Int) -> Int {
    if x <= 0 {
      0
    } else {
      1 + recursive_function(x - 1)
    }
  }
  
  // Simple function calls
  let mut simple_sum = 0
  for i in 0..<1000 {
    simple_sum = simple_sum + simple_function(i)
  }
  assert_eq(simple_sum, 500500)  // Sum of 1..1000
  
  // Complex function calls
  let mut complex_sum = 0
  for i in 0..<100 {
    complex_sum = complex_sum + complex_function(i)
  }
  assert_eq(complex_sum, 5450)  // Each call adds 0+1+...+9 = 45, so sum is 100*45 + sum(0..99) = 4500 + 4950
  
  // Higher-order function calls
  let mut higher_order_sum = 0
  for i in 0..<100 {
    higher_order_sum = higher_order_sum + higher_order_function(simple_function, i)
  }
  assert_eq(higher_order_sum, 5150)  // Each call adds (i+1)+1 = i+2, so sum is sum(2..101) = 5150
  
  // Recursive function calls
  let recursive_sum = recursive_function(100)
  assert_eq(recursive_sum, 100)
}