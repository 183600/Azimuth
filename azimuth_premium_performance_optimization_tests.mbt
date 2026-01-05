// Azimuth Premium Test Suite - Performance Optimization and Benchmarks
// This file contains comprehensive test cases for performance optimization, algorithms, and benchmarking

// Test 1: Algorithm Complexity and Big O Analysis
test "algorithm complexity and big O analysis" {
  // Test O(1) - Constant time operations
  let constant_time_operations = |data_size| {
    let array = [1, 2, 3, 4, 5]
    let hash_map = { "key1": "value1", "key2": "value2", "key3": "value3" }
    
    // Array index access is O(1)
    let array_access = array[2]
    
    // Hash map lookup is O(1) on average
    let map_lookup = hash_map["key2"]
    
    // Stack push/pop is O(1)
    let mut stack = []
    stack.push(42)
    let popped = stack.pop()
    
    (array_access, map_lookup, popped)
  }
  
  // Test with different data sizes - should be consistent
  let result1 = constant_time_operations(100)
  let result2 = constant_time_operations(1000)
  let result3 = constant_time_operations(10000)
  
  assert_eq(result1, (3, "value2", Some(42)))
  assert_eq(result2, (3, "value2", Some(42)))
  assert_eq(result3, (3, "value2", Some(42)))
  
  // Test O(log n) - Logarithmic time operations
  let binary_search = |sorted_array, target| {
    let mut left = 0
    let mut right = sorted_array.length() - 1
    let mut comparisons = 0
    
    while left <= right {
      comparisons = comparisons + 1
      let mid = (left + right) / 2
      
      if sorted_array[mid] == target {
        return (true, comparisons)
      } else if sorted_array[mid] < target {
        left = mid + 1
      } else {
        right = mid - 1
      }
    }
    
    (false, comparisons)
  }
  
  // Create sorted arrays of different sizes
  let array100 = []
  for i = 0; i < 100; i = i + 1 {
    array100.push(i)
  }
  
  let array1000 = []
  for i = 0; i < 1000; i = i + 1 {
    array1000.push(i)
  }
  
  let array10000 = []
  for i = 0; i < 10000; i = i + 1 {
    array10000.push(i)
  }
  
  let (found1, comps1) = binary_search(array100, 50)
  let (found2, comps2) = binary_search(array1000, 500)
  let (found3, comps3) = binary_search(array10000, 5000)
  
  assert_true(found1)
  assert_true(found2)
  assert_true(found3)
  
  // Comparisons should grow logarithmically
  assert_true(comps1 < 10)    // log2(100) ≈ 6.6
  assert_true(comps2 < 15)   // log2(1000) ≈ 9.97
  assert_true(comps3 < 20)   // log2(10000) ≈ 13.3
  
  // Test O(n) - Linear time operations
  let linear_search = |array, target| {
    let mut comparisons = 0
    
    for element in array {
      comparisons = comparisons + 1
      if element == target {
        return (true, comparisons)
      }
    }
    
    (false, comparisons)
  }
  
  let (found_l1, comps_l1) = linear_search(array100, 99)    // Worst case
  let (found_l2, comps_l2) = linear_search(array1000, 999)  // Worst case
  let (found_l3, comps_l3) = linear_search(array10000, 9999) // Worst case
  
  assert_true(found_l1)
  assert_true(found_l2)
  assert_true(found_l3)
  
  // Comparisons should grow linearly
  assert_eq(comps_l1, 100)
  assert_eq(comps_l2, 1000)
  assert_eq(comps_l3, 10000)
  
  // Test O(n log n) - Linearithmic time operations
  let merge_sort = |array| {
    if array.length() <= 1 {
      return (array, 0)
    }
    
    let mid = array.length() / 2
    let (left, left_comps) = merge_sort(array.slice(0, mid))
    let (right, right_comps) = merge_sort(array.slice(mid, array.length()))
    
    // Merge step
    let mut merged = []
    let mut i = 0
    let mut j = 0
    let mut merge_comps = 0
    
    while i < left.length() && j < right.length() {
      merge_comps = merge_comps + 1
      if left[i] <= right[j] {
        merged.push(left[i])
        i = i + 1
      } else {
        merged.push(right[j])
        j = j + 1
      }
    }
    
    // Add remaining elements
    while i < left.length() {
      merged.push(left[i])
      i = i + 1
    }
    
    while j < right.length() {
      merged.push(right[j])
      j = j + 1
    }
    
    (merged, left_comps + right_comps + merge_comps)
  }
  
  let unsorted100 = [100, 1, 99, 2, 98, 3, 97, 4, 96, 5].repeat(10)
  let unsorted1000 = [1000, 1, 999, 2, 998, 3, 997, 4, 996, 5].repeat(100)
  
  let (_, comps_m1) = merge_sort(unsorted100)
  let (_, comps_m2) = merge_sort(unsorted1000)
  
  // Comparisons should grow approximately as n log n
  let expected_comps_100 = 100 * 6.6  // n * log2(n)
  let expected_comps_1000 = 1000 * 9.97
  
  assert_true(comps_m1 < expected_comps_100 * 2)  // Allow for constant factors
  assert_true(comps_m2 < expected_comps_1000 * 2)
  
  // Test O(n²) - Quadratic time operations
  let bubble_sort = |array| {
    let mut sorted = array.copy()
    let mut comparisons = 0
    let n = sorted.length()
    
    for i = 0; i < n; i = i + 1 {
      for j = 0; j < n - i - 1; j = j + 1 {
        comparisons = comparisons + 1
        if sorted[j] > sorted[j + 1] {
          let temp = sorted[j]
          sorted[j] = sorted[j + 1]
          sorted[j + 1] = temp
        }
      }
    }
    
    (sorted, comparisons)
  }
  
  let unsorted30 = [30, 1, 29, 2, 28, 3, 27, 4, 26, 5].repeat(3)
  let unsorted50 = [50, 1, 49, 2, 48, 3, 47, 4, 46, 5].repeat(5)
  
  let (_, comps_b1) = bubble_sort(unsorted30)
  let (_, comps_b2) = bubble_sort(unsorted50)
  
  // Comparisons should grow approximately as n²
  let expected_comps_30 = 30 * 29 / 2  // n * (n-1) / 2
  let expected_comps_50 = 50 * 49 / 2
  
  assert_eq(comps_b1, expected_comps_30)
  assert_eq(comps_b2, expected_comps_50)
  
  // Ratio should be approximately (50²)/(30²) = 2.78
  let ratio = comps_b2.to_float() / comps_b1.to_float()
  assert_true(ratio > 2.5 && ratio < 3.0)
}

// Test 2: Memory Optimization and Efficiency
test "memory optimization and efficiency" {
  // Test memory-efficient data structures
  let memory_efficient_array = |size| {
    // Using packed representation instead of objects
    let mut packed_array = []
    
    for i = 0; i < size; i = i + 1 {
      packed_array.push(i)
    }
    
    packed_array
  }
  
  let memory_inefficient_array = |size| {
    // Using objects with properties
    let mut object_array = []
    
    for i = 0; i < size; i = i + 1 {
      object_array.push({ value: i, id: "item_" + i.to_string() })
    }
    
    object_array
  }
  
  // Test with different sizes
  let efficient_100 = memory_efficient_array(100)
  let inefficient_100 = memory_inefficient_array(100)
  
  assert_eq(efficient_100.length(), 100)
  assert_eq(inefficient_100.length(), 100)
  
  // Access patterns
  let sum_efficient = efficient_100.reduce(|acc, val| acc + val, 0)
  let sum_inefficient = inefficient_100.reduce(|acc, obj| acc + obj.value, 0)
  
  assert_eq(sum_efficient, 4950)  // Sum of 0..99
  assert_eq(sum_inefficient, 4950)
  
  // Test memory pooling
  let memory_pool = |pool_size| {
    let mut pool = []
    let mut allocated = []
    
    // Initialize pool
    for i = 0; i < pool_size; i = i + 1 {
      pool.push({ id: i, in_use: false })
    }
    
    let allocate = || {
      for i = 0; i < pool.length(); i = i + 1 {
        if !pool[i].in_use {
          pool[i].in_use = true
          allocated.push(i)
          return i
        }
      }
      -1  // Pool exhausted
    }
    
    let deallocate = |index| {
      if index >= 0 && index < pool.length() {
        pool[index].in_use = false
        
        // Remove from allocated list
        let mut new_allocated = []
        for alloc_index in allocated {
          if alloc_index != index {
            new_allocated.push(alloc_index)
          }
        }
        allocated = new_allocated
        true
      } else {
        false
      }
    }
    
    let get_pool_stats = || {
      let in_use_count = pool.reduce(|acc, item| if item.in_use { acc + 1 } else { acc }, 0)
      {
        total_size: pool.length(),
        in_use: in_use_count,
        available: pool.length() - in_use_count
      }
    }
    
    (allocate, deallocate, get_pool_stats)
  }
  
  let (allocate, deallocate, get_stats) = memory_pool(10)
  
  // Test pool allocation
  let alloc1 = allocate()
  let alloc2 = allocate()
  let alloc3 = allocate()
  
  assert_eq(alloc1, 0)
  assert_eq(alloc2, 1)
  assert_eq(alloc3, 2)
  
  let stats = get_stats()
  assert_eq(stats.total_size, 10)
  assert_eq(stats.in_use, 3)
  assert_eq(stats.available, 7)
  
  // Test deallocation
  assert_true(deallocate(2))
  assert_false(deallocate(5))  // Not allocated
  
  let stats2 = get_stats()
  assert_eq(stats2.in_use, 2)
  assert_eq(stats2.available, 8)
  
  // Test memory-efficient string operations
  let string_builder = || {
    let mut parts = []
    
    let append = |text| {
      parts.push(text)
    }
    
    let build = || {
      parts.reduce(|acc, part| acc + part, "")
    }
    
    let clear = || {
      parts = []
    }
    
    (append, build, clear)
  }
  
  let (append, build, clear) = string_builder()
  
  // Test string building
  append("Hello")
  append(", ")
  append("World")
  append("!")
  
  let result = build()
  assert_eq(result, "Hello, World!")
  
  // Test clearing and rebuilding
  clear()
  append("New")
  append(" ")
  append("String")
  
  let result2 = build()
  assert_eq(result2, "New String")
  
  // Test memory-efficient caching with LRU eviction
  let lru_cache = |capacity| {
    let mut cache = {}
    let mut access_order = []
    
    let get = |key| {
      if cache.contains(key) {
        // Move to end (most recently used)
        let mut new_order = []
        for k in access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
        
        cache[key]
      } else {
        None
      }
    }
    
    let put = |key, value| {
      if cache.contains(key) {
        // Update existing
        cache[key] = value
        
        // Move to end
        let mut new_order = []
        for k in access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
      } else {
        // Add new
        if cache.length() >= capacity {
          // Evict least recently used
          let lru_key = access_order[0]
          cache.remove(lru_key)
          
          let mut new_order = []
          for k in access_order {
            if k != lru_key {
              new_order.push(k)
            }
          }
          access_order = new_order
        }
        
        cache[key] = value
        access_order.push(key)
      }
    }
    
    let get_cache_info = || {
      {
        size: cache.length(),
        capacity: capacity,
        keys: access_order
      }
    }
    
    (get, put, get_cache_info)
  }
  
  let (get, put, get_info) = lru_cache(3)
  
  // Test cache operations
  put("key1", "value1")
  put("key2", "value2")
  put("key3", "value3")
  
  let info = get_info()
  assert_eq(info.size, 3)
  assert_eq(info.keys, ["key1", "key2", "key3"])
  
  // Access key1 (should become most recently used)
  assert_eq(get("key1"), Some("value1"))
  
  let info2 = get_info()
  assert_eq(info2.keys, ["key2", "key3", "key1"])
  
  // Add key4 (should evict key2 - least recently used)
  put("key4", "value4")
  
  let info3 = get_info()
  assert_eq(info3.size, 3)
  assert_eq(info3.keys, ["key3", "key1", "key4"])
  assert_eq(get("key2"), None)  // Should be evicted
}

// Test 3: CPU Optimization and Vectorization
test "cpu optimization and vectorization" {
  // Test loop unrolling
  let sum_with_unrolling = |array| {
    let mut sum = 0
    let n = array.length()
    let mut i = 0
    
    // Unroll loop by 4
    while i + 3 < n {
      sum = sum + array[i] + array[i + 1] + array[i + 2] + array[i + 3]
      i = i + 4
    }
    
    // Handle remaining elements
    while i < n {
      sum = sum + array[i]
      i = i + 1
    }
    
    sum
  }
  
  let sum_regular = |array| {
    let mut sum = 0
    for element in array {
      sum = sum + element
    }
    sum
  }
  
  // Test with array that's multiple of 4
  let array_multiple_of_4 = []
  for i = 0; i < 1000; i = i + 1 {
    array_multiple_of_4.push(i)
  }
  
  let sum_unrolled = sum_with_unrolling(array_multiple_of_4)
  let sum_normal = sum_regular(array_multiple_of_4)
  
  assert_eq(sum_unrolled, sum_normal)
  assert_eq(sum_unrolled, 499500)  // Sum of 0..999
  
  // Test with array that's not multiple of 4
  let array_not_multiple = []
  for i = 0; i < 1003; i = i + 1 {
    array_not_multiple.push(i)
  }
  
  let sum_unrolled2 = sum_with_unrolling(array_not_multiple)
  let sum_normal2 = sum_regular(array_not_multiple)
  
  assert_eq(sum_unrolled2, sum_normal2)
  assert_eq(sum_unrolled2, 502503)  // Sum of 0..1002
  
  // Test branch prediction optimization
  let conditional_sum_optimized = |array, threshold| {
    let mut sum_above = 0
    let mut sum_below = 0
    
    // Separate loops to improve branch prediction
    for element in array {
      if element > threshold {
        sum_above = sum_above + element
      }
    }
    
    for element in array {
      if element <= threshold {
        sum_below = sum_below + element
      }
    }
    
    (sum_above, sum_below)
  }
  
  let conditional_sum_regular = |array, threshold| {
    let mut sum_above = 0
    let mut sum_below = 0
    
    for element in array {
      if element > threshold {
        sum_above = sum_above + element
      } else {
        sum_below = sum_below + element
      }
    }
    
    (sum_above, sum_below)
  }
  
  let test_array = []
  for i = 0; i < 1000; i = i + 1 {
    test_array.push(i % 100)  // Values 0..99 repeated
  }
  
  let (above_opt, below_opt) = conditional_sum_optimized(test_array, 50)
  let (above_reg, below_reg) = conditional_sum_regular(test_array, 50)
  
  assert_eq(above_opt, above_reg)
  assert_eq(below_opt, below_reg)
  
  // Test SIMD-style vectorized operations
  let vectorized_add = |array1, array2| {
    let mut result = []
    let n = array1.length()
    
    // Process 4 elements at a time (SIMD-style)
    let mut i = 0
    while i + 3 < n {
      result.push(array1[i] + array2[i])
      result.push(array1[i + 1] + array2[i + 1])
      result.push(array1[i + 2] + array2[i + 2])
      result.push(array1[i + 3] + array2[i + 3])
      i = i + 4
    }
    
    // Handle remaining elements
    while i < n {
      result.push(array1[i] + array2[i])
      i = i + 1
    }
    
    result
  }
  
  let regular_add = |array1, array2| {
    let mut result = []
    for i = 0; i < array1.length(); i = i + 1 {
      result.push(array1[i] + array2[i])
    }
    result
  }
  
  let array1 = []
  let array2 = []
  for i = 0; i < 1000; i = i + 1 {
    array1.push(i)
    array2.push(i * 2)
  }
  
  let result_vectorized = vectorized_add(array1, array2)
  let result_regular = regular_add(array1, array2)
  
  assert_eq(result_vectorized.length(), result_regular.length())
  
  for i = 0; i < result_vectorized.length(); i = i + 1 {
    assert_eq(result_vectorized[i], result_regular[i])
    assert_eq(result_vectorized[i], i + i * 2)  // i + 2i = 3i
  }
  
  // Test cache-friendly matrix multiplication
  let cache_friendly_matrix_multiply = |matrix_a, matrix_b| {
    let rows_a = matrix_a.length()
    let cols_a = matrix_a[0].length()
    let cols_b = matrix_b[0].length()
    
    // Initialize result matrix
    let mut result = []
    for i = 0; i < rows_a; i = i + 1 {
      let row = []
      for j = 0; j < cols_b; j = j + 1 {
        row.push(0)
      }
      result.push(row)
    }
    
    // Cache-friendly block multiplication
    let block_size = 32
    
    for i0 = 0; i0 < rows_a; i0 = i0 + block_size {
      let i_max = if i0 + block_size < rows_a { i0 + block_size } else { rows_a }
      
      for k0 = 0; k0 < cols_a; k0 = k0 + block_size {
        let k_max = if k0 + block_size < cols_a { k0 + block_size } else { cols_a }
        
        for j0 = 0; j0 < cols_b; j0 = j0 + block_size {
          let j_max = if j0 + block_size < cols_b { j0 + block_size } else { cols_b }
          
          // Compute block
          for i = i0; i < i_max; i = i + 1 {
            for k = k0; k < k_max; k = k + 1 {
              let a_ik = matrix_a[i][k]
              for j = j0; j < j_max; j = j + 1 {
                result[i][j] = result[i][j] + a_ik * matrix_b[k][j]
              }
            }
          }
        }
      }
    }
    
    result
  }
  
  let regular_matrix_multiply = |matrix_a, matrix_b| {
    let rows_a = matrix_a.length()
    let cols_a = matrix_a[0].length()
    let cols_b = matrix_b[0].length()
    
    // Initialize result matrix
    let mut result = []
    for i = 0; i < rows_a; i = i + 1 {
      let row = []
      for j = 0; j < cols_b; j = j + 1 {
        row.push(0)
      }
      result.push(row)
    }
    
    // Regular multiplication
    for i = 0; i < rows_a; i = i + 1 {
      for j = 0; j < cols_b; j = j + 1 {
        for k = 0; k < cols_a; k = k + 1 {
          result[i][j] = result[i][j] + matrix_a[i][k] * matrix_b[k][j]
        }
      }
    }
    
    result
  }
  
  // Create test matrices
  let matrix_a = []
  let matrix_b = []
  
  for i = 0; i < 64; i = i + 1 {
    let row_a = []
    let row_b = []
    
    for j = 0; j < 64; j = j + 1 {
      row_a.push(i + j)
      row_b.push(i - j)
    }
    
    matrix_a.push(row_a)
    matrix_b.push(row_b)
  }
  
  let result_cache_friendly = cache_friendly_matrix_multiply(matrix_a, matrix_b)
  let result_regular = regular_matrix_multiply(matrix_a, matrix_b)
  
  // Verify results are the same
  assert_eq(result_cache_friendly.length(), result_regular.length())
  for i = 0; i < result_cache_friendly.length(); i = i + 1 {
    assert_eq(result_cache_friendly[i].length(), result_regular[i].length())
    for j = 0; j < result_cache_friendly[i].length(); j = j + 1 {
      assert_eq(result_cache_friendly[i][j], result_regular[i][j])
    }
  }
}

// Test 4: I/O Optimization and Buffering
test "io optimization and buffering" {
  // Test buffered reading simulation
  let buffered_reader = |data, buffer_size| {
    let mut position = 0
    let mut buffer = []
    let mut buffer_position = 0
    let mut buffer_filled = false
    
    let fill_buffer = || {
      if position >= data.length() {
        buffer_filled = false
        return
      }
      
      buffer = []
      let end_position = if position + buffer_size <= data.length() {
        position + buffer_size
      } else {
        data.length()
      }
      
      for i = position; i < end_position; i = i + 1 {
        buffer.push(data[i])
      }
      
      position = end_position
      buffer_position = 0
      buffer_filled = true
    }
    
    let read_byte = || {
      if !buffer_filled || buffer_position >= buffer.length() {
        fill_buffer()
      }
      
      if buffer_filled && buffer_position < buffer.length() {
        let byte = buffer[buffer_position]
        buffer_position = buffer_position + 1
        Some(byte)
      } else {
        None
      }
    }
    
    let read_bytes = |count| {
      let mut bytes = []
      for i = 0; i < count; i = i + 1 {
        match read_byte() {
          Some(byte) => bytes.push(byte),
          None => break
        }
      }
      bytes
    }
    
    (read_byte, read_bytes)
  }
  
  // Create test data
  let test_data = []
  for i = 0; i < 1000; i = i + 1 {
    test_data.push(i % 256)  // Bytes 0..255 repeated
  }
  
  let (read_byte, read_bytes) = buffered_reader(test_data, 100)
  
  // Test byte reading
  let byte1 = read_byte()
  let byte2 = read_byte()
  let byte3 = read_byte()
  
  assert_eq(byte1, Some(0))
  assert_eq(byte2, Some(1))
  assert_eq(byte3, Some(2))
  
  // Test batch reading
  let bytes_batch = read_bytes(10)
  assert_eq(bytes_batch.length(), 10)
  assert_eq(bytes_batch, [3, 4, 5, 6, 7, 8, 9, 10, 11, 12])
  
  // Test buffered writing simulation
  let buffered_writer = |buffer_size| {
    let mut buffer = []
    let mut written = []
    
    let write_byte = |byte| {
      buffer.push(byte)
      
      if buffer.length() >= buffer_size {
        written = written + buffer
        buffer = []
      }
    }
    
    let write_bytes = |bytes| {
      for byte in bytes {
        write_byte(byte)
      }
    }
    
    let flush = || {
      if buffer.length() > 0 {
        written = written + buffer
        buffer = []
      }
    }
    
    let get_written = || {
      written
    }
    
    (write_byte, write_bytes, flush, get_written)
  }
  
  let (write_byte, write_bytes, flush, get_written) = buffered_writer(50)
  
  // Test byte writing
  for i = 0; i < 30; i = i + 1 {
    write_byte(i)
  }
  
  assert_eq(get_written().length(), 0)  // Should still be in buffer
  
  // Write more to trigger flush
  for i = 30; i < 75; i = i + 1 {
    write_byte(i)
  }
  
  assert_eq(get_written().length(), 50)  // First buffer should be flushed
  
  // Test batch writing
  write_bytes([75, 76, 77, 78, 79])
  
  flush()  // Flush remaining buffer
  
  let final_written = get_written()
  assert_eq(final_written.length(), 80)  // All bytes should be written
  
  for i = 0; i < 80; i = i + 1 {
    assert_eq(final_written[i], i)
  }
  
  // Test memory-mapped file simulation
  let memory_mapped_file = |data| {
    let mut mapped_data = data
    let mut modifications = []
    
    let read = |position| {
      if position >= 0 && position < mapped_data.length() {
        Some(mapped_data[position])
      } else {
        None
      }
    }
    
    let write = |position, value| {
      if position >= 0 && position < mapped_data.length() {
        let old_value = mapped_data[position]
        mapped_data[position] = value
        modifications.push((position, old_value, value))
        true
      } else {
        false
      }
    }
    
    let slice = |start, length| {
      if start >= 0 && start < mapped_data.length() && length > 0 {
        let end = if start + length <= mapped_data.length() {
          start + length
        } else {
          mapped_data.length()
        }
        
        mapped_data.slice(start, end)
      } else {
        []
      }
    }
    
    let get_modifications = || {
      modifications
    }
    
    (read, write, slice, get_modifications)
  }
  
  // Create test data for memory mapping
  let map_data = []
  for i = 0; i < 1000; i = i + 1 {
    map_data.push(i * 2)
  }
  
  let (read, write, slice, get_modifications) = memory_mapped_file(map_data)
  
  // Test read operations
  assert_eq(read(10), Some(20))
  assert_eq(read(500), Some(1000))
  assert_eq(read(999), Some(1998))
  assert_eq(read(1000), None)  // Out of bounds
  
  // Test write operations
  assert_true(write(10, 99))
  assert_false(write(1000, 99))  // Out of bounds
  
  assert_eq(read(10), Some(99))  // Should be updated
  
  // Test slice operations
  let slice1 = slice(10, 5)
  assert_eq(slice1, [99, 22, 24, 26, 28])
  
  let slice2 = slice(995, 10)  // Partial slice at end
  assert_eq(slice2, [1990, 1992, 1994, 1996, 1998])
  
  // Test modifications tracking
  let modifications = get_modifications()
  assert_eq(modifications.length(), 1)
  assert_eq(modifications[0], (10, 20, 99))
}

// Test 5: Network Optimization and Connection Pooling
test "network optimization and connection pooling" {
  // Test connection pool simulation
  let connection_pool = |max_connections| {
    let mut available_connections = []
    let mut active_connections = []
    let mut connection_counter = 0
    
    // Initialize pool
    for i = 0; i < max_connections; i = i + 1 {
      available_connections.push({
        id: "conn_" + i.to_string(),
        created_at: 1640995200000L + i.to_long(),
        last_used: 1640995200000L + i.to_long(),
        in_use: false
      })
    }
    
    let get_connection = || {
      if available_connections.length() > 0 {
        let mut connection = available_connections.shift()
        connection.in_use = true
        connection.last_used = 1640995200000L + 1000  // Current time
        active_connections.push(connection.id)
        Some(connection)
      } else {
        None
      }
    }
    
    let release_connection = |connection_id| {
      let mut index = -1
      for i = 0; i < active_connections.length(); i = i + 1 {
        if active_connections[i] == connection_id {
          index = i
          break
        }
      }
      
      if index >= 0 {
        active_connections.remove(index)
        
        // Find and update connection
        for i = 0; i < available_connections.length(); i = i + 1 {
          if available_connections[i].id == connection_id {
            available_connections[i].in_use = false
            available_connections[i].last_used = 1640995200000L + 1000
            return true
          }
        }
      }
      
      false
    }
    
    let get_pool_stats = || {
      {
        total_connections: max_connections,
        available: available_connections.length(),
        active: active_connections.length()
      }
    }
    
    (get_connection, release_connection, get_pool_stats)
  }
  
  let (get_connection, release_connection, get_stats) = connection_pool(5)
  
  // Test initial state
  let stats = get_stats()
  assert_eq(stats.total_connections, 5)
  assert_eq(stats.available, 5)
  assert_eq(stats.active, 0)
  
  // Test connection acquisition
  let conn1 = get_connection()
  let conn2 = get_connection()
  let conn3 = get_connection()
  
  assert_true(conn1.is_some())
  assert_true(conn2.is_some())
  assert_true(conn3.is_some())
  
  match conn1 {
    Some(conn) => assert_eq(conn.id, "conn_0")
    None => assert_true(false)
  }
  
  let stats2 = get_stats()
  assert_eq(stats2.available, 2)
  assert_eq(stats2.active, 3)
  
  // Test connection release
  assert_true(release_connection("conn_1"))
  assert_false(release_connection("nonexistent"))
  
  let stats3 = get_stats()
  assert_eq(stats3.available, 3)
  assert_eq(stats3.active, 2)
  
  // Test request batching
  let request_batcher = |batch_size, timeout_ms| {
    let mut pending_requests = []
    let mut batches = []
    let mut batch_counter = 0
    
    let add_request = |request| {
      pending_requests.push(request)
      
      if pending_requests.length() >= batch_size {
        let batch = {
          id: "batch_" + batch_counter.to_string(),
          requests: pending_requests,
          created_at: 1640995200000L + batch_counter.to_long()
        }
        
        batches.push(batch)
        pending_requests = []
        batch_counter = batch_counter + 1
        true  // Batch sent
      } else {
        false  // Not enough requests yet
      }
    }
    
    let flush = || {
      if pending_requests.length() > 0 {
        let batch = {
          id: "batch_" + batch_counter.to_string(),
          requests: pending_requests,
          created_at: 1640995200000L + batch_counter.to_long()
        }
        
        batches.push(batch)
        pending_requests = []
        batch_counter = batch_counter + 1
        true
      } else {
        false
      }
    }
    
    let get_batches = || {
      batches
    }
    
    (add_request, flush, get_batches)
  }
  
  let (add_request, flush, get_batches) = request_batcher(3, 100)
  
  // Test request batching
  assert_false(add_request("req1"))  // Not enough yet
  assert_false(add_request("req2"))  // Not enough yet
  assert_true(add_request("req3"))   // Batch should be sent
  
  assert_false(add_request("req4"))  // Not enough yet
  assert_true(add_request("req5"))   // Should be sent with flush
  
  let batches = get_batches()
  assert_eq(batches.length(), 2)
  assert_eq(batches[0].requests.length(), 3)
  assert_eq(batches[1].requests.length(), 2)
  
  // Test compression simulation
  let simple_compression = |data| {
    let mut compressed = []
    let mut i = 0
    
    while i < data.length() {
      let current = data[i]
      let mut count = 1
      
      // Count consecutive identical bytes
      while i + count < data.length() && data[i + count] == current {
        count = count + 1
      }
      
      // Store as (value, count) pair
      compressed.push(current)
      compressed.push(count)
      
      i = i + count
    }
    
    compressed
  }
  
  let simple_decompression = |compressed| {
    let mut decompressed = []
    let mut i = 0
    
    while i < compressed.length() {
      let value = compressed[i]
      let count = compressed[i + 1]
      
      for j = 0; j < count; j = j + 1 {
        decompressed.push(value)
      }
      
      i = i + 2
    }
    
    decompressed
  }
  
  // Test compression with repetitive data
  let repetitive_data = []
  for i = 0; i < 100; i = i + 1 {
    repetitive_data.push(i % 10)  // 0,1,2,3,4,5,6,7,8,9 repeated
  }
  
  let compressed = simple_compression(repetitive_data)
  let decompressed = simple_decompression(compressed)
  
  // Should be smaller due to compression
  assert_true(compressed.length() < repetitive_data.length())
  
  // Should decompress to original
  assert_eq(decompressed.length(), repetitive_data.length())
  for i = 0; i < decompressed.length(); i = i + 1 {
    assert_eq(decompressed[i], repetitive_data[i])
  }
  
  // Test compression with non-repetitive data
  let random_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let compressed_random = simple_compression(random_data)
  let decompressed_random = simple_decompression(compressed_random)
  
  // Might be larger due to compression overhead
  assert_eq(decompressed_random.length(), random_data.length())
  for i = 0; i < decompressed_random.length(); i = i + 1 {
    assert_eq(decompressed_random[i], random_data[i])
  }
}

// Test 6: Database Optimization and Query Performance
test "database optimization and query performance" {
  // Test index lookup simulation
  let create_indexed_table = |data| {
    let mut table = []
    let mut index = {}
    
    // Create table
    for i = 0; i < data.length(); i = i + 1 {
      let record = {
        id: data[i].id,
        name: data[i].name,
        value: data[i].value
      }
      table.push(record)
      
      // Create index on id field
      index[data[i].id] = i
    }
    
    let find_by_id = |id| {
      if index.contains(id) {
        let record_index = index[id]
        Some(table[record_index])
      } else {
        None
      }
    }
    
    let find_by_name_scan = |name| {
      let mut results = []
      for record in table {
        if record.name == name {
          results.push(record)
        }
      }
      results
    }
    
    let get_table_stats = || {
      {
        record_count: table.length(),
        index_size: index.keys().length()
      }
    }
    
    (find_by_id, find_by_name_scan, get_table_stats)
  }
  
  // Create test data
  let test_data = []
  for i = 0; i < 1000; i = i + 1 {
    test_data.push({
      id: "id_" + i.to_string(),
      name: "name_" + (i % 100).to_string(),
      value: i * 10
    })
  }
  
  let (find_by_id, find_by_name_scan, get_stats) = create_indexed_table(test_data)
  
  // Test indexed lookup
  let result1 = find_by_id("id_500")
  match result1 {
    Some(record) => {
      assert_eq(record.id, "id_500")
      assert_eq(record.value, 5000)
    }
    None => assert_true(false)
  }
  
  let result2 = find_by_id("nonexistent")
  assert_eq(result2, None)
  
  // Test scan lookup
  let scan_results = find_by_name_scan("name_42")
  assert_eq(scan_results.length(), 10)  // Should be 10 records with name_42
  
  for record in scan_results {
    assert_eq(record.name, "name_42")
    assert_eq(record.value % 420, 0)  // Value should be multiple of 420
  }
  
  // Test query optimization
  let query_optimizer = || {
    let mut query_plans = []
    
    let select = |table, filters| {
      let mut plan = {
        type: "sequential_scan",
        cost: table.length().to_float(),
        filters: filters
      }
      
      // Check if we can use index
      let mut index_available = false
      for filter in filters {
        if filter.field == "id" {
          index_available = true
          plan.type = "index_lookup"
          plan.cost = 1.0  // Index lookup is O(1)
          break
        }
      }
      
      query_plans.push(plan)
      
      // Execute query
      let mut results = []
      
      if index_available && filters[0].field == "id" {
        // Use index lookup
        for record in table {
          if record.id == filters[0].value {
            results.push(record)
            break
          }
        }
      } else {
        // Sequential scan
        for record in table {
          let mut matches = true
          
          for filter in filters {
            match filter.field {
              "name" => {
                if record.name != filter.value {
                  matches = false
                  break
                }
              }
              "value" => {
                if filter.operator == ">" && record.value <= filter.value {
                  matches = false
                  break
                } else if filter.operator == "<" && record.value >= filter.value {
                  matches = false
                  break
                }
              }
              _ => {}
            }
          }
          
          if matches {
            results.push(record)
          }
        }
      }
      
      results
    }
    
    let get_query_plans = || {
      query_plans
    }
    
    (select, get_query_plans)
  }
  
  let (select, get_query_plans) = query_optimizer()
  
  // Test index-optimized query
  let index_query = select(test_data, [{ field: "id", operator: "=", value: "id_123" }])
  assert_eq(index_query.length(), 1)
  assert_eq(index_query[0].id, "id_123")
  
  // Test sequential scan query
  let scan_query = select(test_data, [{ field: "name", operator: "=", value: "name_45" }])
  assert_eq(scan_query.length(), 10)
  
  let plans = get_query_plans()
  assert_eq(plans.length(), 2)
  assert_eq(plans[0].type, "index_lookup")
  assert_eq(plans[1].type, "sequential_scan")
  
  // Test connection pooling for database
  let db_connection_pool = |max_connections| {
    let mut pool = []
    let mut active = []
    
    // Initialize pool
    for i = 0; i < max_connections; i = i + 1 {
      pool.push({
        id: "db_conn_" + i.to_string(),
        created_at: 1640995200000L + i.to_long(),
        last_used: 1640995200000L + i.to_long(),
        query_count: 0
      })
    }
    
    let get_connection = || {
      if pool.length() > 0 {
        let mut connection = pool.shift()
        connection.last_used = 1640995200000L + 1000
        active.push(connection.id)
        Some(connection)
      } else {
        None
      }
    }
    
    let execute_query = |connection_id, query| {
      // Simulate query execution
      for i = 0; i < active.length(); i = i + 1 {
        if active[i] == connection_id {
          // Find connection in pool and update query count
          for j = 0; j < pool.length(); j = j + 1 {
            if pool[j].id == connection_id {
              pool[j].query_count = pool[j].query_count + 1
              pool[j].last_used = 1640995200000L + 1000
              break
            }
          }
          
          // Return to pool
          active.remove(i)
          pool.push({
            id: connection_id,
            created_at: 1640995200000L,
            last_used: 1640995200000L + 1000,
            query_count: 1
          })
          
          return "Query executed successfully"
        }
      }
      
      "Connection not found"
    }
    
    let get_pool_stats = || {
      let total_queries = pool.reduce(|acc, conn| acc + conn.query_count, 0)
      {
        total_connections: max_connections,
        available: pool.length(),
        active: active.length(),
        total_queries: total_queries
      }
    }
    
    (get_connection, execute_query, get_pool_stats)
  }
  
  let (get_connection, execute_query, get_pool_stats) = db_connection_pool(3)
  
  // Test database connection pool
  let conn1 = get_connection()
  let conn2 = get_connection()
  
  match conn1 {
    Some(conn) => {
      let result = execute_query(conn.id, "SELECT * FROM users")
      assert_eq(result, "Query executed successfully")
    }
    None => assert_true(false)
  }
  
  match conn2 {
    Some(conn) => {
      let result = execute_query(conn.id, "SELECT * FROM products")
      assert_eq(result, "Query executed successfully")
    }
    None => assert_true(false)
  }
  
  let stats = get_pool_stats()
  assert_eq(stats.total_connections, 3)
  assert_eq(stats.available, 2)  // Both connections returned to pool
  assert_eq(stats.active, 0)
  assert_eq(stats.total_queries, 2)
}

// Test 7: Cache Optimization Strategies
test "cache optimization strategies" {
  // Test multi-level cache
  let multi_level_cache = |l1_size, l2_size| {
    let mut l1_cache = {}
    let mut l2_cache = {}
    let mut l1_access_order = []
    let mut l2_access_order = []
    
    let get = |key| {
      // Check L1 cache first
      if l1_cache.contains(key) {
        // Move to end of L1 access order
        let mut new_order = []
        for k in l1_access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        l1_access_order = new_order
        
        return (Some(l1_cache[key]), "L1")
      }
      
      // Check L2 cache
      if l2_cache.contains(key) {
        let value = l2_cache[key]
        
        // Move to end of L2 access order
        let mut new_order = []
        for k in l2_access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        l2_access_order = new_order
        
        // Promote to L1 if space available
        if l1_cache.length() < l1_size {
          l1_cache[key] = value
          l1_access_order.push(key)
        } else {
          // Evict from L1 and promote
          let lru_key = l1_access_order[0]
          l1_cache.remove(lru_key)
          
          let mut new_order = []
          for k in l1_access_order {
            if k != lru_key {
              new_order.push(k)
            }
          }
          new_order.push(key)
          l1_access_order = new_order
          
          l1_cache[key] = value
        }
        
        return (Some(value), "L2")
      }
      
      (None, "Miss")
    }
    
    let put = |key, value| {
      // Add to L1 if space available
      if l1_cache.length() < l1_size {
        l1_cache[key] = value
        l1_access_order.push(key)
      } else {
        // Evict from L1 and move to L2
        let lru_key = l1_access_order[0]
        let evicted_value = l1_cache[lru_key]
        
        l1_cache.remove(lru_key)
        
        let mut new_order = []
        for k in l1_access_order {
          if k != lru_key {
            new_order.push(k)
          }
        }
        l1_access_order = new_order
        
        // Add to L1
        l1_cache[key] = value
        l1_access_order.push(key)
        
        // Add evicted to L2 if space available
        if l2_cache.length() < l2_size {
          l2_cache[lru_key] = evicted_value
          l2_access_order.push(lru_key)
        } else {
          // Evict from L2
          let l2_lru_key = l2_access_order[0]
          l2_cache.remove(l2_lru_key)
          
          let mut new_order = []
          for k in l2_access_order {
            if k != l2_lru_key {
              new_order.push(k)
            }
          }
          new_order.push(lru_key)
          l2_access_order = new_order
          
          l2_cache[lru_key] = evicted_value
        }
      }
    }
    
    let get_cache_stats = || {
      {
        l1_size: l1_cache.length(),
        l2_size: l2_cache.length(),
        l1_capacity: l1_size,
        l2_capacity: l2_size
      }
    }
    
    (get, put, get_cache_stats)
  }
  
  let (get, put, get_stats) = multi_level_cache(3, 5)
  
  // Test cache operations
  put("key1", "value1")
  put("key2", "value2")
  put("key3", "value3")
  
  let (value1, level1) = get("key1")
  assert_eq(value1, Some("value1"))
  assert_eq(level1, "L1")
  
  // Add more items to trigger eviction
  put("key4", "value4")
  put("key5", "value5")
  put("key6", "value6")
  
  let (value2, level2) = get("key1")
  assert_eq(value2, Some("value1"))
  assert_eq(level2, "L2")  // Should be in L2 after eviction
  
  let stats = get_stats()
  assert_eq(stats.l1_size, 3)
  assert_eq(stats.l2_size, 3)
  
  // Test write-through cache
  let write_through_cache = |cache_size| {
    let mut cache = {}
    let mut persistent_storage = {}
    let mut access_order = []
    
    let get = |key| {
      if cache.contains(key) {
        // Move to end of access order
        let mut new_order = []
        for k in access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
        
        Some(cache[key])
      } else if persistent_storage.contains(key) {
        let value = persistent_storage[key]
        
        // Add to cache
        if cache.length() < cache_size {
          cache[key] = value
          access_order.push(key)
        } else {
          // Evict LRU
          let lru_key = access_order[0]
          cache.remove(lru_key)
          
          let mut new_order = []
          for k in access_order {
            if k != lru_key {
              new_order.push(k)
            }
          }
          new_order.push(key)
          access_order = new_order
          
          cache[key] = value
        }
        
        Some(value)
      } else {
        None
      }
    }
    
    let put = |key, value| {
      // Write to both cache and persistent storage
      persistent_storage[key] = value
      
      if cache.contains(key) {
        // Update existing
        cache[key] = value
        
        // Move to end of access order
        let mut new_order = []
        for k in access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
      } else {
        // Add new
        if cache.length() < cache_size {
          cache[key] = value
          access_order.push(key)
        } else {
          // Evict LRU
          let lru_key = access_order[0]
          cache.remove(lru_key)
          
          let mut new_order = []
          for k in access_order {
            if k != lru_key {
              new_order.push(k)
            }
          }
          new_order.push(key)
          access_order = new_order
          
          cache[key] = value
        }
      }
    }
    
    let get_storage_stats = || {
      {
        cache_size: cache.length(),
        storage_size: persistent_storage.length(),
        cache_capacity: cache_size
      }
    }
    
    (get, put, get_storage_stats)
  }
  
  let (get, put, get_storage_stats) = write_through_cache(3)
  
  // Test write-through cache
  put("key1", "value1")
  put("key2", "value2")
  put("key3", "value3")
  
  let value1 = get("key1")
  assert_eq(value1, Some("value1"))
  
  // Add more to trigger eviction
  put("key4", "value4")
  put("key5", "value5")
  
  let value2 = get("key1")  // Should be loaded from storage
  assert_eq(value2, Some("value1"))
  
  let stats = get_storage_stats()
  assert_eq(stats.cache_size, 3)
  assert_eq(stats.storage_size, 5)
  
  // Test cache warming strategies
  let cache_warmer = |cache_size, warm_data| {
    let mut cache = {}
    let mut access_order = []
    
    // Warm cache with initial data
    for (key, value) in warm_data {
      if cache.length() < cache_size {
        cache[key] = value
        access_order.push(key)
      } else {
        // Evict LRU
        let lru_key = access_order[0]
        cache.remove(lru_key)
        
        let mut new_order = []
        for k in access_order {
          if k != lru_key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
        
        cache[key] = value
      }
    }
    
    let get = |key| {
      if cache.contains(key) {
        // Move to end of access order
        let mut new_order = []
        for k in access_order {
          if k != key {
            new_order.push(k)
          }
        }
        new_order.push(key)
        access_order = new_order
        
        Some(cache[key])
      } else {
        None
      }
    }
    
    let warm = |new_data| {
      for (key, value) in new_data {
        if cache.contains(key) {
          // Update existing
          cache[key] = value
          
          // Move to end of access order
          let mut new_order = []
          for k in access_order {
            if k != key {
              new_order.push(k)
            }
          }
          new_order.push(key)
          access_order = new_order
        } else {
          // Add new if space available
          if cache.length() < cache_size {
            cache[key] = value
            access_order.push(key)
          }
          // Otherwise ignore (cache is full)
        }
      }
    }
    
    (get, warm)
  }
  
  let initial_data = [
    ("hot_key1", "value1"),
    ("hot_key2", "value2"),
    ("hot_key3", "value3")
  ]
  
  let (get, warm) = cache_warmer(5, initial_data)
  
  // Test pre-warmed cache
  assert_eq(get("hot_key1"), Some("value1"))
  assert_eq(get("hot_key2"), Some("value2"))
  assert_eq(get("hot_key3"), Some("value3"))
  
  // Warm with additional data
  let additional_data = [
    ("hot_key4", "value4"),
    ("hot_key5", "value5")
  ]
  
  warm(additional_data)
  
  assert_eq(get("hot_key4"), Some("value4"))
  assert_eq(get("hot_key5"), Some("value5"))
}

// Test 8: Parallel Processing and Work Distribution
test "parallel processing and work distribution" {
  // Test work stealing queue
  let work_stealing_queue = |num_workers| {
    let mut queues = []
    let mut stolen_tasks = []
    
    // Initialize queues for each worker
    for i = 0; i < num_workers; i = i + 1 {
      queues.push([])
    }
    
    let add_task = |worker_id, task| {
      if worker_id >= 0 && worker_id < num_workers {
        queues[worker_id].push({
          task: task,
          owner: worker_id,
          id: "task_" + worker_id.to_string() + "_" + queues[worker_id].length().to_string()
        })
        true
      } else {
        false
      }
    }
    
    let get_task = |worker_id| {
      if worker_id >= 0 && worker_id < num_workers && queues[worker_id].length() > 0 {
        let task = queues[worker_id].shift()
        Some(task)
      } else {
        None
      }
    }
    
    let steal_task = |thief_worker_id| {
      // Try to steal from other workers
      for i = 0; i < num_workers; i = i + 1 {
        if i != thief_worker_id && queues[i].length() > 0 {
          let task = queues[i].shift()
          stolen_tasks.push({
            task: task.task,
            from_worker: i,
            to_worker: thief_worker_id
          })
          return Some(task)
        }
      }
      None
    }
    
    let get_queue_stats = || {
      let mut stats = []
      for i = 0; i < num_workers; i = i + 1 {
        stats.push({
          worker_id: i,
          queue_size: queues[i].length()
        })
      }
      stats
    }
    
    let get_stolen_tasks = || {
      stolen_tasks
    }
    
    (add_task, get_task, steal_task, get_queue_stats, get_stolen_tasks)
  }
  
  let (add_task, get_task, steal_task, get_stats, get_stolen) = work_stealing_queue(4)
  
  // Test work distribution
  add_task(0, "task_0_1")
  add_task(0, "task_0_2")
  add_task(1, "task_1_1")
  add_task(2, "task_2_1")
  add_task(2, "task_2_2")
  add_task(2, "task_2_3")
  
  let stats = get_stats()
  assert_eq(stats[0].queue_size, 2)
  assert_eq(stats[1].queue_size, 1)
  assert_eq(stats[2].queue_size, 3)
  assert_eq(stats[3].queue_size, 0)
  
  // Test task retrieval
  let task1 = get_task(0)
  let task2 = get_task(1)
  
  match task1 {
    Some(t) => assert_eq(t.task, "task_0_1")
    None => assert_true(false)
  }
  
  match task2 {
    Some(t) => assert_eq(t.task, "task_1_1")
    None => assert_true(false)
  }
  
  // Test work stealing
  let stolen_task = steal_task(3)  // Worker 3 steals from others
  match stolen_task {
    Some(t) => assert_eq(t.task, "task_2_1")  // Should steal from worker 2
    None => assert_true(false)
  }
  
  let stolen_tasks = get_stolen()
  assert_eq(stolen_tasks.length(), 1)
  assert_eq(stolen_tasks[0].from_worker, 2)
  assert_eq(stolen_tasks[0].to_worker, 3)
  
  // Test parallel map-reduce
  let parallel_map_reduce = |data, map_function, reduce_function, num_workers| {
    let chunk_size = (data.length() + num_workers - 1) / num_workers
    let mut map_results = []
    
    // Map phase (parallel)
    for i = 0; i < num_workers; i = i + 1 {
      let start = i * chunk_size
      let end = if (i + 1) * chunk_size < data.length() {
        (i + 1) * chunk_size
      } else {
        data.length()
      }
      
      if start < data.length() {
        let chunk = data.slice(start, end)
        let mapped_chunk = chunk.map(map_function)
        map_results.push(mapped_chunk)
      }
    }
    
    // Reduce phase
    let mut flattened = []
    for result in map_results {
      flattened = flattened + result
    }
    
    if flattened.length() > 0 {
      flattened.reduce(reduce_function)
    } else {
      None
    }
  }
  
  // Test parallel processing
  let data = []
  for i = 0; i < 1000; i = i + 1 {
    data.push(i)
  }
  
  let sum_result = parallel_map_reduce(
    data,
    |x| x,
    |acc, val| acc + val,
    4
  )
  
  match sum_result {
    Some(sum) => assert_eq(sum, 499500)  // Sum of 0..999
    None => assert_true(false)
  }
  
  let product_result = parallel_map_reduce(
    data.slice(0, 10),  // Use smaller data for product
    |x| x,
    |acc, val| acc * val,
    3
  )
  
  match product_result {
    Some(product) => assert_eq(product, 3628800)  // 10!
    None => assert_true(false)
  }
  
  // Test load balancing
  let load_balancer = |num_workers| {
    let mut worker_loads = []
    
    // Initialize worker loads
    for i = 0; i < num_workers; i = i + 1 {
      worker_loads.push(0)
    }
    
    let assign_task = |task_weight| {
      // Find worker with minimum load
      let mut min_load = worker_loads[0]
      let mut min_worker = 0
      
      for i = 1; i < num_workers; i = i + 1 {
        if worker_loads[i] < min_load {
          min_load = worker_loads[i]
          min_worker = i
        }
      }
      
      worker_loads[min_worker] = worker_loads[min_worker] + task_weight
      min_worker
    }
    
    let get_load_distribution = || {
      let total_load = worker_loads.reduce(|acc, load| acc + load, 0)
      let avg_load = total_load / num_workers
      let max_load = worker_loads.reduce(|acc, load| if load > acc { load } else { acc }, 0)
      let min_load = worker_loads.reduce(|acc, load| if load < acc { load } else { acc }, max_load)
      
      {
        worker_loads: worker_loads,
        total_load: total_load,
        avg_load: avg_load,
        max_load: max_load,
        min_load: min_load,
        imbalance: if avg_load > 0 { (max_load - min_load).to_float() / avg_load.to_float() } else { 0.0 }
      }
    }
    
    (assign_task, get_load_distribution)
  }
  
  let (assign_task, get_distribution) = load_balancer(4)
  
  // Assign tasks with different weights
  let task_weights = [5, 3, 8, 2, 7, 4, 6, 1, 9, 5]
  for weight in task_weights {
    assign_task(weight)
  }
  
  let distribution = get_distribution()
  assert_eq(distribution.total_load, 50)  // Sum of all weights
  assert_eq(distribution.avg_load, 12)   // 50 / 4
  
  // Load should be reasonably balanced
  assert_true(distribution.imbalance < 1.0)
  
  // Test pipeline parallelism
  let pipeline = |stages, input_data| {
    let mut current_data = input_data
    
    for stage in stages {
      current_data = current_data.map(stage)
    }
    
    current_data
  }
  
  // Create processing stages
  let stage1 = |x| x * 2
  let stage2 = |x| x + 1
  let stage3 = |x| x / 3
  
  let pipeline_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let pipeline_result = pipeline([stage1, stage2, stage3], pipeline_data)
  
  // Verify pipeline result: ((x * 2) + 1) / 3
  for i = 0; i < pipeline_result.length(); i = i + 1 {
    let expected = ((pipeline_data[i] * 2) + 1) / 3
    assert_eq(pipeline_result[i], expected)
  }
}

// Test 9: Lazy Loading and On-Demand Computation
test "lazy loading and on-demand computation" {
  // Test lazy evaluation
  let lazy_value = |computation| {
    let mut computed = false
    let mut cached_value = None
    
    let get = || {
      if !computed {
        cached_value = Some(computation())
        computed = true
      }
      cached_value
    }
    
    let is_computed = || {
      computed
    }
    
    let reset = || {
      computed = false
      cached_value = None
    }
    
    (get, is_computed, reset)
  }
  
  // Test lazy computation
  let mut computation_count = 0
  let expensive_computation = || {
    computation_count = computation_count + 1
    42 * 10  // Some expensive operation
  }
  
  let (get, is_computed, reset) = lazy_value(expensive_computation)
  
  assert_false(is_computed())
  assert_eq(computation_count, 0)
  
  let value1 = get()
  assert_eq(value1, Some(420))
  assert_true(is_computed())
  assert_eq(computation_count, 1)
  
  // Should not recompute
  let value2 = get()
  assert_eq(value2, Some(420))
  assert_eq(computation_count, 1)  // Still 1, not recomputed
  
  reset()
  assert_false(is_computed())
  
  let value3 = get()
  assert_eq(value3, Some(420))
  assert_eq(computation_count, 2)  // Recomputed after reset
  
  // Test lazy sequence
  let lazy_sequence = |generator| {
    let mut cache = []
    let mut position = 0
    
    let get = |index| {
      while index >= cache.length() {
        cache.push(generator(position))
        position = position + 1
      }
      cache[index]
    }
    
    let take = |count| {
      let mut result = []
      for i = 0; i < count; i = i + 1 {
        result.push(get(i))
      }
      result
    }
    
    (get, take)
  }
  
  // Generate Fibonacci sequence lazily
  let fibonacci_generator = |n| {
    if n == 0 {
      0
    } else if n == 1 {
      1
    } else {
      get(n - 1) + get(n - 2)  // This would be recursive in a real implementation
    }
  }
  
  // For our test, we'll use a simpler generator
  let simple_generator = |n| n * n
  
  let (get, take) = lazy_sequence(simple_generator)
  
  assert_eq(get(0), 0)
  assert_eq(get(5), 25)
  assert_eq(get(10), 100)
  
  let first_5 = take(5)
  assert_eq(first_5, [0, 1, 4, 9, 16])
  
  // Test memoization
  let memoize = |func| {
    let mut cache = {}
    
    |input| {
      if cache.contains(input) {
        cache[input]
      } else {
        let result = func(input)
        cache[input] = result
        result
      }
    }
  }
  
  // Test with expensive function
  let mut call_count = 0
  let expensive_func = |n| {
    call_count = call_count + 1
    n * n * n  // Cube operation
  }
  
  let memoized_func = memoize(expensive_func)
  
  assert_eq(memoized_func(5), 125)
  assert_eq(call_count, 1)
  
  assert_eq(memoized_func(5), 125)  // Should use cache
  assert_eq(call_count, 1)  // Still 1, not recomputed
  
  assert_eq(memoized_func(10), 1000)
  assert_eq(call_count, 2)
  
  assert_eq(memoized_func(5), 125)  // Should use cache
  assert_eq(call_count, 2)  // Still 2
  
  // Test lazy loading of data
  let lazy_loader = |data_loader| {
    let mut loaded = false
    let mut data = None
    
    let load = || {
      if !loaded {
        data = Some(data_loader())
        loaded = true
      }
      data
    }
    
    let is_loaded = || {
      loaded
    }
    
    let unload = || {
      loaded = false
      data = None
    }
    
    (load, is_loaded, unload)
  }
  
  // Test lazy data loading
  let mut load_count = 0
  let data_loader = || {
    load_count = load_count + 1
    [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  }
  
  let (load, is_loaded, unload) = lazy_loader(data_loader)
  
  assert_false(is_loaded())
  assert_eq(load_count, 0)
  
  let data1 = load()
  match data1 {
    Some(d) => assert_eq(d, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    None => assert_true(false)
  }
  
  assert_true(is_loaded())
  assert_eq(load_count, 1)
  
  // Should not reload
  let data2 = load()
  match data2 {
    Some(d) => assert_eq(d, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    None => assert_true(false)
  }
  
  assert_eq(load_count, 1)  // Still 1, not reloaded
  
  unload()
  assert_false(is_loaded())
  
  let data3 = load()
  match data3 {
    Some(d) => assert_eq(d, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10])
    None => assert_true(false)
  }
  
  assert_eq(load_count, 2)  // Reloaded after unload
}

// Test 10: Performance Profiling and Metrics
test "performance profiling and metrics" {
  // Test performance profiler
  let performance_profiler = || {
    let mut function_calls = {}
    let mut call_stack = []
    let mut total_time = 0
    
    let start_function = |function_name| {
      let start_time = total_time
      call_stack.push({
        name: function_name,
        start_time: start_time
      })
    }
    
    let end_function = |function_name| {
      if call_stack.length() > 0 {
        let frame = call_stack.pop()
        
        if frame.name == function_name {
          let duration = total_time - frame.start_time
          
          if function_calls.contains(function_name) {
            let current = function_calls[function_name]
            function_calls[function_name] = {
              count: current.count + 1,
              total_time: current.total_time + duration,
              avg_time: (current.total_time + duration) / (current.count + 1)
            }
          } else {
            function_calls[function_name] = {
              count: 1,
              total_time: duration,
              avg_time: duration
            }
          }
        }
      }
    }
    
    let advance_time = |time_delta| {
      total_time = total_time + time_delta
    }
    
    let get_profile_data = || {
      function_calls
    }
    
    let reset = || {
      function_calls = {}
      call_stack = []
      total_time = 0
    }
    
    (start_function, end_function, advance_time, get_profile_data, reset)
  }
  
  let (start, end, advance, get_profile, reset) = performance_profiler()
  
  // Simulate function calls with timing
  start("function_a")
  advance(10)
  end("function_a")
  
  start("function_b")
  advance(5)
  start("function_c")
  advance(3)
  end("function_c")
  advance(2)
  end("function_b")
  
  start("function_a")
  advance(8)
  end("function_a")
  
  let profile_data = get_profile()
  
  // Verify profile data
  assert_true(profile_data.contains("function_a"))
  assert_true(profile_data.contains("function_b"))
  assert_true(profile_data.contains("function_c"))
  
  assert_eq(profile_data["function_a"].count, 2)
  assert_eq(profile_data["function_a"].total_time, 18)
  assert_eq(profile_data["function_a"].avg_time, 9)
  
  assert_eq(profile_data["function_b"].count, 1)
  assert_eq(profile_data["function_b"].total_time, 10)
  assert_eq(profile_data["function_b"].avg_time, 10)
  
  assert_eq(profile_data["function_c"].count, 1)
  assert_eq(profile_data["function_c"].total_time, 3)
  assert_eq(profile_data["function_c"].avg_time, 3)
  
  // Test performance metrics collector
  let metrics_collector = || {
    let mut metrics = {}
    let mut timestamps = []
    
    let record_metric = |name, value| {
      let timestamp = 1640995200000L + timestamps.length().to_long()
      timestamps.push(timestamp)
      
      if metrics.contains(name) {
        let current = metrics[name]
        metrics[name] = {
          count: current.count + 1,
          sum: current.sum + value,
          min: if value < current.min { value } else { current.min },
          max: if value > current.max { value } else { current.max },
          avg: (current.sum + value) / (current.count + 1)
        }
      } else {
        metrics[name] = {
          count: 1,
          sum: value,
          min: value,
          max: value,
          avg: value
        }
      }
    }
    
    let get_metrics = || {
      metrics
    }
    
    let reset = || {
      metrics = {}
      timestamps = []
    }
    
    (record_metric, get_metrics, reset)
  }
  
  let (record, get_metrics, reset) = metrics_collector()
  
  // Record various metrics
  record("response_time", 100)
  record("response_time", 150)
  record("response_time", 120)
  record("response_time", 80)
  
  record("memory_usage", 512)
  record("memory_usage", 620)
  record("memory_usage", 480)
  
  record("cpu_usage", 25)
  record("cpu_usage", 30)
  record("cpu_usage", 45)
  record("cpu_usage", 20)
  
  let metrics = get_metrics()
  
  // Verify response time metrics
  assert_eq(metrics["response_time"].count, 4)
  assert_eq(metrics["response_time"].sum, 450)
  assert_eq(metrics["response_time"].min, 80)
  assert_eq(metrics["response_time"].max, 150)
  assert_eq(metrics["response_time"].avg, 112.5)
  
  // Verify memory usage metrics
  assert_eq(metrics["memory_usage"].count, 3)
  assert_eq(metrics["memory_usage"].sum, 1612)
  assert_eq(metrics["memory_usage"].min, 480)
  assert_eq(metrics["memory_usage"].max, 620)
  assert_eq(metrics["memory_usage"].avg, 537.3333333333334)
  
  // Verify CPU usage metrics
  assert_eq(metrics["cpu_usage"].count, 4)
  assert_eq(metrics["cpu_usage"].sum, 120)
  assert_eq(metrics["cpu_usage"].min, 20)
  assert_eq(metrics["cpu_usage"].max, 45)
  assert_eq(metrics["cpu_usage"].avg, 30)
  
  // Test performance benchmark
  let benchmark = |name, operation, iterations| {
    let mut results = []
    
    for i = 0; i < iterations; i = i + 1 {
      let start_time = 1640995200000L + i.to_long()
      let result = operation()
      let end_time = 1640995200000L + i.to_long() + 10  // Simulate 10ms execution
      
      results.push({
        iteration: i,
        start_time: start_time,
        end_time: end_time,
        duration: end_time - start_time,
        result: result
      })
    }
    
    // Calculate statistics
    let total_duration = results.reduce(|acc, r| acc + r.duration, 0)
    let avg_duration = total_duration / iterations.to_long()
    
    let mut min_duration = results[0].duration
    let mut max_duration = results[0].duration
    
    for result in results {
      if result.duration < min_duration {
        min_duration = result.duration
      }
      if result.duration > max_duration {
        max_duration = result.duration
      }
    }
    
    {
      name: name,
      iterations: iterations,
      total_duration: total_duration,
      avg_duration: avg_duration,
      min_duration: min_duration,
      max_duration: max_duration,
      results: results
    }
  }
  
  // Test benchmarking
  let benchmark1 = benchmark(
    "array_sum",
    || {
      let mut sum = 0
      for i = 0; i < 100; i = i + 1 {
        sum = sum + i
      }
      sum
    },
    10
  )
  
  let benchmark2 = benchmark(
    "string_concat",
    || {
      let mut result = ""
      for i = 0; i < 50; i = i + 1 {
        result = result + "a"
      }
      result.length()
    },
    5
  )
  
  // Verify benchmark results
  assert_eq(benchmark1.name, "array_sum")
  assert_eq(benchmark1.iterations, 10)
  assert_eq(benchmark1.avg_duration, 10)
  assert_eq(benchmark1.min_duration, 10)
  assert_eq(benchmark1.max_duration, 10)
  
  assert_eq(benchmark2.name, "string_concat")
  assert_eq(benchmark2.iterations, 5)
  assert_eq(benchmark2.avg_duration, 10)
  assert_eq(benchmark2.min_duration, 10)
  assert_eq(benchmark2.max_duration, 10)
  
  // Test performance regression detection
  let regression_detector = |baseline_threshold| {
    let mut baseline_metrics = {}
    
    let set_baseline = |metric_name, value| {
      baseline_metrics[metric_name] = value
    }
    
    let check_regression = |metric_name, current_value| {
      if baseline_metrics.contains(metric_name) {
        let baseline = baseline_metrics[metric_name]
        let regression_percent = ((current_value - baseline).to_float() / baseline.to_float()) * 100.0
        
        if regression_percent > baseline_threshold {
          (true, regression_percent)
        } else {
          (false, regression_percent)
        }
      } else {
        (false, 0.0)
      }
    }
    
    (set_baseline, check_regression)
  }
  
  let (set_baseline, check_regression) = regression_detector(10.0)  // 10% threshold
  
  // Set baseline values
  set_baseline("response_time", 100)
  set_baseline("memory_usage", 500)
  set_baseline("cpu_usage", 25)
  
  // Check for regressions
  let (regression1, percent1) = check_regression("response_time", 115)
  assert_true(regression1)  // 15% increase
  assert_true(percent1 > 14.0 && percent1 < 16.0)
  
  let (regression2, percent2) = check_regression("response_time", 105)
  assert_false(regression2)  // 5% increase
  assert_true(percent2 > 4.0 && percent2 < 6.0)
  
  let (regression3, percent3) = check_regression("memory_usage", 600)
  assert_true(regression3)  // 20% increase
  assert_true(percent3 > 19.0 && percent3 < 21.0)
  
  let (regression4, percent4) = check_regression("cpu_usage", 27)
  assert_false(regression4)  // 8% increase
  assert_true(percent4 > 7.0 && percent4 < 9.0)
}