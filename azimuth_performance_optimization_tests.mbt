// Performance Optimization Test Suite for Azimuth
// 性能优化测试套件 - 测试各种性能优化技术和算法

// Test 1: 算法复杂度测试
test "algorithm complexity tests" {
  // 线性搜索 vs 二分搜索
  let linear_search = fn(arr, target) {
    for i in 0..arr.length() {
      if arr[i] == target {
        return Some(i)
      }
    }
    None
  }
  
  let binary_search = fn(arr, target) {
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
  
  let sorted_arr = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
  
  // 线性搜索需要遍历更多元素
  assert_eq(linear_search(sorted_arr, 20), Some(19))
  assert_eq(linear_search(sorted_arr, 1), Some(0))
  assert_eq(linear_search(sorted_arr, 21), None)
  
  // 二分搜索效率更高
  assert_eq(binary_search(sorted_arr, 20), Some(19))
  assert_eq(binary_search(sorted_arr, 1), Some(0))
  assert_eq(binary_search(sorted_arr, 21), None)
  
  // 冒泡排序 vs 快速排序
  let bubble_sort = fn(arr) {
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
  
  let quick_sort = fn(arr) {
    if arr.length() <= 1 {
      arr
    } else {
      let pivot = arr[0]
      let less = arr.slice(1).filter(fn(x) { x <= pivot })
      let greater = arr.slice(1).filter(fn(x) { x > pivot })
      quick_sort(less) + [pivot] + quick_sort(greater)
    }
  }
  
  let unsorted = [5, 2, 8, 1, 9, 3, 7, 4, 6]
  let bubble_result = bubble_sort(unsorted)
  let quick_result = quick_sort(unsorted)
  
  assert_eq(bubble_result, [1, 2, 3, 4, 5, 6, 7, 8, 9])
  assert_eq(quick_result, [1, 2, 3, 4, 5, 6, 7, 8, 9])
}

// Test 2: 缓存优化测试
test "caching optimization tests" {
  // 斐波那契数列 - 带缓存和不带缓存
  let fib_without_cache = fn(n) {
    if n <= 1 {
      n
    } else {
      fib_without_cache(n - 1) + fib_without_cache(n - 2)
    }
  }
  
  let fib_with_cache = fn() {
    let cache = {}
    
    let fib = fn(n) {
      if cache.contains(n) {
        return cache.get(n).unwrap()
      }
      
      let result = if n <= 1 {
        n
      } else {
        fib(n - 1) + fib(n - 2)
      }
      
      cache.set(n, result)
      result
    }
    
    fib
  }
  
  let cached_fib = fib_with_cache()
  
  // 小数字测试
  assert_eq(fib_without_cache(5), 5)
  assert_eq(cached_fib(5), 5)
  
  assert_eq(fib_without_cache(10), 55)
  assert_eq(cached_fib(10), 55)
  
  // 缓存版本对于大数字效率更高
  assert_eq(cached_fib(20), 6765)
  
  // 记忆化模式
  let memoize = fn(f) {
    let cache = {}
    
    fn(x) {
      if cache.contains(x) {
        return cache.get(x).unwrap()
      }
      
      let result = f(x)
      cache.set(x, result)
      result
    }
  }
  
  let expensive_operation = fn(n) {
    // 模拟昂贵操作
    let mut sum = 0
    for i in 0..n {
      sum = sum + i
    }
    sum
  }
  
  let memoized_operation = memoize(expensive_operation)
  
  assert_eq(memoized_operation(100), 5050)  // 第一次计算
  assert_eq(memoized_operation(100), 5050)  // 从缓存获取
  assert_eq(memoized_operation(200), 20100) // 新值计算
}

// Test 3: 惰性求值测试
test "lazy evaluation tests" {
  // 惰性序列
  let lazy_range = fn(start, step) {
    let current = ref start
    
    fn() {
      let value = !current
      current := !current + step
      value
    }
  }
  
  let next = lazy_range(1, 2)
  assert_eq(next(), 1)
  assert_eq(next(), 3)
  assert_eq(next(), 5)
  assert_eq(next(), 7)
  
  // 惰性过滤器
  let lazy_filter = fn(iterator, predicate) {
    fn() {
      let value = iterator()
      if predicate(value) {
        value
      } else {
        // 递归直到找到满足条件的值
        lazy_filter(iterator, predicate)()
      }
    }
  }
  
  let numbers = lazy_range(1, 1)
  let even_numbers = lazy_filter(numbers, fn(x) { x % 2 == 0 })
  
  assert_eq(even_numbers(), 2)
  assert_eq(even_numbers(), 4)
  assert_eq(even_numbers(), 6)
  
  // 惰性映射
  let lazy_map = fn(iterator, transform) {
    fn() {
      let value = iterator()
      transform(value)
    }
  }
  
  let squares = lazy_map(numbers, fn(x) { x * x })
  assert_eq(squares(), 9)  // 3*3 (next()已被调用3次)
  assert_eq(squares(), 16) // 4*4
}

// Test 4: 批处理优化测试
test "batch processing optimization tests" {
  // 批量处理 vs 单个处理
  let batch_insert = fn(records) {
    // 模拟批量插入，一次操作处理所有记录
    let mut result = []
    for record in records {
      result.push("inserted: " + record)
    }
    result
  }
  
  let individual_insert = fn(records) {
    // 模拟单个插入，每条记录一次操作
    let mut result = []
    for record in records {
      result.push("inserted: " + record)
    }
    result
  }
  
  let records = ["record1", "record2", "record3", "record4", "record5"]
  let batch_result = batch_insert(records)
  let individual_result = individual_insert(records)
  
  assert_eq(batch_result, individual_result)
  
  // 批量大小优化
  let optimal_batch_size = fn(total_records, max_batch_size) {
    // 计算最优批量大小
    if total_records <= max_batch_size {
      total_records
    } else {
      // 简单策略：使用最大批量大小
      max_batch_size
    }
  }
  
  assert_eq(optimal_batch_size(100, 50), 50)
  assert_eq(optimal_batch_size(30, 50), 30)
  
  // 分批处理
  let process_in_batches = fn(records, batch_size, processor) {
    let mut result = []
    let mut i = 0
    
    while i < records.length() {
      let end = if i + batch_size > records.length() {
        records.length()
      } else {
        i + batch_size
      }
      
      let batch = records.slice(i, end)
      result = result + processor(batch)
      i = i + batch_size
    }
    
    result
  }
  
  let batched_result = process_in_batches(records, 2, batch_insert)
  assert_eq(batched_result.length(), 5)
}

// Test 5: 内存池优化测试
test "memory pool optimization tests" {
  // 对象池模式
  let create_object_pool = fn(create_fn, reset_fn, initial_size) {
    let pool = ref []
    
    // 初始化池
    for i in 0..initial_size {
      let obj = create_fn()
      pool.push(obj)
    }
    
    fn() {
      // 获取对象
      if (!pool).is_empty() {
        (!pool).pop()
      } else {
        create_fn()
      }
    },
    
    fn(obj) {
      // 归还对象
      reset_fn(obj)
      pool.push(obj)
    }
  }
  
  // 示例：使用对象池管理缓冲区
  let create_buffer = fn() { {data: [], size: 0} }
  let reset_buffer = fn(buf) { buf.data = []; buf.size = 0 }
  
  let (get_buffer, return_buffer) = create_object_pool(create_buffer, reset_buffer, 2)
  
  // 获取缓冲区
  let buf1 = get_buffer()
  buf1.data.push("item1")
  buf1.size = 1
  
  let buf2 = get_buffer()
  buf2.data.push("item2")
  buf2.size = 1
  
  // 归还缓冲区
  return_buffer(buf1)
  assert_eq(buf1.size, 0)
  assert_eq(buf1.data.length(), 0)
  
  return_buffer(buf2)
  assert_eq(buf2.size, 0)
  assert_eq(buf2.data.length(), 0)
  
  // 再次获取，应该重用之前归还的对象
  let buf3 = get_buffer()
  assert_eq(buf3.size, 0)
}

// Test 6: 字符串优化测试
test "string optimization tests" {
  // 字符串连接优化
  let inefficient_concat = fn(strings) {
    let mut result = ""
    for s in strings {
      result = result + s
    }
    result
  }
  
  let efficient_concat = fn(strings) {
    strings.reduce(fn(acc, s) { acc + s }, "")
  }
  
  let strings = ["hello", " ", "world", "!", " ", "this", " ", "is", " ", "a", " ", "test"]
  
  let inefficient_result = inefficient_concat(strings)
  let efficient_result = efficient_concat(strings)
  
  assert_eq(inefficient_result, "hello world! this is a test")
  assert_eq(efficient_result, "hello world! this is a test")
  
  // 字符串构建器模式
  let string_builder = fn() {
    let parts = ref []
    
    fn() {
      // 添加部分
      fn(part) {
        parts.push(part)
      }
    },
    
    fn() {
      // 构建最终字符串
      (!parts).reduce(fn(acc, part) { acc + part }, "")
    }
  }
  
  let (add_part, build) = string_builder()
  add_part("optimized")
  add_part(" ")
  add_part("string")
  add_part(" ")
  add_part("building")
  
  let built_string = build()
  assert_eq(built_string, "optimized string building")
}

// Test 7: 循环优化测试
test "loop optimization tests" {
  // 循环展开
  let unrolled_sum = fn(arr) {
    let mut sum = 0
    let mut i = 0
    let n = arr.length()
    
    // 每次处理4个元素
    while i + 3 < n {
      sum = sum + arr[i] + arr[i + 1] + arr[i + 2] + arr[i + 3]
      i = i + 4
    }
    
    // 处理剩余元素
    while i < n {
      sum = sum + arr[i]
      i = i + 1
    }
    
    sum
  }
  
  let regular_sum = fn(arr) {
    let mut sum = 0
    for item in arr {
      sum = sum + item
    }
    sum
  }
  
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  assert_eq(regular_sum(numbers), 55)
  assert_eq(unrolled_sum(numbers), 55)
  
  // 循环融合
  let fused_map_filter = fn(arr, map_fn, filter_fn) {
    let mut result = []
    for item in arr {
      let mapped = map_fn(item)
      if filter_fn(mapped) {
        result.push(mapped)
      }
    }
    result
  }
  
  let separate_map_filter = fn(arr, map_fn, filter_fn) {
    let mapped = arr.map(map_fn)
    mapped.filter(filter_fn)
  }
  
  let doubled_evens = fused_map_filter(
    numbers,
    fn(x) { x * 2 },
    fn(x) { x % 4 == 0 }
  )
  
  assert_eq(doubled_evens, [4, 8, 12, 16, 20])
}

// Test 8: 数据结构优化测试
test "data structure optimization tests" {
  // 使用适当的数据结构
  // 数组 vs 哈希表查找
  
  let array_contains = fn(arr, value) {
    for item in arr {
      if item == value {
        return true
      }
    }
    false
  }
  
  let set_contains = fn(set, value) {
    set.contains(value)
  }
  
  let large_array = []
  for i in 1..=1000 {
    large_array.push(i)
  }
  
  let large_set = large_array.to_set()
  
  // 数组查找需要遍历
  assert_true(array_contains(large_array, 500))
  assert_false(array_contains(large_array, 1500))
  
  // 集合查找效率更高
  assert_true(set_contains(large_set, 500))
  assert_false(set_contains(large_set, 1500))
  
  // 预分配容量
  let preallocated_array = fn(size) {
    let mut arr = []
    arr.reserve(size)
    arr
  }
  
  let dynamic_array = fn() { [] }
  
  let prealloc = preallocated_array(1000)
  let dynamic = dynamic_array()
  
  // 填充数组
  for i in 1..=1000 {
    prealloc.push(i)
    dynamic.push(i)
  }
  
  assert_eq(prealloc.length(), 1000)
  assert_eq(dynamic.length(), 1000)
}

// Test 9: 算法选择优化测试
test "algorithm selection optimization tests" {
  // 根据数据规模选择算法
  let smart_sort = fn(arr) {
    if arr.length() <= 10 {
      // 小数组使用插入排序
      let mut result = arr.clone()
      for i in 1..result.length() {
        let key = result[i]
        let mut j = i - 1
        
        while j >= 0 && result[j] > key {
          result[j + 1] = result[j]
          j = j - 1
        }
        result[j + 1] = key
      }
      result
    } else {
      // 大数组使用快速排序
      if arr.length() <= 1 {
        arr
      } else {
        let pivot = arr[0]
        let less = arr.slice(1).filter(fn(x) { x <= pivot })
        let greater = arr.slice(1).filter(fn(x) { x > pivot })
        smart_sort(less) + [pivot] + smart_sort(greater)
      }
    }
  }
  
  let small_array = [5, 2, 8, 1, 3]
  let large_array = [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 11, 12, 13, 14, 15]
  
  let sorted_small = smart_sort(small_array)
  let sorted_large = smart_sort(large_array)
  
  assert_eq(sorted_small, [1, 2, 3, 5, 8])
  assert_eq(sorted_large, [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15])
}

// Test 10: 并行处理优化测试
test "parallel processing optimization tests" {
  // 模拟并行处理
  let parallel_map = fn(arr, transform_fn, num_workers) {
    let chunk_size = (arr.length() + num_workers - 1) / num_workers
    let mut results = []
    
    for i in 0..num_workers {
      let start = i * chunk_size
      let end = if start + chunk_size > arr.length() {
        arr.length()
      } else {
        start + chunk_size
      }
      
      if start < arr.length() {
        let chunk = arr.slice(start, end)
        let chunk_result = chunk.map(transform_fn)
        results = results + chunk_result
      }
    }
    
    results
  }
  
  let sequential_map = fn(arr, transform_fn) {
    arr.map(transform_fn)
  }
  
  let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  let square = fn(x) { x * x }
  
  let sequential_result = sequential_map(numbers, square)
  let parallel_result = parallel_map(numbers, square, 4)
  
  // 结果应该相同，但并行处理可能更快
  assert_eq(sequential_result.length(), parallel_result.length())
  
  // 验证结果内容
  for i in 0..sequential_result.length() {
    assert_true(parallel_result.contains(sequential_result[i]))
  }
}