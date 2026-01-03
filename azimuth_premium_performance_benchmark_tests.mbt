// Azimuth Performance Benchmark Tests
// This file contains test cases for performance benchmarking

// Test 1: Algorithm Performance Comparison
test "算法性能比较" {
  // 定义排序算法
  let bubble_sort = fn(arr: Array[Int]) {
    let result = arr.copy()
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
  
  let quick_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      return arr
    }
    
    let pivot = arr[0]
    let less = arr.slice(1, arr.length()).filter(fn(x) { x <= pivot })
    let greater = arr.slice(1, arr.length()).filter(fn(x) { x > pivot })
    
    quick_sort(less) + [pivot] + quick_sort(greater)
  }
  
  let merge_sort = fn(arr: Array[Int]) {
    if arr.length() <= 1 {
      return arr
    }
    
    let mid = arr.length() / 2
    let left = merge_sort(arr.slice(0, mid))
    let right = merge_sort(arr.slice(mid, arr.length()))
    
    let merge = fn(left: Array[Int], right: Array[Int]) {
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
      
      // 添加剩余元素
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
  
  // 性能测试函数
  let benchmark_sort = fn(sort_fn: (Array[Int]) -> Array[Int>, data: Array[Int], iterations: Int) {
    let start_time = Time::now()
    
    for i in 0..iterations {
      let _ = sort_fn(data)
    }
    
    let end_time = Time::now()
    end_time - start_time
  }
  
  // 创建测试数据
  let small_data = [5, 2, 8, 1, 9, 3]
  let medium_data = []
  for i in 0..100 {
    medium_data = medium_data.push((100 - i) % 50)  // 0-49的重复值
  }
  
  // 测试小数据集
  let bubble_time_small = benchmark_sort(bubble_sort, small_data, 100)
  let quick_time_small = benchmark_sort(quick_sort, small_data, 100)
  let merge_time_small = benchmark_sort(merge_sort, small_data, 100)
  
  // 验证排序结果正确性
  let bubble_result = bubble_sort(small_data)
  let quick_result = quick_sort(small_data)
  let merge_result = merge_sort(small_data)
  
  assert_eq(bubble_result, [1, 2, 3, 5, 8, 9])
  assert_eq(quick_result, [1, 2, 3, 5, 8, 9])
  assert_eq(merge_result, [1, 2, 3, 5, 8, 9])
  
  // 测试中等数据集
  let bubble_time_medium = benchmark_sort(bubble_sort, medium_data, 10)
  let quick_time_medium = benchmark_sort(quick_sort, medium_data, 10)
  let merge_time_medium = benchmark_sort(merge_sort, medium_data, 10)
  
  // 验证性能差异
  // 对于小数据集，性能差异可能不明显
  // 对于中等数据集，快速排序和归并排序应该明显快于冒泡排序
  assert_true(bubble_time_medium > quick_time_medium)
  assert_true(bubble_time_medium > merge_time_medium)
}

// Test 2: Data Structure Performance
test "数据结构性能测试" {
  // 测试数组操作性能
  let benchmark_array_operations = fn(size: Int) {
    // 创建数组
    let start_time = Time::now()
    let mut arr = []
    for i in 0..size {
      arr = arr.push(i)
    }
    let creation_time = Time::now() - start_time
    
    // 查找操作
    start_time = Time::now()
    let target = size / 2
    let found = arr.contains(target)
    let search_time = Time::now() - start_time
    
    // 过滤操作
    start_time = Time::now()
    let filtered = arr.filter(fn(x) { x % 2 == 0 })
    let filter_time = Time::now() - start_time
    
    // 映射操作
    start_time = Time::now()
    let mapped = arr.map(fn(x) { x * 2 })
    let map_time = Time::now() - start_time
    
    {
      creation_time,
      search_time,
      filter_time,
      map_time,
      array_size: size,
      filtered_count: filtered.length(),
      mapped_count: mapped.length()
    }
  }
  
  // 测试Map操作性能
  let benchmark_map_operations = fn(size: Int) {
    // 创建Map
    let start_time = Time::now()
    let mut map = Map::empty()
    for i in 0..size {
      let _ = Map::insert(map, "key" + i.to_string(), i)
    }
    let creation_time = Time::now() - start_time
    
    // 查找操作
    start_time = Time::now()
    let target_key = "key" + (size / 2).to_string()
    let found = Map::get(map, target_key)
    let search_time = Time::now() - start_time
    
    // 更新操作
    start_time = Time::now()
    let mut updated_map = map
    for i in 0..size {
      let _ = Map::insert(updated_map, "key" + i.to_string(), i * 2)
    }
    let update_time = Time::now() - start_time
    
    {
      creation_time,
      search_time,
      update_time,
      map_size: size,
      found_value: found
    }
  }
  
  // 测试不同大小的数据结构
  let small_array_perf = benchmark_array_operations(100)
  let medium_array_perf = benchmark_array_operations(1000)
  let large_array_perf = benchmark_array_operations(10000)
  
  let small_map_perf = benchmark_map_operations(100)
  let medium_map_perf = benchmark_map_operations(1000)
  let large_map_perf = benchmark_map_operations(10000)
  
  // 验证结果
  assert_eq(small_array_perf.array_size, 100)
  assert_eq(small_array_perf.filtered_count, 50)  // 一半的偶数
  assert_eq(small_array_perf.mapped_count, 100)
  
  assert_eq(medium_array_perf.array_size, 1000)
  assert_eq(medium_array_perf.filtered_count, 500)
  assert_eq(medium_array_perf.mapped_count, 1000)
  
  assert_eq(large_array_perf.array_size, 10000)
  assert_eq(large_array_perf.filtered_count, 5000)
  assert_eq(large_array_perf.mapped_count, 10000)
  
  // 验证Map操作
  match small_map_perf.found_value {
    Some(value) => assert_eq(value, 50)
    None => assert_true(false)
  }
  
  // 验证性能差异
  // 大数据集的操作应该比小数据集慢
  assert_true(large_array_perf.creation_time > small_array_perf.creation_time)
  assert_true(large_map_perf.creation_time > small_map_perf.creation_time)
  
  // Map查找应该比数组查找快（对于大数据集）
  assert_true(large_map_perf.search_time < large_array_perf.search_time)
}

// Test 3: Memory Allocation Performance
test "内存分配性能测试" {
  // 测试字符串创建性能
  let benchmark_string_creation = fn(count: Int, length: Int) {
    let start_time = Time::now()
    let mut strings = []
    
    for i in 0..count {
      let mut str = ""
      for j in 0..length {
        str = str + "a"
      }
      strings = strings.push(str)
    }
    
    let end_time = Time::now()
    {
      creation_time: end_time - start_time,
      string_count: count,
      total_length: count * length,
      avg_length: length
    }
  }
  
  // 测试对象创建性能
  type TestObject = {
    id: Int,
    name: String,
    data: Array[Int]
  }
  
  let benchmark_object_creation = fn(count: Int) {
    let start_time = Time::now()
    let mut objects = []
    
    for i in 0..count {
      let obj = {
        id: i,
        name: "object-" + i.to_string(),
        data: [i, i * 2, i * 3]
      }
      objects = objects.push(obj)
    }
    
    let end_time = Time::now()
    {
      creation_time: end_time - start_time,
      object_count: count,
      memory_usage: count * 100  // 估算内存使用
    }
  }
  
  // 测试数组扩展性能
  let benchmark_array_expansion = fn(initial_size: Int, growth_factor: Int) {
    let start_time = Time::now()
    let mut arr = []
    
    // 初始填充
    for i in 0..initial_size {
      arr = arr.push(i)
    }
    
    // 扩展数组
    for i in 0..(initial_size * growth_factor) {
      arr = arr.push(initial_size + i)
    }
    
    let end_time = Time::now()
    {
      expansion_time: end_time - start_time,
      initial_size,
      final_size: arr.length(),
      growth_factor
    }
  }
  
  // 执行性能测试
  let string_perf_small = benchmark_string_creation(100, 10)
  let string_perf_medium = benchmark_string_creation(1000, 100)
  let string_perf_large = benchmark_string_creation(10000, 1000)
  
  let object_perf_small = benchmark_object_creation(100)
  let object_perf_medium = benchmark_object_creation(1000)
  let object_perf_large = benchmark_object_creation(10000)
  
  let array_expansion_perf = benchmark_array_expansion(1000, 5)
  
  // 验证结果
  assert_eq(string_perf_small.string_count, 100)
  assert_eq(string_perf_small.total_length, 1000)
  
  assert_eq(string_perf_medium.string_count, 1000)
  assert_eq(string_perf_medium.total_length, 100000)
  
  assert_eq(string_perf_large.string_count, 10000)
  assert_eq(string_perf_large.total_length, 10000000)
  
  assert_eq(object_perf_small.object_count, 100)
  assert_eq(object_perf_medium.object_count, 1000)
  assert_eq(object_perf_large.object_count, 10000)
  
  assert_eq(array_expansion_perf.initial_size, 1000)
  assert_eq(array_expansion_perf.final_size, 6000)  // 1000 + 1000 * 5
  assert_eq(array_expansion_perf.growth_factor, 5)
  
  // 验证性能差异
  assert_true(string_perf_large.creation_time > string_perf_small.creation_time)
  assert_true(object_perf_large.creation_time > object_perf_small.creation_time)
  
  // 字符串长度对性能的影响
  assert_true(string_perf_large.creation_time > string_perf_medium.creation_time)
}

// Test 4: Function Call Overhead
test "函数调用开销测试" {
  // 简单函数
  let simple_function = fn(x: Int) { x + 1 }
  
  // 带多个参数的函数
  let multi_param_function = fn(a: Int, b: Int, c: Int) { a + b + c }
  
  // 递归函数
  let recursive_function = fn(n: Int) {
    if n <= 1 {
      1
    } else {
      n + recursive_function(n - 1)
    }
  }
  
  // 高阶函数
  let higher_order_function = fn(f: (Int) -> Int, x: Int) { f(x) * 2 }
  
  // 性能测试函数
  let benchmark_function_calls = fn(func: () -> Unit, iterations: Int) {
    let start_time = Time::now()
    
    for i in 0..iterations {
      func()
    }
    
    let end_time = Time::now()
    end_time - start_time
  }
  
  // 测试简单函数调用
  let simple_func = fn() { simple_function(42) }
  let simple_time = benchmark_function_calls(simple_func, 100000)
  
  // 测试多参数函数调用
  let multi_param_func = fn() { multi_param_function(1, 2, 3) }
  let multi_param_time = benchmark_function_calls(multi_param_func, 100000)
  
  // 测试递归函数调用
  let recursive_func = fn() { recursive_function(10) }
  let recursive_time = benchmark_function_calls(recursive_func, 10000)  // 较少迭代，因为递归开销大
  
  // 测试高阶函数调用
  let higher_order_func = fn() { higher_order_function(simple_function, 5) }
  let higher_order_time = benchmark_function_calls(higher_order_func, 100000)
  
  // 测试内联操作（无函数调用）
  let inline_func = fn() { 
    let x = 42
    let _ = x + 1
  }
  let inline_time = benchmark_function_calls(inline_func, 100000)
  
  // 验证函数正确性
  assert_eq(simple_function(42), 43)
  assert_eq(multi_param_function(1, 2, 3), 6)
  assert_eq(recursive_function(5), 15)  // 5 + 4 + 3 + 2 + 1
  assert_eq(higher_order_function(simple_function, 5), 12)  // (5 + 1) * 2
  
  // 验证性能差异
  // 内联操作应该最快
  assert_true(inline_time < simple_time)
  
  // 高阶函数应该比简单函数慢
  assert_true(higher_order_time > simple_time)
  
  // 递归函数应该最慢（即使迭代次数较少）
  assert_true(recursive_time > simple_time)
}

// Test 5: Telemetry Collection Performance
test "遥测数据收集性能测试" {
  // 定义遥测数据点
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 遥测收集器
  type TelemetryCollector = {
    points: Array[TelemetryPoint],
    buffer_size: Int
  }
  
  // 创建遥测收集器
  let create_collector = fn(buffer_size: Int) {
    {
      points: [],
      buffer_size
    }
  }
  
  // 添加遥测点
  let add_telemetry_point = fn(collector: TelemetryCollector, point: TelemetryPoint) {
    let updated_points = collector.points.push(point)
    
    // 如果超过缓冲区大小，移除最旧的点
    let final_points = if updated_points.length() > collector.buffer_size {
      updated_points.slice(1, updated_points.length())
    } else {
      updated_points
    }
    
    {
      ...collector,
      points: final_points
    }
  }
  
  // 批量添加遥测点
  let batch_add_points = fn(collector: TelemetryCollector, points: Array[TelemetryPoint]) {
    let mut updated_collector = collector
    
    for point in points {
      updated_collector = add_telemetry_point(updated_collector, point)
    }
    
    updated_collector
  }
  
  // 性能测试
  let benchmark_telemetry_collection = fn(point_count: Int, batch_size: Int) {
    let collector = create_collector(10000)
    
    // 创建测试数据
    let mut test_points = []
    for i in 0..point_count {
      let point = {
        timestamp: Time::now() + i,
        metric_name: "metric_" + (i % 10).to_string(),
        value: (i % 100) as Float,
        tags: [("source", "test"), ("index", i.to_string())]
      }
      test_points = test_points.push(point)
    }
    
    // 单点添加测试
    let start_time = Time::now()
    let mut single_collector = collector
    for point in test_points {
      single_collector = add_telemetry_point(single_collector, point)
    }
    let single_add_time = Time::now() - start_time
    
    // 批量添加测试
    start_time = Time::now()
    let batch_collector = batch_add_points(collector, test_points)
    let batch_add_time = Time::now() - start_time
    
    // 查询测试
    start_time = Time::now()
    let filtered_points = batch_collector.points.filter(fn(p) { p.metric_name == "metric_5" })
    let query_time = Time::now() - start_time
    
    // 聚合测试
    start_time = Time::now()
    let sum = batch_collector.points.reduce(fn(acc, p) { acc + p.value }, 0.0)
    let avg = sum / (batch_collector.points.length() as Float)
    let aggregation_time = Time::now() - start_time
    
    {
      point_count,
      single_add_time,
      batch_add_time,
      query_time,
      aggregation_time,
      final_count: batch_collector.points.length(),
      filtered_count: filtered_points.length(),
      average_value: avg
    }
  }
  
  // 执行性能测试
  let small_collection_perf = benchmark_telemetry_collection(1000, 100)
  let medium_collection_perf = benchmark_telemetry_collection(10000, 1000)
  let large_collection_perf = benchmark_telemetry_collection(100000, 10000)
  
  // 验证结果
  assert_eq(small_collection_perf.point_count, 1000)
  assert_eq(small_collection_perf.final_count, 1000)  // 未超过缓冲区
  
  assert_eq(medium_collection_perf.point_count, 10000)
  assert_eq(medium_collection_perf.final_count, 10000)  // 刚好达到缓冲区
  
  assert_eq(large_collection_perf.point_count, 100000)
  assert_eq(large_collection_perf.final_count, 10000)  // 超过缓冲区，被截断
  
  // 验证聚合结果
  assert_true(small_collection_perf.average_value > 0.0)
  assert_true(medium_collection_perf.average_value > 0.0)
  assert_true(large_collection_perf.average_value > 0.0)
  
  // 验证过滤结果
  assert_true(small_collection_perf.filtered_count > 0)
  assert_true(medium_collection_perf.filtered_count > 0)
  assert_true(large_collection_perf.filtered_count > 0)
  
  // 验证性能差异
  // 批量添加应该比单点添加快
  assert_true(medium_collection_perf.batch_add_time < medium_collection_perf.single_add_time)
  
  // 大数据集操作应该比小数据集慢
  assert_true(large_collection_perf.query_time > small_collection_perf.query_time)
  assert_true(large_collection_perf.aggregation_time > small_collection_perf.aggregation_time)
}