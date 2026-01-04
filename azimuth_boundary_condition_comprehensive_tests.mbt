// Azimuth 边界条件综合测试用例
// 专注于边界条件、极端情况和异常场景测试

// 测试1: 数值边界条件测试
test "数值边界条件测试" {
  // 定义数值边界测试函数
  let test_int_boundaries = fn(operation: (Int -> Int)) {
    // 测试零值
    let zero_result = operation(0)
    
    // 测试正边界
    let max_int = 2147483647
    let max_result = operation(max_int)
    
    // 测试负边界
    let min_int = -2147483648
    let min_result = operation(min_int)
    
    // 测试接近边界的值
    let near_max_result = operation(max_int - 1)
    let near_min_result = operation(min_int + 1)
    
    (zero_result, max_result, min_result, near_max_result, near_min_result)
  }
  
  // 测试加法边界
  let addition = fn(x: Int) { x + 1 }
  let addition_results = test_int_boundaries(addition)
  
  assert_eq(addition_results.0, 1)  # 0 + 1 = 1
  assert_eq(addition_results.2, -2147483647)  # -2147483648 + 1 = -2147483647
  
  // 测试减法边界
  let subtraction = fn(x: Int) { x - 1 }
  let subtraction_results = test_int_boundaries(subtraction)
  
  assert_eq(subtraction_results.0, -1)  # 0 - 1 = -1
  assert_eq(subtraction_results.1, 2147483646)  # 2147483647 - 1 = 2147483646
  
  // 测试乘法边界
  let multiplication = fn(x: Int) { x * 2 }
  let multiplication_results = test_int_boundaries(multiplication)
  
  assert_eq(multiplication_results.0, 0)  # 0 * 2 = 0
  
  // 测试除法边界
  let safe_division = fn(x: Int) {
    if x == 0 {
      0  # 避免除零错误
    } else {
      1000 / x
    }
  }
  
  let division_results = test_int_boundaries(safe_division)
  assert_eq(division_results.0, 0)  # 特殊情况处理
  
  // 测试浮点数边界
  let test_float_boundaries = fn(operation: (Float -> Float)) {
    let zero_result = operation(0.0)
    let max_result = operation(Float::max_value)
    let min_result = operation(Float::min_value)
    let inf_result = operation(Float::infinity)
    let neg_inf_result = operation(Float::neg_infinity)
    let nan_result = operation(Float::nan)
    
    (zero_result, max_result, min_result, inf_result, neg_inf_result, nan_result)
  }
  
  // 测试浮点数加法
  let float_addition = fn(x: Float) { x + 1.0 }
  let float_addition_results = test_float_boundaries(float_addition)
  
  assert_eq(float_addition_results.0, 1.0)  # 0.0 + 1.0 = 1.0
  assert_true(float_addition_results.3.is_infinity())  # infinity + 1.0 = infinity
  
  // 测试浮点数除法
  let float_division = fn(x: Float) {
    if x == 0.0 {
      0.0  # 避免除零错误
    } else {
      1.0 / x
    }
  }
  
  let float_division_results = test_float_boundaries(float_division)
  assert_eq(float_division_results.0, 0.0)  # 特殊情况处理
  assert_eq(float_division_results.3, 0.0)  # 1.0 / infinity = 0.0
  assert_eq(float_division_results.4, 0.0)  # 1.0 / -infinity = 0.0
  assert_true(float_division_results.5.is_nan())  # 1.0 / NaN = NaN
  
  // 测试数组边界
  let test_array_boundaries = fn() {
    let empty_array = []
    let single_element_array = [1]
    let large_array = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    
    // 测试空数组访问
    let empty_first = if empty_array.length() > 0 { Some(empty_array[0]) } else { None }
    let empty_last = if empty_array.length() > 0 { Some(empty_array[empty_array.length() - 1]) } else { None }
    
    // 测试单元素数组访问
    let single_first = if single_element_array.length() > 0 { Some(single_element_array[0]) } else { None }
    let single_last = if single_element_array.length() > 0 { Some(single_element_array[single_element_array.length() - 1]) } else { None }
    
    // 测试大数组访问
    let large_first = if large_array.length() > 0 { Some(large_array[0]) } else { None }
    let large_last = if large_array.length() > 0 { Some(large_array[large_array.length() - 1]) } else { None }
    
    // 测试边界索引访问
    let large_boundary = if large_array.length() > 4 { Some(large_array[4]) } else { None }
    
    (empty_first, empty_last, single_first, single_last, large_first, large_last, large_boundary)
  }
  
  let array_results = test_array_boundaries()
  assert_eq(array_results.0, None)  # 空数组第一个元素
  assert_eq(array_results.1, None)  # 空数组最后一个元素
  assert_eq(array_results.2, Some(1))  # 单元素数组第一个元素
  assert_eq(array_results.3, Some(1))  # 单元素数组最后一个元素
  assert_eq(array_results.4, Some(1))  # 大数组第一个元素
  assert_eq(array_results.5, Some(10))  # 大数组最后一个元素
  assert_eq(array_results.6, Some(5))  # 大数组边界索引
}

// 测试2: 字符串边界条件测试
test "字符串边界条件测试" {
  // 测试空字符串
  let empty_string = ""
  assert_eq(empty_string.length(), 0)
  assert_eq(empty_string.to_uppercase(), "")
  assert_eq(empty_string.to_lowercase(), "")
  assert_false(empty_string.contains("a"))
  assert_true(empty_string.starts_with(""))
  assert_true(empty_string.ends_with(""))
  
  // 测试单字符字符串
  let single_char_string = "a"
  assert_eq(single_char_string.length(), 1)
  assert_eq(single_char_string.to_uppercase(), "A")
  assert_eq(single_char_string.to_lowercase(), "a")
  assert_true(single_char_string.contains("a"))
  assert_false(single_char_string.contains("b"))
  assert_true(single_char_string.starts_with("a"))
  assert_true(single_char_string.ends_with("a"))
  assert_false(single_char_string.starts_with("b"))
  assert_false(single_char_string.ends_with("b"))
  
  // 测试长字符串
  let long_string = "a".repeat(10000)
  assert_eq(long_string.length(), 10000)
  assert_true(long_string.contains("a"))
  assert_false(long_string.contains("b"))
  assert_true(long_string.starts_with("a"))
  assert_true(long_string.ends_with("a"))
  
  // 测试Unicode字符串
  let unicode_string = "你好世界"
  assert_eq(unicode_string.length(), 4)
  assert_true(unicode_string.contains("你好"))
  assert_true(unicode_string.contains("世界"))
  assert_false(unicode_string.contains("hello"))
  assert_true(unicode_string.starts_with("你好"))
  assert_true(unicode_string.ends_with("世界"))
  
  // 测试特殊字符字符串
  let special_chars_string = "!@#$%^&*()_+-=[]{}|;':\",./<>?"
  assert_true(special_chars_string.length() > 0)
  assert_true(special_chars_string.contains("!"))
  assert_true(special_chars_string.contains("?"))
  assert_false(special_chars_string.contains("a"))
  
  // 测试字符串截取边界
  let test_string = "Hello, World!"
  
  // 测试空截取
  let empty_substring = test_string.substring(0, 0)
  assert_eq(empty_substring, "")
  
  // 测试完整截取
  let full_substring = test_string.substring(0, test_string.length())
  assert_eq(full_substring, test_string)
  
  // 测试单字符截取
  let single_char_substring = test_string.substring(0, 1)
  assert_eq(single_char_substring, "H")
  
  // 测试边界截取
  let boundary_substring = test_string.substring(test_string.length() - 1, test_string.length())
  assert_eq(boundary_substring, "!")
  
  // 测试超出边界截取
  let out_of_bounds_substring = test_string.substring(0, test_string.length() + 10)
  assert_eq(out_of_bounds_substring, test_string)
  
  // 测试字符串分割边界
  let empty_split_string = ""
  let empty_split_result = empty_split_string.split(",")
  assert_eq(empty_split_result.length(), 1)
  assert_eq(empty_split_result[0], "")
  
  let no_delimiter_string = "nodelimiters"
  let no_delimiter_result = no_delimiter_string.split(",")
  assert_eq(no_delimiter_result.length(), 1)
  assert_eq(no_delimiter_result[0], "nodelimiters")
  
  let single_delimiter_string = "a,b"
  let single_delimiter_result = single_delimiter_string.split(",")
  assert_eq(single_delimiter_result.length(), 2)
  assert_eq(single_delimiter_result[0], "a")
  assert_eq(single_delimiter_result[1], "b")
  
  let multiple_delimiter_string = "a,b,c,d,e"
  let multiple_delimiter_result = multiple_delimiter_string.split(",")
  assert_eq(multiple_delimiter_result.length(), 5)
  assert_eq(multiple_delimiter_result[0], "a")
  assert_eq(multiple_delimiter_result[4], "e")
  
  // 测试连续分隔符
  let consecutive_delimiters_string = "a,,b"
  let consecutive_delimiters_result = consecutive_delimiters_string.split(",")
  assert_eq(consecutive_delimiters_result.length(), 3)
  assert_eq(consecutive_delimiters_result[0], "a")
  assert_eq(consecutive_delimiters_result[1], "")
  assert_eq(consecutive_delimiters_result[2], "b")
  
  // 测试首尾分隔符
  let edge_delimiters_string = ",a,b,"
  let edge_delimiters_result = edge_delimiters_string.split(",")
  assert_eq(edge_delimiters_result.length(), 4)
  assert_eq(edge_delimiters_result[0], "")
  assert_eq(edge_delimiters_result[1], "a")
  assert_eq(edge_delimiters_result[2], "b")
  assert_eq(edge_delimiters_result[3], "")
}

// 测试3: 集合边界条件测试
test "集合边界条件测试" {
  // 测试空集合
  let empty_set = Set::new()
  assert_eq(empty_set.size(), 0)
  assert_false(empty_set.contains("a"))
  assert_true(empty_set.is_empty())
  
  // 测试单元素集合
  let single_set = Set::singleton("a")
  assert_eq(single_set.size(), 1)
  assert_true(single_set.contains("a"))
  assert_false(single_set.contains("b"))
  assert_false(single_set.is_empty())
  
  // 测试大集合
  let mut large_set = Set::new()
  for i in 0..1000 {
    large_set = large_set.add("item_" + i.to_string())
  }
  assert_eq(large_set.size(), 1000)
  assert_true(large_set.contains("item_0"))
  assert_true(large_set.contains("item_999"))
  assert_false(large_set.contains("item_1000"))
  
  // 测试集合操作边界
  let set_a = Set::from_array(["a", "b", "c"])
  let set_b = Set::from_array(["c", "d", "e"])
  let empty_set_b = Set::new()
  
  // 测试并集
  let union_ab = set_a.union(set_b)
  assert_eq(union_ab.size(), 5)
  assert_true(union_ab.contains("a"))
  assert_true(union_ab.contains("e"))
  
  let union_with_empty = set_a.union(empty_set_b)
  assert_eq(union_with_empty.size(), 3)
  assert_eq(union_with_empty, set_a)
  
  // 测试交集
  let intersection_ab = set_a.intersect(set_b)
  assert_eq(intersection_ab.size(), 1)
  assert_true(intersection_ab.contains("c"))
  
  let intersection_with_empty = set_a.intersect(empty_set_b)
  assert_eq(intersection_with_empty.size(), 0)
  assert_true(intersection_with_empty.is_empty())
  
  // 测试差集
  let difference_ab = set_a.difference(set_b)
  assert_eq(difference_ab.size(), 2)
  assert_true(difference_ab.contains("a"))
  assert_true(difference_ab.contains("b"))
  assert_false(difference_ab.contains("c"))
  
  let difference_with_empty = set_a.difference(empty_set_b)
  assert_eq(difference_with_empty.size(), 3)
  assert_eq(difference_with_empty, set_a)
  
  // 测试空Map
  let empty_map = Map::new()
  assert_eq(empty_map.size(), 0)
  assert_eq(empty_map.get("a"), None)
  assert_true(empty_map.is_empty())
  
  // 测试单元素Map
  let single_map = Map::new().set("a", 1)
  assert_eq(single_map.size(), 1)
  assert_eq(single_map.get("a"), Some(1))
  assert_eq(single_map.get("b"), None)
  assert_false(single_map.is_empty())
  
  // 测试大Map
  let mut large_map = Map::new()
  for i in 0..1000 {
    large_map = large_map.set("key_" + i.to_string(), i)
  }
  assert_eq(large_map.size(), 1000)
  assert_eq(large_map.get("key_0"), Some(0))
  assert_eq(large_map.get("key_999"), Some(999))
  assert_eq(large_map.get("key_1000"), None)
  
  // 测试Map操作边界
  let map_a = Map::new().set("a", 1).set("b", 2).set("c", 3)
  let map_b = Map::new().set("c", 4).set("d", 5).set("e", 6)
  let empty_map_b = Map::new()
  
  // 测试Map合并
  let merged_ab = map_a.merge(map_b)
  assert_eq(merged_ab.size(), 5)
  assert_eq(merged_ab.get("a"), Some(1))
  assert_eq(merged_ab.get("c"), Some(4))  # map_b的值覆盖map_a的值
  assert_eq(merged_ab.get("e"), Some(6))
  
  let merged_with_empty = map_a.merge(empty_map_b)
  assert_eq(merged_with_empty.size(), 3)
  assert_eq(merged_with_empty, map_a)
  
  // 测试Map过滤
  let filtered_map = map_a.filter_fn(key, value) { value > 1 }
  assert_eq(filtered_map.size(), 2)
  assert_eq(filtered_map.get("a"), None)
  assert_eq(filtered_map.get("b"), Some(2))
  assert_eq(filtered_map.get("c"), Some(3))
  
  let filtered_all = map_a.filter_fn(key, value) { value > 10 }
  assert_eq(filtered_all.size(), 0)
  assert_true(filtered_all.is_empty())
  
  let filtered_none = map_a.filter_fn(key, value) { value >= 0 }
  assert_eq(filtered_none.size(), 3)
  assert_eq(filtered_none, map_a)
}

// 测试4: 时间边界条件测试
test "时间边界条件测试" {
  // 测试时间戳边界
  let zero_timestamp = 0
  let max_timestamp = 2147483647  # 32位有符号整数最大值
  let min_timestamp = -2147483648  # 32位有符号整数最小值
  
  // 测试时间计算
  let add_day = fn(timestamp: Int) { timestamp + 86400 }  # 加一天
  let subtract_day = fn(timestamp: Int) { timestamp - 86400 }  # 减一天
  
  let zero_plus_day = add_day(zero_timestamp)
  assert_eq(zero_plus_day, 86400)
  
  let max_plus_day = add_day(max_timestamp)
  // 注意：这可能会导致溢出，实际应用中需要处理
  
  let min_minus_day = subtract_day(min_timestamp)
  // 注意：这可能会导致溢出，实际应用中需要处理
  
  // 测试时间间隔计算
  let calculate_duration = fn(start: Int, end: Int) {
    if end >= start {
      end - start
    } else {
      0  # 简化处理，实际应用中可能需要负数持续时间
    }
  }
  
  let same_time_duration = calculate_duration(1000, 1000)
  assert_eq(same_time_duration, 0)
  
  let positive_duration = calculate_duration(1000, 2000)
  assert_eq(positive_duration, 1000)
  
  let negative_duration = calculate_duration(2000, 1000)
  assert_eq(negative_duration, 0)  # 简化处理
  
  let zero_start_duration = calculate_duration(0, 1000)
  assert_eq(zero_start_duration, 1000)
  
  let zero_end_duration = calculate_duration(1000, 0)
  assert_eq(zero_end_duration, 0)  # 简化处理
  
  // 测试时间格式化边界
  let format_timestamp = fn(timestamp: Int) {
    if timestamp == 0 {
      "1970-01-01 00:00:00 UTC"
    } else if timestamp == 86400 {
      "1970-01-02 00:00:00 UTC"
    } else if timestamp == -86400 {
      "1969-12-31 00:00:00 UTC"
    } else {
      "Other time"
    }
  }
  
  assert_eq(format_timestamp(0), "1970-01-01 00:00:00 UTC")
  assert_eq(format_timestamp(86400), "1970-01-02 00:00:00 UTC")
  assert_eq(format_timestamp(-86400), "1969-12-31 00:00:00 UTC")
  assert_eq(format_timestamp(123456789), "Other time")
  
  // 测试时区边界
  let convert_timezone = fn(timestamp: Int, offset_hours: Int) {
    timestamp + (offset_hours * 3600)
  }
  
  let utc_time = 43200  # 1970-01-01 12:00:00 UTC
  
  let utc_plus_1 = convert_timezone(utc_time, 1)
  assert_eq(utc_plus_1, 46800)  # 1970-01-01 13:00:00 UTC+1
  
  let utc_minus_1 = convert_timezone(utc_time, -1)
  assert_eq(utc_minus_1, 39600)  # 1970-01-01 11:00:00 UTC-1
  
  let utc_plus_12 = convert_timezone(utc_time, 12)
  assert_eq(utc_plus_12, 86400)  # 1970-01-02 00:00:00 UTC+12
  
  let utc_minus_12 = convert_timezone(utc_time, -12)
  assert_eq(utc_minus_12, 0)  # 1970-01-01 00:00:00 UTC-12
  
  // 测试日期边界
  let is_leap_year = fn(year: Int) {
    (year % 4 == 0 and year % 100 != 0) or (year % 400 == 0)
  }
  
  assert_false(is_leap_year(1900))
  assert_true(is_leap_year(2000))
  assert_true(is_leap_year(2004))
  assert_false(is_leap_year(2100))
  
  // 测试月份天数
  let days_in_month = fn(year: Int, month: Int) {
    match month {
      1 => 31,
      2 => if is_leap_year(year) { 29 } else { 28 },
      3 => 31,
      4 => 30,
      5 => 31,
      6 => 30,
      7 => 31,
      8 => 31,
      9 => 30,
      10 => 31,
      11 => 30,
      12 => 31,
      _ => 0
    }
  }
  
  assert_eq(days_in_month(2000, 2), 29)  # 闰年二月
  assert_eq(days_in_month(2001, 2), 28)  # 平年二月
  assert_eq(days_in_month(2000, 4), 30)  # 四月
  assert_eq(days_in_month(2000, 12), 31)  # 十二月
  assert_eq(days_in_month(2000, 0), 0)   # 无效月份
  assert_eq(days_in_month(2000, 13), 0)  # 无效月份
}

// 测试5: 资源边界条件测试
test "资源边界条件测试" {
  // 测试内存分配边界
  let test_memory_allocation = fn(size: Int) {
    // 简化的内存分配测试
    if size <= 0 {
      ("empty", 0)
    } else if size == 1 {
      ("minimal", 1)
    } else if size >= 1000000 {
      ("large", size)
    } else {
      ("normal", size)
    }
  }
  
  let empty_allocation = test_memory_allocation(0)
  assert_eq(empty_allocation, ("empty", 0))
  
  let minimal_allocation = test_memory_allocation(1)
  assert_eq(minimal_allocation, ("minimal", 1))
  
  let normal_allocation = test_memory_allocation(1000)
  assert_eq(normal_allocation, ("normal", 1000))
  
  let large_allocation = test_memory_allocation(1000000)
  assert_eq(large_allocation, ("large", 1000000))
  
  // 测试文件大小边界
  let test_file_size = fn(size_bytes: Int) {
    if size_bytes == 0 {
      "empty"
    } else if size_bytes < 1024 {
      "small"
    } else if size_bytes < 1024 * 1024 {
      "medium"
    } else if size_bytes < 1024 * 1024 * 1024 {
      "large"
    } else {
      "huge"
    }
  }
  
  assert_eq(test_file_size(0), "empty")
  assert_eq(test_file_size(512), "small")
  assert_eq(test_file_size(1024), "medium")
  assert_eq(test_file_size(1024 * 512), "medium")
  assert_eq(test_file_size(1024 * 1024), "large")
  assert_eq(test_file_size(1024 * 1024 * 512), "large")
  assert_eq(test_file_size(1024 * 1024 * 1024), "huge")
  
  // 测试并发连接边界
  let test_connection_pool = fn(current_connections: Int, max_connections: Int) {
    if current_connections >= max_connections {
      ("rejected", current_connections)
    } else if current_connections >= max_connections * 80 / 100 {
      ("warning", current_connections + 1)
    } else {
      ("accepted", current_connections + 1)
    }
  }
  
  let max_connections = 100
  
  let empty_pool = test_connection_pool(0, max_connections)
  assert_eq(empty_pool, ("accepted", 1))
  
  let normal_pool = test_connection_pool(50, max_connections)
  assert_eq(normal_pool, ("accepted", 51))
  
  let warning_pool = test_connection_pool(85, max_connections)
  assert_eq(warning_pool, ("warning", 86))
  
  let full_pool = test_connection_pool(100, max_connections)
  assert_eq(full_pool, ("rejected", 100))
  
  let overfull_pool = test_connection_pool(150, max_connections)
  assert_eq(overfull_pool, ("rejected", 150))
  
  // 测试队列容量边界
  let test_queue_capacity = fn(queue_size: Int, max_capacity: Int) {
    if queue_size >= max_capacity {
      ("full", false)
    } else if queue_size >= max_capacity * 90 / 100 {
      ("nearly_full", true)
    } else {
      ("available", true)
    }
  }
  
  let max_capacity = 1000
  
  let empty_queue = test_queue_capacity(0, max_capacity)
  assert_eq(empty_queue, ("available", true))
  
  let normal_queue = test_queue_capacity(500, max_capacity)
  assert_eq(normal_queue, ("available", true))
  
  let nearly_full_queue = test_queue_capacity(950, max_capacity)
  assert_eq(nearly_full_queue, ("nearly_full", true))
  
  let full_queue = test_queue_capacity(1000, max_capacity)
  assert_eq(full_queue, ("full", false))
  
  let overfull_queue = test_queue_capacity(1500, max_capacity)
  assert_eq(overfull_queue, ("full", false))
  
  // 测试缓存大小边界
  let test_cache_size = fn(current_size: Int, max_size: Int) {
    let utilization = current_size.to_float() / max_size.to_float()
    
    if utilization >= 1.0 {
      ("overflow", 1.0)
    } else if utilization >= 0.9 {
      ("critical", utilization)
    } else if utilization >= 0.7 {
      ("warning", utilization)
    } else if utilization >= 0.3 {
      ("normal", utilization)
    } else {
      ("underutilized", utilization)
    }
  }
  
  let max_cache_size = 10000
  
  let empty_cache = test_cache_size(0, max_cache_size)
  assert_eq(empty_cache, ("underutilized", 0.0))
  
  let small_cache = test_cache_size(1000, max_cache_size)
  assert_eq(small_cache, ("underutilized", 0.1))
  
  let normal_cache = test_cache_size(5000, max_cache_size)
  assert_eq(normal_cache, ("normal", 0.5))
  
  let warning_cache = test_cache_size(8000, max_cache_size)
  assert_eq(warning_cache, ("warning", 0.8))
  
  let critical_cache = test_cache_size(9500, max_cache_size)
  assert_eq(critical_cache, ("critical", 0.95))
  
  let full_cache = test_cache_size(10000, max_cache_size)
  assert_eq(full_cache, ("overflow", 1.0))
  
  let overfull_cache = test_cache_size(12000, max_cache_size)
  assert_eq(overfull_cache, ("overflow", 1.0))
}

// 测试6: 网络边界条件测试
test "网络边界条件测试" {
  // 测试端口边界
  let test_port_validation = fn(port: Int) {
    if port < 0 {
      ("invalid", "Port cannot be negative")
    } else if port == 0 {
      ("invalid", "Port 0 is reserved")
    } else if port < 1024 {
      ("privileged", "Privileged port")
    } else if port <= 65535 {
      ("valid", "Regular port")
    } else {
      ("invalid", "Port exceeds maximum value")
    }
  }
  
  assert_eq(test_port_validation(-1), ("invalid", "Port cannot be negative"))
  assert_eq(test_port_validation(0), ("invalid", "Port 0 is reserved"))
  assert_eq(test_port_validation(22), ("privileged", "Privileged port"))
  assert_eq(test_port_validation(80), ("privileged", "Privileged port"))
  assert_eq(test_port_validation(8080), ("valid", "Regular port"))
  assert_eq(test_port_validation(65535), ("valid", "Regular port"))
  assert_eq(test_port_validation(65536), ("invalid", "Port exceeds maximum value"))
  
  // 测试IP地址边界
  let test_ip_validation = fn(ip: String) {
    if ip == "" {
      ("invalid", "Empty IP address")
    } else if ip == "0.0.0.0" {
      ("unspecified", "Unspecified address")
    } else if ip == "127.0.0.1" {
      ("loopback", "Loopback address")
    } else if ip.starts_with("192.168.") {
      ("private", "Private network")
    } else if ip.starts_with("10.") {
      ("private", "Private network")
    } else if ip.starts_with("172.") {
      let parts = ip.split(".")
      if parts.length() >= 2 {
        let second_octet = parts[1].to_int()
        if second_octet >= 16 and second_octet <= 31 {
          ("private", "Private network")
        } else {
          ("public", "Public address")
        }
      } else {
        ("invalid", "Invalid IP format")
      }
    } else {
      ("public", "Public address")
    }
  }
  
  assert_eq(test_ip_validation(""), ("invalid", "Empty IP address"))
  assert_eq(test_ip_validation("0.0.0.0"), ("unspecified", "Unspecified address"))
  assert_eq(test_ip_validation("127.0.0.1"), ("loopback", "Loopback address"))
  assert_eq(test_ip_validation("192.168.1.1"), ("private", "Private network"))
  assert_eq(test_ip_validation("10.0.0.1"), ("private", "Private network"))
  assert_eq(test_ip_validation("172.16.0.1"), ("private", "Private network"))
  assert_eq(test_ip_validation("172.32.0.1"), ("public", "Public address"))
  assert_eq(test_ip_validation("8.8.8.8"), ("public", "Public address"))
  
  // 测试URL长度边界
  let test_url_length = fn(url: String) {
    let length = url.length()
    
    if length == 0 {
      ("invalid", "Empty URL")
    } else if length < 10 {
      ("short", "Very short URL")
    } else if length <= 2048 {
      ("normal", "Normal URL length")
    } else if length <= 8192 {
      ("long", "Long URL")
    } else {
      ("too_long", "URL too long")
    }
  }
  
  assert_eq(test_url_length(""), ("invalid", "Empty URL"))
  assert_eq(test_url_length("a.co"), ("short", "Very short URL"))
  assert_eq(test_url_length("https://example.com"), ("normal", "Normal URL length"))
  
  let long_url = "https://example.com/" + "a".repeat(2000)
  assert_eq(test_url_length(long_url), ("long", "Long URL"))
  
  let very_long_url = "https://example.com/" + "a".repeat(9000)
  assert_eq(test_url_length(very_long_url), ("too_long", "URL too long"))
  
  // 测试超时边界
  let test_timeout = fn(timeout_ms: Int) {
    if timeout_ms <= 0 {
      ("invalid", "Timeout must be positive")
    } else if timeout_ms < 100 {
      ("very_short", "Very short timeout")
    } else if timeout_ms < 1000 {
      ("short", "Short timeout")
    } else if timeout_ms < 30000 {
      ("normal", "Normal timeout")
    } else if timeout_ms < 300000 {
      ("long", "Long timeout")
    } else {
      ("very_long", "Very long timeout")
    }
  }
  
  assert_eq(test_timeout(0), ("invalid", "Timeout must be positive"))
  assert_eq(test_timeout(-100), ("invalid", "Timeout must be positive"))
  assert_eq(test_timeout(50), ("very_short", "Very short timeout"))
  assert_eq(test_timeout(500), ("short", "Short timeout"))
  assert_eq(test_timeout(5000), ("normal", "Normal timeout"))
  assert_eq(test_timeout(60000), ("long", "Long timeout"))
  assert_eq(test_timeout(600000), ("very_long", "Very long timeout"))
  
  // 测试连接池边界
  let test_connection_pool_size = fn(size: Int) {
    if size <= 0 {
      ("invalid", "Pool size must be positive")
    } else if size == 1 {
      ("minimal", "Minimal pool size")
    } else if size <= 10 {
      ("small", "Small pool size")
    } else if size <= 100 {
      ("medium", "Medium pool size")
    } else if size <= 1000 {
      ("large", "Large pool size")
    } else {
      ("very_large", "Very large pool size")
    }
  }
  
  assert_eq(test_connection_pool_size(0), ("invalid", "Pool size must be positive"))
  assert_eq(test_connection_pool_size(-5), ("invalid", "Pool size must be positive"))
  assert_eq(test_connection_pool_size(1), ("minimal", "Minimal pool size"))
  assert_eq(test_connection_pool_size(5), ("small", "Small pool size"))
  assert_eq(test_connection_pool_size(50), ("medium", "Medium pool size"))
  assert_eq(test_connection_pool_size(500), ("large", "Large pool size"))
  assert_eq(test_connection_pool_size(5000), ("very_large", "Very large pool size"))
}

// 测试7: 输入验证边界条件测试
test "输入验证边界条件测试" {
  // 测试用户名边界
  let test_username = fn(username: String) {
    let length = username.length()
    
    if length == 0 {
      ("invalid", "Username cannot be empty")
    } else if length < 3 {
      ("invalid", "Username too short")
    } else if length > 20 {
      ("invalid", "Username too long")
    } else if username.starts_with(" ") or username.ends_with(" ") {
      ("invalid", "Username cannot start or end with space")
    } else if username.contains(" ") {
      ("invalid", "Username cannot contain spaces")
    } else {
      ("valid", "Valid username")
    }
  }
  
  assert_eq(test_username(""), ("invalid", "Username cannot be empty"))
  assert_eq(test_username("ab"), ("invalid", "Username too short"))
  assert_eq(test_username("a"), ("invalid", "Username too short"))
  assert_eq(test_username("a" * 21), ("invalid", "Username too long"))
  assert_eq(test_username(" username"), ("invalid", "Username cannot start or end with space"))
  assert_eq(test_username("username "), ("invalid", "Username cannot start or end with space"))
  assert_eq(test_username("user name"), ("invalid", "Username cannot contain spaces"))
  assert_eq(test_username("user"), ("valid", "Valid username"))
  assert_eq(test_username("a" * 20), ("valid", "Valid username"))
  
  // 测试密码强度边界
  let test_password_strength = fn(password: String) {
    let length = password.length()
    let has_upper = password.chars().any(fn(c) { c >= 'A' and c <= 'Z' })
    let has_lower = password.chars().any(fn(c) { c >= 'a' and c <= 'z' })
    let has_digit = password.chars().any(fn(c) { c >= '0' and c <= '9' })
    let has_special = password.chars().any(fn(c) { "!@#$%^&*()_+-=[]{}|;':\",./<>?".contains(c.to_string()) })
    
    if length < 6 {
      ("invalid", "Password too short")
    } else if length > 128 {
      ("invalid", "Password too long")
    } else if length < 8 {
      ("weak", "Weak password")
    } else if has_upper and has_lower and has_digit and has_special {
      ("strong", "Strong password")
    } else if (has_upper and has_lower) or (has_lower and has_digit) or (has_upper and has_digit) {
      ("medium", "Medium password")
    } else {
      ("weak", "Weak password")
    }
  }
  
  assert_eq(test_password_strength(""), ("invalid", "Password too short"))
  assert_eq(test_password_strength("12345"), ("invalid", "Password too short"))
  assert_eq(test_password_strength("a" * 129), ("invalid", "Password too long"))
  assert_eq(test_password_strength("password"), ("weak", "Weak password"))
  assert_eq(test_password_strength("Password123"), ("medium", "Medium password"))
  assert_eq(test_password_strength("Password123!"), ("strong", "Strong password"))
  
  // 测试邮箱验证边界
  let test_email = fn(email: String) {
    if email.length() == 0 {
      ("invalid", "Email cannot be empty")
    } else if not(email.contains("@")) {
      ("invalid", "Email must contain @")
    } else if email.starts_with("@") {
      ("invalid", "Email cannot start with @")
    } else if email.ends_with("@") {
      ("invalid", "Email cannot end with @")
    } else if email.count("@") > 1 {
      ("invalid", "Email can contain only one @")
    } else {
      let parts = email.split("@")
      let local = parts[0]
      let domain = parts[1]
      
      if local.length() == 0 {
        ("invalid", "Local part cannot be empty")
      } else if domain.length() == 0 {
        ("invalid", "Domain part cannot be empty")
      } else if not(domain.contains(".")) {
        ("invalid", "Domain must contain .")
      } else {
        ("valid", "Valid email")
      }
    }
  }
  
  assert_eq(test_email(""), ("invalid", "Email cannot be empty"))
  assert_eq(test_email("email"), ("invalid", "Email must contain @"))
  assert_eq(test_email("@domain.com"), ("invalid", "Local part cannot be empty"))
  assert_eq(test_email("user@"), ("invalid", "Domain part cannot be empty"))
  assert_eq(test_email("user@domain"), ("invalid", "Domain must contain ."))
  assert_eq(test_email("user@domain.com"), ("valid", "Valid email"))
  assert_eq(test_email("user.name@domain.com"), ("valid", "Valid email"))
  
  // 测试电话号码验证边界
  let test_phone = fn(phone: String) {
    let digits_only = phone.chars().filter(fn(c) { c >= '0' and c <= '9' }).join("")
    let length = digits_only.length()
    
    if phone.length() == 0 {
      ("invalid", "Phone cannot be empty")
    } else if length < 7 {
      ("invalid", "Phone too short")
    } else if length > 15 {
      ("invalid", "Phone too long")
    } else if length == 10 {
      ("valid_10_digit", "Valid 10-digit phone")
    } else if length == 11 and digits_only.starts_with("1") {
      ("valid_11_digit", "Valid 11-digit phone with country code")
    } else {
      ("valid_international", "Valid international phone")
    }
  }
  
  assert_eq(test_phone(""), ("invalid", "Phone cannot be empty"))
  assert_eq(test_phone("123"), ("invalid", "Phone too short"))
  assert_eq(test_phone("1234567890123456"), ("invalid", "Phone too long"))
  assert_eq(test_phone("1234567"), ("valid_international", "Valid international phone"))
  assert_eq(test_phone("1234567890"), ("valid_10_digit", "Valid 10-digit phone"))
  assert_eq(test_phone("11234567890"), ("valid_11_digit", "Valid 11-digit phone with country code"))
  assert_eq(test_phone("(123) 456-7890"), ("valid_10_digit", "Valid 10-digit phone"))
  assert_eq(test_phone("+1 (123) 456-7890"), ("valid_11_digit", "Valid 11-digit phone with country code"))
  
  // 测试年龄验证边界
  let test_age = fn(age: Int) {
    if age < 0 {
      ("invalid", "Age cannot be negative")
    } else if age == 0 {
      ("infant", "Infant")
    } else if age < 13 {
      ("child", "Child")
    } else if age < 18 {
      ("teenager", "Teenager")
    } else if age < 65 {
      ("adult", "Adult")
    } else if age < 120 {
      ("senior", "Senior")
    } else {
      ("invalid", "Age unrealistic")
    }
  }
  
  assert_eq(test_age(-1), ("invalid", "Age cannot be negative"))
  assert_eq(test_age(0), ("infant", "Infant"))
  assert_eq(test_age(5), ("child", "Child"))
  assert_eq(test_age(12), ("child", "Child"))
  assert_eq(test_age(13), ("teenager", "Teenager"))
  assert_eq(test_age(17), ("teenager", "Teenager"))
  assert_eq(test_age(18), ("adult", "Adult"))
  assert_eq(test_age(64), ("adult", "Adult"))
  assert_eq(test_age(65), ("senior", "Senior"))
  assert_eq(test_age(119), ("senior", "Senior"))
  assert_eq(test_age(120), ("invalid", "Age unrealistic"))
  assert_eq(test_age(150), ("invalid", "Age unrealistic"))
}