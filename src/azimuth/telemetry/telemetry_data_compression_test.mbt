// 遥测数据压缩测试用例

test "telemetry_string_compression" {
  // 测试字符串压缩（模拟压缩算法）
  
  let telemetry_strings = [
    "http.request.method=GET",
    "http.request.url=/api/users",
    "http.response.status_code=200",
    "service.name=user-service",
    "trace.id=0af7651916cd43dd8448eb211c80319c",
    "span.id=b7ad6b7169203331",
    "user.id=12345",
    "session.id=abcdef123456",
    "request.duration=125ms",
    "database.query.time=45ms"
  ]
  
  // 验证原始数据
  assert_eq(telemetry_strings.length(), 10)
  
  // 计算原始总长度
  let mut original_total_length = 0
  let mut i = 0
  while i < telemetry_strings.length() {
    original_total_length = original_total_length + telemetry_strings[i].length()
    i = i + 1
  }
  
  // 模拟压缩：重复的键值对使用字典替换
  let compression_dict = [
    ("http.request.method", "A1"),
    ("http.request.url", "A2"),
    ("http.response.status_code", "A3"),
    ("service.name", "B1"),
    ("trace.id", "C1"),
    ("span.id", "C2"),
    ("user.id", "D1"),
    ("session.id", "D2"),
    ("request.duration", "E1"),
    ("database.query.time", "E2")
  ]
  
  // 压缩数据
  let compressed_strings = []
  i = 0
  while i < telemetry_strings.length() {
    let original = telemetry_strings[i]
    let mut compressed = original
    
    let mut j = 0
    while j < compression_dict.length() {
      let key = compression_dict[j].0
      let code = compression_dict[j].1
      compressed = compressed.replace(key, code)
      j = j + 1
    }
    
    compressed_strings.push(compressed)
    i = i + 1
  }
  
  // 计算压缩后总长度
  let mut compressed_total_length = 0
  i = 0
  while i < compressed_strings.length() {
    compressed_total_length = compressed_total_length + compressed_strings[i].length()
    i = i + 1
  }
  
  // 验证压缩效果
  assert_eq(compressed_total_length < original_total_length, true)
  
  // 计算压缩率
  let compression_ratio = compressed_total_length.to_double() / original_total_length.to_double()
  assert_eq(compression_ratio > 0.5, true)
  assert_eq(compression_ratio < 0.8, true)
}

test "telemetry_numeric_compression" {
  // 测试数值压缩（使用差分编码）
  
  let time_series_values = [100, 105, 103, 108, 110, 107, 112, 115, 113, 118]
  
  // 验证原始数据
  assert_eq(time_series_values.length(), 10)
  
  // 差分压缩：存储相邻值的差值
  let differential_values = []
  differential_values.push(time_series_values[0])  // 第一个值保持不变
  
  let mut i = 1
  while i < time_series_values.length() {
    let diff = time_series_values[i] - time_series_values[i - 1]
    differential_values.push(diff)
    i = i + 1
  }
  
  // 验证差分值
  assert_eq(differential_values.length(), 10)
  assert_eq(differential_values[0], 100)  // 第一个值
  assert_eq(differential_values[1], 5)    // 105 - 100
  assert_eq(differential_values[2], -2)   // 103 - 105
  assert_eq(differential_values[9], 5)    // 118 - 113
  
  // 计算差分后的数值范围（应该更小）
  let mut min_diff = differential_values[0]
  let mut max_diff = differential_values[0]
  
  i = 1
  while i < differential_values.length() {
    if differential_values[i] < min_diff {
      min_diff = differential_values[i]
    }
    if differential_values[i] > max_diff {
      max_diff = differential_values[i]
    }
    i = i + 1
  }
  
  let original_range = 118 - 100  // 原始数据范围
  let diff_range = max_diff - min_diff  // 差分数据范围
  
  assert_eq(diff_range < original_range, true)
}

test "telemetry_batch_compression" {
  // 测试批量数据压缩
  
  let telemetry_batch = [
    ("service.name", "auth-service"),
    ("service.name", "user-service"),
    ("service.name", "payment-service"),
    ("service.name", "order-service"),
    ("service.name", "auth-service"),
    ("http.method", "GET"),
    ("http.method", "POST"),
    ("http.method", "GET"),
    ("http.method", "PUT"),
    ("http.method", "GET")
  ]
  
  // 验证原始数据
  assert_eq(telemetry_batch.length(), 10)
  
  // 统计重复键值对
  let key_value_counts = {}
  
  let mut i = 0
  while i < telemetry_batch.length() {
    let key = telemetry_batch[i].0
    let value = telemetry_batch[i].1
    let key_value = key + "=" + value
    
    // 简化的计数逻辑
    let mut count = 0
    let mut j = 0
    while j < telemetry_batch.length() {
      if telemetry_batch[j].0 == key && telemetry_batch[j].1 == value {
        count = count + 1
      }
      j = j + 1
    }
    
    // 只添加一次到压缩结果
    let mut already_added = false
    j = 0
    while j < i {
      if telemetry_batch[j].0 == key && telemetry_batch[j].1 == value {
        already_added = true
        break
      }
      j = j + 1
    }
    
    if !already_added {
      // 添加键值对和计数
      let compressed_entry = key + "=" + value + ":" + count.to_string()
      key_value_counts[compressed_entry] = count
    }
    
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(key_value_counts.size(), 7)  // 7个唯一的键值对
  
  // 验证计数正确性
  assert_eq(key_value_counts.contains("service.name=auth-service:2"), true)
  assert_eq(key_value_counts.contains("http.method=GET:3"), true)
  assert_eq(key_value_counts.contains("service.name=user-service:1"), true)
}

test "telemetry_delta_compression" {
  // 测试增量压缩（只存储变化的部分）
  
  let telemetry_snapshots = [
    [("cpu", 45.0), ("memory", 60.2), ("disk", 30.5)],
    [("cpu", 48.0), ("memory", 61.1), ("disk", 30.5)],
    [("cpu", 52.0), ("memory", 62.3), ("disk", 31.0)],
    [("cpu", 49.0), ("memory", 62.3), ("disk", 31.0)],
    [("cpu", 46.0), ("memory", 63.0), ("disk", 31.2)]
  ]
  
  // 验证原始数据
  assert_eq(telemetry_snapshots.length(), 5)
  
  // 增量压缩：只存储相对于上一个快照的变化
  let delta_snapshots = []
  
  // 第一个快照完整存储
  delta_snapshots.push(telemetry_snapshots[0])
  
  let mut i = 1
  while i < telemetry_snapshots.length() {
    let previous = telemetry_snapshots[i - 1]
    let current = telemetry_snapshots[i]
    let mut delta = []
    
    let mut j = 0
    while j < current.length() {
      let metric_name = current[j].0
      let current_value = current[j].1
      
      // 找到上一个快照中的对应值
      let mut previous_value = 0.0
      let mut found = false
      let mut k = 0
      while k < previous.length() {
        if previous[k].0 == metric_name {
          previous_value = previous[k].1
          found = true
          break
        }
        k = k + 1
      }
      
      // 如果值发生变化，则存储变化
      if !found || current_value != previous_value {
        delta.push((metric_name, current_value))
      }
      
      j = j + 1
    }
    
    delta_snapshots.push(delta)
    i = i + 1
  }
  
  // 验证增量压缩效果
  assert_eq(delta_snapshots.length(), 5)
  assert_eq(delta_snapshots[0].length(), 3)  // 第一个快照完整
  assert_eq(delta_snapshots[1].length(), 2)  // cpu和memory变化
  assert_eq(delta_snapshots[2].length(), 3)  // 全部变化
  assert_eq(delta_snapshots[3].length(), 1)  // 只有cpu变化
  assert_eq(delta_snapshots[4].length(), 2)  // cpu和memory变化
}

test "telemetry_huffman_compression" {
  // 测试霍夫曼压缩概念（简化版本）
  
  let telemetry_symbols = [
    "GET", "GET", "POST", "GET", "PUT", "GET", "DELETE", "GET",
    "POST", "GET", "GET", "PATCH", "GET", "POST", "GET", "GET"
  ]
  
  // 验证原始数据
  assert_eq(telemetry_symbols.length(), 16)
  
  // 统计符号频率
  let symbol_counts = {}
  let mut i = 0
  while i < telemetry_symbols.length() {
    let symbol = telemetry_symbols[i]
    let mut count = 0
    
    let mut j = 0
    while j < telemetry_symbols.length() {
      if telemetry_symbols[j] == symbol {
        count = count + 1
      }
      j = j + 1
    }
    
    // 只添加一次
    let mut already_added = false
    j = 0
    while j < i {
      if telemetry_symbols[j] == symbol {
        already_added = true
        break
      }
      j = j + 1
    }
    
    if !already_added {
      symbol_counts[symbol] = count
    }
    
    i = i + 1
  }
  
  // 验证频率统计
  assert_eq(symbol_counts.size(), 5)  // 5个不同的符号
  assert_eq(symbol_counts["GET"], 10)  // GET出现10次
  assert_eq(symbol_counts["POST"], 3)  // POST出现3次
  assert_eq(symbol_counts["PUT"], 1)   // PUT出现1次
  assert_eq(symbol_counts["DELETE"], 1) // DELETE出现1次
  assert_eq(symbol_counts["PATCH"], 1)  // PATCH出现1次
  
  // 分配编码长度（频率越高的符号编码越短）
  let symbol_codes = {}
  symbol_codes["GET"] = "0"      // 最高频率，最短编码
  symbol_codes["POST"] = "10"    // 次高频率
  symbol_codes["PUT"] = "110"    // 低频率
  symbol_codes["DELETE"] = "1110" // 低频率
  symbol_codes["PATCH"] = "1111"  // 低频率
  
  // 计算压缩后总长度
  let mut compressed_length = 0
  i = 0
  while i < telemetry_symbols.length() {
    let symbol = telemetry_symbols[i]
    let code = symbol_codes[symbol]
    compressed_length = compressed_length + code.length()
    i = i + 1
  }
  
  // 原始长度（假设每个符号用8位表示）
  let original_length = telemetry_symbols.length() * 8
  
  // 验证压缩效果
  assert_eq(compressed_length < original_length, true)
  
  // 计算压缩率
  let compression_ratio = compressed_length.to_double() / original_length.to_double()
  assert_eq(compression_ratio > 0.3, true)
  assert_eq(compression_ratio < 0.5, true)
}