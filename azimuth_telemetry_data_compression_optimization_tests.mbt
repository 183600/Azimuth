// Azimuth 遥测数据压缩优化测试用例
// 专注于测试遥测数据的压缩算法、存储优化和传输效率

// 测试1: 基本字符串压缩算法
test "基本字符串压缩算法" {
  // 创建重复性高的遥测数据
  let original_data = "service=auth,region=us-east-1,version=1.2.3,status=ok;service=auth,region=us-east-1,version=1.2.3,status=ok;service=api,region=us-east-1,version=1.2.3,status=ok"
  
  // 模拟简单压缩：替换重复字符串
  let compression_map = [
    ("service=auth", "A"),
    ("region=us-east-1", "B"),
    ("version=1.2.3", "C"),
    ("status=ok", "D"),
    ("service=api", "E")
  ]
  
  // 应用压缩
  let compressed_data = compression_map.fold(original_data, fn(acc, mapping) {
    acc.replace(mapping.0, mapping.1)
  })
  
  // 验证压缩效果
  assert_true(compressed_data.length() < original_data.length())
  assert_true(compressed_data.contains("A"))  // service=auth 被压缩为 A
  assert_true(compressed_data.contains("B"))  // region=us-east-1 被压缩为 B
  
  // 模拟解压缩
  let decompressed_data = compression_map.fold(compressed_data, fn(acc, mapping) {
    acc.replace(mapping.1, mapping.0)
  })
  
  // 验证解压缩后的数据完整性
  assert_eq(decompressed_data, original_data)
}

// 测试2: 数值数据压缩
test "数值数据压缩" {
  // 创建时间序列数值数据
  let metric_values = [100, 102, 105, 103, 108, 110, 107, 112, 115, 113]
  
  // 使用增量编码压缩
  let delta_encoded = [metric_values[0]] + metric_values.slice(1).map_with_index(fn(i, value) {
    value - metric_values[i]
  })
  
  // 验证增量编码
  assert_eq(delta_encoded[0], 100)
  assert_eq(delta_encoded[1], 2)   // 102 - 100
  assert_eq(delta_encoded[2], 3)   // 105 - 102
  assert_eq(delta_encoded[3], -2)  // 103 - 105
  
  // 验证增量编码后的数值范围更小（更容易压缩）
  let original_range = metric_values.max() - metric_values.min()
  let delta_range = delta_encoded.max() - delta_encoded.min()
  
  assert_eq(original_range, 15)  // 115 - 100
  assert_eq(delta_range, 8)     // 5 - (-3)
  assert_true(delta_range < original_range)
  
  // 解压缩
  let decompressed = delta_encoded.slice(1).fold([delta_encoded[0]], fn(acc, delta) {
    acc + [acc[acc.length() - 1] + delta]
  })
  
  // 验证解压缩后的数据
  assert_eq(decompressed.length(), metric_values.length())
  assert_true(decompressed.all_with_index(fn(i, value) { value == metric_values[i] }))
}

// 测试3: 属性键值对压缩
test "属性键值对压缩" {
  // 创建重复的属性键值对
  let attributes = [
    ("service.name", "authentication-service"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production"),
    ("host.name", "auth-server-01"),
    ("service.name", "authentication-service"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production"),
    ("host.name", "auth-server-02")
  ]
  
  // 提取唯一的键
  let unique_keys = attributes.map(fn(attr) { attr.0 }).to_set().to_array()
  assert_eq(unique_keys.length(), 4)
  
  // 为每个键分配索引
  let key_indices = [
    ("service.name", 0),
    ("service.version", 1),
    ("deployment.environment", 2),
    ("host.name", 3)
  ]
  
  // 压缩属性：用索引替换键
  let compressed_attrs = attributes.map(fn(attr) {
    match key_indices.find(fn(ki) { ki.0 == attr.0 }) {
      Some(ki) => (ki.1.to_string(), attr.1)
      None => (attr.0, attr.1)
    }
  })
  
  // 验证压缩效果
  assert_eq(compressed_attrs.length(), attributes.length())
  assert_eq(compressed_attrs[0].0, "0")  // service.name -> 0
  assert_eq(compressed_attrs[1].0, "1")  // service.version -> 1
  
  // 验证值保持不变
  assert_eq(compressed_attrs[0].1, "authentication-service")
  assert_eq(compressed_attrs[1].1, "1.2.3")
}

// 测试4: 时间戳压缩
test "时间戳压缩" {
  // 创建连续的时间戳
  let timestamps = [
    1640995200000L,  // 2022-01-01 00:00:00
    1640995201000L,  // 2022-01-01 00:00:01
    1640995202000L,  // 2022-01-01 00:00:02
    1640995203000L,  // 2022-01-01 00:00:03
    1640995204000L   // 2022-01-01 00:00:04
  ]
  
  // 使用基准时间 + 偏移量压缩
  let base_timestamp = timestamps[0]
  let compressed_timestamps = timestamps.map(fn(ts) { ts - base_timestamp })
  
  // 验证压缩效果
  assert_eq(compressed_timestamps[0], 0L)
  assert_eq(compressed_timestamps[1], 1000L)
  assert_eq(compressed_timestamps[2], 2000L)
  assert_eq(compressed_timestamps[3], 3000L)
  assert_eq(compressed_timestamps[4], 4000L)
  
  // 验证压缩后的数值范围更小
  let original_max = timestamps.max()
  let compressed_max = compressed_timestamps.max()
  
  assert_true(compressed_max < original_max)
  assert_eq(compressed_max, 4000L)
  
  // 解压缩
  let decompressed_timestamps = compressed_timestamps.map(fn(offset) { 
    base_timestamp + offset 
  })
  
  // 验证解压缩后的时间戳
  assert_true(decompressed_timestamps.all_with_index(fn(i, ts) { 
    ts == timestamps[i] 
  }))
}

// 测试5: 批量数据压缩
test "批量数据压缩" {
  // 创建大量遥测数据点
  let data_points = [
    {timestamp: 1640995200000L, metric_name: "http.requests", value: 100.0, tags: [("service", "auth"), ("method", "POST")]},
    {timestamp: 1640995201000L, metric_name: "http.requests", value: 105.0, tags: [("service", "auth"), ("method", "POST")]},
    {timestamp: 1640995202000L, metric_name: "http.requests", value: 98.0, tags: [("service", "auth"), ("method", "GET")]},
    {timestamp: 1640995203000L, metric_name: "http.requests", value: 102.0, tags: [("service", "api"), ("method", "GET")]},
    {timestamp: 1640995204000L, metric_name: "http.requests", value: 110.0, tags: [("service", "api"), ("method", "POST")]}
  ]
  
  // 提取唯一的metric_name和tags
  let unique_metric_names = data_points.map(fn(dp) { dp.metric_name }).to_set().to_array()
  let unique_tags = data_points.map(fn(dp) { dp.tags }).to_set().to_array()
  
  // 验证唯一值提取
  assert_eq(unique_metric_names.length(), 1)  // 只有 "http.requests"
  assert_eq(unique_tags.length(), 3)  // 三种不同的tag组合
  
  // 创建压缩映射
  let metric_name_map = [("http.requests", "M1")]
  let tags_map = [
    ([("service", "auth"), ("method", "POST")], "T1"),
    ([("service", "auth"), ("method", "GET")], "T2"),
    ([("service", "api"), ("method", "GET")], "T3"),
    ([("service", "api"), ("method", "POST")], "T4")
  ]
  
  // 压缩数据点
  let compressed_points = data_points.map(fn(dp) {
    let compressed_metric_name = match metric_name_map.find(fn(m) { m.0 == dp.metric_name }) {
      Some(m) => m.1
      None => dp.metric_name
    }
    
    let compressed_tags = match tags_map.find_fn(fn(m) { 
      m.0.length() == dp.tags.length() && 
      m.0.all_with_index(fn(i, tag) { tag == dp.tags[i] })
    }) {
      Some(m) => m.1
      None => ""
    }
    
    {
      timestamp: dp.timestamp - 1640995200000L,  // 基准时间压缩
      metric_name: compressed_metric_name,
      value: dp.value,
      tags: compressed_tags
    }
  })
  
  // 验证压缩效果
  assert_eq(compressed_points[0].metric_name, "M1")
  assert_eq(compressed_points[0].tags, "T1")
  assert_eq(compressed_points[0].timestamp, 0L)
  
  assert_eq(compressed_points[1].metric_name, "M1")
  assert_eq(compressed_points[1].tags, "T1")
  assert_eq(compressed_points[1].timestamp, 1000L)
  
  // 计算压缩率（简化计算）
  let original_size = data_points.fold(0, fn(acc, dp) { 
    acc + dp.metric_name.length() + dp.tags.length() * 20  // 估算大小
  })
  
  let compressed_size = compressed_points.fold(0, fn(acc, dp) { 
    acc + dp.metric_name.length() + dp.tags.length()
  })
  
  // 验证压缩效果
  assert_true(compressed_size < original_size)
}

// 测试6: 自适应压缩策略
test "自适应压缩策略" {
  // 创建不同类型的遥测数据
  let high_repetition_data = [
    "service=auth,region=us-east-1,status=ok",
    "service=auth,region=us-east-1,status=ok",
    "service=auth,region=us-east-1,status=ok",
    "service=auth,region=us-east-1,status=ok"
  ]
  
  let low_repetition_data = [
    "unique_metric_1=value1",
    "unique_metric_2=value2",
    "unique_metric_3=value3",
    "unique_metric_4=value4"
  ]
  
  let numeric_data = ["100", "101", "102", "103", "104", "105"]
  
  // 分析数据特征并选择压缩策略
  let high_repetition_ratio = high_repetition_data.to_set().to_array().length().to_double() / high_repetition_data.length().to_double()
  let low_repetition_ratio = low_repetition_data.to_set().to_array().length().to_double() / low_repetition_data.length().to_double()
  
  // 验证重复率分析
  assert_eq(high_repetition_ratio, 0.25)  // 只有1/4是唯一的
  assert_eq(low_repetition_ratio, 1.0)    // 全部是唯一的
  
  // 根据数据特征选择压缩策略
  let high_rep_compression = if high_repetition_ratio < 0.5 {
    // 字典压缩适合高重复数据
    "dictionary"
  } else {
    "generic"
  }
  
  let low_rep_compression = if low_repetition_ratio > 0.8 {
    // 通用压缩适合低重复数据
    "generic"
  } else {
    "dictionary"
  }
  
  let numeric_compression = "delta"  // 增量压缩适合数值数据
  
  // 验证压缩策略选择
  assert_eq(high_rep_compression, "dictionary")
  assert_eq(low_rep_compression, "generic")
  assert_eq(numeric_compression, "delta")
  
  // 应用相应的压缩策略
  let compressed_high_rep = high_repetition_data.map(fn(item) { 
    if item == "service=auth,region=us-east-1,status=ok" { "A" } else { item }
  })
  
  let compressed_numeric = numeric_data.slice(1).fold([numeric_data[0]], fn(acc, value) {
    let prev = acc[acc.length() - 1]
    let delta = value.to_int() - prev.to_int()
    acc + [delta.to_string()]
  })
  
  // 验证压缩效果
  assert_true(compressed_high_rep.all(fn(item) { item == "A" }))
  assert_eq(compressed_numeric[1], "1")   // 101 - 100
  assert_eq(compressed_numeric[2], "1")   // 102 - 101
  assert_eq(compressed_numeric[3], "1")   // 103 - 102
}