// 遥测数据压缩测试用例

test "telemetry_string_compression" {
  // 测试遥测字符串压缩
  
  let original_strings = [
    "service.name:payment-api",
    "http.method:GET",
    "http.status_code:200",
    "trace.id:0af7651916cd43dd8448eb211c80319c",
    "span.id:b7ad6b7169203331",
    "user.id:12345",
    "request.duration:125.5",
    "db.query_time:45.2"
  ]
  
  // 模拟压缩过程：移除重复前缀
  let mut compressed_strings = []
  let mut i = 0
  while i < original_strings.length() {
    let original = original_strings[i]
    let mut compressed = original
    
    // 移除常见前缀以压缩
    if compressed.has_prefix("service.name:") {
      compressed = "s:" + compressed.substring(13)
    } else if compressed.has_prefix("http.method:") {
      compressed = "hm:" + compressed.substring(12)
    } else if compressed.has_prefix("http.status_code:") {
      compressed = "hs:" + compressed.substring(17)
    } else if compressed.has_prefix("trace.id:") {
      compressed = "t:" + compressed.substring(10)
    } else if compressed.has_prefix("span.id:") {
      compressed = "s:" + compressed.substring(9)
    } else if compressed.has_prefix("user.id:") {
      compressed = "u:" + compressed.substring(9)
    } else if compressed.has_prefix("request.duration:") {
      compressed = "rd:" + compressed.substring(17)
    } else if compressed.has_prefix("db.query_time:") {
      compressed = "dqt:" + compressed.substring(15)
    }
    
    compressed_strings.push(compressed)
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(compressed_strings.length(), original_strings.length())
  assert_eq(compressed_strings[0], "s:payment-api")
  assert_eq(compressed_strings[1], "hm:GET")
  assert_eq(compressed_strings[2], "hs:200")
  assert_eq(compressed_strings[3], "t:0af7651916cd43dd8448eb211c80319c")
  
  // 计算压缩率
  let mut original_length = 0
  let mut compressed_length = 0
  i = 0
  while i < original_strings.length() {
    original_length = original_length + original_strings[i].length()
    compressed_length = compressed_length + compressed_strings[i].length()
    i = i + 1
  }
  
  let compression_ratio = (original_length.to_double() - compressed_length.to_double()) / original_length.to_double() * 100.0
  assert_eq(compression_ratio > 10.0, true)
  assert_eq(compression_ratio < 50.0, true)
}

test "telemetry_numeric_compression" {
  // 测试遥测数值压缩
  
  let numeric_values = [125.5, 45.2, 78.9, 156.8, 23.4, 89.7, 12.1, 234.5]
  
  // 使用差分编码压缩
  let mut compressed_values = []
  let mut previous_value = 0.0
  
  let mut i = 0
  while i < numeric_values.length() {
    let current_value = numeric_values[i]
    let difference = current_value - previous_value
    compressed_values.push(difference)
    previous_value = current_value
    i = i + 1
  }
  
  // 验证差分编码
  assert_eq(compressed_values.length(), numeric_values.length())
  assert_eq(compressed_values[0], 125.5)  // 第一个值保持原样
  assert_eq(compressed_values[1], 45.2 - 125.5)  // 差值
  assert_eq(compressed_values[2], 78.9 - 45.2)   // 差值
  
  // 解压缩验证
  let mut decompressed_values = []
  let mut accumulated_value = 0.0
  
  i = 0
  while i < compressed_values.length() {
    accumulated_value = accumulated_value + compressed_values[i]
    decompressed_values.push(accumulated_value)
    i = i + 1
  }
  
  // 验证解压缩结果
  assert_eq(decompressed_values.length(), numeric_values.length())
  assert_eq(decompressed_values[0], numeric_values[0])
  assert_eq(decompressed_values[3], numeric_values[3])
  assert_eq(decompressed_values[7], numeric_values[7])
}

test "telemetry_batch_compression" {
  // 测试遥测批次压缩
  
  let telemetry_batch = [
    {"service": "api", "endpoint": "/users", "method": "GET", "status": 200, "duration": 125.5},
    {"service": "api", "endpoint": "/users", "method": "POST", "status": 201, "duration": 156.8},
    {"service": "api", "endpoint": "/products", "method": "GET", "status": 200, "duration": 89.2},
    {"service": "api", "endpoint": "/users", "method": "GET", "status": 404, "duration": 23.4},
    {"service": "db", "endpoint": "query", "method": "SELECT", "status": 200, "duration": 45.6}
  ]
  
  // 按服务分组压缩
  let mut service_groups = []
  
  let mut i = 0
  while i < telemetry_batch.length() {
    let record = telemetry_batch[i]
    let service_name = record["service"]
    
    // 查找或创建服务组
    let mut group_found = false
    let mut j = 0
    while j < service_groups.length() {
      if service_groups[j].0 == service_name {
        service_groups[j].1.push(record)
        group_found = true
        break
      }
      j = j + 1
    }
    
    // 如果服务组不存在，创建新组
    if not group_found {
      service_groups.push((service_name, [record]))
    }
    
    i = i + 1
  }
  
  // 验证分组结果
  assert_eq(service_groups.length(), 2)  // api和db两个服务
  
  // 验证api服务组
  let mut api_group = ("", [])
  i = 0
  while i < service_groups.length() {
    if service_groups[i].0 == "api" {
      api_group = service_groups[i]
      break
    }
    i = i + 1
  }
  assert_eq(api_group.1.length(), 4)
  
  // 验证db服务组
  let mut db_group = ("", [])
  i = 0
  while i < service_groups.length() {
    if service_groups[i].0 == "db" {
      db_group = service_groups[i]
      break
    }
    i = i + 1
  }
  assert_eq(db_group.1.length(), 1)
}

test "telemetry_dictionary_compression" {
  // 测试遥测字典压缩
  
  let telemetry_attributes = [
    ("service.name", "payment-api"),
    ("service.version", "1.2.3"),
    ("deployment.environment", "production"),
    ("host.name", "web-server-01"),
    ("service.name", "payment-api"),
    ("http.method", "GET"),
    ("http.status_code", "200"),
    ("service.name", "payment-api"),
    ("deployment.environment", "production"),
    ("http.method", "POST")
  ]
  
  // 创建字典
  let mut dictionary = []
  let mut compressed_indices = []
  
  let mut i = 0
  while i < telemetry_attributes.length() {
    let key = telemetry_attributes[i].0
    let value = telemetry_attributes[i].1
    let combined = key + "=" + value
    
    // 查找字典中是否存在
    let mut found_index = -1
    let mut j = 0
    while j < dictionary.length() {
      if dictionary[j] == combined {
        found_index = j
        break
      }
      j = j + 1
    }
    
    // 如果不存在，添加到字典
    if found_index == -1 {
      dictionary.push(combined)
      found_index = dictionary.length() - 1
    }
    
    compressed_indices.push(found_index)
    i = i + 1
  }
  
  // 验证字典压缩结果
  assert_eq(dictionary.length(), 6)  // 6个唯一值
  assert_eq(compressed_indices.length(), telemetry_attributes.length())
  
  // 验证字典内容
  assert_eq(dictionary[0], "service.name=payment-api")
  assert_eq(dictionary[1], "service.version=1.2.3")
  assert_eq(dictionary[2], "deployment.environment=production")
  
  // 验证压缩索引
  assert_eq(compressed_indices[0], 0)  // service.name=payment-api
  assert_eq(compressed_indices[1], 1)  // service.version=1.2.3
  assert_eq(compressed_indices[3], 3)  // host.name=web-server-01
  assert_eq(compressed_indices[4], 0)  // service.name=payment-api (重复)
  assert_eq(compressed_indices[9], 5)  // http.method=POST
}

test "telemetry_time_series_compression" {
  // 测试遥测时间序列压缩
  
  let time_series_data = [
    (1640995200L, 25.5),
    (1640995260L, 27.8),
    (1640995320L, 23.2),
    (1640995380L, 29.1),
    (1640995440L, 26.7),
    (1640995500L, 24.9),
    (1640995560L, 28.3),
    (1640995620L, 25.1)
  ]
  
  // 时间间隔压缩（固定间隔）
  let time_interval = 60L  // 60秒间隔
  let mut compressed_data = []
  
  let mut i = 0
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    let value = time_series_data[i].1
    
    // 计算相对时间戳
    let base_timestamp = time_series_data[0].0
    let relative_time = (timestamp - base_timestamp) / time_interval
    
    compressed_data.push((relative_time, value))
    i = i + 1
  }
  
  // 验证时间序列压缩
  assert_eq(compressed_data.length(), time_series_data.length())
  assert_eq(compressed_data[0].0, 0L)    // 基准时间
  assert_eq(compressed_data[1].0, 1L)    // 60秒后
  assert_eq(compressed_data[2].0, 2L)    // 120秒后
  assert_eq(compressed_data[7].0, 7L)    // 420秒后
  
  // 验证数值保持不变
  assert_eq(compressed_data[0].1, 25.5)
  assert_eq(compressed_data[3].1, 29.1)
  assert_eq(compressed_data[7].1, 25.1)
  
  // 计算压缩率（时间戳从64位减少到8位）
  let original_size = time_series_data.length() * (8 + 8)  // 时间戳(8) + 数值(8)
  let compressed_size = time_series_data.length() * (1 + 8)  // 相对时间(1) + 数值(8)
  let compression_ratio = (original_size.to_double() - compressed_size.to_double()) / original_size.to_double() * 100.0
  
  assert_eq(compression_ratio > 40.0, true)
  assert_eq(compression_ratio < 50.0, true)
}

test "telemetry_compression_performance" {
  // 测试遥测压缩性能
  
  let large_dataset_size = 1000
  let mut large_dataset = []
  
  // 生成大数据集
  let mut i = 0
  while i < large_dataset_size {
    let record = "service.api.endpoint." + i.to_string() + ".duration:" + (i.to_double() * 1.5).to_string()
    large_dataset.push(record)
    i = i + 1
  }
  
  // 简单压缩：移除公共前缀
  let common_prefix = "service.api.endpoint."
  let mut compressed_dataset = []
  
  i = 0
  while i < large_dataset.length() {
    let record = large_dataset[i]
    let compressed = record.replace(common_prefix, "s.e.")
    compressed_dataset.push(compressed)
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(compressed_dataset.length(), large_dataset.length())
  assert_eq(compressed_dataset[0].has_prefix("s.e."), true)
  assert_eq(compressed_dataset[0].contains(".duration:"), true)
  
  // 计算压缩统计
  let mut original_total_length = 0
  let mut compressed_total_length = 0
  
  i = 0
  while i < large_dataset.length() {
    original_total_length = original_total_length + large_dataset[i].length()
    compressed_total_length = compressed_total_length + compressed_dataset[i].length()
    i = i + 1
  }
  
  let space_saved = original_total_length - compressed_total_length
  let compression_ratio = space_saved.to_double() / original_total_length.to_double() * 100.0
  
  // 验证压缩效果
  assert_eq(space_saved > 0, true)
  assert_eq(compression_ratio > 20.0, true)
  assert_eq(compression_ratio < 40.0, true)
  
  // 验证数据集大小
  assert_eq(large_dataset_size, 1000)
  assert_eq(original_total_length > compressed_total_length, true)
}