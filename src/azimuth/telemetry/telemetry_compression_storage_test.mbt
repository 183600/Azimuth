// 遥测数据压缩存储测试用例

test "telemetry_run_length_encoding" {
  // 测试游程编码压缩
  
  let metric_values = [10, 10, 10, 15, 15, 20, 20, 20, 20, 25, 25, 30]
  
  // 游程编码实现
  let mut rle_compressed = []
  if metric_values.length() > 0 {
    let mut current_value = metric_values[0]
    let mut count = 1
    
    let mut i = 1
    while i < metric_values.length() {
      if metric_values[i] == current_value {
        count = count + 1
      } else {
        rle_compressed = rle_compressed.push((current_value, count))
        current_value = metric_values[i]
        count = 1
      }
      i = i + 1
    }
    rle_compressed = rle_compressed.push((current_value, count))
  }
  
  // 验证游程编码结果
  assert_eq(rle_compressed.length(), 6)  // 应该有6个不同的值
  
  // 验证第一个值：10重复3次
  assert_eq(rle_compressed[0].0, 10)
  assert_eq(rle_compressed[0].1, 3)
  
  // 验证最后一个值：30重复1次
  assert_eq(rle_compressed[5].0, 30)
  assert_eq(rle_compressed[5].1, 1)
  
  // 计算压缩率
  let original_size = metric_values.length()
  let compressed_size = rle_compressed.length() * 2  // 每个编码包含值和计数
  let compression_ratio = original_size.to_double() / compressed_size.to_double()
  
  assert_eq(compression_ratio > 1.0, true)  // 应该有压缩效果
}

test "telemetry_delta_encoding" {
  // 测试增量编码压缩
  
  let time_series = [100, 102, 105, 107, 110, 112, 115, 118, 120, 123]
  
  // 增量编码实现
  let mut delta_encoded = []
  if time_series.length() > 0 {
    delta_encoded = delta_encoded.push(time_series[0])  // 第一个值保持不变
    
    let mut i = 1
    while i < time_series.length() {
      let delta = time_series[i] - time_series[i - 1]
      delta_encoded = delta_encoded.push(delta)
      i = i + 1
    }
  }
  
  // 验证增量编码结果
  assert_eq(delta_encoded.length(), time_series.length())
  assert_eq(delta_encoded[0], 100)  // 第一个值保持不变
  
  // 验证增量值
  assert_eq(delta_encoded[1], 2)   // 102 - 100
  assert_eq(delta_encoded[2], 3)   // 105 - 102
  assert_eq(delta_encoded[3], 2)   // 107 - 105
  
  // 验证解码（反向操作）
  let mut decoded = []
  if delta_encoded.length() > 0 {
    decoded = decoded.push(delta_encoded[0])
    
    let mut i = 1
    while i < delta_encoded.length() {
      let prev_value = decoded[i - 1]
      let current_value = prev_value + delta_encoded[i]
      decoded = decoded.push(current_value)
      i = i + 1
    }
  }
  
  // 验证解码结果与原始数据一致
  assert_eq(decoded.length(), time_series.length())
  let mut i = 0
  while i < decoded.length() {
    assert_eq(decoded[i], time_series[i])
    i = i + 1
  }
}

test "telemetry_dictionary_compression" {
  // 测试字典压缩
  
  let log_entries = [
    ("GET", "/api/users", 200),
    ("POST", "/api/orders", 201),
    ("GET", "/api/users", 200),
    ("PUT", "/api/users/123", 200),
    ("GET", "/api/users", 200),
    ("DELETE", "/api/orders/456", 204),
    ("GET", "/api/products", 200),
    ("POST", "/api/orders", 201),
    ("GET", "/api/users", 200),
    ("GET", "/api/products", 200)
  ]
  
  // 构建字典
  let mut dictionary = @hashmap.new()
  let mut dict_counter = 0
  
  let mut i = 0
  while i < log_entries.length() {
    let method = log_entries[i].0
    let path = log_entries[i].1
    
    if !dictionary.contains(method) {
      dictionary[method] = dict_counter
      dict_counter = dict_counter + 1
    }
    
    if !dictionary.contains(path) {
      dictionary[path] = dict_counter
      dict_counter = dict_counter + 1
    }
    i = i + 1
  }
  
  // 压缩数据
  let mut compressed_entries = []
  i = 0
  while i < log_entries.length() {
    let method = log_entries[i].0
    let path = log_entries[i].1
    let status = log_entries[i].2
    
    let method_id = dictionary[method]
    let path_id = dictionary[path]
    
    compressed_entries = compressed_entries.push((method_id, path_id, status))
    i = i + 1
  }
  
  // 验证字典压缩结果
  assert_eq(compressed_entries.length(), log_entries.length())
  assert_eq(dictionary.keys().length(), 5)  // 应该有5个不同的字符串
  
  // 验证压缩数据
  assert_eq(compressed_entries[0].0, dictionary["GET"])      // GET方法ID
  assert_eq(compressed_entries[0].1, dictionary["/api/users"])  // 路径ID
  assert_eq(compressed_entries[0].2, 200)                   // 状态码保持不变
  
  // 计算压缩效果
  let original_chars = 0
  let mut i = 0
  while i < log_entries.length() {
    original_chars = original_chars + log_entries[i].0.length() + log_entries[i].1.length()
    i = i + 1
  }
  
  let compressed_size = compressed_entries.length() * 3  // 每个条目3个整数
  assert_eq(compressed_size < original_chars, true)  // 压缩后应该更小
}

test "telemetry_time_series_compression" {
  // 测试时间序列压缩
  
  let metrics = [
    (1000, 10.5),
    (2000, 10.7),
    (3000, 10.6),
    (4000, 10.8),
    (5000, 15.2),  // 显著变化
    (6000, 15.3),
    (7000, 15.1),
    (8000, 15.4),
    (9000, 8.9),   // 显著变化
    (10000, 9.1)
  ]
  
  let threshold = 0.5  // 变化阈值
  
  // 时间序列压缩：只在显著变化时记录
  let mut compressed = []
  if metrics.length() > 0 {
    compressed = compressed.push(metrics[0])  // 总是记录第一个点
    
    let mut i = 1
    while i < metrics.length() {
      let prev_value = metrics[i - 1].1
      let curr_value = metrics[i].1
      let change = @abs(curr_value - prev_value)
      
      if change > threshold {
        compressed = compressed.push(metrics[i])
      }
      i = i + 1
    }
  }
  
  // 验证时间序列压缩结果
  assert_eq(compressed.length() < metrics.length())  // 压缩后应该更少
  assert_eq(compressed.length(), 4)  // 应该有4个关键点
  
  // 验证关键点
  assert_eq(compressed[0].0, 1000)  // 第一个点
  assert_eq(compressed[0].1, 10.5)
  
  assert_eq(compressed[1].0, 5000)  // 第一个显著变化
  assert_eq(compressed[1].1, 15.2)
  
  assert_eq(compressed[2].0, 9000)  // 第二个显著变化
  assert_eq(compressed[2].1, 8.9)
  
  assert_eq(compressed[3].0, 10000)  // 最后一个点
  assert_eq(compressed[3].1, 9.1)
  
  // 计算压缩率
  let compression_ratio = metrics.length().to_double() / compressed.length().to_double()
  assert_eq(compression_ratio > 2.0, true)  // 压缩率应该大于2
}

test "telemetry_batch_compression" {
  // 测试批量压缩
  
  let batch_data = [
    "metric_1:10.5", "metric_1:10.7", "metric_1:10.6",
    "metric_2:25.3", "metric_2:25.4", "metric_2:25.2",
    "metric_1:10.8", "metric_1:10.9", "metric_1:11.0",
    "metric_3:5.1", "metric_3:5.2", "metric_3:5.0"
  ]
  
  // 批量压缩：按metric名称分组
  let mut grouped_data = @hashmap.new()
  
  let mut i = 0
  while i < batch_data.length() {
    let entry = batch_data[i]
    let mut colon_pos = 0
    
    // 找到冒号位置
    let mut j = 0
    while j < entry.length() {
      if entry[j] == ':' {
        colon_pos = j
        break
      }
      j = j + 1
    }
    
    let metric_name = entry[..colon_pos]
    let metric_value = entry[(colon_pos + 1)..]
    
    if grouped_data.contains(metric_name) {
      let values = grouped_data[metric_name]
      grouped_data[metric_name] = values.push(metric_value)
    } else {
      grouped_data[metric_name] = [metric_value]
    }
    i = i + 1
  }
  
  // 验证批量压缩结果
  assert_eq(grouped_data.keys().length(), 3)  // 应该有3个不同的metric
  
  // 验证metric_1的分组
  assert_eq(grouped_data["metric_1"].length(), 6)
  assert_eq(grouped_data["metric_1"][0], "10.5")
  assert_eq(grouped_data["metric_1"][5], "11.0")
  
  // 验证metric_2的分组
  assert_eq(grouped_data["metric_2"].length(), 3)
  assert_eq(grouped_data["metric_2"][0], "25.3")
  assert_eq(grouped_data["metric_2"][2], "25.2")
  
  // 验证metric_3的分组
  assert_eq(grouped_data["metric_3"].length(), 3)
  assert_eq(grouped_data["metric_3"][0], "5.1")
  assert_eq(grouped_data["metric_3"][2], "5.0")
  
  // 计算压缩效果：减少重复的metric名称存储
  let original_size = 0
  let mut i = 0
  while i < batch_data.length() {
    original_size = original_size + batch_data[i].length()
    i = i + 1
  }
  
  let compressed_size = 0
  let keys = grouped_data.keys()
  let mut i = 0
  while i < keys.length() {
    let key = keys[i]
    compressed_size = compressed_size + key.length()  // metric名称
    let values = grouped_data[key]
    let mut j = 0
    while j < values.length() {
      compressed_size = compressed_size + values[j].length()  // 只存储值
      j = j + 1
    }
    i = i + 1
  }
  
  assert_eq(compressed_size < original_size, true)  // 压缩后应该更小
}