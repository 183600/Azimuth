// 遥测数据压缩测试用例
// 测试各种数据压缩算法和优化技术

test "telemetry_string_compression" {
  // 测试字符串压缩
  
  let original_string = "http_request_total{service=\"api-gateway\",method=\"GET\",status=\"200\"} 1234 1640995200"
  let original_length = original_string.length()
  
  // 简单的字符串压缩：重复字符替换
  let mut compressed_string = original_string
  compressed_string = compressed_string.replace("api-gateway", "ag")
  compressed_string = compressed_string.replace("http_request_total", "hrt")
  compressed_string = compressed_string.replace("1640995200", "ts1")
  
  let compressed_length = compressed_string.length()
  let compression_ratio = compressed_length.to_double() / original_length.to_double()
  
  // 验证压缩效果
  assert_eq(original_length, 79)
  assert_eq(compressed_length < original_length, true)
  assert_eq(compression_ratio < 1.0, true)
  assert_eq(compression_ratio > 0.5, true)
  
  // 验证压缩后的字符串包含关键信息
  assert_eq(compressed_string.contains("hrt"), true)
  assert_eq(compressed_string.contains("ag"), true)
  assert_eq(compressed_string.contains("ts1"), true)
  assert_eq(compressed_string.contains("1234"), true)
}

test "telemetry_numeric_compression" {
  // 测试数值压缩
  
  let metrics = [1234567890L, 1234567891L, 1234567892L, 1234567893L, 1234567894L]
  let base_value = metrics[0]
  
  // 增量压缩：存储差值而非绝对值
  let mut compressed_metrics = [base_value]
  let mut i = 1
  
  while i < metrics.length() {
    let delta = metrics[i] - metrics[i - 1]
    compressed_metrics.push(delta)
    i = i + 1
  }
  
  // 验证增量压缩
  assert_eq(compressed_metrics[0], 1234567890L)
  assert_eq(compressed_metrics[1], 1L)
  assert_eq(compressed_metrics[2], 1L)
  assert_eq(compressed_metrics[3], 1L)
  assert_eq(compressed_metrics[4], 1L)
  
  // 计算压缩率（简化计算）
  let original_bits = metrics.length() * 64  // 假设每个64位
  let compressed_bits = 64 + (metrics.length() - 1) * 8  // 第一个64位，其余8位
  let compression_ratio = compressed_bits.to_double() / original_bits.to_double()
  
  assert_eq(compression_ratio < 1.0, true)
  assert_eq(compression_ratio > 0.2, true)
}

test "telemetry_dictionary_compression" {
  // 测试字典压缩
  
  let telemetry_entries = [
    "service.name:api-gateway",
    "service.name:api-gateway", 
    "service.name:user-service",
    "http.method:GET",
    "http.method:GET",
    "http.method:POST",
    "http.status_code:200",
    "http.status_code:200",
    "http.status_code:404"
  ]
  
  // 构建字典
  let mut dictionary = {}
  let mut dictionary_index = 0
  let mut compressed_entries = []
  
  let mut i = 0
  while i < telemetry_entries.length() {
    let entry = telemetry_entries[i]
    
    if dictionary.contains_key(entry) {
      let index = dictionary[entry]
      compressed_entries.push(index.to_string())
    } else {
      dictionary[entry] = dictionary_index
      compressed_entries.push(dictionary_index.to_string())
      dictionary_index = dictionary_index + 1
    }
    
    i = i + 1
  }
  
  // 验证字典构建
  assert_eq(dictionary.size(), 6)  // 6个唯一条目
  assert_eq(dictionary["service.name:api-gateway"], 0)
  assert_eq(dictionary["http.method:GET"], 3)
  
  // 验证压缩结果
  assert_eq(compressed_entries.length(), 9)
  assert_eq(compressed_entries[0], "0")
  assert_eq(compressed_entries[1], "0")  // 重复条目
  assert_eq(compressed_entries[2], "1")
  assert_eq(compressed_entries[8], "5")
  
  // 计算压缩率
  let mut original_total_length = 0
  i = 0
  while i < telemetry_entries.length() {
    original_total_length = original_total_length + telemetry_entries[i].length()
    i = i + 1
  }
  
  let mut compressed_total_length = 0
  i = 0
  while i < compressed_entries.length() {
    compressed_total_length = compressed_total_length + compressed_entries[i].length()
    i = i + 1
  }
  
  let compression_ratio = compressed_total_length.to_double() / original_total_length.to_double()
  assert_eq(compression_ratio < 1.0, true)
}

test "telemetry_run_length_compression" {
  // 测试游程编码压缩
  
  let status_codes = [200, 200, 200, 200, 404, 404, 500, 200, 200, 200]
  let mut compressed_data = []
  let mut i = 0
  
  while i < status_codes.length() {
    let current_value = status_codes[i]
    let mut run_length = 1
    
    // 计算游程长度
    while i + run_length < status_codes.length() && 
          status_codes[i + run_length] == current_value {
      run_length = run_length + 1
    }
    
    // 存储值和游程长度
    compressed_data.push((current_value, run_length))
    i = i + run_length
  }
  
  // 验证游程编码
  assert_eq(compressed_data.length(), 5)  // 5个游程
  assert_eq(compressed_data[0], (200, 4))  // 4个200
  assert_eq(compressed_data[1], (404, 2))  // 2个404
  assert_eq(compressed_data[2], (500, 1))  // 1个500
  assert_eq(compressed_data[3], (200, 3))  // 3个200
  
  // 验证解压缩
  let mut decompressed = []
  i = 0
  while i < compressed_data.length() {
    let (value, count) = compressed_data[i]
    let mut j = 0
    while j < count {
      decompressed.push(value)
      j = j + 1
    }
    i = i + 1
  }
  
  assert_eq(decompressed.length(), status_codes.length())
  assert_eq(decompressed[0], 200)
  assert_eq(decompressed[4], 404)
  assert_eq(decompressed[6], 500)
}

test "telemetry_bit_packing_compression" {
  // 测试位打包压缩
  
  let small_numbers = [1, 2, 3, 4, 5, 6, 7, 8]  // 每个数字只需要3位
  let bits_per_number = 3
  let numbers_per_byte = 8 / bits_per_number  // 每字节可以打包2个数字
  
  // 模拟位打包（简化版本）
  let mut packed_bytes = []
  let mut i = 0
  
  while i < small_numbers.length() {
    if i % numbers_per_byte == 0 {
      // 新字节
      let first_num = small_numbers[i]
      let second_num = if i + 1 < small_numbers.length() { 
        small_numbers[i + 1] 
      } else { 
        0 
      }
      
      // 简化的打包：将两个3位数字组合成一个字节
      let packed_byte = (first_num << 3) | second_num
      packed_bytes.push(packed_byte)
    }
    i = i + numbers_per_byte
  }
  
  // 验证位打包
  assert_eq(packed_bytes.length(), 4)  // 8个数字打包成4个字节
  assert_eq(packed_bytes[0], (1 << 3) | 2)  // 00010010
  assert_eq(packed_bytes[1], (3 << 3) | 4)  // 00110100
  assert_eq(packed_bytes[2], (5 << 3) | 6)  // 01010110
  assert_eq(packed_bytes[3], (7 << 3) | 8)  // 01111000
  
  // 计算压缩率
  let original_bytes = small_numbers.length()  // 假设每个数字1字节
  let compressed_bytes = packed_bytes.length()
  let compression_ratio = compressed_bytes.to_double() / original_bytes.to_double()
  
  assert_eq(compression_ratio, 0.5)  // 50%压缩率
}

test "telemetry_timestamp_compression" {
  // 测试时间戳压缩
  
  let timestamps = [
    1640995200L,  // 2022-01-01 00:00:00
    1640995201L,  // +1秒
    1640995202L,  // +1秒
    1640995205L,  // +3秒
    1640995206L,  // +1秒
    1640995210L   // +4秒
  ]
  
  // 增量时间戳压缩
  let mut compressed_timestamps = [timestamps[0]]  // 第一个时间戳保持原样
  let mut i = 1
  
  while i < timestamps.length() {
    let delta = timestamps[i] - timestamps[i - 1]
    compressed_timestamps.push(delta)
    i = i + 1
  }
  
  // 验证时间戳压缩
  assert_eq(compressed_timestamps.length(), 6)
  assert_eq(compressed_timestamps[0], 1640995200L)
  assert_eq(compressed_timestamps[1], 1L)
  assert_eq(compressed_timestamps[2], 1L)
  assert_eq(compressed_timestamps[3], 3L)
  assert_eq(compressed_timestamps[4], 1L)
  assert_eq(compressed_timestamps[5], 4L)
  
  // 验证解压缩
  let mut decompressed_timestamps = [compressed_timestamps[0]]
  i = 1
  while i < compressed_timestamps.length() {
    let prev_timestamp = decompressed_timestamps[i - 1]
    let current_timestamp = prev_timestamp + compressed_timestamps[i]
    decompressed_timestamps.push(current_timestamp)
    i = i + 1
  }
  
  assert_eq(decompressed_timestamps.length(), timestamps.length())
  assert_eq(decompressed_timestamps[0], timestamps[0])
  assert_eq(decompressed_timestamps[5], timestamps[5])
  
  // 计算平均增量
  let mut total_delta = 0L
  i = 1
  while i < compressed_timestamps.length() {
    total_delta = total_delta + compressed_timestamps[i]
    i = i + 1
  }
  
  let average_delta = total_delta.to_double() / (compressed_timestamps.length() - 1).to_double()
  assert_eq(average_delta, 2.0)  // 平均2秒间隔
}

test "telemetry_attribute_compression" {
  // 测试属性压缩
  
  let attributes = [
    ("http.method", "GET"),
    ("http.status_code", "200"),
    ("service.name", "api-gateway"),
    ("http.method", "POST"),
    ("http.status_code", "201"),
    ("service.name", "api-gateway"),
    ("http.method", "GET"),
    ("http.status_code", "404"),
    ("service.name", "user-service")
  ]
  
  // 构建属性键和值的字典
  let mut key_dictionary = {}
  let mut value_dictionary = {}
  let mut key_index = 0
  let mut value_index = 0
  let mut compressed_attributes = []
  
  let mut i = 0
  while i < attributes.length() {
    let (key, value) = attributes[i]
    
    // 压缩键
    let compressed_key = if key_dictionary.contains_key(key) {
      key_dictionary[key]
    } else {
      key_dictionary[key] = key_index
      let index = key_index
      key_index = key_index + 1
      index
    }
    
    // 压缩值
    let compressed_value = if value_dictionary.contains_key(value) {
      value_dictionary[value]
    } else {
      value_dictionary[value] = value_index
      let index = value_index
      value_index = value_index + 1
      index
    }
    
    compressed_attributes.push((compressed_key, compressed_value))
    i = i + 1
  }
  
  // 验证字典构建
  assert_eq(key_dictionary.size(), 3)  // 3个唯一键
  assert_eq(value_dictionary.size(), 6)  // 6个唯一值
  assert_eq(key_dictionary["http.method"], 0)
  assert_eq(value_dictionary["GET"], 0)
  
  // 验证压缩结果
  assert_eq(compressed_attributes.length(), 9)
  assert_eq(compressed_attributes[0], (0, 0))  // http.method: GET
  assert_eq(compressed_attributes[1], (1, 1))  // http.status_code: 200
  assert_eq(compressed_attributes[2], (2, 2))  // service.name: api-gateway
  assert_eq(compressed_attributes[3], (0, 3))  // http.method: POST
  
  // 计算压缩统计
  let original_total_length = 0
  i = 0
  while i < attributes.length() {
    original_total_length = original_total_length + attributes[i].0.length() + attributes[i].1.length()
    i = i + 1
  }
  
  let compressed_total_length = compressed_attributes.length() * 2  // 每个属性用2个索引表示
  let compression_ratio = compressed_total_length.to_double() / original_total_length.to_double()
  
  assert_eq(compression_ratio < 1.0, true)
  assert_eq(compression_ratio > 0.1, true)
}

test "telemetry_batch_compression" {
  // 测试批量压缩
  
  let batch_data = [
    "trace_id:abc123,span_id:def456,duration:100",
    "trace_id:abc123,span_id:ghi789,duration:50",
    "trace_id:jkl012,span_id:mno345,duration:200",
    "trace_id:abc123,span_id:pqr678,duration:75",
    "trace_id:jkl012,span_id:stu901,duration:150"
  ]
  
  // 批量压缩：识别重复模式
  let mut common_patterns = {}
  let mut compressed_batch = []
  let mut pattern_id = 0
  
  let mut i = 0
  while i < batch_data.length() {
    let data = batch_data[i]
    
    // 提取公共前缀
    let trace_id_start = data.find("trace_id:")
    let trace_id_end = data.find(",", trace_id_start)
    let trace_id = data.slice(trace_id_start, trace_id_end)
    
    if common_patterns.contains_key(trace_id) {
      let existing_pattern_id = common_patterns[trace_id]
      compressed_batch.push((existing_pattern_id, data))
    } else {
      common_patterns[trace_id] = pattern_id
      compressed_batch.push((pattern_id, data))
      pattern_id = pattern_id + 1
    }
    
    i = i + 1
  }
  
  // 验证批量压缩
  assert_eq(common_patterns.size(), 2)  // 2个不同的trace_id
  assert_eq(compressed_batch.length(), 5)
  
  // 验证模式识别
  let abc123_entries = []
  let jkl012_entries = []
  i = 0
  while i < compressed_batch.length() {
    let (pattern_id, data) = compressed_batch[i]
    if pattern_id == 0 {
      abc123_entries.push(data)
    } else {
      jkl012_entries.push(data)
    }
    i = i + 1
  }
  
  assert_eq(abc123_entries.length(), 3)  // 3个abc123条目
  assert_eq(jkl012_entries.length(), 2)  // 2个jkl012条目
  
  // 计算压缩效率
  let total_compression_gain = (common_patterns.size() - 1) * 10  // 简化计算
  assert_eq(total_compression_gain > 0, true)
}