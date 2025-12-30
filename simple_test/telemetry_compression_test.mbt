// 遥测数据压缩测试用例

test "telemetry_run_length_compression" {
  // 测试游程编码压缩
  
  let telemetry_stream = [
    "INFO", "INFO", "INFO", "WARN", "INFO", "INFO", 
    "ERROR", "ERROR", "INFO", "INFO", "INFO", "INFO",
    "WARN", "WARN", "WARN", "INFO", "INFO", "ERROR"
  ]
  
  // 游程编码压缩
  let compressed_data = []
  
  let mut i = 0
  while i < telemetry_stream.length() {
    let current_value = telemetry_stream[i]
    let count = 1
    
    // 计算连续相同值的数量
    let mut j = i + 1
    while j < telemetry_stream.length() && telemetry_stream[j] == current_value {
      count = count + 1
      j = j + 1
    }
    
    compressed_data.push((current_value, count))
    i = j  // 跳过已压缩的元素
  }
  
  // 验证压缩结果
  assert_eq(compressed_data.length(), 6)  // 原始18个元素压缩为6个组
  assert_eq(compressed_data[0], ("INFO", 3))
  assert_eq(compressed_data[1], ("WARN", 1))
  assert_eq(compressed_data[2], ("INFO", 2))
  assert_eq(compressed_data[3], ("ERROR", 2))
  assert_eq(compressed_data[4], ("INFO", 4))
  assert_eq(compressed_data[5], ("WARN", 3))
  
  // 验证解压缩
  let decompressed_length = 0
  i = 0
  while i < compressed_data.length() {
    decompressed_length = decompressed_length + compressed_data[i].1
    i = i + 1
  }
  assert_eq(decompressed_length, telemetry_stream.length())
}

test "telemetry_dictionary_compression" {
  // 测试字典压缩
  
  let log_messages = [
    "User login successful",
    "Database connection established",
    "User login successful",
    "API request processed",
    "User login successful",
    "Cache miss detected",
    "Database connection established",
    "User login successful",
    "API request processed",
    "User login successful"
  ]
  
  // 构建字典
  let dictionary = {}
  let compressed_indices = []
  let mut next_id = 0
  
  let mut i = 0
  while i < log_messages.length() {
    let message = log_messages[i]
    
    if dictionary.contains(message) {
      // 消息已在字典中，使用现有索引
      compressed_indices.push(dictionary[message])
    } else {
      // 新消息，添加到字典
      dictionary[message] = next_id
      compressed_indices.push(next_id)
      next_id = next_id + 1
    }
    
    i = i + 1
  }
  
  // 验证字典压缩结果
  assert_eq(dictionary.length(), 4)  // 4个不同的消息
  assert_eq(compressed_indices.length(), log_messages.length())  // 压缩后长度相同
  
  // 验证字典内容
  assert_eq(dictionary["User login successful"] != nil, true)
  assert_eq(dictionary["Database connection established"] != nil, true)
  assert_eq(dictionary["API request processed"] != nil, true)
  assert_eq(dictionary["Cache miss detected"] != nil, true)
  
  // 验证压缩索引
  assert_eq(compressed_indices[0], compressed_indices[2])  // 相同消息有相同索引
  assert_eq(compressed_indices[0], compressed_indices[4])
  assert_eq(compressed_indices[0], compressed_indices[7])
  assert_eq(compressed_indices[0], compressed_indices[9])
}

test "telemetry_delta_compression" {
  // 测试增量压缩（适用于时间序列数据）
  
  let time_series_values = [100, 105, 103, 108, 110, 107, 112, 115, 113, 118]
  
  // 增量压缩
  let compressed_deltas = []
  let mut previous_value = time_series_values[0]
  
  // 第一个值存储原始值
  compressed_deltas.push(time_series_values[0])
  
  // 后续值存储增量
  let mut i = 1
  while i < time_series_values.length() {
    let current_value = time_series_values[i]
    let delta = current_value - previous_value
    compressed_deltas.push(delta)
    previous_value = current_value
    i = i + 1
  }
  
  // 验证增量压缩结果
  assert_eq(compressed_deltas.length(), time_series_values.length())
  assert_eq(compressed_deltas[0], 100)  // 第一个值
  assert_eq(compressed_deltas[1], 5)    // 105 - 100
  assert_eq(compressed_deltas[2], -2)   // 103 - 105
  assert_eq(compressed_deltas[3], 5)    // 108 - 103
  assert_eq(compressed_deltas[4], 2)    // 110 - 108
  assert_eq(compressed_deltas[5], -3)   // 107 - 110
  assert_eq(compressed_deltas[6], 5)    // 112 - 107
  assert_eq(compressed_deltas[7], 3)    // 115 - 112
  assert_eq(compressed_deltas[8], -2)   // 113 - 115
  assert_eq(compressed_deltas[9], 5)    // 118 - 113
  
  // 验证解压缩
  let mut decompressed_value = compressed_deltas[0]
  assert_eq(decompressed_value, time_series_values[0])
  
  i = 1
  while i < compressed_deltas.length() {
    decompressed_value = decompressed_value + compressed_deltas[i]
    assert_eq(decompressed_value, time_series_values[i])
    i = i + 1
  }
}

test "telemetry_huffman_compression" {
  // 测试霍夫曼压缩（简化版本）
  
  let telemetry_symbols = ["A", "B", "C", "D", "E", "A", "A", "B", "C", "A", "A", "A", "E", "D", "B", "A"]
  
  // 统计符号频率
  let frequency = {
    "A": 0,
    "B": 0,
    "C": 0,
    "D": 0,
    "E": 0
  }
  
  let mut i = 0
  while i < telemetry_symbols.length() {
    let symbol = telemetry_symbols[i]
    frequency[symbol] = frequency[symbol] + 1
    i = i + 1
  }
  
  // 基于频率分配编码（频率越高编码越短）
  let huffman_codes = {}
  
  // 按频率排序并分配编码
  let sorted_symbols = ["A", "B", "C", "D", "E"]  // 假设已按频率排序
  let codes = ["0", "10", "110", "1110", "1111"]  // 霍夫曼编码
  
  i = 0
  while i < sorted_symbols.length() {
    huffman_codes[sorted_symbols[i]] = codes[i]
    i = i + 1
  }
  
  // 压缩数据
  let compressed_bits = ""
  i = 0
  while i < telemetry_symbols.length() {
    let symbol = telemetry_symbols[i]
    compressed_bits = compressed_bits + huffman_codes[symbol]
    i = i + 1
  }
  
  // 验证霍夫曼压缩结果
  assert_eq(frequency["A"], 7)  // A出现7次
  assert_eq(frequency["B"], 3)  // B出现3次
  assert_eq(frequency["C"], 2)  // C出现2次
  assert_eq(frequency["D"], 2)  // D出现2次
  assert_eq(frequency["E"], 2)  // E出现2次
  
  // 验证编码分配
  assert_eq(huffman_codes["A"], "0")      // 最高频率，最短编码
  assert_eq(huffman_codes["B"], "10")     // 次高频率
  assert_eq(huffman_codes["C"], "110")    // 中等频率
  assert_eq(huffman_codes["D"], "1110")   // 较低频率
  assert_eq(huffman_codes["E"], "1111")   // 最低频率，最长编码
  
  // 验证压缩结果
  assert_eq(compressed_bits.length() > 0, true)
  assert_eq(compressed_bits.starts_with("0"), true)  // 第一个符号是A
}