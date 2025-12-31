// 遥测数据压缩测试用例

test "telemetry_data_compression_basic" {
  // 测试基本的遥测数据压缩功能
  
  let original_data = [
    "metric:cpu_usage,value:75.5,timestamp:1640995200",
    "metric:memory_usage,value:68.2,timestamp:1640995260",
    "metric:disk_usage,value:45.8,timestamp:1640995320",
    "metric:network_io,value:120.5,timestamp:1640995380",
    "metric:response_time,value:85.3,timestamp:1640995440"
  ]
  
  // 验证原始数据
  assert_eq(original_data.length(), 5)
  assert_eq(original_data[0].contains("metric:cpu_usage"), true)
  assert_eq(original_data[4].contains("response_time"), true)
  
  // 计算原始数据大小
  let mut original_size = 0
  let mut i = 0
  while i < original_data.length() {
    original_size = original_size + original_data[i].length()
    i = i + 1
  }
  
  // 简化的压缩算法：字典压缩
  let mut compression_dict = []
  let mut compressed_data = []
  
  i = 0
  while i < original_data.length() {
    let data_item = original_data[i]
    let mut compressed_item = data_item
    
    // 替换常见的前缀
    compressed_item = compressed_item.replace("metric:", "m:")
    compressed_item = compressed_item.replace("value:", "v:")
    compressed_item = compressed_item.replace("timestamp:", "t:")
    
    compressed_data.push(compressed_item)
    i = i + 1
  }
  
  // 验证压缩数据
  assert_eq(compressed_data.length(), original_data.length())
  assert_eq(compressed_data[0].contains("m:cpu_usage"), true)
  assert_eq(compressed_data[0].contains("v:75.5"), true)
  assert_eq(compressed_data[0].contains("t:1640995200"), true)
  
  // 计算压缩后大小
  let mut compressed_size = 0
  i = 0
  while i < compressed_data.length() {
    compressed_size = compressed_size + compressed_data[i].length()
    i = i + 1
  }
  
  // 验证压缩效果
  assert_eq(compressed_size < original_size, true)
  let compression_ratio = compressed_size.to_double() / original_size.to_double()
  assert_eq(compression_ratio < 0.9, true)  // 至少10%的压缩率
}

test "telemetry_data_compression_delta_encoding" {
  // 测试增量编码压缩
  
  let time_series_values = [100, 105, 103, 108, 110, 107, 112, 115, 113, 118]
  
  // 验证原始数据
  assert_eq(time_series_values.length(), 10)
  assert_eq(time_series_values[0], 100)
  assert_eq(time_series_values[9], 118)
  
  // 增量编码
  let mut delta_encoded = []
  delta_encoded.push(time_series_values[0])  // 第一个值保持不变
  
  let mut i = 1
  while i < time_series_values.length() {
    let delta = time_series_values[i] - time_series_values[i - 1]
    delta_encoded.push(delta)
    i = i + 1
  }
  
  // 验证增量编码结果
  assert_eq(delta_encoded.length(), time_series_values.length())
  assert_eq(delta_encoded[0], 100)
  assert_eq(delta_encoded[1], 5)   // 105 - 100
  assert_eq(delta_encoded[2], -2)  // 103 - 105
  assert_eq(delta_encoded[9], 5)   // 118 - 113
  
  // 计算原始数据和增量编码数据的范围
  let mut original_min = time_series_values[0]
  let mut original_max = time_series_values[0]
  let mut delta_min = delta_encoded[0]
  let mut delta_max = delta_encoded[0]
  
  i = 0
  while i < time_series_values.length() {
    if time_series_values[i] < original_min {
      original_min = time_series_values[i]
    }
    if time_series_values[i] > original_max {
      original_max = time_series_values[i]
    }
    if delta_encoded[i] < delta_min {
      delta_min = delta_encoded[i]
    }
    if delta_encoded[i] > delta_max {
      delta_max = delta_encoded[i]
    }
    i = i + 1
  }
  
  // 验证增量编码减少了数值范围
  let original_range = original_max - original_min
  let delta_range = delta_max - delta_min
  assert_eq(delta_range < original_range, true)
  
  // 解码验证
  let mut decoded_values = []
  decoded_values.push(delta_encoded[0])
  
  i = 1
  while i < delta_encoded.length() {
    let decoded_value = decoded_values[i - 1] + delta_encoded[i]
    decoded_values.push(decoded_value)
    i = i + 1
  }
  
  // 验证解码结果与原始数据一致
  assert_eq(decoded_values.length(), time_series_values.length())
  i = 0
  while i < decoded_values.length() {
    assert_eq(decoded_values[i], time_series_values[i])
    i = i + 1
  }
}

test "telemetry_data_compression_run_length_encoding" {
  // 测试游程编码压缩
  
  let telemetry_sequence = [
    "normal", "normal", "normal", "warning", "warning",
    "normal", "normal", "error", "normal", "normal",
    "normal", "normal", "warning", "warning", "warning"
  ]
  
  // 验证原始数据
  assert_eq(telemetry_sequence.length(), 15)
  assert_eq(telemetry_sequence[0], "normal")
  assert_eq(telemetry_sequence[7], "error")
  
  // 游程编码
  let mut rle_encoded = []
  let mut i = 0
  while i < telemetry_sequence.length() {
    let current_value = telemetry_sequence[i]
    let mut count = 1
    
    // 计算连续相同值的数量
    let mut j = i + 1
    while j < telemetry_sequence.length() and telemetry_sequence[j] == current_value {
      count = count + 1
      j = j + 1
    }
    
    // 编码为 (值, 数量) 对
    let encoded_pair = current_value + ":" + count.to_string()
    rle_encoded.push(encoded_pair)
    
    i = j  // 跳过已编码的元素
  }
  
  // 验证游程编码结果
  assert_eq(rle_encoded.length(), 6)  // 原始15个值压缩为6个编码
  assert_eq(rle_encoded[0], "normal:3")
  assert_eq(rle_encoded[1], "warning:2")
  assert_eq(rle_encoded[2], "normal:2")
  assert_eq(rle_encoded[3], "error:1")
  assert_eq(rle_encoded[4], "normal:4")
  assert_eq(rle_encoded[5], "warning:3")
  
  // 验证压缩效果
  assert_eq(rle_encoded.length() < telemetry_sequence.length(), true)
  let compression_ratio = rle_encoded.length().to_double() / telemetry_sequence.length().to_double()
  assert_eq(compression_ratio < 0.8, true)  // 至少20%的压缩率
  
  // 解码验证
  let mut decoded_values = []
  i = 0
  while i < rle_encoded.length() {
    let encoded_pair = rle_encoded[i]
    let parts = encoded_pair.split(":")
    let value = parts[0]
    let count = parts[1].to_int()
    
    let mut j = 0
    while j < count {
      decoded_values.push(value)
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证解码结果与原始数据一致
  assert_eq(decoded_values.length(), telemetry_sequence.length())
  i = 0
  while i < decoded_values.length() {
    assert_eq(decoded_values[i], telemetry_sequence[i])
    i = i + 1
  }
}

test "telemetry_data_compression_huffman_style" {
  // 测试霍夫曼风格压缩（简化版）
  
  let telemetry_symbols = [
    "A", "B", "C", "A", "A", "B", "A", "C", "C", "C",
    "A", "B", "A", "A", "C", "B", "A", "A", "C", "A"
  ]
  
  // 验证原始数据
  assert_eq(telemetry_symbols.length(), 20)
  
  // 计算符号频率
  let mut symbol_counts = {}
  let mut i = 0
  while i < telemetry_symbols.length() {
    let symbol = telemetry_symbols[i]
    if symbol_counts.contains(symbol) {
      symbol_counts[symbol] = symbol_counts[symbol] + 1
    } else {
      symbol_counts[symbol] = 1
    }
    i = i + 1
  }
  
  // 验证符号频率
  assert_eq(symbol_counts["A"], 10)  // A出现10次
  assert_eq(symbol_counts["B"], 4)   // B出现4次
  assert_eq(symbol_counts["C"], 6)   // C出现6次
  
  // 根据频率分配可变长度编码（频率越高，编码越短）
  let mut huffman_codes = {}
  huffman_codes["A"] = "0"      // 最高频率，最短编码
  huffman_codes["B"] = "110"    // 中等频率
  huffman_codes["C"] = "10"     // 中等频率
  
  // 验证霍夫曼编码
  assert_eq(huffman_codes["A"].length(), 1)
  assert_eq(huffman_codes["B"].length(), 3)
  assert_eq(huffman_codes["C"].length(), 2)
  
  // 编码数据
  let mut encoded_data = ""
  i = 0
  while i < telemetry_symbols.length() {
    let symbol = telemetry_symbols[i]
    let code = huffman_codes[symbol]
    encoded_data = encoded_data + code
    i = i + 1
  }
  
  // 验证编码数据
  assert_eq(encoded_data.length(), 28)  // 10*1 + 4*3 + 6*2 = 28
  
  // 计算压缩效果（假设原始每个符号用2位表示）
  let original_bits = telemetry_symbols.length() * 2
  let compressed_bits = encoded_data.length()
  let compression_ratio = compressed_bits.to_double() / original_bits.to_double()
  
  // 验证压缩效果
  assert_eq(compression_ratio < 1.0, true)
  assert_eq(compression_ratio < 0.8, true)  // 至少20%的压缩率
  
  // 计算加权平均编码长度
  let mut weighted_length = 0.0
  let total_symbols = telemetry_symbols.length().to_double()
  
  weighted_length = weighted_length + (symbol_counts["A"].to_double() / total_symbols) * huffman_codes["A"].length().to_double()
  weighted_length = weighted_length + (symbol_counts["B"].to_double() / total_symbols) * huffman_codes["B"].length().to_double()
  weighted_length = weighted_length + (symbol_counts["C"].to_double() / total_symbols) * huffman_codes["C"].length().to_double()
  
  // 验证加权平均编码长度小于固定长度编码
  assert_eq(weighted_length < 2.0, true)
  assert_eq(weighted_length > 1.0, true)
}