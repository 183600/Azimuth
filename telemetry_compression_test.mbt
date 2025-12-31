// 数据压缩测试用例

test "telemetry_compression_basic_run_length" {
  // 测试遥测数据基本行程长度压缩
  
  let original_data = "AAAABBBCCDDEEEEFF"
  
  // 简单的行程长度编码
  let mut compressed_data = ""
  let mut i = 0
  while i < original_data.length() {
    let current_char = original_data[i]
    let mut count = 1
    
    // 计算连续字符数量
    let mut j = i + 1
    while j < original_data.length() && original_data[j] == current_char {
      count = count + 1
      j = j + 1
    }
    
    // 添加压缩数据
    compressed_data = compressed_data + count.to_string() + current_char
    
    i = j
  }
  
  // 验证压缩结果
  assert_eq(compressed_data, "4A3B2C2D4E2F")
  assert_eq(compressed_data.length(), 12)
  assert_eq(original_data.length(), 18)
  
  // 计算压缩率
  let compression_ratio = compressed_data.length().to_double() / original_data.length().to_double()
  assert_eq(compression_ratio < 1.0, true) // 压缩后应该更小
}

test "telemetry_compression_dictionary" {
  // 测试遥测数据字典压缩
  
  let telemetry_data = [
    "http_request_total",
    "http_request_duration",
    "http_response_size",
    "http_request_total",
    "http_request_duration",
    "database_query_total",
    "http_request_total",
    "cache_hit_ratio"
  ]
  
  // 构建字典
  let mut dictionary = []
  let mut compressed_indices = []
  
  let mut i = 0
  while i < telemetry_data.length() {
    let item = telemetry_data[i]
    
    // 查找字典索引
    let mut found_index = -1
    let mut j = 0
    while j < dictionary.length() {
      if dictionary[j] == item {
        found_index = j
        break
      }
      j = j + 1
    }
    
    // 如果没找到，添加到字典
    if found_index == -1 {
      dictionary.push(item)
      found_index = dictionary.length() - 1
    }
    
    compressed_indices.push(found_index)
    i = i + 1
  }
  
  // 验证字典压缩
  assert_eq(dictionary.length(), 5) // 5个唯一条目
  assert_eq(compressed_indices.length(), 8) // 8个原始条目
  
  // 验证压缩数据
  assert_eq(compressed_indices[0], 0) // http_request_total
  assert_eq(compressed_indices[1], 1) // http_request_duration
  assert_eq(compressed_indices[3], 0) // 重复的http_request_total
  assert_eq(compressed_indices[5], 4) // database_query_total
  
  // 验证解压缩
  let mut decompressed_data = []
  i = 0
  while i < compressed_indices.length() {
    let index = compressed_indices[i]
    if index >= 0 && index < dictionary.length() {
      decompressed_data.push(dictionary[index])
    }
    i = i + 1
  }
  
  // 验证解压缩结果
  assert_eq(decompressed_data.length(), telemetry_data.length())
  
  let mut decompression_correct = true
  i = 0
  while i < telemetry_data.length() {
    if decompressed_data[i] != telemetry_data[i] {
      decompression_correct = false
      break
    }
    i = i + 1
  }
  
  assert_eq(decompression_correct, true)
}

test "telemetry_compression_delta_encoding" {
  // 测试遥测数据增量编码压缩
  
  let timestamp_values = [1640995200L, 1640995201L, 1640995202L, 1640995204L, 1640995205L]
  let metric_values = [100.0, 102.5, 105.0, 107.5, 110.0]
  
  // 增量编码时间戳
  let mut delta_timestamps = []
  delta_timestamps.push(timestamp_values[0]) // 第一个值保持不变
  
  let mut i = 1
  while i < timestamp_values.length() {
    let delta = timestamp_values[i] - timestamp_values[i - 1]
    delta_timestamps.push(delta)
    i = i + 1
  }
  
  // 增量编码指标值
  let mut delta_metrics = []
  delta_metrics.push(metric_values[0]) // 第一个值保持不变
  
  i = 1
  while i < metric_values.length() {
    let delta = metric_values[i] - metric_values[i - 1]
    delta_metrics.push(delta)
    i = i + 1
  }
  
  // 验证增量编码
  assert_eq(delta_timestamps.length(), timestamp_values.length())
  assert_eq(delta_metrics.length(), metric_values.length())
  
  // 验证时间戳增量
  assert_eq(delta_timestamps[0], 1640995200L)
  assert_eq(delta_timestamps[1], 1L)
  assert_eq(delta_timestamps[2], 1L)
  assert_eq(delta_timestamps[3], 2L) // 跳跃了1秒
  assert_eq(delta_timestamps[4], 1L)
  
  // 验证指标值增量
  assert_eq(delta_metrics[0], 100.0)
  assert_eq(delta_metrics[1], 2.5)
  assert_eq(delta_metrics[2], 2.5)
  assert_eq(delta_metrics[3], 2.5)
  assert_eq(delta_metrics[4], 2.5)
  
  // 验证解压缩
  let mut decompressed_timestamps = []
  let mut decompressed_metrics = []
  
  decompressed_timestamps.push(delta_timestamps[0])
  decompressed_metrics.push(delta_metrics[0])
  
  i = 1
  while i < delta_timestamps.length() {
    let timestamp = decompressed_timestamps[i - 1] + delta_timestamps[i]
    decompressed_timestamps.push(timestamp)
    i = i + 1
  }
  
  i = 1
  while i < delta_metrics.length() {
    let metric = decompressed_metrics[i - 1] + delta_metrics[i]
    decompressed_metrics.push(metric)
    i = i + 1
  }
  
  // 验证解压缩结果
  let mut timestamps_correct = true
  let mut metrics_correct = true
  
  i = 0
  while i < timestamp_values.length() {
    if decompressed_timestamps[i] != timestamp_values[i] {
      timestamps_correct = false
      break
    }
    i = i + 1
  }
  
  i = 0
  while i < metric_values.length() {
    if decompressed_metrics[i] != metric_values[i] {
      metrics_correct = false
      break
    }
    i = i + 1
  }
  
  assert_eq(timestamps_correct, true)
  assert_eq(metrics_correct, true)
}

test "telemetry_compression_huffman_simulation" {
  // 测试遥测数据霍夫曼压缩模拟
  
  let telemetry_symbols = ["A", "B", "C", "D", "E"]
  let symbol_frequencies = [45, 13, 12, 16, 9] // 总计95
  let test_data = "AAAABBBCCCDDEAAAABBBE"
  
  // 模拟霍夫曼编码（基于频率的简化版本）
  let huffman_codes = [
    ("A", "0"),      // 最高频率，最短编码
    ("B", "101"),    
    ("D", "100"),    
    ("C", "110"),    
    ("E", "111")     // 最低频率，最长编码
  ]
  
  // 编码测试数据
  let mut encoded_data = ""
  let mut i = 0
  while i < test_data.length() {
    let symbol = test_data[i].to_string()
    
    let mut j = 0
    while j < huffman_codes.length() {
      if huffman_codes[j].0 == symbol {
        encoded_data = encoded_data + huffman_codes[j].1
        break
      }
      j = j + 1
    }
    
    i = i + 1
  }
  
  // 验证编码结果
  assert_eq(encoded_data.length() > 0, true)
  
  // 计算压缩率（假设原始数据每个字符8位）
  let original_bits = test_data.length() * 8
  let compressed_bits = encoded_data.length()
  let compression_ratio = compressed_bits.to_double() / original_bits.to_double()
  
  assert_eq(compression_ratio < 1.0, true) // 压缩后应该更小
  
  // 验证编码统计
  let mut a_count = 0
  let mut b_count = 0
  let mut c_count = 0
  let mut d_count = 0
  let mut e_count = 0
  
  i = 0
  while i < test_data.length() {
    let symbol = test_data[i].to_string()
    if symbol == "A" { a_count = a_count + 1 }
    if symbol == "B" { b_count = b_count + 1 }
    if symbol == "C" { c_count = c_count + 1 }
    if symbol == "D" { d_count = d_count + 1 }
    if symbol == "E" { e_count = e_count + 1 }
    i = i + 1
  }
  
  assert_eq(a_count, 8)
  assert_eq(b_count, 5)
  assert_eq(c_count, 3)
  assert_eq(d_count, 2)
  assert_eq(e_count, 2)
}

test "telemetry_compression_batch_processing" {
  // 测试遥测数据批量压缩处理
  
  let batch_data = [
    "metric_1:100.5",
    "metric_2:200.3",
    "metric_3:150.7",
    "metric_1:105.2",
    "metric_2:205.1",
    "metric_3:155.9"
  ]
  
  // 批量压缩：提取公共前缀
  let mut common_prefixes = []
  let mut compressed_batch = []
  
  let mut i = 0
  while i < batch_data.length() {
    let data = batch_data[i]
    let parts = data.split(":")
    
    if parts.length() == 2 {
      let metric_name = parts[0]
      let metric_value = parts[1]
      
      // 查找或创建前缀映射
      let mut prefix_index = -1
      let mut j = 0
      while j < common_prefixes.length() {
        if common_prefixes[j] == metric_name {
          prefix_index = j
          break
        }
        j = j + 1
      }
      
      if prefix_index == -1 {
        common_prefixes.push(metric_name)
        prefix_index = common_prefixes.length() - 1
      }
      
      compressed_batch.push((prefix_index, metric_value))
    }
    
    i = i + 1
  }
  
  // 验证批量压缩
  assert_eq(common_prefixes.length(), 3) // 3个唯一的metric名称
  assert_eq(compressed_batch.length(), 6) // 6个数据点
  
  // 验证压缩数据
  assert_eq(common_prefixes[0], "metric_1")
  assert_eq(common_prefixes[1], "metric_2")
  assert_eq(common_prefixes[2], "metric_3")
  
  assert_eq(compressed_batch[0].0, 0) // metric_1
  assert_eq(compressed_batch[0].1, "100.5")
  assert_eq(compressed_batch[3].0, 0) // 重复的metric_1
  assert_eq(compressed_batch[3].1, "105.2")
  
  // 计算压缩率
  let original_size = 0
  i = 0
  while i < batch_data.length() {
    original_size = original_size + batch_data[i].length()
    i = i + 1
  }
  
  let compressed_size = 0
  i = 0
  while i < common_prefixes.length() {
    compressed_size = compressed_size + common_prefixes[i].length()
    i = i + 1
  }
  
  i = 0
  while i < compressed_batch.length() {
    compressed_size = compressed_size + compressed_batch[i].1.length() + 1 // +1 for index
    i = i + 1
  }
  
  let compression_ratio = compressed_size.to_double() / original_size.to_double()
  assert_eq(compression_ratio < 1.0, true)
}

test "telemetry_compression_adaptive_threshold" {
  // 测试遥测数据自适应阈值压缩
  
  let data_series = [
    100.0, 100.1, 100.2, 100.3, // 小变化
    150.0, 150.5, 151.0, 151.5, // 中等变化
    200.0, 250.0, 300.0, 350.0  // 大变化
  ]
  
  let compression_threshold = 10.0 // 变化阈值
  
  // 自适应压缩
  let mut compressed_segments = []
  let mut i = 0
  while i < data_series.length() {
    let start_value = data_series[i]
    let mut segment_length = 1
    let mut can_compress = true
    
    // 检查是否可以压缩（变化小于阈值）
    let mut j = i + 1
    while j < data_series.length() && (data_series[j] - start_value).abs() < compression_threshold {
      segment_length = segment_length + 1
      j = j + 1
    }
    
    // 如果段长度大于1，可以压缩
    if segment_length > 1 {
      compressed_segments.push(("compressed", start_value, segment_length))
      i = i + segment_length
    } else {
      compressed_segments.push(("raw", start_value, 1))
      i = i + 1
    }
  }
  
  // 验证自适应压缩
  assert_eq(compressed_segments.length(), 4) // 应该有4个段
  
  // 验证压缩段
  assert_eq(compressed_segments[0].0, "compressed") // 第一段可以压缩
  assert_eq(compressed_segments[0].1, 100.0)
  assert_eq(compressed_segments[0].2, 4)
  
  assert_eq(compressed_segments[1].0, "compressed") // 第二段可以压缩
  assert_eq(compressed_segments[1].1, 150.0)
  assert_eq(compressed_segments[1].2, 4)
  
  assert_eq(compressed_segments[2].0, "raw") // 第三段变化太大，不能压缩
  assert_eq(compressed_segments[2].1, 200.0)
  assert_eq(compressed_segments[2].2, 1)
  
  assert_eq(compressed_segments[3].0, "raw") // 第四段变化太大，不能压缩
  assert_eq(compressed_segments[3].1, 250.0)
  assert_eq(compressed_segments[3].2, 1)
  
  // 计算压缩率
  let original_count = data_series.length()
  let compressed_count = compressed_segments.length()
  let compression_ratio = compressed_count.to_double() / original_count.to_double()
  
  assert_eq(compression_ratio < 1.0, true)
}

test "telemetry_compression_quality_metrics" {
  // 测试遥测数据压缩质量指标
  
  let original_data = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0]
  
  // 有损压缩：降采样
  let compression_factor = 2 // 保留每2个点中的1个
  let mut compressed_data = []
  
  let mut i = 0
  while i < original_data.length() {
    if i % compression_factor == 0 {
      compressed_data.push(original_data[i])
    }
    i = i + 1
  }
  
  // 验证压缩结果
  assert_eq(compressed_data.length(), 4)
  assert_eq(compressed_data[0], 1.0)
  assert_eq(compressed_data[1], 3.0)
  assert_eq(compressed_data[2], 5.0)
  assert_eq(compressed_data[3], 7.0)
  
  // 计算压缩质量指标
  let compression_ratio = compressed_data.length().to_double() / original_data.length().to_double()
  assert_eq(compression_ratio, 0.5)
  
  // 计算重建误差（简化版本）
  let mut total_error = 0.0
  i = 0
  while i < original_data.length() {
    let original_value = original_data[i]
    let mut reconstructed_value = 0.0
    
    // 简单的线性插值重建
    if i % compression_factor == 0 {
      reconstructed_value = original_value
    } else {
      let prev_index = (i / compression_factor) * compression_factor
      let next_index = ((i / compression_factor) + 1) * compression_factor
      if next_index < original_data.length() {
        let prev_value = original_data[prev_index]
        let next_value = original_data[next_index]
        let interpolation_factor = (i - prev_index).to_double() / (next_index - prev_index).to_double()
        reconstructed_value = prev_value + (next_value - prev_value) * interpolation_factor
      } else {
        reconstructed_value = original_value
      }
    }
    
    let error = (original_value - reconstructed_value).abs()
    total_error = total_error + error
    
    i = i + 1
  }
  
  let mean_error = total_error / original_data.length().to_double()
  
  // 验证质量指标
  assert_eq(mean_error >= 0.0, true)
  assert_eq(mean_error < 2.0, true) // 平均误差应该相对较小
  
  // 计算信噪比（简化版本）
  let signal_power = 0.0
  i = 0
  while i < original_data.length() {
    signal_power = signal_power + original_data[i] * original_data[i]
    i = i + 1
  }
  
  let noise_power = total_error * total_error / original_data.length().to_double()
  let snr = signal_power / noise_power
  
  assert_eq(snr > 1.0, true) // 信噪比应该大于1
}

test "telemetry_compression_streaming" {
  // 测试遥测数据流式压缩
  
  let stream_data = [
    "temperature:25.5",
    "humidity:60.2", 
    "pressure:1013.25",
    "temperature:26.0",
    "humidity:61.0",
    "pressure:1012.8",
    "temperature:26.5",
    "humidity:61.5",
    "pressure:1013.0"
  ]
  
  // 流式压缩：滑动窗口
  let window_size = 3
  let mut compressed_stream = []
  let mut position = 0
  
  while position <= stream_data.length() - window_size {
    // 提取窗口
    let mut window = []
    let mut i = 0
    while i < window_size {
      window.push(stream_data[position + i])
      i = i + 1
    }
    
    // 分析窗口中的模式
    let mut metric_names = []
    let mut metric_values = []
    
    i = 0
    while i < window.length() {
      let data = window[i]
      let parts = data.split(":")
      if parts.length() == 2 {
        metric_names.push(parts[0])
        metric_values.push(parts[1])
      }
      i = i + 1
    }
    
    // 检查是否有重复的metric名称
    let mut name_counts = []
    i = 0
    while i < metric_names.length() {
      let name = metric_names[i]
      let mut count = 0
      let mut j = 0
      while j < metric_names.length() {
        if metric_names[j] == name {
          count = count + 1
        }
        j = j + 1
      }
      name_counts.push((name, count))
      i = i + 1
    }
    
    // 压缩窗口数据
    let mut compressed_window = ""
    i = 0
    while i < window.length() {
      compressed_window = compressed_window + window[i]
      if i < window.length() - 1 {
        compressed_window = compressed_window + "|"
      }
      i = i + 1
    }
    
    compressed_stream.push(compressed_window)
    position = position + 1
  }
  
  // 验证流式压缩
  assert_eq(compressed_stream.length(), 7) // 9-3+1 = 7个窗口
  
  // 验证第一个窗口
  assert_eq(compressed_stream[0].contains("temperature:25.5"), true)
  assert_eq(compressed_stream[0].contains("humidity:60.2"), true)
  assert_eq(compressed_stream[0].contains("pressure:1013.25"), true)
  
  // 验证最后一个窗口
  assert_eq(compressed_stream[6].contains("temperature:26.5"), true)
  assert_eq(compressed_stream[6].contains("humidity:61.5"), true)
  assert_eq(compressed_stream[6].contains("pressure:1013.0"), true)
  
  // 计算压缩统计
  let mut original_total_size = 0
  let mut compressed_total_size = 0
  
  let mut i = 0
  while i < stream_data.length() {
    original_total_size = original_total_size + stream_data[i].length()
    i = i + 1
  }
  
  i = 0
  while i < compressed_stream.length() {
    compressed_total_size = compressed_total_size + compressed_stream[i].length()
    i = i + 1
  }
  
  // 流式压缩可能会有额外的开销，但应该提供某些优势
  let overhead_ratio = compressed_total_size.to_double() / original_total_size.to_double()
  assert_eq(overhead_ratio > 0.0, true)
}