// 遥测数据压缩测试用例
// 测试遥测数据的压缩、解压缩和存储优化功能

test "telemetry_string_compression" {
  // 测试遥测字符串压缩
  
  let original_string = "service.name=payment-service,service.version=1.2.3,trace.id=0af7651916cd43dd8448eb211c80319c,span.id=b7ad6b7169203331"
  
  // 验证原始字符串
  assert_eq(original_string.length(), 98)
  assert_eq(original_string.contains("service.name=payment-service"), true)
  assert_eq(original_string.contains("trace.id=0af7651916cd43dd8448eb211c80319c"), true)
  
  // 模拟压缩：替换重复的模式
  let compressed_string = original_string
    .replace("service.", "s.")
    .replace("version", "v")
    .replace("trace.", "t.")
    .replace("span.", "sp.")
    .replace("payment-", "p-")
    .replace("0af7651916cd43dd8448eb211c80319c", "trace1")
    .replace("b7ad6b7169203331", "span1")
  
  // 验证压缩字符串
  assert_eq(compressed_string.length() < original_string.length(), true)
  assert_eq(compressed_string.contains("s.name=p-service"), true)
  assert_eq(compressed_string.contains("t.id=trace1"), true)
  assert_eq(compressed_string.contains("sp.id=span1"), true)
  
  // 计算压缩率
  let compression_ratio = compressed_string.length().to_double() / original_string.length().to_double()
  let compression_percentage = (1.0 - compression_ratio) * 100.0
  
  // 验证压缩率
  assert_eq(compression_ratio < 1.0, true)
  assert_eq(compression_percentage > 0.0, true)
  
  // 创建压缩报告
  let compression_report = "Original: " + original_string.length().to_string() + 
                          " chars, Compressed: " + compressed_string.length().to_string() + 
                          " chars, Ratio: " + compression_percentage.to_string().slice(0, 5) + "%"
  
  // 验证压缩报告
  assert_eq(compression_report.contains("Original: 98"), true)
  assert_eq(compression_report.contains("Compressed:"), true)
  assert_eq(compression_report.contains("Ratio:"), true)
}

test "telemetry_numeric_compression" {
  // 测试遥测数值压缩
  
  let original_numbers = [1640995200L, 1234.5, 678.9, 101112.0, 456.78]
  let number_strings = ["1640995200", "1234.5", "678.9", "101112.0", "456.78"]
  
  // 验证原始数值
  assert_eq(original_numbers.length(), 5)
  assert_eq(number_strings.length(), 5)
  assert_eq(original_numbers[0], 1640995200L)
  assert_eq(original_numbers[1], 1234.5)
  
  // 计算数值的总字符长度
  let mut total_original_length = 0
  let mut i = 0
  while i < number_strings.length() {
    total_original_length = total_original_length + number_strings[i].length()
    i = i + 1
  }
  
  // 验证原始长度
  assert_eq(total_original_length, 28) // 10 + 5 + 4 + 7 + 2 = 28
  
  // 模拟数值压缩：使用变长编码
  let compressed_numbers = []
  i = 0
  while i < original_numbers.length() {
    let num = original_numbers[i]
    let compressed_str = ""
    
    // 简化的变长编码模拟
    if (num >= 1000000L) {
      compressed_str = "L" + num.to_string() // 大数用L前缀
    } else if (num >= 1000.0) {
      compressed_str = "M" + num.to_string().slice(0, 4) // 中等数用M前缀并截断
    } else {
      compressed_str = "S" + num.to_string() // 小数用S前缀
    }
    
    compressed_numbers.push(compressed_str)
    i = i + 1
  }
  
  // 验证压缩数值
  assert_eq(compressed_numbers.length(), 5)
  assert_eq(compressed_numbers[0].has_prefix("L"), true)
  assert_eq(compressed_numbers[1].has_prefix("M"), true)
  
  // 计算压缩后的总长度
  let mut total_compressed_length = 0
  i = 0
  while i < compressed_numbers.length() {
    total_compressed_length = total_compressed_length + compressed_numbers[i].length()
    i = i + 1
  }
  
  // 验证压缩效果
  assert_eq(total_compressed_length < total_original_length, true)
  
  // 计算压缩率
  let numeric_compression_ratio = total_compressed_length.to_double() / total_original_length.to_double()
  assert_eq(numeric_compression_ratio < 1.0, true)
}

test "telemetry_json_compression" {
  // 测试遥测JSON数据压缩
  
  let original_json = "{"
  original_json = original_json + "\"service_name\":\"payment-service\","
  original_json = original_json + "\"service_version\":\"1.2.3\","
  original_json = original_json + "\"trace_id\":\"0af7651916cd43dd8448eb211c80319c\","
  original_json = original_json + "\"span_id\":\"b7ad6b7169203331\","
  original_json = original_json + "\"timestamp\":1640995200,"
  original_json = original_json + "\"duration\":123.456"
  original_json = original_json + "}"
  
  // 验证原始JSON
  assert_eq(original_json.has_prefix("{"), true)
  assert_eq(original_json.has_suffix("}"), true)
  assert_eq(original_json.contains("\"service_name\":\"payment-service\""), true)
  
  // 模拟JSON压缩：移除空格和使用短键名
  let compressed_json = original_json
    .replace(" ", "")
    .replace("\"service_name\":", "\"sn\":")
    .replace("\"service_version\":", "\"sv\":")
    .replace("\"trace_id\":", "\"ti\":")
    .replace("\"span_id\":", "\"si\":")
    .replace("\"timestamp\":", "\"ts\":")
    .replace("\"duration\":", "\"d\":")
    .replace("payment-service", "p-svc")
    .replace("0af7651916cd43dd8448eb211c80319c", "t1")
    .replace("b7ad6b7169203331", "s1")
  
  // 验证压缩JSON
  assert_eq(compressed_json.length() < original_json.length(), true)
  assert_eq(compressed_json.contains("\"sn\":\"p-svc\""), true)
  assert_eq(compressed_json.contains("\"ti\":\"t1\""), true)
  assert_eq(compressed_json.contains("\"ts\":1640995200"), true)
  
  // 计算JSON压缩率
  let json_compression_ratio = compressed_json.length().to_double() / original_json.length().to_double()
  let json_compression_percentage = (1.0 - json_compression_ratio) * 100.0
  
  // 验证JSON压缩效果
  assert_eq(json_compression_ratio < 1.0, true)
  assert_eq(json_compression_percentage > 20.0, true) // 至少20%的压缩率
  
  // 创建JSON压缩报告
  let json_compression_report = "JSON Original: " + original_json.length().to_string() + 
                                ", Compressed: " + compressed_json.length().to_string() + 
                                ", Savings: " + json_compression_percentage.to_string().slice(0, 5) + "%"
  
  // 验证JSON压缩报告
  assert_eq(json_compression_report.contains("JSON Original:"), true)
  assert_eq(json_compression_report.contains("Savings:"), true)
}

test "telemetry_batch_compression" {
  // 测试遥测批量数据压缩
  
  // 创建批量遥测数据
  let batch_data = []
  let base_timestamp = 1640995200L
  let mut i = 0
  while i < 10 {
    let record = {
      "timestamp": (base_timestamp + i.to_int64()).to_string(),
      "metric": "metric_" + i.to_string(),
      "value": (100.0 + i.to_double()).to_string(),
      "service": "service-" + i.to_string()
    }
    batch_data.push(record)
    i = i + 1
  }
  
  // 验证批量数据
  assert_eq(batch_data.length(), 10)
  assert_eq(batch_data[0]["timestamp"], "1640995200")
  assert_eq(batch_data[9]["timestamp"], "1640995209")
  
  // 计算原始批量数据大小
  let mut original_batch_size = 0
  i = 0
  while i < batch_data.length() {
    let record = batch_data[i]
    let mut record_size = 0
    
    // 计算每个记录的大小（简化版）
    record_size = record_size + record["timestamp"].length()
    record_size = record_size + record["metric"].length()
    record_size = record_size + record["value"].length()
    record_size = record_size + record["service"].length()
    record_size = record_size + 12 // 键名的长度开销
    
    original_batch_size = original_batch_size + record_size
    i = i + 1
  }
  
  // 批量压缩：使用字典压缩
  let compression_dictionary = [
    ("timestamp", "ts"),
    ("metric", "m"),
    ("value", "v"),
    ("service", "s"),
    ("metric_", "mt_"),
    ("service-", "s-")
  ]
  
  // 应用字典压缩
  let compressed_batch = []
  i = 0
  while i < batch_data.length() {
    let compressed_record = {
      "ts": batch_data[i]["timestamp"],
      "m": batch_data[i]["metric"].replace("metric_", "mt_"),
      "v": batch_data[i]["value"],
      "s": batch_data[i]["service"].replace("service-", "s-")
    }
    compressed_batch.push(compressed_record)
    i = i + 1
  }
  
  // 计算压缩后的批量数据大小
  let mut compressed_batch_size = 0
  i = 0
  while i < compressed_batch.length() {
    let record = compressed_batch[i]
    let mut record_size = 0
    
    record_size = record_size + record["ts"].length()
    record_size = record_size + record["m"].length()
    record_size = record_size + record["v"].length()
    record_size = record_size + record["s"].length()
    record_size = record_size + 6 // 压缩后键名的长度开销
    
    compressed_batch_size = compressed_batch_size + record_size
    i = i + 1
  }
  
  // 验证批量压缩效果
  assert_eq(compressed_batch_size < original_batch_size, true)
  assert_eq(compressed_batch.length(), 10)
  assert_eq(compressed_batch[0]["ts"], "1640995200")
  assert_eq(compressed_batch[9]["s"], "s-9")
  
  // 计算批量压缩率
  let batch_compression_ratio = compressed_batch_size.to_double() / original_batch_size.to_double()
  let batch_compression_percentage = (1.0 - batch_compression_ratio) * 100.0
  
  // 验证批量压缩率
  assert_eq(batch_compression_ratio < 1.0, true)
  assert_eq(batch_compression_percentage > 10.0, true)
}

test "telemetry_delta_compression" {
  // 测试遥测增量压缩
  
  // 创建时间序列数据
  let timestamps = [1640995200L, 1640995201L, 1640995202L, 1640995203L, 1640995204L]
  let values = [100.0, 102.5, 98.3, 105.7, 101.2]
  
  // 验证时间序列数据
  assert_eq(timestamps.length(), 5)
  assert_eq(values.length(), 5)
  assert_eq(timestamps[1] - timestamps[0], 1L) // 连续时间戳
  assert_eq(timestamps[4] - timestamps[0], 4L)
  
  // 增量压缩时间戳（存储相对于第一个时间戳的差值）
  let base_timestamp = timestamps[0]
  let delta_timestamps = []
  let mut i = 0
  while i < timestamps.length() {
    let delta = timestamps[i] - base_timestamp
    delta_timestamps.push(delta)
    i = i + 1
  }
  
  // 验证增量时间戳
  assert_eq(delta_timestamps.length(), 5)
  assert_eq(delta_timestamps[0], 0L)
  assert_eq(delta_timestamps[1], 1L)
  assert_eq(delta_timestamps[4], 4L)
  
  // 增量压缩数值（存储相对于前一个值的差值）
  let delta_values = []
  delta_values.push(values[0]) // 第一个值保持原样
  
  i = 1
  while i < values.length() {
    let delta = values[i] - values[i - 1]
    delta_values.push(delta)
    i = i + 1
  }
  
  // 验证增量数值
  assert_eq(delta_values.length(), 5)
  assert_eq(delta_values[0], 100.0) // 原始值
  assert_eq(delta_values[1], 2.5)   // 102.5 - 100.0
  assert_eq(delta_values[2], -4.2)  // 98.3 - 102.5
  assert_eq(delta_values[4], -4.5)  // 101.2 - 105.7
  
  // 计算压缩效果（简化：假设增量值使用更少的字符）
  let mut original_chars = 0
  let mut compressed_chars = 0
  
  // 原始时间戳字符数
  i = 0
  while i < timestamps.length() {
    original_chars = original_chars + timestamps[i].to_string().length()
    i = i + 1
  }
  
  // 压缩时间戳字符数（增量通常更小）
  i = 0
  while i < delta_timestamps.length() {
    compressed_chars = compressed_chars + delta_timestamps[i].to_string().length()
    i = i + 1
  }
  
  // 验证时间戳压缩效果
  assert_eq(compressed_chars < original_chars, true)
  
  // 创建增量压缩报告
  let delta_compression_report = "Timestamps - Original: " + original_chars.to_string() + 
                                 " chars, Delta: " + compressed_chars.to_string() + 
                                 " chars, Savings: " + 
                                 ((1.0 - compressed_chars.to_double() / original_chars.to_double()) * 100.0).to_string().slice(0, 5) + "%"
  
  // 验证增量压缩报告
  assert_eq(delta_compression_report.contains("Timestamps - Original:"), true)
  assert_eq(delta_compression_report.contains("Delta:"), true)
  assert_eq(delta_compression_report.contains("Savings:"), true)
}

test "telemetry_compression_performance" {
  // 测试遥测压缩性能
  
  let data_size = 1000
  let compression_start_time = 1000000L
  let decompression_start_time = 1005000L
  
  // 验证性能参数
  assert_eq(data_size, 1000)
  assert_eq(compression_start_time, 1000000L)
  assert_eq(decompression_start_time, 1005000L)
  
  // 模拟压缩时间
  let compression_duration = 500L // 0.5ms
  let decompression_duration = 200L // 0.2ms
  
  // 验证压缩时间
  assert_eq(compression_duration > 0L, true)
  assert_eq(decompression_duration > 0L, true)
  
  // 计算压缩吞吐量（数据大小/时间）
  let compression_throughput = data_size.to_double() / compression_duration.to_double() * 1000.0
  let decompression_throughput = data_size.to_double() / decompression_duration.to_double() * 1000.0
  
  // 验证吞吐量
  assert_eq(compression_throughput > 1000.0, true) // > 1000 data units per second
  assert_eq(decompression_throughput > 2000.0, true) // > 2000 data units per second
  assert_eq(decompression_throughput > compression_throughput, true) // 解压缩应该更快
  
  // 计算压缩比和性能指标
  let original_size = data_size
  let compressed_size = 300 // 假设压缩到30%
  let compression_ratio = compressed_size.to_double() / original_size.to_double()
  let compression_speed = compression_throughput
  let decompression_speed = decompression_throughput
  
  // 验证压缩比
  assert_eq(compression_ratio < 1.0, true)
  assert_eq(compression_ratio > 0.1, true) // 压缩比应该在10%-100%之间
  
  // 创建性能报告
  let performance_report = "Data Size: " + original_size.to_string() + 
                          ", Compressed: " + compressed_size.to_string() + 
                          ", Ratio: " + (compression_ratio * 100.0).to_string().slice(0, 5) + "%" +
                          ", Compression Speed: " + compression_speed.to_string().slice(0, 8) + 
                          ", Decompression Speed: " + decompression_speed.to_string().slice(0, 8)
  
  // 验证性能报告
  assert_eq(performance_report.contains("Data Size: 1000"), true)
  assert_eq(performance_report.contains("Compressed: 300"), true)
  assert_eq(performance_report.contains("Ratio:"), true)
  assert_eq(performance_report.contains("Compression Speed:"), true)
  assert_eq(performance_report.contains("Decompression Speed:"), true)
}