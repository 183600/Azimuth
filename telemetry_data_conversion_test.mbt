// 数据转换测试用例

test "telemetry_data_type_conversion" {
  // 测试遥测数据类型转换
  
  let string_values = ["42", "3.14159", "true", "false", "123.456"]
  let expected_ints = [42, 3, 1, 0, 123]
  let expected_floats = [42.0, 3.14159, 1.0, 0.0, 123.456]
  let expected_bools = [true, true, true, false, true]
  
  // 字符串到整数转换
  let mut converted_ints = []
  let mut i = 0
  while i < string_values.length() {
    let str_val = string_values[i]
    let int_val = str_val.to_int()
    converted_ints.push(int_val)
    i = i + 1
  }
  
  // 验证整数转换
  assert_eq(converted_ints.length(), expected_ints.length())
  i = 0
  while i < converted_ints.length() {
    assert_eq(converted_ints[i], expected_ints[i])
    i = i + 1
  }
  
  // 字符串到浮点数转换
  let mut converted_floats = []
  i = 0
  while i < string_values.length() {
    let str_val = string_values[i]
    let float_val = str_val.to_double()
    converted_floats.push(float_val)
    i = i + 1
  }
  
  // 验证浮点数转换
  assert_eq(converted_floats.length(), expected_floats.length())
  i = 0
  while i < converted_floats.length() {
    assert_eq(converted_floats[i], expected_floats[i])
    i = i + 1
  }
  
  // 字符串到布尔值转换
  let mut converted_bools = []
  i = 0
  while i < string_values.length() {
    let str_val = string_values[i]
    let bool_val = str_val != "false" && str_val != "0" && str_val.length() > 0
    converted_bools.push(bool_val)
    i = i + 1
  }
  
  // 验证布尔值转换
  assert_eq(converted_bools.length(), expected_bools.length())
  i = 0
  while i < converted_bools.length() {
    assert_eq(converted_bools[i], expected_bools[i])
    i = i + 1
  }
}

test "telemetry_unit_conversion" {
  // 测试遥测单位转换
  
  let temperature_data = [
    ("celsius", 25.0, "fahrenheit", 77.0),
    ("celsius", 0.0, "fahrenheit", 32.0),
    ("celsius", 100.0, "fahrenheit", 212.0),
    ("kilometers", 5.0, "miles", 3.10686),
    ("kilograms", 70.0, "pounds", 154.324),
    ("bytes", 1024.0, "kilobytes", 1.0)
  ]
  
  // 执行单位转换
  let mut conversion_results = []
  let mut i = 0
  while i < temperature_data.length() {
    let from_unit = temperature_data[i].0
    let from_value = temperature_data[i].1
    let to_unit = temperature_data[i].2
    let expected_value = temperature_data[i].3
    
    let converted_value = 
      if from_unit == "celsius" && to_unit == "fahrenheit" {
        from_value * 9.0 / 5.0 + 32.0
      } else if from_unit == "kilometers" && to_unit == "miles" {
        from_value * 0.621371
      } else if from_unit == "kilograms" && to_unit == "pounds" {
        from_value * 2.20462
      } else if from_unit == "bytes" && to_unit == "kilobytes" {
        from_value / 1024.0
      } else {
        from_value
      }
    
    conversion_results.push((from_unit, from_value, to_unit, converted_value, expected_value))
    i = i + 1
  }
  
  // 验证转换结果（允许浮点数误差）
  i = 0
  while i < conversion_results.length() {
    let converted = conversion_results[i].3
    let expected = conversion_results[i].4
    let error = (converted - expected).abs()
    
    assert_eq(error < 0.01, true) // 允许0.01的误差
    i = i + 1
  }
  
  // 验证特定转换
  assert_eq(conversion_results[0].3, 77.0) // 25°C = 77°F
  assert_eq(conversion_results[1].3, 32.0) // 0°C = 32°F
  assert_eq(conversion_results[4].3 > 154.0 && conversion_results[4].3 < 154.5, true) // 70kg ≈ 154.324lbs
}

test "telemetry_format_conversion" {
  // 测试遥测格式转换
  
  let metric_data = [
    ("cpu_usage", 75.5, "percent"),
    ("memory_usage", 60.2, "percent"),
    ("disk_io", 1024.0, "bytes"),
    ("network_latency", 45.6, "milliseconds")
  ]
  
  // 转换为JSON格式
  let mut json_output = "["
  let mut i = 0
  while i < metric_data.length() {
    let name = metric_data[i].0
    let value = metric_data[i].1
    let unit = metric_data[i].2
    
    json_output = json_output + "{"
    json_output = json_output + "\"name\":\"" + name + "\"," 
    json_output = json_output + "\"value\":" + value.to_string() + ","
    json_output = json_output + "\"unit\":\"" + unit + "\""
    json_output = json_output + "}"
    
    if i < metric_data.length() - 1 {
      json_output = json_output + ","
    }
    
    i = i + 1
  }
  json_output = json_output + "]"
  
  // 验证JSON格式
  assert_eq(json_output.has_prefix("["), true)
  assert_eq(json_output.has_suffix("]"), true)
  assert_eq(json_output.contains("\"name\":\"cpu_usage\""), true)
  assert_eq(json_output.contains("\"value\":75.5"), true)
  assert_eq(json_output.contains("\"unit\":\"percent\""), true)
  
  // 转换为Prometheus格式
  let mut prometheus_output = ""
  i = 0
  while i < metric_data.length() {
    let name = metric_data[i].0
    let value = metric_data[i].1
    let unit = metric_data[i].2
    
    prometheus_output = prometheus_output + name + " " + value.to_string()
    
    if i < metric_data.length() - 1 {
      prometheus_output = prometheus_output + "\n"
    }
    
    i = i + 1
  }
  
  // 验证Prometheus格式
  assert_eq(prometheus_output.contains("cpu_usage 75.5"), true)
  assert_eq(prometheus_output.contains("memory_usage 60.2"), true)
  assert_eq(prometheus_output.contains("\n"), true)
}

test "telemetry_data_normalization" {
  // 测试遥测数据标准化
  
  let raw_values = [100.0, 200.0, 300.0, 400.0, 500.0]
  let expected_min_max = [0.0, 0.25, 0.5, 0.75, 1.0]
  let expected_z_score = [-1.264911064, -0.632455532, 0.0, 0.632455532, 1.264911064]
  
  // Min-Max标准化
  let min_val = 100.0
  let max_val = 500.0
  let range = max_val - min_val
  
  let mut min_max_normalized = []
  let mut i = 0
  while i < raw_values.length() {
    let normalized = (raw_values[i] - min_val) / range
    min_max_normalized.push(normalized)
    i = i + 1
  }
  
  // 验证Min-Max标准化
  assert_eq(min_max_normalized.length(), expected_min_max.length())
  i = 0
  while i < min_max_normalized.length() {
    assert_eq(min_max_normalized[i], expected_min_max[i])
    i = i + 1
  }
  
  // Z-score标准化
  let mut sum = 0.0
  i = 0
  while i < raw_values.length() {
    sum = sum + raw_values[i]
    i = i + 1
  }
  let mean = sum / raw_values.length().to_double()
  
  let mut sum_squared_diff = 0.0
  i = 0
  while i < raw_values.length() {
    let diff = raw_values[i] - mean
    sum_squared_diff = sum_squared_diff + diff * diff
    i = i + 1
  }
  let std_dev = (sum_squared_diff / raw_values.length().to_double()).sqrt()
  
  let mut z_score_normalized = []
  i = 0
  while i < raw_values.length() {
    let z_score = (raw_values[i] - mean) / std_dev
    z_score_normalized.push(z_score)
    i = i + 1
  }
  
  // 验证Z-score标准化（允许浮点数误差）
  i = 0
  while i < z_score_normalized.length() {
    let error = (z_score_normalized[i] - expected_z_score[i]).abs()
    assert_eq(error < 0.001, true) // 允许0.001的误差
    i = i + 1
  }
}

test "telemetry_data_aggregation_conversion" {
  // 测试遥测数据聚合转换
  
  let time_series_data = [
    (1640995200L, 10.0), // 00:00:00
    (1640995230L, 20.0), // 00:00:30
    (1640995260L, 30.0), // 00:01:00
    (1640995290L, 40.0), // 00:01:30
    (1640995320L, 50.0), // 00:02:00
    (1640995350L, 60.0)  // 00:02:30
  ]
  
  // 时间窗口聚合
  let window_size = 60L // 1分钟窗口
  let mut aggregated_data = []
  
  let mut i = 0
  while i < time_series_data.length() {
    let timestamp = time_series_data[i].0
    let value = time_series_data[i].1
    
    // 计算窗口开始时间
    let window_start = (timestamp / window_size) * window_size
    
    // 查找或创建聚合窗口
    let mut found = false
    let mut j = 0
    while j < aggregated_data.length() {
      if aggregated_data[j].0 == window_start {
        // 更新聚合值（求和）
        let current_sum = aggregated_data[j].1
        let current_count = aggregated_data[j].2
        aggregated_data[j] = (window_start, current_sum + value, current_count + 1)
        found = true
        break
      }
      j = j + 1
    }
    
    // 如果没找到，创建新窗口
    if found == false {
      aggregated_data.push((window_start, value, 1))
    }
    
    i = i + 1
  }
  
  // 计算平均值
  let mut averaged_data = []
  i = 0
  while i < aggregated_data.length() {
    let window_start = aggregated_data[i].0
    let sum = aggregated_data[i].1
    let count = aggregated_data[i].2
    let average = sum / count.to_double()
    averaged_data.push((window_start, average, count))
    i = i + 1
  }
  
  // 验证聚合结果
  assert_eq(averaged_data.length(), 3) // 3个时间窗口
  
  // 验证第一个窗口（00:00:00-00:01:00）
  assert_eq(averaged_data[0].0, 1640995200L)
  assert_eq(averaged_data[0].1, 15.0) // (10+20)/2
  assert_eq(averaged_data[0].2, 2)
  
  // 验证第二个窗口（00:01:00-00:02:00）
  assert_eq(averaged_data[1].0, 1640995260L)
  assert_eq(averaged_data[1].1, 35.0) // (30+40)/2
  assert_eq(averaged_data[1].2, 2)
  
  // 验证第三个窗口（00:02:00-00:03:00）
  assert_eq(averaged_data[2].0, 1640995320L)
  assert_eq(averaged_data[2].1, 55.0) // (50+60)/2
  assert_eq(averaged_data[2].2, 2)
}

test "telemetry_data_encoding_conversion" {
  // 测试遥测数据编码转换
  
  let original_data = "Hello, 世界! 🌍"
  
  // Base64编码模拟（简化版本）
  let mut base64_chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  let mut encoded_data = ""
  
  // 简化的Base64编码（仅用于演示）
  let mut i = 0
  while i < original_data.length() {
    let char_code = original_data[i].to_int()
    let encoded_char = base64_chars[char_code % base64_chars.length()]
    encoded_data = encoded_data + encoded_char.to_string()
    i = i + 1
  }
  
  // 验证编码结果
  assert_eq(encoded_data.length(), original_data.length())
  assert_eq(encoded_data != original_data, true) // 编码后应该不同
  
  // URL编码模拟
  let mut url_encoded = ""
  i = 0
  while i < original_data.length() {
    let char = original_data[i]
    
    if (char >= 'A' && char <= 'Z') || (char >= 'a' && char <= 'z') || (char >= '0' && char <= '9') {
      url_encoded = url_encoded + char.to_string()
    } else if char == ' ' {
      url_encoded = url_encoded + "+"
    } else {
      url_encoded = url_encoded + "%" + char.to_int().to_string()
    }
    
    i = i + 1
  }
  
  // 验证URL编码
  assert_eq(url_encoded.contains("Hello"), true)
  assert_eq(url_encoded.contains("+"), true) // 空格被编码为+
  assert_eq(url_encoded.contains("%"), true) // 特殊字符被编码为%xx
  
  // 十六进制编码
  let mut hex_encoded = ""
  i = 0
  while i < original_data.length() {
    let char_code = original_data[i].to_int()
    let hex_high = char_code / 16
    let hex_low = char_code % 16
    
    let hex_chars = "0123456789ABCDEF"
    hex_encoded = hex_encoded + hex_chars[hex_high].to_string() + hex_chars[hex_low].to_string()
    
    i = i + 1
  }
  
  // 验证十六进制编码
  assert_eq(hex_encoded.length(), original_data.length() * 2)
  assert_eq(hex_encoded.contains("48"), true) // 'H' = 0x48
  assert_eq(hex_encoded.contains("65"), true) // 'e' = 0x65
}

test "telemetry_data_sampling_conversion" {
  // 测试遥测数据采样转换
  
  let high_frequency_data = []
  let base_timestamp = 1640995200L
  
  // 生成高频数据（每秒一个点，持续10秒）
  let mut i = 0
  while i < 10 {
    let timestamp = base_timestamp + (i * 1L)
    let value = 100.0 + i.to_double()
    high_frequency_data.push((timestamp, value))
    i = i + 1
  }
  
  // 降采样：每3秒采样一次
  let sampling_interval = 3
  let mut downsampled_data = []
  
  i = 0
  while i < high_frequency_data.length() {
    if i % sampling_interval == 0 {
      downsampled_data.push(high_frequency_data[i])
    }
    i = i + 1
  }
  
  // 验证降采样结果
  assert_eq(downsampled_data.length(), 4) // 10/3 ≈ 3.33，向上取整为4
  assert_eq(downsampled_data[0].0, base_timestamp) // 第0秒
  assert_eq(downsampled_data[1].0, base_timestamp + 3L) // 第3秒
  assert_eq(downsampled_data[2].0, base_timestamp + 6L) // 第6秒
  assert_eq(downsampled_data[3].0, base_timestamp + 9L) // 第9秒
  
  // 升采样：线性插值
  let upsample_rate = 2 // 每个原始点之间插入一个点
  let mut upsampled_data = []
  
  i = 0
  while i < downsampled_data.length() {
    // 添加原始点
    upsampled_data.push(downsampled_data[i])
    
    // 如果不是最后一个点，添加插值点
    if i < downsampled_data.length() - 1 {
      let current_point = downsampled_data[i]
      let next_point = downsampled_data[i + 1]
      
      let interp_timestamp = current_point.0 + (next_point.0 - current_point.0) / upsample_rate
      let interp_value = current_point.1 + (next_point.1 - current_point.1) / upsample_rate.to_double()
      
      upsampled_data.push((interp_timestamp, interp_value))
    }
    
    i = i + 1
  }
  
  // 验证升采样结果
  assert_eq(upsampled_data.length(), 7) // 4个原始点 + 3个插值点
  
  // 验证插值点
  assert_eq(upsampled_data[1].0, base_timestamp + 1L) // 第0秒和第3秒之间的中点
  assert_eq(upsampled_data[1].1, 101.5) // (100.0 + 103.0) / 2
  assert_eq(upsampled_data[3].0, base_timestamp + 4L) // 第3秒和第6秒之间的中点
  assert_eq(upsampled_data[3].1, 104.5) // (103.0 + 106.0) / 2
}

test "telemetry_data_dimension_conversion" {
  // 测试遥测数据维度转换
  
  let one_dimensional_data = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0]
  
  // 一维数组转二维矩阵（2x3）
  let rows = 2
  let cols = 3
  let mut two_dimensional_data = []
  
  let mut i = 0
  while i < rows {
    let mut row = []
    let mut j = 0
    while j < cols {
      let index = i * cols + j
      if index < one_dimensional_data.length() {
        row.push(one_dimensional_data[index])
      }
      j = j + 1
    }
    two_dimensional_data.push(row)
    i = i + 1
  }
  
  // 验证二维转换
  assert_eq(two_dimensional_data.length(), 2)
  assert_eq(two_dimensional_data[0].length(), 3)
  assert_eq(two_dimensional_data[1].length(), 3)
  
  assert_eq(two_dimensional_data[0][0], 1.0)
  assert_eq(two_dimensional_data[0][1], 2.0)
  assert_eq(two_dimensional_data[0][2], 3.0)
  assert_eq(two_dimensional_data[1][0], 4.0)
  assert_eq(two_dimensional_data[1][1], 5.0)
  assert_eq(two_dimensional_data[1][2], 6.0)
  
  // 矩阵转置
  let mut transposed_data = []
  i = 0
  while i < cols {
    let mut new_row = []
    let mut j = 0
    while j < rows {
      new_row.push(two_dimensional_data[j][i])
      j = j + 1
    }
    transposed_data.push(new_row)
    i = i + 1
  }
  
  // 验证转置结果
  assert_eq(transposed_data.length(), 3)
  assert_eq(transposed_data[0].length(), 2)
  assert_eq(transposed_data[1].length(), 2)
  assert_eq(transposed_data[2].length(), 2)
  
  assert_eq(transposed_data[0][0], 1.0)
  assert_eq(transposed_data[0][1], 4.0)
  assert_eq(transposed_data[1][0], 2.0)
  assert_eq(transposed_data[1][1], 5.0)
  assert_eq(transposed_data[2][0], 3.0)
  assert_eq(transposed_data[2][1], 6.0)
  
  // 扁平化回一维数组
  let mut flattened_data = []
  i = 0
  while i < transposed_data.length() {
    let mut j = 0
    while j < transposed_data[i].length() {
      flattened_data.push(transposed_data[i][j])
      j = j + 1
    }
    i = i + 1
  }
  
  // 验证扁平化结果
  assert_eq(flattened_data.length(), one_dimensional_data.length())
  
  // 验证数据顺序（转置后应该不同）
  assert_eq(flattened_data[0], 1.0)
  assert_eq(flattened_data[1], 4.0)
  assert_eq(flattened_data[2], 2.0)
  assert_eq(flattened_data[3], 5.0)
  assert_eq(flattened_data[4], 3.0)
  assert_eq(flattened_data[5], 6.0)
}