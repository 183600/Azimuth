// Azimuth 遥测数据压缩和存储优化测试用例
// 专注于测试遥测系统的数据压缩、存储优化和性能

// 测试1: 简单行程编码压缩算法
test "行程编码压缩算法" {
  // 模拟具有重复值的遥测数据
  let original_data = [45.0, 45.0, 45.0, 47.0, 47.0, 45.0, 45.0, 48.0, 48.0, 48.0, 48.0]
  
  // 行程编码压缩
  let mut compressed = []
  if original_data.length() > 0 {
    let mut current_value = original_data[0]
    let mut count = 1
    
    for i in range(1, original_data.length()) {
      if original_data[i] == current_value {
        count = count + 1
      } else {
        compressed = compressed.push({ value: current_value, count: count })
        current_value = original_data[i]
        count = 1
      }
    }
    compressed = compressed.push({ value: current_value, count: count })
  }
  
  // 验证压缩结果
  assert_eq(compressed.length(), 4)
  assert_eq(compressed[0].value, 45.0)
  assert_eq(compressed[0].count, 3)
  assert_eq(compressed[1].value, 47.0)
  assert_eq(compressed[1].count, 2)
  assert_eq(compressed[2].value, 45.0)
  assert_eq(compressed[2].count, 2)
  assert_eq(compressed[3].value, 48.0)
  assert_eq(compressed[3].count, 4)
  
  // 计算压缩率
  let original_size = original_data.length()
  let compressed_size = compressed.length() * 2  // 每个压缩项包含值和计数
  let compression_ratio = original_size.to_float() / compressed_size.to_float()
  
  assert_true(compression_ratio > 1.0)  // 确保压缩有效
  assert_eq(compression_ratio, 11.0 / 8.0)
  
  // 解压缩验证
  let mut decompressed = []
  for item in compressed {
    for i in range(0, item.count) {
      decompressed = decompressed.push(item.value)
    }
  }
  
  // 验证解压缩结果与原始数据相同
  assert_eq(decompressed.length(), original_data.length())
  for i in range(0, original_data.length()) {
    assert_eq(decompressed[i], original_data[i])
  }
}

// 测试2: 增量编码压缩
test "增量编码压缩" {
  // 时间序列遥测数据
  let time_series_data = [
    { timestamp: 1640995200, value: 100.0 },
    { timestamp: 1640995260, value: 102.0 },
    { timestamp: 1640995320, value: 101.0 },
    { timestamp: 1640995380, value: 105.0 },
    { timestamp: 1640995440, value: 103.0 },
    { timestamp: 1640995500, value: 107.0 },
    { timestamp: 1640995560, value: 106.0 },
    { timestamp: 1640995620, value: 108.0 }
  ]
  
  // 提取值序列
  let values = time_series_data.map(d => d.value)
  
  // 增量编码
  let mut delta_encoded = []
  if values.length() > 0 {
    delta_encoded = delta_encoded.push(values[0])  // 第一个值保持不变
    for i in range(1, values.length()) {
      delta_encoded = delta_encoded.push(values[i] - values[i-1])
    }
  }
  
  // 验证增量编码结果
  assert_eq(delta_encoded.length(), values.length())
  assert_eq(delta_encoded[0], 100.0)  // 第一个值不变
  assert_eq(delta_encoded[1], 2.0)    // 102.0 - 100.0
  assert_eq(delta_encoded[2], -1.0)   // 101.0 - 102.0
  assert_eq(delta_encoded[3], 4.0)    // 105.0 - 101.0
  assert_eq(delta_encoded[4], -2.0)   // 103.0 - 105.0
  assert_eq(delta_encoded[5], 4.0)    // 107.0 - 103.0
  assert_eq(delta_encoded[6], -1.0)   // 106.0 - 107.0
  assert_eq(delta_encoded[7], 2.0)    // 108.0 - 106.0
  
  // 解码验证
  let mut decoded = []
  if delta_encoded.length() > 0 {
    decoded = decoded.push(delta_encoded[0])
    for i in range(1, delta_encoded.length()) {
      let prev_value = decoded[i-1]
      decoded = decoded.push(prev_value + delta_encoded[i])
    }
  }
  
  // 验证解码结果与原始数据相同
  assert_eq(decoded.length(), values.length())
  for i in range(0, values.length()) {
    assert_eq(decoded[i], values[i])
  }
}

// 测试3: 时间戳压缩
test "时间戳压缩" {
  // 时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 100.0 },
    { timestamp: 1640995260, value: 102.0 },
    { timestamp: 1640995320, value: 101.0 },
    { timestamp: 1640995380, value: 105.0 },
    { timestamp: 1640995440, value: 103.0 }
  ]
  
  // 提取时间戳
  let timestamps = time_series_data.map(d => d.timestamp)
  
  // 时间戳压缩（存储第一个时间戳和后续的增量）
  let mut compressed_timestamps = []
  if timestamps.length() > 0 {
    compressed_timestamps = compressed_timestamps.push(timestamps[0])
    for i in range(1, timestamps.length()) {
      let delta = timestamps[i] - timestamps[i-1]
      compressed_timestamps = compressed_timestamps.push(delta)
    }
  }
  
  // 验证压缩结果
  assert_eq(compressed_timestamps.length(), timestamps.length())
  assert_eq(compressed_timestamps[0], 1640995200)  // 第一个时间戳不变
  assert_eq(compressed_timestamps[1], 60)          // 增量
  assert_eq(compressed_timestamps[2], 60)          // 增量
  assert_eq(compressed_timestamps[3], 60)          // 增量
  assert_eq(compressed_timestamps[4], 60)          // 增量
  
  // 解压缩验证
  let mut decompressed_timestamps = []
  if compressed_timestamps.length() > 0 {
    decompressed_timestamps = decompressed_timestamps.push(compressed_timestamps[0])
    for i in range(1, compressed_timestamps.length()) {
      let prev_timestamp = decompressed_timestamps[i-1]
      decompressed_timestamps = decompressed_timestamps.push(prev_timestamp + compressed_timestamps[i])
    }
  }
  
  // 验证解压缩结果与原始数据相同
  assert_eq(decompressed_timestamps.length(), timestamps.length())
  for i in range(0, timestamps.length()) {
    assert_eq(decompressed_timestamps[i], timestamps[i])
  }
}

// 测试4: 数据采样策略
test "数据采样策略" {
  // 高频遥测数据（每秒一个数据点，共100个点）
  let mut high_frequency_data = []
  for i in range(0, 100) {
    let data_point = {
      timestamp: 1640995200 + i,
      value: 50.0 + (i % 10).to_float() * 0.5,  // 模拟波动
      metric_id: "cpu_usage"
    }
    high_frequency_data = high_frequency_data.push(data_point)
  }
  
  // 策略1: 均匀采样（每10个点取1个）
  let sampling_interval = 10
  let mut uniform_sampled = []
  for i in range(0, high_frequency_data.length()) {
    if i % sampling_interval == 0 {
      uniform_sampled = uniform_sampled.push(high_frequency_data[i])
    }
  }
  
  // 验证均匀采样结果
  assert_eq(uniform_sampled.length(), 10)
  assert_eq(uniform_sampled[0].timestamp, 1640995200)
  assert_eq(uniform_sampled[9].timestamp, 1640995290)
  
  // 策略2: 最大值采样（每10个点取最大值）
  let mut max_sampled = []
  for i in range(0, high_frequency_data.length() / sampling_interval) {
    let start_index = i * sampling_interval
    let end_index = start_index + sampling_interval
    
    let mut max_value = high_frequency_data[start_index].value
    let mut max_index = start_index
    
    for j in range(start_index + 1, end_index) {
      if high_frequency_data[j].value > max_value {
        max_value = high_frequency_data[j].value
        max_index = j
      }
    }
    
    max_sampled = max_sampled.push(high_frequency_data[max_index])
  }
  
  // 验证最大值采样结果
  assert_eq(max_sampled.length(), 10)
  
  // 策略3: 平均值采样（每10个点取平均值）
  let mut avg_sampled = []
  for i in range(0, high_frequency_data.length() / sampling_interval) {
    let start_index = i * sampling_interval
    let end_index = start_index + sampling_interval
    
    let mut sum = 0.0
    for j in range(start_index, end_index) {
      sum = sum + high_frequency_data[j].value
    }
    let avg_value = sum / sampling_interval.to_float()
    
    let avg_point = {
      timestamp: high_frequency_data[start_index + sampling_interval / 2].timestamp,
      value: avg_value,
      metric_id: "cpu_usage_avg"
    }
    avg_sampled = avg_sampled.push(avg_point)
  }
  
  // 验证平均值采样结果
  assert_eq(avg_sampled.length(), 10)
  
  // 计算采样率
  let original_count = high_frequency_data.length()
  let sampled_count = uniform_sampled.length()
  let sampling_rate = sampled_count.to_float() / original_count.to_float()
  
  assert_eq(sampling_rate, 0.1)  // 10%采样率
}

// 测试5: 数据分块存储
test "数据分块存储" {
  // 大量遥测数据
  let mut large_dataset = []
  for i in range(0, 1000) {
    let data_point = {
      timestamp: 1640995200 + i * 60,  // 每分钟一个数据点
      value: 50.0 + (i % 20).to_float(),
      metric_id: "memory_usage",
      tags: { host: "server" + (i % 5).to_string(), region: "us-west" }
    }
    large_dataset = large_dataset.push(data_point)
  }
  
  // 分块策略：按时间分块（每小时一块）
  let chunk_duration = 3600  // 1小时 = 3600秒
  let mut chunks = []  // 键：时间块ID，值：数据块
  
  for data_point in large_dataset {
    let chunk_id = data_point.timestamp / chunk_duration
    
    // 查找或创建块
    let mut chunk_found = false
    let mut updated_chunks = []
    
    for chunk in chunks {
      if chunk.id == chunk_id {
        // 添加数据点到现有块
        let updated_chunk = {
          id: chunk.id,
          start_time: chunk.start_time,
          end_time: chunk.end_time,
          data: chunk.data.push(data_point),
          count: chunk.count + 1
        }
        updated_chunks = updated_chunks.push(updated_chunk)
        chunk_found = true
      } else {
        updated_chunks = updated_chunks.push(chunk)
      }
    }
    
    if not chunk_found {
      // 创建新块
      let new_chunk = {
        id: chunk_id,
        start_time: data_point.timestamp,
        end_time: data_point.timestamp,
        data: [data_point],
        count: 1
      }
      updated_chunks = updated_chunks.push(new_chunk)
    }
    
    chunks = updated_chunks
  }
  
  // 验证分块结果
  let expected_chunks = 1000 / 60  // 1000个点，每分钟一个，每小时60个点
  assert_true(chunks.length() > 0)
  
  // 验证每个块的数据
  for chunk in chunks {
    assert_true(chunk.count > 0)
    assert_eq(chunk.data.length(), chunk.count)
    
    // 验证块内数据的时间范围
    let mut min_time = chunk.data[0].timestamp
    let mut max_time = chunk.data[0].timestamp
    
    for data_point in chunk.data {
      if data_point.timestamp < min_time {
        min_time = data_point.timestamp
      }
      if data_point.timestamp > max_time {
        max_time = data_point.timestamp
      }
    }
    
    assert_eq(min_time, chunk.start_time)
    assert_eq(max_time, chunk.end_time)
  }
}

// 测试6: 存储空间优化
test "存储空间优化" {
  // 模拟原始遥测数据
  let original_data = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 45.5, host: "server1", region: "us-west" },
    { timestamp: 1640995260, metric_name: "cpu_usage", value: 46.0, host: "server1", region: "us-west" },
    { timestamp: 1640995320, metric_name: "cpu_usage", value: 45.8, host: "server1", region: "us-west" },
    { timestamp: 1640995380, metric_name: "memory_usage", value: 1024.0, host: "server1", region: "us-west" },
    { timestamp: 1640995440, metric_name: "memory_usage", value: 1030.0, host: "server1", region: "us-west" }
  ]
  
  // 优化策略1：字符串字典编码
  let mut string_dict = {}  // 字符串到ID的映射
  let mut next_string_id = 0
  
  let mut optimized_data = []
  for data_point in original_data {
    // 处理metric_name
    let metric_id = match string_dict.get(data_point.metric_name) {
      Some(id) => id,
      None => 
        let new_id = next_string_id.to_string()
        string_dict = string_dict.set(data_point.metric_name, new_id)
        next_string_id = next_string_id + 1
        new_id
    }
    
    // 处理host
    let host_id = match string_dict.get(data_point.host) {
      Some(id) => id,
      None => 
        let new_id = next_string_id.to_string()
        string_dict = string_dict.set(data_point.host, new_id)
        next_string_id = next_string_id + 1
        new_id
    }
    
    // 处理region
    let region_id = match string_dict.get(data_point.region) {
      Some(id) => id,
      None => 
        let new_id = next_string_id.to_string()
        string_dict = string_dict.set(data_point.region, new_id)
        next_string_id = next_string_id + 1
        new_id
    }
    
    let optimized_point = {
      timestamp: data_point.timestamp,
      metric_id: metric_id,
      value: data_point.value,
      host_id: host_id,
      region_id: region_id
    }
    optimized_data = optimized_data.push(optimized_point)
  }
  
  // 验证字典编码结果
  assert_eq(string_dict.size(), 3)  // cpu_usage, memory_usage, server1, us-west
  assert_eq(optimized_data.length(), original_data.length())
  
  // 验证第一个数据点
  assert_eq(optimized_data[0].timestamp, 1640995200)
  assert_eq(optimized_data[0].value, 45.5)
  
  // 优化策略2：数值精度优化
  let mut precision_optimized = []
  for data_point in optimized_data {
    // 将浮点数精度降低到小数点后1位
    let rounded_value = round_to_one_decimal(data_point.value)
    
    let precision_point = {
      timestamp: data_point.timestamp,
      metric_id: data_point.metric_id,
      value: rounded_value,
      host_id: data_point.host_id,
      region_id: data_point.region_id
    }
    precision_optimized = precision_optimized.push(precision_point)
  }
  
  // 验证精度优化结果
  assert_eq(precision_optimized.length(), optimized_data.length())
  assert_eq(precision_optimized[0].value, 45.5)
  assert_eq(precision_optimized[1].value, 46.0)
  
  // 计算存储空间节省（简化计算）
  let original_size = original_data.length() * 100  // 假设每个原始数据点100字节
  let optimized_size = optimized_data.length() * 50 + string_dict.size() * 20  // 优化后每个点50字节，字典条目20字节
  
  assert_true(optimized_size < original_size)
  let space_saving_percent = (original_size.to_float() - optimized_size.to_float()) / original_size.to_float() * 100.0
  assert_true(space_saving_percent > 0.0)
}

// 辅助函数：四舍五入到小数点后一位
func round_to_one_decimal(x : Float) -> Float {
  // 简化的四舍五入实现
  let scaled = x * 10.0
  let rounded = if scaled - floor(scaled) >= 0.5 {
    floor(scaled) + 1.0
  } else {
    floor(scaled)
  }
  return rounded / 10.0
}

// 辅助函数：向下取整
func floor(x : Float) -> Float {
  let int_part = x.to_int()
  if x < int_part.to_float() {
    return (int_part - 1).to_float()
  } else {
    return int_part.to_float()
  }
}