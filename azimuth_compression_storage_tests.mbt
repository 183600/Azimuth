// Azimuth 压缩存储测试套件
// 专注于遥测数据的压缩算法和存储优化功能

// 测试1: 时间序列数据压缩
test "时间序列数据压缩" {
  // 模拟高频时间序列数据
  let raw_time_series = []
  let mut i = 0
  while i < 1440 {  // 24小时，每分钟一个数据点
    let base_value = 50.0
    let trend = 0.01 * i.to_float()  // 缓慢上升趋势
    let daily_pattern = 10.0 * ((i % 720).to_float() / 720.0 * 3.14159).sin()  // 日周期模式
    let noise = (i % 7).to_float() - 3.0  // 小幅随机噪声
    
    raw_time_series = raw_time_series.push({
      timestamp: 1640995200 + i * 60,
      value: base_value + trend + daily_pattern + noise,
      metric: "cpu_usage"
    })
    i = i + 1
  }
  
  // 简单增量压缩算法
  let delta_compression = fn(data: Array[Dynamic]) {
    if data.length() == 0 {
      { compressed: [], original_size: 0, compressed_size: 0 }
    } else {
      let mut compressed = []
      
      // 存储第一个值作为基准
      compressed = compressed.push({
        type: "base",
        timestamp: data[0].timestamp,
        value: data[0].value
      })
      
      // 存储增量
      let mut i = 1
      while i < data.length() {
        let timestamp_delta = data[i].timestamp - data[i-1].timestamp
        let value_delta = data[i].value - data[i-1].value
        
        compressed = compressed.push({
          type: "delta",
          timestamp_delta: timestamp_delta,
          value_delta: value_delta
        })
        i = i + 1
      }
      
      let original_size = data.length() * 16  // 假设每个数据点16字节
      let compressed_size = compressed.length() * 8  // 假设压缩后每个点8字节
      
      { compressed, original_size, compressed_size }
    }
  }
  
  // 解压缩算法
  let delta_decompression = fn(compressed: Array[Dynamic]) {
    if compressed.length() == 0 {
      []
    } else {
      let mut decompressed = []
      
      // 恢复第一个值
      let base = compressed[0]
      decompressed = decompressed.push({
        timestamp: base.timestamp,
        value: base.value,
        metric: "cpu_usage"
      })
      
      // 恢复增量
      let mut i = 1
      while i < compressed.length() {
        let delta = compressed[i]
        let prev = decompressed[i-1]
        
        decompressed = decompressed.push({
          timestamp: prev.timestamp + delta.timestamp_delta,
          value: prev.value + delta.value_delta,
          metric: "cpu_usage"
        })
        i = i + 1
      }
      
      decompressed
    }
  }
  
  // 执行压缩和解压缩
  let compression_result = delta_compression(raw_time_series)
  let decompressed_data = delta_decompression(compression_result.compressed)
  
  // 验证压缩效果
  assert_true(compression_result.compressed_size < compression_result.original_size)
  let compression_ratio = compression_result.compressed_size.to_float() / compression_result.original_size.to_float()
  assert_true(compression_ratio < 0.8)  // 压缩率至少20%
  
  // 验证数据完整性
  assert_eq(decompressed_data.length(), raw_time_series.length())
  
  // 验证前几个数据点
  assert_eq(decompressed_data[0].timestamp, raw_time_series[0].timestamp)
  assert_eq(decompressed_data[0].value, raw_time_series[0].value)
  assert_eq(decompressed_data[1].timestamp, raw_time_series[1].timestamp)
  assert_eq(decompressed_data[1].value, raw_time_series[1].value)
  
  // 验证最后几个数据点
  let last_index = raw_time_series.length() - 1
  assert_eq(decompressed_data[last_index].timestamp, raw_time_series[last_index].timestamp)
  assert_eq(decompressed_data[last_index].value, raw_time_series[last_index].value)
  
  // 计算压缩误差
  let mut total_error = 0.0
  let mut i = 0
  while i < raw_time_series.length() {
    let error = (decompressed_data[i].value - raw_time_series[i].value).abs()
    total_error = total_error + error
    i = i + 1
  }
  let avg_error = total_error / raw_time_series.length().to_float()
  
  // 验证压缩误差很小
  assert_true(avg_error < 0.001)  // 平均误差应该非常小
}

// 测试2: 采样压缩算法
test "采样压缩算法" {
  // 模拟高频遥测数据
  let high_frequency_data = []
  let mut i = 0
  while i < 10000 {  // 10000个高频数据点
    high_frequency_data = high_frequency_data.push({
      timestamp: 1640995200 + i,
      value: 50.0 + 10.0 * ((i % 100).to_float() / 100.0 * 3.14159).sin(),
      metric: "memory_usage",
      service: "web-service"
    })
    i = i + 1
  }
  
  // 时间窗口采样压缩
  let time_window_sampling = fn(data: Array[Dynamic], window_size: Int, sampling_strategy: String) {
    if data.length() == 0 {
      []
    } else {
      let mut sampled = []
      let mut window_start = data[0].timestamp
      
      while window_start <= data[data.length() - 1].timestamp {
        let window_end = window_start + window_size
        
        // 收集窗口内的数据
        let mut window_data = []
        for point in data {
          if point.timestamp >= window_start and point.timestamp < window_end {
            window_data = window_data.push(point)
          }
        }
        
        if window_data.length() > 0 {
          let sampled_point = match sampling_strategy {
            "first" => window_data[0],
            "last" => window_data[window_data.length() - 1],
            "min" => {
              let mut min_point = window_data[0]
              for point in window_data {
                if point.value < min_point.value {
                  min_point = point
                }
              }
              min_point
            },
            "max" => {
              let mut max_point = window_data[0]
              for point in window_data {
                if point.value > max_point.value {
                  max_point = point
                }
              }
              max_point
            },
            "avg" => {
              let mut sum = 0.0
              let mut count = 0
              for point in window_data {
                sum = sum + point.value
                count = count + 1
              }
              {
                timestamp: window_start + window_size / 2,
                value: sum / count.to_float(),
                metric: "memory_usage",
                service: "web-service"
              }
            },
            _ => window_data[0]
          }
          
          sampled = sampled.push(sampled_point)
        }
        
        window_start = window_end
      }
      
      sampled
    }
  }
  
  // 执行不同采样策略
  let first_sampled = time_window_sampling(high_frequency_data, 100, "first")
  let last_sampled = time_window_sampling(high_frequency_data, 100, "last")
  let min_sampled = time_window_sampling(high_frequency_data, 100, "min")
  let max_sampled = time_window_sampling(high_frequency_data, 100, "max")
  let avg_sampled = time_window_sampling(high_frequency_data, 100, "avg")
  
  // 验证采样效果
  let expected_sample_count = 10000 / 100  // 预期采样点数
  assert_eq(first_sampled.length(), expected_sample_count)
  assert_eq(last_sampled.length(), expected_sample_count)
  assert_eq(min_sampled.length(), expected_sample_count)
  assert_eq(max_sampled.length(), expected_sample_count)
  assert_eq(avg_sampled.length(), expected_sample_count)
  
  // 验证压缩率
  let compression_ratio = expected_sample_count.to_float() / 10000.0
  assert_eq(compression_ratio, 0.01)  // 100:1的压缩率
  
  // 验证不同采样策略的差异
  assert_true(first_sampled[0].timestamp <= last_sampled[0].timestamp)
  assert_true(min_sampled[0].value <= max_sampled[0].value)
  
  // 验证采样数据的时间连续性
  let mut i = 0
  while i < first_sampled.length() - 1 {
    assert_true(first_sampled[i].timestamp < first_sampled[i + 1].timestamp)
    assert_true(last_sampled[i].timestamp < last_sampled[i + 1].timestamp)
    i = i + 1
  }
  
  // 计算采样误差
  let calculate_sampling_error = fn(original: Array[Dynamic], sampled: Array[Dynamic]) {
    let mut total_error = 0.0
    let mut sample_count = 0
    
    for sample in sampled {
      // 找到原始数据中最接近的点
      let mut closest_point = original[0]
      let mut min_distance = (sample.timestamp - original[0].timestamp).abs()
      
      for point in original {
        let distance = (sample.timestamp - point.timestamp).abs()
        if distance < min_distance {
          min_distance = distance
          closest_point = point
        }
      }
      
      let error = (sample.value - closest_point.value).abs()
      total_error = total_error + error
      sample_count = sample_count + 1
    }
    
    if sample_count > 0 {
      total_error / sample_count.to_float()
    } else {
      0.0
    }
  }
  
  // 验证不同采样策略的误差
  let first_error = calculate_sampling_error(high_frequency_data, first_sampled)
  let last_error = calculate_sampling_error(high_frequency_data, last_sampled)
  let avg_error = calculate_sampling_error(high_frequency_data, avg_sampled)
  
  assert_true(first_error >= 0.0)
  assert_true(last_error >= 0.0)
  assert_true(avg_error >= 0.0)
  
  // 平均采样策略通常误差较小
  assert_true(avg_error <= first_error + 5.0)
  assert_true(avg_error <= last_error + 5.0)
}

// 测试3: 自适应压缩算法
test "自适应压缩算法" {
  // 模拟不同变化率的遥测数据
  let variable_rate_data = []
  let mut i = 0
  while i < 1000 {
    let value = if i < 300 {
      // 稳定期：变化很小
      50.0 + (i % 10).to_float() * 0.1
    } else if i < 600 {
      // 活跃期：变化较大
      50.0 + 20.0 * ((i % 50).to_float() / 50.0 * 3.14159).sin()
    } else {
      // 过渡期：中等变化
      50.0 + 10.0 * ((i % 25).to_float() / 25.0 * 3.14159).sin()
    }
    
    variable_rate_data = variable_rate_data.push({
      timestamp: 1640995200 + i * 10,
      value: value,
      metric: "disk_io",
      node: "server-" + (i % 5).to_string()
    })
    i = i + 1
  }
  
  // 自适应压缩算法
  let adaptive_compression = fn(data: Array[Dynamic]) {
    let mut compressed = []
    let mut error_threshold = 0.5  // 可接受的误差阈值
    
    // 添加第一个点作为基准
    compressed = compressed.push(data[0])
    let last_compressed = data[0]
    
    let mut i = 1
    while i < data.length() {
      let current_point = data[i]
      
      // 计算与上一个压缩点的线性插值误差
      let time_ratio = (current_point.timestamp - last_compressed.timestamp).to_float() / 
                       (data[data.length() - 1].timestamp - last_compressed.timestamp).to_float()
      let predicted_value = last_compressed.value  // 简化：使用上一个值作为预测
      let prediction_error = (current_point.value - predicted_value).abs()
      
      // 如果误差超过阈值，添加这个点到压缩数据中
      if prediction_error > error_threshold {
        compressed = compressed.push(current_point)
        last_compressed = current_point
      }
      
      i = i + 1
    }
    
    // 确保最后一个点被包含
    if compressed[compressed.length() - 1].timestamp != data[data.length() - 1].timestamp {
      compressed = compressed.push(data[data.length() - 1])
    }
    
    compressed
  }
  
  // 执行自适应压缩
  let compressed_data = adaptive_compression(variable_rate_data)
  
  // 验证压缩效果
  assert_true(compressed_data.length() < variable_rate_data.length())
  let compression_ratio = compressed_data.length().to_float() / variable_rate_data.length().to_float()
  assert_true(compression_ratio < 0.5)  // 压缩率至少50%
  
  // 验证压缩数据包含原始数据的开始和结束点
  assert_eq(compressed_data[0].timestamp, variable_rate_data[0].timestamp)
  assert_eq(compressed_data[0].value, variable_rate_data[0].value)
  assert_eq(compressed_data[compressed_data.length() - 1].timestamp, variable_rate_data[variable_rate_data.length() - 1].timestamp)
  assert_eq(compressed_data[compressed_data.length() - 1].value, variable_rate_data[variable_rate_data.length() - 1].value)
  
  // 验证压缩数据的时间顺序
  let mut i = 0
  while i < compressed_data.length() - 1 {
    assert_true(compressed_data[i].timestamp < compressed_data[i + 1].timestamp)
    i = i + 1
  }
  
  // 计算重构误差
  let calculate_reconstruction_error = fn(original: Array[Dynamic], compressed: Array[Dynamic]) {
    let mut total_error = 0.0
    let mut point_count = 0
    
    let mut compressed_index = 0
    let mut i = 0
    while i < original.length() {
      let original_point = original[i]
      
      // 找到压缩数据中用于重构的点
      while compressed_index < compressed_data.length() - 1 and 
            compressed_data[compressed_index + 1].timestamp <= original_point.timestamp {
        compressed_index = compressed_index + 1
      }
      
      let compressed_point = compressed_data[compressed_index]
      let reconstruction_error = (original_point.value - compressed_point.value).abs()
      
      total_error = total_error + reconstruction_error
      point_count = point_count + 1
      i = i + 1
    }
    
    if point_count > 0 {
      total_error / point_count.to_float()
    } else {
      0.0
    }
  }
  
  // 验证重构误差
  let reconstruction_error = calculate_reconstruction_error(variable_rate_data, compressed_data)
  assert_true(reconstruction_error < 5.0)  // 平均重构误差应该小于5
  
  // 分析不同数据段的压缩效果
  let analyze_segment_compression = fn(original: Array[Dynamic], compressed: Array[Dynamic], start: Int, end: Int) {
    let mut original_points = 0
    let mut compressed_points = 0
    
    let mut i = start
    while i <= end and i < original.length() {
      original_points = original_points + 1
      i = i + 1
    }
    
    for point in compressed {
      if point.timestamp >= original[start].timestamp and point.timestamp <= original[end].timestamp {
        compressed_points = compressed_points + 1
      }
    }
    
    {
      segment_size: original_points,
      compressed_size: compressed_points,
      compression_ratio: compressed_points.to_float() / original_points.to_float()
    }
  }
  
  // 分析稳定期的压缩效果
  let stable_segment = analyze_segment_compression(variable_rate_data, compressed_data, 0, 299)
  
  // 分析活跃期的压缩效果
  let active_segment = analyze_segment_compression(variable_rate_data, compressed_data, 300, 599)
  
  // 分析过渡期的压缩效果
  let transition_segment = analyze_segment_compression(variable_rate_data, compressed_data, 600, 999)
  
  // 验证自适应效果：稳定期应该有更高的压缩率
  assert_true(stable_segment.compression_ratio <= active_segment.compression_ratio + 0.1)
  assert_true(stable_segment.compression_ratio <= transition_segment.compression_ratio + 0.1)
}

// 测试4: 多维度数据压缩
test "多维度数据压缩" {
  // 模拟多维度遥测数据
  let multi_dimensional_data = []
  let mut i = 0
  while i < 500 {
    multi_dimensional_data = multi_dimensional_data.push({
      timestamp: 1640995200 + i * 60,
      metrics: [
        { name: "cpu", value: 50.0 + 10.0 * ((i % 20).to_float() / 20.0 * 3.14159).sin() },
        { name: "memory", value: 1024.0 + 100.0 * ((i % 30).to_float() / 30.0 * 3.14159).sin() },
        { name: "disk", value: 85.0 + 5.0 * ((i % 15).to_float() / 15.0 * 3.14159).sin() },
        { name: "network", value: 200.0 + 50.0 * ((i % 25).to_float() / 25.0 * 3.14159).sin() }
      ],
      tags: [
        ("service", "api"),
        ("version", "1.2.3"),
        ("region", "us-east")
      ]
    })
    i = i + 1
  }
  
  // 多维度数据压缩算法
  let multi_dimensional_compression = fn(data: Array[Dynamic]) {
    let mut compressed = []
    
    // 压缩标签（只在第一个点存储）
    let compressed_tags = if data.length() > 0 {
      data[0].tags
    } else {
      []
    }
    
    // 压缩时间戳（增量压缩）
    let mut compressed_timestamps = []
    if data.length() > 0 {
      compressed_timestamps = compressed_timestamps.push(data[0].timestamp)
      let mut i = 1
      while i < data.length() {
        let delta = data[i].timestamp - data[i-1].timestamp
        compressed_timestamps = compressed_timestamps.push(delta)
        i = i + 1
      }
    }
    
    // 按指标分组压缩
    let metric_names = ["cpu", "memory", "disk", "network"]
    let mut compressed_metrics = []
    
    for metric_name in metric_names {
      let mut metric_values = []
      
      // 提取该指标的所有值
      let mut i = 0
      while i < data.length() {
        let mut found = false
        for metric in data[i].metrics {
          if metric.name == metric_name {
            metric_values = metric_values.push(metric.value)
            found = true
            break
          }
        }
        
        if not found {
          metric_values = metric_values.push(0.0)  // 默认值
        }
        i = i + 1
      }
      
      // 对指标值进行增量压缩
      let mut compressed_values = []
      if metric_values.length() > 0 {
        compressed_values = compressed_values.push(metric_values[0])
        let mut i = 1
        while i < metric_values.length() {
          let delta = metric_values[i] - metric_values[i-1]
          compressed_values = compressed_values.push(delta)
          i = i + 1
        }
      }
      
      compressed_metrics = compressed_metrics.push({
        name: metric_name,
        values: compressed_values
      })
    }
    
    {
      tags: compressed_tags,
      timestamps: compressed_timestamps,
      metrics: compressed_metrics,
      original_size: data.length() * 100,  // 估算原始大小
      compressed_size: compressed_tags.length() * 20 + 
                       compressed_timestamps.length() * 4 + 
                       compressed_metrics.length() * 10 +  // 元数据
                       compressed_metrics.fold(0, fn(acc, m) { acc + m.values.length() * 4 })
    }
  }
  
  // 多维度数据解压缩算法
  let multi_dimensional_decompression = fn(compressed: Dynamic) {
    let mut decompressed = []
    
    // 解压缩时间戳
    let mut timestamps = []
    if compressed.timestamps.length() > 0 {
      timestamps = timestamps.push(compressed.timestamps[0])
      let mut i = 1
      while i < compressed.timestamps.length() {
        let prev_timestamp = timestamps[i-1]
        timestamps = timestamps.push(prev_timestamp + compressed.timestamps[i])
        i = i + 1
      }
    }
    
    // 解压缩指标值
    let mut metric_values = []
    for metric in compressed.metrics {
      let mut values = []
      if metric.values.length() > 0 {
        values = values.push(metric.values[0])
        let mut i = 1
        while i < metric.values.length() {
          let prev_value = values[i-1]
          values = values.push(prev_value + metric.values[i])
          i = i + 1
        }
      }
      metric_values = metric_values.push({
        name: metric.name,
        values: values
      })
    }
    
    // 重构多维度数据
    let mut i = 0
    while i < timestamps.length() {
      let mut point_metrics = []
      for metric_data in metric_values {
        if i < metric_data.values.length() {
          point_metrics = point_metrics.push({
            name: metric_data.name,
            value: metric_data.values[i]
          })
        }
      }
      
      decompressed = decompressed.push({
        timestamp: timestamps[i],
        metrics: point_metrics,
        tags: compressed.tags
      })
      i = i + 1
    }
    
    decompressed
  }
  
  // 执行多维度压缩和解压缩
  let compression_result = multi_dimensional_compression(multi_dimensional_data)
  let decompressed_data = multi_dimensional_decompression(compression_result)
  
  // 验证压缩效果
  assert_true(compression_result.compressed_size < compression_result.original_size)
  let compression_ratio = compression_result.compressed_size.to_float() / compression_result.original_size.to_float()
  assert_true(compression_ratio < 0.7)  // 压缩率至少30%
  
  // 验证数据完整性
  assert_eq(decompressed_data.length(), multi_dimensional_data.length())
  
  // 验证第一个数据点
  assert_eq(decompressed_data[0].timestamp, multi_dimensional_data[0].timestamp)
  assert_eq(decompressed_data[0].tags.length(), multi_dimensional_data[0].tags.length())
  assert_eq(decompressed_data[0].metrics.length(), multi_dimensional_data[0].metrics.length())
  
  // 验证指标值
  let mut i = 0
  while i < decompressed_data[0].metrics.length() {
    let original_metric = multi_dimensional_data[0].metrics[i]
    let decompressed_metric = decompressed_data[0].metrics[i]
    assert_eq(original_metric.name, decompressed_metric.name)
    assert_eq(original_metric.value, decompressed_metric.value)
    i = i + 1
  }
  
  // 验证最后一个数据点
  let last_index = multi_dimensional_data.length() - 1
  assert_eq(decompressed_data[last_index].timestamp, multi_dimensional_data[last_index].timestamp)
  
  // 计算压缩误差
  let mut total_error = 0.0
  let mut metric_count = 0
  
  let mut i = 0
  while i < multi_dimensional_data.length() {
    let mut j = 0
    while j < multi_dimensional_data[i].metrics.length() {
      let original_value = multi_dimensional_data[i].metrics[j].value
      let decompressed_value = decompressed_data[i].metrics[j].value
      let error = (original_value - decompressed_value).abs()
      total_error = total_error + error
      metric_count = metric_count + 1
      j = j + 1
    }
    i = i + 1
  }
  
  let avg_error = total_error / metric_count.to_float()
  
  // 验证压缩误差很小
  assert_true(avg_error < 0.001)  // 平均误差应该非常小
}