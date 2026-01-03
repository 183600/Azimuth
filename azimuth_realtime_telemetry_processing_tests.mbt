// Azimuth 实时遥测处理测试用例
// 专注于测试实时数据流处理和低延迟遥测操作

// 测试1: 实时数据流处理
test "实时数据流处理" {
  // 定义实时数据点
  type RealtimeDataPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建数据流处理器
  let stream_processor = fn(buffer_size: Int) {
    let buffer = []
    let processed_count = 0
    
    fn(data_point: RealtimeDataPoint) {
      // 添加到缓冲区
      let updated_buffer = buffer.push(data_point)
      
      // 如果缓冲区满了，处理数据
      if updated_buffer.length() >= buffer_size {
        let batch = updated_buffer.slice(0, buffer_size)
        
        // 模拟处理：计算平均值
        let sum = batch.reduce(fn(acc, dp) { acc + dp.value }, 0.0)
        let avg = sum / (buffer_size as Float)
        
        // 更新处理计数
        processed_count = processed_count + buffer_size
        
        // 返回处理结果
        Some({
          batch_size: buffer_size,
          average: avg,
          processed_at: Time::now()
        })
      } else {
        None
      }
    }
  }
  
  // 创建处理器（缓冲区大小为5）
  let processor = stream_processor(5)
  
  // 发送数据点
  let data_points = [
    { timestamp: 1000, metric_name: "cpu.usage", value: 45.2, tags: [("host", "server1")] },
    { timestamp: 1001, metric_name: "cpu.usage", value: 47.8, tags: [("host", "server1")] },
    { timestamp: 1002, metric_name: "cpu.usage", value: 46.1, tags: [("host", "server1")] },
    { timestamp: 1003, metric_name: "cpu.usage", value: 48.9, tags: [("host", "server1")] },
    { timestamp: 1004, metric_name: "cpu.usage", value: 49.3, tags: [("host", "server1")] }
  ]
  
  // 处理数据点
  let mut results = []
  for data_point in data_points {
    match processor(data_point) {
      Some(result) => results = results.push(result)
      None => ()
    }
  }
  
  // 验证处理结果
  assert_eq(results.length(), 1)
  let result = results[0]
  assert_eq(result.batch_size, 5)
  assert_eq(result.average.round(), 47.0)  // (45.2 + 47.8 + 46.1 + 48.9 + 49.3) / 5 = 47.46
}

// 测试2: 低延迟事件处理
test "低延迟事件处理" {
  // 定义事件类型
  enum TelemetryEvent {
    MetricEvent(String, Float, Array[(String, String)])  // name, value, tags
    LogEvent(String, String, Array[(String, String)])    // level, message, attributes
    TraceEvent(String, String, Int)                      // operation, trace_id, duration
  }
  
  // 创建低延迟事件处理器
  let event_processor = fn() {
    let mut event_count = 0
    let mut total_latency = 0
    
    fn(event: TelemetryEvent, start_time: Int) {
      event_count = event_count + 1
      
      // 模拟事件处理延迟（微秒级）
      let processing_time = match event {
        TelemetryEvent::MetricEvent(name, value, tags) => 10
        TelemetryEvent::LogEvent(level, message, attributes) => 15
        TelemetryEvent::TraceEvent(operation, trace_id, duration) => 20
      }
      
      total_latency = total_latency + processing_time
      
      let end_time = start_time + processing_time
      let latency = end_time - start_time
      
      {
        event_type: match event {
          TelemetryEvent::MetricEvent(_, _, _) => "metric"
          TelemetryEvent::LogEvent(_, _, _) => "log"
          TelemetryEvent::TraceEvent(_, _, _) => "trace"
        },
        processing_latency: latency,
        total_events: event_count,
        average_latency: total_latency / event_count
      }
    }
  }
  
  let processor = event_processor()
  
  // 处理不同类型的事件
  let events = [
    TelemetryEvent::MetricEvent("memory.usage", 78.5, [("host", "web-server")]),
    TelemetryEvent::LogEvent("ERROR", "Database connection failed", [("service", "api")]),
    TelemetryEvent::TraceEvent("http.request", "trace-12345", 150),
    TelemetryEvent::MetricEvent("request.count", 1250.0, [("endpoint", "/api/users")]),
    TelemetryEvent::LogEvent("INFO", "User authenticated successfully", [("user.id", "12345")])
  ]
  
  let mut processing_results = []
  for event in events {
    let start_time = Time::now()
    let result = processor(event, start_time)
    processing_results = processing_results.push(result)
  }
  
  // 验证处理结果
  assert_eq(processing_results.length(), 5)
  
  // 验证事件类型分布
  let metric_events = processing_results.filter(fn(r) { r.event_type == "metric" })
  let log_events = processing_results.filter(fn(r) { r.event_type == "log" })
  let trace_events = processing_results.filter(fn(r) { r.event_type == "trace" })
  
  assert_eq(metric_events.length(), 2)
  assert_eq(log_events.length(), 2)
  assert_eq(trace_events.length(), 1)
  
  // 验证平均延迟
  let final_result = processing_results[processing_results.length() - 1]
  assert_eq(final_result.total_events, 5)
  assert_eq(final_result.average_latency, 14)  // (10 + 15 + 20 + 10 + 15) / 5 = 14
}

// 测试3: 实时异常检测
test "实时异常检测" {
  // 定义异常检测器
  let anomaly_detector = fn(threshold_multiplier: Float) {
    let historical_data = []
    
    fn(data_point: Float) {
      // 更新历史数据（保留最近100个点）
      let updated_history = if historical_data.length() >= 100 {
        historical_data.slice(1, 100).push(data_point)
      } else {
        historical_data.push(data_point)
      }
      
      // 计算统计信息
      if updated_history.length() >= 10 {
        let mean = updated_history.reduce(fn(acc, val) { acc + val }, 0.0) / (updated_history.length() as Float)
        
        let variance = updated_history.reduce(fn(acc, val) {
          let diff = val - mean
          acc + diff * diff
        }, 0.0) / (updated_history.length() as Float)
        
        let std_dev = variance.sqrt()
        
        // 检测异常（超过3个标准差）
        let is_anomaly = (data_point - mean).abs() > (threshold_multiplier * std_dev)
        
        {
          value: data_point,
          mean: mean,
          std_dev: std_dev,
          is_anomaly: is_anomaly,
          anomaly_score: (data_point - mean).abs() / std_dev,
          data_points: updated_history.length()
        }
      } else {
        {
          value: data_point,
          mean: 0.0,
          std_dev: 0.0,
          is_anomaly: false,
          anomaly_score: 0.0,
          data_points: updated_history.length()
        }
      }
    }
  }
  
  let detector = anomaly_detector(3.0)  // 3个标准差阈值
  
  // 正常数据序列
  let normal_data = [10.0, 12.0, 11.5, 13.0, 9.8, 11.2, 10.5, 12.3, 11.8, 10.9]
  
  // 异常数据点
  let anomaly_data = [10.2, 11.8, 50.0, 9.5, 11.1]  // 50.0是异常值
  
  // 处理正常数据
  let mut normal_results = []
  for data in normal_data {
    let result = detector(data)
    normal_results = normal_results.push(result)
  }
  
  // 处理异常数据
  let mut anomaly_results = []
  for data in anomaly_data {
    let result = detector(data)
    anomaly_results = anomaly_results.push(result)
  }
  
  // 验证正常数据检测结果
  let normal_anomalies = normal_results.filter(fn(r) { r.is_anomaly })
  assert_eq(normal_anomalies.length(), 0)  // 正常数据不应该被检测为异常
  
  // 验证异常数据检测结果
  let detected_anomalies = anomaly_results.filter(fn(r) { r.is_anomaly })
  assert_true(detected_anomalies.length() > 0)  // 应该检测到异常
  
  // 验证异常数据点50.0被正确识别
  let anomaly_point = anomaly_results[2]  // 第三个数据点是50.0
  assert_eq(anomaly_point.value, 50.0)
  assert_true(anomaly_point.is_anomaly)
  assert_true(anomaly_point.anomaly_score > 3.0)
}

// 测试4: 实时数据聚合窗口
test "实时数据聚合窗口" {
  // 定义时间窗口
  type TimeWindow = {
    start_time: Int,
    end_time: Int,
    data_points: Array[(Int, Float)]  // (timestamp, value)
  }
  
  // 创建滑动窗口聚合器
  let sliding_window_aggregator = fn(window_size_ms: Int) {
    let mut current_window: TimeWindow = {
      start_time: 0,
      end_time: 0,
      data_points: []
    }
    
    fn(data_point: (Int, Float)) -> (Option[TimeWindow], TimeWindow) {
      let (timestamp, value) = data_point
      
      // 如果是第一个数据点或超出当前窗口，创建新窗口
      if current_window.data_points.length() == 0 || 
         timestamp > current_window.end_time {
        
        let completed_window = if current_window.data_points.length() > 0 {
          Some(current_window)
        } else {
          None
        }
        
        current_window = {
          start_time: timestamp,
          end_time: timestamp + window_size_ms,
          data_points: [(timestamp, value)]
        }
        
        (completed_window, current_window)
      } else {
        // 添加到当前窗口
        let updated_data_points = current_window.data_points.push((timestamp, value))
        current_window = {
          ..current_window,
          data_points: updated_data_points
        }
        
        (None, current_window)
      }
    }
  }
  
  let aggregator = sliding_window_aggregator(1000)  // 1秒窗口
  
  // 创建测试数据（时间戳，值）
  let test_data = [
    (1000, 10.5),  // 新窗口
    (1500, 12.3),  // 同一窗口
    (2000, 11.8),  // 新窗口
    (2500, 13.2),  // 同一窗口
    (3000, 10.9),  // 新窗口
    (3500, 14.1),  // 同一窗口
    (4500, 12.7),  // 新窗口（跳过了4000）
    (5500, 11.5),  // 新窗口
  ]
  
  // 处理数据点
  let mut completed_windows = []
  for data_point in test_data {
    let (completed_window, current_window) = aggregator(data_point)
    match completed_window {
      Some(window) => completed_windows = completed_windows.push(window)
      None => ()
    }
  }
  
  // 验证完成的窗口数量
  assert_eq(completed_windows.length(), 5)
  
  // 验证第一个窗口（1000-2000）
  let first_window = completed_windows[0]
  assert_eq(first_window.start_time, 1000)
  assert_eq(first_window.end_time, 2000)
  assert_eq(first_window.data_points.length(), 2)
  assert_eq(first_window.data_points[0], (1000, 10.5))
  assert_eq(first_window.data_points[1], (1500, 12.3))
  
  // 验证第二个窗口（2000-3000）
  let second_window = completed_windows[1]
  assert_eq(second_window.start_time, 2000)
  assert_eq(second_window.end_time, 3000)
  assert_eq(second_window.data_points.length(), 2)
  assert_eq(second_window.data_points[0], (2000, 11.8))
  assert_eq(second_window.data_points[1], (2500, 13.2))
  
  // 验证窗口聚合计算
  let calculate_window_stats = fn(window: TimeWindow) {
    let values = window.data_points.map(fn(dp) { dp.1 })
    let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
    let avg = sum / (values.length() as Float)
    let min = values.reduce(fn(acc, val) { if val < acc { val } else { acc } }, values[0])
    let max = values.reduce(fn(acc, val) { if val > acc { val } else { acc } }, values[0])
    
    {
      window_start: window.start_time,
      window_end: window.end_time,
      count: values.length(),
      sum: sum,
      average: avg,
      min: min,
      max: max
    }
  }
  
  let first_stats = calculate_window_stats(first_window)
  assert_eq(first_stats.count, 2)
  assert_eq(first_stats.sum.round(), 23.0)  // 10.5 + 12.3 = 22.8
  assert_eq(first_stats.average.round(), 12.0)
  assert_eq(first_stats.min, 10.5)
  assert_eq(first_stats.max, 12.3)
}

// 测试5: 实时数据压缩优化
test "实时数据压缩优化" {
  // 定义压缩策略
  enum CompressionStrategy {
    NoCompression
    DeltaEncoding    // 增量编码
    RunLength        // 行程编码
    Adaptive         // 自适应压缩
  }
  
  // 创建实时压缩器
  let realtime_compressor = fn(strategy: CompressionStrategy) {
    let mut previous_value: Option[Float] = None
    let mut run_length_count = 0
    let mut run_length_value: Option[Float] = None
    
    fn(data_point: Float) {
      match strategy {
        CompressionStrategy::NoCompression => {
          [data_point]  // 不压缩，直接返回
        }
        CompressionStrategy::DeltaEncoding => {
          match previous_value {
            None => {
              previous_value = Some(data_point)
              [data_point]  // 第一个值直接存储
            }
            Some(prev) => {
              previous_value = Some(data_point)
              [data_point - prev]  // 存储增量
            }
          }
        }
        CompressionStrategy::RunLength => {
          match run_length_value {
            None => {
              run_length_value = Some(data_point)
              run_length_count = 1
              [data_point, 1.0]  // 值和计数
            }
            Some(run_val) => {
              if run_val == data_point {
                run_length_count = run_length_count + 1
                []  // 相同值，不输出
              } else {
                let old_value = run_length_value!
                let old_count = run_length_count as Float
                
                run_length_value = Some(data_point)
                run_length_count = 1
                
                [old_value, old_count, data_point, 1.0]  // 输出旧的和新值
              }
            }
          }
        }
        CompressionStrategy::Adaptive => {
          // 简化的自适应策略：根据数据变化选择压缩方法
          match previous_value {
            None => {
              previous_value = Some(data_point)
              [data_point, 0.0]  // 0表示无压缩
            }
            Some(prev) => {
              let diff = (data_point - prev).abs()
              previous_value = Some(data_point)
              
              if diff < 0.1 {
                [data_point - prev, 1.0]  // 1表示增量编码
              } else {
                [data_point, 0.0]  // 0表示无压缩
              }
            }
          }
        }
      }
    }
  }
  
  // 测试数据
  let test_data = [10.0, 10.1, 10.05, 15.2, 15.2, 15.2, 8.7, 8.8, 8.75]
  
  // 测试无压缩
  let no_compressor = realtime_compressor(CompressionStrategy::NoCompression)
  let no_compressed = test_data.map(fn(x) { no_compressor(x) })
  assert_eq(no_compressed.length(), 9)
  assert_eq(no_compressed[0], [10.0])
  
  // 测试增量编码
  let delta_compressor = realtime_compressor(CompressionStrategy::DeltaEncoding)
  let delta_compressed = test_data.map(fn(x) { delta_compressor(x) })
  assert_eq(delta_compressed[0], [10.0])  // 第一个值
  assert_eq(delta_compressed[1], [0.1])   // 增量
  assert_eq(delta_compressed[2], [-0.05]) // 增量
  
  // 测试行程编码
  let rle_compressor = realtime_compressor(CompressionStrategy::RunLength)
  let rle_compressed = test_data.map(fn(x) { rle_compressor(x) })
  assert_eq(rle_compressed[0], [10.0, 1.0])  // 第一个值和计数
  assert_eq(rle_compressed[3], [15.2, 1.0, 15.2, 1.0])  // 切换到新值
  
  // 测试自适应压缩
  let adaptive_compressor = realtime_compressor(CompressionStrategy::Adaptive)
  let adaptive_compressed = test_data.map(fn(x) { adaptive_compressor(x) })
  assert_eq(adaptive_compressed[0], [10.0, 0.0])  // 无压缩
  assert_eq(adaptive_compressed[1], [0.1, 1.0])   // 增量编码
  assert_eq(adaptive_compressed[3], [15.2, 0.0])  // 大变化，无压缩
  
  // 计算压缩率
  let calculate_compression_ratio = fn(original: Array[Float], compressed: Array[Array[Float]>) {
    let original_size = original.length()
    let compressed_size = compressed.reduce(fn(acc, arr) { acc + arr.length() }, 0)
    (original_size as Float) / (compressed_size as Float)
  }
  
  let delta_ratio = calculate_compression_ratio(test_data, delta_compressed)
  let adaptive_ratio = calculate_compression_ratio(test_data, adaptive_compressed)
  
  // 压缩率应该大于1（表示压缩有效）
  assert_true(delta_ratio > 1.0)
  assert_true(adaptive_ratio > 1.0)
}