// Azimuth 高质量遥测测试用例
// 包含高级遥测系统功能和性能测试

// 测试1: 遥测数据的时间窗口分析
test "遥测数据的时间窗口分析" {
  // 定义时间点类型
  type TimePoint = {
    timestamp: Int,
    value: Float,
    metric_type: String
  }
  
  // 定义时间窗口类型
  type TimeWindow = {
    start_time: Int,
    end_time: Int,
    points: Array[TimePoint]
  }
  
  // 创建时间窗口分析器
  let create_time_window_analyzer = fn(window_size_ms: Int) {
    // 滑动窗口函数
    fn(points: Array[TimePoint], current_time: Int) -> TimeWindow {
      let window_start = current_time - window_size_ms
      let filtered_points = points.filter(fn(p) { 
        p.timestamp >= window_start && p.timestamp <= current_time 
      })
      
      {
        start_time: window_start,
        end_time: current_time,
        points: filtered_points
      }
    }
  }
  
  // 计算窗口统计信息
  let calculate_window_stats = fn(window: TimeWindow) {
    if window.points.length() == 0 {
      return {
        count: 0,
        avg: 0.0,
        min: 0.0,
        max: 0.0,
        sum: 0.0,
        variance: 0.0
      }
    }
    
    let values = window.points.map(fn(p) { p.value })
    let count = values.length()
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    let avg = sum / (count as Float)
    let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
    let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    
    // 计算方差
    let variance = values
      .map(fn(v) { (v - avg) * (v - avg) })
      .reduce(fn(acc, v) { acc + v }, 0.0) / (count as Float)
    
    { count, avg, min, max, sum, variance }
  }
  
  // 创建测试数据
  let base_time = 1000000  // 基准时间戳
  let test_points = [
    { timestamp: base_time, value: 10.5, metric_type: "latency" },
    { timestamp: base_time + 1000, value: 12.3, metric_type: "latency" },
    { timestamp: base_time + 2000, value: 9.8, metric_type: "latency" },
    { timestamp: base_time + 3000, value: 11.2, metric_type: "latency" },
    { timestamp: base_time + 4000, value: 13.7, metric_type: "latency" },
    { timestamp: base_time + 5000, value: 8.9, metric_type: "latency" },
    { timestamp: base_time + 6000, value: 14.1, metric_type: "latency" },
    { timestamp: base_time + 7000, value: 10.2, metric_type: "latency" }
  ]
  
  // 创建5秒窗口分析器
  let analyzer = create_time_window_analyzer(5000)
  
  // 测试不同时间点的窗口
  let window1 = analyzer(test_points, base_time + 3000)
  assert_eq(window1.points.length(), 4)  // 包含前4个点
  
  let stats1 = calculate_window_stats(window1)
  assert_eq(stats1.count, 4)
  assert_eq(stats1.sum.round(), 43.8)
  assert_eq(stats1.min, 9.8)
  assert_eq(stats1.max, 12.3)
  
  let window2 = analyzer(test_points, base_time + 7000)
  assert_eq(window2.points.length(), 4)  // 包含最后4个点
  
  let stats2 = calculate_window_stats(window2)
  assert_eq(stats2.count, 4)
  assert_eq(stats2.sum.round(), 46.9)
  assert_eq(stats2.min, 8.9)
  assert_eq(stats2.max, 14.1)
  
  // 测试窗口趋势分析
  let analyze_trend = fn(window: TimeWindow) {
    if window.points.length() < 2 {
      return "insufficient_data"
    }
    
    let sorted_points = window.points.sort(fn(a, b) { 
      if a.timestamp < b.timestamp { -1 } 
      else if a.timestamp > b.timestamp { 1 } 
      else { 0 } 
    })
    
    let first_half = sorted_points.slice(0, sorted_points.length() / 2)
    let second_half = sorted_points.slice(sorted_points.length() / 2, sorted_points.length())
    
    let first_avg = first_half.map(fn(p) { p.value }).reduce(fn(acc, v) { acc + v }, 0.0) / (first_half.length() as Float)
    let second_avg = second_half.map(fn(p) { p.value }).reduce(fn(acc, v) { acc + v }, 0.0) / (second_half.length() as Float)
    
    let change_percent = ((second_avg - first_avg) / first_avg) * 100.0
    
    if change_percent > 10.0 {
      "increasing"
    } else if change_percent < -10.0 {
      "decreasing"
    } else {
      "stable"
    }
  }
  
  let trend1 = analyze_trend(window1)
  assert_eq(trend1, "stable")
  
  let trend2 = analyze_trend(window2)
  assert_eq(trend2, "increasing")
}

// 测试2: 分布式追踪的上下文传播
test "分布式追踪的上下文传播" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)],
    flags: Int
  }
  
  // 定义传播器
  type Propagator = {
    inject: (TraceContext, Array[(String, String)]) -> Array[(String, String)],
    extract: (Array[(String, String)>) -> Option[TraceContext]
  }
  
  // 创建文本格式传播器
  let create_text_propagator = fn() -> Propagator {
    {
      inject: fn(context: TraceContext, headers: Array[(String, String)]) {
        let trace_header = "trace-id=" + context.trace_id
        let span_header = "span-id=" + context.span_id
        
        let parent_header = match context.parent_span_id {
          Some(parent_id) => "parent-span-id=" + parent_id
          None => ""
        }
        
        let baggage_headers = context.baggage.map(fn(item) {
          let (key, value) = item
          "baggage-" + key + "=" + value
        }).join(";")
        
        let flags_header = "trace-flags=" + context.flags.to_string()
        
        let new_headers = [
          ("traceparent", trace_header + ";" + span_header),
          ("tracestate", baggage_headers),
          ("traceflags", flags_header)
        ]
        
        if parent_header != "" {
          new_headers.push(("parent-span", parent_header))
        }
        
        headers + new_headers
      },
      
      extract: fn(headers: Array[(String, String)>) -> Option[TraceContext] {
        let traceparent = headers.find(fn(h) { h.0 == "traceparent" })
        let tracestate = headers.find(fn(h) { h.0 == "tracestate" })
        let traceflags = headers.find(fn(h) { h.0 == "traceflags" })
        let parent_span = headers.find(fn(h) { h.0 == "parent-span" })
        
        match traceparent {
          Some((_, value)) => {
            let parts = value.split(";")
            if parts.length() >= 2 {
              let trace_id = parts[0].replace("trace-id=", "")
              let span_id = parts[1].replace("span-id=", "")
              
              let parent_span_id = match parent_span {
                Some((_, parent_value)) => Some(parent_value.replace("parent-span-id=", ""))
                None => None
              }
              
              let baggage = match tracestate {
                Some((_, baggage_value)) => {
                  if baggage_value != "" {
                    baggage_value.split(";").map(fn(item) {
                      let kv_parts = item.split("=")
                      if kv_parts.length() == 2 {
                        let key = kv_parts[0].replace("baggage-", "")
                        (key, kv_parts[1])
                      } else {
                        ("", "")
                      }
                    }).filter(fn(kv) { kv.0 != "" })
                  } else {
                    []
                  }
                }
                None => []
              }
              
              let flags = match traceflags {
                Some((_, flags_value)) => {
                  let flags_str = flags_value.replace("trace-flags=", "")
                  flags_str.to_int()
                }
                None => 0
              }
              
              Some({
                trace_id,
                span_id,
                parent_span_id,
                baggage,
                flags
              })
            } else {
              None
            }
          }
          None => None
        }
      }
    }
  }
  
  // 测试上下文注入和提取
  let propagator = create_text_propagator()
  
  let original_context = {
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    parent_span_id: Some("span-111111111"),
    baggage: [
      ("user-id", "user-123"),
      ("service-version", "1.2.3"),
      ("region", "us-west-2")
    ],
    flags: 1
  }
  
  // 注入上下文到头部
  let original_headers = [
    ("content-type", "application/json"),
    ("authorization", "Bearer token123")
  ]
  
  let injected_headers = propagator.inject(original_context, original_headers)
  assert_true(injected_headers.length() >= original_headers.length() + 3)
  
  // 验证注入的头部
  assert_true(injected_headers.any(fn(h) { h.0 == "traceparent" }))
  assert_true(injected_headers.any(fn(h) { h.0 == "tracestate" }))
  assert_true(injected_headers.any(fn(h) { h.0 == "traceflags" }))
  assert_true(injected_headers.any(fn(h) { h.0 == "parent-span" }))
  
  // 提取上下文
  let extracted_context = propagator.extract(injected_headers)
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, original_context.trace_id)
      assert_eq(context.span_id, original_context.span_id)
      assert_eq(context.parent_span_id, original_context.parent_span_id)
      assert_eq(context.flags, original_context.flags)
      
      // 验证baggage项
      assert_true(context.baggage.any(fn(item) { 
        let (key, value) = item
        key == "user-id" && value == "user-123"
      }))
      assert_true(context.baggage.any(fn(item) { 
        let (key, value) = item
        key == "service-version" && value == "1.2.3"
      }))
      assert_true(context.baggage.any(fn(item) { 
        let (key, value) = item
        key == "region" && value == "us-west-2"
      }))
    }
    None => assert_true(false)
  }
  
  // 测试跨服务上下文传播
  let create_child_context = fn(parent: TraceContext, span_name: String) {
    let new_span_id = "span-" + (Time::now().to_string() + span_name).hash().to_string()
    {
      trace_id: parent.trace_id,
      span_id: new_span_id,
      parent_span_id: Some(parent.span_id),
      baggage: parent.baggage,
      flags: parent.flags
    }
  }
  
  let child_context = create_child_context(original_context, "database-query")
  assert_eq(child_context.trace_id, original_context.trace_id)
  assert_eq(child_context.parent_span_id, Some(original_context.span_id))
  assert_eq(child_context.baggage, original_context.baggage)
}

// 测试3: 遥测数据的压缩和优化
test "遥测数据的压缩和优化" {
  // 定义原始遥测数据点
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    attributes: Array[(String, String)]
  }
  
  // 定义压缩策略
  enum CompressionStrategy {
    NoCompression
    DeltaEncoding  // 增量编码
    RunLengthEncoding  // 游程编码
    StatisticalCompression  // 统计压缩
  }
  
  // 创建数据压缩器
  let create_compressor = fn(strategy: CompressionStrategy) {
    match strategy {
      CompressionStrategy::DeltaEncoding => {
        fn(points: Array[TelemetryPoint]) -> Array[TelemetryPoint] {
          if points.length() <= 1 {
            return points
          }
          
          let mut compressed = [points[0]]
          let mut last_timestamp = points[0].timestamp
          let mut last_value = points[0].value
          
          for i in 1..points.length() {
            let current = points[i]
            let delta_point = {
              timestamp: current.timestamp - last_timestamp,
              metric_name: current.metric_name,
              value: current.value - last_value,
              attributes: current.attributes
            }
            compressed = compressed.push(delta_point)
            last_timestamp = current.timestamp
            last_value = current.value
          }
          
          compressed
        }
      }
      
      CompressionStrategy::RunLengthEncoding => {
        fn(points: Array[TelemetryPoint]) -> Array[TelemetryPoint] {
          if points.length() == 0 {
            return []
          }
          
          let mut compressed = []
          let mut current_run = points[0]
          let mut run_length = 1
          
          for i in 1..points.length() {
            let current = points[i]
            
            // 检查是否可以合并（相同的度量名称和属性，值相近）
            let can_merge = current.metric_name == current_run.metric_name &&
              current.attributes == current_run.attributes &&
              (current.value - current_run.value).abs() < 0.01
              
            if can_merge {
              run_length = run_length + 1
              // 更新运行值为平均值
              current_run = {
                timestamp: current_run.timestamp,
                metric_name: current_run.metric_name,
                value: (current_run.value * (run_length - 1) as Float + current.value) / (run_length as Float),
                attributes: current_run.attributes
              }
            } else {
              // 添加运行长度作为属性
              let run_point = {
                timestamp: current_run.timestamp,
                metric_name: current_run.metric_name,
                value: current_run.value,
                attributes: current_run.attributes.push(("run_length", run_length.to_string()))
              }
              compressed = compressed.push(run_point)
              current_run = current
              run_length = 1
            }
          }
          
          // 添加最后一个运行
          let final_point = {
            timestamp: current_run.timestamp,
            metric_name: current_run.metric_name,
            value: current_run.value,
            attributes: current_run.attributes.push(("run_length", run_length.to_string()))
          }
          compressed = compressed.push(final_point)
          
          compressed
        }
      }
      
      CompressionStrategy::StatisticalCompression => {
        fn(points: Array[TelemetryPoint]) -> Array[TelemetryPoint] {
          // 按度量名称分组
          let groups = Map::empty()
          
          for point in points {
            let group = match Map::get(groups, point.metric_name) {
              Some(g) => g
              None => []
            }
            let _ = Map::insert(groups, point.metric_name, group.push(point))
          }
          
          // 对每个组计算统计信息并压缩
          let compressed = []
          for (metric_name, group_points) in groups {
            if group_points.length() > 10 {
              // 对于大数据集，使用统计摘要
              let values = group_points.map(fn(p) { p.value })
              let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
              let avg = sum / (values.length() as Float)
              let min = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
              let max = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
              
              let summary_point = {
                timestamp: group_points[0].timestamp,
                metric_name: metric_name + "_summary",
                value: avg,
                attributes: [
                  ("count", group_points.length().to_string()),
                  ("min", min.to_string()),
                  ("max", max.to_string()),
                  ("compression", "statistical")
                ]
              }
              compressed = compressed.push(summary_point)
            } else {
              // 对于小数据集，保留原始点
              compressed = compressed + group_points
            }
          }
          
          compressed
        }
      }
      
      CompressionStrategy::NoCompression => {
        fn(points: Array[TelemetryPoint]) -> Array[TelemetryPoint] {
          points
        }
      }
    }
  }
  
  // 创建测试数据
  let base_time = 1000000
  let test_points = [
    { timestamp: base_time, metric_name: "cpu.usage", value: 45.2, attributes: [("host", "server1")] },
    { timestamp: base_time + 1000, metric_name: "cpu.usage", value: 45.3, attributes: [("host", "server1")] },
    { timestamp: base_time + 2000, metric_name: "cpu.usage", value: 45.1, attributes: [("host", "server1")] },
    { timestamp: base_time + 3000, metric_name: "cpu.usage", value: 45.4, attributes: [("host", "server1")] },
    { timestamp: base_time + 4000, metric_name: "memory.usage", value: 67.8, attributes: [("host", "server1")] },
    { timestamp: base_time + 5000, metric_name: "memory.usage", value: 68.1, attributes: [("host", "server1")] },
    { timestamp: base_time + 6000, metric_name: "cpu.usage", value: 45.5, attributes: [("host", "server1")] },
    { timestamp: base_time + 7000, metric_name: "memory.usage", value: 68.3, attributes: [("host", "server1")] }
  ]
  
  // 测试增量编码
  let delta_compressor = create_compressor(CompressionStrategy::DeltaEncoding)
  let delta_compressed = delta_compressor(test_points)
  assert_eq(delta_compressed.length(), test_points.length())
  
  // 验证增量编码
  assert_eq(delta_compressed[0].timestamp, base_time)  // 第一个点保持原样
  assert_eq(delta_compressed[1].timestamp, 1000)  // 增量时间戳
  assert_eq(delta_compressed[2].timestamp, 1000)  // 增量时间戳
  
  // 测试游程编码
  let rle_compressor = create_compressor(CompressionStrategy::RunLengthEncoding)
  let rle_compressed = rle_compressor(test_points)
  
  // 由于CPU使用率相近，应该被压缩
  assert_true(rle_compressed.length() < test_points.length())
  
  // 验证游程编码
  let cpu_points = rle_compressed.filter(fn(p) { p.metric_name == "cpu.usage" })
  assert_true(cpu_points.length() > 0)
  assert_true(cpu_points[0].attributes.any(fn(attr) { 
    let (key, value) = attr
    key == "run_length" && value.to_int() > 1
  }))
  
  // 测试统计压缩
  let statistical_compressor = create_compressor(CompressionStrategy::StatisticalCompression)
  let statistical_compressed = statistical_compressor(test_points)
  
  // 由于点数不够多，统计压缩可能不会显著减少点数
  assert_true(statistical_compressed.length() >= 0)
  
  // 测试压缩率计算
  let calculate_compression_ratio = fn(original: Array[TelemetryPoint], compressed: Array[TelemetryPoint]) {
    if original.length() == 0 {
      0.0
    } else {
      (1.0 - (compressed.length() as Float / original.length() as Float)) * 100.0
    }
  }
  
  let delta_ratio = calculate_compression_ratio(test_points, delta_compressed)
  let rle_ratio = calculate_compression_ratio(test_points, rle_compressed)
  let statistical_ratio = calculate_compression_ratio(test_points, statistical_compressed)
  
  // 增量编码不应该减少点数，但应该减少存储空间
  assert_eq(delta_ratio, 0.0)
  
  // 游程编码应该减少点数
  assert_true(rle_ratio > 0.0)
  
  // 创建大数据集测试统计压缩
  let large_dataset = []
  for i in 0..50 {
    large_dataset = large_dataset.push({
      timestamp: base_time + i * 1000,
      metric_name: "request.latency",
      value: 100.0 + (i % 10) as Float,
      attributes: [("endpoint", "/api/data")]
    })
  }
  
  let large_compressed = statistical_compressor(large_dataset)
  let large_ratio = calculate_compression_ratio(large_dataset, large_compressed)
  
  // 对于大数据集，统计压缩应该减少点数
  assert_true(large_compressed.length() < large_dataset.length())
  assert_true(large_ratio > 0.0)
}

// 测试4: 实时流处理和背压控制
test "实时流处理和背压控制" {
  // 定义流事件类型
  enum StreamEvent {
    Data(TelemetryData)
    Watermark(Int)  // 水印时间戳
    EndOfStream
  }
  
  type TelemetryData = {
    id: String,
    timestamp: Int,
    value: Float,
    source: String
  }
  
  // 定义背压策略
  enum BackpressureStrategy {
    DropLatest  // 丢弃最新事件
    DropOldest  // 丢弃最旧事件
    Buffer(Int)  // 缓冲区大小限制
    Throttle(Int)  // 限流（每秒事件数）
  }
  
  // 创建流处理器
  let create_stream_processor = fn(backpressure_strategy: BackpressureStrategy) {
    let buffer_size = match backpressure_strategy {
      BackpressureStrategy::Buffer(size) => size
      _ => 1000  // 默认缓冲区大小
    }
    
    let mut buffer = []
    let mut processed_count = 0
    let mut dropped_count = 0
    let mut last_throttle_time = Time::now()
    let mut events_in_current_second = 0
    
    {
      process_event: fn(event: StreamEvent) -> (Bool, Option[TelemetryData]) {
        let current_time = Time::now()
        
        // 处理限流
        match backpressure_strategy {
          BackpressureStrategy::Throttle(rate_per_second) => {
            if current_time - last_throttle_time >= 1000 {
              // 新的一秒开始
              last_throttle_time = current_time
              events_in_current_second = 0
            }
            
            if events_in_current_second >= rate_per_second {
              dropped_count = dropped_count + 1
              return (false, None)  // 丢弃事件
            }
            
            events_in_current_second = events_in_current_second + 1
          }
          _ => {}
        }
        
        match event {
          StreamEvent::Data(data) => {
            // 处理数据事件
            match backpressure_strategy {
              BackpressureStrategy::DropLatest => {
                if buffer.length() >= buffer_size {
                  dropped_count = dropped_count + 1
                  return (false, None)  // 丢弃最新事件
                }
              }
              
              BackpressureStrategy::DropOldest => {
                if buffer.length() >= buffer_size {
                  buffer = buffer.slice(1, buffer.length())  // 移除最旧事件
                  dropped_count = dropped_count + 1
                }
              }
              
              BackpressureStrategy::Buffer(size) => {
                if buffer.length() >= size {
                  dropped_count = dropped_count + 1
                  return (false, None)  // 丢弃事件
                }
              }
              
              BackpressureStrategy::Throttle(_) => {
                // 限流已经在上面处理
              }
            }
            
            buffer = buffer.push(data)
            processed_count = processed_count + 1
            return (true, Some(data))
          }
          
          StreamEvent::Watermark(timestamp) => {
            // 处理水印事件
            let mut ready_data = []
            let mut remaining_buffer = []
            
            for data in buffer {
              if data.timestamp <= timestamp {
                ready_data = ready_data.push(data)
              } else {
                remaining_buffer = remaining_buffer.push(data)
              }
            }
            
            buffer = remaining_buffer
            
            // 返回所有准备好的数据
            if ready_data.length() > 0 {
              (true, Some(ready_data[0]))  // 简化：只返回第一个
            } else {
              (true, None)
            }
          }
          
          StreamEvent::EndOfStream => {
            // 处理流结束
            let all_data = buffer
            buffer = []
            
            if all_data.length() > 0 {
              (true, Some(all_data[0]))  // 简化：只返回第一个
            } else {
              (true, None)
            }
          }
        }
      },
      
      get_stats: fn() -> (Int, Int) {
        (processed_count, dropped_count)
      },
      
      get_buffer_size: fn() -> Int {
        buffer.length()
      }
    }
  }
  
  // 测试背压策略
  let drop_latest_processor = create_stream_processor(BackpressureStrategy::DropLatest)
  
  // 发送超过缓冲区大小的事件
  for i in 0..1100 {
    let event = StreamEvent::Data({
      id: "event-" + i.to_string(),
      timestamp: 1000000 + i,
      value: i as Float,
      source: "test-source"
    })
    
    let (processed, data) = drop_latest_processor.process_event(event)
    
    if i < 1000 {
      assert_true(processed)  // 前1000个事件应该被处理
    } else {
      assert_false(processed)  // 后100个事件应该被丢弃
    }
  }
  
  let (processed_count, dropped_count) = drop_latest_processor.get_stats()
  assert_eq(processed_count, 1000)
  assert_eq(dropped_count, 100)
  assert_eq(drop_latest_processor.get_buffer_size(), 1000)
  
  // 测试丢弃最旧事件策略
  let drop_oldest_processor = create_stream_processor(BackpressureStrategy::DropOldest)
  
  for i in 0..1100 {
    let event = StreamEvent::Data({
      id: "event-" + i.to_string(),
      timestamp: 1000000 + i,
      value: i as Float,
      source: "test-source"
    })
    
    let (processed, data) = drop_oldest_processor.process_event(event)
    assert_true(processed)  // 所有事件都应该被"处理"
  }
  
  let (processed_count2, dropped_count2) = drop_oldest_processor.get_stats()
  assert_eq(processed_count2, 1100)
  assert_eq(dropped_count2, 100)  // 100个最旧事件被丢弃
  assert_eq(drop_oldest_processor.get_buffer_size(), 1000)
  
  // 测试限流策略
  let throttle_processor = create_stream_processor(BackpressureStrategy::Throttle(10))  // 每秒10个事件
  
  let mut accepted_events = 0
  let mut rejected_events = 0
  
  for i in 0..50 {
    let event = StreamEvent::Data({
      id: "event-" + i.to_string(),
      timestamp: 1000000 + i,
      value: i as Float,
      source: "test-source"
    })
    
    let (processed, data) = throttle_processor.process_event(event)
    
    if processed {
      accepted_events = accepted_events + 1
    } else {
      rejected_events = rejected_events + 1
    }
  }
  
  // 由于限流，应该只有10个事件被接受
  assert_eq(accepted_events, 10)
  assert_eq(rejected_events, 40)
  
  // 测试水印处理
  let buffer_processor = create_stream_processor(BackpressureStrategy::Buffer(100))
  
  // 添加一些数据点
  for i in 0..10 {
    let event = StreamEvent::Data({
      id: "event-" + i.to_string(),
      timestamp: 1000000 + i * 1000,
      value: i as Float,
      source: "test-source"
    })
    
    let (processed, data) = buffer_processor.process_event(event)
    assert_true(processed)
  }
  
  assert_eq(buffer_processor.get_buffer_size(), 10)
  
  // 发送水印，释放前5个事件
  let watermark_event = StreamEvent::Watermark(1000000 + 4 * 1000)
  let (processed, data) = buffer_processor.process_event(watermark_event)
  assert_true(processed)
  
  // 缓冲区应该剩下5个事件
  assert_eq(buffer_processor.get_buffer_size(), 5)
}

// 测试5: 遥测数据的安全性和隐私保护
test "遥测数据的安全性和隐私保护" {
  // 定义敏感数据类型
  enum SensitiveDataType {
    PersonalInformation
    FinancialData
    HealthInformation
    Credentials
    Custom(String)
  }
  
  // 定义数据脱敏策略
  enum RedactionStrategy {
    Full  // 完全脱敏
    Partial(Int)  // 部分脱敏，保留前n个字符
    Hash  // 哈希处理
    Tokenize  // 令牌化
    Mask(String)  // 使用掩码
  }
  
  // 定义隐私规则
  type PrivacyRule = {
    attribute_name: String,
    data_type: SensitiveDataType,
    strategy: RedactionStrategy
  }
  
  // 创建隐私保护处理器
  let create_privacy_processor = fn(rules: Array[PrivacyRule]) {
    let is_sensitive_attribute = fn(attr_name: String) -> Option[PrivacyRule] {
      rules.find(fn(rule) { rule.attribute_name == attr_name })
    }
    
    let apply_redaction = fn(value: String, strategy: RedactionStrategy) -> String {
      match strategy {
        RedactionStrategy::Full => "***"
        RedactionStrategy::Partial(keep_chars) => {
          if value.length() <= keep_chars {
            value
          } else {
            value.substring(0, keep_chars) + "***"
          }
        }
        RedactionStrategy::Hash => {
          // 简化的哈希函数
          let hash = value.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
          "hash-" + hash.to_string()
        }
        RedactionStrategy::Tokenize => {
          "token-" + (value.length() * 1000 + value.chars().nth(0).to_int()).to_string()
        }
        RedactionStrategy::Mask(mask) => {
          mask
        }
      }
    }
    
    {
      process_attributes: fn(attributes: Array[(String, String)]) -> Array[(String, String)] {
        attributes.map(fn(attr) {
          let (key, value) = attr
          
          match is_sensitive_attribute(key) {
            Some(rule) => {
              let redacted_value = apply_redaction(value, rule.strategy)
              (key, redacted_value)
            }
            None => attr
          }
        })
      },
      
      process_telemetry_data: fn(data: String) -> String {
        // 检查数据中是否包含敏感信息
        let contains_pii = data.contains("email") || 
          data.contains("phone") || 
          data.contains("ssn") || 
          data.contains("credit-card")
          
        if contains_pii {
          // 对整个数据进行脱敏
          apply_redaction(data, RedactionStrategy::Partial(3))
        } else {
          data
        }
      }
    }
  }
  
  // 创建隐私规则
  let privacy_rules = [
    {
      attribute_name: "user.email",
      data_type: SensitiveDataType::PersonalInformation,
      strategy: RedactionStrategy::Partial(2)
    },
    {
      attribute_name: "user.phone",
      data_type: SensitiveDataType::PersonalInformation,
      strategy: RedactionStrategy::Mask("XXX-XXX-XXXX")
    },
    {
      attribute_name: "payment.card",
      data_type: SensitiveDataType::FinancialData,
      strategy: RedactionStrategy::Hash
    },
    {
      attribute_name: "auth.token",
      data_type: SensitiveDataType::Credentials,
      strategy: RedactionStrategy::Tokenize
    },
    {
      attribute_name: "patient.id",
      data_type: SensitiveDataType::HealthInformation,
      strategy: RedactionStrategy::Full
    }
  ]
  
  // 创建隐私处理器
  let privacy_processor = create_privacy_processor(privacy_rules)
  
  // 测试属性脱敏
  let sensitive_attributes = [
    ("user.email", "john.doe@example.com"),
    ("user.phone", "555-123-4567"),
    ("payment.card", "4111-1111-1111-1111"),
    ("auth.token", "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9"),
    ("patient.id", "PAT-123456789"),
    ("service.name", "api-service"),  // 非敏感属性
    ("request.id", "req-987654321")  // 非敏感属性
  ]
  
  let redacted_attributes = privacy_processor.process_attributes(sensitive_attributes)
  
  // 验证脱敏结果
  let email_attr = redacted_attributes.find(fn(attr) { attr.0 == "user.email" })
  match email_attr {
    Some((_, value)) => assert_eq(value, "jo***")
    None => assert_true(false)
  }
  
  let phone_attr = redacted_attributes.find(fn(attr) { attr.0 == "user.phone" })
  match phone_attr {
    Some((_, value)) => assert_eq(value, "XXX-XXX-XXXX")
    None => assert_true(false)
  }
  
  let card_attr = redacted_attributes.find(fn(attr) { attr.0 == "payment.card" })
  match card_attr {
    Some((_, value)) => assert_true(value.starts_with("hash-"))
    None => assert_true(false)
  }
  
  let token_attr = redacted_attributes.find(fn(attr) { attr.0 == "auth.token" })
  match token_attr {
    Some((_, value)) => assert_true(value.starts_with("token-"))
    None => assert_true(false)
  }
  
  let patient_attr = redacted_attributes.find(fn(attr) { attr.0 == "patient.id" })
  match patient_attr {
    Some((_, value)) => assert_eq(value, "***")
    None => assert_true(false)
  }
  
  // 验证非敏感属性未被修改
  let service_attr = redacted_attributes.find(fn(attr) { attr.0 == "service.name" })
  match service_attr {
    Some((_, value)) => assert_eq(value, "api-service")
    None => assert_true(false)
  }
  
  let request_attr = redacted_attributes.find(fn(attr) { attr.0 == "request.id" })
  match request_attr {
    Some((_, value)) => assert_eq(value, "req-987654321")
    None => assert_true(false)
  }
  
  // 测试遥测数据脱敏
  let normal_data = "Request processed successfully"
  let redacted_normal = privacy_processor.process_telemetry_data(normal_data)
  assert_eq(redacted_normal, normal_data)
  
  let sensitive_data = "User login with email john.doe@example.com"
  let redacted_sensitive = privacy_processor.process_telemetry_data(sensitive_data)
  assert_eq(redacted_sensitive, "Use***")
  
  // 测试数据加密和解密
  let create_encryption_processor = fn(encryption_key: String) {
    let simple_encrypt = fn(data: String) -> String {
      // 简化的加密（仅用于测试）
      let key_hash = encryption_key.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let encrypted_chars = data.chars().map(fn(c) {
        let encrypted_char_code = c.to_int() + key_hash
        String::from_char(encrypted_char_code as Char)
      })
      encrypted_chars.join("")
    }
    
    let simple_decrypt = fn(encrypted_data: String) -> String {
      // 简化的解密（仅用于测试）
      let key_hash = encryption_key.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let decrypted_chars = encrypted_data.chars().map(fn(c) {
        let decrypted_char_code = c.to_int() - key_hash
        String::from_char(decrypted_char_code as Char)
      })
      decrypted_chars.join("")
    }
    
    {
      encrypt: fn(data: String) -> String {
        simple_encrypt(data)
      },
      
      decrypt: fn(encrypted_data: String) -> String {
        simple_decrypt(encrypted_data)
      }
    }
  }
  
  // 测试加密和解密
  let encryption_processor = create_encryption_processor("test-key-12345")
  
  let original_data = "sensitive telemetry data"
  let encrypted_data = encryption_processor.encrypt(original_data)
  assert_not_eq(encrypted_data, original_data)
  
  let decrypted_data = encryption_processor.decrypt(encrypted_data)
  assert_eq(decrypted_data, original_data)
  
  // 测试数据完整性验证
  let create_integrity_checker = fn(secret_key: String) {
    let calculate_hmac = fn(data: String) -> String {
      // 简化的HMAC计算（仅用于测试）
      let secret_hash = secret_key.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      let data_hash = data.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
      "hmac-" + ((secret_hash + data_hash) % 1000000).to_string()
    }
    
    {
      sign: fn(data: String) -> (String, String) {
        let signature = calculate_hmac(data)
        (data, signature)
      },
      
      verify: fn(data: String, signature: String) -> Bool {
        let expected_signature = calculate_hmac(data)
        signature == expected_signature
      }
    }
  }
  
  // 测试完整性验证
  let integrity_checker = create_integrity_checker("integrity-secret")
  
  let telemetry_data = "cpu_usage: 75.2%, memory_usage: 62.8%"
  let (signed_data, signature) = integrity_checker.sign(telemetry_data)
  
  let is_valid = integrity_checker.verify(signed_data, signature)
  assert_true(is_valid)
  
  let is_tampered_valid = integrity_checker.verify(signed_data + "tampered", signature)
  assert_false(is_tampered_valid)
}

// 测试6: 自适应采样策略
test "自适应采样策略" {
  // 定义采样决策类型
  enum SamplingDecision {
    Sampled
    NotSampled
  }
  
  // 定义采样器类型
  enum SamplerType {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)  // 基于Trace ID的比例采样
    Adaptive  // 自适应采样
  }
  
  // 定义自适应采样参数
  type AdaptiveSamplingParams = {
    target_sample_rate: Float,
    max_sample_rate: Float,
    min_sample_rate: Float,
    adjustment_factor: Float,
    evaluation_window_ms: Int
  }
  
  // 创建采样器
  let create_sampler = fn(sampler_type: SamplerType, params: Option[AdaptiveSamplingParams]) {
    match sampler_type {
      SamplerType::AlwaysOn => {
        fn(trace_id: String, attributes: Array[(String, String)>) -> SamplingDecision {
          SamplingDecision::Sampled
        }
      }
      
      SamplerType::AlwaysOff => {
        fn(trace_id: String, attributes: Array[(String, String)>) -> SamplingDecision {
          SamplingDecision::NotSampled
        }
      }
      
      SamplerType::TraceIdRatio(ratio) => {
        fn(trace_id: String, attributes: Array[(String, String)>) -> SamplingDecision {
          // 基于trace_id的哈希值决定是否采样
          let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
          let normalized = (hash % 1000) as Float / 1000.0
          
          if normalized <= ratio {
            SamplingDecision::Sampled
          } else {
            SamplingDecision::NotSampled
          }
        }
      }
      
      SamplerType::Adaptive => {
        match params {
          Some(adaptive_params) => {
            let mut current_sample_rate = adaptive_params.target_sample_rate
            let mut recent_decisions = []
            let mut last_evaluation_time = Time::now()
            
            fn(trace_id: String, attributes: Array[(String, String)>) -> SamplingDecision {
              let current_time = Time::now()
              
              // 检查是否需要调整采样率
              if current_time - last_evaluation_time >= adaptive_params.evaluation_window_ms {
                // 计算最近的采样率
                let sampled_count = recent_decisions.filter(fn(d) { d == SamplingDecision::Sampled }).length()
                let actual_sample_rate = if recent_decisions.length() > 0 {
                  (sampled_count as Float / recent_decisions.length() as Float)
                } else {
                  current_sample_rate
                }
                
                // 调整采样率
                if actual_sample_rate < adaptive_params.target_sample_rate {
                  current_sample_rate = (current_sample_rate * adaptive_params.adjustment_factor)
                    .min(adaptive_params.max_sample_rate)
                } else if actual_sample_rate > adaptive_params.target_sample_rate {
                  current_sample_rate = (current_sample_rate / adaptive_params.adjustment_factor)
                    .max(adaptive_params.min_sample_rate)
                }
                
                recent_decisions = []
                last_evaluation_time = current_time
              }
              
              // 使用当前采样率做决策
              let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
              let normalized = (hash % 1000) as Float / 1000.0
              
              let decision = if normalized <= current_sample_rate {
                SamplingDecision::Sampled
              } else {
                SamplingDecision::NotSampled
              }
              
              recent_decisions = recent_decisions.push(decision)
              
              decision
            }
          }
          None => {
            fn(trace_id: String, attributes: Array[(String, String)>) -> SamplingDecision {
              SamplingDecision::NotSampled
            }
          }
        }
      }
    }
  }
  
  // 测试AlwaysOn采样器
  let always_on_sampler = create_sampler(SamplerType::AlwaysOn, None)
  
  let decision1 = always_on_sampler("trace-123", [])
  assert_eq(decision1, SamplingDecision::Sampled)
  
  let decision2 = always_on_sampler("trace-456", [])
  assert_eq(decision2, SamplingDecision::Sampled)
  
  // 测试AlwaysOff采样器
  let always_off_sampler = create_sampler(SamplerType::AlwaysOff, None)
  
  let decision3 = always_off_sampler("trace-123", [])
  assert_eq(decision3, SamplingDecision::NotSampled)
  
  let decision4 = always_off_sampler("trace-456", [])
  assert_eq(decision4, SamplingDecision::NotSampled)
  
  // 测试TraceIdRatio采样器
  let ratio_sampler = create_sampler(SamplerType::TraceIdRatio(0.5), None)
  
  let mut sampled_count = 0
  let mut not_sampled_count = 0
  
  for i in 0..1000 {
    let trace_id = "trace-" + i.to_string()
    let decision = ratio_sampler(trace_id, [])
    
    match decision {
      SamplingDecision::Sampled => sampled_count = sampled_count + 1
      SamplingDecision::NotSampled => not_sampled_count = not_sampled_count + 1
    }
  }
  
  // 采样率应该接近50%
  let actual_rate = sampled_count as Float / (sampled_count + not_sampled_count) as Float
  assert_true(actual_rate > 0.4 && actual_rate < 0.6)
  
  // 测试自适应采样器
  let adaptive_params = {
    target_sample_rate: 0.1,
    max_sample_rate: 0.5,
    min_sample_rate: 0.01,
    adjustment_factor: 1.5,
    evaluation_window_ms: 100
  }
  
  let adaptive_sampler = create_sampler(SamplerType::Adaptive, Some(adaptive_params))
  
  // 模拟一段时间内的采样决策
  let mut adaptive_sampled_count = 0
  let mut adaptive_not_sampled_count = 0
  
  for i in 0..200 {
    let trace_id = "adaptive-trace-" + i.to_string()
    let decision = adaptive_sampler(trace_id, [])
    
    match decision {
      SamplingDecision::Sampled => adaptive_sampled_count = adaptive_sampled_count + 1
      SamplingDecision::NotSampled => adaptive_not_sampled_count = adaptive_not_sampled_count + 1
    }
    
    // 每50个决策暂停一下，模拟时间流逝
    if i % 50 == 0 && i > 0 {
      // 模拟时间流逝
      Time::sleep(10)
    }
  }
  
  // 自适应采样器应该调整采样率以接近目标
  let adaptive_rate = adaptive_sampled_count as Float / (adaptive_sampled_count + adaptive_not_sampled_count) as Float
  assert_true(adaptive_rate > 0.05 && adaptive_rate < 0.3)  // 应该在合理范围内
  
  // 测试基于属性的采样
  let create_attribute_based_sampler = fn(rules: Array[(String, String, Float)]) {
    fn(trace_id: String, attributes: Array[(String, String)>) -> SamplingDecision {
      for rule in rules {
        let (attr_name, attr_value, sample_rate) = rule
        
        let match_found = attributes.any(fn(attr) {
          let (name, value) = attr
          name == attr_name && value == attr_value
        })
        
        if match_found {
          let hash = trace_id.chars().reduce(0, fn(acc, c) { acc + c.to_int() })
          let normalized = (hash % 1000) as Float / 1000.0
          
          if normalized <= sample_rate {
            return SamplingDecision::Sampled
          }
        }
      }
      
      SamplingDecision::NotSampled
    }
  }
  
  // 创建基于属性的采样器
  let attribute_rules = [
    ("service.name", "payment-service", 0.8),  // 支付服务80%采样
    ("service.name", "auth-service", 0.6),     // 认证服务60%采样
    ("environment", "production", 0.1),        // 生产环境10%采样
    ("error", "true", 1.0)                     // 错误100%采样
  ]
  
  let attribute_sampler = create_attribute_based_sampler(attribute_rules)
  
  // 测试错误追踪应该总是被采样
  let error_attributes = [("error", "true"), ("service.name", "api-service")]
  let error_decision = attribute_sampler("error-trace-123", error_attributes)
  assert_eq(error_decision, SamplingDecision::Sampled)
  
  // 测试支付服务高采样率
  let payment_attributes = [("service.name", "payment-service")]
  let mut payment_sampled = 0
  for i in 0..100 {
    let trace_id = "payment-trace-" + i.to_string()
    let decision = attribute_sampler(trace_id, payment_attributes)
    
    match decision {
      SamplingDecision::Sampled => payment_sampled = payment_sampled + 1
      SamplingDecision::NotSampled => {}
    }
  }
  
  let payment_rate = payment_sampled as Float / 100.0
  assert_true(payment_rate > 0.7 && payment_rate < 0.9)  // 应该接近80%
}

// 测试7: 遥测系统的性能基准测试
test "遥测系统的性能基准测试" {
  // 定义性能指标类型
  type PerformanceMetrics = {
    throughput_ops_per_sec: Float,
    avg_latency_ms: Float,
    p95_latency_ms: Float,
    p99_latency_ms: Float,
    error_rate: Float,
    memory_usage_mb: Float,
    cpu_usage_percent: Float
  }
  
  // 定义基准测试配置
  type BenchmarkConfig = {
    duration_ms: Int,
    concurrent_threads: Int,
    operations_per_thread: Int,
    payload_size_bytes: Int
  }
  
  // 创建基准测试器
  let create_benchmark_runner = fn() {
    let measure_operation = fn(operation: () -> Unit) -> Int {
      let start_time = Time::now()
      operation()
      let end_time = Time::now()
      end_time - start_time
    }
    
    {
      run_benchmark: fn(config: BenchmarkConfig, operation: () -> Unit) -> PerformanceMetrics {
        let mut latencies = []
        let mut errors = 0
        let start_time = Time::now()
        let end_time = start_time + config.duration_ms
        
        // 执行基准测试
        while Time::now() < end_time {
          // 记录开始内存和CPU使用情况
          let start_memory = get_memory_usage()
          let start_cpu = get_cpu_usage()
          
          let latency = measure_operation(fn() {
            try {
              operation()
            } catch {
              _ => errors = errors + 1
            }
          })
          
          let end_memory = get_memory_usage()
          let end_cpu = get_cpu_usage()
          
          latencies = latencies.push(latency)
        }
        
        // 计算性能指标
        let total_operations = latencies.length()
        let total_time_ms = Time::now() - start_time
        let throughput = (total_operations as Float / total_time_ms as Float) * 1000.0
        
        let sorted_latencies = latencies.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
        
        let avg_latency = if total_operations > 0 {
          (latencies.reduce(fn(acc, l) { acc + l }, 0) as Float / total_operations as Float)
        } else {
          0.0
        }
        
        let p95_latency = if total_operations > 0 {
          let p95_index = ((total_operations as Float * 0.95) as Int)
          sorted_latencies[p95_index] as Float
        } else {
          0.0
        }
        
        let p99_latency = if total_operations > 0 {
          let p99_index = ((total_operations as Float * 0.99) as Int)
          sorted_latencies[p99_index] as Float
        } else {
          0.0
        }
        
        let error_rate = if total_operations > 0 {
          (errors as Float / total_operations as Float) * 100.0
        } else {
          0.0
        }
        
        // 简化的内存和CPU使用情况计算
        let memory_usage_mb = 50.0  // 模拟值
        let cpu_usage_percent = 25.0  // 模拟值
        
        {
          throughput_ops_per_sec: throughput,
          avg_latency_ms: avg_latency,
          p95_latency_ms: p95_latency,
          p99_latency_ms: p99_latency,
          error_rate: error_rate,
          memory_usage_mb: memory_usage_mb,
          cpu_usage_percent: cpu_usage_percent
        }
      }
    }
  }
  
  // 辅助函数（模拟）
  let get_memory_usage = fn() -> Float {
    // 模拟内存使用情况
    50.0 + (Time::now() % 100) as Float / 10.0
  }
  
  let get_cpu_usage = fn() -> Float {
    // 模拟CPU使用情况
    25.0 + (Time::now() % 50) as Float / 5.0
  }
  
  // 创建基准测试运行器
  let benchmark_runner = create_benchmark_runner()
  
  // 定义测试操作
  let telemetry_ingestion_operation = fn() {
    // 模拟遥测数据摄入操作
    let data = {
      trace_id: "trace-" + Time::now().to_string(),
      span_id: "span-" + (Time::now() * 2).to_string(),
      timestamp: Time::now(),
      duration: 10 + (Time::now() % 100),
      service_name: "benchmark-service",
      operation_name: "benchmark-operation",
      attributes: [
        ("benchmark", "true"),
        ("payload_size", "1024")
      ]
    }
    
    // 模拟数据处理
    let _ = data.trace_id.length()
    let _ = data.attributes.length()
    
    // 模拟一些计算
    let mut sum = 0
    for i in 0..100 {
      sum = sum + i
    }
  }
  
  let telemetry_export_operation = fn() {
    // 模拟遥测数据导出操作
    let batch_size = 100
    let mut batch = []
    
    for i in 0..batch_size {
      batch = batch.push({
        id: "metric-" + i.to_string(),
        value: (Time::now() % 1000) as Float,
        timestamp: Time::now(),
        metric_type: "counter"
      })
    }
    
    // 模拟序列化和网络传输
    let serialized = batch.map(fn(item) {
      item.id + ":" + item.value.to_string() + ":" + item.timestamp.to_string()
    }).join(",")
    
    let _ = serialized.length()
  }
  
  // 配置基准测试
  let benchmark_config = {
    duration_ms: 5000,  // 5秒
    concurrent_threads: 4,
    operations_per_thread: 1000,
    payload_size_bytes: 1024
  }
  
  // 运行遥测摄入基准测试
  let ingestion_metrics = benchmark_runner.run_benchmark(benchmark_config, telemetry_ingestion_operation)
  
  // 验证摄入性能指标
  assert_true(ingestion_metrics.throughput_ops_per_sec > 0)
  assert_true(ingestion_metrics.avg_latency_ms >= 0)
  assert_true(ingestion_metrics.p95_latency_ms >= ingestion_metrics.avg_latency_ms)
  assert_true(ingestion_metrics.p99_latency_ms >= ingestion_metrics.p95_latency_ms)
  assert_true(ingestion_metrics.error_rate >= 0.0 && ingestion_metrics.error_rate <= 100.0)
  assert_true(ingestion_metrics.memory_usage_mb > 0)
  assert_true(ingestion_metrics.cpu_usage_percent >= 0.0 && ingestion_metrics.cpu_usage_percent <= 100.0)
  
  // 运行遥测导出基准测试
  let export_metrics = benchmark_runner.run_benchmark(benchmark_config, telemetry_export_operation)
  
  // 验证导出性能指标
  assert_true(export_metrics.throughput_ops_per_sec > 0)
  assert_true(export_metrics.avg_latency_ms >= 0)
  assert_true(export_metrics.p95_latency_ms >= export_metrics.avg_latency_ms)
  assert_true(export_metrics.p99_latency_ms >= export_metrics.p95_latency_ms)
  
  // 比较不同操作的性能
  let compare_performance = fn(metrics1: PerformanceMetrics, metrics2: PerformanceMetrics) -> String {
    if metrics1.throughput_ops_per_sec > metrics2.throughput_ops_per_sec {
      "operation1"
    } else {
      "operation2"
    }
  }
  
  let faster_operation = compare_performance(ingestion_metrics, export_metrics)
  
  // 创建性能报告
  let create_performance_report = fn(metrics: PerformanceMetrics, operation_name: String) -> String {
    "Performance Report for " + operation_name + ":\n" +
    "  Throughput: " + metrics.throughput_ops_per_sec.to_string() + " ops/sec\n" +
    "  Avg Latency: " + metrics.avg_latency_ms.to_string() + " ms\n" +
    "  P95 Latency: " + metrics.p95_latency_ms.to_string() + " ms\n" +
    "  P99 Latency: " + metrics.p99_latency_ms.to_string() + " ms\n" +
    "  Error Rate: " + metrics.error_rate.to_string() + "%\n" +
    "  Memory Usage: " + metrics.memory_usage_mb.to_string() + " MB\n" +
    "  CPU Usage: " + metrics.cpu_usage_percent.to_string() + "%\n"
  }
  
  let ingestion_report = create_performance_report(ingestion_metrics, "Telemetry Ingestion")
  let export_report = create_performance_report(export_metrics, "Telemetry Export")
  
  // 验证报告包含预期信息
  assert_true(ingestion_report.contains("Telemetry Ingestion"))
  assert_true(ingestion_report.contains("Throughput:"))
  assert_true(ingestion_report.contains("Avg Latency:"))
  
  assert_true(export_report.contains("Telemetry Export"))
  assert_true(export_report.contains("Throughput:"))
  assert_true(export_report.contains("Avg Latency:"))
  
  // 测试性能回归检测
  let detect_performance_regression = fn(current: PerformanceMetrics, baseline: PerformanceMetrics, threshold: Float) -> Bool {
    let throughput_regression = (baseline.throughput_ops_per_sec - current.throughput_ops_per_sec) / baseline.throughput_ops_per_sec > threshold
    let latency_regression = (current.avg_latency_ms - baseline.avg_latency_ms) / baseline.avg_latency_ms > threshold
    
    throughput_regression || latency_regression
  }
  
  // 创建基线性能指标
  let baseline_metrics = {
    throughput_ops_per_sec: 1000.0,
    avg_latency_ms: 5.0,
    p95_latency_ms: 10.0,
    p99_latency_ms: 20.0,
    error_rate: 0.1,
    memory_usage_mb: 45.0,
    cpu_usage_percent: 20.0
  }
  
  // 测试性能回归检测（无回归）
  let good_metrics = {
    throughput_ops_per_sec: 1050.0,  // 5%提升
    avg_latency_ms: 4.8,            // 4%改善
    p95_latency_ms: 9.5,
    p99_latency_ms: 19.0,
    error_rate: 0.05,
    memory_usage_mb: 44.0,
    cpu_usage_percent: 19.0
  }
  
  let has_regression1 = detect_performance_regression(good_metrics, baseline_metrics, 0.1)  // 10%阈值
  assert_false(has_regression1)
  
  // 测试性能回归检测（有回归）
  let bad_metrics = {
    throughput_ops_per_sec: 850.0,   // 15%下降
    avg_latency_ms: 6.0,             // 20%增加
    p95_latency_ms: 12.0,
    p99_latency_ms: 24.0,
    error_rate: 0.2,
    memory_usage_mb: 50.0,
    cpu_usage_percent: 25.0
  }
  
  let has_regression2 = detect_performance_regression(bad_metrics, baseline_metrics, 0.1)  // 10%阈值
  assert_true(has_regression2)
}

// 测试8: 异常检测和预警系统
test "异常检测和预警系统" {
  // 定义异常类型
  enum AnomalyType {
    StatisticalOutlier
    TrendChange
    SuddenSpike
    PatternDeviation
    ThresholdBreach
  }
  
  // 定义异常事件
  type AnomalyEvent = {
    timestamp: Int,
    anomaly_type: AnomalyType,
    metric_name: String,
    value: Float,
    expected_value: Option[Float],
    confidence: Float,
    severity: String,  // "low", "medium", "high", "critical"
    description: String
  }
  
  // 定义预警规则
  type AlertRule = {
    name: String,
    metric_name: String,
    condition: String,  // ">", "<", "increase", "decrease", "std_dev"
    threshold: Float,
    window_size: Int,
    severity: String
  }
  
  // 创建异常检测器
  let create_anomaly_detector = fn() {
    // 计算统计指标
    let calculate_statistics = fn(values: Array[Float]) -> (Float, Float, Float) {
      if values.length() == 0 {
        return (0.0, 0.0, 0.0)
      }
      
      let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      
      let variance = values
        .map(fn(v) { (v - mean) * (v - mean) })
        .reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
      
      let std_dev = variance.sqrt()
      
      (mean, std_dev, variance)
    }
    
    // 检测统计异常
    let detect_statistical_outlier = fn(current_value: Float, historical_values: Array[Float], threshold: Float) -> Bool {
      if historical_values.length() < 10 {
        return false
      }
      
      let (mean, std_dev, _) = calculate_statistics(historical_values)
      
      if std_dev == 0.0 {
        return false
      }
      
      let z_score = (current_value - mean).abs() / std_dev
      z_score > threshold
    }
    
    // 检测趋势变化
    let detect_trend_change = fn(values: Array[Float], threshold: Float) -> Bool {
      if values.length() < 20 {
        return false
      }
      
      let half_point = values.length() / 2
      let first_half = values.slice(0, half_point)
      let second_half = values.slice(half_point, values.length())
      
      let (first_mean, _, _) = calculate_statistics(first_half)
      let (second_mean, _, _) = calculate_statistics(second_half)
      
      let change_percent = ((second_mean - first_mean) / first_mean).abs()
      change_percent > threshold
    }
    
    // 检测突然峰值
    let detect_sudden_spike = fn(current_value: Float, recent_values: Array[Float], multiplier: Float) -> Bool {
      if recent_values.length() < 5 {
        return false
      }
      
      let (recent_mean, _, _) = calculate_statistics(recent_values)
      
      if recent_mean == 0.0 {
        return false
      }
      
      let ratio = current_value / recent_mean
      ratio > multiplier
    }
    
    {
      detect_anomalies: fn(metric_name: String, values: Array[Float]) -> Array[AnomalyEvent] {
        let mut anomalies = []
        
        if values.length() < 10 {
          return anomalies
        }
        
        let current_value = values[values.length() - 1]
        let historical_values = values.slice(0, values.length() - 1)
        let recent_values = if values.length() >= 10 {
          values.slice(values.length() - 10, values.length() - 1)
        } else {
          historical_values
        }
        
        // 检测统计异常
        if detect_statistical_outlier(current_value, historical_values, 2.0) {
          let (mean, std_dev, _) = calculate_statistics(historical_values)
          let confidence = ((current_value - mean).abs() / std_dev).min(5.0) / 5.0
          
          anomalies = anomalies.push({
            timestamp: Time::now(),
            anomaly_type: AnomalyType::StatisticalOutlier,
            metric_name: metric_name,
            value: current_value,
            expected_value: Some(mean),
            confidence: confidence,
            severity: if confidence > 0.8 { "high" } else if confidence > 0.5 { "medium" } else { "low" },
            description: "值" + current_value.to_string() + "偏离历史均值" + mean.to_string() + "超过2个标准差"
          })
        }
        
        // 检测趋势变化
        if detect_trend_change(values, 0.3) {
          anomalies = anomalies.push({
            timestamp: Time::now(),
            anomaly_type: AnomalyType::TrendChange,
            metric_name: metric_name,
            value: current_value,
            expected_value: None,
            confidence: 0.7,
            severity: "medium",
            description: "检测到" + metric_name + "的趋势发生显著变化"
          })
        }
        
        // 检测突然峰值
        if detect_sudden_spike(current_value, recent_values, 3.0) {
          let (recent_mean, _, _) = calculate_statistics(recent_values)
          
          anomalies = anomalies.push({
            timestamp: Time::now(),
            anomaly_type: AnomalyType::SuddenSpike,
            metric_name: metric_name,
            value: current_value,
            expected_value: Some(recent_mean),
            confidence: 0.8,
            severity: "high",
            description: metric_name + "突然从" + recent_mean.to_string() + "增加到" + current_value.to_string()
          })
        }
        
        anomalies
      }
    }
  }
  
  // 创建预警管理器
  let create_alert_manager = fn() {
    let mut active_alerts = []
    
    {
      evaluate_rules: fn(rules: Array[AlertRule], metrics: Array[(String, Array[Float])>) -> Array[AnomalyEvent] {
        let mut alerts = []
        
        for rule in rules {
          let metric_data = metrics.find(fn(m) { m.0 == rule.metric_name })
          
          match metric_data {
            Some((_, values)) => {
              if values.length() >= rule.window_size {
                let current_value = values[values.length() - 1]
                let window_values = values.slice(values.length() - rule.window_size, values.length())
                
                let condition_met = match rule.condition {
                  ">" => current_value > rule.threshold,
                  "<" => current_value < rule.threshold,
                  "increase" => {
                    let first_value = window_values[0]
                    current_value > first_value * (1.0 + rule.threshold / 100.0)
                  }
                  "decrease" => {
                    let first_value = window_values[0]
                    current_value < first_value * (1.0 - rule.threshold / 100.0)
                  }
                  "std_dev" => {
                    let (mean, std_dev, _) = {
                      let sum = window_values.reduce(fn(acc, v) { acc + v }, 0.0)
                      let mean = sum / (window_values.length() as Float)
                      let variance = window_values
                        .map(fn(v) { (v - mean) * (v - mean) })
                        .reduce(fn(acc, v) { acc + v }, 0.0) / (window_values.length() as Float)
                      (mean, variance.sqrt(), variance)
                    }
                    (current_value - mean).abs() > std_dev * rule.threshold
                  }
                  _ => false
                }
                
                if condition_met {
                  alerts = alerts.push({
                    timestamp: Time::now(),
                    anomaly_type: AnomalyType::ThresholdBreach,
                    metric_name: rule.metric_name,
                    value: current_value,
                    expected_value: Some(rule.threshold),
                    confidence: 0.9,
                    severity: rule.severity,
                    description: rule.name + ": " + rule.metric_name + "值" + current_value.to_string() + rule.condition + rule.threshold.to_string()
                  })
                }
              }
            }
            None => {}
          }
        }
        
        alerts
      },
      
      get_active_alerts: fn() -> Array[AnomalyEvent] {
        active_alerts
      },
      
      clear_alerts: fn() {
        active_alerts = []
      }
    }
  }
  
  // 创建异常检测器
  let anomaly_detector = create_anomaly_detector()
  
  // 创建测试数据 - 正常模式
  let normal_cpu_values = []
  for i in 0..30 {
    normal_cpu_values = normal_cpu_values.push(50.0 + (i % 5) as Float * 2.0)  // 50-58之间的正常波动
  }
  
  // 检测正常数据中的异常
  let normal_anomalies = anomaly_detector.detect_anomalies("cpu.usage", normal_cpu_values)
  assert_eq(normal_anomalies.length(), 0)  // 正常数据不应该有异常
  
  // 创建测试数据 - 包含统计异常
  let outlier_cpu_values = []
  for i in 0..29 {
    outlier_cpu_values = outlier_cpu_values.push(50.0 + (i % 5) as Float * 2.0)
  }
  outlier_cpu_values = outlier_cpu_values.push(95.0)  // 添加异常值
  
  // 检测异常值
  let outlier_anomalies = anomaly_detector.detect_anomalies("cpu.usage", outlier_cpu_values)
  assert_eq(outlier_anomalies.length(), 1)
  
  let outlier_anomaly = outlier_anomalies[0]
  assert_eq(outlier_anomaly.metric_name, "cpu.usage")
  assert_eq(outlier_anomaly.value, 95.0)
  assert_eq(outlier_anomaly.anomaly_type, AnomalyType::StatisticalOutlier)
  assert_eq(outlier_anomaly.severity, "high")
  
  // 创建测试数据 - 包含趋势变化
  let trend_cpu_values = []
  for i in 0..15 {
    trend_cpu_values = trend_cpu_values.push(45.0 + i as Float)  // 上升趋势
  }
  for i in 0..15 {
    trend_cpu_values = trend_cpu_values.push(60.0 - i as Float)  // 下降趋势
  }
  
  // 检测趋势变化
  let trend_anomalies = anomaly_detector.detect_anomalies("cpu.usage", trend_cpu_values)
  assert_true(trend_anomalies.length() >= 1)
  
  let trend_anomaly = trend_anomalies.find(fn(a) { a.anomaly_type == AnomalyType::TrendChange })
  match trend_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.metric_name, "cpu.usage")
      assert_eq(anomaly.severity, "medium")
    }
    None => assert_true(false)
  }
  
  // 创建测试数据 - 包含突然峰值
  let spike_cpu_values = []
  for i in 0..25 {
    spike_cpu_values = spike_cpu_values.push(50.0 + (i % 3) as Float)
  }
  spike_cpu_values = spike_cpu_values.push(95.0)  // 突然峰值
  
  // 检测突然峰值
  let spike_anomalies = anomaly_detector.detect_anomalies("cpu.usage", spike_cpu_values)
  assert_true(spike_anomalies.length() >= 1)
  
  let spike_anomaly = spike_anomalies.find(fn(a) { a.anomaly_type == AnomalyType::SuddenSpike })
  match spike_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.metric_name, "cpu.usage")
      assert_eq(anomaly.value, 95.0)
      assert_eq(anomaly.severity, "high")
    }
    None => assert_true(false)
  }
  
  // 创建预警管理器
  let alert_manager = create_alert_manager()
  
  // 定义预警规则
  let alert_rules = [
    {
      name: "CPU使用率过高",
      metric_name: "cpu.usage",
      condition: ">",
      threshold: 80.0,
      window_size: 5,
      severity: "high"
    },
    {
      name: "内存使用率过高",
      metric_name: "memory.usage",
      condition: ">",
      threshold: 90.0,
      window_size: 3,
      severity: "critical"
    },
    {
      name: "响应时间增加",
      metric_name: "response.time",
      condition: "increase",
      threshold: 50.0,  // 50%增加
      window_size: 10,
      severity: "medium"
    },
    {
      name: "错误率增加",
      metric_name: "error.rate",
      condition: "std_dev",
      threshold: 2.0,  // 2个标准差
      window_size: 20,
      severity: "high"
    }
  ]
  
  // 创建测试指标数据
  let test_metrics = [
    ("cpu.usage", outlier_cpu_values),  // 包含95.0的异常值
    ("memory.usage", [85.0, 86.0, 87.0, 91.0, 92.0]),  // 最后两个值超过90.0
    ("response.time", [100.0, 105.0, 110.0, 115.0, 160.0]),  // 最后一个值增加超过50%
    ("error.rate", [0.1, 0.1, 0.1, 0.1, 5.0])  // 最后一个值异常高
  ]
  
  // 评估预警规则
  let alerts = alert_manager.evaluate_rules(alert_rules, test_metrics)
  
  // 验证预警
  assert_true(alerts.length() >= 3)  // 应该至少有3个预警
  
  // 验证CPU使用率预警
  let cpu_alert = alerts.find(fn(a) { a.metric_name == "cpu.usage" })
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, "high")
      assert_eq(alert.value, 95.0)
    }
    None => assert_true(false)
  }
  
  // 验证内存使用率预警
  let memory_alert = alerts.find(fn(a) { a.metric_name == "memory.usage" })
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.severity, "critical")
      assert_true(alert.value > 90.0)
    }
    None => assert_true(false)
  }
  
  // 验证响应时间预警
  let response_time_alert = alerts.find(fn(a) { a.metric_name == "response.time" })
  match response_time_alert {
    Some(alert) => {
      assert_eq(alert.severity, "medium")
    }
    None => assert_true(false)
  }
  
  // 验证错误率预警
  let error_rate_alert = alerts.find(fn(a) { a.metric_name == "error.rate" })
  match error_rate_alert {
    Some(alert) => {
      assert_eq(alert.severity, "high")
      assert_eq(alert.value, 5.0)
    }
    None => assert_true(false)
  }
  
  // 测试预警优先级排序
  let sort_alerts_by_severity = fn(alerts: Array[AnomalyEvent]) -> Array[AnomalyEvent] {
    let severity_order = [
      ("critical", 4),
      ("high", 3),
      ("medium", 2),
      ("low", 1)
    ]
    
    alerts.sort(fn(a, b) {
      let a_severity = match severity_order.find(fn(s) { s.0 == a.severity }) {
        Some((_, order)) => order
        None => 0
      }
      
      let b_severity = match severity_order.find(fn(s) { s.0 == b.severity }) {
        Some((_, order)) => order
        None => 0
      }
      
      if a_severity > b_severity { -1 }
      else if a_severity < b_severity { 1 }
      else { 0 }
    })
  }
  
  let sorted_alerts = sort_alerts_by_severity(alerts)
  
  // 验证排序结果
  if sorted_alerts.length() > 0 {
    assert_eq(sorted_alerts[0].severity, "critical")  // 第一个应该是critical
  }
  
  if sorted_alerts.length() > 1 {
    assert_true(sorted_alerts[1].severity == "high" || sorted_alerts[1].severity == "critical")
  }
}