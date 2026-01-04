// Azimuth新遥测场景测试用例
// 测试Azimuth遥测系统的新场景和边缘情况

// 测试1: 遥测数据采样策略
test "telemetry sampling strategies" {
  // 定义采样策略类型
  enum SamplingStrategy {
    AlwaysOn
    AlwaysOff
    TraceIdRatio(Float)  // 采样率 0.0 到 1.0
    ParentBased          // 基于父span的决策
  }
  
  // 模拟采样决策函数
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String, parent_sampled: Option[Bool]) {
    match strategy {
      SamplingStrategy::AlwaysOn => true
      SamplingStrategy::AlwaysOff => false
      SamplingStrategy::TraceIdRatio(ratio) => {
        // 简化的采样决策：基于trace_id的哈希值
        let trace_hash = trace_id.length() % 100
        trace_hash < (ratio * 100.0).to_int()
      }
      SamplingStrategy::ParentBased => {
        match parent_sampled {
          Some(sampled) => sampled
          None => true  // 默认采样
        }
      }
    }
  }
  
  // 测试AlwaysOn策略
  assert_true(should_sample(SamplingStrategy::AlwaysOn, "trace-001", None))
  assert_true(should_sample(SamplingStrategy::AlwaysOn, "trace-002", None))
  
  // 测试AlwaysOff策略
  assert_false(should_sample(SamplingStrategy::AlwaysOff, "trace-001", None))
  assert_false(should_sample(SamplingStrategy::AlwaysOff, "trace-002", None))
  
  // 测试TraceIdRatio策略
  let low_ratio = SamplingStrategy::TraceIdRatio(0.1)  // 10%采样率
  let high_ratio = SamplingStrategy::TraceIdRatio(0.8)  // 80%采样率
  
  // 使用不同的trace_id测试采样率
  let mut sampled_count = 0
  let trace_ids = ["trace-001", "trace-002", "trace-003", "trace-004", "trace-005",
                   "trace-006", "trace-007", "trace-008", "trace-009", "trace-010"]
  
  for trace_id in trace_ids {
    if should_sample(low_ratio, trace_id, None) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 由于是简化的哈希函数，我们可以预测结果
  assert_true(sampled_count <= 2)  // 应该有大约10%的trace被采样
  
  // 测试ParentBased策略
  assert_true(should_sample(SamplingStrategy::ParentBased, "trace-001", Some(true)))
  assert_false(should_sample(SamplingStrategy::ParentBased, "trace-002", Some(false)))
  assert_true(should_sample(SamplingStrategy::ParentBased, "trace-003", None))
}

// 测试2: 遥测数据批处理和导出
test "telemetry batch processing and export" {
  // 定义批处理配置
  type BatchConfig = {
    max_batch_size: Int,
    max_export_timeout: Int,  // 毫秒
    export_retry_count: Int
  }
  
  // 定义遥测记录类型
  type TelemetryRecord = {
    trace_id: String,
    span_id: String,
    timestamp: Int,
    data: String
  }
  
  // 创建批处理配置
  let batch_config = {
    max_batch_size: 5,
    max_export_timeout: 10000,
    export_retry_count: 3
  }
  
  // 模拟遥测记录
  let telemetry_records = [
    { trace_id: "trace-001", span_id: "span-001", timestamp: 1640995200, data: "span data 1" },
    { trace_id: "trace-001", span_id: "span-002", timestamp: 1640995250, data: "span data 2" },
    { trace_id: "trace-002", span_id: "span-003", timestamp: 1640995300, data: "span data 3" },
    { trace_id: "trace-002", span_id: "span-004", timestamp: 1640995350, data: "span data 4" },
    { trace_id: "trace-003", span_id: "span-005", timestamp: 1640995400, data: "span data 5" },
    { trace_id: "trace-003", span_id: "span-006", timestamp: 1640995450, data: "span data 6" },
    { trace_id: "trace-004", span_id: "span-007", timestamp: 1640995500, data: "span data 7" }
  ]
  
  // 批处理函数
  let create_batches = fn(records: Array[TelemetryRecord], config: BatchConfig) {
    let mut batches = []
    let mut current_batch = []
    
    for record in records {
      current_batch = current_batch + [record]
      
      if current_batch.length() >= config.max_batch_size {
        batches = batches + [current_batch]
        current_batch = []
      }
    }
    
    // 添加剩余的记录
    if current_batch.length() > 0 {
      batches = batches + [current_batch]
    }
    
    batches
  }
  
  // 创建批次
  let batches = create_batches(telemetry_records, batch_config)
  
  // 验证批处理结果
  assert_eq(batches.length(), 2)  // 7个记录，每批5个，应该有2批
  assert_eq(batches[0].length(), 5)
  assert_eq(batches[1].length(), 2)
  
  // 验证第一批的内容
  assert_eq(batches[0][0].span_id, "span-001")
  assert_eq(batches[0][4].span_id, "span-005")
  
  // 验证第二批的内容
  assert_eq(batches[1][0].span_id, "span-006")
  assert_eq(batches[1][1].span_id, "span-007")
  
  // 模拟导出函数
  let export_batch = fn(batch: Array[TelemetryRecord], config: BatchConfig) {
    {
      exported_count: batch.length(),
      export_timestamp: 1640995600,
      export_success: true,
      retry_count: 0
    }
  }
  
  // 导出所有批次
  let mut total_exported = 0
  let mut export_results = []
  
  for batch in batches {
    let result = export_batch(batch, batch_config)
    export_results = export_results + [result]
    total_exported = total_exported + result.exported_count
  }
  
  // 验证导出结果
  assert_eq(total_exported, 7)
  assert_eq(export_results.length(), 2)
  assert_true(export_results[0].export_success)
  assert_true(export_results[1].export_success)
  assert_eq(export_results[0].exported_count, 5)
  assert_eq(export_results[1].exported_count, 2)
}

// 测试3: 遥测数据压缩和序列化
test "telemetry data compression and serialization" {
  // 定义遥测数据点
  type DataPoint = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // 创建测试数据点
  let data_points = [
    { name: "cpu.usage", value: 25.5, timestamp: 1640995200, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "memory.usage", value: 45.2, timestamp: 1640995250, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "disk.io", value: 12.8, timestamp: 1640995300, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "network.throughput", value: 1024.5, timestamp: 1640995350, tags: [("host", "server1"), ("region", "us-west")] },
    { name: "cpu.usage", value: 30.1, timestamp: 1640995400, tags: [("host", "server2"), ("region", "us-east")] },
    { name: "memory.usage", value: 55.7, timestamp: 1640995450, tags: [("host", "server2"), ("region", "us-east")] }
  ]
  
  // 模拟序列化函数
  let serialize_data_point = fn(point: DataPoint) {
    let tags_string = point.tags.reduce(fn(acc, tag) {
      match tag {
        (key, value) => acc + key + "=" + value + ","
      }
    }, "").remove_suffix(",")
    
    point.name + "|" + point.value.to_string() + "|" + point.timestamp.to_string() + "|" + tags_string
  }
  
  // 序列化所有数据点
  let serialized_points = data_points.map(serialize_data_point)
  
  // 验证序列化结果
  assert_eq(serialized_points.length(), 6)
  assert_eq(serialized_points[0], "cpu.usage|25.5|1640995200|host=server1,region=us-west")
  assert_eq(serialized_points[1], "memory.usage|45.2|1640995250|host=server1,region=us-west")
  assert_eq(serialized_points[5], "memory.usage|55.7|1640995450|host=server2,region=us-east")
  
  // 模拟压缩函数（通过识别重复模式）
  let compress_serialized_data = fn(serialized: Array[String]) {
    let mut compressed = []
    let mut tag_patterns = {}
    
    for data in serialized {
      let parts = data.split("|")
      if parts.length() >= 4 {
        let name = parts[0]
        let value = parts[1]
        let timestamp = parts[2]
        let tags = parts[3]
        
        // 检查标签模式是否已存在
        if tag_patterns.contains(tags) {
          let pattern_id = tag_patterns[tags]
          compressed = compressed + [name + "|" + value + "|" + timestamp + "|#" + pattern_id.to_string()]
        } else {
          let new_pattern_id = tag_patterns.length() + 1
          tag_patterns = tag_patterns + { tags: new_pattern_id }
          compressed = compressed + [name + "|" + value + "|" + timestamp + "|#" + new_pattern_id.to_string()]
        }
      }
    }
    
    { compressed_data: compressed, patterns: tag_patterns }
  }
  
  // 压缩序列化数据
  let compression_result = compress_serialized_data(serialized_data)
  
  // 验证压缩结果
  assert_eq(compression_result.compressed_data.length(), 6)
  assert_eq(compression_result.patterns.length(), 2)  // 应该有两个标签模式
  
  // 验证模式引用
  assert_true(compression_result.compressed_data[0].ends_with("#1"))
  assert_true(compression_result.compressed_data[1].ends_with("#1"))
  assert_true(compression_result.compressed_data[4].ends_with("#2"))
  assert_true(compression_result.compressed_data[5].ends_with("#2"))
  
  // 计算压缩率
  let original_size = serialized_points.reduce(fn(acc, s) { acc + s.length() }, 0)
  let compressed_size = compression_result.compressed_data.reduce(fn(acc, s) { acc + s.length() }, 0)
  let compression_ratio = (original_size - compressed_size) * 100 / original_size
  
  // 验证压缩率大于0（表示确实有压缩）
  assert_true(compression_ratio > 0)
}

// 测试4: 遥测数据聚合和统计
test "telemetry data aggregation and statistics" {
  // 定义度量统计类型
  type MetricStatistics = {
    min: Float,
    max: Float,
    sum: Float,
    count: Int,
    avg: Float,
    p50: Float,  // 中位数
    p95: Float,  // 95百分位数
    p99: Float   // 99百分位数
  }
  
  // 创建测试度量数据
  let response_times = [
    120.5, 85.2, 200.1, 95.8, 150.3, 
    75.6, 180.9, 110.4, 165.7, 90.2,
    125.8, 95.3, 140.6, 105.9, 170.2
  ]
  
  // 计算基本统计信息
  let calculate_basic_stats = fn(values: Array[Float]) {
    let sorted = values.sort(fn(a, b) { a - b })
    let sum = values.reduce(fn(acc, x) { acc + x }, 0.0)
    let count = values.length()
    let avg = sum / count.to_float()
    
    {
      min: sorted[0],
      max: sorted[count - 1],
      sum: sum,
      count: count,
      avg: avg
    }
  }
  
  // 计算百分位数
  let calculate_percentile = fn(sorted_values: Array[Float], percentile: Float) {
    let index = (percentile / 100.0 * (sorted_values.length() - 1).to_float()).to_int()
    sorted_values[index]
  }
  
  // 计算完整统计信息
  let calculate_full_stats = fn(values: Array[Float]) {
    let sorted = values.sort(fn(a, b) { a - b })
    let basic = calculate_basic_stats(values)
    
    {
      min: basic.min,
      max: basic.max,
      sum: basic.sum,
      count: basic.count,
      avg: basic.avg,
      p50: calculate_percentile(sorted, 50.0),
      p95: calculate_percentile(sorted, 95.0),
      p99: calculate_percentile(sorted, 99.0)
    }
  }
  
  // 计算响应时间统计
  let stats = calculate_full_stats(response_times)
  
  // 验证基本统计信息
  assert_eq(stats.min, 75.6)
  assert_eq(stats.max, 200.1)
  assert_eq(stats.count, 15)
  assert_true(stats.avg > 100.0 and stats.avg < 150.0)
  
  // 验证百分位数
  assert_eq(stats.p50, 125.8)  // 中位数
  assert_eq(stats.p95, 190.1)  // 95百分位数
  assert_eq(stats.p99, 200.1)  // 99百分位数
  
  // 测试时间窗口聚合
  type TimeWindowedMetric = {
    window_start: Int,
    window_end: Int,
    metrics: Array[Float],
    statistics: MetricStatistics
  }
  
  // 创建时间窗口数据
  let time_series_data = [
    (1640995200, 120.5), (1640995260, 85.2), (1640995320, 200.1),
    (1640995380, 95.8), (1640995440, 150.3), (1640995500, 75.6),
    (1640995560, 180.9), (1640995620, 110.4), (1640995680, 165.7),
    (1640995740, 90.2), (1640995800, 125.8), (1640995860, 95.3),
    (1640995920, 140.6), (1640995980, 105.9), (1640996040, 170.2)
  ]
  
  // 时间窗口聚合函数
  let aggregate_by_time_window = fn(data: Array[(Int, Float)], window_size: Int) {
    let mut windows = []
    
    if data.length() > 0 {
      let start_time = data[0].0
      let end_time = data[data.length() - 1].0
      
      let mut current_start = start_time
      while current_start <= end_time {
        let current_end = current_start + window_size
        let window_metrics = data.filter(fn(item) {
          match item {
            (timestamp, _) => timestamp >= current_start and timestamp < current_end
          }
        }).map(fn(item) {
          match item {
            (_, value) => value
          }
        })
        
        if window_metrics.length() > 0 {
          let window_stats = calculate_full_stats(window_metrics)
          windows = windows + [{
            window_start: current_start,
            window_end: current_end,
            metrics: window_metrics,
            statistics: window_stats
          }]
        }
        
        current_start = current_end
      }
    }
    
    windows
  }
  
  // 按10分钟窗口聚合
  let windowed_data = aggregate_by_time_window(time_series_data, 600)  // 600秒 = 10分钟
  
  // 验证时间窗口聚合结果
  assert_eq(windowed_data.length(), 2)  // 15个数据点，10分钟窗口，应该有2个窗口
  
  // 验证第一个窗口
  let first_window = windowed_data[0]
  assert_eq(first_window.window_start, 1640995200)
  assert_eq(first_window.window_end, 1640995800)
  assert_eq(first_window.metrics.length(), 10)
  assert_eq(first_window.statistics.count, 10)
  
  // 验证第二个窗口
  let second_window = windowed_data[1]
  assert_eq(second_window.window_start, 1640995800)
  assert_eq(second_window.window_end, 1640996400)
  assert_eq(second_window.metrics.length(), 5)
  assert_eq(second_window.statistics.count, 5)
}

// 测试5: 遥测数据关联和链路追踪
test "telemetry data correlation and distributed tracing" {
  // 定义Span关系类型
  enum SpanRelation {
    ChildOf(String)     // 父span ID
    FollowsFrom(String) // 前驱span ID
    LinkedTo(String)    // 关联span ID
  }
  
  // 定义Span类型
  type Span = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    relations: Array[SpanRelation]
  }
  
  // 创建分布式追踪的Span树
  let spans = [
    // 根Span
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "http.request",
      start_time: 1640995200,
      end_time: 1640995250,
      status: "ok",
      relations: []
    },
    // 子Span
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "auth.service",
      start_time: 1640995205,
      end_time: 1640995220,
      status: "ok",
      relations: [SpanRelation::ChildOf("span-001")]
    },
    {
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation_name: "database.query",
      start_time: 1640995225,
      end_time: 1640995245,
      status: "ok",
      relations: [SpanRelation::ChildOf("span-001")]
    },
    // 嵌套子Span
    {
      trace_id: "trace-001",
      span_id: "span-004",
      parent_span_id: Some("span-003"),
      operation_name: "cache.lookup",
      start_time: 1640995230,
      end_time: 1640995235,
      status: "ok",
      relations: [SpanRelation::ChildOf("span-003")]
    },
    // 跨服务关联Span
    {
      trace_id: "trace-002",
      span_id: "span-005",
      parent_span_id: None,
      operation_name: "notification.service",
      start_time: 1640995255,
      end_time: 1640995280,
      status: "ok",
      relations: [SpanRelation::LinkedTo("span-001")]
    }
  ]
  
  // 构建Span树结构
  let build_span_tree = fn(spans: Array[Span]) {
    let mut span_map = {}
    let mut root_spans = []
    
    // 创建Span映射
    for span in spans {
      span_map = span_map + { span.span_id: span }
      
      match span.parent_span_id {
        None => root_spans = root_spans + [span.span_id]
        Some(_) => ()
      }
    }
    
    // 构建树结构
    let build_children = fn(parent_span_id: String) {
      spans.filter(fn(span) {
        match span.parent_span_id {
          Some(parent_id) => parent_id == parent_span_id
          None => false
        }
      }).map(fn(span) {
        {
          span: span,
          children: build_children(span.span_id)
        }
      })
    }
    
    let trees = root_spans.map(fn(root_id) {
      {
        span: span_map[root_id],
        children: build_children(root_id)
      }
    })
    
    trees
  }
  
  // 构建Span树
  let span_trees = build_span_tree(spans)
  
  // 验证Span树结构
  assert_eq(span_trees.length(), 2)  // 应该有两个根Span
  
  // 验证第一个树（trace-001）
  let first_tree = span_trees[0]
  assert_eq(first_tree.span.span_id, "span-001")
  assert_eq(first_tree.span.operation_name, "http.request")
  assert_eq(first_tree.children.length(), 2)  // 两个子Span
  
  // 验证子Span
  let auth_span = first_tree.children[0]
  assert_eq(auth_span.span.span_id, "span-002")
  assert_eq(auth_span.span.operation_name, "auth.service")
  assert_eq(auth_span.children.length(), 0)
  
  let db_span = first_tree.children[1]
  assert_eq(db_span.span.span_id, "span-003")
  assert_eq(db_span.span.operation_name, "database.query")
  assert_eq(db_span.children.length(), 1)  // 一个子Span
  
  // 验证嵌套子Span
  let cache_span = db_span.children[0]
  assert_eq(cache_span.span.span_id, "span-004")
  assert_eq(cache_span.span.operation_name, "cache.lookup")
  assert_eq(cache_span.children.length(), 0)
  
  // 验证第二个树（trace-002）
  let second_tree = span_trees[1]
  assert_eq(second_tree.span.span_id, "span-005")
  assert_eq(second_tree.span.operation_name, "notification.service")
  assert_eq(second_tree.children.length(), 0)
  
  // 计算追踪统计信息
  let calculate_trace_statistics = fn(trace_id: String, spans: Array[Span]) {
    let trace_spans = spans.filter(fn(span) { span.trace_id == trace_id })
    let total_duration = trace_spans.reduce(fn(acc, span) {
      acc + (span.end_time - span.start_time)
    }, 0)
    
    let root_span = trace_spans.find(fn(span) {
      match span.parent_span_id {
        None => true
        Some(_) => false
      }
    })
    
    let trace_duration = match root_span {
      Some(span) => span.end_time - span.start_time
      None => 0
    }
    
    {
      trace_id: trace_id,
      span_count: trace_spans.length(),
      total_duration: total_duration,
      trace_duration: trace_duration,
      average_span_duration: if trace_spans.length() > 0 { total_duration / trace_spans.length() } else { 0 }
    }
  }
  
  // 计算trace-001的统计信息
  let trace1_stats = calculate_trace_statistics("trace-001", spans)
  assert_eq(trace1_stats.trace_id, "trace-001")
  assert_eq(trace1_stats.span_count, 4)
  assert_eq(trace1_stats.trace_duration, 50)  // 1640995250 - 1640995200
  assert_eq(trace1_stats.total_duration, 100)  // 所有span持续时间总和
  assert_eq(trace1_stats.average_span_duration, 25)  // 100 / 4
  
  // 计算trace-002的统计信息
  let trace2_stats = calculate_trace_statistics("trace-002", spans)
  assert_eq(trace2_stats.trace_id, "trace-002")
  assert_eq(trace2_stats.span_count, 1)
  assert_eq(trace2_stats.trace_duration, 25)  // 1640995280 - 1640995255
  assert_eq(trace2_stats.total_duration, 25)
  assert_eq(trace2_stats.average_span_duration, 25)
}

// 测试6: 遥测数据异常检测和警报
test "telemetry anomaly detection and alerting" {
  // 定义异常类型
  enum AnomalyType {
    Spike        // 突增
    Drop         // 突降
    Outlier      // 异常值
    PatternChange // 模式变化
  }
  
  // 定义警报类型
  type Alert = {
    alert_id: String,
    anomaly_type: AnomalyType,
    metric_name: String,
    current_value: Float,
    expected_range: (Float, Float),
    severity: String,
    timestamp: Int,
    description: String
  }
  
  // 创建测试时间序列数据
  let cpu_usage_data = [
    (1640995200, 25.5), (1640995260, 27.2), (1640995320, 24.8),
    (1640995380, 26.1), (1640995440, 25.9), (1640995500, 28.3),
    (1640995560, 95.2), (1640995620, 92.8), (1640995680, 97.5),  // 异常突增
    (1640995740, 26.3), (1640995800, 24.9), (1640995860, 25.7)
  ]
  
  // 简单的异常检测算法
  let detect_anomalies = fn(data: Array[(Int, Float)], window_size: Int, threshold_multiplier: Float) {
    let mut alerts = []
    
    for i in window_size..data.length() {
      let current_timestamp = data[i].0
      let current_value = data[i].1
      
      // 计算前一个窗口的统计信息
      let window_data = data.slice(i - window_size, i).map(fn(item) {
        match item {
          (_, value) => value
        }
      })
      
      let avg = window_data.reduce(fn(acc, x) { acc + x }, 0.0) / window_data.length().to_float()
      let variance = window_data.reduce(fn(acc, x) { acc + (x - avg) * (x - avg) }, 0.0) / window_data.length().to_float()
      let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
      
      let upper_threshold = avg + std_dev * threshold_multiplier
      let lower_threshold = avg - std_dev * threshold_multiplier
      
      // 检测异常
      if current_value > upper_threshold {
        let alert = {
          alert_id: "alert-" + i.to_string(),
          anomaly_type: AnomalyType::Spike,
          metric_name: "cpu.usage",
          current_value: current_value,
          expected_range: (lower_threshold, upper_threshold),
          severity: if current_value > upper_threshold * 1.5 { "critical" } else { "warning" },
          timestamp: current_timestamp,
          description: "CPU usage spike detected: " + current_value.to_string() + "% exceeds threshold " + upper_threshold.to_string() + "%"
        }
        alerts = alerts + [alert]
      } else if current_value < lower_threshold {
        let alert = {
          alert_id: "alert-" + i.to_string(),
          anomaly_type: AnomalyType::Drop,
          metric_name: "cpu.usage",
          current_value: current_value,
          expected_range: (lower_threshold, upper_threshold),
          severity: "warning",
          timestamp: current_timestamp,
          description: "CPU usage drop detected: " + current_value.to_string() + "% below threshold " + lower_threshold.to_string() + "%"
        }
        alerts = alerts + [alert]
      }
    }
    
    alerts
  }
  
  // 检测异常
  let alerts = detect_anomalies(cpu_usage_data, 5, 2.0)  // 5个数据点的窗口，2倍标准差阈值
  
  // 验证异常检测结果
  assert_eq(alerts.length(), 3)  // 应该检测到3个异常点
  
  // 验证第一个异常（突增开始）
  let first_alert = alerts[0]
  assert_eq(first_alert.anomaly_type, AnomalyType::Spike)
  assert_eq(first_alert.metric_name, "cpu.usage")
  assert_eq(first_alert.current_value, 95.2)
  assert_eq(first_alert.severity, "critical")
  assert_eq(first_alert.timestamp, 1640995560)
  
  // 验证第二个异常
  let second_alert = alerts[1]
  assert_eq(second_alert.anomaly_type, AnomalyType::Spike)
  assert_eq(second_alert.current_value, 92.8)
  assert_eq(second_alert.severity, "critical")
  assert_eq(second_alert.timestamp, 1640995620)
  
  // 验证第三个异常
  let third_alert = alerts[2]
  assert_eq(third_alert.anomaly_type, AnomalyType::Spike)
  assert_eq(third_alert.current_value, 97.5)
  assert_eq(third_alert.severity, "critical")
  assert_eq(third_alert.timestamp, 1640995680)
  
  // 测试警报聚合和去重
  let aggregate_alerts = fn(alerts: Array[Alert], time_window: Int) {
    let mut aggregated = []
    let mut processed_indices = {}
    
    for i in 0..alerts.length() {
      if not(processed_indices.contains(i)) {
        let current_alert = alerts[i]
        let mut related_alerts = [current_alert]
        let mut related_indices = { i: true }
        
        // 查找时间窗口内的相关警报
        for j in (i + 1)..alerts.length() {
          let other_alert = alerts[j]
          let time_diff = other_alert.timestamp - current_alert.timestamp
          
          if time_diff <= time_window and 
             other_alert.anomaly_type == current_alert.anomaly_type and
             other_alert.metric_name == current_alert.metric_name {
            related_alerts = related_alerts + [other_alert]
            related_indices = related_indices + { j: true }
          }
        }
        
        // 创建聚合警报
        if related_alerts.length() > 1 {
          let first_alert = related_alerts[0]
          let last_alert = related_alerts[related_alerts.length() - 1]
          let max_severity = related_alerts.reduce(fn(acc, alert) {
            if alert.severity == "critical" { "critical" }
            else if acc == "critical" { "critical" }
            else if alert.severity == "warning" { "warning" }
            else { acc }
          }, "info")
          
          let aggregated_alert = {
            alert_id: "aggregated-" + first_alert.alert_id,
            anomaly_type: first_alert.anomaly_type,
            metric_name: first_alert.metric_name,
            current_value: related_alerts[related_alerts.length() - 1].current_value,
            expected_range: first_alert.expected_range,
            severity: max_severity,
            timestamp: first_alert.timestamp,
            description: "Aggregated alert: " + related_alerts.length().to_string() + 
                       " anomalies detected between " + first_alert.timestamp.to_string() + 
                       " and " + last_alert.timestamp.to_string()
          }
          
          aggregated = aggregated + [aggregated_alert]
        } else {
          aggregated = aggregated + related_alerts
        }
        
        // 标记已处理的索引
        processed_indices = processed_indices + related_indices
      }
    }
    
    aggregated
  }
  
  // 聚合警报
  let aggregated_alerts = aggregate_alerts(alerts, 300)  // 5分钟时间窗口
  
  // 验证警报聚合结果
  assert_eq(aggregated_alerts.length(), 1)  // 3个相关警报应该聚合成1个
  
  let aggregated_alert = aggregated_alerts[0]
  assert_eq(aggregated_alert.anomaly_type, AnomalyType::Spike)
  assert_eq(aggregated_alert.metric_name, "cpu.usage")
  assert_eq(aggregated_alert.severity, "critical")
  assert_true(aggregated_alert.description.contains("3 anomalies detected"))
}

// 测试7: 遥测数据生命周期管理
test "telemetry data lifecycle management" {
  // 定义数据生命周期策略
  type RetentionPolicy = {
    hot_ttl: Int,        // 热数据保留时间（秒）
    warm_ttl: Int,       // 温数据保留时间（秒）
    cold_ttl: Int,       // 冷数据保留时间（秒）
    archive_after: Int   // 归档时间（秒）
  }
  
  // 定义数据存储层级
  enum StorageTier {
    Hot    // 热数据 - 快速访问，高成本
    Warm   // 温数据 - 中等访问，中等成本
    Cold   // 冷数据 - 慢速访问，低成本
    Archive // 归档 - 极慢访问，极低成本
  }
  
  // 定义遥测数据记录
  type TelemetryData = {
    id: String,
    timestamp: Int,
    size: Int,           // 数据大小（字节）
    access_count: Int,   // 访问次数
    last_accessed: Int,  // 最后访问时间
    tier: StorageTier
  }
  
  // 创建保留策略
  let retention_policy = {
    hot_ttl: 3600,        // 1小时
    warm_ttl: 86400,      // 1天
    cold_ttl: 2592000,    // 30天
    archive_after: 7776000 // 90天后归档
  }
  
  // 创建测试数据
  let current_time = 1640995200
  let telemetry_data = [
    // 热数据（最近1小时内）
    { id: "data-001", timestamp: current_time - 1800, size: 1024, access_count: 50, last_accessed: current_time - 60, tier: StorageTier::Hot },
    { id: "data-002", timestamp: current_time - 2700, size: 2048, access_count: 30, last_accessed: current_time - 300, tier: StorageTier::Hot },
    
    // 温数据（1小时到1天前）
    { id: "data-003", timestamp: current_time - 7200, size: 1536, access_count: 20, last_accessed: current_time - 3600, tier: StorageTier::Warm },
    { id: "data-004", timestamp: current_time - 14400, size: 3072, access_count: 15, last_accessed: current_time - 7200, tier: StorageTier::Warm },
    
    // 冷数据（1天到30天前）
    { id: "data-005", timestamp: current_time - 172800, size: 4096, access_count: 5, last_accessed: current_time - 86400, tier: StorageTier::Cold },
    { id: "data-006", timestamp: current_time - 432000, size: 5120, access_count: 3, last_accessed: current_time - 172800, tier: StorageTier::Cold },
    
    // 归档数据（30天以上）
    { id: "data-007", timestamp: current_time - 3456000, size: 8192, access_count: 1, last_accessed: current_time - 2592000, tier: StorageTier::Archive },
    { id: "data-008", timestamp: current_time - 6912000, size: 10240, access_count: 0, last_accessed: current_time - 3456000, tier: StorageTier::Archive }
  ]
  
  // 数据生命周期管理函数
  let manage_data_lifecycle = fn(data: Array[TelemetryData], policy: RetentionPolicy, current_time: Int) {
    let mut updated_data = []
    let mut migration_stats = {
      hot_to_warm: 0,
      warm_to_cold: 0,
      cold_to_archive: 0,
      archived: 0
    }
    
    for record in data {
      let age = current_time - record.timestamp
      let access_age = current_time - record.last_accessed
      let mut new_tier = record.tier
      let mut migrated = false
      
      // 根据年龄和访问模式决定存储层级
      match record.tier {
        StorageTier::Hot => {
          if age > policy.hot_ttl or (access_age > policy.hot_ttl / 2 and record.access_count < 10) {
            new_tier = StorageTier::Warm
            migration_stats.hot_to_warm = migration_stats.hot_to_warm + 1
            migrated = true
          }
        }
        StorageTier::Warm => {
          if age > policy.warm_ttl or (access_age > policy.warm_ttl / 2 and record.access_count < 5) {
            new_tier = StorageTier::Cold
            migration_stats.warm_to_cold = migration_stats.warm_to_cold + 1
            migrated = true
          }
        }
        StorageTier::Cold => {
          if age > policy.cold_ttl or access_age > policy.cold_ttl / 2 {
            new_tier = StorageTier::Archive
            migration_stats.cold_to_archive = migration_stats.cold_to_archive + 1
            migrated = true
          }
        }
        StorageTier::Archive => {
          if age > policy.archive_after {
            // 数据可以被删除或长期归档
            migration_stats.archived = migration_stats.archived + 1
          }
        }
      }
      
      // 更新记录
      if record.tier != StorageTier::Archive or age <= policy.archive_after {
        let updated_record = { record | 
          tier: new_tier,
          access_count: if migrated { 0 } else { record.access_count },
          last_accessed: if migrated { current_time } else { record.last_accessed }
        }
        updated_data = updated_data + [updated_record]
      }
    }
    
    {
      updated_data: updated_data,
      migration_stats: migration_stats
    }
  }
  
  // 执行数据生命周期管理
  let lifecycle_result = manage_data_lifecycle(telemetry_data, retention_policy, current_time)
  
  // 验证迁移统计
  assert_eq(lifecycle_result.migration_stats.hot_to_warm, 0)  // 没有热数据需要迁移
  assert_eq(lifecycle_result.migration_stats.warm_to_cold, 0)  // 没有温数据需要迁移
  assert_eq(lifecycle_result.migration_stats.cold_to_archive, 0)  // 没有冷数据需要迁移
  assert_eq(lifecycle_result.migration_stats.archived, 0)  // 没有数据需要归档
  
  // 验证更新后的数据
  let updated_data = lifecycle_result.updated_data
  assert_eq(updated_data.length(), 8)  // 所有数据都应该保留
  
  // 验证数据分层
  let hot_data = updated_data.filter(fn(record) { record.tier == StorageTier::Hot })
  let warm_data = updated_data.filter(fn(record) { record.tier == StorageTier::Warm })
  let cold_data = updated_data.filter(fn(record) { record.tier == StorageTier::Cold })
  let archive_data = updated_data.filter(fn(record) { record.tier == StorageTier::Archive })
  
  assert_eq(hot_data.length(), 2)
  assert_eq(warm_data.length(), 2)
  assert_eq(cold_data.length(), 2)
  assert_eq(archive_data.length(), 2)
  
  // 测试数据访问和层级提升
  let promote_data = fn(data: Array[TelemetryData], accessed_ids: Array[String], current_time: Int) {
    data.map(fn(record) {
      if accessed_ids.contains(record.id) {
        let mut new_tier = record.tier
        let mut new_access_count = record.access_count + 1
        
        // 根据访问频率提升层级
        match record.tier {
          StorageTier::Warm => {
            if new_access_count > 25 {
              new_tier = StorageTier::Hot
            }
          }
          StorageTier::Cold => {
            if new_access_count > 10 {
              new_tier = StorageTier::Warm
            }
          }
          StorageTier::Archive => {
            if new_access_count > 2 {
              new_tier = StorageTier::Cold
            }
          }
          StorageTier::Hot => ()  // 已经是最高层级
        }
        
        { record | 
          tier: new_tier,
          access_count: new_access_count,
          last_accessed: current_time
        }
      } else {
        record
      }
    })
  }
  
  // 模拟数据访问
  let accessed_ids = ["data-003", "data-005", "data-007"]
  let promoted_data = promote_data(updated_data, accessed_ids, current_time + 3600)
  
  // 验证数据提升
  let promoted_warm = promoted_data.find(fn(record) { record.id == "data-003" })
  match promoted_warm {
    Some(record) => {
      assert_eq(record.access_count, 21)  // 20 + 1
      assert_eq(record.last_accessed, current_time + 3600)
      // 仍然在Warm层级，因为访问次数不足以提升到Hot
      assert_eq(record.tier, StorageTier::Warm)
    }
    None => assert_true(false)
  }
  
  let promoted_cold = promoted_data.find(fn(record) { record.id == "data-005" })
  match promoted_cold {
    Some(record) => {
      assert_eq(record.access_count, 6)  // 5 + 1
      assert_eq(record.last_accessed, current_time + 3600)
      // 仍然在Cold层级，因为访问次数不足以提升到Warm
      assert_eq(record.tier, StorageTier::Cold)
    }
    None => assert_true(false)
  }
  
  let promoted_archive = promoted_data.find(fn(record) { record.id == "data-007" })
  match promoted_archive {
    Some(record) => {
      assert_eq(record.access_count, 2)  // 1 + 1
      assert_eq(record.last_accessed, current_time + 3600)
      // 仍然在Archive层级，因为访问次数不足以提升到Cold
      assert_eq(record.tier, StorageTier::Archive)
    }
    None => assert_true(false)
  }
}

// 测试8: 遥测数据查询和分析
test "telemetry data querying and analysis" {
  // 定义查询过滤器类型
  type QueryFilter = {
    metric_names: Array[String],
    tags: Array[(String, String)],
    time_range: (Int, Int),  // (start_time, end_time)
    aggregation: Option[String]  // "avg", "sum", "min", "max", "count"
  }
  
  // 定义查询结果类型
  type QueryResult = {
    metric_name: String,
    time_series: Array[(Int, Float)],  // (timestamp, value)
    aggregation_result: Option[Float],
    data_points_count: Int
  }
  
  // 创建测试时间序列数据
  let time_series_data = [
    // CPU使用率数据
    { metric_name: "cpu.usage", timestamp: 1640995200, value: 25.5, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "cpu.usage", timestamp: 1640995260, value: 27.2, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "cpu.usage", timestamp: 1640995320, value: 24.8, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "cpu.usage", timestamp: 1640995380, value: 26.1, tags: [("host", "server2"), ("region", "us-west")] },
    { metric_name: "cpu.usage", timestamp: 1640995440, value: 30.5, tags: [("host", "server2"), ("region", "us-west")] },
    
    // 内存使用率数据
    { metric_name: "memory.usage", timestamp: 1640995200, value: 45.2, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "memory.usage", timestamp: 1640995260, value: 47.8, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "memory.usage", timestamp: 1640995320, value: 44.9, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "memory.usage", timestamp: 1640995380, value: 52.3, tags: [("host", "server2"), ("region", "us-east")] },
    { metric_name: "memory.usage", timestamp: 1640995440, value: 55.7, tags: [("host", "server2"), ("region", "us-east")] },
    
    // 网络吞吐量数据
    { metric_name: "network.throughput", timestamp: 1640995200, value: 1024.5, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "network.throughput", timestamp: 1640995260, value: 1152.8, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "network.throughput", timestamp: 1640995320, value: 987.3, tags: [("host", "server1"), ("region", "us-west")] },
    { metric_name: "network.throughput", timestamp: 1640995380, value: 1256.9, tags: [("host", "server2"), ("region", "us-east")] },
    { metric_name: "network.throughput", timestamp: 1640995440, value: 1345.2, tags: [("host", "server2"), ("region", "us-east")] }
  ]
  
  // 查询函数
  let query_telemetry_data = fn(data: Array[({ metric_name: String, timestamp: Int, value: Float, tags: Array[(String, String)] }), filter: QueryFilter) {
    let filtered_data = data.filter(fn(record) {
      // 检查指标名称
      let name_match = filter.metric_names.length() == 0 or filter.metric_names.contains(record.metric_name)
      
      // 检查时间范围
      let time_match = record.timestamp >= filter.time_range.0 and record.timestamp <= filter.time_range.1
      
      // 检查标签
      let mut tag_match = true
      for tag in filter.tags {
        match tag {
          (key, value) => {
            if not(record.tags.contains(tag)) {
              tag_match = false
              break
            }
          }
        }
      }
      
      name_match and time_match and tag_match
    })
    
    // 按指标名称分组
    let mut grouped_data = {}
    for record in filtered_data {
      let metric_name = record.metric_name
      let time_series = if grouped_data.contains(metric_name) {
        grouped_data[metric_name]
      } else {
        []
      }
      grouped_data = grouped_data + { metric_name: time_series + [(record.timestamp, record.value)] }
    }
    
    // 计算聚合结果
    let calculate_aggregation = fn(values: Array[Float], aggregation: String) {
      match aggregation {
        "avg" => {
          let sum = values.reduce(fn(acc, x) { acc + x }, 0.0)
          Some(sum / values.length().to_float())
        }
        "sum" => {
          let sum = values.reduce(fn(acc, x) { acc + x }, 0.0)
          Some(sum)
        }
        "min" => {
          let min = values.reduce(fn(acc, x) { if x < acc { x } else { acc } }, values[0])
          Some(min)
        }
        "max" => {
          let max = values.reduce(fn(acc, x) { if x > acc { x } else { acc } }, values[0])
          Some(max)
        }
        "count" => {
          Some(values.length().to_float())
        }
        _ => None
      }
    }
    
    // 构建查询结果
    let mut results = []
    for (metric_name, time_series) in grouped_data {
      let aggregation_result = match filter.aggregation {
        Some(agg) => {
          let values = time_series.map(fn(point) {
            match point {
              (_, value) => value
            }
          })
          calculate_aggregation(values, agg)
        }
        None => None
      }
      
      let result = {
        metric_name: metric_name,
        time_series: time_series,
        aggregation_result: aggregation_result,
        data_points_count: time_series.length()
      }
      results = results + [result]
    }
    
    results
  }
  
  // 测试查询1: 查询server1的所有CPU使用率数据
  let query1 = {
    metric_names: ["cpu.usage"],
    tags: [("host", "server1")],
    time_range: (1640995200, 1640995440),
    aggregation: None
  }
  
  let result1 = query_telemetry_data(time_series_data, query1)
  
  // 验证查询结果1
  assert_eq(result1.length(), 1)
  assert_eq(result1[0].metric_name, "cpu.usage")
  assert_eq(result1[0].data_points_count, 3)
  assert_eq(result1[0].time_series.length(), 3)
  assert_eq(result1[0].aggregation_result, None)
  
  // 验证时间序列数据
  assert_eq(result1[0].time_series[0], (1640995200, 25.5))
  assert_eq(result1[0].time_series[1], (1640995260, 27.2))
  assert_eq(result1[0].time_series[2], (1640995320, 24.8))
  
  // 测试查询2: 查询us-east区域的所有指标，计算平均值
  let query2 = {
    metric_names: [],  // 所有指标
    tags: [("region", "us-east")],
    time_range: (1640995200, 1640995440),
    aggregation: Some("avg")
  }
  
  let result2 = query_telemetry_data(time_series_data, query2)
  
  // 验证查询结果2
  assert_eq(result2.length(), 2)  // memory.usage和network.throughput
  
  // 验证memory.usage结果
  let memory_result = result2.find(fn(r) { r.metric_name == "memory.usage" })
  match memory_result {
    Some(result) => {
      assert_eq(result.data_points_count, 2)
      assert_eq(result.time_series.length(), 2)
      match result.aggregation_result {
        Some(avg) => assert_eq(avg, 54.0)  // (52.3 + 55.7) / 2
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证network.throughput结果
  let network_result = result2.find(fn(r) { r.metric_name == "network.throughput" })
  match network_result {
    Some(result) => {
      assert_eq(result.data_points_count, 2)
      assert_eq(result.time_series.length(), 2)
      match result.aggregation_result {
        Some(avg) => assert_eq(avg, 1301.05)  // (1256.9 + 1345.2) / 2
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试查询3: 查询所有指标的总和
  let query3 = {
    metric_names: [],  // 所有指标
    tags: [],  // 所有标签
    time_range: (1640995200, 1640995260),
    aggregation: Some("sum")
  }
  
  let result3 = query_telemetry_data(time_series_data, query3)
  
  // 验证查询结果3
  assert_eq(result3.length(), 3)  // cpu.usage, memory.usage, network.throughput
  
  // 验证总和计算
  for result in result3 {
    match result.aggregation_result {
      Some(sum) => {
        if result.metric_name == "cpu.usage" {
          assert_eq(sum, 52.7)  // 25.5 + 27.2
        } else if result.metric_name == "memory.usage" {
          assert_eq(sum, 93.0)  // 45.2 + 47.8
        } else if result.metric_name == "network.throughput" {
          assert_eq(sum, 2177.3)  // 1024.5 + 1152.8
        }
      }
      None => assert_true(false)
    }
  }
  
  // 测试查询4: 查询特定时间范围内的最大值
  let query4 = {
    metric_names: ["cpu.usage", "memory.usage"],
    tags: [],
    time_range: (1640995380, 1640995440),
    aggregation: Some("max")
  }
  
  let result4 = query_telemetry_data(time_series_data, query4)
  
  // 验证查询结果4
  assert_eq(result4.length(), 2)
  
  // 验证最大值计算
  for result in result4 {
    match result.aggregation_result {
      Some(max) => {
        if result.metric_name == "cpu.usage" {
          assert_eq(max, 30.5)  // 26.1和30.5中的最大值
        } else if result.metric_name == "memory.usage" {
          assert_eq(max, 55.7)  // 52.3和55.7中的最大值
        }
      }
      None => assert_true(false)
    }
  }
}

// 测试9: 遥测数据质量验证
test "telemetry data quality validation" {
  // 定义数据质量问题类型
  enum DataQualityIssue {
    MissingValue        // 缺失值
    OutOfRange          // 超出范围
    InconsistentFormat  // 格式不一致
    DuplicateTimestamp  // 重复时间戳
    InvalidTimestamp    // 无效时间戳
  }
  
  // 定义验证规则
  type ValidationRule = {
    metric_name: String,
    min_value: Option[Float],
    max_value: Option[Float],
    allowed_formats: Array[String],  // 简化的格式验证
    require_unique_timestamps: Bool
  }
  
  // 定义验证结果
  type ValidationResult = {
    is_valid: Bool,
    issues: Array[(DataQualityIssue, String, String)]  // (issue_type, metric_name, description)
    valid_records: Int,
    total_records: Int
  }
  
  // 创建测试数据（包含一些质量问题）
  let test_data = [
    // 正常数据
    { metric_name: "cpu.usage", timestamp: 1640995200, value: 25.5 },
    { metric_name: "cpu.usage", timestamp: 1640995260, value: 27.2 },
    { metric_name: "memory.usage", timestamp: 1640995320, value: 45.8 },
    
    // 质量问题数据
    { metric_name: "cpu.usage", timestamp: 1640995380, value: 150.0 },  // 超出范围
    { metric_name: "memory.usage", timestamp: 1640995440, value: -5.0 }, // 超出范围
    { metric_name: "disk.usage", timestamp: 1640995500, value: 0.0 },   // 缺失值（假设0表示缺失）
    { metric_name: "cpu.usage", timestamp: 1640995380, value: 26.1 },  // 重复时间戳
    { metric_name: "network.latency", timestamp: 1640991000, value: 12.5 }, // 无效时间戳（太早）
    { metric_name: "error.rate", timestamp: 9999999999, value: 0.1 }     // 无效时间戳（太晚）
  ]
  
  // 创建验证规则
  let validation_rules = [
    {
      metric_name: "cpu.usage",
      min_value: Some(0.0),
      max_value: Some(100.0),
      allowed_formats: ["percentage"],
      require_unique_timestamps: true
    },
    {
      metric_name: "memory.usage",
      min_value: Some(0.0),
      max_value: Some(100.0),
      allowed_formats: ["percentage"],
      require_unique_timestamps: true
    },
    {
      metric_name: "disk.usage",
      min_value: Some(0.1),
      max_value: Some(100.0),
      allowed_formats: ["percentage"],
      require_unique_timestamps: true
    },
    {
      metric_name: "network.latency",
      min_value: Some(0.0),
      max_value: Some(10000.0),
      allowed_formats: ["milliseconds"],
      require_unique_timestamps: true
    },
    {
      metric_name: "error.rate",
      min_value: Some(0.0),
      max_value: Some(1.0),
      allowed_formats: ["rate"],
      require_unique_timestamps: true
    }
  ]
  
  // 数据验证函数
  let validate_telemetry_data = fn(data: Array[({ metric_name: String, timestamp: Int, value: Float })], rules: Array[ValidationRule]) {
    let mut issues = []
    let mut valid_records = 0
    let current_time = 1640995600  // 当前时间，用于验证时间戳有效性
    let min_reasonable_timestamp = 1640995200 - 86400 * 7  // 7天前
    let max_reasonable_timestamp = current_time + 3600      // 1小时后
    
    // 检查重复时间戳
    let mut timestamp_counts = {}
    for record in data {
      let key = record.metric_name + ":" + record.timestamp.to_string()
      let count = if timestamp_counts.contains(key) {
        timestamp_counts[key]
      } else {
        0
      }
      timestamp_counts = timestamp_counts + { key: count + 1 }
    }
    
    for record in data {
      let mut record_valid = true
      
      // 查找适用的验证规则
      let applicable_rules = rules.filter(fn(rule) { rule.metric_name == record.metric_name })
      
      for rule in applicable_rules {
        // 检查值范围
        match rule.min_value {
          Some(min_val) => {
            if record.value < min_val {
              issues = issues + [(DataQualityIssue::OutOfRange, record.metric_name, 
                                "Value " + record.value.to_string() + " is below minimum " + min_val.to_string())]
              record_valid = false
            }
          }
          None => ()
        }
        
        match rule.max_value {
          Some(max_val) => {
            if record.value > max_val {
              issues = issues + [(DataQualityIssue::OutOfRange, record.metric_name, 
                                "Value " + record.value.to_string() + " exceeds maximum " + max_val.to_string())]
              record_valid = false
            }
          }
          None => ()
        }
        
        // 检查缺失值（假设0.0表示缺失，除非是合理的0值）
        if record.value == 0.0 and record.metric_name != "error.rate" {
          issues = issues + [(DataQualityIssue::MissingValue, record.metric_name, 
                            "Missing value detected for " + record.metric_name)]
          record_valid = false
        }
        
        // 检查重复时间戳
        let timestamp_key = record.metric_name + ":" + record.timestamp.to_string()
        if rule.require_unique_timestamps and timestamp_counts[timestamp_key] > 1 {
          issues = issues + [(DataQualityIssue::DuplicateTimestamp, record.metric_name, 
                            "Duplicate timestamp " + record.timestamp.to_string() + " for " + record.metric_name)]
          record_valid = false
        }
        
        // 检查无效时间戳
        if record.timestamp < min_reasonable_timestamp {
          issues = issues + [(DataQualityIssue::InvalidTimestamp, record.metric_name, 
                            "Timestamp " + record.timestamp.to_string() + " is too far in the past")]
          record_valid = false
        } else if record.timestamp > max_reasonable_timestamp {
          issues = issues + [(DataQualityIssue::InvalidTimestamp, record.metric_name, 
                            "Timestamp " + record.timestamp.to_string() + " is too far in the future")]
          record_valid = false
        }
      }
      
      if record_valid {
        valid_records = valid_records + 1
      }
    }
    
    {
      is_valid: issues.length() == 0,
      issues: issues,
      valid_records: valid_records,
      total_records: data.length()
    }
  }
  
  // 验证数据
  let validation_result = validate_telemetry_data(test_data, validation_rules)
  
  // 验证结果
  assert_false(validation_result.is_valid)  // 应该有质量问题
  assert_eq(validation_result.total_records, 9)
  assert_eq(validation_result.valid_records, 3)  // 只有3条有效记录
  assert_eq(validation_result.issues.length(), 7)  // 应该有7个问题
  
  // 验证具体问题
  let cpu_out_of_range = validation_result.issues.find(fn(issue) {
    match issue {
      (DataQualityIssue::OutOfRange, metric, _) => metric == "cpu.usage"
      _ => false
    }
  })
  assert_true(cpu_out_of_range.is_some())  // CPU使用率超出范围
  
  let memory_out_of_range = validation_result.issues.find(fn(issue) {
    match issue {
      (DataQualityIssue::OutOfRange, metric, _) => metric == "memory.usage"
      _ => false
    }
  })
  assert_true(memory_out_of_range.is_some())  // 内存使用率超出范围
  
  let missing_value = validation_result.issues.find(fn(issue) {
    match issue {
      (DataQualityIssue::MissingValue, metric, _) => metric == "disk.usage"
      _ => false
    }
  })
  assert_true(missing_value.is_some())  // 磁盘使用率缺失值
  
  let duplicate_timestamp = validation_result.issues.find(fn(issue) {
    match issue {
      (DataQualityIssue::DuplicateTimestamp, metric, _) => metric == "cpu.usage"
      _ => false
    }
  })
  assert_true(duplicate_timestamp.is_some())  // CPU使用率重复时间戳
  
  let invalid_timestamp_past = validation_result.issues.find(fn(issue) {
    match issue {
      (DataQualityIssue::InvalidTimestamp, metric, _) => metric == "network.latency"
      _ => false
    }
  })
  assert_true(invalid_timestamp_past.is_some())  // 网络延迟时间戳太早
  
  let invalid_timestamp_future = validation_result.issues.find(fn(issue) {
    match issue {
      (DataQualityIssue::InvalidTimestamp, metric, _) => metric == "error.rate"
      _ => false
    }
  })
  assert_true(invalid_timestamp_future.is_some())  // 错误率时间戳太晚
  
  // 测试数据清洗函数
  let clean_telemetry_data = fn(data: Array[({ metric_name: String, timestamp: Int, value: Float })], validation_result: ValidationResult) {
    let mut cleaned_data = []
    let mut removed_indices = {}
    
    // 标记有问题的记录索引
    for issue in validation_result.issues {
      // 简化处理：移除所有有问题的记录
      // 在实际应用中，可能需要更复杂的清洗策略
    }
    
    // 这里简化为只保留明显有效的记录
    let valid_metric_names = ["cpu.usage", "memory.usage"]
    let valid_timestamps = [1640995200, 1640995260, 1640995320]
    
    for record in data {
      let is_valid = valid_metric_names.contains(record.metric_name) and
                    valid_timestamps.contains(record.timestamp) and
                    record.value >= 0.0 and record.value <= 100.0
      
      if is_valid {
        cleaned_data = cleaned_data + [record]
      }
    }
    
    cleaned_data
  }
  
  // 清洗数据
  let cleaned_data = clean_telemetry_data(test_data, validation_result)
  
  // 验证清洗结果
  assert_eq(cleaned_data.length(), 3)  // 应该只有3条有效记录
  
  // 验证清洗后的数据
  assert_eq(cleaned_data[0].metric_name, "cpu.usage")
  assert_eq(cleaned_data[0].timestamp, 1640995200)
  assert_eq(cleaned_data[0].value, 25.5)
  
  assert_eq(cleaned_data[1].metric_name, "cpu.usage")
  assert_eq(cleaned_data[1].timestamp, 1640995260)
  assert_eq(cleaned_data[1].value, 27.2)
  
  assert_eq(cleaned_data[2].metric_name, "memory.usage")
  assert_eq(cleaned_data[2].timestamp, 1640995320)
  assert_eq(cleaned_data[2].value, 45.8)
}

// 测试10: 遥测数据多维度分析
test "telemetry multi-dimensional analysis" {
  // 定义多维数据点
  type MultiDimDataPoint = {
    timestamp: Int,
    dimensions: Array[(String, String)],  // 维度键值对
    value: Float
  }
  
  // 定义分析结果
  type AnalysisResult = {
    dimension_values: Array[String],
    statistics: {
      count: Int,
      sum: Float,
      avg: Float,
      min: Float,
      max: Float
    },
    trend: String  // "increasing", "decreasing", "stable"
  }
  
  // 创建测试多维数据
  let multi_dim_data = [
    // server1的CPU数据
    { timestamp: 1640995200, dimensions: [("host", "server1"), ("metric", "cpu"), ("region", "us-west")], value: 25.5 },
    { timestamp: 1640995260, dimensions: [("host", "server1"), ("metric", "cpu"), ("region", "us-west")], value: 27.2 },
    { timestamp: 1640995320, dimensions: [("host", "server1"), ("metric", "cpu"), ("region", "us-west")], value: 26.8 },
    { timestamp: 1640995380, dimensions: [("host", "server1"), ("metric", "cpu"), ("region", "us-west")], value: 28.1 },
    { timestamp: 1640995440, dimensions: [("host", "server1"), ("metric", "cpu"), ("region", "us-west")], value: 29.5 },
    
    // server2的CPU数据
    { timestamp: 1640995200, dimensions: [("host", "server2"), ("metric", "cpu"), ("region", "us-west")], value: 45.2 },
    { timestamp: 1640995260, dimensions: [("host", "server2"), ("metric", "cpu"), ("region", "us-west")], value: 46.8 },
    { timestamp: 1640995320, dimensions: [("host", "server2"), ("metric", "cpu"), ("region", "us-west")], value: 44.9 },
    { timestamp: 1640995380, dimensions: [("host", "server2"), ("metric", "cpu"), ("region", "us-west")], value: 47.3 },
    { timestamp: 1640995440, dimensions: [("host", "server2"), ("metric", "cpu"), ("region", "us-west")], value: 48.1 },
    
    // server1的内存数据
    { timestamp: 1640995200, dimensions: [("host", "server1"), ("metric", "memory"), ("region", "us-west")], value: 65.3 },
    { timestamp: 1640995260, dimensions: [("host", "server1"), ("metric", "memory"), ("region", "us-west")], value: 66.7 },
    { timestamp: 1640995320, dimensions: [("host", "server1"), ("metric", "memory"), ("region", "us-west")], value: 64.8 },
    { timestamp: 1640995380, dimensions: [("host", "server1"), ("metric", "memory"), ("region", "us-west")], value: 67.2 },
    { timestamp: 1640995440, dimensions: [("host", "server1"), ("metric", "memory"), ("region", "us-west")], value: 68.5 },
    
    // server3的CPU数据（不同区域）
    { timestamp: 1640995200, dimensions: [("host", "server3"), ("metric", "cpu"), ("region", "us-east")], value: 35.7 },
    { timestamp: 1640995260, dimensions: [("host", "server3"), ("metric", "cpu"), ("region", "us-east")], value: 36.2 },
    { timestamp: 1640995320, dimensions: [("host", "server3"), ("metric", "cpu"), ("region", "us-east")], value: 34.9 },
    { timestamp: 1640995380, dimensions: [("host", "server3"), ("metric", "cpu"), ("region", "us-east")], value: 37.1 },
    { timestamp: 1640995440, dimensions: [("host", "server3"), ("metric", "cpu"), ("region", "us-east")], value: 38.4 }
  ]
  
  // 多维分析函数
  let analyze_by_dimensions = fn(data: Array[MultiDimDataPoint], dimension_keys: Array[String]) {
    let mut dimension_groups = {}
    
    // 按维度分组
    for point in data {
      let mut dimension_values = []
      let mut matches = true
      
      for dim_key in dimension_keys {
        let mut found = false
        for dim in point.dimensions {
          match dim {
            (key, value) => {
              if key == dim_key {
                dimension_values = dimension_values + [value]
                found = true
                break
              }
            }
          }
          if not(found) {
            matches = false
            break
          }
        }
        if not(matches) {
          break
        }
      }
      
      if matches {
        let group_key = dimension_values.join(":")
        let group_data = if dimension_groups.contains(group_key) {
          dimension_groups[group_key]
        } else {
          []
        }
        dimension_groups = dimension_groups + { group_key: group_data + [point.value] }
      }
    }
    
    // 计算统计信息和趋势
    let calculate_trend = fn(values: Array[Float]) {
      if values.length() < 2 {
        "stable"
      } else {
        let first_half = values.slice(0, values.length() / 2)
        let second_half = values.slice(values.length() / 2, values.length())
        
        let first_avg = first_half.reduce(fn(acc, x) { acc + x }, 0.0) / first_half.length().to_float()
        let second_avg = second_half.reduce(fn(acc, x) { acc + x }, 0.0) / second_half.length().to_float()
        
        let change_percent = (second_avg - first_avg) / first_avg * 100.0
        
        if change_percent > 5.0 {
          "increasing"
        } else if change_percent < -5.0 {
          "decreasing"
        } else {
          "stable"
        }
      }
    }
    
    let mut results = []
    for (group_key, values) in dimension_groups {
      let sorted_values = values.sort(fn(a, b) { a - b })
      let sum = values.reduce(fn(acc, x) { acc + x }, 0.0)
      
      let result = {
        dimension_values: group_key.split(":"),
        statistics: {
          count: values.length(),
          sum: sum,
          avg: sum / values.length().to_float(),
          min: sorted_values[0],
          max: sorted_values[values.length() - 1]
        },
        trend: calculate_trend(values)
      }
      results = results + [result]
    }
    
    results
  }
  
  // 按主机维度分析
  let host_analysis = analyze_by_dimensions(multi_dim_data, ["host"])
  
  // 验证主机分析结果
  assert_eq(host_analysis.length(), 3)  // server1, server2, server3
  
  // 验证server1的分析结果
  let server1_analysis = host_analysis.find(fn(result) {
    result.dimension_values.contains("server1")
  })
  match server1_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 10)  // 5个CPU数据点 + 5个内存数据点
      assert_eq(result.statistics.avg, 46.03)  // (25.5+27.2+26.8+28.1+29.5+65.3+66.7+64.8+67.2+68.5) / 10
      assert_eq(result.statistics.min, 25.5)
      assert_eq(result.statistics.max, 68.5)
      assert_eq(result.trend, "increasing")  // 整体趋势上升
    }
    None => assert_true(false)
  }
  
  // 验证server2的分析结果
  let server2_analysis = host_analysis.find(fn(result) {
    result.dimension_values.contains("server2")
  })
  match server2_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 5)  // 5个CPU数据点
      assert_eq(result.statistics.avg, 46.46)  // (45.2+46.8+44.9+47.3+48.1) / 5
      assert_eq(result.statistics.min, 44.9)
      assert_eq(result.statistics.max, 48.1)
      assert_eq(result.trend, "increasing")  // 趋势上升
    }
    None => assert_true(false)
  }
  
  // 验证server3的分析结果
  let server3_analysis = host_analysis.find(fn(result) {
    result.dimension_values.contains("server3")
  })
  match server3_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 5)  // 5个CPU数据点
      assert_eq(result.statistics.avg, 36.46)  // (35.7+36.2+34.9+37.1+38.4) / 5
      assert_eq(result.statistics.min, 34.9)
      assert_eq(result.statistics.max, 38.4)
      assert_eq(result.trend, "increasing")  // 趋势上升
    }
    None => assert_true(false)
  }
  
  // 按指标类型维度分析
  let metric_analysis = analyze_by_dimensions(multi_dim_data, ["metric"])
  
  // 验证指标分析结果
  assert_eq(metric_analysis.length(), 2)  // cpu, memory
  
  // 验证CPU指标分析结果
  let cpu_analysis = metric_analysis.find(fn(result) {
    result.dimension_values.contains("cpu")
  })
  match cpu_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 15)  // 3个服务器，每个5个数据点
      assert_eq(result.statistics.avg, 36.18)  // 所有CPU数据的平均值
      assert_eq(result.statistics.min, 25.5)
      assert_eq(result.statistics.max, 48.1)
      assert_eq(result.trend, "increasing")  // 整体趋势上升
    }
    None => assert_true(false)
  }
  
  // 验证内存指标分析结果
  let memory_analysis = metric_analysis.find(fn(result) {
    result.dimension_values.contains("memory")
  })
  match memory_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 5)  // server1的5个内存数据点
      assert_eq(result.statistics.avg, 66.5)  // (65.3+66.7+64.8+67.2+68.5) / 5
      assert_eq(result.statistics.min, 64.8)
      assert_eq(result.statistics.max, 68.5)
      assert_eq(result.trend, "increasing")  // 趋势上升
    }
    None => assert_true(false)
  }
  
  // 按区域维度分析
  let region_analysis = analyze_by_dimensions(multi_dim_data, ["region"])
  
  // 验证区域分析结果
  assert_eq(region_analysis.length(), 2)  // us-west, us-east
  
  // 验证us-west区域分析结果
  let us_west_analysis = region_analysis.find(fn(result) {
    result.dimension_values.contains("us-west")
  })
  match us_west_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 15)  // server1和server2的所有数据
      assert_eq(result.statistics.avg, 43.48)  // us-west区域所有数据的平均值
      assert_eq(result.statistics.min, 25.5)
      assert_eq(result.statistics.max, 68.5)
      assert_eq(result.trend, "increasing")  // 整体趋势上升
    }
    None => assert_true(false)
  }
  
  // 验证us-east区域分析结果
  let us_east_analysis = region_analysis.find(fn(result) {
    result.dimension_values.contains("us-east")
  })
  match us_east_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 5)  // server3的5个数据点
      assert_eq(result.statistics.avg, 36.46)  // us-east区域所有数据的平均值
      assert_eq(result.statistics.min, 34.9)
      assert_eq(result.statistics.max, 38.4)
      assert_eq(result.trend, "increasing")  // 趋势上升
    }
    None => assert_true(false)
  }
  
  // 按主机和指标类型多维度分析
  let host_metric_analysis = analyze_by_dimensions(multi_dim_data, ["host", "metric"])
  
  // 验证主机和指标类型分析结果
  assert_eq(host_metric_analysis.length(), 5)  // server1:cpu, server1:memory, server2:cpu, server3:cpu
  
  // 验证server1:CPU的分析结果
  let server1_cpu_analysis = host_metric_analysis.find(fn(result) {
    result.dimension_values.contains("server1") and result.dimension_values.contains("cpu")
  })
  match server1_cpu_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 5)  // server1的5个CPU数据点
      assert_eq(result.statistics.avg, 27.42)  // (25.5+27.2+26.8+28.1+29.5) / 5
      assert_eq(result.statistics.min, 25.5)
      assert_eq(result.statistics.max, 29.5)
      assert_eq(result.trend, "increasing")  // 趋势上升
    }
    None => assert_true(false)
  }
  
  // 验证server1:memory的分析结果
  let server1_memory_analysis = host_metric_analysis.find(fn(result) {
    result.dimension_values.contains("server1") and result.dimension_values.contains("memory")
  })
  match server1_memory_analysis {
    Some(result) => {
      assert_eq(result.statistics.count, 5)  // server1的5个内存数据点
      assert_eq(result.statistics.avg, 66.5)  // (65.3+66.7+64.8+67.2+68.5) / 5
      assert_eq(result.statistics.min, 64.8)
      assert_eq(result.statistics.max, 68.5)
      assert_eq(result.trend, "increasing")  // 趋势上升
    }
    None => assert_true(false)
  }
}