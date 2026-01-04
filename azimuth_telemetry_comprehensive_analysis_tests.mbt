// Azimuth遥测系统综合测试用例
// 专注于遥测数据处理、分析和可视化功能

// 测试1: 遥测数据序列化与反序列化
test "telemetry data serialization and deserialization" {
  // 定义遥测数据结构
  type TelemetryPoint = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    metric_name: String,
    metric_value: Float,
    tags: Array[(String, String)]
  }
  
  // 创建测试数据
  let telemetry_point = {
    timestamp: 1640995200,
    trace_id: "trace-001",
    span_id: "span-001",
    metric_name: "response_time",
    metric_value: 125.5,
    tags: [("service", "api"), ("endpoint", "/users"), ("method", "GET")]
  }
  
  // 序列化为JSON字符串（模拟）
  let serialize_to_json = fn(point: TelemetryPoint) {
    "{" +
    "\"timestamp\":" + point.timestamp.to_string() + "," +
    "\"trace_id\":\"" + point.trace_id + "\"," +
    "\"span_id\":\"" + point.span_id + "\"," +
    "\"metric_name\":\"" + point.metric_name + "\"," +
    "\"metric_value\":" + point.metric_value.to_string() + "," +
    "\"tags\":[" +
    point.tags.map(fn(tag) {
      match tag {
        (key, value) => "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      }
    }).join(",") +
    "]" +
    "}"
  }
  
  let json_string = serialize_to_json(telemetry_point)
  
  // 验证JSON字符串包含必要字段
  assert_true(json_string.contains("\"timestamp\":1640995200"))
  assert_true(json_string.contains("\"trace_id\":\"trace-001\""))
  assert_true(json_string.contains("\"span_id\":\"span-001\""))
  assert_true(json_string.contains("\"metric_name\":\"response_time\""))
  assert_true(json_string.contains("\"metric_value\":125.5"))
  assert_true(json_string.contains("\"service\""))
  assert_true(json_string.contains("\"api\""))
  assert_true(json_string.contains("\"endpoint\""))
  assert_true(json_string.contains("\"/users\""))
  
  // 反序列化（简化版）
  let parse_json_field = fn(json: String, field: String) {
    let pattern = "\"" + field + "\":"
    let start_index = json.index_of(pattern)
    if start_index >= 0 {
      let value_start = start_index + pattern.length()
      if json[value_start] == '"' {
        let end_index = json.index_of("\"", value_start + 1)
        if end_index > value_start {
          json.substring(value_start + 1, end_index - value_start - 1)
        } else {
          ""
        }
      } else {
        // 处理数值类型
        let end_index = json.index_of(",", value_start)
        if end_index < 0 {
          end_index = json.index_of("}", value_start)
        }
        if end_index > value_start {
          json.substring(value_start, end_index - value_start)
        } else {
          ""
        }
      }
    } else {
      ""
    }
  }
  
  // 验证反序列化结果
  assert_eq(parse_json_field(json_string, "trace_id"), "trace-001")
  assert_eq(parse_json_field(json_string, "span_id"), "span-001")
  assert_eq(parse_json_field(json_string, "metric_name"), "response_time")
  assert_eq(parse_json_field(json_string, "timestamp"), "1640995200")
  assert_eq(parse_json_field(json_string, "metric_value"), "125.5")
}

// 测试2: 遥测数据采样策略
test "telemetry data sampling strategies" {
  // 定义采样策略枚举
  enum SamplingStrategy {
    Always
    Never
    Probabilistic(Float)  // 采样概率 (0.0-1.0)
    RateBased(Int)        // 每N个采样1个
  }
  
  // 定义采样决策函数
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String, span_index: Int) {
    match strategy {
      SamplingStrategy::Always => true,
      SamplingStrategy::Never => false,
      SamplingStrategy::Probabilistic(probability) => {
        // 使用trace_id的哈希值模拟确定性随机采样
        let hash = trace_id.reduce(fn(acc, char) { acc + char.to_int() }, 0)
        (hash % 100) < (probability * 100).to_int()
      },
      SamplingStrategy::RateBased(rate) => span_index % rate == 0
    }
  }
  
  // 测试始终采样策略
  assert_true(should_sample(SamplingStrategy::Always, "trace-001", 0))
  assert_true(should_sample(SamplingStrategy::Always, "trace-002", 5))
  assert_true(should_sample(SamplingStrategy::Always, "trace-003", 10))
  
  // 测试永不采样策略
  assert_false(should_sample(SamplingStrategy::Never, "trace-001", 0))
  assert_false(should_sample(SamplingStrategy::Never, "trace-002", 5))
  assert_false(should_sample(SamplingStrategy::Never, "trace-003", 10))
  
  // 测试概率采样策略
  assert_true(should_sample(SamplingStrategy::Probabilistic(1.0), "trace-001", 0))  // 100%采样
  assert_false(should_sample(SamplingStrategy::Probabilistic(0.0), "trace-002", 0)) // 0%采样
  
  // 测试基于速率的采样策略
  assert_true(should_sample(SamplingStrategy::RateBased(1), "trace-001", 0))   // 每个都采样
  assert_true(should_sample(SamplingStrategy::RateBased(1), "trace-002", 5))
  assert_true(should_sample(SamplingStrategy::RateBased(5), "trace-001", 0))   // 每5个采样1个
  assert_false(should_sample(SamplingStrategy::RateBased(5), "trace-002", 1))
  assert_false(should_sample(SamplingStrategy::RateBased(5), "trace-003", 2))
  assert_false(should_sample(SamplingStrategy::RateBased(5), "trace-004", 3))
  assert_false(should_sample(SamplingStrategy::RateBased(5), "trace-005", 4))
  assert_true(should_sample(SamplingStrategy::RateBased(5), "trace-006", 5))
  
  // 测试采样效果
  let traces = ["trace-001", "trace-002", "trace-003", "trace-004", "trace-005"]
  let span_indices = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
  
  // 使用概率采样策略采样
  let prob_strategy = SamplingStrategy::Probabilistic(0.5)  // 50%采样
  let prob_sampled = traces.reduce(fn(acc, trace) {
    let trace_sampled = span_indices.reduce(fn(trace_acc, index) {
      if should_sample(prob_strategy, trace, index) {
        trace_acc + 1
      } else {
        trace_acc
      }
    }, 0)
    acc + trace_sampled
  }, 0)
  
  // 验证采样数量（由于确定性哈希，结果应该是可预测的）
  assert_true(prob_sampled > 0)
  assert_true(prob_sampled < traces.length() * span_indices.length())
  
  // 使用基于速率的采样策略采样
  let rate_strategy = SamplingStrategy::RateBased(3)  // 每3个采样1个
  let rate_sampled = traces.reduce(fn(acc, trace) {
    let trace_sampled = span_indices.reduce(fn(trace_acc, index) {
      if should_sample(rate_strategy, trace, index) {
        trace_acc + 1
      } else {
        trace_acc
      }
    }, 0)
    acc + trace_sampled
  }, 0)
  
  // 验证采样数量（应该是总数的三分之一，向上取整）
  let expected_rate_sampled = (traces.length() * span_indices.length() + 2) / 3
  assert_eq(rate_sampled, expected_rate_sampled)
}

// 测试3: 遥测数据聚合与时间窗口
test "telemetry data aggregation with time windows" {
  // 定义遥测指标类型
  type Metric = {
    name: String,
    value: Float,
    timestamp: Int,
    tags: Array[(String, String)]
  }
  
  // 创建测试指标数据
  let metrics = [
    { name: "cpu_usage", value: 25.5, timestamp: 1640995200, tags: [("host", "server1")] },
    { name: "cpu_usage", value: 30.2, timestamp: 1640995230, tags: [("host", "server1")] },
    { name: "cpu_usage", value: 35.8, timestamp: 1640995260, tags: [("host", "server1")] },
    { name: "cpu_usage", value: 40.1, timestamp: 1640995290, tags: [("host", "server1")] },
    { name: "cpu_usage", value: 45.3, timestamp: 1640995320, tags: [("host", "server1")] },
    { name: "cpu_usage", value: 50.7, timestamp: 1640995350, tags: [("host", "server1")] },
    { name: "memory_usage", value: 60.2, timestamp: 1640995200, tags: [("host", "server1")] },
    { name: "memory_usage", value: 65.4, timestamp: 1640995260, tags: [("host", "server1")] },
    { name: "memory_usage", value: 70.8, timestamp: 1640995320, tags: [("host", "server1")] },
    { name: "memory_usage", value: 75.1, timestamp: 1640995380, tags: [("host", "server1")] }
  ]
  
  // 定义时间窗口聚合函数
  let aggregate_by_time_window = fn(metrics: Array[Metric], window_size: Int, aggregation_fn: (Array[Float]) -> Float) {
    // 按时间戳排序指标
    let sorted_metrics = metrics.sort(fn(a, b) { a.timestamp - b.timestamp })
    
    if sorted_metrics.length() == 0 {
      []
    } else {
      let start_time = sorted_metrics[0].timestamp
      let end_time = sorted_metrics[sorted_metrics.length() - 1].timestamp
      
      let mut windows = []
      let mut current_window_start = start_time
      
      while current_window_start <= end_time {
        let current_window_end = current_window_start + window_size
        let window_metrics = sorted_metrics.filter(fn(metric) {
          metric.timestamp >= current_window_start and metric.timestamp < current_window_end
        })
        
        if window_metrics.length() > 0 {
          let values = window_metrics.map(fn(metric) { metric.value })
          let aggregated_value = aggregation_fn(values)
          
          windows = windows + [{
            window_start: current_window_start,
            window_end: current_window_end,
            metric_count: window_metrics.length(),
            aggregated_value
          }]
        }
        
        current_window_start = current_window_end
      }
      
      windows
    }
  }
  
  // 定义聚合函数
  let avg = fn(values: Array[Float]) {
    let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
    sum / values.length().to_float()
  }
  
  let max = fn(values: Array[Float]) {
    values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
  }
  
  let min = fn(values: Array[Float]) {
    values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
  }
  
  // 测试CPU使用率的平均聚合
  let cpu_metrics = metrics.filter(fn(metric) { metric.name == "cpu_usage" })
  let cpu_avg_windows = aggregate_by_time_window(cpu_metrics, 180, avg)  // 3分钟窗口
  
  assert_eq(cpu_avg_windows.length(), 2)
  
  // 第一个窗口 (1640995200-1640995380)
  assert_eq(cpu_avg_windows[0].window_start, 1640995200)
  assert_eq(cpu_avg_windows[0].window_end, 1640995380)
  assert_eq(cpu_avg_windows[0].metric_count, 5)
  assert_eq(cpu_avg_windows[0].aggregated_value, 35.38)  // (25.5+30.2+35.8+40.1+45.3)/5
  
  // 第二个窗口 (1640995380-1640995560)
  assert_eq(cpu_avg_windows[1].window_start, 1640995380)
  assert_eq(cpu_avg_windows[1].window_end, 1640995560)
  assert_eq(cpu_avg_windows[1].metric_count, 1)
  assert_eq(cpu_avg_windows[1].aggregated_value, 50.7)
  
  // 测试内存使用率的最大值聚合
  let memory_metrics = metrics.filter(fn(metric) { metric.name == "memory_usage" })
  let memory_max_windows = aggregate_by_time_window(memory_metrics, 300, max)  // 5分钟窗口
  
  assert_eq(memory_max_windows.length(), 2)
  
  // 第一个窗口 (1640995200-1640995500)
  assert_eq(memory_max_windows[0].window_start, 1640995200)
  assert_eq(memory_max_windows[0].window_end, 1640995500)
  assert_eq(memory_max_windows[0].metric_count, 3)
  assert_eq(memory_max_windows[0].aggregated_value, 70.8)  // max(60.2, 65.4, 70.8)
  
  // 第二个窗口 (1640995500-1640995800)
  assert_eq(memory_max_windows[1].window_start, 1640995500)
  assert_eq(memory_max_windows[1].window_end, 1640995800)
  assert_eq(memory_max_windows[1].metric_count, 1)
  assert_eq(memory_max_windows[1].aggregated_value, 75.1)
  
  // 测试多指标聚合
  let multi_metric_aggregation = fn(metrics: Array[Metric], window_size: Int) {
    let grouped_metrics = metrics.reduce(fn(acc, metric) {
      let key = metric.name
      if acc.contains(key) {
        let updated = acc.get(key).map(fn(group) { group + [metric] })
        acc.set(key, updated.get_or_else([]))
      } else {
        acc.set(key, [metric])
      }
      acc
    }, {})
    
    grouped_metrics.map(fn(pair) {
      match pair {
        (name, group) => {
          let windows = aggregate_by_time_window(group, window_size, avg)
          (name, windows)
        }
      }
    })
  }
  
  let multi_results = multi_metric_aggregation(metrics, 300)
  assert_eq(multi_results.length(), 2)
  
  // 验证CPU指标结果
  let cpu_result = multi_results.find(fn(pair) {
    match pair {
      (name, _) => name == "cpu_usage"
    }
  })
  match cpu_result {
    Some((_, windows)) => assert_eq(windows.length(), 2)
    None => assert_true(false)
  }
  
  // 验证内存指标结果
  let memory_result = multi_results.find(fn(pair) {
    match pair {
      (name, _) => name == "memory_usage"
    }
  })
  match memory_result {
    Some((_, windows)) => assert_eq(windows.length(), 2)
    None => assert_true(false)
  }
}

// 测试4: 遥测数据异常检测
test "telemetry data anomaly detection" {
  // 定义异常类型
  enum AnomalyType {
    Spike(Float)      // 值突增
    Drop(Float)       // 值突降
    Outlier(Float)    // 离群点
    Stagnation(Int)   // 停滞（连续相同值）
  }
  
  // 定义异常检测结果
  type Anomaly = {
    timestamp: Int,
    metric_name: String,
    anomaly_type: AnomalyType,
    value: Float,
    expected_range: (Float, Float),
    confidence: Float
  }
  
  // 创建测试数据
  let metrics = [
    { name: "response_time", value: 100.0, timestamp: 1640995200 },
    { name: "response_time", value: 105.0, timestamp: 1640995230 },
    { name: "response_time", value: 98.0, timestamp: 1640995260 },
    { name: "response_time", value: 102.0, timestamp: 1640995290 },
    { name: "response_time", value: 500.0, timestamp: 1640995320 },  // 异常突增
    { name: "response_time", value: 95.0, timestamp: 1640995350 },
    { name: "response_time", value: 10.0, timestamp: 1640995380 },   // 异常突降
    { name: "response_time", value: 99.0, timestamp: 1640995410 },
    { name: "response_time", value: 101.0, timestamp: 1640995440 },
    { name: "response_time", value: 100.0, timestamp: 1640995470 },
    { name: "response_time", value: 100.0, timestamp: 1640995500 },
    { name: "response_time", value: 100.0, timestamp: 1640995530 },
    { name: "response_time", value: 100.0, timestamp: 1640995560 },
    { name: "response_time", value: 100.0, timestamp: 1640995590 },
    { name: "response_time", value: 100.0, timestamp: 1640995620 }   // 异常停滞
  ]
  
  // 定义异常检测函数
  let detect_anomalies = fn(metrics: Array[{ name: String, value: Float, timestamp: Int }]) {
    if metrics.length() < 5 {
      []
    } else {
      let mut anomalies = []
      
      // 计算移动平均值和标准差
      let calculate_stats = fn(values: Array[Float]) {
        let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
        let variance = values.reduce(fn(acc, v) { acc + (v - mean) * (v - mean) }, 0.0) / values.length().to_float()
        let std_dev = if variance > 0.0 { variance.sqrt() } else { 0.0 }
        (mean, std_dev)
      }
      
      // 检测每个点是否异常
      for i in 2..metrics.length() - 2 {
        let current = metrics[i]
        let context_values = [
          metrics[i-2].value,
          metrics[i-1].value,
          metrics[i+1].value,
          metrics[i+2].value
        ]
        let (mean, std_dev) = calculate_stats(context_values)
        let threshold = 2.0 * std_dev  // 2倍标准差阈值
        
        // 检测突增
        if current.value > mean + threshold {
          anomalies = anomalies + [{
            timestamp: current.timestamp,
            metric_name: current.name,
            anomaly_type: AnomalyType::Spike(current.value - mean),
            value: current.value,
            expected_range: (mean - threshold, mean + threshold),
            confidence: if std_dev > 0.0 { (current.value - mean) / std_dev } else { 0.0 }
          }]
        }
        // 检测突降
        else if current.value < mean - threshold {
          anomalies = anomalies + [{
            timestamp: current.timestamp,
            metric_name: current.name,
            anomaly_type: AnomalyType::Drop(mean - current.value),
            value: current.value,
            expected_range: (mean - threshold, mean + threshold),
            confidence: if std_dev > 0.0 { (mean - current.value) / std_dev } else { 0.0 }
          }]
        }
      }
      
      // 检测停滞异常
      let mut streak_start = 0
      let mut streak_length = 1
      
      for i in 1..metrics.length() {
        if metrics[i].value == metrics[i-1].value {
          streak_length = streak_length + 1
        } else {
          if streak_length >= 5 {
            anomalies = anomalies + [{
              timestamp: metrics[streak_start].timestamp,
              metric_name: metrics[streak_start].name,
              anomaly_type: AnomalyType::Stagnation(streak_length),
              value: metrics[streak_start].value,
              expected_range: (metrics[streak_start].value, metrics[streak_start].value),
              confidence: 1.0
            }]
          }
          streak_start = i
          streak_length = 1
        }
      }
      
      // 检查最后一个序列
      if streak_length >= 5 {
        anomalies = anomalies + [{
          timestamp: metrics[streak_start].timestamp,
          metric_name: metrics[streak_start].name,
          anomaly_type: AnomalyType::Stagnation(streak_length),
          value: metrics[streak_start].value,
          expected_range: (metrics[streak_start].value, metrics[streak_start].value),
          confidence: 1.0
        }]
      }
      
      anomalies
    }
  }
  
  // 执行异常检测
  let anomalies = detect_anomalies(metrics)
  
  // 验证检测结果
  assert_eq(anomalies.length(), 3)
  
  // 验证突增异常
  let spike_anomaly = anomalies.find(fn(anomaly) {
    match anomaly.anomaly_type {
      AnomalyType::Spike(_) => true
      _ => false
    }
  })
  match spike_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.timestamp, 1640995320)
      assert_eq(anomaly.metric_name, "response_time")
      assert_eq(anomaly.value, 500.0)
      match anomaly.anomaly_type {
        AnomalyType::Spike(magnitude) => assert_eq(magnitude, 400.0)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证突降异常
  let drop_anomaly = anomalies.find(fn(anomaly) {
    match anomaly.anomaly_type {
      AnomalyType::Drop(_) => true
      _ => false
    }
  })
  match drop_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.timestamp, 1640995380)
      assert_eq(anomaly.metric_name, "response_time")
      assert_eq(anomaly.value, 10.0)
      match anomaly.anomaly_type {
        AnomalyType::Drop(magnitude) => assert_eq(magnitude, 90.0)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证停滞异常
  let stagnation_anomaly = anomalies.find(fn(anomaly) {
    match anomaly.anomaly_type {
      AnomalyType::Stagnation(_) => true
      _ => false
    }
  })
  match stagnation_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.timestamp, 1640995470)
      assert_eq(anomaly.metric_name, "response_time")
      assert_eq(anomaly.value, 100.0)
      match anomaly.anomaly_type {
        AnomalyType::Stagnation(length) => assert_eq(length, 6)
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 测试异常严重程度评估
  let assess_severity = fn(anomaly: Anomaly) {
    match anomaly.anomaly_type {
      AnomalyType::Spike(magnitude) => {
        if magnitude > 300.0 { "critical" }
        else if magnitude > 100.0 { "high" }
        else if magnitude > 50.0 { "medium" }
        else { "low" }
      },
      AnomalyType::Drop(magnitude) => {
        if magnitude > 80.0 { "critical" }
        else if magnitude > 50.0 { "high" }
        else if magnitude > 20.0 { "medium" }
        else { "low" }
      },
      AnomalyType::Outlier(distance) => {
        if distance > 5.0 { "high" }
        else if distance > 3.0 { "medium" }
        else { "low" }
      },
      AnomalyType::Stagnation(duration) => {
        if duration > 10 { "high" }
        else if duration > 5 { "medium" }
        else { "low" }
      }
    }
  }
  
  // 验证严重程度评估
  match assess_severity(spike_anomaly.get_or_else({
    timestamp: 0,
    metric_name: "",
    anomaly_type: AnomalyType::Spike(0.0),
    value: 0.0,
    expected_range: (0.0, 0.0),
    confidence: 0.0
  })) {
    "critical" => assert_true(true)
    _ => assert_true(false)
  }
  
  match assess_severity(drop_anomaly.get_or_else({
    timestamp: 0,
    metric_name: "",
    anomaly_type: AnomalyType::Drop(0.0),
    value: 0.0,
    expected_range: (0.0, 0.0),
    confidence: 0.0
  })) {
    "critical" => assert_true(true)
    _ => assert_true(false)
  }
  
  match assess_severity(stagnation_anomaly.get_or_else({
    timestamp: 0,
    metric_name: "",
    anomaly_type: AnomalyType::Stagnation(0),
    value: 0.0,
    expected_range: (0.0, 0.0),
    confidence: 0.0
  })) {
    "medium" => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试5: 遥测数据关联分析
test "telemetry data correlation analysis" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float
  }
  
  // 创建测试数据
  let cpu_series = [
    { timestamp: 1640995200, value: 25.5 },
    { timestamp: 1640995230, value: 30.2 },
    { timestamp: 1640995260, value: 35.8 },
    { timestamp: 1640995290, value: 40.1 },
    { timestamp: 1640995320, value: 45.3 },
    { timestamp: 1640995350, value: 50.7 },
    { timestamp: 1640995380, value: 55.2 },
    { timestamp: 1640995410, value: 60.8 }
  ]
  
  let memory_series = [
    { timestamp: 1640995200, value: 60.2 },
    { timestamp: 1640995230, value: 62.4 },
    { timestamp: 1640995260, value: 65.8 },
    { timestamp: 1640995290, value: 68.1 },
    { timestamp: 1640995320, value: 70.3 },
    { timestamp: 1640995350, value: 75.7 },
    { timestamp: 1640995380, value: 78.2 },
    { timestamp: 1640995410, value: 80.8 }
  ]
  
  let response_time_series = [
    { timestamp: 1640995200, value: 100.0 },
    { timestamp: 1640995230, value: 105.0 },
    { timestamp: 1640995260, value: 110.0 },
    { timestamp: 1640995290, value: 125.0 },
    { timestamp: 1640995320, value: 140.0 },
    { timestamp: 1640995350, value: 160.0 },
    { timestamp: 1640995380, value: 180.0 },
    { timestamp: 1640995410, value: 200.0 }
  ]
  
  // 定义相关系数计算函数
  let calculate_correlation = fn(series1: Array[TimeSeriesPoint], series2: Array[TimeSeriesPoint]) {
    if series1.length() != series2.length() or series1.length() < 2 {
      0.0
    } else {
      let n = series1.length().to_float()
      
      // 提取值
      let values1 = series1.map(fn(point) { point.value })
      let values2 = series2.map(fn(point) { point.value })
      
      // 计算平均值
      let mean1 = values1.reduce(fn(acc, v) { acc + v }, 0.0) / n
      let mean2 = values2.reduce(fn(acc, v) { acc + v }, 0.0) / n
      
      // 计算协方差和方差
      let mut covariance = 0.0
      let mut variance1 = 0.0
      let mut variance2 = 0.0
      
      for i in 0..values1.length() {
        let diff1 = values1[i] - mean1
        let diff2 = values2[i] - mean2
        
        covariance = covariance + diff1 * diff2
        variance1 = variance1 + diff1 * diff1
        variance2 = variance2 + diff2 * diff2
      }
      
      // 计算相关系数
      if variance1 > 0.0 and variance2 > 0.0 {
        covariance / (variance1.sqrt() * variance2.sqrt())
      } else {
        0.0
      }
    }
  }
  
  // 计算相关系数
  let cpu_memory_corr = calculate_correlation(cpu_series, memory_series)
  let cpu_response_time_corr = calculate_correlation(cpu_series, response_time_series)
  let memory_response_time_corr = calculate_correlation(memory_series, response_time_series)
  
  // 验证正相关关系
  assert_true(cpu_memory_corr > 0.9)  // CPU和内存高度正相关
  assert_true(cpu_response_time_corr > 0.9)  // CPU和响应时间高度正相关
  assert_true(memory_response_time_corr > 0.9)  // 内存和响应时间高度正相关
  
  // 定义相关性强度评估函数
  let assess_correlation_strength = fn(correlation: Float) {
    if correlation.abs() > 0.8 {
      "strong"
    } else if correlation.abs() > 0.5 {
      "moderate"
    } else if correlation.abs() > 0.3 {
      "weak"
    } else {
      "negligible"
    }
  }
  
  // 验证相关性强度评估
  assert_eq(assess_correlation_strength(cpu_memory_corr), "strong")
  assert_eq(assess_correlation_strength(cpu_response_time_corr), "strong")
  assert_eq(assess_correlation_strength(memory_response_time_corr), "strong")
  
  // 测试时间滞后相关性
  let calculate_lagged_correlation = fn(series1: Array[TimeSeriesPoint], series2: Array[TimeSeriesPoint], lag: Int) {
    if series1.length() != series2.length() or lag >= series1.length() {
      0.0
    } else {
      let lagged_series1 = if lag >= 0 {
        series1.slice(lag, series1.length())
      } else {
        series1.slice(0, series1.length() + lag)
      }
      
      let lagged_series2 = if lag >= 0 {
        series2.slice(0, series2.length() - lag)
      } else {
        series2.slice(-lag, series2.length())
      }
      
      calculate_correlation(lagged_series1, lagged_series2)
    }
  }
  
  // 测试不同滞后时间的相关性
  let lag_neg_2 = calculate_lagged_correlation(cpu_series, response_time_series, -2)
  let lag_0 = calculate_lagged_correlation(cpu_series, response_time_series, 0)
  let lag_2 = calculate_lagged_correlation(cpu_series, response_time_series, 2)
  
  // 验证滞后相关性（在这个例子中，CPU变化可能先于响应时间变化）
  assert_true(lag_neg_2 > 0.9)
  assert_true(lag_0 > 0.9)
  assert_true(lag_2 > 0.9)
  
  // 定义最佳滞后时间查找函数
  let find_best_lag = fn(series1: Array[TimeSeriesPoint], series2: Array[TimeSeriesPoint], max_lag: Int) {
    let mut best_lag = 0
    let mut best_correlation = 0.0
    
    for lag in -max_lag..=max_lag {
      let correlation = calculate_lagged_correlation(series1, series2, lag)
      if correlation.abs() > best_correlation.abs() {
        best_correlation = correlation
        best_lag = lag
      }
    }
    
    (best_lag, best_correlation)
  }
  
  // 查找最佳滞后时间
  let (best_lag, best_corr) = find_best_lag(cpu_series, response_time_series, 2)
  
  // 验证最佳滞后时间
  assert_true(best_lag >= -2 and best_lag <= 2)
  assert_true(best_corr > 0.9)
  
  // 定义多变量相关性分析
  type CorrelationMatrix = Array[(String, Array[(String, Float)])>
  
  let calculate_correlation_matrix = fn(series_map: Array[(String, Array[TimeSeriesPoint])>) {
    let mut matrix = []
    
    for (name1, series1) in series_map {
      let mut correlations = []
      for (name2, series2) in series_map {
        let correlation = calculate_correlation(series1, series2)
        correlations = correlations + [(name2, correlation)]
      }
      matrix = matrix + [(name1, correlations)]
    }
    
    matrix
  }
  
  // 计算相关性矩阵
  let series_map = [
    ("cpu", cpu_series),
    ("memory", memory_series),
    ("response_time", response_time_series)
  ]
  
  let correlation_matrix = calculate_correlation_matrix(series_map)
  assert_eq(correlation_matrix.length(), 3)
  
  // 验证对角线元素（自相关）为1
  for (name, correlations) in correlation_matrix {
    let self_correlation = correlations.find(fn(pair) {
      match pair {
        (corr_name, _) => corr_name == name
      }
    })
    match self_correlation {
      Some((_, corr)) => assert_true(corr > 0.99)  // 应该接近1.0
      None => assert_true(false)
    }
  }
  
  // 验证CPU和内存的相关性
  let cpu_row = correlation_matrix.find(fn(pair) {
    match pair {
      (name, _) => name == "cpu"
    }
  })
  match cpu_row {
    Some((_, correlations)) => {
      let cpu_memory_corr = correlations.find(fn(pair) {
        match pair {
          (name, _) => name == "memory"
        }
      })
      match cpu_memory_corr {
        Some((_, corr)) => assert_true(corr > 0.9)
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试6: 遥测数据基线分析
test "telemetry data baseline analysis" {
  // 定义基线类型
  enum BaselineType {
    Fixed(Float)           // 固定基线值
    MovingAverage(Int)     // 移动平均基线（窗口大小）
    Seasonal(Int, Int)     // 季节性基线（周期，窗口大小）
    AnomalyBased(Float)    // 基于异常的基线（异常阈值）
  }
  
  // 定义基线分析结果
  type BaselineAnalysis = {
    baseline_values: Array[Float],
    deviations: Array[Float],
    anomaly_indices: Array[Int],
    baseline_type: BaselineType,
    confidence_score: Float
  }
  
  // 创建测试数据（包含周期性模式）
  let metrics = [
    { timestamp: 1640995200, value: 100.0 },  // 周期开始
    { timestamp: 1640995230, value: 105.0 },
    { timestamp: 1640995260, value: 110.0 },
    { timestamp: 1640995290, value: 105.0 },
    { timestamp: 1640995320, value: 100.0 },  // 周期结束
    { timestamp: 1640995350, value: 100.0 },  // 新周期开始
    { timestamp: 1640995380, value: 105.0 },
    { timestamp: 1640995410, value: 110.0 },
    { timestamp: 1640995440, value: 105.0 },
    { timestamp: 1640995470, value: 100.0 },  // 周期结束
    { timestamp: 1640995500, value: 100.0 },  // 新周期开始
    { timestamp: 1640995530, value: 105.0 },
    { timestamp: 1640995560, value: 110.0 },
    { timestamp: 1640995590, value: 105.0 },
    { timestamp: 1640995620, value: 100.0 },  // 周期结束
    { timestamp: 1640995650, value: 200.0 },  // 异常点
    { timestamp: 1640995680, value: 100.0 },
    { timestamp: 1640995710, value: 105.0 },
    { timestamp: 1640995740, value: 110.0 },
    { timestamp: 1640995770, value: 105.0 }
  ]
  
  // 定义基线计算函数
  let calculate_baseline = fn(values: Array[Float], baseline_type: BaselineType) {
    match baseline_type {
      BaselineType::Fixed(fixed_value) => {
        values.map(fn(_) { fixed_value })
      },
      BaselineType::MovingAverage(window_size) => {
        let mut baseline_values = []
        for i in 0..values.length() {
          let start = if i >= window_size { i - window_size } else { 0 }
          let end = i
          let window = values.slice(start, end + 1)
          let average = window.reduce(fn(acc, v) { acc + v }, 0.0) / window.length().to_float()
          baseline_values = baseline_values + [average]
        }
        baseline_values
      },
      BaselineType::Seasonal(period, window_size) => {
        let mut baseline_values = []
        for i in 0..values.length() {
          let mut seasonal_values = []
          
          // 收集相同周期位置的值
          for j in 0..values.length() {
            if j % period == i % period {
              seasonal_values = seasonal_values + [values[j]]
            }
          }
          
          // 计算季节性基线
          if seasonal_values.length() > 0 {
            let baseline = seasonal_values.reduce(fn(acc, v) { acc + v }, 0.0) / seasonal_values.length().to_float()
            baseline_values = baseline_values + [baseline]
          } else {
            baseline_values = baseline_values + [values[i]]
          }
        }
        baseline_values
      },
      BaselineType::AnomalyBased(threshold) => {
        // 首先计算中位数
        let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
        let median = if sorted_values.length() > 0 {
          let mid = sorted_values.length() / 2
          if sorted_values.length() % 2 == 0 {
            (sorted_values[mid - 1] + sorted_values[mid]) / 2.0
          } else {
            sorted_values[mid]
          }
        } else {
          0.0
        }
        
        // 计算绝对偏差
        let abs_deviations = values.map(fn(v) { (v - median).abs() })
        let mad = if abs_deviations.length() > 0 {
          abs_deviations.reduce(fn(acc, v) { acc + v }, 0.0) / abs_deviations.length().to_float()
        } else {
          0.0
        }
        
        // 使用修正的中位数绝对偏差估计标准差
        let estimated_std = mad * 1.4826
        
        // 基线为中位数，异常阈值为threshold倍标准差
        values.map(fn(_) { median })
      }
    }
  }
  
  // 定义基线分析函数
  let analyze_baseline = fn(metrics: Array[{ timestamp: Int, value: Float }], baseline_type: BaselineType, anomaly_threshold: Float) {
    let values = metrics.map(fn(m) { m.value })
    let baseline_values = calculate_baseline(values, baseline_type)
    
    // 计算偏差
    let deviations = values.map(fn(v) { v - baseline_values[values.index_of(v)] })
    
    // 计算偏差的标准差
    let mean_deviation = deviations.reduce(fn(acc, d) { acc + d }, 0.0) / deviations.length().to_float()
    let variance = deviations.reduce(fn(acc, d) { acc + (d - mean_deviation) * (d - mean_deviation) }, 0.0) / deviations.length().to_float()
    let std_deviation = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    // 识别异常点
    let mut anomaly_indices = []
    for i in 0..deviations.length() {
      if deviations[i].abs() > anomaly_threshold * std_deviation {
        anomaly_indices = anomaly_indices + [i]
      }
    }
    
    // 计算置信度分数
    let anomaly_ratio = anomaly_indices.length().to_float() / values.length().to_float()
    let confidence_score = 1.0 - anomaly_ratio
    
    {
      baseline_values,
      deviations,
      anomaly_indices,
      baseline_type,
      confidence_score
    }
  }
  
  // 测试固定基线分析
  let fixed_baseline = analyze_baseline(metrics, BaselineType::Fixed(105.0), 2.0)
  
  assert_eq(fixed_baseline.baseline_values.length(), metrics.length())
  assert_true(fixed_baseline.baseline_values.all(fn(v) { v == 105.0 }))
  assert_eq(fixed_baseline.anomaly_indices.length(), 1)  // 只有200.0是异常
  assert_eq(fixed_baseline.anomaly_indices[0], 15)      // 200.0在索引15
  assert_true(fixed_baseline.confidence_score > 0.9)
  
  // 测试移动平均基线分析
  let moving_avg_baseline = analyze_baseline(metrics, BaselineType::MovingAverage(3), 2.0)
  
  assert_eq(moving_avg_baseline.baseline_values.length(), metrics.length())
  assert_eq(moving_avg_baseline.anomaly_indices.length(), 1)  // 只有200.0是异常
  assert_eq(moving_avg_baseline.anomaly_indices[0], 15)      // 200.0在索引15
  assert_true(moving_avg_baseline.confidence_score > 0.9)
  
  // 测试季节性基线分析
  let seasonal_baseline = analyze_baseline(metrics, BaselineType::Seasonal(5, 3), 2.0)
  
  assert_eq(seasonal_baseline.baseline_values.length(), metrics.length())
  assert_eq(seasonal_baseline.anomaly_indices.length(), 1)  // 只有200.0是异常
  assert_eq(seasonal_baseline.anomaly_indices[0], 15)      // 200.0在索引15
  assert_true(seasonal_baseline.confidence_score > 0.9)
  
  // 验证季节性基线值的正确性
  // 周期性模式：100, 105, 110, 105, 100
  assert_eq(seasonal_baseline.baseline_values[0], 100.0)  // 位置0的基线
  assert_eq(seasonal_baseline.baseline_values[1], 105.0)  // 位置1的基线
  assert_eq(seasonal_baseline.baseline_values[2], 110.0)  // 位置2的基线
  assert_eq(seasonal_baseline.baseline_values[3], 105.0)  // 位置3的基线
  assert_eq(seasonal_baseline.baseline_values[4], 100.0)  // 位置4的基线
  assert_eq(seasonal_baseline.baseline_values[5], 100.0)  // 位置5的基线（新周期）
  
  // 测试基于异常的基线分析
  let anomaly_baseline = analyze_baseline(metrics, BaselineType::AnomalyBased(3.0), 2.0)
  
  assert_eq(anomaly_baseline.baseline_values.length(), metrics.length())
  assert_eq(anomaly_baseline.anomaly_indices.length(), 1)  // 只有200.0是异常
  assert_eq(anomaly_baseline.anomaly_indices[0], 15)      // 200.0在索引15
  assert_true(anomaly_baseline.confidence_score > 0.9)
  
  // 验证基于异常的基线值为中位数
  // 排序后的值：100.0 (多次), 105.0 (多次), 110.0 (多次), 200.0
  // 中位数应该是105.0
  assert_eq(anomaly_baseline.baseline_values[0], 105.0)
  
  // 测试基线比较函数
  let compare_baselines = fn(baseline1: BaselineAnalysis, baseline2: BaselineAnalysis) {
    let confidence_diff = baseline1.confidence_score - baseline2.confidence_score
    let anomaly_count_diff = baseline1.anomaly_indices.length() - baseline2.anomaly_indices.length()
    
    if confidence_diff.abs() < 0.01 {
      if anomaly_count_diff == 0 {
        "equal"
      } else if anomaly_count_diff < 0 {
        "baseline2_fewer_anomalies"
      } else {
        "baseline1_fewer_anomalies"
      }
    } else if confidence_diff > 0 {
      "baseline1_higher_confidence"
    } else {
      "baseline2_higher_confidence"
    }
  }
  
  // 比较不同基线类型
  let fixed_vs_moving_avg = compare_baselines(fixed_baseline, moving_avg_baseline)
  let fixed_vs_seasonal = compare_baselines(fixed_baseline, seasonal_baseline)
  let fixed_vs_anomaly = compare_baselines(fixed_baseline, anomaly_baseline)
  
  // 验证比较结果
  assert_eq(fixed_vs_moving_avg, "equal")
  assert_eq(fixed_vs_seasonal, "equal")
  assert_eq(fixed_vs_anomaly, "equal")
  
  // 测试最佳基线选择函数
  let select_best_baseline = fn(baselines: Array[BaselineAnalysis]) {
    baselines.reduce(fn(best, current) {
      if current.confidence_score > best.confidence_score {
        current
      } else if current.confidence_score == best.confidence_score {
        if current.anomaly_indices.length() < best.anomaly_indices.length() {
          current
        } else {
          best
        }
      } else {
        best
      }
    }, baselines[0])
  }
  
  // 选择最佳基线
  let all_baselines = [fixed_baseline, moving_avg_baseline, seasonal_baseline, anomaly_baseline]
  let best_baseline = select_best_baseline(all_baselines)
  
  // 验证最佳基线选择结果
  assert_true(best_baseline.confidence_score > 0.9)
  assert_eq(best_baseline.anomaly_indices.length(), 1)
}

// 测试7: 遥测数据趋势分析
test "telemetry data trend analysis" {
  // 定义趋势类型
  enum TrendType {
    Increasing(Float)    // 上升趋势（斜率）
    Decreasing(Float)    // 下降趋势（斜率）
    Stable(Float)        // 稳定趋势（斜率接近0）
    Volatile(Float)      // 波动趋势（方差）
  }
  
  // 定义趋势分析结果
  type TrendAnalysis = {
    trend_type: TrendType,
    slope: Float,
    correlation: Float,
    confidence: Float,
    seasonal_component: Option[Array[Float]],
    forecast_values: Option[Array[Float]]
  }
  
  // 创建测试数据
  let increasing_metrics = [
    { timestamp: 1640995200, value: 100.0 },
    { timestamp: 1640995230, value: 102.0 },
    { timestamp: 1640995260, value: 104.0 },
    { timestamp: 1640995290, value: 106.0 },
    { timestamp: 1640995320, value: 108.0 },
    { timestamp: 1640995350, value: 110.0 },
    { timestamp: 1640995380, value: 112.0 },
    { timestamp: 1640995410, value: 114.0 }
  ]
  
  let decreasing_metrics = [
    { timestamp: 1640995200, value: 200.0 },
    { timestamp: 1640995230, value: 195.0 },
    { timestamp: 1640995260, value: 190.0 },
    { timestamp: 1640995290, value: 185.0 },
    { timestamp: 1640995320, value: 180.0 },
    { timestamp: 1640995350, value: 175.0 },
    { timestamp: 1640995380, value: 170.0 },
    { timestamp: 1640995410, value: 165.0 }
  ]
  
  let volatile_metrics = [
    { timestamp: 1640995200, value: 100.0 },
    { timestamp: 1640995230, value: 150.0 },
    { timestamp: 1640995260, value: 80.0 },
    { timestamp: 1640995290, value: 120.0 },
    { timestamp: 1640995320, value: 90.0 },
    { timestamp: 1640995350, value: 140.0 },
    { timestamp: 1640995380, value: 70.0 },
    { timestamp: 1640995410, value: 130.0 }
  ]
  
  // 定义线性回归函数
  let linear_regression = fn(points: Array[{ x: Float, y: Float }]) {
    let n = points.length().to_float()
    
    if n < 2.0 {
      (0.0, 0.0)  // 斜率，截距
    } else {
      let sum_x = points.reduce(fn(acc, p) { acc + p.x }, 0.0)
      let sum_y = points.reduce(fn(acc, p) { acc + p.y }, 0.0)
      let sum_xy = points.reduce(fn(acc, p) { acc + p.x * p.y }, 0.0)
      let sum_x2 = points.reduce(fn(acc, p) { acc + p.x * p.x }, 0.0)
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      (slope, intercept)
    }
  }
  
  // 定义相关系数计算函数
  let calculate_correlation = fn(points: Array[{ x: Float, y: Float }]) {
    let n = points.length().to_float()
    
    if n < 2.0 {
      0.0
    } else {
      let sum_x = points.reduce(fn(acc, p) { acc + p.x }, 0.0)
      let sum_y = points.reduce(fn(acc, p) { acc + p.y }, 0.0)
      let sum_xy = points.reduce(fn(acc, p) { acc + p.x * p.y }, 0.0)
      let sum_x2 = points.reduce(fn(acc, p) { acc + p.x * p.x }, 0.0)
      let sum_y2 = points.reduce(fn(acc, p) { acc + p.y * p.y }, 0.0)
      
      let numerator = n * sum_xy - sum_x * sum_y
      let denominator = ((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt()
      
      if denominator > 0.0 {
        numerator / denominator
      } else {
        0.0
      }
    }
  }
  
  // 定义方差计算函数
  let calculate_variance = fn(values: Array[Float]) {
    if values.length() < 2 {
      0.0
    } else {
      let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
      let squared_diffs = values.map(fn(v) { (v - mean) * (v - mean) })
      squared_diffs.reduce(fn(acc, d) { acc + d }, 0.0) / values.length().to_float()
    }
  }
  
  // 定义趋势分析函数
  let analyze_trend = fn(metrics: Array[{ timestamp: Int, value: Float }]) {
    if metrics.length() < 2 {
      {
        trend_type: TrendType::Stable(0.0),
        slope: 0.0,
        correlation: 0.0,
        confidence: 0.0,
        seasonal_component: None,
        forecast_values: None
      }
    } else {
      // 准备回归数据点
      let start_time = metrics[0].timestamp.to_float()
      let points = metrics.map(fn(m) { 
        { x: (m.timestamp - start_time).to_float(), y: m.value } 
      })
      
      // 计算线性回归
      let (slope, intercept) = linear_regression(points)
      
      // 计算相关系数
      let correlation = calculate_correlation(points)
      
      // 计算方差
      let values = metrics.map(fn(m) { m.value })
      let variance = calculate_variance(values)
      
      // 确定趋势类型
      let trend_type = if slope.abs() < 0.1 {
        TrendType::Stable(slope)
      } else if variance > 1000.0 {
        TrendType::Volatile(variance)
      } else if slope > 0.0 {
        TrendType::Increasing(slope)
      } else {
        TrendType::Decreasing(slope)
      }
      
      // 计算置信度
      let confidence = correlation.abs()
      
      // 简单预测（基于线性回归）
      let last_timestamp = metrics[metrics.length() - 1].timestamp
      let forecast_points = [
        { x: (last_timestamp + 30 - start_time).to_float(), y: 0.0 },
        { x: (last_timestamp + 60 - start_time).to_float(), y: 0.0 },
        { x: (last_timestamp + 90 - start_time).to_float(), y: 0.0 }
      ]
      let forecast_values = Some(forecast_points.map(fn(p) { 
        slope * p.x + intercept 
      }))
      
      {
        trend_type,
        slope,
        correlation,
        confidence,
        seasonal_component: None,  // 简化实现，不考虑季节性
        forecast_values
      }
    }
  }
  
  // 分析上升趋势
  let increasing_trend = analyze_trend(increasing_metrics)
  
  match increasing_trend.trend_type {
    TrendType::Increasing(s) => {
      assert_true(s > 0.0)
      assert_eq(increasing_trend.slope, s)
    }
    _ => assert_true(false)
  }
  assert_true(increasing_trend.correlation > 0.9)
  assert_true(increasing_trend.confidence > 0.9)
  match increasing_trend.forecast_values {
    Some(forecasts) => {
      assert_eq(forecasts.length(), 3)
      assert_true(forecasts[0] > increasing_metrics[increasing_metrics.length() - 1].value)
      assert_true(forecasts[1] > forecasts[0])
      assert_true(forecasts[2] > forecasts[1])
    }
    None => assert_true(false)
  }
  
  // 分析下降趋势
  let decreasing_trend = analyze_trend(decreasing_metrics)
  
  match decreasing_trend.trend_type {
    TrendType::Decreasing(s) => {
      assert_true(s < 0.0)
      assert_eq(decreasing_trend.slope, s)
    }
    _ => assert_true(false)
  }
  assert_true(decreasing_trend.correlation > 0.9)
  assert_true(decreasing_trend.confidence > 0.9)
  match decreasing_trend.forecast_values {
    Some(forecasts) => {
      assert_eq(forecasts.length(), 3)
      assert_true(forecasts[0] < decreasing_metrics[decreasing_metrics.length() - 1].value)
      assert_true(forecasts[1] < forecasts[0])
      assert_true(forecasts[2] < forecasts[1])
    }
    None => assert_true(false)
  }
  
  // 分析波动趋势
  let volatile_trend = analyze_trend(volatile_metrics)
  
  match volatile_trend.trend_type {
    TrendType::Volatile(variance) => {
      assert_true(variance > 1000.0)
    }
    _ => assert_true(false)
  }
  assert_true(volatile_trend.correlation.abs() < 0.5)  // 低相关性
  assert_true(volatile_trend.confidence < 0.5)        // 低置信度
  
  // 测试趋势比较函数
  let compare_trends = fn(trend1: TrendAnalysis, trend2: TrendAnalysis) {
    match (trend1.trend_type, trend2.trend_type) {
      (TrendType::Increasing(s1), TrendType::Increasing(s2)) => {
        if s1 > s2 { "trend1_steeper" } else if s1 < s2 { "trend2_steeper" } else { "equal" }
      },
      (TrendType::Decreasing(s1), TrendType::Decreasing(s2)) => {
        if s1 < s2 { "trend1_steeper" } else if s1 > s2 { "trend2_steeper" } else { "equal" }
      },
      (TrendType::Volatile(v1), TrendType::Volatile(v2)) => {
        if v1 > v2 { "trend1_more_volatile" } else if v1 < v2 { "trend2_more_volatile" } else { "equal" }
      },
      (TrendType::Stable(s1), TrendType::Stable(s2)) => {
        if s1.abs() < s2.abs() { "trend1_more_stable" } else if s1.abs() > s2.abs() { "trend2_more_stable" } else { "equal" }
      },
      _ => "different_types"
    }
  }
  
  // 比较上升趋势和下降趋势
  let increasing_vs_decreasing = compare_trends(increasing_trend, decreasing_trend)
  assert_eq(increasing_vs_decreasing, "different_types")
  
  // 测试趋势强度评估函数
  let assess_trend_strength = fn(trend: TrendAnalysis) {
    if trend.confidence > 0.8 {
      "strong"
    } else if trend.confidence > 0.5 {
      "moderate"
    } else if trend.confidence > 0.3 {
      "weak"
    } else {
      "negligible"
    }
  }
  
  // 验证趋势强度评估
  assert_eq(assess_trend_strength(increasing_trend), "strong")
  assert_eq(assess_trend_strength(decreasing_trend), "strong")
  assert_eq(assess_trend_strength(volatile_trend), "weak")
  
  // 测试多指标趋势比较
  let compare_multiple_trends = fn(trends: Array[(String, TrendAnalysis)]) {
    let mut sorted_trends = trends.sort(fn(a, b) {
      match (a, b) {
        ((_, trend_a), (_, trend_b)) => {
          if trend_a.confidence > trend_b.confidence { -1 }
          else if trend_a.confidence < trend_b.confidence { 1 }
          else { 0 }
        }
      }
    })
    sorted_trends
  }
  
  // 创建多个趋势分析结果
  let multiple_trends = [
    ("cpu", increasing_trend),
    ("memory", decreasing_trend),
    ("response_time", volatile_trend)
  ]
  
  let sorted_trends = compare_multiple_trends(multiple_trends)
  
  // 验证排序结果（按置信度降序）
  assert_eq(sorted_trends[0], ("cpu", increasing_trend))
  assert_eq(sorted_trends[1], ("memory", decreasing_trend))
  assert_eq(sorted_trends[2], ("response_time", volatile_trend))
}

// 测试8: 遥测数据根因分析
test "telemetry data root cause analysis" {
  // 定义事件类型
  enum EventType {
    Metric(String, Float)      // 指标名称，值
    Log(String, String)        // 日志级别，消息
    Span(String, String, Int)  // 操作名称，状态，持续时间
    Alert(String, String)      // 警报名称，严重程度
  }
  
  // 定义事件
  type Event = {
    timestamp: Int,
    trace_id: String,
    service: String,
    event_type: EventType
  }
  
  // 定义根因分析结果
  type RootCauseAnalysis = {
    root_cause_events: Array[Event],
    contributing_factors: Array[Event],
    affected_services: Array[String],
    timeline: Array[(Int, String)],  // 时间戳，描述
    confidence: Float
  }
  
  // 创建测试事件序列
  let events = [
    // 基线正常状态
    { timestamp: 1640995200, trace_id: "trace-001", service: "database", event_type: EventType::Metric("cpu_usage", 30.0) },
    { timestamp: 1640995210, trace_id: "trace-002", service: "database", event_type: EventType::Metric("memory_usage", 60.0) },
    { timestamp: 1640995220, trace_id: "trace-003", service: "api", event_type: EventType::Span("user_request", "ok", 100) },
    { timestamp: 1640995230, trace_id: "trace-004", service: "database", event_type: EventType::Span("db_query", "ok", 50) },
    
    // 问题开始
    { timestamp: 1640995240, trace_id: "trace-005", service: "database", event_type: EventType::Metric("cpu_usage", 80.0) },
    { timestamp: 1640995250, trace_id: "trace-006", service: "database", event_type: EventType::Log("warn", "High CPU usage detected") },
    { timestamp: 1640995260, trace_id: "trace-007", service: "database", event_type: EventType::Span("db_query", "slow", 500) },
    { timestamp: 1640995270, trace_id: "trace-008", service: "api", event_type: EventType::Span("user_request", "ok", 600) },
    
    // 问题扩散
    { timestamp: 1640995280, trace_id: "trace-009", service: "database", event_type: EventType::Metric("memory_usage", 85.0) },
    { timestamp: 1640995290, trace_id: "trace-010", service: "database", event_type: EventType::Log("error", "Database connection pool exhausted") },
    { timestamp: 1640995300, trace_id: "trace-011", service: "api", event_type: EventType::Span("user_request", "error", 1000) },
    { timestamp: 1640995310, trace_id: "trace-012", service: "api", event_type: EventType::Log("error", "Request timeout") },
    
    // 警报触发
    { timestamp: 1640995320, trace_id: "trace-013", service: "monitoring", event_type: EventType::Alert("high_response_time", "critical") },
    { timestamp: 1640995330, trace_id: "trace-014", service: "database", event_type: EventType::Metric("cpu_usage", 95.0) },
    { timestamp: 1640995340, trace_id: "trace-015", service: "database", event_type: EventType::Span("db_query", "error", 2000) },
    
    // 恢复开始
    { timestamp: 1640995350, trace_id: "trace-016", service: "database", event_type: EventType::Log("info", "Database restart initiated") },
    { timestamp: 1640995360, trace_id: "trace-017", service: "database", event_type: EventType::Metric("cpu_usage", 40.0) },
    { timestamp: 1640995370, trace_id: "trace-018", service: "api", event_type: EventType::Span("user_request", "ok", 150) }
  ]
  
  // 定义事件过滤函数
  let filter_events = fn(events: Array[Event], predicate: (Event) -> Bool) {
    events.filter(predicate)
  }
  
  // 定义事件排序函数
  let sort_events = fn(events: Array[Event]) {
    events.sort(fn(a, b) { a.timestamp - b.timestamp })
  }
  
  // 定义根因分析函数
  let analyze_root_cause = fn(events: Array[Event]) {
    let sorted_events = sort_events(events)
    
    // 识别关键事件
    let error_events = filter_events(sorted_events, fn(event) {
      match event.event_type {
        EventType::Log(level, _) => level == "error"
        EventType::Span(_, status, _) => status == "error"
        EventType::Alert(_, severity) => severity == "critical"
        _ => false
      }
    })
    
    let warning_events = filter_events(sorted_events, fn(event) {
      match event.event_type {
        EventType::Log(level, _) => level == "warn"
        EventType::Span(_, status, _) => status == "slow"
        _ => false
      }
    })
    
    let metric_anomalies = filter_events(sorted_events, fn(event) {
      match event.event_type {
        EventType::Metric(name, value) => {
          match name {
            "cpu_usage" => value > 70.0
            "memory_usage" => value > 80.0
            _ => false
          }
        }
        _ => false
      }
    })
    
    // 查找最早的关键事件作为潜在根因
    let root_cause_events = if error_events.length() > 0 {
      [error_events[0]]
    } else if warning_events.length() > 0 {
      [warning_events[0]]
    } else if metric_anomalies.length() > 0 {
      [metric_anomalies[0]]
    } else {
      []
    }
    
    // 识别贡献因素
    let contributing_factors = warning_events + metric_anomalies
    
    // 识别受影响的服务
    let affected_services = sorted_events.reduce(fn(acc, event) {
      if not(acc.contains(event.service)) {
        acc + [event.service]
      } else {
        acc
      }
    }, [])
    
    // 构建时间线
    let timeline = sorted_events.map(fn(event) {
      let description = match event.event_type {
        EventType::Metric(name, value) => event.service + ": " + name + " = " + value.to_string()
        EventType::Log(level, message) => event.service + " [" + level + "]: " + message
        EventType::Span(operation, status, duration) => event.service + ": " + operation + " " + status + " (" + duration.to_string() + "ms)"
        EventType::Alert(name, severity) => event.service + ": " + name + " [" + severity + "]"
      }
      (event.timestamp, description)
    })
    
    // 计算置信度（基于事件的一致性）
    let confidence = if root_cause_events.length() > 0 {
      let root_cause_time = root_cause_events[0].timestamp
      let related_events = sorted_events.filter(fn(event) {
        event.timestamp >= root_cause_time and event.timestamp <= root_cause_time + 300  // 5分钟窗口
      })
      related_events.length().to_float() / sorted_events.length().to_float()
    } else {
      0.0
    }
    
    {
      root_cause_events,
      contributing_factors,
      affected_services,
      timeline,
      confidence
    }
  }
  
  // 执行根因分析
  let analysis = analyze_root_cause(events)
  
  // 验证根因事件
  assert_eq(analysis.root_cause_events.length(), 1)
  assert_eq(analysis.root_cause_events[0].timestamp, 1640995250)  // 第一个警告事件
  match analysis.root_cause_events[0].event_type {
    EventType::Log(level, message) => {
      assert_eq(level, "warn")
      assert_eq(message, "High CPU usage detected")
    }
    _ => assert_true(false)
  }
  
  // 验证贡献因素
  assert_true(analysis.contributing_factors.length() > 0)
  assert_true(analysis.contributing_factors.any(fn(event) {
    match event.event_type {
      EventType::Log(level, _) => level == "warn"
      EventType::Span(_, status, _) => status == "slow"
      EventType::Metric(_, _) => true
      _ => false
    }
  }))
  
  // 验证受影响的服务
  assert_eq(analysis.affected_services.length(), 3)
  assert_true(analysis.affected_services.contains("database"))
  assert_true(analysis.affected_services.contains("api"))
  assert_true(analysis.affected_services.contains("monitoring"))
  
  // 验证时间线
  assert_eq(analysis.timeline.length(), events.length())
  assert_eq(analysis.timeline[0], (1640995200, "database: cpu_usage = 30.0"))
  assert_eq(analysis.timeline[analysis.timeline.length() - 1], (1640995370, "api: user_request ok (150ms)"))
  
  // 验证置信度
  assert_true(analysis.confidence > 0.5)
  
  // 测试根因假设生成
  let generate_root_cause_hypotheses = fn(analysis: RootCauseAnalysis) {
    if analysis.root_cause_events.length() == 0 {
      []
    } else {
      let root_cause = analysis.root_cause_events[0]
      let mut hypotheses = []
      
      match root_cause.event_type {
        EventType::Log(level, message) => {
          if message.contains("CPU") {
            hypotheses = hypotheses + ["CPU资源不足导致服务性能下降"]
          }
          if message.contains("memory") {
            hypotheses = hypotheses + ["内存泄漏导致系统不稳定"]
          }
          if message.contains("connection") {
            hypotheses = hypotheses + ["连接池耗尽导致请求失败"]
          }
        }
        EventType::Metric(name, value) => {
          if name.contains("cpu") && value > 80.0 {
            hypotheses = hypotheses + ["CPU使用率过高导致系统响应缓慢"]
          }
          if name.contains("memory") && value > 80.0 {
            hypotheses = hypotheses + ["内存使用率过高导致系统不稳定"]
          }
        }
        EventType::Span(operation, status, duration) => {
          if status == "error" {
            hypotheses = hypotheses + [operation + "操作失败导致服务异常"]
          }
          if status == "slow" {
            hypotheses = hypotheses + [operation + "操作缓慢导致服务响应延迟"]
          }
        }
        _ => ()
      }
      
      // 基于受影响的服务生成假设
      if analysis.affected_services.contains("database") {
        hypotheses = hypotheses + ["数据库服务异常导致依赖服务受影响"]
      }
      
      hypotheses
    }
  }
  
  // 生成根因假设
  let hypotheses = generate_root_cause_hypotheses(analysis)
  
  // 验证根因假设
  assert_true(hypotheses.length() > 0)
  assert_true(hypotheses.any(fn(h) { h.contains("CPU") }))
  
  // 测试根因分析报告生成
  let generate_root_cause_report = fn(analysis: RootCauseAnalysis, hypotheses: Array[String]) {
    let mut report = "根因分析报告\n"
    report = report + "=============\n\n"
    
    report = report + "根本原因:\n"
    for event in analysis.root_cause_events {
      match event.event_type {
        EventType::Log(level, message) => {
          report = report + "- " + event.service + " [" + level + "]: " + message + "\n"
        }
        EventType::Metric(name, value) => {
          report = report + "- " + event.service + ": " + name + " = " + value.to_string() + "\n"
        }
        EventType::Span(operation, status, duration) => {
          report = report + "- " + event.service + ": " + operation + " " + status + " (" + duration.to_string() + "ms)\n"
        }
        EventType::Alert(name, severity) => {
          report = report + "- " + event.service + ": " + name + " [" + severity + "]\n"
        }
      }
    }
    
    report = report + "\n贡献因素:\n"
    for event in analysis.contributing_factors {
      match event.event_type {
        EventType::Log(level, message) => {
          report = report + "- " + event.service + " [" + level + "]: " + message + "\n"
        }
        EventType::Metric(name, value) => {
          report = report + "- " + event.service + ": " + name + " = " + value.to_string() + "\n"
        }
        EventType::Span(operation, status, duration) => {
          report = report + "- " + event.service + ": " + operation + " " + status + " (" + duration.to_string() + "ms)\n"
        }
        _ => ()
      }
    }
    
    report = report + "\n受影响的服务:\n"
    for service in analysis.affected_services {
      report = report + "- " + service + "\n"
    }
    
    report = report + "\n根因假设:\n"
    for hypothesis in hypotheses {
      report = report + "- " + hypothesis + "\n"
    }
    
    report = report + "\n分析置信度: " + (analysis.confidence * 100.0).to_string() + "%\n"
    
    report
  }
  
  // 生成根因分析报告
  let report = generate_root_cause_report(analysis, hypotheses)
  
  // 验证报告内容
  assert_true(report.contains("根因分析报告"))
  assert_true(report.contains("根本原因"))
  assert_true(report.contains("贡献因素"))
  assert_true(report.contains("受影响的服务"))
  assert_true(report.contains("根因假设"))
  assert_true(report.contains("分析置信度"))
  assert_true(report.contains("High CPU usage detected"))
  assert_true(report.contains("CPU资源不足导致服务性能下降"))
  assert_true(report.contains("database"))
  assert_true(report.contains("api"))
  assert_true(report.contains("monitoring"))
}

// 测试9: 遥测数据预测分析
test "telemetry data predictive analysis" {
  // 定义预测模型类型
  enum PredictionModel {
    LinearRegression(Float, Float)  // 斜率，截距
    MovingAverage(Int)              // 窗口大小
    ExponentialSmoothing(Float)     // 平滑因子
    Seasonal(Int, Int)              // 周期，窗口大小
  }
  
  // 定义预测结果
  type PredictionResult = {
    predicted_values: Array[Float],
    confidence_intervals: Array[(Float, Float)],  // 下限，上限
    model_accuracy: Float,
    model_type: PredictionModel
  }
  
  // 创建测试数据
  let metrics = [
    { timestamp: 1640995200, value: 100.0 },
    { timestamp: 1640995230, value: 105.0 },
    { timestamp: 1640995260, value: 110.0 },
    { timestamp: 1640995290, value: 115.0 },
    { timestamp: 1640995320, value: 120.0 },
    { timestamp: 1640995350, value: 125.0 },
    { timestamp: 1640995380, value: 130.0 },
    { timestamp: 1640995410, value: 135.0 },
    { timestamp: 1640995440, value: 140.0 },
    { timestamp: 1640995470, value: 145.0 }
  ]
  
  // 定义线性回归预测函数
  let linear_regression_predict = fn(points: Array[{ x: Float, y: Float }], future_points: Array[Float]) {
    let n = points.length().to_float()
    
    if n < 2.0 {
      (future_points.map(fn(_) { 0.0 }), 0.0)
    } else {
      let sum_x = points.reduce(fn(acc, p) { acc + p.x }, 0.0)
      let sum_y = points.reduce(fn(acc, p) { acc + p.y }, 0.0)
      let sum_xy = points.reduce(fn(acc, p) { acc + p.x * p.y }, 0.0)
      let sum_x2 = points.reduce(fn(acc, p) { acc + p.x * p.x }, 0.0)
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      // 计算预测值
      let predictions = future_points.map(fn(x) { slope * x + intercept })
      
      // 计算模型准确性（R²）
      let y_mean = sum_y / n
      let total_sum_squares = points.reduce(fn(acc, p) { acc + (p.y - y_mean) * (p.y - y_mean) }, 0.0)
      let residual_sum_squares = points.reduce(fn(acc, p) {
        let predicted = slope * p.x + intercept
        acc + (p.y - predicted) * (p.y - predicted)
      }, 0.0)
      
      let r_squared = if total_sum_squares > 0.0 {
        1.0 - (residual_sum_squares / total_sum_squares)
      } else {
        0.0
      }
      
      (predictions, r_squared)
    }
  }
  
  // 定义移动平均预测函数
  let moving_average_predict = fn(values: Array[Float], window_size: Int, steps: Int) {
    if values.length() < window_size {
      (steps.to_array().map(fn(_) { 0.0 }), 0.0)
    } else {
      let mut predictions = []
      let mut current_values = values
      
      for _ in 0..steps {
        let window = current_values.slice(current_values.length() - window_size, current_values.length())
        let average = window.reduce(fn(acc, v) { acc + v }, 0.0) / window.length().to_float()
        predictions = predictions + [average]
        current_values = current_values + [average]
      }
      
      // 计算模型准确性（基于最近的窗口）
      let actual_values = values.slice(values.length() - window_size, values.length())
      let predicted_values = actual_values.map(fn(_) { actual_values.reduce(fn(acc, v) { acc + v }, 0.0) / actual_values.length().to_float() })
      
      let mse = actual_values.reduce(fn(acc, actual, i) {
        let predicted = predicted_values[i]
        acc + (actual - predicted) * (actual - predicted)
      }, 0.0) / actual_values.length().to_float()
      
      let variance = actual_values.reduce(fn(acc, actual) {
        let mean = actual_values.reduce(fn(acc, v) { acc + v }, 0.0) / actual_values.length().to_float()
        acc + (actual - mean) * (actual - mean)
      }, 0.0) / actual_values.length().to_float()
      
      let r_squared = if variance > 0.0 {
        1.0 - (mse / variance)
      } else {
        0.0
      }
      
      (predictions, r_squared)
    }
  }
  
  // 定义指数平滑预测函数
  let exponential_smoothing_predict = fn(values: Array[Float], alpha: Float, steps: Int) {
    if values.length() == 0 {
      (steps.to_array().map(fn(_) { 0.0 }), 0.0)
    } else {
      // 计算平滑值
      let mut smoothed_values = [values[0]]
      for i in 1..values.length() {
        let smoothed = alpha * values[i] + (1.0 - alpha) * smoothed_values[i-1]
        smoothed_values = smoothed_values + [smoothed]
      }
      
      // 预测未来值
      let mut predictions = []
      let last_smoothed = smoothed_values[smoothed_values.length() - 1]
      
      for _ in 0..steps {
        predictions = predictions + [last_smoothed]
      }
      
      // 计算模型准确性
      let mse = values.reduce(fn(acc, actual, i) {
        let predicted = if i == 0 { actual } else { smoothed_values[i-1] }
        acc + (actual - predicted) * (actual - predicted)
      }, 0.0) / values.length().to_float()
      
      let variance = values.reduce(fn(acc, actual) {
        let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
        acc + (actual - mean) * (actual - mean)
      }, 0.0) / values.length().to_float()
      
      let r_squared = if variance > 0.0 {
        1.0 - (mse / variance)
      } else {
        0.0
      }
      
      (predictions, r_squared)
    }
  }
  
  // 定义季节性预测函数
  let seasonal_predict = fn(values: Array[Float], period: Int, steps: Int) {
    if values.length() < period {
      (steps.to_array().map(fn(_) { 0.0 }), 0.0)
    } else {
      // 计算季节性模式
      let mut seasonal_pattern = []
      for i in 0..period {
        let mut seasonal_values = []
        for j in i..values.length() {
          if j % period == i {
            seasonal_values = seasonal_values + [values[j]]
          }
        }
        let seasonal_avg = seasonal_values.reduce(fn(acc, v) { acc + v }, 0.0) / seasonal_values.length().to_float()
        seasonal_pattern = seasonal_pattern + [seasonal_avg]
      }
      
      // 预测未来值
      let mut predictions = []
      for i in 0..steps {
        let seasonal_index = i % period
        predictions = predictions + [seasonal_pattern[seasonal_index]]
      }
      
      // 计算模型准确性
      let predicted_values = values.map(fn(v, i) { seasonal_pattern[i % period] })
      let mse = values.reduce(fn(acc, actual, i) {
        let predicted = predicted_values[i]
        acc + (actual - predicted) * (actual - predicted)
      }, 0.0) / values.length().to_float()
      
      let variance = values.reduce(fn(acc, actual) {
        let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
        acc + (actual - mean) * (actual - mean)
      }, 0.0) / values.length().to_float()
      
      let r_squared = if variance > 0.0 {
        1.0 - (mse / variance)
      } else {
        0.0
      }
      
      (predictions, r_squared)
    }
  }
  
  // 定义通用预测函数
  let predict = fn(metrics: Array[{ timestamp: Int, value: Float }], model: PredictionModel, steps: Int) {
    let values = metrics.map(fn(m) { m.value })
    
    match model {
      PredictionModel::LinearRegression(slope, intercept) => {
        let start_time = metrics[0].timestamp.to_float()
        let points = metrics.map(fn(m) { 
          { x: (m.timestamp - start_time).to_float(), y: m.value } 
        })
        let future_points = steps.to_array().map(fn(i) { 
          (metrics[metrics.length() - 1].timestamp - start_time + (i + 1) * 30).to_float() 
        })
        let (predictions, accuracy) = linear_regression_predict(points, future_points)
        
        // 计算置信区间（简化实现）
        let confidence_intervals = predictions.map(fn(p) { (p * 0.9, p * 1.1) })
        
        {
          predicted_values: predictions,
          confidence_intervals,
          model_accuracy: accuracy,
          model_type: model
        }
      },
      PredictionModel::MovingAverage(window_size) => {
        let (predictions, accuracy) = moving_average_predict(values, window_size, steps)
        
        // 计算置信区间
        let confidence_intervals = predictions.map(fn(p) { (p * 0.8, p * 1.2) })
        
        {
          predicted_values: predictions,
          confidence_intervals,
          model_accuracy: accuracy,
          model_type: model
        }
      },
      PredictionModel::ExponentialSmoothing(alpha) => {
        let (predictions, accuracy) = exponential_smoothing_predict(values, alpha, steps)
        
        // 计算置信区间
        let confidence_intervals = predictions.map(fn(p) { (p * 0.85, p * 1.15) })
        
        {
          predicted_values: predictions,
          confidence_intervals,
          model_accuracy: accuracy,
          model_type: model
        }
      },
      PredictionModel::Seasonal(period, _) => {
        let (predictions, accuracy) = seasonal_predict(values, period, steps)
        
        // 计算置信区间
        let confidence_intervals = predictions.map(fn(p) { (p * 0.9, p * 1.1) })
        
        {
          predicted_values: predictions,
          confidence_intervals,
          model_accuracy: accuracy,
          model_type: model
        }
      }
    }
  }
  
  // 测试线性回归预测
  let linear_model = PredictionModel::LinearRegression(0.0, 0.0)  // 参数将被重新计算
  let linear_prediction = predict(metrics, linear_model, 3)
  
  assert_eq(linear_prediction.predicted_values.length(), 3)
  assert_eq(linear_prediction.confidence_intervals.length(), 3)
  assert_true(linear_prediction.model_accuracy > 0.9)  // 高准确性
  
  // 验证预测值的递增趋势
  assert_true(linear_prediction.predicted_values[0] > metrics[metrics.length() - 1].value)
  assert_true(linear_prediction.predicted_values[1] > linear_prediction.predicted_values[0])
  assert_true(linear_prediction.predicted_values[2] > linear_prediction.predicted_values[1])
  
  // 测试移动平均预测
  let moving_avg_model = PredictionModel::MovingAverage(3)
  let moving_avg_prediction = predict(metrics, moving_avg_model, 3)
  
  assert_eq(moving_avg_prediction.predicted_values.length(), 3)
  assert_eq(moving_avg_prediction.confidence_intervals.length(), 3)
  assert_true(moving_avg_prediction.model_accuracy >= 0.0)
  
  // 测试指数平滑预测
  let exp_smoothing_model = PredictionModel::ExponentialSmoothing(0.3)
  let exp_smoothing_prediction = predict(metrics, exp_smoothing_model, 3)
  
  assert_eq(exp_smoothing_prediction.predicted_values.length(), 3)
  assert_eq(exp_smoothing_prediction.confidence_intervals.length(), 3)
  assert_true(exp_smoothing_prediction.model_accuracy >= 0.0)
  
  // 测试季节性预测
  let seasonal_model = PredictionModel::Seasonal(5, 3)
  let seasonal_prediction = predict(metrics, seasonal_model, 5)
  
  assert_eq(seasonal_prediction.predicted_values.length(), 5)
  assert_eq(seasonal_prediction.confidence_intervals.length(), 5)
  assert_true(seasonal_prediction.model_accuracy >= 0.0)
  
  // 测试模型比较函数
  let compare_models = fn(predictions: Array[PredictionResult]) {
    predictions.sort(fn(a, b) {
      if a.model_accuracy > b.model_accuracy { -1 }
      else if a.model_accuracy < b.model_accuracy { 1 }
      else { 0 }
    })
  }
  
  // 比较不同模型的预测结果
  let all_predictions = [
    linear_prediction,
    moving_avg_prediction,
    exp_smoothing_prediction,
    seasonal_prediction
  ]
  
  let sorted_predictions = compare_models(all_predictions)
  
  // 验证排序结果（按准确性降序）
  assert_true(sorted_predictions[0].model_accuracy >= sorted_predictions[1].model_accuracy)
  assert_true(sorted_predictions[1].model_accuracy >= sorted_predictions[2].model_accuracy)
  assert_true(sorted_predictions[2].model_accuracy >= sorted_predictions[3].model_accuracy)
  
  // 测试预测异常检测
  let detect_prediction_anomalies = fn(prediction: PredictionResult, anomaly_threshold: Float) {
    let mut anomalies = []
    
    for i in 0..prediction.predicted_values.length() {
      let predicted = prediction.predicted_values[i]
      let (lower_bound, upper_bound) = prediction.confidence_intervals[i]
      
      // 检查预测值是否在合理范围内
      if predicted < lower_bound or predicted > upper_bound {
        anomalies = anomalies + {
          index: i,
          predicted_value: predicted,
          confidence_interval: (lower_bound, upper_bound),
          deviation: if predicted < lower_bound { 
            lower_bound - predicted 
          } else { 
            predicted - upper_bound 
          }
        }
      }
    }
    
    anomalies
  }
  
  // 检测预测异常
  let linear_anomalies = detect_prediction_anomalies(linear_prediction, 0.1)
  let moving_avg_anomalies = detect_prediction_anomalies(moving_avg_prediction, 0.1)
  
  // 验证异常检测结果（在我们的例子中，应该没有异常）
  assert_eq(linear_anomalies.length(), 0)
  assert_eq(moving_avg_anomalies.length(), 0)
  
  // 测试预测报告生成
  let generate_prediction_report = fn(prediction: PredictionResult, model_name: String) {
    let mut report = model_name + " 预测报告\n"
    report = report + "==================\n\n"
    
    report = report + "预测值:\n"
    for i in 0..prediction.predicted_values.length() {
      report = report + "- 步骤 " + (i + 1).to_string() + ": " + prediction.predicted_values[i].to_string() + "\n"
    }
    
    report = report + "\n置信区间:\n"
    for i in 0..prediction.confidence_intervals.length() {
      let (lower, upper) = prediction.confidence_intervals[i]
      report = report + "- 步骤 " + (i + 1).to_string() + ": [" + lower.to_string() + ", " + upper.to_string() + "]\n"
    }
    
    report = report + "\n模型准确性: " + (prediction.model_accuracy * 100.0).to_string() + "%\n"
    
    report
  }
  
  // 生成预测报告
  let linear_report = generate_prediction_report(linear_prediction, "线性回归")
  let moving_avg_report = generate_prediction_report(moving_avg_prediction, "移动平均")
  
  // 验证报告内容
  assert_true(linear_report.contains("线性回归 预测报告"))
  assert_true(linear_report.contains("预测值"))
  assert_true(linear_report.contains("置信区间"))
  assert_true(linear_report.contains("模型准确性"))
  
  assert_true(moving_avg_report.contains("移动平均 预测报告"))
  assert_true(moving_avg_report.contains("预测值"))
  assert_true(moving_avg_report.contains("置信区间"))
  assert_true(moving_avg_report.contains("模型准确性"))
}

// 测试10: 遥测数据可视化分析
test "telemetry data visualization analysis" {
  // 定义图表类型
  enum ChartType {
    LineChart      // 线图
    BarChart       // 柱状图
    PieChart       // 饼图
    ScatterPlot    // 散点图
    Heatmap        // 热力图
  }
  
  // 定义数据点
  type DataPoint = {
    x: Float,
    y: Float,
    label: Option[String],
    color: Option[String]
  }
  
  // 定义图表数据
  type ChartData = {
    title: String,
    chart_type: ChartType,
    x_axis_label: String,
    y_axis_label: String,
    data_points: Array[DataPoint],
    annotations: Array[String]
  }
  
  // 创建测试数据
  let time_series_metrics = [
    { timestamp: 1640995200, value: 100.0, label: "10:00" },
    { timestamp: 1640995230, value: 105.0, label: "10:05" },
    { timestamp: 1640995260, value: 110.0, label: "10:10" },
    { timestamp: 1640995290, value: 115.0, label: "10:15" },
    { timestamp: 1640995320, value: 120.0, label: "10:20" },
    { timestamp: 1640995350, value: 125.0, label: "10:25" },
    { timestamp: 1640995380, value: 130.0, label: "10:30" },
    { timestamp: 1640995410, value: 135.0, label: "10:35" }
  ]
  
  let category_metrics = [
    { category: "api", value: 450.0, color: "blue" },
    { category: "database", value: 320.0, color: "green" },
    { category: "cache", value: 180.0, color: "orange" },
    { category: "queue", value: 150.0, color: "red" },
    { category: "storage", value: 120.0, color: "purple" }
  ]
  
  let correlation_metrics = [
    { x: 25.5, y: 100.0, label: "Point A" },
    { x: 30.2, y: 105.0, label: "Point B" },
    { x: 35.8, y: 110.0, label: "Point C" },
    { x: 40.1, y: 125.0, label: "Point D" },
    { x: 45.3, y: 140.0, label: "Point E" },
    { x: 50.7, y: 160.0, label: "Point F" },
    { x: 55.2, y: 180.0, label: "Point G" },
    { x: 60.8, y: 200.0, label: "Point H" }
  ]
  
  // 定义线图生成函数
  let generate_line_chart = fn(metrics: Array[{ timestamp: Int, value: Float, label: String }], title: String, x_label: String, y_label: String) {
    let start_time = metrics[0].timestamp.to_float()
    let data_points = metrics.map(fn(m) {
      {
        x: (m.timestamp - start_time).to_float(),
        y: m.value,
        label: Some(m.label),
        color: None
      }
    })
    
    // 添加趋势线注释
    let first_value = metrics[0].value
    let last_value = metrics[metrics.length() - 1].value
    let trend = if last_value > first_value { "上升" } else if last_value < first_value { "下降" } else { "平稳" }
    let annotations = ["趋势: " + trend, "变化: " + (last_value - first_value).to_string()]
    
    {
      title,
      chart_type: ChartType::LineChart,
      x_axis_label: x_label,
      y_axis_label: y_label,
      data_points,
      annotations
    }
  }
  
  // 定义柱状图生成函数
  let generate_bar_chart = fn(metrics: Array[{ category: String, value: Float, color: String }], title: String, x_label: String, y_label: String) {
    let data_points = metrics.map(fn(m, i) {
      {
        x: i.to_float(),
        y: m.value,
        label: Some(m.category),
        color: Some(m.color)
      }
    })
    
    // 添加最大值注释
    let max_value = metrics.reduce(fn(acc, m) { if m.value > acc { m.value } else { acc } }, metrics[0].value)
    let max_category = metrics.find(fn(m) { m.value == max_value })
    let annotations = match max_category {
      Some(m) => ["最大值: " + m.category + " (" + max_value.to_string() + ")"]
      None => ["最大值: " + max_value.to_string()]
    }
    
    {
      title,
      chart_type: ChartType::BarChart,
      x_axis_label: x_label,
      y_axis_label: y_label,
      data_points,
      annotations
    }
  }
  
  // 定义散点图生成函数
  let generate_scatter_plot = fn(metrics: Array[{ x: Float, y: Float, label: String }], title: String, x_label: String, y_label: String) {
    let data_points = metrics.map(fn(m) {
      {
        x: m.x,
        y: m.y,
        label: Some(m.label),
        color: None
      }
    })
    
    // 计算相关系数
    let n = metrics.length().to_float()
    let sum_x = metrics.reduce(fn(acc, m) { acc + m.x }, 0.0)
    let sum_y = metrics.reduce(fn(acc, m) { acc + m.y }, 0.0)
    let sum_xy = metrics.reduce(fn(acc, m) { acc + m.x * m.y }, 0.0)
    let sum_x2 = metrics.reduce(fn(acc, m) { acc + m.x * m.x }, 0.0)
    let sum_y2 = metrics.reduce(fn(acc, m) { acc + m.y * m.y }, 0.0)
    
    let correlation = if n > 1.0 {
      let numerator = n * sum_xy - sum_x * sum_y
      let denominator = ((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt()
      if denominator > 0.0 { numerator / denominator } else { 0.0 }
    } else {
      0.0
    }
    
    let correlation_desc = if correlation > 0.7 { "强正相关" }
                          else if correlation > 0.3 { "弱正相关" }
                          else if correlation > -0.3 { "无相关" }
                          else if correlation > -0.7 { "弱负相关" }
                          else { "强负相关" }
    
    let annotations = ["相关系数: " + correlation.to_string() + " (" + correlation_desc + ")"]
    
    {
      title,
      chart_type: ChartType::ScatterPlot,
      x_axis_label: x_label,
      y_axis_label: y_label,
      data_points,
      annotations
    }
  }
  
  // 生成线图
  let line_chart = generate_line_chart(time_series_metrics, "响应时间趋势", "时间", "响应时间 (ms)")
  
  assert_eq(line_chart.title, "响应时间趋势")
  assert_eq(line_chart.x_axis_label, "时间")
  assert_eq(line_chart.y_axis_label, "响应时间 (ms)")
  assert_eq(line_chart.data_points.length(), time_series_metrics.length())
  assert_eq(line_chart.annotations.length(), 2)
  assert_true(line_chart.annotations[0].contains("上升"))
  assert_true(line_chart.annotations[1].contains("35.0"))  // 135.0 - 100.0 = 35.0
  
  // 生成柱状图
  let bar_chart = generate_bar_chart(category_metrics, "服务请求量", "服务", "请求数")
  
  assert_eq(bar_chart.title, "服务请求量")
  assert_eq(bar_chart.x_axis_label, "服务")
  assert_eq(bar_chart.y_axis_label, "请求数")
  assert_eq(bar_chart.data_points.length(), category_metrics.length())
  assert_eq(bar_chart.annotations.length(), 1)
  assert_true(bar_chart.annotations[0].contains("api"))
  assert_true(bar_chart.annotations[0].contains("450.0"))
  
  // 生成散点图
  let scatter_plot = generate_scatter_plot(correlation_metrics, "CPU与响应时间关系", "CPU使用率 (%)", "响应时间 (ms)")
  
  assert_eq(scatter_plot.title, "CPU与响应时间关系")
  assert_eq(scatter_plot.x_axis_label, "CPU使用率 (%)")
  assert_eq(scatter_plot.y_axis_label, "响应时间 (ms)")
  assert_eq(scatter_plot.data_points.length(), correlation_metrics.length())
  assert_eq(scatter_plot.annotations.length(), 1)
  assert_true(scatter_plot.annotations[0].contains("强正相关"))
  
  // 定义图表数据统计函数
  let analyze_chart_data = fn(chart: ChartData) {
    let y_values = chart.data_points.map(fn(p) { p.y })
    
    let min_value = y_values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, y_values[0])
    let max_value = y_values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, y_values[0])
    let mean_value = y_values.reduce(fn(acc, v) { acc + v }, 0.0) / y_values.length().to_float()
    
    let variance = y_values.reduce(fn(acc, v) { acc + (v - mean_value) * (v - mean_value) }, 0.0) / y_values.length().to_float()
    let std_deviation = if variance > 0.0 { variance.sqrt() } else { 0.0 }
    
    {
      data_point_count: chart.data_points.length(),
      min_value,
      max_value,
      mean_value,
      range: max_value - min_value,
      std_deviation,
      chart_type: chart.chart_type
    }
  }
  
  // 分析线图数据
  let line_chart_stats = analyze_chart_data(line_chart)
  
  assert_eq(line_chart_stats.data_point_count, 8)
  assert_eq(line_chart_stats.min_value, 100.0)
  assert_eq(line_chart_stats.max_value, 135.0)
  assert_eq(line_chart_stats.mean_value, 117.5)  // (100+105+110+115+120+125+130+135)/8
  assert_eq(line_chart_stats.range, 35.0)
  assert_eq(line_chart_stats.chart_type, ChartType::LineChart)
  
  // 分析柱状图数据
  let bar_chart_stats = analyze_chart_data(bar_chart)
  
  assert_eq(bar_chart_stats.data_point_count, 5)
  assert_eq(bar_chart_stats.min_value, 120.0)
  assert_eq(bar_chart_stats.max_value, 450.0)
  assert_eq(bar_chart_stats.mean_value, 244.0)  // (450+320+180+150+120)/5
  assert_eq(bar_chart_stats.range, 330.0)
  assert_eq(bar_chart_stats.chart_type, ChartType::BarChart)
  
  // 分析散点图数据
  let scatter_plot_stats = analyze_chart_data(scatter_plot)
  
  assert_eq(scatter_plot_stats.data_point_count, 8)
  assert_eq(scatter_plot_stats.min_value, 100.0)
  assert_eq(scatter_plot_stats.max_value, 200.0)
  assert_true(scatter_plot_stats.mean_value > 130.0)
  assert_eq(scatter_plot_stats.range, 100.0)
  assert_eq(scatter_plot_stats.chart_type, ChartType::ScatterPlot)
  
  // 定义图表比较函数
  let compare_charts = fn(chart1: ChartData, chart2: ChartData) {
    let stats1 = analyze_chart_data(chart1)
    let stats2 = analyze_chart_data(chart2)
    
    let comparison = {
      data_point_diff: stats1.data_point_count - stats2.data_point_count,
      mean_diff: stats1.mean_value - stats2.mean_value,
      range_diff: stats1.range - stats2.range,
      std_dev_diff: stats1.std_deviation - stats2.std_deviation
    }
    
    match (chart1.chart_type, chart2.chart_type) {
      (ChartType::LineChart, ChartType::LineChart) => "both_line_charts",
      (ChartType::BarChart, ChartType::BarChart) => "both_bar_charts",
      (ChartType::ScatterPlot, ChartType::ScatterPlot) => "both_scatter_plots",
      _ => "different_chart_types"
    }
  }
  
  // 比较不同图表
  let line_vs_bar = compare_charts(line_chart, bar_chart)
  let line_vs_scatter = compare_charts(line_chart, scatter_plot)
  
  assert_eq(line_vs_bar, "different_chart_types")
  assert_eq(line_vs_scatter, "different_chart_types")
  
  // 定义图表报告生成函数
  let generate_chart_report = fn(chart: ChartData, stats: { data_point_count: Int, min_value: Float, max_value: Float, mean_value: Float, range: Float, std_deviation: Float, chart_type: ChartType }) {
    let mut report = chart.title + " 图表报告\n"
    report = report + "================\n\n"
    
    report = report + "图表类型: "
    report = report + match chart.chart_type {
      ChartType::LineChart => "线图"
      ChartType::BarChart => "柱状图"
      ChartType::PieChart => "饼图"
      ChartType::ScatterPlot => "散点图"
      ChartType::Heatmap => "热力图"
    }
    report = report + "\n"
    
    report = report + "数据点数量: " + stats.data_point_count.to_string() + "\n"
    report = report + "最小值: " + stats.min_value.to_string() + "\n"
    report = report + "最大值: " + stats.max_value.to_string() + "\n"
    report = report + "平均值: " + stats.mean_value.to_string() + "\n"
    report = report + "值域: " + stats.range.to_string() + "\n"
    report = report + "标准差: " + stats.std_deviation.to_string() + "\n"
    
    report = report + "\n注释:\n"
    for annotation in chart.annotations {
      report = report + "- " + annotation + "\n"
    }
    
    report
  }
  
  // 生成图表报告
  let line_chart_report = generate_chart_report(line_chart, line_chart_stats)
  let bar_chart_report = generate_chart_report(bar_chart, bar_chart_stats)
  let scatter_plot_report = generate_chart_report(scatter_plot, scatter_plot_stats)
  
  // 验证报告内容
  assert_true(line_chart_report.contains("响应时间趋势 图表报告"))
  assert_true(line_chart_report.contains("线图"))
  assert_true(line_chart_report.contains("数据点数量: 8"))
  assert_true(line_chart_report.contains("趋势: 上升"))
  
  assert_true(bar_chart_report.contains("服务请求量 图表报告"))
  assert_true(bar_chart_report.contains("柱状图"))
  assert_true(bar_chart_report.contains("数据点数量: 5"))
  assert_true(bar_chart_report.contains("最大值: api (450.0)"))
  
  assert_true(scatter_plot_report.contains("CPU与响应时间关系 图表报告"))
  assert_true(scatter_plot_report.contains("散点图"))
  assert_true(scatter_plot_report.contains("数据点数量: 8"))
  assert_true(scatter_plot_report.contains("强正相关"))
}