// Azimuth 新增MoonBit测试用例
// 包含遥测系统相关的新测试场景

// 测试1: 遥测数据编码与解码
test "telemetry data encoding and decoding" {
  // 模拟Base64编码过程
  let encode_base64 = fn(input: String) {
    let chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    let result = "encoded:" + input.length().to_string() + ":" + input
    result
  }
  
  let decode_base64 = fn(encoded: String) {
    if encoded.starts_with("encoded:") {
      let parts = encoded.split(":")
      if parts.length() >= 3 {
        parts[2]  // 返回原始字符串
      } else {
        ""
      }
    } else {
      encoded
    }
  }
  
  let telemetry_data = "trace_id=12345:span_id=67890:status=success"
  let encoded = encode_base64(telemetry_data)
  let decoded = decode_base64(encoded)
  
  assert_eq(encoded, "encoded:38:trace_id=12345:span_id=67890:status=success")
  assert_eq(decoded, telemetry_data)
  assert_true(encoded.contains("encoded:"))
  assert_true(decoded.contains("trace_id"))
}

// 测试2: 遥测数据压缩与解压
test "telemetry data compression and decompression" {
  // 模拟压缩过程（移除重复字符）
  let compress = fn(input: String) {
    let mut result = ""
    let mut prev_char = '\0'
    
    for i in 0..input.length() {
      let current_char = input[i]
      if current_char != prev_char {
        result = result + current_char.to_string()
        prev_char = current_char
      }
    }
    
    "compressed:" + result
  }
  
  let decompress = fn(compressed: String) {
    if compressed.starts_with("compressed:") {
      let data = compressed.substring(11, compressed.length() - 11)
      // 简化的解压：每个字符重复2次
      let mut result = ""
      for i in 0..data.length() {
        let c = data[i]
        result = result + c.to_string() + c.to_string()
      }
      result
    } else {
      compressed
    }
  }
  
  let redundant_data = "tracee_iiiddd=112233"
  let compressed = compress(redundant_data)
  let decompressed = decompress(compressed)
  
  assert_eq(compressed, "compressed:trace_id=123")
  assert_eq(decompressed, "ttrraaccee_iidd=112233")
  assert_true(compressed.length() < redundant_data.length())
}

// 测试3: 遥测采样策略
test "telemetry sampling strategies" {
  // 定义采样策略枚举
  enum SamplingStrategy {
    Always
    Never
    Probabilistic(Float)
    RateLimited(Int)  // 每秒采样数量
  }
  
  // 采样决策函数
  let should_sample = fn(strategy: SamplingStrategy, trace_id: String) {
    match strategy {
      SamplingStrategy::Always => true,
      SamplingStrategy::Never => false,
      SamplingStrategy::Probabilistic(probability) => {
        // 简化的概率采样：基于trace_id的哈希
        let hash = trace_id.length() % 100
        (hash.to_float() / 100.0) < probability
      },
      SamplingStrategy::RateLimited(rate) => {
        // 简化的速率限制：基于trace_id的奇偶性
        trace_id.length() % rate == 0
      }
    }
  }
  
  // 测试各种采样策略
  let always_strategy = SamplingStrategy::Always
  let never_strategy = SamplingStrategy::Never
  let probabilistic_strategy = SamplingStrategy::Probabilistic(0.5)
  let rate_limited_strategy = SamplingStrategy::RateLimited(3)
  
  let test_trace_id = "trace-12345"
  
  assert_true(should_sample(always_strategy, test_trace_id))
  assert_false(should_sample(never_strategy, test_trace_id))
  
  // 对于概率采样，结果取决于trace_id的长度
  let prob_result = should_sample(probabilistic_strategy, test_trace_id)
  
  // 对于速率限制采样，结果取决于trace_id长度是否能被3整除
  let rate_result = should_sample(rate_limited_strategy, test_trace_id)
  assert_eq(rate_result, test_trace_id.length() % 3 == 0)
  
  // 测试不同的trace_id
  let short_trace = "abc"
  let long_trace = "abcdefghijklmnopqrstuvwxyz"
  
  let short_prob_result = should_sample(probabilistic_strategy, short_trace)
  let long_prob_result = should_sample(probabilistic_strategy, long_trace)
  
  let short_rate_result = should_sample(rate_limited_strategy, short_trace)
  let long_rate_result = should_sample(rate_limited_strategy, long_trace)
  
  assert_eq(short_rate_result, short_trace.length() % 3 == 0)
  assert_eq(long_rate_result, long_trace.length() % 3 == 0)
}

// 测试4: 遥测数据聚合窗口
test "telemetry data aggregation windows" {
  // 定义时间窗口结构
  type TimeWindow = {
    start_time: Int,
    end_time: Int,
    metrics: Array[(String, Float)]
  }
  
  // 创建时间窗口
  let create_window = fn(start: Int, duration: Int) {
    {
      start_time: start,
      end_time: start + duration,
      metrics: []
    }
  }
  
  // 添加指标到窗口
  let add_metric = fn(window: TimeWindow, name: String, value: Float) {
    { window | 
      metrics: window.metrics + [(name, value)] 
    }
  }
  
  // 计算窗口内指标的平均值
  let window_average = fn(window: TimeWindow, metric_name: String) {
    let mut sum = 0.0
    let mut count = 0
    
    for (name, value) in window.metrics {
      if name == metric_name {
        sum = sum + value
        count = count + 1
      }
    }
    
    if count > 0 {
      sum / count.to_float()
    } else {
      0.0
    }
  }
  
  // 测试时间窗口操作
  let window1 = create_window(1640995200, 60)  // 1分钟窗口
  let window2 = create_window(1640995260, 60)  // 下一个1分钟窗口
  
  // 添加指标到第一个窗口
  let window1_with_metrics = window1
    |> add_metric("cpu", 25.5)
    |> add_metric("memory", 45.2)
    |> add_metric("cpu", 30.1)
    |> add_metric("network", 10.0)
  
  // 添加指标到第二个窗口
  let window2_with_metrics = window2
    |> add_metric("cpu", 35.7)
    |> add_metric("memory", 50.3)
    |> add_metric("network", 15.5)
  
  // 计算平均值
  let window1_cpu_avg = window_average(window1_with_metrics, "cpu")
  let window1_memory_avg = window_average(window1_with_metrics, "memory")
  let window1_network_avg = window_average(window1_with_metrics, "network")
  
  let window2_cpu_avg = window_average(window2_with_metrics, "cpu")
  let window2_memory_avg = window_average(window2_with_metrics, "memory")
  let window2_network_avg = window_average(window2_with_metrics, "network")
  
  assert_eq(window1_cpu_avg, (25.5 + 30.1) / 2.0)
  assert_eq(window1_memory_avg, 45.2)
  assert_eq(window1_network_avg, 10.0)
  
  assert_eq(window2_cpu_avg, 35.7)
  assert_eq(window2_memory_avg, 50.3)
  assert_eq(window2_network_avg, 15.5)
  
  // 测试窗口重叠检查
  let windows_overlap = fn(w1: TimeWindow, w2: TimeWindow) {
    not(w1.end_time <= w2.start_time or w2.end_time <= w1.start_time)
  }
  
  assert_false(windows_overlap(window1, window2))  // 不重叠
  assert_true(windows_overlap(window1, { window1 | end_time: window1.start_time + 30 }))  // 重叠
  
  // 测试窗口合并
  let merge_windows = fn(w1: TimeWindow, w2: TimeWindow) {
    {
      start_time: if w1.start_time < w2.start_time { w1.start_time } else { w2.start_time },
      end_time: if w1.end_time > w2.end_time { w1.end_time } else { w2.end_time },
      metrics: w1.metrics + w2.metrics
    }
  }
  
  let merged_window = merge_windows(window1_with_metrics, window2_with_metrics)
  assert_eq(merged_window.start_time, window1.start_time)
  assert_eq(merged_window.end_time, window2.end_time)
  assert_eq(merged_window.metrics.length(), 7)  // 4 + 3
}

// 测试5: 遥测数据相关性分析
test "telemetry data correlation analysis" {
  // 定义数据点结构
  type DataPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float
  }
  
  // 创建测试数据点
  let data_points = [
    { timestamp: 1000, metric_name: "cpu", value: 20.0 },
    { timestamp: 1000, metric_name: "memory", value: 40.0 },
    { timestamp: 2000, metric_name: "cpu", value: 30.0 },
    { timestamp: 2000, metric_name: "memory", value: 50.0 },
    { timestamp: 3000, metric_name: "cpu", value: 40.0 },
    { timestamp: 3000, metric_name: "memory", value: 60.0 },
    { timestamp: 4000, metric_name: "cpu", value: 35.0 },
    { timestamp: 4000, metric_name: "memory", value: 55.0 },
    { timestamp: 5000, metric_name: "cpu", value: 25.0 },
    { timestamp: 5000, metric_name: "memory", value: 45.0 }
  ]
  
  // 按时间戳和指标名称查找数据点
  let find_data_point = fn(points: Array[DataPoint], timestamp: Int, metric_name: String) {
    let mut result = None
    for point in points {
      if point.timestamp == timestamp and point.metric_name == metric_name {
        result = Some(point)
        break
      }
    }
    result
  }
  
  // 计算两个指标之间的简单相关性
  let calculate_correlation = fn(points: Array[DataPoint], metric1: String, metric2: String) {
    let mut pairs = []
    
    // 收集相同时间戳的数据对
    for point in points {
      if point.metric_name == metric1 {
        match find_data_point(points, point.timestamp, metric2) {
          Some(matching_point) => {
            pairs = pairs + [(point.value, matching_point.value)]
          }
          None => ()
        }
      }
    }
    
    // 简化的相关性计算：如果值同向变化则为正相关，否则为负相关
    if pairs.length() < 2 {
      0.0
    } else {
      let mut positive_changes = 0
      let mut negative_changes = 0
      
      for i in 1..pairs.length() {
        let (prev_x, prev_y) = pairs[i - 1]
        let (curr_x, curr_y) = pairs[i]
        
        let x_change = curr_x - prev_x
        let y_change = curr_y - prev_y
        
        if (x_change > 0.0 and y_change > 0.0) or (x_change < 0.0 and y_change < 0.0) {
          positive_changes = positive_changes + 1
        } else if x_change != 0.0 and y_change != 0.0 {
          negative_changes = negative_changes + 1
        }
      }
      
      let total_changes = positive_changes + negative_changes
      if total_changes > 0 {
        positive_changes.to_float() / total_changes.to_float()
      } else {
        0.0
      }
    }
  }
  
  // 测试数据点查找
  let cpu_point_2000 = find_data_point(data_points, 2000, "cpu")
  let memory_point_3000 = find_data_point(data_points, 3000, "memory")
  let nonexistent_point = find_data_point(data_points, 9999, "network")
  
  match cpu_point_2000 {
    Some(point) => {
      assert_eq(point.timestamp, 2000)
      assert_eq(point.metric_name, "cpu")
      assert_eq(point.value, 30.0)
    }
    None => assert_true(false)
  }
  
  match memory_point_3000 {
    Some(point) => {
      assert_eq(point.timestamp, 3000)
      assert_eq(point.metric_name, "memory")
      assert_eq(point.value, 60.0)
    }
    None => assert_true(false)
  }
  
  match nonexistent_point {
    Some(_) => assert_true(false)
    None => assert_true(true)
  }
  
  // 测试相关性计算
  let cpu_memory_correlation = calculate_correlation(data_points, "cpu", "memory")
  
  // 根据测试数据，CPU和内存使用率应该有正相关性
  assert_true(cpu_memory_correlation > 0.5)  // 强正相关
  
  // 添加一些负相关的测试数据
  let inverse_data_points = [
    { timestamp: 1000, metric_name: "throughput", value: 100.0 },
    { timestamp: 1000, metric_name: "latency", value: 10.0 },
    { timestamp: 2000, metric_name: "throughput", value: 80.0 },
    { timestamp: 2000, metric_name: "latency", value: 20.0 },
    { timestamp: 3000, metric_name: "throughput", value: 60.0 },
    { timestamp: 3000, metric_name: "latency", value: 30.0 },
    { timestamp: 4000, metric_name: "throughput", value: 90.0 },
    { timestamp: 4000, metric_name: "latency", value: 15.0 },
    { timestamp: 5000, metric_name: "throughput", value: 70.0 },
    { timestamp: 5000, metric_name: "latency", value: 25.0 }
  ]
  
  let throughput_latency_correlation = calculate_correlation(inverse_data_points, "throughput", "latency")
  
  // 根据测试数据，吞吐量和延迟应该有负相关性
  assert_true(throughput_latency_correlation < 0.5)  // 负相关
}

// 测试6: 遥测数据异常检测
test "telemetry data anomaly detection" {
  // 定义异常类型
  enum AnomalyType {
    StatisticalOutlier
    SuddenSpike
    SuddenDrop
    PatternViolation
  }
  
  // 定义异常检测结果
  type AnomalyDetectionResult = {
    is_anomaly: Bool,
    anomaly_type: Option[AnomalyType],
    confidence: Float,
    details: String
  }
  
  // 简单的统计异常检测
  let detect_statistical_outlier = fn(values: Array[Float], threshold: Float) {
    if values.length() < 3 {
      {
        is_anomaly: false,
        anomaly_type: None,
        confidence: 0.0,
        details: "Insufficient data for statistical analysis"
      }
    } else {
      // 计算平均值
      let sum = values.reduce(fn(acc, x) { acc + x }, 0.0)
      let mean = sum / values.length().to_float()
      
      // 计算标准差
      let variance_sum = values.reduce(fn(acc, x) {
        let diff = x - mean
        acc + (diff * diff)
      }, 0.0)
      let std_dev = (variance_sum / values.length().to_float()).sqrt()
      
      // 检查最后一个值是否为异常值
      let last_value = values[values.length() - 1]
      let z_score = (last_value - mean) / std_dev
      
      let is_outlier = z_score.abs() > threshold
      
      {
        is_anomaly: is_outlier,
        anomaly_type: if is_outlier { Some(AnomalyType::StatisticalOutlier) } else { None },
        confidence: if is_outlier { z_score.abs() / threshold } else { 0.0 },
        details: if is_outlier {
          "Value " + last_value.to_string() + " is " + z_score.abs().to_string() + " standard deviations from mean"
        } else {
          "No statistical anomaly detected"
        }
      }
    }
  }
  
  // 突变检测
  let detect_sudden_change = fn(values: Array[Float], change_threshold: Float) {
    if values.length() < 2 {
      {
        is_anomaly: false,
        anomaly_type: None,
        confidence: 0.0,
        details: "Insufficient data for change detection"
      }
    } else {
      let last_value = values[values.length() - 1]
      let prev_value = values[values.length() - 2]
      
      if prev_value == 0.0 {
        {
          is_anomaly: false,
          anomaly_type: None,
          confidence: 0.0,
          details: "Cannot calculate relative change from zero"
        }
      } else {
        let relative_change = (last_value - prev_value).abs() / prev_value.abs()
        let is_anomaly = relative_change > change_threshold
        
        let anomaly_type = if is_anomaly {
          if last_value > prev_value {
            Some(AnomalyType::SuddenSpike)
          } else {
            Some(AnomalyType::SuddenDrop)
          }
        } else {
          None
        }
        
        {
          is_anomaly,
          anomaly_type,
          confidence: if is_anomaly { relative_change / change_threshold } else { 0.0 },
          details: if is_anomaly {
            let change_type = if last_value > prev_value { "spike" } else { "drop" }
            "Sudden " + change_type + " detected: " + (relative_change * 100.0).to_string() + "% change"
          } else {
            "No sudden change detected"
          }
        }
      }
    }
  }
  
  // 测试统计异常检测
  let normal_values = [10.0, 12.0, 11.0, 13.0, 9.0, 14.0, 8.0, 15.0, 10.0, 12.0]
  let outlier_values = [10.0, 12.0, 11.0, 13.0, 9.0, 14.0, 8.0, 15.0, 10.0, 50.0]  // 最后一个值是异常值
  
  let normal_result = detect_statistical_outlier(normal_values, 2.0)
  let outlier_result = detect_statistical_outlier(outlier_values, 2.0)
  
  assert_false(normal_result.is_anomaly)
  assert_eq(normal_result.anomaly_type, None)
  assert_eq(normal_result.confidence, 0.0)
  
  assert_true(outlier_result.is_anomaly)
  match outlier_result.anomaly_type {
    Some(AnomalyType::StatisticalOutlier) => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(outlier_result.confidence > 1.0)
  assert_true(outlier_result.details.contains("standard deviations"))
  
  // 测试突变检测
  let stable_values = [10.0, 11.0, 12.0, 13.0, 14.0]
  let spike_values = [10.0, 11.0, 12.0, 13.0, 50.0]  // 突然增加
  let drop_values = [50.0, 45.0, 40.0, 35.0, 5.0]    // 突然减少
  
  let stable_result = detect_sudden_change(stable_values, 0.5)
  let spike_result = detect_sudden_change(spike_values, 0.5)
  let drop_result = detect_sudden_change(drop_values, 0.5)
  
  assert_false(stable_result.is_anomaly)
  assert_eq(stable_result.anomaly_type, None)
  
  assert_true(spike_result.is_anomaly)
  match spike_result.anomaly_type {
    Some(AnomalyType::SuddenSpike) => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(spike_result.details.contains("spike"))
  
  assert_true(drop_result.is_anomaly)
  match drop_result.anomaly_type {
    Some(AnomalyType::SuddenDrop) => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(drop_result.details.contains("drop"))
}

// 测试7: 遥测数据路由与分发
test "telemetry data routing and distribution" {
  // 定义路由规则
  type RoutingRule = {
    condition: (String, String) -> Bool,  // (key, value) -> Bool
    destination: String
  }
  
  // 定义路由目标
  enum RoutingDestination {
    Database(String)
    MessageQueue(String)
    LogFile(String)
    AlertSystem(String)
  }
  
  // 创建路由规则
  let create_error_rule = fn() {
    {
      condition: fn(key, value) { key == "level" and value == "error" },
      destination: "alert_system"
    }
  }
  
  let create_metric_rule = fn() {
    {
      condition: fn(key, value) { key.starts_with("metric.") },
      destination: "database"
    }
  }
  
  let create_trace_rule = fn() {
    {
      condition: fn(key, value) { key == "type" and value == "trace" },
      destination: "message_queue"
    }
  }
  
  // 路由遥测数据
  let route_telemetry_data = fn(attributes: Array[(String, String)], rules: Array[RoutingRule]) {
    let mut destinations = []
    
    for rule in rules {
      let mut matches = false
      
      for (key, value) in attributes {
        if rule.condition(key, value) {
          matches = true
          break
        }
      }
      
      if matches {
        destinations = destinations + [rule.destination]
      }
    }
    
    destinations
  }
  
  // 测试路由规则
  let error_rule = create_error_rule()
  let metric_rule = create_metric_rule()
  let trace_rule = create_trace_rule()
  
  let rules = [error_rule, metric_rule, trace_rule]
  
  // 测试错误日志路由
  let error_attributes = [
    ("level", "error"),
    ("message", "Database connection failed"),
    ("timestamp", "1640995200")
  ]
  
  let error_destinations = route_telemetry_data(error_attributes, rules)
  assert_eq(error_destinations.length(), 1)
  assert_true(error_destinations.contains("alert_system"))
  
  // 测试指标路由
  let metric_attributes = [
    ("metric.name", "cpu_usage"),
    ("metric.value", "75.5"),
    ("metric.unit", "percent")
  ]
  
  let metric_destinations = route_telemetry_data(metric_attributes, rules)
  assert_eq(metric_destinations.length(), 1)
  assert_true(metric_destinations.contains("database"))
  
  // 测试跟踪路由
  let trace_attributes = [
    ("type", "trace"),
    ("trace_id", "trace-12345"),
    ("span_id", "span-67890")
  ]
  
  let trace_destinations = route_telemetry_data(trace_attributes, rules)
  assert_eq(trace_destinations.length(), 1)
  assert_true(trace_destinations.contains("message_queue"))
  
  // 测试多匹配路由
  let multi_match_attributes = [
    ("level", "error"),
    ("metric.name", "error_count"),
    ("metric.value", "5")
  ]
  
  let multi_match_destinations = route_telemetry_data(multi_match_attributes, rules)
  assert_eq(multi_match_destinations.length(), 2)
  assert_true(multi_match_destinations.contains("alert_system"))
  assert_true(multi_match_destinations.contains("database"))
  
  // 测试无匹配路由
  let no_match_attributes = [
    ("level", "info"),
    ("message", "Application started"),
    ("timestamp", "1640995200")
  ]
  
  let no_match_destinations = route_telemetry_data(no_match_attributes, rules)
  assert_eq(no_match_destinations.length(), 0)
  
  // 测试条件组合
  let create_complex_rule = fn() {
    {
      condition: fn(key, value) {
        (key == "level" and (value == "error" or value == "critical")) or
        (key.starts_with("metric.") and value.to_float() > 90.0)
      },
      destination: "alert_system"
    }
  }
  
  let complex_rule = create_complex_rule()
  let complex_rules = [complex_rule, metric_rule, trace_rule]
  
  // 测试严重错误
  let critical_attributes = [
    ("level", "critical"),
    ("message", "System crash"),
    ("timestamp", "1640995200")
  ]
  
  let critical_destinations = route_telemetry_data(critical_attributes, complex_rules)
  assert_eq(critical_destinations.length(), 1)
  assert_true(critical_destinations.contains("alert_system"))
  
  // 测试高指标值
  let high_metric_attributes = [
    ("metric.name", "cpu_usage"),
    ("metric.value", "95.5"),
    ("metric.unit", "percent")
  ]
  
  let high_metric_destinations = route_telemetry_data(high_metric_attributes, complex_rules)
  assert_eq(high_metric_destinations.length(), 2)
  assert_true(high_metric_destinations.contains("alert_system"))
  assert_true(high_metric_destinations.contains("database"))
}

// 测试8: 遥测数据生命周期管理
test "telemetry data lifecycle management" {
  // 定义数据生命周期状态
  enum DataLifecycleState {
    Active
    WarmStorage
    ColdStorage
    Archived
    Deleted
  }
  
  // 定义遥测数据记录
  type TelemetryDataRecord = {
    id: String,
    data: String,
    created_at: Int,
    last_accessed: Int,
    access_count: Int,
    state: DataLifecycleState,
    size_bytes: Int
  }
  
  // 创建新的遥测数据记录
  let create_record = fn(id: String, data: String, current_time: Int) {
    {
      id,
      data,
      created_at: current_time,
      last_accessed: current_time,
      access_count: 0,
      state: DataLifecycleState::Active,
      size_bytes: data.length()
    }
  }
  
  // 访问记录
  let access_record = fn(record: TelemetryDataRecord, current_time: Int) {
    { record |
      last_accessed: current_time,
      access_count: record.access_count + 1
    }
  }
  
  // 更新记录状态
  let update_state = fn(record: TelemetryDataRecord, new_state: DataLifecycleState) {
    { record | state: new_state }
  }
  
  // 计算记录年龄（秒）
  let calculate_age = fn(record: TelemetryDataRecord, current_time: Int) {
    current_time - record.created_at
  }
  
  // 计算自上次访问以来的时间（秒）
  let calculate_idle_time = fn(record: TelemetryDataRecord, current_time: Int) {
    current_time - record.last_accessed
  }
  
  // 生命周期管理策略
  let apply_lifecycle_policy = fn(record: TelemetryDataRecord, current_time: Int) {
    let age = calculate_age(record, current_time)
    let idle_time = calculate_idle_time(record, current_time)
    
    match record.state {
      DataLifecycleState::Active => {
        if age > 86400 {  // 1天后移动到温存储
          update_state(record, DataLifecycleState::WarmStorage)
        } else {
          record
        }
      }
      DataLifecycleState::WarmStorage => {
        if age > 604800 {  // 7天后移动到冷存储
          update_state(record, DataLifecycleState::ColdStorage)
        } else if idle_time < 3600 and record.access_count > 10 {  // 如果最近有频繁访问，移回活跃状态
          update_state(record, DataLifecycleState::Active)
        } else {
          record
        }
      }
      DataLifecycleState::ColdStorage => {
        if age > 2592000 {  // 30天后归档
          update_state(record, DataLifecycleState::Archived)
        } else {
          record
        }
      }
      DataLifecycleState::Archived => {
        if age > 7776000 {  // 90天后删除
          update_state(record, DataLifecycleState::Deleted)
        } else {
          record
        }
      }
      DataLifecycleState::Deleted => record  // 已删除的记录保持不变
    }
  }
  
  // 测试记录创建
  let current_time = 1640995200
  let record1 = create_record("record-1", "sample telemetry data", current_time)
  
  assert_eq(record1.id, "record-1")
  assert_eq(record1.data, "sample telemetry data")
  assert_eq(record1.created_at, current_time)
  assert_eq(record1.last_accessed, current_time)
  assert_eq(record1.access_count, 0)
  match record1.state {
    DataLifecycleState::Active => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(record1.size_bytes, 20)
  
  // 测试记录访问
  let access_time = current_time + 3600  // 1小时后
  let accessed_record = access_record(record1, access_time)
  
  assert_eq(accessed_record.last_accessed, access_time)
  assert_eq(accessed_record.access_count, 1)
  
  // 测试状态更新
  let warm_record = update_state(accessed_record, DataLifecycleState::WarmStorage)
  match warm_record.state {
    DataLifecycleState::WarmStorage => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试年龄计算
  let future_time = current_time + 7200  // 2小时后
  let age = calculate_age(record1, future_time)
  assert_eq(age, 7200)
  
  // 测试空闲时间计算
  let idle_time = calculate_idle_time(accessed_record, future_time)
  assert_eq(idle_time, 3600)  // 2小时 - 1小时 = 1小时
  
  // 测试生命周期策略应用
  let old_active_time = current_time + 90000  // 超过1天
  let old_active_record = { record1 | created_at: current_time - 90000 }
  let policy_result1 = apply_lifecycle_policy(old_active_record, old_active_time)
  match policy_result1.state {
    DataLifecycleState::WarmStorage => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试从温存储回到活跃状态
  let frequently_accessed_warm = { warm_record | 
    access_count: 15,
    last_accessed: current_time + 100
  }
  let policy_result2 = apply_lifecycle_policy(frequently_accessed_warm, current_time + 200)
  match policy_result2.state {
    DataLifecycleState::Active => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试从温存储到冷存储
  let old_warm_record = { warm_record | created_at: current_time - 700000 }
  let policy_result3 = apply_lifecycle_policy(old_warm_record, current_time)
  match policy_result3.state {
    DataLifecycleState::ColdStorage => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试从冷存储到归档
  let old_cold_record = { policy_result3 | created_at: current_time - 3000000 }
  let policy_result4 = apply_lifecycle_policy(old_cold_record, current_time)
  match policy_result4.state {
    DataLifecycleState::Archived => assert_true(true)
    _ => assert_true(false)
  }
  
  // 测试从归档到删除
  let old_archived_record = { policy_result4 | created_at: current_time - 8000000 }
  let policy_result5 = apply_lifecycle_policy(old_archived_record, current_time)
  match policy_result5.state {
    DataLifecycleState::Deleted => assert_true(true)
    _ => assert_true(false)
  }
}

// 测试9: 遥测数据隐私保护
test "telemetry data privacy protection" {
  // 定义敏感数据类型
  enum SensitiveDataType {
    PersonalIdentifier
    Credential
    FinancialInformation
    HealthInformation
    Custom(String)
  }
  
  // 定义隐私保护策略
  type PrivacyPolicy = {
    sensitive_patterns: Array[String],
    protection_method: String,  // "redact", "mask", "hash", "encrypt"
    custom_rules: Array[(String, SensitiveDataType)]
  }
  
  // 创建隐私策略
  let create_default_policy = fn() {
    {
      sensitive_patterns: [
        "email",
        "password",
        "ssn",
        "credit_card",
        "api_key",
        "token",
        "secret"
      ],
      protection_method: "redact",
      custom_rules: [
        ("user_id", SensitiveDataType::PersonalIdentifier),
        ("phone", SensitiveDataType::PersonalIdentifier)
      ]
    }
  }
  
  // 检查字符串是否包含敏感模式
  let contains_sensitive_pattern = fn(text: String, patterns: Array[String]) {
    let mut found = false
    
    for pattern in patterns {
      if text.contains(pattern) {
        found = true
        break
      }
    }
    
    found
  }
  
  // 应用隐私保护
  let apply_privacy_protection = fn(data: String, policy: PrivacyPolicy) {
    if contains_sensitive_pattern(data, policy.sensitive_patterns) {
      match policy.protection_method {
        "redact" => "[REDACTED]",
        "mask" => {
          if data.length() <= 4 {
            "[MASKED]"
          } else {
            data.substring(0, 2) + "*".repeat(data.length() - 4) + data.substring(data.length() - 2, 2)
          }
        }
        "hash" => {
          // 简化的哈希：返回字符串长度和首尾字符
          "[HASH:" + data.length().to_string() + ":" + data[0].to_string() + data[data.length() - 1].to_string() + "]"
        }
        _ => "[PROTECTED]"
      }
    } else {
      data
    }
  }
  
  // 保护属性键值对
  let protect_attributes = fn(attributes: Array[(String, String)], policy: PrivacyPolicy) {
    let mut protected = []
    
    for (key, value) in attributes {
      let is_key_sensitive = contains_sensitive_pattern(key, policy.sensitive_patterns)
      let is_value_sensitive = contains_sensitive_pattern(value, policy.sensitive_patterns)
      
      // 检查自定义规则
      let mut is_custom_sensitive = false
      for (custom_key, _) in policy.custom_rules {
        if key == custom_key {
          is_custom_sensitive = true
          break
        }
      }
      
      if is_key_sensitive or is_value_sensitive or is_custom_sensitive {
        protected = protected + [(key, apply_privacy_protection(value, policy))]
      } else {
        protected = protected + [(key, value)]
      }
    }
    
    protected
  }
  
  // 测试敏感模式检测
  let policy = create_default_policy()
  
  assert_true(contains_sensitive_pattern("user_email", policy.sensitive_patterns))
  assert_true(contains_sensitive_pattern("api_key_value", policy.sensitive_patterns))
  assert_false(contains_sensitive_pattern("user_name", policy.sensitive_patterns))
  assert_false(contains_sensitive_pattern("request_id", policy.sensitive_patterns))
  
  // 测试隐私保护方法
  let sensitive_data1 = "user@example.com"
  let sensitive_data2 = "1234567890"
  let sensitive_data3 = "secret_token"
  let normal_data = "application_started"
  
  let redacted_policy = { policy | protection_method: "redact" }
  let masked_policy = { policy | protection_method: "mask" }
  let hashed_policy = { policy | protection_method: "hash" }
  
  assert_eq(apply_privacy_protection(sensitive_data1, redacted_policy), "[REDACTED]")
  assert_eq(apply_privacy_protection(normal_data, redacted_policy), "application_started")
  
  assert_eq(apply_privacy_protection(sensitive_data2, masked_policy), "12******90")
  assert_eq(apply_privacy_protection("abc", masked_policy), "[MASKED]")
  assert_eq(apply_privacy_protection(normal_data, masked_policy), "application_started")
  
  assert_eq(apply_privacy_protection(sensitive_data3, hashed_policy), "[HASH:12:st]")
  assert_eq(apply_privacy_protection(normal_data, hashed_policy), "application_started")
  
  // 测试属性保护
  let sensitive_attributes = [
    ("user_id", "12345"),
    ("email", "user@example.com"),
    ("username", "john_doe"),
    ("request_id", "req-12345"),
    ("api_key", "sk-1234567890"),
    ("operation", "database_query")
  ]
  
  let protected_attributes = protect_attributes(sensitive_attributes, redacted_policy)
  
  assert_eq(protected_attributes.length(), 6)
  
  // 检查敏感属性是否被保护
  let user_id_value = protected_attributes.filter(fn(attr) { match attr { (key, _) => key == "user_id" } })[0].1
  let email_value = protected_attributes.filter(fn(attr) { match attr { (key, _) => key == "email" } })[0].1
  let username_value = protected_attributes.filter(fn(attr) { match attr { (key, _) => key == "username" } })[0].1
  let request_id_value = protected_attributes.filter(fn(attr) { match attr { (key, _) => key == "request_id" } })[0].1
  let api_key_value = protected_attributes.filter(fn(attr) { match attr { (key, _) => key == "api_key" } })[0].1
  let operation_value = protected_attributes.filter(fn(attr) { match attr { (key, _) => key == "operation" } })[0].1
  
  assert_eq(user_id_value, "[REDACTED]")  // 自定义规则
  assert_eq(email_value, "[REDACTED]")   // 敏感模式
  assert_eq(username_value, "john_doe")  // 非敏感
  assert_eq(request_id_value, "req-12345")  // 非敏感
  assert_eq(api_key_value, "[REDACTED]")   // 敏感模式
  assert_eq(operation_value, "database_query")  // 非敏感
  
  // 测试自定义保护策略
  let custom_policy = {
    sensitive_patterns: ["custom_field"],
    protection_method: "mask",
    custom_rules: [
      ("special_id", SensitiveDataType::Custom("special_identifier"))
    ]
  }
  
  let custom_attributes = [
    ("custom_field", "sensitive_value"),
    ("special_id", "special-12345"),
    ("normal_field", "normal_value")
  ]
  
  let custom_protected = protect_attributes(custom_attributes, custom_policy)
  
  let custom_field_value = custom_protected.filter(fn(attr) { match attr { (key, _) => key == "custom_field" } })[0].1
  let special_id_value = custom_protected.filter(fn(attr) { match attr { (key, _) => key == "special_id" } })[0].1
  let normal_field_value = custom_protected.filter(fn(attr) { match attr { (key, _) => key == "normal_field" } })[0].1
  
  assert_eq(custom_field_value, "se***********ue")  // 敏感模式 + 掩码
  assert_eq(special_id_value, "sp************45")  // 自定义规则 + 掩码
  assert_eq(normal_field_value, "normal_value")    // 非敏感
}