// Azimuth 增强遥测功能测试用例
// 专注于遥测系统的高级功能和特性

// 测试1: 遥测数据采集
test "遥测数据采集和预处理" {
  // 定义遥测数据点结构
  type TelemetryPoint = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[String],
    source: String
  }
  
  // 创建测试数据点
  let data_points = [
    { timestamp: 1640995200, metric_name: "cpu.usage", value: 75.5, tags: ["service:api", "region:us-west"], source: "host-001" },
    { timestamp: 1640995260, metric_name: "memory.usage", value: 60.2, tags: ["service:api", "region:us-west"], source: "host-001" },
    { timestamp: 1640995320, metric_name: "response.time", value: 120.0, tags: ["service:web", "region:us-east"], source: "host-002" },
    { timestamp: 1640995380, metric_name: "error.rate", value: 0.5, tags: ["service:api", "region:us-west"], source: "host-001" },
    { timestamp: 1640995440, metric_name: "throughput", value: 1500.0, tags: ["service:web", "region:us-east"], source: "host-002" }
  ]
  
  // 测试数据采集
  assert_eq(data_points.length(), 5)
  
  // 测试数据过滤 - 按来源
  let host_001_data = data_points.filter(fn(point) { point.source == "host-001" })
  assert_eq(host_001_data.length(), 3)
  
  // 测试数据过滤 - 按标签
  let api_service_data = data_points.filter(fn(point) { 
    point.tags.contains("service:api") 
  })
  assert_eq(api_service_data.length(), 3)
  
  // 测试数据聚合 - 按指标名称
  let metric_groups = data_points.reduce(fn(acc, point) {
    let existing = acc.filter(fn(group) { group.metric_name == point.metric_name })
    if existing.length() > 0 {
      acc.map(fn(group) {
        if group.metric_name == point.metric_name {
          { group | count: group.count + 1, total_value: group.total_value + point.value }
        } else {
          group
        }
      })
    } else {
      acc.push({ metric_name: point.metric_name, count: 1, total_value: point.value })
    }
  }, [] as Array[{metric_name: String, count: Int, total_value: Float}])
  
  assert_eq(metric_groups.length(), 5)
  
  // 计算CPU使用率平均值
  let cpu_group = metric_groups.filter(fn(g) { g.metric_name == "cpu.usage" })
  assert_eq(cpu_group.length(), 1)
  assert_eq(cpu_group[0].count, 1)
  assert_eq(cpu_group[0].total_value, 75.5)
}

// 测试2: 遥测数据传输
test "遥测数据传输和序列化" {
  // 定义遥测数据包结构
  type TelemetryPacket = {
    packet_id: String,
    trace_id: String,
    data_points: Array[String],
    checksum: String,
    timestamp: Int
  }
  
  // 模拟数据序列化
  let serialize_data_point = fn(point: {name: String, value: Float, timestamp: Int}) {
    point.name + ":" + point.value.to_string() + ":" + point.timestamp.to_string()
  }
  
  // 创建测试数据点
  let raw_points = [
    { name: "cpu.usage", value: 75.5, timestamp: 1640995200 },
    { name: "memory.usage", value: 60.2, timestamp: 1640995260 },
    { name: "response.time", value: 120.0, timestamp: 1640995320 }
  ]
  
  // 序列化数据点
  let serialized_points = raw_points.map(serialize_data_point)
  assert_eq(serialized_points.length(), 3)
  assert_true(serialized_points[0].contains("cpu.usage:75.5"))
  
  // 创建数据包
  let packet = {
    packet_id: "packet-12345",
    trace_id: "trace-67890",
    data_points: serialized_points,
    checksum: "abc123",
    timestamp: 1640995400
  }
  
  // 测试数据包创建
  assert_eq(packet.packet_id, "packet-12345")
  assert_eq(packet.trace_id, "trace-67890")
  assert_eq(packet.data_points.length(), 3)
  
  // 模拟数据传输验证
  let validate_packet = fn(p: TelemetryPacket) {
    p.packet_id.length() > 0 && 
    p.trace_id.length() > 0 && 
    p.data_points.length() > 0 &&
    p.checksum.length() > 0
  }
  
  assert_true(validate_packet(packet))
  
  // 测试数据包分割（大包分片）
  let split_large_packet = fn(p: TelemetryPacket, max_size: Int) {
    if p.data_points.length() <= max_size {
      [p]
    } else {
      let mut chunks = []
      let mut remaining = p.data_points
      
      while remaining.length() > 0 {
        let chunk_size = if remaining.length() > max_size { max_size } else { remaining.length() }
        let chunk = remaining.slice(0, chunk_size)
        chunks = chunks.push({
          packet_id: p.packet_id + "-chunk-" + chunks.length().to_string(),
          trace_id: p.trace_id,
          data_points: chunk,
          checksum: p.checksum,
          timestamp: p.timestamp
        })
        remaining = remaining.slice(chunk_size, remaining.length())
      }
      
      chunks
    }
  }
  
  // 测试小包（不需要分割）
  let small_packets = split_large_packet(packet, 5)
  assert_eq(small_packets.length(), 1)
  
  // 测试大包分割
  let large_packet = { packet | data_points: serialized_points + serialized_points }
  let split_packets = split_large_packet(large_packet, 2)
  assert_eq(split_packets.length(), 3)
  assert_eq(split_packets[0].data_points.length(), 2)
  assert_eq(split_packets[1].data_points.length(), 2)
  assert_eq(split_packets[2].data_points.length(), 2)
}

// 测试3: 遥测数据存储
test "遥测数据存储和检索" {
  // 定义时间序列数据结构
  type TimeSeriesData = {
    metric_name: String,
    timestamps: Array[Int],
    values: Array[Float],
    tags: Array[String]
  }
  
  // 创建时间序列数据
  let cpu_series = {
    metric_name: "cpu.usage",
    timestamps: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440],
    values: [75.5, 78.2, 82.1, 79.8, 76.3],
    tags: ["service:api", "host:server-001"]
  }
  
  // 测试时间序列数据创建
  assert_eq(cpu_series.metric_name, "cpu.usage")
  assert_eq(cpu_series.timestamps.length(), 5)
  assert_eq(cpu_series.values.length(), 5)
  assert_eq(cpu_series.timestamps[0], 1640995200)
  assert_eq(cpu_series.values[0], 75.5)
  
  // 测试数据查询 - 按时间范围
  let query_by_time_range = fn(series: TimeSeriesData, start_time: Int, end_time: Int) {
    let mut result_timestamps = []
    let mut result_values = []
    
    for i in 0..series.timestamps.length() {
      if series.timestamps[i] >= start_time && series.timestamps[i] <= end_time {
        result_timestamps = result_timestamps.push(series.timestamps[i])
        result_values = result_values.push(series.values[i])
      }
    }
    
    { metric_name: series.metric_name, timestamps: result_timestamps, values: result_values, tags: series.tags }
  }
  
  // 查询前3个数据点
  let partial_data = query_by_time_range(cpu_series, 1640995200, 1640995320)
  assert_eq(partial_data.timestamps.length(), 3)
  assert_eq(partial_data.values.length(), 3)
  assert_eq(partial_data.timestamps[0], 1640995200)
  assert_eq(partial_data.values[0], 75.5)
  
  // 测试数据聚合 - 计算平均值
  let calculate_average = fn(series: TimeSeriesData) {
    if series.values.length() == 0 {
      0.0
    } else {
      series.values.reduce(fn(acc, val) { acc + val }, 0.0) / (series.values.length() as Float)
    }
  }
  
  let avg_cpu = calculate_average(cpu_series)
  assert_eq(avg_cpu.round(), 78.0)
  
  // 测试数据压缩 - 降采样
  let downsample = fn(series: TimeSeriesData, factor: Int) {
    let mut down_timestamps = []
    let mut down_values = []
    
    for i in 0..series.timestamps.length() {
      if i % factor == 0 {
        down_timestamps = down_timestamps.push(series.timestamps[i])
        down_values = down_values.push(series.values[i])
      }
    }
    
    { metric_name: series.metric_name, timestamps: down_timestamps, values: down_values, tags: series.tags }
  }
  
  // 每2个点采样1个
  let downsampled_data = downsample(cpu_series, 2)
  assert_eq(downsampled_data.timestamps.length(), 3)
  assert_eq(downsampled_data.values.length(), 3)
  assert_eq(downsampled_data.timestamps[0], 1640995200)
  assert_eq(downsampled_data.timestamps[1], 1640995320)
  assert_eq(downsampled_data.timestamps[2], 1640995440)
}

// 测试4: 遥测数据分析
test "遥测数据分析和统计" {
  // 定义分析结果结构
  type AnalysisResult = {
    metric_name: String,
    min: Float,
    max: Float,
    avg: Float,
    p50: Float,
    p95: Float,
    p99: Float,
    count: Int
  }
  
  // 创建测试数据集
  let response_times = [45.2, 120.5, 85.3, 200.1, 95.7, 150.8, 65.4, 180.9, 110.6, 75.3]
  
  // 测试基本统计计算
  let calculate_stats = fn(values: Array[Float]) {
    let count = values.length()
    let sum = values.reduce(fn(acc, val) { acc + val }, 0.0)
    let avg = sum / (count as Float)
    
    let sorted_values = values // 简化：假设已排序
    let min = sorted_values[0]
    let max = sorted_values[count - 1]
    
    // 简化的百分位计算
    let p50_index = (count * 50) / 100
    let p95_index = (count * 95) / 100
    let p99_index = (count * 99) / 100
    
    let p50 = sorted_values[p50_index]
    let p95 = sorted_values[p95_index]
    let p99 = sorted_values[p99_index]
    
    { min, max, avg, p50, p95, p99, count }
  }
  
  let stats = calculate_stats(response_times)
  assert_eq(stats.count, 10)
  assert_eq(stats.min, 45.2)
  assert_eq(stats.max, 200.1)
  assert_eq(stats.avg.round(), 123.0)
  
  // 测试异常值检测
  let detect_outliers = fn(values: Array[Float], threshold: Float) {
    let stats = calculate_stats(values)
    values.filter(fn(val) { 
      val > stats.avg + threshold || val < stats.avg - threshold 
    })
  }
  
  let outliers = detect_outliers(response_times, 50.0)
  assert_eq(outliers.length(), 2) // 200.1 和 180.9 可能是异常值
  
  // 测试趋势分析 - 简单线性趋势
  let calculate_trend = fn(values: Array[Float]) {
    if values.length() < 2 {
      0.0
    } else {
      let first_half = values.slice(0, values.length() / 2)
      let second_half = values.slice(values.length() / 2, values.length())
      
      let first_avg = first_half.reduce(fn(acc, val) { acc + val }, 0.0) / (first_half.length() as Float)
      let second_avg = second_half.reduce(fn(acc, val) { acc + val }, 0.0) / (second_half.length() as Float)
      
      second_avg - first_avg
    }
  }
  
  let trend = calculate_trend(response_times)
  assert_true(trend > 0.0) // 后半部分平均值更高，上升趋势
  
  // 创建完整分析结果
  let analysis_result = {
    metric_name: "response.time",
    min: stats.min,
    max: stats.max,
    avg: stats.avg,
    p50: stats.p50,
    p95: stats.p95,
    p99: stats.p99,
    count: stats.count
  }
  
  assert_eq(analysis_result.metric_name, "response.time")
  assert_eq(analysis_result.count, 10)
}

// 测试5: 遥测告警
test "遥测告警和阈值监控" {
  // 定义告警规则结构
  type AlertRule = {
    rule_id: String,
    metric_name: String,
    condition: String, // "gt", "lt", "eq"
    threshold: Float,
    duration: Int, // 持续时间（秒）
    severity: String // "critical", "warning", "info"
  }
  
  // 定义告警状态结构
  type AlertState = {
    rule_id: String,
    metric_name: String,
    current_value: Float,
    threshold: Float,
    triggered: Bool,
    trigger_time: Int,
    severity: String
  }
  
  // 创建告警规则
  let alert_rules = [
    { rule_id: "cpu-high", metric_name: "cpu.usage", condition: "gt", threshold: 80.0, duration: 300, severity: "warning" },
    { rule_id: "memory-critical", metric_name: "memory.usage", condition: "gt", threshold: 90.0, duration: 60, severity: "critical" },
    { rule_id: "error-rate", metric_name: "error.rate", condition: "gt", threshold: 1.0, duration: 120, severity: "warning" },
    { rule_id: "response-time", metric_name: "response.time", condition: "gt", threshold: 500.0, duration: 180, severity: "critical" }
  ]
  
  // 测试告警规则创建
  assert_eq(alert_rules.length(), 4)
  assert_eq(alert_rules[0].rule_id, "cpu-high")
  assert_eq(alert_rules[0].threshold, 80.0)
  assert_eq(alert_rules[0].severity, "warning")
  
  // 模拟当前指标值
  let current_metrics = [
    { name: "cpu.usage", value: 85.2, timestamp: 1640995400 },
    { name: "memory.usage", value: 75.5, timestamp: 1640995400 },
    { name: "error.rate", value: 1.5, timestamp: 1640995400 },
    { name: "response.time", value: 450.0, timestamp: 1640995400 }
  ]
  
  // 测试告警条件评估
  let evaluate_rule = fn(rule: AlertRule, metrics: Array[{name: String, value: Float, timestamp: Int}]) {
    let metric = metrics.find(fn(m) { m.name == rule.metric_name })
    
    match metric {
      Some(m) => {
        let triggered = match rule.condition {
          "gt" => m.value > rule.threshold,
          "lt" => m.value < rule.threshold,
          "eq" => m.value == rule.threshold,
          _ => false
        }
        
        {
          rule_id: rule.rule_id,
          metric_name: rule.metric_name,
          current_value: m.value,
          threshold: rule.threshold,
          triggered,
          trigger_time: m.timestamp,
          severity: rule.severity
        }
      }
      None => {
        {
          rule_id: rule.rule_id,
          metric_name: rule.metric_name,
          current_value: 0.0,
          threshold: rule.threshold,
          triggered: false,
          trigger_time: 0,
          severity: rule.severity
        }
      }
    }
  }
  
  // 评估所有告警规则
  let alert_states = alert_rules.map(fn(rule) { evaluate_rule(rule, current_metrics) })
  
  // 测试告警状态
  let cpu_alert = alert_states.find(fn(state) { state.rule_id == "cpu-high" })
  match cpu_alert {
    Some(alert) => {
      assert_true(alert.triggered)
      assert_eq(alert.current_value, 85.2)
      assert_eq(alert.threshold, 80.0)
      assert_eq(alert.severity, "warning")
    }
    None => assert_true(false)
  }
  
  let memory_alert = alert_states.find(fn(state) { state.rule_id == "memory-critical" })
  match memory_alert {
    Some(alert) => {
      assert_false(alert.triggered) // 75.5 < 90.0
      assert_eq(alert.current_value, 75.5)
    }
    None => assert_true(false)
  }
  
  let error_alert = alert_states.find(fn(state) { state.rule_id == "error-rate" })
  match error_alert {
    Some(alert) => {
      assert_true(alert.triggered) // 1.5 > 1.0
      assert_eq(alert.current_value, 1.5)
    }
    None => assert_true(false)
  }
  
  let response_alert = alert_states.find(fn(state) { state.rule_id == "response-time" })
  match response_alert {
    Some(alert) => {
      assert_false(alert.triggered) // 450.0 < 500.0
      assert_eq(alert.current_value, 450.0)
    }
    None => assert_true(false)
  }
  
  // 测试告警聚合
  let triggered_alerts = alert_states.filter(fn(state) { state.triggered })
  assert_eq(triggered_alerts.length(), 2) // cpu-high 和 error-rate
  
  let critical_alerts = triggered_alerts.filter(fn(state) { state.severity == "critical" })
  let warning_alerts = triggered_alerts.filter(fn(state) { state.severity == "warning" })
  
  assert_eq(critical_alerts.length(), 0)
  assert_eq(warning_alerts.length(), 2)
}

// 测试6: 遥测配置管理
test "遥测配置管理和动态更新" {
  // 定义配置结构
  type TelemetryConfig = {
    sampling_rate: Float,
    batch_size: Int,
    export_interval: Int,
    enabled_metrics: Array[String],
    tags: Array[(String, String)]
  }
  
  // 创建默认配置
  let default_config = {
    sampling_rate: 1.0,
    batch_size: 100,
    export_interval: 60,
    enabled_metrics: ["cpu.usage", "memory.usage", "response.time"],
    tags: [("service", "api"), ("version", "1.0.0")]
  }
  
  // 测试默认配置
  assert_eq(default_config.sampling_rate, 1.0)
  assert_eq(default_config.batch_size, 100)
  assert_eq(default_config.export_interval, 60)
  assert_eq(default_config.enabled_metrics.length(), 3)
  assert_eq(default_config.tags.length(), 2)
  
  // 测试配置更新
  let update_config = fn(config: TelemetryConfig, updates: {sampling_rate: Option[Float], batch_size: Option[Int], export_interval: Option[Int]}) {
    {
      sampling_rate: match updates.sampling_rate { Some(v) => v, None => config.sampling_rate },
      batch_size: match updates.batch_size { Some(v) => v, None => config.batch_size },
      export_interval: match updates.export_interval { Some(v) => v, None => config.export_interval },
      enabled_metrics: config.enabled_metrics,
      tags: config.tags
    }
  }
  
  // 更新部分配置
  let updated_config = update_config(default_config, {
    sampling_rate: Some(0.5),
    batch_size: None,
    export_interval: Some(30)
  })
  
  assert_eq(updated_config.sampling_rate, 0.5)
  assert_eq(updated_config.batch_size, 100) // 未更新
  assert_eq(updated_config.export_interval, 30)
  
  // 测试配置验证
  let validate_config = fn(config: TelemetryConfig) {
    config.sampling_rate > 0.0 && 
    config.sampling_rate <= 1.0 &&
    config.batch_size > 0 &&
    config.export_interval > 0 &&
    config.enabled_metrics.length() > 0
  }
  
  assert_true(validate_config(default_config))
  assert_true(validate_config(updated_config))
  
  // 测试无效配置
  let invalid_config = { default_config | sampling_rate: 1.5 }
  assert_false(validate_config(invalid_config))
  
  // 测试配置合并
  let merge_configs = fn(base: TelemetryConfig, override: TelemetryConfig) {
    {
      sampling_rate: override.sampling_rate,
      batch_size: override.batch_size,
      export_interval: override.export_interval,
      enabled_metrics: base.enabled_metrics + override.enabled_metrics,
      tags: base.tags + override.tags
    }
  }
  
  let override_config = {
    sampling_rate: 0.8,
    batch_size: 200,
    export_interval: 120,
    enabled_metrics: ["error.rate", "throughput"],
    tags: [("environment", "production")]
  }
  
  let merged_config = merge_configs(default_config, override_config)
  assert_eq(merged_config.sampling_rate, 0.8)
  assert_eq(merged_config.batch_size, 200)
  assert_eq(merged_config.export_interval, 120)
  assert_eq(merged_config.enabled_metrics.length(), 5)
  assert_eq(merged_config.tags.length(), 3)
  
  // 测试配置序列化
  let serialize_config = fn(config: TelemetryConfig) {
    "sampling_rate=" + config.sampling_rate.to_string() + 
    ",batch_size=" + config.batch_size.to_string() + 
    ",export_interval=" + config.export_interval.to_string()
  }
  
  let serialized = serialize_config(default_config)
  assert_true(serialized.contains("sampling_rate=1.0"))
  assert_true(serialized.contains("batch_size=100"))
  assert_true(serialized.contains("export_interval=60"))
}

// 测试7: 遥测性能监控
test "遥测系统性能监控" {
  // 定义性能指标结构
  type PerformanceMetrics = {
    cpu_usage: Float,
    memory_usage: Float,
    disk_io: Float,
    network_io: Float,
    processing_time: Float,
    throughput: Float
  }
  
  // 定义性能阈值结构
  type PerformanceThresholds = {
    cpu_warning: Float,
    cpu_critical: Float,
    memory_warning: Float,
    memory_critical: Float,
    processing_time_warning: Float,
    processing_time_critical: Float
  }
  
  // 创建性能阈值
  let thresholds = {
    cpu_warning: 70.0,
    cpu_critical: 90.0,
    memory_warning: 80.0,
    memory_critical: 95.0,
    processing_time_warning: 1000.0,
    processing_time_critical: 5000.0
  }
  
  // 创建当前性能指标
  let current_metrics = {
    cpu_usage: 75.5,
    memory_usage: 82.3,
    disk_io: 45.2,
    network_io: 60.8,
    processing_time: 1200.0,
    throughput: 1500.0
  }
  
  // 测试性能指标创建
  assert_eq(current_metrics.cpu_usage, 75.5)
  assert_eq(current_metrics.memory_usage, 82.3)
  assert_eq(current_metrics.processing_time, 1200.0)
  
  // 测试性能状态评估
  let evaluate_performance = fn(metrics: PerformanceMetrics, thresholds: PerformanceThresholds) {
    let cpu_status = if metrics.cpu_usage >= thresholds.cpu_critical {
      "critical"
    } else if metrics.cpu_usage >= thresholds.cpu_warning {
      "warning"
    } else {
      "normal"
    }
    
    let memory_status = if metrics.memory_usage >= thresholds.memory_critical {
      "critical"
    } else if metrics.memory_usage >= thresholds.memory_warning {
      "warning"
    } else {
      "normal"
    }
    
    let processing_status = if metrics.processing_time >= thresholds.processing_time_critical {
      "critical"
    } else if metrics.processing_time >= thresholds.processing_time_warning {
      "warning"
    } else {
      "normal"
    }
    
    let overall_status = if cpu_status == "critical" || memory_status == "critical" || processing_status == "critical" {
      "critical"
    } else if cpu_status == "warning" || memory_status == "warning" || processing_status == "warning" {
      "warning"
    } else {
      "normal"
    }
    
    {
      cpu_status,
      memory_status,
      processing_status,
      overall_status
    }
  }
  
  let performance_status = evaluate_performance(current_metrics, thresholds)
  assert_eq(performance_status.cpu_status, "warning") // 75.5 >= 70.0
  assert_eq(performance_status.memory_status, "warning") // 82.3 >= 80.0
  assert_eq(performance_status.processing_status, "warning") // 1200.0 >= 1000.0
  assert_eq(performance_status.overall_status, "warning")
  
  // 测试性能趋势分析
  let performance_history = [
    { timestamp: 1640995200, cpu: 65.2, memory: 75.3, processing_time: 800.0 },
    { timestamp: 1640995260, cpu: 68.5, memory: 78.1, processing_time: 950.0 },
    { timestamp: 1640995320, cpu: 72.3, memory: 80.5, processing_time: 1100.0 },
    { timestamp: 1640995380, cpu: 75.5, memory: 82.3, processing_time: 1200.0 },
    { timestamp: 1640995440, cpu: 78.2, memory: 84.1, processing_time: 1350.0 }
  ]
  
  // 计算CPU趋势
  let cpu_values = performance_history.map(fn(entry) { entry.cpu })
  let cpu_trend = if cpu_values.length() >= 2 {
    cpu_values[cpu_values.length() - 1] - cpu_values[0]
  } else {
    0.0
  }
  
  assert_true(cpu_trend > 0.0) // CPU使用率上升趋势
  assert_eq(cpu_trend.round(), 13.0) // 78.2 - 65.2 ≈ 13.0
  
  // 测试性能预测（简单线性外推）
  let predict_performance = fn(history: Array[{timestamp: Int, cpu: Float, memory: Float, processing_time: Float}]) {
    if history.length() < 2 {
      { cpu_prediction: 0.0, memory_prediction: 0.0, processing_time_prediction: 0.0 }
    } else {
      let recent = history.slice(history.length() - 2, history.length())
      let cpu_change = recent[1].cpu - recent[0].cpu
      let memory_change = recent[1].memory - recent[0].memory
      let processing_change = recent[1].processing_time - recent[0].processing_time
      
      {
        cpu_prediction: recent[1].cpu + cpu_change,
        memory_prediction: recent[1].memory + memory_change,
        processing_time_prediction: recent[1].processing_time + processing_change
      }
    }
  }
  
  let prediction = predict_performance(performance_history)
  assert_eq(prediction.cpu_prediction.round(), 81.0) // 78.2 + (78.2 - 75.5) ≈ 81.0
  assert_eq(prediction.memory_prediction.round(), 86.0) // 84.1 + (84.1 - 82.3) ≈ 86.0
  assert_eq(prediction.processing_time_prediction.round(), 1500.0) // 1350.0 + (1350.0 - 1200.0) = 1500.0
  
  // 测试性能优化建议
  let generate_optimization_suggestions = fn(metrics: PerformanceMetrics, thresholds: PerformanceThresholds) {
    let mut suggestions = []
    
    if metrics.cpu_usage >= thresholds.cpu_warning {
      suggestions = suggestions.push("Consider scaling horizontally or optimizing CPU-intensive operations")
    }
    
    if metrics.memory_usage >= thresholds.memory_warning {
      suggestions = suggestions.push("Review memory usage patterns and consider memory optimization")
    }
    
    if metrics.processing_time >= thresholds.processing_time_warning {
      suggestions = suggestions.push("Optimize processing pipelines and consider batching strategies")
    }
    
    suggestions
  }
  
  let suggestions = generate_optimization_suggestions(current_metrics, thresholds)
  assert_eq(suggestions.length(), 3) // 所有指标都超过警告阈值
  assert_true(suggestions[0].contains("scaling"))
  assert_true(suggestions[1].contains("memory"))
  assert_true(suggestions[2].contains("batching"))
}

// 测试8: 遥测数据可视化
test "遥测数据可视化和图表生成" {
  // 定义图表数据点结构
  type ChartPoint = {
    x: Float, // X轴值（通常是时间）
    y: Float, // Y轴值（指标值）
    label: String // 可选标签
  }
  
  // 定义图表配置结构
  type ChartConfig = {
    title: String,
    x_axis_label: String,
    y_axis_label: String,
    x_min: Float,
    x_max: Float,
    y_min: Float,
    y_max: Float,
    color: String
  }
  
  // 创建测试数据点
  let chart_data = [
    { x: 0.0, y: 75.5, label: "00:00" },
    { x: 1.0, y: 78.2, label: "00:01" },
    { x: 2.0, y: 82.1, label: "00:02" },
    { x: 3.0, y: 79.8, label: "00:03" },
    { x: 4.0, y: 76.3, label: "00:04" },
    { x: 5.0, y: 80.5, label: "00:05" }
  ]
  
  // 测试图表数据创建
  assert_eq(chart_data.length(), 6)
  assert_eq(chart_data[0].x, 0.0)
  assert_eq(chart_data[0].y, 75.5)
  assert_eq(chart_data[0].label, "00:00")
  
  // 测试图表配置创建
  let chart_config = {
    title: "CPU Usage Over Time",
    x_axis_label: "Time (minutes)",
    y_axis_label: "CPU Usage (%)",
    x_min: 0.0,
    x_max: 6.0,
    y_min: 0.0,
    y_max: 100.0,
    color: "#FF5733"
  }
  
  assert_eq(chart_config.title, "CPU Usage Over Time")
  assert_eq(chart_config.x_axis_label, "Time (minutes)")
  assert_eq(chart_config.y_axis_label, "CPU Usage (%)")
  assert_eq(chart_config.y_min, 0.0)
  assert_eq(chart_config.y_max, 100.0)
  
  // 测试数据范围计算
  let calculate_data_range = fn(data: Array[ChartPoint]) {
    if data.length() == 0 {
      { x_min: 0.0, x_max: 0.0, y_min: 0.0, y_max: 0.0 }
    } else {
      let mut x_min = data[0].x
      let mut x_max = data[0].x
      let mut y_min = data[0].y
      let mut y_max = data[0].y
      
      for point in data {
        if point.x < x_min { x_min = point.x }
        if point.x > x_max { x_max = point.x }
        if point.y < y_min { y_min = point.y }
        if point.y > y_max { y_max = point.y }
      }
      
      { x_min, x_max, y_min, y_max }
    }
  }
  
  let data_range = calculate_data_range(chart_data)
  assert_eq(data_range.x_min, 0.0)
  assert_eq(data_range.x_max, 5.0)
  assert_eq(data_range.y_min, 75.5)
  assert_eq(data_range.y_max, 82.1)
  
  // 测试自动缩放配置
  let auto_scale_config = fn(config: ChartConfig, data_range: {x_min: Float, x_max: Float, y_min: Float, y_max: Float}) {
    let x_padding = (data_range.x_max - data_range.x_min) * 0.1
    let y_padding = (data_range.y_max - data_range.y_min) * 0.1
    
    {
      title: config.title,
      x_axis_label: config.x_axis_label,
      y_axis_label: config.y_axis_label,
      x_min: data_range.x_min - x_padding,
      x_max: data_range.x_max + x_padding,
      y_min: data_range.y_min - y_padding,
      y_max: data_range.y_max + y_padding,
      color: config.color
    }
  }
  
  let scaled_config = auto_scale_config(chart_config, data_range)
  assert_true(scaled_config.x_min < 0.0) // 添加了填充
  assert_true(scaled_config.x_max > 5.0) // 添加了填充
  assert_true(scaled_config.y_min < 75.5) // 添加了填充
  assert_true(scaled_config.y_max > 82.1) // 添加了填充
  
  // 测试数据聚合（用于降低数据点密度）
  let aggregate_data = fn(data: Array[ChartPoint], bucket_size: Int) {
    if data.length() == 0 {
      []
    } else {
      let mut buckets = []
      let mut current_bucket = []
      let bucket_count = (data.length() as Int + bucket_size - 1) / bucket_size
      
      for i in 0..bucket_count {
        let start_index = i * bucket_size
        let end_index = if (i + 1) * bucket_size < data.length() { (i + 1) * bucket_size } else { data.length() }
        let bucket = data.slice(start_index, end_index)
        
        if bucket.length() > 0 {
          let avg_x = bucket.reduce(fn(acc, p) { acc + p.x }, 0.0) / (bucket.length() as Float)
          let avg_y = bucket.reduce(fn(acc, p) { acc + p.y }, 0.0) / (bucket.length() as Float)
          let first_label = bucket[0].label
          
          buckets = buckets.push({ x: avg_x, y: avg_y, label: first_label })
        }
      }
      
      buckets
    }
  }
  
  // 每3个数据点聚合为1个
  let aggregated_data = aggregate_data(chart_data, 3)
  assert_eq(aggregated_data.length(), 2) // 6个点分成2个桶
  
  // 测试第一个聚合点
  assert_eq(aggregated_data[0].x.round(), 1.0) // (0.0 + 1.0 + 2.0) / 3 = 1.0
  assert_eq(aggregated_data[0].y.round(), 78.0) // (75.5 + 78.2 + 82.1) / 3 ≈ 78.0
  assert_eq(aggregated_data[0].label, "00:00") // 使用第一个标签
  
  // 测试图表数据序列化（用于前端渲染）
  let serialize_chart_data = fn(data: Array[ChartPoint], config: ChartConfig) {
    let data_string = data.map(fn(point) {
      "(" + point.x.to_string() + "," + point.y.to_string() + ")"
    }).join(",")
    
    "chart:" + config.title + "|" + 
    "config:" + config.x_axis_label + "," + config.y_axis_label + "|" +
    "data:" + data_string
  }
  
  let serialized_chart = serialize_chart_data(chart_data, chart_config)
  assert_true(serialized_chart.contains("chart:CPU Usage Over Time"))
  assert_true(serialized_chart.contains("config:Time (minutes),CPU Usage (%)"))
  assert_true(serialized_chart.contains("data:(0.0,75.5)"))
  assert_true(serialized_chart.contains("(1.0,78.2)"))
}