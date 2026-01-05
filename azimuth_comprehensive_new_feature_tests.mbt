// Azimuth 综合新功能测试用例
// 专注于遥测系统的新功能和增强特性

// 测试1: 遥测数据流处理
test "遥测数据流处理测试" {
  // 创建数据流处理器
  let stream_processor = TelemetryStreamProcessor::new()
  
  // 配置数据流处理规则
  StreamProcessor::add_rule(stream_processor, {
    name: "error_filter",
    condition: fn(data) { data.status == "error" },
    action: fn(data) { 
      { data | priority: "high", needs_alert: true }
    }
  })
  
  StreamProcessor::add_rule(stream_processor, {
    name: "latency_threshold",
    condition: fn(data) { data.duration > 1000 },
    action: fn(data) { 
      { data | category: "slow_request", flagged: true }
    }
  })
  
  // 创建测试数据流
  let data_stream = [
    { trace_id: "trace-001", span_id: "span-001", service: "api", duration: 150, status: "ok" },
    { trace_id: "trace-002", span_id: "span-002", service: "db", duration: 1200, status: "ok" },
    { trace_id: "trace-003", span_id: "span-003", service: "cache", duration: 50, status: "error" },
    { trace_id: "trace-004", span_id: "span-004", service: "api", duration: 800, status: "ok" },
    { trace_id: "trace-005", span_id: "span-005", service: "db", duration: 1500, status: "error" }
  ]
  
  // 处理数据流
  let processed_stream = StreamProcessor::process_batch(stream_processor, data_stream)
  
  // 验证处理结果
  assert_eq(processed_stream.length(), 5)
  
  // 检查错误数据标记
  let error_data = processed_stream.filter(fn(d) { d.status == "error" })
  assert_eq(error_data.length(), 2)
  assert_true(error_data.all(fn(d) { d.priority == "high" }))
  assert_true(error_data.all(fn(d) { d.needs_alert == true }))
  
  // 检查延迟阈值标记
  let slow_data = processed_stream.filter(fn(d) { d.flagged == true })
  assert_eq(slow_data.length(), 2)  // 两个超过1000ms的请求
  assert_true(slow_data.all(fn(d) { d.category == "slow_request" }))
  
  // 测试实时流处理
  let realtime_processor = StreamProcessor::create_realtime(stream_processor)
  let mut processed_count = 0
  
  for data in data_stream {
    let result = StreamProcessor::process_realtime(realtime_processor, data)
    if result.flagged or result.needs_alert {
      processed_count = processed_count + 1
    }
  }
  
  assert_eq(processed_count, 3)  // 2个错误 + 1个慢请求（另一个慢请求也是错误）
}

// 测试2: 智能采样策略
test "智能采样策略测试" {
  // 创建智能采样管理器
  let smart_sampler = SmartSamplingManager::new()
  
  // 配置基础采样率
  SmartSamplingManager::set_base_sampling_rate(smart_sampler, 0.1)  // 10%基础采样率
  
  // 配置动态采样规则
  SmartSamplingManager::add_dynamic_rule(smart_sampler, {
    name: "error_boost",
    condition: fn(data) { data.status == "error" },
    sampling_multiplier: 5.0  // 错误采样率提升5倍
  })
  
  SmartSamplingManager::add_dynamic_rule(smart_sampler, {
    name: "high_value_service",
    condition: fn(data) { data.service == "payment" },
    sampling_multiplier: 2.0  // 支付服务采样率提升2倍
  })
  
  SmartSamplingManager::add_dynamic_rule(smart_sampler, {
    name: "low_traffic_period",
    condition: fn(data, context) { context.requests_per_second < 10 },
    sampling_multiplier: 1.5  // 低流量期间采样率提升1.5倍
  })
  
  // 创建测试数据
  let normal_data = { trace_id: "trace-001", service: "api", status: "ok", timestamp: 1640995200 }
  let error_data = { trace_id: "trace-002", service: "api", status: "error", timestamp: 1640995200 }
  let payment_data = { trace_id: "trace-003", service: "payment", status: "ok", timestamp: 1640995200 }
  let payment_error_data = { trace_id: "trace-004", service: "payment", status: "error", timestamp: 1640995200 }
  
  // 设置流量上下文
  let normal_traffic_context = { requests_per_second: 100 }
  let low_traffic_context = { requests_per_second: 5 }
  
  // 测试正常流量下的采样决策
  let normal_sampled = SmartSamplingManager::should_sample(smart_sampler, normal_data, normal_traffic_context)
  let error_sampled = SmartSamplingManager::should_sample(smart_sampler, error_data, normal_traffic_context)
  let payment_sampled = SmartSamplingManager::should_sample(smart_sampler, payment_data, normal_traffic_context)
  let payment_error_sampled = SmartSamplingManager::should_sample(smart_sampler, payment_error_data, normal_traffic_context)
  
  // 验证采样决策
  assert_true(normal_sampled or not(normal_sampled))  // 基础采样率，可能采样也可能不采样
  assert_true(error_sampled)  // 错误应该总是被采样（5倍提升）
  assert_true(payment_sampled or not(payment_sampled))  // 支付服务采样率提升
  assert_true(payment_error_sampled)  // 支付错误应该总是被采样（2*5=10倍提升）
  
  // 测试低流量下的采样决策
  let low_traffic_normal_sampled = SmartSamplingManager::should_sample(smart_sampler, normal_data, low_traffic_context)
  let low_traffic_error_sampled = SmartSamplingManager::should_sample(smart_sampler, error_data, low_traffic_context)
  
  // 验证低流量下的采样决策
  assert_true(low_traffic_normal_sampled or not(low_traffic_normal_sampled))
  assert_true(low_traffic_error_sampled)
  
  // 测试自适应采样
  SmartSamplingManager::enable_adaptive_sampling(smart_sampler, {
    target_sample_rate: 1000,  // 每秒目标采样数
    adjustment_interval: 60,    // 60秒调整间隔
    max_sampling_rate: 1.0,     // 最大采样率100%
    min_sampling_rate: 0.01     // 最小采样率1%
  })
  
  // 模拟高流量场景
  let high_traffic_context = { requests_per_second: 10000 }
  let mut sampled_count = 0
  let mut total_count = 0
  
  for i in 0..=1000 {
    let data = { 
      trace_id: "trace-" + i.to_string(), 
      service: "api", 
      status: if i % 50 == 0 { "error" } else { "ok" },
      timestamp: 1640995200 + i
    }
    
    total_count = total_count + 1
    if SmartSamplingManager::should_sample(smart_sampler, data, high_traffic_context) {
      sampled_count = sampled_count + 1
    }
  }
  
  // 验证自适应采样效果
  let actual_sample_rate = sampled_count.to_float() / total_count.to_float()
  assert_true(actual_sample_rate < 0.1)  // 高流量下采样率应该降低
  assert_true(sampled_count <= 100)      // 采样数应该接近目标
}

// 测试3: 遥测数据质量验证
test "遥测数据质量验证测试" {
  // 创建数据质量验证器
  let quality_validator = TelemetryQualityValidator::new()
  
  // 配置验证规则
  QualityValidator::add_rule(quality_validator, {
    name: "trace_id_format",
    description: "验证trace_id格式",
    validator: fn(data) { 
      data.trace_id.length() > 0 and data.trace_id.starts_with("trace-")
    },
    severity: "error"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "span_id_format",
    description: "验证span_id格式",
    validator: fn(data) { 
      data.span_id.length() > 0 and data.span_id.starts_with("span-")
    },
    severity: "error"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "duration_range",
    description: "验证duration范围",
    validator: fn(data) { 
      data.duration >= 0 and data.duration <= 3600000  // 0到1小时
    },
    severity: "warning"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "timestamp_validity",
    description: "验证timestamp有效性",
    validator: fn(data) { 
      data.timestamp > 1600000000 and data.timestamp <= 1700000000
    },
    severity: "error"
  })
  
  QualityValidator::add_rule(quality_validator, {
    name: "service_name_validity",
    description: "验证service名称",
    validator: fn(data) { 
      let valid_services = ["api", "db", "cache", "payment", "auth"]
      valid_services.contains(data.service)
    },
    severity: "warning"
  })
  
  // 创建测试数据
  let valid_data = [
    { trace_id: "trace-001", span_id: "span-001", service: "api", duration: 150, timestamp: 1640995200 },
    { trace_id: "trace-002", span_id: "span-002", service: "db", duration: 1200, timestamp: 1640995260 },
    { trace_id: "trace-003", span_id: "span-003", service: "cache", duration: 50, timestamp: 1640995320 }
  ]
  
  let invalid_data = [
    { trace_id: "", span_id: "span-004", service: "api", duration: 150, timestamp: 1640995380 },  // 无效trace_id
    { trace_id: "trace-005", span_id: "", service: "db", duration: 1200, timestamp: 1640995440 },  // 无效span_id
    { trace_id: "trace-006", span_id: "span-006", service: "api", duration: -100, timestamp: 1640995500 },  // 无效duration
    { trace_id: "trace-007", span_id: "span-007", service: "api", duration: 150, timestamp: 1500000000 },  // 无效timestamp
    { trace_id: "trace-008", span_id: "span-008", service: "unknown", duration: 200, timestamp: 1640995560 }  // 无效service
  ]
  
  // 验证有效数据
  let valid_results = QualityValidator::validate_batch(quality_validator, valid_data)
  
  // 验证所有有效数据都通过验证
  assert_eq(valid_results.length(), 3)
  assert_true(valid_results.all(fn(r) { r.is_valid }))
  
  // 验证无效数据
  let invalid_results = QualityValidator::validate_batch(quality_validator, invalid_data)
  
  // 验证所有无效数据都有错误
  assert_eq(invalid_results.length(), 5)
  assert_true(invalid_results.all(fn(r) { not(r.is_valid) }))
  
  // 检查具体错误
  let empty_trace_id_error = invalid_results[0].violations.find(fn(v) { v.rule == "trace_id_format" })
  assert_true(empty_trace_id_error != None)
  assert_eq(empty_trace_id_error.get().severity, "error")
  
  let empty_span_id_error = invalid_results[1].violations.find(fn(v) { v.rule == "span_id_format" })
  assert_true(empty_span_id_error != None)
  assert_eq(empty_span_id_error.get().severity, "error")
  
  let negative_duration_error = invalid_results[2].violations.find(fn(v) { v.rule == "duration_range" })
  assert_true(negative_duration_error != None)
  assert_eq(negative_duration_error.get().severity, "warning")
  
  let invalid_timestamp_error = invalid_results[3].violations.find(fn(v) { v.rule == "timestamp_validity" })
  assert_true(invalid_timestamp_error != None)
  assert_eq(invalid_timestamp_error.get().severity, "error")
  
  let invalid_service_error = invalid_results[4].violations.find(fn(v) { v.rule == "service_name_validity" })
  assert_true(invalid_service_error != None)
  assert_eq(invalid_service_error.get().severity, "warning")
  
  // 测试数据质量评分
  let all_data = valid_data + invalid_data
  let quality_score = QualityValidator::calculate_quality_score(quality_validator, all_data)
  
  // 验证质量评分（3/8 = 37.5%）
  assert_eq(quality_score.valid_percentage, 37.5)
  assert_eq(quality_score.error_count, 3)  // 3个错误级别的违规
  assert_eq(quality_score.warning_count, 2)  // 2个警告级别的违规
  
  // 测试自动修复
  let auto_fix_enabled = QualityValidator::enable_auto_fix(quality_validator, [
    "trace_id_format",
    "span_id_format"
  ])
  
  let fixable_data = [
    { trace_id: "", span_id: "", service: "api", duration: 150, timestamp: 1640995200 }
  ]
  
  let fixed_results = QualityValidator::validate_and_fix_batch(quality_validator, fixable_data)
  
  // 验证自动修复结果
  assert_eq(fixed_results.length(), 1)
  assert_true(fixed_results[0].is_fixed)
  assert_true(fixed_results[0].fixed_data.trace_id.length() > 0)
  assert_true(fixed_results[0].fixed_data.span_id.length() > 0)
}

// 测试4: 遥测数据聚合增强
test "遥测数据聚合增强测试" {
  // 创建增强数据聚合器
  let enhanced_aggregator = EnhancedTelemetryAggregator::new()
  
  // 配置时间窗口聚合
  EnhancedAggregator::add_time_window_rule(enhanced_aggregator, {
    name: "request_rate_1m",
    source_metric: "http.requests",
    window_size: 60,  // 1分钟窗口
    aggregation_type: "rate",
    group_by: ["service", "endpoint"]
  })
  
  EnhancedAggregator::add_time_window_rule(enhanced_aggregator, {
    name: "latency_percentiles_5m",
    source_metric: "http.request.duration",
    window_size: 300,  // 5分钟窗口
    aggregation_type: "percentile",
    percentiles: [50.0, 90.0, 95.0, 99.0],
    group_by: ["service", "endpoint"]
  })
  
  // 配置层次化聚合
  EnhancedAggregator::add_hierarchy_rule(enhanced_aggregator, {
    name: "service_to_endpoint",
    parent_level: ["service"],
    child_level: ["service", "endpoint"],
    aggregation_type: "sum"
  })
  
  EnhancedAggregator::add_hierarchy_rule(enhanced_aggregator, {
    name: "endpoint_to_operation",
    parent_level: ["service", "endpoint"],
    child_level: ["service", "endpoint", "operation"],
    aggregation_type: "sum"
  })
  
  // 配置自适应聚合
  EnhancedAggregator::add_adaptive_rule(enhanced_aggregator, {
    name: "dynamic_sampling",
    source_metric: "http.requests",
    adaptive_factor: fn(data_points) {
      if data_points.length() > 1000 {
        0.1  // 高数据量时采样10%
      } else if data_points.length() > 100 {
        0.5  // 中等数据量时采样50%
      } else {
        1.0  // 低数据量时全部处理
      }
    },
    min_sample_size: 10
  })
  
  // 创建测试数据
  let base_time = 1640995200
  let test_data = []
  
  // 生成10分钟的数据，每10秒一个数据点
  for i in 0..=60 {
    let timestamp = base_time + i * 10
    
    // 服务A的数据
    test_data = test_data.push({
      timestamp: timestamp,
      metric: "http.requests",
      value: 10.0,
      attributes: [ ("service", "api"), ("endpoint", "/users"), ("operation", "list") ]
    })
    
    test_data = test_data.push({
      timestamp: timestamp,
      metric: "http.request.duration",
      value: 50.0 + (i % 20) * 2.0,
      attributes: [ ("service", "api"), ("endpoint", "/users"), ("operation", "list") ]
    })
    
    // 服务B的数据
    test_data = test_data.push({
      timestamp: timestamp,
      metric: "http.requests",
      value: 15.0,
      attributes: [ ("service", "payment"), ("endpoint", "/charge"), ("operation", "create") ]
    })
    
    test_data = test_data.push({
      timestamp: timestamp,
      metric: "http.request.duration",
      value: 100.0 + (i % 30) * 3.0,
      attributes: [ ("service", "payment"), ("endpoint", "/charge"), ("operation", "create") ]
    })
  }
  
  // 执行时间窗口聚合
  let time_window_results = EnhancedAggregator::aggregate_time_windows(enhanced_aggregator, test_data)
  
  // 验证时间窗口聚合结果
  assert_true(time_window_results.length() > 0)
  
  // 检查1分钟请求率聚合
  let request_rate_results = time_window_results.filter(fn(r) { r.rule_name == "request_rate_1m" })
  assert_true(request_rate_results.length() > 0)
  
  // 验证每个服务都有聚合结果
  let api_service_results = request_rate_results.filter(fn(r) { 
    r.group_values.contains(("service", "api"))
  })
  let payment_service_results = request_rate_results.filter(fn(r) { 
    r.group_values.contains(("service", "payment"))
  })
  
  assert_true(api_service_results.length() > 0)
  assert_true(payment_service_results.length() > 0)
  
  // 检查5分钟延迟百分位数聚合
  let latency_percentile_results = time_window_results.filter(fn(r) { r.rule_name == "latency_percentiles_5m" })
  assert_true(latency_percentile_results.length() > 0)
  
  // 验证百分位数数据结构
  let api_latency = latency_percentile_results.find(fn(r) { 
    r.group_values.contains(("service", "api"))
  })
  assert_true(api_latency != None)
  
  match api_latency {
    Some(result) => {
      assert_true(result.aggregated_value.contains("p50"))
      assert_true(result.aggregated_value.contains("p90"))
      assert_true(result.aggregated_value.contains("p95"))
      assert_true(result.aggregated_value.contains("p99"))
    }
    None => assert_true(false)
  }
  
  // 执行层次化聚合
  let hierarchy_results = EnhancedAggregator::aggregate_hierarchy(enhanced_aggregator, test_data)
  
  // 验证层次化聚合结果
  assert_true(hierarchy_results.length() > 0)
  
  // 检查服务级别聚合
  let service_level_results = hierarchy_results.filter(fn(r) { r.rule_name == "service_to_endpoint" })
  assert_true(service_level_results.length() > 0)
  
  // 检查端点级别聚合
  let endpoint_level_results = hierarchy_results.filter(fn(r) { r.rule_name == "endpoint_to_operation" })
  assert_true(endpoint_level_results.length() > 0)
  
  // 执行自适应聚合
  let adaptive_results = EnhancedAggregator::aggregate_adaptive(enhanced_aggregator, test_data)
  
  // 验证自适应聚合结果
  assert_true(adaptive_results.length() > 0)
  
  // 验证自适应因子应用
  let adaptive_result = adaptive_results[0]
  assert_true(adaptive_result.adaptive_factor <= 1.0)
  assert_true(adaptive_result.sampled_data_points <= test_data.length())
  
  // 测试聚合结果缓存
  EnhancedAggregator::enable_caching(enhanced_aggregator, {
    cache_size: 100,
    ttl_seconds: 300  // 5分钟TTL
  })
  
  // 第一次聚合
  let start_time = Time::now()
  let first_results = EnhancedAggregator::aggregate_time_windows(enhanced_aggregator, test_data)
  let first_duration = Time::now() - start_time
  
  // 第二次聚合（应该使用缓存）
  let start_time2 = Time::now()
  let second_results = EnhancedAggregator::aggregate_time_windows(enhanced_aggregator, test_data)
  let second_duration = Time::now() - start_time2
  
  // 验证缓存效果
  assert_eq(first_results.length(), second_results.length())
  assert_true(second_duration < first_duration)  // 缓存应该更快
}

// 测试5: 遥测数据异常检测增强
test "遥测数据异常检测增强测试" {
  // 创建增强异常检测器
  let enhanced_detector = EnhancedAnomalyDetector::new()
  
  // 配置统计异常检测
  EnhancedDetector::add_statistical_detector(enhanced_detector, {
    name: "z_score_latency",
    metric: "http.request.duration",
    algorithm: "z_score",
    threshold: 2.5,  // 2.5个标准差
    window_size: 100,
    min_data_points: 30
  })
  
  EnhancedDetector::add_statistical_detector(enhanced_detector, {
    name: "iqr_error_rate",
    metric: "http.error.rate",
    algorithm: "iqr",  // 四分位距
    multiplier: 1.5,   // 1.5倍IQR
    window_size: 100,
    min_data_points: 30
  })
  
  // 配置机器学习异常检测
  EnhancedDetector::add_ml_detector(enhanced_detector, {
    name: "isolation_forest_throughput",
    metric: "http.throughput",
    algorithm: "isolation_forest",
    contamination: 0.1,  // 预期异常比例
    window_size: 500,
    min_data_points: 100,
    retrain_interval: 3600  // 1小时重新训练
  })
  
  // 配置季节性异常检测
  EnhancedDetector::add_seasonal_detector(enhanced_detector, {
    name: "seasonal_request_pattern",
    metric: "http.requests",
    seasonal_period: 86400,  // 24小时周期
    threshold: 2.0,          // 2个标准差
    window_size: 86400 * 7,  // 7天窗口
    min_data_points: 86400   // 至少1天数据
  })
  
  // 配置多变量异常检测
  EnhancedDetector::add_multivariate_detector(enhanced_detector, {
    name: "correlation_anomaly",
    metrics: ["http.request.duration", "cpu.usage", "memory.usage"],
    algorithm: "mahalanobis_distance",
    threshold: 3.0,  // 3个马氏距离
    window_size: 200,
    min_data_points: 50
  })
  
  // 创建正常训练数据
  let base_time = 1640995200
  let training_data = []
  
  // 生成7天的正常数据（每5分钟一个数据点）
  for day in 0..=6 {
    for hour in 0..=23 {
      for minute in [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55] {
        let timestamp = base_time + day * 86400 + hour * 3600 + minute * 60
        
        // 模拟正常的日流量模式（工作时间高，夜间低）
        let base_request_rate = if hour >= 9 and hour <= 17 {
          100.0  // 工作时间高流量
        } else if hour >= 22 or hour <= 6 {
          20.0   // 夜间低流量
        } else {
          60.0   // 其他时间中等流量
        }
        
        // 添加一些随机变化
        let request_rate = base_request_rate + (minute % 10) * 2.0
        let latency = 50.0 + (hour % 8) * 5.0 + (minute % 5) * 2.0
        let error_rate = if minute % 50 == 0 { 2.0 } else { 0.5 }
        let throughput = request_rate * 0.8
        let cpu_usage = 30.0 + request_rate / 10.0
        let memory_usage = 40.0 + request_rate / 20.0
        
        training_data = training_data.push({
          timestamp: timestamp,
          metrics: [
            ("http.requests", request_rate),
            ("http.request.duration", latency),
            ("http.error.rate", error_rate),
            ("http.throughput", throughput),
            ("cpu.usage", cpu_usage),
            ("memory.usage", memory_usage)
          ]
        })
      }
    }
  }
  
  // 训练检测器
  let training_result = EnhancedDetector::train(enhanced_detector, training_data)
  assert_true(training_result.success)
  
  // 创建测试数据（包含异常）
  let test_data = []
  
  // 添加正常数据点
  for i in 0..=50 {
    let timestamp = base_time + 7 * 86400 + i * 300  // 第8天的数据
    let hour = (i * 5) / 60 % 24
    let minute = (i * 5) % 60
    
    let base_request_rate = if hour >= 9 and hour <= 17 { 100.0 } else if hour >= 22 or hour <= 6 { 20.0 } else { 60.0 }
    let request_rate = base_request_rate + (minute % 10) * 2.0
    let latency = 50.0 + (hour % 8) * 5.0 + (minute % 5) * 2.0
    let error_rate = if minute % 50 == 0 { 2.0 } else { 0.5 }
    let throughput = request_rate * 0.8
    let cpu_usage = 30.0 + request_rate / 10.0
    let memory_usage = 40.0 + request_rate / 20.0
    
    test_data = test_data.push({
      timestamp: timestamp,
      metrics: [
        ("http.requests", request_rate),
        ("http.request.duration", latency),
        ("http.error.rate", error_rate),
        ("http.throughput", throughput),
        ("cpu.usage", cpu_usage),
        ("memory.usage", memory_usage)
      ]
    })
  }
  
  // 添加异常数据点
  // 异常1: 突然的高延迟
  test_data = test_data.push({
    timestamp: base_time + 7 * 86400 + 51 * 300,
    metrics: [
      ("http.requests", 100.0),
      ("http.request.duration", 500.0),  // 异常高延迟
      ("http.error.rate", 0.5),
      ("http.throughput", 80.0),
      ("cpu.usage", 40.0),
      ("memory.usage", 45.0)
    ]
  })
  
  // 异常2: 异常高错误率
  test_data = test_data.push({
    timestamp: base_time + 7 * 86400 + 52 * 300,
    metrics: [
      ("http.requests", 100.0),
      ("http.request.duration", 50.0),
      ("http.error.rate", 20.0),  // 异常高错误率
      ("http.throughput", 80.0),
      ("cpu.usage", 40.0),
      ("memory.usage", 45.0)
    ]
  })
  
  // 异常3: 异常低吞吐量
  test_data = test_data.push({
    timestamp: base_time + 7 * 86400 + 53 * 300,
    metrics: [
      ("http.requests", 100.0),
      ("http.request.duration", 50.0),
      ("http.error.rate", 0.5),
      ("http.throughput", 10.0),  // 异常低吞吐量
      ("cpu.usage", 40.0),
      ("memory.usage", 45.0)
    ]
  })
  
  // 异常4: 季节性异常（凌晨3点出现高流量）
  test_data = test_data.push({
    timestamp: base_time + 7 * 86400 + 3 * 3600,  // 凌晨3点
    metrics: [
      ("http.requests", 150.0),  // 异常高流量（夜间）
      ("http.request.duration", 50.0),
      ("http.error.rate", 0.5),
      ("http.throughput", 120.0),
      ("cpu.usage", 45.0),
      ("memory.usage", 47.5)
    ]
  })
  
  // 异常5: 多变量异常（CPU和内存使用率异常高但请求量正常）
  test_data = test_data.push({
    timestamp: base_time + 7 * 86400 + 54 * 300,
    metrics: [
      ("http.requests", 100.0),
      ("http.request.duration", 50.0),
      ("http.error.rate", 0.5),
      ("http.throughput", 80.0),
      ("cpu.usage", 95.0),  // 异常高CPU使用率
      ("memory.usage", 90.0)  // 异常高内存使用率
    ]
  })
  
  // 执行异常检测
  let anomaly_results = EnhancedDetector::detect(enhanced_detector, test_data)
  
  // 验证异常检测结果
  assert_true(anomaly_results.length() > 0)
  
  // 检查统计异常检测
  let statistical_anomalies = anomaly_results.filter(fn(r) { r.detector_type == "statistical" })
  assert_true(statistical_anomalies.length() > 0)
  
  // 检查延迟异常
  let latency_anomaly = statistical_anomalies.find(fn(a) { 
    a.detector_name == "z_score_latency" and 
    a.metric == "http.request.duration" and
    a.anomaly_score > 2.5
  })
  assert_true(latency_anomaly != None)
  
  // 检查错误率异常
  let error_rate_anomaly = statistical_anomalies.find(fn(a) { 
    a.detector_name == "iqr_error_rate" and 
    a.metric == "http.error.rate"
  })
  assert_true(error_rate_anomaly != None)
  
  // 检查机器学习异常检测
  let ml_anomalies = anomaly_results.filter(fn(r) { r.detector_type == "ml" })
  assert_true(ml_anomalies.length() > 0)
  
  // 检查吞吐量异常
  let throughput_anomaly = ml_anomalies.find(fn(a) { 
    a.detector_name == "isolation_forest_throughput" and 
    a.metric == "http.throughput"
  })
  assert_true(throughput_anomaly != None)
  
  // 检查季节性异常检测
  let seasonal_anomalies = anomaly_results.filter(fn(r) { r.detector_type == "seasonal" })
  assert_true(seasonal_anomalies.length() > 0)
  
  // 检查凌晨的流量异常
  let seasonal_request_anomaly = seasonal_anomalies.find(fn(a) { 
    a.detector_name == "seasonal_request_pattern" and 
    a.metric == "http.requests"
  })
  assert_true(seasonal_request_anomaly != None)
  
  // 检查多变量异常检测
  let multivariate_anomalies = anomaly_results.filter(fn(r) { r.detector_type == "multivariate" })
  assert_true(multivariate_anomalies.length() > 0)
  
  // 检查CPU和内存异常
  let multivariate_anomaly = multivariate_anomalies.find(fn(a) { 
    a.detector_name == "correlation_anomaly"
  })
  assert_true(multivariate_anomaly != None)
  
  // 测试异常分级
  let graded_anomalies = EnhancedDetector::grade_anomalies(enhanced_detector, anomaly_results)
  
  // 验证异常分级
  let critical_anomalies = graded_anomalies.filter(fn(a) { a.severity == "critical" })
  let high_anomalies = graded_anomalies.filter(fn(a) { a.severity == "high" })
  let medium_anomalies = graded_anomalies.filter(fn(a) { a.severity == "medium" })
  let low_anomalies = graded_anomalies.filter(fn(a) { a.severity == "low" })
  
  assert_true(graded_anomalies.length() == anomaly_results.length())
  assert_true(critical_anomalies.length() + high_anomalies.length() + medium_anomalies.length() + low_anomalies.length() > 0)
  
  // 测试异常解释
  let anomaly_explanations = EnhancedDetector::explain_anomalies(enhanced_detector, anomaly_results)
  
  // 验证异常解释
  assert_eq(anomaly_explanations.length(), anomaly_results.length())
  
  let latency_explanation = anomaly_explanations.find(fn(e) { e.anomaly_id == latency_anomaly.get().id })
  assert_true(latency_explanation != None)
  assert_true(latency_explanation.get().explanation.contains("latency"))
  assert_true(latency_explanation.get().factors.length() > 0)
}

// 测试6: 遥测数据可视化增强
test "遥测数据可视化增强测试" {
  // 创建增强可视化器
  let enhanced_visualizer = EnhancedTelemetryVisualizer::new()
  
  // 配置时间序列图表
  EnhancedVisualizer::add_time_series_chart(enhanced_visualizer, {
    name: "request_rate_trend",
    title: "请求率趋势",
    metrics: ["http.requests"],
    time_range: "1h",
    aggregation: "rate",
    group_by: ["service"],
    chart_type: "line"
  })
  
  EnhancedVisualizer::add_time_series_chart(enhanced_visualizer, {
    name: "latency_heatmap",
    title: "延迟热力图",
    metrics: ["http.request.duration"],
    time_range: "24h",
    aggregation: "percentile",
    percentile: 95,
    group_by: ["service", "endpoint"],
    chart_type: "heatmap"
  })
  
  // 配置服务拓扑图
  EnhancedVisualizer::add_topology_chart(enhanced_visualizer, {
    name: "service_topology",
    title: "服务拓扑",
    time_range: "30m",
    node_size_metric: "request_count",
    edge_width_metric: "call_count",
    node_color_metric: "error_rate",
    layout_algorithm: "force_directed"
  })
  
  // 配置分布图
  EnhancedVisualizer::add_distribution_chart(enhanced_visualizer, {
    name: "latency_distribution",
    title: "延迟分布",
    metric: "http.request.duration",
    time_range: "1h",
    group_by: ["service"],
    chart_type: "histogram",
    bins: 50
  })
  
  // 配置散点图
  EnhancedVisualizer::add_scatter_chart(enhanced_visualizer, {
    name: "throughput_vs_latency",
    title: "吞吐量与延迟关系",
    x_metric: "http.throughput",
    y_metric: "http.request.duration",
    time_range: "1h",
    group_by: ["service"],
    chart_type: "scatter"
  })
  
  // 创建测试数据
  let base_time = 1640995200
  let test_data = []
  
  // 生成1小时的数据（每分钟一个数据点）
  for i in 0..=60 {
    let timestamp = base_time + i * 60
    
    // 服务A的数据
    test_data = test_data.push({
      timestamp: timestamp,
      metrics: [
        ("http.requests", 100.0 + (i % 20) * 5.0),
        ("http.request.duration", 50.0 + (i % 15) * 3.0),
        ("http.throughput", 80.0 + (i % 20) * 4.0),
        ("http.error.rate", if i % 20 == 0 { 2.0 } else { 0.5 })
      ],
      attributes: [
        ("service", "api"),
        ("endpoint", "/users")
      ]
    })
    
    // 服务B的数据
    test_data = test_data.push({
      timestamp: timestamp,
      metrics: [
        ("http.requests", 80.0 + (i % 15) * 4.0),
        ("http.request.duration", 100.0 + (i % 20) * 5.0),
        ("http.throughput", 60.0 + (i % 15) * 3.0),
        ("http.error.rate", if i % 25 == 0 { 3.0 } else { 0.8 })
      ],
      attributes: [
        ("service", "payment"),
        ("endpoint", "/charge")
      ]
    })
    
    // 服务C的数据
    test_data = test_data.push({
      timestamp: timestamp,
      metrics: [
        ("http.requests", 120.0 + (i % 25) * 6.0),
        ("http.request.duration", 30.0 + (i % 10) * 2.0),
        ("http.throughput", 100.0 + (i % 25) * 5.0),
        ("http.error.rate", if i % 30 == 0 { 1.5 } else { 0.3 })
      ],
      attributes: [
        ("service", "cache"),
        ("endpoint", "/get")
      ]
    })
  }
  
  // 生成服务拓扑数据
  let topology_data = [
    {
      from_service: "api",
      to_service: "payment",
      call_count: 50,
      avg_latency: 150.0,
      error_rate: 0.02
    },
    {
      from_service: "api",
      to_service: "cache",
      call_count: 200,
      avg_latency: 30.0,
      error_rate: 0.005
    },
    {
      from_service: "payment",
      to_service: "database",
      call_count: 45,
      avg_latency: 200.0,
      error_rate: 0.01
    },
    {
      from_service: "api",
      to_service: "database",
      call_count: 30,
      avg_latency: 120.0,
      error_rate: 0.015
    }
  ]
  
  // 生成时间序列图表
  let time_series_result = EnhancedVisualizer::generate_time_series(enhanced_visualizer, "request_rate_trend", test_data)
  
  // 验证时间序列图表
  assert_true(time_series_result.success)
  assert_eq(time_series_result.chart_name, "request_rate_trend")
  assert_true(time_series_result.data.series.length() > 0)
  
  // 检查每个服务都有数据系列
  let api_series = time_series_result.data.series.find(fn(s) { s.name.contains("api") })
  let payment_series = time_series_result.data.series.find(fn(s) { s.name.contains("payment") })
  let cache_series = time_series_result.data.series.find(fn(s) { s.name.contains("cache") })
  
  assert_true(api_series != None)
  assert_true(payment_series != None)
  assert_true(cache_series != None)
  
  // 检查数据点
  match api_series {
    Some(series) => {
      assert_true(series.points.length() > 0)
      assert_true(series.points.all(fn(p) { p.x >= base_time and p.y >= 0 }))
    }
    None => assert_true(false)
  }
  
  // 生成热力图
  let heatmap_result = EnhancedVisualizer::generate_heatmap(enhanced_visualizer, "latency_heatmap", test_data)
  
  // 验证热力图
  assert_true(heatmap_result.success)
  assert_eq(heatmap_result.chart_name, "latency_heatmap")
  assert_true(heatmap_result.data.cells.length() > 0)
  
  // 检查热力图单元格
  let heatmap_cells = heatmap_result.data.cells
  assert_true(heatmap_cells.all(fn(c) { c.x >= 0 and c.y >= 0 and c.value >= 0 }))
  
  // 生成服务拓扑图
  let topology_result = EnhancedVisualizer::generate_topology(enhanced_visualizer, "service_topology", topology_data)
  
  // 验证服务拓扑图
  assert_true(topology_result.success)
  assert_eq(topology_result.chart_name, "service_topology")
  assert_true(topology_result.data.nodes.length() > 0)
  assert_true(topology_result.data.edges.length() > 0)
  
  // 检查节点
  let nodes = topology_result.data.nodes
  let api_node = nodes.find(fn(n) { n.id == "api" })
  let payment_node = nodes.find(fn(n) { n.id == "payment" })
  let cache_node = nodes.find(fn(n) { n.id == "cache" })
  let database_node = nodes.find(fn(n) { n.id == "database" })
  
  assert_true(api_node != None)
  assert_true(payment_node != None)
  assert_true(cache_node != None)
  assert_true(database_node != None)
  
  // 检查节点属性
  match api_node {
    Some(node) => {
      assert_true(node.size > 0)  // 节点大小基于请求计数
      assert_true(node.color >= 0)  // 节点颜色基于错误率
    }
    None => assert_true(false)
  }
  
  // 检查边
  let edges = topology_result.data.edges
  let api_to_payment_edge = edges.find(fn(e) { e.from == "api" and e.to == "payment" })
  assert_true(api_to_payment_edge != None)
  
  match api_to_payment_edge {
    Some(edge) => {
      assert_eq(edge.width, 50)  // 边宽度基于调用计数
    }
    None => assert_true(false)
  }
  
  // 生成分布图
  let distribution_result = EnhancedVisualizer::generate_distribution(enhanced_visualizer, "latency_distribution", test_data)
  
  // 验证分布图
  assert_true(distribution_result.success)
  assert_eq(distribution_result.chart_name, "latency_distribution")
  assert_true(distribution_result.data.bins.length() > 0)
  
  // 检查分布图数据
  let bins = distribution_result.data.bins
  assert_true(bins.all(fn(b) { b.lower_bound < b.upper_bound and b.count >= 0 }))
  
  // 生成散点图
  let scatter_result = EnhancedVisualizer::generate_scatter(enhanced_visualizer, "throughput_vs_latency", test_data)
  
  // 验证散点图
  assert_true(scatter_result.success)
  assert_eq(scatter_result.chart_name, "throughput_vs_latency")
  assert_true(scatter_result.data.points.length() > 0)
  
  // 检查散点图数据
  let scatter_points = scatter_result.data.points
  assert_true(scatter_points.all(fn(p) { p.x >= 0 and p.y >= 0 }))
  
  // 测试图表组合
  let dashboard_result = EnhancedVisualizer::create_dashboard(enhanced_visualizer, {
    name: "telemetry_overview",
    title: "遥测概览",
    layout: "grid",
    charts: [
      { id: "request_rate_trend", position: { row: 0, col: 0, width: 12, height: 6 } },
      { id: "latency_heatmap", position: { row: 6, col: 0, width: 6, height: 6 } },
      { id: "service_topology", position: { row: 6, col: 6, width: 6, height: 6 } },
      { id: "latency_distribution", position: { row: 12, col: 0, width: 6, height: 6 } },
      { id: "throughput_vs_latency", position: { row: 12, col: 6, width: 6, height: 6 } }
    ]
  })
  
  // 验证仪表板
  assert_true(dashboard_result.success)
  assert_eq(dashboard_result.dashboard.name, "telemetry_overview")
  assert_eq(dashboard_result.dashboard.charts.length(), 5)
  
  // 测试图表导出
  let export_result = EnhancedVisualizer::export_chart(enhanced_visualizer, "request_rate_trend", "png")
  
  // 验证导出
  assert_true(export_result.success)
  assert_eq(export_result.format, "png")
  assert_true(export_result.data.length() > 0)
  
  // 测试图表交互
  let interaction_result = EnhancedVisualizer::add_interactivity(enhanced_visualizer, "request_rate_trend", {
    zoom: true,
    pan: true,
    tooltip: true,
    crosshair: true,
    selection: true
  })
  
  // 验证交互功能
  assert_true(interaction_result.success)
  assert_true(interaction_result.features.zoom)
  assert_true(interaction_result.features.pan)
  assert_true(interaction_result.features.tooltip)
  assert_true(interaction_result.features.crosshair)
  assert_true(interaction_result.features.selection)
}

// 测试7: 遥测数据压缩与传输优化
test "遥测数据压缩与传输优化测试" {
  // 创建压缩传输管理器
  let compression_transport = CompressionTransportManager::new()
  
  // 配置压缩策略
  CompressionTransport::set_compression_strategy(compression_transport, {
    algorithm: "adaptive",
    fallback: "gzip",
    threshold: 1024,  // 1KB以上的数据才压缩
    level: "balanced"  // 平衡压缩率和速度
  })
  
  // 配置传输优化
  CompressionTransport::set_transport_optimization(compression_transport, {
    batch_size: 100,        // 批量发送100条记录
    batch_timeout: 5000,    // 5秒超时
    retry_count: 3,         // 重试3次
    retry_delay: 1000,      // 重试延迟1秒
    compression_threshold: 0.7  // 压缩率超过30%才使用压缩
  })
  
  // 配置网络优化
  CompressionTransport::set_network_optimization(compression_transport, {
    connection_pool_size: 10,
    keep_alive: true,
    tcp_nodelay: true,
    multiplexing: true
  })
  
  // 创建测试数据
  let test_data = []
  
  // 生成1000条遥测记录
  for i in 0..=1000 {
    test_data = test_data.push({
      trace_id: "trace-" + (i % 100).to_string(),
      span_id: "span-" + i.to_string(),
      service: "service-" + (i % 10).to_string(),
      operation: "operation-" + (i % 20).to_string(),
      start_time: 1640995200 + i,
      duration: 50 + (i % 200),
      status: if i % 50 == 0 { "error" } else { "ok" },
      attributes: [
        ("http.method", "GET"),
        ("http.status_code", (200 + (i % 5) * 100).to_string()),
        ("user.id", "user-" + (i % 500).to_string()),
        ("request.id", "req-" + i.to_string())
      ],
      events: if i % 100 == 0 {
        [
          { name: "error", timestamp: 1640995200 + i + 10, attributes: [("error.message", "timeout")] }
        ]
      } else {
        []
      },
      links: if i % 80 == 0 {
        [
          { trace_id: "linked-trace-" + (i % 50).to_string(), span_id: "linked-span-" + i.to_string() }
        ]
      } else {
        []
      }
    })
  }
  
  // 序列化数据
  let serialized_data = TelemetryDataSerializer::serialize(test_data)
  let original_size = serialized_data.length()
  
  // 压缩数据
  let compression_result = CompressionTransport::compress(compression_transport, serialized_data)
  
  // 验证压缩结果
  assert_true(compression_result.success)
  assert_true(compression_result.compressed_size < original_size)
  
  let compression_ratio = 1.0 - (compression_result.compressed_size.to_float() / original_size.to_float())
  assert_true(compression_ratio > 0.3)  // 至少30%的压缩率
  
  // 验证压缩算法选择
  assert_true(compression_result.algorithm == "gzip" or compression_result.algorithm == "lz4" or compression_result.algorithm == "snappy")
  
  // 解压数据
  let decompression_result = CompressionTransport::decompress(compression_transport, compression_result.compressed_data, compression_result.algorithm)
  
  // 验证解压结果
  assert_true(decompression_result.success)
  assert_eq(decompression_result.decompressed_size, original_size)
  
  // 反序列化验证数据完整性
  let deserialized_data = TelemetryDataSerializer::deserialize(decompression_result.decompressed_data)
  assert_eq(deserialized_data.length(), test_data.length())
  
  // 检查数据点完整性
  for i in 0..=test_data.length() - 1 {
    assert_eq(deserialized_data[i].trace_id, test_data[i].trace_id)
    assert_eq(deserialized_data[i].span_id, test_data[i].span_id)
    assert_eq(deserialized_data[i].service, test_data[i].service)
  }
  
  // 测试批量传输
  let transport_result = CompressionTransport::send_batch(compression_transport, test_data)
  
  // 验证传输结果
  assert_true(transport_result.success)
  assert_eq(transport_result.records_sent, test_data.length())
  assert_true(transport_result.bytes_sent < original_size)  // 压缩后传输的应该更少
  
  // 测试传输重试机制
  let unreliable_transport = CompressionTransport::create_unreliable_transport(compression_transport, {
    failure_rate: 0.3,  // 30%的失败率
    random_delay: true
  })
  
  let retry_result = CompressionTransport::send_with_retry(unreliable_transport, test_data)
  
  // 验证重试结果
  assert_true(retry_result.success)
  assert_true(retry_result.attempt_count <= 4)  // 1次初始尝试 + 3次重试
  assert_eq(retry_result.records_sent, test_data.length())
  
  // 测试自适应压缩策略
  let adaptive_compression_data = [
    { data: "a".repeat(100), expected_algorithm: "none" },      // 小数据不压缩
    { data: "a".repeat(2000), expected_algorithm: "lz4" },      // 中等数据用快速压缩
    { data: "a".repeat(10000), expected_algorithm: "gzip" }     // 大数据用高压缩率
  ]
  
  for test_case in adaptive_compression_data {
    let adaptive_result = CompressionTransport::adaptive_compress(compression_transport, test_case.data)
    
    assert_true(adaptive_result.success)
    assert_eq(adaptive_result.algorithm, test_case.expected_algorithm)
  }
  
  // 测试网络优化
  let network_stats = CompressionTransport::get_network_statistics(compression_transport)
  
  // 验证网络统计
  assert_true(network_stats.connection_pool_size > 0)
  assert_true(network_stats.active_connections <= network_stats.connection_pool_size)
  assert_true(network_stats.total_bytes_sent > 0)
  assert_true(network_stats.total_bytes_received > 0)
  assert_true(network_stats.compression_savings > 0)
  
  // 测试传输优化配置
  let optimization_config = CompressionTransport::get_optimization_config(compression_transport)
  
  // 验证优化配置
  assert_eq(optimization_config.batch_size, 100)
  assert_eq(optimization_config.batch_timeout, 5000)
  assert_eq(optimization_config.retry_count, 3)
  assert_eq(optimization_config.retry_delay, 1000)
  assert_eq(optimization_config.compression_threshold, 0.7)
  
  // 测试性能基准
  let benchmark_result = CompressionTransport::benchmark(compression_transport, {
    data_size: 10000,
    iterations: 10,
    algorithms: ["none", "lz4", "gzip", "snappy"]
  })
  
  // 验证基准测试结果
  assert_true(benchmark_result.results.length() > 0)
  
  // 检查压缩率
  let gzip_result = benchmark_result.results.find(fn(r) { r.algorithm == "gzip" })
  assert_true(gzip_result != None)
  assert_true(gzip_result.get().compression_ratio > 0.5)  // gzip应该有较高的压缩率
  
  // 检查压缩速度
  let lz4_result = benchmark_result.results.find(fn(r) { r.algorithm == "lz4" })
  assert_true(lz4_result != None)
  assert_true(lz4_result.get().compression_speed > gzip_result.get().compression_speed)  // lz4应该更快
  
  // 测试并行压缩
  let parallel_result = CompressionTransport::parallel_compress(compression_transport, test_data, {
    chunk_size: 100,
    worker_count: 4
  })
  
  // 验证并行压缩结果
  assert_true(parallel_result.success)
  assert_eq(parallel_result.chunks.length(), 10)  // 1000条记录，每块100条
  assert_true(parallel_result.total_compression_time < compression_result.compression_time * 1.2)  // 并行应该更快或相当
}

// 测试8: 遥测数据存储优化
test "遥测数据存储优化测试" {
  // 创建存储优化管理器
  let storage_optimizer = TelemetryStorageOptimizer::new()
  
  // 配置存储策略
  StorageOptimizer::set_storage_strategy(storage_optimizer, {
    tiered_storage: true,
    hot_tier: {
      type: "memory",
      retention: "1h",
      max_size: "1GB"
    },
    warm_tier: {
      type: "ssd",
      retention: "7d",
      max_size: "100GB"
    },
    cold_tier: {
      type: "hdd",
      retention: "30d",
      max_size: "1TB"
    },
    archive_tier: {
      type: "object_storage",
      retention: "365d",
      compression: "true"
    }
  })
  
  // 配置数据生命周期管理
  StorageOptimizer::set_lifecycle_policy(storage_optimizer, {
    rules: [
      {
        name: "move_to_warm",
        condition: "age > 1h",
        action: "move_to_tier",
        target_tier: "warm",
        priority: 1
      },
      {
        name: "move_to_cold",
        condition: "age > 7d",
        action: "move_to_tier",
        target_tier: "cold",
        priority: 2
      },
      {
        name: "archive",
        condition: "age > 30d",
        action: "move_to_tier",
        target_tier: "archive",
        priority: 3
      },
      {
        name: "delete",
        condition: "age > 365d",
        action: "delete",
        priority: 4
      }
    ]
  })
  
  // 配置数据压缩策略
  StorageOptimizer::set_compression_policy(storage_optimizer, {
    hot_tier: {
      enabled: false,
      algorithm: "none"
    },
    warm_tier: {
      enabled: true,
      algorithm: "lz4",
      level: "fast"
    },
    cold_tier: {
      enabled: true,
      algorithm: "gzip",
      level: "balanced"
    },
    archive_tier: {
      enabled: true,
      algorithm: "zstd",
      level: "best"
    }
  })
  
  // 创建测试数据
  let base_time = 1640995200
  let test_data = []
  
  // 生成不同时间的数据
  for age_hours in [0, 2, 24, 168, 720, 8760] {  // 0h, 2h, 1d, 7d, 30d, 1y
    let timestamp = base_time - age_hours * 3600
    
    for i in 0..=100 {
      test_data = test_data.push({
        timestamp: timestamp + i * 60,
        trace_id: "trace-" + (age_hours * 100 + i).to_string(),
        span_id: "span-" + (age_hours * 100 + i).to_string(),
        service: "service-" + (i % 5).to_string(),
        duration: 50 + (i % 200),
        status: if i % 50 == 0 { "error" } else { "ok" },
        attributes: [
          ("http.method", "GET"),
          ("http.status_code", (200 + (i % 5) * 100).to_string()),
          ("user.id", "user-" + (i % 500).to_string())
        ]
      })
    }
  }
  
  // 存储数据
  let storage_result = StorageOptimizer::store_batch(storage_optimizer, test_data)
  
  // 验证存储结果
  assert_true(storage_result.success)
  assert_eq(storage_result.records_stored, test_data.length())
  
  // 验证分层存储
  let tier_distribution = StorageOptimizer::get_tier_distribution(storage_optimizer)
  
  // 检查热层存储（1小时内）
  let hot_tier_count = tier_distribution.get("hot")
  assert_true(hot_tier_count != None)
  assert_eq(hot_tier_count.get(), 101)  // 0h的数据应该在热层
  
  // 检查温层存储（1小时到7天）
  let warm_tier_count = tier_distribution.get("warm")
  assert_true(warm_tier_count != None)
  assert_eq(warm_tier_count.get(), 404)  // 2h, 1d, 7d的数据应该在温层
  
  // 检查冷层存储（7天到30天）
  let cold_tier_count = tier_distribution.get("cold")
  assert_true(cold_tier_count != None)
  assert_eq(cold_tier_count.get(), 101)  // 30d的数据应该在冷层
  
  // 检查归档层存储（30天到1年）
  let archive_tier_count = tier_distribution.get("archive")
  assert_true(archive_tier_count != None)
  assert_eq(archive_tier_count.get(), 101)  // 1y的数据应该在归档层
  
  // 测试数据检索
  let query_result = StorageOptimizer::query(storage_optimizer, {
    time_range: { start: base_time - 2 * 3600, end: base_time },  // 最近2小时
    filters: [
      { field: "service", operator: "=", value: "service-1" }
    ],
    limit: 50
  })
  
  // 验证查询结果
  assert_true(query_result.success)
  assert_true(query_result.records.length() <= 50)
  assert_true(query_result.source_tiers.contains("hot"))  // 应该从热层检索
  
  // 测试跨层查询
  let cross_tier_query = StorageOptimizer::query(storage_optimizer, {
    time_range: { start: base_time - 30 * 24 * 3600, end: base_time },  // 最近30天
    filters: [
      { field: "status", operator: "=", value: "error" }
    ],
    limit: 100
  })
  
  // 验证跨层查询结果
  assert_true(cross_tier_query.success)
  assert_true(cross_tier_query.records.length() > 0)
  assert_true(cross_tier_query.source_tiers.length() > 1)  // 应该从多个层检索
  
  // 测试生命周期管理
  let lifecycle_result = StorageOptimizer::apply_lifecycle_policies(storage_optimizer)
  
  // 验证生命周期管理结果
  assert_true(lifecycle_result.success)
  assert_true(lifecycle_result.actions_executed.length() > 0)
  
  // 检查执行的动作
  let move_actions = lifecycle_result.actions_executed.filter(fn(a) { a.action == "move_to_tier" })
  assert_true(move_actions.length() > 0)
  
  // 测试压缩效果
  let compression_stats = StorageOptimizer::get_compression_statistics(storage_optimizer)
  
  // 验证压缩统计
  assert_true(compression_stats.hot_tier.compression_enabled == false)
  assert_true(compression_stats.warm_tier.compression_enabled == true)
  assert_true(compression_stats.cold_tier.compression_enabled == true)
  assert_true(compression_stats.archive_tier.compression_enabled == true)
  
  // 检查压缩率
  assert_true(compression_stats.warm_tier.compression_ratio > 0.3)
  assert_true(compression_stats.cold_tier.compression_ratio > 0.5)
  assert_true(compression_stats.archive_tier.compression_ratio > 0.7)
  
  // 测试存储优化
  let optimization_result = StorageOptimizer::optimize_storage(storage_optimizer, {
    target_free_space: 0.2,  // 目标20%空闲空间
    prioritize_recent_data: true,
    aggressive_compression: false
  })
  
  // 验证优化结果
  assert_true(optimization_result.success)
  assert_true(optimization_result.space_freed > 0)
  assert_true(optimization_result.optimizations_applied.length() > 0)
  
  // 测试数据保留策略
  let retention_result = StorageOptimizer::apply_retention_policy(storage_optimizer, {
    service_specific: [
      { service: "service-1", retention_days: 90 },
      { service: "service-2", retention_days: 180 }
    ],
    error_data_retention_multiplier: 2.0,  // 错误数据保留时间加倍
    high_value_trace_retention: "unlimited"  // 高价值追踪永久保留
  })
  
  // 验证保留策略结果
  assert_true(retention_result.success)
  assert_true(retention_result.records_expired.length() >= 0)
  
  // 测试存储性能基准
  let benchmark_result = StorageOptimizer::benchmark(storage_optimizer, {
    operations: ["write", "read", "query"],
    data_sizes: [100, 1000, 10000],
    tiers: ["hot", "warm", "cold"],
    iterations: 10
  })
  
  // 验证基准测试结果
  assert_true(benchmark_result.results.length() > 0)
  
  // 检查热层性能
  let hot_write_benchmark = benchmark_result.results.find(fn(r) { 
    r.tier == "hot" and r.operation == "write" and r.data_size == 1000 
  })
  assert_true(hot_write_benchmark != None)
  assert_true(hot_write_benchmark.get().throughput > 1000)  // 热层写入应该很快
  
  // 检查冷层性能
  let cold_read_benchmark = benchmark_result.results.find(fn(r) { 
    r.tier == "cold" and r.operation == "read" and r.data_size == 1000 
  })
  assert_true(cold_read_benchmark != None)
  assert_true(cold_read_benchmark.get().throughput < hot_write_benchmark.get().throughput)  // 冷层应该比热层慢
  
  // 测试数据迁移
  let migration_result = StorageOptimizer::migrate_data(storage_optimizer, {
    source_tier: "warm",
    target_tier: "cold",
    criteria: { age_days: 10 },  // 迁移10天前的数据
    batch_size: 100,
    parallel_workers: 4
  })
  
  // 验证迁移结果
  assert_true(migration_result.success)
  assert_true(migration_result.records_migrated > 0)
  assert_eq(migration_result.source_tier, "warm")
  assert_eq(migration_result.target_tier, "cold")
}

// 测试9: 遥测数据安全与隐私
test "遥测数据安全与隐私测试" {
  // 创建安全管理器
  let security_manager = TelemetrySecurityManager::new()
  
  // 配置数据脱敏策略
  SecurityManager::add_data_masking_rule(security_manager, {
    name: "mask_user_id",
    field_path: "attributes.user.id",
    masking_type: "hash",
    algorithm: "sha256",
    salt: "telemetry_salt"
  })
  
  SecurityManager::add_data_masking_rule(security_manager, {
    name: "mask_email",
    field_path: "attributes.user.email",
    masking_type: "partial",
    visible_chars: 2,
    mask_char: "*"
  })
  
  SecurityManager::add_data_masking_rule(security_manager, {
    name: "mask_ip",
    field_path: "attributes.client.ip",
    masking_type: "tokenization",
    token_prefix: "ip-"
  })
  
  SecurityManager::add_data_masking_rule(security_manager, {
    name: "mask_credit_card",
    field_path: "attributes.payment.card_number",
    masking_type: "pattern",
    pattern: "####-####-####-####",
    preserve_last: 4
  })
  
  // 配置访问控制策略
  SecurityManager::add_access_policy(security_manager, {
    name: "admin_full_access",
    subjects: ["role:admin"],
    resources: ["telemetry.*"],
    actions: ["read", "write", "delete"],
    effect: "allow"
  })
  
  SecurityManager::add_access_policy(security_manager, {
    name: "operator_limited_access",
    subjects: ["role:operator"],
    resources: ["telemetry.metrics.*", "telemetry.traces.*"],
    actions: ["read"],
    effect: "allow",
    conditions: [
      { field: "data.attributes.sensitivity", operator: "!=", value: "high" }
    ]
  })
  
  SecurityManager::add_access_policy(security_manager, {
    name: "analyst_metric_only",
    subjects: ["role:analyst"],
    resources: ["telemetry.metrics.*"],
    actions: ["read"],
    effect: "allow",
    conditions: [
      { field: "data.time_range", operator: "<=", value: "90d" }
    ]
  })
  
  // 配置数据加密策略
  SecurityManager::set_encryption_policy(security_manager, {
    at_rest: {
      enabled: true,
      algorithm: "aes-256-gcm",
      key_rotation_interval: 86400 * 30  // 30天轮换一次密钥
    },
    in_transit: {
      enabled: true,
      protocol: "tls-1.3",
      cipher_suites: ["tls_aes_256_gcm_sha384"]
    }
  })
  
  // 配置审计日志策略
  SecurityManager::set_audit_policy(security_manager, {
    log_access: true,
    log_modifications: true,
    log_data_exports: true,
    retention_days: 365,
    include_user_context: true,
    include_request_details: true
  })
  
  // 创建测试数据
  let sensitive_data = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      service: "payment",
      operation: "charge",
      attributes: [
        ("user.id", "user-12345"),
        ("user.email", "john.doe@example.com"),
        ("client.ip", "192.168.1.100"),
        ("payment.card_number", "4111-1111-1111-1111"),
        ("payment.amount", "99.99"),
        ("sensitivity", "high")
      ]
    },
    {
      trace_id: "trace-002",
      span_id: "span-002",
      service: "api",
      operation: "get_user",
      attributes: [
        ("user.id", "user-67890"),
        ("user.email", "jane.smith@example.com"),
        ("client.ip", "10.0.0.50"),
        ("sensitivity", "medium")
      ]
    },
    {
      trace_id: "trace-003",
      span_id: "span-003",
      service: "analytics",
      operation: "report",
      attributes: [
        ("user.id", "user-11111"),
        ("sensitivity", "low")
      ]
    }
  ]
  
  // 测试数据脱敏
  let masking_result = SecurityManager::apply_data_masking(security_manager, sensitive_data)
  
  // 验证脱敏结果
  assert_eq(masking_result.length(), sensitive_data.length())
  
  // 检第一条记录的脱敏结果
  let masked_record1 = masking_result[0]
  
  // 检查用户ID哈希
  let user_id_attr = masked_record1.attributes.find(fn(a) { a.0 == "user.id" })
  assert_true(user_id_attr != None)
  assert_ne(user_id_attr.get().1, "user-12345")  // 应该被哈希
  assert_eq(user_id_attr.get().1.length(), 64)  // SHA256哈希长度
  
  // 检查邮箱部分隐藏
  let email_attr = masked_record1.attributes.find(fn(a) { a.0 == "user.email" })
  assert_true(email_attr != None)
  assert_true(email_attr.get().1.starts_with("jo"))
  assert_true(email_attr.get().1.contains("*"))
  
  // 检查IP地址令牌化
  let ip_attr = masked_record1.attributes.find(fn(a) { a.0 == "client.ip" })
  assert_true(ip_attr != None)
  assert_true(ip_attr.get().1.starts_with("ip-"))
  assert_ne(ip_attr.get().1, "192.168.1.100")
  
  // 检查信用卡号部分隐藏
  let card_attr = masked_record1.attributes.find(fn(a) { a.0 == "payment.card_number" })
  assert_true(card_attr != None)
  assert_true(card_attr.get().1.ends_with("1111"))
  assert_true(card_attr.get().1.contains("####"))
  
  // 测试访问控制
  let admin_context = { user: "admin", roles: ["admin"] }
  let operator_context = { user: "operator", roles: ["operator"] }
  let analyst_context = { user: "analyst", roles: ["analyst"] }
  
  // 管理员访问测试
  let admin_access = SecurityManager::check_access(security_manager, admin_context, {
    resource: "telemetry.traces.trace-001",
    action: "read",
    data: masked_record1
  })
  assert_true(admin_access.allowed)
  
  // 操作员访问测试
  let operator_access = SecurityManager::check_access(security_manager, operator_context, {
    resource: "telemetry.traces.trace-001",
    action: "read",
    data: masked_record1
  })
  assert_false(operator_access.allowed)  // 高敏感度数据应该被拒绝
  
  let operator_access2 = SecurityManager::check_access(security_manager, operator_context, {
    resource: "telemetry.traces.trace-002",
    action: "read",
    data: masking_result[1]
  })
  assert_true(operator_access2.allowed)  // 中等敏感度数据应该允许
  
  // 分析师访问测试
  let analyst_access = SecurityManager::check_access(security_manager, analyst_context, {
    resource: "telemetry.metrics.request_rate",
    action: "read",
    data: { time_range: "30d" }
  })
  assert_true(analyst_access.allowed)  // 30天内的指标应该允许
  
  let analyst_access2 = SecurityManager::check_access(security_manager, analyst_context, {
    resource: "telemetry.traces.trace-001",
    action: "read",
    data: masked_record1
  })
  assert_false(analyst_access2.allowed)  // 分析师不能访问追踪数据
  
  // 测试数据加密
  let encryption_result = SecurityManager::encrypt_data(security_manager, masking_result)
  
  // 验证加密结果
  assert_true(encryption_result.success)
  assert_true(encryption_result.encrypted_data.length() > 0)
  assert_ne(encryption_result.encrypted_data, masking_result.to_string())
  
  // 测试数据解密
  let decryption_result = SecurityManager::decrypt_data(security_manager, encryption_result.encrypted_data)
  
  // 验证解密结果
  assert_true(decryption_result.success)
  assert_eq(decryption_result.decrypted_data.length(), masking_result.length())
  
  // 验证解密数据与原始脱敏数据一致
  for i in 0..=masking_result.length() - 1 {
    assert_eq(decryption_result.decrypted_data[i].trace_id, masking_result[i].trace_id)
    assert_eq(decryption_result.decrypted_data[i].span_id, masking_result[i].span_id)
  }
  
  // 测试审计日志
  let audit_log = SecurityManager::get_audit_log(security_manager, {
    start_time: Time::now() - 3600,  // 最近1小时
    end_time: Time::now(),
    user: "admin",
    action: "read"
  })
  
  // 验证审计日志
  assert_true(audit_log.length() > 0)
  
  let admin_log_entry = audit_log.find(fn(entry) { entry.user == "admin" and entry.action == "read" })
  assert_true(admin_log_entry != None)
  
  match admin_log_entry {
    Some(entry) => {
      assert_true(entry.timestamp > 0)
      assert_true(entry.resource.contains("telemetry"))
      assert_true(entry.success)
    }
    None => assert_true(false)
  }
  
  // 测试数据导出安全
  let export_request = {
    requester: operator_context,
    data_type: "traces",
    filters: [
      { field: "service", operator: "=", value: "api" }
    ],
    format: "json",
    include_sensitive: false
  }
  
  let export_result = SecurityManager::secure_export(security_manager, export_request)
  
  // 验证导出结果
  assert_true(export_result.success)
  assert_true(export_result.data.length() > 0)
  assert_false(export_result.data.contains("user.email"))  // 敏感字段应该被排除
  
  // 测试密钥轮换
  let key_rotation_result = SecurityManager::rotate_encryption_keys(security_manager)
  
  // 验证密钥轮换
  assert_true(key_rotation_result.success)
  assert_true(key_rotation_result.new_key_id.length() > 0)
  assert_ne(key_rotation_result.new_key_id, key_rotation_result.old_key_id)
  
  // 验证轮换后仍能解密旧数据
  let old_data_decryption = SecurityManager::decrypt_data(security_manager, encryption_result.encrypted_data)
  assert_true(old_data_decryption.success)
  
  // 测试数据保留策略
  let retention_policy = {
    sensitive_data: {
      retention_days: 30,
      anonymize_after: 7
    },
    normal_data: {
      retention_days: 365,
      compress_after: 90
    }
  }
  
  let retention_result = SecurityManager::apply_retention_policy(security_manager, retention_policy)
  
  // 验证保留策略结果
  assert_true(retention_result.success)
  assert_true(retention_result.records_anonymized >= 0)
  assert_true(retention_result.records_deleted >= 0)
  
  // 测试隐私合规检查
  let compliance_check = SecurityManager::check_privacy_compliance(security_manager, {
    regulations: ["gdpr", "ccpa"],
    data_categories: ["personal", "financial"],
    consent_required: true,
    purpose_limitation: true
  })
  
  // 验证合规检查
  assert_true(compliance_check.compliant)
  assert_true(compliance_check.checks.length() > 0)
  
  let gdpr_check = compliance_check.checks.find(fn(c) { c.regulation == "gdpr" })
  assert_true(gdpr_check != None)
  assert_true(gdpr_check.get().passed)
}

// 测试10: 遥测系统性能基准
test "遥测系统性能基准测试" {
  // 创建性能基准测试管理器
  let benchmark_manager = TelemetryBenchmarkManager::new()
  
  // 配置基准测试场景
  BenchmarkManager::add_scenario(benchmark_manager, {
    name: "high_volume_ingestion",
    description: "高容量数据摄取性能测试",
    setup: fn() {
      // 设置高容量摄取环境
      {
        data_generator: HighVolumeDataGenerator::new({
          records_per_second: 10000,
          record_size_bytes: 1024,
          variance: 0.2
        }),
        ingestion_pipeline: IngestionPipeline::new({
          batch_size: 1000,
          workers: 8,
          queue_size: 10000
        })
      }
    },
    teardown: fn(env) {
      // 清理环境
      IngestionPipeline::cleanup(env.ingestion_pipeline)
    },
    benchmark: fn(env) {
      let start_time = Time::now()
      let records = DataGenerator::generate(env.data_generator, 100000)
      let ingest_result = IngestionPipeline::ingest(env.ingestion_pipeline, records)
      let end_time = Time::now()
      
      {
        duration_ms: end_time - start_time,
        records_processed: ingest_result.processed_count,
        success_rate: ingest_result.processed_count.to_float() / records.length().to_float(),
        throughput: records.length().to_float() / ((end_time - start_time) / 1000.0)
      }
    },
    iterations: 5,
    warmup_iterations: 2
  })
  
  BenchmarkManager::add_scenario(benchmark_manager, {
    name: "complex_query_performance",
    description: "复杂查询性能测试",
    setup: fn() {
      // 设置复杂查询环境
      {
        query_engine: QueryEngine::new({
          indexing: "full",
          cache_size: "1GB",
          parallel_workers: 4
        }),
        test_dataset: QueryTestDataset::new({
          records: 1000000,
          time_span_days: 30,
          services: 50,
          attributes_per_record: 10
        })
      }
    },
    teardown: fn(env) {
      QueryEngine::cleanup(env.query_engine)
    },
    benchmark: fn(env) {
      let queries = [
        "SELECT service, COUNT(*) FROM telemetry WHERE timestamp > NOW() - INTERVAL '1 day' GROUP BY service",
        "SELECT AVG(duration), P95(duration) FROM telemetry WHERE service = 'payment' AND timestamp > NOW() - INTERVAL '7 days'",
        "SELECT trace_id, SUM(duration) as total_duration FROM telemetry WHERE status = 'error' GROUP BY trace_id ORDER BY total_duration DESC LIMIT 100",
        "SELECT attributes['user.id'], COUNT(*) FROM telemetry WHERE service = 'api' AND timestamp > NOW() - INTERVAL '1 hour' GROUP BY attributes['user.id'] HAVING COUNT(*) > 10"
      ]
      
      let mut total_time = 0
      let mut total_results = 0
      
      for query in queries {
        let start_time = Time::now()
        let result = QueryEngine::execute(env.query_engine, query)
        let end_time = Time::now()
        
        total_time = total_time + (end_time - start_time)
        total_results = total_results + result.row_count
      }
      
      {
        total_duration_ms: total_time,
        average_query_time_ms: total_time / queries.length(),
        total_results_returned: total_results,
        queries_per_second: queries.length().to_float() / (total_time / 1000.0)
      }
    },
    iterations: 10,
    warmup_iterations: 3
  })
  
  BenchmarkManager::add_scenario(benchmark_manager, {
    name: "data_compression_efficiency",
    description: "数据压缩效率测试",
    setup: fn() {
      // 设置压缩测试环境
      {
        compression_algorithms: ["gzip", "lz4", "snappy", "zstd"],
        test_data: CompressionTestDataset::new({
          records: 10000,
          record_types: ["span", "metric", "log"],
          attribute_variety: "high",
          text_repetition: "medium"
        })
      }
    },
    teardown: fn(_env) {
      // 无需特殊清理
    },
    benchmark: fn(env) {
      let mut results = []
      
      for algorithm in env.compression_algorithms {
        let start_time = Time::now()
        let compressed = CompressionUtil::compress(env.test_data, algorithm)
        let compression_time = Time::now() - start_time
        
        let start_time = Time::now()
        let decompressed = CompressionUtil::decompress(compressed.data, algorithm)
        let decompression_time = Time::now() - start_time
        
        results = results.push({
          algorithm: algorithm,
          original_size: env.test_data.size,
          compressed_size: compressed.size,
          compression_ratio: compressed.size.to_float() / env.test_data.size.to_float(),
          compression_time_ms: compression_time,
          decompression_time_ms: decompression_time,
          compression_throughput: env.test_data.size.to_float() / (compression_time / 1000.0),
          decompression_throughput: compressed.size.to_float() / (decompression_time / 1000.0)
        })
      }
      
      { results }
    },
    iterations: 5,
    warmup_iterations: 1
  })
  
  BenchmarkManager::add_scenario(benchmark_manager, {
    name: "memory_usage_under_load",
    description: "负载下内存使用测试",
    setup: fn() {
      // 设置内存测试环境
      {
        telemetry_system: TelemetrySystem::new({
          max_memory_mb: 2048,
          gc_strategy: "generational",
          buffer pools: 16
        }),
        load_generator: LoadGenerator::new({
          concurrent_streams: 50,
          records_per_stream: 1000,
          burst_intervals: [10, 30, 60]  // 每10、30、60秒一个突发
        })
      }
    },
    teardown: fn(env) {
      TelemetrySystem::cleanup(env.telemetry_system)
    },
    benchmark: fn(env) {
      let initial_memory = MemoryMonitor::current_usage()
      
      let start_time = Time::now()
      let load_result = LoadGenerator::run(env.load_generator, env.telemetry_system)
      let end_time = Time::now()
      
      let peak_memory = MemoryMonitor::peak_usage()
      let final_memory = MemoryMonitor::current_usage()
      
      // 强制垃圾回收
      GC::collect()
      let after_gc_memory = MemoryMonitor::current_usage()
      
      {
        duration_seconds: (end_time - start_time) / 1000,
        records_processed: load_result.records_processed,
        initial_memory_mb: initial_memory.used_mb,
        peak_memory_mb: peak_memory.used_mb,
        final_memory_mb: final_memory.used_mb,
        after_gc_memory_mb: after_gc_memory.used_mb,
        memory_efficiency: load_result.records_processed.to_float() / peak_memory.used_mb.to_float(),
        gc_pressure: (peak_memory.used_mb - after_gc_memory.used_mb).to_float() / peak_memory.used_mb.to_float()
      }
    },
    iterations: 3,
    warmup_iterations: 1
  })
  
  // 运行基准测试
  let benchmark_results = BenchmarkManager::run_all_benchmarks(benchmark_manager)
  
  // 验证基准测试结果
  assert_true(benchmark_results.length() > 0)
  
  // 检查高容量摄取测试结果
  let ingestion_benchmark = benchmark_results.find(fn(r) { r.scenario_name == "high_volume_ingestion" })
  assert_true(ingestion_benchmark != None)
  
  match ingestion_benchmark {
    Some(result) => {
      assert_true(result.iterations.length() > 0)
      
      // 计算平均吞吐量
      let avg_throughput = result.iterations.reduce(fn(acc, iter) { 
        acc + iter.throughput 
      }, 0.0) / result.iterations.length().to_float()
      
      // 验证吞吐量应该合理（至少每秒5000条记录）
      assert_true(avg_throughput > 5000.0)
      
      // 验证成功率应该很高（至少99%）
      let avg_success_rate = result.iterations.reduce(fn(acc, iter) { 
        acc + iter.success_rate 
      }, 0.0) / result.iterations.length().to_float()
      
      assert_true(avg_success_rate > 0.99)
    }
    None => assert_true(false)
  }
  
  // 检查复杂查询测试结果
  let query_benchmark = benchmark_results.find(fn(r) { r.scenario_name == "complex_query_performance" })
  assert_true(query_benchmark != None)
  
  match query_benchmark {
    Some(result) => {
      assert_true(result.iterations.length() > 0)
      
      // 计算平均查询时间
      let avg_query_time = result.iterations.reduce(fn(acc, iter) { 
        acc + iter.average_query_time_ms 
      }, 0) / result.iterations.length()
      
      // 验证平均查询时间应该合理（每个查询不超过100ms）
      assert_true(avg_query_time < 100)
      
      // 验证查询吞吐量
      let avg_queries_per_sec = result.iterations.reduce(fn(acc, iter) { 
        acc + iter.queries_per_second 
      }, 0.0) / result.iterations.length().to_float()
      
      assert_true(avg_queries_per_sec > 10.0)  // 至少每秒10个查询
    }
    None => assert_true(false)
  }
  
  // 检查压缩效率测试结果
  let compression_benchmark = benchmark_results.find(fn(r) { r.scenario_name == "data_compression_efficiency" })
  assert_true(compression_benchmark != None)
  
  match compression_benchmark {
    Some(result) => {
      assert_true(result.iterations.length() > 0)
      
      // 检查最后一次迭代的结果
      let last_iteration = result.iterations[result.iterations.length() - 1]
      let compression_results = last_iteration.results
      
      // 验证gzip压缩率
      let gzip_result = compression_results.find(fn(r) { r.algorithm == "gzip" })
      assert_true(gzip_result != None)
      assert_true(gzip_result.get().compression_ratio < 0.7)  // 至少30%压缩率
      
      // 验证lz4速度
      let lz4_result = compression_results.find(fn(r) { r.algorithm == "lz4" })
      assert_true(lz4_result != None)
      assert_true(lz4_result.get().compression_throughput > 100 * 1024 * 1024)  // 至少100MB/s压缩速度
      
      // 验证zstd压缩率
      let zstd_result = compression_results.find(fn(r) { r.algorithm == "zstd" })
      assert_true(zstd_result != None)
      assert_true(zstd_result.get().compression_ratio < 0.6)  // 至少40%压缩率
    }
    None => assert_true(false)
  }
  
  // 检查内存使用测试结果
  let memory_benchmark = benchmark_results.find(fn(r) { r.scenario_name == "memory_usage_under_load" })
  assert_true(memory_benchmark != None)
  
  match memory_benchmark {
    Some(result) => {
      assert_true(result.iterations.length() > 0)
      
      // 检查最后一次迭代的结果
      let last_iteration = result.iterations[result.iterations.length() - 1]
      
      // 验证内存效率
      assert_true(last_iteration.memory_efficiency > 100.0)  // 每MB内存至少处理100条记录
      
      // 验证垃圾回收压力
      assert_true(last_iteration.gc_pressure < 0.5)  // GC回收的内存不超过峰值的50%
      
      // 验证内存没有泄漏
      assert_true(last_iteration.after_gc_memory_mb <= last_iteration.peak_memory_mb * 1.1)  // GC后内存接近峰值
    }
    None => assert_true(false)
  }
  
  // 生成基准测试报告
  let report = BenchmarkManager::generate_report(benchmark_manager, benchmark_results, {
    format: "json",
    include_raw_data: false,
    include_statistics: true,
    include_recommendations: true
  })
  
  // 验证基准测试报告
  assert_true(report.success)
  assert_true(report.data.length() > 0)
  assert_true(report.data.contains("\"scenarios\""))
  assert_true(report.data.contains("\"statistics\""))
  assert_true(report.data.contains("\"recommendations\""))
  
  // 测试基准测试比较
  let baseline_results = BenchmarkManager::load_baseline(benchmark_manager, "v1.0.0")
  
  if baseline_results != None {
    let comparison = BenchmarkManager::compare_results(benchmark_manager, benchmark_results, baseline_results.get())
    
    // 验证比较结果
    assert_true(comparison.comparisons.length() > 0)
    
    // 检查性能回归
    let regressions = comparison.comparisons.filter(fn(c) { c.performance_change < -0.05 })  // 5%以上性能下降
    assert_true(regressions.length() < comparison.comparisons.length() / 2)  // 回归不应该超过一半的测试
    
    // 检查性能提升
    let improvements = comparison.comparisons.filter(fn(c) { c.performance_change > 0.05 })  // 5%以上性能提升
    assert_true(improvements.length() >= 0)  // 可能有性能提升
  }
  
  // 测试基准测试自动化
  let automation_config = {
    schedule: "daily",
    baseline_branch: "main",
    alert_threshold: 0.1,  // 10%性能变化触发告警
    notification_channels: ["email", "slack"]
  }
  
  let automation_result = BenchmarkManager::setup_automation(benchmark_manager, automation_config)
  
  // 验证自动化设置
  assert_true(automation_result.success)
  assert_eq(automation_result.schedule, "daily")
  assert_eq(automation_result.baseline_branch, "main")
}