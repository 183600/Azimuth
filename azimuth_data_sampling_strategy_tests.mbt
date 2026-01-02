// Azimuth Data Sampling Strategy Tests
// 数据采样策略测试用例 - 专注于各种数据采样策略的实现和优化

// Test 1: 基础采样策略实现和验证
test "basic sampling strategy implementation and validation" {
  // 创建采样策略管理器
  let sampling_manager = SamplingStrategyManager::new()
  
  // 配置采样参数
  SamplingManager::configure(sampling_manager, [
    ("default.sampling.rate", FloatValue(0.1)), // 默认10%采样率
    ("max.sample.size", IntValue(10000)),
    ("sampling.random.seed", IntValue(42)),
    ("adaptive.sampling.enabled", BoolValue(true))
  ])
  
  // 创建测试数据集
  let test_dataset = TelemetryDataset::new("sampling_test")
  
  // 生成测试数据
  let base_time = Time::now() - (24 * 60 * 60 * 1000) // 24小时前
  
  for i in 0..<5000 {
    let timestamp = base_time + (i * 10 * 1000) // 每10秒一个数据点
    
    let attributes = [
      ("service", StringValue(["api", "database", "cache", "queue"][i % 4])),
      ("environment", StringValue(["prod", "staging", "dev"][i % 3])),
      ("region", StringValue(["us-east", "us-west", "eu-west"][i % 3])),
      ("cpu_usage", FloatValue(20.0 + (Math::random() * 60.0))),
      ("memory_usage", FloatValue(30.0 + (Math::random() * 50.0))),
      ("response_time", FloatValue(10.0 + (Math::random() * 500.0))),
      ("error_count", IntValue((Math::random() * 10).to_int())),
      ("request_count", IntValue((Math::random() * 1000).to_int()))
    ]
    
    let data_point = TelemetryDataPoint::new_with_timestamp(
      "telemetry_" + i.to_string(),
      attributes,
      timestamp
    )
    
    test_dataset.add(data_point)
  }
  
  // 验证数据集大小
  assert_eq(test_dataset.size(), 5000)
  
  // 测试随机采样策略
  let random_sampling = RandomSamplingStrategy::new(0.1) // 10%采样率
  
  let random_sample_start = Time::now()
  let random_sample = SamplingManager::apply_strategy(
    sampling_manager,
    test_dataset,
    random_sampling
  )
  let random_sample_end = Time::now()
  let random_sample_duration = random_sample_end - random_sample_start
  
  // 验证随机采样结果
  assert_true(random_sample_duration < 1000) // 采样应在1秒内完成
  assert_true(random_sample.size() > 0)
  
  // 验证采样率
  let actual_rate = random_sample.size().to_float() / test_dataset.size().to_float()
  assert_true(Math::abs(actual_rate - 0.1) < 0.02) // 实际采样率应在10%±2%范围内
  
  // 测试系统采样策略
  let systematic_sampling = SystematicSamplingStrategy::new(0.1) // 10%采样率
  
  let systematic_sample = SamplingManager::apply_strategy(
    sampling_manager,
    test_dataset,
    systematic_sampling
  )
  
  // 验证系统采样结果
  assert_true(systematic_sample.size() > 0)
  
  let systematic_rate = systematic_sample.size().to_float() / test_dataset.size().to_float()
  assert_true(Math::abs(systematic_rate - 0.1) < 0.02)
  
  // 验证系统采样的规律性
  let systematic_indices = systematic_sample.map(fn(point) { 
    TelemetryDataPoint::index(point)
  })
  
  // 系统采样应该有规律的间隔
  let mut intervals = []
  for i in 1..<systematic_indices.length() {
    let interval = systematic_indices[i] - systematic_indices[i-1]
    intervals = intervals.push(interval)
  }
  
  // 计算间隔的标准差，应该很小
  let avg_interval = intervals.reduce(0, fn(acc, val) { acc + val }) / intervals.length()
  let variance = intervals.reduce(0.0, fn(acc, val) { 
    acc + Math::pow(val.to_float() - avg_interval.to_float(), 2.0) 
  }) / intervals.length()
  let std_dev = Math::sqrt(variance)
  
  assert_true(std_dev < 2.0) // 标准差应该很小
  
  // 测试分层采样策略
  let strata_definition = [
    Stratum::new("service", "api", 0.15), // API服务15%采样率
    Stratum::new("service", "database", 0.2), // 数据库20%采样率
    Stratum::new("service", "cache", 0.1), // 缓存10%采样率
    Stratum::new("service", "queue", 0.05) // 队列5%采样率
  ]
  
  let stratified_sampling = StratifiedSamplingStrategy::new(strata_definition)
  
  let stratified_sample = SamplingManager::apply_strategy(
    sampling_manager,
    test_dataset,
    stratified_sampling
  )
  
  // 验证分层采样结果
  assert_true(stratified_sample.size() > 0)
  
  // 验证每层的采样率
  for stratum in strata_definition {
    let stratum_name = Stratum::attribute(stratum) + ":" + Stratum::value(stratum)
    let expected_rate = Stratum::sampling_rate(stratum)
    
    let stratum_data = stratified_sample.filter(fn(point) {
      match TelemetryDataPoint::get_attribute(point, Stratum::attribute(stratum)) {
        Some(StringValue(value)) => value == Stratum::value(stratum)
        _ => false
      }
    })
    
    let total_stratum_data = test_dataset.filter(fn(point) {
      match TelemetryDataPoint::get_attribute(point, Stratum::attribute(stratum)) {
        Some(StringValue(value)) => value == Stratum::value(stratum)
        _ => false
      }
    })
    
    if (total_stratum_data.size() > 0) {
      let actual_stratum_rate = stratum_data.size().to_float() / total_stratum_data.size().to_float()
      assert_true(Math::abs(actual_stratum_rate - expected_rate) < 0.05) // 每层误差在5%内
    }
  }
  
  // 测试聚类采样策略
  let cluster_definition = [
    Cluster::new("environment", "prod", 0.2), // 生产环境20%采样率
    Cluster::new("environment", "staging", 0.5), // 预发布环境50%采样率
    Cluster::new("environment", "dev", 0.8) // 开发环境80%采样率
  ]
  
  let cluster_sampling = ClusterSamplingStrategy::new(cluster_definition)
  
  let cluster_sample = SamplingManager::apply_strategy(
    sampling_manager,
    test_dataset,
    cluster_sampling
  )
  
  // 验证聚类采样结果
  assert_true(cluster_sample.size() > 0)
  
  // 验证每个聚类的采样率
  for cluster in cluster_definition {
    let cluster_name = Cluster::attribute(cluster) + ":" + Cluster::value(cluster)
    let expected_rate = Cluster::sampling_rate(cluster)
    
    let cluster_data = cluster_sample.filter(fn(point) {
      match TelemetryDataPoint::get_attribute(point, Cluster::attribute(cluster)) {
        Some(StringValue(value)) => value == Cluster::value(cluster)
        _ => false
      }
    })
    
    let total_cluster_data = test_dataset.filter(fn(point) {
      match TelemetryDataPoint::get_attribute(point, Cluster::attribute(cluster)) {
        Some(StringValue(value)) => value == Cluster::value(cluster)
        _ => false
      }
    })
    
    if (total_cluster_data.size() > 0) {
      let actual_cluster_rate = cluster_data.size().to_float() / total_cluster_data.size().to_float()
      assert_true(Math::abs(actual_cluster_rate - expected_rate) < 0.1) // 每个聚类误差在10%内
    }
  }
}

// Test 2: 自适应采样策略
test "adaptive sampling strategies" {
  // 创建自适应采样管理器
  let adaptive_sampling_manager = AdaptiveSamplingManager::new()
  
  // 配置自适应采样参数
  AdaptiveSamplingManager::configure(adaptive_sampling_manager, [
    ("base.sampling.rate", FloatValue(0.1)),
    ("max.sampling.rate", FloatValue(1.0)),
    ("min.sampling.rate", FloatValue(0.01)),
    ("adaptation.threshold", FloatValue(0.05)),
    ("evaluation.window.minutes", IntValue(10)
  ])
  
  // 创建动态遥测数据集
  let dynamic_dataset = TelemetryDataset::new("dynamic_telemetry")
  
  // 生成具有不同特征的动态数据
  let base_time = Time::now() - (2 * 60 * 60 * 1000) // 2小时前
  
  for i in 0::<3000 {
    let timestamp = base_time + (i * 2 * 1000) // 每2秒一个数据点
    
    // 模拟不同的数据特征
    let service_type = ["api", "database", "cache", "queue"][i % 4]
    let is_high_priority = i % 10 == 0 // 10%高优先级数据
    let is_error = i % 50 == 0 // 2%错误数据
    let is_high_latency = i % 25 == 0 // 4%高延迟数据
    
    let attributes = [
      ("service", StringValue(service_type)),
      ("priority", StringValue(if (is_high_priority) { "high" } else { "normal" })),
      ("status", StringValue(if (is_error) { "error" } else { "success" })),
      ("latency_ms", IntValue(if (is_high_latency) { 1000 + (Math::random() * 2000).to_int() } else { 50 + (Math::random() * 200).to_int() })),
      ("cpu_usage", FloatValue(20.0 + (Math::random() * 60.0))),
      ("memory_usage", FloatValue(30.0 + (Math::random() * 50.0)))
    ]
    
    let data_point = TelemetryDataPoint::new_with_timestamp(
      "dynamic_" + i.to_string(),
      attributes,
      timestamp
    )
    
    dynamic_dataset.add(data_point)
  }
  
  // 测试基于优先级的自适应采样
  let priority_adaptive_strategy = PriorityAdaptiveSamplingStrategy::new(
    0.1, // 基础采样率
    [
      PriorityRule::new("high", 0.8), // 高优先级80%采样率
      PriorityRule::new("normal", 0.05) // 普通优先级5%采样率
    ]
  )
  
  let priority_sample = AdaptiveSamplingManager::apply_adaptive_strategy(
    adaptive_sampling_manager,
    dynamic_dataset,
    priority_adaptive_strategy
  )
  
  // 验证优先级自适应采样结果
  assert_true(priority_sample.size() > 0)
  
  // 验证高优先级数据的采样率
  let high_priority_data = priority_sample.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "priority") {
      Some(StringValue(priority)) => priority == "high"
      _ => false
    }
  })
  
  let total_high_priority = dynamic_dataset.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "priority") {
      Some(StringValue(priority)) => priority == "high"
      _ => false
    }
  })
  
  if (total_high_priority.size() > 0) {
    let high_priority_rate = high_priority_data.size().to_float() / total_high_priority.size().to_float()
    assert_true(Math::abs(high_priority_rate - 0.8) < 0.1) // 高优先级采样率应在80%±10%范围内
  }
  
  // 验证普通优先级数据的采样率
  let normal_priority_data = priority_sample.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "priority") {
      Some(StringValue(priority)) => priority == "normal"
      _ => false
    }
  })
  
  let total_normal_priority = dynamic_dataset.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "priority") {
      Some(StringValue(priority)) => priority == "normal"
      _ => false
    }
  })
  
  if (total_normal_priority.size() > 0) {
    let normal_priority_rate = normal_priority_data.size().to_float() / total_normal_priority.size().to_float()
    assert_true(Math::abs(normal_priority_rate - 0.05) < 0.02) // 普通优先级采样率应在5%±2%范围内
  }
  
  // 测试基于错误率的自适应采样
  let error_adaptive_strategy = ErrorAdaptiveSamplingStrategy::new(
    0.1, // 基础采样率
    0.5, // 错误数据采样率
    0.05 // 正常数据采样率
  )
  
  let error_sample = AdaptiveSamplingManager::apply_adaptive_strategy(
    adaptive_sampling_manager,
    dynamic_dataset,
    error_adaptive_strategy
  )
  
  // 验证错误自适应采样结果
  assert_true(error_sample.size() > 0)
  
  // 验证错误数据的采样率
  let error_data = error_sample.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "status") {
      Some(StringValue(status)) => status == "error"
      _ => false
    }
  })
  
  let total_error_data = dynamic_dataset.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "status") {
      Some(StringValue(status)) => status == "error"
      _ => false
    }
  })
  
  if (total_error_data.size() > 0) {
    let error_rate = error_data.size().to_float() / total_error_data.size().to_float()
    assert_true(Math::abs(error_rate - 0.5) < 0.1) // 错误数据采样率应在50%±10%范围内
  }
  
  // 测试基于延迟的自适应采样
  let latency_adaptive_strategy = LatencyAdaptiveSamplingStrategy::new(
    0.1, // 基础采样率
    1000, // 高延迟阈值(ms)
    0.6, // 高延迟数据采样率
    0.02  // 正常延迟数据采样率
  )
  
  let latency_sample = AdaptiveSamplingManager::apply_adaptive_strategy(
    adaptive_sampling_manager,
    dynamic_dataset,
    latency_adaptive_strategy
  )
  
  // 验证延迟自适应采样结果
  assert_true(latency_sample.size() > 0)
  
  // 验证高延迟数据的采样率
  let high_latency_data = latency_sample.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "latency_ms") {
      Some(IntValue(latency)) => latency >= 1000
      _ => false
    }
  })
  
  let total_high_latency = dynamic_dataset.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "latency_ms") {
      Some(IntValue(latency)) => latency >= 1000
      _ => false
    }
  })
  
  if (total_high_latency.size() > 0) {
    let high_latency_rate = high_latency_data.size().to_float() / total_high_latency.size().to_float()
    assert_true(Math::abs(high_latency_rate - 0.6) < 0.1) // 高延迟数据采样率应在60%±10%范围内
  }
  
  // 测试流量自适应采样策略
  let traffic_adaptive_strategy = TrafficAdaptiveSamplingStrategy::new(
    0.1, // 基础采样率
    100, // 低流量阈值
    0.2, // 低流量采样率
    1000, // 高流量阈值
    0.05  // 高流量采样率
  )
  
  // 模拟不同流量水平的数据
  let low_traffic_dataset = TelemetryDataset::new("low_traffic")
  let high_traffic_dataset = TelemetryDataset::new("high_traffic")
  
  // 低流量数据（100个数据点）
  for i in 0..<100 {
    let timestamp = base_time + (i * 60 * 1000) // 每分钟一个数据点
    let data_point = TelemetryDataPoint::new_with_timestamp(
      "low_traffic_" + i.to_string(),
      [("service", StringValue("api"))],
      timestamp
    )
    low_traffic_dataset.add(data_point)
  }
  
  // 高流量数据（1000个数据点）
  for i in 0..<1000 {
    let timestamp = base_time + (i * 6 * 1000) // 每6秒一个数据点
    let data_point = TelemetryDataPoint::new_with_timestamp(
      "high_traffic_" + i.to_string(),
      [("service", StringValue("api"))],
      timestamp
    )
    high_traffic_dataset.add(data_point)
  }
  
  let low_traffic_sample = AdaptiveSamplingManager::apply_adaptive_strategy(
    adaptive_sampling_manager,
    low_traffic_dataset,
    traffic_adaptive_strategy
  )
  
  let high_traffic_sample = AdaptiveSamplingManager::apply_adaptive_strategy(
    adaptive_sampling_manager,
    high_traffic_dataset,
    traffic_adaptive_strategy
  )
  
  // 验证流量自适应采样结果
  let low_traffic_rate = low_traffic_sample.size().to_float() / low_traffic_dataset.size().to_float()
  let high_traffic_rate = high_traffic_sample.size().to_float() / high_traffic_dataset.size().to_float()
  
  assert_true(Math::abs(low_traffic_rate - 0.2) < 0.05) // 低流量采样率应在20%±5%范围内
  assert_true(Math::abs(high_traffic_rate - 0.05) < 0.02) // 高流量采样率应在5%±2%范围内
  assert_true(low_traffic_rate > high_traffic_rate) // 低流量采样率应高于高流量采样率
}

// Test 3: 智能采样策略
test "intelligent sampling strategies" {
  // 创建智能采样管理器
  let intelligent_sampling_manager = IntelligentSamplingManager::new()
  
  // 配置智能采样参数
  IntelligentSamplingManager::configure(intelligent_sampling_manager, [
    ("ml.model.enabled", BoolValue(true)),
    ("anomaly.detection.enabled", BoolValue(true)),
    ("pattern.recognition.enabled", BoolValue(true)),
    ("learning.rate", FloatValue(0.01)),
    ("model.update.interval", IntValue(3600) // 1小时
  ])
  
  // 创建复杂遥测数据集
  let complex_dataset = TelemetryDataset::new("complex_telemetry")
  
  // 生成具有复杂模式的数据
  let base_time = Time::now() - (4 * 60 * 60 * 1000) // 4小时前
  
  for i in 0::<4000 {
    let timestamp = base_time + (i * 3 * 1000) // 每3秒一个数据点
    
    // 模拟复杂的数据模式
    let hour_of_day = (timestamp / (60 * 60 * 1000)) % 24
    let is_business_hours = hour_of_day >= 9 && hour_of_day <= 17
    
    let service_type = ["api", "database", "cache", "queue", "search"][i % 5]
    let is_weekend = (timestamp / (24 * 60 * 60 * 1000)) % 7 >= 5
    
    // 模拟异常模式
    let is_anomaly = if (i % 100 == 0) {
      true // 1%的异常数据
    } else if (is_business_hours && !is_weekend && Math::random() < 0.1) {
      true // 工作时间10%异常率
    } else {
      false
    }
    
    let cpu_usage = if (is_anomaly) {
      80.0 + (Math::random() * 20.0) // 异常高CPU使用率
    } else {
      20.0 + (Math::random() * 40.0) // 正常CPU使用率
    }
    
    let memory_usage = if (is_anomaly) {
      85.0 + (Math::random() * 15.0) // 异常高内存使用率
    } else {
      30.0 + (Math::random() * 40.0) // 正常内存使用率
    }
    
    let response_time = if (is_anomaly) {
      1000.0 + (Math::random() * 2000.0) // 异常高响应时间
    } else {
      50.0 + (Math::random() * 200.0) // 正常响应时间
    }
    
    let attributes = [
      ("service", StringValue(service_type)),
      ("hour_of_day", IntValue(hour_of_day)),
      ("is_business_hours", BoolValue(is_business_hours)),
      ("is_weekend", BoolValue(is_weekend)),
      ("is_anomaly", BoolValue(is_anomaly)),
      ("cpu_usage", FloatValue(cpu_usage)),
      ("memory_usage", FloatValue(memory_usage)),
      ("response_time", FloatValue(response_time)),
      ("error_count", IntValue(if (is_anomaly) { (Math::random() * 10).to_int() } else { (Math::random() * 2).to_int() })),
      ("request_count", IntValue(if (is_business_hours) { (Math::random() * 1000).to_int() } else { (Math::random() * 200).to_int() }))
    ]
    
    let data_point = TelemetryDataPoint::new_with_timestamp(
      "complex_" + i.to_string(),
      attributes,
      timestamp
    )
    
    complex_dataset.add(data_point)
  }
  
  // 测试异常检测采样策略
  let anomaly_sampling_strategy = AnomalyDetectionSamplingStrategy::new(
    0.1, // 基础采样率
    0.8, // 异常数据采样率
    0.05, // 正常数据采样率
    ["cpu_usage", "memory_usage", "response_time"] // 用于异常检测的指标
  )
  
  let anomaly_sample = IntelligentSamplingManager::apply_intelligent_strategy(
    intelligent_sampling_manager,
    complex_dataset,
    anomaly_sampling_strategy
  )
  
  // 验证异常检测采样结果
  assert_true(anomaly_sample.size() > 0)
  
  // 验证异常数据的采样率
  let anomaly_data = anomaly_sample.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "is_anomaly") {
      Some(BoolValue(is_anomaly)) => is_anomaly
      _ => false
    }
  })
  
  let total_anomaly_data = complex_dataset.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "is_anomaly") {
      Some(BoolValue(is_anomaly)) => is_anomaly
      _ => false
    }
  })
  
  if (total_anomaly_data.size() > 0) {
    let anomaly_rate = anomaly_data.size().to_float() / total_anomaly_data.size().to_float()
    assert_true(Math::abs(anomaly_rate - 0.8) < 0.1) // 异常数据采样率应在80%±10%范围内
  }
  
  // 测试模式识别采样策略
  let pattern_sampling_strategy = PatternRecognitionSamplingStrategy::new(
    0.1, // 基础采样率
    [
      PatternRule::new("business_hours", "is_business_hours", true, 0.15), // 工作时间15%采样率
      PatternRule::new("weekend", "is_weekend", true, 0.05), // 周末5%采样率
      PatternRule::new("high_traffic_service", "service", "api", 0.2) // API服务20%采样率
    ]
  )
  
  let pattern_sample = IntelligentSamplingManager::apply_intelligent_strategy(
    intelligent_sampling_manager,
    complex_dataset,
    pattern_sampling_strategy
  )
  
  // 验证模式识别采样结果
  assert_true(pattern_sample.size() > 0)
  
  // 验证工作时间数据的采样率
  let business_hours_data = pattern_sample.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "is_business_hours") {
      Some(BoolValue(is_business_hours)) => is_business_hours
      _ => false
    }
  })
  
  let total_business_hours = complex_dataset.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "is_business_hours") {
      Some(BoolValue(is_business_hours)) => is_business_hours
      _ => false
    }
  })
  
  if (total_business_hours.size() > 0) {
    let business_hours_rate = business_hours_data.size().to_float() / total_business_hours.size().to_float()
    assert_true(Math::abs(business_hours_rate - 0.15) < 0.05) // 工作时间采样率应在15%±5%范围内
  }
  
  // 测试机器学习采样策略
  let ml_sampling_strategy = MLSamplingStrategy::new(
    0.1, // 基础采样率
    ["cpu_usage", "memory_usage", "response_time", "error_count"], // 特征
    "importance" // 目标变量
  )
  
  // 训练ML模型
  let training_data = complex_dataset.slice(0, 3000) // 前3000个数据点用于训练
  let test_data = complex_dataset.slice(3000, 1000) // 后1000个数据点用于测试
  
  let model_training_result = IntelligentSamplingManager::train_sampling_model(
    intelligent_sampling_manager,
    training_data,
    ml_sampling_strategy
  )
  
  assert_true(model_training_result)
  
  // 应用ML采样策略
  let ml_sample = IntelligentSamplingManager::apply_intelligent_strategy(
    intelligent_sampling_manager,
    test_data,
    ml_sampling_strategy
  )
  
  // 验证ML采样结果
  assert_true(ml_sample.size() > 0)
  
  // 验证ML采样的效果
  let ml_sample_importance = ml_sample.map(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "importance") {
      Some(FloatValue(importance)) => importance
      _ => 0.0
    }
  })
  
  let test_data_importance = test_data.map(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "importance") {
      Some(FloatValue(importance)) => importance
      _ => 0.0
    }
  })
  
  let avg_ml_importance = ml_sample_importance.reduce(0.0, fn(acc, val) { acc + val }) / ml_sample_importance.length()
  let avg_test_importance = test_data_importance.reduce(0.0, fn(acc, val) { acc + val }) / test_data_importance.length()
  
  // ML采样应该倾向于选择重要性更高的数据
  assert_true(avg_ml_importance >= avg_test_importance)
  
  // 测试混合智能采样策略
  let hybrid_strategy = HybridIntelligentSamplingStrategy::new(
    0.1, // 基础采样率
    [
      anomaly_sampling_strategy,
      pattern_sampling_strategy,
      ml_sampling_strategy
    ],
    [0.4, 0.3, 0.3] // 权重
  )
  
  let hybrid_sample = IntelligentSamplingManager::apply_intelligent_strategy(
    intelligent_sampling_manager,
    complex_dataset,
    hybrid_strategy
  )
  
  // 验证混合智能采样结果
  assert_true(hybrid_sample.size() > 0)
  
  // 验证混合采样结合了多种策略的优势
  let hybrid_anomaly_data = hybrid_sample.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "is_anomaly") {
      Some(BoolValue(is_anomaly)) => is_anomaly
      _ => false
    }
  })
  
  let total_anomaly_data = complex_dataset.filter(fn(point) {
    match TelemetryDataPoint::get_attribute(point, "is_anomaly") {
      Some(BoolValue(is_anomaly)) => is_anomaly
      _ => false
    }
  })
  
  if (total_anomaly_data.size() > 0) {
    let hybrid_anomaly_rate = hybrid_anomaly_data.size().to_float() / total_anomaly_data.size().to_float()
    assert_true(hybrid_anomaly_rate > 0.3) // 混合策略应该保持较高的异常数据采样率
  }
}

// Test 4: 采样策略性能优化
test "sampling strategy performance optimization" {
  // 创建采样性能优化器
  let sampling_optimizer = SamplingPerformanceOptimizer::new()
  
  // 配置优化参数
  SamplingOptimizer::configure(sampling_optimizer, [
    ("optimization.goal", StringValue("balanced")), // balanced, accuracy, efficiency
    ("max.processing.time.ms", IntValue(1000)),
    ("memory.limit.mb", IntValue(512)),
    ("parallel.processing", BoolValue(true))
  ])
  
  // 创建大型数据集用于性能测试
  let large_dataset = TelemetryDataset::new("performance_test")
  
  // 生成大量数据
  let base_time = Time::now() - (24 * 60 * 60 * 1000) // 24小时前
  
  for i in 0::<20000 {
    let timestamp = base_time + (i * 4 * 1000) // 每4秒一个数据点
    
    let attributes = [
      ("service", StringValue(["api", "database", "cache", "queue", "search", "analytics"][i % 6])),
      ("environment", StringValue(["prod", "staging", "dev", "test"][i % 4])),
      ("region", StringValue(["us-east", "us-west", "eu-west", "eu-central", "ap-southeast"][i % 5])),
      ("cpu_usage", FloatValue(20.0 + (Math::random() * 60.0))),
      ("memory_usage", FloatValue(30.0 + (Math::random() * 50.0))),
      ("response_time", FloatValue(10.0 + (Math::random() * 500.0))),
      ("error_count", IntValue((Math::random() * 10).to_int())),
      ("request_count", IntValue((Math::random() * 1000).to_int())),
      ("priority", StringValue(["high", "medium", "low"][i % 3]))
    ]
    
    let data_point = TelemetryDataPoint::new_with_timestamp(
      "perf_" + i.to_string(),
      attributes,
      timestamp
    )
    
    large_dataset.add(data_point)
  }
  
  // 验证数据集大小
  assert_eq(large_dataset.size(), 20000)
  
  // 测试不同采样策略的性能
  let sampling_strategies = [
    ("random", RandomSamplingStrategy::new(0.1)),
    ("systematic", SystematicSamplingStrategy::new(0.1)),
    ("stratified", StratifiedSamplingStrategy::new([
      Stratum::new("service", "api", 0.1),
      Stratum::new("service", "database", 0.1),
      Stratum::new("service", "cache", 0.1),
      Stratum::new("service", "queue", 0.1),
      Stratum::new("service", "search", 0.1),
      Stratum::new("service", "analytics", 0.1)
    ])),
    ("cluster", ClusterSamplingStrategy::new([
      Cluster::new("environment", "prod", 0.1),
      Cluster::new("environment", "staging", 0.1),
      Cluster::new("environment", "dev", 0.1),
      Cluster::new("environment", "test", 0.1)
    ]))
  ]
  
  let performance_results = []
  
  for (strategy_name, strategy) in sampling_strategies {
    // 测试串行性能
    let serial_start = Time::now()
    let serial_result = SamplingOptimizer::apply_strategy_serial(
      sampling_optimizer,
      large_dataset,
      strategy
    )
    let serial_end = Time::now()
    let serial_duration = serial_end - serial_start
    
    // 测试并行性能
    let parallel_start = Time::now()
    let parallel_result = SamplingOptimizer::apply_strategy_parallel(
      sampling_optimizer,
      large_dataset,
      strategy
    )
    let parallel_end = Time::now()
    let parallel_duration = parallel_end - parallel_start
    
    // 验证结果一致性
    assert_eq(serial_result.size(), parallel_result.size())
    
    // 记录性能结果
    performance_results = performance_results.push((
      strategy_name,
      serial_duration,
      parallel_duration,
      serial_result.size()
    ))
    
    // 验证性能要求
    assert_true(serial_duration < 5000) // 串行处理应在5秒内完成
    assert_true(parallel_duration < 2000) // 并行处理应在2秒内完成
    assert_true(parallel_duration <= serial_duration) // 并行应该更快或相等
  }
  
  // 分析性能结果
  for result in performance_results {
    let (strategy_name, serial_time, parallel_time, sample_size) = result
    
    // 计算加速比
    let speedup = serial_time.to_float() / parallel_time.to_float()
    
    // 验证采样率
    let actual_rate = sample_size.to_float() / large_dataset.size().to_float()
    assert_true(Math::abs(actual_rate - 0.1) < 0.02) // 采样率应在10%±2%范围内
    
    // 验证并行加速效果
    assert_true(speedup >= 1.0) // 加速比应该至少为1
  }
  
  // 测试内存优化采样
  let memory_optimized_strategy = MemoryOptimizedSamplingStrategy::new(
    0.1, // 采样率
    100  // 批处理大小
  )
  
  let memory_start = Time::now()
  let memory_result = SamplingOptimizer::apply_memory_optimized(
    sampling_optimizer,
    large_dataset,
    memory_optimized_strategy
  )
  let memory_end = Time::now()
  let memory_duration = memory_end - memory_start
  
  // 验证内存优化效果
  assert_true(memory_duration < 3000) // 内存优化应在3秒内完成
  assert_true(memory_result.size() > 0)
  
  // 测试缓存优化采样
  let cache_optimized_strategy = CacheOptimizedSamplingStrategy::new(
    0.1, // 采样率
    1000 // 缓存大小
  )
  
  // 第一次执行（无缓存）
  let cache_first_start = Time::now()
  let cache_first_result = SamplingOptimizer::apply_cache_optimized(
    sampling_optimizer,
    large_dataset,
    cache_optimized_strategy
  )
  let cache_first_end = Time::now()
  let cache_first_duration = cache_first_end - cache_first_start
  
  // 第二次执行（有缓存）
  let cache_second_start = Time::now()
  let cache_second_result = SamplingOptimizer::apply_cache_optimized(
    sampling_optimizer,
    large_dataset,
    cache_optimized_strategy
  )
  let cache_second_end = Time::now()
  let cache_second_duration = cache_second_end - cache_second_start
  
  // 验证缓存优化效果
  assert_eq(cache_first_result.size(), cache_second_result.size())
  assert_true(cache_second_duration <= cache_first_duration) // 缓存应该更快或相等
  
  // 测试自适应性能优化
  let adaptive_performance_strategy = AdaptivePerformanceSamplingStrategy::new(
    0.1, // 基础采样率
    1000, // 性能阈值(ms)
    0.05, // 低性能时降低采样率
    0.2   // 高性能时提高采样率
  )
  
  let adaptive_start = Time::now()
  let adaptive_result = SamplingOptimizer::apply_adaptive_performance(
    sampling_optimizer,
    large_dataset,
    adaptive_performance_strategy
  )
  let adaptive_end = Time::now()
  let adaptive_duration = adaptive_end - adaptive_start
  
  // 验证自适应性能优化效果
  assert_true(adaptive_duration < 2000) // 自适应应在2秒内完成
  assert_true(adaptive_result.size() > 0)
  
  // 生成性能优化报告
  let performance_report = SamplingOptimizer::generate_performance_report(
    sampling_optimizer,
    performance_results
  )
  
  // 验证性能报告
  assert_true(PerformanceReport::has_summary(performance_report))
  assert_true(PerformanceReport::has_bottlenecks(performance_report))
  assert_true(PerformanceReport::has_recommendations(performance_report))
  
  let summary = PerformanceReport::summary(performance_report)
  assert_true(Summary::has_average_speedup(summary))
  assert_true(Summary::has_best_strategy(summary))
  
  let average_speedup = Summary::average_speedup(summary)
  assert_true(average_speedup >= 1.0) // 平均加速比应该至少为1
}

// Test 5: 采样策略评估和比较
test "sampling strategy evaluation and comparison" {
  // 创建采样策略评估器
  let sampling_evaluator = SamplingStrategyEvaluator::new()
  
  // 配置评估参数
  SamplingEvaluator::configure(sampling_evaluator, [
    ("evaluation.metrics", StringValue("accuracy,precision,recall,f1")),
    ("cross.validation.folds", IntValue(5)),
    ("statistical.tests", StringValue("ks_test,chi_square_test")),
    ("confidence.level", FloatValue(0.95))
  ])
  
  // 创建评估数据集
  let evaluation_dataset = TelemetryDataset::new("evaluation_test")
  
  // 生成已知分布的评估数据
  let base_time = Time::now() - (7 * 24 * 60 * 60 * 1000) // 7天前
  
  for i in 0::<10000 {
    let timestamp = base_time + (i * 60 * 1000) // 每分钟一个数据点
    
    // 生成已知分布的数据
    let service_distribution = [0.4, 0.3, 0.2, 0.1] // api:40%, database:30%, cache:20%, queue:10%
    let service_type = if (i % 10 < 4) { "api" } 
                      else if (i % 10 < 7) { "database" }
                      else if (i % 10 < 9) { "cache" }
                      else { "queue" }
    
    let cpu_distribution = [0.2, 0.5, 0.3] // low:20%, medium:50%, high:30%
    let cpu_category = if (i % 10 < 2) { "low" }
                      else if (i % 10 < 7) { "medium" }
                      else { "high" }
    
    let cpu_usage = match cpu_category {
      "low" => 20.0 + (Math::random() * 10.0),
      "medium" => 40.0 + (Math::random() * 20.0),
      "high" => 70.0 + (Math::random() * 20.0),
      _ => 50.0
    }
    
    let attributes = [
      ("service", StringValue(service_type)),
      ("cpu_category", StringValue(cpu_category)),
      ("cpu_usage", FloatValue(cpu_usage)),
      ("memory_usage", FloatValue(30.0 + (Math::random() * 50.0))),
      ("response_time", FloatValue(10.0 + (Math::random() * 500.0))),
      ("error_count", IntValue((Math::random() * 10).to_int())),
      ("request_count", IntValue((Math::random() * 1000).to_int()))
    ]
    
    let data_point = TelemetryDataPoint::new_with_timestamp(
      "eval_" + i.to_string(),
      attributes,
      timestamp
    )
    
    evaluation_dataset.add(data_point)
  }
  
  // 定义要评估的采样策略
  let evaluation_strategies = [
    ("random_5", RandomSamplingStrategy::new(0.05)),
    ("random_10", RandomSamplingStrategy::new(0.1)),
    ("random_20", RandomSamplingStrategy::new(0.2)),
    ("systematic_10", SystematicSamplingStrategy::new(0.1)),
    ("stratified_service", StratifiedSamplingStrategy::new([
      Stratum::new("service", "api", 0.1),
      Stratum::new("service", "database", 0.1),
      Stratum::new("service", "cache", 0.1),
      Stratum::new("service", "queue", 0.1)
    ])),
    ("stratified_cpu", StratifiedSamplingStrategy::new([
      Stratum::new("cpu_category", "low", 0.1),
      Stratum::new("cpu_category", "medium", 0.1),
      Stratum::new("cpu_category", "high", 0.1)
    ])),
    ("cluster_env", ClusterSamplingStrategy::new([
      Cluster::new("environment", "prod", 0.1),
      Cluster::new("environment", "staging", 0.1),
      Cluster::new("environment", "dev", 0.1)
    ]))
  ]
  
  // 执行策略评估
  let evaluation_results = []
  
  for (strategy_name, strategy) in evaluation_strategies {
    // 执行采样
    let sample = SamplingEvaluator::apply_strategy_for_evaluation(
      sampling_evaluator,
      evaluation_dataset,
      strategy
    )
    
    // 计算评估指标
    let accuracy = SamplingEvaluator::calculate_accuracy(
      sampling_evaluator,
      evaluation_dataset,
      sample,
      ["service", "cpu_category"]
    )
    
    let precision = SamplingEvaluator::calculate_precision(
      sampling_evaluator,
      evaluation_dataset,
      sample,
      "cpu_usage"
    )
    
    let recall = SamplingEvaluator::calculate_recall(
      sampling_evaluator,
      evaluation_dataset,
      sample,
      "cpu_usage"
    )
    
    let f1_score = SamplingEvaluator::calculate_f1_score(precision, recall)
    
    // 执行统计测试
    let ks_test_result = SamplingEvaluator::perform_ks_test(
      sampling_evaluator,
      evaluation_dataset,
      sample,
      "cpu_usage"
    )
    
    let chi_square_result = SamplingEvaluator::perform_chi_square_test(
      sampling_evaluator,
      evaluation_dataset,
      sample,
      "service"
    )
    
    // 记录评估结果
    evaluation_results = evaluation_results.push((
      strategy_name,
      sample.size(),
      accuracy,
      precision,
      recall,
      f1_score,
      ks_test_result,
      chi_square_result
    ))
  }
  
  // 分析评估结果
  for result in evaluation_results {
    let (strategy_name, sample_size, accuracy, precision, recall, f1_score, ks_test, chi_square) = result
    
    // 验证采样率
    let actual_rate = sample_size.to_float() / evaluation_dataset.size().to_float()
    
    match strategy_name {
      "random_5" => assert_true(Math::abs(actual_rate - 0.05) < 0.01),
      "random_10" => assert_true(Math::abs(actual_rate - 0.1) < 0.02),
      "random_20" => assert_true(Math::abs(actual_rate - 0.2) < 0.03),
      _ => assert_true(Math::abs(actual_rate - 0.1) < 0.02) // 其他策略应该是10%左右
    }
    
    // 验证评估指标
    assert_true(accuracy >= 0.0 && accuracy <= 1.0)
    assert_true(precision >= 0.0 && precision <= 1.0)
    assert_true(recall >= 0.0 && recall <= 1.0)
    assert_true(f1_score >= 0.0 && f1_score <= 1.0)
    
    // 验证统计测试
    assert_true(KSTest::has_p_value(ks_test))
    assert_true(KSTest::has_statistic(ks_test))
    
    assert_true(ChiSquareTest::has_p_value(chi_square))
    assert_true(ChiSquareTest::has_statistic(chi_square))
    
    let ks_p_value = KSTest::p_value(ks_test)
    let chi_p_value = ChiSquareTest::p_value(chi_square)
    
    // p值应该在0-1范围内
    assert_true(ks_p_value >= 0.0 && ks_p_value <= 1.0)
    assert_true(chi_p_value >= 0.0 && chi_p_value <= 1.0)
  }
  
  // 比较策略性能
  let strategy_comparison = SamplingEvaluator::compare_strategies(
    sampling_evaluator,
    evaluation_results
  )
  
  // 验证策略比较结果
  assert_true(StrategyComparison::has_ranking(strategy_comparison))
  assert_true(StrategyComparison::has_best_accuracy(strategy_comparison))
  assert_true(StrategyComparison::has_best_f1_score(strategy_comparison))
  
  let ranking = StrategyComparison::ranking(strategy_comparison)
  assert_true(ranking.length() == evaluation_results.length())
  
  // 验证排名
  for i in 0..<ranking.length() - 1 {
    let current_score = ranking[i].score
    let next_score = ranking[i + 1].score
    assert_true(current_score >= next_score) // 分数应该递减
  }
  
  // 测试交叉验证评估
  let cross_validation_results = SamplingEvaluator::cross_validate_strategies(
    sampling_evaluator,
    evaluation_dataset,
    [
      evaluation_strategies[1], // random_10
      evaluation_strategies[4], // stratified_service
      evaluation_strategies[5]  // stratified_cpu
    ],
    5 // 5折交叉验证
  )
  
  // 验证交叉验证结果
  assert_true(CrossValidationResults::has_mean_accuracy(cross_validation_results))
  assert_true(CrossValidationResults::has_std_deviation(cross_validation_results))
  
  let mean_accuracy = CrossValidationResults::mean_accuracy(cross_validation_results)
  let std_deviation = CrossValidationResults::std_deviation(cross_validation_results)
  
  assert_true(mean_accuracy >= 0.0 && mean_accuracy <= 1.0)
  assert_true(std_deviation >= 0.0)
  
  // 生成评估报告
  let evaluation_report = SamplingEvaluator::generate_evaluation_report(
    sampling_evaluator,
    evaluation_results,
    strategy_comparison,
    cross_validation_results
  )
  
  // 验证评估报告
  assert_true(EvaluationReport::has_summary(evaluation_report))
  assert_true(EvaluationReport::has_detailed_metrics(evaluation_report))
  assert_true(EvaluationReport::has_statistical_tests(evaluation_report))
  assert_true(EvaluationReport::has_recommendations(evaluation_report))
  
  let summary = EvaluationReport::summary(evaluation_report)
  assert_true(Summary::has_best_overall_strategy(summary))
  assert_true(Summary::has_performance_ranking(summary))
  
  let recommendations = EvaluationReport::recommendations(evaluation_report)
  assert_true(Recommendations::has_strategy_recommendations(recommendations))
  assert_true(Recommendations::has_sampling_rate_recommendations(recommendations))
}