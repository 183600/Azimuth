// Azimuth 遥测数据自适应采样策略测试用例
// 专注于遥测系统中的自适应采样策略和动态调整机制

// 测试1: 基于负载的自适应采样
test "基于负载的自适应采样" {
  // 模拟不同负载情况下的遥测数据流
  let telemetry_stream = [
    { timestamp: 1640995200, service: "auth", load_level: "low", request_count: 50, error_rate: 0.1 },
    { timestamp: 1640995260, service: "auth", load_level: "low", request_count: 55, error_rate: 0.1 },
    { timestamp: 1640995320, service: "auth", load_level: "medium", request_count: 200, error_rate: 0.2 },
    { timestamp: 1640995380, service: "auth", load_level: "medium", request_count: 250, error_rate: 0.3 },
    { timestamp: 1640995440, service: "auth", load_level: "high", request_count: 800, error_rate: 0.5 },
    { timestamp: 1640995500, service: "auth", load_level: "high", request_count: 900, error_rate: 0.8 },
    { timestamp: 1640995560, service: "auth", load_level: "critical", request_count: 1500, error_rate: 2.0 },
    { timestamp: 1640995620, service: "auth", load_level: "critical", request_count: 1800, error_rate: 3.5 },
    { timestamp: 1640995680, service: "auth", load_level: "high", request_count: 750, error_rate: 0.6 },
    { timestamp: 1640995740, service: "auth", load_level: "medium", request_count: 180, error_rate: 0.2 }
  ]
  
  // 定义负载-采样率映射策略
  let load_sampling_strategy = {
    "low": { base_rate: 1.0, error_multiplier: 1.0 },
    "medium": { base_rate: 0.5, error_multiplier: 1.5 },
    "high": { base_rate: 0.2, error_multiplier: 2.0 },
    "critical": { base_rate: 0.1, error_multiplier: 3.0 }
  }
  
  // 自适应采样算法
  let adaptive_sampling = fn(load_level, request_count, error_rate) {
    let strategy = load_sampling_strategy.get(load_level).unwrap()
    let base_rate = strategy.base_rate
    
    // 基于错误率调整采样率
    let error_adjustment = if error_rate > 1.0 {
      strategy.error_multiplier * (1.0 + error_rate / 10.0)
    } else if error_rate > 0.5 {
      strategy.error_multiplier * (1.0 + error_rate / 5.0)
    } else {
      1.0
    }
    
    // 基于请求量微调
    let volume_adjustment = if request_count > 1000 {
      0.8 // 高流量时降低采样率
    } else if request_count < 100 {
      1.2 // 低流量时提高采样率
    } else {
      1.0
    }
    
    // 计算最终采样率
    let final_rate = base_rate * error_adjustment * volume_adjustment
    
    // 确保采样率在合理范围内
    if final_rate > 1.0 { 1.0 }
    else if final_rate < 0.01 { 0.01 }
    else { final_rate }
  }
  
  // 应用自适应采样策略
  let mut sampling_results = []
  for data_point in telemetry_stream {
    let sampling_rate = adaptive_sampling(
      data_point.load_level, 
      data_point.request_count, 
      data_point.error_rate
    )
    
    // 模拟采样决策
    let random_value = 0.5 // 固定值用于测试
    let is_sampled = random_value <= sampling_rate
    
    sampling_results = sampling_results.push({
      timestamp: data_point.timestamp,
      load_level: data_point.load_level,
      request_count: data_point.request_count,
      error_rate: data_point.error_rate,
      sampling_rate: sampling_rate,
      is_sampled: is_sampled
    })
  }
  
  // 验证自适应采样结果
  assert_eq(sampling_results.length(), 10)
  
  // 验证低负载时的采样率
  let low_load_samples = sampling_results.filter(fn(r) { r.load_level == "low" })
  assert_eq(low_load_samples.length(), 2)
  assert_true(low_load_samples.all(fn(r) { r.sampling_rate >= 0.9 })) // 低负载时高采样率
  
  // 验证中等负载时的采样率
  let medium_load_samples = sampling_results.filter(fn(r) { r.load_level == "medium" })
  assert_eq(medium_load_samples.length(), 3)
  assert_true(medium_load_samples.all(fn(r) { r.sampling_rate >= 0.4 && r.sampling_rate <= 0.8 }))
  
  // 验证高负载时的采样率
  let high_load_samples = sampling_results.filter(fn(r) { r.load_level == "high" })
  assert_eq(high_load_samples.length(), 3)
  assert_true(high_load_samples.all(fn(r) { r.sampling_rate <= 0.5 })) // 高负载时低采样率
  
  // 验证临界负载时的采样率
  let critical_load_samples = sampling_results.filter(fn(r) { r.load_level == "critical" })
  assert_eq(critical_load_samples.length(), 2)
  assert_true(critical_load_samples.all(fn(r) { r.sampling_rate <= 0.3 })) // 临界负载时最低采样率
  
  // 验证错误率对采样率的影响
  let high_error_samples = sampling_results.filter(fn(r) { r.error_rate > 1.0 })
  assert_eq(high_error_samples.length(), 2)
  assert_true(high_error_samples.all(fn(r) { r.sampling_rate > 0.2 })) // 高错误率时提高采样率
}

// 测试2: 基于服务重要性的分层采样
test "基于服务重要性的分层采样" {
  // 模拟不同重要性的服务遥测数据
  let service_telemetry = [
    { service: "auth-service", tier: "critical", request_count: 100, latency: 50.0 },
    { service: "payment-service", tier: "critical", request_count: 80, latency: 120.0 },
    { service: "user-service", tier: "high", request_count: 200, latency: 30.0 },
    { service: "order-service", tier: "high", request_count: 150, latency: 45.0 },
    { service: "notification-service", tier: "medium", request_count: 300, latency: 25.0 },
    { service: "analytics-service", tier: "medium", request_count: 50, latency: 200.0 },
    { service: "logging-service", tier: "low", request_count: 500, latency: 15.0 },
    { service: "cache-service", tier: "low", request_count: 1000, latency: 5.0 }
  ]
  
  // 定义服务分层采样策略
  let tier_sampling_strategy = {
    "critical": { base_rate: 1.0, latency_threshold: 100.0, latency_multiplier: 1.5 },
    "high": { base_rate: 0.8, latency_threshold: 50.0, latency_multiplier: 1.3 },
    "medium": { base_rate: 0.5, latency_threshold: 30.0, latency_multiplier: 1.2 },
    "low": { base_rate: 0.2, latency_threshold: 20.0, latency_multiplier: 1.1 }
  }
  
  // 分层采样算法
  let tier_based_sampling = fn(service_tier, request_count, latency) {
    let strategy = tier_sampling_strategy.get(service_tier).unwrap()
    let base_rate = strategy.base_rate
    
    // 基于延迟调整采样率
    let latency_adjustment = if latency > strategy.latency_threshold {
      strategy.latency_multiplier * (1.0 + (latency - strategy.latency_threshold) / 100.0)
    } else {
      1.0
    }
    
    // 基于请求量调整
    let volume_adjustment = if request_count > 500 {
      0.7 // 高流量服务降低采样率
    } else if request_count < 100 {
      1.3 // 低流量服务提高采样率
    } else {
      1.0
    }
    
    // 计算最终采样率
    let final_rate = base_rate * latency_adjustment * volume_adjustment
    
    // 确保采样率在合理范围内
    if final_rate > 1.0 { 1.0 }
    else if final_rate < 0.01 { 0.01 }
    else { final_rate }
  }
  
  // 应用分层采样策略
  let mut tier_sampling_results = []
  for service_data in service_telemetry {
    let sampling_rate = tier_based_sampling(
      service_data.tier,
      service_data.request_count,
      service_data.latency
    )
    
    tier_sampling_results = tier_sampling_results.push({
      service: service_data.service,
      tier: service_data.tier,
      request_count: service_data.request_count,
      latency: service_data.latency,
      sampling_rate: sampling_rate
    })
  }
  
  // 验证分层采样结果
  assert_eq(tier_sampling_results.length(), 8)
  
  // 验证关键服务采样率
  let critical_services = tier_sampling_results.filter(fn(r) { r.tier == "critical" })
  assert_eq(critical_services.length(), 2)
  assert_true(critical_services.all(fn(r) { r.sampling_rate >= 0.9 }))
  
  // 验证高优先级服务采样率
  let high_services = tier_sampling_results.filter(fn(r) { r.tier == "high" })
  assert_eq(high_services.length(), 2)
  assert_true(high_services.all(fn(r) { r.sampling_rate >= 0.6 }))
  
  // 验证中等优先级服务采样率
  let medium_services = tier_sampling_results.filter(fn(r) { r.tier == "medium" })
  assert_eq(medium_services.length(), 2)
  assert_true(medium_services.all(fn(r) { r.sampling_rate >= 0.4 && r.sampling_rate <= 0.8 }))
  
  // 验证低优先级服务采样率
  let low_services = tier_sampling_results.filter(fn(r) { r.tier == "low" })
  assert_eq(low_services.length(), 2)
  assert_true(low_services.all(fn(r) { r.sampling_rate <= 0.3 }))
  
  // 验证延迟对采样率的影响
  let payment_service = tier_sampling_results.filter(fn(r) { r.service == "payment-service" })[0]
  let auth_service = tier_sampling_results.filter(fn(r) { r.service == "auth-service" })[0]
  
  // payment-service延迟更高，采样率应该也更高
  assert_true(payment_service.sampling_rate > auth_service.sampling_rate)
}

// 测试3: 基于异常检测的动态采样
test "基于异常检测的动态采样" {
  // 模拟包含异常的遥测数据流
  let anomaly_telemetry = [
    { timestamp: 1640995200, metric: "cpu", value: 30.0, is_anomaly: false },
    { timestamp: 1640995260, metric: "memory", value: 60.0, is_anomaly: false },
    { timestamp: 1640995320, metric: "cpu", value: 95.0, is_anomaly: true },
    { timestamp: 1640995380, metric: "disk_io", value: 150.0, is_anomaly: true },
    { timestamp: 1640995440, metric: "network", value: 45.0, is_anomaly: false },
    { timestamp: 1640995500, metric: "cpu", value: 35.0, is_anomaly: false },
    { timestamp: 1640995560, metric: "memory", value: 98.0, is_anomaly: true },
    { timestamp: 1640995620, metric: "response_time", value: 500.0, is_anomaly: true },
    { timestamp: 1640995680, metric: "cpu", value: 32.0, is_anomaly: false },
    { timestamp: 1640995740, metric: "network", value: 50.0, is_anomaly: false }
  ]
  
  // 异常检测采样策略
  let anomaly_sampling_strategy = {
    normal_rate: 0.3,           // 正常数据采样率
    anomaly_rate: 1.0,          // 异常数据采样率
    post_anomaly_window: 300,   // 异常后时间窗口（秒）
    post_anomaly_rate: 0.8      // 异常后窗口内采样率
  }
  
  // 动态采样算法
  let anomaly_based_sampling = fn(is_anomaly, timestamp, last_anomaly_time) {
    if is_anomaly {
      // 异常数据全采样
      { sampling_rate: anomaly_sampling_strategy.anomaly_rate, is_anomaly: true }
    } else {
      // 检查是否在异常后时间窗口内
      let time_since_last_anomaly = timestamp - last_anomaly_time
      if time_since_last_anomaly <= anomaly_sampling_strategy.post_anomaly_window {
        // 异常后窗口内提高采样率
        { sampling_rate: anomaly_sampling_strategy.post_anomaly_rate, is_anomaly: false }
      } else {
        // 正常采样率
        { sampling_rate: anomaly_sampling_strategy.normal_rate, is_anomaly: false }
      }
    }
  }
  
  // 应用异常检测采样策略
  let mut anomaly_sampling_results = []
  let mut last_anomaly_time = 0
  
  for data_point in anomaly_telemetry {
    let sampling_decision = anomaly_based_sampling(
      data_point.is_anomaly,
      data_point.timestamp,
      last_anomaly_time
    )
    
    // 更新最后异常时间
    if data_point.is_anomaly {
      last_anomaly_time = data_point.timestamp
    }
    
    // 模拟采样决策
    let random_value = 0.5 // 固定值用于测试
    let is_sampled = random_value <= sampling_decision.sampling_rate
    
    anomaly_sampling_results = anomaly_sampling_results.push({
      timestamp: data_point.timestamp,
      metric: data_point.metric,
      value: data_point.value,
      is_anomaly: data_point.is_anomaly,
      sampling_rate: sampling_decision.sampling_rate,
      is_sampled: is_sampled
    })
  }
  
  // 验证异常检测采样结果
  assert_eq(anomaly_sampling_results.length(), 10)
  
  // 验证异常数据采样率
  let anomaly_data = anomaly_sampling_results.filter(fn(r) { r.is_anomaly })
  assert_eq(anomaly_data.length(), 4)
  assert_true(anomaly_data.all(fn(r) { r.sampling_rate == 1.0 })) // 异常数据全采样
  assert_true(anomaly_data.all(fn(r) { r.is_sampled })) // 异常数据全部被采样
  
  // 验证正常数据采样率
  let normal_data = anomaly_sampling_results.filter(fn(r) { not r.is_anomaly })
  assert_eq(normal_data.length(), 6)
  
  // 验证异常后窗口内的采样率
  let post_anomaly_data = normal_data.filter(fn(r) { r.sampling_rate > 0.5 })
  assert_true(post_anomaly_data.length() >= 2) // 至少有一些正常数据在异常后窗口内
  
  // 验证采样率分布
  let high_rate_samples = anomaly_sampling_results.filter(fn(r) { r.sampling_rate >= 0.8 })
  let low_rate_samples = anomaly_sampling_results.filter(fn(r) { r.sampling_rate <= 0.5 })
  
  assert_true(high_rate_samples.length() >= 4) // 异常数据+异常后窗口数据
  assert_true(low_rate_samples.length() >= 2) // 远离异常的正常数据
}

// 测试4: 多因子组合采样策略
test "多因子组合采样策略" {
  // 模拟多因子遥测数据
  let multi_factor_data = [
    { 
      timestamp: 1640995200, 
      service: "auth", 
      tier: "critical", 
      load: "low", 
      error_rate: 0.1, 
      latency: 50.0,
      business_value: 100.0
    },
    { 
      timestamp: 1640995260, 
      service: "cache", 
      tier: "low", 
      load: "high", 
      error_rate: 0.5, 
      latency: 5.0,
      business_value: 20.0
    },
    { 
      timestamp: 1640995320, 
      service: "payment", 
      tier: "critical", 
      load: "medium", 
      error_rate: 2.0, 
      latency: 200.0,
      business_value: 200.0
    },
    { 
      timestamp: 1640995380, 
      service: "analytics", 
      tier: "medium", 
      load: "low", 
      error_rate: 0.2, 
      latency: 100.0,
      business_value: 50.0
    },
    { 
      timestamp: 1640995440, 
      service: "logging", 
      tier: "low", 
      load: "high", 
      error_rate: 0.1, 
      latency: 15.0,
      business_value: 10.0
    }
  ]
  
  // 多因子权重配置
  let factor_weights = {
    tier: 0.3,         // 服务层级权重
    load: 0.2,         // 负载权重
    error_rate: 0.2,   // 错误率权重
    latency: 0.15,     // 延迟权重
    business_value: 0.15 // 业务价值权重
  }
  
  // 因子评分函数
  let score_tier = fn(tier) {
    match tier {
      "critical" => 1.0,
      "high" => 0.8,
      "medium" => 0.6,
      "low" => 0.4,
      _ => 0.2
    }
  }
  
  let score_load = fn(load) {
    match load {
      "critical" => 1.0,
      "high" => 0.8,
      "medium" => 0.6,
      "low" => 0.4,
      _ => 0.2
    }
  }
  
  let score_error_rate = fn(error_rate) {
    if error_rate > 2.0 { 1.0 }
    else if error_rate > 1.0 { 0.8 }
    else if error_rate > 0.5 { 0.6 }
    else if error_rate > 0.1 { 0.4 }
    else { 0.2 }
  }
  
  let score_latency = fn(latency) {
    if latency > 200.0 { 1.0 }
    else if latency > 100.0 { 0.8 }
    else if latency > 50.0 { 0.6 }
    else if latency > 20.0 { 0.4 }
    else { 0.2 }
  }
  
  let score_business_value = fn(business_value) {
    if business_value > 150.0 { 1.0 }
    else if business_value > 100.0 { 0.8 }
    else if business_value > 50.0 { 0.6 }
    else if business_value > 20.0 { 0.4 }
    else { 0.2 }
  }
  
  // 多因子组合采样算法
  let multi_factor_sampling = fn(service_data) {
    // 计算各因子得分
    let tier_score = score_tier(service_data.tier)
    let load_score = score_load(service_data.load)
    let error_rate_score = score_error_rate(service_data.error_rate)
    let latency_score = score_latency(service_data.latency)
    let business_value_score = score_business_value(service_data.business_value)
    
    // 计算加权总分
    let total_score = 
      tier_score * factor_weights.tier +
      load_score * factor_weights.load +
      error_rate_score * factor_weights.error_rate +
      latency_score * factor_weights.latency +
      business_value_score * factor_weights.business_value
    
    // 将分数映射到采样率（0.1-1.0）
    let sampling_rate = 0.1 + total_score * 0.9
    
    // 确保采样率在合理范围内
    if sampling_rate > 1.0 { 1.0 }
    else if sampling_rate < 0.1 { 0.1 }
    else { sampling_rate }
  }
  
  // 应用多因子采样策略
  let mut multi_factor_results = []
  for data_point in multi_factor_data {
    let sampling_rate = multi_factor_sampling(data_point)
    
    multi_factor_results = multi_factor_results.push({
      service: data_point.service,
      tier: data_point.tier,
      load: data_point.load,
      error_rate: data_point.error_rate,
      latency: data_point.latency,
      business_value: data_point.business_value,
      sampling_rate: sampling_rate
    })
  }
  
  // 验证多因子采样结果
  assert_eq(multi_factor_results.length(), 5)
  
  // 按采样率排序
  let sorted_results = multi_factor_results.sort_by(fn(a, b) {
    if a.sampling_rate > b.sampling_rate { -1 }
    else if a.sampling_rate < b.sampling_rate { 1 }
    else { 0 }
  })
  
  // 验证采样率排序
  assert_true(sorted_results[0].sampling_rate >= sorted_results[1].sampling_rate)
  assert_true(sorted_results[1].sampling_rate >= sorted_results[2].sampling_rate)
  
  // 验证最高采样率服务（应该是payment服务）
  let highest_sampled = sorted_results[0]
  assert_eq(highest_sampled.service, "payment")
  assert_eq(highest_sampled.tier, "critical")
  assert_eq(highest_sampled.error_rate, 2.0)
  assert_true(highest_sampled.sampling_rate > 0.8)
  
  // 验证最低采样率服务（应该是logging或cache服务）
  let lowest_sampled = sorted_results[sorted_results.length() - 1]
  assert_true(lowest_sampled.service == "logging" || lowest_sampled.service == "cache")
  assert_eq(lowest_sampled.tier, "low")
  assert_true(lowest_sampled.sampling_rate < 0.5)
  
  // 验证采样率分布
  let high_rate_services = sorted_results.filter(fn(r) { r.sampling_rate > 0.7 })
  let medium_rate_services = sorted_results.filter(fn(r) { r.sampling_rate >= 0.4 && r.sampling_rate <= 0.7 })
  let low_rate_services = sorted_results.filter(fn(r) { r.sampling_rate < 0.4 })
  
  assert_eq(high_rate_services.length(), 2) // payment和auth
  assert_eq(medium_rate_services.length(), 2) // analytics和其中一个低优先级服务
  assert_eq(low_rate_services.length(), 1) // 一个低优先级服务
}

// 测试5: 采样策略效果评估
test "采样策略效果评估" {
  // 模拟原始遥测数据和采样后数据
  let original_data = [
    { service: "auth", tier: "critical", error_count: 10, total_requests: 1000 },
    { service: "payment", tier: "critical", error_count: 20, total_requests: 500 },
    { service: "user", tier: "high", error_count: 15, total_requests: 2000 },
    { service: "order", tier: "high", error_count: 5, total_requests: 1500 },
    { service: "notification", tier: "medium", error_count: 8, total_requests: 3000 },
    { service: "analytics", tier: "medium", error_count: 2, total_requests: 800 },
    { service: "logging", tier: "low", error_count: 50, total_requests: 5000 },
    { service: "cache", tier: "low", error_count: 1, total_requests: 10000 }
  ]
  
  // 模拟采样后的数据
  let sampled_data = [
    { service: "auth", sampled_error_count: 10, sampled_requests: 900, sampling_rate: 0.9 },
    { service: "payment", sampled_error_count: 20, sampled_requests: 480, sampling_rate: 0.96 },
    { service: "user", sampled_error_count: 12, sampled_requests: 1000, sampling_rate: 0.5 },
    { service: "order", sampled_error_count: 4, sampled_requests: 750, sampling_rate: 0.5 },
    { service: "notification", sampled_error_count: 4, sampled_requests: 900, sampling_rate: 0.3 },
    { service: "analytics", sampled_error_count: 2, sampled_requests: 320, sampling_rate: 0.4 },
    { service: "logging", sampled_error_count: 8, sampled_requests: 750, sampling_rate: 0.15 },
    { service: "cache", sampled_error_count: 1, sampled_requests: 1200, sampling_rate: 0.12 }
  ]
  
  // 计算采样效果指标
  let mut sampling_effectiveness = []
  
  for service_sampled in sampled_data {
    // 找到对应的原始数据
    let service_original = original_data.filter(fn(s) { s.service == service_sampled.service })[0]
    
    // 计算误差率
    let original_error_rate = service_original.error_count.to_float() / service_original.total_requests.to_float()
    let sampled_error_rate = service_sampled.sampled_error_count.to_float() / service_sampled.sampled_requests.to_float()
    let error_rate_deviation = (sampled_error_rate - original_error_rate).abs() / original_error_rate * 100.0
    
    // 计算数据压缩比
    let compression_ratio = 1.0 - service_sampled.sampling_rate
    
    // 计算信息保留率（基于错误检测能力）
    let information_retention = if service_original.error_count > 0 {
      service_sampled.sampled_error_count.to_float() / service_original.error_count.to_float()
    } else {
      1.0
    }
    
    // 计算综合效果得分
    let effectiveness_score = 
      (information_retention * 0.5) + 
      ((1.0 - error_rate_deviation / 100.0) * 0.3) + 
      (compression_ratio * 0.2)
    
    sampling_effectiveness = sampling_effectiveness.push({
      service: service_sampled.service,
      tier: service_original.tier,
      sampling_rate: service_sampled.sampling_rate,
      compression_ratio: compression_ratio,
      error_rate_deviation: error_rate_deviation,
      information_retention: information_retention,
      effectiveness_score: effectiveness_score
    })
  }
  
  // 验证采样效果评估结果
  assert_eq(sampling_effectiveness.length(), 8)
  
  // 按效果得分排序
  let sorted_effectiveness = sampling_effectiveness.sort_by(fn(a, b) {
    if a.effectiveness_score > b.effectiveness_score { -1 }
    else if a.effectiveness_score < b.effectiveness_score { 1 }
    else { 0 }
  })
  
  // 验证关键服务采样效果
  let critical_services = sorted_effectiveness.filter(fn(e) { e.tier == "critical" })
  assert_eq(critical_services.length(), 2)
  assert_true(critical_services.all(fn(e) { e.effectiveness_score > 0.7 })) // 关键服务效果应该更好
  
  // 验证采样率与信息保留率的关系
  let high_sampling_rate = sorted_effectiveness.filter(fn(e) { e.sampling_rate > 0.8 })
  let low_sampling_rate = sorted_effectiveness.filter(fn(e) { e.sampling_rate < 0.3 })
  
  assert_true(high_sampling_rate.length() >= 2)
  assert_true(low_sampling_rate.length() >= 2)
  
  // 高采样率服务的信息保留率应该更高
  let high_rate_retention = high_sampling_rate.reduce(fn(acc, e) { acc + e.information_retention }, 0.0) / high_sampling_rate.length().to_float()
  let low_rate_retention = low_sampling_rate.reduce(fn(acc, e) { acc + e.information_retention }, 0.0) / low_sampling_rate.length().to_float()
  
  assert_true(high_rate_retention > low_rate_retention)
  
  // 验证整体采样效果
  let overall_effectiveness = sorted_effectiveness.reduce(fn(acc, e) { acc + e.effectiveness_score }, 0.0) / sorted_effectiveness.length().to_float()
  assert_true(overall_effectiveness > 0.5) // 整体效果应该超过50%
  
  // 验证数据压缩效果
  let overall_compression = sorted_effectiveness.reduce(fn(acc, e) { acc + e.compression_ratio }, 0.0) / sorted_effectiveness.length().to_float()
  assert_true(overall_compression > 0.3) // 整体压缩率应该超过30%
  
  // 验证错误率偏差
  let overall_error_deviation = sorted_effectiveness.reduce(fn(acc, e) { acc + e.error_rate_deviation }, 0.0) / sorted_effectiveness.length().to_float()
  assert_true(overall_error_deviation < 50.0) // 整体错误率偏差应该小于50%
}