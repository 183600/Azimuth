// Azimuth 多维度数据分析测试
// 专注于遥测数据的多维度分析和洞察提取

// 测试1: 多维度数据聚合
test "多维度数据聚合分析" {
  // 创建多维度分析器
  let multi_analyzer = MultiDimensionalAnalyzer::new()
  
  // 配置分析维度
  MultiDimensionalAnalyzer::add_dimension(multi_analyzer, "service", {
    type: "categorical",
    values: ["api.gateway", "auth.service", "data.service", "cache.service"],
    weight: 0.3
  })
  
  MultiDimensionalAnalyzer::add_dimension(multi_analyzer, "operation", {
    type: "categorical",
    values: ["GET", "POST", "PUT", "DELETE"],
    weight: 0.2
  })
  
  MultiDimensionalAnalyzer::add_dimension(multi_analyzer, "status_code", {
    type: "numerical",
    range: [100, 599],
    buckets: [200, 400, 500],
    weight: 0.2
  })
  
  MultiDimensionalAnalyzer::add_dimension(multi_analyzer, "response_time", {
    type: "numerical",
    range: [0, 5000],
    buckets: [50, 100, 200, 500, 1000],
    weight: 0.3
  })
  
  // 创建多维度测试数据
  let multi_dimensional_data = []
  let base_time = 1640995200
  
  for i in 0..=200 {
    let service = ["api.gateway", "auth.service", "data.service", "cache.service"][i % 4]
    let operation = ["GET", "POST", "PUT", "DELETE"][i % 4]
    let status_code = if i % 10 == 0 { 500 } else if i % 5 == 0 { 404 } else { 200 }
    let response_time = 50 + (i % 10) * 100 + (service == "data.service" ? 200 : 0)
    
    multi_dimensional_data = multi_dimensional_data.push({
      timestamp: base_time + i * 60,
      service: service,
      operation: operation,
      status_code: status_code,
      response_time: response_time,
      user_id: "user-" + (i % 50).to_string(),
      region: ["us-east-1", "us-west-2", "eu-west-1"][i % 3]
    })
  }
  
  // 执行多维度聚合
  let aggregation_result = MultiDimensionalAnalyzer::aggregate(multi_analyzer, multi_dimensional_data, [
    "service", "operation", "status_code", "response_time"
  ])
  
  // 验证聚合结果
  assert_true(aggregation_result.dimensions.length() == 4)
  assert_true(aggregation_result.total_data_points == 201)
  
  // 验证服务维度聚合
  let service_dimension = aggregation_result.dimensions.find(fn(d) { d.name == "service" })
  assert_true(service_dimension != None)
  
  match service_dimension {
    Some(dim) => {
      assert_eq(dim.buckets.length(), 4)
      let api_gateway_bucket = dim.buckets.find(fn(b) { b.value == "api.gateway" })
      assert_true(api_gateway_bucket != None)
      match api_gateway_bucket {
        Some(bucket) => {
          assert_eq(bucket.count, 51)  // 201 / 4 = 50.25，向上取整为51
          assert_true(bucket.percentage > 24.0 and bucket.percentage < 26.0)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证响应时间维度聚合
  let response_time_dimension = aggregation_result.dimensions.find(fn(d) { d.name == "response_time" })
  assert_true(response_time_dimension != None)
  
  match response_time_dimension {
    Some(dim) => {
      assert_eq(dim.buckets.length(), 5)
      let fast_bucket = dim.buckets.find(fn(b) { b.range_start >= 0 and b.range_end <= 100 })
      assert_true(fast_bucket != None)
      match fast_bucket {
        Some(bucket) => {
          assert_true(bucket.count > 0)
          assert_true(bucket.percentage > 0.0)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 执行交叉维度分析
  let cross_analysis = MultiDimensionalAnalyzer::cross_analyze(multi_analyzer, multi_dimensional_data, [
    { dimension1: "service", dimension2: "status_code" },
    { dimension1: "service", dimension2: "response_time" },
    { dimension1: "operation", dimension2: "status_code" }
  ])
  
  // 验证交叉分析结果
  assert_eq(cross_analysis.cross_tables.length(), 3)
  
  let service_status_cross = cross_analysis.cross_tables.find(fn(t) { 
    t.dimension1 == "service" and t.dimension2 == "status_code" 
  })
  assert_true(service_status_cross != None)
  
  match service_status_cross {
    Some(table) => {
      assert_eq(table.rows, 4)  // 4个服务
      assert_eq(table.columns, 3)  // 3个状态码桶(200, 4xx, 5xx)
      
      // 验证具体交叉数据
      let api_gateway_200 = table.data.find(fn(d) { 
        d.dim1_value == "api.gateway" and d.dim2_value == "200" 
      })
      assert_true(api_gateway_200 != None)
    }
    None => assert_true(false)
  }
}

// 测试2: 多维度相关性分析
test "多维度相关性分析" {
  // 创建相关性分析器
  let correlation_analyzer = CorrelationAnalyzer::new()
  
  // 配置分析参数
  CorrelationAnalyzer::set_methods(correlation_analyzer, [
    "pearson",    // 线性相关性
    "spearman",   // 秩相关性
    "mutual_info" // 互信息
  ])
  
  CorrelationAnalyzer::set_significance_threshold(correlation_analyzer, 0.05)
  CorrelationAnalyzer::set_correlation_threshold(correlation_analyzer, 0.3)
  
  // 创建具有相关性的测试数据
  let correlated_data = []
  let base_time = 1640995200
  
  for i in 0..=100 {
    // 基础指标
    let request_count = 50 + (i % 20) * 5
    let cpu_usage = 20.0 + request_count * 0.8 + (i % 5) * 2.0  // 与请求数正相关
    let memory_usage = 100.0 + request_count * 2.0 + (i % 3) * 5.0  // 与请求数正相关
    let response_time = 50.0 + cpu_usage * 0.5 + (i % 10) * 3.0  // 与CPU使用率正相关
    let error_rate = if cpu_usage > 60.0 { (cpu_usage - 60.0) * 0.1 } else { 0.0 }  // CPU高时错误率增加
    
    correlated_data = correlated_data.push({
      timestamp: base_time + i * 60,
      request_count: request_count,
      cpu_usage: cpu_usage,
      memory_usage: memory_usage,
      response_time: response_time,
      error_rate: error_rate,
      cache_hit_rate: 80.0 - (i % 20) * 2.0  // 与其他指标负相关
    })
  }
  
  // 执行相关性分析
  let correlation_result = CorrelationAnalyzer::analyze(correlation_analyzer, correlated_data, [
    "request_count", "cpu_usage", "memory_usage", "response_time", "error_rate", "cache_hit_rate"
  ])
  
  // 验证相关性分析结果
  assert_true(correlation_result.correlations.length() > 0)
  
  // 验证强相关性
  let request_cpu_corr = correlation_result.correlations.find(fn(c) { 
    (c.metric1 == "request_count" and c.metric2 == "cpu_usage") or
    (c.metric1 == "cpu_usage" and c.metric2 == "request_count")
  })
  assert_true(request_cpu_corr != None)
  
  match request_cpu_corr {
    Some(corr) => {
      assert_true(corr.pearson > 0.7)  // 强正相关
      assert_true(corr.spearman > 0.7)
      assert_true(corr.p_value < 0.05)  // 统计显著
      assert_eq(corr.significance, "high")
    }
    None => assert_true(false)
  }
  
  // 验证负相关性
  let cpu_cache_corr = correlation_result.correlations.find(fn(c) { 
    (c.metric1 == "cpu_usage" and c.metric2 == "cache_hit_rate") or
    (c.metric1 == "cache_hit_rate" and c.metric2 == "cpu_usage")
  })
  assert_true(cpu_cache_corr != None)
  
  match cpu_cache_corr {
    Some(corr) => {
      assert_true(corr.pearson < -0.5)  // 负相关
      assert_true(corr.p_value < 0.05)
      assert_eq(corr.significance, "medium" or corr.significance == "high")
    }
    None => assert_true(false)
  }
  
  // 生成相关性矩阵
  let correlation_matrix = CorrelationAnalyzer::generate_matrix(correlation_analyzer, correlation_result)
  
  // 验证相关性矩阵
  assert_eq(correlation_matrix.metrics.length(), 6)
  assert_eq(correlation_matrix.matrix.length(), 6)
  assert_eq(correlation_matrix.matrix[0].length(), 6)
  
  // 验证对角线元素（自相关性）
  for i in 0..=5 {
    assert_eq(correlation_matrix.matrix[i][i], 1.0)
  }
  
  // 生成相关性网络图数据
  let network_data = CorrelationAnalyzer::generate_network_data(correlation_analyzer, correlation_result, 0.5)
  
  // 验证网络图数据
  assert_true(network_data.nodes.length() > 0)
  assert_true(network_data.edges.length() > 0)
  
  // 验证边数据
  let strong_edges = network_data.edges.filter(fn(e) { e.weight > 0.7 })
  assert_true(strong_edges.length() > 0)
  
  // 验证节点数据
  let high_degree_nodes = network_data.nodes.filter(fn(n) { n.degree > 2 })
  assert_true(high_degree_nodes.length() > 0)
}

// 测试3: 多维度异常检测
test "多维度异常检测分析" {
  // 创建多维异常检测器
  let anomaly_detector = MultiDimensionalAnomalyDetector::new()
  
  // 配置检测算法
  MultiDimensionalAnomalyDetector::add_algorithm(anomaly_detector, "isolation_forest", {
    contamination: 0.1,
    n_estimators: 100,
    max_samples: "auto"
  })
  
  MultiDimensionalAnomalyDetector::add_algorithm(anomaly_detector, "local_outlier_factor", {
    n_neighbors: 20,
    contamination: 0.1,
    novelty: false
  })
  
  MultiDimensionalAnomalyDetector::add_algorithm(anomaly_detector, "one_class_svm", {
    kernel: "rbf",
    gamma: "scale",
    nu: 0.1
  })
  
  // 创建多维测试数据
  let normal_data = []
  let anomaly_data = []
  let base_time = 1640995200
  
  // 生成正常数据
  for i in 0..=200 {
    normal_data = normal_data.push({
      timestamp: base_time + i * 60,
      cpu_usage: 30.0 + (i % 20) * 2.0,
      memory_usage: 50.0 + (i % 15) * 3.0,
      disk_io: 10.0 + (i % 10) * 1.5,
      network_io: 20.0 + (i % 25) * 2.0,
      request_rate: 100.0 + (i % 30) * 5.0,
      error_rate: if i % 20 == 0 { 0.01 } else { 0.0 },
      service: "api.service"
    })
  }
  
  // 生成异常数据
  for i in 0..=10 {
    anomaly_data = anomaly_data.push({
      timestamp: base_time + (200 + i) * 60,
      cpu_usage: 90.0 + i * 2.0,        // 异常高CPU
      memory_usage: 85.0 + i * 1.5,     // 异常高内存
      disk_io: 50.0 + i * 5.0,          // 异常高磁盘IO
      network_io: 80.0 + i * 3.0,       // 异常高网络IO
      request_rate: 500.0 + i * 10.0,   // 异常高请求率
      error_rate: 0.1 + i * 0.01,       // 异常高错误率
      service: "api.service"
    })
  }
  
  // 合并数据
  let all_data = normal_data + anomaly_data
  
  // 执行异常检测
  let anomaly_result = MultiDimensionalAnomalyDetector::detect(anomaly_detector, all_data, [
    "cpu_usage", "memory_usage", "disk_io", "network_io", "request_rate", "error_rate"
  ])
  
  // 验证异常检测结果
  assert_true(anomaly_result.total_data_points == 211)
  assert_true(anomaly_result.anomaly_count > 0)
  assert_true(anomaly_result.normal_count > 0)
  
  // 验证异常分数
  let anomaly_scores = anomaly_result.data_points.filter(fn(dp) { dp.is_anomaly })
  assert_true(anomaly_scores.length() > 0)
  
  // 验证异常分数分布
  let high_score_anomalies = anomaly_scores.filter(fn(dp) { dp.anomaly_score > 0.8 })
  assert_true(high_score_anomalies.length() > 0)
  
  // 验证异常维度贡献分析
  let anomaly_contributions = MultiDimensionalAnomalyDetector::analyze_contributions(anomaly_detector, anomaly_result)
  
  // 验证贡献分析
  assert_true(anomaly_contributions.length() > 0)
  
  let cpu_contribution = anomaly_contributions.find(fn(c) { c.dimension == "cpu_usage" })
  assert_true(cpu_contribution != None)
  
  match cpu_contribution {
    Some(contrib) => {
      assert_true(contrib.average_contribution > 0.0)
      assert_true(contrib.max_contribution >= contrib.average_contribution)
    }
    None => assert_true(false)
  }
  
  // 执行异常模式分析
  let pattern_analysis = MultiDimensionalAnomalyDetector::analyze_patterns(anomaly_detector, anomaly_result)
  
  // 验证模式分析
  assert_true(pattern_analysis.patterns.length() > 0)
  
  let high_resource_pattern = pattern_analysis.patterns.find(fn(p) { 
    p.name.contains("high_resource") or p.name.contains("resource_usage")
  })
  assert_true(high_resource_pattern != None)
  
  match high_resource_pattern {
    Some(pattern) => {
      assert_true(pattern.frequency > 0)
      assert_true(pattern.confidence > 0.5)
      assert_true(pattern.dimensions.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 生成异常解释
  let explanations = MultiDimensionalAnomalyDetector::generate_explanations(anomaly_detector, anomaly_result)
  
  // 验证异常解释
  assert_true(explanations.length() > 0)
  
  let first_explanation = explanations[0]
  assert_true(first_explanation.anomaly_score > 0.0)
  assert_true(first_explanation.explanation.length() > 0)
  assert_true(first_explanation.contributing_factors.length() > 0)
}

// 测试4: 多维度趋势分析
test "多维度趋势分析" {
  // 创建趋势分析器
  let trend_analyzer = MultiDimensionalTrendAnalyzer::new()
  
  // 配置分析参数
  TrendAnalyzer::set_time_windows(trend_analyzer, [
    { name: "short", duration: 3600 },    // 1小时
    { name: "medium", duration: 86400 },  // 24小时
    { name: "long", duration: 604800 }    // 7天
  ])
  
  TrendAnalyzer::set_trend_methods(trend_analyzer, [
    "linear_regression",
    "moving_average",
    "seasonal_decomposition"
  ])
  
  TrendAnalyzer::set_significance_threshold(trend_analyzer, 0.05)
  
  // 创建时间序列测试数据
  let time_series_data = []
  let base_time = 1640995200
  
  for i in 0..=720 {  // 12小时的数据，每分钟一个点
    let hour_of_day = (i / 60) % 24
    let day_of_week = ((base_time + i * 60) / 86400) % 7
    
    // 模拟日内模式（白天高，夜晚低）
    let daily_pattern = if hour_of_day >= 8 and hour_of_day <= 18 { 1.2 } else { 0.8 }
    
    // 模拟周内模式（工作日高，周末低）
    let weekly_pattern = if day_of_week >= 1 and day_of_week <= 5 { 1.1 } else { 0.9 }
    
    // 模拟整体上升趋势
    let overall_trend = 1.0 + (i / 720.0) * 0.2
    
    // 添加随机噪声
    let noise = 0.9 + (i % 20) * 0.01
    
    let base_request_rate = 100.0
    let request_rate = base_request_rate * daily_pattern * weekly_pattern * overall_trend * noise
    
    // 其他指标的相关变化
    let cpu_usage = 20.0 + request_rate * 0.15 + (i % 10) * 0.5
    let memory_usage = 40.0 + request_rate * 0.25 + (i % 8) * 1.0
    let response_time = 50.0 + cpu_usage * 0.8 + (i % 15) * 2.0
    
    time_series_data = time_series_data.push({
      timestamp: base_time + i * 60,
      request_rate: request_rate,
      cpu_usage: cpu_usage,
      memory_usage: memory_usage,
      response_time: response_time,
      error_rate: if request_rate > 150.0 { 0.02 } else { 0.005 },
      hour_of_day: hour_of_day,
      day_of_week: day_of_week
    })
  }
  
  // 执行多维度趋势分析
  let trend_result = TrendAnalyzer::analyze(trend_analyzer, time_series_data, [
    "request_rate", "cpu_usage", "memory_usage", "response_time", "error_rate"
  ])
  
  // 验证趋势分析结果
  assert_eq(trend_result.metrics.length(), 5)
  
  // 验证请求率趋势
  let request_rate_trend = trend_result.metrics.find(fn(m) { m.name == "request_rate" })
  assert_true(request_rate_trend != None)
  
  match request_rate_trend {
    Some(trend) => {
      // 验证整体趋势（应该是上升趋势）
      assert_true(trend.overall_trend.direction == "increasing")
      assert_true(trend.overall_trend.slope > 0.0)
      assert_true(trend.overall_trend.confidence > 0.5)
      
      // 验证季节性模式
      assert_true(trend.seasonal_patterns.length() > 0)
      
      let daily_pattern = trend.seasonal_patterns.find(fn(p) { p.period == "daily" })
      assert_true(daily_pattern != None)
      
      match daily_pattern {
        Some(pattern) => {
          assert_true(pattern.strength > 0.3)  // 有明显的日模式
          assert_eq(pattern.period_hours, 24)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 验证CPU使用率趋势
  let cpu_trend = trend_result.metrics.find(fn(m) { m.name == "cpu_usage" })
  assert_true(cpu_trend != None)
  
  match cpu_trend {
    Some(trend) => {
      // CPU使用率应该与请求率有相似的趋势
      assert_true(trend.overall_trend.direction == "increasing")
      assert_true(trend.overall_trend.slope > 0.0)
    }
    None => assert_true(false)
  }
  
  // 验证错误率趋势
  let error_rate_trend = trend_result.metrics.find(fn(m) { m.name == "error_rate" })
  assert_true(error_rate_trend != None)
  
  match error_rate_trend {
    Some(trend) => {
      // 错误率应该与请求率正相关
      assert_true(trend.correlation_with_request_rate > 0.5)
    }
    None => assert_true(false)
  }
  
  // 执行交叉趋势分析
  let cross_trend_result = TrendAnalyzer::cross_analyze(trend_analyzer, time_series_data, [
    { metric1: "request_rate", metric2: "cpu_usage" },
    { metric1: "request_rate", metric2: "response_time" },
    { metric1: "cpu_usage", metric2: "response_time" }
  ])
  
  // 验证交叉趋势分析
  assert_eq(cross_trend_result.correlations.length(), 3)
  
  let request_cpu_cross = cross_trend_result.correlations.find(fn(c) { 
    c.metric1 == "request_rate" and c.metric2 == "cpu_usage" 
  })
  assert_true(request_cpu_cross != None)
  
  match request_cpu_cross {
    Some(cross) => {
      assert_true(cross.correlation > 0.7)  // 强正相关
      assert_true(cross.lag == 0)          // 无延迟
      assert_true(cross.significance == "high")
    }
    None => assert_true(false)
  }
  
  // 生成趋势预测
  let prediction_result = TrendAnalyzer::predict(trend_analyzer, trend_result, 180)  // 预测未来3小时
  
  // 验证预测结果
  assert_eq(prediction_result.metrics.length(), 5)
  assert_eq(prediction_result.prediction_horizon, 180)
  
  let request_rate_prediction = prediction_result.metrics.find(fn(m) { m.name == "request_rate" })
  assert_true(request_rate_prediction != None)
  
  match request_rate_prediction {
    Some(pred) => {
      assert_eq(pred.predicted_values.length(), 180)
      assert_true(pred.confidence_intervals.length() == 180)
      assert_true(pred.prediction_accuracy > 0.0)
      
      // 验证预测趋势延续
      let first_predicted = pred.predicted_values[0]
      let last_predicted = pred.predicted_values[179]
      assert_true(last_predicted > first_predicted)  // 继续上升趋势
    }
    None => assert_true(false)
  }
}

// 测试5: 多维度根因分析
test "多维度根因分析" {
  // 创建根因分析器
  let root_cause_analyzer = RootCauseAnalyzer::new()
  
  // 配置分析方法
  RootCauseAnalyzer::add_method(root_cause_analyzer, "correlation_based", {
    threshold: 0.7,
    time_window: 3600,
    lag_tolerance: 300
  })
  
  RootCauseAnalyzer::add_method(root_cause_analyzer, "causal_inference", {
    algorithm: "pc_algorithm",
    significance_level: 0.05,
    max_conditioning_set: 3
  })
  
  RootCauseAnalyzer::add_method(root_cause_analyzer, "attribute_importance", {
    algorithm: "random_forest",
    n_estimators: 100,
    max_depth: 10
  })
  
  // 创建问题场景测试数据
  let incident_data = []
  let base_time = 1640995200
  
  // 正常时期数据
  for i in 0..=240 {  // 4小时正常数据
    incident_data = incident_data.push({
      timestamp: base_time + i * 60,
      cpu_usage: 30.0 + (i % 20) * 2.0,
      memory_usage: 50.0 + (i % 15) * 3.0,
      disk_io: 10.0 + (i % 10) * 1.5,
      network_io: 20.0 + (i % 25) * 2.0,
      request_rate: 100.0 + (i % 30) * 5.0,
      error_rate: 0.01,
      response_time: 50.0 + (i % 20) * 3.0,
      cache_hit_rate: 85.0,
      db_connections: 10,
      incident: false
    })
  }
  
  // 事件发生时期数据
  let incident_start = 241
  for i in incident_start..=360 {  // 2小时事件数据
    let time_offset = i - incident_start
    
    // 模拟事件发展过程
    let db_connection_issue = if time_offset < 30 { 0.0 } else { 1.0 }
    let cache_failure = if time_offset >= 20 and time_offset < 80 { 1.0 } else { 0.0 }
    
    incident_data = incident_data.push({
      timestamp: base_time + i * 60,
      cpu_usage: 30.0 + (i % 20) * 2.0 + db_connection_issue * 40.0 + cache_failure * 20.0,
      memory_usage: 50.0 + (i % 15) * 3.0 + db_connection_issue * 30.0,
      disk_io: 10.0 + (i % 10) * 1.5 + db_connection_issue * 60.0,
      network_io: 20.0 + (i % 25) * 2.0 + cache_failure * 40.0,
      request_rate: 100.0 + (i % 30) * 5.0 - db_connection_issue * 30.0,
      error_rate: 0.01 + db_connection_issue * 0.15 + cache_failure * 0.05,
      response_time: 50.0 + (i % 20) * 3.0 + db_connection_issue * 200.0 + cache_failure * 100.0,
      cache_hit_rate: 85.0 - cache_failure * 70.0,
      db_connections: 10 + db_connection_issue * 50,
      incident: true
    })
  }
  
  // 执行根因分析
  let root_cause_result = RootCauseAnalyzer::analyze(root_cause_analyzer, incident_data, {
    incident_start_time: base_time + incident_start * 60,
    incident_end_time: base_time + 360 * 60,
    target_metrics: ["error_rate", "response_time"],
    candidate_causes: ["cpu_usage", "memory_usage", "disk_io", "network_io", "db_connections", "cache_hit_rate"]
  })
  
  // 验证根因分析结果
  assert_true(root_cause_result.incident_detected)
  assert_true(root_cause_result.root_causes.length() > 0)
  
  // 验证识别出的根因
  let db_connection_cause = root_cause_result.root_causes.find(fn(rc) { 
    rc.metric == "db_connections" or rc.metric.contains("db")
  })
  assert_true(db_connection_cause != None)
  
  match db_connection_cause {
    Some(cause) => {
      assert_true(cause.confidence > 0.7)
      assert_true(cause.contribution_score > 0.5)
      assert_true(cause.time_to_impact >= 0)
      assert_true(cause.causal_path.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证缓存失效作为次要原因
  let cache_cause = root_cause_result.root_causes.find(fn(rc) { 
    rc.metric == "cache_hit_rate" or rc.metric.contains("cache")
  })
  assert_true(cache_cause != None)
  
  match cache_cause {
    Some(cause) => {
      assert_true(cause.confidence > 0.5)
      assert_true(cause.contribution_score > 0.3)
    }
    None => assert_true(false)
  }
  
  // 验证因果链
  let causal_chains = RootCauseAnalyzer::build_causal_chains(root_cause_analyzer, root_cause_result)
  
  // 验证因果链
  assert_true(causal_chains.length() > 0)
  
  let primary_chain = causal_chains.find(fn(chain) { chain.is_primary })
  assert_true(primary_chain != None)
  
  match primary_chain {
    Some(chain) => {
      assert_true(chain.events.length() >= 2)
      assert_true(chain.confidence > 0.6)
      
      // 验证因果链顺序
      let db_event = chain.events.find(fn(e) { e.metric.contains("db") })
      assert_true(db_event != None)
      
      let performance_event = chain.events.find(fn(e) { 
        e.metric == "response_time" or e.metric == "error_rate" 
      })
      assert_true(performance_event != None)
      
      // 验证时间顺序
      match (db_event, performance_event) {
        (Some(db), Some(perf)) => {
          assert_true(db.timestamp <= perf.timestamp)
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 生成根因分析报告
  let analysis_report = RootCauseAnalyzer::generate_report(root_cause_analyzer, root_cause_result)
  
  // 验证报告内容
  assert_true(analysis_report.executive_summary.length() > 0)
  assert_true(analysis_report.incident_timeline.length() > 0)
  assert_true(analysis_report.root_cause_analysis.length() > 0)
  assert_true(analysis_report.recommendations.length() > 0)
  
  // 验证建议内容
  let prevention_recommendations = analysis_report.recommendations.filter(fn(r) { r.category == "prevention" })
  assert_true(prevention_recommendations.length() > 0)
  
  let monitoring_recommendations = analysis_report.recommendations.filter(fn(r) { r.category == "monitoring" })
  assert_true(monitoring_recommendations.length() > 0)
  
  // 验证影响分析
  let impact_analysis = RootCauseAnalyzer::analyze_impact(root_cause_analyzer, incident_data, root_cause_result)
  
  // 验证影响分析
  assert_true(impact_analysis.affected_metrics.length() > 0)
  assert_true(impact_analysis.affected_services.length() > 0)
  assert_true(impact_analysis.business_impact.duration > 0)
  assert_true(impact_analysis.business_impact.estimated_cost > 0.0)
}