// Azimuth Premium Observability Analytics Test Suite
// 高级可观测性分析测试套件 - 专注于可观测性数据的聚合分析和洞察

// Test 1: 多维度数据聚合分析
test "multi-dimensional data aggregation analysis" {
  // 创建多维度数据分析器
  let analytics_engine = @azimuth.ObservabilityAnalyticsEngine::new()
  
  // 配置分析维度
  let analysis_dimensions = [
    @azimuth.AnalysisDimension::new("service", "服务名称", @azimensionType::Categorical),
    @azimuth.AnalysisDimension::new("operation", "操作名称", @azimensionType::Categorical),
    @azimuth.AnalysisDimension::new("user_tier", "用户层级", @azimensionType::Categorical),
    @azimuth.AnalysisDimension::new("response_time", "响应时间", @azimensionType::Numerical),
    @azimuth.AnalysisDimension::new("error_rate", "错误率", @azimensionType::Numerical),
    @azimuth.AnalysisDimension::new("throughput", "吞吐量", @azimensionType::Numerical),
    @azimuth.AnalysisDimension::new("timestamp", "时间戳", @azimensionType::Temporal)
  ]
  
  @azimuth.ObservabilityAnalyticsEngine::configure_dimensions(analytics_engine, analysis_dimensions)
  
  // 配置聚合函数
  let aggregation_functions = [
    @azimuth.AggregationFunction::new("avg", "平均值", @azimuth.AggregationType::Mean),
    @azimuth.AggregationFunction::new("sum", "总和", @azimuth.AggregationType::Sum),
    @azimuth.AggregationFunction::new("max", "最大值", @azimuth.AggregationType::Max),
    @azimuth.AggregationFunction::new("min", "最小值", @azimuth.AggregationType::Min),
    @azimuth.AggregationFunction::new("p95", "95百分位", @azimuth.AggregationType::Percentile, 95),
    @azimuth.AggregationFunction::new("p99", "99百分位", @azimuth.AggregationType::Percentile, 99),
    @azimuth.AggregationFunction::new("count", "计数", @azimuth.AggregationType::Count),
    @azimuth.AggregationFunction::new("rate", "比率", @azimuth.AggregationType::Rate)
  ]
  
  @azimuth.ObservabilityAnalyticsEngine::configure_aggregations(analytics_engine, aggregation_functions)
  
  // 生成多维度测试数据
  let current_time = @azimuth.Time::now_unix_nanos()
  let services = ["auth-service", "payment-service", "user-service", "order-service", "notification-service"]
  let operations = ["login", "process-payment", "get-profile", "create-order", "send-notification"]
  let user_tiers = ["basic", "premium", "enterprise"]
  let mut telemetry_data = []
  
  for i in 0..=5000 {
    let service = services.get(i % services.length()).unwrap()
    let operation = operations.get(i % operations.length()).unwrap()
    let user_tier = user_tiers.get(i % user_tiers.length()).unwrap()
    let timestamp = current_time + (i * 1000000) // 每毫秒一个数据点
    
    let data_point = @azimuth.TelemetryDataPoint::new([
      ("service", @azimuth.StringValue(service)),
      ("operation", @azimuth.StringValue(operation)),
      ("user_tier", @azimuth.StringValue(user_tier)),
      ("response_time", @azimuth.FloatValue(50.0 + (i % 200) * 2.0 + @azimuth.Math::random() * 50.0)),
      ("error_rate", @azimuth.FloatValue(if i % 20 == 0 { 5.0 + @azimuth.Math::random() * 5.0 } else { 0.1 + @azimuth.Math::random() * 0.5 })),
      ("throughput", @azimuth.FloatValue(100.0 + (i % 100) * 5.0 + @azimuth.Math::random() * 20.0)),
      ("timestamp", @azimuth.IntValue(timestamp))
    ])
    telemetry_data = telemetry_data.push(data_point)
  }
  
  // 执行多维度聚合分析
  let aggregation_request = @azimuth.AggregationRequest::new()
    .with_dimensions(["service", "operation"])
    .with_metrics(["response_time", "error_rate", "throughput"])
    .with_functions(["avg", "p95", "p99", "count"])
    .with_time_range(current_time, current_time + (5000 * 1000000))
    .with_grouping(["service", "user_tier"])
  
  let aggregation_result = @azimuth.ObservabilityAnalyticsEngine::execute_aggregation(analytics_engine, aggregation_request, telemetry_data)
  
  // 验证聚合结果
  assert_true(aggregation_result.success)
  assert_true(aggregation_result.groups.length() > 0)
  assert_true(aggregation_result.total_data_points == 5001)
  
  // 验证分组结果
  let service_groups = aggregation_result.groups.filter(fn(group) {
    group.group_by.contains("service")
  })
  assert_true(service_groups.length() >= 5) // 至少5个服务
  
  // 验证每个服务的聚合指标
  for group in service_groups {
    let service_name = group.group_dimensions.get("service")
    match service_name {
      Some(name) => {
        assert_true(services.contains(name))
        
        let response_time_avg = group.aggregated_metrics.get("response_time_avg")
        let response_time_p95 = group.aggregated_metrics.get("response_time_p95")
        let error_rate_avg = group.aggregated_metrics.get("error_rate_avg")
        let throughput_avg = group.aggregated_metrics.get("throughput_avg")
        
        match (response_time_avg, response_time_p95, error_rate_avg, throughput_avg) {
          (Some(avg), Some(p95), Some(error), Some(throughput)) => {
            assert_true(avg > 0.0)
            assert_true(p95 >= avg)
            assert_true(error >= 0.0)
            assert_true(throughput > 0.0)
          }
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // 测试时间序列聚合
  let time_series_request = @azimuth.AggregationRequest::new()
    .with_dimensions(["timestamp"])
    .with_metrics(["response_time", "throughput"])
    .with_functions(["avg", "max"])
    .with_time_range(current_time, current_time + (5000 * 1000000))
    .with_time_bucket(@azimuth.TimeBucket::Seconds(1)) // 按秒聚合
  
  let time_series_result = @azimuth.ObservabilityAnalyticsEngine::execute_aggregation(analytics_engine, time_series_request, telemetry_data)
  
  // 验证时间序列结果
  assert_true(time_series_result.success)
  assert_true(time_series_result.time_buckets.length() >= 5) // 至少5个时间桶
  
  // 验证时间序列数据的连续性
  let mut previous_timestamp = 0L
  for bucket in time_series_result.time_buckets {
    assert_true(bucket.timestamp > previous_timestamp)
    previous_timestamp = bucket.timestamp
    
    let response_time_avg = bucket.metrics.get("response_time_avg")
    let throughput_avg = bucket.metrics.get("throughput_avg")
    
    match (response_time_avg, throughput_avg) {
      (Some(avg), Some(throughput)) => {
        assert_true(avg > 0.0)
        assert_true(throughput > 0.0)
      }
      _ => assert_true(false)
    }
  }
  
  // 测试交叉维度分析
  let cross_dimension_request = @azimuth.AggregationRequest::new()
    .with_dimensions(["service", "user_tier"])
    .with_metrics(["response_time", "error_rate"])
    .with_functions(["avg", "p95"])
    .with_time_range(current_time, current_time + (5000 * 1000000))
    .with_cross_analysis(["service", "user_tier"]) // 交叉分析服务和用户层级
  
  let cross_analysis_result = @azimuth.ObservabilityAnalyticsEngine::execute_cross_analysis(analytics_engine, cross_dimension_request, telemetry_data)
  
  // 验证交叉分析结果
  assert_true(cross_analysis_result.success)
  assert_true(cross_analysis_result.correlation_matrix.length() > 0)
  assert_true(cross_analysis_result.insights.length() > 0)
  
  // 验证相关性分析
  let correlations = cross_analysis_result.correlation_matrix
  for correlation in correlations {
    assert_true(correlation.correlation_coefficient >= -1.0)
    assert_true(correlation.correlation_coefficient <= 1.0)
    assert_true(correlation.significance_level >= 0.0)
    assert_true(correlation.significance_level <= 1.0)
  }
}

// Test 2: 智能异常模式识别
test "intelligent anomaly pattern recognition" {
  // 创建智能异常检测器
  let anomaly_detector = @azimuth.IntelligentAnomalyDetector::new()
  
  // 配置检测算法
  let detection_algorithms = [
    @azimuth.DetectionAlgorithm::StatisticalOutlier,
    @azimuth.DetectionAlgorithm::MachineLearning,
    @azimuth.DetectionAlgorithm::TimeSeriesDecomposition,
    @azimuth.DetectionAlgorithm::PatternRecognition,
    @azimuth.DetectionAlgorithm::BehavioralAnalysis
  ]
  
  @azimuth.IntelligentAnomalyDetector::configure_algorithms(anomaly_detector, detection_algorithms)
  
  // 配置异常类型
  let anomaly_types = [
    @azimuth.AnomalyType::Spike("突然峰值"),
    @azimuth.AnomalyType::Drop("突然下降"),
    @azimuth.AnomalyType::TrendChange("趋势变化"),
    @azimuth.AnomalyType::PatternViolation("模式违反"),
    @azimuth.AnomalyType::SeasonalAnomaly("季节性异常"),
    @azimuth.AnomalyType::ContextualAnomaly("上下文异常")
  ]
  
  @azimuth.IntelligentAnomalyDetector::configure_anomaly_types(anomaly_detector, anomaly_types)
  
  // 生成包含不同异常模式的测试数据
  let current_time = @azimuth.Time::now_unix_nanos()
  let mut normal_data = []
  let mut anomaly_data = []
  
  // 生成正常数据（基线）
  for i in 0..=2000 {
    let timestamp = current_time + (i * 60000000000) // 每分钟一个数据点
    let hour_of_day = ((timestamp / 3600000000000) % 24).to_int()
    
    // 正常的日周期模式
    let base_value = 50.0 + (hour_of_day / 24.0) * 30.0
    let noise = @azimuth.Math::random() * 10.0 - 5.0
    let value = base_value + noise
    
    normal_data = normal_data.push(@azimuth.MetricDataPoint::new("cpu.usage", value, timestamp))
  }
  
  // 注入不同类型的异常
  // 突然峰值异常
  for i in 2001..=2050 {
    let timestamp = current_time + (i * 60000000000)
    let spike_value = 150.0 + @azimuth.Math::random() * 50.0
    anomaly_data = anomaly_data.push(@azimuth.MetricDataPoint::new("cpu.usage", spike_value, timestamp))
  }
  
  // 突然下降异常
  for i in 2051..=2100 {
    let timestamp = current_time + (i * 60000000000)
    let drop_value = 5.0 + @azimuth.Math::random() * 5.0
    anomaly_data = anomaly_data.push(@azimuth.MetricDataPoint::new("cpu.usage", drop_value, timestamp))
  }
  
  // 趋势变化异常
  for i in 2101..=2200 {
    let timestamp = current_time + (i * 60000000000)
    let trend_value = 20.0 + (i - 2100) * 0.5 // 逐渐下降的趋势
    anomaly_data = anomaly_data.push(@azimuth.MetricDataPoint::new("cpu.usage", trend_value, timestamp))
  }
  
  // 合并所有数据
  let all_data = normal_data.concat(anomaly_data)
  
  // 执行异常检测
  let detection_request = @azimuth.AnomalyDetectionRequest::new()
    .with_metric("cpu.usage")
    .with_algorithms([@azimuth.DetectionAlgorithm::StatisticalOutlier, @azimuth.DetectionAlgorithm::MachineLearning])
    .with_sensitivity(0.8) // 80%敏感度
    .with_min_anomaly_duration(@azimuth.Duration::Minutes(5))
  
  let detection_result = @azimuth.IntelligentAnomalyDetector::detect_anomalies(anomaly_detector, detection_request, all_data)
  
  // 验证检测结果
  assert_true(detection_result.success)
  assert_true(detection_result.anomalies.length() >= 3) // 至少检测到3种类型的异常
  
  // 验证峰值异常检测
  let spike_anomalies = detection_result.anomalies.filter(fn(anomaly) {
    anomaly.anomaly_type == @azimuth.AnomalyType::Spike("")
  })
  assert_true(spike_anomalies.length() >= 1)
  
  for spike_anomaly in spike_anomalies {
    assert_true(spike_anomaly.anomaly_score >= 0.8)
    assert_true(spike_anomaly.duration_minutes >= 5)
    assert_true(spike_anomaly.start_time >= current_time + (2001 * 60000000000))
    assert_true(spike_anomaly.end_time <= current_time + (2050 * 60000000000))
  }
  
  // 验证下降异常检测
  let drop_anomalies = detection_result.anomalies.filter(fn(anomaly) {
    anomaly.anomaly_type == @azimuth.AnomalyType::Drop("")
  })
  assert_true(drop_anomalies.length() >= 1)
  
  for drop_anomaly in drop_anomalies {
    assert_true(drop_anomaly.anomaly_score >= 0.8)
    assert_true(drop_anomaly.start_time >= current_time + (2051 * 60000000000))
    assert_true(drop_anomaly.end_time <= current_time + (2100 * 60000000000))
  }
  
  // 验证趋势变化异常检测
  let trend_anomalies = detection_result.anomalies.filter(fn(anomaly) {
    anomaly.anomaly_type == @azimuth.AnomalyType::TrendChange("")
  })
  assert_true(trend_anomalies.length() >= 1)
  
  // 测试异常模式学习
  let learning_request = @azimuth.PatternLearningRequest::new()
    .with_historical_data(normal_data)
    .with_anomaly_patterns(detection_result.anomalies)
    .with_learning_algorithm(@azimuth.LearningAlgorithm::Unsupervised)
    .with_pattern_validation(true)
  
  let learning_result = @azimuth.IntelligentAnomalyDetector::learn_patterns(anomaly_detector, learning_request)
  
  // 验证学习结果
  assert_true(learning_result.success)
  assert_true(learning_result.learned_patterns.length() >= 3)
  assert_true(learning_result.pattern_accuracy >= 0.7)
  
  // 测试预测性异常检测
  let predictive_request = @azimuth.PredictiveDetectionRequest::new()
    .with_learned_patterns(learning_result.learned_patterns)
    .with_prediction_horizon(@azimuth.Duration::Hours(1))
    .with_confidence_threshold(0.75)
  
  let predictive_result = @azimuth.IntelligentAnomalyDetector::predict_anomalies(anomaly_detector, predictive_request, all_data)
  
  // 验证预测结果
  assert_true(predictive_result.success)
  assert_true(predictive_result.predicted_anomalies.length() >= 0)
  
  for predicted_anomaly in predictive_result.predicted_anomalies {
    assert_true(predicted_anomaly.confidence_score >= 0.75)
    assert_true(predicted_anomaly.prediction_time > current_time)
    assert_true(predicted_anomaly.expected_duration_minutes > 0)
  }
  
  // 测试异常根因分析
  let root_cause_request = @azimuth.RootCauseAnalysisRequest::new()
    .with_anomalies(detection_result.anomalies)
    .with_context_data([
      @azimuth.ContextData::new("deployment.events", ["new-version-deployed"]),
      @azimuth.ContextData::new("system.events", ["high-traffic-period"]),
      @azimuth.ContextData::new("configuration.changes", ["memory-limit-increased"])
    ])
    .with_analysis_depth(@azimuth.AnalysisDepth::Deep)
  
  let root_cause_result = @azimuth.IntelligentAnomalyDetector::analyze_root_causes(anomaly_detector, root_cause_request)
  
  // 验证根因分析结果
  assert_true(root_cause_result.success)
  assert_true(root_cause_result.root_causes.length() >= 1)
  
  for root_cause in root_cause_result.root_causes {
    assert_true(root_cause.confidence_score >= 0.5)
    assert_true(root_cause.causal_factors.length() >= 1)
    assert_true(root_cause.recommendations.length() >= 1)
  }
}

// Test 3: 性能基准分析和对比
test "performance benchmarking and comparative analysis" {
  // 创建性能基准分析器
  let performance_benchmark = @azimuth.PerformanceBenchmarkAnalyzer::new()
  
  // 配置基准指标
  let benchmark_metrics = [
    @azimuth.BenchmarkMetric::new("response_time", "响应时间", @azimuth.Unit::Milliseconds, @azimuth.AggregationType::Percentile, 95),
    @azimuth.BenchmarkMetric::new("throughput", "吞吐量", @azimuth.Unit::RequestsPerSecond, @azimuth.AggregationType::Mean, 0),
    @azimuth.BenchmarkMetric::new("error_rate", "错误率", @azimuth.Unit::Percentage, @azimuth.AggregationType::Mean, 0),
    @azimuth.BenchmarkMetric::new("cpu_utilization", "CPU利用率", @azimuth.Unit::Percentage, @azimuth.AggregationType::Max, 0),
    @azimuth.BenchmarkMetric::new("memory_utilization", "内存利用率", @azimuth.Unit::Percentage, @azimuth.AggregationType::Max, 0),
    @azimuth.BenchmarkMetric::new("disk_io", "磁盘IO", @azimuth.Unit::MegabytesPerSecond, @azimuth.AggregationType::Mean, 0)
  ]
  
  @azimuth.PerformanceBenchmarkAnalyzer::configure_metrics(performance_benchmark, benchmark_metrics)
  
  // 创建基准数据集
  let baseline_dataset = @azimuth.BenchmarkDataset::new("baseline", "基线性能")
  
  // 生成基线性能数据
  let current_time = @azimuth.Time::now_unix_nanos()
  let mut baseline_data = []
  
  for i in 0..=1000 {
    let timestamp = current_time + (i * 60000000000) // 每分钟一个数据点
    
    let baseline_point = @azimuth.PerformanceDataPoint::new(timestamp, [
      ("response_time", @azimuth.FloatValue(100.0 + @azimuth.Math::random() * 20.0)),
      ("throughput", @azimuth.FloatValue(1000.0 + @azimuth.Math::random() * 100.0)),
      ("error_rate", @azimuth.FloatValue(0.5 + @azimuth.Math::random() * 0.5)),
      ("cpu_utilization", @azimuth.FloatValue(60.0 + @azimuth.Math::random() * 10.0)),
      ("memory_utilization", @azimuth.FloatValue(70.0 + @azimuth.Math::random() * 10.0)),
      ("disk_io", @azimuth.FloatValue(50.0 + @azimuth.Math::random() * 10.0))
    ])
    baseline_data = baseline_data.push(baseline_point)
  }
  
  @azimuth.BenchmarkDataset::add_data(baseline_dataset, baseline_data)
  
  // 创建测试版本数据集
  let test_dataset = @azimuth.BenchmarkDataset::new("v2.0", "版本2.0性能")
  let mut test_data = []
  
  for i in 0..=1000 {
    let timestamp = current_time + (i * 60000000000)
    
    // 模拟性能改进：响应时间降低，吞吐量提高
    let test_point = @azimuth.PerformanceDataPoint::new(timestamp, [
      ("response_time", @azimuth.FloatValue(80.0 + @azimuth.Math::random() * 15.0)), // 改进
      ("throughput", @azimuth.FloatValue(1200.0 + @azimuth.Math::random() * 120.0)), // 改进
      ("error_rate", @azimuth.FloatValue(0.3 + @azimuth.Math::random() * 0.3)), // 改进
      ("cpu_utilization", @azimuth.FloatValue(55.0 + @azimuth.Math::random() * 10.0)), // 改进
      ("memory_utilization", @azimuth.FloatValue(75.0 + @azimuth.Math::random() * 10.0)), // 轻微退化
      ("disk_io", @azimuth.FloatValue(45.0 + @azimuth.Math::random() * 10.0)) // 改进
    ])
    test_data = test_data.push(test_point)
  }
  
  @azimuth.BenchmarkDataset::add_data(test_dataset, test_data)
  
  // 创建对比版本数据集
  let comparison_dataset = @azimuth.BenchmarkDataset::new("v1.9", "版本1.9性能")
  let mut comparison_data = []
  
  for i in 0..=1000 {
    let timestamp = current_time + (i * 60000000000)
    
    // 模拟性能退化：响应时间增加，吞吐量降低
    let comparison_point = @azimuth.PerformanceDataPoint::new(timestamp, [
      ("response_time", @azimuth.FloatValue(120.0 + @azimuth.Math::random() * 25.0)), // 退化
      ("throughput", @azimuth.FloatValue(900.0 + @azimuth.Math::random() * 90.0)), // 退化
      ("error_rate", @azimuth.FloatValue(0.8 + @azimuth.Math::random() * 0.7)), // 退化
      ("cpu_utilization", @azimuth.FloatValue(65.0 + @azimuth.Math::random() * 12.0)), // 退化
      ("memory_utilization", @azimuth.FloatValue(68.0 + @azimuth.Math::random() * 12.0)), // 轻微改进
      ("disk_io", @azimuth.FloatValue(55.0 + @azimuth.Math::random() * 12.0)) // 轻微退化
    ])
    comparison_data = comparison_data.push(comparison_point)
  }
  
  @azimuth.BenchmarkDataset::add_data(comparison_dataset, comparison_data)
  
  // 执行基准分析
  let benchmark_request = @azimuth.BenchmarkAnalysisRequest::new()
    .with_baseline_dataset(baseline_dataset)
    .with_comparison_datasets([test_dataset, comparison_dataset])
    .with_metrics(["response_time", "throughput", "error_rate", "cpu_utilization", "memory_utilization", "disk_io"])
    .with_statistical_significance(0.95) // 95%统计显著性
    .with_effect_size_threshold(0.2) // 20%效应大小阈值
  
  let benchmark_result = @azimuth.PerformanceBenchmarkAnalyzer::execute_analysis(performance_benchmark, benchmark_request)
  
  // 验证基准分析结果
  assert_true(benchmark_result.success)
  assert_eq(benchmark_result.dataset_comparisons.length(), 2)
  
  // 验证v2.0与基线的对比
  let v2_comparison = benchmark_result.dataset_comparisons.find(fn(comp) {
    comp.dataset_name == "v2.0"
  })
  match v2_comparison {
    Some(comparison) => {
      // 验证响应时间改进
      let response_time_change = comparison.metric_changes.get("response_time")
      match response_time_change {
        Some(change) => {
          assert_true(change.percent_change < 0) // 应该是负值（改进）
          assert_true(change.absolute_change < 0)
          assert_true(change.is_statistically_significant)
          assert_true(change.effect_size > 0.2)
        }
        None => assert_true(false)
      }
      
      // 验证吞吐量改进
      let throughput_change = comparison.metric_changes.get("throughput")
      match throughput_change {
        Some(change) => {
          assert_true(change.percent_change > 0) // 应该是正值（改进）
          assert_true(change.absolute_change > 0)
          assert_true(change.is_statistically_significant)
        }
        None => assert_true(false)
      }
      
      // 验证错误率改进
      let error_rate_change = comparison.metric_changes.get("error_rate")
      match error_rate_change {
        Some(change) => {
          assert_true(change.percent_change < 0) // 应该是负值（改进）
          assert_true(change.absolute_change < 0)
        }
        None => assert_true(false)
      }
      
      // 验证总体评估
      assert_eq(comparison.overall_assessment, @azimuth.PerformanceAssessment::Improved)
      assert_true(comparison.confidence_score >= 0.8)
    }
    None => assert_true(false)
  }
  
  // 验证v1.9与基线的对比
  let v1_9_comparison = benchmark_result.dataset_comparisons.find(fn(comp) {
    comp.dataset_name == "v1.9"
  })
  match v1_9_comparison {
    Some(comparison) => {
      // 验证总体评估
      assert_eq(comparison.overall_assessment, @azimuth.PerformanceAssessment::Degraded)
      assert_true(comparison.confidence_score >= 0.7)
    }
    None => assert_true(false)
  }
  
  // 测试性能趋势分析
  let trend_analysis_request = @azimuth.TrendAnalysisRequest::new()
    .with_datasets([baseline_dataset, test_dataset, comparison_dataset])
    .with_time_range(current_time, current_time + (1000 * 60000000000))
    .with_trend_detection_algorithm(@azimuth.TrendAlgorithm::LinearRegression)
    .with_seasonality_detection(true)
  
  let trend_analysis_result = @azimuth.PerformanceBenchmarkAnalyzer::analyze_trends(performance_benchmark, trend_analysis_request)
  
  // 验证趋势分析结果
  assert_true(trend_analysis_result.success)
  assert_true(trend_analysis_result.detected_trends.length() >= 1)
  
  for trend in trend_analysis_result.detected_trends {
    assert_true(trend.confidence_score >= 0.5)
    assert_true(trend.trend_strength >= 0.0)
    assert_true(trend.trend_direction == @azimuth.TrendDirection::Increasing || 
               trend.trend_direction == @azimuth.TrendDirection::Decreasing ||
               trend.trend_direction == @azimuth.TrendDirection::Stable)
  }
  
  // 测试性能回归检测
  let regression_detection_request = @azimuth.RegressionDetectionRequest::new()
    .with_baseline_dataset(baseline_dataset)
    .with_test_dataset(test_dataset)
    .with_regression_threshold(0.05) // 5%回归阈值
    .with_critical_metrics(["response_time", "error_rate"])
  
  let regression_result = @azimuth.PerformanceBenchmarkAnalyzer::detect_regressions(performance_benchmark, regression_detection_request)
  
  // 验证回归检测结果
  assert_true(regression_result.success)
  assert_false(regression_result.has_regressions) // v2.0应该没有回归
  
  // 测试性能改进建议
  let improvement_request = @azimuth.ImprovementAnalysisRequest::new()
    .with_underperforming_dataset(comparison_dataset)
    .with_baseline_dataset(baseline_dataset)
    .with_improvement_targets([
      @azimuth.ImprovementTarget::new("response_time", @azimuth.TargetType::Reduce, 20.0),
      @azimuth.ImprovementTarget::new("throughput", @azimuth.TargetType::Increase, 15.0),
      @azimuth.ImprovementTarget::new("error_rate", @azimuth.TargetType::Reduce, 50.0)
    ])
  
  let improvement_result = @azimuth.PerformanceBenchmarkAnalyzer::generate_improvement_recommendations(performance_benchmark, improvement_request)
  
  // 验证改进建议
  assert_true(improvement_result.success)
  assert_true(improvement_result.recommendations.length() >= 1)
  
  for recommendation in improvement_result.recommendations {
    assert_true(recommendation.priority >= 1 && recommendation.priority <= 5)
    assert_true(recommendation.estimated_impact > 0.0)
    assert_true(recommendation.implementation_effort >= 1 && recommendation.implementation_effort <= 5)
    assert_true(recommendation.description.length() > 0)
  }
}

// Test 4: 业务影响分析和ROI计算
test "business impact analysis and ROI calculation" {
  // 创建业务影响分析器
  let business_impact_analyzer = @azimuth.BusinessImpactAnalyzer::new()
  
  // 配置业务指标
  let business_metrics = [
    @azimuth.BusinessMetric::new("revenue", "收入", @azimuth.MetricType::Currency, @azimuth.Currency::USD),
    @azimuth.BusinessMetric::new("user_satisfaction", "用户满意度", @azimuth.MetricType::Score, @azimuth.Currency::None),
    @azimuth.BusinessMetric::new("customer_retention", "客户保留率", @azimuth.MetricType::Percentage, @azimuth.Currency::None),
    @azimuth.BusinessMetric::new("operational_cost", "运营成本", @azimuth.MetricType::Currency, @azimuth.Currency::USD),
    @azimuth.BusinessMetric::new("conversion_rate", "转化率", @azimuth.MetricType::Percentage, @azimuth.Currency::None)
  ]
  
  @azimuth.BusinessImpactAnalyzer::configure_business_metrics(business_impact_analyzer, business_metrics)
  
  // 配置技术到业务的映射
  let tech_to_business_mappings = [
    @azimuth.TechToBusinessMapping::new("response_time", "user_satisfaction", @azimuth.MappingType::InverseCorrelation, 0.8),
    @azimuth.TechToBusinessMapping::new("error_rate", "user_satisfaction", @azimuth.MappingType::InverseCorrelation, 0.9),
    @azimuth.TechToBusinessMapping::new("availability", "revenue", @azimuth.MappingType::DirectCorrelation, 0.95),
    @azimuth.TechToBusinessMapping::new("response_time", "conversion_rate", @azimuth.MappingType::InverseCorrelation, 0.7),
    @azimuth.TechToBusinessMapping::new("cpu_utilization", "operational_cost", @azimuth.MappingType::DirectCorrelation, 0.6)
  ]
  
  @azimuth.BusinessImpactAnalyzer::configure_mappings(business_impact_analyzer, tech_to_business_mappings)
  
  // 创建技术性能数据
  let current_time = @azimuth.Time::now_unix_nanos()
  let mut technical_performance_data = []
  
  for i in 0..=720 { // 12小时的数据，每小时一个数据点
    let timestamp = current_time + (i * 3600000000000) // 每小时
    
    let performance_point = @azimuth.TechnicalPerformanceData::new(timestamp, [
      ("response_time", @azimuth.FloatValue(100.0 + (i % 24) * 10.0)), // 响应时间变化
      ("error_rate", @azimuth.FloatValue(0.5 + (i % 12) * 0.2)), // 错误率变化
      ("availability", @azimuth.FloatValue(99.5 + (i % 10) * 0.05)), // 可用性变化
      ("cpu_utilization", @azimuth.FloatValue(60.0 + (i % 8) * 5.0)), // CPU利用率变化
      ("throughput", @azimuth.FloatValue(1000.0 + (i % 20) * 50.0)) // 吞吐量变化
    ])
    technical_performance_data = technical_performance_data.push(performance_point)
  }
  
  // 创建业务结果数据
  let mut business_outcome_data = []
  
  for i in 0..=720 {
    let timestamp = current_time + (i * 3600000000000)
    
    // 基于技术性能模拟业务结果
    let response_time_factor = 1.0 - (technical_performance_data.get(i).unwrap().metrics.get("response_time").unwrap() - 100.0) / 200.0
    let error_rate_factor = 1.0 - technical_performance_data.get(i).unwrap().metrics.get("error_rate").unwrap() * 5.0
    let availability_factor = technical_performance_data.get(i).unwrap().metrics.get("availability").unwrap() / 100.0
    
    let business_point = @azimuth.BusinessOutcomeData::new(timestamp, [
      ("revenue", @azimuth.FloatValue(10000.0 * availability_factor * (0.8 + response_time_factor * 0.2))),
      ("user_satisfaction", @azimuth.FloatValue(4.0 * response_time_factor * error_rate_factor)),
      ("customer_retention", @azimuth.FloatValue(0.85 * error_rate_factor * (0.9 + availability_factor * 0.1))),
      ("operational_cost", @azimuth.FloatValue(5000.0 * (1.0 + (technical_performance_data.get(i).unwrap().metrics.get("cpu_utilization").unwrap() - 60.0) / 100.0))),
      ("conversion_rate", @azimuth.FloatValue(0.05 * response_time_factor * availability_factor))
    ])
    business_outcome_data = business_outcome_data.push(business_point)
  }
  
  // 执行业务影响分析
  let impact_analysis_request = @azimuth.BusinessImpactAnalysisRequest::new()
    .with_technical_data(technical_performance_data)
    .with_business_data(business_outcome_data)
    .with_analysis_period(@azimuth.TimePeriod::Hours(12))
    .with_correlation_threshold(0.5)
  
  let impact_analysis_result = @azimuth.BusinessImpactAnalyzer::analyze_impact(business_impact_analyzer, impact_analysis_request)
  
  // 验证业务影响分析结果
  assert_true(impact_analysis_result.success)
  assert_true(impact_analysis_result.identified_impacts.length() >= 3)
  
  // 验证技术指标对业务指标的影响
  for impact in impact_analysis_result.identified_impacts {
    assert_true(impact.technical_metric.length() > 0)
    assert_true(impact.business_metric.length() > 0)
    assert_true(impact.correlation_coefficient >= -1.0 && impact.correlation_coefficient <= 1.0)
    assert_true(impact.business_impact_score >= 0.0 && impact.business_impact_score <= 1.0)
    assert_true(impact.financial_impact >= 0.0)
  }
  
  // 测试ROI计算
  let roi_calculation_request = @azimuth.ROICalculationRequest::new()
    .with_investment_data([
      @azimuth.InvestmentItem::new("performance_optimization", 50000.0, @azimuth.InvestmentType::Capital),
      @azimuth.InvestmentItem::new("infrastructure_upgrade", 30000.0, @azimuth.InvestmentType::Operational),
      @azimuth.InvestmentItem::new("team_training", 10000.0, @azimuth.InvestmentType::HumanCapital)
    ])
    .with_benefit_data([
      @azimuth.BenefitItem::new("revenue_increase", 80000.0, @azimuth.BenefitType::Direct),
      @azimuth.BenefitItem::new("cost_reduction", 25000.0, @azimuth.BenefitType::Direct),
      @azimuth.BenefitItem::new("customer_satisfaction_improvement", 15000.0, @azimuth.BenefitType::Indirect),
      @azimuth.BenefitItem::new("operational_efficiency", 20000.0, @azimuth.BenefitType::Indirect)
    ])
    .with_time_horizon(@azimuth.TimePeriod::Months(12))
    .with_discount_rate(0.1) // 10%折现率
  
  let roi_result = @azimuth.BusinessImpactAnalyzer::calculate_roi(business_impact_analyzer, roi_calculation_request)
  
  // 验证ROI计算结果
  assert_true(roi_result.success)
  assert_true(roi_result.total_investment == 90000.0)
  assert_true(roi_result.total_benefits == 140000.0)
  assert_true(roi_result.net_benefit == 50000.0)
  assert_true(roi_result.roi_percentage > 0.0)
  assert_true(roi_result.payback_period_months > 0)
  assert_true(roi_result.npv >= 0.0) // 净现值应该非负
  
  // 测试what-if分析
  let what_if_scenarios = [
    @azimuth.WhatIfScenario::new("optimistic", "乐观场景", [
      @azimuth.TechnicalParameterChange::new("response_time", @azimuth.ChangeType::Decrease, 30.0),
      @azimuth.TechnicalParameterChange::new("error_rate", @azimuth.ChangeType::Decrease, 50.0)
    ]),
    @azimuth.WhatIfScenario::new("pessimistic", "悲观场景", [
      @azimuth.TechnicalParameterChange::new("response_time", @azimuth.ChangeType::Increase, 20.0),
      @azimuth.TechnicalParameterChange::new("error_rate", @azimuth.ChangeType::Increase, 30.0)
    ]),
    @azimuth.WhatIfScenario::new("realistic", "现实场景", [
      @azimuth.TechnicalParameterChange::new("response_time", @azimuth.ChangeType::Decrease, 15.0),
      @azimuth.TechnicalParameterChange::new("error_rate", @azimuth.ChangeType::Decrease, 25.0)
    ])
  ]
  
  let what_if_request = @azimuth.WhatIfAnalysisRequest::new()
    .with_baseline_technical_data(technical_performance_data)
    .with_baseline_business_data(business_outcome_data)
    .with_scenarios(what_if_scenarios)
    .with_prediction_model(@azimuth.PredictionModel::LinearRegression)
  
  let what_if_result = @azimuth.BusinessImpactAnalyzer::analyze_what_if(business_impact_analyzer, what_if_request)
  
  // 验证what-if分析结果
  assert_true(what_if_result.success)
  assert_eq(what_if_result.scenario_results.length(), 3)
  
  for scenario_result in what_if_result.scenario_results {
    assert_true(scenario_result.predicted_business_impacts.length() >= 1)
    assert_true(scenario_result.confidence_score >= 0.0 && scenario_result.confidence_score <= 1.0)
    
    // 验证乐观场景的业务影响应该最好
    if scenario_result.scenario_name == "optimistic" {
      let optimistic_revenue = scenario_result.predicted_business_impacts.find(fn(impact) {
        impact.metric == "revenue"
      })
      match optimistic_revenue {
        Some(impact) => assert_true(impact.predicted_change > 0.0)
        None => assert_true(false)
      }
    }
    
    // 验证悲观场景的业务影响应该最差
    if scenario_result.scenario_name == "pessimistic" {
      let pessimistic_revenue = scenario_result.predicted_business_impacts.find(fn(impact) {
        impact.metric == "revenue"
      })
      match pessimistic_revenue {
        Some(impact) => assert_true(impact.predicted_change < 0.0)
        None => assert_true(false)
      }
    }
  }
  
  // 测试投资优先级分析
  let investment_prioritization_request = @azimuth.InvestmentPrioritizationRequest::new()
    .with_investment_options([
      @azimuth.InvestmentOption::new("response_time_optimization", 30000.0, [
        @azimuth.ExpectedBenefit::new("user_satisfaction", 0.2, @azimuth.BenefitType::Indirect),
        @azimuth.ExpectedBenefit::new("conversion_rate", 0.15, @azimuth.BenefitType::Direct)
      ]),
      @azimuth.InvestmentOption::new("error_reduction", 25000.0, [
        @azimuth.ExpectedBenefit::new("customer_retention", 0.1, @azimuth.BenefitType::Direct),
        @azimuth.ExpectedBenefit::new("user_satisfaction", 0.25, @azimuth.BenefitType::Indirect)
      ]),
      @azimuth.InvestmentOption::new("infrastructure_scaling", 50000.0, [
        @azimuth.ExpectedBenefit::new("revenue", 0.3, @azimuth.BenefitType::Direct),
        @azimuth.ExpectedBenefit::new("operational_cost", -0.1, @azimuth.BenefitType::Direct)
      ])
    ])
    .with_budget_constraint(80000.0)
    .with_prioritization_method(@azimuth.PrioritizationMethod::ROI)
  
  let prioritization_result = @azimuth.BusinessImpactAnalyzer::prioritize_investments(business_impact_analyzer, investment_prioritization_request)
  
  // 验证投资优先级结果
  assert_true(prioritization_result.success)
  assert_true(prioritization_result.recommended_investments.length() >= 1)
  assert_true(prioritization_result.total_investment <= 80000.0)
  assert_true(prioritization_result.expected_roi > 0.0)
  
  // 验证推荐投资按ROI排序
  let mut previous_roi = 999.0
  for investment in prioritization_result.recommended_investments {
    assert_true(investment.expected_roi <= previous_roi)
    previous_roi = investment.expected_roi
  }
}