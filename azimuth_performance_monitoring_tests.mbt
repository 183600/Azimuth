// Azimuth 性能监控和度量测试
// 专注于系统性能监控、度量收集和性能分析功能

// 测试1: 性能度量收集器
test "性能度量收集器基础功能" {
  // 创建性能度量收集器
  let metric_collector = PerformanceMetricCollector::new()
  
  // 配置收集器
  PerformanceMetricCollector::set_sampling_interval(metric_collector, 1000)  // 1秒
  PerformanceMetricCollector::set_retention_period(metric_collector, 3600000)  // 1小时
  PerformanceMetricCollector::enable_auto_aggregation(metric_collector, true)
  
  // 注册系统度量
  PerformanceMetricCollector::register_cpu_metric(metric_collector, {
    name: "cpu.usage",
    unit: "percent",
    collection_type: "gauge",
    tags: ["system", "performance"]
  })
  
  PerformanceMetricCollector::register_memory_metric(metric_collector, {
    name: "memory.usage",
    unit: "bytes",
    collection_type: "gauge",
    tags: ["system", "performance"]
  })
  
  PerformanceMetricCollector::register_network_metric(metric_collector, {
    name: "network.throughput",
    unit: "bytes_per_second",
    collection_type: "counter",
    tags: ["system", "network"]
  })
  
  PerformanceMetricCollector::register_disk_metric(metric_collector, {
    name: "disk.io",
    unit: "bytes_per_second",
    collection_type: "counter",
    tags: ["system", "disk"]
  })
  
  // 模拟性能数据收集
  let base_time = 1640995200
  
  for i in 0..=60 {
    let timestamp = base_time + i * 1000
    
    // CPU使用率模拟
    let cpu_usage = 30.0 + (i % 20) * 2.5 + (i % 10) * 1.0
    PerformanceMetricCollector::add_metric(metric_collector, timestamp, "cpu.usage", cpu_usage, [
      ("core", StringValue("0")),
      ("instance", StringValue("server-1"))
    ])
    
    // 内存使用率模拟
    let memory_usage = 2147483648.0 + (i % 30) * 10485760.0  // 2GB + 变化
    PerformanceMetricCollector::add_metric(metric_collector, timestamp, "memory.usage", memory_usage, [
      ("type", StringValue("physical")),
      ("instance", StringValue("server-1"))
    ])
    
    // 网络吞吐量模拟
    let network_throughput = 1048576.0 + (i % 15) * 524288.0  // 1MB/s + 变化
    PerformanceMetricCollector::add_metric(metric_collector, timestamp, "network.throughput", network_throughput, [
      ("interface", StringValue("eth0")),
      ("direction", StringValue("outbound"))
    ])
    
    // 磁盘IO模拟
    let disk_io = 524288.0 + (i % 25) * 262144.0  // 512KB/s + 变化
    PerformanceMetricCollector::add_metric(metric_collector, timestamp, "disk.io", disk_io, [
      ("device", StringValue("sda")),
      ("operation", StringValue("read"))
    ])
  }
  
  // 获取度量统计
  let metric_stats = PerformanceMetricCollector::get_metric_stats(metric_collector)
  assert_eq(metric_stats.total_metrics, 4)
  assert_eq(metric_stats.total_data_points, 240)  // 60秒 * 4个指标
  
  // 验证CPU度量
  let cpu_metrics = PerformanceMetricCollector::get_metrics_by_name(metric_collector, "cpu.usage")
  assert_true(cpu_metrics.length() > 0)
  
  let cpu_avg = cpu_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0) / cpu_metrics.length().to_float()
  assert_true(cpu_avg > 30.0 and cpu_avg < 80.0)  // 合理的CPU使用率范围
  
  // 验证内存度量
  let memory_metrics = PerformanceMetricCollector::get_metrics_by_name(metric_collector, "memory.usage")
  assert_true(memory_metrics.length() > 0)
  
  let memory_min = memory_metrics.reduce(fn(acc, m) { if m.value < acc { m.value } else { acc } }, memory_metrics[0].value)
  let memory_max = memory_metrics.reduce(fn(acc, m) { if m.value > acc { m.value } else { acc } }, memory_metrics[0].value)
  assert_true(memory_max > memory_min)
  
  // 测试度量聚合
  let aggregation_window = 10000  // 10秒
  let aggregated_metrics = PerformanceMetricCollector::aggregate_metrics(metric_collector, "cpu.usage", aggregation_window, "avg")
  
  assert_true(aggregated_metrics.length() == 6)  // 60秒 / 10秒 = 6个聚合点
  
  for metric in aggregated_metrics {
    assert_true(metric.value >= 30.0 and metric.value <= 80.0)
  }
}

// 测试2: 应用程序性能监控(APM)
test "应用程序性能监控功能" {
  // 创建APM监控器
  let apm_monitor = ApmMonitor::new()
  
  // 配置APM
  ApmMonitor::set_service_name(apm_monitor, "payment-service")
  ApmMonitor::set_service_version(apm_monitor, "1.2.3")
  ApmMonitor::set_environment(apm_monitor, "production")
  
  // 启用事务跟踪
  ApmMonitor::enable_transaction_tracing(apm_monitor, {
    sampling_rate: 0.1,  // 10%采样率
    max_spans_per_transaction: 100,
    slow_transaction_threshold: 1000  // 1秒
  })
  
  // 启用实时用户监控(RUM)
  ApmMonitor::enable_real_user_monitoring(apm_monitor, {
    sampling_rate: 0.05,  // 5%采样率
    max_user_sessions: 1000
  })
  
  // 创建事务
  let payment_transaction = ApmMonitor::start_transaction(apm_monitor, "payment.process", "web")
  Transaction::set_user_id(payment_transaction, "user-12345")
  Transaction::add_attribute(payment_transaction, "payment.amount", FloatValue(99.99))
  Transaction::add_attribute(payment_transaction, "payment.method", StringValue("credit_card"))
  
  // 添加span
  let validation_span = Transaction::start_span(payment_transaction, "payment.validation", "app")
  Span::add_attribute(validation_span, "validation.type", StringValue("card_validation"))
  // 模拟处理时间
  Time::sleep(50)
  Span::end(validation_span)
  
  let database_span = Transaction::start_span(payment_transaction, "database.query", "db")
  Span::add_attribute(database_span, "db.statement", StringValue("SELECT * FROM users WHERE id = ?"))
  Span::add_attribute(database_span, "db.type", StringValue("postgresql"))
  // 模拟处理时间
  Time::sleep(120)
  Span::end(database_span)
  
  let external_api_span = Transaction::start_span(payment_transaction, "external.api.call", "external")
  Span::add_attribute(external_api_span, "http.url", StringValue("https://api.payment-gateway.com/charge"))
  Span::add_attribute(external_api_span, "http.method", StringValue("POST"))
  // 模拟处理时间
  Time::sleep(200)
  Span::end(external_api_span)
  
  // 结束事务
  Transaction::end(payment_transaction, Transaction::Success)
  
  // 创建错误事务
  let failed_transaction = ApmMonitor::start_transaction(apm_monitor, "payment.process", "web")
  Transaction::set_user_id(failed_transaction, "user-67890")
  
  let error_span = Transaction::start_span(failed_transaction, "payment.processing", "app")
  // 模拟错误
  Span::record_error(error_span, "Payment gateway timeout", {
    error_type: "timeout",
    error_code: "PAYMENT_TIMEOUT",
    stack_trace: "at PaymentService.processPayment\n  at PaymentController.charge\n  ..."
  })
  Span::end(error_span)
  
  Transaction::end(failed_transaction, Transaction::Error)
  
  // 创建慢事务
  let slow_transaction = ApmMonitor::start_transaction(apm_monitor, "report.generate", "web")
  let slow_span = Transaction::start_span(slow_transaction, "data.aggregation", "app")
  // 模拟慢处理
  Time::sleep(1500)  // 1.5秒
  Span::end(slow_span)
  Transaction::end(slow_transaction, Transaction::Success)
  
  // 获取事务统计
  let transaction_stats = ApmMonitor::get_transaction_stats(apm_monitor)
  assert_eq(transaction_stats.total_transactions, 3)
  assert_eq(transaction_stats.successful_transactions, 2)
  assert_eq(transaction_stats.failed_transactions, 1)
  assert_eq(transaction_stats.slow_transactions, 1)
  
  // 验证事务分布
  let payment_transactions = transaction_stats.transactions_by_type.get("payment.process")
  assert_eq(payment_transactions, Some(2))
  
  let report_transactions = transaction_stats.transactions_by_type.get("report.generate")
  assert_eq(report_transactions, Some(1))
  
  // 获取性能指标
  let performance_metrics = ApmMonitor::get_performance_metrics(apm_monitor)
  
  // 验证响应时间指标
  let response_time_metrics = performance_metrics.filter(fn(m) { m.name == "transaction.duration" })
  assert_true(response_time_metrics.length() > 0)
  
  let avg_response_time = response_time_metrics.reduce(fn(acc, m) { acc + m.value }, 0.0) / response_time_metrics.length().to_float()
  assert_true(avg_response_time > 0.0)
  
  // 验证错误率指标
  let error_rate_metrics = performance_metrics.filter(fn(m) { m.name == "transaction.error_rate" })
  assert_true(error_rate_metrics.length() > 0)
  
  let error_rate = error_rate_metrics[0].value
  assert_eq(error_rate, 1.0 / 3.0)  // 1个错误 / 3个总事务
  
  // 验证吞吐量指标
  let throughput_metrics = performance_metrics.filter(fn(m) { m.name == "transaction.throughput" })
  assert_true(throughput_metrics.length() > 0)
  
  let throughput = throughput_metrics[0].value
  assert_true(throughput > 0.0)
}

// 测试3: 性能基线和异常检测
test "性能基线和异常检测功能" {
  // 创建性能基线管理器
  let baseline_manager = PerformanceBaselineManager::new()
  
  // 配置基线
  BaselineManager::configure(baseline_manager, {
    learning_period: 7 * 24 * 3600000,  // 7天学习期
    update_frequency: 3600000,          // 1小时更新频率
    anomaly_threshold: 2.0,             // 2个标准差
    min_data_points: 100                // 最少100个数据点
  })
  
  // 添加基线指标
  BaselineManager::add_metric(baseline_manager, {
    name: "response_time.p95",
    aggregation: "percentile",
    percentile: 95.0,
    unit: "milliseconds"
  })
  
  BaselineManager::add_metric(baseline_manager, {
    name: "error_rate",
    aggregation: "rate",
    unit: "percent"
  })
  
  BaselineManager::add_metric(baseline_manager, {
    name: "throughput",
    aggregation: "sum",
    unit: "requests_per_second"
  })
  
  // 模拟历史数据（基线学习）
  let base_time = 1640995200
  
  for day in 0..=6 {  // 7天
    for hour in 0..=23 {  // 每天24小时
      for minute in 0..=59 {  // 每小时60分钟
        let timestamp = base_time + day * 24 * 3600 + hour * 3600 + minute * 60
        
        // 响应时间模式：工作时间和非工作时间不同
        let base_response_time = if hour >= 9 and hour <= 17 { 100.0 } else { 50.0 }
        let response_time = base_response_time + (minute % 20) * 2.0 + (hour % 3) * 5.0
        
        // 错误率模式：通常较低，偶尔有峰值
        let base_error_rate = if hour == 14 and minute == 30 { 5.0 } else { 0.5 }
        let error_rate = base_error_rate + (minute % 10) * 0.1
        
        // 吞吐量模式：工作时间高，非工作时间低
        let base_throughput = if hour >= 9 and hour <= 17 { 100.0 } else { 20.0 }
        let throughput = base_throughput + (minute % 15) * 2.0
        
        BaselineManager::add_data_point(baseline_manager, timestamp, "response_time.p95", response_time)
        BaselineManager::add_data_point(baseline_manager, timestamp, "error_rate", error_rate)
        BaselineManager::add_data_point(baseline_manager, timestamp, "throughput", throughput)
      }
    }
  }
  
  // 计算基线
  let baseline_result = BaselineManager::calculate_baseline(baseline_manager)
  assert_true(baseline_result.success)
  assert_eq(baseline_result.metrics.length(), 3)
  
  // 验证响应时间基线
  let response_time_baseline = baseline_result.metrics.find(fn(m) { m.name == "response_time.p95" })
  assert_true(response_time_baseline != None)
  
  match response_time_baseline {
    Some(baseline) => {
      assert_true(baseline.mean > 0.0)
      assert_true(baseline.std_dev > 0.0)
      assert_true(baseline.upper_bound > baseline.mean)
      assert_true(baseline.lower_bound < baseline.mean)
    }
    None => assert_true(false)
  }
  
  // 验证错误率基线
  let error_rate_baseline = baseline_result.metrics.find(fn(m) { m.name == "error_rate" })
  assert_true(error_rate_baseline != None)
  
  match error_rate_baseline {
    Some(baseline) => {
      assert_true(baseline.mean >= 0.0)
      assert_true(baseline.upper_bound > baseline.mean)
      assert_true(baseline.lower_bound >= 0.0)
    }
    None => assert_true(false)
  }
  
  // 验证吞吐量基线
  let throughput_baseline = baseline_result.metrics.find(fn(m) { m.name == "throughput" })
  assert_true(throughput_baseline != None)
  
  match throughput_baseline {
    Some(baseline) => {
      assert_true(baseline.mean > 0.0)
      assert_true(baseline.upper_bound > baseline.mean)
      assert_true(baseline.lower_bound < baseline.mean)
    }
    None => assert_true(false)
  }
  
  // 创建异常检测器
  let anomaly_detector = PerformanceAnomalyDetector::new()
  AnomalyDetector::set_baseline(anomaly_detector, baseline_result)
  
  // 测试正常数据
  let normal_data = [
    ("response_time.p95", 105.0),
    ("error_rate", 0.6),
    ("throughput", 105.0)
  ]
  
  let normal_result = AnomalyDetector::detect(anomaly_detector, normal_data)
  assert_false(normal_result.is_anomaly)
  assert_eq(normal_result.anomalies.length(), 0)
  
  // 测试异常数据
  let anomalous_data = [
    ("response_time.p95", 500.0),  // 异常高响应时间
    ("error_rate", 0.8),           // 正常错误率
    ("throughput", 95.0)           // 正常吞吐量
  ]
  
  let anomaly_result = AnomalyDetector::detect(anomaly_detector, anomalous_data)
  assert_true(anomaly_result.is_anomaly)
  assert_true(anomaly_result.anomalies.length() > 0)
  
  // 验证响应时间异常
  let response_time_anomaly = anomaly_result.anomalies.find(fn(a) { a.metric == "response_time.p95" })
  assert_true(response_time_anomaly != None)
  
  match response_time_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.severity, "high")
      assert_true(anomaly.description.contains("response_time.p95"))
      assert_true(anomaly.deviation > 2.0)  // 超过2个标准差
    }
    None => assert_true(false)
  }
  
  // 测试多指标异常
  let multi_anomaly_data = [
    ("response_time.p95", 400.0),  // 异常高响应时间
    ("error_rate", 10.0),          // 异常高错误率
    ("throughput", 15.0)           // 异常低吞吐量
  ]
  
  let multi_anomaly_result = AnomalyDetector::detect(anomaly_detector, multi_anomaly_data)
  assert_true(multi_anomaly_result.is_anomaly)
  assert_true(multi_anomaly_result.anomalies.length() >= 2)
  
  // 验证综合异常评分
  assert_true(multi_anomaly_result.overall_score > anomaly_result.overall_score)
}

// 测试4: 性能趋势分析
test "性能趋势分析功能" {
  // 创建趋势分析器
  let trend_analyzer = PerformanceTrendAnalyzer::new()
  
  // 配置分析参数
  TrendAnalyzer::configure(trend_analyzer, {
    analysis_window: 7 * 24 * 3600000,  // 7天分析窗口
    granularities: [3600000, 86400000], // 1小时和1天粒度
    trend_threshold: 0.1,               // 10%变化阈值
    seasonality_detection: true
  })
  
  // 生成测试数据
  let base_time = 1640995200
  
  // 生成30天的数据
  for day in 0..=29 {
    for hour in 0..=23 {
      let timestamp = base_time + day * 24 * 3600 + hour * 3600
      
      // 响应时间：逐渐增加趋势
      let base_response_time = 100.0 + day * 2.0  // 每天增加2ms
      let response_time = base_response_time + (hour % 10) * 5.0
      
      // 错误率：周期性变化
      let error_rate = 0.5 + (day % 7) * 0.2 + (hour % 24) * 0.02
      
      // 吞吐量：周末下降
      let base_throughput = if day % 7 >= 5 { 50.0 } else { 100.0 }  // 周末下降
      let throughput = base_throughput + (hour % 8) * 5.0
      
      TrendAnalyzer::add_data_point(trend_analyzer, timestamp, "response_time.avg", response_time)
      TrendAnalyzer::add_data_point(trend_analyzer, timestamp, "error_rate", error_rate)
      TrendAnalyzer::add_data_point(trend_analyzer, timestamp, "throughput", throughput)
    }
  }
  
  // 执行趋势分析
  let trend_analysis = TrendAnalyzer::analyze(trend_analyzer)
  assert_true(trend_analysis.metrics.length() == 3)
  
  // 验证响应时间趋势
  let response_time_trend = trend_analysis.metrics.find(fn(m) { m.name == "response_time.avg" })
  assert_true(response_time_trend != None)
  
  match response_time_trend {
    Some(trend) => {
      assert_eq(trend.direction, "increasing")  // 逐渐增加
      assert_true(trend.magnitude > 0.0)
      assert_true(trend.confidence > 0.5)  // 高置信度
      
      // 验证趋势强度
      assert_true(trend.strength > 0.5)  // 强趋势
      
      // 验证预测
      assert_true(trend.forecast.length() > 0)
      assert_true(trend.forecast[0].value > trend.current_value)
    }
    None => assert_true(false)
  }
  
  // 验证错误率趋势
  let error_rate_trend = trend_analysis.metrics.find(fn(m) { m.name == "error_rate" })
  assert_true(error_rate_trend != None)
  
  match error_rate_trend {
    Some(trend) => {
      assert_eq(trend.direction, "stable")  // 周期性变化，整体稳定
      assert_true(trend.seasonal)  // 检测到季节性
      
      // 验证季节性模式
      assert_true(trend.seasonality_pattern.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证吞吐量趋势
  let throughput_trend = trend_analysis.metrics.find(fn(m) { m.name == "throughput" })
  assert_true(throughput_trend != None)
  
  match throughput_trend {
    Some(trend) => {
      assert_eq(trend.direction, "decreasing")  // 周末导致整体下降趋势
      assert_true(trend.magnitude < 0.0)
      
      // 验证周期性
      assert_true(trend.seasonal)
      assert_true(trend.seasonality_pattern.length() == 7)  // 7天周期
    }
    None => assert_true(false)
  }
  
  // 测试变化点检测
  let change_point_analysis = TrendAnalyzer::detect_change_points(trend_analyzer, "response_time.avg")
  assert_true(change_point_analysis.change_points.length() >= 0)
  
  // 测试异常周期检测
  let seasonal_patterns = TrendAnalyzer::detect_seasonal_patterns(trend_analyzer, "throughput")
  assert_true(seasonal_patterns.length() > 0)
  
  let weekly_pattern = seasonal_patterns.find(fn(p) { p.period == 7 * 24 * 3600 })
  assert_true(weekly_pattern != None)
  
  match weekly_pattern {
    Some(pattern) => {
      assert_true(pattern.strength > 0.5)  // 强季节性
      assert_eq(pattern.period, 7 * 24 * 3600)  // 7天周期
    }
    None => assert_true(false)
  }
  
  // 测试预测准确性
  let forecast_accuracy = TrendAnalyzer::validate_forecast(trend_analyzer, "response_time.avg", 24 * 3600)
  assert_true(forecast_accuracy.mae > 0.0)  // 平均绝对误差
  assert_true(forecast_accuracy.rmse > 0.0)  // 均方根误差
  assert_true(forecast_accuracy.mape < 0.2)  // 平均绝对百分比误差 < 20%
}

// 测试5: 性能基准测试
test "性能基准测试功能" {
  // 创建基准测试管理器
  let benchmark_manager = BenchmarkManager::new()
  
  // 配置基准测试
  BenchmarkManager::configure(benchmark_manager, {
    warmup_iterations: 10,
    measurement_iterations: 100,
    cooldown_time: 1000,
    statistical_significance: 0.95
  })
  
  // 创建基准测试用例
  let database_benchmark = BenchmarkManager::create_benchmark(benchmark_manager, {
    name: "database_query_performance",
    description: "数据库查询性能基准测试",
    setup: fn() {
      // 设置测试数据库连接
      DatabaseConnection::connect("postgresql://localhost:5432/test")
    },
    teardown: fn(conn) {
      // 清理测试数据
      conn.close()
    },
    benchmark_fn: fn(conn) {
      // 执行测试查询
      conn.execute("SELECT * FROM users WHERE id = $1", [12345])
    }
  })
  
  let api_benchmark = BenchmarkManager::create_benchmark(benchmark_manager, {
    name: "api_response_performance",
    description: "API响应性能基准测试",
    setup: fn() {
      // 设置API客户端
      ApiClient::new("https://api.example.com")
    },
    teardown: fn(client) {
      // 清理资源
      client.close()
    },
    benchmark_fn: fn(client) {
      // 执行API调用
      client.get("/users/12345")
    }
  })
  
  let serialization_benchmark = BenchmarkManager::create_benchmark(benchmark_manager, {
    name: "json_serialization_performance",
    description: "JSON序列化性能基准测试",
    setup: fn() {
      // 创建测试数据
      {
        users: (0..=1000).map(fn(i) {
          {
            id: i,
            name: "User " + i.to_string(),
            email: "user" + i.to_string() + "@example.com",
            created_at: "2023-01-01T00:00:00Z"
          }
        }),
        metadata: {
          total: 1000,
          page: 1,
          per_page: 1000
        }
      }
    },
    teardown: fn(_data) {
      // 无需清理
    },
    benchmark_fn: fn(data) {
      // 序列化为JSON
      Json::serialize(data)
    }
  })
  
  // 运行基准测试
  let database_result = BenchmarkManager::run_benchmark(benchmark_manager, database_benchmark)
  assert_true(database_result.success)
  assert_eq(database_result.iterations, 100)
  assert_true(database_result.mean_time > 0.0)
  assert_true(database_result.min_time > 0.0)
  assert_true(database_result.max_time >= database_result.min_time)
  assert_true(database_result.std_dev > 0.0)
  
  // 验证结果统计
  assert_true(database_result.percentiles.length() >= 3)
  assert_true(database_result.percentiles.any(fn(p) { p.percentile == 50.0 }))
  assert_true(database_result.percentiles.any(fn(p) { p.percentile == 95.0 }))
  assert_true(database_result.percentiles.any(fn(p) { p.percentile == 99.0 }))
  
  // 验证百分位数顺序
  let sorted_percentiles = database_result.percentiles.sort_by(fn(p) { p.percentile })
  for i in 1..sorted_percentiles.length() {
    assert_true(sorted_percentiles[i-1].value <= sorted_percentiles[i].value)
  }
  
  // 运行API基准测试
  let api_result = BenchmarkManager::run_benchmark(benchmark_manager, api_benchmark)
  assert_true(api_result.success)
  assert_true(api_result.mean_time > 0.0)
  
  // 运行序列化基准测试
  let serialization_result = BenchmarkManager::run_benchmark(benchmark_manager, serialization_benchmark)
  assert_true(serialization_result.success)
  assert_true(serialization_result.mean_time > 0.0)
  
  // 比较基准测试结果
  let comparison = BenchmarkManager::compare_results(benchmark_manager, [
    ("database", database_result),
    ("api", api_result),
    ("serialization", serialization_result)
  ])
  
  assert_eq(comparison.results.length(), 3)
  
  // 验证最快的基准测试
  let fastest = comparison.fastest
  assert_true(fastest == "database" or fastest == "api" or fastest == "serialization")
  
  // 验证最慢的基准测试
  let slowest = comparison.slowest
  assert_true(slowest == "database" or slowest == "api" or slowest == "serialization")
  assert_true(fastest != slowest or comparison.results.length() == 1)
  
  // 验证性能比率
  assert_true(comparison.performance_ratio >= 1.0)
  
  // 保存基准测试结果
  let save_result = BenchmarkManager::save_results(benchmark_manager, comparison, "benchmark_results.json")
  assert_true(save_result.success)
  
  // 加载历史基准测试结果
  let historical_results = BenchmarkManager::load_historical_results(benchmark_manager, "database_query_performance")
  assert_true(historical_results.length() > 0)
  
  // 分析性能趋势
  let trend_analysis = BenchmarkManager::analyze_performance_trend(benchmark_manager, historical_results)
  assert_true(trend_analysis.trend == "improving" or trend_analysis.trend == "degrading" or trend_analysis.trend == "stable")
  assert_true(trend_analysis.confidence >= 0.0 and trend_analysis.confidence <= 1.0)
}

// 测试6: 性能配置文件分析
test "性能配置文件分析功能" {
  // 创建性能配置文件分析器
  let profiler = PerformanceProfiler::new()
  
  // 配置分析器
  Profiler::configure(profiler, {
    sampling_interval: 10,  // 10ms采样间隔
    max_samples: 10000,     // 最大采样数
    call_stack_depth: 32,   // 最大调用栈深度
    include_system_frames: false
  })
  
  // 启动性能分析
  Profiler::start(profiler)
  
  // 模拟应用程序执行
  let mut result = 0
  
  // CPU密集型操作
  for i in 0..=10000 {
    result = result + i * i
  }
  
  // 内存密集型操作
  let large_array = []
  for i in 0..=1000 {
    large_array = large_array.push({
      id: i,
      data: "data".repeat(100),
      timestamp: Time::now(),
      metadata: {
        type: "test",
        priority: i % 10,
        tags: ["tag1", "tag2", "tag3"]
      }
    })
  }
  
  // I/O密集型操作（模拟）
  for i in 0..=100 {
    let file_content = "This is a test file content with some data to process".to_byte_array()
    let processed_content = file_content.map(fn(b) { b ^ 0xFF })  // 简单处理
    let _ = processed_content.length()
  }
  
  // 停止性能分析
  let profile_result = Profiler::stop(profiler)
  assert_true(profile_result.success)
  assert_true(profile_result.sample_count > 0)
  
  // 分析CPU使用情况
  let cpu_analysis = Profiler::analyze_cpu_usage(profiler, profile_result)
  assert_true(cpu_analysis.total_samples > 0)
  assert_true(cpu_analysis.hot_functions.length() > 0)
  
  // 验证热点函数
  let hottest_function = cpu_analysis.hot_functions[0]
  assert_true(hottest_function.sample_count > 0)
  assert_true(hottest_function.percentage > 0.0)
  assert_true(hottest_function.function_name.length() > 0)
  
  // 验证函数调用关系
  assert_true(hottest_function.call_stack.length() > 0)
  
  // 分析内存使用情况
  let memory_analysis = Profiler::analyze_memory_usage(profiler, profile_result)
  assert_true(memory_analysis.total_allocations > 0)
  assert_true(memory_analysis.total_bytes > 0)
  assert_true(memory_analysis.allocation_sites.length() > 0)
  
  // 验证内存分配热点
  let memory_hotspot = memory_analysis.allocation_sites[0]
  assert_true(memory_hotspot.allocation_count > 0)
  assert_true(memory_hotspot.total_bytes > 0)
  assert_true(memory_hotspot.average_size > 0.0)
  
  // 分析I/O使用情况
  let io_analysis = Profiler::analyze_io_usage(profiler, profile_result)
  assert_true(io_analysis.total_operations > 0)
  assert_true(io_analysis.total_bytes > 0)
  
  // 生成性能报告
  let performance_report = Profiler::generate_report(profiler, profile_result)
  assert_true(performance_report.summary.length() > 0)
  assert_true(performance_report.cpu_analysis.length() > 0)
  assert_true(performance_report.memory_analysis.length() > 0)
  assert_true(performance_report.io_analysis.length() > 0)
  assert_true(performance_report.recommendations.length() > 0)
  
  // 验证性能建议
  let has_cpu_recommendation = performance_report.recommendations.any(fn(r) {
    r.category == "cpu" and r.description.length() > 0
  })
  assert_true(has_cpu_recommendation)
  
  let has_memory_recommendation = performance_report.recommendations.any(fn(r) {
    r.category == "memory" and r.description.length() > 0
  })
  assert_true(has_memory_recommendation)
  
  // 比较性能配置文件
  Profiler::start(profiler)
  
  // 优化后的代码
  let mut optimized_result = 0
  for i in 0..=10000 {
    optimized_result = optimized_result + i * i
  }
  
  // 使用更高效的内存操作
  let optimized_array = Array::with_capacity(1001)
  for i in 0..=1000 {
    optimized_array.push({
      id: i,
      data: "data".repeat(100),
      timestamp: Time::now(),
      metadata: {
        type: "test",
        priority: i % 10,
        tags: ["tag1", "tag2", "tag3"]
      }
    })
  }
  
  let optimized_profile_result = Profiler::stop(profiler)
  
  // 比较性能
  let comparison = Profiler::compare_profiles(profiler, profile_result, optimized_profile_result)
  assert_true(comparison.cpu_improvement >= 0.0)
  assert_true(comparison.memory_improvement >= 0.0)
  assert_true(comparison.io_improvement >= 0.0)
  
  // 验证改进报告
  assert_true(comparison.improvement_summary.length() > 0)
  assert_true(comparison.regressions.length() >= 0)
}

// 测试7: 实时性能监控
test "实时性能监控功能" {
  // 创建实时性能监控器
  let realtime_monitor = RealtimePerformanceMonitor::new()
  
  // 配置监控器
  RealtimeMonitor::configure(realtime_monitor, {
    update_interval: 1000,      // 1秒更新间隔
    alert_threshold: 80.0,      // 80%告警阈值
    history_size: 300,          // 5分钟历史数据
    dashboard_enabled: true
  })
  
  // 添加监控指标
  RealtimeMonitor::add_metric(realtime_monitor, {
    name: "cpu_usage",
    type: "gauge",
    unit: "percent",
    alert_threshold: 80.0,
    warning_threshold: 60.0
  })
  
  RealtimeMonitor::add_metric(realtime_monitor, {
    name: "memory_usage",
    type: "gauge",
    unit: "percent",
    alert_threshold: 85.0,
    warning_threshold: 70.0
  })
  
  RealtimeMonitor::add_metric(realtime_monitor, {
    name: "response_time",
    type: "histogram",
    unit: "milliseconds",
    alert_threshold: 1000.0,
    warning_threshold: 500.0
  })
  
  RealtimeMonitor::add_metric(realtime_monitor, {
    name: "error_rate",
    type: "counter",
    unit: "errors_per_second",
    alert_threshold: 10.0,
    warning_threshold: 5.0
  })
  
  // 启动监控
  RealtimeMonitor::start(realtime_monitor)
  
  // 模拟实时数据流
  let base_time = Time::now()
  
  for i in 0..=120 {
    let timestamp = base_time + i * 1000
    
    // CPU使用率模拟
    let cpu_usage = 40.0 + (i % 20) * 2.0
    RealtimeMonitor::update_metric(realtime_monitor, timestamp, "cpu_usage", cpu_usage)
    
    // 内存使用率模拟
    let memory_usage = 60.0 + (i % 15) * 2.0
    RealtimeMonitor::update_metric(realtime_monitor, timestamp, "memory_usage", memory_usage)
    
    // 响应时间模拟
    let response_time = 200.0 + (i % 30) * 10.0
    RealtimeMonitor::update_metric(realtime_monitor, timestamp, "response_time", response_time)
    
    // 错误率模拟
    let error_rate = if i % 20 == 0 { 8.0 } else { 2.0 }
    RealtimeMonitor::update_metric(realtime_monitor, timestamp, "error_rate", error_rate)
    
    // 模拟告警条件
    if i == 60 {
      // 触发CPU告警
      RealtimeMonitor::update_metric(realtime_monitor, timestamp, "cpu_usage", 85.0)
    }
    
    if i == 80 {
      // 触发内存告警
      RealtimeMonitor::update_metric(realtime_monitor, timestamp, "memory_usage", 90.0)
    }
    
    // 短暂延迟以模拟实时性
    Time::sleep(10)
  }
  
  // 获取实时监控状态
  let monitor_status = RealtimeMonitor::get_status(realtime_monitor)
  assert_true(monitor_status.is_running)
  assert_eq(monitor_status.monitored_metrics, 4)
  assert_true(monitor_status.data_points > 0)
  
  // 获取当前指标值
  let current_metrics = RealtimeMonitor::get_current_metrics(realtime_monitor)
  assert_eq(current_metrics.length(), 4)
  
  // 验证CPU指标
  let cpu_metric = current_metrics.find(fn(m) { m.name == "cpu_usage" })
  assert_true(cpu_metric != None)
  
  match cpu_metric {
    Some(metric) => {
      assert_true(metric.current_value > 0.0)
      assert_true(metric.unit == "percent")
      assert_true(metric.history.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 验证告警状态
  let alert_status = RealtimeMonitor::get_alert_status(realtime_monitor)
  assert_true(alert_status.active_alerts.length() >= 2)  // CPU和内存告警
  
  // 验证CPU告警
  let cpu_alert = alert_status.active_alerts.find(fn(a) { a.metric_name == "cpu_usage" })
  assert_true(cpu_alert != None)
  
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, "alert")
      assert_true(alert.triggered_at > 0)
      assert_true(alert.message.contains("cpu_usage"))
    }
    None => assert_true(false)
  }
  
  // 验证内存告警
  let memory_alert = alert_status.active_alerts.find(fn(a) { a.metric_name == "memory_usage" })
  assert_true(memory_alert != None)
  
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.severity, "alert")
      assert_true(alert.triggered_at > 0)
      assert_true(alert.message.contains("memory_usage"))
    }
    None => assert_true(false)
  }
  
  // 获取性能趋势
  let performance_trends = RealtimeMonitor::get_performance_trends(realtime_monitor, 60000)  // 1分钟趋势
  assert_true(performance_trends.length() > 0)
  
  // 验证趋势数据
  for trend in performance_trends {
    assert_true(trend.metric_name.length() > 0)
    assert_true(trend.data_points.length() > 0)
    assert_true(trend.direction == "up" or trend.direction == "down" or trend.direction == "stable")
  }
  
  // 生成实时仪表板数据
  let dashboard_data = RealtimeMonitor::generate_dashboard_data(realtime_monitor)
  assert_true(dashboard_data.metrics.length() > 0)
  assert_true(dashboard_data.alerts.length() > 0)
  assert_true(dashboard_data.trends.length() > 0)
  
  // 停止监控
  RealtimeMonitor::stop(realtime_monitor)
  
  let stopped_status = RealtimeMonitor::get_status(realtime_monitor)
  assert_false(stopped_status.is_running)
}

// 测试8: 性能优化建议
test "性能优化建议功能" {
  // 创建性能优化建议引擎
  let optimization_engine = PerformanceOptimizationEngine::new()
  
  // 配置建议引擎
  OptimizationEngine::configure(optimization_engine, {
    analysis_depth: "comprehensive",
    recommendation_threshold: 0.7,  // 70%置信度阈值
    max_recommendations: 10
  })
  
  // 添加优化规则
  OptimizationEngine::add_rule(optimization_engine, {
    name: "high_cpu_usage",
    condition: {
      metric: "cpu_usage",
      operator: "greater_than",
      value: 80.0,
      duration: 300000  // 5分钟
    },
    recommendations: [
      {
        category: "cpu",
        priority: "high",
        description: "CPU使用率过高，建议优化算法或增加计算资源",
        actions: [
          "分析热点函数并优化算法",
          "考虑使用缓存减少计算",
          "检查是否有死循环或无效计算",
          "评估是否需要增加CPU资源"
        ],
        estimated_improvement: "20-40%"
      }
    ]
  })
  
  OptimizationEngine::add_rule(optimization_engine, {
    name: "high_memory_usage",
    condition: {
      metric: "memory_usage",
      operator: "greater_than",
      value: 85.0,
      duration: 300000  // 5分钟
    },
    recommendations: [
      {
        category: "memory",
        priority: "high",
        description: "内存使用率过高，可能存在内存泄漏或分配过多",
        actions: [
          "检查内存分配，避免不必要的对象创建",
          "使用内存分析工具查找内存泄漏",
          "优化数据结构，减少内存占用",
          "考虑增加内存或使用内存优化技术"
        ],
        estimated_improvement: "15-30%"
      }
    ]
  })
  
  OptimizationEngine::add_rule(optimization_engine, {
    name: "slow_response_time",
    condition: {
      metric: "response_time_p95",
      operator: "greater_than",
      value: 1000.0,
      duration: 180000  // 3分钟
    },
    recommendations: [
      {
        category: "latency",
        priority: "medium",
        description: "响应时间过长，影响用户体验",
        actions: [
          "分析慢查询并优化数据库访问",
          "使用异步处理减少阻塞",
          "增加缓存以提高响应速度",
          "优化网络通信和序列化"
        ],
        estimated_improvement: "30-50%"
      }
    ]
  })
  
  OptimizationEngine::add_rule(optimization_engine, {
    name: "high_error_rate",
    condition: {
      metric: "error_rate",
      operator: "greater_than",
      value: 5.0,
      duration: 120000  // 2分钟
    },
    recommendations: [
      {
        category: "reliability",
        priority: "critical",
        description: "错误率过高，系统稳定性存在问题",
        actions: [
          "分析错误日志，找出根本原因",
          "增加错误处理和重试机制",
          "实施熔断器模式防止级联故障",
          "加强监控和告警机制"
        ],
        estimated_improvement: "50-80%"
      }
    ]
  })
  
  // 模拟性能数据
  let performance_data = [
    {
      timestamp: 1640995200,
      metrics: [
        ("cpu_usage", 85.0),
        ("memory_usage", 75.0),
        ("response_time_p95", 800.0),
        ("error_rate", 2.0)
      ]
    },
    {
      timestamp: 1640995260,
      metrics: [
        ("cpu_usage", 88.0),
        ("memory_usage", 78.0),
        ("response_time_p95", 900.0),
        ("error_rate", 3.0)
      ]
    },
    {
      timestamp: 1640995320,
      metrics: [
        ("cpu_usage", 90.0),
        ("memory_usage", 88.0),
        ("response_time_p95", 1200.0),
        ("error_rate", 8.0)
      ]
    },
    {
      timestamp: 1640995380,
      metrics: [
        ("cpu_usage", 87.0),
        ("memory_usage", 90.0),
        ("response_time_p95", 1100.0),
        ("error_rate", 6.0)
      ]
    }
  ]
  
  // 分析性能数据并生成建议
  let analysis_result = OptimizationEngine::analyze(optimization_engine, performance_data)
  assert_true(analysis_result.success)
  assert_true(analysis_result.recommendations.length() > 0)
  
  // 验证CPU使用率建议
  let cpu_recommendations = analysis_result.recommendations.filter(fn(r) { r.category == "cpu" })
  assert_true(cpu_recommendations.length() > 0)
  
  let cpu_rec = cpu_recommendations[0]
  assert_eq(cpu_rec.priority, "high")
  assert_true(cpu_rec.description.contains("CPU"))
  assert_true(cpu_rec.actions.length() > 0)
  assert_true(cpu_rec.estimated_improvement.length() > 0)
  assert_true(cpu_rec.confidence >= 0.7)
  
  // 验证内存使用率建议
  let memory_recommendations = analysis_result.recommendations.filter(fn(r) { r.category == "memory" })
  assert_true(memory_recommendations.length() > 0)
  
  let memory_rec = memory_recommendations[0]
  assert_eq(memory_rec.priority, "high")
  assert_true(memory_rec.description.contains("内存"))
  assert_true(memory_rec.actions.length() > 0)
  
  // 验证响应时间建议
  let latency_recommendations = analysis_result.recommendations.filter(fn(r) { r.category == "latency" })
  assert_true(latency_recommendations.length() > 0)
  
  let latency_rec = latency_recommendations[0]
  assert_eq(latency_rec.priority, "medium")
  assert_true(latency_rec.description.contains("响应时间"))
  
  // 验证错误率建议
  let reliability_recommendations = analysis_result.recommendations.filter(fn(r) { r.category == "reliability" })
  assert_true(reliability_recommendations.length() > 0)
  
  let reliability_rec = reliability_recommendations[0]
  assert_eq(reliability_rec.priority, "critical")
  assert_true(reliability_rec.description.contains("错误率"))
  
  // 验证建议优先级排序
  let sorted_recommendations = analysis_result.recommendations.sort_by(fn(r) {
    match r.priority {
      "critical" => 1,
      "high" => 2,
      "medium" => 3,
      "low" => 4,
      _ => 5
    }
  })
  
  for i in 1..sorted_recommendations.length() {
    let prev_priority = sorted_recommendations[i-1].priority
    let curr_priority = sorted_recommendations[i].priority
    
    let prev_level = match prev_priority {
      "critical" => 1,
      "high" => 2,
      "medium" => 3,
      "low" => 4,
      _ => 5
    }
    
    let curr_level = match curr_priority {
      "critical" => 1,
      "high" => 2,
      "medium" => 3,
      "low" => 4,
      _ => 5
    }
    
    assert_true(prev_level <= curr_level)
  }
  
  // 生成优化计划
  let optimization_plan = OptimizationEngine::generate_plan(optimization_engine, analysis_result.recommendations)
  assert_true(optimization_plan.phases.length() > 0)
  
  // 验证优化计划阶段
  let immediate_phase = optimization_plan.phases.find(fn(p) { p.name == "immediate" })
  assert_true(immediate_phase != None)
  
  match immediate_phase {
    Some(phase) => {
      assert_true(phase.recommendations.length() > 0)
      assert_true(phase.estimated_duration > 0)
      assert_true(phase.estimated_impact.length() > 0)
    }
    None => assert_true(false)
  }
  
  // 估算总体改进
  let total_improvement = OptimizationEngine::estimate_total_improvement(optimization_engine, analysis_result.recommendations)
  assert_true(total_improvement.performance_improvement > 0.0)
  assert_true(total_improvement.stability_improvement > 0.0)
  assert_true(total_improvement.resource_efficiency > 0.0)
}