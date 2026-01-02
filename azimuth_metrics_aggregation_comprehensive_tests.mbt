// Azimuth Metrics Aggregation and Analysis Test Suite
// 测试遥测系统的度量聚合和分析功能

test "多维度度量聚合和统计" {
  // 创建具有多维度聚合功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_multi_dimensional_aggregation()
  
  // 配置聚合设置
  let aggregation_config = MultiDimensionalAggregationConfig {
    default_dimensions: ["service", "environment", "host"],
    max_cardinality: 1000,
    aggregation_window: "5m",
    supported_functions: ["sum", "avg", "min", "max", "count", "p50", "p95", "p99"],
    rollup_schedule: ["1m", "5m", "15m", "1h", "1d"]
  }
  
  MultiDimensionalAggregation::configure(telemetry_provider, aggregation_config)
  
  // 创建度量收集器
  let collector = MetricsCollector::new("multi.dimension.test")
  
  // 生成多维度度量数据
  let services = ["api-gateway", "user-service", "order-service", "payment-service"]
  let environments = ["production", "staging", "development"]
  let hosts = ["server-01", "server-02", "server-03"]
  let operations = ["GET", "POST", "PUT", "DELETE"]
  
  let base_time = Timestamp::from_string("2025-01-02T10:00:00Z")
  
  // 生成1小时的度量数据，每分钟一个数据点
  for minute = 0; minute < 60; minute = minute + 1 {
    let timestamp = base_time.add_minutes(minute)
    
    for service in services {
      for environment in environments {
        for host in hosts {
          for operation in operations {
            // 生成请求延迟数据
            let latency = 50.0 + Math::random() * 200.0  // 50-250ms
            
            let latency_metric = Metric::new(
              "request.latency",
              latency,
              "ms",
              timestamp,
              [
                ("service", service),
                ("environment", environment),
                ("host", host),
                ("operation", operation),
                ("status", "200")
              ]
            )
            
            MetricsCollector::add_metric(collector, latency_metric)
            
            // 生成请求计数数据
            let count = 1.0
            
            let count_metric = Metric::new(
              "request.count",
              count,
              "count",
              timestamp,
              [
                ("service", service),
                ("environment", environment),
                ("host", host),
                ("operation", operation),
                ("status", "200")
              ]
            )
            
            MetricsCollector::add_metric(collector, count_metric)
            
            // 偶尔生成错误数据
            if Math::random() < 0.05 {  // 5%错误率
              let error_latency = 500.0 + Math::random() * 1000.0  // 500-1500ms
              
              let error_metric = Metric::new(
                "request.latency",
                error_latency,
                "ms",
                timestamp,
                [
                  ("service", service),
                  ("environment", environment),
                  ("host", host),
                  ("operation", operation),
                  ("status", "500")
                ]
              )
              
              MetricsCollector::add_metric(collector, error_metric)
              
              let error_count_metric = Metric::new(
                "request.count",
                1.0,
                "count",
                timestamp,
                [
                  ("service", service),
                  ("environment", environment),
                  ("host", host),
                  ("operation", operation),
                  ("status", "500")
                ]
              )
              
              MetricsCollector::add_metric(collector, error_count_metric)
            }
          }
        }
      }
    }
  }
  
  // 验证度量收集
  let total_metrics = MetricsCollector::get_total_metrics(collector)
  let expected_metrics = 60 * 4 * 3 * 3 * 4 * 2  // 60分钟 × 4服务 × 3环境 × 3主机 × 4操作 × (成功+错误)
  assert_true(total_metrics >= expected_metrics * 0.95)  // 允许5%的误差
  
  // 测试单维度聚合
  let service_aggregation = MultiDimensionalAggregation::aggregate_by_dimension(
    telemetry_provider, 
    collector, 
    "request.latency", 
    "service", 
    ["avg", "p95", "p99"]
  )
  
  assert_eq(service_aggregation.groups.length(), 4)  // 4个服务
  assert_true(service_aggregation.groups.all(fn(group) { 
    group.aggregations.contains("avg") && 
    group.aggregations.contains("p95") && 
    group.aggregations.contains("p99") 
  }))
  
  // 验证每个服务的聚合结果
  for service in services {
    let service_group = service_aggregation.groups.find(fn(g) { g.dimension_value == service })
    assert_not_eq(service_group, None)
    assert_true(service_group.count > 0)
    assert_true(service_group.aggregations.get("avg") > 0)
  }
  
  // 测试多维度聚合
  let multi_dimension_aggregation = MultiDimensionalAggregation::aggregate_by_dimensions(
    telemetry_provider, 
    collector, 
    "request.latency", 
    ["service", "environment"], 
    ["avg", "p95", "count"]
  )
  
  assert_eq(multi_dimension_aggregation.groups.length(), 12)  // 4服务 × 3环境
  
  // 验证多维度聚合结果
  for service in services {
    for environment in environments {
      let group_key = service + ":" + environment
      let group = multi_dimension_aggregation.groups.find(fn(g) { g.group_key == group_key })
      assert_not_eq(group, None)
      assert_true(group.count > 0)
    }
  }
  
  // 测试时间窗口聚合
  let time_window_aggregation = MultiDimensionalAggregation::aggregate_by_time_window(
    telemetry_provider, 
    collector, 
    "request.latency", 
    "5m", 
    ["avg", "min", "max"]
  )
  
  assert_eq(time_window_aggregation.time_windows.length(), 12)  // 60分钟 / 5分钟 = 12个窗口
  
  // 验证时间窗口聚合
  for window in time_window_aggregation.time_windows {
    assert_true(window.start_time < window.end_time)
    assert_true(window.aggregations.contains("avg"))
    assert_true(window.aggregations.contains("min"))
    assert_true(window.aggregations.contains("max"))
    
    // 验证数学关系：min <= avg <= max
    let min_val = window.aggregations.get("min")
    let avg_val = window.aggregations.get("avg")
    let max_val = window.aggregations.get("max")
    
    assert_true(min_val <= avg_val)
    assert_true(avg_val <= max_val)
  }
  
  // 测试过滤聚合
  let filtered_aggregation = MultiDimensionalAggregation::aggregate_with_filter(
    telemetry_provider, 
    collector, 
    "request.latency", 
    [("status", "500")],  // 只聚合错误请求
    ["service"], 
    ["avg", "count"]
  )
  
  assert_true(filtered_aggregation.groups.length() > 0)
  
  // 验证过滤聚合结果
  for group in filtered_aggregation.groups {
    assert_true(group.aggregations.get("count") > 0)  // 应该有错误请求
    assert_true(group.aggregations.get("avg") > 500)  // 错误请求延迟应该较高
  }
  
  // 测试百分位数聚合
  let percentile_aggregation = MultiDimensionalAggregation::aggregate_percentiles(
    telemetry_provider, 
    collector, 
    "request.latency", 
    ["service"], 
    [50.0, 90.0, 95.0, 99.0, 99.9]
  )
  
  assert_eq(percentile_aggregation.groups.length(), 4)  // 4个服务
  
  // 验证百分位数聚合结果
  for group in percentile_aggregation.groups {
    let p50 = group.percentiles.get(50.0)
    let p90 = group.percentiles.get(90.0)
    let p95 = group.percentiles.get(95.0)
    let p99 = group.percentiles.get(99.0)
    let p99_9 = group.percentiles.get(99.9)
    
    // 验证百分位数的数学关系
    assert_true(p50 <= p90)
    assert_true(p90 <= p95)
    assert_true(p95 <= p99)
    assert_true(p99 <= p99_9)
  }
  
  assert_true(true)
}

test "实时度量计算和流式聚合" {
  // 创建具有实时度量计算功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_real_time_metrics()
  
  // 配置实时度量设置
  let real_time_config = RealTimeMetricsConfig {
    sliding_window_size: 300,  // 5分钟滑动窗口
    update_interval: 10,       // 10秒更新一次
    max_memory_usage: 100 * 1024 * 1024,  // 100MB
    stream_processing_enabled: true,
    alert_thresholds: {
      "error_rate": 0.05,      // 5%错误率阈值
      "latency_p95": 500.0,    // 95%延迟阈值500ms
      "throughput_min": 100.0  // 最小吞吐量阈值
    }
  }
  
  RealTimeMetrics::configure(telemetry_provider, real_time_config)
  
  // 创建实时度量处理器
  let processor = RealTimeMetricsProcessor::new("real.time.test")
  
  // 配置实时度量计算
  RealTimeMetricsProcessor::add_sliding_window_metric(processor, "request.rate", "1m", "count")
  RealTimeMetricsProcessor::add_sliding_window_metric(processor, "error.rate", "1m", "rate", [
    ("status", "500")
  ])
  RealTimeMetricsProcessor::add_sliding_window_metric(processor, "latency.avg", "1m", "avg")
  RealTimeMetricsProcessor::add_sliding_window_metric(processor, "latency.p95", "1m", "percentile", 95.0)
  RealTimeMetricsProcessor::add_sliding_window_metric(processor, "throughput", "1m", "sum", [
    ("metric_name", "request.count")
  ])
  
  // 启动实时处理器
  RealTimeMetricsProcessor::start(processor)
  
  // 模拟实时度量流
  let base_time = Timestamp::from_string("2025-01-02T10:00:00Z")
  let services = ["api-gateway", "user-service", "order-service"]
  
  // 生成5分钟的实时度量数据
  for second = 0; second < 300; second = second + 1 {
    let timestamp = base_time.add_seconds(second)
    
    for service in services {
      // 每秒生成1-10个请求
      let request_count = 1 + Math::floor(Math::random() * 10.0).to_int()
      
      for i = 0; i < request_count; i = i + 1 {
        // 生成请求延迟
        let latency = 50.0 + Math::random() * 200.0
        
        // 95%的请求成功，5%失败
        let status = if Math::random() < 0.95 { "200" } else { "500" }
        
        let metric = Metric::new(
          "request.latency",
          latency,
          "ms",
          timestamp,
          [
            ("service", service),
            ("status", status)
          ]
        )
        
        // 处理实时度量
        RealTimeMetricsProcessor::process_metric(processor, metric)
        
        // 添加请求计数度量
        let count_metric = Metric::new(
          "request.count",
          1.0,
          "count",
          timestamp,
          [
            ("service", service),
            ("status", status)
          ]
        )
        
        RealTimeMetricsProcessor::process_metric(processor, count_metric)
      }
    }
    
    // 每10秒检查一次实时度量
    if second % 10 == 0 && second > 0 {
      let current_time = base_time.add_seconds(second)
      
      // 获取实时度量
      let real_time_metrics = RealTimeMetricsProcessor::get_current_metrics(processor, current_time)
      
      // 验证实时度量计算
      for service in services {
        let service_metrics = real_time_metrics.get(service)
        
        // 验证请求率
        let request_rate = service_metrics.get("request.rate")
        assert_true(request_rate >= 0.0)
        
        // 验证错误率
        let error_rate = service_metrics.get("error.rate")
        assert_true(error_rate >= 0.0 && error_rate <= 1.0)
        
        // 验证延迟
        let latency_avg = service_metrics.get("latency.avg")
        let latency_p95 = service_metrics.get("latency.p95")
        
        assert_true(latency_avg > 0.0)
        assert_true(latency_p95 >= latency_avg)
        
        // 验证吞吐量
        let throughput = service_metrics.get("throughput")
        assert_true(throughput >= 0.0)
      }
    }
  }
  
  // 停止实时处理器
  RealTimeMetricsProcessor::stop(processor)
  
  // 获取最终的实时度量统计
  let final_metrics = RealTimeMetricsProcessor::get_final_metrics(processor)
  
  // 验证最终统计
  for service in services {
    let service_stats = final_metrics.get(service)
    
    assert_true(service_stats.get("total_requests") > 0)
    assert_true(service_stats.get("total_errors") >= 0)
    assert_true(service_stats.get("avg_latency") > 0)
    assert_true(service_stats.get("max_latency") >= service_stats.get("avg_latency"))
    assert_true(service_stats.get("min_latency") <= service_stats.get("avg_latency"))
  }
  
  // 测试实时告警
  let alerts = RealTimeMetricsProcessor::get_alerts(processor)
  assert_true(alerts.length() >= 0)  // 可能有也可能没有告警
  
  // 验证告警内容
  for alert in alerts {
    assert_true(alert.metric_name.length() > 0)
    assert_true(alert.service_name.length() > 0)
    assert_true(alert.threshold > 0)
    assert_true(alert.actual_value > 0)
    assert_true(alert.timestamp > base_time)
  }
  
  // 测试实时度量导出
  let export_result = RealTimeMetricsProcessor::export_metrics(processor, "json")
  assert_true(export_result.success)
  assert_true(export_result.data.length() > 0)
  
  // 验证导出的JSON包含所有服务
  let exported_data = JSON::parse(export_result.data)
  for service in services {
    assert_true(exported_data.contains(service))
  }
  
  assert_true(true)
}

test "度量基线分析和异常检测" {
  // 创建具有度量基线分析功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_metrics_baseline_analysis()
  
  // 配置基线分析设置
  let baseline_config = BaselineAnalysisConfig {
    baseline_period: 604800,     // 7天基线周期
    update_frequency: 86400,     // 24小时更新一次基线
    anomaly_detection_method: "statistical",  // 统计方法
    sensitivity_level: 0.95,     // 95%置信度
    seasonality_detection: true,
    trend_analysis: true
  }
  
  MetricsBaselineAnalysis::configure(telemetry_provider, baseline_config)
  
  // 创建基线数据收集器
  let baseline_collector = MetricsCollector::new("baseline.data")
  
  // 生成7天的基线数据
  let base_time = Timestamp::from_string("2025-01-02T00:00:00Z")
  let services = ["api-gateway", "user-service", "order-service"]
  let metrics = ["request.latency", "error.rate", "throughput"]
  
  for day = 0; day < 7; day = day + 1 {
    for hour = 0; hour < 24; hour = hour + 1 {
      let timestamp = base_time.add_days(day).add_hours(hour)
      
      for service in services {
        for metric_name in metrics {
          // 生成具有日季节性和周季节性的基线数据
          let base_value = get_baseline_metric_value(metric_name, day, hour)
          
          // 添加一些随机噪声
          let noise = (Math::random() - 0.5) * base_value * 0.1
          let final_value = base_value + noise
          
          let metric = Metric::new(
            metric_name,
            final_value,
            get_metric_unit(metric_name),
            timestamp,
            [
              ("service", service),
              ("environment", "production")
            ]
          )
          
          MetricsCollector::add_metric(baseline_collector, metric)
        }
      }
    }
  }
  
  // 计算基线
  let baseline_result = MetricsBaselineAnalysis::calculate_baseline(
    telemetry_provider, 
    baseline_collector, 
    ["service", "metric_name"]
  )
  
  assert_true(baseline_result.success)
  assert_eq(baseline_result.baselines.length(), 9)  // 3服务 × 3指标
  
  // 验证基线计算结果
  for service in services {
    for metric_name in metrics {
      let baseline_key = service + ":" + metric_name
      let baseline = baseline_result.baselines.find(fn(b) { b.key == baseline_key })
      
      assert_not_eq(baseline, None)
      assert_true(baseline.mean > 0)
      assert_true(baseline.std_deviation > 0)
      assert_true(baseline.percentiles.contains(50.0))
      assert_true(baseline.percentiles.contains(95.0))
      assert_true(baseline.percentiles.contains(99.0))
      
      // 验证百分位数的数学关系
      assert_true(baseline.percentiles.get(50.0) <= baseline.percentiles.get(95.0))
      assert_true(baseline.percentiles.get(95.0) <= baseline.percentiles.get(99.0))
    }
  }
  
  // 创建测试数据收集器（用于异常检测）
  let test_collector = MetricsCollector::new("test.data")
  
  // 生成1小时的测试数据，包含一些异常值
  for minute = 0; minute < 60; minute = minute + 1 {
    let timestamp = base_time.add_days(7).add_hours(10).add_minutes(minute)
    
    for service in services {
      for metric_name in metrics {
        // 获取基线值
        let baseline_key = service + ":" + metric_name
        let baseline = baseline_result.baselines.find(fn(b) { b.key == baseline_key })
        
        // 生成测试值，大部分在正常范围内，少数为异常值
        let test_value = if minute == 15 || minute == 30 || minute == 45 {
          // 异常值：偏离基线3个标准差
          baseline.mean + baseline.std_deviation * 3.0 * (if Math::random() < 0.5 { -1.0 } else { 1.0 })
        } else {
          // 正常值：在基线均值附近
          baseline.mean + (Math::random() - 0.5) * baseline.std_deviation
        }
        
        let metric = Metric::new(
          metric_name,
          test_value,
          get_metric_unit(metric_name),
          timestamp,
          [
            ("service", service),
            ("environment", "production")
          ]
        )
        
        MetricsCollector::add_metric(test_collector, metric)
      }
    }
  }
  
  // 执行异常检测
  let anomaly_detection_result = MetricsBaselineAnalysis::detect_anomalies(
    telemetry_provider, 
    test_collector, 
    baseline_result.baselines
  )
  
  assert_true(anomaly_detection_result.success)
  assert_true(anomaly_detection_result.anomalies.length() >= 9)  // 至少每个服务每个指标有一个异常
  
  // 验证异常检测结果
  for service in services {
    for metric_name in metrics {
      let service_metric_anomalies = anomaly_detection_result.anomalies.filter(fn(a) { 
        a.service == service && a.metric_name == metric_name 
      })
      
      assert_true(service_metric_anomalies.length() >= 1)  // 至少一个异常
      
      // 验证异常严重程度
      for anomaly in service_metric_anomalies {
        assert_true(anomaly.severity_score >= 0.0)
        assert_true(anomaly.deviation_score >= 2.0)  // 至少2个标准差
        assert_true(anomaly.timestamp > base_time.add_days(7))
      }
    }
  }
  
  // 测试异常告警生成
  let alerts = MetricsBaselineAnalysis::generate_alerts(telemetry_provider, anomaly_detection_result.anomalies)
  assert_true(alerts.length() >= 9)
  
  // 验证告警内容
  for alert in alerts {
    assert_true(alert.service.length() > 0)
    assert_true(alert.metric.length() > 0)
    assert_true(alert.severity in ["low", "medium", "high", "critical"])
    assert_true(alert.message.length() > 0)
    assert_true(alert.timestamp > base_time.add_days(7))
  }
  
  // 测试基线更新
  let updated_baseline_result = MetricsBaselineAnalysis::update_baseline(
    telemetry_provider, 
    baseline_result, 
    test_collector
  )
  
  assert_true(updated_baseline_result.success)
  
  // 验证基线更新
  for service in services {
    for metric_name in metrics {
      let baseline_key = service + ":" + metric_name
      let old_baseline = baseline_result.baselines.find(fn(b) { b.key == baseline_key })
      let new_baseline = updated_baseline_result.baselines.find(fn(b) { b.key == baseline_key })
      
      // 更新后的基线应该有所变化（因为包含了异常值）
      assert_not_eq(new_baseline.mean, old_baseline.mean)
    }
  }
  
  // 生成基线分析报告
  let report = MetricsBaselineAnalysis::generate_baseline_report(
    telemetry_provider, 
    baseline_result, 
    anomaly_detection_result
  )
  
  assert_true(report.success)
  assert_true(report.html_content.length() > 0)
  
  // 验证报告内容
  assert_true(report.html_content.contains("基线分析报告"))
  assert_true(report.html_content.contains("异常检测结果"))
  assert_true(report.html_content.contains("基线统计"))
  
  assert_true(true)
}

test "度量预测和容量规划" {
  // 创建具有度量预测功能的遥测提供者
  let telemetry_provider = TelemetryProvider::with_metrics_prediction()
  
  // 配置预测设置
  let prediction_config = MetricsPredictionConfig {
    prediction_models: ["linear_regression", "exponential_smoothing", "arima"],
    training_period: 1209600,     // 2周训练数据
    prediction_horizon: 604800,   // 1周预测
    confidence_interval: 0.95,    // 95%置信区间
    model_selection: "auto",      // 自动选择最佳模型
    cross_validation_folds: 5     // 5折交叉验证
  }
  
  MetricsPrediction::configure(telemetry_provider, prediction_config)
  
  // 创建历史数据收集器
  let historical_collector = MetricsCollector::new("historical.data")
  
  // 生成2周的历史数据
  let base_time = Timestamp::from_string("2025-01-02T00:00:00Z")
  let services = ["api-gateway", "user-service", "order-service"]
  let metrics = ["request.count", "cpu.usage", "memory.usage", "disk.usage"]
  
  for day = 0; day < 14; day = day + 1 {
    for hour = 0; hour < 24; hour = hour + 1 {
      let timestamp = base_time.add_days(day).add_hours(hour)
      
      for service in services {
        for metric_name in metrics {
          // 生成具有趋势、季节性和噪声的历史数据
          let historical_value = get_historical_metric_value(metric_name, day, hour)
          
          let metric = Metric::new(
            metric_name,
            historical_value,
            get_metric_unit(metric_name),
            timestamp,
            [
              ("service", service),
              ("environment", "production")
            ]
          )
          
          MetricsCollector::add_metric(historical_collector, metric)
        }
      }
    }
  }
  
  // 训练预测模型
  let training_result = MetricsPrediction::train_models(
    telemetry_provider, 
    historical_collector, 
    ["service", "metric_name"]
  )
  
  assert_true(training_result.success)
  assert_eq(training_result.models.length(), 12)  // 3服务 × 4指标
  
  // 验证模型训练结果
  for service in services {
    for metric_name in metrics {
      let model_key = service + ":" + metric_name
      let model = training_result.models.find(fn(m) { m.key == model_key })
      
      assert_not_eq(model, None)
      assert_true(model.model_type.length() > 0)
      assert_true(model.accuracy_score > 0.5)  // 至少50%准确率
      assert_true(model.mean_absolute_error >= 0.0)
      assert_true(model.root_mean_squared_error >= 0.0)
    }
  }
  
  // 执行预测
  let prediction_result = MetricsPrediction::predict(
    telemetry_provider, 
    training_result.models, 
    base_time.add_days(14),  // 从历史数据结束时间开始预测
    7 * 24  // 预测7天，每小时一个点
  )
  
  assert_true(prediction_result.success)
  assert_eq(prediction_result.predictions.length(), 12)  // 3服务 × 4指标
  
  // 验证预测结果
  for service in services {
    for metric_name in metrics {
      let prediction_key = service + ":" + metric_name
      let prediction = prediction_result.predictions.find(fn(p) { p.key == prediction_key })
      
      assert_not_eq(prediction, None)
      assert_eq(prediction.forecast_points.length(), 168)  // 7天 × 24小时
      
      // 验证预测点的置信区间
      for point in prediction.forecast_points {
        assert_true(point.lower_bound <= point.predicted_value)
        assert_true(point.upper_bound >= point.predicted_value)
        assert_true(point.upper_bound > point.lower_bound)
      }
      
      // 验证预测趋势合理性
      let first_point = prediction.forecast_points[0]
      let last_point = prediction.forecast_points[167]
      
      // 对于使用率指标，预测值应该在合理范围内
      if metric_name.contains("usage") {
        assert_true(first_point.predicted_value >= 0.0 && first_point.predicted_value <= 100.0)
        assert_true(last_point.predicted_value >= 0.0 && last_point.predicted_value <= 100.0)
      }
    }
  }
  
  // 容量规划分析
  let capacity_analysis = MetricsPrediction::analyze_capacity_needs(
    telemetry_provider, 
    prediction_result.predictions,
    [
      ("cpu.usage", 80.0),      // CPU使用率阈值80%
      ("memory.usage", 85.0),   // 内存使用率阈值85%
      ("disk.usage", 90.0)      // 磁盘使用率阈值90%
    ]
  )
  
  assert_true(capacity_analysis.success)
  assert_eq(capacity_analysis.capacity_alerts.length(), 9)  // 3服务 × 3资源指标
  
  // 验证容量分析结果
  for service in services {
    for metric_name in ["cpu.usage", "memory.usage", "disk.usage"] {
      let alert_key = service + ":" + metric_name
      let alert = capacity_analysis.capacity_alerts.find(fn(a) { a.key == alert_key })
      
      assert_not_eq(alert, None)
      assert_true(alert.threshold > 0)
      
      // 如果预测超过阈值，应该有告警
      if alert.exceeds_threshold {
        assert_true(alert.time_to_threshold > 0)
        assert_true(alert.recommended_action.length() > 0)
      }
    }
  }
  
  // 生成资源增长预测
  let growth_analysis = MetricsPrediction::analyze_resource_growth(
    telemetry_provider, 
    prediction_result.predictions
  )
  
  assert_true(growth_analysis.success)
  assert_eq(growth_analysis.growth_rates.length(), 12)  // 3服务 × 4指标
  
  // 验证增长分析结果
  for service in services {
    for metric_name in metrics {
      let growth_key = service + ":" + metric_name
      let growth = growth_analysis.growth_rates.find(fn(g) { g.key == growth_key })
      
      assert_not_eq(growth, None)
      assert_true(growth.daily_growth_rate >= -1.0)  // 增长率应该合理
      assert_true(growth.weekly_growth_rate >= -1.0)
      assert_true(growth.monthly_growth_rate >= -1.0)
    }
  }
  
  // 生成预测和容量规划报告
  let report = MetricsPrediction::generate_prediction_report(
    telemetry_provider, 
    training_result, 
    prediction_result, 
    capacity_analysis, 
    growth_analysis
  )
  
  assert_true(report.success)
  assert_true(report.html_content.length() > 0)
  
  // 验证报告内容
  assert_true(report.html_content.contains("预测分析报告"))
  assert_true(report.html_content.contains("模型性能"))
  assert_true(report.html_content.contains("容量规划"))
  assert_true(report.html_content.contains("资源增长分析"))
  
  assert_true(true)
}

// 辅助函数：获取基线度量值
fn get_baseline_metric_value(metric_name : String, day : Int, hour : Int) -> Double {
  let base_value = match metric_name {
    "request.latency" => 100.0,
    "error.rate" => 0.02,
    "throughput" => 1000.0,
    _ => 50.0
  }
  
  // 添加日季节性（工作时间更高）
  let daily_factor = if hour >= 9 && hour <= 17 { 1.2 } else { 0.8 }
  
  // 添加周季节性（工作日更高）
  let weekly_factor = if day % 7 < 5 { 1.1 } else { 0.9 }
  
  base_value * daily_factor * weekly_factor
}

// 辅助函数：获取历史度量值
fn get_historical_metric_value(metric_name : String, day : Int, hour : Int) -> Double {
  let base_value = match metric_name {
    "request.count" => 10000.0,
    "cpu.usage" => 60.0,
    "memory.usage" => 70.0,
    "disk.usage" => 40.0 + day.to_double() * 0.5,  // 磁盘使用率逐渐增长
    _ => 50.0
  }
  
  // 添加趋势
  let trend = day.to_double() * 2.0  // 每天增长2
  
  // 添加日季节性
  let daily_seasonality = 10.0 * Math::sin((hour - 6).to_double() / 24.0 * 2 * Math::PI)
  
  // 添加周季节性
  let weekly_seasonality = 5.0 * Math::sin(day.to_double() / 7.0 * 2 * Math::PI)
  
  // 添加噪声
  let noise = (Math::random() - 0.5) * base_value * 0.1
  
  base_value + trend + daily_seasonality + weekly_seasonality + noise
}

// 辅助函数：获取度量单位
fn get_metric_unit(metric_name : String) -> String {
  match metric_name {
    "request.latency" => "ms",
    "request.count" => "count",
    "error.rate" => "rate",
    "throughput" => "req/s",
    "cpu.usage" => "%",
    "memory.usage" => "%",
    "disk.usage" => "%",
    _ => "unit"
  }
}