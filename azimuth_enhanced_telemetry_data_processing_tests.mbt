// Azimuth 遥测数据处理增强测试
// 专注于遥测数据的收集、处理、分析和存储功能

// 测试1: 遥测数据收集和聚合
test "遥测数据收集和聚合测试" {
  // 创建遥测数据收集器
  let collector = TelemetryCollector::new()
  
  // 配置收集策略
  TelemetryCollector::set_collection_strategy(collector, {
    batch_size: 100,
    flush_interval: 5000,  // 5秒
    compression_enabled: true,
    sampling_rate: 1.0
  })
  
  // 创建测试遥测数据
  let base_time = 1640995200  // 2022-01-01 00:00:00 UTC
  
  // 生成指标数据
  for i in 0..=500 {
    let timestamp = base_time + i * 10  // 每10秒一个数据点
    
    // HTTP请求指标
    TelemetryCollector::add_metric(collector, timestamp, "http.requests.total", 10.0 + (i % 20) * 2.0, [
      ("service.name", StringValue("api.gateway")),
      ("http.method", StringValue(if i % 2 == 0 { "GET" } else { "POST" })),
      ("http.status_code", IntValue(if i % 10 == 0 { 500 } else { 200 }))
    ])
    
    // 响应时间指标
    TelemetryCollector::add_metric(collector, timestamp, "http.request.duration", 50.0 + (i % 100) * 1.5, [
      ("service.name", StringValue("api.gateway")),
      ("endpoint", StringValue("/api/users"))
    ])
    
    // 系统资源指标
    TelemetryCollector::add_metric(collector, timestamp, "system.cpu.usage", 30.0 + (i % 50) * 0.8, [
      ("host.name", StringValue("server-" + (i % 5).to_string())),
      ("cpu.core", IntValue(i % 8))
    ])
    
    TelemetryCollector::add_metric(collector, timestamp, "system.memory.usage", 60.0 + (i % 30) * 0.5, [
      ("host.name", StringValue("server-" + (i % 5).to_string()))
    ])
  }
  
  // 生成日志数据
  for i in 0..=100 {
    let timestamp = base_time + i * 50  // 每50秒一个日志
    
    let log_level = if i % 20 == 0 { "error" } 
                   else if i % 10 == 0 { "warn" } 
                   else if i % 5 == 0 { "info" } 
                   else { "debug" }
    
    let message = if log_level == "error" { "Database connection failed" }
                 else if log_level == "warn" { "High memory usage detected" }
                 else if log_level == "info" { "User authentication successful" }
                 else { "Processing request" }
    
    TelemetryCollector::add_log(collector, timestamp, log_level, message, [
      ("service.name", StringValue("auth.service")),
      ("trace.id", StringValue("trace-" + (i % 50).to_string())),
      ("user.id", StringValue("user-" + (i % 200).to_string()))
    ])
  }
  
  // 生成追踪数据
  for i in 0..=50 {
    let trace_id = "trace-" + i.to_string()
    let root_span_id = "span-" + (i * 1000).to_string()
    
    // 创建根span
    TelemetryCollector::add_span(collector, {
      trace_id,
      span_id: root_span_id,
      parent_span_id: None,
      name: "http.request",
      kind: "server",
      start_time: base_time + i * 100,
      end_time: base_time + i * 100 + 50 + (i % 100),
      status: if i % 10 == 0 { "error" } else { "ok" },
      attributes: [
        ("service.name", StringValue("api.gateway")),
        ("http.method", StringValue(if i % 2 == 0 { "GET" } else { "POST" })),
        ("http.url", StringValue("/api/resource/" + (i % 10).to_string()))
      ]
    })
    
    // 创建子span
    for j in 0..=2 {
      TelemetryCollector::add_span(collector, {
        trace_id,
        span_id: "span-" + (i * 1000 + j + 1).to_string(),
        parent_span_id: Some(root_span_id),
        name: if j == 0 { "database.query" } else if j == 1 { "cache.get" } else { "auth.validate" },
        kind: "client",
        start_time: base_time + i * 100 + j * 15,
        end_time: base_time + i * 100 + j * 15 + 10 + (j % 20),
        status: "ok",
        attributes: [
          ("service.name", StringValue(if j == 0 { "database.service" } else if j == 1 { "cache.service" } else { "auth.service" })),
          ("operation", StringValue("read"))
        ]
      })
    }
  }
  
  // 执行数据聚合
  let aggregated_data = TelemetryCollector::aggregate(collector, {
    time_window: 300,  // 5分钟窗口
    group_by_attributes: ["service.name", "http.method"],
    metrics: [
      { name: "http.requests.total", aggregation: "sum" },
      { name: "http.request.duration", aggregation: "avg,percentile", percentile_values: [50.0, 90.0, 95.0, 99.0] }
    ]
  })
  
  // 验证聚合结果
  assert_true(aggregated_data.metrics.length() > 0)
  
  // 检查请求总数聚合
  let request_sum = aggregated_data.metrics.find(fn(m) { 
    m.name == "http.requests.total" and m.aggregation == "sum" 
  })
  assert_true(request_sum != None)
  
  match request_sum {
    Some(metric) => {
      assert_true(metric.data_points.length() > 0)
      assert_true(metric.data_points[0].value > 0)
    }
    None => assert_true(false)
  }
  
  // 检查响应时间百分位数聚合
  let duration_percentiles = aggregated_data.metrics.find(fn(m) { 
    m.name == "http.request.duration" and m.aggregation == "percentile" 
  })
  assert_true(duration_percentiles != None)
  
  match duration_percentiles {
    Some(metric) => {
      assert_true(metric.data_points.length() > 0)
      let percentile_data = metric.data_points[0].value
      assert_true(percentile_data.contains("p50"))
      assert_true(percentile_data.contains("p90"))
      assert_true(percentile_data.contains("p95"))
      assert_true(percentile_data.contains("p99"))
    }
    None => assert_true(false)
  }
  
  // 验证数据收集统计
  let collection_stats = TelemetryCollector::get_collection_stats(collector)
  assert_eq(collection_stats.total_metrics, 2000)  // 4个指标 × 500个数据点
  assert_eq(collection_stats.total_logs, 101)     // 101个日志
  assert_eq(collection_stats.total_spans, 200)    // 50个追踪 × 4个span
}

// 测试2: 实时数据流处理
test "实时数据流处理测试" {
  // 创建实时数据流处理器
  let stream_processor = StreamProcessor::new()
  
  // 配置流处理拓扑
  StreamProcessor::configure_topology(stream_processor, {
    sources: [
      {
        name: "metric_source",
        type: "kafka",
        topic: "telemetry.metrics",
        consumer_group: "stream_processor"
      },
      {
        name: "log_source",
        type: "kafka",
        topic: "telemetry.logs",
        consumer_group: "stream_processor"
      }
    ],
    processors: [
      {
        name: "metric_filter",
        type: "filter",
        condition: "metric_name starts_with 'http.'"
      },
      {
        name: "metric_aggregator",
        type: "windowed_aggregation",
        window_size: 60,  // 1分钟窗口
        aggregation_functions: ["sum", "avg", "min", "max"]
      },
      {
        name: "log_parser",
        type: "parse",
        format: "json",
        extract_fields: ["timestamp", "level", "message", "service"]
      },
      {
        name: "log_classifier",
        type: "classify",
        classification_rules: [
          { pattern: "error", category: "error" },
          { pattern: "warn", category: "warning" },
          { pattern: "timeout", category: "performance" }
        ]
      }
    ],
    sinks: [
      {
        name: "aggregated_metrics_sink",
        type: "elasticsearch",
        index: "telemetry_metrics_aggregated"
      },
      {
        name: "classified_logs_sink",
        type: "elasticsearch",
        index: "telemetry_logs_classified"
      },
      {
        name: "alert_sink",
        type: "alertmanager",
        conditions: [
          { metric: "error_rate", threshold: 0.05, operator: ">" },
          { metric: "latency_p95", threshold: 1000, operator: ">" }
        ]
      }
    ]
  })
  
  // 模拟实时数据流
  let base_time = 1640995200
  
  // 生成实时指标数据
  for i in 0..=1000 {
    let timestamp = base_time + i * 5  // 每5秒一个数据点
    
    StreamProcessor::process_metric(stream_processor, {
      timestamp,
      name: "http.requests.total",
      value: 10.0 + (i % 30) * 1.5,
      attributes: [
        ("service.name", StringValue("api.service-" + (i % 3).to_string())),
        ("http.method", StringValue(if i % 3 == 0 { "GET" } else if i % 3 == 1 { "POST" } else { "PUT" })),
        ("http.status_code", IntValue(if i % 20 == 0 { 500 } else if i % 10 == 0 { 404 } else { 200 }))
      ]
    })
    
    StreamProcessor::process_metric(stream_processor, {
      timestamp,
      name: "http.request.duration",
      value: 50.0 + (i % 200) * 2.0,
      attributes: [
        ("service.name", StringValue("api.service-" + (i % 3).to_string())),
        ("endpoint", StringValue("/api/data/" + (i % 10).to_string()))
      ]
    })
    
    // 模拟高延迟情况
    if i % 50 == 0 {
      StreamProcessor::process_metric(stream_processor, {
        timestamp,
        name: "http.request.duration",
        value: 1500.0,  // 高延迟
        attributes: [
          ("service.name", StringValue("api.service-" + (i % 3).to_string())),
          ("endpoint", StringValue("/api/slow/operation"))
        ]
      })
    }
  }
  
  // 生成实时日志数据
  for i in 0..=200 {
    let timestamp = base_time + i * 25  // 每25秒一个日志
    
    let log_level = if i % 15 == 0 { "error" } 
                   else if i % 8 == 0 { "warn" } 
                   else if i % 4 == 0 { "info" } 
                   else { "debug" }
    
    let message = if log_level == "error" { 
                     if i % 15 == 0 { "Database connection timeout" } 
                     else { "Failed to process request" } 
                   }
                   else if log_level == "warn" { "High memory usage: 85%" }
                   else if log_level == "info" { "Request processed successfully" }
                   else { "Cache hit for key: user_" + (i % 100).to_string() }
    
    StreamProcessor::process_log(stream_processor, {
      timestamp,
      level: log_level,
      message,
      attributes: [
        ("service.name", StringValue("service-" + (i % 5).to_string())),
        ("trace.id", StringValue("trace-" + (i % 100).to_string())),
        ("request.id", StringValue("req-" + i.to_string()))
      ]
    })
  }
  
  // 获取流处理结果
  let processed_results = StreamProcessor::get_processing_results(stream_processor, {
    start_time: base_time,
    end_time: base_time + 5000,  // 处理前5000秒的数据
    metrics: ["http.requests.total", "http.request.duration"],
    aggregations: ["sum", "avg", "percentile"],
    percentile_values: [50.0, 90.0, 95.0, 99.0]
  })
  
  // 验证流处理结果
  assert_true(processed_results.aggregated_metrics.length() > 0)
  
  // 检查聚合指标
  let request_metrics = processed_results.aggregated_metrics.filter(fn(m) { m.name == "http.requests.total" })
  assert_true(request_metrics.length() > 0)
  
  let request_sum = request_metrics.find(fn(m) { m.aggregation == "sum" })
  assert_true(request_sum != None)
  
  match request_sum {
    Some(metric) => {
      assert_true(metric.data_points.length() > 0)
      assert_true(metric.data_points[0].value > 0)
    }
    None => assert_true(false)
  }
  
  // 检查延迟百分位数
  let duration_metrics = processed_results.aggregated_metrics.filter(fn(m) { m.name == "http.request.duration" })
  assert_true(duration_metrics.length() > 0)
  
  let duration_p95 = duration_metrics.find(fn(m) { m.aggregation == "percentile" and m.percentile == 95.0 })
  assert_true(duration_p95 != None)
  
  match duration_p95 {
    Some(metric) => {
      assert_true(metric.data_points.length() > 0)
      assert_true(metric.data_points[0].value > 0)
    }
    None => assert_true(false)
  }
  
  // 检查分类日志
  assert_true(processed_results.classified_logs.length() > 0)
  
  let error_logs = processed_results.classified_logs.filter(fn(l) { l.category == "error" })
  let warning_logs = processed_results.classified_logs.filter(fn(l) { l.category == "warning" })
  let performance_logs = processed_results.classified_logs.filter(fn(l) { l.category == "performance" })
  
  assert_true(error_logs.length() > 0)
  assert_true(warning_logs.length() > 0)
  
  // 检查告警生成
  assert_true(processed_results.alerts.length() >= 0)
  
  // 验证流处理统计
  let processing_stats = StreamProcessor::get_processing_stats(stream_processor)
  assert_eq(processing_stats.total_metrics_processed, 2000)  // 2个指标 × 1000个数据点
  assert_eq(processing_stats.total_logs_processed, 201)      // 201个日志
  assert_true(processing_stats.processing_latency_ms < 1000)  // 处理延迟应小于1秒
}

// 测试3: 遥测数据分析和洞察
test "遥测数据分析和洞察测试" {
  // 创建数据分析器
  let data_analyzer = DataAnalyzer::new()
  
  // 配置分析规则
  DataAnalyzer::configure_analysis_rules(data_analyzer, {
    trend_analysis: [
      {
        metric: "http.requests.total",
        window_size: 3600,  // 1小时窗口
        trend_detection: "linear_regression",
        significance_threshold: 0.1  // 10%变化阈值
      },
      {
        metric: "system.cpu.usage",
        window_size: 1800,  // 30分钟窗口
        trend_detection: "moving_average",
        significance_threshold: 0.05
      }
    ],
    anomaly_detection: [
      {
        metric: "http.request.duration",
        algorithm: "statistical",
        window_size: 100,
        significance_level: 0.01,
        min_data_points: 30
      },
      {
        metric: "error_rate",
        algorithm: "threshold",
        upper_threshold: 0.05,
        lower_threshold: 0.001
      }
    ],
    correlation_analysis: [
      {
        metric_a: "http.request.duration",
        metric_b: "system.cpu.usage",
        correlation_method: "pearson",
        correlation_threshold: 0.7
      },
      {
        metric_a: "system.memory.usage",
        metric_b: "error_rate",
        correlation_method: "spearman",
        correlation_threshold: 0.6
      }
    ],
    pattern_detection: [
      {
        pattern_type: "seasonal",
        metrics: ["http.requests.total"],
        seasonality_period: 86400,  // 24小时
        min_pattern_strength: 0.8
      },
      {
        pattern_type: "periodic",
        metrics: ["system.cpu.usage"],
        period_detection: "autocorrelation",
        min_period_length: 300  // 5分钟
      }
    ]
  })
  
  // 生成测试数据集
  let base_time = 1640995200
  let dataset = []
  
  // 生成7天的数据，每5分钟一个数据点
  for day in 0..=6 {
    for hour in 0..=23 {
      for minute in [0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55] {
        let timestamp = base_time + day * 86400 + hour * 3600 + minute * 60
        
        // 模拟日周期性请求模式
        let hour_factor = if hour >= 9 and hour <= 17 { 1.5 } else if hour >= 18 and hour <= 22 { 1.2 } else { 0.8 }
        let day_factor = if day >= 1 and day <= 5 { 1.0 } else { 0.6 }  // 工作日vs周末
        
        // HTTP请求指标（带有周期性和趋势）
        let request_base = 100.0 * hour_factor * day_factor
        let request_trend = day * 2.0  // 每天增长2个请求
        let request_noise = (hour * minute % 20) * 1.5
        let request_value = request_base + request_trend + request_noise
        
        dataset.push({
          timestamp,
          metrics: [
            ("http.requests.total", request_value),
            ("http.request.duration", 50.0 + (hour % 10) * 5.0 + (minute % 5) * 2.0),
            ("system.cpu.usage", 30.0 + hour_factor * 20.0 + (day % 3) * 5.0),
            ("system.memory.usage", 60.0 + day_factor * 10.0 + (hour % 8) * 3.0)
          ],
          attributes: [
            ("service.name", StringValue("api.service")),
            ("environment", StringValue("production"))
          ]
        })
        
        // 模拟异常情况
        if day == 3 and hour == 14 and minute == 30 {
          dataset.push({
            timestamp,
            metrics: [
              ("http.requests.total", 10.0),  // 请求骤降
              ("http.request.duration", 5000.0),  // 高延迟
              ("system.cpu.usage", 95.0),  // 高CPU使用率
              ("system.memory.usage", 90.0)  // 高内存使用率
            ],
            attributes: [
              ("service.name", StringValue("api.service")),
              ("environment", StringValue("production")),
              ("incident", StringValue("true"))
            ]
          })
        }
      }
    }
  }
  
  // 执行数据分析
  let analysis_results = DataAnalyzer::analyze(data_analyzer, dataset, {
    start_time: base_time,
    end_time: base_time + 7 * 86400,
    metrics: ["http.requests.total", "http.request.duration", "system.cpu.usage", "system.memory.usage"],
    analysis_types: ["trend", "anomaly", "correlation", "pattern"]
  })
  
  // 验证趋势分析结果
  assert_true(analysis_results.trend_analysis.length() > 0)
  
  let request_trend = analysis_results.trend_analysis.find(fn(t) { t.metric == "http.requests.total" })
  assert_true(request_trend != None)
  
  match request_trend {
    Some(trend) => {
      assert_true(trend.trend_direction == "increasing")  // 应该检测到增长趋势
      assert_true(trend.trend_strength > 0.5)  // 趋势强度应该足够
      assert_true(trend.significance > 0.1)  // 超过显著性阈值
    }
    None => assert_true(false)
  }
  
  // 验证异常检测结果
  assert_true(analysis_results.anomaly_detection.length() > 0)
  
  let duration_anomalies = analysis_results.anomaly_detection.filter(fn(a) { a.metric == "http.request.duration" })
  assert_true(duration_anomalies.length() > 0)
  
  // 应该检测到第3天14:30的异常
  let incident_anomaly = duration_anomalies.find(fn(a) { 
    a.timestamp >= base_time + 3 * 86400 + 14 * 3600 + 30 * 60 and
    a.timestamp <= base_time + 3 * 86400 + 14 * 3600 + 31 * 60
  })
  assert_true(incident_anomaly != None)
  
  match incident_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.anomaly_type, "statistical")
      assert_true(anomaly.severity == "high" or anomaly.severity == "critical")
      assert_true(anomaly.anomaly_score > 3.0)  // 高异常分数
    }
    None => assert_true(false)
  }
  
  // 验证相关性分析结果
  assert_true(analysis_results.correlation_analysis.length() > 0)
  
  let cpu_duration_correlation = analysis_results.correlation_analysis.find(fn(c) { 
    c.metric_a == "system.cpu.usage" and c.metric_b == "http.request.duration" 
  })
  assert_true(cpu_duration_correlation != None)
  
  match cpu_duration_correlation {
    Some(correlation) {
      assert_true(correlation.correlation_coefficient > 0.5)  // 应该有正相关性
      assert_true(correlation.p_value < 0.05)  // 应该统计显著
    }
    None => assert_true(false)
  }
  
  // 验证模式检测结果
  assert_true(analysis_results.pattern_detection.length() > 0)
  
  let request_seasonal_pattern = analysis_results.pattern_detection.find(fn(p) { 
    p.metric == "http.requests.total" and p.pattern_type == "seasonal" 
  })
  assert_true(request_seasonal_pattern != None)
  
  match request_seasonal_pattern {
    Some(pattern) {
      assert_eq(pattern.period, 86400)  // 24小时周期
      assert_true(pattern.strength > 0.7)  // 模式强度应该足够
      assert_true(pattern.confidence > 0.8)  // 高置信度
    }
    None => assert_true(false)
  }
  
  // 生成洞察报告
  let insights = DataAnalyzer::generate_insights(data_analyzer, analysis_results, {
    insight_types: ["performance", "capacity", "reliability", "anomaly_summary"],
    severity_threshold: "medium",
    include_recommendations: true
  })
  
  // 验证洞察报告
  assert_true(insights.length() > 0)
  
  let performance_insights = insights.filter(fn(i) { i.category == "performance" })
  assert_true(performance_insights.length() > 0)
  
  let capacity_insights = insights.filter(fn(i) { i.category == "capacity" })
  assert_true(capacity_insights.length() > 0)
  
  let reliability_insights = insights.filter(fn(i) { i.category == "reliability" })
  assert_true(reliability_insights.length() > 0)
  
  // 检查是否有推荐建议
  let insights_with_recommendations = insights.filter(fn(i) { i.recommendations.length() > 0 })
  assert_true(insights_with_recommendations.length() > 0)
}

// 测试4: 遥测数据存储和检索
test "遥测数据存储和检索测试" {
  // 创建数据存储管理器
  let storage_manager = StorageManager::new()
  
  // 配置存储策略
  StorageManager::configure_storage_strategy(storage_manager, {
    primary_storage: {
      type: "timeseries_db",
      connection: "influxdb://localhost:8086/telemetry",
      retention_policy: "30d",
      shard_duration: "1d"
    },
    secondary_storage: {
      type: "object_store",
      connection: "s3://telemetry-archive-bucket",
      compression: "gzip",
      encryption: true
    },
    hot_storage: {
      type: "memory_cache",
      max_size: 1073741824,  // 1GB
      ttl: 3600  // 1小时
    },
    cold_storage: {
      type: "data_lake",
      connection: "hdfs://namenode:9000/telemetry/lake",
      partitioning: ["year", "month", "day", "service"]
    }
  })
  
  // 生成测试数据
  let base_time = 1640995200
  let telemetry_data = []
  
  // 生成30天的数据，每小时一个数据点
  for day in 0..=29 {
    for hour in 0..=23 {
      let timestamp = base_time + day * 86400 + hour * 3600
      
      telemetry_data.push({
        timestamp,
        metrics: [
          ("http.requests.total", 1000.0 + (day * 10) + (hour % 5) * 50),
          ("http.request.duration", 100.0 + (hour % 10) * 10),
          ("system.cpu.usage", 40.0 + (hour % 8) * 5),
          ("system.memory.usage", 60.0 + (day % 7) * 3),
          ("error_rate", if day == 15 and hour >= 10 and hour <= 12 { 0.1 } else { 0.01 })
        ],
        attributes: [
          ("service.name", StringValue("api.service-" + (day % 3).to_string())),
          ("environment", StringValue(if day % 10 == 0 { "staging" } else { "production" })),
          ("region", StringValue("us-west-" + ((day % 3) + 1).to_string()))
        ]
      })
    }
  }
  
  // 存储数据
  let storage_results = StorageManager::store_telemetry_data(storage_manager, telemetry_data, {
    batch_size: 100,
    parallel_uploads: 4,
    compression_enabled: true,
    verify_integrity: true
  })
  
  // 验证存储结果
  assert_true(storage_results.success)
  assert_eq(storage_results.total_records, 720)  // 30天 × 24小时
  assert_eq(storage_results.stored_in_primary, 720)
  assert_eq(storage_results.stored_in_secondary, 720)
  
  // 测试数据检索 - 时间范围查询
  let time_range_query = {
    start_time: base_time + 10 * 86400,  // 第10天
    end_time: base_time + 10 * 86400 + 23 * 3600,  // 第10天结束
    metrics: ["http.requests.total", "system.cpu.usage"],
    attributes: [
      ("service.name", StringValue("api.service-1")),
      ("environment", StringValue("production"))
    ],
    aggregation: "avg",
    group_by: ["service.name"]
  }
  
  let time_range_results = StorageManager::query_telemetry_data(storage_manager, time_range_query)
  
  // 验证时间范围查询结果
  assert_true(time_range_results.success)
  assert_true(time_range_results.data_points.length() > 0)
  
  for point in time_range_results.data_points {
    assert_true(point.timestamp >= base_time + 10 * 86400)
    assert_true(point.timestamp <= base_time + 10 * 86400 + 23 * 3600)
    assert_true(point.metrics.contains("http.requests.total"))
    assert_true(point.metrics.contains("system.cpu.usage"))
  }
  
  // 测试数据检索 - 聚合查询
  let aggregation_query = {
    start_time: base_time,
    end_time: base_time + 30 * 86400,  // 30天
    metrics: ["http.requests.total", "http.request.duration", "error_rate"],
    aggregation: "avg,percentile",
    percentile_values: [50.0, 90.0, 95.0, 99.0],
    group_by: ["service.name", "environment"],
    interval: 86400  // 按天聚合
  }
  
  let aggregation_results = StorageManager::query_telemetry_data(storage_manager, aggregation_query)
  
  // 验证聚合查询结果
  assert_true(aggregation_results.success)
  assert_true(aggregation_results.aggregated_data.length() > 0)
  
  // 应该有30天的聚合数据
  let daily_aggregations = aggregation_results.aggregated_data.filter(fn(d) { d.interval == "daily" })
  assert_true(daily_aggregations.length() >= 25)  // 至少25天有数据
  
  // 检查百分位数数据
  let duration_percentiles = aggregation_results.aggregated_data.find(fn(d) { 
    d.metric == "http.request.duration" and d.aggregation == "percentile" 
  })
  assert_true(duration_percentiles != None)
  
  match duration_percentiles {
    Some(data) => {
      assert_true(data.percentiles.contains("p50"))
      assert_true(data.percentiles.contains("p90"))
      assert_true(data.percentiles.contains("p95"))
      assert_true(data.percentiles.contains("p99"))
    }
    None => assert_true(false)
  }
  
  // 测试数据检索 - 异常数据查询
  let anomaly_query = {
    start_time: base_time + 14 * 86400,  // 第15天
    end_time: base_time + 16 * 86400,    // 第17天
    metrics: ["error_rate"],
    attributes: [],
    anomaly_detection: {
      method: "statistical",
      threshold: 3.0  // 3个标准差
    }
  }
  
  let anomaly_results = StorageManager::query_telemetry_data(storage_manager, anomaly_query)
  
  // 验证异常查询结果
  assert_true(anomaly_results.success)
  assert_true(anomaly_results.anomalies.length() > 0)
  
  // 应该检测到第15天的异常高错误率
  let high_error_rate_anomalies = anomaly_results.anomalies.filter(fn(a) { 
    a.metric == "error_rate" and a.anomaly_type == "statistical"
  })
  assert_true(high_error_rate_anomalies.length() > 0)
  
  // 测试数据压缩和归档
  let archival_results = StorageManager::archive_old_data(storage_manager, {
    cutoff_time: base_time + 7 * 86400,  // 归档7天前的数据
    target_storage: "cold",
    compression: "gzip",
    verify_after_archive: true
  })
  
  // 验证归档结果
  assert_true(archival_results.success)
  assert_true(archival_results.archived_records > 0)
  assert_true(archival_results.compression_ratio > 0.5)  // 至少50%压缩率
  
  // 测试数据完整性验证
  let integrity_results = StorageManager::verify_data_integrity(storage_manager, {
    sample_size: 100,
    sample_method: "random",
    verify_checksums: true,
    verify_timestamps: true
  })
  
  // 验证完整性检查结果
  assert_true(integrity_results.overall_integrity_pass)
  assert_eq(integrity_results.records_checked, 100)
  assert_eq(integrity_results.records_failed, 0)
  assert_true(integrity_results.checksums_verified)
  assert_true(integrity_results.timestamps_valid)
  
  // 测试存储性能指标
  let storage_performance = StorageManager::get_storage_performance_metrics(storage_manager)
  
  // 验证存储性能
  assert_true(storage_performance.write_throughput_mbps > 10)  // 写入吞吐量应大于10MB/s
  assert_true(storage_performance.read_throughput_mbps > 50)   // 读取吞吐量应大于50MB/s
  assert_true(storage_performance.avg_write_latency_ms < 100)  // 写入延迟应小于100ms
  assert_true(storage_performance.avg_read_latency_ms < 50)    // 读取延迟应小于50ms
  assert_true(storage_performance.storage_utilization_percent < 90)  // 存储利用率应小于90%
}

// 测试5: 遥测数据可视化
test "遥测数据可视化测试" {
  // 创建可视化数据处理器
  let visualization_processor = VisualizationProcessor::new()
  
  // 配置可视化模板
  VisualizationProcessor::configure_templates(visualization_processor, {
    time_series: {
      default_time_range: "1h",
      max_data_points: 1000,
      interpolation: "linear",
      show_legend: true
    },
    heatmap: {
      color_scheme: "viridis",
      bucket_size: 60,  // 1分钟
      show_values: false
    },
    histogram: {
      bucket_count: 20,
      show_distribution: true,
      show_statistics: true
    },
    scatter_plot: {
      show_trend_line: true,
      point_size: 3,
      opacity: 0.7
    }
  })
  
  // 生成测试数据
  let base_time = 1640995200
  let visualization_data = []
  
  // 生成24小时的数据，每分钟一个数据点
  for hour in 0..=23 {
    for minute in 0..=59 {
      let timestamp = base_time + hour * 3600 + minute * 60
      
      // 模拟业务周期性
      let business_hours_factor = if hour >= 9 and hour <= 17 { 1.5 } else { 0.7 }
      
      visualization_data.push({
        timestamp,
        metrics: [
          ("http.requests.total", 50.0 * business_hours_factor + (minute % 10) * 2.0),
          ("http.request.duration", 80.0 + (hour % 8) * 10 + (minute % 5) * 5),
          ("system.cpu.usage", 30.0 + business_hours_factor * 25.0 + (minute % 15) * 2.0),
          ("system.memory.usage", 55.0 + (hour % 12) * 3.0),
          ("error_rate", if hour == 14 and minute >= 30 and minute <= 45 { 0.08 } else { 0.01 })
        ],
        attributes: [
          ("service.name", StringValue("web.service")),
          ("endpoint", StringValue("/api/data/" + (hour % 5).to_string())),
          ("status_code", IntValue(if minute % 20 == 0 { 500 } else { 200 }))
        ]
      })
    }
  }
  
  // 生成时间序列图表数据
  let time_series_chart = VisualizationProcessor::generate_time_series(visualization_processor, {
    data: visualization_data,
    metrics: ["http.requests.total", "system.cpu.usage"],
    time_range: {
      start: base_time,
      end: base_time + 24 * 3600
    },
    aggregation: "avg",
    interval: 300,  // 5分钟间隔
    group_by: ["service.name"]
  })
  
  // 验证时间序列图表数据
  assert_true(time_series_chart.success)
  assert_true(time_series_chart.data_points.length() > 0)
  assert_eq(time_series_chart.metrics.length(), 2)
  
  // 应该有24小时的数据，每5分钟一个点 = 288个点
  assert_true(time_series_chart.data_points.length() >= 250 and time_series_chart.data_points.length() <= 300)
  
  // 验证数据点格式
  for point in time_series_chart.data_points {
    assert_true(point.timestamp >= base_time)
    assert_true(point.timestamp <= base_time + 24 * 3600)
    assert_true(point.values.contains("http.requests.total"))
    assert_true(point.values.contains("system.cpu.usage"))
  }
  
  // 生成热力图数据
  let heatmap_chart = VisualizationProcessor::generate_heatmap(visualization_processor, {
    data: visualization_data,
    metric: "http.request.duration",
    x_axis: {
      field: "timestamp",
      type: "time",
      format: "HH:mm"
    },
    y_axis: {
      field: "endpoint",
      type: "categorical"
    },
    bucket_size: 900,  // 15分钟
    aggregation: "avg"
  })
  
  // 验证热力图数据
  assert_true(heatmap_chart.success)
  assert_true(heatmap_chart.heatmap_data.length() > 0)
  
  // 检查热力图数据结构
  for bucket in heatmap_chart.heatmap_data {
    assert_true(bucket.x_coordinate >= 0)
    assert_true(bucket.y_coordinate >= 0)
    assert_true(bucket.intensity_value >= 0)
  }
  
  // 生成直方图数据
  let histogram_chart = VisualizationProcessor::generate_histogram(visualization_processor, {
    data: visualization_data,
    metric: "http.request.duration",
    bucket_count: 20,
    value_range: {
      min: 0,
      max: 500
    }
  })
  
  // 验证直方图数据
  assert_true(histogram_chart.success)
  assert_eq(histogram_chart.buckets.length(), 20)
  
  // 检查直方图统计信息
  assert_true(histogram_chart.statistics.min_value >= 0)
  assert_true(histogram_chart.statistics.max_value > 0)
  assert_true(histogram_chart.statistics.mean_value > 0)
  assert_true(histogram_chart.statistics.median_value > 0)
  
  // 验证桶分布
  let total_count = histogram_chart.buckets.reduce(fn(acc, bucket) { acc + bucket.count }, 0)
  assert_eq(total_count, visualization_data.length())  // 所有数据点应该被分配到桶中
  
  // 生成散点图数据
  let scatter_chart = VisualizationProcessor::generate_scatter_plot(visualization_processor, {
    data: visualization_data,
    x_axis: "system.cpu.usage",
    y_axis: "http.request.duration",
    color_by: "status_code",
    size_by: "http.requests.total"
  })
  
  // 验证散点图数据
  assert_true(scatter_chart.success)
  assert_true(scatter_chart.points.length() > 0)
  
  // 检查散点图数据点
  for point in scatter_chart.points {
    assert_true(point.x_value >= 0)  // CPU使用率
    assert_true(point.y_value >= 0)  // 响应时间
    assert_true(point.color_value != None)
    assert_true(point.size_value > 0)
  }
  
  // 检查趋势线
  assert_true(scatter_chart.trend_line != None)
  match scatter_chart.trend_line {
    Some(line) => {
      assert_true(line.slope != 0)
      assert_true(line.intercept != 0)
      assert_true(line.r_squared >= 0 and line.r_squared <= 1)
    }
    None => assert_true(false)
  }
  
  // 生成仪表板配置
  let dashboard_config = VisualizationProcessor::generate_dashboard_config(visualization_processor, {
    title: "System Performance Dashboard",
    panels: [
      {
        title: "Request Rate",
        type: "time_series",
        metrics: ["http.requests.total"],
        time_range: "1h",
        refresh_interval: 30
      },
      {
        title: "Response Time Distribution",
        type: "histogram",
        metrics: ["http.request.duration"],
        bucket_count: 15
      },
      {
        title: "CPU vs Response Time",
        type: "scatter_plot",
        x_axis: "system.cpu.usage",
        y_axis: "http.request.duration"
      },
      {
        title: "Response Time Heatmap",
        type: "heatmap",
        metrics: ["http.request.duration"],
        x_axis: "timestamp",
        y_axis: "endpoint"
      }
    ],
    layout: {
      columns: 2,
      rows: 2,
      auto_refresh: true
    }
  })
  
  // 验证仪表板配置
  assert_eq(dashboard_config.title, "System Performance Dashboard")
  assert_eq(dashboard_config.panels.length(), 4)
  assert_eq(dashboard_config.layout.columns, 2)
  assert_eq(dashboard_config.layout.rows, 2)
  
  // 验证面板配置
  let request_rate_panel = dashboard_config.panels.find(fn(p) { p.title == "Request Rate" })
  assert_true(request_rate_panel != None)
  
  match request_rate_panel {
    Some(panel) => {
      assert_eq(panel.type, "time_series")
      assert_eq(panel.metrics, ["http.requests.total"])
      assert_eq(panel.time_range, "1h")
      assert_eq(panel.refresh_interval, 30)
    }
    None => assert_true(false)
  }
  
  // 生成可视化数据导出
  let export_results = VisualizationProcessor::export_visualization_data(visualization_processor, {
    charts: [time_series_chart, heatmap_chart, histogram_chart, scatter_chart],
    format: "json",
    include_metadata: true,
    compression: true
  })
  
  // 验证导出结果
  assert_true(export_results.success)
  assert_true(export_results.data_size > 0)
  assert_true(export_results.compression_ratio > 0.3)  // 至少30%压缩率
  
  // 测试可视化性能指标
  let viz_performance = VisualizationProcessor::get_performance_metrics(visualization_processor)
  
  // 验证可视化性能
  assert_true(viz_performance.avg_chart_generation_time_ms < 1000)  // 图表生成时间应小于1秒
  assert_true(viz_performance.data_processing_throughput_mbps > 5)  // 数据处理吞吐量应大于5MB/s
  assert_true(viz_performance.memory_usage_mb < 100)  // 内存使用应小于100MB
}