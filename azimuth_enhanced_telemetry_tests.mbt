// Azimuth Enhanced Telemetry Test Suite
// 增强遥测测试套件 - 包含高级遥测功能的测试用例

// Test 1: 遥测数据采集测试
test "telemetry data collection operations" {
  // 创建数据采集器
  let collector = @azimuth.DataCollector.new({
    service_name: "azimuth-test-service",
    collection_interval: 1000,
    batch_size: 50,
    enable_compression: true
  })
  
  // 测试数据采集配置
  assert_eq(collector.config.service_name, "azimuth-test-service")
  assert_eq(collector.config.collection_interval, 1000)
  assert_eq(collector.config.batch_size, 50)
  assert_true(collector.config.enable_compression)
  
  // 模拟数据采集
  let sample_data = [
    @azimuth.TelemetryPoint {
      metric_name: "cpu_usage",
      value: 75.5,
      timestamp: @azimuth.current_timestamp(),
      tags: [("host", "server1"), ("region", "us-west")]
    },
    @azimuth.TelemetryPoint {
      metric_name: "memory_usage",
      value: 60.2,
      timestamp: @azimuth.current_timestamp(),
      tags: [("host", "server1"), ("region", "us-west")]
    }
  ]
  
  // 测试数据采集
  let collection_result = @azimuth.collect_data(collector, sample_data)
  assert_true(collection_result.success)
  assert_eq(collection_result.collected_points, 2)
  
  // 验证采集的数据
  let collected_metrics = @azimuth.get_collected_metrics(collector)
  assert_eq(collected_metrics.length(), 2)
  assert_true(collected_metrics.contains("cpu_usage"))
  assert_true(collected_metrics.contains("memory_usage"))
}

// Test 2: 遥测数据处理测试
test "telemetry data processing operations" {
  // 创建数据处理器
  let processor = @azimuth.DataProcessor.new({
    enable_filtering: true,
    enable_aggregation: true,
    enable_normalization: true,
    filter_rules: [
      @azimuth.FilterRule { metric_pattern: "test.*", action: "exclude" },
      @azimuth.FilterRule { metric_pattern: "prod.*", action: "include" }
    ]
  })
  
  // 创建测试数据
  let raw_data = [
    @azimuth.RawTelemetryData {
      id: "data-1",
      metric_name: "test.cpu",
      value: 80.0,
      timestamp: @azimuth.current_timestamp(),
      quality_score: 0.95
    },
    @azimuth.RawTelemetryData {
      id: "data-2",
      metric_name: "prod.memory",
      value: 65.5,
      timestamp: @azimuth.current_timestamp(),
      quality_score: 0.98
    },
    @azimuth.RawTelemetryData {
      id: "data-3",
      metric_name: "prod.disk",
      value: 45.2,
      timestamp: @azimuth.current_timestamp(),
      quality_score: 0.87
    }
  ]
  
  // 测试数据处理
  let processing_result = @azimuth.process_data(processor, raw_data)
  assert_true(processing_result.success)
  
  // 验证过滤结果（test.* 应被排除）
  assert_eq(processing_result.filtered_data.length(), 2)
  assert_false(processing_result.filtered_data.any(fn(d) { d.metric_name == "test.cpu" }))
  assert_true(processing_result.filtered_data.any(fn(d) { d.metric_name == "prod.memory" }))
  assert_true(processing_result.filtered_data.any(fn(d) { d.metric_name == "prod.disk" }))
  
  // 测试数据聚合
  let aggregation_result = @azimuth.aggregate_data(processing_result.filtered_data, ["metric_name"])
  assert_eq(aggregation_result.aggregated_metrics.length(), 2)
  
  // 测试数据标准化
  let normalized_data = @azimuth.normalize_data(processing_result.filtered_data)
  assert_true(normalized_data.all(fn(d) { d.value >= 0.0 and d.value <= 100.0 }))
}

// Test 3: 遥测数据分析测试
test "telemetry data analysis operations" {
  // 创建数据分析器
  let analyzer = @azimuth.DataAnalyzer.new({
    enable_trend_analysis: true,
    enable_anomaly_detection: true,
    enable_correlation_analysis: true,
    analysis_window: 3600, // 1小时窗口
    confidence_threshold: 0.95
  })
  
  // 创建时间序列数据
  let time_series_data = [
    @azimuth.TimeSeriesPoint {
      timestamp: @azimuth.current_timestamp() - 3600,
      value: 50.0,
      metric_name: "response_time"
    },
    @azimuth.TimeSeriesPoint {
      timestamp: @azimuth.current_timestamp() - 2700,
      value: 52.0,
      metric_name: "response_time"
    },
    @azimuth.TimeSeriesPoint {
      timestamp: @azimuth.current_timestamp() - 1800,
      value: 55.0,
      metric_name: "response_time"
    },
    @azimuth.TimeSeriesPoint {
      timestamp: @azimuth.current_timestamp() - 900,
      value: 58.0,
      metric_name: "response_time"
    },
    @azimuth.TimeSeriesPoint {
      timestamp: @azimuth.current_timestamp(),
      value: 95.0, // 异常值
      metric_name: "response_time"
    }
  ]
  
  // 测试趋势分析
  let trend_analysis = @azimuth.analyze_trends(analyzer, time_series_data)
  assert_true(trend_analysis.has_trend)
  assert_eq(trend_analysis.trend_direction, "increasing")
  assert_true(trend_analysis.confidence_score > 0.8)
  
  // 测试异常检测
  let anomaly_detection = @azimuth.detect_anomalies(analyzer, time_series_data)
  assert_true(anomaly_detection.has_anomalies)
  assert_eq(anomaly_detection.anomalies.length(), 1)
  assert_eq(anomaly_detection.anomalies[0].value, 95.0)
  
  // 测试相关性分析
  let correlated_data = [
    @azimuth.TimeSeriesPoint {
      timestamp: @azimuth.current_timestamp(),
      value: 95.0,
      metric_name: "response_time"
    },
    @azimuth.TimeSeriesPoint {
      timestamp: @azimuth.current_timestamp(),
      value: 85.0,
      metric_name: "cpu_usage"
    }
  ]
  
  let correlation_analysis = @azimuth.analyze_correlations(analyzer, correlated_data)
  assert_true(correlation_analysis.correlations.length() > 0)
  assert_true(correlation_analysis.correlations[0].correlation_coefficient > 0.5)
}

// Test 4: 遥测指标聚合测试
test "telemetry metrics aggregation operations" {
  // 创建指标聚合器
  let aggregator = @azimuth.MetricsAggregator.new({
    aggregation_window: 300, // 5分钟窗口
    enable_percentiles: true,
    enable_histograms: true,
    percentiles: [50, 90, 95, 99]
  })
  
  // 创建指标数据
  let metrics_data = [
    @azimuth.MetricPoint {
      name: "http_request_duration",
      value: 100.0,
      timestamp: @azimuth.current_timestamp() - 240,
      tags: [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.MetricPoint {
      name: "http_request_duration",
      value: 150.0,
      timestamp: @azimuth.current_timestamp() - 180,
      tags: [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.MetricPoint {
      name: "http_request_duration",
      value: 200.0,
      timestamp: @azimuth.current_timestamp() - 120,
      tags: [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.MetricPoint {
      name: "http_request_duration",
      value: 250.0,
      timestamp: @azimuth.current_timestamp() - 60,
      tags: [("endpoint", "/api/users"), ("method", "GET")]
    },
    @azimuth.MetricPoint {
      name: "http_request_duration",
      value: 300.0,
      timestamp: @azimuth.current_timestamp(),
      tags: [("endpoint", "/api/users"), ("method", "GET")]
    }
  ]
  
  // 测试基本聚合
  let basic_aggregation = @azimuth.aggregate_basic_metrics(aggregator, metrics_data)
  assert_eq(basic_aggregation.count, 5)
  assert_eq(basic_aggregation.sum, 1000.0)
  assert_eq(basic_aggregation.min, 100.0)
  assert_eq(basic_aggregation.max, 300.0)
  assert_eq(basic_aggregation.avg, 200.0)
  
  // 测试百分位数计算
  let percentile_aggregation = @azimuth.aggregate_percentiles(aggregator, metrics_data)
  assert_eq(percentile_aggregation.p50, 200.0)
  assert_eq(percentile_aggregation.p90, 280.0)
  assert_eq(percentile_aggregation.p95, 290.0)
  assert_eq(percentile_aggregation.p99, 296.0)
  
  // 测试直方图生成
  let histogram = @azimuth.generate_histogram(aggregator, metrics_data, 5)
  assert_eq(histogram.buckets.length(), 5)
  assert_eq(histogram.buckets[0].count, 1) // 100-140
  assert_eq(histogram.buckets[1].count, 1) // 140-180
  assert_eq(histogram.buckets[2].count, 1) // 180-220
  assert_eq(histogram.buckets[3].count, 1) // 220-260
  assert_eq(histogram.buckets[4].count, 1) // 260-300
  
  // 测试按标签聚合
  let tagged_aggregation = @azimuth.aggregate_by_tags(aggregator, metrics_data, ["endpoint", "method"])
  assert_eq(tagged_aggregation.groups.length(), 1)
  assert_eq(tagged_aggregation.groups[0].tags[0], ("endpoint", "/api/users"))
  assert_eq(tagged_aggregation.groups[0].tags[1], ("method", "GET"))
  assert_eq(tagged_aggregation.groups[0].metrics.count, 5)
}

// Test 5: 遥测异常检测测试
test "telemetry anomaly detection operations" {
  // 创建异常检测器
  let detector = @azimuth.AnomalyDetector.new({
    algorithm: "statistical",
    sensitivity: 0.8,
    training_window: 7200, // 2小时训练窗口
    detection_window: 300,  // 5分钟检测窗口
    min_data_points: 50
  })
  
  // 创建正常模式数据
  let normal_data = []
  for i = 0; i < 100; i = i + 1 {
    normal_data = normal_data.push(@azimuth.TelemetryPoint {
      metric_name: "cpu_usage",
      value: 50.0 + @azimuth.random_gaussian(0.0, 5.0), // 正态分布，均值50，标准差5
      timestamp: @azimuth.current_timestamp() - (100 - i) * 60,
      tags: [("host", "server1")]
    })
  }
  
  // 训练异常检测模型
  let training_result = @azimuth.train_detector(detector, normal_data)
  assert_true(training_result.success)
  assert_true(training_result.model_accuracy > 0.9)
  
  // 创建包含异常的测试数据
  let test_data = [
    @azimuth.TelemetryPoint {
      metric_name: "cpu_usage",
      value: 48.0, // 正常值
      timestamp: @azimuth.current_timestamp() - 240,
      tags: [("host", "server1")]
    },
    @azimuth.TelemetryPoint {
      metric_name: "cpu_usage",
      value: 52.0, // 正常值
      timestamp: @azimuth.current_timestamp() - 180,
      tags: [("host", "server1")]
    },
    @azimuth.TelemetryPoint {
      metric_name: "cpu_usage",
      value: 95.0, // 异常值
      timestamp: @azimuth.current_timestamp() - 120,
      tags: [("host", "server1")]
    },
    @azimuth.TelemetryPoint {
      metric_name: "cpu_usage",
      value: 15.0, // 异常值
      timestamp: @azimuth.current_timestamp() - 60,
      tags: [("host", "server1")]
    },
    @azimuth.TelemetryPoint {
      metric_name: "cpu_usage",
      value: 51.0, // 正常值
      timestamp: @azimuth.current_timestamp(),
      tags: [("host", "server1")]
    }
  ]
  
  // 测试异常检测
  let detection_result = @azimuth.detect_anomalies_batch(detector, test_data)
  assert_eq(detection_result.total_points, 5)
  assert_eq(detection_result.anomaly_count, 2)
  
  // 验证检测到的异常
  let anomalies = detection_result.anomalies
  assert_eq(anomalies.length(), 2)
  assert_eq(anomalies[0].value, 95.0)
  assert_eq(anomalies[0].anomaly_score, 1.0)
  assert_eq(anomalies[1].value, 15.0)
  assert_eq(anomalies[1].anomaly_score, 1.0)
  
  // 测试异常严重程度分级
  let severity_levels = @azimuth.classify_anomaly_severity(anomalies)
  assert_true(severity_levels.any(fn(s) { s.severity == "critical" }))
  
  // 测试异常通知生成
  let notifications = @azimuth.generate_anomaly_notifications(anomalies)
  assert_eq(notifications.length(), 2)
  assert_true(notifications.all(fn(n) { n.message.contains("cpu_usage") }))
}

// Test 6: 遥测配置管理测试
test "telemetry configuration management operations" {
  // 创建配置管理器
  let config_manager = @azimuth.ConfigManager.new({
    config_source: "file",
    config_path: "/etc/azimuth/telemetry.json",
    auto_reload: true,
    reload_interval: 60000, // 1分钟
    validation_enabled: true
  })
  
  // 创建测试配置
  let test_config = @azimuth.TelemetryConfig {
    service_name: "azimuth-enhanced-service",
    service_version: "2.0.0",
    enabled: true,
    sampling_rate: 0.1,
    batch_size: 200,
    export_interval_ms: 10000,
    headers: [
      ("authorization", "Bearer token123"),
      ("x-api-key", "api-key-456")
    ],
    custom_attributes: [
      ("environment", "production"),
      ("data_center", "us-west-2"),
      ("cluster", "main")
    ],
    advanced_settings: {
      enable_compression: true,
      enable_encryption: true,
      retry_policy: {
        max_retries: 3,
        backoff_factor: 2.0,
        max_backoff: 30000
      }
    }
  }
  
  // 测试配置保存
  let save_result = @azimuth.save_config(config_manager, test_config)
  assert_true(save_result.success)
  assert_true(save_result.config_path.contains("telemetry.json"))
  
  // 测试配置加载
  let load_result = @azimuth.load_config(config_manager)
  assert_true(load_result.success)
  
  let loaded_config = load_result.config
  assert_eq(loaded_config.service_name, "azimuth-enhanced-service")
  assert_eq(loaded_config.service_version, "2.0.0")
  assert_true(loaded_config.enabled)
  assert_eq(loaded_config.sampling_rate, 0.1)
  assert_eq(loaded_config.batch_size, 200)
  assert_eq(loaded_config.export_interval_ms, 10000)
  
  // 测试配置验证
  let validation_result = @azimuth.validate_config(loaded_config)
  assert_true(validation_result.is_valid)
  assert_eq(validation_result.errors.length(), 0)
  
  // 测试配置更新
  let updated_config = @azimuth.update_config_value(loaded_config, "sampling_rate", 0.2)
  assert_eq(updated_config.sampling_rate, 0.2)
  
  // 测试配置合并
  let override_config = @azimuth.TelemetryConfig {
    service_name: "azimuth-override-service",
    sampling_rate: 0.5,
    custom_attributes: [
      ("environment", "staging"),
      ("feature_flag", "new_algorithm")
    ],
    // 其他字段使用默认值
    service_version: "1.0.0",
    enabled: true,
    batch_size: 100,
    export_interval_ms: 5000,
    headers: [],
    advanced_settings: {
      enable_compression: false,
      enable_encryption: false,
      retry_policy: {
        max_retries: 1,
        backoff_factor: 1.0,
        max_backoff: 10000
      }
    }
  }
  
  let merged_config = @azimuth.merge_configs(loaded_config, override_config)
  assert_eq(merged_config.service_name, "azimuth-override-service") // 被覆盖
  assert_eq(merged_config.service_version, "2.0.0") // 保留原值
  assert_eq(merged_config.sampling_rate, 0.5) // 被覆盖
  assert_eq(merged_config.batch_size, 200) // 保留原值
  
  // 测试配置环境变量替换
  let env_config = @azimuth.TelemetryConfig {
    service_name: "${SERVICE_NAME:azimuth-default}",
    service_version: "${SERVICE_VERSION:1.0.0}",
    enabled: true,
    sampling_rate: 0.1,
    batch_size: 100,
    export_interval_ms: 5000,
    headers: [
      ("authorization", "${AUTH_TOKEN:default-token}")
    ],
    custom_attributes: [
      ("environment", "${ENVIRONMENT:development}")
    ],
    advanced_settings: {
      enable_compression: true,
      enable_encryption: true,
      retry_policy: {
        max_retries: 3,
        backoff_factor: 2.0,
        max_backoff: 30000
      }
    }
  }
  
  let resolved_config = @azimuth.resolve_config_variables(env_config)
  assert_true(resolved_config.service_name.contains("azimuth"))
  assert_true(resolved_config.service_version.contains("1.0.0"))
  assert_true(resolved_config.headers[0].1.contains("token"))
  assert_true(resolved_config.custom_attributes[0].1.contains("environment"))
}

// Test 7: 遥测性能优化测试
test "telemetry performance optimization operations" {
  // 创建性能优化器
  let optimizer = @azimuth.PerformanceOptimizer.new({
    enable_caching: true,
    cache_size: 1000,
    cache_ttl: 300000, // 5分钟
    enable_batching: true,
    batch_size: 100,
    batch_timeout: 5000, // 5秒
    enable_compression: true,
    compression_level: 6
  })
  
  // 创建大量测试数据
  let large_dataset = []
  for i = 0; i < 10000; i = i + 1 {
    large_dataset = large_dataset.push(@azimuth.TelemetryPoint {
      metric_name: "performance_metric_" + (i % 10).to_string(),
      value: @azimuth.random_uniform(0.0, 100.0),
      timestamp: @azimuth.current_timestamp() - i,
      tags: [
        ("host", "server-" + (i % 5).to_string()),
        ("region", ["us-east", "us-west", "eu-west", "ap-southeast"][i % 4])
      ]
    })
  }
  
  // 测试批处理性能
  let batch_start_time = @azimuth.current_timestamp()
  let batch_results = @azimuth.process_in_batches(optimizer, large_dataset)
  let batch_end_time = @azimuth.current_timestamp()
  let batch_duration = batch_end_time - batch_start_time
  
  assert_true(batch_duration < 10000) // 应在10秒内完成
  assert_eq(batch_results.processed_count, 10000)
  assert_true(batch_results.average_batch_processing_time < 1000) // 每批次处理时间应小于1秒
  
  // 测试缓存性能
  let cache_start_time = @azimuth.current_timestamp()
  
  // 第一次处理（无缓存）
  let first_processing = @azimuth.process_with_cache(optimizer, large_dataset.slice(0, 1000))
  
  // 第二次处理相同数据（使用缓存）
  let cached_processing = @azimuth.process_with_cache(optimizer, large_dataset.slice(0, 1000))
  
  let cache_end_time = @azimuth.current_timestamp()
  let cache_duration = cache_end_time - cache_start_time
  
  assert_true(cached_processing.processing_time < first_processing.processing_time)
  assert_true(cached_processing.cache_hit_rate > 0.8)
  
  // 测试压缩性能
  let compression_start_time = @azimuth.current_timestamp()
  let compression_result = @azimuth.compress_data(optimizer, large_dataset)
  let compression_end_time = @azimuth.current_timestamp()
  let compression_duration = compression_end_time - compression_start_time
  
  assert_true(compression_result.compression_ratio > 0.5) // 压缩率应大于50%
  assert_true(compression_duration < 5000) // 压缩应在5秒内完成
  
  // 测试解压缩性能
  let decompression_start_time = @azimuth.current_timestamp()
  let decompression_result = @azimuth.decompress_data(optimizer, compression_result.compressed_data)
  let decompression_end_time = @azimuth.current_timestamp()
  let decompression_duration = decompression_end_time - decompression_start_time
  
  assert_eq(decompression_result.decompressed_count, 10000)
  assert_true(decompression_duration < 5000) // 解压缩应在5秒内完成
  
  // 测试内存使用优化
  let memory_usage_before = @azimuth.get_memory_usage()
  let optimized_processing = @azimuth.optimize_memory_usage(optimizer, large_dataset)
  let memory_usage_after = @azimuth.get_memory_usage()
  
  assert_true(optimized_processing.memory_efficient)
  assert_true(memory_usage_after.peak_usage < memory_usage_before.peak_usage * 1.2) // 内存增长不应超过20%
  
  // 测试并发处理性能
  let concurrent_start_time = @azimuth.current_timestamp()
  let concurrent_results = @azimuth.process_concurrently(optimizer, large_dataset, 4) // 4个并发工作线程
  let concurrent_end_time = @azimuth.current_timestamp()
  let concurrent_duration = concurrent_end_time - concurrent_start_time
  
  assert_true(concurrent_duration < batch_duration * 0.5) // 并发处理应至少快50%
  assert_eq(concurrent_results.processed_count, 10000)
  assert_true(concurrent_results.worker_count == 4)
}

// Test 8: 遥测数据导出测试
test "telemetry data export operations" {
  // 创建数据导出器
  let exporter = @azimuth.DataExporter.new({
    export_format: "json",
    compression_enabled: true,
    encryption_enabled: false,
    export_destinations: [
      @azimuth.ExportDestination {
        type: "file",
        path: "/tmp/telemetry_export.json",
        rotation_policy: {
          max_file_size: 10485760, // 10MB
          max_files: 5
        }
      },
      @azimuth.ExportDestination {
        type: "http",
        endpoint: "https://telemetry.example.com/api/v1/data",
        headers: [
          ("authorization", "Bearer export-token"),
          ("content-type", "application/json")
        ],
        retry_policy: {
          max_retries: 3,
          backoff_factor: 2.0,
          max_backoff: 30000
        }
      }
    ]
  })
  
  // 创建测试数据
  let export_data = [
    @azimuth.TelemetryRecord {
      id: "record-1",
      trace_id: "trace-1234567890abcdef",
      span_id: "span-1234567890abcdef",
      parent_span_id: Some("span-fedcba0987654321"),
      operation_name: "http_request",
      start_time: @azimuth.current_timestamp() - 1000,
      end_time: @azimuth.current_timestamp(),
      status: @azimuth.SpanStatus.OK,
      attributes: [
        ("http.method", @azimuth.StringValue("GET")),
        ("http.url", @azimuth.StringValue("/api/users")),
        ("http.status_code", @azimuth.IntValue(200))
      ],
      events: [
        @azimuth.SpanEvent {
          name: "database_query",
          timestamp: @azimuth.current_timestamp() - 500,
          attributes: [
            ("db.statement", @azimuth.StringValue("SELECT * FROM users")),
            ("db.duration", @azimuth.IntValue(200))
          ]
        }
      ]
    },
    @azimuth.TelemetryRecord {
      id: "record-2",
      trace_id: "trace-1234567890abcdef",
      span_id: "span-fedcba0987654321",
      parent_span_id: None,
      operation_name: "database_operation",
      start_time: @azimuth.current_timestamp() - 800,
      end_time: @azimuth.current_timestamp() - 300,
      status: @azimuth.SpanStatus.OK,
      attributes: [
        ("db.system", @azimuth.StringValue("postgresql")),
        ("db.name", @azimuth.StringValue("azimuth_db")),
        ("db.operation", @azimuth.StringValue("SELECT"))
      ],
      events: []
    }
  ]
  
  // 测试数据序列化
  let serialization_result = @azimuth.serialize_telemetry_data(exporter, export_data, "json")
  assert_true(serialization_result.success)
  assert_true(serialization_result.serialized_data.length() > 0)
  assert_true(serialization_result.serialized_data.contains("trace_id"))
  assert_true(serialization_result.serialized_data.contains("span_id"))
  assert_true(serialization_result.serialized_data.contains("operation_name"))
  
  // 测试文件导出
  let file_export_result = @azimuth.export_to_file(exporter, export_data, 0)
  assert_true(file_export_result.success)
  assert_true(file_export_result.file_path.contains("telemetry_export.json"))
  assert_true(file_export_result.bytes_written > 0)
  
  // 测试HTTP导出（模拟）
  let http_export_result = @azimuth.export_to_http(exporter, export_data, 1)
  assert_true(http_export_result.success)
  assert_eq(http_export_result.status_code, 200)
  assert_true(http_export_result.response.contains("success"))
  
  // 测试批量导出
  let batch_export_data = []
  for i = 0; i < 100; i = i + 1 {
    batch_export_data = batch_export_data.push(@azimuth.TelemetryRecord {
      id: "batch-record-" + i.to_string(),
      trace_id: "batch-trace-" + (i % 10).to_string(),
      span_id: "batch-span-" + i.to_string(),
      parent_span_id: if i > 0 { Some("batch-span-" + (i - 1).to_string()) } else { None },
      operation_name: "batch_operation_" + (i % 5).to_string(),
      start_time: @azimuth.current_timestamp() - (100 - i) * 100,
      end_time: @azimuth.current_timestamp() - (99 - i) * 100,
      status: if i % 10 == 0 { @azimuth.SpanStatus.ERROR } else { @azimuth.SpanStatus.OK },
      attributes: [
        ("batch.index", @azimuth.IntValue(i)),
        ("batch.size", @azimuth.IntValue(100))
      ],
      events: []
    })
  }
  
  let batch_export_result = @azimuth.export_batch_data(exporter, batch_export_data)
  assert_true(batch_export_result.success)
  assert_eq(batch_export_result.exported_count, 100)
  assert_true(batch_export_result.total_duration < 10000) // 应在10秒内完成
  
  // 测试导出统计
  let export_stats = @azimuth.get_export_statistics(exporter)
  assert_eq(export_stats.total_exports, 4) // 2个单独导出 + 1个批量导出 + 1个序列化
  assert_eq(export_stats.total_records_exported, 104) // 2 + 100
  assert_true(export_stats.average_export_time > 0)
  assert_true(export_stats.success_rate > 0.9)
  
  // 测试导出格式转换
  let format_conversion_result = @azimuth.convert_export_format(
    serialization_result.serialized_data,
    "json",
    "protobuf"
  )
  assert_true(format_conversion_result.success)
  assert_true(format_conversion_result.converted_data.length() > 0)
  assert_ne(format_conversion_result.converted_data, serialization_result.serialized_data)
}