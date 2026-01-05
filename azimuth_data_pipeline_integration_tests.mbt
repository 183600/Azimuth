// Azimuth Data Pipeline Integration Test Suite
// 数据管道集成测试套件 - 测试遥测数据在管道中的处理和转换

// Test 1: 数据收集和缓冲
test "data collection and buffering operations" {
  // 创建数据收集器
  let collector = @azimuth.Collector::new(
    @azimuth.CollectorConfig {
      max_batch_size: 100,
      max_buffer_size: 1000,
      flush_interval_ms: 5000,
      retry_attempts: 3
    }
  )
  
  // 创建遥测数据点
  let telemetry_data_points = [
    @azimuth.TelemetryDataPoint {
      trace_id: "trace-001",
      span_id: "span-001",
      timestamp: 1640995200000,
      metric_name: "request.duration",
      metric_value: 125.5,
      attributes: [("http.method", @azimuth.StringValue("GET"))]
    },
    @azimuth.TelemetryDataPoint {
      trace_id: "trace-002",
      span_id: "span-002",
      timestamp: 1640995201000,
      metric_name: "request.duration",
      metric_value: 250.0,
      attributes: [("http.method", @azimuth.StringValue("POST"))]
    },
    @azimuth.TelemetryDataPoint {
      trace_id: "trace-003",
      span_id: "span-003",
      timestamp: 1640995202000,
      metric_name: "error.count",
      metric_value: 1.0,
      attributes: [("error.type", @azimuth.StringValue("timeout"))]
    }
  ]
  
  // 添加数据点到收集器
  for data_point in telemetry_data_points {
    @azimuth.Collector::add_data_point(collector, data_point)
  }
  
  // 验证数据点已添加
  assert_eq(@azimuth.Collector::buffer_size(collector), 3)
  
  // 获取缓冲的数据
  let buffered_data = @azimuth.Collector::get_buffered_data(collector)
  assert_eq(buffered_data.length(), 3)
  
  // 验证数据点内容
  let first_point = buffered_data[0]
  assert_eq(first_point.trace_id, "trace-001")
  assert_eq(first_point.metric_name, "request.duration")
  assert_eq(first_point.metric_value, 125.5)
  
  // 测试批量处理
  let batch = @azimuth.Collector::create_batch(collector, 2)
  assert_eq(batch.length(), 2)
  
  // 验证批次数据
  let batch_first = batch[0]
  let batch_second = batch[1]
  assert_eq(batch_first.trace_id, "trace-001")
  assert_eq(batch_second.trace_id, "trace-002")
}

// Test 2: 数据转换和格式化
test "data transformation and formatting" {
  // 创建原始遥测数据
  let raw_telemetry = @azimuth.RawTelemetryData {
    trace_id: "raw-trace-123",
    span_id: "raw-span-456",
    parent_span_id: Some("raw-span-789"),
    operation_name: "database.query",
    start_time_ns: 1640995200000000000L,
    end_time_ns: 1640995200500000000L,
    status_code: 1,
    status_message: "OK",
    attributes: [
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = $1"),
      ("db.user", "app_user"),
      ("peer.service", "database")
    ],
    events: [
      @azimuth.TelemetryEvent {
        name: "db.query.start",
        timestamp_ns: 1640995200000000000L,
        attributes: [("db.operation", "SELECT")]
      },
      @azimuth.TelemetryEvent {
        name: "db.query.complete",
        timestamp_ns: 1640995200500000000L,
        attributes: [("db.rows_affected", "1")]
      }
    ]
  }
  
  // 创建数据转换器
  let transformer = @azimuth.DataTransformer::new(
    @azimuth.TransformationConfig {
      target_format: "otlp",
      include_resource_attributes: true,
      attribute_mappings: [
        ("db.system", "database.system"),
        ("db.statement", "database.statement"),
        ("db.user", "database.user")
      ],
      time_format: "unix_nanos"
    }
  )
  
  // 执行数据转换
  let transformed_data = @azimuth.DataTransformer::transform(transformer, raw_telemetry)
  
  // 验证转换结果
  assert_eq(transformed_data.trace_id, "raw-trace-123")
  assert_eq(transformed_data.span_id, "raw-span-456")
  assert_eq(transformed_data.operation_name, "database.query")
  
  // 验证属性映射
  let transformed_attributes = transformed_data.attributes
  match @azimuth.Attributes::get(transformed_attributes, "database.system") {
    Some(@azimuth.StringValue(db_system)) => assert_eq(db_system, "postgresql")
    _ => assert_true(false)
  }
  
  match @azimuth.Attributes::get(transformed_attributes, "database.statement") {
    Some(@azimuth.StringValue(statement)) => assert_eq(statement, "SELECT * FROM users WHERE id = $1")
    _ => assert_true(false)
  }
  
  // 验证持续时间计算
  let duration_ns = transformed_data.end_time_ns - transformed_data.start_time_ns
  assert_eq(duration_ns, 500000000L) // 500ms
  
  // 验证事件转换
  assert_eq(transformed_data.events.length(), 2)
  let first_event = transformed_data.events[0]
  assert_eq(first_event.name, "db.query.start")
  assert_eq(first_event.timestamp_ns, 1640995200000000000L)
}

// Test 3: 数据聚合和统计
test "data aggregation and statistics" {
  // 创建时间序列数据点
  let time_series_data = [
    (1640995200000L, 100.5),   // 10:00:00
    (1640995260000L, 150.2),   // 10:01:00
    (1640995320000L, 125.8),   // 10:02:00
    (1640995380000L, 200.1),   // 10:03:00
    (1640995440000L, 175.3),   // 10:04:00
    (1640995500000L, 180.7),   // 10:05:00
    (1640995560000L, 165.4),   // 10:06:00
    (1640995620000L, 190.9),   // 10:07:00
    (1640995680000L, 155.6),   // 10:08:00
    (1640995740000L, 170.2)    // 10:09:00
  ]
  
  // 创建数据聚合器
  let aggregator = @azimuth.DataAggregator::new(
    @azimuth.AggregationConfig {
      aggregation_window_ms: 300000,  // 5分钟窗口
      aggregation_functions: ["avg", "min", "max", "sum", "count"],
      group_by_attributes: ["service.name", "operation.name"]
    }
  )
  
  // 添加时间序列数据到聚合器
  for (timestamp, value) in time_series_data {
    let data_point = @azimuth.TimeSeriesDataPoint {
      timestamp,
      metric_name: "response.time",
      value,
      attributes: [
        ("service.name", @azimuth.StringValue("api-service")),
        ("operation.name", @azimuth.StringValue("user.profile"))
      ]
    }
    @azimuth.DataAggregator::add_data_point(aggregator, data_point)
  }
  
  // 执行数据聚合
  let aggregation_results = @azimuth.DataAggregator::aggregate(aggregator)
  
  // 验证聚合结果
  assert_eq(aggregation_results.length(), 2) // 两个5分钟窗口
  
  // 验证第一个窗口的聚合结果 (10:00-10:05)
  let first_window = aggregation_results[0]
  assert_eq(first_window.window_start_ms, 1640995200000L)
  assert_eq(first_window.window_end_ms, 1640995500000L)
  
  // 验证聚合函数结果
  match @azimuth.AggregationResult::get_function_value(first_window, "avg") {
    Some(@azimuth.FloatValue(avg)) => {
      let expected_avg = (100.5 + 150.2 + 125.8 + 200.1 + 175.3) / 5.0
      assert_eq(avg, expected_avg)
    }
    _ => assert_true(false)
  }
  
  match @azimuth.AggregationResult::get_function_value(first_window, "min") {
    Some(@azimuth.FloatValue(min)) => assert_eq(min, 100.5)
    _ => assert_true(false)
  }
  
  match @azimuth.AggregationResult::get_function_value(first_window, "max") {
    Some(@azimuth.FloatValue(max)) => assert_eq(max, 200.1)
    _ => assert_true(false)
  }
  
  match @azimuth.AggregationResult::get_function_value(first_window, "sum") {
    Some(@azimuth.FloatValue(sum)) => assert_eq(sum, 751.9)
    _ => assert_true(false)
  }
  
  match @azimuth.AggregationResult::get_function_value(first_window, "count") {
    Some(@azimuth.IntValue(count)) => assert_eq(count, 5)
    _ => assert_true(false)
  }
}

// Test 4: 数据过滤和路由
test "data filtering and routing" {
  // 创建多样化的遥测数据
  let mixed_telemetry_data = [
    @azimuth.TelemetryData {
      trace_id: "trace-001",
      span_id: "span-001",
      operation_name: "user.login",
      service_name: "auth-service",
      duration_ms: 120,
      status: "success",
      attributes: [
        ("http.method", @azimuth.StringValue("POST")),
        ("http.status_code", @azimuth.IntValue(200)),
        ("user.id", @azimuth.StringValue("user-123"))
      ]
    },
    @azimuth.TelemetryData {
      trace_id: "trace-002",
      span_id: "span-002",
      operation_name: "user.profile",
      service_name: "user-service",
      duration_ms: 85,
      status: "success",
      attributes: [
        ("http.method", @azimuth.StringValue("GET")),
        ("http.status_code", @azimuth.IntValue(200)),
        ("user.id", @azimuth.StringValue("user-123"))
      ]
    },
    @azimuth.TelemetryData {
      trace_id: "trace-003",
      span_id: "span-003",
      operation_name: "payment.process",
      service_name: "payment-service",
      duration_ms: 1500,
      status: "error",
      attributes: [
        ("http.method", @azimuth.StringValue("POST")),
        ("http.status_code", @azimuth.IntValue(500)),
        ("error.type", @azimuth.StringValue("timeout"))
      ]
    },
    @azimuth.TelemetryData {
      trace_id: "trace-004",
      span_id: "span-004",
      operation_name: "database.query",
      service_name: "user-service",
      duration_ms: 45,
      status: "success",
      attributes: [
        ("db.system", @azimuth.StringValue("postgresql")),
        ("db.operation", @azimuth.StringValue("SELECT"))
      ]
    }
  ]
  
  // 创建数据过滤器
  let success_filter = @azimuth.DataFilter::new(
    fn(data: @azimuth.TelemetryData) {
      data.status == "success"
    }
  )
  
  let error_filter = @azimuth.DataFilter::new(
    fn(data: @azimuth.TelemetryData) {
      data.status == "error"
    }
  )
  
  let high_latency_filter = @azimuth.DataFilter::new(
    fn(data: @azimuth.TelemetryData) {
      data.duration_ms > 1000
    }
  )
  
  let auth_service_filter = @azimuth.DataFilter::new(
    fn(data: @azimuth.TelemetryData) {
      data.service_name == "auth-service"
    }
  )
  
  // 应用过滤器
  let success_data = @azimuth.DataFilter::apply(success_filter, mixed_telemetry_data)
  let error_data = @azimuth.DataFilter::apply(error_filter, mixed_telemetry_data)
  let high_latency_data = @azimuth.DataFilter::apply(high_latency_filter, mixed_telemetry_data)
  let auth_service_data = @azimuth.DataFilter::apply(auth_service_filter, mixed_telemetry_data)
  
  // 验证过滤结果
  assert_eq(success_data.length(), 3)
  assert_eq(error_data.length(), 1)
  assert_eq(high_latency_data.length(), 1)
  assert_eq(auth_service_data.length(), 1)
  
  // 验证成功数据内容
  let success_operations = success_data.map(fn(data) { data.operation_name })
  assert_true(success_operations.contains("user.login"))
  assert_true(success_operations.contains("user.profile"))
  assert_true(success_operations.contains("database.query"))
  assert_false(success_operations.contains("payment.process"))
  
  // 验证错误数据
  assert_eq(error_data[0].operation_name, "payment.process")
  assert_eq(error_data[0].status, "error")
  
  // 验证高延迟数据
  assert_eq(high_latency_data[0].operation_name, "payment.process")
  assert_true(high_latency_data[0].duration_ms > 1000)
  
  // 验证认证服务数据
  assert_eq(auth_service_data[0].service_name, "auth-service")
  assert_eq(auth_service_data[0].operation_name, "user.login")
  
  // 创建数据路由器
  let router = @azimuth.DataRouter::new()
  
  // 添加路由规则
  @azimuth.DataRouter::add_route(router, "success-store", success_filter)
  @azimuth.DataRouter::add_route(router, "error-alerts", error_filter)
  @azimuth.DataRouter::add_route(router, "performance-monitoring", high_latency_filter)
  
  // 路由数据
  let routing_results = @azimuth.DataRouter::route(router, mixed_telemetry_data)
  
  // 验证路由结果
  assert_eq(routing_results.get("success-store").length(), 3)
  assert_eq(routing_results.get("error-alerts").length(), 1)
  assert_eq(routing_results.get("performance-monitoring").length(), 1)
}

// Test 5: 数据持久化和存储
test "data persistence and storage operations" {
  // 创建存储配置
  let storage_config = @azimuth.StorageConfig {
    storage_type: "file",
    connection_string: "/tmp/telemetry-data",
    batch_size: 100,
    compression_enabled: true,
    encryption_enabled: false,
    retention_days: 30
  }
  
  // 创建存储适配器
  let storage = @azimuth.StorageAdapter::new(storage_config)
  
  // 创建测试数据
  let test_data = [
    @azimuth.StoredTelemetryData {
      id: "data-001",
      trace_id: "trace-001",
      span_id: "span-001",
      timestamp: 1640995200000,
      data_type: "span",
      serialized_data: "{\"operation_name\": \"test.operation\", \"duration_ms\": 100}",
      metadata: [
        ("source.service", @azimuth.StringValue("test-service")),
        ("environment", @azimuth.StringValue("test"))
      ]
    },
    @azimuth.StoredTelemetryData {
      id: "data-002",
      trace_id: "trace-002",
      span_id: "span-002",
      timestamp: 1640995260000,
      data_type: "metric",
      serialized_data: "{\"metric_name\": \"test.metric\", \"value\": 42.5}",
      metadata: [
        ("source.service", @azimuth.StringValue("test-service")),
        ("environment", @azimuth.StringValue("test"))
      ]
    }
  ]
  
  // 存储数据
  for data in test_data {
    let store_result = @azimuth.StorageAdapter::store(storage, data)
    assert_true(store_result.success)
    assert_eq(store_result.id, data.id)
  }
  
  // 查询存储的数据
  let query = @azimuth.StorageQuery {
    filters: [
      ("environment", @azimuth.StringValue("test"))
    ],
    time_range: Some((
      1640995000000,
      1640995300000
    )),
    limit: 100,
    offset: 0
  }
  
  let query_result = @azimuth.StorageAdapter::query(storage, query)
  assert_true(query_result.success)
  assert_eq(query_result.data.length(), 2)
  
  // 验证查询结果
  let first_stored = query_result.data[0]
  assert_eq(first_stored.id, "data-001")
  assert_eq(first_stored.trace_id, "trace-001")
  assert_eq(first_stored.data_type, "span")
  
  // 按ID查询单个数据
  let single_query = @azimuth.StorageAdapter::get_by_id(storage, "data-002")
  assert_true(single_query.success)
  assert_eq(single_query.data.id, "data-002")
  assert_eq(single_query.data.data_type, "metric")
  
  // 测试数据更新
  let updated_data = { test_data[0] | 
    serialized_data: "{\"operation_name\": \"updated.operation\", \"duration_ms\": 150}",
    metadata: test_data[0].metadata.push(("updated", @azimuth.StringValue("true")))
  }
  
  let update_result = @azimuth.StorageAdapter::update(storage, updated_data)
  assert_true(update_result.success)
  
  // 验证更新结果
  let updated_query = @azimuth.StorageAdapter::get_by_id(storage, "data-001")
  assert_true(updated_query.success)
  assert_true(updated_query.data.serialized_data.contains("updated.operation"))
  
  // 测试数据删除
  let delete_result = @azimuth.StorageAdapter::delete(storage, "data-001")
  assert_true(delete_result.success)
  
  // 验证删除结果
  let deleted_query = @azimuth.StorageAdapter::get_by_id(storage, "data-001")
  assert_false(deleted_query.success)
  
  // 验证剩余数据
  let final_query = @azimuth.StorageAdapter::query(storage, query)
  assert_true(final_query.success)
  assert_eq(final_query.data.length(), 1)
  assert_eq(final_query.data[0].id, "data-002")
}

// Test 6: 数据管道性能测试
test "data pipeline performance testing" {
  // 创建大量测试数据
  let large_dataset = []
  for i = 0; i < 1000; i = i + 1 {
    large_dataset = large_dataset.push(@azimuth.TelemetryData {
      trace_id: "trace-" + i.to_string(),
      span_id: "span-" + i.to_string(),
      operation_name: "operation." + (i % 10).to_string(),
      service_name: "service." + (i % 5).to_string(),
      duration_ms: 50 + (i % 500),
      status: if i % 20 == 0 { "error" } else { "success" },
      attributes: [
        ("http.method", @azimuth.StringValue(if i % 2 == 0 { "GET" } else { "POST" })),
        ("instance.id", @azimuth.StringValue("instance-" + (i % 10).to_string()))
      ]
    })
  }
  
  // 创建高性能数据管道
  let pipeline = @azimuth.DataPipeline::new(
    @azimuth.PipelineConfig {
      batch_size: 100,
      worker_threads: 4,
      buffer_size: 1000,
      enable_compression: true,
      enable_metrics: true
    }
  )
  
  // 添加处理阶段
  @azimuth.DataPipeline::add_stage(pipeline, @azimuth.ValidationStage::new())
  @azimuth.DataPipeline::add_stage(pipeline, @azimuth.TransformationStage::new())
  @azimuth.DataPipeline::add_stage(pipeline, @azimuth.AggregationStage::new())
  @azimuth.DataPipeline::add_stage(pipeline, @azimuth.FilteringStage::new())
  
  // 测量处理时间
  let start_time = @azimuth.current_timestamp()
  
  // 处理数据
  let processing_result = @azimuth.DataPipeline::process(pipeline, large_dataset)
  
  let end_time = @azimuth.current_timestamp()
  let processing_duration = end_time - start_time
  
  // 验证处理结果
  assert_true(processing_result.success)
  assert_eq(processing_result.processed_count, 1000)
  assert_eq(processing_result.error_count, 0)
  
  // 验证性能要求
  assert_true(processing_duration < 5000) // 应在5秒内完成
  assert_true(processing_result.throughput_per_second > 200) // 至少200条/秒
  
  // 验证管道指标
  let pipeline_metrics = @azimuth.DataPipeline::get_metrics(pipeline)
  assert_true(pipeline_metrics.total_processed >= 1000)
  assert_true(pipeline_metrics.average_processing_time_ms > 0)
  assert_true(pipeline_metrics.memory_usage_mb < 100) // 内存使用应小于100MB
  
  // 测试批量处理性能
  let batch_start_time = @azimuth.current_timestamp()
  
  let batches = []
  for i = 0; i < 10; i = i + 1 {
    let batch_start = i * 100
    let batch_end = (i + 1) * 100
    let batch = large_dataset.slice(batch_start, batch_end)
    batches = batches.push(batch)
  }
  
  let batch_results = batches.map(fn(batch) {
    @azimuth.DataPipeline::process(pipeline, batch)
  })
  
  let batch_end_time = @azimuth.current_timestamp()
  let batch_processing_duration = batch_end_time - batch_start_time
  
  // 验证批量处理结果
  assert_true(batch_results.all(fn(result) { result.success }))
  assert_eq(batch_results.reduce(fn(acc, result) { acc + result.processed_count }, 0), 1000)
  
  // 验证批量处理性能
  assert_true(batch_processing_duration < processing_duration * 1.2) // 批量处理不应显著慢于单个处理
}