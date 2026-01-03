// Azimuth 实时流处理测试用例
// 测试实时数据流的处理、转换、聚合和分析功能

test "实时流数据源创建和管理" {
  // 创建流处理器
  let stream_processor = azimuth::StreamProcessor::new("telemetry-processor")
  
  // 创建数据源配置
  let source_config = azimuth::StreamSourceConfig {
    source_type: azimuth::SourceType::Kafka,
    name: "telemetry-events",
    topic: "azimuth.telemetry.events",
    bootstrap_servers: ["kafka-01:9092", "kafka-02:9092"],
    consumer_group: "telemetry-processor-group",
    batch_size: 100,
    batch_timeout_ms: 1000,
    auto_offset_reset: azimuth::OffsetReset::Latest,
    key_deserializer: azimuth::Deserializer::String,
    value_deserializer: azimuth::Deserializer::Json,
    properties: [
      ("enable.auto.commit", "false"),
      ("max.poll.records", "500")
    ]
  }
  
  // 添加数据源
  let source_id = stream_processor.add_source(source_config)
  assert_true(source_id.length() > 0)
  
  // 验证数据源状态
  let source_status = stream_processor.get_source_status(source_id)
  assert_eq(source_status.state, azimuth::SourceState::Connected)
  assert_eq(source_status.name, "telemetry-events")
  assert_eq(source_status.current_offset, 0)
  assert_eq(source_status.lag, 0)
  
  // 创建第二个数据源
  let metrics_config = azimuth::StreamSourceConfig {
    source_type: azimuth::SourceType::Kafka,
    name: "metrics-data",
    topic: "azimuth.metrics.data",
    bootstrap_servers: ["kafka-01:9092"],
    consumer_group: "metrics-processor-group",
    batch_size: 200,
    batch_timeout_ms: 2000,
    auto_offset_reset: azimuth::OffsetReset::Earliest,
    key_deserializer: azimuth::Deserializer::String,
    value_deserializer: azimuth::Deserializer::Avro,
    properties: [
      ("schema.registry.url", "http://schema-registry:8081")
    ]
  }
  
  let metrics_source_id = stream_processor.add_source(metrics_config)
  assert_true(metrics_source_id.length() > 0)
  assert_ne(source_id, metrics_source_id)
  
  // 获取所有数据源
  let all_sources = stream_processor.get_all_sources()
  assert_eq(all_sources.length(), 2)
}

test "实时流数据处理管道" {
  // 创建流处理器
  let stream_processor = azimuth::StreamProcessor::new("data-pipeline")
  
  // 创建数据源
  let source_config = azimuth::StreamSourceConfig {
    source_type: azimuth::SourceType::Http,
    name: "webhook-events",
    endpoint: "/webhook/events",
    port: 8080,
    path: "/events",
    method: "POST",
    headers: [("Content-Type", "application/json")],
    batch_size: 50,
    batch_timeout_ms: 500,
    value_deserializer: azimuth::Deserializer::Json,
    properties: []
  }
  
  let source_id = stream_processor.add_source(source_config)
  
  // 定义处理管道
  let pipeline = azimuth::ProcessingPipeline::new("event-processing")
  
  // 步骤1：数据验证
  pipeline.add_step(azimuth::ProcessingStep::Validation {
    name: "schema-validation",
    schema: azimuth::Schema::JsonSchema({
      "type": "object",
      "required": ["event_id", "timestamp", "event_type", "data"],
      "properties": {
        "event_id": {"type": "string"},
        "timestamp": {"type": "integer"},
        "event_type": {"type": "string"},
        "data": {"type": "object"}
      }
    }),
    error_handling: azimuth::ErrorHandling::SkipInvalid
  })
  
  // 步骤2：数据转换
  pipeline.add_step(azimuth::ProcessingStep::Transformation {
    name: "timestamp-normalization",
    transformer: azimuth::Transformer::TimestampNormalizer,
    target_format: "iso8601",
    source_field: "timestamp"
  })
  
  // 步骤3：数据丰富
  pipeline.add_step(azimuth::ProcessingStep::Enrichment {
    name: "geo-enrichment",
    enricher: azimuth::Enricher::GeoIpEnricher,
    source_field: "client_ip",
    target_fields: ["country", "city", "latitude", "longitude"]
  })
  
  // 步骤4：数据过滤
  pipeline.add_step(azimuth::ProcessingStep::Filtering {
    name: "internal-traffic-filter",
    filter: azimuth::Filter::FieldNotEquals {
      field: "network.subnet",
      value: "10.0.0.0/8"
    }
  })
  
  // 步骤5：数据聚合
  pipeline.add_step(azimuth::ProcessingStep::Aggregation {
    name: "event-count-aggregation",
    aggregator: azimuth::Aggregator::CountByField {
      field: "event_type",
      window_size_ms: 60000, // 1分钟窗口
      output_field: "event_count"
    }
  })
  
  // 注册管道
  let pipeline_id = stream_processor.register_pipeline(pipeline)
  
  // 连接数据源到管道
  stream_processor.connect_source_to_pipeline(source_id, pipeline_id)
  
  // 验证管道状态
  let pipeline_status = stream_processor.get_pipeline_status(pipeline_id)
  assert_eq(pipeline_status.state, azimuth::PipelineState::Running)
  assert_eq(pipeline_status.steps_count, 5)
  assert_eq(pipeline_status.processed_records, 0)
  assert_eq(pipeline_status.error_count, 0)
}

test "实时流数据窗口操作" {
  // 创建流处理器
  let stream_processor = azimuth::StreamProcessor::new("window-processor")
  
  // 创建时间窗口操作
  let tumbling_window = azimuth::TimeWindow::Tumbling {
    size_ms: 5000, // 5秒窗口
    allowed_lateness_ms: 1000
  }
  
  let sliding_window = azimuth::TimeWindow::Sliding {
    size_ms: 10000, // 10秒窗口
    slide_ms: 5000,  // 每5秒滑动一次
    allowed_lateness_ms: 2000
  }
  
  let session_window = azimuth::SessionWindow {
    gap_ms: 30000, // 30秒会话超时
    max_session_time_ms: 300000 // 5分钟最大会话时间
  }
  
  // 创建窗口聚合器
  let count_aggregator = azimuth::WindowAggregator::Count {
    window: tumbling_window,
    key_field: "user_id"
  }
  
  let sum_aggregator = azimuth::WindowAggregator::Sum {
    window: sliding_window,
    key_field: "product_id",
    value_field: "price"
  }
  
  let avg_aggregator = azimuth::WindowAggregator::Average {
    window: session_window,
    key_field: "session_id",
    value_field: "response_time"
  }
  
  // 注册窗口操作
  let count_op_id = stream_processor.add_window_operation("user_activity_count", count_aggregator)
  let sum_op_id = stream_processor.add_window_operation("product_revenue_sum", sum_aggregator)
  let avg_op_id = stream_processor.add_window_operation("session_avg_response", avg_aggregator)
  
  // 模拟数据流
  let test_events = [
    ("user-001", "product-001", 100.0, 150, 1609459200000L),
    ("user-001", "product-002", 50.0, 200, 1609459201000L),
    ("user-002", "product-001", 100.0, 120, 1609459202000L),
    ("user-001", "product-003", 75.0, 180, 1609459203000L),
    ("user-002", "product-002", 50.0, 250, 1609459204000L)
  ]
  
  for (user_id, product_id, price, response_time, timestamp) in test_events {
    let event = azimuth::StreamEvent::new(timestamp)
    event.set_field("user_id", user_id)
    event.set_field("product_id", product_id)
    event.set_field("price", price)
    event.set_field("response_time", response_time)
    event.set_field("session_id", "session-" + user_id)
    
    stream_processor.process_event(event)
  }
  
  // 等待窗口处理完成
  azimuth::TimeUtil::sleep_ms(6000)
  
  // 验证窗口聚合结果
  let count_results = stream_processor.get_window_results(count_op_id)
  let sum_results = stream_processor.get_window_results(sum_op_id)
  let avg_results = stream_processor.get_window_results(avg_op_id)
  
  // 验证计数结果
  assert_true(count_results.length() > 0)
  let user_001_count = count_results.find(fn(result) { result.key == "user-001" })
  match user_001_count {
    Some(count) => assert_eq(count.value, 3),
    None => assert_true(false)
  }
  
  let user_002_count = count_results.find(fn(result) { result.key == "user-002" })
  match user_002_count {
    Some(count) => assert_eq(count.value, 2),
    None => assert_true(false)
  }
  
  // 验证求和结果
  assert_true(sum_results.length() > 0)
  let product_001_sum = sum_results.find(fn(result) { result.key == "product-001" })
  match product_001_sum {
    Some(sum) => assert_eq(sum.value, 200.0),
    None => assert_true(false)
  }
  
  // 验证平均值结果
  assert_true(avg_results.length() > 0)
  let session_user_001 = avg_results.find(fn(result) { result.key == "session-user-001" })
  match session_user_001 {
    Some(avg) => assert_eq(avg.value, 176.67), // (150 + 200 + 180) / 3
    None => assert_true(false)
  }
}

test "实时流数据模式检测" {
  // 创建流处理器
  let stream_processor = azimuth::StreamProcessor::new("pattern-detector")
  
  // 创建模式检测器
  let anomaly_detector = azimuth::PatternDetector::AnomalyDetection {
    algorithm: azimuth::AnomalyAlgorithm::ZScore,
    threshold: 2.0,
    window_size: 100,
    field: "response_time"
  }
  
  let trend_detector = azimuth::PatternDetector::TrendAnalysis {
    algorithm: azimuth::TrendAlgorithm::LinearRegression,
    window_size: 200,
    field: "request_count",
    min_slope: 0.1
  }
  
  let correlation_detector = azimuth::PatternDetector::CorrelationAnalysis {
    field1: "cpu_usage",
    field2: "response_time",
    window_size: 150,
    correlation_threshold: 0.7
  }
  
  let sequence_detector = azimuth::PatternDetector::SequencePattern {
    pattern: [
      ("event_type", "user_login"),
      ("event_type", "page_view"),
      ("event_type", "add_to_cart"),
      ("event_type", "checkout")
    ],
    max_time_gap_ms: 1800000, // 30分钟
    key_field: "user_id"
  }
  
  // 注册模式检测器
  let anomaly_id = stream_processor.add_pattern_detector("response_anomaly", anomaly_detector)
  let trend_id = stream_processor.add_pattern_detector("request_trend", trend_detector)
  let correlation_id = stream_processor.add_pattern_detector("cpu_response_correlation", correlation_detector)
  let sequence_id = stream_processor.add_pattern_detector("purchase_funnel", sequence_detector)
  
  // 模拟正常数据
  for i in 0..=99 {
    let event = azimuth::StreamEvent::new(1609459200000L + (i * 1000L))
    event.set_field("response_time", 100 + (i.to_int() % 50))
    event.set_field("request_count", 1000 + (i.to_int() % 200))
    event.set_field("cpu_usage", 30.0 + (i.to_int() % 40).to_float())
    stream_processor.process_event(event)
  }
  
  // 模拟异常数据
  let anomaly_event = azimuth::StreamEvent::new(1609459300000L)
  anomaly_event.set_field("response_time", 500) // 异常高的响应时间
  anomaly_event.set_field("request_count", 1200)
  anomaly_event.set_field("cpu_usage", 85.0)
  stream_processor.process_event(anomaly_event)
  
  // 模拟用户行为序列
  let user_events = [
    ("user-001", "user_login", 1609459200000L),
    ("user-001", "page_view", 1609459210000L),
    ("user-001", "add_to_cart", 1609459220000L),
    ("user-001", "checkout", 1609459230000L)
  ]
  
  for (user_id, event_type, timestamp) in user_events {
    let event = azimuth::StreamEvent::new(timestamp)
    event.set_field("user_id", user_id)
    event.set_field("event_type", event_type)
    stream_processor.process_event(event)
  }
  
  // 等待模式检测完成
  azimuth::TimeUtil::sleep_ms(2000)
  
  // 验证异常检测结果
  let anomaly_results = stream_processor.get_pattern_results(anomaly_id)
  assert_true(anomaly_results.length() > 0)
  
  let detected_anomaly = anomaly_results.find(fn(result) { 
    result.pattern_type == azimuth::PatternType::Anomaly 
  })
  match detected_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.severity, azimuth::Severity::High)
      assert_true(anomaly.confidence > 0.8)
    }
    None => assert_true(false)
  }
  
  // 验证序列模式检测结果
  let sequence_results = stream_processor.get_pattern_results(sequence_id)
  assert_true(sequence_results.length() > 0)
  
  let detected_sequence = sequence_results.find(fn(result) { 
    result.pattern_type == azimuth::PatternType::Sequence 
  })
  match detected_sequence {
    Some(sequence) => {
      assert_eq(sequence.key, "user-001")
      assert_eq(sequence.matched_events.length(), 4)
    }
    None => assert_true(false)
  }
}

test "实时流数据状态管理" {
  // 创建流处理器
  let stream_processor = azimuth::StreamProcessor::new("stateful-processor")
  
  // 创建状态存储
  let state_store = azimuth::StateStore::InMemory {
    name: "user-session-store",
    ttl_ms: 1800000, // 30分钟TTL
    max_entries: 10000
  }
  
  let state_store_id = stream_processor.add_state_store(state_store)
  
  // 创建状态ful操作
  let session_state = azimuth::StatefulOperation::UserSession {
    store_id: state_store_id,
    session_timeout_ms: 900000, // 15分钟会话超时
    key_field: "user_id",
    tracked_fields: ["last_activity", "page_views", "total_time_spent"]
  }
  
  let count_state = azimuth::StatefulOperation::RunningCount {
    store_id: state_store_id,
    key_field: "event_type",
    output_field: "event_count"
  }
  
  // 注册状态ful操作
  let session_op_id = stream_processor.add_stateful_operation("session_tracking", session_state)
  let count_op_id = stream_processor.add_stateful_operation("event_counting", count_state)
  
  // 模拟用户活动
  let user_activities = [
    ("user-001", "page_view", 1609459200000L),
    ("user-001", "page_view", 1609459210000L),
    ("user-002", "page_view", 1609459205000L),
    ("user-001", "add_to_cart", 1609459220000L),
    ("user-002", "page_view", 1609459215000L),
    ("user-001", "checkout", 1609459230000L)
  ]
  
  for (user_id, event_type, timestamp) in user_activities {
    let event = azimuth::StreamEvent::new(timestamp)
    event.set_field("user_id", user_id)
    event.set_field("event_type", event_type)
    stream_processor.process_event(event)
  }
  
  // 验证状态
  let session_state = stream_processor.get_state(session_op_id, "user-001")
  match session_state {
    Some(state) => {
      assert_eq(state.get_field("page_views"), 3)
      assert_eq(state.get_field("last_activity"), 1609459230000L)
    }
    None => assert_true(false)
  }
  
  let user_002_state = stream_processor.get_state(session_op_id, "user-002")
  match user_002_state {
    Some(state) => {
      assert_eq(state.get_field("page_views"), 2)
    }
    None => assert_true(false)
  }
  
  // 验证计数状态
  let page_view_count = stream_processor.get_state(count_op_id, "page_view")
  match page_view_count {
    Some(state) => {
      assert_eq(state.get_field("event_count"), 5)
    }
    None => assert_true(false)
  }
  
  // 测试状态过期
  let expired_event = azimuth::StreamEvent::new(1609459200000L + 1800000L + 1000L) // 超过TTL
  expired_event.set_field("user_id", "user-001")
  expired_event.set_field("event_type", "page_view")
  stream_processor.process_event(expired_event)
  
  // 验证旧状态已过期
  let expired_state = stream_processor.get_state(session_op_id, "user-001")
  match expired_state {
    Some(state) => {
      // 应该是新的会话状态
      assert_eq(state.get_field("page_views"), 1)
    }
    None => assert_true(false)
  }
}

test "实时流数据输出和分发" {
  // 创建流处理器
  let stream_processor = azimuth::StreamProcessor::new("output-distributor")
  
  // 创建输出配置
  let alert_sink = azimuth::OutputSink::Webhook {
    name: "alert-webhook",
    url: "https://alerts.example.com/webhook",
    method: "POST",
    headers: [
      ("Authorization", "Bearer token123"),
      ("Content-Type", "application/json")
    ],
    batch_size: 10,
    batch_timeout_ms: 5000,
    retry_policy: azimuth::RetryPolicy::ExponentialBackoff {
      max_retries: 3,
      initial_delay_ms: 1000,
      max_delay_ms: 10000,
      multiplier: 2.0
    }
  }
  
  let metrics_sink = azimuth::OutputSink::Prometheus {
    name: "metrics-exporter",
    port: 9090,
    path: "/metrics",
    metric_prefix: "azimuth_stream_"
  }
  
  let storage_sink = azimuth::OutputSink::ElasticSearch {
    name: "archive-storage",
    hosts: ["es-01:9200", "es-02:9200"],
    index_pattern: "azimuth-events-{yyyy.MM.dd}",
    batch_size: 100,
    batch_timeout_ms: 10000
  }
  
  // 注册输出
  let alert_sink_id = stream_processor.add_output_sink(alert_sink)
  let metrics_sink_id = stream_processor.add_output_sink(metrics_sink)
  let storage_sink_id = stream_processor.add_output_sink(storage_sink)
  
  // 创建输出路由
  let alert_router = azimuth::OutputRouter::Condition {
    name: "alert-router",
    condition: azimuth::Condition::FieldGreaterThan {
      field: "error_rate",
      value: 0.05
    },
    output_sink: alert_sink_id
  }
  
  let metrics_router = azimuth::OutputRouter::All {
    name: "metrics-router",
    output_sink: metrics_sink_id
  }
  
  let storage_router = azimuth::OutputRouter::Sampling {
    name: "storage-router",
    sampling_rate: 0.1, // 10%采样率
    output_sink: storage_sink_id
  }
  
  // 注册路由
  stream_processor.add_output_router(alert_router)
  stream_processor.add_output_router(metrics_router)
  stream_processor.add_output_router(storage_router)
  
  // 模拟数据流
  for i in 0..=99 {
    let event = azimuth::StreamEvent::new(1609459200000L + (i * 1000L))
    event.set_field("service", "api-gateway")
    event.set_field("request_count", 1000 + (i.to_int() % 500))
    event.set_field("error_count", if i.to_int() % 20 == 0 { 100 } else { 5 })
    event.set_field("error_rate", if i.to_int() % 20 == 0 { 0.1 } else { 0.005 })
    
    stream_processor.process_event(event)
  }
  
  // 等待输出处理完成
  azimuth::TimeUtil::sleep_ms(6000)
  
  // 验证输出统计
  let alert_stats = stream_processor.get_output_stats(alert_sink_id)
  assert_eq(alert_stats.sent_records, 5) // 5个事件满足告警条件
  assert_eq(alert_stats.successful_sends, 5)
  assert_eq(alert_stats.failed_sends, 0)
  
  let metrics_stats = stream_processor.get_output_stats(metrics_sink_id)
  assert_eq(metrics_stats.sent_records, 100) // 所有事件都发送到指标
  assert_eq(metrics_stats.successful_sends, 100)
  
  let storage_stats = stream_processor.get_output_stats(storage_sink_id)
  assert_true(storage_stats.sent_records >= 5 && storage_stats.sent_records <= 15) // 约10%采样
}

test "实时流处理容错和恢复" {
  // 创建流处理器
  let stream_processor = azimuth::StreamProcessor::new("fault-tolerant-processor")
  
  // 配置容错设置
  let fault_tolerance_config = azimuth::FaultToleranceConfig {
    checkpoint_interval_ms: 10000,
    checkpoint_storage: azimuth::CheckpointStorage::FileSystem {
      path: "/tmp/checkpoints"
    },
    max_retries: 3,
    retry_delay_ms: 1000,
    dead_letter_queue: true,
    dlq_topic: "azimuth.dlq.events"
  }
  
  stream_processor.configure_fault_tolerance(fault_tolerance_config)
  
  // 创建处理管道
  let pipeline = azimuth::ProcessingPipeline::new("resilient-pipeline")
  
  // 添加可能失败的步骤
  pipeline.add_step(azimuth::ProcessingStep::Transformation {
    name: "risky-transformation",
    transformer: azimuth::Transformer::Custom {
      transform_fn: fn(event) {
        // 模拟10%的失败率
        if azimuth::Random::next_double() < 0.1 {
          return azimuth::Result::Error("Transformation failed")
        }
        return azimuth::Result::Ok(event)
      }
    },
    error_handling: azimuth::ErrorHandling::RetryWithDeadLetter
  })
  
  // 注册管道
  let pipeline_id = stream_processor.register_pipeline(pipeline)
  
  // 模拟数据流
  let processed_events = 0
  let failed_events = 0
  
  for i in 0..=99 {
    let event = azimuth::StreamEvent::new(1609459200000L + (i * 1000L))
    event.set_field("event_id", "event-" + i.to_string())
    
    let result = stream_processor.process_event_with_pipeline(event, pipeline_id)
    
    match result {
      azimuth::Result::Ok(_) => processed_events = processed_events + 1,
      azimuth::Result::Error(_) => failed_events = failed_events + 1
    }
  }
  
  // 验证处理结果
  assert_true(processed_events > 80) // 至少80%的事件应该成功处理
  assert_true(failed_events > 0 && failed_events < 20) // 应该有一些失败事件
  
  // 验证死信队列
  let dlq_events = stream_processor.get_dlq_events()
  assert_eq(dlq_events.length(), failed_events)
  
  // 验证检查点
  let checkpoints = stream_processor.get_checkpoints()
  assert_true(checkpoints.length() > 0)
  
  // 模拟故障恢复
  stream_processor.simulate_failure()
  assert_eq(stream_processor.get_state(), azimuth::ProcessorState::Failed)
  
  // 尝试恢复
  let recovery_result = stream_processor.recover_from_latest_checkpoint()
  assert_true(recovery_result)
  assert_eq(stream_processor.get_state(), azimuth::ProcessorState::Running)
  
  // 验证恢复后的状态
  let recovered_checkpoints = stream_processor.get_checkpoints()
  assert_true(recovered_checkpoints.length() >= checkpoints.length())
}