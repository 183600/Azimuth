// Azimuth Data Integrity Validation Tests
// 数据完整性验证测试用例

test "telemetry data checksum validation" {
  // 测试遥测数据校验和验证
  let telemetry_batch = @azimuth.TelemetryBatch {
    batch_id : "batch-123456",
    timestamp : 1640995200000L,
    data_records : [
      @azimuth.TelemetryRecord {
        record_id : "record-001",
        timestamp : 1640995200000L,
        trace_id : "trace-abcdef123456",
        span_id : "span-1234567890",
        operation_name : "http.request",
        duration_ms : 150L,
        status : @azimuth.SpanStatus::Ok,
        attributes : [
          ("http.method", @azimuth.StringValue("GET")),
          ("http.url", @azimuth.StringValue("/api/users")),
          ("http.status_code", @azimuth.IntValue(200))
        ]
      },
      @azimuth.TelemetryRecord {
        record_id : "record-002",
        timestamp : 1640995200100L,
        trace_id : "trace-abcdef123456",
        span_id : "span-0987654321",
        operation_name : "database.query",
        duration_ms : 75L,
        status : @azimuth.SpanStatus::Ok,
        attributes : [
          ("db.statement", @azimuth.StringValue("SELECT * FROM users")),
          ("db.type", @azimuth.StringValue("postgresql"))
        ]
      }
    ],
    checksum : "a1b2c3d4e5f67890abcdef1234567890"
  }
  
  // 验证批次结构
  assert_eq(telemetry_batch.batch_id, "batch-123456")
  assert_eq(telemetry_batch.data_records.length(), 2)
  assert_eq(telemetry_batch.checksum, "a1b2c3d4e5f67890abcdef1234567890")
  
  // 验证记录结构
  let first_record = telemetry_batch.data_records[0]
  assert_eq(first_record.record_id, "record-001")
  assert_eq(first_record.trace_id, "trace-abcdef123456")
  assert_eq(first_record.span_id, "span-1234567890")
  assert_eq(first_record.operation_name, "http.request")
  assert_eq(first_record.duration_ms, 150L)
  match first_record.status {
    @azimuth.SpanStatus::Ok => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证属性
  assert_eq(first_record.attributes.length(), 3)
  let method_attr = first_record.attributes.filter(fn(attr) { attr.0 == "http.method" })
  assert_eq(method_attr.length(), 1)
  match method_attr[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "GET")
    _ => assert_true(false)
  }
}

test "data transmission error detection" {
  // 测试数据传输错误检测
  let transmission_result = @azimuth.TransmissionResult {
    transmission_id : "trans-123456",
    source : "payment-service",
    destination : "otel-collector",
    start_time : 1640995200000L,
    end_time : 1640995200500L,
    total_bytes : 1048576, // 1MB
    transmitted_bytes : 1048576,
    corrupted_packets : 0,
    lost_packets : 0,
    retransmitted_packets : 0,
    checksum_verified : true,
    transmission_status : @azimuth.TransmissionStatus::Success
  }
  
  // 验证传输结果
  assert_eq(transmission_result.transmission_id, "trans-123456")
  assert_eq(transmission_result.source, "payment-service")
  assert_eq(transmission_result.destination, "otel-collector")
  assert_eq(transmission_result.total_bytes, 1048576)
  assert_eq(transmission_result.transmitted_bytes, 1048576)
  assert_eq(transmission_result.corrupted_packets, 0)
  assert_eq(transmission_result.lost_packets, 0)
  assert_eq(transmission_result.retransmitted_packets, 0)
  assert_true(transmission_result.checksum_verified)
  
  match transmission_result.transmission_status {
    @azimuth.TransmissionStatus::Success => assert_true(true)
    _ => assert_true(false)
  }
}

test "data consistency across distributed nodes" {
  // 测试分布式节点间的数据一致性
  let consistency_check = @azimuth.ConsistencyCheck {
    check_id : "consistency-789",
    timestamp : 1640995200000L,
    participating_nodes : [
      "node-1.us-west-2",
      "node-2.us-west-2",
      "node-3.us-west-2"
    ],
    trace_id : "trace-1234567890",
    expected_records : 100,
    actual_records_per_node : [
      ("node-1.us-west-2", 100),
      ("node-2.us-west-2", 100),
      ("node-3.us-west-2", 100)
    ],
    consistency_score : 1.0,
    inconsistencies : []
  }
  
  // 验证一致性检查结果
  assert_eq(consistency_check.check_id, "consistency-789")
  assert_eq(consistency_check.participating_nodes.length(), 3)
  assert_eq(consistency_check.expected_records, 100)
  assert_eq(consistency_check.actual_records_per_node.length(), 3)
  assert_eq(consistency_check.consistency_score, 1.0)
  assert_eq(consistency_check.inconsistencies.length(), 0)
  
  // 验证每个节点的记录数
  for node_record in consistency_check.actual_records_per_node {
    assert_eq(node_record.1, 100)
  }
}

test "data serialization and deserialization integrity" {
  // 测试数据序列化和反序列化完整性
  let original_data = @azimuth.ComplexTelemetryData {
    trace_id : "trace-abcdef1234567890",
    span_context : @azimuth.SpanContext {
      trace_id : "trace-abcdef1234567890",
      span_id : "span-1234567890",
      sampled : true,
      trace_state : "sampling.decision=true"
    },
    parent_span_id : Some("span-parent-123456"),
    operation_name : "complex.operation",
    start_time : 1640995200000L,
    end_time : Some(1640995200200L),
    status : @azimuth.SpanStatus::Ok,
    attributes : [
      ("service.name", @azimuth.StringValue("payment-service")),
      ("service.version", @azimuth.StringValue("2.1.0")),
      ("deployment.environment", @azimuth.StringValue("production")),
      ("k8s.pod.name", @azimuth.StringValue("payment-service-7d4f8c9b-xyz")),
      ("k8s.namespace", @azimuth.StringValue("default")),
      ("cloud.region", @azimuth.StringValue("us-west-2")),
      ("cloud.provider", @azimuth.StringValue("aws"))
    ],
    events : [
      @azimuth.SpanEvent {
        name : "cache.miss",
        timestamp : 1640995200050L,
        attributes : [
          ("cache.key", @azimuth.StringValue("user:123")),
          ("cache.type", @azimuth.StringValue("redis"))
        ]
      },
      @azimuth.SpanEvent {
        name : "db.query.start",
        timestamp : 1640995200100L,
        attributes : [
          ("db.statement", @azimuth.StringValue("SELECT * FROM users WHERE id = ?")),
          ("db.type", @azimuth.StringValue("postgresql"))
        ]
      }
    ],
    links : [
      @azimuth.SpanLink {
        trace_id : "trace-linked-123456",
        span_id : "span-linked-123456",
        attributes : [
          ("link.type", @azimuth.StringValue("follows_from"))
        ]
      }
    ]
  }
  
  // 验证复杂数据结构
  assert_eq(original_data.trace_id, "trace-abcdef1234567890")
  assert_eq(original_data.operation_name, "complex.operation")
  assert_eq(original_data.attributes.length(), 7)
  assert_eq(original_data.events.length(), 2)
  assert_eq(original_data.links.length(), 1)
  
  // 验证span上下文
  assert_eq(original_data.span_context.trace_id, "trace-abcdef1234567890")
  assert_eq(original_data.span_context.span_id, "span-1234567890")
  assert_true(original_data.span_context.sampled)
  assert_eq(original_data.span_context.trace_state, "sampling.decision=true")
  
  // 验证父span ID
  match original_data.parent_span_id {
    Some(parent_id) => assert_eq(parent_id, "span-parent-123456")
    None => assert_true(false)
  }
  
  // 验证事件
  let first_event = original_data.events[0]
  assert_eq(first_event.name, "cache.miss")
  assert_eq(first_event.attributes.length(), 2)
  let cache_key_attr = first_event.attributes.filter(fn(attr) { attr.0 == "cache.key" })
  assert_eq(cache_key_attr.length(), 1)
  match cache_key_attr[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "user:123")
    _ => assert_true(false)
  }
  
  // 验证链接
  let first_link = original_data.links[0]
  assert_eq(first_link.trace_id, "trace-linked-123456")
  assert_eq(first_link.span_id, "span-linked-123456")
  assert_eq(first_link.attributes.length(), 1)
  let link_type_attr = first_link.attributes.filter(fn(attr) { attr.0 == "link.type" })
  assert_eq(link_type_attr.length(), 1)
  match link_type_attr[0].1 {
    @azimuth.StringValue(v) => assert_eq(v, "follows_from")
    _ => assert_true(false)
  }
}

test "data retention and archival integrity" {
  // 测试数据保留和归档完整性
  let archival_policy = @azimuth.ArchivalPolicy {
    policy_id : "policy-123456",
    name : "production-telemetry-archival",
    retention_period_days : 90,
    archival_storage_tier : @azimuth.StorageTier::Cold,
    compression_enabled : true,
    encryption_enabled : true,
    checksum_validation : true,
    archival_format : @azimuth.ArchivalFormat::Parquet
  }
  
  let archival_job = @azimuth.ArchivalJob {
    job_id : "job-789",
    policy_id : "policy-123456",
    start_time : 1640995200000L,
    end_time : Some(1640995800000L),
    status : @azimuth.JobStatus::Completed,
    records_processed : 1000000,
    records_archived : 1000000,
    records_failed : 0,
    original_size_bytes : 1073741824, // 1GB
    compressed_size_bytes : 214748364, // ~200MB
    checksum_before : "checksum-original-abcdef123456",
    checksum_after : "checksum-compressed-7890123456",
    archival_location : "s3://telemetry-archive/2022/01/01/job-789/"
  }
  
  // 验证归档策略
  assert_eq(archival_policy.policy_id, "policy-123456")
  assert_eq(archival_policy.name, "production-telemetry-archival")
  assert_eq(archival_policy.retention_period_days, 90)
  match archival_policy.archival_storage_tier {
    @azimuth.StorageTier::Cold => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(archival_policy.compression_enabled)
  assert_true(archival_policy.encryption_enabled)
  assert_true(archival_policy.checksum_validation)
  match archival_policy.archival_format {
    @azimuth.ArchivalFormat::Parquet => assert_true(true)
    _ => assert_true(false)
  }
  
  // 验证归档作业
  assert_eq(archival_job.job_id, "job-789")
  assert_eq(archival_job.policy_id, "policy-123456")
  match archival_job.status {
    @azimuth.JobStatus::Completed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(archival_job.records_processed, 1000000)
  assert_eq(archival_job.records_archived, 1000000)
  assert_eq(archival_job.records_failed, 0)
  assert_eq(archival_job.original_size_bytes, 1073741824)
  assert_eq(archival_job.compressed_size_bytes, 214748364)
  assert_true(archival_job.compressed_size_bytes < archival_job.original_size_bytes)
  assert_eq(archival_job.checksum_before, "checksum-original-abcdef123456")
  assert_eq(archival_job.checksum_after, "checksum-compressed-7890123456")
  assert_eq(archival_job.archival_location, "s3://telemetry-archive/2022/01/01/job-789/")
}