// Azimuth 分布式追踪性能测试
// 专注于分布式追踪系统在高负载场景下的性能表现

// 测试1: 高并发追踪创建性能
test "高并发追踪创建性能" {
  // 创建分布式追踪性能测试器
  let tracing_performance_tester = DistributedTracingPerformanceTester::new()
  
  // 配置测试参数
  TracingPerformanceTester::set_test_parameters(tracing_performance_tester, {
    concurrent_spans: 1000,      // 并发span数量
    span_depth: 5,               // 追踪深度
    test_duration: 60,           // 测试持续时间（秒）
    warmup_duration: 10,         // 预热时间（秒）
    sampling_rate: 1.0           // 100%采样率
  })
  
  // 创建追踪器池
  let tracer_pool = TracerPool::new()
  TracerPool::initialize(tracer_pool, {
    pool_size: 10,
    buffer_size: 10000,
    batch_size: 100,
    flush_interval: 1000
  })
  
  // 预热阶段
  let warmup_result = TracingPerformanceTester::warmup(tracing_performance_tester, tracer_pool)
  
  // 验证预热结果
  assert_eq(warmup_result.spans_created, 1000)  // 预热创建1000个span
  assert_true(warmup_result.average_creation_time_ms > 0)
  assert_true(warmup_result.memory_stabilized)
  
  // 执行高并发追踪创建测试
  let concurrent_creation_result = TracingPerformanceTester::run_concurrent_creation_test(
    tracing_performance_tester, 
    tracer_pool
  )
  
  // 验证并发创建性能
  assert_true(concurrent_creation_result.test_completed)
  assert_eq(concurrent_creation_result.total_spans_created, 60000)  // 1000 spans/sec × 60 sec
  assert_true(concurrent_creation_result.successful_spans > 59000)  // 至少98%成功率
  
  // 验证创建延迟
  assert_true(concurrent_creation_result.average_creation_time_ms < 1.0)    // 平均创建时间小于1ms
  assert_true(concurrent_creation_result.p95_creation_time_ms < 5.0)        // P95创建时间小于5ms
  assert_true(concurrent_creation_result.p99_creation_time_ms < 10.0)       // P99创建时间小于10ms
  
  // 验证吞吐量
  assert_true(concurrent_creation_result.peak_throughput >= 1000)           // 峰值吞吐量至少1000 spans/sec
  assert_true(concurrent_creation_result.average_throughput >= 900)         // 平均吞吐量至少900 spans/sec
  
  // 验证资源使用
  assert_true(concurrent_creation_result.max_memory_usage_mb < 100)         // 内存使用小于100MB
  assert_true(concurrent_creation_result.max_cpu_usage_percent < 80)        // CPU使用率小于80%
  
  // 验证错误率
  assert_true(concurrent_creation_result.error_rate < 0.02)                 // 错误率小于2%
  assert_eq(concurrent_creation_result.timeouts, 0)                         // 不应该有超时
  
  // 测试不同追踪深度的性能影响
  let depth_performance_results = []
  for depth in [1, 3, 5, 8, 10] {
    let depth_result = TracingPerformanceTester::test_span_depth_performance(
      tracing_performance_tester, 
      tracer_pool, 
      depth
    )
    depth_performance_results = depth_performance_results.push(depth_result)
  }
  
  // 验证深度对性能的影响
  assert_eq(depth_performance_results.length(), 5)
  
  // 深度1应该最快
  let depth1_result = depth_performance_results[0]
  let depth10_result = depth_performance_results[4]
  
  assert_true(depth1_result.average_creation_time_ms < depth10_result.average_creation_time_ms)
  assert_true(depth1_result.average_throughput > depth10_result.average_throughput)
  
  // 验证性能退化是合理的（不超过10倍）
  let performance_degradation = depth10_result.average_creation_time_ms / depth1_result.average_creation_time_ms
  assert_true(performance_degradation < 10.0)
  
  // 测试批量创建性能
  let batch_creation_result = TracingPerformanceTester::test_batch_creation(
    tracing_performance_tester, 
    tracer_pool, 
    [10, 50, 100, 500, 1000]
  )
  
  // 验证批量创建性能
  assert_eq(batch_creation_result.batch_sizes.length(), 5)
  assert_eq(batch_creation_result.average_times_ms.length(), 5)
  
  // 批量创建应该比单个创建更高效
  let single_span_time = batch_creation_result.average_times_ms[0] / 10  // 单个span平均时间
  let large_batch_time = batch_creation_result.average_times_ms[4] / 1000  // 大批量中单个span平均时间
  
  assert_true(large_batch_time < single_span_time)  // 批量创建应该更高效
  
  // 测试追踪器池扩展性
  let scalability_result = TracingPerformanceTester::test_tracer_pool_scalability(
    tracing_performance_tester, 
    [1, 5, 10, 20, 50]
  )
  
  // 验证扩展性
  assert_eq(scalability_result.pool_sizes.length(), 5)
  assert_eq(scalability_result.throughput_per_pool.length(), 5)
  
  // 更多的追踪器应该提供更高的吞吐量（但有收益递减）
  let single_tracer_throughput = scalability_result.throughput_per_pool[0]
  let fifty_tracer_throughput = scalability_result.throughput_per_pool[4]
  
  assert_true(fifty_tracer_throughput > single_tracer_throughput)
  
  // 验证扩展效率（50个追踪器不应该提供50倍吞吐量）
  let scalability_ratio = fifty_tracer_throughput / single_tracer_throughput
  assert_true(scalability_ratio < 50.0)
  assert_true(scalability_ratio > 10.0)  // 但至少应该有10倍提升
}

// 测试2: 追踪数据序列化性能
test "追踪数据序列化性能" {
  // 创建序列化性能测试器
  let serialization_performance_tester = SerializationPerformanceTester::new()
  
  // 配置测试序列化格式
  SerializationPerformanceTester::add_format(serialization_performance_tester, "json", {
    name: "JSON",
    content_type: "application/json",
    pretty_print: false,
    include_metadata: true
  })
  
  SerializationPerformanceTester::add_format(serialization_performance_tester, "protobuf", {
    name: "Protocol Buffers",
    content_type: "application/x-protobuf",
    optimize_for: "speed",
    include_metadata: true
  })
  
  SerializationPerformanceTester::add_format(serialization_performance_tester, "msgpack", {
    name: "MessagePack",
    content_type: "application/x-msgpack",
    include_metadata: true
  })
  
  SerializationPerformanceTester::add_format(serialization_performance_tester, "avro", {
    name: "Apache Avro",
    content_type: "application/avro",
    schema_compatibility: "backward",
    include_metadata: true
  })
  
  // 创建不同复杂度的测试数据
  let simple_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: "",
    operation_name: "simple_operation",
    start_time: 1640995200000000,
    end_time: 1640995200100000,
    status: "ok",
    service_name: "test.service",
    attributes: [],
    events: [],
    links: []
  }
  
  let medium_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: "parent-123",
    operation_name: "medium_operation",
    start_time: 1640995200000000,
    end_time: 1640995200100000,
    status: "ok",
    service_name: "test.service",
    attributes: [
      ("http.method", StringValue("GET")),
      ("http.url", StringValue("/api/users")),
      ("http.status_code", IntValue(200)),
      ("user.id", StringValue("user-123"))
    ],
    events: [
      {
        name: "database_query",
        timestamp: 1640995200050000,
        attributes: [
          ("db.statement", StringValue("SELECT * FROM users")),
          ("db.type", StringValue("postgresql"))
        ]
      }
    ],
    links: []
  }
  
  let complex_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: "parent-123",
    operation_name: "complex_operation",
    start_time: 1640995200000000,
    end_time: 1640995200100000,
    status: "error",
    service_name: "test.service",
    attributes: [
      ("http.method", StringValue("POST")),
      ("http.url", StringValue("/api/process")),
      ("http.status_code", IntValue(500)),
      ("user.id", StringValue("user-123")),
      ("request.size", IntValue(1024)),
      ("response.size", IntValue(512)),
      ("error.type", StringValue("timeout")),
      ("retry.count", IntValue(3))
    ],
    events: [
      {
        name: "database_query",
        timestamp: 1640995200020000,
        attributes: [
          ("db.statement", StringValue("SELECT * FROM users WHERE id = $1")),
          ("db.type", StringValue("postgresql")),
          ("db.duration", LongValue(50000000))
        ]
      },
      {
        name: "cache_lookup",
        timestamp: 1640995200040000,
        attributes: [
          ("cache.key", StringValue("user-123")),
          ("cache.hit", BoolValue(false))
        ]
      },
      {
        name: "error",
        timestamp: 1640995200080000,
        attributes: [
          ("error.message", StringValue("Connection timeout")),
          ("error.stack_trace", StringValue("at processRequest (index.js:123:45)"))
        ]
      }
    ],
    links: [
      {
        trace_id: "trace-linked-1",
        span_id: "span-linked-1",
        attributes: [
          ("link.type", StringValue("follows_from"))
        ]
      }
    ]
  }
  
  // 测试不同复杂度span的序列化性能
  let complexity_results = []
  let test_spans = [simple_span, medium_span, complex_span]
  let complexity_names = ["simple", "medium", "complex"]
  
  for i in 0..=2 {
    let span = test_spans[i]
    let complexity_name = complexity_names[i]
    
    let format_results = []
    for format in ["json", "protobuf", "msgpack", "avro"] {
      let format_result = SerializationPerformanceTester::test_serialization_format(
        serialization_performance_tester,
        span,
        format,
        1000  // 序列化1000次
      )
      format_results = format_results.push(format_result)
    }
    
    complexity_results = complexity_results.push({
      complexity: complexity_name,
      formats: format_results
    })
  }
  
  // 验证复杂度对序列化性能的影响
  assert_eq(complexity_results.length(), 3)
  
  // 简单span应该序列化最快
  let simple_results = complexity_results[0]
  let complex_results = complexity_results[2]
  
  let simple_json_time = simple_results.formats[0].average_serialization_time_ms
  let complex_json_time = complex_results.formats[0].average_serialization_time_ms
  
  assert_true(simple_json_time < complex_json_time)
  
  // 验证不同格式的性能差异
  for complexity_result in complexity_results {
    let json_time = complexity_result.formats[0].average_serialization_time_ms
    let protobuf_time = complexity_result.formats[1].average_serialization_time_ms
    let msgpack_time = complexity_result.formats[2].average_serialization_time_ms
    let avro_time = complexity_result.formats[3].average_serialization_time_ms
    
    // Protobuf和MessagePack应该比JSON快
    assert_true(protobuf_time < json_time)
    assert_true(msgpack_time < json_time)
    
    // 验证序列化大小
    let json_size = complexity_result.formats[0].average_serialized_size_bytes
    let protobuf_size = complexity_result.formats[1].average_serialized_size_bytes
    let msgpack_size = complexity_result.formats[2].average_serialized_size_bytes
    
    // 二进制格式应该比JSON更紧凑
    assert_true(protobuf_size < json_size)
    assert_true(msgpack_size < json_size)
  }
  
  // 测试批量序列化性能
  let batch_sizes = [10, 50, 100, 500, 1000]
  let batch_serialization_results = []
  
  for batch_size in batch_sizes {
    let batch = []
    for i in 0..=batch_size - 1 {
      batch = batch.push(medium_span)  // 使用中等复杂度的span
    }
    
    let batch_result = SerializationPerformanceTester::test_batch_serialization(
      serialization_performance_tester,
      batch,
      ["json", "protobuf", "msgpack"]
    )
    
    batch_serialization_results = batch_serialization_results.push(batch_result)
  }
  
  // 验证批量序列化性能
  assert_eq(batch_serialization_results.length(), 5)
  
  // 批量序列化应该有更好的吞吐量
  let small_batch = batch_serialization_results[0]  // 10个span
  let large_batch = batch_serialization_results[4]  // 1000个span
  
  let small_batch_throughput = small_batch.formats[0].throughput_spans_per_sec
  let large_batch_throughput = large_batch.formats[0].throughput_spans_per_sec
  
  assert_true(large_batch_throughput > small_batch_throughput)
  
  // 测试反序列化性能
  let deserialization_results = []
  
  for format in ["json", "protobuf", "msgpack", "avro"] {
    // 先序列化一个span
    let serialized = SerializationPerformanceTester::serialize_span(
      serialization_performance_tester,
      complex_span,
      format
    )
    
    // 然后测试反序列化性能
    let deserialization_result = SerializationPerformanceTester::test_deserialization(
      serialization_performance_tester,
      serialized,
      format,
      1000  // 反序列化1000次
    )
    
    deserialization_results = deserialization_results.push(deserialization_result)
  }
  
  // 验证反序列化性能
  assert_eq(deserialization_results.length(), 4)
  
  // 反序列化性能应该与序列化性能相当
  for i in 0..=3 {
    let serialization_time = complexity_results[2].formats[i].average_serialization_time_ms
    let deserialization_time = deserialization_results[i].average_deserialization_time_ms
    
    // 反序列化时间不应该超过序列化时间的2倍
    assert_true(deserialization_time < serialization_time * 2.0)
  }
  
  // 测试内存使用效率
  let memory_efficiency_results = SerializationPerformanceTester::test_memory_efficiency(
    serialization_performance_tester,
    [simple_span, medium_span, complex_span],
    ["json", "protobuf", "msgpack", "avro"],
    10000  // 10000个span
  )
  
  // 验证内存效率
  assert_eq(memory_efficiency_results.span_complexities.length(), 3)
  assert_eq(memory_efficiency_results.formats.length(), 4)
  
  // 二进制格式应该使用更少的内存
  for i in 0..=2 {
    let json_memory = memory_efficiency_results.formats[i].json_memory_usage_mb
    let protobuf_memory = memory_efficiency_results.formats[i].protobuf_memory_usage_mb
    let msgpack_memory = memory_efficiency_results.formats[i].msgpack_memory_usage_mb
    
    assert_true(protobuf_memory < json_memory)
    assert_true(msgpack_memory < json_memory)
  }
}

// 测试3: 追踪数据传输性能
test "追踪数据传输性能" {
  // 创建传输性能测试器
  let transport_performance_tester = TransportPerformanceTester::new()
  
  // 配置传输协议
  TransportPerformanceTester::add_protocol(transport_performance_tester, "http", {
    name: "HTTP/HTTPS",
    endpoint: "https://collector.example.com/api/v1/traces",
    method: "POST",
    headers: [
      ("Content-Type", "application/json"),
      ("Authorization", "Bearer test-token")
    ],
    compression: "gzip",
    timeout_ms: 5000,
    retry_policy: {
      max_retries: 3,
      backoff_strategy: "exponential",
      initial_delay_ms: 100
    }
  })
  
  TransportPerformanceTester::add_protocol(transport_performance_tester, "grpc", {
    name: "gRPC",
    endpoint: "collector.example.com:4317",
    service: "opentelemetry.proto.collector.trace.v1.TraceService",
    method: "Export",
    compression: "gzip",
    timeout_ms: 5000,
    retry_policy: {
      max_retries: 3,
      backoff_strategy: "exponential",
      initial_delay_ms: 100
    }
  })
  
  TransportPerformanceTester::add_protocol(transport_performance_tester, "kafka", {
    name: "Apache Kafka",
    brokers: ["kafka1.example.com:9092", "kafka2.example.com:9092"],
    topic: "otel-traces",
    compression: "snappy",
    batch_size: 100,
    linger_ms: 10,
    retry_policy: {
      max_retries: 3,
      backoff_strategy: "exponential",
      initial_delay_ms: 100
    }
  })
  
  // 创建测试追踪数据
  let trace_batches = TransportPerformanceTester::generate_trace_batches(
    transport_performance_tester,
    {
      batch_count: 10,
      spans_per_batch: 100,
      trace_depth: 5,
      attribute_count: 10,
      event_count: 2
    }
  )
  
  // 验证追踪批次
  assert_eq(trace_batches.length(), 10)
  assert_eq(trace_batches[0].spans.length(), 100)
  
  // 测试不同协议的传输性能
  let protocol_results = []
  
  for protocol in ["http", "grpc", "kafka"] {
    let protocol_result = TransportPerformanceTester::test_protocol_performance(
      transport_performance_tester,
      protocol,
      trace_batches,
      3  // 每个协议测试3次
    )
    
    protocol_results = protocol_results.push(protocol_result)
  }
  
  // 验证协议性能
  assert_eq(protocol_results.length(), 3)
  
  // 验证传输延迟
  for protocol_result in protocol_results {
    assert_true(protocol_result.average_latency_ms < 1000)  // 平均延迟小于1秒
    assert_true(protocol_result.p95_latency_ms < 2000)     // P95延迟小于2秒
    assert_true(protocol_result.p99_latency_ms < 5000)     // P99延迟小于5秒
  }
  
  // 验证传输吞吐量
  for protocol_result in protocol_results {
    assert_true(protocol_result.throughput_spans_per_sec > 100)  // 至少100 spans/sec
    assert_true(protocol_result.throughput_bytes_per_sec > 1000) // 至少1KB/sec
  }
  
  // 验证传输可靠性
  for protocol_result in protocol_results {
    assert_true(protocol_result.success_rate > 0.99)  // 成功率大于99%
    assert_eq(protocol_result.lost_spans, 0)         // 不应该有丢失的span
  }
  
  // gRPC应该比HTTP更快
  let http_result = protocol_results[0]
  let grpc_result = protocol_results[1]
  
  assert_true(grpc_result.average_latency_ms < http_result.average_latency_ms)
  assert_true(grpc_result.throughput_spans_per_sec > http_result.throughput_spans_per_sec)
  
  // 测试不同批量大小的传输性能
  let batch_size_results = []
  let batch_sizes = [10, 50, 100, 500, 1000, 5000]
  
  for batch_size in batch_sizes {
    let large_trace_batches = TransportPerformanceTester::generate_trace_batches(
      transport_performance_tester,
      {
        batch_count: 5,
        spans_per_batch: batch_size,
        trace_depth: 3,
        attribute_count: 5,
        event_count: 1
      }
    )
    
    let batch_size_result = TransportPerformanceTester::test_batch_size_performance(
      transport_performance_tester,
      "grpc",  // 使用最快的协议
      large_trace_batches
    )
    
    batch_size_results = batch_size_results.push(batch_size_result)
  }
  
  // 验证批量大小对性能的影响
  assert_eq(batch_size_results.length(), 6)
  
  // 较大的批量应该有更好的吞吐量（但有收益递减）
  let small_batch = batch_size_results[0]  // 10个span
  let large_batch = batch_size_results[4]  // 1000个span
  
  assert_true(large_batch.throughput_spans_per_sec > small_batch.throughput_spans_per_sec)
  
  // 验证批量大小对延迟的影响
  assert_true(small_batch.average_latency_ms < large_batch.average_latency_ms)
  
  // 测试压缩对传输性能的影响
  let compression_results = []
  
  for compression in ["none", "gzip", "snappy", "lz4"] {
    let compression_result = TransportPerformanceTester::test_compression_performance(
      transport_performance_tester,
      "grpc",
      trace_batches,
      compression
    )
    
    compression_results = compression_results.push(compression_result)
  }
  
  // 验证压缩性能
  assert_eq(compression_results.length(), 4)
  
  let no_compression = compression_results[0]
  let gzip_compression = compression_results[1]
  let snappy_compression = compression_results[2]
  let lz4_compression = compression_results[3]
  
  // 压缩应该减少传输大小
  assert_true(gzip_compression.average_compressed_size_bytes < no_compression.average_compressed_size_bytes)
  assert_true(snappy_compression.average_compressed_size_bytes < no_compression.average_compressed_size_bytes)
  assert_true(lz4_compression.average_compressed_size_bytes < no_compression.average_compressed_size_bytes)
  
  // 验证压缩比
  assert_true(gzip_compression.compression_ratio > 2.0)  // gzip至少2倍压缩
  assert_true(snappy_compression.compression_ratio > 1.5) // snappy至少1.5倍压缩
  assert_true(lz4_compression.compression_ratio > 1.5)    // lz4至少1.5倍压缩
  
  // 验证压缩对性能的影响
  // Snappy和LZ4应该比GZIP更快
  assert_true(snappy_compression.compression_time_ms < gzip_compression.compression_time_ms)
  assert_true(lz4_compression.compression_time_ms < gzip_compression.compression_time_ms)
  
  // 测试网络条件对传输性能的影响
  let network_condition_results = []
  
  for network_condition in ["perfect", "high_latency", "packet_loss", "low_bandwidth"] {
    let network_result = TransportPerformanceTester::test_network_conditions(
      transport_performance_tester,
      "grpc",
      trace_batches,
      network_condition
    )
    
    network_condition_results = network_condition_results.push(network_result)
  }
  
  // 验证网络条件对性能的影响
  assert_eq(network_condition_results.length(), 4)
  
  let perfect_network = network_condition_results[0]
  let high_latency_network = network_condition_results[1]
  let packet_loss_network = network_condition_results[2]
  let low_bandwidth_network = network_condition_results[3]
  
  // 高延迟网络应该增加延迟
  assert_true(high_latency_network.average_latency_ms > perfect_network.average_latency_ms)
  
  // 丢包网络应该降低成功率
  assert_true(packet_loss_network.success_rate < perfect_network.success_rate)
  
  // 低带宽网络应该降低吞吐量
  assert_true(low_bandwidth_network.throughput_spans_per_sec < perfect_network.throughput_spans_per_sec)
  
  // 测试重试机制
  let retry_results = TransportPerformanceTester::test_retry_mechanism(
    transport_performance_tester,
    "http",
    trace_batches,
    {
      failure_rate: 0.1,  // 10%失败率
      failure_types: ["timeout", "connection_error", "server_error"]
    }
  )
  
  // 验证重试机制
  assert_true(retry_results.initial_failures > 0)
  assert_true(retry_results.retry_attempts > 0)
  assert_true(retry_results.final_success_rate > retry_results.initial_success_rate)
  assert_true(retry_results.retry_efficiency > 0.8)  // 重试效率大于80%
}

// 测试4: 追踪存储性能
test "追踪存储性能" {
  // 创建存储性能测试器
  let storage_performance_tester = StoragePerformanceTester::new()
  
  // 配置存储后端
  StoragePerformanceTester::add_backend(storage_performance_tester, "elasticsearch", {
    name: "Elasticsearch",
    endpoint: "https://elasticsearch.example.com:9200",
    index_pattern: "otel-traces-{yyyy.MM.dd}",
    shard_count: 3,
    replica_count: 1,
    refresh_interval: "5s",
    bulk_size: 1000,
    flush_interval: 10000
  })
  
  StoragePerformanceTester::add_backend(storage_performance_tester, "cassandra", {
    name: "Apache Cassandra",
    contact_points: ["cassandra1.example.com", "cassandra2.example.com"],
    keyspace: "otel_traces",
    table: "spans",
    replication_factor: 2,
    consistency_level: "QUORUM",
    batch_size: 100,
    concurrent_writes: 10
  })
  
  StoragePerformanceTester::add_backend(storage_performance_tester, "clickhouse", {
    name: "ClickHouse",
    endpoint: "https://clickhouse.example.com:8443",
    database: "otel",
    table: "traces",
    engine: "MergeTree",
    partition_by: "toYYYYMMDD(timestamp)",
    order_by: "(trace_id, span_id)",
    index_granularity: 8192,
    max_insert_block_size: 1000000
  })
  
  // 创建测试数据集
  let test_datasets = StoragePerformanceTester::generate_test_datasets(
    storage_performance_tester,
    [
      {
        name: "small_dataset",
        span_count: 10000,
        trace_count: 1000,
        attribute_count: 5,
        event_count: 1,
        time_range_hours: 1
      },
      {
        name: "medium_dataset",
        span_count: 100000,
        trace_count: 10000,
        attribute_count: 10,
        event_count: 2,
        time_range_hours: 24
      },
      {
        name: "large_dataset",
        span_count: 1000000,
        trace_count: 100000,
        attribute_count: 15,
        event_count: 3,
        time_range_hours: 168  // 1周
      }
    ]
  )
  
  // 验证测试数据集
  assert_eq(test_datasets.length(), 3)
  assert_eq(test_datasets[0].spans.length(), 10000)
  assert_eq(test_datasets[2].spans.length(), 1000000)
  
  // 测试写入性能
  let write_performance_results = []
  
  for backend in ["elasticsearch", "cassandra", "clickhouse"] {
    let backend_results = []
    
    for dataset in test_datasets {
      let write_result = StoragePerformanceTester::test_write_performance(
        storage_performance_tester,
        backend,
        dataset.spans
      )
      
      backend_results = backend_results.push(write_result)
    }
    
    write_performance_results = write_performance_results.push({
      backend: backend,
      datasets: backend_results
    })
  }
  
  // 验证写入性能
  assert_eq(write_performance_results.length(), 3)
  
  for backend_result in write_performance_results {
    assert_eq(backend_result.datasets.length(), 3)
    
    // 验证写入吞吐量
    for dataset_result in backend_result.datasets {
      assert_true(dataset_result.write_throughput_spans_per_sec > 100)
      assert_true(dataset_result.write_throughput_bytes_per_sec > 1000)
    }
    
    // 验证写入延迟
    let small_dataset = backend_result.datasets[0]
    let large_dataset = backend_result.datasets[2]
    
    assert_true(small_dataset.average_write_latency_ms < 100)
    assert_true(large_dataset.average_write_latency_ms < 1000)
    
    // 验证写入可靠性
    for dataset_result in backend_result.datasets {
      assert_eq(dataset_result.spans_written, dataset_result.spans_attempted)
      assert_eq(dataset_result.write_errors, 0)
    }
  }
  
  // ClickHouse应该有最高的写入吞吐量
  let elasticsearch_write = write_performance_results[0]
  let cassandra_write = write_performance_results[1]
  let clickhouse_write = write_performance_results[2]
  
  assert_true(clickhouse_write.datasets[1].write_throughput_spans_per_sec > 
              elasticsearch_write.datasets[1].write_throughput_spans_per_sec)
  
  // 测试查询性能
  let query_performance_results = []
  
  for backend in ["elasticsearch", "cassandra", "clickhouse"] {
    let query_results = []
    
    // 测试不同类型的查询
    let query_types = [
      {
        name: "trace_by_id",
        description: "根据trace ID查询完整追踪",
        query_template: "SELECT * FROM spans WHERE trace_id = ?",
        expected_result_count: 10  // 平均每个trace 10个span
      },
      {
        name: "spans_by_service",
        description: "根据服务名查询span",
        query_template: "SELECT * FROM spans WHERE service_name = ?",
        expected_result_count: 1000
      },
      {
        name: "spans_by_time_range",
        description: "根据时间范围查询span",
        query_template: "SELECT * FROM spans WHERE timestamp >= ? AND timestamp <= ?",
        expected_result_count: 10000
      },
      {
        name: "error_spans",
        description: "查询错误span",
        query_template: "SELECT * FROM spans WHERE status = 'error'",
        expected_result_count: 100
      },
      {
        name: "aggregation_query",
        description: "聚合查询",
        query_template: "SELECT service_name, COUNT(*) as span_count, AVG(duration) as avg_duration FROM spans GROUP BY service_name",
        expected_result_count: 10
      }
    ]
    
    for query_type in query_types {
      let query_result = StoragePerformanceTester::test_query_performance(
        storage_performance_tester,
        backend,
        query_type,
        test_datasets[1]  // 使用中等数据集
      )
      
      query_results = query_results.push(query_result)
    }
    
    query_performance_results = query_results.push({
      backend: backend,
      queries: query_results
    })
  }
  
  // 验证查询性能
  assert_eq(query_performance_results.length(), 3)
  
  for backend_result in query_performance_results {
    assert_eq(backend_result.queries.length(), 5)
    
    // 验证查询延迟
    for query_result in backend_result.queries {
      assert_true(query_result.average_latency_ms < 5000)  // 平均延迟小于5秒
      assert_true(query_result.p95_latency_ms < 10000)     // P95延迟小于10秒
    }
    
    // trace_by_id查询应该最快
    let trace_by_id = backend_result.queries[0]
    let aggregation_query = backend_result.queries[4]
    
    assert_true(trace_by_id.average_latency_ms < aggregation_query.average_latency_ms)
    
    // 验证查询准确性
    for query_result in backend_result.queries {
      assert_true(query_result.result_count > 0)
      assert_eq(query_result.query_errors, 0)
    }
  }
  
  // Elasticsearch应该对复杂查询有更好的性能
  let elasticsearch_queries = query_performance_results[0]
  let clickhouse_queries = query_performance_results[2]
  
  assert_true(elasticsearch_queries.queries[4].average_latency_ms < 
              clickhouse_queries.queries[4].average_latency_ms)
  
  // 测试存储压缩效果
  let compression_results = []
  
  for backend in ["elasticsearch", "cassandra", "clickhouse"] {
    let compression_result = StoragePerformanceTester::test_storage_compression(
      storage_performance_tester,
      backend,
      test_datasets[1]  // 使用中等数据集
    )
    
    compression_results = compression_results.push(compression_result)
  }
  
  // 验证存储压缩效果
  assert_eq(compression_results.length(), 3)
  
  for compression_result in compression_results {
    assert_true(compression_result.compression_ratio > 1.0)
    assert_true(compression_result.storage_efficiency > 0.5)
  }
  
  // ClickHouse应该有最好的压缩效果
  let elasticsearch_compression = compression_results[0]
  let clickhouse_compression = compression_results[2]
  
  assert_true(clickhouse_compression.compression_ratio > elasticsearch_compression.compression_ratio)
  
  // 测试存储扩展性
  let scalability_results = []
  
  for backend in ["elasticsearch", "cassandra", "clickhouse"] {
    let scalability_result = StoragePerformanceTester::test_storage_scalability(
      storage_performance_tester,
      backend,
      [1000, 10000, 100000, 1000000]  // 不同的数据量
    )
    
    scalability_results = scalability_results.push(scalability_result)
  }
  
  // 验证存储扩展性
  assert_eq(scalability_results.length(), 3)
  
  for scalability_result in scalability_results {
    assert_eq(scalability_result.data_points.length(), 4)
    
    // 验证写入性能扩展性
    let small_data = scalability_result.data_points[0]
    let large_data = scalability_result.data_points[3]
    
    // 性能下降应该是线性的，而不是指数级的
    let performance_degradation = large_data.average_write_latency_ms / small_data.average_write_latency_ms
    assert_true(performance_degradation < large_data.span_count / small_data.span_count)
    
    // 验证查询性能扩展性
    let small_query = scalability_result.data_points[0]
    let large_query = scalability_result.data_points[3]
    
    // 查询性能下降应该是可控的
    let query_degradation = large_query.average_query_latency_ms / small_query.average_query_latency_ms
    assert_true(query_degradation < 10.0)  // 查询时间增长不超过10倍
  }
  
  // 测试存储可靠性
  let reliability_results = []
  
  for backend in ["elasticsearch", "cassandra", "clickhouse"] {
    let reliability_result = StoragePerformanceTester::test_storage_reliability(
      storage_performance_tester,
      backend,
      test_datasets[0],  // 使用小数据集
      {
        node_failure_simulation: true,
        network_partition_simulation: true,
        write_failure_rate: 0.01,  // 1%写入失败率
        read_failure_rate: 0.005   // 0.5%读取失败率
      }
    )
    
    reliability_results = reliability_results.push(reliability_result)
  }
  
  // 验证存储可靠性
  assert_eq(reliability_results.length(), 3)
  
  for reliability_result in reliability_results {
    assert_true(reliability_result.data_durability > 0.999)  // 数据持久性大于99.9%
    assert_true(reliability_result.availability > 0.99)      // 可用性大于99%
    assert_true(reliability_result.consistency_level > 0.95) // 一致性大于95%
  }
  
  // Cassandra应该有最高的可用性
  let cassandra_reliability = reliability_results[1]
  let elasticsearch_reliability = reliability_results[0]
  
  assert_true(cassandra_reliability.availability > elasticsearch_reliability.availability)
}

// 测试5: 分布式追踪端到端性能
test "分布式追踪端到端性能" {
  // 创建端到端性能测试器
  let e2e_performance_tester = E2EPerformanceTester::new()
  
  // 配置测试场景
  E2EPerformanceTester::add_scenario(e2e_performance_tester, "microservices", {
    name: "Microservices Architecture",
    description: "典型的微服务架构追踪场景",
    service_count: 10,
    max_depth: 5,
    fan_out: 3,
    async_calls: true,
    parallel_processing: true,
    error_rate: 0.01,
    latency_simulation: {
      base_latency_ms: 50,
      variance: 20,
      network_latency_ms: 10,
      processing_latency_ms: 30
    }
  })
  
  E2EPerformanceTester::add_scenario(e2e_performance_tester, "monolithic", {
    name: "Monolithic Application",
    description: "单体应用内部追踪场景",
    service_count: 1,
    max_depth: 8,
    fan_out: 1,
    async_calls: false,
    parallel_processing: false,
    error_rate: 0.005,
    latency_simulation: {
      base_latency_ms: 10,
      variance: 5,
      network_latency_ms: 0,
      processing_latency_ms: 10
    }
  })
  
  E2EPerformanceTester::add_scenario(e2e_performance_tester, "serverless", {
    name: "Serverless Functions",
    description: "无服务器函数追踪场景",
    service_count: 15,
    max_depth: 3,
    fan_out: 2,
    async_calls: true,
    parallel_processing: true,
    error_rate: 0.02,
    latency_simulation: {
      base_latency_ms: 100,
      variance: 50,
      network_latency_ms: 20,
      processing_latency_ms: 80
    }
  })
  
  // 配置追踪链
  E2EPerformanceTester::configure_tracing_chain(e2e_performance_tester, {
    sampling: {
      type: "probabilistic",
      rate: 1.0  // 100%采样用于性能测试
    },
    propagation: {
      format: "w3c_trace_context",
      baggage_enabled: true
    },
    exporters: [
      {
        type: "otlp_http",
        endpoint: "http://collector.example.com:4318/v1/traces",
        batch_size: 100,
        timeout_ms: 5000
      }
    ],
    processors: [
      {
        type: "batch",
        max_queue_size: 2048,
        scheduled_delay_ms: 5000,
        max_export_batch_size: 512
      },
      {
        type: "resource",
        attributes: [
          ("service.name", "test-service"),
          ("service.version", "1.0.0"),
          ("deployment.environment", "test")
        ]
      }
    ]
  })
  
  // 执行端到端性能测试
  let e2e_results = []
  
  for scenario in ["microservices", "monolithic", "serverless"] {
    let e2e_result = E2EPerformanceTester::run_e2e_test(
      e2e_performance_tester,
      scenario,
      {
        request_count: 1000,
        concurrent_requests: 50,
        test_duration: 300,  // 5分钟
        warmup_requests: 100
      }
    )
    
    e2e_results = e2e_results.push(e2e_result)
  }
  
  // 验证端到端性能
  assert_eq(e2e_results.length(), 3)
  
  for e2e_result in e2e_results {
    // 验证追踪完整性
    assert_eq(e2e_result.total_requests, 1000)
    assert_eq(e2e_result.completed_traces, 1000)
    assert_eq(e2e_result.lost_traces, 0)
    assert_eq(e2e_result.incomplete_traces, 0)
    
    // 验证追踪延迟
    assert_true(e2e_result.average_trace_latency_ms < 1000)
    assert_true(e2e_result.p95_trace_latency_ms < 2000)
    assert_true(e2e_result.p99_trace_latency_ms < 5000)
    
    // 验证span完整性
    assert_eq(e2e_result.total_spans_generated, e2e_result.total_spans_collected)
    assert_eq(e2e_result.span_collection_rate, 1.0)
    
    // 验证资源开销
    assert_true(e2e_result.application_overhead_percent < 10.0)
    assert_true(e2e_result.memory_overhead_mb < 100)
    assert_true(e2e_result.cpu_overhead_percent < 15.0)
  }
  
  // 微服务架构应该产生更多的span
  let microservices_result = e2e_results[0]
  let monolithic_result = e2e_results[1]
  
  assert_true(microservices_result.total_spans_generated > monolithic_result.total_spans_generated)
  
  // 无服务器架构应该有更高的延迟
  let serverless_result = e2e_results[2]
  assert_true(serverless_result.average_trace_latency_ms > microservices_result.average_trace_latency_ms)
  
  // 测试不同采样率对性能的影响
  let sampling_rate_results = []
  
  for sampling_rate in [0.01, 0.1, 0.5, 1.0] {
    let sampling_result = E2EPerformanceTester::test_sampling_rate_impact(
      e2e_performance_tester,
      "microservices",
      sampling_rate,
      {
        request_count: 1000,
        concurrent_requests: 20,
        test_duration: 120
      }
    )
    
    sampling_rate_results = sampling_rate_results.push(sampling_result)
  }
  
  // 验证采样率对性能的影响
  assert_eq(sampling_rate_results.length(), 4)
  
  // 更高的采样率应该有更高的资源开销
  let low_sampling = sampling_rate_results[0]  // 1%
  let high_sampling = sampling_rate_results[3]  // 100%
  
  assert_true(high_sampling.application_overhead_percent > low_sampling.application_overhead_percent)
  assert_true(high_sampling.memory_overhead_mb > low_sampling.memory_overhead_mb)
  assert_true(high_sampling.cpu_overhead_percent > low_sampling.cpu_overhead_percent)
  
  // 验证采样率对数据完整性的影响
  assert_true(high_sampling.total_spans_collected > low_sampling.total_spans_collected)
  
  // 测试高负载下的追踪性能
  let high_load_result = E2EPerformanceTester::test_high_load_performance(
    e2e_performance_tester,
    "microservices",
    {
      request_count: 10000,
      concurrent_requests: 500,
      test_duration: 600,  // 10分钟
      ramp_up_time: 60     // 1分钟 ramp-up
    }
  )
  
  // 验证高负载性能
  assert_eq(high_load_result.total_requests, 10000)
  assert_true(high_load_result.completed_traces > 9900)  // 至少99%完成率
  
  // 验证高负载下的性能稳定性
  assert_true(high_load_result.performance_degradation < 0.2)  // 性能下降小于20%
  assert_true(high_load_result.memory_growth_rate < 0.1)       // 内存增长率小于10%
  assert_true(high_load_result.error_rate < 0.01)              // 错误率小于1%
  
  // 测试追踪系统故障恢复
  let fault_tolerance_result = E2EPerformanceTester::test_fault_tolerance(
    e2e_performance_tester,
    "microservices",
    {
      request_count: 1000,
      concurrent_requests: 20,
      test_duration: 300,
      fault_injections: [
        {
          type: "collector_unavailable",
          start_time: 60,   // 1分钟后注入故障
          duration: 30,     // 持续30秒
          severity: "medium"
        },
        {
          type: "network_latency_spike",
          start_time: 150,  // 2.5分钟后注入故障
          duration: 20,     // 持续20秒
          severity: "low"
        },
        {
          type: "storage_error",
          start_time: 240,  // 4分钟后注入故障
          duration: 15,     // 持续15秒
          severity: "high"
        }
      ]
    }
  )
  
  // 验证故障恢复能力
  assert_true(fault_tolerance_result.overall_success_rate > 0.95)  // 整体成功率大于95%
  
  // 验证故障期间的表现
  assert_true(fault_tolerance_result.fault_period_success_rate > 0.8)  // 故障期间成功率大于80%
  
  // 验证故障恢复时间
  assert_true(fault_tolerance_result.average_recovery_time_ms < 10000)  // 平均恢复时间小于10秒
  
  // 验证缓冲机制
  assert_true(fault_tolerance_result.buffer_utilization_peak > 0.5)     // 缓冲峰值利用率大于50%
  assert_true(fault_tolerance_result.buffer_drain_time_ms < 30000)     // 缓冲排空时间小于30秒
  
  // 测试长期运行稳定性
  let long_term_stability_result = E2EPerformanceTester::test_long_term_stability(
    e2e_performance_tester,
    "microservices",
    {
      test_duration: 3600,     // 1小时
      request_rate: 10,        // 每秒10个请求
      monitoring_interval: 60  // 每分钟监控一次
    }
  )
  
  // 验证长期稳定性
  assert_true(long_term_stability_result.test_completed)
  assert_eq(long_term_stability_result.total_requests, 36000)
  assert_true(long_term_stability_result.success_rate > 0.99)  // 成功率大于99%
  
  // 验证性能稳定性
  assert_true(long_term_stability_result.performance_variance < 0.1)  // 性能方差小于10%
  assert_true(long_term_stability_result.memory_leak_detected == false)
  assert_true(long_term_stability_result.resource_usage_stable == true)
  
  // 生成端到端性能报告
  let e2e_performance_report = E2EPerformanceTester::generate_comprehensive_report(
    e2e_performance_tester,
    e2e_results,
    sampling_rate_results,
    high_load_result,
    fault_tolerance_result,
    long_term_stability_result
  )
  
  // 验证端到端性能报告
  assert_true(e2e_performance_report.executive_summary.length() > 0)
  assert_true(e2e_performance_report.scenario_comparison.length() > 0)
  assert_true(e2e_performance_report.sampling_rate_analysis.length() > 0)
  assert_true(e2e_performance_report.scalability_analysis.length() > 0)
  assert_true(e2e_performance_report.reliability_analysis.length() > 0)
  assert_true(e2e_performance_report.recommendations.length() > 0)
  
  // 验证建议内容
  let performance_recommendations = e2e_performance_report.recommendations.filter(fn(r) { r.category == "performance" })
  assert_true(performance_recommendations.length() > 0)
  
  let reliability_recommendations = e2e_performance_report.recommendations.filter(fn(r) { r.category == "reliability" })
  assert_true(reliability_recommendations.length() > 0)
  
  let scalability_recommendations = e2e_performance_report.recommendations.filter(fn(r) { r.category == "scalability" })
  assert_true(scalability_recommendations.length() > 0)
}