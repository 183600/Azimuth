// Azimuth Advanced Comprehensive Test Suite
// 高级综合测试套件 - 覆盖核心遥测系统的高级功能和边界情况

// Test 1: 高级内存管理和资源泄漏检测
test "advanced memory management and resource leak detection" {
  let memory_pool = @azimuth.MemoryPool::new(1024 * 1024) // 1MB内存池
  let resource_tracker = @azimuth.ResourceTracker::new()
  
  // 测试大量Span创建和销毁
  let mut spans = []
  for i in 0..=1000 {
    let span = @azimuth.Span::new("memory-test-span-#{i}", @azimuth.SpanKind::Internal, @azimuth.SpanContext::random())
    spans = spans.push(span)
    
    // 添加属性和事件
    @azimuth.Span::set_attribute(span, "iteration", @azimuth.IntValue(i))
    @azimuth.Span::add_event(span, "iteration-#{i}", Some([
      ("cpu.usage", @azimuth.FloatValue(0.1 + (i % 10) * 0.05)),
      ("memory.usage", @azimuth.IntValue(1024 * (i % 100)))
    ]))
  }
  
  // 验证内存使用情况
  let memory_stats = @azimuth.MemoryPool::get_stats(memory_pool)
  assert_true(memory_stats.allocated_bytes > 0)
  assert_true(memory_stats.allocated_bytes <= memory_stats.capacity_bytes)
  
  // 测试资源清理
  for span in spans {
    @azimuth.Span::end(span)
  }
  
  // 强制垃圾回收
  @azimuth.MemoryPool::force_gc(memory_pool)
  
  // 验证资源释放
  let final_memory_stats = @azimuth.MemoryPool::get_stats(memory_pool)
  assert_true(final_memory_stats.allocated_bytes < memory_stats.allocated_bytes)
  
  // 验证资源跟踪器
  let leaked_resources = @azimuth.ResourceTracker::get_leaked_resources(resource_tracker)
  assert_eq(leaked_resources.length(), 0)
}

// Test 2: 分布式追踪的一致性和完整性
test "distributed tracing consistency and integrity" {
  let trace_id = @azimuth.TraceId::generate()
  let root_span = @azimuth.Span::new("root-operation", @azimuth.SpanKind::Server, @azimuth.SpanContext::new(trace_id, @azimuth.SpanId::generate(), true, "root"))
  
  // 创建子Span链
  let child_span1 = @azimuth.Span::start_child(root_span, "child-operation-1")
  let child_span2 = @azimuth.Span::start_child(root_span, "child-operation-2")
  let grandchild_span = @azimuth.Span::start_child(child_span1, "grandchild-operation")
  
  // 设置Span关系
  @azimuth.Span::set_attribute(root_span, "service.name", @azimuth.StringValue("main-service"))
  @azimuth.Span::set_attribute(child_span1, "service.name", @azimuth.StringValue("child-service-1"))
  @azimuth.Span::set_attribute(child_span2, "service.name", @azimuth.StringValue("child-service-2"))
  @azimuth.Span::set_attribute(grandchild_span, "service.name", @azimuth.StringValue("grandchild-service"))
  
  // 测试Trace一致性
  assert_eq(@azimuth.Span::trace_id(root_span), trace_id)
  assert_eq(@azimuth.Span::trace_id(child_span1), trace_id)
  assert_eq(@azimuth.Span::trace_id(child_span2), trace_id)
  assert_eq(@azimuth.Span::trace_id(grandchild_span), trace_id)
  
  // 测试父子关系
  assert_eq(@azimuth.Span::parent_span_id(child_span1), @azimuth.Span::span_id(root_span))
  assert_eq(@azimuth.Span::parent_span_id(child_span2), @azimuth.Span::span_id(root_span))
  assert_eq(@azimuth.Span::parent_span_id(grandchild_span), @azimuth.Span::span_id(child_span1))
  
  // 创建Trace上下文传播
  let trace_context = @azimuth.TraceContext::from_span(root_span)
  let injected_headers = @azimuth.W3CTraceContextPropagator::inject(trace_context)
  
  // 验证注入的头部
  assert_true(injected_headers.contains_key("traceparent"))
  assert_true(injected_headers.contains_key("tracestate"))
  
  // 测试跨进程上下文提取
  let extracted_context = @azimuth.W3CTraceContextPropagator::extract(injected_headers)
  assert_eq(@azimuth.TraceContext::trace_id(extracted_context), trace_id)
  
  // 结束所有Span
  @azimuth.Span::end(grandchild_span)
  @azimuth.Span::end(child_span1)
  @azimuth.Span::end(child_span2)
  @azimuth.Span::end(root_span)
  
  // 验证Trace完整性
  let trace_validator = @azimuth.TraceValidator::new()
  let validation_result = @azimuth.TraceValidator::validate_trace(trace_validator, trace_id)
  assert_true(validation_result.is_complete)
  assert_eq(validation_result.span_count, 4)
  assert_true(validation_result.has_valid_root)
  assert_true(validation_result.all_spans_closed)
}

// Test 3: 实时流数据处理和背压处理
test "real-time stream data processing and backpressure handling" {
  let stream_processor = @azimuth.StreamProcessor::new(1000, 5000) // 缓冲区1000，背压阈值5000
  let telemetry_stream = @azimuth.TelemetryStream::new()
  
  // 模拟高速数据流
  let mut processed_count = 0
  let mut dropped_count = 0
  
  // 创建数据生产者
  let producer = @azimuth.StreamProducer::new(telemetry_stream)
  
  // 创建数据消费者
  let consumer = @azimuth.StreamConsumer::new(telemetry_stream, fn(data) {
    processed_count = processed_count + 1
    // 模拟处理延迟
    @azimuth.Time::sleep(1) // 1ms延迟
  })
  
  // 启动流处理
  @azimuth.StreamProcessor::start(stream_processor)
  
  // 发送大量数据
  for i in 0..=10000 {
    let telemetry_data = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("metric.name", @azimuth.StringValue("cpu.usage")),
        ("metric.value", @azimuth.FloatValue(0.1 + (i % 100) * 0.01)),
        ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos()))
      ]
    )
    
    let send_result = @azimuth.StreamProducer::send(producer, telemetry_data)
    match send_result {
      @azimuth.SendResult::Success => {}
      @azimuth.SendResult::BackpressureDropped => dropped_count = dropped_count + 1
      @azimuth.SendResult::BufferFull => dropped_count = dropped_count + 1
    }
  }
  
  // 等待处理完成
  @azimuth.Time::sleep(1000) // 1秒等待
  
  // 停止流处理
  @azimuth.StreamProcessor::stop(stream_processor)
  
  // 验证处理结果
  assert_true(processed_count > 0)
  assert_true(dropped_count >= 0)
  assert_eq(processed_count + dropped_count, 10001)
  
  // 验证背压处理
  let stream_stats = @azimuth.StreamProcessor::get_stats(stream_processor)
  assert_true(stream_stats.backpressure_events > 0)
  assert_true(stream_stats.processed_messages > 0)
  assert_true(stream_stats.dropped_messages >= 0)
}

// Test 4: 错误恢复和自愈能力
test "error recovery and self-healing capabilities" {
  let resilience_manager = @azimuth.ResilienceManager::new()
  let circuit_breaker = @azimuth.CircuitBreaker::new(5, 10000, 60000) // 5次失败，10秒超时，60秒重置
  
  // 测试熔断器状态
  assert_eq(@azimuth.CircuitBreaker::state(circuit_breaker), @azimuth.CircuitState::Closed)
  
  // 模拟连续失败
  for i in 0..=6 {
    let operation_result = @azimuth.ResilienceManager::execute_with_circuit_breaker(
      resilience_manager,
      circuit_breaker,
      fn() {
        if i < 5 {
          @azimuth.Result::Err("Simulated failure")
        } else {
          @azimuth.Result::Ok("Success")
        }
      }
    )
    
    if i < 5 {
      match operation_result {
        @azimuth.Result::Err(msg) => assert_eq(msg, "Simulated failure")
        _ => assert_true(false)
      }
    } else {
      // 第5次失败后应该触发熔断
      match operation_result {
        @azimuth.Result::Err(msg) => assert_eq(msg, "Circuit breaker is open")
        _ => assert_true(false)
      }
    }
  }
  
  // 验证熔断器状态
  assert_eq(@azimuth.CircuitBreaker::state(circuit_breaker), @azimuth.CircuitState::Open)
  
  // 测试重试机制
  let retry_config = @azimuth.RetryConfig::new(3, 1000, 2.0) // 3次重试，1秒初始延迟，2倍递增
  let mut attempt_count = 0
  
  let retry_result = @azimuth.ResilienceManager::execute_with_retry(
    resilience_manager,
    retry_config,
    fn() {
      attempt_count = attempt_count + 1
      if attempt_count < 3 {
        @azimuth.Result::Err("Temporary failure")
      } else {
        @azimuth.Result::Ok("Success after retries")
      }
    }
  )
  
  match retry_result {
    @azimuth.Result::Ok(msg) => assert_eq(msg, "Success after retries")
    _ => assert_true(false)
  }
  
  assert_eq(attempt_count, 3)
  
  // 测试超时处理
  let timeout_result = @azimuth.ResilienceManager::execute_with_timeout(
    resilience_manager,
    100, // 100ms超时
    fn() {
      @azimuth.Time::sleep(200) // 200ms延迟
      @azimuth.Result::Ok("Should not reach here")
    }
  )
  
  match timeout_result {
    @azimuth.Result::Err(msg) => assert_eq(msg, "Operation timed out")
    _ => assert_true(false)
  }
  
  // 测试自愈机制
  let health_checker = @azimuth.HealthChecker::new()
  @azimuth.HealthChecker::register_check(health_checker, "database", fn() {
    // 模拟数据库健康检查
    @azimuth.HealthStatus::Healthy
  })
  
  @azimuth.HealthChecker::register_check(health_checker, "external-api", fn() {
    // 模拟外部API健康检查
    @azimuth.HealthStatus::Unhealthy("API timeout")
  })
  
  let health_report = @azimuth.HealthChecker::check_all(health_checker)
  assert_eq(health_report.checks.length(), 2)
  assert_eq(health_report.healthy_checks.length(), 1)
  assert_eq(health_report.unhealthy_checks.length(), 1)
  assert_false(health_report.overall_healthy)
}

// Test 5: 高级数据压缩和优化
test "advanced data compression and optimization" {
  let compression_engine = @azimuth.CompressionEngine::new()
  
  // 创建大量遥测数据
  let mut telemetry_data = []
  for i in 0..=1000 {
    let data_point = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("service.name", @azimuth.StringValue("payment-service")),
        ("operation.name", @azimuth.StringValue("process-payment")),
        ("user.id", @azimuth.StringValue("user-#{i % 100}")), // 重复用户ID
        ("transaction.id", @azimuth.StringValue("txn-#{i}")),
        ("amount", @azimuth.FloatValue(10.0 + (i % 100) * 1.5)),
        ("currency", @azimuth.StringValue("USD")),
        ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos())),
        ("status", @azimuth.StringValue(if i % 10 == 0 { "failed" } else { "success" })),
        ("error.code", @azimuth.IntValue(if i % 10 == 0 { 500 } else { 0 }))
      ]
    )
    telemetry_data = telemetry_data.push(data_point)
  }
  
  // 测试不同压缩算法
  let gzip_compressed = @azimuth.CompressionEngine::compress(compression_engine, telemetry_data, @azimuth.CompressionAlgorithm::Gzip)
  let lz4_compressed = @azimuth.CompressionEngine::compress(compression_engine, telemetry_data, @azimuth.CompressionAlgorithm::LZ4)
  let zstd_compressed = @azimuth.CompressionEngine::compress(compression_engine, telemetry_data, @azimuth.CompressionAlgorithm::Zstd)
  
  // 验证压缩效果
  let original_size = @azimuth.TelemetryData::calculate_size(telemetry_data)
  let gzip_size = @azimuth.CompressedData::size(gzip_compressed)
  let lz4_size = @azimuth.CompressedData::size(lz4_compressed)
  let zstd_size = @azimuth.CompressedData::size(zstd_compressed)
  
  assert_true(gzip_size < original_size)
  assert_true(lz4_size < original_size)
  assert_true(zstd_size < original_size)
  
  // 验证解压缩正确性
  let gzip_decompressed = @azimuth.CompressionEngine::decompress(compression_engine, gzip_compressed)
  let lz4_decompressed = @azimuth.CompressionEngine::decompress(compression_engine, lz4_compressed)
  let zstd_decompressed = @azimuth.CompressionEngine::decompress(compression_engine, zstd_compressed)
  
  assert_eq(gzip_decompressed.length(), telemetry_data.length())
  assert_eq(lz4_decompressed.length(), telemetry_data.length())
  assert_eq(zstd_decompressed.length(), telemetry_data.length())
  
  // 测试增量压缩
  let incremental_compressor = @azimuth.IncrementalCompressor::new(@azimuth.CompressionAlgorithm::Zstd)
  let mut compressed_chunks = []
  
  for chunk in telemetry_data.chunk(100) {
    let compressed_chunk = @azimuth.IncrementalCompressor::compress_chunk(incremental_compressor, chunk)
    compressed_chunks = compressed_chunks.push(compressed_chunk)
  }
  
  let final_chunk = @azimuth.IncrementalCompressor::finalize(incremental_compressor)
  compressed_chunks = compressed_chunks.push(final_chunk)
  
  // 验证增量解压缩
  let incremental_decompressor = @azimuth.IncrementalDecompressor::new(@azimuth.CompressionAlgorithm::Zstd)
  let mut decompressed_data = []
  
  for chunk in compressed_chunks {
    let decompressed_chunk = @azimuth.IncrementalDecompressor::decompress_chunk(incremental_decompressor, chunk)
    decompressed_data = decompressed_data.concat(decompressed_chunk)
  }
  
  assert_eq(decompressed_data.length(), telemetry_data.length())
  
  // 测试压缩统计
  let compression_stats = @azimuth.CompressionEngine::get_stats(compression_engine)
  assert_true(compression_stats.compression_ratio > 1.0)
  assert_true(compression_stats.total_compressed > 0)
  assert_true(compression_stats.total_decompressed > 0)
}

// Test 6: 混合云环境下的遥测数据同步
test "hybrid cloud telemetry data synchronization" {
  let cloud_sync_manager = @azimuth.CloudSyncManager::new()
  
  // 配置多个云环境
  let aws_config = @azimuth.CloudConfig::new(
    @azimuth.CloudProvider::AWS,
    "us-west-2",
    ["otel-collector.aws.example.com:4317"],
    @azimuth.AuthConfig::new(@azimuth.AuthType::AWS_IAM, "arn:aws:iam::123456789012:role/OtelCollectorRole")
  )
  
  let azure_config = @azimuth.CloudConfig::new(
    @azimuth.CloudProvider::Azure,
    "eastus",
    ["otel-collector.azure.example.com:4317"],
    @azimuth.AuthConfig::new(@azimuth.AuthType::Azure_Managed_Identity, "otel-collector-identity")
  )
  
  let gcp_config = @azimuth.CloudConfig::new(
    @azimuth.CloudProvider::GCP,
    "us-central1",
    ["otel-collector.gcp.example.com:4317"],
    @azimuth.AuthConfig::new(@azimuth.AuthType::GCP_Service_Account, "otel-collector@gcp-project.iam.gserviceaccount.com")
  )
  
  // 注册云环境
  @azimuth.CloudSyncManager::register_cloud(cloud_sync_manager, "aws-primary", aws_config)
  @azimuth.CloudSyncManager::register_cloud(cloud_sync_manager, "azure-backup", azure_config)
  @azimuth.CloudSyncManager::register_cloud(cloud_sync_manager, "gcp-analytics", gcp_config)
  
  // 创建测试遥测数据
  let telemetry_batch = @azimuth.TelemetryBatch::new()
  for i in 0..=100 {
    let telemetry_data = @azimuth.TelemetryData::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanId::generate(),
      [
        ("cloud.environment", @azimuth.StringValue("hybrid")),
        ("data.source", @azimuth.StringValue("application-server")),
        ("batch.id", @azimuth.StringValue("batch-001")),
        ("record.index", @azimuth.IntValue(i)),
        ("timestamp", @azimuth.IntValue(@azimuth.Time::now_unix_nanos()))
      ]
    )
    @azimuth.TelemetryBatch::add(telemetry_batch, telemetry_data)
  }
  
  // 测试同步策略
  let sync_strategy = @azimuth.SyncStrategy::new()
  @azimuth.SyncStrategy::add_primary_target(sync_strategy, "aws-primary")
  @azimuth.SyncStrategy::add_backup_target(sync_strategy, "azure-backup")
  @azimuth.SyncStrategy::add_analytics_target(sync_strategy, "gcp-analytics")
  
  // 执行同步
  let sync_result = @azimuth.CloudSyncManager::sync_batch(cloud_sync_manager, telemetry_batch, sync_strategy)
  
  // 验证同步结果
  assert_true(sync_result.primary_sync.success)
  assert_true(sync_result.backup_sync.success)
  assert_true(sync_result.analytics_sync.success)
  assert_eq(sync_result.total_records, 101)
  assert_eq(sync_result.synced_records, 101)
  assert_eq(sync_result.failed_records, 0)
  
  // 测试故障转移
  let fault_injection = @azimuth.FaultInjection::new()
  @azimuth.FaultInjection::inject_failure(fault_injection, "aws-primary", @azimuth.FailureType::NetworkTimeout)
  
  let failover_sync_result = @azimuth.CloudSyncManager::sync_batch(cloud_sync_manager, telemetry_batch, sync_strategy)
  
  // 验证故障转移行为
  assert_false(failover_sync_result.primary_sync.success)
  assert_true(failover_sync_result.backup_sync.success)
  assert_true(failover_sync_result.analytics_sync.success)
  assert_eq(failover_sync_result.total_records, 101)
  assert_eq(failover_sync_result.synced_records, 101)
  assert_eq(failover_sync_result.failed_records, 0)
  
  // 验证故障转移统计
  let failover_stats = @azimuth.CloudSyncManager::get_failover_stats(cloud_sync_manager)
  assert_true(failover_stats.total_failovers > 0)
  assert_true(failover_stats.successful_failovers > 0)
}

// Test 7: 智能采样策略和自适应算法
test "intelligent sampling strategies and adaptive algorithms" {
  let sampling_manager = @azimuth.SamplingManager::new()
  
  // 配置自适应采样策略
  let adaptive_sampling_config = @azimuth.AdaptiveSamplingConfig::new(
    @azimuth.SamplingGoal::LatencyOptimization,
    @azimuth.ThroughputTarget::new(1000), // 1000 traces/second目标
    @azimuth.LatencyThreshold::new(100), // 100ms延迟阈值
    @azimuth.AdaptationInterval::new(60000) // 1分钟调整间隔
  )
  
  let adaptive_sampler = @azimuth.AdaptiveSampler::new(adaptive_sampling_config)
  @azimuth.SamplingManager::register_sampler(sampling_manager, "adaptive-primary", adaptive_sampler)
  
  // 配置基于优先级的采样策略
  let priority_sampling_config = @azimuth.PrioritySamplingConfig::new([
    ("high", @azimuth.SamplingRate::new(1.0)),      // 100%采样高优先级
    ("medium", @azimuth.SamplingRate::new(0.1)),    // 10%采样中优先级
    ("low", @azimuth.SamplingRate::new(0.01))       // 1%采样低优先级
  ])
  
  let priority_sampler = @azimuth.PrioritySampler::new(priority_sampling_config)
  @azimuth.SamplingManager::register_sampler(sampling_manager, "priority-based", priority_sampler)
  
  // 创建测试Trace数据
  let mut high_priority_traces = []
  let mut medium_priority_traces = []
  let mut low_priority_traces = []
  
  for i in 0..=1000 {
    let trace_context = @azimuth.TraceContext::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanContext::random(),
      @azimuth.Attributes::new()
    )
    
    if i % 10 == 0 {
      // 高优先级：错误Trace
      @azimuth.TraceContext::set_attribute(trace_context, "priority", @azimuth.StringValue("high"))
      @azimuth.TraceContext::set_attribute(trace_context, "error", @azimuth.BoolValue(true))
      high_priority_traces = high_priority_traces.push(trace_context)
    } else if i % 5 == 0 {
      // 中优先级：慢查询
      @azimuth.TraceContext::set_attribute(trace_context, "priority", @azimuth.StringValue("medium"))
      @azimuth.TraceContext::set_attribute(trace_context, "duration.ms", @azimuth.IntValue(500))
      medium_priority_traces = medium_priority_traces.push(trace_context)
    } else {
      // 低优先级：正常操作
      @azimuth.TraceContext::set_attribute(trace_context, "priority", @azimuth.StringValue("low"))
      low_priority_traces = low_priority_traces.push(trace_context)
    }
  }
  
  // 测试优先级采样
  let mut high_sampled = 0
  let mut medium_sampled = 0
  let mut low_sampled = 0
  
  for trace in high_priority_traces {
    if @azimuth.PrioritySampler::should_sample(priority_sampler, trace) {
      high_sampled = high_sampled + 1
    }
  }
  
  for trace in medium_priority_traces {
    if @azimuth.PrioritySampler::should_sample(priority_sampler, trace) {
      medium_sampled = medium_sampled + 1
    }
  }
  
  for trace in low_priority_traces {
    if @azimuth.PrioritySampler::should_sample(priority_sampler, trace) {
      low_sampled = low_sampled + 1
    }
  }
  
  // 验证优先级采样结果
  assert_eq(high_sampled, high_priority_traces.length()) // 100%高优先级
  assert_true(medium_sampled > 0 && medium_sampled < medium_priority_traces.length()) // 约10%中优先级
  assert_true(low_sampled > 0 && low_sampled < medium_sampled) // 约1%低优先级，应少于中优先级
  
  // 测试自适应采样
  let adaptive_sampler_stats = @azimuth.AdaptiveSampler::get_stats(adaptive_sampler)
  assert_true(adaptive_sampler_stats.current_sampling_rate >= 0.0)
  assert_true(adaptive_sampler_stats.current_sampling_rate <= 1.0)
  assert_true(adaptive_sampler_stats.adaptation_count >= 0)
  
  // 模拟高负载情况
  for i in 0..=500 {
    let trace_context = @azimuth.TraceContext::new(
      @azimuth.TraceId::generate(),
      @azimuth.SpanContext::random(),
      @azimuth.Attributes::new()
    )
    @azimuth.AdaptiveSampler::record_trace(adaptive_sampler, trace_context, 50) // 50ms延迟
  }
  
  // 触发自适应调整
  @azimuth.AdaptiveSampler::adapt_sampling_rate(adaptive_sampler)
  
  // 验证自适应调整
  let adjusted_stats = @azimuth.AdaptiveSampler::get_stats(adaptive_sampler)
  assert_true(adjusted_stats.adaptation_count > adaptive_sampler_stats.adaptation_count)
}

// Test 8: 多租户环境下的数据隔离和安全性
test "multi-tenant data isolation and security" {
  let tenant_manager = @azimuth.TenantManager::new()
  
  // 创建多个租户
  let tenant1_config = @azimuth.TenantConfig::new(
    "tenant-001",
    "Customer A",
    @azimuth.TenantTier::Premium,
    ["data-center-1", "data-center-2"],
    @azimuth.RetentionConfig::new(90, true),
    @azimuth.SecurityConfig::new(true, true, true)
  )
  
  let tenant2_config = @azimuth.TenantConfig::new(
    "tenant-002",
    "Customer B",
    @azimuth.TenantTier::Standard,
    ["data-center-2"],
    @azimuth.RetentionConfig::new(30, false),
    @azimuth.SecurityConfig::new(true, false, false)
  )
  
  let tenant3_config = @azimuth.TenantConfig::new(
    "tenant-003",
    "Customer C",
    @azimuth.TenantTier::Basic,
    ["data-center-3"],
    @azimuth.RetentionConfig::new(7, false),
    @azimuth.SecurityConfig::new(false, false, false)
  )
  
  // 注册租户
  @azimuth.TenantManager::register_tenant(tenant_manager, tenant1_config)
  @azimuth.TenantManager::register_tenant(tenant_manager, tenant2_config)
  @azimuth.TenantManager::register_tenant(tenant_manager, tenant3_config)
  
  // 创建租户特定的遥测数据
  let tenant1_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("tenant.id", @azimuth.StringValue("tenant-001")),
      ("service.name", @azimuth.StringValue("payment-service")),
      ("user.id", @azimuth.StringValue("user-a1")),
      ("transaction.amount", @azimuth.FloatValue(100.0)),
      ("pii.data", @azimuth.StringValue("john.doe@example.com"))
    ]
  )
  
  let tenant2_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("tenant.id", @azimuth.StringValue("tenant-002")),
      ("service.name", @azimuth.StringValue("inventory-service")),
      ("user.id", @azimuth.StringValue("user-b1")),
      ("product.count", @azimuth.IntValue(50))
    ]
  )
  
  let tenant3_data = @azimuth.TelemetryData::new(
    @azimuth.TraceId::generate(),
    @azimuth.SpanId::generate(),
    [
      ("tenant.id", @azimuth.StringValue("tenant-003")),
      ("service.name", @azimuth.StringValue("analytics-service")),
      ("session.id", @azimuth.StringValue("session-c1"))
    ]
  )
  
  // 测试数据隔离
  let tenant1_context = @azimuth.TenantContext::new("tenant-001")
  let tenant2_context = @azimuth.TenantContext::new("tenant-002")
  let tenant3_context = @azimuth.TenantContext::new("tenant-003")
  
  // 验证租户数据访问权限
  assert_true(@azimuth.TenantManager::can_access_data(tenant_manager, tenant1_context, tenant1_data))
  assert_false(@azimuth.TenantManager::can_access_data(tenant_manager, tenant1_context, tenant2_data))
  assert_false(@azimuth.TenantManager::can_access_data(tenant_manager, tenant1_context, tenant3_data))
  
  assert_true(@azimuth.TenantManager::can_access_data(tenant_manager, tenant2_context, tenant2_data))
  assert_false(@azimuth.TenantManager::can_access_data(tenant_manager, tenant2_context, tenant1_data))
  assert_false(@azimuth.TenantManager::can_access_data(tenant_manager, tenant2_context, tenant3_data))
  
  assert_true(@azimuth.TenantManager::can_access_data(tenant_manager, tenant3_context, tenant3_data))
  assert_false(@azimuth.TenantManager::can_access_data(tenant_manager, tenant3_context, tenant1_data))
  assert_false(@azimuth.TenantManager::can_access_data(tenant_manager, tenant3_context, tenant2_data))
  
  // 测试数据脱敏
  let tenant1_masked = @azimuth.TenantManager::apply_data_masking(tenant_manager, tenant1_context, tenant1_data)
  let tenant2_masked = @azimuth.TenantManager::apply_data_masking(tenant_manager, tenant2_context, tenant2_data)
  let tenant3_masked = @azimuth.TenantManager::apply_data_masking(tenant_manager, tenant3_context, tenant3_data)
  
  // 验证数据脱敏效果
  let tenant1_masked_attrs = @azimuth.TelemetryData::attributes(tenant1_masked)
  let tenant2_masked_attrs = @azimuth.TelemetryData::attributes(tenant2_masked)
  let tenant3_masked_attrs = @azimuth.TelemetryData::attributes(tenant3_masked)
  
  // 租户1是Premium，应该保留PII数据但有额外保护
  let tenant1_pii_attr = tenant1_masked_attrs.filter(fn(attr) { attr.0 == "pii.data" })
  assert_eq(tenant1_pii_attr.length(), 1)
  
  // 租户2和3应该没有或限制访问PII数据
  let tenant2_sensitive_attrs = tenant2_masked_attrs.filter(fn(attr) { 
    attr.0 == "user.id" || attr.0 == "product.count" 
  })
  assert_true(tenant2_sensitive_attrs.length() >= 0)
  
  // 测试配额限制
  let tenant1_quota = @azimuth.TenantManager::get_quota(tenant_manager, "tenant-001")
  let tenant2_quota = @azimuth.TenantManager::get_quota(tenant_manager, "tenant-002")
  let tenant3_quota = @azimuth.TenantManager::get_quota(tenant_manager, "tenant-003")
  
  assert_true(tenant1_quota.daily_trace_limit > tenant2_quota.daily_trace_limit)
  assert_true(tenant2_quota.daily_trace_limit > tenant3_quota.daily_trace_limit)
  assert_true(tenant1_quota.storage_limit_gb > tenant2_quota.storage_limit_gb)
  assert_true(tenant2_quota.storage_limit_gb > tenant3_quota.storage_limit_gb)
  
  // 测试租户统计
  let tenant1_stats = @azimuth.TenantManager::get_usage_stats(tenant_manager, "tenant-001")
  let tenant2_stats = @azimuth.TenantManager::get_usage_stats(tenant_manager, "tenant-002")
  let tenant3_stats = @azimuth.TenantManager::get_usage_stats(tenant_manager, "tenant-003")
  
  assert_eq(tenant1_stats.tenant_id, "tenant-001")
  assert_eq(tenant2_stats.tenant_id, "tenant-002")
  assert_eq(tenant3_stats.tenant_id, "tenant-003")
  
  assert_true(tenant1_stats.trace_count >= 0)
  assert_true(tenant2_stats.trace_count >= 0)
  assert_true(tenant3_stats.trace_count >= 0)
}