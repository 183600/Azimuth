// Azimuth Comprehensive Real-time Stream Processing Integrity Test Suite
// 综合实时流处理完整性测试套件

// Test 1: 高吞吐量实时流处理完整性验证
test "high-throughput real-time stream processing integrity" {
  let stream_processor = @azimuth.RealtimeStreamProcessor::new()
  
  // 配置流处理器
  let stream_config = @azimuth.StreamProcessorConfig::new()
  @azimuth.StreamProcessorConfig::set_buffer_size(stream_config, 10000)
  @azimuth.StreamProcessorConfig::set_batch_size(stream_config, 100)
  @azimuth.StreamProcessorConfig::set_processing_timeout(stream_config, 5000000000) // 5秒
  @azimuth.StreamProcessorConfig::enable_checkpointing(stream_config, true)
  @azimuth.StreamProcessorConfig::set_checkpoint_interval(stream_config, 1000000000) // 1秒
  @azimuth.StreamProcessorConfig::enable_exactly_once_processing(stream_config, true)
  
  @azimuth.RealtimeStreamProcessor::configure(stream_processor, stream_config)
  
  // 创建完整性验证器
  let integrity_validator = @azimuth.StreamIntegrityValidator::new()
  @azimuth.StreamIntegrityValidator::enable_sequence_validation(integrity_validator, true)
  @azimuth.StreamIntegrityValidator::enable_duplicate_detection(integrity_validator, true)
  @azimuth.StreamIntegrityValidator::enable_data_validation(integrity_validator, true)
  
  // 生成高频率测试数据流
  let stream_generator = @azimuth.TelemetryStreamGenerator::new()
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  // 配置数据流生成器
  @azimuth.TelemetryStreamGenerator::set_rate(stream_generator, 1000) // 1000条/秒
  @azimuth.TelemetryStreamGenerator::set_duration(stream_generator, 10000000000) // 10秒
  @azimuth.TelemetryStreamGenerator::set_data_pattern(stream_generator, @azimuth.DataPattern::Mixed)
  
  // 启动流处理
  @azimuth.RealtimeStreamProcessor::start(stream_processor)
  
  // 注册完整性验证器
  @azimuth.RealtimeStreamProcessor::register_validator(stream_processor, integrity_validator)
  
  // 生成并发送数据流
  let mut sent_data = []
  let mut sequence_numbers = []
  
  for i in 0..=10000 {
    let timestamp = base_timestamp + i * 1000000 // 1ms间隔
    let sequence_number = i
    
    // 创建带有序列号的遥测数据
    let attributes = [
      ("sequence.number", @azimuth.IntValue(sequence_number)),
      ("stream.id", @azimuth.StringValue("test-stream-001")),
      ("source.service", @azimuth.StringValue("data-generator")),
      ("data.type", @azimuth.StringValue(["metric", "log", "trace"][i % 3])),
      ("generation.timestamp", @azimuth.IntValue(timestamp))
    ]
    
    let telemetry_data = match i % 3 {
      0 => {
        let metric_point = @azimuth.MetricPoint::new(timestamp, "test.metric", i.to_float(), attributes)
        @azimuth.TelemetryData::Metric(metric_point)
      }
      1 => {
        let log_record = @azimuth.LogRecord::new(timestamp, @azimuth.Severity::Info, "Test log message " + i.to_string(), attributes)
        @azimuth.TelemetryData::Log(log_record)
      }
      _ => {
        let span_data = @azimuth.SpanData::new(
          @azimuth.SpanContext::new(@azimuth.TraceId::new(), @azimuth.SpanId::new(), []),
          "test.operation",
          timestamp,
          timestamp + 1000000,
          @azimuth.SpanKind::Internal,
          attributes
        )
        @azimuth.TelemetryData::Span(span_data)
      }
    }
    
    sent_data = sent_data.push(telemetry_data)
    sequence_numbers = sequence_numbers.push(sequence_number)
    
    // 发送到流处理器
    @azimuth.RealtimeStreamProcessor::process_data(stream_processor, telemetry_data)
  }
  
  // 等待处理完成
  @azimuth.RealtimeStreamProcessor::flush(stream_processor)
  
  // 获取处理结果
  let processed_data = @azimuth.RealtimeStreamProcessor::get_processed_data(stream_processor)
  let integrity_report = @azimuth.StreamIntegrityValidator::get_report(integrity_validator)
  
  // 验证数据完整性
  assert_eq(processed_data.length(), sent_data.length()) // 所有数据都应该被处理
  
  // 验证序列完整性
  let processed_sequences = processed_data.map(fn(data) {
    match data {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        let seq_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "sequence.number" })
        match seq_attr {
          Some(attr) => {
            match @azimuth.Attribute::value(attr) {
              @azimuth.IntValue(seq) => seq
              _ => -1
            }
          }
          None => -1
        }
      }
      @azimuth.TelemetryData::Log(log) => {
        let attributes = @azimuth.LogRecord::attributes(log)
        let seq_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "sequence.number" })
        match seq_attr {
          Some(attr) => {
            match @azimuth.Attribute::value(attr) {
              @azimuth.IntValue(seq) => seq
              _ => -1
            }
          }
          None => -1
        }
      }
      @azimuth.TelemetryData::Span(span) => {
        let attributes = @azimuth.SpanData::attributes(span)
        let seq_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "sequence.number" })
        match seq_attr {
          Some(attr) => {
            match @azimuth.Attribute::value(attr) {
              @azimuth.IntValue(seq) => seq
              _ => -1
            }
          }
          None => -1
        }
      }
    }
  })
  
  // 验证序列号连续性
  let sorted_sequences = @azimuth.Array::sort(processed_sequences)
  for i in 1..sorted_sequences.length() - 1 {
    assert_eq(sorted_sequences[i], sorted_sequences[i-1] + 1)
  }
  
  // 验证完整性报告
  assert_eq(@azimuth.IntegrityReport::total_processed(integrity_report), sent_data.length())
  assert_eq(@azimuth.IntegrityReport::duplicate_count(integrity_report), 0)
  assert_eq(@azimuth.IntegrityReport::missing_count(integrity_report), 0)
  assert_eq(@azimuth.IntegrityReport::validation_errors(integrity_report), 0)
  
  // 停止流处理器
  @azimuth.RealtimeStreamProcessor::stop(stream_processor)
  
  assert_true(true)
}

// Test 2: 故障恢复和状态一致性测试
test "failure recovery and state consistency" {
  let fault_tolerant_processor = @azimuth.FaultTolerantStreamProcessor::new()
  
  // 配置容错流处理器
  let fault_config = @azimuth.FaultToleranceConfig::new()
  @azimuth.FaultToleranceConfig::enable_state_persistence(fault_config, true)
  @azimuth.FaultToleranceConfig::set_persistence_interval(fault_config, 500000000) // 0.5秒
  @azimuth.FaultToleranceConfig::enable_auto_recovery(fault_config, true)
  @azimuth.FaultToleranceConfig::set_max_retry_attempts(fault_config, 3)
  @azimuth.FaultToleranceConfig::set_retry_delay(fault_config, 100000000) // 0.1秒
  
  @azimuth.FaultTolerantStreamProcessor::configure(fault_tolerant_processor, fault_config)
  
  // 创建状态管理器
  let state_manager = @azimuth.StreamStateManager::new()
  @azimuth.StreamStateManager::enable_checkpointing(state_manager, true)
  @azimuth.StreamStateManager::set_checkpoint_directory(state_manager, "/tmp/checkpoints")
  
  // 注册状态管理器
  @azimuth.FaultTolerantStreamProcessor::register_state_manager(fault_tolerant_processor, state_manager)
  
  // 启动处理器
  @azimuth.FaultTolerantStreamProcessor::start(fault_tolerant_processor)
  
  // 生成测试数据流
  let mut test_data = []
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  for i in 0..=5000 {
    let timestamp = base_timestamp + i * 2000000 // 2ms间隔
    
    let attributes = [
      ("batch.id", @azimuth.IntValue(i / 100)), // 每100条数据一个批次
      ("item.id", @azimuth.IntValue(i)),
      ("processing.status", @azimuth.StringValue("pending")),
      ("data.checksum", @azimuth.StringValue("checksum-" + i.to_string()))
    ]
    
    let metric_point = @azimuth.MetricPoint::new(timestamp, "fault.test.metric", i.to_float(), attributes)
    test_data = test_data.push(@azimuth.TelemetryData::Metric(metric_point))
    
    // 处理数据
    @azimuth.FaultTolerantStreamProcessor::process_data(fault_tolerant_processor, @azimuth.TelemetryData::Metric(metric_point))
    
    // 模拟故障（每处理1000条数据后）
    if i > 0 && i % 1000 == 0 {
      // 获取当前状态
      let current_state = @azimuth.StreamStateManager::get_current_state(state_manager)
      let processed_count = @azimuth.StreamState::processed_count(current_state)
      
      // 模拟处理器故障
      @azimuth.FaultTolerantStreamProcessor::simulate_failure(fault_tolerant_processor)
      
      // 验证处理器已停止
      assert_false(@azimuth.FaultTolerantStreamProcessor::is_running(fault_tolerant_processor))
      
      // 从故障中恢复
      @azimuth.FaultTolerantStreamProcessor::recover(fault_tolerant_processor)
      
      // 验证处理器已恢复
      assert_true(@azimuth.FaultTolerantStreamProcessor::is_running(fault_tolerant_processor))
      
      // 验证状态一致性
      let recovered_state = @azimuth.StreamStateManager::get_current_state(state_manager)
      let recovered_count = @azimuth.StreamState::processed_count(recovered_state)
      
      assert_eq(recovered_count, processed_count) // 状态应该一致
    }
  }
  
  // 等待所有数据处理完成
  @azimuth.FaultTolerantStreamProcessor::flush(fault_tolerant_processor)
  
  // 获取最终处理结果
  let final_processed_data = @azimuth.FaultTolerantStreamProcessor::get_processed_data(fault_tolerant_processor)
  let final_state = @azimuth.StreamStateManager::get_current_state(state_manager)
  
  // 验证数据完整性
  assert_eq(final_processed_data.length(), test_data.length())
  assert_eq(@azimuth.StreamState::processed_count(final_state), test_data.length())
  
  // 验证数据顺序和一致性
  for i in 0..final_processed_data.length() - 1 {
    match final_processed_data[i] {
      @azimuth.TelemetryData::Metric(metric) => {
        let attributes = @azimuth.MetricPoint::attributes(metric)
        let batch_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "batch.id" })
        let item_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "item.id" })
        
        match (batch_attr, item_attr) {
          (Some(batch), Some(item)) => {
            match (@azimuth.Attribute::value(batch), @azimuth.Attribute::value(item)) {
              (@azimuth.IntValue(batch_id), @azimuth.IntValue(item_id)) => {
                assert_eq(batch_id, i / 100)
                assert_eq(item_id, i)
              }
              _ => assert_true(false)
            }
          }
          _ => assert_true(false)
        }
      }
      _ => assert_true(false)
    }
  }
  
  // 停止处理器
  @azimuth.FaultTolerantStreamProcessor::stop(fault_tolerant_processor)
  
  assert_true(true)
}

// Test 3: 背压处理和流量控制测试
test "backpressure handling and flow control" {
  let backpressure_processor = @azimuth.BackpressureAwareStreamProcessor::new()
  
  // 配置背压处理器
  let backpressure_config = @azimuth.BackpressureConfig::new()
  @azimuth.BackpressureConfig::set_max_buffer_size(backpressure_config, 5000)
  @azimuth.BackpressureConfig::set_high_watermark(backpressure_config, 4000) // 80%
  @azimuth.BackpressureConfig::set_low_watermark(backpressure_config, 1000) // 20%
  @azimuth.BackpressureConfig::enable_adaptive_throttling(backpressure_config, true)
  @azimuth.BackpressureConfig::set_throttling_factor(backpressure_config, 0.5) // 50%节流
  
  @azimuth.BackpressureAwareStreamProcessor::configure(backpressure_processor, backpressure_config)
  
  // 创建流量监控器
  let flow_monitor = @azimuth.FlowMonitor::new()
  @azimuth.FlowMonitor::enable_input_rate_monitoring(flow_monitor, true)
  @azimuth.FlowMonitor::enable_output_rate_monitoring(flow_monitor, true)
  @azimuth.FlowMonitor::enable_buffer_utilization_monitoring(flow_monitor, true)
  
  // 注册流量监控器
  @azimuth.BackpressureAwareStreamProcessor::register_flow_monitor(backpressure_processor, flow_monitor)
  
  // 启动处理器
  @azimuth.BackpressureAwareStreamProcessor::start(backpressure_processor)
  
  // 模拟高输入速率场景
  let high_rate_generator = @azimuth.TelemetryStreamGenerator::new()
  @azimuth.TelemetryStreamGenerator::set_rate(high_rate_generator, 2000) // 2000条/秒
  @azimuth.TelemetryStreamGenerator::set_burst_pattern(high_rate_generator, true)
  @azimuth.TelemetryStreamGenerator::set_burst_size(high_rate_generator, 500)
  @azimuth.TelemetryStreamGenerator::set_burst_interval(high_rate_generator, 1000000000) // 1秒
  
  // 模拟慢消费者
  let slow_consumer = @azimuth.SlowTelemetryConsumer::new()
  @azimuth.SlowTelemetryConsumer::set_processing_delay(slow_consumer, 500000) // 0.5ms处理延迟
  
  // 注册消费者
  @azimuth.BackpressureAwareStreamProcessor::register_consumer(backpressure_processor, slow_consumer)
  
  // 生成并发送数据
  let mut sent_count = 0
  let mut received_count = 0
  let mut backpressure_events = 0
  
  // 监控背压事件
  @azimuth.BackpressureAwareStreamProcessor::on_backpressure_triggered(backpressure_processor, fn() {
    backpressure_events = backpressure_events + 1
  })
  
  // 监控数据接收
  @azimuth.SlowTelemetryConsumer::on_data_received(slow_consumer, fn() {
    received_count = received_count + 1
  })
  
  // 运行测试场景
  let test_duration = 10000000000 // 10秒
  let start_time = @azimuth.Time::now_unix_nanos()
  
  while @azimuth.Time::now_unix_nanos() - start_time < test_duration {
    // 生成数据
    let telemetry_data = @azimuth.TelemetryStreamGenerator::generate_next(high_rate_generator)
    @azimuth.BackpressureAwareStreamProcessor::process_data(backpressure_processor, telemetry_data)
    sent_count = sent_count + 1
    
    // 短暂休眠
    @azimuth.Time::sleep(100000) // 0.1ms
  }
  
  // 等待所有数据处理完成
  @azimuth.BackpressureAwareStreamProcessor::flush(backpressure_processor)
  
  // 获取流量监控报告
  let flow_report = @azimuth.FlowMonitor::get_report(flow_monitor)
  
  // 验证背压处理效果
  assert_true(backpressure_events > 0) // 应该触发背压事件
  
  // 验证流量控制效果
  let avg_input_rate = @azimuth.FlowReport::average_input_rate(flow_report)
  let avg_output_rate = @azimuth.FlowReport::average_output_rate(flow_report)
  let peak_buffer_utilization = @azimuth.FlowReport::peak_buffer_utilization(flow_report)
  
  assert_true(avg_output_rate <= avg_input_rate) // 输出速率应该不超过输入速率
  assert_true(peak_buffer_utilization <= 100) // 缓冲区利用率不应该超过100%
  
  // 验证数据完整性
  let processed_data = @azimuth.BackpressureAwareStreamProcessor::get_processed_data(backpressure_processor)
  assert_true(processed_data.length() > 0)
  assert_true(processed_data.length() <= sent_count) // 处理的数据量应该不超过发送的数据量
  
  // 验证数据顺序
  for i in 1..processed_data.length() - 1 {
    // 简单验证数据顺序（基于时间戳）
    match (processed_data[i-1], processed_data[i]) {
      (@azimuth.TelemetryData::Metric(prev_metric), @azimuth.TelemetryData::Metric(curr_metric)) => {
        let prev_timestamp = @azimuth.MetricPoint::timestamp(prev_metric)
        let curr_timestamp = @azimuth.MetricPoint::timestamp(curr_metric)
        assert_true(curr_timestamp >= prev_timestamp)
      }
      _ => assert_true(false)
    }
  }
  
  // 停止处理器
  @azimuth.BackpressureAwareStreamProcessor::stop(backpressure_processor)
  
  assert_true(true)
}

// Test 4: 多流合并和分流处理测试
test "multi-stream merge and split processing" {
  let multi_stream_processor = @azimuth.MultiStreamProcessor::new()
  
  // 配置多流处理器
  let multi_stream_config = @azimuth.MultiStreamConfig::new()
  @azimuth.MultiStreamConfig::set_max_input_streams(multi_stream_config, 5)
  @azimuth.MultiStreamConfig::set_max_output_streams(multi_stream_config, 3)
  @azimuth.MultiStreamConfig::enable_stream_routing(multi_stream_config, true)
  @azimuth.MultiStreamConfig::enable_stream_joining(multi_stream_config, true)
  
  @azimuth.MultiStreamProcessor::configure(multi_stream_processor, multi_stream_config)
  
  // 创建流路由器
  let stream_router = @azimuth.StreamRouter::new()
  
  // 定义路由规则
  @azimuth.StreamRouter::add_rule(stream_router, "metric-stream", fn(data) {
    match data {
      @azimuth.TelemetryData::Metric(_) => true
      _ => false
    }
  })
  
  @azimuth.StreamRouter::add_rule(stream_router, "log-stream", fn(data) {
    match data {
      @azimuth.TelemetryData::Log(_) => true
      _ => false
    }
  })
  
  @azimuth.StreamRouter::add_rule(stream_router, "trace-stream", fn(data) {
    match data {
      @azimuth.TelemetryData::Span(_) => true
      _ => false
    }
  })
  
  // 注册路由器
  @azimuth.MultiStreamProcessor::register_router(multi_stream_processor, stream_router)
  
  // 创建流合并器
  let stream_joiner = @azimuth.StreamJoiner::new()
  @azimuth.StreamJoiner::set_join_strategy(stream_joiner, @azimuth.JoinStrategy::TimeBased)
  @azimuth.StreamJoiner::set_time_window(stream_joiner, 1000000000) // 1秒时间窗口
  
  // 注册合并器
  @azimuth.MultiStreamProcessor::register_joiner(multi_stream_processor, stream_joiner)
  
  // 启动处理器
  @azimuth.MultiStreamProcessor::start(multi_stream_processor)
  
  // 创建多个输入流
  let input_stream1 = @azimuth.TelemetryInputStream::new("input-stream-1")
  let input_stream2 = @azimuth.TelemetryInputStream::new("input-stream-2")
  let input_stream3 = @azimuth.TelemetryInputStream::new("input-stream-3")
  
  // 注册输入流
  @azimuth.MultiStreamProcessor::register_input_stream(multi_stream_processor, input_stream1)
  @azimuth.MultiStreamProcessor::register_input_stream(multi_stream_processor, input_stream2)
  @azimuth.MultiStreamProcessor::register_input_stream(multi_stream_processor, input_stream3)
  
  // 创建输出流
  let output_stream1 = @azimuth.TelemetryOutputStream::new("metric-output")
  let output_stream2 = @azimuth.TelemetryOutputStream::new("log-output")
  let output_stream3 = @azimuth.TelemetryOutputStream::new("trace-output")
  
  // 注册输出流
  @azimuth.MultiStreamProcessor::register_output_stream(multi_stream_processor, output_stream1)
  @azimuth.MultiStreamProcessor::register_output_stream(multi_stream_processor, output_stream2)
  @azimuth.MultiStreamProcessor::register_output_stream(multi_stream_processor, output_stream3)
  
  // 生成并发送多流数据
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  for i in 0..=3000 {
    let timestamp = base_timestamp + i * 1000000 // 1ms间隔
    
    // 根据索引选择不同的输入流
    let input_stream = match i % 3 {
      0 => input_stream1
      1 => input_stream2
      _ => input_stream3
    }
    
    // 生成不同类型的遥测数据
    let telemetry_data = match i % 3 {
      0 => {
        let attributes = [
          ("stream.source", @azimuth.StringValue("stream-" + (i % 3).to_string())),
          ("data.category", @azimuth.StringValue("performance")),
          ("metric.type", @azimuth.StringValue("counter"))
        ]
        let metric_point = @azimuth.MetricPoint::new(timestamp, "test.metric", i.to_float(), attributes)
        @azimuth.TelemetryData::Metric(metric_point)
      }
      1 => {
        let attributes = [
          ("stream.source", @azimuth.StringValue("stream-" + (i % 3).to_string())),
          ("log.level", @azimuth.StringValue(["INFO", "WARN", "ERROR"][i % 3])),
          ("log.category", @azimuth.StringValue("application"))
        ]
        let log_record = @azimuth.LogRecord::new(timestamp, @azimuth.Severity::Info, "Test log " + i.to_string(), attributes)
        @azimuth.TelemetryData::Log(log_record)
      }
      _ => {
        let attributes = [
          ("stream.source", @azimuth.StringValue("stream-" + (i % 3).to_string())),
          ("span.type", @azimuth.StringValue("http")),
          ("span.category", @azimuth.StringValue("request"))
        ]
        let span_data = @azimuth.SpanData::new(
          @azimuth.SpanContext::new(@azimuth.TraceId::new(), @azimuth.SpanId::new(), []),
          "test.span",
          timestamp,
          timestamp + 500000,
          @azimuth.SpanKind::Server,
          attributes
        )
        @azimuth.TelemetryData::Span(span_data)
      }
    }
    
    // 发送到对应的输入流
    @azimuth.TelemetryInputStream::send(input_stream, telemetry_data)
  }
  
  // 等待处理完成
  @azimuth.MultiStreamProcessor::flush(multi_stream_processor)
  
  // 获取输出流数据
  let metric_output_data = @azimuth.TelemetryOutputStream::get_data(output_stream1)
  let log_output_data = @azimuth.TelemetryOutputStream::get_data(output_stream2)
  let trace_output_data = @azimuth.TelemetryOutputStream::get_data(output_stream3)
  
  // 验证分流效果
  assert_true(metric_output_data.length() > 0)
  assert_true(log_output_data.length() > 0)
  assert_true(trace_output_data.length() > 0)
  
  // 验证路由正确性
  for data in metric_output_data {
    assert_type(data, @azimuth.TelemetryData::Metric)
  }
  
  for data in log_output_data {
    assert_type(data, @azimuth.TelemetryData::Log)
  }
  
  for data in trace_output_data {
    assert_type(data, @azimuth.TelemetryData::Span)
  }
  
  // 验证数据完整性
  let total_output = metric_output_data.length() + log_output_data.length() + trace_output_data.length()
  assert_eq(total_output, 3001) // 所有数据都应该被路由到某个输出流
  
  // 验证多流合并效果
  let joined_streams = @azimuth.MultiStreamProcessor::get_joined_streams(multi_stream_processor)
  assert_true(joined_streams.length() > 0)
  
  // 停止处理器
  @azimuth.MultiStreamProcessor::stop(multi_stream_processor)
  
  assert_true(true)
}

// Test 5: 实时流处理延迟和性能测试
test "real-time stream processing latency and performance" {
  let performance_processor = @azimuth.PerformanceOptimizedStreamProcessor::new()
  
  // 配置性能优化处理器
  let perf_config = @azimuth.PerformanceConfig::new()
  @azimuth.PerformanceConfig::enable_parallel_processing(perf_config, true)
  @azimuth.PerformanceConfig::set_worker_threads(perf_config, 4)
  @azimuth.PerformanceConfig::enable_batch_processing(perf_config, true)
  @azimuth.PerformanceConfig::set_batch_size(perf_config, 50)
  @azimuth.PerformanceConfig::enable_zero_copy(perf_config, true)
  @azimuth.PerformanceConfig::enable_memory_pooling(perf_config, true)
  
  @azimuth.PerformanceOptimizedStreamProcessor::configure(performance_processor, perf_config)
  
  // 创建性能监控器
  let perf_monitor = @azimuth.PerformanceMonitor::new()
  @azimuth.PerformanceMonitor::enable_latency_tracking(perf_monitor, true)
  @azimuth.PerformanceMonitor::enable_throughput_tracking(perf_monitor, true)
  @azimuth.PerformanceMonitor::enable_memory_usage_tracking(perf_monitor, true)
  @azimuth.PerformanceMonitor::enable_cpu_usage_tracking(perf_monitor, true)
  
  // 注册性能监控器
  @azimuth.PerformanceOptimizedStreamProcessor::register_performance_monitor(performance_processor, perf_monitor)
  
  // 启动处理器
  @azimuth.PerformanceOptimizedStreamProcessor::start(performance_processor)
  
  // 创建延迟测试数据
  let mut latency_samples = []
  let base_timestamp = @azimuth.Time::now_unix_nanos()
  
  for i in 0..=5000 {
    let send_time = @azimuth.Time::now_unix_nanos()
    
    let attributes = [
      ("send.time", @azimuth.IntValue(send_time)),
      ("message.id", @azimuth.IntValue(i)),
      ("test.type", @azimuth.StringValue("latency-test"))
    ]
    
    let metric_point = @azimuth.MetricPoint::new(send_time, "latency.test.metric", i.to_float(), attributes)
    let telemetry_data = @azimuth.TelemetryData::Metric(metric_point)
    
    // 处理数据
    @azimuth.PerformanceOptimizedStreamProcessor::process_data(performance_processor, telemetry_data)
    
    // 记录发送时间
    latency_samples = latency_samples.push((i, send_time))
  }
  
  // 等待所有数据处理完成
  @azimuth.PerformanceOptimizedStreamProcessor::flush(performance_processor)
  
  // 获取处理结果和性能报告
  let processed_data = @azimuth.PerformanceOptimizedStreamProcessor::get_processed_data(performance_processor)
  let performance_report = @azimuth.PerformanceMonitor::get_report(perf_monitor)
  
  // 计算延迟统计
  let mut latencies = []
  
  for (message_id, send_time) in latency_samples {
    // 查找对应的处理结果
    match processed_data.find(fn(data) {
      match data {
        @azimuth.TelemetryData::Metric(metric) => {
          let attributes = @azimuth.MetricPoint::attributes(metric)
          let id_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "message.id" })
          match id_attr {
            Some(attr) => {
              match @azimuth.Attribute::value(attr) {
                @azimuth.IntValue(id) => id == message_id
                _ => false
              }
            }
            None => false
          }
        }
        _ => false
      }
    }) {
      Some(processed_metric) => {
        match processed_metric {
          @azimuth.TelemetryData::Metric(metric) => {
            let attributes = @azimuth.MetricPoint::attributes(metric)
            let process_time_attr = attributes.find(fn(attr) { @azimuth.Attribute::key(attr) == "process.time" })
            match process_time_attr {
              Some(attr) => {
                match @azimuth.Attribute::value(attr) {
                  @azimuth.IntValue(process_time) => {
                    latencies = latencies.push(process_time - send_time)
                  }
                  _ => assert_true(false)
                }
              }
              None => assert_true(false)
            }
          }
          _ => assert_true(false)
        }
      }
      None => assert_true(false)
    }
  }
  
  // 计算延迟统计指标
  let sorted_latencies = @azimuth.Array::sort(latencies)
  let min_latency = sorted_latencies[0]
  let max_latency = sorted_latencies[sorted_latencies.length() - 1]
  
  let avg_latency = latencies.fold_left(0, fn(acc, latency) { acc + latency }) / latencies.length()
  
  let p50_index = sorted_latencies.length() / 2
  let p95_index = (sorted_latencies.length() * 95) / 100
  let p99_index = (sorted_latencies.length() * 99) / 100
  
  let p50_latency = sorted_latencies[p50_index]
  let p95_latency = sorted_latencies[p95_index]
  let p99_latency = sorted_latencies[p99_index]
  
  // 验证延迟性能
  assert_true(avg_latency < 10000000) // 平均延迟小于10ms
  assert_true(p95_latency < 20000000) // P95延迟小于20ms
  assert_true(p99_latency < 50000000) // P99延迟小于50ms
  
  // 验证吞吐量性能
  let avg_throughput = @azimuth.PerformanceReport::average_throughput(performance_report)
  let peak_throughput = @azimuth.PerformanceReport::peak_throughput(performance_report)
  
  assert_true(avg_throughput > 1000) // 平均吞吐量大于1000条/秒
  assert_true(peak_throughput > 5000) // 峰值吞吐量大于5000条/秒
  
  // 验证资源使用效率
  let avg_memory_usage = @azimuth.PerformanceReport::average_memory_usage(performance_report)
  let peak_memory_usage = @azimuth.PerformanceReport::peak_memory_usage(performance_report)
  let avg_cpu_usage = @azimuth.PerformanceReport::average_cpu_usage(performance_report)
  
  assert_true(avg_memory_usage < 104857600) // 平均内存使用小于100MB
  assert_true(peak_memory_usage < 209715200) // 峰值内存使用小于200MB
  assert_true(avg_cpu_usage < 80) // 平均CPU使用率小于80%
  
  // 停止处理器
  @azimuth.PerformanceOptimizedStreamProcessor::stop(performance_processor)
  
  assert_true(true)
}