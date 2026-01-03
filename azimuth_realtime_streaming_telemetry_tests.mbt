// Azimuth Real-time Streaming Telemetry Tests
// 专注于实时遥测数据流处理、传输和消费的高级测试用例

test "实时遥测数据流创建和配置测试" {
  // 创建实时数据流配置
  let stream_config = azimuth::StreamConfig {
    stream_id: "stream-12345",
    name: "application-metrics-stream",
    description: "实时应用指标数据流",
    source_type: "application",
    buffer_size: 10000,
    batch_size: 100,
    flush_interval_ms: 1000,
    compression: "gzip",
    encryption: true,
    retry_policy: azimuth::RetryPolicy {
      max_retries: 3,
      backoff_ms: 1000,
      exponential_backoff: true
    }
  }
  
  // 验证流配置
  assert_eq(stream_config.stream_id, "stream-12345")
  assert_eq(stream_config.name, "application-metrics-stream")
  assert_eq(stream_config.buffer_size, 10000)
  assert_eq(stream_config.batch_size, 100)
  assert_eq(stream_config.flush_interval_ms, 1000)
  assert_eq(stream_config.compression, "gzip")
  assert_true(stream_config.encryption)
  assert_eq(stream_config.retry_policy.max_retries, 3)
  
  // 创建实时数据流
  let stream = azimuth::create_stream(stream_config)
  
  // 验证流创建成功
  assert_eq(stream.id, stream_config.stream_id)
  assert_eq(stream.status, "active")
  assert_eq(stream.buffer.capacity(), stream_config.buffer_size)
  assert_eq(stream.consumer_count, 0)
  
  // 验证流统计信息
  let stats = azimuth::get_stream_stats(stream.id)
  assert_eq(stats.total_messages, 0)
  assert_eq(stats.total_bytes, 0)
  assert_eq(stats.last_activity, None)
}

test "实时遥测数据流生产者测试" {
  // 创建数据流
  let stream_config = azimuth::StreamConfig {
    stream_id: "producer-test-stream",
    name: "producer-test",
    description: "生产者测试流",
    source_type: "test",
    buffer_size: 1000,
    batch_size: 10,
    flush_interval_ms: 500,
    compression: "none",
    encryption: false,
    retry_policy: azimuth::RetryPolicy {
      max_retries: 3,
      backoff_ms: 500,
      exponential_backoff: true
    }
  }
  
  let stream = azimuth::create_stream(stream_config)
  
  // 创建生产者
  let producer = azimuth::create_producer(stream.id)
  
  // 生成测试遥测数据
  let telemetry_data = [
    azimuth::TelemetryData {
      timestamp: 1634567890123,
      metric_name: "cpu.usage",
      metric_value: 45.6,
      tags: [("host", "server-1"), ("region", "us-east-1")]
    },
    azimuth::TelemetryData {
      timestamp: 1634567890124,
      metric_name: "memory.usage",
      metric_value: 78.2,
      tags: [("host", "server-1"), ("region", "us-east-1")]
    },
    azimuth::TelemetryData {
      timestamp: 1634567890125,
      metric_name: "disk.usage",
      metric_value: 62.3,
      tags: [("host", "server-1"), ("region", "us-east-1")]
    }
  ]
  
  // 发送遥测数据到流
  for data in telemetry_data {
    let result = azimuth::send_telemetry_data(producer, data)
    match result {
      azimuth::SendResult::Success(message_id) => assert_true(message_id.length() > 0)
      azimuth::SendResult::Failure(error) => assert_true(false, "Failed to send data: " + error)
      azimuth::SendResult::BufferFull => assert_true(false, "Buffer should not be full")
    }
  }
  
  // 验证流统计信息
  let stats = azimuth::get_stream_stats(stream.id)
  assert_eq(stats.total_messages, 3)
  assert_true(stats.total_bytes > 0)
  match stats.last_activity {
    Some(timestamp) => assert_true(timestamp > 1634567890000)
    None => assert_true(false)
  }
  
  // 验证缓冲区状态
  let buffer_stats = azimuth::get_buffer_stats(stream.id)
  assert_eq(buffer_stats.current_size, 3)
  assert_eq(buffer_stats.capacity, stream_config.buffer_size)
  assert_eq(buffer_stats.available, stream_config.buffer_size - 3)
}

test "实时遥测数据流消费者测试" {
  // 创建数据流
  let stream_config = azimuth::StreamConfig {
    stream_id: "consumer-test-stream",
    name: "consumer-test",
    description: "消费者测试流",
    source_type: "test",
    buffer_size: 100,
    batch_size: 5,
    flush_interval_ms: 100,
    compression: "none",
    encryption: false,
    retry_policy: azimuth::RetryPolicy {
      max_retries: 3,
      backoff_ms: 100,
      exponential_backoff: true
    }
  }
  
  let stream = azimuth::create_stream(stream_config)
  
  // 创建生产者并发送数据
  let producer = azimuth::create_producer(stream.id)
  
  let test_data = [
    azimuth::TelemetryData {
      timestamp: 1634567890123,
      metric_name: "request.count",
      metric_value: 100.0,
      tags: [("service", "api"), ("endpoint", "/users")]
    },
    azimuth::TelemetryData {
      timestamp: 1634567890124,
      metric_name: "response.time",
      metric_value: 250.5,
      tags: [("service", "api"), ("endpoint", "/users")]
    },
    azimuth::TelemetryData {
      timestamp: 1634567890125,
      metric_name: "error.rate",
      metric_value: 0.02,
      tags: [("service", "api"), ("endpoint", "/users")]
    }
  ]
  
  for data in test_data {
    azimuth::send_telemetry_data(producer, data)
  }
  
  // 创建消费者
  let consumer = azimuth::create_consumer(stream.id, "test-consumer-group")
  
  // 消费数据
  let consumed_data = azimuth::consume_messages(consumer, 3, 5000)  // 最多3条消息，超时5秒
  
  // 验证消费的数据
  assert_eq(consumed_data.length(), 3)
  
  // 验证数据完整性
  for i in 0..test_data.length() {
    let original = test_data[i]
    let consumed = consumed_data[i]
    
    assert_eq(consumed.timestamp, original.timestamp)
    assert_eq(consumed.metric_name, original.metric_name)
    assert_eq(consumed.metric_value, original.metric_value)
    assert_eq(consumed.tags.length(), original.tags.length())
  }
  
  // 验证消费者偏移量
  let offset = azimuth::get_consumer_offset(consumer)
  assert_true(offset > 0)
}

test "实时遥测数据流背压处理测试" {
  // 创建小缓冲区的流来测试背压
  let stream_config = azimuth::StreamConfig {
    stream_id: "backpressure-test-stream",
    name: "backpressure-test",
    description: "背压测试流",
    source_type: "test",
    buffer_size: 10,  // 小缓冲区
    batch_size: 5,
    flush_interval_ms: 1000,  // 长刷新间隔
    compression: "none",
    encryption: false,
    retry_policy: azimuth::RetryPolicy {
      max_retries: 3,
      backoff_ms: 100,
      exponential_backoff: true
    }
  }
  
  let stream = azimuth::create_stream(stream_config)
  let producer = azimuth::create_producer(stream.id)
  
  // 快速发送大量数据以触发背压
  let mut success_count = 0
  let mut buffer_full_count = 0
  let mut failure_count = 0
  
  for i in 0..20 {  // 发送20条消息，超过缓冲区容量
    let data = azimuth::TelemetryData {
      timestamp: 1634567890123 + i,
      metric_name: "test.metric",
      metric_value: i.to_float(),
      tags: [("index", i.to_string())]
    }
    
    let result = azimuth::send_telemetry_data(producer, data)
    match result {
      azimuth::SendResult::Success(_) => success_count = success_count + 1
      azimuth::SendResult::BufferFull => buffer_full_count = buffer_full_count + 1
      azimuth::SendResult::Failure(_) => failure_count = failure_count + 1
    }
  }
  
  // 验证背压处理
  assert_eq(success_count, 10)  // 只有缓冲区容量的消息成功
  assert_eq(buffer_full_count, 10)  // 剩余的消息触发背压
  assert_eq(failure_count, 0)  // 没有失败
  
  // 验证流统计
  let stats = azimuth::get_stream_stats(stream.id)
  assert_eq(stats.total_messages, 10)  // 只有成功发送的消息被计入
  
  // 验证缓冲区状态
  let buffer_stats = azimuth::get_buffer_stats(stream.id)
  assert_eq(buffer_stats.current_size, 10)  // 缓冲区满
  assert_eq(buffer_stats.available, 0)
}

test "实时遥测数据流故障恢复测试" {
  // 创建数据流
  let stream_config = azimuth::StreamConfig {
    stream_id: "failure-recovery-stream",
    name: "failure-recovery-test",
    description: "故障恢复测试流",
    source_type: "test",
    buffer_size: 100,
    batch_size: 10,
    flush_interval_ms: 100,
    compression: "none",
    encryption: false,
    retry_policy: azimuth::RetryPolicy {
      max_retries: 3,
      backoff_ms: 100,
      exponential_backoff: true
    }
  }
  
  let stream = azimuth::create_stream(stream_config)
  let producer = azimuth::create_producer(stream.id)
  
  // 发送一些数据
  let test_data = [
    azimuth::TelemetryData {
      timestamp: 1634567890123,
      metric_name: "test.metric.1",
      metric_value: 1.0,
      tags: []
    },
    azimuth::TelemetryData {
      timestamp: 1634567890124,
      metric_name: "test.metric.2",
      metric_value: 2.0,
      tags: []
    }
  ]
  
  for data in test_data {
    azimuth::send_telemetry_data(producer, data)
  }
  
  // 模拟流故障
  azimuth::simulate_stream_failure(stream.id)
  
  // 验证流状态变为故障
  let status = azimuth::get_stream_status(stream.id)
  assert_eq(status, "failed")
  
  // 尝试在故障状态下发送数据
  let failure_data = azimuth::TelemetryData {
    timestamp: 1634567890125,
    metric_name: "test.metric.failure",
    metric_value: 99.0,
    tags: []
  }
  
  let result = azimuth::send_telemetry_data(producer, failure_data)
  match result {
    azimuth::SendResult::Failure(error) => assert_true(error.length() > 0)
    _ => assert_true(false, "Should fail when stream is in failed state")
  }
  
  // 恢复流
  let recovery_result = azimuth::recover_stream(stream.id)
  match recovery_result {
    azimuth::RecoveryResult::Success => assert_true(true)
    azimuth::RecoveryResult::Failure(error) => assert_true(false, "Recovery failed: " + error)
  }
  
  // 验证流状态恢复
  let recovered_status = azimuth::get_stream_status(stream.id)
  assert_eq(recovered_status, "active")
  
  // 验证数据仍然可用（持久化）
  let consumer = azimuth::create_consumer(stream.id, "recovery-test-group")
  let consumed_data = azimuth::consume_messages(consumer, 2, 5000)
  assert_eq(consumed_data.length(), 2)
}

test "实时遥测数据流多消费者并行处理测试" {
  // 创建数据流
  let stream_config = azimuth::StreamConfig {
    stream_id: "multi-consumer-stream",
    name: "multi-consumer-test",
    description: "多消费者并行处理测试流",
    source_type: "test",
    buffer_size: 1000,
    batch_size: 50,
    flush_interval_ms: 100,
    compression: "none",
    encryption: false,
    retry_policy: azimuth::RetryPolicy {
      max_retries: 3,
      backoff_ms: 100,
      exponential_backoff: true
    }
  }
  
  let stream = azimuth::create_stream(stream_config)
  let producer = azimuth::create_producer(stream.id)
  
  // 发送大量数据
  let mut test_data = []
  for i in 0..100 {
    let data = azimuth::TelemetryData {
      timestamp: 1634567890123 + i,
      metric_name: "parallel.metric",
      metric_value: i.to_float(),
      tags: [("batch", (i / 10).to_string()), ("index", i.to_string())]
    }
    test_data = test_data + [data]
    azimuth::send_telemetry_data(producer, data)
  }
  
  // 创建多个消费者
  let consumer1 = azimuth::create_consumer(stream.id, "parallel-group-1")
  let consumer2 = azimuth::create_consumer(stream.id, "parallel-group-2")
  let consumer3 = azimuth::create_consumer(stream.id, "parallel-group-3")
  
  // 并行消费数据
  let consumed1 = azimuth::consume_messages(consumer1, 35, 5000)
  let consumed2 = azimuth::consume_messages(consumer2, 35, 5000)
  let consumed3 = azimuth::consume_messages(consumer3, 30, 5000)
  
  // 验证消费的数据总数
  assert_eq(consumed1.length(), 35)
  assert_eq(consumed2.length(), 35)
  assert_eq(consumed3.length(), 30)
  
  // 验证数据完整性（无重复）
  let all_consumed = consumed1 + consumed2 + consumed3
  let unique_timestamps = all_consumed.map(fn(data) { data.timestamp }).unique()
  assert_eq(unique_timestamps.length(), 100)  // 所有数据都应该是唯一的
  
  // 验证消费者组隔离
  let offset1 = azimuth::get_consumer_offset(consumer1)
  let offset2 = azimuth::get_consumer_offset(consumer2)
  let offset3 = azimuth::get_consumer_offset(consumer3)
  
  assert_true(offset1 > 0)
  assert_true(offset2 > 0)
  assert_true(offset3 > 0)
  
  // 验证流统计
  let stats = azimuth::get_stream_stats(stream.id)
  assert_eq(stats.total_messages, 100)
  assert_eq(stats.consumer_count, 3)
}