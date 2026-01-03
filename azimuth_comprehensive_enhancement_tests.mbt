// Azimuth 综合增强测试用例
// 包含性能、内存管理、网络通信、配置管理、安全性等方面的测试

// 测试1: 性能基准测试
test "遥测系统性能基准测试" {
  // 性能度量点类型
  type PerformanceMetric = {
    operation: String,
    start_time: Int,
    end_time: Int,
    memory_usage: Int,
    cpu_usage: Float
  }
  
  // 创建性能测试器
  let performance_tester = fn(operation_name: String, operation: () -> Unit) {
    let start_time = Time::now()
    let start_memory = Memory::used()
    
    // 执行操作
    operation()
    
    let end_time = Time::now()
    let end_memory = Memory::used()
    
    {
      operation: operation_name,
      start_time: start_time,
      end_time: end_time,
      memory_usage: end_memory - start_memory,
      cpu_usage: 0.0  // 简化实现
    }
  }
  
  // 测试span创建性能
  let span_creation_test = fn() {
    let tracer_provider = TracerProvider::default()
    let tracer = TracerProvider::get_tracer(tracer_provider, "performance.test")
    
    for i in 0..1000 {
      let span = Tracer::start_span(tracer, "performance.span." + i.to_string())
      Span::set_attribute(span, "iteration", i)
      Span::end(span)
    }
  }
  
  // 测试度量记录性能
  let metric_recording_test = fn() {
    let meter_provider = MeterProvider::default()
    let meter = MeterProvider::get_meter(meter_provider, "performance.test")
    let counter = Meter::create_counter(meter, "performance.counter", None, None)
    
    for i in 0..1000 {
      Counter::add(counter, 1.0)
      Counter::add_with_attributes(counter, 1.0, [("iteration", i.to_string())])
    }
  }
  
  // 测试日志记录性能
  let logging_test = fn() {
    let logger_provider = LoggerProvider::default()
    let logger = LoggerProvider::get_logger(logger_provider, "performance.test")
    
    for i in 0..1000 {
      let log = LogRecord::new(Info, "Performance test log " + i.to_string())
      LogRecord::add_attribute(log, "iteration", i.to_string())
      Logger::emit(logger, log)
    }
  }
  
  // 执行性能测试
  let span_metrics = performance_tester("span_creation", span_creation_test)
  let metric_metrics = performance_tester("metric_recording", metric_recording_test)
  let logging_metrics = performance_tester("logging", logging_test)
  
  // 验证性能指标
  assert_true(span_metrics.end_time > span_metrics.start_time)
  assert_true(metric_metrics.end_time > metric_metrics.start_time)
  assert_true(logging_metrics.end_time > logging_metrics.start_time)
  
  // 验证内存使用在合理范围内
  assert_true(span_metrics.memory_usage >= 0)
  assert_true(metric_metrics.memory_usage >= 0)
  assert_true(logging_metrics.memory_usage >= 0)
}

// 测试2: 内存管理和资源清理测试
test "内存管理和资源清理测试" {
  // 资源管理器接口
  trait ResourceManager {
    acquire() -> Unit
    release() -> Unit
    is_released() -> Bool
  }
  
  // 实现一个简单的资源管理器
  let create_resource_manager = fn(resource_id: String) {
    let mut released = false
    
    {
      acquire: fn() {
        println("Acquiring resource: " + resource_id)
      },
      release: fn() {
        println("Releasing resource: " + resource_id)
        released = true
      },
      is_released: fn() { released }
    }
  }
  
  // 创建多个资源管理器
  let resource1 = create_resource_manager("resource-1")
  let resource2 = create_resource_manager("resource-2")
  let resource3 = create_resource_manager("resource-3")
  
  // 获取资源
  resource1.acquire()
  resource2.acquire()
  resource3.acquire()
  
  // 测试资源状态
  assert_false(resource1.is_released())
  assert_false(resource2.is_released())
  assert_false(resource3.is_released())
  
  // 释放资源
  resource1.release()
  resource2.release()
  resource3.release()
  
  // 验证资源已释放
  assert_true(resource1.is_released())
  assert_true(resource2.is_released())
  assert_true(resource3.is_released())
  
  // 测试自动资源管理
  let with_resource = fn(manager: ResourceManager, operation: () -> Unit) {
    manager.acquire()
    operation()
    manager.release()
  }
  
  let auto_manager = create_resource_manager("auto-resource")
  with_resource(auto_manager, fn() {
    assert_false(auto_manager.is_released())
    // 执行一些操作
  })
  
  // 验证资源已自动释放
  assert_true(auto_manager.is_released())
}

// 测试3: 网络通信和遥测数据传输测试
test "网络通信和遥测数据传输测试" {
  // 定义遥测数据包
  type TelemetryPacket = {
    packet_id: String,
    timestamp: Int,
    payload: Array[Byte],
    checksum: Int
  }
  
  // 定义网络传输接口
  trait NetworkTransport {
    send(packet: TelemetryPacket) -> Result[Unit, String]
    receive() -> Result[TelemetryPacket, String]
    is_connected() -> Bool
  }
  
  // 实现模拟网络传输
  let create_mock_transport = fn() {
    let mut connected = true
    let mut sent_packets = []
    let mut receive_index = 0
    
    {
      send: fn(packet: TelemetryPacket) {
        if not connected {
          Err("Network disconnected")
        } else {
          sent_packets = sent_packets.push(packet)
          Ok(())
        }
      },
      receive: fn() {
        if not connected {
          Err("Network disconnected")
        } else if receive_index >= sent_packets.length() {
          Err("No packets available")
        } else {
          let packet = sent_packets[receive_index]
          receive_index = receive_index + 1
          Ok(packet)
        }
      },
      is_connected: fn() { connected }
    }
  }
  
  // 创建遥测数据包
  let create_packet = fn(id: String, data: String) {
    let payload_bytes = data.to_bytes()
    let checksum = payload_bytes.reduce(0, fn(acc, byte) { acc + byte })
    
    {
      packet_id: id,
      timestamp: Time::now(),
      payload: payload_bytes,
      checksum: checksum
    }
  }
  
  // 创建网络传输
  let transport = create_mock_transport()
  
  // 测试网络连接状态
  assert_true(transport.is_connected())
  
  // 创建并发送数据包
  let packet1 = create_packet("packet-1", "Telemetry data 1")
  let packet2 = create_packet("packet-2", "Telemetry data 2")
  let packet3 = create_packet("packet-3", "Telemetry data 3")
  
  // 发送数据包
  let send_result1 = transport.send(packet1)
  let send_result2 = transport.send(packet2)
  let send_result3 = transport.send(packet3)
  
  // 验证发送结果
  assert_eq(send_result1, Ok(()))
  assert_eq(send_result2, Ok(()))
  assert_eq(send_result3, Ok(()))
  
  // 接收数据包
  let receive_result1 = transport.receive()
  let receive_result2 = transport.receive()
  let receive_result3 = transport.receive()
  
  // 验证接收结果
  assert_eq(receive_result1, Ok(packet1))
  assert_eq(receive_result2, Ok(packet2))
  assert_eq(receive_result3, Ok(packet3))
  
  // 测试接收空数据包
  let empty_receive = transport.receive()
  assert_eq(empty_receive, Err("No packets available"))
}

// 测试4: 配置管理和动态更新测试
test "配置管理和动态更新测试" {
  // 定义配置类型
  type Configuration = {
    telemetry_enabled: Bool,
    sampling_rate: Float,
    batch_size: Int,
    export_interval: Int,
    attributes: Array[(String, String)]
  }
  
  // 配置变更监听器
  trait ConfigChangeListener {
    on_config_changed(old_config: Configuration, new_config: Configuration) -> Unit
  }
  
  // 实现配置管理器
  let create_config_manager = fn(initial_config: Configuration) {
    let mut current_config = initial_config
    let mut listeners = []
    
    {
      get_config: fn() { current_config },
      update_config: fn(new_config: Configuration) {
        let old_config = current_config
        current_config = new_config
        
        // 通知所有监听器
        for listener in listeners {
          listener.on_config_changed(old_config, new_config)
        }
      },
      add_listener: fn(listener: ConfigChangeListener) {
        listeners = listeners.push(listener)
      },
      remove_listener: fn(listener: ConfigChangeListener) {
        listeners = listeners.filter(fn(l) { l != listener })
      }
    }
  }
  
  // 创建默认配置
  let default_config = {
    telemetry_enabled: true,
    sampling_rate: 0.1,
    batch_size: 100,
    export_interval: 5000,
    attributes: [("service.name", "test-service")]
  }
  
  // 创建配置管理器
  let config_manager = create_config_manager(default_config)
  
  // 验证初始配置
  let initial_config = config_manager.get_config()
  assert_true(initial_config.telemetry_enabled)
  assert_eq(initial_config.sampling_rate, 0.1)
  assert_eq(initial_config.batch_size, 100)
  assert_eq(initial_config.export_interval, 5000)
  assert_eq(initial_config.attributes.length(), 1)
  
  // 创建配置变更监听器
  let mut change_count = 0
  let listener = {
    on_config_changed: fn(old_config: Configuration, new_config: Configuration) {
      change_count = change_count + 1
    }
  }
  
  // 添加监听器
  config_manager.add_listener(listener)
  
  // 更新配置
  let updated_config = {
    telemetry_enabled: false,
    sampling_rate: 0.5,
    batch_size: 200,
    export_interval: 10000,
    attributes: [("service.name", "test-service"), ("environment", "test")]
  }
  
  config_manager.update_config(updated_config)
  
  // 验证配置已更新
  let current_config = config_manager.get_config()
  assert_false(current_config.telemetry_enabled)
  assert_eq(current_config.sampling_rate, 0.5)
  assert_eq(current_config.batch_size, 200)
  assert_eq(current_config.export_interval, 10000)
  assert_eq(current_config.attributes.length(), 2)
  
  // 验证监听器被调用
  assert_eq(change_count, 1)
  
  // 再次更新配置
  let another_config = {
    telemetry_enabled: true,
    sampling_rate: 0.2,
    batch_size: 150,
    export_interval: 7500,
    attributes: [("service.name", "test-service"), ("environment", "test"), ("version", "1.0.0")]
  }
  
  config_manager.update_config(another_config)
  
  // 验证监听器再次被调用
  assert_eq(change_count, 2)
}

// 测试5: 安全性和加密测试
test "安全性和加密测试" {
  // 定义加密接口
  trait Encryptor {
    encrypt(data: Array[Byte], key: Array[Byte]) -> Result[Array[Byte], String]
    decrypt(encrypted_data: Array[Byte], key: Array[Byte]) -> Result[Array[Byte], String]
  }
  
  // 实现简单的XOR加密器
  let create_xor_encryptor = fn() {
    {
      encrypt: fn(data: Array[Byte], key: Array[Byte]) {
        if key.length() == 0 {
          Err("Empty encryption key")
        } else {
          let encrypted = data.map_with_index(fn(i, byte) {
            byte ^ key[i % key.length()]
          })
          Ok(encrypted)
        }
      },
      decrypt: fn(encrypted_data: Array[Byte], key: Array[Byte]) {
        if key.length() == 0 {
          Err("Empty decryption key")
        } else {
          let decrypted = encrypted_data.map_with_index(fn(i, byte) {
            byte ^ key[i % key.length()]
          })
          Ok(decrypted)
        }
      }
    }
  }
  
  // 创建加密器
  let encryptor = create_xor_encryptor()
  
  // 测试数据
  let sensitive_data = "Sensitive telemetry data".to_bytes()
  let encryption_key = "my-secret-key".to_bytes()
  
  // 加密数据
  let encrypted_result = encryptor.encrypt(sensitive_data, encryption_key)
  assert_true(encrypted_result.is_ok())
  
  let encrypted_data = match encrypted_result {
    Ok(data) => data
    Err(_) => []  // 不应该发生
  }
  
  // 验证加密后的数据与原始数据不同
  assert_false(encrypted_data == sensitive_data)
  
  // 解密数据
  let decrypted_result = encryptor.decrypt(encrypted_data, encryption_key)
  assert_true(decrypted_result.is_ok())
  
  let decrypted_data = match decrypted_result {
    Ok(data) => data
    Err(_) => []  // 不应该发生
  }
  
  // 验证解密后的数据与原始数据相同
  assert_eq(decrypted_data, sensitive_data)
  
  // 测试使用错误的密钥解密
  let wrong_key = "wrong-key".to_bytes()
  let wrong_decrypt_result = encryptor.decrypt(encrypted_data, wrong_key)
  assert_true(wrong_decrypt_result.is_ok())
  
  let wrong_decrypted_data = match wrong_decrypt_result {
    Ok(data) => data
    Err(_) => []  // 不应该发生
  }
  
  // 验证使用错误密钥解密的结果与原始数据不同
  assert_false(wrong_decrypted_data == sensitive_data)
  
  // 测试空密钥错误
  let empty_key = "".to_bytes()
  let empty_key_encrypt_result = encryptor.encrypt(sensitive_data, empty_key)
  assert_eq(empty_key_encrypt_result, Err("Empty encryption key"))
  
  let empty_key_decrypt_result = encryptor.decrypt(encrypted_data, empty_key)
  assert_eq(empty_key_decrypt_result, Err("Empty decryption key"))
}

// 测试6: 数据序列化和反序列化测试
test "数据序列化和反序列化测试" {
  // 定义序列化接口
  trait Serializer[T] {
    serialize(data: T) -> Result[Array[Byte], String]
    deserialize(bytes: Array[Byte]) -> Result[T, String]
  }
  
  // 实现JSON序列化器
  let create_json_serializer = fn() {
    {
      serialize: fn(data: String) {
        // 简化的JSON序列化实现
        let json_string = "\"" + data + "\""
        Ok(json_string.to_bytes())
      },
      deserialize: fn(bytes: Array[Byte]) {
        // 简化的JSON反序列化实现
        let string = String::from_bytes(bytes)
        if string.starts_with("\"") && string.ends_with("\"") {
          let content = string.substring(1, string.length() - 1)
          Ok(content)
        } else {
          Err("Invalid JSON string format")
        }
      }
    }
  }
  
  // 实现二进制序列化器
  let create_binary_serializer = fn() {
    {
      serialize: fn(data: String) {
        // 简化的二进制序列化实现
        let length_bytes = (data.length() as Int).to_bytes()
        let data_bytes = data.to_bytes()
        Ok(length_bytes + data_bytes)
      },
      deserialize: fn(bytes: Array[Byte]) {
        // 简化的二进制反序列化实现
        if bytes.length() < 4 {
          Err("Invalid binary data format")
        } else {
          let length_bytes = bytes.slice(0, 4)
          let data_bytes = bytes.slice(4, bytes.length())
          let length = Int::from_bytes(length_bytes)
          
          if data_bytes.length() != length {
            Err("Data length mismatch")
          } else {
            Ok(String::from_bytes(data_bytes))
          }
        }
      }
    }
  }
  
  // 测试JSON序列化
  let json_serializer = create_json_serializer()
  let test_data = "Telemetry data for serialization test"
  
  let json_serialize_result = json_serializer.serialize(test_data)
  assert_true(json_serialize_result.is_ok())
  
  let json_bytes = match json_serialize_result {
    Ok(bytes) => bytes
    Err(_) => []  // 不应该发生
  }
  
  let json_deserialize_result = json_serializer.deserialize(json_bytes)
  assert_true(json_deserialize_result.is_ok())
  
  let json_deserialized_data = match json_deserialize_result {
    Ok(data) => data
    Err(_) => ""  // 不应该发生
  }
  
  assert_eq(json_deserialized_data, test_data)
  
  // 测试二进制序列化
  let binary_serializer = create_binary_serializer()
  
  let binary_serialize_result = binary_serializer.serialize(test_data)
  assert_true(binary_serialize_result.is_ok())
  
  let binary_bytes = match binary_serialize_result {
    Ok(bytes) => bytes
    Err(_) => []  // 不应该发生
  }
  
  let binary_deserialize_result = binary_serializer.deserialize(binary_bytes)
  assert_true(binary_deserialize_result.is_ok())
  
  let binary_deserialized_data = match binary_deserialize_result {
    Ok(data) => data
    Err(_) => ""  // 不应该发生
  }
  
  assert_eq(binary_deserialized_data, test_data)
  
  // 测试无效数据反序列化
  let invalid_json_bytes = "invalid json".to_bytes()
  let invalid_json_result = json_serializer.deserialize(invalid_json_bytes)
  assert_eq(invalid_json_result, Err("Invalid JSON string format"))
  
  let invalid_binary_bytes = [1, 2, 3]  // 长度不足
  let invalid_binary_result = binary_serializer.deserialize(invalid_binary_bytes)
  assert_eq(invalid_binary_result, Err("Invalid binary data format"))
}

// 测试7: 时间序列数据处理测试
test "时间序列数据处理测试" {
  // 定义时间序列数据点
  type TimeSeriesPoint = {
    timestamp: Int,
    value: Float,
    tags: Array[(String, String)]
  }
  
  // 定义时间序列聚合类型
  enum AggregationType {
    Sum
    Average
    Min
    Max
    Count
    Percentile(Float)
  }
  
  // 创建时间序列聚合器
  let aggregate_time_series = fn(points: Array[TimeSeriesPoint], aggregation_type: AggregationType) {
    if points.length() == 0 {
      return 0.0
    }
    
    let values = points.map(fn(p) { p.value })
    
    match aggregation_type {
      AggregationType::Sum => {
        values.reduce(fn(acc, v) { acc + v }, 0.0)
      }
      AggregationType::Average => {
        let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
        sum / (values.length() as Float)
      }
      AggregationType::Min => {
        values.reduce(fn(acc, v) { if v < acc { v } else { acc }, values[0])
      }
      AggregationType::Max => {
        values.reduce(fn(acc, v) { if v > acc { v } else { acc }, values[0])
      }
      AggregationType::Count => {
        values.length() as Float
      }
      AggregationType::Percentile(p) => {
        let sorted_values = values.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
        let index = ((values.length() as Float) * p / 100.0) as Int
        if index < values.length() {
          sorted_values[index]
        } else {
          sorted_values[values.length() - 1]
        }
      }
    }
  }
  
  // 按时间窗口聚合
  let aggregate_by_time_window = fn(points: Array[TimeSeriesPoint], window_size_ms: Int, aggregation_type: AggregationType) {
    if points.length() == 0 {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    
    let mut windows = []
    let mut current_window = []
    let mut window_start = sorted_points[0].timestamp
    let window_end = window_start + window_size_ms
    
    for point in sorted_points {
      if point.timestamp >= window_start && point.timestamp < window_end {
        current_window = current_window.push(point)
      } else {
        if current_window.length() > 0 {
          let aggregated_value = aggregate_time_series(current_window, aggregation_type)
          windows = windows.push({
            timestamp: window_start,
            value: aggregated_value,
            tags: []
          })
        }
        
        // 开始新窗口
        current_window = [point]
        window_start = point.timestamp
        window_end = window_start + window_size_ms
      }
    }
    
    // 处理最后一个窗口
    if current_window.length() > 0 {
      let aggregated_value = aggregate_time_series(current_window, aggregation_type)
      windows = windows.push({
        timestamp: window_start,
        value: aggregated_value,
        tags: []
      })
    }
    
    windows
  }
  
  // 创建测试数据
  let base_time = 1609459200000  // 2021-01-01 00:00:00 UTC
  let test_points = [
    { timestamp: base_time, value: 10.5, tags: [("service", "api")] },
    { timestamp: base_time + 1000, value: 15.2, tags: [("service", "api")] },
    { timestamp: base_time + 2000, value: 8.7, tags: [("service", "web")] },
    { timestamp: base_time + 3000, value: 12.3, tags: [("service", "api")] },
    { timestamp: base_time + 4000, value: 9.8, tags: [("service", "web")] },
    { timestamp: base_time + 5000, value: 11.1, tags: [("service", "api")] }
  ]
  
  // 测试基本聚合
  let sum_result = aggregate_time_series(test_points, AggregationType::Sum)
  assert_eq(sum_result.round(), 67.6)
  
  let avg_result = aggregate_time_series(test_points, AggregationType::Average)
  assert_eq(avg_result.round(), 11.27)
  
  let min_result = aggregate_time_series(test_points, AggregationType::Min)
  assert_eq(min_result, 8.7)
  
  let max_result = aggregate_time_series(test_points, AggregationType::Max)
  assert_eq(max_result, 15.2)
  
  let count_result = aggregate_time_series(test_points, AggregationType::Count)
  assert_eq(count_result, 6.0)
  
  // 测试时间窗口聚合
  let windowed_sum = aggregate_by_time_window(test_points, 3000, AggregationType::Sum)
  assert_eq(windowed_sum.length(), 2)  // 两个窗口
  assert_eq(windowed_sum[0].value.round(), 34.4)  // 前三个点的和
  assert_eq(windowed_sum[1].value.round(), 33.2)  // 后三个点的和
  
  let windowed_avg = aggregate_by_time_window(test_points, 3000, AggregationType::Average)
  assert_eq(windowed_avg.length(), 2)
  assert_eq(windowed_avg[0].value.round(), 11.47)  // 前三个点的平均值
  assert_eq(windowed_avg[1].value.round(), 11.07)  // 后三个点的平均值
}

// 测试8: 分布式追踪一致性测试
test "分布式追踪一致性测试" {
  // 定义追踪上下文
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array[(String, String)]
  }
  
  // 定义传播器接口
  trait Propagator {
    inject(context: TraceContext, carrier: Array[(String, String)]) -> Array[(String, String)]
    extract(carrier: Array[(String, String)]) -> Option[TraceContext]
  }
  
  // 实现文本映射传播器
  let create_text_map_propagator = fn() {
    {
      inject: fn(context: TraceContext, carrier: Array[(String, String)]) {
        let updated_carrier = carrier
        
        // 注入追踪标识
        let with_trace_id = updated_carrier.filter(fn(item) { item.0 != "traceparent" })
        let with_trace_parent = with_trace_id.push(("traceparent", "00-" + context.trace_id + "-" + context.span_id + "-" + context.trace_flags.to_string()))
        
        // 注入追踪状态
        let with_trace_state = with_trace_parent.filter(fn(item) { item.0 != "tracestate" })
        if context.trace_state.length() > 0 {
          let state_string = context.trace_state.map(fn(item) { item.0 + "=" + item.1 }).join(",")
          with_trace_state.push(("tracestate", state_string))
        } else {
          with_trace_state
        }
      },
      extract: fn(carrier: Array[(String, String)]) {
        // 提取追踪标识
        let trace_parent = carrier.find(fn(item) { item.0 == "traceparent" })
        let trace_state = carrier.find(fn(item) { item.0 == "tracestate" })
        
        match trace_parent {
          Some((_, value)) => {
            // 解析traceparent格式: 00-traceId-spanId-traceFlags
            if value.length() >= 55 && value.starts_with("00-") {
              let trace_id = value.substring(3, 35)
              let span_id = value.substring(36, 52)
              let trace_flags = value.substring(53, 55)
              
              let flags_int = match trace_state {
                Some((_, state_value)) => {
                  // 解析tracestate
                  let state_pairs = state_value.split(",").map(fn(pair) {
                    let parts = pair.split("=")
                    if parts.length() == 2 {
                      (parts[0], parts[1])
                    } else {
                      ("", "")
                    }
                  })
                  state_pairs.filter(fn(pair) { pair.0 != "" })
                }
                None => []
              }
              
              Some({
                trace_id: trace_id,
                span_id: span_id,
                parent_span_id: None,
                trace_flags: trace_flags.to_int(),
                trace_state: flags_int
              })
            } else {
              None
            }
          }
          None => None
        }
      }
    }
  }
  
  // 创建传播器
  let propagator = create_text_map_propagator()
  
  // 创建原始追踪上下文
  let original_context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: [("vendor1", "value1"), ("vendor2", "value2")]
  }
  
  // 创建载体
  let carrier = [
    ("user-agent", "Mozilla/5.0"),
    ("accept", "application/json")
  ]
  
  // 注入追踪上下文
  let injected_carrier = propagator.inject(original_context, carrier)
  
  // 验证注入结果
  assert_true(injected_carrier.length() >= carrier.length())
  assert_true(injected_carrier.any(fn(item) { item.0 == "traceparent" }))
  assert_true(injected_carrier.any(fn(item) { item.0 == "tracestate" }))
  
  let trace_parent = injected_carrier.find(fn(item) { item.0 == "traceparent" })
  match trace_parent {
    Some((_, value)) => {
      assert_true(value.contains("4bf92f3577b34da6a3ce929d0e0e4736"))
      assert_true(value.contains("00f067aa0ba902b7"))
    }
    None => assert_true(false)
  }
  
  // 提取追踪上下文
  let extracted_context = propagator.extract(injected_carrier)
  assert_true(extracted_context.is_some())
  
  let context = match extracted_context {
    Some(ctx) => ctx
    None => {
      trace_id: "",
      span_id: "",
      parent_span_id: None,
      trace_flags: 0,
      trace_state: []
    }
  }
  
  // 验证提取的上下文与原始上下文一致
  assert_eq(context.trace_id, original_context.trace_id)
  assert_eq(context.span_id, original_context.span_id)
  assert_eq(context.trace_flags, original_context.trace_flags)
  assert_eq(context.trace_state.length(), original_context.trace_state.length())
  
  // 测试跨服务传播
  let service_a_carrier = [("service", "service-a")]
  let service_a_injected = propagator.inject(context, service_a_carrier)
  
  // 模拟服务B提取上下文
  let service_b_context = propagator.extract(service_a_injected)
  assert_true(service_b_context.is_some())
  
  let service_b_ctx = match service_b_context {
    Some(ctx) => ctx
    None => {
      trace_id: "",
      span_id: "",
      parent_span_id: None,
      trace_flags: 0,
      trace_state: []
    }
  }
  
  // 服务B创建子span
  let service_b_child_context = {
    trace_id: service_b_ctx.trace_id,
    span_id: "1234567890abcdef",
    parent_span_id: Some(service_b_ctx.span_id),
    trace_flags: service_b_ctx.trace_flags,
    trace_state: service_b_ctx.trace_state
  }
  
  // 服务B注入子span上下文
  let service_b_carrier = [("service", "service-b")]
  let service_b_injected = propagator.inject(service_b_child_context, service_b_carrier)
  
  // 服务C提取上下文
  let service_c_context = propagator.extract(service_b_injected)
  assert_true(service_c_context.is_some())
  
  let service_c_ctx = match service_c_context {
    Some(ctx) => ctx
    None => {
      trace_id: "",
      span_id: "",
      parent_span_id: None,
      trace_flags: 0,
      trace_state: []
    }
  }
  
  // 验证追踪链一致性
  assert_eq(service_c_ctx.trace_id, original_context.trace_id)
  assert_eq(service_c_ctx.parent_span_id, Some(service_b_child_context.span_id))
}