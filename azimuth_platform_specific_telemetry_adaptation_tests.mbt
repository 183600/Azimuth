// 平台特定遥测适配测试
// 测试不同平台和环境下的遥测功能适配性

// Test 1: WebAssembly平台遥测测试
pub test "WebAssembly平台遥测测试" {
  // 测试WebAssembly环境下的遥测功能
  let tracer_provider = azimuth::TracerProvider::default()
  let wasm_tracer = azimuth::TracerProvider::get_tracer(tracer_provider, "wasm-platform-tracer")
  
  // 创建WebAssembly特定的Span
  let wasm_span = azimuth::Tracer::start_span(wasm_tracer, "wasm-operation")
  
  // 添加WebAssembly特定属性
  azimuth::Span::add_event(wasm_span, "wasm.platform.init", Some([
    ("platform.type", azimuth::StringValue("webassembly")),
    ("runtime.version", azimuth::StringValue("wasm-gc")),
    ("memory.model", azimuth::StringValue("gc"))
  ]))
  
  // 验证WebAssembly Span的属性
  assert_eq(azimuth::Span::name(wasm_span), "wasm-operation")
  assert_true(azimuth::Span::is_recording(wasm_span))
  
  // 测试WebAssembly环境下的度量
  let meter_provider = azimuth::MeterProvider::default()
  let wasm_meter = azimuth::MeterProvider::get_meter(meter_provider, "wasm-platform-meter")
  
  // 创建WebAssembly特定的度量
  let wasm_counter = azimuth::Meter::create_counter(wasm_meter, "wasm.operations.count")
  let wasm_histogram = azimuth::Meter::create_histogram(wasm_meter, "wasm.execution.time")
  
  // 添加度量值
  azimuth::Counter::add(wasm_counter, 1.0)
  azimuth::Histogram::record(wasm_histogram, 42.5)
  
  // 验证WebAssembly度量属性
  assert_eq(wasm_counter.name, "wasm.operations.count")
  assert_eq(wasm_histogram.name, "wasm.execution.time")
  
  // 测试WebAssembly环境下的日志
  let wasm_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "wasm-platform-logger")
  
  let wasm_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("WebAssembly platform telemetry initialized"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("wasm-trace-id"),
    Some("wasm-span-id"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(wasm_logger, wasm_log)
  
  // 验证WebAssembly日志
  assert_eq(azimuth::LogRecord::severity_number(wasm_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(wasm_log), Some("WebAssembly platform telemetry initialized"))
}

// Test 2: 云原生环境遥测测试
pub test "云原生环境遥测测试" {
  // 创建云原生特定的资源属性
  let cloud_resource_attrs = [
    ("service.name", azimuth::StringValue("cloud-native-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("cloud.provider", azimuth::StringValue("aws")),
    ("cloud.region", azimuth::StringValue("us-west-2")),
    ("cloud.availability_zone", azimuth::StringValue("us-west-2a")),
    ("k8s.cluster.name", azimuth::StringValue("production-cluster")),
    ("k8s.namespace.name", azimuth::StringValue("azimuth-namespace")),
    ("k8s.pod.name", azimuth::StringValue("azimuth-pod-12345")),
    ("k8s.container.name", azimuth::StringValue("azimuth-container")),
    ("deployment.environment", azimuth::StringValue("production"))
  ]
  
  let cloud_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), cloud_resource_attrs)
  
  // 验证云原生资源属性
  assert_eq(azimuth::Resource::get_attribute(cloud_resource, "cloud.provider"), Some(azimuth::StringValue("aws")))
  assert_eq(azimuth::Resource::get_attribute(cloud_resource, "cloud.region"), Some(azimuth::StringValue("us-west-2")))
  assert_eq(azimuth::Resource::get_attribute(cloud_resource, "k8s.cluster.name"), Some(azimuth::StringValue("production-cluster")))
  assert_eq(azimuth::Resource::get_attribute(cloud_resource, "k8s.namespace.name"), Some(azimuth::StringValue("azimuth-namespace")))
  assert_eq(azimuth::Resource::get_attribute(cloud_resource, "k8s.pod.name"), Some(azimuth::StringValue("azimuth-pod-12345")))
  
  // 创建云原生特定的Tracer
  let cloud_tracer_provider = azimuth::TracerProvider::default()
  let cloud_tracer = azimuth::TracerProvider::get_tracer(cloud_tracer_provider, "cloud-native-tracer")
  
  // 创建云原生特定的Span
  let cloud_span = azimuth::Tracer::start_span(cloud_tracer, "k8s.operation")
  
  // 添加云原生特定事件
  azimuth::Span::add_event(cloud_span, "k8s.pod.started", Some([
    ("k8s.pod.name", azimuth::StringValue("azimuth-pod-12345")),
    ("k8s.namespace", azimuth::StringValue("azimuth-namespace")),
    ("node.name", azimuth::StringValue("worker-node-1"))
  ]))
  
  // 验证云原生Span
  assert_eq(azimuth::Span::name(cloud_span), "k8s.operation")
  
  // 创建云原生特定的度量
  let cloud_meter_provider = azimuth::MeterProvider::default()
  let cloud_meter = azimuth::MeterProvider::get_meter(cloud_meter_provider, "cloud-native-meter")
  
  let k8s_counter = azimuth::Meter::create_counter(cloud_meter, "k8s.requests.count")
  let pod_memory_gauge = azimuth::Meter::create_gauge(cloud_meter, "k8s.pod.memory.usage")
  
  // 添加度量值
  azimuth::Counter::add(k8s_counter, 1.0)
  
  // 验证云原生度量
  assert_eq(k8s_counter.name, "k8s.requests.count")
  assert_eq(pod_memory_gauge.name, "k8s.pod.memory.usage")
}

// Test 3: 微服务架构遥测测试
pub test "微服务架构遥测测试" {
  // 模拟微服务调用链
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "api-gateway")
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  let db_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "database-service")
  
  // API网关创建根Span
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "api.gateway.request")
  let gateway_span_ctx = azimuth::Span::span_context(gateway_span)
  
  // 服务A创建子Span
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service.a.processing")
  let service_a_span_ctx = azimuth::Span::span_context(service_a_span)
  
  // 服务B创建子Span
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service.b.business.logic")
  let service_b_span_ctx = azimuth::Span::span_context(service_b_span)
  
  // 数据库服务创建子Span
  let db_span = azimuth::Tracer::start_span(db_tracer, "database.query")
  
  // 验证微服务Span属性
  assert_eq(azimuth::Span::name(gateway_span), "api.gateway.request")
  assert_eq(azimuth::Span::name(service_a_span), "service.a.processing")
  assert_eq(azimuth::Span::name(service_b_span), "service.b.business.logic")
  assert_eq(azimuth::Span::name(db_span), "database.query")
  
  // 添加微服务特定事件
  azimuth::Span::add_event(gateway_span, "request.received", Some([
    ("http.method", azimuth::StringValue("GET")),
    ("http.url", azimuth::StringValue("/api/v1/resource")),
    ("user.agent", azimuth::StringValue("microservice-client/1.0"))
  ]))
  
  azimuth::Span::add_event(service_a_span, "service.a.started", Some([
    ("service.name", azimuth::StringValue("service-a")),
    ("service.version", azimuth::StringValue("2.1.0"))
  ]))
  
  azimuth::Span::add_event(service_b_span, "service.b.processing", Some([
    ("service.name", azimuth::StringValue("service-b")),
    ("business.rule", azimuth::StringValue("validation-rule-123"))
  ]))
  
  azimuth::Span::add_event(db_span, "db.query.executed", Some([
    ("db.statement", azimuth::StringValue("SELECT * FROM users WHERE id = ?")),
    ("db.type", azimuth::StringValue("postgresql")),
    ("db.connection.pool", azimuth::StringValue("primary"))
  ]))
  
  // 测试微服务Baggage传播
  let baggage = azimuth::Baggage::new()
  let microservice_baggage = azimuth::Baggage::set_entry(
    azimuth::Baggage::set_entry(
      azimuth::Baggage::set_entry(baggage, "user.id", "user-12345"),
      "request.id", "req-67890"
    ),
    "trace.id", "trace-abcde"
  )
  
  // 验证微服务Baggage
  assert_eq(azimuth::Baggage::get_entry(microservice_baggage, "user.id"), Some("user-12345"))
  assert_eq(azimuth::Baggage::get_entry(microservice_baggage, "request.id"), Some("req-67890"))
  assert_eq(azimuth::Baggage::get_entry(microservice_baggage, "trace.id"), Some("trace-abcde"))
  
  // 创建微服务特定的度量
  let microservice_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "microservice-meter")
  
  let request_counter = azimuth::Meter::create_counter(microservice_meter, "microservice.requests.total")
  let latency_histogram = azimuth::Meter::create_histogram(microservice_meter, "microservice.request.duration")
  let error_counter = azimuth::Meter::create_counter(microservice_meter, "microservice.errors.total")
  let active_connections_gauge = azimuth::Meter::create_gauge(microservice_meter, "microservice.active.connections")
  
  // 添加度量值
  azimuth::Counter::add(request_counter, 1.0)
  azimuth::Histogram::record(latency_histogram, 150.5)
  azimuth::Counter::add(error_counter, 0.0)  // 无错误
  
  // 验证微服务度量
  assert_eq(request_counter.name, "microservice.requests.total")
  assert_eq(latency_histogram.name, "microservice.request.duration")
  assert_eq(error_counter.name, "microservice.errors.total")
  assert_eq(active_connections_gauge.name, "microservice.active.connections")
}

// Test 4: 边缘计算环境遥测测试
pub test "边缘计算环境遥测测试" {
  // 创建边缘计算特定的资源属性
  let edge_resource_attrs = [
    ("service.name", azimuth::StringValue("edge-computing-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("edge.device.id", azimuth::StringValue("edge-device-12345")),
    ("edge.location", azimuth::StringValue("datacenter-edge-1")),
    ("edge.region", azimuth::StringValue("us-west-edge")),
    ("edge.network.type", azimuth::StringValue("5g")),
    ("edge.latency", azimuth::IntValue(5)),  // 5ms latency
    ("edge.bandwidth", azimuth::IntValue(1000)),  // 1000 Mbps
    ("deployment.environment", azimuth::StringValue("edge"))
  ]
  
  let edge_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), edge_resource_attrs)
  
  // 验证边缘计算资源属性
  assert_eq(azimuth::Resource::get_attribute(edge_resource, "edge.device.id"), Some(azimuth::StringValue("edge-device-12345")))
  assert_eq(azimuth::Resource::get_attribute(edge_resource, "edge.location"), Some(azimuth::StringValue("datacenter-edge-1")))
  assert_eq(azimuth::Resource::get_attribute(edge_resource, "edge.network.type"), Some(azimuth::StringValue("5g")))
  assert_eq(azimuth::Resource::get_attribute(edge_resource, "edge.latency"), Some(azimuth::IntValue(5)))
  assert_eq(azimuth::Resource::get_attribute(edge_resource, "edge.bandwidth"), Some(azimuth::IntValue(1000)))
  
  // 创建边缘计算特定的Tracer
  let edge_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "edge-computing-tracer")
  
  // 创建边缘计算特定的Span
  let edge_span = azimuth::Tracer::start_span(edge_tracer, "edge.processing")
  
  // 添加边缘计算特定事件
  azimuth::Span::add_event(edge_span, "edge.device.connected", Some([
    ("edge.device.id", azimuth::StringValue("edge-device-12345")),
    ("edge.connection.type", azimuth::StringValue("wireless")),
    ("edge.signal.strength", azimuth::IntValue(85))
  ]))
  
  azimuth::Span::add_event(edge_span, "edge.data.processed", Some([
    ("data.size", azimuth::IntValue(1024)),
    ("processing.time", azimuth::FloatValue(2.5)),
    ("edge.algorithm", azimuth::StringValue("ml-inference"))
  ]))
  
  // 验证边缘计算Span
  assert_eq(azimuth::Span::name(edge_span), "edge.processing")
  
  // 创建边缘计算特定的度量
  let edge_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "edge-computing-meter")
  
  let edge_throughput_counter = azimuth::Meter::create_counter(edge_meter, "edge.throughput.bytes")
  let edge_latency_histogram = azimuth::Meter::create_histogram(edge_meter, "edge.processing.latency")
  let edge_cpu_gauge = azimuth::Meter::create_gauge(edge_meter, "edge.cpu.usage")
  let edge_memory_gauge = azimuth::Meter::create_gauge(edge_meter, "edge.memory.usage")
  
  // 添加度量值
  azimuth::Counter::add(edge_throughput_counter, 1024.0)
  azimuth::Histogram::record(edge_latency_histogram, 2.5)
  
  // 验证边缘计算度量
  assert_eq(edge_throughput_counter.name, "edge.throughput.bytes")
  assert_eq(edge_latency_histogram.name, "edge.processing.latency")
  assert_eq(edge_cpu_gauge.name, "edge.cpu.usage")
  assert_eq(edge_memory_gauge.name, "edge.memory.usage")
}

// Test 5: IoT设备遥测测试
pub test "IoT设备遥测测试" {
  // 创建IoT设备特定的资源属性
  let iot_resource_attrs = [
    ("service.name", azimuth::StringValue("iot-telemetry-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("iot.device.id", azimuth::StringValue("iot-sensor-12345")),
    ("iot.device.type", azimuth::StringValue("temperature-sensor")),
    ("iot.manufacturer", azimuth::StringValue("sensor-corp")),
    ("iot.model", azimuth::StringValue("temp-sense-v2")),
    ("iot.firmware.version", azimuth::StringValue("3.2.1")),
    ("iot.battery.level", azimuth::IntValue(85)),
    ("iot.signal.strength", azimuth::IntValue(92)),
    ("deployment.environment", azimuth::StringValue("production"))
  ]
  
  let iot_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), iot_resource_attrs)
  
  // 验证IoT设备资源属性
  assert_eq(azimuth::Resource::get_attribute(iot_resource, "iot.device.id"), Some(azimuth::StringValue("iot-sensor-12345")))
  assert_eq(azimuth::Resource::get_attribute(iot_resource, "iot.device.type"), Some(azimuth::StringValue("temperature-sensor")))
  assert_eq(azimuth::Resource::get_attribute(iot_resource, "iot.manufacturer"), Some(azimuth::StringValue("sensor-corp")))
  assert_eq(azimuth::Resource::get_attribute(iot_resource, "iot.battery.level"), Some(azimuth::IntValue(85)))
  assert_eq(azimuth::Resource::get_attribute(iot_resource, "iot.signal.strength"), Some(azimuth::IntValue(92)))
  
  // 创建IoT设备特定的Tracer
  let iot_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "iot-device-tracer")
  
  // 创建IoT设备特定的Span
  let iot_span = azimuth::Tracer::start_span(iot_tracer, "iot.sensor.reading")
  
  // 添加IoT设备特定事件
  azimuth::Span::add_event(iot_span, "sensor.data.collected", Some([
    ("sensor.type", azimuth::StringValue("temperature")),
    ("sensor.value", azimuth::FloatValue(23.5)),
    ("sensor.unit", azimuth::StringValue("celsius")),
    ("collection.timestamp", azimuth::StringValue("2025-01-02T12:00:00Z"))
  ]))
  
  azimuth::Span::add_event(iot_span, "data.transmitted", Some([
    ("transmission.protocol", azimuth::StringValue("mqtt")),
    ("transmission.size", azimuth::IntValue(128)),
    ("transmission.success", azimuth::BoolValue(true))
  ]))
  
  // 验证IoT设备Span
  assert_eq(azimuth::Span::name(iot_span), "iot.sensor.reading")
  
  // 创建IoT设备特定的度量
  let iot_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "iot-device-meter")
  
  let sensor_reading_gauge = azimuth::Meter::create_gauge(iot_meter, "iot.sensor.temperature")
  let battery_level_gauge = azimuth::Meter::create_gauge(iot_meter, "iot.battery.level")
  let signal_strength_gauge = azimuth::Meter::create_gauge(iot_meter, "iot.signal.strength")
  let transmission_counter = azimuth::Meter::create_counter(iot_meter, "iot.data.transmissions")
  
  // 添加度量值
  azimuth::Counter::add(transmission_counter, 1.0)
  
  // 验证IoT设备度量
  assert_eq(sensor_reading_gauge.name, "iot.sensor.temperature")
  assert_eq(battery_level_gauge.name, "iot.battery.level")
  assert_eq(signal_strength_gauge.name, "iot.signal.strength")
  assert_eq(transmission_counter.name, "iot.data.transmissions")
  
  // 创建IoT设备特定的日志
  let iot_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "iot-device-logger")
  
  let iot_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("IoT sensor reading completed successfully"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("iot-trace-id"),
    Some("iot-span-id"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(iot_logger, iot_log)
  
  // 验证IoT设备日志
  assert_eq(azimuth::LogRecord::severity_number(iot_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(iot_log), Some("IoT sensor reading completed successfully"))
}

// Test 6: 高性能计算环境遥测测试
pub test "高性能计算环境遥测测试" {
  // 创建高性能计算特定的资源属性
  let hpc_resource_attrs = [
    ("service.name", azimuth::StringValue("hpc-compute-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("hpc.cluster.name", azimuth::StringValue("research-cluster")),
    ("hpc.node.id", azimuth::StringValue("compute-node-42")),
    ("hpc.cpu.cores", azimuth::IntValue(64)),
    ("hpc.memory.total", azimuth::IntValue(256000)),  // 256GB
    ("hpc.gpu.count", azimuth::IntValue(8)),
    ("hpc.gpu.type", azimuth::StringValue("nvidia-a100")),
    ("hpc.interconnect", azimuth::StringValue("infiniband")),
    ("deployment.environment", azimuth::StringValue("hpc"))
  ]
  
  let hpc_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), hpc_resource_attrs)
  
  // 验证高性能计算资源属性
  assert_eq(azimuth::Resource::get_attribute(hpc_resource, "hpc.cluster.name"), Some(azimuth::StringValue("research-cluster")))
  assert_eq(azimuth::Resource::get_attribute(hpc_resource, "hpc.node.id"), Some(azimuth::StringValue("compute-node-42")))
  assert_eq(azimuth::Resource::get_attribute(hpc_resource, "hpc.cpu.cores"), Some(azimuth::IntValue(64)))
  assert_eq(azimuth::Resource::get_attribute(hpc_resource, "hpc.memory.total"), Some(azimuth::IntValue(256000)))
  assert_eq(azimuth::Resource::get_attribute(hpc_resource, "hpc.gpu.count"), Some(azimuth::IntValue(8)))
  assert_eq(azimuth::Resource::get_attribute(hpc_resource, "hpc.gpu.type"), Some(azimuth::StringValue("nvidia-a100")))
  
  // 创建高性能计算特定的Tracer
  let hpc_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "hpc-compute-tracer")
  
  // 创建高性能计算特定的Span
  let hpc_span = azimuth::Tracer::start_span(hpc_tracer, "hpc.parallel.computation")
  
  // 添加高性能计算特定事件
  azimuth::Span::add_event(hpc_span, "mpi.job.started", Some([
    ("mpi.rank", azimuth::IntValue(0)),
    ("mpi.size", azimuth::IntValue(128)),
    ("job.id", azimuth::StringValue("hpc-job-12345"))
  ]))
  
  azimuth::Span::add_event(hpc_span, "gpu.kernel.launch", Some([
    ("gpu.device.id", azimuth::IntValue(0)),
    ("kernel.name", azimuth::StringValue("matrix_multiply")),
    ("block.size", azimuth::IntValue(256)),
    ("grid.size", azimuth::IntValue(1024))
  ]))
  
  // 验证高性能计算Span
  assert_eq(azimuth::Span::name(hpc_span), "hpc.parallel.computation")
  
  // 创建高性能计算特定的度量
  let hpc_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "hpc-compute-meter")
  
  let cpu_utilization_gauge = azimuth::Meter::create_gauge(hpc_meter, "hpc.cpu.utilization")
  let memory_usage_gauge = azimuth::Meter::create_gauge(hpc_meter, "hpc.memory.usage")
  let gpu_utilization_gauge = azimuth::Meter::create_gauge(hpc_meter, "hpc.gpu.utilization")
  let network_throughput_counter = azimuth::Meter::create_counter(hpc_meter, "hpc.network.throughput")
  let computation_time_histogram = azimuth::Meter::create_histogram(hpc_meter, "hpc.computation.time")
  
  // 添加度量值
  azimuth::Counter::add(network_throughput_counter, 1024000.0)  // 1GB/s
  azimuth::Histogram::record(computation_time_histogram, 3600.5)  // 1 hour
  
  // 验证高性能计算度量
  assert_eq(cpu_utilization_gauge.name, "hpc.cpu.utilization")
  assert_eq(memory_usage_gauge.name, "hpc.memory.usage")
  assert_eq(gpu_utilization_gauge.name, "hpc.gpu.utilization")
  assert_eq(network_throughput_counter.name, "hpc.network.throughput")
  assert_eq(computation_time_histogram.name, "hpc.computation.time")
  
  // 创建高性能计算特定的日志
  let hpc_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "hpc-compute-logger")
  
  let hpc_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("HPC computation job completed successfully"),
    Some(azimuth::Attributes::new()),
    Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
    None,
    Some("hpc-trace-id"),
    Some("hpc-span-id"),
    Some(azimuth::Context::root())
  )
  
  azimuth::Logger::emit(hpc_logger, hpc_log)
  
  // 验证高性能计算日志
  assert_eq(azimuth::LogRecord::severity_number(hpc_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::body(hpc_log), Some("HPC computation job completed successfully"))
}