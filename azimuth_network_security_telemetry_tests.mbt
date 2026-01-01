// Azimuth Network Security Telemetry Tests
// 网络安全遥测测试用例
// 测试网络安全事件监控、威胁检测和安全遥测数据收集

import "azimuth/azimuth"

// Test 1: 网络安全事件监控测试
pub test "网络安全事件监控测试" {
  // 创建网络安全遥测组件
  let security_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "security-tracer")
  let security_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "security-meter")
  let security_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "security-logger")
  
  // 创建网络安全事件Span
  let security_span = azimuth::Tracer::start_span(security_tracer, "network.security.event")
  azimuth::Span::add_event(security_span, "security.monitoring.started", Some([
    ("monitoring.type", azimuth::StringValue("network.security")),
    ("start.time", azimuth::IntValue(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())))
  ]))
  
  // 模拟各种网络安全事件
  let security_events = [
    {
      "type": "authentication.failure",
      "severity": "medium",
      "source.ip": "192.168.1.100",
      "target.service": "auth.service",
      "count": 5
    },
    {
      "type": "unauthorized.access",
      "severity": "high",
      "source.ip": "10.0.0.50",
      "target.service": "admin.panel",
      "count": 1
    },
    {
      "type": "suspicious.pattern",
      "severity": "low",
      "source.ip": "172.16.0.25",
      "target.service": "api.gateway",
      "count": 12
    },
    {
      "type": "data.exfiltration.attempt",
      "severity": "critical",
      "source.ip": "203.0.113.10",
      "target.service": "database",
      "count": 3
    }
  ]
  
  // 处理安全事件
  for event in security_events {
    // 创建事件特定的Span
    let event_span = azimuth::Tracer::start_span(security_tracer, "security.event." + event["type"])
    
    // 添加事件属性
    azimuth::Span::add_event(event_span, "security.event.detected", Some([
      ("event.type", azimuth::StringValue(event["type"])),
      ("event.severity", azimuth::StringValue(event["severity"])),
      ("source.ip", azimuth::StringValue(event["source.ip"])),
      ("target.service", azimuth::StringValue(event["target.service"])),
      ("event.count", azimuth::IntValue(Int::from_string(event["count"])))
    ]))
    
    // 更新安全度量
    let security_counter = azimuth::Meter::create_counter(security_meter, "security.events.total")
    azimuth::Counter::add(security_counter, Int::from_string(event["count"]).to_double(), Some([
      ("event.type", azimuth::StringValue(event["type"])),
      ("event.severity", azimuth::StringValue(event["severity"]))
    ]))
    
    // 记录安全日志
    let log_severity = match event["severity"] {
      "critical" => azimuth::Error,
      "high" => azimuth::Error,
      "medium" => azimuth::Warn,
      _ => azimuth::Info
    }
    
    let log_record = azimuth::LogRecord::new_with_context(
      log_severity,
      Some("Security event detected: " + event["type"] + " from " + event["source.ip"]),
      Some([
        ("event.type", azimuth::StringValue(event["type"])),
        ("event.severity", azimuth::StringValue(event["severity"])),
        ("source.ip", azimuth::StringValue(event["source.ip"])),
        ("target.service", azimuth::StringValue(event["target.service"]))
      ]),
      Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
      None,
      Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(security_span))),
      Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(event_span))),
      Some(azimuth::Context::root())
    )
    
    azimuth::Logger::emit(security_logger, log_record)
    azimuth::Span::end(event_span)
  }
  
  // 创建安全威胁检测度量
  let threat_counter = azimuth::Meter::create_counter(security_meter, "security.threats.detected")
  let threat_histogram = azimuth::Meter::create_histogram(security_meter, "security.threat.response.time", Some("Security threat response time"), Some("ms"))
  
  // 模拟威胁检测和响应
  let threat_types = ["malware", "phishing", "ddos", "injection", "xss"]
  for threat_type in threat_types {
    let detection_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 模拟威胁检测过程
    azimuth::Clock::sleep(Int::from_string(match threat_type {
      "malware" => "50",
      "phishing" => "30",
      "ddos" => "10",
      "injection" => "40",
      _ => "20"
    }))
    
    let detection_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    let response_time = (detection_end - detection_start) / 1000000L  // 转换为毫秒
    
    // 记录威胁检测度量
    azimuth::Counter::add(threat_counter, 1.0, Some([
      ("threat.type", azimuth::StringValue(threat_type)),
      ("detection.status", azimuth::StringValue("detected"))
    ]))
    
    azimuth::Histogram::record(threat_histogram, response_time.to_double(), Some([
      ("threat.type", azimuth::StringValue(threat_type))
    ]))
    
    // 记录威胁检测事件
    azimuth::Span::add_event(security_span, "threat.detected", Some([
      ("threat.type", azimuth::StringValue(threat_type)),
      ("response.time", azimuth::IntValue(response_time)),
      ("detection.time", azimuth::IntValue(detection_end))
    ]))
  }
  
  azimuth::Span::end(security_span)
  
  // 验证安全遥测数据
  let security_metrics = azimuth::MeterProvider::get_metrics(security_meter)
  assert_true(security_metrics.length() > 0)
  
  let security_logs = azimuth::LoggerProvider::get_logs(security_logger)
  assert_true(security_logs.length() >= security_events.length())
}

// Test 2: 网络流量异常检测测试
pub test "网络流量异常检测测试" {
  // 创建网络流量监控组件
  let network_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "network-tracer")
  let network_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "network-meter")
  
  // 创建网络流量监控Span
  let monitoring_span = azimuth::Tracer::start_span(network_tracer, "network.traffic.monitoring")
  
  // 创建网络流量度量
  let bytes_in_counter = azimuth::Meter::create_counter(network_meter, "network.bytes.in")
  let bytes_out_counter = azimuth::Meter::create_counter(network_meter, "network.bytes.out")
  let packets_in_counter = azimuth::Meter::create_counter(network_meter, "network.packets.in")
  let packets_out_counter = azimuth::Meter::create_counter(network_meter, "network.packets.out")
  let connection_gauge = azimuth::Meter::create_gauge(network_meter, "network.connections.active")
  let connection_rate_histogram = azimuth::Meter::create_histogram(network_meter, "network.connection.rate", Some("Network connection rate"), Some("connections/sec"))
  
  // 模拟正常网络流量
  let normal_traffic_samples = []
  for i in 0..100 {
    let bytes_in = 1000 + (i % 5000)  // 1000-6000 bytes
    let bytes_out = 800 + (i % 4000)   // 800-4800 bytes
    let packets_in = 10 + (i % 50)     // 10-60 packets
    let packets_out = 8 + (i % 40)     // 8-48 packets
    let connections = 20 + (i % 30)    // 20-50 connections
    
    // 记录正常流量度量
    azimuth::Counter::add(bytes_in_counter, bytes_in.to_double())
    azimuth::Counter::add(bytes_out_counter, bytes_out.to_double())
    azimuth::Counter::add(packets_in_counter, packets_in.to_double())
    azimuth::Counter::add(packets_out_counter, packets_out.to_double())
    
    // 模拟连接率
    let connection_rate = 1.0 + (i % 10).to_double() * 0.5  // 1-6 connections/sec
    azimuth::Histogram::record(connection_rate_histogram, connection_rate)
    
    normal_traffic_samples.push({
      "bytes_in": bytes_in,
      "bytes_out": bytes_out,
      "packets_in": packets_in,
      "packets_out": packets_out,
      "connections": connections,
      "connection_rate": connection_rate
    })
  }
  
  // 模拟异常网络流量
  let anomaly_events = [
    {
      "type": "traffic.surge",
      "description": "Sudden increase in incoming traffic",
      "bytes_in": 50000,
      "bytes_out": 10000,
      "packets_in": 500,
      "packets_out": 100,
      "severity": "high"
    },
    {
      "type": "unusual.protocol",
      "description": "Unusual protocol traffic detected",
      "bytes_in": 2000,
      "bytes_out": 8000,
      "packets_in": 15,
      "packets_out": 60,
      "severity": "medium"
    },
    {
      "type": "port.scan",
      "description": "Possible port scanning activity",
      "bytes_in": 500,
      "bytes_out": 1500,
      "packets_in": 100,
      "packets_out": 50,
      "severity": "high"
    },
    {
      "type": "data.exfiltration",
      "description": "Large amount of outgoing data transfer",
      "bytes_in": 1000,
      "bytes_out": 100000,
      "packets_in": 10,
      "packets_out": 200,
      "severity": "critical"
    }
  ]
  
  // 处理异常事件
  for anomaly in anomaly_events {
    // 创建异常检测Span
    let anomaly_span = azimuth::Tracer::start_span(network_tracer, "network.anomaly." + anomaly["type"])
    
    // 记录异常流量度量
    azimuth::Counter::add(bytes_in_counter, Int::from_string(anomaly["bytes_in"]).to_double(), Some([
      ("traffic.type", azimuth::StringValue("anomaly")),
      ("anomaly.type", azimuth::StringValue(anomaly["type"]))
    ]))
    
    azimuth::Counter::add(bytes_out_counter, Int::from_string(anomaly["bytes_out"]).to_double(), Some([
      ("traffic.type", azimuth::StringValue("anomaly")),
      ("anomaly.type", azimuth::StringValue(anomaly["type"]))
    ]))
    
    azimuth::Counter::add(packets_in_counter, Int::from_string(anomaly["packets_in"]).to_double(), Some([
      ("traffic.type", azimuth::StringValue("anomaly")),
      ("anomaly.type", azimuth::StringValue(anomaly["type"]))
    ]))
    
    azimuth::Counter::add(packets_out_counter, Int::from_string(anomaly["packets_out"]).to_double(), Some([
      ("traffic.type", azimuth::StringValue("anomaly")),
      ("anomaly.type", azimuth::StringValue(anomaly["type"]))
    ]))
    
    // 添加异常事件
    azimuth::Span::add_event(anomaly_span, "network.anomaly.detected", Some([
      ("anomaly.type", azimuth::StringValue(anomaly["type"])),
      ("anomaly.description", azimuth::StringValue(anomaly["description"])),
      ("anomaly.severity", azimuth::StringValue(anomaly["severity"])),
      ("bytes.in", azimuth::IntValue(Int::from_string(anomaly["bytes_in"]))),
      ("bytes.out", azimuth::IntValue(Int::from_string(anomaly["bytes_out"])))
    ]))
    
    // 创建异常度量
    let anomaly_counter = azimuth::Meter::create_counter(network_meter, "network.anomalies.detected")
    azimuth::Counter::add(anomaly_counter, 1.0, Some([
      ("anomaly.type", azimuth::StringValue(anomaly["type"])),
      ("anomaly.severity", azimuth::StringValue(anomaly["severity"]))
    ]))
    
    azimuth::Span::end(anomaly_span)
  }
  
  // 创建网络性能基线度量
  let baseline_calculator = azimuth::NetworkBaseline::new()
  
  // 计算正常流量的基线
  for sample in normal_traffic_samples {
    azimuth::NetworkBaseline::add_sample(baseline_calculator, {
      "bytes_in": Int::from_string(sample["bytes_in"]).to_double(),
      "bytes_out": Int::from_string(sample["bytes_out"]).to_double(),
      "packets_in": Int::from_string(sample["packets_in"]).to_double(),
      "packets_out": Int::from_string(sample["packets_out"]).to_double()
    })
  }
  
  // 获取基线统计
  let baseline_stats = azimuth::NetworkBaseline::get_statistics(baseline_calculator)
  
  // 验证基线计算
  assert_true(azimuth::NetworkBaseline::has_mean(baseline_stats, "bytes_in"))
  assert_true(azimuth::NetworkBaseline::has_stddev(baseline_stats, "bytes_in"))
  assert_true(azimuth::NetworkBaseline::has_percentile(baseline_stats, "bytes_in", 95))
  
  // 添加基线度量到监控Span
  azimuth::Span::add_event(monitoring_span, "network.baseline.calculated", Some([
    ("baseline.bytes_in.mean", azimuth::FloatValue(azimuth::NetworkBaseline::get_mean(baseline_stats, "bytes_in"))),
    ("baseline.bytes_in.stddev", azimuth::FloatValue(azimuth::NetworkBaseline::get_stddev(baseline_stats, "bytes_in"))),
    ("baseline.bytes_in.p95", azimuth::FloatValue(azimuth::NetworkBaseline::get_percentile(baseline_stats, "bytes_in", 95)))
  ]))
  
  azimuth::Span::end(monitoring_span)
  
  // 验证网络流量遥测数据
  let network_metrics = azimuth::MeterProvider::get_metrics(network_meter)
  assert_true(network_metrics.length() > 0)
  
  // 验证异常检测
  let anomaly_count = azimuth::MeterProvider::get_metric_value(network_meter, "network.anomalies.detected")
  assert_true(anomaly_count >= anomaly_events.length().to_double())
}

// Test 3: 身份验证和授权安全遥测测试
pub test "身份验证和授权安全遥测测试" {
  // 创建身份验证和授权遥测组件
  let auth_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "auth-tracer")
  let auth_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "auth-meter")
  let auth_logger = azimuth::LoggerProvider::get_logger(azimuth::LoggerProvider::default(), "auth-logger")
  
  // 创建身份验证监控Span
  let auth_monitoring_span = azimuth::Tracer::start_span(auth_tracer, "auth.security.monitoring")
  
  // 创建身份验证度量
  let auth_attempts_counter = azimuth::Meter::create_counter(auth_meter, "auth.attempts.total")
  let auth_success_counter = azimuth::Meter::create_counter(auth_meter, "auth.success.total")
  let auth_failure_counter = azimuth::Meter::create_counter(auth_meter, "auth.failures.total")
  let auth_duration_histogram = azimuth::Meter::create_histogram(auth_meter, "auth.processing.duration", Some("Authentication processing duration"), Some("ms"))
  
  // 创建授权度量
  let authz_attempts_counter = azimuth::Meter::create_counter(auth_meter, "authz.attempts.total")
  let authz_success_counter = azimuth::Meter::create_counter(auth_meter, "authz.success.total")
  let authz_denied_counter = azimuth::Meter::create_counter(auth_meter, "authz.denied.total")
  
  // 模拟身份验证事件
  let auth_events = [
    {
      "user.id": "user123",
      "auth.method": "password",
      "auth.result": "success",
      "client.ip": "192.168.1.100",
      "user.agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      "processing.time": "45"
    },
    {
      "user.id": "user456",
      "auth.method": "oauth",
      "auth.result": "success",
      "client.ip": "10.0.0.50",
      "user.agent": "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
      "processing.time": "120"
    },
    {
      "user.id": "user789",
      "auth.method": "password",
      "auth.result": "failure",
      "client.ip": "203.0.113.10",
      "user.agent": "curl/7.68.0",
      "processing.time": "25"
    },
    {
      "user.id": "user101",
      "auth.method": "certificate",
      "auth.result": "success",
      "client.ip": "172.16.0.25",
      "user.agent": "Java/11.0.2",
      "processing.time": "80"
    }
  ]
  
  // 处理身份验证事件
  for auth_event in auth_events {
    let auth_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 创建身份验证Span
    let auth_span = azimuth::Tracer::start_span(auth_tracer, "auth.attempt")
    
    // 添加身份验证属性
    azimuth::Span::add_event(auth_span, "auth.started", Some([
      ("user.id", azimuth::StringValue(auth_event["user.id"])),
      ("auth.method", azimuth::StringValue(auth_event["auth.method"])),
      ("client.ip", azimuth::StringValue(auth_event["client.ip"])),
      ("user.agent", azimuth::StringValue(auth_event["user.agent"]))
    ]))
    
    // 记录身份验证度量
    azimuth::Counter::add(auth_attempts_counter, 1.0, Some([
      ("auth.method", azimuth::StringValue(auth_event["auth.method"]))
    ]))
    
    // 记录处理时间
    let processing_time = Int::from_string(auth_event["processing.time"])
    azimuth::Histogram::record(auth_duration_histogram, processing_time.to_double(), Some([
      ("auth.method", azimuth::StringValue(auth_event["auth.method"]))
    ]))
    
    // 模拟处理延迟
    azimuth::Clock::sleep(processing_time / 10)  // 缩短实际等待时间
    
    let auth_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    
    // 根据结果记录不同的度量
    if auth_event["auth.result"] == "success" {
      azimuth::Counter::add(auth_success_counter, 1.0, Some([
        ("auth.method", azimuth::StringValue(auth_event["auth.method"]))
      ]))
      
      azimuth::Span::add_event(auth_span, "auth.success", Some([
        ("auth.duration", azimuth::IntValue((auth_end - auth_start) / 1000000L))
      ]))
    } else {
      azimuth::Counter::add(auth_failure_counter, 1.0, Some([
        ("auth.method", azimuth::StringValue(auth_event["auth.method"]))
      ]))
      
      azimuth::Span::add_event(auth_span, "auth.failure", Some([
        ("auth.duration", azimuth::IntValue((auth_end - auth_start) / 1000000L)),
        ("failure.reason", azimuth::StringValue("invalid.credentials"))
      ]))
      
      // 记录失败日志
      let failure_log = azimuth::LogRecord::new_with_context(
        azimuth::Warn,
        Some("Authentication failed for user: " + auth_event["user.id"]),
        Some([
          ("user.id", azimuth::StringValue(auth_event["user.id"])),
          ("auth.method", azimuth::StringValue(auth_event["auth.method"])),
          ("client.ip", azimuth::StringValue(auth_event["client.ip"]))
        ]),
        Some(auth_end),
        None,
        Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(auth_monitoring_span))),
        Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(auth_span))),
        Some(azimuth::Context::root())
      )
      
      azimuth::Logger::emit(auth_logger, failure_log)
    }
    
    azimuth::Span::end(auth_span)
  }
  
  // 模拟授权事件
  let authz_events = [
    {
      "user.id": "user123",
      "resource": "/api/users/profile",
      "action": "read",
      "authz.result": "granted",
      "policy": "user.read.own"
    },
    {
      "user.id": "user456",
      "resource": "/api/admin/settings",
      "action": "write",
      "authz.result": "denied",
      "policy": "admin.write"
    },
    {
      "user.id": "user789",
      "resource": "/api/data/export",
      "action": "execute",
      "authz.result": "granted",
      "policy": "data.export"
    }
  ]
  
  // 处理授权事件
  for authz_event in authz_events {
    // 创建授权Span
    let authz_span = azimuth::Tracer::start_span(auth_tracer, "authz.attempt")
    
    // 添加授权属性
    azimuth::Span::add_event(authz_span, "authz.started", Some([
      ("user.id", azimuth::StringValue(authz_event["user.id"])),
      ("resource", azimuth::StringValue(authz_event["resource"])),
      ("action", azimuth::StringValue(authz_event["action"])),
      ("policy", azimuth::StringValue(authz_event["policy"]))
    ]))
    
    // 记录授权度量
    azimuth::Counter::add(authz_attempts_counter, 1.0, Some([
      ("action", azimuth::StringValue(authz_event["action"])),
      ("resource.type", azimuth::StringValue(match authz_event["resource"].substring(0, 5) {
        "/api/" => "api"
        _ => "other"
      }))
    ]))
    
    // 根据结果记录不同的度量
    if authz_event["authz.result"] == "granted" {
      azimuth::Counter::add(authz_success_counter, 1.0, Some([
        ("action", azimuth::StringValue(authz_event["action"])),
        ("policy", azimuth::StringValue(authz_event["policy"]))
      ]))
      
      azimuth::Span::add_event(authz_span, "authz.granted", Some([
        ("policy", azimuth::StringValue(authz_event["policy"]))
      ]))
    } else {
      azimuth::Counter::add(authz_denied_counter, 1.0, Some([
        ("action", azimuth::StringValue(authz_event["action"])),
        ("policy", azimuth::StringValue(authz_event["policy"]))
      ]))
      
      azimuth::Span::add_event(authz_span, "authz.denied", Some([
        ("policy", azimuth::StringValue(authz_event["policy"])),
        ("denial.reason", azimuth::StringValue("insufficient.permissions"))
      ]))
      
      // 记录拒绝日志
      let denial_log = azimuth::LogRecord::new_with_context(
        azimuth::Warn,
        Some("Authorization denied for user: " + authz_event["user.id"] + " on resource: " + authz_event["resource"]),
        Some([
          ("user.id", azimuth::StringValue(authz_event["user.id"])),
          ("resource", azimuth::StringValue(authz_event["resource"])),
          ("action", azimuth::StringValue(authz_event["action"])),
          ("policy", azimuth::StringValue(authz_event["policy"]))
        ]),
        Some(azimuth::Clock::now_unix_nanos(azimuth::Clock::system())),
        None,
        Some(azimuth::SpanContext::trace_id(azimuth::Span::span_context(auth_monitoring_span))),
        Some(azimuth::SpanContext::span_id(azimuth::Span::span_context(authz_span))),
        Some(azimuth::Context::root())
      )
      
      azimuth::Logger::emit(auth_logger, denial_log)
    }
    
    azimuth::Span::end(authz_span)
  }
  
  // 创建安全会话度量
  let active_sessions_gauge = azimuth::Meter::create_gauge(auth_meter, "auth.sessions.active")
  let session_duration_histogram = azimuth::Meter::create_histogram(auth_meter, "auth.session.duration", Some("Authentication session duration"), Some("minutes"))
  
  // 模拟活动会话
  let active_sessions = 25 + (azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) % 50).to_int()  // 25-75个活动会话
  azimuth::Gauge::record(active_sessions_gauge, active_sessions.to_double())
  
  // 模拟会话持续时间
  for i in 0..50 {
    let session_duration = 5 + (i % 120)  // 5-125分钟
    azimuth::Histogram::record(session_duration_histogram, session_duration.to_double())
  }
  
  // 添加会话统计到监控Span
  azimuth::Span::add_event(auth_monitoring_span, "auth.session.stats", Some([
    ("active.sessions", azimuth::IntValue(active_sessions)),
    ("avg.session.duration", azimuth::FloatValue(62.5))
  ]))
  
  azimuth::Span::end(auth_monitoring_span)
  
  // 验证身份验证和授权遥测数据
  let auth_metrics = azimuth::MeterProvider::get_metrics(auth_meter)
  assert_true(auth_metrics.length() > 0)
  
  let auth_logs = azimuth::LoggerProvider::get_logs(auth_logger)
  assert_true(auth_logs.length() > 0)
  
  // 验证身份验证成功率
  let total_attempts = azimuth::MeterProvider::get_metric_value(auth_meter, "auth.attempts.total")
  let total_success = azimuth::MeterProvider::get_metric_value(auth_meter, "auth.success.total")
  let total_failures = azimuth::MeterProvider::get_metric_value(auth_meter, "auth.failures.total")
  
  assert_eq(total_attempts, total_success + total_failures)
  assert_true(total_success > 0)
  assert_true(total_failures > 0)
  
  // 验证授权统计
  let total_authz_attempts = azimuth::MeterProvider::get_metric_value(auth_meter, "authz.attempts.total")
  let total_authz_success = azimuth::MeterProvider::get_metric_value(auth_meter, "authz.success.total")
  let total_authz_denied = azimuth::MeterProvider::get_metric_value(auth_meter, "authz.denied.total")
  
  assert_eq(total_authz_attempts, total_authz_success + total_authz_denied)
}