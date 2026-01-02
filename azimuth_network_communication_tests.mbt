// Azimuth Network Communication Tests
// 测试网络通信、HTTP客户端和遥测数据传输

test "HTTP客户端基本操作" {
  // 测试HTTP客户端基本操作
  let http_client = HttpClient::new()
  
  // 配置HTTP客户端
  HttpClient::set_timeout(http_client, 5000)  // 5秒超时
  HttpClient::set_user_agent(http_client, "Azimuth-Telemetry/2.1.0")
  HttpClient::add_header(http_client, "Accept", "application/json")
  HttpClient::add_header(http_client, "Content-Type", "application/json")
  
  // 测试GET请求
  let get_response = HttpClient::get(http_client, "https://httpbin.org/get")
  assert_eq(get_response.status_code, 200)
  assert_true(get_response.body.contains("httpbin.org"))
  assert_true(get_response.headers.contains("content-type"))
  
  // 测试POST请求
  let post_data = """
  {
    "trace_id": "abc123def456",
    "span_data": {
      "name": "test.operation",
      "duration": 125000000,
      "status": "OK"
    }
  }
  """
  
  let post_response = HttpClient::post(
    http_client, 
    "https://httpbin.org/post", 
    post_data
  )
  assert_eq(post_response.status_code, 200)
  assert_true(post_response.body.contains("trace_id"))
  assert_true(post_response.body.contains("test.operation"))
  
  // 测试PUT请求
  let put_data = """
  {
    "config": {
      "sampling_rate": 0.1,
      "batch_size": 100
    }
  }
  """
  
  let put_response = HttpClient::put(
    http_client,
    "https://httpbin.org/put",
    put_data
  )
  assert_eq(put_response.status_code, 200)
  assert_true(put_response.body.contains("sampling_rate"))
  
  // 测试DELETE请求
  let delete_response = HttpClient::delete(http_client, "https://httpbin.org/delete")
  assert_eq(delete_response.status_code, 200)
}

test "遥测数据批量传输" {
  // 测试遥测数据批量传输
  let telemetry_client = TelemetryClient::new("https://collector.example.com:4317")
  
  // 生成批量遥测数据
  let telemetry_batch = []
  let base_timestamp = 1735689600000000000L
  
  for i in 0..=49 {
    let span_data = {
      "trace_id": "trace-" + (i % 10).to_string(),
      "span_id": "span-" + i.to_string(),
      "parent_span_id": if i > 0 { "span-" + (i - 1).to_string() } else { "" },
      "operation_name": "operation." + (i % 5).to_string(),
      "start_time": base_timestamp + (i * 1000000L),
      "duration": (i + 1) * 1000,
      "status": if i % 20 == 0 { "ERROR" } else { "OK" },
      "attributes": {
        "service.name": "test-service",
        "service.version": "1.0.0",
        "host.name": "host-" + (i % 3).to_string(),
        "region": ["us-west-2", "us-east-1", "eu-west-1"][i % 3]
      },
      "events": if i % 10 == 0 {
        [
          {
            "name": "custom.event",
            "timestamp": base_timestamp + (i * 1000000L) + 500000L,
            "attributes": {
              "event.type": "checkpoint",
              "event.data": "checkpoint-" + i.to_string()
            }
          }
        ]
      } else {
        []
      }
    }
    telemetry_batch = telemetry_batch.push(span_data)
  }
  
  // 测试批量发送
  let batch_result = TelemetryClient::send_batch(telemetry_client, telemetry_batch)
  assert_true(batch_result.success)
  assert_eq(batch_result.sent_count, 50)
  assert_eq(batch_result.failed_count, 0)
  
  // 测试部分失败的批量发送
  let partial_batch = telemetry_batch.slice(0, 25)
  TelemetryClient::simulate_network_errors(telemetry_client, 0.2)  // 20%错误率
  
  let partial_result = TelemetryClient::send_batch(telemetry_client, partial_batch)
  assert_true(partial_result.success || partial_result.failed_count > 0)
  assert_eq(partial_result.sent_count + partial_result.failed_count, 25)
  
  TelemetryClient::simulate_network_errors(telemetry_client, 0.0)  // 重置错误率
}

test "网络重试和错误处理" {
  // 测试网络重试和错误处理
  let resilient_client = ResilientHttpClient::new()
  
  // 配置重试策略
  let retry_config = {
    "max_retries": 3,
    "base_delay": 1000,  // 1秒
    "max_delay": 10000,  // 10秒
    "backoff_multiplier": 2.0,
    "retryable_status_codes": [408, 429, 500, 502, 503, 504],
    "retryable_errors": ["timeout", "connection_error", "dns_error"]
  }
  
  ResilientHttpClient::configure_retry(resilient_client, retry_config)
  
  // 测试临时网络错误重试
  ResilientHttpClient::simulate_errors(resilient_client, {
    "error_type": "timeout",
    "error_count": 2,  // 前2次请求失败
    "then_succeed": true
  })
  
  let retry_result = ResilientHttpClient::get(
    resilient_client, 
    "https://httpbin.org/delay/1"
  )
  
  assert_true(retry_result.success)
  assert_eq(retry_result.attempt_count, 3)  // 初始请求 + 2次重试
  
  // 测试不可重试错误
  ResilientHttpClient::simulate_errors(resilient_client, {
    "error_type": "authentication_error",
    "error_count": 1,
    "then_succeed": false
  })
  
  let auth_result = ResilientHttpClient::get(
    resilient_client,
    "https://httpbin.org/status/401"
  )
  
  assert_false(auth_result.success)
  assert_eq(auth_result.attempt_count, 1)  // 不应重试认证错误
  
  // 测试熔断器
  let circuit_breaker = CircuitBreaker::new({
    "failure_threshold": 5,
    "recovery_timeout": 30000,  // 30秒
    "monitoring_period": 60000  // 60秒
  })
  
  // 触发熔断器
  ResilientHttpClient::simulate_errors(resilient_client, {
    "error_type": "server_error",
    "error_count": 6,  // 超过失败阈值
    "then_succeed": true
  })
  
  for i in 0..=5 {
    ResilientHttpClient::get(resilient_client, "https://httpbin.org/status/500")
  }
  
  // 验证熔断器状态
  assert_true(CircuitBreaker::is_open(circuit_breaker))
  
  // 测试熔断器下的请求
  let circuit_breaker_result = ResilientHttpClient::get(
    resilient_client,
    "https://httpbin.org/get"
  )
  
  assert_false(circuit_breaker_result.success)
  assert_true(circuit_breaker_result.error.contains("circuit breaker"))
}

test "遥测协议支持" {
  // 测试多种遥测协议支持
  let protocol_client = MultiProtocolTelemetryClient::new()
  
  // 生成测试数据
  let test_span = {
    "trace_id": "trace-protocol-test",
    "span_id": "span-protocol-test",
    "operation_name": "protocol.test.operation",
    "start_time": 1735689600000000000L,
    "duration": 150000000L,
    "status": "OK",
    "attributes": {
      "service.name": "protocol-test-service",
      "protocol": "test"
    }
  }
  
  // 测试OTLP协议
  let otlp_result = MultiProtocolTelemetryClient::send_otlp(
    protocol_client,
    [test_span],
    {
      "endpoint": "https://collector.example.com:4317/v1/traces",
      "headers": {
        "Content-Type": "application/x-protobuf"
      }
    }
  )
  assert_true(otlp_result.success)
  
  // 测试Jaeger协议
  let jaeger_result = MultiProtocolTelemetryClient::send_jaeger(
    protocol_client,
    [test_span],
    {
      "endpoint": "https://jaeger.example.com:14268/api/traces",
      "headers": {
        "Content-Type": "application/json"
      }
    }
  )
  assert_true(jaeger_result.success)
  
  // 测试Zipkin协议
  let zipkin_result = MultiProtocolTelemetryClient::send_zipkin(
    protocol_client,
    [test_span],
    {
      "endpoint": "https://zipkin.example.com:9411/api/v2/spans",
      "headers": {
        "Content-Type": "application/json"
      }
    }
  )
  assert_true(zipkin_result.success)
  
  // 测试OpenTelemetry指标协议
  let test_metrics = [
    {
      "name": "test.counter",
      "type": "counter",
      "value": 42.0,
      "attributes": {
        "service": "protocol-test"
      }
    },
    {
      "name": "test.histogram",
      "type": "histogram",
      "buckets": [10.0, 25.0, 50.0, 100.0],
      "counts": [5, 10, 3, 2],
      "sum": 825.0,
      "count": 20,
      "attributes": {
        "service": "protocol-test"
      }
    }
  ]
  
  let metrics_result = MultiProtocolTelemetryClient::send_metrics_otlp(
    protocol_client,
    test_metrics,
    {
      "endpoint": "https://collector.example.com:4317/v1/metrics",
      "headers": {
        "Content-Type": "application/x-protobuf"
      }
    }
  )
  assert_true(metrics_result.success)
}

test "网络性能和优化" {
  // 测试网络性能和优化功能
  let performance_client = OptimizedHttpClient::new()
  
  // 配置连接池
  let pool_config = {
    "max_connections": 20,
    "max_idle_connections": 5,
    "idle_timeout": 30000,  // 30秒
    "max_lifetime": 300000  // 5分钟
  }
  
  OptimizedHttpClient::configure_connection_pool(performance_client, pool_config)
  
  // 配置压缩
  OptimizedHttpClient::enable_compression(performance_client, "gzip")
  
  // 测试并发请求性能
  let concurrent_requests = []
  let start_time = get_current_timestamp()
  
  for i in 0..=9 {
    let request_future = OptimizedHttpClient::get_async(
      performance_client,
      "https://httpbin.org/delay/" + ((i % 3) + 1).to_string()
    )
    concurrent_requests = concurrent_requests.push(request_future)
  }
  
  // 等待所有请求完成
  let results = []
  for request in concurrent_requests {
    let result = wait_for_future(request)
    results = results.push(result)
  }
  
  let end_time = get_current_timestamp()
  let total_duration = end_time - start_time
  
  // 验证并发性能
  assert_eq(results.length(), 10)
  assert_true(results.all(fn(r) { r.success }))
  assert_true(total_duration < 5000000000L)  // 应该在5秒内完成（并发执行）
  
  // 测试压缩效果
  let large_data = "x" * 10000  // 10KB数据
  let compressed_request = OptimizedHttpClient::post(
    performance_client,
    "https://httpbin.org/post",
    large_data
  )
  
  assert_true(compressed_request.success)
  assert_true(compressed_request.request_size < large_data.length())  // 压缩后应该更小
  
  // 测试连接复用
  let reuse_result = OptimizedHttpClient::get(
    performance_client,
    "https://httpbin.org/get"
  )
  
  assert_true(reuse_result.success)
  assert_true(reuse_result.connection_reused)  // 应该复用连接
  
  // 测试HTTP/2支持
  let http2_result = OptimizedHttpClient::get(
    performance_client,
    "https://httpbin.org/get",
    {"protocol": "h2"}
  )
  
  assert_true(http2_result.success)
  assert_eq(http2_result.protocol, "HTTP/2")
}