// 网络异常处理测试
// 测试遥测系统在网络异常情况下的处理能力

import "azimuth/azimuth"

pub test "HTTP客户端超时处理测试" {
  let http_client = azimuth::HttpClient::with_timeout_config(5000)  // 5秒超时
  
  // 测试正常请求
  let normal_headers = [("Content-Type", "application/json")]
  let normal_request = azimuth::HttpRequest::new("GET", "https://httpbin.org/delay/1", normal_headers, None)
  let normal_response = azimuth::HttpClient::execute(http_client, normal_request)
  
  assert_eq(azimuth::HttpResponse::status_code(normal_response), 200)
  
  // 测试超时请求
  let timeout_request = azimuth::HttpRequest::new("GET", "https://httpbin.org/delay/10", normal_headers, None)
  let timeout_response = azimuth::HttpClient::execute(http_client, timeout_request)
  
  // 应该收到超时错误
  assert_eq(azimuth::HttpResponse::status_code(timeout_response), 408)  // Request Timeout
}

pub test "网络连接重试机制测试" {
  let retry_client = azimuth::HttpClient::with_retry_config(3, 1000)  // 重试3次，间隔1秒
  
  // 测试可重试的错误
  let retry_headers = [("Content-Type", "application/json")]
  let retry_request = azimuth::HttpRequest::new("GET", "https://httpbin.org/status/503", retry_headers, None)
  let retry_response = azimuth::HttpClient::execute(retry_client, retry_request)
  
  // 应该尝试重试并最终返回错误
  assert_true(azimuth::HttpResponse::status_code(retry_response) >= 500)
  
  // 验证重试次数
  let retry_count = azimuth::HttpClient::get_retry_count(retry_client)
  assert_true(retry_count >= 1)
}

pub test "遥测数据导出网络异常处理测试" {
  let exporter = azimuth::TelemetryExporter::with_endpoint("https://invalid-endpoint.example.com/telemetry")
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "network-test")
  let span = azimuth::Tracer::start_span(tracer, "network-exception-test")
  
  // 尝试导出到无效端点
  let export_result = azimuth::TelemetryExporter::export_span(exporter, span)
  
  // 应该处理网络错误
  assert_false(azimuth::TelemetryExporter::is_success(export_result))
  assert_true(azimuth::TelemetryExporter::is_network_error(export_result))
  
  // 测试备用端点
  let fallback_exporter = azimuth::TelemetryExporter::with_fallback_endpoints([
    "https://invalid-endpoint.example.com/telemetry",
    "https://httpbin.org/post"  // 有效端点
  ])
  
  let fallback_result = azimuth::TelemetryExporter::export_span(fallback_exporter, span)
  
  // 应该成功使用备用端点
  assert_true(azimuth::TelemetryExporter::is_success(fallback_result))
}

pub test "部分网络故障处理测试" {
  let batch_exporter = azimuth::BatchTelemetryExporter::new()
  
  // 创建多个Span
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "partial-failure-test")
  let spans = []
  
  for i in 0..10 {
    let span = azimuth::Tracer::start_span(tracer, "partial-span-" + i.to_string())
    spans.push(span)
  }
  
  // 模拟部分导出失败
  let partial_result = azimuth::BatchTelemetryExporter::export_with_failure_simulation(
    batch_exporter, 
    spans, 
    0.3  // 30%失败率
  )
  
  // 验证部分成功处理
  let success_count = azimuth::BatchTelemetryExporter::success_count(partial_result)
  let failure_count = azimuth::BatchTelemetryExporter::failure_count(partial_result)
  
  assert_true(success_count > 0)
  assert_true(failure_count > 0)
  assert_eq(success_count + failure_count, 10)
  
  // 验证失败的Span被正确记录
  let failed_spans = azimuth::BatchTelemetryExporter::failed_spans(partial_result)
  assert_eq(failed_spans.length(), failure_count)
}

pub test "网络断开和恢复测试" {
  let resilient_exporter = azimuth::ResilientTelemetryExporter::new()
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "resilience-test")
  
  // 模拟网络断开
  azimuth::ResilientTelemetryExporter::simulate_network_disconnect(resilient_exporter)
  
  // 在网络断开时尝试导出
  let span1 = azimuth::Tracer::start_span(tracer, "disconnected-span")
  let disconnected_result = azimuth::ResilientTelemetryExporter::export_span(resilient_exporter, span1)
  
  // 应该缓存导出数据
  assert_false(azimuth::ResilientTelemetryExporter::is_success(disconnected_result))
  assert_eq(azimuth::ResilientTelemetryExporter::cached_count(resilient_exporter), 1)
  
  // 创建更多Span
  for i in 0..5 {
    let span = azimuth::Tracer::start_span(tracer, "cached-span-" + i.to_string())
    azimuth::ResilientTelemetryExporter::export_span(resilient_exporter, span)
  }
  
  // 验证缓存的数据量
  assert_eq(azimuth::ResilientTelemetryExporter::cached_count(resilient_exporter), 6)
  
  // 模拟网络恢复
  azimuth::ResilientTelemetryExporter::simulate_network_reconnect(resilient_exporter)
  
  // 触发缓存数据导出
  let flush_result = azimuth::ResilientTelemetryExporter::flush_cached_data(resilient_exporter)
  
  // 验证缓存数据成功导出
  assert_true(azimuth::ResilientTelemetryExporter::is_success(flush_result))
  assert_eq(azimuth::ResilientTelemetryExporter::cached_count(resilient_exporter), 0)
}

pub test "网络性能降级测试" {
  let adaptive_exporter = azimuth::AdaptiveTelemetryExporter::new()
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "adaptive-test")
  
  // 测试正常网络条件下的性能
  let normal_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(tracer, "normal-span-" + i.to_string())
    azimuth::AdaptiveTelemetryExporter::export_span(adaptive_exporter, span)
  }
  
  let normal_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let normal_duration = normal_end - normal_start
  
  // 模拟网络性能降级
  azimuth::AdaptiveTelemetryExporter::simulate_slow_network(adaptive_exporter, 2000)  // 2秒延迟
  
  // 测试网络降级后的性能
  let degraded_start = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  
  for i in 0..20 {
    let span = azimuth::Tracer::start_span(tracer, "degraded-span-" + i.to_string())
    azimuth::AdaptiveTelemetryExporter::export_span(adaptive_exporter, span)
  }
  
  let degraded_end = azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  let degraded_duration = degraded_end - degraded_start
  
  // 验证自适应调整
  assert_true(degraded_duration > normal_duration)  // 降级后应该更慢
  
  // 验证自适应策略调整
  let batch_size = azimuth::AdaptiveTelemetryExporter::get_batch_size(adaptive_exporter)
  let retry_delay = azimuth::AdaptiveTelemetryExporter::get_retry_delay(adaptive_exporter)
  
  // 在网络降级时应该调整批处理大小和重试延迟
  assert_true(batch_size < 20)  // 批处理大小应该减小
  assert_true(retry_delay > 1000)  // 重试延迟应该增加
}

pub test "网络认证和安全异常处理测试" {
  let secure_exporter = azimuth::SecureTelemetryExporter::with_auth_token("test-token")
  
  let tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "security-test")
  let span = azimuth::Tracer::start_span(tracer, "security-span")
  
  // 测试无效认证令牌
  let invalid_exporter = azimuth::SecureTelemetryExporter::with_auth_token("invalid-token")
  let auth_result = azimuth::SecureTelemetryExporter::export_span(invalid_exporter, span)
  
  // 应该收到认证错误
  assert_false(azimuth::SecureTelemetryExporter::is_success(auth_result))
  assert_true(azimuth::SecureTelemetryExporter::is_auth_error(auth_result))
  
  // 测试令牌刷新机制
  let refreshed_exporter = azimuth::SecureTelemetryExporter::with_token_refresh("initial-token")
  
  // 模拟令牌过期
  azimuth::SecureTelemetryExporter::simulate_token_expiry(refreshed_exporter)
  
  let refresh_result = azimuth::SecureTelemetryExporter::export_span(refreshed_exporter, span)
  
  // 应该自动刷新令牌并成功导出
  assert_true(azimuth::SecureTelemetryExporter::is_success(refresh_result))
  assert_true(azimuth::SecureTelemetryExporter::was_token_refreshed(refreshed_exporter))
  
  // 测试SSL/TLS错误处理
  let tls_exporter = azimuth::SecureTelemetryExporter::with_tls_validation()
  let tls_result = azimuth::SecureTelemetryExporter::export_span(tls_exporter, span)
  
  // 应该正确处理TLS验证
  assert_true(azimuth::SecureTelemetryExporter::has_tls_validation(tls_exporter))
}