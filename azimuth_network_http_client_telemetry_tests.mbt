// Azimuth Network and HTTP Client Telemetry Tests
// 网络遥测和HTTP客户端测试用例 - 专注于网络性能、HTTP监控和客户端遥测

// Test 1: HTTP客户端性能监控
test "HTTP client performance monitoring" {
  // 创建HTTP客户端监控器
  let http_monitor = HttpClientMonitor::new()
  
  // 配置监控指标
  Monitor::add_metric(http_monitor, "http_requests_total", MetricType::Counter)
  Monitor::add_metric(http_monitor, "http_request_duration_ms", MetricType::Histogram)
  Monitor::add_metric(http_monitor, "http_response_size_bytes", MetricType::Histogram)
  Monitor::add_metric(http_monitor, "http_connection_pool_active", MetricType::Gauge)
  Monitor::add_metric(http_monitor, "http_connection_pool_idle", MetricType::Gauge)
  
  // 创建受监控的HTTP客户端
  let monitored_client = MonitoredHttpClient::new("https://api.example.com")
  HttpClient::set_monitor(monitored_client, http_monitor)
  HttpClient::set_timeout(monitored_client, 5000) // 5秒超时
  HttpClient::set_connection_pool_size(monitored_client, 10)
  
  // 执行各种HTTP请求
  let endpoints = [
    ("/users", "GET"),
    ("/users/123", "GET"),
    ("/users", "POST"),
    ("/users/123", "PUT"),
    ("/users/123", "DELETE"),
    ("/products", "GET"),
    ("/products/search", "GET"),
    ("/orders", "GET"),
    ("/orders", "POST"),
    ("/health", "GET")
  ]
  
  for (endpoint, method) in endpoints {
    // 模拟不同响应时间的请求
    let response_time = 100 + Math::random() * 900 // 100-1000ms
    let response_size = 100 + Math::random() * 10000 // 100-10100字节
    let status_code = if Math::random() < 0.95 { 200 } else { 500 } // 5%错误率
    
    // 模拟HTTP请求
    let request = HttpRequest::new(method, endpoint)
    HttpClient::add_header(request, "Content-Type", "application/json")
    HttpClient::add_header(request, "User-Agent", "Azimuth-Telemetry/1.0")
    
    let response = HttpResponse::new(status_code)
    HttpResponse::set_header(response, "Content-Type", "application/json")
    HttpResponse::set_body_size(response, response_size)
    
    // 记录请求指标
    let start_time = Time::now()
    Monitor::record_counter(http_monitor, "http_requests_total", 1, [
      ("method", method),
      ("endpoint", endpoint),
      ("status_code", status_code.to_string())
    ])
    
    Monitor::record_histogram(http_monitor, "http_request_duration_ms", response_time, [
      ("method", method),
      ("endpoint", endpoint)
    ])
    
    Monitor::record_histogram(http_monitor, "http_response_size_bytes", response_size.to_float(), [
      ("method", method),
      ("endpoint", endpoint)
    ])
    
    // 模拟连接池状态变化
    let active_connections = 1 + Math::random() * 5.0
    let idle_connections = Math::random() * 5.0
    
    Monitor::set_gauge(http_monitor, "http_connection_pool_active", active_connections)
    Monitor::set_gauge(http_monitor, "http_connection_pool_idle", idle_connections)
    
    Thread::sleep(response_time.to_int()) // 模拟网络延迟
  }
  
  // 验证监控指标
  let metrics = Monitor::get_metrics(http_monitor)
  
  let request_counter = Metrics::get_metric(metrics, "http_requests_total")
  match request_counter {
    Some(counter) => {
      assert_true(counter.value > 0)
      assert_true(counter.labels.length() > 0)
    }
    None => assert_true(false)
  }
  
  let duration_histogram = Metrics::get_metric(metrics, "http_request_duration_ms")
  match duration_histogram {
    Some(histogram) => {
      assert_true(histogram.count > 0)
      assert_true(histogram.sum > 0)
      assert_true(histogram.average > 0)
    }
    None => assert_true(false)
  }
  
  let response_size_histogram = Metrics::get_metric(metrics, "http_response_size_bytes")
  match response_size_histogram {
    Some(histogram) => {
      assert_true(histogram.count > 0)
      assert_true(histogram.sum > 0)
    }
    None => assert_true(false)
  }
  
  let active_connections_gauge = Metrics::get_metric(metrics, "http_connection_pool_active")
  match active_connections_gauge {
    Some(gauge) => {
      assert_true(gauge.value > 0)
    }
    None => assert_true(false)
  }
  
  // 测试性能分析
  let performance_analyzer = HttpClientPerformanceAnalyzer::new()
  Analyzer::add_metrics(performance_analyzer, metrics)
  
  let performance_report = Analyzer::generate_report(performance_analyzer)
  
  assert_true(performance_report.total_requests > 0)
  assert_true(performance_report.average_response_time > 0)
  assert_true(performance_report.error_rate >= 0.0)
  assert_true(performance_report.throughput > 0)
  
  // 验证慢请求检测
  assert_true(performance_report.slow_requests.length() >= 0)
  for slow_request in performance_report.slow_requests {
    assert_true(slow_request.duration > 500.0) // 大于500ms的请求
  }
  
  // 验证错误分析
  assert_true(performance_report.error_breakdown.length() > 0)
  for (status_code, count) in performance_report.error_breakdown {
    assert_true(count > 0)
    assert_true(status_code >= 400) // HTTP错误状态码
  }
}

// Test 2: 网络连接监控和诊断
test "network connection monitoring and diagnostics" {
  // 创建网络监控器
  let network_monitor = NetworkMonitor::new()
  
  // 配置监控指标
  Monitor::add_metric(network_monitor, "network_connections_total", MetricType::Counter)
  Monitor::add_metric(network_monitor, "network_connection_duration_ms", MetricType::Histogram)
  Monitor::add_metric(network_monitor, "network_bytes_sent", MetricType::Counter)
  Monitor::add_metric(network_monitor, "network_bytes_received", MetricType::Counter)
  Monitor::add_metric(network_monitor, "network_connection_errors", MetricType::Counter)
  Monitor::add_metric(network_monitor, "network_dns_lookup_duration_ms", MetricType::Histogram)
  Monitor::add_metric(network_monitor, "network_tcp_connect_duration_ms", MetricType::Histogram)
  Monitor::add_metric(network_monitor, "network_tls_handshake_duration_ms", MetricType::Histogram)
  
  // 模拟网络连接
  let hosts = [
    "api.example.com",
    "database.example.com",
    "cache.example.com",
    "auth.example.com",
    "metrics.example.com"
  ]
  
  for host in hosts {
    for i in 0..<10 {
      // 模拟DNS查找
      let dns_start = Time::now()
      let dns_lookup_time = 10 + Math::random() * 50 // 10-60ms
      Monitor::record_histogram(network_monitor, "network_dns_lookup_duration_ms", dns_lookup_time, [
        ("host", host)
      ])
      
      // 模拟TCP连接
      let tcp_start = Time::now()
      let tcp_connect_time = 20 + Math::random() * 100 // 20-120ms
      Monitor::record_histogram(network_monitor, "network_tcp_connect_duration_ms", tcp_connect_time, [
        ("host", host)
      ])
      
      // 模拟TLS握手
      let tls_start = Time::now()
      let tls_handshake_time = 50 + Math::random() * 150 // 50-200ms
      Monitor::record_histogram(network_monitor, "network_tls_handshake_duration_ms", tls_handshake_time, [
        ("host", host)
      ])
      
      // 模拟连接建立
      let connection_duration = 1000 + Math::random() * 5000 // 1-6秒
      Monitor::record_histogram(network_monitor, "network_connection_duration_ms", connection_duration, [
        ("host", host)
      ])
      
      // 模拟数据传输
      let bytes_sent = 1000 + Math::random() * 9000
      let bytes_received = 2000 + Math::random() * 18000
      
      Monitor::record_counter(network_monitor, "network_bytes_sent", bytes_sent, [
        ("host", host)
      ])
      
      Monitor::record_counter(network_monitor, "network_bytes_received", bytes_received, [
        ("host", host)
      ])
      
      // 模拟连接错误
      if Math::random() < 0.1 { // 10%错误率
        Monitor::record_counter(network_monitor, "network_connection_errors", 1, [
          ("host", host),
          ("error_type", ["timeout", "connection_refused", "dns_error"][i % 3])
        ])
      } else {
        Monitor::record_counter(network_monitor, "network_connections_total", 1, [
          ("host", host)
        ])
      }
      
      Thread::sleep(10) // 短暂延迟
    }
  }
  
  // 验证网络监控指标
  let metrics = Monitor::get_metrics(network_monitor)
  
  let connections_counter = Metrics::get_metric(metrics, "network_connections_total")
  match connections_counter {
    Some(counter) => {
      assert_true(counter.value > 0)
    }
    None => assert_true(false)
  }
  
  let bytes_sent_counter = Metrics::get_metric(metrics, "network_bytes_sent")
  match bytes_sent_counter {
    Some(counter) => {
      assert_true(counter.value > 0)
    }
    None => assert_true(false)
  }
  
  let bytes_received_counter = Metrics::get_metric(metrics, "network_bytes_received")
  match bytes_received_counter {
    Some(counter) => {
      assert_true(counter.value > 0)
    }
    None => assert_true(false)
  }
  
  // 测试网络诊断
  let network_diagnostics = NetworkDiagnostics::new()
  Diagnostics::add_metrics(network_diagnostics, metrics)
  
  let diagnostic_report = Diagnostics::analyze(network_diagnostics)
  
  assert_true(diagnostic_report.total_connections > 0)
  assert_true(diagnostic_report.total_bytes_sent > 0)
  assert_true(diagnostic_report.total_bytes_received > 0)
  assert_true(diagnostic_report.connection_error_rate >= 0.0)
  
  // 验证主机性能分析
  assert_true(diagnostic_report.host_performance.length() > 0)
  for (host, performance) in diagnostic_report.host_performance {
    assert_true(host.length() > 0)
    assert_true(performance.average_dns_lookup_time > 0)
    assert_true(performance.average_tcp_connect_time > 0)
    assert_true(performance.average_tls_handshake_time > 0)
    assert_true(performance.total_bytes_sent > 0)
    assert_true(performance.total_bytes_received > 0)
    assert_true(performance.connection_success_rate >= 0.0 && performance.connection_success_rate <= 1.0)
  }
  
  // 测试网络健康检查
  let health_checker = NetworkHealthChecker::new()
  HealthChecker::add_host(health_checker, "api.example.com")
  HealthChecker::add_host(health_checker, "database.example.com")
  HealthChecker::add_host(health_checker, "cache.example.com")
  
  let health_results = HealthChecker::check_all(health_checker)
  assert_eq(health_results.length(), 3)
  
  for health in health_results {
    assert_true(health.host.length() > 0)
    assert_true(health.is_healthy == true || health.is_healthy == false)
    assert_true(health.response_time >= 0)
    assert_true(health.status_code >= 0)
  }
}

// Test 3: HTTP请求重试和断路器模式
test "HTTP request retry and circuit breaker patterns" {
  // 创建重试管理器
  let retry_manager = HttpRequestRetryManager::new()
  
  // 配置重试策略
  let retry_policy = RetryPolicy::new()
  RetryPolicy::set_max_attempts(retry_policy, 3)
  RetryPolicy::set_initial_delay_ms(retry_policy, 100)
  RetryPolicy::set_max_delay_ms(retry_policy, 1000)
  RetryPolicy::set_backoff_multiplier(retry_policy, 2.0)
  RetryPolicy::set_retryable_status_codes(retry_policy, [500, 502, 503, 504])
  RetryPolicy::set_retryable_exceptions(retry_policy, ["timeout", "connection_error"])
  
  RetryManager::set_policy(retry_manager, retry_policy)
  
  // 创建断路器管理器
  let circuit_breaker_manager = CircuitBreakerManager::new()
  
  // 配置断路器
  let circuit_breaker = CircuitBreaker::new("api.example.com")
  CircuitBreaker::set_failure_threshold(circuit_breaker, 5) // 5次失败后打开
  CircuitBreaker::set_success_threshold(circuit_breaker, 3) // 3次成功后关闭
  CircuitBreaker::set_timeout_ms(circuit_breaker, 30000) // 30秒后尝试半开
  CircuitBreaker::set_monitoring_period_ms(circuit_breaker, 60000) // 1分钟监控周期
  
  CircuitBreakerManager::add_circuit_breaker(circuit_breaker_manager, circuit_breaker)
  
  // 创建带有重试和断路器的HTTP客户端
  let resilient_client = ResilientHttpClient::new("https://api.example.com")
  HttpClient::set_retry_manager(resilient_client, retry_manager)
  HttpClient::set_circuit_breaker_manager(resilient_client, circuit_breaker_manager)
  
  // 模拟请求场景
  let test_scenarios = [
    // (成功请求次数, 失败请求次数, 预期最终结果)
    (10, 0, true),   // 全部成功
    (5, 5, true),    // 部分失败，但最终成功
    (0, 10, false),  // 全部失败，触发断路器
    (10, 2, true),   // 大部分成功
    (3, 8, false)    // 大部分失败
  ]
  
  for (success_count, failure_count, expected_final_result) in test_scenarios {
    // 重置断路器状态
    CircuitBreaker::reset(circuit_breaker)
    
    let successful_requests = AtomicInt::new(0)
    let failed_requests = AtomicInt::new(0)
    let retry_attempts = AtomicInt::new(0)
    
    // 模拟请求序列
    for i in 0..<(success_count + failure_count) {
      let should_succeed = i < success_count
      
      // 执行请求
      let request = HttpRequest::new("GET", "/test")
      let response = if should_succeed {
        HttpResponse::new(200)
      } else {
        HttpResponse::new(500)
      }
      
      let request_result = HttpClient::execute_with_resilience(resilient_client, request)
      
      match request_result {
        Ok(res) => {
          if res.status_code == 200 {
            AtomicInt::increment(successful_requests)
          } else {
            AtomicInt::increment(failed_requests)
            
            // 检查是否进行了重试
            if res.retry_count > 0 {
              AtomicInt::add(retry_attempts, res.retry_count)
            }
          }
        }
        Err(_) => {
          AtomicInt::increment(failed_requests)
        }
      }
      
      Thread::sleep(10) // 短暂延迟
    }
    
    // 验证结果
    let final_success = AtomicInt::get(successful_requests)
    let final_failures = AtomicInt::get(failed_requests)
    let total_retries = AtomicInt::get(retry_attempts)
    
    assert_true(final_success > 0 || expected_final_result == false)
    assert_true(final_failures >= 0)
    assert_true(total_retries >= 0)
    
    // 验证断路器状态
    let breaker_state = CircuitBreaker::get_state(circuit_breaker)
    if expected_final_result == false {
      assert_true(breaker_state == CircuitBreakerState::Open || breaker_state == CircuitBreakerState::HalfOpen)
    } else {
      assert_true(breaker_state == CircuitBreakerState::Closed)
    }
  }
  
  // 测试断路器恢复
  let breaker_state = CircuitBreaker::get_state(circuit_breaker)
  if breaker_state == CircuitBreakerState::Open {
    // 等待超时时间
    Thread::sleep(circuit_breaker.timeout_ms)
    
    // 执行成功请求以关闭断路器
    for i in 0..<5 {
      let request = HttpRequest::new("GET", "/test")
      let response = HttpResponse::new(200)
      
      let request_result = HttpClient::execute_with_resilience(resilient_client, request)
      match request_result {
        Ok(res) => {
          assert_eq(res.status_code, 200)
        }
        Err(_) => assert_true(false)
      }
      
      Thread::sleep(10)
    }
    
    // 验证断路器已关闭
    let final_state = CircuitBreaker::get_state(circuit_breaker)
    assert_eq(final_state, CircuitBreakerState::Closed)
  }
  
  // 验证重试统计
  let retry_stats = RetryManager::get_statistics(retry_manager)
  assert_true(retry_stats.total_retries >= 0)
  assert_true(retry_stats.successful_retries >= 0)
  assert_true(retry_stats.failed_retries >= 0)
  assert_true(retry_stats.average_retry_delay_ms > 0)
}

// Test 4: HTTP客户端负载均衡和服务发现
test "HTTP client load balancing and service discovery" {
  // 创建服务发现管理器
  let service_discovery = ServiceDiscoveryManager::new()
  
  // 注册服务实例
  let api_instances = [
    ServiceInstance::new("api-service", "api-1.example.com", 8080),
    ServiceInstance::new("api-service", "api-2.example.com", 8080),
    ServiceInstance::new("api-service", "api-3.example.com", 8080)
  ]
  
  let db_instances = [
    ServiceInstance::new("db-service", "db-1.example.com", 5432),
    ServiceInstance::new("db-service", "db-2.example.com", 5432)
  ]
  
  for instance in api_instances {
    ServiceDiscovery::register_instance(service_discovery, instance)
  }
  
  for instance in db_instances {
    ServiceDiscovery::register_instance(service_discovery, instance)
  }
  
  // 创建负载均衡器
  let load_balancer = LoadBalancer::new()
  
  // 配置负载均衡策略
  LoadBalancer::set_strategy(load_balancer, "api-service", LoadBalancingStrategy::RoundRobin)
  LoadBalancer::set_strategy(load_balancer, "db-service", LoadBalancingStrategy::WeightedRoundRobin)
  
  // 设置权重
  LoadBalancer::set_weight(load_balancer, "api-1.example.com", 1)
  LoadBalancer::set_weight(load_balancer, "api-2.example.com", 2)
  LoadBalancer::set_weight(load_balancer, "api-3.example.com", 1)
  
  LoadBalancer::set_weight(load_balancer, "db-1.example.com", 3)
  LoadBalancer::set_weight(load_balancer, "db-2.example.com", 1)
  
  // 创建带有负载均衡的HTTP客户端
  let balanced_client = LoadBalancedHttpClient::new()
  HttpClient::set_service_discovery(balanced_client, service_discovery)
  HttpClient::set_load_balancer(balanced_client, load_balancer)
  
  // 测试API服务负载均衡
  let api_request_counts = [
    ("api-1.example.com", 0),
    ("api-2.example.com", 0),
    ("api-3.example.com", 0)
  ]
  
  for i in 0..<30 { // 30个请求
    let request = HttpRequest::new("GET", "/api/data")
    let selected_instance = LoadBalancer::select_instance(load_balancer, "api-service")
    
    match selected_instance {
      Some(instance) => {
        assert_eq(instance.service_name, "api-service")
        
        // 更新计数
        for i in 0..<api_request_counts.length() {
          let (host, count) = api_request_counts[i]
          if host == instance.host {
            api_request_counts[i] = (host, count + 1)
            break
          }
        }
      }
      None => assert_true(false)
    }
  }
  
  // 验证负载均衡分布
  let total_api_requests = ArrayUtils::sum(api_request_counts.map(fn((_, c)) { c }))
  assert_eq(total_api_requests, 30)
  
  // RoundRobin应该均匀分布
  for (_, count) in api_request_counts {
    assert_true(count >= 8 && count <= 12) // 允许一些偏差
  }
  
  // 测试数据库服务加权负载均衡
  let db_request_counts = [
    ("db-1.example.com", 0),
    ("db-2.example.com", 0)
  ]
  
  for i in 0..<40 { // 40个请求
    let request = HttpRequest::new("GET", "/db/query")
    let selected_instance = LoadBalancer::select_instance(load_balancer, "db-service")
    
    match selected_instance {
      Some(instance) => {
        assert_eq(instance.service_name, "db-service")
        
        // 更新计数
        for i in 0..<db_request_counts.length() {
          let (host, count) = db_request_counts[i]
          if host == instance.host {
            db_request_counts[i] = (host, count + 1)
            break
          }
        }
      }
      None => assert_true(false)
    }
  }
  
  // 验证加权负载均衡分布
  let total_db_requests = ArrayUtils::sum(db_request_counts.map(fn((_, c)) { c }))
  assert_eq(total_db_requests, 40)
  
  // 权重为3:1，所以请求分布应该大约是3:1
  assert_true(db_request_counts[0].1 > db_request_counts[1].1 * 2) // 至少2倍差异
  
  // 测试健康检查和实例剔除
  let health_checker = ServiceHealthChecker::new()
  HealthChecker::add_service(health_checker, "api-service")
  HealthChecker::add_service(health_checker, "db-service")
  
  // 模拟实例健康检查
  let health_results = [
    ("api-1.example.com", true),
    ("api-2.example.com", false), // 标记为不健康
    ("api-3.example.com", true),
    ("db-1.example.com", true),
    ("db-2.example.com", true)
  ]
  
  for (host, is_healthy) in health_results {
    HealthChecker::update_instance_health(health_checker, host, is_healthy)
  }
  
  // 更新负载均衡器以考虑健康状态
  LoadBalancer::update_health_status(load_balancer, health_checker)
  
  // 测试剔除不健康实例后的负载均衡
  let healthy_api_request_counts = [
    ("api-1.example.com", 0),
    ("api-3.example.com", 0)
  ]
  
  for i in 0..<20 { // 20个请求
    let request = HttpRequest::new("GET", "/api/data")
    let selected_instance = LoadBalancer::select_instance(load_balancer, "api-service")
    
    match selected_instance {
      Some(instance) => {
        assert_eq(instance.service_name, "api-service")
        assert_ne(instance.host, "api-2.example.com") // 不应该选择不健康的实例
        
        // 更新计数
        for i in 0..<healthy_api_request_counts.length() {
          let (host, count) = healthy_api_request_counts[i]
          if host == instance.host {
            healthy_api_request_counts[i] = (host, count + 1)
            break
          }
        }
      }
      None => assert_true(false)
    }
  }
  
  // 验证只有健康实例被选择
  let total_healthy_requests = ArrayUtils::sum(healthy_api_request_counts.map(fn((_, c)) { c }))
  assert_eq(total_healthy_requests, 20)
  
  // 测试服务发现更新
  let new_instance = ServiceInstance::new("api-service", "api-4.example.com", 8080)
  ServiceDiscovery::register_instance(service_discovery, new_instance)
  
  // 通知负载均衡器更新
  LoadBalancer::refresh_instances(load_balancer, "api-service", service_discovery)
  
  // 验证新实例被包含在负载均衡中
  let all_instances = LoadBalancer::get_instances(load_balancer, "api-service")
  assert_true(all_instances.length() == 4) // 包括新实例
  
  let instance_hosts = all_instances.map(fn(i) { i.host })
  assert_true(instance_hosts.contains("api-4.example.com"))
}

// Test 5: HTTP客户端缓存和性能优化
test "HTTP client caching and performance optimization" {
  // 创建缓存管理器
  let cache_manager = HttpClientCacheManager::new()
  
  // 配置缓存策略
  let cache_config = CacheConfiguration::new()
  CacheConfig::set_max_size_mb(cache_config, 100)
  CacheConfig::set_default_ttl_seconds(cache_config, 300) // 5分钟
  CacheConfig::set_max_entries(cache_config, 10000)
  CacheConfig::set_eviction_policy(cache_config, EvictionPolicy::LRU)
  
  CacheManager::set_configuration(cache_manager, cache_config)
  
  // 创建带有缓存的HTTP客户端
  let cached_client = CachedHttpClient::new("https://api.example.com")
  HttpClient::set_cache_manager(cached_client, cache_manager)
  
  // 配置特定端点的缓存策略
  CacheManager::set_endpoint_ttl(cache_manager, "/users", 600) // 10分钟
  CacheManager::set_endpoint_ttl(cache_manager, "/products", 1800) // 30分钟
  CacheManager::set_endpoint_ttl(cache_manager, "/orders", 60) // 1分钟
  CacheManager::disable_caching(cache_manager, "/auth") // 不缓存认证端点
  
  // 测试缓存行为
  let test_requests = [
    ("/users", "GET", true),   // 应该缓存
    ("/products", "GET", true), // 应该缓存
    ("/orders", "GET", true),   // 应该缓存
    ("/auth", "POST", false),  // 不应该缓存
    ("/users/123", "GET", true), // 应该缓存
    ("/users", "POST", false)   // POST请求不应该缓存
  ]
  
  let cache_hits = AtomicInt::new(0)
  let cache_misses = AtomicInt::new(0)
  let network_requests = AtomicInt::new(0)
  
  // 第一轮请求 - 应该都是缓存未命中
  for (endpoint, method, should_cache) in test_requests {
    let request = HttpRequest::new(method, endpoint)
    HttpClient::add_header(request, "Cache-Control", "max-age=300")
    
    let response = if should_cache {
      HttpResponse::new(200)
    } else {
      HttpResponse::new(201)
    }
    
    HttpResponse::add_header(response, "Cache-Control", "max-age=300")
    HttpResponse::set_body(response, "Response data for " + endpoint)
    
    let result = HttpClient::execute_with_cache(cached_client, request)
    
    match result {
      Ok(res) => {
        if res.from_cache {
          AtomicInt::increment(cache_hits)
        } else {
          AtomicInt::increment(cache_misses)
          AtomicInt::increment(network_requests)
        }
        
        assert_eq(res.status_code, if should_cache { 200 } else { 201 })
      }
      Err(_) => assert_true(false)
    }
  }
  
  // 第二轮请求 - 缓存的请求应该是缓存命中
  for (endpoint, method, should_cache) in test_requests {
    let request = HttpRequest::new(method, endpoint)
    HttpClient::add_header(request, "Cache-Control", "max-age=300")
    
    let result = HttpClient::execute_with_cache(cached_client, request)
    
    match result {
      Ok(res) => {
        if res.from_cache {
          AtomicInt::increment(cache_hits)
        } else {
          AtomicInt::increment(cache_misses)
          AtomicInt::increment(network_requests)
        }
        
        assert_eq(res.status_code, if should_cache { 200 } else { 201 })
      }
      Err(_) => assert_true(false)
    }
  }
  
  // 验证缓存效果
  let final_cache_hits = AtomicInt::get(cache_hits)
  let final_cache_misses = AtomicInt::get(cache_misses)
  let final_network_requests = AtomicInt::get(network_requests)
  
  assert_true(final_cache_hits > 0)
  assert_true(final_cache_misses > 0)
  assert_true(final_network_requests > 0)
  
  // 缓存命中率应该大于50%
  let cache_hit_rate = final_cache_hits.to_float() / (final_cache_hits + final_cache_misses).to_float()
  assert_true(cache_hit_rate > 0.5)
  
  // 测试缓存统计
  let cache_stats = CacheManager::get_statistics(cache_manager)
  assert_true(cache_stats.total_requests > 0)
  assert_true(cache_stats.cache_hits > 0)
  assert_true(cache_stats.cache_misses > 0)
  assert_true(cache_stats.cache_size_bytes > 0)
  assert_true(cache_stats.cache_entries > 0)
  assert_true(cache_stats.eviction_count >= 0)
  
  // 测试缓存失效
  let invalidate_endpoint = "/users"
  CacheManager::invalidate_endpoint(cache_manager, invalidate_endpoint)
  
  // 再次请求已失效的端点
  let invalidate_request = HttpRequest::new("GET", invalidate_endpoint)
  let invalidate_result = HttpClient::execute_with_cache(cached_client, invalidate_request)
  
  match invalidate_result {
    Ok(res) => {
      assert_false(res.from_cache) // 应该不是来自缓存
    }
    Err(_) => assert_true(false)
  }
  
  // 测试性能优化
  let performance_optimizer = HttpClientPerformanceOptimizer::new()
  Optimizer::add_technique(performance_optimizer, OptimizationTechnique::ConnectionPooling)
  Optimizer::add_technique(performance_optimizer, OptimizationTechnique::RequestCompression)
  Optimizer::add_technique(performance_optimizer, OptimizationTechnique::ResponseCompression)
  Optimizer::add_technique(performance_optimizer, OptimizationTechnique::Http2)
  
  let optimized_client = OptimizedHttpClient::new("https://api.example.com")
  HttpClient::set_optimizer(optimized_client, performance_optimizer)
  
  // 测试优化效果
  let optimization_start = Time::now()
  
  for i in 0..<50 {
    let request = HttpRequest::new("GET", "/optimized/data")
    HttpClient::add_header(request, "Accept-Encoding", "gzip, deflate")
    
    let response = HttpResponse::new(200)
    HttpResponse::add_header(response, "Content-Encoding", "gzip")
    HttpResponse::set_body(response, "Compressed response data " + i.to_string())
    
    let result = HttpClient::execute_optimized(optimized_client, request)
    match result {
      Ok(res) => {
        assert_eq(res.status_code, 200)
        assert_true(res.compressed || res.from_connection_pool)
      }
      Err(_) => assert_true(false)
    }
  }
  
  let optimization_time = Time::now() - optimization_start
  
  // 验证优化统计
  let optimization_stats = Optimizer::get_statistics(performance_optimizer)
  assert_true(optimization_stats.total_requests > 0)
  assert_true(optimization_stats.connection_pool_hits > 0)
  assert_true(optimization_stats.compressed_requests > 0)
  assert_true(optimization_stats.compressed_responses > 0)
  assert_true(optimization_stats.average_response_time_ms > 0)
}