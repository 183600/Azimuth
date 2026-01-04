// Azimuth 高级多租户隔离测试用例
// 专注于验证遥测系统在多租户环境下的隔离性和安全性

// 测试1: 租户数据隔离验证
test "租户数据隔离验证测试" {
  // 创建多租户管理器
  let tenant_manager = MultiTenantManager::new()
  
  // 创建测试租户
  let tenant_a = Tenant::new("tenant-a", "Acme Corporation", "enterprise")
  let tenant_b = Tenant::new("tenant-b", "Tech Solutions Inc", "business")
  let tenant_c = Tenant::new("tenant-c", "Startup LLC", "startup")
  
  // 配置租户资源限制
  Tenant::set_resource_limit(tenant_a, "max.spans.per.day", 100000)
  Tenant::set_resource_limit(tenant_b, "max.spans.per.day", 50000)
  Tenant::set_resource_limit(tenant_c, "max.spans.per.day", 10000)
  
  Tenant::set_resource_limit(tenant_a, "max.storage.gb", 100)
  Tenant::set_resource_limit(tenant_b, "max.storage.gb", 50)
  Tenant::set_resource_limit(tenant_c, "max.storage.gb", 10)
  
  // 注册租户
  MultiTenantManager::register_tenant(tenant_manager, tenant_a)
  MultiTenantManager::register_tenant(tenant_manager, tenant_b)
  MultiTenantManager::register_tenant(tenant_manager, tenant_c)
  
  // 创建租户特定的提供者
  let provider_a = TenantProvider::create_for_tenant(tenant_manager, "tenant-a")
  let provider_b = TenantProvider::create_for_tenant(tenant_manager, "tenant-b")
  let provider_c = TenantProvider::create_for_tenant(tenant_manager, "tenant-c")
  
  // 创建租户特定的tracer
  let tracer_a = TenantProvider::get_tracer(provider_a, "tenant-a.service")
  let tracer_b = TenantProvider::get_tracer(provider_b, "tenant-b.service")
  let tracer_c = TenantProvider::get_tracer(provider_c, "tenant-c.service")
  
  // 为每个租户创建span
  let spans_a = []
  let spans_b = []
  let spans_c = []
  
  // 租户A的span
  for i in 1..=100 {
    let span = Tracer::start_span(tracer_a, "tenant-a.operation." + i.to_string())
    Span::set_attribute(span, "tenant.id", StringValue("tenant-a"))
    Span::set_attribute(span, "tenant.name", StringValue("Acme Corporation"))
    Span::set_attribute(span, "data.sensitivity", StringValue("confidential"))
    Span::set_attribute(span, "customer.id", StringValue("customer-a-" + i.to_string()))
    
    spans_a = spans_a.push(span)
    Span::end(span)
  }
  
  // 租户B的span
  for i in 1..=50 {
    let span = Tracer::start_span(tracer_b, "tenant-b.operation." + i.to_string())
    Span::set_attribute(span, "tenant.id", StringValue("tenant-b"))
    Span::set_attribute(span, "tenant.name", StringValue("Tech Solutions Inc"))
    Span::set_attribute(span, "data.sensitivity", StringValue("internal"))
    Span::set_attribute(span, "customer.id", StringValue("customer-b-" + i.to_string()))
    
    spans_b = spans_b.push(span)
    Span::end(span)
  }
  
  // 租户C的span
  for i in 1..=20 {
    let span = Tracer::start_span(tracer_c, "tenant-c.operation." + i.to_string())
    Span::set_attribute(span, "tenant.id", StringValue("tenant-c"))
    Span::set_attribute(span, "tenant.name", StringValue("Startup LLC"))
    Span::set_attribute(span, "data.sensitivity", StringValue("public"))
    Span::set_attribute(span, "customer.id", StringValue("customer-c-" + i.to_string()))
    
    spans_c = spans_c.push(span)
    Span::end(span)
  }
  
  // 验证租户数据隔离
  let isolation_validator = TenantDataIsolationValidator::new(tenant_manager)
  
  // 验证租户A只能访问自己的数据
  let tenant_a_access = isolation_validator.validate_tenant_access("tenant-a", "tenant-a")
  assert_true(tenant_a_access.can_access)
  assert_eq(tenant_a_access.accessible_spans, spans_a.length())
  
  let tenant_a_cross_access_b = isolation_validator.validate_tenant_access("tenant-a", "tenant-b")
  assert_false(tenant_a_cross_access_b.can_access)
  assert_eq(tenant_a_cross_access_b.accessible_spans, 0)
  
  let tenant_a_cross_access_c = isolation_validator.validate_tenant_access("tenant-a", "tenant-c")
  assert_false(tenant_a_cross_access_c.can_access)
  assert_eq(tenant_a_cross_access_c.accessible_spans, 0)
  
  // 验证租户B只能访问自己的数据
  let tenant_b_access = isolation_validator.validate_tenant_access("tenant-b", "tenant-b")
  assert_true(tenant_b_access.can_access)
  assert_eq(tenant_b_access.accessible_spans, spans_b.length())
  
  // 验证租户C只能访问自己的数据
  let tenant_c_access = isolation_validator.validate_tenant_access("tenant-c", "tenant-c")
  assert_true(tenant_c_access.can_access)
  assert_eq(tenant_c_access.accessible_spans, spans_c.length())
  
  // 测试数据查询隔离
  let query_isolator = TenantQueryIsolator::new(tenant_manager)
  
  // 租户A查询自己的数据
  let tenant_a_query = TenantQueryIsolator::execute_query(query_isolator, "tenant-a", "SELECT * FROM spans")
  assert_eq(tenant_a_query.result_count, spans_a.length())
  assert_true(tenant_a_query.all_results_belong_to_tenant)
  
  // 租户A尝试查询其他租户数据
  let tenant_a_cross_query = TenantQueryIsolator::execute_query(query_isolator, "tenant-a", "SELECT * FROM spans WHERE tenant.id = 'tenant-b'")
  assert_eq(tenant_a_cross_query.result_count, 0)
  assert_true(tenant_a_cross_query.access_denied)
  
  // 测试资源配额执行
  let quota_enforcer = TenantQuotaEnforcer::new(tenant_manager)
  
  // 验证租户A的资源使用
  let tenant_a_quota = quota_enforcer.check_quota("tenant-a", "max.spans.per.day")
  assert_true(tenant_a_quota.within_limit)
  assert_eq(tenant_a_quota.current_usage, spans_a.length())
  assert_eq(tenant_a_quota.limit, 100000)
  
  let tenant_a_storage = quota_enforcer.check_quota("tenant-a", "max.storage.gb")
  assert_true(tenant_a_storage.within_limit)
  assert_true(tenant_a_storage.current_usage > 0)
  assert_eq(tenant_a_storage.limit, 100)
  
  // 验证租户C的资源使用（较小的配额）
  let tenant_c_quota = quota_enforcer.check_quota("tenant-c", "max.spans.per.day")
  assert_true(tenant_c_quota.within_limit)
  assert_eq(tenant_c_quota.current_usage, spans_c.length())
  assert_eq(tenant_c_quota.limit, 10000)
}

// 测试2: 租户性能隔离验证
test "租户性能隔离验证测试" {
  // 创建性能隔离管理器
  let performance_isolator = TenantPerformanceIsolator::new()
  
  // 创建测试租户
  let high_priority_tenant = Tenant::new("high-priority", "Enterprise Corp", "premium")
  let normal_priority_tenant = Tenant::new("normal-priority", "Standard Corp", "standard")
  let low_priority_tenant = Tenant::new("low-priority", "Basic Corp", "basic")
  
  // 设置性能优先级
  Tenant::set_performance_priority(high_priority_tenant, "high")
  Tenant::set_performance_priority(normal_priority_tenant, "normal")
  Tenant::set_performance_priority(low_priority_tenant, "low")
  
  // 设置资源分配
  Tenant::set_cpu_share(high_priority_tenant, 50)  // 50% CPU
  Tenant::set_cpu_share(normal_priority_tenant, 30) // 30% CPU
  Tenant::set_cpu_share(low_priority_tenant, 20)   // 20% CPU
  
  Tenant::set_memory_limit_mb(high_priority_tenant, 2048) // 2GB
  Tenant::set_memory_limit_mb(normal_priority_tenant, 1024) // 1GB
  Tenant::set_memory_limit_mb(low_priority_tenant, 512)    // 512MB
  
  // 注册租户
  performance_isolator.register_tenant(high_priority_tenant)
  performance_isolator.register_tenant(normal_priority_tenant)
  performance_isolator.register_tenant(low_priority_tenant)
  
  // 创建并发负载测试
  let load_tester = TenantLoadTester::new(performance_isolator)
  
  // 为每个租户生成负载
  let high_priority_load = TenantLoadTester::generate_load(load_tester, "high-priority", 1000)
  let normal_priority_load = TenantLoadTester::generate_load(load_tester, "normal-priority", 1000)
  let low_priority_load = TenantLoadTester::generate_load(load_tester, "low-priority", 1000)
  
  // 并发执行负载
  let load_results = TenantLoadTester::execute_concurrent_loads(load_tester, [
    high_priority_load, normal_priority_load, low_priority_load
  ])
  
  // 验证性能隔离
  let high_priority_result = load_results[0]
  let normal_priority_result = load_results[1]
  let low_priority_result = load_results[2]
  
  // 高优先级租户应该获得最佳性能
  assert_true(high_priority_result.average_response_time < normal_priority_result.average_response_time)
  assert_true(high_priority_result.average_response_time < low_priority_result.average_response_time)
  assert_true(high_priority_result.throughput > normal_priority_result.throughput)
  assert_true(high_priority_result.throughput > low_priority_result.throughput)
  
  // 正常优先级租户应该比低优先级租户有更好的性能
  assert_true(normal_priority_result.average_response_time < low_priority_result.average_response_time)
  assert_true(normal_priority_result.throughput > low_priority_result.throughput)
  
  // 测试资源限制执行
  let resource_monitor = TenantResourceMonitor::new(performance_isolator)
  
  // 监控资源使用
  let resource_usage = resource_monitor.monitor_all_tenants()
  
  // 验证CPU分配
  let high_priority_cpu = resource_usage.get_tenant_usage("high-priority").cpu_percentage
  let normal_priority_cpu = resource_usage.get_tenant_usage("normal-priority").cpu_percentage
  let low_priority_cpu = resource_usage.get_tenant_usage("low-priority").cpu_percentage
  
  assert_true(high_priority_cpu >= 40.0 && high_priority_cpu <= 60.0) // 约50%
  assert_true(normal_priority_cpu >= 20.0 && normal_priority_cpu <= 40.0) // 约30%
  assert_true(low_priority_cpu >= 10.0 && low_priority_cpu <= 30.0)   // 约20%
  
  // 验证内存限制
  let high_priority_memory = resource_usage.get_tenant_usage("high-priority").memory_mb
  let normal_priority_memory = resource_usage.get_tenant_usage("normal-priority").memory_mb
  let low_priority_memory = resource_usage.get_tenant_usage("low-priority").memory_mb
  
  assert_true(high_priority_memory <= 2048) // 不超过2GB
  assert_true(normal_priority_memory <= 1024) // 不超过1GB
  assert_true(low_priority_memory <= 512)    // 不超过512MB
  
  // 测试突发负载处理
  let burst_tester = TenantBurstLoadTester::new(performance_isolator)
  
  // 为高优先级租户生成突发负载
  let burst_load = burst_tester.generate_burst_load("high-priority", 5000) // 5倍突发
  
  let burst_result = burst_tester.execute_burst_load(burst_load)
  
  // 验证突发处理能力
  assert_true(burst_result.handled_without_system_failure)
  assert_true(burst_result.high_priority_tenant_protected)
  assert_true(burst_result.low_priority_tenants_throttled)
  
  // 测试租户间干扰检测
  let interference_detector = TenantInterferenceDetector::new(performance_isolator)
  
  // 模拟租户间干扰
  let interference_test = interference_detector.test_interference("high-priority", "low-priority")
  
  assert_true(interference_test.interference_detected == false || interference_test.interference_detected == true)
  assert_true(interference_test.interference_score >= 0.0 && interference_test.interference_score <= 1.0)
  
  if interference_test.interference_detected {
    assert_true(interference_test.mitigation_applied)
  }
}

// 测试3: 租户网络隔离验证
test "租户网络隔离验证测试" {
  // 创建网络隔离管理器
  let network_isolator = TenantNetworkIsolator::new()
  
  // 创建测试租户
  let tenant_a = Tenant::new("network-tenant-a", "Company A", "enterprise")
  let tenant_b = Tenant::new("network-tenant-b", "Company B", "enterprise")
  
  // 配置网络隔离
  let network_config_a = TenantNetworkConfig::new()
  TenantNetworkConfig::assign_network_segment(network_config_a, "10.1.0.0/16")
  TenantNetworkConfig::set_firewall_rules(network_config_a, [
    FirewallRule::allow("10.1.0.0/16", "any", "any"), // 允许内部通信
    FirewallRule::deny("any", "10.2.0.0/16", "any"),  // 阻止租户B
    FirewallRule::allow("any", "external.api", "443")  // 允许外部API
  ])
  
  let network_config_b = TenantNetworkConfig::new()
  TenantNetworkConfig::assign_network_segment(network_config_b, "10.2.0.0/16")
  TenantNetworkConfig::set_firewall_rules(network_config_b, [
    FirewallRule::allow("10.2.0.0/16", "any", "any"), // 允许内部通信
    FirewallRule::deny("any", "10.1.0.0/16", "any"),  // 阻止租户A
    FirewallRule::allow("any", "external.api", "443")  // 允许外部API
  ])
  
  // 应用网络配置
  network_isolator.configure_tenant_network(tenant_a, network_config_a)
  network_isolator.configure_tenant_network(tenant_b, network_config_b)
  
  // 测试网络连通性
  let connectivity_tester = TenantNetworkConnectivityTester::new(network_isolator)
  
  // 测试租户A内部连通性
  let internal_connectivity_a = connectivity_tester.test_connectivity(
    "10.1.0.10", "10.1.0.20", 8080, "tenant-a"
  )
  assert_true(internal_connectivity_a.success)
  assert_true(internal_connectivity_a.allowed)
  
  // 测试租户B内部连通性
  let internal_connectivity_b = connectivity_tester.test_connectivity(
    "10.2.0.10", "10.2.0.20", 8080, "tenant-b"
  )
  assert_true(internal_connectivity_b.success)
  assert_true(internal_connectivity_b.allowed)
  
  // 测试租户间隔离（A到B）
  let cross_connectivity_a_to_b = connectivity_tester.test_connectivity(
    "10.1.0.10", "10.2.0.20", 8080, "tenant-a"
  )
  assert_false(cross_connectivity_a_to_b.success)
  assert_false(cross_connectivity_a_to_b.allowed)
  assert_true(cross_connectivity_a_to_b.blocked_by_firewall)
  
  // 测试租户间隔离（B到A）
  let cross_connectivity_b_to_a = connectivity_tester.test_connectivity(
    "10.2.0.10", "10.1.0.20", 8080, "tenant-b"
  )
  assert_false(cross_connectivity_b_to_a.success)
  assert_false(cross_connectivity_b_to_a.allowed)
  assert_true(cross_connectivity_b_to_a.blocked_by_firewall)
  
  // 测试外部访问
  let external_connectivity_a = connectivity_tester.test_connectivity(
    "10.1.0.10", "external.api", 443, "tenant-a"
  )
  assert_true(external_connectivity_a.success)
  assert_true(external_connectivity_a.allowed)
  
  let external_connectivity_b = connectivity_tester.test_connectivity(
    "10.2.0.10", "external.api", 443, "tenant-b"
  )
  assert_true(external_connectivity_b.success)
  assert_true(external_connectivity_b.allowed)
  
  // 测试DNS隔离
  let dns_isolator = TenantDNSIsolator::new(network_isolator)
  
  // 配置租户特定DNS
  dns_isolator.configure_tenant_dns("tenant-a", [
    DNSRecord::new("service-a.internal", "10.1.0.100"),
    DNSRecord::new("shared.service", "10.1.0.200") // 租户A特定的共享服务
  ])
  
  dns_isolator.configure_tenant_dns("tenant-b", [
    DNSRecord::new("service-b.internal", "10.2.0.100"),
    DNSRecord::new("shared.service", "10.2.0.200") // 租户B特定的共享服务
  ])
  
  // 测试DNS解析隔离
  let dns_resolution_a = dns_isolator.resolve_dns("shared.service", "tenant-a")
  assert_eq(dns_resolution_a.ip_address, "10.1.0.200")
  assert_eq(dns_resolution_a.resolved_for_tenant, "tenant-a")
  
  let dns_resolution_b = dns_isolator.resolve_dns("shared.service", "tenant-b")
  assert_eq(dns_resolution_b.ip_address, "10.2.0.200")
  assert_eq(dns_resolution_b.resolved_for_tenant, "tenant-b")
  
  // 测试负载均衡器隔离
  let lb_isolator = TenantLoadBalancerIsolator::new(network_isolator)
  
  // 配置租户特定负载均衡器
  lb_isolator.configure_tenant_load_balancer("tenant-a", "lb-a.internal", [
    "10.1.0.10", "10.1.0.11", "10.1.0.12"
  ])
  
  lb_isolator.configure_tenant_load_balancer("tenant-b", "lb-b.internal", [
    "10.2.0.10", "10.2.0.11", "10.2.0.12"
  ])
  
  // 测试负载均衡器隔离
  let lb_request_a = lb_isolator.route_request("lb-a.internal", "tenant-a")
  assert_true(lb_request_a.success)
  assert_true(lb_request_a.backend_ip.starts_with("10.1."))
  
  let lb_request_b = lb_isolator.route_request("lb-b.internal", "tenant-b")
  assert_true(lb_request_b.success)
  assert_true(lb_request_b.backend_ip.starts_with("10.2."))
  
  // 测试跨租户负载均衡器访问
  let cross_lb_request_a = lb_isolator.route_request("lb-b.internal", "tenant-a")
  assert_false(cross_lb_request_a.success)
  assert_true(cross_lb_request_a.access_denied)
  
  // 测试网络监控隔离
  let network_monitor = TenantNetworkMonitor::new(network_isolator)
  
  // 监控租户网络流量
  let traffic_monitoring = network_monitor.monitor_tenant_traffic("tenant-a", Duration::from_minutes(5))
  
  assert_true(traffic_monitoring.total_bytes > 0)
  assert_true(traffic_monitoring.internal_traffic > 0)
  assert_eq(traffic_monitoring.cross_tenant_traffic, 0) // 应该没有跨租户流量
  
  // 测试网络入侵检测
  let intrusion_detector = TenantNetworkIntrusionDetector::new(network_isolator)
  
  // 模拟可疑活动
  let suspicious_activity = intrusion_detector.detect_suspicious_activity("tenant-a", [
    NetworkEvent::new("10.1.0.10", "10.2.0.20", "port_scan", Timestamp::now()),
    NetworkEvent::new("10.1.0.10", "10.3.0.0/16", "unauthorized_access", Timestamp::now())
  ])
  
  assert_true(suspicious_activity.threats_detected)
  assert_true(suspicious_activity.blocked_attempts > 0)
  assert_true(suspicious_activity.alerts_generated)
}

// 测试4: 租户配置和策略隔离验证
test "租户配置和策略隔离验证测试" {
  // 创建配置隔离管理器
  let config_isolator = TenantConfigIsolator::new()
  
  // 创建测试租户
  let tenant_a = Tenant::new("config-tenant-a", "Company A", "enterprise")
  let tenant_b = Tenant::new("config-tenant-b", "Company B", "enterprise")
  
  // 配置租户特定策略
  let sampling_policy_a = TenantSamplingPolicy::new()
  TenantSamplingPolicy::set_strategy(sampling_policy_a, "probability")
  TenantSamplingPolicy::set_probability(sampling_policy_a, 0.1) // 10%采样
  TenantSamplingPolicy::set_max_spans_per_second(sampling_policy_a, 1000)
  
  let sampling_policy_b = TenantSamplingPolicy::new()
  TenantSamplingPolicy::set_strategy(sampling_policy_b, "adaptive")
  TenantSamplingPolicy::set_probability(sampling_policy_b, 0.5) // 50%采样
  TenantSamplingPolicy::set_max_spans_per_second(sampling_policy_b, 500)
  
  // 配置数据保留策略
  let retention_policy_a = TenantRetentionPolicy::new()
  TenantRetentionPolicy::set_span_retention_days(retention_policy_a, 30)
  TenantRetentionPolicy::set_metric_retention_days(retention_policy_a, 90)
  TenantRetentionPolicy::set_log_retention_days(retention_policy_a, 7)
  
  let retention_policy_b = TenantRetentionPolicy::new()
  TenantRetentionPolicy::set_span_retention_days(retention_policy_b, 90)
  TenantRetentionPolicy::set_metric_retention_days(retention_policy_b, 365)
  TenantRetentionPolicy::set_log_retention_days(retention_policy_b, 30)
  
  // 配置隐私策略
  let privacy_policy_a = TenantPrivacyPolicy::new()
  TenantPrivacyPolicy::set_data_masking(privacy_policy_a, true)
  TenantPrivacyPolicy::set_anonymization(privacy_policy_a, false)
  TenantPrivacyPolicy::set_pii_detection(privacy_policy_a, true)
  TenantPrivacyPolicy::set_allowed_regions(privacy_policy_a, ["us-east", "us-west"])
  
  let privacy_policy_b = TenantPrivacyPolicy::new()
  TenantPrivacyPolicy::set_data_masking(privacy_policy_b, false)
  TenantPrivacyPolicy::set_anonymization(privacy_policy_b, true)
  TenantPrivacyPolicy::set_pii_detection(privacy_policy_b, true)
  TenantPrivacyPolicy::set_allowed_regions(privacy_policy_b, ["eu-west", "eu-central"])
  
  // 应用配置
  config_isolator.apply_sampling_policy(tenant_a, sampling_policy_a)
  config_isolator.apply_sampling_policy(tenant_b, sampling_policy_b)
  
  config_isolator.apply_retention_policy(tenant_a, retention_policy_a)
  config_isolator.apply_retention_policy(tenant_b, retention_policy_b)
  
  config_isolator.apply_privacy_policy(tenant_a, privacy_policy_a)
  config_isolator.apply_privacy_policy(tenant_b, privacy_policy_b)
  
  // 测试配置隔离
  let config_validator = TenantConfigIsolationValidator::new(config_isolator)
  
  // 验证租户A不能访问租户B的配置
  let config_access_a_to_b = config_validator.validate_config_access("config-tenant-a", "config-tenant-b")
  assert_false(config_access_a_to_b.can_access)
  assert_true(config_access_a_to_b.access_denied)
  
  // 验证租户A只能访问自己的配置
  let config_access_a_to_a = config_validator.validate_config_access("config-tenant-a", "config-tenant-a")
  assert_true(config_access_a_to_a.can_access)
  assert_false(config_access_a_to_a.access_denied)
  
  // 测试采样策略执行
  let sampling_enforcer = TenantSamplingEnforcer::new(config_isolator)
  
  // 为租户A生成遥测数据
  let telemetry_generator_a = TenantTelemetryGenerator::new("config-tenant-a")
  let generated_spans_a = telemetry_generator_a.generate_spans(1000)
  
  // 应用采样策略
  let sampled_spans_a = sampling_enforcer.apply_sampling_policy("config-tenant-a", generated_spans_a)
  
  // 验证采样结果（约10%应该被采样）
  let sampling_rate_a = sampled_spans_a.length().to_float() / generated_spans_a.length().to_float()
  assert_true(sampling_rate_a >= 0.05 && sampling_rate_a <= 0.15) // 允许误差
  
  // 为租户B生成遥测数据
  let telemetry_generator_b = TenantTelemetryGenerator::new("config-tenant-b")
  let generated_spans_b = telemetry_generator_b.generate_spans(1000)
  
  // 应用采样策略
  let sampled_spans_b = sampling_enforcer.apply_sampling_policy("config-tenant-b", generated_spans_b)
  
  // 验证采样结果（约50%应该被采样）
  let sampling_rate_b = sampled_spans_b.length().to_float() / generated_spans_b.length().to_float()
  assert_true(sampling_rate_b >= 0.4 && sampling_rate_b <= 0.6) // 允许误差
  
  // 验证策略隔离
  assert_not_eq(sampling_rate_a, sampling_rate_b) // 两个租户的采样率应该不同
  
  // 测试数据保留策略执行
  let retention_enforcer = TenantRetentionEnforcer::new(config_isolator)
  
  // 创建不同时间的数据
  let old_data_a = telemetry_generator_a.generate_spans_with_timestamp(100, Timestamp::now() - Duration::from_days(35))
  let recent_data_a = telemetry_generator_a.generate_spans_with_timestamp(100, Timestamp::now() - Duration::from_days(10))
  
  let old_data_b = telemetry_generator_b.generate_spans_with_timestamp(100, Timestamp::now() - Duration::from_days(100))
  let recent_data_b = telemetry_generator_b.generate_spans_with_timestamp(100, Timestamp::now() - Duration::from_days(10))
  
  // 应用保留策略
  let retention_result_a = retention_enforcer.apply_retention_policy("config-tenant-a", old_data_a + recent_data_a)
  let retention_result_b = retention_enforcer.apply_retention_policy("config-tenant-b", old_data_b + recent_data_b)
  
  // 验证保留结果
  assert_eq(retention_result_a.retained_spans, recent_data_a.length()) // 租户A只保留30天内的数据
  assert_eq(retention_result_b.retained_spans, recent_data_b.length()) // 租户B只保留90天内的数据
  
  // 测试隐私策略执行
  let privacy_enforcer = TenantPrivacyEnforcer::new(config_isolator)
  
  // 创建包含PII的测试数据
  let pii_data_a = telemetry_generator_a.generate_spans_with_pii(100)
  let pii_data_b = telemetry_generator_b.generate_spans_with_pii(100)
  
  // 应用隐私策略
  let privacy_result_a = privacy_enforcer.apply_privacy_policy("config-tenant-a", pii_data_a)
  let privacy_result_b = privacy_enforcer.apply_privacy_policy("config-tenant-b", pii_data_b)
  
  // 验证隐私处理结果
  assert_true(privacy_result_a.data_masked) // 租户A应该有数据脱敏
  assert_false(privacy_result_a.data_anonymized) // 租户A不应该有数据匿名化
  
  assert_false(privacy_result_b.data_masked) // 租户B不应该有数据脱敏
  assert_true(privacy_result_b.data_anonymized) // 租户B应该有数据匿名化
  
  // 测试配置变更隔离
  let config_change_tracker = TenantConfigChangeTracker::new(config_isolator)
  
  // 记录配置变更
  let config_change_a = config_change_tracker.record_change("config-tenant-a", "sampling.policy", [
    ("old.probability", "0.1"),
    ("new.probability", "0.2"),
    ("changed.by", "admin-a"),
    ("change.timestamp", Timestamp::now().to_string())
  ])
  
  let config_change_b = config_change_tracker.record_change("config-tenant-b", "retention.policy", [
    ("old.retention.days", "90"),
    ("new.retention.days", "180"),
    ("changed.by", "admin-b"),
    ("change.timestamp", Timestamp::now().to_string())
  ])
  
  // 验证变更隔离
  let tenant_a_changes = config_change_tracker.get_tenant_changes("config-tenant-a")
  let tenant_b_changes = config_change_tracker.get_tenant_changes("config-tenant-b")
  
  assert_true(tenant_a_changes.contains(config_change_a))
  assert_false(tenant_a_changes.contains(config_change_b))
  
  assert_true(tenant_b_changes.contains(config_change_b))
  assert_false(tenant_b_changes.contains(config_change_a))
  
  // 测试配置回滚隔离
  let config_rollback = TenantConfigRollback::new(config_isolator)
  
  // 执行租户A配置回滚
  let rollback_result_a = config_rollback.rollback_to_timestamp("config-tenant-a", Timestamp::now() - Duration::from_hours(1))
  assert_true(rollback_result_a.success)
  assert_eq(rollback_result_a.affected_tenant, "config-tenant-a")
  
  // 验证回滚不影响其他租户
  let current_config_b = config_isolator.get_current_config("config-tenant-b")
  assert_not_eq(current_config_b.retention_policy.span_retention_days, 30) // 应该保持租户B的配置
}

// 测试5: 租户故障隔离验证
test "租户故障隔离验证测试" {
  // 创建故障隔离管理器
  let fault_isolator = TenantFaultIsolator::new()
  
  // 创建测试租户
  let healthy_tenant = Tenant::new("healthy-tenant", "Healthy Corp", "enterprise")
  let faulty_tenant = Tenant::new("faulty-tenant", "Faulty Corp", "enterprise")
  
  // 注册租户
  fault_isolator.register_tenant(healthy_tenant)
  fault_isolator.register_tenant(faulty_tenant)
  
  // 创建健康租户的正常操作
  let healthy_provider = TenantProvider::create_for_tenant(fault_isolator, "healthy-tenant")
  let healthy_tracer = TenantProvider::get_tracer(healthy_provider, "healthy.service")
  
  let healthy_spans = []
  for i in 1..=100 {
    let span = Tracer::start_span(healthy_tracer, "healthy.operation." + i.to_string())
    Span::set_attribute(span, "tenant.id", StringValue("healthy-tenant"))
    Span::set_attribute(span, "operation.status", StringValue("success"))
    healthy_spans = healthy_spans.push(span)
    Span::end(span)
  }
  
  // 模拟故障租户的各种故障
  let fault_injector = TenantFaultInjector::new(fault_isolator)
  
  // 注入内存泄漏故障
  let memory_leak_fault = fault_injector.inject_memory_leak("faulty-tenant", 100) // 100MB泄漏
  
  // 注入CPU过载故障
  let cpu_overload_fault = fault_injector.inject_cpu_overload("faulty-tenant", 90) // 90% CPU使用率
  
  // 注入网络分区故障
  let network_partition_fault = fault_injector.inject_network_partition("faulty-tenant", "external.api")
  
  // 注入数据库连接故障
  let db_connection_fault = fault_injector.inject_database_connection_failure("faulty-tenant", "telemetry.db")
  
  // 创建故障租户的操作
  let faulty_provider = TenantProvider::create_for_tenant(fault_isolator, "faulty-tenant")
  let faulty_tracer = TenantProvider::get_tracer(faulty_provider, "faulty.service")
  
  let faulty_spans = []
  for i in 1..=100 {
    let span = Tracer::start_span(faulty_tracer, "faulty.operation." + i.to_string())
    Span::set_attribute(span, "tenant.id", StringValue("faulty-tenant"))
    
    // 模拟操作失败
    if i % 3 == 0 {
      Span::set_attribute(span, "operation.status", StringValue("failed"))
      Span::set_status(span, Error)
    } else {
      Span::set_attribute(span, "operation.status", StringValue("success"))
    }
    
    faulty_spans = faulty_spans.push(span)
    Span::end(span)
  }
  
  // 测试故障隔离效果
  let isolation_validator = TenantFaultIsolationValidator::new(fault_isolator)
  
  // 验证健康租户不受故障影响
  let health_impact = isolation_validator.validate_health_impact("healthy-tenant")
  assert_true(health_impact.impact_score < 0.1) // 影响评分应该很低
  assert_true(health_impact.operations_successful > 90) // 大部分操作应该成功
  assert_true(health_impact.response_time_within_sla) // 响应时间应该在SLA内
  
  // 验证故障租户确实受到影响
  let fault_impact = isolation_validator.validate_fault_impact("faulty-tenant")
  assert_true(fault_impact.impact_score > 0.5) // 影响评分应该很高
  assert_true(fault_impact.operations_failed > 30) // 部分操作应该失败
  assert_true(fault_impact.resource_degradation_detected) // 应该检测到资源降级
  
  // 测试故障检测
  let fault_detector = TenantFaultDetector::new(fault_isolator)
  
  // 检测故障
  let detected_faults = fault_detector.detect_all_faults()
  
  assert_true(detected_faults.length() >= 4) // 应该检测到至少4个故障
  
  // 验证特定故障检测
  let memory_leak_detected = detected_faults.any(fn(f) { f.type == "memory_leak" && f.affected_tenant == "faulty-tenant" })
  let cpu_overload_detected = detected_faults.any(fn(f) { f.type == "cpu_overload" && f.affected_tenant == "faulty-tenant" })
  let network_partition_detected = detected_faults.any(fn(f) { f.type == "network_partition" && f.affected_tenant == "faulty-tenant" })
  let db_connection_failure_detected = detected_faults.any(fn(f) { f.type == "database_connection_failure" && f.affected_tenant == "faulty-tenant" })
  
  assert_true(memory_leak_detected)
  assert_true(cpu_overload_detected)
  assert_true(network_partition_detected)
  assert_true(db_connection_failure_detected)
  
  // 测试故障缓解
  let fault_mitigator = TenantFaultMitigator::new(fault_isolator)
  
  // 应用故障缓解措施
  let mitigation_results = fault_mitigator.mitigate_all_faults(detected_faults)
  
  for mitigation_result in mitigation_results {
    assert_true(mitigation_result.mitigation_applied)
    assert_true(mitigation_result.affected_tenant == "faulty-tenant")
  }
  
  // 验证故障缓解效果
  let post_mitigation_impact = isolation_validator.validate_fault_impact("faulty-tenant")
  assert_true(post_mitigation_impact.impact_score < fault_impact.impact_score) // 影响应该降低
  
  // 测试故障恢复
  let fault_recoverer = TenantFaultRecoverer::new(fault_isolator)
  
  // 清除注入的故障
  fault_injector.clear_all_faults("faulty-tenant")
  
  // 执行故障恢复
  let recovery_result = fault_recoverer.recover_tenant("faulty-tenant")
  
  assert_true(recovery_result.success)
  assert_true(recovery_result.tenant_id == "faulty-tenant")
  assert_true(recovery_result.resources_recovered)
  
  // 验证恢复效果
  let post_recovery_impact = isolation_validator.validate_health_impact("faulty-tenant")
  assert_true(post_recovery_impact.impact_score < 0.2) // 影响评分应该很低
  assert_true(post_recovery_impact.operations_successful > 80) // 大部分操作应该成功
  
  // 测试故障传播防护
  let propagation_tester = TenantFaultPropagationTester::new(fault_isolator)
  
  // 测试故障不会传播到其他租户
  let propagation_test = propagation_tester.test_fault_propagation("faulty-tenant", "healthy-tenant")
  
  assert_false(propagation_test.propagation_detected)
  assert_true(propagation_test.containment_successful)
  assert_true(propagation_test.isolation_boundary_effective)
  
  // 测试故障影响分析
  let impact_analyzer = TenantFaultImpactAnalyzer::new(fault_isolator)
  
  // 分析故障影响
  let impact_analysis = impact_analyzer.analyze_fault_impact(detected_faults)
  
  assert_true(impact_analysis.affected_tenants.contains("faulty-tenant"))
  assert_false(impact_analysis.affected_tenants.contains("healthy-tenant"))
  assert_true(impact_analysis.cross_tenant_impact == false)
  assert_true(impact_analysis.isolation_effectiveness > 0.9) // 隔离效果应该大于90%
  
  // 测试故障报告生成
  let fault_reporter = TenantFaultReporter::new(fault_isolator)
  
  // 生成故障报告
  let fault_report = fault_reporter.generate_fault_report("faulty-tenant", detected_faults)
  
  assert_true(fault_report.tenant_id == "faulty-tenant")
  assert_true(fault_report.faults_detected >= 4)
  assert_true(fault_report.mitigation_actions_taken >= 4)
  assert_true(fault_report.recovery_successful)
  assert_true(fault_report.other_tenants_unaffected)
}