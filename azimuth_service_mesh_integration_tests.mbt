// Azimuth Service Mesh Integration Test Suite
// 服务网格集成测试套件 - 测试遥测系统与服务网格的集成功能

// Test 1: Istio服务网格遥测集成
test "istio service mesh telemetry integration" {
  // 创建Istio集成管理器
  let istio_manager = @azimuth.IstioIntegrationManager::new(
    @azimuth.IstioConfig {
      mesh_name: "istio-system",
      telemetry_version: "v1.12",
      enable_tracing: true,
      enable_metrics: true,
      enable_access_logs: true,
      sampling_rate: 1.0,
      tracing_endpoint: "http://jaeger-collector.istio-system:14268/api/traces",
      metrics_endpoint: "http://prometheus.istio-system:9090",
      custom_headers: [
        ("x-request-id", "auto-generated"),
        ("x-b3-traceid", "auto-propagated"),
        ("x-b3-spanid", "auto-propagated"),
        ("x-b3-parentspanid", "auto-propagated"),
        ("x-b3-sampled", "auto-propagated"),
        ("x-ot-span-context", "auto-propagated")
      ]
    }
  )
  
  // 创建服务网格资源
  let mesh_resource = @azimuth.Resource::with_attributes(
    @azimuth.Resource::new(),
    [
      ("service.name", @azimuth.StringValue("payment-service")),
      ("service.version", @azimuth.StringValue("v2.1.0")),
      ("service.instance.id", @azimuth.StringValue("payment-pod-12345")),
      ("namespace", @azimuth.StringValue("finance")),
      ("mesh.name", @azimuth.StringValue("istio-system")),
      ("mesh.version", @azimuth.StringValue("1.12.0")),
      ("workload.name", @azimuth.StringValue("payment")),
      ("workload.kind", @azimuth.StringValue("deployment"))
    ]
  )
  
  // 模拟Istio入口请求
  let ingress_span = @azimuth.Span::new(
    "istio-ingress.request",
    @azimuth.Server,
    @azimuth.SpanContext::new("istio-trace-001", "istio-span-001", true, "")
  )
  
  // 添加Istio特定属性
  @azimuth.Span::set_attribute(ingress_span, "http.method", @azimuth.StringValue("POST"))
  @azimuth.Span::set_attribute(ingress_span, "http.url", @azimuth.StringValue("/api/v1/payments"))
  @azimuth.Span::set_attribute(ingress_span, "http.scheme", @azimuth.StringValue("https"))
  @azimuth.Span::set_attribute(ingress_span, "http.host", @azimuth.StringValue("api.example.com"))
  @azimuth.Span::set_attribute(ingress_span, "http.user_agent", @azimuth.StringValue("Mozilla/5.0"))
  @azimuth.Span::set_attribute(ingress_span, "http.protocol", @azimuth.StringValue("HTTP/2"))
  @azimuth.Span::set_attribute(ingress_span, "net.host.name", @azimuth.StringValue("istio-ingressgateway"))
  @azimuth.Span::set_attribute(ingress_span, "net.host.port", @azimuth.IntValue(443))
  @azimuth.Span::set_attribute(ingress_span, "istio.policy.name", @azimuth.StringValue("payment-policy"))
  @azimuth.Span::set_attribute(ingress_span, "istio.policy.action", @azimuth.StringValue("allow"))
  
  // 添加Istio入口事件
  @azimuth.Span::add_event(ingress_span, "istio.authz.allowed", Some([
    ("source.principal", @azimuth.StringValue("cluster.local/ns/istio-system/sa/istio-ingressgateway-service-account")),
    ("destination.principal", @azimuth.StringValue("cluster.local/ns/finance/sa/payment-service-account")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(ingress_span, "istio.routing.selected", Some([
    ("destination.service", @azimuth.StringValue("payment-service.finance.svc.cluster.local")),
    ("destination.subset", @azimuth.StringValue("v2-1-0")),
    ("destination.weight", @azimuth.IntValue(100)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  // 创建服务间调用Span
  let service_call_span = @azimuth.Span::new(
    "payment-service.process",
    @azimuth.Internal,
    @azimuth.SpanContext::new("istio-trace-001", "istio-span-002", true, "")
  )
  
  // 添加服务调用属性
  @azimuth.Span::set_attribute(service_call_span, "service.name", @azimuth.StringValue("payment-service"))
  @azimuth.Span::set_attribute(service_call_span, "service.version", @azimuth.StringValue("v2.1.0"))
  @azimuth.Span::set_attribute(service_call_span, "operation.name", @azimuth.StringValue("process_payment"))
  
  // 添加数据库调用Span
  let db_call_span = @azimuth.Span::new(
    "database.query",
    @azimuth.Client,
    @azimuth.SpanContext::new("istio-trace-001", "istio-span-003", true, "")
  )
  
  // 添加数据库调用属性
  @azimuth.Span::set_attribute(db_call_span, "db.system", @azimuth.StringValue("postgresql"))
  @azimuth.Span::set_attribute(db_call_span, "db.name", @azimuth.StringValue("payments"))
  @azimuth.Span::set_attribute(db_call_span, "db.statement", @azimuth.StringValue("INSERT INTO transactions (...) VALUES (...)"))
  @azimuth.Span::set_attribute(db_call_span, "db.user", @azimuth.StringValue("payment_user"))
  @azimuth.Span::set_attribute(db_call_span, "net.peer.name", @azimuth.StringValue("postgres-primary.finance.svc.cluster.local"))
  @azimuth.Span::set_attribute(db_call_span, "net.peer.port", @azimuth.IntValue(5432))
  @azimuth.Span::set_attribute(db_call_span, "istio.canonical_service", @azimuth.StringValue("payment-service"))
  
  // 结束Span
  @azimuth.Span::end(db_call_span)
  @azimuth.Span::end(service_call_span)
  @azimuth.Span::end(ingress_span)
  
  // 验证Istio集成
  let istio_context = @azimuth.IstioIntegrationManager::extract_istio_context(ingress_span)
  assert_eq(istio_context.trace_id, "istio-trace-001")
  assert_eq(istio_context.request_protocol, "HTTP/2")
  assert_eq(istio_context.destination_service, "payment-service.finance.svc.cluster.local")
  
  // 验证Istio指标
  let istio_metrics = @azimuth.IstioIntegrationManager::generate_istio_metrics(istio_manager, ingress_span)
  assert_true(istio_metrics.length() > 0)
  
  let request_count_metric = istio_metrics.find(fn(m) { m.name == "istio_requests_total" })
  match request_count_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("response_code", "200")))
      assert_true(metric.labels.contains(("destination_service", "payment-service")))
    }
    None => assert_true(false)
  }
  
  let request_duration_metric = istio_metrics.find(fn(m) { m.name == "istio_request_duration_milliseconds" })
  match request_duration_metric {
    Some(metric) => {
      assert_true(metric.value > 0.0)
      assert_true(metric.labels.contains(("destination_service", "payment-service")))
    }
    None => assert_true(false)
  }
}

// Test 2: Linkerd服务网格遥测集成
test "linkerd service mesh telemetry integration" {
  // 创建Linkerd集成管理器
  let linkerd_manager = @azimuth.LinkerdIntegrationManager::new(
    @azimuth.LinkerdConfig {
      mesh_name: "linkerd",
      telemetry_version: "stable-2.12",
      enable_profiles: true,
      enable_tap: true,
      enable_stat: true,
      enable_proxy: true,
      controller_namespace: "linkerd",
      proxy_inbound_port: 4143,
      proxy_outbound_port: 4140,
      admin_port: 9990
    }
  )
  
  // 创建Linkerd资源
  let linkerd_resource = @azimuth.Resource::with_attributes(
    @azimuth.Resource::new(),
    [
      ("service.name", @azimuth.StringValue("user-service")),
      ("service.version", @azimuth.StringValue("v1.5.2")),
      ("service.instance.id", @azimuth.StringValue("user-pod-67890")),
      ("namespace", @azimuth.StringValue("users")),
      ("mesh.name", @azimuth.StringValue("linkerd")),
      ("mesh.version", @azimuth.StringValue("stable-2.12")),
      ("linkerd.controller.namespace", @azimuth.StringValue("linkerd")),
      ("deployment.name", @azimuth.StringValue("user-service"))
    ]
  )
  
  // 模拟Linkerd代理请求
  let proxy_span = @azimuth.Span::new(
    "linkerd.proxy.request",
    @azimuth.Server,
    @azimuth.SpanContext::new("linkerd-trace-001", "linkerd-span-001", true, "")
  )
  
  // 添加Linkerd特定属性
  @azimuth.Span::set_attribute(proxy_span, "http.method", @azimuth.StringValue("GET"))
  @azimuth.Span::set_attribute(proxy_span, "http.url", @azimuth.StringValue("/api/users/12345"))
  @azimuth.Span::set_attribute(proxy_span, "http.scheme", @azimuth.StringValue("http"))
  @azimuth.Span::set_attribute(proxy_span, "http.host", @azimuth.StringValue("user-service.users.svc.cluster.local:80"))
  @azimuth.Span::set_attribute(proxy_span, "linkerd.proxy.direction", @azimuth.StringValue("inbound"))
  @azimuth.Span::set_attribute(proxy_span, "linkerd.proxy.src.addr", @azimuth.StringValue("10.244.0.15:38264"))
  @azimuth.Span::set_attribute(proxy_span, "linkerd.proxy.dst.addr", @azimuth.StringValue("10.244.1.20:8080"))
  @azimuth.Span::set_attribute(proxy_span, "linkerd.proxy.control.port", @azimuth.IntValue(4190))
  @azimuth.Span::set_attribute(proxy_span, "linkerd.proxy.inbound.port", @azimuth.IntValue(4143))
  
  // 添加Linkerd代理事件
  @azimuth.Span::add_event(proxy_span, "linkerd.proxy.started", Some([
    ("proxy.direction", @azimuth.StringValue("inbound")),
    ("src.addr", @azimuth.StringValue("10.244.0.15:38264")),
    ("dst.addr", @azimuth.StringValue("10.244.1.20:8080")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(proxy_span, "linkerd.proxy.terminated", Some([
    ("proxy.direction", @azimuth.StringValue("inbound")),
    ("response.status", @azimuth.IntValue(200)),
    ("response.bytes", @azimuth.IntValue(1024)),
    ("duration_ms", @azimuth.IntValue(45)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 45))
  ]))
  
  // 创建服务调用Span
  let outbound_span = @azimuth.Span::new(
    "linkerd.proxy.outbound",
    @azimuth.Client,
    @azimuth.SpanContext::new("linkerd-trace-001", "linkerd-span-002", true, "")
  )
  
  // 添加出站调用属性
  @azimuth.Span::set_attribute(outbound_span, "linkerd.proxy.direction", @azimuth.StringValue("outbound"))
  @azimuth.Span::set_attribute(outbound_span, "linkerd.proxy.src.addr", @azimuth.StringValue("10.244.1.20:41258"))
  @azimuth.Span::set_attribute(outbound_span, "linkerd.proxy.dst.addr", @azimuth.StringValue("10.244.2.5:4140"))
  @azimuth.Span::set_attribute(outbound_span, "linkerd.proxy.outbound.port", @azimuth.IntValue(4140))
  @azimuth.Span::set_attribute(outbound_span, "net.peer.name", @azimuth.StringValue("auth-service.users.svc.cluster.local"))
  @azimuth.Span::set_attribute(outbound_span, "net.peer.port", @azimuth.IntValue(8080))
  
  // 结束Span
  @azimuth.Span::end(outbound_span)
  @azimuth.Span::end(proxy_span)
  
  // 验证Linkerd集成
  let linkerd_context = @azimuth.LinkerdIntegrationManager::extract_linkerd_context(proxy_span)
  assert_eq(linkerd_context.trace_id, "linkerd-trace-001")
  assert_eq(linkerd_context.proxy_direction, "inbound")
  assert_eq(linkerd_context.src_addr, "10.244.0.15:38264")
  assert_eq(linkerd_context.dst_addr, "10.244.1.20:8080")
  
  // 验证Linkerd指标
  let linkerd_metrics = @azimuth.LinkerdIntegrationManager::generate_linkerd_metrics(linkerd_manager, proxy_span)
  assert_true(linkerd_metrics.length() > 0)
  
  let request_total_metric = linkerd_metrics.find(fn(m) { m.name == "linkerd_protocol_total" })
  match request_total_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("direction", "inbound")))
      assert_true(metric.labels.contains(("protocol", "HTTP/1")))
    }
    None => assert_true(false)
  }
  
  let response_latency_metric = linkerd_metrics.find(fn(m) { m.name == "linkerd_protocol_response_latency_ms" })
  match response_latency_metric {
    Some(metric) => {
      assert_true(metric.value > 0.0)
      assert_true(metric.labels.contains(("direction", "inbound")))
      assert_true(metric.labels.contains(("protocol", "HTTP/1")))
    }
    None => assert_true(false)
  }
}

// Test 3: Consul Connect服务网格遥测集成
test "consul connect service mesh telemetry integration" {
  // 创建Consul Connect集成管理器
  let consul_manager = @azimuth.ConsulConnectIntegrationManager::new(
    @azimuth.ConsulConnectConfig {
      datacenter: "dc1",
      enable_service_mesh: true,
      enable_intentions: true,
      enable_gateways: true,
      enable_ingress_gateways: true,
      proxy_mode: "transparent",
      local_agent_address: "127.0.0.1:8500",
      ca_file: "/etc/consul/tls/ca.pem",
      cert_file: "/etc/consul/tls/client.pem",
      key_file: "/etc/consul/tls/client-key.pem"
    }
  )
  
  // 创建Consul Connect资源
  let consul_resource = @azimuth.Resource::with_attributes(
    @azimuth.Resource::new(),
    [
      ("service.name", @azimuth.StringValue("inventory-service")),
      ("service.version", @azimuth.StringValue("v3.2.1")),
      ("service.instance.id", @azimuth.StringValue("inventory-pod-11111")),
      ("namespace", @azimuth.StringValue("inventory")),
      ("mesh.name", @azimuth.StringValue("consul-connect")),
      ("consul.datacenter", @azimuth.StringValue("dc1")),
      ("consul.partition", @azimuth.StringValue("default")),
      ("consul.namespace", @azimuth.StringValue("inventory")),
      ("consul.service", @azimuth.StringValue("inventory-service")),
      ("consul.proxy.mode", @azimuth.StringValue("transparent"))
    ]
  )
  
  // 模拟Consul Connect代理请求
  let connect_span = @azimuth.Span::new(
    "consul.connect.proxy",
    @azimuth.Server,
    @azimuth.SpanContext::new("consul-trace-001", "consul-span-001", true, "")
  )
  
  // 添加Consul Connect特定属性
  @azimuth.Span::set_attribute(connect_span, "http.method", @azimuth.StringValue("PUT"))
  @azimuth.Span::set_attribute(connect_span, "http.url", @azimuth.StringValue("/api/inventory/items/98765"))
  @azimuth.Span::set_attribute(connect_span, "http.scheme", @azimuth.StringValue("https"))
  @azimuth.Span::set_attribute(connect_span, "http.host", @azimuth.StringValue("inventory-service.inventory.consul"))
  @azimuth.Span::set_attribute(connect_span, "consul.connect.source.service", @azimuth.StringValue("order-service"))
  @azimuth.Span::set_attribute(connect_span, "consul.connect.source.namespace", @azimuth.StringValue("orders"))
  @azimuth.Span::set_attribute(connect_span, "consul.connect.destination.service", @azimuth.StringValue("inventory-service"))
  @azimuth.Span::set_attribute(connect_span, "consul.connect.destination.namespace", @azimuth.StringValue("inventory"))
  @azimuth.Span::set_attribute(connect_span, "consul.connect.intention.action", @azimuth.StringValue("allow"))
  @azimuth.Span::set_attribute(connect_span, "consul.connect.proxy.mode", @azimuth.StringValue("transparent"))
  
  // 添加Consul Connect代理事件
  @azimuth.Span::add_event(connect_span, "consul.connect.intention.checked", Some([
    ("source.service", @azimuth.StringValue("order-service")),
    ("source.namespace", @azimuth.StringValue("orders")),
    ("destination.service", @azimuth.StringValue("inventory-service")),
    ("destination.namespace", @azimuth.StringValue("inventory")),
    ("intention.action", @azimuth.StringValue("allow")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(connect_span, "consul.connect.tls.established", Some([
    ("tls.version", @azimuth.StringValue("TLSv1.3")),
    ("cipher.suite", @azimuth.StringValue("TLS_AES_256_GCM_SHA384")),
    ("source.uri", @azimuth.StringValue("spiffe://consul/dc1/ns/orders/order-service")),
    ("destination.uri", @azimuth.StringValue("spiffe://consul/dc1/ns/inventory/inventory-service")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 10))
  ]))
  
  @azimuth.Span::add_event(connect_span, "consul.connect.request.completed", Some([
    ("http.status_code", @azimuth.IntValue(200)),
    ("request.bytes", @azimuth.IntValue(512)),
    ("response.bytes", @azimuth.IntValue(256)),
    ("duration_ms", @azimuth.IntValue(75)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 75))
  ]))
  
  // 结束Span
  @azimuth.Span::end(connect_span)
  
  // 验证Consul Connect集成
  let consul_context = @azimuth.ConsulConnectIntegrationManager::extract_consul_context(connect_span)
  assert_eq(consul_context.trace_id, "consul-trace-001")
  assert_eq(consul_context.source_service, "order-service")
  assert_eq(consul_context.destination_service, "inventory-service")
  assert_eq(consul_context.intention_action, "allow")
  
  // 验证Consul Connect指标
  let consul_metrics = @azimuth.ConsulConnectIntegrationManager::generate_consul_metrics(consul_manager, connect_span)
  assert_true(consul_metrics.length() > 0)
  
  let connect_request_metric = consul_metrics.find(fn(m) { m.name == "consul_connect_connect_request_duration_seconds" })
  match connect_request_metric {
    Some(metric) => {
      assert_true(metric.value > 0.0)
      assert_true(metric.labels.contains(("source_service", "order-service")))
      assert_true(metric.labels.contains(("destination_service", "inventory-service")))
    }
    None => assert_true(false)
  }
  
  let tls_handshake_metric = consul_metrics.find(fn(m) { m.name == "consul_connect_tls_handshake_duration_seconds" })
  match tls_handshake_metric {
    Some(metric) => {
      assert_true(metric.value > 0.0)
      assert_true(metric.labels.contains(("source_service", "order-service")))
      assert_true(metric.labels.contains(("destination_service", "inventory-service")))
    }
    None => assert_true(false)
  }
}

// Test 4: 服务网格流量管理遥测
test "service mesh traffic management telemetry" {
  // 创建流量管理器
  let traffic_manager = @azimuth.MeshTrafficManager::new(
    @azimuth.TrafficManagementConfig {
      mesh_type: "istio",
      enable_canary_deployments: true,
      enable_circuit_breaking: true,
      enable_fault_injection: true,
      enable_retry_policies: true,
      enable_timeout_policies: true,
      enable_load_balancing: true,
      default_load_balancing_policy: "round_robin"
    }
  )
  
  // 模拟金丝雀部署流量
  let canary_span = @azimuth.Span::new(
    "mesh.traffic.canary",
    @azimuth.Server,
    @azimuth.SpanContext::new("traffic-trace-001", "traffic-span-001", true, "")
  )
  
  // 添加金丝雀部署属性
  @azimuth.Span::set_attribute(canary_span, "service.name", @azimuth.StringValue("product-service"))
  @azimuth.Span::set_attribute(canary_span, "mesh.traffic.policy", @azimuth.StringValue("canary"))
  @azimuth.Span::set_attribute(canary_span, "mesh.traffic.subset.v1", @azimuth.StringValue("stable"))
  @azimuth.Span::set_attribute(canary_span, "mesh.traffic.subset.v2", @azimuth.StringValue("canary"))
  @azimuth.Span::set_attribute(canary_span, "mesh.traffic.weight.v1", @azimuth.IntValue(90))
  @azimuth.Span::set_attribute(canary_span, "mesh.traffic.weight.v2", @azimuth.IntValue(10))
  
  // 添加流量路由事件
  @azimuth.Span::add_event(canary_span, "mesh.traffic.routed", Some([
    ("destination.subset", @azimuth.StringValue("stable")),
    ("routing.weight", @azimuth.IntValue(90)),
    ("routing.reason", @azimuth.StringValue("canary_deployment")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  // 创建熔断器Span
  let circuit_breaker_span = @azimuth.Span::new(
    "mesh.traffic.circuit_breaker",
    @azimuth.Client,
    @azimuth.SpanContext::new("traffic-trace-002", "traffic-span-002", true, "")
  )
  
  // 添加熔断器属性
  @azimuth.Span::set_attribute(circuit_breaker_span, "service.name", @azimuth.StringValue("review-service"))
  @azimuth.Span::set_attribute(circuit_breaker_span, "mesh.circuit_breaker.enabled", @azimuth.StringValue("true"))
  @azimuth.Span::set_attribute(circuit_breaker_span, "mesh.circuit_breaker.consecutive_errors", @azimuth.IntValue(5))
  @azimuth.Span::set_attribute(circuit_breaker_span, "mesh.circuit_breaker.interval", @azimuth.StringValue("30s"))
  @azimuth.Span::set_attribute(circuit_breaker_span, "mesh.circuit_breaker.base_ejection_time", @azimuth.StringValue("30s"))
  @azimuth.Span::set_attribute(circuit_breaker_span, "mesh.circuit_breaker.max_ejection_percent", @azimuth.IntValue(50))
  
  // 添加熔断器事件
  @azimuth.Span::add_event(circuit_breaker_span, "mesh.circuit_breaker.trippped", Some([
    ("error_count", @azimuth.IntValue(5)),
    ("error_rate", @azimuth.FloatValue(0.8)),
    ("consecutive_errors", @azimuth.IntValue(5)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(circuit_breaker_span, "mesh.circuit_breaker.calls_rejected", Some([
    ("rejected_count", @azimuth.IntValue(15)),
    ("rejection_reason", @azimuth.StringValue("circuit_open")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 10000))
  ]))
  
  // 结束Span
  @azimuth.Span::end(circuit_breaker_span)
  @azimuth.Span::end(canary_span)
  
  // 验证流量管理指标
  let traffic_metrics = @azimuth.MeshTrafficManager::generate_traffic_metrics(traffic_manager, canary_span)
  assert_true(traffic_metrics.length() > 0)
  
  let canary_metric = traffic_metrics.find(fn(m) { m.name == "mesh_canary_requests_total" })
  match canary_metric {
    Some(metric) => {
      assert_true(metric.value >= 0.0)
      assert_true(metric.labels.contains(("service", "product-service")))
      assert_true(metric.labels.contains(("subset", "v1")) or metric.labels.contains(("subset", "v2")))
    }
    None => assert_true(false)
  }
  
  let circuit_breaker_metrics = @azimuth.MeshTrafficManager::generate_traffic_metrics(traffic_manager, circuit_breaker_span)
  assert_true(circuit_breaker_metrics.length() > 0)
  
  let cb_tripped_metric = circuit_breaker_metrics.find(fn(m) { m.name == "mesh_circuit_breaker_tripped_total" })
  match cb_tripped_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("service", "review-service")))
    }
    None => assert_true(false)
  }
  
  let cb_rejected_metric = circuit_breaker_metrics.find(fn(m) { m.name == "mesh_circuit_breaker_calls_rejected_total" })
  match cb_rejected_metric {
    Some(metric) => {
      assert_eq(metric.value, 15.0)
      assert_true(metric.labels.contains(("service", "review-service")))
    }
    None => assert_true(false)
  }
}

// Test 5: 服务网格安全性遥测
test "service mesh security telemetry" {
  // 创建安全管理器
  let security_manager = @azimuth.MeshSecurityManager::new(
    @azimuth.MeshSecurityConfig {
      mesh_type: "istio",
      enable_mtls: true,
      enable_mutual_tls: true,
      enable_certificate_rotation: true,
      enable_authorization_policy: true,
      enable_peer_authentication: true,
      enable_request_authentication: true,
      certificate_rotation_interval: "24h",
      min_certificate_lifetime: "1h"
    }
  )
  
  // 模拟mTLS认证Span
  let mtls_span = @azimuth.Span::new(
    "mesh.security.mtls",
    @azimuth.Server,
    @azimuth.SpanContext::new("security-trace-001", "security-span-001", true, "")
  )
  
  // 添加mTLS属性
  @azimuth.Span::set_attribute(mtls_span, "service.name", @azimuth.StringValue("notification-service"))
  @azimuth.Span::set_attribute(mtls_span, "mesh.security.mtls.enabled", @azimuth.StringValue("true"))
  @azimuth.Span::set_attribute(mtls_span, "mesh.security.tls.mode", @azimuth.StringValue("ISTIO_MUTUAL"))
  @azimuth.Span::set_attribute(mtls_span, "mesh.security.source.principal", @azimuth.StringValue("cluster.local/ns/notifications/sa/notification-service-account"))
  @azimuth.Span::set_attribute(mtls_span, "mesh.security.destination.principal", @azimuth.StringValue("cluster.local/ns/messaging/sa/email-service-account"))
  
  // 添加mTLS事件
  @azimuth.Span::add_event(mtls_span, "mesh.security.tls.handshake.started", Some([
    ("tls.version", @azimuth.StringValue("TLSv1.3")),
    ("cipher.suite", @azimuth.StringValue("TLS_AES_256_GCM_SHA384")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(mtls_span, "mesh.security.tls.handshake.completed", Some([
    ("tls.version", @azimuth.StringValue("TLSv1.3")),
    ("cipher.suite", @azimuth.StringValue("TLS_AES_256_GCM_SHA384")),
    ("certificate.source", @azimuth.StringValue("istio-ca")),
    ("certificate.valid_from", @azimuth.IntValue(@azimuth.current_timestamp() - 86400000)),
    ("certificate.valid_until", @azimuth.IntValue(@azimuth.current_timestamp() + 30 * 86400000)),
    ("handshake.duration_ms", @azimuth.IntValue(25)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 25))
  ]))
  
  // 模拟授权策略Span
  let authz_span = @azimuth.Span::new(
    "mesh.security.authorization",
    @azimuth.Server,
    @azimuth.SpanContext::new("security-trace-002", "security-span-002", true, "")
  )
  
  // 添加授权策略属性
  @azimuth.Span::set_attribute(authz_span, "service.name", @azimuth.StringValue("admin-service"))
  @azimuth.Span::set_attribute(authz_span, "mesh.security.authorization.enabled", @azimuth.StringValue("true"))
  @azimuth.Span::set_attribute(authz_span, "mesh.security.authorization.policy", @azimuth.StringValue("admin-access-policy"))
  @azimuth.Span::set_attribute(authz_span, "mesh.security.authorization.action", @azimuth.StringValue("ALLOW"))
  @azimuth.Span::set_attribute(authz_span, "mesh.security.authorization.source.principal", @azimuth.StringValue("cluster.local/ns/admin/sa/admin-service-account"))
  @azimuth.Span::set_attribute(authz_span, "mesh.security.authorization.source.namespace", @azimuth.StringValue("admin"))
  @azimuth.Span::set_attribute(authz_span, "mesh.security.authorization.request.principal", @azimuth.StringValue("cluster.local/ns/frontend/sa/frontend-service-account"))
  @azimuth.Span::set_attribute(authz_span, "mesh.security.authorization.request.namespace", @azimuth.StringValue("frontend"))
  
  // 添加授权策略事件
  @azimuth.Span::add_event(authz_span, "mesh.security.authorization.evaluated", Some([
    ("policy.name", @azimuth.StringValue("admin-access-policy")),
    ("policy.action", @azimuth.StringValue("ALLOW")),
    ("evaluation.result", @azimuth.StringValue("allowed")),
    ("evaluation.duration_ms", @azimuth.IntValue(5)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  // 模拟证书轮换Span
  let cert_rotation_span = @azimuth.Span::new(
    "mesh.security.certificate.rotation",
    @azimuth.Internal,
    @azimuth.SpanContext::new("security-trace-003", "security-span-003", true, "")
  )
  
  // 添加证书轮换属性
  @azimuth.Span::set_attribute(cert_rotation_span, "service.name", @azimuth.StringValue("analytics-service"))
  @azimuth.Span::set_attribute(cert_rotation_span, "mesh.security.certificate.rotation.enabled", @azimuth.StringValue("true"))
  @azimuth.Span::set_attribute(cert_rotation_span, "mesh.security.certificate.rotation.interval", @azimuth.StringValue("24h"))
  @azimuth.Span::set_attribute(cert_rotation_span, "mesh.security.certificate.old.serial", @azimuth.StringValue("1234567890abcdef"))
  @azimuth.Span::set_attribute(cert_rotation_span, "mesh.security.certificate.new.serial", @azimuth.StringValue("fedcba0987654321"))
  
  // 添加证书轮换事件
  @azimuth.Span::add_event(cert_rotation_span, "mesh.security.certificate.rotation.started", Some([
    ("rotation.reason", @azimuth.StringValue("scheduled")),
    ("old.cert.not.after", @azimuth.IntValue(@azimuth.current_timestamp() + 3600000)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(cert_rotation_span, "mesh.security.certificate.rotation.completed", Some([
    ("new.cert.serial", @azimuth.StringValue("fedcba0987654321")),
    ("new.cert.not.before", @azimuth.IntValue(@azimuth.current_timestamp())),
    ("new.cert.not.after", @azimuth.IntValue(@azimuth.current_timestamp() + 25 * 86400000)),
    ("rotation.duration_ms", @azimuth.IntValue(150)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 150))
  ]))
  
  // 结束Span
  @azimuth.Span::end(cert_rotation_span)
  @azimuth.Span::end(authz_span)
  @azimuth.Span::end(mtls_span)
  
  // 验证安全指标
  let security_metrics = @azimuth.MeshSecurityManager::generate_security_metrics(security_manager, mtls_span)
  assert_true(security_metrics.length() > 0)
  
  let mtls_metric = security_metrics.find(fn(m) { m.name == "mesh_mtls_handshake_success_total" })
  match mtls_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("service", "notification-service")))
      assert_true(metric.labels.contains(("tls_version", "1.3")))
    }
    None => assert_true(false)
  }
  
  let authz_metrics = @azimuth.MeshSecurityManager::generate_security_metrics(security_manager, authz_span)
  assert_true(authz_metrics.length() > 0)
  
  let authz_allowed_metric = authz_metrics.find(fn(m) { m.name == "mesh_authorization_allowed_total" })
  match authz_allowed_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("service", "admin-service")))
      assert_true(metric.labels.contains(("policy", "admin-access-policy")))
    }
    None => assert_true(false)
  }
  
  let cert_rotation_metrics = @azimuth.MeshSecurityManager::generate_security_metrics(security_manager, cert_rotation_span)
  assert_true(cert_rotation_metrics.length() > 0)
  
  let cert_rotation_metric = cert_rotation_metrics.find(fn(m) { m.name == "mesh_certificate_rotation_total" })
  match cert_rotation_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("service", "analytics-service")))
      assert_true(metric.labels.contains(("reason", "scheduled")))
    }
    None => assert_true(false)
  }
}

// Test 6: 多服务网格环境遥测
test "multi-mesh environment telemetry" {
  // 创建多网格管理器
  let multi_mesh_manager = @azimuth.MultiMeshManager::new(
    @azimuth.MultiMeshConfig {
      primary_mesh: "istio",
      secondary_meshes: ["linkerd", "consul-connect"],
      enable_mesh_gateway: true,
      enable_cross_mesh_routing: true,
      enable_mesh_federation: true,
      mesh_gateway_port: 15443,
      federation_enabled: true
    }
  )
  
  // 创建主网格资源
  let primary_mesh_resource = @azimuth.Resource::with_attributes(
    @azimuth.Resource::new(),
    [
      ("service.name", @azimuth.StringValue("gateway-service")),
      ("service.version", @azimuth.StringValue("v1.0.0")),
      ("mesh.type", @azimuth.StringValue("istio")),
      ("mesh.name", @azimuth.StringValue("istio-primary")),
      ("mesh.role", @azimuth.StringValue("primary"))
    ]
  )
  
  // 创建次网格资源
  let secondary_mesh_resource = @azimuth.Resource::with_attributes(
    @azimuth.Resource::new(),
    [
      ("service.name", @azimuth.StringValue("legacy-service")),
      ("service.version", @azimuth.StringValue("v2.3.1")),
      ("mesh.type", @azimuth.StringValue("linkerd")),
      ("mesh.name", @azimuth.StringValue("linkerd-secondary")),
      ("mesh.role", @azimuth.StringValue("secondary"))
    ]
  )
  
  // 模拟跨网格调用Span
  let cross_mesh_span = @azimuth.Span::new(
    "multi.mesh.cross.mesh.call",
    @azimuth.Client,
    @azimuth.SpanContext::new("multi-mesh-trace-001", "multi-mesh-span-001", true, "")
  )
  
  // 添加跨网格调用属性
  @azimuth.Span::set_attribute(cross_mesh_span, "source.mesh.type", @azimuth.StringValue("istio"))
  @azimuth.Span::set_attribute(cross_mesh_span, "source.mesh.name", @azimuth.StringValue("istio-primary"))
  @azimuth.Span::set_attribute(cross_mesh_span, "source.service.name", @azimuth.StringValue("gateway-service"))
  @azimuth.Span::set_attribute(cross_mesh_span, "destination.mesh.type", @azimuth.StringValue("linkerd"))
  @azimuth.Span::set_attribute(cross_mesh_span, "destination.mesh.name", @azimuth.StringValue("linkerd-secondary"))
  @azimuth.Span::set_attribute(cross_mesh_span, "destination.service.name", @azimuth.StringValue("legacy-service"))
  @azimuth.Span::set_attribute(cross_mesh_span, "multi.mesh.gateway.host", @azimuth.StringValue("istio-eastwestgateway.istio-primary.svc.cluster.local"))
  @azimuth.Span::set_attribute(cross_mesh_span, "multi.mesh.gateway.port", @azimuth.IntValue(15443))
  
  // 添加跨网格调用事件
  @azimuth.Span::add_event(cross_mesh_span, "multi.mesh.routing.started", Some([
    ("source.mesh", @azimuth.StringValue("istio-primary")),
    ("destination.mesh", @azimuth.StringValue("linkerd-secondary")),
    ("routing.type", @azimuth.StringValue("mesh_gateway")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(cross_mesh_span, "multi.mesh.gateway.connected", Some([
    ("gateway.host", @azimuth.StringValue("istio-eastwestgateway.istio-primary.svc.cluster.local")),
    ("gateway.port", @azimuth.IntValue(15443)),
    ("connection.established_ms", @azimuth.IntValue(45)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 45))
  ]))
  
  @azimuth.Span::add_event(cross_mesh_span, "multi.mesh.context.propagated", Some([
    ("trace.id", @azimuth.StringValue("multi-mesh-trace-001")),
    ("span.id", @azimuth.StringValue("multi-mesh-span-001")),
    ("propagation.format", @azimuth.StringValue("w3c-tracecontext")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 50))
  ]))
  
  @azimuth.Span::add_event(cross_mesh_span, "multi.mesh.response.received", Some([
    ("response.status", @azimuth.IntValue(200)),
    ("response.bytes", @azimuth.IntValue(2048)),
    ("total.duration_ms", @azimuth.IntValue(250)),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 250))
  ]))
  
  // 模拟网格联邦Span
  let federation_span = @azimuth.Span::new(
    "multi.mesh.federation",
    @azimuth.Internal,
    @azimuth.SpanContext::new("multi-mesh-trace-002", "multi-mesh-span-002", true, "")
  )
  
  // 添加网格联邦属性
  @azimuth.Span::set_attribute(federation_span, "federation.enabled", @azimuth.StringValue("true"))
  @azimuth.Span::set_attribute(federation_span, "federation.primary.mesh", @azimuth.StringValue("istio-primary"))
  @azimuth.Span::set_attribute(federation_span, "federation.secondary.mesh", @azimuth.StringValue("consul-connect-remote"))
  @azimuth.Span::set_attribute(federation_span, "federation.service.discovery", @azimuth.StringValue("enabled"))
  @azimuth.Span::set_attribute(federation_span, "federation.certificate.exchange", @azimuth.StringValue("enabled"))
  
  // 添加网格联邦事件
  @azimuth.Span::add_event(federation_span, "multi.mesh.federation.established", Some([
    ("primary.mesh", @azimuth.StringValue("istio-primary")),
    ("remote.mesh", @azimuth.StringValue("consul-connect-remote")),
    ("federation.type", @azimuth.StringValue("gateway")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp()))
  ]))
  
  @azimuth.Span::add_event(federation_span, "multi.mesh.service.discovered", Some([
    ("discovered.service", @azimuth.StringValue("remote-analytics-service")),
    ("source.mesh", @azimuth.StringValue("consul-connect-remote")),
    ("destination.mesh", @azimuth.StringValue("istio-primary")),
    ("timestamp", @azimuth.IntValue(@azimuth.current_timestamp() + 10000))
  ]))
  
  // 结束Span
  @azimuth.Span::end(federation_span)
  @azimuth.Span::end(cross_mesh_span)
  
  // 验证多网格指标
  let multi_mesh_metrics = @azimuth.MultiMeshManager::generate_multi_mesh_metrics(multi_mesh_manager, cross_mesh_span)
  assert_true(multi_mesh_metrics.length() > 0)
  
  let cross_mesh_metric = multi_mesh_metrics.find(fn(m) { m.name == "multi_mesh_cross_mesh_requests_total" })
  match cross_mesh_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("source_mesh", "istio-primary")))
      assert_true(metric.labels.contains(("destination_mesh", "linkerd-secondary")))
    }
    None => assert_true(false)
  }
  
  let federation_metrics = @azimuth.MultiMeshManager::generate_multi_mesh_metrics(multi_mesh_manager, federation_span)
  assert_true(federation_metrics.length() > 0)
  
  let federation_metric = federation_metrics.find(fn(m) { m.name == "multi_mesh_federation_established_total" })
  match federation_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_true(metric.labels.contains(("primary_mesh", "istio-primary")))
      assert_true(metric.labels.contains(("remote_mesh", "consul-connect-remote")))
    }
    None => assert_true(false)
  }
  
  // 验证跨网格上下文传播
  let propagated_context = @azimuth.MultiMeshManager::extract_propagated_context(cross_mesh_span)
  assert_eq(propagated_context.trace_id, "multi-mesh-trace-001")
  assert_eq(propagated_context.span_id, "multi-mesh-span-001")
  assert_eq(propagated_context.source_mesh, "istio-primary")
  assert_eq(propagated_context.destination_mesh, "linkerd-secondary")
}