// 云原生环境下的遥测测试
// 测试容器化、Kubernetes、微服务和云原生架构下的遥测功能

pub test "容器化环境遥测测试" {
  let cloud_native_manager = azimuth::CloudNativeManager::new()
  
  // 创建容器遥测管理器
  let container_telemetry = azimuth::CloudNativeManager::create_container_telemetry(cloud_native_manager)
  
  // 模拟容器环境信息
  let container_info = {
    container_id: "container-123456789",
    container_name: "telemetry-service-v1.2.3",
    image: "azimuth/telemetry:1.2.3",
    pod_name: "telemetry-pod-abcde",
    namespace: "monitoring",
    node_name: "worker-node-1",
    cluster_name: "production-cluster",
    labels: [
      ("app", "telemetry"),
      ("version", "1.2.3"),
      ("environment", "production"),
      ("team", "observability")
    ],
    annotations: [
      ("prometheus.io/scrape", "true"),
      ("prometheus.io/port", "8080"),
      ("prometheus.io/path", "/metrics")
    ]
  }
  
  // 注册容器信息
  azimuth::ContainerTelemetry::register_container(container_telemetry, container_info)
  
  // 测试容器资源遥测
  let resource_metrics = azimuth::ContainerTelemetry::collect_resource_metrics(container_telemetry)
  
  // 验证资源指标收集
  assert_true(resource_metrics.cpu_usage >= 0.0 && resource_metrics.cpu_usage <= 100.0)
  assert_true(resource_metrics.memory_usage > 0)
  assert_true(resource_metrics.memory_limit > 0)
  assert_true(resource_metrics.disk_usage >= 0)
  assert_true(resource_metrics.network_rx >= 0)
  assert_true(resource_metrics.network_tx >= 0)
  
  // 测试容器生命周期事件
  let lifecycle_events = [
    {
      event_type: "container_created",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      details: "Container started successfully"
    },
    {
      event_type: "container_running",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000000000L,
      details: "Container is running"
    },
    {
      event_type: "health_check_passed",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 2000000000L,
      details: "Health check passed"
    }
  ]
  
  for event in lifecycle_events {
    azimuth::ContainerTelemetry::record_lifecycle_event(container_telemetry, event)
  }
  
  let recorded_events = azimuth::ContainerTelemetry::get_lifecycle_events(container_telemetry)
  assert_eq(recorded_events.length(), 3)
  assert_eq(recorded_events[0].event_type, "container_created")
  assert_eq(recorded_events[1].event_type, "container_running")
  assert_eq(recorded_events[2].event_type, "health_check_passed")
  
  // 测试容器性能基准
  let performance_benchmark = azimuth::ContainerTelemetry::run_performance_benchmark(container_telemetry, {
    cpu_test_duration: 5000,      # 5秒
    memory_test_size: 1024 * 1024, # 1MB
    network_test_cycles: 100,
    disk_test_size: 1024 * 1024    # 1MB
  })
  
  assert_true(performance_benchmark.cpu_score > 0)
  assert_true(performance_benchmark.memory_score > 0)
  assert_true(performance_benchmark.network_score > 0)
  assert_true(performance_benchmark.disk_score > 0)
  assert_true(performance_benchmark.overall_score > 0)
  
  // 测试容器健康监控
  let health_monitor = azimuth::ContainerTelemetry::create_health_monitor(container_telemetry)
  
  // 设置健康检查规则
  azimuth::HealthMonitor::add_check_rule(health_monitor, {
    name: "cpu_threshold",
    metric: "cpu_usage",
    operator: ">",
    threshold: 80.0,
    severity: "warning"
  })
  
  azimuth::HealthMonitor::add_check_rule(health_monitor, {
    name: "memory_threshold",
    metric: "memory_usage_percentage",
    operator: ">",
    threshold: 90.0,
    severity: "critical"
  })
  
  azimuth::HealthMonitor::add_check_rule(health_monitor, {
    name: "disk_space",
    metric: "disk_usage_percentage",
    operator: ">",
    threshold: 85.0,
    severity: "warning"
  })
  
  // 模拟健康检查
  let health_status = azimuth::HealthMonitor::check_health(health_monitor)
  assert_true(health_status.overall == "healthy" || health_status.overall == "warning" || health_status.overall == "critical")
  
  // 测试容器自动扩缩容遥测
  let autoscaler_metrics = azimuth::ContainerTelemetry::collect_autoscaler_metrics(container_telemetry)
  
  assert_true(autoscaler_metrics.current_replicas >= 1)
  assert_true(autoscaler_metrics.desired_replicas >= 1)
  assert_true(autoscaler_metrics.target_cpu_utilization >= 0)
  assert_true(autoscaler_metrics.target_memory_utilization >= 0)
  
  // 测试容器网络遥测
  let network_metrics = azimuth::ContainerTelemetry::collect_network_metrics(container_telemetry)
  
  assert_true(network_metrics.bytes_rx >= 0)
  assert_true(network_metrics.bytes_tx >= 0)
  assert_true(network_metrics.packets_rx >= 0)
  assert_true(network_metrics.packets_tx >= 0)
  assert_true(network_metrics.connections_active >= 0)
  assert_true(network_metrics.connections_failed >= 0)
}

pub test "Kubernetes环境遥测测试" {
  let cloud_native_manager = azimuth::CloudNativeManager::new()
  
  // 创建Kubernetes遥测管理器
  let k8s_telemetry = azimuth::CloudNativeManager::create_kubernetes_telemetry(cloud_native_manager)
  
  // 模拟Kubernetes集群信息
  let cluster_info = {
    cluster_name: "production-cluster",
    cluster_version: "v1.28.0",
    region: "us-west-2",
    provider: "EKS",
    nodes_count: 15,
    namespaces_count: 25,
    pods_count: 150,
    services_count: 45
  }
  
  // 注册集群信息
  azimuth::KubernetesTelemetry::register_cluster(k8s_telemetry, cluster_info)
  
  // 测试节点级别遥测
  let node_metrics = azimuth::KubernetesTelemetry::collect_node_metrics(k8s_telemetry)
  
  assert_true(node_metrics.length() >= 10)  # 至少10个节点
  
  for node_metric in node_metrics {
    assert_true(node_metric.cpu_capacity > 0)
    assert_true(node_metric.memory_capacity > 0)
    assert_true(node_metric.cpu_allocatable >= 0)
    assert_true(node_metric.memory_allocatable >= 0)
    assert_true(node_metric.pods_capacity >= 0)
    assert_true(node_metric.pods_running >= 0)
  }
  
  // 测试Pod级别遥测
  let pod_metrics = azimuth::KubernetesTelemetry::collect_pod_metrics(k8s_telemetry, "monitoring")
  
  assert_true(pod_metrics.length() >= 5)  # 至少5个Pod
  
  for pod_metric in pod_metrics {
    assert_true(pod_metric.cpu_request >= 0)
    assert_true(pod_metric.memory_request > 0)
    assert_true(pod_metric.cpu_limit >= 0)
    assert_true(pod_metric.memory_limit > 0)
    assert_true(pod_metric.restart_count >= 0)
    assert_true(pod_metric.age_seconds > 0)
  }
  
  // 测试服务级别遥测
  let service_metrics = azimuth::KubernetesTelemetry::collect_service_metrics(k8s_telemetry)
  
  assert_true(service_metrics.length() >= 3)  # 至少3个服务
  
  for service_metric in service_metrics {
    assert_true(service_metric.endpoint_count >= 0)
    assert_true(service_metric.connection_count >= 0)
    assert_true(service_metric.request_rate >= 0)
    assert_true(service_metric.error_rate >= 0)
    assert_true(service_metric.latency_p50 >= 0)
    assert_true(service_metric.latency_p95 >= 0)
    assert_true(service_metric.latency_p99 >= 0)
  }
  
  // 测试Kubernetes事件遥测
  let k8s_events = [
    {
      type: "Normal",
      reason: "Scheduled",
      message: "Successfully assigned monitoring/telemetry-pod-abcde to worker-node-1",
      source: "default-scheduler",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
    },
    {
      type: "Normal",
      reason: "Pulling",
      message: "Pulling image \"azimuth/telemetry:1.2.3\"",
      source: "kubelet",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 1000000000L
    },
    {
      type: "Warning",
      reason: "Failed",
      message: "Failed to pull image \"azimuth/telemetry:1.2.3\": rpc error: code = Unknown",
      source: "kubelet",
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 2000000000L
    }
  ]
  
  for event in k8s_events {
    azimuth::KubernetesTelemetry::record_event(k8s_telemetry, event)
  }
  
  let recorded_events = azimuth::KubernetesTelemetry::get_events(k8s_telemetry)
  assert_eq(recorded_events.length(), 3)
  
  let warning_events = recorded_events.filter(fn(event) { event.type == "Warning" })
  assert_eq(warning_events.length(), 1)
  
  // 测试Kubernetes资源配额遥测
  let quota_metrics = azimuth::KubernetesTelemetry::collect_resource_quota_metrics(k8s_telemetry)
  
  assert_true(quota_metrics.length() >= 1)
  
  for quota_metric in quota_metrics {
    assert_true(quota_metric.cpu_hard >= 0)
    assert_true(quota_metric.cpu_used >= 0)
    assert_true(quota_metric.memory_hard > 0)
    assert_true(quota_metric.memory_used >= 0)
    assert_true(quota_metric.pods_hard >= 0)
    assert_true(quota_metric.pods_used >= 0)
    assert_true(quota_metric.services_hard >= 0)
    assert_true(quota_metric.services_used >= 0)
  }
  
  // 测试Kubernetes网络策略遥测
  let network_policy_metrics = azimuth::KubernetesTelemetry::collect_network_policy_metrics(k8s_telemetry)
  
  assert_true(network_policy_metrics.policy_count >= 0)
  assert_true(network_policy_metrics.ingress_rules >= 0)
  assert_true(network_policy_metrics.egress_rules >= 0)
  assert_true(network_policy_metrics.denied_connections >= 0)
  assert_true(network_policy_metrics.allowed_connections >= 0)
  
  // 测试Kubernetes HPA（水平Pod自动扩缩容）遥测
  let hpa_metrics = azimuth::KubernetesTelemetry::collect_hpa_metrics(k8s_telemetry)
  
  assert_true(hpa_metrics.length() >= 1)
  
  for hpa_metric in hpa_metrics {
    assert_true(hpa_metric.min_replicas >= 1)
    assert_true(hpa_metric.max_replicas >= hpa_metric.min_replicas)
    assert_true(hpa_metric.current_replicas >= hpa_metric.min_replicas)
    assert_true(hpa_metric.current_replicas <= hpa_metric.max_replicas)
    assert_true(hpa_metric.target_cpu_utilization >= 0)
    assert_true(hpa_metric.current_cpu_utilization >= 0)
    assert_true(hpa_metric.last_scale_time > 0)
  }
  
  // 测试Kubernetes CSI（容器存储接口）遥测
  let csi_metrics = azimuth::KubernetesTelemetry::collect_csi_metrics(k8s_telemetry)
  
  assert_true(csi_metrics.volume_count >= 0)
  assert_true(csi_metrics.provisioned_volumes >= 0)
  assert_true(csi_metrics.attached_volumes >= 0)
  assert_true(csi_metrics.storage_operations >= 0)
  assert_true(csi_metrics.storage_errors >= 0)
}

pub test "微服务架构遥测测试" {
  let cloud_native_manager = azimuth::CloudNativeManager::new()
  
  // 创建微服务遥测管理器
  let microservice_telemetry = azimuth::CloudNativeManager::create_microservice_telemetry(cloud_native_manager)
  
  // 定义微服务拓扑
  let service_topology = {
    services: [
      {
        name: "api-gateway",
        version: "v1.2.0",
        instances: 3,
        ports: [80, 443],
        dependencies: ["auth-service", "user-service", "order-service"]
      },
      {
        name: "auth-service",
        version: "v2.1.0",
        instances: 2,
        ports: [8080],
        dependencies: ["user-database", "redis-cache"]
      },
      {
        name: "user-service",
        version: "v1.5.3",
        instances: 2,
        ports: [8080],
        dependencies: ["user-database", "notification-service"]
      },
      {
        name: "order-service",
        version: "v3.0.1",
        instances: 4,
        ports: [8080],
        dependencies: ["order-database", "payment-service", "inventory-service"]
      },
      {
        name: "payment-service",
        version: "v1.8.2",
        instances: 2,
        ports: [8080],
        dependencies: ["payment-database", "external-payment-gateway"]
      }
    ]
  }
  
  // 注册服务拓扑
  azimuth::MicroserviceTelemetry::register_service_topology(microservice_telemetry, service_topology)
  
  // 测试服务发现遥测
  let discovery_metrics = azimuth::MicroserviceTelemetry::collect_service_discovery_metrics(microservice_telemetry)
  
  assert_true(discovery_metrics.registered_services >= 5)
  assert_true(discovery_metrics.healthy_instances >= 10)
  assert_true(discovery_metrics.unhealthy_instances >= 0)
  assert_true(discovery_metrics.discovery_latency_ms >= 0)
  
  // 测试服务间通信遥测
  let communication_metrics = azimuth::MicroserviceTelemetry::collect_service_communication_metrics(microservice_telemetry)
  
  assert_true(communication_metrics.length() >= 5)  # 至少5个服务
  
  for comm_metric in communication_metrics {
    assert_true(comm_metric.request_count >= 0)
    assert_true(comm_metric.error_count >= 0)
    assert_true(comm_metric.latency_p50 >= 0)
    assert_true(comm_metric.latency_p95 >= 0)
    assert_true(comm_metric.latency_p99 >= 0)
    assert_true(comm_metric.throughput >= 0)
  }
  
  // 测试服务熔断器遥测
  let circuit_breaker_metrics = azimuth::MicroserviceTelemetry::collect_circuit_breaker_metrics(microservice_telemetry)
  
  assert_true(circuit_breaker_metrics.length() >= 3)
  
  for cb_metric in circuit_breaker_metrics {
    assert_true(cb_metric.state == "closed" || cb_metric.state == "open" || cb_metric.state == "half-open")
    assert_true(cb_metric.request_count >= 0)
    assert_true(cb_metric.failure_count >= 0)
    assert_true(cb_metric.failure_threshold >= 0)
    assert_true(cb_metric.recovery_timeout_ms >= 0)
  }
  
  // 测试服务重试遥测
  let retry_metrics = azimuth::MicroserviceTelemetry::collect_retry_metrics(microservice_telemetry)
  
  assert_true(retry_metrics.total_retries >= 0)
  assert_true(retry_metrics.successful_retries >= 0)
  assert_true(retry_metrics.failed_retries >= 0)
  assert_true(retry_metrics.max_retry_attempts >= 1)
  assert_true(retry_metrics.average_retry_delay_ms >= 0)
  
  // 测试服务限流遥测
  let rate_limiter_metrics = azimuth::MicroserviceTelemetry::collect_rate_limiter_metrics(microservice_telemetry)
  
  assert_true(rate_limiter_metrics.length() >= 3)
  
  for rl_metric in rate_limiter_metrics {
    assert_true(rl_metric.requests_total >= 0)
    assert_true(rl_metric.requests_allowed >= 0)
    assert_true(rl_metric.requests_denied >= 0)
    assert_true(rl_metric.rate_limit >= 0)
    assert_true(rl_metric.current_usage >= 0)
  }
  
  // 测试分布式配置遥测
  let config_metrics = azimuth::MicroserviceTelemetry::collect_config_metrics(microservice_telemetry)
  
  assert_true(config_metrics.config_changes >= 0)
  assert_true(config_metrics.config_rollbacks >= 0)
  assert_true(config_metrics.config_reload_success >= 0)
  assert_true(config_metrics.config_reload_failure >= 0)
  assert_true(config_metrics.last_config_change_timestamp >= 0)
  
  // 测试服务网格遥测
  let service_mesh_metrics = azimuth::MicroserviceTelemetry::collect_service_mesh_metrics(microservice_telemetry)
  
  assert_true(service_mesh_metrics.proxy_instances >= 0)
  assert_true(service_mesh_metrics.inbound_requests >= 0)
  assert_true(service_mesh_metrics.outbound_requests >= 0)
  assert_true(service_mesh_metrics.connection_count >= 0)
  assert_true(service_mesh_metrics.active_connections >= 0)
  assert_true(service_mesh_metrics.failed_connections >= 0)
  
  // 测试API网关遥测
  let gateway_metrics = azimuth::MicroserviceTelemetry::collect_api_gateway_metrics(microservice_telemetry)
  
  assert_true(gateway_metrics.total_requests >= 0)
  assert_true(gateway_metrics.successful_requests >= 0)
  assert_true(gateway_metrics.failed_requests >= 0)
  assert_true(gateway_metrics.average_request_time_ms >= 0)
  assert_true(gateway_metrics.p95_request_time_ms >= 0)
  assert_true(gateway_metrics.p99_request_time_ms >= 0)
  assert_true(gateway_metrics.active_connections >= 0)
  
  // 测试服务健康检查遥测
  let health_check_metrics = azimuth::MicroserviceTelemetry::collect_health_check_metrics(microservice_telemetry)
  
  assert_true(health_check_metrics.length() >= 5)
  
  for hc_metric in health_check_metrics {
    assert_true(hc_metric.checks_total >= 0)
    assert_true(hc_metric.checks_passed >= 0)
    assert_true(hc_metric.checks_failed >= 0)
    assert_true(hc_metric.average_response_time_ms >= 0)
    assert_true(hc_metric.last_check_timestamp >= 0)
  }
}

pub test "云原生可观测性集成测试" {
  let cloud_native_manager = azimuth::CloudNativeManager::new()
  
  // 创建可观测性集成管理器
  let observability_manager = azimuth::CloudNativeManager::create_observability_manager(cloud_native_manager)
  
  // 测试Prometheus集成
  let prometheus_config = {
    endpoint: "http://prometheus.monitoring.svc.cluster.local:9090",
    scrape_interval: "15s",
    metrics_path: "/metrics",
    query_timeout: "30s"
  }
  
  let prometheus_integration = azimuth::ObservabilityManager::create_prometheus_integration(observability_manager, prometheus_config)
  
  // 测试Prometheus查询
  let prometheus_queries = [
    "up",
    "rate(http_requests_total[5m])",
    "histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))",
    "process_cpu_seconds_total",
    "process_resident_memory_bytes"
  ]
  
  for query in prometheus_queries {
    let query_result = azimuth::PrometheusIntegration::query(prometheus_integration, query)
    assert_true(query_result.status == "success")
    assert_true(query_result.data.result_type == "vector" || query_result.data.result_type == "matrix")
  }
  
  // 测试Grafana集成
  let grafana_config = {
    endpoint: "http://grafana.monitoring.svc.cluster.local:3000",
    api_key: "eyJrIjOi...",
    default_dashboard: "cloud-native-overview"
  }
  
  let grafana_integration = azimuth::ObservabilityManager::create_grafana_integration(observability_manager, grafana_config)
  
  // 测试Grafana仪表板
  let dashboard_list = azimuth::GrafanaIntegration::list_dashboards(grafana_integration)
  assert_true(dashboard_list.length() >= 1)
  
  // 创建临时仪表板
  let temp_dashboard = {
    title: "Cloud Native Telemetry Test Dashboard",
    panels: [
      {
        title: "Request Rate",
        type: "graph",
        targets: [
          {
            expr: "rate(http_requests_total[5m])",
            legendFormat: "{{service}}"
          }
        ]
      },
      {
        title: "Response Time",
        type: "graph",
        targets: [
          {
            expr: "histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m]))",
            legendFormat: "95th percentile"
          }
        ]
      }
    ]
  }
  
  let created_dashboard = azimuth::GrafanaIntegration::create_dashboard(grafana_integration, temp_dashboard)
  assert_true(created_dashboard.uid != "")
  assert_eq(created_dashboard.title, "Cloud Native Telemetry Test Dashboard")
  
  // 测试Jaeger集成
  let jaeger_config = {
    endpoint: "http://jaeger.monitoring.svc.cluster.local:16686",
    query_endpoint: "http://jaeger.monitoring.svc.cluster.local:16686/api/traces",
    collector_endpoint: "http://jaeger-collector.monitoring.svc.cluster.local:14268/api/traces"
  }
  
  let jaeger_integration = azimuth::ObservabilityManager::create_jaeger_integration(observability_manager, jaeger_config)
  
  // 测试分布式追踪查询
  let trace_query = {
    service: "api-gateway",
    limit: 100,
    lookback: "1h"
  }
  
  let trace_results = azimuth::JaegerIntegration::find_traces(jaeger_integration, trace_query)
  assert_true(trace_results.length() >= 0)
  
  // 测试Elasticsearch集成
  let elasticsearch_config = {
    endpoints: ["http://elasticsearch.monitoring.svc.cluster.local:9200"],
    index_pattern: "otel-logs-*",
    username: "elastic",
    password: "changeme"
  }
  
  let elasticsearch_integration = azimuth::ObservabilityManager::create_elasticsearch_integration(observability_manager, elasticsearch_config)
  
  // 测试日志查询
  let log_query = {
    query: "service.name:api-gateway AND level:ERROR",
    size: 50,
    sort: "@timestamp:desc"
  }
  
  let log_results = azimuth::ElasticsearchIntegration::search_logs(elasticsearch_integration, log_query)
  assert_true(log_results.hits.total.value >= 0)
  
  // 测试Fluentd/Fluent Bit集成
  let fluentd_config = {
    endpoint: "http://fluentd.monitoring.svc.cluster.local:24224",
    tag_prefix: "otel.",
    buffer_size: 1048576
  }
  
  let fluentd_integration = azimuth::ObservabilityManager::create_fluentd_integration(observability_manager, fluentd_config)
  
  // 测试日志转发
  let test_logs = [
    {
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      level: "INFO",
      service: "api-gateway",
      message: "Request processed successfully",
      trace_id: "trace-123456",
      span_id: "span-789012"
    },
    {
      timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
      level: "ERROR",
      service: "auth-service",
      message: "Authentication failed",
      trace_id: "trace-345678",
      span_id: "span-901234"
    }
  ]
  
  for log in test_logs {
    let forward_result = azimuth::FluentdIntegration::forward_log(fluentd_integration, log)
    assert_true(forward_result.success)
  }
  
  // 测试OpenTelemetry Collector集成
  let otel_collector_config = {
    endpoint: "http://otel-collector.monitoring.svc.cluster.local:4317",
    protocol: "grpc",
    headers: [("x-otel-token", "collector-token")]
  }
  
  let otel_collector_integration = azimuth::ObservabilityManager::create_otel_collector_integration(observability_manager, otel_collector_config)
  
  // 测试遥测数据导出
  let telemetry_data = {
    traces: [
      {
        trace_id: "trace-123456789",
        spans: [
          {
            span_id: "span-111",
            parent_span_id: "",
            operation_name: "GET /api/users",
            service_name: "api-gateway",
            start_time: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
            end_time: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()) + 100000000L,
            status: "ok"
          }
        ]
      }
    ],
    metrics: [
      {
        name: "http_requests_total",
        value: 150.0,
        labels: [("service", "api-gateway"), ("method", "GET"), ("status", "200")]
      }
    ],
    logs: [
      {
        timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system()),
        severity: "INFO",
        body: "Request completed",
        attributes: [("service", "api-gateway"), ("trace_id", "trace-123456789")]
      }
    ]
  }
  
  let export_result = azimuth::OTelCollectorIntegration::export_telemetry(otel_collector_integration, telemetry_data)
  assert_true(export_result.success)
  assert_true(export_result.exported_traces >= 1)
  assert_true(export_result.exported_metrics >= 1)
  assert_true(export_result.exported_logs >= 1)
  
  // 测试可观测性数据关联
  let correlation_result = azimuth::ObservabilityManager::correlate_telemetry_data(observability_manager, {
    trace_id: "trace-123456789",
    time_range: { start: "now-1h", end: "now" },
    services: ["api-gateway", "auth-service", "user-service"]
  })
  
  assert_true(correlation_result.related_traces.length >= 1)
  assert_true(correlation_result.related_metrics.length >= 0)
  assert_true(correlation_result.related_logs.length >= 0)
  
  // 测试可观测性告警
  let alerting_config = {
    prometheus_rules: [
      {
        name: "HighErrorRate",
        expr: "rate(http_requests_total{status=~\"5..\"}[5m]) > 0.1",
        for: "5m",
        severity: "critical"
      },
      {
        name: "HighLatency",
        expr: "histogram_quantile(0.95, rate(http_request_duration_seconds_bucket[5m])) > 1",
        for: "10m",
        severity: "warning"
      }
    ],
    grafana_alerts: [
      {
        name: "ServiceDown",
        condition: "up == 0",
        severity: "critical"
      }
    ]
  }
  
  let alerting_integration = azimuth::ObservabilityManager::create_alerting_integration(observability_manager, alerting_config)
  
  // 测试告警规则
  let alert_rules = azimuth::AlertingIntegration::list_rules(alerting_integration)
  assert_true(alert_rules.length >= 2)
  
  // 测试告警状态
  let alert_status = azimuth::AlertingIntegration::get_alert_status(alerting_integration)
  assert_true(alert_status.active_alerts >= 0)
  assert_true(alert_status.firing_alerts >= 0)
  assert_true(alert_status.resolved_alerts >= 0)
  
  // 测试告警通知
  let notification_config = {
    channels: [
      {
        type: "slack",
        webhook_url: "https://hooks.slack.com/services/...",
        channel: "#alerts"
      },
      {
        type: "email",
        smtp_server: "smtp.company.com",
        recipients: ["ops@company.com"]
      }
    ]
  }
  
  azimuth::AlertingIntegration::configure_notifications(alerting_integration, notification_config)
  
  // 模拟告警触发
  let test_alert = {
    name: "TestAlert",
    severity: "warning",
    message: "This is a test alert",
    timestamp: azimuth::Clock::now_unix_nanos(azimuth::Clock::system())
  }
  
  let notification_result = azimuth::AlertingIntegration::send_notification(alerting_integration, test_alert)
  assert_true(notification_result.success)
}