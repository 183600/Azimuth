// Azimuth Service Mesh Tests
// This file contains test cases for service mesh telemetry and observability

// Test 1: Service Mesh Configuration and Topology
test "service mesh configuration and topology discovery" {
  // Define service mesh types
  enum MeshProvider {
    Istio
    Linkerd
    Consul
    AppMesh
    Octarine
  }
  
  enum ServiceState {
    Running
    Pending
    Failed
    Terminating
    Unknown
  }
  
  type Service = {
    name: String,
    namespace: String,
    version: String,
    labels: Array[(String, String)],
    state: ServiceState,
    endpoints: Array[String],
    port: Int
  }
  
  type ServiceMesh = {
    name: String,
    provider: MeshProvider,
    namespace: String,
    services: Array[Service],
    ingress_gateways: Array[Gateway],
    egress_gateways: Array[Gateway],
    mesh_policies: Array[MeshPolicy]
  }
  
  type Gateway = {
    name: String,
    type: String,  // "ingress", "egress"
    hosts: Array[String],
    ports: Array[(String, Int)],
    tls: Bool
  }
  
  type MeshPolicy = {
    name: String,
    type: String,  // "authentication", "authorization", "traffic-management"
    rules: Array[String],
    target_services: Array[String]
  }
  
  // Create service mesh
  let create_mesh = fn(
    name: String,
    provider: MeshProvider,
    namespace: String
  ) {
    {
      name,
      provider,
      namespace,
      services: [],
      ingress_gateways: [],
      egress_gateways: [],
      mesh_policies: []
    }
  }
  
  // Add service to mesh
  let add_service = fn(mesh: ServiceMesh, service: Service) {
    {
      name: mesh.name,
      provider: mesh.provider,
      namespace: mesh.namespace,
      services: mesh.services.push(service),
      ingress_gateways: mesh.ingress_gateways,
      egress_gateways: mesh.egress_gateways,
      mesh_policies: mesh.mesh_policies
    }
  }
  
  // Add gateway to mesh
  let add_ingress_gateway = fn(mesh: ServiceMesh, gateway: Gateway) {
    {
      name: mesh.name,
      provider: mesh.provider,
      namespace: mesh.namespace,
      services: mesh.services,
      ingress_gateways: mesh.ingress_gateways.push(gateway),
      egress_gateways: mesh.egress_gateways,
      mesh_policies: mesh.mesh_policies
    }
  }
  
  // Discover service dependencies
  let discover_dependencies = fn(services: Array[Service]) {
    let mut dependencies = []
    
    for service in services {
      // Simulate dependency discovery based on service labels
      for (key, value) in service.labels {
        if key == "depends_on" {
          let dep_services = value.split(",")
          for dep_service in dep_services {
            dependencies = dependencies.push((service.name, dep_service))
          }
        }
      }
    }
    
    dependencies
  }
  
  // Test service mesh creation
  let istio_mesh = create_mesh("azimuth-mesh", MeshProvider::Istio, "azimuth")
  
  assert_eq(istio_mesh.name, "azimuth-mesh")
  match istio_mesh.provider {
    MeshProvider::Istio => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(istio_mesh.namespace, "azimuth")
  assert_eq(istio_mesh.services.length(), 0)
  
  // Create test services
  let telemetry_service = {
    name: "azimuth-telemetry",
    namespace: "azimuth",
    version: "v1.2.3",
    labels: [
      ("app", "azimuth-telemetry"),
      ("version", "v1.2.3"),
      ("depends_on", "otel-collector,redis")
    ],
    state: ServiceState::Running,
    endpoints: ["10.1.1.10", "10.1.1.11", "10.1.1.12"],
    port: 8080
  }
  
  let collector_service = {
    name: "otel-collector",
    namespace: "azimuth",
    version: "v0.88.0",
    labels: [
      ("app", "otel-collector"),
      ("version", "v0.88.0"),
      ("depends_on", "jaeger,prometheus")
    ],
    state: ServiceState::Running,
    endpoints: ["10.1.1.20", "10.1.1.21"],
    port: 4317
  }
  
  let redis_service = {
    name: "redis",
    namespace: "azimuth",
    version: "v7.0.0",
    labels: [
      ("app", "redis"),
      ("version", "v7.0.0"),
      ("depends_on", "")
    ],
    state: ServiceState::Running,
    endpoints: ["10.1.1.30"],
    port: 6379
  }
  
  // Add services to mesh
  let mesh_with_telemetry = add_service(istio_mesh, telemetry_service)
  let mesh_with_collector = add_service(mesh_with_telemetry, collector_service)
  let mesh_with_all_services = add_service(mesh_with_collector, redis_service)
  
  assert_eq(mesh_with_all_services.services.length(), 3)
  
  // Add ingress gateway
  let ingress_gateway = {
    name: "azimuth-ingress",
    type: "ingress",
    hosts: ["telemetry.azimuth.example.com"],
    ports: [("http", 80), ("https", 443)],
    tls: true
  }
  
  let mesh_with_gateway = add_ingress_gateway(mesh_with_all_services, ingress_gateway)
  assert_eq(mesh_with_gateway.ingress_gateways.length(), 1)
  
  // Test dependency discovery
  let dependencies = discover_dependencies(mesh_with_gateway.services)
  assert_eq(dependencies.length(), 3)  // telemetry -> otel-collector, telemetry -> redis, otel-collector -> jaeger
  
  assert_eq(dependencies[0], ("azimuth-telemetry", "otel-collector"))
  assert_eq(dependencies[1], ("azimuth-telemetry", "redis"))
  assert_eq(dependencies[2], ("otel-collector", "jaeger"))
}

// Test 2: Service Mesh Traffic Management
test "service mesh traffic management and routing" {
  // Define traffic management types
  enum TrafficRuleType {
    VirtualService
    DestinationRule
    ServiceEntry
    Gateway
    EnvoyFilter
  }
  
  enum LoadBalancingPolicy {
    RoundRobin
    LeastConn
    Random
    Maglev
  }
  
  type TrafficRule = {
    name: String,
    rule_type: TrafficRuleType,
    hosts: Array[String],
    rules: Array[RoutingRule]
  }
  
  type RoutingRule = {
    match: Array[MatchCondition],
    route: Array[RouteDestination],
    timeout: Option[Int],
    retries: Option[RetryPolicy]
  }
  
  type MatchCondition = {
    key: String,
    value: String,
    type: String  // "exact", "prefix", "regex"
  }
  
  type RouteDestination = {
    host: String,
    subset: Option[String],
    weight: Int,
    port: Option[Int]
  }
  
  type RetryPolicy = {
    attempts: Int,
    per_try_timeout: Int,
    retry_on: String
  }
  
  type DestinationRule = {
    name: String,
    host: String,
    traffic_policy: TrafficPolicy,
    subsets: Array[Subset]
  }
  
  type TrafficPolicy = {
    load_balancer: Option[LoadBalancer],
    connection_pool: Option[ConnectionPool],
    outlier_detection: Option[OutlierDetection]
  }
  
  type LoadBalancer = {
    policy: LoadBalancingPolicy,
    consistent_hash: Option[ConsistentHash]
  }
  
  type ConsistentHash = {
    type: String,  // "header", "cookie", "source_ip"
    header_name: Option<String>,
    cookie_name: Option<String>
  }
  
  type ConnectionPool = {
    tcp: Option[TCPSettings],
    http: Option[HTTPSettings]
  }
  
  type TCPSettings = {
    max_connections: Int,
    connect_timeout: Int
  }
  
  type HTTPSettings = {
    http1_max_pending_requests: Int,
    http2_max_requests: Int,
    max_requests_per_connection: Int,
    max_retries: Int
  }
  
  type OutlierDetection = {
    consecutive_errors: Int,
    interval: Int,
    base_ejection_time: Int,
    max_ejection_percent: Int
  }
  
  type Subset = {
    name: String,
    labels: Array[(String, String)]
  }
  
  // Create virtual service
  let create_virtual_service = fn(
    name: String,
    hosts: Array[String],
    rules: Array[RoutingRule]
  ) {
    {
      name,
      rule_type: TrafficRuleType::VirtualService,
      hosts,
      rules
    }
  }
  
  // Create destination rule
  let create_destination_rule = fn(
    name: String,
    host: String,
    traffic_policy: TrafficPolicy,
    subsets: Array[Subset]
  ) {
    {
      name,
      rule_type: TrafficRuleType::DestinationRule,
      hosts: [host],
      rules: []
    }
  }
  
  // Test traffic routing
  let telemetry_routing_rules = [
    {
      match: [
        {
          key: "uri",
          value: "/api/v1/telemetry",
          type: "prefix"
        }
      ],
      route: [
        {
          host: "azimuth-telemetry",
          subset: Some("v1"),
          weight: 90,
          port: Some(8080)
        },
        {
          host: "azimuth-telemetry",
          subset: Some("v2"),
          weight: 10,
          port: Some(8080)
        }
      ],
      timeout: Some(30000),
      retries: Some({
        attempts: 3,
        per_try_timeout: 10000,
        retry_on: "5xx"
      })
    },
    {
      match: [
        {
          key: "uri",
          value: "/api/v2/telemetry",
          type: "prefix"
        }
      ],
      route: [
        {
          host: "azimuth-telemetry",
          subset: Some("v2"),
          weight: 100,
          port: Some(8080)
        }
      ],
      timeout: Some(30000),
      retries: None
    }
  ]
  
  let telemetry_virtual_service = create_virtual_service(
    "azimuth-telemetry-vs",
    ["azimuth-telemetry.azimuth.svc.cluster.local"],
    telemetry_routing_rules
  )
  
  assert_eq(telemetry_virtual_service.name, "azimuth-telemetry-vs")
  match telemetry_virtual_service.rule_type {
    TrafficRuleType::VirtualService => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(telemetry_virtual_service.hosts.length(), 1)
  assert_eq(telemetry_virtual_service.rules.length(), 2)
  
  // Test first routing rule
  let first_rule = telemetry_virtual_service.rules[0]
  assert_eq(first_rule.match.length(), 1)
  assert_eq(first_rule.match[0].key, "uri")
  assert_eq(first_rule.match[0].value, "/api/v1/telemetry")
  assert_eq(first_rule.match[0].type, "prefix")
  
  assert_eq(first_rule.route.length(), 2)
  assert_eq(first_rule.route[0].host, "azimuth-telemetry")
  assert_eq(first_rule.route[0].subset, Some("v1"))
  assert_eq(first_rule.route[0].weight, 90)
  assert_eq(first_rule.route[1].subset, Some("v2"))
  assert_eq(first_rule.route[1].weight, 10)
  
  match first_rule.timeout {
    Some(timeout) => assert_eq(timeout, 30000)
    None => assert_true(false)
  }
  
  match first_rule.retries {
    Some(retry_policy) => {
      assert_eq(retry_policy.attempts, 3)
      assert_eq(retry_policy.per_try_timeout, 10000)
      assert_eq(retry_policy.retry_on, "5xx")
    }
    None => assert_true(false)
  }
  
  // Test destination rule with load balancing
  let telemetry_traffic_policy = {
    load_balancer: Some({
      policy: LoadBalancingPolicy::LeastConn,
      consistent_hash: Some({
        type: "header",
        header_name: Some("x-user-id"),
        cookie_name: None
      })
    }),
    connection_pool: Some({
      tcp: Some({
        max_connections: 100,
        connect_timeout: 30
      }),
      http: Some({
        http1_max_pending_requests: 50,
        http2_max_requests: 1000,
        max_requests_per_connection: 10,
        max_retries: 3
      })
    }),
    outlier_detection: Some({
      consecutive_errors: 3,
      interval: 30000,
      base_ejection_time: 30000,
      max_ejection_percent: 50
    })
  }
  
  let telemetry_subsets = [
    {
      name: "v1",
      labels: [("version", "v1.2.3")]
    },
    {
      name: "v2",
      labels: [("version", "v2.0.0")]
    }
  ]
  
  let telemetry_destination_rule = create_destination_rule(
    "azimuth-telemetry-dr",
    "azimuth-telemetry.azimuth.svc.cluster.local",
    telemetry_traffic_policy,
    telemetry_subsets
  )
  
  assert_eq(telemetry_destination_rule.name, "azimuth-telemetry-dr")
  match telemetry_destination_rule.rule_type {
    TrafficRuleType::DestinationRule => assert_true(true)
    _ => assert_true(false)
  }
  
  match telemetry_destination_rule.traffic_policy.load_balancer {
    Some(lb) => {
      match lb.policy {
        LoadBalancingPolicy::LeastConn => assert_true(true)
        _ => assert_true(false)
      }
      match lb.consistent_hash {
        Some(hash) => {
          assert_eq(hash.type, "header")
          assert_eq(hash.header_name, Some("x-user-id"))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  assert_eq(telemetry_destination_rule.subsets.length(), 2)
  assert_eq(telemetry_destination_rule.subsets[0].name, "v1")
  assert_eq(telemetry_destination_rule.subsets[1].name, "v2")
}

// Test 3: Service Mesh Security Policies
test "service mesh security policies and mTLS" {
  // Define security policy types
  enum SecurityPolicyType {
    Authentication
    Authorization
    mTLS
    JWT
    OIDC
    RBAC
  }
  
  enum mTLSMode {
    DISABLE
    PERMISSIVE
    STRICT
  }
  
  type SecurityPolicy = {
    name: String,
    policy_type: SecurityPolicyType,
    namespace: String,
    rules: Array[SecurityRule]
  }
  
  type SecurityRule = {
    name: String,
    action: String,  // "allow", "deny"
    when: Array[SecurityCondition],
    to: Array[SecurityTarget],
    from: Array[SecurityTarget]
  }
  
  type SecurityCondition = {
    key: String,
    values: Array[String],
    operation: String  // "equals", "not_equals", "in", "not_in"
  }
  
  type SecurityTarget = {
    principals: Array[String],  // Service identities
    namespaces: Array[String],
    ip_blocks: Array[String]
  }
  
  type mTLSPolicy = {
    mode: mTLSMode,
    ca_certificates: Array[String],
    certificate_lifetime: Int,
    rotation_policy: String
  }
  
  type JWTPolicy = {
    issuer: String,
    audiences: Array[String],
    jwks_uri: String,
    from_headers: Array[String],
    from_params: Array[String]
  }
  
  // Create authentication policy
  let create_auth_policy = fn(
    name: String,
    namespace: String,
    rules: Array[SecurityRule]
  ) {
    {
      name,
      policy_type: SecurityPolicyType::Authentication,
      namespace,
      rules
    }
  }
  
  // Create mTLS policy
  let create_mtls_policy = fn(
    name: String,
    namespace: String,
    mode: mTLSMode
  ) {
    let mtls_rule = {
      name: "mtls-rule",
      action: "allow",
      when: [],
      to: [],
      from: []
    }
    
    {
      name,
      policy_type: SecurityPolicyType::mTLS,
      namespace,
      rules: [mtls_rule]
    }
  }
  
  // Create JWT policy
  let create_jwt_policy = fn(
    name: String,
    namespace: String,
    jwt_config: JWTPolicy
  ) {
    let jwt_rule = {
      name: "jwt-rule",
      action: "allow",
      when: [],
      to: [],
      from: []
    }
    
    {
      name,
      policy_type: SecurityPolicyType::JWT,
      namespace,
      rules: [jwt_rule]
    }
  }
  
  // Test authentication policy
  let telemetry_auth_rules = [
    {
      name: "allow-frontend",
      action: "allow",
      when: [
        {
          key: "source.namespace",
          values: ["frontend"],
          operation: "equals"
        }
      ],
      to: [
        {
          principals: ["azimuth-telemetry"],
          namespaces: ["azimuth"],
          ip_blocks: []
        }
      ],
      from: []
    },
    {
      name: "deny-unauthenticated",
      action: "deny",
      when: [
        {
          key: "source.principal",
          values: [],
          operation: "equals"
        }
      ],
      to: [],
      from: []
    }
  ]
  
  let telemetry_auth_policy = create_auth_policy(
    "azimuth-telemetry-auth",
    "azimuth",
    telemetry_auth_rules
  )
  
  assert_eq(telemetry_auth_policy.name, "azimuth-telemetry-auth")
  match telemetry_auth_policy.policy_type {
    SecurityPolicyType::Authentication => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(telemetry_auth_policy.namespace, "azimuth")
  assert_eq(telemetry_auth_policy.rules.length(), 2)
  
  let first_rule = telemetry_auth_policy.rules[0]
  assert_eq(first_rule.name, "allow-frontend")
  assert_eq(first_rule.action, "allow")
  assert_eq(first_rule.when.length(), 1)
  assert_eq(first_rule.when[0].key, "source.namespace")
  assert_eq(first_rule.when[0].values[0], "frontend")
  assert_eq(first_rule.when[0].operation, "equals")
  
  assert_eq(first_rule.to.length(), 1)
  assert_eq(first_rule.to[0].principals.length(), 1)
  assert_eq(first_rule.to[0].principals[0], "azimuth-telemetry")
  
  // Test mTLS policy
  let telemetry_mtls_policy = create_mtls_policy(
    "azimuth-telemetry-mtls",
    "azimuth",
    mTLSMode::STRICT
  )
  
  assert_eq(telemetry_mtls_policy.name, "azimuth-telemetry-mtls")
  match telemetry_mtls_policy.policy_type {
    SecurityPolicyType::mTLS => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(telemetry_mtls_policy.namespace, "azimuth")
  
  let mtls_rule = telemetry_mtls_policy.rules[0]
  assert_eq(mtls_rule.name, "mtls-rule")
  assert_eq(mtls_rule.action, "allow")
  
  // Test JWT policy
  let jwt_config = {
    issuer: "https://auth.example.com",
    audiences: ["azimuth-telemetry"],
    jwks_uri: "https://auth.example.com/.well-known/jwks.json",
    from_headers: ["authorization"],
    from_params: ["access_token"]
  }
  
  let telemetry_jwt_policy = create_jwt_policy(
    "azimuth-telemetry-jwt",
    "azimuth",
    jwt_config
  )
  
  assert_eq(telemetry_jwt_policy.name, "azimuth-telemetry-jwt")
  match telemetry_jwt_policy.policy_type {
    SecurityPolicyType::JWT => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(telemetry_jwt_policy.namespace, "azimuth")
}

// Test 4: Service Mesh Observability and Telemetry
test "service mesh observability and telemetry collection" {
  // Define telemetry types
  enum TelemetryType {
    Metrics
    Logs
    Traces
    AccessLogs
  }
  
  enum TelemetrySource {
    Envoy
    Application
    ControlPlane
  }
  
  type TelemetryConfig = {
    name: String,
    telemetry_type: TelemetryType,
    source: TelemetrySource,
    enabled: Bool,
    providers: Array[TelemetryProvider]
  }
  
  type TelemetryProvider = {
    name: String,
    type: String,  // "prometheus", "jaeger", "zipkin", "fluentd"
    endpoint: String,
    credentials: Option[String]
  }
  
  type MeshTelemetry = {
    mesh_name: String,
    configs: Array[TelemetryConfig],
    collected_data: Array[TelemetryData]
  }
  
  type TelemetryData = {
    timestamp: Int,
    source_service: String,
    destination_service: Option<String>,
    data_type: TelemetryType,
    data: String,  // JSON or formatted string
    labels: Array[(String, String)]
  }
  
  // Create telemetry configuration
  let create_telemetry_config = fn(
    name: String,
    telemetry_type: TelemetryType,
    source: TelemetrySource,
    providers: Array[TelemetryProvider]
  ) {
    {
      name,
      telemetry_type,
      source,
      enabled: true,
      providers
    }
  }
  
  // Add telemetry data
  let add_telemetry_data = fn(
    mesh_telemetry: MeshTelemetry,
    data: TelemetryData
  ) {
    {
      mesh_name: mesh_telemetry.mesh_name,
      configs: mesh_telemetry.configs,
      collected_data: mesh_telemetry.collected_data.push(data)
    }
  }
  
  // Filter telemetry data
  let filter_telemetry_data = fn(
    data: Array[TelemetryData],
    filters: Array[(String, String)]
  ) {
    let mut filtered_data = []
    
    for item in data {
      let mut matches = true
      
      for (filter_key, filter_value) in filters {
        if filter_key == "source_service" && item.source_service != filter_value {
          matches = false
        } else if filter_key == "data_type" {
          match item.data_type {
            TelemetryType::Metrics => {
              if filter_value != "metrics" { matches = false }
            }
            TelemetryType::Logs => {
              if filter_value != "logs" { matches = false }
            }
            TelemetryType::Traces => {
              if filter_value != "traces" { matches = false }
            }
            TelemetryType::AccessLogs => {
              if filter_value != "access_logs" { matches = false }
            }
          }
        }
      }
      
      if matches {
        filtered_data = filtered_data.push(item)
      }
    }
    
    filtered_data
  }
  
  // Test telemetry configuration
  let prometheus_provider = {
    name: "prometheus",
    type: "prometheus",
    endpoint: "http://prometheus.azimuth.svc.cluster.local:9090",
    credentials: None
  }
  
  let jaeger_provider = {
    name: "jaeger",
    type: "jaeger",
    endpoint: "http://jaeger.azimuth.svc.cluster.local:14268",
    credentials: None
  }
  
  let fluentd_provider = {
    name: "fluentd",
    type: "fluentd",
    endpoint: "http://fluentd.azimuth.svc.cluster.local:24224",
    credentials: None
  }
  
  let envoy_metrics_config = create_telemetry_config(
    "envoy-metrics",
    TelemetryType::Metrics,
    TelemetrySource::Envoy,
    [prometheus_provider]
  )
  
  let envoy_traces_config = create_telemetry_config(
    "envoy-traces",
    TelemetryType::Traces,
    TelemetrySource::Envoy,
    [jaeger_provider]
  )
  
  let envoy_access_logs_config = create_telemetry_config(
    "envoy-access-logs",
    TelemetryType::AccessLogs,
    TelemetrySource::Envoy,
    [fluentd_provider]
  )
  
  assert_eq(envoy_metrics_config.name, "envoy-metrics")
  match envoy_metrics_config.telemetry_type {
    TelemetryType::Metrics => assert_true(true)
    _ => assert_true(false)
  }
  match envoy_metrics_config.source {
    TelemetrySource::Envoy => assert_true(true)
    _ => assert_true(false)
  }
  assert_true(envoy_metrics_config.enabled)
  assert_eq(envoy_metrics_config.providers.length(), 1)
  
  // Create mesh telemetry
  let mesh_telemetry = {
    mesh_name: "azimuth-mesh",
    configs: [
      envoy_metrics_config,
      envoy_traces_config,
      envoy_access_logs_config
    ],
    collected_data: []
  }
  
  // Add test telemetry data
  let metric_data = {
    timestamp: 1000,
    source_service: "azimuth-telemetry",
    destination_service: Some("otel-collector"),
    data_type: TelemetryType::Metrics,
    data: "{\"request_count\": 1250, \"request_duration\": 75.5, \"response_size\": 1024000}",
    labels: [
      ("source_workload", "azimuth-telemetry"),
      ("destination_workload", "otel-collector"),
      ("request_protocol", "http")
    ]
  }
  
  let trace_data = {
    timestamp: 1005,
    source_service: "azimuth-telemetry",
    destination_service: Some("otel-collector"),
    data_type: TelemetryType::Traces,
    data: "{\"trace_id\": \"abc123\", \"span_id\": \"def456\", \"operation\": \"POST /api/v1/telemetry\"}",
    labels: [
      ("source_workload", "azimuth-telemetry"),
      ("destination_workload", "otel-collector"),
      ("http.method", "POST"),
      ("http.url", "/api/v1/telemetry")
    ]
  }
  
  let access_log_data = {
    timestamp: 1010,
    source_service: "istio-ingressgateway",
    destination_service: Some("azimuth-telemetry"),
    data_type: TelemetryType::AccessLogs,
    data: "{\"method\": \"GET\", \"path\": \"/health\", \"status\": \"200\", \"user_agent\": \"curl/7.68.0\"}",
    labels: [
      ("source_workload", "istio-ingressgateway"),
      ("destination_workload", "azimuth-telemetry"),
      ("response_code", "200")
    ]
  }
  
  let telemetry_with_metrics = add_telemetry_data(mesh_telemetry, metric_data)
  let telemetry_with_traces = add_telemetry_data(telemetry_with_metrics, trace_data)
  let telemetry_with_all = add_telemetry_data(telemetry_with_traces, access_log_data)
  
  assert_eq(telemetry_with_all.collected_data.length(), 3)
  
  // Test filtering
  let metrics_only = filter_telemetry_data(telemetry_with_all.collected_data, [("data_type", "metrics")])
  assert_eq(metrics_only.length(), 1)
  assert_eq(metrics_only[0].source_service, "azimuth-telemetry")
  match metrics_only[0].data_type {
    TelemetryType::Metrics => assert_true(true)
    _ => assert_true(false)
  }
  
  let telemetry_service_data = filter_telemetry_data(
    telemetry_with_all.collected_data,
    [("source_service", "azimuth-telemetry")]
  )
  assert_eq(telemetry_service_data.length(), 2)
  
  let ingress_data = filter_telemetry_data(
    telemetry_with_all.collected_data,
    [("source_service", "istio-ingressgateway")]
  )
  assert_eq(ingress_data.length(), 1)
  assert_eq(ingress_data[0].source_service, "istio-ingressgateway")
}

// Test 5: Service Mesh Performance and Reliability
test "service mesh performance and reliability monitoring" {
  // Define performance metrics
  type PerformanceMetrics = {
    service_name: String,
    request_count: Int,
    error_count: Int,
    latency_p50: Float,
    latency_p90: Float,
    latency_p99: Float,
    throughput: Float,
    timestamp: Int
  }
  
  type ReliabilityMetrics = {
    service_name: String,
    availability: Float,  // percentage
    mttr: Int,            // mean time to repair in minutes
    mtbf: Int,            // mean time between failures in hours
    incident_count: Int,
    recovery_time: Int,   // average recovery time in minutes
    timestamp: Int
  }
  
  type HealthStatus = {
    service_name: String,
    status: String,       // "healthy", "degraded", "unhealthy"
    last_check: Int,
    consecutive_failures: Int,
    uptime_percentage: Float
  }
  
  type MeshPerformance = {
    mesh_name: String,
    performance_metrics: Array[PerformanceMetrics],
    reliability_metrics: Array[ReliabilityMetrics],
    health_status: Array[HealthStatus],
    sla_compliance: Array[SLACompliance]
  }
  
  type SLACompliance = {
    service_name: String,
    sla_name: String,
    target: Float,
    actual: Float,
    compliant: Bool,
    period: String  // "daily", "weekly", "monthly"
  }
  
  // Calculate error rate
  let calculate_error_rate = fn(metrics: PerformanceMetrics) {
    if metrics.request_count > 0 {
      Int::to_float(metrics.error_count) / Int::to_float(metrics.request_count) * 100.0
    } else {
      0.0
    }
  }
  
  // Check SLA compliance
  let check_sla_compliance = fn(
    service_name: String,
    sla_name: String,
    target: Float,
    actual: Float,
    period: String
  ) {
    {
      service_name,
      sla_name,
      target,
      actual,
      compliant: actual <= target,
      period
    }
  }
  
  // Update health status
  let update_health_status = fn(
    current_status: HealthStatus,
    is_healthy: Bool,
    current_time: Int
  ) {
    if is_healthy {
      {
        service_name: current_status.service_name,
        status: "healthy",
        last_check: current_time,
        consecutive_failures: 0,
        uptime_percentage: current_status.uptime_percentage
      }
    } else {
      let new_failures = current_status.consecutive_failures + 1
      let new_status = if new_failures >= 3 { "unhealthy" } else { "degraded" }
      
      {
        service_name: current_status.service_name,
        status: new_status,
        last_check: current_time,
        consecutive_failures: new_failures,
        uptime_percentage: current_status.uptime_percentage * 0.99  // Decrease uptime slightly
      }
    }
  }
  
  // Test performance metrics
  let telemetry_performance = {
    service_name: "azimuth-telemetry",
    request_count: 10000,
    error_count: 50,
    latency_p50: 75.5,
    latency_p90: 150.2,
    latency_p99: 300.8,
    throughput: 250.5,
    timestamp: 10000
  }
  
  let collector_performance = {
    service_name: "otel-collector",
    request_count: 15000,
    error_count: 75,
    latency_p50: 50.2,
    latency_p90: 100.5,
    latency_p99: 200.3,
    throughput: 375.8,
    timestamp: 10000
  }
  
  // Calculate error rates
  let telemetry_error_rate = calculate_error_rate(telemetry_performance)
  let collector_error_rate = calculate_error_rate(collector_performance)
  
  assert_true(telemetry_error_rate > 0.49 && telemetry_error_rate < 0.51)  // 50/10000 * 100
  assert_true(collector_error_rate > 0.49 && collector_error_rate < 0.51)  // 75/15000 * 100
  
  // Test reliability metrics
  let telemetry_reliability = {
    service_name: "azimuth-telemetry",
    availability: 99.9,
    mttr: 5,
    mtbf: 720,  // 30 days
    incident_count: 2,
    recovery_time: 8,
    timestamp: 10000
  }
  
  let collector_reliability = {
    service_name: "otel-collector",
    availability: 99.95,
    mttr: 3,
    mtbf: 1440,  // 60 days
    incident_count: 1,
    recovery_time: 5,
    timestamp: 10000
  }
  
  // Test health status
  let telemetry_health = {
    service_name: "azimuth-telemetry",
    status: "healthy",
    last_check: 10000,
    consecutive_failures: 0,
    uptime_percentage: 99.9
  }
  
  // Test health status updates
  let telemetry_health_after_failure = update_health_status(telemetry_health, false, 10005)
  assert_eq(telemetry_health_after_failure.status, "degraded")
  assert_eq(telemetry_health_after_failure.consecutive_failures, 1)
  
  let telemetry_health_after_multiple_failures = update_health_status(
    telemetry_health_after_failure,
    false,
    10010
  )
  assert_eq(telemetry_health_after_multiple_failures.status, "degraded")
  assert_eq(telemetry_health_after_multiple_failures.consecutive_failures, 2)
  
  let telemetry_health_unhealthy = update_health_status(
    telemetry_health_after_multiple_failures,
    false,
    10015
  )
  assert_eq(telemetry_health_unhealthy.status, "unhealthy")
  assert_eq(telemetry_health_unhealthy.consecutive_failures, 3)
  
  let telemetry_health_recovered = update_health_status(
    telemetry_health_unhealthy,
    true,
    10020
  )
  assert_eq(telemetry_health_recovered.status, "healthy")
  assert_eq(telemetry_health_recovered.consecutive_failures, 0)
  
  // Test SLA compliance
  let latency_sla = check_sla_compliance(
    "azimuth-telemetry",
    "p99_latency",
    250.0,  // target: 250ms
    telemetry_performance.latency_p99,  // actual: 300.8ms
    "monthly"
  )
  
  assert_false(latency_sla.compliant)
  assert_eq(latency_sla.service_name, "azimuth-telemetry")
  assert_eq(latency_sla.sla_name, "p99_latency")
  assert_eq(latency_sla.target, 250.0)
  assert_eq(latency_sla.actual, 300.8)
  
  let availability_sla = check_sla_compliance(
    "azimuth-telemetry",
    "availability",
    99.5,   // target: 99.5%
    telemetry_reliability.availability,  // actual: 99.9%
    "monthly"
  )
  
  assert_true(availability_sla.compliant)
  assert_eq(availability_sla.service_name, "azimuth-telemetry")
  assert_eq(availability_sla.sla_name, "availability")
  assert_eq(availability_sla.target, 99.5)
  assert_eq(availability_sla.actual, 99.9)
  
  // Create mesh performance summary
  let mesh_performance = {
    mesh_name: "azimuth-mesh",
    performance_metrics: [telemetry_performance, collector_performance],
    reliability_metrics: [telemetry_reliability, collector_reliability],
    health_status: [telemetry_health_recovered],
    sla_compliance: [latency_sla, availability_sla]
  }
  
  assert_eq(mesh_performance.mesh_name, "azimuth-mesh")
  assert_eq(mesh_performance.performance_metrics.length(), 2)
  assert_eq(mesh_performance.reliability_metrics.length(), 2)
  assert_eq(mesh_performance.health_status.length(), 1)
  assert_eq(mesh_performance.sla_compliance.length(), 2)
}