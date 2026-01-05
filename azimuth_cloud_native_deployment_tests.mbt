// Azimuth 云原生部署测试用例
// 专注于遥测系统在Kubernetes和容器化环境中的部署、扩展和管理

// 测试1: Kubernetes资源管理
test "Kubernetes资源管理和部署" {
  // 定义Kubernetes资源类型
  enum K8sResourceType {
    Pod
    Deployment
    Service
    ConfigMap
    Secret
    Ingress
    StatefulSet
    DaemonSet
  }
  
  // 定义资源状态
  enum ResourceStatus {
    Pending
    Running
    Succeeded
    Failed
    Unknown
  }
  
  // 定义Kubernetes资源
  type K8sResource = {
    name: String,
    namespace: String,
    resource_type: K8sResourceType,
    status: ResourceStatus,
    labels: Array[(String, String)],
    annotations: Array[(String, String)],
    created_at: Int,
    spec: Map[String, String]  // 简化的资源规格
  }
  
  // 定义Pod规格
  type PodSpec = {
    containers: Array[ContainerSpec],
    restart_policy: String,
    node_selector: Map[String, String],
    tolerations: Array[Toleration]
  }
  
  // 定义容器规格
  type ContainerSpec = {
    name: String,
    image: String,
    ports: Array[ContainerPort],
    env_vars: Array[EnvVar],
    resources: ResourceRequirements,
    liveness_probe: Option[Probe],
    readiness_probe: Option[Probe]
  }
  
  // 定义容器端口
  type ContainerPort = {
    name: String,
    container_port: Int,
    protocol: String
  }
  
  // 定义环境变量
  type EnvVar = {
    name: String,
    value: Option[String],
    value_from: Option[EnvVarSource]
  }
  
  // 定义环境变量来源
  type EnvVarSource = {
    config_map_key_ref: Option[ConfigMapKeyRef],
    secret_key_ref: Option[SecretKeyRef]
  }
  
  // 定义ConfigMap引用
  type ConfigMapKeyRef = {
    name: String,
    key: String
  }
  
  // 定义Secret引用
  type SecretKeyRef = {
    name: String,
    key: String
  }
  
  // 定义资源需求
  type ResourceRequirements = {
    requests: Map[String, String],
    limits: Map[String, String]
  }
  
  // 定义探针
  type Probe = {
    http_get: Option[HTTPGetAction],
    tcp_socket: Option[TCPSocketAction],
    exec: Option[ExecAction],
    initial_delay_seconds: Int,
    period_seconds: Int,
    timeout_seconds: Int,
    failure_threshold: Int
  }
  
  // 定义HTTP操作
  type HTTPGetAction = {
    path: String,
    port: Int,
    scheme: String
  }
  
  // 定义TCP操作
  type TCPSocketAction = {
    port: Int
  }
  
  // 定义执行操作
  type ExecAction = {
    command: Array[String]
  }
  
  // 定义容忍度
  type Toleration = {
    key: String,
    operator: String,
    value: String,
    effect: String
  }
  
  // 创建Kubernetes资源
  let create_k8s_resource = fn(name: String, namespace: String, resource_type: K8sResourceType) {
    {
      name: name,
      namespace: namespace,
      resource_type: resource_type,
      status: ResourceStatus::Pending,
      labels: [
        ("app", "azimuth"),
        ("version", "1.0.0")
      ],
      annotations: [
        ("created-by", "azimuth-test-suite"),
        ("description", "Azimuth telemetry component")
      ],
      created_at: Time::now(),
      spec: Map::empty()
    }
  }
  
  // 创建Pod规格
  let create_pod_spec = fn(containers: Array[ContainerSpec]) {
    {
      containers: containers,
      restart_policy: "Always",
      node_selector: Map::from_array([("node-type", "telemetry")]),
      tolerations: [
        {
          key: "telemetry",
          operator: "Equal",
          value: "true",
          effect: "NoSchedule"
        }
      ]
    }
  }
  
  // 创建容器规格
  let create_container_spec = fn(name: String, image: String, ports: Array[ContainerPort]) {
    {
      name: name,
      image: image,
      ports: ports,
      env_vars: [
        {
          name: "LOG_LEVEL",
          value: Some("info"),
          value_from: None
        },
        {
          name: "TELEMETRY_ENDPOINT",
          value: None,
          value_from: Some({
            config_map_key_ref: Some({
              name: "azimuth-config",
              key: "telemetry-endpoint"
            }),
            secret_key_ref: None
          })
        }
      ],
      resources: {
        requests: Map::from_array([
          ("cpu", "100m"),
          ("memory", "128Mi")
        ]),
        limits: Map::from_array([
          ("cpu", "500m"),
          ("memory", "512Mi")
        ])
      },
      liveness_probe: Some({
        http_get: Some({
          path: "/health",
          port: 8080,
          scheme: "HTTP"
        }),
        tcp_socket: None,
        exec: None,
        initial_delay_seconds: 30,
        period_seconds: 10,
        timeout_seconds: 5,
        failure_threshold: 3
      }),
      readiness_probe: Some({
        http_get: Some({
          path: "/ready",
          port: 8080,
          scheme: "HTTP"
        }),
        tcp_socket: None,
        exec: None,
        initial_delay_seconds: 5,
        period_seconds: 5,
        timeout_seconds: 3,
        failure_threshold: 3
      })
    }
  }
  
  // 创建Deployment资源
  let create_deployment = fn(name: String, namespace: String, replicas: Int, pod_spec: PodSpec) {
    let deployment = create_k8s_resource(name, namespace, K8sResourceType::Deployment)
    let deployment_spec = Map::from_array([
      ("replicas", replicas.to_string()),
      ("selector", "app=azimuth"),
      ("strategy", "RollingUpdate"),
      ("maxUnavailable", "25%"),
      ("maxSurge", "25%")
    ])
    
    { deployment | spec: deployment_spec }
  }
  
  // 创建Service资源
  let create_service = fn(name: String, namespace: String, service_type: String, ports: Array[ContainerPort]) {
    let service = create_k8s_resource(name, namespace, K8sResourceType::Service)
    let service_spec = Map::from_array([
      ("type", service_type),
      ("selector", "app=azimuth"),
      ("sessionAffinity", "None"),
      ("ports", ports.map(fn(p) { p.container_port.to_string() }).join(","))
    ])
    
    { service | spec: service_spec }
  }
  
  // 创建ConfigMap资源
  let create_config_map = fn(name: String, namespace: String, data: Map[String, String]) {
    let config_map = create_k8s_resource(name, namespace, K8sResourceType::ConfigMap)
    let config_spec = Map::from_array([
      ("data", data.keys().join(","))
    ])
    
    { config_map | spec: config_spec }
  }
  
  // 模拟资源状态更新
  let update_resource_status = fn(resource: K8sResource, new_status: ResourceStatus) {
    { resource | status: new_status }
  }
  
  // 检查资源就绪状态
  let is_resource_ready = fn(resource: K8sResource) {
    match resource.status {
      ResourceStatus::Running => true
      ResourceStatus::Succeeded => true
      _ => false
    }
  }
  
  // 创建测试资源
  let telemetry_ports = [
    { name: "http", container_port: 8080, protocol: "TCP" },
    { name: "metrics", container_port: 9090, protocol: "TCP" }
  ]
  
  let telemetry_container = create_container_spec("azimuth-telemetry", "azimuth/telemetry:1.0.0", telemetry_ports)
  let telemetry_pod_spec = create_pod_spec([telemetry_container])
  
  let telemetry_deployment = create_deployment("azimuth-telemetry", "azimuth-system", 3, telemetry_pod_spec)
  let telemetry_service = create_service("azimuth-telemetry", "azimuth-system", "ClusterIP", telemetry_ports)
  
  let config_data = Map::from_array([
    ("telemetry-endpoint", "http://collector:14268/api/traces"),
    ("log-level", "info"),
    ("sampling-rate", "0.1")
  ])
  
  let telemetry_config_map = create_config_map("azimuth-config", "azimuth-system", config_data)
  
  // 验证资源创建
  assert_eq(telemetry_deployment.name, "azimuth-telemetry")
  assert_eq(telemetry_deployment.namespace, "azimuth-system")
  assert_eq(telemetry_deployment.resource_type, K8sResourceType::Deployment)
  assert_eq(telemetry_deployment.status, ResourceStatus::Pending)
  
  assert_eq(telemetry_service.name, "azimuth-telemetry")
  assert_eq(telemetry_service.resource_type, K8sResourceType::Service)
  
  assert_eq(telemetry_config_map.name, "azimuth-config")
  assert_eq(telemetry_config_map.resource_type, K8sResourceType::ConfigMap)
  
  // 验证容器规格
  assert_eq(telemetry_container.name, "azimuth-telemetry")
  assert_eq(telemetry_container.image, "azimuth/telemetry:1.0.0")
  assert_eq(telemetry_container.ports.length(), 2)
  assert_eq(telemetry_container.env_vars.length(), 2)
  
  // 验证资源需求
  assert_eq(telemetry_container.resources.requests.get("cpu"), Some("100m"))
  assert_eq(telemetry_container.resources.requests.get("memory"), Some("128Mi"))
  assert_eq(telemetry_container.resources.limits.get("cpu"), Some("500m"))
  assert_eq(telemetry_container.resources.limits.get("memory"), Some("512Mi"))
  
  // 验证探针配置
  match telemetry_container.liveness_probe {
    Some(probe) => {
      assert_eq(probe.initial_delay_seconds, 30)
      assert_eq(probe.period_seconds, 10)
      assert_eq(probe.failure_threshold, 3)
      match probe.http_get {
        Some(action) => {
          assert_eq(action.path, "/health")
          assert_eq(action.port, 8080)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 模拟部署过程
  let running_deployment = update_resource_status(telemetry_deployment, ResourceStatus::Running)
  let running_service = update_resource_status(telemetry_service, ResourceStatus::Running)
  let running_config_map = update_resource_status(telemetry_config_map, ResourceStatus::Running)
  
  // 验证资源就绪状态
  assert_true(is_resource_ready(running_deployment))
  assert_true(is_resource_ready(running_service))
  assert_true(is_resource_ready(running_config_map))
  
  // 验证标签和注解
  assert_true(running_deployment.labels.contains(("app", "azimuth")))
  assert_true(running_deployment.labels.contains(("version", "1.0.0")))
  assert_true(running_deployment.annotations.contains(("created-by", "azimuth-test-suite")))
}

// 测试2: 服务发现和负载均衡
test "服务发现和负载均衡配置" {
  // 定义服务类型
  enum ServiceType {
    ClusterIP
    NodePort
    LoadBalancer
    ExternalName
  }
  
  // 定义负载均衡策略
  enum LoadBalancingPolicy {
    RoundRobin
    LeastConnection
    Random
    IPHash
    ConsistentHash
  }
  
  // 定义服务端点
  type ServiceEndpoint = {
    ip: String,
    port: Int,
    ready: Bool,
    metadata: Map[String, String]
  }
  
  // 定义服务发现
  type ServiceDiscovery = {
    service_name: String,
    namespace: String,
    service_type: ServiceType,
    cluster_ip: String,
    external_ips: Array[String],
    ports: Array[ServicePort],
    selector: Map[String, String],
    endpoints: Array[ServiceEndpoint],
    load_balancing_policy: LoadBalancingPolicy
  }
  
  // 定义服务端口
  type ServicePort = {
    name: String,
    protocol: String,
    port: Int,
    target_port: Int,
    node_port: Option[Int>
  }
  
  // 定义In规则
  type IngressRule = {
    host: String,
    paths: Array[IngressPath]
  }
  
  // 定义In路径
  type IngressPath = {
    path: String,
    path_type: String,
    backend: IngressBackend
  }
  
  // 定义In后端
  type IngressBackend = {
    service_name: String,
    service_port: Int
  }
  
  // 定义Ingress
  type Ingress = {
    name: String,
    namespace: String,
    rules: Array[IngressRule],
    tls: Array[IngressTLS],
    annotations: Map[String, String]
  }
  
  // 定义Ingress TLS
  type IngressTLS = {
    hosts: Array[String],
    secret_name: String
  }
  
  // 创建服务发现
  let create_service_discovery = fn(name: String, namespace: String, service_type: ServiceType, policy: LoadBalancingPolicy) {
    {
      service_name: name,
      namespace: namespace,
      service_type: service_type,
      cluster_ip: "10.0.0." + (Time::now() % 255).to_string(),
      external_ips: [],
      ports: [],
      selector: Map::from_array([("app", "azimuth")]),
      endpoints: [],
      load_balancing_policy: policy
    }
  }
  
  // 添加服务端口
  let add_service_port = fn(service: ServiceDiscovery, name: String, protocol: String, port: Int, target_port: Int) {
    let new_port = {
      name: name,
      protocol: protocol,
      port: port,
      target_port: target_port,
      node_port: None
    }
    
    { service | ports: service.ports.push(new_port) }
  }
  
  // 添加服务端点
  let add_service_endpoint = fn(service: ServiceDiscovery, ip: String, port: Int, ready: Bool) {
    let endpoint = {
      ip: ip,
      port: port,
      ready: ready,
      metadata: Map::from_array([
        ("last-seen", Time::now().to_string()),
        ("zone", "us-west-2a")
      ])
    }
    
    { service | endpoints: service.endpoints.push(endpoint) }
  }
  
  // 负载均衡选择端点
  let select_endpoint = fn(service: ServiceDiscovery) {
    let ready_endpoints = service.endpoints.filter(fn(ep) { ep.ready })
    
    if ready_endpoints.length() == 0 {
      return None
    }
    
    match service.load_balancing_policy {
      LoadBalancingPolicy::RoundRobin => {
        let index = Time::now() % ready_endpoints.length()
        Some(ready_endpoints[index])
      }
      
      LoadBalancingPolicy::LeastConnection => {
        // 简化：选择第一个就绪端点
        Some(ready_endpoints[0])
      }
      
      LoadBalancingPolicy::Random => {
        let index = Math::random() * ready_endpoints.length() as Int
        Some(ready_endpoints[index])
      }
      
      LoadBalancingPolicy::IPHash => {
        // 简化：基于客户端IP哈希选择
        let client_ip = "192.168.1.100"
        let hash = client_ip.split(".").reduce(fn(acc, part) { acc + part.to_int() }, 0)
        let index = hash % ready_endpoints.length()
        Some(ready_endpoints[index])
      }
      
      LoadBalancingPolicy::ConsistentHash => {
        // 简化：基于服务名称哈希选择
        let hash = service.service_name.chars().reduce(fn(acc, c) { acc + c.to_int() }, 0)
        let index = hash % ready_endpoints.length()
        Some(ready_endpoints[index])
      }
    }
  }
  
  // 创建Ingress
  let create_ingress = fn(name: String, namespace: String, rules: Array[IngressRule]) {
    {
      name: name,
      namespace: namespace,
      rules: rules,
      tls: [],
      annotations: Map::from_array([
        ("kubernetes.io/ingress.class", "nginx"),
        ("nginx.ingress.kubernetes.io/rewrite-target", "/")
      ])
    }
  }
  
  // 添加TLS配置
  let add_ingress_tls = fn(ingress: Ingress, hosts: Array[String], secret_name: String) {
    let tls_config = {
      hosts: hosts,
      secret_name: secret_name
    }
    
    { ingress | tls: ingress.tls.push(tls_config) }
  }
  
  // 创建测试服务
  let telemetry_service = create_service_discovery("azimuth-telemetry", "azimuth-system", ServiceType::ClusterIP, LoadBalancingPolicy::RoundRobin)
  
  // 添加端口
  let telemetry_service_with_ports = telemetry_service
    |> add_service_port("http", "TCP", 8080, 8080)
    |> add_service_port("metrics", "TCP", 9090, 9090)
  
  // 添加端点
  let telemetry_service_with_endpoints = telemetry_service_with_ports
    |> add_service_endpoint("10.0.1.10", 8080, true)
    |> add_service_endpoint("10.0.1.11", 8080, true)
    |> add_service_endpoint("10.0.1.12", 8080, false)  // 未就绪
    |> add_service_endpoint("10.0.1.13", 8080, true)
  
  // 验证服务配置
  assert_eq(telemetry_service_with_endpoints.service_name, "azimuth-telemetry")
  assert_eq(telemetry_service_with_endpoints.service_type, ServiceType::ClusterIP)
  assert_eq(telemetry_service_with_endpoints.load_balancing_policy, LoadBalancingPolicy::RoundRobin)
  assert_eq(telemetry_service_with_endpoints.ports.length(), 2)
  assert_eq(telemetry_service_with_endpoints.endpoints.length(), 4)
  
  // 验证端口配置
  let http_port = telemetry_service_with_endpoints.ports.find(fn(p) { p.name == "http" })
  match http_port {
    Some(port) => {
      assert_eq(port.protocol, "TCP")
      assert_eq(port.port, 8080)
      assert_eq(port.target_port, 8080)
    }
    None => assert_true(false)
  }
  
  // 验证端点配置
  let ready_endpoints = telemetry_service_with_endpoints.endpoints.filter(fn(ep) { ep.ready })
  assert_eq(ready_endpoints.length(), 3)  // 3个就绪端点
  
  // 测试负载均衡
  let selected_endpoint = select_endpoint(telemetry_service_with_endpoints)
  assert_true(selected_endpoint.is_some())
  assert_true(selected_endpoint.unwrap().ready)
  
  // 测试不同负载均衡策略
  let least_conn_service = { telemetry_service_with_endpoints | load_balancing_policy: LoadBalancingPolicy::LeastConnection }
  let random_service = { telemetry_service_with_endpoints | load_balancing_policy: LoadBalancingPolicy::Random }
  let ip_hash_service = { telemetry_service_with_endpoints | load_balancing_policy: LoadBalancingPolicy::IPHash }
  let consistent_hash_service = { telemetry_service_with_endpoints | load_balancing_policy: LoadBalancingPolicy::ConsistentHash }
  
  let least_conn_endpoint = select_endpoint(least_conn_service)
  let random_endpoint = select_endpoint(random_service)
  let ip_hash_endpoint = select_endpoint(ip_hash_service)
  let consistent_hash_endpoint = select_endpoint(consistent_hash_service)
  
  assert_true(least_conn_endpoint.is_some())
  assert_true(random_endpoint.is_some())
  assert_true(ip_hash_endpoint.is_some())
  assert_true(consistent_hash_endpoint.is_some())
  
  // 创建Ingress规则
  let ingress_path = {
    path: "/telemetry",
    path_type: "Prefix",
    backend: {
      service_name: "azimuth-telemetry",
      service_port: 8080
    }
  }
  
  let ingress_rule = {
    host: "telemetry.azimuth.local",
    paths: [ingress_path]
  }
  
  let telemetry_ingress = create_ingress("azimuth-telemetry-ingress", "azimuth-system", [ingress_rule])
  
  // 添加TLS配置
  let telemetry_ingress_with_tls = add_ingress_tls(telemetry_ingress, ["telemetry.azimuth.local"], "azimuth-tls-secret")
  
  // 验证Ingress配置
  assert_eq(telemetry_ingress_with_tls.name, "azimuth-telemetry-ingress")
  assert_eq(telemetry_ingress_with_tls.namespace, "azimuth-system")
  assert_eq(telemetry_ingress_with_tls.rules.length(), 1)
  assert_eq(telemetry_ingress_with_tls.tls.length(), 1)
  
  // 验证Ingress规则
  let rule = telemetry_ingress_with_tls.rules[0]
  assert_eq(rule.host, "telemetry.azimuth.local")
  assert_eq(rule.paths.length(), 1)
  
  let path = rule.paths[0]
  assert_eq(path.path, "/telemetry")
  assert_eq(path.backend.service_name, "azimuth-telemetry")
  assert_eq(path.backend.service_port, 8080)
  
  // 验证TLS配置
  let tls = telemetry_ingress_with_tls.tls[0]
  assert_eq(tls.secret_name, "azimuth-tls-secret")
  assert_true(tls.hosts.contains("telemetry.azimuth.local"))
  
  // 验证注解
  assert_true(telemetry_ingress_with_tls.annotations.contains(("kubernetes.io/ingress.class", "nginx")))
}

// 测试3: 自动扩缩容
test "Pod自动扩缩容(HPA)配置" {
  // 定义扩缩容指标类型
  enum MetricType {
    Resource
    Pods
    Object
    External
  }
  
  // 定义资源指标
  enum ResourceMetric {
    CPU
    Memory
  }
  
  // 定义扩缩容策略
  type ScalingPolicy = {
    min_replicas: Int,
    max_replicas: Int,
    target_cpu_utilization: Option[Int],  // 百分比
    target_memory_utilization: Option[Int],  // 百分比
    stabilization_window_seconds: Int,
    scale_up_cooldown_period: Int,
    scale_down_cooldown_period: Int
  }
  
  // 定义指标规格
  type MetricSpec = {
    metric_type: MetricType,
    resource_metric: Option<ResourceMetric>,
    object_metric: Option[ObjectMetric],
    external_metric: Option[ExternalMetric],
    target: MetricTarget
  }
  
  // 定义对象指标
  type ObjectMetric = {
    described_object: DescribedObject,
    metric: String,
    target_value: Option<String],
    target_average_value: Option<String>
  }
  
  // 定义外部指标
  type ExternalMetric = {
    metric: String,
    target_value: Option[String],
    target_average_value: Option[String]
  }
  
  // 定义描述对象
  type DescribedObject = {
    api_version: String,
    kind: String,
    name: String
  }
  
  // 定义指标目标
  type MetricTarget = {
    type: String,  // "Value", "AverageValue", "Utilization"
    value: Option<String>,
    average_value: Option[String],
    average_utilization: Option[Int>
  }
  
  // 定义HPA状态
  type HPAStatus = {
    current_replicas: Int,
    desired_replicas: Int,
    last_scale_time: Option[Int>,
    current_cpu_utilization: Option[Int>,
    current_memory_utilization: Option[Int>
  }
  
  // 定义水平Pod自动扩缩容
  type HorizontalPodAutoscaler = {
    name: String,
    namespace: String,
    scale_target_ref: ScaleTargetRef,
    min_replicas: Int,
    max_replicas: Int,
    metrics: Array[MetricSpec],
    behavior: Option<ScalingBehavior>,
    status: HPAStatus
  }
  
  // 定义扩缩容目标引用
  type ScaleTargetRef = {
    api_version: String,
    kind: String,
    name: String
  }
  
  // 定义扩缩容行为
  type ScalingBehavior = {
    scale_up: Option<HPAScalingRules>,
    scale_down: Option[HPAScalingRules]
  }
  
  // 定义HPA扩缩容规则
  type HPAScalingRules = {
    stabilization_window_seconds: Int,
    policies: Array[HPAScalingPolicy],
    select_policy: String
  }
  
  // 定义HPA扩缩容策略
  type HPAScalingPolicy = {
    type: String,  // "Pods", "Percent"
    value: Int,
    period_seconds: Int
  }
  
  // 创建HPA
  let create_hpa = fn(name: String, namespace: String, target_ref: ScaleTargetRef, min_replicas: Int, max_replicas: Int) {
    {
      name: name,
      namespace: namespace,
      scale_target_ref: target_ref,
      min_replicas: min_replicas,
      max_replicas: max_replicas,
      metrics: [],
      behavior: None,
      status: {
        current_replicas: min_replicas,
        desired_replicas: min_replicas,
        last_scale_time: Some(Time::now()),
        current_cpu_utilization: None,
        current_memory_utilization: None
      }
    }
  }
  
  // 添加CPU指标
  let add_cpu_metric = fn(hpa: HorizontalPodAutoscaler, target_utilization: Int) {
    let cpu_metric = {
      metric_type: MetricType::Resource,
      resource_metric: Some(ResourceMetric::CPU),
      object_metric: None,
      external_metric: None,
      target: {
        type: "Utilization",
        value: None,
        average_value: None,
        average_utilization: Some(target_utilization)
      }
    }
    
    { hpa | metrics: hpa.metrics.push(cpu_metric) }
  }
  
  // 添加内存指标
  let add_memory_metric = fn(hpa: HorizontalPodAutoscaler, target_utilization: Int) {
    let memory_metric = {
      metric_type: MetricType::Resource,
      resource_metric: Some(ResourceMetric::Memory),
      object_metric: None,
      external_metric: None,
      target: {
        type: "Utilization",
        value: None,
        average_value: None,
        average_utilization: Some(target_utilization)
      }
    }
    
    { hpa | metrics: hpa.metrics.push(memory_metric) }
  }
  
  // 添加自定义指标
  let add_custom_metric = fn(hpa: HorizontalPodAutoscaler, metric_name: String, target_value: String) {
    let custom_metric = {
      metric_type: MetricType::External,
      resource_metric: None,
      object_metric: None,
      external_metric: Some({
        metric: metric_name,
        target_value: Some(target_value),
        target_average_value: None
      }),
      target: {
        type: "Value",
        value: Some(target_value),
        average_value: None,
        average_utilization: None
      }
    }
    
    { hpa | metrics: hpa.metrics.push(custom_metric) }
  }
  
  // 设置扩缩容行为
  let set_scaling_behavior = fn(hpa: HorizontalPodAutoscaler, behavior: ScalingBehavior) {
    { hpa | behavior: Some(behavior) }
  }
  
  // 计算期望副本数
  let calculate_desired_replicas = fn(hpa: HorizontalPodAutoscaler, current_cpu: Int, current_memory: Int) {
    let mut desired_replicas = hpa.status.current_replicas
    
    // 基于CPU利用率计算
    for metric in hpa.metrics {
      match metric.resource_metric {
        Some(ResourceMetric::CPU) => {
          match metric.target.average_utilization {
            Some(target_cpu) => {
              if current_cpu > target_cpu {
                // CPU使用率超过目标，需要扩容
                let scale_ratio = (current_cpu as Float) / (target_cpu as Float)
                let cpu_based_replicas = ((desired_replicas as Float) * scale_ratio).ceil() as Int
                desired_replicas = if cpu_based_replicas > desired_replicas { cpu_based_replicas } else { desired_replicas }
              } else if current_cpu < target_cpu * 80 / 100 {
                // CPU使用率低于目标的80%，可能需要缩容
                let scale_ratio = (current_cpu as Float) / (target_cpu as Float)
                let cpu_based_replicas = ((desired_replicas as Float) * scale_ratio).floor() as Int
                desired_replicas = if cpu_based_replicas < desired_replicas { cpu_based_replicas } else { desired_replicas }
              }
            }
            None => {}
          }
        }
        Some(ResourceMetric::Memory) => {
          match metric.target.average_utilization {
            Some(target_memory) => {
              if current_memory > target_memory {
                // 内存使用率超过目标，需要扩容
                let scale_ratio = (current_memory as Float) / (target_memory as Float)
                let memory_based_replicas = ((desired_replicas as Float) * scale_ratio).ceil() as Int
                desired_replicas = if memory_based_replicas > desired_replicas { memory_based_replicas } else { desired_replicas }
              }
            }
            None => {}
          }
        }
        None => {}
      }
    }
    
    // 应用最小和最大副本数限制
    if desired_replicas < hpa.min_replicas {
      desired_replicas = hpa.min_replicas
    } else if desired_replicas > hpa.max_replicas {
      desired_replicas = hpa.max_replicas
    }
    
    desired_replicas
  }
  
  // 更新HPA状态
  let update_hpa_status = fn(hpa: HorizontalPodAutoscaler, current_cpu: Option[Int>, current_memory: Option<Int>) {
    let cpu = current_cpu.unwrap_or(0)
    let memory = current_memory.unwrap_or(0)
    let desired_replicas = calculate_desired_replicas(hpa, cpu, memory)
    
    let new_status = {
      current_replicas: hpa.status.current_replicas,
      desired_replicas: desired_replicas,
      last_scale_time: if desired_replicas != hpa.status.current_replicas {
        Some(Time::now())
      } else {
        hpa.status.last_scale_time
      },
      current_cpu_utilization: current_cpu,
      current_memory_utilization: current_memory
    }
    
    { hpa | status: new_status }
  }
  
  // 创建测试HPA
  let scale_target_ref = {
    api_version: "apps/v1",
    kind: "Deployment",
    name: "azimuth-telemetry"
  }
  
  let telemetry_hpa = create_hpa("azimuth-telemetry-hpa", "azimuth-system", scale_target_ref, 2, 10)
  
  // 添加指标
  let telemetry_hpa_with_metrics = telemetry_hpa
    |> add_cpu_metric(70)  // 目标CPU使用率70%
    |> add_memory_metric(80)  // 目标内存使用率80%
    |> add_custom_metric("http_requests_per_second", "1000")  // 自定义指标
  
  // 验证HPA配置
  assert_eq(telemetry_hpa_with_metrics.name, "azimuth-telemetry-hpa")
  assert_eq(telemetry_hpa_with_metrics.namespace, "azimuth-system")
  assert_eq(telemetry_hpa_with_metrics.min_replicas, 2)
  assert_eq(telemetry_hpa_with_metrics.max_replicas, 10)
  assert_eq(telemetry_hpa_with_metrics.metrics.length(), 3)
  
  // 验证指标配置
  let cpu_metric = telemetry_hpa_with_metrics.metrics.find(fn(m) {
    match m.resource_metric {
      Some(ResourceMetric::CPU) => true
      _ => false
    }
  })
  
  match cpu_metric {
    Some(metric) => {
      assert_eq(metric.metric_type, MetricType::Resource)
      assert_eq(metric.target.average_utilization, Some(70))
    }
    None => assert_true(false)
  }
  
  // 测试扩缩容计算
  // 场景1: CPU使用率高，需要扩容
  let high_cpu_hpa = update_hpa_status(telemetry_hpa_with_metrics, Some(85), Some(60))
  assert_eq(high_cpu_hpa.status.current_cpu_utilization, Some(85))
  assert_eq(high_cpu_hpa.status.current_memory_utilization, Some(60))
  assert_true(high_cpu_hpa.status.desired_replicas > high_cpu_hpa.status.current_replicas)
  
  // 场景2: CPU使用率低，需要缩容
  let low_cpu_hpa = update_hpa_status(telemetry_hpa_with_metrics, Some(50), Some(60))
  assert_eq(low_cpu_hpa.status.current_cpu_utilization, Some(50))
  assert_eq(low_cpu_hpa.status.current_memory_utilization, Some(60))
  assert_true(low_cpu_hpa.status.desired_replicas <= low_cpu_hpa.status.current_replicas)
  
  // 场景3: 内存使用率高，需要扩容
  let high_memory_hpa = update_hpa_status(telemetry_hpa_with_metrics, Some(60), Some(90))
  assert_eq(high_memory_hpa.status.current_cpu_utilization, Some(60))
  assert_eq(high_memory_hpa.status.current_memory_utilization, Some(90))
  assert_true(high_memory_hpa.status.desired_replicas > high_memory_hpa.status.current_replicas)
  
  // 测试最小和最大副本数限制
  let min_replicas_hpa = update_hpa_status(telemetry_hpa_with_metrics, Some(20), Some(20))
  assert_eq(min_replicas_hpa.status.desired_replicas, 2)  // 不应低于最小副本数
  
  let max_replicas_hpa = update_hpa_status(telemetry_hpa_with_metrics, Some(95), Some(95))
  assert_eq(max_replicas_hpa.status.desired_replicas, 10)  // 不应超过最大副本数
  
  // 设置扩缩容行为
  let scaling_behavior = {
    scale_up: Some({
      stabilization_window_seconds: 60,
      policies: [
        {
          type: "Percent",
          value: 100,  // 每次最多扩容100%
          period_seconds: 15
        }
      ],
      select_policy: "Max"
    }),
    scale_down: Some({
      stabilization_window_seconds: 300,
      policies: [
        {
          type: "Percent",
          value: 10,  // 每次最多缩容10%
          period_seconds: 60
        }
      ],
      select_policy: "Min"
    })
  }
  
  let telemetry_hpa_with_behavior = set_scaling_behavior(telemetry_hpa_with_metrics, scaling_behavior)
  
  // 验证扩缩容行为
  match telemetry_hpa_with_behavior.behavior {
    Some(behavior) => {
      match behavior.scale_up {
        Some(scale_up) => {
          assert_eq(scale_up.stabilization_window_seconds, 60)
          assert_eq(scale_up.policies.length(), 1)
          assert_eq(scale_up.policies[0].type, "Percent")
          assert_eq(scale_up.policies[0].value, 100)
        }
        None => assert_true(false)
      }
      
      match behavior.scale_down {
        Some(scale_down) => {
          assert_eq(scale_down.stabilization_window_seconds, 300)
          assert_eq(scale_down.policies.length(), 1)
          assert_eq(scale_down.policies[0].type, "Percent")
          assert_eq(scale_down.policies[0].value, 10)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// 测试4: 配置管理和密钥管理
test "配置管理和密钥管理系统" {
  // 定义配置类型
  enum ConfigType {
    ConfigMap
    Secret
  }
  
  // 定义密钥类型
  enum SecretType {
    Opaque
    ServiceAccountToken
    DockerConfigJson
    TLS
  }
  
  // 定义配置数据
  type ConfigData = {
    key: String,
    value: String,
    encoding: String  // "plain", "base64"
  }
  
  // 定义配置项
  type ConfigItem = {
    name: String,
    namespace: String,
    config_type: ConfigType,
    secret_type: Option<SecretType>,
    data: Array[ConfigData],
    labels: Map[String, String],
    annotations: Map[String, String],
    immutable: Bool,
    created_at: Int
  }
  
  // 定义配置引用
  type ConfigRef = {
    name: String,
    key: String,
    optional: Bool
  }
  
  // 定义环境变量来源
  type EnvVarSource = {
    config_map_ref: Option[ConfigRef],
    secret_ref: Option[ConfigRef>
  }
  
  // 定义卷挂载来源
  type VolumeSource = {
    config_map: Option[ConfigMapVolumeSource>,
    secret: Option[SecretVolumeSource>
  }
  
  // 定义ConfigMap卷来源
  type ConfigMapVolumeSource = {
    name: String,
    items: Option[Array[KeyToPath>>
  }
  
  // 定义Secret卷来源
  type SecretVolumeSource = {
    secret_name: String,
    items: Option[Array[KeyToPath>>
  }
  
  // 定义键到路径映射
  type KeyToPath = {
    key: String,
    path: String,
    mode: Option<Int>
  }
  
  // 创建配置项
  let create_config_item = fn(name: String, namespace: String, config_type: ConfigType) {
    {
      name: name,
      namespace: namespace,
      config_type: config_type,
      secret_type: None,
      data: [],
      labels: Map::from_array([
        ("app", "azimuth"),
        ("managed-by", "azimuth-config")
      ]),
      annotations: Map::empty(),
      immutable: false,
      created_at: Time::now()
    }
  }
  
  // 设置密钥类型
  let set_secret_type = fn(config: ConfigItem, secret_type: SecretType) {
    { config | secret_type: Some(secret_type) }
  }
  
  // 添加配置数据
  let add_config_data = fn(config: ConfigItem, key: String, value: String, encoding: String) {
    let new_data = {
      key: key,
      value: value,
      encoding: encoding
    }
    
    { config | data: config.data.push(new_data) }
  }
  
  // 设置不可变
  let set_immutable = fn(config: ConfigItem, immutable: Bool) {
    { config | immutable: immutable }
  }
  
  // Base64编码
  let base64_encode = fn(input: String) {
    // 简化的Base64编码（实际应使用标准库）
    let bytes = input.to_utf8_bytes()
    let mut encoded = ""
    
    for i in 0..bytes.length() {
      encoded = encoded + bytes[i].to_string() + ","
    }
    
    encoded
  }
  
  // Base64解码
  let base64_decode = fn(input: String) {
    // 简化的Base64解码（实际应使用标准库）
    let parts = input.split(",")
    let mut bytes = []
    
    for part in parts {
      if part.length() > 0 {
        bytes = bytes.push(part.to_int())
      }
    }
    
    match String::from_utf8(bytes) {
      Ok(decoded) => decoded,
      Err(_) => input
    }
  }
  
  // 创建ConfigMap
  let create_config_map = fn(name: String, namespace: String, data: Map[String, String]) {
    let config_map = create_config_item(name, namespace, ConfigType::ConfigMap)
    
    let config_with_data = data.reduce(fn(config, kv) {
      add_config_data(config, kv.0, kv.1, "plain")
    }, config_map)
    
    config_with_data
  }
  
  // 创建Secret
  let create_secret = fn(name: String, namespace: String, secret_type: SecretType, data: Map[String, String]) {
    let secret = create_config_item(name, namespace, ConfigType::Secret)
      |> set_secret_type(secret_type)
    
    let secret_with_data = data.reduce(fn(config, kv) {
      // Secret数据默认使用Base64编码
      let encoded_value = base64_encode(kv.1)
      add_config_data(config, kv.0, encoded_value, "base64")
    }, secret)
    
    secret_with_data
  }
  
  // 获取配置值
  let get_config_value = fn(config: ConfigItem, key: String) {
    match config.data.find_fn(d) { d.key == key } {
      Some(data) => {
        if data.encoding == "base64" {
          base64_decode(data.value)
        } else {
          data.value
        }
      }
      None => ""
    }
  }
  
  // 创建环境变量来源
  let create_env_var_source = fn(config_map_ref: Option[ConfigRef>, secret_ref: Option[ConfigRef>) {
    {
      config_map_ref: config_map_ref,
      secret_ref: secret_ref
    }
  }
  
  // 创建卷挂载来源
  let create_volume_source = fn(config_map: Option[ConfigMapVolumeSource>, secret: Option[SecretVolumeSource>) {
    {
      config_map: config_map,
      secret: secret
    }
  }
  
  // 创建测试ConfigMap
  let config_map_data = Map::from_array([
    ("telemetry.endpoint", "http://collector:14268/api/traces"),
    ("telemetry.sampling.rate", "0.1"),
    ("log.level", "info"),
    ("service.version", "1.0.0")
  ])
  
  let telemetry_config_map = create_config_map("azimuth-config", "azimuth-system", config_map_data)
  
  // 验证ConfigMap
  assert_eq(telemetry_config_map.name, "azimuth-config")
  assert_eq(telemetry_config_map.namespace, "azimuth-system")
  assert_eq(telemetry_config_map.config_type, ConfigType::ConfigMap)
  assert_eq(telemetry_config_map.data.length(), 4)
  assert_false(telemetry_config_map.immutable)
  
  // 验证配置数据
  assert_eq(get_config_value(telemetry_config_map, "telemetry.endpoint"), "http://collector:14268/api/traces")
  assert_eq(get_config_value(telemetry_config_map, "telemetry.sampling.rate"), "0.1")
  assert_eq(get_config_value(telemetry_config_map, "log.level"), "info")
  assert_eq(get_config_value(telemetry_config_map, "service.version"), "1.0.0")
  
  // 创建测试Secret
  let secret_data = Map::from_array([
    ("database.username", "azimuth_user"),
    ("database.password", "super_secret_password"),
    ("api.key", "abcd1234567890efghijklmnopqrstuv")
  ])
  
  let telemetry_secret = create_secret("azimuth-secrets", "azimuth-system", SecretType::Opaque, secret_data)
  
  // 验证Secret
  assert_eq(telemetry_secret.name, "azimuth-secrets")
  assert_eq(telemetry_secret.namespace, "azimuth-system")
  assert_eq(telemetry_secret.config_type, ConfigType::Secret)
  assert_eq(telemetry_secret.secret_type, Some(SecretType::Opaque))
  assert_eq(telemetry_secret.data.length(), 3)
  
  // 验证Secret数据（Base64解码）
  assert_eq(get_config_value(telemetry_secret, "database.username"), "azimuth_user")
  assert_eq(get_config_value(telemetry_secret, "database.password"), "super_secret_password")
  assert_eq(get_config_value(telemetry_secret, "api.key"), "abcd1234567890efghijklmnopqrstuv")
  
  // 创建TLS Secret
  let tls_data = Map::from_array([
    ("tls.crt", "-----BEGIN CERTIFICATE-----\nMIIC..."),
    ("tls.key", "-----BEGIN RSA PRIVATE KEY-----\nMIIE...")
  ])
  
  let tls_secret = create_secret("azimuth-tls", "azimuth-system", SecretType::TLS, tls_data)
  
  // 验证TLS Secret
  assert_eq(tls_secret.secret_type, Some(SecretType::TLS))
  assert_eq(get_config_value(tls_secret, "tls.crt"), "-----BEGIN CERTIFICATE-----\nMIIC...")
  assert_eq(get_config_value(tls_secret, "tls.key"), "-----BEGIN RSA PRIVATE KEY-----\nMIIE...")
  
  // 设置不可变配置
  let immutable_config = set_immutable(telemetry_config_map, true)
  assert_true(immutable_config.immutable)
  
  // 创建环境变量来源
  let config_map_ref = {
    name: "azimuth-config",
    key: "telemetry.endpoint",
    optional: false
  }
  
  let secret_ref = {
    name: "azimuth-secrets",
    key: "database.password",
    optional: false
  }
  
  let env_var_source = create_env_var_source(Some(config_map_ref), Some(secret_ref))
  
  // 验证环境变量来源
  match env_var_source.config_map_ref {
    Some(ref) => {
      assert_eq(ref.name, "azimuth-config")
      assert_eq(ref.key, "telemetry.endpoint")
      assert_false(ref.optional)
    }
    None => assert_true(false)
  }
  
  match env_var_source.secret_ref {
    Some(ref) => {
      assert_eq(ref.name, "azimuth-secrets")
      assert_eq(ref.key, "database.password")
      assert_false(ref.optional)
    }
    None => assert_true(false)
  }
  
  // 创建卷挂载来源
  let key_to_paths = [
    {
      key: "telemetry.endpoint",
      path: "endpoint.conf",
      mode: Some(420)  // 0644 in octal
    },
    {
      key: "log.level",
      path: "log-level.conf",
      mode: Some(420)
    }
  ]
  
  let config_map_volume = {
    name: "azimuth-config",
    items: Some(key_to_paths)
  }
  
  let secret_volume = {
    secret_name: "azimuth-secrets",
    items: Some([
      {
        key: "database.password",
        path: "db-password",
        mode: Some(384)  // 0600 in octal
      }
    ])
  }
  
  let volume_source = create_volume_source(Some(config_map_volume), Some(secret_volume))
  
  // 验证卷挂载来源
  match volume_source.config_map {
    Some(cm) => {
      assert_eq(cm.name, "azimuth-config")
      assert_true(cm.items.is_some())
      assert_eq(cm.items.unwrap().length(), 2)
    }
    None => assert_true(false)
  }
  
  match volume_source.secret {
    Some(secret) => {
      assert_eq(secret.secret_name, "azimuth-secrets")
      assert_true(secret.items.is_some())
      assert_eq(secret.items.unwrap().length(), 1)
    }
    None => assert_true(false)
  }
  
  // 测试配置更新
  let updated_config_data = Map::from_array([
    ("telemetry.endpoint", "http://new-collector:14268/api/traces"),
    ("telemetry.sampling.rate", "0.2"),
    ("log.level", "debug"),
    ("service.version", "1.1.0"),
    ("new.feature.enabled", "true")
  ])
  
  let updated_config_map = create_config_map("azimuth-config", "azimuth-system", updated_config_data)
  
  // 验证更新后的配置
  assert_eq(get_config_value(updated_config_map, "telemetry.endpoint"), "http://new-collector:14268/api/traces")
  assert_eq(get_config_value(updated_config_map, "telemetry.sampling.rate"), "0.2")
  assert_eq(get_config_value(updated_config_map, "log.level"), "debug")
  assert_eq(get_config_value(updated_config_map, "service.version"), "1.1.0")
  assert_eq(get_config_value(updated_config_map, "new.feature.enabled"), "true")
}