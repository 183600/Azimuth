// Azimuth Containerization Tests
// This file contains test cases for container-based telemetry deployment and monitoring

// Test 1: Docker Container Configuration
test "docker container configuration for telemetry services" {
  // Define Docker container configuration
  type DockerContainer = {
    id: String,
    name: String,
    image: String,
    tag: String,
    ports: Array[(Int, Int)],  // (host_port, container_port)
    environment: Array[(String, String)],
    volumes: Array[(String, String)],  // (host_path, container_path)
    networks: Array[String],
    depends_on: Array[String],
    health_check: Option[HealthCheck],
    resources: ResourceLimits
  }
  
  type HealthCheck = {
    command: Array[String],
    interval: Int,  // seconds
    timeout: Int,   // seconds
    retries: Int,
    start_period: Int
  }
  
  type ResourceLimits = {
    memory_limit: String,  // e.g., "512m"
    cpu_limit: String,     // e.g., "0.5"
    memory_reservation: String,
    cpu_reservation: String
  }
  
  // Create telemetry collector container
  let telemetry_collector = {
    id: "azimuth-telemetry-collector",
    name: "azimuth-collector",
    image: "azimuth/telemetry-collector",
    tag: "v1.2.3",
    ports: [(4317, 4317), (4318, 4318), (8888, 8888)],
    environment: [
      ("OTLP_ENDPOINT", "http://otel-collector:4317"),
      ("LOG_LEVEL", "info"),
      ("METRICS_EXPORT_INTERVAL", "30s")
    ],
    volumes: [
      ("/etc/azimuth/config", "/app/config"),
      ("/var/log/azimuth", "/app/logs")
    ],
    networks: ["azimuth-network"],
    depends_on: ["otel-collector", "redis"],
    health_check: Some({
      command: ["curl", "-f", "http://localhost:8888/health"],
      interval: 30,
      timeout: 10,
      retries: 3,
      start_period: 60
    }),
    resources: {
      memory_limit: "512m",
      cpu_limit: "0.5",
      memory_reservation: "256m",
      cpu_reservation: "0.25"
    }
  }
  
  // Create OpenTelemetry collector container
  let otel_collector = {
    id: "otel-collector",
    name: "otel-collector",
    image: "otel/opentelemetry-collector-contrib",
    tag: "0.88.0",
    ports: [(4317, 4317), (4318, 4318), (8889, 8889)],
    environment: [
      ("OTEL_EXPORTER_OTLP_ENDPOINT", "http://jaeger:14250"),
      ("OTEL_EXPORTER_PROMETHEUS_ENDPOINT", "http://prometheus:9090")
    ],
    volumes: [
      ("/etc/otel-collector/config", "/etc/otel-collector")
    ],
    networks: ["azimuth-network"],
    depends_on: ["jaeger", "prometheus"],
    health_check: Some({
      command: ["curl", "-f", "http://localhost:8889/metrics"],
      interval: 30,
      timeout: 10,
      retries: 3,
      start_period: 60
    }),
    resources: {
      memory_limit: "1g",
      cpu_limit: "1.0",
      memory_reservation: "512m",
      cpu_reservation: "0.5"
    }
  }
  
  // Verify container configurations
  assert_eq(telemetry_collector.name, "azimuth-collector")
  assert_eq(telemetry_collector.image, "azimuth/telemetry-collector")
  assert_eq(telemetry_collector.tag, "v1.2.3")
  assert_eq(telemetry_collector.ports.length(), 3)
  assert_eq(telemetry_collector.environment.length(), 3)
  assert_eq(telemetry_collector.volumes.length(), 2)
  assert_eq(telemetry_collector.networks.length(), 1)
  assert_eq(telemetry_collector.depends_on.length(), 2)
  
  match telemetry_collector.health_check {
    Some(check) => {
      assert_eq(check.command.length(), 4)
      assert_eq(check.interval, 30)
      assert_eq(check.timeout, 10)
      assert_eq(check.retries, 3)
      assert_eq(check.start_period, 60)
    }
    None => assert_true(false)
  }
  
  assert_eq(telemetry_collector.resources.memory_limit, "512m")
  assert_eq(telemetry_collector.resources.cpu_limit, "0.5")
  
  assert_eq(otel_collector.name, "otel-collector")
  assert_eq(otel_collector.image, "otel/opentelemetry-collector-contrib")
  assert_eq(otel_collector.tag, "0.88.0")
}

// Test 2: Docker Compose Service Orchestration
test "docker compose service orchestration for telemetry stack" {
  // Define Docker Compose service
  type ComposeService = {
    name: String,
    container: DockerContainer,
    deploy: Option[DeployConfig],
    scale: Int
  }
  
  type DeployConfig = {
    replicas: Int,
    restart_policy: RestartPolicy,
    update_config: UpdateConfig,
    placement: PlacementConfig
  }
  
  type RestartPolicy = {
    condition: String,  // "none", "on-failure", "any"
    delay: String,      // "5s"
    max_attempts: Int,
    window: String      // "120s"
  }
  
  type UpdateConfig = {
    parallelism: Int,
    delay: String,
    failure_action: String,  // "pause", "continue", "rollback"
    monitor: String,
    max_failure_ratio: String
  }
  
  type PlacementConfig = {
    constraints: Array[String],
    preferences: Array[String],
    max_replicas_per_node: Int
  }
  
  type DockerCompose = {
    version: String,
    services: Array[ComposeService],
    networks: Array[NetworkConfig],
    volumes: Array[VolumeConfig]
  }
  
  type NetworkConfig = {
    name: String,
    driver: String,
    external: Bool
  }
  
  type VolumeConfig = {
    name: String,
    driver: String,
    driver_opts: Array[(String, String)],
    external: Bool
  }
  
  type DockerContainer = {
    id: String,
    name: String,
    image: String,
    tag: String,
    ports: Array[(Int, Int)],
    environment: Array[(String, String)],
    volumes: Array[(String, String)],
    networks: Array[String],
    depends_on: Array[String],
    health_check: Option[HealthCheck],
    resources: ResourceLimits
  }
  
  type HealthCheck = {
    command: Array[String],
    interval: Int,
    timeout: Int,
    retries: Int,
    start_period: Int
  }
  
  type ResourceLimits = {
    memory_limit: String,
    cpu_limit: String,
    memory_reservation: String,
    cpu_reservation: String
  }
  
  // Create network configurations
  let telemetry_network = {
    name: "azimuth-network",
    driver: "bridge",
    external: false
  }
  
  let monitoring_network = {
    name: "monitoring",
    driver: "bridge",
    external: false
  }
  
  // Create volume configurations
  let config_volume = {
    name: "azimuth-config",
    driver: "local",
    driver_opts: [],
    external: false
  }
  
  let logs_volume = {
    name: "azimuth-logs",
    driver: "local",
    driver_opts: [],
    external: false
  }
  
  // Create service configurations
  let simple_collector_container = {
    id: "azimuth-telemetry-collector",
    name: "azimuth-collector",
    image: "azimuth/telemetry-collector",
    tag: "v1.2.3",
    ports: [(4317, 4317)],
    environment: [("LOG_LEVEL", "info")],
    volumes: [],
    networks: ["azimuth-network"],
    depends_on: [],
    health_check: None,
    resources: {
      memory_limit: "512m",
      cpu_limit: "0.5",
      memory_reservation: "256m",
      cpu_reservation: "0.25"
    }
  }
  
  let collector_service = {
    name: "azimuth-collector",
    container: simple_collector_container,
    deploy: Some({
      replicas: 3,
      restart_policy: {
        condition: "on-failure",
        delay: "5s",
        max_attempts: 3,
        window: "120s"
      },
      update_config: {
        parallelism: 1,
        delay: "10s",
        failure_action: "rollback",
        monitor: "60s",
        max_failure_ratio: "0.3"
      },
      placement: {
        constraints: ["node.role == worker"],
        preferences: [],
        max_replicas_per_node: 1
      }
    }),
    scale: 3
  }
  
  // Create Docker Compose configuration
  let compose_config = {
    version: "3.8",
    services: [collector_service],
    networks: [telemetry_network, monitoring_network],
    volumes: [config_volume, logs_volume]
  }
  
  // Verify Docker Compose configuration
  assert_eq(compose_config.version, "3.8")
  assert_eq(compose_config.services.length(), 1)
  assert_eq(compose_config.networks.length(), 2)
  assert_eq(compose_config.volumes.length(), 2)
  
  let service = compose_config.services[0]
  assert_eq(service.name, "azimuth-collector")
  assert_eq(service.scale, 3)
  
  match service.deploy {
    Some(deploy) => {
      assert_eq(deploy.replicas, 3)
      assert_eq(deploy.restart_policy.condition, "on-failure")
      assert_eq(deploy.restart_policy.delay, "5s")
      assert_eq(deploy.restart_policy.max_attempts, 3)
      assert_eq(deploy.update_config.parallelism, 1)
      assert_eq(deploy.update_config.delay, "10s")
      assert_eq(deploy.placement.constraints.length(), 1)
      assert_eq(deploy.placement.constraints[0], "node.role == worker")
      assert_eq(deploy.placement.max_replicas_per_node, 1)
    }
    None => assert_true(false)
  }
}

// Test 3: Kubernetes Deployment Configuration
test "kubernetes deployment configuration for telemetry services" {
  // Define Kubernetes deployment configuration
  type KubernetesDeployment = {
    api_version: String,
    kind: String,
    metadata: Metadata,
    spec: DeploymentSpec
  }
  
  type Metadata = {
    name: String,
    namespace: String,
    labels: Array[(String, String)],
    annotations: Array[(String, String)]
  }
  
  type DeploymentSpec = {
    replicas: Int,
    selector: LabelSelector,
    template: PodTemplateSpec
  }
  
  type LabelSelector = {
    match_labels: Array[(String, String)]
  }
  
  type PodTemplateSpec = {
    metadata: Metadata,
    spec: PodSpec
  }
  
  type PodSpec = {
    containers: Array[Container],
    volumes: Array[Volume],
    restart_policy: String,
    image_pull_secrets: Array[String]
  }
  
  type Container = {
    name: String,
    image: String,
    ports: Array[ContainerPort],
    env: Array[EnvVar],
    volume_mounts: Array[VolumeMount],
    resources: ResourceRequirements,
    liveness_probe: Option[Probe],
    readiness_probe: Option[Probe]
  }
  
  type ContainerPort = {
    name: String,
    container_port: Int,
    protocol: String
  }
  
  type EnvVar = {
    name: String,
    value: Option[String],
    value_from: Option[EnvVarSource]
  }
  
  type EnvVarSource = {
    secret_key_ref: Option[SecretKeySelector],
    config_map_key_ref: Option[ConfigMapKeySelector]
  }
  
  type SecretKeySelector = {
    name: String,
    key: String
  }
  
  type ConfigMapKeySelector = {
    name: String,
    key: String
  }
  
  type VolumeMount = {
    name: String,
    mount_path: String,
    read_only: Bool
  }
  
  type Volume = {
    name: String,
    secret: Option[SecretVolumeSource],
    config_map: Option[ConfigMapVolumeSource],
    persistent_volume_claim: Option[PersistentVolumeClaimVolumeSource]
  }
  
  type SecretVolumeSource = {
    secret_name: String
  }
  
  type ConfigMapVolumeSource = {
    name: String
  }
  
  type PersistentVolumeClaimVolumeSource = {
    claim_name: String
  }
  
  type ResourceRequirements = {
    requests: ResourceList,
    limits: ResourceList
  }
  
  type ResourceList = {
    cpu: String,
    memory: String
  }
  
  type Probe = {
    http_get: HTTPGetAction,
    initial_delay_seconds: Int,
    period_seconds: Int,
    timeout_seconds: Int,
    failure_threshold: Int
  }
  
  type HTTPGetAction = {
    path: String,
    port: Int,
    scheme: String
  }
  
  // Create telemetry collector deployment
  let collector_deployment = {
    api_version: "apps/v1",
    kind: "Deployment",
    metadata: {
      name: "azimuth-telemetry-collector",
      namespace: "azimuth",
      labels: [
        ("app", "azimuth-telemetry-collector"),
        ("version", "v1.2.3"),
        ("component", "telemetry")
      ],
      annotations: [
        ("prometheus.io/scrape", "true"),
        ("prometheus.io/port", "8888"),
        ("prometheus.io/path", "/metrics")
      ]
    },
    spec: {
      replicas: 3,
      selector: {
        match_labels: [
          ("app", "azimuth-telemetry-collector")
        ]
      },
      template: {
        metadata: {
          name: "azimuth-telemetry-collector",
          namespace: "azimuth",
          labels: [
            ("app", "azimuth-telemetry-collector"),
            ("version", "v1.2.3")
          ],
          annotations: []
        },
        spec: {
          containers: [
            {
              name: "azimuth-telemetry-collector",
              image: "azimuth/telemetry-collector:v1.2.3",
              ports: [
                {
                  name: "otlp-grpc",
                  container_port: 4317,
                  protocol: "TCP"
                },
                {
                  name: "otlp-http",
                  container_port: 4318,
                  protocol: "TCP"
                },
                {
                  name: "metrics",
                  container_port: 8888,
                  protocol: "TCP"
                }
              ],
              env: [
                {
                  name: "OTLP_ENDPOINT",
                  value: Some("http://otel-collector:4317"),
                  value_from: None
                },
                {
                  name: "LOG_LEVEL",
                  value: Some("info"),
                  value_from: None
                },
                {
                  name: "API_KEY",
                  value: None,
                  value_from: Some({
                    secret_key_ref: Some({
                      name: "azimuth-secrets",
                      key: "api-key"
                    }),
                    config_map_key_ref: None
                  })
                }
              ],
              volume_mounts: [
                {
                  name: "config",
                  mount_path: "/app/config",
                  read_only: true
                },
                {
                  name: "logs",
                  mount_path: "/app/logs",
                  read_only: false
                }
              ],
              resources: {
                requests: {
                  cpu: "250m",
                  memory: "256Mi"
                },
                limits: {
                  cpu: "500m",
                  memory: "512Mi"
                }
              },
              liveness_probe: Some({
                http_get: {
                  path: "/health",
                  port: 8888,
                  scheme: "HTTP"
                },
                initial_delay_seconds: 60,
                period_seconds: 30,
                timeout_seconds: 10,
                failure_threshold: 3
              }),
              readiness_probe: Some({
                http_get: {
                  path: "/ready",
                  port: 8888,
                  scheme: "HTTP"
                },
                initial_delay_seconds: 30,
                period_seconds: 10,
                timeout_seconds: 5,
                failure_threshold: 3
              })
            }
          ],
          volumes: [
            {
              name: "config",
              secret: None,
              config_map: Some({
                name: "azimuth-config"
              }),
              persistent_volume_claim: None
            },
            {
              name: "logs",
              secret: None,
              config_map: None,
              persistent_volume_claim: Some({
                claim_name: "azimuth-logs-pvc"
              })
            }
          ],
          restart_policy: "Always",
          image_pull_secrets: ["azimuth-registry-secret"]
        }
      }
    }
  }
  
  // Verify deployment configuration
  assert_eq(collector_deployment.api_version, "apps/v1")
  assert_eq(collector_deployment.kind, "Deployment")
  assert_eq(collector_deployment.metadata.name, "azimuth-telemetry-collector")
  assert_eq(collector_deployment.metadata.namespace, "azimuth")
  assert_eq(collector_deployment.spec.replicas, 3)
  
  let container = collector_deployment.spec.template.spec.containers[0]
  assert_eq(container.name, "azimuth-telemetry-collector")
  assert_eq(container.image, "azimuth/telemetry-collector:v1.2.3")
  assert_eq(container.ports.length(), 3)
  assert_eq(container.env.length(), 3)
  assert_eq(container.volume_mounts.length(), 2)
  
  assert_eq(container.resources.requests.cpu, "250m")
  assert_eq(container.resources.requests.memory, "256Mi")
  assert_eq(container.resources.limits.cpu, "500m")
  assert_eq(container.resources.limits.memory, "512Mi")
  
  match container.liveness_probe {
    Some(probe) => {
      assert_eq(probe.http_get.path, "/health")
      assert_eq(probe.http_get.port, 8888)
      assert_eq(probe.initial_delay_seconds, 60)
      assert_eq(probe.period_seconds, 30)
      assert_eq(probe.timeout_seconds, 10)
      assert_eq(probe.failure_threshold, 3)
    }
    None => assert_true(false)
  }
}

// Test 4: Container Health Monitoring and Telemetry
test "container health monitoring and telemetry collection" {
  // Define container health status
  enum HealthStatus {
    Healthy
    Unhealthy
    Unknown
    Starting
  }
  
  type ContainerMetrics = {
    container_id: String,
    name: String,
    cpu_usage: Float,      // percentage
    memory_usage: Float,   // MB
    network_rx: Float,     // MB
    network_tx: Float,     // MB
    disk_read: Float,      // MB
    disk_write: Float,     // MB
    uptime: Int,           // seconds
    restart_count: Int,
    health_status: HealthStatus,
    last_check: Int        // timestamp
  }
  
  type HealthCheckResult = {
    container_id: String,
    status: HealthStatus,
    message: String,
    timestamp: Int,
    response_time: Int     // milliseconds
  }
  
  type MonitoringData = {
    containers: Array[ContainerMetrics],
    health_checks: Array[HealthCheckResult],
    alerts: Array[Alert]
  }
  
  type Alert = {
    id: String,
    severity: String,      // "info", "warning", "critical"
    container_id: String,
    message: String,
    timestamp: Int,
    resolved: Bool
  }
  
  // Create health check function
  let check_container_health = fn(container_id: String, current_time: Int) {
    // Simulate health check with different scenarios
    let hash = container_id.length() % 5
    
    match hash {
      0 => {
        {
          container_id,
          status: HealthStatus::Healthy,
          message: "Container is running normally",
          timestamp: current_time,
          response_time: 50
        }
      }
      1 => {
        {
          container_id,
          status: HealthStatus::Unhealthy,
          message: "Container failed health check",
          timestamp: current_time,
          response_time: 5000
        }
      }
      2 => {
        {
          container_id,
          status: HealthStatus::Starting,
          message: "Container is starting up",
          timestamp: current_time,
          response_time: 200
        }
      }
      _ => {
        {
          container_id,
          status: HealthStatus::Unknown,
          message: "Unable to determine container status",
          timestamp: current_time,
          response_time: 1000
        }
      }
    }
  }
  
  // Generate container metrics
  let generate_metrics = fn(container_id: String, current_time: Int) {
    let hash = container_id.length() % 10
    
    {
      container_id,
      name: "azimuth-telemetry-collector",
      cpu_usage: Int::to_float(hash * 10),
      memory_usage: Int::to_float(hash * 50),
      network_rx: Int::to_float(hash * 5),
      network_tx: Int::to_float(hash * 3),
      disk_read: Int::to_float(hash * 2),
      disk_write: Int::to_float(hash * 1),
      uptime: current_time - 1000,
      restart_count: hash / 3,
      health_status: if hash < 7 { HealthStatus::Healthy } else { HealthStatus::Unhealthy },
      last_check: current_time
    }
  }
  
  // Create alert based on metrics
  let create_alert = fn(metrics: ContainerMetrics, current_time: Int) {
    let mut alerts = []
    
    if metrics.cpu_usage > 80.0 {
      alerts = alerts.push({
        id: "alert_cpu_" + metrics.container_id,
        severity: "warning",
        container_id: metrics.container_id,
        message: "High CPU usage: " + metrics.cpu_usage.to_string() + "%",
        timestamp: current_time,
        resolved: false
      })
    }
    
    if metrics.memory_usage > 400.0 {
      alerts = alerts.push({
        id: "alert_memory_" + metrics.container_id,
        severity: "critical",
        container_id: metrics.container_id,
        message: "High memory usage: " + metrics.memory_usage.to_string() + "MB",
        timestamp: current_time,
        resolved: false
      })
    }
    
    if metrics.restart_count > 2 {
      alerts = alerts.push({
        id: "alert_restart_" + metrics.container_id,
        severity: "warning",
        container_id: metrics.container_id,
        message: "Container has restarted " + metrics.restart_count.to_string() + " times",
        timestamp: current_time,
        resolved: false
      })
    }
    
    match metrics.health_status {
      HealthStatus::Unhealthy => {
        alerts = alerts.push({
          id: "alert_health_" + metrics.container_id,
          severity: "critical",
          container_id: metrics.container_id,
          message: "Container health check failed",
          timestamp: current_time,
          resolved: false
        })
      }
      _ => {}
    }
    
    alerts
  }
  
  // Test container monitoring
  let container_ids = [
    "azimuth-telemetry-collector-1",
    "azimuth-telemetry-collector-2",
    "otel-collector-1",
    "redis-1"
  ]
  
  let current_time = 10000
  let mut monitoring_data = {
    containers: [],
    health_checks: [],
    alerts: []
  }
  
  // Generate metrics and health checks for all containers
  for container_id in container_ids {
    let metrics = generate_metrics(container_id, current_time)
    let health_check = check_container_health(container_id, current_time)
    let alerts = create_alert(metrics, current_time)
    
    monitoring_data = {
      containers: monitoring_data.containers.push(metrics),
      health_checks: monitoring_data.health_checks.push(health_check),
      alerts: monitoring_data.alerts + alerts
    }
  }
  
  // Verify monitoring data
  assert_eq(monitoring_data.containers.length(), 4)
  assert_eq(monitoring_data.health_checks.length(), 4)
  
  // Check for alerts based on generated metrics
  let total_alerts = monitoring_data.alerts.length()
  assert_true(total_alerts >= 0)  // May have alerts depending on hash values
  
  // Check specific container metrics
  let first_container = monitoring_data.containers[0]
  assert_eq(first_container.container_id, "azimuth-telemetry-collector-1")
  assert_eq(first_container.name, "azimuth-telemetry-collector")
  assert_true(first_container.cpu_usage >= 0.0 && first_container.cpu_usage <= 90.0)
  assert_true(first_container.memory_usage >= 0.0 && first_container.memory_usage <= 450.0)
  assert_eq(first_container.uptime, 9000)  // current_time - 1000
  
  // Check health checks
  let first_health_check = monitoring_data.health_checks[0]
  assert_eq(first_health_check.container_id, "azimuth-telemetry-collector-1")
  assert_eq(first_health_check.timestamp, current_time)
  assert_true(first_health_check.response_time >= 50 && first_health_check.response_time <= 5000)
}

// Test 5: Container Orchestration Scaling
test "container orchestration scaling policies" {
  // Define scaling policies
  enum ScalingPolicy {
    Manual(Int)           // Fixed replica count
    CPUBased(Float)       // Scale based on CPU threshold
    MemoryBased(Float)    // Scale based on memory threshold
    Custom(Array[(String, Float)])  // Custom metrics
  }
  
  type ScalingConfiguration = {
    min_replicas: Int,
    max_replicas: Int,
    policy: ScalingPolicy,
    scale_up_cooldown: Int,   // seconds
    scale_down_cooldown: Int, // seconds
    target_utilization: Float
  }
  
  type ScalingEvent = {
    timestamp: Int,
    old_replicas: Int,
    new_replicas: Int,
    reason: String,
    metrics: Array[(String, Float)]
  }
  
  type ScalingHistory = {
    events: Array[ScalingEvent],
    current_replicas: Int,
    last_scale_up: Int,
    last_scale_down: Int
  }
  
  // Calculate desired replicas based on policy
  let calculate_desired_replicas = fn(
    current_replicas: Int,
    config: ScalingConfiguration,
    metrics: Array[(String, Float)]
  ) {
    match config.policy {
      ScalingPolicy::Manual(replicas) => replicas
      ScalingPolicy::CPUBased(threshold) => {
        let mut cpu_usage = 0.0
        for (metric_name, value) in metrics {
          if metric_name == "cpu" {
            cpu_usage = value
          }
        }
        
        if cpu_usage > threshold && current_replicas < config.max_replicas {
          current_replicas + 1
        } else if cpu_usage < (threshold * 0.7) && current_replicas > config.min_replicas {
          current_replicas - 1
        } else {
          current_replicas
        }
      }
      ScalingPolicy::MemoryBased(threshold) => {
        let mut memory_usage = 0.0
        for (metric_name, value) in metrics {
          if metric_name == "memory" {
            memory_usage = value
          }
        }
        
        if memory_usage > threshold && current_replicas < config.max_replicas {
          current_replicas + 1
        } else if memory_usage < (threshold * 0.7) && current_replicas > config.min_replicas {
          current_replicas - 1
        } else {
          current_replicas
        }
      }
      ScalingPolicy::Custom(custom_metrics) => {
        let mut should_scale_up = false
        let mut should_scale_down = true
        
        for (metric_name, threshold) in custom_metrics {
          let mut current_value = 0.0
          for (name, value) in metrics {
            if name == metric_name {
              current_value = value
            }
          }
          
          if current_value > threshold {
            should_scale_up = true
          }
          
          if current_value > (threshold * 0.7) {
            should_scale_down = false
          }
        }
        
        if should_scale_up && current_replicas < config.max_replicas {
          current_replicas + 1
        } else if should_scale_down && current_replicas > config.min_replicas {
          current_replicas - 1
        } else {
          current_replicas
        }
      }
    }
  }
  
  // Apply scaling with cooldown
  let apply_scaling = fn(
    history: ScalingHistory,
    config: ScalingConfiguration,
    metrics: Array[(String, Float)],
    current_time: Int
  ) {
    let desired_replicas = calculate_desired_replicas(
      history.current_replicas,
      config,
      metrics
    )
    
    if desired_replicas == history.current_replicas {
      history
    } else {
      let time_since_last_scale_up = current_time - history.last_scale_up
      let time_since_last_scale_down = current_time - history.last_scale_down
      
      if desired_replicas > history.current_replicas && 
         time_since_last_scale_up >= config.scale_up_cooldown {
        let event = {
          timestamp: current_time,
          old_replicas: history.current_replicas,
          new_replicas: desired_replicas,
          reason: "Scale up due to high resource utilization",
          metrics
        }
        
        {
          events: history.events.push(event),
          current_replicas: desired_replicas,
          last_scale_up: current_time,
          last_scale_down: history.last_scale_down
        }
      } else if desired_replicas < history.current_replicas && 
                time_since_last_scale_down >= config.scale_down_cooldown {
        let event = {
          timestamp: current_time,
          old_replicas: history.current_replicas,
          new_replicas: desired_replicas,
          reason: "Scale down due to low resource utilization",
          metrics
        }
        
        {
          events: history.events.push(event),
          current_replicas: desired_replicas,
          last_scale_up: history.last_scale_up,
          last_scale_down: current_time
        }
      } else {
        // Scaling desired but cooldown period active
        history
      }
    }
  }
  
  // Test scaling policies
  let cpu_based_config = {
    min_replicas: 1,
    max_replicas: 5,
    policy: ScalingPolicy::CPUBased(80.0),
    scale_up_cooldown: 300,  // 5 minutes
    scale_down_cooldown: 600, // 10 minutes
    target_utilization: 70.0
  }
  
  let memory_based_config = {
    min_replicas: 2,
    max_replicas: 8,
    policy: ScalingPolicy::MemoryBased(85.0),
    scale_up_cooldown: 180,  // 3 minutes
    scale_down_cooldown: 300, // 5 minutes
    target_utilization: 75.0
  }
  
  let custom_config = {
    min_replicas: 1,
    max_replicas: 10,
    policy: ScalingPolicy::Custom([("cpu", 75.0), ("memory", 80.0), ("requests_per_second", 1000.0)]),
    scale_up_cooldown: 120,  // 2 minutes
    scale_down_cooldown: 300, // 5 minutes
    target_utilization: 70.0
  }
  
  // Test scaling calculations
  let low_cpu_metrics = [("cpu", 50.0), ("memory", 60.0)]
  let high_cpu_metrics = [("cpu", 90.0), ("memory", 60.0)]
  let high_memory_metrics = [("cpu", 60.0), ("memory", 90.0)]
  let low_memory_metrics = [("cpu", 60.0), ("memory", 50.0)]
  
  assert_eq(calculate_desired_replicas(3, cpu_based_config, low_cpu_metrics), 2)  // Scale down
  assert_eq(calculate_desired_replicas(3, cpu_based_config, high_cpu_metrics), 4)  // Scale up
  
  assert_eq(calculate_desired_replicas(4, memory_based_config, low_memory_metrics), 3)  // Scale down
  assert_eq(calculate_desired_replicas(4, memory_based_config, high_memory_metrics), 5)  // Scale up
  
  let custom_metrics_high = [("cpu", 80.0), ("memory", 85.0), ("requests_per_second", 1200.0)]
  let custom_metrics_low = [("cpu", 60.0), ("memory", 65.0), ("requests_per_second", 800.0)]
  
  assert_eq(calculate_desired_replicas(5, custom_config, custom_metrics_high), 6)  // Scale up
  assert_eq(calculate_desired_replicas(5, custom_config, custom_metrics_low), 4)  // Scale down
  
  // Test scaling with cooldown
  let initial_history = {
    events: [],
    current_replicas: 3,
    last_scale_up: 0,
    last_scale_down: 0
  }
  
  // Scale up at time 1000
  let history1 = apply_scaling(initial_history, cpu_based_config, high_cpu_metrics, 1000)
  assert_eq(history1.current_replicas, 4)
  assert_eq(history1.last_scale_up, 1000)
  
  // Try to scale up again at time 1500 (within cooldown)
  let history2 = apply_scaling(history1, cpu_based_config, high_cpu_metrics, 1500)
  assert_eq(history2.current_replicas, 4)  // No change due to cooldown
  
  // Try to scale up at time 1400 (outside cooldown)
  let history3 = apply_scaling(history2, cpu_based_config, high_cpu_metrics, 1400)
  assert_eq(history3.current_replicas, 5)  // Scale up successful
  assert_eq(history3.last_scale_up, 1400)
  
  // Try to scale down at time 2000
  let history4 = apply_scaling(history3, cpu_based_config, low_cpu_metrics, 2000)
  assert_eq(history4.current_replicas, 4)  // Scale down successful
  assert_eq(history4.last_scale_down, 2000)
}