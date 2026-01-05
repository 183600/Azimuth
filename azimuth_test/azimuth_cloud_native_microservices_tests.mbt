// Azimuth Cloud Native Microservices Test Suite
// This file contains test cases for cloud-native microservices telemetry functionality

// Test 1: Service Discovery and Registration
test "service discovery and registration" {
  // Define service registry structure
  type ServiceRegistry = {
    services: Array[ServiceInstance],
    health_checks: Array[HealthCheck]
  }
  
  type ServiceInstance = {
    id: String,
    name: String,
    host: String,
    port: Int,
    metadata: Array[(String, String)],
    status: String,
    last_heartbeat: Int
  }
  
  type HealthCheck = {
    service_id: String,
    endpoint: String,
    interval: Int,
    timeout: Int,
    healthy_threshold: Int,
    unhealthy_threshold: Int
  }
  
  // Create service registry
  let registry = {
    services: [],
    health_checks: []
  }
  
  // Register a new service
  let register_service = fn(registry: ServiceRegistry, service: ServiceInstance) {
    let updated_services = registry.services.push(service)
    { registry | services: updated_services }
  }
  
  let payment_service = {
    id: "payment-service-001",
    name: "payment-service",
    host: "payment-service.default.svc.cluster.local",
    port: 8080,
    metadata: [
      ("version", "1.2.3"),
      ("environment", "production"),
      ("region", "us-west-2")
    ],
    status: "healthy",
    last_heartbeat: 1640995200
  }
  
  let updated_registry = register_service(registry, payment_service)
  assert_eq(updated_registry.services.length(), 1)
  
  let registered_service = updated_registry.services[0]
  assert_eq(registered_service.name, "payment-service")
  assert_eq(registered_service.host, "payment-service.default.svc.cluster.local")
  assert_eq(registered_service.port, 8080)
  assert_eq(registered_service.status, "healthy")
  
  // Discover services by name
  let discover_services = fn(registry: ServiceRegistry, service_name: String) {
    registry.services.filter(fn(s) { s.name == service_name })
  }
  
  let payment_services = discover_services(updated_registry, "payment-service")
  assert_eq(payment_services.length(), 1)
  assert_eq(payment_services[0].id, "payment-service-001")
  
  // Discover services with metadata filter
  let discover_by_metadata = fn(registry: ServiceRegistry, key: String, value: String) {
    registry.services.filter(fn(s) {
      s.metadata.any(fn(pair) {
        let (k, v) = pair
        k == key and v == value
      })
    })
  }
  
  let prod_services = discover_by_metadata(updated_registry, "environment", "production")
  assert_eq(prod_services.length(), 1)
  assert_eq(prod_services[0].name, "payment-service")
  
  let staging_services = discover_by_metadata(updated_registry, "environment", "staging")
  assert_eq(staging_services.length(), 0)
}

// Test 2: Circuit Breaker Pattern
test "circuit breaker pattern implementation" {
  // Define circuit breaker states
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // Define circuit breaker structure
  type CircuitBreaker = {
    state: CircuitState,
    failure_count: Int,
    failure_threshold: Int,
    success_threshold: Int,
    timeout: Int,
    last_failure_time: Int,
    last_success_time: Int
  }
  
  // Create circuit breaker
  let create_circuit_breaker = fn(failure_threshold: Int, success_threshold: Int, timeout: Int) {
    {
      state: CircuitState::Closed,
      failure_count: 0,
      failure_threshold,
      success_threshold,
      timeout,
      last_failure_time: 0,
      last_success_time: 0
    }
  }
  
  let circuit_breaker = create_circuit_breaker(5, 3, 60000)
  assert_eq(circuit_breaker.state, CircuitState::Closed)
  assert_eq(circuit_breaker.failure_count, 0)
  assert_eq(circuit_breaker.failure_threshold, 5)
  
  // Simulate operation results
  let handle_result = fn(circuit_breaker: CircuitBreaker, success: Bool, current_time: Int) {
    match circuit_breaker.state {
      CircuitState::Closed => {
        if success {
          // Reset failure count on success
          { circuit_breaker | 
            failure_count: 0,
            last_success_time: current_time
          }
        } else {
          // Increment failure count
          let new_failure_count = circuit_breaker.failure_count + 1
          let new_state = if new_failure_count >= circuit_breaker.failure_threshold {
            CircuitState::Open
          } else {
            CircuitState::Closed
          }
          
          { circuit_breaker |
            failure_count: new_failure_count,
            state: new_state,
            last_failure_time: current_time
          }
        }
      }
      
      CircuitState::Open => {
        // Check if timeout has elapsed
        if current_time - circuit_breaker.last_failure_time > circuit_breaker.timeout {
          // Transition to half-open
          { circuit_breaker | state: CircuitState::HalfOpen }
        } else {
          // Stay open
          circuit_breaker
        }
      }
      
      CircuitState::HalfOpen => {
        if success {
          // Check if we've reached success threshold
          // In a real implementation, we'd track consecutive successes
          { circuit_breaker |
            state: CircuitState::Closed,
            failure_count: 0,
            last_success_time: current_time
          }
        } else {
          // Back to open state
          { circuit_breaker |
            state: CircuitState::Open,
            failure_count: circuit_breaker.failure_threshold,
            last_failure_time: current_time
          }
        }
      }
    }
  }
  
  // Test normal operation (success)
  let cb1 = handle_result(circuit_breaker, true, 1640995200)
  assert_eq(cb1.state, CircuitState::Closed)
  assert_eq(cb1.failure_count, 0)
  
  // Test failures
  let cb2 = handle_result(cb1, false, 1640995300)
  assert_eq(cb2.state, CircuitState::Closed)
  assert_eq(cb2.failure_count, 1)
  
  let cb3 = handle_result(cb2, false, 1640995400)
  assert_eq(cb3.state, CircuitState::Closed)
  assert_eq(cb3.failure_count, 2)
  
  let cb4 = handle_result(cb3, false, 1640995500)
  assert_eq(cb4.state, CircuitState::Closed)
  assert_eq(cb4.failure_count, 3)
  
  let cb5 = handle_result(cb4, false, 1640995600)
  assert_eq(cb5.state, CircuitState::Closed)
  assert_eq(cb5.failure_count, 4)
  
  // This failure should trip the circuit breaker
  let cb6 = handle_result(cb5, false, 1640995700)
  assert_eq(cb6.state, CircuitState::Open)
  assert_eq(cb6.failure_count, 5)
  
  // Test timeout transition to half-open
  let cb7 = handle_result(cb6, true, 1640995700 + 70000)  // After timeout
  assert_eq(cb7.state, CircuitState::HalfOpen)
  
  // Test successful recovery
  let cb8 = handle_result(cb7, true, 1640995700 + 80000)
  assert_eq(cb8.state, CircuitState::Closed)
  assert_eq(cb8.failure_count, 0)
  
  // Test failed recovery (back to open)
  let cb9 = handle_result(cb7, false, 1640995700 + 80000)
  assert_eq(cb9.state, CircuitState::Open)
  assert_eq(cb9.failure_count, 5)
}

// Test 3: Distributed Configuration Management
test "distributed configuration management" {
  // Define configuration structure
  type Configuration = {
    key: String,
    value: String,
    version: Int,
    environment: String,
    service: String,
    metadata: Array[(String, String)],
    created_at: Int,
    updated_at: Int
  }
  
  type ConfigStore = {
    configurations: Array[Configuration],
    watchers: Array[ConfigWatcher]
  }
  
  type ConfigWatcher = {
    id: String,
    service: String,
    pattern: String,
    callback: (Configuration) -> Unit
  }
  
  // Create config store
  let config_store = {
    configurations: [],
    watchers: []
  }
  
  // Add configuration
  let add_config = fn(store: ConfigStore, config: Configuration) {
    let updated_configs = store.configurations.push(config)
    { store | configurations: updated_configs }
  }
  
  let db_config = {
    key: "database.connection_string",
    value: "postgresql://user:pass@db:5432/azimuth",
    version: 1,
    environment: "production",
    service: "*",
    metadata: [
      ("type", "string"),
      ("sensitive", "true"),
      ("description", "Database connection string")
    ],
    created_at: 1640995200,
    updated_at: 1640995200
  }
  
  let updated_store = add_config(config_store, db_config)
  assert_eq(updated_store.configurations.length(), 1)
  
  // Get configuration by key
  let get_config = fn(store: ConfigStore, key: String, environment: String, service: String) {
    store.configurations.filter(fn(c) {
      c.key == key and 
      (c.environment == environment or c.environment == "*") and
      (c.service == service or c.service == "*")
    })
  }
  
  let configs = get_config(updated_store, "database.connection_string", "production", "payment-service")
  assert_eq(configs.length(), 1)
  assert_eq(configs[0].value, "postgresql://user:pass@db:5432/azimuth")
  
  // Get configuration with environment override
  let dev_db_config = {
    key: "database.connection_string",
    value: "postgresql://dev_user:dev_pass@dev-db:5432/azimuth_dev",
    version: 1,
    environment: "development",
    service: "*",
    metadata: [
      ("type", "string"),
      ("sensitive", "true"),
      ("description", "Database connection string for development")
    ],
    created_at: 1640995200,
    updated_at: 1640995200
  }
  
  let store_with_dev = add_config(updated_store, dev_db_config)
  
  // Production service should get production config
  let prod_configs = get_config(store_with_dev, "database.connection_string", "production", "payment-service")
  assert_eq(prod_configs.length(), 1)
  assert_eq(prod_configs[0].value, "postgresql://user:pass@db:5432/azimuth")
  
  // Development service should get development config
  let dev_configs = get_config(store_with_dev, "database.connection_string", "development", "payment-service")
  assert_eq(dev_configs.length(), 1)
  assert_eq(dev_configs[0].value, "postgresql://dev_user:dev_pass@dev-db:5432/azimuth_dev")
  
  // Update configuration
  let update_config = fn(store: ConfigStore, key: String, new_value: String, environment: String, service: String, current_time: Int) {
    let mut updated = false
    let updated_configs = store.configurations.map(fn(c) {
      if c.key == key and c.environment == environment and c.service == service {
        updated = true
        { c |
          value: new_value,
          version: c.version + 1,
          updated_at: current_time
        }
      } else {
        c
      }
    })
    
    if updated {
      { store | configurations: updated_configs }
    } else {
      store
    }
  }
  
  let store_with_update = update_config(
    store_with_dev, 
    "database.connection_string", 
    "postgresql://user:updated_pass@db:5432/azimuth", 
    "production", 
    "*", 
    1640995300
  )
  
  let updated_configs = get_config(store_with_update, "database.connection_string", "production", "payment-service")
  assert_eq(updated_configs.length(), 1)
  assert_eq(updated_configs[0].value, "postgresql://user:updated_pass@db:5432/azimuth")
  assert_eq(updated_configs[0].version, 2)
  assert_eq(updated_configs[0].updated_at, 1640995300)
}

// Test 4: Service Mesh Telemetry
test "service mesh telemetry collection" {
  // Define service mesh telemetry structure
  type MeshTelemetry = {
    request_id: String,
    trace_id: String,
    source_service: String,
    source_pod: String,
    destination_service: String,
    destination_pod: String,
    request_path: String,
    method: String,
    status_code: Int,
    duration: Int,
    bytes_sent: Int,
    bytes_received: Int,
    timestamp: Int,
    flags: Array[String]
  }
  
  type MeshMetrics = {
    total_requests: Int,
    success_rate: Float,
    error_rate: Float,
    avg_response_time: Float,
    p95_response_time: Float,
    p99_response_time: Float,
    throughput: Float,
    error_count: Int
  }
  
  // Create mesh telemetry records
  let telemetry_data = [
    {
      request_id: "req-001",
      trace_id: "trace-001",
      source_service: "frontend",
      source_pod: "frontend-7d4f8c9b9-abc123",
      destination_service: "payment-service",
      destination_pod: "payment-service-6f5d8c7a8-def456",
      request_path: "/api/payments",
      method: "POST",
      status_code: 200,
      duration: 250,
      bytes_sent: 1024,
      bytes_received: 2048,
      timestamp: 1640995200,
      flags: ["encrypted", "authenticated"]
    },
    {
      request_id: "req-002",
      trace_id: "trace-002",
      source_service: "frontend",
      source_pod: "frontend-7d4f8c9b9-abc123",
      destination_service: "payment-service",
      destination_pod: "payment-service-6f5d8c7a8-def456",
      request_path: "/api/payments",
      method: "POST",
      status_code: 500,
      duration: 5000,
      bytes_sent: 1024,
      bytes_received: 512,
      timestamp: 1640995210,
      flags: ["encrypted", "authenticated"]
    },
    {
      request_id: "req-003",
      trace_id: "trace-003",
      source_service: "frontend",
      source_pod: "frontend-7d4f8c9b9-abc123",
      destination_service: "user-service",
      destination_pod: "user-service-8e5g9d8b9-ghi789",
      request_path: "/api/users/123",
      method: "GET",
      status_code: 200,
      duration: 150,
      bytes_sent: 512,
      bytes_received: 1024,
      timestamp: 1640995220,
      flags: ["encrypted", "authenticated"]
    }
  ]
  
  // Calculate metrics for a specific service
  let calculate_metrics = fn(telemetry: Array[MeshTelemetry], service_name: String) {
    let service_telemetry = telemetry.filter(fn(t) { t.destination_service == service_name })
    
    let total_requests = service_telemetry.length()
    let success_count = service_telemetry.filter(fn(t) { t.status_code >= 200 and t.status_code < 300 }).length()
    let error_count = service_telemetry.filter(fn(t) { t.status_code >= 400 }).length()
    
    let success_rate = if total_requests > 0 {
      success_count.to_float() / total_requests.to_float() * 100.0
    } else {
      0.0
    }
    
    let error_rate = if total_requests > 0 {
      error_count.to_float() / total_requests.to_float() * 100.0
    } else {
      0.0
    }
    
    let durations = service_telemetry.map(fn(t) { t.duration })
    let total_duration = durations.reduce(fn(acc, d) { acc + d }, 0)
    let avg_response_time = if total_requests > 0 {
      total_duration.to_float() / total_requests.to_float()
    } else {
      0.0
    }
    
    // Simplified percentile calculation
    let sorted_durations = durations.sort(fn(a, b) { a - b })
    let p95_response_time = if total_requests > 0 {
      sorted_durations[(total_requests * 95) / 100].to_float()
    } else {
      0.0
    }
    
    let p99_response_time = if total_requests > 0 {
      sorted_durations[(total_requests * 99) / 100].to_float()
    } else {
      0.0
    }
    
    // Calculate throughput (requests per second)
    let timestamps = service_telemetry.map(fn(t) { t.timestamp })
    let min_timestamp = timestamps.reduce(fn(acc, ts) { if ts < acc { ts } else { acc } }, 9999999999)
    let max_timestamp = timestamps.reduce(fn(acc, ts) { if ts > acc { ts } else { acc } }, 0)
    let time_window = if max_timestamp > min_timestamp { max_timestamp - min_timestamp } else { 1 }
    let throughput = total_requests.to_float() / time_window.to_float()
    
    {
      total_requests,
      success_rate,
      error_rate,
      avg_response_time,
      p95_response_time,
      p99_response_time,
      throughput,
      error_count
    }
  }
  
  // Calculate metrics for payment service
  let payment_metrics = calculate_metrics(telemetry_data, "payment-service")
  assert_eq(payment_metrics.total_requests, 2)
  assert_eq(payment_metrics.error_count, 1)
  assert_eq(payment_metrics.success_rate, 50.0)
  assert_eq(payment_metrics.error_rate, 50.0)
  assert_eq(payment_metrics.avg_response_time, 2625.0)  // (250 + 5000) / 2
  
  // Calculate metrics for user service
  let user_metrics = calculate_metrics(telemetry_data, "user-service")
  assert_eq(user_metrics.total_requests, 1)
  assert_eq(user_metrics.error_count, 0)
  assert_eq(user_metrics.success_rate, 100.0)
  assert_eq(user_metrics.error_rate, 0.0)
  assert_eq(user_metrics.avg_response_time, 150.0)
  
  // Filter telemetry by flags
  let filter_by_flags = fn(telemetry: Array[MeshTelemetry], required_flags: Array[String]) {
    telemetry.filter(fn(t) {
      required_flags.all(fn(flag) { t.flags.contains(flag) })
    })
  }
  
  let encrypted_telemetry = filter_by_flags(telemetry_data, ["encrypted"])
  assert_eq(encrypted_telemetry.length(), 3)
  
  let encrypted_authenticated = filter_by_flags(telemetry_data, ["encrypted", "authenticated"])
  assert_eq(encrypted_authenticated.length(), 3)
  
  let non_existent_flag = filter_by_flags(telemetry_data, ["non-existent"])
  assert_eq(non_existent_flag.length(), 0)
}

// Test 5: Container Orchestration Telemetry
test "container orchestration telemetry" {
  // Define container and pod telemetry structure
  type ContainerMetrics = {
    container_id: String,
    pod_name: String,
    namespace: String,
    node_name: String,
    cpu_usage_cores: Float,
    memory_usage_bytes: Int,
    cpu_limit_cores: Float,
    memory_limit_bytes: Int,
    restart_count: Int,
    status: String,
    timestamp: Int
  }
  
  type PodMetrics = {
    pod_name: String,
    namespace: String,
    node_name: String,
    phase: String,
    pod_ip: String,
    containers: Array[ContainerMetrics],
    timestamp: Int
  }
  
  type NodeMetrics = {
    node_name: String,
    cpu_capacity_cores: Float,
    memory_capacity_bytes: Int,
    cpu_allocatable_cores: Float,
    memory_allocatable_bytes: Int,
    pods_running: Int,
    pods_capacity: Int,
    timestamp: Int
  }
  
  // Create sample container metrics
  let container_metrics = [
    {
      container_id: "container-001",
      pod_name: "payment-service-6f5d8c7a8-def456",
      namespace: "default",
      node_name: "worker-node-1",
      cpu_usage_cores: 0.25,
      memory_usage_bytes: 268435456,  // 256 MB
      cpu_limit_cores: 1.0,
      memory_limit_bytes: 1073741824,  // 1 GB
      restart_count: 0,
      status: "running",
      timestamp: 1640995200
    },
    {
      container_id: "container-002",
      pod_name: "user-service-8e5g9d8b9-ghi789",
      namespace: "default",
      node_name: "worker-node-2",
      cpu_usage_cores: 0.15,
      memory_usage_bytes: 134217728,  // 128 MB
      cpu_limit_cores: 0.5,
      memory_limit_bytes: 536870912,  // 512 MB
      restart_count: 2,
      status: "running",
      timestamp: 1640995200
    },
    {
      container_id: "container-003",
      pod_name: "frontend-7d4f8c9b9-abc123",
      namespace: "default",
      node_name: "worker-node-1",
      cpu_usage_cores: 0.4,
      memory_usage_bytes: 335544320,  // 320 MB
      cpu_limit_cores: 1.0,
      memory_limit_bytes: 1073741824,  // 1 GB
      restart_count: 1,
      status: "running",
      timestamp: 1640995200
    }
  ]
  
  // Group containers by node
  let group_by_node = fn(containers: Array[ContainerMetrics]) {
    let mut grouped = []
    let mut processed_nodes = []
    
    for container in containers {
      if not(processed_nodes.contains(container.node_name)) {
        processed_nodes = processed_nodes.push(container.node_name)
        let node_containers = containers.filter(fn(c) { c.node_name == container.node_name })
        grouped = grouped.push((container.node_name, node_containers))
      }
    }
    
    grouped
  }
  
  let node_groups = group_by_node(container_metrics)
  assert_eq(node_groups.length(), 2)
  
  let worker_node_1_containers = node_groups.filter(fn(g) { g.0 == "worker-node-1" })[0].1
  assert_eq(worker_node_1_containers.length(), 2)
  
  let worker_node_2_containers = node_groups.filter(fn(g) { g.0 == "worker-node-2" })[0].1
  assert_eq(worker_node_2_containers.length(), 1)
  
  // Calculate resource utilization by node
  let calculate_node_utilization = fn(node_name: String, containers: Array[ContainerMetrics]) {
    let node_containers = containers.filter(fn(c) { c.node_name == node_name })
    
    let total_cpu_usage = node_containers.reduce(fn(acc, c) { acc + c.cpu_usage_cores }, 0.0)
    let total_memory_usage = node_containers.reduce(fn(acc, c) { acc + c.memory_usage_bytes }, 0)
    let total_cpu_limit = node_containers.reduce(fn(acc, c) { acc + c.cpu_limit_cores }, 0.0)
    let total_memory_limit = node_containers.reduce(fn(acc, c) { acc + c.memory_limit_bytes }, 0)
    let total_restarts = node_containers.reduce(fn(acc, c) { acc + c.restart_count }, 0)
    
    let cpu_utilization = if total_cpu_limit > 0.0 {
      (total_cpu_usage / total_cpu_limit) * 100.0
    } else {
      0.0
    }
    
    let memory_utilization = if total_memory_limit > 0 {
      (total_memory_usage.to_float() / total_memory_limit.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      node_name,
      container_count: node_containers.length(),
      total_cpu_usage,
      total_memory_usage,
      total_cpu_limit,
      total_memory_limit,
      cpu_utilization,
      memory_utilization,
      total_restarts
    }
  }
  
  let node1_utilization = calculate_node_utilization("worker-node-1", container_metrics)
  assert_eq(node1_utilization.node_name, "worker-node-1")
  assert_eq(node1_utilization.container_count, 2)
  assert_eq(node1_utilization.total_cpu_usage, 0.65)  // 0.25 + 0.4
  assert_eq(node1_utilization.total_cpu_limit, 2.0)    // 1.0 + 1.0
  assert_eq(node1_utilization.cpu_utilization, 32.5)  // (0.65 / 2.0) * 100
  assert_eq(node1_utilization.total_restarts, 1)       // 0 + 1
  
  let node2_utilization = calculate_node_utilization("worker-node-2", container_metrics)
  assert_eq(node2_utilization.node_name, "worker-node-2")
  assert_eq(node2_utilization.container_count, 1)
  assert_eq(node2_utilization.total_cpu_usage, 0.15)
  assert_eq(node2_utilization.total_cpu_limit, 0.5)
  assert_eq(node2_utilization.cpu_utilization, 30.0)   // (0.15 / 0.5) * 100
  assert_eq(node2_utilization.total_restarts, 2)
  
  // Find containers with high restart count
  let find_unhealthy_containers = fn(containers: Array[ContainerMetrics], restart_threshold: Int) {
    containers.filter(fn(c) { c.restart_count >= restart_threshold })
  }
  
  let unhealthy_containers = find_unhealthy_containers(container_metrics, 2)
  assert_eq(unhealthy_containers.length(), 1)
  assert_eq(unhealthy_containers[0].container_id, "container-002")
  assert_eq(unhealthy_containers[0].pod_name, "user-service-8e5g9d8b9-ghi789")
  
  // Find containers approaching resource limits
  let find_containers_near_limits = fn(containers: Array[ContainerMetrics], cpu_threshold: Float, memory_threshold: Float) {
    containers.filter(fn(c) {
      let cpu_usage_ratio = c.cpu_usage_cores / c.cpu_limit_cores
      let memory_usage_ratio = c.memory_usage_bytes.to_float() / c.memory_limit_bytes.to_float()
      
      cpu_usage_ratio >= cpu_threshold or memory_usage_ratio >= memory_threshold
    })
  }
  
  // Find containers using more than 30% CPU or 50% memory
  let near_limit_containers = find_containers_near_limits(container_metrics, 0.3, 0.5)
  assert_eq(near_limit_containers.length(), 1)
  assert_eq(near_limit_containers[0].container_id, "container-003")
  assert_eq(near_limit_containers[0].cpu_usage_cores, 0.4)
  assert_eq(near_limit_containers[0].cpu_limit_cores, 1.0)
}

// Test 6: Microservice Resilience Patterns
test "microservice resilience patterns" {
  // Define retry policy
  type RetryPolicy = {
    max_attempts: Int,
    initial_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float,
    retryable_errors: Array[Int]
  }
  
  // Define bulkhead pattern
  type Bulkhead = {
    name: String,
    max_concurrent: Int,
    max_queue: Int,
    active_count: Int,
    queue_count: Int,
    rejected_count: Int
  }
  
  // Define timeout configuration
  type TimeoutConfig = {
    connect_timeout_ms: Int,
    read_timeout_ms: Int,
    write_timeout_ms: Int,
    total_timeout_ms: Int
  }
  
  // Create retry policy
  let retry_policy = {
    max_attempts: 3,
    initial_delay_ms: 100,
    max_delay_ms: 1000,
    backoff_multiplier: 2.0,
    retryable_errors: [408, 429, 500, 502, 503, 504]
  }
  
  // Calculate retry delay
  let calculate_retry_delay = fn(attempt: Int, policy: RetryPolicy) {
    if attempt <= 1 {
      policy.initial_delay_ms
    } else {
      let delay = policy.initial_delay_ms.to_float() * 
                  policy.backoff_multiplier.pow(attempt - 1)
      delay.to_int().min(policy.max_delay_ms)
    }
  }
  
  // Test retry delay calculation
  assert_eq(calculate_retry_delay(1, retry_policy), 100)
  assert_eq(calculate_retry_delay(2, retry_policy), 200)
  assert_eq(calculate_retry_delay(3, retry_policy), 400)
  
  // Test with max delay cap
  let aggressive_policy = {
    max_attempts: 5,
    initial_delay_ms: 100,
    max_delay_ms: 300,
    backoff_multiplier: 3.0,
    retryable_errors: [500, 502, 503]
  }
  
  assert_eq(calculate_retry_delay(1, aggressive_policy), 100)
  assert_eq(calculate_retry_delay(2, aggressive_policy), 300)
  assert_eq(calculate_retry_delay(3, aggressive_policy), 300)  // Capped at max
  
  // Check if error is retryable
  let is_retryable_error = fn(status_code: Int, policy: RetryPolicy) {
    policy.retryable_errors.contains(status_code)
  }
  
  assert_true(is_retryable_error(500, retry_policy))
  assert_true(is_retryable_error(429, retry_policy))
  assert_false(is_retryable_error(404, retry_policy))
  assert_false(is_retryable_error(400, retry_policy))
  
  // Simulate retry logic
  let should_retry = fn(attempt: Int, status_code: Int, policy: RetryPolicy) {
    attempt < policy.max_attempts and is_retryable_error(status_code, policy)
  }
  
  assert_true(should_retry(1, 500, retry_policy))
  assert_true(should_retry(2, 503, retry_policy))
  assert_false(should_retry(3, 500, retry_policy))  // Max attempts reached
  assert_false(should_retry(1, 404, retry_policy))   // Non-retryable error
  
  // Create bulkhead
  let create_bulkhead = fn(name: String, max_concurrent: Int, max_queue: Int) {
    {
      name,
      max_concurrent,
      max_queue,
      active_count: 0,
      queue_count: 0,
      rejected_count: 0
    }
  }
  
  let payment_bulkhead = create_bulkhead("payment-service", 10, 50)
  assert_eq(payment_bulkhead.name, "payment-service")
  assert_eq(payment_bulkhead.max_concurrent, 10)
  assert_eq(payment_bulkhead.max_queue, 50)
  assert_eq(payment_bulkhead.active_count, 0)
  assert_eq(payment_bulkhead.queue_count, 0)
  assert_eq(payment_bulkhead.rejected_count, 0)
  
  // Simulate acquiring resources from bulkhead
  let acquire_resource = fn(bulkhead: Bulkhead) {
    if bulkhead.active_count < bulkhead.max_concurrent {
      // Can execute immediately
      { bulkhead | active_count: bulkhead.active_count + 1 }
    } else if bulkhead.queue_count < bulkhead.max_queue {
      // Queue the request
      { bulkhead | queue_count: bulkhead.queue_count + 1 }
    } else {
      // Reject the request
      { bulkhead | rejected_count: bulkhead.rejected_count + 1 }
    }
  }
  
  // Simulate releasing resources from bulkhead
  let release_resource = fn(bulkhead: Bulkhead) {
    if bulkhead.active_count > 0 {
      if bulkhead.queue_count > 0 {
        // Process queued request
        { bulkhead | 
          queue_count: bulkhead.queue_count - 1
        }
      } else {
        // Just decrement active count
        { bulkhead | active_count: bulkhead.active_count - 1 }
      }
    } else {
      bulkhead
    }
  }
  
  // Test bulkhead resource acquisition and release
  let bh1 = acquire_resource(payment_bulkhead)
  assert_eq(bh1.active_count, 1)
  assert_eq(bh1.queue_count, 0)
  assert_eq(bh1.rejected_count, 0)
  
  // Fill up to capacity
  let bh2 = { bh1 | active_count: 10 }  // Simulate reaching max concurrent
  let bh3 = acquire_resource(bh2)
  assert_eq(bh3.active_count, 10)       // Still at max
  assert_eq(bh3.queue_count, 1)         // Queued
  assert_eq(bh3.rejected_count, 0)
  
  // Fill up queue
  let bh4 = { bh3 | queue_count: 50 }  // Simulate reaching max queue
  let bh5 = acquire_resource(bh4)
  assert_eq(bh5.active_count, 10)       // Still at max
  assert_eq(bh5.queue_count, 50)        // Still at max
  assert_eq(bh5.rejected_count, 1)      // Rejected
  
  // Release resource and process queued request
  let bh6 = release_resource(bh5)
  assert_eq(bh6.active_count, 10)       // Still at max (queued request processed)
  assert_eq(bh6.queue_count, 49)        // One less in queue
  assert_eq(bh6.rejected_count, 1)      // Unchanged
  
  // Create timeout configuration
  let timeout_config = {
    connect_timeout_ms: 5000,
    read_timeout_ms: 10000,
    write_timeout_ms: 10000,
    total_timeout_ms: 30000
  }
  
  // Calculate effective timeout based on operation type
  let get_timeout = fn(config: TimeoutConfig, operation: String) {
    match operation {
      "connect" => config.connect_timeout_ms
      "read" => config.read_timeout_ms
      "write" => config.write_timeout_ms
      "total" => config.total_timeout_ms
      _ => config.total_timeout_ms  // Default
    }
  }
  
  assert_eq(get_timeout(timeout_config, "connect"), 5000)
  assert_eq(get_timeout(timeout_config, "read"), 10000)
  assert_eq(get_timeout(timeout_config, "write"), 10000)
  assert_eq(get_timeout(timeout_config, "total"), 30000)
  assert_eq(get_timeout(timeout_config, "unknown"), 30000)  // Default
  
  // Check if operation timed out
  let is_timeout = fn(start_time: Int, current_time: Int, timeout_ms: Int) {
    current_time - start_time > timeout_ms
  }
  
  assert_false(is_timeout(1640995200, 1640995250, 10000))  // 50ms < 10s
  assert_true(is_timeout(1640995200, 1640995300, 500))     // 100ms > 500ms
  assert_false(is_timeout(1640995200, 1640995210, 100))    // 10ms < 100ms
  assert_true(is_timeout(1640995200, 1640995210, 5))       // 10ms > 5ms
}

// Test 7: Distributed Tracing in Microservices
test "distributed tracing in microservices" {
  // Define trace span structure
  type TraceSpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    service_name: String,
    start_time: Int,
    end_time: Int,
    duration: Int,
    status: String,
    tags: Array[(String, String)],
    logs: Array[SpanLog],
    resource: Array[(String, String)]
  }
  
  type SpanLog = {
    timestamp: Int,
    level: String,
    message: String,
    fields: Array[(String, String)]
  }
  
  type TraceContext = {
    trace_id: String,
    span_id: String,
    baggage: Array[(String, String)]
  }
  
  // Create trace spans
  let spans = [
    {
      trace_id: "trace-001",
      span_id: "span-001",
      parent_span_id: None,
      operation_name: "HTTP GET /api/orders/123",
      service_name: "frontend",
      start_time: 1640995200000,  // Microseconds
      end_time: 1640995200500,
      duration: 500,
      status: "ok",
      tags: [
        ("http.method", "GET"),
        ("http.url", "/api/orders/123"),
        ("http.status_code", "200"),
        ("user.id", "user-123")
      ],
      logs: [
        {
          timestamp: 1640995200100,
          level: "info",
          message: "Request started",
          fields: [
            ("request.id", "req-001"),
            ("client.ip", "192.168.1.100")
          ]
        },
        {
          timestamp: 1640995200450,
          level: "info",
          message: "Request completed",
          fields: [
            ("response.size", "1024"),
            ("response.time", "500")
          ]
        }
      ],
      resource: [
        ("service.name", "frontend"),
        ("service.version", "1.2.3"),
        ("deployment.environment", "production")
      ]
    },
    {
      trace_id: "trace-001",
      span_id: "span-002",
      parent_span_id: Some("span-001"),
      operation_name: "HTTP GET /orders/123",
      service_name: "order-service",
      start_time: 1640995200100,
      end_time: 1640995200300,
      duration: 200,
      status: "ok",
      tags: [
        ("http.method", "GET"),
        ("http.url", "/orders/123"),
        ("http.status_code", "200"),
        ("db.query", "SELECT * FROM orders WHERE id = 123")
      ],
      logs: [
        {
          timestamp: 1640995200150,
          level: "debug",
          message: "Database query executed",
          fields: [
            ("db.statement", "SELECT * FROM orders WHERE id = 123"),
            ("db.duration", "150")
          ]
        }
      ],
      resource: [
        ("service.name", "order-service"),
        ("service.version", "2.1.0"),
        ("deployment.environment", "production")
      ]
    },
    {
      trace_id: "trace-001",
      span_id: "span-003",
      parent_span_id: Some("span-001"),
      operation_name: "HTTP GET /users/123",
      service_name: "user-service",
      start_time: 1640995200200,
      end_time: 1640995200400,
      duration: 200,
      status: "ok",
      tags: [
        ("http.method", "GET"),
        ("http.url", "/users/123"),
        ("http.status_code", "200"),
        ("cache.hit", "true")
      ],
      logs: [
        {
          timestamp: 1640995200250,
          level: "debug",
          message: "Cache hit",
          fields: [
            ("cache.key", "user:123"),
            ("cache.ttl", "300")
          ]
        }
      ],
      resource: [
        ("service.name", "user-service"),
        ("service.version", "1.5.2"),
        ("deployment.environment", "production")
      ]
    }
  ]
  
  // Build trace tree
  let build_trace_tree = fn(spans: Array[TraceSpan]) {
    let root_spans = spans.filter(fn(s) { s.parent_span_id.is_none() })
    
    let find_children = fn(parent_id: String) {
      spans.filter(fn(s) { 
        match s.parent_span_id {
          Some(id) => id == parent_id
          None => false
        }
      })
    }
    
    let build_tree = fn(span: TraceSpan) {
      let children = find_children(span.span_id)
      let child_trees = children.map(build_tree)
      (span, child_trees)
    }
    
    root_spans.map(build_tree)
  }
  
  let trace_tree = build_trace_tree(spans)
  assert_eq(trace_tree.length(), 1)
  
  let (root_span, child_spans) = trace_tree[0]
  assert_eq(root_span.span_id, "span-001")
  assert_eq(root_span.service_name, "frontend")
  assert_eq(child_spans.length(), 2)
  
  let (order_span, order_children) = child_spans[0]
  assert_eq(order_span.span_id, "span-002")
  assert_eq(order_span.service_name, "order-service")
  assert_eq(order_children.length(), 0)
  
  let (user_span, user_children) = child_spans[1]
  assert_eq(user_span.span_id, "span-003")
  assert_eq(user_span.service_name, "user-service")
  assert_eq(user_children.length(), 0)
  
  // Calculate trace statistics
  let calculate_trace_stats = fn(spans: Array[TraceSpan]) {
    let total_spans = spans.length()
    let services = spans.map(fn(s) { s.service_name })
    let unique_services = {
      let mut result = []
      for service in services {
        if not(result.contains(service)) {
          result = result.push(service)
        }
      }
      result
    }
    
    let durations = spans.map(fn(s) { s.duration })
    let total_duration = durations.reduce(fn(acc, d) { acc + d }, 0)
    let avg_duration = if total_spans > 0 {
      total_duration / total_spans
    } else {
      0
    }
    
    let sorted_durations = durations.sort(fn(a, b) { a - b })
    let max_duration = if total_spans > 0 { sorted_durations[total_spans - 1] } else { 0 }
    let min_duration = if total_spans > 0 { sorted_durations[0] } else { 0 }
    
    let error_spans = spans.filter(fn(s) { s.status != "ok" })
    let error_rate = if total_spans > 0 {
      (error_spans.length().to_float() / total_spans.to_float()) * 100.0
    } else {
      0.0
    }
    
    {
      trace_id: spans[0].trace_id,
      total_spans,
      unique_services: unique_services.length(),
      total_duration,
      avg_duration,
      min_duration,
      max_duration,
      error_rate
    }
  }
  
  let trace_stats = calculate_trace_stats(spans)
  assert_eq(trace_stats.trace_id, "trace-001")
  assert_eq(trace_stats.total_spans, 3)
  assert_eq(trace_stats.unique_services, 3)
  assert_eq(trace_stats.total_duration, 900)
  assert_eq(trace_stats.avg_duration, 300)
  assert_eq(trace_stats.min_duration, 200)
  assert_eq(trace_stats.max_duration, 500)
  assert_eq(trace_stats.error_rate, 0.0)
  
  // Find spans by service
  let find_spans_by_service = fn(spans: Array[TraceSpan], service_name: String) {
    spans.filter(fn(s) { s.service_name == service_name })
  }
  
  let frontend_spans = find_spans_by_service(spans, "frontend")
  assert_eq(frontend_spans.length(), 1)
  assert_eq(frontend_spans[0].span_id, "span-001")
  
  let order_spans = find_spans_by_service(spans, "order-service")
  assert_eq(order_spans.length(), 1)
  assert_eq(order_spans[0].span_id, "span-002")
  
  // Find spans with specific tags
  let find_spans_by_tag = fn(spans: Array[TraceSpan], key: String, value: String) {
    spans.filter(fn(s) {
      s.tags.any(fn(tag) {
        let (k, v) = tag
        k == key and v == value
      })
    })
  }
  
  let get_spans = find_spans_by_tag(spans, "http.method", "GET")
  assert_eq(get_spans.length(), 3)
  
  let cache_hit_spans = find_spans_by_tag(spans, "cache.hit", "true")
  assert_eq(cache_hit_spans.length(), 1)
  assert_eq(cache_hit_spans[0].span_id, "span-003")
  
  // Extract trace context for propagation
  let extract_trace_context = fn(span: TraceSpan) {
    {
      trace_id: span.trace_id,
      span_id: span.span_id,
      baggage: []
    }
  }
  
  let context = extract_trace_context(spans[0])
  assert_eq(context.trace_id, "trace-001")
  assert_eq(context.span_id, "span-001")
}

// Test 8: API Gateway Telemetry
test "api gateway telemetry" {
  // Define API gateway request/response structure
  type GatewayRequest = {
    request_id: String,
    method: String,
    path: String,
    query_params: Array[(String, String)],
    headers: Array[(String, String)],
    client_ip: String,
    user_agent: String,
    timestamp: Int,
    api_key: Option[String],
    auth_token: Option[String]
  }
  
  type GatewayResponse = {
    request_id: String,
    status_code: Int,
    response_size: Int,
    headers: Array[(String, String)],
    duration: Int,
    upstream_service: String,
    upstream_duration: Int,
    cache_hit: Bool,
    timestamp: Int
  }
  
  type GatewayMetrics = {
    total_requests: Int,
    requests_by_method: Array[(String, Int)],
    requests_by_path: Array[(String, Int)],
    requests_by_status: Array[(Int, Int)],
    avg_response_time: Float,
    error_rate: Float,
    cache_hit_rate: Float,
    top_clients: Array[(String, Int)],
    top_user_agents: Array[(String, Int)]
  }
  
  // Create sample gateway requests and responses
  let gateway_requests = [
    {
      request_id: "req-001",
      method: "GET",
      path: "/api/v1/orders",
      query_params: [
        ("limit", "10"),
        ("offset", "0")
      ],
      headers: [
        ("host", "api.example.com"),
        ("accept", "application/json"),
        ("authorization", "Bearer token123")
      ],
      client_ip: "192.168.1.100",
      user_agent: "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
      timestamp: 1640995200,
      api_key: Some("key-123"),
      auth_token: Some("token123")
    },
    {
      request_id: "req-002",
      method: "POST",
      path: "/api/v1/payments",
      query_params: [],
      headers: [
        ("host", "api.example.com"),
        ("content-type", "application/json"),
        ("authorization", "Bearer token456")
      ],
      client_ip: "192.168.1.101",
      user_agent: "curl/7.68.0",
      timestamp: 1640995210,
      api_key: Some("key-456"),
      auth_token: Some("token456")
    },
    {
      request_id: "req-003",
      method: "GET",
      path: "/api/v1/users/123",
      query_params: [],
      headers: [
        ("host", "api.example.com"),
        ("accept", "application/json"),
        ("authorization", "Bearer token789")
      ],
      client_ip: "192.168.1.102",
      user_agent: "Mozilla/5.0 (iPhone; CPU iPhone OS 14_7_1 like Mac OS X)",
      timestamp: 1640995220,
      api_key: Some("key-789"),
      auth_token: Some("token789")
    }
  ]
  
  let gateway_responses = [
    {
      request_id: "req-001",
      status_code: 200,
      response_size: 2048,
      headers: [
        ("content-type", "application/json"),
        ("cache-control", "max-age=300")
      ],
      duration: 150,
      upstream_service: "order-service",
      upstream_duration: 100,
      cache_hit: false,
      timestamp: 1640995215
    },
    {
      request_id: "req-002",
      status_code: 201,
      response_size: 512,
      headers: [
        ("content-type", "application/json"),
        ("location", "/api/v1/payments/12345")
      ],
      duration: 500,
      upstream_service: "payment-service",
      upstream_duration: 450,
      cache_hit: false,
      timestamp: 1640995270
    },
    {
      request_id: "req-003",
      status_code: 200,
      response_size: 1024,
      headers: [
        ("content-type", "application/json"),
        ("cache-control", "max-age=600")
      ],
      duration: 50,
      upstream_service: "user-service",
      upstream_duration: 10,
      cache_hit: true,
      timestamp: 1640995270
    }
  ]
  
  // Calculate gateway metrics
  let calculate_gateway_metrics = fn(requests: Array[GatewayRequest], responses: Array[GatewayResponse]) {
    let total_requests = requests.length()
    
    // Group by method
    let methods = requests.map(fn(r) { r.method })
    let mut method_counts = []
    let mut processed_methods = []
    
    for method in methods {
      if not(processed_methods.contains(method)) {
        processed_methods = processed_methods.push(method)
        let count = methods.filter(fn(m) { m == method }).length()
        method_counts = method_counts.push((method, count))
      }
    }
    
    // Group by path
    let paths = requests.map(fn(r) { r.path })
    let mut path_counts = []
    let mut processed_paths = []
    
    for path in paths {
      if not(processed_paths.contains(path)) {
        processed_paths = processed_paths.push(path)
        let count = paths.filter(fn(p) { p == path }).length()
        path_counts = path_counts.push((path, count))
      }
    }
    
    // Group by status code
    let status_codes = responses.map(fn(r) { r.status_code })
    let mut status_counts = []
    let mut processed_statuses = []
    
    for status in status_codes {
      if not(processed_statuses.contains(status)) {
        processed_statuses = processed_statuses.push(status)
        let count = status_codes.filter(fn(s) { s == status }).length()
        status_counts = status_counts.push((status, count))
      }
    }
    
    // Calculate average response time
    let durations = responses.map(fn(r) { r.duration })
    let total_duration = durations.reduce(fn(acc, d) { acc + d }, 0)
    let avg_response_time = if total_requests > 0 {
      total_duration.to_float() / total_requests.to_float()
    } else {
      0.0
    }
    
    // Calculate error rate (4xx and 5xx)
    let error_responses = responses.filter(fn(r) { r.status_code >= 400 })
    let error_rate = if total_requests > 0 {
      (error_responses.length().to_float() / total_requests.to_float()) * 100.0
    } else {
      0.0
    }
    
    // Calculate cache hit rate
    let cache_hits = responses.filter(fn(r) { r.cache_hit }).length()
    let cache_hit_rate = if total_requests > 0 {
      (cache_hits.to_float() / total_requests.to_float()) * 100.0
    } else {
      0.0
    }
    
    // Group by client IP
    let client_ips = requests.map(fn(r) { r.client_ip })
    let mut client_counts = []
    let mut processed_clients = []
    
    for ip in client_ips {
      if not(processed_clients.contains(ip)) {
        processed_clients = processed_clients.push(ip)
        let count = client_ips.filter(fn(c) { c == ip }).length()
        client_counts = client_counts.push((ip, count))
      }
    }
    
    // Sort clients by count (descending)
    let sorted_clients = client_counts.sort(fn(a, b) { b.1 - a.1 })
    let top_clients = sorted_clients.slice(0, 5)
    
    // Group by user agent
    let user_agents = requests.map(fn(r) { r.user_agent })
    let mut ua_counts = []
    let mut processed_uas = []
    
    for ua in user_agents {
      if not(processed_uas.contains(ua)) {
        processed_uas = processed_uas.push(ua)
        let count = user_agents.filter(fn(u) { u == ua }).length()
        ua_counts = ua_counts.push((ua, count))
      }
    }
    
    // Sort user agents by count (descending)
    let sorted_uas = ua_counts.sort(fn(a, b) { b.1 - a.1 })
    let top_user_agents = sorted_uas.slice(0, 5)
    
    {
      total_requests,
      requests_by_method: method_counts,
      requests_by_path: path_counts,
      requests_by_status: status_counts,
      avg_response_time,
      error_rate,
      cache_hit_rate,
      top_clients,
      top_user_agents
    }
  }
  
  let metrics = calculate_gateway_metrics(gateway_requests, gateway_responses)
  assert_eq(metrics.total_requests, 3)
  assert_eq(metrics.requests_by_method.length(), 2)  // GET, POST
  assert_eq(metrics.requests_by_path.length(), 3)    // 3 different paths
  assert_eq(metrics.requests_by_status.length(), 2)  // 200, 201
  assert_eq(metrics.avg_response_time, 233.33)       // (150 + 500 + 50) / 3
  assert_eq(metrics.error_rate, 0.0)                 // No errors
  assert_eq(metrics.cache_hit_rate, 33.33)           // 1 cache hit out of 3
  assert_eq(metrics.top_clients.length(), 3)         // 3 unique clients
  assert_eq(metrics.top_user_agents.length(), 3)     // 3 unique user agents
  
  // Find requests by method
  let find_requests_by_method = fn(requests: Array[GatewayRequest], method: String) {
    requests.filter(fn(r) { r.method == method })
  }
  
  let get_requests = find_requests_by_method(gateway_requests, "GET")
  assert_eq(get_requests.length(), 2)
  
  let post_requests = find_requests_by_method(gateway_requests, "POST")
  assert_eq(post_requests.length(), 1)
  
  // Find requests by path pattern
  let find_requests_by_path_pattern = fn(requests: Array[GatewayRequest], pattern: String) {
    requests.filter(fn(r) { r.path.starts_with(pattern) })
  }
  
  let api_requests = find_requests_by_path_pattern(gateway_requests, "/api/v1/")
  assert_eq(api_requests.length(), 3)
  
  let order_requests = find_requests_by_path_pattern(gateway_requests, "/api/v1/orders")
  assert_eq(order_requests.length(), 1)
  
  // Find slow requests
  let find_slow_requests = fn(requests: Array[GatewayRequest], responses: Array[GatewayResponse], threshold: Int) {
    let slow_response_ids = responses
      .filter(fn(r) { r.duration > threshold })
      .map(fn(r) { r.request_id })
    
    requests.filter(fn(r) { slow_response_ids.contains(r.request_id) })
  }
  
  let slow_requests = find_slow_requests(gateway_requests, gateway_responses, 200)
  assert_eq(slow_requests.length(), 1)
  assert_eq(slow_requests[0].request_id, "req-002")
  assert_eq(slow_requests[0].method, "POST")
  assert_eq(slow_requests[0].path, "/api/v1/payments")
}