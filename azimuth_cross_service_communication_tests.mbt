// Azimuth Cross-Service Communication Tests
// This file contains test cases for cross-service communication functionality

// Test 1: Service Discovery and Registration
test "service discovery and registration" {
  // Define service instance
  type ServiceInstance = {
    id: String,
    name: String,
    host: String,
    port: Int,
    protocol: String,
    tags: Array[String],
    metadata: Array[(String, String)],
    health_check_url: String,
    last_heartbeat: Int
  }
  
  // Define service registry
  type ServiceRegistry = {
    services: Array[ServiceInstance>,
    lock: Bool
  }
  
  // Create service registry
  let create_registry = fn() {
    { services: [], lock: false }
  }
  
  // Register service
  let register_service = fn(registry: ServiceRegistry, service: ServiceInstance) {
    // Check if service already exists
    let existing_service = registry.services.find(fn(s) { s.id == service.id })
    
    if existing_service == None {
      // Add new service
      { registry | services: registry.services.push(service) }
    } else {
      // Update existing service
      let updated_services = registry.services.map(fn(s) {
        if s.id == service.id { service } else { s }
      })
      { registry | services: updated_services }
    }
  }
  
  // Discover services by name
  let discover_services = fn(registry: ServiceRegistry, service_name: String) {
    registry.services.filter(fn(s) { s.name == service_name })
  }
  
  // Discover service by name and tags
  let discover_services_by_tags = fn(registry: ServiceRegistry, service_name: String, tags: Array[String]) {
    registry.services.filter(fn(s) { 
      s.name == service_name and 
      tags.all(fn(tag) { s.tags.contains(tag) })
    })
  }
  
  // Deregister service
  let deregister_service = fn(registry: ServiceRegistry, service_id: String) {
    let filtered_services = registry.services.filter(fn(s) { s.id != service_id })
    { registry | services: filtered_services }
  }
  
  // Update heartbeat
  let update_heartbeat = fn(registry: ServiceRegistry, service_id: String, timestamp: Int) {
    let updated_services = registry.services.map(fn(s) {
      if s.id == service_id {
        { s | last_heartbeat: timestamp }
      } else {
        s
      }
    })
    { registry | services: updated_services }
  }
  
  // Create test services
  let user_service_1 = {
    id: "user-service-1",
    name: "user-service",
    host: "10.0.1.10",
    port: 8080,
    protocol: "http",
    tags: ["v1", "primary"],
    metadata: [("version", "1.2.3"), ("region", "us-west-2")],
    health_check_url: "http://10.0.1.10:8080/health",
    last_heartbeat: 1640995200
  }
  
  let user_service_2 = {
    id: "user-service-2",
    name: "user-service",
    host: "10.0.1.11",
    port: 8080,
    protocol: "http",
    tags: ["v1", "secondary"],
    metadata: [("version", "1.2.3"), ("region", "us-west-2")],
    health_check_url: "http://10.0.1.11:8080/health",
    last_heartbeat: 1640995200
  }
  
  let order_service = {
    id: "order-service-1",
    name: "order-service",
    host: "10.0.1.20",
    port: 8080,
    protocol: "http",
    tags: ["v1", "primary"],
    metadata: [("version", "2.1.0"), ("region", "us-west-2")],
    health_check_url: "http://10.0.1.20:8080/health",
    last_heartbeat: 1640995200
  }
  
  // Test service registration
  let registry = create_registry()
  assert_eq(registry.services.length(), 0)
  
  let registry_with_user1 = register_service(registry, user_service_1)
  assert_eq(registry_with_user1.services.length(), 1)
  
  let registry_with_user2 = register_service(registry_with_user1, user_service_2)
  assert_eq(registry_with_user2.services.length(), 2)
  
  let registry_with_order = register_service(registry_with_user2, order_service)
  assert_eq(registry_with_order.services.length(), 3)
  
  // Test service discovery by name
  let user_services = discover_services(registry_with_order, "user-service")
  assert_eq(user_services.length(), 2)
  
  let order_services = discover_services(registry_with_order, "order-service")
  assert_eq(order_services.length(), 1)
  
  let payment_services = discover_services(registry_with_order, "payment-service")
  assert_eq(payment_services.length(), 0)
  
  // Test service discovery by name and tags
  let primary_user_services = discover_services_by_tags(registry_with_order, "user-service", ["primary"])
  assert_eq(primary_user_services.length(), 1)
  assert_eq(primary_user_services[0].id, "user-service-1")
  
  let secondary_user_services = discover_services_by_tags(registry_with_order, "user-service", ["secondary"])
  assert_eq(secondary_user_services.length(), 1)
  assert_eq(secondary_user_services[0].id, "user-service-2")
  
  // Test service update
  let updated_user_service = { user_service_1 | metadata: [("version", "1.2.4"), ("region", "us-west-2")] }
  let registry_with_update = register_service(registry_with_order, updated_user_service)
  
  let updated_services = discover_services(registry_with_update, "user-service")
  let updated_service_1 = updated_services.filter(fn(s) { s.id == "user-service-1" })[0]
  assert_eq(updated_service_1.metadata.filter(fn(m) { m.0 == "version" })[0].1, "1.2.4")
  
  // Test heartbeat update
  let registry_with_heartbeat = update_heartbeat(registry_with_update, "user-service-1", 1640995300)
  let services_with_heartbeat = discover_services(registry_with_heartbeat, "user-service")
  let service_with_heartbeat = services_with_heartbeat.filter(fn(s) { s.id == "user-service-1" })[0]
  assert_eq(service_with_heartbeat.last_heartbeat, 1640995300)
  
  // Test service deregistration
  let registry_after_dereg = deregister_service(registry_with_heartbeat, "user-service-2")
  assert_eq(registry_after_dereg.services.length(), 2)
  
  let remaining_user_services = discover_services(registry_after_dereg, "user-service")
  assert_eq(remaining_user_services.length(), 1)
  assert_eq(remaining_user_services[0].id, "user-service-1")
}

// Test 2: Load Balancing
test "load balancing strategies" {
  // Define service instance
  type ServiceInstance = {
    id: String,
    host: String,
    port: Int,
    weight: Int,
    current_connections: Int,
    max_connections: Int,
    healthy: Bool
  }
  
  // Define load balancer
  type LoadBalancer = {
    strategy: String,
    instances: Array[ServiceInstance>,
    round_robin_index: Int
  }
  
  // Create load balancer
  let create_load_balancer = fn(strategy: String, instances: Array[ServiceInstance>) {
    {
      strategy,
      instances,
      round_robin_index: 0
    }
  }
  
  // Round Robin selection
  let round_robin_select = fn(lb: LoadBalancer) {
    if lb.instances.length() == 0 {
      None
    } else {
      let healthy_instances = lb.instances.filter(fn(i) { i.healthy })
      
      if healthy_instances.length() == 0 {
        None
      } else {
        let index = lb.round_robin_index % healthy_instances.length()
        let selected = healthy_instances[index]
        Some((selected, { lb | round_robin_index: lb.round_robin_index + 1 }))
      }
    }
  }
  
  // Weighted Round Robin selection
  let weighted_round_robin_select = fn(lb: LoadBalancer) {
    if lb.instances.length() == 0 {
      None
    } else {
      let healthy_instances = lb.instances.filter(fn(i) { i.healthy })
      
      if healthy_instances.length() == 0 {
        None
      } else {
        // Calculate total weight
        let total_weight = healthy_instances.reduce(fn(sum, i) { sum + i.weight }, 0)
        
        if total_weight == 0 {
          None
        } else {
          // Simplified: just select based on weight proportion
          let target_weight = lb.round_robin_index % total_weight
          let mut current_weight = 0
          let mut selected = None
          
          for instance in healthy_instances {
            current_weight = current_weight + instance.weight
            if current_weight > target_weight and selected == None {
              selected = Some(instance)
            }
          }
          
          match selected {
            Some(instance) => {
              Some((instance, { lb | round_robin_index: lb.round_robin_index + 1 }))
            }
            None => Some((healthy_instances[0], { lb | round_robin_index: lb.round_robin_index + 1 }))
          }
        }
      }
    }
  }
  
  // Least Connections selection
  let least_connections_select = fn(lb: LoadBalancer) {
    if lb.instances.length() == 0 {
      None
    } else {
      let healthy_instances = lb.instances.filter(fn(i) { i.healthy })
      
      if healthy_instances.length() == 0 {
        None
      } else {
        let min_connections = healthy_instances.reduce(fn(min, i) { 
          if i.current_connections < min { i.current_connections } else { min } 
        }, healthy_instances[0].current_connections)
        
        let least_loaded_instances = healthy_instances.filter(fn(i) { i.current_connections == min_connections })
        
        // If multiple instances have the same minimum connections, use round robin among them
        let index = lb.round_robin_index % least_loaded_instances.length()
        let selected = least_loaded_instances[index]
        
        Some((selected, { lb | round_robin_index: lb.round_robin_index + 1 }))
      }
    }
  }
  
  // Create test instances
  let instances = [
    { id: "instance-1", host: "10.0.1.10", port: 8080, weight: 1, current_connections: 5, max_connections: 100, healthy: true },
    { id: "instance-2", host: "10.0.1.11", port: 8080, weight: 2, current_connections: 3, max_connections: 100, healthy: true },
    { id: "instance-3", host: "10.0.1.12", port: 8080, weight: 3, current_connections: 8, max_connections: 100, healthy: true },
    { id: "instance-4", host: "10.0.1.13", port: 8080, weight: 1, current_connections: 1, max_connections: 100, healthy: false }  // Unhealthy
  ]
  
  // Test Round Robin
  let rr_lb = create_load_balancer("round_robin", instances)
  
  match round_robin_select(rr_lb) {
    Some((selected, _)) => assert_eq(selected.id, "instance-1")
    None => assert_true(false)
  }
  
  let rr_lb_after_1 = match round_robin_select(rr_lb) {
    Some((_, updated)) => updated
    None => rr_lb
  }
  
  match round_robin_select(rr_lb_after_1) {
    Some((selected, _)) => assert_eq(selected.id, "instance-2")
    None => assert_true(false)
  }
  
  let rr_lb_after_2 = match round_robin_select(rr_lb_after_1) {
    Some((_, updated)) => updated
    None => rr_lb_after_1
  }
  
  match round_robin_select(rr_lb_after_2) {
    Some((selected, _)) => assert_eq(selected.id, "instance-3")
    None => assert_true(false)
  }
  
  let rr_lb_after_3 = match round_robin_select(rr_lb_after_2) {
    Some((_, updated)) => updated
    None => rr_lb_after_2
  }
  
  // Should cycle back to instance-1
  match round_robin_select(rr_lb_after_3) {
    Some((selected, _)) => assert_eq(selected.id, "instance-1")
    None => assert_true(false)
  }
  
  // Test Weighted Round Robin
  let wrr_lb = create_load_balancer("weighted_round_robin", instances)
  
  // With weights 1, 2, 3, the distribution should favor instance-3
  let mut wrr_selections = []
  let mut current_wrr_lb = wrr_lb
  
  for i in 0 .. 6 {
    match weighted_round_robin_select(current_wrr_lb) {
      Some((selected, updated)) => {
        wrr_selections = wrr_selections.push(selected.id)
        current_wrr_lb = updated
      }
      None => assert_true(false)
    }
  }
  
  // Instance-3 should be selected more often due to higher weight
  let instance_1_count = wrr_selections.filter(fn(id) { id == "instance-1" }).length()
  let instance_2_count = wrr_selections.filter(fn(id) { id == "instance-2" }).length()
  let instance_3_count = wrr_selections.filter(fn(id) { id == "instance-3" }).length()
  
  assert_eq(instance_3_count, 3)  // Selected 3 times
  assert_eq(instance_2_count, 2)  // Selected 2 times
  assert_eq(instance_1_count, 1)  // Selected 1 time
  
  // Test Least Connections
  let lc_lb = create_load_balancer("least_connections", instances)
  
  match least_connections_select(lc_lb) {
    Some((selected, _)) => assert_eq(selected.id, "instance-2")  // 3 connections
    None => assert_true(false)
  }
  
  // Test with unhealthy instance
  let unhealthy_instances = [
    { id: "instance-1", host: "10.0.1.10", port: 8080, weight: 1, current_connections: 5, max_connections: 100, healthy: false },
    { id: "instance-2", host: "10.0.1.11", port: 8080, weight: 2, current_connections: 3, max_connections: 100, healthy: true }
  ]
  
  let unhealthy_lb = create_load_balancer("round_robin", unhealthy_instances)
  
  match round_robin_select(unhealthy_lb) {
    Some((selected, _)) => assert_eq(selected.id, "instance-2")  // Only healthy instance
    None => assert_true(false)
  }
  
  // Test with all unhealthy instances
  let all_unhealthy_instances = [
    { id: "instance-1", host: "10.0.1.10", port: 8080, weight: 1, current_connections: 5, max_connections: 100, healthy: false },
    { id: "instance-2", host: "10.0.1.11", port: 8080, weight: 2, current_connections: 3, max_connections: 100, healthy: false }
  ]
  
  let all_unhealthy_lb = create_load_balancer("round_robin", all_unhealthy_instances)
  
  match round_robin_select(all_unhealthy_lb) {
    Some(_) => assert_true(false)  // Should not select any instance
    None => assert_true(true)      // Should return None
  }
}

// Test 3: Circuit Breaker Pattern
test "circuit breaker pattern" {
  // Define circuit breaker state
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // Define circuit breaker
  type CircuitBreaker = {
    state: CircuitState,
    failure_count: Int,
    success_count: Int,
    failure_threshold: Int,
    success_threshold: Int,
    recovery_timeout_ms: Int,
    last_failure_time: Option[Int],
    request_count: Int
  }
  
  // Create circuit breaker
  let create_circuit_breaker = fn(failure_threshold: Int, success_threshold: Int, recovery_timeout_ms: Int) {
    {
      state: CircuitState::Closed,
      failure_count: 0,
      success_count: 0,
      failure_threshold,
      success_threshold,
      recovery_timeout_ms,
      last_failure_time: None,
      request_count: 0
    }
  }
  
  // Check if circuit allows requests
  let can_execute = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Closed => true
      CircuitState::Open => {
        match breaker.last_failure_time {
          Some(last_failure) => {
            (current_time - last_failure) > breaker.recovery_timeout_ms
          }
          None => false
        }
      }
      CircuitState::HalfOpen => true
    }
  }
  
  // Record success
  let record_success = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Closed => {
        { breaker | failure_count: 0, success_count: 0 }
      }
      CircuitState::HalfOpen => {
        let new_success_count = breaker.success_count + 1
        if new_success_count >= breaker.success_threshold {
          // Close the circuit
          {
            state: CircuitState::Closed,
            failure_count: 0,
            success_count: 0,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            recovery_timeout_ms: breaker.recovery_timeout_ms,
            last_failure_time: None,
            request_count: breaker.request_count + 1
          }
        } else {
          { breaker | success_count: new_success_count, request_count: breaker.request_count + 1 }
        }
      }
      CircuitState::Open => breaker  // Shouldn't happen in normal operation
    }
  }
  
  // Record failure
  let record_failure = fn(breaker: CircuitBreaker, current_time: Int) {
    let new_failure_count = breaker.failure_count + 1
    
    match breaker.state {
      CircuitState::Closed => {
        if new_failure_count >= breaker.failure_threshold {
          // Open the circuit
          {
            state: CircuitState::Open,
            failure_count: new_failure_count,
            success_count: 0,
            failure_threshold: breaker.failure_threshold,
            success_threshold: breaker.success_threshold,
            recovery_timeout_ms: breaker.recovery_timeout_ms,
            last_failure_time: Some(current_time),
            request_count: breaker.request_count + 1
          }
        } else {
          { breaker | failure_count: new_failure_count, request_count: breaker.request_count + 1 }
        }
      }
      CircuitState::HalfOpen => {
        // Open the circuit again
        {
          state: CircuitState::Open,
          failure_count: new_failure_count,
          success_count: 0,
          failure_threshold: breaker.failure_threshold,
          success_threshold: breaker.success_threshold,
          recovery_timeout_ms: breaker.recovery_timeout_ms,
          last_failure_time: Some(current_time),
          request_count: breaker.request_count + 1
        }
      }
      CircuitState::Open => breaker  // Shouldn't happen in normal operation
    }
  }
  
  // Try to transition from Open to HalfOpen
  let try_transition_to_half_open = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Open => {
        match breaker.last_failure_time {
          Some(last_failure) => {
            if (current_time - last_failure) > breaker.recovery_timeout_ms {
              { breaker | state: CircuitState::HalfOpen, success_count: 0 }
            } else {
              breaker
            }
          }
          None => breaker
        }
      }
      _ => breaker
    }
  }
  
  // Create circuit breaker with threshold of 3 failures and recovery timeout of 5000ms
  let breaker = create_circuit_breaker(3, 2, 5000)
  assert_eq(breaker.state, CircuitState::Closed)
  assert_true(can_execute(breaker, 1640995200))
  
  // Record failures
  let breaker_after_fail_1 = record_failure(breaker, 1640995200)
  assert_eq(breaker_after_fail_1.state, CircuitState::Closed)
  assert_eq(breaker_after_fail_1.failure_count, 1)
  assert_true(can_execute(breaker_after_fail_1, 1640995200))
  
  let breaker_after_fail_2 = record_failure(breaker_after_fail_1, 1640995210)
  assert_eq(breaker_after_fail_2.state, CircuitState::Closed)
  assert_eq(breaker_after_fail_2.failure_count, 2)
  assert_true(can_execute(breaker_after_fail_2, 1640995210))
  
  // Third failure should open the circuit
  let breaker_after_fail_3 = record_failure(breaker_after_fail_2, 1640995220)
  assert_eq(breaker_after_fail_3.state, CircuitState::Open)
  assert_eq(breaker_after_fail_3.failure_count, 3)
  assert_eq(breaker_after_fail_3.last_failure_time, Some(1640995220))
  assert_false(can_execute(breaker_after_fail_3, 1640995220))  // Should not allow requests
  
  // Should still not allow requests within recovery timeout
  assert_false(can_execute(breaker_after_fail_3, 1640995220 + 1000))  // 1 second later
  
  // Should allow requests after recovery timeout
  assert_true(can_execute(breaker_after_fail_3, 1640995220 + 6000))  // 6 seconds later
  
  // Transition to HalfOpen
  let breaker_half_open = try_transition_to_half_open(breaker_after_fail_3, 1640995220 + 6000)
  assert_eq(breaker_half_open.state, CircuitState::HalfOpen)
  assert_true(can_execute(breaker_half_open, 1640995220 + 6000))
  
  // Record success in HalfOpen state
  let breaker_half_open_success_1 = record_success(breaker_half_open, 1640995220 + 6000)
  assert_eq(breaker_half_open_success_1.state, CircuitState::HalfOpen)
  assert_eq(breaker_half_open_success_1.success_count, 1)
  
  // Second success should close the circuit (success_threshold is 2)
  let breaker_closed = record_success(breaker_half_open_success_1, 1640995220 + 6010)
  assert_eq(breaker_closed.state, CircuitState::Closed)
  assert_eq(breaker_closed.failure_count, 0)
  assert_eq(breaker_closed.success_count, 0)
  
  // Test failure in HalfOpen state
  let breaker_half_open_fail = record_failure(breaker_half_open, 1640995220 + 6000)
  assert_eq(breaker_half_open_fail.state, CircuitState::Open)
  assert_eq(breaker_half_open_fail.failure_count, 4)
  assert_eq(breaker_half_open_fail.last_failure_time, Some(1640995220 + 6000))
  
  // Test success in Closed state
  let breaker_closed_success = record_success(breaker, 1640995200)
  assert_eq(breaker_closed_success.state, CircuitState::Closed)
  assert_eq(breaker_closed_success.failure_count, 0)
}

// Test 4: Request Retry with Exponential Backoff
test "request retry with exponential backoff" {
  // Define retry configuration
  type RetryConfig = {
    max_attempts: Int,
    base_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float,
    jitter: Bool
  }
  
  // Define request result
  type RequestResult = 
    Success(String)
    | Failure(String, Int)  // Error message, status code
  
  // Create default retry configuration
  let default_retry_config = {
    max_attempts: 3,
    base_delay_ms: 100,
    max_delay_ms: 1000,
    backoff_multiplier: 2.0,
    jitter: true
  }
  
  // Calculate delay for retry attempt
  let calculate_delay = fn(attempt: Int, config: RetryConfig) {
    let base_delay = config.base_delay_ms.to_float() * 
      config.backoff_multiplier.pow((attempt - 1).to_float())
    let delay = base_delay.min(config.max_delay_ms.to_float())
    
    if config.jitter {
      // Add jitter: ±25% of the delay
      let jitter_range = delay * 0.25
      delay + jitter_range * 2.0 * 0.5 - jitter_range  // Random between -jitter_range and +jitter_range
    } else {
      delay
    }
  }
  
  // Execute request with retry
  let execute_with_retry = fn(
    operation: () -> RequestResult,
    config: RetryConfig
  ) {
    let mut attempt = 1
    let mut last_result = Failure("No attempts made", 0)
    
    while attempt <= config.max_attempts {
      last_result = operation()
      
      match last_result {
        Success(_) => return last_result
        Failure(_, status_code) => {
          // Don't retry on client errors (4xx)
          if status_code >= 400 and status_code < 500 {
            return last_result
          }
          
          if attempt < config.max_attempts {
            let delay = calculate_delay(attempt, config)
            // In a real implementation, we would wait for the delay here
            attempt = attempt + 1
          } else {
            return last_result
          }
        }
      }
    }
    
    last_result
  }
  
  // Test delay calculation without jitter
  let no_jitter_config = { default_retry_config | jitter: false }
  
  assert_eq(calculate_delay(1, no_jitter_config), 100.0)
  assert_eq(calculate_delay(2, no_jitter_config), 200.0)
  assert_eq(calculate_delay(3, no_jitter_config), 400.0)
  assert_eq(calculate_delay(4, no_jitter_config), 800.0)
  assert_eq(calculate_delay(5, no_jitter_config), 1000.0)  // Capped at max_delay_ms
  
  // Test delay calculation with jitter (range check)
  let delay_with_jitter_1 = calculate_delay(1, default_retry_config)
  assert_true(delay_with_jitter_1 >= 75.0 and delay_with_jitter_1 <= 125.0)  // 100 ± 25%
  
  let delay_with_jitter_2 = calculate_delay(2, default_retry_config)
  assert_true(delay_with_jitter_2 >= 150.0 and delay_with_jitter_2 <= 250.0)  // 200 ± 25%
  
  // Test successful operation
  let success_operation = fn() { Success("Operation succeeded") }
  let result = execute_with_retry(success_operation, default_retry_config)
  
  match result {
    Success(value) => assert_eq(value, "Operation succeeded")
    Failure(_, _) => assert_true(false)
  }
  
  // Test operation that succeeds after retries
  let mut attempt_count = 0
  let retry_success_operation = fn() {
    attempt_count = attempt_count + 1
    if attempt_count < 3 {
      Failure("Server error", 500)
    } else {
      Success("Operation succeeded after retries")
    }
  }
  
  attempt_count = 0
  let retry_result = execute_with_retry(retry_success_operation, default_retry_config)
  
  match retry_result {
    Success(value) => assert_eq(value, "Operation succeeded after retries")
    Failure(_, _) => assert_true(false)
  }
  assert_eq(attempt_count, 3)
  
  // Test operation that fails after all retries
  attempt_count = 0
  let always_fail_operation = fn() {
    attempt_count = attempt_count + 1
    Failure("Server error", 500)
  }
  
  attempt_count = 0
  let fail_result = execute_with_retry(always_fail_operation, default_retry_config)
  
  match fail_result {
    Success(_) => assert_true(false)
    Failure(message, status_code) => {
      assert_eq(message, "Server error")
      assert_eq(status_code, 500)
    }
  }
  assert_eq(attempt_count, 3)
  
  // Test operation that fails with client error (should not retry)
  attempt_count = 0
  let client_error_operation = fn() {
    attempt_count = attempt_count + 1
    Failure("Client error", 400)
  }
  
  attempt_count = 0
  let client_error_result = execute_with_retry(client_error_operation, default_retry_config)
  
  match client_error_result {
    Success(_) => assert_true(false)
    Failure(message, status_code) => {
      assert_eq(message, "Client error")
      assert_eq(status_code, 400)
    }
  }
  assert_eq(attempt_count, 1)  // Should only attempt once
}

// Test 5: Request Timeout and Cancellation
test "request timeout and cancellation" {
  // Define request state
  enum RequestState {
    Pending
    Executing
    Completed
    Failed
    Cancelled
  }
  
  // Define request
  type Request = {
    id: String,
    url: String,
    method: String,
    timeout_ms: Int,
    state: RequestState,
    start_time: Option[Int],
    end_time: Option[Int],
    result: Option[String],
    error: Option[String]
  }
  
  // Define request manager
  type RequestManager = {
    requests: Array[Request>,
    current_time: Int
  }
  
  // Create request manager
  let create_request_manager = fn() {
    { requests: [], current_time: 0 }
  }
  
  // Create request
  let create_request = fn(id: String, url: String, method: String, timeout_ms: Int) {
    {
      id,
      url,
      method,
      timeout_ms,
      state: RequestState::Pending,
      start_time: None,
      end_time: None,
      result: None,
      error: None
    }
  }
  
  // Submit request
  let submit_request = fn(manager: RequestManager, request: Request) {
    let updated_request = { request | state: RequestState::Executing, start_time: Some(manager.current_time) }
    { manager | requests: manager.requests.push(updated_request) }
  }
  
  // Complete request
  let complete_request = fn(manager: RequestManager, request_id: String, result: String) {
    let updated_requests = manager.requests.map(fn(req) {
      if req.id == request_id {
        {
          id: req.id,
          url: req.url,
          method: req.method,
          timeout_ms: req.timeout_ms,
          state: RequestState::Completed,
          start_time: req.start_time,
          end_time: Some(manager.current_time),
          result: Some(result),
          error: None
        }
      } else {
        req
      }
    })
    { manager | requests: updated_requests }
  }
  
  // Fail request
  let fail_request = fn(manager: RequestManager, request_id: String, error: String) {
    let updated_requests = manager.requests.map(fn(req) {
      if req.id == request_id {
        {
          id: req.id,
          url: req.url,
          method: req.method,
          timeout_ms: req.timeout_ms,
          state: RequestState::Failed,
          start_time: req.start_time,
          end_time: Some(manager.current_time),
          result: None,
          error: Some(error)
        }
      } else {
        req
      }
    })
    { manager | requests: updated_requests }
  }
  
  // Cancel request
  let cancel_request = fn(manager: RequestManager, request_id: String) {
    let updated_requests = manager.requests.map(fn(req) {
      if req.id == request_id and (req.state == RequestState::Pending or req.state == RequestState::Executing) {
        {
          id: req.id,
          url: req.url,
          method: req.method,
          timeout_ms: req.timeout_ms,
          state: RequestState::Cancelled,
          start_time: req.start_time,
          end_time: Some(manager.current_time),
          result: None,
          error: Some("Request cancelled")
        }
      } else {
        req
      }
    })
    { manager | requests: updated_requests }
  }
  
  // Check for timeouts
  let check_timeouts = fn(manager: RequestManager) {
    let updated_requests = manager.requests.map(fn(req) {
      if req.state == RequestState::Executing {
        match req.start_time {
          Some(start_time) => {
            let elapsed = manager.current_time - start_time
            if elapsed > req.timeout_ms {
              {
                id: req.id,
                url: req.url,
                method: req.method,
                timeout_ms: req.timeout_ms,
                state: RequestState::Failed,
                start_time: req.start_time,
                end_time: Some(manager.current_time),
                result: None,
                error: Some("Request timed out")
              }
            } else {
              req
            }
          }
          None => req
        }
      } else {
        req
      }
    })
    { manager | requests: updated_requests }
  }
  
  // Advance time
  let advance_time = fn(manager: RequestManager, delta_ms: Int) {
    let new_time = manager.current_time + delta_ms
    let updated_manager = { manager | current_time: new_time }
    check_timeouts(updated_manager)
  }
  
  // Test request lifecycle
  let manager = create_request_manager()
  let request = create_request("req-1", "http://example.com/api/users", "GET", 1000)
  
  // Submit request
  let manager_with_request = submit_request(manager, request)
  assert_eq(manager_with_request.requests.length(), 1)
  assert_eq(manager_with_request.requests[0].state, RequestState::Executing)
  assert_eq(manager_with_request.requests[0].start_time, Some(0))
  
  // Complete request
  let manager_with_completed = complete_request(manager_with_request, "req-1", "Response data")
  assert_eq(manager_with_completed.requests[0].state, RequestState::Completed)
  assert_eq(manager_with_completed.requests[0].result, Some("Response data"))
  assert_eq(manager_with_completed.requests[0].end_time, Some(0))
  
  // Test request failure
  let manager2 = create_request_manager()
  let request2 = create_request("req-2", "http://example.com/api/orders", "POST", 1000)
  let manager_with_request2 = submit_request(manager2, request2)
  let manager_with_failed = fail_request(manager_with_request2, "req-2", "Connection refused")
  
  assert_eq(manager_with_failed.requests[0].state, RequestState::Failed)
  assert_eq(manager_with_failed.requests[0].error, Some("Connection refused"))
  assert_eq(manager_with_failed.requests[0].end_time, Some(0))
  
  // Test request cancellation
  let manager3 = create_request_manager()
  let request3 = create_request("req-3", "http://example.com/api/products", "GET", 1000)
  let manager_with_request3 = submit_request(manager3, request3)
  let manager_with_cancelled = cancel_request(manager_with_request3, "req-3")
  
  assert_eq(manager_with_cancelled.requests[0].state, RequestState::Cancelled)
  assert_eq(manager_with_cancelled.requests[0].error, Some("Request cancelled"))
  assert_eq(manager_with_cancelled.requests[0].end_time, Some(0))
  
  // Test timeout
  let manager4 = create_request_manager()
  let request4 = create_request("req-4", "http://example.com/api/payments", "POST", 1000)
  let manager_with_request4 = submit_request(manager4, request4)
  
  // Advance time by 500ms (should not timeout yet)
  let manager_after_500ms = advance_time(manager_with_request4, 500)
  assert_eq(manager_after_500ms.requests[0].state, RequestState::Executing)
  
  // Advance time by another 600ms (total 1100ms, should timeout)
  let manager_after_1100ms = advance_time(manager_after_500ms, 600)
  assert_eq(manager_after_1100ms.requests[0].state, RequestState::Failed)
  assert_eq(manager_after_1100ms.requests[0].error, Some("Request timed out"))
  assert_eq(manager_after_1100ms.requests[0].end_time, Some(1100))
}

// Test 6: Service Mesh Communication
test "service mesh communication" {
  // Define service mesh configuration
  type ServiceMeshConfig = {
    name: String,
    namespace: String,
    sidecar_proxy_port: Int,
    control_plane_address: String,
    mtls_enabled: Bool,
    tracing_enabled: Bool
  }
  
  // Define service endpoint
  type ServiceEndpoint = {
    service_name: String,
    hostname: String,
    port: Int,
    protocol: String
  }
  
  // Define traffic rule
  type TrafficRule = {
    name: String,
    source: String,
    destination: String,
    port: Int,
    action: String  // "allow", "deny", "redirect"
    redirect_to: Option[String]
  }
  
  // Define service mesh
  type ServiceMesh = {
    config: ServiceMeshConfig,
    services: Array[ServiceEndpoint>,
    traffic_rules: Array[TrafficRule>,
    certificates: Array[(String, String)>  // Service name, certificate
  }
  
  // Create service mesh
  let create_service_mesh = fn(config: ServiceMeshConfig) {
    {
      config,
      services: [],
      traffic_rules: [],
      certificates: []
    }
  }
  
  // Register service
  let register_service = fn(mesh: ServiceMesh, service: ServiceEndpoint) {
    { mesh | services: mesh.services.push(service) }
  }
  
  // Add traffic rule
  let add_traffic_rule = fn(mesh: ServiceMesh, rule: TrafficRule) {
    { mesh | traffic_rules: mesh.traffic_rules.push(rule) }
  }
  
  // Issue certificate
  let issue_certificate = fn(mesh: ServiceMesh, service_name: String, certificate: String) {
    { mesh | certificates: mesh.certificates.push((service_name, certificate)) }
  }
  
  // Check if traffic is allowed
  let is_traffic_allowed = fn(mesh: ServiceMesh, source: String, destination: String, port: Int) {
    let matching_rules = mesh.traffic_rules.filter(fn(rule) {
      rule.source == source and rule.destination == destination and rule.port == port
    })
    
    if matching_rules.length() == 0 {
      true  // Default allow
    } else {
      let rule = matching_rules[0]
      rule.action == "allow"
    }
  }
  
  // Get redirect destination
  let get_redirect_destination = fn(mesh: ServiceMesh, source: String, destination: String, port: Int) {
    let matching_rules = mesh.traffic_rules.filter(fn(rule) {
      rule.source == source and rule.destination == destination and rule.port == port and rule.action == "redirect"
    })
    
    if matching_rules.length() > 0 {
      matching_rules[0].redirect_to
    } else {
      None
    }
  }
  
  // Get service endpoint
  let get_service_endpoint = fn(mesh: ServiceMesh, service_name: String) {
    mesh.services.find(fn(service) { service.service_name == service_name })
  }
  
  // Get certificate for service
  let get_certificate = fn(mesh: ServiceMesh, service_name: String) {
    mesh.certificates.find(fn(cert) { cert.0 == service_name })
  }
  
  // Create service mesh configuration
  let mesh_config = {
    name: "azimuth-mesh",
    namespace: "production",
    sidecar_proxy_port: 15001,
    control_plane_address: "istiod.istio-system.svc:15012",
    mtls_enabled: true,
    tracing_enabled: true
  }
  
  // Create service mesh
  let mesh = create_service_mesh(mesh_config)
  
  // Register services
  let user_service = { service_name: "user-service", hostname: "user-service.production.svc.cluster.local", port: 8080, protocol: "http" }
  let order_service = { service_name: "order-service", hostname: "order-service.production.svc.cluster.local", port: 8080, protocol: "http" }
  let payment_service = { service_name: "payment-service", hostname: "payment-service.production.svc.cluster.local", port: 8080, protocol: "http" }
  
  let mesh_with_services = mesh
    |> register_service(user_service)
    |> register_service(order_service)
    |> register_service(payment_service)
  
  assert_eq(mesh_with_services.services.length(), 3)
  
  // Add traffic rules
  let user_to_order_rule = {
    name: "user-to-order",
    source: "user-service",
    destination: "order-service",
    port: 8080,
    action: "allow",
    redirect_to: None
  }
  
  let order_to_payment_rule = {
    name: "order-to-payment",
    source: "order-service",
    destination: "payment-service",
    port: 8080,
    action: "allow",
    redirect_to: None
  }
  
  let block_external_rule = {
    name: "block-external",
    source: "user-service",
    destination: "external-api.com",
    port: 443,
    action: "deny",
    redirect_to: None
  }
  
  let redirect_rule = {
    name: "payment-redirect",
    source: "order-service",
    destination: "payment-service",
    port: 8080,
    action: "redirect",
    redirect_to: Some("payment-service-v2")
  }
  
  let mesh_with_rules = mesh_with_services
    |> add_traffic_rule(user_to_order_rule)
    |> add_traffic_rule(order_to_payment_rule)
    |> add_traffic_rule(block_external_rule)
    |> add_traffic_rule(redirect_rule)
  
  assert_eq(mesh_with_rules.traffic_rules.length(), 4)
  
  // Test traffic rules
  assert_true(is_traffic_allowed(mesh_with_rules, "user-service", "order-service", 8080))
  assert_true(is_traffic_allowed(mesh_with_rules, "order-service", "payment-service", 8080))
  assert_false(is_traffic_allowed(mesh_with_rules, "user-service", "external-api.com", 443))
  
  // Default allow for unspecified traffic
  assert_true(is_traffic_allowed(mesh_with_rules, "unknown-service", "unknown-service", 8080))
  
  // Test redirect
  match get_redirect_destination(mesh_with_rules, "order-service", "payment-service", 8080) {
    Some(destination) => assert_eq(destination, "payment-service-v2")
    None => assert_true(false)
  }
  
  // Test service endpoint lookup
  match get_service_endpoint(mesh_with_rules, "user-service") {
    Some(endpoint) => {
      assert_eq(endpoint.hostname, "user-service.production.svc.cluster.local")
      assert_eq(endpoint.port, 8080)
    }
    None => assert_true(false)
  }
  
  // Test certificate management
  let mesh_with_certs = mesh_with_rules
    |> issue_certificate("user-service", "-----BEGIN CERTIFICATE-----\nuser-service-cert\n-----END CERTIFICATE-----")
    |> issue_certificate("order-service", "-----BEGIN CERTIFICATE-----\norder-service-cert\n-----END CERTIFICATE-----")
  
  assert_eq(mesh_with_certs.certificates.length(), 2)
  
  match get_certificate(mesh_with_certs, "user-service") {
    Some((_, cert)) => assert_true(cert.contains("user-service-cert"))
    None => assert_true(false)
  }
  
  // Test mTLS requirement
  assert_true(mesh_with_certs.config.mtls_enabled)
  
  // Test tracing configuration
  assert_true(mesh_with_certs.config.tracing_enabled)
}