// Azimuth Service Mesh Test Suite
// This file contains test cases for service mesh integration and observability

// Test 1: Service Discovery and Registration
test "service discovery and registration in mesh" {
  // Define service instance
  type ServiceInstance = {
    service_name: String,
    instance_id: String,
    host: String,
    port: Int,
    protocol: String,
    metadata: Array[(String, String)],
    health_status: String,
    last_heartbeat: Int
  }
  
  // Define service registry
  type ServiceRegistry = {
    services: Array[ServiceInstance],
    health_check_interval: Int,
    deregistration_timeout: Int
  }
  
  // Create service registry
  let create_registry = fn(health_check_interval: Int, deregistration_timeout: Int) {
    {
      services: [],
      health_check_interval,
      deregistration_timeout
    }
  }
  
  // Register service instance
  let register_service = fn(registry: ServiceRegistry, instance: ServiceInstance) {
    let existing = registry.services.find_index(fn(s) { 
      s.service_name == instance.service_name && s.instance_id == instance.instance_id 
    })
    
    match existing {
      Some(index) => {
        // Update existing instance
        let updated_services = registry.services.slice(0, index) + 
                              [instance] + 
                              registry.services.slice(index + 1, registry.services.length())
        { registry | services: updated_services }
      }
      None => {
        // Add new instance
        { registry | services: registry.services.push(instance) }
      }
    }
  }
  
  // Discover service instances
  let discover_service = fn(registry: ServiceRegistry, service_name: String) {
    registry.services.filter(fn(s) { 
      s.service_name == service_name && s.health_status == "healthy"
    })
  }
  
  // Perform health check
  let perform_health_check = fn(registry: ServiceRegistry, current_time: Int) {
    let updated_services = registry.services.map(fn(service) {
      let time_since_heartbeat = current_time - service.last_heartbeat
      
      if time_since_heartbeat > registry.deregistration_timeout {
        // Mark as unhealthy
        { service | health_status: "unhealthy" }
      } else if time_since_heartbeat > registry.health_check_interval {
        // Mark as unknown (might be stale)
        { service | health_status: "unknown" }
      } else {
        service
      }
    })
    
    { registry | services: updated_services }
  }
  
  // Test service registry creation
  let registry = create_registry(30, 90)  // 30s health check, 90s timeout
  assert_eq(registry.services.length(), 0)
  assert_eq(registry.health_check_interval, 30)
  assert_eq(registry.deregistration_timeout, 90)
  
  // Test service registration
  let user_service = {
    service_name: "user-service",
    instance_id: "user-service-1",
    host: "10.0.1.10",
    port: 8080,
    protocol: "http",
    metadata: [("version", "1.2.3"), ("region", "us-west")],
    health_status: "healthy",
    last_heartbeat: 1640995200
  }
  
  let registry1 = register_service(registry, user_service)
  assert_eq(registry1.services.length(), 1)
  assert_eq(registry1.services[0].service_name, "user-service")
  
  // Test multiple instances of same service
  let user_service2 = {
    service_name: "user-service",
    instance_id: "user-service-2",
    host: "10.0.1.11",
    port: 8080,
    protocol: "http",
    metadata: [("version", "1.2.3"), ("region", "us-west")],
    health_status: "healthy",
    last_heartbeat: 1640995200
  }
  
  let registry2 = register_service(registry1, user_service2)
  assert_eq(registry2.services.length(), 2)
  
  // Test service discovery
  let user_instances = discover_service(registry2, "user-service")
  assert_eq(user_instances.length(), 2)
  
  let order_instances = discover_service(registry2, "order-service")
  assert_eq(order_instances.length(), 0)  // Service not registered
  
  // Test health check
  let current_time = 1640995350  // 150 seconds after registration
  let registry3 = perform_health_check(registry2, current_time)
  
  // Services should be marked as unhealthy (150s > 90s timeout)
  assert_eq(registry3.services[0].health_status, "unhealthy")
  assert_eq(registry3.services[1].health_status, "unhealthy")
  
  // Test discovery with unhealthy services
  let healthy_user_instances = discover_service(registry3, "user-service")
  assert_eq(healthy_user_instances.length(), 0)  // No healthy instances
  
  // Test service update
  let updated_user_service = {
    service_name: "user-service",
    instance_id: "user-service-1",
    host: "10.0.1.10",
    port: 8080,
    protocol: "http",
    metadata: [("version", "1.3.0"), ("region", "us-west")],  // Updated version
    health_status: "healthy",
    last_heartbeat: 1640995400
  }
  
  let registry4 = register_service(registry3, updated_user_service)
  assert_eq(registry4.services.length(), 2)
  
  // Find updated service
  let updated_service = registry4.services.find(fn(s) { s.instance_id == "user-service-1" })
  match updated_service {
    Some(service) => {
      assert_eq(service.metadata[0], ("version", "1.3.0"))
      assert_eq(service.health_status, "healthy")
      assert_eq(service.last_heartbeat, 1640995400)
    }
    None => assert_true(false)
  }
  
  // Test load balancing across service instances
  type LoadBalancingStrategy = {
    strategy_type: String,
    select_instance: (Array[ServiceInstance>) -> Option[ServiceInstance>
  }
  
  let round_robin = {
    strategy_type: "round_robin",
    select_instance: fn(instances: Array[ServiceInstance>) {
      // Simplified round-robin - just return first instance
      if instances.length() > 0 {
        Some(instances[0])
      } else {
        None
      }
    }
  }
  
  let random = {
    strategy_type: "random",
    select_instance: fn(instances: Array[ServiceInstance>) {
      if instances.length() > 0 {
        // Simplified random selection - return last instance
        Some(instances[instances.length() - 1])
      } else {
        None
      }
    }
  }
  
  // Test load balancing
  let healthy_registry = {
    services: [
      { user_service | last_heartbeat: 1640995400, health_status: "healthy" },
      { user_service2 | last_heartbeat: 1640995400, health_status: "healthy" }
    ],
    health_check_interval: 30,
    deregistration_timeout: 90
  }
  
  let healthy_instances = discover_service(healthy_registry, "user-service")
  assert_eq(healthy_instances.length(), 2)
  
  let round_robin_result = round_robin.select_instance(healthy_instances)
  match round_robin_result {
    Some(instance) => assert_eq(instance.instance_id, "user-service-1")
    None => assert_true(false)
  }
  
  let random_result = random.select_instance(healthy_instances)
  match random_result {
    Some(instance) => assert_eq(instance.instance_id, "user-service-2")
    None => assert_true(false)
  }
}

// Test 2: Traffic Management and Routing
test "traffic management and routing rules" {
  // Define routing rule
  type RoutingRule = {
    rule_id: String,
    name: String,
    priority: Int,
    match_criteria: Array[(String, String)>,  // Key-value pairs to match
    destination: String,
    weight: Int,
    timeout_ms: Int,
    retries: Int
  }
  
  // Define virtual service
  type VirtualService = {
    name: String,
    hosts: Array<String>,
    gateways: Array<String>,
    http: Array[RoutingRule>,
    tcp: Array[RoutingRule>
  }
  
  // Define destination rule
  type DestinationRule = {
    name: String,
    host: String,
    traffic_policy: {
      load_balancer: {
        strategy: String
      },
      connection_pool: {
        tcp: {
          max_connections: Int
        },
        http: {
          http1_max_pending_requests: Int,
          max_requests_per_connection: Int
        }
      }
    },
    subsets: Array<{
      name: String,
      labels: Array[(String, String)>
    }>
  }
  
  // Match routing rule
  let match_rule = fn(rule: RoutingRule, request_headers: Array[(String, String)>, 
                     request_path: String, request_method: String) -> Bool {
    // Check headers
    let header_matches = rule.match_criteria.all(fn(criteria) {
      if criteria.0.starts_with("header.") {
        let header_name = criteria.0.substring(7, criteria.0.length())
        request_headers.any(fn(header) { 
          header.0 == header_name && header.1.contains(criteria.1) 
        })
      } else {
        true  // Non-header criteria handled elsewhere
      }
    })
    
    // Check path
    let path_match = rule.match_criteria.any(fn(criteria) {
      if criteria.0 == "path" {
        request_path.contains(criteria.1)
      } else {
        false
      }
    })
    
    // Check method
    let method_match = rule.match_criteria.any(fn(criteria) {
      if criteria.0 == "method" {
        request_method == criteria.1
      } else {
        false
      }
    })
    
    let has_path_criteria = rule.match_criteria.any(fn(c) { c.0 == "path" })
    let has_method_criteria = rule.match_criteria.any(fn(c) { c.0 == "method" })
    
    header_matches && 
    (if has_path_criteria { path_match } else { true }) &&
    (if has_method_criteria { method_match } else { true })
  }
  
  // Find matching routing rule
  let find_matching_rule = fn(rules: Array[RoutingRule>, request_headers: Array[(String, String)>,
                            request_path: String, request_method: String) {
    let matching_rules = rules.filter(fn(rule) {
      match_rule(rule, request_headers, request_path, request_method)
    })
    
    // Sort by priority (higher priority first)
    let sorted_rules = matching_rules.sort(fn(a, b) {
      if a.priority > b.priority { -1 } else if a.priority < b.priority { 1 } else { 0 }
    })
    
    if sorted_rules.length() > 0 {
      Some(sorted_rules[0])
    } else {
      None
    }
  }
  
  // Create routing rules
  let routing_rules = [
    {
      rule_id: "user-service-v1",
      name: "User Service v1",
      priority: 100,
      match_criteria: [
        ("header.user-agent", "mobile"),
        ("path", "/api/users"),
        ("method", "GET")
      ],
      destination: "user-service-v1",
      weight: 80,
      timeout_ms: 5000,
      retries: 3
    },
    {
      rule_id: "user-service-v2",
      name: "User Service v2",
      priority: 90,
      match_criteria: [
        ("path", "/api/users"),
        ("method", "GET")
      ],
      destination: "user-service-v2",
      weight: 20,
      timeout_ms: 5000,
      retries: 3
    },
    {
      rule_id: "user-service-write",
      name: "User Service Write",
      priority: 80,
      match_criteria: [
        ("path", "/api/users"),
        ("method", "POST")
      ],
      destination: "user-service-v2",
      weight: 100,
      timeout_ms: 10000,
      retries: 2
    }
  ]
  
  // Test rule matching
  let mobile_headers = [("user-agent", "MobileApp/1.0"), ("authorization", "Bearer token")]
  let mobile_rule = find_matching_rule(routing_rules, mobile_headers, "/api/users/123", "GET")
  match mobile_rule {
    Some(rule) => {
      assert_eq(rule.rule_id, "user-service-v1")
      assert_eq(rule.destination, "user-service-v1")
      assert_eq(rule.weight, 80)
    }
    None => assert_true(false)
  }
  
  // Test fallback rule
  let desktop_headers = [("user-agent", "Chrome/91.0"), ("authorization", "Bearer token")]
  let desktop_rule = find_matching_rule(routing_rules, desktop_headers, "/api/users/123", "GET")
  match desktop_rule {
    Some(rule) => {
      assert_eq(rule.rule_id, "user-service-v2")
      assert_eq(rule.destination, "user-service-v2")
      assert_eq(rule.weight, 20)
    }
    None => assert_true(false)
  }
  
  // Test method-specific routing
  let write_rule = find_matching_rule(routing_rules, desktop_headers, "/api/users", "POST")
  match write_rule {
    Some(rule) => {
      assert_eq(rule.rule_id, "user-service-write")
      assert_eq(rule.destination, "user-service-v2")
      assert_eq(rule.timeout_ms, 10000)
    }
    None => assert_true(false)
  }
  
  // Test no matching rule
  let no_match_rule = find_matching_rule(routing_rules, desktop_headers, "/api/orders", "GET")
  assert_eq(no_match_rule, None)
  
  // Test weighted traffic splitting
  type WeightedDestination = {
    destination: String,
    weight: Int
  }
  
  let calculate_traffic_split = fn(rules: Array[RoutingRule>, request_headers: Array[(String, String)>,
                                request_path: String, request_method: String) {
    let matching_rules = rules.filter(fn(rule) {
      match_rule(rule, request_headers, request_path, request_method)
    })
    
    // Group by destination and sum weights
    let mut destinations = []
    let mut processed_destinations = []
    
    for rule in matching_rules {
      if not(processed_destinations.contains(rule.destination)) {
        processed_destinations = processed_destinations.push(rule.destination)
        
        let total_weight = matching_rules
          .filter(fn(r) { r.destination == rule.destination })
          .reduce(fn(sum, r) { sum + r.weight }, 0)
        
        destinations = destinations.push({
          destination: rule.destination,
          weight: total_weight
        })
      }
    }
    
    // Calculate percentages
    let total_weight = destinations.reduce(fn(sum, d) { sum + d.weight }, 0)
    
    destinations.map(fn(d) {
      {
        destination: d.destination,
        weight: d.weight,
        percentage: if total_weight > 0 {
          (d.weight as Float / total_weight as Float) * 100.0
        } else {
          0.0
        }
      }
    })
  }
  
  // Test traffic splitting
  let traffic_split = calculate_traffic_split(routing_rules, desktop_headers, "/api/users", "GET")
  assert_eq(traffic_split.length(), 1)  // Only user-service-v2 matches
  assert_eq(traffic_split[0].destination, "user-service-v2")
  assert_eq(traffic_split[0].percentage, 100.0)
  
  // Test destination rule with subsets
  let user_destination_rule = {
    name: "user-service",
    host: "user-service.default.svc.cluster.local",
    traffic_policy: {
      load_balancer: {
        strategy: "round_robin"
      },
      connection_pool: {
        tcp: {
          max_connections: 100
        },
        http: {
          http1_max_pending_requests: 50,
          max_requests_per_connection: 10
        }
      }
    },
    subsets: [
      {
        name: "v1",
        labels: [("version", "v1")]
      },
      {
        name: "v2",
        labels: [("version", "v2")]
      }
    ]
  }
  
  // Test subset selection
  let select_subset = fn(destination_rule: DestinationRule, version: String) {
    destination_rule.subsets.find(fn(subset) { subset.name == version })
  }
  
  let v1_subset = select_subset(user_destination_rule, "v1")
  match v1_subset {
    Some(subset) => {
      assert_eq(subset.name, "v1")
      assert_eq(subset.labels[0], ("version", "v1"))
    }
    None => assert_true(false)
  }
  
  let v3_subset = select_subset(user_destination_rule, "v3")
  assert_eq(v3_subset, None)  // v3 subset doesn't exist
  
  // Test circuit breaker integration
  type CircuitBreakerConfig = {
    destination: String,
    max_connections: Int,
    max_pending_requests: Int,
    max_requests: Int,
    max_retries: Int,
    consecutive_errors: Int,
    interval: Int,
    base_ejection_time: Int
  }
  
  let circuit_breaker_configs = [
    {
      destination: "user-service-v1",
      max_connections: 100,
      max_pending_requests: 50,
      max_requests: 1000,
      max_retries: 3,
      consecutive_errors: 5,
      interval: 30000,  // 30 seconds
      base_ejection_time: 30000  // 30 seconds
    },
    {
      destination: "user-service-v2",
      max_connections: 50,
      max_pending_requests: 25,
      max_requests: 500,
      max_retries: 2,
      consecutive_errors: 3,
      interval: 15000,  // 15 seconds
      base_ejection_time: 15000  // 15 seconds
    }
  ]
  
  // Find circuit breaker config for destination
  let find_circuit_breaker = fn(configs: Array[CircuitBreakerConfig>, destination: String) {
    configs.find(fn(config) { config.destination == destination })
  }
  
  let v1_circuit_breaker = find_circuit_breaker(circuit_breaker_configs, "user-service-v1")
  match v1_circuit_breaker {
    Some(config) => {
      assert_eq(config.max_connections, 100)
      assert_eq(config.consecutive_errors, 5)
    }
    None => assert_true(false)
  }
  
  let v3_circuit_breaker = find_circuit_breaker(circuit_breaker_configs, "user-service-v3")
  assert_eq(v3_circuit_breaker, None)  // No config for v3
}

// Test 3: Security and mTLS
test "security and mutual TLS (mTLS) configuration" {
  // Define certificate
  type Certificate = {
    serial_number: String,
    subject: String,
    issuer: String,
    not_before: Int,
    not_after: Int,
    public_key: String,
    signature_algorithm: String
  }
  
  // Define identity
  type Identity = {
    service_name: String,
    namespace: String,
    certificate_chain: Array[Certificate>
  }
  
  // Define mTLS policy
  type MtlsPolicy = {
    name: String,
    mode: String,  // "DISABLE", "PERMISSIVE", "STRICT"
    peer_certificate_validation: {
      validate_trust_chain: Bool,
      check_revocation: Bool,
      allowed_sans: Array[String>
    },
    certificate_rotation: {
      rotation_interval: Int,
      grace_period_ratio: Float
    }
  }
  
  // Validate certificate chain
  let validate_certificate_chain = fn(chain: Array[Certificate>, current_time: Int) {
    if chain.length() == 0 {
      { is_valid: false, error: "Empty certificate chain" }
    } else {
      // Check leaf certificate (first in chain)
      let leaf = chain[0]
      
      // Check expiration
      let is_expired = current_time < leaf.not_before || current_time > leaf.not_after
      if is_expired {
        { is_valid: false, error: "Certificate expired or not yet valid" }
      } else {
        // Simplified chain validation - check issuer-subject relationship
        let mut chain_valid = true
        let mut error_message = ""
        
        for i in 0..(chain.length() - 1) {
          let cert = chain[i]
          let issuer_cert = chain[i + 1]
          
          if cert.issuer != issuer_cert.subject {
            chain_valid = false
            error_message = "Invalid certificate chain: issuer-subject mismatch"
            break
          }
        }
        
        {
          is_valid: chain_valid,
          error: if chain_valid { "" } else { error_message }
        }
      }
    }
  }
  
  // Verify peer identity
  let verify_peer_identity = fn(peer_identity: Identity, mtls_policy: MtlsPolicy, 
                              current_time: Int) {
    let chain_validation = validate_certificate_chain(peer_identity.certificate_chain, current_time)
    
    if not(chain_validation.is_valid) {
      {
        is_verified: false,
        error: "Certificate validation failed: " + chain_validation.error
      }
    } else {
      // Check SANs (Subject Alternative Names)
      let leaf_cert = peer_identity.certificate_chain[0]
      let sans_valid = mtls_policy.peer_certificate_validation.allowed_sans.length() == 0 ||
                      mtls_policy.peer_certificate_validation.allowed_sans
                        .any(fn(san) { leaf_cert.subject.contains(san) })
      
      if not(sans_valid) {
        {
          is_verified: false,
          error: "Peer certificate contains unauthorized SAN"
        }
      } else {
        {
          is_verified: true,
          error: ""
        }
      }
    }
  }
  
  // Create sample certificates
  let current_time = 1640995200
  
  let root_cert = {
    serial_number: "ROOT-001",
    subject: "CN=Root CA",
    issuer: "CN=Root CA",
    not_before: 1609459200,  // 2021-01-01
    not_after: 1672531200,   // 2023-01-01
    public_key: "root-public-key",
    signature_algorithm: "SHA256-RSA"
  }
  
  let intermediate_cert = {
    serial_number: "INT-001",
    subject: "CN=Intermediate CA",
    issuer: "CN=Root CA",
    not_before: 1609459200,
    not_after: 1672531200,
    public_key: "intermediate-public-key",
    signature_algorithm: "SHA256-RSA"
  }
  
  let leaf_cert = {
    serial_number: "LEAF-001",
    subject: "CN=user-service.default.svc.cluster.local",
    issuer: "CN=Intermediate CA",
    not_before: 1609459200,
    not_after: 1672531200,
    public_key: "leaf-public-key",
    signature_algorithm: "SHA256-RSA"
  }
  
  // Test certificate chain validation
  let valid_chain = [leaf_cert, intermediate_cert, root_cert]
  let chain_validation = validate_certificate_chain(valid_chain, current_time)
  assert_true(chain_validation.is_valid)
  
  // Test invalid chain (wrong issuer)
  let invalid_intermediate = { intermediate_cert | issuer: "CN=Wrong CA" }
  let invalid_chain = [leaf_cert, invalid_intermediate, root_cert]
  let invalid_validation = validate_certificate_chain(invalid_chain, current_time)
  assert_false(invalid_validation.is_valid)
  assert_true(invalid_validation.error.contains("issuer-subject mismatch"))
  
  // Test expired certificate
  let expired_leaf = { leaf_cert | not_after: 1609459100 }  // Expired
  let expired_chain = [expired_leaf, intermediate_cert, root_cert]
  let expired_validation = validate_certificate_chain(expired_chain, current_time)
  assert_false(expired_validation.is_valid)
  assert_true(expired_validation.error.contains("expired"))
  
  // Create mTLS policy
  let mtls_policy = {
    name: "default-mtls-policy",
    mode: "STRICT",
    peer_certificate_validation: {
      validate_trust_chain: true,
      check_revocation: false,
      allowed_sans: [
        "user-service.default.svc.cluster.local",
        "order-service.default.svc.cluster.local"
      ]
    },
    certificate_rotation: {
      rotation_interval: 86400000,  // 24 hours
      grace_period_ratio: 0.1        // 10% of rotation interval
    }
  }
  
  // Test peer identity verification
  let user_service_identity = {
    service_name: "user-service",
    namespace: "default",
    certificate_chain: valid_chain
  }
  
  let identity_verification = verify_peer_identity(user_service_identity, mtls_policy, current_time)
  assert_true(identity_verification.is_verified)
  
  // Test unauthorized peer
  let unauthorized_leaf = { leaf_cert | 
    subject: "CN=unauthorized-service.default.svc.cluster.local" 
  }
  let unauthorized_chain = [unauthorized_leaf, intermediate_cert, root_cert]
  let unauthorized_identity = {
    service_name: "unauthorized-service",
    namespace: "default",
    certificate_chain: unauthorized_chain
  }
  
  let unauthorized_verification = verify_peer_identity(unauthorized_identity, mtls_policy, current_time)
  assert_false(unauthorized_verification.is_verified)
  assert_true(unauthorized_verification.error.contains("unauthorized SAN"))
  
  // Test certificate rotation scheduling
  type RotationSchedule = {
    next_rotation_time: Int,
    grace_period_end: Int,
    rotation_in_progress: Bool
  }
  
  let calculate_rotation_schedule = fn(certificate: Certificate, policy: MtlsPolicy, current_time: Int) {
    let rotation_interval = policy.certificate_rotation.rotation_interval
    let grace_period = (rotation_interval as Float * policy.certificate_rotation.grace_period_ratio) as Int
    
    // Calculate next rotation time (before expiration)
    let time_until_expiration = certificate.not_after - current_time
    let next_rotation = if time_until_expiration <= rotation_interval {
      current_time + 3600  // Rotate in 1 hour if expiring soon
    } else {
      certificate.not_after - rotation_interval
    }
    
    {
      next_rotation_time: next_rotation,
      grace_period_end: next_rotation + grace_period,
      rotation_in_progress: false
    }
  }
  
  // Test rotation scheduling
  let rotation_schedule = calculate_rotation_schedule(leaf_cert, mtls_policy, current_time)
  assert_true(rotation_schedule.next_rotation_time > current_time)
  assert_true(rotation_schedule.next_rotation_time < leaf_cert.not_after)
  assert_eq(rotation_schedule.grace_period_end, rotation_schedule.next_rotation_time + 8640000)  // 10% of 24h
  
  // Test authorization policy
  type AuthorizationPolicy = {
    name: String,
    action: String,  // "ALLOW", "DENY"
    rules: Array<{
      from: Array<{
        source: {
          principals: Array<String>
          namespaces: Array<String>
        }
      }>,
      to: Array<{
        operation: {
          methods: Array<String>
          paths: Array<String>
        }
      }>
    }>
  }
  
  // Check authorization
  let check_authorization = fn(policy: AuthorizationPolicy, source_identity: Identity, 
                              method: String, path: String) {
    let source_principal = source_identity.service_name + "." + source_identity.namespace
    
    // Check if source matches any rule
    let rule_matches = policy.rules.any(fn(rule) {
      let source_matches = rule.from.any(fn(from) {
        let principal_matches = from.source.principals.length() == 0 ||
                               from.source.principals.any(fn(principal) { 
                                 principal == source_principal || principal == "*" 
                               })
        
        let namespace_matches = from.source.namespaces.length() == 0 ||
                               from.source.namespaces.contains(source_identity.namespace)
        
        principal_matches && namespace_matches
      })
      
      let operation_matches = rule.to.any(fn(to) {
        let method_matches = to.operation.methods.length() == 0 ||
                             to.operation.methods.contains(method)
        
        let path_matches = to.operation.paths.length() == 0 ||
                           to.operation.paths.any(fn(p) { path.contains(p) })
        
        method_matches && path_matches
      })
      
      source_matches && operation_matches
    })
    
    if policy.action == "ALLOW" {
      rule_matches
    } else {  // DENY
      not(rule_matches)
    }
  }
  
  // Create authorization policy
  let auth_policy = {
    name: "user-service-authz",
    action: "ALLOW",
    rules: [
      {
        from: [
          {
            source: {
              principals: ["order-service.default"],
              namespaces: ["default", "production"]
            }
          }
        ],
        to: [
          {
            operation: {
              methods: ["GET"],
              paths: ["/api/users"]
            }
          }
        ]
      }
    ]
  }
  
  // Test authorization
  let order_service_identity = {
    service_name: "order-service",
    namespace: "default",
    certificate_chain: valid_chain
  }
  
  let authorized = check_authorization(auth_policy, order_service_identity, "GET", "/api/users/123")
  assert_true(authorized)
  
  let unauthorized_method = check_authorization(auth_policy, order_service_identity, "POST", "/api/users")
  assert_false(unauthorized_method)
  
  let unauthorized_path = check_authorization(auth_policy, order_service_identity, "GET", "/api/admin")
  assert_false(unauthorized_path)
  
  let unauthorized_service = {
    service_name: "external-service",
    namespace: "default",
    certificate_chain: valid_chain
  }
  
  let unauthorized_source = check_authorization(auth_policy, unauthorized_service, "GET", "/api/users/123")
  assert_false(unauthorized_source)
}

// Test 4: Observability and Telemetry Integration
test "observability and telemetry integration with service mesh" {
  // Define telemetry configuration
  type TelemetryConfig = {
    tracing: {
      sampling: Float,
      custom_tags: Array<(String, String)>
    },
    metrics: {
      prometheus: Bool,
      statsd: Bool,
      custom_dimensions: Array<String>
    },
    access_logging: {
      enabled: Bool,
      log_format: String,
      filter_headers: Array<String>
    }
  }
  
  // Define mesh telemetry data
  type MeshTelemetry = {
    request_id: String,
    source_service: String,
    source_pod: String,
    destination_service: String,
    destination_pod: String,
    request_protocol: String,
    request_method: String,
    request_path: String,
    request_headers: Array[(String, String)>,
    response_code: Int,
    response_flags: String,
    duration_ms: Int,
    bytes_sent: Int,
    bytes_received: Int,
    timestamp: Int
  }
  
  // Process mesh telemetry
  let process_mesh_telemetry = fn(raw_data: MeshTelemetry, config: TelemetryConfig) {
    // Add custom tags to tracing data
    let tracing_tags = config.tracing.custom_tags.map(fn(tag) {
      (tag.0, tag.1)
    })
    
    // Create metrics dimensions
    let metric_dimensions = [
      ("source_service", raw_data.source_service),
      ("destination_service", raw_data.destination_service),
      ("request_method", raw_data.request_method),
      ("response_code", raw_data.response_code.to_string())
    ] + config.metrics.custom_dimensions.map(fn(dim) {
      if dim == "request_protocol" {
        ("request_protocol", raw_data.request_protocol)
      } else if dim == "response_flags" {
        ("response_flags", raw_data.response_flags)
      } else {
        (dim, "unknown")
      }
    })
    
    // Create access log entry
    let log_entry = if config.access_logging.enabled {
      let filtered_headers = raw_data.request_headers.filter(fn(header) {
        not(config.access_logging.filter_headers.contains(header.0))
      })
      
      Some({
        timestamp: raw_data.timestamp,
        request_id: raw_data.request_id,
        source: raw_data.source_service + "(" + raw_data.source_pod + ")",
        destination: raw_data.destination_service + "(" + raw_data.destination_pod + ")",
        request: raw_data.request_method + " " + raw_data.request_path,
        response: raw_data.response_code.to_string(),
        duration: raw_data.duration_ms.to_string() + "ms",
        bytes: raw_data.bytes_sent.to_string() + "/" + raw_data.bytes_received.to_string()
      })
    } else {
      None
    }
    
    {
      tracing_data: {
        trace_id: raw_data.request_id,
        tags: tracing_tags,
        duration_ms: raw_data.duration_ms
      },
      metrics_data: {
        dimensions: metric_dimensions,
        values: [
          ("request_count", 1),
          ("request_duration_ms", raw_data.duration_ms),
          ("request_bytes", raw_data.bytes_received),
          ("response_bytes", raw_data.bytes_sent)
        ]
      },
      access_log: log_entry
    }
  }
  
  // Create sample telemetry data
  let sample_telemetry = {
    request_id: "trace-1234567890",
    source_service: "frontend",
    source_pod: "frontend-7d4f8c9b5-abcde",
    destination_service: "user-service",
    destination_pod: "user-service-6f5d4c3b2-12345",
    request_protocol: "HTTP/1.1",
    request_method: "GET",
    request_path: "/api/users/123",
    request_headers: [
      ("x-request-id", "req-0987654321"),
      ("user-agent", "Mozilla/5.0"),
      ("authorization", "Bearer ***")
    ],
    response_code: 200,
    response_flags: "-",
    duration_ms: 150,
    bytes_sent: 1024,
    bytes_received: 512,
    timestamp: 1640995200
  }
  
  // Create telemetry configuration
  let telemetry_config = {
    tracing: {
      sampling: 0.1,
      custom_tags: [
        ("mesh_version", "1.12.0"),
        ("cluster", "production")
      ]
    },
    metrics: {
      prometheus: true,
      statsd: false,
      custom_dimensions: ["request_protocol", "response_flags"]
    },
    access_logging: {
      enabled: true,
      log_format: "JSON",
      filter_headers: ["authorization", "cookie"]
    }
  }
  
  // Test telemetry processing
  let processed_telemetry = process_mesh_telemetry(sample_telemetry, telemetry_config)
  
  // Check tracing data
  assert_eq(processed_telemetry.tracing_data.trace_id, "trace-1234567890")
  assert_eq(processed_telemetry.tracing_data.tags.length(), 2)
  assert_eq(processed_telemetry.tracing_data.tags[0], ("mesh_version", "1.12.0"))
  assert_eq(processed_telemetry.tracing_data.duration_ms, 150)
  
  // Check metrics data
  assert_eq(processed_telemetry.metrics_data.dimensions.length(), 6)
  assert_true(processed_telemetry.metrics_data.dimensions.contains(("source_service", "frontend")))
  assert_true(processed_telemetry.metrics_data.dimensions.contains(("destination_service", "user-service")))
  assert_true(processed_telemetry.metrics_data.dimensions.contains(("request_protocol", "HTTP/1.1")))
  assert_true(processed_telemetry.metrics_data.dimensions.contains(("response_flags", "-")))
  
  assert_eq(processed_telemetry.metrics_data.values.length(), 4)
  assert_true(processed_telemetry.metrics_data.values.contains(("request_count", 1)))
  assert_true(processed_telemetry.metrics_data.values.contains(("request_duration_ms", 150)))
  
  // Check access log
  match processed_telemetry.access_log {
    Some(log_entry) => {
      assert_eq(log_entry.timestamp, 1640995200)
      assert_eq(log_entry.request_id, "trace-1234567890")
      assert_eq(log_entry.source, "frontend(frontend-7d4f8c9b5-abcde)")
      assert_eq(log_entry.destination, "user-service(user-service-6f5d4c3b2-12345)")
      assert_eq(log_entry.request, "GET /api/users/123")
      assert_eq(log_entry.response, "200")
      assert_eq(log_entry.duration, "150ms")
      assert_eq(log_entry.bytes, "1024/512")
    }
    None => assert_true(false)
  }
  
  // Test service mesh metrics aggregation
  type MeshMetrics = {
    total_requests: Int,
    error_rate: Float,
    average_latency_ms: Float,
    p95_latency_ms: Int,
    p99_latency_ms: Int,
    throughput_rps: Float
  }
  
  let aggregate_mesh_metrics = fn(telemetry_data: Array<MeshTelemetry>, time_window_seconds: Int) {
    if telemetry_data.length() == 0 {
      {
        total_requests: 0,
        error_rate: 0.0,
        average_latency_ms: 0.0,
        p95_latency_ms: 0,
        p99_latency_ms: 0,
        throughput_rps: 0.0
      }
    } else {
      let total_requests = telemetry_data.length()
      let error_count = telemetry_data.filter(fn(d) { d.response_code >= 400 }).length()
      let error_rate = (error_count as Float / total_requests as Float) * 100.0
      
      let latencies = telemetry_data.map(fn(d) { d.duration_ms })
      let sorted_latencies = latencies.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
      
      let average_latency = latencies.reduce(fn(sum, latency) { sum + latency }, 0) as Float / 
                           total_requests as Float
      
      let p95_index = ((total_requests as Float) * 0.95) as Int
      let p99_index = ((total_requests as Float) * 0.99) as Int
      
      let p95_latency = if p95_index < total_requests {
        sorted_latencies[p95_index]
      } else {
        sorted_latencies[total_requests - 1]
      }
      
      let p99_latency = if p99_index < total_requests {
        sorted_latencies[p99_index]
      } else {
        sorted_latencies[total_requests - 1]
      }
      
      // Calculate throughput
      let time_span = if telemetry_data.length() > 1 {
        telemetry_data[telemetry_data.length() - 1].timestamp - telemetry_data[0].timestamp
      } else {
        time_window_seconds
      }
      
      let throughput_rps = if time_span > 0 {
        total_requests as Float / time_span as Float
      } else {
        0.0
      }
      
      {
        total_requests,
        error_rate,
        average_latency_ms: average_latency,
        p95_latency_ms: p95_latency,
        p99_latency_ms: p99_latency,
        throughput_rps
      }
    }
  }
  
  // Create multiple telemetry samples
  let telemetry_samples = [
    sample_telemetry,
    { sample_telemetry | 
      request_id: "trace-1234567891",
      response_code: 500,
      duration_ms: 2000,
      timestamp: 1640995201
    },
    { sample_telemetry | 
      request_id: "trace-1234567892",
      response_code: 200,
      duration_ms: 120,
      timestamp: 1640995202
    },
    { sample_telemetry | 
      request_id: "trace-1234567893",
      response_code: 200,
      duration_ms: 80,
      timestamp: 1640995203
    },
    { sample_telemetry | 
      request_id: "trace-1234567894",
      response_code: 404,
      duration_ms: 50,
      timestamp: 1640995204
    }
  ]
  
  // Test metrics aggregation
  let mesh_metrics = aggregate_mesh_metrics(telemetry_samples, 60)
  assert_eq(mesh_metrics.total_requests, 5)
  assert_eq(mesh_metrics.error_rate, 40.0)  // 2 errors out of 5 requests
  assert_eq(mesh_metrics.average_latency_ms.round(), 480.0)  // (150 + 2000 + 120 + 80 + 50) / 5
  assert_eq(mesh_metrics.p95_latency_ms, 2000)  // 95th percentile
  assert_eq(mesh_metrics.p99_latency_ms, 2000)  // 99th percentile
  assert_eq(mesh_metrics.throughput_rps.round(), 0.83)  // 5 requests / 6 seconds
  
  // Test Golden Signals monitoring
  type GoldenSignals = {
    traffic: Float,      // Requests per second
    latency: {
      average: Float,
      p95: Float,
      p99: Float
    },
    errors: Float,       // Error rate percentage
    saturation: Float    // Resource utilization percentage
  }
  
  let calculate_golden_signals = fn(metrics: MeshMetrics, resource_metrics: {
    cpu_utilization: Float,
    memory_utilization: Float,
    network_utilization: Float
  }) {
    let traffic = metrics.throughput_rps
    let latency = {
      average: metrics.average_latency_ms,
      p95: metrics.p95_latency_ms as Float,
      p99: metrics.p99_latency_ms as Float
    }
    let errors = metrics.error_rate
    let saturation = (resource_metrics.cpu_utilization + 
                    resource_metrics.memory_utilization + 
                    resource_metrics.network_utilization) / 3.0
    
    {
      traffic,
      latency,
      errors,
      saturation
    }
  }
  
  // Test Golden Signals calculation
  let resource_metrics = {
    cpu_utilization: 65.0,
    memory_utilization: 70.0,
    network_utilization: 45.0
  }
  
  let golden_signals = calculate_golden_signals(mesh_metrics, resource_metrics)
  assert_eq(golden_signals.traffic.round(), 0.83)
  assert_eq(golden_signals.latency.average.round(), 480.0)
  assert_eq(golden_signals.latency.p95, 2000.0)
  assert_eq(golden_signals.errors, 40.0)
  assert_eq(golden_signals.saturation, 60.0)  // (65 + 70 + 45) / 3
  
  // Test SLI/SLO monitoring
  type ServiceLevelIndicator = {
    name: String,
    good_count: Int,
    total_count: Int,
    percentage: Float
  }
  
  type ServiceLevelObjective = {
    name: String,
    target_percentage: Float,
    time_window_seconds: Int,
    current_sli: ServiceLevelIndicator
  }
  
  let calculate_availability_sli = fn(telemetry_data: Array<MeshTelemetry>) {
    let total_requests = telemetry_data.length()
    let successful_requests = telemetry_data.filter(fn(d) { d.response_code < 500 }).length()
    
    let percentage = if total_requests > 0 {
      (successful_requests as Float / total_requests as Float) * 100.0
    } else {
      100.0
    }
    
    {
      name: "availability",
      good_count: successful_requests,
      total_count: total_requests,
      percentage
    }
  }
  
  let calculate_latency_sli = fn(telemetry_data: Array<MeshTelemetry>, threshold_ms: Int) {
    let total_requests = telemetry_data.length()
    let fast_requests = telemetry_data.filter(fn(d) { d.duration_ms <= threshold_ms }).length()
    
    let percentage = if total_requests > 0 {
      (fast_requests as Float / total_requests as Float) * 100.0
    } else {
      100.0
    }
    
    {
      name: "latency",
      good_count: fast_requests,
      total_count: total_requests,
      percentage
    }
  }
  
  // Test SLI calculation
  let availability_sli = calculate_availability_sli(telemetry_samples)
  assert_eq(availability_sli.name, "availability")
  assert_eq(availability_sli.good_count, 4)  // 4 successful requests (excluding 500 error)
  assert_eq(availability_sli.total_count, 5)
  assert_eq(availability_sli.percentage, 80.0)
  
  let latency_sli = calculate_latency_sli(telemetry_samples, 1000)  // 1 second threshold
  assert_eq(latency_sli.name, "latency")
  assert_eq(latency_sli.good_count, 4)  // 4 requests under 1 second (excluding 2000ms)
  assert_eq(latency_sli.total_count, 5)
  assert_eq(latency_sli.percentage, 80.0)
  
  // Test SLO compliance
  let availability_slo = {
    name: "availability-slo",
    target_percentage: 99.9,
    time_window_seconds: 86400,  // 24 hours
    current_sli: availability_sli
  }
  
  let latency_slo = {
    name: "latency-slo",
    target_percentage: 95.0,
    time_window_seconds: 3600,  // 1 hour
    current_sli: latency_sli
  }
  
  let check_slo_compliance = fn(slo: ServiceLevelObjective) {
    slo.current_sli.percentage >= slo.target_percentage
  }
  
  let availability_compliance = check_slo_compliance(availability_slo)
  assert_false(availability_compliance)  // 80% < 99.9%
  
  let latency_compliance = check_slo_compliance(latency_slo)
  assert_false(latency_compliance)  // 80% < 95.0%
}