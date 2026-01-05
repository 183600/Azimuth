// Distributed Systems Tests for Azimuth Telemetry System
// This file contains test cases for distributed systems functionality

// Test 1: Distributed Tracing Context Propagation
test "distributed tracing context propagation" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    baggage: Array[(String, String)]
  }
  
  // Create trace context
  let create_trace_context = fn(trace_id: String, span_id: String, parent_span_id: Option[String]) {
    {
      trace_id,
      span_id,
      parent_span_id,
      baggage: []
    }
  }
  
  // Add baggage item
  let add_baggage = fn(context: TraceContext, key: String, value: String) {
    let updated_baggage = context.baggage.push((key, value))
    { context | baggage: updated_baggage }
  }
  
  // Extract trace context from headers
  let extract_from_headers = fn(headers: Array[(String, String)]) {
    let mut trace_id = None
    let mut span_id = None
    let mut parent_span_id = None
    let mut baggage = []
    
    for (key, value) in headers {
      match key {
        "x-trace-id" => trace_id = Some(value)
        "x-span-id" => span_id = Some(value)
        "x-parent-span-id" => parent_span_id = Some(value)
        "x-baggage" => {
          // Parse baggage items (key=value, key2=value2)
          let items = value.split(",")
          for item in items {
            let parts = item.split("=")
            if parts.length() == 2 {
              baggage = baggage.push((parts[0], parts[1]))
            }
          }
        }
        _ => ()
      }
    }
    
    match (trace_id, span_id) {
      (Some(tid), Some(sid)) => Some({
        trace_id: tid,
        span_id: sid,
        parent_span_id,
        baggage
      })
      _ => None
    }
  }
  
  // Inject trace context into headers
  let inject_to_headers = fn(context: TraceContext) {
    let headers = [
      ("x-trace-id", context.trace_id),
      ("x-span-id", context.span_id)
    ]
    
    let headers_with_parent = match context.parent_span_id {
      Some(parent_id) => headers.push(("x-parent-span-id", parent_id))
      None => headers
    }
    
    // Add baggage
    let baggage_string = context.baggage.map(fn((k, v)) { k + "=" + v }).join(",")
    if baggage_string.length() > 0 {
      headers_with_parent.push(("x-baggage", baggage_string))
    } else {
      headers_with_parent
    }
  }
  
  // Test creating trace context
  let trace_context = create_trace_context("trace-12345", "span-67890", None)
  assert_eq(trace_context.trace_id, "trace-12345")
  assert_eq(trace_context.span_id, "span-67890")
  assert_eq(trace_context.parent_span_id, None)
  assert_eq(trace_context.baggage.length(), 0)
  
  // Test adding baggage
  let context_with_baggage = add_baggage(trace_context, "user.id", "user-123")
  let context_with_more_baggage = add_baggage(context_with_baggage, "service.version", "1.0.0")
  
  assert_eq(context_with_more_baggage.baggage.length(), 2)
  assert_true(context_with_more_baggage.baggage.contains(("user.id", "user-123")))
  assert_true(context_with_more_baggage.baggage.contains(("service.version", "1.0.0")))
  
  // Test injection and extraction
  let headers = inject_to_headers(context_with_more_baggage)
  
  assert_eq(headers.length(), 4)
  assert_true(headers.contains(("x-trace-id", "trace-12345")))
  assert_true(headers.contains(("x-span-id", "span-67890")))
  assert_true(headers.contains(("x-baggage", "user.id=user-123,service.version=1.0.0")))
  
  let extracted_context = extract_from_headers(headers)
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, "trace-12345")
      assert_eq(context.span_id, "span-67890")
      assert_eq(context.parent_span_id, None)
      assert_eq(context.baggage.length(), 2)
      assert_true(context.baggage.contains(("user.id", "user-123")))
      assert_true(context.baggage.contains(("service.version", "1.0.0")))
    }
    None => assert_true(false)
  }
  
  // Test parent span propagation
  let child_context = create_trace_context("trace-12345", "span-abcde", Some("span-67890"))
  let child_headers = inject_to_headers(child_context)
  
  assert_eq(child_headers.length(), 3)
  assert_true(child_headers.contains(("x-trace-id", "trace-12345")))
  assert_true(child_headers.contains(("x-span-id", "span-abcde")))
  assert_true(child_headers.contains(("x-parent-span-id", "span-67890")))
}

// Test 2: Service Mesh Communication
test "service mesh communication patterns" {
  // Define service endpoint
  type ServiceEndpoint = {
    name: String,
    host: String,
    port: Int,
    protocol: String
  }
  
  // Define service call
  type ServiceCall = {
    source: String,
    destination: ServiceEndpoint,
    method: String,
    path: String,
    headers: Array[(String, String)],
    timestamp: Int
  }
  
  // Create service endpoint
  let create_endpoint = fn(name: String, host: String, port: Int, protocol: String) {
    { name, host, port, protocol }
  }
  
  // Create service call
  let create_call = fn(source: String, destination: ServiceEndpoint, method: String, path: String, headers: Array[(String, String)], timestamp: Int) {
    {
      source,
      destination,
      method,
      path,
      headers,
      timestamp
    }
  }
  
  // Define service mesh
  type ServiceMesh = {
    services: Array[ServiceEndpoint],
    calls: Array[ServiceCall]
  }
  
  // Add service to mesh
  let add_service = fn(mesh: ServiceMesh, service: ServiceEndpoint) {
    let updated_services = mesh.services.push(service)
    { mesh | services: updated_services }
  }
  
  // Record service call
  let record_call = fn(mesh: ServiceMesh, call: ServiceCall) {
    let updated_calls = mesh.calls.push(call)
    { mesh | calls: updated_calls }
  }
  
  // Find service by name
  let find_service = fn(mesh: ServiceMesh, name: String) {
    mesh.services.find(fn(s) { s.name == name })
  }
  
  // Get calls from/to a service
  let get_service_calls = fn(mesh: ServiceMesh, service_name: String) {
    mesh.calls.filter(fn(c) { 
      c.source == service_name or c.destination.name == service_name 
    })
  }
  
  // Create service mesh
  let mut mesh = { services: [], calls: [] }
  
  // Add services
  let api_service = create_endpoint("api-service", "api.example.com", 8080, "http")
  let user_service = create_endpoint("user-service", "user.example.com", 8081, "http")
  let order_service = create_endpoint("order-service", "order.example.com", 8082, "http")
  let payment_service = create_endpoint("payment-service", "payment.example.com", 8083, "http")
  
  mesh = add_service(mesh, api_service)
  mesh = add_service(mesh, user_service)
  mesh = add_service(mesh, order_service)
  mesh = add_service(mesh, payment_service)
  
  // Verify services
  assert_eq(mesh.services.length(), 4)
  assert_eq(find_service(mesh, "api-service").unwrap().host, "api.example.com")
  assert_eq(find_service(mesh, "payment-service").unwrap().port, 8083)
  
  // Record service calls
  let call1 = create_call(
    "api-service",
    user_service,
    "GET",
    "/users/123",
    [("x-trace-id", "trace-123"), ("x-span-id", "span-456")],
    1640995200
  )
  
  let call2 = create_call(
    "api-service",
    order_service,
    "POST",
    "/orders",
    [("x-trace-id", "trace-123"), ("x-span-id", "span-789")],
    1640995205
  )
  
  let call3 = create_call(
    "order-service",
    payment_service,
    "POST",
    "/payments",
    [("x-trace-id", "trace-123"), ("x-span-id", "span-abc")],
    1640995210
  )
  
  mesh = record_call(mesh, call1)
  mesh = record_call(mesh, call2)
  mesh = record_call(mesh, call3)
  
  // Verify calls
  assert_eq(mesh.calls.length(), 3)
  
  // Get API service calls
  let api_calls = get_service_calls(mesh, "api-service")
  assert_eq(api_calls.length(), 2)
  assert_eq(api_calls[0].destination.name, "user-service")
  assert_eq(api_calls[1].destination.name, "order-service")
  
  // Get payment service calls
  let payment_calls = get_service_calls(mesh, "payment-service")
  assert_eq(payment_calls.length(), 1)
  assert_eq(payment_calls[0].source, "order-service")
  
  // Analyze call patterns
  let analyze_call_patterns = fn(mesh: ServiceMesh) {
    let mut patterns = []
    
    for service in mesh.services {
      let outgoing = mesh.calls.filter(fn(c) { c.source == service.name })
      let incoming = mesh.calls.filter(fn(c) { c.destination.name == service.name })
      
      patterns = patterns.push({
        service: service.name,
        outgoing_calls: outgoing.length(),
        incoming_calls: incoming.length(),
        total_calls: outgoing.length() + incoming.length()
      })
    }
    
    patterns
  }
  
  let patterns = analyze_call_patterns(mesh)
  
  // Verify patterns
  assert_eq(patterns.length(), 4)
  
  let api_pattern = patterns.find(fn(p) { p.service == "api-service" }).unwrap()
  assert_eq(api_pattern.outgoing_calls, 2)
  assert_eq(api_pattern.incoming_calls, 0)
  assert_eq(api_pattern.total_calls, 2)
  
  let payment_pattern = patterns.find(fn(p) { p.service == "payment-service" }).unwrap()
  assert_eq(payment_pattern.outgoing_calls, 0)
  assert_eq(payment_pattern.incoming_calls, 1)
  assert_eq(payment_pattern.total_calls, 1)
  
  let order_pattern = patterns.find(fn(p) { p.service == "order-service" }).unwrap()
  assert_eq(order_pattern.outgoing_calls, 1)
  assert_eq(order_pattern.incoming_calls, 1)
  assert_eq(order_pattern.total_calls, 2)
}

// Test 3: Distributed Consistency
test "distributed consistency mechanisms" {
  // Define consistency level
  enum ConsistencyLevel {
    Strong
    Eventual
    Weak
    ReadYourWrites
  }
  
  // Define data replica
  type DataReplica = {
    id: String,
    host: String,
    port: Int,
    last_updated: Int,
    version: Int,
    is_online: Bool
  }
  
  // Define replication operation
  type ReplicationOperation = {
    operation_id: String,
    key: String,
    value: String,
    replicas: Array[String],
    consistency_level: ConsistencyLevel,
    timestamp: Int
  }
  
  // Create data replica
  let create_replica = fn(id: String, host: String, port: Int) {
    {
      id,
      host,
      port,
      last_updated: 0,
      version: 0,
      is_online: true
    }
  }
  
  // Create replication operation
  let create_operation = fn(operation_id: String, key: String, value: String, replicas: Array[String], consistency_level: ConsistencyLevel, timestamp: Int) {
    {
      operation_id,
      key,
      value,
      replicas,
      consistency_level,
      timestamp
    }
  }
  
  // Define distributed system
  type DistributedSystem = {
    replicas: Array[DataReplica],
    operations: Array[ReplicationOperation],
    quorum_size: Int
  }
  
  // Add replica to system
  let add_replica = fn(system: DistributedSystem, replica: DataReplica) {
    let updated_replicas = system.replicas.push(replica)
    { system | replicas: updated_replicas }
  }
  
  // Record operation
  let record_operation = fn(system: DistributedSystem, operation: ReplicationOperation) {
    let updated_operations = system.operations.push(operation)
    { system | operations: updated_operations }
  }
  
  // Calculate quorum size
  let calculate_quorum = fn(replica_count: Int) {
    (replica_count / 2) + 1
  }
  
  // Check if operation meets consistency requirements
  let check_consistency = fn(system: DistributedSystem, operation: ReplicationOperation) {
    let required_replicas = match operation.consistency_level {
      ConsistencyLevel::Strong => system.replicas.length()
      ConsistencyLevel::ReadYourWrites => calculate_quorum(system.replicas.length())
      ConsistencyLevel::Eventual => calculate_quorum(system.replicas.length())
      ConsistencyLevel::Weak => 1
    }
    
    operation.replicas.length() >= required_replicas
  }
  
  // Create distributed system
  let replica1 = create_replica("replica-1", "db1.example.com", 5432)
  let replica2 = create_replica("replica-2", "db2.example.com", 5432)
  let replica3 = create_replica("replica-3", "db3.example.com", 5432)
  let replica4 = create_replica("replica-4", "db4.example.com", 5432)
  let replica5 = create_replica("replica-5", "db5.example.com", 5432)
  
  let quorum_size = calculate_quorum(5)
  let system = {
    replicas: [replica1, replica2, replica3, replica4, replica5],
    operations: [],
    quorum_size
  }
  
  // Verify quorum calculation
  assert_eq(quorum_size, 3)  // (5 / 2) + 1 = 3
  assert_eq(system.replicas.length(), 5)
  
  // Test consistency levels
  let strong_op = create_operation(
    "op-123",
    "user:123",
    "updated_data",
    ["replica-1", "replica-2", "replica-3", "replica-4", "replica-5"],
    ConsistencyLevel::Strong,
    1640995200
  )
  
  let eventual_op = create_operation(
    "op-456",
    "user:456",
    "more_data",
    ["replica-1", "replica-2", "replica-3"],
    ConsistencyLevel::Eventual,
    1640995205
  )
  
  let weak_op = create_operation(
    "op-789",
    "user:789",
    "less_data",
    ["replica-1"],
    ConsistencyLevel::Weak,
    1640995210
  )
  
  // Check consistency requirements
  assert_true(check_consistency(system, strong_op))  // All 5 replicas for strong
  assert_true(check_consistency(system, eventual_op))  // 3 replicas for eventual (quorum)
  assert_true(check_consistency(system, weak_op))  // 1 replica for weak
  
  // Test insufficient replicas
  let insufficient_strong = create_operation(
    "op-abc",
    "user:abc",
    "data",
    ["replica-1", "replica-2", "replica-3"],
    ConsistencyLevel::Strong,
    1640995215
  )
  
  assert_false(check_consistency(system, insufficient_strong))  // Only 3 replicas, need 5 for strong
  
  // Simulate replica failure
  let simulate_replica_failure = fn(system: DistributedSystem, replica_id: String) {
    let updated_replicas = system.replicas.map(fn(r) {
      if r.id == replica_id {
        { r | is_online: false }
      } else {
        r
      }
    })
    
    let online_replicas = updated_replicas.filter(fn(r) { r.is_online })
    let new_quorum = calculate_quorum(online_replicas.length())
    
    {
      replicas: updated_replicas,
      operations: system.operations,
      quorum_size: new_quorum
    }
  }
  
  let failed_system = simulate_replica_failure(system, "replica-3")
  let online_replicas = failed_system.replicas.filter(fn(r) { r.is_online })
  
  assert_eq(online_replicas.length(), 4)
  assert_eq(failed_system.quorum_size, 3)  // (4 / 2) + 1 = 3
  
  // Check consistency with failed replica
  assert_false(check_consistency(failed_system, strong_op))  // Only 4 online replicas, need 5 for strong
  assert_true(check_consistency(failed_system, eventual_op))  // 3 replicas still meet quorum
}

// Test 4: Circuit Breaker Pattern
test "circuit breaker pattern implementation" {
  // Define circuit breaker state
  enum CircuitState {
    Closed
    Open
    HalfOpen
  }
  
  // Define circuit breaker configuration
  type CircuitBreakerConfig = {
    failure_threshold: Int,
    recovery_timeout: Int,
    expected_exception_threshold: Int
  }
  
  // Define circuit breaker
  type CircuitBreaker = {
    name: String,
    state: CircuitState,
    failure_count: Int,
    last_failure_time: Int,
    success_count: Int,
    config: CircuitBreakerConfig
  }
  
  // Create circuit breaker
  let create_circuit_breaker = fn(name: String, config: CircuitBreakerConfig) {
    {
      name,
      state: CircuitState::Closed,
      failure_count: 0,
      last_failure_time: 0,
      success_count: 0,
      config
    }
  }
  
  // Record success
  let record_success = fn(breaker: CircuitBreaker) {
    match breaker.state {
      CircuitState::Closed => {
        { breaker | failure_count: 0, success_count: breaker.success_count + 1 }
      }
      CircuitState::HalfOpen => {
        if breaker.success_count + 1 >= breaker.config.expected_exception_threshold {
          { breaker | state: CircuitState::Closed, failure_count: 0, success_count: 0 }
        } else {
          { breaker | success_count: breaker.success_count + 1 }
        }
      }
      CircuitState::Open => breaker  // Can't succeed when open
    }
  }
  
  // Record failure
  let record_failure = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Closed => {
        let new_failure_count = breaker.failure_count + 1
        if new_failure_count >= breaker.config.failure_threshold {
          { breaker | 
            state: CircuitState::Open, 
            failure_count: new_failure_count, 
            last_failure_time: current_time,
            success_count: 0
          }
        } else {
          { breaker | failure_count: new_failure_count }
        }
      }
      CircuitState::HalfOpen => {
        { breaker | 
          state: CircuitState::Open, 
          failure_count: breaker.failure_count + 1, 
          last_failure_time: current_time,
          success_count: 0
        }
      }
      CircuitState::Open => {
        { breaker | last_failure_time: current_time }
      }
    }
  }
  
  // Check if circuit allows requests
  let allow_request = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Closed => true
      CircuitState::Open => {
        if current_time - breaker.last_failure_time >= breaker.config.recovery_timeout {
          false  // Should transition to half-open, but that's handled elsewhere
        } else {
          false
        }
      }
      CircuitState::HalfOpen => true
    }
  }
  
  // Try to transition to half-open
  let try_transition_to_half_open = fn(breaker: CircuitBreaker, current_time: Int) {
    match breaker.state {
      CircuitState::Open => {
        if current_time - breaker.last_failure_time >= breaker.config.recovery_timeout {
          { breaker | state: CircuitState::HalfOpen, success_count: 0 }
        } else {
          breaker
        }
      }
      _ => breaker
    }
  }
  
  // Create circuit breaker configuration
  let config = {
    failure_threshold: 3,
    recovery_timeout: 60000,  // 60 seconds in milliseconds
    expected_exception_threshold: 2
  }
  
  // Create circuit breaker
  let breaker = create_circuit_breaker("api-service", config)
  
  // Verify initial state
  match breaker.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(breaker.failure_count, 0)
  assert_eq(breaker.success_count, 0)
  
  // Record successes
  let mut updated_breaker = record_success(breaker)
  updated_breaker = record_success(updated_breaker)
  updated_breaker = record_success(updated_breaker)
  
  // Should still be closed with no failures
  match updated_breaker.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(updated_breaker.failure_count, 0)
  assert_eq(updated_breaker.success_count, 3)
  
  // Record failures to trip the circuit
  updated_breaker = record_failure(updated_breaker, 1640995200)
  updated_breaker = record_failure(updated_breaker, 1640995205)
  updated_breaker = record_failure(updated_breaker, 1640995210)
  
  // Should now be open
  match updated_breaker.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(updated_breaker.failure_count, 3)
  assert_eq(updated_breaker.last_failure_time, 1640995210)
  
  // Should not allow requests when open
  assert_false(allow_request(updated_breaker, 1640995215))
  
  // Try to transition to half-open (not enough time passed)
  let still_open = try_transition_to_half_open(updated_breaker, 1640995250)
  match still_open.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
  
  // Transition to half-open (enough time passed)
  let half_open = try_transition_to_half_open(updated_breaker, 1640995270)  // 60 seconds passed
  match half_open.state {
    CircuitState::HalfOpen => assert_true(true)
    _ => assert_true(false)
  }
  
  // Should allow requests when half-open
  assert_true(allow_request(half_open, 1640995275))
  
  // Record successes in half-open to close the circuit
  let closing_breaker = record_success(half_open)
  let closed_breaker = record_success(closing_breaker)
  
  // Should now be closed again
  match closed_breaker.state {
    CircuitState::Closed => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(closed_breaker.failure_count, 0)
  assert_eq(closed_breaker.success_count, 0)
  
  // Test failure in half-open reopens circuit
  let reopened = record_failure(half_open, 1640995280)
  match reopened.state {
    CircuitState::Open => assert_true(true)
    _ => assert_true(false)
  }
}

// Test 5: Distributed Locking
test "distributed locking mechanisms" {
  // Define lock
  type DistributedLock = {
    resource: String,
    owner: String,
    acquired_at: Int,
    expires_at: Int,
    is_active: Bool
  }
  
  // Define lock request
  type LockRequest = {
    resource: String,
    requester: String,
    ttl: Int,  // Time to live in seconds
    timestamp: Int
  }
  
  // Create lock
  let create_lock = fn(resource: String, owner: String, ttl: Int, current_time: Int) {
    {
      resource,
      owner,
      acquired_at: current_time,
      expires_at: current_time + ttl,
      is_active: true
    }
  }
  
  // Check if lock is expired
  let is_expired = fn(lock: DistributedLock, current_time: Int) {
    current_time >= lock.expires_at
  }
  
  // Try to acquire lock
  let try_acquire_lock = fn(existing_lock: Option[DistributedLock], request: LockRequest) {
    match existing_lock {
      Some(lock) => {
        if is_expired(lock, request.timestamp) {
          // Lock is expired, can acquire new one
          Some(create_lock(request.resource, request.requester, request.ttl, request.timestamp))
        } else {
          // Lock is still active
          None
        }
      }
      None => {
        // No existing lock, can acquire new one
        Some(create_lock(request.resource, request.requester, request.ttl, request.timestamp))
      }
    }
  }
  
  // Release lock
  let release_lock = fn(lock: DistributedLock, requester: String) {
    if lock.owner == requester and lock.is_active {
      { lock | is_active: false }
    } else {
      lock
    }
  }
  
  // Define lock manager
  type LockManager = {
    locks: Array[DistributedLock]
  }
  
  // Find lock for resource
  let find_lock = fn(manager: LockManager, resource: String) {
    manager.locks.find(fn(l) { l.resource == resource and l.is_active })
  }
  
  // Add or update lock
  let upsert_lock = fn(manager: LockManager, lock: DistributedLock) {
    let filtered = manager.locks.filter(fn(l) { not(l.resource == lock.resource and l.is_active) })
    { locks: filtered.push(lock) }
  }
  
  // Request lock
  let request_lock = fn(manager: LockManager, request: LockRequest) {
    let existing_lock = find_lock(manager, request.resource)
    let new_lock = try_acquire_lock(existing_lock, request)
    
    match new_lock {
      Some(lock) => {
        let updated_manager = upsert_lock(manager, lock)
        (updated_manager, Some(lock))
      }
      None => (manager, None)
    }
  }
  
  // Create lock manager
  let manager = { locks: [] }
  
  // Create lock requests
  let request1 = {
    resource: "resource-1",
    requester: "service-1",
    ttl: 60,  // 60 seconds
    timestamp: 1640995200
  }
  
  let request2 = {
    resource: "resource-1",
    requester: "service-2",
    ttl: 60,
    timestamp: 1640995205
  }
  
  let request3 = {
    resource: "resource-2",
    requester: "service-1",
    ttl: 60,
    timestamp: 1640995210
  }
  
  // Test initial lock acquisition
  let (manager1, lock1) = request_lock(manager, request1)
  
  match lock1 {
    Some(l) => {
      assert_eq(l.resource, "resource-1")
      assert_eq(l.owner, "service-1")
      assert_eq(l.acquired_at, 1640995200)
      assert_eq(l.expires_at, 1640995260)
      assert_true(l.is_active)
    }
    None => assert_true(false)
  }
  
  // Test lock contention
  let (manager2, lock2) = request_lock(manager1, request2)
  
  assert_eq(lock2, None)  // Should not get lock, already held by service-1
  
  // Test different resource
  let (manager3, lock3) = request_lock(manager2, request3)
  
  match lock3 {
    Some(l) => {
      assert_eq(l.resource, "resource-2")
      assert_eq(l.owner, "service-1")
      assert_eq(l.acquired_at, 1640995210)
      assert_eq(l.expires_at, 1640995270)
      assert_true(l.is_active)
    }
    None => assert_true(false)
  }
  
  // Test lock expiration
  let expired_request = {
    resource: "resource-1",
    requester: "service-2",
    ttl: 60,
    timestamp: 1640995270  // After first lock expires
  }
  
  let (manager4, lock4) = request_lock(manager3, expired_request)
  
  match lock4 {
    Some(l) => {
      assert_eq(l.resource, "resource-1")
      assert_eq(l.owner, "service-2")
      assert_eq(l.acquired_at, 1640995270)
      assert_eq(l.expires_at, 1640995330)
      assert_true(l.is_active)
    }
    None => assert_true(false)
  }
  
  // Test lock release
  match find_lock(manager4, "resource-2") {
    Some(lock) => {
      let released_lock = release_lock(lock, "service-1")
      assert_false(released_lock.is_active)
      
      let updated_manager = upsert_lock(manager4, released_lock)
      
      // Should be able to acquire released lock
      let release_request = {
        resource: "resource-2",
        requester: "service-3",
        ttl: 60,
        timestamp: 1640995275
      }
      
      let (_, new_lock) = request_lock(updated_manager, release_request)
      
      match new_lock {
        Some(l) => {
          assert_eq(l.resource, "resource-2")
          assert_eq(l.owner, "service-3")
          assert_true(l.is_active)
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 6: Event Sourcing
test "event sourcing patterns" {
  // Define event
  type Event = {
    id: String,
    aggregate_id: String,
    event_type: String,
    data: String,
    timestamp: Int,
    version: Int
  }
  
  // Define aggregate
  type Aggregate = {
    id: String,
    type: String,
    version: Int,
    data: String
  }
  
  // Create event
  let create_event = fn(id: String, aggregate_id: String, event_type: String, data: String, timestamp: Int, version: Int) {
    {
      id,
      aggregate_id,
      event_type,
      data,
      timestamp,
      version
    }
  }
  
  // Create aggregate
  let create_aggregate = fn(id: String, type: String) {
    {
      id,
      type,
      version: 0,
      data: "{}"
    }
  }
  
  // Apply event to aggregate
  let apply_event = fn(aggregate: Aggregate, event: Event) {
    if aggregate.id == event.aggregate_id and event.version == aggregate.version + 1 {
      {
        id: aggregate.id,
        type: aggregate.type,
        version: event.version,
        data: event.data  // In a real system, this would be more complex
      }
    } else {
      aggregate  // Event doesn't apply
    }
  }
  
  // Define event store
  type EventStore = {
    events: Array[Event]
  }
  
  // Save event
  let save_event = fn(store: EventStore, event: Event) {
    let updated_events = store.events.push(event)
    { events: updated_events }
  }
  
  // Get events for aggregate
  let get_events = fn(store: EventStore, aggregate_id: String) {
    store.events.filter(fn(e) { e.aggregate_id == aggregate_id })
  }
  
  // Rebuild aggregate from events
  let rebuild_aggregate = fn(store: EventStore, aggregate_id: String, aggregate_type: String) {
    let events = get_events(store, aggregate_id)
    let sorted_events = events.sort(fn(a, b) { a.version <= b.version })
    
    let mut aggregate = create_aggregate(aggregate_id, aggregate_type)
    
    for event in sorted_events {
      aggregate = apply_event(aggregate, event)
    }
    
    aggregate
  }
  
  // Create event store
  let store = { events: [] }
  
  // Create events for a user aggregate
  let user_created = create_event(
    "event-1",
    "user-123",
    "UserCreated",
    "{\"name\":\"John Doe\",\"email\":\"john@example.com\"}",
    1640995200,
    1
  )
  
  let user_updated = create_event(
    "event-2",
    "user-123",
    "UserUpdated",
    "{\"name\":\"John Smith\",\"email\":\"john.smith@example.com\"}",
    1640995210,
    2
  )
  
  let user_address_added = create_event(
    "event-3",
    "user-123",
    "UserAddressAdded",
    "{\"street\":\"123 Main St\",\"city\":\"Anytown\",\"state\":\"CA\",\"zip\":\"12345\"}",
    1640995220,
    3
  )
  
  // Save events
  let store1 = save_event(store, user_created)
  let store2 = save_event(store1, user_updated)
  let store3 = save_event(store2, user_address_added)
  
  // Verify events saved
  assert_eq(store3.events.length(), 3)
  
  // Get events for aggregate
  let user_events = get_events(store3, "user-123")
  assert_eq(user_events.length(), 3)
  
  // Rebuild aggregate from events
  let user_aggregate = rebuild_aggregate(store3, "user-123", "User")
  
  assert_eq(user_aggregate.id, "user-123")
  assert_eq(user_aggregate.type, "User")
  assert_eq(user_aggregate.version, 3)
  assert_eq(user_aggregate.data, "{\"street\":\"123 Main St\",\"city\":\"Anytown\",\"state\":\"CA\",\"zip\":\"12345\"}")
  
  // Test event ordering
  let sorted_events = user_events.sort(fn(a, b) { a.version <= b.version })
  assert_eq(sorted_events[0].event_type, "UserCreated")
  assert_eq(sorted_events[1].event_type, "UserUpdated")
  assert_eq(sorted_events[2].event_type, "UserAddressAdded")
  
  // Test concurrent events (should maintain version order)
  let concurrent_event = create_event(
    "event-4",
    "user-123",
    "UserPasswordChanged",
    "{\"password\":\"new_hashed_password\"}",
    1640995230,
    4
  )
  
  let store4 = save_event(store3, concurrent_event)
  let updated_user = rebuild_aggregate(store4, "user-123", "User")
  
  assert_eq(updated_user.version, 4)
  assert_eq(updated_user.data, "{\"password\":\"new_hashed_password\"}")
  
  // Test snapshot functionality
  let create_snapshot = fn(aggregate: Aggregate, timestamp: Int) {
    {
      aggregate_id: aggregate.id,
      aggregate_type: aggregate.type,
      data: aggregate.data,
      version: aggregate.version,
      timestamp
    }
  }
  
  let snapshot = create_snapshot(updated_user, 1640995240)
  
  assert_eq(snapshot.aggregate_id, "user-123")
  assert_eq(snapshot.aggregate_type, "User")
  assert_eq(snapshot.version, 4)
  assert_eq(snapshot.data, "{\"password\":\"new_hashed_password\"}")
}

// Test 7: Distributed Cache Coordination
test "distributed cache coordination" {
  // Define cache entry
  type CacheEntry = {
    key: String,
    value: String,
    timestamp: Int,
    ttl: Int,
    version: Int
  }
  
  // Define cache node
  type CacheNode = {
    id: String,
    host: String,
    port: Int,
    entries: Array[CacheEntry],
    is_active: Bool
  }
  
  // Define cache invalidation message
  type InvalidationMessage = {
    key: String,
    node_id: String,
    timestamp: Int,
    version: Int
  }
  
  // Create cache node
  let create_cache_node = fn(id: String, host: String, port: Int) {
    {
      id,
      host,
      port,
      entries: [],
      is_active: true
    }
  }
  
  // Create cache entry
  let create_cache_entry = fn(key: String, value: String, timestamp: Int, ttl: Int, version: Int) {
    {
      key,
      value,
      timestamp,
      ttl,
      version
    }
  }
  
  // Check if entry is expired
  let is_expired = fn(entry: CacheEntry, current_time: Int) {
    current_time >= entry.timestamp + entry.ttl
  }
  
  // Add entry to cache
  let add_entry = fn(node: CacheNode, entry: CacheEntry) {
    let filtered = node.entries.filter(fn(e) { not(e.key == entry.key) })
    let updated_entries = filtered.push(entry)
    { node | entries: updated_entries }
  }
  
  // Get entry from cache
  let get_entry = fn(node: CacheNode, key: String, current_time: Int) {
    node.entries.find(fn(e) { 
      e.key == key and not(is_expired(e, current_time))
    })
  }
  
  // Define cache cluster
  type CacheCluster = {
    nodes: Array[CacheNode],
    invalidation_messages: Array[InvalidationMessage]
  }
  
  // Add node to cluster
  let add_node = fn(cluster: CacheCluster, node: CacheNode) {
    let updated_nodes = cluster.nodes.push(node)
    { cluster | nodes: updated_nodes }
  }
  
  // Invalidate entry across cluster
  let invalidate_entry = fn(cluster: CacheCluster, key: String, node_id: String, timestamp: Int, version: Int) {
    let message = {
      key,
      node_id,
      timestamp,
      version
    }
    
    let updated_messages = cluster.invalidation_messages.push(message)
    
    // Apply invalidation to all nodes
    let updated_nodes = cluster.nodes.map(fn(node) {
      let filtered_entries = node.entries.filter(fn(e) { 
        not(e.key == key and e.version < version)
      })
      { node | entries: filtered_entries }
    })
    
    {
      nodes: updated_nodes,
      invalidation_messages: updated_messages
    }
  }
  
  // Get entry from cluster (try local node first, then other nodes)
  let get_cluster_entry = fn(cluster: CacheCluster, key: String, current_time: Int, preferred_node_id: String) {
    // Try preferred node first
    let preferred_node = cluster.nodes.find(fn(n) { n.id == preferred_node_id and n.is_active })
    
    match preferred_node {
      Some(node) => {
        match get_entry(node, key, current_time) {
          Some(entry) => Some(entry)
          None => {
            // Try other nodes
            let other_nodes = cluster.nodes.filter(fn(n) { n.id != preferred_node_id and n.is_active })
            
            let mut found_entry = None
            for node in other_nodes {
              match get_entry(node, key, current_time) {
                Some(entry) => {
                  found_entry = Some(entry)
                }
                None => ()
              }
            }
            
            found_entry
          }
        }
      }
      None => {
        // Preferred node not found, try any active node
        let active_nodes = cluster.nodes.filter(fn(n) { n.is_active })
        
        let mut found_entry = None
        for node in active_nodes {
          match get_entry(node, key, current_time) {
            Some(entry) => {
              found_entry = Some(entry)
            }
            None => ()
          }
        }
        
        found_entry
      }
    }
  }
  
  // Create cache cluster
  let node1 = create_cache_node("cache-1", "cache1.example.com", 11211)
  let node2 = create_cache_node("cache-2", "cache2.example.com", 11211)
  let node3 = create_cache_node("cache-3", "cache3.example.com", 11211)
  
  let cluster = {
    nodes: [node1, node2, node3],
    invalidation_messages: []
  }
  
  // Create cache entries
  let entry1 = create_cache_entry("user:123", "user_data_123", 1640995200, 3600, 1)
  let entry2 = create_cache_entry("user:456", "user_data_456", 1640995205, 3600, 1)
  let entry3 = create_cache_entry("product:789", "product_data_789", 1640995210, 3600, 1)
  
  // Add entries to different nodes
  let node1_with_entry = add_entry(cluster.nodes[0], entry1)
  let node2_with_entry = add_entry(cluster.nodes[1], entry2)
  let node3_with_entry = add_entry(cluster.nodes[2], entry3)
  
  let cluster_with_entries = {
    nodes: [node1_with_entry, node2_with_entry, node3_with_entry],
    invalidation_messages: cluster.invalidation_messages
  }
  
  // Test getting entries from preferred node
  let entry_from_node1 = get_cluster_entry(cluster_with_entries, "user:123", 1640995250, "cache-1")
  match entry_from_node1 {
    Some(entry) => {
      assert_eq(entry.key, "user:123")
      assert_eq(entry.value, "user_data_123")
      assert_eq(entry.version, 1)
    }
    None => assert_true(false)
  }
  
  // Test getting entries from other nodes when not in preferred
  let entry_from_other = get_cluster_entry(cluster_with_entries, "user:456", 1640995250, "cache-1")
  match entry_from_other {
    Some(entry) => {
      assert_eq(entry.key, "user:456")
      assert_eq(entry.value, "user_data_456")
      assert_eq(entry.version, 1)
    }
    None => assert_true(false)
  }
  
  // Test cache invalidation
  let updated_entry = create_cache_entry("user:123", "updated_user_data_123", 1640995300, 3600, 2)
  
  // Add updated entry to node2
  let node2_with_updated = add_entry(cluster_with_entries.nodes[1], updated_entry)
  
  let cluster_with_update = {
    nodes: [cluster_with_entries.nodes[0], node2_with_updated, cluster_with_entries.nodes[2]],
    invalidation_messages: cluster_with_entries.invalidation_messages
  }
  
  // Invalidate old version
  let cluster_after_invalidation = invalidate_entry(cluster_with_update, "user:123", "cache-2", 1640995305, 2)
  
  // Check that old version is invalidated
  let old_entry = get_cluster_entry(cluster_after_invalidation, "user:123", 1640995310, "cache-1")
  match old_entry {
    Some(entry) => {
      // Should get the updated version, not the old one
      assert_eq(entry.value, "updated_user_data_123")
      assert_eq(entry.version, 2)
    }
    None => assert_true(false)
  }
  
  // Test expired entry
  let expired_entry = create_cache_entry("temp:data", "temp_value", 1640995000, 300, 1)  // 5 minute TTL, expired
  
  let node_with_expired = add_entry(cluster_after_invalidation.nodes[0], expired_entry)
  let cluster_with_expired = {
    nodes: [node_with_expired, cluster_after_invalidation.nodes[1], cluster_after_invalidation.nodes[2]],
    invalidation_messages: cluster_after_invalidation.invalidation_messages
  }
  
  let expired_result = get_cluster_entry(cluster_with_expired, "temp:data", 1640995400, "cache-1")
  assert_eq(expired_result, None)  // Should be None because entry is expired
}

// Test 8: Leader Election
test "leader election in distributed systems" {
  // Define node state
  enum NodeState {
    Follower
    Candidate
    Leader
  }
  
  // Define node
  type Node = {
    id: String,
    host: String,
    port: Int,
    state: NodeState,
    term: Int,
    voted_for: Option[String],
    last_heartbeat: Int
  }
  
  // Define vote request
  type VoteRequest = {
    candidate_id: String,
    term: Int,
    last_log_index: Int,
    last_log_term: Int
  }
  
  // Define vote response
  type VoteResponse = {
    term: Int,
    vote_granted: Bool
  }
  
  // Create node
  let create_node = fn(id: String, host: String, port: Int) {
    {
      id,
      host,
      port,
      state: NodeState::Follower,
      term: 0,
      voted_for: None,
      last_heartbeat: 0
    }
  }
  
  // Start election
  let start_election = fn(node: Node, current_time: Int) {
    {
      node |
      state: NodeState::Candidate,
      term: node.term + 1,
      voted_for: Some(node.id),
      last_heartbeat: current_time
    }
  }
  
  // Become leader
  let become_leader = fn(node: Node, current_time: Int) {
    {
      node |
      state: NodeState::Leader,
      last_heartbeat: current_time
    }
  }
  
  // Become follower
  let become_follower = fn(node: Node, term: Int, current_time: Int) {
    {
      node |
      state: NodeState::Follower,
      term: term,
      voted_for: None,
      last_heartbeat: current_time
    }
  }
  
  // Handle vote request
  let handle_vote_request = fn(node: Node, request: VoteRequest, current_time: Int) {
    let mut vote_granted = false
    let mut updated_node = node
    
    if request.term > node.term {
      // Higher term, become follower
      updated_node = become_follower(node, request.term, current_time)
    }
    
    if (request.term >= updated_node.term and 
        (updated_node.voted_for.is_none() or updated_node.voted_for.unwrap() == request.candidate_id)) {
      vote_granted = true
      updated_node = { updated_node | voted_for: Some(request.candidate_id) }
    }
    
    (updated_node, {
      term: updated_node.term,
      vote_granted
    })
  }
  
  // Handle heartbeat
  let handle_heartbeat = fn(node: Node, leader_term: Int, current_time: Int) {
    if leader_term >= node.term {
      become_follower(node, leader_term, current_time)
    } else {
      node
    }
  }
  
  // Check if election should start
  let should_start_election = fn(node: Node, current_time: Int, election_timeout: Int) {
    node.state != NodeState::Leader and 
    current_time - node.last_heartbeat > election_timeout
  }
  
  // Define cluster
  type Cluster = {
    nodes: Array[Node]
  }
  
  // Create cluster
  let node1 = create_node("node-1", "node1.example.com", 8001)
  let node2 = create_node("node-2", "node2.example.com", 8002)
  let node3 = create_node("node-3", "node3.example.com", 8003)
  let node4 = create_node("node-4", "node4.example.com", 8004)
  let node5 = create_node("node-5", "node5.example.com", 8005)
  
  let cluster = {
    nodes: [node1, node2, node3, node4, node5]
  }
  
  // Start election on node1
  let candidate1 = start_election(cluster.nodes[0], 1640995200)
  
  match candidate1.state {
    NodeState::Candidate => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(candidate1.term, 1)
  assert_eq(candidate1.voted_for.unwrap(), "node-1")
  
  // Node1 requests votes from other nodes
  let vote_request = {
    candidate_id: "node-1",
    term: 1,
    last_log_index: 100,
    last_log_term: 0
  }
  
  // Process vote requests
  let mut responses = []
  let mut updated_nodes = [candidate1]
  
  for i in 1..cluster.nodes.length() {
    let (updated_node, response) = handle_vote_request(cluster.nodes[i], vote_request, 1640995205)
    updated_nodes = updated_nodes.push(updated_node)
    responses = responses.push(response)
  }
  
  // Count votes
  let votes_granted = responses.filter(fn(r) { r.vote_granted }).length()
  
  // Node1 voted for itself, so add 1
  let total_votes = votes_granted + 1
  
  // Should have majority (3 out of 5)
  assert_eq(total_votes, 3)
  
  // Node1 becomes leader
  let leader1 = become_leader(updated_nodes[0], 1640995210)
  
  match leader1.state {
    NodeState::Leader => assert_true(true)
    _ => assert_true(false)
  }
  
  // Leader sends heartbeats
  let mut heartbeat_nodes = [leader1]
  
  for i in 1..updated_nodes.length() {
    let follower = handle_heartbeat(updated_nodes[i], leader1.term, 1640995215)
    heartbeat_nodes = heartbeat_nodes.push(follower)
  }
  
  // Verify followers updated their terms and heartbeat times
  for i in 1..heartbeat_nodes.length() {
    match heartbeat_nodes[i].state {
      NodeState::Follower => assert_true(true)
      _ => assert_true(false)
    }
    assert_eq(heartbeat_nodes[i].term, 1)
    assert_eq(heartbeat_nodes[i].last_heartbeat, 1640995215)
  }
  
  // Simulate leader failure
  let election_timeout = 10000  // 10 seconds
  
  // Followers detect leader failure and start new election
  let mut new_candidates = []
  
  for i in 1..heartbeat_nodes.length() {
    if should_start_election(heartbeat_nodes[i], 1640995225, election_timeout) {
      let candidate = start_election(heartbeat_nodes[i], 1640995225)
      new_candidates = new_candidates.push(candidate)
    } else {
      new_candidates = new_candidates.push(heartbeat_nodes[i])
    }
  }
  
  // Should have at least one new candidate
  assert_true(new_candidates.filter(fn(n) { 
    match n.state {
      NodeState::Candidate => true
      _ => false
    }
  }).length() > 0)
  
  // Find the candidate with highest term
  let highest_term_candidate = new_candidates.reduce(fn(acc, node) {
    if node.term > acc.term {
      node
    } else {
      acc
    }
  }, new_candidates[0])
  
  // This candidate should win the election
  let new_leader = become_leader(highest_term_candidate, 1640995230)
  
  match new_leader.state {
    NodeState::Leader => assert_true(true)
    _ => assert_true(false)
  }
  assert_eq(new_leader.term, 2)  # Should be higher than previous term
}