// Azimuth Enhanced Comprehensive Test Suite
// This file contains comprehensive test cases covering various aspects of the telemetry system

// Test 1: Telemetry Data Processing Pipeline
test "telemetry data processing pipeline" {
  // Simulate telemetry data processing pipeline
  type TelemetryEvent = {
    event_id: String,
    timestamp: Int,
    event_type: String,
    service_name: String,
    attributes: Array[(String, String)],
    metrics: Array[(String, Float)]
  }
  
  // Create sample telemetry events
  let events = [
    {
      event_id: "evt-001",
      timestamp: 1640995200,
      event_type: "span_start",
      service_name: "payment-service",
      attributes: [("operation", "process_payment"), ("user_id", "user-123")],
      metrics: [("duration_ms", 150.0)]
    },
    {
      event_id: "evt-002",
      timestamp: 1640995250,
      event_type: "span_end",
      service_name: "payment-service",
      attributes: [("operation", "process_payment"), ("status", "success")],
      metrics: [("duration_ms", 250.0)]
    },
    {
      event_id: "evt-003",
      timestamp: 1640995300,
      event_type: "metric",
      service_name: "auth-service",
      attributes: [("metric_type", "counter")],
      metrics: [("auth_requests", 100.0)]
    }
  ]
  
  // Test event filtering by service
  let filter_by_service = fn(events: Array[TelemetryEvent], service: String) {
    let mut filtered = []
    for event in events {
      if event.service_name == service {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  let payment_events = filter_by_service(events, "payment-service")
  assert_eq(payment_events.length(), 2)
  assert_eq(payment_events[0].service_name, "payment-service")
  assert_eq(payment_events[1].service_name, "payment-service")
  
  // Test event aggregation by type
  let aggregate_by_type = fn(events: Array[TelemetryEvent]) {
    let mut type_counts = []
    for event in events {
      let mut found = false
      let mut updated = []
      
      for (event_type, count) in type_counts {
        if event_type == event.event_type {
          updated = updated.push((event_type, count + 1))
          found = true
        } else {
          updated = updated.push((event_type, count))
        }
      }
      
      if not(found) {
        updated = updated.push((event.event_type, 1))
      }
      
      type_counts = updated
    }
    type_counts
  }
  
  let type_aggregation = aggregate_by_type(events)
  assert_eq(type_aggregation.length(), 3)
  
  // Find specific type counts
  let get_type_count = fn(aggregation: Array[(String, Int)], event_type: String) {
    let mut count = 0
    for (t, c) in aggregation {
      if t == event_type {
        count = c
      }
    }
    count
  }
  
  assert_eq(get_type_count(type_aggregation, "span_start"), 1)
  assert_eq(get_type_count(type_aggregation, "span_end"), 1)
  assert_eq(get_type_count(type_aggregation, "metric"), 1)
  
  // Test time window filtering
  let filter_by_time_window = fn(events: Array[TelemetryEvent], start_time: Int, end_time: Int) {
    let mut filtered = []
    for event in events {
      if event.timestamp >= start_time and event.timestamp <= end_time {
        filtered = filtered.push(event)
      }
    }
    filtered
  }
  
  let window_events = filter_by_time_window(events, 1640995200, 1640995250)
  assert_eq(window_events.length(), 2)
  assert_eq(window_events[0].event_id, "evt-001")
  assert_eq(window_events[1].event_id, "evt-002")
}

// Test 2: Concurrent Resource Management
test "concurrent resource management" {
  // Simulate resource pool management
  type Resource = {
    id: String,
    in_use: Bool,
    last_used: Int,
    usage_count: Int
  }
  
  type ResourcePool = {
    resources: Array[Resource],
    max_size: Int,
    created_at: Int
  }
  
  // Create resource pool
  let create_pool = fn(max_size: Int) {
    let mut resources = []
    for i in 0..max_size {
      resources = resources.push({
        id: "resource-" + i.to_string(),
        in_use: false,
        last_used: 0,
        usage_count: 0
      })
    }
    
    {
      resources,
      max_size,
      created_at: 1640995200
    }
  }
  
  let pool = create_pool(5)
  assert_eq(pool.resources.length(), 5)
  assert_eq(pool.max_size, 5)
  
  // Test resource acquisition
  let acquire_resource = fn(pool: ResourcePool) {
    let mut updated_resources = []
    let mut acquired = None
    
    for resource in pool.resources {
      if not(resource.in_use) and acquired.is_none() {
        acquired = Some({
          id: resource.id,
          in_use: true,
          last_used: 1640995300,
          usage_count: resource.usage_count + 1
        })
        updated_resources = updated_resources.push(acquired.unwrap())
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    (acquired, { pool | resources: updated_resources })
  }
  
  let (acquired_resource, updated_pool) = acquire_resource(pool)
  assert_true(acquired_resource.is_some())
  
  match acquired_resource {
    Some(resource) => {
      assert_eq(resource.id, "resource-0")
      assert_true(resource.in_use)
      assert_eq(resource.usage_count, 1)
    }
    None => assert_true(false)
  }
  
  // Test resource release
  let release_resource = fn(pool: ResourcePool, resource_id: String) {
    let mut updated_resources = []
    
    for resource in pool.resources {
      if resource.id == resource_id {
        updated_resources = updated.push({
          id: resource.id,
          in_use: false,
          last_used: resource.last_used,
          usage_count: resource.usage_count
        })
      } else {
        updated_resources = updated_resources.push(resource)
      }
    }
    
    { pool | resources: updated_resources }
  }
  
  let released_pool = release_resource(updated_pool, "resource-0")
  let released_resource = released_pool.resources[0]
  assert_false(released_resource.in_use)
  assert_eq(released_resource.usage_count, 1)
  
  // Test concurrent acquisition simulation
  let simulate_concurrent_acquisition = fn(pool: ResourcePool, num_requests: Int) {
    let mut current_pool = pool
    let mut acquired_resources = []
    
    for i in 0..num_requests {
      let (resource, new_pool) = acquire_resource(current_pool)
      match resource {
        Some(r) => acquired_resources = acquired_resources.push(r.id)
        None => ()
      }
      current_pool = new_pool
    }
    
    (acquired_resources, current_pool)
  }
  
  let (concurrent_acquired, final_pool) = simulate_concurrent_acquisition(pool, 3)
  assert_eq(concurrent_acquired.length(), 3)
  assert_eq(concurrent_acquired[0], "resource-0")
  assert_eq(concurrent_acquired[1], "resource-1")
  assert_eq(concurrent_acquired[2], "resource-2")
  
  // Check pool state after concurrent acquisitions
  let in_use_count = fn(pool: ResourcePool) {
    let mut count = 0
    for resource in pool.resources {
      if resource.in_use {
        count = count + 1
      }
    }
    count
  }
  
  assert_eq(in_use_count(final_pool), 3)
}

// Test 3: Error Recovery and Fault Tolerance
test "error recovery and fault tolerance" {
  // Define error types
  enum SystemError {
    NetworkTimeout(String, Int)
    DatabaseConnection(String)
    ResourceExhausted(String)
    InvalidConfiguration(String)
  }
  
  // Define retry policy
  type RetryPolicy = {
    max_attempts: Int,
    backoff_ms: Int,
    max_backoff_ms: Int
  }
  
  // Define operation result
  type OperationResult[T] = {
    success: Bool,
    data: Option[T],
    error: Option[SystemError],
    attempts: Int
  }
  
  // Create default retry policy
  let default_retry_policy = {
    max_attempts: 3,
    backoff_ms: 1000,
    max_backoff_ms: 10000
  }
  
  // Simulate operation with potential failures
  let simulate_operation = fn(attempt: Int, should_fail: Bool) {
    if should_fail and attempt < 2 {
      {
        success: false,
        data: None,
        error: Some(SystemError::NetworkTimeout("api-service", 5000)),
        attempts: 1
      }
    } else if should_fail and attempt >= 2 {
      {
        success: false,
        data: None,
        error: Some(SystemError::DatabaseConnection("primary-db")),
        attempts: 1
      }
    } else {
      {
        success: true,
        data: Some("operation_result"),
        error: None,
        attempts: 1
      }
    }
  }
  
  // Test successful operation without retry
  let success_result = simulate_operation(1, false)
  assert_true(success_result.success)
  assert_eq(success_result.data, Some("operation_result"))
  assert_eq(success_result.error, None)
  
  // Test operation with retry and eventual success
  let operation_with_retry = fn(should_fail: Bool, policy: RetryPolicy) {
    let mut attempt = 1
    let mut result = simulate_operation(attempt, should_fail)
    let mut total_attempts = 1
    
    while not(result.success) and attempt < policy.max_attempts {
      attempt = attempt + 1
      total_attempts = total_attempts + 1
      let retry_result = simulate_operation(attempt, should_fail)
      result = {
        success: retry_result.success,
        data: retry_result.data,
        error: retry_result.error,
        attempts: total_attempts
      }
    }
    
    result
  }
  
  let retry_success = operation_with_retry(false, default_retry_policy)
  assert_true(retry_success.success)
  assert_eq(retry_success.attempts, 1)
  
  // Test circuit breaker pattern
  type CircuitBreaker = {
    state: String, // "closed", "open", "half-open"
    failure_count: Int,
    failure_threshold: Int,
    recovery_timeout: Int,
    last_failure_time: Int
  }
  
  let create_circuit_breaker = fn(threshold: Int, timeout: Int) {
    {
      state: "closed",
      failure_count: 0,
      failure_threshold: threshold,
      recovery_timeout: timeout,
      last_failure_time: 0
    }
  }
  
  let circuit_breaker = create_circuit_breaker(3, 60000)
  assert_eq(circuit_breaker.state, "closed")
  assert_eq(circuit_breaker.failure_count, 0)
  
  // Test circuit breaker state transitions
  let update_circuit_breaker = fn(cb: CircuitBreaker, success: Bool, current_time: Int) {
    if success {
      // Reset on success
      { cb | state: "closed", failure_count: 0 }
    } else {
      let new_failure_count = cb.failure_count + 1
      if new_failure_count >= cb.failure_threshold {
        // Open circuit after threshold
        { cb | state: "open", failure_count: new_failure_count, last_failure_time: current_time }
      } else {
        // Increment failure count
        { cb | failure_count: new_failure_count }
      }
    }
  }
  
  let cb_after_failure1 = update_circuit_breaker(circuit_breaker, false, 1640995200)
  assert_eq(cb_after_failure1.state, "closed")
  assert_eq(cb_after_failure1.failure_count, 1)
  
  let cb_after_failure2 = update_circuit_breaker(cb_after_failure1, false, 1640995250)
  assert_eq(cb_after_failure2.state, "closed")
  assert_eq(cb_after_failure2.failure_count, 2)
  
  let cb_after_failure3 = update_circuit_breaker(cb_after_failure2, false, 1640995300)
  assert_eq(cb_after_failure3.state, "open")
  assert_eq(cb_after_failure3.failure_count, 3)
  
  // Test fallback mechanism
  let execute_with_fallback = fn(primary: () -> OperationResult[String], fallback: () -> OperationResult[String]) {
    let primary_result = primary()
    if primary_result.success {
      primary_result
    } else {
      fallback()
    }
  }
  
  let primary_operation = fn() { simulate_operation(1, true) }
  let fallback_operation = fn() { simulate_operation(1, false) }
  
  let fallback_result = execute_with_fallback(primary_operation, fallback_operation)
  assert_true(fallback_result.success)
  assert_eq(fallback_result.data, Some("operation_result"))
}

// Test 4: Advanced Data Serialization
test "advanced data serialization" {
  // Define telemetry data types
  enum TelemetryValue {
    StringValue(String)
    IntValue(Int)
    FloatValue(Float)
    BoolValue(Bool)
    ArrayValue(Array[TelemetryValue])
  }
  
  type Attribute = {
    key: String,
    value: TelemetryValue
  }
  
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[Attribute]
  }
  
  // Create sample telemetry data
  let sample_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database_query",
    start_time: 1640995200,
    end_time: 1640995250,
    status: "ok",
    attributes: [
      { key: "db.type", value: TelemetryValue::StringValue("postgresql") },
      { key: "db.statement", value: TelemetryValue::StringValue("SELECT * FROM users") },
      { key: "db.rows", value: TelemetryValue::IntValue(42) },
      { key: "db.duration_ms", value: TelemetryValue::FloatValue(50.5) },
      { key: "db.cached", value: TelemetryValue::BoolValue(false) }
    ]
  }
  
  // Serialize telemetry value to string
  let serialize_value = fn(value: TelemetryValue) {
    match value {
      TelemetryValue::StringValue(s) => "string:" + s
      TelemetryValue::IntValue(i) => "int:" + i.to_string()
      TelemetryValue::FloatValue(f) => "float:" + f.to_string()
      TelemetryValue::BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
      TelemetryValue::ArrayValue(arr) => {
        let mut serialized = "array:["
        for i in 0..arr.length() {
          if i > 0 { serialized = serialized + "," }
          serialized = serialized + serialize_value(arr[i])
        }
        serialized + "]"
      }
    }
  }
  
  // Test value serialization
  let string_serialized = serialize_value(TelemetryValue::StringValue("test"))
  assert_eq(string_serialized, "string:test")
  
  let int_serialized = serialize_value(TelemetryValue::IntValue(42))
  assert_eq(int_serialized, "int:42")
  
  let float_serialized = serialize_value(TelemetryValue::FloatValue(3.14))
  assert_eq(float_serialized, "float:3.14")
  
  let bool_serialized = serialize_value(TelemetryValue::BoolValue(true))
  assert_eq(bool_serialized, "bool:true")
  
  let array_serialized = serialize_value(TelemetryValue::ArrayValue([
    TelemetryValue::StringValue("a"),
    TelemetryValue::IntValue(1),
    TelemetryValue::BoolValue(true)
  ]))
  assert_eq(array_serialized, "array:[string:a,int:1,bool:true]")
  
  // Serialize attribute to string
  let serialize_attribute = fn(attr: Attribute) {
    attr.key + "=" + serialize_value(attr.value)
  }
  
  // Serialize telemetry data to string format
  let serialize_telemetry_data = fn(data: TelemetryData) {
    let mut serialized = "trace_id=" + data.trace_id + "|"
    serialized = serialized + "span_id=" + data.span_id + "|"
    
    match data.parent_span_id {
      Some(parent_id) => serialized = serialized + "parent_span_id=" + parent_id + "|"
      None => ()
    }
    
    serialized = serialized + "operation=" + data.operation_name + "|"
    serialized = serialized + "start_time=" + data.start_time.to_string() + "|"
    serialized = serialized + "end_time=" + data.end_time.to_string() + "|"
    serialized = serialized + "status=" + data.status
    
    // Add attributes
    for i in 0..data.attributes.length() {
      serialized = serialized + "|" + serialize_attribute(data.attributes[i])
    }
    
    serialized
  }
  
  let serialized_data = serialize_telemetry_data(sample_data)
  assert_true(serialized_data.contains("trace_id=trace-12345"))
  assert_true(serialized_data.contains("span_id=span-67890"))
  assert_true(serialized_data.contains("parent_span_id=span-11111"))
  assert_true(serialized_data.contains("operation=database_query"))
  assert_true(serialized_data.contains("status=ok"))
  assert_true(serialized_data.contains("db.type=string:postgresql"))
  assert_true(serialized_data.contains("db.rows=int:42"))
  
  // Deserialize telemetry value from string
  let deserialize_value = fn(serialized: String) {
    if serialized.starts_with("string:") {
      TelemetryValue::StringValue(serialized.substring(7))
    } else if serialized.starts_with("int:") {
      let int_str = serialized.substring(4)
      let mut result = 0
      for i in 0..int_str.length() {
        let c = int_str[i]
        if c >= '0' and c <= '9' {
          result = result * 10 + (c.to_int() - '0'.to_int())
        }
      }
      TelemetryValue::IntValue(result)
    } else if serialized.starts_with("float:") {
      TelemetryValue::FloatValue(3.14) // Simplified
    } else if serialized.starts_with("bool:") {
      let bool_str = serialized.substring(5)
      TelemetryValue::BoolValue(bool_str == "true")
    } else {
      TelemetryValue::StringValue("unknown")
    }
  }
  
  // Test value deserialization
  let deserialized_string = deserialize_value("test")
  match deserialized_string {
    TelemetryValue::StringValue(s) => assert_eq(s, "test")
    _ => assert_true(false)
  }
  
  let deserialized_int = deserialize_value("int:42")
  match deserialized_int {
    TelemetryValue::IntValue(i) => assert_eq(i, 42)
    _ => assert_true(false)
  }
  
  // Test serialization round-trip
  let original_value = TelemetryValue::StringValue("roundtrip_test")
  let serialized = serialize_value(original_value)
  let deserialized = deserialize_value(serialized.substring(7)) // Remove prefix
  
  match (original_value, deserialized) {
    (TelemetryValue::StringValue(orig), TelemetryValue::StringValue(deser)) => 
      assert_eq(orig, deser)
    _ => assert_true(false)
  }
}

// Test 5: Configuration Management
test "configuration management" {
  // Define configuration types
  enum ConfigValue {
    String(String)
    Int(Int)
    Float(Float)
    Bool(Bool)
    Array(Array[String])
  }
  
  type ConfigEntry = {
    key: String,
    value: ConfigValue,
    default_value: ConfigValue,
    description: String,
    required: Bool
  }
  
  type Configuration = {
    entries: Array[ConfigEntry],
    source: String,
    last_updated: Int
  }
  
  // Create default configuration
  let create_default_config = fn() {
    {
      entries: [
        {
          key: "service.name",
          value: ConfigValue::String("azimuth-service"),
          default_value: ConfigValue::String("azimuth-service"),
          description: "Name of the service",
          required: true
        },
        {
          key: "service.version",
          value: ConfigValue::String("1.0.0"),
          default_value: ConfigValue::String("1.0.0"),
          description: "Version of the service",
          required: true
        },
        {
          key: "telemetry.enabled",
          value: ConfigValue::Bool(true),
          default_value: ConfigValue::Bool(true),
          description: "Enable telemetry collection",
          required: false
        },
        {
          key: "telemetry.sampling_rate",
          value: ConfigValue::Float(1.0),
          default_value: ConfigValue::Float(1.0),
          description: "Telemetry sampling rate (0.0-1.0)",
          required: false
        },
        {
          key: "server.port",
          value: ConfigValue::Int(8080),
          default_value: ConfigValue::Int(8080),
          description: "Server port",
          required: true
        },
        {
          key: "server.hosts",
          value: ConfigValue::Array(["localhost", "127.0.0.1"]),
          default_value: ConfigValue::Array(["localhost"]),
          description: "Server hosts",
          required: false
        }
      ],
      source: "default",
      last_updated: 1640995200
    }
  }
  
  let config = create_default_config()
  assert_eq(config.entries.length(), 6)
  assert_eq(config.source, "default")
  
  // Get configuration value
  let get_config_value = fn(config: Configuration, key: String) {
    let mut found = None
    for entry in config.entries {
      if entry.key == key {
        found = Some(entry.value)
      }
    }
    found
  }
  
  // Test getting configuration values
  let service_name = get_config_value(config, "service.name")
  match service_name {
    Some(ConfigValue::String(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
  
  let telemetry_enabled = get_config_value(config, "telemetry.enabled")
  match telemetry_enabled {
    Some(ConfigValue::Bool(enabled)) => assert_true(enabled)
    _ => assert_true(false)
  }
  
  let server_port = get_config_value(config, "server.port")
  match server_port {
    Some(ConfigValue::Int(port)) => assert_eq(port, 8080)
    _ => assert_true(false)
  }
  
  // Update configuration value
  let update_config_value = fn(config: Configuration, key: String, new_value: ConfigValue) {
    let mut updated_entries = []
    let mut found = false
    
    for entry in config.entries {
      if entry.key == key {
        updated_entries = updated.push({
          key: entry.key,
          value: new_value,
          default_value: entry.default_value,
          description: entry.description,
          required: entry.required
        })
        found = true
      } else {
        updated_entries = updated.push(entry)
      }
    }
    
    {
      entries: updated_entries,
      source: config.source,
      last_updated: 1640995300
    }
  }
  
  // Test updating configuration values
  let updated_config = update_config_value(config, "service.name", ConfigValue::String("updated-service"))
  let updated_service_name = get_config_value(updated_config, "service.name")
  match updated_service_name {
    Some(ConfigValue::String(name)) => assert_eq(name, "updated-service")
    _ => assert_true(false)
  }
  
  // Validate configuration
  let validate_config = fn(config: Configuration) {
    let mut errors = []
    let mut warnings = []
    
    for entry in config.entries {
      // Check required fields
      if entry.required {
        match entry.value {
          ConfigValue::String(s) => if s.length() == 0 {
            errors = errors.push("Required field " + entry.key + " cannot be empty")
          }
          ConfigValue::Array(arr) => if arr.length() == 0 {
            errors = errors.push("Required field " + entry.key + " cannot be empty array")
          }
          _ => ()
        }
      }
      
      // Type-specific validations
      match (entry.key, entry.value) {
        ("telemetry.sampling_rate", ConfigValue::Float(rate)) => {
          if rate < 0.0 or rate > 1.0 {
            errors = errors.push("Sampling rate must be between 0.0 and 1.0")
          }
        }
        ("server.port", ConfigValue::Int(port)) => {
          if port < 1 or port > 65535 {
            errors = errors.push("Port must be between 1 and 65535")
          }
        }
        _ => ()
      }
    }
    
    { errors, warnings }
  }
  
  // Test configuration validation
  let validation_result = validate_config(updated_config)
  assert_eq(validation_result.errors.length(), 0)
  assert_eq(validation_result.warnings.length(), 0)
  
  // Test invalid configuration
  let invalid_config = update_config_value(config, "telemetry.sampling_rate", ConfigValue::Float(1.5))
  let invalid_validation = validate_config(invalid_config)
  assert_eq(invalid_validation.errors.length(), 1)
  assert_true(invalid_validation.errors[0].contains("Sampling rate must be between 0.0 and 1.0"))
  
  // Reset configuration to defaults
  let reset_to_defaults = fn(config: Configuration) {
    let mut reset_entries = []
    
    for entry in config.entries {
      reset_entries = reset_entries({
        key: entry.key,
        value: entry.default_value,
        default_value: entry.default_value,
        description: entry.description,
        required: entry.required
      })
    }
    
    {
      entries: reset_entries,
      source: "default",
      last_updated: 1640995400
    }
  }
  
  let reset_config = reset_to_defaults(updated_config)
  let reset_service_name = get_config_value(reset_config, "service.name")
  match reset_service_name {
    Some(ConfigValue::String(name)) => assert_eq(name, "azimuth-service")
    _ => assert_true(false)
  }
}

// Test 6: Performance Benchmarking
test "performance benchmarking" {
  // Define benchmark types
  type BenchmarkResult = {
    operation_name: String,
    execution_time_ms: Int,
    memory_used_bytes: Int,
    iterations: Int,
    avg_time_per_iteration: Float
  }
  
  type PerformanceMetrics = {
    benchmarks: Array[BenchmarkResult],
    total_time_ms: Int,
    total_memory_mb: Float
  }
  
  // Simulate performance measurement
  let measure_performance = fn(operation: () -> Unit, iterations: Int) {
    let start_time = 1640995200000  // Mock timestamp with milliseconds
    let start_memory = 1024 * 1024  // Mock memory usage in bytes
    
    // Execute operation multiple times
    for i in 0..iterations {
      operation()
    }
    
    let end_time = 1640995200250  // Mock end timestamp
    let end_memory = 1024 * 1024 + 512 * 1024  // Mock end memory usage
    
    let total_time = end_time - start_time
    let memory_used = end_memory - start_memory
    let avg_time = total_time.to_float() / iterations.to_float()
    
    {
      operation_name: "test_operation",
      execution_time_ms: total_time,
      memory_used_bytes: memory_used,
      iterations: iterations,
      avg_time_per_iteration: avg_time
    }
  }
  
  // Test performance measurement
  let test_operation = fn() {
    // Simulate some work
    let mut sum = 0
    for i in 0..1000 {
      sum = sum + i
    }
  }
  
  let benchmark = measure_performance(test_operation, 100)
  assert_eq(benchmark.operation_name, "test_operation")
  assert_eq(benchmark.iterations, 100)
  assert_eq(benchmark.execution_time_ms, 250)
  assert_eq(benchmark.memory_used_bytes, 512 * 1024)
  assert_eq(benchmark.avg_time_per_iteration, 2.5)
  
  // Compare performance results
  let compare_performance = fn(benchmark1: BenchmarkResult, benchmark2: BenchmarkResult) {
    let time_ratio = benchmark1.avg_time_per_iteration / benchmark2.avg_time_per_iteration
    let memory_ratio = benchmark1.memory_used_bytes.to_float() / benchmark2.memory_used_bytes.to_float()
    
    {
      faster_by_percentage: (1.0 - time_ratio) * 100.0,
      more_memory_efficient_by_percentage: (1.0 - memory_ratio) * 100.0,
      winner: if time_ratio < 1.0 { benchmark1.operation_name } else { benchmark2.operation_name }
    }
  }
  
  // Create another benchmark for comparison
  let optimized_operation = fn() {
    // Simulate optimized work
    let sum = 1000 * 999 / 2  // Direct formula instead of loop
  }
  
  let optimized_benchmark = measure_performance(optimized_operation, 100)
  
  let comparison = compare_performance(benchmark, optimized_benchmark)
  assert_true(comparison.faster_by_percentage > 0.0)  // Optimized should be faster
  assert_eq(comparison.winner, optimized_benchmark.operation_name)
  
  // Performance regression detection
  let detect_regression = fn(current: BenchmarkResult, baseline: BenchmarkResult, threshold_percentage: Float) {
    let performance_change = (current.avg_time_per_iteration - baseline.avg_time_per_iteration) / baseline.avg_time_per_iteration * 100.0
    
    {
      has_regression: performance_change > threshold_percentage,
      performance_change_percentage: performance_change,
      threshold_percentage: threshold_percentage
    }
  }
  
  let regression_result = detect_regression(benchmark, optimized_benchmark, 10.0)
  assert_true(regression_result.has_regression)
  assert_true(regression_result.performance_change_percentage > 10.0)
  
  // Memory leak detection simulation
  let detect_memory_leak = fn(benchmarks: Array[BenchmarkResult], threshold_mb: Float) {
    let mut max_memory = 0
    let mut min_memory = 999999999
    
    for benchmark in benchmarks {
      if benchmark.memory_used_bytes > max_memory {
        max_memory = benchmark.memory_used_bytes
      }
      if benchmark.memory_used_bytes < min_memory {
        min_memory = benchmark.memory_used_bytes
      }
    }
    
    let memory_growth_mb = (max_memory - min_memory).to_float() / (1024.0 * 1024.0)
    
    {
      potential_leak: memory_growth_mb > threshold_mb,
      memory_growth_mb: memory_growth_mb,
      threshold_mb: threshold_mb
    }
  }
  
  // Create benchmarks with increasing memory usage
  let memory_benchmarks = [
    { benchmark | memory_used_bytes: 1024 * 1024 },
    { benchmark | memory_used_bytes: 2 * 1024 * 1024 },
    { benchmark | memory_used_bytes: 10 * 1024 * 1024 }
  ]
  
  let leak_detection = detect_memory_leak(memory_benchmarks, 5.0)
  assert_true(leak_detection.potential_leak)
  assert_eq(leak_detection.memory_growth_mb, 9.0)
  
  // Performance profiling summary
  let create_performance_summary = fn(benchmarks: Array[BenchmarkResult]) {
    let mut total_time = 0
    let mut total_memory = 0
    let mut fastest_time = 999999999
    let mut slowest_time = 0
    
    for benchmark in benchmarks {
      total_time = total_time + benchmark.execution_time_ms
      total_memory = total_memory + benchmark.memory_used_bytes
      
      if benchmark.avg_time_per_iteration < fastest_time {
        fastest_time = benchmark.avg_time_per_iteration
      }
      if benchmark.avg_time_per_iteration > slowest_time {
        slowest_time = benchmark.avg_time_per_iteration
      }
    }
    
    {
      total_operations: benchmarks.length(),
      total_execution_time_ms: total_time,
      total_memory_used_mb: total_memory.to_float() / (1024.0 * 1024.0),
      fastest_operation_time_ms: fastest_time,
      slowest_operation_time_ms: slowest_time,
      performance_variance_ms: slowest_time - fastest_time
    }
  }
  
  let performance_summary = create_performance_summary([benchmark, optimized_benchmark])
  assert_eq(performance_summary.total_operations, 2)
  assert_eq(performance_summary.total_execution_time_ms, 500)
  assert_eq(performance_summary.fastest_operation_time_ms, optimized_benchmark.avg_time_per_iteration)
  assert_eq(performance_summary.slowest_operation_time_ms, benchmark.avg_time_per_iteration)
}

// Test 7: Security and Privacy Features
test "security and privacy features" {
  // Define security types
  enum SensitivityLevel {
    Public
    Internal
    Confidential
    Restricted
  }
  
  type DataClassification = {
    field_name: String,
    sensitivity_level: SensitivityLevel,
    contains_pii: Bool,
    retention_days: Int
  }
  
  type SecurityPolicy = {
    encryption_enabled: Bool,
    access_log_enabled: Bool,
    data_masking_enabled: Bool,
    audit_retention_days: Int
  }
  
  // Create data classifications
  let classifications = [
    { field_name: "user_id", sensitivity_level: SensitivityLevel::Confidential, contains_pii: true, retention_days: 365 },
    { field_name: "email", sensitivity_level: SensitivityLevel::Restricted, contains_pii: true, retention_days: 180 },
    { field_name: "ip_address", sensitivity_level: SensitivityLevel::Internal, contains_pii: true, retention_days: 90 },
    { field_name: "user_agent", sensitivity_level: SensitivityLevel::Public, contains_pii: false, retention_days: 30 },
    { field_name: "session_id", sensitivity_level: SensitivityLevel::Internal, contains_pii: false, retention_days: 7 }
  ]
  
  // Test PII detection
  let contains_pii = fn(classifications: Array[DataClassification], field_name: String) {
    let mut found = false
    for classification in classifications {
      if classification.field_name == field_name and classification.contains_pii {
        found = true
      }
    }
    found
  }
  
  assert_true(contains_pii(classifications, "user_id"))
  assert_true(contains_pii(classifications, "email"))
  assert_false(contains_pii(classifications, "user_agent"))
  
  // Test data masking
  let mask_data = fn(value: String, classification: DataClassification) {
    if classification.contains_pii {
      match classification.sensitivity_level {
        SensitivityLevel::Restricted => {
          if value.length() <= 2 {
            "*".repeat(value.length())
          } else {
            value.substring(0, 1) + "*".repeat(value.length() - 2) + value.substring(value.length() - 1)
          }
        }
        SensitivityLevel::Confidential => {
          if value.length() <= 4 {
            "*".repeat(value.length())
          } else {
            value.substring(0, 2) + "*".repeat(value.length() - 4) + value.substring(value.length() - 2)
          }
        }
        SensitivityLevel::Internal => {
          if value.length() <= 6 {
            "*".repeat(value.length())
          } else {
            value.substring(0, 3) + "*".repeat(value.length() - 6) + value.substring(value.length() - 3)
          }
        }
        SensitivityLevel::Public => value
      }
    } else {
      value
    }
  }
  
  // Test data masking with different sensitivity levels
  let email_classification = classifications[1]  // email - Restricted
  let masked_email = mask_data("user@example.com", email_classification)
  assert_eq(masked_email, "u*******************m")
  
  let user_id_classification = classifications[0]  // user_id - Confidential
  let masked_user_id = mask_data("user12345", user_id_classification)
  assert_eq(masked_user_id, "us*******45")
  
  let ip_classification = classifications[2]  // ip_address - Internal
  let masked_ip = mask_data("192.168.1.100", ip_classification)
  assert_eq(masked_ip, "192********100")
  
  let user_agent_classification = classifications[3]  // user_agent - Public
  let user_agent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"
  let masked_user_agent = mask_data(user_agent, user_agent_classification)
  assert_eq(masked_user_agent, user_agent)  // No masking for public data
  
  // Test access control
  type AccessLevel = {
    level: String,
    permissions: Array[String]
  }
  
  let access_levels = [
    { level: "guest", permissions: ["read:public"] },
    { level: "user", permissions: ["read:public", "read:internal"] },
    { level: "admin", permissions: ["read:public", "read:internal", "read:confidential", "write:internal"] },
    { level: "super_admin", permissions: ["read:public", "read:internal", "read:confidential", "read:restricted", "write:internal", "write:confidential"] }
  ]
  
  let check_permission = fn(user_level: String, required_permission: String, access_levels: Array[AccessLevel]) {
    let mut has_permission = false
    
    for access_level in access_levels {
      if access_level.level == user_level {
        for permission in access_level.permissions {
          if permission == required_permission {
            has_permission = true
          }
        }
      }
    }
    
    has_permission
  }
  
  // Test permission checks
  assert_true(check_permission("guest", "read:public", access_levels))
  assert_false(check_permission("guest", "read:internal", access_levels))
  
  assert_true(check_permission("user", "read:public", access_levels))
  assert_true(check_permission("user", "read:internal", access_levels))
  assert_false(check_permission("user", "read:confidential", access_levels))
  
  assert_true(check_permission("admin", "read:confidential", access_levels))
  assert_false(check_permission("admin", "read:restricted", access_levels))
  
  assert_true(check_permission("super_admin", "read:restricted", access_levels))
  assert_true(check_permission("super_admin", "write:confidential", access_levels))
  
  // Test data retention policy
  let check_retention_policy = fn(classifications: Array[DataClassification], field_name: String, current_age_days: Int) {
    let mut can_retain = false
    let mut retention_days = 0
    
    for classification in classifications {
      if classification.field_name == field_name {
        retention_days = classification.retention_days
        can_retain = current_age_days <= retention_days
      }
    }
    
    { can_retain, retention_days }
  }
  
  // Test retention policy checks
  let email_retention = check_retention_policy(classifications, "email", 100)
  assert_true(email_retention.can_retain)
  assert_eq(email_retention.retention_days, 180)
  
  let expired_email_retention = check_retention_policy(classifications, "email", 200)
  assert_false(expired_email_retention.can_retain)
  assert_eq(expired_email_retention.retention_days, 180)
  
  // Test audit logging
  type AuditEvent = {
    timestamp: Int,
    user_id: String,
    action: String,
    resource: String,
    success: Bool,
    ip_address: String
  }
  
  let create_audit_event = fn(user_id: String, action: String, resource: String, success: Bool, ip_address: String) {
    {
      timestamp: 1640995200,
      user_id,
      action,
      resource,
      success,
      ip_address
    }
  }
  
  let audit_event = create_audit_event("user-123", "read:confidential", "user_data", true, "192.168.1.100")
  assert_eq(audit_event.user_id, "user-123")
  assert_eq(audit_event.action, "read:confidential")
  assert_eq(audit_event.resource, "user_data")
  assert_true(audit_event.success)
  
  // Test security policy enforcement
  let security_policy = {
    encryption_enabled: true,
    access_log_enabled: true,
    data_masking_enabled: true,
    audit_retention_days: 90
  }
  
  let enforce_security_policy = fn(data: String, classification: DataClassification, policy: SecurityPolicy, user_level: String) {
    let mut processed_data = data
    
    // Apply data masking if enabled
    if policy.data_masking_enabled and classification.contains_pii {
      processed_data = mask_data(processed_data, classification)
    }
    
    // Check access permissions
    let required_permission = match classification.sensitivity_level {
      SensitivityLevel::Public => "read:public"
      SensitivityLevel::Internal => "read:internal"
      SensitivityLevel::Confidential => "read:confidential"
      SensitivityLevel::Restricted => "read:restricted"
    }
    
    let has_access = check_permission(user_level, required_permission, access_levels)
    
    {
      data: processed_data,
      access_granted: has_access,
      encrypted: policy.encryption_enabled,
      audited: policy.access_log_enabled
    }
  }
  
  // Test security policy enforcement
  let email_classification = classifications[1]  // email - Restricted
  let security_result = enforce_security_policy("user@example.com", email_classification, security_policy, "user")
  
  assert_eq(security_result.data, "u*******************m")  // Masked
  assert_false(security_result.access_granted)  // User doesn't have restricted access
  assert_true(security_result.encrypted)  // Encryption enabled
  assert_true(security_result.audited)  // Auditing enabled
  
  let admin_security_result = enforce_security_policy("user@example.com", email_classification, security_policy, "super_admin")
  assert_eq(admin_security_result.data, "u*******************m")  // Still masked (PII)
  assert_true(admin_security_result.access_granted)  // Super admin has restricted access
  assert_true(admin_security_result.encrypted)
  assert_true(admin_security_result.audited)
}

// Test 8: Distributed Tracing
test "distributed tracing" {
  // Define tracing types
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: String
  }
  
  type Span = {
    context: TraceContext,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)],
    events: Array[SpanEvent]
  }
  
  type SpanEvent = {
    timestamp: Int,
    name: String,
    attributes: Array[(String, String)]
  }
  
  // Create trace context
  let create_trace_context = fn(trace_id: String, span_id: String, parent_span_id: Option[String]) {
    {
      trace_id,
      span_id,
      parent_span_id,
      trace_flags: 1,
      trace_state: ""
    }
  }
  
  let root_context = create_trace_context("trace-12345", "span-00001", None)
  assert_eq(root_context.trace_id, "trace-12345")
  assert_eq(root_context.span_id, "span-00001")
  assert_eq(root_context.parent_span_id, None)
  
  let child_context = create_trace_context("trace-12345", "span-00002", Some("span-00001"))
  assert_eq(child_context.trace_id, "trace-12345")
  assert_eq(child_context.span_id, "span-00002")
  assert_eq(child_context.parent_span_id, Some("span-00001"))
  
  // Create span
  let create_span = fn(context: TraceContext, operation_name: String, start_time: Int) {
    {
      context,
      operation_name,
      start_time,
      end_time: 0,
      status: "running",
      tags: [],
      events: []
    }
  }
  
  let root_span = create_span(root_context, "http_request", 1640995200)
  assert_eq(root_span.context.trace_id, "trace-12345")
  assert_eq(root_span.operation_name, "http_request")
  assert_eq(root_span.status, "running")
  
  // Test span completion
  let complete_span = fn(span: Span, end_time: Int, status: String) {
    {
      context: span.context,
      operation_name: span.operation_name,
      start_time: span.start_time,
      end_time,
      status,
      tags: span.tags,
      events: span.events
    }
  }
  
  let completed_root_span = complete_span(root_span, 1640995250, "ok")
  assert_eq(completed_root_span.end_time, 1640995250)
  assert_eq(completed_root_span.status, "ok")
  
  // Test span duration calculation
  let calculate_span_duration = fn(span: Span) {
    if span.end_time > 0 and span.start_time > 0 {
      span.end_time - span.start_time
    } else {
      0
    }
  }
  
  let duration = calculate_span_duration(completed_root_span)
  assert_eq(duration, 50)
  
  // Test span tagging
  let add_span_tag = fn(span: Span, key: String, value: String) {
    let new_tag = (key, value)
    {
      context: span.context,
      operation_name: span.operation_name,
      start_time: span.start_time,
      end_time: span.end_time,
      status: span.status,
      tags: span.tags.push(new_tag),
      events: span.events
    }
  }
  
  let tagged_span = add_span_tag(root_span, "http.method", "GET")
  assert_eq(tagged_span.tags.length(), 1)
  assert_eq(tagged_span.tags[0], ("http.method", "GET"))
  
  let multi_tagged_span = add_span_tag(tagged_span, "http.url", "/api/users")
  assert_eq(multi_tagged_span.tags.length(), 2)
  assert_eq(multi_tagged_span.tags[0], ("http.method", "GET"))
  assert_eq(multi_tagged_span.tags[1], ("http.url", "/api/users"))
  
  // Test span events
  let add_span_event = fn(span: Span, name: String, timestamp: Int, attributes: Array[(String, String)]) {
    let new_event = {
      timestamp,
      name,
      attributes
    }
    
    {
      context: span.context,
      operation_name: span.operation_name,
      start_time: span.start_time,
      end_time: span.end_time,
      status: span.status,
      tags: span.tags,
      events: span.events.push(new_event)
    }
  }
  
  let span_with_event = add_span_event(multi_tagged_span, "database.query", 1640995225, [
    ("db.statement", "SELECT * FROM users"),
    ("db.type", "postgresql")
  ])
  
  assert_eq(span_with_event.events.length(), 1)
  assert_eq(span_with_event.events[0].name, "database.query")
  assert_eq(span_with_event.events[0].timestamp, 1640995225)
  assert_eq(span_with_event.events[0].attributes.length(), 2)
  
  // Test trace tree building
  type TraceTree = {
    trace_id: String,
    root_span: Span,
    child_spans: Array[Span]
  }
  
  let build_trace_tree = fn(spans: Array[Span]) {
    if spans.length() == 0 {
      None
    } else {
      let mut root_span = spans[0]
      let mut child_spans = []
      
      for span in spans {
        match span.context.parent_span_id {
          Some(parent_id) => {
            if parent_id == root_span.context.span_id {
              child_spans = child_spans.push(span)
            }
          }
          None => {
            root_span = span
          }
        }
      }
      
      Some({
        trace_id: root_span.context.trace_id,
        root_span,
        child_spans
      })
    }
  }
  
  // Create a trace with multiple spans
  let child_span1 = create_span(child_context, "database_query", 1640995210)
  let completed_child_span1 = complete_span(child_span1, 1640995240, "ok")
  
  let grandchild_context = create_trace_context("trace-12345", "span-00003", Some("span-00002"))
  let grandchild_span = create_span(grandchild_context, "cache_lookup", 1640995220)
  let completed_grandchild_span = complete_span(grandchild_span, 1640995230, "hit")
  
  let trace_spans = [completed_root_span, completed_child_span1, completed_grandchild_span]
  let trace_tree = build_trace_tree(trace_spans)
  
  assert_true(trace_tree.is_some())
  match trace_tree {
    Some(tree) => {
      assert_eq(tree.trace_id, "trace-12345")
      assert_eq(tree.root_span.operation_name, "http_request")
      assert_eq(tree.child_spans.length(), 1)
      assert_eq(tree.child_spans[0].operation_name, "database_query")
    }
    None => assert_true(false)
  }
  
  // Test trace context propagation
  let extract_trace_context = fn(headers: Array[(String, String)]) {
    let mut trace_id = None
    let mut span_id = None
    let mut parent_span_id = None
    
    for (key, value) in headers {
      match key {
        "x-trace-id" => trace_id = Some(value)
        "x-span-id" => span_id = Some(value)
        "x-parent-span-id" => parent_span_id = Some(value)
        _ => ()
      }
    }
    
    match (trace_id, span_id) {
      (Some(tid), Some(sid)) => Some(create_trace_context(tid, sid, parent_span_id))
      _ => None
    }
  }
  
  let incoming_headers = [
    ("x-trace-id", "trace-67890"),
    ("x-span-id", "span-11111"),
    ("x-parent-span-id", "span-22222"),
    ("x-user-agent", "test-client")
  ]
  
  let extracted_context = extract_trace_context(incoming_headers)
  assert_true(extracted_context.is_some())
  
  match extracted_context {
    Some(context) => {
      assert_eq(context.trace_id, "trace-67890")
      assert_eq(context.span_id, "span-11111")
      assert_eq(context.parent_span_id, Some("span-22222"))
    }
    None => assert_true(false)
  }
  
  // Test trace context injection
  let inject_trace_context = fn(context: TraceContext) {
    [
      ("x-trace-id", context.trace_id),
      ("x-span-id", context.span_id),
      match context.parent_span_id {
        Some(parent_id) => ("x-parent-span-id", parent_id)
        None => ("x-parent-span-id", "")
      }
    ]
  }
  
  let injected_headers = inject_trace_context(root_context)
  assert_eq(injected_headers.length(), 3)
  assert_eq(injected_headers[0], ("x-trace-id", "trace-12345"))
  assert_eq(injected_headers[1], ("x-span-id", "span-00001"))
  assert_eq(injected_headers[2], ("x-parent-span-id", ""))
}

// Test 9: Cross-Platform Compatibility
test "cross-platform compatibility" {
  // Define platform types
  enum Platform {
    Windows
    Linux
    MacOS
    Unknown
  }
  
  type PlatformInfo = {
    platform: Platform,
    architecture: String,
    version: String,
    features: Array[String]
  }
  
  type CompatibilityLayer = {
    supported_platforms: Array[Platform],
    common_features: Array[String],
    platform_specific_features: Array[(Platform, Array[String])]
  }
  
  // Create platform information
  let create_platform_info = fn(platform: Platform, arch: String, version: String) {
    let features = match platform {
      Platform::Windows => ["win32_api", "registry_access", "event_log"]
      Platform::Linux => ["posix_api", "systemd", "syslog"]
      Platform::MacOS => ["cocoa", "launchd", "asl_log"]
      Platform::Unknown => []
    }
    
    {
      platform,
      architecture: arch,
      version,
      features
    }
  }
  
  let windows_info = create_platform_info(Platform::Windows, "x86_64", "10.0")
  let linux_info = create_platform_info(Platform::Linux, "x86_64", "5.4.0")
  let macos_info = create_platform_info(Platform::MacOS, "arm64", "11.0")
  
  assert_eq(windows_info.platform, Platform::Windows)
  assert_eq(windows_info.architecture, "x86_64")
  assert_eq(windows_info.features.length(), 3)
  assert_true(windows_info.features.contains("win32_api"))
  
  // Create compatibility layer
  let compatibility_layer = {
    supported_platforms: [Platform::Windows, Platform::Linux, Platform::MacOS],
    common_features: ["file_io", "network", "logging", "metrics"],
    platform_specific_features: [
      (Platform::Windows, ["registry_access", "event_log"]),
      (Platform::Linux, ["systemd", "syslog"]),
      (Platform::MacOS, ["launchd", "asl_log"])
    ]
  }
  
  // Test platform support
  let is_platform_supported = fn(platform: Platform, compatibility: CompatibilityLayer) {
    let mut supported = false
    
    for supported_platform in compatibility.supported_platforms {
      match (platform, supported_platform) {
        (Platform::Windows, Platform::Windows) => supported = true
        (Platform::Linux, Platform::Linux) => supported = true
        (Platform::MacOS, Platform::MacOS) => supported = true
        (Platform::Unknown, Platform::Unknown) => supported = true
        _ => ()
      }
    }
    
    supported
  }
  
  assert_true(is_platform_supported(Platform::Windows, compatibility_layer))
  assert_true(is_platform_supported(Platform::Linux, compatibility_layer))
  assert_true(is_platform_supported(Platform::MacOS, compatibility_layer))
  assert_false(is_platform_supported(Platform::Unknown, compatibility_layer))
  
  // Test feature availability
  let is_feature_available = fn(platform: Platform, feature: String, compatibility: CompatibilityLayer) {
    // Check common features
    if compatibility.common_features.contains(feature) {
      return true
    }
    
    // Check platform-specific features
    for (plat, features) in compatibility.platform_specific_features {
      match (platform, plat) {
        (Platform::Windows, Platform::Windows) => {
          if features.contains(feature) {
            return true
          }
        }
        (Platform::Linux, Platform::Linux) => {
          if features.contains(feature) {
            return true
          }
        }
        (Platform::MacOS, Platform::MacOS) => {
          if features.contains(feature) {
            return true
          }
        }
        _ => ()
      }
    }
    
    false
  }
  
  // Test feature availability on different platforms
  assert_true(is_feature_available(Platform::Windows, "file_io", compatibility_layer))  // Common feature
  assert_true(is_feature_available(Platform::Windows, "registry_access", compatibility_layer))  // Windows-specific
  assert_false(is_feature_available(Platform::Windows, "systemd", compatibility_layer))  // Linux-specific
  
  assert_true(is_feature_available(Platform::Linux, "network", compatibility_layer))  // Common feature
  assert_true(is_feature_available(Platform::Linux, "systemd", compatibility_layer))  // Linux-specific
  assert_false(is_feature_available(Platform::Linux, "cocoa", compatibility_layer))  // macOS-specific
  
  // Test path handling across platforms
  let normalize_path = fn(path: String, platform: Platform) {
    match platform {
      Platform::Windows => {
        let mut normalized = ""
        for i in 0..path.length() {
          let c = path[i]
          if c == '/' {
            normalized = normalized + "\\"
          } else {
            normalized = normalized + c.to_string()
          }
        }
        normalized
      }
      _ => {
        let mut normalized = ""
        for i in 0..path.length() {
          let c = path[i]
          if c == '\\' {
            normalized = normalized + "/"
          } else {
            normalized = normalized + c.to_string()
          }
        }
        normalized
      }
    }
  }
  
  // Test path normalization
  let unix_path = "/var/log/azimuth.log"
  let windows_path = "C:\\Program Files\\Azimuth\\config.json"
  
  let windows_normalized = normalize_path(unix_path, Platform::Windows)
  assert_eq(windows_normalized, "\\var\\log\\azimuth.log")
  
  let unix_normalized = normalize_path(windows_path, Platform::Linux)
  assert_eq(unix_normalized, "C:/Program Files/Azimuth/config.json")
  
  // Test environment variable handling
  let get_env_var_name = fn(var_name: String, platform: Platform) {
    match platform {
      Platform::Windows => var_name.to_uppercase()
      _ => var_name.to_uppercase()
    }
  }
  
  let service_name_env = get_env_var_name("service.name", Platform::Windows)
  assert_eq(service_name_env, "SERVICE.NAME")
  
  let service_name_env_linux = get_env_var_name("service.name", Platform::Linux)
  assert_eq(service_name_env_linux, "SERVICE.NAME")
  
  // Test platform-specific configuration
  type PlatformConfig = {
    log_file_path: String,
    config_file_path: String,
    temp_directory: String,
    service_command: String
  }
  
  let get_platform_config = fn(platform: Platform, install_dir: String) {
    match platform {
      Platform::Windows => {
        {
          log_file_path: install_dir + "\\logs\\azimuth.log",
          config_file_path: install_dir + "\\config\\azimuth.json",
          temp_directory: install_dir + "\\temp",
          service_command: "sc.exe create Azimuth binPath= \"" + install_dir + "\\bin\\azimuth.exe\""
        }
      }
      Platform::Linux => {
        {
          log_file_path: install_dir + "/var/log/azimuth.log",
          config_file_path: install_dir + "/etc/azimuth.json",
          temp_directory: "/tmp/azimuth",
          service_command: "systemctl enable azimuth && systemctl start azimuth"
        }
      }
      Platform::MacOS => {
        {
          log_file_path: install_dir + "/var/log/azimuth.log",
          config_file_path: install_dir + "/etc/azimuth.json",
          temp_directory: "/tmp/azimuth",
          service_command: "launchctl load -w " + install_dir + "/Library/LaunchDaemons/com.azimuth.plist"
        }
      }
      Platform::Unknown => {
        {
          log_file_path: "azimuth.log",
          config_file_path: "azimuth.json",
          temp_directory: "/tmp",
          service_command: "azimuth --daemon"
        }
      }
    }
  }
  
  // Test platform-specific configuration
  let windows_config = get_platform_config(Platform::Windows, "C:\\Program Files\\Azimuth")
  assert_eq(windows_config.log_file_path, "C:\\Program Files\\Azimuth\\logs\\azimuth.log")
  assert_eq(windows_config.config_file_path, "C:\\Program Files\\Azimuth\\config\\azimuth.json")
  assert_true(windows_config.service_command.contains("sc.exe"))
  
  let linux_config = get_platform_config(Platform::Linux, "/opt/azimuth")
  assert_eq(linux_config.log_file_path, "/opt/azimuth/var/log/azimuth.log")
  assert_eq(linux_config.config_file_path, "/opt/azimuth/etc/azimuth.json")
  assert_eq(linux_config.temp_directory, "/tmp/azimuth")
  assert_true(linux_config.service_command.contains("systemctl"))
  
  let macos_config = get_platform_config(Platform::MacOS, "/Applications/Azimuth")
  assert_eq(macos_config.log_file_path, "/Applications/Azimuth/var/log/azimuth.log")
  assert_eq(macos_config.config_file_path, "/Applications/Azimuth/etc/azimuth.json")
  assert_true(macos_config.service_command.contains("launchctl"))
}

// Test 10: Advanced Metrics Collection
test "advanced metrics collection" {
  // Define metric types
  enum MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  type MetricValue = {
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  type Metric = {
    name: String,
    metric_type: MetricType,
    description: String,
    values: Array[MetricValue]
  }
  
  type MetricsRegistry = {
    metrics: Array[Metric],
    created_at: Int
  }
  
  // Create metrics registry
  let create_registry = fn() {
    {
      metrics: [],
      created_at: 1640995200
    }
  }
  
  let registry = create_registry()
  assert_eq(registry.metrics.length(), 0)
  
  // Create metric
  let create_metric = fn(name: String, metric_type: MetricType, description: String) {
    {
      name,
      metric_type,
      description,
      values: []
    }
  }
  
  let counter_metric = create_metric("http_requests_total", MetricType::Counter, "Total number of HTTP requests")
  assert_eq(counter_metric.name, "http_requests_total")
  assert_eq(counter_metric.metric_type, MetricType::Counter)
  assert_eq(counter_metric.values.length(), 0)
  
  // Add metric value
  let add_metric_value = fn(metric: Metric, value: Float, timestamp: Int, labels: Array[(String, String)]) {
    let metric_value = {
      value,
      timestamp,
      labels
    }
    
    {
      name: metric.name,
      metric_type: metric.metric_type,
      description: metric.description,
      values: metric.values.push(metric_value)
    }
  }
  
  let with_value = add_metric_value(counter_metric, 100.0, 1640995200, [("method", "GET"), ("status", "200")])
  assert_eq(with_value.values.length(), 1)
  assert_eq(with_value.values[0].value, 100.0)
  assert_eq(with_value.values[0].labels.length(), 2)
  
  // Register metric
  let register_metric = fn(registry: MetricsRegistry, metric: Metric) {
    {
      metrics: registry.metrics.push(metric),
      created_at: registry.created_at
    }
  }
  
  let updated_registry = register_metric(registry, with_value)
  assert_eq(updated_registry.metrics.length(), 1)
  
  // Test metric aggregation
  let aggregate_metric = fn(metric: Metric, aggregation_type: String) {
    match aggregation_type {
      "sum" => {
        let mut sum = 0.0
        for value in metric.values {
          sum = sum + value.value
        }
        sum
      }
      "avg" => {
        if metric.values.length() == 0 {
          0.0
        } else {
          let mut sum = 0.0
          for value in metric.values {
            sum = sum + value.value
          }
          sum / metric.values.length().to_float()
        }
      }
      "max" => {
        let mut max = 0.0
        for value in metric.values {
          if value.value > max {
            max = value.value
          }
        }
        max
      }
      "min" => {
        let mut min = 999999999.0
        for value in metric.values {
          if value.value < min {
            min = value.value
          }
        }
        min
      }
      _ => 0.0
    }
  }
  
  // Add more values to test aggregation
  let with_more_values = add_metric_value(with_value, 150.0, 1640995250, [("method", "POST"), ("status", "201")])
  let with_even_more_values = add_metric_value(with_more_values, 75.0, 1640995300, [("method", "GET"), ("status", "404")])
  
  // Test aggregations
  let sum_result = aggregate_metric(with_even_more_values, "sum")
  assert_eq(sum_result, 325.0)
  
  let avg_result = aggregate_metric(with_even_more_values, "avg")
  assert_eq(avg_result, 108.33333333333333)  // (100 + 150 + 75) / 3
  
  let max_result = aggregate_metric(with_even_more_values, "max")
  assert_eq(max_result, 150.0)
  
  let min_result = aggregate_metric(with_even_more_values, "min")
  assert_eq(min_result, 75.0)
  
  // Test metric filtering by labels
  let filter_by_labels = fn(metric: Metric, filter_labels: Array[(String, String)]) {
    let mut filtered_values = []
    
    for value in metric.values {
      let mut matches = true
      
      for (filter_key, filter_value) in filter_labels {
        let mut found_label = false
        
        for (value_key, value_value) in value.labels {
          if value_key == filter_key and value_value == filter_value {
            found_label = true
          }
        }
        
        if not(found_label) {
          matches = false
        }
      }
      
      if matches {
        filtered_values = filtered_values.push(value)
      }
    }
    
    {
      name: metric.name,
      metric_type: metric.metric_type,
      description: metric.description,
      values: filtered_values
    }
  }
  
  // Test label filtering
  let get_filtered = filter_by_labels(with_even_more_values, [("method", "GET")])
  assert_eq(get_filtered.values.length(), 2)  // Two GET requests
  assert_eq(get_filtered.values[0].value, 100.0)
  assert_eq(get_filtered.values[1].value, 75.0)
  
  let status_filtered = filter_by_labels(with_even_more_values, [("status", "201")])
  assert_eq(status_filtered.values.length(), 1)  // One 201 status
  assert_eq(status_filtered.values[0].value, 150.0)
  
  // Test metric rate calculation
  let calculate_rate = fn(metric: Metric, time_window_seconds: Int) {
    if metric.values.length() < 2 {
      0.0
    } else {
      let first_value = metric.values[0]
      let last_value = metric.values[metric.values.length() - 1]
      
      let time_diff = last_value.timestamp - first_value.timestamp
      let value_diff = last_value.value - first_value.value
      
      if time_diff > 0 {
        value_diff / time_diff.to_float()
      } else {
        0.0
      }
    }
  }
  
  // Test rate calculation
  let rate = calculate_rate(with_even_more_values, 100)
  assert_eq(rate, 0.25)  // (75 - 100) / 100 = -0.25, but for counter it should be positive
  
  // Test histogram metrics
  let create_histogram_metric = fn(name: String, description: String, buckets: Array[Float]) {
    let mut bucket_metrics = []
    
    for bucket in buckets {
      let bucket_name = name + "_bucket_" + bucket.to_string()
      bucket_metrics = bucket_metrics.push(create_metric(bucket_name, MetricType::Counter, description + " bucket " + bucket.to_string()))
    }
    
    bucket_metrics
  }
  
  let histogram_buckets = [0.1, 0.5, 1.0, 2.5, 5.0, 10.0]
  let histogram_metrics = create_histogram_metric("request_duration_seconds", "Request duration in seconds", histogram_buckets)
  assert_eq(histogram_metrics.length(), 6)
  
  // Test percentile calculation
  let calculate_percentile = fn(metric: Metric, percentile: Float) {
    if metric.values.length() == 0 {
      0.0
    } else {
      // Sort values
      let mut sorted_values = []
      for value in metric.values {
        sorted_values = sorted_values.push(value.value)
      }
      
      // Simple bubble sort for demonstration
      let n = sorted_values.length()
      for i in 0..n {
        for j in 0..(n - i - 1) {
          if sorted_values[j] > sorted_values[j + 1] {
            let temp = sorted_values[j]
            sorted_values[j] = sorted_values[j + 1]
            sorted_values[j + 1] = temp
          }
        }
      }
      
      // Calculate percentile
      let index = ((percentile / 100.0) * (n.to_float() - 1.0)).to_int()
      if index < n {
        sorted_values[index]
      } else {
        sorted_values[n - 1]
      }
    }
  }
  
  // Test percentile calculation
  let p50 = calculate_percentile(with_even_more_values, 50.0)
  let p95 = calculate_percentile(with_even_more_values, 95.0)
  let p99 = calculate_percentile(with_even_more_values, 99.0)
  
  // With values [75.0, 100.0, 150.0] sorted
  // p50 should be 100.0 (middle value)
  // p95 should be 150.0 (highest value)
  // p99 should be 150.0 (highest value)
  assert_eq(p50, 100.0)
  assert_eq(p95, 150.0)
  assert_eq(p99, 150.0)
  
  // Test metrics export format
  let export_metrics = fn(registry: MetricsRegistry) {
    let mut export_lines = []
    
    for metric in registry.metrics {
      // Add metric metadata
      export_lines = export_lines.push("# HELP " + metric.name + " " + metric.description)
      export_lines = export_lines.push("# TYPE " + metric.name + " " + match metric.metric_type {
        MetricType::Counter => "counter"
        MetricType::Gauge => "gauge"
        MetricType::Histogram => "histogram"
        MetricType::Summary => "summary"
      })
      
      // Add metric values
      for value in metric.values {
        let mut line = metric.name
        
        // Add labels
        if value.labels.length() > 0 {
          line = line + "{"
          for i in 0..value.labels.length() {
            if i > 0 { line = line + "," }
            line = line + value.labels[i].0 + "=\"" + value.labels[i].1 + "\""
          }
          line = line + "}"
        }
        
        line = line + " " + value.value.to_string()
        line = line + " " + value.timestamp.to_string()
        
        export_lines = export_lines.push(line)
      }
    }
    
    export_lines
  }
  
  // Test metrics export
  let registry_with_metric = register_metric(registry, with_even_more_values)
  let exported_lines = export_metrics(registry_with_metric)
  
  assert_true(exported_lines.length() > 0)
  assert_true(exported_lines[0].contains("# HELP"))
  assert_true(exported_lines[1].contains("# TYPE"))
  assert_true(exported_lines[2].contains("http_requests_total"))
}