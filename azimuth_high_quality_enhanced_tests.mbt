// Azimuth High Quality Enhanced Test Suite
// This file contains high-quality test cases focusing on advanced features

// Test 1: Memory Management and Resource Cleanup
test "memory management and resource cleanup operations" {
  // Test resource acquisition and release pattern
  let resource_manager = ResourceManager::new()
  
  // Acquire multiple resources
  let resource1 = ResourceManager::acquire(resource_manager, "database_connection")
  let resource2 = ResourceManager::acquire(resource_manager, "file_handle")
  let resource3 = ResourceManager::acquire(resource_manager, "network_socket")
  
  // Verify resources are acquired
  assert_true(ResourceManager::is_active(resource_manager, resource1))
  assert_true(ResourceManager::is_active(resource_manager, resource2))
  assert_true(ResourceManager::is_active(resource_manager, resource3))
  
  // Test resource cleanup
  ResourceManager::release(resource_manager, resource1)
  assert_false(ResourceManager::is_active(resource_manager, resource1))
  assert_true(ResourceManager::is_active(resource_manager, resource2))
  assert_true(ResourceManager::is_active(resource_manager, resource3))
  
  // Test automatic cleanup on scope exit
  let scope_resources = ResourceManager::create_scope(resource_manager)
  let scoped_resource1 = ResourceManager::acquire_in_scope(scope_resources, "temp_file")
  let scoped_resource2 = ResourceManager::acquire_in_scope(scope_resources, "temp_buffer")
  
  // Resources should be active within scope
  assert_true(ResourceManager::is_active(resource_manager, scoped_resource1))
  assert_true(ResourceManager::is_active(resource_manager, scoped_resource2))
  
  // Exit scope (simulated)
  ResourceManager::exit_scope(scope_resources)
  
  // Resources should be automatically cleaned up
  assert_false(ResourceManager::is_active(resource_manager, scoped_resource1))
  assert_false(ResourceManager::is_active(resource_manager, scoped_resource2))
  
  // Test memory leak detection
  let leak_detector = LeakDetector::new()
  LeakDetector::start_tracking(leak_detector)
  
  // Allocate and deallocate resources
  let tracked_resource = LeakDetector::allocate(leak_detector, 1024)
  assert_eq(LeakDetector::get_tracked_memory(leak_detector), 1024)
  
  LeakDetector::deallocate(leak_detector, tracked_resource)
  assert_eq(LeakDetector::get_tracked_memory(leak_detector), 0)
  
  // Test leak detection
  let leaked_resource = LeakDetector::allocate(leak_detector, 512)
  let leaks = LeakDetector::detect_leaks(leak_detector)
  assert_eq(leaks.length(), 1)
  assert_eq(leaks[0].size, 512)
  
  LeakDetector::cleanup_all(leak_detector)
}

// Test 2: Network Communication and API Integration
test "network communication and api integration operations" {
  // Test HTTP client operations
  let http_client = HttpClient::new()
  
  // Test GET request
  let get_response = HttpClient::get(http_client, "https://api.example.com/data")
  assert_eq(get_response.status_code, 200)
  assert_true(get_response.body.contains("\"success\": true"))
  
  // Test POST request with JSON payload
  let json_payload = "{\"name\": \"test\", \"value\": 42}"
  let post_response = HttpClient::post_json(http_client, "https://api.example.com/create", json_payload)
  assert_eq(post_response.status_code, 201)
  assert_true(post_response.body.contains("\"id\":"))
  
  // Test request headers
  let headers = [
    ("Authorization", "Bearer token123"),
    ("Content-Type", "application/json"),
    ("X-Custom-Header", "custom_value")
  ]
  
  let header_response = HttpClient::get_with_headers(http_client, "https://api.example.com/protected", headers)
  assert_eq(header_response.status_code, 200)
  
  // Test error handling
  let error_response = HttpClient::get(http_client, "https://api.example.com/nonexistent")
  assert_eq(error_response.status_code, 404)
  assert_true(error_response.body.contains("\"error\":"))
  
  // Test timeout handling
  let timeout_client = HttpClient::with_timeout(5000)  // 5 second timeout
  let timeout_start = TimeUtil::current_time_millis()
  let timeout_response = HttpClient::get(timeout_client, "https://api.example.com/slow")
  let timeout_elapsed = TimeUtil::current_time_millis() - timeout_start
  
  assert_true(timeout_elapsed >= 4900 && timeout_elapsed <= 6000)  // Should timeout after ~5 seconds
  
  // Test retry mechanism
  let retry_client = HttpClient::with_retry(3)  // Retry 3 times
  let retry_response = HttpClient::get(retry_client, "https://api.example.com/flaky")
  assert_true(retry_response.status_code == 200 || retry_response.status_code == 500)
  
  // Test WebSocket connection
  let websocket = WebSocketClient::connect("wss://api.example.com/ws")
  assert_true(WebSocketClient::is_connected(websocket))
  
  // Test sending and receiving messages
  WebSocketClient::send(websocket, "{\"type\": \"ping\"}")
  let response = WebSocketClient::receive(websocket, 1000)  // 1 second timeout
  assert_true(response.contains("\"type\": \"pong\""))
  
  WebSocketClient::disconnect(websocket)
  assert_false(WebSocketClient::is_connected(websocket))
}

// Test 3: Data Serialization and Deserialization
test "data serialization and deserialization operations" {
  // Test JSON serialization
  let test_data = {
    name: "azimuth_test",
    version: "1.0.0",
    metrics: [10.5, 20.3, 30.7],
    metadata: {
      created_at: 1640995200,
      environment: "test",
      features: ["telemetry", "monitoring", "analytics"]
    }
  }
  
  let json_string = JsonSerializer::serialize(test_data)
  assert_true(json_string.contains("\"name\": \"azimuth_test\""))
  assert_true(json_string.contains("\"version\": \"1.0.0\""))
  assert_true(json_string.contains("\"metrics\": [10.5, 20.3, 30.7]"))
  
  // Test JSON deserialization
  let deserialized_data = JsonSerializer::deserialize(json_string)
  assert_eq(deserialized_data.name, "azimuth_test")
  assert_eq(deserialized_data.version, "1.0.0")
  assert_eq(deserialized_data.metrics.length(), 3)
  assert_eq(deserialized_data.metrics[0], 10.5)
  
  // Test binary serialization
  let binary_data = BinarySerializer::serialize(test_data)
  assert_true(binary_data.length() > 0)
  
  // Test binary deserialization
  let binary_deserialized = BinarySerializer::deserialize(binary_data)
  assert_eq(binary_deserialized.name, test_data.name)
  assert_eq(binary_deserialized.version, test_data.version)
  
  // Test protocol buffer serialization
  let proto_data = ProtobufSerializer::serialize(test_data)
  assert_true(proto_data.length() > 0)
  
  // Test protocol buffer deserialization
  let proto_deserialized = ProtobufSerializer::deserialize(proto_data)
  assert_eq(proto_deserialized.name, test_data.name)
  assert_eq(proto_deserialized.metadata.environment, "test")
  
  // Test CSV serialization
  let csv_data = [
    {name: "item1", value: 100, category: "A"},
    {name: "item2", value: 200, category: "B"},
    {name: "item3", value: 300, category: "A"}
  ]
  
  let csv_string = CsvSerializer::serialize(csv_data)
  assert_true(csv_string.contains("name,value,category"))
  assert_true(csv_string.contains("item1,100,A"))
  
  // Test CSV deserialization
  let csv_deserialized = CsvSerializer::deserialize(csv_string)
  assert_eq(csv_deserialized.length(), 3)
  assert_eq(csv_deserialized[0].name, "item1")
  assert_eq(csv_deserialized[2].value, 300)
  
  // Test XML serialization
  let xml_string = XmlSerializer::serialize(test_data)
  assert_true(xml_string.contains("<name>azimuth_test</name>"))
  assert_true(xml_string.contains("<version>1.0.0</version>"))
  
  // Test XML deserialization
  let xml_deserialized = XmlSerializer::deserialize(xml_string)
  assert_eq(xml_deserialized.name, "azimuth_test")
  assert_eq(xml_deserialized.version, "1.0.0")
  
  // Test YAML serialization
  let yaml_string = YamlSerializer::serialize(test_data)
  assert_true(yaml_string.contains("name: azimuth_test"))
  assert_true(yaml_string.contains("version: 1.0.0"))
  
  // Test YAML deserialization
  let yaml_deserialized = YamlSerializer::deserialize(yaml_string)
  assert_eq(yaml_deserialized.name, "azimuth_test")
  assert_eq(yaml_deserialized.version, "1.0.0")
}

// Test 4: Error Recovery and Fault Tolerance
test "error recovery and fault tolerance operations" {
  // Test circuit breaker pattern
  let circuit_breaker = CircuitBreaker::new(
    failure_threshold = 5,
    recovery_timeout = 10000,
    expected_exception = "NetworkException"
  )
  
  // Initially closed
  assert_eq(CircuitBreaker::get_state(circuit_breaker), "CLOSED")
  
  // Simulate failures
  for i in 0..3 {
    let result = CircuitBreaker::execute(circuit_breaker, fn() {
      throw NetworkException("Connection failed")
    })
    assert_true(result.is_error)
    assert_eq(CircuitBreaker::get_state(circuit_breaker), "CLOSED")
  }
  
  // More failures to trip the circuit
  for i in 0..3 {
    let result = CircuitBreaker::execute(circuit_breaker, fn() {
      throw NetworkException("Connection failed")
    })
    assert_true(result.is_error)
  }
  
  // Circuit should be open now
  assert_eq(CircuitBreaker::get_state(circuit_breaker), "OPEN")
  
  // Requests should fail immediately without execution
  let quick_fail_result = CircuitBreaker::execute(circuit_breaker, fn() {
    "This should not execute"
  })
  assert_true(quick_fail_result.is_error)
  assert_true(quick_fail_result.error.contains("Circuit breaker is open"))
  
  // Test retry mechanism with exponential backoff
  let retry_policy = RetryPolicy::exponential_backoff(
    max_attempts = 5,
    initial_delay = 100,
    max_delay = 5000,
    multiplier = 2.0
  )
  
  let attempt_count = { mut count: 0 }
  
  let retry_result = RetryPolicy::execute(retry_policy, fn() {
    attempt_count.count = attempt_count.count + 1
    if attempt_count.count < 3 {
      throw TemporaryException("Temporary failure")
    }
    "Success after retries"
  })
  
  assert_true(retry_result.is_success)
  assert_eq(retry_result.value, "Success after retries")
  assert_eq(attempt_count.count, 3)
  
  // Test timeout with fallback
  let timeout_with_fallback = TimeoutPolicy::with_fallback(
    timeout_ms = 1000,
    fallback_fn = fn() { "Fallback result" }
  )
  
  let slow_result = timeout_with_fallback.execute(fn() {
    TimeUtil::sleep(2000)  // Sleep for 2 seconds
    "Slow operation result"
  })
  
  assert_eq(slow_result, "Fallback result")
  
  // Test bulkhead pattern
  let bulkhead = Bulkhead::new(
    max_concurrent = 3,
    max_queue = 10
  )
  
  let results = []
  let threads = []
  
  // Submit tasks that exceed bulkhead capacity
  for i in 0..10 {
    let thread = ThreadUtil::spawn(() -> {
      Bulkhead::execute(bulkhead, fn() {
        TimeUtil::sleep(500)  // Simulate work
        return "Task " + i.to_string()
      })
    })
    ArrayUtil::push(threads, thread)
  }
  
  // Wait for all threads to complete
  for thread in threads {
    let result = ThreadUtil::join(thread)
    ArrayUtil::push(results, result)
  }
  
  // Some tasks should succeed, others should be rejected
  let success_count = ArrayUtil::count(results, fn(r) { r.is_success })
  let rejection_count = ArrayUtil::count(results, fn(r) { r.is_error })
  
  assert_true(success_count <= 3)  // At most 3 concurrent executions
  assert_true(rejection_count > 0)  // Some should be rejected
}

// Test 5: Performance Benchmarking
test "performance benchmarking operations" {
  // Test CPU benchmarking
  let cpu_benchmark = CPUBenchmark::new()
  
  let benchmark_start = TimeUtil::current_time_millis()
  let cpu_result = CPUBenchmark::run_compute_intensive(cpu_benchmark, 1000000)
  let benchmark_duration = TimeUtil::current_time_millis() - benchmark_start
  
  assert_true(cpu_result.score > 0)
  assert_true(benchmark_duration > 0)
  
  // Test memory benchmarking
  let memory_benchmark = MemoryBenchmark::new()
  
  let memory_result = MemoryBenchmark::run_allocation_test(memory_benchmark, 10000, 1024)
  assert_true(memory_result.allocation_rate > 0)
  assert_true(memory_result.deallocation_rate > 0)
  assert_eq(memory_result.allocated_bytes, 10000 * 1024)
  
  // Test I/O benchmarking
  let io_benchmark = IOBenchmark::new()
  
  let io_result = IOBenchmark::run_file_io_test(io_benchmark, "/tmp/test_file", 1024 * 1024)
  assert_true(io_result.write_speed > 0)
  assert_true(io_result.read_speed > 0)
  assert_eq(io_result.bytes_written, 1024 * 1024)
  assert_eq(io_result.bytes_read, 1024 * 1024)
  
  // Test network benchmarking
  let network_benchmark = NetworkBenchmark::new()
  
  let network_result = NetworkBenchmark::run_latency_test(network_benchmark, "https://www.google.com", 10)
  assert_true(network_result.average_latency > 0)
  assert_eq(network_result.request_count, 10)
  assert_true(network_result.success_rate > 0.5)  // At least 50% success rate
  
  // Test database benchmarking
  let db_benchmark = DatabaseBenchmark::new()
  
  let db_result = DatabaseBenchmark::run_crud_test(db_benchmark, "test_table", 1000)
  assert_true(db_result.insert_rate > 0)
  assert_true(db_result.select_rate > 0)
  assert_true(db_result.update_rate > 0)
  assert_true(db_result.delete_rate > 0)
  
  // Test algorithm benchmarking
  let algo_benchmark = AlgorithmBenchmark::new()
  
  let sort_data = ArrayUtil::shuffle(ArrayUtil::range(1, 10000))
  
  let quicksort_result = AlgorithmBenchmark::run_sort_test(algo_benchmark, "quicksort", sort_data)
  let mergesort_result = AlgorithmBenchmark::run_sort_test(algo_benchmark, "mergesort", sort_data)
  let heapsort_result = AlgorithmBenchmark::run_sort_test(algo_benchmark, "heapsort", sort_data)
  
  assert_true(quicksort_result.execution_time > 0)
  assert_true(mergesort_result.execution_time > 0)
  assert_true(heapsort_result.execution_time > 0)
  
  // Verify all sorts produce correct results
  assert_eq(quicksort_result.output.length(), sort_data.length())
  assert_true(ArrayUtil::is_sorted(quicksort_result.output))
  
  assert_eq(mergesort_result.output.length(), sort_data.length())
  assert_true(ArrayUtil::is_sorted(mergesort_result.output))
  
  assert_eq(heapsort_result.output.length(), sort_data.length())
  assert_true(ArrayUtil::is_sorted(heapsort_result.output))
  
  // Test performance regression detection
  let baseline_metrics = {
    cpu_score: 1000,
    memory_bandwidth: 5000,
    io_throughput: 100,
    network_latency: 50,
    database_ops: 1000
  }
  
  let current_metrics = {
    cpu_score: cpu_result.score,
    memory_bandwidth: memory_result.allocation_rate,
    io_throughput: io_result.write_speed,
    network_latency: network_result.average_latency,
    database_ops: db_result.insert_rate
  }
  
  let regression_report = PerformanceAnalyzer::detect_regressions(baseline_metrics, current_metrics)
  
  // Check for significant regressions (>20% degradation)
  assert_true(regression_report.cpu_regression < 0.2)
  assert_true(regression_report.memory_regression < 0.2)
  assert_true(regression_report.io_regression < 0.2)
  assert_true(regression_report.network_regression < 0.2)
  assert_true(regression_report.database_regression < 0.2)
}

// Test 6: Security and Cryptography
test "security and cryptography operations" {
  // Test hash functions
  let test_string = "azimuth_telemetry_test_data"
  
  let md5_hash = HashUtil::md5(test_string)
  assert_eq(md5_hash.length(), 32)
  
  let sha1_hash = HashUtil::sha1(test_string)
  assert_eq(sha1_hash.length(), 40)
  
  let sha256_hash = HashUtil::sha256(test_string)
  assert_eq(sha256_hash.length(), 64)
  
  let sha512_hash = HashUtil::sha512(test_string)
  assert_eq(sha512_hash.length(), 128)
  
  // Test hash verification
  assert_true(HashUtil::verify_md5(test_string, md5_hash))
  assert_true(HashUtil::verify_sha256(test_string, sha256_hash))
  assert_false(HashUtil::verify_sha256(test_string, "invalid_hash"))
  
  // Test symmetric encryption
  let encryption_key = EncryptionUtil::generate_key(256)  // 256-bit key
  let plaintext = "Sensitive telemetry data"
  
  let encrypted_data = EncryptionUtil::encrypt_aes256(plaintext, encryption_key)
  assert_true(encrypted_data.length() > 0)
  assert_false(encrypted_data.contains("Sensitive"))
  
  let decrypted_data = EncryptionUtil::decrypt_aes256(encrypted_data, encryption_key)
  assert_eq(decrypted_data, plaintext)
  
  // Test with wrong key
  let wrong_key = EncryptionUtil::generate_key(256)
  let decrypt_with_wrong_key = try {
    EncryptionUtil::decrypt_aes256(encrypted_data, wrong_key)
  } catch {
    _ => "decryption_failed"
  }
  assert_eq(decrypt_with_wrong_key, "decryption_failed")
  
  // Test asymmetric encryption
  let key_pair = EncryptionUtil::generate_rsa_key_pair(2048)
  let public_key = key_pair.public_key
  let private_key = key_pair.private_key
  
  let rsa_encrypted = EncryptionUtil::encrypt_rsa(plaintext, public_key)
  let rsa_decrypted = EncryptionUtil::decrypt_rsa(rsa_encrypted, private_key)
  assert_eq(rsa_decrypted, plaintext)
  
  // Test digital signatures
  let message = "Important telemetry report"
  let signature = SignatureUtil::sign(message, private_key)
  
  assert_true(SignatureUtil::verify(message, signature, public_key))
  assert_false(SignatureUtil::verify("Tampered message", signature, public_key))
  
  // Test certificate operations
  let certificate = CertificateUtil::generate_self_signed(
    common_name = "azimuth.example.com",
    organization = "Azimuth Telemetry",
    valid_days = 365
  )
  
  assert_true(CertificateUtil::is_valid(certificate))
  assert_eq(CertificateUtil::get_subject(certificate), "CN=azimuth.example.com, O=Azimuth Telemetry")
  
  let expired_certificate = CertificateUtil::generate_expired(
    common_name = "expired.example.com",
    organization = "Azimuth Telemetry"
  )
  
  assert_false(CertificateUtil::is_valid(expired_certificate))
  
  // Test secure random generation
  let random_bytes = SecureRandom::generate_bytes(32)
  assert_eq(random_bytes.length(), 32)
  
  let random_string = SecureRandom::generate_string(16, "alphanumeric")
  assert_eq(random_string.length(), 16)
  assert_true(random_string.matches("^[a-zA-Z0-9]+$"))
  
  // Test password hashing
  let password = "secure_password_123"
  let password_hash = PasswordUtil::hash(password)
  
  assert_true(PasswordUtil::verify(password, password_hash))
  assert_false(PasswordUtil::verify("wrong_password", password_hash))
  
  // Test token generation and validation
  let token_payload = {
    user_id: "user123",
    permissions: ["read", "write"],
    expires_at: TimeUtil::current_time() + 3600
  }
  
  let jwt_token = TokenUtil::generate_jwt(token_payload, "secret_key")
  assert_true(jwt_token.length() > 0)
  
  let decoded_payload = TokenUtil::verify_jwt(jwt_token, "secret_key")
  assert_eq(decoded_payload.user_id, "user123")
  assert_eq(decoded_payload.permissions.length(), 2)
  
  // Test with wrong secret
  let verify_with_wrong_secret = try {
    TokenUtil::verify_jwt(jwt_token, "wrong_secret")
  } catch {
    _ => { user_id: "invalid" }
  }
  assert_eq(verify_with_wrong_secret.user_id, "invalid")
}

// Test 7: Data Validation and Transformation
test "data validation and transformation operations" {
  // Test data validation rules
  let validation_rules = ValidationRules::new()
  
  // Add validation rules
  ValidationRules::add_rule(validation_rules, "name", ValidationRule::required())
  ValidationRules::add_rule(validation_rules, "name", ValidationRule::min_length(3))
  ValidationRules::add_rule(validation_rules, "name", ValidationRule::max_length(50))
  
  ValidationRules::add_rule(validation_rules, "age", ValidationRule::required())
  ValidationRules::add_rule(validation_rules, "age", ValidationRule::min_value(0))
  ValidationRules::add_rule(validation_rules, "age", ValidationRule::max_value(150))
  
  ValidationRules::add_rule(validation_rules, "email", ValidationRule::email())
  ValidationRules::add_rule(validation_rules, "email", ValidationRule::required())
  
  // Test valid data
  let valid_data = {
    name: "John Doe",
    age: 30,
    email: "john.doe@example.com"
  }
  
  let valid_result = ValidationRules::validate(validation_rules, valid_data)
  assert_true(valid_result.is_valid)
  assert_eq(valid_result.errors.length(), 0)
  
  // Test invalid data
  let invalid_data = {
    name: "Jo",  // Too short
    age: 200,    // Too high
    email: "invalid-email"
  }
  
  let invalid_result = ValidationRules::validate(validation_rules, invalid_data)
  assert_false(invalid_result.is_valid)
  assert_eq(invalid_result.errors.length(), 3)
  assert_true(invalid_result.errors.some(fn(e) { e.contains("name") && e.contains("minimum length") }))
  assert_true(invalid_result.errors.some(fn(e) { e.contains("age") && e.contains("maximum value") }))
  assert_true(invalid_result.errors.some(fn(e) { e.contains("email") && e.contains("valid email") }))
  
  // Test data transformation
  let transformation_pipeline = TransformationPipeline::new()
  
  // Add transformations
  TransformationPipeline::add_step(transformation_pipeline, "trim", TransformStep::trim_strings())
  TransformationPipeline::add_step(transformation_pipeline, "normalize", TransformStep::normalize_case())
  TransformationPipeline::add_step(transformation_pipeline, "sanitize", TransformStep::sanitize_html())
  
  let raw_data = {
    name: "  John Doe  ",
    email: "  JOHN.DOE@EXAMPLE.COM  ",
    bio: "<script>alert('xss')</script>Hello World!"
  }
  
  let transformed_data = TransformationPipeline::process(transformation_pipeline, raw_data)
  assert_eq(transformed_data.name, "John Doe")
  assert_eq(transformed_data.email, "john.doe@example.com")
  assert_eq(transformed_data.bio, "Hello World!")
  
  // Test data normalization
  let address_normalizer = AddressNormalizer::new()
  
  let raw_address = {
    street: " 123 Main St. ",
    city: "New York",
    state: "NY",
    zip_code: "10001",
    country: "USA"
  }
  
  let normalized_address = AddressNormalizer::normalize(address_normalizer, raw_address)
  assert_eq(normalized_address.street, "123 Main Street")
  assert_eq(normalized_address.city, "New York")
  assert_eq(normalized_address.state, "NY")
  assert_eq(normalized_address.zip_code, "10001")
  assert_eq(normalized_address.country, "United States")
  
  // Test phone number normalization
  let phone_normalizer = PhoneNormalizer::new()
  
  let raw_phone_numbers = [
    "+1 (555) 123-4567",
    "555.123.4567",
    "5551234567",
    "(555) 123-4567 ext. 123"
  ]
  
  let normalized_phones = ArrayUtil::map(raw_phone_numbers, fn(p) {
    PhoneNormalizer::normalize(phone_normalizer, p, "US")
  })
  
  assert_eq(normalized_phones[0], "+15551234567")
  assert_eq(normalized_phones[1], "+15551234567")
  assert_eq(normalized_phones[2], "+15551234567")
  assert_eq(normalized_phones[3], "+15551234567")
  
  // Test data deduplication
  let deduplicator = DataDeduplicator::new()
  
  let duplicate_records = [
    {id: 1, name: "John Doe", email: "john@example.com"},
    {id: 2, name: "John Doe", email: "john@example.com"},
    {id: 3, name: "Jane Smith", email: "jane@example.com"},
    {id: 4, name: "J. Smith", email: "jane@example.com"}  // Similar but not exact
  ]
  
  let deduplicated = DataDeduplicator::deduplicate(deduplicator, duplicate_records, ["email"])
  assert_eq(deduplicated.length(), 2)  // Two unique emails
  
  // Test fuzzy matching
  let fuzzy_matcher = FuzzyMatcher::new()
  
  let similarity = FuzzyMatcher::similarity(fuzzy_matcher, "John Doe", "Jon Doe")
  assert_true(similarity > 0.8)  // High similarity
  
  let match_result = FuzzyMatcher::find_best_match(fuzzy_matcher, "Jane Smith", ["John Doe", "Jane Smith", "Bob Johnson"])
  assert_eq(match_result.match, "Jane Smith")
  assert_true(match_result.score > 0.9)
}

// Test 8: Event Processing and Pub/Sub
test "event processing and pub/sub operations" {
  // Test event bus
  let event_bus = EventBus::new()
  
  // Test event subscription
  let received_events = []
  
  let subscription1 = EventBus::subscribe(event_bus, "user.created", fn(event) {
    ArrayUtil::push(received_events, ("user.created", event.data))
  })
  
  let subscription2 = EventBus::subscribe(event_bus, "user.*", fn(event) {
    ArrayUtil::push(received_events, ("user.*", event.data))
  })
  
  // Test event publishing
  let user_event = {
    user_id: "user123",
    name: "John Doe",
    email: "john@example.com",
    timestamp: TimeUtil::current_time()
  }
  
  EventBus::publish(event_bus, "user.created", user_event)
  
  // Verify events were received
  assert_eq(received_events.length(), 2)
  assert_eq(received_events[0], ("user.created", user_event))
  assert_eq(received_events[1], ("user.*", user_event))
  
  // Test event filtering
  let filtered_events = []
  
  let filter_subscription = EventBus::subscribe_with_filter(event_bus, "metrics.*", fn(event) {
    ArrayUtil::push(filtered_events, event)
  }, fn(event) {
    event.data.value > 100  // Only accept metrics with value > 100
  })
  
  EventBus::publish(event_bus, "metrics.cpu", {value: 80, timestamp: TimeUtil::current_time()})
  EventBus::publish(event_bus, "metrics.memory", {value: 150, timestamp: TimeUtil::current_time()})
  
  // Only the second event should pass the filter
  assert_eq(filtered_events.length(), 1)
  assert_eq(filtered_events[0].data.value, 150)
  
  // Test event transformation
  let transformed_events = []
  
  let transform_subscription = EventBus::subscribe_with_transform(event_bus, "raw.*", fn(event) {
    ArrayUtil::push(transformed_events, event)
  }, fn(event) {
    {
      type: event.type,
      data: {
        original: event.data,
        processed: true,
        processed_at: TimeUtil::current_time()
      },
      metadata: event.metadata
    }
  })
  
  EventBus::publish(event_bus, "raw.telemetry", {sensor_id: "temp-001", reading: 25.5})
  
  assert_eq(transformed_events.length(), 1)
  assert_eq(transformed_events[0].data.original.sensor_id, "temp-001")
  assert_true(transformed_events[0].data.processed)
  assert_true(transformed_events[0].data.processed_at > 0)
  
  // Test event aggregation
  let event_aggregator = EventAggregator::new()
  
  EventAggregator::add_aggregation_rule(event_aggregator, "metrics.cpu", AggregationRule::average(10))
  EventAggregator::add_aggregation_rule(event_aggregator, "metrics.memory", AggregationRule::sum(5))
  
  // Publish multiple events
  for i in 0..15 {
    EventBus::publish(event_bus, "metrics.cpu", {value: 50 + i, timestamp: TimeUtil::current_time()})
  }
  
  for i in 0..8 {
    EventBus::publish(event_bus, "metrics.memory", {value: 100 + i * 10, timestamp: TimeUtil::current_time()})
  }
  
  // Get aggregated results
  let cpu_average = EventAggregator::get_aggregated_value(event_aggregator, "metrics.cpu")
  let memory_sum = EventAggregator::get_aggregated_value(event_aggregator, "metrics.memory")
  
  assert_true(cpu_average > 50 && cpu_average < 65)  // Average of values 50-64
  assert_eq(memory_sum, 100 + 110 + 120 + 130 + 140)  // Sum of first 5 values
  
  // Test event persistence
  let event_store = EventStore::new()
  
  let stored_event_id = EventStore::store(event_store, {
    type: "order.created",
    data: {order_id: "order-123", amount: 99.99},
    metadata: {source: "web", version: "1.0"}
  })
  
  assert_true(stored_event_id.length() > 0)
  
  let retrieved_event = EventStore::get(event_store, stored_event_id)
  assert_eq(retrieved_event.type, "order.created")
  assert_eq(retrieved_event.data.order_id, "order-123")
  assert_eq(retrieved_event.data.amount, 99.99)
  
  // Test event replay
  let replay_events = []
  
  EventStore::replay(event_store, "order.*", fn(event) {
    ArrayUtil::push(replay_events, event)
  })
  
  assert_eq(replay_events.length(), 1)
  assert_eq(replay_events[0].type, "order.created")
  
  // Clean up subscriptions
  EventBus::unsubscribe(event_bus, subscription1)
  EventBus::unsubscribe(event_bus, subscription2)
  EventBus::unsubscribe(event_bus, filter_subscription)
  EventBus::unsubscribe(event_bus, transform_subscription)
}

// Test 9: Caching Mechanisms
test "caching mechanisms operations" {
  // Test in-memory cache
  let memory_cache = MemoryCache::new(max_size = 100)
  
  // Test cache put and get
  MemoryCache::put(memory_cache, "key1", "value1")
  MemoryCache::put(memory_cache, "key2", "value2")
  MemoryCache::put(memory_cache, "key3", "value3")
  
  assert_eq(MemoryCache::get(memory_cache, "key1"), Some("value1"))
  assert_eq(MemoryCache::get(memory_cache, "key2"), Some("value2"))
  assert_eq(MemoryCache::get(memory_cache, "key3"), Some("value3"))
  assert_eq(MemoryCache::get(memory_cache, "nonexistent"), None)
  
  // Test cache size limits
  for i in 0..150 {
    MemoryCache::put(memory_cache, "key" + i.to_string(), "value" + i.to_string())
  }
  
  assert_eq(MemoryCache::size(memory_cache), 100)  // Should be limited to max_size
  
  // Test LRU eviction
  assert_eq(MemoryCache::get(memory_cache, "key1"), None)  // Should be evicted
  assert_eq(MemoryCache::get(memory_cache, "key150"), Some("value150"))  // Should still be there
  
  // Test TTL-based cache
  let ttl_cache = TTLCache::new(default_ttl = 1000)  // 1 second TTL
  
  TTLCache::put(ttl_cache, "temp_key", "temp_value", 500)  // 500ms TTL
  assert_eq(TTLCache::get(ttl_cache, "temp_key"), Some("temp_value"))
  
  TimeUtil::sleep(600)  // Wait for expiration
  assert_eq(TTLCache::get(ttl_cache, "temp_key"), None)  // Should be expired
  
  // Test distributed cache
  let distributed_cache = DistributedCache::new("redis://localhost:6379")
  
  DistributedCache::put(distributed_cache, "distributed_key1", "distributed_value1")
  DistributedCache::put(distributed_cache, "distributed_key2", "distributed_value2")
  
  assert_eq(DistributedCache::get(distributed_cache, "distributed_key1"), Some("distributed_value1"))
  assert_eq(DistributedCache::get(distributed_cache, "distributed_key2"), Some("distributed_value2"))
  
  // Test cache warming
  let cache_warmer = CacheWarmer::new(memory_cache)
  
  CacheWarmer::add_warming_rule(cache_warmer, "user.*", fn(key_pattern) {
    // Simulate loading user data
    [
      ("user.1", {name: "User 1", email: "user1@example.com"}),
      ("user.2", {name: "User 2", email: "user2@example.com"}),
      ("user.3", {name: "User 3", email: "user3@example.com"})
    ]
  })
  
  CacheWarmer::warm(cache_warmer, "user.*")
  
  assert_eq(MemoryCache::get(memory_cache, "user.1"), Some({name: "User 1", email: "user1@example.com"}))
  assert_eq(MemoryCache::get(memory_cache, "user.2"), Some({name: "User 2", email: "user2@example.com"}))
  assert_eq(MemoryCache::get(memory_cache, "user.3"), Some({name: "User 3", email: "user3@example.com"}))
  
  // Test cache invalidation
  let invalidation_cache = MemoryCache::new(100)
  
  MemoryCache::put(invalidation_cache, "user.1", {name: "User 1", email: "user1@example.com"})
  MemoryCache::put(invalidation_cache, "user.1.profile", {age: 30, city: "New York"})
  MemoryCache::put(invalidation_cache, "user.1.settings", {theme: "dark", notifications: true})
  
  // Invalidate all user.1 related keys
  MemoryCache::invalidate_pattern(invalidation_cache, "user.1*")
  
  assert_eq(MemoryCache::get(invalidation_cache, "user.1"), None)
  assert_eq(MemoryCache::get(invalidation_cache, "user.1.profile"), None)
  assert_eq(MemoryCache::get(invalidation_cache, "user.1.settings"), None)
  
  // Test cache statistics
  let stats_cache = MemoryCache::new(100)
  
  // Perform some operations
  MemoryCache::put(stats_cache, "stats_key1", "stats_value1")
  MemoryCache::get(stats_cache, "stats_key1")  // Hit
  MemoryCache::get(stats_cache, "stats_key1")  // Hit
  MemoryCache::get(stats_cache, "nonexistent")  // Miss
  
  let stats = MemoryCache::get_statistics(stats_cache)
  assert_eq(stats.hits, 2)
  assert_eq(stats.misses, 1)
  assert_eq(stats.hit_rate, 2.0 / 3.0)  // 2 hits out of 3 total requests
  
  // Test multi-level cache
  let l1_cache = MemoryCache::new(50)  // L1: Small, fast cache
  let l2_cache = MemoryCache::new(200)  // L2: Larger, slower cache
  let multi_cache = MultiLevelCache::new([l1_cache, l2_cache])
  
  MultiLevelCache::put(multi_cache, "ml_key1", "ml_value1")
  
  // First access - should populate L1 and L2
  assert_eq(MultiLevelCache::get(multi_cache, "ml_key1"), Some("ml_value1"))
  
  // Clear L1 to simulate eviction
  MemoryCache::clear(l1_cache)
  
  // Second access - should get from L2 and repopulate L1
  assert_eq(MultiLevelCache::get(multi_cache, "ml_key1"), Some("ml_value1"))
  
  // Verify L1 is repopulated
  assert_eq(MemoryCache::get(l1_cache, "ml_key1"), Some("ml_value1"))
}

// Test 10: Logging and Monitoring
test "logging and monitoring operations" {
  // Test logger configuration
  let logger_config = LoggerConfig::new()
  LoggerConfig::set_level(logger_config, "INFO")
  LoggerConfig::add_appender(logger_config, ConsoleAppender::new())
  LoggerConfig::add_appender(logger_config, FileAppender::new("/tmp/azimuth_test.log"))
  
  let logger = Logger::new(logger_config)
  
  // Test different log levels
  Logger::trace(logger, "This is a trace message")
  Logger::debug(logger, "This is a debug message")
  Logger::info(logger, "This is an info message")
  Logger::warn(logger, "This is a warning message")
  Logger::error(logger, "This is an error message")
  Logger::fatal(logger, "This is a fatal message")
  
  // Test structured logging
  Logger::info_structured(logger, "User action", {
    user_id: "user123",
    action: "login",
    ip_address: "192.168.1.100",
    timestamp: TimeUtil::current_time()
  })
  
  // Test log filtering
  let filter_logger = Logger::with_filter(logger, fn(record) {
    record.level >= "WARN" || record.message.contains("IMPORTANT")
  })
  
  Logger::debug(filter_logger, "This debug message should be filtered out")
  Logger::warn(filter_logger, "This warning should pass through")
  Logger::info(filter_logger, "IMPORTANT: This info should pass through")
  
  // Test metrics collection
  let metrics_collector = MetricsCollector::new()
  
  // Register metrics
  let counter_metric = MetricsCollector::register_counter(metrics_collector, "requests.total")
  let gauge_metric = MetricsCollector::register_gauge(metrics_collector, "memory.usage")
  let histogram_metric = MetricsCollector::register_histogram(metrics_collector, "response.time")
  
  // Record metrics
  MetricsCollector::increment_counter(counter_metric)
  MetricsCollector::increment_counter(counter_metric, 5)  // Increment by 5
  MetricsCollector::set_gauge(gauge_metric, 1024)
  MetricsCollector::record_histogram(histogram_metric, 150)
  MetricsCollector::record_histogram(histogram_metric, 200)
  MetricsCollector::record_histogram(histogram_metric, 100)
  
  // Get metric values
  assert_eq(MetricsCollector::get_counter_value(counter_metric), 6)
  assert_eq(MetricsCollector::get_gauge_value(gauge_metric), 1024)
  
  let histogram_stats = MetricsCollector::get_histogram_stats(histogram_metric)
  assert_eq(histogram_stats.count, 3)
  assert_eq(histogram_stats.sum, 450)
  assert_eq(histogram_stats.min, 100)
  assert_eq(histogram_stats.max, 200)
  assert_eq(histogram_stats.average, 150)
  
  // Test health checks
  let health_checker = HealthChecker::new()
  
  // Add health checks
  HealthChecker::add_check(health_checker, "database", fn() {
    // Simulate database connectivity check
    HealthStatus::healthy("Database is responding normally")
  })
  
  HealthChecker::add_check(health_checker, "redis", fn() {
    // Simulate Redis connectivity check
    HealthStatus::healthy("Redis is responding normally")
  })
  
  HealthChecker::add_check(health_checker, "external_api", fn() {
    // Simulate external API check that fails
    HealthStatus::unhealthy("External API is not responding")
  })
  
  let health_status = HealthChecker::check_all(health_checker)
  assert_false(health_status.overall_healthy)  // One check is unhealthy
  assert_eq(health_status.checks.length(), 3)
  assert_true(health_status.checks["database"].healthy)
  assert_true(health_status.checks["redis"].healthy)
  assert_false(health_status.checks["external_api"].healthy)
  
  // Test alerting
  let alert_manager = AlertManager::new()
  
  // Add alert rules
  AlertManager::add_rule(alert_manager, AlertRule::threshold(
    name = "high_error_rate",
    metric = "error.rate",
    threshold = 0.05,  // 5%
    operator = "greater_than",
    severity = "warning"
  ))
  
  AlertManager::add_rule(alert_manager, AlertRule::threshold(
    name = "service_down",
    metric = "service.availability",
    threshold = 0.99,  // 99%
    operator = "less_than",
    severity = "critical"
  ))
  
  // Simulate metric values that trigger alerts
  AlertManager::evaluate_metric(alert_manager, "error.rate", 0.08)  // Above 5% threshold
  AlertManager::evaluate_metric(alert_manager, "service.availability", 0.95)  // Below 99% threshold
  
  let alerts = AlertManager::get_active_alerts(alert_manager)
  assert_eq(alerts.length(), 2)
  assert_true(alerts.some(fn(a) { a.name == "high_error_rate" && a.severity == "warning" }))
  assert_true(alerts.some(fn(a) { a.name == "service_down" && a.severity == "critical" }))
  
  // Test tracing
  let tracer = Tracer::new("azimuth_service")
  
  // Create a span
  let parent_span = Tracer::start_span(tracer, "process_request")
  Tracer::set_tag(parent_span, "http.method", "GET")
  Tracer::set_tag(parent_span, "http.url", "/api/data")
  
  // Create child spans
  let child_span1 = Tracer::start_child_span(parent_span, "database_query")
  TimeUtil::sleep(50)  // Simulate work
  Tracer::finish_span(child_span1)
  
  let child_span2 = Tracer::start_child_span(parent_span, "cache_lookup")
  TimeUtil::sleep(20)  // Simulate work
  Tracer::finish_span(child_span2)
  
  Tracer::finish_span(parent_span)
  
  // Test span context propagation
  let span_context = Tracer::extract_context(tracer, parent_span)
  let propagated_tracer = Tracer::new("downstream_service")
  let propagated_span = Tracer::start_span_from_context(propagated_tracer, "downstream_processing", span_context)
  
  Tracer::set_tag(propagated_span, "service.name", "downstream_service")
  Tracer::finish_span(propagated_span)
  
  // Verify trace relationship
  assert_eq(Tracer::get_trace_id(parent_span), Tracer::get_trace_id(propagated_span))
  assert_eq(Tracer::get_parent_span_id(propagated_span), Tracer::get_span_id(parent_span))
}