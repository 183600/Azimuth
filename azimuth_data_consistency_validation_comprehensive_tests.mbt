// Azimuth Telemetry System - Data Consistency Validation Comprehensive Tests
// This file contains comprehensive test cases for data consistency validation

// Test 1: Trace Context Consistency
test "trace context consistency across services" {
  let trace_consistency_validator = TraceConsistencyValidator::new()
  
  // Create a trace with multiple spans
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let root_span = Span::new("root_span", Server, SpanContext::new(trace_id, "root_span_id", true, ""))
  
  let child_span1 = Span::new("child_span1", Client, SpanContext::new(trace_id, "child_span1_id", true, ""))
  let child_span2 = Span::new("child_span2", Internal, SpanContext::new(trace_id, "child_span2_id", true, ""))
  
  // Set parent-child relationships
  Span::set_parent(child_span1, root_span)
  Span::set_parent(child_span2, root_span)
  
  // Add spans to validator
  trace_consistency_validator.add_span(root_span)
  trace_consistency_validator.add_span(child_span1)
  trace_consistency_validator.add_span(child_span2)
  
  // Validate trace consistency
  let validation_result = trace_consistency_validator.validate()
  
  // Verify validation passes
  assert_true(validation_result.is_consistent)
  assert_eq(validation_result.trace_id, trace_id)
  assert_eq(validation_result.span_count, 3)
  
  // Test inconsistent trace
  let inconsistent_span = Span::new("inconsistent_span", Internal, SpanContext::new("different_trace_id", "inconsistent_span_id", true, ""))
  trace_consistency_validator.add_span(inconsistent_span)
  
  let inconsistent_validation = trace_consistency_validator.validate()
  assert_false(inconsistent_validation.is_consistent)
  assert_eq(inconsistent_validation.errors.length(), 1)
  assert_eq(inconsistent_validation.errors[0], "Trace ID mismatch: expected 0af7651916cd43dd8448eb211c80319c, found different_trace_id")
}

// Test 2: Metric Data Consistency
test "metric data consistency validation" {
  let metric_consistency_validator = MetricConsistencyValidator::new()
  
  // Create metrics with consistent data types
  let counter_metric = Metric::new_counter("request_count", "Total number of requests", "count")
  Metric::add_data_point(counter_metric, DataPoint::new(100.0, Attributes::new()))
  Metric::add_data_point(counter_metric, DataPoint::new(200.0, Attributes::new()))
  
  let histogram_metric = Metric::new_histogram("request_duration", "Request duration", "ms")
  Metric::add_data_point(histogram_metric, DataPoint::new(50.0, Attributes::new()))
  Metric::add_data_point(histogram_metric, DataPoint::new(100.0, Attributes::new()))
  
  // Add metrics to validator
  metric_consistency_validator.add_metric(counter_metric)
  metric_consistency_validator.add_metric(histogram_metric)
  
  // Validate metric consistency
  let validation_result = metric_consistency_validator.validate()
  
  // Verify validation passes
  assert_true(validation_result.is_consistent)
  assert_eq(validation_result.metric_count, 2)
  
  // Test inconsistent metric data type
  let inconsistent_metric = Metric::new_counter("inconsistent_metric", "Inconsistent metric", "count")
  Metric::add_data_point(inconsistent_metric, DataPoint::new("invalid_string_value", Attributes::new())) // String value for counter
  
  metric_consistency_validator.add_metric(inconsistent_metric)
  
  let inconsistent_validation = metric_consistency_validator.validate()
  assert_false(inconsistent_validation.is_consistent)
  assert_eq(inconsistent_validation.errors.length(), 1)
  assert_true(inconsistent_validation.errors[0].contains("Data type mismatch"))
}

// Test 3: Log Record Consistency
test "log record consistency validation" {
  let log_consistency_validator = LogConsistencyValidator::new()
  
  // Create consistent log records
  let log_record1 = LogRecord::new(Info, "User login successful")
  LogRecord::set_timestamp(log_record1, 1234567890L)
  LogRecord::set_trace_id(log_record1, "trace_id_123")
  LogRecord::set_span_id(log_record1, "span_id_123")
  
  let log_record2 = LogRecord::new(Error, "Database connection failed")
  LogRecord::set_timestamp(log_record2, 1234567891L)
  LogRecord::set_trace_id(log_record2, "trace_id_123")
  LogRecord::set_span_id(log_record2, "span_id_456")
  
  // Add log records to validator
  log_consistency_validator.add_log_record(log_record1)
  log_consistency_validator.add_log_record(log_record2)
  
  // Validate log consistency
  let validation_result = log_consistency_validator.validate()
  
  // Verify validation passes
  assert_true(validation_result.is_consistent)
  assert_eq(validation_result.log_count, 2)
  
  // Test inconsistent log timestamp (log before span creation)
  let inconsistent_log = LogRecord::new(Info, "Inconsistent timestamp")
  LogRecord::set_timestamp(inconsistent_log, 1234567889L) // Before span creation
  LogRecord::set_trace_id(inconsistent_log, "trace_id_123")
  LogRecord::set_span_id(inconsistent_log, "span_id_123")
  
  log_consistency_validator.add_log_record(inconsistent_log)
  
  let inconsistent_validation = log_consistency_validator.validate()
  assert_false(inconsistent_validation.is_consistent)
  assert_eq(inconsistent_validation.errors.length(), 1)
  assert_true(inconsistent_validation.errors[0].contains("Timestamp inconsistency"))
}

// Test 4: Attribute Consistency
test "attribute consistency validation" {
  let attribute_consistency_validator = AttributeConsistencyValidator::new()
  
  // Define attribute schema
  let schema = AttributeSchema::new()
  AttributeSchema::add_attribute(schema, "user_id", AttributeType::String, true) // Required
  AttributeSchema::add_attribute(schema, "request_id", AttributeType::String, true) // Required
  AttributeSchema::add_attribute(schema, "response_time", AttributeType::Float, false) // Optional
  AttributeSchema::add_attribute(schema, "success", AttributeType::Boolean, false) // Optional
  
  attribute_consistency_validator.set_schema(schema)
  
  // Create attributes with consistent data
  let consistent_attrs = Attributes::new()
  Attributes::set(consistent_attrs, "user_id", StringValue("user_123"))
  Attributes::set(consistent_attrs, "request_id", StringValue("req_456"))
  Attributes::set(consistent_attrs, "response_time", FloatValue(250.5))
  Attributes::set(consistent_attrs, "success", BoolValue(true))
  
  // Validate consistent attributes
  let consistent_result = attribute_consistency_validator.validate(consistent_attrs)
  assert_true(consistent_result.is_consistent)
  
  // Test missing required attribute
  let missing_required_attrs = Attributes::new()
  Attributes::set(missing_required_attrs, "user_id", StringValue("user_123"))
  // Missing required "request_id" attribute
  
  let missing_result = attribute_consistency_validator.validate(missing_required_attrs)
  assert_false(missing_result.is_consistent)
  assert_eq(missing_result.errors.length(), 1)
  assert_true(missing_result.errors[0].contains("Missing required attribute: request_id"))
  
  // Test incorrect attribute type
  let incorrect_type_attrs = Attributes::new()
  Attributes::set(incorrect_type_attrs, "user_id", StringValue("user_123"))
  Attributes::set(incorrect_type_attrs, "request_id", StringValue("req_456"))
  Attributes::set(incorrect_type_attrs, "response_time", StringValue("250.5")) // String instead of Float
  
  let incorrect_type_result = attribute_consistency_validator.validate(incorrect_type_attrs)
  assert_false(incorrect_type_result.is_consistent)
  assert_eq(incorrect_type_result.errors.length(), 1)
  assert_true(incorrect_type_result.errors[0].contains("Type mismatch for attribute: response_time"))
}

// Test 5: Cross-Service Data Consistency
test "cross-service data consistency validation" {
  let cross_service_validator = CrossServiceConsistencyValidator::new()
  
  // Define service topology
  let topology = ServiceTopology::new()
  ServiceTopology::add_service(topology, "api-gateway")
  ServiceTopology::add_service(topology, "user-service")
  ServiceTopology::add_service(topology, "order-service")
  ServiceTopology::add_dependency(topology, "api-gateway", "user-service")
  ServiceTopology::add_dependency(topology, "api-gateway", "order-service")
  
  cross_service_validator.set_topology(topology)
  
  // Create telemetry data from different services
  let api_gateway_span = Span::new("api-request", Server, SpanContext::new("trace_123", "span_1", true, ""))
  Span::set_service_name(api_gateway_span, "api-gateway")
  
  let user_service_span = Span::new("user-lookup", Client, SpanContext::new("trace_123", "span_2", true, ""))
  Span::set_service_name(user_service_span, "user-service")
  Span::set_parent(user_service_span, api_gateway_span)
  
  let order_service_span = Span::new("order-create", Client, SpanContext::new("trace_123", "span_3", true, ""))
  Span::set_service_name(order_service_span, "order-service")
  Span::set_parent(order_service_span, api_gateway_span)
  
  // Add spans to validator
  cross_service_validator.add_span(api_gateway_span)
  cross_service_validator.add_span(user_service_span)
  cross_service_validator.add_span(order_service_span)
  
  // Validate cross-service consistency
  let validation_result = cross_service_validator.validate()
  
  // Verify validation passes
  assert_true(validation_result.is_consistent)
  assert_eq(validation_result.service_count, 3)
  assert_eq(validation_result.span_count, 3)
  
  // Test inconsistent service relationship
  let orphan_span = Span::new("orphan-operation", Internal, SpanContext::new("trace_123", "span_4", true, ""))
  Span::set_service_name(orphan_span, "unknown-service") // Service not in topology
  
  cross_service_validator.add_span(orphan_span)
  
  let inconsistent_validation = cross_service_validator.validate()
  assert_false(inconsistent_validation.is_consistent)
  assert_eq(inconsistent_validation.errors.length(), 1)
  assert_true(inconsistent_validation.errors[0].contains("Unknown service: unknown-service"))
}

// Test 6: Temporal Consistency
test "temporal consistency validation" {
  let temporal_consistency_validator = TemporalConsistencyValidator::new()
  
  // Create spans with consistent timestamps
  let root_span = Span::new("root_operation", Server, SpanContext::new("trace_123", "span_1", true, ""))
  Span::set_start_time(root_span, 1000L)
  Span::set_end_time(root_span, 1500L)
  
  let child_span1 = Span::new("child_operation1", Client, SpanContext::new("trace_123", "span_2", true, ""))
  Span::set_start_time(child_span1, 1100L)
  Span::set_end_time(child_span1, 1300L)
  Span::set_parent(child_span1, root_span)
  
  let child_span2 = Span::new("child_operation2", Client, SpanContext::new("trace_123", "span_3", true, ""))
  Span::set_start_time(child_span2, 1300L)
  Span::set_end_time(child_span2, 1400L)
  Span::set_parent(child_span2, root_span)
  
  // Add spans to validator
  temporal_consistency_validator.add_span(root_span)
  temporal_consistency_validator.add_span(child_span1)
  temporal_consistency_validator.add_span(child_span2)
  
  // Validate temporal consistency
  let validation_result = temporal_consistency_validator.validate()
  
  // Verify validation passes
  assert_true(validation_result.is_consistent)
  
  // Test temporal inconsistency (child span starts before parent)
  let inconsistent_span = Span::new("inconsistent_operation", Client, SpanContext::new("trace_123", "span_4", true, ""))
  Span::set_start_time(inconsistent_span, 900L) // Before parent span starts
  Span::set_end_time(inconsistent_span, 1000L)
  Span::set_parent(inconsistent_span, root_span)
  
  temporal_consistency_validator.add_span(inconsistent_span)
  
  let inconsistent_validation = temporal_consistency_validator.validate()
  assert_false(inconsistent_validation.is_consistent)
  assert_eq(inconsistent_validation.errors.length(), 1)
  assert_true(inconsistent_validation.errors[0].contains("Temporal inconsistency"))
}

// Test 7: Causal Consistency
test "causal consistency validation" {
  let causal_consistency_validator = CausalConsistencyValidator::new()
  
  // Create operations with causal relationships
  let operation1 = Operation::new("create_user", 1L)
  Operation::add_causal_predecessor(operation1, [])
  
  let operation2 = Operation::new("update_user", 2L)
  Operation::add_causal_predecessor(operation2, [1L]) // Depends on operation1
  
  let operation3 = Operation::new("delete_user", 3L)
  Operation::add_causal_predecessor(operation3, [1L, 2L]) // Depends on operation1 and operation2
  
  // Add operations to validator
  causal_consistency_validator.add_operation(operation1)
  causal_consistency_validator.add_operation(operation2)
  causal_consistency_validator.add_operation(operation3)
  
  // Validate causal consistency
  let validation_result = causal_consistency_validator.validate()
  
  // Verify validation passes
  assert_true(validation_result.is_consistent)
  
  // Test causal inconsistency (missing predecessor)
  let orphaned_operation = Operation::new("orphaned_operation", 4L)
  Operation::add_causal_predecessor(orphaned_operation, [5L]) // Depends on non-existent operation
  
  causal_consistency_validator.add_operation(orphaned_operation)
  
  let inconsistent_validation = causal_consistency_validator.validate()
  assert_false(inconsistent_validation.is_consistent)
  assert_eq(inconsistent_validation.errors.length(), 1)
  assert_true(inconsistent_validation.errors[0].contains("Missing causal predecessor"))
}

// Test 8: Data Integrity Consistency
test "data integrity consistency validation" {
  let data_integrity_validator = DataIntegrityValidator::new()
  
  // Create telemetry data with integrity checksums
  let telemetry_data = TelemetryData::new()
  TelemetryData::add_span(telemetry_data, Span::new("test_span", Internal, SpanContext::new("trace_123", "span_1", true, "")))
  TelemetryData::add_metric(telemetry_data, Metric::new_counter("test_counter", "Test counter", "count"))
  TelemetryData::add_log(telemetry_data, LogRecord::new(Info, "Test log"))
  
  // Calculate and set integrity checksum
  let checksum = DataIntegrity::calculate_checksum(telemetry_data)
  TelemetryData::set_integrity_checksum(telemetry_data, checksum)
  
  // Validate data integrity
  let validation_result = data_integrity_validator.validate(telemetry_data)
  
  // Verify validation passes
  assert_true(validation_result.is_integrity_preserved)
  
  // Test data corruption
  let corrupted_data = TelemetryData::clone(telemetry_data)
  TelemetryData::corrupt_data(corrupted_data) // Simulate data corruption
  
  let corrupted_validation = data_integrity_validator.validate(corrupted_data)
  assert_false(corrupted_validation.is_integrity_preserved)
  assert_eq(corrupted_validation.errors.length(), 1)
  assert_true(corrupted_validation.errors[0].contains("Checksum mismatch"))
}

// Test 9: Semantic Consistency
test "semantic consistency validation" {
  let semantic_consistency_validator = SemanticConsistencyValidator::new()
  
  // Define semantic rules
  let semantic_rules = SemanticRules::new()
  SemanticRules::add_rule(semantic_rules, "HTTP status codes", fn(attrs) {
    match Attributes::get(attrs, "http.status_code") {
      Some(IntValue(code)) => code >= 100 && code < 600
      _ => false
    }
  })
  
  SemanticRules::add_rule(semantic_rules, "Response time", fn(attrs) {
    match Attributes::get(attrs, "response_time") {
      Some(FloatValue(time)) => time >= 0.0
      _ => false
    }
  })
  
  semantic_consistency_validator.set_rules(semantic_rules)
  
  // Create attributes with semantically consistent data
  let consistent_attrs = Attributes::new()
  Attributes::set(consistent_attrs, "http.status_code", IntValue(200))
  Attributes::set(consistent_attrs, "response_time", FloatValue(150.5))
  
  // Validate semantic consistency
  let consistent_result = semantic_consistency_validator.validate(consistent_attrs)
  assert_true(consistent_result.is_semantically_consistent)
  
  // Create attributes with semantically inconsistent data
  let inconsistent_attrs = Attributes::new()
  Attributes::set(inconsistent_attrs, "http.status_code", IntValue(999)) // Invalid HTTP status code
  Attributes::set(inconsistent_attrs, "response_time", FloatValue(-10.5)) // Negative response time
  
  let inconsistent_result = semantic_consistency_validator.validate(inconsistent_attrs)
  assert_false(inconsistent_result.is_semantically_consistent)
  assert_eq(inconsistent_result.errors.length(), 2)
  assert_true(inconsistent_result.errors[0].contains("HTTP status codes"))
  assert_true(inconsistent_result.errors[1].contains("Response time"))
}

// Test 10: Distributed Transaction Consistency
test "distributed transaction consistency validation" {
  let distributed_transaction_validator = DistributedTransactionValidator::new()
  
  // Create a distributed transaction with multiple participants
  let transaction = DistributedTransaction::new("txn_123")
  
  let participant1 = TransactionParticipant::new("account-service", "account_123")
  TransactionParticipant::set_operation(participant1, "debit", 100.0)
  
  let participant2 = TransactionParticipant::new("order-service", "order_456")
  TransactionParticipant::set_operation(participant2, "create", 100.0)
  
  let participant3 = TransactionParticipant::new("inventory-service", "item_789")
  TransactionParticipant::set_operation(participant3, "reserve", 1)
  
  // Add participants to transaction
  DistributedTransaction::add_participant(transaction, participant1)
  DistributedTransaction::add_participant(transaction, participant2)
  DistributedTransaction::add_participant(transaction, participant3)
  
  // Set transaction state to committed
  DistributedTransaction::set_state(transaction, Committed)
  
  // Add transaction to validator
  distributed_transaction_validator.add_transaction(transaction)
  
  // Validate distributed transaction consistency
  let validation_result = distributed_transaction_validator.validate()
  
  // Verify validation passes
  assert_true(validation_result.is_consistent)
  assert_eq(validation_result.participant_count, 3)
  
  // Test inconsistent transaction state (participants in different states)
  let inconsistent_transaction = DistributedTransaction::new("txn_456")
  
  let consistent_participant = TransactionParticipant::new("account-service", "account_789")
  TransactionParticipant::set_operation(consistent_participant, "credit", 50.0)
  TransactionParticipant::set_state(consistent_participant, Committed)
  
  let inconsistent_participant = TransactionParticipant::new("order-service", "order_123")
  TransactionParticipant::set_operation(inconsistent_participant, "cancel", 50.0)
  TransactionParticipant::set_state(inconsistent_participant, Aborted) // Different state
  
  DistributedTransaction::add_participant(inconsistent_transaction, consistent_participant)
  DistributedTransaction::add_participant(inconsistent_transaction, inconsistent_participant)
  DistributedTransaction::set_state(inconsistent_transaction, Committed)
  
  distributed_transaction_validator.add_transaction(inconsistent_transaction)
  
  let inconsistent_validation = distributed_transaction_validator.validate()
  assert_false(inconsistent_validation.is_consistent)
  assert_eq(inconsistent_validation.errors.length(), 1)
  assert_true(inconsistent_validation.errors[0].contains("Participant state mismatch"))
}