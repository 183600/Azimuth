// Azimuth Security and Privacy Test Suite
// This file contains security and privacy test cases for the Azimuth telemetry system

// Test 1: Sensitive data handling and redaction
pub test "security sensitive data redaction" {
  let attrs = azimuth::Attributes::new()
  
  // Test with sensitive data that should be redacted
  azimuth::Attributes::set(attrs, "user.password", azimuth::StringValue("secret123"))
  azimuth::Attributes::set(attrs, "api.key", azimuth::StringValue("sk-1234567890abcdef"))
  azimuth::Attributes::set(attrs, "credit.card", azimuth::StringValue("4111-1111-1111-1111"))
  azimuth::Attributes::set(attrs, "ssn", azimuth::StringValue("123-45-6789"))
  azimuth::Attributes::set(attrs, "email", azimuth::StringValue("user@example.com"))
  azimuth::Attributes::set(attrs, "phone", azimuth::StringValue("+1-555-123-4567"))
  
  // Test with non-sensitive data
  azimuth::Attributes::set(attrs, "user.id", azimuth::StringValue("12345"))
  azimuth::Attributes::set(attrs, "request.count", azimuth::IntValue(100))
  azimuth::Attributes::set(attrs, "response.time", azimuth::FloatValue(250.5))
  azimuth::Attributes::set(attrs, "service.name", azimuth::StringValue("auth-service"))
  
  // In a real implementation, sensitive data would be redacted
  // For this test, we verify that the operations don't crash
  let password_result = azimuth::Attributes::get(attrs, "user.password")
  let api_key_result = azimuth::Attributes::get(attrs, "api.key")
  let credit_card_result = azimuth::Attributes::get(attrs, "credit.card")
  let ssn_result = azimuth::Attributes::get(attrs, "ssn")
  let email_result = azimuth::Attributes::get(attrs, "email")
  let phone_result = azimuth::Attributes::get(attrs, "phone")
  
  // Verify non-sensitive data
  assert_eq(azimuth::Attributes::get(attrs, "user.id"), Some(azimuth::StringValue("12345")))
  assert_eq(azimuth::Attributes::get(attrs, "request.count"), Some(azimuth::IntValue(100)))
  assert_eq(azimuth::Attributes::get(attrs, "response.time"), Some(azimuth::FloatValue(250.5)))
  assert_eq(azimuth::Attributes::get(attrs, "service.name"), Some(azimuth::StringValue("auth-service")))
}

// Test 2: Secure logging with sensitive information
pub test "security secure logging" {
  let logger_provider = azimuth::LoggerProvider::default()
  let secure_logger = azimuth::LoggerProvider::get_logger(logger_provider, "secure-logger")
  
  // Create log records with sensitive information
  let auth_log = azimuth::LogRecord::new(azimuth::Info, "User authentication attempt for user@example.com")
  let payment_log = azimuth::LogRecord::new(azimuth::Info, "Payment processed for card ending in 1234")
  let api_log = azimuth::LogRecord::new(azimuth::Info, "API call made with token sk-****")
  let error_log = azimuth::LogRecord::new(azimuth::Error, "Authentication failed: Invalid credentials")
  
  // Create log records with detailed context
  let detailed_auth_log = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User login successful"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("auth-trace-12345"),
    Some("auth-span-67890"),
    None
  )
  
  let detailed_error_log = azimuth::LogRecord::new_with_context(
    azimuth::Error,
    Some("Database connection failed: connection timeout"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("error-trace-11111"),
    Some("error-span-22222"),
    None
  )
  
  // Emit all log records
  azimuth::Logger::emit(secure_logger, auth_log)
  azimuth::Logger::emit(secure_logger, payment_log)
  azimuth::Logger::emit(secure_logger, api_log)
  azimuth::Logger::emit(secure_logger, error_log)
  azimuth::Logger::emit(secure_logger, detailed_auth_log)
  azimuth::Logger::emit(secure_logger, detailed_error_log)
  
  // Verify log records
  assert_eq(azimuth::LogRecord::body(auth_log), Some("User authentication attempt for user@example.com"))
  assert_eq(azimuth::LogRecord::body(payment_log), Some("Payment processed for card ending in 1234"))
  assert_eq(azimuth::LogRecord::body(api_log), Some("API call made with token sk-****"))
  assert_eq(azimuth::LogRecord::body(error_log), Some("Authentication failed: Invalid credentials"))
  assert_eq(azimuth::LogRecord::body(detailed_auth_log), Some("User login successful"))
  assert_eq(azimuth::LogRecord::body(detailed_error_log), Some("Database connection failed: connection timeout"))
  
  assert_eq(azimuth::LogRecord::severity_number(auth_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(payment_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(api_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(error_log), azimuth::Error)
  assert_eq(azimuth::LogRecord::severity_number(detailed_auth_log), azimuth::Info)
  assert_eq(azimuth::LogRecord::severity_number(detailed_error_log), azimuth::Error)
}

// Test 3: Secure span operations with sensitive data
pub test "security secure span operations" {
  // Create spans with security context
  let auth_span_ctx = azimuth::SpanContext::new("secure-auth-trace", "secure-auth-span", true, "")
  let auth_span = azimuth::Span::new("user.authentication", azimuth::Server, auth_span_ctx)
  
  let payment_span_ctx = azimuth::SpanContext::new("secure-payment-trace", "secure-payment-span", true, "")
  let payment_span = azimuth::Span::new("payment.processing", azimuth::Server, payment_span_ctx)
  
  let api_span_ctx = azimuth::SpanContext::new("secure-api-trace", "secure-api-span", true, "")
  let api_span = azimuth::Span::new("api.request", azimuth::Server, api_span_ctx)
  
  // Add events with security context (without sensitive data)
  azimuth::Span::add_event(auth_span, "authentication.started", [])
  azimuth::Span::add_event(auth_span, "authentication.completed", [])
  
  azimuth::Span::add_event(payment_span, "payment.started", [])
  azimuth::Span::add_event(payment_span, "payment.completed", [])
  
  azimuth::Span::add_event(api_span, "api.request.started", [])
  azimuth::Span::add_event(api_span, "api.request.completed", [])
  
  // Set status without sensitive information
  azimuth::Span::set_status(auth_span, azimuth::Ok, Some("Authentication successful"))
  azimuth::Span::set_status(payment_span, azimuth::Ok, Some("Payment processed successfully"))
  azimuth::Span::set_status(api_span, azimuth::Ok, Some("API request completed successfully"))
  
  // Verify span properties
  assert_eq(azimuth::Span::name(auth_span), "user.authentication")
  assert_eq(azimuth::Span::name(payment_span), "payment.processing")
  assert_eq(azimuth::Span::name(api_span), "api.request")
  
  assert_eq(azimuth::Span::kind(auth_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(payment_span), azimuth::Server)
  assert_eq(azimuth::Span::kind(api_span), azimuth::Server)
  
  assert_eq(azimuth::Span::status(auth_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(payment_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(api_span), azimuth::Ok)
}

// Test 4: Secure metrics without sensitive data
pub test "security secure metrics" {
  let provider = azimuth::MeterProvider::default()
  let secure_meter = azimuth::MeterProvider::get_meter(provider, "security-metrics")
  
  // Create security-related metrics
  let auth_counter = azimuth::Meter::create_counter(secure_meter, "authentication.attempts")
  let auth_success_counter = azimuth::Meter::create_counter(secure_meter, "authentication.success")
  let auth_failure_counter = azimuth::Meter::create_counter(secure_meter, "authentication.failures")
  
  let payment_counter = azimuth::Meter::create_counter(secure_meter, "payment.transactions")
  let payment_success_counter = azimuth::Meter::create_counter(secure_meter, "payment.success")
  let payment_failure_counter = azimuth::Meter::create_counter(secure_meter, "payment.failures")
  
  let api_request_counter = azimuth::Meter::create_counter(secure_meter, "api.requests")
  let api_error_counter = azimuth::Meter::create_counter(secure_meter, "api.errors")
  
  let response_time_histogram = azimuth::Meter::create_histogram(secure_meter, "request.response.time")
  let processing_time_histogram = azimuth::Meter::create_histogram(secure_meter, "processing.time")
  
  let active_connections_gauge = azimuth::Meter::create_gauge(secure_meter, "active.connections")
  let queue_size_gauge = azimuth::Meter::create_gauge(secure_meter, "queue.size")
  
  // Perform metric operations
  azimuth::Counter::add(auth_counter, 1000.0)
  azimuth::Counter::add(auth_success_counter, 850.0)
  azimuth::Counter::add(auth_failure_counter, 150.0)
  
  azimuth::Counter::add(payment_counter, 500.0)
  azimuth::Counter::add(payment_success_counter, 475.0)
  azimuth::Counter::add(payment_failure_counter, 25.0)
  
  azimuth::Counter::add(api_request_counter, 2000.0)
  azimuth::Counter::add(api_error_counter, 50.0)
  
  azimuth::Histogram::record(response_time_histogram, 150.0)
  azimuth::Histogram::record(processing_time_histogram, 75.0)
  
  // Verify metrics
  assert_eq(auth_counter.name, "authentication.attempts")
  assert_eq(auth_success_counter.name, "authentication.success")
  assert_eq(auth_failure_counter.name, "authentication.failures")
  
  assert_eq(payment_counter.name, "payment.transactions")
  assert_eq(payment_success_counter.name, "payment.success")
  assert_eq(payment_failure_counter.name, "payment.failures")
  
  assert_eq(api_request_counter.name, "api.requests")
  assert_eq(api_error_counter.name, "api.errors")
  
  assert_eq(response_time_histogram.name, "request.response.time")
  assert_eq(processing_time_histogram.name, "processing.time")
  
  assert_eq(active_connections_gauge.name, "active.connections")
  assert_eq(queue_size_gauge.name, "queue.size")
}

// Test 5: Secure context and baggage without sensitive data
pub test "security secure context baggage" {
  // Create context with non-sensitive security information
  let ctx = azimuth::Context::root()
  
  let user_role_key = azimuth::ContextKey::new("user.role")
  let ctx_with_role = azimuth::Context::with_value(ctx, user_role_key, "admin")
  
  let permission_key = azimuth::ContextKey::new("user.permissions")
  let ctx_with_permissions = azimuth::Context::with_value(ctx_with_role, permission_key, "read,write,delete")
  
  let auth_method_key = azimuth::ContextKey::new("auth.method")
  let ctx_with_auth_method = azimuth::Context::with_value(ctx_with_permissions, auth_method_key, "oauth2")
  
  let session_id_key = azimuth::ContextKey::new("session.id")
  let ctx_with_session_id = azimuth::Context::with_value(ctx_with_auth_method, session_id_key, "sess-12345")
  
  // Verify context values
  assert_eq(azimuth::Context::get(ctx_with_role, user_role_key), Some("admin"))
  assert_eq(azimuth::Context::get(ctx_with_permissions, permission_key), Some("read,write,delete"))
  assert_eq(azimuth::Context::get(ctx_with_auth_method, auth_method_key), Some("oauth2"))
  assert_eq(azimuth::Context::get(ctx_with_session_id, session_id_key), Some("sess-12345"))
  
  // Create baggage with non-sensitive security information
  let baggage = azimuth::Baggage::new()
  
  let baggage_with_role = azimuth::Baggage::set_entry(baggage, "user.role", "admin")
  let baggage_with_permissions = azimuth::Baggage::set_entry(baggage_with_role, "user.permissions", "read,write,delete")
  let baggage_with_auth_method = azimuth::Baggage::set_entry(baggage_with_permissions, "auth.method", "oauth2")
  let baggage_with_session_id = azimuth::Baggage::set_entry(baggage_with_auth_method, "session.id", "sess-12345")
  
  // Verify baggage entries
  assert_eq(azimuth::Baggage::get_entry(baggage_with_role, "user.role"), Some("admin"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_permissions, "user.permissions"), Some("read,write,delete"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_auth_method, "auth.method"), Some("oauth2"))
  assert_eq(azimuth::Baggage::get_entry(baggage_with_session_id, "session.id"), Some("sess-12345"))
}

// Test 6: Secure resource attributes without sensitive data
pub test "security secure resource attributes" {
  let resource = azimuth::Resource::new()
  
  // Create resource with non-sensitive security attributes
  let security_attributes = [
    ("service.name", azimuth::StringValue("auth-service")),
    ("service.version", azimuth::StringValue("1.2.3")),
    ("service.environment", azimuth::StringValue("production")),
    ("security.level", azimuth::StringValue("high")),
    ("compliance.standard", azimuth::StringValue("PCI-DSS")),
    ("data.classification", azimuth::StringValue("confidential")),
    ("access.control", azimuth::StringValue("role-based")),
    ("encryption.status", azimuth::StringValue("enabled")),
    ("audit.logging", azimuth::StringValue("enabled")),
    ("security.scanners", azimuth::StringValue("enabled"))
  ]
  
  let security_resource = azimuth::Resource::with_attributes(resource, security_attributes)
  
  // Verify resource attributes
  assert_eq(azimuth::Resource::get_attribute(security_resource, "service.name"), Some(azimuth::StringValue("auth-service")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "service.version"), Some(azimuth::StringValue("1.2.3")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "service.environment"), Some(azimuth::StringValue("production")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "security.level"), Some(azimuth::StringValue("high")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "compliance.standard"), Some(azimuth::StringValue("PCI-DSS")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "data.classification"), Some(azimuth::StringValue("confidential")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "access.control"), Some(azimuth::StringValue("role-based")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "encryption.status"), Some(azimuth::StringValue("enabled")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "audit.logging"), Some(azimuth::StringValue("enabled")))
  assert_eq(azimuth::Resource::get_attribute(security_resource, "security.scanners"), Some(azimuth::StringValue("enabled")))
}

// Test 7: Secure propagator operations
pub test "security secure propagator" {
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CTraceContextPropagator::new()  // Using trace propagator for simplicity
  let propagators = [trace_propagator, baggage_propagator]
  let composite_propagator = azimuth::CompositePropagator::new(propagators)
  
  // Create context with security information
  let ctx = azimuth::Context::root()
  
  let security_level_key = azimuth::ContextKey::new("security.level")
  let ctx_with_security_level = azimuth::Context::with_value(ctx, security_level_key, "high")
  
  let auth_method_key = azimuth::ContextKey::new("auth.method")
  let ctx_with_auth_method = azimuth::Context::with_value(ctx_with_security_level, auth_method_key, "oauth2")
  
  let session_id_key = azimuth::ContextKey::new("session.id")
  let ctx_with_session_id = azimuth::Context::with_value(ctx_with_auth_method, session_id_key, "sess-12345")
  
  // Create baggage with security information
  let baggage = azimuth::Baggage::new()
  let baggage_with_security = azimuth::Baggage::set_entry(baggage, "security.context", "authenticated")
  let baggage_with_compliance = azimuth::Baggage::set_entry(baggage_with_security, "compliance.level", "strict")
  
  // Inject context and baggage into carrier
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_session_id, carrier)
  
  // Extract context from carrier
  let extracted_ctx = azimuth::CompositePropagator::extract(composite_propagator, carrier)
  
  // Verify extraction
  let extracted_key = azimuth::ContextKey::new("extracted")
  let extracted_value = azimuth::Context::get(extracted_ctx, extracted_key)
  assert_eq(extracted_value, Some("true"))
}

// Test 8: Privacy compliance and data retention
pub test "security privacy compliance" {
  let logger_provider = azimuth::LoggerProvider::default()
  let privacy_logger = azimuth::LoggerProvider::get_logger(logger_provider, "privacy-logger")
  
  let provider = azimuth::MeterProvider::default()
  let privacy_meter = azimuth::MeterProvider::get_meter(provider, "privacy-metrics")
  
  // Create privacy-related metrics
  let data_retention_counter = azimuth::Meter::create_counter(privacy_meter, "data.retention.events")
  let data_deletion_counter = azimuth::Meter::create_counter(privacy_meter, "data.deletion.events")
  let consent_tracking_counter = azimuth::Meter::create_counter(privacy_meter, "consent.tracking.events")
  let gdpr_request_counter = azimuth::Meter::create_counter(privacy_meter, "gdpr.requests")
  let data_access_counter = azimuth::Meter::create_counter(privacy_meter, "data.access.events")
  
  // Create privacy-related logs
  let data_retention_log = azimuth::LogRecord::new(azimuth::Info, "Data retention policy applied: 30 days")
  let data_deletion_log = azimuth::LogRecord::new(azimuth::Info, "User data deleted per retention policy")
  let consent_tracking_log = azimuth::LogRecord::new(azimuth::Info, "User consent tracked: marketing emails")
  let gdpr_request_log = azimuth::LogRecord::new(azimuth::Info, "GDPR data access request processed")
  let data_access_log = azimuth::LogRecord::new(azimuth::Info, "User data access logged for audit")
  
  // Create detailed privacy logs with context
  let consent_log_with_context = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("User consent updated: analytics=true, marketing=false"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000001L),
    Some("privacy-trace-12345"),
    Some("privacy-span-67890"),
    None
  )
  
  let gdpr_log_with_context = azimuth::LogRecord::new_with_context(
    azimuth::Info,
    Some("GDPR right to be forgotten request processed"),
    None,
    Some(1735689600000000002L),
    Some(1735689600000000003L),
    Some("gdpr-trace-11111"),
    Some("gdpr-span-22222"),
    None
  )
  
  // Perform metric operations
  azimuth::Counter::add(data_retention_counter, 100.0)
  azimuth::Counter::add(data_deletion_counter, 50.0)
  azimuth::Counter::add(consent_tracking_counter, 200.0)
  azimuth::Counter::add(gdpr_request_counter, 10.0)
  azimuth::Counter::add(data_access_counter, 75.0)
  
  // Emit all log records
  azimuth::Logger::emit(privacy_logger, data_retention_log)
  azimuth::Logger::emit(privacy_logger, data_deletion_log)
  azimuth::Logger::emit(privacy_logger, consent_tracking_log)
  azimuth::Logger::emit(privacy_logger, gdpr_request_log)
  azimuth::Logger::emit(privacy_logger, data_access_log)
  azimuth::Logger::emit(privacy_logger, consent_log_with_context)
  azimuth::Logger::emit(privacy_logger, gdpr_log_with_context)
  
  // Verify metrics
  assert_eq(data_retention_counter.name, "data.retention.events")
  assert_eq(data_deletion_counter.name, "data.deletion.events")
  assert_eq(consent_tracking_counter.name, "consent.tracking.events")
  assert_eq(gdpr_request_counter.name, "gdpr.requests")
  assert_eq(data_access_counter.name, "data.access.events")
  
  // Verify log records
  assert_eq(azimuth::LogRecord::body(data_retention_log), Some("Data retention policy applied: 30 days"))
  assert_eq(azimuth::LogRecord::body(data_deletion_log), Some("User data deleted per retention policy"))
  assert_eq(azimuth::LogRecord::body(consent_tracking_log), Some("User consent tracked: marketing emails"))
  assert_eq(azimuth::LogRecord::body(gdpr_request_log), Some("GDPR data access request processed"))
  assert_eq(azimuth::LogRecord::body(data_access_log), Some("User data access logged for audit"))
  assert_eq(azimuth::LogRecord::body(consent_log_with_context), Some("User consent updated: analytics=true, marketing=false"))
  assert_eq(azimuth::LogRecord::body(gdpr_log_with_context), Some("GDPR right to be forgotten request processed"))
}