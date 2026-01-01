// Azimuth Cross-service Integration Tests
// This file contains tests for cross-service integration in the Azimuth telemetry system

// Test 1: End-to-end distributed tracing across services
pub test "end-to-end distributed tracing across services" {
  // Simulate multiple services in a distributed system
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "gateway-service")
  let user_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "user-service")
  let order_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "order-service")
  let payment_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "payment-service")
  let notification_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "notification-service")
  
  // Configure propagator for cross-service context propagation
  let trace_propagator = azimuth::W3CTraceContextPropagator::new()
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  let composite_propagator = azimuth::CompositePropagator::new([trace_propagator, baggage_propagator])
  
  // Start trace at gateway
  let root_span = azimuth::Tracer::start_span(gateway_tracer, "gateway.process-request")
  azimuth::Span::set_attribute(root_span, "http.method", azimuth::StringValue("POST"))
  azimuth::Span::set_attribute(root_span, "http.url", azimuth::StringValue("/api/orders"))
  azimuth::Span::set_attribute(root_span, "user.id", azimuth::StringValue("user-123"))
  
  let root_ctx = azimuth::Context::root()
  let ctx_with_span = azimuth::Context::with_value(root_ctx, azimuth::ContextKey::new("current-span"), root_span)
  
  // Extract context for propagation
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::CompositePropagator::inject(composite_propagator, ctx_with_span, carrier)
  
  // Simulate service-to-service calls
  // Gateway -> User Service
  let user_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(user_carrier, "traceparent", azimuth::TextMapCarrier::get(carrier, "traceparent").unwrap_or(""))
  azimuth::TextMapCarrier::set(user_carrier, "baggage", azimuth::TextMapCarrier::get(carrier, "baggage").unwrap_or(""))
  
  let user_ctx = azimuth::CompositePropagator::extract(composite_propagator, user_carrier)
  let user_span = azimuth::Tracer::start_span(user_tracer, "user.validate-user")
  azimuth::Span::set_attribute(user_span, "user.id", azimuth::StringValue("user-123"))
  azimuth::Span::set_attribute(user_span, "service.name", azimuth::StringValue("user-service"))
  
  // User Service -> Order Service
  let order_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(order_carrier, "traceparent", azimuth::TextMapCarrier::get(user_carrier, "traceparent").unwrap_or(""))
  azimuth::TextMapCarrier::set(order_carrier, "baggage", azimuth::TextMapCarrier::get(user_carrier, "baggage").unwrap_or(""))
  
  let order_ctx = azimuth::CompositePropagator::extract(composite_propagator, order_carrier)
  let order_span = azimuth::Tracer::start_span(order_tracer, "order.create-order")
  azimuth::Span::set_attribute(order_span, "order.id", azimuth::StringValue("order-456"))
  azimuth::Span::set_attribute(order_span, "order.amount", azimuth::IntValue(9999))
  azimuth::Span::set_attribute(order_span, "service.name", azimuth::StringValue("order-service"))
  
  // Order Service -> Payment Service
  let payment_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(payment_carrier, "traceparent", azimuth::TextMapCarrier::get(order_carrier, "traceparent").unwrap_or(""))
  azimuth::TextMapCarrier::set(payment_carrier, "baggage", azimuth::TextMapCarrier::get(order_carrier, "baggage").unwrap_or(""))
  
  let payment_ctx = azimuth::CompositePropagator::extract(composite_propagator, payment_carrier)
  let payment_span = azimuth::Tracer::start_span(payment_tracer, "payment.process-payment")
  azimuth::Span::set_attribute(payment_span, "payment.id", azimuth::StringValue("payment-789"))
  azimuth::Span::set_attribute(payment_span, "payment.amount", azimuth::IntValue(9999))
  azimuth::Span::set_attribute(payment_span, "payment.method", azimuth::StringValue("credit-card"))
  azimuth::Span::set_attribute(payment_span, "service.name", azimuth::StringValue("payment-service"))
  
  // Payment Service -> Notification Service
  let notification_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(notification_carrier, "traceparent", azimuth::TextMapCarrier::get(payment_carrier, "traceparent").unwrap_or(""))
  azimuth::TextMapCarrier::set(notification_carrier, "baggage", azimuth::TextMapCarrier::get(payment_carrier, "baggage").unwrap_or(""))
  
  let notification_ctx = azimuth::CompositePropagator::extract(composite_propagator, notification_carrier)
  let notification_span = azimuth::Tracer::start_span(notification_tracer, "notification.send-confirmation")
  azimuth::Span::set_attribute(notification_span, "notification.type", azimuth::StringValue("email"))
  azimuth::Span::set_attribute(notification_span, "notification.recipient", azimuth::StringValue("user@example.com"))
  azimuth::Span::set_attribute(notification_span, "service.name", azimuth::StringValue("notification-service"))
  
  // End spans in reverse order (LIFO)
  azimuth::Span::end(notification_span)
  azimuth::Span::end(payment_span)
  azimuth::Span::end(order_span)
  azimuth::Span::end(user_span)
  azimuth::Span::end(root_span)
  
  // Verify trace consistency across services
  let root_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::context(root_span))
  let user_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::context(user_span))
  let order_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::context(order_span))
  let payment_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::context(payment_span))
  let notification_trace_id = azimuth::SpanContext::trace_id(azimuth::Span::context(notification_span))
  
  // All spans should have the same trace ID
  assert_eq(root_trace_id, user_trace_id)
  assert_eq(root_trace_id, order_trace_id)
  assert_eq(root_trace_id, payment_trace_id)
  assert_eq(root_trace_id, notification_trace_id)
  
  // Verify span hierarchy (parent-child relationships)
  let root_span_id = azimuth::SpanContext::span_id(azimuth::Span::context(root_span))
  let user_span_id = azimuth::SpanContext::span_id(azimuth::Span::context(user_span))
  let order_span_id = azimuth::SpanContext::span_id(azimuth::Span::context(order_span))
  let payment_span_id = azimuth::SpanContext::span_id(azimuth::Span::context(payment_span))
  let notification_span_id = azimuth::SpanContext::span_id(azimuth::Span::context(notification_span))
  
  // Each span should have a unique span ID
  assert_true(root_span_id != user_span_id)
  assert_true(user_span_id != order_span_id)
  assert_true(order_span_id != payment_span_id)
  assert_true(payment_span_id != notification_span_id)
  
  // Verify service attributes
  assert_eq(azimuth::Span::get_attribute(user_span, "service.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Span::get_attribute(order_span, "service.name"), Some(azimuth::StringValue("order-service")))
  assert_eq(azimuth::Span::get_attribute(payment_span, "service.name"), Some(azimuth::StringValue("payment-service")))
  assert_eq(azimuth::Span::get_attribute(notification_span, "service.name"), Some(azimuth::StringValue("notification-service")))
}

// Test 2: Cross-service metrics correlation
pub test "cross-service metrics correlation" {
  // Create meter providers for different services
  let gateway_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "gateway-service")
  let user_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "user-service")
  let order_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "order-service")
  let payment_meter = azimuth::MeterProvider::get_meter(azimuth::MeterProvider::default(), "payment-service")
  
  // Create correlated metrics across services
  let gateway_requests = azimuth::Meter::create_counter(gateway_meter, "http.requests", Some("HTTP requests"), Some("count"))
  let gateway_duration = azimuth::Meter::create_histogram(gateway_meter, "http.request.duration", Some("Request duration"), Some("ms"))
  
  let user_validations = azimuth::Meter::create_counter(user_meter, "user.validations", Some("User validations"), Some("count"))
  let user_db_queries = azimuth::Meter::create_counter(user_meter, "user.db.queries", Some("Database queries"), Some("count"))
  
  let order_creations = azimuth::Meter::create_counter(order_meter, "order.creations", Some("Order creations"), Some("count"))
  let order_inventory_checks = azimuth::Meter::create_counter(order_meter, "order.inventory.checks", Some("Inventory checks"), Some("count"))
  
  let payment_transactions = azimuth::Meter::create_counter(payment_meter, "payment.transactions", Some("Payment transactions"), Some("count"))
  let payment_amounts = azimuth::Meter::create_histogram(payment_meter, "payment.amounts", Some("Payment amounts"), Some("cents"))
  
  // Simulate correlated operations with trace context
  for i = 0; i < 10; i = i + 1 {
    let trace_id = "trace-" + i.to_string()
    let user_id = "user-" + (i % 5).to_string()
    let order_id = "order-" + i.to_string()
    let payment_id = "payment-" + i.to_string()
    
    // Gateway metrics
    azimuth::Counter::add(gateway_requests, 1, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("user.id", azimuth::StringValue(user_id)),
      ("http.method", azimuth::StringValue("POST")),
      ("http.status", azimuth::StringValue("200"))
    ])
    
    let gateway_latency = 100.0 + (i % 200).to_double()
    azimuth::Histogram::record(gateway_duration, gateway_latency, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("user.id", azimuth::StringValue(user_id))
    ])
    
    // User service metrics
    azimuth::Counter::add(user_validations, 1, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("user.id", azimuth::StringValue(user_id)),
      ("validation.type", azimuth::StringValue("auth"))
    ])
    
    azimuth::Counter::add(user_db_queries, 2, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("user.id", azimuth::StringValue(user_id)),
      ("db.table", azimuth::StringValue("users"))
    ])
    
    // Order service metrics
    azimuth::Counter::add(order_creations, 1, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("order.id", azimuth::StringValue(order_id)),
      ("user.id", azimuth::StringValue(user_id))
    ])
    
    azimuth::Counter::add(order_inventory_checks, 3, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("order.id", azimuth::StringValue(order_id)),
      ("inventory.status", azimuth::StringValue("available"))
    ])
    
    // Payment service metrics
    azimuth::Counter::add(payment_transactions, 1, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("payment.id", azimuth::StringValue(payment_id)),
      ("order.id", azimuth::StringValue(order_id)),
      ("payment.method", azimuth::StringValue("credit-card"))
    ])
    
    let payment_amount = (1000 + i * 100).to_double()
    azimuth::Histogram::record(payment_amounts, payment_amount, [
      ("trace.id", azimuth::StringValue(trace_id)),
      ("payment.id", azimuth::StringValue(payment_id)),
      ("payment.method", azimuth::StringValue("credit-card"))
    ])
  }
  
  // Test cross-service correlation analysis
  let correlation_analyzer = azimuth::CorrelationAnalyzer::new()
  
  // Register metrics for correlation
  azimuth::CorrelationAnalyzer::add_metric(correlation_analyzer, "gateway.requests", gateway_requests)
  azimuth::CorrelationAnalyzer::add_metric(correlation_analyzer, "user.validations", user_validations)
  azimuth::CorrelationAnalyzer::add_metric(correlation_analyzer, "order.creations", order_creations)
  azimuth::CorrelationAnalyzer::add_metric(correlation_analyzer, "payment.transactions", payment_transactions)
  
  // Analyze correlations by trace ID
  let trace_correlations = azimuth::CorrelationAnalyzer::analyze_by_trace(correlation_analyzer)
  
  // Verify trace correlations
  assert_true(trace_correlations.length() >= 10)
  
  for correlation in trace_correlations.take(5) {
    assert_true(correlation.trace_id.starts_with("trace-"))
    assert_eq(correlation.metrics["gateway.requests"], 1)
    assert_eq(correlation.metrics["user.validations"], 1)
    assert_eq(correlation.metrics["order.creations"], 1)
    assert_eq(correlation.metrics["payment.transactions"], 1)
  }
  
  // Analyze correlations by user ID
  let user_correlations = azimuth::CorrelationAnalyzer::analyze_by_user(correlation_analyzer)
  
  // Verify user correlations
  assert_true(user_correlations.length() >= 5)  # 5 different users
  
  for correlation in user_correlations {
    assert_true(correlation.user_id.starts_with("user-"))
    assert_eq(correlation.metrics["gateway.requests"], correlation.metrics["user.validations"])
    assert_eq(correlation.metrics["user.validations"], correlation.metrics["order.creations"])
    assert_eq(correlation.metrics["order.creations"], correlation.metrics["payment.transactions"])
  }
}

// Test 3: Cross-service error propagation and handling
pub test "cross-service error propagation and handling" {
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "gateway-service")
  let auth_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "auth-service")
  let data_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "data-service")
  
  // Configure propagator
  let propagator = azimuth::CompositePropagator::new([
    azimuth::W3CTraceContextPropagator::new(),
    azimuth::W3CBaggagePropagator::new()
  ])
  
  // Test error propagation scenario
  let root_span = azimuth::Tracer::start_span(gateway_tracer, "gateway.process-request")
  azimuth::Span::set_attribute(root_span, "http.method", azimuth::StringValue("GET"))
  azimuth::Span::set_attribute(root_span, "http.url", azimuth::StringValue("/api/protected-data"))
  
  // Extract context for propagation
  let carrier = azimuth::TextMapCarrier::new()
  let ctx = azimuth::Context::with_value(azimuth::Context::root(), azimuth::ContextKey::new("current-span"), root_span)
  azimuth::CompositePropagator::inject(propagator, ctx, carrier)
  
  // Simulate auth service error
  let auth_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(auth_carrier, "traceparent", azimuth::TextMapCarrier::get(carrier, "traceparent").unwrap_or(""))
  
  let auth_ctx = azimuth::CompositePropagator::extract(propagator, auth_carrier)
  let auth_span = azimuth::Tracer::start_span(auth_tracer, "auth.authenticate")
  azimuth::Span::set_attribute(auth_span, "auth.token", azimuth::StringValue("invalid-token"))
  
  // Simulate authentication failure
  let auth_error = azimuth::SpanError::new("AUTHENTICATION_FAILED", "Invalid authentication token")
    .with_attribute("error.code", azimuth::IntValue(401))
    .with_attribute("error.type", azimuth::StringValue("authentication"))
  
  azimuth::Span::set_status(auth_span, azimuth::Error)
  azimuth::Span::record_error(auth_span, auth_error)
  azimuth::Span::end(auth_span)
  
  // Simulate error propagation back to gateway
  let error_propagated = azimuth::ErrorPropagator::propagate(auth_error, carrier)
  assert_true(error_propagated)
  
  // Gateway handles the error
  let gateway_error = azimuth::ErrorPropagator::extract_error(carrier)
  assert_true(gateway_error.is_some)
  
  match gateway_error {
    Some(error) => {
      assert_eq(error.code, "AUTHENTICATION_FAILED")
      assert_eq(error.message, "Invalid authentication token")
      assert_true(error.attributes.contains(("error.code", azimuth::IntValue(401))))
    }
    None => assert_true(false)
  }
  
  // Record error on gateway span
  match gateway_error {
    Some(error) => azimuth::Span::record_error(root_span, error)
    None => {}
  }
  
  azimuth::Span::set_status(root_span, azimuth::Error)
  azimuth::Span::end(root_span)
  
  // Verify error propagation
  let auth_span_context = azimuth::Span::context(auth_span)
  let gateway_span_context = azimuth::Span::context(root_span)
  
  assert_eq(azimuth::SpanContext::trace_id(auth_span_context), azimuth::SpanContext::trace_id(gateway_span_context))
  assert_eq(azimuth::Span::status(auth_span), azimuth::Error)
  assert_eq(azimuth::Span::status(root_span), azimuth::Error)
  
  // Test error recovery scenario
  let recovery_span = azimuth::Tracer::start_span(gateway_tracer, "gateway.recovery-attempt")
  azimuth::Span::set_attribute(recovery_span, "recovery.strategy", azimuth::StringValue("retry-with-fallback"))
  
  // Simulate successful recovery
  let fallback_carrier = azimuth::TextMapCarrier::new()
  let fallback_ctx = azimuth::Context::with_value(azimuth::Context::root(), azimuth::ContextKey::new("current-span"), recovery_span)
  azimuth::CompositePropagator::inject(propagator, fallback_ctx, fallback_carrier)
  
  let data_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(data_carrier, "traceparent", azimuth::TextMapCarrier::get(fallback_carrier, "traceparent").unwrap_or(""))
  
  let data_ctx = azimuth::CompositePropagator::extract(propagator, data_carrier)
  let data_span = azimuth::Tracer::start_span(data_tracer, "data.fetch-fallback")
  azimuth::Span::set_attribute(data_span, "data.source", azimuth::StringValue("cache"))
  
  // Successful operation
  azimuth::Span::set_status(data_span, azimuth::Ok)
  azimuth::Span::end(data_span)
  
  azimuth::Span::set_status(recovery_span, azimuth::Ok)
  azimuth::Span::end(recovery_span)
  
  // Verify recovery
  assert_eq(azimuth::Span::status(data_span), azimuth::Ok)
  assert_eq(azimuth::Span::status(recovery_span), azimuth::Ok)
  assert_eq(azimuth::SpanContext::trace_id(azimuth::Span::context(data_span)), azimuth::SpanContext::trace_id(azimuth::Span::context(recovery_span)))
}

// Test 4: Cross-service baggage propagation
pub test "cross-service baggage propagation" {
  let gateway_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "gateway-service")
  let service_a_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-a")
  let service_b_tracer = azimuth::TracerProvider::get_tracer(azimuth::TracerProvider::default(), "service-b")
  
  // Configure baggage propagator
  let baggage_propagator = azimuth::W3CBaggagePropagator::new()
  
  // Create initial baggage at gateway
  let initial_baggage = azimuth::Baggage::new()
  let baggage_with_user = azimuth::Baggage::set_entry(initial_baggage, "user.id", "user-123")
  let baggage_with_session = azimuth::Baggage::set_entry(baggage_with_user, "session.id", "session-456")
  let baggage_with_request = azimuth::Baggage::set_entry(baggage_with_session, "request.id", "req-789")
  
  // Start span with baggage
  let gateway_span = azimuth::Tracer::start_span(gateway_tracer, "gateway.process-request")
  let gateway_ctx = azimuth::Context::with_value(azimuth::Context::root(), azimuth::ContextKey::new("baggage"), baggage_with_request)
  
  // Propagate baggage
  let carrier = azimuth::TextMapCarrier::new()
  azimuth::W3CBaggagePropagator::inject(baggage_propagator, gateway_ctx, carrier)
  
  // Service A receives baggage and adds to it
  let service_a_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(service_a_carrier, "baggage", azimuth::TextMapCarrier::get(carrier, "baggage").unwrap_or(""))
  
  let service_a_ctx = azimuth::W3CBaggagePropagator::extract(baggage_propagator, service_a_carrier)
  let service_a_baggage = azimuth::Context::get(service_a_ctx, azimuth::ContextKey::new("baggage")).unwrap_or(azimuth::Baggage::new())
  
  // Add service-specific baggage entry
  let service_a_enhanced_baggage = azimuth::Baggage::set_entry(service_a_baggage, "service.a.version", "1.2.3")
  let service_a_enhanced_baggage_with_timestamp = azimuth::Baggage::set_entry(service_a_enhanced_baggage, "service.a.timestamp", "1640995200000")
  
  let service_a_span = azimuth::Tracer::start_span(service_a_tracer, "service-a.process")
  let service_a_enhanced_ctx = azimuth::Context::with_value(service_a_ctx, azimuth::ContextKey::new("baggage"), service_a_enhanced_baggage_with_timestamp)
  
  // Propagate enhanced baggage
  let service_a_carrier_out = azimuth::TextMapCarrier::new()
  azimuth::W3CBaggagePropagator::inject(baggage_propagator, service_a_enhanced_ctx, service_a_carrier_out)
  
  // Service B receives enhanced baggage
  let service_b_carrier = azimuth::TextMapCarrier::new()
  azimuth::TextMapCarrier::set(service_b_carrier, "baggage", azimuth::TextMapCarrier::get(service_a_carrier_out, "baggage").unwrap_or(""))
  
  let service_b_ctx = azimuth::W3CBaggagePropagator::extract(baggage_propagator, service_b_carrier)
  let service_b_baggage = azimuth::Context::get(service_b_ctx, azimuth::ContextKey::new("baggage")).unwrap_or(azimuth::Baggage::new())
  
  // Add service B specific baggage entry
  let service_b_enhanced_baggage = azimuth::Baggage::set_entry(service_b_baggage, "service.b.version", "2.1.0")
  let service_b_enhanced_baggage_with_result = azimuth::Baggage::set_entry(service_b_enhanced_baggage, "service.b.result", "success")
  
  let service_b_span = azimuth::Tracer::start_span(service_b_tracer, "service-b.process")
  
  // Verify baggage propagation
  let user_id = azimuth::Baggage::get_entry(service_b_enhanced_baggage_with_result, "user.id")
  let session_id = azimuth::Baggage::get_entry(service_b_enhanced_baggage_with_result, "session.id")
  let request_id = azimuth::Baggage::get_entry(service_b_enhanced_baggage_with_result, "request.id")
  let service_a_version = azimuth::Baggage::get_entry(service_b_enhanced_baggage_with_result, "service.a.version")
  let service_a_timestamp = azimuth::Baggage::get_entry(service_b_enhanced_baggage_with_result, "service.a.timestamp")
  let service_b_version = azimuth::Baggage::get_entry(service_b_enhanced_baggage_with_result, "service.b.version")
  let service_b_result = azimuth::Baggage::get_entry(service_b_enhanced_baggage_with_result, "service.b.result")
  
  // Verify all baggage entries are present
  assert_eq(user_id, Some("user-123"))
  assert_eq(session_id, Some("session-456"))
  assert_eq(request_id, Some("req-789"))
  assert_eq(service_a_version, Some("1.2.3"))
  assert_eq(service_a_timestamp, Some("1640995200000"))
  assert_eq(service_b_version, Some("2.1.0"))
  assert_eq(service_b_result, Some("success"))
  
  // End spans
  azimuth::Span::end(service_b_span)
  azimuth::Span::end(service_a_span)
  azimuth::Span::end(gateway_span)
  
  // Test baggage serialization format
  let serialized_baggage = azimuth::Baggage::serialize(service_b_enhanced_baggage_with_result)
  assert_true(serialized_baggage.contains("user.id=user-123"))
  assert_true(serialized_baggage.contains("session.id=session-456"))
  assert_true(serialized_baggage.contains("request.id=req-789"))
  assert_true(serialized_baggage.contains("service.a.version=1.2.3"))
  assert_true(serialized_baggage.contains("service.b.version=2.1.0"))
  
  // Test baggage deserialization
  let deserialized_baggage = azimuth::Baggage::deserialize(serialized_baggage)
  let deserialized_user_id = azimuth::Baggage::get_entry(deserialized_baggage, "user.id")
  let deserialized_service_a_version = azimuth::Baggage::get_entry(deserialized_baggage, "service.a.version")
  
  assert_eq(deserialized_user_id, Some("user-123"))
  assert_eq(deserialized_service_a_version, Some("1.2.3"))
}

// Test 5: Cross-service resource attribute merging
pub test "cross-service resource attribute merging" {
  // Create resources for different services
  let gateway_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("gateway-service")),
    ("service.version", azimuth::StringValue("1.0.0")),
    ("service.instance.id", azimuth::StringValue("gateway-1")),
    ("deployment.environment", azimuth::StringValue("production"))
  ])
  
  let user_service_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("user-service")),
    ("service.version", azimuth::StringValue("2.1.0")),
    ("service.instance.id", azimuth::StringValue("user-service-1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("database.type", azimuth::StringValue("postgresql"))
  ])
  
  let order_service_resource = azimuth::Resource::with_attributes(azimuth::Resource::new(), [
    ("service.name", azimuth::StringValue("order-service")),
    ("service.version", azimuth::StringValue("3.2.1")),
    ("service.instance.id", azimuth::StringValue("order-service-1")),
    ("deployment.environment", azimuth::StringValue("production")),
    ("database.type", azimuth::StringValue("mongodb")),
    ("cache.type", azimuth::StringValue("redis"))
  ])
  
  // Test resource merging strategy
  let merge_strategy = azimuth::ResourceMergeStrategy::new()
    .with_conflict_resolution("service_specific")  # Service-specific attributes take precedence
    .with_common_attributes(["deployment.environment", "region"])  # Keep common attributes
    .with_service_prefix("service.")  # Prefix service-specific attributes
  
  // Merge all service resources
  let merged_resource = azimuth::ResourceMergeStrategy::merge(merge_strategy, [
    gateway_resource,
    user_service_resource,
    order_service_resource
  ])
  
  // Verify merged resource attributes
  let merged_attrs = azimuth::Resource::get_all_attributes(merged_resource)
  
  // Check for common attributes
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "deployment.environment"), Some(azimuth::StringValue("production")))
  
  // Check for service-specific attributes with prefixes
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.gateway.name"), Some(azimuth::StringValue("gateway-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.gateway.version"), Some(azimuth::StringValue("1.0.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.gateway.instance.id"), Some(azimuth::StringValue("gateway-1")))
  
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.user.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.user.version"), Some(azimuth::StringValue("2.1.0")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.user.instance.id"), Some(azimuth::StringValue("user-service-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.user.database.type"), Some(azimuth::StringValue("postgresql")))
  
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.order.name"), Some(azimuth::StringValue("order-service")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.order.version"), Some(azimuth::StringValue("3.2.1")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.order.instance.id"), Some(azimuth::StringValue("order-service-1")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.order.database.type"), Some(azimuth::StringValue("mongodb")))
  assert_eq(azimuth::Resource::get_attribute(merged_resource, "service.order.cache.type"), Some(azimuth::StringValue("redis")))
  
  // Test resource attribute filtering
  let filtered_resource = azimuth::ResourceMergeStrategy::filter_by_prefix(merged_resource, "service.user.")
  
  assert_eq(azimuth::Resource::get_attribute(filtered_resource, "service.user.name"), Some(azimuth::StringValue("user-service")))
  assert_eq(azimuth::Resource::get_attribute(filtered_resource, "service.user.version"), Some(azimuth::StringValue("2.1.0")))
  assert_eq(azimuth::Resource::get_attribute(filtered_resource, "deployment.environment"), None)  # Should be filtered out
  
  // Test resource attribute export
  let exported_json = azimuth::Resource::export_json(merged_resource)
  assert_true(exported_json.contains("gateway-service"))
  assert_true(exported_json.contains("user-service"))
  assert_true(exported_json.contains("order-service"))
  assert_true(exported_json.contains("production"))
  
  let exported_yaml = azimuth::Resource::export_yaml(merged_resource)
  assert_true(exported_yaml.contains("gateway-service"))
  assert_true(exported_yaml.contains("user-service"))
  assert_true(exported_yaml.contains("order-service"))
  assert_true(exported_yaml.contains("production"))
}