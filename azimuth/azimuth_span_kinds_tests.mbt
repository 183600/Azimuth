// Span Kinds Test Suite for Azimuth Telemetry System
// This file contains test cases for different span kinds in distributed tracing

test "span kind internal operations" {
  // Test Internal span kind for application internal operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "internal.operations")
  
  // Create internal span
  let span_ctx = SpanContext::new("trace-12345", "span-67890", true, "key1=value1")
  let internal_span = Span::new("internal.processing", Internal, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(internal_span), "internal.processing")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  assert_eq(SpanContext::trace_id(Span::span_context(internal_span)), "trace-12345")
  assert_eq(SpanContext::span_id(Span::span_context(internal_span)), "span-67890")
  
  // Add events to internal span
  Span::add_event(internal_span, "processing.started", Some([("step", StringValue("1")), ("operation", StringValue("data.validation"))]))
  Span::add_event(internal_span, "processing.completed", Some([("step", StringValue("2")), ("records.processed", IntValue(100))]))
  
  // Set status and end span
  Span::set_status(internal_span, Ok, Some("Internal processing completed successfully"))
  Span::end(internal_span)
}

test "span kind server operations" {
  // Test Server span kind for handling incoming requests
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "server.operations")
  
  // Create server span
  let span_ctx = SpanContext::new("trace-server-001", "span-server-001", true, "service=api")
  let server_span = Span::new("http.request.handler", Server, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(server_span), "http.request.handler")
  assert_eq(Span::kind(server_span), Server)
  assert_true(Span::is_recording(server_span))
  
  // Add server-specific events
  Span::add_event(server_span, "request.received", Some([
    ("http.method", StringValue("GET")),
    ("http.url", StringValue("/api/users")),
    ("user.agent", StringValue("Mozilla/5.0..."))
  ]))
  
  Span::add_event(server_span, "authentication.completed", Some([
    ("user.id", StringValue("user-12345")),
    ("auth.method", StringValue("jwt"))
  ]))
  
  // Simulate processing
  Span::add_event(server_span, "database.query.executed", Some([
    ("query.type", StringValue("SELECT")),
    ("table", StringValue("users")),
    ("duration", StringValue("25ms"))
  ]))
  
  // Complete request
  Span::add_event(server_span, "response.sent", Some([
    ("http.status_code", IntValue(200)),
    ("response.size", IntValue(1024))
  ]))
  
  Span::set_status(server_span, Ok, Some("Request processed successfully"))
  Span::end(server_span)
}

test "span kind client operations" {
  // Test Client span kind for outgoing requests
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "client.operations")
  
  // Create client span
  let span_ctx = SpanContext::new("trace-client-001", "span-client-001", true, "service=web-app")
  let client_span = Span::new("http.client.request", Client, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(client_span), "http.client.request")
  assert_eq(Span::kind(client_span), Client)
  assert_true(Span::is_recording(client_span))
  
  // Add client-specific events
  Span::add_event(client_span, "request.started", Some([
    ("http.method", StringValue("POST")),
    ("http.url", StringValue("https://api.payment.com/charge")),
    ("service.target", StringValue("payment.service"))
  ]))
  
  Span::add_event(client_span, "connection.established", Some([
    ("remote.address", StringValue("10.0.1.100:443")),
    ("connection.time", StringValue("15ms"))
  ]))
  
  // Simulate response
  Span::add_event(client_span, "response.received", Some([
    ("http.status_code", IntValue(201)),
    ("response.time", StringValue("150ms")),
    ("response.size", IntValue(512))
  ]))
  
  Span::set_status(client_span, Ok, Some("Payment processed successfully"))
  Span::end(client_span)
}

test "span kind producer operations" {
  // Test Producer span kind for message publishing
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "producer.operations")
  
  // Create producer span
  let span_ctx = SpanContext::new("trace-producer-001", "span-producer-001", true, "service=order.processor")
  let producer_span = Span::new("message.publish", Producer, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(producer_span), "message.publish")
  assert_eq(Span::kind(producer_span), Producer)
  assert_true(Span::is_recording(producer_span))
  
  // Add producer-specific events
  Span::add_event(producer_span, "message.created", Some([
    ("message.id", StringValue("msg-12345")),
    ("message.type", StringValue("order.created")),
    ("message.size", IntValue(2048))
  ]))
  
  Span::add_event(producer_span, "message.serialized", Some([
    ("serialization.format", StringValue("json")),
    ("serialization.time", StringValue("5ms"))
  ]))
  
  Span::add_event(producer_span, "message.published", Some([
    ("messaging.system", StringValue("kafka")),
    ("messaging.destination", StringValue("orders.topic")),
    ("messaging.message_id", StringValue("msg-12345"))
  ]))
  
  Span::set_status(producer_span, Ok, Some("Message published successfully"))
  Span::end(producer_span)
}

test "span kind consumer operations" {
  // Test Consumer span kind for message processing
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "consumer.operations")
  
  // Create consumer span
  let span_ctx = SpanContext::new("trace-consumer-001", "span-consumer-001", true, "service=email.sender")
  let consumer_span = Span::new("message.process", Consumer, span_ctx)
  
  // Verify span properties
  assert_eq(Span::name(consumer_span), "message.process")
  assert_eq(Span::kind(consumer_span), Consumer)
  assert_true(Span::is_recording(consumer_span))
  
  // Add consumer-specific events
  Span::add_event(consumer_span, "message.received", Some([
    ("messaging.system", StringValue("kafka")),
    ("messaging.destination", StringValue("orders.topic")),
    ("messaging.message_id", StringValue("msg-12345"))
  ]))
  
  Span::add_event(consumer_span, "message.deserialized", Some([
    ("deserialization.format", StringValue("json")),
    ("deserialization.time", StringValue("3ms"))
  ]))
  
  Span::add_event(consumer_span, "message.processed", Some([
    ("processing.time", StringValue("200ms")),
    ("order.id", StringValue("order-67890")),
    ("customer.id", StringValue("customer-11111"))
  ]))
  
  Span::add_event(consumer_span, "email.sent", Some([
    ("email.provider", StringValue("sendgrid")),
    ("email.address", StringValue("customer@example.com")),
    ("email.template", StringValue("order.confirmation"))
  ]))
  
  Span::set_status(consumer_span, Ok, Some("Message processed and email sent"))
  Span::end(consumer_span)
}

test "span kind mixed workflow" {
  // Test workflow with multiple span kinds
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "mixed.workflow")
  
  // Server span handles incoming request
  let server_span_ctx = SpanContext::new("trace-workflow-001", "span-server-001", true, "service=api.gateway")
  let server_span = Span::new("api.request", Server, server_span_ctx)
  Span::add_event(server_span, "request.received", Some([("endpoint", StringValue("/api/process"))]))
  
  // Internal span processes data
  let internal_span_ctx = SpanContext::new("trace-workflow-001", "span-internal-001", true, "service=api.gateway")
  let internal_span = Span::new("data.validation", Internal, internal_span_ctx)
  Span::add_event(internal_span, "validation.started", Some([("records.count", IntValue(50))]))
  Span::add_event(internal_span, "validation.completed", Some([("valid.records", IntValue(48)), ("invalid.records", IntValue(2))]))
  Span::end(internal_span)
  
  // Client span calls external service
  let client_span_ctx = SpanContext::new("trace-workflow-001", "span-client-001", true, "service=api.gateway")
  let client_span = Span::new("external.api.call", Client, client_span_ctx)
  Span::add_event(client_span, "api.call.started", Some([("target.service", StringValue("payment.api"))]))
  Span::add_event(client_span, "api.call.completed", Some([("response.time", StringValue("300ms"))]))
  Span::end(client_span)
  
  // Producer span publishes result
  let producer_span_ctx = SpanContext::new("trace-workflow-001", "span-producer-001", true, "service=api.gateway")
  let producer_span = Span::new("result.publish", Producer, producer_span_ctx)
  Span::add_event(producer_span, "message.published", Some([("topic", StringValue("results.topic"))]))
  Span::end(producer_span)
  
  // Complete server span
  Span::add_event(server_span, "request.completed", Some([("total.time", StringValue("650ms"))]))
  Span::set_status(server_span, Ok, Some("Request processed successfully"))
  Span::end(server_span)
  
  // Verify all spans have correct kinds
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
}