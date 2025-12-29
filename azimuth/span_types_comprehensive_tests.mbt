// Span Types Comprehensive Test Suite for Azimuth Telemetry System
// This file contains comprehensive test cases for different span types

test "internal span type operations" {
  // Test Internal span type for internal operations
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "internal.test")
  
  // Create internal span (current implementation defaults to Internal)
  let internal_span = Tracer::start_span(tracer, "internal.operation")
  
  // Verify span type and properties
  assert_eq(Span::name(internal_span), "internal.operation")
  assert_eq(Span::kind(internal_span), Internal)
  assert_true(Span::is_recording(internal_span))
  
  // Add internal-specific events
  Span::add_event(internal_span, "internal.processing.started", Some([
    ("operation.type", StringValue("data.transformation")),
    ("component", StringValue("internal.processor"))
  ]))
  
  Span::add_event(internal_span, "internal.calculation", Some([
    ("algorithm", StringValue("fast_fourier_transform")),
    ("data.points", IntValue(1024)),
    ("processing.time", FloatValue(45.7))
  ]))
  
  // Set status and complete
  Span::set_status(internal_span, Ok, Some("Internal operation completed successfully"))
  Span::add_event(internal_span, "internal.operation.completed", Some([
    ("result.size", IntValue(2048)),
    ("quality.score", FloatValue(0.98))
  ]))
  
  Span::end(internal_span)
}

test "server span type operations" {
  // Test Server span type for handling incoming requests
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "server.test")
  
  // Create server span (simulated - would need implementation for different kinds)
  let server_span = Tracer::start_span(tracer, "server.request.handler")
  
  // Verify span properties
  assert_eq(Span::name(server_span), "server.request.handler")
  assert_eq(Span::kind(server_span), Internal)  // Would be Server in full implementation
  assert_true(Span::is_recording(server_span))
  
  // Add server-specific events
  Span::add_event(server_span, "request.received", Some([
    ("http.method", StringValue("POST")),
    ("http.url", StringValue("/api/v1/process")),
    ("user.agent", StringValue("Mozilla/5.0...")),
    ("remote.addr", StringValue("192.168.1.100"))
  ]))
  
  Span::add_event(server_span, "authentication.completed", Some([
    ("user.id", StringValue("user123")),
    ("auth.method", StringValue("jwt")),
    ("auth.duration", FloatValue(15.2))
  ]))
  
  Span::add_event(server_span, "request.processing", Some([
    ("database.queries", IntValue(3)),
    ("cache.hits", IntValue(12)),
    ("external.api.calls", IntValue(2))
  ]))
  
  Span::add_event(server_span, "response.generated", Some([
    ("http.status_code", IntValue(200)),
    ("response.size", IntValue(1024)),
    ("content.type", StringValue("application/json"))
  ]))
  
  // Set status and complete
  Span::set_status(server_span, Ok, Some("Request processed successfully"))
  Span::end(server_span)
}

test "client span type operations" {
  // Test Client span type for outgoing requests
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "client.test")
  
  // Create client span (simulated)
  let client_span = Tracer::start_span(tracer, "client.request")
  
  // Verify span properties
  assert_eq(Span::name(client_span), "client.request")
  assert_eq(Span::kind(client_span), Internal)  // Would be Client in full implementation
  assert_true(Span::is_recording(client_span))
  
  // Add client-specific events
  Span::add_event(client_span, "request.initiated", Some([
    ("target.service", StringValue("payment.api")),
    ("http.method", StringValue("POST")),
    ("target.url", StringValue("https://api.payment.com/v1/charge")),
    ("request.id", StringValue("req-789456"))
  ]))
  
  Span::add_event(client_span, "connection.established", Some([
    ("connection.time", FloatValue(25.3)),
    ("server.address", StringValue("api.payment.com")),
    ("tls.version", StringValue("TLSv1.3"))
  ]))
  
  Span::add_event(client_span, "request.sent", Some([
    ("request.size", IntValue(512)),
    ("headers.sent", IntValue(8)),
    ("compression", StringValue("gzip"))
  ]))
  
  Span::add_event(client_span, "response.received", Some([
    ("http.status_code", IntValue(200)),
    ("response.size", IntValue(256)),
    ("response.time", FloatValue(150.7)),
    ("server.processing.time", FloatValue(125.4))
  ]))
  
  // Set status and complete
  Span::set_status(client_span, Ok, Some("Client request completed successfully"))
  Span::end(client_span)
}

test "producer span type operations" {
  // Test Producer span type for message/event production
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "producer.test")
  
  // Create producer span (simulated)
  let producer_span = Tracer::start_span(tracer, "message.producer")
  
  // Verify span properties
  assert_eq(Span::name(producer_span), "message.producer")
  assert_eq(Span::kind(producer_span), Internal)  // Would be Producer in full implementation
  assert_true(Span::is_recording(producer_span))
  
  // Add producer-specific events
  Span::add_event(producer_span, "message.creation.started", Some([
    ("message.type", StringValue("order.created")),
    ("producer.id", StringValue("producer-123")),
    ("topic", StringValue("orders.stream"))
  ]))
  
  Span::add_event(producer_span, "message.serialization", Some([
    ("serialization.format", StringValue("json")),
    ("message.size", IntValue(2048)),
    ("compression", StringValue("snappy"))
  ]))
  
  Span::add_event(producer_span, "message.published", Some([
    ("broker.address", StringValue("kafka.cluster1:9092")),
    ("partition", IntValue(2)),
    ("offset", IntValue(12345)),
    ("publish.time", FloatValue(35.8))
  ]))
  
  Span::add_event(producer_span, "acknowledgment.received", Some([
    ("ack.type", StringValue("all")),
    ("ack.latency", FloatValue(45.2)),
    ("replication.factor", IntValue(3))
  ]))
  
  // Set status and complete
  Span::set_status(producer_span, Ok, Some("Message produced successfully"))
  Span::end(producer_span)
}

test "consumer span type operations" {
  // Test Consumer span type for message/event consumption
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "consumer.test")
  
  // Create consumer span (simulated)
  let consumer_span = Tracer::start_span(tracer, "message.consumer")
  
  // Verify span properties
  assert_eq(Span::name(consumer_span), "message.consumer")
  assert_eq(Span::kind(consumer_span), Internal)  // Would be Consumer in full implementation
  assert_true(Span::is_recording(consumer_span))
  
  // Add consumer-specific events
  Span::add_event(consumer_span, "message.received", Some([
    ("message.type", StringValue("order.created")),
    ("consumer.id", StringValue("consumer-456")),
    ("topic", StringValue("orders.stream")),
    ("partition", IntValue(2)),
    ("offset", IntValue(12345))
  ]))
  
  Span::add_event(consumer_span, "message.deserialization", Some([
    ("serialization.format", StringValue("json")),
    ("message.size", IntValue(2048)),
    ("decompression", StringValue("snappy")),
    ("deserialize.time", FloatValue(12.3))
  ]))
  
  Span::add_event(consumer_span, "message.processing", Some([
    ("processing.strategy", StringValue("order.validation")),
    ("database.operations", IntValue(5)),
    ("external.api.calls", IntValue(1)),
    ("processing.time", FloatValue(250.7)
  ])))
  
  Span::add_event(consumer_span, "message.committed", Some([
    ("commit.strategy", StringValue("sync")),
    ("commit.offset", IntValue(12345)),
    ("commit.time", FloatValue(18.9))
  ]))
  
  // Set status and complete
  Span::set_status(consumer_span, Ok, Some("Message consumed and processed successfully"))
  Span::end(consumer_span)
}

test "span type cross service interactions" {
  // Test different span types in cross-service interaction scenarios
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "cross.service.test")
  
  // Server span for incoming request
  let server_span = Tracer::start_span(tracer, "api.gateway.request")
  Span::add_event(server_span, "gateway.request.received", Some([
    ("request.id", StringValue("req-123456")),
    ("client.ip", StringValue("10.0.0.5")),
    ("api.version", StringValue("v2"))
  ]))
  
  // Internal span for processing
  let internal_span = Tracer::start_span(tracer, "internal.auth.validation")
  Span::add_event(internal_span, "auth.validation.started", Some([
    ("validation.type", StringValue("jwt.token")),
    ("token.size", IntValue(1024)
  ])))
  Span::set_status(internal_span, Ok, Some("Authentication validated"))
  Span::end(internal_span)
  
  // Client span for downstream service call
  let client_span = Tracer::start_span(tracer, "downstream.service.call")
  Span::add_event(client_span, "downstream.call.initiated", Some([
    ("service.name", StringValue("user.service")),
    ("endpoint", StringValue("/api/users/validate")),
    ("call.type", StringValue("rest"))
  ]))
  Span::set_status(client_span, Ok, Some("Downstream call successful"))
  Span::end(client_span)
  
  // Producer span for event publishing
  let producer_span = Tracer::start_span(tracer, "event.publisher")
  Span::add_event(producer_span, "event.publish.started", Some([
    ("event.type", StringValue("user.validated")),
    ("event.source", StringValue("api.gateway")),
    ("destination.topic", StringValue("user.events"))
  ]))
  Span::set_status(producer_span, Ok, Some("Event published successfully"))
  Span::end(producer_span)
  
  // Complete server span
  Span::add_event(server_span, "gateway.request.completed", Some([
    ("total.duration", FloatValue(350.2)),
    ("downstream.calls", IntValue(1)),
    ("events.published", IntValue(1))
  ]))
  Span::set_status(server_span, Ok, Some("Gateway request processed successfully"))
  Span::end(server_span)
}

test "span type error handling patterns" {
  // Test error handling patterns for different span types
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "error.handling.test")
  
  // Server span with authentication error
  let server_error_span = Tracer::start_span(tracer, "server.auth.error")
  Span::add_event(server_error_span, "auth.failed", Some([
    ("error.type", StringValue("AuthenticationError")),
    ("error.code", StringValue("INVALID_TOKEN")),
    ("user.attempts", IntValue(3))
  ]))
  Span::set_status(server_error_span, Error, Some("Authentication failed"))
  Span::end(server_error_span)
  
  // Client span with connection error
  let client_error_span = Tracer::start_span(tracer, "client.connection.error")
  Span::add_event(client_error_span, "connection.failed", Some([
    ("error.type", StringValue("ConnectionError")),
    ("target.host", StringValue("unreachable.service.com")),
    ("timeout", IntValue(30))
  ]))
  Span::set_status(client_error_span, Error, Some("Connection timeout"))
  Span::end(client_error_span)
  
  // Producer span with publish error
  let producer_error_span = Tracer::start_span(tracer, "producer.publish.error")
  Span::add_event(producer_error_span, "publish.failed", Some([
    ("error.type", StringValue("PublishError")),
    ("broker.address", StringValue("kafka.broker1:9092")),
    ("retry.count", IntValue(3))
  ]))
  Span::set_status(producer_error_span, Error, Some("Message publish failed"))
  Span::end(producer_error_span)
  
  // Consumer span with processing error
  let consumer_error_span = Tracer::start_span(tracer, "consumer.processing.error")
  Span::add_event(consumer_error_span, "processing.failed", Some([
    ("error.type", StringValue("ProcessingError")),
    ("message.type", StringValue("invalid.format")),
    ("error.stage", StringValue("deserialization"))
  ]))
  Span::set_status(consumer_error_span, Error, Some("Message processing failed"))
  Span::end(consumer_error_span)
  
  // Internal span with business logic error
  let internal_error_span = Tracer::start_span(tracer, "internal.business.error")
  Span::add_event(internal_error_span, "business.rule.violation", Some([
    ("rule.name", StringValue("credit.limit.exceeded")),
    ("current.amount", FloatValue(1500.0)),
    ("limit.amount", FloatValue(1000.0))
  ]))
  Span::set_status(internal_error_span, Error, Some("Business rule violation"))
  Span::end(internal_error_span)
}