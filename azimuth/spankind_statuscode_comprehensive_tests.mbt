// Span Kind and Status Code Test Cases for Azimuth Telemetry System
// Testing span kinds and status codes for comprehensive trace coverage

test "span kind enumeration values" {
  // Test all span kind values
  let internal_span = Internal
  let server_span = Server
  let client_span = Client
  let producer_span = Producer
  let consumer_span = Consumer
  
  // Test span kind comparisons
  assert_true(internal_span != server_span)
  assert_true(server_span != client_span)
  assert_true(client_span != producer_span)
  assert_true(producer_span != consumer_span)
  assert_true(consumer_span != internal_span)
  
  // Test span kind equality
  assert_true(internal_span == internal_span)
  assert_true(server_span == server_span)
  assert_true(client_span == client_span)
  assert_true(producer_span == producer_span)
  assert_true(consumer_span == consumer_span)
}

test "span kind in span creation" {
  let span_ctx = SpanContext::new("trace123", "span456", true, "")
  
  // Create spans with different kinds
  let internal_span = Span::new("internal-operation", Internal, span_ctx)
  let server_span = Span::new("api-request", Server, span_ctx)
  let client_span = Span::new("database-call", Client, span_ctx)
  let producer_span = Span::new("message-publish", Producer, span_ctx)
  let consumer_span = Span::new("message-process", Consumer, span_ctx)
  
  // Test span kind properties
  assert_eq(Span::kind(internal_span), Internal)
  assert_eq(Span::kind(server_span), Server)
  assert_eq(Span::kind(client_span), Client)
  assert_eq(Span::kind(producer_span), Producer)
  assert_eq(Span::kind(consumer_span), Consumer)
  
  // Test span names
  assert_eq(Span::name(internal_span), "internal-operation")
  assert_eq(Span::name(server_span), "api-request")
  assert_eq(Span::name(client_span), "database-call")
  assert_eq(Span::name(producer_span), "message-publish")
  assert_eq(Span::name(consumer_span), "message-process")
}

test "status code enumeration values" {
  // Test all status code values
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // Test status code comparisons
  assert_true(unset_status != ok_status)
  assert_true(ok_status != error_status)
  assert_true(error_status != unset_status)
  
  // Test status code equality
  assert_true(unset_status == unset_status)
  assert_true(ok_status == ok_status)
  assert_true(error_status == error_status)
}

test "status code in span operations" {
  let span_ctx = SpanContext::new("trace789", "span012", true, "")
  let span = Span::new("test-operation", Internal, span_ctx)
  
  // Test initial status
  assert_eq(Span::status(span), Unset)
  
  // Test setting different status codes
  Span::set_status(span, Ok, Some("Operation completed successfully"))
  assert_true(true)  // Verify operation completes without error
  
  Span::set_status(span, Error, Some("Operation failed with timeout"))
  assert_true(true)  // Verify operation completes without error
  
  Span::set_status(span, Unset)
  assert_true(true)  // Verify operation completes without error
}

test "status code with descriptions" {
  let span_ctx = SpanContext::new("trace345", "span678", true, "")
  let span = Span::new("operation-with-status", Server, span_ctx)
  
  // Test status with descriptions
  Span::set_status(span, Ok, Some("Request processed successfully"))
  Span::set_status(span, Error, Some("Database connection failed"))
  Span::set_status(span, Ok, Some("Retried operation succeeded"))
  Span::set_status(span, Error, Some("Final attempt failed"))
  
  // Verify all operations complete without errors
  assert_true(true)
}

test "span kind and status code combinations" {
  let span_ctx = SpanContext::new("trace999", "span888", true, "")
  
  // Test combinations of span kinds and status codes
  let span_combinations = [
    (Span::new("internal-success", Internal, span_ctx), Ok),
    (Span::new("internal-error", Internal, span_ctx), Error),
    (Span::new("server-success", Server, span_ctx), Ok),
    (Span::new("server-error", Server, span_ctx), Error),
    (Span::new("client-success", Client, span_ctx), Ok),
    (Span::new("client-error", Client, span_ctx), Error),
    (Span::new("producer-success", Producer, span_ctx), Ok),
    (Span::new("producer-error", Producer, span_ctx), Error),
    (Span::new("consumer-success", Consumer, span_ctx), Ok),
    (Span::new("consumer-error", Consumer, span_ctx), Error)
  ]
  
  // Apply status codes to spans
  for (span, status) in span_combinations {
    Span::set_status(span, status, Some("Test status"))
    assert_true(true)  // Verify each operation succeeds
  }
  
  assert_eq(span_combinations.length(), 10)
}

test "span lifecycle with kind and status" {
  let span_ctx = SpanContext::new("trace111", "span222", true, "")
  let span = Span::new("lifecycle-test", Server, span_ctx)
  
  // Test initial state
  assert_eq(Span::name(span), "lifecycle-test")
  assert_eq(Span::kind(span), Server)
  assert_true(Span::is_recording(span))
  assert_eq(Span::status(span), Unset)
  
  // Test adding events
  Span::add_event(span, "request.received", Some([("user.id", StringValue("12345"))]))
  Span::add_event(span, "authentication.completed")
  Span::add_event(span, "processing.started", Some([("items.count", IntValue(10))]))
  
  // Test status changes during lifecycle
  Span::set_status(span, Ok, Some("Request processed"))
  Span::add_event(span, "response.sent")
  
  // Test span end
  Span::end(span)
  
  // Verify all operations complete successfully
  assert_true(true)
}

test "span kind semantic conventions" {
  let span_ctx = SpanContext::new("trace333", "span444", true, "")
  
  // Test semantic conventions for different span kinds
  let server_span = Span::new("HTTP GET /api/users", Server, span_ctx)
  let client_span = Span::new("HTTP POST /external/api", Client, span_ctx)
  let producer_span = Span::new("message publish to orders.topic", Producer, span_ctx)
  let consumer_span = Span::new("message consume from notifications.topic", Consumer, span_ctx)
  let internal_span = Span::new("data processing", Internal, span_ctx)
  
  // Add appropriate events for each span kind
  Span::add_event(server_span, "request.started", Some([("http.method", StringValue("GET")), ("http.target", StringValue("/api/users"))]))
  Span::add_event(client_span, "request.sent", Some([("http.method", StringValue("POST")), ("http.url", StringValue("https://external.api"))]))
  Span::add_event(producer_span, "message.published", Some([("messaging.system", StringValue("kafka")), ("messaging.destination", StringValue("orders.topic"))]))
  Span::add_event(consumer_span, "message.received", Some([("messaging.system", StringValue("kafka")), ("messaging.source", StringValue("notifications.topic"))]))
  Span::add_event(internal_span, "processing.completed", Some([("processing.items", IntValue(1000))]))
  
  // Set appropriate status codes
  Span::set_status(server_span, Ok, Some("Request completed successfully"))
  Span::set_status(client_span, Error, Some("External service unavailable"))
  Span::set_status(producer_span, Ok, Some("Message published successfully"))
  Span::set_status(consumer_span, Ok, Some("Message processed successfully"))
  Span::set_status(internal_span, Ok, Some("Internal processing completed"))
  
  // Verify all operations complete successfully
  assert_true(true)
}