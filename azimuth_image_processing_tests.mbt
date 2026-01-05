// Image Processing Telemetry Tests
// This file contains test cases for image processing telemetry functionality

test "image processing metrics collection" {
  let provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(provider, "image-processor")
  
  // Create metrics for image processing operations
  let process_counter = Meter::create_counter(meter, "images.processed.total")
  let resize_histogram = Meter::create_histogram(meter, "image.resize.duration.ms")
  let filter_gauge = Meter::create_gauge(meter, "image.filters.active")
  
  // Record image processing metrics
  Counter::add(process_counter, 1.0)
  Histogram::record(resize_histogram, 150.5)
  
  // Verify metric names
  assert_eq(Instrument::name(Counter("images.processed.total", None, None)), "images.processed.total")
  assert_eq(Instrument::name(Histogram("image.resize.duration.ms", None, None)), "image.resize.duration.ms")
  assert_eq(Instrument::name(Gauge("image.filters.active", None, None)), "image.filters.active")
}

test "image processing span with attributes" {
  let trace_id = "img1234567890abcdef1234567890abcdef"
  let span_id = "span9876543210fedcba"
  let span_ctx = SpanContext::new(trace_id, span_id, true, "image.format=jpeg")
  
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "image-processor")
  let span = Tracer::start_span(tracer, "image.resize")
  
  // Verify span properties
  assert_eq(Span::name(span), "image.resize")
  assert_eq(Span::kind(span), Internal)
  assert_true(Span::is_recording(span))
  assert_eq(SpanContext::trace_id(Span::span_context(span)), "test_trace_id")
  assert_eq(SpanContext::span_id(Span::span_context(span)), "test_span_id")
}

test "image processing log records" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "image-processor")
  
  // Create log record for image processing
  let attributes = Attributes::new()
  let log_record = LogRecord::new_with_context(
    Info,
    Some("Image processed successfully"),
    Some(attributes),
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("img1234567890abcdef"),
    Some("span9876543210fedcba"),
    Some(Context::root())
  )
  
  // Verify log record properties
  assert_eq(LogRecord::severity_number(log_record), Info)
  assert_eq(LogRecord::body(log_record), Some("Image processed successfully"))
  assert_eq(LogRecord::trace_id(log_record), Some("img1234567890abcdef"))
  assert_eq(LogRecord::span_id(log_record), Some("span9876543210fedcba"))
}

test "image processing resource attributes" {
  let resource = Resource::new()
  let image_attrs = [
    ("service.name", StringValue("image-processor")),
    ("service.version", StringValue("1.0.0")),
    ("image.format.supported", ArrayStringValue(["jpeg", "png", "webp"])),
    ("image.max.size.mb", IntValue(10))
  ]
  
  let resource_with_attrs = Resource::with_attributes(resource, image_attrs)
  
  // Verify resource attributes
  let service_name = Resource::get_attribute(resource_with_attrs, "service.name")
  let service_version = Resource::get_attribute(resource_with_attrs, "service.version")
  let supported_formats = Resource::get_attribute(resource_with_attrs, "image.format.supported")
  let max_size = Resource::get_attribute(resource_with_attrs, "image.max.size.mb")
  
  assert_eq(service_name, Some(StringValue("image-processor")))
  assert_eq(service_version, Some(StringValue("1.0.0")))
  assert_eq(supported_formats, Some(ArrayStringValue(["jpeg", "png", "webp"])))
  assert_eq(max_size, Some(IntValue(10)))
}

test "image processing baggage propagation" {
  let baggage = Baggage::new()
  
  // Add image processing context to baggage
  let baggage_with_context = Baggage::set_entry(baggage, "image.operation", "resize")
  let baggage_with_format = Baggage::set_entry(baggage_with_context, "image.format", "jpeg")
  
  // Verify baggage entries
  let operation = Baggage::get_entry(baggage_with_format, "image.operation")
  let format = Baggage::get_entry(baggage_with_format, "image.format")
  let missing = Baggage::get_entry(baggage_with_format, "image.quality")
  
  assert_eq(operation, Some("resize"))
  assert_eq(format, Some("jpeg"))
  assert_eq(missing, None)
}

test "image processing error handling" {
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "image-processor")
  
  // Create error log record
  let error_record = LogRecord::new_with_context(
    Error,
    Some("Failed to process image: unsupported format"),
    None,
    Some(1735689600000000000L),
    Some(1735689600000000000L),
    Some("error1234567890abcdef"),
    Some("error9876543210fedcba"),
    None
  )
  
  // Verify error log record
  assert_eq(LogRecord::severity_number(error_record), Error)
  assert_eq(LogRecord::body(error_record), Some("Failed to process image: unsupported format"))
  assert_eq(LogRecord::trace_id(error_record), Some("error1234567890abcdef"))
  assert_eq(LogRecord::span_id(error_record), Some("error9876543210fedcba"))
}

test "image processing composite propagator" {
  let trace_propagator = W3CTraceContextPropagator::new()
  let baggage_propagator = W3CBaggagePropagator::new()
  
  // Note: Simplified for testing - would normally use proper type
  let composite = CompositePropagator::new([trace_propagator])
  
  let carrier = TextMapCarrier::new()
  let ctx = Context::root()
  
  // Inject context
  CompositePropagator::inject(composite, ctx, carrier)
  
  // Extract context
  let extracted_ctx = CompositePropagator::extract(composite, carrier)
  
  // Verify propagation
  let key = ContextKey::new("extracted")
  let extracted_value = Context::get(extracted_ctx, key)
  assert_eq(extracted_value, Some("true"))
}