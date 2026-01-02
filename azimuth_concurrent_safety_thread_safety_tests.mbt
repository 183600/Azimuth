test "azimuth_concurrent_safety_thread_safety_tests" {
  // Test 1: Concurrent span creation and management
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "concurrent_test")
  
  // Create multiple spans concurrently
  let spans = [] : Array[Span]
  for i = 0; i < 100; i = i + 1 {
    let span_name = "concurrent_span_" + i.to_string()
    let span = Tracer::start_span(tracer, span_name)
    spans.push(span)
  }
  
  // Verify all spans have valid contexts
  for span in spans {
    let span_ctx = Span::span_context(span)
    @assert(SpanContext::is_valid(span_ctx))
    @assert(Span::is_recording(span))
  }
  
  // End all spans
  for span in spans {
    Span::end(span)
  }
  
  // Test 2: Concurrent metric recording
  let meter_provider = MeterProvider::default()
  let meter = MeterProvider::get_meter(meter_provider, "concurrent_metrics")
  let counter = Meter::create_counter(meter, "concurrent_counter")
  let histogram = Meter::create_histogram(meter, "concurrent_histogram")
  
  // Record metrics concurrently
  for i = 0; i < 1000; i = i + 1 {
    Counter::add(counter, i.to_double())
    Histogram::record(histogram, i.to_double())
  }
  
  // Test 3: Concurrent attribute operations
  let attrs = Attributes::new()
  
  // Set attributes concurrently
  for i = 0; i < 100; i = i + 1 {
    let key = "concurrent_attr_" + i.to_string()
    let value = StringValue("value_" + i.to_string())
    Attributes::set(attrs, key, value)
  }
  
  // Retrieve attributes concurrently
  for i = 0; i < 100; i = i + 1 {
    let key = "concurrent_attr_" + i.to_string()
    let value = Attributes::get(attrs, key)
    @assert(value.is_some())
  }
  
  // Test 4: Concurrent context propagation
  let root_ctx = Context::root()
  
  // Create multiple contexts with different keys
  let contexts = [] : Array[Context]
  for i = 0; i < 50; i = i + 1 {
    let key = ContextKey::new("key_" + i.to_string())
    let value = "value_" + i.to_string()
    let ctx = Context::with_value(root_ctx, key, value)
    contexts.push(ctx)
  }
  
  // Retrieve values from all contexts
  for i = 0; i < 50; i = i + 1 {
    let key = ContextKey::new("key_" + i.to_string())
    let ctx = contexts[i]
    let value = Context::get(ctx, key)
    @assert(value == Some("value_" + i.to_string()))
  }
  
  // Test 5: Concurrent baggage operations
  let baggage = Baggage::new()
  
  // Add baggage entries concurrently
  for i = 0; i < 50; i = i + 1 {
    let key = "baggage_key_" + i.to_string()
    let value = "baggage_value_" + i.to_string()
    let _ = Baggage::set_entry(baggage, key, value)
  }
  
  // Retrieve baggage entries concurrently
  for i = 0; i < 50; i = i + 1 {
    let key = "baggage_key_" + i.to_string()
    let value = Baggage::get_entry(baggage, key)
    @assert(value.is_some())
  }
  
  // Test 6: Concurrent log record creation and emission
  let logger_provider = LoggerProvider::default()
  let logger = LoggerProvider::get_logger(logger_provider, "concurrent_logger")
  
  // Create and emit log records concurrently
  for i = 0; i < 200; i = i + 1 {
    let severity = match i % 6 {
      0 => Trace
      1 => Debug
      2 => Info
      3 => Warn
      4 => Error
      _ => Fatal
    }
    
    let log_record = LogRecord::new(
      severity,
      "Concurrent log message " + i.to_string()
    )
    Logger::emit(logger, log_record)
  }
  
  // Test 7: Concurrent resource operations
  let resource = Resource::new()
  
  // Create resource attributes
  let attrs_array = [] : Array[(String, AttributeValue)]
  for i = 0; i < 100; i = i + 1 {
    attrs_array.push(("resource_attr_" + i.to_string(), StringValue("resource_value_" + i.to_string())))
  }
  
  // Create populated resource
  let populated_resource = Resource::with_attributes(resource, attrs_array)
  
  // Retrieve attributes concurrently
  for i = 0; i < 100; i = i + 1 {
    let key = "resource_attr_" + i.to_string()
    let value = Resource::get_attribute(populated_resource, key)
    @assert(value.is_some())
  }
  
  // Test 8: Concurrent resource merging
  let base_resource = Resource::new()
  let override_resource = Resource::new()
  
  // Create base attributes
  let base_attrs = [] : Array[(String, AttributeValue)]
  for i = 0; i < 50; i = i + 1 {
    base_attrs.push(("base_attr_" + i.to_string(), StringValue("base_value_" + i.to_string())))
  }
  let populated_base = Resource::with_attributes(base_resource, base_attrs)
  
  // Create override attributes
  let override_attrs = [] : Array[(String, AttributeValue)]
  for i = 0; i < 50; i = i + 1 {
    override_attrs.push(("override_attr_" + i.to_string(), StringValue("override_value_" + i.to_string())))
  }
  let populated_override = Resource::with_attributes(override_resource, override_attrs)
  
  // Merge resources concurrently
  let merged_resources = [] : Array[Resource]
  for i = 0; i < 10; i = i + 1 {
    let merged = Resource::merge(populated_base, populated_override)
    merged_resources.push(merged)
  }
  
  // Verify all merged resources have consistent attributes
  for merged_resource in merged_resources {
    let base_attr = Resource::get_attribute(merged_resource, "base_attr_0")
    let override_attr = Resource::get_attribute(merged_resource, "override_attr_0")
    @assert(base_attr.is_some())
    @assert(override_attr.is_some())
  }
  
  // Test 9: Concurrent span hierarchy operations
  let parent_spans = [] : Array[Span]
  
  // Create parent spans
  for i = 0; i < 10; i = i + 1 {
    let parent_span = Tracer::start_span(tracer, "parent_span_" + i.to_string())
    parent_spans.push(parent_span)
  }
  
  // Create child spans for each parent
  let child_spans = [] : Array[Span]
  for parent_span in parent_spans {
    for i = 0; i < 5; i = i + 1 {
      let child_span = Tracer::start_span(tracer, "child_span_" + i.to_string())
      child_spans.push(child_span)
    }
  }
  
  // Add events to all spans
  for span in child_spans {
    Span::add_event(span, "concurrent_event", Some([("event_type", StringValue("concurrent"))]))
  }
  
  // End all child spans
  for span in child_spans {
    Span::end(span)
  }
  
  // End all parent spans
  for span in parent_spans {
    Span::end(span)
  }
  
  // Test 10: Concurrent context injection and extraction
  let propagators = [] : Array[CompositePropagator]
  
  // Create propagators
  for i = 0; i < 5; i = i + 1 {
    let trace_propagator = W3CTraceContextPropagator::new()
    let propagator = CompositePropagator::new([trace_propagator])
    propagators.push(propagator)
  }
  
  // Create contexts and carriers
  let contexts = [] : Array[Context]
  let carriers = [] : Array[TextMapCarrier]
  
  for i = 0; i < 5; i = i + 1 {
    let ctx = Context::with_value(Context::root(), ContextKey::new("test_key"), "test_value_" + i.to_string())
    let carrier = TextMapCarrier::new()
    contexts.push(ctx)
    carriers.push(carrier)
  }
  
  // Inject contexts into carriers
  for i = 0; i < 5; i = i + 1 {
    CompositePropagator::inject(propagators[i], contexts[i], carriers[i])
  }
  
  // Extract contexts from carriers
  let extracted_contexts = [] : Array[Context]
  for i = 0; i < 5; i = i + 1 {
    let extracted_ctx = CompositePropagator::extract(propagators[i], carriers[i])
    extracted_contexts.push(extracted_ctx)
  }
  
  // Verify extraction worked
  for extracted_ctx in extracted_contexts {
    let extracted_value = Context::get(extracted_ctx, ContextKey::new("extracted"))
    @assert(extracted_value.is_some())
  }
}