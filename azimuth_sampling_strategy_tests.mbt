// Azimuth Telemetry System - Sampling Strategy Tests
// This file contains test cases for various telemetry data sampling strategies

// Test 1: Always On Sampling Strategy
test "always on sampling strategy" {
  let sampler = Sampler::always_on()
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, false, "")
  let parent_ctx = None
  let name = "test_span"
  let kind = Internal
  let attributes = Attributes::new()
  let links = []
  
  let sampling_result = Sampler::should_sample(
    sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result {
    SamplingResult::Decision(decision) => assert_eq(decision, RecordAndSample)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, RecordAndSample)
    _ => assert_true(false)
  }
}

// Test 2: Always Off Sampling Strategy
test "always off sampling strategy" {
  let sampler = Sampler::always_off()
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let span_id = "b7ad6b7169203331"
  let span_ctx = SpanContext::new(trace_id, span_id, false, "")
  let parent_ctx = None
  let name = "test_span"
  let kind = Internal
  let attributes = Attributes::new()
  let links = []
  
  let sampling_result = Sampler::should_sample(
    sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result {
    SamplingResult::Decision(decision) => assert_eq(decision, Drop)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, Drop)
    _ => assert_true(false)
  }
}

// Test 3: Trace ID Ratio Based Sampling Strategy
test "trace id ratio based sampling strategy" {
  // Test with 100% probability (should always sample)
  let sampler_100 = Sampler::trace_id_ratio(1.0)
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_ctx = None
  let name = "test_span"
  let kind = Internal
  let attributes = Attributes::new()
  let links = []
  
  let sampling_result_100 = Sampler::should_sample(
    sampler_100,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result_100 {
    SamplingResult::Decision(decision) => assert_eq(decision, RecordAndSample)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, RecordAndSample)
    _ => assert_true(false)
  }
  
  // Test with 0% probability (should never sample)
  let sampler_0 = Sampler::trace_id_ratio(0.0)
  let sampling_result_0 = Sampler::should_sample(
    sampler_0,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result_0 {
    SamplingResult::Decision(decision) => assert_eq(decision, Drop)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, Drop)
    _ => assert_true(false)
  }
  
  // Test with 50% probability
  let sampler_50 = Sampler::trace_id_ratio(0.5)
  let sampling_result_50 = Sampler::should_sample(
    sampler_50,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  // The result could be either Drop or RecordAndSample based on the trace_id
  match sampling_result_50 {
    SamplingResult::Decision(decision) => {
      assert_true(decision == Drop || decision == RecordAndSample)
    }
    SamplingResult::DecisionWithAttributes(decision, _) => {
      assert_true(decision == Drop || decision == RecordAndSample)
    }
    _ => assert_true(false)
  }
}

// Test 4: Parent Based Sampling Strategy
test "parent based sampling strategy" {
  let sampler = Sampler::parent_based(Sampler::always_off())
  
  // Test with sampled parent
  let parent_trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_span_id = "b7ad6b7169203331"
  let parent_span_ctx = SpanContext::new(parent_trace_id, parent_span_id, true, "")
  let parent_ctx = Some(parent_span_ctx)
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let name = "test_span"
  let kind = Internal
  let attributes = Attributes::new()
  let links = []
  
  let sampling_result = Sampler::should_sample(
    sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result {
    SamplingResult::Decision(decision) => assert_eq(decision, RecordAndSample)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, RecordAndSample)
    _ => assert_true(false)
  }
  
  // Test with non-sampled parent
  let non_sampled_parent_ctx = SpanContext::new(parent_trace_id, parent_span_id, false, "")
  let non_sampled_parent = Some(non_sampled_parent_ctx)
  
  let sampling_result_non_sampled = Sampler::should_sample(
    sampler,
    non_sampled_parent,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result_non_sampled {
    SamplingResult::Decision(decision) => assert_eq(decision, Drop)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, Drop)
    _ => assert_true(false)
  }
  
  // Test with no parent (should use root sampler)
  let no_parent = None
  
  let sampling_result_no_parent = Sampler::should_sample(
    sampler,
    no_parent,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result_no_parent {
    SamplingResult::Decision(decision) => assert_eq(decision, Drop)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, Drop)
    _ => assert_true(false)
  }
}

// Test 5: Attribute Based Sampling Strategy
test "attribute based sampling strategy" {
  let attributes = Attributes::new()
  Attributes::set(attributes, "http.method", StringValue("GET"))
  Attributes::set(attributes, "http.status_code", IntValue(200))
  
  // Create a sampler that samples only GET requests with status code 200
  let sampler = Sampler::attribute_based(|attrs| {
    let method = Attributes::get(attrs, "http.method")
    let status_code = Attributes::get(attrs, "http.status_code")
    
    match (method, status_code) {
      (Some(StringValue("GET")), Some(IntValue(200))) => true
      _ => false
    }
  })
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_ctx = None
  let name = "test_span"
  let kind = Internal
  let links = []
  
  // Test with matching attributes
  let sampling_result = Sampler::should_sample(
    sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result {
    SamplingResult::Decision(decision) => assert_eq(decision, RecordAndSample)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, RecordAndSample)
    _ => assert_true(false)
  }
  
  // Test with non-matching attributes
  let non_matching_attrs = Attributes::new()
  Attributes::set(non_matching_attrs, "http.method", StringValue("POST"))
  Attributes::set(non_matching_attrs, "http.status_code", IntValue(200))
  
  let sampling_result_non_matching = Sampler::should_sample(
    sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    non_matching_attrs,
    links
  )
  
  match sampling_result_non_matching {
    SamplingResult::Decision(decision) => assert_eq(decision, Drop)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, Drop)
    _ => assert_true(false)
  }
}

// Test 6: Composite Sampling Strategy
test "composite sampling strategy" {
  // Create a composite sampler that samples if either of the child samplers decides to sample
  let sampler1 = Sampler::trace_id_ratio(0.5)
  let sampler2 = Sampler::attribute_based(|attrs| {
    let method = Attributes::get(attrs, "http.method")
    match method {
      Some(StringValue("GET")) => true
      _ => false
    }
  })
  
  let composite_sampler = Sampler::any_of([sampler1, sampler2])
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_ctx = None
  let name = "test_span"
  let kind = Internal
  let links = []
  
  // Test with GET request (should sample regardless of trace ID ratio)
  let get_attrs = Attributes::new()
  Attributes::set(get_attrs, "http.method", StringValue("GET"))
  
  let sampling_result_get = Sampler::should_sample(
    composite_sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    get_attrs,
    links
  )
  
  match sampling_result_get {
    SamplingResult::Decision(decision) => assert_eq(decision, RecordAndSample)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, RecordAndSample)
    _ => assert_true(false)
  }
  
  // Test with POST request (sampling depends on trace ID ratio)
  let post_attrs = Attributes::new()
  Attributes::set(post_attrs, "http.method", StringValue("POST"))
  
  let sampling_result_post = Sampler::should_sample(
    composite_sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    post_attrs,
    links
  )
  
  // The result could be either Drop or RecordAndSample based on the trace_id
  match sampling_result_post {
    SamplingResult::Decision(decision) => {
      assert_true(decision == Drop || decision == RecordAndSample)
    }
    SamplingResult::DecisionWithAttributes(decision, _) => {
      assert_true(decision == Drop || decision == RecordAndSample)
    }
    _ => assert_true(false)
  }
}

// Test 7: Adaptive Sampling Strategy
test "adaptive sampling strategy" {
  // Create an adaptive sampler that adjusts sampling rate based on load
  let adaptive_sampler = Sampler::adaptive(0.1, 1000) // Base rate 10%, max 1000 samples per second
  
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_ctx = None
  let name = "test_span"
  let kind = Internal
  let attributes = Attributes::new()
  let links = []
  
  // Test sampling under normal load
  let sampling_result_normal = Sampler::should_sample(
    adaptive_sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  // The result could be either Drop or RecordAndSample based on the adaptive algorithm
  match sampling_result_normal {
    SamplingResult::Decision(decision) => {
      assert_true(decision == Drop || decision == RecordAndSample)
    }
    SamplingResult::DecisionWithAttributes(decision, _) => {
      assert_true(decision == Drop || decision == RecordAndSample)
    }
    _ => assert_true(false)
  }
  
  // Test with high priority span (should always sample)
  let high_priority_attrs = Attributes::new()
  Attributes::set(high_priority_attrs, "span.priority", StringValue("high"))
  
  let sampling_result_high_priority = Sampler::should_sample(
    adaptive_sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    high_priority_attrs,
    links
  )
  
  match sampling_result_high_priority {
    SamplingResult::Decision(decision) => assert_eq(decision, RecordAndSample)
    SamplingResult::DecisionWithAttributes(decision, _) => assert_eq(decision, RecordAndSample)
    _ => assert_true(false)
  }
}

// Test 8: Sampling Decision Description
test "sampling decision description" {
  let sampler = Sampler::always_on()
  let trace_id = "0af7651916cd43dd8448eb211c80319c"
  let parent_ctx = None
  let name = "test_span"
  let kind = Internal
  let attributes = Attributes::new()
  let links = []
  
  let sampling_result = Sampler::should_sample(
    sampler,
    parent_ctx,
    trace_id,
    name,
    kind,
    attributes,
    links
  )
  
  match sampling_result {
    SamplingResult::DecisionWithAttributes(decision, attrs) => {
      assert_eq(decision, RecordAndSample)
      // Check that sampling attributes are added
      let sampler_type = Attributes::get(attrs, "sampler.type")
      match sampler_type {
        Some(StringValue(_)) => assert_true(true)
        _ => assert_true(false)
      }
    }
    SamplingResult::Decision(decision) => {
      assert_eq(decision, RecordAndSample)
    }
    _ => assert_true(false)
  }
}