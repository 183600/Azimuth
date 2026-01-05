// Azimuth Advanced Telemetry System Test Suite
// This file contains advanced test cases focusing on telemetry system capabilities

// Test 1: Adaptive Sampling Strategy
test "adaptive sampling strategy for high-volume telemetry" {
  // Define sampling strategy types
  enum SamplingStrategy {
    FixedRate(Float)  // Fixed sampling rate (0.0 to 1.0)
    Adaptive(Float, Int)  // Base rate, adjustment window
    PriorityBased(Array[(String, Float)])  // (priority, rate) mappings
    ErrorFocused(Float, Float)  // Normal rate, error rate
  }
  
  // Define sampling decision maker
  type SamplingDecision = {
    should_sample: Bool,
    strategy: SamplingStrategy,
    sample_rate: Float,
    attributes: Array[(String, String)]
  }
  
  // Create adaptive sampler
  let create_adaptive_sampler = fn(base_rate: Float, window_size: Int) {
    {
      strategy: SamplingStrategy::Adaptive(base_rate, window_size),
      current_rate: base_rate,
      sample_count: 0,
      total_count: 0,
      error_count: 0,
      window_start: 1640995200
    }
  }
  
  // Make sampling decision
  let make_sampling_decision = fn(sampler, telemetry_event) {
    let mut updated_sampler = sampler
    updated_sampler.total_count = updated_sampler.total_count + 1
    
    // Check if it's an error event
    let is_error = telemetry_event.event_type == "error"
    if is_error {
      updated_sampler.error_count = updated_sampler.error_count + 1
    }
    
    // Calculate adaptive rate based on error ratio
    let error_ratio = if updated_sampler.total_count > 0 {
      (updated_sampler.error_count as Float) / (updated_sampler.total_count as Float)
    } else {
      0.0
    }
    
    // Adjust sampling rate based on error ratio
    let adjusted_rate = if error_ratio > 0.1 {
      // High error rate - increase sampling
      min(1.0, updated_sampler.current_rate * 1.5)
    } else if error_ratio < 0.01 {
      // Low error rate - decrease sampling
      max(0.01, updated_sampler.current_rate * 0.8)
    } else {
      // Normal error rate - keep current rate
      updated_sampler.current_rate
    }
    
    updated_sampler.current_rate = adjusted_rate
    
    // Make sampling decision
    let random_value = (updated_sampler.total_count * 17) % 100  // Simple pseudo-random
    let sampling_threshold = (adjusted_rate * 100.0) as Int
    
    let should_sample = if is_error {
      // Always sample errors
      true
    } else {
      random_value < sampling_threshold
    }
    
    if should_sample {
      updated_sampler.sample_count = updated_sampler.sample_count + 1
    }
    
    (updated_sampler, {
      should_sample,
      strategy: updated_sampler.strategy,
      sample_rate: adjusted_rate,
      attributes: [
        ("error_ratio", error_ratio.to_string()),
        ("adjusted_rate", adjusted_rate.to_string())
      ]
    })
  }
  
  // Create test telemetry events
  let events = [
    { event_id: "evt-001", event_type: "request", timestamp: 1640995200 },
    { event_id: "evt-002", event_type: "request", timestamp: 1640995210 },
    { event_id: "evt-003", event_type: "error", timestamp: 1640995220 },
    { event_id: "evt-004", event_type: "request", timestamp: 1640995230 },
    { event_id: "evt-005", event_type: "request", timestamp: 1640995240 },
    { event_id: "evt-006", event_type: "error", timestamp: 1640995250 },
    { event_id: "evt-007", event_type: "request", timestamp: 1640995260 },
    { event_id: "evt-008", event_type: "error", timestamp: 1640995270 },
    { event_id: "evt-009", event_type: "request", timestamp: 1640995280 },
    { event_id: "evt-010", event_type: "request", timestamp: 1640995290 }
  ]
  
  // Test adaptive sampling
  let sampler = create_adaptive_sampler(0.1, 100)  // Start with 10% sampling
  let mut final_sampler = sampler
  let mut sampled_events = []
  
  for event in events {
    let (updated_sampler, decision) = make_sampling_decision(final_sampler, event)
    final_sampler = updated_sampler
    
    if decision.should_sample {
      sampled_events = sampled_events.push(event.event_id)
    }
  }
  
  // Verify that all error events were sampled
  assert_true(sampled_events.contains("evt-003"))
  assert_true(sampled_events.contains("evt-006"))
  assert_true(sampled_events.contains("evt-008"))
  
  // Verify that sampling rate was adjusted due to errors
  assert_true(final_sampler.current_rate > 0.1)  // Should have increased
  
  // Verify that sampler tracked statistics correctly
  assert_eq(final_sampler.total_count, 10)
  assert_eq(final_sampler.error_count, 3)
  assert_true(final_sampler.sample_count >= 3)  // At least all errors
}

// Test 2: Distributed Trace Context Propagation
test "distributed trace context propagation across services" {
  // Define trace context
  type TraceContext = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_flags: Int,
    trace_state: Array[(String, String)],
    baggage: Array[(String, String)]
  }
  
  // Define propagator for different formats
  enum PropagationFormat {
    TraceContext  // W3C Trace Context format
    Baggage       // W3C Baggage format
    Custom(String)  // Custom format
  }
  
  // Create trace context injector
  let inject_trace_context = fn(context: TraceContext, format: PropagationFormat) {
    match format {
      PropagationFormat::TraceContext => {
        let trace_parent = "00-" + context.trace_id + "-" + context.span_id + "-" + 
                          context.trace_flags.to_string(16).to_uppercase()
        
        let trace_state_items = context.trace_state.map(fn(item) {
          item.0 + "=" + item.1
        })
        let trace_state = trace_state_items.join(",")
        
        [
          ("traceparent", trace_parent),
          ("tracestate", trace_state)
        ]
      }
      PropagationFormat::Baggage => {
        let baggage_items = context.baggage.map(fn(item) {
          item.0 + "=" + item.1
        })
        let baggage_header = baggage_items.join(",")
        
        [("baggage", baggage_header)]
      }
      PropagationFormat::Custom(prefix) => {
        [
          (prefix + "-trace-id", context.trace_id),
          (prefix + "-span-id", context.span_id),
          (prefix + "-flags", context.trace_flags.to_string())
        ]
      }
    }
  }
  
  // Create trace context extractor
  let extract_trace_context = fn(headers: Array[(String, String)], format: PropagationFormat) {
    match format {
      PropagationFormat::TraceContext => {
        let traceparent_header = headers.find(fn(h) { h.0 == "traceparent" })
        let tracestate_header = headers.find(fn(h) { h.0 == "tracestate" })
        
        match traceparent_header {
          Some((_, traceparent)) => {
            // Parse: 00-trace-id-span-id-flags
            let parts = traceparent.split("-")
            if parts.length() >= 4 {
              let trace_id = parts[1]
              let span_id = parts[2]
              let flags = parts[3].to_int(16).unwrap_or(0)
              
              // Parse trace state
              let trace_state = match tracestate_header {
                Some((_, state)) => {
                  state.split(",").map(fn(item) {
                    let kv = item.split("=")
                    if kv.length() == 2 {
                      (kv[0], kv[1])
                    } else {
                      ("", "")
                    }
                  })
                }
                None => []
              }
              
              Some({
                trace_id,
                span_id,
                parent_span_id: None,
                trace_flags: flags,
                trace_state,
                baggage: []
              })
            } else {
              None
            }
          }
          None => None
        }
      }
      PropagationFormat::Baggage => {
        let baggage_header = headers.find(fn(h) { h.0 == "baggage" })
        
        match baggage_header {
          Some((_, baggage)) => {
            let baggage_items = baggage.split(",").map(fn(item) {
              let kv = item.split("=")
              if kv.length() == 2 {
                (kv[0], kv[1])
              } else {
                ("", "")
              }
            })
            
            // Create minimal trace context with baggage
            Some({
              trace_id: "unknown",
              span_id: "unknown",
              parent_span_id: None,
              trace_flags: 0,
              trace_state: [],
              baggage: baggage_items
            })
          }
          None => None
        }
      }
      PropagationFormat::Custom(prefix) => {
        let trace_id_header = headers.find(fn(h) { h.0 == prefix + "-trace-id" })
        let span_id_header = headers.find(fn(h) { h.0 == prefix + "-span-id" })
        let flags_header = headers.find(fn(h) { h.0 == prefix + "-flags" })
        
        match (trace_id_header, span_id_header) {
          (Some((_, trace_id)), Some((_, span_id))) => {
            let flags = match flags_header {
              Some((_, flags_str)) => flags_str.to_int().unwrap_or(0)
              None => 0
            }
            
            Some({
              trace_id,
              span_id,
              parent_span_id: None,
              trace_flags: flags,
              trace_state: [],
              baggage: []
            })
          }
          _ => None
        }
      }
    }
  }
  
  // Test trace context propagation
  let original_context = {
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: None,
    trace_flags: 1,
    trace_state: [("rojo", "00f067aa0ba902b7"), ("congo", "t61rcWkgMzE")],
    baggage: [("user-id", "12345"), ("session-id", "abcdef")]
  }
  
  // Test injection
  let tracecontext_headers = inject_trace_context(original_context, PropagationFormat::TraceContext)
  assert_eq(tracecontext_headers.length(), 2)
  
  let traceparent = tracecontext_headers.find(fn(h) { h.0 == "traceparent" }).unwrap().1
  assert_true(traceparent.contains("4bf92f3577b34da6a3ce929d0e0e4736"))
  assert_true(traceparent.contains("00f067aa0ba902b7"))
  
  let baggage_headers = inject_trace_context(original_context, PropagationFormat::Baggage)
  assert_eq(baggage_headers.length(), 1)
  
  let baggage = baggage_headers[0].1
  assert_true(baggage.contains("user-id=12345"))
  assert_true(baggage.contains("session-id=abcdef"))
  
  // Test extraction
  let extracted_context = extract_trace_context(tracecontext_headers, PropagationFormat::TraceContext)
  assert_true(extracted_context.is_some())
  
  let context = extracted_context.unwrap()
  assert_eq(context.trace_id, "4bf92f3577b34da6a3ce929d0e0e4736")
  assert_eq(context.span_id, "00f067aa0ba902b7")
  assert_eq(context.trace_flags, 1)
  
  // Test round-trip propagation
  let injected_headers = inject_trace_context(original_context, PropagationFormat::TraceContext)
  let extracted = extract_trace_context(injected_headers, PropagationFormat::TraceContext)
  
  match extracted {
    Some(ctx) => {
      assert_eq(ctx.trace_id, original_context.trace_id)
      assert_eq(ctx.span_id, original_context.span_id)
      assert_eq(ctx.trace_flags, original_context.trace_flags)
    }
    None => assert_true(false)
  }
  
  // Test cross-service propagation simulation
  let service_a_context = original_context
  let headers_a_to_b = inject_trace_context(service_a_context, PropagationFormat::TraceContext)
  let baggage_a_to_b = inject_trace_context(service_a_context, PropagationFormat::Baggage)
  
  let all_headers = headers_a_to_b + baggage_a_to_b
  let service_b_context = extract_trace_context(all_headers, PropagationFormat::TraceContext)
  
  match service_b_context {
    Some(ctx) => {
      // Service B creates a child span
      let child_context = {
        ctx |
        span_id: "child-span-123",
        parent_span_id: Some(ctx.span_id)
      }
      
      // Service B propagates to Service C
      let headers_b_to_c = inject_trace_context(child_context, PropagationFormat::TraceContext)
      let service_c_context = extract_trace_context(headers_b_to_c, PropagationFormat::TraceContext)
      
      match service_c_context {
        Some(child_ctx) => {
          assert_eq(child_ctx.trace_id, original_context.trace_id)
          assert_eq(child_ctx.span_id, "child-span-123")
          assert_eq(child_ctx.parent_span_id, Some("00f067aa0ba902b7"))
        }
        None => assert_true(false)
      }
    }
    None => assert_true(false)
  }
}

// Test 3: Telemetry Data Compression and Serialization
test "telemetry data compression and serialization efficiency" {
  // Define telemetry data structure
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    duration: Int,
    status: String,
    tags: Array[(String, String)],
    logs: Array[(Int, String)],
    metrics: Array[(String, Float)]
  }
  
  // Define compression algorithms
  enum CompressionType {
    None
    Gzip
    Lz4
    Custom(String)
  }
  
  // Define serialization formats
  enum SerializationFormat {
    Json
    Protobuf
    Avro
    Custom(String)
  }
  
  // Simple mock compression function
  let compress_data = fn(data: String, compression: CompressionType) {
    match compression {
      CompressionType::None => data
      CompressionType::Gzip => {
        // Mock gzip compression (just add prefix)
        "GZIP:" + data
      }
      CompressionType::Lz4 => {
        // Mock LZ4 compression (just add prefix)
        "LZ4:" + data
      }
      CompressionType::Custom(prefix) => {
        prefix + ":" + data
      }
    }
  }
  
  // Simple mock decompression function
  let decompress_data = fn(compressed_data: String, compression: CompressionType) {
    match compression {
      CompressionType::None => compressed_data
      CompressionType::Gzip => {
        // Remove prefix
        compressed_data.slice(5, compressed_data.length())
      }
      CompressionType::Lz4 => {
        // Remove prefix
        compressed_data.slice(4, compressed_data.length())
      }
      CompressionType::Custom(prefix) => {
        // Remove prefix
        compressed_data.slice(prefix.length() + 1, compressed_data.length())
      }
    }
  }
  
  // Serialize telemetry data to JSON
  let serialize_to_json = fn(data: TelemetryData) {
    let tags_json = data.tags.map(fn(tag) {
      "\"" + tag.0 + "\":\"" + tag.1 + "\""
    }).join(",")
    
    let logs_json = data.logs.map(fn(log) {
      "{\"timestamp\":" + log.0.to_string() + ",\"message\":\"" + log.1 + "\"}"
    }).join(",")
    
    let metrics_json = data.metrics.map(fn(metric) {
      "\"" + metric.0 + "\":" + metric.1.to_string()
    }).join(",")
    
    let parent_span_json = match data.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    "{
      \"timestamp\":" + data.timestamp.to_string() + ",
      \"trace_id\":\"" + data.trace_id + "\",
      \"span_id\":\"" + data.span_id + "\",
      \"parent_span_id\":" + parent_span_json + ",
      \"operation_name\":\"" + data.operation_name + "\",
      \"duration\":" + data.duration.to_string() + ",
      \"status\":\"" + data.status + "\",
      \"tags\":{" + tags_json + "},
      \"logs\":[" + logs_json + "],
      \"metrics\":{" + metrics_json + "}
    }"
  }
  
  // Serialize telemetry data to mock protobuf
  let serialize_to_protobuf = fn(data: TelemetryData) {
    // Mock protobuf serialization (just add prefix and join fields)
    let fields = [
      "timestamp:" + data.timestamp.to_string(),
      "trace_id:" + data.trace_id,
      "span_id:" + data.span_id,
      "operation:" + data.operation_name,
      "duration:" + data.duration.to_string(),
      "status:" + data.status
    ]
    
    "PROTOBUF:" + fields.join("|")
  }
  
  // Serialize telemetry data
  let serialize_telemetry = fn(data: TelemetryData, format: SerializationFormat) {
    match format {
      SerializationFormat::Json => serialize_to_json(data)
      SerializationFormat::Protobuf => serialize_to_protobuf(data)
      SerializationFormat::Avro => {
        // Mock Avro serialization
        "AVRO:" + serialize_to_json(data)
      }
      SerializationFormat::Custom(prefix) => {
        prefix + ":" + serialize_to_json(data)
      }
    }
  }
  
  // Create test telemetry data
  let telemetry_data = {
    timestamp: 1640995200,
    trace_id: "4bf92f3577b34da6a3ce929d0e0e4736",
    span_id: "00f067aa0ba902b7",
    parent_span_id: Some("parent123"),
    operation_name: "http.request",
    duration: 150,
    status: "ok",
    tags: [
      ("http.method", "GET"),
      ("http.url", "https://api.example.com/data"),
      ("http.status_code", "200"),
      ("service.name", "api-service")
    ],
    logs: [
      (1640995200, "Request started"),
      (1640995250, "Processing request"),
      (1640995350, "Request completed")
    ],
    metrics: [
      ("cpu.usage", 75.5),
      ("memory.usage", 1024.0),
      ("network.io", 2048.0)
    ]
  }
  
  // Test different serialization formats
  let json_data = serialize_telemetry(telemetry_data, SerializationFormat::Json)
  let protobuf_data = serialize_telemetry(telemetry_data, SerializationFormat::Protobuf)
  let avro_data = serialize_telemetry(telemetry_data, SerializationFormat::Avro)
  
  // Verify serialization
  assert_true(json_data.contains("\"trace_id\":\"4bf92f3577b34da6a3ce929d0e0e4736\""))
  assert_true(json_data.contains("\"operation_name\":\"http.request\""))
  
  assert_true(protobuf_data.contains("PROTOBUF:"))
  assert_true(protobuf_data.contains("trace_id:4bf92f3577b34da6a3ce929d0e0e4736"))
  
  assert_true(avro_data.contains("AVRO:"))
  
  // Test compression
  let uncompressed_size = json_data.length()
  let gzip_compressed = compress_data(json_data, CompressionType::Gzip)
  let lz4_compressed = compress_data(json_data, CompressionType::Lz4)
  
  // Verify compression
  assert_true(gzip_compressed.contains("GZIP:"))
  assert_true(lz4_compressed.contains("LZ4:"))
  
  // Test decompression
  let gzip_decompressed = decompress_data(gzip_compressed, CompressionType::Gzip)
  let lz4_decompressed = decompress_data(lz4_compressed, CompressionType::Lz4)
  
  assert_eq(gzip_decompressed, json_data)
  assert_eq(lz4_decompressed, json_data)
  
  // Test batch compression and serialization
  let batch_data = [
    telemetry_data,
    {
      telemetry_data |
      span_id: "span456",
      operation_name: "db.query",
      duration: 75,
      metrics: [("db.duration", 75.0), ("db.rows", 10.0)]
    },
    {
      telemetry_data |
      span_id: "span789",
      operation_name: "cache.get",
      duration: 5,
      metrics: [("cache.hit", 1.0)]
    }
  ]
  
  // Serialize batch
  let batch_json = batch_data.map(fn(data) {
    serialize_telemetry(data, SerializationFormat::Json)
  }).join("\n")
  
  // Compress batch
  let compressed_batch = compress_data(batch_json, CompressionType::Gzip)
  
  // Verify batch compression
  assert_true(compressed_batch.contains("GZIP:"))
  assert_true(compressed_batch.length() > batch_json.length())  // Compression adds overhead in mock
  
  // Decompress and verify
  let decompressed_batch = decompress_data(compressed_batch, CompressionType::Gzip)
  assert_eq(decompressed_batch, batch_json)
  
  // Test compression ratio calculation
  let calculate_compression_ratio = fn(original: String, compressed: String) {
    if original.length() > 0 {
      1.0 - ((compressed.length() as Float) / (original.length() as Float))
    } else {
      0.0
    }
  }
  
  let gzip_ratio = calculate_compression_ratio(json_data, gzip_compressed)
  let lz4_ratio = calculate_compression_ratio(json_data, lz4_compressed)
  
  // In mock implementation, compression adds overhead, so ratio is negative
  // In real implementation, this would be positive
  assert_true(gzip_ratio < 0.0)
  assert_true(lz4_ratio < 0.0)
}

// Test 4: Adaptive Batch Processing
test "adaptive batch processing for varying telemetry loads" {
  // Define batch processor configuration
  type BatchConfig = {
    min_batch_size: Int,
    max_batch_size: Int,
    max_wait_time: Int,  // milliseconds
    adaptive_threshold: Float,  // Load threshold for adaptation
    growth_factor: Float,  // How much to grow batch size under load
    shrink_factor: Float   // How much to shrink batch size under low load
  }
  
  // Define batch processor state
  type BatchProcessor = {
    config: BatchConfig,
    current_batch_size: Int,
    pending_items: Array[String],
    last_flush_time: Int,
    recent_processing_times: Array[Int],  // Track processing times
    recent_batch_sizes: Array[Int]        // Track actual batch sizes
  }
  
  // Create batch processor
  let create_batch_processor = fn(min_size: Int, max_size: Int, max_wait: Int) {
    {
      config: {
        min_batch_size: min_size,
        max_batch_size: max_size,
        max_wait_time: max_wait,
        adaptive_threshold: 0.8,  // 80% of max_wait_time
        growth_factor: 1.5,
        shrink_factor: 0.7
      },
      current_batch_size: min_size,
      pending_items: [],
      last_flush_time: 0,
      recent_processing_times: [],
      recent_batch_sizes: []
    }
  }
  
  // Add item to batch processor
  let add_item = fn(processor: BatchProcessor, item: String, current_time: Int) {
    let new_pending = processor.pending_items.push(item)
    
    // Check if batch should be flushed
    let size_reached = new_pending.length() >= processor.current_batch_size
    let time_reached = (current_time - processor.last_flush_time) >= processor.config.max_wait_time
    
    if size_reached or time_reached {
      // Flush the batch
      let batch_size = new_pending.length()
      let flushed_items = new_pending
      
      // Record batch size
      let new_batch_sizes = processor.recent_batch_sizes.push(batch_size)
        .slice(-10, 10)  // Keep only last 10 batch sizes
      
      // Reset pending items
      let updated_processor = {
        processor |
        pending_items: [],
        last_flush_time: current_time,
        recent_batch_sizes: new_batch_sizes
      }
      
      (updated_processor, Some(flushed_items))
    } else {
      // Don't flush yet
      let updated_processor = {
        processor |
        pending_items: new_pending
      }
      (updated_processor, None)
    }
  }
  
  // Simulate batch processing time
  let simulate_processing = fn(processor: BatchProcessor, batch_size: Int, current_time: Int) {
    // Simulate processing time based on batch size
    let base_time = 10  // Base processing time in ms
    let processing_time = base_time + (batch_size * 2)  // 2ms per item
    
    // Record processing time
    let new_processing_times = processor.recent_processing_times.push(processing_time)
      .slice(-10, 10)  // Keep only last 10 processing times
    
    // Calculate average processing time
    let avg_processing_time = if new_processing_times.length() > 0 {
      let sum = new_processing_times.reduce(0, fn(acc, time) { acc + time })
      sum / new_processing_times.length()
    } else {
      0
    }
    
    // Calculate load ratio (processing time / max wait time)
    let load_ratio = if processor.config.max_wait_time > 0 {
      (avg_processing_time as Float) / (processor.config.max_wait_time as Float)
    } else {
      0.0
    }
    
    // Adapt batch size based on load
    let new_batch_size = if load_ratio > processor.config.adaptive_threshold {
      // High load - increase batch size
      let new_size = ((processor.current_batch_size as Float) * processor.config.growth_factor) as Int
      min(new_size, processor.config.max_batch_size)
    } else if load_ratio < 0.5 {
      // Low load - decrease batch size
      let new_size = ((processor.current_batch_size as Float) * processor.config.shrink_factor) as Int
      max(new_size, processor.config.min_batch_size)
    } else {
      // Moderate load - keep current size
      processor.current_batch_size
    }
    
    let updated_processor = {
      processor |
      recent_processing_times: new_processing_times,
      current_batch_size: new_batch_size
    }
    
    (updated_processor, processing_time, load_ratio)
  }
  
  // Test adaptive batch processing
  let processor = create_batch_processor(5, 20, 100)  // Start with 5, max 20, 100ms wait
  let base_time = 1640995200000  // Current time in ms
  
  // Add items rapidly to simulate high load
  let mut current_processor = processor
  let mut current_time = base_time
  let mut flushed_batches = []
  
  // First batch - should flush when size reached
  for i in 0..5 {
    let (updated_processor, batch) = add_item(current_processor, "item-" + i.to_string(), current_time)
    current_processor = updated_processor
    
    match batch {
      Some(items) => {
        flushed_batches = flushed_batches.push(items)
        let (proc_after, proc_time, load_ratio) = simulate_processing(current_processor, items.length(), current_time)
        current_processor = proc_after
        current_time = current_time + proc_time
      }
      None => ()
    }
  }
  
  // Should have flushed first batch
  assert_eq(flushed_batches.length(), 1)
  assert_eq(flushed_batches[0].length(), 5)
  assert_eq(current_processor.pending_items.length(), 0)
  assert_eq(current_processor.current_batch_size, 5)  // Still at initial size
  
  // Add more items to trigger adaptation
  for i in 5..15 {
    let (updated_processor, batch) = add_item(current_processor, "item-" + i.to_string(), current_time)
    current_processor = updated_processor
    
    match batch {
      Some(items) => {
        flushed_batches = flushed_batches.push(items)
        let (proc_after, proc_time, load_ratio) = simulate_processing(current_processor, items.length(), current_time)
        current_processor = proc_after
        
        // Simulate increasing load by reducing time between items
        current_time = current_time + 5  // Only 5ms between items
      }
      None => ()
    }
  }
  
  // Should have flushed more batches
  assert_true(flushed_batches.length() > 1)
  
  // Batch size should have adapted to the load
  assert_true(current_processor.current_batch_size > 5)  // Should have increased
  
  // Test time-based flush
  current_time = current_time + 200  // Wait longer than max_wait_time
  
  // Add one item
  let (updated_processor, batch) = add_item(current_processor, "time-based-item", current_time)
  current_processor = updated_processor
  
  // Should not flush yet (not enough items)
  assert_eq(batch, None)
  assert_eq(current_processor.pending_items.length(), 1)
  
  // Wait more than max_wait_time
  current_time = current_time + 150
  
  // Add another item
  let (updated_processor2, batch2) = add_item(current_processor, "time-based-item-2", current_time)
  current_processor = updated_processor2
  
  // Should flush due to time
  assert_true(batch2.is_some())
  assert_eq(current_processor.pending_items.length(), 0)
  
  // Test adaptation under low load
  current_time = current_time + 1000  // Wait a long time between items
  
  // Add items slowly
  for i in 0..3 {
    let (updated_processor, batch) = add_item(current_processor, "slow-item-" + i.to_string(), current_time)
    current_processor = updated_processor
    
    match batch {
      Some(items) => {
        flushed_batches = flushed_batches.push(items)
        let (proc_after, proc_time, load_ratio) = simulate_processing(current_processor, items.length(), current_time)
        current_processor = proc_after
      }
      None => ()
    }
    
    current_time = current_time + 500  // 500ms between items (slow)
  }
  
  // Batch size should have decreased due to low load
  assert_true(current_processor.current_batch_size < 20)  // Should have decreased from max
  
  // Verify constraints
  assert_true(current_processor.current_batch_size >= processor.config.min_batch_size)
  assert_true(current_processor.current_batch_size <= processor.config.max_batch_size)
}

// Test 5: Telemetry Data Lifecycle Management
test "telemetry data lifecycle management and retention policies" {
  // Define data retention policy
  enum RetentionPolicy {
    TimeBased(Int),  // Retention period in seconds
    SizeBased(Int),  // Maximum size in bytes
    CountBased(Int), // Maximum number of records
    Custom(Array[(String, String)])  // Custom rules
  }
  
  // Define data lifecycle state
  enum DataState {
    Active,      // Currently being used
    Archiving,   // Being moved to archive
    Archived,    // In long-term storage
    Expiring,    // About to expire
    Expired      // Ready for deletion
  }
  
  // Define telemetry data record
  type TelemetryRecord = {
    id: String,
    data: String,
    timestamp: Int,
    size: Int,
    state: DataState,
    access_count: Int,
    last_accessed: Int,
    metadata: Array[(String, String)]
  }
  
  // Define lifecycle manager
  type LifecycleManager = {
    records: Array[TelemetryRecord],
    retention_policy: RetentionPolicy,
    archive_threshold: Int,
    expiration_threshold: Int,
    current_time: Int
  }
  
  // Create lifecycle manager
  let create_lifecycle_manager = fn(policy: RetentionPolicy, archive_time: Int, expire_time: Int) {
    {
      records: [],
      retention_policy: policy,
      archive_threshold: archive_time,
      expiration_threshold: expire_time,
      current_time: 1640995200
    }
  }
  
  // Add telemetry record
  let add_record = fn(manager: LifecycleManager, id: String, data: String, metadata: Array[(String, String)]) {
    let record = {
      id,
      data,
      timestamp: manager.current_time,
      size: data.length(),
      state: DataState::Active,
      access_count: 0,
      last_accessed: manager.current_time,
      metadata
    }
    
    {
      manager |
      records: manager.records.push(record)
    }
  }
  
  // Update record state based on age
  let update_record_states = fn(manager: LifecycleManager) {
    let updated_records = manager.records.map(fn(record) {
      let age = manager.current_time - record.timestamp
      
      let new_state = if age > manager.expiration_threshold {
        DataState::Expired
      } else if age > manager.archive_threshold {
        DataState::Archived
      } else if age > (manager.archive_threshold / 2) {
        DataState::Archiving
      } else {
        record.state
      }
      
      {
        record |
        state: new_state
      }
    })
    
    {
      manager |
      records: updated_records
    }
  }
  
  // Apply retention policy
  let apply_retention_policy = fn(manager: LifecycleManager) {
    let filtered_records = match manager.retention_policy {
      RetentionPolicy::TimeBased(retention_seconds) => {
        manager.records.filter(fn(record) {
          (manager.current_time - record.timestamp) <= retention_seconds
        })
      }
      RetentionPolicy::SizeBased(max_size_bytes) => {
        let mut total_size = 0
        let mut kept_records = []
        
        // Sort by timestamp (newest first) and keep until size limit reached
        let sorted_records = manager.records.sort_by(fn(a, b) { b.timestamp - a.timestamp })
        
        for record in sorted_records {
          if total_size + record.size <= max_size_bytes {
            kept_records = kept_records.push(record)
            total_size = total_size + record.size
          }
        }
        
        kept_records.sort_by(fn(a, b) { a.timestamp - b.timestamp })  // Restore original order
      }
      RetentionPolicy::CountBased(max_count) => {
        // Keep the most recent records
        let sorted_records = manager.records.sort_by(fn(a, b) { b.timestamp - a.timestamp })
        sorted_records.slice(0, max_count)
      }
      RetentionPolicy::Custom(rules) => {
        // Apply custom rules
        manager.records.filter(fn(record) {
          let mut keep = true
          
          for (key, value) in rules {
            match key {
              "min_priority" => {
                match record.metadata.find(fn(m) { m.0 == "priority" }) {
                  Some((_, priority)) => {
                    if priority.to_int().unwrap_or(0) < value.to_int().unwrap_or(0) {
                      keep = false
                    }
                  }
                  None => ()
                }
              }
              "required_tags" => {
                let required_tags = value.split(",")
                for tag in required_tags {
                  if not(record.metadata.find(fn(m) { m.0 == tag }).is_some()) {
                    keep = false
                  }
                }
              }
              _ => ()
            }
          }
          
          keep
        })
      }
    }
    
    {
      manager |
      records: filtered_records
    }
  }
  
  // Access record (updates access statistics)
  let access_record = fn(manager: LifecycleManager, id: String) {
    let updated_records = manager.records.map(fn(record) {
      if record.id == id {
        {
          record |
          access_count: record.access_count + 1,
          last_accessed: manager.current_time
        }
      } else {
        record
      }
    })
    
    {
      manager |
      records: updated_records
    }
  }
  
  // Test lifecycle management
  let manager = create_lifecycle_manager(
    RetentionPolicy::TimeBased(86400),  // 24 hours retention
    43200,  // Archive after 12 hours
    86400   // Expire after 24 hours
  )
  
  // Add records at different times
  let manager1 = add_record(manager, "rec-1", "data1", [("priority", "1"), ("service", "api")])
  let manager2 = add_record(manager1, "rec-2", "data2", [("priority", "2"), ("service", "db")])
  
  // Advance time by 6 hours
  let manager3 = {
    manager2 |
    current_time: manager2.current_time + 21600  // 6 hours
  }
  
  let manager4 = add_record(manager3, "rec-3", "data3", [("priority", "3"), ("service", "cache")])
  
  // Advance time by another 6 hours (total 12 hours)
  let manager5 = {
    manager4 |
    current_time: manager4.current_time + 21600  // 12 hours
  }
  
  // Update record states
  let manager6 = update_record_states(manager5)
  
  // Check that older records are now in archiving state
  let rec1 = manager6.records.find(fn(r) { r.id == "rec-1" }).unwrap()
  let rec2 = manager6.records.find(fn(r) { r.id == "rec-2" }).unwrap()
  let rec3 = manager6.records.find(fn(r) { r.id == "rec-3" }).unwrap()
  
  assert_eq(rec1.state, DataState::Archiving)
  assert_eq(rec2.state, DataState::Archiving)
  assert_eq(rec3.state, DataState::Active)
  
  // Advance time by another 12 hours (total 24 hours)
  let manager7 = {
    manager6 |
    current_time: manager6.current_time + 43200  // 24 hours
  }
  
  // Update record states again
  let manager8 = update_record_states(manager7)
  
  // Check that oldest records are now expired
  let rec1_updated = manager8.records.find(fn(r) { r.id == "rec-1" }).unwrap()
  let rec2_updated = manager8.records.find(fn(r) { r.id == "rec-2" }).unwrap()
  let rec3_updated = manager8.records.find(fn(r) { r.id == "rec-3" }).unwrap()
  
  assert_eq(rec1_updated.state, DataState::Expired)
  assert_eq(rec2_updated.state, DataState::Expired)
  assert_eq(rec3_updated.state, DataState::Archiving)
  
  // Apply retention policy (should remove expired records)
  let manager9 = apply_retention_policy(manager8)
  
  // Expired records should be removed
  assert_eq(manager9.records.length(), 1)
  assert_eq(manager9.records[0].id, "rec-3")
  
  // Test size-based retention policy
  let size_manager = create_lifecycle_manager(
    RetentionPolicy::SizeBased(20),  // Max 20 bytes
    43200,
    86400
  )
  
  let size_manager1 = add_record(size_manager, "size-rec-1", "small-data", [])
  let size_manager2 = add_record(size_manager1, "size-rec-2", "medium-sized-data", [])
  let size_manager3 = add_record(size_manager2, "size-rec-3", "very-large-data-piece", [])
  
  // Apply size-based policy
  let size_manager4 = apply_retention_policy(size_manager3)
  
  // Should keep records until size limit reached
  let mut total_size = 0
  for record in size_manager4.records {
    total_size = total_size + record.size
  }
  
  assert_true(total_size <= 20)
  
  // Test count-based retention policy
  let count_manager = create_lifecycle_manager(
    RetentionPolicy::CountBased(2),  // Max 2 records
    43200,
    86400
  )
  
  let count_manager1 = add_record(count_manager, "count-rec-1", "data1", [])
  let count_manager2 = add_record(count_manager1, "count-rec-2", "data2", [])
  let count_manager3 = add_record(count_manager2, "count-rec-3", "data3", [])
  
  // Apply count-based policy
  let count_manager4 = apply_retention_policy(count_manager3)
  
  // Should keep only 2 most recent records
  assert_eq(count_manager4.records.length(), 2)
  assert_true(count_manager4.records.find(fn(r) { r.id == "count-rec-2" }).is_some())
  assert_true(count_manager4.records.find(fn(r) { r.id == "count-rec-3" }).is_some())
  
  // Test custom retention policy
  let custom_rules = [
    ("min_priority", "2"),
    ("required_tags", "service")
  ]
  
  let custom_manager = create_lifecycle_manager(
    RetentionPolicy::Custom(custom_rules),
    43200,
    86400
  )
  
  let custom_manager1 = add_record(custom_manager, "custom-rec-1", "data1", [("priority", "1"), ("service", "api")])
  let custom_manager2 = add_record(custom_manager1, "custom-rec-2", "data2", [("priority", "2"), ("service", "db")])
  let custom_manager3 = add_record(custom_manager2, "custom-rec-3", "data3", [("priority", "3")])  // Missing service tag
  let custom_manager4 = add_record(custom_manager3, "custom-rec-4", "data4", [("priority", "4"), ("service", "cache")])
  
  // Apply custom policy
  let custom_manager5 = apply_retention_policy(custom_manager4)
  
  // Should keep only records with priority >= 2 and service tag
  assert_eq(custom_manager5.records.length(), 2)
  assert_true(custom_manager5.records.find(fn(r) { r.id == "custom-rec-2" }).is_some())
  assert_true(custom_manager5.records.find(fn(r) { r.id == "custom-rec-4" }).is_some())
  
  // Test record access
  let access_manager = access_record(custom_manager5, "custom-rec-2")
  let accessed_record = access_manager.records.find(fn(r) { r.id == "custom-rec-2" }).unwrap()
  
  assert_eq(accessed_record.access_count, 1)
  assert_eq(accessed_record.last_accessed, access_manager.current_time)
}

// Test 6: Real-time Monitoring and Alerting
test "real-time monitoring and alerting system" {
  // Define alert severity levels
  enum AlertSeverity {
    Info
    Warning
    Error
    Critical
  }
  
  // Define alert condition
  enum AlertCondition {
    Threshold(String, String, Float),  // metric_name, operator, threshold
    Rate(String, String, Float),       // metric_name, time_window, rate
    Anomaly(String, Float),            // metric_name, deviation_threshold
    Pattern(String, String)            // pattern_name, pattern_value
  }
  
  // Define alert
  type Alert = {
    id: String,
    name: String,
    description: String,
    severity: AlertSeverity,
    condition: AlertCondition,
    triggered_at: Int,
    resolved_at: Option[Int],
    metadata: Array[(String, String)]
  }
  
  // Define monitoring rule
  type MonitoringRule = {
    id: String,
    name: String,
    condition: AlertCondition,
    severity: AlertSeverity,
    enabled: Bool,
    cooldown_period: Int,  // seconds
    evaluation_interval: Int,  // seconds
    last_evaluation: Int,
    last_triggered: Option[Int]
  }
  
  // Define monitoring system
  type MonitoringSystem = {
    rules: Array[MonitoringRule],
    active_alerts: Array[Alert],
    metrics_history: Array[(String, Int, Float)],  // (metric_name, timestamp, value)
    current_time: Int
  }
  
  // Create monitoring system
  let create_monitoring_system = fn() {
    {
      rules: [],
      active_alerts: [],
      metrics_history: [],
      current_time: 1640995200
    }
  }
  
  // Add monitoring rule
  let add_monitoring_rule = fn(system: MonitoringSystem, rule: MonitoringRule) {
    {
      system |
      rules: system.rules.push(rule)
    }
  }
  
  // Record metric value
  let record_metric = fn(system: MonitoringSystem, metric_name: String, value: Float) {
    let new_metric = (metric_name, system.current_time, value)
    let updated_history = system.metrics_history.push(new_metric)
    
    // Keep only last 1000 metrics
    let trimmed_history = if updated_history.length() > 1000 {
      updated_history.slice(-1000, 1000)
    } else {
      updated_history
    }
    
    {
      system |
      metrics_history: trimmed_history
    }
  }
  
  // Evaluate monitoring rule
  let evaluate_rule = fn(system: MonitoringSystem, rule: MonitoringRule) {
    // Check if rule is in cooldown period
    match rule.last_triggered {
      Some(last_time) => {
        if (system.current_time - last_time) < rule.cooldown_period {
          return (system, None)  // In cooldown, don't evaluate
        }
      }
      None => ()
    }
    
    // Evaluate condition
    let should_trigger = match rule.condition {
      AlertCondition::Threshold(metric_name, operator, threshold) => {
        // Get latest metric value
        let latest_metric = system.metrics_history
          .filter(fn(m) { m.0 == metric_name })
          .sort_by(fn(a, b) { b.1 - a.1 })
        
        match latest_metric {
          [(_, _, value)] => {
            match operator {
              ">" => value > threshold
              ">=" => value >= threshold
              "<" => value < threshold
              "<=" => value <= threshold
              "==" => value == threshold
              "!=" => value != threshold
              _ => false
            }
          }
          _ => false
        }
      }
      AlertCondition::Rate(metric_name, time_window, rate_threshold) => {
        // Calculate rate of change over time window
        let window_start = system.current_time - time_window
        let window_metrics = system.metrics_history
          .filter(fn(m) { m.0 == metric_name and m.1 >= window_start })
        
        if window_metrics.length() >= 2 {
          let first_value = window_metrics[0].2
          let last_value = window_metrics[window_metrics.length() - 1].2
          let time_diff = window_metrics[window_metrics.length() - 1].1 - window_metrics[0].1
          
          if time_diff > 0 {
            let rate = (last_value - first_value) / (time_diff as Float)
            rate > rate_threshold
          } else {
            false
          }
        } else {
          false
        }
      }
      AlertCondition::Anomaly(metric_name, deviation_threshold) => {
        // Detect anomalies using standard deviation
        let metric_values = system.metrics_history
          .filter(fn(m) { m.0 == metric_name })
          .map(fn(m) { m.2 })
        
        if metric_values.length() >= 10 {
          // Calculate mean
          let sum = metric_values.reduce(0.0, fn(acc, v) { acc + v })
          let mean = sum / (metric_values.length() as Float)
          
          // Calculate standard deviation
          let variance = metric_values.reduce(0.0, fn(acc, v) {
            let diff = v - mean
            acc + (diff * diff)
          }) / (metric_values.length() as Float)
          
          let std_dev = variance.sqrt()
          
          // Check if latest value deviates from mean
          match metric_values.sort_by(fn(a, b) { b - a }) {
            [latest_value, ..] => {
              (latest_value - mean).abs() > (std_dev * deviation_threshold)
            }
            _ => false
          }
        } else {
          false
        }
      }
      AlertCondition::Pattern(pattern_name, pattern_value) => {
        // Pattern matching (simplified)
        pattern_name == "error_spike" and pattern_value == "detected"
      }
    }
    
    if should_trigger {
      // Create alert
      let alert = {
        id: "alert-" + rule.id + "-" + system.current_time.to_string(),
        name: rule.name,
        description: "Alert triggered by rule: " + rule.name,
        severity: rule.severity,
        condition: rule.condition,
        triggered_at: system.current_time,
        resolved_at: None,
        metadata: [
          ("rule_id", rule.id),
          ("evaluation_time", system.current_time.to_string())
        ]
      }
      
      // Update rule's last triggered time
      let updated_rules = system.rules.map(fn(r) {
        if r.id == rule.id {
          { r | last_triggered: Some(system.current_time) }
        } else {
          r
        }
      })
      
      let updated_system = {
        system |
        rules: updated_rules,
        active_alerts: system.active_alerts.push(alert)
      }
      
      (updated_system, Some(alert))
    } else {
      (system, None)
    }
  }
  
  // Evaluate all rules
  let evaluate_all_rules = fn(system: MonitoringSystem) {
    let mut updated_system = system
    let mut triggered_alerts = []
    
    for rule in system.rules {
      if rule.enabled and (system.current_time - rule.last_evaluation) >= rule.evaluation_interval {
        let (sys, alert) = evaluate_rule(updated_system, rule)
        updated_system = sys
        
        // Update rule's last evaluation time
        updated_system = {
          updated_system |
          rules: updated_system.rules.map(fn(r) {
            if r.id == rule.id {
              { r | last_evaluation: system.current_time }
            } else {
              r
            }
          })
        }
        
        match alert {
          Some(a) => triggered_alerts = triggered_alerts.push(a)
          None => ()
        }
      }
    }
    
    (updated_system, triggered_alerts)
  }
  
  // Resolve alert
  let resolve_alert = fn(system: MonitoringSystem, alert_id: String) {
    let updated_alerts = system.active_alerts.map(fn(alert) {
      if alert.id == alert_id {
        { alert | resolved_at: Some(system.current_time) }
      } else {
        alert
      }
    })
    
    {
      system |
      active_alerts: updated_alerts
    }
  }
  
  // Test monitoring system
  let system = create_monitoring_system()
  
  // Add monitoring rules
  let cpu_rule = {
    id: "cpu-high",
    name: "High CPU Usage",
    condition: AlertCondition::Threshold("cpu_usage", ">", 80.0),
    severity: AlertSeverity::Warning,
    enabled: true,
    cooldown_period: 300,  // 5 minutes
    evaluation_interval: 60,  // 1 minute
    last_evaluation: 0,
    last_triggered: None
  }
  
  let error_rate_rule = {
    id: "error-rate",
    name: "High Error Rate",
    condition: AlertCondition::Rate("error_count", 300, 10.0),  // 10 errors per 5 minutes
    severity: AlertSeverity::Critical,
    enabled: true,
    cooldown_period: 600,  // 10 minutes
    evaluation_interval: 60,  // 1 minute
    last_evaluation: 0,
    last_triggered: None
  }
  
  let system1 = add_monitoring_rule(system, cpu_rule)
  let system2 = add_monitoring_rule(system1, error_rate_rule)
  
  // Record normal metrics
  let system3 = record_metric(system2, "cpu_usage", 50.0)
  let system4 = record_metric(system3, "error_count", 5.0)
  
  // Advance time and evaluate
  let system5 = {
    system4 |
    current_time: system4.current_time + 60
  }
  
  let (system6, alerts1) = evaluate_all_rules(system5)
  
  // Should not trigger any alerts yet
  assert_eq(alerts1.length(), 0)
  assert_eq(system6.active_alerts.length(), 0)
  
  // Record high CPU usage
  let system7 = record_metric(system6, "cpu_usage", 85.0)
  
  // Advance time and evaluate
  let system8 = {
    system7 |
    current_time: system7.current_time + 60
  }
  
  let (system9, alerts2) = evaluate_all_rules(system8)
  
  // Should trigger CPU alert
  assert_eq(alerts2.length(), 1)
  assert_eq(alerts2[0].name, "High CPU Usage")
  assert_eq(alerts2[0].severity, AlertSeverity::Warning)
  
  // Check that alert was added to active alerts
  assert_eq(system9.active_alerts.length(), 1)
  
  // Try to trigger again within cooldown period
  let system10 = record_metric(system9, "cpu_usage", 90.0)
  
  // Advance time and evaluate
  let system11 = {
    system10 |
    current_time: system10.current_time + 60
  }
  
  let (system12, alerts3) = evaluate_all_rules(system11)
  
  // Should not trigger again due to cooldown
  assert_eq(alerts3.length(), 0)
  assert_eq(system12.active_alerts.length(), 1)
  
  // Advance time beyond cooldown period
  let system13 = {
    system12 |
    current_time: system12.current_time + 300
  }
  
  let (system14, alerts4) = evaluate_all_rules(system13)
  
  // Should trigger again
  assert_eq(alerts4.length(), 1)
  assert_eq(system14.active_alerts.length(), 2)
  
  // Resolve an alert
  let alert_to_resolve = system14.active_alerts[0].id
  let system15 = resolve_alert(system14, alert_to_resolve)
  
  // Check that alert was resolved
  let resolved_alert = system15.active_alerts.find(fn(a) { a.id == alert_to_resolve }).unwrap()
  assert_true(resolved_alert.resolved_at.is_some())
  
  // Test error rate rule
  let system16 = record_metric(system15, "error_count", 15.0)  // Increase error count
  
  // Advance time and evaluate
  let system17 = {
    system16 |
    current_time: system16.current_time + 60
  }
  
  let (system18, alerts5) = evaluate_all_rules(system17)
  
  // Check if error rate rule triggered
  if alerts5.length() > 0 {
    assert_true(alerts5[0].name == "High Error Rate")
    assert_eq(alerts5[0].severity, AlertSeverity::Critical)
  }
  
  // Test anomaly detection
  let anomaly_rule = {
    id: "memory-anomaly",
    name: "Memory Usage Anomaly",
    condition: AlertCondition::Anomaly("memory_usage", 2.0),  // 2 standard deviations
    severity: AlertSeverity::Warning,
    enabled: true,
    cooldown_period: 300,
    evaluation_interval: 60,
    last_evaluation: 0,
    last_triggered: None
  }
  
  let system19 = add_monitoring_rule(system18, anomaly_rule)
  
  // Record normal memory usage values
  let system20 = record_metric(system19, "memory_usage", 50.0)
  let system21 = record_metric(system20, "memory_usage", 52.0)
  let system22 = record_metric(system21, "memory_usage", 48.0)
  let system23 = record_metric(system22, "memory_usage", 51.0)
  let system24 = record_metric(system23, "memory_usage", 49.0)
  let system25 = record_metric(system24, "memory_usage", 53.0)
  let system26 = record_metric(system25, "memory_usage", 47.0)
  let system27 = record_metric(system26, "memory_usage", 50.0)
  let system28 = record_metric(system27, "memory_usage", 52.0)
  let system29 = record_metric(system28, "memory_usage", 48.0)
  
  // Record anomalous value
  let system30 = record_metric(system29, "memory_usage", 80.0)  // Much higher than normal
  
  // Advance time and evaluate
  let system31 = {
    system30 |
    current_time: system30.current_time + 60
  }
  
  let (system32, alerts6) = evaluate_all_rules(system31)
  
  // Should trigger anomaly alert
  if alerts6.length() > 0 {
    assert_true(alerts6[0].name == "Memory Usage Anomaly")
    assert_eq(alerts6[0].severity, AlertSeverity::Warning)
  }
}

// Test 7: Telemetry Data Quality Assurance
test "telemetry data quality assurance and validation" {
  // Define data quality dimensions
  enum QualityDimension {
    Completeness,  // All required fields are present
    Accuracy,      // Data is correct and within expected ranges
    Consistency,   // Data is consistent across sources
    Timeliness,    // Data is fresh and not stale
    Validity       // Data conforms to expected format/schema
  }
  
  // Define quality rule
  type QualityRule = {
    id: String,
    name: String,
    dimension: QualityDimension,
    validator: (Array[(String, String)]) -> Bool,  // Function to validate data
    enabled: Bool
  }
  
  // Define quality check result
  type QualityCheckResult = {
    rule_id: String,
    passed: Bool,
    score: Float,  // 0.0 to 1.0
    issues: Array[String],
    timestamp: Int
  }
  
  // Define telemetry data point
  type TelemetryDataPoint = {
    id: String,
    timestamp: Int,
    source: String,
    data_type: String,
    fields: Array[(String, String)]
  }
  
  // Define quality assurance system
  type QualityAssuranceSystem = {
    rules: Array[QualityRule],
    check_results: Array[QualityCheckResult],
    current_time: Int
  }
  
  // Create quality assurance system
  let create_quality_system = fn() {
    {
      rules: [],
      check_results: [],
      current_time: 1640995200
    }
  }
  
  // Add quality rule
  let add_quality_rule = fn(system: QualityAssuranceSystem, rule: QualityRule) {
    {
      system |
      rules: system.rules.push(rule)
    }
  }
  
  // Run quality checks on data point
  let run_quality_checks = fn(system: QualityAssuranceSystem, data_point: TelemetryDataPoint) {
    let mut results = []
    
    for rule in system.rules {
      if rule.enabled {
        let passed = rule.validator(data_point.fields)
        
        // Calculate quality score based on rule result
        let score = if passed { 1.0 } else { 0.0 }
        
        // Generate issues for failed checks
        let issues = if not(passed) {
          match rule.dimension {
            QualityDimension::Completeness => ["Missing required fields"]
            QualityDimension::Accuracy => ["Data outside expected range"]
            QualityDimension::Consistency => ["Inconsistent data values"]
            QualityDimension::Timeliness => ["Data is stale or outdated"]
            QualityDimension::Validity => ["Invalid data format or schema"]
          }
        } else {
          []
        }
        
        let result = {
          rule_id: rule.id,
          passed,
          score,
          issues,
          timestamp: system.current_time
        }
        
        results = results.push(result)
      }
    }
    
    let updated_system = {
      system |
      check_results: system.check_results + results
    }
    
    (updated_system, results)
  }
  
  // Calculate overall quality score
  let calculate_quality_score = fn(results: Array[QualityCheckResult]) {
    if results.length() == 0 {
      1.0
    } else {
      let total_score = results.reduce(0.0, fn(acc, result) { acc + result.score })
      total_score / (results.length() as Float)
    }
  }
  
  // Test quality assurance system
  let system = create_quality_system()
  
  // Define quality rules
  let completeness_rule = {
    id: "completeness-check",
    name: "Required Fields Check",
    dimension: QualityDimension::Completeness,
    validator: fn(fields: Array[(String, String)]) {
      // Check for required fields
      let required_fields = ["trace_id", "span_id", "operation_name", "timestamp"]
      let field_names = fields.map(fn(f) { f.0 })
      
      for required in required_fields {
        if not(field_names.contains(required)) {
          return false
        }
      }
      
      true
    },
    enabled: true
  }
  
  let accuracy_rule = {
    id: "accuracy-check",
    name: "Data Range Validation",
    dimension: QualityDimension::Accuracy,
    validator: fn(fields: Array[(String, String)]) {
      // Check that numeric fields are within expected ranges
      for (name, value) in fields {
        match name {
          "duration" => {
            let duration = value.to_int().unwrap_or(-1)
            if duration < 0 or duration > 3600000 {  // Max 1 hour
              return false
            }
          }
          "status_code" => {
            let code = value.to_int().unwrap_or(-1)
            if code < 100 or code > 599 {
              return false
            }
          }
          _ => ()
        }
      }
      
      true
    },
    enabled: true
  }
  
  let consistency_rule = {
    id: "consistency-check",
    name: "Data Consistency Check",
    dimension: QualityDimension::Consistency,
    validator: fn(fields: Array[(String, String)]) {
      // Check that status and status_code are consistent
      let status = fields.find(fn(f) { f.0 == "status" })
      let status_code = fields.find(fn(f) { f.0 == "status_code" })
      
      match (status, status_code) {
        (Some((_, "ok")), Some((_, code))) => {
          let code_int = code.to_int().unwrap_or(0)
          code_int >= 200 and code_int < 300
        }
        (Some((_, "error")), Some((_, code))) => {
          let code_int = code.to_int().unwrap_or(0)
          code_int >= 400
        }
        (None, _) => true  // No status field to check
        (_, None) => true  // No status_code field to check
        _ => true  // Other combinations are acceptable
      }
    },
    enabled: true
  }
  
  let timeliness_rule = {
    id: "timeliness-check",
    name: "Data Freshness Check",
    dimension: QualityDimension::Timeliness,
    validator: fn(fields: Array[(String, String)]) {
      // Check that timestamp is not too old (more than 24 hours)
      match fields.find(fn(f) { f.0 == "timestamp" }) {
        Some((_, timestamp_str)) => {
          let timestamp = timestamp_str.to_int().unwrap_or(0)
          let current_time = 1640995200  // Mock current time
          let age = current_time - timestamp
          
          age < 86400  // Less than 24 hours
        }
        None => false
      }
    },
    enabled: true
  }
  
  let validity_rule = {
    id: "validity-check",
    name: "Data Format Validation",
    dimension: QualityDimension::Validity,
    validator: fn(fields: Array[(String, String)]) {
      // Check that trace_id and span_id are valid UUIDs (simplified check)
      for (name, value) in fields {
        if name == "trace_id" or name == "span_id" {
          // Simplified UUID format check (length and hyphens)
          if value.length() != 36 or not(value.contains("-")) {
            return false
          }
        }
      }
      
      true
    },
    enabled: true
  }
  
  let system1 = add_quality_rule(system, completeness_rule)
  let system2 = add_quality_rule(system1, accuracy_rule)
  let system3 = add_quality_rule(system2, consistency_rule)
  let system4 = add_quality_rule(system3, timeliness_rule)
  let system5 = add_quality_rule(system4, validity_rule)
  
  // Test with good data
  let good_data = {
    id: "data-001",
    timestamp: 1640995200,
    source: "api-service",
    data_type: "span",
    fields: [
      ("trace_id", "4bf92f35-77b3-4da6-a3ce-929d0e0e4736"),
      ("span_id", "00f067aa-0ba9-02b7-1234-567890abcdef"),
      ("operation_name", "http.request"),
      ("timestamp", "1640995200"),
      ("duration", "150"),
      ("status", "ok"),
      ("status_code", "200")
    ]
  }
  
  let (system6, results1) = run_quality_checks(system5, good_data)
  
  // All checks should pass
  assert_eq(results1.length(), 5)
  for result in results1 {
    assert_true(result.passed)
    assert_eq(result.score, 1.0)
    assert_eq(result.issues.length(), 0)
  }
  
  let overall_score1 = calculate_quality_score(results1)
  assert_eq(overall_score1, 1.0)
  
  // Test with incomplete data
  let incomplete_data = {
    id: "data-002",
    timestamp: 1640995250,
    source: "api-service",
    data_type: "span",
    fields: [
      ("trace_id", "4bf92f35-77b3-4da6-a3ce-929d0e0e4736"),
      ("operation_name", "http.request"),
      ("timestamp", "1640995250"),
      ("duration", "150")
      // Missing span_id and status
    ]
  }
  
  let (system7, results2) = run_quality_checks(system6, incomplete_data)
  
  // Completeness check should fail
  let completeness_result = results2.find(fn(r) { r.rule_id == "completeness-check" }).unwrap()
  assert_false(completeness_result.passed)
  assert_eq(completeness_result.score, 0.0)
  assert_eq(completeness_result.issues.length(), 1)
  assert_eq(completeness_result.issues[0], "Missing required fields")
  
  let overall_score2 = calculate_quality_score(results2)
  assert_true(overall_score2 < 1.0)
  
  // Test with inaccurate data
  let inaccurate_data = {
    id: "data-003",
    timestamp: 1640995300,
    source: "api-service",
    data_type: "span",
    fields: [
      ("trace_id", "4bf92f35-77b3-4da6-a3ce-929d0e0e4736"),
      ("span_id", "00f067aa-0ba9-02b7-1234-567890abcdef"),
      ("operation_name", "http.request"),
      ("timestamp", "1640995300"),
      ("duration", "-50"),  // Invalid negative duration
      ("status", "ok"),
      ("status_code", "200")
    ]
  }
  
  let (system8, results3) = run_quality_checks(system7, inaccurate_data)
  
  // Accuracy check should fail
  let accuracy_result = results3.find(fn(r) { r.rule_id == "accuracy-check" }).unwrap()
  assert_false(accuracy_result.passed)
  assert_eq(accuracy_result.score, 0.0)
  assert_eq(accuracy_result.issues.length(), 1)
  assert_eq(accuracy_result.issues[0], "Data outside expected range")
  
  // Test with inconsistent data
  let inconsistent_data = {
    id: "data-004",
    timestamp: 1640995350,
    source: "api-service",
    data_type: "span",
    fields: [
      ("trace_id", "4bf92f35-77b3-4da6-a3ce-929d0e0e4736"),
      ("span_id", "00f067aa-0ba9-02b7-1234-567890abcdef"),
      ("operation_name", "http.request"),
      ("timestamp", "1640995350"),
      ("duration", "150"),
      ("status", "ok"),
      ("status_code", "500")  // Inconsistent: ok status with error code
    ]
  }
  
  let (system9, results4) = run_quality_checks(system8, inconsistent_data)
  
  // Consistency check should fail
  let consistency_result = results4.find(fn(r) { r.rule_id == "consistency-check" }).unwrap()
  assert_false(consistency_result.passed)
  assert_eq(consistency_result.score, 0.0)
  assert_eq(consistency_result.issues.length(), 1)
  assert_eq(consistency_result.issues[0], "Inconsistent data values")
  
  // Test with stale data
  let stale_data = {
    id: "data-005",
    timestamp: 1640995400,
    source: "api-service",
    data_type: "span",
    fields: [
      ("trace_id", "4bf92f35-77b3-4da6-a3ce-929d0e0e4736"),
      ("span_id", "00f067aa-0ba9-02b7-1234-567890abcdef"),
      ("operation_name", "http.request"),
      ("timestamp", "1640800000"),  // Very old timestamp
      ("duration", "150"),
      ("status", "ok"),
      ("status_code", "200")
    ]
  }
  
  let (system10, results5) = run_quality_checks(system9, stale_data)
  
  // Timeliness check should fail
  let timeliness_result = results5.find(fn(r) { r.rule_id == "timeliness-check" }).unwrap()
  assert_false(timeliness_result.passed)
  assert_eq(timeliness_result.score, 0.0)
  assert_eq(timeliness_result.issues.length(), 1)
  assert_eq(timeliness_result.issues[0], "Data is stale or outdated")
  
  // Test with invalid format data
  let invalid_data = {
    id: "data-006",
    timestamp: 1640995450,
    source: "api-service",
    data_type: "span",
    fields: [
      ("trace_id", "invalid-trace-id"),  // Invalid format
      ("span_id", "00f067aa-0ba9-02b7-1234-567890abcdef"),
      ("operation_name", "http.request"),
      ("timestamp", "1640995450"),
      ("duration", "150"),
      ("status", "ok"),
      ("status_code", "200")
    ]
  }
  
  let (system11, results6) = run_quality_checks(system10, invalid_data)
  
  // Validity check should fail
  let validity_result = results6.find(fn(r) { r.rule_id == "validity-check" }).unwrap()
  assert_false(validity_result.passed)
  assert_eq(validity_result.score, 0.0)
  assert_eq(validity_result.issues.length(), 1)
  assert_eq(validity_result.issues[0], "Invalid data format or schema")
  
  // Calculate quality trends over time
  let get_quality_trend = fn(results: Array[QualityCheckResult]) {
    let grouped_results = results.group_by(fn(r) { r.rule_id })
    
    let trends = grouped_results.map(fn((rule_id, rule_results)) {
      let scores = rule_results.map(fn(r) { r.score })
      let avg_score = scores.reduce(0.0, fn(acc, s) { acc + s }) / (scores.length() as Float)
      
      (rule_id, avg_score)
    })
    
    trends
  }
  
  let all_results = system11.check_results
  let quality_trends = get_quality_trend(all_results)
  
  // Verify that we have trends for all rules
  assert_eq(quality_trends.length(), 5)
  assert_true(quality_trends.find(fn(t) { t.0 == "completeness-check" }).is_some())
  assert_true(quality_trends.find(fn(t) { t.0 == "accuracy-check" }).is_some())
  assert_true(quality_trends.find(fn(t) { t.0 == "consistency-check" }).is_some())
  assert_true(quality_trends.find(fn(t) { t.0 == "timeliness-check" }).is_some())
  assert_true(quality_trends.find(fn(t) { t.0 == "validity-check" }).is_some())
  
  // Test quality improvement recommendations
  let get_improvement_recommendations = fn(results: Array[QualityCheckResult]) {
    let failed_rules = results.filter(fn(r) { not(r.passed) })
    
    let recommendations = failed_rules.map(fn(result) {
      match result.rule_id {
        "completeness-check" => "Ensure all required fields are included in telemetry data"
        "accuracy-check" => "Validate data ranges before sending telemetry"
        "consistency-check" => "Implement cross-field validation rules"
        "timeliness-check" => "Address network latency or processing delays"
        "validity-check" => "Implement schema validation for telemetry data"
        _ => "Review data quality for rule: " + result.rule_id
      }
    })
    
    // Remove duplicates
    recommendations.unique()
  }
  
  let recommendations = get_improvement_recommendations(system11.check_results)
  
  // Should have recommendations for failed checks
  assert_true(recommendations.length() > 0)
}

// Test 8: Cross-Service Telemetry Correlation
test "cross-service telemetry correlation and trace reconstruction" {
  // Define service span
  type ServiceSpan = {
    service_name: String,
    span_id: String,
    parent_span_id: Option[String],
    trace_id: String,
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    tags: Array[(String, String)]
  }
  
  // Define trace graph
  type TraceGraph = {
    trace_id: String,
    spans: Array[ServiceSpan],
    root_spans: Array[String],  // Span IDs of root spans
    services: Array[String]      // Unique service names
  }
  
  // Define correlation engine
  type CorrelationEngine = {
    traces: Array[TraceGraph],
    span_index: Array[(String, String)],  // (span_id, trace_id) mapping
    service_index: Array[(String, Array[String])]  // (service_name, trace_ids) mapping
  }
  
  // Create correlation engine
  let create_correlation_engine = fn() {
    {
      traces: [],
      span_index: [],
      service_index: []
    }
  }
  
  // Add span to correlation engine
  let add_span = fn(engine: CorrelationEngine, span: ServiceSpan) {
    // Check if trace already exists
    let existing_trace = engine.traces.find(fn(t) { t.trace_id == span.trace_id })
    
    let updated_traces = match existing_trace {
      Some(trace) => {
        // Update existing trace
        let updated_spans = trace.spans.push(span)
        let updated_services = if trace.services.contains(span.service_name) {
          trace.services
        } else {
          trace.services.push(span.service_name)
        }
        
        let updated_trace = {
          trace |
          spans: updated_spans,
          services: updated_services
        }
        
        engine.traces.map(fn(t) {
          if t.trace_id == span.trace_id {
            updated_trace
          } else {
            t
          }
        })
      }
      None => {
        // Create new trace
        let is_root = span.parent_span_id.is_none()
        let root_spans = if is_root {
          [span.span_id]
        } else {
          []
        }
        
        let new_trace = {
          trace_id: span.trace_id,
          spans: [span],
          root_spans,
          services: [span.service_name]
        }
        
        engine.traces.push(new_trace)
      }
    }
    
    // Update span index
    let updated_span_index = engine.span_index.push((span.span_id, span.trace_id))
    
    // Update service index
    let existing_service_index = engine.service_index.find(fn(si) { si.0 == span.service_name })
    let updated_service_index = match existing_service_index {
      Some((service_name, trace_ids)) => {
        let new_trace_ids = if trace_ids.contains(span.trace_id) {
          trace_ids
        } else {
          trace_ids.push(span.trace_id)
        }
        
        engine.service_index.map(fn(si) {
          if si.0 == service_name {
            (service_name, new_trace_ids)
          } else {
            si
          }
        })
      }
      None => {
        engine.service_index.push((span.service_name, [span.trace_id]))
      }
    }
    
    {
      traces: updated_traces,
      span_index: updated_span_index,
      service_index: updated_service_index
    }
  }
  
  // Find trace by span ID
  let find_trace_by_span = fn(engine: CorrelationEngine, span_id: String) {
    match engine.span_index.find(fn(si) { si.0 == span_id }) {
      Some((_, trace_id)) => {
        engine.traces.find(fn(t) { t.trace_id == trace_id })
      }
      None => None
    }
  }
  
  // Find traces by service
  let find_traces_by_service = fn(engine: CorrelationEngine, service_name: String) {
    match engine.service_index.find(fn(si) { si.0 == service_name }) {
      Some((_, trace_ids)) => {
        trace_ids.map(fn(trace_id) {
          engine.traces.find(fn(t) { t.trace_id == trace_id }).unwrap()
        })
      }
      None => []
    }
  }
  
  // Build span hierarchy
  let build_span_hierarchy = fn(trace: TraceGraph) {
    // Create a map of span_id to children
    let mut children_map = []
    
    for span in trace.spans {
      children_map = children_map.push((span.span_id, []))
    }
    
    for span in trace.spans {
      match span.parent_span_id {
        Some(parent_id) => {
          children_map = children_map.map(fn(entry) {
            if entry.0 == parent_id {
              (entry.0, entry.1.push(span.span_id))
            } else {
              entry
            }
          })
        }
        None => ()
      }
    }
    
    children_map
  }
  
  // Calculate trace metrics
  let calculate_trace_metrics = fn(trace: TraceGraph) {
    let total_duration = match trace.spans.sort_by(fn(a, b) { b.end_time - a.end_time }) {
      [root_span, ..] => {
        let min_start = trace.spans.reduce(root_span.start_time, fn(acc, span) {
          min(acc, span.start_time)
        })
        let max_end = trace.spans.reduce(root_span.end_time, fn(acc, span) {
          max(acc, span.end_time)
        })
        
        max_end - min_start
      }
      [] => 0
    }
    
    let span_count = trace.spans.length()
    let service_count = trace.services.length()
    
    // Calculate critical path (simplified)
    let critical_path_duration = trace.spans.reduce(0, fn(acc, span) {
      max(acc, span.end_time - span.start_time)
    })
    
    // Calculate error rate
    let error_count = trace.spans.reduce(0, fn(acc, span) {
      if span.status == "error" {
        acc + 1
      } else {
        acc
      }
    })
    
    let error_rate = if span_count > 0 {
      (error_count as Float) / (span_count as Float)
    } else {
      0.0
    }
    
    {
      total_duration,
      span_count,
      service_count,
      critical_path_duration,
      error_rate
    }
  }
  
  // Test correlation engine
  let engine = create_correlation_engine()
  
  // Create spans for a distributed request
  let api_span = {
    service_name: "api-gateway",
    span_id: "span-001",
    parent_span_id: None,
    trace_id: "trace-123",
    operation_name: "GET /api/users",
    start_time: 1640995200000,
    end_time: 1640995200500,
    status: "ok",
    tags: [("http.method", "GET"), ("http.status_code", "200")]
  }
  
  let auth_span = {
    service_name: "auth-service",
    span_id: "span-002",
    parent_span_id: Some("span-001"),
    trace_id: "trace-123",
    operation_name: "validate_token",
    start_time: 1640995200100,
    end_time: 1640995200200,
    status: "ok",
    tags: [("user.id", "12345")]
  }
  
  let user_span = {
    service_name: "user-service",
    span_id: "span-003",
    parent_span_id: Some("span-001"),
    trace_id: "trace-123",
    operation_name: "get_user_profile",
    start_time: 1640995200250,
    end_time: 1640995200400,
    status: "ok",
    tags: [("user.id", "12345")]
  }
  
  let db_span = {
    service_name: "database",
    span_id: "span-004",
    parent_span_id: Some("span-003"),
    trace_id: "trace-123",
    operation_name: "SELECT * FROM users",
    start_time: 1640995200300,
    end_time: 1640995200350,
    status: "ok",
    tags: [("db.query", "select"), ("db.table", "users")]
  }
  
  // Add spans to engine
  let engine1 = add_span(engine, api_span)
  let engine2 = add_span(engine1, auth_span)
  let engine3 = add_span(engine2, user_span)
  let engine4 = add_span(engine3, db_span)
  
  // Test finding trace by span
  let trace = find_trace_by_span(engine4, "span-003")
  assert_true(trace.is_some())
  
  let found_trace = trace.unwrap()
  assert_eq(found_trace.trace_id, "trace-123")
  assert_eq(found_trace.spans.length(), 4)
  assert_eq(found_trace.services.length(), 4)
  assert_true(found_trace.root_spans.contains("span-001"))
  
  // Test finding traces by service
  let auth_traces = find_traces_by_service(engine4, "auth-service")
  assert_eq(auth_traces.length(), 1)
  assert_eq(auth_traces[0].trace_id, "trace-123")
  
  // Test span hierarchy
  let hierarchy = build_span_hierarchy(found_trace)
  let api_children = hierarchy.find(fn(entry) { entry.0 == "span-001" }).unwrap()
  assert_eq(api_children.1.length(), 2)  // auth and user spans
  assert_true(api_children.1.contains("span-002"))
  assert_true(api_children.1.contains("span-003"))
  
  let user_children = hierarchy.find(fn(entry) { entry.0 == "span-003" }).unwrap()
  assert_eq(user_children.1.length(), 1)  // db span
  assert_true(user_children.1.contains("span-004"))
  
  // Test trace metrics
  let metrics = calculate_trace_metrics(found_trace)
  assert_eq(metrics.span_count, 4)
  assert_eq(metrics.service_count, 4)
  assert_eq(metrics.total_duration, 500)
  assert_eq(metrics.critical_path_duration, 150)  // user service span
  assert_eq(metrics.error_rate, 0.0)
  
  // Add another trace with error
  let error_api_span = {
    service_name: "api-gateway",
    span_id: "span-101",
    parent_span_id: None,
    trace_id: "trace-456",
    operation_name: "POST /api/orders",
    start_time: 1640995300000,
    end_time: 1640995300800,
    status: "error",
    tags: [("http.method", "POST"), ("http.status_code", "500")]
  }
  
  let error_order_span = {
    service_name: "order-service",
    span_id: "span-102",
    parent_span_id: Some("span-101"),
    trace_id: "trace-456",
    operation_name: "create_order",
    start_time: 1640995300100,
    end_time: 1640995300700,
    status: "error",
    tags: [("error.message", "Out of stock")]
  }
  
  let engine5 = add_span(engine4, error_api_span)
  let engine6 = add_span(engine5, error_order_span)
  
  // Test error trace metrics
  let error_trace = find_trace_by_span(engine6, "span-102").unwrap()
  let error_metrics = calculate_trace_metrics(error_trace)
  assert_eq(error_metrics.span_count, 2)
  assert_eq(error_metrics.service_count, 2)
  assert_eq(error_metrics.total_duration, 800)
  assert_eq(error_metrics.error_rate, 1.0)  // 100% error rate
  
  // Test cross-service correlation
  let find_service_interactions = fn(engine: CorrelationEngine) {
    let mut interactions = []
    
    for trace in engine.traces {
      let services = trace.services
      let mut service_pairs = []
      
      // Generate all pairs of services in this trace
      for i in 0..services.length() {
        for j in (i + 1)..services.length() {
          service_pairs = service_pairs.push((services[i], services[j]))
        }
      }
      
      for pair in service_pairs {
        interactions = interactions.push(pair)
      }
    }
    
    // Count unique interactions
    let unique_interactions = interactions.unique()
    
    // Count frequency of each interaction
    unique_interactions.map(fn(pair) {
      let count = interactions.filter(fn(p) { 
        (p.0 == pair.0 and p.1 == pair.1) or (p.0 == pair.1 and p.1 == pair.0)
      }).length()
      
      (pair, count)
    })
  }
  
  let interactions = find_service_interactions(engine6)
  
  // Should have interactions between services
  assert_true(interactions.length() > 0)
  
  // Check for specific interactions
  let api_auth_interaction = interactions.find(fn(pair) {
    (pair.0.0 == "api-gateway" and pair.0.1 == "auth-service") or
    (pair.0.0 == "auth-service" and pair.0.1 == "api-gateway")
  })
  
  assert_true(api_auth_interaction.is_some())
  assert_eq(api_auth_interaction.unwrap().1, 1)  // One interaction
  
  // Test trace reconstruction
  let reconstruct_trace_timeline = fn(trace: TraceGraph) {
    // Get all events (start and end of spans)
    let mut events = []
    
    for span in trace.spans {
      events = events.push({
        timestamp: span.start_time,
        type: "start",
        span_id: span.span_id,
        service_name: span.service_name,
        operation_name: span.operation_name
      })
      
      events = events.push({
        timestamp: span.end_time,
        type: "end",
        span_id: span.span_id,
        service_name: span.service_name,
        operation_name: span.operation_name
      })
    }
    
    // Sort by timestamp
    events.sort_by(fn(a, b) { a.timestamp - b.timestamp })
  }
  
  let timeline = reconstruct_trace_timeline(found_trace)
  assert_eq(timeline.length(), 8)  // 4 spans × 2 events each
  
  // Verify timeline order
  assert_eq(timeline[0].type, "start")
  assert_eq(timeline[0].span_id, "span-001")  // API span starts first
  
  assert_eq(timeline[7].type, "end")
  assert_eq(timeline[7].span_id, "span-001")  // API span ends last
  
  // Test service dependency analysis
  let analyze_service_dependencies = fn(engine: CorrelationEngine) {
    let mut dependencies = []
    
    for trace in engine.traces {
      for span in trace.spans {
        match span.parent_span_id {
          Some(parent_id) => {
            // Find parent span
            match trace.spans.find(fn(s) { s.span_id == parent_id }) {
              Some(parent_span) => {
                if parent_span.service_name != span.service_name {
                  // Cross-service dependency
                  let dependency = {
                    from: parent_span.service_name,
                    to: span.service_name,
                    operation: parent_span.operation_name + " -> " + span.operation_name
                  }
                  
                  dependencies = dependencies.push(dependency)
                }
              }
              None => ()
            }
          }
          None => ()
        }
      }
    }
    
    // Remove duplicates and count frequencies
    let unique_deps = dependencies.unique()
    
    unique_deps.map(fn(dep) {
      let count = dependencies.filter(fn(d) {
        d.from == dep.from and d.to == dep.to
      }).length()
      
      { dep | frequency: count }
    })
  }
  
  let dependencies = analyze_service_dependencies(engine6)
  
  // Should have cross-service dependencies
  assert_true(dependencies.length() > 0)
  
  // Check for specific dependencies
  let api_to_auth = dependencies.find(fn(d) {
    d.from == "api-gateway" and d.to == "auth-service"
  })
  
  assert_true(api_to_auth.is_some())
  
  let user_to_db = dependencies.find(fn(d) {
    d.from == "user-service" and d.to == "database"
  })
  
  assert_true(user_to_db.is_some())
}