// Azimuth Premium Data Collection and Validation Test Suite
// Advanced test cases for telemetry data collection and validation functionality

// Test 1: Telemetry Data Collection with Quality Assurance
test "comprehensive telemetry data collection with quality assurance" {
  // Define telemetry data structure
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    service_name: String,
    operation_name: String,
    duration_ms: Int,
    status: String,
    attributes: Array<(String, String)>,
    events: Array[(String, Int, Array[(String, String)])]
  }
  
  // Create data validator
  let validate_telemetry_data = fn(data: TelemetryData) {
    let mut errors = []
    
    // Validate timestamp
    if data.timestamp <= 0 {
      errors = errors.push("Invalid timestamp: must be positive")
    }
    
    // Validate trace ID format (should be like "trace-123456789")
    if data.trace_id.length() != 13 or not(data.trace_id.starts_with("trace-")) {
      errors = errors.push("Invalid trace ID format")
    }
    
    // Validate span ID format (should be like "span-123456789")
    if data.span_id.length() != 13 or not(data.span_id.starts_with("span-")) {
      errors = errors.push("Invalid span ID format")
    }
    
    // Validate service name
    if data.service_name.length() == 0 or data.service_name.length() > 50 {
      errors = errors.push("Invalid service name length")
    }
    
    // Validate operation name
    if data.operation_name.length() == 0 or data.operation_name.length() > 100 {
      errors = errors.push("Invalid operation name length")
    }
    
    // Validate duration
    if data.duration_ms < 0 {
      errors = errors.push("Duration cannot be negative")
    }
    
    // Validate status
    if data.status != "ok" and data.status != "error" and data.status != "timeout" {
      errors = errors.push("Invalid status value")
    }
    
    // Validate attributes
    for (key, value) in data.attributes {
      if key.length() == 0 or key.length() > 255 {
        errors = errors.push("Invalid attribute key length")
      }
      if value.length() > 255 {
        errors = errors.push("Invalid attribute value length")
      }
    }
    
    errors
  }
  
  // Create valid telemetry data
  let valid_data = {
    timestamp: 1640995200,
    trace_id: "trace-123456789",
    span_id: "span-987654321",
    parent_span_id: Some("span-111111111"),
    service_name: "payment-service",
    operation_name: "process_payment",
    duration_ms: 250,
    status: "ok",
    attributes: [
      ("user.id", "user-12345"),
      ("payment.method", "credit_card"),
      ("amount", "99.99")
    ],
    events: [
      ("validation_start", 1640995200, [("step", "card_validation")]),
      ("validation_complete", 1640995225, [("result", "success")])
    ]
  }
  
  // Test valid data validation
  let validation_errors = validate_telemetry_data(valid_data)
  assert_eq(validation_errors.length(), 0)
  
  // Test invalid timestamp
  let invalid_timestamp_data = { valid_data | timestamp: -1 }
  let timestamp_errors = validate_telemetry_data(invalid_timestamp_data)
  assert_eq(timestamp_errors.length(), 1)
  assert_true(timestamp_errors[0].contains("Invalid timestamp"))
  
  // Test invalid trace ID
  let invalid_trace_data = { valid_data | trace_id: "invalid" }
  let trace_errors = validate_telemetry_data(invalid_trace_data)
  assert_eq(trace_errors.length(), 1)
  assert_true(trace_errors[0].contains("Invalid trace ID"))
  
  // Test invalid service name
  let invalid_service_data = { valid_data | service_name: "" }
  let service_errors = validate_telemetry_data(invalid_service_data)
  assert_eq(service_errors.length(), 1)
  assert_true(service_errors[0].contains("Invalid service name"))
  
  // Test multiple validation errors
  let multiple_errors_data = {
    timestamp: -1,
    trace_id: "invalid",
    span_id: "bad",
    parent_span_id: Some("span-111111111"),
    service_name: "",
    operation_name: "process_payment",
    duration_ms: 250,
    status: "ok",
    attributes: [
      ("user.id", "user-12345"),
      ("payment.method", "credit_card"),
      ("amount", "99.99")
    ],
    events: []
  }
  let multiple_errors = validate_telemetry_data(multiple_errors_data)
  assert_true(multiple_errors.length() >= 4)
}

// Test 2: Telemetry Data Sampling Strategy
test "advanced telemetry data sampling strategy implementation" {
  // Define sampling strategy types
  enum SamplingStrategy {
    AlwaysOn
    AlwaysOff
    Probability(Float)  // 0.0 to 1.0
    RateLimit(Int)      // max samples per second
    Adaptive            // based on system load
  }
  
  // Define sampling decision
  type SamplingDecision = {
    should_sample: Bool,
    strategy: SamplingStrategy,
    sample_rate: Float,
    reason: String
  }
  
  // Create sampling decision engine
  let make_sampling_decision = fn(
    strategy: SamplingStrategy,
    trace_id: String,
    current_load: Float,
    samples_per_second: Int
  ) {
    match strategy {
      SamplingStrategy::AlwaysOn => {
        {
          should_sample: true,
          strategy,
          sample_rate: 1.0,
          reason: "Always on sampling"
        }
      }
      SamplingStrategy::AlwaysOff => {
        {
          should_sample: false,
          strategy,
          sample_rate: 0.0,
          reason: "Always off sampling"
        }
      }
      SamplingStrategy::Probability(rate) => {
        // Use trace ID hash for consistent sampling
        let hash = trace_id.length() % 100
        let threshold = (rate * 100.0).to_int()
        let should_sample = hash < threshold
        
        {
          should_sample,
          strategy,
          sample_rate: rate,
          reason: "Probabilistic sampling with rate " + rate.to_string()
        }
      }
      SamplingStrategy::RateLimit(max_samples_per_sec) => {
        let should_sample = samples_per_second < max_samples_per_sec
        
        {
          should_sample,
          strategy,
          sample_rate: if should_sample { 1.0 } else { 0.0 },
          reason: "Rate limited sampling, current: " + samples_per_second.to_string() + "/s"
        }
      }
      SamplingStrategy::Adaptive => {
        // Adaptive sampling based on system load
        let adjusted_rate = if current_load > 0.8 {
          0.1  // Reduce sampling under high load
        } else if current_load > 0.5 {
          0.5  // Moderate sampling under medium load
        } else {
          1.0  // Full sampling under low load
        }
        
        let hash = trace_id.length() % 100
        let threshold = (adjusted_rate * 100.0).to_int()
        let should_sample = hash < threshold
        
        {
          should_sample,
          strategy,
          sample_rate: adjusted_rate,
          reason: "Adaptive sampling based on load " + current_load.to_string()
        }
      }
    }
  }
  
  // Test always on sampling
  let always_on_decision = make_sampling_decision(
    SamplingStrategy::AlwaysOn,
    "trace-123456789",
    0.5,
    10
  )
  assert_true(always_on_decision.should_sample)
  assert_eq(always_on_decision.sample_rate, 1.0)
  
  // Test always off sampling
  let always_off_decision = make_sampling_decision(
    SamplingStrategy::AlwaysOff,
    "trace-123456789",
    0.5,
    10
  )
  assert_false(always_off_decision.should_sample)
  assert_eq(always_off_decision.sample_rate, 0.0)
  
  // Test probabilistic sampling
  let prob_decision = make_sampling_decision(
    SamplingStrategy::Probability(0.5),
    "trace-123456789",  // length 13, 13 % 100 = 13
    0.5,
    10
  )
  // 13 < 50, so should sample
  assert_true(prob_decision.should_sample)
  assert_eq(prob_decision.sample_rate, 0.5)
  
  // Test rate limit sampling
  let rate_limit_decision = make_sampling_decision(
    SamplingStrategy::RateLimit(100),
    "trace-123456789",
    0.5,
    150  // Over the limit
  )
  assert_false(rate_limit_decision.should_sample)
  
  let rate_limit_decision_ok = make_sampling_decision(
    SamplingStrategy::RateLimit(100),
    "trace-123456789",
    0.5,
    50   // Under the limit
  )
  assert_true(rate_limit_decision_ok.should_sample)
  
  // Test adaptive sampling
  let adaptive_low_load = make_sampling_decision(
    SamplingStrategy::Adaptive,
    "trace-123456789",
    0.3,  // Low load
    10
  )
  assert_eq(adaptive_low_load.sample_rate, 1.0)
  
  let adaptive_medium_load = make_sampling_decision(
    SamplingStrategy::Adaptive,
    "trace-123456789",
    0.6,  // Medium load
    10
  )
  assert_eq(adaptive_medium_load.sample_rate, 0.5)
  
  let adaptive_high_load = make_sampling_decision(
    SamplingStrategy::Adaptive,
    "trace-123456789",
    0.9,  // High load
    10
  )
  assert_eq(adaptive_high_load.sample_rate, 0.1)
}

// Test 3: Telemetry Data Aggregation and Batching
test "telemetry data aggregation and batching optimization" {
  // Define batch configuration
  type BatchConfig = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    compression_enabled: Bool
  }
  
  // Define telemetry batch
  type TelemetryBatch = {
    batch_id: String,
    items: Array[String],  // Serialized telemetry data
    created_at: Int,
    size_bytes: Int,
    compressed: Bool
  }
  
  // Create batch processor
  let create_batch_processor = fn(config: BatchConfig) {
    let mut current_batch = []
    let mut batch_start_time = 0
    let mut batch_counter = 0
    
    {
      add_item: fn(item: String, current_timestamp: Int) {
        // Check if we need to start a new batch
        let should_create_new_batch = 
          current_batch.length() >= config.max_batch_size or
          (current_batch.length() > 0 and current_timestamp - batch_start_time >= config.max_wait_time_ms)
        
        if should_create_new_batch and current_batch.length() > 0 {
          let batch_id = "batch-" + batch_counter.to_string()
          let total_size = current_batch.reduce(fn(acc, item) { acc + item.length() }, 0)
          
          let batch = {
            batch_id,
            items: current_batch,
            created_at: batch_start_time,
            size_bytes: total_size,
            compressed: config.compression_enabled
          }
          
          // Reset for next batch
          current_batch = []
          batch_start_time = current_timestamp
          batch_counter = batch_counter + 1
          
          Some(batch)
        } else {
          // Add to current batch
          current_batch = current_batch.push(item)
          if current_batch.length() == 1 {
            batch_start_time = current_timestamp
          }
          
          None
        }
      },
      
      flush: fn(current_timestamp: Int) {
        if current_batch.length() > 0 {
          let batch_id = "batch-" + batch_counter.to_string()
          let total_size = current_batch.reduce(fn(acc, item) { acc + item.length() }, 0)
          
          let batch = {
            batch_id,
            items: current_batch,
            created_at: batch_start_time,
            size_bytes: total_size,
            compressed: config.compression_enabled
          }
          
          current_batch = []
          batch_counter = batch_counter + 1
          
          Some(batch)
        } else {
          None
        }
      }
    }
  }
  
  // Create batch configuration
  let batch_config = {
    max_batch_size: 3,
    max_wait_time_ms: 1000,
    compression_enabled: true
  }
  
  // Create batch processor
  let processor = create_batch_processor(batch_config)
  
  // Test batch creation by size
  let timestamp1 = 1640995200
  let result1 = processor.add_item("item1", timestamp1)
  assert_eq(result1, None)  // No batch yet
  
  let timestamp2 = 1640995200
  let result2 = processor.add_item("item2", timestamp2)
  assert_eq(result2, None)  // No batch yet
  
  let timestamp3 = 1640995200
  let result3 = processor.add_item("item3", timestamp3)
  assert_not_eq(result3, None)  // Should create batch
  
  match result3 {
    Some(batch) => {
      assert_eq(batch.batch_id, "batch-0")
      assert_eq(batch.items.length(), 3)
      assert_eq(batch.size_bytes, 15)  // "item1" + "item2" + "item3"
      assert_true(batch.compressed)
    }
    None => assert_true(false)
  }
  
  // Test batch creation by time
  let timestamp4 = 1640995200
  let result4 = processor.add_item("item4", timestamp4)
  assert_eq(result4, None)  // No batch yet
  
  let timestamp5 = 1640996300  // 1100ms later,超过max_wait_time_ms
  let result5 = processor.add_item("item5", timestamp5)
  assert_not_eq(result5, None)  // Should create batch due to time
  
  match result5 {
    Some(batch) => {
      assert_eq(batch.batch_id, "batch-1")
      assert_eq(batch.items.length(), 2)  // "item4" and "item5"
      assert_true(batch.compressed)
    }
    None => assert_true(false)
  }
  
  // Test flush operation
  let timestamp6 = 1640996300
  let result6 = processor.add_item("item6", timestamp6)
  assert_eq(result6, None)  // No batch yet
  
  let flush_result = processor.flush(1640996300)
  assert_not_eq(flush_result, None)  // Should flush remaining items
  
  match flush_result {
    Some(batch) => {
      assert_eq(batch.batch_id, "batch-2")
      assert_eq(batch.items.length(), 1)  // Only "item6"
      assert_true(batch.compressed)
    }
    None => assert_true(false)
  }
  
  // Test flush when no items
  let empty_flush = processor.flush(1640996300)
  assert_eq(empty_flush, None)
}

// Test 4: Telemetry Data Quality Metrics
test "telemetry data quality metrics and monitoring" {
  // Define quality metrics
  type QualityMetrics = {
    total_records: Int,
    valid_records: Int,
    invalid_records: Int,
    completeness_score: Float,
    accuracy_score: Float,
    consistency_score: Float,
    timeliness_score: Float,
    data_freshness_ms: Int
  }
  
  // Define quality rule
  type QualityRule = {
    name: String,
    description: String,
    validator: (String, String) -> Bool,
    weight: Float
  }
  
  // Create quality rules
  let quality_rules = [
    {
      name: "trace_id_format",
      description: "Trace ID must follow format 'trace-xxxxxxxxx'",
      validator: fn(key, value) {
        if key == "trace_id" {
          value.length() == 13 and value.starts_with("trace-")
        } else {
          true
        }
      },
      weight: 0.2
    },
    {
      name: "timestamp_range",
      description: "Timestamp must be within reasonable range",
      validator: fn(key, value) {
        if key == "timestamp" {
          let timestamp = value.to_int()
          timestamp > 1600000000 and timestamp < 2000000000
        } else {
          true
        }
      },
      weight: 0.2
    },
    {
      name: "service_name_presence",
      description: "Service name must be present and valid",
      validator: fn(key, value) {
        if key == "service_name" {
          value.length() > 0 and value.length() <= 50
        } else {
          true
        }
      },
      weight: 0.15
    },
    {
      name: "duration_non_negative",
      description: "Duration must be non-negative",
      validator: fn(key, value) {
        if key == "duration_ms" {
          value.to_int() >= 0
        } else {
          true
        }
      },
      weight: 0.15
    },
    {
      name: "status_valid_values",
      description: "Status must be one of valid values",
      validator: fn(key, value) {
        if key == "status" {
          value == "ok" or value == "error" or value == "timeout"
        } else {
          true
        }
      },
      weight: 0.15
    },
    {
      name: "attribute_key_format",
      description: "Attribute keys must follow naming conventions",
      validator: fn(key, value) {
        if key.starts_with("attr.") {
          let attr_key = key.substring(5, key.length() - 5)
          attr_key.length() > 0 and not(attr_key.contains(" ")) and attr_key.to_lowercase() == attr_key
        } else {
          true
        }
      },
      weight: 0.15
    }
  ]
  
  // Create quality calculator
  let calculate_quality_metrics = fn(records: Array<Array<(String, String)>>, current_timestamp: Int) {
    let mut total_records = records.length()
    let mut valid_records = 0
    let mut rule_scores = []
    
    // Initialize rule scores
    for rule in quality_rules {
      rule_scores = rule_scores.push((rule.name, 0.0, rule.weight))
    }
    
    // Evaluate each record
    for record in records {
      let mut record_valid = true
      let mut record_rule_scores = []
      
      // Evaluate each rule for this record
      for rule in quality_rules {
        let rule_passed = record.all(fn(pair) {
          rule.validator(pair.0, pair.1)
        })
        
        if rule_passed {
          record_rule_scores = record_rule_scores.push((rule.name, 1.0, rule.weight))
        } else {
          record_rule_scores = record_rule_scores.push((rule.name, 0.0, rule.weight))
          record_valid = false
        }
      }
      
      if record_valid {
        valid_records = valid_records + 1
      }
      
      // Accumulate rule scores
      let mut updated_scores = []
      for (name, score, weight) in rule_scores {
        let record_score = record_rule_scores.find(fn(rs) { rs.0 == name })
        match record_score {
          Some((_, rs_score, _)) => {
            updated_scores = updated_scores.push((name, score + rs_score, weight))
          }
          None => updated_scores = updated_scores.push((name, score, weight))
        }
      }
      rule_scores = updated_scores
    }
    
    // Calculate final scores
    let mut completeness_score = 0.0
    let mut accuracy_score = 0.0
    let mut consistency_score = 0.0
    let mut timeliness_score = 0.0
    
    for (name, score, weight) in rule_scores {
      let normalized_score = if total_records > 0 { score / total_records.to_float() } else { 0.0 }
      let weighted_score = normalized_score * weight
      
      match name {
        "trace_id_format" => completeness_score = completeness_score + weighted_score
        "timestamp_range" => timeliness_score = timeliness_score + weighted_score
        "service_name_presence" => completeness_score = completeness_score + weighted_score
        "duration_non_negative" => accuracy_score = accuracy_score + weighted_score
        "status_valid_values" => accuracy_score = accuracy_score + weighted_score
        "attribute_key_format" => consistency_score = consistency_score + weighted_score
        _ => ()
      }
    }
    
    // Calculate data freshness (average age of records)
    let mut total_age = 0
    for record in records {
      let timestamp_pair = record.find(fn(pair) { pair.0 == "timestamp" })
      match timestamp_pair {
        Some((_, timestamp_str)) => {
          let timestamp = timestamp_str.to_int()
          total_age = total_age + (current_timestamp - timestamp)
        }
        None => ()
      }
    }
    
    let data_freshness_ms = if total_records > 0 { total_age / total_records } else { 0 }
    
    {
      total_records,
      valid_records,
      invalid_records: total_records - valid_records,
      completeness_score,
      accuracy_score,
      consistency_score,
      timeliness_score,
      data_freshness_ms
    }
  }
  
  // Create test records
  let valid_record = [
    ("trace_id", "trace-123456789"),
    ("timestamp", "1640995200"),
    ("service_name", "payment-service"),
    ("duration_ms", "250"),
    ("status", "ok"),
    ("attr.user.id", "user-12345"),
    ("attr.payment.method", "credit_card")
  ]
  
  let invalid_record1 = [
    ("trace_id", "invalid"),  // Invalid format
    ("timestamp", "1640995200"),
    ("service_name", "payment-service"),
    ("duration_ms", "250"),
    ("status", "ok"),
    ("attr.user.id", "user-12345")
  ]
  
  let invalid_record2 = [
    ("trace_id", "trace-123456789"),
    ("timestamp", "invalid"),  // Invalid timestamp
    ("service_name", "payment-service"),
    ("duration_ms", "-100"),   // Negative duration
    ("status", "unknown"),     // Invalid status
    ("attr.invalid key", "value")  // Invalid attribute key
  ]
  
  let records = [valid_record, invalid_record1, invalid_record2]
  let current_timestamp = 1640995300
  
  // Calculate quality metrics
  let metrics = calculate_quality_metrics(records, current_timestamp)
  
  // Verify metrics
  assert_eq(metrics.total_records, 3)
  assert_eq(metrics.valid_records, 1)
  assert_eq(metrics.invalid_records, 2)
  
  // Score should be between 0 and 1
  assert_true(metrics.completeness_score >= 0.0 and metrics.completeness_score <= 1.0)
  assert_true(metrics.accuracy_score >= 0.0 and metrics.accuracy_score <= 1.0)
  assert_true(metrics.consistency_score >= 0.0 and metrics.consistency_score <= 1.0)
  assert_true(metrics.timeliness_score >= 0.0 and metrics.timeliness_score <= 1.0)
  
  // Data freshness should be positive
  assert_true(metrics.data_freshness_ms >= 0)
  
  // With 1 valid record out of 3, scores should be relatively low
  assert_true(metrics.valid_records < metrics.total_records)
}

// Test 5: Telemetry Data Retention and Cleanup
test "telemetry data retention and cleanup policies" {
  // Define retention policy
  type RetentionPolicy = {
    max_age_days: Int,
    max_records: Int,
    cleanup_interval_hours: Int,
    priority_levels: Array[String]
  }
  
  // Define telemetry record with priority
  type TelemetryRecord = {
    id: String,
    timestamp: Int,
    priority: String,
    size_bytes: Int,
    data: String
  }
  
  // Create retention manager
  let create_retention_manager = fn(policy: RetentionPolicy) {
    let mut records = []
    
    {
      add_record: fn(record: TelemetryRecord) {
        records = records.push(record)
      },
      
      cleanup_expired: fn(current_timestamp: Int) {
        let max_age_seconds = policy.max_age_days * 24 * 60 * 60
        let cutoff_timestamp = current_timestamp - max_age_seconds
        
        let before_count = records.length()
        records = records.filter(fn(record) {
          record.timestamp >= cutoff_timestamp
        })
        let after_count = records.length()
        
        before_count - after_count
      },
      
      cleanup_by_count: fn() {
        if records.length() > policy.max_records {
          // Sort by priority and timestamp (keep high priority and recent records)
          let priority_order = policy.priority_levels
          
          let sorted_records = records.sort(fn(a, b) {
            let a_priority_index = priority_order.find_index(fn(p) { p == a.priority })
            let b_priority_index = priority_order.find_index(fn(p) { p == b.priority })
            
            match (a_priority_index, b_priority_index) {
              (Some(ai), Some(bi)) => {
                if ai != bi {
                  ai < bi  // Lower index = higher priority
                } else {
                  a.timestamp > b.timestamp  // Within same priority, keep newer
                }
              }
              (Some(_), None) => true
              (None, Some(_)) => false
              (None, None) => a.timestamp > b.timestamp
            }
          })
          
          let before_count = records.length()
          records = sorted_records.take(policy.max_records)
          let after_count = records.length()
          
          before_count - after_count
        } else {
          0
        }
      },
      
      get_storage_stats: fn() {
        let total_records = records.length()
        let total_size = records.reduce(fn(acc, record) { acc + record.size_bytes }, 0)
        let priority_counts = []
        
        for priority in policy.priority_levels {
          let count = records.count(fn(record) { record.priority == priority })
          priority_counts = priority_counts.push((priority, count))
        }
        
        let oldest_timestamp = records.reduce(fn(acc, record) {
          if record.timestamp < acc { record.timestamp } else { acc }
        }, 9999999999)
        
        let newest_timestamp = records.reduce(fn(acc, record) {
          if record.timestamp > acc { record.timestamp } else { acc }
        }, 0)
        
        {
          total_records,
          total_size,
          priority_counts,
          oldest_timestamp,
          newest_timestamp
        }
      }
    }
  }
  
  // Create retention policy
  let retention_policy = {
    max_age_days: 30,
    max_records: 5,
    cleanup_interval_hours: 24,
    priority_levels: ["critical", "high", "medium", "low"]
  }
  
  // Create retention manager
  let manager = create_retention_manager(retention_policy)
  
  // Add test records with different priorities and ages
  let base_timestamp = 1640995200  // 2022-01-01
  
  let records = [
    {
      id: "record-1",
      timestamp: base_timestamp,
      priority: "critical",
      size_bytes: 100,
      data: "critical data 1"
    },
    {
      id: "record-2",
      timestamp: base_timestamp + 86400,  // 1 day later
      priority: "high",
      size_bytes: 200,
      data: "high priority data"
    },
    {
      id: "record-3",
      timestamp: base_timestamp + 172800,  // 2 days later
      priority: "medium",
      size_bytes: 150,
      data: "medium priority data"
    },
    {
      id: "record-4",
      timestamp: base_timestamp + 259200,  // 3 days later
      priority: "low",
      size_bytes: 120,
      data: "low priority data"
    },
    {
      id: "record-5",
      timestamp: base_timestamp + 345600,  // 4 days later
      priority: "medium",
      size_bytes: 180,
      data: "medium priority data 2"
    },
    {
      id: "record-6",
      timestamp: base_timestamp + 432000,  // 5 days later
      priority: "low",
      size_bytes: 130,
      data: "low priority data 2"
    },
    {
      id: "record-7",
      timestamp: base_timestamp - 86400 * 35,  // 35 days ago (expired)
      priority: "high",
      size_bytes: 250,
      data: "expired high priority data"
    }
  ]
  
  // Add all records
  for record in records {
    manager.add_record(record)
  }
  
  // Get initial stats
  let initial_stats = manager.get_storage_stats()
  assert_eq(initial_stats.total_records, 7)
  assert_eq(initial_stats.total_size, 1130)
  
  // Test cleanup by age (remove records older than 30 days)
  let current_timestamp = base_timestamp + 518400  // 6 days after base
  let expired_removed = manager.cleanup_expired(current_timestamp)
  assert_eq(expired_removed, 1)  // Should remove record-7
  
  let after_age_cleanup_stats = manager.get_storage_stats()
  assert_eq(after_age_cleanup_stats.total_records, 6)
  assert_eq(after_age_cleanup_stats.total_size, 880)
  
  // Test cleanup by count (keep only 5 most important records)
  let count_removed = manager.cleanup_by_count()
  assert_eq(count_removed, 1)  // Should remove 1 record to get to 5
  
  let final_stats = manager.get_storage_stats()
  assert_eq(final_stats.total_records, 5)
  
  // Verify priority ordering (critical, high, medium, medium, low)
  let expected_priorities = ["critical", "high", "medium", "medium", "low"]
  let actual_priorities = final_stats.priority_counts.map(fn(pair) { pair.0 })
  assert_eq(actual_priorities, expected_priorities)
  
  // Verify that lowest priority oldest record was removed
  assert_eq(final_stats.priority_counts[3].0, "low")
  assert_eq(final_stats.priority_counts[3].1, 1)  // Only one low priority record remains
}