// Azimuth Adaptive Sampling Strategy Tests
// This file contains test cases for adaptive telemetry sampling strategies

// Test 1: Dynamic Rate-Based Sampling
test "dynamic rate-based sampling" {
  // Define sampling rate adjustment factors
  enum RateAdjustmentFactor {
    Increase(Float)    // Increase rate by factor
    Decrease(Float)    // Decrease rate by factor
    Set(Float)         // Set exact rate
    NoChange
  }
  
  // Define rate-based sampler configuration
  type RateBasedSampler = {
    current_rate: Float,        // Current sampling rate (0.0 to 1.0)
    min_rate: Float,           // Minimum allowed rate
    max_rate: Float,           // Maximum allowed rate
    adjustment_window: Int,    // Time window for adjustments (seconds)
    target_throughput: Int,    // Target samples per second
    current_throughput: Int    // Current samples per second
  }
  
  // Define sampling decision
  type SamplingDecision = {
    should_sample: Bool,
    updated_rate: Float,
    reason: String
  }
  
  // Make sampling decision based on current rate
  let make_sampling_decision = fn(sampler: RateBasedSampler, trace_id: String) {
    // Simple deterministic sampling based on trace ID hash
    let hash = trace_id.chars().fold(0, fn(acc, char) { 
      (acc * 31 + char.to_int()) % 10000 
    })
    let normalized = (hash as Float) / 10000.0
    
    let should_sample = normalized <= sampler.current_rate
    
    // Adjust rate based on throughput
    let rate_adjustment = if sampler.current_throughput < sampler.target_throughput {
      // Need higher throughput, increase rate
      let factor = 1.0 + ((sampler.target_throughput - sampler.current_throughput) as Float) / (sampler.target_throughput as Float)
      RateAdjustmentFactor::Increase(factor.min(2.0))  // Cap at 2x increase
    } else if sampler.current_throughput > sampler.target_throughput * 2 {
      // Too much throughput, decrease rate
      let factor = (sampler.target_throughput as Float) / (sampler.current_throughput as Float)
      RateAdjustmentFactor::Decrease(factor.max(0.5))  // Cap at 50% decrease
    } else {
      RateAdjustmentFactor::NoChange
    }
    
    let updated_rate = match rate_adjustment {
      RateAdjustmentFactor::Increase(factor) => {
        (sampler.current_rate * factor).min(sampler.max_rate)
      }
      RateAdjustmentFactor::Decrease(factor) => {
        (sampler.current_rate * factor).max(sampler.min_rate)
      }
      RateAdjustmentFactor::Set(rate) => {
        rate.max(sampler.min_rate).min(sampler.max_rate)
      }
      RateAdjustmentFactor::NoChange => {
        sampler.current_rate
      }
    }
    
    let reason = match rate_adjustment {
      RateAdjustmentFactor::Increase(_) => "throughput_too_low"
      RateAdjustmentFactor::Decrease(_) => "throughput_too_high"
      RateAdjustmentFactor::Set(_) => "manual_adjustment"
      RateAdjustmentFactor::NoChange => "optimal_throughput"
    }
    
    {
      should_sample: should_sample,
      updated_rate: updated_rate,
      reason: reason
    }
  }
  
  // Update sampler with new throughput data
  let update_sampler_throughput = fn(sampler: RateBasedSampler, new_throughput: Int) {
    { sampler | current_throughput: new_throughput }
  }
  
  // Test rate-based sampling
  let sampler = {
    current_rate: 0.1,        // 10% sampling rate
    min_rate: 0.01,           // 1% minimum
    max_rate: 0.5,            // 50% maximum
    adjustment_window: 60,    // 1 minute
    target_throughput: 100,   // 100 samples per second
    current_throughput: 50    // Currently 50 samples per second
  }
  
  // Test sampling decision with low throughput
  let decision1 = make_sampling_decision(sampler, "trace-12345")
  assert_eq(decision1.reason, "throughput_too_low")
  assert_true(decision1.updated_rate > sampler.current_rate)  // Rate should increase
  
  // Test sampling decision with optimal throughput
  let optimal_sampler = update_sampler_throughput(sampler, 100)
  let decision2 = make_sampling_decision(optimal_sampler, "trace-67890")
  assert_eq(decision2.reason, "optimal_throughput")
  assert_eq(decision2.updated_rate, optimal_sampler.current_rate)  // Rate unchanged
  
  // Test sampling decision with high throughput
  let high_throughput_sampler = update_sampler_throughput(sampler, 250)
  let decision3 = make_sampling_decision(high_throughput_sampler, "trace-11111")
  assert_eq(decision3.reason, "throughput_too_high")
  assert_true(decision3.updated_rate < high_throughput_sampler.current_rate)  // Rate should decrease
  
  // Test rate boundaries
  let min_rate_sampler = { sampler | current_rate: 0.01, current_throughput: 10 }
  let min_decision = make_sampling_decision(min_rate_sampler, "trace-min")
  assert_eq(min_decision.updated_rate, 0.01)  // Should not go below minimum
  
  let max_rate_sampler = { sampler | current_rate: 0.5, current_throughput: 1000 }
  let max_decision = make_sampling_decision(max_rate_sampler, "trace-max")
  assert_eq(max_decision.updated_rate, 0.5)  // Should not go above maximum
  
  // Test sampling consistency
  let consistent_decisions = fn(sampler: RateBasedSampler, trace_id: String, count: Int) {
    let mut sample_count = 0
    let mut total_rate = 0.0
    
    for i in 0..count {
      let decision = make_sampling_decision(sampler, trace_id)
      if decision.should_sample {
        sample_count = sample_count + 1
      }
      total_rate = total_rate + decision.updated_rate
    }
    
    {
      sample_count: sample_count,
      sample_rate: (sample_count as Float) / (count as Float),
      average_rate: total_rate / (count as Float)
    }
  }
  
  let consistency_results = consistent_decisions(sampler, "trace-consistent", 1000)
  assert_eq(consistency_results.sample_count, 1000)  // Same trace ID should always get same decision
  assert_eq(consistency_results.sample_rate, 1.0)
}

// Test 2: Attribute-Based Adaptive Sampling
test "attribute-based adaptive sampling" {
  // Define sampling priority levels
  enum PriorityLevel {
    Critical    // Always sample
    High        // High sampling rate
    Medium      // Medium sampling rate
    Low         // Low sampling rate
    Minimal     // Minimal sampling rate
  }
  
  // Define attribute-based sampling rule
  type AttributeRule = {
    attribute_name: String,
    pattern: String,           // Pattern to match
    priority: PriorityLevel,
    sampling_rate: Float
  }
  
  // Define attribute-based sampler
  type AttributeSampler = {
    rules: Array[AttributeRule],
    default_rate: Float,
    cache: Map[String, PriorityLevel]  // Cache for previously evaluated traces
  }
  
  // Define sampling result
  type AttributeSamplingResult = {
    should_sample: Bool,
    applied_rate: Float,
    matched_rule: Option[String],
    priority: PriorityLevel
  }
  
  // Evaluate trace attributes against rules
  let evaluate_trace_priority = fn(attributes: Array[(String, String)], sampler: AttributeSampler) {
    let mut highest_priority = PriorityLevel::Minimal
    let mut matched_rule = None
    let mut applied_rate = sampler.default_rate
    
    for rule in sampler.rules {
      match attributes.find_fn(a) { a.0 == rule.attribute_name } {
        Some((key, value)) => {
          if value.contains(rule.pattern) {
            // Check if this rule has higher priority
            let is_higher = match (highest_priority, rule.priority) {
              (PriorityLevel::Minimal, _) => true
              (PriorityLevel::Low, PriorityLevel::Medium | PriorityLevel::High | PriorityLevel::Critical) => true
              (PriorityLevel::Medium, PriorityLevel::High | PriorityLevel::Critical) => true
              (PriorityLevel::High, PriorityLevel::Critical) => true
              _ => false
            }
            
            if is_higher {
              highest_priority = rule.priority
              matched_rule = Some(rule.attribute_name + "=" + rule.pattern)
              applied_rate = rule.sampling_rate
            }
          }
        }
        None => {}  // Attribute not present
      }
    }
    
    (highest_priority, matched_rule, applied_rate)
  }
  
  // Make attribute-based sampling decision
  let make_attribute_sampling_decision = fn(sampler: AttributeSampler, trace_id: String, attributes: Array[(String, String)>) {
    // Check cache first
    let cached_priority = match Map::get(sampler.cache, trace_id) {
      Some(priority) => priority
      None => {
        let (priority, _, _) = evaluate_trace_priority(attributes, sampler)
        priority
      }
    }
    
    let (priority, matched_rule, applied_rate) = evaluate_trace_priority(attributes, sampler)
    
    // Determine sampling based on priority
    let should_sample = match priority {
      PriorityLevel::Critical => true
      PriorityLevel::High => {
        // Use deterministic sampling with high rate
        let hash = trace_id.chars().fold(0, fn(acc, char) { 
          (acc * 31 + char.to_int()) % 1000 
        })
        (hash as Float) / 1000.0 <= applied_rate
      }
      PriorityLevel::Medium => {
        let hash = trace_id.chars().fold(0, fn(acc, char) { 
          (acc * 31 + char.to_int()) % 1000 
        })
        (hash as Float) / 1000.0 <= applied_rate
      }
      PriorityLevel::Low => {
        let hash = trace_id.chars().fold(0, fn(acc, char) { 
          (acc * 31 + char.to_int()) % 1000 
        })
        (hash as Float) / 1000.0 <= applied_rate
      }
      PriorityLevel::Minimal => {
        let hash = trace_id.chars().fold(0, fn(acc, char) { 
          (acc * 31 + char.to_int()) % 1000 
        })
        (hash as Float) / 1000.0 <= applied_rate
      }
    }
    
    {
      should_sample: should_sample,
      applied_rate: applied_rate,
      matched_rule: matched_rule,
      priority: priority
    }
  }
  
  // Test attribute-based sampling
  let sampler = {
    rules: [
      {
        attribute_name: "service",
        pattern: "payment",
        priority: PriorityLevel::High,
        sampling_rate: 0.8
      },
      {
        attribute_name: "status",
        pattern: "error",
        priority: PriorityLevel::Critical,
        sampling_rate: 1.0
      },
      {
        attribute_name: "environment",
        pattern: "production",
        priority: PriorityLevel::Medium,
        sampling_rate: 0.3
      },
      {
        attribute_name: "user_tier",
        pattern: "premium",
        priority: PriorityLevel::High,
        sampling_rate: 0.7
      }
    ],
    default_rate: 0.05,
    cache: Map::empty()
  }
  
  // Test critical priority (error status)
  let error_attributes = [
    ("service", "user-api"),
    ("status", "error"),
    ("environment", "production")
  ]
  
  let error_decision = make_attribute_sampling_decision(sampler, "trace-error-123", error_attributes)
  assert_true(error_decision.should_sample)
  assert_eq(error_decision.applied_rate, 1.0)
  assert_eq(error_decision.priority, PriorityLevel::Critical)
  match error_decision.matched_rule {
    Some(rule) => assert_eq(rule, "status=error")
    None => assert_true(false)
  }
  
  // Test high priority (payment service)
  let payment_attributes = [
    ("service", "payment"),
    ("status", "success"),
    ("environment", "production")
  ]
  
  let payment_decision = make_attribute_sampling_decision(sampler, "trace-payment-456", payment_attributes)
  assert_eq(payment_decision.applied_rate, 0.8)
  assert_eq(payment_decision.priority, PriorityLevel::High)
  match payment_decision.matched_rule {
    Some(rule) => assert_eq(rule, "service=payment")
    None => assert_true(false)
  }
  
  // Test medium priority (production environment)
  let prod_attributes = [
    ("service", "user-api"),
    ("status", "success"),
    ("environment", "production")
  ]
  
  let prod_decision = make_attribute_sampling_decision(sampler, "trace-prod-789", prod_attributes)
  assert_eq(prod_decision.applied_rate, 0.3)
  assert_eq(prod_decision.priority, PriorityLevel::Medium)
  match prod_decision.matched_rule {
    Some(rule) => assert_eq(rule, "environment=production")
    None => assert_true(false)
  }
  
  // Test default rate (no matching rules)
  let default_attributes = [
    ("service", "internal-api"),
    ("status", "success"),
    ("environment", "development")
  ]
  
  let default_decision = make_attribute_sampling_decision(sampler, "trace-default-012", default_attributes)
  assert_eq(default_decision.applied_rate, 0.05)
  assert_eq(default_decision.priority, PriorityLevel::Minimal)
  assert_eq(default_decision.matched_rule, None)
  
  // Test rule precedence (critical > high > medium)
  let premium_error_attributes = [
    ("service", "payment"),
    ("user_tier", "premium"),
    ("status", "error"),
    ("environment", "production")
  ]
  
  let precedence_decision = make_attribute_sampling_decision(sampler, "trace-precedence-345", premium_error_attributes)
  assert_true(precedence_decision.should_sample)
  assert_eq(precedence_decision.applied_rate, 1.0)  // Critical rate takes precedence
  assert_eq(precedence_decision.priority, PriorityLevel::Critical)
}

// Test 3: Probabilistic Adaptive Sampling with Feedback
test "probabilistic adaptive sampling with feedback" {
  // Define sampling feedback metrics
  type SamplingMetrics = {
    total_requests: Int,
    sampled_requests: Int,
    dropped_requests: Int,
    error_rate: Float,
    average_latency: Float,
    resource_usage: Float
  }
  
  // Define probabilistic sampler with feedback
  type ProbabilisticSampler = {
    base_probability: Float,
    current_probability: Float,
    min_probability: Float,
    max_probability: Float,
    adjustment_factor: Float,
    metrics: SamplingMetrics,
    feedback_window: Int
  }
  
  // Define sampling feedback
  type SamplingFeedback = {
    error_rate_threshold: Float,
    latency_threshold: Float,
    resource_threshold: Float,
    target_sample_rate: Float
  }
  
  // Update sampler metrics
  let update_metrics = fn(sampler: ProbabilisticSampler, new_metrics: SamplingMetrics) {
    { sampler | metrics: new_metrics }
  }
  
  // Adjust sampling probability based on feedback
  let adjust_sampling_probability = fn(sampler: ProbabilisticSampler, feedback: SamplingFeedback) {
    let metrics = sampler.metrics
    let mut adjustment = 0.0
    let mut reasons = []
    
    // Adjust based on error rate
    if metrics.error_rate > feedback.error_rate_threshold {
      adjustment = adjustment - 0.1  // Decrease sampling rate
      reasons = reasons.push("high_error_rate")
    } else if metrics.error_rate < feedback.error_rate_threshold / 2 {
      adjustment = adjustment + 0.05  // Increase sampling rate
      reasons = reasons.push("low_error_rate")
    }
    
    // Adjust based on latency
    if metrics.average_latency > feedback.latency_threshold {
      adjustment = adjustment - 0.1  // Decrease sampling rate
      reasons = reasons.push("high_latency")
    } else if metrics.average_latency < feedback.latency_threshold / 2 {
      adjustment = adjustment + 0.05  // Increase sampling rate
      reasons = reasons.push("low_latency")
    }
    
    // Adjust based on resource usage
    if metrics.resource_usage > feedback.resource_threshold {
      adjustment = adjustment - 0.15  // Decrease sampling rate
      reasons = reasons.push("high_resource_usage")
    } else if metrics.resource_usage < feedback.resource_threshold / 2 {
      adjustment = adjustment + 0.05  // Increase sampling rate
      reasons = reasons.push("low_resource_usage")
    }
    
    // Apply adjustment with bounds
    let new_probability = (sampler.current_probability + adjustment)
      .max(sampler.min_probability)
      .min(sampler.max_probability)
    
    {
      updated_probability: new_probability,
      adjustment: adjustment,
      reasons: reasons
    }
  }
  
  // Make probabilistic sampling decision
  let make_probabilistic_decision = fn(sampler: ProbabilisticSampler, trace_id: String) {
    // Generate deterministic hash for consistent sampling
    let hash = trace_id.chars().fold(0, fn(acc, char) { 
      (acc * 31 + char.to_int()) % 10000 
    })
    let normalized = (hash as Float) / 10000.0
    
    let should_sample = normalized <= sampler.current_probability
    
    {
      should_sample: should_sample,
      probability_used: sampler.current_probability,
      trace_hash: hash
    }
  }
  
  // Test probabilistic sampling with feedback
  let sampler = {
    base_probability: 0.1,
    current_probability: 0.1,
    min_probability: 0.01,
    max_probability: 0.5,
    adjustment_factor: 0.1,
    metrics: {
      total_requests: 1000,
      sampled_requests: 100,
      dropped_requests: 900,
      error_rate: 0.05,
      average_latency: 100.0,
      resource_usage: 0.6
    },
    feedback_window: 60
  }
  
  let feedback = {
    error_rate_threshold: 0.1,
    latency_threshold: 200.0,
    resource_threshold: 0.8,
    target_sample_rate: 0.15
  }
  
  // Test initial sampling decision
  let decision1 = make_probabilistic_decision(sampler, "trace-prob-123")
  assert_eq(decision1.probability_used, 0.1)
  assert_eq(decision1.trace_hash >= 0 && decision1.trace_hash < 10000, true)
  
  // Test probability adjustment with good metrics
  let good_metrics = {
    total_requests: 1000,
    sampled_requests: 100,
    dropped_requests: 900,
    error_rate: 0.02,  // Below threshold
    average_latency: 80.0,  // Below threshold
    resource_usage: 0.3   // Below threshold
  }
  
  let good_sampler = update_metrics(sampler, good_metrics)
  let adjustment1 = adjust_sampling_probability(good_sampler, feedback)
  assert_true(adjustment1.adjustment > 0)  // Should increase probability
  assert_true(adjustment1.reasons.length() >= 2)  // Multiple reasons for increase
  
  // Test probability adjustment with bad metrics
  let bad_metrics = {
    total_requests: 1000,
    sampled_requests: 100,
    dropped_requests: 900,
    error_rate: 0.15,  // Above threshold
    average_latency: 250.0,  // Above threshold
    resource_usage: 0.9   // Above threshold
  }
  
  let bad_sampler = update_metrics(sampler, bad_metrics)
  let adjustment2 = adjust_sampling_probability(bad_sampler, feedback)
  assert_true(adjustment2.adjustment < 0)  // Should decrease probability
  assert_eq(adjustment2.reasons.length(), 3)  // All reasons for decrease
  
  // Test probability boundaries
  let min_sampler = { sampler | current_probability: 0.01 }
  let min_adjustment = adjust_sampling_probability(min_sampler, feedback)
  assert_eq(min_adjustment.updated_probability, 0.01)  // Should not go below minimum
  
  let max_sampler = { sampler | current_probability: 0.5 }
  let max_adjustment = adjust_sampling_probability(max_sampler, feedback)
  assert_eq(max_adjustment.updated_probability, 0.5)  // Should not go above maximum
  
  // Test sampling distribution
  let test_sampling_distribution = fn(sampler: ProbabilisticSampler, trace_ids: Array[String]) {
    let mut sampled_count = 0
    let mut total_hash = 0
    
    for trace_id in trace_ids {
      let decision = make_probabilistic_decision(sampler, trace_id)
      if decision.should_sample {
        sampled_count = sampled_count + 1
      }
      total_hash = total_hash + decision.trace_hash
    }
    
    let actual_rate = (sampled_count as Float) / (trace_ids.length() as Float)
    
    {
      sampled_count: sampled_count,
      total_traces: trace_ids.length(),
      actual_rate: actual_rate,
      expected_rate: sampler.current_probability,
      rate_error: (actual_rate - sampler.current_probability).abs()
    }
  }
  
  let test_trace_ids = []
  let mut i = 0
  while i < 1000 {
    test_trace_ids = test_trace_ids.push("trace-dist-" + i.to_string())
    i = i + 1
  }
  
  let distribution_results = test_sampling_distribution(sampler, test_trace_ids)
  assert_eq(distribution_results.total_traces, 1000)
  assert_eq(distribution_results.expected_rate, 0.1)
  assert_true(distribution_results.rate_error < 0.05)  // Within 5% error
}

// Test 4: Multi-Layer Adaptive Sampling
test "multi-layer adaptive sampling" {
  // Define sampling layer types
  enum LayerType {
    Global      // Global sampling across all services
    Service     // Service-specific sampling
    Operation   // Operation-specific sampling
    User        // User-specific sampling
  }
  
  // Define sampling layer
  type SamplingLayer = {
    layer_type: LayerType,
    name: String,
    sampling_rate: Float,
    min_rate: Float,
    max_rate: Float,
    priority: Int  // Higher priority layers override lower ones
  }
  
  // Define multi-layer sampler
  type MultiLayerSampler = {
    layers: Array[SamplingLayer],
    fallback_rate: Float,
    layer_cache: Map[String, Float]  // Cache for computed rates
  }
  
  // Define multi-layer sampling result
  type MultiLayerResult = {
    should_sample: Bool,
    applied_layers: Array[String],
    final_rate: Float,
    sampling_path: Array[String>
  }
  
  // Get effective sampling rate for a trace
  let get_effective_sampling_rate = fn(sampler: MultiLayerSampler, trace_id: String, attributes: Array[(String, String)>) {
    let mut applied_layers = []
    let mut sampling_path = []
    let mut effective_rate = sampler.fallback_rate
    
    // Sort layers by priority (highest first)
    let sorted_layers = sampler.layers.sort_by(fn(a, b) { 
      if a.priority > b.priority { -1 } 
      else if a.priority < b.priority { 1 } 
      else { 0 } 
    })
    
    for layer in sorted_layers {
      let layer_applies = match layer.layer_type {
        LayerType::Global => true
        LayerType::Service => {
          match attributes.find_fn(a) { a.0 == "service" } {
            Some((_, service_name)) => service_name == layer.name
            None => false
          }
        }
        LayerType::Operation => {
          match attributes.find_fn(a) { a.0 == "operation" } {
            Some((_, operation_name)) => operation_name == layer.name
            None => false
          }
        }
        LayerType::User => {
          match attributes.find_fn(a) { a.0 == "user_id" } {
            Some((_, user_id)) => user_id == layer.name
            None => false
          }
        }
      }
      
      if layer_applies {
        applied_layers = applied_layers.push(layer.name)
        sampling_path = sampling_path.push(layer.layer_type.to_string() + ":" + layer.name)
        effective_rate = layer.sampling_rate
        
        // High priority layers override, so break after first match
        break
      }
    }
    
    {
      effective_rate: effective_rate,
      applied_layers: applied_layers,
      sampling_path: sampling_path
    }
  }
  
  // Make multi-layer sampling decision
  let make_multi_layer_decision = fn(sampler: MultiLayerSampler, trace_id: String, attributes: Array[(String, String)>) {
    let rate_info = get_effective_sampling_rate(sampler, trace_id, attributes)
    
    // Generate deterministic hash for consistent sampling
    let hash = trace_id.chars().fold(0, fn(acc, char) { 
      (acc * 31 + char.to_int()) % 10000 
    })
    let normalized = (hash as Float) / 10000.0
    
    let should_sample = normalized <= rate_info.effective_rate
    
    {
      should_sample: should_sample,
      applied_layers: rate_info.applied_layers,
      final_rate: rate_info.effective_rate,
      sampling_path: rate_info.sampling_path
    }
  }
  
  // Test multi-layer sampling
  let sampler = {
    layers: [
      {
        layer_type: LayerType::User,
        name: "premium-user-123",
        sampling_rate: 1.0,
        min_rate: 0.5,
        max_rate: 1.0,
        priority: 100  // Highest priority
      },
      {
        layer_type: LayerType::Operation,
        name: "payment",
        sampling_rate: 0.8,
        min_rate: 0.1,
        max_rate: 1.0,
        priority: 80
      },
      {
        layer_type: LayerType::Service,
        name: "payment-service",
        sampling_rate: 0.5,
        min_rate: 0.05,
        max_rate: 0.9,
        priority: 60
      },
      {
        layer_type: LayerType::Global,
        name: "default",
        sampling_rate: 0.1,
        min_rate: 0.01,
        max_rate: 0.3,
        priority: 40  // Lowest priority
      }
    ],
    fallback_rate: 0.05,
    layer_cache: Map::empty()
  }
  
  // Test user layer (highest priority)
  let premium_user_attributes = [
    ("service", "payment-service"),
    ("operation", "payment"),
    ("user_id", "premium-user-123"),
    ("environment", "production")
  ]
  
  let user_decision = make_multi_layer_decision(sampler, "trace-user-456", premium_user_attributes)
  assert_eq(user_decision.final_rate, 1.0)  // User layer rate
  assert_eq(user_decision.applied_layers.length(), 1)
  assert_eq(user_decision.applied_layers[0], "premium-user-123")
  assert_eq(user_decision.sampling_path[0], "User:premium-user-123")
  
  // Test operation layer (second priority)
  let payment_operation_attributes = [
    ("service", "user-service"),
    ("operation", "payment"),
    ("environment", "production")
  ]
  
  let operation_decision = make_multi_layer_decision(sampler, "trace-operation-789", payment_operation_attributes)
  assert_eq(operation_decision.final_rate, 0.8)  // Operation layer rate
  assert_eq(operation_decision.applied_layers.length(), 1)
  assert_eq(operation_decision.applied_layers[0], "payment")
  assert_eq(operation_decision.sampling_path[0], "Operation:payment")
  
  // Test service layer (third priority)
  let payment_service_attributes = [
    ("service", "payment-service"),
    ("operation", "query"),
    ("environment", "production")
  ]
  
  let service_decision = make_multi_layer_decision(sampler, "trace-service-012", payment_service_attributes)
  assert_eq(service_decision.final_rate, 0.5)  // Service layer rate
  assert_eq(service_decision.applied_layers.length(), 1)
  assert_eq(service_decision.applied_layers[0], "payment-service")
  assert_eq(service_decision.sampling_path[0], "Service:payment-service")
  
  // Test global layer (lowest priority)
  let other_service_attributes = [
    ("service", "analytics-service"),
    ("operation", "report"),
    ("environment", "production")
  ]
  
  let global_decision = make_multi_layer_decision(sampler, "trace-global-345", other_service_attributes)
  assert_eq(global_decision.final_rate, 0.1)  // Global layer rate
  assert_eq(global_decision.applied_layers.length(), 1)
  assert_eq(global_decision.applied_layers[0], "default")
  assert_eq(global_decision.sampling_path[0], "Global:default")
  
  // Test fallback (no matching layers)
  let fallback_attributes = [
    ("service", "unknown-service"),
    ("operation", "unknown")
  ]
  
  let fallback_decision = make_multi_layer_decision(sampler, "trace-fallback-678", fallback_attributes)
  assert_eq(fallback_decision.final_rate, 0.05)  // Fallback rate
  assert_eq(fallback_decision.applied_layers.length(), 0)
  assert_eq(fallback_decision.sampling_path.length(), 0)
  
  // Test layer precedence and sampling consistency
  let test_layer_precedence = fn(sampler: MultiLayerSampler, trace_count: Int) {
    let mut user_samples = 0
    let mut operation_samples = 0
    let mut service_samples = 0
    let mut global_samples = 0
    let mut fallback_samples = 0
    
    let mut i = 0
    while i < trace_count {
      let trace_id = "trace-precedence-" + i.to_string()
      
      // Test different attribute combinations
      let user_decision = make_multi_layer_decision(sampler, trace_id + "-user", premium_user_attributes)
      let operation_decision = make_multi_layer_decision(sampler, trace_id + "-operation", payment_operation_attributes)
      let service_decision = make_multi_layer_decision(sampler, trace_id + "-service", payment_service_attributes)
      let global_decision = make_multi_layer_decision(sampler, trace_id + "-global", other_service_attributes)
      let fallback_decision = make_multi_layer_decision(sampler, trace_id + "-fallback", fallback_attributes)
      
      if user_decision.should_sample { user_samples = user_samples + 1 }
      if operation_decision.should_sample { operation_samples = operation_samples + 1 }
      if service_decision.should_sample { service_samples = service_samples + 1 }
      if global_decision.should_sample { global_samples = global_samples + 1 }
      if fallback_decision.should_sample { fallback_samples = fallback_samples + 1 }
      
      i = i + 1
    }
    
    {
      user_rate: (user_samples as Float) / (trace_count as Float),
      operation_rate: (operation_samples as Float) / (trace_count as Float),
      service_rate: (service_samples as Float) / (trace_count as Float),
      global_rate: (global_samples as Float) / (trace_count as Float),
      fallback_rate: (fallback_samples as Float) / (trace_count as Float)
    }
  }
  
  let precedence_results = test_layer_precedence(sampler, 1000)
  assert_eq(precedence_results.user_rate, 1.0)  // 100% sampling for premium users
  assert_true((precedence_results.operation_rate - 0.8).abs() < 0.05)  // ~80% for payment operations
  assert_true((precedence_results.service_rate - 0.5).abs() < 0.05)   // ~50% for payment service
  assert_true((precedence_results.global_rate - 0.1).abs() < 0.05)    // ~10% for global
  assert_true((precedence_results.fallback_rate - 0.05).abs() < 0.05)  // ~5% for fallback
}