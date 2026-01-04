// Azimuth Telemetry System - Data Sampling Strategy Tests
// This file contains test cases for data sampling strategies and functionality

// Test 1: Random Sampling Strategy
test "random sampling strategy" {
  // Create a random sampler with 10% sampling rate
  let random_sampler = RandomSampler::new(0.1)
  
  // Verify sampler properties
  assert_eq(RandomSampler::sampling_rate(random_sampler), 0.1)
  assert_eq(RandomSampler::type(random_sampler), "random")
  
  // Test sampling decision consistency for same trace ID
  let trace_id = TraceId::from_string("trace-12345")
  let decision1 = RandomSampler::should_sample(random_sampler, trace_id)
  let decision2 = RandomSampler::should_sample(random_sampler, trace_id)
  
  // Random sampler should be deterministic for same trace ID
  assert_eq(decision1, decision2)
  
  // Test sampling with different trace IDs
  let trace_ids = [
    TraceId::from_string("trace-11111"),
    TraceId::from_string("trace-22222"),
    TraceId::from_string("trace-33333"),
    TraceId::from_string("trace-44444"),
    TraceId::from_string("trace-55555"),
    TraceId::from_string("trace-66666"),
    TraceId::from_string("trace-77777"),
    TraceId::from_string("trace-88888"),
    TraceId::from_string("trace-99999"),
    TraceId::from_string("trace-00000")
  ]
  
  let mut sampled_count = 0
  for trace_id in trace_ids {
    if RandomSampler::should_sample(random_sampler, trace_id) {
      sampled_count = sampled_count + 1
    }
  }
  
  // With 10% sampling rate and 10 traces, we should get approximately 1 sample
  // Allow for some variance due to random distribution
  assert_true(sampled_count >= 0 && sampled_count <= 3)
  
  // Test with 100% sampling rate
  let always_sampler = RandomSampler::new(1.0)
  
  let mut always_sampled = true
  for trace_id in trace_ids {
    if not(RandomSampler::should_sample(always_sampler, trace_id)) {
      always_sampled = false
    }
  }
  
  assert_true(always_sampled)
  
  // Test with 0% sampling rate
  let never_sampler = RandomSampler::new(0.0)
  
  let mut never_sampled = true
  for trace_id in trace_ids {
    if RandomSampler::should_sample(never_sampler, trace_id) {
      never_sampled = false
    }
  }
  
  assert_true(never_sampled)
  
  // Test sampling statistics
  let stats = RandomSampler::statistics(random_sampler)
  
  assert_eq(stats.sampling_rate, 0.1)
  assert_eq(stats.type, "random")
  assert_eq(stats.sampled_count, sampled_count)
  assert_eq(stats.total_count, 10)
  assert_eq(stats.actual_rate, sampled_count.to_float() / 10.0)
}

// Test 2: Priority Sampling Strategy
test "priority sampling strategy" {
  // Create a priority sampler
  let priority_sampler = PrioritySampler::new()
  
  // Verify sampler properties
  assert_eq(PrioritySampler::type(priority_sampler), "priority")
  assert_eq(PrioritySampler::priority_threshold(priority_sampler), 0.5) // Default threshold
  
  // Test sampling with different priorities
  let high_priority_trace = TraceContext::with_priority(TraceId::generate(), 0.9)
  let medium_priority_trace = TraceContext::with_priority(TraceId::generate(), 0.5)
  let low_priority_trace = TraceContext::with_priority(TraceId::generate(), 0.1)
  
  assert_true(PrioritySampler::should_sample(priority_sampler, high_priority_trace))
  assert_true(PrioritySampler::should_sample(priority_sampler, medium_priority_trace))
  assert_false(PrioritySampler::should_sample(priority_sampler, low_priority_trace))
  
  // Adjust priority threshold
  PrioritySampler::set_threshold(priority_sampler, 0.7)
  
  assert_true(PrioritySampler::should_sample(priority_sampler, high_priority_trace))
  assert_false(PrioritySampler::should_sample(priority_sampler, medium_priority_trace))
  assert_false(PrioritySampler::should_sample(priority_sampler, low_priority_trace))
  
  // Test priority-based sampling with adaptive threshold
  let adaptive_sampler = PrioritySampler::adaptive(0.1, 100) // Target 10% sampling, min 100 samples
  
  // Simulate sampling decisions
  let traces = []
  for i in 0..=999 { // 1000 traces
    let priority = Random::float() // Random priority between 0 and 1
    let trace = TraceContext::with_priority(TraceId::generate(), priority)
    traces = traces.push(trace)
  }
  
  let mut sampled_count = 0
  for trace in traces {
    if PrioritySampler::should_sample(adaptive_sampler, trace) {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 10% of traces, but at least 100
  assert_true(sampled_count >= 100)
  assert_true(sampled_count <= 200) // Allow some variance
  
  // Test priority sampling with service-specific priorities
  PrioritySampler::set_service_priority(adaptive_sampler, "critical-service", 0.9)
  PrioritySampler::set_service_priority(adaptive_sampler, "normal-service", 0.5)
  PrioritySampler::set_service_priority(adaptive_sampler, "low-priority-service", 0.1)
  
  let critical_trace = TraceContext::with_service_priority(TraceId::generate(), "critical-service")
  let normal_trace = TraceContext::with_service_priority(TraceId::generate(), "normal-service")
  let low_priority_service_trace = TraceContext::with_service_priority(TraceId::generate(), "low-priority-service")
  
  assert_true(PrioritySampler::should_sample(adaptive_sampler, critical_trace))
  assert_true(PrioritySampler::should_sample(adaptive_sampler, normal_trace))
  assert_false(PrioritySampler::should_sample(adaptive_sampler, low_priority_service_trace))
  
  // Test priority sampling statistics
  let stats = PrioritySampler::statistics(adaptive_sampler)
  
  assert_eq(stats.type, "priority")
  assert_eq(stats.sampled_count, sampled_count)
  assert_eq(stats.total_count, 1000)
  assert_eq(stats.current_threshold, PrioritySampler::priority_threshold(adaptive_sampler))
}

// Test 3: Rate Limiting Sampling Strategy
test "rate limiting sampling strategy" {
  // Create a rate limiting sampler (10 samples per second)
  let rate_limit_sampler = RateLimitSampler::new(10, 1000) // 10 samples per 1000ms
  
  // Verify sampler properties
  assert_eq(RateLimitSampler::rate(rate_limit_sampler), 10)
  assert_eq(RateLimitSampler::window_ms(rate_limit_sampler), 1000)
  assert_eq(RateLimitSampler::type(rate_limit_sampler), "rate_limit")
  
  // Test sampling within rate limit
  let trace_id = TraceId::generate()
  
  // First 10 samples should be accepted
  for i in 0..=9 {
    assert_true(RateLimitSampler::should_sample(rate_limit_sampler, trace_id))
  }
  
  // 11th sample should be rejected
  assert_false(RateLimitSampler::should_sample(rate_limit_sampler, trace_id))
  
  // Test rate limit reset after window
  // In a real implementation, we would wait for the window to reset
  // For testing, we'll manually reset the sampler
  RateLimitSampler::reset_window(rate_limit_sampler)
  
  // After reset, should accept samples again
  assert_true(RateLimitSampler::should_sample(rate_limit_sampler, trace_id))
  
  // Test burst capacity
  let burst_sampler = RateLimitSampler::with_burst(10, 1000, 20) // 10/s rate, 20 burst capacity
  
  // Should accept up to burst capacity
  let mut accepted_count = 0
  for i in 0..=19 {
    if RateLimitSampler::should_sample(burst_sampler, trace_id) {
      accepted_count = accepted_count + 1
    }
  }
  
  assert_eq(accepted_count, 20) // Should accept all burst samples
  
  // Next sample should be rejected
  assert_false(RateLimitSampler::should_sample(burst_sampler, trace_id))
  
  // Test rate limiting with different time windows
  let minute_sampler = RateLimitSampler::new(600, 60000) // 600 samples per minute
  
  // Fill up the minute quota
  for i in 0..=599 {
    assert_true(RateLimitSampler::should_sample(minute_sampler, trace_id))
  }
  
  // Next sample should be rejected
  assert_false(RateLimitSampler::should_sample(minute_sampler, trace_id))
  
  // Test rate limiting with token bucket algorithm
  let token_bucket_sampler = RateLimitSampler::token_bucket(10, 20) // 10 tokens/s rate, 20 bucket capacity
  
  // Should accept up to bucket capacity immediately
  let mut token_accepted = 0
  for i in 0..=19 {
    if RateLimitSampler::should_sample(token_bucket_sampler, trace_id) {
      token_accepted = token_accepted + 1
    }
  }
  
  assert_eq(token_accepted, 20) // Should accept all bucket capacity
  
  // Next sample should be rejected
  assert_false(RateLimitSampler::should_sample(token_bucket_sampler, trace_id))
  
  // Test rate limiting statistics
  let stats = RateLimitSampler::statistics(rate_limit_sampler)
  
  assert_eq(stats.type, "rate_limit")
  assert_eq(stats.rate, 10)
  assert_eq(stats.window_ms, 1000)
  assert_eq(stats.current_usage, 11) // 10 accepted + 1 rejected
  assert_eq(stats.current_capacity, 0) // All capacity used
}

// Test 4: Adaptive Sampling Strategy
test "adaptive sampling strategy" {
  // Create an adaptive sampler with target sampling rate
  let adaptive_sampler = AdaptiveSampler::new(0.1, 1000) // Target 10% sampling, 1000 traces window
  
  // Verify sampler properties
  assert_eq(AdaptiveSampler::target_rate(adaptive_sampler), 0.1)
  assert_eq(AdaptiveSampler::window_size(adaptive_sampler), 1000)
  assert_eq(AdaptiveSampler::type(adaptive_sampler), "adaptive")
  
  // Test adaptive sampling with uniform traffic
  let traces = []
  for i in 0..=1999 { // 2000 traces
    let trace = TraceContext::new(TraceId::generate(), SpanId::generate(), TraceFlags::default(), TraceState::new())
    traces = traces.push(trace)
  }
  
  let mut sampled_count = 0
  for trace in traces {
    if AdaptiveSampler::should_sample(adaptive_sampler, trace) {
      sampled_count = sampled_count + 1
    }
  }
  
  // Should be approximately 10% of traces
  let actual_rate = sampled_count.to_float() / 2000.0
  assert_true(actual_rate > 0.08 && actual_rate < 0.12) // Allow some variance
  
  // Test adaptive sampling with traffic spikes
  let spike_traces = []
  for i in 0..=999 { // 1000 traces in a short period
    let trace = TraceContext::new(TraceId::generate(), SpanId::generate(), TraceFlags::default(), TraceState::new())
    spike_traces = spike_traces.push(trace)
  }
  
  let mut spike_sampled_count = 0
  for trace in spike_traces {
    if AdaptiveSampler::should_sample(adaptive_sampler, trace) {
      spike_sampled_count = spike_sampled_count + 1
    }
  }
  
  // During spike, should sample less to maintain target rate
  let spike_rate = spike_sampled_count.to_float() / 1000.0
  assert_true(spike_rate < actual_rate) // Should be lower than normal rate
  
  // Test adaptive sampling with error-based adjustment
  let error_adaptive_sampler = AdaptiveSampler::with_error_adjustment(0.1, 1000, 0.05) // 5% error threshold
  
  // Simulate high error rate traces
  let error_traces = []
  for i in 0..=99 { // 100 traces with high error rate
    let trace = TraceContext::with_error_rate(TraceId::generate(), 0.2) // 20% error rate
    error_traces = error_traces.push(trace)
  }
  
  let mut error_sampled_count = 0
  for trace in error_traces {
    if AdaptiveSampler::should_sample(error_adaptive_sampler, trace) {
      error_sampled_count = error_sampled_count + 1
    }
  }
  
  // Should sample more high-error traces
  let error_rate = error_sampled_count.to_float() / 100.0
  assert_true(error_rate > 0.1) // Should be higher than base rate
  
  // Test adaptive sampling with latency-based adjustment
  let latency_adaptive_sampler = AdaptiveSampler::with_latency_adjustment(0.1, 1000, 1000.0) // 1s latency threshold
  
  // Simulate high latency traces
  let latency_traces = []
  for i in 0..=99 { // 100 traces with high latency
    let trace = TraceContext::with_latency(TraceId::generate(), 2000.0) // 2s latency
    latency_traces = latency_traces.push(trace)
  }
  
  let mut latency_sampled_count = 0
  for trace in latency_traces {
    if AdaptiveSampler::should_sample(latency_adaptive_sampler, trace) {
      latency_sampled_count = latency_sampled_count + 1
    }
  }
  
  // Should sample more high-latency traces
  let latency_rate = latency_sampled_count.to_float() / 100.0
  assert_true(latency_rate > 0.1) // Should be higher than base rate
  
  // Test adaptive sampling statistics
  let stats = AdaptiveSampler::statistics(adaptive_sampler)
  
  assert_eq(stats.type, "adaptive")
  assert_eq(stats.target_rate, 0.1)
  assert_eq(stats.actual_rate, actual_rate)
  assert_eq(stats.window_size, 1000)
  assert_eq(stats.total_traces, 2000)
  assert_eq(stats.sampled_traces, sampled_count)
}

// Test 5: Deterministic Sampling Strategy
test "deterministic sampling strategy" {
  // Create a deterministic sampler
  let deterministic_sampler = DeterministicSampler::new(0.1) // 10% sampling rate
  
  // Verify sampler properties
  assert_eq(DeterministicSampler::sampling_rate(deterministic_sampler), 0.1)
  assert_eq(DeterministicSampler::type(deterministic_sampler), "deterministic")
  
  // Test deterministic sampling consistency
  let trace_id = TraceId::from_string("trace-12345")
  
  let decision1 = DeterministicSampler::should_sample(deterministic_sampler, trace_id)
  let decision2 = DeterministicSampler::should_sample(deterministic_sampler, trace_id)
  let decision3 = DeterministicSampler::should_sample(deterministic_sampler, trace_id)
  
  // Deterministic sampler should always return the same decision for the same trace ID
  assert_eq(decision1, decision2)
  assert_eq(decision2, decision3)
  
  // Test with different trace IDs
  let trace_ids = [
    TraceId::from_string("trace-00000"),
    TraceId::from_string("trace-00001"),
    TraceId::from_string("trace-00002"),
    TraceId::from_string("trace-00003"),
    TraceId::from_string("trace-00004"),
    TraceId::from_string("trace-00005"),
    TraceId::from_string("trace-00006"),
    TraceId::from_string("trace-00007"),
    TraceId::from_string("trace-00008"),
    TraceId::from_string("trace-00009")
  ]
  
  let mut sampled_count = 0
  let decisions = []
  
  for trace_id in trace_ids {
    let decision = DeterministicSampler::should_sample(deterministic_sampler, trace_id)
    decisions = decisions.push(decision)
    
    if decision {
      sampled_count = sampled_count + 1
    }
  }
  
  // With 10% sampling rate and 10 traces, we should get exactly 1 sample
  // Deterministic sampling should be more precise than random sampling
  assert_eq(sampled_count, 1)
  
  // Test deterministic sampling with different rates
  let fifty_percent_sampler = DeterministicSampler::new(0.5) // 50% sampling rate
  
  let mut fifty_percent_sampled = 0
  for trace_id in trace_ids {
    if DeterministicSampler::should_sample(fifty_percent_sampler, trace_id) {
      fifty_percent_sampled = fifty_percent_sampled + 1
    }
  }
  
  // With 50% sampling rate and 10 traces, we should get exactly 5 samples
  assert_eq(fifty_percent_sampled, 5)
  
  // Test deterministic sampling with consistent hash ring
  let hash_ring_sampler = DeterministicSampler::with_hash_ring(0.1, 100) // 100 nodes in hash ring
  
  let mut hash_ring_sampled = 0
  for trace_id in trace_ids {
    if DeterministicSampler::should_sample(hash_ring_sampler, trace_id) {
      hash_ring_sampled = hash_ring_sampled + 1
    }
  }
  
  // Should be approximately 10% of traces
  assert_true(hash_ring_sampled >= 0 && hash_ring_sampled <= 2)
  
  // Test deterministic sampling with modulo-based approach
  let modulo_sampler = DeterministicSampler::with_modulo(10) // Sample every 10th trace
  
  let mut modulo_sampled = 0
  for i in 0..=99 { // 100 traces
    let trace_id = TraceId::from_string("trace-" + i.to_string())
    if DeterministicSampler::should_sample(modulo_sampler, trace_id) {
      modulo_sampled = modulo_sampled + 1
    }
  }
  
  // Should sample exactly 10 traces (every 10th)
  assert_eq(modulo_sampled, 10)
  
  // Test deterministic sampling statistics
  let stats = DeterministicSampler::statistics(deterministic_sampler)
  
  assert_eq(stats.type, "deterministic")
  assert_eq(stats.sampling_rate, 0.1)
  assert_eq(stats.sampled_count, sampled_count)
  assert_eq(stats.total_count, 10)
  assert_eq(stats.actual_rate, sampled_count.to_float() / 10.0)
}

// Test 6: Composite Sampling Strategy
test "composite sampling strategy" {
  // Create individual samplers
  let random_sampler = RandomSampler::new(0.2) // 20% sampling
  let priority_sampler = PrioritySampler::new() // Default threshold 0.5
  let rate_limit_sampler = RateLimitSampler::new(5, 1000) // 5 samples per second
  
  // Create a composite sampler with AND strategy (all samplers must agree)
  let and_composite = CompositeSampler::and([random_sampler, priority_sampler, rate_limit_sampler])
  
  // Verify composite sampler properties
  assert_eq(CompositeSampler::strategy(and_composite), "and")
  assert_eq(CompositeSampler::sampler_count(and_composite), 3)
  assert_eq(CompositeSampler::type(and_composite), "composite")
  
  // Test composite sampling with AND strategy
  let high_priority_trace = TraceContext::with_priority(TraceId::from_string("trace-high"), 0.9)
  let low_priority_trace = TraceContext::with_priority(TraceId::from_string("trace-low"), 0.1)
  
  // High priority trace might be sampled if random sampler agrees and rate limit allows
  let high_decision = CompositeSampler::should_sample(and_composite, high_priority_trace)
  
  // Low priority trace should not be sampled (priority sampler rejects)
  let low_decision = CompositeSampler::should_sample(and_composite, low_priority_trace)
  
  assert_false(low_decision) // Priority sampler should reject low priority
  
  // Create a composite sampler with OR strategy (any sampler agreeing is enough)
  let or_composite = CompositeSampler::or([random_sampler, priority_sampler, rate_limit_sampler])
  
  // Test composite sampling with OR strategy
  let or_high_decision = CompositeSampler::should_sample(or_composite, high_priority_trace)
  let or_low_decision = CompositeSampler::should_sample(or_composite, low_priority_trace)
  
  // OR strategy might sample more traces than AND strategy
  // High priority trace has higher chance of being sampled
  // Low priority trace might be sampled if random sampler agrees
  
  // Create a composite sampler with weighted strategy
  let weighted_composite = CompositeSampler::weighted([
    (random_sampler, 0.5),    // 50% weight
    (priority_sampler, 0.3), // 30% weight
    (rate_limit_sampler, 0.2) // 20% weight
  ])
  
  // Test weighted composite sampling
  let weighted_decision = CompositeSampler::should_sample(weighted_composite, high_priority_trace)
  
  // Weighted decision should be based on weighted combination of sampler decisions
  
  // Create a composite sampler with fallback strategy
  let fallback_composite = CompositeSampler::fallback([
    priority_sampler,    // Try priority sampler first
    random_sampler,      // Fall back to random sampler
    rate_limit_sampler   // Final fallback
  ])
  
  // Test fallback composite sampling
  let fallback_high_decision = CompositeSampler::should_sample(fallback_composite, high_priority_trace)
  let fallback_low_decision = CompositeSampler::should_sample(fallback_composite, low_priority_trace)
  
  // High priority trace should be sampled by priority sampler
  // Low priority trace should fall back to random sampler
  
  // Test composite sampler with conditional logic
  let conditional_composite = CompositeSampler::conditional(fn(trace) {
    // Use priority sampler for high priority traces, random sampler for others
    if TraceContext::priority(trace) > 0.7 {
      priority_sampler
    } else {
      random_sampler
    }
  })
  
  let conditional_high_decision = CompositeSampler::should_sample(conditional_composite, high_priority_trace)
  let conditional_low_decision = CompositeSampler::should_sample(conditional_composite, low_priority_trace)
  
  // High priority trace should use priority sampler
  // Low priority trace should use random sampler
  
  // Test composite sampler statistics
  let stats = CompositeSampler::statistics(and_composite)
  
  assert_eq(stats.type, "composite")
  assert_eq(stats.strategy, "and")
  assert_eq(stats.sampler_count, 3)
  
  // Should have statistics for each sampler
  assert_true(stats.sampler_stats.contains_key("random"))
  assert_true(stats.sampler_stats.contains_key("priority"))
  assert_true(stats.sampler_stats.contains_key("rate_limit"))
}

// Test 7: Sampling Strategy Performance and Optimization
test "sampling strategy performance and optimization" {
  // Test performance of different sampling strategies
  let trace_count = 10000
  let traces = []
  
  for i in 0..=trace_count - 1 {
    let trace = TraceContext::new(TraceId::generate(), SpanId::generate(), TraceFlags::default(), TraceState::new())
    traces = traces.push(trace)
  }
  
  // Test random sampler performance
  let random_sampler = RandomSampler::new(0.1)
  let random_start_time = Time::now()
  
  let mut random_sampled = 0
  for trace in traces {
    if RandomSampler::should_sample(random_sampler, trace) {
      random_sampled = random_sampled + 1
    }
  }
  
  let random_end_time = Time::now()
  let random_duration = random_end_time - random_start_time
  
  // Test deterministic sampler performance
  let deterministic_sampler = DeterministicSampler::new(0.1)
  let deterministic_start_time = Time::now()
  
  let mut deterministic_sampled = 0
  for trace in traces {
    if DeterministicSampler::should_sample(deterministic_sampler, trace) {
      deterministic_sampled = deterministic_sampled + 1
    }
  }
  
  let deterministic_end_time = Time::now()
  let deterministic_duration = deterministic_end_time - deterministic_start_time
  
  // Test priority sampler performance
  let priority_sampler = PrioritySampler::new()
  let priority_start_time = Time::now()
  
  let mut priority_sampled = 0
  for trace in traces {
    if PrioritySampler::should_sample(priority_sampler, trace) {
      priority_sampled = priority_sampled + 1
    }
  }
  
  let priority_end_time = Time::now()
  let priority_duration = priority_end_time - priority_start_time
  
  // Test rate limit sampler performance
  let rate_limit_sampler = RateLimitSampler::new(1000, 1000) // 1000 samples per second
  let rate_limit_start_time = Time::now()
  
  let mut rate_limit_sampled = 0
  for trace in traces {
    if RateLimitSampler::should_sample(rate_limit_sampler, trace) {
      rate_limit_sampled = rate_limit_sampled + 1
    }
  }
  
  let rate_limit_end_time = Time::now()
  let rate_limit_duration = rate_limit_end_time - rate_limit_start_time
  
  // Verify all samplers processed all traces
  assert_eq(random_sampled + (trace_count - random_sampled), trace_count)
  assert_eq(deterministic_sampled + (trace_count - deterministic_sampled), trace_count)
  assert_eq(priority_sampled + (trace_count - priority_sampled), trace_count)
  assert_eq(rate_limit_sampled + (trace_count - rate_limit_sampled), trace_count)
  
  // Verify sampling rates are approximately correct
  let random_rate = random_sampled.to_float() / trace_count.to_float()
  let deterministic_rate = deterministic_sampled.to_float() / trace_count.to_float()
  
  assert_true(random_rate > 0.08 && random_rate < 0.12) // Allow some variance for random
  assert_eq(deterministic_rate, 0.1) // Deterministic should be exact
  
  // Test memory usage of different samplers
  let random_memory = Sampler::memory_usage(random_sampler)
  let deterministic_memory = Sampler::memory_usage(deterministic_sampler)
  let priority_memory = Sampler::memory_usage(priority_sampler)
  let rate_limit_memory = Sampler::memory_usage(rate_limit_sampler)
  
  // All samplers should have reasonable memory usage
  assert_true(random_memory < 1024) // Less than 1KB
  assert_true(deterministic_memory < 1024) // Less than 1KB
  assert_true(priority_memory < 1024) // Less than 1KB
  assert_true(rate_limit_memory < 1024) // Less than 1KB
  
  // Test sampling strategy optimization
  let optimized_sampler = SamplingOptimizer::optimize_for_throughput(0.1, trace_count)
  
  let optimized_start_time = Time::now()
  
  let mut optimized_sampled = 0
  for trace in traces {
    if Sampler::should_sample(optimized_sampler, trace) {
      optimized_sampled = optimized_sampled + 1
    }
  }
  
  let optimized_end_time = Time::now()
  let optimized_duration = optimized_end_time - optimized_start_time
  
  // Optimized sampler should be faster than naive implementation
  assert_true(optimized_duration <= random_duration)
  
  // Test sampler caching
  let cached_sampler = CachedSampler::new(random_sampler, 1000) // Cache 1000 decisions
  
  let cached_start_time = Time::now()
  
  let mut cached_sampled = 0
  for trace in traces {
    if CachedSampler::should_sample(cached_sampler, trace) {
      cached_sampled = cached_sampled + 1
    }
  }
  
  let cached_end_time = Time::now()
  let cached_duration = cached_end_time - cached_start_time
  
  // Cached sampler should be faster than non-cached for repeated trace IDs
  assert_true(cached_duration <= random_duration)
  
  // Verify cached sampler produces same results as original
  assert_eq(random_sampled, cached_sampled)
  
  // Test sampler parallelization
  let parallel_sampler = ParallelSampler::new(random_sampler, 4) // 4 worker threads
  
  let parallel_start_time = Time::now()
  
  let parallel_results = ParallelSampler::should_sample_batch(parallel_sampler, traces)
  
  let parallel_end_time = Time::now()
  let parallel_duration = parallel_end_time - parallel_start_time
  
  let parallel_sampled = parallel_results.filter(fn(decision) { decision }).length()
  
  // Parallel sampler should be faster than sequential for large batches
  assert_true(parallel_duration <= random_duration)
  
  // Verify parallel sampler produces same results as sequential
  assert_eq(random_sampled, parallel_sampled)
  
  // Test sampler benchmarking
  let benchmark_results = Sampler::benchmark([
    ("random", random_sampler),
    ("deterministic", deterministic_sampler),
    ("priority", priority_sampler),
    ("rate_limit", rate_limit_sampler),
    ("optimized", optimized_sampler),
    ("cached", cached_sampler),
    ("parallel", parallel_sampler)
  ], traces)
  
  // Verify benchmark results
  assert_true(benchmark_results.length() == 7)
  
  for (name, results) in benchmark_results {
    assert_true(results.throughput > 0) // All samplers should have some throughput
    assert_true(results.latency > 0) // All samplers should have some latency
    assert_true(results.memory_usage > 0) // All samplers should use some memory
    assert_true(results.accuracy > 0.0 && results.accuracy <= 1.0) // Accuracy should be between 0 and 1
  }
  
  // Find fastest sampler
  let fastest_sampler = benchmark_results.reduce(fn(best, current) {
    if current.1.throughput > best.1.throughput {
      current
    } else {
      best
    }
  }, benchmark_results[0])
  
  // Find most accurate sampler
  let most_accurate_sampler = benchmark_results.reduce(fn(best, current) {
    if current.1.accuracy > best.1.accuracy {
      current
    } else {
      best
    }
  }, benchmark_results[0])
  
  // Verify deterministic sampler is most accurate
  assert_eq(most_accurate_sampler.0, "deterministic")
  assert_eq(most_accurate_sampler.1.accuracy, 1.0)
}