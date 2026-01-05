// Azimuth Telemetry Sampling Strategy Tests
// This file contains test cases for telemetry data sampling strategies

// Test 1: Fixed Rate Sampling Strategy
test "fixed rate sampling strategy" {
  // Define sampling configuration
  type SamplingConfig = {
    strategy: String,
    rate: Double,  // 0.0 to 1.0
    sample_size: Int
  }
  
  // Create fixed rate sampler
  let create_fixed_rate_sampler = fn(config: SamplingConfig) {
    fn(trace_id: String) {
      // Simple hash-based sampling
      let hash = trace_id.length() % 100
      let threshold = (config.rate * 100.0).to_int()
      hash < threshold
    }
  }
  
  // Test with 50% sampling rate
  let config_50 = {
    strategy: "fixed_rate",
    rate: 0.5,
    sample_size: 100
  }
  
  let sampler_50 = create_fixed_rate_sampler(config_50)
  
  // Test deterministic sampling based on trace ID
  assert_true(sampler_50("trace-1"))  // hash = 6, threshold = 50
  assert_false(sampler_50("trace-10"))  // hash = 7, threshold = 50
  assert_true(sampler_50("trace-100"))  // hash = 8, threshold = 50
  
  // Test with 100% sampling rate
  let config_100 = {
    strategy: "fixed_rate",
    rate: 1.0,
    sample_size: 100
  }
  
  let sampler_100 = create_fixed_rate_sampler(config_100)
  assert_true(sampler_100("any-trace-id"))
  assert_true(sampler_100("another-trace"))
  
  // Test with 0% sampling rate
  let config_0 = {
    strategy: "fixed_rate",
    rate: 0.0,
    sample_size: 100
  }
  
  let sampler_0 = create_fixed_rate_sampler(config_0)
  assert_false(sampler_0("any-trace-id"))
  assert_false(sampler_0("another-trace"))
}

// Test 2: Adaptive Sampling Strategy
test "adaptive sampling strategy" {
  // Define adaptive sampling state
  type AdaptiveSamplingState = {
    current_rate: Double,
    min_rate: Double,
    max_rate: Double,
    target_sample_size: Int,
    current_sample_count: Int,
    total_requests: Int
  }
  
  // Create adaptive sampler
  let create_adaptive_sampler = fn(initial_state: AdaptiveSamplingState) {
    let state = { mut current_rate: initial_state.current_rate }
    
    fn(trace_id: String, should_adjust: Bool) {
      // Simple hash-based sampling with current rate
      let hash = trace_id.length() % 100
      let threshold = (state.current_rate * 100.0).to_int()
      let should_sample = hash < threshold
      
      // Adjust sampling rate if needed
      if should_adjust {
        if initial_state.current_sample_count < initial_state.target_sample_size {
          // Increase rate if we're not getting enough samples
          state.current_rate = @min(state.current_rate + 0.1, initial_state.max_rate)
        } else if initial_state.current_sample_count > initial_state.target_sample_size * 2 {
          // Decrease rate if we're getting too many samples
          state.current_rate = @max(state.current_rate - 0.1, initial_state.min_rate)
        }
      }
      
      should_sample
    }
  }
  
  // Test adaptive sampling
  let initial_state = {
    current_rate: 0.5,
    min_rate: 0.1,
    max_rate: 0.9,
    target_sample_size: 100,
    current_sample_count: 50,
    total_requests: 200
  }
  
  let adaptive_sampler = create_adaptive_sampler(initial_state)
  
  // Test initial sampling behavior
  let sample1 = adaptive_sampler("trace-1", false)  // No adjustment
  let sample2 = adaptive_sampler("trace-2", false)  // No adjustment
  
  // Test with adjustment
  let sample3 = adaptive_sampler("trace-3", true)   // With adjustment
  
  // Verify sampling decisions are consistent with rate
  assert_true(sample1 || sample2 || sample3)  // At least one should be true
}

// Test 3: Priority-Based Sampling
test "priority-based sampling strategy" {
  // Define span priority levels
  enum Priority {
    Low
    Medium
    High
    Critical
  }
  
  // Define priority sampling config
  type PrioritySamplingConfig = {
    low_priority_rate: Double,
    medium_priority_rate: Double,
    high_priority_rate: Double,
    critical_priority_rate: Double
  }
  
  // Create priority sampler
  let create_priority_sampler = fn(config: PrioritySamplingConfig) {
    fn(priority: Priority, trace_id: String) {
      let hash = trace_id.length() % 100
      
      let threshold = match priority {
        Priority::Low => (config.low_priority_rate * 100.0).to_int()
        Priority::Medium => (config.medium_priority_rate * 100.0).to_int()
        Priority::High => (config.high_priority_rate * 100.0).to_int()
        Priority::Critical => (config.critical_priority_rate * 100.0).to_int()
      }
      
      hash < threshold
    }
  }
  
  // Test with priority-based rates
  let config = {
    low_priority_rate: 0.1,      // 10%
    medium_priority_rate: 0.3,   // 30%
    high_priority_rate: 0.7,     // 70%
    critical_priority_rate: 1.0  // 100%
  }
  
  let priority_sampler = create_priority_sampler(config)
  
  // Test critical priority (always sampled)
  assert_true(priority_sampler(Priority::Critical, "any-trace"))
  
  // Test high priority (70% chance)
  assert_true(priority_sampler(Priority::High, "trace-1"))   // hash = 6, threshold = 70
  assert_false(priority_sampler(Priority::High, "trace-10"))  // hash = 7, threshold = 70
  
  // Test medium priority (30% chance)
  assert_false(priority_sampler(Priority::Medium, "trace-100"))  // hash = 8, threshold = 30
  
  // Test low priority (10% chance)
  assert_false(priority_sampler(Priority::Low, "trace-1000"))  // hash = 9, threshold = 10
}

// Test 4: Error Budget-Based Sampling
test "error budget-based sampling strategy" {
  // Define error budget state
  type ErrorBudget = {
    total_operations: Int,
    error_operations: Int,
    error_rate_threshold: Double,
    sampling_rate: Double
  }
  
  // Create error budget sampler
  let create_error_budget_sampler = fn(initial_budget: ErrorBudget) {
    let budget = { mut sampling_rate: initial_budget.sampling_rate }
    
    fn(trace_id: String, current_errors: Int, total_ops: Int) {
      // Calculate current error rate
      let current_error_rate = if total_ops > 0 {
        current_errors.to_double() / total_ops.to_double()
      } else {
        0.0
      }
      
      // Adjust sampling rate based on error rate
      if current_error_rate > initial_budget.error_rate_threshold {
        // Increase sampling when error rate is high
        budget.sampling_rate = @min(budget.sampling_rate + 0.2, 1.0)
      } else if current_error_rate < initial_budget.error_rate_threshold / 2.0 {
        // Decrease sampling when error rate is low
        budget.sampling_rate = @max(budget.sampling_rate - 0.1, 0.1)
      }
      
      // Apply sampling
      let hash = trace_id.length() % 100
      let threshold = (budget.sampling_rate * 100.0).to_int()
      hash < threshold
    }
  }
  
  // Test error budget sampling
  let initial_budget = {
    total_operations: 1000,
    error_operations: 50,
    error_rate_threshold: 0.05,  // 5%
    sampling_rate: 0.5
  }
  
  let error_budget_sampler = create_error_budget_sampler(initial_budget)
  
  // Test normal error rate (5%)
  let normal_sample = error_budget_sampler("trace-1", 50, 1000)
  
  // Test high error rate (10%)
  let high_error_sample = error_budget_sampler("trace-2", 100, 1000)
  
  // Test low error rate (2%)
  let low_error_sample = error_budget_sampler("trace-3", 20, 1000)
  
  // Verify sampling behavior
  assert_true(normal_sample || high_error_sample || low_error_sample)
}

// Test 5: Sampling Decision Consistency
test "sampling decision consistency" {
  // Define consistent sampler
  type ConsistentSampler = {
    seed: Int,
    sampling_rate: Double
  }
  
  let create_consistent_sampler = fn(sampler: ConsistentSampler) {
    fn(trace_id: String) {
      // Simple consistent hash function
      let hash = (trace_id.length() + sampler.seed) % 100
      let threshold = (sampler.sampling_rate * 100.0).to_int()
      hash < threshold
    }
  }
  
  // Test consistency
  let sampler = {
    seed: 42,
    sampling_rate: 0.5
  }
  
  let consistent_sampler = create_consistent_sampler(sampler)
  
  // Same trace ID should always produce same decision
  let decision1 = consistent_sampler("consistent-trace")
  let decision2 = consistent_sampler("consistent-trace")
  let decision3 = consistent_sampler("consistent-trace")
  
  assert_eq(decision1, decision2)
  assert_eq(decision2, decision3)
  
  // Different trace IDs can have different decisions
  let decision_a = consistent_sampler("trace-a")
  let decision_b = consistent_sampler("trace-b")
  
  // They might be same or different, but should be deterministic
  let decision_a_again = consistent_sampler("trace-a")
  let decision_b_again = consistent_sampler("trace-b")
  
  assert_eq(decision_a, decision_a_again)
  assert_eq(decision_b, decision_b_again)
}

// Test 6: Sampling Rate Limiting
test "sampling rate limiting" {
  // Define rate limiter
  type RateLimiter = {
    max_samples_per_second: Int,
    current_samples: Int,
    window_start: Int,
    window_size: Int  // in seconds
  }
  
  let create_rate_limited_sampler = fn(base_sampler: (String) -> Bool, limiter: RateLimiter) {
    let state = { 
      mut current_samples: limiter.current_samples,
      mut window_start: limiter.window_start
    }
    
    fn(trace_id: String, current_time: Int) {
      // Reset window if needed
      if current_time - state.window_start >= limiter.window_size {
        state.current_samples = 0
        state.window_start = current_time
      }
      
      // Check if we've hit the limit
      if state.current_samples >= limiter.max_samples_per_second {
        false
      } else {
        // Apply base sampler
        let should_sample = base_sampler(trace_id)
        
        // Increment counter if sampling
        if should_sample {
          state.current_samples = state.current_samples + 1
        }
        
        should_sample
      }
    }
  }
  
  // Test rate limiting
  let base_sampler = fn(trace_id: String) {
    // Always return true for this test
    true
  }
  
  let limiter = {
    max_samples_per_second: 3,
    current_samples: 0,
    window_start: 1000,
    window_size: 1
  }
  
  let rate_limited_sampler = create_rate_limited_sampler(base_sampler, limiter)
  
  // Test within limit
  assert_true(rate_limited_sampler("trace-1", 1000))
  assert_true(rate_limited_sampler("trace-2", 1000))
  assert_true(rate_limited_sampler("trace-3", 1000))
  
  // Test exceeding limit
  assert_false(rate_limited_sampler("trace-4", 1000))
  assert_false(rate_limited_sampler("trace-5", 1000))
  
  // Test window reset
  assert_true(rate_limited_sampler("trace-6", 1001))  // New window
  assert_true(rate_limited_sampler("trace-7", 1001))
  assert_true(rate_limited_sampler("trace-8", 1001))
  
  // Test limit again in new window
  assert_false(rate_limited_sampler("trace-9", 1001))
}

// Test 7: Composite Sampling Strategy
test "composite sampling strategy" {
  // Define composite sampler
  type CompositeSampler = {
    samplers: Array<(String) -> Bool>,
    combination_strategy: String  // "any", "all", "majority"
  }
  
  let create_composite_sampler = fn(composite: CompositeSampler) {
    fn(trace_id: String) {
      let results = composite.samplers.map(fn(sampler) { sampler(trace_id) })
      
      match composite.combination_strategy {
        "any" => results.contains(true),
        "all" => results.all(fn(r) { r }),
        "majority" => {
          let true_count = results.filter(fn(r) { r }).length()
          true_count > results.length() / 2
        },
        _ => false
      }
    }
  }
  
  // Test composite sampling
  let sampler_50 = fn(trace_id: String) {
    (trace_id.length() % 100) < 50
  }
  
  let sampler_30 = fn(trace_id: String) {
    (trace_id.length() % 100) < 30
  }
  
  let sampler_70 = fn(trace_id: String) {
    (trace_id.length() % 100) < 70
  }
  
  // Test "any" strategy
  let any_composite = create_composite_sampler({
    samplers: [sampler_50, sampler_30, sampler_70],
    combination_strategy: "any"
  })
  
  assert_true(any_composite("trace-1"))   // At least one sampler should return true
  
  // Test "all" strategy
  let all_composite = create_composite_sampler({
    samplers: [sampler_50, sampler_30, sampler_70],
    combination_strategy: "all"
  })
  
  // For trace-1 (length 6): sampler_50=true, sampler_30=true, sampler_70=true
  assert_true(all_composite("trace-1"))
  
  // For trace-10 (length 7): sampler_50=true, sampler_30=true, sampler_70=true
  assert_true(all_composite("trace-10"))
  
  // Test "majority" strategy
  let majority_composite = create_composite_sampler({
    samplers: [sampler_50, sampler_30, sampler_70],
    combination_strategy: "majority"
  })
  
  assert_true(majority_composite("trace-1"))   // 3/3 true = majority
}

// Test 8: Sampling Performance Optimization
test "sampling performance optimization" {
  // Define performance-optimized sampler
  type OptimizedSampler = {
    sampling_rate: Double,
    cache_size: Int,
    cache_hits: Int,
    cache_misses: Int
  }
  
  let create_optimized_sampler = fn(sampler: OptimizedSampler) {
    let cache = { mut entries: [] }
    let stats = { mut hits: 0, mut misses: 0 }
    
    fn(trace_id: String) {
      // Check cache first
      let mut cached_result = None
      for (cached_id, result) in cache.entries {
        if cached_id == trace_id {
          cached_result = Some(result)
          stats.hits = stats.hits + 1
          break
        }
      }
      
      match cached_result {
        Some(result) => result,
        None => {
          // Cache miss - compute result
          stats.misses = stats.misses + 1
          let hash = trace_id.length() % 100
          let threshold = (sampler.sampling_rate * 100.0).to_int()
          let result = hash < threshold
          
          // Add to cache (simple LRU would be better in production)
          if cache.entries.length() < sampler.cache_size {
            cache.entries = cache.entries.push((trace_id, result))
          }
          
          result
        }
      }
    }
  }
  
  // Test optimized sampler
  let optimized_sampler = create_optimized_sampler({
    sampling_rate: 0.5,
    cache_size: 10,
    cache_hits: 0,
    cache_misses: 0
  })
  
  // First call - cache miss
  let result1 = optimized_sampler("cached-trace")
  
  // Second call - cache hit
  let result2 = optimized_sampler("cached-trace")
  
  // Third call - cache hit
  let result3 = optimized_sampler("cached-trace")
  
  // Results should be consistent
  assert_eq(result1, result2)
  assert_eq(result2, result3)
  
  // Test with different trace IDs
  let result_a = optimized_sampler("trace-a")
  let result_b = optimized_sampler("trace-b")
  let result_a_again = optimized_sampler("trace-a")  // Should be cached
  
  assert_eq(result_a, result_a_again)
}