// Azimuth Performance Benchmark and Load Test Suite
// This file contains tests for performance benchmarking and load testing

// Test 1: Performance Metrics Collection
test "performance metrics collection" {
  // Performance metrics structure
  type PerformanceMetrics = {
    operation_name: String,
    execution_time: Int,
    memory_usage: Int,
    cpu_usage: Float,
    start_time: Int,
    end_time: Int
  }
  
  // Benchmark result
  type BenchmarkResult = {
    operation_name: String,
    sample_count: Int,
    min_time: Int,
    max_time: Int,
    avg_time: Float,
    median_time: Float,
    p95_time: Float,
    p99_time: Float,
    total_time: Int
  }
  
  // Simulate operation with variable performance
  let simulate_operation = fn(base_time: Int, variance: Int) {
    // Simple simulation with some randomness based on input
    let random_factor = (base_time % 10).to_float() / 10.0
    let variance_factor = (variance % 5).to_float()
    let execution_time = base_time + (variance_factor * random_factor).to_int()
    
    {
      operation_name: "test_operation",
      execution_time,
      memory_usage: 1024 + (base_time % 512),
      cpu_usage: 0.1 + (base_time % 50).to_float() / 100.0,
      start_time: 1640995200,
      end_time: 1640995200 + execution_time
    }
  }
  
  // Calculate percentile
  let calculate_percentile = fn(sorted_times: Array[Int], percentile: Float) {
    if sorted_times.length() == 0 {
      return 0
    }
    
    let index = ((percentile / 100.0) * (sorted_times.length() - 1).to_float()).to_int()
    sorted_times[index]
  }
  
  // Calculate benchmark statistics
  let calculate_benchmark = fn(metrics: Array[PerformanceMetrics]) {
    if metrics.length() == 0 {
      return {
        operation_name: "",
        sample_count: 0,
        min_time: 0,
        max_time: 0,
        avg_time: 0.0,
        median_time: 0.0,
        p95_time: 0.0,
        p99_time: 0.0,
        total_time: 0
      }
    }
    
    let execution_times = metrics.map(fn(m) { m.execution_time }).sort(fn(a, b) { a - b })
    let total_time = execution_times.reduce(fn(acc, time) { acc + time }, 0)
    let avg_time = total_time.to_float() / execution_times.length().to_float()
    
    let min_time = execution_times[0]
    let max_time = execution_times[execution_times.length() - 1]
    let median_time = calculate_percentile(execution_times, 50.0).to_float()
    let p95_time = calculate_percentile(execution_times, 95.0).to_float()
    let p99_time = calculate_percentile(execution_times, 99.0).to_float()
    
    {
      operation_name: metrics[0].operation_name,
      sample_count: metrics.length(),
      min_time,
      max_time,
      avg_time,
      median_time,
      p95_time,
      p99_time,
      total_time
    }
  }
  
  // Test metrics collection
  let metrics = [
    simulate_operation(100, 10),
    simulate_operation(120, 15),
    simulate_operation(90, 5),
    simulate_operation(110, 12),
    simulate_operation(105, 8),
    simulate_operation(95, 7),
    simulate_operation(115, 13),
    simulate_operation(85, 3),
    simulate_operation(125, 18),
    simulate_operation(80, 2)
  ]
  
  // Test individual metrics
  assert_eq(metrics.length(), 10)
  assert_eq(metrics[0].operation_name, "test_operation")
  assert_true(metrics[0].execution_time >= 100 and metrics[0].execution_time <= 110)
  assert_true(metrics[0].memory_usage >= 1024 and metrics[0].memory_usage <= 1536)
  assert_true(metrics[0].cpu_usage >= 0.1 and metrics[0].cpu_usage <= 0.6)
  
  // Test benchmark calculation
  let benchmark = calculate_benchmark(metrics)
  
  assert_eq(benchmark.operation_name, "test_operation")
  assert_eq(benchmark.sample_count, 10)
  assert_true(benchmark.min_time > 0)
  assert_true(benchmark.max_time >= benchmark.min_time)
  assert_true(benchmark.avg_time >= benchmark.min_time.to_float() and benchmark.avg_time <= benchmark.max_time.to_float())
  assert_true(benchmark.median_time >= benchmark.min_time.to_float() and benchmark.median_time <= benchmark.max_time.to_float())
  assert_true(benchmark.p95_time >= benchmark.median_time and benchmark.p95_time <= benchmark.max_time.to_float())
  assert_true(benchmark.p99_time >= benchmark.p95_time and benchmark.p99_time <= benchmark.max_time.to_float())
  
  // Test with empty metrics
  let empty_benchmark = calculate_benchmark([])
  assert_eq(empty_benchmark.sample_count, 0)
  assert_eq(empty_benchmark.min_time, 0)
  assert_eq(empty_benchmark.max_time, 0)
  assert_eq(empty_benchmark.avg_time, 0.0)
  
  // Test with single metric
  let single_metric = [simulate_operation(100, 10)]
  let single_benchmark = calculate_benchmark(single_metric)
  assert_eq(single_benchmark.sample_count, 1)
  assert_eq(single_benchmark.min_time, single_benchmark.max_time)
  assert_eq(single_benchmark.avg_time, single_benchmark.min_time.to_float())
  assert_eq(single_benchmark.median_time, single_benchmark.min_time.to_float())
  assert_eq(single_benchmark.p95_time, single_benchmark.min_time.to_float())
  assert_eq(single_benchmark.p99_time, single_benchmark.min_time.to_float())
}

// Test 2: Load Testing Simulation
test "load testing simulation" {
  // Load test configuration
  type LoadTestConfig = {
    concurrent_users: Int,
    requests_per_user: Int,
    ramp_up_time: Int,
    test_duration: Int
  }
  
  // User session
  type UserSession = {
    user_id: Int,
    start_time: Int,
    requests_completed: Int,
    total_response_time: Int,
    errors: Int
  }
  
  // Load test result
  type LoadTestResult = {
    total_requests: Int,
    successful_requests: Int,
    failed_requests: Int,
    avg_response_time: Float,
    min_response_time: Int,
    max_response_time: Int,
    throughput: Float,
    error_rate: Float,
    test_duration: Int
  }
  
  // Simulate user request
  let simulate_user_request = fn(user_id: Int, request_number: Int) {
    // Simulate variable response times based on user and request
    let base_time = 50 + (user_id % 20)
    let variance = request_number % 15
    let response_time = base_time + variance
    
    // Simulate occasional errors (5% error rate)
    let is_error = (user_id + request_number) % 20 == 0
    
    {
      response_time,
      is_error
    }
  }
  
  // Simulate user session
  let simulate_user_session = fn(user_id: Int, config: LoadTestConfig, start_offset: Int) {
    let mut session = {
      user_id,
      start_time: start_offset,
      requests_completed: 0,
      total_response_time: 0,
      errors: 0
    }
    
    for i in 0..config.requests_per_user {
      let request_result = simulate_user_request(user_id, i)
      
      session.requests_completed = session.requests_completed + 1
      session.total_response_time = session.total_response_time + request_result.response_time
      
      if request_result.is_error {
        session.errors = session.errors + 1
      }
    }
    
    session
  }
  
  // Run load test
  let run_load_test = fn(config: LoadTestConfig) {
    let mut sessions = []
    let ramp_up_delay = if config.concurrent_users > 1 {
      config.ramp_up_time / (config.concurrent_users - 1)
    } else {
      0
    }
    
    // Start user sessions with ramp-up
    for i in 0..config.concurrent_users {
      let start_offset = i * ramp_up_delay
      let session = simulate_user_session(i, config, start_offset)
      sessions = sessions.push(session)
    }
    
    // Calculate results
    let total_requests = sessions.reduce(fn(acc, session) { acc + session.requests_completed }, 0)
    let successful_requests = sessions.reduce(fn(acc, session) { 
      acc + (session.requests_completed - session.errors) 
    }, 0)
    let failed_requests = total_requests - successful_requests
    let total_response_time = sessions.reduce(fn(acc, session) { acc + session.total_response_time }, 0)
    
    let avg_response_time = if total_requests > 0 {
      total_response_time.to_float() / total_requests.to_float()
    } else {
      0.0
    }
    
    // Calculate min/max response times (simplified)
    let min_response_time = 50  // Based on our simulation
    let max_response_time = 85  // Based on our simulation
    
    // Calculate throughput (requests per second)
    let actual_test_duration = config.ramp_up_time + config.test_duration
    let throughput = if actual_test_duration > 0 {
      total_requests.to_float() / actual_test_duration.to_float()
    } else {
      0.0
    }
    
    // Calculate error rate
    let error_rate = if total_requests > 0 {
      failed_requests.to_float() / total_requests.to_float()
    } else {
      0.0
    }
    
    {
      total_requests,
      successful_requests,
      failed_requests,
      avg_response_time,
      min_response_time,
      max_response_time,
      throughput,
      error_rate,
      test_duration: actual_test_duration
    }
  }
  
  // Test load test configuration
  let config = {
    concurrent_users: 10,
    requests_per_user: 5,
    ramp_up_time: 30,
    test_duration: 60
  }
  
  assert_eq(config.concurrent_users, 10)
  assert_eq(config.requests_per_user, 5)
  assert_eq(config.ramp_up_time, 30)
  assert_eq(config.test_duration, 60)
  
  // Test individual user session
  let session = simulate_user_session(1, config, 0)
  assert_eq(session.user_id, 1)
  assert_eq(session.requests_completed, 5)
  assert_true(session.total_response_time > 0)
  assert_true(session.errors >= 0 and session.errors <= 5)
  
  // Test load test execution
  let result = run_load_test(config)
  
  assert_eq(result.total_requests, 50)  // 10 users * 5 requests each
  assert_eq(result.successful_requests + result.failed_requests, 50)
  assert_true(result.avg_response_time > 0.0)
  assert_eq(result.min_response_time, 50)
  assert_eq(result.max_response_time, 85)
  assert_true(result.throughput > 0.0)
  assert_true(result.error_rate >= 0.0 and result.error_rate <= 1.0)
  assert_eq(result.test_duration, 90)  // 30 ramp-up + 60 test
  
  // Test with different configurations
  let high_concurrency_config = {
    concurrent_users: 50,
    requests_per_user: 10,
    ramp_up_time: 60,
    test_duration: 120
  }
  
  let high_concurrency_result = run_load_test(high_concurrency_config)
  assert_eq(high_concurrency_result.total_requests, 500)  // 50 users * 10 requests each
  assert_true(high_concurrency_result.throughput > result.throughput)  // Higher throughput with more users
  
  let low_concurrency_config = {
    concurrent_users: 2,
    requests_per_user: 3,
    ramp_up_time: 10,
    test_duration: 30
  }
  
  let low_concurrency_result = run_load_test(low_concurrency_config)
  assert_eq(low_concurrency_result.total_requests, 6)  // 2 users * 3 requests each
  assert_true(low_concurrency_result.throughput < result.throughput)  // Lower throughput with fewer users
}

// Test 3: Memory Usage Profiling
test "memory usage profiling" {
  // Memory snapshot
  type MemorySnapshot = {
    timestamp: Int,
    total_memory: Int,
    used_memory: Int,
    free_memory: Int,
    heap_size: Int,
    heap_used: Int,
    non_heap_size: Int,
    non_heap_used: Int
  }
  
  // Memory analysis result
  type MemoryAnalysis = {
    max_total_memory: Int,
    min_total_memory: Int,
    avg_total_memory: Float,
    max_heap_used: Int,
    min_heap_used: Int,
    avg_heap_used: Float,
    memory_growth_rate: Float,
    gc_frequency: Int
  }
  
  // Simulate memory usage over time
  let simulate_memory_usage = fn(base_memory: Int, time_offset: Int, operation_type: String) {
    let mut total_memory = base_memory
    let mut heap_used = base_memory * 70 / 100  // 70% of total
    let mut non_heap_used = base_memory * 10 / 100  // 10% of total
    
    // Simulate different memory patterns based on operation
    match operation_type {
      "allocation" => {
        heap_used = heap_used + (time_offset % 100)
        total_memory = total_memory + (time_offset % 50)
      }
      "gc" => {
        heap_used = heap_used * 60 / 100  // Reduce heap usage after GC
      }
      "steady" => {
        // Minor fluctuations
        heap_used = heap_used + (time_offset % 20) - 10
      }
      _ => {}
    }
    
    let used_memory = heap_used + non_heap_used
    let free_memory = total_memory - used_memory
    
    {
      timestamp: 1640995200 + time_offset,
      total_memory,
      used_memory,
      free_memory,
      heap_size: total_memory * 80 / 100,  // 80% of total is heap
      heap_used,
      non_heap_size: total_memory * 20 / 100,  // 20% of total is non-heap
      non_heap_used
    }
  }
  
  // Analyze memory snapshots
  let analyze_memory = fn(snapshots: Array[MemorySnapshot]) {
    if snapshots.length() == 0 {
      return {
        max_total_memory: 0,
        min_total_memory: 0,
        avg_total_memory: 0.0,
        max_heap_used: 0,
        min_heap_used: 0,
        avg_heap_used: 0.0,
        memory_growth_rate: 0.0,
        gc_frequency: 0
      }
    }
    
    let total_memories = snapshots.map(fn(s) { s.total_memory })
    let heap_usages = snapshots.map(fn(s) { s.heap_used })
    
    let max_total = total_memories.reduce(fn(acc, mem) { if mem > acc { mem } else { acc }, 0)
    let min_total = total_memories.reduce(fn(acc, mem) { if mem < acc { mem } else { acc }, total_memories[0])
    let avg_total = total_memories.reduce(fn(acc, mem) { acc + mem }, 0).to_float() / total_memories.length().to_float()
    
    let max_heap = heap_usages.reduce(fn(acc, heap) { if heap > acc { heap } else { acc }, 0)
    let min_heap = heap_usages.reduce(fn(acc, heap) { if heap < acc { heap } else { acc }, heap_usages[0])
    let avg_heap = heap_usages.reduce(fn(acc, heap) { acc + heap }, 0).to_float() / heap_usages.length().to_float()
    
    // Calculate memory growth rate (simplified)
    let first_heap = heap_usages[0]
    let last_heap = heap_usages[heap_usages.length() - 1]
    let growth_rate = if first_heap > 0 {
      (last_heap - first_heap).to_float() / first_heap.to_float()
    } else {
      0.0
    }
    
    // Simulate GC frequency detection (simplified)
    let mut gc_count = 0
    for i in 1..snapshots.length() {
      if snapshots[i].heap_used < snapshots[i - 1].heap_used * 80 / 100 {
        gc_count = gc_count + 1
      }
    }
    
    {
      max_total_memory: max_total,
      min_total_memory: min_total,
      avg_total_memory: avg_total,
      max_heap_used: max_heap,
      min_heap_used: min_heap,
      avg_heap_used: avg_heap,
      memory_growth_rate: growth_rate,
      gc_frequency: gc_count
    }
  }
  
  // Test memory usage simulation
  let base_memory = 1024 * 1024  // 1MB
  
  // Simulate allocation phase
  let allocation_snapshots = []
  for i in 0..10 {
    allocation_snapshots = allocation_snapshots.push(simulate_memory_usage(base_memory, i * 10, "allocation"))
  }
  
  // Simulate GC phase
  let gc_snapshots = []
  for i in 0..5 {
    gc_snapshots = gc_snapshots.push(simulate_memory_usage(base_memory + 1000, 100 + i * 10, "gc"))
  }
  
  // Simulate steady phase
  let steady_snapshots = []
  for i in 0..10 {
    steady_snapshots = steady_snapshots.push(simulate_memory_usage(base_memory + 500, 150 + i * 10, "steady"))
  }
  
  // Test individual snapshots
  assert_eq(allocation_snapshots.length(), 10)
  assert_eq(allocation_snapshots[0].timestamp, 1640995200)
  assert_eq(allocation_snapshots[9].timestamp, 1640995200 + 90)
  
  // Test memory analysis
  let all_snapshots = allocation_snapshots + gc_snapshots + steady_snapshots
  let analysis = analyze_memory(all_snapshots)
  
  assert_true(analysis.max_total_memory >= analysis.min_total_memory)
  assert_true(analysis.avg_total_memory >= analysis.min_total_memory.to_float() and analysis.avg_total_memory <= analysis.max_total_memory.to_float())
  assert_true(analysis.max_heap_used >= analysis.min_heap_used)
  assert_true(analysis.avg_heap_used >= analysis.min_heap_used.to_float() and analysis.avg_heap_used <= analysis.max_heap_used.to_float())
  assert_true(analysis.gc_frequency >= 0)
  
  // Test with empty snapshots
  let empty_analysis = analyze_memory([])
  assert_eq(empty_analysis.max_total_memory, 0)
  assert_eq(empty_analysis.avg_total_memory, 0.0)
  assert_eq(empty_analysis.gc_frequency, 0)
  
  // Test with single snapshot
  let single_snapshot = [simulate_memory_usage(base_memory, 0, "steady")]
  let single_analysis = analyze_memory(single_snapshot)
  assert_eq(single_analysis.max_total_memory, single_analysis.min_total_memory)
  assert_eq(single_analysis.max_heap_used, single_analysis.min_heap_used)
  assert_eq(single_analysis.memory_growth_rate, 0.0)
  assert_eq(single_analysis.gc_frequency, 0)
}

// Test 4: CPU Usage Profiling
test "cpu usage profiling" {
  // CPU snapshot
  type CPUSnapshot = {
    timestamp: Int,
    user_cpu: Float,
    system_cpu: Float,
    idle_cpu: Float,
    total_cpu: Float,
    load_average: Float
  }
  
  // CPU analysis result
  type CPUAnalysis = {
    max_cpu: Float,
    min_cpu: Float,
    avg_cpu: Float,
    max_user_cpu: Float,
    max_system_cpu: Float,
    avg_load_average: Float,
    cpu_spike_count: Int,
    sustained_high_cpu_periods: Int
  }
  
  // Simulate CPU usage over time
  let simulate_cpu_usage = fn(time_offset: Int, workload_type: String) {
    let mut user_cpu = 0.0
    let mut system_cpu = 0.0
    let mut idle_cpu = 100.0
    
    match workload_type {
      "cpu_intensive" => {
        user_cpu = 70.0 + (time_offset % 20).to_float()
        system_cpu = 10.0 + (time_offset % 10).to_float()
        idle_cpu = 100.0 - user_cpu - system_cpu
      }
      "io_intensive" => {
        user_cpu = 20.0 + (time_offset % 10).to_float()
        system_cpu = 30.0 + (time_offset % 15).to_float()
        idle_cpu = 100.0 - user_cpu - system_cpu
      }
      "mixed" => {
        user_cpu = 40.0 + (time_offset % 15).to_float()
        system_cpu = 15.0 + (time_offset % 10).to_float()
        idle_cpu = 100.0 - user_cpu - system_cpu
      }
      "idle" => {
        user_cpu = 5.0 + (time_offset % 5).to_float()
        system_cpu = 2.0 + (time_offset % 3).to_float()
        idle_cpu = 100.0 - user_cpu - system_cpu
      }
      _ => {}
    }
    
    // Ensure values are within valid range
    if idle_cpu < 0.0 {
      idle_cpu = 0.0
    }
    
    let total_cpu = user_cpu + system_cpu
    
    // Simulate load average
    let load_average = total_cpu / 100.0 * 4.0  // Assuming 4 CPU cores
    
    {
      timestamp: 1640995200 + time_offset,
      user_cpu,
      system_cpu,
      idle_cpu,
      total_cpu,
      load_average
    }
  }
  
  // Analyze CPU snapshots
  let analyze_cpu = fn(snapshots: Array[CPUSnapshot]) {
    if snapshots.length() == 0 {
      return {
        max_cpu: 0.0,
        min_cpu: 0.0,
        avg_cpu: 0.0,
        max_user_cpu: 0.0,
        max_system_cpu: 0.0,
        avg_load_average: 0.0,
        cpu_spike_count: 0,
        sustained_high_cpu_periods: 0
      }
    }
    
    let total_cpus = snapshots.map(fn(s) { s.total_cpu })
    let user_cpus = snapshots.map(fn(s) { s.user_cpu })
    let system_cpus = snapshots.map(fn(s) { s.system_cpu })
    let load_averages = snapshots.map(fn(s) { s.load_average })
    
    let max_cpu = total_cpus.reduce(fn(acc, cpu) { if cpu > acc { cpu } else { acc }, 0.0)
    let min_cpu = total_cpus.reduce(fn(acc, cpu) { if cpu < acc { cpu } else { acc }, total_cpus[0])
    let avg_cpu = total_cpus.reduce(fn(acc, cpu) { acc + cpu }, 0.0) / total_cpus.length().to_float()
    
    let max_user_cpu = user_cpus.reduce(fn(acc, cpu) { if cpu > acc { cpu } else { acc }, 0.0)
    let max_system_cpu = system_cpus.reduce(fn(acc, cpu) { if cpu > acc { cpu } else { acc }, 0.0)
    let avg_load_average = load_averages.reduce(fn(acc, load) { acc + load }, 0.0) / load_averages.length().to_float()
    
    // Count CPU spikes (usage > 80%)
    let cpu_spike_count = total_cpus.filter(fn(cpu) { cpu > 80.0 }).length()
    
    // Count sustained high CPU periods (5+ consecutive samples > 70%)
    let mut sustained_high_cpu_periods = 0
    let mut current_streak = 0
    
    for cpu in total_cpus {
      if cpu > 70.0 {
        current_streak = current_streak + 1
        if current_streak >= 5 {
          sustained_high_cpu_periods = sustained_high_cpu_periods + 1
        }
      } else {
        current_streak = 0
      }
    }
    
    {
      max_cpu,
      min_cpu,
      avg_cpu,
      max_user_cpu,
      max_system_cpu,
      avg_load_average,
      cpu_spike_count,
      sustained_high_cpu_periods
    }
  }
  
  // Test CPU usage simulation
  let cpu_intensive_snapshots = []
  for i in 0..10 {
    cpu_intensive_snapshots = cpu_intensive_snapshots.push(simulate_cpu_usage(i * 10, "cpu_intensive"))
  }
  
  let io_intensive_snapshots = []
  for i in 0..10 {
    io_intensive_snapshots = io_intensive_snapshots.push(simulate_cpu_usage(i * 10, "io_intensive"))
  }
  
  let mixed_snapshots = []
  for i in 0..10 {
    mixed_snapshots = mixed_snapshots.push(simulate_cpu_usage(i * 10, "mixed"))
  }
  
  let idle_snapshots = []
  for i in 0..10 {
    idle_snapshots = idle_snapshots.push(simulate_cpu_usage(i * 10, "idle"))
  }
  
  // Test individual snapshots
  assert_eq(cpu_intensive_snapshots.length(), 10)
  assert_true(cpu_intensive_snapshots[0].user_cpu >= 70.0)
  assert_true(cpu_intensive_snapshots[0].system_cpu >= 10.0)
  
  // Test CPU analysis for different workloads
  let cpu_intensive_analysis = analyze_cpu(cpu_intensive_snapshots)
  let io_intensive_analysis = analyze_cpu(io_intensive_snapshots)
  let mixed_analysis = analyze_cpu(mixed_snapshots)
  let idle_analysis = analyze_cpu(idle_snapshots)
  
  // CPU intensive should have highest CPU usage
  assert_true(cpu_intensive_analysis.avg_cpu > io_intensive_analysis.avg_cpu)
  assert_true(cpu_intensive_analysis.avg_cpu > mixed_analysis.avg_cpu)
  assert_true(cpu_intensive_analysis.avg_cpu > idle_analysis.avg_cpu)
  
  // IO intensive should have higher system CPU than user CPU
  assert_true(io_intensive_analysis.max_system_cpu > io_intensive_analysis.max_user_cpu)
  
  // Idle should have lowest CPU usage
  assert_true(idle_analysis.avg_cpu < mixed_analysis.avg_cpu)
  assert_true(idle_analysis.avg_cpu < io_intensive_analysis.avg_cpu)
  assert_true(idle_analysis.avg_cpu < cpu_intensive_analysis.avg_cpu)
  
  // Test with empty snapshots
  let empty_cpu_analysis = analyze_cpu([])
  assert_eq(empty_cpu_analysis.max_cpu, 0.0)
  assert_eq(empty_cpu_analysis.avg_cpu, 0.0)
  assert_eq(empty_cpu_analysis.cpu_spike_count, 0)
  
  // Test with single snapshot
  let single_cpu_snapshot = [simulate_cpu_usage(0, "mixed")]
  let single_cpu_analysis = analyze_cpu(single_cpu_snapshot)
  assert_eq(single_cpu_analysis.max_cpu, single_cpu_analysis.min_cpu)
  assert_eq(single_cpu_analysis.avg_cpu, single_cpu_analysis.max_cpu)
  assert_eq(single_cpu_analysis.cpu_spike_count, if single_cpu_snapshot[0].total_cpu > 80.0 { 1 } else { 0 })
}

// Test 5: Performance Regression Detection
test "performance regression detection" {
  // Performance benchmark
  type PerformanceBenchmark = {
    test_name: String,
    timestamp: Int,
    avg_response_time: Float,
    p95_response_time: Float,
    p99_response_time: Float,
    throughput: Float,
    error_rate: Float,
    cpu_usage: Float,
    memory_usage: Int
  }
  
  // Regression threshold
  type RegressionThreshold = {
    response_time_increase_percent: Float,
    throughput_decrease_percent: Float,
    error_rate_increase_percent: Float,
    cpu_usage_increase_percent: Float,
    memory_usage_increase_percent: Float
  }
  
  // Regression result
  type RegressionResult = {
    test_name: String,
    has_regression: Bool,
    regression_metrics: Array[String],
    severity: String
  }
  
  // Default regression thresholds
  let default_thresholds = {
    response_time_increase_percent: 20.0,
    throughput_decrease_percent: 15.0,
    error_rate_increase_percent: 50.0,
    cpu_usage_increase_percent: 25.0,
    memory_usage_increase_percent: 30.0
  }
  
  // Calculate percent change
  let percent_change = fn(old_value: Float, new_value: Float) {
    if old_value == 0.0 {
      if new_value == 0.0 {
        0.0
      } else {
        100.0  // Infinite increase, cap at 100%
      }
    } else {
      ((new_value - old_value) / old_value) * 100.0
    }
  }
  
  // Detect regression
  let detect_regression = fn(baseline: PerformanceBenchmark, current: PerformanceBenchmark, thresholds: RegressionThreshold) {
    let mut regression_metrics = []
    
    // Check response time regression
    let avg_response_time_change = percent_change(baseline.avg_response_time, current.avg_response_time)
    if avg_response_time_change > thresholds.response_time_increase_percent {
      regression_metrics = regression_metrics.push("avg_response_time")
    }
    
    let p95_response_time_change = percent_change(baseline.p95_response_time, current.p95_response_time)
    if p95_response_time_change > thresholds.response_time_increase_percent {
      regression_metrics = regression_metrics.push("p95_response_time")
    }
    
    let p99_response_time_change = percent_change(baseline.p99_response_time, current.p99_response_time)
    if p99_response_time_change > thresholds.response_time_increase_percent {
      regression_metrics = regression_metrics.push("p99_response_time")
    }
    
    // Check throughput regression
    let throughput_change = percent_change(baseline.throughput, current.throughput)
    if -throughput_change > thresholds.throughput_decrease_percent {  // Negative change means decrease
      regression_metrics = regression_metrics.push("throughput")
    }
    
    // Check error rate regression
    let error_rate_change = percent_change(baseline.error_rate, current.error_rate)
    if error_rate_change > thresholds.error_rate_increase_percent {
      regression_metrics = regression_metrics.push("error_rate")
    }
    
    // Check CPU usage regression
    let cpu_usage_change = percent_change(baseline.cpu_usage, current.cpu_usage)
    if cpu_usage_change > thresholds.cpu_usage_increase_percent {
      regression_metrics = regression_metrics.push("cpu_usage")
    }
    
    // Check memory usage regression
    let memory_usage_change = percent_change(baseline.memory_usage.to_float(), current.memory_usage.to_float())
    if memory_usage_change > thresholds.memory_usage_increase_percent {
      regression_metrics = regression_metrics.push("memory_usage")
    }
    
    // Determine severity
    let severity = if regression_metrics.length() >= 4 {
      "critical"
    } else if regression_metrics.length() >= 2 {
      "major"
    } else if regression_metrics.length() >= 1 {
      "minor"
    } else {
      "none"
    }
    
    {
      test_name: current.test_name,
      has_regression: regression_metrics.length() > 0,
      regression_metrics,
      severity
    }
  }
  
  // Test baseline benchmark
  let baseline = {
    test_name: "api_endpoint_test",
    timestamp: 1640995200,
    avg_response_time: 100.0,
    p95_response_time: 200.0,
    p99_response_time: 300.0,
    throughput: 1000.0,
    error_rate: 0.01,
    cpu_usage: 50.0,
    memory_usage: 512 * 1024 * 1024  // 512MB
  }
  
  // Test no regression
  let no_regression = {
    baseline |
    timestamp: 1640995300,
    avg_response_time: 105.0,  // 5% increase
    p95_response_time: 210.0,  // 5% increase
    p99_response_time: 315.0,  // 5% increase
    throughput: 1050.0,  // 5% increase
    error_rate: 0.009,  // 10% decrease
    cpu_usage: 52.5,  // 5% increase
    memory_usage: 537 * 1024 * 1024  // 5% increase
  }
  
  let no_regression_result = detect_regression(baseline, no_regression, default_thresholds)
  assert_false(no_regression_result.has_regression)
  assert_eq(no_regression_result.severity, "none")
  assert_eq(no_regression_result.regression_metrics.length(), 0)
  
  // Test minor regression
  let minor_regression = {
    baseline |
    timestamp: 1640995300,
    avg_response_time: 125.0,  // 25% increase
    p95_response_time: 190.0,  // 5% decrease
    p99_response_time: 290.0,  // 3% decrease
    throughput: 950.0,  // 5% decrease
    error_rate: 0.011,  // 10% increase
    cpu_usage: 52.5,  // 5% increase
    memory_usage: 537 * 1024 * 1024  // 5% increase
  }
  
  let minor_regression_result = detect_regression(baseline, minor_regression, default_thresholds)
  assert_true(minor_regression_result.has_regression)
  assert_eq(minor_regression_result.severity, "minor")
  assert_eq(minor_regression_result.regression_metrics.length(), 1)
  assert_true(minor_regression_result.regression_metrics.contains("avg_response_time"))
  
  // Test major regression
  let major_regression = {
    baseline |
    timestamp: 1640995300,
    avg_response_time: 125.0,  // 25% increase
    p95_response_time: 250.0,  // 25% increase
    p99_response_time: 290.0,  // 3% decrease
    throughput: 800.0,  // 20% decrease
    error_rate: 0.011,  // 10% increase
    cpu_usage: 52.5,  // 5% increase
    memory_usage: 537 * 1024 * 1024  // 5% increase
  }
  
  let major_regression_result = detect_regression(baseline, major_regression, default_thresholds)
  assert_true(major_regression_result.has_regression)
  assert_eq(major_regression_result.severity, "major")
  assert_eq(major_regression_result.regression_metrics.length(), 3)
  
  // Test critical regression
  let critical_regression = {
    baseline |
    timestamp: 1640995300,
    avg_response_time: 125.0,  // 25% increase
    p95_response_time: 250.0,  // 25% increase
    p99_response_time: 375.0,  // 25% increase
    throughput: 800.0,  // 20% decrease
    error_rate: 0.02,  // 100% increase
    cpu_usage: 65.0,  // 30% increase
    memory_usage: 665 * 1024 * 1024  // 30% increase
  }
  
  let critical_regression_result = detect_regression(baseline, critical_regression, default_thresholds)
  assert_true(critical_regression_result.has_regression)
  assert_eq(critical_regression_result.severity, "critical")
  assert_eq(critical_regression_result.regression_metrics.length(), 6)
  
  // Test custom thresholds
  let strict_thresholds = {
    response_time_increase_percent: 10.0,
    throughput_decrease_percent: 5.0,
    error_rate_increase_percent: 20.0,
    cpu_usage_increase_percent: 10.0,
    memory_usage_increase_percent: 15.0
  }
  
  let strict_result = detect_regression(baseline, minor_regression, strict_thresholds)
  assert_true(strict_result.has_regression)
  assert_true(strict_result.regression_metrics.length() > minor_regression_result.regression_metrics.length())
}