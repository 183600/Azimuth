// Azimuth Resource Monitoring Test Suite
// This file contains test cases for resource monitoring and management

// Test 1: CPU Monitoring
test "CPU monitoring and utilization tracking" {
  // Define CPU metrics
  type CpuMetrics = {
    usage_percent: Float,
    load_average_1m: Float,
    load_average_5m: Float,
    load_average_15m: Float,
    core_count: Int,
    context_switches: Int,
    interrupts: Int
  }
  
  // Define CPU state
  type CpuState = {
    user_time: Float,
    system_time: Float,
    idle_time: Float,
    wait_time: Float,
    total_time: Float
  }
  
  // Calculate CPU usage from states
  let calculate_cpu_usage = fn(prev_state: CpuState, curr_state: CpuState) {
    let total_diff = curr_state.total_time - prev_state.total_time
    if total_diff > 0.0 {
      let idle_diff = curr_state.idle_time - prev_state.idle_time
      let wait_diff = curr_state.wait_time - prev_state.wait_time
      let non_idle_diff = total_diff - idle_diff - wait_diff
      
      (non_idle_diff / total_diff) * 100.0
    } else {
      0.0
    }
  }
  
  // Create initial CPU state
  let initial_state = {
    user_time: 1000.0,
    system_time: 500.0,
    idle_time: 5000.0,
    wait_time: 100.0,
    total_time: 6600.0
  }
  
  // Create current CPU state
  let current_state = {
    user_time: 1200.0,  // +200
    system_time: 600.0,  // +100
    idle_time: 5200.0,  // +200
    wait_time: 150.0,   // +50
    total_time: 7150.0  // +550
  }
  
  // Test CPU usage calculation
  let cpu_usage = calculate_cpu_usage(initial_state, current_state)
  assert_eq(cpu_usage.round(), 54.55)  // (550 - 200 - 50) / 550 * 100
  
  // Test CPU metrics collection
  let collect_cpu_metrics = fn(state: CpuState, core_count: Int) {
    let total_active = state.user_time + state.system_time
    let total_time = state.total_time
    let usage_percent = if total_time > 0.0 {
      (total_active / total_time) * 100.0
    } else {
      0.0
    }
    
    {
      usage_percent,
      load_average_1m: 1.5,
      load_average_5m: 1.2,
      load_average_15m: 0.8,
      core_count,
      context_switches: 15000,
      interrupts: 25000
    }
  }
  
  let cpu_metrics = collect_cpu_metrics(current_state, 4)
  assert_eq(cpu_metrics.core_count, 4)
  assert_eq(cpu_metrics.usage_percent.round(), 25.17)  // (1200 + 600) / 7150 * 100
  assert_eq(cpu_metrics.load_average_1m, 1.5)
  assert_eq(cpu_metrics.context_switches, 15000)
  
  // Test CPU threshold monitoring
  let check_cpu_thresholds = fn(metrics: CpuMetrics, warning_threshold: Float, critical_threshold: Float) {
    if metrics.usage_percent >= critical_threshold {
      "CRITICAL"
    } else if metrics.usage_percent >= warning_threshold {
      "WARNING"
    } else {
      "NORMAL"
    }
  }
  
  let normal_status = check_cpu_thresholds(cpu_metrics, 70.0, 90.0)
  assert_eq(normal_status, "NORMAL")
  
  let high_cpu_metrics = { cpu_metrics | usage_percent: 75.0 }
  let warning_status = check_cpu_thresholds(high_cpu_metrics, 70.0, 90.0)
  assert_eq(warning_status, "WARNING")
  
  let critical_cpu_metrics = { cpu_metrics | usage_percent: 95.0 }
  let critical_status = check_cpu_thresholds(critical_cpu_metrics, 70.0, 90.0)
  assert_eq(critical_status, "CRITICAL")
  
  // Test CPU trend analysis
  type CpuTrend = {
    trend: String,  // "INCREASING", "DECREASING", "STABLE"
    rate: Float,    // Rate of change per minute
    prediction: Float  // Predicted usage in 5 minutes
  }
  
  let analyze_cpu_trend = fn(historical_usage: Array<Float>) {
    if historical_usage.length() < 2 {
      {
        trend: "INSUFFICIENT_DATA",
        rate: 0.0,
        prediction: historical_usage.get(0, 0.0)
      }
    } else {
      let latest = historical_usage[historical_usage.length() - 1]
      let previous = historical_usage[historical_usage.length() - 2]
      
      let rate = latest - previous
      let trend = if rate > 1.0 {
        "INCREASING"
      } else if rate < -1.0 {
        "DECREASING"
      } else {
        "STABLE"
      }
      
      let prediction = latest + (rate * 5.0)
      
      {
        trend,
        rate,
        prediction
      }
    }
  }
  
  let cpu_history = [45.2, 47.8, 50.1, 52.3, 54.7]
  let trend_analysis = analyze_cpu_trend(cpu_history)
  assert_eq(trend_analysis.trend, "INCREASING")
  assert_eq(trend_analysis.rate.round(), 2.4)
  assert_eq(trend_analysis.prediction.round(), 66.7)  // 54.7 + 2.4 * 5
}

// Test 2: Memory Monitoring
test "memory monitoring and leak detection" {
  // Define memory metrics
  type MemoryMetrics = {
    total_bytes: Int,
    used_bytes: Int,
    free_bytes: Int,
    cached_bytes: Int,
    buffers_bytes: Int,
    swap_total_bytes: Int,
    swap_used_bytes: Int,
    swap_free_bytes: Int
  }
  
  // Define memory usage breakdown
  type MemoryBreakdown = {
    application_percent: Float,
    cache_percent: Float,
    buffers_percent: Float,
    free_percent: Float
  }
  
  // Calculate memory breakdown
  let calculate_memory_breakdown = fn(metrics: MemoryMetrics) {
    let total = metrics.total_bytes as Float
    let application = (metrics.used_bytes - metrics.cached_bytes - metrics.buffers_bytes) as Float
    let cache = metrics.cached_bytes as Float
    let buffers = metrics.buffers_bytes as Float
    let free = metrics.free_bytes as Float
    
    {
      application_percent: (application / total) * 100.0,
      cache_percent: (cache / total) * 100.0,
      buffers_percent: (buffers / total) * 100.0,
      free_percent: (free / total) * 100.0
    }
  }
  
  // Create sample memory metrics
  let memory_metrics = {
    total_bytes: 8589934592,      // 8GB
    used_bytes: 6442450944,       // 6GB
    free_bytes: 2147483648,       // 2GB
    cached_bytes: 1073741824,      // 1GB
    buffers_bytes: 268435456,     // 256MB
    swap_total_bytes: 4294967296,  // 4GB
    swap_used_bytes: 0,            // 0GB
    swap_free_bytes: 4294967296    // 4GB
  }
  
  // Test memory breakdown calculation
  let breakdown = calculate_memory_breakdown(memory_metrics)
  assert_eq(breakdown.application_percent.round(), 48.83)  // (6GB - 1GB - 256MB) / 8GB
  assert_eq(breakdown.cache_percent.round(), 12.5)         // 1GB / 8GB
  assert_eq(breakdown.buffers_percent.round(), 3.13)       // 256MB / 8GB
  assert_eq(breakdown.free_percent.round(), 25.0)          // 2GB / 8GB
  
  // Test memory pressure detection
  let detect_memory_pressure = fn(metrics: MemoryMetrics) {
    let usage_percent = (metrics.used_bytes as Float / metrics.total_bytes as Float) * 100.0
    let swap_usage_percent = if metrics.swap_total_bytes > 0 {
      (metrics.swap_used_bytes as Float / metrics.swap_total_bytes as Float) * 100.0
    } else {
      0.0
    }
    
    if usage_percent >= 90.0 || swap_usage_percent >= 50.0 {
      "CRITICAL"
    } else if usage_percent >= 80.0 || swap_usage_percent >= 25.0 {
      "WARNING"
    } else {
      "NORMAL"
    }
  }
  
  let memory_pressure = detect_memory_pressure(memory_metrics)
  assert_eq(memory_pressure, "WARNING")  // 75% usage
  
  let high_memory_metrics = { memory_metrics | used_bytes: 8224355584 }  // 7.66GB
  let high_pressure = detect_memory_pressure(high_memory_metrics)
  assert_eq(high_pressure, "CRITICAL")
  
  // Test memory leak detection
  type MemoryLeakDetection = {
    is_leaking: Bool,
    leak_rate_mb_per_hour: Float,
    confidence: Float,
    time_to_exhaustion_hours: Option[Int>
  }
  
  let detect_memory_leak = fn(memory_samples: Array<(Int, Int)>, time_window_hours: Int) {
    if memory_samples.length() < 3 {
      {
        is_leaking: false,
        leak_rate_mb_per_hour: 0.0,
        confidence: 0.0,
        time_to_exhaustion_hours: None
      }
    } else {
      // Calculate memory growth rate using linear regression (simplified)
      let n = memory_samples.length()
      let sum_x = memory_samples.reduce(fn(sum, _) { sum + 1 }, 0)
      let sum_y = memory_samples.reduce(fn(sum, sample) { sum + sample.1 }, 0)
      let sum_xy = memory_samples.reduce_with_index(fn(sum, sample, index) { 
        sum + ((index + 1) * sample.1) 
      }, 0)
      let sum_x2 = memory_samples.reduce_with_index(fn(sum, _, index) { 
        sum + ((index + 1) * (index + 1)) 
      }, 0)
      
      let slope = ((n as Float * sum_xy as Float) - (sum_x as Float * sum_y as Float)) /
                  ((n as Float * sum_x2 as Float) - (sum_x as Float * sum_x as Float))
      
      let leak_rate_mb_per_hour = slope / (1024.0 * 1024.0)  // Convert bytes to MB
      let is_leaking = leak_rate_mb_per_hour > 10.0  // Threshold: 10MB/hour
      
      let confidence = if n >= 5 {
        0.8
      } else if n >= 3 {
        0.6
      } else {
        0.3
      }
      
      let latest_memory = memory_samples[n - 1].1
      let time_to_exhaustion = if is_leaking && leak_rate_mb_per_hour > 0.0 {
        let hours_to_exhaust = (latest_memory as Float / 1024.0 / 1024.0) / leak_rate_mb_per_hour
        Some(hours_to_exhaust as Int)
      } else {
        None
      }
      
      {
        is_leaking,
        leak_rate_mb_per_hour,
        confidence,
        time_to_exhaustion_hours: time_to_exhaustion
      }
    }
  }
  
  // Test memory leak detection
  let memory_samples = [
    (1640995200, 1073741824),   // 1GB
    (1640995800, 1178599424),   // 1.1GB
    (1640996400, 1283457024),   // 1.2GB
    (1640997000, 1388314624),   // 1.3GB
    (1640997600, 1493172224)    // 1.4GB
  ]
  
  let leak_detection = detect_memory_leak(memory_samples, 2)
  assert_true(leak_detection.is_leaking)
  assert_eq(leak_detection.leak_rate_mb_per_hour.round(), 83.89)  // ~84MB/hour
  assert_eq(leak_detection.confidence, 0.8)
  assert_eq(leak_detection.time_to_exhaustion_hours, Some(16))  // ~16 hours to exhaustion
  
  // Test with stable memory
  let stable_samples = [
    (1640995200, 1073741824),
    (1640995800, 1075838976),
    (1640996400, 1073741824),
    (1640997000, 1075838976),
    (1640997600, 1073741824)
  ]
  
  let stable_detection = detect_memory_leak(stable_samples, 2)
  assert_false(stable_detection.is_leaking)
  assert_eq(stable_detection.leak_rate_mb_per_hour.round(), 0)
  
  // Test memory efficiency metrics
  type MemoryEfficiency = {
    cache_hit_ratio: Float,
    swap_usage_ratio: Float,
    fragmentation_ratio: Float,
    reclamation_efficiency: Float
  }
  
  let calculate_memory_efficiency = fn(metrics: MemoryMetrics, page_stats: (Int, Int, Int)) {
    let (cache_hits, cache_misses, page_reclaims) = page_stats
    let total_cache_requests = cache_hits + cache_misses
    let cache_hit_ratio = if total_cache_requests > 0 {
      (cache_hits as Float / total_cache_requests as Float) * 100.0
    } else {
      0.0
    }
    
    let swap_usage_ratio = if metrics.swap_total_bytes > 0 {
      (metrics.swap_used_bytes as Float / metrics.swap_total_bytes as Float) * 100.0
    } else {
      0.0
    }
    
    // Simplified fragmentation calculation
    let fragmentation_ratio = if metrics.free_bytes > 0 {
      ((metrics.total_bytes - metrics.used_bytes) as Float / metrics.total_bytes as Float) * 100.0
    } else {
      0.0
    }
    
    // Reclamation efficiency (page reclaims vs total pages)
    let reclamation_efficiency = if page_reclaims > 0 {
      (page_reclaims as Float / (page_reclaims + cache_misses) as Float) * 100.0
    } else {
      0.0
    }
    
    {
      cache_hit_ratio,
      swap_usage_ratio,
      fragmentation_ratio,
      reclamation_efficiency
    }
  }
  
  let page_stats = (85000, 15000, 5000)
  let efficiency = calculate_memory_efficiency(memory_metrics, page_stats)
  assert_eq(efficiency.cache_hit_ratio.round(), 85.0)  // 85000 / (85000 + 15000)
  assert_eq(efficiency.swap_usage_ratio, 0.0)         // No swap usage
  assert_eq(efficiency.fragmentation_ratio.round(), 25.0)  // 2GB free / 8GB total
  assert_eq(efficiency.reclamation_efficiency.round(), 25.0)  // 5000 / (5000 + 15000)
}

// Test 3: Disk I/O Monitoring
test "disk I/O monitoring and performance analysis" {
  // Define disk metrics
  type DiskMetrics = {
    device: String,
    read_bytes_per_sec: Float,
    write_bytes_per_sec: Float,
    read_ops_per_sec: Float,
    write_ops_per_sec: Float,
    queue_depth: Int,
    utilization_percent: Float,
    await_time_ms: Float,
    service_time_ms: Float
  }
  
  // Define disk space metrics
  type DiskSpaceMetrics = {
    device: String,
    total_bytes: Int,
    used_bytes: Int,
    free_bytes: Int,
    inodes_used: Int,
    inodes_free: Int
  }
  
  // Calculate disk throughput
  let calculate_throughput = fn(metrics: DiskMetrics) {
    let total_bytes_per_sec = metrics.read_bytes_per_sec + metrics.write_bytes_per_sec
    let total_ops_per_sec = metrics.read_ops_per_sec + metrics.write_ops_per_sec
    
    {
      total_throughput_mb_per_sec: total_bytes_per_sec / (1024.0 * 1024.0),
      total_ops_per_sec,
      read_throughput_mb_per_sec: metrics.read_bytes_per_sec / (1024.0 * 1024.0),
      write_throughput_mb_per_sec: metrics.write_bytes_per_sec / (1024.0 * 1024.0)
    }
  }
  
  // Create sample disk metrics
  let disk_metrics = {
    device: "/dev/sda1",
    read_bytes_per_sec: 52428800.0,    // 50MB/s
    write_bytes_per_sec: 31457280.0,   // 30MB/s
    read_ops_per_sec: 100.0,
    write_ops_per_sec: 75.0,
    queue_depth: 3,
    utilization_percent: 65.0,
    await_time_ms: 5.2,
    service_time_ms: 4.8
  }
  
  // Test throughput calculation
  let throughput = calculate_throughput(disk_metrics)
  assert_eq(throughput.total_throughput_mb_per_sec.round(), 76.29)  // (50 + 30) MB/s
  assert_eq(throughput.total_ops_per_sec, 175.0)                   // 100 + 75
  assert_eq(throughput.read_throughput_mb_per_sec.round(), 50.0)
  assert_eq(throughput.write_throughput_mb_per_sec.round(), 30.0)
  
  // Test disk performance classification
  let classify_disk_performance = fn(metrics: DiskMetrics) {
    let is_high_utilization = metrics.utilization_percent > 80.0
    let is_high_queue = metrics.queue_depth > 5
    let is_high_await = metrics.await_time_ms > 10.0
    
    if is_high_utilization && is_high_queue && is_high_await {
      "CRITICAL"
    } else if is_high_utilization || is_high_queue || is_high_await {
      "WARNING"
    } else {
      "NORMAL"
    }
  }
  
  let performance_status = classify_disk_performance(disk_metrics)
  assert_eq(performance_status, "WARNING")  // High utilization
  
  let good_disk_metrics = { disk_metrics | 
    utilization_percent: 45.0, 
    queue_depth: 2, 
    await_time_ms: 3.1 
  }
  let good_status = classify_disk_performance(good_disk_metrics)
  assert_eq(good_status, "NORMAL")
  
  // Test disk space analysis
  let analyze_disk_space = fn(space_metrics: DiskSpaceMetrics) {
    let usage_percent = (space_metrics.used_bytes as Float / space_metrics.total_bytes as Float) * 100.0
    let inodes_total = space_metrics.inodes_used + space_metrics.inodes_free
    let inodes_usage_percent = if inodes_total > 0 {
      (space_metrics.inodes_used as Float / inodes_total as Float) * 100.0
    } else {
      0.0
    }
    
    let space_status = if usage_percent >= 95.0 {
      "CRITICAL"
    } else if usage_percent >= 85.0 {
      "WARNING"
    } else {
      "NORMAL"
    }
    
    let inodes_status = if inodes_usage_percent >= 95.0 {
      "CRITICAL"
    } else if inodes_usage_percent >= 85.0 {
      "WARNING"
    } else {
      "NORMAL"
    }
    
    {
      usage_percent,
      inodes_usage_percent,
      space_status,
      inodes_status,
      free_gb: space_metrics.free_bytes / (1024 * 1024 * 1024)
    }
  }
  
  let disk_space = {
    device: "/dev/sda1",
    total_bytes: 107374182400,  // 100GB
    used_bytes: 85899345920,    // 80GB
    free_bytes: 21474836480,    // 20GB
    inodes_used: 1500000,
    inodes_free: 500000
  }
  
  let space_analysis = analyze_disk_space(disk_space)
  assert_eq(space_analysis.usage_percent, 80.0)
  assert_eq(space_analysis.inodes_usage_percent.round(), 75.0)
  assert_eq(space_analysis.space_status, "NORMAL")
  assert_eq(space_analysis.inodes_status, "NORMAL")
  assert_eq(space_analysis.free_gb, 20)
  
  // Test disk I/O pattern analysis
  type IoPattern = {
    pattern_type: String,  // "SEQUENTIAL_READ", "SEQUENTIAL_WRITE", "RANDOM", "MIXED"
    avg_request_size_kb: Float,
    read_write_ratio: Float,
    efficiency_score: Float
  }
  
  let analyze_io_pattern = fn(historical_metrics: Array<DiskMetrics>) {
    if historical_metrics.length() < 2 {
      {
        pattern_type: "INSUFFICIENT_DATA",
        avg_request_size_kb: 0.0,
        read_write_ratio: 0.0,
        efficiency_score: 0.0
      }
    } else {
      let total_read_ops = historical_metrics.reduce(fn(sum, m) { sum + m.read_ops_per_sec }, 0.0)
      let total_write_ops = historical_metrics.reduce(fn(sum, m) { sum + m.write_ops_per_sec }, 0.0)
      let total_read_bytes = historical_metrics.reduce(fn(sum, m) { sum + m.read_bytes_per_sec }, 0.0)
      let total_write_bytes = historical_metrics.reduce(fn(sum, m) { sum + m.write_bytes_per_sec }, 0.0)
      
      let avg_read_size_kb = if total_read_ops > 0.0 {
        (total_read_bytes / total_read_ops) / 1024.0
      } else {
        0.0
      }
      
      let avg_write_size_kb = if total_write_ops > 0.0 {
        (total_write_bytes / total_write_ops) / 1024.0
      } else {
        0.0
      }
      
      let avg_request_size_kb = (avg_read_size_kb + avg_write_size_kb) / 2.0
      let read_write_ratio = if total_write_ops > 0.0 {
        total_read_ops / total_write_ops
      } else {
        0.0
      }
      
      // Determine pattern type
      let pattern_type = if avg_request_size_kb > 128.0 {
        if read_write_ratio > 2.0 {
          "SEQUENTIAL_READ"
        } else if read_write_ratio < 0.5 {
          "SEQUENTIAL_WRITE"
        } else {
          "MIXED"
        }
      } else {
        "RANDOM"
      }
      
      // Calculate efficiency score (simplified)
      let avg_utilization = historical_metrics.reduce(fn(sum, m) { sum + m.utilization_percent }, 0.0) / 
                           (historical_metrics.length() as Float)
      let avg_await = historical_metrics.reduce(fn(sum, m) { sum + m.await_time_ms }, 0.0) / 
                     (historical_metrics.length() as Float)
      
      let efficiency_score = if avg_utilization > 0.0 {
        (100.0 / avg_utilization) * (10.0 / avg_await)
      } else {
        0.0
      }
      
      {
        pattern_type,
        avg_request_size_kb,
        read_write_ratio,
        efficiency_score
      }
    }
  }
  
  // Test I/O pattern analysis
  let io_history = [
    { disk_metrics | read_bytes_per_sec: 104857600.0, write_bytes_per_sec: 10485760.0, 
                      read_ops_per_sec: 50.0, write_ops_per_sec: 20.0 },
    { disk_metrics | read_bytes_per_sec: 209715200.0, write_bytes_per_sec: 20971520.0, 
                      read_ops_per_sec: 100.0, write_ops_per_sec: 40.0 },
    { disk_metrics | read_bytes_per_sec: 157286400.0, write_bytes_per_sec: 15728640.0, 
                      read_ops_per_sec: 75.0, write_ops_per_sec: 30.0 }
  ]
  
  let io_pattern = analyze_io_pattern(io_history)
  assert_eq(io_pattern.pattern_type, "SEQUENTIAL_READ")
  assert_eq(io_pattern.avg_request_size_kb.round(), 2048.0)  // 2MB average
  assert_eq(io_pattern.read_write_ratio.round(), 2.5)
  assert_true(io_pattern.efficiency_score > 0.0)
}

// Test 4: Network Monitoring
test "network monitoring and bandwidth analysis" {
  // Define network interface metrics
  type NetworkMetrics = {
    interface: String,
    rx_bytes_per_sec: Float,
    tx_bytes_per_sec: Float,
    rx_packets_per_sec: Float,
    tx_packets_per_sec: Float,
    rx_errors_per_sec: Float,
    tx_errors_per_sec: Float,
    rx_dropped_per_sec: Float,
    tx_dropped_per_sec: Float
  }
  
  // Define network connection metrics
  type ConnectionMetrics = {
    established: Int,
    listening: Int,
    time_wait: Int,
    close_wait: Int,
    syn_sent: Int,
    syn_recv: Int
  }
  
  // Calculate network utilization
  let calculate_network_utilization = fn(metrics: NetworkMetrics, interface_speed_mbps: Float) {
    let total_bps = (metrics.rx_bytes_per_sec + metrics.tx_bytes_per_sec) * 8.0  // Convert to bits
    let interface_capacity_bps = interface_speed_mbps * 1000000.0  // Convert Mbps to bps
    
    let utilization_percent = if interface_capacity_bps > 0.0 {
      (total_bps / interface_capacity_bps) * 100.0
    } else {
      0.0
    }
    
    let error_rate = if (metrics.rx_packets_per_sec + metrics.tx_packets_per_sec) > 0.0 {
      ((metrics.rx_errors_per_sec + metrics.tx_errors_per_sec) / 
       (metrics.rx_packets_per_sec + metrics.tx_packets_per_sec)) * 100.0
    } else {
      0.0
    }
    
    let drop_rate = if (metrics.rx_packets_per_sec + metrics.tx_packets_per_sec) > 0.0 {
      ((metrics.rx_dropped_per_sec + metrics.tx_dropped_per_sec) / 
       (metrics.rx_packets_per_sec + metrics.tx_packets_per_sec)) * 100.0
    } else {
      0.0
    }
    
    {
      utilization_percent,
      error_rate,
      drop_rate,
      total_throughput_mbps: total_bps / 1000000.0
    }
  }
  
  // Create sample network metrics
  let network_metrics = {
    interface: "eth0",
    rx_bytes_per_sec: 10485760.0,   // 10MB/s
    tx_bytes_per_sec: 5242880.0,    // 5MB/s
    rx_packets_per_sec: 10000.0,
    tx_packets_per_sec: 5000.0,
    rx_errors_per_sec: 5.0,
    tx_errors_per_sec: 2.0,
    rx_dropped_per_sec: 1.0,
    tx_dropped_per_sec: 0.0
  }
  
  // Test network utilization calculation
  let utilization = calculate_network_utilization(network_metrics, 1000.0)  // 1Gbps interface
  assert_eq(utilization.total_throughput_mbps, 120.0)  // (10 + 5) MB/s * 8 = 120 Mbps
  assert_eq(utilization.utilization_percent, 12.0)     // 120 / 1000 * 100
  assert_eq(utilization.error_rate.round(), 0.05)      // (5 + 2) / (10000 + 5000) * 100
  assert_eq(utilization.drop_rate.round(), 0.01)       // (1 + 0) / (10000 + 5000) * 100
  
  // Test network health assessment
  let assess_network_health = fn(utilization: {utilization_percent: Float, error_rate: Float, drop_rate: Float}) {
    let high_utilization = utilization.utilization_percent > 80.0
    let high_error_rate = utilization.error_rate > 1.0
    let high_drop_rate = utilization.drop_rate > 0.5
    
    if high_error_rate || high_drop_rate {
      "CRITICAL"
    } else if high_utilization {
      "WARNING"
    } else {
      "NORMAL"
    }
  }
  
  let network_health = assess_network_health(utilization)
  assert_eq(network_health, "NORMAL")
  
  let high_error_utilization = { utilization | error_rate: 2.0 }
  let error_health = assess_network_health(high_error_utilization)
  assert_eq(error_health, "CRITICAL")
  
  // Test connection analysis
  let analyze_connections = fn(conn_metrics: ConnectionMetrics) {
    let total_connections = conn_metrics.established + conn_metrics.listening + 
                           conn_metrics.time_wait + conn_metrics.close_wait + 
                           conn_metrics.syn_sent + conn_metrics.syn_recv
    
    let active_connections = conn_metrics.established + conn_metrics.syn_sent + conn_metrics.syn_recv
    let waiting_connections = conn_metrics.time_wait + conn_metrics.close_wait
    
    let connection_health = if total_connections > 10000 {
      "WARNING"
    } else if conn_metrics.time_wait > total_connections / 2 {
      "WARNING"
    } else {
      "NORMAL"
    }
    
    {
      total_connections,
      active_connections,
      waiting_connections,
      connection_health
    }
  }
  
  let connection_metrics = {
    established: 500,
    listening: 20,
    time_wait: 100,
    close_wait: 50,
    syn_sent: 10,
    syn_recv: 5
  }
  
  let connection_analysis = analyze_connections(connection_metrics)
  assert_eq(connection_analysis.total_connections, 685)
  assert_eq(connection_analysis.active_connections, 515)
  assert_eq(connection_analysis.waiting_connections, 150)
  assert_eq(connection_analysis.connection_health, "NORMAL")
  
  // Test network bandwidth prediction
  type BandwidthPrediction = {
    predicted_rx_mbps: Float,
    predicted_tx_mbps: Float,
    confidence_interval: (Float, Float),
    trend: String
  }
  
  let predict_bandwidth = fn(historical_metrics: Array<NetworkMetrics>, prediction_window_minutes: Int) {
    if historical_metrics.length() < 3 {
      {
        predicted_rx_mbps: 0.0,
        predicted_tx_mbps: 0.0,
        confidence_interval: (0.0, 0.0),
        trend: "INSUFFICIENT_DATA"
      }
    } else {
      // Simple linear extrapolation
      let rx_rates = historical_metrics.map(fn(m) { m.rx_bytes_per_sec / 125000.0 })  // Convert to Mbps
      let tx_rates = historical_metrics.map(fn(m) { m.tx_bytes_per_sec / 125000.0 })  // Convert to Mbps
      
      let rx_slope = (rx_rates[rx_rates.length() - 1] - rx_rates[0]) / (rx_rates.length() as Float)
      let tx_slope = (tx_rates[tx_rates.length() - 1] - tx_rates[0]) / (tx_rates.length() as Float)
      
      let latest_rx = rx_rates[rx_rates.length() - 1]
      let latest_tx = tx_rates[tx_rates.length() - 1]
      
      let predicted_rx = latest_rx + (rx_slope * prediction_window_minutes as Float)
      let predicted_tx = latest_tx + (tx_slope * prediction_window_minutes as Float)
      
      // Calculate variance for confidence interval (simplified)
      let rx_variance = rx_rates.reduce(fn(sum, rate) { 
        let mean = rx_rates.reduce(fn(s, r) { s + r }, 0.0) / rx_rates.length() as Float
        sum + ((rate - mean) * (rate - mean))
      }, 0.0) / rx_rates.length() as Float
      
      let confidence = rx_variance.sqrt() * 2.0  // 95% confidence interval
      
      let trend = if (rx_slope + tx_slope) > 1.0 {
        "INCREASING"
      } else if (rx_slope + tx_slope) < -1.0 {
        "DECREASING"
      } else {
        "STABLE"
      }
      
      {
        predicted_rx_mbps: predicted_rx,
        predicted_tx_mbps: predicted_tx,
        confidence_interval: (predicted_rx - confidence, predicted_rx + confidence),
        trend
      }
    }
  }
  
  // Test bandwidth prediction
  let network_history = [
    { network_metrics | rx_bytes_per_sec: 8388608.0, tx_bytes_per_sec: 4194304.0 },   // 8MB/s, 4MB/s
    { network_metrics | rx_bytes_per_sec: 10485760.0, tx_bytes_per_sec: 5242880.0 },  // 10MB/s, 5MB/s
    { network_metrics | rx_bytes_per_sec: 12582912.0, tx_bytes_per_sec: 6291456.0 },  // 12MB/s, 6MB/s
    { network_metrics | rx_bytes_per_sec: 14680064.0, tx_bytes_per_sec: 7340032.0 },  // 14MB/s, 7MB/s
    { network_metrics | rx_bytes_per_sec: 16777216.0, tx_bytes_per_sec: 8388608.0 }   // 16MB/s, 8MB/s
  ]
  
  let bandwidth_prediction = predict_bandwidth(network_history, 10)  // 10 minutes ahead
  assert_eq(bandwidth_prediction.predicted_rx_mbps.round(), 144.0)   // 16MB/s + trend * 10
  assert_eq(bandwidth_prediction.predicted_tx_mbps.round(), 72.0)    // 8MB/s + trend * 10
  assert_eq(bandwidth_prediction.trend, "INCREASING")
}

// Test 5: Resource Threshold Monitoring
test "resource threshold monitoring and alerting" {
  // Define threshold configuration
  type ThresholdConfig = {
    metric_name: String,
    warning_threshold: Float,
    critical_threshold: Float,
    operator: String,  // ">", "<", ">=", "<=", "=="
    duration_seconds: Int  // How long threshold must be exceeded
  }
  
  // Define threshold violation
  type ThresholdViolation = {
    metric_name: String,
    current_value: Float,
    threshold_value: Float,
    severity: String,
    duration_seconds: Int,
    first_occurrence: Int
  }
  
  // Define monitoring state
  type MonitoringState = {
    active_violations: Array<ThresholdViolation>,
    metric_history: Array[(String, Float, Int)>  // (metric_name, value, timestamp)
  }
  
  // Check threshold violation
  let check_threshold = fn(config: ThresholdConfig, value: Float) {
    let is_violation = match config.operator {
      ">" => value > config.critical_threshold,
      "<" => value < config.critical_threshold,
      ">=" => value >= config.critical_threshold,
      "<=" => value <= config.critical_threshold,
      "==" => value == config.critical_threshold,
      _ => false
    }
    
    let severity = if is_violation {
      "CRITICAL"
    } else {
      let is_warning = match config.operator {
        ">" => value > config.warning_threshold,
        "<" => value < config.warning_threshold,
        ">=" => value >= config.warning_threshold,
        "<=" => value <= config.warning_threshold,
        "==" => value == config.warning_threshold,
        _ => false
      }
      
      if is_warning {
        "WARNING"
      } else {
        "NORMAL"
      }
    }
    
    {
      is_violation,
      severity
    }
  }
  
  // Update monitoring state with new metrics
  let update_monitoring_state = fn(state: MonitoringState, metrics: Array<(String, Float)>, 
                                  timestamp: Int, configs: Array<ThresholdConfig>) {
    let mut updated_violations = []
    let mut updated_history = state.metric_history
    
    // Add new metrics to history
    for (metric_name, value) in metrics {
      updated_history = updated_history.push((metric_name, value, timestamp))
    }
    
    // Keep only last 1000 entries
    if updated_history.length() > 1000 {
      updated_history = updated_history.slice(updated_history.length() - 1000, updated_history.length())
    }
    
    // Check each metric against its thresholds
    for (metric_name, value) in metrics {
      let config = configs.find(fn(c) { c.metric_name == metric_name })
      
      match config {
        Some(threshold_config) => {
          let check_result = check_threshold(threshold_config, value)
          
          if check_result.severity != "NORMAL" {
            // Check if this is an existing violation
            let existing_violation = state.active_violations.find(fn(v) { 
              v.metric_name == metric_name 
            })
            
            match existing_violation {
              Some(violation) => {
                // Update existing violation
                let duration = timestamp - violation.first_occurrence
                let updated_violation = {
                  metric_name,
                  current_value: value,
                  threshold_value: threshold_config.critical_threshold,
                  severity: check_result.severity,
                  duration_seconds: duration,
                  first_occurrence: violation.first_occurrence
                }
                updated_violations = updated_violations.push(updated_violation)
              }
              None => {
                // New violation
                let new_violation = {
                  metric_name,
                  current_value: value,
                  threshold_value: threshold_config.critical_threshold,
                  severity: check_result.severity,
                  duration_seconds: 0,
                  first_occurrence: timestamp
                }
                updated_violations = updated_violations.push(new_violation)
              }
            }
          }
        }
        None => {}  // No threshold config for this metric
      }
    }
    
    // Remove violations that are no longer active
    let active_metric_names = metrics.map(fn(m) { m.0 })
    let filtered_violations = updated_violations.filter(fn(v) { 
      active_metric_names.contains(v.metric_name) 
    })
    
    {
      active_violations: filtered_violations,
      metric_history: updated_history
    }
  }
  
  // Create threshold configurations
  let threshold_configs = [
    {
      metric_name: "cpu_usage",
      warning_threshold: 70.0,
      critical_threshold: 90.0,
      operator: ">",
      duration_seconds: 300  // 5 minutes
    },
    {
      metric_name: "memory_usage",
      warning_threshold: 80.0,
      critical_threshold: 95.0,
      operator: ">",
      duration_seconds: 180  // 3 minutes
    },
    {
      metric_name: "disk_usage",
      warning_threshold: 85.0,
      critical_threshold: 95.0,
      operator: ">",
      duration_seconds: 600  // 10 minutes
    },
    {
      metric_name: "network_error_rate",
      warning_threshold: 1.0,
      critical_threshold: 5.0,
      operator: ">",
      duration_seconds: 120  // 2 minutes
    }
  ]
  
  // Initialize monitoring state
  let initial_state = {
    active_violations: [],
    metric_history: []
  }
  
  // Test normal metrics
  let normal_metrics = [
    ("cpu_usage", 45.0),
    ("memory_usage", 60.0),
    ("disk_usage", 70.0),
    ("network_error_rate", 0.1)
  ]
  
  let state1 = update_monitoring_state(initial_state, normal_metrics, 1640995200, threshold_configs)
  assert_eq(state1.active_violations.length(), 0)
  
  // Test warning metrics
  let warning_metrics = [
    ("cpu_usage", 75.0),   // Above warning threshold
    ("memory_usage", 85.0), // Above warning threshold
    ("disk_usage", 70.0),
    ("network_error_rate", 0.5)
  ]
  
  let state2 = update_monitoring_state(state1, warning_metrics, 1640995300, threshold_configs)
  assert_eq(state2.active_violations.length(), 2)
  assert_eq(state2.active_violations[0].severity, "WARNING")
  assert_eq(state2.active_violations[1].severity, "WARNING")
  
  // Test critical metrics
  let critical_metrics = [
    ("cpu_usage", 95.0),   // Above critical threshold
    ("memory_usage", 97.0), // Above critical threshold
    ("disk_usage", 70.0),
    ("network_error_rate", 0.5)
  ]
  
  let state3 = update_monitoring_state(state2, critical_metrics, 1640995400, threshold_configs)
  assert_eq(state3.active_violations.length(), 2)
  assert_eq(state3.active_violations[0].severity, "CRITICAL")
  assert_eq(state3.active_violations[1].severity, "CRITICAL")
  assert_eq(state3.active_violations[0].duration_seconds, 100)  // 200 seconds total
  
  // Test violation resolution
  let resolved_metrics = [
    ("cpu_usage", 50.0),   // Back to normal
    ("memory_usage", 60.0), // Back to normal
    ("disk_usage", 70.0),
    ("network_error_rate", 0.5)
  ]
  
  let state4 = update_monitoring_state(state3, resolved_metrics, 1640995500, threshold_configs)
  assert_eq(state4.active_violations.length(), 0)  // All violations resolved
  
  // Test alert generation
  type Alert = {
    alert_id: String,
    metric_name: String,
    severity: String,
    message: String,
    timestamp: Int,
    duration_seconds: Int
  }
  
  let generate_alerts = fn(violations: Array<ThresholdViolation>) {
    violations.filter(fn(v) { 
      (v.severity == "CRITICAL" && v.duration_seconds >= 60) ||
      (v.severity == "WARNING" && v.duration_seconds >= 300)
    }).map(fn(v) {
      {
        alert_id: "alert-" + v.metric_name + "-" + v.first_occurrence.to_string(),
        metric_name: v.metric_name,
        severity: v.severity,
        message: v.metric_name + " is " + v.severity + ": " + 
                v.current_value.to_string() + " (threshold: " + 
                v.threshold_value.to_string() + ")",
        timestamp: v.first_occurrence + v.duration_seconds,
        duration_seconds: v.duration_seconds
      }
    })
  }
  
  // Test alert generation
  let long_violation_state = {
    active_violations: [
      {
        metric_name: "cpu_usage",
        current_value: 95.0,
        threshold_value: 90.0,
        severity: "CRITICAL",
        duration_seconds: 120,
        first_occurrence: 1640995200
      },
      {
        metric_name: "memory_usage",
        current_value: 85.0,
        threshold_value: 80.0,
        severity: "WARNING",
        duration_seconds: 400,
        first_occurrence: 1640995100
      },
      {
        metric_name: "network_error_rate",
        current_value: 6.0,
        threshold_value: 5.0,
        severity: "CRITICAL",
        duration_seconds: 30,
        first_occurrence: 1640995370
      }
    ],
    metric_history: []
  }
  
  let alerts = generate_alerts(long_violation_state.active_violations)
  assert_eq(alerts.length(), 2)  // Only CPU and memory violations meet duration criteria
  
  let cpu_alert = alerts.find(fn(a) { a.metric_name == "cpu_usage" })
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.severity, "CRITICAL")
      assert_eq(alert.duration_seconds, 120)
      assert_true(alert.message.contains("cpu_usage is CRITICAL"))
    }
    None => assert_true(false)
  }
  
  let memory_alert = alerts.find(fn(a) { a.metric_name == "memory_usage" })
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.severity, "WARNING")
      assert_eq(alert.duration_seconds, 400)
      assert_true(alert.message.contains("memory_usage is WARNING"))
    }
    None => assert_true(false)
  }
  
  // Test trend-based alerting
  let analyze_metric_trend = fn(history: Array<(String, Float, Int)>, metric_name: String, 
                               window_minutes: Int) {
    let metric_history = history.filter(fn(h) { h.0 == metric_name })
    
    if metric_history.length() < 3 {
      "INSUFFICIENT_DATA"
    } else {
      // Get recent values within window
      let current_time = metric_history[metric_history.length() - 1].2
      let window_start = current_time - (window_minutes * 60)
      let recent_values = metric_history.filter(fn(h) { h.2 >= window_start })
        .map(fn(h) { h.1 })
      
      if recent_values.length() < 3 {
        "INSUFFICIENT_DATA"
      } else {
        // Simple trend calculation
        let first_value = recent_values[0]
        let last_value = recent_values[recent_values.length() - 1]
        let change_rate = (last_value - first_value) / first_value
        
        if change_rate > 0.5 {
          "RAPIDLY_INCREASING"
        } else if change_rate > 0.2 {
          "MODERATELY_INCREASING"
        } else if change_rate < -0.3 {
          "RAPIDLY_DECREASING"
        } else {
          "STABLE"
        }
      }
    }
  }
  
  // Create trend history
  let trend_history = [
    ("cpu_usage", 50.0, 1640995200),
    ("cpu_usage", 60.0, 1640995260),
    ("cpu_usage", 72.0, 1640995320),
    ("cpu_usage", 85.0, 1640995380),
    ("cpu_usage", 92.0, 1640995440)
  ]
  
  let cpu_trend = analyze_metric_trend(trend_history, "cpu_usage", 15)
  assert_eq(cpu_trend, "RAPIDLY_INCREASING")
}