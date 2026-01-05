// Azimuth Performance Monitoring Tests
// This file contains test cases for performance monitoring functionality

// Test 1: CPU Performance Monitoring
test "cpu performance monitoring" {
  // Define CPU metrics types
  type CPUMetrics = {
    timestamp: Int,
    cpu_usage_percent: Float,
    load_average_1min: Float,
    load_average_5min: Float,
    load_average_15min: Float,
    context_switches: Int,
    processes_running: Int,
    processes_total: Int
  }
  
  type CPUAlertThresholds = {
    warning_percent: Float,
    critical_percent: Float,
    warning_load_average: Float,
    critical_load_average: Float
  }
  
  // Create sample CPU metrics
  let cpu_metrics = [
    {
      timestamp: 1640995200,
      cpu_usage_percent: 25.5,
      load_average_1min: 0.8,
      load_average_5min: 0.7,
      load_average_15min: 0.6,
      context_switches: 1250,
      processes_running: 2,
      processes_total: 156
    },
    {
      timestamp: 1640995260,
      cpu_usage_percent: 45.2,
      load_average_1min: 1.2,
      load_average_5min: 0.9,
      load_average_15min: 0.7,
      context_switches: 1580,
      processes_running: 3,
      processes_total: 158
    },
    {
      timestamp: 1640995320,
      cpu_usage_percent: 85.7,
      load_average_1min: 2.5,
      load_average_5min: 1.8,
      load_average_15min: 1.2,
      context_switches: 2350,
      processes_running: 5,
      processes_total: 160
    }
  ]
  
  // Define alert thresholds
  let alert_thresholds = {
    warning_percent: 70.0,
    critical_percent: 90.0,
    warning_load_average: 2.0,
    critical_load_average: 3.0
  }
  
  // Define CPU monitoring function
  let monitor_cpu = fn(metrics: CPUMetrics, thresholds: CPUAlertThresholds) {
    let mut alerts = []
    
    // Check CPU usage
    if metrics.cpu_usage_percent >= thresholds.critical_percent {
      alerts = alerts.push("CRITICAL: CPU usage at " + metrics.cpu_usage_percent.to_string() + "%")
    } else if metrics.cpu_usage_percent >= thresholds.warning_percent {
      alerts = alerts.push("WARNING: CPU usage at " + metrics.cpu_usage_percent.to_string() + "%")
    }
    
    // Check load average
    if metrics.load_average_1min >= thresholds.critical_load_average {
      alerts = alerts.push("CRITICAL: Load average at " + metrics.load_average_1min.to_string())
    } else if metrics.load_average_1min >= thresholds.warning_load_average {
      alerts = alerts.push("WARNING: Load average at " + metrics.load_average_1min.to_string())
    }
    
    // Check for excessive context switches
    if metrics.context_switches > 2000 {
      alerts = alerts.push("WARNING: High context switches: " + metrics.context_switches.to_string())
    }
    
    {
      metrics: metrics,
      alerts: alerts,
      status: if alerts.length() > 0 { "alert" } else { "normal" }
    }
  }
  
  // Monitor each CPU metric
  let monitoring_results = cpu_metrics.map(fn(m) { monitor_cpu(m, alert_thresholds) })
  
  // Verify the results
  assert_eq(monitoring_results.length(), 3)
  
  // First metric should be normal
  assert_eq(monitoring_results[0].status, "normal")
  assert_eq(monitoring_results[0].alerts.length(), 0)
  
  // Second metric should have warning
  assert_eq(monitoring_results[1].status, "alert")
  assert_eq(monitoring_results[1].alerts.length(), 1)
  assert_true(monitoring_results[1].alerts[0].contains("WARNING: CPU usage"))
  
  // Third metric should have multiple alerts
  assert_eq(monitoring_results[2].status, "alert")
  assert_eq(monitoring_results[2].alerts.length(), 3)
  assert_true(monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: CPU usage") }))
  assert_true(monitoring_results[2].alerts.some(fn(a) { a.contains("WARNING: Load average") }))
  assert_true(monitoring_results[2].alerts.some(fn(a) { a.contains("WARNING: High context switches") }))
}

// Test 2: Memory Performance Monitoring
test "memory performance monitoring" {
  // Define memory metrics types
  type MemoryMetrics = {
    timestamp: Int,
    total_memory_mb: Int,
    used_memory_mb: Int,
    free_memory_mb: Int,
    cached_memory_mb: Int,
    swap_total_mb: Int,
    swap_used_mb: Int,
    swap_free_mb: Int,
    page_faults: Int
  }
  
  type MemoryAlertThresholds = {
    warning_percent: Float,
    critical_percent: Float,
    swap_warning_percent: Float,
    swap_critical_percent: Float
  }
  
  // Create sample memory metrics
  let memory_metrics = [
    {
      timestamp: 1640995200,
      total_memory_mb: 8192,
      used_memory_mb: 4096,
      free_memory_mb: 2048,
      cached_memory_mb: 2048,
      swap_total_mb: 2048,
      swap_used_mb: 0,
      swap_free_mb: 2048,
      page_faults: 125
    },
    {
      timestamp: 1640995260,
      total_memory_mb: 8192,
      used_memory_mb: 6144,
      free_memory_mb: 1024,
      cached_memory_mb: 1024,
      swap_total_mb: 2048,
      swap_used_mb: 512,
      swap_free_mb: 1536,
      page_faults: 250
    },
    {
      timestamp: 1640995320,
      total_memory_mb: 8192,
      used_memory_mb: 7680,
      free_memory_mb: 256,
      cached_memory_mb: 256,
      swap_total_mb: 2048,
      swap_used_mb: 1024,
      swap_free_mb: 1024,
      page_faults: 500
    }
  ]
  
  // Define alert thresholds
  let memory_alert_thresholds = {
    warning_percent: 75.0,
    critical_percent: 90.0,
    swap_warning_percent: 25.0,
    swap_critical_percent: 50.0
  }
  
  // Define memory monitoring function
  let monitor_memory = fn(metrics: MemoryMetrics, thresholds: MemoryAlertThresholds) {
    let mut alerts = []
    
    // Calculate memory usage percentage
    let memory_usage_percent = (metrics.used_memory_mb.to_float() / metrics.total_memory_mb.to_float()) * 100.0
    
    // Check memory usage
    if memory_usage_percent >= thresholds.critical_percent {
      alerts = alerts.push("CRITICAL: Memory usage at " + memory_usage_percent.to_string() + "%")
    } else if memory_usage_percent >= thresholds.warning_percent {
      alerts = alerts.push("WARNING: Memory usage at " + memory_usage_percent.to_string() + "%")
    }
    
    // Calculate swap usage percentage
    let swap_usage_percent = if metrics.swap_total_mb > 0 {
      (metrics.swap_used_mb.to_float() / metrics.swap_total_mb.to_float()) * 100.0
    } else {
      0.0
    }
    
    // Check swap usage
    if swap_usage_percent >= thresholds.swap_critical_percent {
      alerts = alerts.push("CRITICAL: Swap usage at " + swap_usage_percent.to_string() + "%")
    } else if swap_usage_percent >= thresholds.swap_warning_percent {
      alerts = alerts.push("WARNING: Swap usage at " + swap_usage_percent.to_string() + "%")
    }
    
    // Check for excessive page faults
    if metrics.page_faults > 300 {
      alerts = alerts.push("WARNING: High page faults: " + metrics.page_faults.to_string())
    }
    
    {
      metrics: metrics,
      memory_usage_percent: memory_usage_percent,
      swap_usage_percent: swap_usage_percent,
      alerts: alerts,
      status: if alerts.length() > 0 { "alert" } else { "normal" }
    }
  }
  
  // Monitor each memory metric
  let memory_monitoring_results = memory_metrics.map(fn(m) { monitor_memory(m, memory_alert_thresholds) })
  
  // Verify the results
  assert_eq(memory_monitoring_results.length(), 3)
  
  // First metric should be normal
  assert_eq(memory_monitoring_results[0].status, "normal")
  assert_eq(memory_monitoring_results[0].memory_usage_percent, 50.0)
  assert_eq(memory_monitoring_results[0].swap_usage_percent, 0.0)
  assert_eq(memory_monitoring_results[0].alerts.length(), 0)
  
  // Second metric should have warnings
  assert_eq(memory_monitoring_results[1].status, "alert")
  assert_eq(memory_monitoring_results[1].memory_usage_percent, 75.0)
  assert_eq(memory_monitoring_results[1].swap_usage_percent, 25.0)
  assert_eq(memory_monitoring_results[1].alerts.length(), 2)
  assert_true(memory_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: Memory usage") }))
  assert_true(memory_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: Swap usage") }))
  
  // Third metric should have critical alerts
  assert_eq(memory_monitoring_results[2].status, "alert")
  assert_eq(memory_monitoring_results[2].memory_usage_percent, 93.75)
  assert_eq(memory_monitoring_results[2].swap_usage_percent, 50.0)
  assert_eq(memory_monitoring_results[2].alerts.length(), 3)
  assert_true(memory_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: Memory usage") }))
  assert_true(memory_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: Swap usage") }))
  assert_true(memory_monitoring_results[2].alerts.some(fn(a) { a.contains("WARNING: High page faults") }))
}

// Test 3: Disk I/O Performance Monitoring
test "disk i/o performance monitoring" {
  // Define disk I/O metrics types
  type DiskIOMetrics = {
    timestamp: Int,
    device_name: String,
    read_bytes_per_sec: Int,
    write_bytes_per_sec: Int,
    read_ops_per_sec: Int,
    write_ops_per_sec: Int,
    avg_queue_size: Float,
    avg_wait_time_ms: Float,
    utilization_percent: Float
  }
  
  type DiskIOAlertThresholds = {
    read_throughput_warning_mb: Int,
    read_throughput_critical_mb: Int,
    write_throughput_warning_mb: Int,
    write_throughput_critical_mb: Int,
    wait_time_warning_ms: Float,
    wait_time_critical_ms: Float,
    utilization_warning_percent: Float,
    utilization_critical_percent: Float
  }
  
  // Create sample disk I/O metrics
  let disk_io_metrics = [
    {
      timestamp: 1640995200,
      device_name: "/dev/sda",
      read_bytes_per_sec: 1048576,    // 1 MB/s
      write_bytes_per_sec: 2097152,   // 2 MB/s
      read_ops_per_sec: 100,
      write_ops_per_sec: 150,
      avg_queue_size: 0.5,
      avg_wait_time_ms: 2.5,
      utilization_percent: 15.0
    },
    {
      timestamp: 1640995260,
      device_name: "/dev/sda",
      read_bytes_per_sec: 10485760,   // 10 MB/s
      write_bytes_per_sec: 20971520,  // 20 MB/s
      read_ops_per_sec: 500,
      write_ops_per_sec: 750,
      avg_queue_size: 2.0,
      avg_wait_time_ms: 10.0,
      utilization_percent: 60.0
    },
    {
      timestamp: 1640995320,
      device_name: "/dev/sda",
      read_bytes_per_sec: 52428800,   // 50 MB/s
      write_bytes_per_sec: 104857600, // 100 MB/s
      read_ops_per_sec: 1000,
      write_ops_per_sec: 1500,
      avg_queue_size: 5.0,
      avg_wait_time_ms: 25.0,
      utilization_percent: 95.0
    }
  ]
  
  // Define alert thresholds
  let disk_io_alert_thresholds = {
    read_throughput_warning_mb: 20,   // 20 MB/s
    read_throughput_critical_mb: 40,  // 40 MB/s
    write_throughput_warning_mb: 40,  // 40 MB/s
    write_throughput_critical_mb: 80, // 80 MB/s
    wait_time_warning_ms: 10.0,
    wait_time_critical_ms: 20.0,
    utilization_warning_percent: 70.0,
    utilization_critical_percent: 90.0
  }
  
  // Define disk I/O monitoring function
  let monitor_disk_io = fn(metrics: DiskIOMetrics, thresholds: DiskIOAlertThresholds) {
    let mut alerts = []
    
    // Convert bytes per second to MB per second
    let read_throughput_mb = metrics.read_bytes_per_sec / 1048576
    let write_throughput_mb = metrics.write_bytes_per_sec / 1048576
    
    // Check read throughput
    if read_throughput_mb >= thresholds.read_throughput_critical_mb {
      alerts = alerts.push("CRITICAL: High read throughput: " + read_throughput_mb.to_string() + " MB/s")
    } else if read_throughput_mb >= thresholds.read_throughput_warning_mb {
      alerts = alerts.push("WARNING: High read throughput: " + read_throughput_mb.to_string() + " MB/s")
    }
    
    // Check write throughput
    if write_throughput_mb >= thresholds.write_throughput_critical_mb {
      alerts = alerts.push("CRITICAL: High write throughput: " + write_throughput_mb.to_string() + " MB/s")
    } else if write_throughput_mb >= thresholds.write_throughput_warning_mb {
      alerts = alerts.push("WARNING: High write throughput: " + write_throughput_mb.to_string() + " MB/s")
    }
    
    // Check wait time
    if metrics.avg_wait_time_ms >= thresholds.wait_time_critical_ms {
      alerts = alerts.push("CRITICAL: High wait time: " + metrics.avg_wait_time_ms.to_string() + " ms")
    } else if metrics.avg_wait_time_ms >= thresholds.wait_time_warning_ms {
      alerts = alerts.push("WARNING: High wait time: " + metrics.avg_wait_time_ms.to_string() + " ms")
    }
    
    // Check utilization
    if metrics.utilization_percent >= thresholds.utilization_critical_percent {
      alerts = alerts.push("CRITICAL: High utilization: " + metrics.utilization_percent.to_string() + "%")
    } else if metrics.utilization_percent >= thresholds.utilization_warning_percent {
      alerts = alerts.push("WARNING: High utilization: " + metrics.utilization_percent.to_string() + "%")
    }
    
    {
      metrics: metrics,
      read_throughput_mb: read_throughput_mb,
      write_throughput_mb: write_throughput_mb,
      alerts: alerts,
      status: if alerts.length() > 0 { "alert" } else { "normal" }
    }
  }
  
  // Monitor each disk I/O metric
  let disk_io_monitoring_results = disk_io_metrics.map(fn(m) { monitor_disk_io(m, disk_io_alert_thresholds) })
  
  // Verify the results
  assert_eq(disk_io_monitoring_results.length(), 3)
  
  // First metric should be normal
  assert_eq(disk_io_monitoring_results[0].status, "normal")
  assert_eq(disk_io_monitoring_results[0].read_throughput_mb, 1)
  assert_eq(disk_io_monitoring_results[0].write_throughput_mb, 2)
  assert_eq(disk_io_monitoring_results[0].alerts.length(), 0)
  
  // Second metric should have warnings
  assert_eq(disk_io_monitoring_results[1].status, "alert")
  assert_eq(disk_io_monitoring_results[1].read_throughput_mb, 10)
  assert_eq(disk_io_monitoring_results[1].write_throughput_mb, 20)
  assert_eq(disk_io_monitoring_results[1].alerts.length(), 2)
  assert_true(disk_io_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High wait time") }))
  assert_true(disk_io_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High utilization") }))
  
  // Third metric should have critical alerts
  assert_eq(disk_io_monitoring_results[2].status, "alert")
  assert_eq(disk_io_monitoring_results[2].read_throughput_mb, 50)
  assert_eq(disk_io_monitoring_results[2].write_throughput_mb, 100)
  assert_eq(disk_io_monitoring_results[2].alerts.length(), 5)
  assert_true(disk_io_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High read throughput") }))
  assert_true(disk_io_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High write throughput") }))
  assert_true(disk_io_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High wait time") }))
  assert_true(disk_io_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High utilization") }))
}

// Test 4: Network Performance Monitoring
test "network performance monitoring" {
  // Define network metrics types
  type NetworkMetrics = {
    timestamp: Int,
    interface_name: String,
    bytes_received_per_sec: Int,
    bytes_sent_per_sec: Int,
    packets_received_per_sec: Int,
    packets_sent_per_sec: Int,
    errors_in: Int,
    errors_out: Int,
    drops_in: Int,
    drops_out: Int
  }
  
  type NetworkAlertThresholds = {
    bandwidth_warning_mb: Int,
    bandwidth_critical_mb: Int,
    packet_rate_warning: Int,
    packet_rate_critical: Int,
    error_rate_warning_percent: Float,
    error_rate_critical_percent: Float
  }
  
  // Create sample network metrics
  let network_metrics = [
    {
      timestamp: 1640995200,
      interface_name: "eth0",
      bytes_received_per_sec: 1048576,    // 1 MB/s
      bytes_sent_per_sec: 2097152,       // 2 MB/s
      packets_received_per_sec: 1000,
      packets_sent_per_sec: 1500,
      errors_in: 0,
      errors_out: 0,
      drops_in: 0,
      drops_out: 0
    },
    {
      timestamp: 1640995260,
      interface_name: "eth0",
      bytes_received_per_sec: 20971520,   // 20 MB/s
      bytes_sent_per_sec: 31457280,      // 30 MB/s
      packets_received_per_sec: 10000,
      packets_sent_per_sec: 15000,
      errors_in: 5,
      errors_out: 3,
      drops_in: 2,
      drops_out: 1
    },
    {
      timestamp: 1640995320,
      interface_name: "eth0",
      bytes_received_per_sec: 52428800,   // 50 MB/s
      bytes_sent_per_sec: 73400320,      // 70 MB/s
      packets_received_per_sec: 25000,
      packets_sent_per_sec: 35000,
      errors_in: 50,
      errors_out: 30,
      drops_in: 20,
      drops_out: 10
    }
  ]
  
  // Define alert thresholds
  let network_alert_thresholds = {
    bandwidth_warning_mb: 40,   // 40 MB/s
    bandwidth_critical_mb: 80,  // 80 MB/s
    packet_rate_warning: 20000, // 20,000 packets/s
    packet_rate_critical: 40000, // 40,000 packets/s
    error_rate_warning_percent: 0.1, // 0.1%
    error_rate_critical_percent: 0.5  // 0.5%
  }
  
  // Define network monitoring function
  let monitor_network = fn(metrics: NetworkMetrics, thresholds: NetworkAlertThresholds) {
    let mut alerts = []
    
    // Convert bytes per second to MB per second
    let receive_throughput_mb = metrics.bytes_received_per_sec / 1048576
    let send_throughput_mb = metrics.bytes_sent_per_sec / 1048576
    let total_throughput_mb = receive_throughput_mb + send_throughput_mb
    
    // Check bandwidth
    if total_throughput_mb >= thresholds.bandwidth_critical_mb {
      alerts = alerts.push("CRITICAL: High bandwidth usage: " + total_throughput_mb.to_string() + " MB/s")
    } else if total_throughput_mb >= thresholds.bandwidth_warning_mb {
      alerts = alerts.push("WARNING: High bandwidth usage: " + total_throughput_mb.to_string() + " MB/s")
    }
    
    // Check packet rate
    let total_packet_rate = metrics.packets_received_per_sec + metrics.packets_sent_per_sec
    if total_packet_rate >= thresholds.packet_rate_critical {
      alerts = alerts.push("CRITICAL: High packet rate: " + total_packet_rate.to_string() + " packets/s")
    } else if total_packet_rate >= thresholds.packet_rate_warning {
      alerts = alerts.push("WARNING: High packet rate: " + total_packet_rate.to_string() + " packets/s")
    }
    
    // Calculate error rate
    let total_packets = metrics.packets_received_per_sec + metrics.packets_sent_per_sec
    let total_errors = metrics.errors_in + metrics.errors_out
    let error_rate = if total_packets > 0 {
      (total_errors.to_float() / total_packets.to_float()) * 100.0
    } else {
      0.0
    }
    
    // Check error rate
    if error_rate >= thresholds.error_rate_critical_percent {
      alerts = alerts.push("CRITICAL: High error rate: " + error_rate.to_string() + "%")
    } else if error_rate >= thresholds.error_rate_warning_percent {
      alerts = alerts.push("WARNING: High error rate: " + error_rate.to_string() + "%")
    }
    
    // Check for drops
    let total_drops = metrics.drops_in + metrics.drops_out
    if total_drops > 0 {
      alerts = alerts.push("WARNING: Packet drops detected: " + total_drops.to_string())
    }
    
    {
      metrics: metrics,
      total_throughput_mb: total_throughput_mb,
      total_packet_rate: total_packet_rate,
      error_rate: error_rate,
      alerts: alerts,
      status: if alerts.length() > 0 { "alert" } else { "normal" }
    }
  }
  
  // Monitor each network metric
  let network_monitoring_results = network_metrics.map(fn(m) { monitor_network(m, network_alert_thresholds) })
  
  // Verify the results
  assert_eq(network_monitoring_results.length(), 3)
  
  // First metric should be normal
  assert_eq(network_monitoring_results[0].status, "normal")
  assert_eq(network_monitoring_results[0].total_throughput_mb, 3)
  assert_eq(network_monitoring_results[0].total_packet_rate, 2500)
  assert_eq(network_monitoring_results[0].error_rate, 0.0)
  assert_eq(network_monitoring_results[0].alerts.length(), 0)
  
  // Second metric should have warnings
  assert_eq(network_monitoring_results[1].status, "alert")
  assert_eq(network_monitoring_results[1].total_throughput_mb, 50)
  assert_eq(network_monitoring_results[1].total_packet_rate, 25000)
  assert_eq(network_monitoring_results[1].error_rate, 0.032)
  assert_eq(network_monitoring_results[1].alerts.length(), 3)
  assert_true(network_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High bandwidth usage") }))
  assert_true(network_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High packet rate") }))
  assert_true(network_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: Packet drops detected") }))
  
  // Third metric should have critical alerts
  assert_eq(network_monitoring_results[2].status, "alert")
  assert_eq(network_monitoring_results[2].total_throughput_mb, 120)
  assert_eq(network_monitoring_results[2].total_packet_rate, 60000)
  assert_eq(network_monitoring_results[2].error_rate, 0.16)
  assert_eq(network_monitoring_results[2].alerts.length(), 4)
  assert_true(network_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High bandwidth usage") }))
  assert_true(network_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High packet rate") }))
  assert_true(network_monitoring_results[2].alerts.some(fn(a) { a.contains("WARNING: High error rate") }))
  assert_true(network_monitoring_results[2].alerts.some(fn(a) { a.contains("WARNING: Packet drops detected") }))
}

// Test 5: Application Performance Monitoring
test "application performance monitoring" {
  // Define application metrics types
  type ApplicationMetrics = {
    timestamp: Int,
    service_name: String,
    request_rate: Int,              // requests per second
    response_time_avg_ms: Float,    // average response time
    response_time_p95_ms: Float,    // 95th percentile response time
    response_time_p99_ms: Float,    // 99th percentile response time
    error_rate_percent: Float,      // error rate percentage
    active_connections: Int,
    thread_pool_active: Int,
    thread_pool_size: Int,
    gc_pause_time_ms: Float         // garbage collection pause time
  }
  
  type ApplicationAlertThresholds = {
    response_time_warning_ms: Float,
    response_time_critical_ms: Float,
    p99_response_time_warning_ms: Float,
    p99_response_time_critical_ms: Float,
    error_rate_warning_percent: Float,
    error_rate_critical_percent: Float,
    thread_pool_usage_warning_percent: Float,
    thread_pool_usage_critical_percent: Float,
    gc_pause_warning_ms: Float,
    gc_pause_critical_ms: Float
  }
  
  // Create sample application metrics
  let application_metrics = [
    {
      timestamp: 1640995200,
      service_name: "payment-service",
      request_rate: 100,
      response_time_avg_ms: 50.0,
      response_time_p95_ms: 75.0,
      response_time_p99_ms: 100.0,
      error_rate_percent: 0.5,
      active_connections: 25,
      thread_pool_active: 10,
      thread_pool_size: 50,
      gc_pause_time_ms: 5.0
    },
    {
      timestamp: 1640995260,
      service_name: "payment-service",
      request_rate: 500,
      response_time_avg_ms: 150.0,
      response_time_p95_ms: 250.0,
      response_time_p99_ms: 400.0,
      error_rate_percent: 2.0,
      active_connections: 100,
      thread_pool_active: 35,
      thread_pool_size: 50,
      gc_pause_time_ms: 15.0
    },
    {
      timestamp: 1640995320,
      service_name: "payment-service",
      request_rate: 1000,
      response_time_avg_ms: 500.0,
      response_time_p95_ms: 800.0,
      response_time_p99_ms: 1200.0,
      error_rate_percent: 10.0,
      active_connections: 200,
      thread_pool_active: 48,
      thread_pool_size: 50,
      gc_pause_time_ms: 50.0
    }
  ]
  
  // Define alert thresholds
  let application_alert_thresholds = {
    response_time_warning_ms: 100.0,
    response_time_critical_ms: 300.0,
    p99_response_time_warning_ms: 500.0,
    p99_response_time_critical_ms: 1000.0,
    error_rate_warning_percent: 1.0,
    error_rate_critical_percent: 5.0,
    thread_pool_usage_warning_percent: 70.0,
    thread_pool_usage_critical_percent: 90.0,
    gc_pause_warning_ms: 20.0,
    gc_pause_critical_ms: 40.0
  }
  
  // Define application monitoring function
  let monitor_application = fn(metrics: ApplicationMetrics, thresholds: ApplicationAlertThresholds) {
    let mut alerts = []
    
    // Check average response time
    if metrics.response_time_avg_ms >= thresholds.response_time_critical_ms {
      alerts = alerts.push("CRITICAL: High avg response time: " + metrics.response_time_avg_ms.to_string() + "ms")
    } else if metrics.response_time_avg_ms >= thresholds.response_time_warning_ms {
      alerts = alerts.push("WARNING: High avg response time: " + metrics.response_time_avg_ms.to_string() + "ms")
    }
    
    // Check P99 response time
    if metrics.response_time_p99_ms >= thresholds.p99_response_time_critical_ms {
      alerts = alerts.push("CRITICAL: High P99 response time: " + metrics.response_time_p99_ms.to_string() + "ms")
    } else if metrics.response_time_p99_ms >= thresholds.p99_response_time_warning_ms {
      alerts = alerts.push("WARNING: High P99 response time: " + metrics.response_time_p99_ms.to_string() + "ms")
    }
    
    // Check error rate
    if metrics.error_rate_percent >= thresholds.error_rate_critical_percent {
      alerts = alerts.push("CRITICAL: High error rate: " + metrics.error_rate_percent.to_string() + "%")
    } else if metrics.error_rate_percent >= thresholds.error_rate_warning_percent {
      alerts = alerts.push("WARNING: High error rate: " + metrics.error_rate_percent.to_string() + "%")
    }
    
    // Check thread pool usage
    let thread_pool_usage_percent = (metrics.thread_pool_active.to_float() / metrics.thread_pool_size.to_float()) * 100.0
    if thread_pool_usage_percent >= thresholds.thread_pool_usage_critical_percent {
      alerts = alerts.push("CRITICAL: High thread pool usage: " + thread_pool_usage_percent.to_string() + "%")
    } else if thread_pool_usage_percent >= thresholds.thread_pool_usage_warning_percent {
      alerts = alerts.push("WARNING: High thread pool usage: " + thread_pool_usage_percent.to_string() + "%")
    }
    
    // Check GC pause time
    if metrics.gc_pause_time_ms >= thresholds.gc_pause_critical_ms {
      alerts = alerts.push("CRITICAL: High GC pause time: " + metrics.gc_pause_time_ms.to_string() + "ms")
    } else if metrics.gc_pause_time_ms >= thresholds.gc_pause_warning_ms {
      alerts = alerts.push("WARNING: High GC pause time: " + metrics.gc_pause_time_ms.to_string() + "ms")
    }
    
    {
      metrics: metrics,
      thread_pool_usage_percent: thread_pool_usage_percent,
      alerts: alerts,
      status: if alerts.length() > 0 { "alert" } else { "normal" }
    }
  }
  
  // Monitor each application metric
  let application_monitoring_results = application_metrics.map(fn(m) { monitor_application(m, application_alert_thresholds) })
  
  // Verify the results
  assert_eq(application_monitoring_results.length(), 3)
  
  // First metric should be normal
  assert_eq(application_monitoring_results[0].status, "normal")
  assert_eq(application_monitoring_results[0].thread_pool_usage_percent, 20.0)
  assert_eq(application_monitoring_results[0].alerts.length(), 0)
  
  // Second metric should have warnings
  assert_eq(application_monitoring_results[1].status, "alert")
  assert_eq(application_monitoring_results[1].thread_pool_usage_percent, 70.0)
  assert_eq(application_monitoring_results[1].alerts.length(), 4)
  assert_true(application_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High avg response time") }))
  assert_true(application_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High P99 response time") }))
  assert_true(application_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High error rate") }))
  assert_true(application_monitoring_results[1].alerts.some(fn(a) { a.contains("WARNING: High thread pool usage") }))
  
  // Third metric should have critical alerts
  assert_eq(application_monitoring_results[2].status, "alert")
  assert_eq(application_monitoring_results[2].thread_pool_usage_percent, 96.0)
  assert_eq(application_monitoring_results[2].alerts.length(), 5)
  assert_true(application_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High avg response time") }))
  assert_true(application_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High P99 response time") }))
  assert_true(application_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High error rate") }))
  assert_true(application_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High thread pool usage") }))
  assert_true(application_monitoring_results[2].alerts.some(fn(a) { a.contains("CRITICAL: High GC pause time") }))
}