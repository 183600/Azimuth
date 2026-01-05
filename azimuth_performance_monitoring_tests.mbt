// Performance Monitoring Tests for Azimuth Telemetry System
// This file contains test cases for performance monitoring functionality

// Test 1: CPU Usage Monitoring
test "cpu usage monitoring functionality" {
  // Define CPU metrics
  type CpuMetrics = {
    timestamp: Int,
    user_time: Float,
    system_time: Float,
    idle_time: Float,
    total_time: Float
  }
  
  // Calculate CPU usage percentage
  let calculate_cpu_usage = fn(metrics: CpuMetrics) {
    let active_time = metrics.user_time + metrics.system_time
    let usage_percentage = (active_time / metrics.total_time) * 100.0
    usage_percentage
  }
  
  // Create sample CPU metrics
  let cpu_metrics = {
    timestamp: 1640995200,
    user_time: 150.5,
    system_time: 75.2,
    idle_time: 274.3,
    total_time: 500.0
  }
  
  // Calculate CPU usage
  let cpu_usage = calculate_cpu_usage(cpu_metrics)
  let expected_usage = ((150.5 + 75.2) / 500.0) * 100.0
  
  assert_true(cpu_usage > 45.0 and cpu_usage < 46.0)
  
  // Test CPU usage over time
  let cpu_metrics_series = [
    { timestamp: 1640995200, user_time: 150.5, system_time: 75.2, idle_time: 274.3, total_time: 500.0 },
    { timestamp: 1640995260, user_time: 180.3, system_time: 90.1, idle_time: 229.6, total_time: 500.0 },
    { timestamp: 1640995320, user_time: 165.7, system_time: 82.8, idle_time: 251.5, total_time: 500.0 },
    { timestamp: 1640995380, user_time: 195.2, system_time: 97.6, idle_time: 207.2, total_time: 500.0 },
    { timestamp: 1640995440, user_time: 175.8, system_time: 87.9, idle_time: 236.3, total_time: 500.0 }
  ]
  
  // Calculate CPU usage for each timestamp
  let cpu_usage_series = cpu_metrics_series.map(calculate_cpu_usage)
  
  // Verify CPU usage calculations
  assert_eq(cpu_usage_series.length(), 5)
  assert_true(cpu_usage_series[0] > 45.0 and cpu_usage_series[0] < 46.0)
  assert_true(cpu_usage_series[1] > 54.0 and cpu_usage_series[1] < 55.0)
  assert_true(cpu_usage_series[2] > 49.0 and cpu_usage_series[2] < 50.0)
  assert_true(cpu_usage_series[3] > 58.0 and cpu_usage_series[3] < 59.0)
  assert_true(cpu_usage_series[4] > 52.0 and cpu_usage_series[4] < 53.0)
  
  // Calculate average CPU usage
  let avg_cpu_usage = cpu_usage_series.reduce(fn(acc, usage) { acc + usage }, 0.0) / cpu_usage_series.length().to_float()
  assert_true(avg_cpu_usage > 50.0 and avg_cpu_usage < 55.0)
  
  // Detect high CPU usage periods
  let detect_high_cpu = fn(usage_series: Array[Float], threshold: Float) {
    usage_series.filter(fn(usage) { usage > threshold })
  }
  
  let high_cpu_periods = detect_high_cpu(cpu_usage_series, 55.0)
  assert_eq(high_cpu_periods.length(), 2)
}

// Test 2: Memory Usage Monitoring
test "memory usage monitoring functionality" {
  // Define memory metrics
  type MemoryMetrics = {
    timestamp: Int,
    total_memory: Int,
    used_memory: Int,
    free_memory: Int,
    cached_memory: Int,
    buffers_memory: Int
  }
  
  // Calculate memory usage percentage
  let calculate_memory_usage = fn(metrics: MemoryMetrics) {
    let actually_used = metrics.used_memory - metrics.cached_memory - metrics.buffers_memory
    let usage_percentage = (actually_used.to_float() / metrics.total_memory.to_float()) * 100.0
    usage_percentage
  }
  
  // Create sample memory metrics
  let memory_metrics = {
    timestamp: 1640995200,
    total_memory: 8192,  // 8GB in MB
    used_memory: 6144,    // 6GB in MB
    free_memory: 2048,    // 2GB in MB
    cached_memory: 1024,  // 1GB in MB
    buffers_memory: 256   // 256MB in MB
  }
  
  // Calculate memory usage
  let memory_usage = calculate_memory_usage(memory_metrics)
  let actually_used = 6144 - 1024 - 256  // 4864MB
  let expected_usage = (4864.0 / 8192.0) * 100.0
  
  assert_true(memory_usage > 59.0 and memory_usage < 60.0)
  
  // Test memory usage over time
  let memory_metrics_series = [
    { timestamp: 1640995200, total_memory: 8192, used_memory: 6144, free_memory: 2048, cached_memory: 1024, buffers_memory: 256 },
    { timestamp: 1640995260, total_memory: 8192, used_memory: 6400, free_memory: 1792, cached_memory: 1152, buffers_memory: 256 },
    { timestamp: 1640995320, total_memory: 8192, used_memory: 6656, free_memory: 1536, cached_memory: 1280, buffers_memory: 256 },
    { timestamp: 1640995380, total_memory: 8192, used_memory: 6912, free_memory: 1280, cached_memory: 1408, buffers_memory: 256 },
    { timestamp: 1640995440, total_memory: 8192, used_memory: 6784, free_memory: 1408, cached_memory: 1344, buffers_memory: 256 }
  ]
  
  // Calculate memory usage for each timestamp
  let memory_usage_series = memory_metrics_series.map(calculate_memory_usage)
  
  // Verify memory usage calculations
  assert_eq(memory_usage_series.length(), 5)
  assert_true(memory_usage_series[0] > 59.0 and memory_usage_series[0] < 60.0)
  assert_true(memory_usage_series[1] > 61.0 and memory_usage_series[1] < 62.0)
  assert_true(memory_usage_series[2] > 64.0 and memory_usage_series[2] < 65.0)
  assert_true(memory_usage_series[3] > 66.0 and memory_usage_series[3] < 67.0)
  assert_true(memory_usage_series[4] > 65.0 and memory_usage_series[4] < 66.0)
  
  // Detect memory leaks (increasing usage over time)
  let detect_memory_leak = fn(usage_series: Array[Float], threshold: Float) {
    if usage_series.length() < 2 {
      false
    } else {
      let first_usage = usage_series[0]
      let last_usage = usage_series[usage_series.length() - 1]
      let increase = last_usage - first_usage
      increase > threshold
    }
  }
  
  let is_memory_leak = detect_memory_leak(memory_usage_series, 5.0)
  assert_true(is_memory_leak)  // Usage increased by ~6%
}

// Test 3: Disk I/O Monitoring
test "disk i/o monitoring functionality" {
  // Define disk I/O metrics
  type DiskMetrics = {
    timestamp: Int,
    read_bytes: Int,
    write_bytes: Int,
    read_operations: Int,
    write_operations: Int
  }
  
  // Calculate I/O throughput
  let calculate_io_throughput = fn(current: DiskMetrics, previous: DiskMetrics) {
    let time_diff = current.timestamp - previous.timestamp
    if time_diff <= 0 {
      { read_throughput: 0.0, write_throughput: 0.0 }
    } else {
      let read_diff = current.read_bytes - previous.read_bytes
      let write_diff = current.write_bytes - previous.write_bytes
      
      {
        read_throughput: read_diff.to_float() / time_diff.to_float(),
        write_throughput: write_diff.to_float() / time_diff.to_float()
      }
    }
  }
  
  // Create sample disk metrics
  let disk_metrics_series = [
    { timestamp: 1640995200, read_bytes: 1048576, write_bytes: 524288, read_operations: 100, write_operations: 50 },
    { timestamp: 1640995260, read_bytes: 2097152, write_bytes: 1048576, read_operations: 200, write_operations: 100 },
    { timestamp: 1640995320, read_bytes: 3145728, write_bytes: 1572864, read_operations: 300, write_operations: 150 },
    { timestamp: 1640995380, read_bytes: 4194304, write_bytes: 2097152, read_operations: 400, write_operations: 200 },
    { timestamp: 1640995440, read_bytes: 5242880, write_bytes: 2621440, read_operations: 500, write_operations: 250 }
  ]
  
  // Calculate I/O throughput between consecutive measurements
  let mut io_throughputs = []
  
  for i in 1..disk_metrics_series.length() {
    let current = disk_metrics_series[i]
    let previous = disk_metrics_series[i - 1]
    let throughput = calculate_io_throughput(current, previous)
    io_throughputs = io_throughputs.push(throughput)
  }
  
  // Verify I/O throughput calculations
  assert_eq(io_throughputs.length(), 4)
  
  // Each interval is 60 seconds, and we're reading/writing 1MB/0.5MB per interval
  let expected_read_throughput = 1048576.0 / 60.0  // ~17476.3 bytes/second
  let expected_write_throughput = 524288.0 / 60.0  // ~8738.1 bytes/second
  
  for throughput in io_throughputs {
    assert_true(throughput.read_throughput > expected_read_throughput - 100.0)
    assert_true(throughput.read_throughput < expected_read_throughput + 100.0)
    assert_true(throughput.write_throughput > expected_write_throughput - 100.0)
    assert_true(throughput.write_throughput < expected_write_throughput + 100.0)
  }
  
  // Calculate average I/O operations per second
  let calculate_iops = fn(current: DiskMetrics, previous: DiskMetrics) {
    let time_diff = current.timestamp - previous.timestamp
    if time_diff <= 0 {
      { read_iops: 0.0, write_iops: 0.0 }
    } else {
      let read_ops_diff = current.read_operations - previous.read_operations
      let write_ops_diff = current.write_operations - previous.write_operations
      
      {
        read_iops: read_ops_diff.to_float() / time_diff.to_float(),
        write_iops: write_ops_diff.to_float() / time_diff.to_float()
      }
    }
  }
  
  // Calculate IOPS for each interval
  let mut iops_series = []
  
  for i in 1..disk_metrics_series.length() {
    let current = disk_metrics_series[i]
    let previous = disk_metrics_series[i - 1]
    let iops = calculate_iops(current, previous)
    iops_series = iops_series.push(iops)
  }
  
  // Verify IOPS calculations
  assert_eq(iops_series.length(), 4)
  
  // Each interval is 60 seconds, and we're doing 100 read/50 write operations per interval
  let expected_read_iops = 100.0 / 60.0  // ~1.67 ops/second
  let expected_write_iops = 50.0 / 60.0  // ~0.83 ops/second
  
  for iops in iops_series {
    assert_true(iops.read_iops > expected_read_iops - 0.1)
    assert_true(iops.read_iops < expected_read_iops + 0.1)
    assert_true(iops.write_iops > expected_write_iops - 0.1)
    assert_true(iops.write_iops < expected_write_iops + 0.1)
  }
}

// Test 4: Network I/O Monitoring
test "network i/o monitoring functionality" {
  // Define network I/O metrics
  type NetworkMetrics = {
    timestamp: Int,
    bytes_sent: Int,
    bytes_received: Int,
    packets_sent: Int,
    packets_received: Int,
    errors_in: Int,
    errors_out: Int
  }
  
  // Calculate network throughput
  let calculate_network_throughput = fn(current: NetworkMetrics, previous: NetworkMetrics) {
    let time_diff = current.timestamp - previous.timestamp
    if time_diff <= 0 {
      { 
        send_throughput: 0.0, 
        receive_throughput: 0.0,
        send_packet_rate: 0.0,
        receive_packet_rate: 0.0,
        error_rate: 0.0
      }
    } else {
      let sent_diff = current.bytes_sent - previous.bytes_sent
      let received_diff = current.bytes_received - previous.bytes_received
      let sent_packets_diff = current.packets_sent - previous.packets_sent
      let received_packets_diff = current.packets_received - previous.packets_received
      let errors_diff = (current.errors_in + current.errors_out) - (previous.errors_in + previous.errors_out)
      
      {
        send_throughput: sent_diff.to_float() / time_diff.to_float(),
        receive_throughput: received_diff.to_float() / time_diff.to_float(),
        send_packet_rate: sent_packets_diff.to_float() / time_diff.to_float(),
        receive_packet_rate: received_packets_diff.to_float() / time_diff.to_float(),
        error_rate: errors_diff.to_float() / time_diff.to_float()
      }
    }
  }
  
  // Create sample network metrics
  let network_metrics_series = [
    { timestamp: 1640995200, bytes_sent: 10485760, bytes_received: 20971520, packets_sent: 1000, packets_received: 2000, errors_in: 0, errors_out: 0 },
    { timestamp: 1640995260, bytes_sent: 20971520, bytes_received: 41943040, packets_sent: 2000, packets_received: 4000, errors_in: 1, errors_out: 0 },
    { timestamp: 1640995320, bytes_sent: 31457280, bytes_received: 62914560, packets_sent: 3000, packets_received: 6000, errors_in: 1, errors_out: 1 },
    { timestamp: 1640995380, bytes_sent: 41943040, bytes_received: 83886080, packets_sent: 4000, packets_received: 8000, errors_in: 2, errors_out: 1 },
    { timestamp: 1640995440, bytes_sent: 52428800, bytes_received: 104857600, packets_sent: 5000, packets_received: 10000, errors_in: 2, errors_out: 2 }
  ]
  
  // Calculate network throughput between consecutive measurements
  let mut network_throughputs = []
  
  for i in 1..network_metrics_series.length() {
    let current = network_metrics_series[i]
    let previous = network_metrics_series[i - 1]
    let throughput = calculate_network_throughput(current, previous)
    network_throughputs = network_throughputs.push(throughput)
  }
  
  // Verify network throughput calculations
  assert_eq(network_throughputs.length(), 4)
  
  // Each interval is 60 seconds, and we're sending 10MB/receiving 20MB per interval
  let expected_send_throughput = 10485760.0 / 60.0  // ~174762.7 bytes/second
  let expected_receive_throughput = 20971520.0 / 60.0  // ~349525.3 bytes/second
  
  // Each interval is 60 seconds, and we're sending 1000/receiving 2000 packets per interval
  let expected_send_packet_rate = 1000.0 / 60.0  // ~16.67 packets/second
  let expected_receive_packet_rate = 2000.0 / 60.0  // ~33.33 packets/second
  
  for throughput in network_throughputs {
    assert_true(throughput.send_throughput > expected_send_throughput - 1000.0)
    assert_true(throughput.send_throughput < expected_send_throughput + 1000.0)
    assert_true(throughput.receive_throughput > expected_receive_throughput - 1000.0)
    assert_true(throughput.receive_throughput < expected_receive_throughput + 1000.0)
    assert_true(throughput.send_packet_rate > expected_send_packet_rate - 0.5)
    assert_true(throughput.send_packet_rate < expected_send_packet_rate + 0.5)
    assert_true(throughput.receive_packet_rate > expected_receive_packet_rate - 0.5)
    assert_true(throughput.receive_packet_rate < expected_receive_packet_rate + 0.5)
    assert_true(throughput.error_rate >= 0.0)
  }
  
  // Calculate packet loss rate
  let calculate_packet_loss = fn(throughputs: Array[{send_packet_rate: Float, receive_packet_rate: Float, error_rate: Float}]) {
    throughputs.map(fn(t) {
      let total_sent = t.send_packet_rate * 60.0  // Convert back to total packets in interval
      let total_received = t.receive_packet_rate * 60.0
      let total_errors = t.error_rate * 60.0
      
      if total_sent > 0.0 {
        (total_sent - total_received + total_errors) / total_sent
      } else {
        0.0
      }
    })
  }
  
  let packet_loss_rates = calculate_packet_loss(network_throughputs)
  
  // Verify packet loss calculations
  assert_eq(packet_loss_rates.length(), 4)
  
  // First interval: 1000 sent, 2000 received, 1 error in (no loss, just more received)
  assert_true(packet_loss_rates[0] >= 0.0 and packet_loss_rates[0] <= 0.1)
  
  // Last interval: 1000 sent, 2000 received, 2 errors in (still no loss, just more received)
  assert_true(packet_loss_rates[3] >= 0.0 and packet_loss_rates[3] <= 0.1)
}

// Test 5: Response Time Monitoring
test "response time monitoring functionality" {
  // Define response time metrics
  type ResponseTimeMetrics = {
    timestamp: Int,
    min_response_time: Float,
    max_response_time: Float,
    avg_response_time: Float,
    p50_response_time: Float,
    p95_response_time: Float,
    p99_response_time: Float,
    request_count: Int
  }
  
  // Create sample response time metrics
  let response_time_metrics = [
    { timestamp: 1640995200, min_response_time: 10.5, max_response_time: 250.8, avg_response_time: 85.3, p50_response_time: 75.2, p95_response_time: 180.5, p99_response_time: 220.3, request_count: 1000 },
    { timestamp: 1640995260, min_response_time: 12.3, max_response_time: 280.5, avg_response_time: 95.7, p50_response_time: 85.1, p95_response_time: 195.2, p99_response_time: 245.8, request_count: 1200 },
    { timestamp: 1640995320, min_response_time: 11.8, max_response_time: 300.2, avg_response_time: 105.4, p50_response_time: 95.3, p95_response_time: 210.7, p99_response_time: 270.5, request_count: 1100 },
    { timestamp: 1640995380, min_response_time: 14.2, max_response_time: 320.8, avg_response_time: 115.8, p50_response_time: 105.6, p95_response_time: 225.3, p99_response_time: 285.2, request_count: 1300 },
    { timestamp: 1640995440, min_response_time: 13.7, max_response_time: 290.5, avg_response_time: 98.5, p50_response_time: 88.4, p95_response_time: 200.8, p99_response_time: 250.3, request_count: 1150 }
  ]
  
  // Calculate overall statistics
  let total_requests = response_time_metrics.reduce(fn(acc, m) { acc + m.request_count }, 0)
  let weighted_avg = response_time_metrics.reduce(fn(acc, m) { 
    acc + (m.avg_response_time * m.request_count.to_float()) 
  }, 0.0) / total_requests.to_float()
  
  // Verify calculations
  assert_eq(total_requests, 5750)
  assert_true(weighted_avg > 95.0 and weighted_avg < 105.0)
  
  // Detect response time anomalies
  let detect_response_anomalies = fn(metrics: Array[ResponseTimeMetrics], p95_threshold: Float) {
    metrics.filter(fn(m) { m.p95_response_time > p95_threshold })
  }
  
  let anomalies = detect_response_anomalies(response_time_metrics, 200.0)
  assert_eq(anomalies.length(), 3)  // Last 3 measurements have P95 > 200ms
  
  // Calculate response time trend
  let calculate_trend = fn(values: Array[Float]) {
    if values.length() < 2 {
      0.0
    } else {
      let first_half_avg = values.slice(0, values.length() / 2).reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() / 2).to_float()
      let second_half_avg = values.slice(values.length() / 2, values.length()).reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() - values.length() / 2).to_float()
      second_half_avg - first_half_avg
    }
  }
  
  let avg_response_times = response_time_metrics.map(fn(m) { m.avg_response_time })
  let trend = calculate_trend(avg_response_times)
  
  // Positive trend indicates increasing response times
  assert_true(trend > 10.0)  # Response times are increasing
}

// Test 6: Throughput Monitoring
test "throughput monitoring functionality" {
  // Define throughput metrics
  type ThroughputMetrics = {
    timestamp: Int,
    requests_per_second: Float,
    bytes_per_second: Float,
    active_connections: Int,
    total_requests: Int
  }
  
  // Create sample throughput metrics
  let throughput_metrics = [
    { timestamp: 1640995200, requests_per_second: 100.5, bytes_per_second: 1048576.0, active_connections: 50, total_requests: 100000 },
    { timestamp: 1640995260, requests_per_second: 120.3, bytes_per_second: 1258291.2, active_connections: 60, total_requests: 107218 },
    { timestamp: 1640995320, requests_per_second: 115.7, bytes_per_second: 1207959.6, active_connections: 58, total_requests: 114160 },
    { timestamp: 1640995380, requests_per_second: 130.2, bytes_per_second: 1363148.8, active_connections: 65, total_requests: 121971 },
    { timestamp: 1640995440, requests_per_second: 125.8, bytes_per_second: 1314867.2, active_connections: 63, total_requests: 129526 }
  ]
  
  // Calculate average throughput
  let avg_rps = throughput_metrics.reduce(fn(acc, m) { acc + m.requests_per_second }, 0.0) / throughput_metrics.length().to_float()
  let avg_bps = throughput_metrics.reduce(fn(acc, m) { acc + m.bytes_per_second }, 0.0) / throughput_metrics.length().to_float()
  let avg_connections = throughput_metrics.reduce(fn(acc, m) { acc + m.active_connections }, 0) / throughput_metrics.length()
  
  // Verify calculations
  assert_true(avg_rps > 115.0 and avg_rps < 125.0)
  assert_true(avg_bps > 1200000.0 and avg_bps < 1300000.0)
  assert_eq(avg_connections, 59)  // (50+60+58+65+63) / 5
  
  // Calculate peak throughput
  let peak_rps = throughput_metrics.reduce(fn(acc, m) { if m.requests_per_second > acc { m.requests_per_second } else { acc } }, 0.0)
  let peak_bps = throughput_metrics.reduce(fn(acc, m) { if m.bytes_per_second > acc { m.bytes_per_second } else { acc } }, 0.0)
  let peak_connections = throughput_metrics.reduce(fn(acc, m) { if m.active_connections > acc { m.active_connections } else { acc } }, 0)
  
  // Verify peak calculations
  assert_eq(peak_rps, 130.2)
  assert_eq(peak_bps, 1363148.8)
  assert_eq(peak_connections, 65)
  
  // Detect throughput anomalies
  let detect_throughput_anomalies = fn(metrics: Array[ThroughputMetrics], threshold_factor: Float) {
    let avg_rps = metrics.reduce(fn(acc, m) { acc + m.requests_per_second }, 0.0) / metrics.length().to_float()
    let threshold = avg_rps * threshold_factor
    
    metrics.filter(fn(m) { m.requests_per_second > threshold })
  }
  
  let high_throughput_periods = detect_throughput_anomalies(throughput_metrics, 1.2)  // 20% above average
  assert_eq(high_throughput_periods.length(), 2)  # 130.2 and 125.8 are > 20% above average
  
  // Calculate throughput efficiency (requests per connection)
  let calculate_efficiency = fn(metrics: Array[ThroughputMetrics]) {
    metrics.map(fn(m) { 
      if m.active_connections > 0 {
        m.requests_per_second / m.active_connections.to_float()
      } else {
        0.0
      }
    })
  }
  
  let efficiency_metrics = calculate_efficiency(throughput_metrics)
  
  // Verify efficiency calculations
  assert_eq(efficiency_metrics.length(), 5)
  for efficiency in efficiency_metrics {
    assert_true(efficiency > 1.5 and efficiency < 2.5)
  }
}

// Test 7: Error Rate Monitoring
test "error rate monitoring functionality" {
  // Define error rate metrics
  type ErrorRateMetrics = {
    timestamp: Int,
    total_requests: Int,
    success_requests: Int,
    error_requests: Int,
    timeout_errors: Int,
    server_errors: Int,
    client_errors: Int
  }
  
  // Calculate error rates
  let calculate_error_rates = fn(metrics: ErrorRateMetrics) {
    let total_error_rate = if metrics.total_requests > 0 {
      metrics.error_requests.to_float() / metrics.total_requests.to_float() * 100.0
    } else {
      0.0
    }
    
    let timeout_rate = if metrics.total_requests > 0 {
      metrics.timeout_errors.to_float() / metrics.total_requests.to_float() * 100.0
    } else {
      0.0
    }
    
    let server_error_rate = if metrics.total_requests > 0 {
      metrics.server_errors.to_float() / metrics.total_requests.to_float() * 100.0
    } else {
      0.0
    }
    
    let client_error_rate = if metrics.total_requests > 0 {
      metrics.client_errors.to_float() / metrics.total_requests.to_float() * 100.0
    } else {
      0.0
    }
    
    {
      total_error_rate,
      timeout_rate,
      server_error_rate,
      client_error_rate
    }
  }
  
  // Create sample error rate metrics
  let error_metrics = [
    { timestamp: 1640995200, total_requests: 1000, success_requests: 950, error_requests: 50, timeout_errors: 10, server_errors: 25, client_errors: 15 },
    { timestamp: 1640995260, total_requests: 1200, success_requests: 1116, error_requests: 84, timeout_errors: 15, server_errors: 40, client_errors: 29 },
    { timestamp: 1640995320, total_requests: 1100, success_requests: 1034, error_requests: 66, timeout_errors: 12, server_errors: 30, client_errors: 24 },
    { timestamp: 1640995380, total_requests: 1300, success_requests: 1210, error_requests: 90, timeout_errors: 18, server_errors: 45, client_errors: 27 },
    { timestamp: 1640995440, total_requests: 1150, success_requests: 1092, error_requests: 58, timeout_errors: 11, server_errors: 28, client_errors: 19 }
  ]
  
  // Calculate error rates for each timestamp
  let error_rates = error_metrics.map(calculate_error_rates)
  
  // Verify error rate calculations
  assert_eq(error_rates.length(), 5)
  
  // First timestamp: 50 errors / 1000 total = 5%
  assert_true(error_rates[0].total_error_rate > 4.9 and error_rates[0].total_error_rate < 5.1)
  assert_true(error_rates[0].timeout_rate > 0.9 and error_rates[0].timeout_rate < 1.1)
  assert_true(error_rates[0].server_error_rate > 2.4 and error_rates[0].server_error_rate < 2.6)
  assert_true(error_rates[0].client_error_rate > 1.4 and error_rates[0].client_error_rate < 1.6)
  
  // Detect error rate spikes
  let detect_error_spikes = fn(rates: Array[{total_error_rate: Float, timeout_rate: Float, server_error_rate: Float, client_error_rate: Float}], threshold: Float) {
    let avg_error_rate = rates.reduce(fn(acc, r) { acc + r.total_error_rate }, 0.0) / rates.length().to_float()
    let spike_threshold = avg_error_rate * threshold
    
    rates.filter(fn(r) { r.total_error_rate > spike_threshold })
  }
  
  let error_spikes = detect_error_spikes(error_rates, 1.3)  // 30% above average
  assert_eq(error_spikes.length(), 2)  # Second and fourth timestamps have higher error rates
  
  // Calculate error distribution
  let calculate_error_distribution = fn(rates: Array[{total_error_rate: Float, timeout_rate: Float, server_error_rate: Float, client_error_rate: Float}]) {
    let total_timeout_rate = rates.reduce(fn(acc, r) { acc + r.timeout_rate }, 0.0)
    let total_server_rate = rates.reduce(fn(acc, r) { acc + r.server_error_rate }, 0.0)
    let total_client_rate = rates.reduce(fn(acc, r) { acc + r.client_error_rate }, 0.0)
    let total_error_rate = total_timeout_rate + total_server_rate + total_client_rate
    
    if total_error_rate > 0.0 {
      {
        timeout_percentage: total_timeout_rate / total_error_rate * 100.0,
        server_percentage: total_server_rate / total_error_rate * 100.0,
        client_percentage: total_client_rate / total_error_rate * 100.0
      }
    } else {
      {
        timeout_percentage: 0.0,
        server_percentage: 0.0,
        client_percentage: 0.0
      }
    }
  }
  
  let error_distribution = calculate_error_distribution(error_rates)
  
  // Verify error distribution
  assert_true(error_distribution.timeout_percentage > 15.0 and error_distribution.timeout_percentage < 20.0)
  assert_true(error_distribution.server_percentage > 45.0 and error_distribution.server_percentage < 50.0)
  assert_true(error_distribution.client_percentage > 30.0 and error_distribution.client_percentage < 35.0)
}

// Test 8: Performance Baseline Comparison
test "performance baseline comparison functionality" {
  // Define baseline metrics
  type BaselineMetrics = {
    avg_response_time: Float,
    p95_response_time: Float,
    requests_per_second: Float,
    error_rate: Float,
    cpu_usage: Float,
    memory_usage: Float
  }
  
  // Define current metrics
  type CurrentMetrics = {
    avg_response_time: Float,
    p95_response_time: Float,
    requests_per_second: Float,
    error_rate: Float,
    cpu_usage: Float,
    memory_usage: Float
  }
  
  // Compare current metrics with baseline
  let compare_with_baseline = fn(current: CurrentMetrics, baseline: BaselineMetrics) {
    let response_time_change = (current.avg_response_time - baseline.avg_response_time) / baseline.avg_response_time * 100.0
    let p95_change = (current.p95_response_time - baseline.p95_response_time) / baseline.p95_response_time * 100.0
    let throughput_change = (current.requests_per_second - baseline.requests_per_second) / baseline.requests_per_second * 100.0
    let error_rate_change = (current.error_rate - baseline.error_rate) / baseline.error_rate * 100.0
    let cpu_change = (current.cpu_usage - baseline.cpu_usage) / baseline.cpu_usage * 100.0
    let memory_change = (current.memory_usage - baseline.memory_usage) / baseline.memory_usage * 100.0
    
    {
      response_time_change,
      p95_change,
      throughput_change,
      error_rate_change,
      cpu_change,
      memory_change,
      overall_score: (response_time_change + p95_change + error_rate_change + cpu_change + memory_change - throughput_change) / 6.0
    }
  }
  
  // Create baseline metrics
  let baseline = {
    avg_response_time: 85.0,
    p95_response_time: 180.0,
    requests_per_second: 110.0,
    error_rate: 5.0,
    cpu_usage: 50.0,
    memory_usage: 60.0
  }
  
  // Create current metrics (some better, some worse)
  let current = {
    avg_response_time: 95.0,  // +11.8%
    p95_response_time: 200.0,  // +11.1%
    requests_per_second: 120.0,  // +9.1%
    error_rate: 7.0,  // +40%
    cpu_usage: 55.0,  // +10%
    memory_usage: 65.0  // +8.3%
  }
  
  // Compare with baseline
  let comparison = compare_with_baseline(current, baseline)
  
  // Verify comparison calculations
  assert_true(comparison.response_time_change > 11.0 and comparison.response_time_change < 12.0)
  assert_true(comparison.p95_change > 11.0 and comparison.p95_change < 12.0)
  assert_true(comparison.throughput_change > 9.0 and comparison.throughput_change < 10.0)
  assert_true(comparison.error_rate_change > 39.0 and comparison.error_rate_change < 41.0)
  assert_true(comparison.cpu_change > 9.0 and comparison.cpu_change < 11.0)
  assert_true(comparison.memory_change > 8.0 and comparison.memory_change < 9.0)
  
  // Overall score should be positive (indicating degradation)
  assert_true(comparison.overall_score > 10.0 and comparison.overall_score < 15.0)
  
  // Determine performance status
  let determine_performance_status = fn(comparison: {response_time_change: Float, p95_change: Float, throughput_change: Float, error_rate_change: Float, cpu_change: Float, memory_change: Float, overall_score: Float}) {
    if comparison.overall_score > 20.0 {
      "critical"
    } else if comparison.overall_score > 10.0 {
      "degraded"
    } else if comparison.overall_score > 0.0 {
      "slightly_degraded"
    } else if comparison.overall_score > -10.0 {
      "stable"
    } else {
      "improved"
    }
  }
  
  let performance_status = determine_performance_status(comparison)
  assert_eq(performance_status, "degraded")
  
  // Test with improved metrics
  let improved = {
    avg_response_time: 75.0,  // -11.8%
    p95_response_time: 160.0,  // -11.1%
    requests_per_second: 130.0,  // +18.2%
    error_rate: 3.0,  // -40%
    cpu_usage: 45.0,  // -10%
    memory_usage: 55.0  // -8.3%
  }
  
  let improved_comparison = compare_with_baseline(improved, baseline)
  let improved_status = determine_performance_status(improved_comparison)
  
  // Overall score should be negative (indicating improvement)
  assert_true(improved_comparison.overall_score < -10.0)
  assert_eq(improved_status, "improved")
}