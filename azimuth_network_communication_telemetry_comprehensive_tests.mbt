// Azimuth Network Communication Telemetry Comprehensive Test Suite
// This file contains comprehensive tests for network communication telemetry in the Azimuth system

// Test 1: Network Latency Measurement
test "network latency measurement" {
  // Network latency measurement structures
  type LatencyMeasurement = {
    source: String,
    destination: String,
    rtt: Int,  // Round-trip time in milliseconds
    timestamp: Int
  }
  
  type NetworkNode = {
    id: String,
    address: String,
    measurements: Array[LatencyMeasurement]
  }
  
  // Create network nodes
  let create_network_nodes = fn(count: Int) {
    let mut nodes = []
    
    for i in 0..count {
      nodes = nodes.push({
        id: "node-" + i.to_string(),
        address: "192.168.1." + (100 + i).to_string(),
        measurements: []
      })
    }
    
    nodes
  }
  
  let network_nodes = create_network_nodes(5)
  assert_eq(network_nodes.length(), 5)
  
  // Simulate network latency measurement
  let measure_latency = fn(source: NetworkNode, destination: NetworkNode, current_time: Int) {
    // Simulate network latency based on node IDs
    let source_id = source.id.split("-")[1].to_int()
    let dest_id = destination.id.split("-")[1].to_int()
    
    // Base latency + distance-based component
    let base_latency = 10
    let distance_latency = (source_id - dest_id).abs() * 5
    let random_component = (source_id + dest_id) % 10
    
    let rtt = base_latency + distance_latency + random_component
    
    {
      source: source.id,
      destination: destination.id,
      rtt,
      timestamp: current_time
    }
  }
  
  // Test latency measurement between nodes
  let measurement_1 = measure_latency(network_nodes[0], network_nodes[1], 1640995200)
  assert_eq(measurement_1.source, "node-0")
  assert_eq(measurement_1.destination, "node-1")
  assert_eq(measurement_1.timestamp, 1640995200)
  assert_true(measurement_1.rtt > 0)
  
  // Test bidirectional latency
  let measurement_2 = measure_latency(network_nodes[1], network_nodes[0], 1640995200)
  assert_eq(measurement_2.source, "node-1")
  assert_eq(measurement_2.destination, "node-0")
  assert_eq(measurement_2.timestamp, 1640995200)
  
  // Latency should be the same in both directions (in our simplified model)
  assert_eq(measurement_1.rtt, measurement_2.rtt)
  
  // Collect measurements for all node pairs
  let collect_measurements = fn(nodes: Array[NetworkNode], current_time: Int) {
    let mut updated_nodes = []
    
    for i in 0..nodes.length() {
      let mut measurements = []
      
      for j in 0..nodes.length() {
        if i != j {
          let measurement = measure_latency(nodes[i], nodes[j], current_time)
          measurements = measurements.push(measurement)
        }
      }
      
      updated_nodes = updated_nodes.push({
        id: nodes[i].id,
        address: nodes[i].address,
        measurements
      })
    }
    
    updated_nodes
  }
  
  let nodes_with_measurements = collect_measurements(network_nodes, 1640995200)
  
  // Verify each node has measurements to all other nodes
  for node in nodes_with_measurements {
    assert_eq(node.measurements.length(), 4)  // 5 nodes - 1 (itself) = 4 measurements
  }
  
  // Calculate latency statistics
  let calculate_latency_stats = fn(measurements: Array[LatencyMeasurement>) {
    if measurements.length() == 0 {
      { min: 0, max: 0, avg: 0.0, median: 0 }
    } else {
      let sorted = measurements.map(fn(m) { m.rtt }).sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
      let min = sorted[0]
      let max = sorted[sorted.length() - 1]
      let sum = sorted.reduce(fn(acc, rtt) { acc + rtt }, 0)
      let avg = sum.to_float() / sorted.length().to_float()
      
      let median = if sorted.length() % 2 == 0 {
        (sorted[sorted.length() / 2 - 1] + sorted[sorted.length() / 2]) / 2
      } else {
        sorted[sorted.length() / 2]
      }
      
      { min, max, avg, median }
    }
  }
  
  // Test latency statistics calculation
  let all_measurements = []
  for node in nodes_with_measurements {
    all_measurements = all_measurements + node.measurements
  }
  
  let stats = calculate_latency_stats(all_measurements)
  assert_true(stats.min > 0)
  assert_true(stats.max >= stats.min)
  assert_true(stats.avg > 0.0)
  assert_true(stats.median > 0)
  
  // Detect latency anomalies
  let detect_latency_anomalies = fn(measurements: Array[LatencyMeasurement], threshold_multiplier: Float) {
    let stats = calculate_latency_stats(measurements)
    let threshold = stats.avg + (stats.avg - stats.min.to_float()).abs() * threshold_multiplier
    
    measurements.filter(fn(m) { m.rtt.to_float() > threshold })
  }
  
  let anomalies = detect_latency_anomalies(all_measurements, 1.5)
  
  // In our simple model, we might not have anomalies, but the function should work
  for anomaly in anomalies {
    assert_true(anomaly.rtt > 0)
  }
  
  // Test network latency heatmap generation
  let generate_latency_heatmap = fn(nodes: Array[NetworkNode>) {
    let mut heatmap = {}
    
    for node in nodes {
      let mut row = {}
      
      for measurement in node.measurements {
        row = row.insert(measurement.destination, measurement.rtt)
      }
      
      heatmap = heatmap.insert(node.id, row)
    }
    
    heatmap
  }
  
  let heatmap = generate_latency_heatmap(nodes_with_measurements)
  
  // Verify heatmap structure
  for node in nodes_with_measurements {
    assert_true(heatmap.contains_key(node.id))
    
    let row = heatmap[node.id]
    for measurement in node.measurements {
      assert_true(row.contains_key(measurement.destination))
      assert_eq(row[measurement.destination], measurement.rtt)
    }
  }
  
  // Test latency trend analysis
  let analyze_latency_trend = fn(node: NetworkNode, destination: String) {
    let destination_measurements = node.measurements.filter(fn(m) { m.destination == destination })
    
    if destination_measurements.length() == 0 {
      { trend: "no_data", slope: 0.0 }
    } else {
      // Sort by timestamp
      let sorted = destination_measurements.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
      
      // Calculate simple linear regression
      let n = sorted.length().to_float()
      let sum_x = (0..sorted.length()).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
      let sum_y = sorted.reduce(fn(acc, m) { acc + m.rtt.to_float() }, 0.0)
      let sum_xy = (0..sorted.length()).reduce(fn(acc, i) { 
        acc + i.to_float() * sorted[i].rtt.to_float()
      }, 0.0)
      let sum_x2 = (0..sorted.length()).reduce(fn(acc, i) { 
        acc + i.to_float() * i.to_float()
      }, 0.0)
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      
      let trend = if slope > 1.0 {
        "increasing"
      } else if slope < -1.0 {
        "decreasing"
      } else {
        "stable"
      }
      
      { trend, slope }
    }
  }
  
  // Test trend analysis
  let trend = analyze_latency_trend(nodes_with_measurements[0], "node-1")
  assert_true(trend.trend == "increasing" || trend.trend == "decreasing" || trend.trend == "stable")
  assert_true(trend.slope.is_finite())
}

// Test 2: Network Throughput Monitoring
test "network throughput monitoring" {
  // Network throughput structures
  type ThroughputMeasurement = {
    source: String,
    destination: String,
    bytes_transferred: Int,
    duration: Int,  // in milliseconds
    timestamp: Int
  }
  
  type ThroughputStats = {
    current_bps: Int,  // bytes per second
    average_bps: Float,
    peak_bps: Int,
    total_bytes: Int
  }
  
  // Create throughput measurements
  let create_throughput_measurement = fn(source: String, destination: String, bytes: Int, duration: Int, timestamp: Int) {
    {
      source,
      destination,
      bytes_transferred: bytes,
      duration,
      timestamp
    }
  }
  
  // Test throughput measurement creation
  let measurement = create_throughput_measurement("client-1", "server-1", 1024000, 1000, 1640995200)
  assert_eq(measurement.source, "client-1")
  assert_eq(measurement.destination, "server-1")
  assert_eq(measurement.bytes_transferred, 1024000)
  assert_eq(measurement.duration, 1000)
  assert_eq(measurement.timestamp, 1640995200)
  
  // Calculate throughput from measurement
  let calculate_throughput = fn(measurement: ThroughputMeasurement) {
    if measurement.duration > 0 {
      measurement.bytes_transferred * 1000 / measurement.duration  // bytes per second
    } else {
      0
    }
  }
  
  let throughput = calculate_throughput(measurement)
  assert_eq(throughput, 1024000)  // 1024000 bytes per second
  
  // Create multiple measurements for trending
  let create_measurements_series = fn(source: String, destination: String, count: Int) {
    let mut measurements = []
    
    for i in 0..count {
      let bytes = 1024 * (100 + i * 10)  // Increasing bytes
      let duration = 1000 + i * 100       // Increasing duration
      let timestamp = 1640995200 + i * 5000  // 5 second intervals
      
      measurements = measurements.push(
        create_throughput_measurement(source, destination, bytes, duration, timestamp)
      )
    }
    
    measurements
  }
  
  let measurements_series = create_measurements_series("client-1", "server-1", 10)
  assert_eq(measurements_series.length(), 10)
  
  // Calculate throughput statistics
  let calculate_throughput_stats = fn(measurements: Array[ThroughputMeasurement]) {
    if measurements.length() == 0 {
      {
        current_bps: 0,
        average_bps: 0.0,
        peak_bps: 0,
        total_bytes: 0
      }
    } else {
      let throughputs = measurements.map(calculate_throughput)
      let current_bps = if throughputs.length() > 0 { throughputs[throughputs.length() - 1] } else { 0 }
      let peak_bps = throughputs.reduce(fn(acc, bps) { if bps > acc { bps } else { acc }, 0)
      let total_bytes = measurements.reduce(fn(acc, m) { acc + m.bytes_transferred }, 0)
      let average_bps = throughputs.reduce(fn(acc, bps) { acc + bps.to_float() }, 0.0) / throughputs.length().to_float()
      
      {
        current_bps,
        average_bps,
        peak_bps,
        total_bytes
      }
    }
  }
  
  let stats = calculate_throughput_stats(measurements_series)
  assert_true(stats.current_bps > 0)
  assert_true(stats.average_bps > 0.0)
  assert_true(stats.peak_bps > 0)
  assert_true(stats.total_bytes > 0)
  
  // Verify peak is >= current
  assert_true(stats.peak_bps >= stats.current_bps)
  
  // Test bandwidth utilization
  let calculate_bandwidth_utilization = fn(throughput_bps: Int, max_bandwidth_bps: Int) {
    if max_bandwidth_bps > 0 {
      throughput_bps.to_float() / max_bandwidth_bps.to_float() * 100.0
    } else {
      0.0
    }
  }
  
  let max_bandwidth = 10485760  // 10 MB/s
  let utilization = calculate_bandwidth_utilization(stats.current_bps, max_bandwidth)
  assert_true(utilization >= 0.0 && utilization <= 100.0)
  
  // Detect throughput anomalies
  let detect_throughput_anomalies = fn(measurements: Array[ThroughputMeasurement], threshold: Float) {
    let stats = calculate_throughput_stats(measurements)
    let throughputs = measurements.map(calculate_throughput)
    
    let mean = stats.average_bps
    let variance = throughputs.reduce(fn(acc, bps) { 
      acc + (bps.to_float() - mean) * (bps.to_float() - mean)
    }, 0.0) / throughputs.length().to_float()
    
    let std_dev = variance.sqrt()
    let anomaly_threshold = mean + std_dev * threshold
    
    measurements.filter(fn(m) { calculate_throughput(m).to_float() > anomaly_threshold })
  }
  
  let anomalies = detect_throughput_anomalies(measurements_series, 2.0)
  
  // Test network congestion detection
  let detect_congestion = fn(measurements: Array[ThroughputMeasurement], window_size: Int, threshold: Float) {
    if measurements.length() < window_size {
      false
    } else {
      let recent_measurements = measurements.slice(measurements.length() - window_size, window_size)
      let recent_stats = calculate_throughput_stats(recent_measurements)
      let all_stats = calculate_throughput_stats(measurements)
      
      // Congestion if recent throughput is significantly lower than average
      recent_stats.average_bps < all_stats.average_bps * (1.0 - threshold)
    }
  }
  
  let is_congested = detect_congestion(measurements_series, 5, 0.2)
  assert_true(is_congested == false || is_congested == true)  // Just verify it returns a boolean
  
  // Test throughput prediction
  let predict_throughput = fn(measurements: Array[ThroughputMeasurement], prediction_steps: Int) {
    if measurements.length() < 2 {
      []
    } else {
      // Simple linear regression for prediction
      let n = measurements.length().to_float()
      let sum_x = (0..measurements.length()).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
      let sum_y = measurements.reduce(fn(acc, m) { acc + calculate_throughput(m).to_float() }, 0.0)
      let sum_xy = (0..measurements.length()).reduce(fn(acc, i) { 
        acc + i.to_float() * calculate_throughput(measurements[i]).to_float()
      }, 0.0)
      let sum_x2 = (0..measurements.length()).reduce(fn(acc, i) { 
        acc + i.to_float() * i.to_float()
      }, 0.0)
      
      let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      let intercept = (sum_y - slope * sum_x) / n
      
      let mut predictions = []
      
      for i in 1..=prediction_steps {
        let x_value = measurements.length().to_float() + i.to_float()
        let predicted_throughput = slope * x_value + intercept
        
        predictions = predictions.push({
          step: i,
          predicted_bps: predicted_throughput.to_int(),
          timestamp: measurements[measurements.length() - 1].timestamp + i * 5000
        })
      }
      
      predictions
    }
  }
  
  let predictions = predict_throughput(measurements_series, 3)
  assert_eq(predictions.length(), 3)
  
  for prediction in predictions {
    assert_true(prediction.step >= 1 && prediction.step <= 3)
    assert_true(prediction.predicted_bps >= 0)
    assert_true(prediction.timestamp > measurements_series[measurements_series.length() - 1].timestamp)
  }
}

// Test 3: Network Error Rate Monitoring
test "network error rate monitoring" {
  // Network error monitoring structures
  type NetworkError = {
    error_type: String,
    source: String,
    destination: String,
    timestamp: Int,
    retry_count: Int,
    resolved: Bool
  }
  
  type ErrorRateStats = {
    total_requests: Int,
    total_errors: Int,
    error_rate: Float,
    errors_by_type: Array[(String, Int)]
  }
  
  // Create network error
  let create_network_error = fn(error_type: String, source: String, destination: String, timestamp: Int, retry_count: Int) {
    {
      error_type,
      source,
      destination,
      timestamp,
      retry_count,
      resolved: false
    }
  }
  
  // Test error creation
  let error = create_network_error("timeout", "client-1", "server-1", 1640995200, 3)
  assert_eq(error.error_type, "timeout")
  assert_eq(error.source, "client-1")
  assert_eq(error.destination, "server-1")
  assert_eq(error.timestamp, 1640995200)
  assert_eq(error.retry_count, 3)
  assert_false(error.resolved)
  
  // Create error series for testing
  let create_error_series = fn(count: Int) {
    let mut errors = []
    let error_types = ["timeout", "connection_refused", "dns_error", "packet_loss"]
    
    for i in 0..count {
      let error_type = error_types[i % error_types.length()]
      let source = "client-" + ((i % 3) + 1).to_string()
      let destination = "server-" + ((i % 2) + 1).to_string()
      let timestamp = 1640995200 + i * 600  // 10 minute intervals
      let retry_count = (i % 5) + 1
      
      errors = errors.push(create_network_error(error_type, source, destination, timestamp, retry_count))
    }
    
    errors
  }
  
  let error_series = create_error_series(20)
  assert_eq(error_series.length(), 20)
  
  // Calculate error rate statistics
  let calculate_error_rate_stats = fn(errors: Array[NetworkError], total_requests: Int) {
    let error_counts = {}
    
    for error in errors {
      let count = match error_counts[error.error_type] {
        Some(c) => c + 1,
        None => 1
      }
      
      error_counts = error_counts.insert(error.error_type, count)
    }
    
    let mut errors_by_type = []
    
    for (error_type, count) in error_counts {
      errors_by_type = errors_by_type.push((error_type, count))
    }
    
    let total_errors = errors.length()
    let error_rate = if total_requests > 0 {
      total_errors.to_float() / total_requests.to_float() * 100.0
    } else {
      0.0
    }
    
    {
      total_requests,
      total_errors,
      error_rate,
      errors_by_type
    }
  }
  
  let stats = calculate_error_rate_stats(error_series, 100)
  assert_eq(stats.total_requests, 100)
  assert_eq(stats.total_errors, 20)
  assert_eq(stats.error_rate, 20.0)
  assert_true(stats.errors_by_type.length() > 0)
  
  // Test error type distribution
  let timeout_errors = stats.errors_by_type.filter(fn(pair) { pair.0 == "timeout" })
  assert_true(timeout_errors.length() > 0)
  if timeout_errors.length() > 0 {
    assert_eq(timeout_errors[0].0, "timeout")
    assert_true(timeout_errors[0].1 > 0)
  }
  
  // Detect error spikes
  let detect_error_spikes = fn(errors: Array[NetworkError>, window_size: Int, threshold_multiplier: Float) {
    if errors.length() < window_size * 2 {
      []
    } else {
      let mut spikes = []
      
      for i in window_size..(errors.length() - window_size) {
        let window_start = i - window_size
        let window_end = i + window_size
        
        let before_window = errors.slice(window_start, window_size)
        let after_window = errors.slice(window_end, window_size)
        
        let before_rate = before_window.length().to_float() / window_size.to_float()
        let after_rate = after_window.length().to_float() / window_size.to_float()
        
        if after_rate > before_rate * threshold_multiplier {
          spikes = spikes.push({
            timestamp: errors[i].timestamp,
            before_rate,
            after_rate,
            spike_factor: after_rate / before_rate
          })
        }
      }
      
      spikes
    }
  }
  
  let spikes = detect_error_spikes(error_series, 5, 2.0)
  
  // Test error correlation analysis
  let analyze_error_correlation = fn(errors: Array[NetworkError>) {
    let mut source_dest_pairs = {}
    
    for error in errors {
      let pair = error.source + "->" + error.destination
      let count = match source_dest_pairs[pair] {
        Some(c) => c + 1,
        None => 1
      }
      
      source_dest_pairs = source_dest_pairs.insert(pair, count)
    }
    
    // Find pairs with highest error counts
    let mut sorted_pairs = []
    
    for (pair, count) in source_dest_pairs {
      sorted_pairs = sorted_pairs.push((pair, count))
    }
    
    sorted_pairs.sort(fn(a, b) { if a.1 > b.1 { -1 } else if a.1 < b.1 { 1 } else { 0 } })
    
    sorted_pairs
  }
  
  let correlated_pairs = analyze_error_correlation(error_series)
  assert_true(correlated_pairs.length() > 0)
  
  // Verify sorting
  for i in 1..correlated_pairs.length() {
    assert_true(correlated_pairs[i-1].1 >= correlated_pairs[i].1)
  }
  
  // Test error resolution tracking
  let resolve_error = fn(errors: Array[NetworkError>, error_index: Int) {
    let mut updated_errors = []
    
    for i in 0..errors.length() {
      if i == error_index {
        updated_errors = updated_errors.push({
          error_type: errors[i].error_type,
          source: errors[i].source,
          destination: errors[i].destination,
          timestamp: errors[i].timestamp,
          retry_count: errors[i].retry_count,
          resolved: true
        })
      } else {
        updated_errors = updated_errors.push(errors[i])
      }
    }
    
    updated_errors
  }
  
  // Resolve an error
  let resolved_errors = resolve_error(error_series, 0)
  assert_true(resolved_errors[0].resolved)
  assert_false(resolved_errors[1].resolved)
  
  // Calculate resolution rate
  let calculate_resolution_rate = fn(errors: Array[NetworkError>) {
    if errors.length() == 0 {
      0.0
    } else {
      let resolved_count = errors.reduce(fn(acc, error) { 
        if error.resolved { acc + 1 } else { acc }
      }, 0)
      
      resolved_count.to_float() / errors.length().to_float() * 100.0
    }
  }
  
  let resolution_rate = calculate_resolution_rate(resolved_errors)
  assert_eq(resolution_rate, 5.0)  // 1 out of 20 errors resolved = 5%
  
  // Test error pattern analysis
  let analyze_error_patterns = fn(errors: Array[NetworkError]) {
    let mut hourly_distribution = {}
    let mut retry_distribution = {}
    
    for error in errors {
      // Hourly distribution
      let hour = (error.timestamp / 3600) % 24
      let count = match hourly_distribution[hour] {
        Some(c) => c + 1,
        None => 1
      }
      
      hourly_distribution = hourly_distribution.insert(hour, count)
      
      // Retry distribution
      let retry_count = error.retry_count
      let retry_count_key = retry_count.to_string()
      let retry_count_value = match retry_distribution[retry_count_key] {
        Some(c) => c + 1,
        None => 1
      }
      
      retry_distribution = retry_distribution.insert(retry_count_key, retry_count_value)
    }
    
    {
      hourly_distribution,
      retry_distribution
    }
  }
  
  let patterns = analyze_error_patterns(error_series)
  assert_true(patterns.hourly_distribution.size() > 0)
  assert_true(patterns.retry_distribution.size() > 0)
  
  // Test error prediction
  let predict_errors = fn(errors: Array[NetworkError], prediction_window: Int) {
    if errors.length() < prediction_window {
      0
    } else {
      let recent_errors = errors.slice(errors.length() - prediction_window, prediction_window)
      let historical_errors = errors.slice(0, errors.length() - prediction_window)
      
      let recent_rate = recent_errors.length().to_float() / prediction_window.to_float()
      let historical_rate = historical_errors.length().to_float() / historical_errors.length().to_float()
      
      // Simple prediction based on rate change
      let rate_change = recent_rate - historical_rate
      let predicted_count = (recent_errors.length().to_float() + rate_change * prediction_window.to_float()).to_int()
      
      if predicted_count < 0 { 0 } else { predicted_count }
    }
  }
  
  let predicted_errors = predict_errors(error_series, 5)
  assert_true(predicted_errors >= 0)
}

// Test 4: Connection Pool Monitoring
test "connection pool monitoring" {
  // Connection pool structures
  type Connection = {
    id: String,
    created_at: Int,
    last_used: Int,
    in_use: Bool,
    source: String,
    destination: String
  }
  
  type ConnectionPool = {
    id: String,
    source: String,
    destination: String,
    max_connections: Int,
    connections: Array[Connection],
    created_at: Int
  }
  
  type PoolStats = {
    active_connections: Int,
    idle_connections: Int,
    utilization_rate: Float,
    avg_lifetime: Float,
    total_created: Int,
    total_destroyed: Int
  }
  
  // Create connection pool
  let create_connection_pool = fn(id: String, source: String, destination: String, max_connections: Int) {
    {
      id,
      source,
      destination,
      max_connections,
      connections: [],
      created_at: 1640995200
    }
  }
  
  // Test connection pool creation
  let pool = create_connection_pool("pool-1", "client-1", "server-1", 10)
  assert_eq(pool.id, "pool-1")
  assert_eq(pool.source, "client-1")
  assert_eq(pool.destination, "server-1")
  assert_eq(pool.max_connections, 10)
  assert_eq(pool.connections.length(), 0)
  
  // Create connection
  let create_connection = fn(pool: ConnectionPool, current_time: Int) {
    if pool.connections.length() < pool.max_connections {
      let connection_id = "conn-" + pool.connections.length().to_string()
      let connection = {
        id: connection_id,
        created_at: current_time,
        last_used: current_time,
        in_use: false,
        source: pool.source,
        destination: pool.destination
      }
      
      { pool | connections: pool.connections.push(connection) }
    } else {
      pool
    }
  }
  
  // Test connection creation
  let pool_with_conn = create_connection(pool, 1640995200)
  assert_eq(pool_with_conn.connections.length(), 1)
  
  let connection = pool_with_conn.connections[0]
  assert_eq(connection.id, "conn-0")
  assert_eq(connection.created_at, 1640995200)
  assert_eq(connection.last_used, 1640995200)
  assert_false(connection.in_use)
  
  // Acquire connection
  let acquire_connection = fn(pool: ConnectionPool, current_time: Int) {
    let mut updated_pool = pool
    let mut acquired = false
    
    for i in 0..pool.connections.length() {
      if not(pool.connections[i].in_use) {
        let updated_connection = {
          id: pool.connections[i].id,
          created_at: pool.connections[i].created_at,
          last_used: current_time,
          in_use: true,
          source: pool.connections[i].source,
          destination: pool.connections[i].destination
        }
        
        let mut updated_connections = []
        for j in 0..pool.connections.length() {
          if j == i {
            updated_connections = updated_connections.push(updated_connection)
          } else {
            updated_connections = updated_connections.push(pool.connections[j])
          }
        }
        
        updated_pool = { pool | connections: updated_connections }
        acquired = true
        break
      }
    }
    
    (updated_pool, acquired)
  }
  
  // Test connection acquisition
  let (pool_with_acquired, acquired) = acquire_connection(pool_with_conn, 1640995300)
  assert_true(acquired)
  assert_true(pool_with_acquired.connections[0].in_use)
  assert_eq(pool_with_acquired.connections[0].last_used, 1640995300)
  
  // Release connection
  let release_connection = fn(pool: ConnectionPool, connection_id: String, current_time: Int) {
    let mut updated_connections = []
    
    for connection in pool.connections {
      if connection.id == connection_id {
        updated_connections = updated_connections.push({
          id: connection.id,
          created_at: connection.created_at,
          last_used: current_time,
          in_use: false,
          source: connection.source,
          destination: connection.destination
        })
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    { pool | connections: updated_connections }
  }
  
  // Test connection release
  let pool_with_released = release_connection(pool_with_acquired, "conn-0", 1640995400)
  assert_false(pool_with_released.connections[0].in_use)
  assert_eq(pool_with_released.connections[0].last_used, 1640995400)
  
  // Calculate pool statistics
  let calculate_pool_stats = fn(pool: ConnectionPool, current_time: Int) {
    let active_connections = pool.connections.filter(fn(c) { c.in_use }).length()
    let idle_connections = pool.connections.filter(fn(c) { not(c.in_use) }).length()
    
    let utilization_rate = if pool.max_connections > 0 {
      active_connections.to_float() / pool.max_connections.to_float() * 100.0
    } else {
      0.0
    }
    
    let avg_lifetime = if pool.connections.length() > 0 {
      let total_lifetime = pool.connections.reduce(fn(acc, c) { acc + (current_time - c.created_at) }, 0)
      total_lifetime.to_float() / pool.connections.length().to_float()
    } else {
      0.0
    }
    
    {
      active_connections,
      idle_connections,
      utilization_rate,
      avg_lifetime,
      total_created: pool.connections.length(),
      total_destroyed: 0  // Simplified
    }
  }
  
  // Test pool statistics calculation
  let stats = calculate_pool_stats(pool_with_released, 1640995500)
  assert_eq(stats.active_connections, 0)
  assert_eq(stats.idle_connections, 1)
  assert_eq(stats.utilization_rate, 0.0)
  assert_true(stats.avg_lifetime > 0.0)
  assert_eq(stats.total_created, 1)
  
  // Test connection cleanup
  let cleanup_idle_connections = fn(pool: ConnectionPool, max_idle_time: Int, current_time: Int) {
    let mut updated_connections = []
    
    for connection in pool.connections {
      let idle_time = current_time - connection.last_used
      
      if not(connection.in_use) && idle_time > max_idle_time {
        // Remove idle connection
        continue
      } else {
        updated_connections = updated_connections.push(connection)
      }
    }
    
    { pool | connections: updated_connections }
  }
  
  // Test connection cleanup
  let pool_for_cleanup = create_connection(pool_with_released, 1640995200)
  let pool_after_cleanup = cleanup_idle_connections(pool_for_cleanup, 10000, 1640996000)  // 10s max idle, current time is much later
  
  // The old connection should be cleaned up
  assert_eq(pool_after_cleanup.connections.length(), 0)
  
  // Test pool expansion
  let expand_pool = fn(pool: ConnectionPool, new_max: Int) {
    if new_max > pool.max_connections {
      { pool | max_connections: new_max }
    } else {
      pool
    }
  }
  
  let expanded_pool = expand_pool(pool, 20)
  assert_eq(expanded_pool.max_connections, 20)
  
  // Test pool shrinking
  let shrink_pool = fn(pool: ConnectionPool, new_max: Int) {
    if new_max < pool.max_connections {
      let active_connections = pool.connections.filter(fn(c) { c.in_use }).length()
      
      if active_connections <= new_max {
        let mut updated_connections = []
        let mut removed_count = 0
        
        for connection in pool.connections {
          if not(connection.in_use) && (pool.connections.length() - updated_connections.length() - removed_count) > new_max {
            // Remove this connection
            removed_count = removed_count + 1
          } else {
            updated_connections = updated_connections.push(connection)
          }
        }
        
        {
          id: pool.id,
          source: pool.source,
          destination: pool.destination,
          max_connections: new_max,
          connections: updated_connections,
          created_at: pool.created_at
        }
      } else {
        pool
      }
    } else {
      pool
    }
  }
  
  let shrunk_pool = shrink_pool(expanded_pool, 5)
  assert_eq(shrunk_pool.max_connections, 5)
  
  // Test pool monitoring metrics
  let monitor_pool_health = fn(pool: ConnectionPool, current_time: Int) {
    let stats = calculate_pool_stats(pool, current_time)
    
    let health_issues = []
    
    // Check for high utilization
    if stats.utilization_rate > 80.0 {
      health_issues = health_issues.push("high_utilization")
    }
    
    // Check for no available connections
    if stats.idle_connections == 0 && stats.active_connections == pool.max_connections {
      health_issues = health_issues.push("exhausted")
    }
    
    // Check for old connections
    let old_connections = pool.connections.filter(fn(c) { 
      current_time - c.created_at > 3600000  // 1 hour
    }).length()
    
    if old_connections > pool.max_connections / 2 {
      health_issues = health_issues.push("old_connections")
    }
    
    {
      is_healthy: health_issues.length() == 0,
      issues: health_issues,
      stats
    }
  }
  
  // Test pool health monitoring
  let health = monitor_pool_health(pool, 1640995500)
  assert_true(health.is_healthy)
  assert_eq(health.issues.length(), 0)
  
  // Create a pool with high utilization for testing
  let mut high_util_pool = pool
  for i in 0..8 {
    high_util_pool = create_connection(high_util_pool, 1640995200)
  }
  
  // Acquire most connections
  let mut temp_pool = high_util_pool
  for i in 0..7 {
    let (updated_pool, _) = acquire_connection(temp_pool, 1640995300)
    temp_pool = updated_pool
  }
  
  let high_util_health = monitor_pool_health(temp_pool, 1640995500)
  
  // Should detect high utilization
  if high_util_health.stats.utilization_rate > 80.0 {
    assert_false(high_util_health.is_healthy)
    assert_true(high_util_health.issues.contains("high_utilization"))
  }
}

// Test 5: Network Protocol Telemetry
test "network protocol telemetry" {
  // Protocol telemetry structures
  type ProtocolMetric = {
    protocol: String,
    operation: String,
    source: String,
    destination: String,
    bytes_sent: Int,
    bytes_received: Int,
    duration: Int,
    timestamp: Int,
    status: String
  }
  
  type ProtocolStats = {
    total_operations: Int,
    success_rate: Float,
    avg_duration: Float,
    total_bytes_sent: Int,
    total_bytes_received: Int,
    operations_by_type: Array[(String, Int)]
  }
  
  // Create protocol metric
  let create_protocol_metric = fn(protocol: String, operation: String, source: String, destination: String, 
                                 bytes_sent: Int, bytes_received: Int, duration: Int, timestamp: Int, status: String) {
    {
      protocol,
      operation,
      source,
      destination,
      bytes_sent,
      bytes_received,
      duration,
      timestamp,
      status
    }
  }
  
  // Test metric creation
  let metric = create_protocol_metric("HTTP", "GET", "client-1", "server-1", 1024, 2048, 150, 1640995200, "success")
  assert_eq(metric.protocol, "HTTP")
  assert_eq(metric.operation, "GET")
  assert_eq(metric.source, "client-1")
  assert_eq(metric.destination, "server-1")
  assert_eq(metric.bytes_sent, 1024)
  assert_eq(metric.bytes_received, 2048)
  assert_eq(metric.duration, 150)
  assert_eq(metric.timestamp, 1640995200)
  assert_eq(metric.status, "success")
  
  // Create metrics series for testing
  let create_metrics_series = fn(count: Int) {
    let mut metrics = []
    let protocols = ["HTTP", "HTTPS", "TCP", "UDP"]
    let operations = ["GET", "POST", "PUT", "DELETE"]
    let statuses = ["success", "error", "timeout"]
    
    for i in 0..count {
      let protocol = protocols[i % protocols.length()]
      let operation = operations[i % operations.length()]
      let source = "client-" + ((i % 3) + 1).to_string()
      let destination = "server-" + ((i % 2) + 1).to_string()
      let bytes_sent = 512 + i * 64
      let bytes_received = 1024 + i * 128
      let duration = 100 + i * 10
      let timestamp = 1640995200 + i * 1000
      let status = statuses[i % statuses.length()]
      
      metrics = metrics.push(
        create_protocol_metric(protocol, operation, source, destination, bytes_sent, bytes_received, duration, timestamp, status)
      )
    }
    
    metrics
  }
  
  let metrics_series = create_metrics_series(20)
  assert_eq(metrics_series.length(), 20)
  
  // Calculate protocol statistics
  let calculate_protocol_stats = fn(metrics: Array[ProtocolMetric]) {
    let total_operations = metrics.length()
    let success_count = metrics.filter(fn(m) { m.status == "success" }).length()
    let success_rate = if total_operations > 0 {
      success_count.to_float() / total_operations.to_float() * 100.0
    } else {
      0.0
    }
    
    let total_duration = metrics.reduce(fn(acc, m) { acc + m.duration }, 0)
    let avg_duration = if total_operations > 0 {
      total_duration.to_float() / total_operations.to_float()
    } else {
      0.0
    }
    
    let total_bytes_sent = metrics.reduce(fn(acc, m) { acc + m.bytes_sent }, 0)
    let total_bytes_received = metrics.reduce(fn(acc, m) { acc + m.bytes_received }, 0)
    
    let mut operations_by_type = {}
    
    for metric in metrics {
      let op_type = metric.protocol + ":" + metric.operation
      let count = match operations_by_type[op_type] {
        Some(c) => c + 1,
        None => 1
      }
      
      operations_by_type = operations_by_type.insert(op_type, count)
    }
    
    let mut ops_by_type_array = []
    
    for (op_type, count) in operations_by_type {
      ops_by_type_array = ops_by_type_array.push((op_type, count))
    }
    
    {
      total_operations,
      success_rate,
      avg_duration,
      total_bytes_sent,
      total_bytes_received,
      operations_by_type: ops_by_type_array
    }
  }
  
  let stats = calculate_protocol_stats(metrics_series)
  assert_eq(stats.total_operations, 20)
  assert_true(stats.success_rate >= 0.0 && stats.success_rate <= 100.0)
  assert_true(stats.avg_duration > 0.0)
  assert_true(stats.total_bytes_sent > 0)
  assert_true(stats.total_bytes_received > 0)
  assert_true(stats.operations_by_type.length() > 0)
  
  // Filter metrics by protocol
  let filter_by_protocol = fn(metrics: Array[ProtocolMetric], protocol: String) {
    metrics.filter(fn(m) { m.protocol == protocol })
  }
  
  let http_metrics = filter_by_protocol(metrics_series, "HTTP")
  assert_true(http_metrics.length() > 0)
  
  for metric in http_metrics {
    assert_eq(metric.protocol, "HTTP")
  }
  
  // Filter metrics by time range
  let filter_by_time_range = fn(metrics: Array[ProtocolMetric], start_time: Int, end_time: Int) {
    metrics.filter(fn(m) { m.timestamp >= start_time && m.timestamp <= end_time })
  }
  
  let time_filtered = filter_by_time_range(metrics_series, 1640995200, 16409953000)
  assert_true(time_filtered.length() > 0)
  
  for metric in time_filtered {
    assert_true(metric.timestamp >= 1640995200 && metric.timestamp <= 16409953000)
  }
  
  // Analyze protocol performance
  let analyze_protocol_performance = fn(metrics: Array[ProtocolMetric]) {
    let mut protocol_performance = {}
    
    for metric in metrics {
      let perf = match protocol_performance[metric.protocol] {
        Some(p) => p,
        None => {
          total_ops: 0,
          total_duration: 0,
          success_count: 0,
          total_bytes_sent: 0,
          total_bytes_received: 0
        }
      }
      
      let updated_perf = {
        total_ops: perf.total_ops + 1,
        total_duration: perf.total_duration + metric.duration,
        success_count: if metric.status == "success" { perf.success_count + 1 } else { perf.success_count },
        total_bytes_sent: perf.total_bytes_sent + metric.bytes_sent,
        total_bytes_received: perf.total_bytes_received + metric.bytes_received
      }
      
      protocol_performance = protocol_performance.insert(metric.protocol, updated_perf)
    }
    
    // Calculate averages and rates
    let mut results = {}
    
    for (protocol, perf) in protocol_performance {
      let avg_duration = if perf.total_ops > 0 {
        perf.total_duration.to_float() / perf.total_ops.to_float()
      } else {
        0.0
      }
      
      let success_rate = if perf.total_ops > 0 {
        perf.success_count.to_float() / perf.total_ops.to_float() * 100.0
      } else {
        0.0
      }
      
      results = results.insert(protocol, {
        operation_count: perf.total_ops,
        avg_duration,
        success_rate,
        total_bytes_sent: perf.total_bytes_sent,
        total_bytes_received: perf.total_bytes_received
      })
    }
    
    results
  }
  
  let performance = analyze_protocol_performance(metrics_series)
  assert_true(performance.size() > 0)
  
  // Test network efficiency calculation
  let calculate_network_efficiency = fn(metrics: Array[ProtocolMetric]) {
    let total_bytes_sent = metrics.reduce(fn(acc, m) { acc + m.bytes_sent }, 0)
    let total_bytes_received = metrics.reduce(fn(acc, m) { acc + m.bytes_received }, 0)
    let total_duration = metrics.reduce(fn(acc, m) { acc + m.duration }, 0)
    
    if total_duration > 0 {
      let throughput = (total_bytes_sent + total_bytes_received).to_float() / total_duration.to_float()
      let efficiency = if total_bytes_sent > 0 {
        total_bytes_received.to_float() / total_bytes_sent.to_float()
      } else {
        0.0
      }
      
      {
        throughput,  // bytes per millisecond
        efficiency   // received / sent ratio
      }
    } else {
      {
        throughput: 0.0,
        efficiency: 0.0
      }
    }
  }
  
  let efficiency = calculate_network_efficiency(metrics_series)
  assert_true(efficiency.throughput > 0.0)
  assert_true(efficiency.efficiency > 0.0)
  
  // Test protocol error analysis
  let analyze_protocol_errors = fn(metrics: Array[ProtocolMetric>) {
    let error_metrics = metrics.filter(fn(m) { m.status != "success" })
    
    let mut error_types = {}
    
    for error in error_metrics {
      let count = match error_types[error.status] {
        Some(c) => c + 1,
        None => 1
      }
      
      error_types = error_types.insert(error.status, count)
    }
    
    let mut error_distribution = []
    
    for (error_type, count) in error_types {
      error_distribution = error_distribution.push((error_type, count))
    }
    
    {
      total_errors: error_metrics.length(),
      error_rate: if metrics.length() > 0 {
        error_metrics.length().to_float() / metrics.length().to_float() * 100.0
      } else {
        0.0
      },
      error_distribution
    }
  }
  
  let error_analysis = analyze_protocol_errors(metrics_series)
  assert_true(error_analysis.total_errors >= 0)
  assert_true(error_analysis.error_rate >= 0.0 && error_analysis.error_rate <= 100.0)
  
  // Test protocol comparison
  let compare_protocols = fn(metrics: Array[ProtocolMetric]) {
    let protocol_stats = analyze_protocol_performance(metrics)
    
    let mut comparisons = []
    
    for (protocol1, stats1) in protocol_stats {
      for (protocol2, stats2) in protocol_stats {
        if protocol1 != protocol2 {
          let duration_diff = stats1.avg_duration - stats2.avg_duration
          let success_rate_diff = stats1.success_rate - stats2.success_rate
          
          comparisons = comparisons.push({
            protocol1,
            protocol2,
            duration_diff,
            success_rate_diff,
            faster: duration_diff < 0,
            more_reliable: success_rate_diff > 0
          })
        }
      }
    }
    
    comparisons
  }
  
  let comparisons = compare_protocols(metrics_series)
  assert_true(comparisons.length() > 0)
  
  for comparison in comparisons {
    assert_not_eq(comparison.protocol1, comparison.protocol2)
    assert_true(comparison.faster == (comparison.duration_diff < 0))
    assert_true(comparison.more_reliable == (comparison.success_rate_diff > 0))
  }
}

// Test 6: Network Topology Discovery
test "network topology discovery" {
  // Network topology structures
  type NetworkNode = {
    id: String,
    address: String,
    neighbors: Array[String>,
    metadata: Array[(String, String)]
  }
  
  type NetworkTopology = {
    nodes: Array[NetworkNode>,
    edges: Array[(String, String)],
    discovered_at: Int
  }
  
  type TopologyChange = {
    change_type: String,  // "node_added", "node_removed", "edge_added", "edge_removed"
    affected_nodes: Array[String>,
    timestamp: Int
  }
  
  // Create network node
  let create_network_node = fn(id: String, address: String, neighbors: Array[String>, metadata: Array[(String, String)]) {
    {
      id,
      address,
      neighbors,
      metadata
    }
  }
  
  // Test node creation
  let node = create_network_node("node-1", "192.168.1.10", ["node-2", "node-3"], [("role", "server"), ("region", "us-west-1")])
  assert_eq(node.id, "node-1")
  assert_eq(node.address, "192.168.1.10")
  assert_eq(node.neighbors.length(), 2)
  assert_eq(node.metadata.length(), 2)
  
  // Create network topology
  let create_network_topology = fn(nodes: Array[NetworkNode>) {
    let mut edges = []
    
    for node in nodes {
      for neighbor in node.neighbors {
        // Add edge if not already present
        let edge_exists = edges.any(fn(e) { 
          (e.0 == node.id && e.1 == neighbor) || (e.0 == neighbor && e.1 == node.id)
        })
        
        if not(edge_exists) {
          edges = edges.push((node.id, neighbor))
        }
      }
    }
    
    {
      nodes,
      edges,
      discovered_at: 1640995200
    }
  }
  
  // Create test nodes
  let test_nodes = [
    create_network_node("node-1", "192.168.1.10", ["node-2", "node-3"], [("role", "server")]),
    create_network_node("node-2", "192.168.1.11", ["node-1", "node-4"], [("role", "client")]),
    create_network_node("node-3", "192.168.1.12", ["node-1", "node-4"], [("role", "client")]),
    create_network_node("node-4", "192.168.1.13", ["node-2", "node-3"], [("role", "server")])
  ]
  
  let topology = create_network_topology(test_nodes)
  
  assert_eq(topology.nodes.length(), 4)
  assert_eq(topology.edges.length(), 4)  // 1-2, 1-3, 2-4, 3-4
  assert_eq(topology.discovered_at, 1640995200)
  
  // Verify edges
  assert_true(topology.edges.contains(("node-1", "node-2")))
  assert_true(topology.edges.contains(("node-1", "node-3")))
  assert_true(topology.edges.contains(("node-2", "node-4")))
  assert_true(topology.edges.contains(("node-3", "node-4")))
  
  // Find shortest path
  let find_shortest_path = fn(topology: NetworkTopology, source: String, destination: String) {
    if source == destination {
      [source]
    } else {
      let mut queue = [(source, [source])]
      let mut visited = {}
      visited = visited.insert(source, true)
      
      while queue.length() > 0 {
        let (current, path) = queue[0]
        queue = queue.slice(1, queue.length())
        
        // Get neighbors of current node
        let current_node = topology.nodes.find(fn(n) { n.id == current })
        
        match current_node {
          Some(node) => {
            for neighbor in node.neighbors {
              if neighbor == destination {
                return path.push(neighbor)
              }
              
              if not(visited.contains_key(neighbor)) {
                visited = visited.insert(neighbor, true)
                queue = queue.push((neighbor, path.push(neighbor)))
              }
            }
          }
          None => {}
        }
      }
      
      []  // No path found
    }
  }
  
  // Test shortest path finding
  let path_1_to_4 = find_shortest_path(topology, "node-1", "node-4")
  assert_eq(path_1_to_4.length(), 3)  // node-1 -> node-2 -> node-4 or node-1 -> node-3 -> node-4
  assert_eq(path_1_to_4[0], "node-1")
  assert_eq(path_1_to_4[2], "node-4")
  
  let path_same_node = find_shortest_path(topology, "node-1", "node-1")
  assert_eq(path_same_node, ["node-1"])
  
  // Calculate network metrics
  let calculate_network_metrics = fn(topology: NetworkTopology) {
    let node_count = topology.nodes.length()
    let edge_count = topology.edges.length()
    
    // Calculate degree distribution
    let mut degree_distribution = {}
    
    for node in topology.nodes {
      let degree = node.neighbors.length()
      let count = match degree_distribution[degree] {
        Some(c) => c + 1,
        None => 1
      }
      
      degree_distribution = degree_distribution.insert(degree, count)
    }
    
    // Calculate average degree
    let total_degree = topology.nodes.reduce(fn(acc, node) { acc + node.neighbors.length() }, 0)
    let avg_degree = if node_count > 0 {
      total_degree.to_float() / node_count.to_float()
    } else {
      0.0
    }
    
    // Calculate clustering coefficient
    let mut clustering_coefficient = 0.0
    
    for node in topology.nodes {
      let k = node.neighbors.length()
      
      if k >= 2 {
        let mut edges_between_neighbors = 0
        
        for i in 0..k {
          for j in (i + 1)..k {
            let neighbor1 = node.neighbors[i]
            let neighbor2 = node.neighbors[j]
            
            let neighbor1_node = topology.nodes.find(fn(n) { n.id == neighbor1 })
            
            match neighbor1_node {
              Some(n1) => {
                if n1.neighbors.contains(neighbor2) {
                  edges_between_neighbors = edges_between_neighbors + 1
                }
              }
              None => {}
            }
          }
        }
        
        let possible_edges = k * (k - 1) / 2
        clustering_coefficient = clustering_coefficient + (edges_between_neighbors.to_float() / possible_edges.to_float())
      }
    }
    
    let avg_clustering = if node_count > 0 {
      clustering_coefficient / node_count.to_float()
    } else {
      0.0
    }
    
    {
      node_count,
      edge_count,
      avg_degree,
      avg_clustering,
      degree_distribution
    }
  }
  
  let metrics = calculate_network_metrics(topology)
  assert_eq(metrics.node_count, 4)
  assert_eq(metrics.edge_count, 4)
  assert_true(metrics.avg_degree > 0.0)
  assert_true(metrics.avg_clustering >= 0.0 && metrics.avg_clustering <= 1.0)
  
  // Detect topology changes
  let detect_topology_changes = fn(old_topology: NetworkTopology, new_topology: NetworkTopology) {
    let mut changes = []
    
    // Check for added nodes
    for new_node in new_topology.nodes {
      if not(old_topology.nodes.any(fn(n) { n.id == new_node.id })) {
        changes = changes.push({
          change_type: "node_added",
          affected_nodes: [new_node.id],
          timestamp: 1640995300
        })
      }
    }
    
    // Check for removed nodes
    for old_node in old_topology.nodes {
      if not(new_topology.nodes.any(fn(n) { n.id == old_node.id })) {
        changes = changes.push({
          change_type: "node_removed",
          affected_nodes: [old_node.id],
          timestamp: 1640995300
        })
      }
    }
    
    // Check for added edges
    for new_edge in new_topology.edges {
      if not(old_topology.edges.any(fn(e) { e == new_edge })) {
        changes = changes.push({
          change_type: "edge_added",
          affected_nodes: [new_edge.0, new_edge.1],
          timestamp: 1640995300
        })
      }
    }
    
    // Check for removed edges
    for old_edge in old_topology.edges {
      if not(new_topology.edges.any(fn(e) { e == old_edge })) {
        changes = changes.push({
          change_type: "edge_removed",
          affected_nodes: [old_edge.0, old_edge.1],
          timestamp: 1640995300
        })
      }
    }
    
    changes
  }
  
  // Test topology change detection
  let modified_nodes = [
    create_network_node("node-1", "192.168.1.10", ["node-2", "node-3", "node-5"], [("role", "server")]),
    create_network_node("node-2", "192.168.1.11", ["node-1", "node-4"], [("role", "client")]),
    create_network_node("node-3", "192.168.1.12", ["node-1", "node-4"], [("role", "client")]),
    create_network_node("node-4", "192.168.1.13", ["node-2", "node-3"], [("role", "server")]),
    create_network_node("node-5", "192.168.1.14", ["node-1"], [("role", "client")])  // New node
  ]
  
  let modified_topology = create_network_topology(modified_nodes)
  let changes = detect_topology_changes(topology, modified_topology)
  
  // Should detect node-5 being added
  assert_true(changes.any(fn(c) { c.change_type == "node_added" && c.affected_nodes.contains("node-5") }))
  
  // Test network resilience analysis
  let analyze_network_resilience = fn(topology: NetworkTopology) {
    let mut resilience_scores = {}
    
    for node in topology.nodes {
      // Simulate removing this node and check connectivity
      let remaining_nodes = topology.nodes.filter(fn(n) { n.id != node.id })
      
      if remaining_nodes.length() == 0 {
        resilience_scores = resilience_scores.insert(node.id, 0.0)
      } else {
        // Count reachable nodes after removing this node
        let mut reachable_count = 0
        
        for target in remaining_nodes {
          if remaining_nodes.length() > 0 {
            let path = find_shortest_path({
              nodes: remaining_nodes,
              edges: topology.edges.filter(fn(e) { 
                e.0 != node.id && e.1 != node.id
              }),
              discovered_at: topology.discovered_at
            }, remaining_nodes[0].id, target.id)
            
            if path.length() > 0 {
              reachable_count = reachable_count + 1
            }
          }
        }
        
        let resilience = reachable_count.to_float() / remaining_nodes.length().to_float()
        resilience_scores = resilience_scores.insert(node.id, resilience)
      }
    }
    
    resilience_scores
  }
  
  let resilience_scores = analyze_network_resilience(topology)
  
  // Verify resilience scores
  for (node_id, score) in resilience_scores {
    assert_true(score >= 0.0 && score <= 1.0)
  }
  
  // Test network visualization data generation
  let generate_visualization_data = fn(topology: NetworkTopology) {
    let mut node_data = []
    let mut edge_data = []
    
    for node in topology.nodes {
      let x = (node.id.split("-")[1].to_int() * 100) % 500
      let y = (node.id.split("-")[1].to_int() * 100) / 500 * 300
      
      node_data = node_data.push({
        id: node.id,
        label: node.id,
        x,
        y,
        metadata: node.metadata
      })
    }
    
    for edge in topology.edges {
      edge_data = edge_data.push({
        source: edge.0,
        target: edge.1
      })
    }
    
    {
      nodes: node_data,
      edges: edge_data
    }
  }
  
  let viz_data = generate_visualization_data(topology)
  assert_eq(viz_data.nodes.length(), topology.nodes.length())
  assert_eq(viz_data.edges.length(), topology.edges.length())
  
  // Verify node data structure
  for node_viz in viz_data.nodes {
    assert_true(node_viz.x >= 0 && node_viz.x <= 500)
    assert_true(node_viz.y >= 0 && node_viz.y <= 300)
    assert_true(node_viz.metadata.length() > 0)
  }
  
  // Verify edge data structure
  for edge_viz in viz_data.edges {
    assert_true(topology.nodes.any(fn(n) { n.id == edge_viz.source }))
    assert_true(topology.nodes.any(fn(n) { n.id == edge_viz.target }))
  }
}

// Test 7: Network Bandwidth Allocation
test "network bandwidth allocation" {
  // Bandwidth allocation structures
  type BandwidthAllocation = {
    connection_id: String,
    source: String,
    destination: String,
    allocated_bandwidth: Int,  // bytes per second
    guaranteed_bandwidth: Int,
    burst_bandwidth: Int,
    priority: Int,
    created_at: Int
  }
  
  type BandwidthPool = {
    total_bandwidth: Int,
    allocated_bandwidth: Int,
    available_bandwidth: Int,
    allocations: Array[BandwidthAllocation]
  }
  
  // Create bandwidth pool
  let create_bandwidth_pool = fn(total_bandwidth: Int) {
    {
      total_bandwidth,
      allocated_bandwidth: 0,
      available_bandwidth: total_bandwidth,
      allocations: []
    }
  }
  
  // Test pool creation
  let pool = create_bandwidth_pool(10485760)  // 10 MB/s
  assert_eq(pool.total_bandwidth, 10485760)
  assert_eq(pool.allocated_bandwidth, 0)
  assert_eq(pool.available_bandwidth, 10485760)
  assert_eq(pool.allocations.length(), 0)
  
  // Create bandwidth allocation
  let create_bandwidth_allocation = fn(connection_id: String, source: String, destination: String,
                                     guaranteed_bandwidth: Int, burst_bandwidth: Int, priority: Int, created_at: Int) {
    {
      connection_id,
      source,
      destination,
      allocated_bandwidth: guaranteed_bandwidth,
      guaranteed_bandwidth,
      burst_bandwidth,
      priority,
      created_at
    }
  }
  
  // Test allocation creation
  let allocation = create_bandwidth_allocation("conn-1", "client-1", "server-1", 1048576, 2097152, 1, 1640995200)
  assert_eq(allocation.connection_id, "conn-1")
  assert_eq(allocation.source, "client-1")
  assert_eq(allocation.destination, "server-1")
  assert_eq(allocation.allocated_bandwidth, 1048576)
  assert_eq(allocation.guaranteed_bandwidth, 1048576)
  assert_eq(allocation.burst_bandwidth, 2097152)
  assert_eq(allocation.priority, 1)
  assert_eq(allocation.created_at, 1640995200)
  
  // Allocate bandwidth
  let allocate_bandwidth = fn(pool: BandwidthPool, allocation: BandwidthAllocation) {
    if allocation.allocated_bandwidth <= pool.available_bandwidth {
      let updated_allocations = pool.allocations.push(allocation)
      
      {
        total_bandwidth: pool.total_bandwidth,
        allocated_bandwidth: pool.allocated_bandwidth + allocation.allocated_bandwidth,
        available_bandwidth: pool.available_bandwidth - allocation.allocated_bandwidth,
        allocations: updated_allocations
      }
    } else {
      pool  // Not enough bandwidth available
    }
  }
  
  // Test bandwidth allocation
  let allocated_pool = allocate_bandwidth(pool, allocation)
  assert_eq(allocated_pool.allocated_bandwidth, 1048576)
  assert_eq(allocated_pool.available_bandwidth, 9437184)  // 10485760 - 1048576
  assert_eq(allocated_pool.allocations.length(), 1)
  
  // Release bandwidth
  let release_bandwidth = fn(pool: BandwidthPool, connection_id: String) {
    let mut updated_allocations = []
    let mut released_bandwidth = 0
    
    for alloc in pool.allocations {
      if alloc.connection_id == connection_id {
        released_bandwidth = alloc.allocated_bandwidth
      } else {
        updated_allocations = updated_allocations.push(alloc)
      }
    }
    
    {
      total_bandwidth: pool.total_bandwidth,
      allocated_bandwidth: pool.allocated_bandwidth - released_bandwidth,
      available_bandwidth: pool.available_bandwidth + released_bandwidth,
      allocations: updated_allocations
    }
  }
  
  // Test bandwidth release
  let released_pool = release_bandwidth(allocated_pool, "conn-1")
  assert_eq(released_pool.allocated_bandwidth, 0)
  assert_eq(released_pool.available_bandwidth, 10485760)  // Back to original
  assert_eq(released_pool.allocations.length(), 0)
  
  // Test priority-based allocation
  let allocate_by_priority = fn(pool: BandwidthPool, allocations: Array[BandwidthAllocation>) {
    let mut sorted_allocations = allocations
    sorted_allocations.sort(fn(a, b) { if a.priority < b.priority { -1 } else if a.priority > b.priority { 1 } else { 0 } })
    
    let mut updated_pool = pool
    
    for allocation in sorted_allocations {
      updated_pool = allocate_bandwidth(updated_pool, allocation)
    }
    
    updated_pool
  }
  
  // Create multiple allocations with different priorities
  let allocation1 = create_bandwidth_allocation("conn-1", "client-1", "server-1", 1048576, 2097152, 1, 1640995200)
  let allocation2 = create_bandwidth_allocation("conn-2", "client-2", "server-1", 2097152, 4194304, 2, 1640995200)
  let allocation3 = create_bandwidth_allocation("conn-3", "client-3", "server-2", 524288, 1048576, 1, 1640995200)
  
  let priority_pool = allocate_by_priority(pool, [allocation1, allocation2, allocation3])
  
  // Verify allocations (should be allocated by priority)
  assert_eq(priority_pool.allocations.length(), 3)
  assert_eq(priority_pool.allocated_bandwidth, 1048576 + 2097152 + 524288)
  assert_eq(priority_pool.available_bandwidth, 10485760 - (1048576 + 2097152 + 524288))
  
  // Test bandwidth reallocation
  let reallocate_bandwidth = fn(pool: BandwidthPool, connection_id: String, new_bandwidth: Int) {
    let mut updated_pool = pool
    let mut old_bandwidth = 0
    
    // Find the allocation
    for allocation in pool.allocations {
      if allocation.connection_id == connection_id {
        old_bandwidth = allocation.allocated_bandwidth
        break
      }
    }
    
    if old_bandwidth > 0 {
      // First release the old allocation
      updated_pool = release_bandwidth(updated_pool, connection_id)
      
      // Then allocate with new bandwidth if available
      if new_bandwidth <= updated_pool.available_bandwidth {
        let old_allocation = pool.allocations.find(fn(a) { a.connection_id == connection_id })
        
        match old_allocation {
          Some(alloc) => {
            let new_allocation = {
              connection_id: alloc.connection_id,
              source: alloc.source,
              destination: alloc.destination,
              allocated_bandwidth: new_bandwidth,
              guaranteed_bandwidth: alloc.guaranteed_bandwidth,
              burst_bandwidth: alloc.burst_bandwidth,
              priority: alloc.priority,
              created_at: alloc.created_at
            }
            
            updated_pool = allocate_bandwidth(updated_pool, new_allocation)
          }
          None => updated_pool
        }
      }
    }
    
    updated_pool
  }
  
  // Test bandwidth reallocation
  let reallocated_pool = reallocate_bandwidth(priority_pool, "conn-2", 1048576)  // Reduce from 2MB to 1MB
  
  // Verify reallocation
  let conn2_allocation = reallocated_pool.allocations.find(fn(a) { a.connection_id == "conn-2" })
  match conn2_allocation {
    Some(alloc) => assert_eq(alloc.allocated_bandwidth, 1048576),
    None => assert_true(false)
  }
  
  // Test burst bandwidth usage
  let use_burst_bandwidth = fn(pool: BandwidthPool, connection_id: String, burst_duration: Int) {
    let mut updated_pool = pool
    
    for allocation in pool.allocations {
      if allocation.connection_id == connection_id {
        let burst_bandwidth = allocation.burst_bandwidth - allocation.allocated_bandwidth
        
        if burst_bandwidth > 0 && burst_bandwidth <= updated_pool.available_bandwidth {
          // Temporarily allocate burst bandwidth
          updated_pool = {
            total_bandwidth: updated_pool.total_bandwidth,
            allocated_bandwidth: updated_pool.allocated_bandwidth + burst_bandwidth,
            available_bandwidth: updated_pool.available_bandwidth - burst_bandwidth,
            allocations: updated_pool.allocations
          }
          
          // After burst duration, release burst bandwidth
          updated_pool = {
            total_bandwidth: updated_pool.total_bandwidth,
            allocated_bandwidth: updated_pool.allocated_bandwidth - burst_bandwidth,
            available_bandwidth: updated_pool.available_bandwidth + burst_bandwidth,
            allocations: updated_pool.allocations
          }
        }
        
        break
      }
    }
    
    updated_pool
  }
  
  // Test burst bandwidth usage
  let burst_pool = use_burst_bandwidth(priority_pool, "conn-1", 5000)
  
  // Pool should return to original state after burst
  assert_eq(burst_pool.allocated_bandwidth, priority_pool.allocated_bandwidth)
  assert_eq(burst_pool.available_bandwidth, priority_pool.available_bandwidth)
  
  // Test bandwidth utilization calculation
  let calculate_bandwidth_utilization = fn(pool: BandwidthPool) {
    if pool.total_bandwidth > 0 {
      pool.allocated_bandwidth.to_float() / pool.total_bandwidth.to_float() * 100.0
    } else {
      0.0
    }
  }
  
  let utilization = calculate_bandwidth_utilization(priority_pool)
  assert_true(utilization > 0.0 && utilization <= 100.0)
  
  // Test bandwidth fairness analysis
  let analyze_bandwidth_fairness = fn(pool: BandwidthPool) {
    if pool.allocations.length() == 0 {
      {
        fairness_index: 1.0,
        min_allocation: 0,
        max_allocation: 0,
        avg_allocation: 0.0
      }
    } else {
      let allocations = pool.allocations.map(fn(a) { a.allocated_bandwidth })
      let total_allocation = allocations.reduce(fn(acc, a) { acc + a }, 0)
      let avg_allocation = total_allocation.to_float() / allocations.length().to_float()
      
      let min_allocation = allocations.reduce(fn(acc, a) { if a < acc { a } else { acc }, allocations[0])
      let max_allocation = allocations.reduce(fn(acc, a) { if a > acc { a } else { acc }, allocations[0])
      
      // Calculate Jain's Fairness Index
      let sum_of_squares = allocations.reduce(fn(acc, a) { acc + a.to_float() * a.to_float() }, 0.0)
      let fairness_index = if total_allocation > 0 {
        (total_allocation.to_float() * total_allocation.to_float()) / 
        (allocations.length().to_float() * sum_of_squares)
      } else {
        1.0
      }
      
      {
        fairness_index,
        min_allocation,
        max_allocation,
        avg_allocation
      }
    }
  }
  
  let fairness = analyze_bandwidth_fairness(priority_pool)
  assert_true(fairness.fairness_index >= 0.0 && fairness.fairness_index <= 1.0)
  assert_true(fairness.min_allocation >= 0)
  assert_true(fairness.max_allocation >= fairness.min_allocation)
  assert_true(fairness.avg_allocation > 0.0)
  
  // Test bandwidth oversubscription detection
  let detect_oversubscription = fn(pool: BandwidthPool, threshold: Float) {
    let utilization = calculate_bandwidth_utilization(pool)
    utilization > threshold
  }
  
  let is_oversubscribed = detect_oversubscription(priority_pool, 90.0)
  assert_true(is_oversubscribed == true || is_oversubscribed == false)  // Just verify it returns a boolean
  
  // Test bandwidth optimization
  let optimize_bandwidth_allocation = fn(pool: BandwidthPool) {
    if pool.allocations.length() == 0 {
      pool
    } else {
      // Sort allocations by priority
      let mut sorted_allocations = pool.allocations
      sorted_allocations.sort(fn(a, b) { if a.priority < b.priority { -1 } else if a.priority > b.priority { 1 } else { 0 } })
      
      // Release all allocations
      let mut reset_pool = {
        total_bandwidth: pool.total_bandwidth,
        allocated_bandwidth: 0,
        available_bandwidth: pool.total_bandwidth,
        allocations: []
      }
      
      // Reallocate by priority
      for allocation in sorted_allocations {
        let adjusted_allocation = {
          connection_id: allocation.connection_id,
          source: allocation.source,
          destination: allocation.destination,
          allocated_bandwidth: allocation.guaranteed_bandwidth,  // Use guaranteed bandwidth
          guaranteed_bandwidth: allocation.guaranteed_bandwidth,
          burst_bandwidth: allocation.burst_bandwidth,
          priority: allocation.priority,
          created_at: allocation.created_at
        }
        
        reset_pool = allocate_bandwidth(reset_pool, adjusted_allocation)
      }
      
      reset_pool
    }
  }
  
  let optimized_pool = optimize_bandwidth_allocation(priority_pool)
  
  // Verify optimization
  let optimized_utilization = calculate_bandwidth_utilization(optimized_pool)
  let original_utilization = calculate_bandwidth_utilization(priority_pool)
  
  // Should have lower utilization after optimization (using only guaranteed bandwidth)
  assert_true(optimized_utilization <= original_utilization)
  
  // Test bandwidth quota enforcement
  let enforce_bandwidth_quota = fn(pool: BandwidthPool, connection_id: String, quota: Int) {
    let mut updated_pool = pool
    
    for allocation in pool.allocations {
      if allocation.connection_id == connection_id {
        if allocation.allocated_bandwidth > quota {
          // Reduce allocation to quota
          let reduction = allocation.allocated_bandwidth - quota
          
          updated_pool = {
            total_bandwidth: pool.total_bandwidth,
            allocated_bandwidth: pool.allocated_bandwidth - reduction,
            available_bandwidth: pool.available_bandwidth + reduction,
            allocations: pool.allocations
          }
        }
        
        break
      }
    }
    
    updated_pool
  }
  
  // Test quota enforcement
  let quota_pool = enforce_bandwidth_quota(priority_pool, "conn-2", 1048576)  // Limit conn-2 to 1MB
  
  // Verify quota enforcement
  let conn2_quota_allocation = quota_pool.allocations.find(fn(a) { a.connection_id == "conn-2" })
  match conn2_quota_allocation {
    Some(alloc) => assert_eq(alloc.allocated_bandwidth, 1048576),
    None => assert_true(false)
  }
}

// Test 8: Network Latency Prediction
test "network latency prediction" {
  // Latency prediction structures
  type LatencyRecord = {
    timestamp: Int,
    source: String,
    destination: String,
    latency: Int
  }
  
  type LatencyModel = {
    source: String,
    destination: String,
    base_latency: Float,
    trend: Float,
    seasonality: Array[Float],
    accuracy: Float,
    last_updated: Int
  }
  
  type LatencyPrediction = {
    source: String,
    destination: String,
    predicted_latency: Float,
    confidence: Float,
    prediction_time: Int
  }
  
  // Create latency record
  let create_latency_record = fn(timestamp: Int, source: String, destination: String, latency: Int) {
    {
      timestamp,
      source,
      destination,
      latency
    }
  }
  
  // Test record creation
  let record = create_latency_record(1640995200, "client-1", "server-1", 50)
  assert_eq(record.timestamp, 1640995200)
  assert_eq(record.source, "client-1")
  assert_eq(record.destination, "server-1")
  assert_eq(record.latency, 50)
  
  // Create latency records series
  let create_latency_series = fn(source: String, destination: String, count: Int) {
    let mut records = []
    
    for i in 0..count {
      let timestamp = 1640995200 + i * 300  // 5-minute intervals
      let base_latency = 30
      let trend = i * 0.5  // Increasing trend
      let seasonality = 10.0 * (i.to_float() * 0.1).sin()  // Seasonal component
      let noise = (i % 5) * 2  // Random noise
      
      let latency = (base_latency + trend + seasonality + noise).to_int()
      
      records = records.push(create_latency_record(timestamp, source, destination, latency))
    }
    
    records
  }
  
  let latency_series = create_latency_series("client-1", "server-1", 24)  // 24 records (2 hours)
  assert_eq(latency_series.length(), 24)
  
  // Build latency prediction model
  let build_latency_model = fn(records: Array[LatencyRecord>) {
    if records.length() < 2 {
      None
    } else {
      let source = records[0].source
      let destination = records[0].destination
      
      // Calculate base latency (average)
      let total_latency = records.reduce(fn(acc, r) { acc + r.latency }, 0)
      let base_latency = total_latency.to_float() / records.length().to_float()
      
      // Calculate trend (linear regression)
      let n = records.length().to_float()
      let sum_x = (0..records.length()).reduce(fn(acc, i) { acc + i.to_float() }, 0.0)
      let sum_y = records.reduce(fn(acc, r) { acc + r.latency.to_float() }, 0.0)
      let sum_xy = (0..records.length()).reduce(fn(acc, i) { 
        acc + i.to_float() * records[i].latency.to_float()
      }, 0.0)
      let sum_x2 = (0..records.length()).reduce(fn(acc, i) { 
        acc + i.to_float() * i.to_float()
      }, 0.0)
      
      let trend = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
      
      // Calculate seasonality (simplified)
      let mut seasonality = []
      
      for i in 0..24 {  // 24-hour seasonality
        let hour_values = []
        
        for j in 0..records.length() {
          let hour = (records[j].timestamp / 3600) % 24
          
          if hour == i {
            hour_values = hour_values.push(records[j].latency.to_float())
          }
        }
        
        let seasonal_value = if hour_values.length() > 0 {
          hour_values.reduce(fn(acc, v) { acc + v }, 0.0) / hour_values.length().to_float() - base_latency
        } else {
          0.0
        }
        
        seasonality = seasonality.push(seasonal_value)
      }
      
      // Calculate model accuracy (R-squared)
      let mut ss_res = 0.0
      let mut ss_tot = 0.0
      let mean_y = sum_y / n
      
      for i in 0..records.length() {
        let predicted = base_latency + trend * i.to_float() + seasonality[(records[i].timestamp / 3600) % 24]
        let actual = records[i].latency.to_float()
        
        ss_res = ss_res + (actual - predicted) * (actual - predicted)
        ss_tot = ss_tot + (actual - mean_y) * (actual - mean_y)
      }
      
      let accuracy = if ss_tot > 0.0 {
        1.0 - (ss_res / ss_tot)
      } else {
        1.0
      }
      
      Some({
        source,
        destination,
        base_latency,
        trend,
        seasonality,
        accuracy,
        last_updated: records[records.length() - 1].timestamp
      })
    }
  }
  
  // Test model building
  let model_option = build_latency_model(latency_series)
  assert_true(model_option.is_some())
  
  let model = model_option.unwrap()
  assert_eq(model.source, "client-1")
  assert_eq(model.destination, "server-1")
  assert_true(model.base_latency > 0.0)
  assert_true(model.accuracy >= 0.0 && model.accuracy <= 1.0)
  assert_eq(model.seasonality.length(), 24)
  
  // Predict latency using model
  let predict_latency = fn(model: LatencyModel, prediction_time: Int) {
    let time_diff = (prediction_time - model.last_updated) / 300  // 5-minute intervals
    
    let predicted_latency = model.base_latency + model.trend * time_diff.to_float() + 
                         model.seasonality[(prediction_time / 3600) % 24]
    
    // Calculate confidence based on model accuracy and prediction distance
    let confidence = model.accuracy * (1.0 - (time_diff.to_float() / 100.0).min(0.5))
    
    {
      source: model.source,
      destination: model.destination,
      predicted_latency,
      confidence,
      prediction_time
    }
  }
  
  // Test latency prediction
  let prediction = predict_latency(model, 1640997000)  // 30 minutes in the future
  assert_eq(prediction.source, "client-1")
  assert_eq(prediction.destination, "server-1")
  assert_true(prediction.predicted_latency > 0.0)
  assert_true(prediction.confidence >= 0.0 && prediction.confidence <= 1.0)
  assert_eq(prediction.prediction_time, 1640997000)
  
  // Test multiple predictions
  let predict_multiple_latencies = fn(models: Array[LatencyModel>, prediction_time: Int) {
    models.map(fn(m) { predict_latency(m, prediction_time) })
  }
  
  // Create models for multiple paths
  let latency_series_2 = create_latency_series("client-1", "server-2", 24)
  let latency_series_3 = create_latency_series("client-2", "server-1", 24)
  
  let model_option_2 = build_latency_model(latency_series_2)
  let model_option_3 = build_latency_model(latency_series_3)
  
  let mut models = [model]
  
  if model_option_2.is_some() {
    models = models.push(model_option_2.unwrap())
  }
  
  if model_option_3.is_some() {
    models = models.push(model_option_3.unwrap())
  }
  
  let predictions = predict_multiple_latencies(models, 1640997000)
  assert_eq(predictions.length(), models.length())
  
  // Test prediction accuracy validation
  let validate_prediction_accuracy = fn(model: LatencyModel, test_records: Array[LatencyRecord>) {
    if test_records.length() == 0 {
      1.0
    } else {
      let mut total_error = 0.0
      
      for record in test_records {
        let prediction = predict_latency(model, record.timestamp)
        let error = (prediction.predicted_latency - record.latency.to_float()).abs()
        total_error = total_error + error
      }
      
      let mean_absolute_error = total_error / test_records.length().to_float()
      let avg_latency = test_records.reduce(fn(acc, r) { acc + r.latency }, 0) / test_records.length()
      
      if avg_latency > 0 {
        1.0 - (mean_absolute_error / avg_latency.to_float())
      } else {
        1.0
      }
    }
  }
  
  // Create test records for validation
  let test_records = create_latency_series("client-1", "server-1", 6)  // 6 test records
  
  let accuracy = validate_prediction_accuracy(model, test_records)
  assert_true(accuracy >= 0.0 && accuracy <= 1.0)
  
  // Test latency anomaly detection
  let detect_latency_anomalies = fn(records: Array[LatencyRecord], threshold: Float) {
    if records.length() < 2 {
      []
    } else {
      let latencies = records.map(fn(r) { r.latency.to_float() })
      let mean = latencies.reduce(fn(acc, l) { acc + l }, 0.0) / latencies.length().to_float()
      let variance = latencies.reduce(fn(acc, l) { 
        acc + (l - mean) * (l - mean)
      }, 0.0) / latencies.length().to_float()
      
      let std_dev = variance.sqrt()
      let anomaly_threshold = mean + std_dev * threshold
      
      records.filter(fn(r) { r.latency.to_float() > anomaly_threshold })
    }
  }
  
  // Add some anomalous records
  let mut anomalous_records = latency_series
  anomalous_records = anomalous_records.push(create_latency_record(1640997200, "client-1", "server-1", 200))  // High latency
  
  let anomalies = detect_latency_anomalies(anomalous_records, 2.0)
  assert_true(anomalies.length() > 0)
  
  // Test path latency prediction
  type NetworkPath = {
    nodes: Array[String],
    total_latency: Int
  }
  
  let predict_path_latency = fn(path: NetworkPath, models: Array[LatencyModel], prediction_time: Int) {
    let mut predicted_total = 0.0
    let mut confidence = 1.0
    
    for i in 0..(path.nodes.length() - 1) {
      let source = path.nodes[i]
      let destination = path.nodes[i + 1]
      
      let mut found_model = None
      
      for model in models {
        if model.source == source && model.destination == destination {
          found_model = Some(model)
          break
        }
      }
      
      match found_model {
        Some(model) => {
          let prediction = predict_latency(model, prediction_time)
          predicted_total = predicted_total + prediction.predicted_latency
          confidence = confidence * prediction.confidence
        }
        None => {
          // Use historical average if no model available
          predicted_total = predicted_total + path.total_latency.to_float() / (path.nodes.length() - 1).to_float()
          confidence = confidence * 0.5  // Lower confidence for missing model
        }
      }
    }
    
    {
      path,
      predicted_latency: predicted_total,
      confidence,
      prediction_time
    }
  }
  
  // Test path latency prediction
  let test_path = {
    nodes: ["client-1", "server-1", "database-1"],
    total_latency: 150
  }
  
  let path_prediction = predict_path_latency(test_path, models, 1640997000)
  assert_eq(path_prediction.path, test_path)
  assert_true(path_prediction.predicted_latency > 0.0)
  assert_true(path_prediction.confidence >= 0.0 && path_prediction.confidence <= 1.0)
  
  // Test model updating
  let update_latency_model = fn(model: LatencyModel, new_records: Array[LatencyRecord>) {
    let all_records = []
    
    // Add new records to existing ones
    for record in new_records {
      all_records = all_records.push(record)
    }
    
    // Rebuild model with all records
    let updated_model_option = build_latency_model(all_records)
    
    match updated_model_option {
      Some(updated_model) => updated_model,
      None => model
    }
  }
  
  // Test model updating
  let new_records = create_latency_series("client-1", "server-1", 6)
  let updated_model = update_latency_model(model, new_records)
  
  assert_eq(updated_model.source, model.source)
  assert_eq(updated_model.destination, model.destination)
  assert_eq(updated_model.last_updated, new_records[new_records.length() - 1].timestamp)
  
  // Test prediction confidence calculation
  let calculate_prediction_confidence = fn(model: LatencyModel, prediction_distance: Int) {
    let time_factor = if prediction_distance < 60 {
      1.0  // High confidence for short-term predictions
    } else if prediction_distance < 300 {
      0.8  // Medium confidence for medium-term predictions
    } else {
      0.6  // Lower confidence for long-term predictions
    }
    
    let data_factor = if model.seasonality.length() > 0 {
      0.9  // Slightly lower confidence with seasonality
    } else {
      1.0
    }
    
    model.accuracy * time_factor * data_factor
  }
  
  let confidence = calculate_prediction_confidence(model, 180)  // 3 hours ahead
  assert_true(confidence >= 0.0 && confidence <= 1.0)
  
  // Test latency trend analysis
  let analyze_latency_trend = fn(records: Array[LatencyRecord]) {
    if records.length() < 2 {
      "insufficient_data"
    } else {
      let first_latency = records[0].latency.to_float()
      let last_latency = records[records.length() - 1].latency.to_float()
      
      let trend = (last_latency - first_latency) / records.length().to_float()
      
      if trend > 1.0 {
        "increasing"
      } else if trend < -1.0 {
        "decreasing"
      } else {
        "stable"
      }
    }
  }
  
  let trend = analyze_latency_trend(latency_series)
  assert_true(trend == "increasing" || trend == "decreasing" || trend == "stable" || trend == "insufficient_data")
}

// Test 9: Network Traffic Shaping
test "network traffic shaping" {
  // Traffic shaping structures
  type TrafficRule = {
    id: String,
    source: String,
    destination: String,
    protocol: String,
    max_bandwidth: Int,    // bytes per second
    burst_size: Int,       // bytes
    priority: Int,
    action: String         // "allow", "throttle", "block"
  }
  
  type TrafficShaper = {
    rules: Array[TrafficRule],
    default_bandwidth: Int,
    total_bandwidth: Int
  }
  
  type Packet = {
    id: String,
    source: String,
    destination: String,
    protocol: String,
    size: Int,
    timestamp: Int
  }
  
  type ShapingResult = {
    packet: Packet,
    action: String,
    delay: Int,
    shaped_size: Int
  }
  
  // Create traffic rule
  let create_traffic_rule = fn(id: String, source: String, destination: String, protocol: String,
                              max_bandwidth: Int, burst_size: Int, priority: Int, action: String) {
    {
      id,
      source,
      destination,
      protocol,
      max_bandwidth,
      burst_size,
      priority,
      action
    }
  }
  
  // Test rule creation
  let rule = create_traffic_rule("rule-1", "client-1", "server-1", "HTTP", 1048576, 2097152, 1, "allow")
  assert_eq(rule.id, "rule-1")
  assert_eq(rule.source, "client-1")
  assert_eq(rule.destination, "server-1")
  assert_eq(rule.protocol, "HTTP")
  assert_eq(rule.max_bandwidth, 1048576)  // 1MB/s
  assert_eq(rule.burst_size, 2097152)     // 2MB
  assert_eq(rule.priority, 1)
  assert_eq(rule.action, "allow")
  
  // Create traffic shaper
  let create_traffic_shaper = fn(default_bandwidth: Int, total_bandwidth: Int) {
    {
      rules: [],
      default_bandwidth,
      total_bandwidth
    }
  }
  
  // Test shaper creation
  let shaper = create_traffic_shaper(1048576, 10485760)  // 1MB default, 10MB total
  assert_eq(shaper.default_bandwidth, 1048576)
  assert_eq(shaper.total_bandwidth, 10485760)
  assert_eq(shaper.rules.length(), 0)
  
  // Add rule to shaper
  let add_rule = fn(shaper: TrafficShaper, rule: TrafficRule) {
    { shaper | rules: shaper.rules.push(rule) }
  }
  
  // Test rule addition
  let shaper_with_rule = add_rule(shaper, rule)
  assert_eq(shaper_with_rule.rules.length(), 1)
  assert_eq(shaper_with_rule.rules[0].id, "rule-1")
  
  // Create packet
  let create_packet = fn(id: String, source: String, destination: String, protocol: String, size: Int, timestamp: Int) {
    {
      id,
      source,
      destination,
      protocol,
      size,
      timestamp
    }
  }
  
  // Test packet creation
  let packet = create_packet("packet-1", "client-1", "server-1", "HTTP", 1024, 1640995200)
  assert_eq(packet.id, "packet-1")
  assert_eq(packet.source, "client-1")
  assert_eq(packet.destination, "server-1")
  assert_eq(packet.protocol, "HTTP")
  assert_eq(packet.size, 1024)
  assert_eq(packet.timestamp, 1640995200)
  
  // Apply traffic shaping
  let apply_traffic_shaping = fn(shaper: TrafficShaper, packet: Packet, current_time: Int) {
    // Find matching rule
    let mut matching_rule = None
    
    for rule in shaper.rules {
      if (rule.source == packet.source || rule.source == "*") &&
         (rule.destination == packet.destination || rule.destination == "*") &&
         (rule.protocol == packet.protocol || rule.protocol == "*") {
        
        match matching_rule {
          None => matching_rule = Some(rule),
          Some(current_rule) => {
            // Use rule with higher priority (lower number)
            if rule.priority < current_rule.priority {
              matching_rule = Some(rule)
            }
          }
        }
      }
    }
    
    match matching_rule {
      Some(rule) => {
        match rule.action {
          "block" => {
            {
              packet,
              action: "blocked",
              delay: 0,
              shaped_size: 0
            }
          }
          "throttle" => {
            // Calculate delay based on bandwidth and packet size
            let transmission_time = (packet.size.to_float() / rule.max_bandwidth.to_float()) * 1000.0
            let delay = transmission_time.to_int()
            
            {
              packet,
              action: "throttled",
              delay,
              shaped_size: packet.size
            }
          }
          _ => {
            {
              packet,
              action: "allowed",
              delay: 0,
              shaped_size: packet.size
            }
          }
        }
      }
      None => {
        // Apply default bandwidth
        let transmission_time = (packet.size.to_float() / shaper.default_bandwidth.to_float()) * 1000.0
        let delay = transmission_time.to_int()
        
        {
          packet,
          action: "allowed",
          delay,
          shaped_size: packet.size
        }
      }
    }
  }
  
  // Test traffic shaping
  let shaping_result = apply_traffic_shaping(shaper_with_rule, packet, 1640995200)
  assert_eq(shaping_result.packet.id, "packet-1")
  assert_eq(shaping_result.action, "allowed")
  assert_true(shaping_result.delay >= 0)
  assert_eq(shaping_result.shaped_size, 1024)
  
  // Test blocking rule
  let block_rule = create_traffic_rule("block-rule", "client-2", "*", "*", 0, 0, 1, "block")
  let shaper_with_block = add_rule(shaper_with_rule, block_rule)
  
  let block_packet = create_packet("packet-2", "client-2", "server-1", "HTTP", 1024, 1640995200)
  let block_result = apply_traffic_shaping(shaper_with_block, block_packet, 1640995200)
  
  assert_eq(block_result.action, "blocked")
  assert_eq(block_result.shaped_size, 0)
  
  // Test throttling rule
  let throttle_rule = create_traffic_rule("throttle-rule", "client-3", "*", "*", 512000, 1024000, 1, "throttle")
  let shaper_with_throttle = add_rule(shaper_with_block, throttle_rule)
  
  let throttle_packet = create_packet("packet-3", "client-3", "server-1", "HTTP", 2048, 1640995200)
  let throttle_result = apply_traffic_shaping(shaper_with_throttle, throttle_packet, 1640995200)
  
  assert_eq(throttle_result.action, "throttled")
  assert_true(throttle_result.delay > 0)
  assert_eq(throttle_result.shaped_size, 2048)
  
  // Calculate expected delay: 2048 bytes / 512000 bytes/s * 1000 = 4000ms
  assert_eq(throttle_result.delay, 4000)
  
  // Test burst handling
  let handle_burst = fn(shaper: TrafficShaper, packets: Array[Packet], current_time: Int) {
    let mut results = []
    let mut bucket_sizes = {}
    
    // Initialize token buckets
    for rule in shaper.rules {
      bucket_sizes = bucket_sizes.insert(rule.id, rule.burst_size)
    }
    
    for packet in packets {
      let result = apply_traffic_shaping(shaper, packet, current_time)
      
      // Update token bucket for throttled packets
      if result.action == "throttled" {
        let mut matching_rule = None
        
        for rule in shaper.rules {
          if (rule.source == packet.source || rule.source == "*") &&
             (rule.destination == packet.destination || rule.destination == "*") &&
             (rule.protocol == packet.protocol || rule.protocol == "*") {
            
            match matching_rule {
              None => matching_rule = Some(rule),
              Some(current_rule) => {
                if rule.priority < current_rule.priority {
                  matching_rule = Some(rule)
                }
              }
            }
          }
        }
        
        match matching_rule {
          Some(rule) => {
            let current_bucket = bucket_sizes[rule.id]
            
            if current_bucket >= packet.size {
              bucket_sizes = bucket_sizes.insert(rule.id, current_bucket - packet.size)
              // Allow packet without additional delay
              results = results.push({
                packet: result.packet,
                action: "allowed",
                delay: 0,
                shaped_size: packet.size
              })
            } else {
              // Insufficient tokens, apply delay
              results = results.push(result)
            }
          }
          None => results = results.push(result)
        }
      } else {
        results = results.push(result)
      }
    }
    
    results
  }
  
  // Test burst handling
  let burst_packets = [
    create_packet("burst-1", "client-3", "server-1", "HTTP", 512000, 1640995200),  // Within burst
    create_packet("burst-2", "client-3", "server-1", "HTTP", 512000, 1640995200),  // Within burst
    create_packet("burst-3", "client-3", "server-1", "HTTP", 1024000, 1640995200) // Exceeds burst
  ]
  
  let burst_results = handle_burst(shaper_with_throttle, burst_packets, 1640995200)
  assert_eq(burst_results.length(), 3)
  
  // First two packets should be allowed (within burst size), third throttled
  assert_eq(burst_results[0].action, "allowed")
  assert_eq(burst_results[1].action, "allowed")
  assert_eq(burst_results[2].action, "throttled")
  
  // Test traffic class-based shaping
  let shape_by_class = fn(shaper: TrafficShaper, packets: Array[Packet], current_time: Int) {
    let mut class_results = {}
    
    for packet in packets {
      let result = apply_traffic_shaping(shaper, packet, current_time)
      
      let class_name = packet.protocol + "-" + packet.source
      let class_packets = match class_results[class_name] {
        Some(packets) => packets,
        None => []
      }
      
      class_results = class_results.insert(class_name, class_packets.push(result))
    }
    
    class_results
  }
  
  // Test class-based shaping
  let class_packets = [
    create_packet("class-1", "client-1", "server-1", "HTTP", 1024, 1640995200),
    create_packet("class-2", "client-1", "server-1", "HTTP", 2048, 1640995300),
    create_packet("class-3", "client-2", "server-1", "TCP", 1024, 1640995400),
    create_packet("class-4", "client-2", "server-1", "UDP", 512, 1640995500)
  ]
  
  let class_results = shape_by_class(shaper_with_block, class_packets, 1640995200)
  
  // Verify class-based results
  assert_true(class_results.contains_key("HTTP-client-1"))
  assert_true(class_results.contains_key("TCP-client-2"))
  assert_true(class_results.contains_key("UDP-client-2"))
  
  // HTTP-client-1 should have 2 packets
  assert_eq(class_results["HTTP-client-1"].length(), 2)
  
  // TCP-client-2 should have 1 packet (blocked)
  assert_eq(class_results["TCP-client-2"].length(), 1)
  assert_eq(class_results["TCP-client-2"][0].action, "blocked")
  
  // Calculate traffic shaping statistics
  let calculate_shaping_stats = fn(results: Array[ShapingResult]) {
    let total_packets = results.length()
    let allowed_packets = results.filter(fn(r) { r.action == "allowed" }).length()
    let throttled_packets = results.filter(fn(r) { r.action == "throttled" }).length()
    let blocked_packets = results.filter(fn(r) { r.action == "blocked" }).length()
    
    let total_delay = results.reduce(fn(acc, r) { acc + r.delay }, 0)
    let total_shaped_size = results.reduce(fn(acc, r) { acc + r.shaped_size }, 0)
    
    {
      total_packets,
      allowed_packets,
      throttled_packets,
      blocked_packets,
      allow_rate: if total_packets > 0 { allowed_packets.to_float() / total_packets.to_float() * 100.0 } else { 0.0 },
      throttle_rate: if total_packets > 0 { throttled_packets.to_float() / total_packets.to_float() * 100.0 } else { 0.0 },
      block_rate: if total_packets > 0 { blocked_packets.to_float() / total_packets.to_float() * 100.0 } else { 0.0 },
      avg_delay: if total_packets > 0 { total_delay.to_float() / total_packets.to_float() } else { 0.0 },
      total_shaped_size
    }
  }
  
  // Test statistics calculation
  let all_results = burst_results + [
    block_result,
    throttle_result,
    shaping_result
  ]
  
  let stats = calculate_shaping_stats(all_results)
  assert_eq(stats.total_packets, all_results.length())
  assert_true(stats.allow_rate >= 0.0 && stats.allow_rate <= 100.0)
  assert_true(stats.throttle_rate >= 0.0 && stats.throttle_rate <= 100.0)
  assert_true(stats.block_rate >= 0.0 && stats.block_rate <= 100.0)
  assert_true(stats.avg_delay >= 0.0)
  assert_true(stats.total_shaped_size > 0)
  
  // Verify rates sum to 100%
  assert_true((stats.allow_rate + stats.throttle_rate + stats.block_rate - 100.0).abs() < 0.01)
  
  // Test dynamic rule updates
  let update_rule = fn(shaper: TrafficShaper, rule_id: String, updates: {
    max_bandwidth: Option[Int],
    burst_size: Option[Int>,
    priority: Option[Int>,
    action: Option<String]
  }) {
    let mut updated_rules = []
      
      for rule in shaper.rules {
        if rule.id == rule_id {
          let updated_rule = {
            id: rule.id,
            source: rule.source,
            destination: rule.destination,
            protocol: rule.protocol,
            max_bandwidth: match updates.max_bandwidth { Some(bw) => bw, None => rule.max_bandwidth },
            burst_size: match updates.burst_size { Some(bs) => bs, None => rule.burst_size },
            priority: match updates.priority { Some(p) => p, None => rule.priority },
            action: match updates.action { Some(a) => a, None => rule.action }
          }
          
          updated_rules = updated_rules.push(updated_rule)
        } else {
          updated_rules = updated_rules.push(rule)
        }
      }
      
      { shaper | rules: updated_rules }
  }
  
  // Test rule update
  let updated_shaper = update_rule(shaper_with_throttle, "throttle-rule", {
    max_bandwidth: Some(256000),  // Reduce from 512KB/s to 256KB/s
    burst_size: None,
    priority: None,
    action: None
  })
  
  // Verify rule was updated
  let updated_rule = updated_shaper.rules.find(fn(r) { r.id == "throttle-rule" })
  match updated_rule {
    Some(rule) => assert_eq(rule.max_bandwidth, 256000),
    None => assert_true(false)
  }
  
  // Test updated shaping behavior
  let updated_throttle_result = apply_traffic_shaping(updated_shaper, throttle_packet, 1640995200)
  
  // Delay should increase with reduced bandwidth
  assert_eq(updated_throttle_result.action, "throttled")
  assert_eq(updated_throttle_result.delay, 8000)  // 2048 bytes / 256000 bytes/s * 1000 = 8000ms
  
  // Test priority-based shaping
  let shape_by_priority = fn(shaper: TrafficShaper, packets: Array[Packet>, current_time: Int) {
    let mut sorted_packets = packets
    sorted_packets.sort(fn(a, b) { if a.timestamp < b.timestamp { -1 } else if a.timestamp > b.timestamp { 1 } else { 0 } })
    
    let mut results = []
    
    for packet in sorted_packets {
      let result = apply_traffic_shaping(shaper, packet, current_time)
      results = results.push(result)
    }
    
    results
  }
  
  // Test priority-based shaping
  let priority_packets = [
    create_packet("priority-1", "client-1", "server-1", "HTTP", 1024, 1640995100),  // Earlier
    create_packet("priority-2", "client-1", "server-1", "HTTP", 2048, 1640995200),  // Later
    create_packet("priority-3", "client-1", "server-1", "HTTP", 512, 1640995300)    // Latest
  ]
  
  let priority_results = shape_by_priority(shaper_with_rule, priority_packets, 1640995200)
  assert_eq(priority_results.length(), 3)
  
  // Packets should be processed in timestamp order
  for i in 1..priority_results.length() {
    assert_true(priority_results[i-1].packet.timestamp <= priority_results[i].packet.timestamp)
  }
}

// Test 10: Network Quality of Service (QoS)
test "network quality of service" {
  // QoS structures
  type QoSClass = {
    id: String,
    name: String,
    priority: Int,
    min_bandwidth: Int,
    max_bandwidth: Int,
    max_delay: Int,        // milliseconds
    max_jitter: Int,       // milliseconds
    max_loss_rate: Float   // percentage
  }
  
  type QoSPolicy = {
    name: String,
    classes: Array[QoSClass],
    default_class: String
  }
  
  type QoSFlow = {
    id: String,
    source: String,
    destination: String,
    protocol: String,
    qos_class_id: String,
    metrics: {
      bandwidth: Int,
      delay: Int,
      jitter: Int,
      loss_rate: Float
    }
  }
  
  type QoSResult = {
    flow: QoSFlow,
    compliant: Bool,
    violations: Array[String]
  }
  
  // Create QoS class
  let create_qos_class = fn(id: String, name: String, priority: Int, min_bandwidth: Int, 
                           max_bandwidth: Int, max_delay: Int, max_jitter: Int, max_loss_rate: Float) {
    {
      id,
      name,
      priority,
      min_bandwidth,
      max_bandwidth,
      max_delay,
      max_jitter,
      max_loss_rate
    }
  }
  
  // Test QoS class creation
  let qos_class = create_qos_class("gold", "Gold Class", 1, 1048576, 10485760, 50, 10, 0.1)
  assert_eq(qos_class.id, "gold")
  assert_eq(qos_class.name, "Gold Class")
  assert_eq(qos_class.priority, 1)
  assert_eq(qos_class.min_bandwidth, 1048576)   // 1MB/s
  assert_eq(qos_class.max_bandwidth, 10485760)  // 10MB/s
  assert_eq(qos_class.max_delay, 50)           // 50ms
  assert_eq(qos_class.max_jitter, 10)          // 10ms
  assert_eq(qos_class.max_loss_rate, 0.1)     // 0.1%
  
  // Create QoS policy
  let create_qos_policy = fn(name: String, classes: Array[QoSClass], default_class: String) {
    {
      name,
      classes,
      default_class
    }
  }
  
  // Test policy creation
  let silver_class = create_qos_class("silver", "Silver Class", 2, 512000, 5242880, 100, 20, 0.5)
  let bronze_class = create_qos_class("bronze", "Bronze Class", 3, 256000, 1048576, 200, 50, 1.0)
  
  let policy = create_qos_policy("Standard Policy", [qos_class, silver_class, bronze_class], "bronze")
  
  assert_eq(policy.name, "Standard Policy")
  assert_eq(policy.classes.length(), 3)
  assert_eq(policy.default_class, "bronze")
  
  // Create QoS flow
  let create_qos_flow = fn(id: String, source: String, destination: String, protocol: String, 
                          qos_class_id: String, bandwidth: Int, delay: Int, jitter: Int, loss_rate: Float) {
    {
      id,
      source,
      destination,
      protocol,
      qos_class_id,
      metrics: {
        bandwidth,
        delay,
        jitter,
        loss_rate
      }
    }
  }
  
  // Test flow creation
  let flow = create_qos_flow("flow-1", "client-1", "server-1", "HTTP", "gold", 2097152, 30, 5, 0.05)
  assert_eq(flow.id, "flow-1")
  assert_eq(flow.source, "client-1")
  assert_eq(flow.destination, "server-1")
  assert_eq(flow.protocol, "HTTP")
  assert_eq(flow.qos_class_id, "gold")
  assert_eq(flow.metrics.bandwidth, 2097152)
  assert_eq(flow.metrics.delay, 30)
  assert_eq(flow.metrics.jitter, 5)
  assert_eq(flow.metrics.loss_rate, 0.05)
  
  // Check QoS compliance
  let check_qos_compliance = fn(policy: QoSPolicy, flow: QoSFlow) {
    let mut compliant = true
    let mut violations = []
    
    // Find the QoS class
    let qos_class = policy.classes.find(fn(c) { c.id == flow.qos_class_id })
    
    match qos_class {
      Some(cls) => {
        // Check bandwidth compliance
        if flow.metrics.bandwidth < cls.min_bandwidth {
          compliant = false
          violations = violations.push("bandwidth_below_minimum")
        } else if flow.metrics.bandwidth > cls.max_bandwidth {
          compliant = false
          violations = violations.push("bandwidth_exceeds_maximum")
        }
        
        // Check delay compliance
        if flow.metrics.delay > cls.max_delay {
          compliant = false
          violations = violations.push("delay_exceeds_maximum")
        }
        
        // Check jitter compliance
        if flow.metrics.jitter > cls.max_jitter {
          compliant = false
          violations = violations.push("jitter_exceeds_maximum")
        }
        
        // Check loss rate compliance
        if flow.metrics.loss_rate > cls.max_loss_rate {
          compliant = false
          violations = violations.push("loss_rate_exceeds_maximum")
        }
      }
      None => {
        compliant = false
        violations = violations.push("qos_class_not_found")
      }
    }
    
    {
      flow,
      compliant,
      violations
    }
  }
  
  // Test QoS compliance checking
  let compliance_result = check_qos_compliance(policy, flow)
  assert_eq(compliance_result.flow.id, "flow-1")
  assert_true(compliance_result.compliant)  // Should be compliant with gold class
  assert_eq(compliance_result.violations.length(), 0)
  
  // Test non-compliant flow
  let non_compliant_flow = create_qos_flow("flow-2", "client-2", "server-1", "HTTP", "gold", 52428800, 100, 30, 0.2)
  let non_compliant_result = check_qos_compliance(policy, non_compliant_flow)
  
  assert_false(non_compliant_result.compliant)
  assert_true(non_compliant_result.violations.length() > 0)
  assert_true(non_compliant_result.violations.contains("bandwidth_exceeds_maximum"))
  assert_true(non_compliant_result.violations.contains("delay_exceeds_maximum"))
  assert_true(non_compliant_result.violations.contains("loss_rate_exceeds_maximum"))
  
  // Test QoS enforcement
  let enforce_qos = fn(policy: QoSPolicy, flow: QoSFlow) {
    let compliance_result = check_qos_compliance(policy, flow)
    
    if compliance_result.compliant {
      flow
    } else {
      // Apply remediation based on violations
      let mut remediated_flow = flow
      
      for violation in compliance_result.violations {
        match violation {
          "bandwidth_below_minimum" => {
            remediated_flow = { remediated_flow | 
              metrics: { bandwidth: policy.classes.find(fn(c) { c.id == flow.qos_class_id }).unwrap().min_bandwidth }
            }
          }
          "bandwidth_exceeds_maximum" => {
            remediated_flow = { remediated_flow | 
              metrics: { bandwidth: policy.classes.find(fn(c) { c.id == flow.qos_class_id }).unwrap().max_bandwidth }
            }
          }
          "delay_exceeds_maximum" => {
            remediated_flow = { remediated_flow | 
              metrics: { delay: policy.classes.find(fn(c) { c.id == flow.qos_class_id }).unwrap().max_delay }
            }
          }
          "jitter_exceeds_maximum" => {
            remediated_flow = { remediated_flow | 
              metrics: { jitter: policy.classes.find(fn(c) { c.id == flow.qos_class_id }).unwrap().max_jitter }
            }
          }
          "loss_rate_exceeds_maximum" => {
            remediated_flow = { remediated_flow | 
              metrics: { loss_rate: policy.classes.find(fn(c) { c.id == flow.qos_class_id }).unwrap().max_loss_rate }
            }
          }
          _ => {}
        }
      }
      
      remediated_flow
    }
  }
  
  // Test QoS enforcement
  let enforced_flow = enforce_qos(policy, non_compliant_flow)
  
  // Verify enforcement
  let enforced_compliance = check_qos_compliance(policy, enforced_flow)
  assert_true(enforced_compliance.compliant)
  assert_eq(enforced_compliance.violations.length(), 0)
  
  // Verify metrics were adjusted
  let gold_class = policy.classes.find(fn(c) { c.id == "gold" }).unwrap()
  assert_eq(enforced_flow.metrics.bandwidth, gold_class.max_bandwidth)
  assert_eq(enforced_flow.metrics.delay, gold_class.max_delay)
  assert_eq(enforced_flow.metrics.jitter, gold_class.max_jitter)
  assert_eq(enforced_flow.metrics.loss_rate, gold_class.max_loss_rate)
  
  // Test QoS class assignment
  let assign_qos_class = fn(policy: QoSPolicy, flow: QoSFlow) {
    // Simple assignment based on application requirements
    let assigned_class = match flow.protocol {
      "VoIP" => "gold",
      "video" => "silver",
      "HTTP" => "bronze",
      _ => policy.default_class
    }
    
    { flow | qos_class_id: assigned_class }
  }
  
  // Test QoS class assignment
  let voip_flow = create_qos_flow("voip-flow", "client-1", "server-1", "VoIP", "", 256000, 20, 5, 0.01)
  let assigned_voip_flow = assign_qos_class(policy, voip_flow)
  
  assert_eq(assigned_voip_flow.qos_class_id, "gold")
  
  let video_flow = create_qos_flow("video-flow", "client-1", "server-1", "video", "", 2097152, 50, 10, 0.1)
  let assigned_video_flow = assign_qos_class(policy, video_flow)
  
  assert_eq(assigned_video_flow.qos_class_id, "silver")
  
  let http_flow = create_qos_flow("http-flow", "client-1", "server-1", "HTTP", "", 1048576, 100, 20, 0.5)
  let assigned_http_flow = assign_qos_class(policy, http_flow)
  
  assert_eq(assigned_http_flow.qos_class_id, "bronze")
  
  // Test QoS statistics
  let calculate_qos_stats = fn(policy: QoSPolicy, flows: Array[QoSFlow>) {
    let mut class_stats = {}
    
    for flow in flows {
      let class_name = match policy.classes.find(fn(c) { c.id == flow.qos_class_id }) {
        Some(cls) => cls.name,
        None => "Unknown"
      }
      
      let stats = match class_stats[class_name] {
        Some(s) => s,
        None => {
          flow_count: 0,
          total_bandwidth: 0,
          avg_delay: 0.0,
          avg_jitter: 0.0,
          avg_loss_rate: 0.0,
          compliant_count: 0
        }
      }
      
      let compliance_result = check_qos_compliance(policy, flow)
      
      let updated_stats = {
        flow_count: stats.flow_count + 1,
        total_bandwidth: stats.total_bandwidth + flow.metrics.bandwidth,
        avg_delay: (stats.avg_delay * stats.flow_count.to_float() + flow.metrics.delay.to_float()) / (stats.flow_count + 1).to_float(),
        avg_jitter: (stats.avg_jitter * stats.flow_count.to_float() + flow.metrics.jitter.to_float()) / (stats.flow_count + 1).to_float(),
        avg_loss_rate: (stats.avg_loss_rate * stats.flow_count.to_float() + flow.metrics.loss_rate) / (stats.flow_count + 1).to_float(),
        compliant_count: stats.compliant_count + (if compliance_result.compliant { 1 } else { 0 })
      }
      
      class_stats = class_stats.insert(class_name, updated_stats)
    }
    
    class_stats
  }
  
  // Test QoS statistics
  let test_flows = [assigned_voip_flow, assigned_video_flow, assigned_http_flow]
  let qos_stats = calculate_qos_stats(policy, test_flows)
  
  assert_true(qos_stats.contains_key("Gold Class"))
  assert_true(qos_stats.contains_key("Silver Class"))
  assert_true(qos_stats.contains_key("Bronze Class"))
  
  let gold_stats = qos_stats["Gold Class"]
  assert_eq(gold_stats.flow_count, 1)
  assert_eq(gold_stats.total_bandwidth, 256000)
  assert_eq(gold_stats.compliant_count, 1)
  
  // Test QoS priority queueing
  let prioritize_flows = fn(flows: Array[QoSFlow>, policy: QoSPolicy) {
    let mut prioritized_flows = flows
    
    prioritized_flows.sort(fn(a, b) {
      let class_a = policy.classes.find(fn(c) { c.id == a.qos_class_id })
      let class_b = policy.classes.find(fn(c) { c.id == b.qos_class_id })
      
      match (class_a, class_b) {
        (Some(a), Some(b)) => {
          if a.priority != b.priority {
            if a.priority < b.priority { -1 } else { 1 }
          } else {
            0  // Same priority
          }
        }
        (Some(_), None) => -1,
        (None, Some(_)) => 1,
        (None, None) => 0
      }
    })
    
    prioritized_flows
  }
  
  // Test priority queueing
  let prioritized_flows = prioritize_flows(test_flows, policy)
  
  // Gold class should come first (priority 1)
  assert_eq(prioritized_flows[0].qos_class_id, "gold")
  
  // Silver class should come second (priority 2)
  assert_eq(prioritized_flows[1].qos_class_id, "silver")
  
  // Bronze class should come last (priority 3)
  assert_eq(prioritized_flows[2].qos_class_id, "bronze")
  
  // Test QoS resource allocation
  let allocate_qos_resources = fn(policy: QoSPolicy, total_bandwidth: Int) {
    let mut allocations = {}
    let remaining_bandwidth = total_bandwidth
    
    // Sort classes by priority
    let sorted_classes = policy.classes.sort(fn(a, b) { if a.priority < b.priority { -1 } else if a.priority > b.priority { 1 } else { 0 } })
    
    let mut allocated = 0
    
    for cls in sorted_classes {
      let class_allocation = if allocated + cls.min_bandwidth <= total_bandwidth {
        cls.min_bandwidth
      } else {
        0  // Not enough bandwidth for this class
      }
      
      allocations = allocations.insert(cls.id, class_allocation)
      allocated = allocated + class_allocation
    }
    
    // Distribute remaining bandwidth proportionally
    let remaining = total_bandwidth - allocated
    if remaining > 0 {
      let classes_with_allocation = allocations.filter(fn((_, alloc) { alloc > 0 })
      
      if classes_with_allocation.length() > 0 {
        let per_class = remaining / classes_with_allocation.length()
        
        let mut updated_allocations = {}
        
        for (class_id, allocation) in allocations {
          if allocation > 0 {
            updated_allocations = updated_allocations.insert(class_id, allocation + per_class)
          } else {
            updated_allocations = updated_allocations.insert(class_id, allocation)
          }
        }
        
        allocations = updated_allocations
      }
    }
    
    allocations
  }
  
  // Test resource allocation
  let allocations = allocate_qos_resources(policy, 20971520)  // 20MB total
  
  assert_true(allocations.contains_key("gold"))
  assert_true(allocations.contains_key("silver"))
  assert_true(allocations.contains_key("bronze"))
  
  // Total allocation should not exceed total bandwidth
  let total_allocated = allocations.reduce(fn(acc, (_, alloc)) { acc + alloc }, 0)
  assert_true(total_allocated <= 20971520)
  
  // Test QoS monitoring
  let monitor_qos_health = fn(policy: QoSPolicy, flows: Array[QoSFlow>) {
    let mut health_issues = []
    
    // Check overall compliance rate
    let compliant_flows = flows.filter(fn(f) {
      check_qos_compliance(policy, f).compliant
    }).length()
    
    let compliance_rate = if flows.length() > 0 {
      compliant_flows.to_float() / flows.length().to_float()
    } else {
      1.0
    }
    
    if compliance_rate < 0.9 {
      health_issues = health_issues.push("low_compliance_rate")
    }
    
    // Check class-specific issues
    let qos_stats = calculate_qos_stats(policy, flows)
    
    for (class_name, stats) in qos_stats {
      if stats.flow_count > 0 {
        let class_compliance_rate = stats.compliant_count.to_float() / stats.flow_count.to_float()
        
        if class_compliance_rate < 0.8 {
          health_issues = health_issues.push("class_compliance_issue:" + class_name)
        }
        
        if stats.avg_delay > 100 {
          health_issues = health_issues.push("high_delay:" + class_name)
        }
        
        if stats.avg_loss_rate > 1.0 {
          health_issues = health_issues.push("high_loss_rate:" + class_name)
        }
      }
    }
    
    {
      is_healthy: health_issues.length() == 0,
      compliance_rate,
      issues: health_issues
    }
  }
  
  // Test QoS health monitoring
  let health = monitor_qos_health(policy, test_flows)
  
  assert_true(health.is_healthy == true || health.is_healthy == false)  // Just verify it returns a boolean
  assert_true(health.compliance_rate >= 0.0 && health.compliance_rate <= 1.0)
  
  // Test QoS policy optimization
  let optimize_qos_policy = fn(policy: QoSPolicy, flows: Array[QoSFlow>) {
    let qos_stats = calculate_qos_stats(policy, flows)
    let mut optimized_classes = []
    
    for cls in policy.classes {
      let class_stats = match qos_stats[cls.name] {
        Some(stats) => stats,
        None => {
          flow_count: 0,
          total_bandwidth: 0,
          avg_delay: 0.0,
          avg_jitter: 0.0,
          avg_loss_rate: 0.0,
          compliant_count: 0
        }
      }
      
      // Optimize based on observed metrics
      let optimized_max_bandwidth = if class_stats.flow_count > 0 {
        let avg_bandwidth = class_stats.total_bandwidth / class_stats.flow_count
        let compliance_rate = class_stats.compliant_count.to_float() / class_stats.flow_count.to_float()
        
        if compliance_rate > 0.9 {
          // High compliance, can optimize by reducing bandwidth
          (avg_bandwidth * 1.2).to_int()  // 20% buffer
        } else {
          // Low compliance, need more bandwidth
          (avg_bandwidth * 1.5).to_int()  // 50% buffer
        }
      } else {
        cls.max_bandwidth
      }
      
      let optimized_max_delay = if class_stats.flow_count > 0 {
        if class_stats.avg_delay > cls.max_delay {
          (class_stats.avg_delay * 0.8).to_int()  // Reduce max delay
        } else {
          cls.max_delay
        }
      } else {
        cls.max_delay
      }
      
      let optimized_max_jitter = if class_stats.flow_count > 0 {
        if class_stats.avg_jitter > cls.max_jitter {
          (class_stats.avg_jitter * 0.8).to_int()  // Reduce max jitter
        } else {
          cls.max_jitter
        }
      } else {
        cls.max_jitter
      }
      
      let optimized_max_loss_rate = if class_stats.flow_count > 0 {
        if class_stats.avg_loss_rate > cls.max_loss_rate {
          cls.max_loss_rate * 0.9  // Reduce max loss rate
        } else {
          cls.max_loss_rate
        }
      } else {
        cls.max_loss_rate
      }
      
      optimized_classes = optimized_classes.push({
        id: cls.id,
        name: cls.name,
        priority: cls.priority,
        min_bandwidth: cls.min_bandwidth,
        max_bandwidth: optimized_max_bandwidth,
        max_delay: optimized_max_delay,
        max_jitter: optimized_max_jitter,
        max_loss_rate: optimized_max_loss_rate
      })
    }
    
    {
      name: policy.name + " (Optimized)",
      classes: optimized_classes,
      default_class: policy.default_class
    }
  }
  
  // Test QoS policy optimization
  let optimized_policy = optimize_qos_policy(policy, test_flows)
  
  assert_eq(optimized_policy.name, "Standard Policy (Optimized)")
  assert_eq(optimized_policy.classes.length(), policy.classes.length())
  assert_eq(optimized_policy.default_class, policy.default_class)
}