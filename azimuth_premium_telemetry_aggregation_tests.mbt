// Azimuth Premium Test Suite - Telemetry Data Aggregation
// This file contains comprehensive test cases for telemetry data aggregation, metrics collection, and analytics

// Test 1: Metrics Collection and Aggregation
test "metrics_collection_and_aggregation" {
  // Test metrics collector
  let metrics_collector = || {
    let mut metrics = {}
    let mut timestamps = []
    
    let record_metric = |name, value, tags| {
      let timestamp = 1640995200000L + timestamps.length().to_long()
      timestamps.push(timestamp)
      
      if !metrics.contains(name) {
        metrics[name] = []
      }
      
      metrics[name].push({
        value: value,
        timestamp: timestamp,
        tags: tags
      })
    }
    
    let get_metrics = |name| {
      if metrics.contains(name) {
        metrics[name]
      } else {
        []
      }
    }
    
    let aggregate_metrics = |name, aggregation_type, start_time, end_time| {
      let metric_data = get_metrics(name)
      let mut filtered = []
      
      for metric in metric_data {
        if metric.timestamp >= start_time && metric.timestamp <= end_time {
          filtered.push(metric)
        }
      }
      
      if filtered.length() == 0 {
        return null
      }
      
      match aggregation_type {
        "sum" => {
          let sum = filtered.reduce(|acc, metric| acc + metric.value, 0)
          sum
        }
        "avg" => {
          let sum = filtered.reduce(|acc, metric| acc + metric.value, 0)
          sum / filtered.length()
        }
        "min" => {
          let mut min = filtered[0].value
          for metric in filtered {
            if metric.value < min {
              min = metric.value
            }
          }
          min
        }
        "max" => {
          let mut max = filtered[0].value
          for metric in filtered {
            if metric.value > max {
              max = metric.value
            }
          }
          max
        }
        "count" => {
          filtered.length()
        }
        _ => {
          null
        }
      }
    }
    
    let get_metrics_summary = |name| {
      let metric_data = get_metrics(name)
      
      if metric_data.length() == 0 {
        return {
          count: 0,
          sum: 0,
          avg: 0,
          min: 0,
          max: 0
        }
      }
      
      let sum = metric_data.reduce(|acc, metric| acc + metric.value, 0)
      let avg = sum / metric_data.length()
      
      let mut min = metric_data[0].value
      let mut max = metric_data[0].value
      
      for metric in metric_data {
        if metric.value < min {
          min = metric.value
        }
        if metric.value > max {
          max = metric.value
        }
      }
      
      {
        count: metric_data.length(),
        sum: sum,
        avg: avg,
        min: min,
        max: max
      }
    }
    
    (record_metric, get_metrics, aggregate_metrics, get_metrics_summary)
  }
  
  let (record_metric, get_metrics, aggregate, get_summary) = metrics_collector()
  
  // Test metrics collection
  let base_time = 1640995200000L
  
  record_metric("cpu_usage", 25.5, { "host": "server1" })
  record_metric("cpu_usage", 30.2, { "host": "server1" })
  record_metric("cpu_usage", 45.8, { "host": "server1" })
  
  record_metric("memory_usage", 1024, { "host": "server1" })
  record_metric("memory_usage", 1080, { "host": "server1" })
  record_metric("memory_usage", 1152, { "host": "server1" })
  
  // Test metrics retrieval
  let cpu_metrics = get_metrics("cpu_usage")
  assert_eq(cpu_metrics.length(), 3)
  assert_eq(cpu_metrics[0].value, 25.5)
  assert_eq(cpu_metrics[2].value, 45.8)
  
  // Test metrics aggregation
  let cpu_sum = aggregate("cpu_usage", "sum", base_time, base_time + 2000L)
  assert_eq(cpu_sum, 101.5)  // 25.5 + 30.2 + 45.8
  
  let cpu_avg = aggregate("cpu_usage", "avg", base_time, base_time + 2000L)
  assert_true(cpu_avg > 33.0 && cpu_avg < 34.0)  // (25.5 + 30.2 + 45.8) / 3
  
  let cpu_min = aggregate("cpu_usage", "min", base_time, base_time + 2000L)
  assert_eq(cpu_min, 25.5)
  
  let cpu_max = aggregate("cpu_usage", "max", base_time, base_time + 2000L)
  assert_eq(cpu_max, 45.8)
  
  let cpu_count = aggregate("cpu_usage", "count", base_time, base_time + 2000L)
  assert_eq(cpu_count, 3)
  
  // Test metrics summary
  let memory_summary = get_summary("memory_usage")
  assert_eq(memory_summary.count, 3)
  assert_eq(memory_summary.sum, 3256)
  assert_eq(memory_summary.avg, 1085.3333333333333)
  assert_eq(memory_summary.min, 1024)
  assert_eq(memory_summary.max, 1152)
  
  // Test histogram metrics
  let histogram_collector = || {
    let mut histograms = {}
    
    let record_histogram = |name, value, buckets| {
      if !histograms.contains(name) {
        histograms[name] = {
          buckets: buckets,
          counts: []
          
          // Initialize counts for each bucket
          for bucket in buckets {
            counts.push(0)
          }
        }
      }
      
      let histogram = histograms[name]
      
      // Find the appropriate bucket
      let mut bucket_index = 0
      for i = 0; i < histogram.buckets.length(); i = i + 1 {
        if value <= histogram.buckets[i] {
          bucket_index = i
          break
        }
        bucket_index = histogram.buckets.length()  // Overflow bucket
      }
      
      histogram.counts[bucket_index] = histogram.counts[bucket_index] + 1
    }
    
    let get_histogram = |name| {
      if histograms.contains(name) {
        Some(histograms[name])
      } else {
        None
      }
    }
    
    let calculate_percentile = |name, percentile| {
      if histograms.contains(name) {
        let histogram = histograms[name]
        let total_count = histogram.counts.reduce(|acc, count| acc + count, 0)
        
        if total_count == 0 {
          return 0
        }
        
        let target_count = (total_count * percentile) / 100
        
        let mut cumulative_count = 0
        for i = 0; i < histogram.counts.length(); i = i + 1 {
          cumulative_count = cumulative_count + histogram.counts[i]
          
          if cumulative_count >= target_count {
            return histogram.buckets[i]
          }
        }
        
        return histogram.buckets[histogram.buckets.length() - 1]
      }
      
      0
    }
    
    (record_histogram, get_histogram, calculate_percentile)
  }
  
  let (record_histogram, get_histogram, calculate_percentile) = histogram_collector()
  
  // Test histogram metrics
  let response_time_buckets = [10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000]
  
  record_histogram("response_time", 15, response_time_buckets)  // 10ms bucket
  record_histogram("response_time", 35, response_time_buckets)  // 50ms bucket
  record_histogram("response_time", 75, response_time_buckets)  // 100ms bucket
  record_histogram("response_time", 150, response_time_buckets) // 250ms bucket
  record_histogram("response_time", 350, response_time_buckets) // 500ms bucket
  record_histogram("response_time", 750, response_time_buckets) // 1000ms bucket
  record_histogram("response_time", 1500, response_time_buckets) // 2500ms bucket
  record_histogram("response_time", 3500, response_time_buckets) // 5000ms bucket
  record_histogram("response_time", 7500, response_time_buckets) // 10000ms bucket
  record_histogram("response_time", 15000, response_time_buckets) // Overflow bucket
  
  let histogram = get_histogram("response_time")
  match histogram {
    Some(h) => {
      assert_eq(h.buckets, response_time_buckets)
      assert_eq(h.counts[0], 1)  // 10ms bucket
      assert_eq(h.counts[1], 0)  // 25ms bucket
      assert_eq(h.counts[2], 1)  // 50ms bucket
      assert_eq(h.counts[3], 1)  // 100ms bucket
      assert_eq(h.counts[4], 1)  // 250ms bucket
      assert_eq(h.counts[5], 1)  // 500ms bucket
      assert_eq(h.counts[6], 1)  // 1000ms bucket
      assert_eq(h.counts[7], 1)  // 2500ms bucket
      assert_eq(h.counts[8], 1)  // 5000ms bucket
      assert_eq(h.counts[9], 1)  // 10000ms bucket
      assert_eq(h.counts[10], 1) // Overflow bucket
    }
    None => assert_true(false)
  }
  
  // Test percentile calculation
  let p50 = calculate_percentile("response_time", 50)
  let p95 = calculate_percentile("response_time", 95)
  let p99 = calculate_percentile("response_time", 99)
  
  // With 10 data points, P50 should be around 250ms
  assert_eq(p50, 250)
  // P95 should be around 5000ms
  assert_eq(p95, 5000)
  // P99 should be around 10000ms
  assert_eq(p99, 10000)
}

// Test 2: Time Series Data Processing
test "time_series_data_processing" {
  // Test time series processor
  let time_series_processor = || {
    let mut series = {}
    
    let add_point = |series_name, timestamp, value| {
      if !series.contains(series_name) {
        series[series_name] = []
      }
      
      series[series_name].push({
        timestamp: timestamp,
        value: value
      })
      
      // Sort by timestamp
      series[series_name].sort_by(|a, b| a.timestamp - b.timestamp)
    }
    
    let get_series = |series_name| {
      if series.contains(series_name) {
        series[series_name]
      } else {
        []
      }
    }
    
    let resample = |series_name, interval| {
      let data = get_series(series_name)
      
      if data.length() == 0 {
        return []
      }
      
      let mut resampled = []
      let start_time = data[0].timestamp
      let end_time = data[data.length() - 1].timestamp
      
      let mut current_time = start_time
      
      while current_time <= end_time {
        let next_time = current_time + interval
        let mut window_values = []
        
        for point in data {
          if point.timestamp >= current_time && point.timestamp < next_time {
            window_values.push(point.value)
          }
        }
        
        if window_values.length() > 0 {
          let sum = window_values.reduce(|acc, value| acc + value, 0)
          let avg = sum / window_values.length()
          
          resampled.push({
            timestamp: current_time,
            value: avg
          })
        }
        
        current_time = next_time
      }
      
      resampled
    }
    
    let moving_average = |series_name, window_size| {
      let data = get_series(series_name)
      
      if data.length() < window_size {
        return []
      }
      
      let mut result = []
      
      for i = window_size - 1; i < data.length(); i = i + 1 {
        let mut sum = 0
        
        for j = i - window_size + 1; j <= i; j = j + 1 {
          sum = sum + data[j].value
        }
        
        result.push({
          timestamp: data[i].timestamp,
          value: sum / window_size
        })
      }
      
      result
    }
    
    let detect_anomalies = |series_name, threshold| {
      let data = get_series(series_name)
      let mut anomalies = []
      
      if data.length() < 3 {
        return anomalies
      }
      
      // Simple anomaly detection based on deviation from neighbors
      for i = 1; i < data.length() - 1; i = i + 1 {
        let prev_value = data[i - 1].value
        let current_value = data[i].value
        let next_value = data[i + 1].value
        
        let avg_neighbors = (prev_value + next_value) / 2
        let deviation = current_value - avg_neighbors
        
        if deviation.abs() > threshold {
          anomalies.push({
            timestamp: data[i].timestamp,
            value: current_value,
            deviation: deviation
          })
        }
      }
      
      anomalies
    }
    
    (add_point, get_series, resample, moving_average, detect_anomalies)
  }
  
  let (add_point, get_series, resample, moving_average, detect_anomalies) = time_series_processor()
  
  // Test time series processing
  let base_time = 1640995200000L  // 2022-01-01 00:00:00 UTC
  
  // Add data points every minute
  for i = 0; i < 10; i = i + 1 {
    let timestamp = base_time + (i * 60000L)  // i minutes
    let value = 10 + i * 2 + (i % 3)  // Some variation
    add_point("cpu_usage", timestamp, value)
  }
  
  // Test series retrieval
  let cpu_series = get_series("cpu_usage")
  assert_eq(cpu_series.length(), 10)
  assert_eq(cpu_series[0].timestamp, base_time)
  assert_eq(cpu_series[0].value, 10)
  assert_eq(cpu_series[9].timestamp, base_time + 540000L)  // 9 minutes
  assert_eq(cpu_series[9].value, 28)
  
  // Test resampling
  let resampled = resample("cpu_usage", 180000L)  // 3 minutes
  assert_eq(resampled.length(), 4)  // 10 minutes / 3 minutes = 3.33, rounded up to 4
  
  assert_eq(resampled[0].timestamp, base_time)
  assert_eq(resampled[0].value, (10 + 12 + 11) / 3)  // Average of first 3 points
  
  // Test moving average
  let ma3 = moving_average("cpu_usage", 3)
  assert_eq(ma3.length(), 8)  // 10 - 3 + 1 = 8
  
  assert_eq(ma3[0].value, (10 + 12 + 11) / 3)
  assert_eq(ma3[1].value, (12 + 11 + 14) / 3)
  
  // Test anomaly detection
  // Add some anomalous points
  add_point("cpu_usage", base_time + 600000L, 100)  // 10 minutes, very high value
  add_point("cpu_usage", base_time + 660000L, 5)    // 11 minutes, very low value
  
  let anomalies = detect_anomalies("cpu_usage", 20)
  assert_eq(anomalies.length(), 2)
  assert_eq(anomalies[0].timestamp, base_time + 600000L)
  assert_eq(anomalies[0].value, 100)
  assert_eq(anomalies[1].timestamp, base_time + 660000L)
  assert_eq(anomalies[1].value, 5)
  
  // Test trend analysis
  let trend_analysis = |series_name| {
    let data = get_series(series_name)
    
    if data.length() < 2 {
      return {
        trend: "insufficient_data",
        slope: 0,
        correlation: 0
      }
    }
    
    // Simple linear regression
    let n = data.length()
    let mut sum_x = 0
    let mut sum_y = 0
    let mut sum_xy = 0
    let mut sum_x2 = 0
    let mut sum_y2 = 0
    
    for i = 0; i < data.length(); i = i + 1 {
      let x = i.to_float()
      let y = data[i].value
      
      sum_x = sum_x + x
      sum_y = sum_y + y
      sum_xy = sum_xy + x * y
      sum_x2 = sum_x2 + x * x
      sum_y2 = sum_y2 + y * y
    }
    
    let slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x)
    let correlation = (n * sum_xy - sum_x * sum_y) / 
                      ((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt()
    
    let trend = if slope > 0.5 {
      "increasing"
    } else if slope < -0.5 {
      "decreasing"
    } else {
      "stable"
    }
    
    {
      trend: trend,
      slope: slope,
      correlation: correlation
    }
  }
  
  let trend = trend_analysis("cpu_usage")
  assert_eq(trend.trend, "increasing")
  assert_true(trend.slope > 1.5)
  assert_true(trend.correlation > 0.9)
  
  // Test seasonal decomposition
  let seasonal_decomposition = |series_name, period| {
    let data = get_series(series_name)
    
    if data.length() < period * 2 {
      return {
        trend: [],
        seasonal: [],
        residual: []
      }
    }
    
    // Simple seasonal decomposition
    let mut trend = []
    let mut seasonal = []
    let mut residual = []
    
    // Calculate trend using moving average
    for i = 0; i < data.length(); i = i + 1 {
      let start = if i < period / 2 { 0 } else { i - period / 2 }
      let end = if i + period / 2 >= data.length() { data.length() - 1 } else { i + period / 2 }
      
      let mut sum = 0
      let mut count = 0
      
      for j = start; j <= end; j = j + 1 {
        sum = sum + data[j].value
        count = count + 1
      }
      
      trend.push({
        timestamp: data[i].timestamp,
        value: sum / count
      })
    }
    
    // Calculate seasonal component
    for i = 0; i < period; i = i + 1 {
      let mut seasonal_sum = 0
      let mut seasonal_count = 0
      
      for j = i; j < data.length(); j = j + period {
        seasonal_sum = seasonal_sum + data[j].value
        seasonal_count = seasonal_count + 1
      }
      
      if seasonal_count > 0 {
        seasonal.push({
          period_index: i,
          value: seasonal_sum / seasonal_count
        })
      }
    }
    
    // Calculate residual
    for i = 0; i < data.length(); i = i + 1 {
      let seasonal_index = i % period
      let seasonal_value = if seasonal_index < seasonal.length() {
        seasonal[seasonal_index].value
      } else {
        0
      }
      
      residual.push({
        timestamp: data[i].timestamp,
        value: data[i].value - trend[i].value - seasonal_value
      })
    }
    
    {
      trend: trend,
      seasonal: seasonal,
      residual: residual
    }
  }
  
  // Add seasonal data
  for i = 0; i < 24; i = i + 1 {
    let timestamp = base_time + (i * 3600000L)  // Hourly data
    let value = 10 + (i % 4) * 5  // Seasonal pattern with period 4
    add_point("hourly_pattern", timestamp, value)
  }
  
  let decomposition = seasonal_decomposition("hourly_pattern", 4)
  assert_eq(decomposition.trend.length(), 24)
  assert_eq(decomposition.seasonal.length(), 4)
  assert_eq(decomposition.residual.length(), 24)
}

// Test 3: Distributed Tracing Data Aggregation
test "distributed_tracing_data_aggregation" {
  // Test trace aggregator
  let trace_aggregator = || {
    let mut traces = {}
    let mut spans = {}
    
    let add_span = |trace_id, span_id, parent_span_id, operation_name, start_time, end_time, tags| {
      let span = {
        trace_id: trace_id,
        span_id: span_id,
        parent_span_id: parent_span_id,
        operation_name: operation_name,
        start_time: start_time,
        end_time: end_time,
        duration: end_time - start_time,
        tags: tags
      }
      
      if !spans.contains(trace_id) {
        spans[trace_id] = []
      }
      
      spans[trace_id].push(span)
      
      // Sort by start time
      spans[trace_id].sort_by(|a, b| a.start_time - b.start_time)
    }
    
    let get_trace = |trace_id| {
      if spans.contains(trace_id) {
        spans[trace_id]
      } else {
        []
      }
    }
    
    let calculate_trace_duration = |trace_id| {
      let trace_spans = get_trace(trace_id)
      
      if trace_spans.length() == 0 {
        return 0
      }
      
      let mut min_time = trace_spans[0].start_time
      let mut max_time = trace_spans[0].end_time
      
      for span in trace_spans {
        if span.start_time < min_time {
          min_time = span.start_time
        }
        if span.end_time > max_time {
          max_time = span.end_time
        }
      }
      
      max_time - min_time
    }
    
    let get_span_tree = |trace_id| {
      let trace_spans = get_trace(trace_id)
      let mut span_map = {}
      
      for span in trace_spans {
        span_map[span.span_id] = {
          span: span,
          children: []
        }
      }
      
      // Build tree structure
      let mut root_spans = []
      
      for span in trace_spans {
        if span.parent_span_id == null || span.parent_span_id == "" {
          root_spans.push(span.span_id)
        } else if span_map.contains(span.parent_span_id) {
          span_map[span.parent_span_id].children.push(span.span_id)
        }
      }
      
      {
        span_map: span_map,
        root_spans: root_spans
      }
    }
    
    let calculate_critical_path = |trace_id| {
      let span_tree = get_span_tree(trace_id)
      let span_map = span_tree.span_map
      
      let calculate_end_time = |span_id| {
        if span_map.contains(span_id) {
          let span = span_map[span_id].span
          let mut max_child_end = span.end_time
          
          for child_id in span_map[span_id].children {
            let child_end = calculate_end_time(child_id)
            if child_end > max_child_end {
              max_child_end = child_end
            }
          }
          
          max_child_end
        } else {
          0
        }
      }
      
      let mut critical_path = []
      let mut max_duration = 0
      
      for root_id in span_tree.root_spans {
        let end_time = calculate_end_time(root_id)
        let duration = end_time - span_map[root_id].span.start_time
        
        if duration > max_duration {
          max_duration = duration
          critical_path = [root_id]
        }
      }
      
      {
        duration: max_duration,
        path: critical_path
      }
    }
    
    let get_operation_stats = |operation_name| {
      let mut matching_spans = []
      
      for trace_id in spans {
        for span in spans[trace_id] {
          if span.operation_name == operation_name {
            matching_spans.push(span)
          }
        }
      }
      
      if matching_spans.length() == 0 {
        return {
          count: 0,
          avg_duration: 0,
          min_duration: 0,
          max_duration: 0
        }
      }
      
      let total_duration = matching_spans.reduce(|acc, span| acc + span.duration, 0)
      let avg_duration = total_duration / matching_spans.length()
      
      let mut min_duration = matching_spans[0].duration
      let mut max_duration = matching_spans[0].duration
      
      for span in matching_spans {
        if span.duration < min_duration {
          min_duration = span.duration
        }
        if span.duration > max_duration {
          max_duration = span.duration
        }
      }
      
      {
        count: matching_spans.length(),
        avg_duration: avg_duration,
        min_duration: min_duration,
        max_duration: max_duration
      }
    }
    
    (add_span, get_trace, calculate_trace_duration, get_span_tree, calculate_critical_path, get_operation_stats)
  }
  
  let (add_span, get_trace, calc_duration, get_tree, calc_critical_path, get_op_stats) = trace_aggregator()
  
  // Test distributed tracing aggregation
  let trace_id = "trace-12345"
  let base_time = 1640995200000L
  
  // Add spans for a distributed trace
  add_span(trace_id, "span-1", "", "gateway", base_time, base_time + 100, { "service": "gateway" })
  add_span(trace_id, "span-2", "span-1", "auth", base_time + 50, base_time + 200, { "service": "auth" })
  add_span(trace_id, "span-3", "span-1", "user", base_time + 100, base_time + 300, { "service": "user" })
  add_span(trace_id, "span-4", "span-2", "database", base_time + 150, base_time + 180, { "service": "database" })
  add_span(trace_id, "span-5", "span-3", "cache", base_time + 200, base_time + 250, { "service": "cache" })
  
  // Test trace retrieval
  let trace = get_trace(trace_id)
  assert_eq(trace.length(), 5)
  
  // Test trace duration calculation
  let duration = calc_duration(trace_id)
  assert_eq(duration, 300)  // From 0 to 300ms
  
  // Test span tree
  let span_tree = get_tree(trace_id)
  assert_eq(span_tree.root_spans.length(), 1)
  assert_eq(span_tree.root_spans[0], "span-1")
  
  // Check children
  assert_eq(span_tree.span_map["span-1"].children.length(), 2)
  assert_true(span_tree.span_map["span-1"].children.contains("span-2"))
  assert_true(span_tree.span_map["span-1"].children.contains("span-3"))
  
  assert_eq(span_tree.span_map["span-2"].children.length(), 1)
  assert_eq(span_tree.span_map["span-2"].children[0], "span-4")
  
  assert_eq(span_tree.span_map["span-3"].children.length(), 1)
  assert_eq(span_tree.span_map["span-3"].children[0], "span-5")
  
  // Test critical path calculation
  let critical_path = calc_critical_path(trace_id)
  assert_eq(critical_path.duration, 300)  // Total trace duration
  
  // Test operation stats
  let db_stats = get_op_stats("database")
  assert_eq(db_stats.count, 1)
  assert_eq(db_stats.avg_duration, 30)  // 180 - 150
  assert_eq(db_stats.min_duration, 30)
  assert_eq(db_stats.max_duration, 30)
  
  // Test trace sampling
  let trace_sampler = || {
    let mut sample_rate = 1.0
    let mut sampled_count = 0
    let mut total_count = 0
    
    let set_sample_rate = |rate| {
      sample_rate = rate
    }
    
    let should_sample = |trace_id| {
      total_count = total_count + 1
      
      // Simple deterministic sampling based on trace ID hash
      let hash = trace_id.reduce(|acc, char| acc + char.to_int(), 0)
      let normalized = (hash % 100).to_float() / 100.0
      
      if normalized < sample_rate {
        sampled_count = sampled_count + 1
        return true
      }
      
      false
    }
    
    let get_sampling_stats = || {
      {
        sample_rate: sample_rate,
        total_count: total_count,
        sampled_count: sampled_count,
        actual_rate: if total_count > 0 { sampled_count.to_float() / total_count.to_float() } else { 0.0 }
      }
    }
    
    (set_sample_rate, should_sample, get_sampling_stats)
  }
  
  let (set_rate, should_sample, get_stats) = trace_sampler()
  
  // Test trace sampling
  set_rate(0.5)  // 50% sampling rate
  
  let sampled1 = should_sample("trace-1")
  let sampled2 = should_sample("trace-2")
  let sampled3 = should_sample("trace-3")
  let sampled4 = should_sample("trace-4")
  
  let stats = get_stats()
  assert_eq(stats.total_count, 4)
  assert_eq(stats.sample_rate, 0.5)
  
  // Test trace aggregation by service
  let service_aggregator = || {
    let mut service_metrics = {}
    
    let add_span = |service, operation, duration| {
      if !service_metrics.contains(service) {
        service_metrics[service] = {
          operation_counts: {},
          total_duration: 0,
          span_count: 0
        }
      }
      
      let metrics = service_metrics[service]
      
      if !metrics.operation_counts.contains(operation) {
        metrics.operation_counts[operation] = 0
      }
      
      metrics.operation_counts[operation] = metrics.operation_counts[operation] + 1
      metrics.total_duration = metrics.total_duration + duration
      metrics.span_count = metrics.span_count + 1
    }
    
    let get_service_stats = |service| {
      if service_metrics.contains(service) {
        let metrics = service_metrics[service]
        let avg_duration = if metrics.span_count > 0 {
          metrics.total_duration / metrics.span_count
        } else {
          0
        }
        
        {
          service: service,
          operation_counts: metrics.operation_counts,
          span_count: metrics.span_count,
          total_duration: metrics.total_duration,
          avg_duration: avg_duration
        }
      } else {
        null
      }
    }
    
    let get_all_service_stats = || {
      let mut all_stats = []
      
      for service in service_metrics {
        all_stats.push(get_service_stats(service))
      }
      
      all_stats
    }
    
    (add_span, get_service_stats, get_all_service_stats)
  }
  
  let (add_span, get_service_stats, get_all_stats) = service_aggregator()
  
  // Test service aggregation
  add_span("gateway", "request", 100)
  add_span("gateway", "request", 120)
  add_span("gateway", "response", 80)
  
  add_span("auth", "authenticate", 150)
  add_span("auth", "authorize", 100)
  
  add_span("database", "query", 200)
  add_span("database", "query", 180)
  add_span("database", "update", 250)
  
  let gateway_stats = get_service_stats("gateway")
  match gateway_stats {
    Some(stats) => {
      assert_eq(stats.service, "gateway")
      assert_eq(stats.span_count, 3)
      assert_eq(stats.total_duration, 300)
      assert_eq(stats.avg_duration, 100)
      assert_eq(stats.operation_counts["request"], 2)
      assert_eq(stats.operation_counts["response"], 1)
    }
    None => assert_true(false)
  }
  
  let all_stats = get_all_stats()
  assert_eq(all_stats.length(), 3)
}

// Test 4: Log Aggregation and Analysis
test "log_aggregation_and_analysis" {
  // Test log aggregator
  let log_aggregator = || {
    let mut logs = []
    let mut log_levels = {}
    
    let add_log = |timestamp, level, message, source, tags| {
      let log = {
        timestamp: timestamp,
        level: level,
        message: message,
        source: source,
        tags: tags
      }
      
      logs.push(log)
      
      // Update level counts
      if !log_levels.contains(level) {
        log_levels[level] = 0
      }
      log_levels[level] = log_levels[level] + 1
      
      // Sort by timestamp
      logs.sort_by(|a, b| a.timestamp - b.timestamp)
    }
    
    let get_logs = |level, start_time, end_time| {
      let mut filtered = []
      
      for log in logs {
        if (level == null || log.level == level) &&
           log.timestamp >= start_time &&
           log.timestamp <= end_time {
          filtered.push(log)
        }
      }
      
      filtered
    }
    
    let search_logs = |query, start_time, end_time| {
      let mut matching_logs = []
      
      for log in logs {
        if log.timestamp >= start_time &&
           log.timestamp <= end_time &&
           log.message.contains(query) {
          matching_logs.push(log)
        }
      }
      
      matching_logs
    }
    
    let get_log_levels = || {
      log_levels
    }
    
    let get_error_rate = |start_time, end_time| {
      let total_logs = get_logs(null, start_time, end_time).length()
      let error_logs = get_logs("error", start_time, end_time).length()
      
      if total_logs > 0 {
        error_logs.to_float() / total_logs.to_float()
      } else {
        0.0
      }
    }
    
    let get_log_sources = || {
      let mut sources = {}
      
      for log in logs {
        if !sources.contains(log.source) {
          sources[log.source] = 0
        }
        sources[log.source] = sources[log.source] + 1
      }
      
      sources
    }
    
    (add_log, get_logs, search_logs, get_log_levels, get_error_rate, get_log_sources)
  }
  
  let (add_log, get_logs, search_logs, get_levels, get_error_rate, get_sources) = log_aggregator()
  
  // Test log aggregation
  let base_time = 1640995200000L
  
  add_log(base_time, "info", "Application started", "app", {})
  add_log(base_time + 1000L, "info", "Database connected", "db", {})
  add_log(base_time + 2000L, "warn", "High memory usage detected", "app", { "memory": "85%" })
  add_log(base_time + 3000L, "error", "Database connection failed", "db", { "error": "timeout" })
  add_log(base_time + 4000L, "error", "Failed to process request", "app", { "request_id": "req-123" })
  add_log(base_time + 5000L, "info", "Retrying database connection", "db", {})
  add_log(base_time + 6000L, "info", "Database connected", "db", {})
  
  // Test log retrieval
  let all_logs = get_logs(null, base_time, base_time + 6000L)
  assert_eq(all_logs.length(), 7)
  
  let error_logs = get_logs("error", base_time, base_time + 6000L)
  assert_eq(error_logs.length(), 2)
  
  let db_logs = get_logs(null, base_time + 500L, base_time + 6500L)
  assert_eq(db_logs.length(), 5)  // All logs except the first
  
  // Test log search
  let search_results = search_logs("database", base_time, base_time + 6000L)
  assert_eq(search_results.length(), 3)
  
  let error_search = search_logs("failed", base_time, base_time + 6000L)
  assert_eq(error_search.length(), 1)
  assert_eq(error_search[0].level, "error")
  
  // Test log levels
  let levels = get_levels()
  assert_eq(levels["info"], 4)
  assert_eq(levels["warn"], 1)
  assert_eq(levels["error"], 2)
  
  // Test error rate
  let error_rate = get_error_rate(base_time, base_time + 6000L)
  assert_eq(error_rate, 2.0 / 7.0)  // 2 errors out of 7 total logs
  
  // Test log sources
  let sources = get_sources()
  assert_eq(sources["app"], 3)
  assert_eq(sources["db"], 4)
  
  // Test log pattern detection
  let pattern_detector = || {
    let mut patterns = {}
    
    let detect_pattern = |log_message| {
      // Simple pattern detection based on common log patterns
      if log_message.contains("error") {
        return "error_pattern"
      } else if log_message.contains("started") {
        return "startup_pattern"
      } else if log_message.contains("connected") {
        return "connection_pattern"
      } else if log_message.contains("timeout") {
        return "timeout_pattern"
      } else {
        return "unknown_pattern"
      }
    }
    
    let analyze_logs = |logs| {
      let mut pattern_counts = {}
      
      for log in logs {
        let pattern = detect_pattern(log.message)
        
        if !pattern_counts.contains(pattern) {
          pattern_counts[pattern] = 0
        }
        
        pattern_counts[pattern] = pattern_counts[pattern] + 1
      }
      
      pattern_counts
    }
    
    let get_top_patterns = |pattern_counts, count| {
      let mut sorted_patterns = []
      
      for pattern in pattern_counts {
        sorted_patterns.push({
          pattern: pattern,
          count: pattern_counts[pattern]
        })
      }
      
      // Sort by count (descending)
      sorted_patterns.sort_by(|a, b| b.count - a.count)
      
      // Return top N
      if sorted_patterns.length() > count {
        sorted_patterns.slice(0, count)
      } else {
        sorted_patterns
      }
    }
    
    (detect_pattern, analyze_logs, get_top_patterns)
  }
  
  let (detect_pattern, analyze_logs, get_top_patterns) = pattern_detector()
  
  // Test pattern detection
  assert_eq(detect_pattern("Database connection failed"), "error_pattern")
  assert_eq(detect_pattern("Application started"), "startup_pattern")
  assert_eq(detect_pattern("Database connected"), "connection_pattern")
  assert_eq(detect_pattern("Connection timeout"), "timeout_pattern")
  
  // Test log analysis
  let all_logs = get_logs(null, base_time, base_time + 6000L)
  let pattern_counts = analyze_logs(all_logs)
  
  assert_eq(pattern_counts["error_pattern"], 2)
  assert_eq(pattern_counts["startup_pattern"], 1)
  assert_eq(pattern_counts["connection_pattern"], 3)
  assert_eq(pattern_counts["timeout_pattern"], 1)
  
  // Test top patterns
  let top_patterns = get_top_patterns(pattern_counts, 3)
  assert_eq(top_patterns.length(), 3)
  assert_eq(top_patterns[0].pattern, "connection_pattern")
  assert_eq(top_patterns[0].count, 3)
  
  // Test log correlation
  let log_correlator = || {
    let mut correlations = {}
    
    let correlate_logs = |logs| {
      let mut correlated = {}
      
      for log in logs {
        // Extract correlation keys from tags
        let correlation_keys = []
        
        for tag in log.tags {
          correlation_keys.push(tag)
        }
        
        // Also check message for common correlation patterns
        if log.message.contains("request_id") {
          // Extract request_id from message (simplified)
          correlation_keys.push("request_id")
        }
        
        for key in correlation_keys {
          if !correlated.contains(key) {
            correlated[key] = []
          }
          
          correlated[key].push(log)
        }
      }
      
      correlated
    }
    
    let find_error_sequences = |logs| {
      let mut sequences = []
      
      for i = 0; i < logs.length(); i = i + 1 {
        if logs[i].level == "error" {
          // Look for preceding warning or info logs that might be related
          let mut sequence = []
          
          for j = i - 3; j < i && j >= 0; j = j + 1 {
            if logs[j].level == "warn" || logs[j].level == "info" {
              sequence.push(logs[j])
            }
          }
          
          sequence.reverse()  // Put in chronological order
          sequence.push(logs[i])
          
          if sequence.length() > 1 {
            sequences.push(sequence)
          }
        }
      }
      
      sequences
    }
    
    (correlate_logs, find_error_sequences)
  }
  
  let (correlate_logs, find_error_sequences) = log_correlator()
  
  // Test log correlation
  let correlated = correlate_logs(all_logs)
  assert_true(correlated.contains("error"))
  assert_eq(correlated["error"].length(), 2)
  assert_true(correlated.contains("memory"))
  assert_eq(correlated["memory"].length(), 1)
  
  // Test error sequence detection
  let error_sequences = find_error_sequences(all_logs)
  assert_eq(error_sequences.length(), 2)
  
  // First sequence should lead to "Database connection failed"
  assert_eq(error_sequences[0][error_sequences[0].length() - 1].message, "Database connection failed")
  
  // Test log aggregation by time window
  let time_window_aggregator = || {
    let mut time_windows = {}
    
    let aggregate_by_window = |logs, window_size| {
      let mut windows = {}
      
      for log in logs {
        let window_start = (log.timestamp / window_size) * window_size
        
        if !windows.contains(window_start) {
          windows[window_start] = {
            start_time: window_start,
            end_time: window_start + window_size,
            logs: [],
            level_counts: {}
          }
        }
        
        let window = windows[window_start]
        window.logs.push(log)
        
        if !window.level_counts.contains(log.level) {
          window.level_counts[log.level] = 0
        }
        window.level_counts[log.level] = window.level_counts[log.level] + 1
      }
      
      windows
    }
    
    let get_window_summary = |window| {
      let total_logs = window.logs.length()
      let error_count = if window.level_counts.contains("error") {
        window.level_counts["error"]
      } else {
        0
      }
      
      {
        start_time: window.start_time,
        end_time: window.end_time,
        total_logs: total_logs,
        error_count: error_count,
        error_rate: if total_logs > 0 { error_count.to_float() / total_logs.to_float() } else { 0.0 }
      }
    }
    
    (aggregate_by_window, get_window_summary)
  }
  
  let (aggregate_by_window, get_window_summary) = time_window_aggregator()
  
  // Test time window aggregation
  let windows = aggregate_by_window(all_logs, 3000L)  // 3-second windows
  assert_eq(windows.length(), 3)
  
  let window1 = get_window_summary(windows[base_time])
  assert_eq(window1.total_logs, 3)
  assert_eq(window1.error_count, 0)
  
  let window2 = get_window_summary(windows[base_time + 3000L])
  assert_eq(window2.total_logs, 3)
  assert_eq(window2.error_count, 2)
  assert_eq(window2.error_rate, 2.0 / 3.0)
}

// Test 5: Real-time Stream Processing
test "real_time_stream_processing" {
  // Test stream processor
  let stream_processor = || {
    let mut streams = {}
    let mut processors = {}
    
    let create_stream = |stream_name| {
      streams[stream_name] = {
        data: [],
        subscribers: []
      }
    }
    
    let publish = |stream_name, data| {
      if streams.contains(stream_name) {
        streams[stream_name].data.push({
          data: data,
          timestamp: 1640995200000L
        })
        
        // Notify subscribers
        for subscriber in streams[stream_name].subscribers {
          subscriber(data)
        }
        
        return true
      }
      false
    }
    
    let subscribe = |stream_name, callback| {
      if streams.contains(stream_name) {
        streams[stream_name].subscribers.push(callback)
        return true
      }
      false
    }
    
    let add_processor = |processor_name, input_stream, output_stream, processor_fn| {
      processors[processor_name] = {
        input_stream: input_stream,
        output_stream: output_stream,
        processor_fn: processor_fn
      }
      
      // Subscribe to input stream
      subscribe(input_stream, |data| {
        let processed_data = processor_fn(data)
        publish(output_stream, processed_data)
      })
    }
    
    let get_stream_data = |stream_name| {
      if streams.contains(stream_name) {
        streams[stream_name].data
      } else {
        []
      }
    }
    
    (create_stream, publish, subscribe, add_processor, get_stream_data)
  }
  
  let (create_stream, publish, subscribe, add_processor, get_data) = stream_processor()
  
  // Test stream processing
  create_stream("input")
  create_stream("output")
  create_stream("filtered")
  
  // Add a processor that doubles the value
  add_processor("doubler", "input", "output", |data| {
    data * 2
  })
  
  // Add a processor that filters even numbers
  add_processor("filter", "output", "filtered", |data| {
    if data % 2 == 0 {
      data
    } else {
      null
    }
  })
  
  // Publish some data
  publish("input", 5)
  publish("input", 10)
  publish("input", 15)
  publish("input", 20)
  
  // Check processed data
  let output_data = get_data("output")
  assert_eq(output_data.length(), 4)
  assert_eq(output_data[0].data, 10)  // 5 * 2
  assert_eq(output_data[1].data, 20)  // 10 * 2
  assert_eq(output_data[2].data, 30)  // 15 * 2
  assert_eq(output_data[3].data, 40)  // 20 * 2
  
  // Check filtered data
  let filtered_data = get_data("filtered")
  assert_eq(filtered_data.length(), 4)  // All are even after doubling
  assert_eq(filtered_data[0].data, 10)
  assert_eq(filtered_data[1].data, 20)
  assert_eq(filtered_data[2].data, 30)
  assert_eq(filtered_data[3].data, 40)
  
  // Test windowed aggregation
  let windowed_aggregator = || {
    let mut windows = {}
    let mut window_size = 1000L  // 1 second windows
    
    let set_window_size = |size| {
      window_size = size
    }
    
    let aggregate = |stream_name, data| {
      let timestamp = 1640995200000L
      let window_start = (timestamp / window_size) * window_size
      
      if !windows.contains(window_start) {
        windows[window_start] = {
          start_time: window_start,
          end_time: window_start + window_size,
          values: [],
          count: 0,
          sum: 0,
          min: 0,
          max: 0
        }
      }
      
      let window = windows[window_start]
      window.values.push(data)
      window.count = window.count + 1
      window.sum = window.sum + data
      
      if window.count == 1 {
        window.min = data
        window.max = data
      } else {
        if data < window.min {
          window.min = data
        }
        if data > window.max {
          window.max = data
        }
      }
    }
    
    let get_window_stats = |window_start| {
      if windows.contains(window_start) {
        let window = windows[window_start]
        let avg = if window.count > 0 {
          window.sum / window.count
        } else {
          0
        }
        
        {
          start_time: window.start_time,
          end_time: window.end_time,
          count: window.count,
          sum: window.sum,
          avg: avg,
          min: window.min,
          max: window.max
        }
      } else {
        null
      }
    }
    
    let get_all_windows = || {
      let mut all_windows = []
      
      for window_start in windows {
        all_windows.push(get_window_stats(window_start))
      }
      
      all_windows
    }
    
    (set_window_size, aggregate, get_window_stats, get_all_windows)
  }
  
  let (set_window_size, aggregate, get_window_stats, get_all_windows) = windowed_aggregator()
  
  // Test windowed aggregation
  set_window_size(2000L)  // 2-second windows
  
  // Simulate data points over time
  aggregate("metrics", 10)
  aggregate("metrics", 20)
  aggregate("metrics", 30)
  aggregate("metrics", 40)
  aggregate("metrics", 50)
  
  let window1 = get_window_stats(1640995200000L)
  match window1 {
    Some(stats) => {
      assert_eq(stats.count, 5)
      assert_eq(stats.sum, 150)
      assert_eq(stats.avg, 30)
      assert_eq(stats.min, 10)
      assert_eq(stats.max, 50)
    }
    None => assert_true(false)
  }
  
  // Test stream joining
  let stream_joiner = || {
    let mut joined_streams = {}
    
    let join_streams = |stream_names, join_key, output_stream| {
      for stream_name in stream_names {
        if !joined_streams.contains(stream_name) {
          joined_streams[stream_name] = {
            data: [],
            join_key: join_key,
            output_stream: output_stream
          }
        }
      }
    }
    
    let process_join = |stream_name, data| {
      if joined_streams.contains(stream_name) {
        let stream_info = joined_streams[stream_name]
        stream_info.data.push(data)
        
        // Check if we have matching data in other streams
        let mut matching_data = []
        
        for other_stream_name in joined_streams {
          if other_stream_name != stream_name {
            let other_stream = joined_streams[other_stream_name]
            
            for other_data in other_stream.data {
              if data[stream_info.join_key] == other_data[other_stream.join_key] {
                matching_data.push(other_data)
              }
            }
          }
        }
        
        // If we have matching data, create joined output
        if matching_data.length() > 0 {
          let joined_data = data.copy()
          
          for match_data in matching_data {
            for key in match_data {
              if key != stream_info.join_key {
                joined_data[key] = match_data[key]
              }
            }
          }
          
          publish(stream_info.output_stream, joined_data)
        }
      }
    }
    
    (join_streams, process_join)
  }
  
  // Test event pattern detection
  let event_pattern_detector = || {
    let mut patterns = []
    let mut event_history = []
    
    let add_pattern = |name, conditions| {
      patterns.push({
        name: name,
        conditions: conditions,
        matches: []
      })
    }
    
    let process_event = |event| {
      event_history.push(event)
      
      // Keep only recent events (sliding window)
      if event_history.length() > 100 {
        event_history = event_history.slice(-100)
      }
      
      // Check each pattern
      for i = 0; i < patterns.length(); i = i + 1 {
        let pattern = patterns[i]
        let mut match_found = true
        
        for condition in pattern.conditions {
          if !check_condition(condition, event, event_history) {
            match_found = false
            break
          }
        }
        
        if match_found {
          pattern.matches.push({
            event: event,
            timestamp: 1640995200000L
          })
        }
      }
    }
    
    let check_condition = |condition, event, history| {
      match condition.type {
        "event_type" => {
          event["type"] == condition.value
        }
        "event_count" => {
          let count = history.reduce(|acc, e| if e["type"] == condition.value { acc + 1 } else { acc }, 0)
          count >= condition.threshold
        }
        "time_window" => {
          // Check if events occurred within a time window
          let mut count = 0
          let window_start = 1640995200000L - condition.duration
          
          for e in history {
            if e["type"] == condition.value && e["timestamp"] >= window_start {
              count = count + 1
            }
          }
          
          count >= condition.threshold
        }
        _ => {
          false
        }
      }
    }
    
    let get_pattern_matches = |pattern_name| {
      for pattern in patterns {
        if pattern.name == pattern_name {
          return pattern.matches
        }
      }
      []
    }
    
    (add_pattern, process_event, get_pattern_matches)
  }
  
  let (add_pattern, process_event, get_matches) = event_pattern_detector()
  
  // Test event pattern detection
  add_pattern("error_spike", [
    { type: "event_count", value: "error", threshold: 5 }
  ])
  
  add_pattern("error_followed_by_recovery", [
    { type: "event_type", value: "error" },
    { type: "time_window", value: "recovery", duration: 10000, threshold: 1 }
  ])
  
  // Process some events
  process_event({ "type": "info", "timestamp": 1640995200000L })
  process_event({ "type": "error", "timestamp": 1640995201000L })
  process_event({ "type": "error", "timestamp": 1640995202000L })
  process_event({ "type": "error", "timestamp": 1640995203000L })
  process_event({ "type": "error", "timestamp": 1640995204000L })
  process_event({ "type": "error", "timestamp": 1640995205000L })
  process_event({ "type": "recovery", "timestamp": 1640995210000L })
  
  let error_spike_matches = get_matches("error_spike")
  assert_eq(error_spike_matches.length(), 5)  // Each error should match
  
  let recovery_matches = get_matches("error_followed_by_recovery")
  assert_eq(recovery_matches.length(), 5)  // Each error should be followed by recovery
}

// Test 6: Metrics Dashboard and Visualization
test "metrics_dashboard_and_visualization" {
  // Test dashboard builder
  let dashboard_builder = || {
    let mut widgets = []
    let mut widget_id_counter = 0
    
    let add_widget = |widget_type, title, data_source| {
      let widget_id = widget_id_counter
      widget_id_counter = widget_id_counter + 1
      
      let widget = {
        id: widget_id,
        type: widget_type,
        title: title,
        data_source: data_source,
        config: {}
      }
      
      widgets.push(widget)
      widget_id
    }
    
    let configure_widget = |widget_id, config| {
      for i = 0; i < widgets.length(); i = i + 1 {
        if widgets[i].id == widget_id {
          widgets[i].config = config
          return true
        }
      }
      false
    }
    
    let get_widget = |widget_id| {
      for widget in widgets {
        if widget.id == widget_id {
          return Some(widget)
        }
      }
      None
    }
    
    let get_all_widgets = || {
      widgets
    }
    
    let render_dashboard = || {
      let mut rendered = []
      
      for widget in widgets {
        let rendered_widget = render_widget(widget)
        rendered.push(rendered_widget)
      }
      
      rendered
    }
    
    let render_widget = |widget| {
      match widget.type {
        "metric_chart" => {
          {
            id: widget.id,
            type: widget.type,
            title: widget.title,
            data: get_metric_data(widget.data_source),
            chart_type: widget.config["chart_type"]
          }
        }
        "gauge" => {
          {
            id: widget.id,
            type: widget.type,
            title: widget.title,
            value: get_gauge_value(widget.data_source),
            min: widget.config["min"],
            max: widget.config["max"]
          }
        }
        "table" => {
          {
            id: widget.id,
            type: widget.type,
            title: widget.title,
            columns: widget.config["columns"],
            rows: get_table_data(widget.data_source)
          }
        }
        _ => {
          {
            id: widget.id,
            type: widget.type,
            title: widget.title,
            data: "Unknown widget type"
          }
        }
      }
    }
    
    let get_metric_data = |data_source| {
      // Simulate metric data retrieval
      if data_source == "cpu_usage" {
        [
          { timestamp: 1640995200000L, value: 25.5 },
          { timestamp: 1640995201000L, value: 30.2 },
          { timestamp: 1640995202000L, value: 45.8 },
          { timestamp: 1640995203000L, value: 35.1 },
          { timestamp: 1640995204000L, value: 28.9 }
        ]
      } else if data_source == "memory_usage" {
        [
          { timestamp: 1640995200000L, value: 1024 },
          { timestamp: 1640995201000L, value: 1080 },
          { timestamp: 1640995202000L, value: 1152 },
          { timestamp: 1640995203000L, value: 1200 },
          { timestamp: 1640995204000L, value: 1100 }
        ]
      } else {
        []
      }
    }
    
    let get_gauge_value = |data_source| {
      // Simulate gauge value retrieval
      if data_source == "error_rate" {
        0.02  // 2%
      } else if data_source == "response_time" {
        150  // 150ms
      } else {
        0
      }
    }
    
    let get_table_data = |data_source| {
      // Simulate table data retrieval
      if data_source == "top_errors" {
        [
          { error: "Database timeout", count: 15, last_occurrence: "2022-01-01 12:30:00" },
          { error: "Authentication failed", count: 8, last_occurrence: "2022-01-01 12:15:00" },
          { error: "File not found", count: 5, last_occurrence: "2022-01-01 11:45:00" }
        ]
      } else if data_source == "service_status" {
        [
          { service: "api", status: "healthy", uptime: "99.9%" },
          { service: "database", status: "healthy", uptime: "99.5%" },
          { service: "cache", status: "degraded", uptime: "98.2%" }
        ]
      } else {
        []
      }
    }
    
    (add_widget, configure_widget, get_widget, get_all_widgets, render_dashboard)
  }
  
  let (add_widget, configure_widget, get_widget, get_all_widgets, render_dashboard) = dashboard_builder()
  
  // Test dashboard builder
  let chart_widget = add_widget("metric_chart", "CPU Usage", "cpu_usage")
  let gauge_widget = add_widget("gauge", "Error Rate", "error_rate")
  let table_widget = add_widget("table", "Top Errors", "top_errors")
  
  // Configure widgets
  configure_widget(chart_widget, { "chart_type": "line" })
  configure_widget(gauge_widget, { "min": 0, "max": 1 })
  configure_widget(table_widget, { "columns": ["error", "count", "last_occurrence"] })
  
  // Test widget retrieval
  let widget = get_widget(chart_widget)
  match widget {
    Some(w) => {
      assert_eq(w.id, chart_widget)
      assert_eq(w.type, "metric_chart")
      assert_eq(w.title, "CPU Usage")
      assert_eq(w.data_source, "cpu_usage")
      assert_eq(w.config["chart_type"], "line")
    }
    None => assert_true(false)
  }
  
  // Test dashboard rendering
  let rendered = render_dashboard()
  assert_eq(rendered.length(), 3)
  
  // Check chart widget rendering
  let chart_rendered = rendered[0]
  assert_eq(chart_rendered.type, "metric_chart")
  assert_eq(chart_rendered.title, "CPU Usage")
  assert_eq(chart_rendered.data.length(), 5)
  assert_eq(chart_rendered.chart_type, "line")
  
  // Check gauge widget rendering
  let gauge_rendered = rendered[1]
  assert_eq(gauge_rendered.type, "gauge")
  assert_eq(gauge_rendered.title, "Error Rate")
  assert_eq(gauge_rendered.value, 0.02)
  assert_eq(gauge_rendered.min, 0)
  assert_eq(gauge_rendered.max, 1)
  
  // Test alert manager
  let alert_manager = || {
    let mut alerts = []
    let mut alert_rules = []
    let mut triggered_alerts = []
    
    let add_alert_rule = |name, condition, severity, message| {
      alert_rules.push({
        name: name,
        condition: condition,
        severity: severity,
        message: message,
        triggered: false
      })
    }
    
    let check_alerts = |metrics| {
      for i = 0; i < alert_rules.length(); i = i + 1 {
        let rule = alert_rules[i]
        
        if evaluate_condition(rule.condition, metrics) {
          if !rule.triggered {
            let alert = {
              id: alerts.length(),
              name: rule.name,
              severity: rule.severity,
              message: rule.message,
              timestamp: 1640995200000L,
              acknowledged: false
            }
            
            alerts.push(alert)
            triggered_alerts.push(alert)
            alert_rules[i].triggered = true
          }
        } else {
          if rule.triggered {
            // Alert resolved
            alert_rules[i].triggered = false
          }
        }
      }
    }
    
    let evaluate_condition = |condition, metrics| {
      match condition.metric {
        "cpu_usage" => {
          let value = metrics["cpu_usage"]
          match condition.operator {
            ">" => value > condition.threshold
            "<" => value < condition.threshold
            "=" => value == condition.threshold
            _ => false
          }
        }
        "memory_usage" => {
          let value = metrics["memory_usage"]
          match condition.operator {
          ">" => value > condition.threshold
          "<" => value < condition.threshold
          "=" => value == condition.threshold
          _ => false
          }
        }
        _ => false
      }
    }
    
    let acknowledge_alert = |alert_id| {
      for i = 0; i < alerts.length(); i = i + 1 {
        if alerts[i].id == alert_id {
          alerts[i].acknowledged = true
          return true
        }
      }
      false
    }
    
    let get_active_alerts = || {
      let mut active = []
      
      for alert in alerts {
        if !alert.acknowledged {
          active.push(alert)
        }
      }
      
      active
    }
    
    let get_alert_history = || {
      alerts
    }
    
    (add_alert_rule, check_alerts, acknowledge_alert, get_active_alerts, get_alert_history)
  }
  
  let (add_rule, check_alerts, acknowledge_alert, get_active, get_history) = alert_manager()
  
  // Test alert manager
  add_rule("High CPU Usage", {
    metric: "cpu_usage",
    operator: ">",
    threshold: 80
  }, "warning", "CPU usage is above 80%")
  
  add_rule("Low Memory", {
    metric: "memory_usage",
    operator: "<",
    threshold: 100
  }, "critical", "Available memory is below 100MB")
  
  // Check alerts with normal metrics
  check_alerts({
    "cpu_usage": 45,
    "memory_usage": 500
  })
  
  let active_alerts = get_active()
  assert_eq(active_alerts.length(), 0)
  
  // Check alerts with problematic metrics
  check_alerts({
    "cpu_usage": 85,
    "memory_usage": 50
  })
  
  let active_alerts2 = get_active()
  assert_eq(active_alerts2.length(), 1)
  assert_eq(active_alerts2[0].name, "High CPU Usage")
  assert_eq(active_alerts2[0].severity, "warning")
  
  check_alerts({
    "cpu_usage": 45,
    "memory_usage": 80
  })
  
  let active_alerts3 = get_active()
  assert_eq(active_alerts3.length(), 1)
  assert_eq(active_alerts3[0].name, "Low Memory")
  assert_eq(active_alerts3[0].severity, "critical")
  
  // Test report generator
  let report_generator = || {
    let generate_summary_report = |time_range| {
      {
        title: "System Summary Report",
        time_range: time_range,
        metrics: {
          "cpu_usage": { avg: 45.2, max: 85.3, min: 12.1 },
          "memory_usage": { avg: 512, max: 1024, min: 256 },
          "error_rate": { avg: 0.02, max: 0.05, min: 0.01 },
          "response_time": { avg: 150, max: 300, min: 50 }
        },
        alerts: {
          total: 15,
          critical: 3,
          warning: 8,
          info: 4
        },
        top_errors: [
          { error: "Database timeout", count: 42 },
          { error: "Authentication failed", count: 28 },
          { error: "File not found", count: 15 }
        ]
      }
    }
    
    let generate_trend_report = |metric, time_range| {
      {
        title: metric + " Trend Report",
        time_range: time_range,
        data_points: [
          { timestamp: 1640995200000L, value: 25.5 },
          { timestamp: 1640995260000L, value: 30.2 },
          { timestamp: 1640995320000L, value: 45.8 },
          { timestamp: 1640995380000L, value: 35.1 },
          { timestamp: 1640995440000L, value: 28.9 }
        ],
        trend: "stable",
        change_percent: 5.2
      }
    }
    
    let generate_alert_report = |time_range| {
      {
        title: "Alert Summary Report",
        time_range: time_range,
        total_alerts: 47,
        alerts_by_severity: {
          critical: 8,
          warning: 23,
          info: 16
        },
        alerts_by_service: {
          "api": 15,
          "database": 12,
          "cache": 8,
          "queue": 7,
          "storage": 5
        },
        top_alert_rules: [
          { rule: "High CPU Usage", triggered: 12 },
          { rule: "High Memory Usage", triggered: 8 },
          { rule: "Database Connection Failed", triggered: 6 }
        ],
        mttr: 15.5,  // Mean time to resolution in minutes
        mtbf: 240.0  // Mean time between failures in minutes
      }
    }
    
    (generate_summary_report, generate_trend_report, generate_alert_report)
  }
  
  let (gen_summary, gen_trend, gen_alert) = report_generator()
  
  // Test report generation
  let time_range = {
    start: 1640995200000L,
    end: 1640995280000L
  }
  
  let summary_report = gen_summary(time_range)
  assert_eq(summary_report.title, "System Summary Report")
  assert_eq(summary_report.metrics["cpu_usage"].avg, 45.2)
  assert_eq(summary_report.alerts.total, 15)
  assert_eq(summary_report.top_errors.length(), 3)
  
  let trend_report = gen_trend("CPU Usage", time_range)
  assert_eq(trend_report.title, "CPU Usage Trend Report")
  assert_eq(trend_report.data_points.length(), 5)
  assert_eq(trend_report.trend, "stable")
  
  let alert_report = gen_alert(time_range)
  assert_eq(alert_report.title, "Alert Summary Report")
  assert_eq(alert_report.total_alerts, 47)
  assert_eq(alert_report.alerts_by_severity["critical"], 8)
  assert_eq(alert_report.top_alert_rules.length(), 3)
}

// Test 7: Anomaly Detection in Telemetry Data
test "anomaly_detection_in_telemetry_data" {
  // Test anomaly detector
  let anomaly_detector = || {
    let mut models = {}
    let mut anomalies = []
    
    let add_model = |metric_name, model_type, parameters| {
      models[metric_name] = {
        type: model_type,
        parameters: parameters,
        trained: false,
        training_data: []
      }
    }
    
    let train_model = |metric_name, data| {
      if models.contains(metric_name) {
        let model = models[metric_name]
        model.training_data = data
        
        match model.type {
          "statistical" => {
            // Calculate mean and standard deviation
            let sum = data.reduce(|acc, value| acc + value, 0)
            let mean = sum / data.length()
            
            let variance = data.reduce(|acc, value| acc + (value - mean) * (value - mean), 0) / data.length()
            let std_dev = variance.sqrt()
            
            model.parameters["mean"] = mean
            model.parameters["std_dev"] = std_dev
            model.trained = true
          }
          "threshold" => {
            // Use percentile-based threshold
            let mut sorted_data = data.copy()
            sorted_data.sort()
            
            let p95_index = (sorted_data.length() * 0.95).to_int()
            let p99_index = (sorted_data.length() * 0.99).to_int()
            
            model.parameters["p95"] = sorted_data[p95_index]
            model.parameters["p99"] = sorted_data[p99_index]
            model.trained = true
          }
        }
      }
    }
    
    let detect_anomaly = |metric_name, value| {
      if models.contains(metric_name) {
        let model = models[metric_name]
        
        if !model.trained {
          return false
        }
        
        match model.type {
          "statistical" => {
            let mean = model.parameters["mean"]
            let std_dev = model.parameters["std_dev"]
            let z_score = (value - mean) / std_dev
            
            // Anomaly if z-score > 3 or < -3
            if z_score.abs() > 3 {
              anomalies.push({
                metric: metric_name,
                value: value,
                timestamp: 1640995200000L,
                z_score: z_score,
                type: "statistical"
              })
              return true
            }
          }
          "threshold" => {
            let p99 = model.parameters["p99"]
            
            if value > p99 {
              anomalies.push({
                metric: metric_name,
                value: value,
                timestamp: 1640995200000L,
                threshold: p99,
                type: "threshold"
              })
              return true
            }
          }
        }
      }
      
      false
    }
    
    let get_anomalies = || {
      anomalies
    }
    
    let get_anomaly_summary = || {
      let mut summary = {}
      
      for anomaly in anomalies {
        if !summary.contains(anomaly.metric) {
          summary[anomaly.metric] = {
            count: 0,
            type: anomaly.type
          }
        }
        
        summary[anomaly.metric].count = summary[anomaly.metric].count + 1
      }
      
      summary
    }
    
    (add_model, train_model, detect_anomaly, get_anomalies, get_anomaly_summary)
  }
  
  let (add_model, train_model, detect_anomaly, get_anomalies, get_summary) = anomaly_detector()
  
  // Test anomaly detection
  add_model("cpu_usage", "statistical", {})
  add_model("memory_usage", "threshold", {})
  
  // Train models with normal data
  let cpu_data = [25.5, 30.2, 28.7, 32.1, 29.8, 31.5, 27.9, 30.6, 29.3, 31.2]
  let memory_data = [512, 480, 496, 520, 504, 488, 512, 528, 496, 508]
  
  train_model("cpu_usage", cpu_data)
  train_model("memory_usage", memory_data)
  
  // Test anomaly detection
  assert_false(detect_anomaly("cpu_usage", 30.0))  // Normal value
  assert_false(detect_anomaly("memory_usage", 500))  // Normal value
  
  assert_true(detect_anomaly("cpu_usage", 80.0))   // Anomalous high value
  assert_true(detect_anomaly("memory_usage", 800))  // Anomalous high value
  
  let anomalies = get_anomalies()
  assert_eq(anomalies.length(), 2)
  
  let summary = get_summary()
  assert_eq(summary["cpu_usage"].count, 1)
  assert_eq(summary["memory_usage"].count, 1)
  
  // Test seasonal anomaly detection
  let seasonal_anomaly_detector = || {
    let mut seasonal_patterns = {}
    
    let learn_seasonal_pattern = |metric_name, data, period| {
      let mut pattern = []
      
      // Calculate average for each period position
      for i = 0; i < period; i = i + 1 {
        let mut sum = 0
        let mut count = 0
        
        for j = i; j < data.length(); j = j + period {
          sum = sum + data[j]
          count = count + 1
        }
        
        if count > 0 {
          pattern.push(sum / count)
        } else {
          pattern.push(0)
        }
      }
      
      seasonal_patterns[metric_name] = {
        pattern: pattern,
        period: period
      }
    }
    
    let detect_seasonal_anomaly = |metric_name, value, position| {
      if seasonal_patterns.contains(metric_name) {
        let pattern = seasonal_patterns[metric_name]
        
        if position < pattern.pattern.length() {
          let expected = pattern.pattern[position]
          let deviation = value - expected
          let threshold = expected * 0.5  // 50% deviation threshold
          
          if deviation.abs() > threshold {
            return {
              anomalous: true,
              expected: expected,
              actual: value,
              deviation: deviation,
              threshold: threshold
            }
          }
        }
      }
      
      { anomalous: false }
    }
    
    (learn_seasonal_pattern, detect_seasonal_anomaly)
  }
  
  let (learn_pattern, detect_seasonal) = seasonal_anomaly_detector()
  
  // Test seasonal anomaly detection
  let hourly_data = [
    10, 15, 25, 40, 35, 30, 25, 20, 15, 10, 8, 5,  // First day
    12, 18, 28, 42, 38, 32, 28, 22, 18, 12, 10, 6,   // Second day
    11, 16, 26, 41, 36, 31, 26, 21, 16, 11, 9, 5     // Third day
  ]
  
  learn_pattern("hourly_pattern", hourly_data, 12)  // 12-hour period
  
  // Normal values
  let result1 = detect_seasonal("hourly_pattern", 30, 5)   // 5 AM, expected ~8
  let result2 = detect_seasonal("hourly_pattern", 40, 3)   // 3 AM, expected ~15
  
  assert_false(result1.anomalous)  // 30 is close to expected 8 (within threshold)
  assert_false(result2.anomalous)  // 40 is close to expected 15 (within threshold)
  
  // Anomalous values
  let result3 = detect_seasonal("hourly_pattern", 80, 3)   // 3 AM, expected ~15
  let result4 = detect_seasonal("hourly_pattern", 2, 5)    // 5 AM, expected ~8
  
  assert_true(result3.anomalous)  // 80 is far from expected 15
  assert_true(result4.anomalous)  // 2 is far from expected 8
  
  // Test multivariate anomaly detection
  let multivariate_anomaly_detector = || {
    let mut correlation_matrix = {}
    let mut means = {}
    
    let train_model = |metrics| {
      let metric_names = metrics.keys()
      
      // Calculate means for each metric
      for metric_name in metric_names {
        let values = metrics[metric_name]
        let sum = values.reduce(|acc, value| acc + value, 0)
        means[metric_name] = sum / values.length()
      }
      
      // Calculate correlation matrix
      for metric1 in metric_names {
        correlation_matrix[metric1] = {}
        
        for metric2 in metric_names {
          let values1 = metrics[metric1]
          let values2 = metrics[metric2]
          let mean1 = means[metric1]
          let mean2 = means[metric2]
          
          let mut numerator = 0
          let mut denominator1 = 0
          let mut denominator2 = 0
          
          for i = 0; i < values1.length(); i = i + 1 {
            let diff1 = values1[i] - mean1
            let diff2 = values2[i] - mean2
            
            numerator = numerator + diff1 * diff2
            denominator1 = denominator1 + diff1 * diff1
            denominator2 = denominator2 + diff2 * diff2
          }
          
          let correlation = if denominator1 > 0 && denominator2 > 0 {
            numerator / (denominator1 * denominator2).sqrt()
          } else {
            0
          }
          
          correlation_matrix[metric1][metric2] = correlation
        }
      }
    }
    
    let detect_anomaly = |current_values| {
      let metric_names = current_values.keys()
      let mut anomalies = []
      
      for metric1 in metric_names {
        for metric2 in metric_names {
          if metric1 != metric2 && correlation_matrix.contains(metric1) && 
             correlation_matrix[metric1].contains(metric2) {
            let correlation = correlation_matrix[metric1][metric2]
            
            // Check if the relationship between metrics is anomalous
            let expected_metric2 = means[metric2] + 
              correlation * (current_values[metric1] - means[metric1])
            
            let deviation = current_values[metric2] - expected_metric2
            let threshold = means[metric2] * 0.2  // 20% deviation threshold
            
            if deviation.abs() > threshold {
              anomalies.push({
                metric1: metric1,
                metric2: metric2,
                actual1: current_values[metric1],
                actual2: current_values[metric2],
                expected2: expected_metric2,
                deviation: deviation,
                correlation: correlation
              })
            }
          }
        }
      }
      
      anomalies
    }
    
    (train_model, detect_anomaly)
  }
  
  let (train_model, detect_anomaly) = multivariate_anomaly_detector()
  
  // Test multivariate anomaly detection
  let training_metrics = {
    "cpu_usage": [25, 30, 35, 40, 45],
    "memory_usage": [512, 524, 536, 548, 560],
    "response_time": [100, 110, 120, 130, 140]
  }
  
  train_model(training_metrics)
  
  // Normal correlation: as CPU increases, memory and response time increase
  let normal_values = {
    "cpu_usage": 32,
    "memory_usage": 528,
    "response_time": 112
  }
  
  let normal_anomalies = detect_anomaly(normal_values)
  assert_eq(normal_anomalies.length(), 0)
  
  // Anomalous correlation: CPU is high but memory is low
  let anomalous_values = {
    "cpu_usage": 42,
    "memory_usage": 480,
    "response_time": 130
  }
  
  let anomalous_anomalies = detect_anomaly(anomalous_values)
  assert_eq(anomalous_anomalies.length(), 2)  // memory_usage and response_time anomalies
  
  // Test time series anomaly detection
  let time_series_anomaly_detector = || {
    let detect_change_point = |data, min_segment_size| {
      if data.length() < 2 * min_segment_size {
        return null
      }
      
      let max_diff = 0
      let change_point = -1
      
      for i = min_segment_size; i < data.length() - min_segment_size; i = i + 1 {
        // Calculate mean before and after point i
        let mut sum_before = 0
        let mut sum_after = 0
        
        for j = 0; j < i; j = j + 1 {
          sum_before = sum_before + data[j]
        }
        
        for j = i; j < data.length(); j = j + 1 {
          sum_after = sum_after + data[j]
        }
        
        let mean_before = sum_before / i
        let mean_after = sum_after / (data.length() - i)
        
        let diff = (mean_after - mean_before).abs()
        
        if diff > max_diff {
          max_diff = diff
          change_point = i
        }
      }
      
      if change_point >= 0 {
        return {
          index: change_point,
          magnitude: max_diff
        }
      }
      
      null
    }
    
    let detect_outliers = |data, threshold| {
      let mut outliers = []
      
      if data.length() < 3 {
        return outliers
      }
      
      let sum = data.reduce(|acc, value| acc + value, 0)
      let mean = sum / data.length()
      
      let mut sum_squared_diff = 0
      for value in data {
        sum_squared_diff = sum_squared_diff + (value - mean) * (value - mean)
      }
      
      let std_dev = (sum_squared_diff / data.length()).sqrt()
      
      for i = 0; i < data.length(); i = i + 1 {
        let z_score = (data[i] - mean) / std_dev
        
        if z_score.abs() > threshold {
          outliers.push({
            index: i,
            value: data[i],
            z_score: z_score
          })
        }
      }
      
      outliers
    }
    
    (detect_change_point, detect_outliers)
  }
  
  let (detect_change_point, detect_outliers) = time_series_anomaly_detector()
  
  // Test time series anomaly detection
  let time_series_data = [10, 12, 11, 13, 15, 14, 25, 27, 26, 28, 30, 29]
  
  let change_point = detect_change_point(time_series_data, 3)
  match change_point {
    Some(cp) => {
      assert_eq(cp.index, 6)  // Change occurs at index 6 (value jumps from 14 to 25)
      assert_true(cp.magnitude > 10)
    }
    None => assert_true(false)
  }
  
  let outliers = detect_outliers(time_series_data, 2.0)
  assert_eq(outliers.length(), 0)  // No outliers with threshold of 2.0
  
  let outliers2 = detect_outliers([10, 12, 11, 13, 15, 50, 14, 25, 27, 26, 28, 30, 29], 2.0)
  assert_eq(outliers2.length(), 1)  // Value 50 is an outlier
}

// Test 8: Telemetry Data Retention and Archiving
test "telemetry_data_retention_and_archiving" {
  // Test data retention policy
  let retention_policy_manager = || {
    let mut policies = {}
    
    let add_policy = |data_type, retention_period, archival_policy| {
      policies[data_type] = {
        retention_period: retention_period,
        archival_policy: archival_policy,
        created_at: 1640995200000L
      }
    }
    
    let should_retain = |data_type, timestamp| {
      if policies.contains(data_type) {
        let policy = policies[data_type]
        let current_time = 1640995200000L
        let age = current_time - timestamp
        
        return age <= policy.retention_period
      }
      
      false
    }
    
    let should_archive = |data_type, timestamp| {
      if policies.contains(data_type) {
        let policy = policies[data_type]
        let current_time = 1640995200000L
        let age = current_time - timestamp
        
        return age > policy.retention_period && policy.archival_policy != "delete"
      }
      
      false
    }
    
    let get_retention_info = |data_type| {
      if policies.contains(data_type) {
        Some(policies[data_type])
      } else {
        null
      }
    }
    
    (add_policy, should_retain, should_archive, get_retention_info)
  }
  
  let (add_policy, should_retain, should_archive, get_info) = retention_policy_manager()
  
  // Test retention policies
  add_policy("logs", 86400000L, "compress")  // 1 day retention, then compress
  add_policy("metrics", 604800000L, "archive")  // 7 days retention, then archive
  add_policy("traces", 2592000000L, "delete")  // 30 days retention, then delete
  
  let current_time = 1640995200000L
  
  // Test retention decisions
  assert_true(should_retain("logs", current_time - 3600000L))    // 1 hour ago
  assert_false(should_retain("logs", current_time - 86400000L))  // 1 day ago
  assert_false(should_retain("logs", current_time - 172800000L))  // 2 days ago
  
  assert_true(should_retain("metrics", current_time - 3600000L))    // 1 hour ago
  assert_true(should_retain("metrics", current_time - 604800000L))  // 7 days ago
  assert_false(should_retain("metrics", current_time - 1209600000L)) // 14 days ago
  
  // Test archival decisions
  assert_false(should_archive("logs", current_time - 3600000L))    // 1 hour ago
  assert_true(should_archive("logs", current_time - 86400000L))   // 1 day ago
  
  assert_false(should_archive("metrics", current_time - 604800000L))  // 7 days ago
  assert_true(should_archive("metrics", current_time - 1209600000L)) // 14 days ago
  
  assert_false(should_archive("traces", current_time - 2592000000L)) // 30 days ago
  assert_false(should_archive("traces", current_time - 5184000000L)) // 60 days ago
  
  // Test data archiver
  let data_archiver = || {
    let mut archive_stats = {}
    
    let archive_data = |data_id, data_type, data| {
      if !archive_stats.contains(data_type) {
        archive_stats[data_type] = {
          total_archived: 0,
          total_size: 0,
          compression_ratio: 0
        }
      }
      
      let original_size = data.length()
      let compressed_data = compress_data(data)
      let compressed_size = compressed_data.length()
      
      archive_stats[data_type].total_archived = archive_stats[data_type].total_archived + 1
      archive_stats[data_type].total_size = archive_stats[data_type].total_size + compressed_size
      archive_stats[data_type].compression_ratio = original_size.to_float() / compressed_size.to_float()
      
      return {
        data_id: data_id,
        original_size: original_size,
        compressed_size: compressed_size,
        compression_ratio: original_size.to_float() / compressed_size.to_float(),
        archived_at: 1640995200000L
      }
    }
    
    let compress_data = |data| {
      // Simple compression simulation
      let mut compressed = ""
      let mut i = 0
      
      while i < data.length() {
        let char = data[i]
        let mut count = 1
        
        // Count consecutive identical characters
        while i + count < data.length() && data[i + count] == char {
          count = count + 1
        }
        
        if count > 3 {
          compressed = compressed + char.to_string() + "[" + count.to_string() + "]"
        } else {
          for j = 0; j < count; j = j + 1 {
            compressed = compressed + char
          }
        }
        
        i = i + count
      }
      
      compressed
    }
    
    let get_archive_stats = || {
      archive_stats
    }
    
    (archive_data, get_archive_stats)
  }
  
  let (archive_data, get_stats) = data_archiver()
  
  // Test data archiving
  let log_data = "2022-01-01 12:00:00 INFO Application started\n2022-01-01 12:00:01 INFO Database connected\n2022-01-01 12:00:02 INFO Ready to serve requests"
  let metric_data = "cpu_usage,25.5,memory_usage,512,response_time,150"
  
  let log_archive = archive_data("log-001", "logs", log_data)
  let metric_archive = archive_data("metric-001", "metrics", metric_data)
  
  assert_eq(log_archive.data_id, "log-001")
  assert_eq(log_archive.original_size, 154)
  assert_true(log_archive.compressed_size < log_archive.original_size)
  assert_true(log_archive.compression_ratio > 1.0)
  
  let stats = get_stats()
  assert_eq(stats["logs"].total_archived, 1)
  assert_eq(stats["metrics"].total_archived, 1)
  assert_true(stats["logs"].compression_ratio > 1.0)
  
  // Test data lifecycle manager
  let data_lifecycle_manager = || {
    let mut data_lifecycle = {}
    
    let register_data = |data_id, data_type, timestamp| {
      data_lifecycle[data_id] = {
        data_type: data_type,
        timestamp: timestamp,
        state: "active",
        transitions: [
          { state: "created", timestamp: timestamp }
        ]
      }
    }
    
    let transition_data = |data_id, new_state| {
      if data_lifecycle.contains(data_id) {
        let current_time = 1640995200000L
        data_lifecycle[data_id].state = new_state
        data_lifecycle[data_id].transitions.push({
          state: new_state,
          timestamp: current_time
        })
        return true
      }
      false
    }
    
    let get_data_state = |data_id| {
      if data_lifecycle.contains(data_id) {
        Some(data_lifecycle[data_id].state)
      } else {
        None
      }
    }
    
    let get_data_lifecycle = |data_id| {
      if data_lifecycle.contains(data_id) {
        Some(data_lifecycle[data_id])
      } else {
        None
      }
    }
    
    let get_data_by_state = |state| {
      let mut matching_data = []
      
      for data_id in data_lifecycle {
        if data_lifecycle[data_id].state == state {
          matching_data.push(data_id)
        }
      }
      
      matching_data
    }
    
    (register_data, transition_data, get_data_state, get_data_lifecycle, get_data_by_state)
  }
  
  let (register_data, transition_data, get_state, get_lifecycle, get_by_state) = data_lifecycle_manager()
  
  // Test data lifecycle
  register_data("data-001", "logs", 1640995200000L)
  register_data("data-002", "metrics", 1640995201000L)
  register_data("data-003", "traces", 1640995202000L)
  
  assert_eq(get_state("data-001"), Some("active"))
  
  transition_data("data-001", "archived")
  assert_eq(get_state("data-001"), Some("archived"))
  
  transition_data("data-002", "compressed")
  assert_eq(get_state("data-002"), Some("compressed"))
  
  transition_data("data-003", "deleted")
  assert_eq(get_state("data-003"), Some("deleted"))
  
  let archived_data = get_by_state("archived")
  assert_eq(archived_data.length(), 1)
  assert_true(archived_data.contains("data-001"))
  
  let deleted_data = get_by_state("deleted")
  assert_eq(deleted_data.length(), 1)
  assert_true(deleted_data.contains("data-003"))
  
  // Test data cleanup
  let data_cleanup = || {
    let mut cleanup_stats = {}
    
    let cleanup_expired_data = |current_time, policies| {
      let mut expired_data = []
      
      for data_id in data_lifecycle {
        let lifecycle = data_lifecycle[data_id]
        let age = current_time - lifecycle.timestamp
        
        if policies.contains(lifecycle.data_type) {
          let policy = policies[lifecycle.data_type]
          
          if age > policy.retention_period {
            expired_data.push({
              data_id: data_id,
              data_type: lifecycle.data_type,
              age: age,
              retention_period: policy.retention_period
            })
            
            // Mark as deleted
            transition_data(data_id, "deleted")
          }
        }
      }
      
      expired_data
    }
    
    let get_cleanup_stats = || {
      cleanup_stats
    }
    
    (cleanup_expired_data, get_cleanup_stats)
  }
  
  let (cleanup_expired, get_cleanup_stats) = data_cleanup()
  
  // Test data cleanup
  let policies = {
    "logs": 86400000L,      // 1 day
    "metrics": 604800000L,   // 7 days
    "traces": 2592000000L    // 30 days
  }
  
  let expired_data = cleanup_expired(1640995300000L, policies)
  
  // Test data query interface
  let data_query_interface = || {
    let mut data_store = {}
    
    let store_data = |data_id, data| {
      data_store[data_id] = {
        data: data,
        indexed_at: 1640995200000L,
        tags: {}
      }
    }
    
    let add_tag = |data_id, tag, value| {
      if data_store.contains(data_id) {
        data_store[data_id].tags[tag] = value
      }
    }
    
    let query_by_tag = |tag, value| {
      let mut results = []
      
      for data_id in data_store {
        if data_store[data_id].tags.contains(tag) &&
           data_store[data_id].tags[tag] == value {
          results.push({
            data_id: data_id,
            data: data_store[data_id].data
          })
        }
      }
      
      results
    }
    
    let query_by_time_range = |start_time, end_time| {
      let mut results = []
      
      for data_id in data_store {
        if data_store[data_id].indexed_at >= start_time &&
           data_store[data_id].indexed_at <= end_time {
          results.push({
            data_id: data_id,
            data: data_store[data_id].data
          })
        }
      }
      
      results
    }
    
    let full_text_search = |query| {
      let mut results = []
      
      for data_id in data_store {
        let data = data_store[data_id].data
        
        if data.contains(query) {
          results.push({
            data_id: data_id,
            data: data
          })
        }
      }
      
      results
    }
    
    (store_data, add_tag, query_by_tag, query_by_time_range, full_text_search)
  }
  
  let (store_data, add_tag, query_by_tag, query_by_time, full_text_search) = data_query_interface()
  
  // Test data query interface
  store_data("data-001", "CPU usage is high")
  store_data("data-002", "Memory usage is normal")
  store_data("data-003", "Database connection failed")
  
  add_tag("data-001", "type", "metric")
  add_tag("data-002", "type", "metric")
  add_tag("data-003", "type", "error")
  
  add_tag("data-001", "service", "api")
  add_tag("data-002", "service", "api")
  add_tag("data-003", "service", "database")
  
  // Test tag queries
  let metric_data = query_by_tag("type", "metric")
  assert_eq(metric_data.length(), 2)
  
  let api_data = query_by_tag("service", "api")
  assert_eq(api_data.length(), 2)
  
  // Test time range queries
  let time_data = query_by_time(1640995100000L, 1640995300000L)
  assert_eq(time_data.length(), 3)
  
  // Test full-text search
  let search_results = full_text_search("CPU")
  assert_eq(search_results.length(), 1)
  
  let error_results = full_text_search("failed")
  assert_eq(error_results.length(), 1)
}

// Test 9: Telemetry Data Compression and Optimization
test "telemetry_data_compression_and_optimization" {
  // Test data compression
  let data_compressor = || {
    let compress_string = |input| {
      // Simple run-length encoding compression
      let mut compressed = ""
      let mut i = 0
      
      while i < input.length() {
        let char = input[i]
        let mut count = 1
        
        while i + count < input.length() && input[i + count] == char {
          count = count + 1
        }
        
        if count > 3 {
          compressed = compressed + char.to_string() + "[" + count.to_string() + "]"
        } else {
          for j = 0; j < count; j = j + 1 {
            compressed = compressed + char
          }
        }
        
        i = i + count
      }
      
      compressed
    }
    
    let decompress_string = |compressed| {
      let mut decompressed = ""
      let mut i = 0
      
      while i < compressed.length() {
        let char = compressed[i]
        
        if char == '[' {
          // Find closing bracket
          let mut j = i + 1
          while j < compressed.length() && compressed[j] != ']' {
            j = j + 1
          }
          
          if j < compressed.length() {
            let count_str = compressed.slice(i + 1, j)
            let count = count_str.to_int()
            
            let repeat_char = compressed[i - 1]
            for k = 0; k < count; k = k + 1 {
              decompressed = decompressed + repeat_char
            }
            
            i = j + 1
          } else {
            decompressed = decompressed + char
            i = i + 1
          }
        } else {
          decompressed = decompressed + char
          i = i + 1
        }
      }
      
      decompressed
    }
    
    let compress_json = |json_data| {
      // Simple JSON compression by removing unnecessary whitespace
      let mut compressed = ""
      let mut in_string = false
      let mut escape_next = false
      
      for i = 0; i < json_data.length(); i = i + 1 {
        let char = json_data[i]
        
        if escape_next {
          compressed = compressed + char
          escape_next = false
        } else if char == '\\' {
          compressed = compressed + char
          escape_next = true
        } else if char == '"' {
          compressed = compressed + char
          in_string = !in_string
        } else if !in_string && (char == ' ' || char == '\n' || char == '\t') {
          // Skip whitespace outside strings
        } else {
          compressed = compressed + char
        }
      }
      
      compressed
    }
    
    let compress_numeric_array = |numbers| {
      // Delta encoding for numeric arrays
      if numbers.length() == 0 {
        return ""
      }
      
      let mut compressed = numbers[0].to_string()
      let prev = numbers[0]
      
      for i = 1; i < numbers.length(); i = i + 1 {
        let delta = numbers[i] - prev
        compressed = compressed + "," + delta.to_string()
        prev = numbers[i]
      }
      
      compressed
    }
    
    let decompress_numeric_array = |compressed| {
      let parts = compressed.split(",")
      let mut numbers = []
      
      for i = 0; i < parts.length(); i = i + 1 {
        if i == 0 {
          numbers.push(parts[i].to_int())
        } else {
          if numbers.length() > 0 {
            let prev = numbers[numbers.length() - 1]
            numbers.push(prev + parts[i].to_int())
          }
        }
      }
      
      numbers
    }
    
    (compress_string, decompress_string, compress_json, compress_numeric_array, decompress_numeric_array)
  }
  
  let (compress_str, decompress_str, compress_json, compress_num_array, decompress_num_array) = data_compressor()
  
  // Test string compression
  let original_string = "aaaaabbbcccccddddddeeeee"
  let compressed_string = compress_str(original_string)
  assert_eq(compressed_string, "a[5]b[3]c[5]d[5]e[5]")
  
  let decompressed_string = decompress_str(compressed_string)
  assert_eq(decompressed_string, original_string)
  
  // Test JSON compression
  let json_data = "{ \"name\": \"John\", \"age\": 30, \"active\": true }"
  let compressed_json = compress_json(json_data)
  assert_eq(compressed_json, "{\"name\":\"John\",\"age\":30,\"active\":true}")
  
  // Test numeric array compression
  let numbers = [100, 110, 115, 120, 125, 130, 135]
  let compressed_numbers = compress_num_array(numbers)
  assert_eq(compressed_numbers, "100,10,5,5,5,5,5")
  
  let decompressed_numbers = decompress_num_array(compressed_numbers)
  assert_eq(decompressed_numbers, numbers)
  
  // Test batch processing
  let batch_processor = || {
    let process_batch = |items, batch_size, processor| {
      let mut results = []
      
      for i = 0; i < items.length(); i = i + batch_size {
        let end = if i + batch_size < items.length() { i + batch_size } else { items.length() }
        let batch = items.slice(i, end)
        
        let batch_result = processor(batch)
        results.push(batch_result)
      }
      
      results
    }
    
    (process_batch)
  }
  
  let (process_batch) = batch_processor()
  
  // Test batch processing
  let items = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let batch_results = process_batch(items, 3, |batch| {
    batch.reduce(|acc, val| acc + val, 0)
  })
  
  assert_eq(batch_results.length(), 4)
  assert_eq(batch_results[0], 6)   // 1 + 2 + 3
  assert_eq(batch_results[1], 15)  // 4 + 5 + 6
  assert_eq(batch_results[2], 24)  // 7 + 8 + 9
  assert_eq(batch_results[3], 10)  // 10
  
  // Test data sampling
  let data_sampler = || {
    let sample_random = |data, sample_rate| {
      let mut sampled = []
      
      for item in data {
        if Math.random() < sample_rate {
          sampled.push(item)
        }
      }
      
      sampled
    }
    
    let sample_systematic = |data, interval| {
      let mut sampled = []
      
      for i = 0; i < data.length(); i = i + interval {
        sampled.push(data[i])
      }
      
      sampled
    }
    
    let sample_stratified = |data, strata| {
      let mut sampled = []
      
      for stratum in strata {
        let stratum_data = data.filter(|item| item.category == stratum)
        let stratum_sample = sample_random(stratum_data, 0.1)
        
        for item in stratum_sample {
          sampled.push(item)
        }
      }
      
      sampled
    }
    
    (sample_random, sample_systematic, sample_stratified)
  }
  
  let (sample_random, sample_systematic, sample_stratified) = data_sampler()
  
  // Test data sampling
  let sample_data = [
    { value: 1, category: "A" },
    { value: 2, category: "A" },
    { value: 3, category: "B" },
    { value: 4, category: "B" },
    { value: 5, category: "C" },
    { value: 6, category: "C" }
  ]
  
  let random_sample = sample_random(sample_data, 0.5)
  assert_eq(random_sample.length(), 3)  // Approximately 50% of data
  
  let systematic_sample = sample_systematic(sample_data, 2)
  assert_eq(systematic_sample.length(), 3)  // Items at indices 0, 2, 4
  
  let stratified_sample = sample_stratified(sample_data, ["A", "B", "C"])
  assert_eq(stratified_sample.length(), 3)  // One from each stratum
}

// Test 10: Telemetry Data Security and Privacy
test "telemetry_data_security_and_privacy" {
  // Test data anonymizer
  let data_anonymizer = || {
    let anonymize_string = |input| {
      // Simple anonymization by replacing sensitive patterns
      let anonymized = input
        .replace(/\b\d{3}-\d{2}-\d{4}\b/g, "XXX-XX-XXXX")  // SSN pattern
        .replace(/\b\d{4}[- ]?\d{4}[- ]?\d{4}[- ]?\d{4}\b/g, "XXXX-XXXX-XXXX-XXXX")  // Credit card pattern
        .replace(/\b[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}\b/g, "email@example.com")  // Email pattern
        .replace(/\b(?:\d{1,3}\.){3}\d{1,3}\b/g, "X.X.X.X")  // IP address pattern
      
      anonymized
    }
    
    let anonymize_json = |json_data| {
      let sensitive_fields = ["password", "token", "secret", "key"]
      
      // Parse JSON and anonymize sensitive fields
      let parsed = parse_json(json_data)
      anonymize_object(parsed, sensitive_fields)
    }
    
    let anonymize_object = |obj, sensitive_fields| {
      let anonymized = {}
      
      for key in obj {
        if sensitive_fields.contains(key) {
          anonymized[key] = "*****"
        } else if obj[key] is Object {
          anonymized[key] = anonymize_object(obj[key], sensitive_fields)
        } else if obj[key] is Array {
          anonymized[key] = anonymize_array(obj[key], sensitive_fields)
        } else {
          anonymized[key] = obj[key]
        }
      }
      
      anonymized
    }
    
    let anonymize_array = |arr, sensitive_fields| {
      let anonymized = []
      
      for item in arr {
        if item is Object {
          anonymized.push(anonymize_object(item, sensitive_fields))
        } else if item is Array {
          anonymized.push(anonymize_array(item, sensitive_fields))
        } else {
          anonymized.push(item)
        }
      }
      
      anonymized
    }
    
    let hash_data = |data| {
      // Simple hash function
      let hash = data.reduce(|acc, char| acc + char.to_int(), 0)
      hash.to_string()
    }
    
    let tokenize = |text| {
      // Simple tokenization
      text.split(/\s+/)
    }
    
    (anonymize_string, anonymize_json, anonymize_object, anonymize_array, hash_data, tokenize)
  }
  
  let (anonymize_str, anonymize_json, anonymize_obj, anonymize_arr, hash_data, tokenize) = data_anonymizer()
  
  // Test data anonymization
  let sensitive_text = "User john.doe@example.com with SSN 123-45-6789 and credit card 1111-2222-3333-4444"
  let anonymized_text = anonymize_str(sensitive_text)
  
  assert_eq(anonymized_text, "User email@example.com with SSN XXX-XX-XXXX and credit card XXXX-XXXX-XXXX-XXXX")
  
  let sensitive_json = "{\"user\":\"john\",\"password\":\"secret123\",\"email\":\"john@example.com\"}"
  let anonymized_json = anonymize_json(sensitive_json)
  assert_true(anonymized_json.contains("\"password\":\"*****\""))
  assert_true(anonymized_json.contains("\"email\":\"email@example.com\""))
  
  // Test data encryption
  let data_encryptor = || {
    let encrypt = |data, key| {
      // Simple XOR encryption
      let mut encrypted = ""
      
      for i = 0; i < data.length(); i = i + 1 {
        let char_code = data[i].to_byte()
        let key_code = key[i % key.length()].to_byte()
        let encrypted_code = char_code ^ key_code
        encrypted = encrypted + String.from_byte_code(encrypted_code)
      }
      
      encrypted
    }
    
    let decrypt = |encrypted_data, key| {
      // Simple XOR decryption (same as encryption)
      encrypt(encrypted_data, key)
    }
    
    let generate_key = |length| {
      let mut key = ""
      
      for i = 0; i < length; i = i + 1 {
        key = key + String.from_byte_code((65 + (i % 26)).to_byte())
      }
      
      key
    }
    
    (encrypt, decrypt, generate_key)
  }
  
  let (encrypt, decrypt, generate_key) = data_encryptor()
  
  // Test data encryption
  let secret_data = "This is a secret message"
  let encryption_key = generate_key(16)
  
  let encrypted_data = encrypt(secret_data, encryption_key)
  let decrypted_data = decrypt(encrypted_data, encryption_key)
  
  assert_eq(decrypted_data, secret_data)
  assert_ne(encrypted_data, secret_data)
  
  // Test access control
  let access_control = || {
    let mut permissions = {}
    let mut roles = {}
    
    let add_role = |role_name, permissions| {
      roles[role_name] = permissions
    }
    
    let assign_role = |user, role| {
      permissions[user] = role
    }
    
    let check_permission = |user, permission| {
      if permissions.contains(user) {
        let user_role = permissions[user]
        
        if roles.contains(user_role) {
          return roles[user_role].contains(permission)
        }
      }
      
      false
    }
    
    let get_user_permissions = |user| {
      if permissions.contains(user) {
        let user_role = permissions[user]
        
        if roles.contains(user_role) {
          return roles[user_role]
        }
      }
      
      []
    }
    
    (add_role, assign_role, check_permission, get_user_permissions)
  }
  
  let (add_role, assign_role, check_permission, get_perms) = access_control()
  
  // Test access control
  add_role("admin", ["read", "write", "delete", "admin"])
  add_role("user", ["read", "write"])
  add_role("viewer", ["read"])
  
  assign_role("alice", "admin")
  assign_role("bob", "user")
  assign_role("charlie", "viewer")
  
  assert_true(check_permission("alice", "delete"))
  assert_true(check_permission("alice", "admin"))
  assert_false(check_permission("bob", "delete"))
  assert_true(check_permission("bob", "read"))
  assert_false(check_permission("charlie", "write"))
  assert_true(check_permission("charlie", "read"))
  
  let alice_perms = get_perms("alice")
  assert_eq(alice_perms.length(), 4)
  
  // Test audit logging
  let audit_logger = || {
    let mut audit_logs = []
    
    let log_access = |user, resource, action, success| {
      let log_entry = {
        timestamp: 1640995200000L,
        user: user,
        resource: resource,
        action: action,
        success: success,
        ip_address: "192.168.1.100"
      }
      
      audit_logs.push(log_entry)
    }
    
    let get_access_logs = |user, resource| {
      let mut filtered = []
      
      for log in audit_logs {
        if (user == null || log.user == user) &&
           (resource == null || log.resource == resource) {
          filtered.push(log)
        }
      }
      
      filtered
    }
    
    let get_failed_access_attempts = |user| {
      let mut failed_attempts = []
      
      for log in audit_logs {
        if log.user == user && !log.success {
          failed_attempts.push(log)
        }
      }
      
      failed_attempts
    }
    
    let get_audit_summary = |time_range| {
      let mut summary = {
        total_access: 0,
        successful_access: 0,
        failed_access: 0,
        unique_users: {}
      }
      
      for log in audit_logs {
        if log.timestamp >= time_range.start && log.timestamp <= time_range.end {
          summary.total_access = summary.total_access + 1
          
          if log.success {
            summary.successful_access = summary.successful_access + 1
          } else {
            summary.failed_access = summary.failed_access + 1
          }
          
          summary.unique_users[log.user] = true
        }
      }
      
      summary.unique_user_count = summary.unique_users.keys().length()
      summary
    }
    
    (log_access, get_access_logs, get_failed_access_attempts, get_audit_summary)
  }
  
  let (log_access, get_access_logs, get_failed_attempts, get_summary) = audit_logger()
  
  // Test audit logging
  log_access("alice", "/api/users", "read", true)
  log_access("bob", "/api/users", "write", true)
  log_access("charlie", "/api/admin", "delete", false)
  log_access("bob", "/api/admin", "read", false)
  
  let alice_logs = get_access_logs("alice", null)
  assert_eq(alice_logs.length(), 1)
  
  let admin_logs = get_access_logs(null, "/api/admin")
  assert_eq(admin_logs.length(), 2)
  
  let bob_failed = get_failed_attempts("bob")
  assert_eq(bob_failed.length(), 1)
  assert_eq(bob_failed[0].resource, "/api/admin")
  
  let time_range = { start: 1640995000000L, end: 1640995400000L }
  let summary = get_summary(time_range)
  assert_eq(summary.total_access, 4)
  assert_eq(summary.successful_access, 2)
  assert_eq(summary.failed_access, 2)
  assert_eq(summary.unique_user_count, 3)
}