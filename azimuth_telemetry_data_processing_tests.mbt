// Azimuth Telemetry Data Processing Tests
// This file contains comprehensive test cases for telemetry data processing

// Test 1: Telemetry Data Collection
test "telemetry data collection and aggregation" {
  type MetricType {
    Counter
    Gauge
    Histogram
    Summary
  }
  
  type MetricValue {
    IntValue(Int)
    FloatValue(Float)
    StringValue(String)
  }
  
  type TelemetryMetric {
    name : String
    metric_type : MetricType
    value : MetricValue
    timestamp : Int
    tags : Array[(String, String)]
  }
  
  type TelemetryData {
    mut metrics : Array[TelemetryMetric]
    mut start_time : Int
    mut end_time : Int
  }
  
  let create_telemetry_data = fn(start_time : Int) : TelemetryData {
    {
      metrics: [],
      start_time: start_time,
      end_time: start_time
    }
  }
  
  let add_metric = fn(data : TelemetryData, name : String, metric_type : MetricType, value : MetricValue, timestamp : Int, tags : Array[(String, String)]) {
    let metric = {
      name: name,
      metric_type: metric_type,
      value: value,
      timestamp: timestamp,
      tags: tags
    }
    
    data.metrics = data.metrics.push(metric)
    
    if timestamp > data.end_time {
      data.end_time = timestamp
    }
  }
  
  let add_counter_metric = fn(data : TelemetryData, name : String, value : Int, timestamp : Int, tags : Array[(String, String)]) {
    add_metric(data, name, MetricType::Counter, MetricValue::IntValue(value), timestamp, tags)
  }
  
  let add_gauge_metric = fn(data : TelemetryData, name : String, value : Float, timestamp : Int, tags : Array[(String, String)]) {
    add_metric(data, name, MetricType::Gauge, MetricValue::FloatValue(value), timestamp, tags)
  }
  
  let add_histogram_metric = fn(data : TelemetryData, name : String, value : Float, timestamp : Int, tags : Array[(String, String)]) {
    add_metric(data, name, MetricType::Histogram, MetricValue::FloatValue(value), timestamp, tags)
  }
  
  let get_metrics_by_name = fn(data : TelemetryData, name : String) : Array[TelemetryMetric] {
    let mut result = []
    for metric in data.metrics {
      if metric.name == name {
        result = result.push(metric)
      }
    }
    result
  }
  
  let get_metrics_by_type = fn(data : TelemetryData, metric_type : MetricType) : Array[TelemetryMetric] {
    let mut result = []
    for metric in data.metrics {
      match metric.metric_type {
        MetricType::Counter => {
          match metric_type {
            MetricType::Counter => result = result.push(metric)
            _ => ()
          }
        }
        MetricType::Gauge => {
          match metric_type {
            MetricType::Gauge => result = result.push(metric)
            _ => ()
          }
        }
        MetricType::Histogram => {
          match metric_type {
            MetricType::Histogram => result = result.push(metric)
            _ => ()
          }
        }
        MetricType::Summary => {
          match metric_type {
            MetricType::Summary => result = result.push(metric)
            _ => ()
          }
        }
      }
    }
    result
  }
  
  let get_metrics_by_tag = fn(data : TelemetryData, tag_key : String, tag_value : String) : Array[TelemetryMetric] {
    let mut result = []
    for metric in data.metrics {
      for (key, value) in metric.tags {
        if key == tag_key && value == tag_value {
          result = result.push(metric)
          break
        }
      }
    }
    result
  }
  
  let aggregate_counter_metrics = fn(metrics : Array[TelemetryMetric]) : Int {
    let mut sum = 0
    for metric in metrics {
      match metric.value {
        MetricValue::IntValue(value) => sum = sum + value
        _ => ()
      }
    }
    sum
  }
  
  let average_gauge_metrics = fn(metrics : Array[TelemetryMetric]) : Float {
    if metrics.length() == 0 {
      return 0.0
    }
    
    let mut sum = 0.0
    for metric in metrics {
      match metric.value {
        MetricValue::FloatValue(value) => sum = sum + value
        _ => ()
      }
    }
    
    sum / metrics.length().to_float()
  }
  
  // Test telemetry data collection
  let telemetry_data = create_telemetry_data(1000)
  
  // Add counter metrics
  add_counter_metric(telemetry_data, "requests_total", 10, 1005, [("method", "GET"), ("status", "200")])
  add_counter_metric(telemetry_data, "requests_total", 5, 1010, [("method", "POST"), ("status", "200")])
  add_counter_metric(telemetry_data, "requests_total", 2, 1015, [("method", "GET"), ("status", "404")])
  
  // Add gauge metrics
  add_gauge_metric(telemetry_data, "memory_usage", 512.5, 1008, [("instance", "server1")])
  add_gauge_metric(telemetry_data, "memory_usage", 620.3, 1013, [("instance", "server1")])
  add_gauge_metric(telemetry_data, "memory_usage", 445.7, 1018, [("instance", "server2")])
  
  // Add histogram metrics
  add_histogram_metric(telemetry_data, "request_duration", 0.1, 1006, [("endpoint", "/api/users")])
  add_histogram_metric(telemetry_data, "request_duration", 0.3, 1011, [("endpoint", "/api/users")])
  add_histogram_metric(telemetry_data, "request_duration", 0.2, 1016, [("endpoint", "/api/posts")])
  
  // Verify data collection
  assert_eq(telemetry_data.metrics.length(), 8)
  assert_eq(telemetry_data.start_time, 1000)
  assert_eq(telemetry_data.end_time, 1018)
  
  // Test filtering by name
  let request_metrics = get_metrics_by_name(telemetry_data, "requests_total")
  assert_eq(request_metrics.length(), 3)
  
  let memory_metrics = get_metrics_by_name(telemetry_data, "memory_usage")
  assert_eq(memory_metrics.length(), 3)
  
  // Test filtering by type
  let counter_metrics = get_metrics_by_type(telemetry_data, MetricType::Counter)
  assert_eq(counter_metrics.length(), 3)
  
  let gauge_metrics = get_metrics_by_type(telemetry_data, MetricType::Gauge)
  assert_eq(gauge_metrics.length(), 3)
  
  let histogram_metrics = get_metrics_by_type(telemetry_data, MetricType::Histogram)
  assert_eq(histogram_metrics.length(), 2)
  
  // Test filtering by tags
  let get_metrics = get_metrics_by_tag(telemetry_data, "method", "GET")
  assert_eq(get_metrics.length(), 2)
  
  let post_metrics = get_metrics_by_tag(telemetry_data, "method", "POST")
  assert_eq(post_metrics.length(), 1)
  
  let server1_metrics = get_metrics_by_tag(telemetry_data, "instance", "server1")
  assert_eq(server1_metrics.length(), 2)
  
  // Test aggregation
  let total_requests = aggregate_counter_metrics(request_metrics)
  assert_eq(total_requests, 17)
  
  let avg_memory = average_gauge_metrics(memory_metrics)
  assert_true(avg_memory > 500.0 && avg_memory < 600.0)
}

// Test 2: Time Series Data Processing
test "time series data processing and analysis" {
  type DataPoint {
    timestamp : Int
    value : Float
  }
  
  type TimeSeries {
    name : String
    mut points : Array[DataPoint]
  }
  
  let create_time_series = fn(name : String) : TimeSeries {
    { name: name, points: [] }
  }
  
  let add_point = fn(series : TimeSeries, timestamp : Int, value : Float) {
    let point = { timestamp: timestamp, value: value }
    series.points = series.points.push(point)
  }
  
  let sort_by_timestamp = fn(series : TimeSeries) {
    // Simple bubble sort for testing
    let n = series.points.length()
    for i in 0..<n {
      for j in 0..<n - i - 1 {
        if series.points[j].timestamp > series.points[j + 1].timestamp {
          let temp = series.points[j]
          series.points[j] = series.points[j + 1]
          series.points[j + 1] = temp
        }
      }
    }
  }
  
  let calculate_average = fn(points : Array[DataPoint]) : Float {
    if points.length() == 0 {
      return 0.0
    }
    
    let mut sum = 0.0
    for point in points {
      sum = sum + point.value
    }
    
    sum / points.length().to_float()
  }
  
  let calculate_min = fn(points : Array[DataPoint]) : Float {
    if points.length() == 0 {
      return 0.0
    }
    
    let mut min_value = points[0].value
    for point in points {
      if point.value < min_value {
        min_value = point.value
      }
    }
    
    min_value
  }
  
  let calculate_max = fn(points : Array[DataPoint]) : Float {
    if points.length() == 0 {
      return 0.0
    }
    
    let mut max_value = points[0].value
    for point in points {
      if point.value > max_value {
        max_value = point.value
      }
    }
    
    max_value
  }
  
  let resample = fn(series : TimeSeries, interval : Int) : TimeSeries {
    if series.points.length() == 0 {
      return create_time_series(series.name + "_resampled")
    }
    
    sort_by_timestamp(series)
    
    let resampled = create_time_series(series.name + "_resampled")
    let start_time = series.points[0].timestamp
    
    let mut current_interval_start = start_time
    let mut current_interval_points = []
    
    for point in series.points {
      if point.timestamp < current_interval_start + interval {
        current_interval_points = current_interval_points.push(point)
      } else {
        if current_interval_points.length() > 0 {
          let avg_value = calculate_average(current_interval_points)
          add_point(resampled, current_interval_start, avg_value)
        }
        
        current_interval_start = current_interval_start + interval
        current_interval_points = [point]
      }
    }
    
    // Add the last interval
    if current_interval_points.length() > 0 {
      let avg_value = calculate_average(current_interval_points)
      add_point(resampled, current_interval_start, avg_value)
    }
    
    resampled
  }
  
  let detect_anomalies = fn(points : Array[DataPoint], threshold : Float) : Array[DataPoint] {
    let mut anomalies = []
    let avg = calculate_average(points)
    
    for point in points {
      if (point.value - avg).abs() > threshold {
        anomalies = anomalies.push(point)
      }
    }
    
    anomalies
  }
  
  // Test time series data processing
  let cpu_usage = create_time_series("cpu_usage")
  
  // Add data points (not in chronological order to test sorting)
  add_point(cpu_usage, 1005, 65.2)
  add_point(cpu_usage, 1000, 45.8)
  add_point(cpu_usage, 1015, 78.3)
  add_point(cpu_usage, 1010, 55.1)
  add_point(cpu_usage, 1020, 82.7)
  add_point(cpu_usage, 1003, 50.4)
  add_point(cpu_usage, 1018, 75.9)
  
  // Test sorting
  sort_by_timestamp(cpu_usage)
  assert_eq(cpu_usage.points[0].timestamp, 1000)
  assert_eq(cpu_usage.points[1].timestamp, 1003)
  assert_eq(cpu_usage.points[2].timestamp, 1005)
  assert_eq(cpu_usage.points[6].timestamp, 1020)
  
  // Test statistical calculations
  let avg_cpu = calculate_average(cpu_usage.points)
  assert_true(avg_cpu > 60.0 && avg_cpu < 70.0)
  
  let min_cpu = calculate_min(cpu_usage.points)
  assert_eq(min_cpu, 45.8)
  
  let max_cpu = calculate_max(cpu_usage.points)
  assert_eq(max_cpu, 82.7)
  
  // Test resampling
  let resampled_cpu = resample(cpu_usage, 10)  // 10-second intervals
  
  assert_eq(resampled_cpu.name, "cpu_usage_resampled")
  assert_true(resampled_cpu.points.length() > 0)
  
  // Verify resampled points are at correct intervals
  assert_eq(resampled_cpu.points[0].timestamp, 1000)
  assert_eq(resampled_cpu.points[1].timestamp, 1010)
  
  // Test anomaly detection
  let anomalies = detect_anomalies(cpu_usage.points, 15.0)
  assert_eq(anomalies.length(), 2)  // 45.8 and 82.7 should be anomalies
}

// Test 3: Telemetry Data Export
test "telemetry data export in different formats" {
  type ExportFormat {
    JSON
    CSV
    Prometheus
  }
  
  type ExportResult {
    Success(String)
    Failure(String)
  }
  
  let export_to_json = fn(data : TelemetryData) : ExportResult {
    let mut json = "{\n"
    json = json + "  \"start_time\": " + data.start_time.to_string() + ",\n"
    json = json + "  \"end_time\": " + data.end_time.to_string() + ",\n"
    json = json + "  \"metrics\": [\n"
    
    for i in 0..<data.metrics.length() {
      let metric = data.metrics[i]
      json = json + "    {\n"
      json = json + "      \"name\": \"" + metric.name + "\",\n"
      
      match metric.metric_type {
        MetricType::Counter => json = json + "      \"type\": \"counter\",\n"
        MetricType::Gauge => json = json + "      \"type\": \"gauge\",\n"
        MetricType::Histogram => json = json + "      \"type\": \"histogram\",\n"
        MetricType::Summary => json = json + "      \"type\": \"summary\",\n"
      }
      
      match metric.value {
        MetricValue::IntValue(value) => json = json + "      \"value\": " + value.to_string() + ",\n"
        MetricValue::FloatValue(value) => json = json + "      \"value\": " + value.to_string() + ",\n"
        MetricValue::StringValue(value) => json = json + "      \"value\": \"" + value + "\",\n"
      }
      
      json = json + "      \"timestamp\": " + metric.timestamp.to_string() + ",\n"
      json = json + "      \"tags\": {"
      
      for j in 0..<metric.tags.length() {
        let (key, value) = metric.tags[j]
        json = json + "\"" + key + "\": \"" + value + "\""
        if j < metric.tags.length() - 1 {
          json = json + ", "
        }
      }
      
      json = json + "}\n"
      json = json + "    }"
      
      if i < data.metrics.length() - 1 {
        json = json + ","
      }
      
      json = json + "\n"
    }
    
    json = json + "  ]\n"
    json = json + "}"
    
    ExportResult::Success(json)
  }
  
  let export_to_csv = fn(data : TelemetryData) : ExportResult {
    let mut csv = "name,type,value,timestamp,tags\n"
    
    for metric in data.metrics {
      csv = csv + metric.name + ","
      
      match metric.metric_type {
        MetricType::Counter => csv = csv + "counter,"
        MetricType::Gauge => csv = csv + "gauge,"
        MetricType::Histogram => csv = csv + "histogram,"
        MetricType::Summary => csv = csv + "summary,"
      }
      
      match metric.value {
        MetricValue::IntValue(value) => csv = csv + value.to_string() + ","
        MetricValue::FloatValue(value) => csv = csv + value.to_string() + ","
        MetricValue::StringValue(value) => csv = csv + value + ","
      }
      
      csv = csv + metric.timestamp.to_string() + ","
      
      for j in 0..<metric.tags.length() {
        let (key, value) = metric.tags[j]
        csv = csv + key + "=" + value
        if j < metric.tags.length() - 1 {
          csv = csv + " "
        }
      }
      
      csv = csv + "\n"
    }
    
    ExportResult::Success(csv)
  }
  
  let export_to_prometheus = fn(data : TelemetryData) : ExportResult {
    let mut prometheus = ""
    
    for metric in data.metrics {
      let mut metric_name = metric.name
      
      // Add tags to the metric name
      for j in 0..<metric.tags.length() {
        let (key, value) = metric.tags[j]
        metric_name = metric_name + "{" + key + "=\"" + value + "\""
        if j == metric.tags.length() - 1 {
          metric_name = metric_name + "}"
        } else {
          metric_name = metric_name + ","
        }
      }
      
      match metric.metric_type {
        MetricType::Counter => {
          match metric.value {
            MetricValue::IntValue(value) => {
              prometheus = prometheus + "# TYPE " + metric.name + " counter\n"
              prometheus = prometheus + metric_name + " " + value.to_string() + " " + metric.timestamp.to_string() + "\n"
            }
            _ => ()
          }
        }
        MetricType::Gauge => {
          match metric.value {
            MetricValue::FloatValue(value) => {
              prometheus = prometheus + "# TYPE " + metric.name + " gauge\n"
              prometheus = prometheus + metric_name + " " + value.to_string() + " " + metric.timestamp.to_string() + "\n"
            }
            _ => ()
          }
        }
        MetricType::Histogram => {
          match metric.value {
            MetricValue::FloatValue(value) => {
              prometheus = prometheus + "# TYPE " + metric.name + " histogram\n"
              prometheus = prometheus + metric_name + "_bucket{le=\"+Inf\"} " + value.to_string() + " " + metric.timestamp.to_string() + "\n"
              prometheus = prometheus + metric_name + "_count " + "1" + " " + metric.timestamp.to_string() + "\n"
              prometheus = prometheus + metric_name + "_sum " + value.to_string() + " " + metric.timestamp.to_string() + "\n"
            }
            _ => ()
          }
        }
        MetricType::Summary => {
          // Simplified summary export
          match metric.value {
            MetricValue::FloatValue(value) => {
              prometheus = prometheus + "# TYPE " + metric.name + " summary\n"
              prometheus = prometheus + metric_name + "{quantile=\"0.5\"} " + value.to_string() + " " + metric.timestamp.to_string() + "\n"
            }
            _ => ()
          }
        }
      }
    }
    
    ExportResult::Success(prometheus)
  }
  
  // Create test telemetry data
  let telemetry_data = create_telemetry_data(1000)
  add_counter_metric(telemetry_data, "requests_total", 100, 1005, [("method", "GET"), ("status", "200")])
  add_gauge_metric(telemetry_data, "memory_usage", 512.5, 1010, [("instance", "server1")])
  
  // Test JSON export
  match export_to_json(telemetry_data) {
    ExportResult::Success(json) => {
      assert_true(json.contains("\"name\": \"requests_total\""))
      assert_true(json.contains("\"type\": \"counter\""))
      assert_true(json.contains("\"value\": 100"))
      assert_true(json.contains("\"name\": \"memory_usage\""))
      assert_true(json.contains("\"type\": \"gauge\""))
      assert_true(json.contains("\"value\": 512.5"))
    }
    ExportResult::Failure(_) => assert_true(false)
  }
  
  // Test CSV export
  match export_to_csv(telemetry_data) {
    ExportResult::Success(csv) => {
      assert_true(csv.contains("name,type,value,timestamp,tags"))
      assert_true(csv.contains("requests_total,counter,100,1005,method=GET status=200"))
      assert_true(csv.contains("memory_usage,gauge,512.5,1010,instance=server1"))
    }
    ExportResult::Failure(_) => assert_true(false)
  }
  
  // Test Prometheus export
  match export_to_prometheus(telemetry_data) {
    ExportResult::Success(prometheus) => {
      assert_true(prometheus.contains("# TYPE requests_total counter"))
      assert_true(prometheus.contains("requests_total{method=\"GET\",status=\"200\"} 100"))
      assert_true(prometheus.contains("# TYPE memory_usage gauge"))
      assert_true(prometheus.contains("memory_usage{instance=\"server1\"} 512.5"))
    }
    ExportResult::Failure(_) => assert_true(false)
  }
}

// Test 4: Telemetry Data Filtering and Querying
test "telemetry data filtering and querying" {
  type QueryFilter {
    NameEquals(String)
    NameContains(String)
    TypeEquals(MetricType)
    TagEquals(String, String)
    TimeRange(Int, Int)
    ValueGreaterThan(Float)
    ValueLessThan(Float)
    And(Array[QueryFilter])
    Or(Array[QueryFilter])
  }
  
  let apply_filter = fn(data : TelemetryData, filter : QueryFilter) : Array[TelemetryMetric] {
    let mut result = []
    
    for metric in data.metrics {
      if matches_filter(metric, filter) {
        result = result.push(metric)
      }
    }
    
    result
  }
  
  let matches_filter = fn(metric : TelemetryMetric, filter : QueryFilter) : Bool {
    match filter {
      QueryFilter::NameEquals(name) => metric.name == name
      QueryFilter::NameContains(substring) => metric.name.contains(substring)
      QueryFilter::TypeEquals(metric_type) => {
        match metric.metric_type {
          MetricType::Counter => match metric_type {
            MetricType::Counter => true
            _ => false
          }
          MetricType::Gauge => match metric_type {
            MetricType::Gauge => true
            _ => false
          }
          MetricType::Histogram => match metric_type {
            MetricType::Histogram => true
            _ => false
          }
          MetricType::Summary => match metric_type {
            MetricType::Summary => true
            _ => false
          }
        }
      }
      QueryFilter::TagEquals(key, value) => {
        for (k, v) in metric.tags {
          if k == key && v == value {
            return true
          }
        }
        false
      }
      QueryFilter::TimeRange(start, end) => metric.timestamp >= start && metric.timestamp <= end
      QueryFilter::ValueGreaterThan(threshold) => {
        match metric.value {
          MetricValue::IntValue(value) => value.to_float() > threshold
          MetricValue::FloatValue(value) => value > threshold
          MetricValue::StringValue(_) => false
        }
      }
      QueryFilter::ValueLessThan(threshold) => {
        match metric.value {
          MetricValue::IntValue(value) => value.to_float() < threshold
          MetricValue::FloatValue(value) => value < threshold
          MetricValue::StringValue(_) => false
        }
      }
      QueryFilter::And(filters) => {
        for filter in filters {
          if !matches_filter(metric, filter) {
            return false
          }
        }
        true
      }
      QueryFilter::Or(filters) => {
        for filter in filters {
          if matches_filter(metric, filter) {
            return true
          }
        }
        false
      }
    }
  }
  
  let group_by_tag = fn(metrics : Array[TelemetryMetric], tag_key : String) -> Array[(String, Array[TelemetryMetric])] {
    let mut groups = []
    let mut processed_tags = []
    
    for metric in metrics {
      let mut tag_value = ""
      
      for (key, value) in metric.tags {
        if key == tag_key {
          tag_value = value
          break
        }
      }
      
      if !processed_tags.contains(tag_value) {
        processed_tags = processed_tags.push(tag_value)
        
        let mut group_metrics = []
        for m in metrics {
          for (key, value) in m.tags {
            if key == tag_key && value == tag_value {
              group_metrics = group_metrics.push(m)
              break
            }
          }
        }
        
        groups = groups.push((tag_value, group_metrics))
      }
    }
    
    groups
  }
  
  // Create test telemetry data
  let telemetry_data = create_telemetry_data(1000)
  
  add_counter_metric(telemetry_data, "http_requests_total", 10, 1005, [("method", "GET"), ("status", "200")])
  add_counter_metric(telemetry_data, "http_requests_total", 5, 1010, [("method", "POST"), ("status", "200")])
  add_counter_metric(telemetry_data, "http_requests_total", 2, 1015, [("method", "GET"), ("status", "404")])
  add_gauge_metric(telemetry_data, "memory_usage", 512.5, 1008, [("instance", "server1")])
  add_gauge_metric(telemetry_data, "memory_usage", 620.3, 1013, [("instance", "server2")])
  add_histogram_metric(telemetry_data, "response_time", 0.1, 1006, [("endpoint", "/api/users")])
  add_histogram_metric(telemetry_data, "response_time", 0.3, 1011, [("endpoint", "/api/posts")])
  
  // Test name filtering
  let http_metrics = apply_filter(telemetry_data, QueryFilter::NameEquals("http_requests_total"))
  assert_eq(http_metrics.length(), 3)
  
  let request_metrics = apply_filter(telemetry_data, QueryFilter::NameContains("request"))
  assert_eq(request_metrics.length(), 3)
  
  // Test type filtering
  let counter_metrics = apply_filter(telemetry_data, QueryFilter::TypeEquals(MetricType::Counter))
  assert_eq(counter_metrics.length(), 3)
  
  let gauge_metrics = apply_filter(telemetry_data, QueryFilter::TypeEquals(MetricType::Gauge))
  assert_eq(gauge_metrics.length(), 2)
  
  // Test tag filtering
  let get_metrics = apply_filter(telemetry_data, QueryFilter::TagEquals("method", "GET"))
  assert_eq(get_metrics.length(), 2)
  
  let server1_metrics = apply_filter(telemetry_data, QueryFilter::TagEquals("instance", "server1"))
  assert_eq(server1_metrics.length(), 1)
  
  // Test time range filtering
  let time_filtered = apply_filter(telemetry_data, QueryFilter::TimeRange(1005, 1010))
  assert_eq(time_filtered.length(), 4)
  
  // Test value filtering
  let high_memory = apply_filter(telemetry_data, QueryFilter::ValueGreaterThan(600.0))
  assert_eq(high_memory.length(), 1)
  
  // Test compound filters
  let get_200 = apply_filter(telemetry_data, QueryFilter::And([
    QueryFilter::TagEquals("method", "GET"),
    QueryFilter::TagEquals("status", "200")
  ]))
  assert_eq(get_200.length(), 1)
  
  let get_or_post = apply_filter(telemetry_data, QueryFilter::Or([
    QueryFilter::TagEquals("method", "GET"),
    QueryFilter::TagEquals("method", "POST")
  ]))
  assert_eq(get_or_post.length(), 3)
  
  // Test grouping
  let grouped_by_status = group_by_tag(http_metrics, "status")
  assert_eq(grouped_by_status.length(), 2)
  
  for (status, metrics) in grouped_by_status {
    if status == "200" {
      assert_eq(metrics.length(), 2)
    } else if status == "404" {
      assert_eq(metrics.length(), 1)
    }
  }
}