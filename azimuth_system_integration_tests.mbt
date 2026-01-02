// Azimuth System Integration Test Suite
// This file contains comprehensive test cases for system integration

// Test 1: End-to-End Telemetry Pipeline
test "end-to-end telemetry pipeline" {
  // Simulate a complete telemetry pipeline from data collection to storage
  
  type TelemetryPoint {
    timestamp : Int
    metric_name : String
    value : Float
    tags : Array[(String, String)]
  }
  
  type TelemetryBuffer {
    points : Array[TelemetryPoint]
    max_size : Int
  }
  
  type AggregatedMetrics {
    metric_name : String
    count : Int
    sum : Float
    min : Float
    max : Float
    timestamp : Int
  }
  
  let create_buffer = fn(max_size) {
    { points: [], max_size: max_size }
  }
  
  let add_point = fn(buffer, point) {
    if buffer.points.length() < buffer.max_size {
      { points: buffer.points @ [point], max_size: buffer.max_size }
    } else {
      // Drop oldest point (FIFO)
      { points: buffer.points[1:] @ [point], max_size: buffer.max_size }
    }
  }
  
  let aggregate_points = fn(points, window_size) {
    let mut metrics = []
    
    // Group by metric name
    let mut grouped_points = []
    let mut processed_metrics = []
    
    for point in points {
      let mut found = false
      
      for i in 0..grouped_points.length() {
        if grouped_points[i].0 == point.metric_name {
          grouped_points[i] = (grouped_points[i].0, grouped_points[i].1 @ [point])
          found = true
          break
        }
      }
      
      if not found {
        grouped_points = grouped_points @ [(point.metric_name, [point])]
      }
    }
    
    // Calculate aggregates for each metric
    for (metric_name, metric_points) in grouped_points {
      if metric_points.length() >= window_size {
        let mut count = 0
        let mut sum = 0.0
        let mut min = metric_points[0].value
        let mut max = metric_points[0].value
        
        for point in metric_points {
          count = count + 1
          sum = sum + point.value
          
          if point.value < min {
            min = point.value
          }
          
          if point.value > max {
            max = point.value
          }
        }
        
        metrics = metrics @ [{
          metric_name: metric_name,
          count: count,
          sum: sum,
          min: min,
          max: max,
          timestamp: metric_points[metric_points.length() - 1].timestamp
        }]
      }
    }
    
    metrics
  }
  
  let serialize_metrics = fn(metrics) {
    let mut result = ""
    
    for metric in metrics {
      let line = metric.metric_name + " " + 
                 metric.count.to_string() + " " + 
                 metric.sum.to_string() + " " + 
                 metric.min.to_string() + " " + 
                 metric.max.to_string() + " " + 
                 metric.timestamp.to_string()
      
      result = result + line + "\n"
    }
    
    result
  }
  
  // Create telemetry pipeline
  let buffer = create_buffer(100)
  
  // Add telemetry points
  let buffer1 = add_point(buffer, {
    timestamp: 1000,
    metric_name: "cpu.usage",
    value: 45.2,
    tags: [("host", "server1"), ("region", "us-east")]
  })
  
  let buffer2 = add_point(buffer1, {
    timestamp: 1005,
    metric_name: "memory.usage",
    value: 67.8,
    tags: [("host", "server1"), ("region", "us-east")]
  })
  
  let buffer3 = add_point(buffer2, {
    timestamp: 1010,
    metric_name: "cpu.usage",
    value: 48.1,
    tags: [("host", "server1"), ("region", "us-east")]
  })
  
  let buffer4 = add_point(buffer3, {
    timestamp: 1015,
    metric_name: "disk.usage",
    value: 23.4,
    tags: [("host", "server1"), ("region", "us-east")]
  })
  
  let buffer5 = add_point(buffer4, {
    timestamp: 1020,
    metric_name: "cpu.usage",
    value: 47.9,
    tags: [("host", "server1"), ("region", "us-east")]
  })
  
  // Aggregate metrics
  let aggregated = aggregate_points(buffer5.points, 3)
  
  assert_eq(aggregated.length(), 1)  // Only cpu.usage has 3+ points
  
  // Verify aggregation
  assert_eq(aggregated[0].metric_name, "cpu.usage")
  assert_eq(aggregated[0].count, 3)
  assert_true(aggregated[0].sum > 140.0 and aggregated[0].sum < 142.0)
  assert_eq(aggregated[0].min, 45.2)
  assert_eq(aggregated[0].max, 48.1)
  
  // Serialize metrics
  let serialized = serialize_metrics(aggregated)
  
  assert_true(serialized.contains("cpu.usage"))
  assert_true(serialized.contains("3"))
  assert_true(serialized.contains("47.9"))  // Last timestamp
}

// Test 2: Distributed Tracing Integration
test "distributed tracing integration" {
  // Simulate distributed tracing across multiple services
  
  type Span {
    trace_id : String
    span_id : String
    parent_span_id : Option[String]
    operation_name : String
    start_time : Int
    end_time : Int
    tags : Array[(String, String)]
  }
  
  type TraceCollector {
    spans : Array[Span]
  }
  
  let create_collector = fn() {
    { spans: [] }
  }
  
  let start_span = fn(collector, trace_id, span_id, parent_span_id, operation_name, tags) {
    let span = {
      trace_id: trace_id,
      span_id: span_id,
      parent_span_id: parent_span_id,
      operation_name: operation_name,
      start_time: 1000,
      end_time: 0,
      tags: tags
    }
    
    { spans: collector.spans @ [span] }
  }
  
  let finish_span = fn(collector, span_id, end_time) {
    let mut updated_spans = []
    
    for span in collector.spans {
      if span.span_id == span_id {
        updated_spans = updated_spans @ [{
          trace_id: span.trace_id,
          span_id: span.span_id,
          parent_span_id: span.parent_span_id,
          operation_name: span.operation_name,
          start_time: span.start_time,
          end_time: end_time,
          tags: span.tags
        }]
      } else {
        updated_spans = updated_spans @ [span]
      }
    }
    
    { spans: updated_spans }
  }
  
  let get_trace_by_id = fn(collector, trace_id) {
    let mut trace_spans = []
    
    for span in collector.spans {
      if span.trace_id == trace_id {
        trace_spans = trace_spans @ [span]
      }
    }
    
    trace_spans
  }
  
  let calculate_trace_duration = fn(trace_spans) {
    if trace_spans.length() == 0 {
      0
    } else {
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
  }
  
  let build_trace_tree = fn(trace_spans) {
    let mut root_spans = []
    let mut child_spans = []
    
    // Separate root and child spans
    for span in trace_spans {
      match span.parent_span_id {
        None => root_spans = root_spans @ [span]
        Some(_) => child_spans = child_spans @ [span]
      }
    }
    
    // Build tree structure (simplified)
    { roots: root_spans, children: child_spans }
  }
  
  // Create distributed trace
  let collector = create_collector()
  
  // Start root span (API Gateway)
  let collector1 = start_span(collector, "trace-123", "span-1", None, "api.request", [
    ("service", "api-gateway"),
    ("http.method", "GET"),
    ("http.path", "/api/users")
  ])
  
  // Start child span (User Service)
  let collector2 = start_span(collector1, "trace-123", "span-2", Some("span-1"), "user.service", [
    ("service", "user-service"),
    ("db.query", "SELECT * FROM users")
  ])
  
  // Start another child span (Auth Service)
  let collector3 = start_span(collector2, "trace-123", "span-3", Some("span-1"), "auth.service", [
    ("service", "auth-service"),
    ("auth.method", "jwt")
  ])
  
  // Finish spans
  let collector4 = finish_span(collector3, "span-2", 1050)
  let collector5 = finish_span(collector4, "span-3", 1030)
  let collector6 = finish_span(collector5, "span-1", 1100)
  
  // Get trace by ID
  let trace_spans = get_trace_by_id(collector6, "trace-123")
  
  assert_eq(trace_spans.length(), 3)
  
  // Verify trace structure
  let mut root_span_found = false
  let mut child_spans_found = 0
  
  for span in trace_spans {
    if span.parent_span_id == None {
      root_span_found = true
      assert_eq(span.operation_name, "api.request")
    } else {
      child_spans_found = child_spans_found + 1
    }
  }
  
  assert_true(root_span_found)
  assert_eq(child_spans_found, 2)
  
  // Calculate trace duration
  let duration = calculate_trace_duration(trace_spans)
  assert_eq(duration, 100)  // 1100 - 1000
  
  // Build trace tree
  let trace_tree = build_trace_tree(trace_spans)
  assert_eq(trace_tree.roots.length(), 1)
  assert_eq(trace_tree.children.length(), 2)
}

// Test 3: Configuration and Resource Management Integration
test "configuration and resource management integration" {
  // Test integration between configuration system and resource management
  
  type ConfigValue {
    string_value : Option[String]
    int_value : Option[Int]
    float_value : Option[Float]
  }
  
  type Configuration {
    values : Array[(String, ConfigValue)]
  }
  
  type Resource {
    id : Int
    name : String
    type : String
    config : Configuration
    is_active : Bool
  }
  
  type ResourceManager {
    resources : Array[Resource]
    next_id : Int
  }
  
  let create_resource_manager = fn() {
    { resources: [], next_id: 1 }
  }
  
  let set_config_value = fn(config, key, string_value, int_value, float_value) {
    let config_value = {
      string_value: string_value,
      int_value: int_value,
      float_value: float_value
    }
    
    let mut updated_values = []
    let mut found = false
    
    for (k, v) in config.values {
      if k == key {
        updated_values = updated_values @ [(k, config_value)]
        found = true
      } else {
        updated_values = updated_values @ [(k, v)]
      }
    }
    
    if not found {
      updated_values = updated_values @ [(key, config_value)]
    }
    
    { values: updated_values }
  }
  
  let get_config_string = fn(config, key, default_value) {
    for (k, v) in config.values {
      if k == key {
        match v.string_value {
          Some(value) => return value
          None => ()
        }
      }
    }
    default_value
  }
  
  let get_config_int = fn(config, key, default_value) {
    for (k, v) in config.values {
      if k == key {
        match v.int_value {
          Some(value) => return value
          None => ()
        }
      }
    }
    default_value
  }
  
  let create_resource = fn(manager, name, resource_type, config) {
    let resource = {
      id: manager.next_id,
      name: name,
      type: resource_type,
      config: config,
      is_active: false
    }
    
    {
      resources: manager.resources @ [resource],
      next_id: manager.next_id + 1
    }
  }
  
  let activate_resource = fn(manager, resource_id) {
    let mut updated_resources = []
    
    for resource in manager.resources {
      if resource.id == resource_id {
        // Check if resource has valid configuration
        let required_config = match resource.type {
          "database" => ["host", "port", "username"],
          "cache" => ["host", "port"],
          "api" => ["port", "timeout"],
          _ => []
        }
        
        let mut valid_config = true
        
        for config_key in required_config {
          let value = get_config_string(resource.config, config_key, "")
          if value == "" {
            valid_config = false
            break
          }
        }
        
        if valid_config {
          updated_resources = updated_resources @ [{
            id: resource.id,
            name: resource.name,
            type: resource.type,
            config: resource.config,
            is_active: true
          }]
        } else {
          updated_resources = updated_resources @ [resource]
        }
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    { resources: updated_resources, next_id: manager.next_id }
  }
  
  // Test integration
  let manager = create_resource_manager()
  
  // Create database configuration
  let db_config = set_config_value(
    set_config_value(
      set_config_value({ values: [] }, "host", Some("localhost"), None, None),
      "port", None, Some(5432), None
    ),
    "username", Some("admin"), None, None
  )
  
  // Create cache configuration (missing port)
  let cache_config = set_config_value(
    { values: [] },
    "host", Some("localhost"), None, None
  )
  
  // Create API configuration
  let api_config = set_config_value(
    set_config_value({ values: [] }, "port", None, Some(8080), None),
    "timeout", None, Some(30), None
  )
  
  // Create resources
  let manager1 = create_resource(manager, "main-db", "database", db_config)
  let manager2 = create_resource(manager1, "main-cache", "cache", cache_config)
  let manager3 = create_resource(manager2, "main-api", "api", api_config)
  
  assert_eq(manager3.resources.length(), 3)
  
  // Activate resources
  let manager4 = activate_resource(manager3, 1)  // Database
  let manager5 = activate_resource(manager4, 2)  // Cache
  let manager6 = activate_resource(manager5, 3)  // API
  
  // Check activation status
  let mut db_active = false
  let mut cache_active = false
  let mut api_active = false
  
  for resource in manager6.resources {
    if resource.id == 1 {
      db_active = resource.is_active
    } else if resource.id == 2 {
      cache_active = resource.is_active
    } else if resource.id == 3 {
      api_active = resource.is_active
    }
  }
  
  assert_true(db_active)   // Has valid config
  assert_false(cache_active)  // Missing port
  assert_true(api_active)  // Has valid config
}

// Test 4: Network and Telemetry Integration
test "network and telemetry integration" {
  // Test integration between network communication and telemetry collection
  
  type NetworkRequest {
    method : String
    url : String
    headers : Array[(String, String)]
    body : String
    timestamp : Int
  }
  
  type NetworkResponse {
    status_code : Int
    headers : Array[(String, String)]
    body : String
    timestamp : Int
  }
  
  type TelemetryMetric {
    name : String
    value : Float
    timestamp : Int
    tags : Array[(String, String)]
  }
  
  type NetworkTelemetryCollector {
    requests : Array[NetworkRequest]
    responses : Array[NetworkResponse]
    metrics : Array[TelemetryMetric]
  }
  
  let create_collector = fn() {
    { 
      requests: [], 
      responses: [], 
      metrics: [] 
    }
  }
  
  let record_request = fn(collector, method, url, headers, body) {
    let request = {
      method: method,
      url: url,
      headers: headers,
      body: body,
      timestamp: 1000
    }
    
    {
      requests: collector.requests @ [request],
      responses: collector.responses,
      metrics: collector.metrics
    }
  }
  
  let record_response = fn(collector, status_code, headers, body) {
    let response = {
      status_code: status_code,
      headers: headers,
      body: body,
      timestamp: 1050
    }
    
    {
      requests: collector.requests,
      responses: collector.responses @ [response],
      metrics: collector.metrics
    }
  }
  
  let generate_metrics = fn(collector) {
    let mut new_metrics = []
    
    // Generate request count metric
    new_metrics = new_metrics @ [{
      name: "http.requests.total",
      value: collector.requests.length().to_float(),
      timestamp: 1100,
      tags: []
    }]
    
    // Generate response status metrics
    let mut status_codes = []
    
    for response in collector.responses {
      let mut found = false
      
      for i in 0..status_codes.length() {
        if status_codes[i].0 == response.status_code {
          status_codes[i] = (response.status_code, status_codes[i].1 + 1)
          found = true
          break
        }
      }
      
      if not found {
        status_codes = status_codes @ [(response.status_code, 1)]
      }
    }
    
    for (status_code, count) in status_codes {
      new_metrics = new_metrics @ [{
        name: "http.responses.by_status",
        value: count.to_float(),
        timestamp: 1100,
        tags: [("status_code", status_code.to_string())]
      }]
    }
    
    // Generate latency metrics
    if collector.requests.length() > 0 and collector.responses.length() > 0 {
      let latency = collector.responses[0].timestamp - collector.requests[0].timestamp
      
      new_metrics = new_metrics @ [{
        name: "http.request.latency",
        value: latency.to_float(),
        timestamp: 1100,
        tags: [("url", collector.requests[0].url)]
      }]
    }
    
    {
      requests: collector.requests,
      responses: collector.responses,
      metrics: collector.metrics @ new_metrics
    }
  }
  
  // Test integration
  let collector = create_collector()
  
  // Record HTTP request
  let collector1 = record_request(
    collector,
    "GET",
    "/api/users",
    [("Authorization", "Bearer token123")],
    ""
  )
  
  // Record HTTP response
  let collector2 = record_response(
    collector1,
    200,
    [("Content-Type", "application/json")],
    "{\"users\": []}"
  )
  
  // Generate metrics
  let collector3 = generate_metrics(collector2)
  
  // Verify recorded data
  assert_eq(collector3.requests.length(), 1)
  assert_eq(collector3.responses.length(), 1)
  assert_eq(collector3.metrics.length(), 3)  // request count, status code, latency
  
  // Verify metrics
  let mut request_count_metric = None
  let mut status_code_metric = None
  let mut latency_metric = None
  
  for metric in collector3.metrics {
    match metric.name {
      "http.requests.total" => request_count_metric = Some(metric)
      "http.responses.by_status" => status_code_metric = Some(metric)
      "http.request.latency" => latency_metric = Some(metric)
      _ => ()
    }
  }
  
  match request_count_metric {
    Some(metric) => assert_eq(metric.value, 1.0)
    None => assert_true(false)
  }
  
  match status_code_metric {
    Some(metric) => {
      assert_eq(metric.value, 1.0)
      assert_eq(metric.tags[0], ("status_code", "200"))
    }
    None => assert_true(false)
  }
  
  match latency_metric {
    Some(metric) => {
      assert_eq(metric.value, 50.0)  // 1050 - 1000
      assert_eq(metric.tags[0], ("url", "/api/users"))
    }
    None => assert_true(false)
  }
}

// Test 5: Time Series and Alerting Integration
test "time series and alerting integration" {
  // Test integration between time series data and alerting system
  
  type TimeSeriesPoint {
    timestamp : Int
    value : Float
  }
  
  type AlertRule {
    name : String
    metric : String
    condition : String  // "gt", "lt", "eq"
    threshold : Float
    duration : Int  // seconds
    severity : String
  }
  
  type Alert {
    rule_name : String
    metric : String
    value : Float
    threshold : Float
    triggered_at : Int
    severity : String
  }
  
  type MonitoringSystem {
    time_series : Array[(String, Array[TimeSeriesPoint])]
    rules : Array[AlertRule]
    alerts : Array[Alert]
  }
  
  let create_monitoring_system = fn() {
    { 
      time_series: [], 
      rules: [], 
      alerts: [] 
    }
  }
  
  let add_time_series_point = fn(system, metric_name, timestamp, value) {
    let point = { timestamp: timestamp, value: value }
    let mut updated_series = []
    let mut found = false
    
    for (name, points) in system.time_series {
      if name == metric_name {
        updated_series = updated_series @ [(name, points @ [point])]
        found = true
      } else {
        updated_series = updated_series @ [(name, points)]
      }
    }
    
    if not found {
      updated_series = updated_series @ [(metric_name, [point])]
    }
    
    { 
      time_series: updated_series, 
      rules: system.rules, 
      alerts: system.alerts 
    }
  }
  
  let add_alert_rule = fn(system, name, metric, condition, threshold, duration, severity) {
    let rule = {
      name: name,
      metric: metric,
      condition: condition,
      threshold: threshold,
      duration: duration,
      severity: severity
    }
    
    { 
      time_series: system.time_series, 
      rules: system.rules @ [rule], 
      alerts: system.alerts 
    }
  }
  
  let evaluate_rules = fn(system) {
    let mut new_alerts = []
    
    for rule in system.rules {
      // Find time series for this metric
      let mut series_points = []
      
      for (metric_name, points) in system.time_series {
        if metric_name == rule.metric {
          series_points = points
          break
        }
      }
      
      if series_points.length() > 0 {
        // Get the latest point
        let latest_point = series_points[series_points.length() - 1]
        
        // Check condition
        let condition_met = match rule.condition {
          "gt" => latest_point.value > rule.threshold,
          "lt" => latest_point.value < rule.threshold,
          "eq" => latest_point.value == rule.threshold,
          _ => false
        }
        
        if condition_met {
          // Check if alert already exists for this rule
          let mut alert_exists = false
          
          for alert in system.alerts {
            if alert.rule_name == rule.name {
              alert_exists = true
              break
            }
          }
          
          if not alert_exists {
            new_alerts = new_alerts @ [{
              rule_name: rule.name,
              metric: rule.metric,
              value: latest_point.value,
              threshold: rule.threshold,
              triggered_at: latest_point.timestamp,
              severity: rule.severity
            }]
          }
        }
      }
    }
    
    { 
      time_series: system.time_series, 
      rules: system.rules, 
      alerts: system.alerts @ new_alerts 
    }
  }
  
  // Test integration
  let system = create_monitoring_system()
  
  // Add alert rules
  let system1 = add_alert_rule(
    system,
    "high_cpu",
    "cpu.usage",
    "gt",
    80.0,
    300,
    "warning"
  )
  
  let system2 = add_alert_rule(
    system1,
    "low_memory",
    "memory.usage",
    "lt",
    20.0,
    300,
    "critical"
  )
  
  // Add time series data
  let system3 = add_time_series_point(system2, "cpu.usage", 1000, 75.0)
  let system4 = add_time_series_point(system3, "cpu.usage", 1010, 78.0)
  let system5 = add_time_series_point(system4, "cpu.usage", 1020, 82.0)  // Should trigger alert
  let system6 = add_time_series_point(system5, "memory.usage", 1000, 25.0)
  let system7 = add_time_series_point(system6, "memory.usage", 1010, 22.0)
  let system8 = add_time_series_point(system7, "memory.usage", 1020, 18.0)  // Should trigger alert
  
  // Evaluate rules
  let system9 = evaluate_rules(system8)
  
  // Verify alerts
  assert_eq(system9.alerts.length(), 2)
  
  // Verify CPU alert
  let mut cpu_alert = None
  let mut memory_alert = None
  
  for alert in system9.alerts {
    if alert.rule_name == "high_cpu" {
      cpu_alert = Some(alert)
    } else if alert.rule_name == "low_memory" {
      memory_alert = Some(alert)
    }
  }
  
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.metric, "cpu.usage")
      assert_eq(alert.value, 82.0)
      assert_eq(alert.threshold, 80.0)
      assert_eq(alert.severity, "warning")
    }
    None => assert_true(false)
  }
  
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.metric, "memory.usage")
      assert_eq(alert.value, 18.0)
      assert_eq(alert.threshold, 20.0)
      assert_eq(alert.severity, "critical")
    }
    None => assert_true(false)
  }
}

// Test 6: Serialization and Network Integration
test "serialization and network integration" {
  // Test integration between data serialization and network communication
  
  type UserData {
    id : Int
    name : String
    email : String
  }
  
  type NetworkMessage {
    headers : Array[(String, String)]
    body : String
  }
  
  let serialize_user = fn(user) {
    // Simulate JSON serialization
    "{" +
    "\"id\":" + user.id.to_string() + "," +
    "\"name\":\"" + user.name + "\"," +
    "\"email\":\"" + user.email + "\"" +
    "}"
  }
  
  let deserialize_user = fn(json_string) {
    // Simulate JSON deserialization (simplified)
    let mut id = 0
    let mut name = ""
    let mut email = ""
    
    // Simple parsing for demonstration
    if json_string.contains("\"id\":") {
      let id_start = json_string.index_of("\"id\":") + 5
      let id_end = json_string.index_of(",", id_start)
      id = json_string.substring(id_start, id_end).to_int()
    }
    
    if json_string.contains("\"name\":\"") {
      let name_start = json_string.index_of("\"name\":\"") + 8
      let name_end = json_string.index_of("\"", name_start)
      name = json_string.substring(name_start, name_end)
    }
    
    if json_string.contains("\"email\":\"") {
      let email_start = json_string.index_of("\"email\":\"") + 9
      let email_end = json_string.index_of("\"", email_start)
      email = json_string.substring(email_start, email_end)
    }
    
    { id: id, name: name, email: email }
  }
  
  let create_request = fn(user) {
    let serialized_user = serialize_user(user)
    let message = {
      headers: [
        ("Content-Type", "application/json"),
        ("Content-Length", serialized_user.length().to_string())
      ],
      body: serialized_user
    }
    message
  }
  
  let send_request = fn(message) {
    // Simulate network send
    message
  }
  
  let receive_response = fn() {
    // Simulate network receive
    {
      headers: [
        ("Content-Type", "application/json"),
        ("Status", "200 OK")
      ],
      body: "{\"status\":\"success\",\"user_id\":123}"
    }
  }
  
  let process_response = fn(response) {
    // Simulate response processing
    if response.headers.contains(("Status", "200 OK")) {
      "Success"
    } else {
      "Failed"
    }
  }
  
  // Test integration
  let user = { id: 1, name: "John Doe", email: "john@example.com" }
  
  // Serialize user
  let serialized = serialize_user(user)
  assert_true(serialized.contains("\"id\":1"))
  assert_true(serialized.contains("\"name\":\"John Doe\""))
  assert_true(serialized.contains("\"email\":\"john@example.com\""))
  
  // Deserialize user
  let deserialized_user = deserialize_user(serialized)
  assert_eq(deserialized_user.id, 1)
  assert_eq(deserialized_user.name, "John Doe")
  assert_eq(deserialized_user.email, "john@example.com")
  
  // Create network request
  let request = create_request(user)
  assert_eq(request.headers[0], ("Content-Type", "application/json"))
  assert_eq(request.body, serialized)
  
  // Send request
  let sent_message = send_request(request)
  assert_eq(sent_message.body, serialized)
  
  // Receive response
  let response = receive_response()
  assert_eq(response.headers[1], ("Status", "200 OK"))
  
  // Process response
  let result = process_response(response)
  assert_eq(result, "Success")
}

// Test 7: Concurrent Operations and Resource Management Integration
test "concurrent operations and resource management integration" {
  // Test integration between concurrent operations and resource management
  
  type Resource {
    id : Int
    name : String
    is_locked : Bool
    lock_owner : Option[Int]
    data : String
  }
  
  type Operation {
    id : Int
    thread_id : Int
    resource_id : Int
    operation_type : String
    timestamp : Int
  }
  
  type ConcurrentResourceManager {
    resources : Array[Resource]
    operations : Array[Operation]
    next_operation_id : Int
  }
  
  let create_manager = fn() {
    { 
      resources: [], 
      operations: [], 
      next_operation_id: 1 
    }
  }
  
  let add_resource = fn(manager, id, name, data) {
    let resource = {
      id: id,
      name: name,
      is_locked: false,
      lock_owner: None,
      data: data
    }
    
    { 
      resources: manager.resources @ [resource], 
      operations: manager.operations, 
      next_operation_id: manager.next_operation_id 
    }
  }
  
  let acquire_lock = fn(manager, thread_id, resource_id) {
    let mut updated_resources = []
    let mut lock_acquired = false
    
    for resource in manager.resources {
      if resource.id == resource_id and not resource.is_locked {
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_locked: true,
          lock_owner: Some(thread_id),
          data: resource.data
        }]
        lock_acquired = true
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    let operation = {
      id: manager.next_operation_id,
      thread_id: thread_id,
      resource_id: resource_id,
      operation_type: if lock_acquired { "lock_acquired" } else { "lock_failed" },
      timestamp: 1000
    }
    
    { 
      resources: updated_resources, 
      operations: manager.operations @ [operation], 
      next_operation_id: manager.next_operation_id + 1 
    }
  }
  
  let release_lock = fn(manager, thread_id, resource_id) {
    let mut updated_resources = []
    let mut lock_released = false
    
    for resource in manager.resources {
      if resource.id == resource_id and 
         resource.is_locked and 
         resource.lock_owner == Some(thread_id) {
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_locked: false,
          lock_owner: None,
          data: resource.data
        }]
        lock_released = true
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    let operation = {
      id: manager.next_operation_id,
      thread_id: thread_id,
      resource_id: resource_id,
      operation_type: if lock_released { "lock_released" } else { "lock_release_failed" },
      timestamp: 1000
    }
    
    { 
      resources: updated_resources, 
      operations: manager.operations @ [operation], 
      next_operation_id: manager.next_operation_id + 1 
    }
  }
  
  let modify_resource = fn(manager, thread_id, resource_id, new_data) {
    let mut updated_resources = []
    let mut modification_successful = false
    
    for resource in manager.resources {
      if resource.id == resource_id and 
         resource.is_locked and 
         resource.lock_owner == Some(thread_id) {
        updated_resources = updated_resources @ [{
          id: resource.id,
          name: resource.name,
          is_locked: resource.is_locked,
          lock_owner: resource.lock_owner,
          data: new_data
        }]
        modification_successful = true
      } else {
        updated_resources = updated_resources @ [resource]
      }
    }
    
    let operation = {
      id: manager.next_operation_id,
      thread_id: thread_id,
      resource_id: resource_id,
      operation_type: if modification_successful { "modify_success" } else { "modify_failed" },
      timestamp: 1000
    }
    
    { 
      resources: updated_resources, 
      operations: manager.operations @ [operation], 
      next_operation_id: manager.next_operation_id + 1 
    }
  }
  
  let get_operation_history = fn(manager, resource_id) {
    let mut history = []
    
    for operation in manager.operations {
      if operation.resource_id == resource_id {
        history = history @ [operation]
      }
    }
    
    history
  }
  
  // Test integration
  let manager = create_manager()
  
  // Add resources
  let manager1 = add_resource(add_resource(manager, 1, "resource1", "data1"), 2, "resource2", "data2")
  
  // Thread 1 acquires lock on resource 1
  let manager2 = acquire_lock(manager1, 1, 1)
  
  // Thread 2 tries to acquire lock on resource 1 (should fail)
  let manager3 = acquire_lock(manager2, 2, 1)
  
  // Thread 1 modifies resource 1
  let manager4 = modify_resource(manager3, 1, 1, "data1_modified")
  
  // Thread 1 releases lock on resource 1
  let manager5 = release_lock(manager4, 1, 1)
  
  // Thread 2 acquires lock on resource 1 (should succeed)
  let manager6 = acquire_lock(manager5, 2, 1)
  
  // Thread 2 modifies resource 1
  let manager7 = modify_resource(manager6, 2, 1, "data1_modified_by_thread2")
  
  // Thread 2 releases lock on resource 1
  let manager8 = release_lock(manager7, 2, 1)
  
  // Get operation history for resource 1
  let history = get_operation_history(manager8, 1)
  
  assert_eq(history.length(), 7)
  
  // Verify operation sequence
  assert_eq(history[0].operation_type, "lock_acquired")
  assert_eq(history[0].thread_id, 1)
  
  assert_eq(history[1].operation_type, "lock_failed")
  assert_eq(history[1].thread_id, 2)
  
  assert_eq(history[2].operation_type, "modify_success")
  assert_eq(history[2].thread_id, 1)
  
  assert_eq(history[3].operation_type, "lock_released")
  assert_eq(history[3].thread_id, 1)
  
  assert_eq(history[4].operation_type, "lock_acquired")
  assert_eq(history[4].thread_id, 2)
  
  assert_eq(history[5].operation_type, "modify_success")
  assert_eq(history[5].thread_id, 2)
  
  assert_eq(history[6].operation_type, "lock_released")
  assert_eq(history[6].thread_id, 2)
  
  // Verify final resource state
  let mut final_resource = None
  for resource in manager8.resources {
    if resource.id == 1 {
      final_resource = Some(resource)
      break
    }
  }
  
  match final_resource {
    Some(resource) => {
      assert_false(resource.is_locked)
      assert_eq(resource.lock_owner, None)
      assert_eq(resource.data, "data1_modified_by_thread2")
    }
    None => assert_true(false)
  }
}

// Test 8: Error Handling and System Integration
test "error handling and system integration" {
  // Test integration between error handling and various system components
  
  type Error {
    code : String
    message : String
    component : String
    timestamp : Int
    context : Array[(String, String)]
  }
  
  type SystemComponent {
    name : String
    status : String
    errors : Array[Error]
  }
  
  type IntegratedSystem {
    components : Array[SystemComponent]
    global_errors : Array[Error]
  }
  
  let create_system = fn() {
    { 
      components: [], 
      global_errors: [] 
    }
  }
  
  let add_component = fn(system, name) {
    let component = {
      name: name,
      status: "active",
      errors: []
    }
    
    { 
      components: system.components @ [component], 
      global_errors: system.global_errors 
    }
  }
  
  let report_error = fn(system, component_name, code, message, context) {
    let error = {
      code: code,
      message: message,
      component: component_name,
      timestamp: 1000,
      context: context
    }
    
    let mut updated_components = []
    let mut component_found = false
    
    for component in system.components {
      if component.name == component_name {
        component_found = true
        updated_components = updated_components @ [{
          name: component.name,
          status: "error",
          errors: component.errors @ [error]
        }]
      } else {
        updated_components = updated_components @ [component]
      }
    }
    
    if component_found {
      { 
        components: updated_components, 
        global_errors: system.global_errors @ [error] 
      }
    } else {
      system  // Component not found
    }
  }
  
  let recover_component = fn(system, component_name) {
    let mut updated_components = []
    let mut component_found = false
    
    for component in system.components {
      if component.name == component_name {
        component_found = true
        updated_components = updated_components @ [{
          name: component.name,
          status: "active",
          errors: []
        }]
      } else {
        updated_components = updated_components @ [component]
      }
    }
    
    if component_found {
      { 
        components: updated_components, 
        global_errors: system.global_errors 
      }
    } else {
      system  // Component not found
    }
  }
  
  let get_system_health = fn(system) {
    let mut active_components = 0
    let mut error_components = 0
    
    for component in system.components {
      match component.status {
        "active" => active_components = active_components + 1
        "error" => error_components = error_components + 1
        _ => ()
      }
    }
    
    {
      total_components: system.components.length(),
      active_components: active_components,
      error_components: error_components,
      global_errors: system.global_errors.length()
    }
  }
  
  let get_errors_by_component = fn(system, component_name) {
    let mut component_errors = []
    
    for component in system.components {
      if component.name == component_name {
        component_errors = component.errors
        break
      }
    }
    
    component_errors
  }
  
  // Test integration
  let system = create_system()
  
  // Add components
  let system1 = add_component(add_component(add_component(system, "database"), "cache"), "api")
  
  // Report errors
  let system2 = report_error(
    system1,
    "database",
    "DB_CONN_FAILED",
    "Database connection failed",
    [("host", "localhost"), ("port", "5432")]
  )
  
  let system3 = report_error(
    system2,
    "cache",
    "CACHE_TIMEOUT",
    "Cache operation timed out",
    [("operation", "get"), ("key", "user123")]
  )
  
  // Check system health
  let health = get_system_health(system3)
  assert_eq(health.total_components, 3)
  assert_eq(health.active_components, 1)
  assert_eq(health.error_components, 2)
  assert_eq(health.global_errors, 2)
  
  // Get errors by component
  let db_errors = get_errors_by_component(system3, "database")
  let cache_errors = get_errors_by_component(system3, "cache")
  let api_errors = get_errors_by_component(system3, "api")
  
  assert_eq(db_errors.length(), 1)
  assert_eq(cache_errors.length(), 1)
  assert_eq(api_errors.length(), 0)
  
  // Verify error details
  assert_eq(db_errors[0].code, "DB_CONN_FAILED")
  assert_eq(db_errors[0].component, "database")
  assert_eq(db_errors[0].context.length(), 2)
  
  // Recover database component
  let system4 = recover_component(system3, "database")
  
  // Check system health after recovery
  let health2 = get_system_health(system4)
  assert_eq(health2.active_components, 2)
  assert_eq(health2.error_components, 1)
  
  // Verify database component is recovered
  let db_errors2 = get_errors_by_component(system4, "database")
  assert_eq(db_errors2.length(), 0)
}

// Test 9: Performance Monitoring and Resource Integration
test "performance monitoring and resource integration" {
  // Test integration between performance monitoring and resource management
  
  type ResourceMetrics {
    resource_id : Int
    cpu_usage : Float
    memory_usage : Float
    disk_io : Int
    network_io : Int
    timestamp : Int
  }
  
  type PerformanceAlert {
    resource_id : Int
    metric_type : String
    current_value : Float
    threshold : Float
    timestamp : Int
  }
  
  type PerformanceMonitor {
    resources : Array[Int]  // Resource IDs
    metrics : Array[ResourceMetrics]
    alerts : Array[PerformanceAlert]
    thresholds : Array[(String, Float)]  // (metric_type, threshold)
  }
  
  let create_monitor = fn() {
    { 
      resources: [], 
      metrics: [], 
      alerts: [], 
      thresholds: [
        ("cpu", 80.0),
        ("memory", 90.0)
      ] 
    }
  }
  
  let add_resource = fn(monitor, resource_id) {
    { 
      resources: monitor.resources @ [resource_id], 
      metrics: monitor.metrics, 
      alerts: monitor.alerts, 
      thresholds: monitor.thresholds 
    }
  }
  
  let record_metrics = fn(monitor, resource_id, cpu_usage, memory_usage, disk_io, network_io) {
    let metrics = {
      resource_id: resource_id,
      cpu_usage: cpu_usage,
      memory_usage: memory_usage,
      disk_io: disk_io,
      network_io: network_io,
      timestamp: 1000
    }
    
    { 
      resources: monitor.resources, 
      metrics: monitor.metrics @ [metrics], 
      alerts: monitor.alerts, 
      thresholds: monitor.thresholds 
    }
  }
  
  let check_thresholds = fn(monitor) {
    let mut new_alerts = []
    
    // Get the latest metrics for each resource
    for resource_id in monitor.resources {
      let mut latest_metrics = []
      
      for metrics in monitor.metrics {
        if metrics.resource_id == resource_id {
          latest_metrics = latest_metrics @ [metrics]
        }
      }
      
      if latest_metrics.length() > 0 {
        let latest = latest_metrics[latest_metrics.length() - 1]
        
        // Check CPU threshold
        for (metric_type, threshold) in monitor.thresholds {
          if metric_type == "cpu" and latest.cpu_usage > threshold {
            new_alerts = new_alerts @ [{
              resource_id: resource_id,
              metric_type: metric_type,
              current_value: latest.cpu_usage,
              threshold: threshold,
              timestamp: latest.timestamp
            }]
          }
          
          if metric_type == "memory" and latest.memory_usage > threshold {
            new_alerts = new_alerts @ [{
              resource_id: resource_id,
              metric_type: metric_type,
              current_value: latest.memory_usage,
              threshold: threshold,
              timestamp: latest.timestamp
            }]
          }
        }
      }
    }
    
    { 
      resources: monitor.resources, 
      metrics: monitor.metrics, 
      alerts: monitor.alerts @ new_alerts, 
      thresholds: monitor.thresholds 
    }
  }
  
  let get_resource_performance = fn(monitor, resource_id) {
    let mut resource_metrics = []
    
    for metrics in monitor.metrics {
      if metrics.resource_id == resource_id {
        resource_metrics = resource_metrics @ [metrics]
      }
    }
    
    resource_metrics
  }
  
  // Test integration
  let monitor = create_monitor()
  
  // Add resources
  let monitor1 = add_resource(add_resource(monitor, 1), 2)
  
  // Record metrics for resource 1 (normal)
  let monitor2 = record_metrics(monitor1, 1, 45.2, 67.8, 100, 200)
  
  // Record metrics for resource 2 (high CPU)
  let monitor3 = record_metrics(monitor2, 2, 85.3, 72.1, 150, 300)
  
  // Record more metrics for resource 1 (high memory)
  let monitor4 = record_metrics(monitor3, 1, 50.1, 92.5, 120, 250)
  
  // Check thresholds
  let monitor5 = check_thresholds(monitor4)
  
  // Verify alerts
  assert_eq(monitor5.alerts.length(), 2)
  
  // Verify alert details
  let mut cpu_alert = None
  let mut memory_alert = None
  
  for alert in monitor5.alerts {
    if alert.metric_type == "cpu" {
      cpu_alert = Some(alert)
    } else if alert.metric_type == "memory" {
      memory_alert = Some(alert)
    }
  }
  
  match cpu_alert {
    Some(alert) => {
      assert_eq(alert.resource_id, 2)
      assert_eq(alert.current_value, 85.3)
      assert_eq(alert.threshold, 80.0)
    }
    None => assert_true(false)
  }
  
  match memory_alert {
    Some(alert) => {
      assert_eq(alert.resource_id, 1)
      assert_eq(alert.current_value, 92.5)
      assert_eq(alert.threshold, 90.0)
    }
    None => assert_true(false)
  }
  
  // Get resource performance
  let resource1_performance = get_resource_performance(monitor5, 1)
  assert_eq(resource1_performance.length(), 2)
  
  // Verify performance trend
  assert_true(resource1_performance[1].memory_usage > resource1_performance[0].memory_usage)
}

// Test 10: Full System Integration Test
test "full system integration test" {
  // Test integration across all system components
  
  type SystemState {
    configuration : Array[(String, String)]
    resources : Array[(Int, String)]  // (id, type)
    telemetry : Array[(String, Float)]  // (metric, value)
    network_requests : Int
    errors : Array[String]
    timestamp : Int
  }
  
  let initialize_system = fn() {
    {
      configuration: [
        ("server.port", "8080"),
        ("database.host", "localhost"),
        ("cache.size", "1000")
      ],
      resources: [(1, "database"), (2, "cache"), (3, "api")],
      telemetry: [
        ("cpu.usage", 45.2),
        ("memory.usage", 67.8),
        ("disk.usage", 23.4)
      ],
      network_requests: 0,
      errors: [],
      timestamp: 1000
    }
  }
  
  let process_request = fn(state, request_path) {
    // Simulate processing a request through the system
    let mut new_state = state
    new_state.timestamp = new_state.timestamp + 1
    new_state.network_requests = new_state.network_requests + 1
    
    // Update telemetry
    let mut updated_telemetry = []
    
    for (metric, value) in new_state.telemetry {
      let new_value = match metric {
        "cpu.usage" => value + 1.5,
        "memory.usage" => value + 0.8,
        "disk.usage" => value + 0.2,
        _ => value
      }
      updated_telemetry = updated_telemetry @ [(metric, new_value)]
    }
    
    new_state.telemetry = updated_telemetry
    
    // Check for errors based on configuration
    let server_port = ""
    let db_host = ""
    
    for (key, value) in new_state.configuration {
      match key {
        "server.port" => server_port = value
        "database.host" => db_host = value
        _ => ()
      }
    }
    
    if server_port == "" or db_host == "" {
      new_state.errors = new_state.errors @ ["Configuration error"]
    }
    
    // Check resource limits
    for (id, resource_type) in new_state.resources {
      match resource_type {
        "database" => {
          for (metric, value) in new_state.telemetry {
            if metric == "memory.usage" and value > 90.0 {
              new_state.errors = new_state.errors @ ["Database memory limit exceeded"]
            }
          }
        }
        _ => ()
      }
    }
    
    new_state
  }
  
  let generate_report = fn(state) {
    let config_count = state.configuration.length()
    let resource_count = state.resources.length()
    let telemetry_count = state.telemetry.length()
    let error_count = state.errors.length()
    
    "System Report:\n" +
    "  Configuration: " + config_count.to_string() + " items\n" +
    "  Resources: " + resource_count.to_string() + " items\n" +
    "  Telemetry: " + telemetry_count.to_string() + " metrics\n" +
    "  Network Requests: " + state.network_requests.to_string() + "\n" +
    "  Errors: " + error_count.to_string() + "\n" +
    "  Uptime: " + (state.timestamp - 1000).to_string() + " seconds"
  }
  
  // Test full system integration
  let system = initialize_system()
  
  // Process multiple requests
  let system1 = process_request(system, "/api/users")
  let system2 = process_request(system1, "/api/posts")
  let system3 = process_request(system2, "/api/comments")
  
  // Verify system state
  assert_eq(system3.network_requests, 3)
  assert_eq(system3.timestamp, 1003)
  assert_eq(system3.telemetry.length(), 3)
  
  // Check telemetry updates
  let mut cpu_usage = 0.0
  for (metric, value) in system3.telemetry {
    if metric == "cpu.usage" {
      cpu_usage = value
      break
    }
  }
  
  assert_true(cpu_usage > 45.2)  // Should have increased
  
  // Generate report
  let report = generate_report(system3)
  
  assert_true(report.contains("System Report:"))
  assert_true(report.contains("Configuration: 3 items"))
  assert_true(report.contains("Resources: 3 items"))
  assert_true(report.contains("Telemetry: 3 metrics"))
  assert_true(report.contains("Network Requests: 3"))
  assert_true(report.contains("Errors: 0"))
  assert_true(report.contains("Uptime: 3 seconds"))
}