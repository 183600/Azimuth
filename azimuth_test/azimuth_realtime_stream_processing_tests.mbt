// Azimuth Real-time Stream Processing Test Suite
// This file contains test cases for real-time stream processing functionality

// Test 1: Stream Data Processing Pipeline
test "stream data processing pipeline" {
  // Define stream data structure
  type StreamEvent = {
    event_id: String,
    stream_id: String,
    timestamp: Int,
    data: Array[(String, String)],
    metadata: Array[(String, String)]
  }
  
  type StreamProcessor = {
    processor_id: String,
    name: String,
    input_streams: Array[String],
    output_streams: Array[String],
    processing_rules: Array[ProcessingRule],
    enabled: Bool
  }
  
  type ProcessingRule = {
    rule_id: String,
    name: String,
    condition: String,
    action: String,
    config: Array[(String, String)]
  }
  
  type ProcessingResult = {
    processor_id: String,
    input_event: StreamEvent,
    output_events: Array[StreamEvent],
    processing_time: Int,
    rules_applied: Array[String]
  }
  
  // Create stream events
  let stream_events = [
    {
      event_id: "event-001",
      stream_id: "sensor-data",
      timestamp: 1640995200,
      data: [
        ("sensor_id", "temp-001"),
        ("temperature", "23.5"),
        ("humidity", "65.2"),
        ("location", "warehouse-a")
      ],
      metadata: [
        ("source", "edge-device"),
        ("format", "json"),
        ("version", "1.0")
      ]
    },
    {
      event_id: "event-002",
      stream_id: "sensor-data",
      timestamp: 1640995205,
      data: [
        ("sensor_id", "temp-002"),
        ("temperature", "24.1"),
        ("humidity", "64.8"),
        ("location", "warehouse-a")
      ],
      metadata: [
        ("source", "edge-device"),
        ("format", "json"),
        ("version", "1.0")
      ]
    },
    {
      event_id: "event-003",
      stream_id: "alert-data",
      timestamp: 1640995210,
      data: [
        ("alert_id", "alert-001"),
        ("severity", "warning"),
        ("message", "Temperature exceeding threshold"),
        ("sensor_id", "temp-001")
      ],
      metadata: [
        ("source", "edge-device"),
        ("format", "json"),
        ("version", "1.0")
      ]
    }
  ]
  
  // Create stream processors
  let stream_processors = [
    {
      processor_id: "temp-normalizer",
      name: "Temperature Normalization",
      input_streams: ["sensor-data"],
      output_streams: ["normalized-data"],
      processing_rules: [
        {
          rule_id: "temp-celsius-to-fahrenheit",
          name: "Convert Celsius to Fahrenheit",
          condition: "data.temperature exists",
          action: "transform",
          config: [
            ("input_field", "temperature"),
            ("output_field", "temperature_f"),
            ("formula", "celsius * 9/5 + 32")
          ]
        },
        {
          rule_id: "add-timestamp",
          name: "Add Processing Timestamp",
          condition: "true",
          action: "enrich",
          config: [
            ("field", "processed_at"),
            ("value", "current_timestamp")
          ]
        }
      ],
      enabled: true
    },
    {
      processor_id: "alert-router",
      name: "Alert Routing",
      input_streams: ["alert-data"],
      output_streams: ["critical-alerts", "warning-alerts"],
      processing_rules: [
        {
          rule_id: "route-critical",
          name: "Route Critical Alerts",
          condition: "data.severity == 'critical'",
          action: "route",
          config: [
            ("output_stream", "critical-alerts"),
            ("priority", "high")
          ]
        },
        {
          rule_id: "route-warning",
          name: "Route Warning Alerts",
          condition: "data.severity == 'warning'",
          action: "route",
          config: [
            ("output_stream", "warning-alerts"),
            ("priority", "medium")
          ]
        }
      ],
      enabled: true
    }
  ]
  
  // Filter events by stream
  let filter_events_by_stream = fn(events: Array[StreamEvent], stream_id: String) {
    events.filter(fn(e) { e.stream_id == stream_id })
  }
  
  let sensor_events = filter_events_by_stream(stream_events, "sensor-data")
  assert_eq(sensor_events.length(), 2)
  
  let alert_events = filter_events_by_stream(stream_events, "alert-data")
  assert_eq(alert_events.length(), 1)
  
  // Simulate temperature normalization processor
  let process_temp_normalizer = fn(processor: StreamProcessor, event: StreamEvent) {
    let mut output_events = []
    let mut rules_applied = []
    let mut processed_data = event.data
    
    for rule in processor.processing_rules {
      if rule.enabled {
        match rule.action {
          "transform" => {
            if rule.rule_id == "temp-celsius-to-fahrenheit" {
              let temp_celsius = event.data.filter(fn(d) { d.0 == "temperature" })
              if temp_celsius.length() > 0 {
                let temp_value = temp_celsius[0].1.to_float()
                let temp_fahrenheit = temp_value * 9.0 / 5.0 + 32.0
                processed_data = processed_data.push(("temperature_f", temp_fahrenheit.to_string()))
                rules_applied = rules_applied.push(rule.rule_id)
              }
            }
          }
          
          "enrich" => {
            if rule.rule_id == "add-timestamp" {
              processed_data = processed_data.push(("processed_at", "1640995200"))
              rules_applied = rules_applied.push(rule.rule_id)
            }
          }
          
          _ => {
            // Other actions not implemented
          }
        }
      }
    }
    
    let output_event = {
      event_id: event.event_id + "-processed",
      stream_id: processor.output_streams[0],
      timestamp: event.timestamp,
      data: processed_data,
      metadata: event.metadata.push(("processor", processor.processor_id))
    }
    
    output_events = output_events.push(output_event)
    
    {
      processor_id: processor.processor_id,
      input_event: event,
      output_events,
      processing_time: 50,  // Simulated processing time
      rules_applied
    }
  }
  
  // Process sensor events with temperature normalizer
  let temp_processor = stream_processors.filter(fn(p) { p.processor_id == "temp-normalizer" })[0]
  let processed_sensor_events = sensor_events.map(fn(e) { process_temp_normalizer(temp_processor, e) })
  
  assert_eq(processed_sensor_events.length(), 2)
  
  let first_result = processed_sensor_events[0]
  assert_eq(first_result.processor_id, "temp-normalizer")
  assert_eq(first_result.rules_applied.length(), 2)
  assert_true(first_result.rules_applied.contains("temp-celsius-to-fahrenheit"))
  assert_true(first_result.rules_applied.contains("add-timestamp"))
  assert_eq(first_result.output_events.length(), 1)
  
  let first_output = first_result.output_events[0]
  assert_eq(first_output.stream_id, "normalized-data")
  assert_true(first_output.data.any(fn(d) { d.0 == "temperature_f" }))
  assert_true(first_output.data.any(fn(d) { d.0 == "processed_at" }))
  
  // Check temperature conversion
  let temp_f_field = first_output.data.filter(fn(d) { d.0 == "temperature_f" })[0]
  let expected_temp_f = 23.5 * 9.0 / 5.0 + 32.0
  assert_eq(temp_f_field.1, expected_temp_f.to_string())
  
  // Simulate alert routing processor
  let process_alert_router = fn(processor: StreamProcessor, event: StreamEvent) {
    let mut output_events = []
    let mut rules_applied = []
    
    for rule in processor.processing_rules {
      if rule.enabled {
        match rule.action {
          "route" => {
            let should_route = match rule.rule_id {
              "route-critical" => {
                let severity_field = event.data.filter(fn(d) { d.0 == "severity" })
                severity_field.length() > 0 and severity_field[0].1 == "critical"
              }
              
              "route-warning" => {
                let severity_field = event.data.filter(fn(d) { d.0 == "severity" })
                severity_field.length() > 0 and severity_field[0].1 == "warning"
              }
              
              _ => false
            }
            
            if should_route {
              let output_stream = rule.config.filter(fn(c) { c.0 == "output_stream" })[0].1
              
              let output_event = {
                event_id: event.event_id + "-routed",
                stream_id: output_stream,
                timestamp: event.timestamp,
                data: event.data,
                metadata: event.metadata.push([
                  ("processor", processor.processor_id),
                  ("rule", rule.rule_id),
                  ("priority", rule.config.filter(fn(c) { c.0 == "priority" })[0].1)
                ])
              }
              
              output_events = output_events.push(output_event)
              rules_applied = rules_applied.push(rule.rule_id)
            }
          }
          
          _ => {
            // Other actions not implemented
          }
        }
      }
    }
    
    {
      processor_id: processor.processor_id,
      input_event: event,
      output_events,
      processing_time: 25,  // Simulated processing time
      rules_applied
    }
  }
  
  // Process alert events with alert router
  let alert_processor = stream_processors.filter(fn(p) { p.processor_id == "alert-router" })[0]
  let processed_alert_events = alert_events.map(fn(e) { process_alert_router(alert_processor, e) })
  
  assert_eq(processed_alert_events.length(), 1)
  
  let alert_result = processed_alert_events[0]
  assert_eq(alert_result.processor_id, "alert-router")
  assert_eq(alert_result.rules_applied.length(), 1)
  assert_true(alert_result.rules_applied.contains("route-warning"))
  assert_eq(alert_result.output_events.length(), 1)
  
  let alert_output = alert_result.output_events[0]
  assert_eq(alert_output.stream_id, "warning-alerts")
  assert_true(alert_output.metadata.any(fn(m) { m.contains(("processor", "alert-router")) }))
  assert_true(alert_output.metadata.any(fn(m) { m.contains(("rule", "route-warning")) }))
  assert_true(alert_output.metadata.any(fn(m) { m.contains(("priority", "medium")) }))
  
  // Calculate stream processing metrics
  let calculate_stream_metrics = fn(results: Array[ProcessingResult]) {
    let total_events = results.length()
    let total_processing_time = results.reduce(fn(acc, r) { acc + r.processing_time }, 0)
    let avg_processing_time = if total_events > 0 {
      total_processing_time / total_events
    } else {
      0
    }
    
    let total_rules_applied = results.reduce(fn(acc, r) { acc + r.rules_applied.length() }, 0)
    let avg_rules_per_event = if total_events > 0 {
      total_rules_applied.to_float() / total_events.to_float()
    } else {
      0.0
    }
    
    let total_output_events = results.reduce(fn(acc, r) { acc + r.output_events.length() }, 0)
    let output_ratio = if total_events > 0 {
      total_output_events.to_float() / total_events.to_float()
    } else {
      0.0
    }
    
    {
      total_events,
      avg_processing_time,
      avg_rules_per_event,
      total_output_events,
      output_ratio
    }
  }
  
  let all_results = processed_sensor_events + processed_alert_events
  let stream_metrics = calculate_stream_metrics(all_results)
  
  assert_eq(stream_metrics.total_events, 3)
  assert_eq(stream_metrics.avg_processing_time, 41)  // (50 + 50 + 25) / 3
  assert_eq(stream_metrics.avg_rules_per_event, 1.67)  // (2 + 2 + 1) / 3
  assert_eq(stream_metrics.total_output_events, 3)
  assert_eq(stream_metrics.output_ratio, 1.0)
}

// Test 2: Windowed Stream Aggregation
test "windowed stream aggregation" {
  // Define windowed aggregation structure
  type TimeWindow = {
    window_id: String,
    start_time: Int,
    end_time: Int,
    size_ms: Int,
    slide_ms: Int
  }
  
  type AggregationRule = {
    rule_id: String,
    name: String,
    field: String,
    function: String,
    window: TimeWindow
  }
  
  type AggregationResult = {
    rule_id: String,
    window_id: String,
    result: Float,
    event_count: Int,
    timestamp: Int
  }
  
  // Create time windows
  let windows = [
    {
      window_id: "window-001",
      start_time: 1640995200,
      end_time: 1640995300,
      size_ms: 100000,  // 100 seconds
      slide_ms: 100000   // Non-overlapping windows
    },
    {
      window_id: "window-002",
      start_time: 1640995300,
      end_time: 1640995400,
      size_ms: 100000,
      slide_ms: 100000
    }
  ]
  
  // Create aggregation rules
  let aggregation_rules = [
    {
      rule_id: "avg-temperature",
      name: "Average Temperature",
      field: "temperature",
      function: "avg",
      window: windows[0]
    },
    {
      rule_id: "max-humidity",
      name: "Maximum Humidity",
      field: "humidity",
      function: "max",
      window: windows[0]
    },
    {
      rule_id: "min-pressure",
      name: "Minimum Pressure",
      field: "pressure",
      function: "min",
      window: windows[0]
    },
    {
      rule_id: "count-events",
      name: "Event Count",
      field: "*",
      function: "count",
      window: windows[0]
    }
  ]
  
  // Create stream events for aggregation
  let stream_events = [
    {
      event_id: "event-001",
      stream_id: "sensor-data",
      timestamp: 1640995210,  // In window 1
      data: [
        ("sensor_id", "temp-001"),
        ("temperature", "23.5"),
        ("humidity", "65.2"),
        ("pressure", "1013.25")
      ],
      metadata: []
    },
    {
      event_id: "event-002",
      stream_id: "sensor-data",
      timestamp: 1640995250,  // In window 1
      data: [
        ("sensor_id", "temp-002"),
        ("temperature", "24.1"),
        ("humidity", "64.8"),
        ("pressure", "1013.15")
      ],
      metadata: []
    },
    {
      event_id: "event-003",
      stream_id: "sensor-data",
      timestamp: 1640995280,  // In window 1
      data: [
        ("sensor_id", "temp-003"),
        ("temperature", "22.9"),
        ("humidity", "66.1"),
        ("pressure", "1013.35")
      ],
      metadata: []
    },
    {
      event_id: "event-004",
      stream_id: "sensor-data",
      timestamp: 1640995310,  // In window 2
      data: [
        ("sensor_id", "temp-004"),
        ("temperature", "25.2"),
        ("humidity", "63.5"),
        ("pressure", "1013.05")
      ],
      metadata: []
    }
  ]
  
  // Filter events by time window
  let filter_events_by_window = fn(events: Array[StreamEvent], window: TimeWindow) {
    events.filter(fn(e) { 
      e.timestamp >= window.start_time and e.timestamp < window.end_time 
    })
  }
  
  let window1_events = filter_events_by_window(stream_events, windows[0])
  assert_eq(window1_events.length(), 3)
  
  let window2_events = filter_events_by_window(stream_events, windows[1])
  assert_eq(window2_events.length(), 1)
  
  // Extract field values from events
  let extract_field_values = fn(events: Array[StreamEvent], field: String) {
    events.filter(fn(e) {
      e.data.any(fn(d) { d.0 == field })
    }).map(fn(e) {
      e.data.filter(fn(d) { d.0 == field })[0].1.to_float()
    })
  }
  
  // Calculate aggregation
  let calculate_aggregation = fn(events: Array[StreamEvent], rule: AggregationRule) {
    let window_events = filter_events_by_window(events, rule.window)
    
    match rule.function {
      "avg" => {
        let values = extract_field_values(window_events, rule.field)
        if values.length() > 0 {
          let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
          sum / values.length().to_float()
        } else {
          0.0
        }
      }
      
      "max" => {
        let values = extract_field_values(window_events, rule.field)
        if values.length() > 0 {
          values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
        } else {
          0.0
        }
      }
      
      "min" => {
        let values = extract_field_values(window_events, rule.field)
        if values.length() > 0 {
          values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
        } else {
          0.0
        }
      }
      
      "count" => {
        window_events.length().to_float()
      }
      
      "sum" => {
        let values = extract_field_values(window_events, rule.field)
        values.reduce(fn(acc, v) { acc + v }, 0.0)
      }
      
      _ => 0.0
    }
  }
  
  // Test average temperature aggregation
  let avg_temp_rule = aggregation_rules.filter(fn(r) { r.rule_id == "avg-temperature" })[0]
  let avg_temp = calculate_aggregation(stream_events, avg_temp_rule)
  let expected_avg_temp = (23.5 + 24.1 + 22.9) / 3.0
  assert_eq(avg_temp, expected_avg_temp)
  
  // Test max humidity aggregation
  let max_humidity_rule = aggregation_rules.filter(fn(r) { r.rule_id == "max-humidity" })[0]
  let max_humidity = calculate_aggregation(stream_events, max_humidity_rule)
  let expected_max_humidity = 66.1
  assert_eq(max_humidity, expected_max_humidity)
  
  // Test min pressure aggregation
  let min_pressure_rule = aggregation_rules.filter(fn(r) { r.rule_id == "min-pressure" })[0]
  let min_pressure = calculate_aggregation(stream_events, min_pressure_rule)
  let expected_min_pressure = 1013.15
  assert_eq(min_pressure, expected_min_pressure)
  
  // Test count aggregation
  let count_rule = aggregation_rules.filter(fn(r) { r.rule_id == "count-events" })[0]
  let event_count = calculate_aggregation(stream_events, count_rule)
  assert_eq(event_count, 3.0)
  
  // Calculate all aggregations for a window
  let calculate_window_aggregations = fn(events: Array[StreamEvent], rules: Array[AggregationRule], window: TimeWindow) {
    rules.map(fn(rule) {
      let result = calculate_aggregation(events, rule)
      let window_events = filter_events_by_window(events, rule.window)
      
      {
        rule_id: rule.rule_id,
        window_id: window.window_id,
        result,
        event_count: window_events.length(),
        timestamp: window.end_time
      }
    })
  }
  
  let window1_results = calculate_window_aggregations(stream_events, aggregation_rules, windows[0])
  assert_eq(window1_results.length(), 4)
  
  let avg_temp_result = window1_results.filter(fn(r) { r.rule_id == "avg-temperature" })[0]
  assert_eq(avg_temp_result.window_id, "window-001")
  assert_eq(avg_temp_result.result, expected_avg_temp)
  assert_eq(avg_temp_result.event_count, 3)
  
  let max_humidity_result = window1_results.filter(fn(r) { r.rule_id == "max-humidity" })[0]
  assert_eq(max_humidity_result.window_id, "window-001")
  assert_eq(max_humidity_result.result, expected_max_humidity)
  assert_eq(max_humidity_result.event_count, 3)
  
  // Test with sliding windows
  let sliding_windows = [
    {
      window_id: "sliding-001",
      start_time: 1640995200,
      end_time: 1640995300,
      size_ms: 100000,
      slide_ms: 50000  // 50% overlap
    },
    {
      window_id: "sliding-002",
      start_time: 1640995250,
      end_time: 1640995350,
      size_ms: 100000,
      slide_ms: 50000
    }
  ]
  
  let sliding_window1_events = filter_events_by_window(stream_events, sliding_windows[0])
  assert_eq(sliding_window1_events.length(), 3)
  
  let sliding_window2_events = filter_events_by_window(stream_events, sliding_windows[1])
  assert_eq(sliding_window2_events.length(), 2)  // Events 2, 3, and 4, but only 2 and 3 are in range
  
  // Create sliding window aggregation rule
  let sliding_avg_temp_rule = {
    rule_id: "sliding-avg-temperature",
    name: "Sliding Average Temperature",
    field: "temperature",
    function: "avg",
    window: sliding_windows[0]
  }
  
  let sliding_avg_temp1 = calculate_aggregation(stream_events, sliding_avg_temp_rule)
  assert_eq(sliding_avg_temp1, expected_avg_temp)
  
  let sliding_avg_temp_rule2 = {
    rule_id: "sliding-avg-temperature-2",
    name: "Sliding Average Temperature 2",
    field: "temperature",
    function: "avg",
    window: sliding_windows[1]
  }
  
  let sliding_avg_temp2 = calculate_aggregation(stream_events, sliding_avg_temp_rule2)
  let expected_sliding_avg_temp2 = (24.1 + 22.9) / 2.0
  assert_eq(sliding_avg_temp2, expected_sliding_avg_temp2)
  
  // Calculate aggregation performance metrics
  let calculate_aggregation_metrics = fn(results: Array[AggregationResult]) {
    let total_aggregations = results.length()
    let unique_windows = {
      let mut window_ids = []
      for result in results {
        if not(window_ids.contains(result.window_id)) {
          window_ids = window_ids.push(result.window_id)
        }
      }
      window_ids.length()
    }
    
    let total_events_processed = results.reduce(fn(acc, r) { acc + r.event_count }, 0)
    let avg_events_per_aggregation = if total_aggregations > 0 {
      total_events_processed.to_float() / total_aggregations.to_float()
    } else {
      0.0
    }
    
    {
      total_aggregations,
      unique_windows,
      total_events_processed,
      avg_events_per_aggregation
    }
  }
  
  let all_window_results = window1_results
  let aggregation_metrics = calculate_aggregation_metrics(all_window_results)
  
  assert_eq(aggregation_metrics.total_aggregations, 4)
  assert_eq(aggregation_metrics.unique_windows, 1)
  assert_eq(aggregation_metrics.total_events_processed, 12)  // 3 events * 4 aggregations
  assert_eq(aggregation_metrics.avg_events_per_aggregation, 3.0)
}