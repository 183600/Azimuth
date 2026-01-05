// Telemetry Pipeline Tests for Azimuth Telemetry System
// This file contains test cases for telemetry pipeline functionality

// Test 1: Telemetry Data Collection
test "telemetry data collection functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define collector
  type Collector = {
    id: String,
    name: String,
    buffer: Array[TelemetryData],
    buffer_size: Int,
    is_active: Bool
  }
  
  // Create telemetry data point
  let create_data_point = fn(timestamp: Int, metric_name: String, value: Float, tags: Array[(String, String)], resource: String) {
    {
      timestamp,
      metric_name,
      value,
      tags,
      resource
    }
  }
  
  // Create collector
  let create_collector = fn(id: String, name: String, buffer_size: Int) {
    {
      id,
      name,
      buffer: [],
      buffer_size,
      is_active: true
    }
  }
  
  // Add data point to collector
  let collect_data = fn(collector: Collector, data: TelemetryData) {
    if not(collector.is_active) {
      collector
    } else if collector.buffer.length() >= collector.buffer_size {
      // Buffer full, drop oldest data
      let updated_buffer = collector.buffer.slice(1, collector.buffer.length()).push(data)
      { collector | buffer: updated_buffer }
    } else {
      // Add to buffer
      let updated_buffer = collector.buffer.push(data)
      { collector | buffer: updated_buffer }
    }
  }
  
  // Create collector
  let collector = create_collector("collector-1", "main-collector", 100)
  
  // Create sample telemetry data
  let data1 = create_data_point(
    1640995200,
    "cpu_usage",
    75.5,
    [("host", "server-1"), ("region", "us-west")],
    "server-1"
  )
  
  let data2 = create_data_point(
    1640995205,
    "memory_usage",
    60.2,
    [("host", "server-1"), ("region", "us-west")],
    "server-1"
  )
  
  let data3 = create_data_point(
    1640995210,
    "disk_usage",
    45.8,
    [("host", "server-1"), ("region", "us-west")],
    "server-1"
  )
  
  // Collect data
  let collector1 = collect_data(collector, data1)
  let collector2 = collect_data(collector1, data2)
  let collector3 = collect_data(collector2, data3)
  
  // Verify data collection
  assert_eq(collector3.buffer.length(), 3)
  assert_eq(collector3.buffer[0].metric_name, "cpu_usage")
  assert_eq(collector3.buffer[1].metric_name, "memory_usage")
  assert_eq(collector3.buffer[2].metric_name, "disk_usage")
  
  // Test buffer overflow
  let small_collector = create_collector("small-collector", "small", 2)
  
  let small1 = collect_data(small_collector, data1)
  let small2 = collect_data(small1, data2)
  let small3 = collect_data(small2, data3)  // Should drop data1
  
  assert_eq(small3.buffer.length(), 2)
  assert_eq(small3.buffer[0].metric_name, "memory_usage")  // data1 was dropped
  assert_eq(small3.buffer[1].metric_name, "disk_usage")
  
  // Test inactive collector
  let inactive_collector = { collector | is_active: false }
  let inactive_result = collect_data(inactive_collector, data1)
  
  assert_eq(inactive_result.buffer.length(), 0)
}

// Test 2: Telemetry Data Transformation
test "telemetry data transformation functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define transformer
  type Transformer = {
    id: String,
    name: String,
    transformations: Array[Transformation]
  }
  
  // Define transformation
  type Transformation = {
    type: String,
    config: Array[(String, String)]
  }
  
  // Apply transformation to data
  let apply_transformation = fn(data: TelemetryData, transformation: Transformation) {
    match transformation.type {
      "unit_conversion" => {
        // Find conversion factor in config
        let from_unit = transformation.config.find(fn((k, _)) { k == "from" })
        let to_unit = transformation.config.find(fn((k, _)) { k == "to" })
        
        match (from_unit, to_unit) {
          (Some((_, "bytes")), Some((_, "kilobytes"))) => {
            { data | value: data.value / 1024.0 }
          }
          (Some((_, "kilobytes")), Some((_, "bytes"))) => {
            { data | value: data.value * 1024.0 }
          }
          (Some((_, "milliseconds")), Some((_, "seconds"))) => {
            { data | value: data.value / 1000.0 }
          }
          (Some((_, "seconds")), Some((_, "milliseconds"))) => {
            { data | value: data.value * 1000.0 }
          }
          _ => data  // No conversion applied
        }
      }
      "tag_enrichment" => {
        // Add new tags
        let new_tags = transformation.config
        let updated_tags = data.tags.concat(new_tags)
        { data | tags: updated_tags }
      }
      "metric_rename" => {
        // Find new name in config
        let new_name_config = transformation.config.find(fn((k, _)) { k == "new_name" })
        
        match new_name_config {
          Some((_, new_name)) => {
            { data | metric_name: new_name }
          }
          None => data
        }
      }
      "value_scaling" => {
        // Find scaling factor in config
        let factor_config = transformation.config.find(fn((k, _)) { k == "factor" })
        
        match factor_config {
          Some((_, factor_str)) => {
            let factor = factor_str.to_float()
            { data | value: data.value * factor }
          }
          None => data
        }
      }
      _ => data  // Unknown transformation
    }
  }
  
  // Apply all transformations
  let apply_transformations = fn(data: TelemetryData, transformer: Transformer) {
    transformer.transformations.reduce(fn(d, t) { apply_transformation(d, t) }, data)
  }
  
  // Create transformer
  let transformer = {
    id: "transformer-1",
    name: "main-transformer",
    transformations: [
      {
        type: "unit_conversion",
        config: [("from", "bytes"), ("to", "kilobytes")]
      },
      {
        type: "tag_enrichment",
        config: [("environment", "production"), ("datacenter", "dc1")]
      },
      {
        type: "metric_rename",
        config: [("new_name", "memory_usage_kb")]
      }
    ]
  }
  
  // Create sample data
  let data = {
    timestamp: 1640995200,
    metric_name: "memory_usage_bytes",
    value: 1048576.0,  // 1MB in bytes
    tags: [("host", "server-1")],
    resource: "server-1"
  }
  
  // Apply transformations
  let transformed_data = apply_transformations(data, transformer)
  
  // Verify transformations
  assert_eq(transformed_data.metric_name, "memory_usage_kb")
  assert_eq(transformed_data.value, 1024.0)  // 1048576 / 1024 = 1024
  assert_eq(transformed_data.tags.length(), 3)
  assert_true(transformed_data.tags.contains(("host", "server-1")))
  assert_true(transformed_data.tags.contains(("environment", "production")))
  assert_true(transformed_data.tags.contains(("datacenter", "dc1")))
  
  // Test value scaling
  let scaling_transformer = {
    id: "scaler-1",
    name: "value-scaler",
    transformations: [
      {
        type: "value_scaling",
        config: [("factor", "100.0")]
      }
    ]
  }
  
  let cpu_data = {
    timestamp: 1640995205,
    metric_name: "cpu_usage",
    value: 0.75,  // 75%
    tags: [("host", "server-1")],
    resource: "server-1"
  }
  
  let scaled_data = apply_transformations(cpu_data, scaling_transformer)
  
  assert_eq(scaled_data.metric_name, "cpu_usage")
  assert_eq(scaled_data.value, 75.0)  // 0.75 * 100 = 75
}

// Test 3: Telemetry Data Aggregation
test "telemetry data aggregation functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define aggregation function
  enum AggregationFunction {
    Sum
    Average
    Min
    Max
    Count
  }
  
  // Define aggregation rule
  type AggregationRule = {
    metric_name: String,
    function: AggregationFunction,
    interval: Int,  // in seconds
    group_by_tags: Array[String]
  }
  
  // Define aggregated data point
  type AggregatedData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    count: Int,
    interval: Int
  }
  
  // Group data by metric and tags
  let group_data = fn(data_points: Array[TelemetryData], rule: AggregationRule) {
    let mut groups = []
    
    for data in data_points {
      if data.metric_name == rule.metric_name {
        // Create group key from specified tags
        let mut tag_values = []
        for tag_key in rule.group_by_tags {
          let tag_value = data.tags.find(fn((k, _)) { k == tag_key })
          match tag_value {
            Some((_, v)) => tag_values = tag_values.push(v)
            None => tag_values = tag_values.push("")
          }
        }
        
        let group_key = tag_values.join("|")
        
        // Find or create group
        let mut existing_group = None
        let mut group_index = 0
        
        for i in 0..groups.length() {
          if groups[i].key == group_key {
            existing_group = Some(groups[i])
            group_index = i
          }
        }
        
        match existing_group {
          Some(group) => {
            // Update existing group
            let updated_points = group.points.push(data)
            groups[group_index] = { group | points: updated_points }
          }
          None => {
            // Create new group
            groups = groups.push({
              key: group_key,
              points: [data]
            })
          }
        }
      }
    }
    
    groups
  }
  
  // Apply aggregation function to a group of data points
  let apply_aggregation = fn(points: Array[TelemetryData], function: AggregationFunction) {
    match function {
      AggregationFunction::Sum => {
        points.reduce(fn(acc, p) { acc + p.value }, 0.0)
      }
      AggregationFunction::Average => {
        let sum = points.reduce(fn(acc, p) { acc + p.value }, 0.0)
        sum / points.length().to_float()
      }
      AggregationFunction::Min => {
        points.reduce(fn(acc, p) { if p.value < acc { p.value } else { acc } }, points[0].value)
      }
      AggregationFunction::Max => {
        points.reduce(fn(acc, p) { if p.value > acc { p.value } else { acc } }, points[0].value)
      }
      AggregationFunction::Count => {
        points.length().to_float()
      }
    }
  }
  
  // Aggregate data points
  let aggregate_data = fn(data_points: Array[TelemetryData], rule: AggregationRule, window_start: Int) {
    let groups = group_data(data_points, rule)
    let mut results = []
    
    for group in groups {
      let aggregated_value = apply_aggregation(group.points, rule.function)
      
      // Create tags for aggregated data
      let mut aggregated_tags = []
      for tag_key in rule.group_by_tags {
        let tag_value = group.points[0].tags.find(fn((k, _)) { k == tag_key })
        match tag_value {
          Some((_, v)) => aggregated_tags = aggregated_tags.push((tag_key, v))
          None => ()
        }
      }
      
      results = results.push({
        timestamp: window_start,
        metric_name: rule.metric_name + "_" + match rule.function {
          AggregationFunction::Sum => "sum"
          AggregationFunction::Average => "avg"
          AggregationFunction::Min => "min"
          AggregationFunction::Max => "max"
          AggregationFunction::Count => "count"
        },
        value: aggregated_value,
        tags: aggregated_tags,
        count: group.points.length(),
        interval: rule.interval
      })
    }
    
    results
  }
  
  // Create sample data points
  let data_points = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 75.5, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995205, metric_name: "cpu_usage", value: 80.2, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995210, metric_name: "cpu_usage", value: 70.8, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 65.3, tags: [("host", "server-2"), ("region", "us-west")], resource: "server-2" },
    { timestamp: 1640995205, metric_name: "cpu_usage", value: 68.7, tags: [("host", "server-2"), ("region", "us-west")], resource: "server-2" },
    { timestamp: 1640995210, metric_name: "cpu_usage", value: 72.1, tags: [("host", "server-2"), ("region", "us-west")], resource: "server-2" },
    { timestamp: 1640995200, metric_name: "memory_usage", value: 60.5, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995205, metric_name: "memory_usage", value: 62.3, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" }
  ]
  
  // Create aggregation rule
  let rule = {
    metric_name: "cpu_usage",
    function: AggregationFunction::Average,
    interval: 60,  // 1 minute
    group_by_tags: ["host", "region"]
  }
  
  // Aggregate data
  let aggregated_results = aggregate_data(data_points, rule, 1640995200)
  
  // Verify aggregation results
  assert_eq(aggregated_results.length(), 2)  // Two groups: server-1 and server-2
  
  // Find server-1 result
  let server1_result = aggregated_results.find(fn(r) { 
    r.tags.contains(("host", "server-1")) 
  }).unwrap()
  
  assert_eq(server1_result.metric_name, "cpu_usage_avg")
  assert_eq(server1_result.count, 3)
  assert_eq(server1_result.value, (75.5 + 80.2 + 70.8) / 3.0)
  
  // Find server-2 result
  let server2_result = aggregated_results.find(fn(r) { 
    r.tags.contains(("host", "server-2")) 
  }).unwrap()
  
  assert_eq(server2_result.metric_name, "cpu_usage_avg")
  assert_eq(server2_result.count, 3)
  assert_eq(server2_result.value, (65.3 + 68.7 + 72.1) / 3.0)
  
  // Test sum aggregation
  let sum_rule = {
    metric_name: "memory_usage",
    function: AggregationFunction::Sum,
    interval: 60,
    group_by_tags: ["host"]
  }
  
  let sum_results = aggregate_data(data_points, sum_rule, 1640995200)
  
  assert_eq(sum_results.length(), 1)  // One group: server-1
  assert_eq(sum_results[0].metric_name, "memory_usage_sum")
  assert_eq(sum_results[0].count, 2)
  assert_eq(sum_results[0].value, 60.5 + 62.3)
}

// Test 4: Telemetry Data Filtering
test "telemetry data filtering functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define filter condition
  enum FilterCondition {
    Equals(String)
    NotEquals(String)
    GreaterThan(Float)
    LessThan(Float)
    Contains(String)
    Regex(String)  // Simplified - just contains check
  }
  
  // Define filter rule
  type FilterRule = {
    field: String,  // "metric_name", "value", "tag:<key>", "resource"
    condition: FilterCondition
  }
  
  // Define filter
  type Filter = {
    id: String,
    name: String,
    rules: Array[FilterRule>,
    operator: String  // "AND" or "OR"
  }
  
  // Check if data point matches a filter rule
  let matches_rule = fn(data: TelemetryData, rule: FilterRule) {
    let field_value = match rule.field {
      "metric_name" => Some(data.metric_name)
      "resource" => Some(data.resource)
      "value" => None  // Special case for numeric values
      _ => {
        if rule.field.starts_with("tag:") {
          let tag_key = rule.field.substring(4, rule.field.length() - 4)
          let tag_value = data.tags.find(fn((k, _)) { k == tag_key })
          match tag_value {
            Some((_, v)) => Some(v)
            None => None
          }
        } else {
          None
        }
      }
    }
    
    match field_value {
      Some(value) => {
        match rule.condition {
          FilterCondition::Equals(expected) => value == expected
          FilterCondition::NotEquals(expected) => value != expected
          FilterCondition::Contains(substring) => value.contains(substring)
          FilterCondition::Regex(pattern) => value.contains(pattern)  // Simplified
          _ => false  // Numeric conditions don't apply to string values
        }
      }
      None => {
        if rule.field == "value" {
          match rule.condition {
            FilterCondition::GreaterThan(threshold) => data.value > threshold
            FilterCondition::LessThan(threshold) => data.value < threshold
            FilterCondition::Equals(expected_str) => {
              let expected = expected_str.to_float()
              data.value == expected
            }
            FilterCondition::NotEquals(expected_str) => {
              let expected = expected_str.to_float()
              data.value != expected
            }
            _ => false
          }
        } else {
          false
        }
      }
    }
  }
  
  // Check if data point matches all filter rules
  let matches_filter = fn(data: TelemetryData, filter: Filter) {
    let rule_results = filter.rules.map(fn(rule) { matches_rule(data, rule) })
    
    match filter.operator {
      "AND" => rule_results.filter(fn(r) { r }).length() == filter.rules.length()
      "OR" => rule_results.filter(fn(r) { r }).length() > 0
      _ => false
    }
  }
  
  // Apply filter to data points
  let apply_filter = fn(data_points: Array[TelemetryData], filter: Filter) {
    data_points.filter(fn(data) { matches_filter(data, filter) })
  }
  
  // Create sample data points
  let data_points = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 75.5, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995205, metric_name: "memory_usage", value: 60.2, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995210, metric_name: "cpu_usage", value: 85.3, tags: [("host", "server-2"), ("region", "us-east")], resource: "server-2" },
    { timestamp: 1640995215, metric_name: "disk_usage", value: 45.8, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995220, metric_name: "cpu_usage", value: 65.7, tags: [("host", "server-3"), ("region", "us-west")], resource: "server-3" },
    { timestamp: 1640995225, metric_name: "network_usage", value: 120.5, tags: [("host", "server-2"), ("region", "us-east")], resource: "server-2" }
  ]
  
  // Test metric name filter
  let cpu_filter = {
    id: "cpu-filter",
    name: "CPU Metrics Filter",
    rules: [
      { field: "metric_name", condition: FilterCondition::Equals("cpu_usage") }
    ],
    operator: "AND"
  }
  
  let cpu_results = apply_filter(data_points, cpu_filter)
  
  assert_eq(cpu_results.length(), 3)
  for result in cpu_results {
    assert_eq(result.metric_name, "cpu_usage")
  }
  
  // Test tag filter
  let west_filter = {
    id: "west-filter",
    name: "West Region Filter",
    rules: [
      { field: "tag:region", condition: FilterCondition::Equals("us-west") }
    ],
    operator: "AND"
  }
  
  let west_results = apply_filter(data_points, west_filter)
  
  assert_eq(west_results.length(), 4)
  for result in west_results {
    assert_true(result.tags.contains(("region", "us-west")))
  }
  
  // Test value filter
  let high_cpu_filter = {
    id: "high-cpu-filter",
    name: "High CPU Usage Filter",
    rules: [
      { field: "metric_name", condition: FilterCondition::Equals("cpu_usage") },
      { field: "value", condition: FilterCondition::GreaterThan(70.0) }
    ],
    operator: "AND"
  }
  
  let high_cpu_results = apply_filter(data_points, high_cpu_filter)
  
  assert_eq(high_cpu_results.length(), 2)
  for result in high_cpu_results {
    assert_eq(result.metric_name, "cpu_usage")
    assert_true(result.value > 70.0)
  }
  
  // Test OR filter
  let resource_filter = {
    id: "resource-filter",
    name: "Resource Filter",
    rules: [
      { field: "resource", condition: FilterCondition::Equals("server-1") },
      { field: "resource", condition: FilterCondition::Equals("server-2") }
    ],
    operator: "OR"
  }
  
  let resource_results = apply_filter(data_points, resource_filter)
  
  assert_eq(resource_results.length(), 5)
  for result in resource_results {
    assert_true(result.resource == "server-1" or result.resource == "server-2")
  }
  
  // Test contains filter
  let usage_filter = {
    id: "usage-filter",
    name: "Usage Metrics Filter",
    rules: [
      { field: "metric_name", condition: FilterCondition::Contains("usage") }
    ],
    operator: "AND"
  }
  
  let usage_results = apply_filter(data_points, usage_filter)
  
  assert_eq(usage_results.length(), 5)
  for result in usage_results {
    assert_true(result.metric_name.contains("usage"))
  }
}

// Test 5: Telemetry Data Sampling
test "telemetry data sampling functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define sampling strategy
  enum SamplingStrategy {
    Random(Float)  // Sample rate between 0.0 and 1.0
    Fixed(Int)     // Sample every Nth point
    TimeWindow(Int)  // Sample at most N points per time window
    Reservoir(Int)  // Keep a reservoir of N samples
  }
  
  // Define sampler
  type Sampler = {
    id: String,
    name: String,
    strategy: SamplingStrategy,
    state: SamplerState
  }
  
  // Define sampler state
  type SamplerState = {
    counter: Int,
    samples: Array[TelemetryData],
    last_reset: Int
  }
  
  // Create sampler
  let create_sampler = fn(id: String, name: String, strategy: SamplingStrategy) {
    {
      id,
      name,
      strategy,
      state: {
        counter: 0,
        samples: [],
        last_reset: 0
      }
    }
  }
  
  // Sample data point
  let sample_data = fn(sampler: Sampler, data: TelemetryData, current_time: Int) {
    match sampler.strategy {
      SamplingStrategy::Random(rate) => {
        // Simple random sampling based on counter
        let should_sample = (sampler.state.counter.to_float() * rate).floor() < 
                           ((sampler.state.counter + 1).to_float() * rate).floor()
        
        let updated_counter = sampler.state.counter + 1
        
        if should_sample {
          let updated_samples = sampler.state.samples.push(data)
          {
            sampler |
            state: {
              counter: updated_counter,
              samples: updated_samples,
              last_reset: sampler.state.last_reset
            }
          }
        } else {
          {
            sampler |
            state: {
              counter: updated_counter,
              samples: sampler.state.samples,
              last_reset: sampler.state.last_reset
            }
          }
        }
      }
      SamplingStrategy::Fixed(interval) => {
        let should_sample = sampler.state.counter % interval == 0
        let updated_counter = sampler.state.counter + 1
        
        if should_sample {
          let updated_samples = sampler.state.samples.push(data)
          {
            sampler |
            state: {
              counter: updated_counter,
              samples: updated_samples,
              last_reset: sampler.state.last_reset
            }
          }
        } else {
          {
            sampler |
            state: {
              counter: updated_counter,
              samples: sampler.state.samples,
              last_reset: sampler.state.last_reset
            }
          }
        }
      }
      SamplingStrategy::TimeWindow(max_samples) => {
        // Reset counter if time window has passed (simplified - using counter as time)
        let window_size = 60  // 60 second window
        
        if current_time - sampler.state.last_reset > window_size {
          // Reset for new window
          let updated_samples = [data]
          {
            sampler |
            state: {
              counter: 1,
              samples: updated_samples,
              last_reset: current_time
            }
          }
        } else if sampler.state.samples.length() < max_samples {
          // Add sample
          let updated_samples = sampler.state.samples.push(data)
          let updated_counter = sampler.state.counter + 1
          
          {
            sampler |
            state: {
              counter: updated_counter,
              samples: updated_samples,
              last_reset: sampler.state.last_reset
            }
          }
        } else {
          // Skip sample
          let updated_counter = sampler.state.counter + 1
          
          {
            sampler |
            state: {
              counter: updated_counter,
              samples: sampler.state.samples,
              last_reset: sampler.state.last_reset
            }
          }
        }
      }
      SamplingStrategy::Reservoir(size) => {
        let updated_counter = sampler.state.counter + 1
        
        if sampler.state.samples.length() < size {
          // Reservoir not full, add sample
          let updated_samples = sampler.state.samples.push(data)
          
          {
            sampler |
            state: {
              counter: updated_counter,
              samples: updated_samples,
              last_reset: sampler.state.last_reset
            }
          }
        } else {
          // Reservoir full, randomly replace
          let replace_index = updated_counter % (size + 1)
          
          if replace_index < size {
            let updated_samples = sampler.state.samples.map_with_index(fn(i, sample) {
              if i == replace_index {
                data
              } else {
                sample
              }
            })
            
            {
              sampler |
              state: {
                counter: updated_counter,
                samples: updated_samples,
                last_reset: sampler.state.last_reset
              }
            }
          } else {
            {
              sampler |
              state: {
                counter: updated_counter,
                samples: sampler.state.samples,
                last_reset: sampler.state.last_reset
              }
            }
          }
        }
      }
    }
  }
  
  // Get samples from sampler
  let get_samples = fn(sampler: Sampler) {
    sampler.state.samples
  }
  
  // Create sample data points
  let data_points = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 75.5, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995205, metric_name: "cpu_usage", value: 80.2, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995210, metric_name: "cpu_usage", value: 70.8, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995215, metric_name: "cpu_usage", value: 85.3, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995220, metric_name: "cpu_usage", value: 65.7, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995225, metric_name: "cpu_usage", value: 78.9, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995230, metric_name: "cpu_usage", value: 82.4, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995235, metric_name: "cpu_usage", value: 73.1, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995240, metric_name: "cpu_usage", value: 76.8, tags: [("host", "server-1")], resource: "server-1" },
    { timestamp: 1640995245, metric_name: "cpu_usage", value: 79.5, tags: [("host", "server-1")], resource: "server-1" }
  ]
  
  // Test fixed sampling (every 3rd point)
  let fixed_sampler = create_sampler("fixed-sampler", "Fixed Sampler", SamplingStrategy::Fixed(3))
  
  let mut fixed_sampler_state = fixed_sampler
  for data in data_points {
    fixed_sampler_state = sample_data(fixed_sampler_state, data, data.timestamp)
  }
  
  let fixed_samples = get_samples(fixed_sampler_state)
  
  // Should have samples 0, 3, 6, 9 (every 3rd)
  assert_eq(fixed_samples.length(), 4)
  assert_eq(fixed_samples[0].timestamp, 1640995200)
  assert_eq(fixed_samples[1].timestamp, 1640995215)
  assert_eq(fixed_samples[2].timestamp, 1640995230)
  assert_eq(fixed_samples[3].timestamp, 1640995245)
  
  // Test time window sampling (max 3 per window)
  let window_sampler = create_sampler("window-sampler", "Window Sampler", SamplingStrategy::TimeWindow(3))
  
  let mut window_sampler_state = window_sampler
  for data in data_points {
    window_sampler_state = sample_data(window_sampler_state, data, data.timestamp)
  }
  
  let window_samples = get_samples(window_sampler_state)
  
  // Should have at most 3 samples (since all in same time window)
  assert_eq(window_samples.length(), 3)
  
  // Test reservoir sampling (keep 5 samples)
  let reservoir_sampler = create_sampler("reservoir-sampler", "Reservoir Sampler", SamplingStrategy::Reservoir(5))
  
  let mut reservoir_sampler_state = reservoir_sampler
  for data in data_points {
    reservoir_sampler_state = sample_data(reservoir_sampler_state, data, data.timestamp)
  }
  
  let reservoir_samples = get_samples(reservoir_sampler_state)
  
  // Should have exactly 5 samples
  assert_eq(reservoir_samples.length(), 5)
  
  // Test random sampling (50% rate)
  let random_sampler = create_sampler("random-sampler", "Random Sampler", SamplingStrategy::Random(0.5))
  
  let mut random_sampler_state = random_sampler
  for data in data_points {
    random_sampler_state = sample_data(random_sampler_state, data, data.timestamp)
  }
  
  let random_samples = get_samples(random_sampler_state)
  
  // Should have approximately 50% of samples (around 5)
  assert_true(random_samples.length() >= 3 and random_samples.length() <= 7)
}

// Test 6: Telemetry Data Routing
test "telemetry data routing functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define routing rule
  type RoutingRule = {
    id: String,
    name: String,
    condition: RoutingCondition,
    destination: String
  }
  
  // Define routing condition
  enum RoutingCondition {
    MetricName(String)
    Tag(String, String)  // key, value
    Resource(String)
    ValueRange(Float, Float)  // min, max
    Custom(String)  // Custom condition name
  }
  
  // Define router
  type Router = {
    id: String,
    name: String,
    rules: Array[RoutingRule],
    default_destination: String
  }
  
  // Define routing result
  type RoutingResult = {
    data: TelemetryData,
    destination: String,
    matched_rule: Option[String]
  }
  
  // Check if data matches routing condition
  let matches_condition = fn(data: TelemetryData, condition: RoutingCondition) {
    match condition {
      RoutingCondition::MetricName(name) => data.metric_name == name
      RoutingCondition::Tag(key, value) => {
        let tag_value = data.tags.find(fn((k, v)) { k == key })
        match tag_value {
          Some((_, v)) => v == value
          None => false
        }
      }
      RoutingCondition::Resource(resource) => data.resource == resource
      RoutingCondition::ValueRange(min, max) => data.value >= min and data.value <= max
      RoutingCondition::Custom(condition_name) => {
        // Simplified custom conditions
        match condition_name {
          "high_cpu" => data.metric_name == "cpu_usage" and data.value > 80.0
          "high_memory" => data.metric_name == "memory_usage" and data.value > 80.0
          "critical_error" => data.metric_name == "error_rate" and data.value > 10.0
          _ => false
        }
      }
    }
  }
  
  // Route data point
  let route_data = fn(router: Router, data: TelemetryData) {
    let mut matched_rule = None
    let mut destination = router.default_destination
    
    for rule in router.rules {
      if matches_condition(data, rule.condition) {
        matched_rule = Some(rule.id)
        destination = rule.destination
        // Use first matching rule
        break
      }
    }
    
    {
      data,
      destination,
      matched_rule
    }
  }
  
  // Route multiple data points
  let route_batch = fn(router: Router, data_points: Array[TelemetryData]) {
    data_points.map(fn(data) { route_data(router, data) })
  }
  
  // Create router
  let router = {
    id: "main-router",
    name: "Main Telemetry Router",
    rules: [
      {
        id: "cpu-rule",
        name: "CPU Metrics Rule",
        condition: RoutingCondition::MetricName("cpu_usage"),
        destination: "cpu-processor"
      },
      {
        id: "memory-rule",
        name: "Memory Metrics Rule",
        condition: RoutingCondition::MetricName("memory_usage"),
        destination: "memory-processor"
      },
      {
        id: "high-cpu-rule",
        name: "High CPU Rule",
        condition: RoutingCondition::Custom("high_cpu"),
        destination: "alerting-system"
      },
      {
        id: "west-region-rule",
        name: "West Region Rule",
        condition: RoutingCondition::Tag("region", "us-west"),
        destination: "west-region-storage"
      },
      {
        id: "server-1-rule",
        name: "Server 1 Rule",
        condition: RoutingCondition::Resource("server-1"),
        destination: "server-1-storage"
      }
    ],
    default_destination: "default-storage"
  }
  
  // Create sample data points
  let data_points = [
    { timestamp: 1640995200, metric_name: "cpu_usage", value: 75.5, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995205, metric_name: "memory_usage", value: 60.2, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995210, metric_name: "cpu_usage", value: 85.3, tags: [("host", "server-2"), ("region", "us-east")], resource: "server-2" },
    { timestamp: 1640995215, metric_name: "disk_usage", value: 45.8, tags: [("host", "server-1"), ("region", "us-west")], resource: "server-1" },
    { timestamp: 1640995220, metric_name: "error_rate", value: 12.5, tags: [("host", "server-3"), ("region", "us-west")], resource: "server-3" },
    { timestamp: 1640995225, metric_name: "network_usage", value: 120.5, tags: [("host", "server-2"), ("region", "us-east")], resource: "server-2" }
  ]
  
  // Route data points
  let routing_results = route_batch(router, data_points)
  
  // Verify routing results
  assert_eq(routing_results.length(), 6)
  
  // CPU usage from server-1 should match cpu-rule (first matching rule)
  let result1 = routing_results[0]
  assert_eq(result1.destination, "cpu-processor")
  assert_eq(result1.matched_rule.unwrap(), "cpu-rule")
  
  // Memory usage should match memory-rule
  let result2 = routing_results[1]
  assert_eq(result2.destination, "memory-processor")
  assert_eq(result2.matched_rule.unwrap(), "memory-rule")
  
  // High CPU usage should match high-cpu-rule (before cpu-rule)
  let result3 = routing_results[2]
  assert_eq(result3.destination, "alerting-system")
  assert_eq(result3.matched_rule.unwrap(), "high-cpu-rule")
  
  // Disk usage should match server-1-rule (no specific metric rule)
  let result4 = routing_results[3]
  assert_eq(result4.destination, "server-1-storage")
  assert_eq(result4.matched_rule.unwrap(), "server-1-rule")
  
  // Error rate should match west-region-rule (no specific rule for error_rate)
  let result5 = routing_results[4]
  assert_eq(result5.destination, "west-region-storage")
  assert_eq(result5.matched_rule.unwrap(), "west-region-rule")
  
  // Network usage should use default destination (no matching rules)
  let result6 = routing_results[5]
  assert_eq(result6.destination, "default-storage")
  assert_eq(result6.matched_rule, None)
  
  // Test rule priority (first matching rule wins)
  let priority_router = {
    id: "priority-router",
    name: "Priority Router",
    rules: [
      {
        id: "high-priority-rule",
        name: "High Priority Rule",
        condition: RoutingCondition::Tag("region", "us-west"),
        destination: "priority-storage"
      },
      {
        id: "low-priority-rule",
        name: "Low Priority Rule",
        condition: RoutingCondition::Resource("server-1"),
        destination: "low-priority-storage"
      }
    ],
    default_destination: "default-storage"
  }
  
  let priority_data = {
    timestamp: 1640995230,
    metric_name: "test_metric",
    value: 50.0,
    tags: [("region", "us-west")],
    resource: "server-1"
  }
  
  let priority_result = route_data(priority_router, priority_data)
  
  // Should match high-priority-rule even though it also matches low-priority-rule
  assert_eq(priority_result.destination, "priority-storage")
  assert_eq(priority_result.matched_rule.unwrap(), "high-priority-rule")
}

// Test 7: Telemetry Data Buffering
test "telemetry data buffering functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define buffer
  type Buffer = {
    id: String,
    name: String,
    data: Array[TelemetryData],
    max_size: Int,
    flush_interval: Int,
    last_flush: Int,
    is_active: Bool
  }
  
  // Define flush result
  type FlushResult = {
    flushed_data: Array[TelemetryData],
    remaining_data: Array[TelemetryData],
    buffer: Buffer
  }
  
  // Create buffer
  let create_buffer = fn(id: String, name: String, max_size: Int, flush_interval: Int) {
    {
      id,
      name,
      data: [],
      max_size,
      flush_interval,
      last_flush: 0,
      is_active: true
    }
  }
  
  // Add data to buffer
  let add_to_buffer = fn(buffer: Buffer, data: TelemetryData, current_time: Int) {
    if not(buffer.is_active) {
      buffer
    } else {
      let updated_data = buffer.data.push(data)
      { buffer | data: updated_data }
    }
  }
  
  // Check if buffer should flush
  let should_flush = fn(buffer: Buffer, current_time: Int) {
    (buffer.data.length() >= buffer.max_size) or 
    (current_time - buffer.last_flush >= buffer.flush_interval)
  }
  
  // Flush buffer
  let flush_buffer = fn(buffer: Buffer, current_time: Int) {
    if buffer.data.length() == 0 {
      {
        flushed_data: [],
        remaining_data: [],
        buffer: { buffer | last_flush: current_time }
      }
    } else {
      let flushed_data = buffer.data
      let updated_buffer = {
        data: [],
        last_flush: current_time,
        id: buffer.id,
        name: buffer.name,
        max_size: buffer.max_size,
        flush_interval: buffer.flush_interval,
        is_active: buffer.is_active
      }
      
      {
        flushed_data,
        remaining_data: [],
        buffer: updated_buffer
      }
    }
  }
  
  // Add data with automatic flush
  let add_with_flush = fn(buffer: Buffer, data: TelemetryData, current_time: Int) {
    let updated_buffer = add_to_buffer(buffer, data, current_time)
    
    if should_flush(updated_buffer, current_time) {
      flush_buffer(updated_buffer, current_time)
    } else {
      {
        flushed_data: [],
        remaining_data: [],
        buffer: updated_buffer
      }
    }
  }
  
  // Create buffer
  let buffer = create_buffer("main-buffer", "Main Buffer", 3, 60)  // Max 3 items, 60s interval
  
  // Create sample data
  let data1 = { timestamp: 1640995200, metric_name: "cpu_usage", value: 75.5, tags: [], resource: "server-1" }
  let data2 = { timestamp: 1640995205, metric_name: "memory_usage", value: 60.2, tags: [], resource: "server-1" }
  let data3 = { timestamp: 1640995210, metric_name: "disk_usage", value: 45.8, tags: [], resource: "server-1" }
  let data4 = { timestamp: 1640995215, metric_name: "network_usage", value: 120.5, tags: [], resource: "server-1" }
  
  // Add data to buffer
  let result1 = add_with_flush(buffer, data1, 1640995200)
  assert_eq(result1.flushed_data.length(), 0)
  assert_eq(result1.buffer.data.length(), 1)
  
  let result2 = add_with_flush(result1.buffer, data2, 1640995205)
  assert_eq(result2.flushed_data.length(), 0)
  assert_eq(result2.buffer.data.length(), 2)
  
  // Adding third data should trigger flush (max size reached)
  let result3 = add_with_flush(result2.buffer, data3, 1640995210)
  assert_eq(result3.flushed_data.length(), 3)
  assert_eq(result3.buffer.data.length(), 0)
  assert_eq(result3.buffer.last_flush, 1640995210)
  
  // Add more data
  let result4 = add_with_flush(result3.buffer, data4, 1640995215)
  assert_eq(result4.flushed_data.length(), 0)
  assert_eq(result4.buffer.data.length(), 1)
  
  // Test time-based flush
  let time_buffer = create_buffer("time-buffer", "Time Buffer", 10, 30)  // 30s interval
  
  let time_result1 = add_with_flush(time_buffer, data1, 1640995200)
  assert_eq(time_result1.flushed_data.length(), 0)
  
  let time_result2 = add_with_flush(time_result1.buffer, data2, 1640995205)
  assert_eq(time_result2.flushed_data.length(), 0)
  
  // Adding data after interval should trigger flush
  let time_result3 = add_with_flush(time_result2.buffer, data3, 1640995240)  // 40s later
  assert_eq(time_result3.flushed_data.length(), 2)
  assert_eq(time_result3.buffer.data.length(), 1)
  assert_eq(time_result3.buffer.last_flush, 1640995240)
  
  // Test inactive buffer
  let inactive_buffer = { buffer | is_active: false }
  let inactive_result = add_with_flush(inactive_buffer, data1, 1640995200)
  
  assert_eq(inactive_result.flushed_data.length(), 0)
  assert_eq(inactive_result.buffer.data.length(), 0)
  
  // Test manual flush
  let manual_buffer = create_buffer("manual-buffer", "Manual Buffer", 10, 60)
  
  let manual_result1 = add_with_flush(manual_buffer, data1, 1640995200)
  let manual_result2 = add_with_flush(manual_result1.buffer, data2, 1640995205)
  
  // Manual flush
  let manual_flush = flush_buffer(manual_result2.buffer, 1640995210)
  
  assert_eq(manual_flush.flushed_data.length(), 2)
  assert_eq(manual_flush.remaining_data.length(), 0)
  assert_eq(manual_flush.buffer.data.length(), 0)
  assert_eq(manual_flush.buffer.last_flush, 1640995210)
}

// Test 8: Telemetry Data Persistence
test "telemetry data persistence functionality" {
  // Define telemetry data point
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array[(String, String)],
    resource: String
  }
  
  // Define storage backend
  enum StorageBackend {
    Memory
    File(String)  // File path
    Database(String)  // Connection string
  }
  
  // Define persistence configuration
  type PersistenceConfig = {
    backend: StorageBackend,
    batch_size: Int,
    flush_interval: Int,
    compression: Bool,
    encryption: Bool
  }
  
  // Define persistence manager
  type PersistenceManager = {
    id: String,
    name: String,
    config: PersistenceConfig,
    pending_data: Array[TelemetryData],
    stored_data: Array[TelemetryData>,
    last_flush: Int
  }
  
  // Create persistence manager
  let create_manager = fn(id: String, name: String, config: PersistenceConfig) {
    {
      id,
      name,
      config,
      pending_data: [],
      stored_data: [],
      last_flush: 0
    }
  }
  
  // Add data to pending queue
  let add_data = fn(manager: PersistenceManager, data: TelemetryData) {
    let updated_pending = manager.pending_data.push(data)
    { manager | pending_data: updated_pending }
  }
  
  // Check if should flush
  let should_flush = fn(manager: PersistenceManager, current_time: Int) {
    (manager.pending_data.length() >= manager.config.batch_size) or
    (current_time - manager.last_flush >= manager.config.flush_interval)
  }
  
  // Persist data (simplified - just move from pending to stored)
  let persist_data = fn(manager: PersistenceManager, current_time: Int) {
    if manager.pending_data.length() == 0 {
      manager
    } else {
      let data_to_store = manager.pending_data
      let updated_stored = manager.stored_data.concat(data_to_store)
      
      {
        pending_data: [],
        stored_data: updated_stored,
        last_flush: current_time,
        id: manager.id,
        name: manager.name,
        config: manager.config
      }
    }
  }
  
  // Add data with automatic flush
  let add_with_persistence = fn(manager: PersistenceManager, data: TelemetryData, current_time: Int) {
    let updated_manager = add_data(manager, data)
    
    if should_flush(updated_manager, current_time) {
      persist_data(updated_manager, current_time)
    } else {
      updated_manager
    }
  }
  
  // Query stored data
  let query_data = fn(manager: PersistenceManager, predicate: TelemetryData -> Bool) {
    manager.stored_data.filter(predicate)
  }
  
  // Create persistence manager
  let config = {
    backend: StorageBackend::Memory,
    batch_size: 3,
    flush_interval: 60,
    compression: false,
    encryption: false
  }
  
  let manager = create_manager("main-manager", "Main Persistence Manager", config)
  
  // Create sample data
  let data1 = { timestamp: 1640995200, metric_name: "cpu_usage", value: 75.5, tags: [("host", "server-1")], resource: "server-1" }
  let data2 = { timestamp: 1640995205, metric_name: "memory_usage", value: 60.2, tags: [("host", "server-1")], resource: "server-1" }
  let data3 = { timestamp: 1640995210, metric_name: "disk_usage", value: 45.8, tags: [("host", "server-1")], resource: "server-1" }
  let data4 = { timestamp: 1640995215, metric_name: "network_usage", value: 120.5, tags: [("host", "server-1")], resource: "server-1" }
  
  // Add data to manager
  let manager1 = add_with_persistence(manager, data1, 1640995200)
  assert_eq(manager1.pending_data.length(), 1)
  assert_eq(manager1.stored_data.length(), 0)
  
  let manager2 = add_with_persistence(manager1, data2, 1640995205)
  assert_eq(manager2.pending_data.length(), 2)
  assert_eq(manager2.stored_data.length(), 0)
  
  // Adding third data should trigger persistence (batch size reached)
  let manager3 = add_with_persistence(manager2, data3, 1640995210)
  assert_eq(manager3.pending_data.length(), 0)
  assert_eq(manager3.stored_data.length(), 3)
  assert_eq(manager3.last_flush, 1640995210)
  
  // Add more data
  let manager4 = add_with_persistence(manager3, data4, 1640995215)
  assert_eq(manager4.pending_data.length(), 1)
  assert_eq(manager4.stored_data.length(), 3)
  
  // Test time-based flush
  let time_config = {
    backend: StorageBackend::Memory,
    batch_size: 10,
    flush_interval: 30,
    compression: false,
    encryption: false
  }
  
  let time_manager = create_manager("time-manager", "Time Manager", time_config)
  
  let time_manager1 = add_with_persistence(time_manager, data1, 1640995200)
  let time_manager2 = add_with_persistence(time_manager1, data2, 1640995205)
  
  // Adding data after interval should trigger persistence
  let time_manager3 = add_with_persistence(time_manager2, data3, 1640995240)  // 40s later
  assert_eq(time_manager3.pending_data.length(), 0)
  assert_eq(time_manager3.stored_data.length(), 2)
  assert_eq(time_manager3.last_flush, 1640995240)
  
  // Test querying
  let cpu_data = query_data(manager4, fn(data) { data.metric_name == "cpu_usage" })
  assert_eq(cpu_data.length(), 1)
  assert_eq(cpu_data[0].metric_name, "cpu_usage")
  
  let server1_data = query_data(manager4, fn(data) { data.resource == "server-1" })
  assert_eq(server1_data.length(), 3)  // cpu, memory, disk
  
  let recent_data = query_data(manager4, fn(data) { data.timestamp >= 1640995205 })
  assert_eq(recent_data.length(), 2)  // memory, disk
  
  // Test manual persistence
  let manual_manager = create_manager("manual-manager", "Manual Manager", config)
  
  let manual_manager1 = add_data(manual_manager, data1)
  let manual_manager2 = add_data(manual_manager1, data2)
  
  // Manual flush
  let manual_manager3 = persist_data(manual_manager2, 1640995210)
  
  assert_eq(manual_manager3.pending_data.length(), 0)
  assert_eq(manual_manager3.stored_data.length(), 2)
  assert_eq(manual_manager3.last_flush, 1640995210)
}