// Azimuth Telemetry System - Data Processing Tests
// This file contains test cases for telemetry data processing functionality

// Test 1: Telemetry Data Aggregation
test "telemetry data aggregation" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data points
  let data_points = [
    TelemetryDataPoint::new("cpu.usage", 75.5, 1000),
    TelemetryDataPoint::new("cpu.usage", 80.2, 2000),
    TelemetryDataPoint::new("cpu.usage", 65.8, 3000),
    TelemetryDataPoint::new("memory.usage", 45.3, 1000),
    TelemetryDataPoint::new("memory.usage", 52.7, 2000)
  ]
  
  // Process data points
  for point in data_points {
    TelemetryProcessor::add_data_point(processor, point)
  }
  
  // Test aggregation results
  let cpu_avg = TelemetryProcessor::get_average(processor, "cpu.usage")
  match cpu_avg {
    Some(value) => {
      assert_true(value > 70.0 && value < 75.0) // Should be around 73.83
    }
    None => assert_true(false)
  }
  
  let memory_avg = TelemetryProcessor::get_average(processor, "memory.usage")
  match memory_avg {
    Some(value) => {
      assert_true(value > 48.0 && value < 50.0) // Should be around 49.0
    }
    None => assert_true(false)
  }
  
  // Test max values
  let cpu_max = TelemetryProcessor::get_max(processor, "cpu.usage")
  match cpu_max {
    Some(value) => assert_eq(value, 80.2)
    None => assert_true(false)
  }
  
  // Test min values
  let cpu_min = TelemetryProcessor::get_min(processor, "cpu.usage")
  match cpu_min {
    Some(value) => assert_eq(value, 65.8)
    None => assert_true(false)
  }
}

// Test 2: Telemetry Data Filtering
test "telemetry data filtering" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data with different attributes
  let data_points = [
    TelemetryDataPoint::with_attributes("response.time", 120.5, 1000, [("service", "api")]),
    TelemetryDataPoint::with_attributes("response.time", 85.3, 2000, [("service", "web")]),
    TelemetryDataPoint::with_attributes("response.time", 95.7, 3000, [("service", "api")]),
    TelemetryDataPoint::with_attributes("error.rate", 0.02, 1000, [("service", "api")]),
    TelemetryDataPoint::with_attributes("error.rate", 0.01, 2000, [("service", "web")])
  ]
  
  // Process data points
  for point in data_points {
    TelemetryProcessor::add_data_point(processor, point)
  }
  
  // Test filtering by attribute
  let api_response_times = TelemetryProcessor::filter_by_attribute(processor, "response.time", "service", "api")
  assert_eq(api_response_times.length(), 2)
  
  let web_response_times = TelemetryProcessor::filter_by_attribute(processor, "response.time", "service", "web")
  assert_eq(web_response_times.length(), 1)
  
  // Test filtering by metric name
  let error_rates = TelemetryProcessor::filter_by_metric(processor, "error.rate")
  assert_eq(error_rates.length(), 2)
  
  // Test filtering by time range
  let recent_data = TelemetryProcessor::filter_by_time_range(processor, 1500, 3500)
  assert_eq(recent_data.length(), 3)
}

// Test 3: Telemetry Data Transformation
test "telemetry data transformation" {
  let processor = TelemetryProcessor::new()
  
  // Create sample telemetry data
  let data_points = [
    TelemetryDataPoint::new("temperature.celsius", 25.5, 1000),
    TelemetryDataPoint::new("temperature.celsius", 30.2, 2000),
    TelemetryDataPoint::new("distance.km", 5.2, 1000),
    TelemetryDataPoint::new("distance.km", 3.8, 2000)
  ]
  
  // Process data points
  for point in data_points {
    TelemetryProcessor::add_data_point(processor, point)
  }
  
  // Test unit transformation
  let fahrenheit_data = TelemetryProcessor::transform_units(processor, "temperature.celsius", "fahrenheit")
  assert_eq(fahrenheit_data.length(), 2)
  
  let first_fahrenheit = fahrenheit_data[0]
  assert_true(first_fahrenheit.value > 77.0 && first_fahrenheit.value < 78.0) // 25.5°C ≈ 77.9°F
  
  // Test distance transformation
  let mile_data = TelemetryProcessor::transform_units(processor, "distance.km", "miles")
  assert_eq(mile_data.length(), 2)
  
  let first_mile = mile_data[0]
  assert_true(first_mile.value > 3.2 && first_mile.value < 3.3) // 5.2km ≈ 3.23 miles
}