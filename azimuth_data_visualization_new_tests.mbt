// Azimuth Data Visualization Tests
// This file contains test cases for data visualization functionality

// Test 1: Time Series Chart Generation
test "time series chart generation" {
  // Create time series data
  let time_series_data = [
    (1640995200, 10.5),   // 2022-01-01 00:00:00
    (1640995260, 15.2),   // 2022-01-01 00:01:00
    (1640995320, 12.8),   // 2022-01-01 00:02:00
    (1640995380, 18.3),   // 2022-01-01 00:03:00
    (1640995440, 22.1),   // 2022-01-01 00:04:00
    (1640995500, 19.7),   // 2022-01-01 00:05:00
    (1640995560, 25.4),   // 2022-01-01 00:06:00
    (1640995620, 28.9),   // 2022-01-01 00:07:00
    (1640995680, 24.6),   // 2022-01-01 00:08:00
    (1640995740, 30.2)    // 2022-01-01 00:09:00
  ]
  
  // Define chart generation functions
  let generate_time_series_chart = fn(data: Array[(Int, Float)], title: String, x_label: String, y_label: String) {
    // Calculate min and max values
    let timestamps = data.map(fn(point) { point.0 })
    let values = data.map(fn(point) { point.1 })
    
    let min_timestamp = timestamps.reduce(fn(acc, ts) { if ts < acc { ts } else { acc } }, timestamps[0])
    let max_timestamp = timestamps.reduce(fn(acc, ts) { if ts > acc { ts } else { acc } }, timestamps[0])
    let min_value = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
    let max_value = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    
    // Generate chart configuration
    {
      type: "time_series",
      title,
      x_label,
      y_label,
      data,
      x_range: (min_timestamp, max_timestamp),
      y_range: (min_value, max_value),
      data_points: data.length()
    }
  }
  
  let format_chart_config = fn(chart: {type: String, title: String, x_label: String, y_label: String, data: Array[(Int, Float)], x_range: (Int, Int), y_range: (Float, Float), data_points: Int}) {
    "{"
      + "\"type\":\"" + chart.type + "\","
      + "\"title\":\"" + chart.title + "\","
      + "\"x_label\":\"" + chart.x_label + "\","
      + "\"y_label\":\"" + chart.y_label + "\","
      + "\"x_range\":[" + chart.x_range.0.to_string() + "," + chart.x_range.1.to_string() + "],"
      + "\"y_range\":[" + chart.y_range.0.to_string() + "," + chart.y_range.1.to_string() + "],"
      + "\"data_points\":" + chart.data_points.to_string() + ","
      + "\"data\":["
      + data.map(fn(point) { "[" + point.0.to_string() + "," + point.1.to_string() + "]" }).join(",")
      + "]"
    + "}"
  }
  
  // Generate chart
  let chart = generate_time_series_chart(time_series_data, "Response Time Over Time", "Time", "Response Time (ms)")
  
  // Verify chart properties
  assert_eq(chart.type, "time_series")
  assert_eq(chart.title, "Response Time Over Time")
  assert_eq(chart.x_label, "Time")
  assert_eq(chart.y_label, "Response Time (ms)")
  assert_eq(chart.data_points, 10)
  assert_eq(chart.x_range.0, 1640995200)
  assert_eq(chart.x_range.1, 1640995740)
  assert_eq(chart.y_range.0, 10.5)
  assert_eq(chart.y_range.1, 30.2)
  
  // Format chart configuration
  let chart_config = format_chart_config(chart)
  
  // Verify chart configuration contains expected data
  assert_true(chart_config.contains("\"type\":\"time_series\""))
  assert_true(chart_config.contains("\"title\":\"Response Time Over Time\""))
  assert_true(chart_config.contains("\"data_points\":10"))
  assert_true(chart_config.contains("[1640995200,10.5]"))
  assert_true(chart_config.contains("[1640995740,30.2]"))
}

// Test 2: Histogram Chart Generation
test "histogram chart generation" {
  // Create histogram data
  let histogram_data = [
    (0.0, 5),    // 0-10ms: 5 requests
    (10.0, 15),  // 10-20ms: 15 requests
    (20.0, 25),  // 20-30ms: 25 requests
    (30.0, 30),  // 30-40ms: 30 requests
    (40.0, 20),  // 40-50ms: 20 requests
    (50.0, 10),  // 50-60ms: 10 requests
    (60.0, 5),   // 60-70ms: 5 requests
    (70.0, 2)    // 70-80ms: 2 requests
  ]
  
  // Define histogram generation functions
  let generate_histogram = fn(data: Array[(Float, Int)], title: String, x_label: String, y_label: String) {
    // Calculate statistics
    let values = data.map(fn(point) { point.1 })
    let counts = data.map(fn(point) { point.1 })
    
    let total_requests = counts.reduce(fn(acc, count) { acc + count }, 0)
    let max_count = counts.reduce(fn(acc, count) { if count > acc { count } else { acc } }, counts[0])
    let avg_bucket_size = total_requests.to_float() / data.length().to_float()
    
    // Find bucket with most requests
    let max_bucket = data.reduce(fn(acc, point) { 
      if point.1 > acc.1 { point } else { acc } 
    }, data[0])
    
    {
      type: "histogram",
      title,
      x_label,
      y_label,
      data,
      total_requests,
      max_count,
      avg_bucket_size,
      peak_bucket: max_bucket
    }
  }
  
  let format_histogram_config = fn(histogram: {type: String, title: String, x_label: String, y_label: String, data: Array[(Float, Int)], total_requests: Int, max_count: Int, avg_bucket_size: Float, peak_bucket: (Float, Int)}) {
    "{"
      + "\"type\":\"" + histogram.type + "\","
      + "\"title\":\"" + histogram.title + "\","
      + "\"x_label\":\"" + histogram.x_label + "\","
      + "\"y_label\":\"" + histogram.y_label + "\","
      + "\"total_requests\":" + histogram.total_requests.to_string() + ","
      + "\"max_count\":" + histogram.max_count.to_string() + ","
      + "\"avg_bucket_size\":" + histogram.avg_bucket_size.to_string() + ","
      + "\"peak_bucket\":[" + histogram.peak_bucket.0.to_string() + "," + histogram.peak_bucket.1.to_string() + "],"
      + "\"data\":["
      + histogram.data.map(fn(point) { "[" + point.0.to_string() + "," + point.1.to_string() + "]" }).join(",")
      + "]"
    + "}"
  }
  
  // Generate histogram
  let histogram = generate_histogram(histogram_data, "Response Time Distribution", "Response Time (ms)", "Request Count")
  
  // Verify histogram properties
  assert_eq(histogram.type, "histogram")
  assert_eq(histogram.title, "Response Time Distribution")
  assert_eq(histogram.x_label, "Response Time (ms)")
  assert_eq(histogram.y_label, "Request Count")
  assert_eq(histogram.total_requests, 112)
  assert_eq(histogram.max_count, 30)
  assert_eq(histogram.avg_bucket_size, 14.0)
  assert_eq(histogram.peak_bucket, (30.0, 30))
  
  // Format histogram configuration
  let histogram_config = format_histogram_config(histogram)
  
  // Verify histogram configuration contains expected data
  assert_true(histogram_config.contains("\"type\":\"histogram\""))
  assert_true(histogram_config.contains("\"title\":\"Response Time Distribution\""))
  assert_true(histogram_config.contains("\"total_requests\":112"))
  assert_true(histogram_config.contains("\"max_count\":30"))
  assert_true(histogram_config.contains("[30.0,30]"))
}

// Test 3: Pie Chart Generation
test "pie chart generation" {
  // Create pie chart data
  let pie_data = [
    ("HTTP GET", 450),
    ("HTTP POST", 320),
    ("HTTP PUT", 120),
    ("HTTP DELETE", 80),
    ("HTTP PATCH", 30)
  ]
  
  // Define pie chart generation functions
  let generate_pie_chart = fn(data: Array[(String, Int)], title: String) {
    // Calculate total and percentages
    let values = data.map(fn(item) { item.1 })
    let total = values.reduce(fn(acc, value) { acc + value }, 0)
    
    let pie_data_with_percentages = data.map(fn(item) {
      let percentage = (item.1.to_float() / total.to_float()) * 100.0
      (item.0, item.1, percentage)
    })
    
    // Find largest and smallest segments
    let largest = pie_data_with_percentages.reduce(fn(acc, item) { 
      if item.1 > acc.1 { item } else { acc } 
    }, pie_data_with_percentages[0])
    
    let smallest = pie_data_with_percentages.reduce(fn(acc, item) { 
      if item.1 < acc.1 { item } else { acc } 
    }, pie_data_with_percentages[0])
    
    {
      type: "pie",
      title,
      data: pie_data_with_percentages,
      total,
      segments: data.length(),
      largest_segment: (largest.0, largest.1, largest.2),
      smallest_segment: (smallest.0, smallest.1, smallest.2)
    }
  }
  
  let format_pie_chart_config = fn(chart: {type: String, title: String, data: Array[(String, Int, Float)], total: Int, segments: Int, largest_segment: (String, Int, Float), smallest_segment: (String, Int, Float)}) {
    "{"
      + "\"type\":\"" + chart.type + "\","
      + "\"title\":\"" + chart.title + "\","
      + "\"total\":" + chart.total.to_string() + ","
      + "\"segments\":" + chart.segments.to_string() + ","
      + "\"largest_segment\":[\"" + chart.largest_segment.0 + "\"," + chart.largest_segment.1.to_string() + "," + chart.largest_segment.2.to_string() + "],"
      + "\"smallest_segment\":[\"" + chart.smallest_segment.0 + "\"," + chart.smallest_segment.1.to_string() + "," + chart.smallest_segment.2.to_string() + "],"
      + "\"data\":["
      + chart.data.map(fn(item) { "[\"" + item.0 + "\"," + item.1.to_string() + "," + item.2.to_string() + "]" }).join(",")
      + "]"
    + "}"
  }
  
  // Generate pie chart
  let pie_chart = generate_pie_chart(pie_data, "HTTP Methods Distribution")
  
  // Verify pie chart properties
  assert_eq(pie_chart.type, "pie")
  assert_eq(pie_chart.title, "HTTP Methods Distribution")
  assert_eq(pie_chart.total, 1000)
  assert_eq(pie_chart.segments, 5)
  assert_eq(pie_chart.largest_segment, ("HTTP GET", 450, 45.0))
  assert_eq(pie_chart.smallest_segment, ("HTTP PATCH", 30, 3.0))
  
  // Format pie chart configuration
  let pie_chart_config = format_pie_chart_config(pie_chart)
  
  // Verify pie chart configuration contains expected data
  assert_true(pie_chart_config.contains("\"type\":\"pie\""))
  assert_true(pie_chart_config.contains("\"title\":\"HTTP Methods Distribution\""))
  assert_true(pie_chart_config.contains("\"total\":1000"))
  assert_true(pie_chart_config.contains("\"segments\":5"))
  assert_true(pie_chart_config.contains("[\"HTTP GET\",450,45.0]"))
  assert_true(pie_chart_config.contains("[\"HTTP PATCH\",30,3.0]"))
}

// Test 4: Heatmap Generation
test "heatmap generation" {
  // Create heatmap data (2D grid)
  let heatmap_data = [
    [1, 3, 5, 7, 9],
    [2, 4, 6, 8, 10],
    [3, 5, 7, 9, 11],
    [4, 6, 8, 10, 12],
    [5, 7, 9, 11, 13]
  ]
  
  // Define heatmap generation functions
  let generate_heatmap = fn(data: Array[Array[Int]], title: String, x_label: String, y_label: String) {
    // Calculate statistics
    let flat_data = []
    for row in data {
      for value in row {
        flat_data = flat_data.push(value)
      }
    }
    
    let min_value = flat_data.reduce(fn(acc, value) { if value < acc { value } else { acc } }, flat_data[0])
    let max_value = flat_data.reduce(fn(acc, value) { if value > acc { value } else { acc } }, flat_data[0])
    let avg_value = flat_data.reduce(fn(acc, value) { acc + value }, 0).to_float() / flat_data.length().to_float()
    
    // Find coordinates of max value
    let mut max_coords = (0, 0)
    let mut found_max = false
    for i in 0..data.length() {
      for j in 0..data[i].length() {
        if data[i][j] == max_value and not(found_max) {
          max_coords = (i, j)
          found_max = true
        }
      }
    }
    
    {
      type: "heatmap",
      title,
      x_label,
      y_label,
      data,
      rows: data.length(),
      cols: if data.length() > 0 { data[0].length() } else { 0 },
      min_value,
      max_value,
      avg_value,
      max_position: max_coords
    }
  }
  
  let format_heatmap_config = fn(heatmap: {type: String, title: String, x_label: String, y_label: String, data: Array[Array[Int]], rows: Int, cols: Int, min_value: Int, max_value: Int, avg_value: Float, max_position: (Int, Int)}) {
    "{"
      + "\"type\":\"" + heatmap.type + "\","
      + "\"title\":\"" + heatmap.title + "\","
      + "\"x_label\":\"" + heatmap.x_label + "\","
      + "\"y_label\":\"" + heatmap.y_label + "\","
      + "\"rows\":" + heatmap.rows.to_string() + ","
      + "\"cols\":" + heatmap.cols.to_string() + ","
      + "\"min_value\":" + heatmap.min_value.to_string() + ","
      + "\"max_value\":" + heatmap.max_value.to_string() + ","
      + "\"avg_value\":" + heatmap.avg_value.to_string() + ","
      + "\"max_position\":[" + heatmap.max_position.0.to_string() + "," + heatmap.max_position.1.to_string() + "],"
      + "\"data\":["
      + heatmap.data.map(fn(row) { "[" + row.map(fn(value) { value.to_string() }).join(",") + "]" }).join(",")
      + "]"
    + "}"
  }
  
  // Generate heatmap
  let heatmap = generate_heatmap(heatmap_data, "Activity Heatmap", "Hour", "Day")
  
  // Verify heatmap properties
  assert_eq(heatmap.type, "heatmap")
  assert_eq(heatmap.title, "Activity Heatmap")
  assert_eq(heatmap.x_label, "Hour")
  assert_eq(heatmap.y_label, "Day")
  assert_eq(heatmap.rows, 5)
  assert_eq(heatmap.cols, 5)
  assert_eq(heatmap.min_value, 1)
  assert_eq(heatmap.max_value, 13)
  assert_eq(heatmap.avg_value, 7.0)
  assert_eq(heatmap.max_position, (4, 4))
  
  // Format heatmap configuration
  let heatmap_config = format_heatmap_config(heatmap)
  
  // Verify heatmap configuration contains expected data
  assert_true(heatmap_config.contains("\"type\":\"heatmap\""))
  assert_true(heatmap_config.contains("\"title\":\"Activity Heatmap\""))
  assert_true(heatmap_config.contains("\"rows\":5"))
  assert_true(heatmap_config.contains("\"cols\":5"))
  assert_true(heatmap_config.contains("\"min_value\":1"))
  assert_true(heatmap_config.contains("\"max_value\":13"))
  assert_true(heatmap_config.contains("\"max_position\":[4,4]"))
}

// Test 5: Scatter Plot Generation
test "scatter plot generation" {
  // Create scatter plot data
  let scatter_data = [
    (10.5, 20.3),
    (15.2, 25.8),
    (12.8, 22.1),
    (18.3, 30.5),
    (22.1, 35.2),
    (19.7, 32.8),
    (25.4, 40.1),
    (28.9, 45.6),
    (24.6, 38.9),
    (30.2, 48.3)
  ]
  
  // Define scatter plot generation functions
  let generate_scatter_plot = fn(data: Array[(Float, Float)], title: String, x_label: String, y_label: String) {
    // Calculate statistics
    let x_values = data.map(fn(point) { point.0 })
    let y_values = data.map(fn(point) { point.1 })
    
    let x_min = x_values.reduce(fn(acc, value) { if value < acc { value } else { acc } }, x_values[0])
    let x_max = x_values.reduce(fn(acc, value) { if value > acc { value } else { acc } }, x_values[0])
    let y_min = y_values.reduce(fn(acc, value) { if value < acc { value } else { acc } }, y_values[0])
    let y_max = y_values.reduce(fn(acc, value) { if value > acc { value } else { acc } }, y_values[0])
    
    let x_avg = x_values.reduce(fn(acc, value) { acc + value }, 0.0) / x_values.length().to_float()
    let y_avg = y_values.reduce(fn(acc, value) { acc + value }, 0.0) / y_values.length().to_float()
    
    // Calculate correlation coefficient (simplified)
    let mut numerator = 0.0
    let mut x_sum_sq = 0.0
    let mut y_sum_sq = 0.0
    
    for i in 0..data.length() {
      let x_diff = data[i].0 - x_avg
      let y_diff = data[i].1 - y_avg
      numerator = numerator + (x_diff * y_diff)
      x_sum_sq = x_sum_sq + (x_diff * x_diff)
      y_sum_sq = y_sum_sq + (y_diff * y_diff)
    }
    
    let correlation = if x_sum_sq > 0.0 and y_sum_sq > 0.0 {
      numerator / (x_sum_sq.sqrt() * y_sum_sq.sqrt())
    } else {
      0.0
    }
    
    {
      type: "scatter",
      title,
      x_label,
      y_label,
      data,
      x_range: (x_min, x_max),
      y_range: (y_min, y_max),
      x_avg,
      y_avg,
      correlation,
      data_points: data.length()
    }
  }
  
  let format_scatter_plot_config = fn(plot: {type: String, title: String, x_label: String, y_label: String, data: Array[(Float, Float)], x_range: (Float, Float), y_range: (Float, Float), x_avg: Float, y_avg: Float, correlation: Float, data_points: Int}) {
    "{"
      + "\"type\":\"" + plot.type + "\","
      + "\"title\":\"" + plot.title + "\","
      + "\"x_label\":\"" + plot.x_label + "\","
      + "\"y_label\":\"" + plot.y_label + "\","
      + "\"x_range\":[" + plot.x_range.0.to_string() + "," + plot.x_range.1.to_string() + "],"
      + "\"y_range\":[" + plot.y_range.0.to_string() + "," + plot.y_range.1.to_string() + "],"
      + "\"x_avg\":" + plot.x_avg.to_string() + ","
      + "\"y_avg\":" + plot.y_avg.to_string() + ","
      + "\"correlation\":" + plot.correlation.to_string() + ","
      + "\"data_points\":" + plot.data_points.to_string() + ","
      + "\"data\":["
      + plot.data.map(fn(point) { "[" + point.0.to_string() + "," + point.1.to_string() + "]" }).join(",")
      + "]"
    + "}"
  }
  
  // Generate scatter plot
  let scatter_plot = generate_scatter_plot(scatter_data, "Response Time vs Request Size", "Request Size (KB)", "Response Time (ms)")
  
  // Verify scatter plot properties
  assert_eq(scatter_plot.type, "scatter")
  assert_eq(scatter_plot.title, "Response Time vs Request Size")
  assert_eq(scatter_plot.x_label, "Request Size (KB)")
  assert_eq(scatter_plot.y_label, "Response Time (ms)")
  assert_eq(scatter_plot.data_points, 10)
  assert_eq(scatter_plot.x_range.0, 10.5)
  assert_eq(scatter_plot.x_range.1, 30.2)
  assert_eq(scatter_plot.y_range.0, 20.3)
  assert_eq(scatter_plot.y_range.1, 48.3)
  assert_true(scatter_plot.correlation > 0.9)  // Strong positive correlation
  
  // Format scatter plot configuration
  let scatter_plot_config = format_scatter_plot_config(scatter_plot)
  
  // Verify scatter plot configuration contains expected data
  assert_true(scatter_plot_config.contains("\"type\":\"scatter\""))
  assert_true(scatter_plot_config.contains("\"title\":\"Response Time vs Request Size\""))
  assert_true(scatter_plot_config.contains("\"data_points\":10"))
  assert_true(scatter_plot_config.contains("[10.5,20.3]"))
  assert_true(scatter_plot_config.contains("[30.2,48.3]"))
}

// Test 6: Dashboard Layout Generation
test "dashboard layout generation" {
  // Create dashboard components
  let dashboard_components = [
    {
      id: "response_time_chart",
      type: "time_series",
      title: "Response Time Trend",
      position: (0, 0),
      size: (6, 4)
    },
    {
      id: "request_count_chart",
      type: "histogram",
      title: "Request Count Distribution",
      position: (6, 0),
      size: (6, 4)
    },
    {
      id: "error_rate_chart",
      type: "pie",
      title: "Error Rate by Service",
      position: (0, 4),
      size: (4, 4)
    },
    {
      id: "throughput_heatmap",
      type: "heatmap",
      title: "Throughput Heatmap",
      position: (4, 4),
      size: (8, 4)
    }
  ]
  
  // Define dashboard generation functions
  let generate_dashboard = fn(components: Array[{id: String, type: String, title: String, position: (Int, Int), size: (Int, Int)}], title: String) {
    // Calculate dashboard grid size
    let max_x = components.reduce(fn(acc, comp) { 
      let comp_max_x = comp.position.0 + comp.size.0
      if comp_max_x > acc { comp_max_x } else { acc }
    }, 0)
    
    let max_y = components.reduce(fn(acc, comp) { 
      let comp_max_y = comp.position.1 + comp.size.1
      if comp_max_y > acc { comp_max_y } else { acc }
    }, 0)
    
    // Count component types
    let component_types = {}
    for comp in components {
      match component_types.get(comp.type) {
        Some(count) => component_types[comp.type] = count + 1
        None => component_types[comp.type] = 1
      }
    }
    
    {
      type: "dashboard",
      title,
      components,
      grid_size: (max_x, max_y),
      component_count: components.length(),
      component_types
    }
  }
  
  let format_dashboard_config = fn(dashboard: {type: String, title: String, components: Array[{id: String, type: String, title: String, position: (Int, Int), size: (Int, Int)}], grid_size: (Int, Int), component_count: Int, component_types: {String: Int}}) {
    "{"
      + "\"type\":\"" + dashboard.type + "\","
      + "\"title\":\"" + dashboard.title + "\","
      + "\"grid_size\":[" + dashboard.grid_size.0.to_string() + "," + dashboard.grid_size.1.to_string() + "],"
      + "\"component_count\":" + dashboard.component_count.to_string() + ","
      + "\"component_types\":{"
      + dashboard.component_types.map(fn(entry) { "\"" + entry.0 + "\":" + entry.1.to_string() }).join(",")
      + "},"
      + "\"components\":["
      + dashboard.components.map(fn(comp) { 
        "{"
          + "\"id\":\"" + comp.id + "\","
          + "\"type\":\"" + comp.type + "\","
          + "\"title\":\"" + comp.title + "\","
          + "\"position\":[" + comp.position.0.to_string() + "," + comp.position.1.to_string() + "],"
          + "\"size\":[" + comp.size.0.to_string() + "," + comp.size.1.to_string() + "]"
        + "}"
      }).join(",")
      + "]"
    + "}"
  }
  
  // Generate dashboard
  let dashboard = generate_dashboard(dashboard_components, "System Monitoring Dashboard")
  
  // Verify dashboard properties
  assert_eq(dashboard.type, "dashboard")
  assert_eq(dashboard.title, "System Monitoring Dashboard")
  assert_eq(dashboard.component_count, 4)
  assert_eq(dashboard.grid_size, (12, 8))
  assert_eq(dashboard.component_types["time_series"], 1)
  assert_eq(dashboard.component_types["histogram"], 1)
  assert_eq(dashboard.component_types["pie"], 1)
  assert_eq(dashboard.component_types["heatmap"], 1)
  
  // Verify component properties
  assert_eq(dashboard.components[0].id, "response_time_chart")
  assert_eq(dashboard.components[0].type, "time_series")
  assert_eq(dashboard.components[0].title, "Response Time Trend")
  assert_eq(dashboard.components[0].position, (0, 0))
  assert_eq(dashboard.components[0].size, (6, 4))
  
  // Format dashboard configuration
  let dashboard_config = format_dashboard_config(dashboard)
  
  // Verify dashboard configuration contains expected data
  assert_true(dashboard_config.contains("\"type\":\"dashboard\""))
  assert_true(dashboard_config.contains("\"title\":\"System Monitoring Dashboard\""))
  assert_true(dashboard_config.contains("\"component_count\":4"))
  assert_true(dashboard_config.contains("\"grid_size\":[12,8]"))
  assert_true(dashboard_config.contains("\"time_series\":1"))
  assert_true(dashboard_config.contains("\"id\":\"response_time_chart\""))
}

// Test 7: Real-time Data Streaming Visualization
test "real-time data streaming visualization" {
  // Create streaming data generator
  let streaming_data_generator = {
    current_value: 100.0,
    trend: 1.0,
    noise_factor: 5.0
  }
  
  // Define streaming functions
  let generate_next_data_point = fn(generator: {current_value: Float, trend: Float, noise_factor: Float}) {
    // Add trend and random noise
    let noise = (generator.noise_factor * 2.0 * (0.5 - 0.0))  // Simplified random between -noise_factor and +noise_factor
    let new_value = generator.current_value + generator.trend + noise
    
    // Update trend occasionally
    let new_trend = if (0.0) < 0.1 {  // 10% chance to change trend
      generator.trend * -1.0
    } else {
      generator.trend
    }
    
    {
      current_value: new_value,
      trend: new_trend,
      noise_factor: generator.noise_factor
    }
  }
  
  let generate_stream_data = fn(generator: {current_value: Float, trend: Float, noise_factor: Float}, points: Int) {
    let mut data = []
    let mut current_gen = generator
    
    for i in 0..points {
      let timestamp = 1640995200 + i * 60  // One minute intervals
      data = data.push((timestamp, current_gen.current_value))
      current_gen = generate_next_data_point(current_gen)
    }
    
    data
  }
  
  let generate_streaming_chart = fn(data: Array[(Int, Float)], title: String, update_interval: Int) {
    // Calculate statistics
    let values = data.map(fn(point) { point.1 })
    let latest_value = if values.length() > 0 { values[values.length() - 1] } else { 0.0 }
    let avg_value = if values.length() > 0 { 
      values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
    } else { 0.0 }
    
    {
      type: "streaming_time_series",
      title,
      data,
      update_interval,
      latest_value,
      avg_value,
      data_points: data.length()
    }
  }
  
  // Generate streaming data
  let stream_data = generate_stream_data(streaming_data_generator, 20)
  
  // Generate streaming chart
  let streaming_chart = generate_streaming_chart(stream_data, "Real-time Metrics", 5)  // 5 second update interval
  
  // Verify streaming chart properties
  assert_eq(streaming_chart.type, "streaming_time_series")
  assert_eq(streaming_chart.title, "Real-time Metrics")
  assert_eq(streaming_chart.update_interval, 5)
  assert_eq(streaming_chart.data_points, 20)
  assert_true(streaming_chart.latest_value > 0.0)
  assert_true(streaming_chart.avg_value > 0.0)
  
  // Verify data points are sequential
  for i in 1..stream_data.length() {
    assert_true(stream_data[i].0 > stream_data[i-1].0)  // Timestamps should be increasing
  }
}

// Test 8: Multi-axis Chart Generation
test "multi-axis chart generation" {
  // Create multi-axis data
  let multi_axis_data = {
    x_values: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    y1_values: [10, 15, 13, 17, 22, 19, 25, 28, 24, 30],  // Response time
    y2_values: [100, 120, 110, 140, 180, 160, 200, 220, 190, 240]  // Request count
  }
  
  // Define multi-axis chart functions
  let generate_multi_axis_chart = fn(data: {x_values: Array[Int], y1_values: Array[Int], y2_values: Array[Int]}, title: String, x_label: String, y1_label: String, y2_label: String) {
    // Calculate statistics for both axes
    let y1_min = data.y1_values.reduce(fn(acc, value) { if value < acc { value } else { acc } }, data.y1_values[0])
    let y1_max = data.y1_values.reduce(fn(acc, value) { if value > acc { value } else { acc } }, data.y1_values[0])
    let y1_avg = data.y1_values.reduce(fn(acc, value) { acc + value }, 0) / data.y1_values.length()
    
    let y2_min = data.y2_values.reduce(fn(acc, value) { if value < acc { value } else { acc } }, data.y2_values[0])
    let y2_max = data.y2_values.reduce(fn(acc, value) { if value > acc { value } else { acc } }, data.y2_values[0])
    let y2_avg = data.y2_values.reduce(fn(acc, value) { acc + value }, 0) / data.y2_values.length()
    
    // Combine data points
    let combined_data = []
    for i in 0..data.x_values.length() {
      combined_data = combined_data.push((data.x_values[i], data.y1_values[i], data.y2_values[i]))
    }
    
    {
      type: "multi_axis",
      title,
      x_label,
      y1_label,
      y2_label,
      data: combined_data,
      x_range: (data.x_values[0], data.x_values[data.x_values.length() - 1]),
      y1_range: (y1_min, y1_max),
      y2_range: (y2_min, y2_max),
      y1_avg,
      y2_avg,
      data_points: data.x_values.length()
    }
  }
  
  let format_multi_axis_config = fn(chart: {type: String, title: String, x_label: String, y1_label: String, y2_label: String, data: Array[(Int, Int, Int)], x_range: (Int, Int), y1_range: (Int, Int), y2_range: (Int, Int), y1_avg: Int, y2_avg: Int, data_points: Int}) {
    "{"
      + "\"type\":\"" + chart.type + "\","
      + "\"title\":\"" + chart.title + "\","
      + "\"x_label\":\"" + chart.x_label + "\","
      + "\"y1_label\":\"" + chart.y1_label + "\","
      + "\"y2_label\":\"" + chart.y2_label + "\","
      + "\"x_range\":[" + chart.x_range.0.to_string() + "," + chart.x_range.1.to_string() + "],"
      + "\"y1_range\":[" + chart.y1_range.0.to_string() + "," + chart.y1_range.1.to_string() + "],"
      + "\"y2_range\":[" + chart.y2_range.0.to_string() + "," + chart.y2_range.1.to_string() + "],"
      + "\"y1_avg\":" + chart.y1_avg.to_string() + ","
      + "\"y2_avg\":" + chart.y2_avg.to_string() + ","
      + "\"data_points\":" + chart.data_points.to_string() + ","
      + "\"data\":["
      + chart.data.map(fn(point) { "[" + point.0.to_string() + "," + point.1.to_string() + "," + point.2.to_string() + "]" }).join(",")
      + "]"
    + "}"
  }
  
  // Generate multi-axis chart
  let multi_axis_chart = generate_multi_axis_chart(
    multi_axis_data, 
    "Response Time and Request Count", 
    "Time", 
    "Response Time (ms)", 
    "Request Count"
  )
  
  // Verify multi-axis chart properties
  assert_eq(multi_axis_chart.type, "multi_axis")
  assert_eq(multi_axis_chart.title, "Response Time and Request Count")
  assert_eq(multi_axis_chart.x_label, "Time")
  assert_eq(multi_axis_chart.y1_label, "Response Time (ms)")
  assert_eq(multi_axis_chart.y2_label, "Request Count")
  assert_eq(multi_axis_chart.data_points, 10)
  assert_eq(multi_axis_chart.x_range, (1, 10))
  assert_eq(multi_axis_chart.y1_range, (10, 30))
  assert_eq(multi_axis_chart.y2_range, (100, 240))
  assert_eq(multi_axis_chart.y1_avg, 20.3)
  assert_eq(multi_axis_chart.y2_avg, 166)
  
  // Verify data points
  assert_eq(multi_axis_chart.data[0], (1, 10, 100))
  assert_eq(multi_axis_chart.data[9], (10, 30, 240))
  
  // Format multi-axis chart configuration
  let multi_axis_config = format_multi_axis_config(multi_axis_chart)
  
  // Verify multi-axis chart configuration contains expected data
  assert_true(multi_axis_config.contains("\"type\":\"multi_axis\""))
  assert_true(multi_axis_config.contains("\"title\":\"Response Time and Request Count\""))
  assert_true(multi_axis_config.contains("\"y1_label\":\"Response Time (ms)\""))
  assert_true(multi_axis_config.contains("\"y2_label\":\"Request Count\""))
  assert_true(multi_axis_config.contains("[1,10,100]"))
  assert_true(multi_axis_config.contains("[10,30,240]"))
}

// Test 9: Data Aggregation for Visualization
test "data aggregation for visualization" {
  // Create raw data
  let raw_data = []
  for i in 0..100 {
    raw_data = raw_data.push((1640995200 + i * 60, 50.0 + (i % 20).to_float()))  // 100 data points, 1 minute apart
  }
  
  // Define aggregation functions
  let aggregate_by_interval = fn(data: Array[(Int, Float)], interval_seconds: Int) {
    if data.length() == 0 {
      []
    } else {
      let start_time = data[0].0
      let end_time = data[data.length() - 1].0
      
      let mut interval_start = start_time
      let mut aggregated_data = []
      
      while interval_start <= end_time {
        let interval_end = interval_start + interval_seconds
        
        // Filter data points in this interval
        let interval_data = data.filter(fn(point) { 
          point.0 >= interval_start and point.0 < interval_end 
        })
        
        if interval_data.length() > 0 {
          let values = interval_data.map(fn(point) { point.1 })
          let avg_value = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
          let min_value = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
          let max_value = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
          
          aggregated_data = aggregated_data.push((interval_start, avg_value, min_value, max_value, interval_data.length()))
        }
        
        interval_start = interval_end
      }
      
      aggregated_data
    }
  }
  
  let generate_aggregated_chart = fn(data: Array[(Int, Float, Float, Float, Int)], title: String, interval: Int) {
    // Calculate overall statistics
    let avg_values = data.map(fn(point) { point.1 })
    let overall_avg = avg_values.reduce(fn(acc, v) { acc + v }, 0.0) / avg_values.length().to_float()
    
    let total_points = data.reduce(fn(acc, point) { acc + point.4 }, 0)
    
    {
      type: "aggregated_time_series",
      title,
      data,
      interval,
      overall_avg,
      total_points,
      aggregated_points: data.length()
    }
  }
  
  // Aggregate data by 5-minute intervals
  let aggregated_data = aggregate_by_interval(raw_data, 300)  // 5 minutes = 300 seconds
  
  // Generate aggregated chart
  let aggregated_chart = generate_aggregated_chart(aggregated_data, "Aggregated Response Time", 300)
  
  // Verify aggregation results
  assert_eq(aggregated_chart.type, "aggregated_time_series")
  assert_eq(aggregated_chart.title, "Aggregated Response Time")
  assert_eq(aggregated_chart.interval, 300)
  assert_eq(aggregated_chart.total_points, 100)
  assert_true(aggregated_chart.aggregated_points < 100)  // Should have fewer points after aggregation
  assert_true(aggregated_chart.overall_avg > 50.0)
  
  // Verify each aggregated point contains avg, min, max, and count
  for point in aggregated_data {
    assert_true(point.1 >= point.2)  // avg >= min
    assert_true(point.1 <= point.3)  // avg <= max
    assert_true(point.4 > 0)        // count > 0
  }
}

// Test 10: Color Scheme Generation
test "color scheme generation" {
  // Define color scheme functions
  let generate_color_scheme = fn(base_color: String, count: Int) {
    // Generate color variations based on base color
    let colors = []
    
    for i in 0..count {
      // In a real implementation, this would use color theory to generate harmonious colors
      // For this test, we'll simulate it with simple variations
      let hue_offset = i * (360 / count)
      let color = "hsl(" + hue_offset.to_string() + ", 70%, 50%)"
      colors = colors.push(color)
    }
    
    colors
  }
  
  let generate_gradient_colors = fn(start_color: String, end_color: String, steps: Int) {
    // Generate gradient colors between start and end
    let colors = []
    
    for i in 0..steps {
      // In a real implementation, this would interpolate between colors
      // For this test, we'll simulate it
      let ratio = i.to_float() / (steps - 1).to_float()
      let color = "gradient(" + start_color + "," + end_color + "," + (ratio * 100).to_string() + "%)"
      colors = colors.push(color)
    }
    
    colors
  }
  
  let generate_sequential_colors = fn(base_hue: Int, count: Int) {
    // Generate sequential colors with same hue but different lightness
    let colors = []
    
    for i in 0..count {
      let lightness = 20 + (i * 60 / (count - 1))  // From 20% to 80%
      let color = "hsl(" + base_hue.to_string() + ", 70%, " + lightness.to_string() + "%)"
      colors = colors.push(color)
    }
    
    colors
  }
  
  // Generate different color schemes
  let scheme1 = generate_color_scheme("blue", 5)
  let scheme2 = generate_gradient_colors("#FF0000", "#0000FF", 5)
  let scheme3 = generate_sequential_colors(200, 5)  // Blue hue
  
  // Verify color schemes
  assert_eq(scheme1.length(), 5)
  assert_eq(scheme2.length(), 5)
  assert_eq(scheme3.length(), 5)
  
  // Verify color scheme properties
  assert_true(scheme1[0].contains("hsl("))
  assert_true(scheme2[0].contains("gradient("))
  assert_true(scheme3[0].contains("hsl("))
  
  // Verify gradient colors contain start and end colors
  assert_true(scheme2[0].contains("#FF0000"))
  assert_true(scheme2[4].contains("#0000FF"))
  
  // Verify sequential colors have same hue but different lightness
  assert_true(scheme3[0].contains("200"))  // Base hue
  assert_true(scheme3[4].contains("200"))  // Same hue
  assert_true(scheme3[0].contains("20%"))   // Lightest
  assert_true(scheme3[4].contains("80%"))   // Darkest
  
  // Test color scheme application to charts
  let apply_colors_to_chart = fn(chart_type: String, colors: Array[String]) {
    {
      chart_type,
      colors,
      color_count: colors.length()
    }
  }
  
  let pie_chart_colors = apply_colors_to_chart("pie", scheme1)
  let bar_chart_colors = apply_colors_to_chart("bar", scheme3)
  
  assert_eq(pie_chart_colors.chart_type, "pie")
  assert_eq(pie_chart_colors.color_count, 5)
  assert_eq(bar_chart_colors.chart_type, "bar")
  assert_eq(bar_chart_colors.color_count, 5)
}