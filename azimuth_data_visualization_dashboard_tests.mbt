// Azimuth Data Visualization and Dashboard Integration Tests
// 数据可视化和仪表板集成测试套件

// Test 1: 图表数据生成和格式化
test "chart data generation and formatting" {
  // 图表类型定义
  let chart_types = [
    {"name": "line", "requires_time_series": true, "min_data_points": 2},
    {"name": "bar", "requires_time_series": false, "min_data_points": 1},
    {"name": "pie", "requires_time_series": false, "min_data_points": 1},
    {"name": "scatter", "requires_time_series": false, "min_data_points": 2},
    {"name": "heatmap", "requires_time_series": false, "min_data_points": 4},
    {"name": "gauge", "requires_time_series": false, "min_data_points": 1}
  ]
  
  // 模拟遥测数据
  let generate_telemetry_data = fn(chart_type, data_points) {
    let data = []
    let base_time = @lib.time.now() - 3600000  // 1小时前
    
    match chart_type {
      "line" => {
        for i in 0..data_points {
          let timestamp = base_time + i * 60000  // 每分钟一个点
          let value = 50.0 + @lib.sin(i.to_decimal() * 0.1) * 20.0 + @lib.random.float() * 5.0
          data = data.push({
            "timestamp": timestamp,
            "value": value,
            "label": @lib.date.format(timestamp, "HH:mm")
          })
        }
      }
      "bar" => {
        let categories = ["CPU", "Memory", "Disk", "Network", "GPU"]
        for category in categories {
          data = data.push({
            "category": category,
            "value": @lib.random.float() * 100.0,
            "label": category
          })
        }
      }
      "pie" => {
        let segments = ["Success", "Warning", "Error", "Critical"]
        let total = 100.0
        let mut remaining = total
        
        for i in 0..segments.length() {
          let segment = segments[i]
          let value = if i == segments.length() - 1 {
            remaining
          } else {
            let segment_value = @lib.random.float() * remaining * 0.6
            remaining = remaining - segment_value
            segment_value
          }
          
          data = data.push({
            "segment": segment,
            "value": value,
            "percentage": value / total * 100.0,
            "label": segment
          })
        }
      }
      "scatter" => {
        for i in 0..data_points {
          data = data.push({
            "x": @lib.random.float() * 100.0,
            "y": @lib.random.float() * 100.0,
            "size": @lib.random.float() * 20.0 + 5.0,
            "label": "Point " + i.to_string()
          })
        }
      }
      "heatmap" => {
        for i in 0..4 {
          for j in 0..4 {
            data = data.push({
              "x": i,
              "y": j,
              "value": @lib.random.float() * 100.0,
              "label": "(" + i.to_string() + "," + j.to_string() + ")"
            })
          }
        }
      }
      "gauge" => {
        data = data.push({
          "value": @lib.random.float() * 100.0,
          "min": 0.0,
          "max": 100.0,
          "thresholds": [30.0, 70.0, 90.0],
          "label": "Usage"
        })
      }
      _ => {}
    }
    
    data
  }
  
  // 图表数据格式化
  let format_chart_data = fn(data, chart_type) {
    match chart_type {
      "line" => {
        let formatted = {
          "labels": data.map_fn(d => d["label"]),
          "datasets": [{
            "label": "Telemetry Data",
            "data": data.map_fn(d => d["value"]),
            "timestamps": data.map_fn(d => d["timestamp"]),
            "borderColor": "#3498db",
            "backgroundColor": "rgba(52, 152, 219, 0.1)"
          }]
        }
        formatted
      }
      "bar" => {
        let formatted = {
          "labels": data.map_fn(d => d["category"]),
          "datasets": [{
            "label": "Resource Usage",
            "data": data.map_fn(d => d["value"]),
            "backgroundColor": ["#3498db", "#e74c3c", "#f39c12", "#2ecc71", "#9b59b6"]
          }]
        }
        formatted
      }
      "pie" => {
        let formatted = {
          "labels": data.map_fn(d => d["segment"]),
          "datasets": [{
            "data": data.map_fn(d => d["value"]),
            "backgroundColor": ["#2ecc71", "#f39c12", "#e74c3c", "#c0392b"]
          }]
        }
        formatted
      }
      "scatter" => {
        let formatted = {
          "datasets": [{
            "label": "Scatter Plot",
            "data": data.map_fn(d => {"x": d["x"], "y": d["y"], "r": d["size"]}),
            "backgroundColor": "rgba(52, 152, 219, 0.6)"
          }]
        }
        formatted
      }
      "heatmap" => {
        let formatted = {
          "data": data.map_fn(d => [d["x"], d["y"], d["value"]]),
          "labels": {
            "x": ["0", "1", "2", "3"],
            "y": ["0", "1", "2", "3"]
          }
        }
        formatted
      }
      "gauge" => {
        let point = data[0]
        let formatted = {
          "value": point["value"],
          "min": point["min"],
          "max": point["max"],
          "thresholds": point["thresholds"],
          "label": point["label"]
        }
        formatted
      }
      _ => {"error": "Unsupported chart type"}
    }
  }
  
  // 测试各种图表类型的数据生成和格式化
  let chart_results = []
  
  for chart_type in chart_types {
    let data_points = @lib.max(chart_type["min_data_points"], 10)
    let raw_data = generate_telemetry_data(chart_type["name"], data_points)
    let formatted_data = format_chart_data(raw_data, chart_type["name"])
    
    chart_results = {
      "chart_type": chart_type["name"],
      "data_points_generated": raw_data.length(),
      "format_success": not(formatted_data["error"]),
      "formatted_data": formatted_data
    }
  }
  
  // 验证图表数据生成
  for result in chart_results {
    assert_true(result["data_points_generated"] >= chart_types.filter_fn(ct => ct["name"] == result["chart_type"])[0]["min_data_points"], 
               result["chart_type"] + "图表应该生成足够的数据点")
    assert_true(result["format_success"], result["chart_type"] + "图表数据格式化应该成功")
  }
  
  // 验证特定图表类型的格式化结果
  let line_chart = chart_results.filter_fn(r => r["chart_type"] == "line")[0]
  assert_true(line_chart["formatted_data"]["labels"].length() > 0, "线图应该有标签")
  assert_true(line_chart["formatted_data"]["datasets"].length() > 0, "线图应该有数据集")
  
  let pie_chart = chart_results.filter_fn(r => r["chart_type"] == "pie")[0]
  assert_true(pie_chart["formatted_data"]["labels"].length() > 0, "饼图应该有标签")
  assert_true(pie_chart["formatted_data"]["datasets"][0]["data"].length() > 0, "饼图应该有数据")
  
  let gauge_chart = chart_results.filter_fn(r => r["chart_type"] == "gauge")[0]
  assert_true(gauge_chart["formatted_data"]["value"] >= 0.0, "仪表盘值应该非负")
  assert_true(gauge_chart["formatted_data"]["value"] <= 100.0, "仪表盘值应该在0-100范围内")
}

// Test 2: 实时数据流可视化
test "real-time data stream visualization" {
  // 实时数据流配置
  let stream_config = {
    "update_interval": 1000,    // 1秒更新间隔
    "buffer_size": 100,         // 缓冲区大小
    "max_display_points": 50,   // 最大显示点数
    "smoothing_enabled": true,  // 启用平滑
    "anomaly_detection": true   // 启用异常检测
  }
  
  // 实时数据流状态
  let stream_state = {
    "data_buffer": [],
    "last_update": 0,
    "update_count": 0,
    "anomalies_detected": [],
    "chart_updates": 0
  }
  
  // 生成实时数据点
  let generate_realtime_point = fn(timestamp) {
    let base_value = 50.0
    let trend = @lib.sin(timestamp.to_decimal() * 0.001) * 10.0
    let noise = (@lib.random.float() - 0.5) * 10.0
    let anomaly = if @lib.random.float() < 0.05 {  // 5%概率异常
      (@lib.random.float() - 0.5) * 50.0
    } else {
      0.0
    }
    
    let value = base_value + trend + noise + anomaly
    let is_anomaly = @lib.abs(anomaly) > 20.0
    
    {
      "timestamp": timestamp,
      "value": value,
      "is_anomaly": is_anomaly,
      "label": @lib.date.format(timestamp, "HH:mm:ss")
    }
  }
  
  // 更新数据流
  let update_data_stream = fn(state, config) {
    let current_time = @lib.time.now()
    
    // 检查是否需要更新
    if current_time - state["last_update"] < config["update_interval"] {
      return {"updated": false}
    }
    
    // 生成新数据点
    let new_point = generate_realtime_point(current_time)
    
    // 更新缓冲区
    state["data_buffer"] = state["data_buffer"].push(new_point)
    
    // 保持缓冲区大小
    if state["data_buffer"].length() > config["buffer_size"] {
      state["data_buffer"] = state["data_buffer"].slice(state["data_buffer"].length() - config["buffer_size"], state["data_buffer"].length())
    }
    
    // 检测异常
    if new_point["is_anomaly"] {
      state["anomalies_detected"] = state["anomalies_detected"].push({
        "timestamp": new_point["timestamp"],
        "value": new_point["value"],
        "severity": if @lib.abs(new_point["value"] - 50.0) > 30.0 { "high" } else { "medium" }
      })
    }
    
    state["last_update"] = current_time
    state["update_count"] = state["update_count"] + 1
    state["chart_updates"] = state["chart_updates"] + 1
    
    {"updated": true, "new_point": new_point}
  }
  
  // 获取图表数据
  let get_chart_data = fn(state, config) {
    let buffer = state["data_buffer"]
    if buffer.length() == 0 {
      return {"labels": [], "data": []}
    }
    
    // 获取最新的显示点
    let display_data = if buffer.length() > config["max_display_points"] {
      buffer.slice(buffer.length() - config["max_display_points"], buffer.length())
    } else {
      buffer
    }
    
    // 应用平滑
    let smoothed_data = if config["smoothing_enabled"] && display_data.length() >= 3 {
      let smoothed = []
      for i in 0..display_data.length() {
        if i == 0 || i == display_data.length() - 1 {
          smoothed = smoothed.push(display_data[i]["value"])
        } else {
          let smoothed_value = (display_data[i-1]["value"] + display_data[i]["value"] + display_data[i+1]["value"]) / 3.0
          smoothed = smoothed.push(smoothed_value)
        }
      }
      smoothed
    } else {
      display_data.map_fn(d => d["value"])
    }
    
    {
      "labels": display_data.map_fn(d => d["label"]),
      "data": smoothed_data,
      "anomalies": display_data.filter_fn(d => d["is_anomaly"]).map_fn(d => d["label"]),
      "update_count": state["update_count"]
    }
  }
  
  // 模拟实时数据流
  let simulation_duration = 10000  // 10秒
  let start_time = @lib.time.now()
  let chart_snapshots = []
  
  while @lib.time.now() - start_time < simulation_duration {
    // 更新数据流
    let update_result = update_data_stream(stream_state, stream_config)
    
    if update_result["updated"] {
      // 获取图表数据快照
      let chart_data = get_chart_data(stream_state, stream_config)
      
      if stream_state["update_count"] % 5 == 0 {  // 每5次更新记录一次快照
        chart_snapshots = chart_snapshots.push({
          "timestamp": @lib.time.now(),
          "update_count": stream_state["update_count"],
          "data_points": chart_data["data"].length(),
          "anomalies_count": chart_data["anomalies"].length()
        })
      }
    }
    
    @lib.time.sleep(200)  // 200ms间隔
  }
  
  // 验证实时数据流
  assert_true(stream_state["update_count"] > 0, "应该有数据更新")
  assert_true(stream_state["data_buffer"].length() > 0, "数据缓冲区应该有数据")
  assert_true(chart_snapshots.length() > 0, "应该有图表快照")
  
  // 验证异常检测
  if stream_config["anomaly_detection"] {
    assert_true(stream_state["anomalies_detected"].length() >= 0, "异常检测应该工作")
  }
  
  // 验证图表更新
  let final_chart_data = get_chart_data(stream_state, stream_config)
  assert_true(final_chart_data["labels"].length() > 0, "最终图表应该有标签")
  assert_true(final_chart_data["data"].length() > 0, "最终图表应该有数据")
  assert_eq(final_chart_data["labels"].length(), final_chart_data["data"].length(), "标签和数据数量应该匹配")
  
  // 验证数据平滑
  if stream_config["smoothing_enabled"] {
    // 平滑后的数据应该变化更平稳
    let data_variances = []
    for i in 1..final_chart_data["data"].length() {
      let variance = @lib.abs(final_chart_data["data"][i] - final_chart_data["data"][i-1])
      data_variances = data_variances.push(variance)
    }
    
    let avg_variance = data_variances.reduce(fn(acc, v) { acc + v }, 0.0) / data_variances.length().to_decimal()
    assert_true(avg_variance < 15.0, "平滑后的数据变化应该更平稳")
  }
}

// Test 3: 仪表板布局和组件管理
test "dashboard layout and component management" {
  // 仪表板布局配置
  let dashboard_layout = {
    "grid_columns": 12,
    "grid_rows": 8,
    "components": [
      {
        "id": "cpu_chart",
        "type": "line_chart",
        "position": {"x": 0, "y": 0, "w": 6, "h": 3},
        "title": "CPU Usage",
        "refresh_interval": 5000
      },
      {
        "id": "memory_gauge",
        "type": "gauge",
        "position": {"x": 6, "y": 0, "w": 6, "h": 3},
        "title": "Memory Usage",
        "refresh_interval": 3000
      },
      {
        "id": "error_pie",
        "type": "pie_chart",
        "position": {"x": 0, "y": 3, "w": 4, "h": 3},
        "title": "Error Distribution",
        "refresh_interval": 10000
      },
      {
        "id": "network_bar",
        "type": "bar_chart",
        "position": {"x": 4, "y": 3, "w": 8, "h": 3},
        "title": "Network Traffic",
        "refresh_interval": 4000
      },
      {
        "id": "status_panel",
        "type": "status_panel",
        "position": {"x": 0, "y": 6, "w": 12, "h": 2},
        "title": "System Status",
        "refresh_interval": 2000
      }
    ]
  }
  
  // 仪表板状态
  let dashboard_state = {
    "active_components": [],
    "component_data": {},
    "layout_valid": true,
    "last_refresh": 0,
    "refresh_count": 0
  }
  
  // 验证布局
  let validate_layout = fn(layout) {
    let grid_width = layout["grid_columns"]
    let grid_height = layout["grid_rows"]
    let occupied_cells = {}
    let layout_valid = true
    let conflicts = []
    
    for component in layout["components"] {
      let pos = component["position"]
      let component_id = component["id"]
      
      // 检查边界
      if pos["x"] < 0 || pos["y"] < 0 || 
         pos["x"] + pos["w"] > grid_width || 
         pos["y"] + pos["h"] > grid_height {
        layout_valid = false
        conflicts = conflicts.push({
          "component": component_id,
          "issue": "out_of_bounds",
          "position": pos
        })
      }
      
      // 检查重叠
      for x in pos["x"]..(pos["x"] + pos["w"] - 1) {
        for y in pos["y"]..(pos["y"] + pos["h"] - 1) {
          let cell_key = x.to_string() + "," + y.to_string()
          
          if occupied_cells[cell_key] {
            layout_valid = false
            conflicts = conflicts.push({
              "component": component_id,
              "issue": "overlap",
              "conflicts_with": occupied_cells[cell_key],
              "cell": cell_key
            })
          } else {
            occupied_cells[cell_key] = component_id
          }
        }
      }
    }
    
    {
      "valid": layout_valid,
      "conflicts": conflicts,
      "occupied_cells": occupied_cells
    }
  }
  
  // 初始化组件
  let initialize_component = fn(component, state) {
    let component_data = {
      "id": component["id"],
      "type": component["type"],
      "title": component["title"],
      "last_update": @lib.time.now(),
      "update_count": 0,
      "data": nil,
      "error": nil
    }
    
    state["component_data"][component["id"]] = component_data
    state["active_components"] = state["active_components"].push(component["id"])
    
    {
      "success": true,
      "component_id": component["id"]
    }
  }
  
  // 刷新组件数据
  let refresh_component = fn(component_id, state, layout) {
    let component = layout["components"].filter_fn(c => c["id"] == component_id)[0]
    let component_data = state["component_data"][component_id]
    
    if not(component) || not(component_data) {
      return {"success": false, "error": "Component not found"}
    }
    
    // 检查刷新间隔
    let current_time = @lib.time.now()
    if current_time - component_data["last_update"] < component["refresh_interval"] {
      return {"success": false, "reason": "too_soon"}
    }
    
    // 模拟数据刷新
    let refresh_success = @lib.random.float() > 0.1  // 90%成功率
    
    if refresh_success {
      component_data["last_update"] = current_time
      component_data["update_count"] = component_data["update_count"] + 1
      
      // 生成模拟数据
      component_data["data"] = match component["type"] {
        "line_chart" => {
          "labels": ["00:00", "00:01", "00:02", "00:03", "00:04"],
          "data": [45.2, 52.1, 48.7, 55.3, 51.8]
        }
        "gauge" => {
          "value": @lib.random.float() * 100.0,
          "thresholds": [30.0, 70.0, 90.0]
        }
        "pie_chart" => {
          "labels": ["Success", "Warning", "Error"],
          "data": [75.0, 20.0, 5.0]
        }
        "bar_chart" => {
          "labels": ["Inbound", "Outbound"],
          "data": [125.5, 98.3]
        }
        "status_panel" => {
          "status": "healthy",
          "services": ["API", "Database", "Cache"],
          "uptime": 99.9
        }
        _ => nil
      }
      
      state["refresh_count"] = state["refresh_count"] + 1
      
      {"success": true, "component_id": component_id}
    } else {
      component_data["error"] = "Data refresh failed"
      {"success": false, "error": "Data refresh failed"}
    }
  }
  
  // 验证布局
  let layout_validation = validate_layout(dashboard_layout)
  assert_true(layout_validation["valid"], "仪表板布局应该有效")
  assert_eq(layout_validation["conflicts"].length(), 0, "不应该有布局冲突")
  
  // 初始化所有组件
  let init_results = []
  for component in dashboard_layout["components"] {
    let result = initialize_component(component, dashboard_state)
    init_results = init_results.push(result)
  }
  
  // 验证组件初始化
  assert_eq(init_results.filter_fn(r => r["success"]).length(), dashboard_layout["components"].length(), 
             "所有组件应该初始化成功")
  assert_eq(dashboard_state["active_components"].length(), dashboard_layout["components"].length(), 
             "活动组件数量应该正确")
  
  // 刷新组件数据
  let refresh_results = []
  for component_id in dashboard_state["active_components"] {
    let result = refresh_component(component_id, dashboard_state, dashboard_layout)
    refresh_results = refresh_results.push(result)
  }
  
  // 验证组件刷新
  let successful_refreshes = refresh_results.filter_fn(r => r["success"])
  assert_true(successful_refreshes.length() > 0, "应该有成功的组件刷新")
  
  // 验证组件数据
  for component_id in dashboard_state["active_components"] {
    let component_data = dashboard_state["component_data"][component_id]
    if component_data {
      assert_true(component_data["update_count"] >= 0, "组件更新次数应该非负")
      
      if component_data["data"] {
        assert_not_nil(component_data["data"], "组件应该有数据")
      }
    }
  }
  
  // 验证仪表板状态
  assert_true(dashboard_state["refresh_count"] > 0, "应该有刷新计数")
  assert_true(dashboard_state["last_refresh"] > 0, "应该有最后刷新时间")
}

// Test 4: 响应式图表适配
test "responsive chart adaptation" {
  // 响应式配置
  let responsive_config = {
    "breakpoints": [
      {"name": "mobile", "max_width": 768, "chart_height": 300, "font_size": 12},
      {"name": "tablet", "max_width": 1024, "chart_height": 400, "font_size": 14},
      {"name": "desktop", "max_width": 999999, "chart_height": 500, "font_size": 16}
    ],
    "adaptive_features": {
      "auto_resize": true,
      "touch_optimized": true,
      "reduced_data_points": true,
      "simplified_labels": true
    }
  }
  
  // 模拟屏幕尺寸
  let screen_sizes = [
    {"width": 480, "height": 800, "type": "mobile"},
    {"width": 768, "height": 1024, "type": "tablet"},
    {"width": 1200, "height": 800, "type": "desktop"},
    {"width": 1920, "height": 1080, "type": "desktop"}
  ]
  
  // 基础图表数据
  let base_chart_data = {
    "labels": ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    "datasets": [{
      "label": "Monthly Data",
      "data": [65, 59, 80, 81, 56, 55, 40, 65, 75, 82, 90, 85],
      "borderColor": "#3498db",
      "backgroundColor": "rgba(52, 152, 219, 0.1)"
    }]
  }
  
  // 获取当前断点
  let get_breakpoint = fn(width, config) {
    for breakpoint in config["breakpoints"] {
      if width <= breakpoint["max_width"] {
        return breakpoint
      }
    }
    config["breakpoints"][config["breakpoints"].length() - 1]
  }
  
  // 适配图表数据
  let adapt_chart_data = fn(data, screen_size, config) {
    let breakpoint = get_breakpoint(screen_size["width"], config)
    let adapted_data = @lib.object.clone(data)
    
    // 根据屏幕大小减少数据点
    if config["adaptive_features"]["reduced_data_points"] && screen_size["type"] == "mobile" {
      let target_points = 6  // 移动设备显示6个点
      let step = @lib.ceil(data["labels"].length().to_decimal() / target_points.to_decimal()).to_int()
      
      let reduced_labels = []
      let reduced_data = []
      
      for i in 0..data["labels"].length() {
        if i % step == 0 || i == data["labels"].length() - 1 {
          reduced_labels = reduced_labels.push(data["labels"][i])
          reduced_data = reduced_data.push(data["datasets"][0]["data"][i])
        }
      }
      
      adapted_data["labels"] = reduced_labels
      adapted_data["datasets"][0]["data"] = reduced_data
    }
    
    // 简化标签
    if config["adaptive_features"]["simplified_labels"] && screen_size["type"] == "mobile" {
      adapted_data["labels"] = adapted_data["labels"].map_fn(label => 
        if label.length() > 3 { label.slice(0, 3) } else { label }
      )
    }
    
    // 设置图表高度
    adapted_data["options"] = {
      "responsive": true,
      "maintainAspectRatio": false,
      "height": breakpoint["chart_height"],
      "fontSize": breakpoint["font_size"]
    }
    
    adapted_data
  }
  
  // 测试响应式适配
  let adaptation_results = []
  
  for screen_size in screen_sizes {
    let breakpoint = get_breakpoint(screen_size["width"], responsive_config)
    let adapted_data = adapt_chart_data(base_chart_data, screen_size, responsive_config)
    
    adaptation_results = adaptation_results.push({
      "screen_size": screen_size,
      "breakpoint": breakpoint["name"],
      "chart_height": adapted_data["options"]["height"],
      "font_size": adapted_data["options"]["fontSize"],
      "data_points": adapted_data["labels"].length(),
      "original_points": base_chart_data["labels"].length()
    })
  }
  
  // 验证响应式适配结果
  for result in adaptation_results {
    let expected_breakpoint = get_breakpoint(result["screen_size"]["width"], responsive_config)
    assert_eq(result["breakpoint"], expected_breakpoint["name"], "断点检测应该正确")
    assert_eq(result["chart_height"], expected_breakpoint["chart_height"], "图表高度应该适配")
    assert_eq(result["font_size"], expected_breakpoint["font_size"], "字体大小应该适配")
  }
  
  // 验证移动设备数据点减少
  let mobile_result = adaptation_results.filter_fn(r => r["screen_size"]["type"] == "mobile")[0]
  assert_true(mobile_result["data_points"] < mobile_result["original_points"], "移动设备应该减少数据点")
  
  // 验证桌面设备数据点保持
  let desktop_result = adaptation_results.filter_fn(r => r["screen_size"]["type"] == "desktop")[0]
  assert_eq(desktop_result["data_points"], desktop_result["original_points"], "桌面设备应该保持所有数据点")
  
  // 验证标签简化
  let mobile_adapted = adapt_chart_data(base_chart_data, screen_sizes[0], responsive_config)
  for label in mobile_adapted["labels"] {
    assert_true(label.length() <= 3, "移动设备标签应该被简化")
  }
}

// Test 5: 图表交互功能
test "chart interaction functionality" {
  // 交互功能配置
  let interaction_config = {
    "zoom_enabled": true,
    "pan_enabled": true,
    "tooltip_enabled": true,
    "legend_interactive": true,
    "data_point_click": true,
    "selection_enabled": true
  }
  
  // 图表交互状态
  let interaction_state = {
    "zoom_level": 1.0,
    "pan_offset": {"x": 0, "y": 0},
    "selected_points": [],
    "hovered_point": nil,
    "legend_visibility": {},
    "interaction_log": []
  }
  
  // 模拟图表数据
  let chart_data = {
    "labels": ["Point 1", "Point 2", "Point 3", "Point 4", "Point 5", "Point 6", "Point 7", "Point 8"],
    "datasets": [
      {
        "label": "Dataset A",
        "data": [12, 19, 3, 5, 2, 3, 15, 8],
        "color": "#3498db"
      },
      {
        "label": "Dataset B", 
        "data": [7, 11, 5, 8, 3, 7, 12, 6],
        "color": "#e74c3c"
      }
    ]
  }
  
  // 缩放功能
  let zoom_chart = fn(state, factor, center_point) {
    if not(interaction_config["zoom_enabled"]) {
      return {"success": false, "reason": "zoom_disabled"}
    }
    
    let old_zoom = state["zoom_level"]
    let new_zoom = old_zoom * factor
    
    // 限制缩放范围
    if new_zoom < 0.5 || new_zoom > 5.0 {
      return {"success": false, "reason": "zoom_out_of_range"}
    }
    
    state["zoom_level"] = new_zoom
    
    // 记录交互
    state["interaction_log"] = state["interaction_log"].push({
      "action": "zoom",
      "old_level": old_zoom,
      "new_level": new_zoom,
      "center": center_point,
      "timestamp": @lib.time.now()
    })
    
    {
      "success": true,
      "old_zoom": old_zoom,
      "new_zoom": new_zoom
    }
  }
  
  // 平移功能
  let pan_chart = fn(state, delta_x, delta_y) {
    if not(interaction_config["pan_enabled"]) {
      return {"success": false, "reason": "pan_disabled"}
    }
    
    let old_offset = state["pan_offset"]
    state["pan_offset"]["x"] = old_offset["x"] + delta_x
    state["pan_offset"]["y"] = old_offset["y"] + delta_y
    
    // 限制平移范围
    let max_pan = 1000
    state["pan_offset"]["x"] = @lib.max(-max_pan, @lib.min(max_pan, state["pan_offset"]["x"]))
    state["pan_offset"]["y"] = @lib.max(-max_pan, @lib.min(max_pan, state["pan_offset"]["y"]))
    
    // 记录交互
    state["interaction_log"] = state["interaction_log"].push({
      "action": "pan",
      "delta": {"x": delta_x, "y": delta_y},
      "new_offset": state["pan_offset"],
      "timestamp": @lib.time.now()
    })
    
    {
      "success": true,
      "old_offset": old_offset,
      "new_offset": state["pan_offset"]
    }
  }
  
  // 数据点选择
  let select_data_point = fn(state, dataset_index, point_index) {
    if not(interaction_config["data_point_click"]) {
      return {"success": false, "reason": "selection_disabled"}
    }
    
    let point_id = dataset_index.to_string() + "_" + point_index.to_string()
    let existing_selection = state["selected_points"].index_of(point_id)
    
    match existing_selection {
      Some(index) => {
        // 取消选择
        state["selected_points"] = state["selected_points"].slice(0, index) + 
                                   state["selected_points"].slice(index + 1, state["selected_points"].length())
        
        state["interaction_log"] = state["interaction_log"].push({
          "action": "deselect_point",
          "point_id": point_id,
          "timestamp": @lib.time.now()
        })
        
        {"success": true, "action": "deselected", "point_id": point_id}
      }
      None => {
        // 添加选择
        state["selected_points"] = state["selected_points"].push(point_id)
        
        state["interaction_log"] = state["interaction_log"].push({
          "action": "select_point",
          "point_id": point_id,
          "timestamp": @lib.time.now()
        })
        
        {"success": true, "action": "selected", "point_id": point_id}
      }
    }
  }
  
  // 图例交互
  let toggle_legend = fn(state, dataset_label) {
    if not(interaction_config["legend_interactive"]) {
      return {"success": false, "reason": "legend_disabled"}
    }
    
    let current_visibility = state["legend_visibility"][dataset_label] ?? true
    state["legend_visibility"][dataset_label] = not(current_visibility)
    
    state["interaction_log"] = state["interaction_log"].push({
      "action": "toggle_legend",
      "dataset": dataset_label,
      "new_visibility": state["legend_visibility"][dataset_label],
      "timestamp": @lib.time.now()
    })
    
    {
      "success": true,
      "dataset": dataset_label,
      "visibility": state["legend_visibility"][dataset_label]
    }
  }
  
  // 测试缩放功能
  let zoom_result = zoom_chart(interaction_state, 1.5, {"x": 0.5, "y": 0.5})
  assert_true(zoom_result["success"], "缩放应该成功")
  assert_eq(interaction_state["zoom_level"], 1.5, "缩放级别应该更新")
  
  // 测试平移功能
  let pan_result = pan_chart(interaction_state, 50, 30)
  assert_true(pan_result["success"], "平移应该成功")
  assert_eq(interaction_state["pan_offset"]["x"], 50, "平移X偏移应该更新")
  assert_eq(interaction_state["pan_offset"]["y"], 30, "平移Y偏移应该更新")
  
  // 测试数据点选择
  let select_result1 = select_data_point(interaction_state, 0, 2)
  assert_true(select_result1["success"], "数据点选择应该成功")
  assert_eq(select_result1["action"], "selected", "应该选择数据点")
  assert_true(interaction_state["selected_points"].contains("0_2"), "数据点应该在选择列表中")
  
  let select_result2 = select_data_point(interaction_state, 0, 2)
  assert_true(select_result2["success"], "重复选择应该成功")
  assert_eq(select_result2["action"], "deselected", "应该取消选择数据点")
  assert_false(interaction_state["selected_points"].contains("0_2"), "数据点应该从选择列表中移除")
  
  // 测试图例交互
  let legend_result = toggle_legend(interaction_state, "Dataset A")
  assert_true(legend_result["success"], "图例切换应该成功")
  assert_false(legend_result["visibility"], "数据集应该被隐藏")
  
  // 验证交互日志
  assert_true(interaction_state["interaction_log"].length() > 0, "应该有交互日志")
  
  let zoom_log = interaction_state["interaction_log"].filter_fn(log => log["action"] == "zoom")
  let pan_log = interaction_state["interaction_log"].filter_fn(log => log["action"] == "pan")
  let select_log = interaction_state["interaction_log"].filter_fn(log => log["action"] == "select_point")
  let legend_log = interaction_state["interaction_log"].filter_fn(log => log["action"] == "toggle_legend")
  
  assert_true(zoom_log.length() > 0, "应该有缩放日志")
  assert_true(pan_log.length() > 0, "应该有平移日志")
  assert_true(select_log.length() > 0, "应该有选择日志")
  assert_true(legend_log.length() > 0, "应该有图例日志")
  
  // 测试缩放限制
  let zoom_over_limit = zoom_chart(interaction_state, 10.0, {"x": 0.5, "y": 0.5})
  assert_false(zoom_over_limit["success"], "超出限制的缩放应该失败")
  assert_eq(zoom_over_limit["reason"], "zoom_out_of_range", "失败原因应该正确")
}

// Test 6: 数据导出和分享功能
test "data export and sharing functionality" {
  // 导出配置
  let export_config = {
    "formats": ["png", "jpg", "svg", "pdf", "csv", "json"],
    "quality": {
      "png": 0.9,
      "jpg": 0.8,
      "svg": 1.0
    },
    "sharing": {
      "enabled": true,
      "url_expiry": 86400,  // 24小时
      "allow_download": true
    }
  }
  
  // 模拟图表数据
  let chart_data = {
    "title": "System Performance Metrics",
    "type": "line_chart",
    "labels": ["00:00", "04:00", "08:00", "12:00", "16:00", "20:00"],
    "datasets": [
      {
        "label": "CPU Usage (%)",
        "data": [45.2, 52.1, 48.7, 55.3, 51.8, 49.6],
        "color": "#3498db"
      },
      {
        "label": "Memory Usage (%)",
        "data": [62.3, 65.8, 61.2, 68.5, 64.7, 63.1],
        "color": "#e74c3c"
      }
    ],
    "metadata": {
      "generated_at": @lib.time.now(),
      "data_source": "telemetry_system",
      "time_range": "24h"
    }
  }
  
  // 导出状态
  let export_state = {
    "exports": [],
    "shares": [],
    "last_export_id": 0
  }
  
  // 导出为图片格式
  let export_as_image = fn(data, format, state, config) {
    if not(config["formats"].contains(format)) {
      return {"success": false, "error": "unsupported_format"}
    }
    
    let export_id = state["last_export_id"] + 1
    state["last_export_id"] = export_id
    
    // 模拟导出过程
    let export_start = @lib.time.now()
    let export_time = match format {
      "png" | "jpg" => @lib.random.int(2000) + 1000  // 1-3秒
      "svg" => @lib.random.int(1000) + 500          // 0.5-1.5秒
      _ => 0
    }
    
    @lib.time.sleep(export_time)
    
    let export_end = @lib.time.now()
    let quality = config["quality"][format] ?? 1.0
    let file_size = match format {
      "png" => (1024 * 1024 * quality).to_int()       // 1MB * quality
      "jpg" => (800 * 1024 * quality).to_int()        // 800KB * quality
      "svg" => (200 * 1024).to_int()                   // 200KB
      _ => 0
    }
    
    let export_record = {
      "id": export_id,
      "format": format,
      "filename": "chart_" + export_id.to_string() + "." + format,
      "file_size": file_size,
      "quality": quality,
      "created_at": export_end,
      "duration": export_end - export_start
    }
    
    state["exports"] = state["exports"].push(export_record)
    
    {
      "success": true,
      "export_id": export_id,
      "filename": export_record["filename"],
      "file_size": file_size
    }
  }
  
  // 导出为数据格式
  let export_as_data = fn(data, format, state, config) {
    if not(config["formats"].contains(format)) {
      return {"success": false, "error": "unsupported_format"}
    }
    
    let export_id = state["last_export_id"] + 1
    state["last_export_id"] = export_id
    
    let export_start = @lib.time.now()
    
    let export_content = match format {
      "csv" => {
        let headers = ["Time", data["datasets"][0]["label"], data["datasets"][1]["label"]]
        let rows = []
        
        for i in 0..data["labels"].length() {
          let row = data["labels"][i] + "," + 
                   data["datasets"][0]["data"][i].to_string() + "," + 
                   data["datasets"][1]["data"][i].to_string()
          rows = rows.push(row)
        }
        
        headers.join(",") + "\n" + rows.join("\n")
      }
      "json" => {
        @lib.json.stringify(data)
      }
      _ => ""
    }
    
    let export_end = @lib.time.now()
    let file_size = export_content.length()
    
    let export_record = {
      "id": export_id,
      "format": format,
      "filename": "data_" + export_id.to_string() + "." + format,
      "file_size": file_size,
      "created_at": export_end,
      "duration": export_end - export_start
    }
    
    state["exports"] = state["exports"].push(export_record)
    
    {
      "success": true,
      "export_id": export_id,
      "filename": export_record["filename"],
      "file_size": file_size
    }
  }
  
  // 创建分享链接
  let create_share_link = fn(export_id, state, config) {
    if not(config["sharing"]["enabled"]) {
      return {"success": false, "error": "sharing_disabled"}
    }
    
    let share_id = @lib.string.random(16)
    let created_at = @lib.time.now()
    let expires_at = created_at + config["sharing"]["url_expiry"] * 1000
    
    let share_record = {
      "share_id": share_id,
      "export_id": export_id,
      "created_at": created_at,
      "expires_at": expires_at,
      "download_count": 0,
      "url": "https://dashboard.example.com/share/" + share_id
    }
    
    state["shares"] = state["shares"].push(share_record)
    
    {
      "success": true,
      "share_id": share_id,
      "url": share_record["url"],
      "expires_at": expires_at
    }
  }
  
  // 测试图片导出
  let image_exports = ["png", "jpg", "svg"]
  let image_export_results = []
  
  for format in image_exports {
    let result = export_as_image(chart_data, format, export_state, export_config)
    image_export_results = image_export_results.push(result)
  }
  
  // 验证图片导出
  assert_eq(image_export_results.filter_fn(r => r["success"]).length(), image_exports.length(), 
             "所有图片格式导出应该成功")
  
  for result in image_export_results {
    assert_true(result["file_size"] > 0, "导出文件应该有大小")
    assert_true(result["filename"].contains("."), "文件名应该包含扩展名")
  }
  
  // 测试数据导出
  let data_exports = ["csv", "json"]
  let data_export_results = []
  
  for format in data_exports {
    let result = export_as_data(chart_data, format, export_state, export_config)
    data_export_results = data_export_results.push(result)
  }
  
  // 验证数据导出
  assert_eq(data_export_results.filter_fn(r => r["success"]).length(), data_exports.length(), 
             "所有数据格式导出应该成功")
  
  // 测试分享功能
  if export_config["sharing"]["enabled"] && export_state["exports"].length() > 0 {
    let first_export_id = export_state["exports"][0]["id"]
    let share_result = create_share_link(first_export_id, export_state, export_config)
    
    assert_true(share_result["success"], "分享链接创建应该成功")
    assert_true(share_result["url"].length() > 0, "分享URL应该不为空")
    assert_true(share_result["expires_at"] > @lib.time.now(), "过期时间应该在未来")
  }
  
  // 验证导出状态
  assert_true(export_state["exports"].length() > 0, "应该有导出记录")
  assert_eq(export_state["last_export_id"], export_state["exports"].length(), "导出ID应该正确")
  
  // 验证导出记录
  for export_record in export_state["exports"] {
    assert_true(export_record["id"] > 0, "导出ID应该大于0")
    assert_true(export_record["filename"].length() > 0, "文件名应该不为空")
    assert_true(export_record["file_size"] > 0, "文件大小应该大于0")
    assert_true(export_record["created_at"] > 0, "创建时间应该有效")
  }
  
  // 验证分享记录
  if export_state["shares"].length() > 0 {
    for share_record in export_state["shares"] {
      assert_true(share_record["share_id"].length() > 0, "分享ID应该不为空")
      assert_true(share_record["url"].length() > 0, "分享URL应该不为空")
      assert_true(share_record["export_id"] > 0, "关联的导出ID应该有效")
    }
  }
}

// Test 7: 主题和样式自定义
test "theme and style customization" {
  // 主题配置
  let themes = {
    "light": {
      "name": "Light Theme",
      "background": "#ffffff",
      "text": "#333333",
      "grid": "#e0e0e0",
      "colors": ["#3498db", "#e74c3c", "#f39c12", "#2ecc71", "#9b59b6"],
      "chart_background": "#fafafa",
      "legend_text": "#666666"
    },
    "dark": {
      "name": "Dark Theme", 
      "background": "#2c3e50",
      "text": "#ecf0f1",
      "grid": "#34495e",
      "colors": ["#3498db", "#e74c3c", "#f39c12", "#2ecc71", "#9b59b6"],
      "chart_background": "#34495e",
      "legend_text": "#bdc3c7"
    },
    "high_contrast": {
      "name": "High Contrast Theme",
      "background": "#000000",
      "text": "#ffffff", 
      "grid": "#666666",
      "colors": ["#ffff00", "#00ff00", "#ff00ff", "#00ffff", "#ffffff"],
      "chart_background": "#1a1a1a",
      "legend_text": "#cccccc"
    }
  }
  
  // 样式配置
  let style_options = {
    "line_width": [1, 2, 3, 4],
    "point_size": [3, 5, 7, 9],
    "font_family": ["Arial", "Helvetica", "Roboto", "Open Sans"],
    "animation_duration": [0, 500, 1000, 2000],
    "border_radius": [0, 4, 8, 12]
  }
  
  // 当前主题和样式状态
  let current_theme = "light"
  let current_style = {
    "line_width": 2,
    "point_size": 5,
    "font_family": "Arial",
    "animation_duration": 1000,
    "border_radius": 4
  }
  
  // 应用主题
  let apply_theme = fn(theme_name, data) {
    let theme = themes[theme_name]
    
    if not(theme) {
      return {"success": false, "error": "theme_not_found"}
    }
    
    // 应用主题到图表数据
    let themed_data = @lib.object.clone(data)
    
    // 更新颜色
    for i in 0..themed_data["datasets"].length() {
      let color_index = i % theme["colors"].length()
      themed_data["datasets"][i]["borderColor"] = theme["colors"][color_index]
      themed_data["datasets"][i]["backgroundColor"] = theme["colors"][color_index] + "33"  // 添加透明度
    }
    
    // 应用主题选项
    themed_data["options"] = {
      "responsive": true,
      "plugins": {
        "legend": {
          "labels": {
            "color": theme["legend_text"],
            "font": {
              "family": current_style["font_family"]
            }
          }
        }
      },
      "scales": {
        "x": {
          "grid": {
            "color": theme["grid"]
          },
          "ticks": {
            "color": theme["text"]
          }
        },
        "y": {
          "grid": {
            "color": theme["grid"]
          },
          "ticks": {
            "color": theme["text"]
          }
        }
      },
      "elements": {
        "line": {
          "borderWidth": current_style["line_width"]
        },
        "point": {
          "radius": current_style["point_size"]
        }
      },
      "animation": {
        "duration": current_style["animation_duration"]
      },
      "layout": {
        "borderRadius": current_style["border_radius"]
      }
    }
    
    {
      "success": true,
      "theme_name": theme_name,
      "themed_data": themed_data
    }
  }
  
  // 更新样式
  let update_style = fn(style_property, value, current_styles) {
    if not(style_options[style_property].contains(value)) {
      return {"success": false, "error": "invalid_style_value"}
    }
    
    current_styles[style_property] = value
    
    {
      "success": true,
      "property": style_property,
      "value": value,
      "updated_styles": current_styles
    }
  }
  
  // 模拟图表数据
  let base_chart_data = {
    "labels": ["January", "February", "March", "April", "May"],
    "datasets": [
      {
        "label": "Dataset 1",
        "data": [12, 19, 3, 5, 2]
      },
      {
        "label": "Dataset 2", 
        "data": [7, 11, 5, 8, 3]
      }
    ]
  }
  
  // 测试主题应用
  let theme_results = []
  
  for theme_name in themes.keys() {
    let result = apply_theme(theme_name, base_chart_data)
    theme_results = theme_results.push(result)
  }
  
  // 验证主题应用
  assert_eq(theme_results.filter_fn(r => r["success"]).length(), themes.keys().length(), 
             "所有主题应该应用成功")
  
  for result in theme_results {
    let themed_data = result["themed_data"]
    assert_not_nil(themed_data["options"], "主题数据应该有选项")
    assert_not_nil(themed_data["options"]["plugins"], "应该有插件配置")
    assert_not_nil(themed_data["options"]["scales"], "应该有缩放配置")
  }
  
  // 测试样式更新
  let style_update_results = []
  
  for style_property in style_options.keys() {
    let valid_values = style_options[style_property]
    let test_value = valid_values[@lib.random.int(valid_values.length())]
    
    let result = update_style(style_property, test_value, current_style)
    style_update_results = style_update_results.push(result)
  }
  
  // 验证样式更新
  assert_eq(style_update_results.filter_fn(r => r["success"]).length(), style_options.keys().length(), 
             "所有样式更新应该成功")
  
  // 测试无效样式值
  let invalid_style_result = update_style("line_width", 10, current_style)
  assert_false(invalid_style_result["success"], "无效样式值应该失败")
  assert_eq(invalid_style_result["error"], "invalid_style_value", "错误信息应该正确")
  
  // 测试主题特定属性
  let light_theme_result = theme_results.filter_fn(r => r["theme_name"] == "light")[0]
  let light_options = light_theme_result["themed_data"]["options"]
  
  assert_eq(light_options["scales"]["x"]["ticks"]["color"], themes["light"]["text"], 
             "浅色主题文本颜色应该正确")
  assert_eq(light_options["scales"]["x"]["grid"]["color"], themes["light"]["grid"], 
             "浅色主题网格颜色应该正确")
  
  let dark_theme_result = theme_results.filter_fn(r => r["theme_name"] == "dark")[0]
  let dark_options = dark_theme_result["themed_data"]["options"]
  
  assert_eq(dark_options["scales"]["x"]["ticks"]["color"], themes["dark"]["text"], 
             "深色主题文本颜色应该正确")
  assert_eq(dark_options["scales"]["x"]["grid"]["color"], themes["dark"]["grid"], 
             "深色主题网格颜色应该正确")
  
  // 验证高对比度主题
  let high_contrast_result = theme_results.filter_fn(r => r["theme_name"] == "high_contrast")[0]
  let high_contrast_data = high_contrast_result["themed_data"]["datasets"][0]
  
  assert_true(themes["high_contrast"]["colors"].contains(high_contrast_data["borderColor"]), 
             "高对比度主题应该使用高对比度颜色")
}

// Test 8: 性能监控和优化
test "performance monitoring and optimization" {
  // 性能监控配置
  let performance_config = {
    "max_render_time": 100,      // 100ms最大渲染时间
    "max_data_points": 1000,     // 最大数据点数
    "update_throttle": 100,      // 100ms更新节流
    "lazy_loading": true,        // 启用懒加载
    "virtualization": true       // 启用虚拟化
  }
  
  // 性能监控状态
  let performance_state = {
    "render_times": [],
    "data_point_counts": [],
    "update_intervals": [],
    "memory_usage": [],
    "optimizations_applied": []
  }
  
  // 模拟图表渲染
  let render_chart = fn(data_points, state, config) {
    let render_start = @lib.time.now()
    
    // 模拟渲染时间（基于数据点数量）
    let base_render_time = 10  // 基础渲染时间10ms
    let data_point_time = data_points.to_decimal() * 0.1  // 每个数据点0.1ms
    let total_render_time = base_render_time + data_point_time
    
    // 应用优化
    let optimized_time = if config["lazy_loading"] && data_points > 100 {
      total_render_time * 0.7  // 懒加载减少30%时间
    } else if config["virtualization"] && data_points > 500 {
      total_render_time * 0.5  // 虚拟化减少50%时间
    } else {
      total_render_time
    }
    
    @lib.time.sleep(optimized_time.to_int())
    
    let render_end = @lib.time.now()
    let actual_render_time = render_end - render_start
    
    // 记录性能数据
    state["render_times"] = state["render_times"].push(actual_render_time)
    state["data_point_counts"] = state["data_point_counts"].push(data_points)
    
    // 检查是否超时
    let timeout = actual_render_time > config["max_render_time"]
    
    {
      "success": not(timeout),
      "render_time": actual_render_time,
      "data_points": data_points,
      "timeout": timeout,
      "optimized": optimized_time < total_render_time
    }
  }
  
  // 数据优化
  let optimize_data = fn(data, max_points, strategy) {
    if data.length() <= max_points {
      return {"optimized": false, "data": data}
    }
    
    let optimized_data = match strategy {
      "sampling" => {
        // 采样：均匀选择数据点
        let step = @lib.ceil(data.length().to_decimal() / max_points.to_decimal()).to_int()
        let sampled = []
        
        for i in 0..data.length() {
          if i % step == 0 || i == data.length() - 1 {
            sampled = sampled.push(data[i])
          }
        }
        
        sampled
      }
      "aggregation" => {
        // 聚合：将数据点分组聚合
        let group_size = @lib.ceil(data.length().to_decimal() / max_points.to_decimal()).to_int()
        let aggregated = []
        
        for i in 0..data.length() {
          if i % group_size == 0 {
            let group_end = @lib.min(i + group_size, data.length())
            let group = data.slice(i, group_end)
            
            // 计算平均值
            let avg = group.reduce(fn(acc, val) { acc + val }, 0) / group.length()
            aggregated = aggregated.push(avg)
          }
        }
        
        aggregated
      }
      "latest" => {
        // 最新：保留最新的数据点
        data.slice(data.length() - max_points, data.length())
      }
      _ => data
    }
    
    {"optimized": true, "data": optimized_data, "original_count": data.length(), "optimized_count": optimized_data.length()}
  }
  
  // 测试不同数据量的渲染性能
  let test_data_sizes = [10, 50, 100, 500, 1000, 2000]
  let render_results = []
  
  for size in test_data_sizes {
    let result = render_chart(size, performance_state, performance_config)
    render_results = render_results.push(result)
  }
  
  // 验证渲染性能
  for result in render_results {
    assert_true(result["render_time"] > 0, "渲染时间应该大于0")
    
    if result["data_points"] <= 1000 {
      assert_true(result["success"], "小数据量渲染应该成功")
    }
  }
  
  // 测试数据优化
  let optimization_strategies = ["sampling", "aggregation", "latest"]
  let large_data = []
  
  for i in 0..2000 {
    large_data = large_data.push(@lib.random.float() * 100.0)
  }
  
  let optimization_results = []
  
  for strategy in optimization_strategies {
    let result = optimize_data(large_data, performance_config["max_data_points"], strategy)
    optimization_results = optimization_results.push(result)
  }
  
  // 验证数据优化
  for result in optimization_results {
    if result["optimized"] {
      assert_true(result["optimized_count"] <= performance_config["max_data_points"], 
                 "优化后数据点数应该不超过限制")
      assert_true(result["optimized_count"] < result["original_count"], 
                 "优化后数据点数应该少于原始数量")
    }
  }
  
  // 计算性能统计
  let avg_render_time = performance_state["render_times"].reduce(fn(acc, time) { acc + time }, 0.0) / 
                        performance_state["render_times"].length().to_decimal()
  
  let max_render_time = performance_state["render_times"].reduce(fn(acc, time) { 
    if time > acc { time } else { acc } 
  }, 0.0)
  
  let timeout_count = render_results.filter_fn(r => r["timeout"]).length()
  
  // 验证性能统计
  assert_true(avg_render_time > 0, "平均渲染时间应该大于0")
  assert_true(max_render_time > 0, "最大渲染时间应该大于0")
  
  // 验证优化效果
  let optimized_renders = render_results.filter_fn(r => r["optimized"])
  assert_true(optimized_renders.length() > 0, "应该有优化的渲染")
  
  // 测试内存使用监控
  let memory_snapshots = []
  
  for i in 0..5 {
    // 模拟内存使用
    let base_memory = 1024 * 1024  // 1MB基础内存
    let data_memory = i * 512 * 1024  // 每次增加512KB
    let total_memory = base_memory + data_memory
    
    memory_snapshots = memory_snapshots.push({
      "snapshot": i,
      "memory_usage": total_memory,
      "data_points": test_data_sizes[@lib.min(i, test_data_sizes.length() - 1)]
    })
    
    performance_state["memory_usage"] = performance_state["memory_usage"].push(total_memory)
  }
  
  // 验证内存监控
  assert_eq(memory_snapshots.length(), 5, "应该有5个内存快照")
  
  for snapshot in memory_snapshots {
    assert_true(snapshot["memory_usage"] > 0, "内存使用应该大于0")
  }
  
  // 验证内存增长趋势
  let memory_growth = memory_snapshots[memory_snapshots.length() - 1]["memory_usage"] - 
                     memory_snapshots[0]["memory_usage"]
  assert_true(memory_growth > 0, "内存使用应该有增长")
}

// Test 9: 多图表联动和交互
test "multi-chart linkage and interaction" {
  // 图表联动配置
  let linkage_config = {
    "cross_filter": true,        // 交叉过滤
    "brush_selection": true,     // 刷选
    "synchronized_zoom": true,   // 同步缩放
    "linked_legends": true,      // 联动图例
    "shared_time_range": true    // 共享时间范围
  }
  
  // 图表集合
  let chart_collection = {
    "charts": [
      {
        "id": "overview_chart",
        "type": "line_chart",
        "title": "System Overview",
        "time_range": {"start": 0, "end": 86400000},  // 24小时
        "linked_charts": ["cpu_chart", "memory_chart", "network_chart"]
      },
      {
        "id": "cpu_chart",
        "type": "area_chart",
        "title": "CPU Usage",
        "time_range": {"start": 0, "end": 86400000},
        "linked_charts": ["overview_chart", "memory_chart"]
      },
      {
        "id": "memory_chart",
        "type": "line_chart",
        "title": "Memory Usage",
        "time_range": {"start": 0, "end": 86400000},
        "linked_charts": ["overview_chart", "cpu_chart", "network_chart"]
      },
      {
        "id": "network_chart",
        "type": "bar_chart",
        "title": "Network Traffic",
        "time_range": {"start": 0, "end": 86400000},
        "linked_charts": ["overview_chart", "memory_chart"]
      }
    ]
  }
  
  // 联动状态
  let linkage_state = {
    "active_filters": {},
    "shared_time_range": {"start": 0, "end": 86400000},
    "selected_datasets": {},
    "zoom_level": 1.0,
    "interaction_log": []
  }
  
  // 应用时间范围过滤
  let apply_time_filter = fn(chart_id, time_range, collection, state) {
    if not(linkage_config["shared_time_range"]) {
      return {"success": false, "reason": "time_sharing_disabled"}
    }
    
    let chart = collection["charts"].filter_fn(c => c["id"] == chart_id)[0]
    if not(chart) {
      return {"success": false, "error": "chart_not_found"}
    }
    
    // 更新共享时间范围
    state["shared_time_range"] = time_range
    
    // 记录交互
    state["interaction_log"] = state["interaction_log"].push({
      "action": "time_filter",
      "source_chart": chart_id,
      "time_range": time_range,
      "affected_charts": chart["linked_charts"],
      "timestamp": @lib.time.now()
    })
    
    {
      "success": true,
      "source_chart": chart_id,
      "time_range": time_range,
      "affected_charts": chart["linked_charts"]
    }
  }
  
  // 应用数据集过滤
  let apply_dataset_filter = fn(chart_id, dataset_filter, collection, state) {
    if not(linkage_config["cross_filter"]) {
      return {"success": false, "reason": "cross_filter_disabled"}
    }
    
    let chart = collection["charts"].filter_fn(c => c["id"] == chart_id)[0]
    if not(chart) {
      return {"success": false, "error": "chart_not_found"}
    }
    
    // 更新选中数据集
    state["selected_datasets"][chart_id] = dataset_filter
    
    // 记录交互
    state["interaction_log"] = state["interaction_log"].push({
      "action": "dataset_filter",
      "source_chart": chart_id,
      "filter": dataset_filter,
      "affected_charts": chart["linked_charts"],
      "timestamp": @lib.time.now()
    })
    
    {
      "success": true,
      "source_chart": chart_id,
      "filter": dataset_filter,
      "affected_charts": chart["linked_charts"]
    }
  }
  
  // 同步缩放
  let synchronize_zoom = fn(chart_id, zoom_level, collection, state) {
    if not(linkage_config["synchronized_zoom"]) {
      return {"success": false, "reason": "zoom_sync_disabled"}
    }
    
    let chart = collection["charts"].filter_fn(c => c["id"] == chart_id)[0]
    if not(chart) {
      return {"success": false, "error": "chart_not_found"}
    }
    
    let old_zoom = state["zoom_level"]
    state["zoom_level"] = zoom_level
    
    // 记录交互
    state["interaction_log"] = state["interaction_log"].push({
      "action": "synchronize_zoom",
      "source_chart": chart_id,
      "old_zoom": old_zoom,
      "new_zoom": zoom_level,
      "affected_charts": chart["linked_charts"],
      "timestamp": @lib.time.now()
    })
    
    {
      "success": true,
      "source_chart": chart_id,
      "zoom_level": zoom_level,
      "affected_charts": chart["linked_charts"]
    }
  }
  
  // 测试时间范围过滤
  let time_filter_result = apply_time_filter(
    "overview_chart", 
    {"start": 21600000, "end": 64800000},  // 6:00-18:00
    chart_collection, 
    linkage_state
  )
  
  assert_true(time_filter_result["success"], "时间范围过滤应该成功")
  assert_eq(linkage_state["shared_time_range"]["start"], 21600000, "共享时间范围开始时间应该更新")
  assert_eq(linkage_state["shared_time_range"]["end"], 64800000, "共享时间范围结束时间应该更新")
  assert_true(time_filter_result["affected_charts"].length() > 0, "应该有受影响的图表")
  
  // 测试数据集过滤
  let dataset_filter_result = apply_dataset_filter(
    "cpu_chart",
    {"enabled": ["CPU_Total"], "disabled": ["CPU_User", "CPU_System"]},
    chart_collection,
    linkage_state
  )
  
  assert_true(dataset_filter_result["success"], "数据集过滤应该成功")
  assert_true(linkage_state["selected_datasets"]["cpu_chart"], "CPU图表应该有过滤设置")
  assert_true(dataset_filter_result["affected_charts"].length() > 0, "应该有受影响的图表")
  
  // 测试同步缩放
  let zoom_sync_result = synchronize_zoom("memory_chart", 2.5, chart_collection, linkage_state)
  
  assert_true(zoom_sync_result["success"], "同步缩放应该成功")
  assert_eq(linkage_state["zoom_level"], 2.5, "缩放级别应该更新")
  assert_true(zoom_sync_result["affected_charts"].length() > 0, "应该有受影响的图表")
  
  // 验证交互日志
  assert_true(linkage_state["interaction_log"].length() > 0, "应该有交互日志")
  
  let time_filter_logs = linkage_state["interaction_log"].filter_fn(log => log["action"] == "time_filter")
  let dataset_filter_logs = linkage_state["interaction_log"].filter_fn(log => log["action"] == "dataset_filter")
  let zoom_sync_logs = linkage_state["interaction_log"].filter_fn(log => log["action"] == "synchronize_zoom")
  
  assert_true(time_filter_logs.length() > 0, "应该有时间过滤日志")
  assert_true(dataset_filter_logs.length() > 0, "应该有数据集过滤日志")
  assert_true(zoom_sync_logs.length() > 0, "应该有同步缩放日志")
  
  // 验证联动关系
  let overview_chart = chart_collection["charts"].filter_fn(c => c["id"] == "overview_chart")[0]
  assert_true(overview_chart["linked_charts"].length() > 0, "总览图表应该有联动图表")
  
  // 验证联动影响范围
  for log in linkage_state["interaction_log"] {
    assert_true(log["affected_charts"].length() > 0, "每个交互都应该影响其他图表")
    assert_true(log["timestamp"] > 0, "交互时间戳应该有效")
  }
  
  // 测试联动配置禁用情况
  let original_config = linkage_config["shared_time_range"]
  linkage_config["shared_time_range"] = false
  
  let disabled_time_filter = apply_time_filter(
    "overview_chart",
    {"start": 0, "end": 43200000},
    chart_collection,
    linkage_state
  )
  
  assert_false(disabled_time_filter["success"], "禁用时间共享时过滤应该失败")
  assert_eq(disabled_time_filter["reason"], "time_sharing_disabled", "失败原因应该正确")
  
  // 恢复配置
  linkage_config["shared_time_range"] = original_config
}

// Test 10: 可访问性支持
test "accessibility support" {
  // 可访问性配置
  let accessibility_config = {
    "keyboard_navigation": true,
    "screen_reader_support": true,
    "high_contrast_mode": true,
    "focus_management": true,
    "aria_labels": true,
    "keyboard_shortcuts": {
      "next_chart": "Tab",
      "previous_chart": "Shift+Tab",
      "toggle_legend": "Space",
      "zoom_in": "Plus",
      "zoom_out": "Minus"
    }
  }
  
  // 可访问性状态
  let accessibility_state = {
    "focused_chart": nil,
    "focusable_elements": [],
    "screen_reader_text": {},
    "keyboard_navigation_log": [],
    "high_contrast_enabled": false
  }
  
  // 模拟图表数据
  let chart_data = {
    "title": "Monthly Performance",
    "type": "bar_chart",
    "labels": ["January", "February", "March", "April", "May"],
    "datasets": [
      {
        "label": "Sales",
        "data": [12000, 19000, 15000, 25000, 22000],
        "color": "#3498db"
      },
      {
        "label": "Expenses",
        "data": [8000, 12000, 10000, 14000, 13000],
        "color": "#e74c3c"
      }
    ]
  }
  
  // 生成屏幕阅读器文本
  let generate_screen_reader_text = fn(data, config) {
    if not(config["screen_reader_support"]) {
      return {"success": false, "reason": "screen_reader_disabled"}
    }
    
    let title_text = "Chart: " + data["title"]
    let type_text = "Type: " + data["type"].replace("_", " ")
    
    let data_text = "Data includes " + data["labels"].length().to_string() + " time periods: " + 
                   data["labels"].join(", ")
    
    let datasets_text = data["datasets"].map_fn(dataset => {
      "Dataset " + dataset["label"] + " with values: " + 
      dataset["data"].map_fn(v => v.to_string()).join(", ")
    }).join(". ")
    
    let full_text = title_text + ". " + type_text + ". " + data_text + ". " + datasets_text
    
    {
      "success": true,
      "title": title_text,
      "type": type_text,
      "data": data_text,
      "datasets": datasets_text,
      "full_text": full_text
    }
  }
  
  // 添加键盘导航支持
  let add_keyboard_navigation = fn(chart_id, elements, state, config) {
    if not(config["keyboard_navigation"]) {
      return {"success": false, "reason": "keyboard_navigation_disabled"}
    }
    
    // 为图表添加可聚焦元素
    let focusable_elements = [
      {"id": chart_id + "_title", "type": "heading", "text": chart_data["title"]},
      {"id": chart_id + "_legend", "type": "legend", "datasets": chart_data["datasets"].length()},
      {"id": chart_id + "_canvas", "type": "canvas", "interactive": true},
      {"id": chart_id + "_tooltip", "type": "tooltip", "visible": false}
    ]
    
    // 更新状态
    state["focusable_elements"] = state["focusable_elements"] + focusable_elements
    
    {
      "success": true,
      "chart_id": chart_id,
      "focusable_elements": focusable_elements
    }
  }
  
  // 处理键盘导航
  let handle_keyboard_navigation = fn(key, state, config) {
    if not(config["keyboard_navigation"]) {
      return {"success": false, "reason": "keyboard_navigation_disabled"}
    }
    
    let shortcuts = config["keyboard_shortcuts"]
    let action = nil
    
    // 查找对应的操作
    for shortcut_action in shortcuts.keys() {
      if shortcuts[shortcut_action] == key {
        action = shortcut_action
        break
      }
    }
    
    match action {
      Some(action_name) => {
        // 记录键盘导航
        state["keyboard_navigation_log"] = state["keyboard_navigation_log"].push({
          "key": key,
          "action": action_name,
          "timestamp": @lib.time.now()
        })
        
        {
          "success": true,
          "key": key,
          "action": action_name
        }
      }
      None => {
        {
          "success": false,
          "key": key,
          "reason": "unknown_shortcut"
        }
      }
    }
  }
  
  // 启用高对比度模式
  let enable_high_contrast = fn(state, config) {
    if not(config["high_contrast_mode"]) {
      return {"success": false, "reason": "high_contrast_disabled"}
    }
    
    state["high_contrast_enabled"] = not(state["high_contrast_enabled"])
    
    {
      "success": true,
      "high_contrast_enabled": state["high_contrast_enabled"]
    }
  }
  
  // 测试屏幕阅读器支持
  let screen_reader_result = generate_screen_reader_text(chart_data, accessibility_config)
  
  assert_true(screen_reader_result["success"], "屏幕阅读器文本生成应该成功")
  assert_true(screen_reader_result["title"].length() > 0, "标题文本应该不为空")
  assert_true(screen_reader_result["type"].length() > 0, "类型文本应该不为空")
  assert_true(screen_reader_result["data"].length() > 0, "数据文本应该不为空")
  assert_true(screen_reader_result["datasets"].length() > 0, "数据集文本应该不为空")
  assert_true(screen_reader_result["full_text"].length() > 0, "完整文本应该不为空")
  
  // 测试键盘导航
  let keyboard_nav_result = add_keyboard_navigation("chart_1", [], accessibility_state, accessibility_config)
  
  assert_true(keyboard_nav_result["success"], "键盘导航添加应该成功")
  assert_eq(keyboard_nav_result["focusable_elements"].length(), 4, "应该有4个可聚焦元素")
  
  // 测试键盘快捷键
  let keyboard_shortcuts = ["Tab", "Space", "Plus", "Minus"]
  let shortcut_results = []
  
  for shortcut in keyboard_shortcuts {
    let result = handle_keyboard_navigation(shortcut, accessibility_state, accessibility_config)
    shortcut_results = shortcut_results.push(result)
  }
  
  // 验证键盘快捷键
  assert_eq(shortcut_results.filter_fn(r => r["success"]).length(), keyboard_shortcuts.length(), 
             "所有键盘快捷键应该成功")
  
  for result in shortcut_results {
    if result["success"] {
      assert_not_nil(result["action"], "成功的快捷键应该有对应操作")
    }
  }
  
  // 验证键盘导航日志
  assert_eq(accessibility_state["keyboard_navigation_log"].length(), keyboard_shortcuts.length(), 
             "键盘导航日志应该记录所有操作")
  
  // 测试高对比度模式
  let high_contrast_result = enable_high_contrast(accessibility_state, accessibility_config)
  
  assert_true(high_contrast_result["success"], "高对比度模式切换应该成功")
  assert_true(high_contrast_result["high_contrast_enabled"], "高对比度应该启用")
  
  // 再次切换应该禁用
  let disable_high_contrast = enable_high_contrast(accessibility_state, accessibility_config)
  assert_false(disable_high_contrast["high_contrast_enabled"], "高对比度应该禁用")
  
  // 测试可访问性配置禁用
  let original_config = accessibility_config["screen_reader_support"]
  accessibility_config["screen_reader_support"] = false
  
  let disabled_screen_reader = generate_screen_reader_text(chart_data, accessibility_config)
  assert_false(disabled_screen_reader["success"], "禁用屏幕阅读器时应该失败")
  assert_eq(disabled_screen_reader["reason"], "screen_reader_disabled", "失败原因应该正确")
  
  // 恢复配置
  accessibility_config["screen_reader_support"] = original_config
  
  // 验证可聚焦元素
  for element in accessibility_state["focusable_elements"] {
    assert_true(element["id"].length() > 0, "元素ID应该不为空")
    assert_true(element["type"].length() > 0, "元素类型应该不为空")
  }
  
  // 验证焦点管理
  assert_true(accessibility_config["focus_management"], "焦点管理应该启用")
}