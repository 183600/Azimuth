// Azimuth 数据可视化测试用例
// 专注于数据可视化和图表生成

// 测试1: 图表数据结构
test "图表数据结构" {
  // 定义图表数据结构
  type ChartData = {
    labels: Array[String],
    datasets: Array[{
      label: String,
      data: Array[Float],
      backgroundColor: String,
      borderColor: String
    }]
  }
  
  type PieChartData = {
    labels: Array[String],
    datasets: Array[{
      data: Array[Float],
      backgroundColor: Array[String]
    }]
  }
  
  // 创建线图数据
  let create_line_chart_data = fn(labels: Array[String], datasets: Array[(String, Array[Float], String, String)]) {
    let mut chart_datasets = []
    
    for (label, data, bg_color, border_color) in datasets {
      chart_datasets = chart_datasets.push({
        label: label,
        data: data,
        backgroundColor: bg_color,
        borderColor: border_color
      })
    }
    
    {
      labels: labels,
      datasets: chart_datasets
    }
  }
  
  // 创建饼图数据
  let create_pie_chart_data = fn(labels: Array[String], data: Array[Float], colors: Array[String]) {
    {
      labels: labels,
      datasets: [{
        data: data,
        backgroundColor: colors
      }]
    }
  }
  
  // 测试图表数据结构
  let time_labels = ["00:00", "04:00", "08:00", "12:00", "16:00", "20:00"]
  let cpu_data = [30.0, 45.0, 60.0, 75.0, 65.0, 40.0]
  let memory_data = [50.0, 55.0, 65.0, 70.0, 60.0, 55.0]
  
  let line_chart_data = create_line_chart_data(
    time_labels,
    [
      ("CPU Usage", cpu_data, "rgba(255, 99, 132, 0.2)", "rgb(255, 99, 132)"),
      ("Memory Usage", memory_data, "rgba(54, 162, 235, 0.2)", "rgb(54, 162, 235)")
    ]
  )
  
  assert_eq(line_chart_data.labels.length(), 6)
  assert_eq(line_chart_data.datasets.length(), 2)
  assert_eq(line_chart_data.datasets[0].label, "CPU Usage")
  assert_eq(line_chart_data.datasets[1].label, "Memory Usage")
  assert_eq(line_chart_data.datasets[0].data[3], 75.0)  // CPU在12:00的值
  assert_eq(line_chart_data.datasets[1].data[2], 65.0)  // Memory在08:00的值
  
  // 测试饼图数据
  let service_labels = ["Auth Service", "Payment Service", "Order Service", "Notification Service"]
  let service_data = [25.0, 35.0, 30.0, 10.0]
  let service_colors = ["#FF6384", "#36A2EB", "#FFCE56", "#4BC0C0"]
  
  let pie_chart_data = create_pie_chart_data(service_labels, service_data, service_colors)
  
  assert_eq(pie_chart_data.labels.length(), 4)
  assert_eq(pie_chart_data.datasets.length(), 1)
  assert_eq(pie_chart_data.datasets[0].data[1], 35.0)  // Payment Service的百分比
  assert_eq(pie_chart_data.datasets[0].backgroundColor[2], "#FFCE56")  // Order Service的颜色
  
  // 测试数据验证
  let validate_chart_data = fn(data: ChartData) {
    let mut valid = true
    let mut error_messages = []
    
    // 检查标签和数据长度一致
    for dataset in data.datasets {
      if dataset.data.length() != data.labels.length() {
        valid = false
        error_messages = error_messages.push("Data length does not match labels length")
        break
      }
    }
    
    // 检查数据是否为数字
    for dataset in data.datasets {
      for value in dataset.data {
        if value.is_nan() || value.is_infinite() {
          valid = false
          error_messages = error_messages.push("Data contains invalid values")
          break
        }
      }
    }
    
    (valid, error_messages)
  }
  
  let (valid, errors) = validate_chart_data(line_chart_data)
  assert_true(valid)
  assert_eq(errors.length(), 0)
  
  // 测试无效数据
  let invalid_data = create_line_chart_data(
    ["00:00", "04:00"],  // 2个标签
    [
      ("CPU", [30.0, 45.0, 60.0], "red", "red")  // 3个数据点，不匹配
    ]
  )
  
  let (invalid_valid, invalid_errors) = validate_chart_data(invalid_data)
  assert_false(invalid_valid)
  assert_true(invalid_errors.length() > 0)
}

// 测试2: 图表配置和选项
test "图表配置和选项" {
  // 定义图表选项
  type ChartOptions = {
    responsive: Bool,
    plugins: {
      title: {
        display: Bool,
        text: String
      },
      legend: {
        display: Bool,
        position: String
      }
    },
    scales: {
      x: {
        display: Bool,
        title: {
          display: Bool,
          text: String
        }
      },
      y: {
        display: Bool,
        title: {
          display: Bool,
          text: String
        },
        begin_at_zero: Bool
      }
    }
  }
  
  // 创建默认图表选项
  let create_default_options = fn(title: String, x_title: String, y_title: String) {
    {
      responsive: true,
      plugins: {
        title: {
          display: true,
          text: title
        },
        legend: {
          display: true,
          position: "top"
        }
      },
      scales: {
        x: {
          display: true,
          title: {
            display: true,
            text: x_title
          }
        },
        y: {
          display: true,
          title: {
            display: true,
            text: y_title
          },
          begin_at_zero: true
        }
      }
    }
  }
  
  // 创建饼图选项
  let create_pie_options = fn(title: String) {
    {
      responsive: true,
      plugins: {
        title: {
          display: true,
          text: title
        },
        legend: {
          display: true,
          position: "right"
        }
      },
      scales: {
        x: {
          display: false,
          title: {
            display: false,
            text: ""
          }
        },
        y: {
          display: false,
          title: {
            display: false,
            text: ""
          },
          begin_at_zero: false
        }
      }
    }
  }
  
  // 测试图表选项创建
  let line_options = create_default_options("System Metrics", "Time", "Usage (%)")
  
  assert_true(line_options.responsive)
  assert_eq(line_options.plugins.title.text, "System Metrics")
  assert_eq(line_options.plugins.legend.position, "top")
  assert_true(line_options.scales.x.display)
  assert_eq(line_options.scales.x.title.text, "Time")
  assert_eq(line_options.scales.y.title.text, "Usage (%)")
  assert_true(line_options.scales.y.begin_at_zero)
  
  let pie_options = create_pie_options("Service Distribution")
  
  assert_true(pie_options.responsive)
  assert_eq(pie_options.plugins.title.text, "Service Distribution")
  assert_eq(pie_options.plugins.legend.position, "right")
  assert_false(pie_options.scales.x.display)
  assert_false(pie_options.scales.y.display)
  
  // 测试选项合并
  let merge_options = fn(base: ChartOptions, override: ChartOptions) {
    {
      responsive: override.responsive,
      plugins: {
        title: {
          display: override.plugins.title.display,
          text: override.plugins.title.text
        },
        legend: {
          display: override.plugins.legend.display,
          position: override.plugins.legend.position
        }
      },
      scales: {
        x: {
          display: override.scales.x.display,
          title: {
            display: override.scales.x.title.display,
            text: override.scales.x.title.text
          }
        },
        y: {
          display: override.scales.y.display,
          title: {
            display: override.scales.y.title.display,
            text: override.scales.y.title.text
          },
          begin_at_zero: override.scales.y.begin_at_zero
        }
      }
    }
  }
  
  let custom_options = {
    responsive: false,
    plugins: {
      title: {
        display: false,
        text: ""
      },
      legend: {
        display: false,
        position: "bottom"
      }
    },
    scales: {
      x: {
        display: true,
        title: {
          display: false,
          text: ""
        }
      },
      y: {
        display: true,
        title: {
          display: true,
          text: "Value"
        },
        begin_at_zero: false
      }
    }
  }
  
  let merged_options = merge_options(line_options, custom_options)
  
  assert_false(merged_options.responsive)
  assert_false(merged_options.plugins.title.display)
  assert_false(merged_options.plugins.legend.display)
  assert_eq(merged_options.plugins.legend.position, "bottom")
  assert_true(merged_options.scales.x.display)
  assert_false(merged_options.scales.x.title.display)
  assert_eq(merged_options.scales.y.title.text, "Value")
  assert_false(merged_options.scales.y.begin_at_zero)
}

// 测试3: 图表类型转换
test "图表类型转换" {
  // 定义通用数据点
  type DataPoint = {
    x: String,
    y: Float
  }
  
  // 将线图数据转换为散点图数据
  let convert_line_to_scatter = fn(labels: Array[String], data: Array[Float]) {
    let mut scatter_data = []
    
    for i in 0..labels.length() {
      if i < data.length() {
        scatter_data = scatter_data.push({
          x: labels[i],
          y: data[i]
        })
      }
    }
    
    scatter_data
  }
  
  // 将多个数据集转换为堆叠图数据
  let convert_to_stacked_data = fn(labels: Array[String], datasets: Array[(String, Array[Float])]) {
    let mut stacked_data = []
    
    for i in 0..labels.length() {
      let mut stacked_point = []
      
      for (_, data) in datasets {
        if i < data.length() {
          stacked_point = stacked_point.push(data[i])
        } else {
          stacked_point = stacked_point.push(0.0)
        }
      }
      
      stacked_data = stacked_point.push(stacked_point)
    }
    
    stacked_data
  }
  
  // 计算堆叠数据的累积值
  let calculate_stacked_totals = fn(stacked_data: Array[Array[Float]]) {
    let mut totals = []
    
    for data_point in stacked_data {
      let mut total = 0.0
      for value in data_point {
        total = total + value
      }
      totals = totals.push(total)
    }
    
    totals
  }
  
  // 测试图表类型转换
  let time_labels = ["Jan", "Feb", "Mar", "Apr", "May"]
  let revenue_data = [100.0, 150.0, 180.0, 220.0, 280.0]
  
  let scatter_data = convert_line_to_scatter(time_labels, revenue_data)
  
  assert_eq(scatter_data.length(), 5)
  assert_eq(scatter_data[2].x, "Mar")
  assert_eq(scatter_data[2].y, 180.0)
  
  // 测试堆叠数据转换
  let datasets = [
    ("Product A", [50.0, 60.0, 70.0, 80.0, 90.0]),
    ("Product B", [30.0, 40.0, 50.0, 60.0, 70.0]),
    ("Product C", [20.0, 30.0, 40.0, 50.0, 60.0])
  ]
  
  let stacked_data = convert_to_stacked_data(time_labels, datasets)
  
  assert_eq(stacked_data.length(), 5)
  assert_eq(stacked_data[0], [50.0, 30.0, 20.0])  // Jan的数据
  assert_eq(stacked_data[4], [90.0, 70.0, 60.0])  // May的数据
  
  let stacked_totals = calculate_stacked_totals(stacked_data)
  
  assert_eq(stacked_totals.length(), 5)
  assert_eq(stacked_totals[0], 100.0)  // 50+30+20
  assert_eq(stacked_totals[4], 220.0)  // 90+70+60
  
  // 测试数据归一化
  let normalize_data = fn(data: Array[Float], min: Float, max: Float) {
    let mut normalized = []
    
    for value in data {
      let normalized_value = (value - min) / (max - min)
      normalized = normalized.push(normalized_value)
    }
    
    normalized
  }
  
  let find_min_max = fn(data: Array[Float]) {
    if data.length() == 0 {
      return (0.0, 0.0)
    }
    
    let mut min = data[0]
    let mut max = data[0]
    
    for value in data {
      if value < min {
        min = value
      }
      if value > max {
        max = value
      }
    }
    
    (min, max)
  }
  
  let (min_val, max_val) = find_min_max(revenue_data)
  let normalized_data = normalize_data(revenue_data, min_val, max_val)
  
  assert_eq(normalized_data.length(), 5)
  assert_eq(normalized_data[0], 0.0)  // 最小值归一化为0
  assert_eq(normalized_data[4], 1.0)  // 最大值归一化为1
  assert_true(normalized_data[2] > 0.0 && normalized_data[2] < 1.0)  // 中间值在0和1之间
}

// 测试4: 图表交互功能
test "图表交互功能" {
  // 定义图表事件
  type ChartEvent = {
    type: String,
    element: {
      index: Int,
      datasetIndex: Int
    },
    data: {
      label: String,
      value: Float
    }
  }
  
  // 模拟点击事件处理
  let handle_chart_click = fn(event: ChartEvent, callback: (String, Float) -> String) {
    if event.type == "click" {
      let label = event.data.label
      let value = event.data.value
      callback(label, value)
    } else {
      "No action for event type: " + event.type
    }
  }
  
  // 模拟悬停事件处理
  let handle_chart_hover = fn(event: ChartEvent, callback: (String, Float) -> String) {
    if event.type == "hover" {
      let label = event.data.label
      let value = event.data.value
      callback(label, value)
    } else {
      "No action for event type: " + event.type
    }
  }
  
  // 测试图表交互
  let click_callback = fn(label: String, value: Float) {
    "Clicked on " + label + " with value " + value.to_string()
  }
  
  let hover_callback = fn(label: String, value: Float) {
    "Hovering over " + label + " with value " + value.to_string()
  }
  
  let click_event = {
    type: "click",
    element: {
      index: 2,
      datasetIndex: 0
    },
    data: {
      label: "March",
      value: 180.0
    }
  }
  
  let hover_event = {
    type: "hover",
    element: {
      index: 3,
      datasetIndex: 1
    },
    data: {
      label: "April",
      value: 220.0
    }
  }
  
  let click_result = handle_chart_click(click_event, click_callback)
  let hover_result = handle_chart_hover(hover_event, hover_callback)
  
  assert_eq(click_result, "Clicked on March with value 180.0")
  assert_eq(hover_result, "Hovering over April with value 220.0")
  
  // 测试图表缩放和平移
  type ZoomState = {
    min: Float,
    max: Float,
    scale: Float
  }
  
  let apply_zoom = fn(current_state: ZoomState, zoom_factor: Float, center_point: Float) {
    let range = current_state.max - current_state.min
    let new_range = range / zoom_factor
    
    let offset = center_point - current_state.min
    let center_ratio = offset / range
    
    let new_min = center_point - new_range * center_ratio
    let new_max = new_min + new_range
    
    {
      min: new_min,
      max: new_max,
      scale: current_state.scale * zoom_factor
    }
  }
  
  let apply_pan = fn(current_state: ZoomState, pan_amount: Float) {
    let range = current_state.max - current_state.min
    let pan_ratio = pan_amount / range
    
    let new_min = current_state.min - range * pan_ratio
    let new_max = current_state.max - range * pan_ratio
    
    {
      min: new_min,
      max: new_max,
      scale: current_state.scale
    }
  }
  
  // 测试缩放
  let initial_zoom = {
    min: 0.0,
    max: 100.0,
    scale: 1.0
  }
  
  let zoomed_in = apply_zoom(initial_zoom, 2.0, 50.0)  // 放大2倍，中心在50
  
  assert_eq(zoomed_in.scale, 2.0)
  assert_eq(zoomed_in.min, 25.0)  // 50 - (100/2)/2
  assert_eq(zoomed_in.max, 75.0)  // 25 + 50
  
  // 测试平移
  let panned = apply_pan(zoomed_in, 10.0)  // 向右平移10%
  
  assert_eq(panned.scale, 2.0)  // 缩放不变
  assert_eq(panned.min, 20.0)   // 25 - 50*0.1
  assert_eq(panned.max, 70.0)   // 75 - 50*0.1
  
  // 测试数据过滤
  let filter_data = fn(data: Array[Float], min_threshold: Float, max_threshold: Float) {
    let mut filtered = []
    
    for value in data {
      if value >= min_threshold && value <= max_threshold {
        filtered = filtered.push(value)
      }
    }
    
    filtered
  }
  
  let test_data = [10.0, 25.0, 50.0, 75.0, 90.0, 110.0, 150.0]
  let filtered_data = filter_data(test_data, 20.0, 100.0)
  
  assert_eq(filtered_data.length(), 4)
  assert_eq(filtered_data[0], 25.0)
  assert_eq(filtered_data[1], 50.0)
  assert_eq(filtered_data[2], 75.0)
  assert_eq(filtered_data[3], 90.0)
}

// 测试5: 图表导出功能
test "图表导出功能" {
  // 模拟图表导出为不同格式
  type ExportFormat = {
    name: String,
    extension: String,
    mime_type: String
  }
  
  let supported_formats = [
    { name: "PNG", extension: "png", mime_type: "image/png" },
    { name: "JPEG", extension: "jpg", mime_type: "image/jpeg" },
    { name: "SVG", extension: "svg", mime_type: "image/svg+xml" },
    { name: "PDF", extension: "pdf", mime_type: "application/pdf" }
  ]
  
  // 模拟导出功能
  let export_chart = fn(chart_data: { String: String }, format: ExportFormat) {
    let export_data = {
      data: "base64_encoded_image_data",
      filename: "chart." + format.extension,
      mime_type: format.mime_type,
      size: 1024  // 模拟文件大小（字节）
    }
    
    export_data
  }
  
  // 测试图表导出
  let mock_chart_data = {
    "title": "System Metrics",
    "type": "line",
    "data": "serialized_chart_data"
  }
  
  let png_export = export_chart(mock_chart_data, supported_formats[0])
  let svg_export = export_chart(mock_chart_data, supported_formats[2])
  
  assert_eq(png_export.filename, "chart.png")
  assert_eq(png_export.mime_type, "image/png")
  assert_eq(png_export.size, 1024)
  
  assert_eq(svg_export.filename, "chart.svg")
  assert_eq(svg_export.mime_type, "image/svg+xml")
  
  // 测试导出配置
  type ExportConfig = {
    format: ExportFormat,
    width: Int,
    height: Int,
    quality: Float,
    background_color: String
  }
  
  let create_export_config = fn(format_index: Int, width: Int, height: Int, quality: Float, background_color: String) {
    {
      format: supported_formats[format_index],
      width: width,
      height: height,
      quality: quality,
      background_color: background_color
    }
  }
  
  let export_with_config = fn(chart_data: { String: String }, config: ExportConfig) {
    let export_data = {
      data: "base64_encoded_image_data",
      filename: "chart." + config.format.extension,
      mime_type: config.format.mime_type,
      size: (config.width * config.height * config.quality).to_int() / 100,  // 模拟文件大小计算
      config: config
    }
    
    export_data
  }
  
  // 测试配置导出
  let export_config = create_export_config(1, 800, 600, 0.8, "#ffffff")  // JPEG格式
  let config_export = export_with_config(mock_chart_data, export_config)
  
  assert_eq(config_export.filename, "chart.jpg")
  assert_eq(config_export.mime_type, "image/jpeg")
  assert_eq(config_export.size, (800 * 600 * 0.8).to_int() / 100)  // 3840
  assert_eq(config_export.config.width, 800)
  assert_eq(config_export.config.height, 600)
  assert_eq(config_export.config.quality, 0.8)
  assert_eq(config_export.config.background_color, "#ffffff")
  
  // 测试数据导出为CSV
  let export_to_csv = fn(labels: Array[String], datasets: Array[(String, Array[Float])]) {
    let mut csv_content = "Label"
    
    // 添加标题行
    for (label, _) in datasets {
      csv_content = csv_content + "," + label
    }
    csv_content = csv_content + "\n"
    
    // 添加数据行
    for i in 0..labels.length() {
      csv_content = csv_content + labels[i]
      
      for (_, data) in datasets {
        if i < data.length() {
          csv_content = csv_content + "," + data[i].to_string()
        } else {
          csv_content = csv_content + ","
        }
      }
      csv_content = csv_content + "\n"
    }
    
    csv_content
  }
  
  // 测试CSV导出
  let csv_labels = ["Jan", "Feb", "Mar", "Apr", "May"]
  let csv_datasets = [
    ("Revenue", [100.0, 150.0, 180.0, 220.0, 280.0]),
    ("Costs", [80.0, 90.0, 100.0, 110.0, 120.0])
  ]
  
  let csv_content = export_to_csv(csv_labels, csv_datasets)
  
  assert_true(csv_content.contains("Label,Revenue,Costs"))
  assert_true(csv_content.contains("Jan,100.0,80.0"))
  assert_true(csv_content.contains("May,280.0,120.0"))
  
  // 测试数据导出为JSON
  let export_to_json = fn(labels: Array[String], datasets: Array[(String, Array[Float])]) {
    let mut json_content = "{\n"
    json_content = json_content + "  \"labels\": ["
    
    // 添加标签
    for i in 0..labels.length() {
      json_content = json_content + "\"" + labels[i] + "\""
      if i < labels.length() - 1 {
        json_content = json_content + ", "
      }
    }
    json_content = json_content + "],\n"
    
    json_content = json_content + "  \"datasets\": [\n"
    
    // 添加数据集
    for i in 0..datasets.length() {
      let (label, data) = datasets[i]
      json_content = json_content + "    {\n"
      json_content = json_content + "      \"label\": \"" + label + "\",\n"
      json_content = json_content + "      \"data\": ["
      
      for j in 0..data.length() {
        json_content = json_content + data[j].to_string()
        if j < data.length() - 1 {
          json_content = json_content + ", "
        }
      }
      json_content = json_content + "]\n"
      json_content = json_content + "    }"
      
      if i < datasets.length() - 1 {
        json_content = json_content + ","
      }
      json_content = json_content + "\n"
    }
    
    json_content = json_content + "  ]\n"
    json_content = json_content + "}"
    
    json_content
  }
  
  // 测试JSON导出
  let json_content = export_to_json(csv_labels, csv_datasets)
  
  assert_true(json_content.contains("\"labels\": [\"Jan\", \"Feb\", \"Mar\", \"Apr\", \"May\"]"))
  assert_true(json_content.contains("\"label\": \"Revenue\""))
  assert_true(json_content.contains("\"data\": [100.0, 150.0, 180.0, 220.0, 280.0]"))
  assert_true(json_content.contains("\"label\": \"Costs\""))
  assert_true(json_content.contains("\"data\": [80.0, 90.0, 100.0, 110.0, 120.0]"))
}