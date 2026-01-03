// Azimuth 遥测系统数据可视化测试
// 专注于遥测数据的可视化生成和图表渲染功能

// 测试1: 时间序列图表生成
test "时间序列图表生成" {
  // 模拟时间序列遥测数据
  let time_series_data = [
    { timestamp: 1640995200, metric: "cpu", value: 45.0, service: "auth" },
    { timestamp: 1640995260, metric: "cpu", value: 50.0, service: "auth" },
    { timestamp: 1640995320, metric: "cpu", value: 48.0, service: "auth" },
    { timestamp: 1640995380, metric: "cpu", value: 55.0, service: "auth" },
    { timestamp: 1640995440, metric: "cpu", value: 52.0, service: "auth" },
    { timestamp: 1640995500, metric: "cpu", value: 60.0, service: "auth" },
    { timestamp: 1640995560, metric: "cpu", value: 58.0, service: "auth" },
    { timestamp: 1640995620, metric: "cpu", value: 65.0, service: "auth" }
  ]
  
  // 时间序列图表配置
  let chart_config = {
    title: "CPU使用率趋势",
    type: "line",
    x_axis: { label: "时间", format: "datetime" },
    y_axis: { label: "CPU使用率 (%)", min: 0, max: 100 },
    width: 800,
    height: 400,
    colors: ["#3498db"],
    grid: true,
    legend: true
  }
  
  // 时间序列图表生成函数
  let generate_time_series_chart = fn(data, config) {
    // 提取时间点和值
    let mut time_points = []
    let mut values = []
    
    for point in data {
      time_points = time_points.push(point.timestamp)
      values = values.push(point.value)
    }
    
    // 计算数据范围
    let mut min_value = values[0]
    let mut max_value = values[0]
    for v in values {
      if v < min_value { min_value = v }
      if v > max_value { max_value = v }
    }
    
    // 生成图表数据结构
    let chart_data = {
      title: config.title,
      type: config.type,
      x_axis: {
        label: config.x_axis.label,
        data: time_points,
        format: config.x_axis.format
      },
      y_axis: {
        label: config.y_axis.label,
        min: config.y_axis.min,
        max: config.y_axis.max,
        data_min: min_value,
        data_max: max_value
      },
      series: [
        {
          name: "CPU使用率",
          data: values,
          color: config.colors[0]
        }
      ],
      dimensions: {
        width: config.width,
        height: config.height
      },
      styling: {
        grid: config.grid,
        legend: config.legend
      }
    }
    
    chart_data
  }
  
  // 生成时间序列图表
  let chart = generate_time_series_chart(time_series_data, chart_config)
  
  // 验证图表结构
  assert_eq(chart.title, "CPU使用率趋势")
  assert_eq(chart.type, "line")
  assert_eq(chart.x_axis.label, "时间")
  assert_eq(chart.x_axis.format, "datetime")
  assert_eq(chart.x_axis.data.length(), 8)
  assert_eq(chart.y_axis.label, "CPU使用率 (%)")
  assert_eq(chart.y_axis.min, 0)
  assert_eq(chart.y_axis.max, 100)
  assert_eq(chart.y_axis.data_min, 45.0)
  assert_eq(chart.y_axis.data_max, 65.0)
  assert_eq(chart.series.length(), 1)
  assert_eq(chart.series[0].name, "CPU使用率")
  assert_eq(chart.series[0].data.length(), 8)
  assert_eq(chart.series[0].color, "#3498db")
  assert_eq(chart.dimensions.width, 800)
  assert_eq(chart.dimensions.height, 400)
  assert_eq(chart.styling.grid, true)
  assert_eq(chart.styling.legend, true)
  
  // 验证数据点
  assert_eq(chart.x_axis.data[0], 1640995200)
  assert_eq(chart.series[0].data[0], 45.0)
  assert_eq(chart.x_axis.data[7], 1640995620)
  assert_eq(chart.series[0].data[7], 65.0)
}

// 测试2: 多指标对比图表生成
test "多指标对比图表生成" {
  // 模拟多指标遥测数据
  let multi_metric_data = [
    { timestamp: 1640995200, cpu: 45.0, memory: 1024.0, disk: 2048.0 },
    { timestamp: 1640995260, cpu: 50.0, memory: 1050.0, disk: 2100.0 },
    { timestamp: 1640995320, cpu: 48.0, memory: 1080.0, disk: 2150.0 },
    { timestamp: 1640995380, cpu: 55.0, memory: 1100.0, disk: 2200.0 },
    { timestamp: 1640995440, cpu: 52.0, memory: 1120.0, disk: 2250.0 }
  ]
  
  // 多指标图表配置
  let chart_config = {
    title: "系统资源使用情况",
    type: "multi_line",
    x_axis: { label: "时间", format: "datetime" },
    y_axes: [
      { label: "CPU/Memory (%)", position: "left", min: 0, max: 100 },
      { label: "Disk (GB)", position: "right", min: 0, max: 3000 }
    ],
    width: 900,
    height: 450,
    colors: ["#e74c3c", "#2ecc71", "#f39c12"],
    grid: true,
    legend: true
  }
  
  // 数据标准化函数
  let normalize_value = fn(value, min, max) {
    (value - min) / (max - min) * 100.0
  }
  
  // 多指标图表生成函数
  let generate_multi_metric_chart = fn(data, config) {
    // 提取时间点
    let mut time_points = []
    for point in data {
      time_points = time_points.push(point.timestamp)
    }
    
    // 提取各指标数据
    let mut cpu_values = []
    let mut memory_values = []
    let mut disk_values = []
    
    for point in data {
      cpu_values = cpu_values.push(point.cpu)
      memory_values = memory_values.push(point.memory)
      disk_values = disk_values.push(point.disk)
    }
    
    // 标准化内存值（假设最大内存为2048MB）
    let mut normalized_memory = []
    for mem in memory_values {
      let normalized = normalize_value(mem, 0.0, 2048.0)
      normalized_memory = normalized_memory.push(normalized)
    }
    
    // 标准化磁盘值（假设最大磁盘为3000GB）
    let mut normalized_disk = []
    for disk in disk_values {
      let normalized = normalize_value(disk, 0.0, 3000.0)
      normalized_disk = normalized_disk.push(normalized)
    }
    
    // 生成图表数据结构
    let chart_data = {
      title: config.title,
      type: config.type,
      x_axis: {
        label: config.x_axis.label,
        data: time_points,
        format: config.x_axis.format
      },
      y_axes: config.y_axes,
      series: [
        {
          name: "CPU使用率",
          data: cpu_values,
          color: config.colors[0],
          y_axis: 0
        },
        {
          name: "内存使用率",
          data: normalized_memory,
          color: config.colors[1],
          y_axis: 0
        },
        {
          name: "磁盘使用率",
          data: normalized_disk,
          color: config.colors[2],
          y_axis: 1
        }
      ],
      dimensions: {
        width: config.width,
        height: config.height
      },
      styling: {
        grid: config.grid,
        legend: config.legend
      }
    }
    
    chart_data
  }
  
  // 生成多指标图表
  let chart = generate_multi_metric_chart(multi_metric_data, chart_config)
  
  // 验证图表结构
  assert_eq(chart.title, "系统资源使用情况")
  assert_eq(chart.type, "multi_line")
  assert_eq(chart.x_axis.data.length(), 5)
  assert_eq(chart.y_axes.length(), 2)
  assert_eq(chart.y_axes[0].label, "CPU/Memory (%)")
  assert_eq(chart.y_axes[0].position, "left")
  assert_eq(chart.y_axes[1].label, "Disk (GB)")
  assert_eq(chart.y_axes[1].position, "right")
  assert_eq(chart.series.length(), 3)
  
  // 验证CPU系列
  let cpu_series = chart.series[0]
  assert_eq(cpu_series.name, "CPU使用率")
  assert_eq(cpu_series.color, "#e74c3c")
  assert_eq(cpu_series.y_axis, 0)
  assert_eq(cpu_series.data.length(), 5)
  assert_eq(cpu_series.data[0], 45.0)
  
  // 验证内存系列（已标准化）
  let memory_series = chart.series[1]
  assert_eq(memory_series.name, "内存使用率")
  assert_eq(memory_series.color, "#2ecc71")
  assert_eq(memory_series.y_axis, 0)
  assert_eq(memory_series.data.length(), 5)
  assert_eq(memory_series.data[0], 50.0) // 1024/2048*100
  
  // 验证磁盘系列（已标准化）
  let disk_series = chart.series[2]
  assert_eq(disk_series.name, "磁盘使用率")
  assert_eq(disk_series.color, "#f39c12")
  assert_eq(disk_series.y_axis, 1)
  assert_eq(disk_series.data.length(), 5)
  assert_eq(disk_series.data[0], 68.26666666666667) // 2048/3000*100
}

// 测试3: 饼图生成
test "饼图生成" {
  // 模拟分类遥测数据
  let category_data = [
    { category: "auth-service", request_count: 4500, error_rate: 0.5 },
    { category: "payment-service", request_count: 3200, error_rate: 0.2 },
    { category: "user-service", request_count: 2800, error_rate: 0.8 },
    { category: "order-service", request_count: 2100, error_rate: 1.2 },
    { category: "notification-service", request_count: 1400, error_rate: 0.3 }
  ]
  
  // 饼图配置
  let chart_config = {
    title: "服务请求量分布",
    type: "pie",
    width: 500,
    height: 500,
    colors: ["#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6"],
    show_percentage: true,
    show_legend: true,
    inner_radius: 0, // 0表示实心饼图，大于0表示环形图
    tooltip_format: "{name}: {value} ({percentage}%)"
  }
  
  // 饼图生成函数
  let generate_pie_chart = fn(data, config) {
    // 计算总请求量
    let mut total_requests = 0
    for item in data {
      total_requests = total_requests + item.request_count
    }
    
    // 计算百分比和角度
    let mut chart_segments = []
    let mut current_angle = 0.0
    
    for item in data {
      let percentage = (item.request_count.to_float() / total_requests.to_float()) * 100.0
      let angle = (item.request_count.to_float() / total_requests.to_float()) * 360.0
      
      let segment = {
        name: item.category,
        value: item.request_count,
        percentage: percentage,
        start_angle: current_angle,
        end_angle: current_angle + angle,
        error_rate: item.error_rate
      }
      
      chart_segments = chart_segments.push(segment)
      current_angle = current_angle + angle
    }
    
    // 按请求量排序（从大到小）
    let mut i = 0
    while i < chart_segments.length() {
      let mut j = i + 1
      while j < chart_segments.length() {
        if chart_segments[i].value < chart_segments[j].value {
          let temp = chart_segments[i]
          chart_segments[i] = chart_segments[j]
          chart_segments[j] = temp
        }
        j = j + 1
      }
      i = i + 1
    }
    
    // 生成图表数据结构
    let chart_data = {
      title: config.title,
      type: config.type,
      total: total_requests,
      segments: chart_segments,
      dimensions: {
        width: config.width,
        height: config.height
      },
      styling: {
        colors: config.colors,
        show_percentage: config.show_percentage,
        show_legend: config.show_legend,
        inner_radius: config.inner_radius,
        tooltip_format: config.tooltip_format
      }
    }
    
    chart_data
  }
  
  // 生成饼图
  let chart = generate_pie_chart(category_data, chart_config)
  
  // 验证图表结构
  assert_eq(chart.title, "服务请求量分布")
  assert_eq(chart.type, "pie")
  assert_eq(chart.total, 14000) // 4500+3200+2800+2100+1400
  assert_eq(chart.segments.length(), 5)
  assert_eq(chart.dimensions.width, 500)
  assert_eq(chart.dimensions.height, 500)
  assert_eq(chart.styling.colors.length(), 5)
  assert_eq(chart.styling.show_percentage, true)
  assert_eq(chart.styling.show_legend, true)
  assert_eq(chart.styling.inner_radius, 0)
  
  // 验证排序后的第一个分段（最大值）
  let first_segment = chart.segments[0]
  assert_eq(first_segment.name, "auth-service")
  assert_eq(first_segment.value, 4500)
  assert_eq(first_segment.percentage, 32.142857142857146) // 4500/14000*100
  assert_eq(first_segment.start_angle, 0.0)
  assert_eq(first_segment.end_angle, 115.71428571428571) // 4500/14000*360
  assert_eq(first_segment.error_rate, 0.5)
  
  // 验证排序后的最后一个分段（最小值）
  let last_segment = chart.segments[4]
  assert_eq(last_segment.name, "notification-service")
  assert_eq(last_segment.value, 1400)
  assert_eq(last_segment.percentage, 10.0) // 1400/14000*100
  assert_eq(last_segment.start_angle, 324.0)
  assert_eq(last_segment.end_angle, 360.0) // 1400/14000*360
  assert_eq(last_segment.error_rate, 0.3)
  
  // 验证角度总和
  let mut total_angle = 0.0
  for segment in chart.segments {
    total_angle = total_angle + (segment.end_angle - segment.start_angle)
  }
  assert_eq(total_angle, 360.0)
  
  // 验证百分比总和
  let mut total_percentage = 0.0
  for segment in chart.segments {
    total_percentage = total_percentage + segment.percentage
  }
  assert_eq(total_percentage, 100.0)
}

// 测试4: 热力图生成
test "热力图生成" {
  // 模拟矩阵遥测数据
  let matrix_data = [
    { hour: 0, day: "周一", value: 15.0 },
    { hour: 0, day: "周二", value: 12.0 },
    { hour: 0, day: "周三", value: 18.0 },
    { hour: 0, day: "周四", value: 20.0 },
    { hour: 0, day: "周五", value: 25.0 },
    { hour: 12, day: "周一", value: 45.0 },
    { hour: 12, day: "周二", value: 52.0 },
    { hour: 12, day: "周三", value: 48.0 },
    { hour: 12, day: "周四", value: 55.0 },
    { hour: 12, day: "周五", value: 65.0 },
    { hour: 18, day: "周一", value: 35.0 },
    { hour: 18, day: "周二", value: 38.0 },
    { hour: 18, day: "周三", value: 32.0 },
    { hour: 18, day: "周四", value: 40.0 },
    { hour: 18, day: "周五", value: 42.0 }
  ]
  
  // 热力图配置
  let chart_config = {
    title: "一周内不同时段的CPU使用率",
    type: "heatmap",
    x_axis: { label: "星期", categories: ["周一", "周二", "周三", "周四", "周五"] },
    y_axis: { label: "小时", categories: [0, 12, 18] },
    width: 600,
    height: 400,
    color_scale: {
      min: "#ecf0f1",  // 浅色（低值）
      max: "#e74c3c"   // 深色（高值）
    },
    show_values: true,
    cell_padding: 2
  }
  
  // 热力图生成函数
  let generate_heatmap = fn(data, config) {
    // 计算数据范围
    let mut min_value = data[0].value
    let mut max_value = data[0].value
    for item in data {
      if item.value < min_value { min_value = item.value }
      if item.value > max_value { max_value = item.value }
    }
    
    // 颜色插值函数
    let interpolate_color = fn(value, min, max, min_color, max_color) {
      let ratio = (value - min) / (max - min)
      // 简化的颜色插值（实际实现会更复杂）
      if ratio < 0.33 {
        "#ecf0f1"  // 浅色
      } else if ratio < 0.66 {
        "#f39c12"  // 中等颜色
      } else {
        "#e74c3c"  // 深色
      }
    }
    
    // 构建矩阵数据
    let mut matrix_cells = []
    let x_categories = config.x_axis.categories
    let y_categories = config.y_axis.categories
    
    for y_index in range(0, y_categories.length()) {
      let y_value = y_categories[y_index]
      
      for x_index in range(0, x_categories.length()) {
        let x_value = x_categories[x_index]
        
        // 查找对应的数据点
        let mut cell_value = 0.0
        let mut found = false
        
        for item in data {
          if item.hour == y_value and item.day == x_value {
            cell_value = item.value
            found = true
            break
          }
        }
        
        if found {
          let color = interpolate_color(cell_value, min_value, max_value, config.color_scale.min, config.color_scale.max)
          
          let cell = {
            x: x_index,
            y: y_index,
            x_label: x_value,
            y_label: y_value.to_string(),
            value: cell_value,
            color: color,
            normalized: (cell_value - min_value) / (max_value - min_value)
          }
          
          matrix_cells = matrix_cells.push(cell)
        }
      }
    }
    
    // 生成图表数据结构
    let chart_data = {
      title: config.title,
      type: config.type,
      x_axis: config.x_axis,
      y_axis: config.y_axis,
      data_range: {
        min: min_value,
        max: max_value
      },
      cells: matrix_cells,
      dimensions: {
        width: config.width,
        height: config.height
      },
      styling: {
        color_scale: config.color_scale,
        show_values: config.show_values,
        cell_padding: config.cell_padding
      }
    }
    
    chart_data
  }
  
  // 生成热力图
  let chart = generate_heatmap(matrix_data, chart_config)
  
  // 验证图表结构
  assert_eq(chart.title, "一周内不同时段的CPU使用率")
  assert_eq(chart.type, "heatmap")
  assert_eq(chart.x_axis.categories.length(), 5)
  assert_eq(chart.y_axis.categories.length(), 3)
  assert_eq(chart.data_range.min, 12.0)
  assert_eq(chart.data_range.max, 65.0)
  assert_eq(chart.cells.length(), 15) // 5*3
  assert_eq(chart.dimensions.width, 600)
  assert_eq(chart.dimensions.height, 400)
  
  // 验证特定单元格
  let mut monday_noon_cell = {}
  let mut friday_evening_cell = {}
  
  for cell in chart.cells {
    if cell.x_label == "周一" and cell.y_label == "0" {
      monday_noon_cell = cell
    } else if cell.x_label == "周五" and cell.y_label == "12" {
      friday_evening_cell = cell
    }
  }
  
  // 验证周一0点单元格
  assert_eq(monday_noon_cell.x, 0)
  assert_eq(monday_noon_cell.y, 0)
  assert_eq(monday_noon_cell.value, 15.0)
  assert_eq(monday_noon_cell.normalized, 0.0641025641025641) // (15-12)/(65-12)
  
  // 验证周五12点单元格（最高值）
  assert_eq(friday_evening_cell.x, 4)
  assert_eq(friday_evening_cell.y, 1)
  assert_eq(friday_evening_cell.value, 65.0)
  assert_eq(friday_evening_cell.normalized, 1.0) // (65-12)/(65-12)
  assert_eq(friday_evening_cell.color, "#e74c3c") // 最高值使用深色
}

// 测试5: 仪表盘生成
test "仪表盘生成" {
  // 模拟实时遥测数据
  let real_time_data = {
    cpu_usage: 75.5,
    memory_usage: 68.2,
    disk_usage: 45.8,
    network_io: 125.6,
    active_connections: 342,
    error_rate: 0.8,
    response_time: 145.3,
    uptime: 98.5
  }
  
  // 仪表盘配置
  let dashboard_config = {
    title: "系统监控仪表盘",
    layout: "grid",
    columns: 3,
    widgets: [
      {
        type: "gauge",
        title: "CPU使用率",
        metric: "cpu_usage",
        unit: "%",
        min: 0,
        max: 100,
        thresholds: [
          { value: 70, color: "#2ecc71" },
          { value: 90, color: "#f39c12" },
          { value: 100, color: "#e74c3c" }
        ]
      },
      {
        type: "gauge",
        title: "内存使用率",
        metric: "memory_usage",
        unit: "%",
        min: 0,
        max: 100,
        thresholds: [
          { value: 70, color: "#2ecc71" },
          { value: 90, color: "#f39c12" },
          { value: 100, color: "#e74c3c" }
        ]
      },
      {
        type: "gauge",
        title: "磁盘使用率",
        metric: "disk_usage",
        unit: "%",
        min: 0,
        max: 100,
        thresholds: [
          { value: 70, color: "#2ecc71" },
          { value: 90, color: "#f39c12" },
          { value: 100, color: "#e74c3c" }
        ]
      },
      {
        type: "metric",
        title: "网络I/O",
        metric: "network_io",
        unit: "MB/s",
        format: "number"
      },
      {
        type: "metric",
        title: "活跃连接",
        metric: "active_connections",
        unit: "",
        format: "integer"
      },
      {
        type: "metric",
        title: "错误率",
        metric: "error_rate",
        unit: "%",
        format: "percentage"
      }
    ]
  }
  
  // 获取阈值颜色函数
  let get_threshold_color = fn(value, thresholds) {
    let mut color = "#2ecc71" // 默认颜色
    
    for threshold in thresholds {
      if value >= threshold.value {
        color = threshold.color
      }
    }
    
    color
  }
  
  // 仪表盘生成函数
  let generate_dashboard = fn(data, config) {
    let mut widgets = []
    
    for widget_config in config.widgets {
      let widget_data = match widget_config.type {
        "gauge" => {
          let value = data[widget_config.metric]
          let color = get_threshold_color(value, widget_config.thresholds)
          
          {
            type: "gauge",
            title: widget_config.title,
            value: value,
            unit: widget_config.unit,
            min: widget_config.min,
            max: widget_config.max,
            color: color,
            percentage: (value / widget_config.max) * 100.0
          }
        }
        "metric" => {
          let value = data[widget_config.metric]
          let formatted_value = match widget_config.format {
            "number" => value.to_string()
            "integer" => value.to_int().to_string()
            "percentage" => value.to_string() + "%"
            _ => value.to_string()
          }
          
          {
            type: "metric",
            title: widget_config.title,
            value: value,
            unit: widget_config.unit,
            formatted_value: formatted_value
          }
        }
        _ => {
          {
            type: "unknown",
            title: widget_config.title,
            value: 0
          }
        }
      }
      
      widgets = widgets.push(widget_data)
    }
    
    // 生成仪表盘数据结构
    let dashboard_data = {
      title: config.title,
      layout: config.layout,
      columns: config.columns,
      widgets: widgets,
      last_updated: 1640995620
    }
    
    dashboard_data
  }
  
  // 生成仪表盘
  let dashboard = generate_dashboard(real_time_data, dashboard_config)
  
  // 验证仪表盘结构
  assert_eq(dashboard.title, "系统监控仪表盘")
  assert_eq(dashboard.layout, "grid")
  assert_eq(dashboard.columns, 3)
  assert_eq(dashboard.widgets.length(), 6)
  assert_eq(dashboard.last_updated, 1640995620)
  
  // 验证CPU仪表盘
  let cpu_widget = dashboard.widgets[0]
  assert_eq(cpu_widget.type, "gauge")
  assert_eq(cpu_widget.title, "CPU使用率")
  assert_eq(cpu_widget.value, 75.5)
  assert_eq(cpu_widget.unit, "%")
  assert_eq(cpu_widget.min, 0)
  assert_eq(cpu_widget.max, 100)
  assert_eq(cpu_widget.color, "#f39c12") // 75.5 > 70，使用警告颜色
  assert_eq(cpu_widget.percentage, 75.5)
  
  // 验证内存仪表盘
  let memory_widget = dashboard.widgets[1]
  assert_eq(memory_widget.type, "gauge")
  assert_eq(memory_widget.title, "内存使用率")
  assert_eq(memory_widget.value, 68.2)
  assert_eq(memory_widget.color, "#2ecc71") // 68.2 < 70，使用正常颜色
  
  // 验证网络I/O指标
  let network_widget = dashboard.widgets[3]
  assert_eq(network_widget.type, "metric")
  assert_eq(network_widget.title, "网络I/O")
  assert_eq(network_widget.value, 125.6)
  assert_eq(network_widget.unit, "MB/s")
  assert_eq(network_widget.formatted_value, "125.6")
  
  // 验证活跃连接指标
  let connections_widget = dashboard.widgets[4]
  assert_eq(connections_widget.type, "metric")
  assert_eq(connections_widget.title, "活跃连接")
  assert_eq(connections_widget.value, 342)
  assert_eq(connections_widget.unit, "")
  assert_eq(connections_widget.formatted_value, "342")
  
  // 验证错误率指标
  let error_rate_widget = dashboard.widgets[5]
  assert_eq(error_rate_widget.type, "metric")
  assert_eq(error_rate_widget.title, "错误率")
  assert_eq(error_rate_widget.value, 0.8)
  assert_eq(error_rate_widget.unit, "%")
  assert_eq(error_rate_widget.formatted_value, "0.8%")
}

// 测试6: 图表数据导出
test "图表数据导出" {
  // 模拟图表数据
  let chart_data = {
    title: "系统性能指标",
    type: "line",
    x_axis: {
      label: "时间",
      data: [1640995200, 1640995260, 1640995320, 1640995380, 1640995440],
      format: "datetime"
    },
    y_axis: {
      label: "使用率 (%)",
      min: 0,
      max: 100
    },
    series: [
      {
        name: "CPU使用率",
        data: [45.0, 50.0, 48.0, 55.0, 52.0]
      },
      {
        name: "内存使用率",
        data: [65.0, 68.0, 70.0, 72.0, 75.0]
      }
    ]
  }
  
  // JSON导出函数
  let export_to_json = fn(data) {
    // 简化的JSON序列化
    let json = "{\n"
    json = json + "  \"title\": \"" + data.title + "\",\n"
    json = json + "  \"type\": \"" + data.type + "\",\n"
    json = json + "  \"x_axis\": {\n"
    json = json + "    \"label\": \"" + data.x_axis.label + "\",\n"
    json = json + "    \"data_points\": " + data.x_axis.data.length().to_string() + "\n"
    json = json + "  },\n"
    json = json + "  \"y_axis\": {\n"
    json = json + "    \"label\": \"" + data.y_axis.label + "\",\n"
    json = json + "    \"min\": " + data.y_axis.min.to_string() + ",\n"
    json = json + "    \"max\": " + data.y_axis.max.to_string() + "\n"
    json = json + "  },\n"
    json = json + "  \"series_count\": " + data.series.length().to_string() + "\n"
    json = json + "}"
    
    json
  }
  
  // CSV导出函数
  let export_to_csv = fn(data) {
    let mut csv = "时间,CPU使用率,内存使用率\n"
    
    for i in range(0, data.x_axis.data.length()) {
      csv = csv + data.x_axis.data[i].to_string()
      
      for series in data.series {
        csv = csv + "," + series.data[i].to_string()
      }
      
      if i < data.x_axis.data.length() - 1 {
        csv = csv + "\n"
      }
    }
    
    csv
  }
  
  // 图像导出函数（模拟）
  let export_to_image = fn(data, format) {
    let image_data = {
      format: format,
      width: 800,
      height: 400,
      title: data.title,
      data_points: data.x_axis.data.length(),
      series_count: data.series.length(),
      file_size: if format == "png" { 24576 } else { 18432 } // 模拟文件大小
    }
    
    image_data
  }
  
  // 执行导出
  let json_export = export_to_json(chart_data)
  let csv_export = export_to_csv(chart_data)
  let png_export = export_to_image(chart_data, "png")
  let jpg_export = export_to_image(chart_data, "jpg")
  
  // 验证JSON导出
  assert_true(json_export.contains("\"title\": \"系统性能指标\""))
  assert_true(json_export.contains("\"type\": \"line\""))
  assert_true(json_export.contains("\"data_points\": 5"))
  assert_true(json_export.contains("\"series_count\": 2"))
  
  // 验证CSV导出
  assert_true(csv_export.contains("时间,CPU使用率,内存使用率"))
  assert_true(csv_export.contains("1640995200,45.0,65.0"))
  assert_true(csv_export.contains("1640995440,52.0,75.0"))
  
  // 验证PNG导出
  assert_eq(png_export.format, "png")
  assert_eq(png_export.width, 800)
  assert_eq(png_export.height, 400)
  assert_eq(png_export.title, "系统性能指标")
  assert_eq(png_export.data_points, 5)
  assert_eq(png_export.series_count, 2)
  assert_eq(png_export.file_size, 24576)
  
  // 验证JPG导出
  assert_eq(jpg_export.format, "jpg")
  assert_eq(jpg_export.width, 800)
  assert_eq(jpg_export.height, 400)
  assert_eq(jpg_export.file_size, 18432)
  
  // 验证不同格式的文件大小差异
  assert_true(png_export.file_size > jpg_export.file_size)
  
  // 导出配置验证
  let export_configs = [
    { format: "json", mime_type: "application/json", file_extension: ".json" },
    { format: "csv", mime_type: "text/csv", file_extension: ".csv" },
    { format: "png", mime_type: "image/png", file_extension: ".png" },
    { format: "jpg", mime_type: "image/jpeg", file_extension: ".jpg" }
  ]
  
  // 验证导出配置
  assert_eq(export_configs.length(), 4)
  assert_eq(export_configs[0].format, "json")
  assert_eq(export_configs[0].mime_type, "application/json")
  assert_eq(export_configs[0].file_extension, ".json")
  assert_eq(export_configs[2].format, "png")
  assert_eq(export_configs[2].mime_type, "image/png")
  assert_eq(export_configs[2].file_extension, ".png")
}