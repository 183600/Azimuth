// Azimuth 遥测数据可视化测试
// 专注于遥测数据的可视化组件、图表生成和仪表板功能

// 测试1: 时间序列图表生成
test "时间序列图表生成" {
  // 模拟时间序列数据
  let time_series_data = [
    { timestamp: 1640995200, value: 45.0, metric: "cpu_usage" },
    { timestamp: 1640995260, value: 50.0, metric: "cpu_usage" },
    { timestamp: 1640995320, value: 55.0, metric: "cpu_usage" },
    { timestamp: 1640995380, value: 48.0, metric: "cpu_usage" },
    { timestamp: 1640995440, value: 52.0, metric: "cpu_usage" },
    { timestamp: 1640995500, value: 60.0, metric: "cpu_usage" },
    { timestamp: 1640995560, value: 58.0, metric: "cpu_usage" },
    { timestamp: 1640995620, value: 65.0, metric: "cpu_usage" },
    { timestamp: 1640995680, value: 70.0, metric: "cpu_usage" },
    { timestamp: 1640995740, value: 68.0, metric: "cpu_usage" }
  ]
  
  // 模拟图表配置
  let chart_config = {
    type: "line_chart",
    title: "CPU Usage Over Time",
    x_axis: {
      label: "Time",
      type: "datetime",
      format: "HH:mm"
    },
    y_axis: {
      label: "CPU Usage (%)",
      min: 0,
      max: 100
    },
    series: [
      {
        name: "CPU Usage",
        data: time_series_data,
        color: "#3366CC",
        line_style: "solid",
        marker_style: "circle"
      }
    ],
    grid: {
      show: true,
      color: "#E0E0E0"
    },
    legend: {
      show: true,
      position: "top"
    }
  }
  
  // 模拟生成的图表数据结构
  let generated_chart = {
    chart_id: "chart-001",
    type: "line_chart",
    title: "CPU Usage Over Time",
    width: 800,
    height: 400,
    data_points: [
      { x: 1640995200, y: 45.0, formatted_x: "08:00", formatted_y: "45%" },
      { x: 1640995260, y: 50.0, formatted_x: "08:01", formatted_y: "50%" },
      { x: 1640995320, y: 55.0, formatted_x: "08:02", formatted_y: "55%" },
      { x: 1640995380, y: 48.0, formatted_x: "08:03", formatted_y: "48%" },
      { x: 1640995440, y: 52.0, formatted_x: "08:04", formatted_y: "52%" },
      { x: 1640995500, y: 60.0, formatted_x: "08:05", formatted_y: "60%" },
      { x: 1640995560, y: 58.0, formatted_x: "08:06", formatted_y: "58%" },
      { x: 1640995620, y: 65.0, formatted_x: "08:07", formatted_y: "65%" },
      { x: 1640995680, y: 70.0, formatted_x: "08:08", formatted_y: "70%" },
      { x: 1640995740, y: 68.0, formatted_x: "08:09", formatted_y: "68%" }
    ],
    axes: {
      x: {
        min: 1640995200,
        max: 1640995740,
        ticks: [
          { value: 1640995200, label: "08:00" },
          { value: 1640995440, label: "08:04" },
          { value: 1640995680, label: "08:08" }
        ]
      },
      y: {
        min: 0,
        max: 100,
        ticks: [
          { value: 0, label: "0%" },
          { value: 25, label: "25%" },
          { value: 50, label: "50%" },
          { value: 75, label: "75%" },
          { value: 100, label: "100%" }
        ]
      }
    },
    series: [
      {
        name: "CPU Usage",
        color: "#3366CC",
        data: [
          { x: 1640995200, y: 45.0 },
          { x: 1640995260, y: 50.0 },
          { x: 1640995320, y: 55.0 },
          { x: 1640995380, y: 48.0 },
          { x: 1640995440, y: 52.0 },
          { x: 1640995500, y: 60.0 },
          { x: 1640995560, y: 58.0 },
          { x: 1640995620, y: 65.0 },
          { x: 1640995680, y: 70.0 },
          { x: 1640995740, y: 68.0 }
        ]
      }
    ]
  }
  
  // 验证图表生成结果
  assert_eq(generated_chart.type, "line_chart")
  assert_eq(generated_chart.title, "CPU Usage Over Time")
  assert_eq(generated_chart.width, 800)
  assert_eq(generated_chart.height, 400)
  assert_eq(generated_chart.data_points.length(), 10)
  
  // 验证数据点格式化
  for point in generated_chart.data_points {
    assert_true(point.formatted_x.starts_with("08:"))
    assert_true(point.formatted_y.ends_with("%"))
    assert_true(point.y >= 0.0 && point.y <= 100.0)
  }
  
  // 验证坐标轴配置
  assert_eq(generated_chart.axes.x.min, 1640995200)
  assert_eq(generated_chart.axes.x.max, 1640995740)
  assert_eq(generated_chart.axes.y.min, 0)
  assert_eq(generated_chart.axes.y.max, 100)
  
  // 验证时间轴刻度
  assert_eq(generated_chart.axes.x.ticks.length(), 3)
  for tick in generated_chart.axes.x.ticks {
    assert_true(tick.label.starts_with("08:"))
  }
  
  // 验证数值轴刻度
  assert_eq(generated_chart.axes.y.ticks.length(), 5)
  for i in 0..generated_chart.axes.y.ticks.length() {
    let tick = generated_chart.axes.y.ticks[i]
    assert_eq(tick.value, i * 25)
    assert_eq(tick.label, (i * 25).to_string() + "%")
  }
  
  // 验证系列数据
  assert_eq(generated_chart.series.length(), 1)
  let series = generated_chart.series[0]
  assert_eq(series.name, "CPU Usage")
  assert_eq(series.color, "#3366CC")
  assert_eq(series.data.length(), 10)
  
  // 验证数据点与原始数据的一致性
  for i in 0..time_series_data.length() {
    assert_eq(series.data[i].x, time_series_data[i].timestamp)
    assert_eq(series.data[i].y, time_series_data[i].value)
  }
  
  // 验证图表统计信息
  let chart_statistics = {
    data_point_count: generated_chart.data_points.length(),
    min_value: generated_chart.data_points.reduce(fn(acc, p) { if p.y < acc { p.y } else { acc } }, generated_chart.data_points[0].y),
    max_value: generated_chart.data_points.reduce(fn(acc, p) { if p.y > acc { p.y } else { acc } }, generated_chart.data_points[0].y),
    avg_value: generated_chart.data_points.reduce(fn(acc, p) { acc + p.y }, 0.0) / generated_chart.data_points.length().to_float(),
    trend: "increasing"
  }
  
  assert_eq(chart_statistics.data_point_count, 10)
  assert_eq(chart_statistics.min_value, 45.0)
  assert_eq(chart_statistics.max_value, 70.0)
  assert_eq(chart_statistics.avg_value, 57.1) // 四舍五入
  assert_eq(chart_statistics.trend, "increasing")
}

// 测试2: 多系列图表生成
test "多系列图表生成" {
  // 模拟多系列时间序列数据
  let multi_series_data = [
    {
      metric: "cpu_usage",
      data: [
        { timestamp: 1640995200, value: 45.0 },
        { timestamp: 1640995260, value: 50.0 },
        { timestamp: 1640995320, value: 55.0 },
        { timestamp: 1640995380, value: 48.0 },
        { timestamp: 1640995440, value: 52.0 }
      ]
    },
    {
      metric: "memory_usage",
      data: [
        { timestamp: 1640995200, value: 60.0 },
        { timestamp: 1640995260, value: 62.0 },
        { timestamp: 1640995320, value: 65.0 },
        { timestamp: 1640995380, value: 58.0 },
        { timestamp: 1640995440, value: 61.0 }
      ]
    },
    {
      metric: "disk_usage",
      data: [
        { timestamp: 1640995200, value: 75.0 },
        { timestamp: 1640995260, value: 76.0 },
        { timestamp: 1640995320, value: 77.0 },
        { timestamp: 1640995380, value: 78.0 },
        { timestamp: 1640995440, value: 79.0 }
      ]
    }
  ]
  
  // 模拟多系列图表配置
  let multi_chart_config = {
    type: "multi_line_chart",
    title: "System Resource Usage",
    x_axis: {
      label: "Time",
      type: "datetime",
      format: "HH:mm"
    },
    y_axis: {
      label: "Usage (%)",
      min: 0,
      max: 100
    },
    series: [
      { name: "CPU Usage", data: multi_series_data[0].data, color: "#3366CC" },
      { name: "Memory Usage", data: multi_series_data[1].data, color: "#DC3912" },
      { name: "Disk Usage", data: multi_series_data[2].data, color: "#FF9900" }
    ],
    legend: {
      show: true,
      position: "right"
    }
  }
  
  // 模拟生成的多系列图表
  let generated_multi_chart = {
    chart_id: "chart-002",
    type: "multi_line_chart",
    title: "System Resource Usage",
    width: 800,
    height: 400,
    series: [
      {
        name: "CPU Usage",
        color: "#3366CC",
        data: [
          { x: 1640995200, y: 45.0 },
          { x: 1640995260, y: 50.0 },
          { x: 1640995320, y: 55.0 },
          { x: 1640995380, y: 48.0 },
          { x: 1640995440, y: 52.0 }
        ]
      },
      {
        name: "Memory Usage",
        color: "#DC3912",
        data: [
          { x: 1640995200, y: 60.0 },
          { x: 1640995260, y: 62.0 },
          { x: 1640995320, y: 65.0 },
          { x: 1640995380, y: 58.0 },
          { x: 1640995440, y: 61.0 }
        ]
      },
      {
        name: "Disk Usage",
        color: "#FF9900",
        data: [
          { x: 1640995200, y: 75.0 },
          { x: 1640995260, y: 76.0 },
          { x: 1640995320, y: 77.0 },
          { x: 1640995380, y: 78.0 },
          { x: 1640995440, y: 79.0 }
        ]
      }
    ],
    legend: {
      items: [
        { name: "CPU Usage", color: "#3366CC", visible: true },
        { name: "Memory Usage", color: "#DC3912", visible: true },
        { name: "Disk Usage", color: "#FF9900", visible: true }
      ]
    }
  }
  
  // 验证多系列图表生成
  assert_eq(generated_multi_chart.type, "multi_line_chart")
  assert_eq(generated_multi_chart.title, "System Resource Usage")
  assert_eq(generated_multi_chart.series.length(), 3)
  
  // 验证每个系列的数据完整性
  for i in 0..multi_series_data.length() {
    let series = generated_multi_chart.series[i]
    let original_data = multi_series_data[i]
    
    assert_eq(series.name, original_data.metric)
    assert_eq(series.data.length(), original_data.data.length())
    
    for j in 0..original_data.data.length() {
      assert_eq(series.data[j].x, original_data.data[j].timestamp)
      assert_eq(series.data[j].y, original_data.data[j].value)
    }
  }
  
  // 验证颜色分配
  assert_eq(generated_multi_chart.series[0].color, "#3366CC")
  assert_eq(generated_multi_chart.series[1].color, "#DC3912")
  assert_eq(generated_multi_chart.series[2].color, "#FF9900")
  
  // 验证图例
  assert_eq(generated_multi_chart.legend.items.length(), 3)
  for i in 0..generated_multi_chart.legend.items.length() {
    let legend_item = generated_multi_chart.legend.items[i]
    let series = generated_multi_chart.series[i]
    assert_eq(legend_item.name, series.name)
    assert_eq(legend_item.color, series.color)
    assert_true(legend_item.visible)
  }
  
  // 验证系列统计
  let series_statistics = []
  for series in generated_multi_chart.series {
    let values = series.data.map(fn(d) { d.y })
    let min_value = values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, values[0])
    let max_value = values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, values[0])
    let avg_value = values.reduce(fn(acc, v) { acc + v }, 0.0) / values.length().to_float()
    
    series_statistics = series_statistics.push({
      name: series.name,
      min_value: min_value,
      max_value: max_value,
      avg_value: avg_value,
      data_points: values.length()
    })
  }
  
  // 验证CPU使用率统计
  let cpu_stats = series_statistics.filter_fn(s => s.name == "CPU Usage")[0]
  assert_eq(cpu_stats.min_value, 45.0)
  assert_eq(cpu_stats.max_value, 55.0)
  assert_eq(cpu_stats.avg_value, 50.0) // (45+50+55+48+52)/5
  assert_eq(cpu_stats.data_points, 5)
  
  // 验证内存使用率统计
  let memory_stats = series_statistics.filter_fn(s => s.name == "Memory Usage")[0]
  assert_eq(memory_stats.min_value, 58.0)
  assert_eq(memory_stats.max_value, 65.0)
  assert_eq(memory_stats.avg_value, 61.2) // (60+62+65+58+61)/5
  assert_eq(memory_stats.data_points, 5)
  
  // 验证磁盘使用率统计
  let disk_stats = series_statistics.filter_fn(s => s.name == "Disk Usage")[0]
  assert_eq(disk_stats.min_value, 75.0)
  assert_eq(disk_stats.max_value, 79.0)
  assert_eq(disk_stats.avg_value, 77.0) // (75+76+77+78+79)/5
  assert_eq(disk_stats.data_points, 5)
}

// 测试3: 热力图生成
test "热力图生成" {
  // 模拟热力图数据
  let heatmap_data = [
    { x: "server-001", y: "cpu", value: 75.0 },
    { x: "server-001", y: "memory", value: 60.0 },
    { x: "server-001", y: "disk", value: 45.0 },
    { x: "server-002", y: "cpu", value: 80.0 },
    { x: "server-002", y: "memory", value: 65.0 },
    { x: "server-002", y: "disk", value: 50.0 },
    { x: "server-003", y: "cpu", value: 70.0 },
    { x: "server-003", y: "memory", value: 55.0 },
    { x: "server-003", y: "disk", value: 40.0 }
  ]
  
  // 模拟热力图配置
  let heatmap_config = {
    type: "heatmap",
    title: "Server Resource Usage Heatmap",
    x_axis: {
      label: "Server",
      categories: ["server-001", "server-002", "server-003"]
    },
    y_axis: {
      label: "Resource",
      categories: ["cpu", "memory", "disk"]
    },
    color_scale: {
      min: 0,
      max: 100,
      colors: ["#313695", "#4575b4", "#74add1", "#abd9e9", "#e0f3f8", "#ffffcc", "#fee090", "#fdae61", "#f46d43", "#d73027", "#a50026"]
    },
    legend: {
      show: true,
      position: "right"
    }
  }
  
  // 模拟生成的热力图
  let generated_heatmap = {
    chart_id: "chart-003",
    type: "heatmap",
    title: "Server Resource Usage Heatmap",
    width: 600,
    height: 400,
    grid: {
      rows: 3,
      columns: 3,
      cells: [
        { x: 0, y: 0, value: 75.0, color: "#fdae61", label: "server-001, cpu: 75%" },
        { x: 1, y: 0, value: 80.0, color: "#f46d43", label: "server-002, cpu: 80%" },
        { x: 2, y: 0, value: 70.0, color: "#fee090", label: "server-003, cpu: 70%" },
        { x: 0, y: 1, value: 60.0, color: "#abd9e9", label: "server-001, memory: 60%" },
        { x: 1, y: 1, value: 65.0, color: "#74add1", label: "server-002, memory: 65%" },
        { x: 2, y: 1, value: 55.0, color: "#e0f3f8", label: "server-003, memory: 55%" },
        { x: 0, y: 2, value: 45.0, color: "#4575b4", label: "server-001, disk: 45%" },
        { x: 1, y: 2, value: 50.0, color: "#74add1", label: "server-002, disk: 50%" },
        { x: 2, y: 2, value: 40.0, color: "#313695", label: "server-003, disk: 40%" }
      ]
    },
    axes: {
      x: {
        categories: ["server-001", "server-002", "server-003"],
        labels: ["Server 1", "Server 2", "Server 3"]
      },
      y: {
        categories: ["cpu", "memory", "disk"],
        labels: ["CPU", "Memory", "Disk"]
      }
    },
    color_scale: {
      min: 0,
      max: 100,
      stops: [
        { value: 0, color: "#313695" },
        { value: 25, color: "#4575b4" },
        { value: 50, color: "#74add1" },
        { value: 75, color: "#fdae61" },
        { value: 100, color: "#a50026" }
      ]
    }
  }
  
  // 验证热力图生成
  assert_eq(generated_heatmap.type, "heatmap")
  assert_eq(generated_heatmap.title, "Server Resource Usage Heatmap")
  assert_eq(generated_heatmap.grid.rows, 3)
  assert_eq(generated_heatmap.grid.columns, 3)
  assert_eq(generated_heatmap.grid.cells.length(), 9)
  
  // 验证网格单元格
  for cell in generated_heatmap.grid.cells {
    assert_true(cell.value >= 0.0 && cell.value <= 100.0)
    assert_true(cell.label.contains("server-"))
    assert_true(cell.label.contains(":"))
    assert_true(cell.label.ends_with("%"))
  }
  
  // 验证坐标轴
  assert_eq(generated_heatmap.axes.x.categories.length(), 3)
  assert_eq(generated_heatmap.axes.y.categories.length(), 3)
  
  // 验证颜色映射
  let color_mapping = [
    { value: 40.0, expected_color: "#313695" },
    { value: 45.0, expected_color: "#4575b4" },
    { value: 50.0, expected_color: "#74add1" },
    { value: 55.0, expected_color: "#e0f3f8" },
    { value: 60.0, expected_color: "#abd9e9" },
    { value: 65.0, expected_color: "#74add1" },
    { value: 70.0, expected_color: "#fee090" },
    { value: 75.0, expected_color: "#fdae61" },
    { value: 80.0, expected_color: "#f46d43" }
  ]
  
  for mapping in color_mapping {
    let matching_cells = generated_heatmap.grid.cells.filter_fn(cell => cell.value == mapping.value)
    assert_true(matching_cells.length() > 0)
    for cell in matching_cells {
      assert_eq(cell.color, mapping.expected_color)
    }
  }
  
  // 验证热力图统计
  let heatmap_statistics = {
    total_cells: generated_heatmap.grid.cells.length(),
    min_value: generated_heatmap.grid.cells.reduce(fn(acc, cell) { if cell.value < acc { cell.value } else { acc } }, generated_heatmap.grid.cells[0].value),
    max_value: generated_heatmap.grid.cells.reduce(fn(acc, cell) { if cell.value > acc { cell.value } else { acc } }, generated_heatmap.grid.cells[0].value),
    avg_value: generated_heatmap.grid.cells.reduce(fn(acc, cell) { acc + cell.value }, 0.0) / generated_heatmap.grid.cells.length().to_float()
  }
  
  assert_eq(heatmap_statistics.total_cells, 9)
  assert_eq(heatmap_statistics.min_value, 40.0)
  assert_eq(heatmap_statistics.max_value, 80.0)
  assert_eq(heatmap_statistics.avg_value, 60.0) // (75+60+45+80+65+50+70+55+40)/9
  
  // 验证热力图模式识别
  let patterns = []
  
  // 按服务器分组分析
  let servers = ["server-001", "server-002", "server-003"]
  for server in servers {
    let server_cells = generated_heatmap.grid.cells.filter_fn(cell => 
      cell.label.contains(server)
    )
    let server_avg = server_cells.reduce(fn(acc, cell) { acc + cell.value }, 0.0) / server_cells.length().to_float()
    
    patterns = patterns.push({
      entity: server,
      type: "server",
      avg_usage: server_avg,
      resource_distribution: server_cells.map(fn(cell) { 
        { resource: cell.label.split(", ")[1].split(":")[0], value: cell.value }
      })
    })
  }
  
  // 验证服务器-001的模式
  let server_001_pattern = patterns.filter_fn(p => p.entity == "server-001")[0]
  assert_eq(server_001_pattern.avg_usage, 60.0) // (75+60+45)/3
  
  // 验证服务器-002的模式
  let server_002_pattern = patterns.filter_fn(p => p.entity == "server-002")[0]
  assert_eq(server_002_pattern.avg_usage, 65.0) // (80+65+50)/3
  
  // 验证服务器-003的模式
  let server_003_pattern = patterns.filter_fn(p => p.entity == "server-003")[0]
  assert_eq(server_003_pattern.avg_usage, 55.0) // (70+55+40)/3
}

// 测试4: 仪表盘生成
test "仪表盘生成" {
  // 模拟仪表盘配置
  let dashboard_config = {
    id: "dashboard-001",
    title: "System Monitoring Dashboard",
    layout: {
      type: "grid",
      columns: 3,
      rows: 2,
      gap: 10
    },
    widgets: [
      {
        id: "widget-001",
        type: "metric_card",
        title: "CPU Usage",
        position: { row: 0, column: 0, width: 1, height: 1 },
        config: {
          metric: "cpu_usage",
          value: 75.5,
          unit: "%",
          threshold: { warning: 70, critical: 90 }
        }
      },
      {
        id: "widget-002",
        type: "metric_card",
        title: "Memory Usage",
        position: { row: 0, column: 1, width: 1, height: 1 },
        config: {
          metric: "memory_usage",
          value: 62.3,
          unit: "%",
          threshold: { warning: 75, critical: 90 }
        }
      },
      {
        id: "widget-003",
        type: "metric_card",
        title: "Disk Usage",
        position: { row: 0, column: 2, width: 1, height: 1 },
        config: {
          metric: "disk_usage",
          value: 45.8,
          unit: "%",
          threshold: { warning: 80, critical: 95 }
        }
      },
      {
        id: "widget-004",
        type: "line_chart",
        title: "CPU Usage Trend",
        position: { row: 1, column: 0, width: 2, height: 1 },
        config: {
          time_range: "1h",
          refresh_interval: 60
        }
      },
      {
        id: "widget-005",
        type: "heatmap",
        title: "Server Status",
        position: { row: 1, column: 2, width: 1, height: 1 },
        config: {
          refresh_interval: 120
        }
      }
    ]
  }
  
  // 模拟生成的仪表盘
  let generated_dashboard = {
    id: "dashboard-001",
    title: "System Monitoring Dashboard",
    width: 1200,
    height: 600,
    layout: {
      type: "grid",
      columns: 3,
      rows: 2,
      gap: 10
    },
    widgets: [
      {
        id: "widget-001",
        type: "metric_card",
        title: "CPU Usage",
        x: 0,
        y: 0,
        width: 390,
        height: 290,
        content: {
          value: 75.5,
          unit: "%",
          status: "warning", // 75.5 > 70 (warning threshold)
          trend: "up",
          change: "+2.3%"
        }
      },
      {
        id: "widget-002",
        type: "metric_card",
        title: "Memory Usage",
        x: 400,
        y: 0,
        width: 390,
        height: 290,
        content: {
          value: 62.3,
          unit: "%",
          status: "normal", // 62.3 < 75 (warning threshold)
          trend: "down",
          change: "-1.2%"
        }
      },
      {
        id: "widget-003",
        type: "metric_card",
        title: "Disk Usage",
        x: 800,
        y: 0,
        width: 390,
        height: 290,
        content: {
          value: 45.8,
          unit: "%",
          status: "normal", // 45.8 < 80 (warning threshold)
          trend: "stable",
          change: "0.0%"
        }
      },
      {
        id: "widget-004",
        type: "line_chart",
        title: "CPU Usage Trend",
        x: 0,
        y: 300,
        width: 790,
        height: 290,
        content: {
          chart_id: "chart-004",
          data_points: 60,
          last_updated: 1640995800
        }
      },
      {
        id: "widget-005",
        type: "heatmap",
        title: "Server Status",
        x: 800,
        y: 300,
        width: 390,
        height: 290,
        content: {
          chart_id: "chart-005",
          last_updated: 1640995800
        }
      }
    ],
    last_updated: 1640995800
  }
  
  // 验证仪表盘生成
  assert_eq(generated_dashboard.id, "dashboard-001")
  assert_eq(generated_dashboard.title, "System Monitoring Dashboard")
  assert_eq(generated_dashboard.widgets.length(), 5)
  
  // 验证布局
  assert_eq(generated_dashboard.layout.type, "grid")
  assert_eq(generated_dashboard.layout.columns, 3)
  assert_eq(generated_dashboard.layout.rows, 2)
  
  // 验证指标卡片
  let metric_cards = generated_dashboard.widgets.filter_fn(w => w.type == "metric_card")
  assert_eq(metric_cards.length(), 3)
  
  // 验证CPU使用率卡片
  let cpu_card = metric_cards.filter_fn(w => w.title == "CPU Usage")[0]
  assert_eq(cpu_card.content.value, 75.5)
  assert_eq(cpu_card.content.unit, "%")
  assert_eq(cpu_card.content.status, "warning") // 75.5 > 70
  assert_eq(cpu_card.content.trend, "up")
  assert_eq(cpu_card.content.change, "+2.3%")
  
  // 验证内存使用率卡片
  let memory_card = metric_cards.filter_fn(w => w.title == "Memory Usage")[0]
  assert_eq(memory_card.content.value, 62.3)
  assert_eq(memory_card.content.unit, "%")
  assert_eq(memory_card.content.status, "normal") // 62.3 < 75
  assert_eq(memory_card.content.trend, "down")
  assert_eq(memory_card.content.change, "-1.2%")
  
  // 验证磁盘使用率卡片
  let disk_card = metric_cards.filter_fn(w => w.title == "Disk Usage")[0]
  assert_eq(disk_card.content.value, 45.8)
  assert_eq(disk_card.content.unit, "%")
  assert_eq(disk_card.content.status, "normal") // 45.8 < 80
  assert_eq(disk_card.content.trend, "stable")
  assert_eq(disk_card.content.change, "0.0%")
  
  // 验证图表组件
  let chart_widgets = generated_dashboard.widgets.filter_fn(w => w.type == "line_chart" || w.type == "heatmap")
  assert_eq(chart_widgets.length(), 2)
  
  // 验证CPU趋势图表
  let cpu_trend_chart = chart_widgets.filter_fn(w => w.title == "CPU Usage Trend")[0]
  assert_eq(cpu_trend_chart.type, "line_chart")
  assert_eq(cpu_trend_chart.content.data_points, 60)
  assert_eq(cpu_trend_chart.content.last_updated, 1640995800)
  
  // 验证服务器状态热力图
  let server_status_heatmap = chart_widgets.filter_fn(w => w.title == "Server Status")[0]
  assert_eq(server_status_heatmap.type, "heatmap")
  assert_eq(server_status_heatmap.content.last_updated, 1640995800)
  
  // 验证仪表盘统计
  let dashboard_statistics = {
    total_widgets: generated_dashboard.widgets.length(),
    metric_cards: metric_cards.length(),
    chart_widgets: chart_widgets.length(),
    warning_count: metric_cards.filter_fn(w => w.content.status == "warning").length(),
    critical_count: metric_cards.filter_fn(w => w.content.status == "critical").length(),
    normal_count: metric_cards.filter_fn(w => w.content.status == "normal").length()
  }
  
  assert_eq(dashboard_statistics.total_widgets, 5)
  assert_eq(dashboard_statistics.metric_cards, 3)
  assert_eq(dashboard_statistics.chart_widgets, 2)
  assert_eq(dashboard_statistics.warning_count, 1) // CPU使用率
  assert_eq(dashboard_statistics.critical_count, 0)
  assert_eq(dashboard_statistics.normal_count, 2) // 内存和磁盘使用率
  
  // 验证仪表盘响应式布局
  let responsive_layout = {
    desktop: {
      width: 1200,
      columns: 3,
      rows: 2
    },
    tablet: {
      width: 768,
      columns: 2,
      rows: 3
    },
    mobile: {
      width: 375,
      columns: 1,
      rows: 5
    }
  }
  
  // 验证桌面布局
  assert_eq(responsive_layout.desktop.width, 1200)
  assert_eq(responsive_layout.desktop.columns, 3)
  assert_eq(responsive_layout.desktop.rows, 2)
  
  // 验证平板布局
  assert_eq(responsive_layout.tablet.width, 768)
  assert_eq(responsive_layout.tablet.columns, 2)
  assert_eq(responsive_layout.tablet.rows, 3)
  
  // 验证手机布局
  assert_eq(responsive_layout.mobile.width, 375)
  assert_eq(responsive_layout.mobile.columns, 1)
  assert_eq(responsive_layout.mobile.rows, 5)
}

// 测试5: 实时数据可视化
test "实时数据可视化" {
  // 模拟实时数据流
  let real_time_data_stream = [
    { timestamp: 1640995200, metric: "cpu_usage", value: 45.0 },
    { timestamp: 1640995260, metric: "cpu_usage", value: 50.0 },
    { timestamp: 1640995320, metric: "cpu_usage", value: 55.0 },
    { timestamp: 1640995380, metric: "cpu_usage", value: 48.0 },
    { timestamp: 1640995440, metric: "cpu_usage", value: 52.0 },
    { timestamp: 1640995500, metric: "cpu_usage", value: 60.0 },
    { timestamp: 1640995560, metric: "cpu_usage", value: 58.0 },
    { timestamp: 1640995620, metric: "cpu_usage", value: 65.0 },
    { timestamp: 1640995680, metric: "cpu_usage", value: 70.0 },
    { timestamp: 1640995740, metric: "cpu_usage", value: 68.0 }
  ]
  
  // 模拟实时图表配置
  let real_time_chart_config = {
    type: "real_time_line_chart",
    title: "Real-time CPU Usage",
    max_data_points: 10,
    update_interval: 1000, // 1秒
    animation: {
      enabled: true,
      duration: 300
    },
    auto_scroll: {
      enabled: true,
      window_size: 10
    }
  }
  
  // 模拟实时图表更新事件
  let real_time_updates = [
    {
      timestamp: 1640995800,
      event_type: "data_update",
      data: { metric: "cpu_usage", value: 72.0 },
      chart_state: {
        data_points: 10,
        visible_range: { start: 1640995740, end: 1640995800 },
        animation_in_progress: false
      }
    },
    {
      timestamp: 1640995860,
      event_type: "data_update",
      data: { metric: "cpu_usage", value: 75.0 },
      chart_state: {
        data_points: 10,
        visible_range: { start: 1640995800, end: 1640995860 },
        animation_in_progress: true
      }
    },
    {
      timestamp: 1640995920,
      event_type: "data_update",
      data: { metric: "cpu_usage", value: 78.0 },
      chart_state: {
        data_points: 10,
        visible_range: { start: 1640995860, end: 1640995920 },
        animation_in_progress: false
      }
    }
  ]
  
  // 验证实时数据流
  assert_eq(real_time_data_stream.length(), 10)
  for i in 1..real_time_data_stream.length() {
    assert_true(real_time_data_stream[i].timestamp > real_time_data_stream[i-1].timestamp)
  }
  
  // 验证实时图表配置
  assert_eq(real_time_chart_config.type, "real_time_line_chart")
  assert_eq(real_time_chart_config.max_data_points, 10)
  assert_eq(real_time_chart_config.update_interval, 1000)
  assert_true(real_time_chart_config.animation.enabled)
  assert_eq(real_time_chart_config.animation.duration, 300)
  assert_true(real_time_chart_config.auto_scroll.enabled)
  assert_eq(real_time_chart_config.auto_scroll.window_size, 10)
  
  // 验证实时更新事件
  assert_eq(real_time_updates.length(), 3)
  
  for update in real_time_updates {
    assert_eq(update.event_type, "data_update")
    assert_true(update.data.metric == "cpu_usage")
    assert_true(update.data.value > 0.0)
    assert_eq(update.chart_state.data_points, 10)
    assert_true(update.chart_state.visible_range.end > update.chart_state.visible_range.start)
  }
  
  // 验证动画状态
  let animation_events = real_time_updates.filter_fn(u => u.chart_state.animation_in_progress)
  assert_eq(animation_events.length(), 1)
  
  // 验证数据点滚动
  let initial_range = real_time_updates[0].chart_state.visible_range
  let final_range = real_time_updates[real_time_updates.length()-1].chart_state.visible_range
  
  assert_true(final_range.start > initial_range.start)
  assert_true(final_range.end > initial_range.end)
  
  // 验证实时图表性能
  let real_time_performance = {
    update_frequency: 1000, // ms
    data_points_per_update: 1,
    max_data_points: 10,
    average_render_time: 15, // ms
    memory_usage_mb: 12.5,
    cpu_usage_percent: 5.2
  }
  
  assert_eq(real_time_performance.update_frequency, 1000)
  assert_eq(real_time_performance.data_points_per_update, 1)
  assert_eq(real_time_performance.max_data_points, 10)
  assert_true(real_time_performance.average_render_time < 50) // 渲染时间应该小于更新间隔的5%
  assert_true(real_time_performance.memory_usage_mb < 50)
  assert_true(real_time_performance.cpu_usage_percent < 10)
  
  // 验证实时数据异常检测
  let anomaly_detection = {
    threshold_type: "statistical",
    method: "z_score",
    threshold: 2.0,
    detected_anomalies: [
      {
        timestamp: 1640995920,
        value: 78.0,
        z_score: 2.3,
        severity: "medium"
      }
    ]
  }
  
  assert_eq(anomaly_detection.threshold_type, "statistical")
  assert_eq(anomaly_detection.method, "z_score")
  assert_eq(anomaly_detection.threshold, 2.0)
  assert_eq(anomaly_detection.detected_anomalies.length(), 1)
  
  let anomaly = anomaly_detection.detected_anomalies[0]
  assert_eq(anomaly.timestamp, 1640995920)
  assert_eq(anomaly.value, 78.0)
  assert_true(anomaly.z_score > anomaly_detection.threshold)
  assert_eq(anomaly.severity, "medium")
  
  // 验证实时告警
  let alerting_system = {
    enabled: true,
    rules: [
      {
        name: "high_cpu_usage",
        condition: "value > 80",
        severity: "warning",
        cooldown: 300 // seconds
      },
      {
        name: "critical_cpu_usage",
        condition: "value > 90",
        severity: "critical",
        cooldown: 60 // seconds
      }
    ],
    triggered_alerts: []
  }
  
  // 模拟告警触发
  let high_cpu_alert = {
    timestamp: 1640995920,
    rule_name: "high_cpu_usage",
    metric: "cpu_usage",
    value: 85.0,
    severity: "warning",
    message: "CPU usage exceeded 80% threshold"
  }
  
  alerting_system.triggered_alerts = alerting_system.triggered_alerts.push(high_cpu_alert)
  
  assert_true(alerting_system.enabled)
  assert_eq(alerting_system.rules.length(), 2)
  assert_eq(alerting_system.triggered_alerts.length(), 1)
  
  let triggered_alert = alerting_system.triggered_alerts[0]
  assert_eq(triggered_alert.rule_name, "high_cpu_usage")
  assert_eq(triggered_alert.metric, "cpu_usage")
  assert_eq(triggered_alert.value, 85.0)
  assert_eq(triggered_alert.severity, "warning")
}

// 测试6: 交互式可视化
test "交互式可视化" {
  // 模拟交互式图表配置
  let interactive_chart_config = {
    type: "interactive_line_chart",
    title: "Interactive System Metrics",
    interactions: {
      zoom: {
        enabled: true,
        min_scale: 0.5,
        max_scale: 5.0
      },
      pan: {
        enabled: true,
        bounds: "auto"
      },
      tooltip: {
        enabled: true,
        format: "detailed",
        show_on: "hover"
      },
      selection: {
        enabled: true,
        mode: "range"
      },
      crosshair: {
        enabled: true,
        snap_to_data: true
      }
    }
  }
  
  // 模拟用户交互事件
  let user_interactions = [
    {
      timestamp: 1640995800,
      event_type: "zoom",
      details: {
        scale: 2.0,
        center: { x: 1640995500, y: 50.0 },
        range: { start: 1640995400, end: 1640995600 }
      }
    },
    {
      timestamp: 1640995810,
      event_type: "pan",
      details: {
        direction: "right",
        distance: 100,
        new_range: { start: 1640995450, end: 1640995650 }
      }
    },
    {
      timestamp: 1640995820,
      event_type: "tooltip_show",
      details: {
        position: { x: 1640995520, y: 55.0 },
        data_point: { timestamp: 1640995520, metric: "cpu_usage", value: 55.0 },
        content: {
          title: "CPU Usage",
          value: "55%",
          timestamp: "2022-01-01 08:02:00",
          additional_info: "Server: server-001"
        }
      }
    },
    {
      timestamp: 1640995830,
      event_type: "selection_start",
      details: {
        start: { x: 1640995480, y: 0 },
        mode: "range"
      }
    },
    {
      timestamp: 1640995840,
      event_type: "selection_end",
      details: {
        end: { x: 1640995580, y: 100 },
        selected_range: { start: 1640995480, end: 1640995580 },
        selected_data: [
          { timestamp: 1640995480, metric: "cpu_usage", value: 52.0 },
          { timestamp: 1640995540, metric: "cpu_usage", value: 60.0 },
          { timestamp: 1640995580, metric: "cpu_usage", value: 58.0 }
        ]
      }
    }
  ]
  
  // 验证交互式图表配置
  assert_eq(interactive_chart_config.type, "interactive_line_chart")
  assert_true(interactive_chart_config.interactions.zoom.enabled)
  assert_eq(interactive_chart_config.interactions.zoom.min_scale, 0.5)
  assert_eq(interactive_chart_config.interactions.zoom.max_scale, 5.0)
  assert_true(interactive_chart_config.interactions.pan.enabled)
  assert_true(interactive_chart_config.interactions.tooltip.enabled)
  assert_eq(interactive_chart_config.interactions.tooltip.format, "detailed")
  assert_true(interactive_chart_config.interactions.selection.enabled)
  assert_eq(interactive_chart_config.interactions.selection.mode, "range")
  assert_true(interactive_chart_config.interactions.crosshair.enabled)
  
  // 验证用户交互事件
  assert_eq(user_interactions.length(), 5)
  
  // 验证缩放事件
  let zoom_event = user_interactions.filter_fn(e => e.event_type == "zoom")[0]
  assert_eq(zoom_event.details.scale, 2.0)
  assert_true(zoom_event.details.scale >= interactive_chart_config.interactions.zoom.min_scale)
  assert_true(zoom_event.details.scale <= interactive_chart_config.interactions.zoom.max_scale)
  
  // 验证平移事件
  let pan_event = user_interactions.filter_fn(e => e.event_type == "pan")[0]
  assert_eq(pan_event.details.direction, "right")
  assert_eq(pan_event.details.distance, 100)
  
  // 验证工具提示事件
  let tooltip_event = user_interactions.filter_fn(e => e.event_type == "tooltip_show")[0]
  assert_eq(tooltip_event.details.data_point.value, 55.0)
  assert_eq(tooltip_event.details.content.title, "CPU Usage")
  assert_eq(tooltip_event.details.content.value, "55%")
  
  // 验证选择事件
  let selection_start_event = user_interactions.filter_fn(e => e.event_type == "selection_start")[0]
  let selection_end_event = user_interactions.filter_fn(e => e.event_type == "selection_end")[0]
  
  assert_eq(selection_start_event.details.mode, "range")
  assert_eq(selection_end_event.details.selected_range.start, 1640995480)
  assert_eq(selection_end_event.details.selected_range.end, 1640995580)
  assert_eq(selection_end_event.details.selected_data.length(), 3)
  
  // 验证交互性能
  let interaction_performance = {
    zoom_response_time: 50,  // ms
    pan_response_time: 30,   // ms
    tooltip_show_delay: 100, // ms
    selection_render_time: 80, // ms
    max_interactions_per_second: 10
  }
  
  assert_true(interaction_performance.zoom_response_time < 100)
  assert_true(interaction_performance.pan_response_time < 100)
  assert_true(interaction_performance.tooltip_show_delay < 200)
  assert_true(interaction_performance.selection_render_time < 150)
  assert_true(interaction_performance.max_interactions_per_second >= 5)
  
  // 验证交互状态管理
  let interaction_state = {
    current_zoom: 2.0,
    current_pan: { x: 50, y: 0 },
    selected_range: { start: 1640995480, end: 1640995580 },
    active_tooltip: {
      visible: true,
      position: { x: 1640995520, y: 55.0 }
    },
    crosshair_position: { x: 1640995520, y: 55.0 }
  }
  
  assert_eq(interaction_state.current_zoom, 2.0)
  assert_eq(interaction_state.current_pan.x, 50)
  assert_eq(interaction_state.selected_range.start, 1640995480)
  assert_eq(interaction_state.selected_range.end, 1640995580)
  assert_true(interaction_state.active_tooltip.visible)
  assert_eq(interaction_state.crosshair_position.x, 1640995520)
  
  // 验证数据导出功能
  let export_options = {
    formats: ["png", "svg", "pdf", "csv", "json"],
    quality: {
      png: 300, // dpi
      svg: "vector",
      pdf: 300   // dpi
    },
    data_selection: "current_view" // "current_view" or "all_data"
  }
  
  assert_eq(export_options.formats.length(), 5)
  assert_eq(export_options.quality.png, 300)
  assert_eq(export_options.quality.svg, "vector")
  assert_eq(export_options.data_selection, "current_view")
  
  // 模拟导出事件
  let export_event = {
    timestamp: 1640995850,
    format: "png",
    quality: 300,
    selection: "current_view",
    filename: "chart_export_20220101_080850.png",
    file_size_kb: 245
  }
  
  assert_eq(export_event.format, "png")
  assert_eq(export_event.quality, 300)
  assert_eq(export_event.selection, "current_view")
  assert_true(export_event.filename.starts_with("chart_export_"))
  assert_true(export_event.file_size_kb > 0)
}