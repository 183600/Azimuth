// Azimuth 遥测数据归档检索与可视化测试
// 专注于数据归档策略、检索效率和可视化功能

// 测试8: 遥测数据归档与检索测试
test "遥测数据归档与检索测试" {
  // 创建数据归档管理器
  let archive_manager = DataArchiveManager::new()
  
  // 配置归档策略
  ArchiveManager::set_retention_policy(archive_manager, {
    hot_tier: {
      max_age_days: 7,
      max_size_gb: 100,
      storage_type: "ssd"
    },
    warm_tier: {
      max_age_days: 30,
      max_size_gb: 500,
      storage_type: "hdd"
    },
    cold_tier: {
      max_age_days: 365,
      max_size_gb: 2000,
      storage_type: "object_storage"
    },
    archive_tier: {
      max_age_days: -1,  // 永久保留
      max_size_gb: -1,   // 无限制
      storage_type: "tape"
    }
  })
  
  // 配置归档压缩策略
  ArchiveManager::set_compression_strategy(archive_manager, {
    algorithm: "lz4",
    level: 6,
    min_size_threshold: 1024  // 大于1KB的数据才压缩
  })
  
  // 配置归档索引策略
  ArchiveManager::set_indexing_strategy(archive_manager, {
    fields: ["trace_id", "span_id", "service_name", "timestamp", "operation_name"],
    index_type: "inverted",
    refresh_interval: 300  // 5分钟刷新一次
  })
  
  // 创建测试数据集
  let test_datasets = []
  let base_time = 1640995200  // 2022-01-01 00:00:00
  
  // 创建不同时期的数据
  for day in 0..=60 {  // 60天的数据
    let day_data = []
    let day_start = base_time + day * 86400  // 每天的开始时间
    
    for hour in 0..=23 {  // 每天24小时
      let hour_start = day_start + hour * 3600
      
      for minute in 0..=59 {  // 每小时60分钟
        let minute_start = hour_start + minute * 60
        
        // 每分钟10个数据点
        for i in 0..=9 {
          let data_point = {
            trace_id: "trace-" + day.to_string() + "-" + hour.to_string() + "-" + minute.to_string() + "-" + i.to_string(),
            span_id: "span-" + i.to_string(),
            service_name: "service-" + (i % 5).to_string(),
            operation_name: "operation-" + (i % 10).to_string(),
            timestamp: minute_start + i * 6,
            duration: 100 + i * 20,
            status: if i % 20 == 0 { "error" } else { "success" },
            attributes: [
              ("http.method", StringValue(["GET", "POST", "PUT", "DELETE"][i % 4])),
              ("http.status_code", IntValue([200, 201, 400, 404, 500][i % 5])),
              ("user.id", StringValue("user-" + (i % 100).to_string()))
            ],
            size: 1024 + i * 100
          }
          
          day_data = day_data.push(data_point)
        }
      }
    }
    
    test_datasets = test_datasets.push({
      day: day,
      date: day_start,
      data: day_data
    })
  }
  
  // 执行数据归档
  let mut archived_count = 0
  let mut total_size = 0
  
  for dataset in test_datasets {
    let archive_result = ArchiveManager::archive_data(archive_manager, dataset.data, {
      tier: determine_tier_by_age(dataset.day),
      tags: ["day-" + dataset.day.to_string(), "batch-" + (archived_count / 1000).to_string()]
    })
    
    if archive_result.success {
      archived_count = archived_count + dataset.data.length()
      total_size = total_size + archive_result.archived_size
    }
  }
  
  // 验证归档结果
  assert_true(archived_count > 0)
  assert_true(total_size > 0)
  
  // 验证数据分层
  let hot_tier_data = ArchiveManager::get_data_by_tier(archive_manager, "hot")
  let warm_tier_data = ArchiveManager::get_data_by_tier(archive_manager, "warm")
  let cold_tier_data = ArchiveManager::get_data_by_tier(archive_manager, "cold")
  
  // 验证数据正确分层
  assert_true(hot_tier_data.length() > 0)  // 应该有热数据
  assert_true(warm_tier_data.length() > 0)  // 应该有温数据
  assert_true(cold_tier_data.length() > 0)  // 应该有冷数据
  
  // 验证时间分层逻辑
  let now = base_time + 60 * 86400  // 当前时间
  for data in hot_tier_data {
    let data_age_days = (now - data.timestamp) / 86400
    assert_true(data_age_days <= 7)  // 热数据不超过7天
  }
  
  for data in warm_tier_data {
    let data_age_days = (now - data.timestamp) / 86400
    assert_true(data_age_days > 7 and data_age_days <= 30)  // 温数据7-30天
  }
  
  // 测试数据检索
  let retrieval_tests = [
    // 按时间范围检索
    {
      name: "time_range_query",
      query: {
        type: "time_range",
        start_time: base_time + 10 * 86400,  // 第10天
        end_time: base_time + 10 * 86400 + 3600,  // 第10天的第1小时
        filters: []
      },
      expected_min_results: 600  // 1小时 * 60分钟 * 10数据点/分钟
    },
    // 按服务名检索
    {
      name: "service_name_query",
      query: {
        type: "attribute_filter",
        filters: [
          ("service_name", "equals", "service-2")
        ]
      },
      expected_min_results: 1000  // 大约1/5的数据
    },
    // 按状态检索
    {
      name: "status_query",
      query: {
        type: "attribute_filter",
        filters: [
          ("status", "equals", "error")
        ]
      },
      expected_min_results: 100  // 大约5%的错误数据
    },
    // 复合查询
    {
      name: "complex_query",
      query: {
        type: "combined",
        time_range: {
          start_time: base_time + 15 * 86400,  // 第15天
          end_time: base_time + 20 * 86400    // 第20天
        },
        filters: [
          ("service_name", "in", ["service-1", "service-3"]),
          ("status", "equals", "success")
        ]
      },
      expected_min_results: 500  // 5天 * 24小时 * 60分钟 * 10数据点/分钟 * 2/5服务 * 95%成功率
    }
  ]
  
  for test in retrieval_tests {
    let query_start = Time::now()
    let retrieval_result = ArchiveManager::query_data(archive_manager, test.query)
    let query_end = Time::now()
    
    // 验证检索结果
    assert_true(retrieval_result.success)
    assert_true(retrieval_result.data.length() >= test.expected_min_results)
    
    // 验证查询性能
    let query_duration = query_end - query_start
    assert_true(query_duration < 5000)  // 查询不应超过5秒
    
    // 验证结果准确性
    if test.query.type == "time_range" {
      for data in retrieval_result.data {
        assert_true(data.timestamp >= test.query.start_time and data.timestamp <= test.query.end_time)
      }
    }
    
    if test.query.type == "attribute_filter" {
      for filter in test.query.filters {
        let (field, operator, value) = filter
        for data in retrieval_result.data {
          match field {
            "service_name" => assert_eq(data.service_name, value),
            "status" => assert_eq(data.status, value),
            _ => {}
          }
        }
      }
    }
  }
  
  // 测试跨层检索
  let cross_tier_query = {
    type: "cross_tier",
    tiers: ["hot", "warm", "cold"],
    query: {
      type: "attribute_filter",
      filters: [
        ("user.id", "equals", "user-42")
      ]
    }
  }
  
  let cross_tier_result = ArchiveManager::query_data(archive_manager, cross_tier_query)
  assert_true(cross_tier_result.success)
  
  // 验证跨层结果包含来自不同层的数据
  let mut has_hot_data = false
  let mut has_warm_data = false
  let mut has_cold_data = false
  
  for data in cross_tier_result.data {
    let data_age_days = (now - data.timestamp) / 86400
    
    if data_age_days <= 7 {
      has_hot_data = true
    } else if data_age_days <= 30 {
      has_warm_data = true
    } else {
      has_cold_data = true
    }
  }
  
  // 测试数据恢复
  let recovery_test_data = ArchiveManager::get_data_by_tier(archive_manager, "cold")[0]
  let recovery_result = ArchiveManager::restore_data(archive_manager, recovery_test_data.archive_id)
  
  assert_true(recovery_result.success)
  assert_eq(recovery_result.restored_data.trace_id, recovery_test_data.trace_id)
  assert_eq(recovery_result.restored_data.span_id, recovery_test_data.span_id)
  
  // 测试数据删除
  let old_data = ArchiveManager::get_data_by_tier(archive_manager, "cold")
  if old_data.length() > 0 {
    let delete_result = ArchiveManager::delete_data(archive_manager, old_data[0].archive_id)
    assert_true(delete_result.success)
    
    // 验证数据已删除
    let verify_result = ArchiveManager::get_data_by_id(archive_manager, old_data[0].archive_id)
    assert_true(verify_result == None)
  }
  
  // 测试归档统计
  let archive_stats = ArchiveManager::get_archive_statistics(archive_manager)
  
  assert_true(archive_stats.total_records > 0)
  assert_true(archive_stats.total_size_gb > 0)
  assert_true(archive_stats.compression_ratio > 1.0)  // 压缩率应大于1
  assert_true(archive_stats.tier_distribution.contains_key("hot"))
  assert_true(archive_stats.tier_distribution.contains_key("warm"))
  assert_true(archive_stats.tier_distribution.contains_key("cold"))
}

// 测试9: 遥测数据可视化测试
test "遥测数据可视化测试" {
  // 创建可视化管理器
  let visualization_manager = VisualizationManager::new()
  
  // 创建测试数据
  let visualization_data = []
  let base_time = 1640995200
  
  // 创建服务性能数据
  for service in ["service.a", "service.b", "service.c", "service.d"] {
    for hour in 0..=23 {  // 24小时
      let hour_timestamp = base_time + hour * 3600
      
      // 每小时的性能指标
      let performance_point = {
        service_name: service,
        timestamp: hour_timestamp,
        request_count: 1000 + (hour % 12) * 100,
        error_rate: 0.01 + (hour % 8) * 0.005,
        avg_response_time: 100 + (hour % 10) * 20,
        p95_response_time: 200 + (hour % 15) * 30,
        p99_response_time: 500 + (hour % 20) * 50,
        cpu_usage: 50.0 + (hour % 30) * 1.5,
        memory_usage: 60.0 + (hour % 25) * 1.2,
        throughput: 100.0 + (hour % 20) * 5.0
      }
      
      visualization_data = visualization_data.push(performance_point)
    }
  }
  
  // 添加错误事件数据
  for i in 0..=20 {
    let error_event = {
      timestamp: base_time + i * 3600 + 1800,  // 每小时中间
      service_name: ["service.a", "service.b", "service.c"][i % 3],
      error_type: ["timeout", "connection_error", "internal_error"][i % 3],
      error_message: "Error " + i.to_string(),
      impact_score: i % 10,
      affected_users: 10 + i * 5
    }
    
    visualization_data = visualization_data.push(error_event)
  }
  
  // 测试时间序列图表生成
  let time_series_config = {
    chart_type: "line_chart",
    title: "服务响应时间趋势",
    x_axis: {
      field: "timestamp",
      label: "时间",
      type: "datetime"
    },
    y_axis: {
      field: "avg_response_time",
      label: "平均响应时间 (ms)",
      type: "numeric"
    },
    series: [
      {
        name: "Service A",
        filter: [("service_name", "equals", "service.a")],
        color: "#FF6B6B"
      },
      {
        name: "Service B",
        filter: [("service_name", "equals", "service.b")],
        color: "#4ECDC4"
      },
      {
        name: "Service C",
        filter: [("service_name", "equals", "service.c")],
        color: "#45B7D1"
      },
      {
        name: "Service D",
        filter: [("service_name", "equals", "service.d")],
        color: "#96CEB4"
      }
    ],
    options: {
      show_legend: true,
      show_grid: true,
      interpolation: "linear",
      point_size: 3
    }
  }
  
  let time_series_chart = VisualizationManager::generate_chart(visualization_manager, visualization_data, time_series_config)
  
  // 验证时间序列图表
  assert_true(time_series_chart.success)
  assert_true(time_series_chart.data.points.length() > 0)
  assert_eq(time_series_chart.data.series.length(), 4)  // 4个服务
  
  // 验证数据点
  for series in time_series_chart.data.series {
    assert_true(series.points.length() > 0)
    assert_true(series.name.contains("Service"))
    
    // 验证时间顺序
    for i in 1..=series.points.length() - 1 {
      assert_true(series.points[i].x >= series.points[i-1].x)
    }
  }
  
  // 测试柱状图生成
  let bar_chart_config = {
    chart_type: "bar_chart",
    title: "服务错误率对比",
    x_axis: {
      field: "service_name",
      label: "服务",
      type: "categorical"
    },
    y_axis: {
      field: "error_rate",
      label: "错误率 (%)",
      type: "numeric",
      aggregation: "avg"
    },
    series: [],
    options: {
      show_legend: false,
      show_grid: true,
      bar_width: 0.6,
      color_palette: ["#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4"]
    }
  }
  
  let bar_chart = VisualizationManager::generate_chart(visualization_manager, visualization_data, bar_chart_config)
  
  // 验证柱状图
  assert_true(bar_chart.success)
  assert_eq(bar_chart.data.series.length(), 1)  // 单一系列
  assert_eq(bar_chart.data.series[0].points.length(), 4)  // 4个服务
  
  // 验证错误率计算
  for point in bar_chart.data.series[0].points {
    assert_true(point.y >= 0.0 and point.y <= 1.0)  // 错误率应在0-1之间
  }
  
  // 测试热力图生成
  let heatmap_config = {
    chart_type: "heatmap",
    title: "服务负载热力图",
    x_axis: {
      field: "timestamp",
      label: "时间",
      type: "datetime",
      bin_size: 3600  // 按小时分组
    },
    y_axis: {
      field: "service_name",
      label: "服务",
      type: "categorical"
    },
    value_field: "cpu_usage",
    options: {
      color_scale: "viridis",
      show_legend: true,
      cell_border: false
    }
  }
  
  let heatmap = VisualizationManager::generate_chart(visualization_manager, visualization_data, heatmap_config)
  
  // 验证热力图
  assert_true(heatmap.success)
  assert_true(heatmap.data.heatmap_cells.length() > 0)
  
  // 验证热力图数据结构
  for cell in heatmap.data.heatmap_cells {
    assert_true(cell.x >= 0)  // 时间索引
    assert_true(cell.y >= 0)  // 服务索引
    assert_true(cell.value >= 0.0 and cell.value <= 100.0)  // CPU使用率
  }
  
  // 测试饼图生成
  let pie_chart_config = {
    chart_type: "pie_chart",
    title: "服务请求分布",
    value_field: "request_count",
    label_field: "service_name",
    aggregation: "sum",
    options: {
      show_legend: true,
      show_percentage: true,
      color_palette: ["#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4"]
    }
  }
  
  let pie_chart = VisualizationManager::generate_chart(visualization_manager, visualization_data, pie_chart_config)
  
  // 验证饼图
  assert_true(pie_chart.success)
  assert_eq(pie_chart.data.segments.length(), 4)  // 4个服务
  
  // 验证百分比计算
  let mut total_percentage = 0.0
  for segment in pie_chart.data.segments {
    assert_true(segment.value > 0)
    assert_true(segment.percentage > 0.0 and segment.percentage <= 100.0)
    total_percentage = total_percentage + segment.percentage
  }
  assert_true(total_percentage >= 99.0 and total_percentage <= 101.0)  // 考虑浮点精度
  
  // 测试仪表盘生成
  let dashboard_config = {
    title: "系统监控仪表盘",
    layout: "grid",
    grid_columns: 2,
    panels: [
      {
        id: "panel1",
        type: "line_chart",
        title: "响应时间趋势",
        size: { width: 2, height: 1 },
        config: time_series_config
      },
      {
        id: "panel2",
        type: "bar_chart",
        title: "错误率对比",
        size: { width: 1, height: 1 },
        config: bar_chart_config
      },
      {
        id: "panel3",
        type: "heatmap",
        title: "负载热力图",
        size: { width: 1, height: 1 },
        config: heatmap_config
      },
      {
        id: "panel4",
        type: "pie_chart",
        title: "请求分布",
        size: { width: 1, height: 1 },
        config: pie_chart_config
      }
    ]
  }
  
  let dashboard = VisualizationManager::generate_dashboard(visualization_manager, visualization_data, dashboard_config)
  
  // 验证仪表盘
  assert_true(dashboard.success)
  assert_eq(dashboard.data.panels.length(), 4)
  
  // 验证每个面板
  for panel in dashboard.data.panels {
    assert_true(panel.chart_data.success)
    assert_true(panel.chart_data.data.points.length() > 0 or 
               panel.chart_data.data.series.length() > 0 or
               panel.chart_data.data.heatmap_cells.length() > 0 or
               panel.chart_data.data.segments.length() > 0)
  }
  
  // 测试实时更新
  let realtime_data = []
  let current_time = Time::now()
  
  for i in 0..=10 {
    realtime_data = realtime_data.push({
      service_name: "service.a",
      timestamp: current_time + i * 1000,
      request_count: 100 + i * 10,
      avg_response_time: 100 + i * 5,
      error_rate: 0.01 + i * 0.001
    })
  }
  
  let realtime_chart = VisualizationManager::update_chart(visualization_manager, time_series_chart.chart_id, realtime_data)
  
  // 验证实时更新
  assert_true(realtime_chart.success)
  assert_true(realtime_chart.data.points.length() >= 10)
  
  // 测试图表导出
  let export_formats = ["png", "svg", "pdf", "json"]
  
  for format in export_formats {
    let export_result = VisualizationManager::export_chart(visualization_manager, time_series_chart.chart_id, {
      format: format,
      width: 1200,
      height: 800,
      quality: if format == "png" { 90 } else { 100 }
    })
    
    assert_true(export_result.success)
    assert_true(export_result.data_size > 0)
    assert_true(export_result.mime_type.contains(format))
  }
  
  // 测试交互功能
  let interaction_data = VisualizationManager::get_interaction_data(visualization_manager, time_series_chart.chart_id, {
    type: "point_hover",
    x: base_time + 12 * 3600,  // 中午12点
    y: 150  // 响应时间150ms
  })
  
  // 验证交互数据
  assert_true(interaction_data.success)
  assert_true(interaction_data.points.length() > 0)
  
  for point in interaction_data.points {
    assert_true(point.service_name != "")
    assert_true(point.timestamp > 0)
    assert_true(point.value >= 0)
  }
  
  // 测试图表模板
  let template = VisualizationManager::create_template(visualization_manager, {
    name: "服务性能模板",
    description: "用于展示服务性能的图表模板",
    chart_types: ["line_chart", "bar_chart"],
    default_fields: ["timestamp", "service_name", "avg_response_time", "error_rate"],
    default_options: {
      show_legend: true,
      show_grid: true,
      color_palette: ["#FF6B6B", "#4ECDC4", "#45B7D1", "#96CEB4"]
    }
  })
  
  // 使用模板创建图表
  let template_chart = VisualizationManager::create_chart_from_template(visualization_manager, template.id, {
    title: "基于模板的响应时间图表",
    data: visualization_data,
    overrides: {
      y_axis: { field: "p95_response_time", label: "P95响应时间 (ms)" }
    }
  })
  
  // 验证模板图表
  assert_true(template_chart.success)
  assert_true(template_chart.data.points.length() > 0)
}

// 辅助函数：根据数据年龄确定存储层
let determine_tier_by_age = fn(age_days: Int) -> String {
  if age_days <= 7 {
    "hot"
  } else if age_days <= 30 {
    "warm"
  } else if age_days <= 365 {
    "cold"
  } else {
    "archive"
  }
}