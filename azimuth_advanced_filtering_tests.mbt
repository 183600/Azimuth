// Azimuth 高级过滤测试用例
// 测试高级数据过滤和查询功能

// 测试1: 多维度数据过滤
test "多维度数据过滤" {
  // 创建测试数据集
  let data_points = []
  let base_time = 1735689600000000000L  // 2025年基准时间戳
  
  // 生成多维度测试数据
  for i in 0..1000 {
    let timestamp = base_time + (i * 60000000L)  // 每分钟一个数据点
    
    let data_point = DataPoint({
      timestamp: timestamp,
      metric_name: match i % 4 {
        0 => "cpu_usage",
        1 => "memory_usage",
        2 => "disk_usage",
        _ => "network_usage"
      },
      value: 20.0 + (Random::next() * 80.0),
      attributes: [
        ("host", "server-" + ((i % 10) + 1).to_string()),
        ("region", match i % 3 {
          0 => "us-west-1",
          1 => "us-east-1",
          _ => "eu-west-1"
        }),
        ("environment", match i % 2 {
          0 => "production",
          _ => "staging"
        }),
        ("service", match i % 5 {
          0 => "api",
          1 => "web",
          2 => "database",
          3 => "cache",
          _ => "queue"
        })
      ]
    })
    
    data_points.push(data_point)
  }
  
  // 创建过滤器
  let filter_engine = FilterEngine()
  
  // 测试单维度过滤
  let cpu_filter = AttributeFilter("metric_name", "cpu_usage")
  let cpu_data = filter_engine.filter(data_points, cpu_filter)
  
  // 验证CPU使用率数据
  assert_eq(cpu_data.length(), 250)  // 1000 / 4
  for point in cpu_data {
    assert_eq(point.metric_name, "cpu_usage")
  }
  
  // 测试多值过滤
  let resource_filters = OrFilter([
    AttributeFilter("metric_name", "cpu_usage"),
    AttributeFilter("metric_name", "memory_usage")
  ])
  let resource_data = filter_engine.filter(data_points, resource_filters)
  
  // 验证资源使用数据
  assert_eq(resource_data.length(), 500)  // 1000 / 2
  for point in resource_data {
    assert_true(point.metric_name == "cpu_usage" || point.metric_name == "memory_usage")
  }
  
  // 测试范围过滤
  let high_usage_filter = ValueRangeFilter("value", 70.0, 100.0)
  let high_usage_data = filter_engine.filter(data_points, high_usage_filter)
  
  // 验证高使用率数据
  assert_true(high_usage_data.length() > 0)
  for point in high_usage_data {
    assert_true(point.value >= 70.0)
    assert_true(point.value <= 100.0)
  }
  
  // 测试复合过滤
  let production_cpu_filter = AndFilter([
    AttributeFilter("metric_name", "cpu_usage"),
    AttributeFilter("environment", "production"),
    ValueRangeFilter("value", 80.0, 100.0)
  ])
  let production_cpu_data = filter_engine.filter(data_points, production_cpu_filter)
  
  // 验证生产环境高CPU使用率数据
  for point in production_cpu_data {
    assert_eq(point.metric_name, "cpu_usage")
    assert_eq(point.get_attribute("environment"), Some("production"))
    assert_true(point.value >= 80.0)
    assert_true(point.value <= 100.0)
  }
  
  // 测试时间范围过滤
  let time_range_filter = TimeRangeFilter(
    base_time + 60000000L * 100,  // 从第100个数据点开始
    base_time + 60000000L * 200   // 到第200个数据点结束
  )
  let time_filtered_data = filter_engine.filter(data_points, time_range_filter)
  
  // 验证时间范围过滤
  assert_eq(time_filtered_data.length(), 101)  // 包含边界点
  for point in time_filtered_data {
    assert_true(point.timestamp >= base_time + 60000000L * 100)
    assert_true(point.timestamp <= base_time + 60000000L * 200)
  }
  
  // 测试正则表达式过滤
  let regex_filter = RegexFilter("host", "server-[1-3]")
  let regex_filtered_data = filter_engine.filter(data_points, regex_filter)
  
  // 验证正则表达式过滤
  for point in regex_filtered_data {
    let host = point.get_attribute("host")
    match host {
      Some(h) => {
        assert_true(h == "server-1" || h == "server-2" || h == "server-3")
      }
      None => assert_true(false)
    }
  }
  
  // 测试自定义过滤函数
  let custom_filter = CustomFilter(fn(data_point) {
    let host = data_point.get_attribute("host")
    let region = data_point.get_attribute("region")
    let value = data_point.value
    
    match (host, region) {
      (Some(h), Some(r)) => {
        // 服务器1-3在us-west-1区域且值大于50
        (h.contains("server-1") || h.contains("server-2") || h.contains("server-3")) &&
        r == "us-west-1" &&
        value > 50.0
      }
      _ => false
    }
  })
  
  let custom_filtered_data = filter_engine.filter(data_points, custom_filter)
  
  // 验证自定义过滤
  for point in custom_filtered_data {
    let host = point.get_attribute("host")
    let region = point.get_attribute("region")
    
    match (host, region) {
      (Some(h), Some(r)) => {
        assert_true(h.contains("server-1") || h.contains("server-2") || h.contains("server-3"))
        assert_eq(r, "us-west-1")
        assert_true(point.value > 50.0)
      }
      _ => assert_true(false)
    }
  }
}

// 测试2: 层级过滤和聚合
test "层级过滤和聚合" {
  // 创建层级数据结构
  let hierarchical_data = []
  
  // 生成组织层级数据
  for region in ["us-west", "us-east", "eu-west", "ap-southeast"] {
    for env in ["production", "staging", "development"] {
      for service in ["api", "web", "database", "cache", "queue"] {
        for instance in 0..5 {
          let data_point = HierarchicalDataPoint({
            timestamp: get_current_timestamp(),
            value: 20.0 + (Random::next() * 80.0),
            hierarchy: [
              ("region", region),
              ("environment", env),
              ("service", service),
              ("instance", service + "-" + instance.to_string())
            ],
            attributes: [
              ("metric_type", "performance"),
              ("unit", "percent")
            ]
          })
          
          hierarchical_data.push(data_point)
        }
      }
    }
  }
  
  // 创建层级过滤器
  let hierarchy_filter = HierarchyFilter()
  
  // 测试按层级过滤
  let us_west_data = hierarchy_filter.filter_by_level(hierarchical_data, "region", "us-west")
  
  // 验证区域过滤
  assert_true(us_west_data.length() > 0)
  for point in us_west_data {
    let region_value = hierarchy_filter.get_hierarchy_value(point, "region")
    match region_value {
      Some(r) => assert_eq(r, "us-west")
      None => assert_true(false)
    }
  }
  
  // 测试多层级过滤
  let prod_api_data = hierarchy_filter.filter_by_multiple_levels(
    hierarchical_data,
    [
      ("environment", "production"),
      ("service", "api")
    ]
  )
  
  // 验证多层级过滤
  for point in prod_api_data {
    let env_value = hierarchy_filter.get_hierarchy_value(point, "environment")
    let service_value = hierarchy_filter.get_hierarchy_value(point, "service")
    
    match (env_value, service_value) {
      (Some(env), Some(service)) => {
        assert_eq(env, "production")
        assert_eq(service, "api")
      }
      _ => assert_true(false)
    }
  }
  
  // 测试层级聚合
  let region_aggregation = hierarchy_filter.aggregate_by_level(
    hierarchical_data,
    "region",
    AggregationFunction("avg")
  )
  
  // 验证区域聚合
  assert_eq(region_aggregation.length(), 4)  // 4个区域
  for agg in region_aggregation {
    assert_eq(agg.level, "region")
    assert_true(agg.value >= 20.0)
    assert_true(agg.value <= 100.0)
    assert_true(agg.count > 0)
  }
  
  // 测试多层级聚合
  let multi_level_aggregation = hierarchy_filter.aggregate_by_multiple_levels(
    hierarchical_data,
    ["region", "environment"],
    AggregationFunction("avg")
  )
  
  // 验证多层级聚合
  assert_eq(multi_level_aggregation.length(), 12)  // 4个区域 * 3个环境
  for agg in multi_level_aggregation {
    assert_eq(agg.levels.length(), 2)
    assert_eq(agg.levels[0], "region")
    assert_eq(agg.levels[1], "environment")
    assert_true(agg.value >= 20.0)
    assert_true(agg.value <= 100.0)
  }
  
  // 测试层级钻取
  let region_drilldown = hierarchy_filter.drill_down(
    hierarchical_data,
    "region",
    "us-west",
    "environment"
  )
  
  // 验证层级钻取
  assert_eq(region_drilldown.length(), 3)  // 3个环境
  for drilldown in region_drilldown {
    assert_eq(drilldown.level, "environment")
    assert_eq(drilldown.parent_level, "region")
    assert_eq(drilldown.parent_value, "us-west")
    assert_true(drilldown.value >= 20.0)
    assert_true(drilldown.value <= 100.0)
  }
  
  // 测试层级比较
  let region_comparison = hierarchy_filter.compare_levels(
    hierarchical_data,
    "region",
    AggregationFunction("avg")
  )
  
  // 验证层级比较
  assert_eq(region_comparison.length(), 4)
  assert_true(region_comparison.contains("us-west"))
  assert_true(region_comparison.contains("us-east"))
  assert_true(region_comparison.contains("eu-west"))
  assert_true(region_comparison.contains("ap-southeast"))
  
  // 测试层级趋势分析
  let time_series_hierarchical_data = []
  let base_time = get_current_timestamp()
  
  // 生成时间序列层级数据
  for i in 0..1440 {  // 24小时的数据，每分钟一个点
    let timestamp = base_time + (i * 60000000L)
    
    for region in ["us-west", "us-east"] {
      let value = 30.0 + (Random::next() * 40.0) + (i % 144 == 0 ? 20.0 : 0.0)  // 每天有峰值
      
      let time_series_point = HierarchicalDataPoint({
        timestamp: timestamp,
        value: value,
        hierarchy: [
          ("region", region),
          ("time", (i / 60).to_string() + "h")  // 按小时分组
        ],
        attributes: [
          ("metric_type", "performance"),
          ("unit", "percent")
        ]
      })
      
      time_series_hierarchical_data.push(time_series_point)
    }
  }
  
  // 分析层级时间趋势
  let region_trends = hierarchy_filter.analyze_temporal_trends(
    time_series_hierarchical_data,
    "region",
    "time",
    AggregationFunction("avg")
  )
  
  // 验证层级趋势分析
  assert_eq(region_trends.length(), 2)  // 2个区域
  
  for trend in region_trends {
    assert_eq(trend.level, "region")
    assert_true(trend.time_series.length() > 0)
    assert_true(trend.trend_direction == "increasing" || 
               trend.trend_direction == "decreasing" || 
               trend.trend_direction == "stable")
  }
}

// 测试3: 高级查询语言
test "高级查询语言" {
  // 创建测试数据
  let test_data = []
  let base_time = get_current_timestamp()
  
  for i in 0..500 {
    let timestamp = base_time + (i * 60000000L)
    
    let data_point = DataPoint({
      timestamp: timestamp,
      metric_name: match i % 5 {
        0 => "cpu_usage",
        1 => "memory_usage",
        2 => "disk_usage",
        3 => "network_in",
        _ => "network_out"
      },
      value: 10.0 + (Random::next() * 90.0),
      attributes: [
        ("host", "server-" + ((i % 10) + 1).to_string()),
        ("region", match i % 3 {
          0 => "us-west-1",
          1 => "us-east-1",
          _ => "eu-west-1"
        }),
        ("environment", match i % 2 {
          0 => "production",
          _ => "staging"
        }),
        ("service", match i % 4 {
          0 => "api",
          1 => "web",
          2 => "database",
          _ => "cache"
        }),
        ("team", match i % 3 {
          0 => "backend",
          1 => "frontend",
          _ => "infrastructure"
        })
      ]
    })
    
    test_data.push(data_point)
  }
  
  // 创建查询解析器
  let query_parser = QueryParser()
  
  // 测试简单查询
  let simple_query = "metric_name = 'cpu_usage'"
  let simple_ast = query_parser.parse(simple_query)
  
  match simple_ast {
    Some(ast) => {
      match ast {
        ComparisonExpression(left, op, right) => {
          assert_eq(left, "metric_name")
          assert_eq(op, "=")
          assert_eq(right, "cpu_usage")
        }
        _ => assert_true(false)
      }
    }
    None => assert_true(false)
  }
  
  // 执行简单查询
  let simple_results = query_parser.execute(test_data, simple_query)
  
  // 验证简单查询结果
  assert_eq(simple_results.length(), 100)  // 500 / 5
  for point in simple_results {
    assert_eq(point.metric_name, "cpu_usage")
  }
  
  // 测试复合查询
  let complex_query = "metric_name IN ('cpu_usage', 'memory_usage') AND environment = 'production' AND value > 50"
  let complex_results = query_parser.execute(test_data, complex_query)
  
  // 验证复合查询结果
  for point in complex_results {
    assert_true(point.metric_name == "cpu_usage" || point.metric_name == "memory_usage")
    assert_eq(point.get_attribute("environment"), Some("production"))
    assert_true(point.value > 50.0)
  }
  
  // 测试范围查询
  let range_query = "value BETWEEN 30 AND 70"
  let range_results = query_parser.execute(test_data, range_query)
  
  // 验证范围查询结果
  for point in range_results {
    assert_true(point.value >= 30.0)
    assert_true(point.value <= 70.0)
  }
  
  // 测试时间范围查询
  let time_query = "timestamp BETWEEN " + (base_time + 60000000L * 100).to_string() + " AND " + (base_time + 60000000L * 200).to_string()
  let time_results = query_parser.execute(test_data, time_query)
  
  // 验证时间范围查询结果
  for point in time_results {
    assert_true(point.timestamp >= base_time + 60000000L * 100)
    assert_true(point.timestamp <= base_time + 60000000L * 200)
  }
  
  // 测试正则表达式查询
  let regex_query = "host MATCHES 'server-[1-3]' AND region = 'us-west-1'"
  let regex_results = query_parser.execute(test_data, regex_query)
  
  // 验证正则表达式查询结果
  for point in regex_results {
    let host = point.get_attribute("host")
    let region = point.get_attribute("region")
    
    match host {
      Some(h) => {
        assert_true(h == "server-1" || h == "server-2" || h == "server-3")
      }
      None => assert_true(false)
    }
    
    match region {
      Some(r) => assert_eq(r, "us-west-1")
      None => assert_true(false)
    }
  }
  
  // 测试聚合查询
  let aggregation_query = "AVG(value) BY region, environment"
  let aggregation_results = query_parser.execute_aggregation(test_data, aggregation_query)
  
  // 验证聚合查询结果
  assert_eq(aggregation_results.length(), 6)  // 3个区域 * 2个环境
  
  for result in aggregation_results {
    assert_true(result.contains("region="))
    assert_true(result.contains("environment="))
    assert_true(result.contains("avg="))
  }
  
  // 测试排序查询
  let order_query = "metric_name = 'cpu_usage' ORDER BY value DESC LIMIT 10"
  let order_results = query_parser.execute(test_data, order_query)
  
  // 验证排序查询结果
  assert_eq(order_results.length(), 10)
  assert_eq(order_results[0].metric_name, "cpu_usage")
  
  // 验证降序排列
  for i in 1..order_results.length() {
    assert_true(order_results[i-1].value >= order_results[i].value)
  }
  
  // 测试分组查询
  let group_query = "SELECT region, COUNT(*) as count, AVG(value) as avg_value FROM data GROUP BY region"
  let group_results = query_parser.execute_group_by(test_data, group_query)
  
  // 验证分组查询结果
  assert_eq(group_results.length(), 3)  // 3个区域
  
  for result in group_results {
    assert_true(result.contains("region="))
    assert_true(result.contains("count="))
    assert_true(result.contains("avg_value="))
  }
  
  // 测试子查询
  let subquery = "metric_name IN (SELECT DISTINCT metric_name FROM data WHERE value > 80)"
  let subquery_results = query_parser.execute(test_data, subquery)
  
  // 验证子查询结果
  // 应该包含所有值大于80的度量类型的数据点
  let high_value_metrics = []
  for point in test_data {
    if point.value > 80.0 {
      if !high_value_metrics.contains(point.metric_name) {
        high_value_metrics.push(point.metric_name)
      }
    }
  }
  
  for point in subquery_results {
    assert_true(high_value_metrics.contains(point.metric_name))
  }
  
  // 测试复杂嵌套查询
  let nested_query = "(metric_name = 'cpu_usage' OR metric_name = 'memory_usage') AND (environment = 'production' AND (region = 'us-west-1' OR region = 'us-east-1')) AND value > 60"
  let nested_results = query_parser.execute(test_data, nested_query)
  
  // 验证复杂嵌套查询结果
  for point in nested_results {
    assert_true(point.metric_name == "cpu_usage" || point.metric_name == "memory_usage")
    assert_eq(point.get_attribute("environment"), Some("production"))
    
    let region = point.get_attribute("region")
    match region {
      Some(r) => {
        assert_true(r == "us-west-1" || r == "us-east-1")
      }
      None => assert_true(false)
    }
    
    assert_true(point.value > 60.0)
  }
}

// 测试4: 实时流过滤
test "实时流过滤" {
  // 创建流处理器
  let stream_filter = StreamFilter({
    buffer_size: 1000,
    filter_latency_ms: 10,
    batch_size: 100
  })
  
  // 注册过滤规则
  let high_cpu_rule = FilterRule({
    name: "high_cpu_alert",
    condition: "metric_name = 'cpu_usage' AND value > 80",
    action: "alert",
    priority: 1,
    enabled: true
  })
  
  let production_error_rule = FilterRule({
    name: "production_error",
    condition: "environment = 'production' AND metric_name = 'error_rate' AND value > 5",
    action: "alert",
    priority: 2,
    enabled: true
  })
  
  let staging_debug_rule = FilterRule({
    name: "staging_debug",
    condition: "environment = 'staging' AND metric_name = 'debug_requests'",
    action: "log",
    priority: 3,
    enabled: true
  })
  
  stream_filter.register_rule(high_cpu_rule)
  stream_filter.register_rule(production_error_rule)
  stream_filter.register_rule(staging_debug_rule)
  
  // 模拟实时数据流
  let base_time = get_current_timestamp()
  let stream_events = []
  
  for i in 0..1000 {
    let timestamp = base_time + (i * 1000)  // 每秒一个事件
    
    let event = StreamEvent({
      id: "event-" + i.to_string(),
      timestamp: timestamp,
      data: DataPoint({
        timestamp: timestamp,
        metric_name: match i % 8 {
          0 => "cpu_usage",
          1 => "memory_usage",
          2 => "disk_usage",
          3 => "error_rate",
          4 => "request_rate",
          5 => "response_time",
          6 => "debug_requests",
          _ => "throughput"
        },
        value: match i % 8 {
          0 => 20.0 + (Random::next() * 80.0),  // cpu_usage: 20-100
          1 => 30.0 + (Random::next() * 60.0),  // memory_usage: 30-90
          2 => 40.0 + (Random::next() * 50.0),  // disk_usage: 40-90
          3 => (i % 20 == 0 ? 8.0 : 2.0),       // error_rate: 偶尔高
          4 => 100.0 + (Random::next() * 200.0), // request_rate: 100-300
          5 => 50.0 + (Random::next() * 150.0),  // response_time: 50-200
          6 => 10.0 + (Random::next() * 20.0),  // debug_requests: 10-30
          _ => 500.0 + (Random::next() * 1000.0) // throughput: 500-1500
        },
        attributes: [
          ("host", "server-" + ((i % 5) + 1).to_string()),
          ("region", match i % 3 {
            0 => "us-west-1",
            1 => "us-east-1",
            _ => "eu-west-1"
          }),
          ("environment", match i % 2 {
            0 => "production",
            _ => "staging"
          }),
          ("service", match i % 4 {
            0 => "api",
            1 => "web",
            2 => "database",
            _ => "cache"
          })
        ]
      })
    })
    
    stream_events.push(event)
  }
  
  // 处理流数据
  let filter_results = []
  
  for event in stream_events {
    let result = stream_filter.process_event(event)
    if result.matched_rules.length() > 0 {
      filter_results.push(result)
    }
  }
  
  // 验证过滤结果
  assert_true(filter_results.length() > 0)
  
  // 验证高CPU告警
  let high_cpu_alerts = filter_results.filter(fn(result) {
    result.matched_rules.any(fn(rule) { rule.name == "high_cpu_alert" })
  })
  
  for alert in high_cpu_alerts {
    let data_point = alert.event.data
    assert_eq(data_point.metric_name, "cpu_usage")
    assert_true(data_point.value > 80.0)
  }
  
  // 验证生产环境错误告警
  let production_error_alerts = filter_results.filter(fn(result) {
    result.matched_rules.any(fn(rule) { rule.name == "production_error" })
  })
  
  for alert in production_error_alerts {
    let data_point = alert.event.data
    assert_eq(data_point.metric_name, "error_rate")
    assert_eq(data_point.get_attribute("environment"), Some("production"))
    assert_true(data_point.value > 5.0)
  }
  
  // 验证暂存环境调试日志
  let staging_debug_logs = filter_results.filter(fn(result) {
    result.matched_rules.any(fn(rule) { rule.name == "staging_debug" })
  })
  
  for log in staging_debug_logs {
    let data_point = log.event.data
    assert_eq(data_point.metric_name, "debug_requests")
    assert_eq(data_point.get_attribute("environment"), Some("staging"))
  }
  
  // 测试动态规则更新
  let new_rule = FilterRule({
    name: "high_memory",
    condition: "metric_name = 'memory_usage' AND value > 85",
    action: "alert",
    priority: 1,
    enabled: true
  })
  
  stream_filter.register_rule(new_rule)
  
  // 处理新数据点
  let high_memory_event = StreamEvent({
    id: "event-high-memory",
    timestamp: base_time + 2000000,
    data: DataPoint({
      timestamp: base_time + 2000000,
      metric_name: "memory_usage",
      value: 90.0,
      attributes: [
        ("host", "server-1"),
        ("region", "us-west-1"),
        ("environment", "production"),
        ("service", "database")
      ]
    })
  })
  
  let new_result = stream_filter.process_event(high_memory_event)
  
  // 验证新规则生效
  assert_true(new_result.matched_rules.length() > 0)
  assert_true(new_result.matched_rules.any(fn(rule) { rule.name == "high_memory" }))
  
  // 测试时间窗口过滤
  let time_window_filter = TimeWindowFilter({
    window_size_ms: 60000,  // 1分钟窗口
    slide_interval_ms: 10000,  // 10秒滑动
    aggregation_function: "avg"
  })
  
  // 添加时间窗口数据
  for i in 0..120 {
    let timestamp = base_time + (i * 1000)  // 每秒一个数据点
    let value = 50.0 + (Random::next() * 30.0)
    
    time_window_filter.add_data(DataPoint({
      timestamp: timestamp,
      metric_name: "cpu_usage",
      value: value,
      attributes: [
        ("host", "server-1"),
        ("region", "us-west-1"),
        ("environment", "production")
      ]
    }))
  }
  
  // 获取时间窗口聚合结果
  let window_results = time_window_filter.get_windows()
  
  // 验证时间窗口结果
  assert_true(window_results.length() > 0)
  
  for window in window_results {
    assert_true(window.start_time > 0)
    assert_true(window.end_time > window.start_time)
    assert_true(window.aggregated_value >= 50.0)
    assert_true(window.aggregated_value <= 80.0)
    assert_true(window.data_points > 0)
  }
  
  // 测试流过滤性能
  let start_time = get_current_timestamp()
  
  for i in 0..10000 {
    let event = StreamEvent({
      id: "perf-event-" + i.to_string(),
      timestamp: base_time + (i * 100),
      data: DataPoint({
        timestamp: base_time + (i * 100),
        metric_name: "cpu_usage",
        value: 20.0 + (Random::next() * 80.0),
        attributes: [
          ("host", "server-" + ((i % 10) + 1).to_string()),
          ("region", "us-west-1"),
          ("environment", "production")
        ]
      })
    })
    
    stream_filter.process_event(event)
  }
  
  let end_time = get_current_timestamp()
  let processing_time = end_time - start_time
  
  // 验证处理性能（简化验证）
  assert_true(processing_time > 0)
  assert_true(processing_time < 5000000000L)  // 小于5秒
}