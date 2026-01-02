// Azimuth 多维度属性查询综合测试用例
// 专注于验证遥测系统的多维度属性查询和分析功能

// 测试1: 基本多维度属性查询
test "基本多维度属性查询" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建具有多维属性的测试数据
  let test_data = []
  
  // 生成不同服务、操作、状态和地区的组合数据
  let services = ["api-gateway", "user-service", "payment-service", "order-service"]
  let operations = ["create", "read", "update", "delete"]
  let statuses = ["success", "failure", "timeout"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"]
  
  for i = 0; i < 1000; i = i + 1 {
    let service = services[i % services.length()]
    let operation = operations[i % operations.length()]
    let status = statuses[i % statuses.length()]
    let region = regions[i % regions.length()]
    let duration = 50 + (Random::int() % 500) // 50-550ms的随机持续时间
    let user_id = "user-" + (Random::int() % 100).to_string()
    
    test_data.push({
      "timestamp": 1640995200000 + (i * 60000), // 每分钟一个数据点
      "attributes": {
        "service.name": service,
        "operation.type": operation,
        "operation.status": status,
        "region": region,
        "user.id": user_id,
        "duration": duration.to_string(),
        "request.size": (100 + Random::int() % 1000).to_string()
      },
      "metrics": {
        "duration": duration,
        "request_size": 100 + Random::int() % 1000
      }
    })
  }
  
  // 添加数据到查询引擎
  QueryEngine::add_data(query_engine, test_data)
  
  // 测试单维度查询
  let service_filter = QueryFilter::eq("service.name", "api-gateway")
  let api_gateway_results = QueryEngine::query(query_engine, service_filter)
  
  // 验证查询结果
  assert_true(api_gateway_results.length() > 0)
  for i = 0; i < api_gateway_results.length(); i = i + 1 {
    assert_eq(api_gateway_results[i].attributes["service.name"], "api-gateway")
  }
  
  // 测试多维度查询
  let multi_filter = QueryFilter::and([
    QueryFilter::eq("service.name", "payment-service"),
    QueryFilter::eq("operation.type", "create"),
    QueryFilter::eq("operation.status", "success")
  ])
  let payment_create_success_results = QueryEngine::query(query_engine, multi_filter)
  
  // 验证多维度查询结果
  for i = 0; i < payment_create_success_results.length(); i = i + 1 {
    let result = payment_create_success_results[i]
    assert_eq(result.attributes["service.name"], "payment-service")
    assert_eq(result.attributes["operation.type"], "create")
    assert_eq(result.attributes["operation.status"], "success")
  }
  
  // 测试OR查询
  let or_filter = QueryFilter::or([
    QueryFilter::eq("operation.status", "failure"),
    QueryFilter::eq("operation.status", "timeout")
  ])
  let error_results = QueryEngine::query(query_engine, or_filter)
  
  // 验证OR查询结果
  for i = 0; i < error_results.length(); i = i + 1 {
    let status = error_results[i].attributes["operation.status"]
    assert_true(status == "failure" || status == "timeout")
  }
}

// 测试2: 范围查询和比较操作
test "范围查询和比较操作" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建具有数值属性的数据
  let range_data = []
  
  for i = 0; i < 500; i = i + 1 {
    let duration = 10 + (Random::int() % 1000) // 10-1010ms
    let response_size = 100 + (Random::int() % 10000) // 100-10100 bytes
    let error_rate = Random::int() % 100 // 0-99%
    let cpu_usage = Random::int() % 100 // 0-99%
    
    range_data.push({
      "timestamp": 1640995200000 + (i * 60000),
      "attributes": {
        "service": "service-" + (i % 10).to_string(),
        "duration": duration.to_string(),
        "response_size": response_size.to_string(),
        "error_rate": error_rate.to_string(),
        "cpu_usage": cpu_usage.to_string()
      }
    })
  }
  
  QueryEngine::add_data(query_engine, range_data)
  
  // 测试大于查询
  let gt_filter = QueryFilter::gt("duration", "500")
  let high_duration_results = QueryEngine::query(query_engine, gt_filter)
  
  // 验证大于查询结果
  for i = 0; i < high_duration_results.length(); i = i + 1 {
    let duration = high_duration_results[i].attributes["duration"].to_int()
    assert_true(duration > 500)
  }
  
  // 测试范围查询
  let range_filter = QueryFilter::and([
    QueryFilter::gte("response_size", "1000"),
    QueryFilter::lte("response_size", "5000")
  ])
  let medium_response_results = QueryEngine::query(query_engine, range_filter)
  
  // 验证范围查询结果
  for i = 0; i < medium_response_results.length(); i = i + 1 {
    let response_size = medium_response_results[i].attributes["response_size"].to_int()
    assert_true(response_size >= 1000 && response_size <= 5000)
  }
  
  // 测试复合范围查询
  let complex_filter = QueryFilter::and([
    QueryFilter::gt("duration", "200"),
    QueryFilter::lt("error_rate", "10"),
    QueryFilter::gte("cpu_usage", "50")
  ])
  let complex_results = QueryEngine::query(query_engine, complex_filter)
  
  // 验证复合查询结果
  for i = 0; i < complex_results.length(); i = i + 1 {
    let result = complex_results[i].attributes
    assert_true(result["duration"].to_int() > 200)
    assert_true(result["error_rate"].to_int() < 10)
    assert_true(result["cpu_usage"].to_int() >= 50)
  }
}

// 测试3: 模糊匹配和正则表达式查询
test "模糊匹配和正则表达式查询" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建包含文本属性的数据
  let text_data = []
  let error_messages = [
    "Database connection failed",
    "Timeout waiting for response",
    "Invalid user credentials",
    "Service unavailable",
    "Rate limit exceeded",
    "Memory allocation failed",
    "Network connection lost",
    "Authentication token expired"
  ]
  let user_agents = [
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36",
    "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36",
    "curl/7.68.0",
    "Python-requests/2.25.1",
    "Java/11.0.12"
  ]
  
  for i = 0; i < 300; i = i + 1 {
    let error_msg = error_messages[i % error_messages.length()]
    let user_agent = user_agents[i % user_agents.length()]
    let endpoint = "/api/v" + (1 + i % 3).to_string() + "/" + ["users", "orders", "products"][i % 3]
    
    text_data.push({
      "timestamp": 1640995200000 + (i * 60000),
      "attributes": {
        "error.message": error_msg,
        "user.agent": user_agent,
        "endpoint": endpoint,
        "request_id": "req-" + (1000 + i).to_string()
      }
    })
  }
  
  QueryEngine::add_data(query_engine, text_data)
  
  // 测试前缀匹配
  let prefix_filter = QueryFilter::prefix("endpoint", "/api/v1")
  let v1_api_results = QueryEngine::query(query_engine, prefix_filter)
  
  // 验证前缀匹配结果
  for i = 0; i < v1_api_results.length(); i = i + 1 {
    let endpoint = v1_api_results[i].attributes["endpoint"]
    assert_true(endpoint.starts_with("/api/v1"))
  }
  
  // 测试后缀匹配
  let suffix_filter = QueryFilter::suffix("user.agent", "AppleWebKit/537.36")
  let browser_results = QueryEngine::query(query_engine, suffix_filter)
  
  // 验证后缀匹配结果
  for i = 0; i < browser_results.length(); i = i + 1 {
    let user_agent = browser_results[i].attributes["user.agent"]
    assert_true(user_agent.ends_with("AppleWebKit/537.36"))
  }
  
  // 测试包含匹配
  let contains_filter = QueryFilter::contains("error.message", "connection")
  let connection_errors = QueryEngine::query(query_engine, contains_filter)
  
  // 验证包含匹配结果
  for i = 0; i < connection_errors.length(); i = i + 1 {
    let error_msg = connection_errors[i].attributes["error.message"]
    assert_true(error_msg.contains("connection"))
  }
  
  // 测试正则表达式查询
  let regex_filter = QueryFilter::regex("request_id", r"req-1[0-9]{3}")
  let regex_results = QueryEngine::query(query_engine, regex_filter)
  
  // 验证正则表达式查询结果
  for i = 0; i < regex_results.length(); i = i + 1 {
    let request_id = regex_results[i].attributes["request_id"]
    assert_true(request_id.matches(r"req-1[0-9]{3}"))
  }
}

// 测试4: 聚合查询和分组操作
test "聚合查询和分组操作" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建适合聚合的数据
  let aggregation_data = []
  let services = ["auth-service", "payment-service", "order-service", "notification-service"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1"]
  
  for i = 0; i < 1000; i = i + 1 {
    let service = services[i % services.length()]
    let region = regions[i % regions.length()]
    let duration = 50 + (Random::int() % 500)
    let status = if (Random::int() % 10 == 0) { "error" } else { "success" }
    
    aggregation_data.push({
      "timestamp": 1640995200000 + (i * 60000),
      "attributes": {
        "service": service,
        "region": region,
        "status": status,
        "duration": duration.to_string()
      },
      "metrics": {
        "duration": duration
      }
    })
  }
  
  QueryEngine::add_data(query_engine, aggregation_data)
  
  // 测试按服务分组并计算平均持续时间
  let service_avg_duration = QueryEngine::group_by_and_aggregate(
    query_engine,
    ["service"],
    "duration",
    "avg"
  )
  
  // 验证分组聚合结果
  assert_eq(service_avg_duration.length(), services.length())
  for i = 0; i < service_avg_duration.length(); i = i + 1 {
    let group = service_avg_duration[i]
    assert_true(services.contains(group.group_key))
    assert_true(group.aggregated_value > 50.0)
    assert_true(group.aggregated_value < 550.0)
  }
  
  // 测试多维度分组
  let multi_group_count = QueryEngine::group_by_and_count(
    query_engine,
    ["service", "region", "status"]
  )
  
  // 验证多维度分组结果
  assert_true(multi_group_count.length() > 0)
  for i = 0; i < multi_group_count.length(); i = i + 1 {
    let group = multi_group_count[i]
    assert_eq(group.group_keys.length(), 3)
    assert_true(group.count > 0)
  }
  
  // 测试多种聚合函数
  let duration_stats = QueryEngine::multi_aggregate(
    query_engine,
    "duration",
    ["avg", "min", "max", "sum", "count"]
  )
  
  // 验证多种聚合结果
  assert_true(duration_stats["avg"] > 0.0)
  assert_true(duration_stats["min"] <= duration_stats["avg"])
  assert_true(duration_stats["max"] >= duration_stats["avg"])
  assert_true(duration_stats["sum"] > 0.0)
  assert_true(duration_stats["count"] > 0)
  
  // 测试条件聚合
  let error_rate_by_service = QueryEngine::conditional_aggregate(
    query_engine,
    ["service"],
    QueryFilter::eq("status", "error"),
    "count"
  )
  
  // 验证条件聚合结果
  for i = 0; i < error_rate_by_service.length(); i = i + 1 {
    let service_errors = error_rate_by_service[i]
    assert_true(services.contains(service_errors.group_key))
  }
}

// 测试5: 时间窗口查询
test "时间窗口查询" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建时间序列数据
  let time_window_data = []
  let base_timestamp = 1640995200000 // 2022-01-01 00:00:00 UTC
  
  for i = 0; i < 1440; i = i + 1 { // 24小时，每分钟一个数据点
    let timestamp = base_timestamp + (i * 60000)
    let hour = i / 60
    let is_business_hour = hour >= 9 && hour < 17
    let request_count = if (is_business_hour) { 100 + Random::int() % 50 } else { 20 + Random::int() % 30 }
    
    time_window_data.push({
      "timestamp": timestamp,
      "attributes": {
        "request_count": request_count.to_string(),
        "hour": hour.to_string(),
        "is_business_hour": is_business_hour.to_string()
      }
    })
  }
  
  QueryEngine::add_data(query_engine, time_window_data)
  
  // 测试绝对时间窗口查询
  let start_time = base_timestamp + (9 * 3600000) // 09:00
  let end_time = base_timestamp + (17 * 3600000)  // 17:00
  let time_filter = QueryFilter::time_range(start_time, end_time)
  let business_hours_results = QueryEngine::query(query_engine, time_filter)
  
  // 验证时间窗口查询结果
  for i = 0; i < business_hours_results.length(); i = i + 1 {
    let timestamp = business_hours_results[i].timestamp
    assert_true(timestamp >= start_time && timestamp <= end_time)
  }
  
  // 测试相对时间窗口查询
  let now = base_timestamp + (24 * 3600000) // 第二天的同一时间
  let last_6_hours = QueryFilter::relative_time_range(now, -6 * 3600000, 0)
  let recent_results = QueryEngine::query(query_engine, last_6_hours)
  
  // 验证相对时间窗口查询结果
  for i = 0; i < recent_results.length(); i = i + 1 {
    let timestamp = recent_results[i].timestamp
    assert_true(timestamp >= now - (6 * 3600000) && timestamp <= now)
  }
  
  // 测试滑动窗口聚合
  let hourly_avg = QueryEngine::sliding_window_aggregate(
    query_engine,
    "request_count",
    "avg",
    3600000, // 1小时窗口
    60000   // 1分钟滑动间隔
  )
  
  // 验证滑动窗口聚合结果
  assert_eq(hourly_avg.length(), 24 * 60) // 24小时，每分钟一个聚合点
  for i = 0; i < hourly_avg.length(); i = i + 1 {
    let window_avg = hourly_avg[i]
    assert_true(window_avg.value >= 20.0)
    assert_true(window_avg.value <= 150.0)
  }
  
  // 测试时间窗口分组
  let hourly_stats = QueryEngine::time_window_group_by(
    query_engine,
    "hour",
    "request_count",
    ["avg", "max", "min"]
  )
  
  // 验证时间窗口分组结果
  assert_eq(hourly_stats.length(), 24) // 24小时
  for i = 0; i < hourly_stats.length(); i = i + 1 {
    let hour_stat = hourly_stats[i]
    assert_eq(hour_stat.group_key, i.to_string())
    assert_true(hour_stat.aggregations["avg"] > 0.0)
    assert_true(hour_stat.aggregations["max"] >= hour_stat.aggregations["min"])
  }
}

// 测试6: 嵌套属性查询
test "嵌套属性查询" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建包含嵌套属性的数据
  let nested_data = []
  
  for i = 0; i < 200; i = i + 1 {
    let http = {
      "method": ["GET", "POST", "PUT", "DELETE"][i % 4],
      "status_code": [200, 201, 400, 404, 500][i % 5],
      "url": "/api/v" + (1 + i % 3).to_string() + "/resource/" + i.to_string()
    }
    
    let user = {
      "id": "user-" + (i % 50).to_string(),
      "role": ["admin", "user", "guest"][i % 3],
      "preferences": {
        "theme": ["light", "dark"][i % 2],
        "language": ["en", "zh", "ja"][i % 3]
      }
    }
    
    nested_data.push({
      "timestamp": 1640995200000 + (i * 60000),
      "attributes": {
        "http": http,
        "user": user,
        "response_time": (50 + Random::int() % 500).to_string()
      }
    })
  }
  
  QueryEngine::add_data(query_engine, nested_data)
  
  // 测试嵌套属性查询
  let http_method_filter = QueryFilter::eq("http.method", "POST")
  let post_requests = QueryEngine::query(query_engine, http_method_filter)
  
  // 验证嵌套属性查询结果
  for i = 0; i < post_requests.length(); i = i + 1 {
    let http_method = post_requests[i].attributes["http"]["method"]
    assert_eq(http_method, "POST")
  }
  
  // 测试深层嵌套属性查询
  let dark_theme_filter = QueryFilter::eq("user.preferences.theme", "dark")
  let dark_theme_users = QueryEngine::query(query_engine, dark_theme_filter)
  
  // 验证深层嵌套属性查询结果
  for i = 0; i < dark_theme_users.length(); i = i + 1 {
    let theme = dark_theme_users[i].attributes["user"]["preferences"]["theme"]
    assert_eq(theme, "dark")
  }
  
  // 测试嵌套属性范围查询
  let status_code_filter = QueryFilter::and([
    QueryFilter::gte("http.status_code", "400"),
    QueryFilter::lt("http.status_code", "500")
  ])
  let client_errors = QueryEngine::query(query_engine, status_code_filter)
  
  // 验证嵌套属性范围查询结果
  for i = 0; i < client_errors.length(); i = i + 1 {
    let status_code = client_errors[i].attributes["http"]["status_code"].to_int()
    assert_true(status_code >= 400 && status_code < 500)
  }
  
  // 测试嵌套属性分组
  let role_stats = QueryEngine::nested_group_by(
    query_engine,
    "user.role",
    "response_time",
    "avg"
  )
  
  // 验证嵌套属性分组结果
  assert_true(role_stats.length() == 3) // admin, user, guest
  for i = 0; i < role_stats.length(); i = i + 1 {
    let role_stat = role_stats[i]
    assert_true(["admin", "user", "guest"].contains(role_stat.group_key))
    assert_true(role_stat.aggregated_value > 50.0)
    assert_true(role_stat.aggregated_value < 550.0)
  }
}

// 测试7: 查询性能优化
test "查询性能优化" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建大量测试数据
  let large_dataset = []
  let services = ["service-" + i.to_string() for i in range(0, 100)]
  let regions = ["region-" + i.to_string() for i in range(0, 20)]
  
  for i = 0; i < 10000; i = i + 1 {
    let service = services[i % services.length()]
    let region = regions[i % regions.length()]
    let status = if (i % 10 == 0) { "error" } else { "success" }
    
    large_dataset.push({
      "timestamp": 1640995200000 + (i * 60000),
      "attributes": {
        "service": service,
        "region": region,
        "status": status,
        "duration": (50 + Random::int() % 500).to_string(),
        "user_id": "user-" + (i % 1000).to_string()
      }
    })
  }
  
  QueryEngine::add_data(query_engine, large_dataset)
  
  // 创建索引以优化查询性能
  QueryEngine::create_index(query_engine, "service")
  QueryEngine::create_index(query_engine, "region")
  QueryEngine::create_index(query_engine, "status")
  
  // 测试索引查询性能
  let start_time = Time::now()
  
  let indexed_filter = QueryFilter::eq("service", "service-5")
  let indexed_results = QueryEngine::query(query_engine, indexed_filter)
  
  let end_time = Time::now()
  let indexed_query_time = end_time - start_time
  
  // 验证索引查询结果
  assert_true(indexed_results.length() > 0)
  for i = 0; i < indexed_results.length(); i = i + 1 {
    assert_eq(indexed_results[i].attributes["service"], "service-5")
  }
  
  // 测试复合索引查询性能
  start_time = Time::now()
  
  let complex_indexed_filter = QueryFilter::and([
    QueryFilter::eq("service", "service-10"),
    QueryFilter::eq("region", "region-5"),
    QueryFilter::eq("status", "success")
  ])
  let complex_indexed_results = QueryEngine::query(query_engine, complex_indexed_filter)
  
  end_time = Time::now()
  let complex_indexed_query_time = end_time - start_time
  
  // 验证复合索引查询结果
  for i = 0; i < complex_indexed_results.length(); i = i + 1 {
    let attrs = complex_indexed_results[i].attributes
    assert_eq(attrs["service"], "service-10")
    assert_eq(attrs["region"], "region-5")
    assert_eq(attrs["status"], "success")
  }
  
  // 验证查询性能（索引查询应该更快）
  assert_true(indexed_query_time < 100000000) // 100ms
  assert_true(complex_indexed_query_time < 50000000) // 50ms
  
  // 测试查询缓存
  start_time = Time::now()
  
  // 执行相同查询第二次（应该使用缓存）
  let cached_results = QueryEngine::query(query_engine, indexed_filter)
  
  end_time = Time::now()
  let cached_query_time = end_time - start_time
  
  // 验证缓存查询结果
  assert_eq(cached_results.length(), indexed_results.length())
  assert_true(cached_query_time < indexed_query_time / 2) // 缓存查询应该更快
}

// 测试8: 查询结果排序和分页
test "查询结果排序和分页" {
  let query_engine = MultiDimensionalQueryEngine::new()
  
  // 创建适合排序和分页的数据
  let sortable_data = []
  
  for i = 0; i < 1000; i = i + 1 {
    let priority = Random::int() % 10
    let score = Random::float() * 100.0
    let timestamp = 1640995200000 + (Random::int() % 86400000) // 一天内的随机时间
    
    sortable_data.push({
      "timestamp": timestamp,
      "attributes": {
        "id": i.to_string(),
        "priority": priority.to_string(),
        "score": score.to_string(),
        "name": "item-" + i.to_string()
      }
    })
  }
  
  QueryEngine::add_data(query_engine, sortable_data)
  
  // 测试按优先级降序排序
  let priority_desc_sort = QuerySort::desc("priority")
  let priority_desc_results = QueryEngine::query_with_sort(query_engine, QueryFilter::all(), priority_desc_sort)
  
  // 验证排序结果
  for i = 1; i < priority_desc_results.length(); i = i + 1 {
    let prev_priority = priority_desc_results[i-1].attributes["priority"].to_int()
    let curr_priority = priority_desc_results[i].attributes["priority"].to_int()
    assert_true(prev_priority >= curr_priority)
  }
  
  // 测试按分数升序排序
  let score_asc_sort = QuerySort::asc("score")
  let score_asc_results = QueryEngine::query_with_sort(query_engine, QueryFilter::all(), score_asc_sort)
  
  // 验证排序结果
  for i = 1; i < score_asc_results.length(); i = i + 1 {
    let prev_score = score_asc_results[i-1].attributes["score"].to_float()
    let curr_score = score_asc_results[i].attributes["score"].to_float()
    assert_true(prev_score <= curr_score)
  }
  
  // 测试多字段排序
  let multi_sort = QuerySort::multi([
    QuerySortField::desc("priority"),
    QuerySortField::asc("score")
  ])
  let multi_sort_results = QueryEngine::query_with_sort(query_engine, QueryFilter::all(), multi_sort)
  
  // 验证多字段排序结果
  for i = 1; i < multi_sort_results.length(); i = i + 1 {
    let prev_item = multi_sort_results[i-1].attributes
    let curr_item = multi_sort_results[i].attributes
    
    let prev_priority = prev_item["priority"].to_int()
    let curr_priority = curr_item["priority"].to_int()
    
    if (prev_priority == curr_priority) {
      // 优先级相同，按分数升序
      let prev_score = prev_item["score"].to_float()
      let curr_score = curr_item["score"].to_float()
      assert_true(prev_score <= curr_score)
    } else {
      // 优先级不同，按优先级降序
      assert_true(prev_priority > curr_priority)
    }
  }
  
  // 测试分页
  let page_size = 50
  let page_1 = QueryEngine::query_with_pagination(query_engine, QueryFilter::all(), QuerySort::asc("id"), page_size, 0)
  let page_2 = QueryEngine::query_with_pagination(query_engine, QueryFilter::all(), QuerySort::asc("id"), page_size, 1)
  let page_10 = QueryEngine::query_with_pagination(query_engine, QueryFilter::all(), QuerySort::asc("id"), page_size, 9)
  
  // 验证分页结果
  assert_eq(page_1.results.length(), page_size)
  assert_eq(page_2.results.length(), page_size)
  assert_true(page_10.results.length() <= page_size) // 最后一页可能不足
  
  // 验证分页连续性
  let page_1_last_id = page_1.results[page_1.results.length() - 1].attributes["id"].to_int()
  let page_2_first_id = page_2.results[0].attributes["id"].to_int()
  assert_true(page_2_first_id > page_1_last_id)
  
  // 验证分页总数
  let total_pages = (1000 + page_size - 1) / page_size
  assert_eq(page_1.total_pages, total_pages)
  assert_eq(page_1.total_items, 1000)
  assert_eq(page_1.current_page, 0)
  assert_eq(page_2.current_page, 1)
  assert_eq(page_10.current_page, 9)
}