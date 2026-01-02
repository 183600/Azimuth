// Azimuth Multidimensional Attribute Query Tests
// 多维度属性查询测试用例，专注于复杂的属性过滤、聚合和查询操作

test "多维度属性过滤查询测试" {
  // 创建具有多维度属性的遥测数据
  let telemetry_data = []
  let services = ["auth-service", "user-service", "order-service", "payment-service", "inventory-service"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"]
  let environments = ["production", "staging", "development"]
  let versions = ["1.0.0", "1.1.0", "1.2.0", "2.0.0"]
  
  // 生成1000条具有多维度属性的遥测数据
  for i = 0; i < 1000; i = i + 1 {
    let service = services[i % services.length()]
    let region = regions[i % regions.length()]
    let environment = environments[i % environments.length()]
    let version = versions[i % versions.length()]
    
    let data_point = {
      "id": "trace_" + i.to_string(),
      "timestamp": 1640995200000L + i.to_long() * 60000L, // 每分钟一个点
      "duration": 100 + (i % 500),
      "status": if i % 20 == 0 { "error" } else { "ok" },
      "attributes": {
        "service.name": service,
        "service.version": version,
        "deployment.environment": environment,
        "cloud.region": region,
        "host.name": "host_" + (i % 10).to_string(),
        "user.id": "user_" + (i % 100).to_string(),
        "operation.name": "operation_" + (i % 5).to_string(),
        "http.method": if i % 2 == 0 { "GET" } else { "POST" },
        "http.status_code": if i % 20 == 0 { 500 } else { 200 },
        "custom.attribute": "value_" + (i % 20).to_string()
      }
    }
    telemetry_data.push(data_point)
  }
  
  // 实现属性过滤查询函数
  let filter_by_attributes = |data: Array[Map[String, Any]], filters: Map[String, Any]| {
    let filtered_data = []
    
    for item in data {
      let match = true
      let attributes = item["attributes"]
      
      for (key, value) in filters {
        if attributes[key] != value {
          match = false
          break
        }
      }
      
      if match {
        filtered_data.push(item)
      }
    }
    
    filtered_data
  }
  
  // 测试单维度属性过滤
  let service_filter = {"service.name": "auth-service"}
  let auth_service_data = filter_by_attributes(telemetry_data, service_filter)
  
  // 验证过滤结果
  assert_true(auth_service_data.length() > 0)
  for item in auth_service_data {
    assert_eq(item["attributes"]["service.name"], "auth-service")
  }
  
  // 测试多维度属性过滤
  let multi_filter = {
    "service.name": "payment-service",
    "deployment.environment": "production",
    "cloud.region": "us-east-1"
  }
  let filtered_data = filter_by_attributes(telemetry_data, multi_filter)
  
  // 验证多维度过滤结果
  assert_true(filtered_data.length() > 0)
  for item in filtered_data {
    assert_eq(item["attributes"]["service.name"], "payment-service")
    assert_eq(item["attributes"]["deployment.environment"], "production")
    assert_eq(item["attributes"]["cloud.region"], "us-east-1")
  }
  
  // 测试数值属性过滤
  let status_code_filter = {"http.status_code": 500}
  let error_data = filter_by_attributes(telemetry_data, status_code_filter)
  
  // 验证错误数据
  assert_true(error_data.length() > 0)
  for item in error_data {
    assert_eq(item["attributes"]["http.status_code"], 500)
    assert_eq(item["status"], "error")
  }
  
  // 测试组合过滤（先按服务过滤，再按状态过滤）
  let user_service_data = filter_by_attributes(telemetry_data, {"service.name": "user-service"})
  let user_service_errors = filter_by_attributes(user_service_data, {"http.status_code": 500})
  
  // 验证组合过滤结果
  assert_true(user_service_errors.length() > 0)
  for item in user_service_errors {
    assert_eq(item["attributes"]["service.name"], "user-service")
    assert_eq(item["attributes"]["http.status_code"], 500)
  }
  
  // 测试不存在属性值的过滤（应该返回空结果）
  let non_existent_filter = {"service.name": "non-existent-service"}
  let empty_result = filter_by_attributes(telemetry_data, non_existent_filter)
  assert_eq(empty_result.length(), 0)
  
  assert_true(true)
}

test "属性值范围查询测试" {
  // 创建具有数值属性的遥测数据
  let metrics_data = []
  let base_timestamp = 1640995200000L
  
  for i = 0; i < 500; i = i + 1 {
    let data_point = {
      "id": "metric_" + i.to_string(),
      "timestamp": base_timestamp + i.to_long() * 60000L,
      "attributes": {
        "service.name": "service_" + (i % 5).to_string(),
        "host.name": "host_" + (i % 10).to_string(),
        "region": "region_" + (i % 3).to_string()
      },
      "metrics": {
        "cpu.usage": 20.0 + (i % 80).to_double() * 0.75, // 20-80%
        "memory.usage": 30.0 + (i % 70).to_double() * 0.7, // 30-70%
        "disk.io": 10.0 + (i % 100).to_double() * 0.9, // 10-100 MB/s
        "network.io": 5.0 + (i % 50).to_double() * 1.8, // 5-95 MB/s
        "request.count": (i % 1000).to_int(), // 0-999
        "response.time": 50 + (i % 200) // 50-249 ms
      }
    }
    metrics_data.push(data_point)
  }
  
  // 实现范围查询函数
  let filter_by_range = |data: Array[Map[String, Any]], metric: String, min_value: Double, max_value: Double| {
    let filtered_data = []
    
    for item in data {
      let value = item["metrics"][metric].to_double()
      if value >= min_value && value <= max_value {
        filtered_data.push(item)
      }
    }
    
    filtered_data
  }
  
  // 测试CPU使用率范围查询
  let high_cpu_data = filter_by_range(metrics_data, "cpu.usage", 70.0, 80.0)
  
  // 验证高CPU使用率数据
  assert_true(high_cpu_data.length() > 0)
  for item in high_cpu_data {
    let cpu_usage = item["metrics"]["cpu.usage"].to_double()
    assert_true(cpu_usage >= 70.0 && cpu_usage <= 80.0)
  }
  
  // 测试内存使用率范围查询
  let medium_memory_data = filter_by_range(metrics_data, "memory.usage", 40.0, 60.0)
  
  // 验证中等内存使用率数据
  assert_true(medium_memory_data.length() > 0)
  for item in medium_memory_data {
    let memory_usage = item["metrics"]["memory.usage"].to_double()
    assert_true(memory_usage >= 40.0 && memory_usage <= 60.0)
  }
  
  // 测试磁盘IO范围查询
  let high_disk_io_data = filter_by_range(metrics_data, "disk.io", 80.0, 100.0)
  
  // 验证高磁盘IO数据
  assert_true(high_disk_io_data.length() > 0)
  for item in high_disk_io_data {
    let disk_io = item["metrics"]["disk.io"].to_double()
    assert_true(disk_io >= 80.0 && disk_io <= 100.0)
  }
  
  // 测试请求计数范围查询
  let medium_request_count_data = filter_by_range(metrics_data, "request.count", 400.0, 600.0)
  
  // 验证中等请求数数据
  assert_true(medium_request_count_data.length() > 0)
  for item in medium_request_count_data {
    let request_count = item["metrics"]["request.count"].to_double()
    assert_true(request_count >= 400.0 && request_count <= 600.0)
  }
  
  // 测试响应时间范围查询
  let slow_response_data = filter_by_range(metrics_data, "response.time", 200.0, 249.0)
  
  // 验证慢响应数据
  assert_true(slow_response_data.length() > 0)
  for item in slow_response_data {
    let response_time = item["metrics"]["response.time"].to_double()
    assert_true(response_time >= 200.0 && response_time <= 249.0)
  }
  
  // 测试无结果的范围查询
  let empty_range_data = filter_by_range(metrics_data, "cpu.usage", 90.0, 100.0)
  assert_eq(empty_range_data.length(), 0)
  
  // 测试边界值范围查询
  let boundary_cpu_data = filter_by_range(metrics_data, "cpu.usage", 20.0, 20.0)
  
  // 验证边界值查询
  assert_true(boundary_cpu_data.length() > 0)
  for item in boundary_cpu_data {
    let cpu_usage = item["metrics"]["cpu.usage"].to_double()
    assert_eq(cpu_usage, 20.0)
  }
  
  assert_true(true)
}

test "属性聚合查询测试" {
  // 创建用于聚合测试的遥测数据
  let aggregation_data = []
  let services = ["web-service", "db-service", "cache-service", "auth-service"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1"]
  let base_timestamp = 1640995200000L
  
  for i = 0; i < 200; i = i + 1 {
    let service = services[i % services.length()]
    let region = regions[i % regions.length()]
    
    let data_point = {
      "id": "data_" + i.to_string(),
      "timestamp": base_timestamp + i.to_long() * 60000L,
      "attributes": {
        "service.name": service,
        "cloud.region": region,
        "operation.name": "op_" + (i % 4).to_string(),
        "http.method": if i % 2 == 0 { "GET" } else { "POST" },
        "status.code": if i % 10 == 0 { 500 } else if i % 5 == 0 { 404 } else { 200 }
      },
      "metrics": {
        "response.time": 50 + (i % 150),
        "cpu.usage": 20.0 + (i % 60).to_double() * 0.8,
        "memory.usage": 30.0 + (i % 50).to_double() * 0.7,
        "request.count": (i % 100) + 1
      }
    }
    aggregation_data.push(data_point)
  }
  
  // 实现聚合查询函数
  let aggregate_by_attribute = |data: Array[Map[String, Any]], attribute: String, metric: String, aggregation_type: String| {
    let groups = {}
    
    // 按属性分组
    for item in data {
      let attr_value = item["attributes"][attribute]
      
      if groups[attr_value] is None {
        groups[attr_value] = []
      }
      
      groups[attr_value].push(item["metrics"][metric].to_double())
    }
    
    // 计算聚合值
    let results = {}
    for (attr_value, values) in groups {
      let aggregated_value = match aggregation_type {
        "avg" => values.reduce(|acc, val| acc + val, 0.0) / values.length().to_double(),
        "sum" => values.reduce(|acc, val| acc + val, 0.0),
        "min" => values.reduce(|acc, val| if val < acc { val } else { acc }, values[0]),
        "max" => values.reduce(|acc, val| if val > acc { val } else { acc }, values[0]),
        "count" => values.length().to_double(),
        _ => 0.0
      }
      
      results[attr_value] = aggregated_value
    }
    
    results
  }
  
  // 测试按服务名称聚合平均响应时间
  let avg_response_time_by_service = aggregate_by_attribute(
    aggregation_data, 
    "service.name", 
    "response.time", 
    "avg"
  )
  
  // 验证服务聚合结果
  assert_eq(avg_response_time_by_service.length(), services.length())
  for service in services {
    assert_true(avg_response_time_by_service.contains(service))
    assert_true(avg_response_time_by_service[service] > 0.0)
  }
  
  // 测试按区域聚合总请求数
  let sum_requests_by_region = aggregate_by_attribute(
    aggregation_data, 
    "cloud.region", 
    "request.count", 
    "sum"
  )
  
  // 验证区域聚合结果
  assert_eq(sum_requests_by_region.length(), regions.length())
  for region in regions {
    assert_true(sum_requests_by_region.contains(region))
    assert_true(sum_requests_by_region[region] > 0.0)
  }
  
  // 测试按操作名称聚合最大CPU使用率
  let max_cpu_by_operation = aggregate_by_attribute(
    aggregation_data, 
    "operation.name", 
    "cpu.usage", 
    "max"
  )
  
  // 验证操作聚合结果
  assert_true(max_cpu_by_operation.length() > 0)
  for (operation, max_cpu) in max_cpu_by_operation {
    assert_true(max_cpu >= 20.0) // 最小CPU使用率是20%
    assert_true(max_cpu <= 68.0) // 最大CPU使用率是20 + 60*0.8 = 68
  }
  
  // 测试按HTTP方法聚合最小内存使用率
  let min_memory_by_method = aggregate_by_attribute(
    aggregation_data, 
    "http.method", 
    "memory.usage", 
    "min"
  )
  
  // 验证HTTP方法聚合结果
  assert_eq(min_memory_by_method.length(), 2) // GET和POST
  assert_true(min_memory_by_method.contains("GET"))
  assert_true(min_memory_by_method.contains("POST"))
  
  // 测试按状态码聚合计数
  let count_by_status_code = aggregate_by_attribute(
    aggregation_data, 
    "status.code", 
    "response.time", 
    "count"
  )
  
  // 验证状态码聚合结果
  assert_true(count_by_status_code.contains(200))
  assert_true(count_by_status_code.contains(404))
  assert_true(count_by_status_code.contains(500))
  
  // 验证状态码200的计数最多
  assert_true(count_by_status_code[200] > count_by_status_code[404])
  assert_true(count_by_status_code[200] > count_by_status_code[500])
  
  assert_true(true)
}

test "多维度属性组合查询测试" {
  // 创建具有复杂多维度属性的遥测数据
  let complex_data = []
  let services =["frontend", "backend", "database", "cache", "queue"]
  let environments = ["prod", "staging", "dev"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"]
  let versions = ["1.0.x", "1.1.x", "1.2.x", "2.0.x"]
  let base_timestamp = 1640995200000L
  
  for i = 0; i < 300; i = i + 1 {
    let service = services[i % services.length()]
    let environment = environments[i % environments.length()]
    let region = regions[i % regions.length()]
    let version = versions[i % versions.length()]
    
    let data_point = {
      "id": "complex_" + i.to_string(),
      "timestamp": base_timestamp + i.to_long() * 60000L,
      "duration": 50 + (i % 500),
      "status": if i % 15 == 0 { "error" } else { "ok" },
      "attributes": {
        "service.name": service,
        "service.version": version,
        "deployment.environment": environment,
        "cloud.region": region,
        "host.name": "host_" + (i % 15).to_string(),
        "container.id": "container_" + (i % 20).to_string(),
        "pod.name": "pod-" + (i % 10).to_string(),
        "operation.name": "op_" + (i % 6).to_string(),
        "user.type": if i % 3 == 0 { "premium" } else { "standard" },
        "request.source": if i % 2 == 0 { "web" } else { "mobile" },
        "error.type": if i % 15 == 0 { "timeout" } else if i % 30 == 0 { "connection" } else { "" }
      },
      "metrics": {
        "cpu.usage": 10.0 + (i % 70).to_double() * 0.9,
        "memory.usage": 20.0 + (i % 60).to_double() * 0.8,
        "response.time": 20 + (i % 200),
        "request.size": 100 + (i % 1000),
        "response.size": 200 + (i % 2000)
      }
    }
    complex_data.push(data_point)
  }
  
  // 实现复杂查询函数
  let complex_query = |data: Array[Map[String, Any]], 
                        attribute_filters: Map[String, Any], 
                        metric_ranges: Map[String, (Double, Double)], 
                        limit: Int| {
    let results = []
    
    for item in data {
      let match = true
      let attributes = item["attributes"]
      let metrics = item["metrics"]
      
      // 检查属性过滤器
      for (key, value) in attribute_filters {
        if attributes[key] != value {
          match = false
          break
        }
      }
      
      // 检查指标范围
      if match {
        for (metric, (min_val, max_val)) in metric_ranges {
          let value = metrics[metric].to_double()
          if value < min_val || value > max_val {
            match = false
            break
          }
        }
      }
      
      if match {
        results.push(item)
        if results.length() >= limit {
          break
        }
      }
    }
    
    results
  }
  
  // 测试复杂组合查询：生产环境中的前端服务，CPU使用率在50-80%之间
  let query1 = complex_query(
    complex_data,
    {"service.name": "frontend", "deployment.environment": "prod"},
    {"cpu.usage": (50.0, 80.0)},
    50
  )
  
  // 验证查询结果
  assert_true(query1.length() > 0)
  for item in query1 {
    assert_eq(item["attributes"]["service.name"], "frontend")
    assert_eq(item["attributes"]["deployment.environment"], "prod")
    let cpu_usage = item["metrics"]["cpu.usage"].to_double()
    assert_true(cpu_usage >= 50.0 && cpu_usage <= 80.0)
  }
  
  // 测试复杂组合查询：数据库服务，响应时间在100-200ms之间，用户类型为premium
  let query2 = complex_query(
    complex_data,
    {"service.name": "database", "user.type": "premium"},
    {"response.time": (100.0, 200.0)},
    30
  )
  
  // 验证查询结果
  assert_true(query2.length() > 0)
  for item in query2 {
    assert_eq(item["attributes"]["service.name"], "database")
    assert_eq(item["attributes"]["user.type"], "premium")
    let response_time = item["metrics"]["response.time"].to_double()
    assert_true(response_time >= 100.0 && response_time <= 200.0)
  }
  
  // 测试复杂组合查询：特定区域和版本的缓存服务，内存使用率在40-60%之间
  let query3 = complex_query(
    complex_data,
    {"service.name": "cache", "cloud.region": "us-east-1", "service.version": "1.2.x"},
    {"memory.usage": (40.0, 60.0)},
    25
  )
  
  // 验证查询结果
  for item in query3 {
    assert_eq(item["attributes"]["service.name"], "cache")
    assert_eq(item["attributes"]["cloud.region"], "us-east-1")
    assert_eq(item["attributes"]["service.version"], "1.2.x")
    let memory_usage = item["metrics"]["memory.usage"].to_double()
    assert_true(memory_usage >= 40.0 && memory_usage <= 60.0)
  }
  
  // 测试多指标范围查询：CPU和内存使用率都在特定范围内
  let query4 = complex_query(
    complex_data,
    {"deployment.environment": "prod"},
    {"cpu.usage": (30.0, 70.0), "memory.usage": (30.0, 70.0)},
    40
  )
  
  // 验证多指标范围查询结果
  for item in query4 {
    assert_eq(item["attributes"]["deployment.environment"], "prod")
    let cpu_usage = item["metrics"]["cpu.usage"].to_double()
    let memory_usage = item["metrics"]["memory.usage"].to_double()
    assert_true(cpu_usage >= 30.0 && cpu_usage <= 70.0)
    assert_true(memory_usage >= 30.0 && memory_usage <= 70.0)
  }
  
  // 测试空结果查询：不存在的组合
  let query5 = complex_query(
    complex_data,
    {"service.name": "non-existent", "deployment.environment": "prod"},
    {"cpu.usage": (50.0, 80.0)},
    10
  )
  
  // 验证空结果
  assert_eq(query5.length(), 0)
  
  assert_true(true)
}

test "属性值正则表达式查询测试" {
  // 创建具有各种格式属性值的遥测数据
  let regex_data = []
  let base_timestamp = 1640995200000L
  
  for i = 0; i < 100; i = i + 1 {
    let data_point = {
      "id": "regex_" + i.to_string(),
      "timestamp": base_timestamp + i.to_long() * 60000L,
      "attributes": {
        "service.name": "service-" + (i % 5).to_string() + "-" + (i % 3).to_string(),
        "operation.name": "operation_" + (i % 4).to_string(),
        "host.name": "host-" + (i % 10).to_string() + ".example.com",
        "pod.name": "pod-" + (i % 8).to_string() + "-abcd1234",
        "request.id": "req-" + i.to_string() + "-" + (i * 7 % 1000).to_string(),
        "trace.id": "trace-" + (i * 13 % 10000).to_string() + "-" + (i * 17 % 1000).to_string(),
        "user.email": "user" + (i % 50).to_string() + "@example.com",
        "error.message": if i % 10 == 0 { "Connection timeout to database-" + (i % 3).to_string() } else { "" }
      },
      "metrics": {
        "response.time": 50 + (i % 150)
      }
    }
    regex_data.push(data_point)
  }
  
  // 实现正则表达式查询函数（简化实现）
  let regex_match = |pattern: String, text: String| {
    // 简化的正则匹配实现，只支持几种基本模式
    if pattern.contains("*") {
      let prefix = pattern.split("*")[0]
      let suffix = if pattern.split("*").length() > 1 { pattern.split("*")[1] } else { "" }
      
      return text.starts_with(prefix) && text.ends_with(suffix)
    } else if pattern.contains("?") {
      // 简单的字符匹配
      return text.length() == pattern.length()
    } else {
      return text == pattern
    }
  }
  
  let regex_query = |data: Array[Map[String, Any]], attribute: String, pattern: String| {
    let results = []
    
    for item in data {
      let value = item["attributes"][attribute]
      if regex_match(pattern, value) {
        results.push(item)
      }
    }
    
    results
  }
  
  // 测试通配符查询：服务名称以"service-2"开头
  let service_pattern = "service-2*"
  let service_results = regex_query(regex_data, "service.name", service_pattern)
  
  // 验证通配符查询结果
  assert_true(service_results.length() > 0)
  for item in service_results {
    let service_name = item["attributes"]["service.name"]
    assert_true(service_name.starts_with("service-2"))
  }
  
  // 测试通配符查询：主机名以".example.com"结尾
  let host_pattern = "*.example.com"
  let host_results = regex_query(regex_data, "host.name", host_pattern)
  
  // 验证通配符查询结果
  assert_true(host_results.length() > 0)
  for item in host_results {
    let host_name = item["attributes"]["host.name"]
    assert_true(host_name.ends_with(".example.com"))
  }
  
  // 测试通配符查询：Pod名称包含"-abcd1234"
  let pod_pattern = "*-abcd1234"
  let pod_results = regex_query(regex_data, "pod.name", pod_pattern)
  
  // 验证通配符查询结果
  assert_true(pod_results.length() > 0)
  for item in pod_results {
    let pod_name = item["attributes"]["pod.name"]
    assert_true(pod_name.ends_with("-abcd1234"))
  }
  
  // 测试精确匹配查询：操作名称为"operation_2"
  let operation_pattern = "operation_2"
  let operation_results = regex_query(regex_data, "operation.name", operation_pattern)
  
  // 验证精确匹配查询结果
  assert_true(operation_results.length() > 0)
  for item in operation_results {
    let operation_name = item["attributes"]["operation.name"]
    assert_eq(operation_name, "operation_2")
  }
  
  // 测试邮箱格式查询：以"@example.com"结尾
  let email_pattern = "*@example.com"
  let email_results = regex_query(regex_data, "user.email", email_pattern)
  
  // 验证邮箱格式查询结果
  assert_true(email_results.length() > 0)
  for item in email_results {
    let email = item["attributes"]["user.email"]
    assert_true(email.ends_with("@example.com"))
  }
  
  // 测试错误消息查询：包含"timeout"
  let error_pattern = "*timeout*"
  let error_results = regex_query(regex_data, "error.message", error_pattern)
  
  // 验证错误消息查询结果
  assert_true(error_results.length() > 0)
  for item in error_results {
    let error_message = item["attributes"]["error.message"]
    assert_true(error_message.contains("timeout"))
  }
  
  // 测试无结果查询：不匹配的模式
  let no_results_pattern = "non-existent-*"
  let no_results = regex_query(regex_data, "service.name", no_results_pattern)
  assert_eq(no_results.length(), 0)
  
  assert_true(true)
}