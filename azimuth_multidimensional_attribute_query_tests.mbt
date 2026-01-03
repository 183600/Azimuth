// Azimuth 多维度属性查询测试用例
// 测试多维度属性的查询、过滤和分析功能

test "多维度属性索引创建" {
  // 创建多维度属性索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  
  // 添加属性字段定义
  index.add_field("service.name", azimuth::AttributeType::String, true)
  index.add_field("service.version", azimuth::AttributeType::String, false)
  index.add_field("service.port", azimuth::AttributeType::Int, false)
  index.add_field("response.time", azimuth::AttributeType::Float, false)
  index.add_field("region", azimuth::AttributeType::String, true)
  index.add_field("environment", azimuth::AttributeType::String, true)
  
  // 验证字段定义
  assert_eq(index.fields.length(), 6)
  assert_true(index.is_indexed("service.name"))
  assert_true(index.is_indexed("region"))
  assert_false(index.is_indexed("service.port"))
}

test "多维度数据插入和查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("service.name", azimuth::AttributeType::String, true)
  index.add_field("service.version", azimuth::AttributeType::String, true)
  index.add_field("response.time", azimuth::AttributeType::Float, true)
  index.add_field("region", azimuth::AttributeType::String, true)
  index.add_field("error.count", azimuth::AttributeType::Int, false)
  
  // 插入多维度数据
  let doc1 = azimuth::AttributeDocument::new("doc1")
  doc1.set_attribute("service.name", "user-service")
  doc1.set_attribute("service.version", "1.2.3")
  doc1.set_attribute("response.time", 150.5)
  doc1.set_attribute("region", "us-east-1")
  doc1.set_attribute("error.count", 2)
  index.insert_document(doc1)
  
  let doc2 = azimuth::AttributeDocument::new("doc2")
  doc2.set_attribute("service.name", "payment-service")
  doc2.set_attribute("service.version", "2.1.0")
  doc2.set_attribute("response.time", 89.2)
  doc2.set_attribute("region", "us-west-2")
  doc2.set_attribute("error.count", 0)
  index.insert_document(doc2)
  
  let doc3 = azimuth::AttributeDocument::new("doc3")
  doc3.set_attribute("service.name", "user-service")
  doc3.set_attribute("service.version", "1.2.3")
  doc3.set_attribute("response.time", 200.0)
  doc3.set_attribute("region", "us-east-1")
  doc3.set_attribute("error.count", 5)
  index.insert_document(doc3)
  
  // 查询特定服务名称的文档
  let query = azimuth::AttributeQuery::new()
  query.add_filter("service.name", azimuth::FilterOperator::Equals, "user-service")
  
  let results = index.query(query)
  assert_eq(results.length(), 2)
  assert_eq(results[0].id, "doc1")
  assert_eq(results[1].id, "doc3")
}

test "多维度复合条件查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("service.name", azimuth::AttributeType::String, true)
  index.add_field("response.time", azimuth::AttributeType::Float, true)
  index.add_field("region", azimuth::AttributeType::String, true)
  index.add_field("status", azimuth::AttributeType::String, true)
  
  // 插入测试数据
  let services = ["user-service", "payment-service", "order-service", "notification-service"]
  let regions = ["us-east-1", "us-west-2", "eu-west-1", "ap-southeast-1"]
  let statuses = ["healthy", "degraded", "unhealthy"]
  
  for i in 0..=19 {
    let doc = azimuth::AttributeDocument::new("doc" + i.to_string())
    doc.set_attribute("service.name", services[i % 4])
    doc.set_attribute("response.time", 50.0 + (i.to_int() * 10.0))
    doc.set_attribute("region", regions[i % 4])
    doc.set_attribute("status", statuses[i % 3])
    index.insert_document(doc)
  }
  
  // 复合条件查询：服务名称为user-service且响应时间大于100ms
  let query = azimuth::AttributeQuery::new()
  query.add_filter("service.name", azimuth::FilterOperator::Equals, "user-service")
  query.add_filter("response.time", azimuth::FilterOperator::GreaterThan, 100.0)
  
  let results = index.query(query)
  
  // 验证结果
  for result in results {
    let service_name = result.get_attribute("service.name")
    match service_name {
      Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "user-service")
      _ => assert_true(false)
    }
    
    let response_time = result.get_attribute("response.time")
    match response_time {
      Some(azimuth::AttributeValue::FloatValue(time)) => assert_true(time > 100.0)
      _ => assert_true(false)
    }
  }
}

test "多维度范围查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("cpu.usage", azimuth::AttributeType::Float, true)
  index.add_field("memory.usage", azimuth::AttributeType::Float, true)
  index.add_field("request.count", azimuth::AttributeType::Int, true)
  index.add_field("timestamp", azimuth::AttributeType::Long, true)
  
  // 插入时间序列数据
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00
  for i in 0..=23 {
    let doc = azimuth::AttributeDocument::new("hourly-" + i.to_string())
    doc.set_attribute("cpu.usage", 20.0 + (i.to_int() * 2.5))
    doc.set_attribute("memory.usage", 40.0 + (i.to_int() * 1.5))
    doc.set_attribute("request.count", 1000 + (i.to_int() * 100))
    doc.set_attribute("timestamp", base_timestamp + (i.to_int() * 3600000L))
    index.insert_document(doc)
  }
  
  // 范围查询：CPU使用率在50%-80%之间
  let query = azimuth::AttributeQuery::new()
  query.add_range_filter("cpu.usage", 50.0, 80.0)
  
  let results = index.query(query)
  
  // 验证结果
  for result in results {
    let cpu_usage = result.get_attribute("cpu.usage")
    match cpu_usage {
      Some(azimuth::AttributeValue::FloatValue(usage)) => {
        assert_true(usage >= 50.0 && usage <= 80.0)
      }
      _ => assert_true(false)
    }
  }
}

test "多维度聚合查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("service.name", azimuth::AttributeType::String, true)
  index.add_field("region", azimuth::AttributeType::String, true)
  index.add_field("response.time", azimuth::AttributeType::Float, true)
  index.add_field("error.count", azimuth::AttributeType::Int, true)
  
  // 插入测试数据
  let services = ["user-service", "payment-service", "order-service"]
  let regions = ["us-east-1", "us-west-2"]
  
  for i in 0..=29 {
    let doc = azimuth::AttributeDocument::new("doc" + i.to_string())
    doc.set_attribute("service.name", services[i % 3])
    doc.set_attribute("region", regions[i % 2])
    doc.set_attribute("response.time", 50.0 + (i.to_int() * 5.0))
    doc.set_attribute("error.count", i.to_int() % 10)
    index.insert_document(doc)
  }
  
  // 按服务名称分组聚合
  let aggregation = azimuth::AggregationQuery::new()
  aggregation.add_group_by("service.name")
  aggregation.add_metric("avg_response_time", azimuth::AggregationType::Average, "response.time")
  aggregation.add_metric("total_errors", azimuth::AggregationType::Sum, "error.count")
  aggregation.add_metric("max_response_time", azimuth::AggregationType::Max, "response.time")
  
  let results = index.aggregate(aggregation)
  
  // 验证聚合结果
  assert_eq(results.length(), 3) // 3个服务
  
  for result in results {
    let service_name = result.group_key["service.name"]
    match service_name {
      Some(azimuth::AttributeValue::StringValue(name)) => {
        assert_true(name == "user-service" || name == "payment-service" || name == "order-service")
      }
      _ => assert_true(false)
    }
    
    // 验证聚合指标存在
    assert_true(result.metrics.contains_key("avg_response_time"))
    assert_true(result.metrics.contains_key("total_errors"))
    assert_true(result.metrics.contains_key("max_response_time"))
  }
}

test "多维度模糊查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("log.message", azimuth::AttributeType::String, true)
  index.add_field("error.stack", azimuth::AttributeType::String, false)
  index.add_field("service.name", azimuth::AttributeType::String, true)
  
  // 插入日志数据
  let log_messages = [
    "Failed to connect to database: connection timeout",
    "User authentication failed: invalid credentials",
    "Payment processing error: insufficient funds",
    "Database connection restored: service resumed",
    "User logged in successfully: authentication passed"
  ]
  
  let services = ["auth-service", "payment-service", "user-service"]
  
  for i in 0..=4 {
    let doc = azimuth::AttributeDocument::new("log" + i.to_string())
    doc.set_attribute("log.message", log_messages[i])
    doc.set_attribute("service.name", services[i % 3])
    index.insert_document(doc)
  }
  
  // 模糊查询：包含"connection"的消息
  let query = azimuth::AttributeQuery::new()
  query.add_fuzzy_filter("log.message", "connection")
  
  let results = index.query(query)
  
  // 验证结果
  assert_eq(results.length(), 2)
  
  for result in results {
    let message = result.get_attribute("log.message")
    match message {
      Some(azimuth::AttributeValue::StringValue(msg)) => {
        assert_true(msg.contains("connection"))
      }
      _ => assert_true(false)
    }
  }
}

test "多维度嵌套属性查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("service.name", azimuth::AttributeType::String, true)
  index.add_field("metadata.labels", azimuth::AttributeType::Object, false)
  index.add_field("metrics.http.2xx.count", azimuth::AttributeType::Int, true)
  index.add_field("metrics.http.4xx.count", azimuth::AttributeType::Int, true)
  index.add_field("metrics.http.5xx.count", azimuth::AttributeType::Int, true)
  
  // 插入嵌套属性数据
  let doc1 = azimuth::AttributeDocument::new("service-1")
  doc1.set_attribute("service.name", "api-gateway")
  doc1.set_nested_attribute("metadata.labels.team", "platform")
  doc1.set_nested_attribute("metadata.labels.environment", "production")
  doc1.set_attribute("metrics.http.2xx.count", 12500)
  doc1.set_attribute("metrics.http.4xx.count", 320)
  doc1.set_attribute("metrics.http.5xx.count", 45)
  index.insert_document(doc1)
  
  let doc2 = azimuth::AttributeDocument::new("service-2")
  doc2.set_attribute("service.name", "user-service")
  doc2.set_nested_attribute("metadata.labels.team", "backend")
  doc2.set_nested_attribute("metadata.labels.environment", "staging")
  doc2.set_attribute("metrics.http.2xx.count", 8900)
  doc2.set_attribute("metrics.http.4xx.count", 180)
  doc2.set_attribute("metrics.http.5xx.count", 25)
  index.insert_document(doc2)
  
  // 嵌套属性查询：团队为platform的服务
  let query = azimuth::AttributeQuery::new()
  query.add_nested_filter("metadata.labels.team", azimuth::FilterOperator::Equals, "platform")
  
  let results = index.query(query)
  
  // 验证结果
  assert_eq(results.length(), 1)
  assert_eq(results[0].id, "service-1")
  
  let team = results[0].get_nested_attribute("metadata.labels.team")
  match team {
    Some(azimuth::AttributeValue::StringValue(t)) => assert_eq(t, "platform")
    _ => assert_true(false)
  }
}

test "多维度统计查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("response.time", azimuth::AttributeType::Float, true)
  index.add_field("service.name", azimuth::AttributeType::String, true)
  index.add_field("status.code", azimuth::AttributeType::Int, true)
  
  // 插入性能数据
  let services = ["web-service", "api-service", "auth-service"]
  let response_times = [25.5, 120.3, 45.8, 89.2, 200.5, 65.7, 150.2, 35.6, 95.4, 180.9]
  let status_codes = [200, 200, 500, 200, 404, 200, 500, 200, 200, 404]
  
  for i in 0..=9 {
    let doc = azimuth::AttributeDocument::new("req" + i.to_string())
    doc.set_attribute("response.time", response_times[i])
    doc.set_attribute("service.name", services[i % 3])
    doc.set_attribute("status.code", status_codes[i])
    index.insert_document(doc)
  }
  
  // 统计查询：响应时间的百分位数
  let stats = index.calculate_statistics("response.time")
  
  // 验证统计结果
  assert_eq(stats.count, 10)
  assert_eq(stats.min, 25.5)
  assert_eq(stats.max, 200.5)
  assert_eq(stats.sum, 1008.5)
  assert_eq(stats.average, 100.85)
  assert_eq(stats.p50, 92.3) // 中位数
  assert_eq(stats.p95, 200.5)
  assert_eq(stats.p99, 200.5)
}

test "多维度时间序列查询" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("timestamp", azimuth::AttributeType::Long, true)
  index.add_field("metric.name", azimuth::AttributeType::String, true)
  index.add_field("metric.value", azimuth::AttributeType::Float, true)
  index.add_field("host.name", azimuth::AttributeType::String, true)
  
  // 插入时间序列数据
  let base_timestamp = 1609459200000L // 2021-01-01 00:00:00
  let metrics = ["cpu.usage", "memory.usage", "disk.usage"]
  let hosts = ["host-01", "host-02", "host-03"]
  
  for day in 0..=6 {
    for hour in 0..=23 {
      for metric_idx in 0..=2 {
        for host_idx in 0..=2 {
          let doc = azimuth::AttributeDocument::new("metric-" + day.to_string() + "-" + hour.to_string() + "-" + metric_idx.to_string() + "-" + host_idx.to_string())
          doc.set_attribute("timestamp", base_timestamp + ((day * 24 + hour) * 3600000L))
          doc.set_attribute("metric.name", metrics[metric_idx])
          doc.set_attribute("metric.value", 20.0 + (hour.to_int() * 2.5))
          doc.set_attribute("host.name", hosts[host_idx])
          index.insert_document(doc)
        }
      }
    }
  }
  
  // 时间范围查询：查询前3天的数据
  let query = azimuth::AttributeQuery::new()
  query.add_time_range_filter("timestamp", base_timestamp, base_timestamp + (3 * 24 * 3600000L))
  
  let results = index.query(query)
  
  // 验证结果（3天 * 24小时 * 3指标 * 3主机 = 648条记录）
  assert_eq(results.length(), 648)
  
  // 按时间排序
  let sorted_results = index.sort_by(results, "timestamp", azimuth::SortOrder::Ascending)
  
  // 验证排序结果
  for i in 1..sorted_results.length() - 1 {
    let current_time = sorted_results[i].get_attribute("timestamp")
    let prev_time = sorted_results[i - 1].get_attribute("timestamp")
    
    match (current_time, prev_time) {
      (Some(azimuth::AttributeValue::LongValue(curr)), Some(azimuth::AttributeValue::LongValue(prev))) => {
        assert_true(curr >= prev)
      }
      _ => assert_true(false)
    }
  }
}

test "多维度查询性能优化" {
  // 创建索引
  let index = azimuth::MultiDimensionalAttributeIndex::new()
  index.add_field("service.name", azimuth::AttributeType::String, true)
  index.add_field("response.time", azimuth::AttributeType::Float, true)
  index.add_field("timestamp", azimuth::AttributeType::Long, true)
  index.add_field("status.code", azimuth::AttributeType::Int, true)
  
  // 插入大量数据（10000条记录）
  let base_timestamp = 1609459200000L
  let services = ["service-a", "service-b", "service-c", "service-d", "service-e"]
  
  for i in 0..=9999 {
    let doc = azimuth::AttributeDocument::new("doc" + i.to_string())
    doc.set_attribute("service.name", services[i % 5])
    doc.set_attribute("response.time", 10.0 + (i.to_int() % 1000).to_float())
    doc.set_attribute("timestamp", base_timestamp + (i.to_int() * 60000L))
    doc.set_attribute("status.code", if i.to_int() % 10 == 0 { 500 } else { 200 })
    index.insert_document(doc)
  }
  
  // 测试查询性能
  let start_time = azimuth::TimeUtil::current_time_millis()
  
  // 复杂查询：特定服务、响应时间范围、时间范围、状态码
  let query = azimuth::AttributeQuery::new()
  query.add_filter("service.name", azimuth::FilterOperator::Equals, "service-c")
  query.add_range_filter("response.time", 100.0, 500.0)
  query.add_time_range_filter("timestamp", base_timestamp, base_timestamp + 3600000L)
  query.add_filter("status.code", azimuth::FilterOperator::Equals, 200)
  
  let results = index.query(query)
  
  let end_time = azimuth::TimeUtil::current_time_millis()
  let query_duration = end_time - start_time
  
  // 验证查询结果
  for result in results {
    let service_name = result.get_attribute("service.name")
    match service_name {
      Some(azimuth::AttributeValue::StringValue(name)) => assert_eq(name, "service-c")
      _ => assert_true(false)
    }
  }
  
  // 验证查询性能（应该在合理时间内完成）
  assert_true(query_duration < 1000) // 查询时间应小于1秒
  assert_true(index.index_size > 0) // 索引应该被正确创建
}