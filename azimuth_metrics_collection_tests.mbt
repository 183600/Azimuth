// Azimuth 指标收集测试用例
// 测试各种类型指标的收集、聚合和报告功能

// 测试1: 计数器指标收集
test "计数器指标收集和聚合" {
  // 创建计数器指标
  let request_counter = {
    name: "http_requests_total",
    description: "HTTP请求总数",
    labels: [
      ("method", "GET"),
      ("status", "200"),
      ("service", "api-gateway")
    ],
    value: 0
  }
  
  // 验证初始状态
  assert_eq(request_counter.name, "http_requests_total")
  assert_eq(request_counter.value, 0)
  assert_eq(request_counter.labels.length(), 3)
  
  // 模拟计数器增量
  let increment_counter = fn(counter: { name: String, description: String, labels: Array[(String, String)], value: Int }, delta: Int) {
    { counter | value: counter.value + delta }
  }
  
  // 增加计数器
  let counter1 = increment_counter(request_counter, 1)
  assert_eq(counter1.value, 1)
  
  let counter2 = increment_counter(counter1, 5)
  assert_eq(counter2.value, 6)
  
  let counter3 = increment_counter(counter2, 3)
  assert_eq(counter3.value, 9)
  
  // 测试多个计数器聚合
  let counters = [
    { name: "http_requests_total", labels: [("method", "GET"), ("status", "200")], value: 10 },
    { name: "http_requests_total", labels: [("method", "GET"), ("status", "404")], value: 2 },
    { name: "http_requests_total", labels: [("method", "POST"), ("status", "201")], value: 5 },
    { name: "http_requests_total", labels: [("method", "POST"), ("status", "400")], value: 1 }
  ]
  
  // 按状态码聚合
  let aggregate_by_status = fn(counters: Array<{ name: String, labels: Array[(String, String)], value: Int }>) {
    let mut result = {}
    for counter in counters {
      let status = match counter.labels.find(fn(label) { label.0 == "status" }) {
        Some((_, value)) => value
        None => "unknown"
      }
      let current = match result.get(status) {
        Some(value) => value
        None => 0
      }
      result = result.insert(status, current + counter.value)
    }
    result
  }
  
  let status_aggregates = aggregate_by_status(counters)
  
  // 验证聚合结果
  let status_200 = match status_aggregates.get("200") {
    Some(value) => value
    None => 0
  }
  assert_eq(status_200, 10)
  
  let status_404 = match status_aggregates.get("404") {
    Some(value) => value
    None => 0
  }
  assert_eq(status_404, 2)
  
  let status_201 = match status_aggregates.get("201") {
    Some(value) => value
    None => 0
  }
  assert_eq(status_201, 5)
  
  let status_400 = match status_aggregates.get("400") {
    Some(value) => value
    None => 0
  }
  assert_eq(status_400, 1)
}

// 测试2: 仪表盘指标收集
test "仪表盘指标收集和变化跟踪" {
  // 创建仪表盘指标
  let memory_gauge = {
    name: "memory_usage_bytes",
    description: "内存使用量（字节）",
    labels: [("instance", "server-01"), ("region", "us-west-2")],
    value: 1024 * 1024 * 512  // 512MB
  }
  
  // 验证初始值
  assert_eq(memory_gauge.name, "memory_usage_bytes")
  assert_eq(memory_gauge.value, 1024 * 1024 * 512)
  assert_eq(memory_gauge.labels.length(), 2)
  
  // 模拟仪表盘值更新
  let set_gauge_value = fn(gauge: { name: String, description: String, labels: Array[(String, String)], value: Int }, new_value: Int) {
    { gauge | value: new_value }
  }
  
  // 更新仪表盘值
  let gauge1 = set_gauge_value(memory_gauge, 1024 * 1024 * 600)  // 600MB
  assert_eq(gauge1.value, 1024 * 1024 * 600)
  
  let gauge2 = set_gauge_value(gauge1, 1024 * 1024 * 450)  // 450MB
  assert_eq(gauge2.value, 1024 * 1024 * 450)
  
  // 测试仪表盘变化跟踪
  let track_gauge_changes = fn(gauges: Array<{ name: String, labels: Array[(String, String)], value: Int }>) {
    let mut changes = []
    for i in 1..gauges.length() {
      let prev = gauges[i - 1]
      let curr = gauges[i]
      let change = curr.value - prev.value
      changes = changes.push({
        timestamp: i * 1000,  // 模拟时间戳
        previous_value: prev.value,
        current_value: curr.value,
        change: change,
        change_percent: (change as Float / prev.value as Float) * 100.0
      })
    }
    changes
  }
  
  // 创建时间序列数据
  let gauge_series = [
    { name: "cpu_usage", labels: [("instance", "server-01")], value: 25 },
    { name: "cpu_usage", labels: [("instance", "server-01")], value: 30 },
    { name: "cpu_usage", labels: [("instance", "server-01")], value: 45 },
    { name: "cpu_usage", labels: [("instance", "server-01")], value: 35 },
    { name: "cpu_usage", labels: [("instance", "server-01")], value: 20 }
  ]
  
  let changes = track_gauge_changes(gauge_series)
  assert_eq(changes.length(), 4)
  
  // 验证第一个变化
  assert_eq(changes[0].previous_value, 25)
  assert_eq(changes[0].current_value, 30)
  assert_eq(changes[0].change, 5)
  assert_eq(changes[0].change_percent.round(), 20.0)
  
  // 验证第二个变化
  assert_eq(changes[1].previous_value, 30)
  assert_eq(changes[1].current_value, 45)
  assert_eq(changes[1].change, 15)
  assert_eq(changes[1].change_percent.round(), 50.0)
  
  // 验证第三个变化（下降）
  assert_eq(changes[2].previous_value, 45)
  assert_eq(changes[2].current_value, 35)
  assert_eq(changes[2].change, -10)
  assert_eq(changes[2].change_percent.round(), -22.0)
}

// 测试3: 直方图指标收集
test "直方图指标收集和统计计算" {
  // 创建直方图指标配置
  let histogram_config = {
    name: "request_duration_seconds",
    description: "请求持续时间分布",
    labels: [("endpoint", "/api/users"), ("method", "GET")],
    buckets: [0.1, 0.5, 1.0, 2.5, 5.0, 10.0],
    bucket_counts: [0, 0, 0, 0, 0, 0, 0],  // 包括+Inf桶
    sum: 0.0,
    count: 0
  }
  
  // 验证初始配置
  assert_eq(histogram_config.name, "request_duration_seconds")
  assert_eq(histogram_config.buckets.length(), 6)
  assert_eq(histogram_config.bucket_counts.length(), 7)  // 包括+Inf桶
  assert_eq(histogram_config.sum, 0.0)
  assert_eq(histogram_config.count, 0)
  
  // 模拟观察直方图值
  let observe_histogram = fn(histogram: { name: String, buckets: Array[Float], bucket_counts: Array[Int], sum: Float, count: Int }, value: Float) {
    let mut new_bucket_counts = histogram.bucket_counts.copy()
    let mut bucket_index = 0
    
    // 找到合适的桶
    for i in 0..histogram.buckets.length() {
      if value <= histogram.buckets[i] {
        bucket_index = i
        break
      }
      bucket_index = histogram.buckets.length()  // +Inf桶
    }
    
    // 增加桶计数
    for i in bucket_index..new_bucket_counts.length() {
      new_bucket_counts[i] = new_bucket_counts[i] + 1
    }
    
    {
      name: histogram.name,
      buckets: histogram.buckets,
      bucket_counts: new_bucket_counts,
      sum: histogram.sum + value,
      count: histogram.count + 1
    }
  }
  
  // 观察一些值
  let histogram1 = observe_histogram(histogram_config, 0.05)  // 应该进入0.1桶
  assert_eq(histogram1.bucket_counts[0], 1)
  assert_eq(histogram1.bucket_counts[1], 1)
  assert_eq(histogram1.bucket_counts[6], 1)
  assert_eq(histogram1.sum, 0.05)
  assert_eq(histogram1.count, 1)
  
  let histogram2 = observe_histogram(histogram1, 0.3)  // 应该进入0.5桶
  assert_eq(histogram2.bucket_counts[0], 1)
  assert_eq(histogram2.bucket_counts[1], 2)
  assert_eq(histogram2.bucket_counts[6], 2)
  assert_eq(histogram2.sum.round(), 0.35)
  assert_eq(histogram2.count, 2)
  
  let histogram3 = observe_histogram(histogram2, 1.5)  // 应该进入2.5桶
  assert_eq(histogram3.bucket_counts[0], 1)
  assert_eq(histogram3.bucket_counts[1], 2)
  assert_eq(histogram3.bucket_counts[2], 2)
  assert_eq(histogram3.bucket_counts[3], 3)
  assert_eq(histogram3.bucket_counts[6], 3)
  assert_eq(histogram3.sum.round(), 1.85)
  assert_eq(histogram3.count, 3)
  
  // 计算百分位数
  let calculate_percentile = fn(histogram: { bucket_counts: Array[Int], buckets: Array[Float], count: Int }, percentile: Float) -> Float {
    if histogram.count == 0 {
      return 0.0
    }
    
    let target_count = (histogram.count as Float * percentile) as Int
    let mut accumulated = 0
    
    for i in 0..histogram.bucket_counts.length() {
      accumulated = accumulated + histogram.bucket_counts[i]
      if accumulated >= target_count {
        if i < histogram.buckets.length() {
          return histogram.buckets[i]
        } else {
          return Float::infinity()
        }
      }
    }
    
    Float::infinity()
  }
  
  // 计算第50百分位数（中位数）
  let p50 = calculate_percentile(histogram3, 0.5)
  assert_true(p50 >= 0.1 && p50 <= 0.5)
  
  // 计算第95百分位数
  let p95 = calculate_percentile(histogram3, 0.95)
  assert_true(p95 >= 1.0 && p95 <= 2.5)
  
  // 计算第99百分位数
  let p99 = calculate_percentile(histogram3, 0.99)
  assert_true(p99 >= 1.5 && p99 <= 2.5)
}

// 测试4: 摘要指标收集
test "摘要指标收集和统计计算" {
  // 创建摘要指标配置
  let summary_config = {
    name: "response_size_bytes",
    description: "响应大小摘要",
    labels: [("endpoint", "/api/data"), ("method", "POST")],
    quantiles: [0.5, 0.9, 0.95, 0.99],
    sample_count: 0,
    sample_sum: 0.0,
    samples: []
  }
  
  // 验证初始配置
  assert_eq(summary_config.name, "response_size_bytes")
  assert_eq(summary_config.quantiles.length(), 4)
  assert_eq(summary_config.sample_count, 0)
  assert_eq(summary_config.sample_sum, 0.0)
  assert_eq(summary_config.samples.length(), 0)
  
  // 模拟观察摘要值
  let observe_summary = fn(summary: { quantiles: Array[Float], sample_count: Int, sample_sum: Float, samples: Array[Float] }, value: Float) {
    let new_samples = summary.samples.push(value)
    {
      quantiles: summary.quantiles,
      sample_count: summary.sample_count + 1,
      sample_sum: summary.sample_sum + value,
      samples: new_samples
    }
  }
  
  // 观察一些值
  let summary1 = observe_summary(summary_config, 1024.0)
  assert_eq(summary1.sample_count, 1)
  assert_eq(summary1.sample_sum, 1024.0)
  assert_eq(summary1.samples.length(), 1)
  
  let summary2 = observe_summary(summary1, 2048.0)
  assert_eq(summary2.sample_count, 2)
  assert_eq(summary2.sample_sum, 3072.0)
  assert_eq(summary2.samples.length(), 2)
  
  let summary3 = observe_summary(summary2, 512.0)
  assert_eq(summary3.sample_count, 3)
  assert_eq(summary3.sample_sum, 3584.0)
  assert_eq(summary3.samples.length(), 3)
  
  let summary4 = observe_summary(summary3, 4096.0)
  assert_eq(summary4.sample_count, 4)
  assert_eq(summary4.sample_sum, 7680.0)
  assert_eq(summary4.samples.length(), 4)
  
  let summary5 = observe_summary(summary4, 1536.0)
  assert_eq(summary5.sample_count, 5)
  assert_eq(summary5.sample_sum, 9216.0)
  assert_eq(summary5.samples.length(), 5)
  
  // 计算分位数
  let calculate_quantile = fn(sorted_samples: Array[Float], quantile: Float) -> Float {
    if sorted_samples.length() == 0 {
      return 0.0
    }
    
    let index = (sorted_samples.length() as Float * quantile) as Int
    if index >= sorted_samples.length() {
      sorted_samples[sorted_samples.length() - 1]
    } else {
      sorted_samples[index]
    }
  }
  
  // 对样本进行排序
  let sorted_samples = summary5.samples.sort(fn(a, b) { if a < b { -1 } else if a > b { 1 } else { 0 } })
  assert_eq(sorted_samples, [512.0, 1024.0, 1536.0, 2048.0, 4096.0])
  
  // 计算各个分位数
  let q50 = calculate_quantile(sorted_samples, 0.5)
  assert_eq(q50, 1536.0)
  
  let q90 = calculate_quantile(sorted_samples, 0.9)
  assert_eq(q90, 2048.0)
  
  let q95 = calculate_quantile(sorted_samples, 0.95)
  assert_eq(q95, 2048.0)
  
  let q99 = calculate_quantile(sorted_samples, 0.99)
  assert_eq(q99, 4096.0)
  
  // 计算平均值
  let average = summary5.sample_sum / (summary5.sample_count as Float)
  assert_eq(average, 9216.0 / 5.0)
  assert_eq(average.round(), 1843.0)
}

// 测试5: 指标标签和维度
test "指标标签和维度管理" {
  // 创建带标签的指标
  let labeled_metric = {
    name: "database_connections",
    description: "数据库连接数",
    label_names: ["service", "database", "environment"],
    values: {}
  }
  
  // 验证初始状态
  assert_eq(labeled_metric.name, "database_connections")
  assert_eq(labeled_metric.label_names.length(), 3)
  
  // 添加带标签的值
  let set_metric_value = fn(metric: { name: String, label_names: Array[String], values: {} }, label_values: Array[String], value: Int) {
    if label_values.length() != metric.label_names.length() {
      return metric  // 标签值数量不匹配
    }
    
    let label_key = label_values.join("|")
    let updated_values = metric.values.insert(label_key, value)
    { metric | values: updated_values }
  }
  
  // 设置不同标签组合的值
  let metric1 = set_metric_value(labeled_metric, ["user-service", "postgres", "production"], 10)
  let metric2 = set_metric_value(metric1, ["order-service", "postgres", "production"], 15)
  let metric3 = set_metric_value(metric2, ["payment-service", "mysql", "production"], 8)
  let metric4 = set_metric_value(metric3, ["user-service", "postgres", "staging"], 5)
  
  // 验证标签值设置
  let user_prod_value = match metric4.values.get("user-service|postgres|production") {
    Some(value) => value
    None => 0
  }
  assert_eq(user_prod_value, 10)
  
  let order_prod_value = match metric4.values.get("order-service|postgres|production") {
    Some(value) => value
    None => 0
  }
  assert_eq(order_prod_value, 15)
  
  let payment_prod_value = match metric4.values.get("payment-service|mysql|production") {
    Some(value) => value
    None => 0
  }
  assert_eq(payment_prod_value, 8)
  
  let user_staging_value = match metric4.values.get("user-service|postgres|staging") {
    Some(value) => value
    None => 0
  }
  assert_eq(user_staging_value, 5)
  
  // 按标签维度聚合
  let aggregate_by_label = fn(metric: { values: {} }, label_index: Int) {
    let mut aggregates = {}
    for (key, value) in metric.values {
      let label_values = key.split("|")
      if label_index < label_values.length() {
        let label_value = label_values[label_index]
        let current = match aggregates.get(label_value) {
          Some(current_value) => current_value
          None => 0
        }
        aggregates = aggregates.insert(label_value, current + value)
      }
    }
    aggregates
  }
  
  // 按服务名称聚合
  let service_aggregates = aggregate_by_label(metric4, 0)
  
  let user_service_total = match service_aggregates.get("user-service") {
    Some(total) => total
    None => 0
  }
  assert_eq(user_service_total, 15)  // 10 (production) + 5 (staging)
  
  let order_service_total = match service_aggregates.get("order-service") {
    Some(total) => total
    None => 0
  }
  assert_eq(order_service_total, 15)
  
  let payment_service_total = match service_aggregates.get("payment-service") {
    Some(total) => total
    None => 0
  }
  assert_eq(payment_service_total, 8)
  
  // 按数据库类型聚合
  let database_aggregates = aggregate_by_label(metric4, 1)
  
  let postgres_total = match database_aggregates.get("postgres") {
    Some(total) => total
    None => 0
  }
  assert_eq(postgres_total, 30)  // 10 + 15 + 5
  
  let mysql_total = match database_aggregates.get("mysql") {
    Some(total) => total
    None => 0
  }
  assert_eq(mysql_total, 8)
  
  // 按环境聚合
  let environment_aggregates = aggregate_by_label(metric4, 2)
  
  let production_total = match environment_aggregates.get("production") {
    Some(total) => total
    None => 0
  }
  assert_eq(production_total, 33)  // 10 + 15 + 8
  
  let staging_total = match environment_aggregates.get("staging") {
    Some(total) => total
    None => 0
  }
  assert_eq(staging_total, 5)
}

// 测试6: 指标导出和格式化
test "指标导出和格式化" {
  // 创建指标集合
  let metrics = [
    {
      name: "http_requests_total",
      type: "counter",
      labels: [("method", "GET"), ("status", "200")],
      value: 1234,
      timestamp: 1640995200000
    },
    {
      name: "http_requests_total",
      type: "counter",
      labels: [("method", "GET"), ("status", "404")],
      value: 56,
      timestamp: 1640995200000
    },
    {
      name: "memory_usage_bytes",
      type: "gauge",
      labels: [("instance", "server-01")],
      value: 1073741824,
      timestamp: 1640995200000
    },
    {
      name: "request_duration_seconds",
      type: "histogram",
      labels: [("le", "0.1")],
      value: 100,
      timestamp: 1640995200000
    }
  ]
  
  // 导出为Prometheus格式
  let export_to_prometheus = fn(metrics: Array<{ name: String, type: String, labels: Array[(String, String)], value: Int, timestamp: Int }>) {
    let mut result = ""
    
    for metric in metrics {
      // 添加指标类型注释
      result = result + "# TYPE " + metric.name + " " + metric.type + "\n"
      
      // 添加标签
      let label_str = if metric.labels.length() > 0 {
        let mut labels = []
        for label in metric.labels {
          labels = labels.push(label.0 + "=\"" + label.1 + "\"")
        }
        "{" + labels.join(",") + "}"
      } else {
        ""
      }
      
      // 添加指标值
      result = result + metric.name + label_str + " " + metric.value.to_string() + " " + metric.timestamp.to_string() + "\n"
    }
    
    result
  }
  
  // 导出指标
  let prometheus_output = export_to_prometheus(metrics)
  
  // 验证Prometheus格式输出
  assert_true(prometheus_output.contains("# TYPE http_requests_total counter"))
  assert_true(prometheus_output.contains("# TYPE memory_usage_bytes gauge"))
  assert_true(prometheus_output.contains("# TYPE request_duration_seconds histogram"))
  
  assert_true(prometheus_output.contains("http_requests_total{method=\"GET\",status=\"200\"} 1234"))
  assert_true(prometheus_output.contains("http_requests_total{method=\"GET\",status=\"404\"} 56"))
  assert_true(prometheus_output.contains("memory_usage_bytes{instance=\"server-01\"} 1073741824"))
  assert_true(prometheus_output.contains("request_duration_seconds{le=\"0.1\"} 100"))
  
  // 验证行数（每个指标2行：类型注释和值）
  let lines = prometheus_output.split("\n")
  assert_eq(lines.length(), 9)  // 8行指标数据 + 1个空行
  
  // 导出为JSON格式
  let export_to_json = fn(metrics: Array<{ name: String, type: String, labels: Array[(String, String)], value: Int, timestamp: Int }>) {
    let mut result = "{\"metrics\":["
    
    for i in 0..metrics.length() {
      let metric = metrics[i]
      result = result + "{"
      result = result + "\"name\":\"" + metric.name + "\"," 
      result = result + "\"type\":\"" + metric.type + "\"," 
      
      // 添加标签
      result = result + "\"labels\":["
      for j in 0..metric.labels.length() {
        let label = metric.labels[j]
        result = result + "{\"key\":\"" + label.0 + "\",\"value\":\"" + label.1 + "\"}"
        if j < metric.labels.length() - 1 {
          result = result + ","
        }
      }
      result = result + "],"
      
      // 添加值和时间戳
      result = result + "\"value\":" + metric.value.to_string() + ","
      result = result + "\"timestamp\":" + metric.timestamp.to_string()
      result = result + "}"
      
      if i < metrics.length() - 1 {
        result = result + ","
      }
    }
    
    result = result + "]}"
  }
  
  // 导出为JSON
  let json_output = export_to_json(metrics)
  
  // 验证JSON格式输出
  assert_true(json_output.contains("\"name\":\"http_requests_total\""))
  assert_true(json_output.contains("\"type\":\"counter\""))
  assert_true(json_output.contains("\"key\":\"method\",\"value\":\"GET\""))
  assert_true(json_output.contains("\"value\":1234"))
  
  // 验证JSON结构
  assert_true(json_output.starts_with("{\"metrics\":["))
  assert_true(json_output.ends_with("]}"))
}