// Azimuth 实时数据流处理测试用例
// 专注于实时流处理、窗口计算、流聚合等功能

// 测试1: 实时数据流基础处理
test "实时数据流基础处理功能" {
  // 定义数据点类型
  type DataPoint = {
    timestamp: Int
    value: Float
    source: String
    tags: Map[String, String]
  }
  
  // 定义流窗口类型
  enum WindowType {
    Tumbling  // 滚动窗口，不重叠
    Sliding   // 滑动窗口，有重叠
    Session   // 会话窗口，基于活动间隙
  }
  
  // 定义时间窗口
  type TimeWindow = {
    window_type: WindowType
    window_size_ms: Int
    slide_ms: Int    // 仅对滑动窗口有效
    session_gap_ms: Int // 仅对会话窗口有效
  }
  
  // 定义窗口聚合结果
  type WindowResult = {
    window_start: Int
    window_end: Int
    count: Int
    sum: Float
    avg: Float
    min: Float
    max: Float
    source: String
  }
  
  // 创建数据点
  let create_data_point = fn(timestamp: Int, value: Float, source: String, tags: Map[String, String]) -> DataPoint {
    { timestamp, value, source, tags }
  }
  
  // 滚动窗口处理
  let process_tumbling_window = fn(points: Array[DataPoint], window_size_ms: Int) -> Array[WindowResult] {
    if points.length() == 0 {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort_by(fn(a, b) { a.timestamp - b.timestamp })
    
    let mut results = []
    let mut window_start = sorted_points[0].timestamp
    let mut window_end = window_start + window_size_ms
    let mut window_points = []
    
    for point in sorted_points {
      if point.timestamp >= window_start && point.timestamp < window_end {
        window_points = window_points.push(point)
      } else {
        // 处理当前窗口
        if window_points.length() > 0 {
          let count = window_points.length()
          let sum = window_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
          let avg = sum / (count as Float)
          let min = window_points.reduce(fn(acc, p) { if p.value < acc { p.value } else { acc } }, 999999.0)
          let max = window_points.reduce(fn(acc, p) { if p.value > acc { p.value } else { acc } }, 0.0)
          
          let result = {
            window_start,
            window_end,
            count,
            sum,
            avg,
            min,
            max,
            source: window_points[0].source
          }
          
          results = results.push(result)
        }
        
        // 开始新窗口
        window_start = point.timestamp
        window_end = window_start + window_size_ms
        window_points = [point]
      }
    }
    
    // 处理最后一个窗口
    if window_points.length() > 0 {
      let count = window_points.length()
      let sum = window_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
      let avg = sum / (count as Float)
      let min = window_points.reduce(fn(acc, p) { if p.value < acc { p.value } else { acc } }, 999999.0)
      let max = window_points.reduce(fn(acc, p) { if p.value > acc { p.value } else { acc } }, 0.0)
      
      let result = {
        window_start,
        window_end,
        count,
        sum,
        avg,
        min,
        max,
        source: window_points[0].source
      }
      
      results = results.push(result)
    }
    
    results
  }
  
  // 滑动窗口处理
  let process_sliding_window = fn(points: Array[DataPoint], window_size_ms: Int, slide_ms: Int) -> Array[WindowResult] {
    if points.length() == 0 {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort_by(fn(a, b) { a.timestamp - b.timestamp })
    
    let mut results = []
    let min_timestamp = sorted_points[0].timestamp
    let max_timestamp = sorted_points[sorted_points.length() - 1].timestamp
    
    let mut window_start = min_timestamp
    
    while window_start <= max_timestamp {
      let window_end = window_start + window_size_ms
      
      // 收集窗口内的数据点
      let window_points = sorted_points.filter(fn(p) {
        p.timestamp >= window_start && p.timestamp < window_end
      })
      
      if window_points.length() > 0 {
        let count = window_points.length()
        let sum = window_points.reduce(fn(acc, p) { acc + p.value }, 0.0)
        let avg = sum / (count as Float)
        let min = window_points.reduce(fn(acc, p) { if p.value < acc { p.value } else { acc } }, 999999.0)
        let max = window_points.reduce(fn(acc, p) { if p.value > acc { p.value } else { acc } }, 0.0)
        
        let result = {
          window_start,
          window_end,
          count,
          sum,
          avg,
          min,
          max,
          source: window_points[0].source
        }
        
        results = results.push(result)
      }
      
      window_start = window_start + slide_ms
    }
    
    results
  }
  
  // 创建测试数据
  let base_time = 1640995200000 // 2022-01-01 00:00:00 UTC in milliseconds
  let service_tags = Map::from([("service", "api"), ("env", "prod")])
  
  let data_points = [
    create_data_point(base_time, 75.5, "service-a", service_tags),
    create_data_point(base_time + 30000, 80.2, "service-a", service_tags),
    create_data_point(base_time + 60000, 65.8, "service-a", service_tags),
    create_data_point(base_time + 90000, 90.1, "service-a", service_tags),
    create_data_point(base_time + 120000, 70.3, "service-a", service_tags),
    create_data_point(base_time + 150000, 85.7, "service-b", service_tags),
    create_data_point(base_time + 180000, 78.2, "service-b", service_tags),
    create_data_point(base_time + 210000, 92.4, "service-b", service_tags),
    create_data_point(base_time + 240000, 68.9, "service-b", service_tags),
    create_data_point(base_time + 270000, 73.6, "service-c", service_tags),
    create_data_point(base_time + 300000, 81.3, "service-c", service_tags),
    create_data_point(base_time + 330000, 77.8, "service-c", service_tags)
  ]
  
  // 测试滚动窗口处理
  let tumbling_results = process_tumbling_window(data_points, 120000) // 2分钟窗口
  assert_eq(tumbling_results.length(), 3) // 应该有3个窗口
  
  // 验证第一个窗口 (0-2分钟)
  let first_window = tumbling_results[0]
  assert_eq(first_window.window_start, base_time)
  assert_eq(first_window.window_end, base_time + 120000)
  assert_eq(first_window.count, 4) // 前4个点
  assert_eq(first_window.avg.round(), ((75.5 + 80.2 + 65.8 + 90.1) / 4.0).round())
  
  // 验证第二个窗口 (2-4分钟)
  let second_window = tumbling_results[1]
  assert_eq(second_window.window_start, base_time + 120000)
  assert_eq(second_window.window_end, base_time + 240000)
  assert_eq(second_window.count, 4) // 第5-8个点
  
  // 测试滑动窗口处理
  let sliding_results = process_sliding_window(data_points, 120000, 60000) // 2分钟窗口，1分钟滑动
  assert_eq(sliding_results.length(), 5) // 应该有5个窗口
  
  // 验证第一个滑动窗口 (0-2分钟)
  let first_sliding = sliding_results[0]
  assert_eq(first_sliding.window_start, base_time)
  assert_eq(first_sliding.window_end, base_time + 120000)
  assert_eq(first_sliding.count, 4)
  
  // 验证第二个滑动窗口 (1-3分钟)
  let second_sliding = sliding_results[1]
  assert_eq(second_sliding.window_start, base_time + 60000)
  assert_eq(second_sliding.window_end, base_time + 180000)
  assert_eq(second_sliding.count, 5) // 第2-6个点
}

// 测试2: 流式聚合与分组计算
test "流式聚合与分组计算功能" {
  // 定义聚合类型
  enum AggregationType {
    Sum
    Average
    Min
    Max
    Count
    Variance
    Percentile
  }
  
  // 定义分组键
  type GroupKey = {
    service: String
    environment: String
    region: String
  }
  
  // 定义聚合结果
  type AggregationResult = {
    group_key: GroupKey
    aggregation_type: AggregationType
    value: Float
    timestamp: Int
    sample_count: Int
  }
  
  // 定义流处理配置
  type StreamConfig = {
    group_by_fields: Array[String]
    aggregation_types: Array[AggregationType]
    window_size_ms: Int
    emit_interval_ms: Int
  }
  
  // 创建分组键
  let create_group_key = fn(point: DataPoint, fields: Array[String]) -> GroupKey {
    let mut service = ""
    let mut environment = ""
    let mut region = ""
    
    for field in fields {
      match Map::get(point.tags, field) {
        Some(value) => {
          match field {
            "service" => service = value,
            "environment" => environment = value,
            "region" => region = value,
            _ => {}
          }
        },
        None => {}
      }
    }
    
    { service, environment, region }
  }
  
  // 计算聚合值
  let calculate_aggregation = fn(values: Array[Float], agg_type: AggregationType) -> Float {
    if values.length() == 0 {
      return 0.0
    }
    
    match agg_type {
      AggregationType::Sum => values.reduce(fn(acc, v) { acc + v }, 0.0),
      AggregationType::Average => {
        let sum = values.reduce(fn(acc, v) { acc + v }, 0.0)
        sum / (values.length() as Float)
      },
      AggregationType::Min => values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, 999999.0),
      AggregationType::Max => values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, 0.0),
      AggregationType::Count => values.length() as Float,
      AggregationType::Variance => {
        let avg = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
        let sum_squared_diff = values.reduce(fn(acc, v) { 
          let diff = v - avg
          acc + (diff * diff)
        }, 0.0)
        sum_squared_diff / (values.length() as Float)
      },
      AggregationType::Percentile => {
        // 简化的95百分位计算
        let sorted = values.sort_by(fn(a, b) { a - b })
        let index = ((sorted.length() as Float) * 0.95) as Int
        if index < sorted.length() {
          sorted[index]
        } else {
          sorted[sorted.length() - 1]
        }
      }
    }
  }
  
  // 流式聚合处理
  let process_stream_aggregation = fn(points: Array[DataPoint], config: StreamConfig) -> Array[AggregationResult] {
    if points.length() == 0 {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort_by(fn(a, b) { a.timestamp - b.timestamp })
    
    // 按分组键分组
    let mut grouped_data = Map::empty()
    
    for point in sorted_points {
      let group_key = create_group_key(point, config.group_by_fields)
      let key_str = group_key.service + ":" + group_key.environment + ":" + group_key.region
      
      let current_points = match Map::get(grouped_data, key_str) {
        Some(points) => points,
        None => []
      }
      
      grouped_data = Map::insert(grouped_data, key_str, current_points.push(point))
    }
    
    // 对每个分组计算聚合
    let mut results = []
    
    for (key_str, group_points) in grouped_data.to_array() {
      let parts = key_str.split(":")
      let group_key = {
        service: parts[0],
        environment: if parts.length() > 1 { parts[1] } else { "" },
        region: if parts.length() > 2 { parts[2] } else { "" }
      }
      
      let values = group_points.map(fn(p) { p.value })
      
      for agg_type in config.aggregation_types {
        let agg_value = calculate_aggregation(values, agg_type)
        let timestamp = group_points[group_points.length() - 1].timestamp // 使用最后一个点的时间戳
        
        let result = {
          group_key,
          aggregation_type: agg_type,
          value: agg_value,
          timestamp,
          sample_count: group_points.length()
        }
        
        results = results.push(result)
      }
    }
    
    results
  }
  
  // 创建测试数据
  let base_time = 1640995200000
  
  let data_points = [
    create_data_point(base_time, 75.5, "service-a", Map::from([("service", "service-a"), ("environment", "prod"), ("region", "us-west")])),
    create_data_point(base_time + 10000, 80.2, "service-a", Map::from([("service", "service-a"), ("environment", "prod"), ("region", "us-west")])),
    create_data_point(base_time + 20000, 65.8, "service-a", Map::from([("service", "service-a"), ("environment", "prod"), ("region", "us-east")])),
    create_data_point(base_time + 30000, 90.1, "service-b", Map::from([("service", "service-b"), ("environment", "staging"), ("region", "us-west")])),
    create_data_point(base_time + 40000, 70.3, "service-b", Map::from([("service", "service-b"), ("environment", "staging"), ("region", "us-west")])),
    create_data_point(base_time + 50000, 85.7, "service-b", Map::from([("service", "service-b"), ("environment", "prod"), ("region", "us-west")])),
    create_data_point(base_time + 60000, 78.2, "service-c", Map::from([("service", "service-c"), ("environment", "prod"), ("region", "eu-west")])),
    create_data_point(base_time + 70000, 92.4, "service-c", Map::from([("service", "service-c"), ("environment", "prod"), ("region", "eu-west")])),
    create_data_point(base_time + 80000, 68.9, "service-c", Map::from([("service", "service-c"), ("environment", "dev"), ("region", "eu-west")])),
    create_data_point(base_time + 90000, 73.6, "service-c", Map::from([("service", "service-c"), ("environment", "dev"), ("region", "eu-west")])),
  ]
  
  // 创建流处理配置
  let stream_config = {
    group_by_fields: ["service", "environment", "region"],
    aggregation_types: [AggregationType::Sum, AggregationType::Average, AggregationType::Min, AggregationType::Max, AggregationType::Count],
    window_size_ms: 60000,
    emit_interval_ms: 60000
  }
  
  // 处理流式聚合
  let aggregation_results = process_stream_aggregation(data_points, stream_config)
  
  // 验证分组数量
  let unique_groups = aggregation_results.map(fn(r) { r.group_key.service + ":" + r.group_key.environment + ":" + r.group_key.region })
    .reduce(fn(acc, group) { 
      if acc.contains(group) { acc } else { acc.push(group) }
    }, [])
  
  assert_eq(unique_groups.length(), 5) // 应该有5个不同的分组
  
  // 验证service-a:prod:us-west的聚合结果
  let service_a_results = aggregation_results.filter(fn(r) { 
    r.group_key.service == "service-a" && 
    r.group_key.environment == "prod" && 
    r.group_key.region == "us-west" 
  })
  
  assert_eq(service_a_results.length(), 5) // 每个分组应该有5种聚合结果
  
  let sum_result = service_a_results.find(fn(r) { r.aggregation_type == AggregationType::Sum })
  match sum_result {
    Some(result) => assert_eq(result.value.round(), (75.5 + 80.2).round()),
    None => assert_true(false)
  }
  
  let avg_result = service_a_results.find(fn(r) { r.aggregation_type == AggregationType::Average })
  match avg_result {
    Some(result) => assert_eq(result.value.round(), ((75.5 + 80.2) / 2.0).round()),
    None => assert_true(false)
  }
  
  let count_result = service_a_results.find(fn(r) { r.aggregation_type == AggregationType::Count })
  match count_result {
    Some(result) => assert_eq(result.value, 2.0),
    None => assert_true(false)
  }
  
  // 验证service-c:dev:eu-west的聚合结果
  let service_c_dev_results = aggregation_results.filter(fn(r) { 
    r.group_key.service == "service-c" && 
    r.group_key.environment == "dev" && 
    r.group_key.region == "eu-west" 
  })
  
  assert_eq(service_c_dev_results.length(), 5)
  
  let sum_result_c = service_c_dev_results.find(fn(r) { r.aggregation_type == AggregationType::Sum })
  match sum_result_c {
    Some(result) => assert_eq(result.value.round(), (68.9 + 73.6).round()),
    None => assert_true(false)
  }
}

// 测试3: 流式数据过滤与转换
test "流式数据过滤与转换功能" {
  // 定义过滤条件
  enum FilterOperator {
    Equals
    NotEquals
    GreaterThan
    LessThan
    Contains
    StartsWith
    EndsWith
    InRange
  }
  
  // 定义过滤规则
  type FilterRule = {
    field: String
    operator: FilterOperator
    value: String
    is_tag: Bool
  }
  
  // 定义转换操作
  enum TransformOperation {
    Multiply
    Divide
    Add
    Subtract
    Round
    Normalize
    ExtractField
  }
  
  // 定义转换规则
  type TransformRule = {
    target_field: String
    operation: TransformOperation
    operand: Float
    source_field: Option[String]
  }
  
  // 定义流处理管道
  type StreamPipeline = {
    filters: Array[FilterRule]
    transforms: Array[TransformRule]
    output_fields: Array[String]
  }
  
  // 应用过滤条件
  let apply_filter = fn(point: DataPoint, rule: FilterRule) -> Bool {
    let field_value = if rule.is_tag {
      match Map::get(point.tags, rule.field) {
        Some(value) => value,
        None => ""
      }
    } else {
      match rule.field {
        "value" => point.value.to_string(),
        "source" => point.source,
        "timestamp" => point.timestamp.to_string(),
        _ => ""
      }
    }
    
    match rule.operator {
      FilterOperator::Equals => field_value == rule.value,
      FilterOperator::NotEquals => field_value != rule.value,
      FilterOperator::GreaterThan => field_value.to_float() > rule.value.to_float(),
      FilterOperator::LessThan => field_value.to_float() < rule.value.to_float(),
      FilterOperator::Contains => field_value.contains(rule.value),
      FilterOperator::StartsWith => field_value.starts_with(rule.value),
      FilterOperator::EndsWith => field_value.ends_with(rule.value),
      FilterOperator::InRange => {
        let range_parts = rule.value.split(",")
        if range_parts.length() == 2 {
          let min = range_parts[0].to_float()
          let max = range_parts[1].to_float()
          let val = field_value.to_float()
          val >= min && val <= max
        } else {
          false
        }
      }
    }
  }
  
  // 应用转换操作
  let apply_transform = fn(point: DataPoint, rule: TransformRule) -> DataPoint {
    match rule.operation {
      TransformOperation::Multiply => {
        { point | value: point.value * rule.operand }
      },
      TransformOperation::Divide => {
        { point | value: point.value / rule.operand }
      },
      TransformOperation::Add => {
        { point | value: point.value + rule.operand }
      },
      TransformOperation::Subtract => {
        { point | value: point.value - rule.operand }
      },
      TransformOperation::Round => {
        { point | value: point.value.round() }
      },
      TransformOperation::Normalize => {
        // 简化的归一化：将值缩放到0-1范围
        let normalized = if point.value > rule.operand {
          1.0
        } else if point.value < 0.0 {
          0.0
        } else {
          point.value / rule.operand
        }
        { point | value: normalized }
      },
      TransformOperation::ExtractField => {
        match rule.source_field {
          Some(source_field) => {
            match Map::get(point.tags, source_field) {
              Some(value) => {
                // 尝试将标签值转换为数值
                let numeric_value = match value.to_float() {
                  Some(v) => v,
                  None => 0.0
                }
                { point | value: numeric_value }
              },
              None => point
            }
          },
          None => point
        }
      }
    }
  }
  
  // 处理流管道
  let process_stream_pipeline = fn(points: Array[DataPoint], pipeline: StreamPipeline) -> Array[DataPoint] {
    let mut filtered_points = points
    
    // 应用过滤器
    for rule in pipeline.filters {
      filtered_points = filtered_points.filter(fn(point) { apply_filter(point, rule) })
    }
    
    // 应用转换
    let mut transformed_points = []
    for point in filtered_points {
      let mut transformed_point = point
      
      for rule in pipeline.transforms {
        transformed_point = apply_transform(transformed_point, rule)
      }
      
      transformed_points = transformed_points.push(transformed_point)
    }
    
    transformed_points
  }
  
  // 创建测试数据
  let base_time = 1640995200000
  
  let data_points = [
    create_data_point(base_time, 75.5, "service-a", Map::from([("service", "service-a"), ("environment", "prod"), ("region", "us-west"), ("priority", "high")])),
    create_data_point(base_time + 10000, 80.2, "service-a", Map::from([("service", "service-a"), ("environment", "prod"), ("region", "us-west"), ("priority", "low")])),
    create_data_point(base_time + 20000, 65.8, "service-b", Map::from([("service", "service-b"), ("environment", "staging"), ("region", "us-east"), ("priority", "medium")])),
    create_data_point(base_time + 30000, 90.1, "service-b", Map::from([("service", "service-b"), ("environment", "staging"), ("region", "us-west"), ("priority", "high")])),
    create_data_point(base_time + 40000, 70.3, "service-c", Map::from([("service", "service-c"), ("environment", "prod"), ("region", "eu-west"), ("priority", "low")])),
    create_data_point(base_time + 50000, 85.7, "service-c", Map::from([("service", "service-c"), ("environment", "dev"), ("region", "eu-west"), ("priority", "medium")])),
    create_data_point(base_time + 60000, 78.2, "service-d", Map::from([("service", "service-d"), ("environment", "prod"), ("region", "ap-south"), ("priority", "high")])),
    create_data_point(base_time + 70000, 92.4, "service-d", Map::from([("service", "service-d"), ("environment", "prod"), ("region", "ap-south"), ("priority", "low")])),
  ]
  
  // 测试过滤器：只保留高优先级和生产环境的数据点
  let filter_pipeline = {
    filters: [
      { field: "priority", operator: FilterOperator::Equals, value: "high", is_tag: true },
      { field: "environment", operator: FilterOperator::Equals, value: "prod", is_tag: true }
    ],
    transforms: [],
    output_fields: ["timestamp", "value", "source"]
  }
  
  let filtered_results = process_stream_pipeline(data_points, filter_pipeline)
  assert_eq(filtered_results.length(), 2) // 只有两个点满足条件
  
  // 验证过滤结果
  for point in filtered_results {
    assert_eq(Map::get(point.tags, "priority"), Some("high"))
    assert_eq(Map::get(point.tags, "environment"), Some("prod"))
  }
  
  // 测试过滤器：值在特定范围内
  let range_filter_pipeline = {
    filters: [
      { field: "value", operator: FilterOperator::InRange, value: "70,90", is_tag: false }
    ],
    transforms: [],
    output_fields: ["timestamp", "value", "source"]
  }
  
  let range_filtered_results = process_stream_pipeline(data_points, range_filter_pipeline)
  assert_eq(range_filtered_results.length(), 4) // 四个点的值在70-90范围内
  
  // 验证范围过滤结果
  for point in range_filtered_results {
    assert_true(point.value >= 70.0 && point.value <= 90.0)
  }
  
  // 测试转换：将值乘以2并四舍五入
  let transform_pipeline = {
    filters: [],
    transforms: [
      { target_field: "value", operation: TransformOperation::Multiply, operand: 2.0, source_field: None },
      { target_field: "value", operation: TransformOperation::Round, operand: 0.0, source_field: None }
    ],
    output_fields: ["timestamp", "value", "source"]
  }
  
  let transformed_results = process_stream_pipeline(data_points, transform_pipeline)
  assert_eq(transformed_results.length(), data_points.length())
  
  // 验证转换结果
  for i in 0..transformed_results.length() {
    let original_value = data_points[i].value
    let transformed_value = transformed_results[i].value
    assert_eq(transformed_value, (original_value * 2.0).round())
  }
  
  // 测试归一化转换
  let normalize_pipeline = {
    filters: [],
    transforms: [
      { target_field: "value", operation: TransformOperation::Normalize, operand: 100.0, source_field: None }
    ],
    output_fields: ["timestamp", "value", "source"]
  }
  
  let normalized_results = process_stream_pipeline(data_points, normalize_pipeline)
  assert_eq(normalized_results.length(), data_points.length())
  
  // 验证归一化结果
  for point in normalized_results {
    assert_true(point.value >= 0.0 && point.value <= 1.0)
  }
  
  // 测试组合过滤器与转换
  let combined_pipeline = {
    filters: [
      { field: "service", operator: FilterOperator::StartsWith, value: "service-", is_tag: true }
    ],
    transforms: [
      { target_field: "value", operation: TransformOperation::Add, operand: 10.0, source_field: None }
    ],
    output_fields: ["timestamp", "value", "source"]
  }
  
  let combined_results = process_stream_pipeline(data_points, combined_pipeline)
  assert_eq(combined_results.length(), data_points.length()) // 所有服务名都以service-开头
  
  // 验证组合结果
  for i in 0..combined_results.length() {
    let original_value = data_points[i].value
    let transformed_value = combined_results[i].value
    assert_eq(transformed_value, original_value + 10.0)
  }
}

// 测试4: 流式数据异常检测
test "流式数据异常检测功能" {
  // 定义异常类型
  enum AnomalyType {
    Spike        // 突增
    Drop         // 突降
    Outlier      // 离群值
    PatternBreak // 模式破坏
    StaleData    // 数据陈旧
  }
  
  // 定义异常事件
  type AnomalyEvent = {
    timestamp: Int
    anomaly_type: AnomalyType
    severity: String
    description: String
    value: Float
    expected_value: Option[Float]
    confidence: Float
    metadata: Map[String, String]
  }
  
  // 定义异常检测器配置
  type AnomalyDetectorConfig = {
    spike_threshold: Float      // 突增阈值（相对于平均值的倍数）
    drop_threshold: Float       // 突降阈值（相对于平均值的倍数）
    outlier_threshold: Float    // 离群值阈值（标准差的倍数）
    stale_data_threshold_ms: Int // 数据陈旧阈值（毫秒）
    min_samples: Int            // 最小样本数
  }
  
  // 计算移动平均
  let calculate_moving_average = fn(values: Array[Float], window_size: Int) -> Array[Float] {
    if values.length() < window_size {
      return []
    }
    
    let mut result = []
    
    for i in window_size - 1..values.length() {
      let mut sum = 0.0
      for j in (i - window_size + 1)..=i {
        sum = sum + values[j]
      }
      let avg = sum / (window_size as Float)
      result = result.push(avg)
    }
    
    result
  }
  
  // 计算标准差
  let calculate_standard_deviation = fn(values: Array[Float]) -> Float {
    if values.length() == 0 {
      return 0.0
    }
    
    let mean = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
    let sum_squared_diff = values.reduce(fn(acc, v) { 
      let diff = v - mean
      acc + (diff * diff)
    }, 0.0)
    
    (sum_squared_diff / (values.length() as Float)).sqrt()
  }
  
  // 检测突增
  let detect_spike = fn(current_value: Float, historical_values: Array[Float], threshold: Float) -> Bool {
    if historical_values.length() == 0 {
      return false
    }
    
    let historical_avg = historical_values.reduce(fn(acc, v) { acc + v }, 0.0) / (historical_values.length() as Float)
    current_value > (historical_avg * threshold)
  }
  
  // 检测突降
  let detect_drop = fn(current_value: Float, historical_values: Array[Float], threshold: Float) -> Bool {
    if historical_values.length() == 0 {
      return false
    }
    
    let historical_avg = historical_values.reduce(fn(acc, v) { acc + v }, 0.0) / (historical_values.length() as Float)
    current_value < (historical_avg / threshold)
  }
  
  // 检测离群值
  let detect_outlier = fn(current_value: Float, historical_values: Array[Float], threshold: Float) -> Bool {
    if historical_values.length() < 3 {
      return false
    }
    
    let std_dev = calculate_standard_deviation(historical_values)
    let mean = historical_values.reduce(fn(acc, v) { acc + v }, 0.0) / (historical_values.length() as Float)
    
    let z_score = (current_value - mean).abs() / std_dev
    z_score > threshold
  }
  
  // 检测陈旧数据
  let detect_stale_data = fn(current_timestamp: Int, expected_timestamp: Int, threshold_ms: Int) -> Bool {
    (current_timestamp - expected_timestamp) > threshold_ms
  }
  
  // 流式异常检测
  let detect_stream_anomalies = fn(points: Array[DataPoint], config: AnomalyDetectorConfig) -> Array[AnomalyEvent] {
    if points.length() < config.min_samples {
      return []
    }
    
    // 按时间戳排序
    let sorted_points = points.sort_by(fn(a, b) { a.timestamp - b.timestamp })
    let values = sorted_points.map(fn(p) { p.value })
    
    let mut anomalies = []
    
    for i in config.min_samples - 1..sorted_points.length() {
      let current_point = sorted_points[i]
      let current_value = current_point.value
      let historical_values = values.slice(i - (config.min_samples - 1), i)
      
      // 检测突增
      if detect_spike(current_value, historical_values, config.spike_threshold) {
        let anomaly = {
          timestamp: current_point.timestamp,
          anomaly_type: AnomalyType::Spike,
          severity: "high",
          description: "检测到数值突增",
          value: current_value,
          expected_value: Some(historical_values.reduce(fn(acc, v) { acc + v }, 0.0) / (historical_values.length() as Float)),
          confidence: 0.8,
          metadata: Map::from([("source", current_point.source)])
        }
        anomalies = anomalies.push(anomaly)
      }
      
      // 检测突降
      if detect_drop(current_value, historical_values, config.drop_threshold) {
        let anomaly = {
          timestamp: current_point.timestamp,
          anomaly_type: AnomalyType::Drop,
          severity: "medium",
          description: "检测到数值突降",
          value: current_value,
          expected_value: Some(historical_values.reduce(fn(acc, v) { acc + v }, 0.0) / (historical_values.length() as Float)),
          confidence: 0.7,
          metadata: Map::from([("source", current_point.source)])
        }
        anomalies = anomalies.push(anomaly)
      }
      
      // 检测离群值
      if detect_outlier(current_value, historical_values, config.outlier_threshold) {
        let anomaly = {
          timestamp: current_point.timestamp,
          anomaly_type: AnomalyType::Outlier,
          severity: "medium",
          description: "检测到离群值",
          value: current_value,
          expected_value: Some(historical_values.reduce(fn(acc, v) { acc + v }, 0.0) / (historical_values.length() as Float)),
          confidence: 0.6,
          metadata: Map::from([("source", current_point.source)])
        }
        anomalies = anomalies.push(anomaly)
      }
      
      // 检测陈旧数据
      if i > 0 {
        let expected_interval = sorted_points[i].timestamp - sorted_points[i - 1].timestamp
        let expected_timestamp = sorted_points[i - 1].timestamp + expected_interval
        
        if detect_stale_data(current_point.timestamp, expected_timestamp, config.stale_data_threshold_ms) {
          let anomaly = {
            timestamp: current_point.timestamp,
            anomaly_type: AnomalyType::StaleData,
            severity: "low",
            description: "检测到数据陈旧",
            value: current_value,
            expected_value: None,
            confidence: 0.9,
            metadata: Map::from([("source", current_point.source), ("delay", (current_point.timestamp - expected_timestamp).to_string())])
          }
          anomalies = anomalies.push(anomaly)
        }
      }
    }
    
    anomalies
  }
  
  // 创建测试数据
  let base_time = 1640995200000
  
  let data_points = [
    create_data_point(base_time, 75.5, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 10000, 78.2, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 20000, 80.1, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 30000, 76.8, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 40000, 79.3, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 50000, 150.2, "service-a", Map::from([("service", "service-a")])), // 突增
    create_data_point(base_time + 60000, 77.5, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 70000, 25.3, "service-a", Map::from([("service", "service-a")])), // 突降
    create_data_point(base_time + 80000, 78.9, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 90000, 79.2, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 100000, 76.1, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 110000, 77.8, "service-a", Map::from([("service", "service-a")])),
    create_data_point(base_time + 240000, 78.5, "service-a", Map::from([("service", "service-a")])), // 陈旧数据
  ]
  
  // 创建异常检测器配置
  let detector_config = {
    spike_threshold: 1.5,      // 1.5倍于平均值
    drop_threshold: 1.5,       // 1.5倍于平均值
    outlier_threshold: 2.0,    // 2倍标准差
    stale_data_threshold_ms: 120000, // 2分钟
    min_samples: 5
  }
  
  // 检测异常
  let anomalies = detect_stream_anomalies(data_points, detector_config)
  
  // 验证异常检测结果
  assert_eq(anomalies.length(), 3) // 应该检测到3个异常：突增、突降、陈旧数据
  
  // 验证突增异常
  let spike_anomaly = anomalies.find(fn(a) { a.anomaly_type == AnomalyType::Spike })
  match spike_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.value, 150.2)
      assert_eq(anomaly.severity, "high")
    },
    None => assert_true(false)
  }
  
  // 验证突降异常
  let drop_anomaly = anomalies.find(fn(a) { a.anomaly_type == AnomalyType::Drop })
  match drop_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.value, 25.3)
      assert_eq(anomaly.severity, "medium")
    },
    None => assert_true(false)
  }
  
  // 验证陈旧数据异常
  let stale_anomaly = anomalies.find(fn(a) { a.anomaly_type == AnomalyType::StaleData })
  match stale_anomaly {
    Some(anomaly) => {
      assert_eq(anomaly.severity, "low")
      assert_eq(anomaly.confidence, 0.9)
    },
    None => assert_true(false)
  }
}