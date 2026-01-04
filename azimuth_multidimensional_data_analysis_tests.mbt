// Azimuth 多维度数据分析测试用例
// 专注于多维度数据分析、数据透视、OLAP操作等功能

// 测试1: 多维度数据立方体构建与查询
test "多维度数据立方体构建与查询功能" {
  // 定义维度类型
  enum DimensionType {
    Time
    Service
    Region
    Environment
    Operation
    Status
  }
  
  // 定义度量类型
  enum MetricType {
    Count
    Sum
    Average
    Min
    Max
    Percentile
  }
  
  // 定义维度值
  type DimensionValue = {
    dimension: DimensionType
    value: String
    hierarchy_level: Int
  }
  
  // 定义度量值
  type MetricValue = {
    metric: MetricType
    value: Float
    unit: String
  }
  
  // 定义数据立方体单元格
  type CubeCell = {
    dimensions: Array[DimensionValue]
    metrics: Array[MetricValue]
  }
  
  // 定义数据立方体
  type DataCube = {
    name: String
    dimensions: Array[DimensionType]
    metrics: Array[MetricType]
    cells: Array[CubeCell]
  }
  
  // 定义查询条件
  type QueryCondition = {
    dimension: DimensionType
    operator: String
    value: String
  }
  
  // 定义立方体查询
  type CubeQuery = {
    dimensions: Array[DimensionType]
    metrics: Array[MetricType]
    conditions: Array[QueryCondition]
    aggregation_level: Map[DimensionType, Int]
  }
  
  // 创建维度值
  let create_dimension_value = fn(dimension: DimensionType, value: String, hierarchy_level: Int) -> DimensionValue {
    { dimension, value, hierarchy_level }
  }
  
  // 创建度量值
  let create_metric_value = fn(metric: MetricType, value: Float, unit: String) -> MetricValue {
    { metric, value, unit }
  }
  
  // 创建数据立方体单元格
  let create_cube_cell = fn(dimensions: Array[DimensionValue], metrics: Array[MetricValue]) -> CubeCell {
    { dimensions, metrics }
  }
  
  // 查询数据立方体
  let query_data_cube = fn(cube: DataCube, query: CubeQuery) -> Array[CubeCell] {
    // 过滤符合条件的单元格
    let filtered_cells = cube.cells.filter(fn(cell) {
      let mut matches = true
      
      for condition in query.conditions {
        let dimension_value = cell.dimensions.find(fn(d) { d.dimension == condition.dimension })
        
        match dimension_value {
          Some(dim_val) => {
            match condition.operator {
              "equals" => {
                if dim_val.value != condition.value {
                  matches = false
                }
              },
              "contains" => {
                if not(dim_val.value.contains(condition.value)) {
                  matches = false
                }
              },
              "starts_with" => {
                if not(dim_val.value.starts_with(condition.value)) {
                  matches = false
                }
              },
              _ => {
                matches = false
              }
            }
          },
          None => {
            matches = false
          }
        }
        
        if not(matches) {
          break
        }
      }
      
      matches
    })
    
    // 聚合结果
    let mut aggregated_results = []
    let mut processed_groups = []
    
    for cell in filtered_cells {
      // 生成分组键
      let mut group_key = ""
      for dimension in query.dimensions {
        let dim_value = cell.dimensions.find(fn(d) { d.dimension == dimension })
        match dim_value {
          Some(dv) => {
            // 检查聚合级别
            let agg_level = match Map::get(query.aggregation_level, dimension) {
              Some(level) => level,
              None => dv.hierarchy_level
            }
            
            // 根据聚合级别调整值
            let aggregated_value = if agg_level < dv.hierarchy_level {
              // 向上聚合（例如从小时聚合到天）
              match dimension {
                DimensionType::Time => {
                  if dv.value.contains(":") {
                    dv.value.split(":")[0] // 只保留小时部分
                  } else {
                    dv.value
                  }
                },
                _ => dv.value
              }
            } else {
              dv.value
            }
            
            group_key = group_key + dimension.to_string() + ":" + aggregated_value + "|"
          },
          None => {}
        }
      }
      
      // 检查是否已经处理过这个分组
      if not(processed_groups.contains(group_key)) {
        processed_groups = processed_groups.push(group_key)
        
        // 聚合度量值
        let mut aggregated_metrics = []
        
        for metric in query.metrics {
          let metric_values = filtered_cells
            .filter(fn(c) => {
              // 检查是否属于同一分组
              let mut cell_group_key = ""
              for dimension in query.dimensions {
                let dim_value = c.dimensions.find(fn(d) { d.dimension == dimension })
                match dim_value {
                  Some(dv) => {
                    let agg_level = match Map::get(query.aggregation_level, dimension) {
                      Some(level) => level,
                      None => dv.hierarchy_level
                    }
                    
                    let aggregated_value = if agg_level < dv.hierarchy_level {
                      match dimension {
                        DimensionType::Time => {
                          if dv.value.contains(":") {
                            dv.value.split(":")[0]
                          } else {
                            dv.value
                          }
                        },
                        _ => dv.value
                      }
                    } else {
                      dv.value
                    }
                    
                    cell_group_key = cell_group_key + dimension.to_string() + ":" + aggregated_value + "|"
                  },
                  None => {}
                }
              }
              
              cell_group_key == group_key
            })
            .map(fn(c) {
              c.metrics.find(fn(m) { m.metric == metric })
            })
            .filter(fn(m) { 
              match m {
                Some(_) => true,
                None => false
              }
            })
            .map(fn(m) { 
              match m {
                Some(metric_val) => metric_val.value,
                None => 0.0
              }
            })
          
          let aggregated_value = if metric_values.length() > 0 {
            match metric {
              MetricType::Count => metric_values.length() as Float,
              MetricType::Sum => metric_values.reduce(fn(acc, v) { acc + v }, 0.0),
              MetricType::Average => {
                let sum = metric_values.reduce(fn(acc, v) { acc + v }, 0.0)
                sum / (metric_values.length() as Float)
              },
              MetricType::Min => metric_values.reduce(fn(acc, v) { if v < acc { v } else { acc } }, 999999.0),
              MetricType::Max => metric_values.reduce(fn(acc, v) { if v > acc { v } else { acc } }, 0.0),
              MetricType::Percentile => {
                // 简化的95百分位计算
                let sorted = metric_values.sort_by(fn(a, b) { a - b })
                let index = ((sorted.length() as Float) * 0.95) as Int
                if index < sorted.length() {
                  sorted[index]
                } else {
                  sorted[sorted.length() - 1]
                }
              }
            }
          } else {
            0.0
          }
          
          let unit = match metric {
            MetricType::Count => "count",
            MetricType::Sum => "total",
            MetricType::Average => "avg",
            MetricType::Min => "min",
            MetricType::Max => "max",
            MetricType::Percentile => "p95"
          }
          
          aggregated_metrics = aggregated_metrics.push(create_metric_value(metric, aggregated_value, unit))
        }
        
        // 创建聚合后的维度
        let aggregated_dimensions = []
        for dimension in query.dimensions {
          let dim_value = cell.dimensions.find(fn(d) { d.dimension == dimension })
          match dim_value {
            Some(dv) => {
              let agg_level = match Map::get(query.aggregation_level, dimension) {
                Some(level) => level,
                None => dv.hierarchy_level
              }
              
              let aggregated_value = if agg_level < dv.hierarchy_level {
                match dimension {
                  DimensionType::Time => {
                    if dv.value.contains(":") {
                      dv.value.split(":")[0]
                    } else {
                      dv.value
                    }
                  },
                  _ => dv.value
                }
              } else {
                dv.value
              }
              
              aggregated_dimensions = aggregated_dimensions.push(
                create_dimension_value(dimension, aggregated_value, agg_level)
              )
            },
            None => {}
          }
        }
        
        let aggregated_cell = create_cube_cell(aggregated_dimensions, aggregated_metrics)
        aggregated_results = aggregated_results.push(aggregated_cell)
      }
    }
    
    aggregated_results
  }
  
  // 创建测试数据立方体
  let time_dims = [
    create_dimension_value(DimensionType::Time, "2022-01-01", 0),
    create_dimension_value(DimensionType::Time, "2022-01-01:09", 1),
    create_dimension_value(DimensionType::Time, "2022-01-01:10", 1),
    create_dimension_value(DimensionType::Time, "2022-01-01:11", 1),
    create_dimension_value(DimensionType::Time, "2022-01-02", 0),
    create_dimension_value(DimensionType::Time, "2022-01-02:09", 1),
    create_dimension_value(DimensionType::Time, "2022-01-02:10", 1),
    create_dimension_value(DimensionType::Time, "2022-01-02:11", 1)
  ]
  
  let service_dims = [
    create_dimension_value(DimensionType::Service, "auth-service", 0),
    create_dimension_value(DimensionType::Service, "payment-service", 0),
    create_dimension_value(DimensionType::Service, "user-service", 0)
  ]
  
  let region_dims = [
    create_dimension_value(DimensionType::Region, "us-east", 0),
    create_dimension_value(DimensionType::Region, "us-west", 0),
    create_dimension_value(DimensionType::Region, "eu-west", 0)
  ]
  
  let env_dims = [
    create_dimension_value(DimensionType::Environment, "prod", 0),
    create_dimension_value(DimensionType::Environment, "staging", 0)
  ]
  
  // 创建立方体单元格
  let cells = [
    // 2022-01-01 auth-service us-east prod
    create_cube_cell(
      [time_dims[1], service_dims[0], region_dims[0], env_dims[0]],
      [
        create_metric_value(MetricType::Count, 100.0, "count"),
        create_metric_value(MetricType::Sum, 10000.0, "ms"),
        create_metric_value(MetricType::Average, 100.0, "ms"),
        create_metric_value(MetricType::Min, 50.0, "ms"),
        create_metric_value(MetricType::Max, 200.0, "ms")
      ]
    ),
    
    // 2022-01-01 auth-service us-west prod
    create_cube_cell(
      [time_dims[1], service_dims[0], region_dims[1], env_dims[0]],
      [
        create_metric_value(MetricType::Count, 150.0, "count"),
        create_metric_value(MetricType::Sum, 18000.0, "ms"),
        create_metric_value(MetricType::Average, 120.0, "ms"),
        create_metric_value(MetricType::Min, 60.0, "ms"),
        create_metric_value(MetricType::Max, 250.0, "ms")
      ]
    ),
    
    // 2022-01-01 payment-service us-east prod
    create_cube_cell(
      [time_dims[1], service_dims[1], region_dims[0], env_dims[0]],
      [
        create_metric_value(MetricType::Count, 80.0, "count"),
        create_metric_value(MetricType::Sum, 16000.0, "ms"),
        create_metric_value(MetricType::Average, 200.0, "ms"),
        create_metric_value(MetricType::Min, 100.0, "ms"),
        create_metric_value(MetricType::Max, 400.0, "ms")
      ]
    ),
    
    // 2022-01-02 auth-service us-east prod
    create_cube_cell(
      [time_dims[5], service_dims[0], region_dims[0], env_dims[0]],
      [
        create_metric_value(MetricType::Count, 120.0, "count"),
        create_metric_value(MetricType::Sum, 10800.0, "ms"),
        create_metric_value(MetricType::Average, 90.0, "ms"),
        create_metric_value(MetricType::Min, 40.0, "ms"),
        create_metric_value(MetricType::Max, 180.0, "ms")
      ]
    ),
    
    // 2022-01-02 payment-service us-east staging
    create_cube_cell(
      [time_dims[5], service_dims[1], region_dims[0], env_dims[1]],
      [
        create_metric_value(MetricType::Count, 60.0, "count"),
        create_metric_value(MetricType::Sum, 15000.0, "ms"),
        create_metric_value(MetricType::Average, 250.0, "ms"),
        create_metric_value(MetricType::Min, 150.0, "ms"),
        create_metric_value(MetricType::Max, 500.0, "ms")
      ]
    )
  ]
  
  let data_cube = {
    name: "telemetry_metrics",
    dimensions: [DimensionType::Time, DimensionType::Service, DimensionType::Region, DimensionType::Environment],
    metrics: [MetricType::Count, MetricType::Sum, MetricType::Average, MetricType::Min, MetricType::Max],
    cells
  }
  
  // 测试查询1: 查询特定服务的总计数
  let service_query = {
    dimensions: [DimensionType::Service],
    metrics: [MetricType::Count, MetricType::Average],
    conditions: [
      { dimension: DimensionType::Service, operator: "equals", value: "auth-service" }
    ],
    aggregation_level: Map::empty()
  }
  
  let service_results = query_data_cube(data_cube, service_query)
  assert_eq(service_results.length(), 1)
  
  let service_result = service_results[0]
  assert_eq(service_result.dimensions.length(), 1)
  assert_eq(service_result.dimensions[0].value, "auth-service")
  
  // 验证聚合后的计数 (100 + 150 + 120 = 370)
  let count_metric = service_result.metrics.find(fn(m) { m.metric == MetricType::Count })
  match count_metric {
    Some(metric) => assert_eq(metric.value, 370.0),
    None => assert_true(false)
  }
  
  // 验证聚合后的平均值 ((100 + 120 + 90) / 3 = 103.33)
  let avg_metric = service_result.metrics.find(fn(m) { m.metric == MetricType::Average })
  match avg_metric {
    Some(metric) => assert_eq(metric.value.round(), 103.0),
    None => assert_true(false)
  }
  
  // 测试查询2: 按天聚合时间维度
  let daily_query = {
    dimensions: [DimensionType::Time, DimensionType::Service],
    metrics: [MetricType::Count, MetricType::Sum],
    conditions: [],
    aggregation_level: Map::from([(DimensionType::Time, 0)]) // 聚合到天级别
  }
  
  let daily_results = query_data_cube(data_cube, daily_query)
  assert_eq(daily_results.length(), 4) // 2天 × 2个服务
  
  // 验证2022-01-01 auth-service的聚合结果 (100 + 150 = 250)
  let day1_auth = daily_results.find(fn(r) => {
    r.dimensions.find(fn(d) => d.dimension == DimensionType::Time && d.value == "2022-01-01") != None &&
    r.dimensions.find(fn(d) => d.dimension == DimensionType::Service && d.value == "auth-service") != None
  })
  
  match day1_auth {
    Some(result) => {
      let count_metric = result.metrics.find(fn(m) { m.metric == MetricType::Count })
      match count_metric {
        Some(metric) => assert_eq(metric.value, 250.0),
        None => assert_true(false)
      }
    },
    None => assert_true(false)
  }
  
  // 测试查询3: 多条件查询
  let multi_condition_query = {
    dimensions: [DimensionType::Service, DimensionType::Region],
    metrics: [MetricType::Count, MetricType::Max],
    conditions: [
      { dimension: DimensionType::Environment, operator: "equals", value: "prod" },
      { dimension: DimensionType::Region, operator: "equals", value: "us-east" }
    ],
    aggregation_level: Map::empty()
  }
  
  let multi_condition_results = query_data_cube(data_cube, multi_condition_query)
  assert_eq(multi_condition_results.length(), 2) // auth-service和payment-service在us-east prod
}

// 测试2: 数据透视表与交叉分析
test "数据透视表与交叉分析功能" {
  // 定义透视表结构
  type PivotTable = {
    row_dimension: String
    column_dimension: String
    value_dimension: String
    aggregation_function: String
    data: Array[Array[Float]]
    row_labels: Array[String]
    column_labels: Array[String]
    grand_total: Float
  }
  
  // 定义交叉分析结果
  type CrossAnalysisResult = {
    dimension1: String
    dimension2: String
    correlation_coefficient: Float
    trend_direction: String
    significance_level: Float
    outlier_count: Int
  }
  
  // 创建透视表
  let create_pivot_table = fn(data: Array[Map[String, String]], row_dim: String, col_dim: String, value_dim: String, agg_func: String) -> PivotTable {
    // 获取所有唯一的行和列标签
    let mut row_labels = []
    let mut column_labels = []
    
    for record in data {
      let row_value = match Map::get(record, row_dim) {
        Some(value) => value,
        None => ""
      }
      
      let col_value = match Map::get(record, col_dim) {
        Some(value) => value,
        None => ""
      }
      
      if not(row_labels.contains(row_value)) {
        row_labels = row_labels.push(row_value)
      }
      
      if not(column_labels.contains(col_value)) {
        column_labels = column_labels.push(col_value)
      }
    }
    
    // 初始化数据矩阵
    let mut pivot_data = []
    for i in 0..row_labels.length() {
      let mut row = []
      for j in 0..column_labels.length() {
        row = row.push(0.0)
      }
      pivot_data = pivot_data.push(row)
    }
    
    // 填充数据矩阵
    let mut grand_total = 0.0
    
    for record in data {
      let row_value = match Map::get(record, row_dim) {
        Some(value) => value,
        None => ""
      }
      
      let col_value = match Map::get(record, col_dim) {
        Some(value) => value,
        None => ""
      }
      
      let value_str = match Map::get(record, value_dim) {
        Some(value) => value,
        None => "0"
      }
      
      let numeric_value = value_str.to_float()
      
      let row_index = row_labels.find_index(fn(label) { label == row_value })
      let col_index = column_labels.find_index(fn(label) { label == col_value })
      
      match (row_index, col_index) {
        (Some(r), Some(c)) => {
          let current_value = pivot_data[r][c]
          let new_value = match agg_func {
            "sum" => current_value + numeric_value,
            "count" => current_value + 1.0,
            "avg" => {
              // 对于平均值，我们需要跟踪计数和总和
              // 这里简化处理，假设每个记录只出现一次
              (current_value + numeric_value) / 2.0
            },
            "min" => {
              if current_value == 0.0 || numeric_value < current_value {
                numeric_value
              } else {
                current_value
              }
            },
            "max" => {
              if numeric_value > current_value {
                numeric_value
              } else {
                current_value
              }
            },
            _ => current_value + numeric_value
          }
          
          pivot_data = pivot_data.update(r, pivot_data[r].update(c, new_value))
          grand_total = grand_total + numeric_value
        },
        _ => {}
      }
    }
    
    {
      row_dimension: row_dim,
      column_dimension: col_dim,
      value_dimension: value_dim,
      aggregation_function: agg_func,
      data: pivot_data,
      row_labels,
      column_labels,
      grand_total
    }
  }
  
  // 计算相关系数
  let calculate_correlation = fn(x_values: Array[Float], y_values: Array[Float]) -> Float {
    if x_values.length() != y_values.length() || x_values.length() == 0 {
      return 0.0
    }
    
    let n = x_values.length() as Float
    let sum_x = x_values.reduce(fn(acc, x) { acc + x }, 0.0)
    let sum_y = y_values.reduce(fn(acc, y) { acc + y }, 0.0)
    let sum_xy = x_values.reduce(fn(acc, x, i) { acc + x * y_values[i] }, 0.0)
    let sum_x2 = x_values.reduce(fn(acc, x) { acc + x * x }, 0.0)
    let sum_y2 = y_values.reduce(fn(acc, y) { acc + y * y }, 0.0)
    
    let numerator = n * sum_xy - sum_x * sum_y
    let denominator = ((n * sum_x2 - sum_x * sum_x) * (n * sum_y2 - sum_y * sum_y)).sqrt()
    
    if denominator == 0.0 {
      0.0
    } else {
      numerator / denominator
    }
  }
  
  // 交叉分析
  let perform_cross_analysis = fn(data: Array[Map[String, String]], dim1: String, dim2: String, value_dim: String) -> CrossAnalysisResult {
    // 按维度1分组
    let mut dim1_groups = Map::empty()
    
    for record in data {
      let dim1_value = match Map::get(record, dim1) {
        Some(value) => value,
        None => ""
      }
      
      let dim2_value = match Map::get(record, dim2) {
        Some(value) => value,
        None => ""
      }
      
      let value_str = match Map::get(record, value_dim) {
        Some(value) => value,
        None => "0"
      }
      
      let numeric_value = value_str.to_float()
      
      let current_values = match Map::get(dim1_groups, dim1_value) {
        Some(values) => values,
        None => []
      }
      
      dim1_groups = Map::insert(dim1_groups, dim1_value, current_values.push(numeric_value))
    }
    
    // 获取所有维度2的值
    let mut dim2_values = []
    for record in data {
      let dim2_value = match Map::get(record, dim2) {
        Some(value) => value,
        None => ""
      }
      
      if not(dim2_values.contains(dim2_value)) {
        dim2_values = dim2_values.push(dim2_value)
      }
    }
    
    // 计算每个维度2值在所有维度1组中的平均值
    let mut dim1_averages = []
    let mut dim2_averages = []
    
    for dim1_value in dim1_groups.keys() {
      let values = match Map::get(dim1_groups, dim1_value) {
        Some(values) => values,
        None => []
      }
      
      if values.length() > 0 {
        let avg = values.reduce(fn(acc, v) { acc + v }, 0.0) / (values.length() as Float)
        dim1_averages = dim1_averages.push(avg)
      }
    }
    
    for dim2_value in dim2_values {
      let mut dim2_sum = 0.0
      let mut dim2_count = 0
      
      for record in data {
        let record_dim2 = match Map::get(record, dim2) {
          Some(value) => value,
          None => ""
        }
        
        if record_dim2 == dim2_value {
          let value_str = match Map::get(record, value_dim) {
            Some(value) => value,
            None => "0"
          }
          
          dim2_sum = dim2_sum + value_str.to_float()
          dim2_count = dim2_count + 1
        }
      }
      
      if dim2_count > 0 {
        let avg = dim2_sum / (dim2_count as Float)
        dim2_averages = dim2_averages.push(avg)
      }
    }
    
    // 计算相关系数
    let correlation = calculate_correlation(dim1_averages, dim2_averages)
    
    // 确定趋势方向
    let trend_direction = if correlation > 0.3 {
      "positive"
    } else if correlation < -0.3 {
      "negative"
    } else {
      "neutral"
    }
    
    // 简化的显著性水平计算
    let significance_level = if correlation.abs() > 0.7 {
      0.95
    } else if correlation.abs() > 0.5 {
      0.8
    } else if correlation.abs() > 0.3 {
      0.6
    } else {
      0.4
    }
    
    // 简化的异常值计数
    let all_values = dim1_averages + dim2_averages
    let mean = all_values.reduce(fn(acc, v) { acc + v }, 0.0) / (all_values.length() as Float)
    let variance = all_values.reduce(fn(acc, v) { 
      let diff = v - mean
      acc + (diff * diff)
    }, 0.0) / (all_values.length() as Float)
    let std_dev = variance.sqrt()
    
    let outlier_count = all_values.reduce(fn(acc, v) => {
      if (v - mean).abs() > 2.0 * std_dev {
        acc + 1
      } else {
        acc
      }
    }, 0)
    
    {
      dimension1: dim1,
      dimension2: dim2,
      correlation_coefficient: correlation,
      trend_direction,
      significance_level,
      outlier_count
    }
  }
  
  // 创建测试数据
  let test_data = [
    Map::from([("service", "auth-service"), ("region", "us-east"), ("response_time", "120")]),
    Map::from([("service", "auth-service"), ("region", "us-east"), ("response_time", "150")]),
    Map::from([("service", "auth-service"), ("region", "us-west"), ("response_time", "100")]),
    Map::from([("service", "auth-service"), ("region", "us-west"), ("response_time", "130")]),
    Map::from([("service", "payment-service"), ("region", "us-east"), ("response_time", "200")]),
    Map::from([("service", "payment-service"), ("region", "us-east"), ("response_time", "250")]),
    Map::from([("service", "payment-service"), ("region", "eu-west"), ("response_time", "180")]),
    Map::from([("service", "payment-service"), ("region", "eu-west"), ("response_time", "220")]),
    Map::from([("service", "user-service"), ("region", "us-east"), ("response_time", "80")]),
    Map::from([("service", "user-service"), ("region", "us-west"), ("response_time", "90")]),
    Map::from([("service", "user-service"), ("region", "eu-west"), ("response_time", "85")]),
    Map::from([("service", "user-service"), ("region", "eu-west"), ("response_time", "95")])
  ]
  
  // 测试透视表创建
  let pivot_table = create_pivot_table(test_data, "service", "region", "response_time", "avg")
  
  // 验证透视表结构
  assert_eq(pivot_table.row_dimension, "service")
  assert_eq(pivot_table.column_dimension, "region")
  assert_eq(pivot_table.value_dimension, "response_time")
  assert_eq(pivot_table.aggregation_function, "avg")
  
  // 验证行标签
  assert_eq(pivot_table.row_labels.length(), 3) // auth-service, payment-service, user-service
  assert_true(pivot_table.row_labels.contains("auth-service"))
  assert_true(pivot_table.row_labels.contains("payment-service"))
  assert_true(pivot_table.row_labels.contains("user-service"))
  
  // 验证列标签
  assert_eq(pivot_table.column_labels.length(), 3) // us-east, us-west, eu-west
  assert_true(pivot_table.column_labels.contains("us-east"))
  assert_true(pivot_table.column_labels.contains("us-west"))
  assert_true(pivot_table.column_labels.contains("eu-west"))
  
  // 验证数据矩阵
  assert_eq(pivot_table.data.length(), 3) // 3行
  assert_eq(pivot_table.data[0].length(), 3) // 3列
  
  // 验证特定单元格的值 (auth-service, us-east) 的平均响应时间 (120 + 150) / 2 = 135
  let auth_service_index = pivot_table.row_labels.find_index(fn(label) { label == "auth-service" })
  let us_east_index = pivot_table.column_labels.find_index(fn(label) { label == "us-east" })
  
  match (auth_service_index, us_east_index) {
    (Some(r), Some(c)) => {
      assert_eq(pivot_table.data[r][c].round(), 135.0)
    },
    _ => assert_true(false)
  }
  
  // 测试交叉分析
  let cross_analysis = perform_cross_analysis(test_data, "service", "region", "response_time")
  
  // 验证交叉分析结果
  assert_eq(cross_analysis.dimension1, "service")
  assert_eq(cross_analysis.dimension2, "region")
  assert_true(cross_analysis.correlation_coefficient >= -1.0 && cross_analysis.correlation_coefficient <= 1.0)
  assert_true(cross_analysis.significance_level >= 0.0 && cross_analysis.significance_level <= 1.0)
  assert_true(cross_analysis.outlier_count >= 0)
  
  // 验证趋势方向
  assert_true(
    cross_analysis.trend_direction == "positive" ||
    cross_analysis.trend_direction == "negative" ||
    cross_analysis.trend_direction == "neutral"
  )
}

// 测试3: 多维度数据钻取与切片
test "多维度数据钻取与切片功能" {
  // 定义钻取操作类型
  enum DrillOperation {
    Down    // 向下钻取（更详细）
    Up      // 向上钻取（更聚合）
    Through // 钻透到相关维度
  }
  
  // 定义切片操作
  type SliceOperation = {
    dimension: String
    value: String
    include: Bool
  }
  
  // 定义数据钻取配置
  type DrillConfig = {
    operation: DrillOperation
    target_dimension: String
    current_level: Int
    target_level: Int
    filters: Array[SliceOperation]
  }
  
  // 定义数据切片配置
  type SliceConfig = {
    dimension: String
    values: Array[String]
    operation: String  // "include", "exclude"
  }
  
  // 定义层次结构
  type HierarchyLevel = {
    level: Int
    name: String
    format: String
    parent_level: Option[Int]
  }
  
  // 定义维度层次结构
  type DimensionHierarchy = {
    dimension: String
    levels: Array[HierarchyLevel]
  }
  
  // 模拟层次结构定义
  let time_hierarchy = {
    dimension: "time",
    levels: [
      { level: 0, name: "year", format: "YYYY", parent_level: None },
      { level: 1, name: "quarter", format: "YYYY-Q[1-4]", parent_level: Some(0) },
      { level: 2, name: "month", format: "YYYY-MM", parent_level: Some(1) },
      { level: 3, name: "day", format: "YYYY-MM-DD", parent_level: Some(2) },
      { level: 4, name: "hour", format: "YYYY-MM-DD:HH", parent_level: Some(3) }
    ]
  }
  
  let service_hierarchy = {
    dimension: "service",
    levels: [
      { level: 0, name: "category", format: "*", parent_level: None },
      { level: 1, name: "service_group", format: "*", parent_level: Some(0) },
      { level: 2, name: "service_name", format: "*", parent_level: Some(1) },
      { level: 3, name: "operation", format: "*", parent_level: Some(2) }
    ]
  }
  
  // 数据钻取操作
  let drill_data = fn(data: Array[Map[String, String]], config: DrillConfig, hierarchies: Array[DimensionHierarchy]) -> Array[Map[String, String]] {
    // 查找目标维度的层次结构
    let target_hierarchy = hierarchies.find(fn(h) { h.dimension == config.target_dimension })
    
    match target_hierarchy {
      Some(hierarchy) => {
        // 执行钻取操作
        match config.operation {
          DrillOperation::Down => {
            // 向下钻取：从当前级别到更详细级别
            if config.current_level < config.target_level && config.target_level < hierarchy.levels.length() {
              let target_level_info = hierarchy.levels[config.target_level]
              let current_level_info = hierarchy.levels[config.current_level]
              
              // 简化处理：为每个当前值生成更详细的值
              data.map(fn(record) => {
                let current_value = match Map::get(record, config.target_dimension) {
                  Some(value) => value,
                  None => ""
                }
                
                // 生成更详细的值
                let detailed_value = match config.target_dimension {
                  "time" => {
                    if config.target_level == 3 && config.current_level == 2 { // 从月到天
                      current_value + "-01" // 添加月份的第一天
                    } else if config.target_level == 4 && config.current_level == 3 { // 从天到小时
                      current_value + ":09" // 添加上午9点
                    } else {
                      current_value
                    }
                  },
                  "service" => {
                    if config.target_level == 3 && config.current_level == 2 { // 从服务名到操作
                      current_value + ".login"
                    } else {
                      current_value
                    }
                  },
                  _ => current_value
                }
                
                let mut updated_record = record
                updated_record = Map::insert(updated_record, config.target_dimension, detailed_value)
                updated_record
              })
            } else {
              data
            }
          },
          DrillOperation::Up => {
            // 向上钻取：从当前级别到更聚合级别
            if config.current_level > config.target_level && config.target_level >= 0 {
              // 简化处理：将详细值聚合到更高级别
              data.map(fn(record) => {
                let current_value = match Map::get(record, config.target_dimension) {
                  Some(value) => value,
                  None => ""
                }
                
                // 生成聚合值
                let aggregated_value = match config.target_dimension {
                  "time" => {
                    if config.target_level == 2 && config.current_level == 3 { // 从天到月
                      current_value.substring(0, 7) // 只保留年月部分
                    } else if config.target_level == 1 && config.current_level == 2 { // 从月到季度
                      let month = current_value.substring(5, 2).to_int()
                      let quarter = if month <= 3 { "Q1" } else if month <= 6 { "Q2" } else if month <= 9 { "Q3" } else { "Q4" }
                      current_value.substring(0, 4) + "-" + quarter
                    } else {
                      current_value
                    }
                  },
                  "service" => {
                    if config.target_level == 1 && config.current_level == 2 { // 从服务名到服务组
                      if current_value.contains("auth") || current_value.contains("user") {
                        "identity-services"
                      } else if current_value.contains("payment") || current_value.contains("billing") {
                        "financial-services"
                      } else {
                        "other-services"
                      }
                    } else {
                      current_value
                    }
                  },
                  _ => current_value
                }
                
                let mut updated_record = record
                updated_record = Map::insert(updated_record, config.target_dimension, aggregated_value)
                updated_record
              })
            } else {
              data
            }
          },
          DrillOperation::Through => {
            // 钻透操作：添加相关维度
            data.map(fn(record) => {
              let current_value = match Map::get(record, config.target_dimension) {
                Some(value) => value,
                None => ""
              }
              
              // 添加相关维度
              let related_dimension = match config.target_dimension {
                "service" => "operation",
                "operation" => "service",
                "time" => "region",
                "region" => "time",
                _ => "unknown"
              }
              
              let related_value = match related_dimension {
                "operation" => current_value + ".operation",
                "service" => if current_value.contains("login") { "auth-service" } else { "unknown-service" },
                "region" => "us-east",
                "time" => "2022-01-01",
                _ => "unknown"
              }
              
              let mut updated_record = record
              updated_record = Map::insert(updated_record, related_dimension, related_value)
              updated_record
            })
          }
        }
      },
      None => data
    }
  }
  
  // 数据切片操作
  let slice_data = fn(data: Array[Map[String, String]], config: SliceConfig) -> Array[Map[String, String]] {
    match config.operation {
      "include" => {
        // 只包含指定值
        data.filter(fn(record) => {
          let value = match Map::get(record, config.dimension) {
            Some(v) => v,
            None => ""
          }
          
          config.values.contains(value)
        })
      },
      "exclude" => {
        // 排除指定值
        data.filter(fn(record) => {
          let value = match Map::get(record, config.dimension) {
            Some(v) => v,
            None => ""
          }
          
          not(config.values.contains(value))
        })
      },
      _ => data
    }
  }
  
  // 创建测试数据
  let test_data = [
    Map::from([("time", "2022-01-01"), ("service", "auth-service"), ("region", "us-east"), ("response_time", "120")]),
    Map::from([("time", "2022-01-01"), ("service", "auth-service"), ("region", "us-west"), ("response_time", "100")]),
    Map::from([("time", "2022-01-02"), ("service", "payment-service"), ("region", "us-east"), ("response_time", "200")]),
    Map::from([("time", "2022-01-02"), ("service", "payment-service"), ("region", "eu-west"), ("response_time", "180")]),
    Map::from([("time", "2022-01-03"), ("service", "user-service"), ("region", "us-east"), ("response_time", "80")]),
    Map::from([("time", "2022-01-03"), ("service", "user-service"), ("region", "us-west"), ("response_time", "90")])
  ]
  
  let hierarchies = [time_hierarchy, service_hierarchy]
  
  // 测试向下钻取：从天到小时
  let drill_down_config = {
    operation: DrillOperation::Down,
    target_dimension: "time",
    current_level: 3,  // 天级别
    target_level: 4,   // 小时级别
    filters: []
  }
  
  let drilled_down_data = drill_data(test_data, drill_down_config, hierarchies)
  
  // 验证向下钻取结果
  assert_eq(drilled_down_data.length(), test_data.length())
  
  for record in drilled_down_data {
    let time_value = match Map::get(record, "time") {
      Some(value) => value,
      None => ""
    }
    
    // 验证时间值现在包含小时部分
    assert_true(time_value.contains(":"))
  }
  
  // 测试向上钻取：从天到月
  let drill_up_config = {
    operation: DrillOperation::Up,
    target_dimension: "time",
    current_level: 3,  // 天级别
    target_level: 2,   // 月级别
    filters: []
  }
  
  let drilled_up_data = drill_data(test_data, drill_up_config, hierarchies)
  
  // 验证向上钻取结果
  assert_eq(drilled_up_data.length(), test_data.length())
  
  for record in drilled_up_data {
    let time_value = match Map::get(record, "time") {
      Some(value) => value,
      None => ""
    }
    
    // 验证时间值现在是月份格式 (YYYY-MM)
    assert_eq(time_value.length(), 7)
    assert_true(time_value.contains("-"))
  }
  
  // 测试钻透操作
  let drill_through_config = {
    operation: DrillOperation::Through,
    target_dimension: "service",
    current_level: 2,
    target_level: 2,
    filters: []
  }
  
  let drilled_through_data = drill_data(test_data, drill_through_config, hierarchies)
  
  // 验证钻透结果
  assert_eq(drilled_through_data.length(), test_data.length())
  
  for record in drilled_through_data {
    // 验证添加了相关维度
    assert_true(Map::contains_key(record, "operation"))
  }
  
  // 测试切片操作：包含
  let include_slice_config = {
    dimension: "service",
    values: ["auth-service", "payment-service"],
    operation: "include"
  }
  
  let sliced_include_data = slice_data(test_data, include_slice_config)
  
  // 验证包含切片结果
  assert_eq(sliced_include_data.length(), 4) // 只包含auth-service和payment-service的记录
  
  for record in sliced_include_data {
    let service_value = match Map::get(record, "service") {
      Some(value) => value,
      None => ""
    }
    
    assert_true(service_value == "auth-service" || service_value == "payment-service")
  }
  
  // 测试切片操作：排除
  let exclude_slice_config = {
    dimension: "region",
    values: ["us-east"],
    operation: "exclude"
  }
  
  let sliced_exclude_data = slice_data(test_data, exclude_slice_config)
  
  // 验证排除切片结果
  assert_eq(sliced_exclude_data.length(), 2) // 排除了us-east的记录
  
  for record in sliced_exclude_data {
    let region_value = match Map::get(record, "region") {
      Some(value) => value,
      None => ""
    }
    
    assert_true(region_value != "us-east")
  }
  
  // 测试组合操作：先钻取再切片
  let combined_data = drill_data(test_data, drill_up_config, hierarchies)
  let final_data = slice_data(combined_data, include_slice_config)
  
  // 验证组合操作结果
  assert_eq(final_data.length(), 4) // 钻取后有6条记录，切片后剩下4条
  
  for record in final_data {
    let time_value = match Map::get(record, "time") {
      Some(value) => value,
      None => ""
    }
    
    let service_value = match Map::get(record, "service") {
      Some(value) => value,
      None => ""
    }
    
    // 验证时间值是月份格式
    assert_eq(time_value.length(), 7)
    
    // 验证服务值是指定的服务
    assert_true(service_value == "auth-service" || service_value == "payment-service")
  }
}