// Azimuth 数据处理管道测试用例
// 专注于数据处理管道、流处理和ETL操作

// 测试1: 基础数据处理管道
test "基础数据处理管道" {
  // 定义数据处理阶段
  enum ProcessingStage {
    Input
    Transform
    Filter
    Aggregate
    Output
  }
  
  // 定义数据记录
  type DataRecord = {
    id: String,
    data: String,
    timestamp: Int,
    metadata: Array[(String, String)]
  }
  
  // 定义管道阶段
  type PipelineStage = {
    name: String,
    stage_type: ProcessingStage,
    processor: (DataRecord) -> DataRecord,
    filter: (DataRecord) -> Bool
  }
  
  // 定义数据管道
  type DataPipeline = {
    name: String,
    stages: Array[PipelineStage],
    input_data: Array[DataRecord],
    output_data: Array[DataRecord],
    processed_count: Int,
    error_count: Int
  }
  
  // 创建数据管道
  let create_pipeline = fn(name: String) {
    {
      name,
      stages: [],
      input_data: [],
      output_data: [],
      processed_count: 0,
      error_count: 0
    }
  }
  
  // 添加管道阶段
  let add_stage = fn(pipeline: DataPipeline, name: String, stage_type: ProcessingStage, processor: (DataRecord) -> DataRecord, filter: (DataRecord) -> Bool) {
    let stage = {
      name,
      stage_type,
      processor,
      filter
    }
    pipeline.stages = pipeline.stages.push(stage)
  }
  
  // 添加输入数据
  let add_input_data = fn(pipeline: DataPipeline, records: Array[DataRecord>) {
    pipeline.input_data = pipeline.input_data + records
  }
  
  // 执行管道处理
  let execute_pipeline = fn(pipeline: DataPipeline) {
    let mut current_data = pipeline.input_data
    pipeline.processed_count = 0
    pipeline.error_count = 0
    
    for stage in pipeline.stages {
      let mut next_data = []
      
      for record in current_data {
        // 应用过滤器
        if stage.filter(record) {
          // 应用处理器
          let processed_record = stage.processor(record)
          next_data = next_data.push(processed_record)
          pipeline.processed_count = pipeline.processed_count + 1
        } else {
          pipeline.error_count = pipeline.error_count + 1
        }
      }
      
      current_data = next_data
    }
    
    pipeline.output_data = current_data
  }
  
  // 测试数据处理管道
  let pipeline = create_pipeline("test_pipeline")
  
  // 添加管道阶段
  add_stage(pipeline, "input_validation", ProcessingStage::Input, 
    fn(record) { { record | metadata: record.metadata.push(("validated", "true")) } },
    fn(record) { record.data.length() > 0 })
  
  add_stage(pipeline, "data_transform", ProcessingStage::Transform,
    fn(record) { { record | data: record.data.to_uppercase() } },
    fn(record) { true })
  
  add_stage(pipeline, "content_filter", ProcessingStage::Filter,
    fn(record) { record },
    fn(record) { not(record.data.contains("INVALID")) })
  
  // 添加测试数据
  let test_records = [
    { id: "1", data: "test data", timestamp: 1640995200, metadata: [] },
    { id: "2", data: "more data", timestamp: 1640995201, metadata: [] },
    { id: "3", data: "INVALID data", timestamp: 1640995202, metadata: [] },
    { id: "4", data: "", timestamp: 1640995203, metadata: [] },  // 空数据，会被过滤
    { id: "5", data: "final data", timestamp: 1640995204, metadata: [] }
  ]
  
  add_input_data(pipeline, test_records)
  
  // 执行管道
  execute_pipeline(pipeline)
  
  // 验证结果
  assert_eq(pipeline.output_data.length(), 3)  // 5个输入 - 1个空数据 - 1个INVALID数据
  assert_eq(pipeline.processed_count, 3)
  assert_eq(pipeline.error_count, 2)
  
  // 验证转换后的数据
  for record in pipeline.output_data {
    assert_eq(record.data, record.data.to_uppercase())
    assert_true(record.metadata.contains(("validated", "true")))
    assert_false(record.data.contains("INVALID"))
  }
  
  // 验证特定记录
  let record1 = pipeline.output_data.find(fn(r) { r.id == "1" })
  assert_true(record1 != None)
  assert_eq(record1.unwrap().data, "TEST DATA")
  
  let record3 = pipeline.output_data.find(fn(r) { r.id == "3" })
  assert_true(record3 == None)  // 被过滤掉
}

// 测试2: 流数据处理
test "流数据处理" {
  // 定义流事件
  type StreamEvent = {
    id: String,
    event_type: String,
    data: String,
    timestamp: Int,
    key: String
  }
  
  // 定义窗口类型
  enum WindowType {
    Tumbling    // 滚动窗口
    Sliding     // 滑动窗口
    Session     // 会话窗口
  }
  
  // 定义时间窗口
  type TimeWindow = {
    window_type: WindowType,
    size: Int,        // 窗口大小（毫秒）
    slide: Int,       // 滑动间隔（毫秒）
    events: Array[StreamEvent]
  }
  
  // 定义流处理器
  type StreamProcessor = {
    windows: Array[TimeWindow],
    current_time: Int,
    processed_events: Int,
    dropped_events: Int
  }
  
  // 创建流处理器
  let create_stream_processor = fn() {
    {
      windows: [],
      current_time: 1640995200,
      processed_events: 0,
      dropped_events: 0
    }
  }
  
  // 添加时间窗口
  let add_window = fn(processor: StreamProcessor, window_type: WindowType, size: Int, slide: Int) {
    let window = {
      window_type,
      size,
      slide,
      events: []
    }
    processor.windows = processor.windows.push(window)
  }
  
  // 处理流事件
  let process_event = fn(processor: StreamProcessor, event: StreamEvent) {
    processor.current_time = event.timestamp
    
    let mut processed = false
    
    for window in processor.windows {
      // 简化的窗口处理逻辑
      let window_start = processor.current_time - window.size
      
      // 清理过期事件
      let mut valid_events = []
      for e in window.events {
        if e.timestamp >= window_start {
          valid_events = valid_events.push(e)
        }
      }
      window.events = valid_events
      
      // 添加新事件
      if event.timestamp >= window_start {
        window.events = window.events.push(event)
        processed = true
      }
    }
    
    if processed {
      processor.processed_events = processor.processed_events + 1
    } else {
      processor.dropped_events = processor.dropped_events + 1
    }
    
    processed
  }
  
  // 获取窗口统计
  let get_window_stats = fn(processor: StreamProcessor, window_index: Int) {
    if window_index >= 0 and window_index < processor.windows.length() {
      let window = processor.windows[window_index]
      let event_types = []
      
      for event in window.events {
        if not(event_types.contains(event.event_type)) {
          event_types = event_types.push(event.event_type)
        }
      }
      
      {
        window_type: window.window_type,
        event_count: window.events.length(),
        event_types,
        time_range: if window.events.length() > 0 {
          let min_time = window.events.reduce(fn(min, e) { if e.timestamp < min { e.timestamp } else { min } }, window.events[0].timestamp)
          let max_time = window.events.reduce(fn(max, e) { if e.timestamp > max { e.timestamp } else { max } }, window.events[0].timestamp)
          (min_time, max_time)
        } else {
          (0, 0)
        }
      }
    } else {
      {
        window_type: WindowType::Tumbling,
        event_count: 0,
        event_types: [],
        time_range: (0, 0)
      }
    }
  }
  
  // 测试流处理
  let processor = create_stream_processor()
  
  // 添加窗口
  add_window(processor, WindowType::Tumbling, 5000, 5000)   // 5秒滚动窗口
  add_window(processor, WindowType::Sliding, 3000, 1000)     // 3秒滑动窗口，1秒滑动
  
  // 创建测试事件
  let events = [
    { id: "1", event_type: "click", data: "button1", timestamp: 1640995200, key: "user1" },
    { id: "2", event_type: "view", data: "page1", timestamp: 1640995201, key: "user1" },
    { id: "3", event_type: "click", data: "button2", timestamp: 1640995202, key: "user2" },
    { id: "4", event_type: "purchase", data: "item1", timestamp: 1640995203, key: "user1" },
    { id: "5", event_type: "view", data: "page2", timestamp: 1640995204, key: "user2" },
    { id: "6", event_type: "click", data: "button3", timestamp: 1640995208, key: "user1" },  // 8秒后
    { id: "7", event_type: "view", data: "page3", timestamp: 1640995209, key: "user3" }      // 9秒后
  ]
  
  // 处理事件
  for event in events {
    process_event(processor, event)
  }
  
  // 验证处理结果
  assert_eq(processor.processed_events, 7)
  assert_eq(processor.dropped_events, 0)
  
  // 获取窗口统计
  let tumbling_stats = get_window_stats(processor, 0)
  assert_eq(tumbling_stats.window_type, WindowType::Tumbling)
  assert_eq(tumbling_stats.event_count, 5)  // 前5个事件在5秒窗口内
  
  let sliding_stats = get_window_stats(processor, 1)
  assert_eq(sliding_stats.window_type, WindowType::Sliding)
  assert_eq(sliding_stats.event_count, 3)  // 最后3个事件在3秒滑动窗口内
  
  // 验证事件类型
  assert_true(tumbling_stats.event_types.contains("click"))
  assert_true(tumbling_stats.event_types.contains("view"))
  assert_true(tumbling_stats.event_types.contains("purchase"))
}

// 测试3: ETL操作
test "ETL操作" {
  // 定义源数据类型
  enum SourceType {
    Database
    File
    API
    MessageQueue
  }
  
  // 定义目标数据类型
  enum TargetType {
    Database
    DataWarehouse
    File
    Analytics
  }
  
  // 定义ETL步骤
  enum ETLStep {
    Extract
    Transform
    Load
  }
  
  // 定义源连接
  type SourceConnection = {
    name: String,
    source_type: SourceType,
    connection_string: String,
    query: String
  }
  
  // 定义目标连接
  type TargetConnection = {
    name: String,
    target_type: TargetType,
    connection_string: String,
    table: String
  }
  
  // 定义转换规则
  type TransformRule = {
    name: String,
    source_field: String,
    target_field: String,
    transformation: (String) -> String
  }
  
  // 定义ETL作业
  type ETLJob = {
    name: String,
    source: SourceConnection,
    target: TargetConnection,
    transform_rules: Array[TransformRule],
    current_step: ETLStep,
    extracted_data: Array[(String, String)],  // (field_name, value)
    transformed_data: Array[(String, String)],
    loaded_records: Int,
    error_records: Int
  }
  
  // 创建ETL作业
  let create_etl_job = fn(name: String, source: SourceConnection, target: TargetConnection) {
    {
      name,
      source,
      target,
      transform_rules: [],
      current_step: ETLStep::Extract,
      extracted_data: [],
      transformed_data: [],
      loaded_records: 0,
      error_records: 0
    }
  }
  
  // 添加转换规则
  let add_transform_rule = fn(job: ETLJob, name: String, source_field: String, target_field: String, transformation: (String) -> String) {
    let rule = {
      name,
      source_field,
      target_field,
      transformation
    }
    job.transform_rules = job.transform_rules.push(rule)
  }
  
  // 执行提取步骤
  let execute_extract = fn(job: ETLJob) {
    // 模拟从源提取数据
    let mock_data = [
      ("user_id", "123"),
      ("user_name", "John Doe"),
      ("email", "john.doe@example.com"),
      ("age", "30"),
      ("registration_date", "2022-01-01")
    ]
    
    job.extracted_data = mock_data
    job.current_step = ETLStep::Transform
  }
  
  // 执行转换步骤
  let execute_transform = fn(job: ETLJob) {
    for rule in job.transform_rules {
      // 查找源字段值
      match job.extracted_data.find(fn(d) { d.0 == rule.source_field }) {
        Some((_, value)) => {
          // 应用转换
          let transformed_value = rule.transformation(value)
          
          // 添加到转换后的数据
          job.transformed_data = job.transformed_data.push((rule.target_field, transformed_value))
        }
        None => {
          job.error_records = job.error_records + 1
        }
      }
    }
    
    job.current_step = ETLStep::Load
  }
  
  // 执行加载步骤
  let execute_load = fn(job: ETLJob) {
    // 模拟加载到目标
    if job.transformed_data.length() > 0 {
      job.loaded_records = 1
      job.current_step = ETLStep::Extract  // 重置为提取步骤，准备下一次运行
    } else {
      job.error_records = job.error_records + 1
    }
  }
  
  // 执行完整ETL作业
  let execute_etl_job = fn(job: ETLJob) {
    execute_extract(job)
    execute_transform(job)
    execute_load(job)
  }
  
  // 测试ETL操作
  let source = {
    name: "user_db",
    source_type: SourceType::Database,
    connection_string: "postgresql://localhost/users",
    query: "SELECT * FROM users"
  }
  
  let target = {
    name: "data_warehouse",
    target_type: TargetType::DataWarehouse,
    connection_string: "postgresql://localhost/dw",
    table: "dim_users"
  }
  
  let job = create_etl_job("user_etl", source, target)
  
  // 添加转换规则
  add_transform_rule(job, "uppercase_name", "user_name", "USER_NAME", fn(value) { value.to_uppercase() })
  add_transform_rule(job, "domain_extract", "email", "EMAIL_DOMAIN", fn(value) {
    let parts = value.split("@")
    if parts.length() >= 2 {
      parts[1]
    } else {
      ""
    }
  })
  add_transform_rule(job, "age_group", "age", "AGE_GROUP", fn(value) {
    let age = value.to_int()
    if age < 18 {
      "Under 18"
    } else if age < 30 {
      "18-29"
    } else if age < 50 {
      "30-49"
    } else {
      "50+"
    }
  })
  
  // 执行ETL作业
  execute_etl_job(job)
  
  // 验证结果
  assert_eq(job.current_step, ETLStep::Extract)  // 作业完成后重置
  assert_eq(job.loaded_records, 1)
  assert_eq(job.error_records, 0)
  
  // 验证转换后的数据
  assert_eq(job.transformed_data.length(), 3)
  
  let user_name = job.transformed_data.find(fn(d) { d.0 == "USER_NAME" })
  assert_eq(user_name, Some(("USER_NAME", "JOHN DOE")))
  
  let email_domain = job.transformed_data.find(fn(d) { d.0 == "EMAIL_DOMAIN" })
  assert_eq(email_domain, Some(("EMAIL_DOMAIN", "example.com")))
  
  let age_group = job.transformed_data.find(fn(d) { d.0 == "AGE_GROUP" })
  assert_eq(age_group, Some(("AGE_GROUP", "30-49")))
}

// 测试4: 数据聚合操作
test "数据聚合操作" {
  // 定义聚合类型
  enum AggregationType {
    Sum
    Average
    Min
    Max
    Count
    CountDistinct
  }
  
  // 定义聚合函数
  type AggregationFunction = {
    name: String,
    type: AggregationType,
    field: String
  }
  
  // 定义分组键
  type GroupBy = {
    fields: Array[String]
  }
  
  // 定义聚合结果
  type AggregationResult = {
    group_key: String,
    values: Array[(String, String)]  // (field_name, aggregated_value)
  }
  
  // 定义数据聚合器
  type DataAggregator = {
    data: Array[(String, String)],  // (field_name, value)
    group_by: GroupBy,
    aggregations: Array[AggregationFunction],
    results: Array[AggregationResult]
  }
  
  // 创建数据聚合器
  let create_aggregator = fn(group_by: GroupBy) {
    {
      data: [],
      group_by,
      aggregations: [],
      results: []
    }
  }
  
  // 添加数据
  let add_data = fn(aggregator: DataAggregator, data: Array[(String, String)]) {
    aggregator.data = aggregator.data + data
  }
  
  // 添加聚合函数
  let add_aggregation = fn(aggregator: DataAggregator, name: String, agg_type: AggregationType, field: String) {
    let agg_func = {
      name,
      type: agg_type,
      field
    }
    aggregator.aggregations = aggregator.aggregations.push(agg_func)
  }
  
  // 执行聚合操作
  let execute_aggregation = fn(aggregator: DataAggregator) {
    // 将数据转换为记录格式
    let mut records = []
    let mut record_index = 0
    
    // 简化处理：假设每3个字段组成一个记录
    for i in 0..aggregator.data.length() / 3 {
      let start = i * 3
      let record = {
        id: i.to_string(),
        fields: [
          aggregator.data[start],
          aggregator.data[start + 1],
          aggregator.data[start + 2]
        ]
      }
      records = records.push(record)
    }
    
    // 按分组键分组
    let mut groups = []
    
    for record in records {
      let mut group_key = ""
      
      // 构建分组键
      for field_name in aggregator.group_by.fields {
        match record.fields.find(fn(f) { f.0 == field_name }) {
          Some((_, value)) => group_key = group_key + value + "|"
          None => {}
        }
      }
      
      // 查找或创建分组
      let mut group_index = -1
      for i in 0..groups.length() {
        if groups[i].0 == group_key {
          group_index = i
          break
        }
      }
      
      if group_index == -1 {
        groups = groups.push((group_key, []))
        group_index = groups.length() - 1
      }
      
      // 添加记录到分组
      let group_records = groups[group_index].1
      groups[group_index] = (group_key, group_records.push(record))
    }
    
    // 对每个分组执行聚合
    for group in groups {
      let (group_key, group_records) = group
      let mut agg_values = []
      
      for agg_func in aggregator.aggregations {
        let mut values = []
        
        // 提取字段值
        for record in group_records {
          match record.fields.find(fn(f) { f.0 == agg_func.field }) {
            Some((_, value)) => values = values.push(value)
            None => {}
          }
        }
        
        // 计算聚合值
        let result = match agg_func.type {
          AggregationType::Sum => {
            let sum = values.reduce(fn(acc, v) { acc + v.to_int() }, 0)
            sum.to_string()
          }
          AggregationType::Average => {
            if values.length() > 0 {
              let sum = values.reduce(fn(acc, v) { acc + v.to_int() }, 0)
              (sum / values.length()).to_string()
            } else {
              "0"
            }
          }
          AggregationType::Min => {
            if values.length() > 0 {
              values.reduce(fn(min, v) { 
                let v_int = v.to_int()
                if v_int < min.to_int() { v } else { min } 
              }, values[0])
            } else {
              ""
            }
          }
          AggregationType::Max => {
            if values.length() > 0 {
              values.reduce(fn(max, v) { 
                let v_int = v.to_int()
                if v_int > max.to_int() { v } else { max } 
              }, values[0])
            } else {
              ""
            }
          }
          AggregationType::Count => values.length().to_string()
          AggregationType::CountDistinct => {
            let mut distinct = []
            for v in values {
              if not(distinct.contains(v)) {
                distinct = distinct.push(v)
              }
            }
            distinct.length().to_string()
          }
        }
        
        agg_values = agg_values.push((agg_func.name, result))
      }
      
      let agg_result = {
        group_key,
        values: agg_values
      }
      
      aggregator.results = aggregator.results.push(agg_result)
    }
  }
  
  // 测试数据聚合
  let group_by = { fields: ["category", "region"] }
  let aggregator = create_aggregator(group_by)
  
  // 添加测试数据
  let test_data = [
    ("category", "A"), ("region", "North"), ("sales", "100"),
    ("category", "A"), ("region", "North"), ("sales", "150"),
    ("category", "A"), ("region", "South"), ("sales", "200"),
    ("category", "B"), ("region", "North"), ("sales", "120"),
    ("category", "B"), ("region", "South"), ("sales", "180"),
    ("category", "A"), ("region", "South"), ("sales", "250")
  ]
  
  add_data(aggregator, test_data)
  
  // 添加聚合函数
  add_aggregation(aggregator, "total_sales", AggregationType::Sum, "sales")
  add_aggregation(aggregator, "avg_sales", AggregationType::Average, "sales")
  add_aggregation(aggregator, "max_sales", AggregationType::Max, "sales")
  add_aggregation(aggregator, "record_count", AggregationType::Count, "sales")
  
  // 执行聚合
  execute_aggregation(aggregator)
  
  // 验证结果
  assert_eq(aggregator.results.length(), 3)  // 3个不同的分组
  
  // 验证A-North分组
  let a_north_result = aggregator.results.find(fn(r) { r.group_key == "A|North|" })
  assert_true(a_north_result != None)
  
  match a_north_result {
    Some(result) => {
      let total_sales = result.values.find(fn(v) { v.0 == "total_sales" })
      assert_eq(total_sales, Some(("total_sales", "250")))  // 100 + 150
      
      let avg_sales = result.values.find(fn(v) { v.0 == "avg_sales" })
      assert_eq(avg_sales, Some(("avg_sales", "125")))  // (100 + 150) / 2
      
      let max_sales = result.values.find(fn(v) { v.0 == "max_sales" })
      assert_eq(max_sales, Some(("max_sales", "150")))
      
      let record_count = result.values.find(fn(v) { v.0 == "record_count" })
      assert_eq(record_count, Some(("record_count", "2")))
    }
    None => assert_true(false)
  }
  
  // 验证A-South分组
  let a_south_result = aggregator.results.find(fn(r) { r.group_key == "A|South|" })
  assert_true(a_south_result != None)
  
  match a_south_result {
    Some(result) => {
      let total_sales = result.values.find(fn(v) { v.0 == "total_sales" })
      assert_eq(total_sales, Some(("total_sales", "450")))  // 200 + 250
      
      let record_count = result.values.find(fn(v) { v.0 == "record_count" })
      assert_eq(record_count, Some(("record_count", "2")))
    }
    None => assert_true(false)
  }
  
  // 验证B-North分组
  let b_north_result = aggregator.results.find(fn(r) { r.group_key == "B|North|" })
  assert_true(b_north_result != None)
  
  match b_north_result {
    Some(result) => {
      let total_sales = result.values.find(fn(v) { v.0 == "total_sales" })
      assert_eq(total_sales, Some(("total_sales", "120")))
      
      let record_count = result.values.find(fn(v) { v.0 == "record_count" })
      assert_eq(record_count, Some(("record_count", "1")))
    }
    None => assert_true(false)
  }
}

// 测试5: 数据清洗操作
test "数据清洗操作" {
  // 定义数据质量问题
  enum DataQualityIssue {
    MissingValue
    InvalidFormat
    Duplicate
    Outlier
    Inconsistent
  }
  
  // 定义清洗规则
  type CleaningRule = {
    name: String,
    issue_type: DataQualityIssue,
    field: String,
    action: String,  // "remove", "replace", "transform"
    parameters: Array[String]
  }
  
  // 定义数据记录
  type DataRecord = {
    id: String,
    fields: Array[(String, String)]
  }
  
  // 定义清洗结果
  type CleaningResult = {
    original_record: DataRecord,
    cleaned_record: Option[DataRecord],
    issues_found: Array[DataQualityIssue],
    applied_rules: Array[String]
  }
  
  // 定义数据清洗器
  type DataCleaner = {
    rules: Array[CleaningRule],
    results: Array[CleaningResult]
  }
  
  // 创建数据清洗器
  let create_cleaner = fn() {
    {
      rules: [],
      results: []
    }
  }
  
  // 添加清洗规则
  let add_cleaning_rule = fn(cleaner: DataCleaner, name: String, issue_type: DataQualityIssue, field: String, action: String, parameters: Array[String>) {
    let rule = {
      name,
      issue_type,
      field,
      action,
      parameters
    }
    cleaner.rules = cleaner.rules.push(rule)
  }
  
  // 检查数据质量问题
  let check_quality_issues = fn(record: DataRecord) {
    let mut issues = []
    
    for field in record.fields {
      let (field_name, field_value) = field
      
      // 检查缺失值
      if field_value == "" or field_value == "NULL" or field_value == "null" {
        issues = issues.push(DataQualityIssue::MissingValue)
      }
      
      // 检查邮箱格式
      if field_name == "email" and field_value != "" {
        if not(field_value.contains("@")) {
          issues = issues.push(DataQualityIssue::InvalidFormat)
        }
      }
      
      // 检查年龄范围
      if field_name == "age" and field_value != "" {
        let age = field_value.to_int()
        if age < 0 or age > 150 {
          issues = issues.push(DataQualityIssue::Outlier)
        }
      }
    }
    
    issues
  }
  
  // 应用清洗规则
  let apply_cleaning_rules = fn(cleaner: DataCleaner, record: DataRecord, issues: Array[DataQualityIssue]) {
    let mut cleaned_fields = record.fields
    let mut applied_rules = []
    
    for issue in issues {
      for rule in cleaner.rules {
        if rule.issue_type == issue {
          let field_index = cleaned_fields.index(fn(f) { f.0 == rule.field })
          
          match field_index {
            Some(index) => {
              match rule.action {
                "remove" => {
                  cleaned_fields = cleaned_fields.slice(0, index) + cleaned_fields.slice(index + 1)
                }
                "replace" => {
                  if rule.parameters.length() > 0 {
                    cleaned_fields[index] = (rule.field, rule.parameters[0])
                  }
                }
                "transform" => {
                  if rule.field == "email" and rule.parameters.contains("lowercase") {
                    let (field_name, field_value) = cleaned_fields[index]
                    cleaned_fields[index] = (field_name, field_value.to_lowercase())
                  }
                }
                _ => {}
              }
              
              applied_rules = applied_rules.push(rule.name)
            }
            None => {}
          }
        }
      }
    }
    
    let cleaned_record = { id: record.id, fields: cleaned_fields }
    (cleaned_record, applied_rules)
  }
  
  // 清洗数据记录
  let clean_record = fn(cleaner: DataCleaner, record: DataRecord) {
    let issues = check_quality_issues(record)
    let (cleaned_record, applied_rules) = apply_cleaning_rules(cleaner, record, issues)
    
    let result = {
      original_record: record,
      cleaned_record: Some(cleaned_record),
      issues_found: issues,
      applied_rules
    }
    
    cleaner.results = cleaner.results.push(result)
  }
  
  // 测试数据清洗
  let cleaner = create_cleaner()
  
  // 添加清洗规则
  add_cleaning_rule(cleaner, "remove_null_email", DataQualityIssue::MissingValue, "email", "remove", [])
  add_cleaning_rule(cleaner, "replace_missing_name", DataQualityIssue::MissingValue, "name", "replace", ["Unknown"])
  add_cleaning_rule(cleaner, "fix_invalid_email", DataQualityIssue::InvalidFormat, "email", "replace", ["invalid@example.com"])
  add_cleaning_rule(cleaner, "lowercase_email", DataQualityIssue::Inconsistent, "email", "transform", ["lowercase"])
  add_cleaning_rule(cleaner, "fix_age_outlier", DataQualityIssue::Outlier, "age", "replace", ["0"])
  
  // 创建测试记录
  let test_records = [
    {
      id: "1",
      fields: [
        ("name", "John Doe"),
        ("email", "JOHN.DOE@EXAMPLE.COM"),
        ("age", "30")
      ]
    },
    {
      id: "2",
      fields: [
        ("name", ""),
        ("email", "jane@example.com"),
        ("age", "25")
      ]
    },
    {
      id: "3",
      fields: [
        ("name", "Bob Smith"),
        ("email", "invalid-email"),
        ("age", "200")
      ]
    },
    {
      id: "4",
      fields: [
        ("name", "Alice Johnson"),
        ("email", ""),
        ("age", "35")
      ]
    },
    {
      id: "5",
      fields: [
        ("name", "Tom Brown"),
        ("email", "tom@example.com"),
        ("age", "-5")
      ]
    }
  ]
  
  // 清洗记录
  for record in test_records {
    clean_record(cleaner, record)
  }
  
  // 验证清洗结果
  assert_eq(cleaner.results.length(), 5)
  
  // 验证第一条记录（邮箱格式不一致）
  let result1 = cleaner.results[0]
  assert_eq(result1.issues_found.length(), 1)
  assert_true(result1.issues_found.contains(DataQualityIssue::Inconsistent))
  assert_true(result1.applied_rules.contains("lowercase_email"))
  
  match result1.cleaned_record {
    Some(record) => {
      let email_field = record.fields.find(fn(f) { f.0 == "email" })
      assert_eq(email_field, Some(("email", "john.doe@example.com")))
    }
    None => assert_true(false)
  }
  
  // 验证第二条记录（缺失姓名）
  let result2 = cleaner.results[1]
  assert_true(result2.issues_found.contains(DataQualityIssue::MissingValue))
  assert_true(result2.applied_rules.contains("replace_missing_name"))
  
  match result2.cleaned_record {
    Some(record) => {
      let name_field = record.fields.find(fn(f) { f.0 == "name" })
      assert_eq(name_field, Some(("name", "Unknown")))
    }
    None => assert_true(false)
  }
  
  // 验证第三条记录（无效邮箱和年龄异常）
  let result3 = cleaner.results[2]
  assert_eq(result3.issues_found.length(), 2)
  assert_true(result3.issues_found.contains(DataQualityIssue::InvalidFormat))
  assert_true(result3.issues_found.contains(DataQualityIssue::Outlier))
  assert_true(result3.applied_rules.contains("fix_invalid_email"))
  assert_true(result3.applied_rules.contains("fix_age_outlier"))
  
  match result3.cleaned_record {
    Some(record) => {
      let email_field = record.fields.find(fn(f) { f.0 == "email" })
      assert_eq(email_field, Some(("email", "invalid@example.com")))
      
      let age_field = record.fields.find(fn(f) { f.0 == "age" })
      assert_eq(age_field, Some(("age", "0")))
    }
    None => assert_true(false)
  }
  
  // 验证第四条记录（缺失邮箱，应该被移除）
  let result4 = cleaner.results[3]
  assert_true(result4.issues_found.contains(DataQualityIssue::MissingValue))
  assert_true(result4.applied_rules.contains("remove_null_email"))
  
  match result4.cleaned_record {
    Some(record) => {
      let email_field = record.fields.find(fn(f) { f.0 == "email" })
      assert_eq(email_field, None)  // 邮箱字段被移除
    }
    None => assert_true(false)
  }
}

// 测试6: 数据管道监控
test "数据管道监控" {
  // 定义管道状态
  enum PipelineStatus {
    Running
    Stopped
    Failed
    Completed
  }
  
  // 定义指标类型
  enum MetricType {
    Counter
    Gauge
    Histogram
    Timer
  }
  
  // 定义管道指标
  type PipelineMetric = {
    name: String,
    type: MetricType,
    value: Float,
    timestamp: Int,
    labels: Array[(String, String)]
  }
  
  // 定义管道事件
  type PipelineEvent = {
    id: String,
    event_type: String,
    message: String,
    timestamp: Int,
    severity: String
  }
  
  // 定义管道监控器
  type PipelineMonitor = {
    pipeline_name: String,
    status: PipelineStatus,
    metrics: Array[PipelineMetric],
    events: Array[PipelineEvent],
    start_time: Int,
    last_update: Int
  }
  
  // 创建管道监控器
  let create_monitor = fn(pipeline_name: String) {
    {
      pipeline_name,
      status: PipelineStatus::Stopped,
      metrics: [],
      events: [],
      start_time: 0,
      last_update: 0
    }
  }
  
  // 启动管道
  let start_pipeline = fn(monitor: PipelineMonitor) {
    monitor.status = PipelineStatus::Running
    monitor.start_time = 1640995200
    monitor.last_update = monitor.start_time
    
    let event = {
      id: "start-" + monitor.start_time.to_string(),
      event_type: "pipeline_start",
      message: "Pipeline started",
      timestamp: monitor.start_time,
      severity: "info"
    }
    monitor.events = monitor.events.push(event)
  }
  
  // 停止管道
  let stop_pipeline = fn(monitor: PipelineMonitor, status: PipelineStatus) {
    monitor.status = status
    monitor.last_update = 1640995300
    
    let event_type = match status {
      PipelineStatus::Stopped => "pipeline_stop"
      PipelineStatus::Failed => "pipeline_fail"
      PipelineStatus::Completed => "pipeline_complete"
      _ => "pipeline_stop"
    }
    
    let message = match status {
      PipelineStatus::Stopped => "Pipeline stopped"
      PipelineStatus::Failed => "Pipeline failed"
      PipelineStatus::Completed => "Pipeline completed successfully"
      _ => "Pipeline stopped"
    }
    
    let event = {
      id: "stop-" + monitor.last_update.to_string(),
      event_type,
      message,
      timestamp: monitor.last_update,
      severity: match status {
        PipelineStatus::Failed => "error"
        PipelineStatus::Completed => "success"
        _ => "info"
      }
    }
    monitor.events = monitor.events.push(event)
  }
  
  // 记录指标
  let record_metric = fn(monitor: PipelineMonitor, name: String, metric_type: MetricType, value: Float, labels: Array[(String, String)]) {
    let metric = {
      name,
      type: metric_type,
      value,
      timestamp: 1640995250,
      labels
    }
    monitor.metrics = monitor.metrics.push(metric)
    monitor.last_update = metric.timestamp
  }
  
  // 记录事件
  let record_event = fn(monitor: PipelineMonitor, event_type: String, message: String, severity: String) {
    let event = {
      id: "event-" + monitor.metrics.length().to_string(),
      event_type,
      message,
      timestamp: 1640995250,
      severity
    }
    monitor.events = monitor.events.push(event)
    monitor.last_update = event.timestamp
  }
  
  // 获取指标值
  let get_metric_value = fn(monitor: PipelineMonitor, name: String, labels: Array[(String, String)]) {
    for metric in monitor.metrics {
      if metric.name == name {
        let mut all_match = true
        
        for label in labels {
          if not(metric.labels.contains(label)) {
            all_match = false
            break
          }
        }
        
        if all_match {
          return Some(metric.value)
        }
      }
    }
    None
  }
  
  // 获取运行时间
  let get_runtime = fn(monitor: PipelineMonitor) {
    if monitor.start_time > 0 and monitor.last_update >= monitor.start_time {
      monitor.last_update - monitor.start_time
    } else {
      0
    }
  }
  
  // 获取事件统计
  let get_event_stats = fn(monitor: PipelineMonitor) {
    let mut error_count = 0
    let mut warning_count = 0
    let mut info_count = 0
    
    for event in monitor.events {
      match event.severity {
        "error" => error_count = error_count + 1
        "warning" => warning_count = warning_count + 1
        "info" => info_count = info_count + 1
        _ => {}
      }
    }
    
    {
      total_events: monitor.events.length(),
      error_count,
      warning_count,
      info_count
    }
  }
  
  // 测试管道监控
  let monitor = create_monitor("test_pipeline")
  
  // 验证初始状态
  assert_eq(monitor.pipeline_name, "test_pipeline")
  assert_eq(monitor.status, PipelineStatus::Stopped)
  assert_eq(monitor.metrics.length(), 0)
  assert_eq(monitor.events.length(), 0)
  
  // 启动管道
  start_pipeline(monitor)
  
  assert_eq(monitor.status, PipelineStatus::Running)
  assert_eq(monitor.start_time, 1640995200)
  assert_eq(monitor.events.length(), 1)
  assert_eq(monitor.events[0].event_type, "pipeline_start")
  
  // 记录指标
  record_metric(monitor, "records_processed", MetricType::Counter, 1000.0, [("stage", "input")])
  record_metric(monitor, "records_processed", MetricType::Counter, 950.0, [("stage", "transform")])
  record_metric(monitor, "records_processed", MetricType::Counter, 950.0, [("stage", "output")])
  
  record_metric(monitor, "processing_time", MetricType::Timer, 5000.0, [])
  record_metric(monitor, "memory_usage", MetricType::Gauge, 512.0, [("unit", "MB")])
  
  // 记录事件
  record_event(monitor, "stage_start", "Input stage started", "info")
  record_event(monitor, "stage_complete", "Input stage completed", "info")
  record_event(monitor, "warning", "Slow processing detected", "warning")
  
  // 验证指标
  assert_eq(monitor.metrics.length(), 5)
  
  let input_records = get_metric_value(monitor, "records_processed", [("stage", "input")])
  assert_eq(input_records, Some(1000.0))
  
  let memory_usage = get_metric_value(monitor, "memory_usage", [("unit", "MB")])
  assert_eq(memory_usage, Some(512.0))
  
  // 验证事件
  assert_eq(monitor.events.length(), 5)
  
  // 获取事件统计
  let event_stats = get_event_stats(monitor)
  assert_eq(event_stats.total_events, 5)
  assert_eq(event_stats.error_count, 0)
  assert_eq(event_stats.warning_count, 1)
  assert_eq(event_stats.info_count, 4)
  
  // 停止管道
  stop_pipeline(monitor, PipelineStatus::Completed)
  
  assert_eq(monitor.status, PipelineStatus::Completed)
  assert_eq(monitor.events.length(), 6)
  assert_eq(monitor.events[5].event_type, "pipeline_complete")
  
  // 获取运行时间
  let runtime = get_runtime(monitor)
  assert_eq(runtime, 100)  // 1640995300 - 1640995200
}