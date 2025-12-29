// Metrics聚合边界测试
test "metrics_counter_boundary_conditions" {
  // 测试Counter的边界条件
  
  let meter = NoopMeter::{}
  let counter = meter.create_counter("boundary.test.counter", "requests", "Boundary test counter")
  
  // 测试边界值
  let boundary_values = [
    0L,  // 零值
    1L,  // 最小正值
    -1L,  // 负值
    9223372036854775807L,  // 最大64位整数
    -9223372036854775808L,  // 最小64位整数
    1000000L,  // 大值
    -1000000L,  // 大负值
    42L,  // 常规值
    -42L  // 常规负值
  ]
  
  let mut i = 0
  while i < boundary_values.length() {
    let attributes = Some([
      ("test.value", AttributeValue::int(boundary_values[i])),
      ("test.index", AttributeValue::int(i.to_int64()))
    ])
    
    // 测试边界值的添加
    counter.add(boundary_values[i], attributes)
    i = i + 1
  }
  
  // 测试带有属性的边界值
  let edge_attributes = [
    ("empty.string", AttributeValue::string("")),
    ("empty.array", AttributeValue::array_string([])),
    ("zero.int", AttributeValue::int(0L)),
    ("zero.float", AttributeValue::float(0.0)),
    ("false.bool", AttributeValue::bool(false)),
    ("max.int", AttributeValue::int(9223372036854775807L)),
    ("min.int", AttributeValue::int(-9223372036854775808L)),
    ("inf.float", AttributeValue::float(1.0/0.0)),
    ("neg.inf.float", AttributeValue::float(-1.0/0.0)),
    ("nan.float", AttributeValue::float(0.0/0.0)),
    ("unicode.string", AttributeValue::string("测试中文🚀")),
    ("special.chars", AttributeValue::string("!@#$%^&*()")),
    ("very.long.string", AttributeValue::string("a".repeat(1000))),
    ("large.array", AttributeValue::array_string([for i = 0; i < 100; i = i + 1].map(fn(_) { "item" })))
  ]
  
  // 测试边界属性
  counter.add(1L, Some(edge_attributes))
  
  // 测试空属性
  counter.add(1L, None)
  counter.add(1L, Some([]))
  
  // 测试大量属性
  let mut large_attributes = []
  let mut j = 0
  while j < 1000 {
    large_attributes.push(("large.attr." + j.to_string(), AttributeValue::int(j.to_int64())))
    j = j + 1
  }
  
  counter.add(1L, Some(large_attributes))
}

test "metrics_histogram_boundary_conditions" {
  // 测试Histogram的边界条件
  
  let meter = NoopMeter::{}
  let histogram = meter.create_histogram("boundary.test.histogram", "seconds", "Boundary test histogram")
  
  // 测试边界值
  let boundary_values = [
    0.0,  // 零值
    1.0e-10,  // 极小正值
    -1.0e-10,  // 极小负值
    1.7976931348623157e+308,  // 最大双精度浮点数
    -1.7976931348623157e+308,  // 最小双精度浮点数
    1.0/0.0,  // 正无穷大
    -1.0/0.0,  // 负无穷大
    0.0/0.0,  // NaN
    3.14159265359,  // Pi
    -3.14159265359,  // 负Pi
    1.0e6,  // 大值
    -1.0e6,  // 大负值
    42.0,  // 常规值
    -42.0  // 常规负值
  ]
  
  let mut i = 0
  while i < boundary_values.length() {
    let attributes = Some([
      ("test.value", AttributeValue::float(boundary_values[i])),
      ("test.index", AttributeValue::int(i.to_int64())),
      ("value.type", AttributeValue::string(if boundary_values[i] >= 0.0 { "positive" } else { "negative" }))
    ])
    
    // 测试边界值的记录
    histogram.record(boundary_values[i], attributes)
    i = i + 1
  }
  
  // 测试特殊浮点值的属性
  let special_float_attributes = [
    ("zero.value", AttributeValue::float(0.0)),
    ("inf.value", AttributeValue::float(1.0/0.0)),
    ("neg.inf.value", AttributeValue::float(-1.0/0.0)),
    ("nan.value", AttributeValue::float(0.0/0.0)),
    ("very.small", AttributeValue::float(1.0e-100)),
    ("very.large", AttributeValue::float(1.0e100))
  ]
  
  histogram.record(1.0, Some(special_float_attributes))
  
  // 测试空属性
  histogram.record(1.0, None)
  histogram.record(1.0, Some([]))
}

test "metrics_up_down_counter_boundary_conditions" {
  // 测试UpDownCounter的边界条件
  
  let meter = NoopMeter::{}
  let up_down_counter = meter.create_up_down_counter("boundary.test.up_down_counter", "operations", "Boundary test up-down counter")
  
  // 测试边界值
  let boundary_values = [
    0L,  // 零值
    1L,  // 最小正值
    -1L,  // 负值
    9223372036854775807L,  // 最大64位整数
    -9223372036854775808L,  // 最小64位整数
    1000000L,  // 大值
    -1000000L,  // 大负值
    42L,  // 常规值
    -42L  // 常规负值
  ]
  
  let mut i = 0
  while i < boundary_values.length() {
    let attributes = Some([
      ("operation.type", AttributeValue::string(if boundary_values[i] >= 0L { "increment" } else { "decrement" })),
      ("operation.value", AttributeValue::int(boundary_values[i])),
      ("operation.index", AttributeValue::int(i.to_int64()))
    ])
    
    // 测试边界值的添加
    up_down_counter.add(boundary_values[i], attributes)
    i = i + 1
  }
  
  // 测试交替增减
  let mut j = 0
  while j < 100 {
    if j % 2 == 0 {
      up_down_counter.add(1L, Some([("alternating", AttributeValue::bool(true))]))
    } else {
      up_down_counter.add(-1L, Some([("alternating", AttributeValue::bool(true))]))
    }
    j = j + 1
  }
  
  // 测试大范围增减
  up_down_counter.add(1000000L, Some([("large.operation", AttributeValue::string("large_increment"))]))
  up_down_counter.add(-999999L, Some([("large.operation", AttributeValue::string("large_decrement"))]))
}

test "metrics_gauge_boundary_conditions" {
  // 测试Gauge的边界条件
  
  let meter = NoopMeter::{}
  let gauge = meter.create_gauge("boundary.test.gauge", "bytes", "Boundary test gauge")
  
  // 测试边界值
  let boundary_values = [
    0.0,  // 零值
    1.0e-10,  // 极小正值
    -1.0e-10,  // 极小负值
    1.7976931348623157e+308,  // 最大双精度浮点数
    -1.7976931348623157e+308,  // 最小双精度浮点数
    1.0/0.0,  // 正无穷大
    -1.0/0.0,  // 负无穷大
    0.0/0.0,  // NaN
    3.14159265359,  // Pi
    -3.14159265359,  // 负Pi
    1.0e6,  // 大值
    -1.0e6,  // 大负值
    42.0,  // 常规值
    -42.0  // 常规负值
  ]
  
  let mut i = 0
  while i < boundary_values.length() {
    let attributes = Some([
      ("gauge.type", AttributeValue::string(if boundary_values[i] >= 0.0 { "positive" } else { "negative" })),
      ("gauge.value", AttributeValue::float(boundary_values[i])),
      ("gauge.index", AttributeValue::int(i.to_int64()))
    ])
    
    // 测试边界值的记录
    gauge.record(boundary_values[i], attributes)
    i = i + 1
  }
  
  // 测试快速变化的值
  let mut j = 0
  while j < 1000 {
    gauge.record(j.to_double(), Some([("rapid.change", AttributeValue::bool(true))]))
    j = j + 1
  }
  
  // 测试周期性值
  let mut k = 0
  while k < 100 {
    let sine_value = (k.to_double() * 0.1).sin()
    gauge.record(sine_value, Some([("periodic", AttributeValue::bool(true))]))
    k = k + 1
  }
}

test "metrics_aggregation_edge_cases" {
  // 测试度量聚合的边界情况
  
  let meter = NoopMeter::{}
  
  // 创建多个相同类型的度量
  let counter1 = meter.create_counter("aggregation.test.counter", "requests", "Aggregation test counter 1")
  let counter2 = meter.create_counter("aggregation.test.counter", "requests", "Aggregation test counter 2")
  let counter3 = meter.create_counter("aggregation.test.counter", "requests", "Aggregation test counter 3")
  
  let histogram1 = meter.create_histogram("aggregation.test.histogram", "seconds", "Aggregation test histogram 1")
  let histogram2 = meter.create_histogram("aggregation.test.histogram", "seconds", "Aggregation test histogram 2")
  
  let gauge1 = meter.create_gauge("aggregation.test.gauge", "bytes", "Aggregation test gauge 1")
  let gauge2 = meter.create_gauge("aggregation.test.gauge", "bytes", "Aggregation test gauge 2")
  
  // 测试相同名称度量的并发操作
  let mut i = 0
  while i < 100 {
    counter1.add(1L, Some([("counter.id", AttributeValue::int(1L))]))
    counter2.add(2L, Some([("counter.id", AttributeValue::int(2L))]))
    counter3.add(3L, Some([("counter.id", AttributeValue::int(3L))]))
    
    histogram1.record(i.to_double(), Some([("histogram.id", AttributeValue::int(1L))]))
    histogram2.record(i.to_double() * 2.0, Some([("histogram.id", AttributeValue::int(2L))]))
    
    gauge1.record(i.to_double() * 0.1, Some([("gauge.id", AttributeValue::int(1L))]))
    gauge2.record(i.to_double() * 0.2, Some([("gauge.id", AttributeValue::int(2L))]))
    
    i = i + 1
  }
  
  // 测试不同属性组合的聚合
  let attribute_combinations = [
    [("service.name", AttributeValue::string("service-a")), ("operation.name", AttributeValue::string("op1"))],
    [("service.name", AttributeValue::string("service-a")), ("operation.name", AttributeValue::string("op2"))],
    [("service.name", AttributeValue::string("service-b")), ("operation.name", AttributeValue::string("op1"))],
    [("service.name", AttributeValue::string("service-b")), ("operation.name", AttributeValue::string("op2"))],
    [("service.name", AttributeValue::string("service-c")), ("operation.name", AttributeValue::string("op1"))],
    [("service.name", AttributeValue::string("service-c")), ("operation.name", AttributeValue::string("op2"))]
  ]
  
  let mut j = 0
  while j < attribute_combinations.length() {
    counter1.add(10L, Some(attribute_combinations[j]))
    histogram1.record(j.to_double(), Some(attribute_combinations[j]))
    gauge1.record(j.to_double() * 10.0, Some(attribute_combinations[j]))
    j = j + 1
  }
}

test "metrics_high_cardinality_attributes" {
  // 测试高基数属性的度量
  
  let meter = NoopMeter::{}
  let counter = meter.create_counter("high.cardinality.test.counter", "requests", "High cardinality test counter")
  let histogram = meter.create_histogram("high.cardinality.test.histogram", "seconds", "High cardinality test histogram")
  
  // 测试高基数用户ID
  let mut user_id = 0
  while user_id < 1000 {
    counter.add(1L, Some([
      ("user.id", AttributeValue::string("user-" + user_id.to_string())),
      ("operation.type", AttributeValue::string("api_request"))
    ]))
    
    histogram.record(0.1, Some([
      ("user.id", AttributeValue::string("user-" + user_id.to_string())),
      ("operation.type", AttributeValue::string("api_request"))
    ]))
    
    user_id = user_id + 1
  }
  
  // 测试高基数请求ID
  let mut request_id = 0
  while request_id < 500 {
    counter.add(1L, Some([
      ("request.id", AttributeValue::string("req-" + request_id.to_string())),
      ("service.name", AttributeValue::string("api-service"))
    ]))
    
    histogram.record(0.05, Some([
      ("request.id", AttributeValue::string("req-" + request_id.to_string())),
      ("service.name", AttributeValue::string("api-service"))
    ]))
    
    request_id = request_id + 1
  }
  
  // 测试高基数会话ID
  let mut session_id = 0
  while session_id < 200 {
    counter.add(1L, Some([
      ("session.id", AttributeValue::string("session-" + session_id.to_string())),
      ("user.type", AttributeValue::string("premium"))
    ]))
    
    histogram.record(0.2, Some([
      ("session.id", AttributeValue::string("session-" + session_id.to_string())),
      ("user.type", AttributeValue::string("premium"))
    ]))
    
    session_id = session_id + 1
  }
  
  // 测试复合高基数属性
  let mut k = 0
  while k < 100 {
    counter.add(1L, Some([
      ("user.id", AttributeValue::string("user-" + k.to_string())),
      ("session.id", AttributeValue::string("session-" + (k * 2).to_string())),
      ("request.id", AttributeValue::string("req-" + (k * 3).to_string())),
      ("trace.id", AttributeValue::string("trace-" + (k * 5).to_string()))
    ]))
    k = k + 1
  }
}

test "metrics_large_scale_operations" {
  // 测试大规模度量操作
  
  let meter = NoopMeter::{}
  
  // 创建多个度量实例
  let counters = []
  let histograms = []
  let gauges = []
  let up_down_counters = []
  
  let mut i = 0
  while i < 100 {
    counters.push(meter.create_counter("large.scale.counter." + i.to_string(), "operations", "Large scale counter " + i.to_string()))
    histograms.push(meter.create_histogram("large.scale.histogram." + i.to_string(), "seconds", "Large scale histogram " + i.to_string()))
    gauges.push(meter.create_gauge("large.scale.gauge." + i.to_string(), "bytes", "Large scale gauge " + i.to_string()))
    up_down_counters.push(meter.create_up_down_counter("large.scale.up_down_counter." + i.to_string(), "operations", "Large scale up-down counter " + i.to_string()))
    i = i + 1
  }
  
  // 对每个度量执行大量操作
  let mut j = 0
  while j < 100 {
    let mut k = 0
    while k < 100 {
      // 对每个计数器添加值
      let mut idx = 0
      while idx < counters.length() {
        counters[idx].add(j.to_int64(), Some([
          ("batch.index", AttributeValue::int(j.to_int64())),
          ("operation.index", AttributeValue::int(k.to_int64())),
          ("counter.index", AttributeValue::int(idx.to_int64()))
        ]))
        idx = idx + 1
      }
      
      // 对每个直方图记录值
      idx = 0
      while idx < histograms.length() {
        histograms[idx].record(k.to_double(), Some([
          ("batch.index", AttributeValue::int(j.to_int64())),
          ("operation.index", AttributeValue::int(k.to_int64())),
          ("histogram.index", AttributeValue::int(idx.to_int64()))
        ]))
        idx = idx + 1
      }
      
      // 对每个仪表记录值
      idx = 0
      while idx < gauges.length() {
        gauges[idx].record(k.to_double() * 0.01, Some([
          ("batch.index", AttributeValue::int(j.to_int64())),
          ("operation.index", AttributeValue::int(k.to_int64())),
          ("gauge.index", AttributeValue::int(idx.to_int64()))
        ]))
        idx = idx + 1
      }
      
      // 对每个上下行计数器添加值
      idx = 0
      while idx < up_down_counters.length() {
        let value = if k % 2 == 0 { 1L } else { -1L }
        up_down_counters[idx].add(value, Some([
          ("batch.index", AttributeValue::int(j.to_int64())),
          ("operation.index", AttributeValue::int(k.to_int64())),
          ("up_down_counter.index", AttributeValue::int(idx.to_int64())),
          ("operation.sign", AttributeValue::string(if value >= 0L { "positive" } else { "negative" }))
        ]))
        idx = idx + 1
      }
      
      k = k + 1
    }
    j = j + 1
  }
}

test "metrics_attribute_boundary_combinations" {
  // 测试属性边界组合
  
  let meter = NoopMeter::{}
  let counter = meter.create_counter("attribute.boundary.test.counter", "requests", "Attribute boundary test counter")
  
  // 测试所有属性类型的边界组合
  let string_attributes = [
    "",  // 空字符串
    " ",  // 空格
    "normal_string",  // 正常字符串
    "string_with_spaces",  // 包含空格
    "string-with-dashes",  // 包含连字符
    "string_with_underscores",  // 包含下划线
    "string.with.dots",  // 包含点号
    "UPPERCASE_STRING",  // 大写
    "lowercase_string",  // 小写
    "MixedCase_String",  // 混合大小写
    "123numeric_start",  // 数字开头
    "numeric_end_123",  // 数字结尾
    "special!@#$%^&*()chars",  // 特殊字符
    "测试中文",  // 中文
    "🚀emoji_test",  // emoji
    "a".repeat(1000),  // 长字符串
    "unicode_\u0000_test",  // Unicode空字符
    "new\nline\ntest",  // 换行符
    "tab\ttest\ttab",  // 制表符
    "carriage\rreturn\rtes"  // 回车符
  ]
  
  let int_attributes = [
    0L,  // 零
    1L,  // 最小正值
    -1L,  // 负值
    9223372036854775807L,  // 最大值
    -9223372036854775808L,  // 最小值
    42L,  // 常规值
    -42L,  // 常规负值
    1000000L,  // 大值
    -1000000L  // 大负值
  ]
  
  let float_attributes = [
    0.0,  // 零
    1.0e-10,  // 极小正值
    -1.0e-10,  // 极小负值
    1.7976931348623157e+308,  // 最大值
    -1.7976931348623157e+308,  // 最小值
    1.0/0.0,  // 正无穷
    -1.0/0.0,  // 负无穷
    0.0/0.0,  // NaN
    3.14159265359,  // Pi
    -3.14159265359  // 负Pi
  ]
  
  let bool_attributes = [
    true,  // 真
    false  // 假
  ]
  
  let array_string_attributes = [
    [],  // 空数组
    ["single_item"],  // 单项数组
    ["item1", "item2", "item3"],  // 多项数组
    [for i = 0; i < 100; i = i + 1].map(fn(_) { "repeated_item" }),  // 大数组
    ["", " ", "normal", "special!@#$%", "测试中文", "🚀"]  // 混合内容
  ]
  
  let array_int_attributes = [
    [],  // 空数组
    [42L],  // 单项数组
    [1L, 2L, 3L, 4L, 5L],  // 多项数组
    [for i = 0; i < 100; i = i + 1].map(fn(_) { 999L }),  // 大数组
    [0L, 1L, -1L, 9223372036854775807L, -9223372036854775808L]  // 边界值数组
  ]
  
  let array_float_attributes = [
    [],  // 空数组
    [3.14],  // 单项数组
    [1.1, 2.2, 3.3, 4.4, 5.5],  // 多项数组
    [for i = 0; i < 100; i = i + 1].map(fn(_) { 99.99 }),  // 大数组
    [0.0, 1.0e-10, 1.7976931348623157e+308, 1.0/0.0, 0.0/0.0]  // 特殊值数组
  ]
  
  let array_bool_attributes = [
    [],  // 空数组
    [true],  // 单项数组
    [true, false, true, false],  // 多项数组
    [for i = 0; i < 100; i = i + 1].map(fn(_) { true }),  // 大数组
    [true, false]  // 混合数组
  ]
  
  // 测试各种属性组合
  let mut i = 0
  while i < string_attributes.length() {
    let mut j = 0
    while j < int_attributes.length() {
      let mut k = 0
      while k < float_attributes.length() {
        let mut l = 0
        while l < bool_attributes.length() {
          // 测试基本属性组合
          counter.add(1L, Some([
            ("string.attr", AttributeValue::string(string_attributes[i])),
            ("int.attr", AttributeValue::int(int_attributes[j])),
            ("float.attr", AttributeValue::float(float_attributes[k])),
            ("bool.attr", AttributeValue::bool(bool_attributes[l]))
          ]))
          
          l = l + 1
        }
        k = k + 1
      }
      j = j + 1
    }
    i = i + 1
  }
  
  // 测试数组属性组合
  let m = 0
  while m < array_string_attributes.length() {
    counter.add(1L, Some([
      ("array.string.attr", AttributeValue::array_string(array_string_attributes[m]))
    ]))
    m = m + 1
  }
  
  let n = 0
  while n < array_int_attributes.length() {
    counter.add(1L, Some([
      ("array.int.attr", AttributeValue::array_int(array_int_attributes[n]))
    ]))
    n = n + 1
  }
  
  let o = 0
  while o < array_float_attributes.length() {
    counter.add(1L, Some([
      ("array.float.attr", AttributeValue::array_float(array_float_attributes[o]))
    ]))
    o = o + 1
  }
  
  let p = 0
  while p < array_bool_attributes.length() {
    counter.add(1L, Some([
      ("array.bool.attr", AttributeValue::array_bool(array_bool_attributes[p]))
    ]))
    p = p + 1
  }
}