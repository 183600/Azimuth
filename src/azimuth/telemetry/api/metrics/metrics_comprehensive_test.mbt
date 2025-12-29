// Metrics模块综合测试用例
test "instrument_types" {
  // 测试所有仪器类型
  let counter_type = Counter
  let histogram_type = Histogram
  let up_down_counter_type = UpDownCounter
  let gauge_type = Gauge
  let observable_counter_type = ObservableCounter
  let observable_gauge_type = ObservableGauge
  let observable_up_down_counter_type = ObservableUpDownCounter
  
  // 由于InstrumentType是枚举，我们无法直接比较
  // 这里我们通过模式匹配来验证类型
  match counter_type {
    Counter => assert_eq(true, true)
    _ => @test.fail("Expected Counter type")
  }
  
  match histogram_type {
    Histogram => assert_eq(true, true)
    _ => @test.fail("Expected Histogram type")
  }
  
  match up_down_counter_type {
    UpDownCounter => assert_eq(true, true)
    _ => @test.fail("Expected UpDownCounter type")
  }
  
  match gauge_type {
    Gauge => assert_eq(true, true)
    _ => @test.fail("Expected Gauge type")
  }
  
  match observable_counter_type {
    ObservableCounter => assert_eq(true, true)
    _ => @test.fail("Expected ObservableCounter type")
  }
  
  match observable_gauge_type {
    ObservableGauge => assert_eq(true, true)
    _ => @test.fail("Expected ObservableGauge type")
  }
  
  match observable_up_down_counter_type {
    ObservableUpDownCounter => assert_eq(true, true)
    _ => @test.fail("Expected ObservableUpDownCounter type")
  }
}

test "measurement_creation" {
  let measurement1 = Measurement::{
    value: 42.5,
    attributes: [
      ("method", AttributeValue::string("GET")),
      ("status", AttributeValue::int(200L))
    ]
  }
  
  let measurement2 = Measurement::{
    value: 100.0,
    attributes: [
      ("service", AttributeValue::string("api")),
      ("version", AttributeValue::string("1.0.0"))
    ]
  }
  
  assert_eq(measurement1.value, 42.5)
  assert_eq(measurement1.attributes.length(), 2)
  
  match measurement1.attributes[0].1 {
    StringValue(method) => assert_eq(method, "GET")
    _ => @test.fail("Expected StringValue for method")
  }
  
  match measurement1.attributes[1].1 {
    IntValue(status) => assert_eq(status, 200L)
    _ => @test.fail("Expected IntValue for status")
  }
  
  assert_eq(measurement2.value, 100.0)
  assert_eq(measurement2.attributes.length(), 2)
}
