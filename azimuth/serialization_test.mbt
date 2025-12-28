// Serialization Test Cases for Azimuth Telemetry System
// Testing data serialization and type conversions

test "attribute value string serialization" {
  let string_value = StringValue("test string")
  
  // Test string value pattern matching for serialization
  let serialized = match string_value {
    StringValue(s) => "string:" + s
    IntValue(_) => "int"
    FloatValue(_) => "float"
    BoolValue(_) => "bool"
    ArrayStringValue(_) => "array_string"
    ArrayIntValue(_) => "array_int"
  }
  
  assert_eq(serialized, "string:test string")
}

test "attribute value int serialization" {
  let int_value = IntValue(42)
  
  // Test int value pattern matching for serialization
  let serialized = match int_value {
    StringValue(_) => "string"
    IntValue(i) => "int:" + i.to_string()
    FloatValue(_) => "float"
    BoolValue(_) => "bool"
    ArrayStringValue(_) => "array_string"
    ArrayIntValue(_) => "array_int"
  }
  
  assert_eq(serialized, "int:42")
}

test "attribute value float serialization" {
  let float_value = FloatValue(3.14)
  
  // Test float value pattern matching for serialization
  let serialized = match float_value {
    StringValue(_) => "string"
    IntValue(_) => "int"
    FloatValue(f) => "float:" + f.to_string()
    BoolValue(_) => "bool"
    ArrayStringValue(_) => "array_string"
    ArrayIntValue(_) => "array_int"
  }
  
  assert_eq(serialized, "float:3.14")
}

test "attribute value bool serialization" {
  let bool_value = BoolValue(true)
  
  // Test bool value pattern matching for serialization
  let serialized = match bool_value {
    StringValue(_) => "string"
    IntValue(_) => "int"
    FloatValue(_) => "float"
    BoolValue(b) => "bool:" + (if b { "true" } else { "false" })
    ArrayStringValue(_) => "array_string"
    ArrayIntValue(_) => "array_int"
  }
  
  assert_eq(serialized, "bool:true")
}

test "attribute value array serialization" {
  let string_array = ArrayStringValue(["value1", "value2", "value3"])
  let int_array = ArrayIntValue([1, 2, 3, 4, 5])
  
  // Test string array serialization
  let string_serialized = match string_array {
    StringValue(_) => "string"
    IntValue(_) => "int"
    FloatValue(_) => "float"
    BoolValue(_) => "bool"
    ArrayStringValue(arr) => "array_string:" + arr.length().to_string()
    ArrayIntValue(_) => "array_int"
  }
  
  // Test int array serialization
  let int_serialized = match int_array {
    StringValue(_) => "string"
    IntValue(_) => "int"
    FloatValue(_) => "float"
    BoolValue(_) => "bool"
    ArrayStringValue(_) => "array_string"
    ArrayIntValue(arr) => "array_int:" + arr.length().to_string()
  }
  
  assert_eq(string_serialized, "array_string:3")
  assert_eq(int_serialized, "array_int:5")
}

test "span kind serialization" {
  let internal_kind = Internal
  let server_kind = Server
  let client_kind = Client
  let producer_kind = Producer
  let consumer_kind = Consumer
  
  // Test span kind serialization
  let internal_serialized = match internal_kind {
    Internal => "internal"
    Server => "server"
    Client => "client"
    Producer => "producer"
    Consumer => "consumer"
  }
  
  let server_serialized = match server_kind {
    Internal => "internal"
    Server => "server"
    Client => "client"
    Producer => "producer"
    Consumer => "consumer"
  }
  
  assert_eq(internal_serialized, "internal")
  assert_eq(server_serialized, "server")
}

test "status code serialization" {
  let unset_status = Unset
  let ok_status = Ok
  let error_status = Error
  
  // Test status code serialization
  let unset_serialized = match unset_status {
    Unset => "unset"
    Ok => "ok"
    Error => "error"
  }
  
  let ok_serialized = match ok_status {
    Unset => "unset"
    Ok => "ok"
    Error => "error"
  }
  
  let error_serialized = match error_status {
    Unset => "unset"
    Ok => "ok"
    Error => "error"
  }
  
  assert_eq(unset_serialized, "unset")
  assert_eq(ok_serialized, "ok")
  assert_eq(error_serialized, "error")
}

test "severity number serialization" {
  let trace_severity = Trace
  let debug_severity = Debug
  let info_severity = Info
  let warn_severity = Warn
  let error_severity = Error
  let fatal_severity = Fatal
  
  // Test severity number serialization
  let trace_serialized = match trace_severity {
    Trace => "trace"
    Debug => "debug"
    Info => "info"
    Warn => "warn"
    Error => "error"
    Fatal => "fatal"
  }
  
  let info_serialized = match info_severity {
    Trace => "trace"
    Debug => "debug"
    Info => "info"
    Warn => "warn"
    Error => "error"
    Fatal => "fatal"
  }
  
  assert_eq(trace_serialized, "trace")
  assert_eq(info_serialized, "info")
}

test "instrument type serialization" {
  let counter_instr = Counter("test.counter", Some("Test counter"), Some("count"))
  let histogram_instr = Histogram("test.histogram", Some("Test histogram"), Some("ms"))
  let updown_counter_instr = UpDownCounter("test.updown", Some("Test updown"), Some("value"))
  let gauge_instr = Gauge("test.gauge", Some("Test gauge"), Some("percent"))
  
  // Test instrument type serialization
  let counter_serialized = match counter_instr {
    Counter(name, desc, unit) => "counter:" + name
    Histogram(_, _, _) => "histogram"
    UpDownCounter(_, _, _) => "updown_counter"
    Gauge(_, _, _) => "gauge"
  }
  
  let histogram_serialized = match histogram_instr {
    Counter(_, _, _) => "counter"
    Histogram(name, desc, unit) => "histogram:" + name
    UpDownCounter(_, _, _) => "updown_counter"
    Gauge(_, _, _) => "gauge"
  }
  
  assert_eq(counter_serialized, "counter:test.counter")
  assert_eq(histogram_serialized, "histogram:test.histogram")
}

test "complex nested serialization" {
  let attributes = [
    ("service.name", StringValue("test-service")),
    ("process.pid", IntValue(1234)),
    ("process.cpu.percent", FloatValue(75.5)),
    ("process.debug", BoolValue(true))
  ]
  
  // Test complex attribute serialization
  let serialized_count = attributes.map(fn((key, value)) {
    match value {
      StringValue(_) => "string"
      IntValue(_) => "int"
      FloatValue(_) => "float"
      BoolValue(_) => "bool"
      ArrayStringValue(_) => "array_string"
      ArrayIntValue(_) => "array_int"
    }
  })
  
  assert_eq(serialized_count.length(), 4)
  assert_eq(serialized_count[0], "string")
  assert_eq(serialized_count[1], "int")
  assert_eq(serialized_count[2], "float")
  assert_eq(serialized_count[3], "bool")
}