// Azimuth 遥测数据序列化测试用例
// 包含遥测数据的序列化、反序列化和格式转换

// 测试1: JSON序列化和反序列化
test "遥测数据JSON序列化和反序列化" {
  // 定义遥测Span数据结构
  type TelemetrySpan = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // 简化的JSON序列化函数
  let span_to_json = fn(span: TelemetrySpan) {
    let mut json = "{"
    json = json + "\"trace_id\":\"" + span.trace_id + "\"," 
    json = json + "\"span_id\":\"" + span.span_id + "\"," 
    
    match span.parent_span_id {
      Some(parent_id) => {
        json = json + "\"parent_span_id\":\"" + parent_id + "\"," 
      }
      None => {
        json = json + "\"parent_span_id\":null," 
      }
    }
    
    json = json + "\"operation_name\":\"" + span.operation_name + "\"," 
    json = json + "\"start_time\":" + span.start_time.to_string() + "," 
    json = json + "\"end_time\":" + span.end_time.to_string() + "," 
    json = json + "\"status\":\"" + span.status + "\"," 
    
    json = json + "\"attributes\":["
    for i in 0..span.attributes.length() {
      let (key, value) = span.attributes[i]
      json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      if i < span.attributes.length() - 1 {
        json = json + "," 
      }
    }
    json = json + "]" 
    
    json = json + "}" 
    json
  }
  
  // 简化的JSON反序列化函数
  let json_to_span = fn(json: String) {
    // 提取trace_id
    let trace_id_start = json.find("\"trace_id\":\"") + 12
    let trace_id_end = json.find("\",\"", trace_id_start)
    let trace_id = json.substring(trace_id_start, trace_id_end - trace_id_start)
    
    // 提取span_id
    let span_id_start = json.find("\"span_id\":\"") + 11
    let span_id_end = json.find("\",\"", span_id_start)
    let span_id = json.substring(span_id_start, span_id_end - span_id_start)
    
    // 提取parent_span_id（可能为null）
    let parent_span_id_start = json.find("\"parent_span_id\":") + 17
    let parent_span_id_end = json.find("\",\"", parent_span_id_start)
    let parent_span_id = if parent_span_id_end > parent_span_id_start {
      Some(json.substring(parent_span_id_start, parent_span_id_end - parent_span_id_start))
    } else {
      None
    }
    
    // 提取operation_name
    let operation_name_start = json.find("\"operation_name\":\"") + 18
    let operation_name_end = json.find("\",\"", operation_name_start)
    let operation_name = json.substring(operation_name_start, operation_name_end - operation_name_start)
    
    // 提取start_time
    let start_time_start = json.find("\"start_time\":") + 13
    let start_time_end = json.find(",", start_time_start)
    let start_time = str_to_int(json.substring(start_time_start, start_time_end - start_time_start))
    
    // 提取end_time
    let end_time_start = json.find("\"end_time\":") + 11
    let end_time_end = json.find(",", end_time_start)
    let end_time = str_to_int(json.substring(end_time_start, end_time_end - end_time_start))
    
    // 提取status
    let status_start = json.find("\"status\":\"") + 10
    let status_end = json.find("\",\"", status_start)
    let status = json.substring(status_start, status_end - status_start)
    
    // 简化的属性解析（仅用于演示）
    let attributes = []
    
    {
      trace_id,
      span_id,
      parent_span_id,
      operation_name,
      start_time,
      end_time,
      status,
      attributes
    }
  }
  
  // 创建测试Span
  let test_span = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database_query",
    start_time: 1640995200000,
    end_time: 1640995250000,
    status: "ok",
    attributes: [
      ("service.name", "payment-service"),
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM orders")
    ]
  }
  
  // 测试序列化
  let json = span_to_json(test_span)
  assert_true(json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json.contains("\"span_id\":\"span-67890\""))
  assert_true(json.contains("\"parent_span_id\":\"span-11111\""))
  assert_true(json.contains("\"operation_name\":\"database_query\""))
  assert_true(json.contains("\"start_time\":1640995200000"))
  assert_true(json.contains("\"end_time\":1640995250000"))
  assert_true(json.contains("\"status\":\"ok\""))
  assert_true(json.contains("\"attributes\""))
  assert_true(json.contains("\"key\":\"service.name\""))
  assert_true(json.contains("\"value\":\"payment-service\""))
  
  // 测试反序列化
  let deserialized_span = json_to_span(json)
  assert_eq(deserialized_span.trace_id, test_span.trace_id)
  assert_eq(deserialized_span.span_id, test_span.span_id)
  assert_eq(deserialized_span.parent_span_id, test_span.parent_span_id)
  assert_eq(deserialized_span.operation_name, test_span.operation_name)
  assert_eq(deserialized_span.start_time, test_span.start_time)
  assert_eq(deserialized_span.end_time, test_span.end_time)
  assert_eq(deserialized_span.status, test_span.status)
  
  // 测试没有父Span的情况
  let orphan_span = {
    trace_id: "trace-54321",
    span_id: "span-98765",
    parent_span_id: None,
    operation_name: "http_request",
    start_time: 1640995300000,
    end_time: 1640995320000,
    status: "error",
    attributes: [
      ("http.method", "GET"),
      ("http.url", "/api/orders"),
      ("http.status_code", "500")
    ]
  }
  
  let orphan_json = span_to_json(orphan_span)
  assert_true(orphan_json.contains("\"parent_span_id\":null"))
  
  let deserialized_orphan = json_to_span(orphan_json)
  assert_eq(deserialized_orphan.parent_span_id, None)
}

// 测试2: 二进制序列化
test "遥测数据二进制序列化" {
  // 定义二进制格式常量
  let SPAN_MARKER = 0x53  // 'S'
  let ATTRIBUTE_MARKER = 0x41  // 'A'
  let STRING_MARKER = 0x54  // 'T'
  let INT_MARKER = 0x49  // 'I'
  let NULL_MARKER = 0x4E  // 'N'
  
  // 将Span序列化为二进制格式
  let span_to_binary = fn(span: TelemetrySpan) {
    let mut binary = []
    
    // 添加Span标记
    binary = binary.push(SPAN_MARKER)
    
    // 序列化trace_id
    binary = binary.push(STRING_MARKER)
    let trace_id_bytes = span.trace_id.to_byte_array()
    binary = binary.push(trace_id_bytes.length() as Int)
    for byte in trace_id_bytes {
      binary = binary.push(byte)
    }
    
    // 序列化span_id
    binary = binary.push(STRING_MARKER)
    let span_id_bytes = span.span_id.to_byte_array()
    binary = binary.push(span_id_bytes.length() as Int)
    for byte in span_id_bytes {
      binary = binary.push(byte)
    }
    
    // 序列化parent_span_id
    match span.parent_span_id {
      Some(parent_id) => {
        binary = binary.push(STRING_MARKER)
        let parent_id_bytes = parent_id.to_byte_array()
        binary = binary.push(parent_id_bytes.length() as Int)
        for byte in parent_id_bytes {
          binary = binary.push(byte)
        }
      }
      None => {
        binary = binary.push(NULL_MARKER)
      }
    }
    
    // 序列化operation_name
    binary = binary.push(STRING_MARKER)
    let operation_name_bytes = span.operation_name.to_byte_array()
    binary = binary.push(operation_name_bytes.length() as Int)
    for byte in operation_name_bytes {
      binary = binary.push(byte)
    }
    
    // 序列化start_time
    binary = binary.push(INT_MARKER)
    let start_time_bytes = int_to_bytes(span.start_time)
    for byte in start_time_bytes {
      binary = binary.push(byte)
    }
    
    // 序列化end_time
    binary = binary.push(INT_MARKER)
    let end_time_bytes = int_to_bytes(span.end_time)
    for byte in end_time_bytes {
      binary = binary.push(byte)
    }
    
    // 序列化status
    binary = binary.push(STRING_MARKER)
    let status_bytes = span.status.to_byte_array()
    binary = binary.push(status_bytes.length() as Int)
    for byte in status_bytes {
      binary = binary.push(byte)
    }
    
    // 序列化属性数量
    binary = binary.push(span.attributes.length() as Int)
    
    // 序列化每个属性
    for (key, value) in span.attributes {
      binary = binary.push(ATTRIBUTE_MARKER)
      
      // 序列化key
      binary = binary.push(STRING_MARKER)
      let key_bytes = key.to_byte_array()
      binary = binary.push(key_bytes.length() as Int)
      for byte in key_bytes {
        binary = binary.push(byte)
      }
      
      // 序列化value
      binary = binary.push(STRING_MARKER)
      let value_bytes = value.to_byte_array()
      binary = binary.push(value_bytes.length() as Int)
      for byte in value_bytes {
        binary = binary.push(byte)
      }
    }
    
    binary
  }
  
  // 辅助函数：将整数转换为字节数组
  let int_to_bytes = fn(n: Int) {
    let mut bytes = []
    let mut value = n
    
    for i in 0..8 {
      bytes = bytes.push((value >> (i * 8)) & 0xFF)
    }
    
    bytes
  }
  
  // 从二进制格式反序列化Span
  let binary_to_span = fn(binary: Array[Int]) {
    let mut index = 0
    
    // 检查Span标记
    assert_eq(binary[index], SPAN_MARKER)
    index = index + 1
    
    // 反序列化trace_id
    assert_eq(binary[index], STRING_MARKER)
    index = index + 1
    let trace_id_length = binary[index]
    index = index + 1
    let trace_id_bytes = []
    for i in 0..trace_id_length {
      trace_id_bytes = trace_id_bytes.push(binary[index + i])
    }
    index = index + trace_id_length
    let trace_id = bytes_to_string(trace_id_bytes)
    
    // 反序列化span_id
    assert_eq(binary[index], STRING_MARKER)
    index = index + 1
    let span_id_length = binary[index]
    index = index + 1
    let span_id_bytes = []
    for i in 0..span_id_length {
      span_id_bytes = span_id_bytes.push(binary[index + i])
    }
    index = index + span_id_length
    let span_id = bytes_to_string(span_id_bytes)
    
    // 反序列化parent_span_id
    let parent_span_id = if binary[index] == STRING_MARKER {
      index = index + 1
      let parent_id_length = binary[index]
      index = index + 1
      let parent_id_bytes = []
      for i in 0..parent_id_length {
        parent_id_bytes = parent_id_bytes.push(binary[index + i])
      }
      index = index + parent_id_length
      Some(bytes_to_string(parent_id_bytes))
    } else if binary[index] == NULL_MARKER {
      index = index + 1
      None
    } else {
      None  // 错误情况
    }
    
    // 反序列化operation_name
    assert_eq(binary[index], STRING_MARKER)
    index = index + 1
    let operation_name_length = binary[index]
    index = index + 1
    let operation_name_bytes = []
    for i in 0..operation_name_length {
      operation_name_bytes = operation_name_bytes.push(binary[index + i])
    }
    index = index + operation_name_length
    let operation_name = bytes_to_string(operation_name_bytes)
    
    // 反序列化start_time
    assert_eq(binary[index], INT_MARKER)
    index = index + 1
    let start_time_bytes = []
    for i in 0..8 {
      start_time_bytes = start_time_bytes.push(binary[index + i])
    }
    index = index + 8
    let start_time = bytes_to_int(start_time_bytes)
    
    // 反序列化end_time
    assert_eq(binary[index], INT_MARKER)
    index = index + 1
    let end_time_bytes = []
    for i in 0..8 {
      end_time_bytes = end_time_bytes.push(binary[index + i])
    }
    index = index + 8
    let end_time = bytes_to_int(end_time_bytes)
    
    // 反序列化status
    assert_eq(binary[index], STRING_MARKER)
    index = index + 1
    let status_length = binary[index]
    index = index + 1
    let status_bytes = []
    for i in 0..status_length {
      status_bytes = status_bytes.push(binary[index + i])
    }
    index = index + status_length
    let status = bytes_to_string(status_bytes)
    
    // 反序列化属性数量
    let attributes_count = binary[index]
    index = index + 1
    
    // 反序列化属性
    let attributes = []
    for i in 0..attributes_count {
      assert_eq(binary[index], ATTRIBUTE_MARKER)
      index = index + 1
      
      // 反序列化key
      assert_eq(binary[index], STRING_MARKER)
      index = index + 1
      let key_length = binary[index]
      index = index + 1
      let key_bytes = []
      for j in 0..key_length {
        key_bytes = key_bytes.push(binary[index + j])
      }
      index = index + key_length
      let key = bytes_to_string(key_bytes)
      
      // 反序列化value
      assert_eq(binary[index], STRING_MARKER)
      index = index + 1
      let value_length = binary[index]
      index = index + 1
      let value_bytes = []
      for j in 0..value_length {
        value_bytes = value_bytes.push(binary[index + j])
      }
      index = index + value_length
      let value = bytes_to_string(value_bytes)
      
      attributes = attributes.push((key, value))
    }
    
    {
      trace_id,
      span_id,
      parent_span_id,
      operation_name,
      start_time,
      end_time,
      status,
      attributes
    }
  }
  
  // 辅助函数：将字节数组转换为字符串
  let bytes_to_string = fn(bytes: Array[Int]) {
    let mut result = ""
    for byte in bytes {
      result = result + (byte as Char).to_string()
    }
    result
  }
  
  // 辅助函数：将字节数组转换为整数
  let bytes_to_int = fn(bytes: Array[Int]) {
    let mut result = 0
    for i in 0..bytes.length() {
      result = result | (bytes[i] << (i * 8))
    }
    result
  }
  
  // 创建测试Span
  let test_span = {
    trace_id: "trace-abc123",
    span_id: "span-def456",
    parent_span_id: Some("span-ghi789"),
    operation_name: "cache_get",
    start_time: 1640995400000,
    end_time: 1640995400100,
    status: "ok",
    attributes: [
      ("cache.key", "user:123"),
      ("cache.hit", "true"),
      ("cache.ttl", "3600")
    ]
  }
  
  // 测试二进制序列化
  let binary = span_to_binary(test_span)
  assert_eq(binary[0], SPAN_MARKER)
  assert_eq(binary.length() > 0, true)
  
  // 测试二进制反序列化
  let deserialized_span = binary_to_span(binary)
  assert_eq(deserialized_span.trace_id, test_span.trace_id)
  assert_eq(deserialized_span.span_id, test_span.span_id)
  assert_eq(deserialized_span.parent_span_id, test_span.parent_span_id)
  assert_eq(deserialized_span.operation_name, test_span.operation_name)
  assert_eq(deserialized_span.start_time, test_span.start_time)
  assert_eq(deserialized_span.end_time, test_span.end_time)
  assert_eq(deserialized_span.status, test_span.status)
  assert_eq(deserialized_span.attributes.length(), test_span.attributes.length())
}

// 测试3: Protocol Buffers风格序列化
test "Protocol Buffers风格序列化" {
  // 定义字段编号常量
  let TRACE_ID_FIELD = 1
  let SPAN_ID_FIELD = 2
  let PARENT_SPAN_ID_FIELD = 3
  let OPERATION_NAME_FIELD = 4
  let START_TIME_FIELD = 5
  let END_TIME_FIELD = 6
  let STATUS_FIELD = 7
  let ATTRIBUTES_FIELD = 8
  
  // Varint编码函数
  let encode_varint = fn(value: Int) {
    let mut bytes = []
    let mut v = value
    
    while v >= 0x80 {
      bytes = bytes.push((v & 0x7F) | 0x80)
      v = v >> 7
    }
    
    bytes = bytes.push(v & 0x7F)
    bytes
  }
  
  // Varint解码函数
  let decode_varint = fn(bytes: Array[Int], start_index: Int) {
    let mut result = 0
    let mut shift = 0
    let mut index = start_index
    
    while index < bytes.length() && (bytes[index] & 0x80) != 0 {
      result = result | ((bytes[index] & 0x7F) << shift)
      shift = shift + 7
      index = index + 1
    }
    
    if index < bytes.length() {
      result = result | (bytes[index] << shift)
      index = index + 1
    }
    
    (result, index - start_index)
  }
  
  // 将字段编号和线类型编码为Varint
  let encode_key = fn(field_number: Int, wire_type: Int) {
    encode_varint((field_number << 3) | wire_type)
  }
  
  // 字符串字段编码
  let encode_string_field = fn(field_number: Int, value: String) {
    let mut result = []
    let value_bytes = value.to_byte_array()
    
    // 添加字段键
    result = result + encode_key(field_number, 2)  // 2 = Length-delimited
    
    // 添加长度
    result = result + encode_varint(value_bytes.length())
    
    // 添加值
    for byte in value_bytes {
      result = result.push(byte)
    }
    
    result
  }
  
  // 整数字段编码
  let encode_int_field = fn(field_number: Int, value: Int) {
    let mut result = []
    
    // 添加字段键
    result = result + encode_key(field_number, 0)  // 0 = Varint
    
    // 添加值
    result = result + encode_varint(value)
    
    result
  }
  
  // Span序列化为Protobuf风格格式
  let span_to_protobuf = fn(span: TelemetrySpan) {
    let mut result = []
    
    // 添加trace_id
    result = result + encode_string_field(TRACE_ID_FIELD, span.trace_id)
    
    // 添加span_id
    result = result + encode_string_field(SPAN_ID_FIELD, span.span_id)
    
    // 添加parent_span_id（如果存在）
    match span.parent_span_id {
      Some(parent_id) => {
        result = result + encode_string_field(PARENT_SPAN_ID_FIELD, parent_id)
      }
      None => {}
    }
    
    // 添加operation_name
    result = result + encode_string_field(OPERATION_NAME_FIELD, span.operation_name)
    
    // 添加start_time
    result = result + encode_int_field(START_TIME_FIELD, span.start_time)
    
    // 添加end_time
    result = result + encode_int_field(END_TIME_FIELD, span.end_time)
    
    // 添加status
    result = result + encode_string_field(STATUS_FIELD, span.status)
    
    // 添加属性
    for (key, value) in span.attributes {
      // 属性是嵌套消息，简化处理
      let mut attr_bytes = []
      attr_bytes = attr_bytes + encode_string_field(1, key)  // key字段
      attr_bytes = attr_bytes + encode_string_field(2, value)  // value字段
      
      // 将属性消息作为长度分隔的字段添加
      result = result + encode_key(ATTRIBUTES_FIELD, 2)  // 2 = Length-delimited
      result = result + encode_varint(attr_bytes.length())
      result = result + attr_bytes
    }
    
    result
  }
  
  // 从Protobuf风格格式反序列化Span
  let protobuf_to_span = fn(bytes: Array[Int]) {
    let mut index = 0
    let mut trace_id = ""
    let mut span_id = ""
    let mut parent_span_id = None
    let mut operation_name = ""
    let mut start_time = 0
    let mut end_time = 0
    let mut status = ""
    let attributes = []
    
    while index < bytes.length() {
      // 解码字段键
      let (key, key_length) = decode_varint(bytes, index)
      index = index + key_length
      
      let field_number = key >> 3
      let wire_type = key & 0x7
      
      match wire_type {
        0 => {  // Varint
          let (value, value_length) = decode_varint(bytes, index)
          index = index + value_length
          
          match field_number {
            START_TIME_FIELD => start_time = value
            END_TIME_FIELD => end_time = value
            _ => {}
          }
        }
        2 => {  // Length-delimited
          let (length, length_length) = decode_varint(bytes, index)
          index = index + length_length
          
          let value_bytes = []
          for i in 0..length {
            value_bytes = value_bytes.push(bytes[index + i])
          }
          index = index + length
          
          let value = bytes_to_string(value_bytes)
          
          match field_number {
            TRACE_ID_FIELD => trace_id = value
            SPAN_ID_FIELD => span_id = value
            PARENT_SPAN_ID_FIELD => parent_span_id = Some(value)
            OPERATION_NAME_FIELD => operation_name = value
            STATUS_FIELD => status = value
            ATTRIBUTES_FIELD => {
              // 解析嵌套的属性消息
              let mut attr_index = 0
              let mut attr_key = ""
              let mut attr_value = ""
              
              while attr_index < value_bytes.length() {
                let (attr_key_data, key_len) = decode_varint(value_bytes, attr_index)
                attr_index = attr_index + key_len
                
                let attr_field_number = attr_key_data >> 3
                let attr_wire_type = attr_key_data & 0x7
                
                if attr_wire_type == 2 {
                  let (attr_len, len_len) = decode_varint(value_bytes, attr_index)
                  attr_index = attr_index + len_len
                  
                  let attr_val_bytes = []
                  for i in 0..attr_len {
                    attr_val_bytes = attr_val_bytes.push(value_bytes[attr_index + i])
                  }
                  attr_index = attr_index + attr_len
                  
                  let attr_val = bytes_to_string(attr_val_bytes)
                  
                  match attr_field_number {
                    1 => attr_key = attr_val
                    2 => attr_value = attr_val
                    _ => {}
                  }
                }
              }
              
              attributes = attributes.push((attr_key, attr_value))
            }
            _ => {}
          }
        }
        _ => {
          // 跳过不支持的线类型
          index = index + 1
        }
      }
    }
    
    {
      trace_id,
      span_id,
      parent_span_id,
      operation_name,
      start_time,
      end_time,
      status,
      attributes
    }
  }
  
  // 创建测试Span
  let test_span = {
    trace_id: "trace-proto123",
    span_id: "span-proto456",
    parent_span_id: Some("span-proto789"),
    operation_name: "message_publish",
    start_time: 1640995500000,
    end_time: 1640995500500,
    status: "ok",
    attributes: [
      ("messaging.system", "kafka"),
      ("messaging.destination", "orders"),
      ("messaging.message_id", "msg-12345")
    ]
  }
  
  // 测试Protobuf风格序列化
  let protobuf = span_to_protobuf(test_span)
  assert_true(protobuf.length() > 0)
  
  // 测试Protobuf风格反序列化
  let deserialized_span = protobuf_to_span(protobuf)
  assert_eq(deserialized_span.trace_id, test_span.trace_id)
  assert_eq(deserialized_span.span_id, test_span.span_id)
  assert_eq(deserialized_span.parent_span_id, test_span.parent_span_id)
  assert_eq(deserialized_span.operation_name, test_span.operation_name)
  assert_eq(deserialized_span.start_time, test_span.start_time)
  assert_eq(deserialized_span.end_time, test_span.end_time)
  assert_eq(deserialized_span.status, test_span.status)
  assert_eq(deserialized_span.attributes.length(), test_span.attributes.length())
}

// 测试4: 批量序列化和反序列化
test "批量遥测数据序列化" {
  // 定义批量Span数据结构
  type SpanBatch = {
    resource: Array<(String, String)>,
    instrumentation_scope: Array<(String, String)>,
    spans: Array[TelemetrySpan]
  }
  
  // 批量JSON序列化
  let batch_to_json = fn(batch: SpanBatch) {
    let mut json = "{"
    
    // 序列化resource
    json = json + "\"resource\":{"
    json = json + "\"attributes\":["
    for i in 0..batch.resource.length() {
      let (key, value) = batch.resource[i]
      json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      if i < batch.resource.length() - 1 {
        json = json + "," 
      }
    }
    json = json + "]},"
    
    // 序列化instrumentation_scope
    json = json + "\"instrumentation_scope\":{"
    json = json + "\"attributes\":["
    for i in 0..batch.instrumentation_scope.length() {
      let (key, value) = batch.instrumentation_scope[i]
      json = json + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
      if i < batch.instrumentation_scope.length() - 1 {
        json = json + "," 
      }
    }
    json = json + "]},"
    
    // 序列化spans
    json = json + "\"spans\":["
    for i in 0..batch.spans.length() {
      json = json + span_to_json(batch.spans[i])
      if i < batch.spans.length() - 1 {
        json = json + "," 
      }
    }
    json = json + "]" 
    
    json = json + "}" 
    json
  }
  
  // 创建测试Span集合
  let span1 = {
    trace_id: "trace-batch-1",
    span_id: "span-batch-1",
    parent_span_id: None,
    operation_name: "http_server_request",
    start_time: 1640995600000,
    end_time: 1640995600100,
    status: "ok",
    attributes: [
      ("http.method", "GET"),
      ("http.route", "/api/users"),
      ("http.status_code", "200")
    ]
  }
  
  let span2 = {
    trace_id: "trace-batch-1",
    span_id: "span-batch-2",
    parent_span_id: Some("span-batch-1"),
    operation_name: "database_query",
    start_time: 1640995600020,
    end_time: 1640995600080,
    status: "ok",
    attributes: [
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = $1"),
      ("db.rows_affected", "1")
    ]
  }
  
  let span3 = {
    trace_id: "trace-batch-2",
    span_id: "span-batch-3",
    parent_span_id: None,
    operation_name: "cache_operation",
    start_time: 1640995601000,
    end_time: 1640995601050,
    status: "error",
    attributes: [
      ("cache.operation", "set"),
      ("cache.key", "user:123"),
      ("error.type", "timeout")
    ]
  }
  
  // 创建测试批次
  let test_batch = {
    resource: [
      ("service.name", "user-service"),
      ("service.version", "1.2.3"),
      ("deployment.environment", "production")
    ],
    instrumentation_scope: [
      ("name", "azimuth-instrumentation"),
      ("version", "0.1.0")
    ],
    spans: [span1, span2, span3]
  }
  
  // 测试批量序列化
  let batch_json = batch_to_json(test_batch)
  assert_true(batch_json.contains("\"resource\""))
  assert_true(batch_json.contains("\"instrumentation_scope\""))
  assert_true(batch_json.contains("\"spans\""))
  assert_true(batch_json.contains("service.name"))
  assert_true(batch_json.contains("azimuth-instrumentation"))
  assert_true(batch_json.contains("trace-batch-1"))
  assert_true(batch_json.contains("trace-batch-2"))
  
  // 计算序列化大小
  let json_size = batch_json.length()
  assert_true(json_size > 0)
  
  // 计算压缩率（简化版）
  let original_size = test_batch.spans.length() * 100  // 假设每个Span原始大小为100字节
  let compression_ratio = (json_size as Float) / (original_size as Float)
  
  // 验证压缩率在合理范围内
  assert_true(compression_ratio > 0.1 && compression_ratio < 10.0)
  
  // 测试大批量序列化性能
  let large_batch = {
    resource: test_batch.resource,
    instrumentation_scope: test_batch.instrumentation_scope,
    spans: []
  }
  
  // 创建1000个Span
  for i in 0..1000 {
    let large_span = {
      trace_id: "trace-large-" + i.to_string(),
      span_id: "span-large-" + i.to_string(),
      parent_span_id: None,
      operation_name: "operation_" + i.to_string(),
      start_time: 1640995600000 + i * 10,
      end_time: 1640995600050 + i * 10,
      status: "ok",
      attributes: [
        ("index", i.to_string()),
        ("batch", "large")
      ]
    }
    large_batch.spans = large_batch.spans.push(large_span)
  }
  
  // 测试大批量序列化
  let large_batch_json = batch_to_json(large_batch)
  assert_true(large_batch_json.length() > batch_json.length())
  
  // 验证所有Span都包含在JSON中
  assert_true(large_batch_json.contains("trace-large-0"))
  assert_true(large_batch_json.contains("trace-large-999"))
}