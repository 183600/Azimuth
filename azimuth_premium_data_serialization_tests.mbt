// Azimuth Premium Data Serialization Tests
// 高级数据序列化测试用例，专注于遥测数据的序列化和反序列化性能与正确性

// 测试 1: JSON 序列化和反序列化
test "JSON serialization and deserialization" {
  // 定义遥测数据结构
  type TelemetryData = {
    trace_id: String,
    span_id: String,
    parent_span_id: Option[String],
    operation_name: String,
    start_time: Int,
    end_time: Int,
    status: String,
    attributes: Array[(String, String)],
    events: Array[(Int, String, Array[(String, String)])]
  }
  
  // 简化的JSON序列化器
  let json_serialize = fn(data: TelemetryData) {
    // 序列化属性数组
    let serialize_attributes = fn(attrs: Array[(String, String)]) {
      let mut result = "["
      for i in 0..attrs.length() {
        let (key, value) = attrs[i]
        result = result + "{\"key\":\"" + key + "\",\"value\":\"" + value + "\"}"
        if i < attrs.length() - 1 {
          result = result + ","
        }
      }
      result = result + "]"
      result
    }
    
    // 序列化事件数组
    let serialize_events = fn(events: Array[(Int, String, Array[(String, String)])]) {
      let mut result = "["
      for i in 0..events.length() {
        let (timestamp, name, attrs) = events[i]
        result = result + "{\"timestamp\":" + timestamp.to_string() + ",\"name\":\"" + name + "\",\"attributes\":" + serialize_attributes(attrs) + "}"
        if i < events.length() - 1 {
          result = result + ","
        }
      }
      result = result + "]"
      result
    }
    
    // 序列化可选的父span ID
    let parent_span_json = match data.parent_span_id {
      Some(id) => "\"" + id + "\""
      None => "null"
    }
    
    // 构建JSON字符串
    let json = "{" +
      "\"trace_id\":\"" + data.trace_id + "\"," +
      "\"span_id\":\"" + data.span_id + "\"," +
      "\"parent_span_id\":" + parent_span_json + "," +
      "\"operation_name\":\"" + data.operation_name + "\"," +
      "\"start_time\":" + data.start_time.to_string() + "," +
      "\"end_time\":" + data.end_time.to_string() + "," +
      "\"status\":\"" + data.status + "\"," +
      "\"attributes\":" + serialize_attributes(data.attributes) + "," +
      "\"events\":" + serialize_events(data.events) +
      "}"
    
    json
  }
  
  // 简化的JSON反序列化器
  let json_deserialize = fn(json: String) {
    // 简化的解析实现，实际项目中应使用专门的JSON库
    let parse_string = fn(s: String, start: Int) {
      let mut result = ""
      let mut i = start + 1  // 跳过开始引号
      let mut escaped = false
      
      while i < s.length() && (s[i] != '"' || escaped) {
        if s[i] == '\\' && not escaped {
          escaped = true
        } else {
          result = result + s[i].to_string()
          escaped = false
        }
        i = i + 1
      }
      
      (result, i + 1)  // 返回结果和结束位置
    }
    
    let parse_array = fn(s: String, start: Int) {
      let mut result = []
      let mut i = start + 1  // 跳过开始括号
      let mut in_string = false
      let mut escaped = false
      let mut current = ""
      let mut depth = 0
      
      while i < s.length() && (s[i] != ']' || depth > 0) {
        if s[i] == '"' && not escaped {
          in_string = not in_string
        } else if s[i] == '\\' && in_string {
          escaped = not escaped
        } else if s[i] == '{' && not in_string {
          depth = depth + 1
        } else if s[i] == '}' && not in_string {
          depth = depth - 1
        } else if s[i] == ',' && not in_string && depth == 0 {
          result = result.push(current)
          current = ""
        } else {
          current = current + s[i].to_string()
        }
        
        if s[i] != '\\' {
          escaped = false
        }
        
        i = i + 1
      }
      
      if current.length() > 0 {
        result = result.push(current)
      }
      
      (result, i + 1)  // 返回结果和结束位置
    }
    
    // 解析JSON字符串（简化版）
    let mut trace_id = ""
    let mut span_id = ""
    let mut parent_span_id = None
    let mut operation_name = ""
    let mut start_time = 0
    let mut end_time = 0
    let mut status = ""
    let mut attributes = []
    let mut events = []
    
    // 简化的字段提取（实际实现会更复杂）
    let mut i = 0
    while i < json.length() {
      if json.substring(i, i + 10) == "\"trace_id\"" {
        let (value, next_i) = parse_string(json, i + 12)
        trace_id = value
        i = next_i
      } else if json.substring(i, i + 8) == "\"span_id\"" {
        let (value, next_i) = parse_string(json, i + 10)
        span_id = value
        i = next_i
      } else if json.substring(i, i + 15) == "\"parent_span_id\"" {
        if json[i + 17] == 'n' && json[i + 18] == 'u' && json[i + 19] == 'l' && json[i + 20] == 'l' {
          parent_span_id = None
          i = i + 21
        } else {
          let (value, next_i) = parse_string(json, i + 17)
          parent_span_id = Some(value)
          i = next_i
        }
      } else if json.substring(i, i + 15) == "\"operation_name\"" {
        let (value, next_i) = parse_string(json, i + 17)
        operation_name = value
        i = next_i
      } else if json.substring(i, i + 11) == "\"start_time\"" {
        let mut num_str = ""
        let mut j = i + 13
        while j < json.length() && json[j] != ',' && json[j] != '}' {
          num_str = num_str + json[j].to_string()
          j = j + 1
        }
        start_time = num_str.to_int()
        i = j
      } else if json.substring(i, i + 9) == "\"end_time\"" {
        let mut num_str = ""
        let mut j = i + 11
        while j < json.length() && json[j] != ',' && json[j] != '}' {
          num_str = num_str + json[j].to_string()
          j = j + 1
        }
        end_time = num_str.to_int()
        i = j
      } else if json.substring(i, i + 7) == "\"status\"" {
        let (value, next_i) = parse_string(json, i + 9)
        status = value
        i = next_i
      } else {
        i = i + 1
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
      attributes,
      events
    }
  }
  
  // 测试序列化和反序列化
  let telemetry_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    parent_span_id: Some("span-11111"),
    operation_name: "database.query",
    start_time: 1634567890123,
    end_time: 1634567890456,
    status: "OK",
    attributes: [
      ("db.type", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = $1"),
      ("db.instance", "prod-db-01")
    ],
    events: [
      (1634567890200, "query.start", []),
      (1634567890400, "query.execute", [("rows.returned", "42")])
    ]
  }
  
  // 序列化
  let json = json_serialize(telemetry_data)
  
  // 验证JSON包含关键字段
  assert_true(json.contains("\"trace_id\":\"trace-12345\""))
  assert_true(json.contains("\"span_id\":\"span-67890\""))
  assert_true(json.contains("\"parent_span_id\":\"span-11111\""))
  assert_true(json.contains("\"operation_name\":\"database.query\""))
  assert_true(json.contains("\"status\":\"OK\""))
  assert_true(json.contains("\"db.type\":\"postgresql\""))
  
  // 反序列化
  let deserialized_data = json_deserialize(json)
  
  // 验证反序列化结果
  assert_eq(deserialized_data.trace_id, telemetry_data.trace_id)
  assert_eq(deserialized_data.span_id, telemetry_data.span_id)
  assert_eq(deserialized_data.parent_span_id, telemetry_data.parent_span_id)
  assert_eq(deserialized_data.operation_name, telemetry_data.operation_name)
  assert_eq(deserialized_data.start_time, telemetry_data.start_time)
  assert_eq(deserialized_data.end_time, telemetry_data.end_time)
  assert_eq(deserialized_data.status, telemetry_data.status)
}

// 测试 2: 二进制序列化
test "binary serialization format" {
  // 定义二进制序列化格式
  type BinaryFormat = {
    magic: Array[Int],  // 魔数
    version: Int,       // 版本号
    flags: Int,         // 标志位
    data: Array[Int]    // 数据
  }
  
  // 创建二进制序列化器
  let create_binary_serializer = fn() {
    // 写入变长整数
    let write_varint = fn(buffer: Array[Int], value: Int) {
      let mut result = buffer
      let mut v = value
      
      while v >= 0x80 {
        result = result.push((v & 0x7F) | 0x80)
        v = v >> 7
      }
      
      result = result.push(v & 0x7F)
      result
    }
    
    // 写入字符串
    let write_string = fn(buffer: Array[Int], s: String) {
      let mut result = write_varint(buffer, s.length())
      
      for c in s.chars() {
        result = result.push(c.to_int())
      }
      
      result
    }
    
    // 写入数组
    let write_array = fn(buffer: Array[Int], arr: Array[Int]) {
      let mut result = write_varint(buffer, arr.length())
      
      for item in arr {
        result = result.push(item)
      }
      
      result
    }
    
    {
      write_varint,
      write_string,
      write_array
    }
  }
  
  // 创建二进制反序列化器
  let create_binary_deserializer = fn() {
    // 读取变长整数
    let read_varint = fn(buffer: Array[Int], offset: Int) {
      let mut result = 0
      let mut shift = 0
      let mut i = offset
      
      while i < buffer.length() && (buffer[i] & 0x80) != 0 {
        result = result | ((buffer[i] & 0x7F) << shift)
        shift = shift + 7
        i = i + 1
      }
      
      if i < buffer.length() {
        result = result | (buffer[i] << shift)
        i = i + 1
      }
      
      (result, i)
    }
    
    // 读取字符串
    let read_string = fn(buffer: Array[Int], offset: Int) {
      let (length, new_offset) = read_varint(buffer, offset)
      let mut result = ""
      
      for i in 0..length {
        if new_offset + i < buffer.length() {
          result = result + (buffer[new_offset + i] as Char).to_string()
        }
      }
      
      (result, new_offset + length)
    }
    
    // 读取数组
    let read_array = fn(buffer: Array[Int], offset: Int) {
      let (length, new_offset) = read_varint(buffer, offset)
      let mut result = []
      
      for i in 0..length {
        if new_offset + i < buffer.length() {
          result = result.push(buffer[new_offset + i])
        }
      }
      
      (result, new_offset + length)
    }
    
    {
      read_varint,
      read_string,
      read_array
    }
  }
  
  // 测试二进制序列化
  let serializer = create_binary_serializer()
  let deserializer = create_binary_deserializer()
  
  // 创建测试数据
  let test_data = {
    trace_id: "trace-12345",
    span_id: "span-67890",
    timestamp: 1634567890123,
    duration: 56789,
    status_code: 1,
    attributes: [
      ("service.name", "payment-service"),
      ("environment", "production"),
      ("region", "us-west-2")
    ]
  }
  
  // 序列化
  let mut buffer = []
  
  // 写入魔数
  buffer = serializer.write_array(buffer, [0x41, 0x5A, 0x4D, 0x54])  // "AZMT"
  
  // 写入版本号
  buffer = serializer.write_varint(buffer, 1)
  
  // 写入标志位
  buffer = serializer.write_varint(buffer, 0)
  
  // 写入追踪ID
  buffer = serializer.write_string(buffer, test_data.trace_id)
  
  // 写入Span ID
  buffer = serializer.write_string(buffer, test_data.span_id)
  
  // 写入时间戳
  buffer = serializer.write_varint(buffer, test_data.timestamp)
  
  // 写入持续时间
  buffer = serializer.write_varint(buffer, test_data.duration)
  
  // 写入状态码
  buffer = serializer.write_varint(buffer, test_data.status_code)
  
  // 写入属性数量
  buffer = serializer.write_varint(buffer, test_data.attributes.length())
  
  // 写入属性
  for (key, value) in test_data.attributes {
    buffer = serializer.write_string(buffer, key)
    buffer = serializer.write_string(buffer, value)
  }
  
  // 验证序列化结果
  assert_true(buffer.length() > 20)  // 至少包含基本头部信息
  assert_eq(buffer[0], 0x41)  // 魔数第一个字节
  assert_eq(buffer[1], 0x5A)  // 魔数第二个字节
  assert_eq(buffer[2], 0x4D)  // 魔数第三个字节
  assert_eq(buffer[3], 0x54)  // 魔数第四个字节
  
  // 反序列化
  let mut offset = 0
  
  // 读取魔数
  let (magic, new_offset) = deserializer.read_array(buffer, offset)
  offset = new_offset
  
  // 读取版本号
  let (version, new_offset2) = deserializer.read_varint(buffer, offset)
  offset = new_offset2
  
  // 读取标志位
  let (flags, new_offset3) = deserializer.read_varint(buffer, offset)
  offset = new_offset3
  
  // 读取追踪ID
  let (trace_id, new_offset4) = deserializer.read_string(buffer, offset)
  offset = new_offset4
  
  // 读取Span ID
  let (span_id, new_offset5) = deserializer.read_string(buffer, offset)
  offset = new_offset5
  
  // 读取时间戳
  let (timestamp, new_offset6) = deserializer.read_varint(buffer, offset)
  offset = new_offset6
  
  // 读取持续时间
  let (duration, new_offset7) = deserializer.read_varint(buffer, offset)
  offset = new_offset7
  
  // 读取状态码
  let (status_code, new_offset8) = deserializer.read_varint(buffer, offset)
  offset = new_offset8
  
  // 读取属性数量
  let (attr_count, new_offset9) = deserializer.read_varint(buffer, offset)
  offset = new_offset9
  
  // 读取属性
  let mut attributes = []
  for i in 0..attr_count {
    let (key, new_offset10) = deserializer.read_string(buffer, offset)
    offset = new_offset10
    
    let (value, new_offset11) = deserializer.read_string(buffer, offset)
    offset = new_offset11
    
    attributes = attributes.push((key, value))
  }
  
  // 验证反序列化结果
  assert_eq(magic, [0x41, 0x5A, 0x4D, 0x54])
  assert_eq(version, 1)
  assert_eq(flags, 0)
  assert_eq(trace_id, test_data.trace_id)
  assert_eq(span_id, test_data.span_id)
  assert_eq(timestamp, test_data.timestamp)
  assert_eq(duration, test_data.duration)
  assert_eq(status_code, test_data.status_code)
  assert_eq(attributes.length(), test_data.attributes.length())
  
  for i in 0..attributes.length() {
    assert_eq(attributes[i], test_data.attributes[i])
  }
}

// 测试 3: 压缩序列化
test "compressed serialization" {
  // 简化的压缩算法（行程编码）
  let compress_rle = fn(data: Array[Int]) {
    if data.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut current = data[0]
    let mut count = 1
    
    for i in 1..data.length() {
      if data[i] == current && count < 255 {
        count = count + 1
      } else {
        result = result.push(current)
        result = result.push(count)
        current = data[i]
        count = 1
      }
    }
    
    // 添加最后一组
    result = result.push(current)
    result = result.push(count)
    
    result
  }
  
  // 简化的解压缩算法
  let decompress_rle = fn(compressed: Array[Int]) {
    if compressed.length() == 0 || compressed.length() % 2 != 0 {
      return []
    }
    
    let mut result = []
    
    for i in 0..compressed.length() / 2 {
      let value = compressed[i * 2]
      let count = compressed[i * 2 + 1]
      
      for j in 0..count {
        result = result.push(value)
      }
    }
    
    result
  }
  
  // 测试压缩和解压缩
  let test_data = [1, 1, 1, 1, 1, 2, 2, 3, 3, 3, 4, 5, 5, 5, 5, 5, 5, 5, 5, 5]
  
  let compressed = compress_rle(test_data)
  
  // 验证压缩结果
  assert_eq(compressed.length(), 10)  // 5组值和计数
  assert_eq(compressed, [1, 5, 2, 2, 3, 3, 4, 1, 5, 9])
  
  // 验证压缩率
  let compression_ratio = (compressed.length() as Float) / (test_data.length() as Float)
  assert_true(compression_ratio < 1.0)  // 应该有压缩效果
  
  let decompressed = decompress_rle(compressed)
  
  // 验证解压缩结果
  assert_eq(decompressed.length(), test_data.length())
  for i in 0..test_data.length() {
    assert_eq(decompressed[i], test_data[i])
  }
  
  // 测试不重复数据的压缩
  let unique_data = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  
  let compressed_unique = compress_rle(unique_data)
  
  // 验证不重复数据的压缩
  assert_eq(compressed_unique.length(), unique_data.length() * 2)  // 每个值计数为1
  
  let decompressed_unique = decompress_rle(compressed_unique)
  
  // 验证解压缩结果
  assert_eq(decompressed_unique.length(), unique_data.length())
  for i in 0..unique_data.length() {
    assert_eq(decompressed_unique[i], unique_data[i])
  }
}

// 测试 4: 自定义序列化格式
test "custom serialization format" {
  // 定义自定义格式标记
  enum CustomFormatTag {
    Null
    Bool
    Int
    Float
    String
    Array
    Object
  }
  
  // 将标记转换为字节
  let tag_to_byte = fn(tag: CustomFormatTag) {
    match tag {
      CustomFormatTag::Null => 0x00
      CustomFormatTag::Bool => 0x01
      CustomFormatTag::Int => 0x02
      CustomFormatTag::Float => 0x03
      CustomFormatTag::String => 0x04
      CustomFormatTag::Array => 0x05
      CustomFormatTag::Object => 0x06
    }
  }
  
  // 将字节转换为标记
  let byte_to_tag = fn(byte: Int) {
    match byte {
      0x00 => CustomFormatTag::Null
      0x01 => CustomFormatTag::Bool
      0x02 => CustomFormatTag::Int
      0x03 => CustomFormatTag::Float
      0x04 => CustomFormatTag::String
      0x05 => CustomFormatTag::Array
      0x06 => CustomFormatTag::Object
      _ => CustomFormatTag::Null
    }
  }
  
  // 创建自定义序列化器
  let create_custom_serializer = fn() {
    let mut buffer = []
    
    // 写入标记
    let write_tag = fn(tag: CustomFormatTag) {
      buffer = buffer.push(tag_to_byte(tag))
    }
    
    // 写入整数
    let write_int = fn(value: Int) {
      write_tag(CustomFormatTag::Int)
      
      // 写入4字节整数（大端序）
      buffer = buffer.push((value >> 24) & 0xFF)
      buffer = buffer.push((value >> 16) & 0xFF)
      buffer = buffer.push((value >> 8) & 0xFF)
      buffer = buffer.push(value & 0xFF)
    }
    
    // 写入字符串
    let write_string = fn(value: String) {
      write_tag(CustomFormatTag::String)
      
      // 写入长度（4字节）
      let len = value.length()
      buffer = buffer.push((len >> 24) & 0xFF)
      buffer = buffer.push((len >> 16) & 0xFF)
      buffer = buffer.push((len >> 8) & 0xFF)
      buffer = buffer.push(len & 0xFF)
      
      // 写入字符串内容
      for c in value.chars() {
        buffer = buffer.push(c.to_int())
      }
    }
    
    // 写入数组
    let write_array = fn(arr: Array[Int]) {
      write_tag(CustomFormatTag::Array)
      
      // 写入长度（4字节）
      let len = arr.length()
      buffer = buffer.push((len >> 24) & 0xFF)
      buffer = buffer.push((len >> 16) & 0xFF)
      buffer = buffer.push((len >> 8) & 0xFF)
      buffer = buffer.push(len & 0xFF)
      
      // 写入数组元素
      for item in arr {
        write_int(item)
      }
    }
    
    // 写入对象
    let write_object = fn(obj: Array[(String, String)]) {
      write_tag(CustomFormatTag::Object)
      
      // 写入字段数量（4字节）
      let len = obj.length()
      buffer = buffer.push((len >> 24) & 0xFF)
      buffer = buffer.push((len >> 16) & 0xFF)
      buffer = buffer.push((len >> 8) & 0xFF)
      buffer = buffer.push(len & 0xFF)
      
      // 写入字段
      for (key, value) in obj {
        write_string(key)
        write_string(value)
      }
    }
    
    // 获取缓冲区
    let get_buffer = fn() {
      buffer
    }
    
    // 重置缓冲区
    let reset = fn() {
      buffer = []
    }
    
    {
      write_int,
      write_string,
      write_array,
      write_object,
      get_buffer,
      reset
    }
  }
  
  // 创建自定义反序列化器
  let create_custom_deserializer = fn(data: Array[Int]) {
    let mut offset = 0
    
    // 读取标记
    let read_tag = fn() {
      if offset < data.length() {
        let tag = byte_to_tag(data[offset])
        offset = offset + 1
        tag
      } else {
        CustomFormatTag::Null
      }
    }
    
    // 读取整数
    let read_int = fn() {
      let tag = read_tag()
      
      if tag == CustomFormatTag::Int && offset + 3 < data.length() {
        let value = (data[offset] << 24) | (data[offset + 1] << 16) | (data[offset + 2] << 8) | data[offset + 3]
        offset = offset + 4
        value
      } else {
        0
      }
    }
    
    // 读取字符串
    let read_string = fn() {
      let tag = read_tag()
      
      if tag == CustomFormatTag::String && offset + 3 < data.length() {
        let len = (data[offset] << 24) | (data[offset + 1] << 16) | (data[offset + 2] << 8) | data[offset + 3]
        offset = offset + 4
        
        if offset + len <= data.length() {
          let mut result = ""
          
          for i in 0..len {
            result = result + (data[offset + i] as Char).to_string()
          }
          
          offset = offset + len
          result
        } else {
          ""
        }
      } else {
        ""
      }
    }
    
    // 读取数组
    let read_array = fn() {
      let tag = read_tag()
      
      if tag == CustomFormatTag::Array && offset + 3 < data.length() {
        let len = (data[offset] << 24) | (data[offset + 1] << 16) | (data[offset + 2] << 8) | data[offset + 3]
        offset = offset + 4
        
        let mut result = []
        
        for i in 0..len {
          result = result.push(read_int())
        }
        
        result
      } else {
        []
      }
    }
    
    // 读取对象
    let read_object = fn() {
      let tag = read_tag()
      
      if tag == CustomFormatTag::Object && offset + 3 < data.length() {
        let len = (data[offset] << 24) | (data[offset + 1] << 16) | (data[offset + 2] << 8) | data[offset + 3]
        offset = offset + 4
        
        let mut result = []
        
        for i in 0..len {
          let key = read_string()
          let value = read_string()
          result = result.push((key, value))
        }
        
        result
      } else {
        []
      }
    }
    
    {
      read_int,
      read_string,
      read_array,
      read_object
    }
  }
  
  // 测试自定义序列化
  let serializer = create_custom_serializer()
  
  // 序列化各种类型的数据
  serializer.reset()
  serializer.write_int(12345678)
  serializer.write_string("test string")
  serializer.write_array([1, 2, 3, 4, 5])
  serializer.write_object([
    ("key1", "value1"),
    ("key2", "value2"),
    ("key3", "value3")
  ])
  
  let buffer = serializer.get_buffer()
  
  // 验证序列化结果
  assert_true(buffer.length() > 0)
  
  // 反序列化
  let deserializer = create_custom_deserializer(buffer)
  
  let int_value = deserializer.read_int()
  let string_value = deserializer.read_string()
  let array_value = deserializer.read_array()
  let object_value = deserializer.read_object()
  
  // 验证反序列化结果
  assert_eq(int_value, 12345678)
  assert_eq(string_value, "test string")
  assert_eq(array_value, [1, 2, 3, 4, 5])
  assert_eq(object_value.length(), 3)
  assert_eq(object_value[0], ("key1", "value1"))
  assert_eq(object_value[1], ("key2", "value2"))
  assert_eq(object_value[2], ("key3", "value3"))
}

// 测试 5: 序列化性能测试
test "serialization performance test" {
  // 创建大量测试数据
  let create_large_dataset = fn(size: Int) {
    let mut dataset = []
    
    for i in 0..size {
      let item = {
        id: i,
        name: "item_" + i.to_string(),
        timestamp: 1634567890123 + i,
        value: (i * 12345) % 1000000,
        tags: ["tag1", "tag2", "tag3"]
      }
      dataset = dataset.push(item)
    }
    
    dataset
  }
  
  // 简单的字符串序列化
  let simple_serialize = fn(dataset: Array[{id: Int, name: String, timestamp: Int, value: Int, tags: Array[String>}]) {
    let mut result = ""
    
    for item in dataset {
      result = result + item.id.to_string() + "|" + item.name + "|" + item.timestamp.to_string() + "|" + item.value.to_string() + "|"
      
      for i in 0..item.tags.length() {
        result = result + item.tags[i]
        if i < item.tags.length() - 1 {
          result = result + ","
        }
      }
      
      result = result + "\n"
    }
    
    result
  }
  
  // 简单的字符串反序列化
  let simple_deserialize = fn(data: String) {
    let lines = data.split("\n")
    let mut result = []
    
    for line in lines {
      if line.length() > 0 {
        let parts = line.split("|")
        if parts.length() >= 4 {
          let id = parts[0].to_int()
          let name = parts[1]
          let timestamp = parts[2].to_int()
          let value = parts[3].to_int()
          
          let tags_str = if parts.length() > 4 { parts[4] } else { "" }
          let tags = tags_str.split(",")
          
          let item = {
            id,
            name,
            timestamp,
            value,
            tags
          }
          
          result = result.push(item)
        }
      }
    }
    
    result
  }
  
  // 测试序列化性能
  let large_dataset = create_large_dataset(1000)
  
  // 序列化
  let serialized_data = simple_serialize(large_dataset)
  
  // 验证序列化结果
  assert_true(serialized_data.length() > 0)
  assert_true(serialized_data.contains("item_0"))
  assert_true(serialized_data.contains("item_999"))
  
  // 反序列化
  let deserialized_data = simple_deserialize(serialized_data)
  
  // 验证反序列化结果
  assert_eq(deserialized_data.length(), large_dataset.length())
  
  // 验证数据完整性
  for i in 0..large_dataset.length() {
    assert_eq(deserialized_data[i].id, large_dataset[i].id)
    assert_eq(deserialized_data[i].name, large_dataset[i].name)
    assert_eq(deserialized_data[i].timestamp, large_dataset[i].timestamp)
    assert_eq(deserialized_data[i].value, large_dataset[i].value)
    assert_eq(deserialized_data[i].tags.length(), large_dataset[i].tags.length())
  }
  
  // 测试序列化大小
  let original_size = large_dataset.length() * 100  // 估算原始大小
  let serialized_size = serialized_data.length()
  
  // 验证序列化大小合理
  assert_true(serialized_size > 0)
  assert_true(serialized_size < original_size * 2)  // 不应该过于膨胀
}