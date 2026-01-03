// Azimuth 序列化反序列化测试用例
// 专注于测试各种数据格式的序列化和反序列化功能

// 测试1: JSON序列化反序列化
test "JSON序列化反序列化测试" {
  // 创建复杂的数据结构
  let telemetry_data = {
    trace_id: "trace-123456789",
    span_id: "span-001",
    parent_span_id: Some("parent-000"),
    operation_name: "http.request",
    start_time: 1640995200000,
    end_time: 1640995200100,
    duration_ms: 100,
    status: "ok",
    tags: {
      "http.method": "GET",
      "http.url": "/api/users/123",
      "http.status_code": "200",
      "service.name": "user-service",
      "service.version": "1.2.3"
    },
    logs: [
      {
        timestamp: 1640995200050,
        level: "info",
        message: "Processing user request",
        fields: {
          "user.id": "123",
          "request.id": "req-abc123"
        }
      },
      {
        timestamp: 1640995200080,
        level: "debug",
        message: "Database query executed",
        fields: {
          "query.time": "25ms",
          "db.connection": "conn-456"
        }
      }
    ],
    metrics: {
      "cpu.usage": 45.2,
      "memory.usage": 67.8,
      "request.count": 1234
    }
  }
  
  // 序列化为JSON
  let json_serializer = JsonSerializer::new()
  let serialized_result = json_serializer.serialize(telemetry_data)
  
  match serialized_result {
    Ok(json_string) => {
      // 验证JSON字符串不为空
      assert_true(json_string.length() > 0)
      
      // 验证JSON包含必要的字段
      assert_true(json_string.contains("\"trace_id\":\"trace-123456789\""))
      assert_true(json_string.contains("\"span_id\":\"span-001\""))
      assert_true(json_string.contains("\"operation_name\":\"http.request\""))
      assert_true(json_string.contains("\"status\":\"ok\""))
      assert_true(json_string.contains("\"http.method\":\"GET\""))
      
      // 反序列化
      let deserialized_result = json_serializer.deserialize(json_string)
      
      match deserialized_result {
        Ok(deserialized_data) => {
          // 验证反序列化数据完整性
          assert_eq(deserialized_data.trace_id, telemetry_data.trace_id)
          assert_eq(deserialized_data.span_id, telemetry_data.span_id)
          assert_eq(deserialized_data.parent_span_id, telemetry_data.parent_span_id)
          assert_eq(deserialized_data.operation_name, telemetry_data.operation_name)
          assert_eq(deserialized_data.start_time, telemetry_data.start_time)
          assert_eq(deserialized_data.end_time, telemetry_data.end_time)
          assert_eq(deserialized_data.status, telemetry_data.status)
          
          // 验证tags
          assert_eq(deserialized_data.tags.get("http.method"), telemetry_data.tags.get("http.method"))
          assert_eq(deserialized_data.tags.get("http.url"), telemetry_data.tags.get("http.url"))
          
          // 验证logs
          assert_eq(deserialized_data.logs.length(), telemetry_data.logs.length())
          assert_eq(deserialized_data.logs[0].level, telemetry_data.logs[0].level)
          assert_eq(deserialized_data.logs[0].message, telemetry_data.logs[0].message)
          
          // 验证metrics
          assert_eq(deserialized_data.metrics.get("cpu.usage"), telemetry_data.metrics.get("cpu.usage"))
          assert_eq(deserialized_data.metrics.get("memory.usage"), telemetry_data.metrics.get("memory.usage"))
        }
        Err(error) => {
          assert_true(false, "反序列化失败: " + error.to_string())
        }
      }
    }
    Err(error) => {
      assert_true(false, "序列化失败: " + error.to_string())
    }
  }
  
  // 测试无效JSON处理
  let invalid_json = "{ invalid json format }"
  let invalid_result = json_serializer.deserialize(invalid_json)
  
  match invalid_result {
    Ok(_) => {
      assert_true(false, "应该解析无效JSON失败")
    }
    Err(error) => {
      match error {
        SerializationError::InvalidJson => {
          assert_true(true, "正确捕获无效JSON错误")
        }
        _ => {
          assert_true(false, "错误类型不匹配")
        }
      }
    }
  }
}

// 测试2: 二进制序列化反序列化
test "二进制序列化反序列化测试" {
  // 创建包含各种数据类型的结构
  let binary_data = {
    id: 12345,
    name: "test_binary_data",
    timestamp: 1640995200000,
    active: true,
    score: 95.7,
    tags: ["tag1", "tag2", "tag3"],
    metadata: {
      version: 1,
      flags: 0b10101010,
      checksum: 3847562938475629384
    },
    payload: [0x01, 0x02, 0x03, 0x04, 0x05]
  }
  
  // 序列化为二进制
  let binary_serializer = BinarySerializer::new({
    format: "protobuf",
    compression: true,
    endian: "little"
  })
  
  let serialized_result = binary_serializer.serialize(binary_data)
  
  match serialized_result {
    Ok(binary_bytes) => {
      // 验证二进制数据不为空
      assert_true(binary_bytes.length() > 0)
      
      // 验证压缩效果（如果启用）
      if binary_serializer.is_compression_enabled() {
        let uncompressed_size = binary_serializer.calculate_uncompressed_size(binary_data)
        assert_true(binary_bytes.length() < uncompressed_size)
      }
      
      // 反序列化
      let deserialized_result = binary_serializer.deserialize(binary_bytes)
      
      match deserialized_result {
        Ok(deserialized_data) => {
          // 验证反序列化数据完整性
          assert_eq(deserialized_data.id, binary_data.id)
          assert_eq(deserialized_data.name, binary_data.name)
          assert_eq(deserialized_data.timestamp, binary_data.timestamp)
          assert_eq(deserialized_data.active, binary_data.active)
          assert_eq(deserialized_data.score, binary_data.score)
          
          // 验证数组
          assert_eq(deserialized_data.tags.length(), binary_data.tags.length())
          for i = 0; i < binary_data.tags.length(); i = i + 1 {
            assert_eq(deserialized_data.tags[i], binary_data.tags[i])
          }
          
          // 验证嵌套对象
          assert_eq(deserialized_data.metadata.version, binary_data.metadata.version)
          assert_eq(deserialized_data.metadata.flags, binary_data.metadata.flags)
          assert_eq(deserialized_data.metadata.checksum, binary_data.metadata.checksum)
          
          // 验证二进制payload
          assert_eq(deserialized_data.payload.length(), binary_data.payload.length())
          for i = 0; i < binary_data.payload.length(); i = i + 1 {
            assert_eq(deserialized_data.payload[i], binary_data.payload[i])
          }
        }
        Err(error) => {
          assert_true(false, "反序列化失败: " + error.to_string())
        }
      }
    }
    Err(error) => {
      assert_true(false, "序列化失败: " + error.to_string())
    }
  }
  
  // 测试版本兼容性
  let v1_data = {
    id: 12345,
    name: "v1_data",
    timestamp: 1640995200000,
    active: true
    // v2新增的字段在v1中不存在
  }
  
  let v1_serialized = binary_serializer.serialize(v1_data).unwrap()
  
  // 使用v2格式反序列化v1数据
  let v2_result = binary_serializer.deserialize_with_version(v1_serialized, "2.0")
  
  match v2_result {
    Ok(v2_data) => {
      // 验证v1字段正确保留
      assert_eq(v2_data.id, v1_data.id)
      assert_eq(v2_data.name, v1_data.name)
      assert_eq(v2_data.timestamp, v1_data.timestamp)
      assert_eq(v2_data.active, v1_data.active)
      
      // v2新增字段应该有默认值
      assert_eq(v2_data.score, 0.0)  // 默认值
      assert_eq(v2_data.tags.length(), 0)  // 默认空数组
    }
    Err(error) => {
      assert_true(false, "版本兼容性测试失败: " + error.to_string())
    }
  }
}

// 测试3: 自定义序列化格式
test "自定义序列化格式测试" {
  // 创建自定义序列化器
  let custom_serializer = CustomFormatSerializer::new({
    format_name: "telemetry_v1",
    field_separator: "|",
    value_separator: "=",
    array_separator: ",",
    escape_character: "\\"
  })
  
  // 创建测试数据
  let custom_data = {
    trace_id: "trace-custom-001",
    span_id: "span-custom-001",
    operation: "custom.operation",
    duration: 250,
    success: true,
    tags: {
      "service": "custom-service",
      "version": "1.0.0",
      "env": "test"
    },
    events: ["start", "process", "end"]
  }
  
  // 序列化为自定义格式
  let serialized_result = custom_serializer.serialize(custom_data)
  
  match serialized_result {
    Ok(custom_string) => {
      // 验证自定义格式字符串
      assert_true(custom_string.length() > 0)
      assert_true(custom_string.contains("trace_id=trace-custom-001"))
      assert_true(custom_string.contains("span_id=span-custom-001"))
      assert_true(custom_string.contains("operation=custom.operation"))
      assert_true(custom_string.contains("duration=250"))
      assert_true(custom_string.contains("success=true"))
      assert_true(custom_string.contains("service=custom-service"))
      assert_true(custom_string.contains("events=start,process,end"))
      
      // 反序列化
      let deserialized_result = custom_serializer.deserialize(custom_string)
      
      match deserialized_result {
        Ok(deserialized_data) => {
          // 验证反序列化数据
          assert_eq(deserialized_data.trace_id, custom_data.trace_id)
          assert_eq(deserialized_data.span_id, custom_data.span_id)
          assert_eq(deserialized_data.operation, custom_data.operation)
          assert_eq(deserialized_data.duration, custom_data.duration)
          assert_eq(deserialized_data.success, custom_data.success)
          
          // 验证tags
          assert_eq(deserialized_data.tags.get("service"), custom_data.tags.get("service"))
          assert_eq(deserialized_data.tags.get("version"), custom_data.tags.get("version"))
          assert_eq(deserialized_data.tags.get("env"), custom_data.tags.get("env"))
          
          // 验证events数组
          assert_eq(deserialized_data.events.length(), custom_data.events.length())
          for i = 0; i < custom_data.events.length(); i = i + 1 {
            assert_eq(deserialized_data.events[i], custom_data.events[i])
          }
        }
        Err(error) => {
          assert_true(false, "反序列化失败: " + error.to_string())
        }
      }
    }
    Err(error) => {
      assert_true(false, "序列化失败: " + error.to_string())
    }
  }
  
  // 测试特殊字符处理
  let special_chars_data = {
    message: "This|contains=special,characters\\that\\need=escaping",
    tags: {
      "key=with=special": "value|with|special",
      "another,key": "another,value"
    }
  }
  
  let special_result = custom_serializer.serialize(special_chars_data)
  
  match special_result {
    Ok(special_string) => {
      // 验证特殊字符被正确转义
      assert_true(special_string.contains("\\|"))
      assert_true(special_string.contains("\\="))
      assert_true(special_string.contains("\\,"))
      assert_true(special_string.contains("\\\\"))
      
      // 反序列化并验证
      let deserialized_special = custom_serializer.deserialize(special_string).unwrap()
      assert_eq(deserialized_special.message, special_chars_data.message)
      assert_eq(deserialized_special.tags.get("key=with=special"), 
                special_chars_data.tags.get("key=with=special"))
    }
    Err(_) => {
      assert_true(false, "特殊字符处理失败")
    }
  }
}

// 测试4: 序列化性能和大小优化
test "序列化性能和大小优化测试" {
  // 创建大型数据集
  let large_dataset = []
  
  for i = 0; i < 1000; i = i + 1 {
    large_dataset.push({
      id: i,
      name: "item_" + i.to_string(),
      description: "This is a detailed description for item " + i.to_string() + 
                   " with additional text to increase size",
      timestamp: 1640995200000 + i * 1000,
      metadata: {
        category: ["category_" + (i % 10).to_string()],
        tags: ["tag1", "tag2", "tag3", "tag4", "tag5"],
        properties: {
          "prop1": "value1_" + i.to_string(),
          "prop2": "value2_" + i.to_string(),
          "prop3": i * 2,
          "prop4": i * 3.14
        }
      },
      measurements: [i * 0.1, i * 0.2, i * 0.3, i * 0.4, i * 0.5]
    })
  }
  
  // 测试不同序列化格式的性能和大小
  let formats = ["json", "binary", "custom"]
  let results = []
  
  for format in formats {
    let serializer = SerializerFactory::create(format)
    
    // 测量序列化时间
    let start_time = timestamp()
    let serialized_result = serializer.serialize(large_dataset)
    let serialization_time = timestamp() - start_time
    
    match serialized_result {
      Ok(serialized_data) => {
        // 测量数据大小
        let data_size = serialized_data.length()
        
        // 测量反序列化时间
        let start_time = timestamp()
        let deserialized_result = serializer.deserialize(serialized_data)
        let deserialization_time = timestamp() - start_time
        
        match deserialized_result {
          Ok(_) => {
            results.push({
              format: format,
              serialization_time: serialization_time,
              deserialization_time: deserialization_time,
              data_size: data_size,
              total_time: serialization_time + deserialization_time
            })
          }
          Err(_) => {
            assert_true(false, format + "格式反序列化失败")
          }
        }
      }
      Err(_) => {
        assert_true(false, format + "格式序列化失败")
      }
    }
  }
  
  // 验证所有格式都成功处理
  assert_eq(results.length(), formats.length())
  
  // 查找最快的序列化格式
  let fastest_serialization = results.reduce(|min, current| 
    if current.serialization_time < min.serialization_time { current } else { min }
  )
  
  // 查找最小的数据格式
  let smallest_size = results.reduce(|min, current| 
    if current.data_size < min.data_size { current } else { min }
  )
  
  // 验证性能在合理范围内
  for result in results {
    // 序列化时间不应超过5秒
    assert_true(result.serialization_time < 5000, 
                result.format + "序列化时间过长: " + result.serialization_time.to_string() + "ms")
    
    // 反序列化时间不应超过5秒
    assert_true(result.deserialization_time < 5000, 
                result.format + "反序列化时间过长: " + result.deserialization_time.to_string() + "ms")
    
    // 数据大小不应超过原始数据的10倍（估算）
    let estimated_original_size = 1000 * 500  // 估算每个项目500字节
    assert_true(result.data_size < estimated_original_size * 10, 
                result.format + "序列化后数据过大")
  }
  
  // 验证二进制格式通常更紧凑
  let binary_result = results.find(|r| r.format == "binary")
  let json_result = results.find(|r| r.format == "json")
  
  match (binary_result, json_result) {
    (Some(binary), Some(json)) => {
      // 二进制格式通常应该比JSON更紧凑
      assert_true(binary.data_size <= json.data_size * 1.2, 
                  "二进制格式应该比JSON更紧凑或相近")
    }
    _ => {}
  }
}

// 测试5: 序列化安全性和验证
test "序列化安全性和验证测试" {
  // 创建安全的序列化器
  let secure_serializer = SecureSerializer::new({
    max_data_size: 10 * 1024 * 1024,  // 10MB限制
    max_depth: 100,  // 最大嵌套深度
    allowed_types: ["telemetry", "metrics", "logs"],
    enable_signature: true,
    encryption_key: "test_encryption_key_12345"
  })
  
  // 创建正常数据
  let normal_data = {
    type: "telemetry",
    trace_id: "trace-secure-001",
    data: {
      operation: "test.operation",
      duration: 100,
      success: true
    }
  }
  
  // 序列化正常数据
  let secure_result = secure_serializer.serialize(normal_data)
  
  match secure_result {
    Ok(secure_data) => {
      // 验证签名存在
      assert_true(secure_serializer.has_signature(secure_data))
      
      // 验证签名
      assert_true(secure_serializer.verify_signature(secure_data))
      
      // 反序列化
      let deserialize_result = secure_serializer.deserialize(secure_data)
      
      match deserialize_result {
        Ok(deserialized_data) => {
          assert_eq(deserialized_data.type, normal_data.type)
          assert_eq(deserialized_data.trace_id, normal_data.trace_id)
        }
        Err(_) => {
          assert_true(false, "安全反序列化失败")
        }
      }
      
      // 测试篡改检测
      let tampered_data = secure_data.substring(0, secure_data.length() - 1) + "X"
      let tampered_result = secure_serializer.deserialize(tampered_data)
      
      match tampered_result {
        Ok(_) => {
          assert_true(false, "应该检测到数据篡改")
        }
        Err(error) => {
          match error {
            SerializationError::InvalidSignature => {
              assert_true(true, "正确检测到无效签名")
            }
            _ => {
              assert_true(false, "错误类型不匹配")
            }
          }
        }
      }
    }
    Err(_) => {
      assert_true(false, "安全序列化失败")
    }
  }
  
  // 测试大小限制
  let oversized_data = {
    type: "telemetry",
    data: "x".repeat(20 * 1024 * 1024)  // 20MB数据，超过10MB限制
  }
  
  let oversized_result = secure_serializer.serialize(oversized_data)
  
  match oversized_result {
    Ok(_) => {
      assert_true(false, "应该拒绝过大数据")
    }
    Err(error) => {
      match error {
        SerializationError::DataTooLarge => {
          assert_true(true, "正确拒绝过大数据")
        }
        _ => {
          assert_true(false, "错误类型不匹配")
        }
      }
    }
  }
  
  // 测试深度限制
  let deep_data = {
    type: "telemetry",
    level: 0
  }
  
  // 创建深度嵌套结构
  let mut current = deep_data
  for i = 1; i <= 150; i = i + 1 {  // 超过100层限制
    current = {
      type: "telemetry",
      level: i,
      nested: current
    }
  }
  
  let deep_result = secure_serializer.serialize(current)
  
  match deep_result {
    Ok(_) => {
      assert_true(false, "应该拒绝过深嵌套")
    }
    Err(error) => {
      match error {
        SerializationError::NestingTooDeep => {
          assert_true(true, "正确拒绝过深嵌套")
        }
        _ => {
          assert_true(false, "错误类型不匹配")
        }
      }
    }
  }
  
  // 测试类型限制
  let unauthorized_type = {
    type: "malicious",  // 不在允许列表中
    data: "malicious payload"
  }
  
  let unauthorized_result = secure_serializer.serialize(unauthorized_type)
  
  match unauthorized_result {
    Ok(_) => {
      assert_true(false, "应该拒绝未授权类型")
    }
    Err(error) => {
      match error {
        SerializationError::UnauthorizedType => {
          assert_true(true, "正确拒绝未授权类型")
        }
        _ => {
          assert_true(false, "错误类型不匹配")
        }
      }
    }
  }
}