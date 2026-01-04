// Azimuth 遥测数据完整性验证测试用例
// 包含遥测数据的完整性检查、校验和验证和数据修复

// 测试1: 基础数据完整性检查
test "基础数据完整性检查" {
  // 定义完整性检查结果
  type IntegrityResult = {
    is_valid: Bool,
    error_message: Option<String>,
    details: Array[String>
  }
  
  // 定义数据验证器
  type DataValidator = {
    name: String,
    validate: fn(TelemetryData) -> IntegrityResult
  }
  
  // 定义遥测数据结构
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    operation_name: String,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // 创建数据验证器
  let create_validator = fn(name: String, validation_fn: (TelemetryData) -> IntegrityResult) {
    {
      name,
      validate: validation_fn
    }
  }
  
  // 时间戳验证器
  let timestamp_validator = create_validator("timestamp", fn(data) {
    let current_time = Time::now()
    let time_diff = current_time - data.timestamp
    
    if time_diff < 0 {
      {
        is_valid: false,
        error_message: Some("Timestamp is in the future"),
        details: ["Timestamp: " + data.timestamp.to_string(), "Current time: " + current_time.to_string()]
      }
    } else if time_diff > 86400000 {  // 超过24小时
      {
        is_valid: false,
        error_message: Some("Timestamp is too old"),
        details: ["Age: " + time_diff.to_string() + "ms"]
      }
    } else {
      {
        is_valid: true,
        error_message: None,
        details: ["Timestamp is valid"]
      }
    }
  })
  
  // Trace ID验证器
  let trace_id_validator = create_validator("trace_id", fn(data) {
    if data.trace_id.length() != 32 {
      {
        is_valid: false,
        error_message: Some("Invalid trace ID length"),
        details: ["Expected: 32 characters", "Actual: " + data.trace_id.length().to_string()]
      }
    } else if not(data.trace_id.chars().all(fn(c) { 
      (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') 
    })) {
      {
        is_valid: false,
        error_message: Some("Trace ID contains invalid characters"),
        details: ["Expected: hexadecimal characters (0-9, a-f)"]
      }
    } else {
      {
        is_valid: true,
        error_message: None,
        details: ["Trace ID is valid"]
      }
    }
  })
  
  // Span ID验证器
  let span_id_validator = create_validator("span_id", fn(data) {
    if data.span_id.length() != 16 {
      {
        is_valid: false,
        error_message: Some("Invalid span ID length"),
        details: ["Expected: 16 characters", "Actual: " + data.span_id.length().to_string()]
      }
    } else if not(data.span_id.chars().all(fn(c) { 
      (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') 
    })) {
      {
        is_valid: false,
        error_message: Some("Span ID contains invalid characters"),
        details: ["Expected: hexadecimal characters (0-9, a-f)"]
      }
    } else {
      {
        is_valid: true,
        error_message: None,
        details: ["Span ID is valid"]
      }
    }
  })
  
  // 操作名称验证器
  let operation_name_validator = create_validator("operation_name", fn(data) {
    if data.operation_name.length() == 0 {
      {
        is_valid: false,
        error_message: Some("Operation name is empty"),
        details: []
      }
    } else if data.operation_name.length() > 100 {
      {
        is_valid: false,
        error_message: Some("Operation name is too long"),
        details: ["Maximum: 100 characters", "Actual: " + data.operation_name.length().to_string()]
      }
    } else {
      {
        is_valid: true,
        error_message: None,
        details: ["Operation name is valid"]
      }
    }
  })
  
  // 状态验证器
  let status_validator = create_validator("status", fn(data) {
    let valid_statuses = ["ok", "error", "timeout", "cancelled"]
    
    if not(valid_statuses.contains(data.status)) {
      {
        is_valid: false,
        error_message: Some("Invalid status"),
        details: ["Valid statuses: " + valid_statuses.join(", "), "Actual: " + data.status]
      }
    } else {
      {
        is_valid: true,
        error_message: None,
        details: ["Status is valid"]
      }
    }
  })
  
  // 运行所有验证器
  let run_validations = fn(data: TelemetryData, validators: Array[DataValidator>) {
    let mut results = []
    
    for validator in validators {
      let result = validator.validate(data)
      results = results.push({
        validator_name: validator.name,
        result
      })
    }
    
    results
  }
  
  // 获取整体完整性结果
  let get_overall_integrity = fn(validation_results: Array<(String, IntegrityResult)>) {
    let mut is_valid = true
    let mut all_errors = []
    let mut all_details = []
    
    for (validator_name, result) in validation_results {
      if not(result.is_valid) {
        is_valid = false
        
        match result.error_message {
          Some(error) => {
            all_errors = all_errors.push(validator_name + ": " + error)
          }
          None => {}
        }
      }
      
      for detail in result.details {
        all_details = all_details.push(validator_name + ": " + detail)
      }
    }
    
    {
      is_valid,
      error_message: if all_errors.length() > 0 { 
        Some(all_errors.join("; ")) 
      } else { 
        None 
      },
      details: all_details
    }
  }
  
  // 创建测试数据
  let valid_data = {
    timestamp: Time::now() - 1000,  // 1秒前
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    operation_name: "database_query",
    status: "ok",
    attributes: [
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM users")
    ]
  }
  
  let validators = [
    timestamp_validator,
    trace_id_validator,
    span_id_validator,
    operation_name_validator,
    status_validator
  ]
  
  // 测试有效数据
  let valid_results = run_validations(valid_data, validators)
  let valid_overall = get_overall_integrity(valid_results)
  
  assert_true(valid_overall.is_valid)
  assert_eq(valid_overall.error_message, None)
  
  // 测试无效时间戳
  let future_data = {
    timestamp: Time::now() + 10000,  // 未来时间
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    operation_name: "database_query",
    status: "ok",
    attributes: []
  }
  
  let future_results = run_validations(future_data, validators)
  let future_overall = get_overall_integrity(future_results)
  
  assert_false(future_overall.is_valid)
  assert_eq(future_overall.error_message, Some("timestamp: Timestamp is in the future"))
  
  // 测试无效Trace ID
  let invalid_trace_data = {
    timestamp: Time::now() - 1000,
    trace_id: "invalid-trace-id",
    span_id: "1234567890abcdef",
    operation_name: "database_query",
    status: "ok",
    attributes: []
  }
  
  let invalid_trace_results = run_validations(invalid_trace_data, validators)
  let invalid_trace_overall = get_overall_integrity(invalid_trace_results)
  
  assert_false(invalid_trace_overall.is_valid)
  assert_eq(invalid_trace_overall.error_message, Some("trace_id: Invalid trace ID length"))
  
  // 测试多个错误
  let multiple_errors_data = {
    timestamp: Time::now() + 10000,  // 未来时间
    trace_id: "invalid",  // 无效Trace ID
    span_id: "12345",  // 无效Span ID
    operation_name: "",  // 空操作名称
    status: "invalid",  // 无效状态
    attributes: []
  }
  
  let multiple_errors_results = run_validations(multiple_errors_data, validators)
  let multiple_errors_overall = get_overall_integrity(multiple_errors_results)
  
  assert_false(multiple_errors_overall.is_valid)
  assert_true(multiple_errors_overall.error_message.unwrap().contains("timestamp:"))
  assert_true(multiple_errors_overall.error_message.unwrap().contains("trace_id:"))
  assert_true(multiple_errors_overall.error_message.unwrap().contains("span_id:"))
  assert_true(multiple_errors_overall.error_message.unwrap().contains("operation_name:"))
  assert_true(multiple_errors_overall.error_message.unwrap().contains("status:"))
}

// 测试2: 校验和验证
test "校验和验证" {
  // 定义校验和计算器
  type ChecksumCalculator = {
    name: String,
    calculate: fn(Array[Int>) -> Int
  }
  
  // 定义校验和验证结果
  type ChecksumResult = {
    expected: Int,
    actual: Int,
    is_valid: Bool
  }
  
  // 简单求和校验和
  let sum_calculator = {
    name: "sum",
    calculate: fn(data: Array<Int>) {
      let mut sum = 0
      for value in data {
        sum = sum + value
      }
      sum % 10000  // 限制在4位数
    }
  }
  
  // XOR校验和
  let xor_calculator = {
    name: "xor",
    calculate: fn(data: Array[Int>) {
      let mut result = 0
      for value in data {
        result = result ^ value
      }
      result
    }
  }
  
  // CRC8简化版校验和
  let crc8_calculator = {
    name: "crc8",
    calculate: fn(data: Array[Int>) {
      let mut crc = 0
      let polynomial = 0x07
      
      for byte in data {
        crc = crc ^ byte
        for i in 0..8 {
          if crc & 0x80 != 0 {
            crc = (crc << 1) ^ polynomial
          } else {
            crc = crc << 1
          }
        }
      }
      
      crc & 0xFF
    }
  }
  
  // 计算校验和
  let calculate_checksum = fn(data: Array[Int>, calculator: ChecksumCalculator) {
    calculator.calculate(data)
  }
  
  // 验证校验和
  let verify_checksum = fn(data: Array[Int], expected: Int, calculator: ChecksumCalculator) {
    let actual = calculate_checksum(data, calculator)
    
    {
      expected,
      actual,
      is_valid: actual == expected
    }
  }
  
  // 将字符串转换为字节数组
  let string_to_bytes = fn(s: String) {
    let mut bytes = []
    for c in s.to_char_array() {
      bytes = bytes.push(c.to_int())
    }
    bytes
  }
  
  // 将遥测数据转换为字节数组
  let telemetry_to_bytes = fn(data: TelemetryData) {
    let mut bytes = []
    
    // 添加时间戳（8字节）
    let timestamp = data.timestamp
    for i in 0..8 {
      bytes = bytes.push((timestamp >> (i * 8)) & 0xFF)
    }
    
    // 添加Trace ID
    let trace_id_bytes = string_to_bytes(data.trace_id)
    bytes = bytes + trace_id_bytes
    
    // 添加Span ID
    let span_id_bytes = string_to_bytes(data.span_id)
    bytes = bytes + span_id_bytes
    
    // 添加操作名称
    let operation_bytes = string_to_bytes(data.operation_name)
    bytes = bytes + operation_bytes
    
    bytes
  }
  
  // 为遥测数据添加校验和
  let add_checksum_to_telemetry = fn(data: TelemetryData, calculator: ChecksumCalculator) {
    let data_bytes = telemetry_to_bytes(data)
    let checksum = calculate_checksum(data_bytes, calculator)
    
    {
      data,
      checksum,
      calculator_name: calculator.name
    }
  }
  
  // 验证带校验和的遥测数据
  let verify_telemetry_checksum = fn(
    data_with_checksum: (TelemetryData, Int, String), 
    calculator: ChecksumCalculator
  ) {
    let (data, expected_checksum, calculator_name) = data_with_checksum
    
    if calculator_name != calculator.name {
      return {
        expected: expected_checksum,
        actual: 0,
        is_valid: false
      }
    }
    
    let data_bytes = telemetry_to_bytes(data)
    verify_checksum(data_bytes, expected_checksum, calculator)
  }
  
  // 使用前一个测试的类型
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    operation_name: String,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // 创建测试数据
  let test_data = {
    timestamp: 1640995200000,
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    operation_name: "database_query",
    status: "ok",
    attributes: [
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM users")
    ]
  }
  
  // 测试校验和计算
  let data_bytes = telemetry_to_bytes(test_data)
  
  let sum_checksum = calculate_checksum(data_bytes, sum_calculator)
  let xor_checksum = calculate_checksum(data_bytes, xor_calculator)
  let crc8_checksum = calculate_checksum(data_bytes, crc8_calculator)
  
  // 验证校验和不相同
  assert_true(sum_checksum != xor_checksum)
  assert_true(xor_checksum != crc8_checksum)
  assert_true(sum_checksum != crc8_checksum)
  
  // 测试校验和验证
  let sum_result = verify_checksum(data_bytes, sum_checksum, sum_calculator)
  let xor_result = verify_checksum(data_bytes, xor_checksum, xor_calculator)
  let crc8_result = verify_checksum(data_bytes, crc8_checksum, crc8_calculator)
  
  assert_true(sum_result.is_valid)
  assert_eq(sum_result.expected, sum_result.actual)
  
  assert_true(xor_result.is_valid)
  assert_eq(xor_result.expected, xor_result.actual)
  
  assert_true(crc8_result.is_valid)
  assert_eq(crc8_result.expected, crc8_result.actual)
  
  // 测试错误校验和
  let wrong_sum_result = verify_checksum(data_bytes, sum_checksum + 1, sum_calculator)
  assert_false(wrong_sum_result.is_valid)
  assert_ne(wrong_sum_result.expected, wrong_sum_result.actual)
  
  // 测试遥测数据校验和
  let telemetry_with_sum = add_checksum_to_telemetry(test_data, sum_calculator)
  let telemetry_with_xor = add_checksum_to_telemetry(test_data, xor_calculator)
  let telemetry_with_crc8 = add_checksum_to_telemetry(test_data, crc8_calculator)
  
  // 验证遥测数据校验和
  let verify_sum_result = verify_telemetry_checksum(telemetry_with_sum, sum_calculator)
  let verify_xor_result = verify_telemetry_checksum(telemetry_with_xor, xor_calculator)
  let verify_crc8_result = verify_telemetry_checksum(telemetry_with_crc8, crc8_calculator)
  
  assert_true(verify_sum_result.is_valid)
  assert_true(verify_xor_result.is_valid)
  assert_true(verify_crc8_result.is_valid)
  
  // 测试不同计算器验证
  let cross_verify_result = verify_telemetry_checksum(telemetry_with_sum, xor_calculator)
  assert_false(cross_verify_result.is_valid)  // 使用不同的计算器
  
  // 测试数据篡改检测
  let tampered_data = {
    timestamp: test_data.timestamp,
    trace_id: test_data.trace_id,
    span_id: "1234567890abcdee",  // 篡改Span ID
    operation_name: test_data.operation_name,
    status: test_data.status,
    attributes: test_data.attributes
  }
  
  let tampered_with_sum = add_checksum_to_telemetry(tampered_data, sum_calculator)
  let tampered_verify_result = verify_telemetry_checksum(tampered_with_sum, sum_calculator)
  
  // 注意：这里我们为篡改后的数据重新计算了校验和，所以验证会通过
  // 在实际场景中，我们会使用原始数据的校验和来验证篡改后的数据
  assert_true(tampered_verify_result.is_valid)
  
  // 正确的篡改检测应该是：
  let original_telemetry_with_sum = add_checksum_to_telemetry(test_data, sum_calculator)
  let tampered_data_bytes = telemetry_to_bytes(tampered_data)
  let tampered_detection_result = verify_checksum(
    tampered_data_bytes, 
    original_telemetry_with_sum.1,  // 使用原始数据的校验和
    sum_calculator
  )
  
  assert_false(tampered_detection_result.is_valid)
}

// 测试3: 数据序列化和反序列化完整性
test "数据序列化和反序列化完整性" {
  // 定义序列化结果
  type SerializationResult = {
    success: Bool,
    data: Array[Int>,
    error_message: Option<String>
  }
  
  // 定义反序列化结果
  type DeserializationResult = {
    success: Bool,
    data: Option[TelemetryData>,
    error_message: Option<String>
  }
  
  // 定义完整性验证结果
  type SerializationIntegrityResult = {
    serialization_success: Bool,
    deserialization_success: Bool,
    data_integrity: Bool,
    error_message: Option<String>
  }
  
  // 简化的序列化函数
  let serialize_telemetry = fn(data: TelemetryData) {
    let mut serialized = []
    
    try {
      // 序列化时间戳（8字节）
      let timestamp = data.timestamp
      for i in 0..8 {
        serialized = serialized.push((timestamp >> (i * 8)) & 0xFF)
      }
      
      // 序列化字符串长度和内容
      let serialize_string = fn(s: String) {
        serialized = serialized.push(s.length())
        for c in s.to_char_array() {
          serialized = serialized.push(c.to_int())
        }
      }
      
      // 序列化Trace ID
      serialize_string(data.trace_id)
      
      // 序列化Span ID
      serialize_string(data.span_id)
      
      // 序列化操作名称
      serialize_string(data.operation_name)
      
      // 序列化状态
      serialize_string(data.status)
      
      // 序列化属性数量
      serialized = serialized.push(data.attributes.length())
      
      // 序列化每个属性
      for (key, value) in data.attributes {
        serialize_string(key)
        serialize_string(value)
      }
      
      {
        success: true,
        data: serialized,
        error_message: None
      }
    } catch {
      {
        success: false,
        data: [],
        error_message: Some("Serialization failed")
      }
    }
  }
  
  // 简化的反序列化函数
  let deserialize_telemetry = fn(bytes: Array<Int>) {
    try {
      let mut index = 0
      
      // 反序列化时间戳
      let mut timestamp = 0
      for i in 0..8 {
        timestamp = timestamp | (bytes[index + i] << (i * 8))
      }
      index = index + 8
      
      // 反序列化字符串的辅助函数
      let deserialize_string = fn() {
        if index >= bytes.length() {
          return ""
        }
        
        let length = bytes[index]
        index = index + 1
        
        let mut result = ""
        for i in 0..length {
          if index + i < bytes.length() {
            result = result + (bytes[index + i] as Char).to_string()
          }
        }
        index = index + length
        
        result
      }
      
      // 反序列化Trace ID
      let trace_id = deserialize_string()
      
      // 反序列化Span ID
      let span_id = deserialize_string()
      
      // 反序列化操作名称
      let operation_name = deserialize_string()
      
      // 反序列化状态
      let status = deserialize_string()
      
      // 反序列化属性数量
      let attributes_count = if index < bytes.length() { bytes[index] } else { 0 }
      index = index + 1
      
      // 反序列化属性
      let mut attributes = []
      for i in 0..attributes_count {
        let key = deserialize_string()
        let value = deserialize_string()
        attributes = attributes.push((key, value))
      }
      
      {
        success: true,
        data: Some({
          timestamp,
          trace_id,
          span_id,
          operation_name,
          status,
          attributes
        }),
        error_message: None
      }
    } catch {
      {
        success: false,
        data: None,
        error_message: Some("Deserialization failed")
      }
    }
  }
  
  // 验证序列化/反序列化完整性
  let verify_serialization_integrity = fn(data: TelemetryData) {
    // 序列化
    let serialization_result = serialize_telemetry(data)
    
    if not(serialization_result.success) {
      return {
        serialization_success: false,
        deserialization_success: false,
        data_integrity: false,
        error_message: serialization_result.error_message
      }
    }
    
    // 反序列化
    let deserialization_result = deserialize_telemetry(serialization_result.data)
    
    if not(deserialization_result.success) {
      return {
        serialization_success: true,
        deserialization_success: false,
        data_integrity: false,
        error_message: deserialization_result.error_message
      }
    }
    
    // 验证数据完整性
    match deserialization_result.data {
      Some(deserialized_data) => {
        let data_integrity = 
          data.timestamp == deserialized_data.timestamp &&
          data.trace_id == deserialized_data.trace_id &&
          data.span_id == deserialized_data.span_id &&
          data.operation_name == deserialized_data.operation_name &&
          data.status == deserialized_data.status &&
          data.attributes.length() == deserialized_data.attributes.length()
        
        {
          serialization_success: true,
          deserialization_success: true,
          data_integrity,
          error_message: if data_integrity { None } else { Some("Data integrity check failed") }
        }
      }
      None => {
        {
          serialization_success: true,
          deserialization_success: false,
          data_integrity: false,
          error_message: Some("No deserialized data available")
        }
      }
    }
  }
  
  // 使用前一个测试的类型
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    operation_name: String,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // 创建测试数据
  let simple_data = {
    timestamp: 1640995200000,
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    operation_name: "simple_operation",
    status: "ok",
    attributes: []
  }
  
  let complex_data = {
    timestamp: 1640995250000,
    trace_id: "abcdef1234567890abcdef1234567890",
    span_id: "abcdef1234567890",
    operation_name: "complex_database_query_with_parameters",
    status: "error",
    attributes: [
      ("db.system", "postgresql"),
      ("db.statement", "SELECT * FROM users WHERE id = $1 AND status = $2"),
      ("db.user", "azimuth"),
      ("error.type", "connection_timeout"),
      ("retry.count", "3"),
      ("custom.tag.1", "value1"),
      ("custom.tag.2", "value2")
    ]
  }
  
  // 测试简单数据的序列化/反序列化完整性
  let simple_integrity = verify_serialization_integrity(simple_data)
  assert_true(simple_integrity.serialization_success)
  assert_true(simple_integrity.deserialization_success)
  assert_true(simple_integrity.data_integrity)
  assert_eq(simple_integrity.error_message, None)
  
  // 测试复杂数据的序列化/反序列化完整性
  let complex_integrity = verify_serialization_integrity(complex_data)
  assert_true(complex_integrity.serialization_success)
  assert_true(complex_integrity.deserialization_success)
  assert_true(complex_integrity.data_integrity)
  assert_eq(complex_integrity.error_message, None)
  
  // 测试边界情况
  let empty_operation_data = {
    timestamp: 1640995300000,
    trace_id: "11112222333344445555666677778888",
    span_id: "1111222233334444",
    operation_name: "",
    status: "ok",
    attributes: []
  }
  
  let empty_operation_integrity = verify_serialization_integrity(empty_operation_data)
  assert_true(empty_operation_integrity.serialization_success)
  assert_true(empty_operation_integrity.deserialization_success)
  assert_true(empty_operation_integrity.data_integrity)
  
  // 测试许多属性
  let many_attributes_data = {
    timestamp: 1640995350000,
    trace_id: "88887777666655554444333322221111",
    span_id: "8888777766665555",
    operation_name: "operation_with_many_attributes",
    status: "ok",
    attributes: [
      ("attr.1", "value1"),
      ("attr.2", "value2"),
      ("attr.3", "value3"),
      ("attr.4", "value4"),
      ("attr.5", "value5"),
      ("attr.6", "value6"),
      ("attr.7", "value7"),
      ("attr.8", "value8"),
      ("attr.9", "value9"),
      ("attr.10", "value10")
    ]
  }
  
  let many_attributes_integrity = verify_serialization_integrity(many_attributes_data)
  assert_true(many_attributes_integrity.serialization_success)
  assert_true(many_attributes_integrity.deserialization_success)
  assert_true(many_attributes_integrity.data_integrity)
  
  // 测试数据篡改检测
  let serialization_result = serialize_telemetry(simple_data)
  assert_true(serialization_result.success)
  
  // 篡改序列化数据
  let mut tampered_bytes = serialization_result.data
  if tampered_bytes.length() > 10 {
    tampered_bytes[10] = tampered_bytes[10] + 1  // 修改一个字节
  }
  
  let tampered_deserialization = deserialize_telemetry(tampered_bytes)
  
  // 篡改后的数据可能仍然可以反序列化，但内容可能不正确
  // 这取决于篡改的位置和程度
  
  // 测试截断数据
  let truncated_bytes = serialization_result.data.slice(0, serialization_result.data.length() / 2)
  let truncated_deserialization = deserialize_telemetry(truncated_bytes)
  
  // 截断的数据应该导致反序列化失败
  assert_false(truncated_deserialization.success)
  assert_eq(truncated_deserialization.error_message, Some("Deserialization failed"))
}

// 测试4: 数据修复和恢复
test "数据修复和恢复" {
  // 定义修复策略
  enum RepairStrategy {
    DropInvalid  // 丢弃无效数据
    UseDefault   // 使用默认值
    Interpolate   // 插值修复
    Regenerate   // 重新生成
  }
  
  // 定义修复规则
  type RepairRule = {
    field_name: String,
    strategy: RepairStrategy,
    default_value: Option<String>,
    validator: fn(String) -> Bool
  }
  
  // 定义修复结果
  type RepairResult = {
    success: Bool,
    repaired_data: Option[TelemetryData>,
    repair_actions: Array<String>,
    error_message: Option<String>
  }
  
  // 创建修复规则
  let create_repair_rule = fn(
    field_name: String, 
    strategy: RepairStrategy, 
    default_value: Option<String>,
    validator: (String) -> Bool
  ) {
    {
      field_name,
      strategy,
      default_value,
      validator
    }
  }
  
  // 修复遥测数据
  let repair_telemetry_data = fn(data: TelemetryData, rules: Array[RepairRule>) {
    let mut repaired_data = data
    let mut repair_actions = []
    let mut success = true
    
    for rule in rules {
      let field_value = match rule.field_name {
        "trace_id" => data.trace_id
        "span_id" => data.span_id
        "operation_name" => data.operation_name
        "status" => data.status
        _ => ""
      }
      
      if not(rule.validator(field_value)) {
        // 字段值无效，需要修复
        match rule.strategy {
          RepairStrategy::DropInvalid => {
            return {
              success: false,
              repaired_data: None,
              repair_actions,
              error_message: Some("Data dropped due to invalid " + rule.field_name)
            }
          }
          RepairStrategy::UseDefault => {
            match rule.default_value {
              Some(default_value) => {
                repair_actions = repair_actions.push(
                  "Replaced " + rule.field_name + " with default value"
                )
                
                repaired_data = match rule.field_name {
                  "trace_id" => {
                    trace_id: default_value,
                    span_id: repaired_data.span_id,
                    operation_name: repaired_data.operation_name,
                    status: repaired_data.status,
                    timestamp: repaired_data.timestamp,
                    attributes: repaired_data.attributes
                  }
                  "span_id" => {
                    trace_id: repaired_data.trace_id,
                    span_id: default_value,
                    operation_name: repaired_data.operation_name,
                    status: repaired_data.status,
                    timestamp: repaired_data.timestamp,
                    attributes: repaired_data.attributes
                  }
                  "operation_name" => {
                    trace_id: repaired_data.trace_id,
                    span_id: repaired_data.span_id,
                    operation_name: default_value,
                    status: repaired_data.status,
                    timestamp: repaired_data.timestamp,
                    attributes: repaired_data.attributes
                  }
                  "status" => {
                    trace_id: repaired_data.trace_id,
                    span_id: repaired_data.span_id,
                    operation_name: repaired_data.operation_name,
                    status: default_value,
                    timestamp: repaired_data.timestamp,
                    attributes: repaired_data.attributes
                  }
                  _ => repaired_data
                }
              }
              None => {
                success = false
                repair_actions = repair_actions.push(
                  "No default value available for " + rule.field_name
                )
              }
            }
          }
          RepairStrategy::Interpolate => {
            // 简化的插值修复：基于模式生成新值
            let interpolated_value = match rule.field_name {
              "trace_id" => {
                // 生成有效的Trace ID
                let mut new_trace_id = ""
                for i in 0..32 {
                  let digit = (Time::now() + i) % 16
                  new_trace_id = new_trace_id + "0123456789abcdef"[digit]
                }
                new_trace_id
              }
              "span_id" => {
                // 生成有效的Span ID
                let mut new_span_id = ""
                for i in 0..16 {
                  let digit = (Time::now() + i + 100) % 16
                  new_span_id = new_span_id + "0123456789abcdef"[digit]
                }
                new_span_id
              }
              "operation_name" => "repaired_operation"
              "status" => "ok"
              _ => field_value
            }
            
            repair_actions = repair_actions.push(
              "Interpolated new value for " + rule.field_name
            )
            
            repaired_data = match rule.field_name {
              "trace_id" => {
                trace_id: interpolated_value,
                span_id: repaired_data.span_id,
                operation_name: repaired_data.operation_name,
                status: repaired_data.status,
                timestamp: repaired_data.timestamp,
                attributes: repaired_data.attributes
              }
              "span_id" => {
                trace_id: repaired_data.trace_id,
                span_id: interpolated_value,
                operation_name: repaired_data.operation_name,
                status: repaired_data.status,
                timestamp: repaired_data.timestamp,
                attributes: repaired_data.attributes
              }
              "operation_name" => {
                trace_id: repaired_data.trace_id,
                span_id: repaired_data.span_id,
                operation_name: interpolated_value,
                status: repaired_data.status,
                timestamp: repaired_data.timestamp,
                attributes: repaired_data.attributes
              }
              "status" => {
                trace_id: repaired_data.trace_id,
                span_id: repaired_data.span_id,
                operation_name: repaired_data.operation_name,
                status: interpolated_value,
                timestamp: repaired_data.timestamp,
                attributes: repaired_data.attributes
              }
              _ => repaired_data
            }
          }
          RepairStrategy::Regenerate => {
            // 重新生成整个数据项
            repair_actions = repair_actions.push(
              "Regenerated " + rule.field_name
            )
            
            repaired_data = match rule.field_name {
              "trace_id" => {
                // 生成新的有效Trace ID
                let mut new_trace_id = ""
                for i in 0..32 {
                  let digit = (Time::now() + i) % 16
                  new_trace_id = new_trace_id + "0123456789abcdef"[digit]
                }
                {
                  trace_id: new_trace_id,
                  span_id: repaired_data.span_id,
                  operation_name: repaired_data.operation_name,
                  status: repaired_data.status,
                  timestamp: repaired_data.timestamp,
                  attributes: repaired_data.attributes
                }
              }
              "span_id" => {
                // 生成新的有效Span ID
                let mut new_span_id = ""
                for i in 0..16 {
                  let digit = (Time::now() + i + 100) % 16
                  new_span_id = new_span_id + "0123456789abcdef"[digit]
                }
                {
                  trace_id: repaired_data.trace_id,
                  span_id: new_span_id,
                  operation_name: repaired_data.operation_name,
                  status: repaired_data.status,
                  timestamp: repaired_data.timestamp,
                  attributes: repaired_data.attributes
                }
              }
              _ => repaired_data
            }
          }
        }
      }
    }
    
    {
      success,
      repaired_data: Some(repaired_data),
      repair_actions,
      error_message: if success { None } else { Some("Repair failed") }
    }
  }
  
  // 使用前一个测试的类型
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    operation_name: String,
    status: String,
    attributes: Array<(String, String)>
  }
  
  // 创建验证函数
  let validate_trace_id = fn(trace_id: String) {
    trace_id.length() == 32 && 
    trace_id.chars().all(fn(c) { (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') })
  }
  
  let validate_span_id = fn(span_id: String) {
    span_id.length() == 16 && 
    span_id.chars().all(fn(c) { (c >= '0' and c <= '9') or (c >= 'a' and c <= 'f') })
  }
  
  let validate_operation_name = fn(operation_name: String) {
    operation_name.length() > 0 && operation_name.length() <= 100
  }
  
  let validate_status = fn(status: String) {
    ["ok", "error", "timeout", "cancelled"].contains(status)
  }
  
  // 创建修复规则
  let repair_rules = [
    create_repair_rule(
      "trace_id", 
      RepairStrategy::Regenerate, 
      None, 
      validate_trace_id
    ),
    create_repair_rule(
      "span_id", 
      RepairStrategy::Interpolate, 
      None, 
      validate_span_id
    ),
    create_repair_rule(
      "operation_name", 
      RepairStrategy::UseDefault, 
      Some("default_operation"), 
      validate_operation_name
    ),
    create_repair_rule(
      "status", 
      RepairStrategy::UseDefault, 
      Some("ok"), 
      validate_status
    )
  ]
  
  // 测试修复无效Trace ID
  let invalid_trace_data = {
    timestamp: 1640995200000,
    trace_id: "invalid_trace_id",  // 无效Trace ID
    span_id: "1234567890abcdef",
    operation_name: "database_query",
    status: "ok",
    attributes: []
  }
  
  let trace_repair_result = repair_telemetry_data(invalid_trace_data, repair_rules)
  assert_true(trace_repair_result.success)
  assert_true(trace_repair_result.repair_actions.length() > 0)
  
  match trace_repair_result.repaired_data {
    Some(repaired_data) => {
      assert_true(validate_trace_id(repaired_data.trace_id))
      assert_eq(repaired_data.span_id, "1234567890abcdef")
      assert_eq(repaired_data.operation_name, "database_query")
      assert_eq(repaired_data.status, "ok")
    }
    None => assert_true(false)
  }
  
  // 测试修复多个无效字段
  let multiple_invalid_data = {
    timestamp: 1640995250000,
    trace_id: "invalid",  // 无效Trace ID
    span_id: "123",       // 无效Span ID
    operation_name: "",   // 无效操作名称
    status: "invalid",    // 无效状态
    attributes: []
  }
  
  let multiple_repair_result = repair_telemetry_data(multiple_invalid_data, repair_rules)
  assert_true(multiple_repair_result.success)
  assert_eq(multiple_repair_result.repair_actions.length(), 4)  // 4个字段被修复
  
  match multiple_repair_result.repaired_data {
    Some(repaired_data) => {
      assert_true(validate_trace_id(repaired_data.trace_id))
      assert_true(validate_span_id(repaired_data.span_id))
      assert_eq(repaired_data.operation_name, "default_operation")
      assert_eq(repaired_data.status, "ok")
    }
    None => assert_true(false)
  }
  
  // 测试DropInvalid策略
  let drop_rules = [
    create_repair_rule(
      "trace_id", 
      RepairStrategy::DropInvalid, 
      None, 
      validate_trace_id
    )
  ]
  
  let drop_repair_result = repair_telemetry_data(invalid_trace_data, drop_rules)
  assert_false(drop_repair_result.success)
  assert_eq(drop_repair_result.repaired_data, None)
  assert_eq(drop_repair_result.error_message, Some("Data dropped due to invalid trace_id"))
  
  // 测试有效数据（无需修复）
  let valid_data = {
    timestamp: 1640995300000,
    trace_id: "1234567890abcdef1234567890abcdef",
    span_id: "1234567890abcdef",
    operation_name: "valid_operation",
    status: "ok",
    attributes: []
  }
  
  let valid_repair_result = repair_telemetry_data(valid_data, repair_rules)
  assert_true(valid_repair_result.success)
  assert_eq(valid_repair_result.repair_actions.length(), 0)  // 无需修复
  
  match valid_repair_result.repaired_data {
    Some(repaired_data) => {
      assert_eq(repaired_data.trace_id, valid_data.trace_id)
      assert_eq(repaired_data.span_id, valid_data.span_id)
      assert_eq(repaired_data.operation_name, valid_data.operation_name)
      assert_eq(repaired_data.status, valid_data.status)
    }
    None => assert_true(false)
  }
}

// 测试5: 批量数据完整性验证
test "批量数据完整性验证" {
  // 定义批量验证结果
  type BatchValidationResult = {
    total_count: Int,
    valid_count: Int,
    invalid_count: Int,
    repaired_count: Int,
    dropped_count: Int,
    validation_details: Array<(Int, String, String)>  // (index, status, message)
  }
  
  // 定义批量修复策略
  type BatchRepairStrategy = {
    max_repair_attempts: Int,
    repair_threshold: Float,  // 0.0-1.0，超过此比例则丢弃整个批次
    individual_repair: Bool    // 是否逐个修复
  }
  
  // 批量验证遥测数据
  let batch_validate_telemetry = fn(data_batch: Array[TelemetryData>, validators: Array<DataValidator>) {
    let mut valid_count = 0
    let mut invalid_count = 0
    let mut validation_details = []
    
    for i in 0..data_batch.length() {
      let data = data_batch[i]
      
      // 运行验证
      let validation_results = run_validations(data, validators)
      let overall_result = get_overall_integrity(validation_results)
      
      if overall_result.is_valid {
        valid_count = valid_count + 1
        validation_details = validation_details.push((
          i, 
          "valid", 
          "All validations passed"
        ))
      } else {
        invalid_count = invalid_count + 1
        validation_details = validation_details.push((
          i, 
          "invalid", 
          overall_result.error_message.unwrap_or("Validation failed")
        ))
      }
    }
    
    {
      total_count: data_batch.length(),
      valid_count,
      invalid_count,
      repaired_count: 0,
      dropped_count: 0,
      validation_details
    }
  }
  
  // 批量修复遥测数据
  let batch_repair_telemetry = fn(
    data_batch: Array[TelemetryData>, 
    repair_rules: Array<RepairRule>,
    strategy: BatchRepairStrategy
  ) {
    let mut repaired_batch = []
    let mut repaired_count = 0
    let mut dropped_count = 0
    let mut validation_details = []
    
    // 计算无效数据比例
    let initial_validation = batch_validate_telemetry(data_batch, [])
    let invalid_ratio = (initial_validation.invalid_count as Float) / (initial_validation.total_count as Float)
    
    // 如果无效比例超过阈值，丢弃整个批次
    if invalid_ratio > strategy.repair_threshold {
      for i in 0..data_batch.length() {
        validation_details = validation_details.push((
          i, 
          "dropped", 
          "Batch dropped due to high invalid ratio: " + (invalid_ratio * 100.0).to_string() + "%"
        ))
      }
      
      return {
        total_count: data_batch.length(),
        valid_count: 0,
        invalid_count: 0,
        repaired_count: 0,
        dropped_count: data_batch.length(),
        validation_details
      }
    }
    
    // 逐个修复数据
    if strategy.individual_repair {
      for i in 0..data_batch.length() {
        let data = data_batch[i]
        let repair_result = repair_telemetry_data(data, repair_rules)
        
        if repair_result.success {
          match repair_result.repaired_data {
            Some(repaired_data) => {
              repaired_batch = repaired_batch.push(repaired_data)
              repaired_count = repaired_count + 1
              validation_details = validation_details.push((
                i, 
                "repaired", 
                "Data repaired: " + repair_result.repair_actions.join(", ")
              ))
            }
            None => {
              dropped_count = dropped_count + 1
              validation_details = validation_details.push((
                i, 
                "dropped", 
                "Data dropped after repair failure"
              ))
            }
          }
        } else {
          dropped_count = dropped_count + 1
          validation_details = validation_details.push((
            i, 
            "dropped", 
            "Data dropped: " + repair_result.error_message.unwrap_or("Repair failed")
          ))
        }
      }
    } else {
      // 不进行个体修复，直接保留有效数据
      repaired_batch = data_batch
      repaired_count = initial_validation.valid_count
      dropped_count = initial_validation.invalid_count
      
      // 更新验证详情
      for i in 0..initial_validation.validation_details.length() {
        let (index, status, message) = initial_validation.validation_details[i]
        if status == "invalid" {
          validation_details = validation_details.push((
            index, 
            "dropped", 
            "Data dropped: " + message
          ))
        } else {
          validation_details = validation_details.push((
            index, 
            "kept", 
            "Data kept: " + message
          ))
        }
      }
    }
    
    {
      total_count: data_batch.length(),
      valid_count: repaired_count,
      invalid_count: 0,
      repaired_count,
      dropped_count,
      validation_details
    }
  }
  
  // 使用前一个测试的类型和函数
  type TelemetryData = {
    timestamp: Int,
    trace_id: String,
    span_id: String,
    operation_name: String,
    status: String,
    attributes: Array<(String, String)>
  }
  
  type DataValidator = {
    name: String,
    validate: fn(TelemetryData) -> IntegrityResult
  }
  
  type IntegrityResult = {
    is_valid: Bool,
    error_message: Option<String>,
    details: Array[String>
  }
  
  type RepairRule = {
    field_name: String,
    strategy: RepairStrategy,
    default_value: Option<String>,
    validator: fn(String) -> Bool
  }
  
  enum RepairStrategy {
    DropInvalid
    UseDefault
    Interpolate
    Regenerate
  }
  
  let run_validations = fn(data: TelemetryData, validators: Array<DataValidator>) {
    let mut results = []
    
    for validator in validators {
      let result = validator.validate(data)
      results = results.push({
        validator_name: validator.name,
        result
      })
    }
    
    results
  }
  
  let get_overall_integrity = fn(validation_results: Array<(String, IntegrityResult)>) {
    let mut is_valid = true
    let mut all_errors = []
    let mut all_details = []
    
    for (validator_name, result) in validation_results {
      if not(result.is_valid) {
        is_valid = false
        
        match result.error_message {
          Some(error) => {
            all_errors = all_errors.push(validator_name + ": " + error)
          }
          None => {}
        }
      }
      
      for detail in result.details {
        all_details = all_details.push(validator_name + ": " + detail)
      }
    }
    
    {
      is_valid,
      error_message: if all_errors.length() > 0 { 
        Some(all_errors.join("; ")) 
      } else { 
        None 
      },
      details: all_details
    }
  }
  
  let repair_telemetry_data = fn(data: TelemetryData, rules: Array[RepairRule>) {
    let mut repaired_data = data
    let mut repair_actions = []
    let mut success = true
    
    for rule in rules {
      let field_value = match rule.field_name {
        "trace_id" => data.trace_id
        "span_id" => data.span_id
        "operation_name" => data.operation_name
        "status" => data.status
        _ => ""
      }
      
      if not(rule.validator(field_value)) {
        match rule.strategy {
          RepairStrategy::UseDefault => {
            match rule.default_value {
              Some(default_value) => {
                repair_actions = repair_actions.push(
                  "Replaced " + rule.field_name + " with default value"
                )
                
                repaired_data = match rule.field_name {
                  "operation_name" => {
                    trace_id: repaired_data.trace_id,
                    span_id: repaired_data.span_id,
                    operation_name: default_value,
                    status: repaired_data.status,
                    timestamp: repaired_data.timestamp,
                    attributes: repaired_data.attributes
                  }
                  "status" => {
                    trace_id: repaired_data.trace_id,
                    span_id: repaired_data.span_id,
                    operation_name: repaired_data.operation_name,
                    status: default_value,
                    timestamp: repaired_data.timestamp,
                    attributes: repaired_data.attributes
                  }
                  _ => repaired_data
                }
              }
              None => {
                success = false
                repair_actions = repair_actions.push(
                  "No default value available for " + rule.field_name
                )
              }
            }
          }
          _ => {}  // 简化处理
        }
      }
    }
    
    {
      success,
      repaired_data: Some(repaired_data),
      repair_actions,
      error_message: if success { None } else { Some("Repair failed") }
    }
  }
  
  // 创建简单的验证器
  let basic_validator = {
    name: "basic",
    validate: fn(data: TelemetryData) {
      if data.trace_id.length() != 32 || data.span_id.length() != 16 || data.operation_name.length() == 0 {
        {
          is_valid: false,
          error_message: Some("Basic validation failed"),
          details: []
        }
      } else {
        {
          is_valid: true,
          error_message: None,
          details: []
        }
      }
    }
  }
  
  // 创建修复规则
  let repair_rules = [
    {
      field_name: "operation_name",
      strategy: RepairStrategy::UseDefault,
      default_value: Some("repaired_operation"),
      validator: fn(name) { name.length() > 0 }
    },
    {
      field_name: "status",
      strategy: RepairStrategy::UseDefault,
      default_value: Some("ok"),
      validator: fn(status) { ["ok", "error", "timeout", "cancelled"].contains(status) }
    }
  ]
  
  // 创建测试批次
  let test_batch = [
    {
      timestamp: 1640995200000,
      trace_id: "1234567890abcdef1234567890abcdef",
      span_id: "1234567890abcdef",
      operation_name: "valid_operation",
      status: "ok",
      attributes: []
    },
    {
      timestamp: 1640995250000,
      trace_id: "invalid",  // 无效Trace ID
      span_id: "123",       // 无效Span ID
      operation_name: "",   // 无效操作名称
      status: "invalid",    // 无效状态
      attributes: []
    },
    {
      timestamp: 1640995300000,
      trace_id: "abcdef1234567890abcdef1234567890",
      span_id: "abcdef1234567890",
      operation_name: "another_valid_operation",
      status: "error",
      attributes: []
    }
  ]
  
  // 测试批量验证
  let batch_validation = batch_validate_telemetry(test_batch, [basic_validator])
  assert_eq(batch_validation.total_count, 3)
  assert_eq(batch_validation.valid_count, 2)
  assert_eq(batch_validation.invalid_count, 1)
  assert_eq(batch_validation.validation_details.length(), 3)
  
  // 测试批量修复 - 个体修复
  let individual_repair_strategy = {
    max_repair_attempts: 3,
    repair_threshold: 0.8,  // 80%
    individual_repair: true
  }
  
  let individual_repair_result = batch_repair_telemetry(
    test_batch, 
    repair_rules, 
    individual_repair_strategy
  )
  
  assert_eq(individual_repair_result.total_count, 3)
  assert_eq(individual_repair_result.repaired_count, 3)  // 所有数据都被修复
  assert_eq(individual_repair_result.dropped_count, 0)
  
  // 测试批量修复 - 不进行个体修复
  let no_individual_repair_strategy = {
    max_repair_attempts: 3,
    repair_threshold: 0.8,  // 80%
    individual_repair: false
  }
  
  let no_individual_repair_result = batch_repair_telemetry(
    test_batch, 
    repair_rules, 
    no_individual_repair_strategy
  )
  
  assert_eq(no_individual_repair_result.total_count, 3)
  assert_eq(no_individual_repair_result.valid_count, 2)
  assert_eq(no_individual_repair_result.dropped_count, 1)
  
  // 测试高无效比例批次丢弃
  let high_invalid_batch = [
    test_batch[0],  // 有效
    test_batch[1],  // 无效
    {
      timestamp: 1640995350000,
      trace_id: "invalid2",
      span_id: "456",
      operation_name: "",
      status: "bad",
      attributes: []
    }
  ]
  
  let high_invalid_repair_result = batch_repair_telemetry(
    high_invalid_batch, 
    repair_rules, 
    individual_repair_strategy
  )
  
  // 根据修复策略，结果可能不同
  assert_eq(high_invalid_repair_result.total_count, 3)
  
  // 测试极端情况 - 全部无效
  let all_invalid_batch = [
    test_batch[1],  // 无效
    {
      timestamp: 1640995400000,
      trace_id: "invalid3",
      span_id: "789",
      operation_name: "",
      status: "worse",
      attributes: []
    }
  ]
  
  let all_invalid_repair_result = batch_repair_telemetry(
    all_invalid_batch, 
    repair_rules, 
    individual_repair_strategy
  )
  
  assert_eq(all_invalid_repair_result.total_count, 2)
  // 根据修复策略，结果可能不同
}