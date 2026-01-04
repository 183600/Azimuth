// Azimuth 遥测数据压缩和传输测试用例
// 包含遥测数据的压缩算法、传输协议和网络优化

// 测试1: 基础数据压缩算法
test "基础数据压缩算法" {
  // 简化的RLE（Run-Length Encoding）压缩
  let rle_compress = fn(data: Array[Int]) {
    if data.length() == 0 {
      return []
    }
    
    let mut compressed = []
    let mut current_value = data[0]
    let mut count = 1
    
    for i in 1..data.length() {
      if data[i] == current_value {
        count = count + 1
      } else {
        // 添加压缩的值和计数
        compressed = compressed.push(current_value)
        compressed = compressed.push(count)
        
        // 重置为新的值
        current_value = data[i]
        count = 1
      }
    }
    
    // 添加最后一个值和计数
    compressed = compressed.push(current_value)
    compressed = compressed.push(count)
    
    compressed
  }
  
  // RLE解压缩
  let rle_decompress = fn(compressed: Array[Int>) {
    if compressed.length() == 0 || compressed.length() % 2 != 0 {
      return []
    }
    
    let mut decompressed = []
    
    for i in 0..compressed.length() / 2 {
      let value = compressed[i * 2]
      let count = compressed[i * 2 + 1]
      
      for j in 0..count {
        decompressed = decompressed.push(value)
      }
    }
    
    decompressed
  }
  
  // 简化的字典压缩
  let dictionary_compress = fn(data: Array[String>) {
    let mut dictionary = []
    let mut compressed = []
    let mut next_id = 0
    
    for item in data {
      let existing_index = dictionary.find_index(fn(entry) { entry == item })
      
      match existing_index {
        Some(index) => {
          // 使用现有字典条目
          compressed = compressed.push(index)
        }
        None => {
          // 添加到字典
          dictionary = dictionary.push(item)
          compressed = compressed.push(next_id)
          next_id = next_id + 1
        }
      }
    }
    
    (dictionary, compressed)
  }
  
  // 字典解压缩
  let dictionary_decompress = fn(dictionary: Array[String>, compressed: Array[Int>) {
    let mut decompressed = []
    
    for index in compressed {
      if index < dictionary.length() {
        decompressed = decompressed.push(dictionary[index])
      }
    }
    
    decompressed
  }
  
  // 测试RLE压缩
  let test_data_rle = [1, 1, 1, 2, 2, 3, 3, 3, 3, 2, 2, 2, 4, 4]
  let compressed_rle = rle_compress(test_data_rle)
  let decompressed_rle = rle_decompress(compressed_rle)
  
  // 验证RLE压缩结果
  assert_eq(compressed_rle, [1, 3, 2, 2, 3, 4, 2, 3, 4, 2])
  assert_eq(decompressed_rle, test_data_rle)
  
  // 验证压缩效果
  assert_true(compressed_rle.length() < test_data_rle.length())
  
  // 测试字典压缩
  let test_data_dict = ["error", "warning", "error", "info", "warning", "error", "error"]
  let (dictionary, compressed_dict) = dictionary_compress(test_data_dict)
  let decompressed_dict = dictionary_decompress(dictionary, compressed_dict)
  
  // 验证字典压缩结果
  assert_eq(dictionary, ["error", "warning", "info"])
  assert_eq(compressed_dict, [0, 1, 0, 2, 1, 0, 0])
  assert_eq(decompressed_dict, test_data_dict)
  
  // 验证压缩效果
  assert_true(compressed_dict.length() < test_data_dict.length())
  
  // 测试边界情况
  let empty_data = []
  let empty_compressed = rle_compress(empty_data)
  let empty_decompressed = rle_decompress(empty_compressed)
  assert_eq(empty_compressed, [])
  assert_eq(empty_decompressed, [])
  
  let single_item = [5]
  let single_compressed = rle_compress(single_item)
  let single_decompressed = rle_decompress(single_compressed)
  assert_eq(single_compressed, [5, 1])
  assert_eq(single_decompressed, single_item)
  
  let unique_data = [1, 2, 3, 4, 5]
  let unique_compressed = rle_compress(unique_data)
  let unique_decompressed = rle_decompress(unique_compressed)
  assert_eq(unique_compressed, [1, 1, 2, 1, 3, 1, 4, 1, 5, 1])
  assert_eq(unique_decompressed, unique_data)
  
  // 测试大数据集
  let large_data = []
  for i in 0..100 {
    large_data = large_data + [i % 10] * 10  // 每个数字重复10次
  }
  
  let large_compressed = rle_compress(large_data)
  let large_decompressed = rle_decompress(large_compressed)
  
  // 验证大数据压缩
  assert_eq(large_decompressed, large_data)
  assert_true(large_compressed.length() < large_data.length())
  
  // 计算压缩比
  let compression_ratio = (large_compressed.length() as Float) / (large_data.length() as Float)
  assert_true(compression_ratio < 0.5)  // 至少50%的压缩率
}

// 测试2: 遥测数据专用压缩
test "遥测数据专用压缩" {
  // 定义遥测数据结构
  type TelemetryData = {
    timestamp: Int,
    metric_name: String,
    value: Float,
    tags: Array<(String, String)>
  }
  
  // 时间增量压缩
  let compress_timestamp_deltas = fn(timestamps: Array[Int]) {
    if timestamps.length() == 0 {
      return []
    }
    
    let mut compressed = []
    compressed = compressed.push(timestamps[0])  // 第一个时间戳
    
    for i in 1..timestamps.length() {
      let delta = timestamps[i] - timestamps[i - 1]
      compressed = compressed.push(delta)
    }
    
    compressed
  }
  
  // 时间增量解压缩
  let decompress_timestamp_deltas = fn(compressed: Array[Int>) {
    if compressed.length() == 0 {
      return []
    }
    
    let mut timestamps = []
    timestamps = timestamps.push(compressed[0])
    
    for i in 1..compressed.length() {
      let next_timestamp = timestamps[i - 1] + compressed[i]
      timestamps = timestamps.push(next_timestamp)
    }
    
    timestamps
  }
  
  // 度量名称字典压缩
  let compress_metric_names = fn(data: Array[TelemetryData>) {
    let mut name_dictionary = []
    let mut next_id = 0
    let mut compressed_data = []
    
    for item in data {
      let name_index = match name_dictionary.find_index(fn(name) { name == item.metric_name }) {
        Some(index) => index
        None => {
          name_dictionary = name_dictionary.push(item.metric_name)
          let current_id = next_id
          next_id = next_id + 1
          current_id
        }
      }
      
      let compressed_item = {
        timestamp: item.timestamp,
        metric_name_id: name_index,
        value: item.value,
        tags: item.tags
      }
      
      compressed_data = compressed_data.push(compressed_item)
    }
    
    (name_dictionary, compressed_data)
  }
  
  // 标签字典压缩
  let compress_tags = fn(data: Array[TelemetryData>) {
    let mut tag_dictionary = []
    let mut next_id = 0
    let mut compressed_data = []
    
    for item in data {
      let mut compressed_tags = []
      
      for (key, value) in item.tags {
        let tag_string = key + "=" + value
        let tag_index = match tag_dictionary.find_index(fn(tag) { tag == tag_string }) {
          Some(index) => index
          None => {
            tag_dictionary = tag_dictionary.push(tag_string)
            let current_id = next_id
            next_id = next_id + 1
            current_id
          }
        }
        
        compressed_tags = compressed_tags.push(tag_index)
      }
      
      let compressed_item = {
        timestamp: item.timestamp,
        metric_name: item.metric_name,
        value: item.value,
        tag_ids: compressed_tags
      }
      
      compressed_data = compressed_data.push(compressed_item)
    }
    
    (tag_dictionary, compressed_data)
  }
  
  // 测试时间增量压缩
  let timestamps = [1000, 1010, 1020, 1035, 1050, 1060, 1070]
  let compressed_timestamps = compress_timestamp_deltas(timestamps)
  let decompressed_timestamps = decompress_timestamp_deltas(compressed_timestamps)
  
  // 验证时间增量压缩
  assert_eq(compressed_timestamps, [1000, 10, 10, 15, 15, 10, 10])
  assert_eq(decompressed_timestamps, timestamps)
  
  // 测试度量名称压缩
  let telemetry_data = [
    { timestamp: 1000, metric_name: "cpu.usage", value: 75.5, tags: [("host", "server1")] },
    { timestamp: 1010, metric_name: "memory.usage", value: 60.2, tags: [("host", "server1")] },
    { timestamp: 1020, metric_name: "cpu.usage", value: 78.1, tags: [("host", "server2")] },
    { timestamp: 1030, metric_name: "disk.io", value: 125.3, tags: [("host", "server1")] },
    { timestamp: 1040, metric_name: "cpu.usage", value: 80.0, tags: [("host", "server2")] }
  ]
  
  let (name_dict, compressed_by_name) = compress_metric_names(telemetry_data)
  
  // 验证度量名称字典
  assert_eq(name_dict, ["cpu.usage", "memory.usage", "disk.io"])
  
  // 验证压缩后的数据
  assert_eq(compressed_by_name[0].metric_name_id, 0)  // cpu.usage
  assert_eq(compressed_by_name[1].metric_name_id, 1)  // memory.usage
  assert_eq(compressed_by_name[2].metric_name_id, 0)  // cpu.usage
  assert_eq(compressed_by_name[3].metric_name_id, 2)  // disk.io
  assert_eq(compressed_by_name[4].metric_name_id, 0)  // cpu.usage
  
  // 测试标签压缩
  let (tag_dict, compressed_by_tags) = compress_tags(telemetry_data)
  
  // 验证标签字典
  assert_eq(tag_dict, ["host=server1", "host=server2"])
  
  // 验证压缩后的标签
  assert_eq(compressed_by_tags[0].tag_ids, [0])  // host=server1
  assert_eq(compressed_by_tags[1].tag_ids, [0])  // host=server1
  assert_eq(compressed_by_tags[2].tag_ids, [1])  // host=server2
  assert_eq(compressed_by_tags[3].tag_ids, [0])  // host=server1
  assert_eq(compressed_by_tags[4].tag_ids, [1])  // host=server2
  
  // 计算压缩效果
  let original_size = telemetry_data.length() * 100  // 假设每条记录100字节
  let compressed_size = name_dict.length() * 20 + compressed_by_name.length() * 80  // 估算
  let compression_ratio = (compressed_size as Float) / (original_size as Float)
  
  assert_true(compression_ratio < 1.0)  // 应该有压缩效果
}

// 测试3: 遥测数据传输协议
test "遥测数据传输协议" {
  // 定义传输协议头
  type ProtocolHeader = {
    version: Int,
    message_type: String,
    compression_type: String,
    payload_size: Int,
    checksum: Int
  }
  
  // 定义传输消息
  type TransportMessage = {
    header: ProtocolHeader,
    payload: Array[Int]
  }
  
  // 创建协议头
  let create_header = fn(
    message_type: String, 
    compression_type: String, 
    payload_size: Int
  ) {
    let checksum = (message_type.length() + compression_type.length() + payload_size) % 10000  // 简化的校验和
    
    {
      version: 1,
      message_type,
      compression_type,
      payload_size,
      checksum
    }
  }
  
  // 序列化协议头
  let serialize_header = fn(header: ProtocolHeader) {
    let mut serialized = []
    
    // 版本号
    serialized = serialized + [header.version / 100, header.version % 100]
    
    // 消息类型长度和内容
    serialized = serialized.push(header.message_type.length())
    for c in header.message_type.to_char_array() {
      serialized = serialized.push(c.to_int())
    }
    
    // 压缩类型长度和内容
    serialized = serialized.push(header.compression_type.length())
    for c in header.compression_type.to_char_array() {
      serialized = serialized.push(c.to_int())
    }
    
    // 载荷大小（4字节）
    serialized = serialized + [
      (header.payload_size >> 24) & 0xFF,
      (header.payload_size >> 16) & 0xFF,
      (header.payload_size >> 8) & 0xFF,
      header.payload_size & 0xFF
    ]
    
    // 校验和（2字节）
    serialized = serialized + [
      (header.checksum >> 8) & 0xFF,
      header.checksum & 0xFF
    ]
    
    serialized
  }
  
  // 反序列化协议头
  let deserialize_header = fn(data: Array[Int]) {
    if data.length() < 10 {
      return None
    }
    
    let mut index = 0
    
    // 版本号
    let version = data[index] * 100 + data[index + 1]
    index = index + 2
    
    // 消息类型
    let message_type_length = data[index]
    index = index + 1
    
    let mut message_type = ""
    for i in 0..message_type_length {
      if index + i < data.length() {
        message_type = message_type + (data[index + i] as Char).to_string()
      }
    }
    index = index + message_type_length
    
    // 压缩类型
    let compression_type_length = data[index]
    index = index + 1
    
    let mut compression_type = ""
    for i in 0..compression_type_length {
      if index + i < data.length() {
        compression_type = compression_type + (data[index + i] as Char).to_string()
      }
    }
    index = index + compression_type_length
    
    // 载荷大小
    let payload_size = (data[index] << 24) | (data[index + 1] << 16) | 
                      (data[index + 2] << 8) | data[index + 3]
    index = index + 4
    
    // 校验和
    let checksum = (data[index] << 8) | data[index + 1]
    
    Some({
      version,
      message_type,
      compression_type,
      payload_size,
      checksum
    })
  }
  
  // 创建传输消息
  let create_message = fn(message_type: String, compression_type: String, payload: Array[Int>) {
    let header = create_header(message_type, compression_type, payload.length())
    let serialized_header = serialize_header(header)
    
    {
      header,
      payload: serialized_header + payload
    }
  }
  
  // 解析传输消息
  let parse_message = fn(message: TransportMessage) {
    let header_data = message.payload.slice(0, message.payload.length() - message.header.payload_size)
    let payload_data = message.payload.slice(header_data.length())
    
    match deserialize_header(header_data) {
      Some(parsed_header) => {
        if parsed_header.checksum == message.header.checksum {
          Some((parsed_header, payload_data))
        } else {
          None  // 校验和不匹配
        }
      }
      None => None
    }
  }
  
  // 测试协议头序列化和反序列化
  let test_header = create_header("telemetry", "rle", 1024)
  let serialized = serialize_header(test_header)
  let parsed = deserialize_header(serialized)
  
  match parsed {
    Some(header) => {
      assert_eq(header.version, test_header.version)
      assert_eq(header.message_type, test_header.message_type)
      assert_eq(header.compression_type, test_header.compression_type)
      assert_eq(header.payload_size, test_header.payload_size)
      assert_eq(header.checksum, test_header.checksum)
    }
    None => assert_true(false)
  }
  
  // 测试传输消息创建和解析
  let test_payload = [1, 2, 3, 4, 5, 1, 1, 1, 2, 2, 3]
  let test_message = create_message("metrics", "rle", test_payload)
  
  match parse_message(test_message) {
    Some((header, payload)) => {
      assert_eq(header.message_type, "metrics")
      assert_eq(header.compression_type, "rle")
      assert_eq(payload, test_payload)
    }
    None => assert_true(false)
  }
  
  // 测试不同消息类型
  let span_payload = [10, 20, 30, 40, 50]
  let span_message = create_message("spans", "dict", span_payload)
  
  match parse_message(span_message) {
    Some((header, payload)) => {
      assert_eq(header.message_type, "spans")
      assert_eq(header.compression_type, "dict")
      assert_eq(payload, span_payload)
    }
    None => assert_true(false)
  }
  
  // 测试错误处理
  let corrupt_payload = [1, 2, 3]  // 太短，无法解析头
  let corrupt_message = {
    header: test_header,
    payload: corrupt_payload
  }
  
  let corrupt_result = parse_message(corrupt_message)
  assert_eq(corrupt_result, None)
}

// 测试4: 网络传输优化
test "网络传输优化" {
  // 定义批处理策略
  type BatchStrategy = {
    max_batch_size: Int,
    max_wait_time_ms: Int,
    compression_threshold: Int
  }
  
  // 定义传输缓冲区
  type TransmissionBuffer = {
    items: Array[TelemetryData],
    last_flush: Int,
    batch_strategy: BatchStrategy
  }
  
  // 创建批处理策略
  let create_batch_strategy = fn(max_size: Int, max_wait: Int, compression_threshold: Int) {
    {
      max_batch_size: max_size,
      max_wait_time_ms: max_wait,
      compression_threshold: compression_threshold
    }
  }
  
  // 创建传输缓冲区
  let create_transmission_buffer = fn(strategy: BatchStrategy) {
    {
      items: [],
      last_flush: Time::now(),
      batch_strategy: strategy
    }
  }
  
  // 添加项目到缓冲区
  let add_to_buffer = fn(buffer: TransmissionBuffer, item: TelemetryData) {
    let mut updated_buffer = buffer
    updated_buffer.items = updated_buffer.items.push(item)
    updated_buffer
  }
  
  // 检查是否应该刷新缓冲区
  let should_flush = fn(buffer: TransmissionBuffer) {
    let current_time = Time::now()
    let time_elapsed = current_time - buffer.last_flush
    let size_threshold = buffer.items.length() >= buffer.batch_strategy.max_batch_size
    let time_threshold = time_elapsed >= buffer.batch_strategy.max_wait_time_ms
    
    size_threshold || time_threshold
  }
  
  // 刷新缓冲区
  let flush_buffer = fn(buffer: TransmissionBuffer) {
    let current_time = Time::now()
    
    // 决定是否压缩
    let should_compress = buffer.items.length() >= buffer.batch_strategy.compression_threshold
    
    // 模拟压缩过程
    let compressed_size = if should_compress {
      (buffer.items.length() as Float * 0.6) as Int  // 40%压缩率
    } else {
      buffer.items.length()
    }
    
    // 创建传输消息
    let compression_type = if should_compress { "rle" } else { "none" }
    let payload = []  // 简化：实际应该是压缩后的数据
    
    let message = create_message("telemetry_batch", compression_type, payload)
    
    // 重置缓冲区
    let updated_buffer = {
      items: [],
      last_flush: current_time,
      batch_strategy: buffer.batch_strategy
    }
    
    (updated_buffer, message, buffer.items.length(), compressed_size)
  }
  
  // 使用前一个测试的函数
  let create_message = fn(message_type: String, compression_type: String, payload: Array[Int>) {
    let header = create_header(message_type, compression_type, payload.length())
    {
      header,
      payload
    }
  }
  
  let create_header = fn(message_type: String, compression_type: String, payload_size: Int) {
    let checksum = (message_type.length() + compression_type.length() + payload_size) % 10000
    
    {
      version: 1,
      message_type,
      compression_type,
      payload_size,
      checksum
    }
  }
  
  // 生成测试遥测数据
  let generate_telemetry_data = fn(count: Int, start_time: Int) {
    let mut data = []
    
    for i in 0..count {
      let item = {
        timestamp: start_time + i * 10,
        metric_name: "test.metric." + (i % 5).to_string(),
        value: (i * 1.5) as Float,
        tags: [
          ("host", "server-" + (i % 3).to_string()),
          ("region", "us-west-" + (i % 2).to_string())
        ]
      }
      
      data = data.push(item)
    }
    
    data
  }
  
  // 测试批处理策略
  let strategy = create_batch_strategy(10, 1000, 5)
  let buffer = create_transmission_buffer(strategy)
  
  // 添加少量数据，不应触发刷新
  let mut current_buffer = buffer
  let small_batch = generate_telemetry_data(3, 1000)
  
  for item in small_batch {
    current_buffer = add_to_buffer(current_buffer, item)
  }
  
  assert_eq(current_buffer.items.length(), 3)
  assert_false(should_flush(current_buffer))
  
  // 添加更多数据触发大小阈值
  let large_batch = generate_telemetry_data(8, 1030)
  
  for item in large_batch {
    current_buffer = add_to_buffer(current_buffer, item)
  }
  
  assert_eq(current_buffer.items.length(), 11)
  assert_true(should_flush(current_buffer))
  
  // 刷新缓冲区
  let (flushed_buffer, message, original_size, compressed_size) = flush_buffer(current_buffer)
  
  // 验证刷新结果
  assert_eq(flushed_buffer.items.length(), 0)
  assert_eq(original_size, 11)
  assert_true(compressed_size < original_size)  // 应该有压缩
  assert_eq(message.header.compression_type, "rle")
  
  // 测试时间阈值触发
  let time_buffer = create_transmission_buffer(strategy)
  let mut current_time_buffer = time_buffer
  
  // 添加少量数据
  let time_batch = generate_telemetry_data(3, 2000)
  
  for item in time_batch {
    current_time_buffer = add_to_buffer(current_time_buffer, item)
  }
  
  assert_eq(current_time_buffer.items.length(), 3)
  assert_false(should_flush(current_time_buffer))
  
  // 模拟时间流逝（在实际环境中，这需要等待）
  let time_elapsed_buffer = {
    items: current_time_buffer.items,
    last_flush: Time::now() - 2000,  // 2秒前
    batch_strategy: current_time_buffer.batch_strategy
  }
  
  assert_true(should_flush(time_elapsed_buffer))
  
  // 测试不同批处理策略
  let high_volume_strategy = create_batch_strategy(50, 500, 20)
  let low_volume_strategy = create_batch_strategy(5, 2000, 3)
  
  let high_buffer = create_transmission_buffer(high_volume_strategy)
  let low_buffer = create_transmission_buffer(low_volume_strategy)
  
  // 添加相同数据到不同缓冲区
  let test_data = generate_telemetry_data(25, 3000)
  
  let mut current_high_buffer = high_buffer
  let mut current_low_buffer = low_buffer
  
  for item in test_data {
    current_high_buffer = add_to_buffer(current_high_buffer, item)
    current_low_buffer = add_to_buffer(current_low_buffer, item)
  }
  
  // 高容量缓冲区应该触发刷新
  assert_true(should_flush(current_high_buffer))
  
  // 低容量缓冲区也应该触发刷新（超过大小阈值）
  assert_true(should_flush(current_low_buffer))
  
  // 刷新并比较压缩效果
  let (_, high_message, high_original, high_compressed) = flush_buffer(current_high_buffer)
  let (_, low_message, low_original, low_compressed) = flush_buffer(current_low_buffer)
  
  // 高容量缓冲区应该使用压缩
  assert_eq(high_message.header.compression_type, "rle")
  assert_true(high_compressed < high_original)
  
  // 低容量缓冲区可能不使用压缩（取决于阈值）
  if low_original >= low_buffer.batch_strategy.compression_threshold {
    assert_eq(low_message.header.compression_type, "rle")
    assert_true(low_compressed < low_original)
  } else {
    assert_eq(low_message.header.compression_type, "none")
    assert_eq(low_compressed, low_original)
  }
  
  // 计算传输效率
  let high_compression_ratio = (high_compressed as Float) / (high_original as Float)
  let low_compression_ratio = (low_compressed as Float) / (low_original as Float)
  
  // 高容量缓冲区应该有更好的压缩比
  assert_true(high_compression_ratio <= low_compression_ratio)
}

// 测试5: 网络错误处理和重试
test "网络错误处理和重试" {
  // 定义网络状态
  enum NetworkStatus {
    Connected
    Disconnected
    Slow
    Error
  }
  
  // 定义传输结果
  type TransmissionResult = {
    success: Bool,
    bytes_sent: Int,
    error_message: Option<String>,
    retry_count: Int
  }
  
  // 定义重试策略
  type RetryStrategy = {
    max_attempts: Int,
    initial_delay_ms: Int,
    max_delay_ms: Int,
    backoff_multiplier: Float
  }
  
  // 模拟网络传输
  let simulate_transmission = fn(
    message: TransportMessage, 
    network_status: NetworkStatus,
    attempt: Int
  ) {
    match network_status {
      NetworkStatus::Connected => {
        // 成功传输
        let bytes_sent = message.payload.length()
        {
          success: true,
          bytes_sent,
          error_message: None,
          retry_count: attempt - 1
        }
      }
      NetworkStatus::Disconnected => {
        // 连接失败
        {
          success: false,
          bytes_sent: 0,
          error_message: Some("Connection refused"),
          retry_count: attempt - 1
        }
      }
      NetworkStatus::Slow => {
        // 网络慢，可能超时
        if attempt <= 2 {
          {
            success: false,
            bytes_sent: message.payload.length() / 2,
            error_message: Some("Timeout"),
            retry_count: attempt - 1
          }
        } else {
          // 重试成功
          {
            success: true,
            bytes_sent: message.payload.length(),
            error_message: None,
            retry_count: attempt - 1
          }
        }
      }
      NetworkStatus::Error => {
        // 网络错误，总是失败
        {
          success: false,
          bytes_sent: 0,
          error_message: Some("Network error"),
          retry_count: attempt - 1
        }
      }
    }
  }
  
  // 计算重试延迟
  let calculate_retry_delay = fn(strategy: RetryStrategy, attempt: Int) {
    let delay = strategy.initial_delay_ms as Float * 
               (strategy.backoff_multiplier.pow((attempt - 1) as Float))
    
    let capped_delay = if delay > strategy.max_delay_ms as Float {
      strategy.max_delay_ms as Float
    } else {
      delay
    }
    
    capped_delay as Int
  }
  
  // 带重试的传输
  let transmit_with_retry = fn(
    message: TransportMessage,
    network_status: NetworkStatus,
    retry_strategy: RetryStrategy
  ) {
    let mut attempt = 1
    let mut total_delay = 0
    let mut result = simulate_transmission(message, network_status, attempt)
    
    while not(result.success) && attempt < retry_strategy.max_attempts {
      attempt = attempt + 1
      let delay = calculate_retry_delay(retry_strategy, attempt)
      total_delay = total_delay + delay
      
      // 模拟延迟等待
      result = simulate_transmission(message, network_status, attempt)
    }
    
    {
      success: result.success,
      bytes_sent: result.bytes_sent,
      error_message: result.error_message,
      retry_count: result.retry_count,
      total_delay_ms: total_delay
    }
  }
  
  // 使用前一个测试的类型
  type TransportMessage = {
    header: ProtocolHeader,
    payload: Array[Int>
  }
  
  type ProtocolHeader = {
    version: Int,
    message_type: String,
    compression_type: String,
    payload_size: Int,
    checksum: Int
  }
  
  // 创建测试消息
  let create_test_message = fn(payload_size: Int) {
    let mut payload = []
    for i in 0..payload_size {
      payload = payload.push(i % 256)
    }
    
    let header = {
      version: 1,
      message_type: "test",
      compression_type: "none",
      payload_size,
      checksum: payload_size % 10000
    }
    
    {
      header,
      payload
    }
  }
  
  // 测试不同网络状态下的传输
  let test_message = create_test_message(1024)
  let retry_strategy = {
    max_attempts: 3,
    initial_delay_ms: 100,
    max_delay_ms: 1000,
    backoff_multiplier: 2.0
  }
  
  // 测试连接正常
  let connected_result = transmit_with_retry(
    test_message, 
    NetworkStatus::Connected, 
    retry_strategy
  )
  
  assert_true(connected_result.success)
  assert_eq(connected_result.bytes_sent, 1024)
  assert_eq(connected_result.retry_count, 0)
  assert_eq(connected_result.total_delay_ms, 0)
  
  // 测试连接断开
  let disconnected_result = transmit_with_retry(
    test_message, 
    NetworkStatus::Disconnected, 
    retry_strategy
  )
  
  assert_false(disconnected_result.success)
  assert_eq(disconnected_result.bytes_sent, 0)
  assert_eq(disconnected_result.retry_count, 2)  // 最大尝试次数-1
  assert_true(disconnected_result.total_delay_ms > 0)
  
  // 测试网络慢
  let slow_result = transmit_with_retry(
    test_message, 
    NetworkStatus::Slow, 
    retry_strategy
  )
  
  assert_true(slow_result.success)
  assert_eq(slow_result.bytes_sent, 1024)
  assert_eq(slow_result.retry_count, 2)  // 前2次失败，第3次成功
  assert_true(slow_result.total_delay_ms > 0)
  
  // 测试网络错误
  let error_result = transmit_with_retry(
    test_message, 
    NetworkStatus::Error, 
    retry_strategy
  )
  
  assert_false(error_result.success)
  assert_eq(error_result.bytes_sent, 0)
  assert_eq(error_result.retry_count, 2)  // 最大尝试次数-1
  assert_true(error_result.total_delay_ms > 0)
  
  // 测试重试延迟计算
  let delay1 = calculate_retry_delay(retry_strategy, 1)
  let delay2 = calculate_retry_delay(retry_strategy, 2)
  let delay3 = calculate_retry_delay(retry_strategy, 3)
  
  assert_eq(delay1, 100)  // 初始延迟
  assert_eq(delay2, 200)  // 100 * 2
  assert_eq(delay3, 400)  // 200 * 2
  
  // 测试最大延迟限制
  let capped_strategy = {
    max_attempts: 5,
    initial_delay_ms: 100,
    max_delay_ms: 300,
    backoff_multiplier: 2.0
  }
  
  let capped_delay1 = calculate_retry_delay(capped_strategy, 1)
  let capped_delay2 = calculate_retry_delay(capped_strategy, 2)
  let capped_delay3 = calculate_retry_delay(capped_strategy, 3)
  
  assert_eq(capped_delay1, 100)  // 初始延迟
  assert_eq(capped_delay2, 200)  // 100 * 2
  assert_eq(capped_delay3, 300)  // 会被限制在最大延迟
  
  // 测试不同重试策略
  let aggressive_strategy = {
    max_attempts: 5,
    initial_delay_ms: 50,
    max_delay_ms: 500,
    backoff_multiplier: 1.5
  }
  
  let conservative_strategy = {
    max_attempts: 2,
    initial_delay_ms: 200,
    max_delay_ms: 1000,
    backoff_multiplier: 3.0
  }
  
  let aggressive_result = transmit_with_retry(
    test_message, 
    NetworkStatus::Slow, 
    aggressive_strategy
  )
  
  let conservative_result = transmit_with_retry(
    test_message, 
    NetworkStatus::Slow, 
    conservative_strategy
  )
  
  // 两种策略都应该成功
  assert_true(aggressive_result.success)
  assert_true(conservative_result.success)
  
  // 激进策略应该有更多重试次数但总延迟更少
  assert_true(aggressive_result.retry_count >= conservative_result.retry_count)
  assert_true(aggressive_result.total_delay_ms <= conservative_result.total_delay_ms)
}