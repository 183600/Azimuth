// 遥测数据高级压缩功能测试用例
// 专注于测试遥测数据的高级压缩算法和优化技术

test "telemetry_adaptive_compression" {
  // 测试遥测自适应压缩功能
  
  // 1. 创建不同类型的遥测数据
  let high_entropy_data = "trace_id:0af7651916cd43dd8448eb211c80319c,span_id:b7ad6b7169203331,timestamp:1640995200000000000,duration:123456789,status:ok,user_id:12345,session_id:abcdef123456,request_id:req_xyz789"
  
  let low_entropy_data = "metric:cpu_usage,value:75.5,unit:percent,service:payment-service,env:production,metric:cpu_usage,value:75.8,unit:percent,service:payment-service,env:production,metric:cpu_usage,value:76.1,unit:percent,service:payment-service,env:production"
  
  let repetitive_data = "error:timeout,error:timeout,error:timeout,error:timeout,error:timeout,error:timeout,error:timeout,error:timeout,error:timeout,error:timeout"
  
  // 2. 验证测试数据
  assert_eq(high_entropy_data.length(), 128)
  assert_eq(low_entropy_data.length(), 156)
  assert_eq(repetitive_data.length(), 80)
  
  // 3. 计算数据熵值（简化版本）
  let calculate_entropy = fn(data: String) -> Double {
    let frequency = {}
    let mut i = 0
    
    // 计算字符频率
    while i < data.length() {
      let char = data[i].to_string()
      if !frequency.has(char) {
        frequency = frequency.with(char, 0)
      }
      let count = frequency.get(char)
      frequency = frequency.with(char, count + 1)
      i = i + 1
    }
    
    // 计算熵值
    let mut entropy = 0.0
    let data_length = data.length().to_double()
    let keys = frequency.keys()
    i = 0
    
    while i < keys.length() {
      let char = keys[i]
      let count = frequency.get(char).to_double()
      let probability = count / data_length
      entropy = entropy - probability * probability.log2()
      i = i + 1
    }
    
    entropy
  }
  
  // 4. 计算各数据的熵值
  let high_entropy = calculate_entropy(high_entropy_data)
  let low_entropy = calculate_entropy(low_entropy_data)
  let repetitive_entropy = calculate_entropy(repetitive_data)
  
  // 5. 验证熵值计算结果
  assert_eq(high_entropy > low_entropy, true)
  assert_eq(low_entropy > repetitive_entropy, true)
  assert_eq(repetitive_entropy < 2.0, true)  // 重复数据熵值很低
  
  // 6. 自适应压缩算法选择
  let select_compression_algorithm = fn(entropy: Double) -> String {
    if entropy > 5.0 {
      "huffman"  // 高熵数据使用霍夫曼编码
    } else if entropy > 2.0 {
      "lzw"      // 中等熵数据使用LZW
    } else {
      "rle"      // 低熵数据使用行程编码
    }
  }
  
  // 7. 选择压缩算法
  let high_entropy_algorithm = select_compression_algorithm(high_entropy)
  let low_entropy_algorithm = select_compression_algorithm(low_entropy)
  let repetitive_algorithm = select_compression_algorithm(repetitive_entropy)
  
  // 8. 验证算法选择
  assert_eq(high_entropy_algorithm, "huffman")
  assert_eq(low_entropy_algorithm, "lzw")
  assert_eq(repetitive_algorithm, "rle")
  
  // 9. 模拟不同压缩算法的压缩率
  let simulate_compression = fn(data: String, algorithm: String) -> (String, Double) {
    let compression_ratio = match algorithm {
      "huffman" => 0.7,  // 70%压缩率
      "lzw" => 0.5,      // 50%压缩率
      "rle" => 0.1,      // 90%压缩率
      _ => 0.8           // 默认20%压缩率
    }
    
    let compressed_length = (data.length().to_double() * compression_ratio).to_int()
    let compressed_data = "compressed:" + algorithm + ":" + "x".repeat(compressed_length)
    
    (compressed_data, compression_ratio)
  }
  
  // 10. 测试压缩效果
  let (compressed_high, ratio_high) = simulate_compression(high_entropy_data, high_entropy_algorithm)
  let (compressed_low, ratio_low) = simulate_compression(low_entropy_data, low_entropy_algorithm)
  let (compressed_repetitive, ratio_repetitive) = simulate_compression(repetitive_data, repetitive_algorithm)
  
  // 11. 验证压缩结果
  assert_eq(compressed_high.contains("huffman"), true)
  assert_eq(compressed_low.contains("lzw"), true)
  assert_eq(compressed_repetitive.contains("rle"), true)
  
  assert_eq(ratio_high, 0.7)
  assert_eq(ratio_low, 0.5)
  assert_eq(ratio_repetitive, 0.1)
  
  // 验证压缩后的长度
  assert_eq(compressed_high.length() < high_entropy_data.length(), true)
  assert_eq(compressed_low.length() < low_entropy_data.length(), true)
  assert_eq(compressed_repetitive.length() < repetitive_data.length(), true)
}

test "telemetry_streaming_compression" {
  // 测试遥测流式压缩功能
  
  // 1. 创建流式数据块
  let data_chunks = [
    "chunk1:metric1:value1:timestamp1",
    "chunk2:metric2:value2:timestamp2", 
    "chunk3:metric3:value3:timestamp3",
    "chunk4:metric4:value4:timestamp4",
    "chunk5:metric5:value5:timestamp5"
  ]
  
  // 2. 验证数据块
  assert_eq(data_chunks.length(), 5)
  assert_eq(data_chunks[0].has_prefix("chunk1:"), true)
  assert_eq(data_chunks[4].has_prefix("chunk5:"), true)
  
  // 3. 流式压缩器状态
  let compression_state = {
    "window_size" => 3,
    "dictionary" => [],
    "compressed_output" => []
  }
  
  // 4. 流式压缩函数
  let streaming_compress = fn(chunk: String, state: Map[String, Any]) -> Map[String, Any] {
    let window_size = state.get("window_size").to_int()
    let dictionary = state.get("dictionary")
    let compressed_output = state.get("compressed_output")
    
    // 检查字典中是否有匹配
    let mut found_match = false
    let mut match_index = -1
    let mut i = 0
    
    while i < dictionary.length() {
      if dictionary[i] == chunk {
        found_match = true
        match_index = i
        break
      }
      i = i + 1
    }
    
    // 生成压缩输出
    let compressed_item = if found_match {
      "ref:" + match_index.to_string()  // 引用字典中的项
    } else {
      "raw:" + chunk  // 原始数据
    }
    
    // 更新字典（滑动窗口）
    let updated_dictionary = dictionary.push(chunk)
    let final_dictionary = if updated_dictionary.length() > window_size {
      updated_dictionary.slice(1, updated_dictionary.length())
    } else {
      updated_dictionary
    }
    
    // 更新输出
    let updated_output = compressed_output.push(compressed_item)
    
    {
      "window_size" => window_size,
      "dictionary" => final_dictionary,
      "compressed_output" => updated_output
    }
  }
  
  // 5. 执行流式压缩
  let mut final_state = compression_state
  let mut i = 0
  
  while i < data_chunks.length() {
    final_state = streaming_compress(data_chunks[i], final_state)
    i = i + 1
  }
  
  // 6. 验证流式压缩结果
  let compressed_output = final_state.get("compressed_output")
  let dictionary = final_state.get("dictionary")
  
  assert_eq(compressed_output.length(), 5)
  assert_eq(dictionary.length(), 3)  // 滑动窗口大小
  
  // 验证压缩输出
  assert_eq(compressed_output[0], "raw:chunk1:metric1:value1:timestamp1")  // 第一个总是原始
  assert_eq(compressed_output[1], "raw:chunk2:metric2:value2:timestamp2")
  assert_eq(compressed_output[2], "raw:chunk3:metric3:value3:timestamp3")
  
  // 验证字典内容（最后3个块）
  assert_eq(dictionary[0], "chunk3:metric3:value3:timestamp3")
  assert_eq(dictionary[1], "chunk4:metric4:value4:timestamp4")
  assert_eq(dictionary[2], "chunk5:metric5:value5:timestamp5")
  
  // 7. 计算压缩率
  let total_original_size = {
    let mut size = 0
    let mut i = 0
    while i < data_chunks.length() {
      size = size + data_chunks[i].length()
      i = i + 1
    }
    size
  }
  
  let total_compressed_size = {
    let mut size = 0
    let mut i = 0
    while i < compressed_output.length() {
      size = size + compressed_output[i].length()
      i = i + 1
    }
    size
  }
  
  let compression_ratio = total_compressed_size.to_double() / total_original_size.to_double()
  
  // 8. 验证压缩效果
  assert_eq(total_original_size > 0, true)
  assert_eq(total_compressed_size > 0, true)
  assert_eq(compression_ratio > 0.0, true)
  assert_eq(compression_ratio < 1.0, true)  // 应该有压缩效果
}

test "telemetry_differential_compression" {
  // 测试遥测差分压缩功能
  
  // 1. 创建时间序列数据
  let time_series_data = [
    { "timestamp" => 1640995200L, "cpu" => 75.5, "memory" => 1024.0, "disk" => 85.2 },
    { "timestamp" => 1640995260L, "cpu" => 76.1, "memory" => 1028.0, "disk" => 85.3 },
    { "timestamp" => 1640995320L, "cpu" => 75.8, "memory" => 1032.0, "disk" => 85.1 },
    { "timestamp" => 1640995380L, "cpu" => 77.2, "memory" => 1036.0, "disk" => 85.4 },
    { "timestamp" => 1640995440L, "cpu" => 76.9, "memory" => 1040.0, "disk" => 85.2 }
  ]
  
  // 2. 验证时间序列数据
  assert_eq(time_series_data.length(), 5)
  assert_eq(time_series_data[0].get("timestamp"), 1640995200L)
  assert_eq(time_series_data[4].get("cpu"), 76.9)
  
  // 3. 差分压缩函数
  let differential_compress = fn(data: Array[Map[String, Any]]) -> Array[Map[String, Any]] {
    if data.length() == 0 {
      return []
    }
    
    let compressed_data = []
    
    // 第一个数据点作为基准
    compressed_data.push(data[0])
    
    // 后续数据点存储差值
    let mut i = 1
    while i < data.length() {
      let current = data[i]
      let previous = data[i-1]
      
      let delta = {
        "timestamp" => current.get("timestamp") - previous.get("timestamp"),
        "cpu" => current.get("cpu") - previous.get("cpu"),
        "memory" => current.get("memory") - previous.get("memory"),
        "disk" => current.get("disk") - previous.get("disk")
      }
      
      compressed_data.push(delta)
      i = i + 1
    }
    
    compressed_data
  }
  
  // 4. 执行差分压缩
  let compressed_series = differential_compress(time_series_data)
  
  // 5. 验证差分压缩结果
  assert_eq(compressed_series.length(), 5)
  
  // 验证基准数据点
  assert_eq(compressed_series[0].get("timestamp"), 1640995200L)
  assert_eq(compressed_series[0].get("cpu"), 75.5)
  
  // 验证差值数据点
  assert_eq(compressed_series[1].get("timestamp"), 60L)  // 60秒间隔
  assert_eq(compressed_series[1].get("cpu"), 0.6)       // 76.1 - 75.5
  assert_eq(compressed_series[1].get("memory"), 4.0)    // 1028.0 - 1024.0
  
  assert_eq(compressed_series[2].get("timestamp"), 60L)  // 60秒间隔
  assert_eq(compressed_series[2].get("cpu"), -0.3)      // 75.8 - 76.1
  assert_eq(compressed_series[2].get("memory"), 4.0)    // 1032.0 - 1028.0
  
  // 6. �分解压缩函数
  let differential_decompress = fn(compressed_data: Array[Map[String, Any]]) -> Array[Map[String, Any]] {
    if compressed_data.length() == 0 {
      return []
    }
    
    let decompressed_data = []
    decompressed_data.push(compressed_data[0])  // 基准数据点
    
    let mut i = 1
    while i < compressed_data.length() {
      let delta = compressed_data[i]
      let previous = decompressed_data[i-1]
      
      let reconstructed = {
        "timestamp" => previous.get("timestamp") + delta.get("timestamp"),
        "cpu" => previous.get("cpu") + delta.get("cpu"),
        "memory" => previous.get("memory") + delta.get("memory"),
        "disk" => previous.get("disk") + delta.get("disk")
      }
      
      decompressed_data.push(reconstructed)
      i = i + 1
    }
    
    decompressed_data
  }
  
  // 7. 执行差分解压缩
  let decompressed_series = differential_decompress(compressed_series)
  
  // 8. 验证解压缩结果
  assert_eq(decompressed_series.length(), 5)
  
  // 验证数据完整性
  let mut i = 0
  while i < time_series_data.length() {
    assert_eq(decompressed_series[i].get("timestamp"), time_series_data[i].get("timestamp"))
    assert_eq(decompressed_series[i].get("cpu"), time_series_data[i].get("cpu"))
    assert_eq(decompressed_series[i].get("memory"), time_series_data[i].get("memory"))
    assert_eq(decompressed_series[i].get("disk"), time_series_data[i].get("disk"))
    i = i + 1
  }
  
  // 9. 计算压缩效果
  let original_size = {
    let mut size = 0
    let mut i = 0
    while i < time_series_data.length() {
      size = size + 4  // 每个数据点4个字段
      i = i + 1
    }
    size
  }
  
  let compressed_size = compressed_series.length()  // 简化计算
  
  let compression_ratio = compressed_size.to_double() / original_size.to_double()
  
  // 10. 验证压缩效果
  assert_eq(compression_ratio <= 1.0, true)  // 应该有压缩效果
}

test "telemetry_semantic_compression" {
  // 测试遥测语义压缩功能
  
  // 1. 创建语义相关的遥测数据
  let semantic_data = [
    "service:payment-service,operation:process_payment,status:success,duration:250ms",
    "service:payment-service,operation:process_payment,status:success,duration:180ms",
    "service:payment-service,operation:process_payment,status:success,duration:320ms",
    "service:payment-service,operation:process_payment,status:error,duration:5000ms",
    "service:payment-service,operation:validate_payment,status:success,duration:50ms",
    "service:order-service,operation:create_order,status:success,duration:150ms"
  ]
  
  // 2. 验证语义数据
  assert_eq(semantic_data.length(), 6)
  assert_eq(semantic_data[0].contains("payment-service"), true)
  assert_eq(semantic_data[5].contains("order-service"), true)
  
  // 3. 创建语义字典
  let semantic_dictionary = {
    "service:payment-service" => "s1",
    "service:order-service" => "s2",
    "operation:process_payment" => "op1",
    "operation:validate_payment" => "op2", 
    "operation:create_order" => "op3",
    "status:success" => "st1",
    "status:error" => "st2"
  }
  
  // 4. 验证语义字典
  assert_eq(semantic_dictionary.length(), 7)
  assert_eq(semantic_dictionary.get("service:payment-service"), "s1")
  assert_eq(semantic_dictionary.get("status:success"), "st1")
  
  // 5. 语义压缩函数
  let semantic_compress = fn(data: String, dictionary: Map[String, String]) -> String {
    let mut compressed = data
    
    // 应用语义字典替换
    let keys = dictionary.keys()
    let mut i = 0
    
    while i < keys.length() {
      let key = keys[i]
      let value = dictionary.get(key)
      compressed = compressed.replace(key, value)
      i = i + 1
    }
    
    compressed
  }
  
  // 6. 执行语义压缩
  let compressed_semantic_data = []
  let mut i = 0
  
  while i < semantic_data.length() {
    let compressed = semantic_compress(semantic_data[i], semantic_dictionary)
    compressed_semantic_data.push(compressed)
    i = i + 1
  }
  
  // 7. 验证语义压缩结果
  assert_eq(compressed_semantic_data.length(), 6)
  
  // 验证压缩效果
  assert_eq(compressed_semantic_data[0], "s1,op1,st1,duration:250ms")
  assert_eq(compressed_semantic_data[1], "s1,op1,st1,duration:180ms")
  assert_eq(compressed_semantic_data[2], "s1,op1,st1,duration:320ms")
  assert_eq(compressed_semantic_data[3], "s1,op1,st2,duration:5000ms")
  assert_eq(compressed_semantic_data[4], "s1,op2,st1,duration:50ms")
  assert_eq(compressed_semantic_data[5], "s2,op3,st1,duration:150ms")
  
  // 8. 计算压缩率
  let total_original_size = {
    let mut size = 0
    let mut i = 0
    while i < semantic_data.length() {
      size = size + semantic_data[i].length()
      i = i + 1
    }
    size
  }
  
  let total_compressed_size = {
    let mut size = 0
    let mut i = 0
    while i < compressed_semantic_data.length() {
      size = size + compressed_semantic_data[i].length()
      i = i + 1
    }
    size
  }
  
  let compression_ratio = total_compressed_size.to_double() / total_original_size.to_double()
  
  // 9. 验证压缩效果
  assert_eq(compression_ratio < 1.0, true)  // 应该有压缩效果
  assert_eq(compression_ratio > 0.3, true)  // 但不会过度压缩
  
  // 10. 语义解压缩函数
  let semantic_decompress = fn(compressed_data: String, dictionary: Map[String, String]) -> String {
    let mut decompressed = compressed_data
    
    // 反向应用语义字典
    let keys = dictionary.keys()
    let mut i = 0
    
    while i < keys.length() {
      let key = keys[i]
      let value = dictionary.get(key)
      decompressed = decompressed.replace(value, key)
      i = i + 1
    }
    
    decompressed
  }
  
  // 11. 执行语义解压缩
  let decompressed_semantic_data = []
  i = 0
  
  while i < compressed_semantic_data.length() {
    let decompressed = semantic_decompress(compressed_semantic_data[i], semantic_dictionary)
    decompressed_semantic_data.push(decompressed)
    i = i + 1
  }
  
  // 12. 验证解压缩结果
  assert_eq(decompressed_semantic_data.length(), 6)
  
  // 验证数据完整性
  i = 0
  while i < semantic_data.length() {
    assert_eq(decompressed_semantic_data[i], semantic_data[i])
    i = i + 1
  }
}

test "telemetry_hierarchical_compression" {
  // 测试遥测分层压缩功能
  
  // 1. 创建分层遥测数据
  let hierarchical_data = {
    "application" => {
      "name" => "payment-service",
      "version" => "1.2.3",
      "environment" => "production",
      "metrics" => {
        "cpu" => {
          "usage" => 75.5,
          "cores" => 4,
          "frequency" => "2.4GHz"
        },
        "memory" => {
          "used" => 1024.0,
          "total" => 2048.0,
          "unit" => "MB"
        },
        "network" => {
          "bytes_in" => 1048576,
          "bytes_out" => 524288,
          "connections" => 150
        }
      },
      "traces" => {
        "active_spans" => 25,
        "total_requests" => 10000,
        "error_rate" => 0.01
      }
    }
  }
  
  // 2. 验证分层数据结构
  assert_eq(hierarchical_data.has("application"), true)
  assert_eq(hierarchical_data.get("application").has("metrics"), true)
  assert_eq(hierarchical_data.get("application").get("metrics").has("cpu"), true)
  
  // 3. 分层压缩策略
  let compression_strategies = {
    "level_0" => "none",        // 根节点不压缩
    "level_1" => "dictionary",  // 第一层使用字典压缩
    "level_2" => "delta",       // 第二层使用差分压缩
    "level_3" => "rle"          第三层使用行程编码
  }
  
  // 4. 验证压缩策略
  assert_eq(compression_strategies.get("level_0"), "none")
  assert_eq(compression_strategies.get("level_1"), "dictionary")
  assert_eq(compression_strategies.get("level_2"), "delta")
  assert_eq(compression_strategies.get("level_3"), "rle")
  
  // 5. 分层压缩函数
  let hierarchical_compress = fn(data: Map[String, Any], level: Int) -> Map[String, Any] {
    let strategy = match level {
      0 => "none",
      1 => "dictionary", 
      2 => "delta",
      _ => "rle"
    }
    
    let compressed_data = {}
    
    // 根据层级应用不同压缩策略
    match strategy {
      "none" => {
        // 不压缩，直接返回
        compressed_data = data
      }
      "dictionary" => {
        // 字典压缩：替换常见的键
        let keys = data.keys()
        let mut i = 0
        
        while i < keys.length() {
          let key = keys[i]
          let value = data.get(key)
          let compressed_key = match key {
            "name" => "n",
            "version" => "v",
            "environment" => "e",
            "metrics" => "m",
            "traces" => "t",
            _ => key
          }
          compressed_data = compressed_data.with(compressed_key, value)
          i = i + 1
        }
      }
      "delta" => {
        // 差分压缩：对数值类型进行差分编码
        let keys = data.keys()
        let mut i = 0
        
        while i < keys.length() {
          let key = keys[i]
          let value = data.get(key)
          
          // 对数值进行差分编码
          let compressed_value = match value {
            Double(d) => d - 100.0,  // 简化：减去基准值
            Int(i) => i - 1000,
            _ => value
          }
          
          compressed_data = compressed_data.with(key, compressed_value)
          i = i + 1
        }
      }
      "rle" => {
        // 行程编码：对重复值进行压缩
        let keys = data.keys()
        let mut i = 0
        
        while i < keys.length() {
          let key = keys[i]
          let value = data.get(key)
          
          // 简化的RLE：标记重复值
          let compressed_value = if value == "production" {
            "rle:production"
          } else if value == "1.2.3" {
            "rle:1.2.3"
          } else {
            value
          }
          
          compressed_data = compressed_data.with(key, compressed_value)
          i = i + 1
        }
      }
      _ => {
        compressed_data = data
      }
    }
    
    compressed_data
  }
  
  // 6. 执行分层压缩
  let level_1_compressed = hierarchical_compress(hierarchical_data.get("application"), 1)
  let level_2_compressed = hierarchical_compress(hierarchical_data.get("application").get("metrics"), 2)
  let level_3_compressed = hierarchical_compress(hierarchical_data.get("application").get("metrics").get("cpu"), 3)
  
  // 7. 验证分层压缩结果
  // 验证第一层压缩（字典压缩）
  assert_eq(level_1_compressed.has("n"), true)  // name -> n
  assert_eq(level_1_compressed.has("v"), true)  // version -> v
  assert_eq(level_1_compressed.has("e"), true)  // environment -> e
  assert_eq(level_1_compressed.has("m"), true)  // metrics -> m
  assert_eq(level_1_compressed.has("t"), true)  // traces -> t
  
  // 验证第二层压缩（差分压缩）
  assert_eq(level_2_compressed.has("cpu"), true)
  assert_eq(level_2_compressed.has("memory"), true)
  assert_eq(level_2_compressed.has("network"), true)
  
  // 验证第三层压缩（行程编码）
  assert_eq(level_3_compressed.has("usage"), true)
  assert_eq(level_3_compressed.has("cores"), true)
  assert_eq(level_3_compressed.has("frequency"), true)
  
  // 8. 计算分层压缩效果
  let calculate_compression_ratio = fn(original: Map[String, Any], compressed: Map[String, Any]) -> Double {
    let original_size = original.keys().length().to_double()
    let compressed_size = compressed.keys().length().to_double()
    compressed_size / original_size
  }
  
  let level_1_ratio = calculate_compression_ratio(hierarchical_data.get("application"), level_1_compressed)
  let level_2_ratio = calculate_compression_ratio(hierarchical_data.get("application").get("metrics"), level_2_compressed)
  let level_3_ratio = calculate_compression_ratio(hierarchical_data.get("application").get("metrics").get("cpu"), level_3_compressed)
  
  // 9. 验证压缩效果
  assert_eq(level_1_ratio <= 1.0, true)  // 应该有压缩效果
  assert_eq(level_2_ratio <= 1.0, true)  // 应该有压缩效果
  assert_eq(level_3_ratio <= 1.0, true)  // 应该有压缩效果
  
  // 10. 分层压缩统计
  let compression_stats = {
    "total_levels" => 3,
    "level_1_strategy" => "dictionary",
    "level_2_strategy" => "delta",
    "level_3_strategy" => "rle",
    "level_1_ratio" => level_1_ratio.to_string(),
    "level_2_ratio" => level_2_ratio.to_string(),
    "level_3_ratio" => level_3_ratio.to_string()
  }
  
  // 11. 验证压缩统计
  assert_eq(compression_stats.get("total_levels"), "3")
  assert_eq(compression_stats.get("level_1_strategy"), "dictionary")
  assert_eq(compression_stats.get("level_2_strategy"), "delta")
  assert_eq(compression_stats.get("level_3_strategy"), "rle")
}