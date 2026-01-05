// Azimuth 遥测数据压缩和传输测试
// 测试遥测数据的各种压缩算法和传输协议

// 测试1: 基本数据压缩算法
test "基本数据压缩算法" {
  // 定义压缩结果类型
  type CompressionResult = {
    compressed_data: Array[UInt8],
    original_size: Int,
    compressed_size: Int,
    compression_ratio: Float,
    algorithm: String
  }
  
  // 创建测试数据
  let create_test_data = fn(size: Int, pattern: String) {
    let mut data = []
    
    match pattern {
      "repetitive" => {
        // 重复模式数据
        let pattern_str = "telemetry_data_"
        for i in 0..size {
          let char = pattern_str[i % pattern_str.length()]
          data = data + [char as UInt8]
        }
      }
      "random" => {
        // 随机数据
        for i in 0..size {
          data = data + [(i % 256) as UInt8]
        }
      }
      "structured" => {
        // 结构化数据（JSON-like）
        let json_template = "{\"trace_id\":\"trace-12345\",\"span_id\":\"span-67890\",\"timestamp\":1640995200000,\"attributes\":{\"service.name\":\"payment-service\",\"environment\":\"production\"}}"
        for i in 0..size {
          let char = json_template[i % json_template.length()]
          data = data + [char as UInt8]
        }
      }
      _ => {
        // 默认：简单的重复数据
        for i in 0..size {
          data = data + [(i % 10 + 65) as UInt8]  // A-J
        }
      }
    }
    
    data
  }
  
  // 行程编码压缩
  let run_length_encode = fn(data: Array[UInt8]) {
    if data.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut current_byte = data[0]
    let mut count = 1
    
    for i in 1..data.length() {
      if data[i] == current_byte && count < 255 {
        count = count + 1
      } else {
        result = result + [count, current_byte]
        current_byte = data[i]
        count = 1
      }
    }
    
    // 添加最后一个序列
    result = result + [count, current_byte]
    result
  }
  
  // 行程编码解压
  let run_length_decode = fn(compressed: Array[UInt8]) {
    let mut result = []
    
    for i in 0..compressed.length() / 2 {
      let count = compressed[i * 2] as Int
      let byte = compressed[i * 2 + 1]
      
      for j in 0..count {
        result = result + [byte]
      }
    }
    
    result
  }
  
  // 简单字典压缩
  let dictionary_compress = fn(data: Array[UInt8], dict_size: Int) {
    if data.length() == 0 {
      return {compressed_data: [], dictionary: []}
    }
    
    let mut dictionary = []
    let mut result = []
    let mut next_dict_id = 0
    
    // 构建字典
    let mut i = 0
    while i < data.length() && dictionary.length() < dict_size {
      let byte = data[i]
      
      if not(dictionary.contains(byte)) {
        dictionary = dictionary + [byte]
        next_dict_id = next_dict_id + 1
      }
      
      i = i + 1
    }
    
    // 压缩数据
    for byte in data {
      match dictionary.index_of(byte) {
        Some(index) => {
          result = result + [index as UInt8]
        }
        None => {
          // 字典外的字节用转义序列表示
          result = result + [255]  // 转义标记
          result = result + [byte]
        }
      }
    }
    
    {compressed_data: result, dictionary: dictionary}
  }
  
  // 简单字典解压
  let dictionary_decompress = fn(compressed: {compressed_data: Array[UInt8], dictionary: Array[UInt8]}) {
    let mut result = []
    let i = 0
    
    while i < compressed.compressed_data.length() {
      let byte = compressed.compressed_data[i]
      
      if byte == 255 && i + 1 < compressed.compressed_data.length() {
        // 转义序列
        result = result + [compressed.compressed_data[i + 1]]
        i = i + 2
      } else if byte < compressed.dictionary.length() as UInt8 {
        // 字典索引
        result = result + [compressed.dictionary[byte as Int]]
        i = i + 1
      } else {
        // 无效索引，跳过
        i = i + 1
      }
    }
    
    result
  }
  
  // 测试行程编码
  let repetitive_data = create_test_data(1000, "repetitive")
  let rle_compressed = run_length_encode(repetitive_data)
  let rle_decompressed = run_length_decode(rle_compressed)
  
  // 验证压缩和解压
  assert_eq(rle_decompressed, repetitive_data)
  assert_true(rle_compressed.length() < repetitive_data.length())
  
  let rle_ratio = rle_compressed.length() as Float / repetitive_data.length() as Float
  assert_true(rle_ratio < 0.5)  // 重复数据应该有很好的压缩比
  
  // 测试字典压缩
  let structured_data = create_test_data(1000, "structured")
  let dict_compressed = dictionary_compress(structured_data, 50)
  let dict_decompressed = dictionary_decompress(dict_compressed)
  
  // 验证压缩和解压
  assert_eq(dict_decompressed, structured_data)
  assert_true(dict_compressed.compressed_data.length() < structured_data.length())
  
  let dict_ratio = dict_compressed.compressed_data.length() as Float / structured_data.length() as Float
  assert_true(dict_ratio < 0.9)  // 结构化数据应该有一定的压缩比
  
  // 测试随机数据压缩（压缩效果应该较差）
  let random_data = create_test_data(1000, "random")
  let random_rle_compressed = run_length_encode(random_data)
  let random_rle_ratio = random_rle_compressed.length() as Float / random_data.length() as Float
  
  // 随机数据的压缩比应该接近1或大于1（压缩效果差）
  assert_true(random_rle_ratio >= 0.8)
}

// 测试2: 高级压缩算法
test "高级压缩算法" {
  // 定义压缩结果类型
  type CompressionResult = {
    compressed_data: Array[UInt8],
    original_size: Int,
    compressed_size: Int,
    compression_ratio: Float,
    algorithm: String,
    compression_time: Int,
    decompression_time: Int
  }
  
  // 创建测试数据
  let create_telemetry_data = fn(record_count: Int) {
    let mut data = ""
    
    for i in 0..record_count {
      let record = "{" +
      "\"trace_id\":\"trace-" + i.to_string() + "\"," +
      "\"span_id\":\"span-" + i.to_string() + "\"," +
      "\"timestamp\":" + (1640995200000 + i * 1000).to_string() + "," +
      "\"service_name\":\"service-" + (i % 10).to_string() + "\"," +
      "\"operation\":\"operation-" + (i % 20).to_string() + "\"," +
      "\"duration\":" + (50 + (i % 500)).to_string() + "," +
      "\"status\":\"" + if i % 20 == 0 { "error" } else { "ok" } + "\"" +
      "}"
      
      data = data + record + "\n"
    }
    
    data.to_bytes()
  }
  
  // LZ77风格压缩算法（简化版）
  let lz77_compress = fn(data: Array[UInt8], window_size: Int, min_match: Int) {
    if data.length() < min_match {
      return {compressed_data: data, positions: []}
    }
    
    let mut result = []
    let mut positions = []
    let mut i = 0
    
    while i < data.length() {
      let mut best_match = {length: 0, distance: 0}
      
      // 在滑动窗口中查找最长匹配
      let window_start = if i >= window_size { i - window_size } else { 0 }
      
      for j in window_start..i {
        let mut match_length = 0
        while i + match_length < data.length() and 
              j + match_length < i and 
              data[i + match_length] == data[j + match_length] {
          match_length = match_length + 1
        }
        
        if match_length > best_match.length and match_length >= min_match {
          best_match = {length: match_length, distance: i - j}
        }
      }
      
      if best_match.length >= min_match {
        // 输出匹配信息
        result = result + [255]  // 匹配标记
        result = result + [best_match.length as UInt8]
        result = result + [(best_match.distance % 256) as UInt8]
        result = result + [(best_match.distance / 256) as UInt8]
        
        positions = positions + {position: i, type: "match", length: best_match.length, distance: best_match.distance}
        i = i + best_match.length
      } else {
        // 输出字面量
        result = result + [data[i]]
        positions = positions + {position: i, type: "literal", value: data[i]}
        i = i + 1
      }
    }
    
    {compressed_data: result, positions: positions}
  }
  
  // LZ77风格解压算法
  let lz77_decompress = fn(compressed: {compressed_data: Array[UInt8], positions: Array[{position: Int, type: String, length: Int, distance: Int, value: UInt8}]}) {
    let mut result = []
    let mut i = 0
    
    while i < compressed.compressed_data.length() {
      let byte = compressed.compressed_data[i]
      
      if byte == 255 && i + 3 < compressed.compressed_data.length() {
        // 匹配信息
        let length = compressed.compressed_data[i + 1] as Int
        let distance = compressed.compressed_data[i + 2] as Int + (compressed.compressed_data[i + 3] as Int) * 256
        
        for j in 0..length {
          let source_index = result.length() - distance
          if source_index >= 0 && source_index < result.length() {
            result = result + [result[source_index]]
          }
        }
        
        i = i + 4
      } else {
        // 字面量
        result = result + [byte]
        i = i + 1
      }
    }
    
    result
  }
  
  // 哈夫曼压缩（简化版）
  let huffman_compress = fn(data: Array[UInt8]) {
    if data.length() == 0 {
      return {compressed_data: [], tree: []}
    }
    
    // 统计频率
    let mut frequencies = Map::empty()
    for byte in data {
      let count = match Map::get(frequencies, byte) {
        Some(c) => c + 1
        None => 1
      }
      let _ = Map::insert(frequencies, byte, count)
    }
    
    // 构建优先队列（简化版，直接排序）
    let mut nodes = []
    for (byte, freq) in frequencies {
      nodes = nodes + {value: byte, frequency: freq, left: None, right: None}
    }
    
    // 按频率排序
    nodes = nodes.sort(fn(a, b) { if a.frequency < b.frequency { -1 } else if a.frequency > b.frequency { 1 } else { 0 } })
    
    // 构建哈夫曼树（简化版）
    while nodes.length() > 1 {
      let left = nodes[0]
      let right = nodes[1]
      
      let new_node = {
        value: 0,  // 内部节点
        frequency: left.frequency + right.frequency,
        left: Some(left),
        right: Some(right)
      }
      
      nodes = nodes.slice(2)  // 移除前两个节点
      nodes = nodes + [new_node]  // 添加新节点
      
      // 重新排序
      nodes = nodes.sort(fn(a, b) { if a.frequency < b.frequency { -1 } else if a.frequency > b.frequency { 1 } else { 0 } })
    }
    
    // 生成编码表（简化版，实际应该遍历树）
    let mut codes = Map::empty()
    for i in 0..256 {
      let byte = i as UInt8
      let code = i.to_string(2).pad_left(8, '0')  // 简化：直接使用8位二进制
      let _ = Map::insert(codes, byte, code)
    }
    
    // 压缩数据（简化版，直接拼接编码）
    let mut bit_string = ""
    for byte in data {
      match Map::get(codes, byte) {
        Some(code) => bit_string = bit_string + code
        None => {}
      }
    }
    
    // 将位字符串转换为字节数组
    let mut compressed = []
    for i in 0..bit_string.length() / 8 {
      let byte_str = bit_string.substring(i * 8, 8)
      let byte = Int::from_str_radix(byte_str, 2).get_or(0)
      compressed = compressed + [byte as UInt8]
    }
    
    {compressed_data: compressed, tree: nodes}
  }
  
  // 哈夫曼解压（简化版）
  let huffman_decompress = fn(compressed: {compressed_data: Array[UInt8], tree: Array[{value: UInt8, frequency: Int, left: Option, right: Option}]}) {
    // 简化版：直接使用8位二进制转换
    let mut result = []
    
    for byte in compressed.compressed_data {
      result = result + [byte]
    }
    
    result
  }
  
  // 测试数据
  let telemetry_data = create_telemetry_data(100)
  
  // 测试LZ77压缩
  let start_time = Time::now()
  let lz77_result = lz77_compress(telemetry_data, 100, 3)
  let lz77_compression_time = Time::now() - start_time
  
  let start_time = Time::now()
  let lz77_decompressed = lz77_decompress(lz77_result)
  let lz77_decompression_time = Time::now() - start_time
  
  // 验证LZ77压缩和解压
  assert_eq(lz77_decompressed, telemetry_data)
  
  let lz77_ratio = lz77_result.compressed_data.length() as Float / telemetry_data.length() as Float
  assert_true(lz77_ratio < 0.9)  // 应该有一定的压缩效果
  
  // 测试哈夫曼压缩
  let start_time = Time::now()
  let huffman_result = huffman_compress(telemetry_data)
  let huffman_compression_time = Time::now() - start_time
  
  let start_time = Time::now()
  let huffman_decompressed = huffman_decompress(huffman_result)
  let huffman_decompression_time = Time::now() - start_time
  
  // 验证哈夫曼压缩和解压
  assert_eq(huffman_decompressed, telemetry_data)
  
  let huffman_ratio = huffman_result.compressed_data.length() as Float / telemetry_data.length() as Float
  assert_true(huffman_ratio <= 1.0)  // 压缩后大小应该小于或等于原始大小
  
  // 比较压缩效果
  assert_true(lz77_compression_time > 0)
  assert_true(lz77_decompression_time > 0)
  assert_true(huffman_compression_time > 0)
  assert_true(huffman_decompression_time > 0)
}

// 测试3: 分块压缩
test "分块压缩" {
  // 定义分块压缩结果
  type ChunkedCompressionResult = {
    chunks: Array[{
      chunk_id: Int,
      compressed_data: Array[UInt8],
      original_size: Int,
      compressed_size: Int,
      compression_ratio: Float
    }],
    total_original_size: Int,
    total_compressed_size: Int,
    overall_compression_ratio: Float
  }
  
  // 创建大型测试数据
  let create_large_data = fn(size: Int) {
    let mut data = []
    
    for i in 0..size {
      // 创建有一定重复模式的数据
      let pattern = if i % 100 < 50 {
        // 重复模式
        (i % 10 + 65) as UInt8  // A-J
      } else {
        // 随机模式
        (i % 256) as UInt8
      }
      
      data = data + [pattern]
    }
    
    data
  }
  
  // 简单压缩函数
  let simple_compress = fn(chunk: Array[UInt8]) {
    // 使用简化的行程编码
    if chunk.length() == 0 {
      return []
    }
    
    let mut result = []
    let mut current_byte = chunk[0]
    let mut count = 1
    
    for i in 1..chunk.length() {
      if chunk[i] == current_byte && count < 255 {
        count = count + 1
      } else {
        result = result + [count, current_byte]
        current_byte = chunk[i]
        count = 1
      }
    }
    
    // 添加最后一个序列
    result = result + [count, current_byte]
    result
  }
  
  // 分块压缩函数
  let chunked_compress = fn(data: Array[UInt8], chunk_size: Int) {
    let mut chunks = []
    let mut total_original = 0
    let mut total_compressed = 0
    
    for i in 0..(data.length() / chunk_size + 1) {
      let start = i * chunk_size
      let end = if start + chunk_size < data.length() { start + chunk_size } else { data.length() }
      
      if start < data.length() {
        let chunk = data.slice(start, end - start)
        let compressed_chunk = simple_compress(chunk)
        
        let chunk_result = {
          chunk_id: i,
          compressed_data: compressed_chunk,
          original_size: chunk.length(),
          compressed_size: compressed_chunk.length(),
          compression_ratio: compressed_chunk.length() as Float / chunk.length() as Float
        }
        
        chunks = chunks + [chunk_result]
        total_original = total_original + chunk.length()
        total_compressed = total_compressed + compressed_chunk.length()
      }
    }
    
    {
      chunks: chunks,
      total_original_size: total_original,
      total_compressed_size: total_compressed,
      overall_compression_ratio: total_compressed as Float / total_original as Float
    }
  }
  
  // 分块解压函数
  let chunked_decompress = fn(compressed: ChunkedCompressionResult) {
    let mut result = []
    
    // 按chunk_id排序
    let sorted_chunks = compressed.chunks.sort(fn(a, b) { if a.chunk_id < b.chunk_id { -1 } else if a.chunk_id > b.chunk_id { 1 } else { 0 } })
    
    for chunk in sorted_chunks {
      // 解压单个块
      let mut decompressed_chunk = []
      let i = 0
      
      while i < chunk.compressed_data.length() / 2 {
        let count = chunk.compressed_data[i * 2] as Int
        let byte = chunk.compressed_data[i * 2 + 1]
        
        for j in 0..count {
          decompressed_chunk = decompressed_chunk + [byte]
        }
        
        i = i + 1
      }
      
      result = result + decompressed_chunk
    }
    
    result
  }
  
  // 创建测试数据
  let large_data = create_large_data(10000)
  
  // 测试不同的分块大小
  let small_chunks = chunked_compress(large_data, 100)
  let medium_chunks = chunked_compress(large_data, 1000)
  let large_chunks = chunked_compress(large_data, 5000)
  
  // 验证分块压缩
  assert_eq(small_chunks.total_original_size, large_data.length())
  assert_eq(medium_chunks.total_original_size, large_data.length())
  assert_eq(large_chunks.total_original_size, large_data.length())
  
  assert_eq(small_chunks.chunks.length(), 100)
  assert_eq(medium_chunks.chunks.length(), 10)
  assert_eq(large_chunks.chunks.length(), 2)
  
  // 验证压缩效果
  assert_true(small_chunks.overall_compression_ratio < 1.0)
  assert_true(medium_chunks.overall_compression_ratio < 1.0)
  assert_true(large_chunks.overall_compression_ratio < 1.0)
  
  // 测试分块解压
  let decompressed_small = chunked_decompress(small_chunks)
  let decompressed_medium = chunked_decompress(medium_chunks)
  let decompressed_large = chunked_decompress(large_chunks)
  
  // 验证解压结果
  assert_eq(decompressed_small, large_data)
  assert_eq(decompressed_medium, large_data)
  assert_eq(decompressed_large, large_data)
  
  // 比较不同分块大小的压缩效果
  // 通常中等大小的块会有更好的压缩效果
  assert_true(medium_chunks.overall_compression_ratio <= large_chunks.overall_compression_ratio)
}

// 测试4: 自适应压缩
test "自适应压缩" {
  // 定义数据特征类型
  type DataCharacteristics = {
    entropy: Float,
    repetition_ratio: Float,
    pattern_frequency: Float,
    size: Int
  }
  
  // 定义压缩策略
  enum CompressionStrategy {
    RunLengthEncoding
    DictionaryCompression
    Lz77
    Huffman
    Hybrid
    None
  }
  
  // 分析数据特征
  let analyze_data = fn(data: Array[UInt8]) {
    if data.length() == 0 {
      return {
        entropy: 0.0,
        repetition_ratio: 0.0,
        pattern_frequency: 0.0,
        size: 0
      }
    }
    
    // 计算熵
    let mut frequencies = Map::empty()
    for byte in data {
      let count = match Map::get(frequencies, byte) {
        Some(c) => c + 1
        None => 1
      }
      let _ = Map::insert(frequencies, byte, count)
    }
    
    let mut entropy = 0.0
    for (byte, count) in frequencies {
      let probability = count as Float / data.length() as Float
      entropy = entropy - probability * probability.log2()
    }
    
    // 计算重复率
    let mut repetitions = 0
    for i in 1..data.length() {
      if data[i] == data[i - 1] {
        repetitions = repetitions + 1
      }
    }
    let repetition_ratio = repetitions as Float / (data.length() - 1) as Float
    
    // 计算模式频率（简化版）
    let mut patterns = Map::empty()
    let pattern_length = 4
    
    for i in 0..(data.length() - pattern_length + 1) {
      let mut pattern = ""
      for j in 0..pattern_length {
        pattern = pattern + (data[i + j] as Char).to_string()
      }
      
      let count = match Map::get(patterns, pattern) {
        Some(c) => c + 1
        None => 1
      }
      let _ = Map::insert(patterns, pattern, count)
    }
    
    let mut pattern_frequency = 0.0
    for (pattern, count) in patterns {
      if count > 1 {
        pattern_frequency = pattern_frequency + count as Float
      }
    }
    pattern_frequency = pattern_frequency / (data.length() - pattern_length + 1) as Float
    
    {
      entropy: entropy,
      repetition_ratio: repetition_ratio,
      pattern_frequency: pattern_frequency,
      size: data.length()
    }
  }
  
  // 选择压缩策略
  let select_strategy = fn(characteristics: DataCharacteristics) {
    if characteristics.size < 50 {
      CompressionStrategy::None  // 小数据不压缩
    } else if characteristics.repetition_ratio > 0.7 {
      CompressionStrategy::RunLengthEncoding  // 高重复率使用RLE
    } else if characteristics.entropy < 4.0 {
      CompressionStrategy::DictionaryCompression  // 低熵使用字典压缩
    } else if characteristics.pattern_frequency > 0.3 {
      CompressionStrategy::Lz77  // 高模式频率使用LZ77
    } else if characteristics.entropy > 6.0 {
      CompressionStrategy::Huffman  // 高熵使用哈夫曼
    } else {
      CompressionStrategy::Hybrid  // 中等特征使用混合策略
    }
  }
  
  // 执行压缩
  let compress_with_strategy = fn(data: Array[UInt8], strategy: CompressionStrategy) {
    match strategy {
      CompressionStrategy::None => {
        {compressed_data: data, strategy: "None"}
      }
      CompressionStrategy::RunLengthEncoding => {
        // 简化的RLE实现
        if data.length() == 0 {
          return {compressed_data: [], strategy: "RunLengthEncoding"}
        }
        
        let mut result = []
        let mut current_byte = data[0]
        let mut count = 1
        
        for i in 1..data.length() {
          if data[i] == current_byte && count < 255 {
            count = count + 1
          } else {
            result = result + [count, current_byte]
            current_byte = data[i]
            count = 1
          }
        }
        
        result = result + [count, current_byte]
        {compressed_data: result, strategy: "RunLengthEncoding"}
      }
      CompressionStrategy::DictionaryCompression => {
        // 简化的字典压缩
        let mut dictionary = []
        let mut result = []
        let dict_size = 64
        
        // 构建字典
        let mut i = 0
        while i < data.length() && dictionary.length() < dict_size {
          let byte = data[i]
          
          if not(dictionary.contains(byte)) {
            dictionary = dictionary + [byte]
          }
          
          i = i + 1
        }
        
        // 压缩数据
        for byte in data {
          match dictionary.index_of(byte) {
            Some(index) => {
              result = result + [index as UInt8]
            }
            None => {
              result = result + [255]  // 转义标记
              result = result + [byte]
            }
          }
        }
        
        {compressed_data: result, strategy: "DictionaryCompression"}
      }
      CompressionStrategy::Lz77 => {
        // 简化的LZ77实现
        if data.length() < 3 {
          return {compressed_data: data, strategy: "Lz77"}
        }
        
        let mut result = []
        let mut i = 0
        let window_size = 100
        let min_match = 3
        
        while i < data.length() {
          let mut best_match = {length: 0, distance: 0}
          
          // 在滑动窗口中查找最长匹配
          let window_start = if i >= window_size { i - window_size } else { 0 }
          
          for j in window_start..i {
            let mut match_length = 0
            while i + match_length < data.length() && 
                  j + match_length < i && 
                  data[i + match_length] == data[j + match_length] {
              match_length = match_length + 1
            }
            
            if match_length > best_match.length && match_length >= min_match {
              best_match = {length: match_length, distance: i - j}
            }
          }
          
          if best_match.length >= min_match {
            // 输出匹配信息
            result = result + [255]  // 匹配标记
            result = result + [best_match.length as UInt8]
            result = result + [(best_match.distance % 256) as UInt8]
            result = result + [(best_match.distance / 256) as UInt8]
            i = i + best_match.length
          } else {
            // 输出字面量
            result = result + [data[i]]
            i = i + 1
          }
        }
        
        {compressed_data: result, strategy: "Lz77"}
      }
      CompressionStrategy::Huffman => {
        // 简化的哈夫曼实现
        let mut bit_string = ""
        
        for byte in data {
          let code = byte.to_string(2).pad_left(8, '0')
          bit_string = bit_string + code
        }
        
        // 将位字符串转换为字节数组
        let mut compressed = []
        for i in 0..bit_string.length() / 8 {
          let byte_str = bit_string.substring(i * 8, 8)
          let byte = Int::from_str_radix(byte_str, 2).get_or(0)
          compressed = compressed + [byte as UInt8]
        }
        
        {compressed_data: compressed, strategy: "Huffman"}
      }
      CompressionStrategy::Hybrid => {
        // 简化的混合策略：先RLE再字典压缩
        let rle_result = compress_with_strategy(data, CompressionStrategy::RunLengthEncoding)
        compress_with_strategy(rle_result.compressed_data, CompressionStrategy::DictionaryCompression)
      }
    }
  }
  
  // 创建不同特征的测试数据
  let repetitive_data = {
    let mut data = []
    for i in 0..1000 {
      data = data + [(i % 10 + 65) as UInt8]  // A-J重复
    }
    data
  }
  
  let random_data = {
    let mut data = []
    for i in 0..1000 {
      data = data + [(i * 17 % 256) as UInt8]  // 伪随机
    }
    data
  }
  
  let structured_data = {
    let mut data = []
    let pattern = "telemetry_data_with_structured_content"
    for i in 0..1000 {
      let char = pattern[i % pattern.length()]
      data = data + [char as UInt8]
    }
    data
  }
  
  // 测试自适应压缩
  let test_adaptive_compression = fn(data: Array[UInt8]) {
    let characteristics = analyze_data(data)
    let strategy = select_strategy(characteristics)
    let result = compress_with_strategy(data, strategy)
    
    {
      characteristics: characteristics,
      strategy: strategy,
      result: result,
      compression_ratio: result.compressed_data.length() as Float / data.length() as Float
    }
  }
  
  // 测试不同类型的数据
  let repetitive_result = test_adaptive_compression(repetitive_data)
  let random_result = test_adaptive_compression(random_data)
  let structured_result = test_adaptive_compression(structured_data)
  
  // 验证策略选择
  assert_eq(repetitive_result.strategy, CompressionStrategy::RunLengthEncoding)
  
  // 验证压缩效果
  assert_true(repetitive_result.compression_ratio < 0.5)  // 重复数据应该有很好的压缩效果
  assert_true(random_result.compression_ratio >= 0.8)  // 随机数据压缩效果差
  assert_true(structured_result.compression_ratio < 0.9)  // 结构化数据有一定压缩效果
  
  // 验证数据特征
  assert_true(repetitive_result.characteristics.repetition_ratio > 0.7)
  assert_true(repetitive_result.characteristics.entropy < 4.0)
}

// 测试5: 传输协议封装
test "传输协议封装" {
  // 定义传输协议类型
  enum TransportProtocol {
    HTTP
    gRPC
    WebSocket
    TCP
    UDP
    AMQP
  }
  
  // 定义传输包结构
  type TransportPacket = {
    protocol: TransportProtocol,
    headers: Array[(String, String)],
    payload: Array[UInt8],
    metadata: {
      packet_id: String,
      timestamp: Int,
      source: String,
      destination: String,
      compression: String
    }
  }
  
  // 创建HTTP传输包
  let create_http_packet = fn(payload: Array[UInt8], compression: String) {
    let headers = [
      ("Content-Type", "application/octet-stream"),
      ("Content-Encoding", compression),
      ("Content-Length", payload.length().to_string()),
      ("X-Packet-ID", "packet-" + Time::now().to_string()),
      ("X-Timestamp", Time::now().to_string()),
      ("X-Source", "azimuth-collector"),
      ("X-Destination", "azimuth-processor")
    ]
    
    {
      protocol: TransportProtocol::HTTP,
      headers: headers,
      payload: payload,
      metadata: {
        packet_id: "packet-" + Time::now().to_string(),
        timestamp: Time::now(),
        source: "azimuth-collector",
        destination: "azimuth-processor",
        compression: compression
      }
    }
  }
  
  // 创建gRPC传输包
  let create_grpc_packet = fn(payload: Array[UInt8], compression: String) {
    let headers = [
      ("content-type", "application/grpc"),
      ("grpc-encoding", compression),
      ("grpc-message-type", "telemetry.TelemetryData"),
      ("packet-id", "packet-" + Time::now().to_string()),
      ("timestamp", Time::now().to_string()),
      ("source", "azimuth-collector"),
      ("destination", "azimuth-processor")
    ]
    
    {
      protocol: TransportProtocol::gRPC,
      headers: headers,
      payload: payload,
      metadata: {
        packet_id: "packet-" + Time::now().to_string(),
        timestamp: Time::now(),
        source: "azimuth-collector",
        destination: "azimuth-processor",
        compression: compression
      }
    }
  }
  
  // 创建WebSocket传输包
  let create_websocket_packet = fn(payload: Array[UInt8], compression: String) {
    let headers = [
      ("Sec-WebSocket-Protocol", "telemetry-v1"),
      ("X-Compression", compression),
      ("X-Packet-ID", "packet-" + Time::now().to_string()),
      ("X-Timestamp", Time::now().to_string()),
      ("X-Source", "azimuth-collector"),
      ("X-Destination", "azimuth-processor")
    ]
    
    {
      protocol: TransportProtocol::WebSocket,
      headers: headers,
      payload: payload,
      metadata: {
        packet_id: "packet-" + Time::now().to_string(),
        timestamp: Time::now(),
        source: "azimuth-collector",
        destination: "azimuth-processor",
        compression: compression
      }
    }
  }
  
  // 创建TCP传输包
  let create_tcp_packet = fn(payload: Array[UInt8], compression: String) {
    let headers = [
      ("X-Protocol", "telemetry-tcp"),
      ("X-Compression", compression),
      ("X-Packet-ID", "packet-" + Time::now().to_string()),
      ("X-Timestamp", Time::now().to_string()),
      ("X-Source", "azimuth-collector"),
      ("X-Destination", "azimuth-processor")
    ]
    
    {
      protocol: TransportProtocol::TCP,
      headers: headers,
      payload: payload,
      metadata: {
        packet_id: "packet-" + Time::now().to_string(),
        timestamp: Time::now(),
        source: "azimuth-collector",
        destination: "azimuth-processor",
        compression: compression
      }
    }
  }
  
  // 创建UDP传输包
  let create_udp_packet = fn(payload: Array[UInt8], compression: String) {
    let headers = [
      ("X-Protocol", "telemetry-udp"),
      ("X-Compression", compression),
      ("X-Packet-ID", "packet-" + Time::now().to_string()),
      ("X-Timestamp", Time::now().to_string()),
      ("X-Source", "azimuth-collector"),
      ("X-Destination", "azimuth-processor")
    ]
    
    {
      protocol: TransportProtocol::UDP,
      headers: headers,
      payload: payload,
      metadata: {
        packet_id: "packet-" + Time::now().to_string(),
        timestamp: Time::now(),
        source: "azimuth-collector",
        destination: "azimuth-processor",
        compression: compression
      }
    }
  }
  
  // 创建AMQP传输包
  let create_amqp_packet = fn(payload: Array[UInt8], compression: String) {
    let headers = [
      ("content-type", "application/octet-stream"),
      ("content-encoding", compression),
      ("message-id", "packet-" + Time::now().to_string()),
      ("timestamp", Time::now().to_string()),
      ("source", "azimuth-collector"),
      ("destination", "azimuth-processor")
    ]
    
    {
      protocol: TransportProtocol::AMQP,
      headers: headers,
      payload: payload,
      metadata: {
        packet_id: "packet-" + Time::now().to_string(),
        timestamp: Time::now(),
        source: "azimuth-collector",
        destination: "azimuth-processor",
        compression: compression
      }
    }
  }
  
  // 创建测试载荷
  let test_payload = "telemetry_data_with_compression".to_bytes()
  
  // 测试不同协议的包创建
  let http_packet = create_http_packet(test_payload, "gzip")
  let grpc_packet = create_grpc_packet(test_payload, "gzip")
  let websocket_packet = create_websocket_packet(test_payload, "gzip")
  let tcp_packet = create_tcp_packet(test_payload, "gzip")
  let udp_packet = create_udp_packet(test_payload, "gzip")
  let amqp_packet = create_amqp_packet(test_payload, "gzip")
  
  // 验证HTTP包
  assert_eq(http_packet.protocol, TransportProtocol::HTTP)
  assert_true(http_packet.headers.contains(("Content-Type", "application/octet-stream")))
  assert_true(http_packet.headers.contains(("Content-Encoding", "gzip")))
  assert_eq(http_packet.payload, test_payload)
  assert_eq(http_packet.metadata.compression, "gzip")
  
  // 验证gRPC包
  assert_eq(grpc_packet.protocol, TransportProtocol::gRPC)
  assert_true(grpc_packet.headers.contains(("content-type", "application/grpc")))
  assert_true(grpc_packet.headers.contains(("grpc-encoding", "gzip")))
  assert_eq(grpc_packet.payload, test_payload)
  assert_eq(grpc_packet.metadata.compression, "gzip")
  
  // 验证WebSocket包
  assert_eq(websocket_packet.protocol, TransportProtocol::WebSocket)
  assert_true(websocket_packet.headers.contains(("Sec-WebSocket-Protocol", "telemetry-v1")))
  assert_true(websocket_packet.headers.contains(("X-Compression", "gzip")))
  assert_eq(websocket_packet.payload, test_payload)
  assert_eq(websocket_packet.metadata.compression, "gzip")
  
  // 验证TCP包
  assert_eq(tcp_packet.protocol, TransportProtocol::TCP)
  assert_true(tcp_packet.headers.contains(("X-Protocol", "telemetry-tcp")))
  assert_true(tcp_packet.headers.contains(("X-Compression", "gzip")))
  assert_eq(tcp_packet.payload, test_payload)
  assert_eq(tcp_packet.metadata.compression, "gzip")
  
  // 验证UDP包
  assert_eq(udp_packet.protocol, TransportProtocol::UDP)
  assert_true(udp_packet.headers.contains(("X-Protocol", "telemetry-udp")))
  assert_true(udp_packet.headers.contains(("X-Compression", "gzip")))
  assert_eq(udp_packet.payload, test_payload)
  assert_eq(udp_packet.metadata.compression, "gzip")
  
  // 验证AMQP包
  assert_eq(amqp_packet.protocol, TransportProtocol::AMQP)
  assert_true(amqp_packet.headers.contains(("content-type", "application/octet-stream")))
  assert_true(amqp_packet.headers.contains(("content-encoding", "gzip")))
  assert_eq(amqp_packet.payload, test_payload)
  assert_eq(amqp_packet.metadata.compression, "gzip")
  
  // 验证所有包的元数据
  let packets = [http_packet, grpc_packet, websocket_packet, tcp_packet, udp_packet, amqp_packet]
  
  for packet in packets {
    assert_eq(packet.metadata.source, "azimuth-collector")
    assert_eq(packet.metadata.destination, "azimuth-processor")
    assert_eq(packet.metadata.compression, "gzip")
    assert_true(packet.metadata.packet_id.starts_with("packet-"))
    assert_true(packet.metadata.timestamp > 0)
  }
}

// 测试6: 传输可靠性
test "传输可靠性" {
  // 定义传输结果
  type TransmissionResult = {
    success: Bool,
    attempts: Int,
    error: Option[String],
    latency: Int,
    packet_loss: Bool,
    corruption: Bool
  }
  
  // 定义重试策略
  type RetryStrategy = {
    max_attempts: Int,
    backoff_ms: Int,
    max_backoff_ms: Int,
    multiplier: Float
  }
  
  // 模拟网络传输
  let simulate_transmission = fn(packet: TransportPacket, strategy: RetryStrategy, failure_rate: Float) {
    let mut attempts = 0
    let mut success = false
    let mut error = None
    let mut total_latency = 0
    
    while attempts < strategy.max_attempts && not success {
      attempts = attempts + 1
      
      // 模拟网络延迟
      let latency = 50 + (attempts * 20)  // 延迟随尝试次数增加
      total_latency = total_latency + latency
      
      // 模拟传输失败
      let random = (Time::now() % 100) as Float / 100.0
      if random < failure_rate {
        error = Some("网络传输失败")
      } else {
        success = true
        error = None
      }
      
      // 如果失败且不是最后一次尝试，等待退避时间
      if not success && attempts < strategy.max_attempts {
        let backoff = (strategy.backoff_ms as Float * 
                      (strategy.multiplier.pow(attempts - 1))).to_int()
        let actual_backoff = if backoff > strategy.max_backoff_ms { 
          strategy.max_backoff_ms 
        } else { 
          backoff 
        }
        total_latency = total_latency + actual_backoff
      }
    }
    
    {
      success: success,
      attempts: attempts,
      error: error,
      latency: total_latency,
      packet_loss: not success,
      corruption: false
    }
  }
  
  // 模拟数据包校验
  let verify_packet_integrity = fn(original: TransportPacket, received: TransportPacket) {
    let headers_match = original.headers.length() == received.headers.length() &&
                       original.headers.all(fn(header) {
                         received.headers.contains(header)
                       })
    
    let payload_match = original.payload == received.payload
    
    let metadata_match = original.metadata.packet_id == received.metadata.packet_id &&
                        original.metadata.source == received.metadata.source &&
                        original.metadata.destination == received.metadata.destination
    
    headers_match && payload_match && metadata_match
  }
  
  // 创建重试策略
  let conservative_strategy = {
    max_attempts: 3,
    backoff_ms: 100,
    max_backoff_ms: 1000,
    multiplier: 2.0
  }
  
  let aggressive_strategy = {
    max_attempts: 5,
    backoff_ms: 50,
    max_backoff_ms: 500,
    multiplier: 1.5
  }
  
  // 创建测试包
  let test_packet = {
    protocol: TransportProtocol::HTTP,
    headers: [
      ("Content-Type", "application/json"),
      ("X-Packet-ID", "test-packet-123")
    ],
    payload: "test telemetry data".to_bytes(),
    metadata: {
      packet_id: "test-packet-123",
      timestamp: 1640995200000,
      source: "test-sender",
      destination: "test-receiver",
      compression: "gzip"
    }
  }
  
  // 测试低失败率场景
  let low_failure_rate = 0.1  // 10%失败率
  let conservative_low = simulate_transmission(test_packet, conservative_strategy, low_failure_rate)
  let aggressive_low = simulate_transmission(test_packet, aggressive_strategy, low_failure_rate)
  
  assert_true(conservative_low.success)
  assert_true(aggressive_low.success)
  assert_true(conservative_low.attempts <= conservative_strategy.max_attempts)
  assert_true(aggressive_low.attempts <= aggressive_strategy.max_attempts)
  assert_false(conservative_low.packet_loss)
  assert_false(aggressive_low.packet_loss)
  
  // 测试中等失败率场景
  let medium_failure_rate = 0.3  // 30%失败率
  let conservative_medium = simulate_transmission(test_packet, conservative_strategy, medium_failure_rate)
  let aggressive_medium = simulate_transmission(test_packet, aggressive_strategy, medium_failure_rate)
  
  // 激进策略应该有更高的成功率
  assert_true(aggressive_medium.success || conservative_medium.success)
  if aggressive_medium.success && conservative_medium.success {
    assert_true(aggressive_medium.latency <= conservative_medium.latency)
  }
  
  // 测试高失败率场景
  let high_failure_rate = 0.7  // 70%失败率
  let conservative_high = simulate_transmission(test_packet, conservative_strategy, high_failure_rate)
  let aggressive_high = simulate_transmission(test_packet, aggressive_strategy, high_failure_rate)
  
  // 激进策略应该有更高的成功率
  assert_true(aggressive_high.attempts >= conservative_high.attempts)
  
  // 测试数据包完整性
  let corrupted_packet = {
    protocol: test_packet.protocol,
    headers: test_packet.headers,
    payload: "corrupted data".to_bytes(),  // 修改载荷
    metadata: test_packet.metadata
  }
  
  let integrity_check = verify_packet_integrity(test_packet, test_packet)
  let corruption_check = verify_packet_integrity(test_packet, corrupted_packet)
  
  assert_true(integrity_check)
  assert_false(corruption_check)
  
  // 测试部分数据包丢失
  let simulate_packet_loss = fn(loss_rate: Float, packet_count: Int) {
    let mut received = 0
    let mut lost = 0
    
    for i in 0..packet_count {
      let random = (Time::now() + i) % 100
      if random < (loss_rate * 100.0) {
        lost = lost + 1
      } else {
        received = received + 1
      }
    }
    
    {
      total: packet_count,
      received: received,
      lost: lost,
      loss_rate: lost as Float / packet_count as Float
    }
  }
  
  let low_loss_result = simulate_packet_loss(0.05, 100)
  let medium_loss_result = simulate_packet_loss(0.15, 100)
  let high_loss_result = simulate_packet_loss(0.3, 100)
  
  assert_eq(low_loss_result.total, 100)
  assert_eq(medium_loss_result.total, 100)
  assert_eq(high_loss_result.total, 100)
  
  assert_true(low_loss_result.loss_rate < medium_loss_result.loss_rate)
  assert_true(medium_loss_result.loss_rate < high_loss_result.loss_rate)
  assert_true(low_loss_result.loss_rate < 0.1)
  assert_true(medium_loss_result.loss_rate < 0.25)
  assert_true(high_loss_result.loss_rate < 0.4)
}

// 测试7: 批量传输优化
test "批量传输优化" {
  // 定义批量传输配置
  type BatchConfig = {
    max_batch_size: Int,
    max_batch_delay_ms: Int,
    compression_threshold: Int,
    priority_levels: Int
  }
  
  // 定义批量传输结果
  type BatchResult = {
    batches: Array<{
      batch_id: String,
      items: Int,
      compressed_size: Int,
      transmission_time: Int
    }>,
    total_items: Int,
    total_size: Int,
    compression_ratio: Float,
    total_time: Int
  }
  
  // 创建遥测数据项
  let create_telemetry_item = fn(id: Int) {
    {
      item_id: "item-" + id.to_string(),
      timestamp: 1640995200000 + id * 1000,
      data: "telemetry data for item " + id.to_string(),
      size: 50 + (id % 100)
    }
  }
  
  // 简单压缩函数
  let simple_compress = fn(data: String) {
    // 简化的压缩：移除重复空格和换行
    let compressed = data.replace("  ", " ").replace("\n", " ")
    if compressed.length() < data.length() {
      compressed
    } else {
      data
    }
  }
  
  // 批量处理函数
  let process_batch = fn(items: Array<{
    item_id: String,
    timestamp: Int,
    data: String,
    size: Int
  }>, config: BatchConfig) {
    let mut batches = []
    let mut current_batch = []
    let mut current_size = 0
    let mut batch_id = 0
    
    for item in items {
      // 检查是否需要开始新批次
      if current_batch.length() >= config.max_batch_size || 
         current_size >= config.compression_threshold {
        
        if current_batch.length() > 0 {
          // 处理当前批次
          let batch_data = current_batch.map(fn(item) { item.data }).join("\n")
          let compressed_data = simple_compress(batch_data)
          let transmission_time = 100 + current_size / 10  // 模拟传输时间
          
          batches = batches + {
            batch_id: "batch-" + batch_id.to_string(),
            items: current_batch.length(),
            compressed_size: compressed_data.length(),
            transmission_time: transmission_time
          }
          
          batch_id = batch_id + 1
          current_batch = []
          current_size = 0
        }
      }
      
      current_batch = current_batch + [item]
      current_size = current_size + item.size
    }
    
    // 处理最后一个批次
    if current_batch.length() > 0 {
      let batch_data = current_batch.map(fn(item) { item.data }).join("\n")
      let compressed_data = simple_compress(batch_data)
      let transmission_time = 100 + current_size / 10
      
      batches = batches + {
        batch_id: "batch-" + batch_id.to_string(),
        items: current_batch.length(),
        compressed_size: compressed_data.length(),
        transmission_time: transmission_time
      }
    }
    
    // 计算总体统计
    let total_items = items.reduce(fn(acc, item) { acc + 1 }, 0)
    let total_size = items.reduce(fn(acc, item) { acc + item.size }, 0)
    let total_compressed_size = batches.reduce(fn(acc, batch) { acc + batch.compressed_size }, 0)
    let total_time = batches.reduce(fn(acc, batch) { acc + batch.transmission_time }, 0)
    
    {
      batches: batches,
      total_items: total_items,
      total_size: total_size,
      compression_ratio: total_compressed_size as Float / total_size as Float,
      total_time: total_time
    }
  }
  
  // 创建测试数据
  let create_test_items = fn(count: Int) {
    let mut items = []
    for i in 0..count {
      items = items + [create_telemetry_item(i)]
    }
    items
  }
  
  // 创建不同的批量配置
  let small_batch_config = {
    max_batch_size: 10,
    max_batch_delay_ms: 100,
    compression_threshold: 500,
    priority_levels: 3
  }
  
  let medium_batch_config = {
    max_batch_size: 50,
    max_batch_delay_ms: 500,
    compression_threshold: 2000,
    priority_levels: 5
  }
  
  let large_batch_config = {
    max_batch_size: 100,
    max_batch_delay_ms: 1000,
    compression_threshold: 5000,
    priority_levels: 10
  }
  
  // 创建不同规模的测试数据
  let small_items = create_test_items(50)
  let medium_items = create_test_items(200)
  let large_items = create_test_items(500)
  
  // 测试不同配置的批量处理
  let small_result = process_batch(small_items, small_batch_config)
  let medium_result = process_batch(medium_items, medium_batch_config)
  let large_result = process_batch(large_items, large_batch_config)
  
  // 验证小规模处理
  assert_eq(small_result.total_items, 50)
  assert_true(small_result.batches.length() >= 5)  // 至少5个批次
  assert_true(small_result.compression_ratio < 1.0)  // 应该有压缩效果
  
  // 验证中等规模处理
  assert_eq(medium_result.total_items, 200)
  assert_true(medium_result.batches.length() >= 4)  // 至少4个批次
  assert_true(medium_result.compression_ratio < 1.0)
  
  // 验证大规模处理
  assert_eq(large_result.total_items, 500)
  assert_true(large_result.batches.length() >= 5)  // 至少5个批次
  assert_true(large_result.compression_ratio < 1.0)
  
  // 验证批次大小限制
  for batch in small_result.batches {
    assert_true(batch.items <= small_batch_config.max_batch_size)
  }
  
  for batch in medium_result.batches {
    assert_true(batch.items <= medium_batch_config.max_batch_size)
  }
  
  for batch in large_result.batches {
    assert_true(batch.items <= large_batch_config.max_batch_size)
  }
  
  // 验证压缩效果随批次大小增加
  assert_true(large_result.compression_ratio <= medium_result.compression_ratio)
  assert_true(medium_result.compression_ratio <= small_result.compression_ratio)
  
  // 测试优先级处理
  let process_with_priority = fn(items: Array<{
    item_id: String,
    timestamp: Int,
    data: String,
    size: Int
  }>, config: BatchConfig) {
    // 为每个项目分配优先级（基于时间戳）
    let mut prioritized_items = []
    for item in items {
      let priority = (item.timestamp % config.priority_levels) as Int
      prioritized_items = prioritized_items + [{item: item, priority: priority}]
    }
    
    // 按优先级排序
    prioritized_items = prioritized_items.sort(fn(a, b) { 
      if a.priority < b.priority { -1 } 
      else if a.priority > b.priority { 1 } 
      else { 0 } 
    })
    
    // 提取排序后的项目
    let sorted_items = prioritized_items.map(fn(p) { p.item })
    
    // 使用标准批量处理
    process_batch(sorted_items, config)
  }
  
  let priority_result = process_with_priority(medium_items, medium_batch_config)
  
  // 验证优先级处理
  assert_eq(priority_result.total_items, medium_result.total_items)
  assert_eq(priority_result.total_size, medium_result.total_size)
  
  // 优先级处理可能会影响压缩比
  assert_true(priority_result.compression_ratio > 0.0)
}

// 测试8: 自适应传输策略
test "自适应传输策略" {
  // 定义网络条件
  type NetworkConditions = {
    bandwidth: Int,        // bps
    latency: Int,          // ms
    packet_loss: Float,    // 0.0-1.0
    jitter: Int,           // ms
    reliability: Float     // 0.0-1.0
  }
  
  // 定义传输策略
  enum TransmissionStrategy {
    HighThroughput  // 高吞吐量
    LowLatency      // 低延迟
    HighReliability // 高可靠性
    Balanced        // 平衡
    Adaptive        // 自适应
  }
  
  // 定义策略参数
  type StrategyParameters = {
    batch_size: Int,
    compression_level: Int,
    retry_count: Int,
    timeout_ms: Int,
    protocol: TransportProtocol
  }
  
  // 分析网络条件
  let analyze_conditions = fn(conditions: NetworkConditions) {
    // 计算网络质量分数
    let bandwidth_score = if conditions.bandwidth > 1000000 { 1.0 } 
                         else if conditions.bandwidth > 100000 { 0.7 } 
                         else if conditions.bandwidth > 10000 { 0.4 } 
                         else { 0.1 }
    
    let latency_score = if conditions.latency < 10 { 1.0 } 
                       else if conditions.latency < 50 { 0.8 } 
                       else if conditions.latency < 200 { 0.5 } 
                       else { 0.2 }
    
    let loss_score = if conditions.packet_loss < 0.01 { 1.0 } 
                    else if conditions.packet_loss < 0.05 { 0.8 } 
                    else if conditions.packet_loss < 0.1 { 0.5 } 
                    else { 0.2 }
    
    let jitter_score = if conditions.jitter < 5 { 1.0 } 
                      else if conditions.jitter < 20 { 0.8 } 
                      else if conditions.jitter < 50 { 0.5 } 
                      else { 0.2 }
    
    let reliability_score = conditions.reliability
    
    // 综合评分
    let overall_score = (bandwidth_score + latency_score + loss_score + jitter_score + reliability_score) / 5.0
    
    {
      overall_score: overall_score,
      bandwidth_score: bandwidth_score,
      latency_score: latency_score,
      loss_score: loss_score,
      jitter_score: jitter_score,
      reliability_score: reliability_score
    }
  }
  
  // 选择传输策略
  let select_strategy = fn(conditions: NetworkConditions, analysis: {
    overall_score: Float,
    bandwidth_score: Float,
    latency_score: Float,
    loss_score: Float,
    jitter_score: Float,
    reliability_score: Float
  }) {
    if analysis.overall_score > 0.8 {
      // 网络条件很好，使用高吞吐量策略
      TransmissionStrategy::HighThroughput
    } else if analysis.latency_score > 0.8 && analysis.jitter_score > 0.8 {
      // 延迟和抖动都很低，使用低延迟策略
      TransmissionStrategy::LowLatency
    } else if analysis.loss_score < 0.5 || analysis.reliability_score < 0.5 {
      // 丢包率高或可靠性低，使用高可靠性策略
      TransmissionStrategy::HighReliability
    } else if analysis.overall_score > 0.5 {
      // 网络条件中等，使用平衡策略
      TransmissionStrategy::Balanced
    } else {
      // 网络条件差，使用自适应策略
      TransmissionStrategy::Adaptive
    }
  }
  
  // 获取策略参数
  let get_strategy_parameters = fn(strategy: TransmissionStrategy) {
    match strategy {
      TransmissionStrategy::HighThroughput => {
        {
          batch_size: 100,
          compression_level: 3,
          retry_count: 2,
          timeout_ms: 5000,
          protocol: TransportProtocol::TCP
        }
      }
      TransmissionStrategy::LowLatency => {
        {
          batch_size: 10,
          compression_level: 1,
          retry_count: 1,
          timeout_ms: 1000,
          protocol: TransportProtocol::UDP
        }
      }
      TransmissionStrategy::HighReliability => {
        {
          batch_size: 20,
          compression_level: 2,
          retry_count: 5,
          timeout_ms: 10000,
          protocol: TransportProtocol::TCP
        }
      }
      TransmissionStrategy::Balanced => {
        {
          batch_size: 50,
          compression_level: 2,
          retry_count: 3,
          timeout_ms: 3000,
          protocol: TransportProtocol::HTTP
        }
      }
      TransmissionStrategy::Adaptive => {
        {
          batch_size: 30,
          compression_level: 2,
          retry_count: 3,
          timeout_ms: 2000,
          protocol: TransportProtocol::WebSocket
        }
      }
    }
  }
  
  // 创建不同的网络条件
  let excellent_conditions = {
    bandwidth: 10000000,  // 10 Mbps
    latency: 5,           // 5 ms
    packet_loss: 0.001,   // 0.1%
    jitter: 2,            // 2 ms
    reliability: 0.999    // 99.9%
  }
  
  let poor_conditions = {
    bandwidth: 100000,    // 100 kbps
    latency: 500,         // 500 ms
    packet_loss: 0.1,     // 10%
    jitter: 100,          // 100 ms
    reliability: 0.9      // 90%
  }
  
  let variable_conditions = {
    bandwidth: 1000000,   // 1 Mbps
    latency: 100,         // 100 ms
    packet_loss: 0.05,    // 5%
    jitter: 50,           // 50 ms
    reliability: 0.95     // 95%
  }
  
  // 测试策略选择
  let excellent_analysis = analyze_conditions(excellent_conditions)
  let excellent_strategy = select_strategy(excellent_conditions, excellent_analysis)
  let excellent_params = get_strategy_parameters(excellent_strategy)
  
  let poor_analysis = analyze_conditions(poor_conditions)
  let poor_strategy = select_strategy(poor_conditions, poor_analysis)
  let poor_params = get_strategy_parameters(poor_strategy)
  
  let variable_analysis = analyze_conditions(variable_conditions)
  let variable_strategy = select_strategy(variable_conditions, variable_analysis)
  let variable_params = get_strategy_parameters(variable_strategy)
  
  // 验证优秀网络条件的策略选择
  assert_eq(excellent_strategy, TransmissionStrategy::HighThroughput)
  assert_eq(excellent_params.batch_size, 100)
  assert_eq(excellent_params.compression_level, 3)
  assert_eq(excellent_params.retry_count, 2)
  assert_eq(excellent_params.protocol, TransportProtocol::TCP)
  
  // 验证差网络条件的策略选择
  assert_eq(poor_strategy, TransmissionStrategy::Adaptive)
  assert_eq(poor_params.batch_size, 30)
  assert_eq(poor_params.compression_level, 2)
  assert_eq(poor_params.retry_count, 3)
  assert_eq(poor_params.protocol, TransportProtocol::WebSocket)
  
  // 验证可变网络条件的策略选择
  assert_eq(variable_strategy, TransmissionStrategy::Balanced)
  assert_eq(variable_params.batch_size, 50)
  assert_eq(variable_params.compression_level, 2)
  assert_eq(variable_params.retry_count, 3)
  assert_eq(variable_params.protocol, TransportProtocol::HTTP)
  
  // 测试策略适应性能
  let test_strategy_adaptation = fn(initial_conditions: NetworkConditions, new_conditions: NetworkConditions) {
    let initial_analysis = analyze_conditions(initial_conditions)
    let initial_strategy = select_strategy(initial_conditions, initial_analysis)
    
    let new_analysis = analyze_conditions(new_conditions)
    let new_strategy = select_strategy(new_conditions, new_analysis)
    
    {
      initial_conditions: initial_conditions,
      new_conditions: new_conditions,
      initial_strategy: initial_strategy,
      new_strategy: new_strategy,
      strategy_changed: initial_strategy != new_strategy
    }
  }
  
  // 测试从优秀到差的条件变化
  let adaptation_result = test_strategy_adaptation(excellent_conditions, poor_conditions)
  
  assert_eq(adaptation_result.initial_strategy, TransmissionStrategy::HighThroughput)
  assert_eq(adaptation_result.new_strategy, TransmissionStrategy::Adaptive)
  assert_true(adaptation_result.strategy_changed)
  
  // 测试策略参数的合理性
  let validate_parameters = fn(params: StrategyParameters) {
    params.batch_size > 0 && params.batch_size <= 1000 &&
    params.compression_level >= 1 && params.compression_level <= 5 &&
    params.retry_count >= 1 && params.retry_count <= 10 &&
    params.timeout_ms > 0 && params.timeout_ms <= 60000
  }
  
  assert_true(validate_parameters(excellent_params))
  assert_true(validate_parameters(poor_params))
  assert_true(validate_parameters(variable_params))
}

// 测试9: 传输监控和指标
test "传输监控和指标" {
  // 定义传输指标
  type TransmissionMetrics = {
    packets_sent: Int,
    packets_received: Int,
    bytes_sent: Int,
    bytes_received: Int,
    successful_transmissions: Int,
    failed_transmissions: Int,
    average_latency: Float,
    min_latency: Int,
    max_latency: Int,
    packet_loss_rate: Float,
    throughput: Float,
    error_rate: Float
  }
  
  // 定义传输记录
  type TransmissionRecord = {
    timestamp: Int,
    packet_id: String,
    size: Int,
    latency: Int,
    success: Bool,
    error: Option[String]
  }
  
  // 创建传输记录
  let create_record = fn(packet_id: String, size: Int, success: Bool, error: Option[String]) {
    {
      timestamp: Time::now(),
      packet_id: packet_id,
      size: size,
      latency: 50 + (Time::now() % 200),  // 50-250ms随机延迟
      success: success,
      error: error
    }
  }
  
  // 计算传输指标
  let calculate_metrics = fn(records: Array[TransmissionRecord]) {
    if records.length() == 0 {
      return {
        packets_sent: 0,
        packets_received: 0,
        bytes_sent: 0,
        bytes_received: 0,
        successful_transmissions: 0,
        failed_transmissions: 0,
        average_latency: 0.0,
        min_latency: 0,
        max_latency: 0,
        packet_loss_rate: 0.0,
        throughput: 0.0,
        error_rate: 0.0
      }
    }
    
    let packets_sent = records.length()
    let successful_records = records.filter(fn(r) { r.success })
    let packets_received = successful_records.length()
    let failed_transmissions = packets_sent - packets_received
    
    let bytes_sent = records.reduce(fn(acc, r) { acc + r.size }, 0)
    let bytes_received = successful_records.reduce(fn(acc, r) { acc + r.size }, 0)
    
    let successful_transmissions = packets_received
    let failed_transmissions = packets_sent - packets_received
    
    let latencies = successful_records.map(fn(r) { r.latency })
    let average_latency = if latencies.length() > 0 {
      latencies.reduce(fn(acc, l) { acc + l }, 0) as Float / latencies.length() as Float
    } else {
      0.0
    }
    
    let min_latency = if latencies.length() > 0 {
      latencies.reduce(fn(acc, l) { if l < acc { l } else { acc }, latencies[0])
    } else {
      0
    }
    
    let max_latency = if latencies.length() > 0 {
      latencies.reduce(fn(acc, l) { if l > acc { l } else { acc }, latencies[0])
    } else {
      0
    }
    
    let packet_loss_rate = if packets_sent > 0 {
      failed_transmissions as Float / packets_sent as Float
    } else {
      0.0
    }
    
    // 计算吞吐量（简化版）
    let time_span = if records.length() > 1 {
      records[records.length() - 1].timestamp - records[0].timestamp
    } else {
      1000  // 默认1秒
    }
    
    let throughput = if time_span > 0 {
      (bytes_received as Float / time_span as Float) * 1000.0  // bytes/second
    } else {
      0.0
    }
    
    let error_rate = packet_loss_rate
    
    {
      packets_sent: packets_sent,
      packets_received: packets_received,
      bytes_sent: bytes_sent,
      bytes_received: bytes_received,
      successful_transmissions: successful_transmissions,
      failed_transmissions: failed_transmissions,
      average_latency: average_latency,
      min_latency: min_latency,
      max_latency: max_latency,
      packet_loss_rate: packet_loss_rate,
      throughput: throughput,
      error_rate: error_rate
    }
  }
  
  // 创建测试记录
  let create_test_records = fn(count: Int, failure_rate: Float) {
    let mut records = []
    
    for i in 0..count {
      let success = (Time::now() + i) % 100 > (failure_rate * 100.0)
      let error = if success { None } else { Some("传输失败") }
      let record = create_record("packet-" + i.to_string(), 1000 + (i % 500), success, error)
      records = records + [record]
    }
    
    records
  }
  
  // 测试低失败率场景
  let low_failure_records = create_test_records(100, 0.05)  // 5%失败率
  let low_failure_metrics = calculate_metrics(low_failure_records)
  
  assert_eq(low_failure_metrics.packets_sent, 100)
  assert_true(low_failure_metrics.packets_received >= 90)
  assert_eq(low_failure_metrics.successful_transmissions, low_failure_metrics.packets_received)
  assert_eq(low_failure_metrics.failed_transmissions, low_failure_metrics.packets_sent - low_failure_metrics.packets_received)
  assert_true(low_failure_metrics.packet_loss_rate < 0.1)
  assert_true(low_failure_metrics.average_latency > 0)
  assert_true(low_failure_metrics.min_latency > 0)
  assert_true(low_failure_metrics.max_latency >= low_failure_metrics.min_latency)
  assert_true(low_failure_metrics.throughput > 0)
  assert_eq(low_failure_metrics.error_rate, low_failure_metrics.packet_loss_rate)
  
  // 测试中等失败率场景
  let medium_failure_records = create_test_records(100, 0.2)  // 20%失败率
  let medium_failure_metrics = calculate_metrics(medium_failure_records)
  
  assert_eq(medium_failure_metrics.packets_sent, 100)
  assert_true(medium_failure_metrics.packets_received >= 70 && medium_failure_metrics.packets_received <= 90)
  assert_true(medium_failure_metrics.packet_loss_rate >= 0.1 && medium_failure_metrics.packet_loss_rate <= 0.3)
  
  // 测试高失败率场景
  let high_failure_records = create_test_records(100, 0.4)  // 40%失败率
  let high_failure_metrics = calculate_metrics(high_failure_records)
  
  assert_eq(high_failure_metrics.packets_sent, 100)
  assert_true(high_failure_metrics.packets_received <= 70)
  assert_true(high_failure_metrics.packet_loss_rate >= 0.3)
  
  // 比较不同失败率的指标
  assert_true(low_failure_metrics.packet_loss_rate < medium_failure_metrics.packet_loss_rate)
  assert_true(medium_failure_metrics.packet_loss_rate < high_failure_metrics.packet_loss_rate)
  assert_true(low_failure_metrics.throughput >= medium_failure_metrics.throughput)
  assert_true(medium_failure_metrics.throughput >= high_failure_metrics.throughput)
  
  // 测试时间窗口内的指标
  let calculate_time_window_metrics = fn(records: Array[TransmissionRecord], window_start: Int, window_end: Int) {
    let window_records = records.filter(fn(r) { r.timestamp >= window_start && r.timestamp <= window_end })
    calculate_metrics(window_records)
  }
  
  let all_records = low_failure_records + medium_failure_records + high_failure_records
  let start_time = all_records[0].timestamp
  let end_time = all_records[all_records.length() - 1].timestamp
  let mid_time = start_time + (end_time - start_time) / 2
  
  let first_half_metrics = calculate_time_window_metrics(all_records, start_time, mid_time)
  let second_half_metrics = calculate_time_window_metrics(all_records, mid_time, end_time)
  
  assert_eq(first_half_metrics.packets_sent + second_half_metrics.packets_sent, all_records.length())
  assert_eq(first_half_metrics.packets_received + second_half_metrics.packets_received, 
             all_records.filter(fn(r) { r.success }).length())
  
  // 测试指标聚合
  let aggregate_metrics = fn(metrics_array: Array[TransmissionMetrics]) {
    let total_packets_sent = metrics_array.reduce(fn(acc, m) { acc + m.packets_sent }, 0)
    let total_packets_received = metrics_array.reduce(fn(acc, m) { acc + m.packets_received }, 0)
    let total_bytes_sent = metrics_array.reduce(fn(acc, m) { acc + m.bytes_sent }, 0)
    let total_bytes_received = metrics_array.reduce(fn(acc, m) { acc + m.bytes_received }, 0)
    let total_successful = metrics_array.reduce(fn(acc, m) { acc + m.successful_transmissions }, 0)
    let total_failed = metrics_array.reduce(fn(acc, m) { acc + m.failed_transmissions }, 0)
    
    let weighted_avg_latency = if total_packets_received > 0 {
      let total_latency = metrics_array.reduce(fn(acc, m) { 
        acc + (m.average_latency * m.packets_received as Float) 
      }, 0.0)
      total_latency / total_packets_received as Float
    } else {
      0.0
    }
    
    let overall_min_latency = metrics_array.reduce(fn(acc, m) { 
      if m.min_latency < acc || acc == 0 { m.min_latency } else { acc } 
    }, 0)
    
    let overall_max_latency = metrics_array.reduce(fn(acc, m) { 
      if m.max_latency > acc { m.max_latency } else { acc } 
    }, 0)
    
    let overall_packet_loss_rate = if total_packets_sent > 0 {
      total_failed as Float / total_packets_sent as Float
    } else {
      0.0
    }
    
    let overall_throughput = if total_bytes_received > 0 {
      let total_time_span = metrics_array.reduce(fn(acc, m) { acc + 1000 }, 0)  // 简化计算
      (total_bytes_received as Float / total_time_span as Float) * 1000.0
    } else {
      0.0
    }
    
    {
      packets_sent: total_packets_sent,
      packets_received: total_packets_received,
      bytes_sent: total_bytes_sent,
      bytes_received: total_bytes_received,
      successful_transmissions: total_successful,
      failed_transmissions: total_failed,
      average_latency: weighted_avg_latency,
      min_latency: overall_min_latency,
      max_latency: overall_max_latency,
      packet_loss_rate: overall_packet_loss_rate,
      throughput: overall_throughput,
      error_rate: overall_packet_loss_rate
    }
  }
  
  let aggregated_metrics = aggregate_metrics([low_failure_metrics, medium_failure_metrics, high_failure_metrics])
  
  assert_eq(aggregated_metrics.packets_sent, 300)
  assert_eq(aggregated_metrics.packets_received, 
             low_failure_metrics.packets_received + medium_failure_metrics.packets_received + high_failure_metrics.packets_received)
  assert_true(aggregated_metrics.average_latency > 0)
  assert_true(aggregated_metrics.throughput > 0)
}

// 测试10: 传输安全性
test "传输安全性" {
  // 定义加密算法
  enum EncryptionAlgorithm {
    None
    AES128
    AES256
    ChaCha20
  }
  
  // 定义安全传输配置
  type SecureTransmissionConfig = {
    encryption_algorithm: EncryptionAlgorithm,
    compression_enabled: Bool,
    integrity_check: Bool,
    authentication: Bool
  }
  
  // 定义安全传输包
  type SecurePacket = {
    headers: Array[(String, String)],
    payload: Array[UInt8],
    signature: Option[String],
    encryption_metadata: {
      algorithm: EncryptionAlgorithm,
      key_id: String,
      iv: String
    }
  }
  
  // 简化的加密函数
  let simple_encrypt = fn(data: Array[UInt8], key: String, algorithm: EncryptionAlgorithm) {
    match algorithm {
      EncryptionAlgorithm::None => {
        {
          encrypted_data: data,
          key_id: "none",
          iv: ""
        }
      }
      EncryptionAlgorithm::AES128 => {
        // 简化的AES加密（XOR模拟）
        let key_bytes = key.to_bytes()
        let mut encrypted = []
        
        for i in 0..data.length() {
          let key_byte = key_bytes[i % key_bytes.length()]
          encrypted = encrypted + [data[i] ^ key_byte]
        }
        
        {
          encrypted_data: encrypted,
          key_id: "aes128",
          iv: "random-iv-12345"
        }
      }
      EncryptionAlgorithm::AES256 => {
        // 简化的AES256加密（XOR模拟）
        let key_bytes = key.to_bytes()
        let mut encrypted = []
        
        for i in 0..data.length() {
          let key_byte = key_bytes[i % key_bytes.length()]
          encrypted = encrypted + [data[i] ^ key_byte ^ 0xFF]  // 额外的XOR
        }
        
        {
          encrypted_data: encrypted,
          key_id: "aes256",
          iv: "random-iv-67890"
        }
      }
      EncryptionAlgorithm::ChaCha20 => {
        // 简化的ChaCha20加密（XOR模拟）
        let key_bytes = key.to_bytes()
        let mut encrypted = []
        
        for i in 0..data.length() {
          let key_byte = key_bytes[(i * 2) % key_bytes.length()]  // 不同的密钥使用模式
          encrypted = encrypted + [data[i] ^ key_byte ^ 0xAA]
        }
        
        {
          encrypted_data: encrypted,
          key_id: "chacha20",
          iv: "random-iv-abcdef"
        }
      }
    }
  }
  
  // 简化的解密函数
  let simple_decrypt = fn(encrypted_data: Array[UInt8], key: String, algorithm: EncryptionAlgorithm) {
    match algorithm {
      EncryptionAlgorithm::None => {
        encrypted_data
      }
      EncryptionAlgorithm::AES128 => {
        // 简化的AES解密
        let key_bytes = key.to_bytes()
        let mut decrypted = []
        
        for i in 0..encrypted_data.length() {
          let key_byte = key_bytes[i % key_bytes.length()]
          decrypted = decrypted + [encrypted_data[i] ^ key_byte]
        }
        
        decrypted
      }
      EncryptionAlgorithm::AES256 => {
        // 简化的AES256解密
        let key_bytes = key.to_bytes()
        let mut decrypted = []
        
        for i in 0..encrypted_data.length() {
          let key_byte = key_bytes[i % key_bytes.length()]
          decrypted = decrypted + [encrypted_data[i] ^ key_byte ^ 0xFF]
        }
        
        decrypted
      }
      EncryptionAlgorithm::ChaCha20 => {
        // 简化的ChaCha20解密
        let key_bytes = key.to_bytes()
        let mut decrypted = []
        
        for i in 0..encrypted_data.length() {
          let key_byte = key_bytes[(i * 2) % key_bytes.length()]
          decrypted = decrypted + [encrypted_data[i] ^ key_byte ^ 0xAA]
        }
        
        decrypted
      }
    }
  }
  
  // 简化的签名函数
  let simple_sign = fn(data: Array[UInt8], key: String) {
    let mut hash = 0
    let data_bytes = data
    let key_bytes = key.to_bytes()
    
    for i in 0..data_bytes.length() {
      let data_byte = data_bytes[i]
      let key_byte = key_bytes[i % key_bytes.length()]
      hash = (hash + data_byte + key_byte) % 2147483647
    }
    
    "sig-" + hash.to_string(16)
  }
  
  // 验证签名
  let verify_signature = fn(data: Array[UInt8], signature: String, key: String) {
    let expected_signature = simple_sign(data, key)
    signature == expected_signature
  }
  
  // 创建安全传输包
  let create_secure_packet = fn(payload: Array[UInt8], config: SecureTransmissionConfig, encryption_key: String) {
    // 压缩数据（如果启用）
    let compressed_payload = if config.compression_enabled {
      // 简化的压缩：移除重复字节
      let mut compressed = []
      if payload.length() > 0 {
        let mut prev_byte = payload[0]
        compressed = compressed + [prev_byte]
        
        for i in 1..payload.length() {
          if payload[i] != prev_byte {
            compressed = compressed + [payload[i]]
            prev_byte = payload[i]
          }
        }
      }
      compressed
    } else {
      payload
    }
    
    // 加密数据
    let encryption_result = simple_encrypt(compressed_payload, encryption_key, config.encryption_algorithm)
    
    // 生成签名（如果启用）
    let signature = if config.integrity_check {
      Some(simple_sign(encryption_result.encrypted_data, encryption_key))
    } else {
      None
    }
    
    {
      headers: [
        ("X-Encryption", config.encryption_algorithm.to_string()),
        ("X-Compression", if config.compression_enabled { "true" } else { "false" }),
        ("X-Integrity-Check", if config.integrity_check { "true" } else { "false" }),
        ("X-Authentication", if config.authentication { "true" } else { "false" })
      ],
      payload: encryption_result.encrypted_data,
      signature: signature,
      encryption_metadata: {
        algorithm: config.encryption_algorithm,
        key_id: encryption_result.key_id,
        iv: encryption_result.iv
      }
    }
  }
  
  // 验证安全传输包
  let verify_secure_packet = fn(packet: SecurePacket, config: SecureTransmissionConfig, encryption_key: String) {
    // 验证签名（如果启用）
    if config.integrity_check {
      match packet.signature {
        Some(signature) => {
          if not verify_signature(packet.payload, signature, encryption_key) {
            return {valid: false, error: "签名验证失败"}
          }
        }
        None => return {valid: false, error: "缺少签名"}
      }
    }
    
    // 解密数据
    let decrypted_payload = simple_decrypt(packet.payload, encryption_key, config.encryption_algorithm)
    
    // 解压数据（如果启用）
    let original_payload = if config.compression_enabled {
      // 简化的解压：恢复重复字节（这里无法完全恢复，仅作示例）
      decrypted_payload
    } else {
      decrypted_payload
    }
    
    {valid: true, error: None, payload: original_payload}
  }
  
  // 创建测试配置
  let insecure_config = {
    encryption_algorithm: EncryptionAlgorithm::None,
    compression_enabled: false,
    integrity_check: false,
    authentication: false
  }
  
  let basic_secure_config = {
    encryption_algorithm: EncryptionAlgorithm::AES128,
    compression_enabled: true,
    integrity_check: true,
    authentication: false
  }
  
  let high_security_config = {
    encryption_algorithm: EncryptionAlgorithm::AES256,
    compression_enabled: true,
    integrity_check: true,
    authentication: true
  }
  
  // 创建测试载荷
  let test_payload = "sensitive telemetry data with user information".to_bytes()
  let encryption_key = "encryption-key-12345"
  
  // 测试不安全配置
  let insecure_packet = create_secure_packet(test_payload, insecure_config, encryption_key)
  let insecure_verification = verify_secure_packet(insecure_packet, insecure_config, encryption_key)
  
  assert_true(insecure_verification.valid)
  assert_eq(insecure_verification.payload, test_payload)
  assert_eq(insecure_packet.encryption_metadata.algorithm, EncryptionAlgorithm::None)
  assert_eq(insecure_packet.signature, None)
  
  // 测试基本安全配置
  let basic_secure_packet = create_secure_packet(test_payload, basic_secure_config, encryption_key)
  let basic_secure_verification = verify_secure_packet(basic_secure_packet, basic_secure_config, encryption_key)
  
  assert_true(basic_secure_verification.valid)
  assert_eq(basic_secure_packet.encryption_metadata.algorithm, EncryptionAlgorithm::AES128)
  assert_true(basic_secure_packet.signature.is_some())
  assert_true(basic_secure_packet.headers.contains(("X-Encryption", "AES128")))
  assert_true(basic_secure_packet.headers.contains(("X-Compression", "true")))
  assert_true(basic_secure_packet.headers.contains(("X-Integrity-Check", "true")))
  
  // 测试高安全配置
  let high_security_packet = create_secure_packet(test_payload, high_security_config, encryption_key)
  let high_security_verification = verify_secure_packet(high_security_packet, high_security_config, encryption_key)
  
  assert_true(high_security_verification.valid)
  assert_eq(high_security_packet.encryption_metadata.algorithm, EncryptionAlgorithm::AES256)
  assert_true(high_security_packet.signature.is_some())
  assert_true(high_security_packet.headers.contains(("X-Encryption", "AES256")))
  assert_true(high_security_packet.headers.contains(("X-Authentication", "true")))
  
  // 测试篡改检测
  let tampered_packet = {
    headers: high_security_packet.headers,
    payload: high_security_packet.payload.slice(0, high_security_packet.payload.length() - 1) + [0],  // 修改最后一个字节
    signature: high_security_packet.signature,
    encryption_metadata: high_security_packet.encryption_metadata
  }
  
  let tampered_verification = verify_secure_packet(tampered_packet, high_security_config, encryption_key)
  assert_false(tampered_verification.valid)
  assert_eq(tampered_verification.error, "签名验证失败")
  
  // 测试错误密钥
  let wrong_key_verification = verify_secure_packet(high_security_packet, high_security_config, "wrong-key")
  assert_false(wrong_key_verification.valid)
  assert_eq(wrong_key_verification.error, "签名验证失败")
  
  // 测试不同加密算法
  let test_encryption_algorithms = fn(payload: Array[UInt8], key: String) {
    let algorithms = [EncryptionAlgorithm::None, EncryptionAlgorithm::AES128, EncryptionAlgorithm::AES256, EncryptionAlgorithm::ChaCha20]
    let mut results = []
    
    for algorithm in algorithms {
      let config = {
        encryption_algorithm: algorithm,
        compression_enabled: false,
        integrity_check: true,
        authentication: false
      }
      
      let packet = create_secure_packet(payload, config, key)
      let verification = verify_secure_packet(packet, config, key)
      
      results = results + {
        algorithm: algorithm,
        success: verification.valid,
        encrypted_size: packet.payload.length()
      }
    }
    
    results
  }
  
  let encryption_results = test_encryption_algorithms(test_payload, encryption_key)
  
  // 验证所有算法都能正确加解密
  for result in encryption_results {
    assert_true(result.success)
  }
  
  // 验证加密后的数据大小
  let none_result = encryption_results[0]
  let aes128_result = encryption_results[1]
  let aes256_result = encryption_results[2]
  let chacha20_result = encryption_results[3]
  
  assert_eq(none_result.algorithm, EncryptionAlgorithm::None)
  assert_eq(aes128_result.algorithm, EncryptionAlgorithm::AES128)
  assert_eq(aes256_result.algorithm, EncryptionAlgorithm::AES256)
  assert_eq(chacha20_result.algorithm, EncryptionAlgorithm::ChaCha20)
  
  // 加密后的数据大小应该与原始数据相同（在这个简化实现中）
  assert_eq(none_result.encrypted_size, test_payload.length())
  assert_eq(aes128_result.encrypted_size, test_payload.length())
  assert_eq(aes256_result.encrypted_size, test_payload.length())
  assert_eq(chacha20_result.encrypted_size, test_payload.length())
}