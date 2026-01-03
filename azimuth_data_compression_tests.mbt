// Azimuth Data Compression and Decompression Tests
// 数据压缩和解压缩测试用例 - 专注于压缩算法、压缩比和性能优化

// Test 1: 基础压缩功能测试
test "basic compression functionality" {
  // 创建测试数据
  let original_data = "This is a test string for compression. It contains repeated patterns and should compress well. "
    + "This is a test string for compression. It contains repeated patterns and should compress well. "
    + "This is a test string for compression. It contains repeated patterns and should compress well."
  
  // 测试GZIP压缩
  let gzip_compressor = GzipCompressor::new()
  let compressed_data = Compressor::compress(gzip_compressor, original_data.to_byte_array())
  
  // 验证压缩数据不为空且比原始数据小
  assert_true(compressed_data.length() > 0)
  assert_true(compressed_data.length() < original_data.length())
  
  // 测试解压缩
  let decompressed_data = Compressor::decompress(gzip_compressor, compressed_data)
  let decompressed_string = Bytearray::to_string(decompressed_data)
  
  // 验证解压缩后的数据与原始数据一致
  assert_eq(decompressed_string, original_data)
  
  // 测试DEFLATE压缩
  let deflate_compressor = DeflateCompressor::new()
  let deflate_compressed = Compressor::compress(deflate_compressor, original_data.to_byte_array())
  
  assert_true(deflate_compressed.length() > 0)
  assert_true(deflate_compressed.length() < original_data.length())
  
  let deflate_decompressed = Compressor::decompress(deflate_compressor, deflate_compressed)
  let deflate_decompressed_string = Bytearray::to_string(deflate_decompressed)
  
  assert_eq(deflate_decompressed_string, original_data)
}

// Test 2: 大数据压缩性能测试
test "large data compression performance" {
  // 创建大型测试数据
  let mut large_data = ""
  for i in 0..<10000 {
    large_data = large_data + "Large data chunk " + i.to_string() + " with repeated patterns. "
    + "This pattern should compress well when repeated many times. "
    + "Compression algorithms are designed to reduce redundancy in data. "
  }
  
  let original_size = large_data.length()
  let start_time = Time::now()
  
  // 测试GZIP压缩性能
  let gzip_compressor = GzipCompressor::new()
  let compressed_data = Compressor::compress(gzip_compressor, large_data.to_byte_array())
  
  let compression_time = Time::now() - start_time
  let compressed_size = compressed_data.length()
  let compression_ratio = compressed_size.to_float() / original_size.to_float()
  
  // 验证压缩性能
  assert_true(compression_time < 5000) // 压缩时间应小于5秒
  assert_true(compression_ratio < 0.5) // 压缩比应小于50%
  
  // 测试解压缩性能
  let decompress_start = Time::now()
  let decompressed_data = Compressor::decompress(gzip_compressor, compressed_data)
  let decompress_time = Time::now() - decompress_start
  
  // 验证解压缩性能
  assert_true(decompress_time < 5000) // 解压缩时间应小于5秒
  
  let decompressed_string = Bytearray::to_string(decompressed_data)
  assert_eq(decompressed_string, large_data)
  
  // 测试不同压缩级别的性能
  let levels = [1, 6, 9] // 快速、默认、最佳压缩
  for level in levels {
    let level_compressor = GzipCompressor::with_level(level)
    let level_start = Time::now()
    let level_compressed = Compressor::compress(level_compressor, large_data.to_byte_array())
    let level_time = Time::now() - level_start
    let level_size = level_compressed.length()
    
    // 验证不同级别的压缩效果
    assert_true(level_time < 10000) // 每个级别都应在10秒内完成
    assert_true(level_size < original_size) // 都应该有压缩效果
    
    // 更高的压缩级别通常压缩比更好但速度更慢
    if level == 9 {
      assert_true(level_size <= compressed_size) // 最佳压缩应该不大于默认压缩
    }
  }
}

// Test 3: 不同数据类型压缩测试
test "different data types compression" {
  // 测试文本数据压缩
  let text_data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. "
    + "Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. "
    + "Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris."
  
  let text_compressed = GzipCompressor::compress(text_data.to_byte_array())
  let text_ratio = text_compressed.length().to_float() / text_data.length().to_float()
  assert_true(text_ratio < 0.7) // 文本数据应该有较好的压缩比
  
  // 测试JSON数据压缩
  let json_data = "{\"name\":\"test\",\"value\":123,\"items\":[{\"id\":1,\"data\":\"item1\"},{\"id\":2,\"data\":\"item2\"}],"
    + "\"metadata\":{\"created\":\"2023-01-01\",\"updated\":\"2023-01-02\",\"tags\":[\"tag1\",\"tag2\",\"tag3\"]}}"
  
  let json_compressed = GzipCompressor::compress(json_data.to_byte_array())
  let json_ratio = json_compressed.length().to_float() / json_data.length().to_float()
  assert_true(json_ratio < 0.8) // JSON数据也应该有不错的压缩比
  
  // 测试重复性数据压缩
  let repetitive_data = "ABCD" * 2500 // 10000个字符，高度重复
  let repetitive_compressed = GzipCompressor::compress(repetitive_data.to_byte_array())
  let repetitive_ratio = repetitive_compressed.length().to_float() / repetitive_data.length().to_float()
  assert_true(repetitive_ratio < 0.1) // 重复数据应该有很高的压缩比
  
  // 测试随机数据压缩（压缩效果应该很差）
  let random_data = RandomDataGenerator::generate_string(10000) // 10000个随机字符
  let random_compressed = GzipCompressor::compress(random_data.to_byte_array())
  let random_ratio = random_compressed.length().to_float() / random_data.length().to_float()
  assert_true(random_ratio > 0.9) // 随机数据压缩效果应该很差
}

// Test 4: 流式压缩测试
test "streaming compression" {
  // 创建流式压缩器
  let stream_compressor = StreamCompressor::new(Gzip)
  
  // 分块压缩数据
  let chunks = [
    "First chunk of data to be compressed. ",
    "Second chunk with more data. ",
    "Third and final chunk of test data."
  ]
  
  let compressed_chunks = []
  for chunk in chunks {
    let compressed_chunk = StreamCompressor::compress_chunk(stream_compressor, chunk.to_byte_array())
    compressed_chunks = compressed_chunks.push(compressed_chunk)
  }
  
  // 完成压缩
  let final_chunk = StreamCompressor::finish(stream_compressor)
  compressed_chunks = compressed_chunks.push(final_chunk)
  
  // 合并压缩数据
  let mut compressed_data = Bytearray::new()
  for chunk in compressed_chunks {
    compressed_data = Bytearray::append(compressed_data, chunk)
  }
  
  // 验证压缩数据
  assert_true(compressed_data.length() > 0)
  
  // 创建流式解压缩器
  let stream_decompressor = StreamDecompressor::new(Gzip)
  
  // 分块解压缩数据
  let decompressed_chunks = []
  let chunk_size = 1024
  let mut offset = 0
  
  while offset < compressed_data.length() {
    let end_offset = if offset + chunk_size < compressed_data.length() {
      offset + chunk_size
    } else {
      compressed_data.length()
    }
    
    let chunk = Bytearray::slice(compressed_data, offset, end_offset)
    let decompressed_chunk = StreamDecompressor::decompress_chunk(stream_decompressor, chunk)
    
    if decompressed_chunk.length() > 0 {
      decompressed_chunks = decompressed_chunks.push(decompressed_chunk)
    }
    
    offset = end_offset
  }
  
  // 完成解压缩
  let final_decompressed = StreamDecompressor::finish(stream_decompressor)
  if final_decompressed.length() > 0 {
    decompressed_chunks = decompressed_chunks.push(final_decompressed)
  }
  
  // 合并解压缩数据
  let mut decompressed_data = Bytearray::new()
  for chunk in decompressed_chunks {
    decompressed_data = Bytearray::append(decompressed_data, chunk)
  }
  
  // 验证解压缩结果
  let decompressed_string = Bytearray::to_string(decompressed_data)
  let original_string = chunks.join("")
  assert_eq(decompressed_string, original_string)
}

// Test 5: 压缩字典测试
test "compression dictionary" {
  // 创建预定义字典（基于常见模式）
  let dictionary = "telemetry metrics span trace context attribute value "
    + "service name version instance id component module "
    + "http request response status code error message "
    + "timestamp duration latency measurement count"
  
  // 创建带字典的压缩器
  let dict_compressor = GzipCompressor::with_dictionary(dictionary.to_byte_array())
  
  // 创建包含字典词汇的测试数据
  let test_data = "telemetry service metrics span trace context attribute value "
    + "service name version instance id component module "
    + "http request response status code error message "
    + "timestamp duration latency measurement count"
    + " additional data not in dictionary"
  
  // 使用字典压缩
  let dict_compressed = Compressor::compress(dict_compressor, test_data.to_byte_array())
  
  // 使用普通压缩器压缩相同数据
  let normal_compressor = GzipCompressor::new()
  let normal_compressed = Compressor::compress(normal_compressor, test_data.to_byte_array())
  
  // 验证字典压缩效果更好
  assert_true(dict_compressed.length() < normal_compressed.length())
  
  // 使用字典解压缩
  let dict_decompressed = Compressor::decompress(dict_compressor, dict_compressed)
  let dict_decompressed_string = Bytearray::to_string(dict_decompressed)
  
  // 验证解压缩结果
  assert_eq(dict_decompressed_string, test_data)
  
  // 测试自定义字典效果
  let custom_dictionary = "azimuth telemetry system monitoring observability "
    + "distributed tracing metrics logging performance"
  
  let custom_compressor = GzipCompressor::with_dictionary(custom_dictionary.to_byte_array())
  
  let custom_test_data = "azimuth telemetry system provides comprehensive monitoring "
    + "and observability capabilities including distributed tracing "
    + "metrics logging and performance analysis"
  
  let custom_compressed = Compressor::compress(custom_compressor, custom_test_data.to_byte_array())
  let custom_normal_compressed = Compressor::compress(normal_compressor, custom_test_data.to_byte_array())
  
  // 验证自定义字典对相关数据有更好的压缩效果
  assert_true(custom_compressed.length() < custom_normal_compressed.length())
}

// Test 6: 并发压缩测试
test "concurrent compression" {
  // 创建测试数据集
  let data_sets = []
  for i in 0..<10 {
    let mut data = ""
    for j in 0..<1000 {
      data = data + "Data set " + i.to_string() + " chunk " + j.to_string() + ". "
    }
    data_sets = data_sets.push(data)
  }
  
  // 并发压缩
  let compression_tasks = []
  for i in 0..<data_sets.length() {
    let task = ConcurrentTask::new(fn(data_set_index: Int) -> ByteArray {
      let data = data_sets[data_set_index]
      let compressor = GzipCompressor::new()
      Compressor::compress(compressor, data.to_byte_array())
    }, i)
    
    compression_tasks = compression_tasks.push(task)
  }
  
  // 执行并发压缩任务
  let compressed_results = ConcurrentTask::execute_all(compression_tasks)
  
  // 验证压缩结果
  assert_eq(compressed_results.length(), data_sets.length())
  
  for i in 0..<compressed_results.length() {
    let compressed_data = compressed_results[i]
    let original_data = data_sets[i]
    
    // 验证压缩数据不为空且比原始数据小
    assert_true(compressed_data.length() > 0)
    assert_true(compressed_data.length() < original_data.length())
    
    // 验证解压缩结果
    let compressor = GzipCompressor::new()
    let decompressed_data = Compressor::decompress(compressor, compressed_data)
    let decompressed_string = Bytearray::to_string(decompressed_data)
    
    assert_eq(decompressed_string, original_data)
  }
}

// Test 7: 压缩错误处理测试
test "compression error handling" {
  // 测试空数据压缩
  let empty_data = ""
  let compressor = GzipCompressor::new()
  let empty_compressed = Compressor::compress(compressor, empty_data.to_byte_array())
  
  // 空数据应该能正常压缩
  assert_true(empty_compressed.length() > 0)
  
  let empty_decompressed = Compressor::decompress(compressor, empty_compressed)
  let empty_decompressed_string = Bytearray::to_string(empty_decompressed)
  assert_eq(empty_decompressed_string, empty_data)
  
  // 测试损坏数据解压缩
  let valid_data = "Valid test data for compression"
  let valid_compressed = Compressor::compress(compressor, valid_data.to_byte_array())
  
  // 损坏压缩数据（修改最后一个字节）
  let mut corrupted_data = valid_compressed
  let last_byte = corrupted_data[corrupted_data.length() - 1]
  corrupted_data[corrupted_data.length() - 1] = if last_byte == 0 { 1 } else { 0 }
  
  // 尝试解压缩损坏数据
  let corrupted_result = Compressor::decompress(compressor, corrupted_data)
  
  match corrupted_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(DecompressionError) => assert_true(true) // 预期错误
    Err(_) => assert_true(false)
  }
  
  // 测试不完整数据解压缩
  let incomplete_data = Bytearray::slice(valid_compressed, 0, valid_compressed.length() - 10)
  let incomplete_result = Compressor::decompress(compressor, incomplete_data)
  
  match incomplete_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(DecompressionError) => assert_true(true) // 预期错误
    Err(_) => assert_true(false)
  }
  
  // 测试内存限制下的压缩
  let large_data = "x" * 100000000 // 100MB数据
  let memory_limited_compressor = GzipCompressor::with_memory_limit(10 * 1024 * 1024) // 10MB限制
  
  let memory_result = Compressor::compress(memory_limited_compressor, large_data.to_byte_array())
  
  match memory_result {
    Ok(_) => assert_true(false) // 应该因内存限制失败
    Err(MemoryLimitError) => assert_true(true) // 预期错误
    Err(_) => assert_true(false)
  }
}

// Test 8: 压缩格式兼容性测试
test "compression format compatibility" {
  let test_data = "Compatibility test data for different compression formats"
  
  // 测试GZIP格式
  let gzip_compressor = GzipCompressor::new()
  let gzip_compressed = Compressor::compress(gzip_compressor, test_data.to_byte_array())
  
  // 验证GZIP格式标识
  assert_true(gzip_compressed[0] == 0x1F && gzip_compressed[1] == 0x8B) // GZIP魔数
  
  // 使用标准GZIP解压缩器解压缩
  let gzip_decompressed = Compressor::decompress(gzip_compressor, gzip_compressed)
  let gzip_decompressed_string = Bytearray::to_string(gzip_decompressed)
  assert_eq(gzip_decompressed_string, test_data)
  
  // 测试DEFLATE格式
  let deflate_compressor = DeflateCompressor::new()
  let deflate_compressed = Compressor::compress(deflate_compressor, test_data.to_byte_array())
  
  // DEFLATE格式没有特定的魔数，但应该能正常解压缩
  let deflate_decompressed = Compressor::decompress(deflate_compressor, deflate_compressed)
  let deflate_decompressed_string = Bytearray::to_string(deflate_decompressed)
  assert_eq(deflate_decompressed_string, test_data)
  
  // 测试跨格式解压缩（应该失败）
  let cross_format_result = Compressor::decompress(gzip_compressor, deflate_compressed)
  
  match cross_format_result {
    Ok(_) => assert_true(false) // 应该失败
    Err(FormatError) => assert_true(true) // 预期错误
    Err(_) => assert_true(false)
  }
  
  // 测试压缩级别兼容性
  let levels = [1, 6, 9]
  let compressed_at_different_levels = []
  
  for level in levels {
    let level_compressor = GzipCompressor::with_level(level)
    let compressed = Compressor::compress(level_compressor, test_data.to_byte_array())
    compressed_at_different_levels = compressed_at_different_levels.push(compressed)
  }
  
  // 所有不同级别压缩的数据都应该能被标准解压缩器解压缩
  for compressed in compressed_at_different_levels {
    let decompressed = Compressor::decompress(gzip_compressor, compressed)
    let decompressed_string = Bytearray::to_string(decompressed)
    assert_eq(decompressed_string, test_data)
  }
}