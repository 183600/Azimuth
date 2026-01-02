// Azimuth Data Compression and Transmission Optimization Tests
// 数据压缩和传输优化测试用例 - 专注于数据传输效率和压缩算法

// Test 1: 基础数据压缩算法测试
test "basic data compression algorithms" {
  // 创建测试数据
  let original_data = "azimuth telemetry data compression test string with repeated patterns azimuth telemetry data"
  let original_bytes = String::to_bytes(original_data)
  
  // 测试GZIP压缩
  let gzip_compressor = GzipCompressor::new()
  let gzip_compressed = Compressor::compress(gzip_compressor, original_bytes)
  let gzip_decompressed = Compressor::decompress(gzip_compressor, gzip_compressed)
  
  assert_true(gzip_compressed.length() < original_bytes.length()) // 压缩后应该更小
  assert_eq(gzip_decompressed, original_bytes) // 解压后应该相同
  
  // 测试LZ4压缩
  let lz4_compressor = LZ4Compressor::new()
  let lz4_compressed = Compressor::compress(lz4_compressor, original_bytes)
  let lz4_decompressed = Compressor::decompress(lz4_compressor, lz4_compressed)
  
  assert_true(lz4_compressed.length() < original_bytes.length())
  assert_eq(lz4_decompressed, original_bytes)
  
  // 比较压缩效率
  let gzip_ratio = gzip_compressed.length().to_float() / original_bytes.length().to_float()
  let lz4_ratio = lz4_compressed.length().to_float() / original_bytes.length().to_float()
  
  assert_true(gzip_ratio < 1.0)
  assert_true(lz4_ratio < 1.0)
  
  // 验证压缩比
  let compression_report = CompressionReport::compare([
    ("gzip", gzip_compressed.length()),
    ("lz4", lz4_compressed.length())
  ], original_bytes.length())
  
  assert_true(compression_report.best_algorithm != "")
  assert_true(compression_report.compression_ratio > 0.0)
}

// Test 2: 批量数据压缩优化
test "batch data compression optimization" {
  let batch_compressor = BatchCompressor::new(1024 * 1024) // 1MB批次大小
  
  // 创建大量小数据块
  let data_blocks = []
  for i in 0..<1000 {
    let block_data = "block_" + i.to_string() + "_repeated_pattern_for_compression_efficiency"
    data_blocks = data_blocks.push(String::to_bytes(block_data))
  }
  
  // 测试单独压缩
  let start_time = Time::now()
  let individually_compressed = []
  for block in data_blocks {
    let compressed = GzipCompressor::compress(block)
    individually_compressed = individually_compressed.push(compressed)
  }
  let individual_time = Time::now() - start_time
  
  // 测试批量压缩
  let batch_start = Time::now()
  let batch_compressed = BatchCompressor::compress(batch_compressor, data_blocks)
  let batch_time = Time::now() - batch_start
  
  // 验证批量压缩效率
  assert_true(batch_time <= individual_time) // 批量应该更快或相等
  
  // 验证压缩结果
  let batch_decompressed = BatchCompressor::decompress(batch_compressor, batch_compressed)
  assert_eq(batch_decompressed.length(), data_blocks.length())
  
  for i in 0..<data_blocks.length() {
    assert_eq(batch_decompressed[i], data_blocks[i])
  }
  
  // 比较压缩率
  let individual_total_size = individually_compressed.reduce(fn(acc, block) { acc + block.length() }, 0)
  let batch_total_size = batch_compressed.reduce(fn(acc, block) { acc + block.length() }, 0)
  
  let batch_efficiency = individual_total_size.to_float() / batch_total_size.to_float()
  assert_true(batch_efficiency >= 1.0) // 批量应该更高效
}

// Test 3: 自适应压缩策略
test "adaptive compression strategy" {
  let adaptive_compressor = AdaptiveCompressor::new()
  
  // 测试不同类型的数据
  let text_data = String::to_bytes("this is repetitive text data with many repeated words and patterns")
  let binary_data = [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07]
  let json_data = String::to_bytes("{\"key\":\"value\",\"array\":[1,2,3,4,5],\"nested\":{\"data\":\"test\"}}")
  let mixed_data = String::to_bytes("mixed content with text 12345 and symbols !@#$% and patterns repeated")
  
  // 压缩不同类型的数据
  let text_compressed = AdaptiveCompressor::compress(adaptive_compressor, text_data)
  let binary_compressed = AdaptiveCompressor::compress(adaptive_compressor, binary_data)
  let json_compressed = AdaptiveCompressor::compress(adaptive_compressor, json_data)
  let mixed_compressed = AdaptiveCompressor::compress(adaptive_compressor, mixed_data)
  
  // 验证压缩算法选择
  let text_strategy = AdaptiveCompressor::get_used_strategy(adaptive_compressor, text_data)
  let binary_strategy = AdaptiveCompressor::get_used_strategy(adaptive_compressor, binary_data)
  let json_strategy = AdaptiveCompressor::get_used_strategy(adaptive_compressor, json_data)
  
  assert_true(text_strategy != "")
  assert_true(binary_strategy != "")
  assert_true(json_strategy != "")
  
  // 验证解压正确性
  let text_decompressed = AdaptiveCompressor::decompress(adaptive_compressor, text_compressed)
  let binary_decompressed = AdaptiveCompressor::decompress(adaptive_compressor, binary_compressed)
  let json_decompressed = AdaptiveCompressor::decompress(adaptive_compressor, json_compressed)
  
  assert_eq(text_decompressed, text_data)
  assert_eq(binary_decompressed, binary_data)
  assert_eq(json_decompressed, json_data)
  
  // 测试自适应学习
  let learning_data = []
  for i in 0..<100 {
    let pattern_data = "learning_pattern_" + (i % 10).to_string() + "_repeated_content"
    learning_data = learning_data.push(String::to_bytes(pattern_data))
  }
  
  AdaptiveCompressor::learn_patterns(adaptive_compressor, learning_data)
  
  // 压缩相似数据应该更高效
  let test_data = String::to_bytes("learning_pattern_5_repeated_content_new_suffix")
  let compressed_before = AdaptiveCompressor::compress(adaptive_compressor, test_data)
  
  AdaptiveCompressor::learn_patterns(adaptive_compressor, [test_data])
  let compressed_after = AdaptiveCompressor::compress(adaptive_compressor, test_data)
  
  assert_true(compressed_after.length() <= compressed_before.length())
}

// Test 4: 网络传输优化
test "network transmission optimization" {
  let transmitter = NetworkTransmitter::new()
  
  // 创建测试数据包
  let packets = []
  for i in 0..<50 {
    let payload = "packet_payload_" + i.to_string() + "_with_additional_data_for_realistic_size"
    let packet = DataPacket::new(i, String::to_bytes(payload))
    packets = packets.push(packet)
  }
  
  // 测试数据包合并
  let merged_packet = NetworkTransmitter::merge_packets(transmitter, packets, 1400) // MTU大小
  assert_true(merged_packet.fragments.length() <= packets.length())
  
  // 测试压缩传输
  let compressed_transmission = NetworkTransmitter::compress_and_send(transmitter, packets)
  let uncompressed_size = packets.reduce(fn(acc, packet) { acc + packet.payload.length() }, 0)
  let compressed_size = compressed_transmission.total_compressed_size
  
  assert_true(compressed_size < uncompressed_size)
  
  // 测试传输恢复
  let recovered_packets = NetworkTransmitter::receive_and_decompress(transmitter, compressed_transmission)
  assert_eq(recovered_packets.length(), packets.length())
  
  for i in 0..<packets.length() {
    assert_eq(recovered_packets[i].id, packets[i].id)
    assert_eq(recovered_packets[i].payload, packets[i].payload)
  }
  
  // 测试带宽估算
  let bandwidth_info = NetworkTransmitter::estimate_bandwidth(transmitter, packets)
  assert_true(bandwidth_info.required_bandwidth > 0)
  assert_true(bandwidth_info.compressed_bandwidth > 0)
  assert_true(bandwidth_info.compression_ratio > 1.0)
}

// Test 5: 增量数据压缩
test "incremental data compression" {
  let incremental_compressor = IncrementalCompressor::new()
  
  // 创建基础数据集
  let base_data = "base telemetry data with common attributes like service.name azimuth and version 1.0.0"
  let base_compressed = IncrementalCompressor::compress_base(incremental_compressor, String::to_bytes(base_data))
  
  // 创建增量数据
  let delta_data = "base telemetry data with common attributes like service.name azimuth and version 1.0.1"
  let delta_compressed = IncrementalCompressor::compress_delta(incremental_compressor, String::to_bytes(delta_data))
  
  // 验证增量压缩效率
  let full_compressed = GzipCompressor::compress(String::to_bytes(delta_data))
  assert_true(delta_compressed.length() < full_compressed.length())
  
  // 解压并验证
  let decompressed = IncrementalCompressor::decompress(incremental_compressor, base_compressed, delta_compressed)
  assert_eq(decompressed, String::to_bytes(delta_data))
  
  // 测试多增量压缩
  let deltas = []
  for i in 0..<10 {
    let version_data = "base telemetry data with common attributes like service.name azimuth and version 1.0." + i.to_string()
    let delta = IncrementalCompressor::compress_delta(incremental_compressor, String::to_bytes(version_data))
    deltas = deltas.push(delta)
  }
  
  // 验证增量大小
  let total_delta_size = deltas.reduce(fn(acc, delta) { acc + delta.length() }, 0)
  let total_full_size = deltas.length() * full_compressed.length()
  
  assert_true(total_delta_size < total_full_size * 0.5) // 增量应该显著更小
}

// Test 6: 流式压缩
test "streaming compression" {
  let stream_compressor = StreamCompressor::new()
  StreamCompressor::initialize(stream_compressor)
  
  // 模拟数据流
  let data_chunks = []
  for i in 0..<100 {
    let chunk_data = "streaming_chunk_" + i.to_string() + "_with_repeated_content_for_compression"
    data_chunks = data_chunks.push(String::to_bytes(chunk_data))
  }
  
  // 流式压缩
  let compressed_stream = []
  for chunk in data_chunks {
    let compressed_chunk = StreamCompressor::compress_chunk(stream_compressor, chunk)
    compressed_stream = compressed_stream.push(compressed_chunk)
  }
  
  // 完成压缩
  let final_data = StreamCompressor::finalize(stream_compressor)
  
  // 验证压缩大小
  let original_size = data_chunks.reduce(fn(acc, chunk) { acc + chunk.length() }, 0)
  let compressed_size = compressed_stream.reduce(fn(acc, chunk) { acc + chunk.length() }, 0) + final_data.length()
  
  assert_true(compressed_size < original_size)
  
  // 流式解压
  let stream_decompressor = StreamDecompressor::new()
  StreamDecompressor::initialize(stream_decompressor)
  
  let decompressed_chunks = []
  for chunk in compressed_stream {
    let decompressed = StreamDecompressor::decompress_chunk(stream_decompressor, chunk)
    decompressed_chunks = decompressed_chunks.push(decompressed)
  }
  
  let final_decompressed = StreamDecompressor::finalize(stream_decompressor)
  
  // 重组数据并验证
  let reconstructed = decompressed_chunks.reduce(fn(acc, chunk) { acc + chunk }, final_decompressed)
  let original_combined = data_chunks.reduce(fn(acc, chunk) { acc + chunk }, [])
  
  assert_eq(reconstructed, original_combined)
  
  // 验证流式处理的内存效率
  let memory_peak = StreamCompressor::get_peak_memory_usage(stream_compressor)
  let total_size = original_size
  assert_true(memory_peak < total_size * 0.2) // 峰值内存应该远小于总数据大小
}

// Test 7: 压缩质量与性能平衡
test "compression quality and performance balance" {
  let quality_tester = CompressionQualityTester::new()
  
  // 创建不同复杂度的测试数据
  let simple_data = String::to_bytes("simple repeated text pattern simple repeated text pattern")
  let complex_data = String::to_bytes("complex data with varied content 12345 !@#$% mixed patterns and structures")
  let large_data = []
  for i in 0..<1000 {
    let content = "large_data_entry_" + i.to_string() + "_with_content_" + (i % 100).to_string()
    large_data = large_data.push(String::to_bytes(content))
  }
  let large_combined = large_data.reduce(fn(acc, chunk) { acc + chunk }, [])
  
  // 测试不同压缩级别
  let compression_levels = [1, 3, 5, 7, 9]
  let performance_results = []
  
  for level in compression_levels {
    let compressor = GzipCompressor::with_level(level)
    
    let start_time = Time::now()
    let simple_compressed = Compressor::compress(compressor, simple_data)
    let complex_compressed = Compressor::compress(compressor, complex_data)
    let large_compressed = Compressor::compress(compressor, large_combined)
    let compression_time = Time::now() - start_time
    
    let result = {
      "level": level,
      "simple_ratio": simple_compressed.length().to_float() / simple_data.length().to_float(),
      "complex_ratio": complex_compressed.length().to_float() / complex_data.length().to_float(),
      "large_ratio": large_compressed.length().to_float() / large_combined.length().to_float(),
      "time_ms": compression_time
    }
    performance_results = performance_results.push(result)
  }
  
  // 验证压缩级别与效果的关系
  for i in 1..<performance_results.length() {
    let prev = performance_results[i - 1]
    let curr = performance_results[i]
    
    // 更高级别应该有更好的压缩率
    assert_true(curr.simple_ratio <= prev.simple_ratio)
    assert_true(curr.complex_ratio <= prev.complex_ratio)
    assert_true(curr.large_ratio <= prev.large_ratio)
    
    // 但可能需要更多时间
    assert_true(curr.time_ms >= prev.time_ms)
  }
  
  // 找到最佳的平衡点
  let optimal_level = CompressionQualityTester::find_optimal_level(quality_tester, performance_results)
  assert_true(optimal_level >= 1 && optimal_level <= 9)
}

// Test 8: 并行压缩处理
test "parallel compression processing" {
  let parallel_compressor = ParallelCompressor::new(4) // 4个并行工作线程
  
  // 创建大量数据块
  let data_blocks = []
  for i in 0..<200 {
    let block_content = "parallel_block_" + i.to_string() + "_with_content_for_compression_testing"
    data_blocks = data_blocks.push(String::to_bytes(block_content))
  }
  
  // 测试顺序压缩
  let sequential_start = Time::now()
  let sequential_results = []
  for block in data_blocks {
    let compressed = GzipCompressor::compress(block)
    sequential_results = sequential_results.push(compressed)
  }
  let sequential_time = Time::now() - sequential_start
  
  // 测试并行压缩
  let parallel_start = Time::now()
  let parallel_results = ParallelCompressor::compress_blocks(parallel_compressor, data_blocks)
  let parallel_time = Time::now() - parallel_start
  
  // 验证并行处理效率
  assert_true(parallel_results.length() == data_blocks.length())
  assert_true(parallel_time < sequential_time) // 并行应该更快
  
  // 验证压缩结果正确性
  for i in 0..<parallel_results.length() {
    let decompressed = GzipCompressor::decompress(parallel_results[i])
    assert_eq(decompressed, data_blocks[i])
  }
  
  // 测试负载均衡
  let load_balance_stats = ParallelCompressor::get_load_balance_stats(parallel_compressor)
  assert_true(load_balance_stats.workers_used > 1)
  assert_true(load_balance_stats.load_variance < 0.3) // 负载应该相对均衡
}

// Test 9: 压缩数据完整性验证
test "compressed data integrity verification" {
  let integrity_verifier = CompressionIntegrityVerifier::new()
  
  // 创建测试数据
  let test_data = "integrity test data with specific content for verification 12345 !@#$%"
  let data_bytes = String::to_bytes(test_data)
  
  // 压缩数据
  let compressor = GzipCompressor::new()
  let compressed = Compressor::compress(compressor, data_bytes)
  
  // 添加校验和
  let checksum = IntegrityVerifier::calculate_checksum(integrity_verifier, compressed)
  let compressed_with_checksum = IntegrityVerifier::add_checksum(integrity_verifier, compressed, checksum)
  
  // 验证完整性
  let is_valid = IntegrityVerifier::verify(integrity_verifier, compressed_with_checksum)
  assert_true(is_valid)
  
  // 模拟数据损坏
  let corrupted_data = IntegrityVerifier::corrupt_byte(integrity_verifier, compressed_with_checksum, 10)
  let is_corrupted = IntegrityVerifier::verify(integrity_verifier, corrupted_data)
  assert_false(is_corrupted)
  
  // 测试自动修复
  let repair_result = IntegrityVerifier::attempt_repair(integrity_verifier, corrupted_data)
  match repair_result {
    Ok(repaired_data) => {
      let is_repaired = IntegrityVerifier::verify(integrity_verifier, repaired_data)
      assert_true(is_repaired)
      
      // 验证修复后的数据可以正确解压
      let decompressed = Compressor::decompress(compressor, IntegrityVerifier::extract_payload(integrity_verifier, repaired_data))
      assert_eq(decompressed, data_bytes)
    }
    Err(_) => assert_true(false)
  }
  
  // 测试多重验证
  let multi_verified = IntegrityVerifier::multi_level_verify(integrity_verifier, compressed_with_checksum)
  assert_true(multi_verified.all_checks_passed)
  assert_true(multi_verified.check_count > 1)
}

// Test 10: 压缩策略优化
test "compression strategy optimization" {
  let optimizer = CompressionStrategyOptimizer::new()
  
  // 创建不同类型的测试数据集
  let datasets = [
    ("text", String::to_bytes("repeated text content with patterns for compression efficiency testing")),
    ("json", String::to_bytes("{\"key\":\"value\",\"array\":[1,2,3],\"nested\":{\"data\":\"test\"}}")),
    ("binary", [0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD, 0x00, 0x01]),
    ("mixed", String::to_bytes("mixed content 12345 symbols !@#$% patterns repeated mixed content"))
  ]
  
  // 测试不同压缩策略
  let strategies = ["gzip", "lz4", "brotli", "adaptive"]
  let optimization_results = []
  
  for (data_type, data) in datasets {
    for strategy in strategies {
      let result = CompressionStrategyOptimizer::test_strategy(optimizer, strategy, data)
      optimization_results = optimization_results.push({
        "data_type": data_type,
        "strategy": strategy,
        "compression_ratio": result.compression_ratio,
        "compression_time": result.compression_time,
        "decompression_time": result.decompression_time,
        "memory_usage": result.memory_usage
      })
    }
  }
  
  // 找到每种数据类型的最佳策略
  let best_strategies = CompressionStrategyOptimizer::find_best_strategies(optimizer, optimization_results)
  assert_true(best_strategies.length() == datasets.length())
  
  // 验证优化结果
  for (data_type, strategy) in best_strategies {
    let strategy_results = optimization_results.filter(fn(r) { r.data_type == data_type && r.strategy == strategy })
    assert_true(strategy_results.length() == 1)
    
    let best_result = strategy_results[0]
    
    // 验证该策略确实是最佳的
    let type_results = optimization_results.filter(fn(r) { r.data_type == data_type })
    for result in type_results {
      if result.strategy != strategy {
        // 最佳策略应该有更好的综合评分
        let best_score = CompressionStrategyOptimizer::calculate_score(optimizer, best_result)
        let other_score = CompressionStrategyOptimizer::calculate_score(optimizer, result)
        assert_true(best_score >= other_score)
      }
    }
  }
  
  // 测试自动策略选择
  let auto_selector = AutoCompressionSelector::new(best_strategies)
  for (data_type, data) in datasets {
    let selected_strategy = AutoCompressionSelector::select_strategy(auto_selector, data_type)
    let expected_strategy = best_strategies.filter(fn(t, s) { t == data_type })[0].1
    assert_eq(selected_strategy, expected_strategy)
  }
}