// Azimuth Telemetry System - Optimized Data Compression Tests
// 优化数据压缩测试用例，测试系统的数据压缩和解压缩功能

test "GZIP压缩算法测试" {
  // 创建GZIP压缩器
  let gzip_compressor = azimuth::GzipCompressor::new()
  
  // 测试文本数据压缩
  let text_data = "这是一段用于测试压缩性能的文本数据。它包含了中文字符和一些重复的内容，这样可以更好地测试压缩算法的效果。重复内容：测试数据、压缩算法、性能评估。"
  let compressed_data = gzip_compressor.compress(text_data)
  
  match compressed_data {
    Ok(compressed) => {
      // 验证压缩后的数据比原始数据小
      assert_true(compressed.length() < text_data.length())
      
      // 测试解压缩
      let decompressed_result = gzip_compressor.decompress(compressed)
      match decompressed_result {
        Ok(decompressed) => {
          assert_eq(decompressed, text_data)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试二进制数据压缩
  let binary_data = [0x01, 0x02, 0x03, 0x04, 0x05, 0x01, 0x02, 0x03, 0x04, 0x05, 0x01, 0x02, 0x03, 0x04, 0x05]
  let binary_compressed = gzip_compressor.compress_bytes(binary_data)
  
  match binary_compressed {
    Ok(compressed) => {
      // 验证压缩后的数据比原始数据小
      assert_true(compressed.length() < binary_data.length())
      
      // 测试解压缩
      let decompressed_result = gzip_compressor.decompress_bytes(compressed)
      match decompressed_result {
        Ok(decompressed) => {
          assert_eq(decompressed, binary_data)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试大文件压缩
  let large_text = "这是一段很长的文本数据，用于测试大文件的压缩效果。" * 1000
  let large_compressed = gzip_compressor.compress(large_text)
  
  match large_compressed {
    Ok(compressed) => {
      // 验证压缩率
      let compression_ratio = compressed.length() as Float / large_text.length() as Float
      assert_true(compression_ratio < 0.5) // 压缩率应该小于50%
      
      // 测试解压缩
      let decompressed_result = gzip_compressor.decompress(compressed)
      match decompressed_result {
        Ok(decompressed) => {
          assert_eq(decompressed, large_text)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

test "DEFLATE压缩算法测试" {
  // 创建DEFLATE压缩器
  let deflate_compressor = azimuth::DeflateCompressor::new()
  
  // 测试JSON数据压缩
  let json_data = "{\"metrics\":[{\"name\":\"cpu_usage\",\"value\":75.5,\"tags\":{\"host\":\"server1\"}},{\"name\":\"memory_usage\",\"value\":60.2,\"tags\":{\"host\":\"server1\"}}]}"
  let compressed_json = deflate_compressor.compress(json_data)
  
  match compressed_json {
    Ok(compressed) => {
      // 验证压缩后的数据比原始数据小
      assert_true(compressed.length() < json_data.length())
      
      // 测试解压缩
      let decompressed_result = deflate_compressor.decompress(compressed)
      match decompressed_result {
        Ok(decompressed) => {
          assert_eq(decompressed, json_data)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试重复数据压缩
  let repetitive_data = "ABCD1234" * 100
  let compressed_repetitive = deflate_compressor.compress(repetitive_data)
  
  match compressed_repetitive {
    Ok(compressed) => {
      // 重复数据应该有很高的压缩率
      let compression_ratio = compressed.length() as Float / repetitive_data.length() as Float
      assert_true(compression_ratio < 0.2) // 压缩率应该小于20%
      
      // 测试解压缩
      let decompressed_result = deflate_compressor.decompress(compressed)
      match decompressed_result {
        Ok(decompressed) => {
          assert_eq(decompressed, repetitive_data)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
}

test "LZ4高速压缩算法测试" {
  // 创建LZ4压缩器
  let lz4_compressor = azimuth::Lz4Compressor::new()
  
  // 测试高速压缩场景
  let telemetry_data = {
    "timestamp": 1609459200000,
    "metrics": [
      {"name": "cpu_usage", "value": 75.5, "tags": {"host": "server1"}},
      {"name": "memory_usage", "value": 60.2, "tags": {"host": "server1"}},
      {"name": "disk_usage", "value": 45.8, "tags": {"host": "server1"}}
    ],
    "spans": [
      {"trace_id": "trace-123", "span_id": "span-456", "operation": "db_query", "duration": 500},
      {"trace_id": "trace-123", "span_id": "span-789", "operation": "cache_lookup", "duration": 100}
    ]
  }
  
  let telemetry_json = azimuth::JsonSerializer::serialize(telemetry_data)
  match telemetry_json {
    Ok(json_str) => {
      let compressed_result = lz4_compressor.compress(json_str)
      match compressed_result {
        Ok(compressed) => {
          // LZ4专注于速度，压缩率可能不如GZIP
          assert_true(compressed.length() < json_str.length())
          
          // 测试解压缩
          let decompressed_result = lz4_compressor.decompress(compressed)
          match decompressed_result {
            Ok(decompressed) => {
              assert_eq(decompressed, json_str)
            }
            Err(_) => assert_true(false)
          }
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试流式压缩
  let stream_compressor = lz4_compressor.create_stream_compressor()
  
  // 分块压缩数据
  let chunk1 = "这是第一块数据"
  let chunk2 = "这是第二块数据"
  let chunk3 = "这是第三块数据"
  
  let compressed_chunk1 = stream_compressor.compress_chunk(chunk1)
  let compressed_chunk2 = stream_compressor.compress_chunk(chunk2)
  let compressed_chunk3 = stream_compressor.compress_chunk(chunk3)
  let final_compressed = stream_compressor.finish()
  
  // 合并所有压缩块
  let full_compressed = compressed_chunk1 + compressed_chunk2 + compressed_chunk3 + final_compressed
  
  // 测试流式解压缩
  let stream_decompressor = lz4_compressor.create_stream_decompressor()
  let decompressed_chunk1 = stream_decompressor.decompress_chunk(compressed_chunk1)
  let decompressed_chunk2 = stream_decompressor.decompress_chunk(compressed_chunk2)
  let decompressed_chunk3 = stream_decompressor.decompress_chunk(compressed_chunk3)
  let final_decompressed = stream_decompressor.finish()
  
  // 验证解压缩结果
  assert_eq(decompressed_chunk1 + decompressed_chunk2 + decompressed_chunk3 + final_decompressed, chunk1 + chunk2 + chunk3)
}

test "Brotli压缩算法测试" {
  // 创建Brotli压缩器
  let brotli_compressor = azimuth::BrotliCompressor::new()
  
  // 测试HTML内容压缩
  let html_content = "<!DOCTYPE html><html><head><title>测试页面</title></head><body><h1>这是一个测试页面</h1><p>用于测试Brotli压缩算法的效果。</p><div>内容区域</div></body></html>"
  let compressed_html = brotli_compressor.compress(html_content)
  
  match compressed_html {
    Ok(compressed) => {
      // 验证压缩后的数据比原始数据小
      assert_true(compressed.length() < html_content.length())
      
      // 测试解压缩
      let decompressed_result = brotli_compressor.decompress(compressed)
      match decompressed_result {
        Ok(decompressed) => {
          assert_eq(decompressed, html_content)
        }
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试不同压缩级别
  let test_data = "这是一段用于测试不同压缩级别的数据。" * 100
  
  // 测试最快压缩级别
  let fast_compressor = azimuth::BrotliCompressor::new_with_level(1)
  let fast_compressed = fast_compressor.compress(test_data)
  
  // 测试最佳压缩级别
  let best_compressor = azimuth::BrotliCompressor::new_with_level(11)
  let best_compressed = best_compressor.compress(test_data)
  
  match (fast_compressed, best_compressed) {
    (Ok(fast), Ok(best)) => {
      // 最佳压缩级别应该产生更小的文件
      assert_true(best.length() <= fast.length())
      
      // 验证两者都能正确解压缩
      let fast_decompressed = fast_compressor.decompress(fast)
      let best_decompressed = best_compressor.decompress(best)
      
      match (fast_decompressed, best_decompressed) {
        (Ok(fast_dec), Ok(best_dec)) => {
          assert_eq(fast_dec, test_data)
          assert_eq(best_dec, test_data)
        }
        _ => assert_true(false)
      }
    }
    _ => assert_true(false)
  }
}

test "压缩算法性能比较测试" {
  // 准备测试数据
  let test_data = {
    "telemetry": {
      "service": "azimuth",
      "version": "1.0.0",
      "metrics": [
        {"name": "cpu_usage", "value": 75.5, "timestamp": 1609459200000},
        {"name": "memory_usage", "value": 60.2, "timestamp": 1609459200000},
        {"name": "disk_usage", "value": 45.8, "timestamp": 1609459200000},
        {"name": "network_in", "value": 1024.5, "timestamp": 1609459200000},
        {"name": "network_out", "value": 2048.3, "timestamp": 1609459200000}
      ],
      "spans": [
        {"trace_id": "trace-123", "span_id": "span-456", "operation": "db_query", "duration": 500},
        {"trace_id": "trace-123", "span_id": "span-789", "operation": "cache_lookup", "duration": 100},
        {"trace_id": "trace-123", "span_id": "span-101", "operation": "api_call", "duration": 200}
      ]
    }
  }
  
  let json_data = azimuth::JsonSerializer::serialize(test_data)
  match json_data {
    Ok(json_str) => {
      // 测试不同压缩算法的性能
      let algorithms = [
        ("GZIP", azimuth::CompressionAlgorithm::Gzip),
        ("DEFLATE", azimuth::CompressionAlgorithm::Deflate),
        ("LZ4", azimuth::CompressionAlgorithm::Lz4),
        ("Brotli", azimuth::CompressionAlgorithm::Brotli)
      ]
      
      let mut compression_results = []
      
      for (name, algorithm) in algorithms {
        let compressor = azimuth::CompressorFactory::create(algorithm)
        
        // 测量压缩时间
        let start_time = azimuth::Time::now()
        let compressed_result = compressor.compress(json_str)
        let compression_time = azimuth::Time::now() - start_time
        
        match compressed_result {
          Ok(compressed) => {
            // 测量解压缩时间
            let start_decompress_time = azimuth::Time::now()
            let decompressed_result = compressor.decompress(compressed)
            let decompression_time = azimuth::Time::now() - start_decompress_time
            
            match decompressed_result {
              Ok(decompressed) => {
                assert_eq(decompressed, json_str)
                
                // 计算压缩率
                let compression_ratio = compressed.length() as Float / json_str.length() as Float
                
                compression_results = compression_results + [{
                  "algorithm": name,
                  "original_size": json_str.length(),
                  "compressed_size": compressed.length(),
                  "compression_ratio": compression_ratio,
                  "compression_time": compression_time,
                  "decompression_time": decompression_time
                }]
              }
              Err(_) => assert_true(false)
            }
          }
          Err(_) => assert_true(false)
        }
      }
      
      // 验证所有算法都产生了结果
      assert_eq(compression_results.length(), 4)
      
      // 验证压缩率
      for result in compression_results {
        assert_true(result.compression_ratio < 1.0) // 所有压缩率都应该小于100%
        assert_true(result.compressed_size < result.original_size) // 压缩后应该更小
      }
      
      // 验证压缩时间合理性
      for result in compression_results {
        assert_true(result.compression_time > 0) // 压缩时间应该大于0
        assert_true(result.decompression_time > 0) // 解压缩时间应该大于0
      }
    }
    Err(_) => assert_true(false)
  }
}

test "自适应压缩策略测试" {
  // 创建自适应压缩器
  let adaptive_compressor = azimuth::AdaptiveCompressor::new()
  
  // 测试小数据压缩（应该使用快速算法）
  let small_data = "小数据测试"
  let small_compressed = adaptive_compressor.compress(small_data)
  
  match small_compressed {
    Ok((compressed, algorithm)) => {
      // 小数据应该使用快速压缩算法
      match algorithm {
        azimuth::CompressionAlgorithm::Lz4 => assert_true(true)
        _ => assert_true(false)
      }
      
      // 验证压缩和解压缩
      let decompressed = adaptive_compressor.decompress(compressed, algorithm)
      match decompressed {
        Ok(data) => assert_eq(data, small_data)
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试中等数据压缩（应该使用平衡算法）
  let medium_data = "中等数据测试" * 100
  let medium_compressed = adaptive_compressor.compress(medium_data)
  
  match medium_compressed {
    Ok((compressed, algorithm)) => {
      // 中等数据应该使用平衡压缩算法
      match algorithm {
        azimuth::CompressionAlgorithm::Gzip => assert_true(true)
        azimuth::CompressionAlgorithm::Deflate => assert_true(true)
        _ => assert_true(false)
      }
      
      // 验证压缩和解压缩
      let decompressed = adaptive_compressor.decompress(compressed, algorithm)
      match decompressed {
        Ok(data) => assert_eq(data, medium_data)
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试大数据压缩（应该使用最佳压缩算法）
  let large_data = "大数据测试" * 1000
  let large_compressed = adaptive_compressor.compress(large_data)
  
  match large_compressed {
    Ok((compressed, algorithm)) => {
      // 大数据应该使用最佳压缩算法
      match algorithm {
        azimuth::CompressionAlgorithm::Brotli => assert_true(true)
        azimuth::CompressionAlgorithm::Gzip => assert_true(true)
        _ => assert_true(false)
      }
      
      // 验证压缩和解压缩
      let decompressed = adaptive_compressor.decompress(compressed, algorithm)
      match decompressed {
        Ok(data) => assert_eq(data, large_data)
        Err(_) => assert_true(false)
      }
    }
    Err(_) => assert_true(false)
  }
  
  // 测试压缩统计
  let stats = adaptive_compressor.get_statistics()
  assert_true(stats.total_compressions > 0)
  assert_true(stats.total_decompressions > 0)
  assert_true(stats.average_compression_time > 0)
  assert_true(stats.average_decompression_time > 0)
}

test "压缩数据完整性测试" {
  // 创建压缩器
  let compressor = azimuth::GzipCompressor::new()
  
  // 测试各种数据类型的压缩和解压缩
  let test_cases = [
    ("空字符串", ""),
    ("短文本", "Hello"),
    ("长文本", "这是一段很长的文本数据，用于测试压缩和解压缩的完整性。" * 100),
    ("特殊字符", "!@#$%^&*()_+-=[]{}|;':\",./<>?"),
    ("中文字符", "这是中文测试数据，包含各种汉字和符号。"),
    ("日文字符", "これは日本語のテストデータです。"),
    ("混合语言", "Hello 世界 こんにちは 안녕하세요"),
    ("数字", "1234567890" * 100),
    ("JSON", "{\"key\":\"value\",\"array\":[1,2,3],\"nested\":{\"inner\":true}}"),
    ("XML", "<root><element attr=\"value\">内容</element></root>"),
    ("二进制数据", [0x00, 0x01, 0x02, 0xFF, 0xFE, 0xFD] * 100)
  ]
  
  for (name, data) in test_cases {
    let compressed_result = match data {
      String(s) => compressor.compress(s),
      Bytes(b) => compressor.compress_bytes(b),
      _ => continue
    }
    
    match compressed_result {
      Ok(compressed) => {
        let decompressed_result = match data {
          String(_) => compressor.decompress(compressed),
          Bytes(_) => compressor.decompress_bytes(compressed),
          _ => continue
        }
        
        match decompressed_result {
          Ok(decompressed) => {
            match data {
              String(s) => assert_eq(decompressed, s),
              Bytes(b) => assert_eq(decompressed, b),
              _ => assert_true(false)
            }
          }
          Err(_) => assert_true(false)
        }
      }
      Err(_) => assert_true(false)
    }
  }
}

test "压缩数据损坏恢复测试" {
  // 创建压缩器
  let compressor = azimuth::GzipCompressor::new()
  
  // 压缩测试数据
  let original_data = "这是用于测试损坏恢复的数据。" * 100
  let compressed_result = compressor.compress(original_data)
  
  match compressed_result {
    Ok(compressed) => {
      // 测试完整数据的解压缩
      let valid_result = compressor.decompress(compressed)
      match valid_result {
        Ok(decompressed) => assert_eq(decompressed, original_data)
        Err(_) => assert_true(false)
      }
      
      // 测试截断数据的解压缩（应该失败）
      let truncated_length = compressed.length() / 2
      let truncated_data = compressed.subarray(0, truncated_length)
      let truncated_result = compressor.decompress(truncated_data)
      match truncated_result {
        Ok(_) => assert_true(false) // 不应该成功
        Err(error) => {
          match error {
            azimuth::CompressionError::InvalidData => assert_true(true)
            azimuth::CompressionError::ChecksumMismatch => assert_true(true)
            _ => assert_true(false)
          }
        }
      }
      
      // 测试损坏数据的解压缩（应该失败）
      let mut corrupted_data = compressed
      if corrupted_data.length() > 10 {
        corrupted_data[5] = corrupted_data[5] + 1 // 修改一个字节
      }
      
      let corrupted_result = compressor.decompress(corrupted_data)
      match corrupted_result {
        Ok(_) => assert_true(false) // 不应该成功
        Err(error) => {
          match error {
            azimuth::CompressionError::InvalidData => assert_true(true)
            azimuth::CompressionError::ChecksumMismatch => assert_true(true)
            _ => assert_true(false)
          }
        }
      }
    }
    Err(_) => assert_true(false)
  }
}

test "压缩流处理测试" {
  // 创建压缩流
  let compression_stream = azimuth::CompressionStream::new(azimuth::CompressionAlgorithm::Gzip)
  
  // 测试分块压缩
  let chunks = [
    "这是第一块数据，",
    "这是第二块数据，",
    "这是第三块数据，",
    "这是最后一块数据。"
  ]
  
  let mut compressed_chunks = []
  for chunk in chunks {
    let compressed_chunk = compression_stream.compress(chunk)
    compressed_chunks = compressed_chunks + [compressed_chunk]
  }
  
  // 完成压缩
  let final_chunk = compression_stream.finish()
  compressed_chunks = compressed_chunks + [final_chunk]
  
  // 合并所有压缩块
  let mut full_compressed = []
  for chunk in compressed_chunks {
    full_compressed = full_compressed + chunk
  }
  
  // 创建解压缩流
  let decompression_stream = azimuth::DecompressionStream::new(azimuth::CompressionAlgorithm::Gzip)
  
  // 测试分块解压缩
  let mut decompressed_chunks = []
  for chunk in compressed_chunks {
    let decompressed_chunk = decompression_stream.decompress(chunk)
    decompressed_chunks = decompressed_chunks + [decompressed_chunk]
  }
  
  // 完成解压缩
  let final_decompressed = decompression_stream.finish()
  decompressed_chunks = decompressed_chunks + [final_decompressed]
  
  // 验证解压缩结果
  let mut full_decompressed = ""
  for chunk in decompressed_chunks {
    full_decompressed = full_decompressed + chunk
  }
  
  let expected_data = chunks.join("")
  assert_eq(full_decompressed, expected_data)
  
  // 测试大文件流式压缩
  let large_stream = azimuth::CompressionStream::new(azimuth::CompressionAlgorithm::Gzip)
  let file_data = "这是模拟的大文件数据。" * 10000
  
  // 分块处理大文件
  let chunk_size = 1024
  let mut file_compressed = []
  
  for i in 0; i < file_data.length(); i = i + chunk_size {
    let end = if i + chunk_size < file_data.length() { i + chunk_size } else { file_data.length() }
    let chunk = file_data.subarray(i, end - i)
    let compressed_chunk = large_stream.compress(chunk)
    file_compressed = file_compressed + compressed_chunk
  }
  
  let file_final = large_stream.finish()
  file_compressed = file_compressed + file_final
  
  // 验证压缩率
  let compression_ratio = file_compressed.length() as Float / file_data.length() as Float
  assert_true(compression_ratio < 0.5) // 压缩率应该小于50%
}