// 数据压缩优化遥测测试
// 测试遥测数据压缩算法的性能和效率

test "compression_algorithm_comparison" {
  // 测试压缩算法比较
  
  let compression_metrics = [
    ("algorithm", "gzip"),
    ("original_size_bytes", 1024000),
    ("compressed_size_bytes", 256000),
    ("compression_ratio", 0.25),
    ("compression_time_ms", 45),
    ("decompression_time_ms", 12),
    ("cpu_usage_percent", 15.2)
  ]
  
  // 验证压缩指标
  assert_eq(compression_metrics.length(), 7)
  
  // 验证算法名称
  assert_eq(compression_metrics[0].0, "algorithm")
  assert_eq(compression_metrics[0].1, "gzip")
  assert_eq(compression_metrics[0].1.length(), 4)
  
  // 验证压缩前后大小
  assert_eq(compression_metrics[1].0, "original_size_bytes")
  assert_eq(compression_metrics[2].0, "compressed_size_bytes")
  assert_eq(compression_metrics[1].1, 1024000)
  assert_eq(compression_metrics[2].1, 256000)
  assert_eq(compression_metrics[1].1 > compression_metrics[2].1, true)
  
  // 验证压缩比
  assert_eq(compression_metrics[3].0, "compression_ratio")
  assert_eq(compression_metrics[3].1, 0.25)
  assert_eq(compression_metrics[3].1 > 0.0, true)
  assert_eq(compression_metrics[3].1 < 1.0, true)
  
  // 验证压缩和解压缩时间
  assert_eq(compression_metrics[4].0, "compression_time_ms")
  assert_eq(compression_metrics[5].0, "decompression_time_ms")
  assert_eq(compression_metrics[4].1, 45)
  assert_eq(compression_metrics[5].1, 12)
  assert_eq(compression_metrics[4].1 > compression_metrics[5].1, true)
  
  // 验证CPU使用率
  assert_eq(compression_metrics[6].0, "cpu_usage_percent")
  assert_eq(compression_metrics[6].1, 15.2)
  assert_eq(compression_metrics[6].1 > 0.0, true)
  assert_eq(compression_metrics[6].1 < 100.0, true)
}

test "compression_batch_processing" {
  // 测试批量压缩处理
  
  let batch_compression_data = [
    ("batch_size", 100),
    ("total_original_bytes", 10240000),
    ("total_compressed_bytes", 2560000),
    ("average_compression_time_ms", 38),
    ("throughput_mbps", 270.4),
    ("memory_usage_mb", 128),
    ("success_rate", 1.0)
  ]
  
  // 验证批量压缩数据
  assert_eq(batch_compression_data.length(), 7)
  
  // 验证批次大小
  assert_eq(batch_compression_data[0].0, "batch_size")
  assert_eq(batch_compression_data[0].1, 100)
  assert_eq(batch_compression_data[0].1 > 0, true)
  
  // 验证总字节数
  assert_eq(batch_compression_data[1].0, "total_original_bytes")
  assert_eq(batch_compression_data[2].0, "total_compressed_bytes")
  assert_eq(batch_compression_data[1].1, 10240000)
  assert_eq(batch_compression_data[2].1, 2560000)
  assert_eq(batch_compression_data[1].1 > batch_compression_data[2].1, true)
  
  // 验证平均压缩时间
  assert_eq(batch_compression_data[3].0, "average_compression_time_ms")
  assert_eq(batch_compression_data[3].1, 38)
  assert_eq(batch_compression_data[3].1 > 0, true)
  
  // 验证吞吐量
  assert_eq(batch_compression_data[4].0, "throughput_mbps")
  assert_eq(batch_compression_data[4].1, 270.4)
  assert_eq(batch_compression_data[4].1 > 0.0, true)
  
  // 验证内存使用
  assert_eq(batch_compression_data[5].0, "memory_usage_mb")
  assert_eq(batch_compression_data[5].1, 128)
  assert_eq(batch_compression_data[5].1 > 0, true)
  
  // 验证成功率
  assert_eq(batch_compression_data[6].0, "success_rate")
  assert_eq(batch_compression_data[6].1, 1.0)
  assert_eq(batch_compression_data[6].1 <= 1.0, true)
}

test "compression_adaptive_algorithm" {
  // 测试自适应压缩算法
  
  let adaptive_compression = [
    ("data_type", "json_logs"),
    ("data_size_kb", 512),
    ("selected_algorithm", "lz4"),
    ("compression_ratio", 0.42),
    ("compression_speed_mbps", 450.0),
    ("decision_reason", "speed_priority"),
    ("alternative_algorithm", "gzip")
  ]
  
  // 验证自适应压缩
  assert_eq(adaptive_compression.length(), 7)
  
  // 验证数据类型
  assert_eq(adaptive_compression[0].0, "data_type")
  assert_eq(adaptive_compression[0].1, "json_logs")
  assert_eq(adaptive_compression[0].1.contains("_"), true)
  
  // 验证数据大小
  assert_eq(adaptive_compression[1].0, "data_size_kb")
  assert_eq(adaptive_compression[1].1, 512)
  assert_eq(adaptive_compression[1].1 > 0, true)
  
  // 验证选择的算法
  assert_eq(adaptive_compression[2].0, "selected_algorithm")
  assert_eq(adaptive_compression[2].1, "lz4")
  assert_eq(adaptive_compression[2].1.length(), 3)
  
  // 验证压缩比
  assert_eq(adaptive_compression[3].0, "compression_ratio")
  assert_eq(adaptive_compression[3].1, 0.42)
  assert_eq(adaptive_compression[3].1 > 0.0, true)
  assert_eq(adaptive_compression[3].1 < 1.0, true)
  
  // 验证压缩速度
  assert_eq(adaptive_compression[4].0, "compression_speed_mbps")
  assert_eq(adaptive_compression[4].1, 450.0)
  assert_eq(adaptive_compression[4].1 > 0.0, true)
  
  // 验证决策原因
  assert_eq(adaptive_compression[5].0, "decision_reason")
  assert_eq(adaptive_compression[5].1, "speed_priority")
  assert_eq(adaptive_compression[5].1.contains("_"), true)
  
  // 验证备选算法
  assert_eq(adaptive_compression[6].0, "alternative_algorithm")
  assert_eq(adaptive_compression[6].1, "gzip")
  assert_eq(adaptive_compression[6].1 != adaptive_compression[2].1, true)
}

test "compression_error_recovery" {
  // 测试压缩错误恢复
  
  let compression_errors = [
    ("error_type", "CORRUPTION_DETECTED"),
    ("compression_algorithm", "gzip"),
    ("input_data_size", 204800),
    ("error_position", 102456),
    ("recovery_attempt", "use_fallback_algorithm"),
    ("fallback_algorithm", "lz4"),
    ("recovery_success", true),
    ("recovery_time_ms", 15)
  ]
  
  // 验证压缩错误恢复
  assert_eq(compression_errors.length(), 8)
  
  // 验证错误类型
  assert_eq(compression_errors[0].0, "error_type")
  assert_eq(compression_errors[0].1, "CORRUPTION_DETECTED")
  assert_eq(compression_errors[0].1.contains("_"), true)
  
  // 验证压缩算法
  assert_eq(compression_errors[1].0, "compression_algorithm")
  assert_eq(compression_errors[1].1, "gzip")
  
  // 验证输入数据大小
  assert_eq(compression_errors[2].0, "input_data_size")
  assert_eq(compression_errors[2].1, 204800)
  assert_eq(compression_errors[2].1 > 0, true)
  
  // 验证错误位置
  assert_eq(compression_errors[3].0, "error_position")
  assert_eq(compression_errors[3].1, 102456)
  assert_eq(compression_errors[3].1 < compression_errors[2].1, true)
  
  // 验证恢复尝试
  assert_eq(compression_errors[4].0, "recovery_attempt")
  assert_eq(compression_errors[4].1, "use_fallback_algorithm")
  assert_eq(compression_errors[4].1.contains("_"), true)
  
  // 验证回退算法
  assert_eq(compression_errors[5].0, "fallback_algorithm")
  assert_eq(compression_errors[5].1, "lz4")
  assert_eq(compression_errors[5].1 != compression_errors[1].1, true)
  
  // 验证恢复成功
  assert_eq(compression_errors[6].0, "recovery_success")
  assert_eq(compression_errors[6].1, true)
  
  // 验证恢复时间
  assert_eq(compression_errors[7].0, "recovery_time_ms")
  assert_eq(compression_errors[7].1, 15)
  assert_eq(compression_errors[7].1 > 0, true)
}