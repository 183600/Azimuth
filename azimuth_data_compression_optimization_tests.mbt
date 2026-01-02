// Azimuth 数据压缩和优化测试用例
// 专注于遥测系统的数据压缩、序列化优化和性能提升

// 测试1: 遥测数据压缩
test "遥测数据压缩测试" {
  let tracer_provider = TracerProvider::default()
  let tracer = TracerProvider::get_tracer(tracer_provider, "compression.test")
  
  // 创建包含大量属性的span
  let span = Tracer::start_span(tracer, "compression.test.span")
  
  // 添加重复性属性值（适合压缩）
  for i in 0..100 {
    Span::set_attribute(span, "repeated.attr." + i.to_string(), StringValue("repeated.value"))
    Span::set_attribute(span, "numeric.attr." + i.to_string(), IntValue(i * 10))
  }
  
  // 添加长字符串属性
  let long_string = "This is a very long string that contains repetitive patterns ".repeat(20)
  Span::set_attribute(span, "long.string.attr", StringValue(long_string))
  
  // 序列化span数据
  let uncompressed_data = Span::serialize(span)
  let uncompressed_size = uncompressed_data.length()
  
  // 使用不同的压缩算法
  let gzip_compressor = GzipCompressor::new()
  let gzip_compressed = Compressor::compress(gzip_compressor, uncompressed_data)
  let gzip_size = gzip_compressed.length()
  
  let lz4_compressor = Lz4Compressor::new()
  let lz4_compressed = Compressor::compress(lz4_compressor, uncompressed_data)
  let lz4_size = lz4_compressed.length()
  
  // 验证压缩效果
  assert_true(gzip_size < uncompressed_size)
  assert_true(lz4_size < uncompressed_size)
  
  // 计算压缩率
  let gzip_ratio = gzip_size.to_float() / uncompressed_size.to_float()
  let lz4_ratio = lz4_size.to_float() / uncompressed_size.to_float()
  
  // 验证压缩率合理（应该小于原大小的80%）
  assert_true(gzip_ratio < 0.8)
  assert_true(lz4_ratio < 0.8)
  
  // 验证解压缩后数据一致性
  let gzip_decompressed = Compressor::decompress(gzip_compressor, gzip_compressed)
  let lz4_decompressed = Compressor::decompress(lz4_compressor, lz4_compressed)
  
  assert_eq(gzip_decompressed, uncompressed_data)
  assert_eq(lz4_decompressed, uncompressed_data)
  
  Span::end(span)
}
