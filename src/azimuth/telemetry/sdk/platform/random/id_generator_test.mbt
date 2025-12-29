// SDK Random平台功能测试用例
// 测试ID生成相关的随机功能

test "trace_id_generation_and_validation" {
  // 测试Trace ID生成和验证
  
  // 模拟Trace ID生成器
  struct TraceIdGenerator {
    last_generated : String
    generation_count : Int64
  }
  
  // 创建Trace ID生成器
  let generator = TraceIdGenerator::{
    last_generated: "",
    generation_count: 0L
  }
  
  // 模拟生成Trace ID（32位十六进制字符串）
  let generate_trace_id = fn() : String {
    "0af7651916cd43dd8448eb211c80319c"  // 示例Trace ID
  }
  
  // 生成多个Trace ID
  let trace_id1 = generate_trace_id()
  let trace_id2 = generate_trace_id()
  let trace_id3 = generate_trace_id()
  
  // 验证Trace ID格式
  assert_eq(trace_id1.length(), 32)
  assert_eq(trace_id2.length(), 32)
  assert_eq(trace_id3.length(), 32)
  
  // 验证Trace ID只包含十六进制字符
  let is_hex_char = fn(c : Char) : Bool {
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  }
  
  let validate_hex = fn(s : String) : Bool {
    let mut i = 0
    let mut valid = true
    while i < s.length() && valid {
      valid = is_hex_char(s[i])
      i = i + 1
    }
    valid
  }
  
  assert_eq(validate_hex(trace_id1), true)
  assert_eq(validate_hex(trace_id2), true)
  assert_eq(validate_hex(trace_id3), true)
  
  // 验证Trace ID不是全零
  let is_not_all_zero = fn(s : String) : Bool {
    s != "00000000000000000000000000000000"
  }
  
  assert_eq(is_not_all_zero(trace_id1), true)
  assert_eq(is_not_all_zero(trace_id2), true)
  assert_eq(is_not_all_zero(trace_id3), true)
  
  // 测试Trace ID前缀和后缀
  assert_eq(trace_id1.has_prefix("0af7"), true)
  assert_eq(trace_id1.has_suffix("319c"), true)
  
  // 测试Trace ID转换为字节数组
  let hex_to_byte = fn(hex_char : Char) : Byte {
    if hex_char >= '0' && hex_char <= '9' {
      (hex_char.to_int() - '0'.to_int()).to_byte()
    } else if hex_char >= 'a' && hex_char <= 'f' {
      (hex_char.to_int() - 'a'.to_int() + 10).to_byte()
    } else {
      (hex_char.to_int() - 'A'.to_int() + 10).to_byte()
    }
  }
  
  let trace_id_bytes = []
  let mut i = 0
  while i < trace_id1.length() / 2 {
    let high_nibble = hex_to_byte(trace_id1[i * 2])
    let low_nibble = hex_to_byte(trace_id1[i * 2 + 1])
    trace_id_bytes.push((high_nibble * 16_byte + low_nibble))
    i = i + 1
  }
  
  assert_eq(trace_id_bytes.length(), 16)
  assert_eq(trace_id_bytes[0], 10_byte)  // 0x0a
  assert_eq(trace_id_bytes[15], 156_byte) // 0x9c
}

test "span_id_generation_and_validation" {
  // 测试Span ID生成和验证
  
  // 模拟Span ID生成器
  struct SpanIdGenerator {
    last_generated : String
    generation_count : Int64
  }
  
  // 创建Span ID生成器
  let generator = SpanIdGenerator::{
    last_generated: "",
    generation_count: 0L
  }
  
  // 模拟生成Span ID（16位十六进制字符串）
  let generate_span_id = fn() : String {
    "b7ad6b7169203331"  // 示例Span ID
  }
  
  // 生成多个Span ID
  let span_id1 = generate_span_id()
  let span_id2 = generate_span_id()
  let span_id3 = generate_span_id()
  
  // 验证Span ID格式
  assert_eq(span_id1.length(), 16)
  assert_eq(span_id2.length(), 16)
  assert_eq(span_id3.length(), 16)
  
  // 验证Span ID只包含十六进制字符
  let is_hex_char = fn(c : Char) : Bool {
    (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
  }
  
  let validate_hex = fn(s : String) : Bool {
    let mut i = 0
    let mut valid = true
    while i < s.length() && valid {
      valid = is_hex_char(s[i])
      i = i + 1
    }
    valid
  }
  
  assert_eq(validate_hex(span_id1), true)
  assert_eq(validate_hex(span_id2), true)
  assert_eq(validate_hex(span_id3), true)
  
  // 验证Span ID不是全零
  let is_not_all_zero = fn(s : String) : Bool {
    s != "0000000000000000"
  }
  
  assert_eq(is_not_all_zero(span_id1), true)
  assert_eq(is_not_all_zero(span_id2), true)
  assert_eq(is_not_all_zero(span_id3), true)
  
  // 测试Span ID前缀和后缀
  assert_eq(span_id1.has_prefix("b7ad"), true)
  assert_eq(span_id1.has_suffix("3331"), true)
  
  // 测试Span ID转换为字节数组
  let hex_to_byte = fn(hex_char : Char) : Byte {
    if hex_char >= '0' && hex_char <= '9' {
      (hex_char.to_int() - '0'.to_int()).to_byte()
    } else if hex_char >= 'a' && hex_char <= 'f' {
      (hex_char.to_int() - 'a'.to_int() + 10).to_byte()
    } else {
      (hex_char.to_int() - 'A'.to_int() + 10).to_byte()
    }
  }
  
  let span_id_bytes = []
  let mut i = 0
  while i < span_id1.length() / 2 {
    let high_nibble = hex_to_byte(span_id1[i * 2])
    let low_nibble = hex_to_byte(span_id1[i * 2 + 1])
    span_id_bytes.push((high_nibble * 16_byte + low_nibble))
    i = i + 1
  }
  
  assert_eq(span_id_bytes.length(), 8)
  assert_eq(span_id_bytes[0], 183_byte)  // 0xb7
  assert_eq(span_id_bytes[7], 49_byte)   // 0x31
}

test "random_sampling_decision" {
  // 测试随机采样决策
  
  // 模拟采样决策器
  struct SamplingDecision {
    sample : Bool
    probability : Double
    random_value : Double
    trace_id : String
  }
  
  // 创建采样决策
  let high_probability_decision = SamplingDecision::{
    sample: true,
    probability: 0.8,
    random_value: 0.6,
    trace_id: "0af7651916cd43dd8448eb211c80319c"
  }
  
  let low_probability_decision = SamplingDecision::{
    sample: false,
    probability: 0.1,
    random_value: 0.9,
    trace_id: "b7ad6b7169203331"
  }
  
  // 验证高概率采样决策
  assert_eq(high_probability_decision.sample, true)
  assert_eq(high_probability_decision.probability, 0.8)
  assert_eq(high_probability_decision.random_value < high_probability_decision.probability, true)
  
  // 验证低概率采样决策
  assert_eq(low_probability_decision.sample, false)
  assert_eq(low_probability_decision.probability, 0.1)
  assert_eq(low_probability_decision.random_value > low_probability_decision.probability, true)
  
  // 测试采样决策逻辑
  let make_sampling_decision = fn(probability : Double, random_value : Double) : Bool {
    random_value < probability
  }
  
  // 测试不同的采样场景
  assert_eq(make_sampling_decision(1.0, 0.5), true)   // 100%采样率，总是采样
  assert_eq(make_sampling_decision(0.0, 0.5), false)  // 0%采样率，从不采样
  assert_eq(make_sampling_decision(0.5, 0.3), true)   // 50%采样率，随机值0.3，采样
  assert_eq(make_sampling_decision(0.5, 0.7), false)  // 50%采样率，随机值0.7，不采样
  
  // 测试边界情况
  assert_eq(make_sampling_decision(0.001, 0.0005), true)   // 极低采样率，采样
  assert_eq(make_sampling_decision(0.001, 0.9995), false)  // 极低采样率，不采样
  assert_eq(make_sampling_decision(0.999, 0.998), true)    // 极高采样率，采样
  assert_eq(make_sampling_decision(0.999, 0.9995), false)  // 极高采样率，不采样
  
  // 测试采样概率验证
  let is_valid_probability = fn(p : Double) : Bool {
    p >= 0.0 && p <= 1.0
  }
  
  let is_valid_random_value = fn(r : Double) : Bool {
    r >= 0.0 && r < 1.0
  }
  
  assert_eq(is_valid_probability(high_probability_decision.probability), true)
  assert_eq(is_valid_probability(low_probability_decision.probability), true)
  assert_eq(is_valid_probability(0.0), true)
  assert_eq(is_valid_probability(1.0), true)
  assert_eq(is_valid_probability(-0.1), false)
  assert_eq(is_valid_probability(1.1), false)
  
  assert_eq(is_valid_random_value(high_probability_decision.random_value), true)
  assert_eq(is_valid_random_value(low_probability_decision.random_value), true)
  assert_eq(is_valid_random_value(0.0), true)
  assert_eq(is_valid_random_value(0.999999), true)
  assert_eq(is_valid_random_value(-0.1), false)
  assert_eq(is_valid_random_value(1.0), false)
}

test "unique_request_id_generation" {
  // 测试唯一请求ID生成
  
  // 模拟请求ID生成器
  struct RequestIdGenerator {
    timestamp_base : Int64
    sequence_counter : Int64
    node_id : String
  }
  
  // 创建请求ID生成器
  let generator = RequestIdGenerator::{
    timestamp_base: 1640995200000L,  // 2022-01-01 00:00:00 UTC
    sequence_counter: 12345L,
    node_id: "node01"
  }
  
  // 模拟生成请求ID的格式：timestamp-nodeid-sequence
  let generate_request_id = fn(timestamp : Int64, node_id : String, sequence : Int64) : String {
    timestamp.to_string() + "-" + node_id + "-" + sequence.to_string()
  }
  
  // 生成多个请求ID
  let request_id1 = generate_request_id(generator.timestamp_base, generator.node_id, generator.sequence_counter)
  let request_id2 = generate_request_id(generator.timestamp_base + 1L, generator.node_id, generator.sequence_counter + 1L)
  let request_id3 = generate_request_id(generator.timestamp_base + 2L, generator.node_id, generator.sequence_counter + 2L)
  
  // 验证请求ID格式
  assert_eq(request_id1.contains("-"), true)
  assert_eq(request_id2.contains("-"), true)
  assert_eq(request_id3.contains("-"), true)
  
  // 验证请求ID组成部分
  let parse_request_id = fn(id : String) : (Int64, String, Int64) {
    let parts = id.split("-")
    let timestamp = parts[0].to_int64()
    let node_id = parts[1]
    let sequence = parts[2].to_int64()
    (timestamp, node_id, sequence)
  }
  
  let (timestamp1, node_id1, sequence1) = parse_request_id(request_id1)
  let (timestamp2, node_id2, sequence2) = parse_request_id(request_id2)
  let (timestamp3, node_id3, sequence3) = parse_request_id(request_id3)
  
  // 验证解析结果
  assert_eq(timestamp1, generator.timestamp_base)
  assert_eq(node_id1, generator.node_id)
  assert_eq(sequence1, generator.sequence_counter)
  
  assert_eq(timestamp2, generator.timestamp_base + 1L)
  assert_eq(node_id2, generator.node_id)
  assert_eq(sequence2, generator.sequence_counter + 1L)
  
  assert_eq(timestamp3, generator.timestamp_base + 2L)
  assert_eq(node_id3, generator.node_id)
  assert_eq(sequence3, generator.sequence_counter + 2L)
  
  // 验证请求ID的唯一性
  assert_eq(request_id1 != request_id2, true)
  assert_eq(request_id2 != request_id3, true)
  assert_eq(request_id1 != request_id3, true)
  
  // 验证时间戳递增
  assert_eq(timestamp2 > timestamp1, true)
  assert_eq(timestamp3 > timestamp2, true)
  
  // 验证序列号递增
  assert_eq(sequence2 > sequence1, true)
  assert_eq(sequence3 > sequence2, true)
  
  // 测试不同节点的请求ID
  let node02_request_id = generate_request_id(generator.timestamp_base, "node02", generator.sequence_counter)
  let (_, node_id02, _) = parse_request_id(node02_request_id)
  assert_eq(node_id02, "node02")
  assert_eq(node02_request_id != request_id1, true)
  
  // 测试长节点ID
  let long_node_id = "very-long-node-name-with-dashes-123"
  let long_node_request_id = generate_request_id(generator.timestamp_base, long_node_id, generator.sequence_counter)
  let (_, parsed_long_node_id, _) = parse_request_id(long_node_request_id)
  assert_eq(parsed_long_node_id, long_node_id)
}